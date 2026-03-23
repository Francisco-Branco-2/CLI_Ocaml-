(* ================================================================== *)
(*  executor.ml -- execucao de processos, pipes, redirects e AST     *)
(*                                                                    *)
(*  Este modulo e o "motor" do shell. Recebe um no da AST e executa-o.*)
(*                                                                    *)
(*  Arquitectura fork/exec (POSIX):                                   *)
(*                                                                    *)
(*    Para cada comando externo:                                       *)
(*      1. fork() -- duplica o processo do shell                      *)
(*      2. filho: aplica redirects, fecha fds desnecessarios, execv() *)
(*      3. pai: waitpid() para esperar pelo filho (ou nao, se &)      *)
(*                                                                    *)
(*    Para pipelines (A | B | C):                                     *)
(*      1. Cria n-1 pipes Unix (pares de file descriptors)            *)
(*      2. Faz fork para cada comando                                  *)
(*      3. Cada filho liga o seu stdin/stdout aos fds do pipe certo   *)
(*      4. Pai fecha todos os fds dos pipes e waitpid em todos        *)
(*                                                                    *)
(*    Para estruturas compostas (if/while/for/...):                   *)
(*      Recursao no eval -- nao e necessario fork                     *)
(* ================================================================== *)

open Types
open Unix

(* ------------------------------------------------------------------ *)
(*  Aplicar redirects a um processo (chamado no filho apos fork)      *)
(*                                                                    *)
(*  Cada redirect:                                                     *)
(*    1. Abre o ficheiro com os flags correctos                       *)
(*    2. dup2(fd_novo, fd_standard) -- substitui stdin/stdout/stderr  *)
(*    3. close(fd_novo) -- o fd original ja nao e necessario          *)
(* ------------------------------------------------------------------ *)

let apply_redirects redirects =
  List.iter (fun r ->
    (match r with
     | Redirect_in f ->
       let fd = openfile f [O_RDONLY] 0 in
       dup2 fd stdin; close fd
     | Redirect_out f ->
       let fd = openfile f [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
       dup2 fd stdout; close fd
     | Redirect_app f ->
       let fd = openfile f [O_WRONLY; O_CREAT; O_APPEND] 0o644 in
       dup2 fd stdout; close fd
     | Redirect_err f ->
       let fd = openfile f [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
       dup2 fd stderr; close fd
     | Redirect_err2 f ->
       let fd = openfile f [O_WRONLY; O_CREAT; O_APPEND] 0o644 in
       dup2 fd stderr; close fd
     | Redirect_both f ->
       let fd = openfile f [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
       dup2 fd stdout; dup2 fd stderr; close fd
     | Heredoc delim ->
       (* o corpo foi lido pelo REPL e guardado em Parser.heredoc_bodies *)
       let content = Parser.get_heredoc_body delim in
       (* cria um pipe temporario e escreve o conteudo nele            *)
       let (pipe_r, pipe_w) = pipe () in
       let written = ref 0 in
       let content_bytes = Bytes.of_string content in
       let len = Bytes.length content_bytes in
       while !written < len do
         let n = write pipe_w content_bytes !written (len - !written) in
         written := !written + n
       done;
       close pipe_w;
       dup2 pipe_r stdin;
       close pipe_r)
  ) redirects

(* ------------------------------------------------------------------ *)
(*  Executar um comando simples                                        *)
(*                                                                    *)
(*  Ordem de pesquisa:                                                 *)
(*    1. Builtin (corre no processo actual)                           *)
(*    2. Funcao definida pelo utilizador (corre no processo actual)   *)
(*    3. Executavel externo via $PATH (fork + exec)                   *)
(* ------------------------------------------------------------------ *)

let rec run_simple_cmd cmd =
  match cmd.argv with
  | [] -> 0
  | name :: args ->
    (* 1. Expande aliases recursivamente *)
    let argv    = Builtins.expand_alias (name :: args) in
    let name    = List.hd argv in
    let args    = List.tl argv in
    (* 2. Builtin? *)
    if Builtins.is_builtin name then begin
      apply_redirects cmd.redirects;
      Builtins.run name args
    end
    (* 3. Funcao definida pelo utilizador? *)
    else if Parser.lookup_function name <> None then begin
      let body = Option.get (Parser.lookup_function name) in
      (* salva e define os argumentos posicionais da funcao           *)
      let saved_args = !Lexer.positional_args in
      Lexer.positional_args := Array.of_list args;
      let code =
        (try eval body
         with Exit_shell n ->
           Lexer.positional_args := saved_args; n)
      in
      Lexer.positional_args := saved_args;
      code
    end
    (* 4. Executavel externo *)
    else begin
      match Builtins.find_in_path name with
      | None ->
        Printf.eprintf "%s: comando nao encontrado\n%!" name; 127
      | Some path ->
        let argv_arr = Array.of_list (name :: args) in
        (match fork () with
         | 0 ->
           (* PROCESSO FILHO *)
           (* aplica redirects antes de exec                           *)
           apply_redirects cmd.redirects;
           (try execv path argv_arr
            with Unix_error (e, _, _) ->
              Printf.eprintf "%s: %s\n%!" name (error_message e);
              exit 126)
         | pid ->
           (* PROCESSO PAI: espera *)
           let _, status = waitpid [] pid in
           status_to_code status)
    end

(* ------------------------------------------------------------------ *)
(*  Converter status Unix em codigo de saida                          *)
(* ------------------------------------------------------------------ *)

and status_to_code = function
  | WEXITED   n -> n
  | WSIGNALED n ->
    (* convencao POSIX: 128 + numero do sinal *)
    128 + n
  | WSTOPPED  _ -> 0  (* nao deveria acontecer sem WUNTRACED           *)

(* ------------------------------------------------------------------ *)
(*  Executar um pipeline                                               *)
(*                                                                    *)
(*  Para n comandos cria n-1 pipes.                                   *)
(*  Cada par adjacente (i, i+1) partilha um pipe:                    *)
(*    stdout do processo i  -> write end do pipe i                   *)
(*    stdin  do processo i+1 -> read end do pipe i                   *)
(*                                                                    *)
(*       cmd[0]     cmd[1]     cmd[2]                                 *)
(*         |  stdout  |  stdout  |                                    *)
(*         v          v          v                                    *)
(*       pipe[0].w  pipe[1].w   (stdout original)                    *)
(*       pipe[0].r  pipe[1].r                                         *)
(*         ^          ^                                               *)
(*         |  stdin   |  stdin                                        *)
(*       cmd[1]     cmd[2]                                            *)
(* ------------------------------------------------------------------ *)

and run_pipeline pl =
  match pl.cmds with
  | [] -> 0

  | [single] ->
    (* caso trivial: pipeline de 1 comando *)
    if pl.background then begin
      run_in_background [single] (List.hd single.argv)
    end else begin
      let code = run_simple_cmd single in
      update_exit_code code;
      code
    end

  | cmds ->
    let n     = List.length cmds in
    (* cria n-1 pipes (r, w) *)
    let pipes = Array.init (n - 1) (fun _ -> pipe ()) in
    let pids  = ref [] in

    List.iteri (fun i cmd ->
      match fork () with
      | 0 ->
        (* FILHO i *)

        (* liga stdin ao pipe anterior (se nao for o primeiro) *)
        if i > 0 then begin
          let (r, _) = pipes.(i - 1) in
          dup2 r stdin; close r
        end;

        (* liga stdout ao pipe seguinte (se nao for o ultimo) *)
        if i < n - 1 then begin
          let (_, w) = pipes.(i) in
          dup2 w stdout; close w
        end;

        (* fecha TODOS os fds de pipe no filho (excepto os que usou)  *)
        (* Isto e CRITICO: se o filho manter o write-end aberto,      *)
        (* o leitor nunca recebe EOF.                                  *)
        Array.iter (fun (r, w) ->
          (try close r with _ -> ());
          (try close w with _ -> ())
        ) pipes;

        (* aplica redirects do comando (sobrepoe ao que o pipe fez)   *)
        apply_redirects cmd.redirects;

        (* executa *)
        (match Builtins.find_in_path (List.hd cmd.argv) with
         | Some path ->
           (try execv path (Array.of_list cmd.argv)
            with _ -> exit 126)
         | None ->
           let name = List.hd cmd.argv in
           if Builtins.is_builtin name then
             exit (Builtins.run name (List.tl cmd.argv))
           else begin
             Printf.eprintf "%s: comando nao encontrado\n%!" name;
             exit 127
           end)

      | pid -> pids := pid :: !pids

    ) cmds;

    (* PAI: fecha todos os fds dos pipes *)
    (* Se o pai nao fechar o write-end dos pipes, os filhos que lerem *)
    (* de pipes anteriores nunca recebem EOF.                         *)
    Array.iter (fun (r, w) ->
      (try close r with _ -> ());
      (try close w with _ -> ())
    ) pipes;

    if pl.background then begin
      let desc = String.concat " | " (List.map (fun c -> List.hd c.argv) cmds) in
      let pids_list = List.rev !pids in
      ignore (Builtins.add_job (List.hd pids_list) pids_list desc);
      0
    end else begin
      (* espera por todos os filhos em ordem inversa (pids foi acumulado ao contrario) *)
      let last_code = ref 0 in
      List.iter (fun pid ->
        let _, status = waitpid [] pid in
        last_code := status_to_code status
      ) !pids;  (* ordem inversa = da direita para a esquerda *)
      update_exit_code !last_code;
      !last_code
    end

(* ------------------------------------------------------------------ *)
(*  Executar em background                                             *)
(* ------------------------------------------------------------------ *)

and run_in_background cmds desc =
  match fork () with
  | 0 ->
    (* filho em background: fecha o terminal (nao recebe Ctrl+C)       *)
    (* Em job control completo usariamos setsid(). *)
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
    (match cmds with
     | [cmd] ->
       let code = run_simple_cmd cmd in
       exit code
     | _ ->
       let code = run_pipeline { cmds; background = false } in
       exit code)
  | pid ->
    ignore (Builtins.add_job pid [pid] desc);
    0

(* ------------------------------------------------------------------ *)
(*  Actualizar $? e last_exit_code                                     *)
(* ------------------------------------------------------------------ *)

and update_exit_code code =
  Builtins.last_exit_code := code;
  Unix.putenv "?" (string_of_int code)

(* ------------------------------------------------------------------ *)
(*  Avaliacao da AST                                                   *)
(*                                                                    *)
(*  Cada no da AST e avaliado recursivamente.                          *)
(*  O codigo de retorno de cada no e propagado para cima.             *)
(* ------------------------------------------------------------------ *)

and eval ast =
  match ast with
  | Nop -> 0

  | Pipeline pl ->
    run_pipeline pl

  | Seq (a, b) ->
    (* executa sempre ambos; o codigo de saida e o do ultimo          *)
    let _ = eval a in
    eval b

  | And (a, b) ->
    (* short-circuit: so executa b se a tiver sucesso (codigo 0)      *)
    let code = eval a in
    if code = 0 then eval b else code

  | Or (a, b) ->
    (* short-circuit: so executa b se a falhar (codigo != 0)          *)
    let code = eval a in
    if code <> 0 then eval b else code

  | If (cond, then_b, else_b) ->
    (* avalia a condicao (codigo 0 = verdadeiro, como em POSIX)       *)
    let code = eval cond in
    if code = 0 then eval then_b
    else (match else_b with
          | Some b -> eval b
          | None   -> 0)

  | While (cond, body) ->
    let code = ref 0 in
    (try
       while eval cond = 0 do
         code := eval body
       done
     with Break_loop   -> ()
        | Continue_loop -> ());
    !code

  | Until (cond, body) ->
    (* executa enquanto a condicao FALHAR (inverso de while)           *)
    let code = ref 0 in
    (try
       while eval cond <> 0 do
         code := eval body
       done
     with Break_loop   -> ()
        | Continue_loop -> ());
    !code

  | For (var, words, body) ->
    (* expande as palavras e itera.                                    *)
    (* "$@" e tratado especialmente: expande para cada argumento       *)
    (* posicional separado, em vez de uma string unica.               *)
    let expanded_words =
      List.concat_map (fun w ->
        if w = "$@" || w = "$*" then
          Array.to_list !Lexer.positional_args
        else
          Lexer.expand_word w
      ) words
    in
    let code = ref 0 in
    (try
       List.iter (fun value ->
         Unix.putenv var value;
         (try code := eval body
          with Continue_loop -> ())
       ) expanded_words
     with Break_loop -> ());
    !code

  | Case (word, clauses) ->
    (* expande a palavra e compara com cada padrao *)
    let w = Lexer.expand_vars (Lexer.expand_tilde word) in
    let code = ref 0 in
    let matched = ref false in
    List.iter (fun (pats, body) ->
      if not !matched then begin
        let matches = List.exists (fun pat ->
          (* converte padrao glob para regexp *)
          let re_str = pat
            |> Str.global_replace (Str.regexp_string "*") ".*"
            |> Str.global_replace (Str.regexp_string "?") "."
          in
          (try Str.string_match (Str.regexp ("^" ^ re_str ^ "$")) w 0
           with _ -> pat = w)
        ) pats in
        if matches then begin
          matched := true;
          code := eval body
        end
      end
    ) clauses;
    !code

  | FuncDef (name, body) ->
    (* regista a funcao -- nao executa ainda *)
    Parser.define_function name body;
    0

  | FuncCall (name, args) ->
    (* invoca a funcao (nao deveria chegar aqui normalmente;           *)
    (* e tratado em run_simple_cmd via lookup_function)               *)
    (match Parser.lookup_function name with
     | None      -> Printf.eprintf "%s: funcao nao definida\n%!" name; 127
     | Some body ->
       let saved = !Lexer.positional_args in
       Lexer.positional_args := Array.of_list args;
       let code =
         (try eval body
          with Exit_shell n -> n)
       in
       Lexer.positional_args := saved;
       code)

  | Subshell body ->
    (* executa em processo filho separado.                             *)
    (* Se vier do parser como "cmd &", o pai nao espera (background). *)
    (match fork () with
     | 0 ->
       (* filho: ignora SIGINT (comportamento standard de background)  *)
       Sys.set_signal Sys.sigint Sys.Signal_ignore;
       let code = (try eval body with Exit_shell n -> n | _ -> 1) in
       exit code
     | pid ->
       (* pai: regista como job e nao bloqueia                         *)
       (* Nota: quando Subshell vem do parser's background handling    *)
       (* queremos comportamento de background; caso contrario bloqueamos *)
       (* Por agora: sempre bloqueia. Para & em compostos usar Pipeline *)
       let _, status = waitpid [] pid in
       status_to_code status)

  | Group body ->
    (* executa no processo actual (redirects afectariam o shell!)     *)
    eval body

(* ------------------------------------------------------------------ *)
(*  Excepcoes de controlo de fluxo para break/continue                *)
(*                                                                    *)
(*  Sao excepcoes OCaml que "saltam" para fora do loop.               *)
(*  O eval de While/Until/For apanha-as.                              *)
(* ------------------------------------------------------------------ *)

let () =
  (* inicializa a referencia circular com builtins *)
  Builtins.eval_ref := eval

(* ------------------------------------------------------------------ *)
(*  Recolher jobs terminados (SIGCHLD / WNOHANG)                      *)
(*                                                                    *)
(*  Chamado no inicio de cada prompt para reportar jobs terminados.   *)
(*  Usa WNOHANG para nao bloquear se nenhum job terminou.             *)
(* ------------------------------------------------------------------ *)

let reap_jobs () =
  let finished = ref [] in
  List.iter (fun (j : job) ->
    (match waitpid [WNOHANG] j.job_pid with
     | 0, _         -> ()   (* ainda a correr                         *)
     | _, WEXITED n ->
       Printf.printf "\n[%d]+  Done(%d)  %s\n%!" j.job_id n j.job_desc;
       finished := j.job_pid :: !finished
     | _, WSIGNALED n ->
       Printf.printf "\n[%d]+  Killed(%d)  %s\n%!" j.job_id n j.job_desc;
       finished := j.job_pid :: !finished
     | _, WSTOPPED _ ->
       if j.job_status <> Stopped then begin
         Printf.printf "\n[%d]+  Stopped  %s\n%!" j.job_id j.job_desc;
         j.job_status <- Stopped
       end)
  ) !Builtins.jobs;
  List.iter Builtins.remove_job !finished

