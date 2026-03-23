(* ================================================================== *)
(*  main.ml -- ponto de entrada do myshell                           *)
(*                                                                    *)
(*  Modos de operacao:                                                 *)
(*    myshell              -- modo interactivo (REPL)                 *)
(*    myshell script.sh    -- modo script (nao-interactivo)           *)
(*    myshell -c "cmd"     -- executa um comando e sai                *)
(*    myshell -s           -- le comandos de stdin                    *)
(*                                                                    *)
(*  Sequencia de arranque:                                             *)
(*    1. setup_signals() -- configura SIGINT, SIGTERM, SIGCHLD        *)
(*    2. History.load()  -- carrega ~/.myshell_history                *)
(*    3. Config.load()   -- executa ~/.myshellrc                      *)
(*    4. run_interactive() ou run_script()                            *)
(* ================================================================== *)

open Myshell_lib

(* ------------------------------------------------------------------ *)
(*  Configuracao de sinais                                              *)
(*                                                                    *)
(*  Sinais POSIX relevantes para um shell:                             *)
(*    SIGINT  (2)  -- Ctrl+C: interrompe o comando em foreground      *)
(*    SIGTERM (15) -- pedido de terminacao (de outro processo)        *)
(*    SIGCHLD (17) -- um processo filho terminou                      *)
(*    SIGTSTP (20) -- Ctrl+Z: suspende o processo em foreground       *)
(*    SIGTTOU     -- escrita no terminal por processo em background    *)
(*    SIGTTIN     -- leitura do terminal por processo em background    *)
(* ------------------------------------------------------------------ *)

let setup_signals () =
  (* SIGINT: nao mata o shell; cancela apenas o comando actual        *)
  (* O nosso readline trata Ctrl+C directamente em modo raw           *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    print_string "\n";
    flush stdout
  ));

  (* SIGTERM: sai guardando o historico *)
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->
    History.save ();
    exit 0
  ));

  (* SIGCHLD: recolhe processos filho terminados                      *)
  (* Nao bloqueamos aqui -- a recolha real e feita em reap_jobs()     *)
  Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ ->
    (* nao e seguro chamar waitpid dentro de signal handlers em OCaml *)
    (* portanto apenas sinaliza; o REPL chama reap_jobs() no inicio   *)
    ()
  ));

  (* SIGTSTP: por defeito suspenderia o shell; ignoramos aqui         *)
  (* (um shell real faria setpgrp e geriria grupos de processos)      *)
  Sys.set_signal Sys.sigtstp Sys.Signal_ignore;

  (* SIGTTOU/SIGTTIN: evita que o shell seja suspenso quando um       *)
  (* processo background tenta escrever/ler do terminal               *)
  Sys.set_signal Sys.sigttou Sys.Signal_ignore;
  Sys.set_signal Sys.sigttin Sys.Signal_ignore

(* ------------------------------------------------------------------ *)
(*  Execucao de uma linha de comando                                   *)
(* ------------------------------------------------------------------ *)

(* Lê os corpos de here-documents do stdin interactivo.              *)
(* Chamado quando a linha contem '<<'. Lê linhas até encontrar o     *)
(* delimitador e guarda o corpo em Parser.heredoc_bodies.             *)
let read_heredoc_bodies line =
  (* procura << DELIM na linha tokenizada *)
  let toks = Lexer.tokenize line in
  let rec scan = function
    | [] -> ()
    | Lexer.THeredoc :: Lexer.TWord delim :: rest ->
      (* lê o corpo interactivamente *)
      let buf = Buffer.create 64 in
      let go  = ref true in
      while !go do
        (if Unix.isatty Unix.stdin then (print_string "> "; flush stdout));
        (try
           let l = input_line stdin in
           if String.trim l = delim then go := false
           else (Buffer.add_string buf l; Buffer.add_char buf '\n')
         with End_of_file -> go := false)
      done;
      Parser.add_heredoc_body delim (Buffer.contents buf);
      scan rest
    | _ :: rest -> scan rest
  in
  scan toks

let exec_line line =
  let line = String.trim line in
  if line = "" || (String.length line > 0 && line.[0] = '#')
  then 0
  else begin
    (* se houver heredocs, lê os corpos antes de parsear *)
    if String.contains line '<' then
      read_heredoc_bodies line;
    match Parser.parse line with
    | None     -> 0
    | Some ast ->
      (try Executor.eval ast
       with
       | Types.Shell_error msg ->
         Printf.eprintf "myshell: %s\n%!" msg;
         Builtins.last_exit_code := 1;
         1
       | Unix.Unix_error (e, fn, _) ->
         Printf.eprintf "myshell: %s: %s\n%!" fn (Unix.error_message e);
         Builtins.last_exit_code := 1;
         1)
  end

(* ------------------------------------------------------------------ *)
(*  Modo interactivo                                                    *)
(*                                                                    *)
(*  O REPL (Read-Eval-Print Loop) repete indefinidamente:             *)
(*    1. reap_jobs() -- reporta jobs terminados                       *)
(*    2. Mostra o prompt                                               *)
(*    3. Le uma linha (com edicao e historico)                        *)
(*    4. Avalia a linha                                                *)
(*    5. Actualiza $?                                                  *)
(* ------------------------------------------------------------------ *)

let run_interactive () =
  History.load ();
  Config.load ();

  (* configura $0 para o nome do shell *)
  Unix.putenv "0" "myshell";
  Unix.putenv "?" "0";

  (* imprime mensagem de boas-vindas *)
  if Unix.isatty Unix.stdin then begin
    Printf.printf "myshell -- uma shell escrita em OCaml\n";
    Printf.printf "Escreve 'exit' ou prime Ctrl+D para sair.\n";
    Printf.printf "Tab para completar, setas para o historico.\n\n";
    flush stdout
  end;

  try
    while true do
      Executor.reap_jobs ();
      let prompt = Prompt.build () in
      let ps2    = Prompt.build_ps2 () in

      (match Readline.read_line_raw ~prompt ~ps2 () with
       | None ->
         (* Ctrl+D -- EOF *)
         print_endline "\nexit";
         History.save ();
         exit !Builtins.last_exit_code

       | Some line when String.trim line = "" -> ()

       | Some line ->
         History.add line;
         ignore (exec_line line))
    done
  with
  | Types.Exit_shell code ->
    History.save ();
    exit code
  | Exit ->
    History.save ();
    print_endline "\nexit";
    exit !Builtins.last_exit_code

(* ------------------------------------------------------------------ *)
(*  Modo de script                                                      *)
(*                                                                    *)
(*  Le o ficheiro linha a linha e executa cada uma.                   *)
(*  Suporta:                                                           *)
(*    - Comentarios (#)                                               *)
(*    - Linhas de continuacao (\ no fim)                              *)
(*    - Estruturas multi-linha (if/while/for/...)                     *)
(* ------------------------------------------------------------------ *)

let run_script path args =
  if not (Sys.file_exists path) then begin
    Printf.eprintf "myshell: %s: ficheiro nao encontrado\n%!" path;
    exit 1
  end;

  (* configura $0 e os argumentos posicionais *)
  Unix.putenv "0" path;
  Lexer.positional_args := Array.of_list args;

  History.load ();
  Config.load ();

  let ic   = open_in path in
  let code = ref 0 in
  let acc  = ref "" in       (* acumula linhas para estruturas multi-linha *)

  (try
     while true do
       let raw_line =
         (try input_line ic
          with End_of_file -> raise Exit)
       in
       (* trata linha de continuacao *)
       let line =
         let rlen = String.length raw_line in
         if rlen > 0 && raw_line.[rlen - 1] = '\\' then begin
           acc := !acc ^ String.sub raw_line 0 (rlen - 1);
           ""  (* continua a acumular *)
         end else begin
           let full = !acc ^ raw_line in
           acc := "";
           full
         end
       in
       if line <> "" then
         code := exec_line line
     done
   with
   | Exit -> ()
   | Types.Exit_shell n -> code := n);

  (* executa o que ficou acumulado *)
  if !acc <> "" then
    code := exec_line !acc;

  close_in ic;
  exit !code

(* ------------------------------------------------------------------ *)
(*  Modo stdin                                                          *)
(* ------------------------------------------------------------------ *)

let run_from_stdin () =
  Unix.putenv "0" "myshell";
  Lexer.positional_args := [| |];

  let code = ref 0 in
  (try
     while true do
       let line = (try input_line stdin with End_of_file -> raise Exit) in
       code := exec_line line
     done
   with
   | Exit -> ()
   | Types.Exit_shell n -> code := n);
  exit !code

(* ------------------------------------------------------------------ *)
(*  Entrada principal                                                   *)
(* ------------------------------------------------------------------ *)

let () =
  setup_signals ();

  (* variaveis de ambiente iniciais *)
  Unix.putenv "?" "0";
  (try ignore (Sys.getenv "HOME")
   with Not_found ->
     (try Unix.putenv "HOME" (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
      with _ -> Unix.putenv "HOME" "/"));
  (try ignore (Sys.getenv "PATH")
   with Not_found ->
     Unix.putenv "PATH" "/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin");

  match Array.to_list Sys.argv with
  | [ _ ]              -> run_interactive ()
  | [ _; "-s" ]        -> run_from_stdin ()
  | _ :: "-c" :: cmds  ->
    (* myshell -c "cmd1; cmd2" *)
    let cmd = String.concat " " cmds in
    (try exit (exec_line cmd)
     with Types.Exit_shell n -> exit n)
  | _ :: "--" :: args  ->
    (* myshell -- script.sh arg1 arg2 *)
    (match args with
     | script :: rest -> run_script script rest
     | []             -> run_from_stdin ())
  | _ :: script :: args ->
    run_script script args
  | [] ->
    run_interactive ()
