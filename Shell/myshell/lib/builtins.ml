(* ================================================================== *)
(*  builtins.ml -- comandos geridos internamente pelo shell           *)
(*                                                                    *)
(*  Builtins sao comandos que TEM de correr no processo do shell      *)
(*  (nao num processo filho), porque modificam o estado do shell:     *)
(*    cd     -- muda o directorio corrente                            *)
(*    export -- define variaveis de ambiente                          *)
(*    exit   -- termina o shell                                       *)
(*    source -- executa um script no contexto actual                  *)
(*                                                                    *)
(*  Outros builtins existem por desempenho (echo, pwd) ou porque      *)
(*  precisam de aceder ao estado interno do shell (alias, jobs).      *)
(* ================================================================== *)

open Types

(* ------------------------------------------------------------------ *)
(*  Estado global do shell                                             *)
(* ------------------------------------------------------------------ *)

(** Codigo de saida do ultimo comando executado ($?)                   *)
let last_exit_code : int ref = ref 0

(** Tabela de aliases: nome -> string de expansao                      *)
let aliases : (string, string) Hashtbl.t = Hashtbl.create 16

(** Lista de jobs geridos pelo shell                                   *)
let jobs : job list ref = ref []

(** Contador para IDs de jobs *)
let next_job_id = ref 1

(* ------------------------------------------------------------------ *)
(*  Gestao de jobs                                                     *)
(* ------------------------------------------------------------------ *)

let add_job pid pids desc =
  let jid = !next_job_id in
  incr next_job_id;
  let j = { job_id = jid; job_pid = pid; job_pids = pids;
            job_desc = desc; job_status = Running } in
  jobs := j :: !jobs;
  Printf.printf "[%d] %d\n%!" jid pid;
  j

let remove_job pid =
  jobs := List.filter (fun j -> j.job_pid <> pid) !jobs

let find_job_by_id jid =
  List.find_opt (fun j -> j.job_id = jid) !jobs

let find_job_by_pid pid =
  List.find_opt (fun j -> j.job_pid = pid) !jobs

(* ------------------------------------------------------------------ *)
(*  Resolucao de executaveis via $PATH                                 *)
(*                                                                    *)
(*  Percorre os directorios em $PATH da esquerda para a direita,      *)
(*  devolvendo o primeiro caminho absoluto onde o executavel existe   *)
(*  e tem permissao de execucao.                                      *)
(* ------------------------------------------------------------------ *)

let find_in_path cmd =
  if String.contains cmd '/' then
    if Sys.file_exists cmd then Some cmd else None
  else begin
    let path_str = try Sys.getenv "PATH"
                   with Not_found -> "/usr/local/bin:/usr/bin:/bin" in
    let dirs = String.split_on_char ':' path_str in
    List.find_opt
      (fun dir ->
         let full = Filename.concat dir cmd in
         Sys.file_exists full &&
         (try Unix.access full [Unix.X_OK]; true
          with Unix.Unix_error _ -> false))
      dirs
    |> Option.map (fun dir -> Filename.concat dir cmd)
  end

(* ------------------------------------------------------------------ *)
(*  Implementacao dos builtins                                         *)
(* ------------------------------------------------------------------ *)

let builtin_cd args =
  let target = match args with
    | []      -> (try Sys.getenv "HOME" with Not_found -> "/")
    | ["-"]   -> (try Sys.getenv "OLDPWD" with Not_found -> ".")
    | dir :: _ -> dir
  in
  let old = Sys.getcwd () in
  (try
     Unix.chdir target;
     Unix.putenv "OLDPWD" old;
     Unix.putenv "PWD" (Sys.getcwd ());
     0
   with Unix.Unix_error (e, _, _) ->
     Printf.eprintf "cd: %s: %s\n%!" target (Unix.error_message e); 1)

let builtin_pwd _args =
  print_endline (Sys.getcwd ()); 0

let builtin_echo args =
  let rec go no_nl = function
    | "-n" :: rest -> go true rest
    | words ->
      let s = String.concat " " words in
      (* processar escapes \n \t etc se -e estivesse activo *)
      if no_nl then (print_string s; flush stdout)
      else print_endline s;
      0
  in
  go false args

let builtin_printf args =
  match args with
  | [] -> 0
  | fmt :: rest ->
    (* implementacao basica de printf *)
    let s = ref fmt in
    let r = ref rest in
    let buf = Buffer.create 64 in
    let i = ref 0 in
    let len = String.length !s in
    while !i < len do
      if !s.[!i] = '%' && !i + 1 < len then begin
        incr i;
        (match !s.[!i] with
         | 's' ->
           (match !r with
            | v :: rest' -> Buffer.add_string buf v; r := rest'
            | [] -> ())
         | 'd' ->
           (match !r with
            | v :: rest' ->
              (try Buffer.add_string buf (string_of_int (int_of_string v))
               with _ -> Buffer.add_string buf v);
              r := rest'
            | [] -> ())
         | 'f' ->
           (match !r with
            | v :: rest' ->
              (try Buffer.add_string buf (Printf.sprintf "%f" (float_of_string v))
               with _ -> Buffer.add_string buf v);
              r := rest'
            | [] -> ())
         | '%' -> Buffer.add_char buf '%'
         | c   -> Buffer.add_char buf '%'; Buffer.add_char buf c);
        incr i
      end else if !s.[!i] = '\\' && !i + 1 < len then begin
        incr i;
        (match !s.[!i] with
         | 'n' -> Buffer.add_char buf '\n'
         | 't' -> Buffer.add_char buf '\t'
         | '\\' -> Buffer.add_char buf '\\'
         | c    -> Buffer.add_char buf '\\'; Buffer.add_char buf c);
        incr i
      end else begin
        Buffer.add_char buf !s.[!i]; incr i
      end
    done;
    ignore s;
    print_string (Buffer.contents buf);
    flush stdout;
    0

let builtin_export args =
  let show_all () =
    (* lista todas as variaveis de ambiente *)
    Array.iter (fun e -> Printf.printf "export %s\n" e) (Unix.environment ())
  in
  (match args with
   | [] -> show_all ()
   | _  ->
     List.iter (fun arg ->
       match String.split_on_char '=' arg with
       | [name] ->
         (* export VAR -- exporta sem mudar o valor *)
         (try Unix.putenv name (Sys.getenv name)
          with Not_found -> Unix.putenv name "")
       | name :: values ->
         Unix.putenv name (String.concat "=" values)
       | [] -> ()
     ) args);
  0

let builtin_unset args =
  List.iter (fun name ->
    (* OCaml nao tem Unix.unsetenv; usamos putenv com string vazia    *)
    (try Unix.putenv name "" with _ -> ())
  ) args;
  0

let builtin_readonly args =
  (* simplificado: trata como export *)
  builtin_export args

let builtin_local args =
  (* em funcoes: declara variaveis locais. Aqui e alias de export.    *)
  (* Uma implementacao completa usaria um stack de scopes.            *)
  builtin_export args

let builtin_alias args =
  (match args with
   | [] ->
     Hashtbl.iter (fun k v ->
       Printf.printf "alias %s='%s'\n" k v) aliases
   | _  ->
     List.iter (fun arg ->
       match String.split_on_char '=' arg with
       | [name] ->
         (match Hashtbl.find_opt aliases name with
          | Some v -> Printf.printf "alias %s='%s'\n" name v
          | None   -> Printf.eprintf "alias: %s: nao definido\n" name)
       | name :: values ->
         Hashtbl.replace aliases name (String.concat "=" values)
       | [] -> ()
     ) args);
  0

let builtin_unalias args =
  List.iter (fun name ->
    if name = "-a" then Hashtbl.clear aliases
    else Hashtbl.remove aliases name
  ) args;
  0

let builtin_jobs _args =
  if !jobs = [] then ()
  else
    List.iter (fun j ->
      let status_str = match j.job_status with
        | Running   -> "Running"
        | Stopped   -> "Stopped"
        | Done n    -> Printf.sprintf "Done(%d)" n
      in
      Printf.printf "[%d]  %-10s  %s\n" j.job_id status_str j.job_desc
    ) (List.rev !jobs);
  0

let builtin_fg args =
  let jid = match args with
    | []     -> (match !jobs with [] -> None | j :: _ -> Some j.job_id)
    | s :: _ -> (try Some (int_of_string (String.sub s 1 (String.length s - 1)))
                 with _ -> try Some (int_of_string s) with _ -> None)
  in
  (match jid with
   | None ->
     Printf.eprintf "fg: sem jobs\n%!"; 1
   | Some id ->
     (match find_job_by_id id with
      | None ->
        Printf.eprintf "fg: [%d]: job nao encontrado\n%!" id; 1
      | Some j ->
        (* resume e espera *)
        Printf.printf "%s\n%!" j.job_desc;
        Unix.kill j.job_pid Sys.sigcont;
        let _, status = Unix.waitpid [] j.job_pid in
        remove_job j.job_pid;
        (match status with
         | Unix.WEXITED n   -> n
         | Unix.WSIGNALED _ -> 130
         | Unix.WSTOPPED  _ -> 0)))

let builtin_bg args =
  let jid = match args with
    | []     -> (match !jobs with [] -> None | j :: _ -> Some j.job_id)
    | s :: _ -> (try Some (int_of_string (String.sub s 1 (String.length s - 1)))
                 with _ -> try Some (int_of_string s) with _ -> None)
  in
  (match jid with
   | None ->
     Printf.eprintf "bg: sem jobs\n%!"; 1
   | Some id ->
     (match find_job_by_id id with
      | None ->
        Printf.eprintf "bg: [%d]: job nao encontrado\n%!" id; 1
      | Some j ->
        Unix.kill j.job_pid Sys.sigcont;
        j.job_status <- Running;
        Printf.printf "[%d] %s &\n%!" j.job_id j.job_desc;
        0))

let builtin_wait args =
  (match args with
   | [] ->
     (* espera por todos os jobs *)
     List.iter (fun j ->
       (try ignore (Unix.waitpid [] j.job_pid) with _ -> ())
     ) !jobs;
     jobs := []
   | pids ->
     List.iter (fun s ->
       (try
          let pid = int_of_string s in
          ignore (Unix.waitpid [] pid);
          remove_job pid
        with _ -> ())
     ) pids);
  0

let builtin_kill args =
  let sig_num = ref Sys.sigterm in
  let pids    = ref [] in
  List.iter (fun arg ->
    if String.length arg > 1 && arg.[0] = '-' then begin
      let sname = String.sub arg 1 (String.length arg - 1) in
      sig_num := (match sname with
        | "9"    | "KILL" -> Sys.sigkill
        | "15"   | "TERM" -> Sys.sigterm
        | "1"    | "HUP"  -> Sys.sighup
        | "2"    | "INT"  -> Sys.sigint
        | "STOP"          -> Sys.sigstop
        | "CONT"          -> Sys.sigcont
        | _               ->
          (try int_of_string sname
           with _ -> Sys.sigterm))
    end else
      (try pids := int_of_string arg :: !pids
       with _ -> Printf.eprintf "kill: %s: argumento invalido\n%!" arg)
  ) args;
  List.iter (fun pid ->
    try Unix.kill pid !sig_num
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "kill: %d: %s\n%!" pid (Unix.error_message e)
  ) !pids;
  0

let builtin_type args =
  List.iter (fun cmd ->
    if Hashtbl.mem aliases cmd then
      Printf.printf "%s: alias para '%s'\n" cmd (Hashtbl.find aliases cmd)
    else if Parser.lookup_function cmd <> None then
      Printf.printf "%s: funcao\n" cmd
    else
      match find_in_path cmd with
      | Some path -> Printf.printf "%s is %s\n" cmd path
      | None      -> Printf.eprintf "type: %s: nao encontrado\n" cmd
  ) args;
  0

let builtin_which args =
  List.iter (fun cmd ->
    match find_in_path cmd with
    | Some path -> print_endline path
    | None      -> Printf.eprintf "which: %s: nao encontrado\n" cmd
  ) args;
  0

let builtin_true  _args = 0
let builtin_false _args = 1

let builtin_test args =
  (* implementacao basica de [ / test *)
  let args = match args with
    | _ :: _ when List.nth args (List.length args - 1) = "]" ->
      (* remove o ] final *)
      let n = List.length args in
      let arr = Array.of_list args in
      Array.to_list (Array.sub arr 0 (n - 1))
    | _ -> args
  in
  (match args with
   | [s] -> if s <> "" then 0 else 1           (* [ STRING ]        *)
   | ["-n"; s] -> if s <> "" then 0 else 1     (* [ -n STRING ]     *)
   | ["-z"; s] -> if s = "" then 0 else 1      (* [ -z STRING ]     *)
   | ["-e"; f] -> if Sys.file_exists f then 0 else 1
   | ["-f"; f] ->
     if Sys.file_exists f && not (Sys.is_directory f) then 0 else 1
   | ["-d"; f] ->
     if Sys.file_exists f && Sys.is_directory f then 0 else 1
   | ["-r"; f] ->
     (try Unix.access f [Unix.R_OK]; 0 with _ -> 1)
   | ["-w"; f] ->
     (try Unix.access f [Unix.W_OK]; 0 with _ -> 1)
   | ["-x"; f] ->
     (try Unix.access f [Unix.X_OK]; 0 with _ -> 1)
   | [a; "=";  b] -> if a = b then 0 else 1
   | [a; "!="; b] -> if a <> b then 0 else 1
   | [a; "-eq"; b] ->
     (try if int_of_string a = int_of_string b then 0 else 1
      with _ -> 2)
   | [a; "-ne"; b] ->
     (try if int_of_string a <> int_of_string b then 0 else 1
      with _ -> 2)
   | [a; "-lt"; b] ->
     (try if int_of_string a < int_of_string b then 0 else 1
      with _ -> 2)
   | [a; "-le"; b] ->
     (try if int_of_string a <= int_of_string b then 0 else 1
      with _ -> 2)
   | [a; "-gt"; b] ->
     (try if int_of_string a > int_of_string b then 0 else 1
      with _ -> 2)
   | [a; "-ge"; b] ->
     (try if int_of_string a >= int_of_string b then 0 else 1
      with _ -> 2)
   | _ -> 1)

let builtin_read args =
  (* read VAR -- le uma linha do stdin para a variavel *)
  let prompt = ref "" in
  let vars   = ref [] in
  let rec proc = function
    | "-p" :: p :: rest -> prompt := p; proc rest
    | v    :: rest      -> vars   := v :: !vars; proc rest
    | []                -> ()
  in
  proc args;
  let vars = List.rev !vars in
  if !prompt <> "" then (print_string !prompt; flush stdout);
  (try
     let line = input_line stdin in
     (match vars with
      | []    -> Unix.putenv "REPLY" line
      | [v]   -> Unix.putenv v line
      | v :: _ ->
        (* distribui palavras pelas variaveis *)
        let parts = String.split_on_char ' ' line
                    |> List.filter (fun s -> s <> "") in
        let n_vars = List.length vars in
        let n_parts = List.length parts in
        List.iteri (fun i var ->
          if i < n_parts - 1 || i < n_vars - 1 then
            Unix.putenv var (try List.nth parts i with _ -> "")
          else
            (* ultima variavel recebe o resto *)
            let rest = String.concat " "
              (List.filteri (fun j _ -> j >= i) parts) in
            Unix.putenv v rest
        ) vars;
        ignore v);
     0
   with End_of_file -> 1)

(* Referencia ao executor - preenchida por executor.ml ao iniciar.  *)
(* Quebra a dependencia circular builtins -> executor.                *)
let eval_ref : (Types.ast -> int) ref = ref (fun _ -> 0)

let builtin_source args =
  (* . file / source file -- executa o ficheiro no contexto actual    *)
  match args with
  | [] -> Printf.eprintf "source: uso: source ficheiro\n%!"; 1
  | file :: _ ->
    if not (Sys.file_exists file) then begin
      Printf.eprintf "source: %s: ficheiro nao encontrado\n%!" file; 1
    end else begin
      let ic = open_in file in
      let code = ref 0 in
      (try
         while true do
           let line = input_line ic in
           let line = String.trim line in
           if line <> "" && (String.length line = 0 || line.[0] <> '#')
           then begin
             match Parser.parse line with
             | None     -> ()
             | Some ast ->
               (* executor e importado pela dependencia circular;       *)
               (* usamos referencia funcional para quebrar o ciclo      *)
               (try code := !eval_ref ast
                with Shell_error msg ->
                  Printf.eprintf "source: %s\n%!" msg;
                  code := 1)
           end
         done
       with End_of_file -> ());
      close_in ic;
      !code
    end


let builtin_exit args =
  let code = match args with
    | []    -> !last_exit_code
    | s :: _ -> (try int_of_string s with _ -> 0)
  in
  raise (Exit_shell code)

let builtin_return args =
  (* dentro de funcoes: define o codigo de retorno                     *)
  let code = match args with
    | []    -> !last_exit_code
    | s :: _ -> (try int_of_string s with _ -> 0)
  in
  raise (Exit_shell code)

let builtin_shift args =
  let n = match args with
    | []    -> 1
    | s :: _ -> (try int_of_string s with _ -> 1)
  in
  let arr = !Lexer.positional_args in
  let len = Array.length arr in
  if n >= len then
    Lexer.positional_args := [| |]
  else
    Lexer.positional_args := Array.sub arr n (len - n);
  0

let builtin_set args =
  match args with
  | []          -> Array.iter print_endline (Unix.environment ()); 0
  | "--" :: rest ->
    Lexer.positional_args := Array.of_list rest; 0
  | _           -> 0

let builtin_eval args =
  let cmd = String.concat " " args in
  (match Parser.parse cmd with
   | None     -> 0
   | Some ast ->
     (try !eval_ref ast
      with Shell_error msg ->
        Printf.eprintf "eval: %s\n%!" msg; 1))

let builtin_sleep args =
  (match args with
   | []    -> ()
   | s :: _ ->
     (try
        let secs = float_of_string s in
        Unix.sleepf secs
      with _ -> ()));
  0

let builtin_history args =
  let entries = History.to_list () in
  let entries = List.rev entries in  (* mais antigos primeiro *)
  (match args with
   | ["-c"] ->
     (* limpa historico *)
     Queue.clear History.history; 0
   | _ ->
     List.iteri (fun i line ->
       Printf.printf " %4d  %s\n" (i + 1) line
     ) entries;
     0)

(* ------------------------------------------------------------------ *)
(*  Expansao de aliases                                                *)
(*                                                                    *)
(*  Expande aliases recursivamente ate max_depth niveis.              *)
(*  Protege contra loops (alias a='b'; alias b='a').                  *)
(* ------------------------------------------------------------------ *)

let rec expand_alias ?(depth = 0) argv =
  if depth > 8 then argv
  else match argv with
    | [] -> []
    | cmd :: rest ->
      (match Hashtbl.find_opt aliases cmd with
       | None           -> argv
       | Some expansion ->
         let parts = String.split_on_char ' ' expansion
                     |> List.filter (fun s -> s <> "") in
         (* evita loop: se a expansao comecar com o mesmo nome, para   *)
         if parts <> [] && List.hd parts = cmd then argv
         else expand_alias ~depth:(depth + 1) (parts @ rest))

(* ------------------------------------------------------------------ *)
(*  Tabela de dispatch                                                  *)
(* ------------------------------------------------------------------ *)

let table : (string * (string list -> int)) list = [
  "cd",       builtin_cd;
  "pwd",      builtin_pwd;
  "echo",     builtin_echo;
  "printf",   builtin_printf;
  "export",   builtin_export;
  "unset",    builtin_unset;
  "readonly", builtin_readonly;
  "local",    builtin_local;
  "alias",    builtin_alias;
  "unalias",  builtin_unalias;
  "jobs",     builtin_jobs;
  "fg",       builtin_fg;
  "bg",       builtin_bg;
  "wait",     builtin_wait;
  "kill",     builtin_kill;
  "type",     builtin_type;
  "which",    builtin_which;
  "true",     builtin_true;
  "false",    builtin_false;
  "test",     builtin_test;
  "[",        builtin_test;
  "read",     builtin_read;
  "source",   builtin_source;
  ".",        builtin_source;
  "exit",     builtin_exit;
  "quit",     builtin_exit;
  "return",   builtin_return;
  "shift",    builtin_shift;
  "set",      builtin_set;
  "eval",     builtin_eval;
  "sleep",    builtin_sleep;
  "history",  builtin_history;
  ":",        (fun _ -> 0);   (* no-op                                  *)
  "break",    builtin_break;
  "continue",  builtin_continue;
]

let is_builtin name = List.assoc_opt name table <> None

let run name args =
  match List.assoc_opt name table with
  | Some f -> f args
  | None   -> raise (Shell_error (Printf.sprintf "%s: nao e um builtin" name))

let builtin_break _args =
  raise Types.Break_loop

let builtin_continue _args =
  raise Types.Continue_loop
