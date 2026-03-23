(* ================================================================== *)
(*  prompt.ml -- constroi o prompt dinamicamente                     *)
(*                                                                    *)
(*  O prompt mostra:                                                   *)
(*    user@host:directorio (branch_git) [codigo_erro]                *)
(*    $ (verde se sucesso, vermelho se erro)                          *)
(*                                                                    *)
(*  Suporta a variavel $PS1 para personalizacao (formato simplificado):*)
(*    \u = utilizador                                                  *)
(*    \h = hostname (curto)                                            *)
(*    \w = directorio corrente (~ para home)                          *)
(*    \$ = $ para utilizador normal, # para root                      *)
(*    \n = nova linha                                                  *)
(* ================================================================== *)

(* Sequencias de escape ANSI                                           *)
(* Nota: as sequencias \001 e \002 delimitam caracteres nao-imprimiveis*)
(* para que o readline possa calcular o comprimento visual da linha.  *)
(* Sem estes delimitadores, o historico e a edicao de linha ficam     *)
(* desalinhados em terminais com linhas longas.                       *)
let esc_start  = "\001"   (* = \[ em bash *)
let esc_end    = "\002"   (* = \] em bash *)
let wrap c     = esc_start ^ c ^ esc_end

let reset  = wrap "\027[0m"
let bold   = wrap "\027[1m"
let red    = wrap "\027[31m"
let green  = wrap "\027[32m"
let yellow = wrap "\027[33m"
let blue   = wrap "\027[34m"
let cyan   = wrap "\027[36m"

(* ------------------------------------------------------------------ *)
(*  Obtencao do branch git                                             *)
(* ------------------------------------------------------------------ *)

let git_branch () =
  try
    let ic = Unix.open_process_in
      "git symbolic-ref --short HEAD 2>/dev/null || \
       git rev-parse --short HEAD 2>/dev/null" in
    let branch = input_line ic in
    ignore (Unix.close_process_in ic);
    if branch = "" then None else Some (String.trim branch)
  with _ -> None

(* ------------------------------------------------------------------ *)
(*  Encurtar o caminho                                                 *)
(*                                                                    *)
(*  /home/user/a/b/c  ->  ~/a/b/c                                    *)
(*  ~/muito/longo/caminho/aqui  ->  ~/muito/.../aqui                  *)
(* ------------------------------------------------------------------ *)

let shorten_path path =
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  let p =
    if home <> "" &&
       String.length path >= String.length home &&
       String.sub path 0 (String.length home) = home
    then "~" ^ String.sub path (String.length home)
                (String.length path - String.length home)
    else path
  in
  (* encurta se tiver mais de 4 segmentos *)
  let parts = String.split_on_char '/' p
              |> List.filter (fun s -> s <> "") in
  let n = List.length parts in
  if n <= 3 then p
  else begin
    let first  = List.nth parts 0 in
    let last   = List.nth parts (n - 1) in
    let second = List.nth parts (n - 2) in
    first ^ "/.../" ^ second ^ "/" ^ last
  end

(* ------------------------------------------------------------------ *)
(*  Interpretar $PS1 personalizado                                     *)
(* ------------------------------------------------------------------ *)

let interpret_ps1 ps1 =
  let buf = Buffer.create 64 in
  let len = String.length ps1 in
  let i   = ref 0 in
  while !i < len do
    if ps1.[!i] = '\\' && !i + 1 < len then begin
      incr i;
      (match ps1.[!i] with
       | 'u' -> Buffer.add_string buf (try Sys.getenv "USER" with Not_found -> "user")
       | 'h' ->
         (try
            let ic = Unix.open_process_in "hostname -s 2>/dev/null" in
            let h  = input_line ic in
            ignore (Unix.close_process_in ic);
            Buffer.add_string buf h
          with _ -> Buffer.add_string buf "localhost")
       | 'H' ->
         (try
            let ic = Unix.open_process_in "hostname 2>/dev/null" in
            let h  = input_line ic in
            ignore (Unix.close_process_in ic);
            Buffer.add_string buf h
          with _ -> Buffer.add_string buf "localhost")
       | 'w' -> Buffer.add_string buf (Sys.getcwd () |> shorten_path)
       | 'W' ->
         let cwd   = Sys.getcwd () in
         let parts = String.split_on_char '/' cwd in
         Buffer.add_string buf (List.nth parts (List.length parts - 1))
       | '$' ->
         let uid = Unix.getuid () in
         Buffer.add_char buf (if uid = 0 then '#' else '$')
       | 'n' -> Buffer.add_char buf '\n'
       | 't' ->
         let t = Unix.localtime (Unix.time ()) in
         Buffer.add_string buf
           (Printf.sprintf "%02d:%02d:%02d" t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec)
       | 'd' ->
         let t = Unix.localtime (Unix.time ()) in
         let dias = [|"Dom";"Seg";"Ter";"Qua";"Qui";"Sex";"Sab"|] in
         let meses = [|"Jan";"Fev";"Mar";"Abr";"Mai";"Jun";
                       "Jul";"Ago";"Set";"Out";"Nov";"Dez"|] in
         Buffer.add_string buf
           (Printf.sprintf "%s %s %d"
              dias.(t.Unix.tm_wday)
              meses.(t.Unix.tm_mon)
              t.Unix.tm_mday)
       | '\\' -> Buffer.add_char buf '\\'
       | c    -> Buffer.add_char buf '\\'; Buffer.add_char buf c);
      incr i
    end else begin
      Buffer.add_char buf ps1.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(*  Construcao do prompt padrao                                        *)
(* ------------------------------------------------------------------ *)

let build () =
  (* verifica se o utilizador definiu $PS1 personalizado *)
  (match (try Some (Sys.getenv "PS1") with Not_found -> None) with
   | Some ps1 -> interpret_ps1 ps1
   | None ->
     (* prompt padrao: user@host:dir (branch) [code]\n$ *)
     let cwd   = Sys.getcwd () |> shorten_path in
     let code  = !Builtins.last_exit_code in
     let user  = try Sys.getenv "USER" with Not_found -> "user" in
     let host  =
       (try
          let ic = Unix.open_process_in "hostname -s 2>/dev/null" in
          let h  = input_line ic in
          ignore (Unix.close_process_in ic);
          String.trim h
        with _ -> "localhost")
     in
     let git_part = match git_branch () with
       | None   -> ""
       | Some b -> Printf.sprintf " %s(%s)%s" yellow b reset
     in
     let code_part =
       if code = 0 then ""
       else Printf.sprintf " %s[%d]%s" red code reset
     in
     let uid = Unix.getuid () in
     let sym = if uid = 0 then "#" else "$" in
     Printf.sprintf "%s%s%s@%s%s:%s%s%s%s%s\n%s%s%s "
       bold cyan user host reset
       blue cwd reset
       git_part code_part
       (if code = 0 then green else red) sym reset)

(* Prompt secundario (para continuacao de linha) *)
let build_ps2 () = "> "
