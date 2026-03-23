(* ================================================================== *)
(*  completion.ml -- tab completion                                   *)
(*                                                                    *)
(*  Quando o utilizador carrega Tab, o shell completa:                *)
(*    - o primeiro token: nome de comando (builtin, funcao, $PATH)    *)
(*    - os outros tokens: nomes de ficheiros/directorios              *)
(*                                                                    *)
(*  Algoritmo:                                                         *)
(*    1. Determina o prefixo (o que o utilizador ja escreveu)         *)
(*    2. Determina o contexto (primeiro token vs. argumento)          *)
(*    3. Gera candidatos                                               *)
(*    4. Se so 1 candidato: completa automaticamente                  *)
(*       Se varios: mostra lista ou completa o prefixo comum          *)
(* ================================================================== *)

(* ------------------------------------------------------------------ *)
(*  Candidatos de ficheiros                                            *)
(* ------------------------------------------------------------------ *)

let file_completions prefix =
  (* separa o directorio base do prefixo final *)
  let dir, base =
    match String.rindex_opt prefix '/' with
    | None   -> (".", prefix)
    | Some i ->
      let d = if i = 0 then "/" else String.sub prefix 0 i in
      let b = String.sub prefix (i + 1) (String.length prefix - i - 1) in
      (d, b)
  in
  (try
     let entries = Sys.readdir dir in
     Array.to_list entries
     |> List.filter (fun e ->
         String.length e >= String.length base &&
         String.sub e 0 (String.length base) = base)
     |> List.map (fun e ->
         let full = if dir = "." then e
                    else if dir = "/" then "/" ^ e
                    else dir ^ "/" ^ e in
         (* acrescenta / se for directorio *)
         if Sys.file_exists full && Sys.is_directory full
         then full ^ "/"
         else full)
     |> List.sort String.compare
   with Sys_error _ -> [])

(* ------------------------------------------------------------------ *)
(*  Candidatos de comandos (primeiro token)                            *)
(* ------------------------------------------------------------------ *)

let command_completions prefix =
  (* 1. builtins *)
  let builtins =
    List.filter_map (fun (name, _) ->
      if String.length name >= String.length prefix &&
         String.sub name 0 (String.length prefix) = prefix
      then Some name
      else None
    ) Builtins.table
  in
  (* 2. funcoes definidas *)
  let funcs =
    Hashtbl.fold (fun name _ acc ->
      if String.length name >= String.length prefix &&
         String.sub name 0 (String.length prefix) = prefix
      then name :: acc
      else acc
    ) Parser.functions []
  in
  (* 3. executaveis em $PATH *)
  let path_cmds =
    let path_str = try Sys.getenv "PATH"
                   with Not_found -> "/usr/bin:/bin" in
    let dirs = String.split_on_char ':' path_str in
    List.concat_map (fun dir ->
      (try
         let entries = Sys.readdir dir in
         Array.to_list entries
         |> List.filter (fun e ->
             String.length e >= String.length prefix &&
             String.sub e 0 (String.length prefix) = prefix &&
             (try Unix.access (Filename.concat dir e) [Unix.X_OK]; true
              with _ -> false))
       with Sys_error _ -> [])
    ) dirs
  in
  (* 4. aliases *)
  let alias_names =
    Hashtbl.fold (fun name _ acc ->
      if String.length name >= String.length prefix &&
         String.sub name 0 (String.length prefix) = prefix
      then name :: acc
      else acc
    ) Builtins.aliases []
  in
  let all = builtins @ funcs @ alias_names @ path_cmds in
  List.sort_uniq String.compare all

(* ------------------------------------------------------------------ *)
(*  Prefixo comum de uma lista de strings                              *)
(* ------------------------------------------------------------------ *)

let common_prefix lst =
  match lst with
  | []        -> ""
  | [s]       -> s
  | first :: rest ->
    let len = ref (String.length first) in
    List.iter (fun s ->
      let l = String.length s in
      if l < !len then len := l;
      let i = ref 0 in
      while !i < !len && first.[!i] = s.[!i] do incr i done;
      len := !i
    ) rest;
    String.sub first 0 !len

(* ------------------------------------------------------------------ *)
(*  Completar uma linha dada                                           *)
(*                                                                    *)
(*  Devolve (novo_input, candidatos_para_mostrar)                      *)
(*  Se candidatos = [] e novo_input = input original: nada a fazer    *)
(* ------------------------------------------------------------------ *)

let complete line =
  let parts = String.split_on_char ' ' line
              |> List.filter (fun s -> s <> "") in
  let is_first = List.length parts <= 1 in
  let prefix = match parts with
    | []      -> ""
    | lst     -> List.nth lst (List.length lst - 1)
  in
  let candidates =
    if is_first || String.contains prefix '/'
    then
      (* primeiro token sem /: completa comandos; com /: ficheiros    *)
      if String.contains prefix '/' || not is_first
      then file_completions prefix
      else command_completions prefix
    else file_completions prefix
  in
  match candidates with
  | [] ->
    (* sem candidatos: toca um sino *)
    print_char '\007'; flush stdout;
    (line, [])
  | [single] ->
    (* um candidato: completa directamente *)
    let base_line = if List.length parts <= 1 then ""
                    else String.concat " " (List.rev (List.tl (List.rev parts))) ^ " " in
    (base_line ^ single ^ (if String.length single > 0 &&
                              single.[String.length single - 1] = '/'
                            then "" else " "),
     [])
  | many ->
    let cp = common_prefix many in
    if String.length cp > String.length prefix then begin
      (* completa ate ao prefixo comum *)
      let base_line = if List.length parts <= 1 then ""
                      else String.concat " "
                             (List.rev (List.tl (List.rev parts))) ^ " " in
      (base_line ^ cp, [])
    end else begin
      (* mostra candidatos *)
      (line, many)
    end

(* ------------------------------------------------------------------ *)
(*  Mostrar candidatos em colunas                                      *)
(* ------------------------------------------------------------------ *)

let display_completions candidates =
  let max_len = List.fold_left
    (fun acc s -> max acc (String.length s)) 0 candidates in
  let col_w = max_len + 2 in
  let term_w = 80 in  (* fallback *)
  let cols   = max 1 (term_w / col_w) in
  print_char '\n';
  List.iteri (fun i s ->
    Printf.printf "%-*s" col_w s;
    if (i + 1) mod cols = 0 then print_char '\n'
  ) candidates;
  if List.length candidates mod cols <> 0 then print_char '\n';
  flush stdout
