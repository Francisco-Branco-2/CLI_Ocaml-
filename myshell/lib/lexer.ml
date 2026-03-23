(* ================================================================== *)
(*  lexer.ml -- tokenizacao e expansao de palavras                   *)
(*                                                                    *)
(*  Responsabilidades:                                                 *)
(*    1. Tokenizar a linha de input em tokens discretos               *)
(*    2. Expandir variaveis ($VAR, ${VAR}, ${VAR:-default}, $?)       *)
(*    3. Expandir til (~, ~/dir, ~user)                               *)
(*    4. Expandir globs (*, ?, [a-z])                                  *)
(*    5. Expandir chaves ({a,b,c}, {1..3})                            *)
(*    6. Command substitution ($(...) e `...`)                        *)
(*    7. Reconhecer here-documents (<<)                               *)
(*                                                                    *)
(*  Ordem de expansao POSIX (secao 2.6):                              *)
(*    brace -> tilde -> variaveis -> cmd-subst -> glob                *)
(* ================================================================== *)

(* ------------------------------------------------------------------ *)
(*  Tipo token                                                          *)
(* ------------------------------------------------------------------ *)

type token =
  | TWord      of string
  | TNewline
  | TPipe                  (* |   *)
  | TRedirIn               (* <   *)
  | TRedirOut              (* >   *)
  | TRedirApp              (* >>  *)
  | TRedirErr              (* 2>  *)
  | TRedirErr2             (* 2>> *)
  | TRedirBoth             (* 2>&1*)
  | THeredoc               (* <<  *)
  | TAnd                   (* &&  *)
  | TOr                    (* ||  *)
  | TSemi                  (* ;   *)
  | TDSemi                 (* ;;  *)
  | TBg                    (* &   *)
  | TLParen                (* (   *)
  | TRParen                (* )   *)
  | TLBrace                (* {   *)
  | TRBrace                (* }   *)
  | TIf | TThen | TElse | TElif | TFi
  | TWhile | TUntil | TDo | TDone
  | TFor | TIn
  | TCase | TEsac
  | TFunction
  | TEOF

let keyword_of_string = function
  | "if"       -> Some TIf
  | "then"     -> Some TThen
  | "else"     -> Some TElse
  | "elif"     -> Some TElif
  | "fi"       -> Some TFi
  | "while"    -> Some TWhile
  | "until"    -> Some TUntil
  | "do"       -> Some TDo
  | "done"     -> Some TDone
  | "for"      -> Some TFor
  | "in"       -> Some TIn
  | "case"     -> Some TCase
  | "esac"     -> Some TEsac
  | "function" -> Some TFunction
  | _          -> None

(* ------------------------------------------------------------------ *)
(*  Argumentos posicionais ($1..$9, $#, $@, $*)                       *)
(* ------------------------------------------------------------------ *)

let positional_args : string array ref = ref [| |]

(* ------------------------------------------------------------------ *)
(*  Command substitution                                               *)
(*                                                                    *)
(*  Executa o comando num subprocesso e captura o stdout.              *)
(*  Passos:                                                             *)
(*    1. pipe() cria (fd_read, fd_write)                              *)
(*    2. fork(): filho redirige stdout para fd_write, execv            *)
(*    3. pai: fecha fd_write, le de fd_read ate EOF, waitpid          *)
(* ------------------------------------------------------------------ *)

let command_substitution cmd =
  try
    let (pipe_r, pipe_w) = Unix.pipe () in
    (match Unix.fork () with
     | 0 ->
       Unix.close pipe_r;
       Unix.dup2 pipe_w Unix.stdout;
       Unix.close pipe_w;
       (try Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
        with _ -> exit 127)
     | pid ->
       Unix.close pipe_w;
       let buf = Buffer.create 256 in
       let tmp = Bytes.create 4096 in
       let go  = ref true in
       while !go do
         let n = Unix.read pipe_r tmp 0 4096 in
         if n = 0 then go := false
         else Buffer.add_subbytes buf tmp 0 n
       done;
       Unix.close pipe_r;
       ignore (Unix.waitpid [] pid);
       (* Remove trailing newlines (comportamento POSIX) *)
       let s = Buffer.contents buf in
       let len = String.length s in
       let e = ref (len - 1) in
       while !e >= 0 && s.[!e] = '\n' do decr e done;
       if !e < 0 then "" else String.sub s 0 (!e + 1))
  with _ -> ""

(* ------------------------------------------------------------------ *)
(*  Expansao de variaveis                                              *)
(*                                                                    *)
(*  Suporta:                                                            *)
(*    $VAR          -- variavel simples                                *)
(*    ${VAR}        -- variavel com delimitadores                      *)
(*    ${VAR:-def}   -- default se VAR indefinida ou vazia              *)
(*    ${VAR:+val}   -- val se VAR definida e nao vazia                 *)
(*    ${#VAR}       -- comprimento do valor de VAR                     *)
(*    $?            -- codigo de saida do ultimo comando               *)
(*    $$            -- PID do shell                                    *)
(*    $#            -- numero de argumentos posicionais                *)
(*    $0..$9        -- argumentos posicionais                          *)
(* ------------------------------------------------------------------ *)

let expand_special_var var =
  match var with
  | "?"  -> (try Sys.getenv "?" with Not_found -> "0")
  | "$"  -> string_of_int (Unix.getpid ())
  | "#"  -> string_of_int (Array.length !positional_args)
  | "@"  | "*" -> String.concat " " (Array.to_list !positional_args)
  | "!"  -> (try Sys.getenv "!" with Not_found -> "")
  | _    ->
    (try
       let n = int_of_string var in
       if n = 0 then (try Sys.getenv "0" with Not_found -> "myshell")
       else if n <= Array.length !positional_args
       then !positional_args.(n - 1)
       else ""
     with Failure _ ->
       try Sys.getenv var with Not_found -> "")

let rec expand_vars s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i   = ref 0 in
  while !i < len do
    if s.[!i] = '$' then begin
      incr i;
      if !i >= len then Buffer.add_char buf '$'
      else match s.[!i] with
        | '{' ->
          (* ${...} *)
          incr i;
          let start = !i in
          (* encontra } correspondente, respeitando aninhamento *)
          let depth = ref 1 in
          while !i < len && !depth > 0 do
            if      s.[!i] = '{' then incr depth
            else if s.[!i] = '}' then decr depth;
            if !depth > 0 then incr i else ()
          done;
          let inner = String.sub s start (!i - start) in
          if !i < len then incr i;  (* consome } *)
          (* processa modificadores *)
          let result =
            if String.length inner > 0 && inner.[0] = '#' then begin
              (* ${#VAR} -- comprimento *)
              let var = String.sub inner 1 (String.length inner - 1) in
              let v = try Sys.getenv var with Not_found -> "" in
              string_of_int (String.length v)
            end else begin
              (* procura :- :+ := :? *)
              let colon_idx =
                let j = ref (-1) in
                String.iteri (fun k c ->
                  if c = ':' && !j = -1 then j := k) inner;
                !j
              in
              if colon_idx >= 0 && colon_idx + 1 < String.length inner then begin
                let var  = String.sub inner 0 colon_idx in
                let modi = inner.[colon_idx + 1] in
                let arg  = String.sub inner (colon_idx + 2)
                             (String.length inner - colon_idx - 2) in
                let v = try Sys.getenv var with Not_found -> "" in
                match modi with
                | '-' -> if v = "" then expand_vars arg else v
                | '+' -> if v = "" then "" else expand_vars arg
                | '?' ->
                  if v = "" then begin
                    Printf.eprintf "myshell: %s: %s\n%!" var arg;
                    raise (Types.Shell_error (var ^ ": " ^ arg))
                  end else v
                | '=' ->
                  if v = "" then begin
                    let nv = expand_vars arg in
                    Unix.putenv var nv; nv
                  end else v
                | _   -> expand_special_var var
              end else
                expand_special_var inner
            end
          in
          Buffer.add_string buf result
        | '(' ->
          (* $(...) -- command substitution *)
          incr i;
          let depth  = ref 1 in
          let cb     = Buffer.create 32 in
          while !i < len && !depth > 0 do
            if      s.[!i] = '(' then (incr depth; Buffer.add_char cb s.[!i])
            else if s.[!i] = ')' then (decr depth;
              if !depth > 0 then Buffer.add_char cb s.[!i])
            else Buffer.add_char cb s.[!i];
            incr i
          done;
          let output = command_substitution (Buffer.contents cb) in
          Buffer.add_string buf output
        | '?' | '$' | '#' | '@' | '*' | '!' ->
          let var = String.make 1 s.[!i] in
          incr i;
          Buffer.add_string buf (expand_special_var var)
        | c when (c >= '0' && c <= '9') ->
          let var = String.make 1 c in
          incr i;
          Buffer.add_string buf (expand_special_var var)
        | _ ->
          (* $VAR simples *)
          let start = !i in
          while !i < len &&
                (let c = s.[!i] in
                 (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                 (c >= '0' && c <= '9') || c = '_') do
            incr i
          done;
          let var = String.sub s start (!i - start) in
          if var = "" then Buffer.add_char buf '$'
          else Buffer.add_string buf (expand_special_var var)
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(*  Expansao de til                                                    *)
(* ------------------------------------------------------------------ *)

let expand_tilde s =
  let home = try Sys.getenv "HOME" with Not_found -> "/" in
  if s = "~" then home
  else if String.length s >= 2 && s.[0] = '~' && s.[1] = '/' then
    home ^ String.sub s 1 (String.length s - 1)
  else if String.length s >= 1 && s.[0] = '~' then begin
    let user = String.sub s 1 (String.length s - 1) in
    (try (Unix.getpwnam user).Unix.pw_dir
     with Not_found -> s)
  end else s

(* ------------------------------------------------------------------ *)
(*  Expansao de chaves                                                 *)
(*                                                                    *)
(*  {a,b,c}   -> ["a";"b";"c"]                                       *)
(*  {1..5}    -> ["1";"2";"3";"4";"5"]                               *)
(*  {a..e}    -> ["a";"b";"c";"d";"e"]                               *)
(*  pre{a,b}  -> ["prea";"preb"]                                      *)
(* ------------------------------------------------------------------ *)

let rec brace_expand word =
  let len = String.length word in
  if not (String.contains word '{') then [word]
  else begin
    (* encontra { mais a esquerda fora de aspas *)
    let open_p  = ref (-1) in
    let close_p = ref (-1) in
    let depth   = ref 0 in
    let j = ref 0 in
    while !j < len && !close_p = -1 do
      (match word.[!j] with
       | '{' ->
         if !open_p = -1 then open_p := !j;
         incr depth
       | '}' ->
         decr depth;
         if !depth = 0 && !open_p >= 0 then close_p := !j
       | _ -> ());
      incr j
    done;
    if !open_p = -1 || !close_p = -1 then [word]
    else begin
      let pre  = String.sub word 0 !open_p in
      let body = String.sub word (!open_p + 1) (!close_p - !open_p - 1) in
      let suf  = String.sub word (!close_p + 1) (len - !close_p - 1) in
      (* detecta range {n..m} *)
      let parts =
        match String.split_on_char '.' body with
        | [a; ""; b] ->
          (try
             let n1 = int_of_string (String.trim a) in
             let n2 = int_of_string (String.trim b) in
             let step = if n1 <= n2 then 1 else -1 in
             let acc  = ref [] in
             let k    = ref n1 in
             while (if step > 0 then !k <= n2 else !k >= n2) do
               acc := string_of_int !k :: !acc;
               k   := !k + step
             done;
             List.rev !acc
           with Failure _ ->
             (* range de caracteres {a..z} *)
             if String.length (String.trim a) = 1 &&
                String.length (String.trim b) = 1
             then begin
               let c1 = Char.code (String.trim a).[0] in
               let c2 = Char.code (String.trim b).[0] in
               let step = if c1 <= c2 then 1 else -1 in
               let acc = ref [] in
               let k = ref c1 in
               while (if step > 0 then !k <= c2 else !k >= c2) do
                 acc := String.make 1 (Char.chr !k) :: !acc;
                 k   := !k + step
               done;
               List.rev !acc
             end else [body])
        | _ ->
          (* {a,b,c} -- separa por virgulas fora de chaves aninhadas *)
          let parts = ref [] in
          let cur   = Buffer.create 8 in
          let d     = ref 0 in
          String.iter (fun c ->
            match c with
            | '{' -> incr d; Buffer.add_char cur c
            | '}' -> decr d; Buffer.add_char cur c
            | ',' when !d = 0 ->
              parts := Buffer.contents cur :: !parts;
              Buffer.clear cur
            | _  -> Buffer.add_char cur c
          ) body;
          parts := Buffer.contents cur :: !parts;
          List.rev !parts
      in
      List.concat_map (fun p ->
        brace_expand (pre ^ p ^ suf)
      ) parts
    end
  end

(* ------------------------------------------------------------------ *)
(*  Glob expansion                                                     *)
(* ------------------------------------------------------------------ *)

let glob_expand word =
  let has_glob =
    let found = ref false in
    String.iter (fun c -> if c = '*' || c = '?' || c = '[' then found := true) word;
    !found
  in
  if not has_glob then [word]
  else begin
    let pat_buf = Buffer.create (String.length word * 2) in
    Buffer.add_char pat_buf '^';
    String.iter (fun c ->
      match c with
      | '*' -> Buffer.add_string pat_buf "[^/]*"
      | '?' -> Buffer.add_string pat_buf "[^/]"
      | '.' | '+' | '^' | '$' | '(' | ')' | '|' | '\\' ->
        Buffer.add_char pat_buf '\\'; Buffer.add_char pat_buf c
      | c   -> Buffer.add_char pat_buf c
    ) word;
    Buffer.add_char pat_buf '$';
    let re = Str.regexp (Buffer.contents pat_buf) in
    let dir, prefix =
      match String.rindex_opt word '/' with
      | None   -> (".", "")
      | Some k -> (String.sub word 0 k, String.sub word 0 (k + 1))
    in
    (try
       let entries = Sys.readdir dir in
       let matches =
         Array.to_list entries
         |> List.filter (fun e ->
             (e.[0] <> '.' ||
              (String.length word > 0 && word.[0] = '.')) &&
             Str.string_match re (prefix ^ e) 0)
         |> List.map   (fun e -> prefix ^ e)
         |> List.sort  String.compare
       in
       if matches = [] then [word] else matches
     with Sys_error _ -> [word])
  end

(* ------------------------------------------------------------------ *)
(*  Expansao completa de uma palavra                                   *)
(* ------------------------------------------------------------------ *)

let expand_word ?(quoted = false) word =
  let w = word |> expand_tilde |> expand_vars in
  if quoted then [w]
  else List.concat_map glob_expand (brace_expand w)

(* ------------------------------------------------------------------ *)
(*  Tokenizador principal                                              *)
(* ------------------------------------------------------------------ *)

let tokenize line =
  let len    = String.length line in
  let tokens = ref [] in
  let i      = ref 0 in
  let add t  = tokens := t :: !tokens in

  let read_word () =
    let buf    = Buffer.create 16 in
    let quoted = ref false in
    let go     = ref true in
    while !go && !i < len do
      match line.[!i] with
      | ' ' | '\t' | '\r' | '\n'
      | '|' | '>' | '<' | '&' | ';' | '(' | ')' | '{' | '}' ->
        go := false
      | '\'' ->
        quoted := true; incr i;
        while !i < len && line.[!i] <> '\'' do
          Buffer.add_char buf line.[!i]; incr i
        done;
        if !i < len then incr i
      | '"' ->
        quoted := true; incr i;
        while !i < len && line.[!i] <> '"' do
          match line.[!i] with
          | '\\' when !i + 1 < len ->
            incr i;
            (match line.[!i] with
             | '"' | '\\' | '$' | '`' | '\n' ->
               Buffer.add_char buf line.[!i]
             | c ->
               Buffer.add_char buf '\\';
               Buffer.add_char buf c);
            incr i
          | '$' ->
            let start = !i in
            incr i;
            if !i < len && line.[!i] = '(' then begin
              incr i;
              let depth = ref 1 in
              let cb    = Buffer.create 16 in
              while !i < len && !depth > 0 do
                if line.[!i] = '(' then (incr depth; Buffer.add_char cb line.[!i])
                else if line.[!i] = ')' then (decr depth;
                  if !depth > 0 then Buffer.add_char cb line.[!i])
                else Buffer.add_char cb line.[!i];
                incr i
              done;
              Buffer.add_string buf (command_substitution (Buffer.contents cb))
            end else begin
              while !i < len &&
                    (let c = line.[!i] in
                     (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                     (c >= '0' && c <= '9') || c = '_' || c = '{' || c = '}') do
                incr i
              done;
              let frag = String.sub line start (!i - start) in
              Buffer.add_string buf (expand_vars frag)
            end
          | '`' ->
            incr i;
            let cb = Buffer.create 16 in
            while !i < len && line.[!i] <> '`' do
              Buffer.add_char cb line.[!i]; incr i
            done;
            if !i < len then incr i;
            Buffer.add_string buf (command_substitution (Buffer.contents cb))
          | c ->
            Buffer.add_char buf c; incr i
        done;
        if !i < len then incr i
      | '`' ->
        incr i;
        let cb = Buffer.create 16 in
        while !i < len && line.[!i] <> '`' do
          Buffer.add_char cb line.[!i]; incr i
        done;
        if !i < len then incr i;
        Buffer.add_string buf (command_substitution (Buffer.contents cb))
      | '\\' when !i + 1 < len ->
        incr i; Buffer.add_char buf line.[!i]; incr i
      | c ->
        Buffer.add_char buf c; incr i
    done;
    (Buffer.contents buf, !quoted)
  in

  while !i < len do
    match line.[!i] with
    | ' ' | '\t' | '\r' -> incr i
    | '\n' -> incr i; add TNewline
    | '#'  -> i := len

    | '|' ->
      incr i;
      if !i < len && line.[!i] = '|' then (incr i; add TOr)
      else add TPipe

    | '&' ->
      incr i;
      if !i < len && line.[!i] = '&' then (incr i; add TAnd)
      else add TBg

    | ';' ->
      incr i;
      if !i < len && line.[!i] = ';' then (incr i; add TDSemi)
      else add TSemi

    | '(' -> incr i; add TLParen
    | ')' -> incr i; add TRParen
    | '{' -> incr i; add TLBrace
    | '}' -> incr i; add TRBrace

    | '>' ->
      incr i;
      if !i < len && line.[!i] = '>' then (incr i; add TRedirApp)
      else add TRedirOut

    | '<' ->
      incr i;
      if !i < len && line.[!i] = '<' then (incr i; add THeredoc)
      else add TRedirIn

    | '2' when !i + 1 < len && line.[!i + 1] = '>' ->
      i := !i + 2;
      if !i + 2 < len &&
         line.[!i] = '>' && line.[!i+1] = '&' && line.[!i+2] = '1' then
        (i := !i + 3; add TRedirBoth)
      else if !i < len && line.[!i] = '>' then
        (incr i; add TRedirErr2)
      else
        add TRedirErr

    | _ ->
      let (w, was_quoted) = read_word () in
      if w <> "" || was_quoted then begin
        if was_quoted then add (TWord w)
        else begin
          match keyword_of_string w with
          | Some kw -> add kw
          | None    ->
            (* aplica expansoes e emite um TWord por resultado *)
            List.iter (fun e -> add (TWord e)) (expand_word w)
        end
      end
  done;
  add TEOF;
  List.rev !tokens
