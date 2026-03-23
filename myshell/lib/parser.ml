(* ================================================================== *)
(*  parser.ml -- constroi AST a partir de tokens                     *)
(*                                                                    *)
(*  Implementa um parser recursivo descendente para a gramatica:      *)
(*                                                                    *)
(*  program   ::= list EOF                                            *)
(*  list      ::= pipeline (("&&" | "||" | ";" | newline) pipeline)* *)
(*  pipeline  ::= command ("|" command)* ["&"]                        *)
(*  command   ::= simple_cmd                                          *)
(*              | if_clause                                           *)
(*              | while_clause                                        *)
(*              | until_clause                                        *)
(*              | for_clause                                           *)
(*              | case_clause                                         *)
(*              | func_def                                            *)
(*              | "(" list ")"   (subshell)                           *)
(*              | "{" list "}"   (group)                              *)
(*  simple_cmd::= word+ redirect*                                     *)
(*  redirect  ::= "<" word | ">" word | ">>" word | "2>" word        *)
(*              | "2>>" word | "2>&1" word | "<<" DELIM               *)
(* ================================================================== *)

open Types
open Lexer

(* ------------------------------------------------------------------ *)
(*  Estado mutavel do parser                                           *)
(* ------------------------------------------------------------------ *)

type state = {
  mutable tokens  : token list;
  mutable line_no : int;   (* para mensagens de erro uteis            *)
}

let make_state tokens = { tokens; line_no = 1 }

(* Observa o token seguinte sem consumir *)
let peek st = match st.tokens with t :: _ -> t | [] -> TEOF

(* Consome e devolve o token seguinte *)
let eat st  =
  match st.tokens with
  | TNewline :: rest -> st.line_no <- st.line_no + 1;
    st.tokens <- rest; TNewline
  | t        :: rest -> st.tokens <- rest; t
  | []               -> TEOF

(* Consome um token especifico; lanca erro se nao corresponder *)
let expect st expected =
  let t = eat st in
  if t <> expected then
    raise (Shell_error
      (Printf.sprintf "linha %d: esperava %s, encontrei %s"
         st.line_no
         (match expected with
          | TThen -> "'then'" | TDo -> "'do'" | TDone -> "'done'"
          | TFi   -> "'fi'"   | TIn -> "'in'" | TRBrace -> "'}'"
          | TRParen -> "')'"  | _   -> "token")
         (match t with
          | TWord w -> Printf.sprintf "'%s'" w
          | TEOF    -> "fim de ficheiro"
          | _       -> "token inesperado")))

(* Salta newlines (tratadas como separadores opcionais em muitos contextos) *)
let skip_newlines st =
  while peek st = TNewline do ignore (eat st) done

(* ------------------------------------------------------------------ *)
(*  Parsing de redirects                                               *)
(* ------------------------------------------------------------------ *)

(* Tabela de heredocs pendentes: (delimitador, buffer_de_destino)     *)
let pending_heredocs : (string * string ref) list ref = ref []

let parse_redirect st =
  match peek st with
  | TRedirIn ->
    ignore (eat st);
    (match peek st with
     | TWord f -> ignore (eat st); Some (Redirect_in f)
     | _ -> raise (Shell_error "esperava ficheiro apos '<'"))

  | TRedirOut ->
    ignore (eat st);
    (match peek st with
     | TWord f -> ignore (eat st); Some (Redirect_out f)
     | _ -> raise (Shell_error "esperava ficheiro apos '>'"))

  | TRedirApp ->
    ignore (eat st);
    (match peek st with
     | TWord f -> ignore (eat st); Some (Redirect_app f)
     | _ -> raise (Shell_error "esperava ficheiro apos '>>'"))

  | TRedirErr ->
    ignore (eat st);
    (match peek st with
     | TWord f -> ignore (eat st); Some (Redirect_err f)
     | _ -> raise (Shell_error "esperava ficheiro apos '2>'"))

  | TRedirErr2 ->
    ignore (eat st);
    (match peek st with
     | TWord f -> ignore (eat st); Some (Redirect_err2 f)
     | _ -> raise (Shell_error "esperava ficheiro apos '2>>'"))

  | TRedirBoth ->
    ignore (eat st);
    (match peek st with
     | TWord f -> ignore (eat st); Some (Redirect_both f)
     | _ -> raise (Shell_error "esperava ficheiro apos '2>&1'"))

  | THeredoc ->
    ignore (eat st);
    (* o delimitador e a proxima palavra *)
    (match peek st with
     | TWord delim ->
       ignore (eat st);
       (* o corpo do heredoc sera lido interactivamente pelo REPL       *)
       (* aqui guardamos o delimitador e um ref onde o corpo ira parar  *)
       let body_ref = ref "" in
       pending_heredocs := (delim, body_ref) :: !pending_heredocs;
       Some (Heredoc delim)  (* executor tratara de usar body_ref       *)
     | _ -> raise (Shell_error "esperava delimitador apos '<<'"))

  | _ -> None

(* ------------------------------------------------------------------ *)
(*  Parsing de um comando simples                                      *)
(* ------------------------------------------------------------------ *)

let parse_simple_cmd st =
  let words  = ref [] in
  let redirs = ref [] in
  let go     = ref true in
  while !go do
    match peek st with
    | TWord w ->
      ignore (eat st); words := w :: !words
    | TRedirIn | TRedirOut | TRedirApp | TRedirErr
    | TRedirErr2 | TRedirBoth | THeredoc ->
      (match parse_redirect st with
       | Some r -> redirs := r :: !redirs
       | None   -> ())
    | _ -> go := false
  done;
  if !words = [] && !redirs = [] then None
  else Some { argv = List.rev !words; redirects = List.rev !redirs }

(* ------------------------------------------------------------------ *)
(*  Parsing de if/elif/else/fi                                         *)
(*                                                                    *)
(*  Gramatica:                                                         *)
(*    if_clause ::= "if" list "then" list                             *)
(*                  ("elif" list "then" list)*                        *)
(*                  ["else" list]                                     *)
(*                  "fi"                                              *)
(* ------------------------------------------------------------------ *)

let rec parse_if st =
  (* consome "if" ja foi consumido pelo chamador *)
  skip_newlines st;
  let cond = match parse_list st with
    | Some a -> a
    | None   -> raise (Shell_error "esperava condicao apos 'if'")
  in
  skip_newlines st;
  expect st TThen;
  skip_newlines st;
  let then_b = match parse_list st with
    | Some a -> a
    | None   -> Nop
  in
  skip_newlines st;
  let else_b = match peek st with
    | TElif ->
      ignore (eat st);
      Some (parse_if st)  (* recursao para elif *)
    | TElse ->
      ignore (eat st);
      skip_newlines st;
      let b = match parse_list st with
        | Some a -> a
        | None   -> Nop
      in
      skip_newlines st;
      expect st TFi;
      Some b
    | TFi ->
      ignore (eat st);
      None
    | _ ->
      raise (Shell_error (Printf.sprintf "linha %d: esperava 'elif', 'else' ou 'fi'" st.line_no))
  in
  If (cond, then_b, else_b)

(* ------------------------------------------------------------------ *)
(*  Parsing de while/until/do/done                                     *)
(* ------------------------------------------------------------------ *)

and parse_while st =
  skip_newlines st;
  let cond = match parse_list st with
    | Some a -> a
    | None   -> raise (Shell_error "esperava condicao apos 'while'")
  in
  skip_newlines st;
  expect st TDo;
  skip_newlines st;
  let body = match parse_list st with
    | Some a -> a
    | None   -> Nop
  in
  skip_newlines st;
  expect st TDone;
  While (cond, body)

and parse_until st =
  skip_newlines st;
  let cond = match parse_list st with
    | Some a -> a
    | None   -> raise (Shell_error "esperava condicao apos 'until'")
  in
  skip_newlines st;
  expect st TDo;
  skip_newlines st;
  let body = match parse_list st with
    | Some a -> a
    | None   -> Nop
  in
  skip_newlines st;
  expect st TDone;
  Until (cond, body)

(* ------------------------------------------------------------------ *)
(*  Parsing de for VAR in WORDS do BODY done                          *)
(* ------------------------------------------------------------------ *)

and parse_for st =
  let var = match peek st with
    | TWord v -> ignore (eat st); v
    | _ -> raise (Shell_error "esperava nome de variavel apos 'for'")
  in
  skip_newlines st;
  let words =
    if peek st = TIn then begin
      ignore (eat st);
      let ws = ref [] in
      while (match peek st with
             | TWord _ -> true | _ -> false) do
        match eat st with
        | TWord w -> ws := w :: !ws
        | _       -> ()
      done;
      List.rev !ws
    end else
      (* for var; do ... done -- itera sobre $@ *)
      [ "$@" ]  (* sera expandido pelo executor *)
  in
  (* consome ; ou newline antes de "do" *)
  (match peek st with TSemi | TNewline -> ignore (eat st) | _ -> ());
  skip_newlines st;
  expect st TDo;
  skip_newlines st;
  let body = match parse_list st with
    | Some a -> a
    | None   -> Nop
  in
  skip_newlines st;
  expect st TDone;
  For (var, words, body)

(* ------------------------------------------------------------------ *)
(*  Parsing de case WORD in patterns esac                             *)
(*                                                                    *)
(*  Gramatica:                                                         *)
(*    case_clause ::= "case" word "in"                                *)
(*                    (pattern ("|" pattern)* ")" list ";;")*         *)
(*                    "esac"                                          *)
(* ------------------------------------------------------------------ *)

and parse_case st =
  let word = match peek st with
    | TWord w -> ignore (eat st); w
    | _ -> raise (Shell_error "esperava palavra apos 'case'")
  in
  skip_newlines st;
  expect st TIn;
  skip_newlines st;
  let clauses = ref [] in
  while peek st <> TEsac && peek st <> TEOF do
    (* recolhe padroes separados por | *)
    let pats = ref [] in
    (* pode comecar com ( opcional *)
    if peek st = TLParen then ignore (eat st);
    (match peek st with
     | TWord p -> ignore (eat st); pats := [p]
     | _ -> ());
    while peek st = TPipe do
      ignore (eat st);
      (match peek st with
       | TWord p -> ignore (eat st); pats := p :: !pats
       | _ -> ())
    done;
    expect st TRParen;
    skip_newlines st;
    let body = match parse_list st with
      | Some a -> a
      | None   -> Nop
    in
    skip_newlines st;
    if peek st = TDSemi then ignore (eat st);
    skip_newlines st;
    if !pats <> [] then
      clauses := (List.rev !pats, body) :: !clauses
  done;
  expect st TEsac;
  Case (word, List.rev !clauses)

(* ------------------------------------------------------------------ *)
(*  Parsing de definicao de funcao                                    *)
(*                                                                    *)
(*  Duas sintaxes suportadas:                                          *)
(*    function nome { corpo }                                          *)
(*    nome () { corpo }                                               *)
(* ------------------------------------------------------------------ *)

and parse_funcdef st name =
  (* consome () se presentes *)
  if peek st = TLParen then begin
    ignore (eat st);
    expect st TRParen
  end;
  skip_newlines st;
  (* o corpo e um grupo ou comando *)
  let body = match peek st with
    | TLBrace ->
      ignore (eat st);
      skip_newlines st;
      let b = match parse_list st with
        | Some a -> a
        | None   -> Nop
      in
      skip_newlines st;
      expect st TRBrace;
      b
    | _ ->
      (match parse_command st with
       | Some c -> c
       | None   -> raise (Shell_error "esperava corpo de funcao"))
  in
  FuncDef (name, body)

(* ------------------------------------------------------------------ *)
(*  Parsing de um comando (unidade de pipeline)                       *)
(* ------------------------------------------------------------------ *)

and parse_command st =
  skip_newlines st;
  match peek st with
  | TIf ->
    ignore (eat st);
    Some (parse_if st)

  | TWhile ->
    ignore (eat st);
    Some (parse_while st)

  | TUntil ->
    ignore (eat st);
    Some (parse_until st)

  | TFor ->
    ignore (eat st);
    Some (parse_for st)

  | TCase ->
    ignore (eat st);
    Some (parse_case st)

  | TFunction ->
    ignore (eat st);
    (match peek st with
     | TWord name ->
       ignore (eat st);
       Some (parse_funcdef st name)
     | _ -> raise (Shell_error "esperava nome de funcao apos 'function'"))

  | TLParen ->
    (* subshell: ( list ) *)
    ignore (eat st);
    skip_newlines st;
    let body = match parse_list st with
      | Some a -> a
      | None   -> Nop
    in
    skip_newlines st;
    expect st TRParen;
    Some (Subshell body)

  | TLBrace ->
    (* group: { list } *)
    ignore (eat st);
    skip_newlines st;
    let body = match parse_list st with
      | Some a -> a
      | None   -> Nop
    in
    skip_newlines st;
    expect st TRBrace;
    Some (Group body)

  | TWord name when
      (* detecta "nome ()" -- definicao de funcao sem keyword *)
      (match st.tokens with
       | _ :: TLParen :: _ -> true
       | _ -> false) ->
    ignore (eat st);
    Some (parse_funcdef st name)

  | TWord _ ->
    (match parse_simple_cmd st with
     | Some cmd -> Some (Pipeline { cmds = [cmd]; background = false })
     | None     -> None)

  | _ -> None

(* ------------------------------------------------------------------ *)
(*  Parsing de um pipeline                                             *)
(* ------------------------------------------------------------------ *)

and parse_pipeline st =
  match parse_command st with
  | None -> None
  | Some first ->
    (* extrai o simple_cmd se for um pipeline trivial *)
    let cmds = ref [first] in
    let bg   = ref false in
    let go   = ref true in
    while !go do
      match peek st with
      | TPipe ->
        ignore (eat st);
        skip_newlines st;
        (match parse_command st with
         | Some c -> cmds := c :: !cmds
         | None   -> raise (Shell_error "esperava comando apos '|'"))
      | TBg ->
        ignore (eat st);
        bg  := true;
        go  := false
      | _ -> go := false
    done;
    (* se tiver varios comandos no pipeline, tem de ser simple_cmds    *)
    (* (pipes entre estruturas compostas nao e suportado nesta versao) *)
    if List.length !cmds = 1 && not !bg then
      (* pipeline simples sem background: devolve o no directamente    *)
      Some (List.hd (List.rev !cmds))
    else begin
      (* multi-comando ou background                                   *)
      (* Caso especial: "estrutura_composta &" -> Subshell             *)
      let nodes = List.rev !cmds in
      (match (nodes, !bg) with
       | ([single], true) when (match single with Pipeline _ -> false | _ -> true) ->
         (* ex: "while ...; done &" -- faz fork sem esperar            *)
         Some (Subshell single)
       | _ ->
         let extract_simple = function
           | Pipeline { cmds = [c]; _ } -> c
           | Pipeline { cmds = c :: _; _ } -> c
           | _ -> raise (Shell_error "estruturas compostas nao podem ser ligadas por pipes")
         in
         let simple_cmds = List.map extract_simple nodes in
         Some (Pipeline { cmds = simple_cmds; background = !bg }))
    end

(* ------------------------------------------------------------------ *)
(*  Parsing de uma lista (nivel mais alto)                             *)
(*                                                                    *)
(*  list ::= pipeline (sep pipeline)*                                 *)
(*  sep  ::= ";" | "&&" | "||" | newline                             *)
(* ------------------------------------------------------------------ *)

and parse_list st =
  match parse_pipeline st with
  | None -> None
  | Some left ->
    (* verifica se ha um operador seguinte *)
    (match peek st with
     | TSemi | TNewline ->
       ignore (eat st);
       skip_newlines st;
       (* pode haver mais ou nao *)
       (match parse_list st with
        | None       -> Some left
        | Some right -> Some (Seq (left, right)))

     | TAnd ->
       ignore (eat st);
       skip_newlines st;
       (match parse_list st with
        | None       -> raise (Shell_error "esperava comando apos '&&'")
        | Some right -> Some (And (left, right)))

     | TOr ->
       ignore (eat st);
       skip_newlines st;
       (match parse_list st with
        | None       -> raise (Shell_error "esperava comando apos '||'")
        | Some right -> Some (Or (left, right)))

     | _ -> Some left)

(* ------------------------------------------------------------------ *)
(*  Entrada publica                                                    *)
(* ------------------------------------------------------------------ *)

(** Parse de uma linha/string completa. Devolve None se a linha for  *)
(*  vazia ou so tiver comentarios/whitespace.                          *)
let parse line =
  let tokens = Lexer.tokenize line in
  let st = make_state tokens in
  skip_newlines st;
  match parse_list st with
  | None -> None
  | Some ast -> Some ast

(** Verifica se uma linha esta completa ou precisa de continuacao     *)
(*  (ex: "if true" sem "then" ainda nao e completo).                 *)
let is_complete line =
  try
    ignore (parse line); true
  with Shell_error _ -> false

(* Tabela de corpos de here-documents pendentes.                     *)
(* Chave: delimitador. Valor: corpo ja lido do stdin.                 *)
(* Preenchida pelo REPL antes de parsear a linha.                     *)
let heredoc_bodies : (string, string) Hashtbl.t = Hashtbl.create 4

let add_heredoc_body delim body =
  Hashtbl.replace heredoc_bodies delim body

let get_heredoc_body delim =
  match Hashtbl.find_opt heredoc_bodies delim with
  | Some body -> body
  | None      -> ""  (* fallback: sem corpo                           *)

(* Tabela de funcoes definidas pelo utilizador                        *)
let functions : (string, ast) Hashtbl.t = Hashtbl.create 16

let define_function name body =
  Hashtbl.replace functions name body

let lookup_function name =
  Hashtbl.find_opt functions name
