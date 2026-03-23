(* ================================================================== *)
(*  readline.ml -- leitura de linha com edicao e historico            *)
(*                                                                    *)
(*  Implementa um mini-readline em modo raw de terminal.              *)
(*                                                                    *)
(*  Funcionalidades:                                                   *)
(*    - Edicao de linha: backspace, delete, Ctrl+A (inicio),          *)
(*      Ctrl+E (fim), Ctrl+K (apaga ate ao fim), setas esq/dir       *)
(*    - Historico: setas cima/baixo navegam no historico              *)
(*    - Ctrl+R: pesquisa incremental no historico                     *)
(*    - Tab: completion (via Completion.complete)                     *)
(*    - Ctrl+C: cancela a linha actual                                *)
(*    - Ctrl+D: EOF (sair do shell) ou apaga caractere sob o cursor  *)
(*    - Multilinhas: se o parse estiver incompleto, mostra PS2        *)
(*                                                                    *)
(*  Modo raw:                                                          *)
(*    O terminal normalmente esta em modo "cooked" onde o kernel      *)
(*    gere o echo e a edicao. Em modo raw recebemos cada tecla        *)
(*    imediatamente e gerimos tudo nos proprios.                      *)
(* ================================================================== *)

(* ------------------------------------------------------------------ *)
(*  Configuracao do terminal (raw mode)                                *)
(* ------------------------------------------------------------------ *)

let saved_termios : Unix.terminal_io option ref = ref None

let enter_raw_mode () =
  if Unix.isatty Unix.stdin then begin
    let t = Unix.tcgetattr Unix.stdin in
    saved_termios := Some t;
    let raw = { t with
      (* desactiva echo, modo canonico (leitura linha a linha),       *)
      (* e alguns sinais de controlo                                  *)
      Unix.c_icanon  = false;
      Unix.c_echo    = false;
      Unix.c_echoe   = false;
      Unix.c_isig    = false;   (* desactiva Ctrl+C/Z via kernel       *)
      Unix.c_ixon    = false;   (* desactiva Ctrl+S/Q                 *)
      Unix.c_vmin    = 1;       (* leitura bloqueante ate 1 char      *)
      Unix.c_vtime   = 0;
    } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW raw
  end

let exit_raw_mode () =
  match !saved_termios with
  | None   -> ()
  | Some t -> Unix.tcsetattr Unix.stdin Unix.TCSANOW t

(* ------------------------------------------------------------------ *)
(*  Leitura de um caractere/sequencia de escape                        *)
(* ------------------------------------------------------------------ *)

type key =
  | Char of char
  | Up | Down | Left | Right
  | Home | End_key
  | Delete
  | Backspace
  | Tab
  | Enter
  | Ctrl of char
  | Esc
  | Unknown of string

let read_key () =
  let buf = Bytes.create 8 in
  let n = Unix.read Unix.stdin buf 0 1 in
  if n = 0 then Ctrl 'd'
  else match Bytes.get buf 0 with
  | '\x1b' ->
    (* sequencia de escape: tenta ler mais *)
    let buf2 = Bytes.create 8 in
    Unix.set_nonblock Unix.stdin;
    let n2 =
      (try Unix.read Unix.stdin buf2 0 8
       with Unix.Unix_error (Unix.EAGAIN, _, _) -> 0)
    in
    Unix.clear_nonblock Unix.stdin;
    if n2 = 0 then Esc
    else begin
      let seq = Bytes.sub_string buf2 0 n2 in
      match seq with
      | "[A" -> Up
      | "[B" -> Down
      | "[C" -> Right
      | "[D" -> Left
      | "[H" | "OH"  -> Home
      | "[F" | "OF"  -> End_key
      | "[3~"        -> Delete
      | _            -> Unknown ("\x1b" ^ seq)
    end
  | '\x7f' | '\x08' -> Backspace
  | '\t'            -> Tab
  | '\r' | '\n'     -> Enter
  | c when Char.code c < 32 ->
    Ctrl (Char.chr (Char.code c + 64))  (* Ctrl+A = \x01, A = 65 = 1+64 *)
  | c -> Char c

(* ------------------------------------------------------------------ *)
(*  Estrutura de estado da linha                                        *)
(* ------------------------------------------------------------------ *)

type line_state = {
  mutable buf       : Buffer.t;  (* conteudo actual da linha          *)
  mutable cursor    : int;       (* posicao do cursor (0..len)        *)
  mutable hist_idx  : int;       (* indice no historico (-1 = actual) *)
  mutable hist_save : string;    (* salva a linha ao entrar no hist.  *)
  mutable searching : bool;      (* estamos em Ctrl+R?                *)
  mutable search_q  : string;    (* query de pesquisa                 *)
}

let make_state () = {
  buf       = Buffer.create 64;
  cursor    = 0;
  hist_idx  = -1;
  hist_save = "";
  searching = false;
  search_q  = "";
}

(* ------------------------------------------------------------------ *)
(*  Operacoes sobre a linha                                             *)
(* ------------------------------------------------------------------ *)

let line_str st = Buffer.contents st.buf
let line_len st = Buffer.length st.buf

let set_line st s =
  Buffer.clear st.buf;
  Buffer.add_string st.buf s;
  st.cursor <- String.length s

let insert_char st c =
  let s   = line_str st in
  let pre = String.sub s 0 st.cursor in
  let suf = String.sub s st.cursor (String.length s - st.cursor) in
  Buffer.clear st.buf;
  Buffer.add_string st.buf (pre ^ String.make 1 c ^ suf);
  st.cursor <- st.cursor + 1

let delete_before st =   (* backspace *)
  if st.cursor > 0 then begin
    let s   = line_str st in
    let pre = String.sub s 0 (st.cursor - 1) in
    let suf = String.sub s st.cursor (String.length s - st.cursor) in
    Buffer.clear st.buf;
    Buffer.add_string st.buf (pre ^ suf);
    st.cursor <- st.cursor - 1
  end

let delete_at st =   (* delete key *)
  let s   = line_str st in
  let len = String.length s in
  if st.cursor < len then begin
    let pre = String.sub s 0 st.cursor in
    let suf = String.sub s (st.cursor + 1) (len - st.cursor - 1) in
    Buffer.clear st.buf;
    Buffer.add_string st.buf (pre ^ suf)
  end

let kill_to_end st =
  let s = line_str st in
  let pre = String.sub s 0 st.cursor in
  Buffer.clear st.buf;
  Buffer.add_string st.buf pre

(* ------------------------------------------------------------------ *)
(*  Rendering                                                           *)
(*                                                                    *)
(*  Apaga a linha actual e redesenha com o conteudo novo.             *)
(*  Usa sequencias ANSI: \r (inicio da linha), \033[K (apaga ate fim) *)
(* ------------------------------------------------------------------ *)

let redraw prompt st =
  (* \r vai ao inicio; \033[K apaga ate ao fim *)
  Printf.printf "\r\033[K%s%s" prompt (line_str st);
  (* posiciona o cursor *)
  let line = line_str st in
  let suffix_len = String.length line - st.cursor in
  if suffix_len > 0 then
    Printf.printf "\033[%dD" suffix_len;
  flush stdout

(* ------------------------------------------------------------------ *)
(*  Pesquisa incremental no historico (Ctrl+R)                        *)
(* ------------------------------------------------------------------ *)

let search_history query =
  let history = History.to_list () in
  List.find_opt (fun line ->
    (* procura query em qualquer posicao da linha *)
    let llen = String.length line in
    let qlen = String.length query in
    if qlen = 0 then true
    else if qlen > llen then false
    else begin
      let found = ref false in
      for i = 0 to llen - qlen do
        if String.sub line i qlen = query then found := true
      done;
      !found
    end
  ) history

(* ------------------------------------------------------------------ *)
(*  Loop principal de leitura                                          *)
(* ------------------------------------------------------------------ *)

let read_line_raw ?(prompt = "$ ") ?(ps2 = "> ") () =
  enter_raw_mode ();
  print_string prompt;
  flush stdout;

  let st = make_state () in
  let history_list = History.to_list () in
  let result = ref None in

  (try
     while !result = None do
       let key = read_key () in
       (match key with
        (* --- Enter: submete a linha --- *)
        | Enter ->
          print_char '\n'; flush stdout;
          (* verifica se o parse esta completo *)
          let line = line_str st in
          (* para simplificar, aceita sempre; multilinhas via script  *)
          result := Some line

        (* --- Ctrl+C: cancela --- *)
        | Ctrl 'C' ->
          print_string "^C\n"; flush stdout;
          set_line st "";
          result := Some ""

        (* --- Ctrl+D: EOF ou apaga caracter --- *)
        | Ctrl 'D' ->
          if line_len st = 0 then
            raise Exit  (* EOF *)
          else
            delete_at st;
          redraw prompt st

        (* --- Ctrl+A: inicio da linha --- *)
        | Ctrl 'A' | Home ->
          st.cursor <- 0;
          redraw prompt st

        (* --- Ctrl+E: fim da linha --- *)
        | Ctrl 'E' | End_key ->
          st.cursor <- line_len st;
          redraw prompt st

        (* --- Ctrl+K: apaga ate ao fim --- *)
        | Ctrl 'K' ->
          kill_to_end st;
          redraw prompt st

        (* --- Ctrl+U: apaga a linha toda --- *)
        | Ctrl 'U' ->
          set_line st "";
          redraw prompt st

        (* --- Ctrl+W: apaga a palavra anterior --- *)
        | Ctrl 'W' ->
          let s = line_str st in
          let i = ref (st.cursor - 1) in
          while !i > 0 && s.[!i] = ' ' do decr i done;
          while !i > 0 && s.[!i] <> ' ' do decr i done;
          let new_line = String.sub s 0 !i ^
            String.sub s st.cursor (String.length s - st.cursor) in
          set_line st new_line;
          st.cursor <- !i;
          redraw prompt st

        (* --- Ctrl+L: limpa o ecra --- *)
        | Ctrl 'L' ->
          print_string "\033[2J\033[H";
          redraw prompt st

        (* --- Seta esquerda --- *)
        | Left ->
          if st.cursor > 0 then st.cursor <- st.cursor - 1;
          redraw prompt st

        (* --- Seta direita --- *)
        | Right ->
          if st.cursor < line_len st then
            st.cursor <- st.cursor + 1;
          redraw prompt st

        (* --- Seta cima: historico anterior --- *)
        | Up ->
          let hist_len = List.length history_list in
          if st.hist_idx = -1 then
            st.hist_save <- line_str st;
          if st.hist_idx < hist_len - 1 then begin
            st.hist_idx <- st.hist_idx + 1;
            set_line st (List.nth history_list st.hist_idx)
          end;
          redraw prompt st

        (* --- Seta baixo: historico mais recente --- *)
        | Down ->
          if st.hist_idx > 0 then begin
            st.hist_idx <- st.hist_idx - 1;
            set_line st (List.nth history_list st.hist_idx)
          end else if st.hist_idx = 0 then begin
            st.hist_idx <- -1;
            set_line st st.hist_save
          end;
          redraw prompt st

        (* --- Tab: completion --- *)
        | Tab ->
          let (new_line, candidates) =
            Completion.complete (line_str st) in
          (match candidates with
           | [] ->
             if new_line <> line_str st then begin
               set_line st new_line;
               redraw prompt st
             end
           | many ->
             Completion.display_completions many;
             print_string prompt;
             print_string (line_str st);
             flush stdout)

        (* --- Ctrl+R: pesquisa no historico --- *)
        | Ctrl 'R' ->
          st.searching <- true;
          st.search_q  <- "";
          Printf.printf "\r\033[K(reverse-i-search)`': ";
          flush stdout

        (* --- Backspace --- *)
        | Backspace ->
          if st.searching then begin
            let q = st.search_q in
            if String.length q > 0 then
              st.search_q <- String.sub q 0 (String.length q - 1)
          end else
            delete_before st;
          redraw prompt st

        (* --- Delete --- *)
        | Delete ->
          delete_at st;
          redraw prompt st

        (* --- Caractere normal --- *)
        | Char c ->
          if st.searching then begin
            st.search_q <- st.search_q ^ String.make 1 c;
            (match search_history st.search_q with
             | None      -> ()
             | Some line -> set_line st line);
            Printf.printf "\r\033[K(reverse-i-search)`%s': %s"
              st.search_q (line_str st);
            flush stdout
          end else begin
            insert_char st c;
            redraw prompt st
          end

        | Esc ->
          st.searching <- false;
          redraw prompt st

        | Ctrl _ | Unknown _ -> ()
       )
     done
   with Exit ->
     exit_raw_mode ();
     raise Exit);

  exit_raw_mode ();
  (* Se a linha comecou uma estrutura composta incompleta, pede mais *)
  match !result with
  | None      -> None
  | Some line ->
    (* verifica se precisa de continuacao *)
    let accumulated = ref line in
    (try
       while not (
         let t = Lexer.tokenize !accumulated in
         (* heuristica: se terminar com then/do/{ sem fechar, continua *)
         let last_meaningful = List.filter (fun tok ->
           tok <> Lexer.TNewline) t in
         let last = match List.rev last_meaningful with
           | t :: _ -> t | [] -> Lexer.TEOF in
         last <> Lexer.TThen && last <> Lexer.TDo &&
         last <> Lexer.TLBrace && last <> Lexer.TPipe &&
         last <> Lexer.TAnd && last <> Lexer.TOr
       ) do
         print_string ps2; flush stdout;
         enter_raw_mode ();
         let st2 = make_state () in
         (try
            while true do
              let key = read_key () in
              (match key with
               | Enter ->
                 print_char '\n'; flush stdout;
                 accumulated := !accumulated ^ "\n" ^ line_str st2;
                 raise Exit
               | Ctrl 'C' ->
                 print_string "^C\n"; flush stdout;
                 accumulated := "";
                 raise Exit
               | Backspace -> delete_before st2; redraw ps2 st2
               | Char c    -> insert_char st2 c; redraw ps2 st2
               | Left      -> if st2.cursor > 0 then st2.cursor <- st2.cursor - 1; redraw ps2 st2
               | Right     -> if st2.cursor < line_len st2 then st2.cursor <- st2.cursor + 1; redraw ps2 st2
               | _         -> ())
            done
          with Exit -> ());
         exit_raw_mode ()
       done
     with _ -> ());
    Some !accumulated
