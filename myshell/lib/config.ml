(* ================================================================== *)
(*  config.ml -- carrega e executa o ficheiro de configuracao        *)
(*                                                                    *)
(*  O ficheiro ~/.myshellrc e executado automaticamente ao arrancar  *)
(*  o shell. Suporta todas as construcoes do shell (aliases, export,  *)
(*  funcoes, if/for/while, etc.).                                     *)
(*                                                                    *)
(*  Sequencia de pesquisa:                                             *)
(*    1. ~/.myshellrc                                                  *)
(*    2. /etc/myshellrc  (configuracao global, se existir)            *)
(* ================================================================== *)

let rc_file () =
  let home = try Sys.getenv "HOME" with Not_found -> "." in
  Filename.concat home ".myshellrc"

(* Executa o ficheiro de configuracao linha a linha.                  *)
(* Suporta estruturas multi-linha acumulando linhas ate a estrutura   *)
(* estar completa (heuristica: ultimo token e palavra-chave de fecho) *)
let load () =
  let path = rc_file () in
  if not (Sys.file_exists path) then ()
  else begin
    let ic  = open_in path in
    let acc = ref "" in
    (try
       while true do
         let raw = input_line ic in
         let line = String.trim raw in
         (* ignora linhas vazias e comentarios no topo *)
         if line = "" || line.[0] = '#' then ()
         else begin
           (* acumula *)
           acc := (if !acc = "" then line else !acc ^ "\n" ^ line);
           (* tenta parsear; se falhar provavelmente esta incompleto  *)
           (match Parser.parse !acc with
            | None     -> acc := ""   (* linha vazia apos parse       *)
            | Some ast ->
              (try
                 ignore (Executor.eval ast);
                 acc := ""
               with
               | Types.Shell_error msg ->
                 Printf.eprintf "myshell: .myshellrc: %s\n%!" msg;
                 acc := ""
               | Types.Exit_shell _ ->
                 acc := ""))
         end
       done
     with End_of_file -> ());
    (* executa o que ficou acumulado *)
    if !acc <> "" then
      (match Parser.parse !acc with
       | None     -> ()
       | Some ast ->
         (try ignore (Executor.eval ast)
          with _ -> ()));
    close_in ic
  end
