(* ================================================================== *)
(*  history.ml -- historico de comandos com persistencia              *)
(*                                                                    *)
(*  Mantem uma fila circular de max_size entradas.                    *)
(*  Persiste em ~/.myshell_history entre sessoes.                     *)
(* ================================================================== *)

let max_size  = 2000
let history : string Queue.t = Queue.create ()

let history_file () =
  let home = try Sys.getenv "HOME" with Not_found -> "." in
  Filename.concat home ".myshell_history"

(* Adiciona uma linha ao historico, evitando duplicados consecutivos *)
let add line =
  let line = String.trim line in
  if line = "" then ()
  else begin
    (* verifica o ultimo elemento *)
    let last =
      if Queue.is_empty history then ""
      else begin
        let v = ref "" in
        Queue.iter (fun s -> v := s) history;
        !v
      end
    in
    if last <> line then begin
      Queue.push line history;
      if Queue.length history > max_size then
        ignore (Queue.pop history)
    end
  end

(* Carrega o historico do ficheiro *)
let load () =
  let path = history_file () in
  if Sys.file_exists path then begin
    let ic = open_in path in
    (try
       while true do
         let line = input_line ic in
         if String.trim line <> "" then
           Queue.push line history
       done
     with End_of_file -> ());
    close_in ic
  end

(* Guarda o historico no ficheiro *)
let save () =
  let path = history_file () in
  (try
     let oc = open_out path in
     Queue.iter (fun line -> output_string oc (line ^ "\n")) history;
     close_out oc
   with Sys_error _ -> ())

(* Devolve o historico como lista, mais recentes primeiro *)
let to_list () =
  let lst = ref [] in
  Queue.iter (fun s -> lst := s :: !lst) history;
  !lst  (* a Queue e FIFO, iter da mais antigos primeiro, rev da recentes *)
