(* ================================================================== *)
(*  types.ml — definições centrais do myshell                        *)
(*                                                                    *)
(*  Este módulo define os tipos que percorrem toda a pipeline:        *)
(*    Lexer → Parser → Executor                                       *)
(*                                                                    *)
(*  Diagrama da pipeline de execução:                                 *)
(*    input string                                                    *)
(*      └─ Lexer.tokenize  → token list                              *)
(*           └─ Parser.parse → ast                                    *)
(*                └─ Executor.eval → exit_code                        *)
(* ================================================================== *)

(* ------------------------------------------------------------------ *)
(*  1. Redirects                                                        *)
(*                                                                    *)
(*  Um redirect descreve como ligar os file descriptors standard de   *)
(*  um processo a ficheiros ou outros descriptors.                    *)
(*                                                                    *)
(*  POSIX fd:  0 = stdin,  1 = stdout,  2 = stderr                   *)
(* ------------------------------------------------------------------ *)

type redirect =
  | Redirect_in    of string   (* <  ficheiro  — fd 0 lê do ficheiro  *)
  | Redirect_out   of string   (* >  ficheiro  — fd 1 escreve         *)
  | Redirect_app   of string   (* >> ficheiro  — fd 1 acrescenta      *)
  | Redirect_err   of string   (* 2> ficheiro  — fd 2 escreve         *)
  | Redirect_err2  of string   (* 2>> ficheiro — fd 2 acrescenta      *)
  | Redirect_both  of string   (* 2>&1 >f      — fd 1 e 2 para f      *)
  | Heredoc        of string   (* << DELIM     — texto inline como fd0 *)

(* ------------------------------------------------------------------ *)
(*  2. Comando simples                                                  *)
(*                                                                    *)
(*  A unidade atómica de execução: um nome + argumentos + redirects.  *)
(*  Exemplos:                                                          *)
(*    ls -la          → { argv=["ls";"-la"]; redirects=[] }           *)
(*    cat < f > g     → { argv=["cat"]; redirects=[In "f"; Out "g"]} *)
(* ------------------------------------------------------------------ *)

type simple_cmd = {
  argv      : string list;      (* argv.(0) = nome do programa        *)
  redirects : redirect list;    (* aplicados da esquerda para direita  *)
}

(* ------------------------------------------------------------------ *)
(*  3. Pipeline                                                         *)
(*                                                                    *)
(*  Uma lista de comandos ligados por "|".  O kernel cria um pipe     *)
(*  entre cada par adjacente: stdout[i] → stdin[i+1].                 *)
(*                                                                    *)
(*  O flag background determina se o shell espera pelo grupo de       *)
(*  processos (foreground) ou retorna imediatamente (background &).   *)
(* ------------------------------------------------------------------ *)

type pipeline = {
  cmds       : simple_cmd list;
  background : bool;
}

(* ------------------------------------------------------------------ *)
(*  4. AST — Árvore de Sintaxe Abstracta                              *)
(*                                                                    *)
(*  Representa a estrutura completa de um comando ou script.          *)
(*  É uma árvore binária onde as folhas são pipelines e os nós        *)
(*  internos são operadores de controlo de fluxo.                     *)
(*                                                                    *)
(*  Gramática aproximada (EBNF):                                       *)
(*    ast ::= pipeline                                                 *)
(*          | ast ";"  ast          (sequência sempre)                *)
(*          | ast "&&" ast          (sequência curto-circuito AND)    *)
(*          | ast "||" ast          (sequência curto-circuito OR)     *)
(*          | "if" ast "then" ast ["else" ast] "fi"                   *)
(*          | "while" ast "do" ast "done"                             *)
(*          | "for" name "in" words "do" ast "done"                   *)
(*          | "function" name "{" ast "}"                             *)
(*          | "{"  ast "}"          (subshell no processo actual)     *)
(*          | "(" ast ")"           (subshell em fork separado)       *)
(* ------------------------------------------------------------------ *)

type ast =
  (* --- Controlo de fluxo básico --- *)
  | Pipeline  of pipeline
  | Seq       of ast * ast          (* cmd1 ; cmd2                    *)
  | And       of ast * ast          (* cmd1 && cmd2                   *)
  | Or        of ast * ast          (* cmd1 || cmd2                   *)

  (* --- Estruturas de scripting --- *)
  | If        of ast * ast * ast option
    (*         condition  then-branch  else-branch-option             *)

  | While     of ast * ast
    (*          condition  body                                       *)

  | Until     of ast * ast
    (*          condition  body  (corre enquanto condição falhar)     *)

  | For       of string * string list * ast
    (*          var-name   word-list    body                          *)

  | Case      of string * (string list * ast) list
    (*           word      patterns * body                            *)

  (* --- Funções --- *)
  | FuncDef   of string * ast
    (*           nome      corpo                                      *)

  | FuncCall  of string * string list
    (*           nome      argumentos                                  *)

  (* --- Agrupamentos --- *)
  | Subshell  of ast    (* ( ast ) — executa em processo filho        *)
  | Group     of ast    (* { ast } — executa no processo actual       *)

  (* --- Nop --- *)
  | Nop                 (* linha vazia ou comentário                  *)

(* ------------------------------------------------------------------ *)
(*  5. Tipos auxiliares                                                 *)
(* ------------------------------------------------------------------ *)

(** Código de saída POSIX: 0 = sucesso, 1-125 = erro, 126 = não      *)
(*  executável, 127 = não encontrado, 128+n = terminado por sinal n   *)
type exit_code = int

(** Registo de um job em background                                    *)
type job = {
  job_id   : int;           (* número sequencial para o utilizador    *)
  job_pid  : int;           (* PID do processo líder do pipeline      *)
  job_pids : int list;      (* todos os PIDs do pipeline              *)
  job_desc : string;        (* representação textual do comando       *)
  mutable job_status : job_status;
}

and job_status =
  | Running               (* a correr em background                   *)
  | Stopped               (* suspenso com SIGTSTP (Ctrl+Z)            *)
  | Done of int           (* terminou com este código de saída        *)

(** Erros de shell — distintos de erros de processo filho              *)
exception Shell_error of string

(** Sinaliza que o script deve terminar (exit builtin)                 *)
exception Exit_shell  of int

(** Excepcoes de controlo de fluxo dentro de loops                    *)
exception Break_loop
exception Continue_loop
