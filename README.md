# myshell — Shell em OCaml

Uma shell POSIX-like escrita em OCaml de raiz.

## Funcionalidades implementadas

### Núcleo
- **REPL** — loop interactivo read-eval-print com prompt colorido
- **Lexer** — tokenização com suporte a aspas simples/duplas e `\` escape
- **Parser** — constrói AST com operadores `;`, `&&`, `||`
- **Executor** — `fork`/`exec`/`wait` via módulo `Unix`
- **Resolução de $PATH** — encontra executáveis sem caminho absoluto

### I/O
- **Pipes** — `cmd1 | cmd2 | cmd3`
- **Redirect saída** — `> ficheiro`, `>> ficheiro`
- **Redirect entrada** — `< ficheiro`
- **Redirect erros** — `2> ficheiro`, `2>> ficheiro`, `2>&1`

### Builtins
| Comando | Descrição |
|---------|-----------|
| `cd [dir]` | Muda de directório (`cd -` para voltar) |
| `pwd` | Mostra directório actual |
| `echo [-n] [-e]` | Imprime texto |
| `export VAR=val` | Exporta variável para o ambiente |
| `unset VAR` | Remove variável |
| `alias nome=cmd` | Define alias |
| `unalias [-a] nome` | Remove alias |
| `jobs` | Lista processos em background |
| `type cmd` | Mostra onde está um comando |
| `exit [n]` | Sai com código n |

### Expansões
- **Variáveis** — `$VAR`, `${VAR}`, `$?`
- **Tilde** — `~`, `~/dir`
- **Glob** — `*.ml`, `?.txt`, `[a-z]*`

### Gestão de processos
- **Background** — `comando &`
- **Job control** — `jobs` lista processos em background
- **Reaping** — processos terminados são recolhidos automaticamente

### Qualidade de vida
- **Histórico** — persistido em `~/.myshell_history`
- **Prompt dinâmico** — mostra utilizador, host, directório, branch git, código de saída
- **Aliases** — expandidos recursivamente (máx. 8 níveis)
- **Modo script** — `myshell script.sh`
- **Modo -c** — `myshell -c "comando"`
- **Dotfile** — carrega `~/.myshellrc` ao iniciar

---

## Instalação

### Requisitos
- OCaml ≥ 4.14
- opam
- dune ≥ 3.0

```bash
# Instalar dependências
opam install dune

# Compilar
cd myshell
dune build

# Instalar (opcional)
dune install

# Ou executar directamente
./_build/default/bin/main.exe
```

### Configuração inicial

```bash
# Copiar o exemplo de configuração
cp example.myshellrc ~/.myshellrc

# Editar conforme necessário
nano ~/.myshellrc
```

---

## Estrutura do projecto

```
myshell/
├── dune-project
├── lib/
│   ├── dune
│   ├── types.ml       ← tipos centrais (AST, redirects)
│   ├── lexer.ml       ← tokenizador + expansões
│   ├── parser.ml      ← parser → AST
│   ├── builtins.ml    ← comandos internos + alias + $PATH
│   ├── executor.ml    ← fork/exec, pipes, redirects
│   ├── history.ml     ← histórico persistente
│   ├── prompt.ml      ← prompt colorido dinâmico
│   └── config.ml      ← carregador de .myshellrc
└── bin/
    ├── dune
    └── main.ml        ← REPL, modo script, sinais
```

---

## Exemplos de uso

```bash
# Pipeline
ls -la | grep ".ml" | wc -l

# Redirects
echo "olá mundo" > saudacao.txt
cat < saudacao.txt >> log.txt

# Operadores lógicos
mkdir nova_pasta && cd nova_pasta
cat ficheiro.txt || echo "ficheiro não existe"

# Background
sleep 10 &
jobs

# Expansões
echo $HOME
echo ~/documentos
ls *.ml

# Aliases (após definir no .myshellrc)
ll
gs

# Modo script
myshell meu_script.sh

# Inline
myshell -c "echo olá && ls"
```

---

## Próximos passos sugeridos

1. **Command substitution** — `$(comando)` e `` `comando` ``
2. **Here-documents** — `<< EOF`
3. **Expansão de chaves** — `mkdir pasta_{1..3}`
4. **Tab completion** — usando `linenoise` ou `lambda-term`
5. **Suspensão** — Ctrl+Z / `fg` / `bg`
6. **Scripting** — `if/else`, `for`, `while`, funções
7. **Syntax highlighting** — colorir o input em tempo real
