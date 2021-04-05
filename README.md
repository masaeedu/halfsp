# halfsp

halfsp is a Haskell language server that doesn't do much, and does it faster than HLS.

It ignores your source code, and expects you to manually:

- Produce analysis information in .hie files during compilation (using `-fwrite-ide-info`)
- Index the .hie files into a database using `hiedb`
- Do this every time your source code changes

If you do all that, it'll provide the following incredible list of...

## features

At the moment, halfsp responds to the following LSP messages:

- `workspace/symbol`

halfsp will soon support the following...

## todo features

- `textDocument/{definition, declaration, implementation, references}`
- `textDocument/hover`

halfsp will never support any other LSP messages.

## goals

As the name suggests, this project is a temporary hack. Specifically, it is intended to work around limitations in GHC recompilation-avoidance in the presence of Template Haskell, which can cause severe performance issues on large codebases with heavy use of TH.
