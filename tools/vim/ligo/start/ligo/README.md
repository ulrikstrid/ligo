# LIGO language support for Vim

This plugin adds support for PascaLIGO, CameLIGO, and ReasonLIGO to Vim.

Syntax highlighting is supported out-of-the box. Language features are provided via LSP. The language server (`ligo-squirrel`) should be installed separately.

## Language server configuration

You can use the following language client plugins:
* `vim-lsp`: preconfigured
* `coc.nvim`:
```
"languageserver": {
  "Ligo": {
    "command": "path/to/ligo-squirrel",
    "filetypes": ["ligo", "mligo", "religo"]
  }
}
```

* `LanguageClient-neovim`:
```
let g:LanguageClient_serverCommands = {
    \ 'ligo': ['path/to/ligo-squirrel'],
    \ 'mligo': ['path/to/ligo-squirrel'],
    \ 'religo': ['path/to/ligo-squirrel'],
    \ }
```
