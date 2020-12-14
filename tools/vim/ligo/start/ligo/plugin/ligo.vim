if executable('ligo-squirrel')
  augroup ligoRegisterLanguageServer
    autocmd User lsp_setup
        \ call lsp#register_server({
        \   'name': 'ligo_lsp',
        \   'cmd': {server_info->['ligo-squirrel']},
        \   'allowlist': ['ligo', 'mligo', 'religo'],
        \ })
  augroup END
endif
