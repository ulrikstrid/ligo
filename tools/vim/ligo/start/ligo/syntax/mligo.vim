if exists("b:current_syntax")
    finish
endif

" Keywords
syntax keyword ligoKeyword begin end
syntax keyword ligoKeyword is
syntax keyword ligoKeyword fun let rec type
syntax keyword ligoConditional if then else match with
syntax match ligoOperator "\v[-+*/=]"
syntax match ligoParens "("
syntax match ligoParens ")"

highlight link ligoKeyword Keyword
highlight link ligoConditional Conditional
highlight link ligoRepeat Repeat
highlight link ligoOperator Operator

" Constants
syntax keyword ligoBoolean True False
syntax match ligoNumber "\v<\d+[a-z]*>"
syntax match ligoNumber "\v<0x[a-fA-F0-9]+>"
syntax region ligoString start=/\v"/ skip=/\v\\./ end=/\v"/

highlight link ligoBoolean Boolean
highlight link ligoNumber Number
highlight link ligoString String

" Comments
syntax region ligoComment start=/\v\(\*/ end=/\v\*\)/ contains=ligoComment
highlight link ligoComment Comment

" Types
syntax region ligoParenTypeExpr start=/\v\(/ end=/\v\)/
            \ contains=ligoParenTypeExpr,ligoComment
            \ contained
syntax region ligoTypeAnnotation 
            \ matchgroup=ligoKeyword start=/:/
            \ matchgroup=ligoKeyword end=/=/
            \ matchgroup=ligoKeyword end=/:=/
            \ matchgroup=ligoKeyword end=/;/
            \ matchgroup=ligoParens end=/)/
            \ contains=ligoParenTypeExpr
highlight link ligoTypeAnnotation Type
highlight link ligoParenTypeExpr Type

let b:current_syntax = "mligo"

