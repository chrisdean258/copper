" Vim syntax file
" Language: Copper
" Maintainer: Chris Dean
" Latest Revision: 21021 Nov 13

if exists("b:current_syntax")
  finish
endif

syn keyword copperKeyword fn global
syn keyword copperConditional while if else and for
syn keyword copperBuiltin print prints
syn keyword copperBool true false
syn keyword copperNull null
syn match copperKeyword '\\'
syn match copperOperator '='
syn match copperOperator '=='
syn match copperOperator '|'
syn match copperOperator '||'
syn match copperOperator '|='
syn match copperOperator '&'
syn match copperOperator '&&'
syn match copperOperator '&='
syn match copperOperator '\^'
syn match copperOperator '\^\^'
syn match copperOperator '\^='
syn match copperOperator '!'
syn match copperOperator '!='
syn match copperOperator '>'
syn match copperOperator '>>'
syn match copperOperator '>='
syn match copperOperator '>>='
syn match copperOperator '<'
syn match copperOperator '<<'
syn match copperOperator '<='
syn match copperOperator '<<='
syn match copperOperator '+'
syn match copperOperator '++'
syn match copperOperator '+='
syn match copperOperator '-'
syn match copperOperator '--'
syn match copperOperator '-='
syn match copperOperator '%'
syn match copperOperator '%='
syn match copperOperator '\*'
syn match copperOperator '\*='
syn match copperOperator '\/'
syn match copperOperator '\/='
syn match copperOperator '\~'


syn match copperNumber '\<\d\+\(\.\d\+\)\?'
syn match copperString '"\([^"\\]\|\\.\)*"'
syn match copperChar '\'\([^"\\]\|\\.\)\''
syn match copperLambdaArg '\\\d\+'
syn match copperComment '#.*$'

syn match copperFunction '[_a-zA-Z][_a-zA-z0-9]*\s*\ze('

let b:current_syntax = "copper"
hi def link copperConditional Conditional
hi def link copperKeyword Keyword
hi def link copperBuiltin Function
hi def link copperNumber Number
hi def link copperString String
hi def link copperChar Character
hi def link copperBool Boolean
hi def link copperOperator Operator
hi def link copperNull Constant
hi def link copperLambdaArg PreProc
hi def link copperFunction Function
hi def link copperComment Comment

