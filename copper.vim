" Vim syntax file
" Language: Copper
" Maintainer: Chris Dean
" Latest Revision: 21021 Nov 13

if exists("b:current_syntax")
  finish
endif

syn keyword copperKeyword fn
syn keyword copperBuiltin while if else and for
syn keyword copperBuiltin print println
syn keyword copperBool true false
syn keyword copperNull null
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


syn match copperNumber '[-+]\?\d\+\(\.\d\+\)\?'
syn match copperString '"\([^"\\]\|\\.\)*"'
syn match copperChar '\'\([^"\\]\|\\.\)\''

let b:current_syntax = "copper"
hi def link copperKeyword Keyword
hi def link copperBuiltin Function
hi def link copperNumber Number
hi def link copperString String
hi def link copperChar Character
hi def link copperBool Boolean
hi def link copperOperator Operator
hi def link copperNull Constant



