; Comments
(comment) @comment

; --- Preprocessor -----------------------------------------------------------
(preproc_directive) @keyword.directive

; The name introduced by `#define NAME` / `#undefine NAME` is a constant.
(preproc_define name: (identifier) @constant)
(preproc_undef name: (identifier) @constant)

; --- Labels -----------------------------------------------------------------
; A label *definition* (`name:`).
(label name: (identifier) @function)

; --- Instructions & directives ---------------------------------------------
(mnemonic) @keyword
(directive_name) @keyword.directive

; --- Registers --------------------------------------------------------------
(register) @variable.builtin

; --- Literals ---------------------------------------------------------------
(number) @number
(string) @string
(escape_sequence) @string.escape

; --- Identifiers (label / macro references inside expressions) --------------
(identifier) @variable

; --- Operators --------------------------------------------------------------
(unary_expression operator: _ @operator)
(binary_expression operator: _ @operator)
(indexed operator: _ @operator)

; --- Punctuation ------------------------------------------------------------
["[" "]" "(" ")"] @punctuation.bracket
["," ":"] @punctuation.delimiter
