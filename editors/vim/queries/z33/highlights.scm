; Z33 (Zorglub-33) assembly highlights for Neovim.
; Adapted from the Zed grammar queries (editors/zed/languages/zorglub33/).
; Capture names follow the nvim-treesitter conventions; semantics/predicates are
; unchanged from Zed.

; Identifiers (label / macro references inside expressions) come FIRST so the
; more specific captures below win under last-match-wins precedence.
(identifier) @variable

; Comments
(comment) @comment

; --- Preprocessor -----------------------------------------------------------
(preproc_directive) @keyword.directive

; The name introduced by `#define NAME` / `#undefine NAME` is a macro constant.
(preproc_define name: (identifier) @constant.macro)
(preproc_undef name: (identifier) @constant.macro)

; --- Labels -----------------------------------------------------------------
; A label *definition* (`name:`).
(label name: (identifier) @label)

; --- Instructions & directives ---------------------------------------------
; The mnemonic is lexically an identifier; only colour it as a keyword when it
; is actually one of the 33 mnemonics (case-insensitive), mirroring the
; reference parser. A non-mnemonic in mnemonic position is left uncoloured.
; NOTE: Neovim's `#match?` compiles the pattern with Vim's regex engine, not
; PCRE — so the Zed query's `(?i)` inline flag becomes `\v\c` (very-magic +
; ignore-case) here. Same semantics, different engine.
((mnemonic) @keyword
 (#match? @keyword
  "\\v\\c^(add|and|call|cmp|div|fas|in|jmp|jeq|jne|jle|jlt|jge|jgt|ld|mul|neg|nop|not|or|out|pop|push|reset|rti|rtn|shl|shr|st|sub|swap|trap|xor)$"))
(directive_name) @keyword.directive

; --- Registers --------------------------------------------------------------
(register) @variable.builtin

; --- Literals ---------------------------------------------------------------
(number) @number
(string) @string
(escape_sequence) @string.escape

; --- Operators --------------------------------------------------------------
(unary_expression operator: _ @operator)
(binary_expression operator: _ @operator)
(indexed operator: _ @operator)

; --- Punctuation ------------------------------------------------------------
["[" "]" "(" ")"] @punctuation.bracket
["," ":"] @punctuation.delimiter
