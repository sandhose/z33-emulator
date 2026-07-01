/**
 * @file Z33 (Zorglub-33) assembly grammar for tree-sitter
 * @author Quentin Gliech <gliech@unistra.fr>
 * @license MIT
 *
 * This grammar mirrors the chumsky-based reference parser in
 * `emulator/src/parser/`. Notable points that were verified against the
 * reference implementation:
 *
 *  - Comments are `//` to end of line (`parser/assembly.rs::inline_comment`).
 *  - Lines are significant: the reference parser is line-based, and
 *    instructions have *optional* operands, so a bare identifier is a valid
 *    immediate operand. Without significant newlines two consecutive
 *    zero-operand instructions (e.g. `trap` / `reset` in `samples/handler.s`)
 *    would ambiguously parse as one instruction with an operand. Newlines are
 *    therefore terminators, not `extras`.
 *  - Instruction mnemonics, registers and directive names are
 *    case-insensitive (`to_ascii_lowercase` / `eq_ignore_ascii_case` in the
 *    reference). Preprocessor keywords are case-sensitive / lowercase
 *    (`just("define")` etc. in `parser/preprocessor.rs`).
 *  - Registers are `%a %b %pc %sp %sr` (`parser/shared.rs::register`).
 *  - Number literals: hex `0x`, octal `0o`, binary `0b`, decimal — all
 *    unsigned; negatives come from the unary `-` operator
 *    (`parser/shared.rs`).
 *  - Expression operators, lowest→highest precedence:
 *    `|`, `&`, `<<`/`>>`, `+`/`-`, `*`/`/`, unary `-`/`~`, atoms
 *    (`parser/expression.rs`, `parser/shared.rs::expression`).
 *  - Addressing modes: immediate (expr), register, direct `[expr]`,
 *    indirect `[%reg]`, indexed `[%reg + expr]` / `[%reg - expr]`
 *    (`parser/assembly.rs::instruction_argument`).
 *  - Directives: `.word .space .string .addr`; argument is a string literal
 *    or an expression (`parser/assembly.rs::directive_argument`).
 *  - Preprocessor directives: `#include "path"`, `#define NAME body`,
 *    `#undefine NAME`, `#error "msg"`, and the
 *    `#if / #elif / #else / #endif` block. `#` may be followed by spaces
 *    before the keyword. `#define` / `#if` / `#elif` bodies are raw text to
 *    end of line, stopping before a `//` comment and never containing `/`
 *    (`parser/preprocessor.rs::directive_argument`).
 */

/// Build a case-insensitive RegExp matching the given literal word.
function ci(word) {
  return new RegExp(
    word
      .split("")
      .map((c) =>
        /[a-zA-Z]/.test(c)
          ? `[${c.toLowerCase()}${c.toUpperCase()}]`
          : c.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"),
      )
      .join(""),
  );
}

const MNEMONICS = [
  "add", "and", "call", "cmp", "div", "fas", "in", "jmp", "jeq", "jne",
  "jle", "jlt", "jge", "jgt", "ld", "mul", "neg", "nop", "not", "or",
  "out", "pop", "push", "reset", "rti", "rtn", "shl", "shr", "st", "sub",
  "swap", "trap", "xor",
];

const DIRECTIVES = ["word", "space", "string", "addr"];
const REGISTERS = ["a", "b", "pc", "sp", "sr"];

module.exports = grammar({
  name: "z33",

  extras: ($) => [/[ \t]/, $.comment],

  word: ($) => $.identifier,

  rules: {
    source_file: ($) => seq(repeat($._item), optional($._line_content)),

    // An item is either a single line (terminated by a newline) or a
    // multi-line preprocessor conditional block.
    _item: ($) => choice(seq(optional($._line_content), $.newline), $.preproc_if),

    _line_content: ($) =>
      choice(
        $._code_line,
        $.preproc_include,
        $.preproc_define,
        $.preproc_undef,
        $.preproc_error,
      ),

    // Labels flush-left, then an optional instruction/directive; or an
    // instruction/directive with no labels.
    _code_line: ($) =>
      choice(seq(repeat1($.label), optional($._statement)), $._statement),

    _statement: ($) => choice($.instruction, $.directive),

    newline: (_) => token(/\r?\n/),

    comment: (_) => token(seq("//", /[^\n]*/)),

    // -------------------------------------------------------------------
    // Labels
    // -------------------------------------------------------------------

    label: ($) => seq(field("name", $.identifier), ":"),

    // -------------------------------------------------------------------
    // Instructions
    // -------------------------------------------------------------------

    instruction: ($) =>
      seq(
        field("mnemonic", $.mnemonic),
        optional(seq($._operand, repeat(seq(",", $._operand)))),
      ),

    mnemonic: (_) => token(prec(2, choice(...MNEMONICS.map(ci)))),

    _operand: ($) =>
      choice($.register, $.direct, $.indirect, $.indexed, $._expression),

    // [expr]
    direct: ($) => seq("[", $._expression, "]"),

    // [%reg]
    indirect: ($) => seq("[", $.register, "]"),

    // [%reg + expr] / [%reg - expr]
    indexed: ($) =>
      seq("[", $.register, field("operator", choice("+", "-")), $._expression, "]"),

    // -------------------------------------------------------------------
    // Directives
    // -------------------------------------------------------------------

    directive: ($) =>
      seq(
        field("name", $.directive_name),
        field("argument", choice($.string, $._expression)),
      ),

    directive_name: (_) =>
      token(prec(2, seq(".", choice(...DIRECTIVES.map(ci))))),

    // -------------------------------------------------------------------
    // Expressions
    // -------------------------------------------------------------------

    _expression: ($) =>
      choice(
        $.number,
        $.identifier,
        $.parenthesized_expression,
        $.unary_expression,
        $.binary_expression,
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    unary_expression: ($) =>
      prec.right(
        6,
        seq(field("operator", choice("-", "~")), field("operand", $._expression)),
      ),

    binary_expression: ($) => {
      const table = [
        [1, "|"],
        [2, "&"],
        [3, choice("<<", ">>")],
        [4, choice("+", "-")],
        [5, choice("*", "/")],
      ];
      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(
            precedence,
            seq(
              field("left", $._expression),
              field("operator", operator),
              field("right", $._expression),
            ),
          ),
        ),
      );
    },

    // -------------------------------------------------------------------
    // Registers, numbers, strings, identifiers
    // -------------------------------------------------------------------

    register: (_) => token(prec(2, seq("%", choice(...REGISTERS.map(ci))))),

    number: (_) =>
      token(
        prec(
          2,
          choice(
            /0[xX][0-9a-fA-F]+/,
            /0[oO][0-7]+/,
            /0[bB][01]+/,
            /[0-9]+/,
          ),
        ),
      ),

    string: ($) =>
      seq('"', repeat(choice($.escape_sequence, $._string_content)), '"'),

    _string_content: (_) => token.immediate(/[^"\\\r\n]+/),

    // \\  \"  \n  and line-continuation (backslash + newline)
    escape_sequence: (_) => token.immediate(/\\(?:[\\"n]|\r?\n)/),

    identifier: (_) => /[A-Za-z_][A-Za-z0-9_]*/,

    // -------------------------------------------------------------------
    // Preprocessor
    // -------------------------------------------------------------------

    // Raw argument text used by `#define` and `#if`/`#elif` (verified against
    // `parser/preprocessor.rs::directive_argument`): a run that neither starts
    // nor ends with a space, contains no `/` (so it stops before a `//`
    // comment) and no newline.
    preproc_arg: (_) => token(/[^ \t\r\n/](?:[^\r\n/]*[^ \t\r\n/])?/),

    preproc_include: ($) =>
      seq(
        alias(token(seq("#", /[ \t]*/, "include")), $.preproc_directive),
        field("path", $.string),
      ),

    preproc_error: ($) =>
      seq(
        alias(token(seq("#", /[ \t]*/, "error")), $.preproc_directive),
        field("message", $.string),
      ),

    preproc_define: ($) =>
      seq(
        alias(token(seq("#", /[ \t]*/, "define")), $.preproc_directive),
        field("name", $.identifier),
        optional(field("value", $.preproc_arg)),
      ),

    preproc_undef: ($) =>
      seq(
        alias(token(seq("#", /[ \t]*/, "undefine")), $.preproc_directive),
        field("name", $.identifier),
      ),

    preproc_if: ($) =>
      // `prec.right` resolves the ambiguity of the optional trailing newline
      // after `#endif` versus a following blank line: prefer consuming it.
      prec.right(seq(
        alias(token(seq("#", /[ \t]*/, "if")), $.preproc_directive),
        field("condition", $.preproc_arg),
        $.newline,
        repeat($._item),
        repeat($.preproc_elif),
        optional($.preproc_else),
        alias(token(seq("#", /[ \t]*/, "endif")), $.preproc_directive),
        // Optional so a file ending in `#endif` with no trailing newline
        // still parses cleanly (no MISSING node).
        optional($.newline),
      )),

    preproc_elif: ($) =>
      seq(
        alias(token(seq("#", /[ \t]*/, "elif")), $.preproc_directive),
        field("condition", $.preproc_arg),
        $.newline,
        repeat($._item),
      ),

    preproc_else: ($) =>
      seq(
        alias(token(seq("#", /[ \t]*/, "else")), $.preproc_directive),
        $.newline,
        repeat($._item),
      ),
  },
});
