; Z33 assembly has no block-structured indentation for code (labels are
; flush-left and instructions are indented purely by convention, and they are
; sibling lines rather than nested nodes, so that cannot be expressed here).
; The only genuine block structure is the preprocessor conditional.

(preproc_if) @indent.begin
(preproc_elif) @indent.begin
(preproc_else) @indent.begin

((preproc_directive) @indent.end
 (#match? @indent.end "endif"))
