; A run button on every label definition; the label name reaches tasks as
; $ZED_CUSTOM_label (see editors/zed/README.md for the tasks.json binding).
; Data labels get a button too: a syntax query cannot tell them from code
; labels.
((label
   name: (identifier) @run @label)
 (#set! tag zorglub33-label))
