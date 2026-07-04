; Label definitions appear in the outline / breadcrumbs.
(label
  name: (identifier) @name) @item

; `#define`d constants appear in the outline too.
(preproc_define
  (preproc_directive) @context
  name: (identifier) @name) @item
