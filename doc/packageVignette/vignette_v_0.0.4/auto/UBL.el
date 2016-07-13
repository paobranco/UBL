(TeX-add-style-hook "UBL"
 (lambda ()
    (LaTeX-add-bibliographies)
    (TeX-add-symbols)
    (TeX-run-style-hooks
     "breakurl"
     "fancyvrb"
     "url"
     "graphicx"
     "amssymb"
     "amsfonts"
     "amsmath"
     ""
     "inputenc"
     "utf8x"
     "latex2e"
     "art10"
     "article"
     "a4paper"
     "10pt")))

