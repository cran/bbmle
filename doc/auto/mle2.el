(TeX-add-style-hook "mle2"
 (lambda ()
    (LaTeX-add-bibliographies)
    (TeX-add-symbols
     '("code" 1))
    (TeX-run-style-hooks
     "latex2e"
     "art10"
     "article")))

