;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; `my-auctex-options.el` to be
;; loaded by `my-loadpackages.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For document parsing
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; add `arara` to the TeX command list
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
		'("Arara" "arara --verbose %s" TeX-run-command t t :help "Run arara") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; end `my-auctex-options.el`
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
