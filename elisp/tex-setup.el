;;
;; Setup tex / bibtex modes
;;
(require 'tex-mode)
(require 'skeleton)
(require 'tex-site nil t)

(setq font-latex-title-fontity (quote color))


(require 'latex)
(require 'longlines)
(defun my-tex-setup()
 (interactive)
 (message "my-tex-setup")
 (auto-fill-mode nil)
 (longlines-mode t)
 (setq LaTeX-indent-level 0)
 (setq LaTeX-item-indent 0)
 (setq indent-line-function 'indent-relative)
 )

(add-hook 'tex-mode-hook 'my-tex-setup)
(add-hook 'TeX-mode-hook 'my-tex-setup)

;; (defun latex-insert-section(type)
;;   "Insert a \\section{} style latex declaration."
;;   (interactive)
;;   (let ((section (read-string (format "\\%s: " type))))
;;     (insert "\\" type "{" section "}\n")))

;; (defun latex-insert-face(face)
;;   "Insert a \textbf{} style typeface declaration"
;;   (interactive)
;;   (insert "\\" face "{}")
;;   (backward-char 1))

;; (define-skeleton tex-latex-block-no-options
;;   "Same as tex-latex-block but without any options."
;;   (let ((choice (completing-read (format "LaTeX block name [%s]: "
;; 					 latex-block-default)
;; 				 (mapcar 'list
;; 					 (append standard-latex-block-names
;; 						 latex-block-names))
;; 				 nil nil nil nil latex-block-default)))
;;     (setq latex-block-default choice)
;;     (unless (or (member choice standard-latex-block-names)
;; 		(member choice latex-block-names))
;;       ;; Remember new block names for later completion.
;;       (push choice latex-block-names))
;;     choice)
;;   \n "\\begin{" str ?\}
;;   \n -2 _ \n
;;   "\\end{" str ?\} > \n)

;; (define-key tex-mode-map "\C-cb"
;;   (lambda () (interactive) (latex-insert-face "textbf")))
;; (define-key tex-mode-map "\C-ce"
;;   (lambda () (interactive) (latex-insert-face "emph")))
;; (define-key tex-mode-map "\C-ci"
;;   (lambda () (interactive) (latex-insert-face "item")))
;; (define-key tex-mode-map "\C-ct"
;;   (lambda () (interactive) (latex-insert-face "texttt")))
;; (define-key tex-mode-map "\C-c\C-a"
;;   (lambda () (interactive) (tex-latex-block-no-options "abstract")))
;; (define-key tex-mode-map "\C-c\C-x"
;;   (lambda () (interactive) (tex-latex-block-no-options "center")))
;; (define-key tex-mode-map "\C-c\C-e"
;;   (lambda () (interactive) (tex-latex-block-no-options "enumerate")))
;; (define-key tex-mode-map "\C-c\C-i"
;;   (lambda () (interactive) (tex-latex-block-no-options "itemize")))
;; (define-key tex-mode-map "\C-c\C-s"
;;   (lambda () (interactive) (latex-insert-section "section")))
;; (define-key tex-mode-map "\C-c\C-u"
;;   (lambda () (interactive) (latex-insert-section "subsection")))
;; (define-key tex-mode-map "\C-c\C-v"
;;   (lambda () (interactive) (tex-latex-block-no-options "verbatim")))
;; (define-key tex-mode-map "\C-c\C-c" 'comment-region)

(require 'bibtex)
(defun my-bibtex-setup()
  (interactive)
  (define-key bibtex-mode-map "\M-q" 'bibtex-fill-entry)
  (define-key bibtex-mode-map "\M-\C-l" 'lcvs-examine)
  (setq bibtex-align-at-equal-sign t)
  (setq fill-column 75))

(add-hook 'bibtex-mode-hook 'my-bibtex-setup)

