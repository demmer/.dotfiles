;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  java-mode.el        msh, mjd     2/96
;;;
;;;  big thanks here to bmm and amd
;;;
;;;  defines java-mode for cs016 java emacs...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set up our load path
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar java-mode-map nil "")
(setq java-mode-map (make-sparse-keymap))
(use-local-map java-mode-map)
(define-key java-mode-map [menu-bar] (make-sparse-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Purty colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 (window-system
  (make-face 'fl-java-member-func-face)
  (set-face-foreground 'fl-java-member-func-face "white")

  (make-face 'fl-java-atomic-face)
  (set-face-foreground 'fl-java-atomic-face "RoyalBlue1")

  (make-face 'fl-java-member-face)
  (set-face-foreground 'fl-java-member-face "white")

  (make-face 'fl-java-mod-face)
  (set-face-foreground 'fl-java-mod-face "SkyBlue1")

  (make-face 'fl-java-class-face)
  (set-face-foreground 'fl-java-class-face "RoyalBlue1")

  (make-face 'fl-java-instance-face)
  (set-face-foreground 'fl-java-instance-face "white")
  (set-face-font 'fl-java-instance-face (face-font 'italic))
  
  (make-face 'fl-java-comment-face)
  (set-face-foreground 'fl-java-comment-face "medium spring green")
  
  (make-face 'fl-java-gp-face)
  (set-face-foreground 'fl-java-gp-face "white")
  
  (make-face 'fl-java-flow-face)
  (set-face-foreground 'fl-java-flow-face "RoyalBlue1")
  
  )
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Font lock defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-java-font-lock-init ()
  (setq font-lock-java-member-face 'fl-java-member-face)
  (setq font-lock-java-member-func-face 'fl-java-member-func-face)
  (setq font-lock-java-atomic-face 'fl-java-atomic-face)
  (setq font-lock-java-mod-face 'fl-java-mod-face)
  (setq font-lock-java-class-face 'fl-java-class-face)
  (setq font-lock-java-gp-face 'fl-java-gp-face)
  (setq font-lock-java-instance-face 'fl-java-instance-face)
  (setq font-lock-java-comment-face 'fl-java-comment-face)
  (setq font-lock-java-flow-face 'fl-java-flow-face)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   All the keywords you can use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst java-keywords nil)

;; java keywords
(let (
      (class-keywords
       (concat "abstract\\|"
	       "class\\|"
	       "extends\\|"
	       "i\\(nterface\\|mp\\(lements\\|ort\\)\\)\\|"
	       "n\\(ew\\|ull\\)\\|"
	       "package\\|"
	       "super\\|"
	       "this"))
      (modifier-keywords
       (concat "final\\|"
	       "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|"
	       "static"))
      (atomic-keywords
       (concat "b\\(yte\\|oolean\\)\\|"
	       "char\\|"
	       "double\\|"
	       "float\\|"
	       "int\\|"
	       "long\\|"
               "short\\|"
	       "String\\|"
	       "void"))
      (flow-keywords
       (concat "break\\|"
	       "c\\(ase\\|atch\\|ontinue\\)\\|"
	       "d\\(efault\\|o\\)\\|"
	       "else\\|"
	       "f\\(or\\|inally\\)\\|"
	       "if\\|"
	       "return\\|"
	       "s\\(witch\\|ynchronized\\)\\|"
               "t\\(ry\\|hrow\\(\\|s\\)\\)\\|"
	       "while"))
      (gp "GP\.\\([a-zA-Z0-9_:~]+\\)[ \t]*")
      (member "\\([a-zA-Z0-9_:~]+\\)[ \t]*(")
      (comment "[ \t]*")
      )
 
 
  (setq java-keywords 
        (list
         (cons (concat "\\<\\(" class-keywords "\\)\\>")
               'font-lock-java-class-face)           
         (cons (concat "\\<\\(" modifier-keywords "\\)\\>")
               'font-lock-java-mod-face)           
         (cons (concat "\\<\\(" atomic-keywords "\\)\\>")
               'font-lock-java-atomic-face)
         (cons (concat "\\<\\(" flow-keywords "\\)\\>")
               'font-lock-java-flow-face)
         (cons (concat "\\<\\(" comment "\\)\\>")
               'font-lock-java-comment-face)
         (cons (concat "\\<\\(" gp "\\)\\>")
               'font-lock-java-gp-face)
         (cons (concat "\\<\\(" member "\\)\\>")
               'font-lock-java-member-func-face)
         (list member 1 'font-lock-java-member-func-face)        
       )))
 
(defconst my-java-style
  '("my-java"
    (c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist
     (statement-block-intro . +)
     (knr-argdecl-intro . 5)
     (substatement-open . +)
     (label . 0)
     (statement-case-open . +)
     (statement-cont . +)
     (arglist-intro . c-lineup-arglist-intro-after-paren)
     (arglist-close . c-lineup-arglist)
     (access-label . 0)
     ))
  "My Java Programming Style")

;;; my java setup 
(defun my-java-setup ()
  (interactive)
;;  (auto-fill-mode nil)
;;  (append-no-dup `(java-mode . (java-keywords)) font-lock-defaults-alist)
  (add-hook 'font-lock-mode-hook 'my-java-font-lock-init t)

  (setq comment-start "// ")
  (setq comment-end "")
  (make-variable-buffer-local 'using-c-comments)
  (setq using-c-comments nil)
  (make-variable-buffer-local 'c-comment-cplusplus)
  (setq c-comment-cplusplus t)
  (setq c-tab-always-indent t)
  (setq c-access-key nil)
  (setq indent-tabs-mode nil)
  (set-default 'c-access-key nil)
  (let ((my-style "my-java"))
    (or (assoc my-style c-style-alist)
	(setq c-style-alist (append c-style-alist (list my-java-style)))))
  (c-set-style "my-java")
  (setq c-baseclass-key
        (concat
         ":?[ \t]*\\(virtual[ \t]+\\)?\\("
         "[ \t]+\\)" c-symbol-key)
        )
  (cond (window-system
	 (font-lock-mode 1)
	 (setq font-lock-keywords java-keywords)
	 (font-lock-fontify-buffer)))
)

(add-hook 'java-mode-hook 'my-java-setup)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  load each java helper file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "java-comment")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  load java keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "java-keybindings")

;; (provide 'java-mode)






