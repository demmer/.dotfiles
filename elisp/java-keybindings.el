;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  java-keybindings.el     msh, mjd    2/96
;;;
;;;  keybindings for cs016 java-mode menus
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(message "Loading java keys.")

;; mjd: commenting out all the menu stuff

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   MENU BAR CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-key java-mode-map [menu-bar] (make-sparse-keymap))
;(define-key java-mode-map [menu-bar tools] 'undefined)
;(define-key java-mode-map [menu-bar search] 'undefined)
;(define-key java-mode-map [menu-bar buffer] 'undefined)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   CLASS BROWSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(define-key java-mode-map [C-down-mouse-2] 'jcb-class-browser)
;;;(cond (window-system
;;;       (local-set-key [C-down-mouse-2] 'jcb-class-browser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   PACKAGE MENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-key java-mode-map "\C-o" 'java-open-package)
;(define-key java-mode-map "\C-q" 'java-close-package)
;(define-key java-mode-map "\C-u" 'java-new-package)
;(define-key java-mode-map "\C-cn" 'java-new-class)
;(define-key java-mode-map "\C-c\C-n" 'java-new-class)
;(define-key java-mode-map "\C-ci" 'java-new-interface)
;(define-key java-mode-map "\C-c\C-i" 'java-new-interface)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   COMPILE MENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-key java-mode-map "\M-k" 'java-compile)
;(define-key java-mode-map "\M-p" 'java-compile-all)
;(define-key java-mode-map [f12] 'java-next-error)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   COMMENT MENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key java-mode-map "\M-i" 'java-instance-comment)
(define-key java-mode-map "\M-f" 'java-function-comment)
(define-key java-mode-map "\M-c" 'java-class-comment)
(define-key java-mode-map "\M-h" 'java-header-comment)
(define-key java-mode-map "\M-9" 'java-box-enterexit)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   DEBUG MENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-key java-mode-map [f7] 'java-debug-run)
;(define-key java-mode-map "\C-x\C-r" 'java-nodebug-run)
;(define-key java-mode-map [f8] 'java-debug-setbreak)
;(define-key java-mode-map [f9] 'java-debug-step)
;(define-key java-mode-map [f10] 'java-debug-next)
;(define-key java-mode-map [SunF36] 'java-debug-cont)
;(define-key java-mode-map [SunF37] 'java-debug-print)
;(define-key java-mode-map "\C-x\C-q" 'java-debug-quit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Taken from c++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key java-mode-map "{"         'c-electric-brace)
(define-key java-mode-map "}"         'c-electric-brace)
(define-key java-mode-map ";"         'c-electric-semi&comma)
(define-key java-mode-map "#"         'c-electric-pound)
(define-key java-mode-map ":"         'c-electric-colon)

(define-key java-mode-map "\e\C-h"    'c-mark-function)
(define-key java-mode-map "\e\C-q"    'c-indent-exp)
(define-key java-mode-map "\ea"       'c-beginning-of-statement)
(define-key java-mode-map "\ee"       'c-end-of-statement)

(define-key java-mode-map "\eq"       'c-fill-paragraph)
(define-key java-mode-map "\C-c\C-f"  'c-forward-conditional)
(define-key java-mode-map "\C-c\C-p"  'c-backward-conditional)
(define-key java-mode-map "\C-c\C-U"  'c-up-conditional)
(define-key java-mode-map "\t"        'c-indent-command)
(define-key java-mode-map "\177"      'c-electric-delete)

(define-key java-mode-map ","         'c-electric-semi&comma)
(define-key java-mode-map "/"         'c-electric-slash)
(define-key java-mode-map "*"         'c-electric-star)
(define-key java-mode-map "\C-c\C-q"  'c-indent-defun)
(define-key java-mode-map "\C-c\C-\\" 'c-backslash-region)

(define-key java-mode-map "\C-c\C-a"  'c-toggle-auto-state)
(define-key java-mode-map "\C-c\C-b"  'c-submit-bug-report)
(define-key java-mode-map "\C-c\C-c"  'comment-region)
(define-key java-mode-map "\C-c\C-d"  'c-toggle-hungry-state)
(define-key java-mode-map "\C-c\C-e"  'c-macro-expand)
(define-key java-mode-map "\C-c\C-o"  'c-set-offset)
(define-key java-mode-map "\C-c\C-s"  'c-show-syntactic-information)
(define-key java-mode-map "\C-c\C-t"  'c-toggle-auto-hungry-state)

(define-key java-mode-map [f20] 'clipboard-kill-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   GENERAL LAST BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key java-mode-map "\C-c\C-u"  'uncomment-region)
(define-key java-mode-map "\C-z" 'undefined)
(provide 'java-keybindings)


