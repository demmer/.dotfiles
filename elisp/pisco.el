;; pisco specific elisp

(require 'lpr)
(setq lpr-page-header-program "cat")
(setq lpr-page-header-switches nil)
(setq lpr-command "enscript")
(setq lpr-headers-switches (list "-B"))

;; (if (require 'zenirc nil t)
;;     (setq zenirc-server-default "tier")
;;   )

(setq mail-host-address "cs.berkeley.edu")
