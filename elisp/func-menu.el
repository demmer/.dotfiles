;;; func-menu.el --- Jump to a function within a buffer.
;; Copyright (C) 1994 Ake Stenhoff <etxaksf@aom.ericsson.se>  
;;
;; Author: Ake Stenhoff <etxaksf@aom.ericsson.se>
;; Created: 10 Sep 1993
;; Version: 2.0
;; Keywords: tools
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Purpose of this package:
;;   Jump to any function within the current buffer.
;;
;; How it works:
;;   Hit a key and it will generate a list of all functions in the 
;;   current buffer and bring up a dialog that you can choose from.
;;   Current version supports C/C++ and Lisp/Emacs Lisp.
;;
;; Installation:
;;   Put this file in your load-path and insert the following in .emacs
;;
;;   (and window-system 
;;      (require 'func-menu)
;;      (define-key global-map [S-down-mouse-3] 
;;        'function-menu))

;;; Change Log:
;;    v2.0 Jan 31 1994 Ake Stenhoff
;;       General workthrough before first public release.
;;       Thanks goes to Lars Lindberg.
;;    v1.9 Jan 19 1994 Ake Stenhoff
;;       Added the ability to display the function names
;;       in several menus.
;;       Set the variable function-menu-max-items to a
;;       value that fits the needs.
;;    v1.8 Jan 18 1994 Ake Stenhoff
;;       Now sorts the function names alphabetic (default) or
;;       in order of detection.
;;       See the variable function-menu-sort-p.
;;    v1.7 Jan 18 1994 Ake Stenhoff
;;       Added C++ regexp as a variable.
;;       Added ':' to the C++ regexp.
;;    v1.6 Jan 16 1994 Ake Stenhoff
;;       Clean up.
;;    v1.5 Oct 29 1993 Ake Stenhoff
;;       Made the jump in wrong buffer.
;;	 Thanks Lars. (Lars Lindberg)
;;    v1.4 Oct 26 1993 Ake Stenhoff
;;	 Skipping prototype declarations.
;;    v1.3 Oct 14 1993 Ake Stenhoff
;;	 Using forward-sexp instead of forward-word.
;;	 Thanks Lars. (Lars Lindberg).
;;    v1.2 Oct 8 1993 Ake Stenhoff
;;       Made a better default regular expression for C and C++ mode.
;;    v1.1 Sep 21 1993 Ake Stenhoff
;;       Now calling (push-mark) before jumping to the choosen 
;;	 function.
;;       Thanks to Lars Lindberg for his suggestion.
;;    v1.0 Sep 10 1993 Ake Stenhoff
;;      First release.

;;; Code
(require 'cl)

;;;
;;; Customizable variables
;;;

(defvar fume-sort-p t
  "*Set this to nil if you don't want any sorting (faster).") 

(defvar fume-max-items 30
  "*Maximum number of elements in a menu.")

;; Every fume-function-name-regexp-<language> should uniqily identify
;; a function for that certain language.
(defvar fume-function-name-regexp-lisp
  "^(defun [A-Za-z0-9_-]+[	 ]*("
  "Expression to get lisp function names")

(defvar fume-function-name-regexp-oopas
  "^\\(PROCEDURE\\|FUNCTION\\|CONSTRUCTOR\\|DESTRUCTOR\\) [a-zA-Z0-9.]+("
  "Expression to get oopas function names")

(defvar fume-function-name-regexp-c
  (concat 
   "^[a-zA-Z0-9]+[ \t]?"		; type specs; there can be no
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"	; more than 3 tokens, right?
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"
   "\\([*&]+[ \t]*\\)?"			; pointer
   "\\([a-zA-Z0-9_*]+\\)[ \t]*("	; name
   )
  "Expression to get C function names")



(defvar fume-function-name-regexp-c++
  (concat 
   "^[a-zA-Z0-9_:~]+[ \t]?"		; type specs; there can be no
   "\\([a-zA-Z0-9_:~*]+[ \t]+\\)?"	; more than 3 tokens, right?
   "\\([a-zA-Z0-9_:~*]+[ \t]+\\)?"
   "\\([*&]+[ \t]*\\)?"			; pointer
   "\\([a-zA-Z0-9_:~*]+\\)[ \t]*("	; name
   )
  "Expression to get C++ function names")

(defvar fume-function-name-regexp-java
  (concat
   "^[ \t]*[a-zA-Z0-9]+[ \t]?"		; type specs; there can be no
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"	; more than 3 tokens, right?
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"
   "\\([*&]+[ \t]*\\)?"			; pointer
   "\\([a-zA-Z0-9_*]+\\)[ \t]*("	; name
   )
  "Expression to get Java function names")

(defvar fume-function-name-regexp-tcl
  (concat
   "^[ \t]?\\([-a-zA-Z0-9_]+[ \t]*method\\|proc\\)[ \t]*"   ; classname method or proc
   "[a-zA-Z0-9_]*[ \t]*"                                  ; func name
   "\\({([- \tA-Za-z_0-9]*)?}\\)?"                        ; optional args
   "[ \t]*{"
   )
  "Expression to get tcl function names")

; (defun f ()
;   (interactive)
;   (re-search-forward fume-function-name-regexp-tcl nil))


(defvar fume-function-name-regexp-alist 
  (list
   (cons 'emacs-lisp-mode fume-function-name-regexp-lisp)
   (cons 'lisp-mode fume-function-name-regexp-lisp)
   (cons 'c-mode fume-function-name-regexp-c)
   (cons 'c++-mode fume-function-name-regexp-c++)
   (cons 'c++-c-mode fume-function-name-regexp-c++)
   (cons 'java-mode fume-function-name-regexp-java)
   (cons 'oopascal-mode fume-function-name-regexp-oopas)
   (cons 'tcl-mode fume-function-name-regexp-tcl))
  "The connection between a mode and the regexp that matches function names.")

;;;
;;; Buffer local variables
;;;

;;The latest list of function names in the buffer.
(defvar fume--funclist nil)
(make-variable-buffer-local 'fume--funclist)

(defvar fume--function-name-regexp nil
  "The keywords to show in a menu")
(make-variable-buffer-local 'fume--function-name-regexp)

;;;
;;; Sets 'fume--function-name-regexp' to something appropriate for the
;;; current mode for this buffer.
;;;
(defun fume--set-defaults ()
  (setq fume--function-name-regexp
	(cdr-safe (assoc major-mode fume-function-name-regexp-alist))))

;;;
;;; Sort function
;;; Sorts the items depending on their function-name
;;; An item look like (NAME . POSITION).
;;;
(defun fume--sort-by-name (item1 item2)
  (string-lessp (upcase (car item1)) (upcase (car item2))))

;;;
;;; Support function to calculate relative position in buffer
(defun fume--relative-position ()
  (let ((pos (point))
	 (total (buffer-size)))
    (if (> total 50000)
	;; Avoid overflow from multiplying by 100!
	(/ (1- pos) (max (/ total 100) 1))
      (/ (* 100 (1- pos)) (max total 1)))))
;;;
;;; Get the next function name in the buffer.
;;;
(defun fume--find-next-function-name ()
  (let (char)
    ;; Search for the function
    (cond
     ((re-search-forward
       fume--function-name-regexp
       nil t)
      (message "Scanning buffer. (%3d%%) done." (fume--relative-position))
      (backward-up-list 1)
      (save-excursion
	(goto-char (scan-sexps (point) 1))
	(setq char (following-char)))
      ;; Skip this function name if it is a prototype declaration.
      (if (and (not (eq major-mode 'oopascal-mode)) (eq char ?\;))
	  (fume--find-next-function-name)

	(let (beg end name)
	  ;; Get the function name and position
	  (setq end (point))
	  (forward-sexp -1)
	  (setq beg (point))
	  (setq name nil)
	  
	  (cond 
	   
	   ((eq major-mode 'c++-mode)
	    
	    (if (eq (preceding-char) ?\:)
		(forward-sexp -1))
	    (setq beg (point))
	    (setq name (buffer-substring beg end)))
	   
	   ((eq major-mode 'tcl-mode)
	    
	    (forward-sexp -1)
	    (let (keyword temp)
	      (setq keyword (buffer-substring (point) beg))
	      (cond
	       ((string-equal keyword "method ")
		(setq temp (point))
		(forward-sexp -1)
		(setq name (concat (buffer-substring (point) temp)
				   (buffer-substring beg end)))
		)
	       
	       (t
		(setq name (buffer-substring beg end))
		))
;	      (message (format "Found tcl func \"%s\" keyword \"%s\"" name keyword))
	    ))
	   
	   (t
	    (setq name (buffer-substring beg end)))
	   )

	  (goto-char end)
	  (cond
	   ((or
	     (string-equal name "if")
	     (string-equal name "while")
	     (string-equal name "for")
	     (string-equal name "synchronized"))
	    (cons name beg)
	    )
	   (t (cons name beg))
	   ))))
     (t
      (message "Scanning buffer. (100%%) done.")
      nil))))

;; Split LIST into sublists of max length N.
;; Example (fume--split '(1 2 3 4 5 6 7 8) 3)-> '((1 2 3) (4 5 6) (7 8))
(defun fume--split (list n)
  (let ((remain list)
	(result '())
	(sublist '())
	(i 0))
    (while remain
      (push (pop remain) sublist)
      (incf i)
      (and (= i n)
	   ;; We have finished a sublist
	   (progn (push (nreverse sublist) result)
		  (setq i 0)
		  (setq sublist '()))))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (and sublist
	 (push (nreverse sublist) result))
    (nreverse result)))

;;;
;;; Splits a menu in to several menus.
;;;
(defun fume--split-menu (menulist)
  (cons "Function menus"
	(mapcar
	 (function
	  (lambda (menu)
	    (cons (format "(%s)" (car (car menu)))
		  (cons '("*Rescan*" . -99) menu))))
	 (fume--split menulist fume-max-items))))

(defun fume--should-ignore (name)
  (cond
   ((or
     (string-equal name "if")
     (string-equal name "while")
     (string-equal name "for")
     (string-equal name "synchronized")
     ) t)
   (t nil)
   ))

;;;
;;; The main function for this package!
;;;
(defun function-menu (event)
  "Pop up a menu of functions for selection with the mouse.
Jumps to the selected function.  A mark is set at the old position,
so you can easily go back with C-u \\[set-mark-command]."
  (interactive "e")
  ;; See to that the window where the mouse is really is selected.
  (let ((window (posn-window (event-start event))))
    (or (framep window) (select-window window)))
  (or (fume--set-defaults)
      (error "The mode \"%s\" is not implemented in 'function-menu' yet." mode-name))
  ;; Create a list for this buffer only if there isn't any. 
  (let ((funcname)
	(funclist '())
	(menu '()))
    (or fume--funclist
	(save-excursion
	  (beginning-of-buffer)
	  (while (setq funcname (fume--find-next-function-name))
	    (if (fume--should-ignore (car funcname)) ()
	      (setq funclist (cons funcname funclist))))
	  (if fume-sort-p
	      (setq fume--funclist
		    (sort funclist 'fume--sort-by-name))
	    (setq fume--funclist (nreverse funclist)))))
    (or fume--funclist
	(error "No functions found in this buffer."))

    ;; Create the menu
    (setq menu (fume--split-menu fume--funclist))
    (let ((position (x-popup-menu event menu)))
      (cond
       ((not position)
	nil)
       ((= position -99)
	;; User wants to rescan the buffer.
	(setq fume--funclist nil)
	(function-menu event))
       (t
	;; Jump to selected function.
	(push-mark)
	(goto-char position))))))

(provide 'func-menu)

;;; func-menu.el ends here
