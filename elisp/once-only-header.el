;;; once-only-header.el --- add CPP once-only-include guards to header files

;; Copyright (C) 1996-2002 Bart Robinson <lomew@pobox.com>

;; Author: Bart Robinson <lomew@pobox.com>
;; Created: Fri Apr 5, 1996
;; Version: 3.0 ($Revision: 1.2 $)
(defconst ooh-version "3.0")
;; Date: Aug 17, 1997
;; Keywords: c c++ cpp preprocessor languages

;; This file is part of the filesytem.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Summary
;; =======
;;
;; This package provides an easy way to add CPP once-only-include
;; guards to header files.  For example:
;;
;;	#ifndef _SYS_TYPES_H_
;;	#define _SYS_TYPES_H_
;;	... the file's text ...
;;	#endif /* _SYS_TYPES_H_ */
;;
;; This package is smart about when to add CPP guards and is flexible
;; in how the guard symbol is specified.  Arbitrarily scoped guard
;; symbols, like the above example of <sys/types.h>, are fully
;; supported.
;;
;; Usage
;; =====
;;
;; The main entry point to this package is `ooh-maybe-insert-cpp-guard'
;; and one normally adds that function to the hook for whatever C or
;; C++ mode one is using, but it can also be called interactively.
;; See the function's documentation for the details of its operation.
;;
;; Here is an example of adding `ooh-maybe-insert-cpp-guard' to the C
;; and C++ mode hooks (assuming this file is on your `load-path'):
;;
;;	(autoload 'ooh-maybe-insert-cpp-guard "once-only-header" nil t)
;;	(add-hook 'c-mode-hook 'ooh-maybe-insert-cpp-guard)
;;	(add-hook 'c++-mode-hook 'ooh-maybe-insert-cpp-guard)
;;
;; (Note that if you're using cc-mode you can just add the function to
;; `c-mode-common-hook'.)
;;
;; Todo
;; ====
;;
;; - be able to replace instead of just detect existing cpp guards
;;   Would want to be flexible about how guards are detected and would
;;   need to be smart about finding the end of the guard -- i.e handling
;;   nested ifdefs correctly
;;
;; Tests
;; =====
;;
;; Things to try after modifying this file:
;;	- add ooh-maybe-insert-cpp-guard to your c/c++ hooks
;;	- try finding:
;;		- an empty .c file
;;		- an empty .h file
;;		- a full .c file with and without existing guard
;;		- a full .h file with and without existing guard
;;	- try running M-x ooh-maybe-insert-cpp-guard in the above plus:
;;		- a buffer like "foo" (i.e. no .c/.h)
;;		- a buffer with no underlying file

;;; History:

;; Sat Aug 17, 1997
;; * added scoped guard symbol functionality
;; * renamed symbols s/magic/guard/
;; * updated some doc
;;
;; Sun May 26, 1996
;; * incorporated suggestions from Jari Aalto <jari.aalto@ntc.nokia.com>
;;	- more configurable CPP symbol style (ooh-magic-sym-maker)
;;	- user can better guide the decision of whether to add magic
;;	  or not (ooh-user-approve-function)
;;	- uses comment-start/end
;; also added magic-detection (ooh-enchanted-p)
;;
;; Mon Jun 17, 1996
;; * incorporated suggestions from Nat Makarevitch <nat@nataa.fr.eu.org>
;;	- typo fixes
;;	- recognize .hpp
;;	- Nat wanted a different style of magic than the #ifndef foo ...
;;	  #define foo style, so I added ooh-magic-template-maker.
;;	  However I didn't change ooh-enchanted-p.

;;; Code:


;; user vars

(defvar ooh-guard-template-maker 'ooh-make-guard-template
  "*Specifies how to make the guard template.
Function is called with two args: the SYMBOL and WHENCE which is
either `start' or `end' depending on if the function is being called
to insert at the start or end of the file.  When WHENCE is `start' and
SYMBOL is _SYS_TYPES_H_ it should return something like
	#ifndef _SYS_TYPES_H_
	#define _SYS_TYPES_H_
And when WHENCE is `end' it should return something like
	#endif /* _SYS_TYPES_H_ */")

(defvar ooh-guard-sym-maker 'ooh-make-guard-sym
  "*Specifies how to generate a guard CPP symbol.
Function is called with one ARG, filename, and it should return
something like \"_TYPES_H_\".")

(defvar ooh-user-approve-function 'ignore
  "*User function to approve insertion of CPP guard.
Called with one ARG which is T if `ooh-maybe-insert-cpp-guard' was
called interactively.  When this function is invoked, the current
buffer is visiting the file.")

(defvar ooh-new-file-unmodify t
  "*If non-nil `ooh-maybe-insert-cpp-guard' will clear the buffer's modify flag
after inserting guard for a new file.")

(defvar ooh-header-file-regexp "\\.\\(h\\|hh\\|H\\|hxx\\|hpp\\)$"
  "*Regexp to match a header file name.")

(defvar ooh-search-max 4096
  "*Specifies how far to search for an existing CPP guard before giving up.")

(defvar ooh-load-hook nil
  "*Hook run when the \"once-only-header\" package is loaded.")


;; user functions

;;;###autoload
(defun ooh-maybe-insert-cpp-guard (&optional force)
  "Inserts a CPP once-only-include guard.

For example:

	#ifndef _SYS_TYPES_H_
	#define _SYS_TYPES_H_
	... the file's text ...
	#endif /* _SYS_TYPES_H_ */

This function is normally just added to the C/C++ mode hooks, but it
can also be called interactively.

Since the C and C++ mode hooks are run for other files besides
headers, this function needs to decide when inserting a guard is
appropriate.  It considers it appropriate when:

	a) optional FORCE argument is non-nil \(\\[universal-argument]
	   when interactive\)
	b) `ooh-user-approve-function' returns non-nil.
	c) the buffer is new \(i.e. empty\) and is visiting a filename
	   that looks like a header \(see `ooh-header-file-regexp'.\)
	d) it is called interactively and the buffer is visiting a
	   filename that looks like a header and that file doesn't
	   already have a guard, or the user says OK

Choosing the guard symbol is done interactively by prompting the user
with a reasonable default symbol (e.g., _TYPES_H_).  If a scoped
symbol is desired the user can type M-p when prompted to add the name
of the current directory to the symbol (e.g., _SYS_TYPES_H_).  If M-p
is typed again then the parent directory name is added (e.g.,
_INCLUDE_SYS_TYPES_H_), and so on.  Analogously, M-n removes the first
directory component (if any) from the default symbol.  If the user wishes
to provide an entirely different guard symbol, the minibuffer may be
edited accordingly.  If no CPP guarding is desired, the user can simply
erase the minibuffer contents and hit RET.

The format of the guard symbol and the CPP directives used can be
customized via `ooh-guard-sym-maker' and `ooh-guard-template-maker',
respectively.

Empty buffers are normally left unmodified after the guard is inserted
\(see `ooh-new-file-unmodify'\)."
  (interactive "P")
  (let ((unmodify (and ooh-new-file-unmodify (zerop (buffer-size))))
	sym)
    (if (or force (ooh-approve-insert (interactive-p)))
	(save-excursion
	  (setq sym (ooh-get-sym))
	  (if (string-equal sym "")
	      nil
	    (goto-char (point-min))
	    (insert (funcall ooh-guard-template-maker sym 'start))
	    (goto-char (point-max))
	    (insert (funcall ooh-guard-template-maker sym 'end))
	    (if unmodify
		(set-buffer-modified-p nil)))))))

(defun ooh-make-guard-template (sym whence)
  "For use as a value of `ooh-guard-template-maker'.
Returns something like
	#ifndef _SYS_TYPES_H_
	#define _SYS_TYPES_H_
when called with WHENCE as 'start, and 
	#endif /* _SYS_TYPES_H_ */
when WHENCE is 'end."
  (cond ((eq whence 'start)
	 (concat "#ifndef " sym "\n"
		 "#define " sym "\n\n"))
	((eq whence 'end)
	 (concat "\n\n#endif " "/* " sym " */" "\n"))))

(defun ooh-make-guard-sym (filename)
  "For use as a value of `ooh-guard-sym-maker'.
Given FILENAME as \"types.h\", return \"_TYPES_H_\"."
  (format "_%s_" (upcase (ooh-legalize-it filename))))

(defun ooh-legalize-it (sym)
  "Make a legal CPP symbol from SYM and return it.
We translate illegal chars to underscores."
  (concat (mapcar (lambda (c)
		    (if (eq (char-syntax c) ?w) c ?_))
		  sym)))


;; internal vars

(defvar ooh-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-p" 'ooh-prepend-dir)
    (define-key map "\M-n" 'ooh-remove-dir)
    (define-key map "\C-m" 'exit-minibuffer)
    (define-key map "\C-g" 'abort-recursive-edit)
    map)
  "The keymap used when prompting for a CPP symbol.")

(defvar ooh-filename)
(defvar ooh-dir-list)
(defvar ooh-dir-pos)


;; internal functions

(defun ooh-build-and-insert-filename ()
  ;; doc
  (let ((l (reverse (nthcdr ooh-dir-pos ooh-dir-list)))
	(filename ooh-filename))
    (while l
      (setq filename (concat (file-name-as-directory (car l))
			     filename))
      (setq l (cdr l)))
    (erase-buffer)
    (insert (funcall ooh-guard-sym-maker filename))))

(defun ooh-remove-dir ()
  ;; doc
  (interactive)
  (if (>= ooh-dir-pos (length ooh-dir-list))
      nil
    (setq ooh-dir-pos (1+ ooh-dir-pos))
    (ooh-build-and-insert-filename)))

(defun ooh-prepend-dir ()
  ;; doc
  (interactive)
  (if (<= ooh-dir-pos 0)
      nil
    (setq ooh-dir-pos (1- ooh-dir-pos))
    (ooh-build-and-insert-filename)))

(defun ooh-build-dir-list (directory)
  ;; Take a directory and build a list from its components
  ;;	/u/lomew/src/am-1/net  ->  ("u" "lomew" "src" "am-1 "net")
  ;; Note that we call `file-name-as-directory' on our argument to
  ;; ensure that it ends with a slash.  Otherwise that while loop will
  ;; terminate too soon.
  (let ((dir (file-name-as-directory directory))
	(ret nil))
    (while (and dir (not (string-equal (directory-file-name dir) dir)))
      (setq ret (cons (file-name-nondirectory (directory-file-name dir))
		      ret))
      (setq dir (file-name-directory (directory-file-name dir))))
    ret))

(defun ooh-get-sym ()
  ;; Get the CPP guard symbol from the user.
  (setq ooh-dir-list (ooh-build-dir-list (expand-file-name default-directory)))
  (setq ooh-dir-pos (length ooh-dir-list))
  (setq ooh-filename (file-name-nondirectory (or (buffer-file-name)
						 (buffer-name))))
  (read-from-minibuffer "CPP Symbol: "
			(funcall ooh-guard-sym-maker ooh-filename)
			ooh-map))

(defun ooh-approve-insert (interactive-flag)
  ;; Determine if we should insert a CPP guard.
  (or (funcall ooh-user-approve-function interactive-flag)
      (and (zerop (buffer-size))
	   (ooh-smells-like-header (buffer-file-name)))
      (and interactive-flag
	   (or (ooh-smells-like-header (buffer-file-name))
	       (y-or-n-p (concat
			  "This doesn't look like a C/C++ header file, "
			  "insert CPP guard anyway? ")))
	   (or (not (ooh-guarded-p))
	       (y-or-n-p "CPP guard detected, add another? ")))))

;; XXX I should probably make this more flexible; like when
;; ooh-guard-template-maker is not the default.
(defun ooh-guarded-p ()
  ;; Try to guess if the buffer has a guard already.
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^#ifndef \\([a-zA-Z0-9_]+\\)\n#define \\1"
		       ooh-search-max t)))

(defun ooh-smells-like-header (filename)
  ;; Return non-nil if FILENAME looks or smells like a header file.
  (and (stringp filename)
       (string-match ooh-header-file-regexp filename)))


(provide 'once-only-header)
(run-hooks 'ooh-load-hook)

;;; once-only-header.el ends here 
