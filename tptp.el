;;; tptp.el --- Work with TPTP files, theorem provers, and model finders
;;
;; Filename: tptp.el
;; Description: Work with TPTP files, theorem provers, and model finders.
;; Author: Jesse Alama (jesse.alama@gmail.com)
;; Maintainer: Jesse Alama (jesse.alama@gmail.com)
;; Copyright (C) 2011, Jesse Alama, all rights reserved.
;; Keywords: TPTP, automated theorem prover, automated theorem proving,
;;   ATP, LADR, prover9, eprover, vampire
;; Compatibility: GNU Emacs: 23.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;; Acknowledgments:
;;
;; Ed Zalta provided the impetus for launching this package.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Names of theorem provers and model finders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *eprover-program* "eprover"
  "The E theorem prover program.

That value of this variable can be either a path or a
program name.  If it is not an absolute path, your PATH
environment variable will be consulted to determine where
eprover can be found.")

(defvar *paradox-program* "paradox"
  "The paradox model finder program.

That value of this variable can be either a path or a
program name.  If it is not an absolute path, your PATH
environment variable will be consulted to determine where
eprover can be found.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Invoking a theorem prover or model finder on the (contents of the)
;;; current buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eprove-current-buffer (additional-e-arguments)
  "Invoke the E prover on the current buffer."
  (interactive "sAdditional flags with which E will be invoked: ")
  (save-buffer)
  (let ((eprover-buffer (get-buffer-create "*eprover*"))
	(tptp-file (buffer-file-name)))
    (save-excursion
      (switch-to-buffer eprover-buffer)
      (erase-buffer)
      (insert "Calling E like this:")
      (newline 2)
      (if (string= additional-e-arguments "")
	  (insert "  " *eprover-program* " " tptp-file)
	  (insert "  " *eprover-program* " " additional-e-arguments " " tptp-file))
      (newline 2)
      (insert "Results:")
      (newline)
      (insert "======================================================================")
      (newline)
      (if (string= additional-e-arguments "")
	  (call-process *eprover-program* nil t t tptp-file)
	  (call-process *eprover-program* nil t t additional-e-arguments tptp-file))
      (setf buffer-read-only t))))

(defun paradox-current-buffer ()
  "Invoke the paradox model finder on the current buffer."
  (interactive)
  (save-buffer)
  (let ((paradox-buffer (get-buffer-create "*paradox*"))
	(tptp-file (buffer-file-name)))
    (save-excursion
      (switch-to-buffer paradox-buffer)
      (erase-buffer)
      (call-process *paradox-program* nil t t tptp-file)
      (setf buffer-read-only t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tptp)

;;; tptp.el ends here