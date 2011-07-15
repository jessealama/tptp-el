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
;;; Variables
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

(defun eprove-current-buffer ()
  "Invoke the E prover on the current buffer."
  (interactive)
  (save-buffer)
  (let ((eprover-buffer (get-buffer-create "*eprover*"))
	(tptp-file (buffer-file-name)))
    (save-excursion
      (switch-to-buffer eprover-buffer)
      (erase-buffer)
      (call-process *eprover-program* tptp-file t t)
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
      (call-process *paradox-program* tptp-file t t)
      (setf buffer-read-only t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tptp)

;;; tptp.el ends here