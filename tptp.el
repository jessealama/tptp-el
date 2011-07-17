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
;;; Variables and constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +report-separator+
  "======================================================================"
  "The separator to use when generating reports.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +empty-string+ ""
  "The empty string.")

(defmacro empty-string? (thing)
  "Assuming that THING is a string, determine whether it is empty."
  `(string= ,thing +empty-string+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tptp nil
  "Work with TPTP files: find proofs and (counter)models."
  :group 'emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Names of theorem provers and model finders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom *eprover-program* "eprover"
  "The E theorem prover program.

That value of this variable can be either a path or a
program name.  If it is not an absolute path, your PATH
environment variable will be consulted to determine where
eprover can be found."
  :tag "E program"
  :group 'tptp
  :type '(string))

(defcustom *paradox-program* "paradox"
  "The paradox model finder program.

That value of this variable can be either a path or a
program name.  If it is not an absolute path, your PATH
environment variable will be consulted to determine where
paradox can be found."
  :tag "Paradox program"
  :group 'tptp
  :type '(string))

(defcustom *equinox-program* "equinox"
  "The equinox theorem prover program.

That value of this variable can be either a path or a
program name.  If it is not an absolute path, your PATH
environment variable will be consulted to determine where
equinox can be found."
  :tag "Equinox program"
  :group 'tptp
  :type '(string))

(defcustom *vampire-program* "vampire"
  "The name of the vampire theorem prover.

That value of this variable can be either a path or a
program name.  If it is not an absolute path, your PATH
environment variable will be consulted to determine where
vampire can be found.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Proof viewing mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar view-proof-mode-map nil "Keymap used by view-proof mode.")

(unless view-proof-mode-map

  (setf view-proof-mode-map (make-sparse-keymap))
  
  ; Navigation
  (define-key view-proof-mode-map " " 'forward-page)
  (define-key view-proof-mode-map "f" 'forward-page)
  (define-key view-proof-mode-map "DEL" 'backward-page)
  (define-key view-proof-mode-map "b" 'backward-page)

  ; Saving proofs
  (define-key view-proof-mode-map "s" 'view-proof-save-deduction)

  ; Help
  (define-key view-proof-mode-map "h" 'describe-mode)
  (define-key view-proof-mode-map "H" 'describe-mode)
  (define-key view-proof-mode-map "?" 'describe-mode))

(defun view-proof-mode (&optional arg)
  "Major mode for viewing proofs.

If ARG is a negative integer, disable `view-proof-mode'; otherwise, enable this mode."
  (interactive "p")
  (kill-all-local-variables)
  (use-local-map view-proof-mode-map)
  (setf major-mode 'view-proof-mode
	mode-name "View-Proof"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Saving deductions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-proof-save-deduction ()
  "Save the results of the current proof to a file."
  (interactive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Model view mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar view-model-mode-map nil "Keymap used by view-model mode.")

(unless view-model-mode-map
  (setf view-model-mode-map (make-sparse-keymap))
  (define-key view-model-mode-map " " 'forward-page)
  (define-key view-model-mode-map "f" 'forward-page)
  (define-key view-model-mode-map "DEL" 'backward-page)
  (define-key view-model-mode-map "b" 'backward-page))

(defun view-model-mode (&optional arg)
  "Major mode for viewing models.

If ARG is a negative integer, disable `view-model-mode'; otherwise, enable this mode."
  (interactive "p")
  (kill-all-local-variables)
  (use-local-map view-model-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Invoking a theorem prover or model finder on the (contents of the)
;;; current buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +proof-buffer-name+ "*proof*"
  "The name of the buffer that stores proofs.")

(defun run-prover (prover additional-arguments)
  (interactive (format "sAdditional flags with which %s will be invoked, if any: " prover))
  (save-buffer)
  (let* ((prover-buffer (get-buffer-create +proof-buffer-name+))
	 (tptp-file-absolute-path (buffer-file-name))
	 (tptp-file-directory (file-name-directory tptp-file-absolute-path))
	 (tptp-file-only-filename (file-name-nondirectory tptp-file-absolute-path))
	 (tptp-file-basename (file-name-sans-extension tptp-file-only-filename)))
    (save-excursion
      (switch-to-buffer prover-buffer)

      ;; Kill everything that might already be here
      (erase-buffer)

      ;; Set up buffer-local variables for later use
      (make-local-variable 'proof-directory)
      (make-local-variable 'proof-absolute-path)
      (make-local-variable 'proof-basename)
      (make-local-variable 'proof-prover)
      (setf proof-directory tptp-file-directory
	    proof-absolute-path tptp-file-absolute-path
	    proof-basename tptp-file-basename
	    proof-prover prover)

      ;; Now start inserting content into the buffer
      (insert (format "Calling %s like this:" prover))
      (newline 2)
      (if (empty-string? additional-arguments)
	  (insert "  " prover " < " tptp-file-absolute-path)
	  (insert "  " prover " " additional-arguments " < " tptp-file-absolute-path))
      (newline 2)
      (insert "Results:")
      (newline)
      (insert +report-separator+)
      (newline)
      (if (empty-string? additional-arguments)
	  (call-process prover tptp-file-absolute-path t t)
	  (call-process prover tptp-file-absolute-path t t additional-arguments))
      (setf buffer-read-only t)
      (view-proof-mode 1))))

(defun equinox-current-buffer (additional-equinox-arguments)
  "Invoke the equinox model finder on the current buffer. The filename of
the current buffer will be used as the file argument;
ADDITIONAL-EQUINOX-ARGUMENTS, a string, will be the other arguments
given to equinox.  The filename argument comes last, after
ADDITIONAL-EQUINOX-ARGUMENTS."
  (interactive "sAdditional flags with which equinox will be invoked, if any: ")
  (run-prover *equinox-program* additional-equinox-arguments))

(defun vampire-current-buffer (additional-vampire-arguments)
  "Invoke the Vampire theorem prover on the current buffer. The
filename of the current buffer will be used as the file
argument (to be precise, the standard input of Vampire will be
the file that the current buffer is editing);
ADDITIONAL-VAMPIRE-ARGUMENTS, a string, will be the other
arguments given to Vampire.  The filename argument comes last,
after ADDITIONAL-VAMPIRE-ARGUMENTS."
  (interactive "sAdditional flags with which vampire will be invoked, if any: ")
  (run-prover *vampire-program* additional-vampire-arguments))

(defun eprove-current-buffer (additional-e-arguments)
  "Invoke the E prover on the current buffer.  The filename of
the current buffer will be used as the file argument;
ADDITIONAL-E-ARGUMENTS, a string, will be the other arguments
given to E.  The filename argument comes last, after
ADDITIONAL-E-ARGUMENTS."
  (interactive "sAdditional flags with which E will be invoked, if any: ")
  (run-prover *eprover-program* additional-e-arguments))

(defun paradox-current-buffer (additional-paradox-arguments)
  "Invoke the paradox model finder on the current buffer. The filename of
the current buffer will be used as the file argument;
ADDITIONAL-PARADOX-ARGUMENTS, a string, will be the other arguments
given to paradox.  The filename argument comes last, after
ADDITIONAL-PARADOX-ARGUMENTS."
  (interactive "sAdditional flags with which paradox will be invoked, if any: ")
  (run-prover *paradox-program* additional-paradox-arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TPTP minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tptp-mode-map nil
  "Keymap used by TPTP mode.")

(unless tptp-mode-map
  (setf tptp-mode-map (make-sparse-keymap)))

(defvar tptp-menu
  '(list "TPTP"
	 '("Proofs"
	   ["Run E on the current buffer"
	    (call-interactively 'eprove-current-buffer)
	    t]
	   ["Run Vampire on the current buffer"
	    (call-interactively 'vampire-current-buffer)
	    t])
	 '("Models"
	   ["Run paradox on the current buffer"
	    (call-interactively 'paradox-current-buffer)
	    t])
	 "-"
	 ["Customize TPTP mode" (customize-group 'tptp) t]))

(defun tptp-menu ()
  "Set up a menu for the TPTP minor mode (which is not yet defined)."
  (easy-menu-define tptp-menu-map
                    tptp-mode-map
		    ""
		    (eval tptp-menu)))

(define-minor-mode tptp-mode
  "Work with TPTP files."
  :lighter " TPTP"
  :require nil
  :global nil
  :group 'tptp
  (tptp-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tptp)

;;; tptp.el ends here