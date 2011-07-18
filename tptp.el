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
  "========================================================================================================================"
  "The separator to use when generating reports.")

(defconst +proof-buffer-name+ "*proof*"
  "The name of the buffer that stores proofs.")

;;; The next four variables will be made buffer local later on.

(defvar proof-absolute-path nil
  "The absolute path of the file for which we have a deduction.")

(defvar proof-prover nil
  "The prover that was used to obtain a proof.")

(defvar proof-time nil
  "The time at which we found a proof.")

(defvar proof-text nil
  "The text of the TPTP file for which we've found a proof.")

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

(defcustom *mace4-program* "mace4"
  "The MACE4 model finder program.

That value of this variable can be either a path or a
program name.  If it is not an absolute path, your PATH
environment variable will be consulted to determine where
MACE4 can be found."
  :tag "MACE4 program"
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

  ; Saving proofs
  (define-key view-proof-mode-map "s" 'view-proof-save-deduction)
  (define-key view-proof-mode-map "S" 'view-proof-save-deduction)

  ; Help
  (define-key view-proof-mode-map "h" 'describe-mode)
  (define-key view-proof-mode-map "H" 'describe-mode)
  (define-key view-proof-mode-map "?" 'describe-mode))

(defconst +vampire-negated-conjecture-marker+
  "[negated conjecture]"
  "A marker used by vampire to indicate the negation of a conjecture.")

(defconst +vampire-assumption-marker+
  "[input]"
  "A marker used by vampire to indicate an input assumption.")

(defun mark-up-vampire-proof ()
  "Mark up a Vampire deduction."
  ;; find the negated conjecture, and mark it up
  (save-excursion
    (goto-char (point-min))
    (search-forward +report-separator+ nil t 2) ;; find two record separators
    (let ((neg-conj-pos (search-forward +vampire-negated-conjecture-marker+ nil t)))
      (when neg-conj-pos
	(let ((neg-conj-begin (- neg-conj-pos
				 (length +vampire-negated-conjecture-marker+))))
	  (beginning-of-line)
	  (re-search-forward "^[0-9]+\. ") ;; vampire proof lines look like this
	  (put-text-property (point) neg-conj-begin
			     'font-lock-face 'cursor)))))
  (save-excursion
    (goto-char (point-min))
    (let ((assumption-pos (search-forward +vampire-assumption-marker+ nil t)))
      (while assumption-pos
	(let ((assumption-marker-begin (- assumption-pos
					  (length +vampire-assumption-marker+))))
	  (beginning-of-line)
	  (re-search-forward "^[0-9]+\. ")
	  (put-text-property (point) assumption-marker-begin
			     'font-lock-face 'custom-changed-face)
	  (end-of-line)
	  (setf assumption-pos
		(search-forward +vampire-assumption-marker+ nil t)))))))

(defun view-proof-mode (&optional prover)
  "Major mode for viewing proofs generated by PROVER."
  (interactive "p")
  (kill-all-local-variables)
  (use-local-map view-proof-mode-map)
  (setf major-mode 'view-proof-mode
	mode-name (format "View-Proof[%s]" prover))
  (when (string= prover "vampire")
    (mark-up-vampire-proof)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Saving deductions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-proof-save-deduction ()
  "Save the results of the current proof to a file."

  (interactive)

  ;; sanity checks: we are in the proof buffer
  (unless (string= (buffer-name) +proof-buffer-name+)
    (error "We are not in the proof buffer; saving deductions doesn't make sense."))
  (let* ((proof-buffer (current-buffer))
	 (proof-prover (buffer-local-value 'proof-prover proof-buffer))
	 (proof-absolute-path (buffer-local-value 'proof-absolute-path proof-buffer))
	 (proof-time (buffer-local-value 'proof-time proof-buffer))
	 (proof-text (buffer-local-value 'proof-text proof-buffer)))

    ;; sanity checks: we know the file and prover whose output the
    ;; proof buffer is storing, the text of that file, and the time
    ;; when the deduction was found
    (unless proof-prover
      (error "The proof buffer somehow does not know the prover whose output it records."))
    (unless proof-absolute-path
      (error "The proof buffer somehow does not know the absolute path of the file on which %s was invoked" proof-prover))
    (unless proof-time
      (error "The proof buffer somehow does not know the time at which %s was invoked" proof-prover))
    (unless proof-text
      (error "The proof buffer somehow does not know the text on which %s was invoked" proof-prover))

  ;; we know we have sensible values; we can continue

  (let* ((tptp-directory (file-name-directory proof-absolute-path))
	 (tptp-only-filename (file-name-nondirectory proof-absolute-path))
	 (tptp-basename (file-name-sans-extension tptp-only-filename))
	 (commented-out-text (format "%% %s"
				     (replace-regexp-in-string "[\n]"
							       "\n% "
							       proof-text))))
    (destructuring-bind (seconds minutes hour day month year dow dst zone)
	(decode-time proof-time)
      ;; ignore SECONDS, DOW, DST, ZONE
      (let ((deduction-path (format "%s%s-%s-%04d-%02d-%02d-%02d-%02d"
				    (file-name-as-directory tptp-directory)
				    tptp-basename
				    proof-prover
				    year
				    month
				    day
				    hour
				    minutes))
	    (should-we-write? nil))
	(if (file-exists-p deduction-path)
	    (if (yes-or-no-p (format "There is already at proof saved under '%s'; overwrite it? " deduction-path))
		(if (file-writable-p deduction-path)
		    (setf should-we-write? t)       
		  (error "We cannot write to %s" deduction-path))
	      (message "OK, refusing to overwrite '%s'" deduction-path))
	  (setf should-we-write? t))
	(when should-we-write?
	  (let ((saved-deduction-buf (find-file-noselect deduction-path))
		(deduction (buffer-substring-no-properties (point-min)
							   (point-max))))
	    (save-excursion
	      (switch-to-buffer saved-deduction-buf)
	      (erase-buffer) ;; there might have been stuff there before
	      (insert deduction)
	      (save-buffer nil))
	    (kill-buffer saved-deduction-buf))
	  (message "Proof to %s" deduction-path)))))))

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

(defun run-prover (prover additional-arguments)
  (interactive (format "sAdditional flags with which %s will be invoked, if any: " prover))
  (save-buffer)
  (let* ((prover-buffer (get-buffer-create +proof-buffer-name+))
	 (tptp-file-absolute-path (buffer-file-name))
	 (text (buffer-substring-no-properties (point-min) (point-max)))
	 (commented-out-text (format "%% %s"
				     (replace-regexp-in-string "[\n]"
							       "\n%"
							       text))))
    (switch-to-buffer prover-buffer)

    ;; Kill everything that might already be here
    (erase-buffer)
    
    ;; Now start inserting content into the buffer
    (insert (format "%% Invoking %s on" prover)) (newline)
    (insert "%") (newline)
    (insert "%") (newline)
    (insert "%   " tptp-file-absolute-path) (newline)
    (insert "%") (newline)
    (insert "%") (newline)
    (insert "% at " (current-time-string) " with the theory") (newline)
    (insert "%") (newline)
    (insert +report-separator+) (newline)
    (insert commented-out-text) (newline)
    (insert +report-separator+) (newline)
    (if (empty-string? additional-arguments)
	(call-process prover tptp-file-absolute-path t t)
      (call-process prover tptp-file-absolute-path t t additional-arguments))
    (view-proof-mode prover)
    (setf buffer-read-only t)

    ;; Set up buffer-local variables for later use
    (set (make-local-variable 'proof-absolute-path) tptp-file-absolute-path)
    (set (make-local-variable 'proof-prover) prover)
    (set (make-local-variable 'proof-time) (current-time))
    (set (make-local-variable 'proof-text) text)))

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

(defun mace4-current-buffer (additional-mace-arguments)
  "Invoke the MACE4 model finder on the current buffer. The filename of
the current buffer will be used as the file argument;
ADDITIONAL-MACE-ARGUMENTS, a string, will be the other arguments
given to MACE4.  The filename argument comes last, after
ADDITIONAL-MACE4-ARGUMENTS."
  (interactive "sAdditional flags with which MACE4 will be invoked, if any: ")
  (run-prover *mace4-program* additional-mace-arguments))

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
	    t]
	   ["Run Equinox on the current buffer"
	    (call-interactively 'equinox-current-buffer)
	    t])
	 '("Models"
	   ["Run paradox on the current buffer"
	    (call-interactively 'paradox-current-buffer)
	    t]
	   ["Run MACE4 on the current buffer"
	    (call-interactively 'mace4-current-buffer)
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