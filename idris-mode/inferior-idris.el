;;; inferior-idris.el --- Run an Idris interpreter using S-Expression communication protocol

;; Copyright (C) 2013 Hannes Mehnert

;; Author: Hannes Mehnert <hannes@mehnert.org>

;; License:
;; Inspiration is taken from SLIME/DIME (http://common-lisp.net/project/slime/) (https://github.com/dylan-lang/dylan-mode)
;; Therefore license is GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'idris-core)
(require 'idris-settings)
(require 'idris-common-utils)
(require 'pp)
(require 'cl-lib)
(require 'idris-events)
(require 'idris-log)
(require 'idris-warnings)
(require 'idris-prover)

;;; Words of encouragement - strongly inspired by Slime
(defun idris-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))


(defvar idris-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    ,(format "%s, this could be the start of a beautiful program."
             (idris-user-first-name))
    ,(format "%s, this could be the start of a beautiful proof."
             (idris-user-first-name))
    "The terms have seized control of the means of computation - a glorious future awaits!"
    "Ship if it typechecks!"
    "Do you know 'Land of My Fathers'?"))

(defun idris-random-words-of-encouragement ()
  "Return a random string of encouragement"
  (nth (random (length idris-words-of-encouragement))
       idris-words-of-encouragement))

;;; Process stuff
(defvar idris-process nil
  "The Idris process.")

(defvar idris-process-current-working-directory ""
  "Working directory of Idris process")

(defvar-local idris-packages nil
  "The list of packages to be loaded by Idris. Set using file or directory variables.")

(defvar idris-currently-loaded-packages nil
  "The list of packages actually loaded by Idris. This is
  maintained to restart Idris when the package list changes.")

(defun idris-run ()
  "Run an inferior Idris process"
  (interactive)

  ;; Kill Idris if the package list needs updating
  (when (not (equal idris-packages idris-currently-loaded-packages))
    (message "Idris package list updated, restarting Idris")
    (idris-quit)
    (sit-for 0.01)) ; allows the sentinel to run and reset idris-process

  ;; Start Idris if necessary
  (when (not idris-process)
    (setq idris-process
          (apply #'start-process "idris" (idris-buffer-name :process)
                 idris-interpreter-path
                 "--ideslave"
                 (append (cl-loop for p in idris-packages collecting "-p" collecting p)
                         idris-interpreter-flags)))
    (set-process-filter idris-process 'idris-output-filter)
    (set-process-sentinel idris-process 'idris-sentinel)
    (set-process-query-on-exit-flag idris-process t)
    (setq idris-process-current-working-directory "")
    (add-hook 'idris-event-hooks 'idris-log-hook-function)
    (add-hook 'idris-event-hooks 'idris-warning-event-hook-function)
    (add-hook 'idris-event-hooks 'idris-prover-event-hook-function)
    (setq idris-currently-loaded-packages idris-packages)
    (message "Connected. %s" (idris-random-words-of-encouragement))))

(defun idris-sentinel (process msg)
  (message "Idris quit unexpectly: %s" (substring msg 0 -1))
  (delete-process idris-process)
  (setq idris-process nil))

(defun idris-output-filter (process string)
  "Accept output from the socket and process all complete messages"
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (idris-process-available-input process))

(defun idris-process-available-input (process)
  "Process all complete messages which arrived from Idris."
  (with-current-buffer (process-buffer process)
    (while (idris-have-input-p)
      (let ((event (idris-receive)))
        (idris-event-log event nil)
        (unwind-protect
            (save-current-buffer
              (idris-dispatch-event event process)))))))

(defun idris-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (idris-decode-length))))

(defun idris-receive ()
  "Read a message from the idris process"
  (goto-char (point-min))
  (let* ((length (idris-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (cl-assert (plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (current-buffer)))
      (delete-region (point-min) end))))

(defun idris-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun idris-send (sexp proc)
  "Send a SEXP to Idris over the PROC. This is the lowest level of communication."
  (let* ((msg (concat (idris-prin1-to-string sexp) "\n"))
         (string (concat (idris-encode-length (length msg)) msg)))
    (idris-event-log sexp t)
    (process-send-string proc string)))

(defun idris-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun idris-prin1-to-string (sexp)
  "Like `prin1-to-string', but don't octal-escape non-ascii characters."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(defvar idris-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(defvar idris-continuation-counter 1
  "Continuation serial number counter.")

(defvar idris-event-hooks)

(defun idris-dispatch-event (event process)
  (or (run-hook-with-args-until-success 'idris-event-hooks event)
      (destructure-case event
        ((:emacs-rex form continuation)
         (let ((id (incf idris-continuation-counter)))
           (idris-send `(,form ,id) process)
           (push (cons id continuation) idris-rex-continuations)))
        ((:return value id)
         (let ((rec (assq id idris-rex-continuations)))
           (cond (rec (setf idris-rex-continuations
                            (remove rec idris-rex-continuations))
                      (funcall (cdr rec) value))
                 (t (error "Unexpected reply: %S %S" id value))))))))

(cl-defmacro idris-rex ((&rest saved-vars) (sexp) &rest continuations)
  "(idris-rex (VAR ...) (SEXP) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Idris.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:error CONDITION).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (let ((result (cl-gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (idris-dispatch-event
        (list :emacs-rex ,sexp
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations))) idris-process))))

(defun idris-eval-async (sexp cont &optional failure-cont)
  "Evaluate EXPR on the superior Idris and call CONT with the result, or FAILURE-CONT in failure case."
  (idris-rex (cont (buffer (current-buffer)) failure-cont)
      (sexp)
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:error condition)
     (when failure-cont
       (set-buffer buffer)
       (funcall failure-cont condition))
     (message "Evaluation returned an error: %s." condition))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar idris-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun idris-eval (sexp)
  "Evaluate EXPR on the inferior Idris and return the result."
  (let* ((tag (cl-gensym (format "idris-result-%d-"
                              (1+ idris-continuation-counter))))
	 (idris-stack-eval-tags (cons tag idris-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (idris-rex (tag sexp)
           (sexp)
         ((:ok value &optional spans)
          (unless (member tag idris-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity (cons value spans))))
         ((:error condition)
          (when (member 'warnings-tree idris-warnings-printing)
            (when (idris-list-compiler-notes)
              (pop-to-buffer (idris-buffer-name :notes))))
          (throw tag (list #'error "%s (synchronous Idris evaluation failed)" condition))))
       (let ((debug-on-quit t)
             (inhibit-quit nil))
         (while t
           (when (eq (process-status idris-process) 'exit)
             (error "Idris process exited unexpectedly"))
           (accept-process-output idris-process 0.1)))))))


(provide 'inferior-idris)
