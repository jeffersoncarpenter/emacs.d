(defgroup show-enclosing-scope nil
  "Show enclosing scope.")

(defcustom show-enclosing-scope--list-scopes-func nil
  "Returns list of scopes surrounding point, from outermost to innermost.

Format of each scope is (text linenum) where `text' is the text
of the scope and `linenum' is the line number of the scope.

If nil, the default function will be used, which lists enclosing
scopes based on indentation.")

(defvar show-enclosing-scope--window nil)

(setq show-enclosing-scope--scroll t)

(defun show-enclosing-scope--delete-window ()
  "Kill the minimap window."
  (let ((scroll-down-amount 0))
    (when (window-live-p show-enclosing-scope--window)
      (let* ((partner (window-parameter
                       show-enclosing-scope--window 'show-enclosing-scope-partner))
             (margin1 (window-margins partner))
             (margin2 (window-margins show-enclosing-scope--window))
             (fringe1 (window-fringes partner))
             (fringe2 (window-fringes show-enclosing-scope--window))
	     (point-pos (point)))
	(dotimes (i (window-total-height show-enclosing-scope--window))
	  (condition-case nil
	      (when show-enclosing-scope--scroll (scroll-down 1))
	    (error
	     (incf scroll-down-amount))))
	(redisplay)
	(when show-enclosing-scope--scroll (goto-char point-pos))
	(delete-window show-enclosing-scope--window)
	(set-window-margins partner (car margin1) (cdr margin2))
	(set-window-fringes partner (car fringe1) (cadr fringe2))
	(setq show-enclosing-scope--window nil)))
    scroll-down-amount))

(defun show-enclosing-scope--get-buffer ()
  (get-buffer-create "show-enclosing-scope"))

(defun show-enclosing-scope--split-window (size)
  "Make a minimap window."
  ;; make sure that the old one is killed
  (let* ((scroll-down-amount (show-enclosing-scope--delete-window))
	 (basewin (selected-window))
         (margin (window-margins basewin))
         (fringe (window-fringes basewin))
	 (point-pos (point)))
    ;; split new one off
    (condition-case nil
	(when show-enclosing-scope--scroll (scroll-up (- window-size scroll-down-amount)))
      (error nil))
    (redisplay)
    (set-window-margins basewin (car margin) 0)
    (set-window-fringes basewin (car fringe) 0)
    (let ((win (split-window
                basewin (- (+ (or (cdr margin) 0) size)) 'above))
	  (buf (show-enclosing-scope--get-buffer)))
      (set-window-margins win 0 (cdr margin))
      (set-window-fringes win 0 (cadr fringe))
      (setq show-enclosing-scope--window win)
      (set-window-buffer win buf)
      (when show-enclosing-scope--scroll (goto-char point-pos)))))

(defun show-enclosing-scope--default-list-scopes-recurse (prev-column k)
  (unless (or
	   (eq 0 prev-column)
	   (eq 1 (line-number-at-pos)))
    (forward-line -1)
    (back-to-indentation)
    (let ((this-column (current-column)))
      (if (< this-column prev-column)
	  (cons
	   `(,(buffer-substring (point) (line-end-position)) ,(line-number-at-pos))
	   (show-enclosing-scope--default-list-scopes-recurse this-column (1+ k)))
	(show-enclosing-scope--default-list-scopes-recurse prev-column (1+ k))))))

(defun show-enclosing-scope--default-list-scopes ()
  (save-excursion
    (back-to-indentation)
    (show-enclosing-scope--default-list-scopes-recurse (current-column) 0)))

(defun show-enclosing-scope--refresh ()
  (let* ((scopes (show-enclosing-scope--default-list-scopes))
	 (window-size (1+ (length scopes))))
    (if (< 1 window-size)
	(show-enclosing-scope--split-window window-size)
      (show-enclosing-scope--delete-window))
    (with-current-buffer (show-enclosing-scope--get-buffer)
      (erase-buffer)
      (setq mode-line-format nil)
      (mapc
       (lambda (scope)
	 (insert (car scope) "\n"))
       (reverse scopes))
      (insert "-----"))))

;; (add-hook 'pre-command-hook 'show-enclosing-scope--delete-window nil :local)
(add-hook 'post-command-hook 'show-enclosing-scope--refresh nil :local)

;; (remove-hook 'pre-command-hook 'show-enclosing-scope--delete-window)
;; (remove-hook 'post-command-hook 'show-enclosing-scope--refresh)
