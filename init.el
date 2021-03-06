;; base environment changes
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(company-auto-complete t)
 '(company-auto-complete-chars nil)
 '(company-backends
   (quote
	(company-clang company-rtags company-css company-semantic company-nxml company-cmake company-capf company-files
				   (company-dabbrev-code company-gtags company-etags company-keywords)
				   company-oddmuse company-dabbrev)))
 '(company-clang-arguments (quote ("-std=c++11")))
 '(company-idle-delay 0)
 '(custom-enabled-themes (quote (wombat)))
 '(delete-active-region nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-do-not-save-duplicates t)
 '(package-selected-packages
   (quote
	(2048-game markdown-mode go-mode haskell-mode tern-auto-complete tern tabbar sublimity flymake magit js2-mode idris-mode graphviz-dot-mode company clang-format)))
 '(safe-local-variable-values
   (quote
	((c-noise-macro-names "UNINIT" "CALLBACK" "ALIGN_STACK"))))
 '(save-interprogram-paste-before-kill t)
 '(sublimity-disable-smooth-scroll t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline (:color "#660000")))))
 '(whitespace-empty ((t (:background "grey15" :foreground "gray35"))))
 '(whitespace-indentation ((t (:background "gray15" :foreground "gray35"))))
 '(whitespace-newline ((t (:foreground "gray35" :weight normal))))
 '(whitespace-space ((t (:background "grey15" :foreground "gray35"))))
 '(whitespace-tab ((t (:background "grey15" :foreground "gray35"))))
 '(whitespace-trailing ((t (:background "gray15" :foreground "gray35" :weight bold)))))

;; enable package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; take out gui
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; truncate lines
(set-default 'truncate-lines t)

;; disable the obnoxious bell
(setq ring-bell-function 'ignore)

;; don't silently add a newline at the ends of files
(setq require-final-newline nil)

;; use good title format
(setq-default frame-title-format '(:eval (concat "%b (" default-directory ")")))

;; show whitespace, todo: remove show trailing whitespace
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(face tabs spaces trailing space-before-tab indentation empty space-after-tab space-mark tab-mark))
(defun prevent-whitespace-mode-for-magit ()
  (not (derived-mode-p 'magit-mode)))
(defun whitespace-turn-on-if-enabled-2 (&rest args)
  (whitespace-turn-on-if-enabled))
(advice-add 'switch-to-buffer :after 'whitespace-turn-on-if-enabled-2)
(global-whitespace-mode 1)
(add-function :before-while whitespace-enable-predicate 'prevent-whitespace-mode-for-magit)

;; turn off fucking c-x c-b
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b C-b") 'list-buffers-same-window)
(global-set-key (kbd "C-x M-b") 'list-buffers-same-window)

;; disable scratch message
(setq initial-scratch-message "")

;; additional keys for executing extended command
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'eval-expression)

;; change window size
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; additional keys for scrolling a little
(global-set-key "\M-P" (lambda () (interactive) (scroll-down 1)))
(global-set-key "\M-N" (lambda () (interactive) (scroll-up 1)))


(global-set-key (kbd "s-m") 'recompile)
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(global-set-key (kbd "s-n") '(lambda () (interactive) (next-line 4)))
(global-set-key (kbd "s-p") '(lambda () (interactive) (next-line -4)))
(global-set-key (kbd "s-f") '(lambda () (interactive) (forward-char 4)))
(global-set-key (kbd "s-b") '(lambda () (interactive) (forward-char -4)))
(global-set-key (kbd "M-n") '(lambda () (interactive) (next-line 4)))
(global-set-key (kbd "M-p") '(lambda () (interactive) (next-line -4)))

(global-set-key (kbd "C-S-n") '(lambda () (interactive) (next-line 4)))
(global-set-key (kbd "C-S-p") '(lambda () (interactive) (next-line -4)))
(global-set-key (kbd "C-S-f") '(lambda () (interactive) (forward-char 10)))
(global-set-key (kbd "C-S-b") '(lambda () (interactive) (forward-char -10)))

;; enable some modes
(setq column-number-mode t)
(electric-pair-mode)
(show-paren-mode)

(load "~/.emacs.d/star-wars-scroll.el")

;; Crtl-; comments/uncomments current region, or current line if no region
(defun comment-or-uncomment ()
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-;") 'comment-or-uncomment)
(global-set-key (kbd "C-x C-k C-r") 'comment-or-uncomment)


(load "~/.emacs.d/switch-window.el")
(global-set-key (kbd "C-x o") 'switch-window)


;; Ctrl-Enter works like in Visual Studio
(defun insert-newline-before-line ()
  (interactive)
  (progn
    (move-end-of-line 0)
    (newline-and-indent)))
(global-set-key (kbd "C-<return>") 'insert-newline-before-line)

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)

;; enable tramp
(require 'tramp)
(setq tramp-default-user "lol"
      tramp-default-host "71.89.76.184"
      trampvebrose "5")

(defun require-package (feature)
  "Installs feature if not present, then requires it"
  (when (not (featurep feature))
    (package-install feature))
  (require feature))

;; browse kill ring
(load "~/.emacs.d/browse-kill-ring.el")

;; enable graphviz mode
(require-package 'graphviz-dot-mode)

;; enable magit
(require-package 'magit)

;; enable flymake for js

(require-package 'flymake)
(load "~/.emacs.d/flymake-cursor.el")
(load "~/.emacs.d/flymake-node-jshint.el")

(global-set-key (kbd "C-c C-x C-n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c C-x C-p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-c C-x C-c") 'flymake-start-syntax-check)

                                        ; run bashrc.cmd if it exists
(defun setup-shell ()
  "runs C:\bashrc.cmd"
  (let ((filename "c:\\bashrc.cmd"))
    (if (file-exists-p filename)
        (progn
          (insert "c:\\bashrc.cmd")
          (comint-send-input)))))

(setq shell-mode-hook 'setup-shell)



;; major modes


;; clang-format
(require-package 'clang-format)

;; cc-mode
;; based on "gnu" style
(add-hook 'c-mode-hook 'c-mode-stuff)
(defun c-mode-stuff ()
  (company-mode)
  (define-key c-mode-map (kbd "C-, d") 'rtags-find-symbol-at-point)
  (define-key c-mode-map (kbd "C-, C-d") 'rtags-find-symbol-at-point))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook 'c++-mode-stuff)
(defun c++-mode-stuff ()
  (company-mode)
  (define-key c++-mode-map (kbd "C-, d") 'rtags-find-symbol-at-point)
  (define-key c++-mode-map (kbd "C-, C-d") 'rtags-find-symbol-at-point)
  ;;(define-key c++-mode-map (kbd "<C-tab>") 'clang-format-buffer)
  ;;(define-key c++-mode-map (kbd "TAB") 'clang-format-region)
  (define-key c++-mode-map (kbd "C-j b") 'mark-c-scope-beg)
  (define-key c++-mode-map (kbd "C-j f") 'mark-c-scope-end)
  (define-key c++-mode-map (kbd "C-j a") 'mark-c-scope-beg)
  (define-key c++-mode-map (kbd "C-j e") 'mark-c-scope-end)
  ;;(setq indent-tabs-mode nil)
  (subword-mode t)
  (c-add-style "user" '((c-basic-offset . 4)
						(c-comment-only-line-offset 0 . 0)
						(c-hanging-braces-alist
						 (substatement-open before after)
						 (arglist-cont-nonempty))
						(c-offsets-alist
						 (innamespace . 0)
						 (arglist-cont-nonempty . c-lineup-arglist-0)
						 (statement-block-intro . +)
						 (knr-argdecl-intro . 5)
						 (substatement-open . +)
						 (substatement-label . 0)
						 (label . 0)
						 (statement-case-intro . +)
						 (statement-case-open . +)
						 (statement-cont . +)
						 (arglist-intro . +)
						 (arglist-close . 0)
						 (inline-open . 0)
						 (brace-list-intro . +)
						 (brace-list-entry . 0)
						 (brace-list-open . +)
						 (topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont)
						 (inlambda . 0)
						 (defun-block-intro . +)
						 (block-close . 0)
						 (inexpr-statement . 0))
						(c-special-indent-hook . c-gnu-impose-minimum)
						(c-block-comment-prefix . ""))))
(setq c-default-style "user")

(defun c-lineup-arglist-0 (_langelem)
  "Line up the current argument line under the first argument.

As a special case, if the indented line is inside a brace block
construct, the indentation is `c-basic-offset' only.  This is intended
as a \"DWIM\" measure in cases like macros that contains statement
blocks, e.g.:

A_VERY_LONG_MACRO_NAME ({
        some (code, with + long, lines * in[it]);
    });
<--> c-basic-offset

This is motivated partly because it's more in line with how code
blocks are handled, and partly since it approximates the behavior of
earlier CC Mode versions, which due to inaccurate analysis tended to
indent such cases this way.

Works with: arglist-cont-nonempty, arglist-close."
  (save-excursion
    (let ((indent-pos (point)))

      (if (c-block-in-arglist-dwim (c-langelem-2nd-pos c-syntactic-element))
		  0		; DWIM case.

		;; Normal case.  Indent to the token after the arglist open paren.
		(goto-char (c-langelem-2nd-pos c-syntactic-element))
		(if (and c-special-brace-lists
				 (c-looking-at-special-brace-list))
			;; Skip a special brace list opener like "({".
			(progn (c-forward-token-2)
				   (forward-char))
		  (forward-char))
		(let ((arglist-content-start (point)))
		  (c-forward-syntactic-ws)
		  (when (< (point) indent-pos)
			(goto-char arglist-content-start)
			(skip-chars-forward " \t"))
		  (vector (current-column)))))))

;; https://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
;; (defadvice c-lineup-arglist (around my activate)
;;   "Improve indentation of continued C++11 lambda function opened as argument."
;;   (setq ad-return-value
;; 		(if (and (equal major-mode 'c++-mode)
;; 				 (ignore-errors
;; 				   (save-excursion
;; 					 (goto-char (c-langelem-pos langelem))
;; 					 ;; Detect "[...](" or "[...]{". preceded by "," or "(",
;; 					 ;;   and with unclosed brace.
;; 					 (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
;; 			0                           ; no additional indent
;; 		            ad-do-it)))                   ; default behavior

(defun mark-c-scope-beg ()
  "Marks the c-scope (region between {}) enclosing the point. 
   Naive, as will be confused by { } within strings"
  (interactive)
  (let
	  ((scope-depth 1))
	(while (not (= scope-depth 0))
	  (search-backward-regexp "}\\|{")
	  (if (string= (char-to-string (char-after)) "}")
		  (setq scope-depth (1+ scope-depth))
		(setq scope-depth (1- scope-depth)))))
  (point))

(defun mark-c-scope-end ()
  "Marks the c-scope (region between {}) enclosing the point. 
   Naive, as will be confused by { } within strings"
  (interactive)
  (let
	  ((scope-depth 1))
	(while (not (= scope-depth 0))
	  (search-forward-regexp "}\\|{")
	  (if (string= (char-to-string (char-before)) "}")
		  (setq scope-depth (1- scope-depth))
		(setq scope-depth (1+ scope-depth)))))
  (point))

;; html2-mode
(load "~/.emacs.d/html2-mode.el")

;; web-mode
(load "~/.emacs.d/web-mode.el")
(setq web-mode-enable-auto-quoting nil)
(add-to-list 'auto-mode-alist '("\\.hjs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; php mode (eewwww)
(load "~/.emacs.d/php-mode.el")




;; minor modes


;; tern-mode
(require-package 'js2-mode)
(require-package 'tern)
(require-package 'tern-auto-complete)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-mode-show-strict-warnings nil)
(add-hook 'js2-mode-hook (lambda ()
                           ;;(tern-mode t)
                           ;;(flymake-find-file-hook) ; what if we're in scratch buffer
                           (subword-mode t)
                           (setq indent-tabs-mode nil)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; csharp mode
(add-hook 'csharp-mode-hook
          (lambda ()
            (c-set-style "c#")
            (define-key csharp-mode-map (kbd "{") nil)
            (define-key csharp-mode-map (kbd "}") nil)
            (define-key csharp-mode-map (kbd ",") nil)))



;; flymake color

(require 'org-install)



(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(add-to-list 'load-path  "~/p/idris-mode")
(require-package 'idris-mode)
(define-key idris-mode-map (kbd "C-c C-SPC") nil)


;; (require 'shm)

;; rtags stuff
(require-package 'company) ; ensure company is installed
(load "~/.emacs.d/rtags.el")
(load "~/.emacs.d/company-rtags.el")
(setq rtags-autostart-diagnostics t)
(setq rtags-completions-enabled t)


;; good stuff
(defun line-beginning (n)
  "Takes a buffer position n.  Returns the character position of
  the first character of that line."
  (save-excursion
    (goto-char n)
    (line-beginning-position)))

(defun line-end (n)
  "Takes a buffer position n.  Returns the character position of
  the last character of that line."
  (save-excursion
    (goto-char n)
    (line-end-position)))



;; C# and JavaScript shit


(defun find-namespace (word)
  "Returns the namespace of a symbol"
  (with-current-buffer (find-tag-noselect word)
    (save-excursion
      (beginning-of-buffer)
      (search-forward "namespace")
      (forward-char) ; go past space
      (buffer-substring (point) (line-end-position)))))


(defun add-using (symbol)
  "Adds a using directive to the top of the file"
  (let ((using (concat "using " (find-namespace symbol) ";")))
    (save-excursion
      (beginning-of-buffer)
      (if (not (search-forward using (point-max) t))
          (progn
            (beginning-of-buffer)
            (insert using "\n"))))))

(defun add-using-for-word-at-point ()
  "Adds using for word at point"
  (interactive)
  (add-using (word-at-point)))

(global-set-key (kbd "C-. u") 'add-using-for-word-at-point)


(defun requirejs-jump-to-require ()
  "Moves point to the beginning of the 'require' statement for current function"
  (interactive)
  (or
   (search-backward "require(" nil t)
   (search-backward "define(" nil t)))


(defun index-of (el list &optional index)
  "Returns the index of el within list"
  (let ((n (if index index 0)))
    (if (equal el (car list))
        n
      (let ((rest (cdr list)))
        (if rest
            (index-of el (cdr list) (+ n 1)))))))


(defun requirejs-dependencies ()
  "Return list of requirejs dependencies brought in to current
    require or define statement."
  (interactive)
  (save-excursion
    (requirejs-jump-to-require)
    (search-forward "(")
    (let* ((beginning-of-args (search-forward "("))
           (end-of-args (- (search-forward ")") 1))
           (args-string (buffer-substring beginning-of-args end-of-args))
           (args-with-shit (split-string args-string ",")))
      (remove "" (mapcar (lambda (str) (replace-regexp-in-string "['\n[:space:]]" "" str)) args-with-shit)))))


(defun requirejs-paths ()
  "Return list of paths brought in to current require or define
  statement."
  (interactive)
  (save-excursion
    (requirejs-jump-to-require)
    (let* ((beginning-of-paths (search-forward "["))
           (end-of-paths (- (search-forward "]") 1))
           (paths-string (buffer-substring beginning-of-paths end-of-paths))
           (paths-with-shit (split-string paths-string ",")))
      (remove "" (mapcar (lambda (str) (replace-regexp-in-string "['\n[:space:]]" "" str)) paths-with-shit)))))


(defun requirejs-add-dependency ()
  "Add a dependency.  Assume we are inside a RequireJS require or
  define statement."
  (interactive)
  (let ((word (word-at-point)))
    (save-excursion
      (requirejs-jump-to-require)
      (let ((dependencies (requirejs-dependencies))
            (paths (requirejs-paths)))
        (if (not (index-of word dependencies))
            (let ((index (index-of word (sort (cons word dependencies) 'string<))))
              (requirejs-jump-to-require)
              ;; add to array
              (search-forward "([")
              (dotimes (i index) (search-forward ","))
              (insert "\n'" word "',")
              (indent-for-tab-command)
              (if (string= "]" (string (char-after (point))))
                  (insert "\n"))
              ;; add to function arguments
              (if (equal (char-after (point)) "]")
                  (insert "\n"))
              (search-forward "(")
              (dotimes (i index) (re-search-forward "[,)]"))
              (if (not (string= (string (char-before (point))) "("))
                  (progn
                    (backward-char)
                    (insert ", " word))
                (insert word)
                (if (not (string= (string (char-after (point))) ")"))
                    (insert ", "))))
          (message "dependency already exists"))))))

(defun requirejs-find-relative-path ()
  "Return relative path of dependency under point.  Assume we are
inside a RequireJS require or define statement."
  (interactive)
  (let ((word (word-at-point)))
    (save-excursion
      (requirejs-jump-to-require)
      (let ((index (index-of word (requirejs-dependencies))))
        (if index
            (nth index (requirejs-paths))
          (message "Unable to find file"))))))

(defun parent-directory (dir)
  (unless (equal "c:/" dir)
    (file-name-directory (directory-file-name dir))))

(defun requirejs-go-to-definition (&optional root)
  "Opens file corresponding to required thing under point"
  (interactive)
  (let* ((root (if root root (file-name-directory (buffer-file-name))))
         (rel-path (requirejs-find-relative-path))
         (full-path (concat root rel-path ".js")))
    (if (file-exists-p full-path)
        (find-file full-path)
      (let ((parent (parent-directory root)))
        (if parent
            (requirejs-go-to-definition parent))))))


;;(global-set-key (kbd "C-, d") 'requirejs-go-to-definition)
;;(global-set-key (kbd "C-, t") (lambda () (interactive) (requirejs-go-to-definition)))
;;(global-set-key (kbd "C-, u") 'requirejs-add-dependency)
;;(global-set-key (kbd "C-x C-b d") 'requirejs-go-to-definition)
;;(global-set-key (kbd "C-x C-b t") (lambda () (interactive) (requirejs-go-to-definition)))
;;(global-set-key (kbd "C-x C-b u") 'requirejs-add-dependency)


(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (if (= oldpos (point))
        (back-to-indentation))))


(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)
(define-key global-map "\C-j" nil)


(defun forward-delete-word (arg)
  "Delete characters forward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key "\M-d" 'forward-delete-word)


(defun list-buffers-same-window (&optional arg)
  "Display a list of existing buffers.
The list is displayed in a buffer named \"*Buffer List*\".
See `buffer-menu' for a description of the Buffer Menu.

By default, all buffers are listed except those whose names start
with a space (which are for internal use).  With prefix argument
ARG, show only buffers that are visiting files."
  (interactive "P")
  (switch-to-buffer (list-buffers-noselect arg)))


(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(setq-default c-basic-offset 4
              js2-basic-offset 4
              indent-tabs-mode t)

(defalias 'rs 'replace-string)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rb 'revert-buffer)


(defun hs-indent ()
  "Indent stuff in haskell"
  (interactive)
  (align-regexp
   (line-beginning (region-beginning))
   (line-end (region-end))
   "\\(\\s-*\\)\\( :\\|->\\).*$"))

(global-set-key (kbd "C-c C-SPC C-TAB") 'hs-indent)


;; open shell in same window
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

;; break company-clang
;; (defun company-clang--candidates (prefix callback)
;;   (and (company-clang--auto-save-p)
;;        (buffer-modified-p)
;;        (basic-save-buffer))
;;   (when (null company-clang--prefix)
;;     (company-clang-set-prefix (or (funcall company-clang-prefix-guesser)
;;                                   'none)))
;;   (apply 'company-clang--start-process
;;          prefix
;;          callback
;;          (company-clang--build-complete-args (point))))

(require-package 'sublimity)
;;(require 'sublimity-scroll)
(require 'sublimity-map)

(require-package 'tabbar)

(define-minor-mode display-enclosing-scopes-mode
  "Toggle display enclosing scopes mode."
  
  )

(require-package 'haskell-mode)
(require-package 'go-mode)
(require-package 'markdown-mode)


;; 
(defun js--indent-in-array-comp (bracket)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((end (point)))
    (save-excursion
      (goto-char bracket)
      (when (looking-at "\\[")
        (forward-char 1)
        (js--forward-syntactic-ws)
        (if (looking-at "[[{]")
            (let (forward-sexp-function) ; Use Lisp version.
              (forward-sexp)             ; Skip destructuring form.
              (js--forward-syntactic-ws)
              (if (and (/= (char-after) ?,) ; Regular array.
                       (looking-at "for"))
                  (match-beginning 0)))
          ;; To skip arbitrary expressions we need the parser,
          ;; so we'll just guess at it.
          (if (and (> end (point)) ; Not empty literal.
                   (re-search-forward "[^,]]* \\(for\\) " end t)
                   ;; Not inside comment or string literal.
                   (not (nth 8 (parse-partial-sexp bracket (point))))
                   (eq
                    (nth 0 (parse-partial-sexp bracket (point)))
                    (1+ (nth 0 (parse-partial-sexp bracket bracket)))))
              (match-beginning 1)))))))

(defun js2-indent-in-array-comp (parse-status)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((bracket (nth 1 parse-status))
        (end (point)))
    (when bracket
      (save-excursion
        (goto-char bracket)
        (when (looking-at "\\[")
          (forward-char 1)
          (js2-forward-sws)
          (if (looking-at "[[{]")
              (let (forward-sexp-function) ; use Lisp version
                (forward-sexp)             ; skip destructuring form
                (js2-forward-sws)
                (if (and (/= (char-after) ?,) ; regular array
                         (looking-at "for"))
                    (match-beginning 0)))
            ;; to skip arbitrary expressions we need the parser,
            ;; so we'll just guess at it.
            (if (and (> end (point)) ; not empty literal
                     (re-search-forward "[^,]]* \\(for\\) " end t)
                     ;; not inside comment or string literal
                     (let ((bracket-state (parse-partial-sexp bracket bracket))
                           (state (parse-partial-sexp bracket (point))))
                       (and
                        (not (nth 3 state))
                        (not (nth 4 state))
                        (eq (nth 0 state)
                            (1+ (nth 0 state))))))
                (match-beginning 1))))))))

(define-key with-editor-mode-map [remap kill-buffer] nil)
(define-key magit-mode-map (kbd "e") nil)
(global-set-key (kbd "C-x C-b C-m") 'magit-status)
