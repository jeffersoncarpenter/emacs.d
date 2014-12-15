; base environment changes

; take out gui
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; truncate lines
(set-default 'truncate-lines t)

; disable the obnoxious bell
(setq ring-bell-function 'ignore)

; don't silently add a newline at the ends of files
(setq require-final-newline nil)

(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
			 (setq tab-width 4)
             (setq indent-line-function (quote insert-tab))))

(setq tab-width 4)
(setq-default tab-width 4)

; base environment additions


; additional keys for executing extended command
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)


; additional keys for scrolling a little
(global-set-key "\M-P" (lambda () (interactive) (scroll-down 1)))
(global-set-key "\M-N" (lambda () (interactive) (scroll-up 1)))


; show column
(setq column-number-mode t)


; Crtl-; comments/uncomments current region, or current line if no region
(defun comment-or-uncomment ()
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-;") 'comment-or-uncomment)
(global-set-key (kbd "C-x C-k C-r") 'comment-or-uncomment)


; back-window
(defun back-window () (interactive) (other-window -1))
(global-set-key (kbd "C-x O") 'back-window)


; Ctrl-Enter works like in Visual Studio
(defun insert-newline-before-line ()
  (interactive)
  (progn
    (move-end-of-line 0)
    (newline-and-indent)))
(global-set-key (kbd "C-<return>") 'insert-newline-before-line)


; set theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(inhibit-startup-screen t))


; enable package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
			 '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


; enable magit
(require 'magit)


; run bashrc.cmd if it exists
(defun setup-shell ()
  "runs C:\bashrc.cmd"
  (let ((filename "c:\\bashrc.cmd"))
    (if (file-exists-p filename)
        (progn
          (insert "c:\\bashrc.cmd")
          (comint-send-input)))))

(setq shell-mode-hook 'setup-shell)


; major modes

; html2-mode
(load "~/.emacs.d/html2-mode.el")

; web-mode
(load "~/.emacs.d/web-mode.el")
(setq web-mode-enable-auto-quoting nil)

; csharp mode
(load "~/.emacs.d/csharp-mode.el")
(flymake-mode)
;(add-hook 'csharp-mode-hook
;	  '(setq-default c-basic-offset 4))
(add-hook 'csharp-mode-hook
	  (lambda ()
            (c-set-style "c#")
            (define-key csharp-mode-map (kbd "{") nil)
            (define-key csharp-mode-map (kbd "}") nil)
            (define-key csharp-mode-map (kbd ",") nil)))




(require 'org-install)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(put 'upcase-region 'disabled nil)


(add-to-list 'load-path  "~/.emacs.d/idris-mode/")
(require 'idris-mode)





; C# and JavaScript shit


(defun find-namespace (word)
  "Returns the namespace of a symbol"
  (with-current-buffer (find-tag-noselect word)
    (save-excursion
      (beginning-of-buffer)
      (search-forward "namespace")
      (forward-char) ; go past space
      (buffer-substring (point) (line-end-position))
      )))
  
(defun add-using (symbol)
  "Adds a using directive to the top of the file"
  (let ((using (concat "using " (find-namespace symbol) ";")))
    (save-excursion
      (beginning-of-buffer)
      (if (not (search-forward using (point-max) t))
          (progn
            (beginning-of-buffer)
            (insert using "\n")
            )))))

(defun add-using-for-word-at-point ()
  "Adds using for word at point"
  (interactive)
  (add-using (word-at-point)))

(global-set-key (kbd "C-. u") 'add-using-for-word-at-point)


(defun fix-lp-file ()
  "Edits references in .linq file to move Solomo.Common projects to the correct place"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((project-prefix "Solomo.Common")
          (old-ref-string "<Reference Relative=\"..\\Solomo.Common")
          (new-ref-string "<Reference Relative=\"..\\Common\\Solomo.Common"))
      (while (search-forward old-ref-string nil t)

        ; replace reference
        (delete-backward-char (length old-ref-string))
        (insert new-ref-string)

        ; replace metadata
        (search-forward ">") ; get past reference relative
        (search-forward project-prefix)
        (backward-char (length project-prefix))
        (insert "Common\\")))))


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


(defun requirejs-find-relative-path ()
  "Assumes we are inside a RequireJS require or define statement.
Returns relative path of required thing under point"
  (interactive)
  (let ((word (word-at-point)))
    (save-excursion
      (requirejs-jump-to-require)
      (let* (
             ;; get paths
             (beginning-of-paths (search-forward "["))
             (end-of-paths (- (search-forward "]") 1))
             (paths-string (buffer-substring beginning-of-paths end-of-paths))
             (paths-with-shit (split-string paths-string ","))
             (paths (mapcar (lambda (str) (replace-regexp-in-string "['\n ]" "" str)) paths-with-shit))

             ;; get args
             (beginning-of-args (search-forward "("))
             (end-of-args (- (search-forward ")") 1))
             (args-string (buffer-substring beginning-of-args end-of-args))
             (args-with-shit (split-string args-string ","))
             (args (mapcar (lambda (str) (replace-regexp-in-string "['\n ]" "" str)) args-with-shit))
             
             ;; find path of word
             (index (index-of word args)))
        (if index
            (nth index paths)
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
    (message full-path)
    (if (file-exists-p full-path)
        (find-file full-path)
      (let ((parent (parent-directory root)))
        (if parent
            (requirejs-go-to-definition parent))))))


(global-set-key (kbd "C-, d") 'requirejs-go-to-definition)
(global-set-key (kbd "C-, t") (lambda () (interactive) (requirejs-go-to-definition)))


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

(setq tab-width 4)


(defun list-buffers-same-window (&optional arg)
  "Display a list of existing buffers.
The list is displayed in a buffer named \"*Buffer List*\".
See `buffer-menu' for a description of the Buffer Menu.

By default, all buffers are listed except those whose names start
with a space (which are for internal use).  With prefix argument
ARG, show only buffers that are visiting files."
  (interactive "P")
  (switch-to-buffer (list-buffers-noselect arg)))

(global-set-key (kbd "C-x C-b") 'list-buffers-same-window)


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
