; todo: figure out how to use the built-in minor mode mode defining thing, then define sln-mode

(defun sln-mode-on (sln-file-path)
  "Turns on sln-mode!"
  (setq sln-directory (file-name-directory sln-file-path))
  (setq sln-projects ())
  (sln-process-solution-file sln-file-path))



(defun sln-process-solution-file (solution-file-path)
  "Parses the solution file and takes all due actions"
  (with-temp-buffer
    (insert-file-contents solution-file-path)

    ;; find all of the projects in the solution
    (while (search-forward "Project(")

      ;; search forward to obtain project name
      (setq sln-project (buffer-substring
			 (search-forward "= \"")
			 (- (search-forward "\"") 1)))

      ;; search forward to obtain project path
      (setq sln-project-rel-path (buffer-substring
				    (search-forward ", \"")
				    (- (search-forward "\"") 1)))
      (setq sln-project-path (concat sln-directory sln-project-rel-path))

      (if (string-match "[a-zA-Z/]*c[cs]proj" sln-project-path)
	  (let ((project (sln-process-project sln-project sln-project-path)))
	    (if project
		(add-to-list 'sln-projects project)))))))

;    (setq sln-projects (sort sln-projects 'string<))
    ;; process project files



(defun insert-create-overlay (string)
  "inserts argument into buffer, creates and returns overlay"
  (let ((beginning (point)))
    (insert string)
    (make-overlay beginning (point))))


(defun string-ends-with (string ending)
  (let ((string-length (length string))
	(ending-length (length ending)))
    (string= ending (substring string (- string-length ending-length)))))

(defun sln-process-project (project-name project-path)
  "Parses a project file and takes all due actions"
  (if (string-ends-with project-path ".csproj")
      (sln-process-csproj-file project-name project-path)))


(defun sln-project-name (project)
  (nth 0 project))

(defun sln-project-path (project)
  (nth 1 project))

(defun sln-project-files (project)
  (nth 2 project))


(defun sln-process-csproj-file (project-name project-path)
  "Parses a csproj file and returns a representation"
  (let ((project-files ()))
    (with-temp-buffer
      (insert-file-contents project-path)
      (while (search-forward-regexp "(<Compile Include=\")|(<Content Include=\")" (point-max) t)
	(let ((file-rel-path (buffer-substring (point)
					       (search-forward "\"")))))
	(add-to-list 'project-files file-rel-path)))
    `(,project-name ,project-path ,project-files)))


(define-minor-mode sln-browse-mode
  "Toggle sln-browse-mode"
  :init-value nil
  :lighter " sln-browse"
  :keymap
  (keymap
   (9 . sln-browse-expand))
  :group 'sln-browse)


    (define-minor-mode hungry-mode
           "Toggle Hungry mode."
          ;; The initial value.
          nil
          ;; The indicator for the mode line.
          " Hungry"
          ;; The minor mode bindings.
          '(([C-backspace] . hungry-electric-delete))
          :group 'hunger)

(fmakunbound 'hungry-mode)

(defun sln-browse ()
  "Opens a buffer to browse the solution"
  (let ((buffer (get-buffer-create "test")))
    (with-current-buffer buffer
      (erase-buffer)
      (mapcar 'sln-browse-project sln-projects)
      (sln-browse-mode)
      (display-buffer buffer))))

(defun sln-browse-project (project)
  (let ((beginning (point)))
    (insert (sln-project-name project))
    (insert "\n")))


(length project-buffer-sizes)
(car project-buffer-sizes)
(last project-buffer-sizes)

(car sln-projects)
(last sln-projects)
(length sln-projects)

(car sln-project-paths)
(last sln-project-paths)
(length sln-project-paths)

(sln-mode-on "c:/tfs/geo.cqrs/Solomo.Geo.CQRS.sln")
(sln-browse)

