(define-derived-mode html2-mode html-mode
  (defun html-indentation-level ()
    "returns the number of tabs to do"
	(setq indentation-level-counter -1) ; first element will set this to 0
	(setq indentation-level-unindent-next-line nil)
    (let ((line (line-number-at-pos))
		  (i 0))
      (save-excursion
		(beginning-of-buffer)
		(dotimes (number line i)
		  (back-to-indentation)
		  (if indentation-level-unindent-next-line
			  (setq indentation-level-counter (- indentation-level-counter 1)))
		  (setq indentation-level-unindent-next-line nil)
		  (forward-char)
		  (if (equal (following-char) ?/)
			  (setq indentation-level-unindent-next-line t)
			(if (equal (preceding-char) ?<)
				(setq indentation-level-counter (+ indentation-level-counter 1))))
		  (next-line))
		indentation-level-counter)))

  (defun html-indent-line ()
	"indents current line"
	(interactive)
	(save-excursion
	  (back-to-indentation)
	  (delete-region (line-beginning-position) (point))
	  (insert (make-string (indentation-level) ?\t))))


  (defun html-indent-region ()
	"indents current region"
	(interactive)
	(save-excursion
	  (dolist (i (number-sequence (region-beginning) (region-end)))
		(goto-line i)
		(html-indent-line))))

  (defun html-indent-line-or-region ()
	"indents line or region"
	(interactive)
	(if (use-region-p)
		(html-indent-region)
	  (html-indent-line)))

  (local-set-key (kbd "TAB") 'html-indent-line-or-region))

