(defun python-split-comment-line (comment &optional max-length)
  "Split a Python comment line to adhere to a maximum length.
   Words are not split. Leading and trailing white spaces are removed.
   Each line starts with '# '.
   MAX-LENGTH is the maximum line length (default value is 72).
   To account for trailing '# ' we subtract 2 from the maximum length."
  (setq max-length (or max-length 72))
  (setq max-length (- max-length 2))
  (let* ((words (split-string comment " "))
         (lines '())
         (current-line "")
         (current-length 0))
    (dolist (word words)
      (let* ((word-length (length word))
             (word-with-space (concat word " "))
             (potential-length (+ current-length (length word-with-space))))
        (if (or (> potential-length max-length)
                (string-empty-p current-line))
            (progn
              (when (not (string-empty-p current-line))
                (setq lines (cons (concat "# " (string-trim current-line)) lines)))
              (setq current-line word-with-space)
              (setq current-length word-length))
          (setq current-line (concat current-line word-with-space))
          (setq current-length potential-length))))
    (when (not (string-empty-p current-line))
      (setq lines (cons (concat "# " (string-trim current-line)) lines)))
    (reverse lines)))

(defun clean-comment (comment)
  "Remove all occurrences of '#', double blank spaces, trailing/leading whitespace, and line breaks from the selected text."
  (let* ((cleaned-text (replace-regexp-in-string "#+" "" comment))
	 (no-double-spaces (replace-regexp-in-string "  +" " " cleaned-text))
	 (no-line-breaks (replace-regexp-in-string "\n" "" no-double-spaces))
	 (final-text (string-trim no-line-breaks)))
    final-text))

(defun python-format-comment ()
  "Format python comment line to conform to maximum length."
  (interactive)
  (when (region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (selected-text (buffer-substring-no-properties start end))
           (cleaned-text (clean-comment selected-text))
	   (comments (python-split-comment-line cleaned-text)))
      (delete-region start end)
      (goto-char start)
      (dolist (line comments)
	(insert (concat line "\n"))))))
