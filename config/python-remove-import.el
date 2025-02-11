;; (defun python-remove-import-check-import-type (line-number)
;;   "Check if the import at LINE-NUMBER is of type single, multi or multiline."
;;   (save-excursion
;;     (goto-line line-number)
;;     (beginning-of-line)
;;     (let ((line-content (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
;;       (cond ((string-match-p "," line-content)
;; 	     "multi")
;; 	    ((string-match-p "(" line-content)
;; 	     "multiline")
;; 	    (t
;; 	     "single")))))

;; (defun python-remove-import-remove-single-import ()
;;   "Removes import of type single"
;;   (beginning-of-line)
;;   (kill-line))

;; (defun python-remove-import-remove-multi-import (import)
;;   "Removes import of type multi"
;;   (beginning-of-line)
;;   (when (search-forward import (line-end-position) t)
;;     (replace-match "")))

;; (defun python-remove-import-remove-multiline-import (import)
;;   "Removes import of type multiline"
;;   (let ((begin (point)))
;;     (beginning-of-line)
;;     (when (search-forward ")" nil t))
;;     (let ((end (point)))
;;       (goto-char begin)
;;       (when (search-forward import end t)
;; 	(replace-match "")))))

;; (defun python-remove-import-extract-single-quoted-text (string)
;;   "Extract contents from inside single quotes in a STRING."
;;   (when (string-match "'\\(.*?\\)'" string)
;;     (match-string 1 string)))

;; (defun python-remove-import-process-flake8-warnings (string)
;;   "Process flake8 warnings for line number and content."
;;   (let* ((lines (split-string string "\n" t))
;;          (line-data (mapcar (lambda (line)
;;                               (let* ((parts (split-string line ":"))
;;                                      (line-number (string-to-number (car parts)))
;;                                      (line-text
;; 				      (cadr
;; 				       (split-string
;; 					(python-remove-import-extract-single-quoted-text (cadr parts)) "\\."))))
;;                                 (list line-number line-text)))
;;                             lines)))
;;     line-data))

;; (defun python-remove-import-remove-all ()
;;   "Removes all unused imports in a python buffer."
;;   (interactive)
;;   (let* ((warnings (shell-command-to-string (concat "flake8 --select=F401 --format='%(row)d:%(text)s' " (buffer-file-name))))
;; 	 (data (python-remove-import-process-flake8-warnings warnings)))
;;     (save-excursion
;;       (dolist (warning data)
;; 	(goto-line (car warning))
;; 	(let ((import-type (python-remove-import-check-import-type (car warning))))
;; 	  (cond
;; 	   ((string-equal import-type "single") (python-remove-import-remove-single-import))
;; 	   ((string-equal import-type "multi") (python-remove-import-remove-multi-import (cadr warning)))
;; 	   ((string-equal import-type "multiline") (python-remove-import-remove-multiline-import (cadr warning)))
;; 	   )))))
;;   (py-isort-buffer)
;;   (blacken-buffer))

;; (add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c j r") 'python-remove-import-remove-all)))
