;; Function to exclude certain modules based on regex patterns
(defun exclude-modules-p (module)
  (or (string-match-p "^_" module)
      (string-match-p "^[0-9]" module)
      (string-match-p "[0-9]\\{2,\\}" module)))

(defun select-all-modules()
  (run-python-internal)
  (sit-for 0.2)
  (let* ((input (python-shell-internal-send-string "help('modules')"))
	 (index0 (string-match "    " input))
	 (index1 (string-match "\n" (reverse (substring input 0 index0))))
	 (text1 (substring input (- index0 index1)))
	 (index2 (string-match "\n\n" text1))
	 (text2 (substring text1 0 index2))
	 (text3 (replace-regexp-in-string "\\s-+" " " text2))
	 (text4 (replace-regexp-in-string "\n" " " text3))
	 (all-modules (split-string text4)))

    ;; Filtering modules using the exclude-modules-p function and storing in a list
    (setq all-modules-list (remove-if 'exclude-modules-p all-modules))))


(defun keep-odd-elements (lst)
  (let (result)
    (while lst
      (setq result (cons (car lst) result))
      (setq lst (cdr (cdr lst))))
    (nreverse result)))


(defun select-installed-modules()
  (let* ((input (shell-command-to-string "pip list --not-required"))
	 (index1 (string-match "\n\n" input))
	 (text1 (substring input 0 index1))
	 (index2 (string-match "---" (reverse text1)))
	 (text2 (substring text1 (- index1 index2) index1))
	 (text4 (replace-regexp-in-string "\\s-+" " " text2))
	 (text5 (replace-regexp-in-string "\n" " " text4))
	 (modules-versions (split-string text5))
	 (installed-modules (keep-odd-elements modules-versions)))

    ;; Filtering modules using the exclude-modules-p function and storing in a list
    (setq installed-modules-list installed-modules)))




(defun current-buffer-directory ()
  "Get the directory of the current buffer."
  (when (buffer-file-name)
    (file-name-directory (buffer-file-name))))


(defun find-all-py-files (directory)
  "Recursively find all .py files inside DIRECTORY and its subdirectories."
  (let (py-files)
    (dolist (file (directory-files directory t))
      (cond
       ((file-directory-p file)
        ;; Skip the . and .. directories to avoid infinite loops
        (unless (string-match "/\\." file)
          (setq py-files (append py-files (find-all-py-files file)))))
       ((string-suffix-p ".py" file)
        (setq py-files (cons file py-files)))))
    py-files))


(defun remove-common-substring (lst common-substring)
  "Remove COMMON-SUBSTRING from the beginning of all strings in LST."
  (mapcar (lambda (str)
            (if (string-prefix-p common-substring str)
                (substring str (length common-substring))
              str))
          lst))


(defun process-py-file-path (file-name)
  "Process a .py file path to eliminate the directory, extension, and replace / with ."
  (replace-regexp-in-string "\\.py$" "" (replace-regexp-in-string "/" "." file-name)))



(defun select-local-modules ()
  (let* ((directory (current-buffer-directory))
	 (all-python-files (find-all-py-files directory))
	 (files-without-dir (remove-common-substring all-python-files directory))
	 (modules (mapcar #'process-py-file-path files-without-dir)))
    (setq local-modules-list modules)))




(defun combine-modules (list1 list2 list3)
  "Combine lists into a single list and remove duplicates keeping the first appearance."
  (let* ((ordered-list1 (sort list1 #'string<))
	 (ordered-list2 (sort list2 #'string<))
	 (ordered-list3 (sort list3 #'string<))
	 (all-lists (list ordered-list1 ordered-list2 ordered-list3))
	 (combined-list '()))
    (dolist (lst all-lists)
      (dolist (elem lst)
        (unless (member elem combined-list)
          (setq combined-list (cons elem combined-list)))))
    (nreverse combined-list)))


(defun list-modules()
  (let* ((local-modules (select-local-modules))
	 (installed-modules (select-installed-modules))
	 (all-modules (select-all-modules))
	 (combined-modules (combine-modules local-modules installed-modules all-modules)))
    combined-modules))

(defun import-type-import (module)
  (format "import %s" module))


(defun import-type-from (module)
  (let* ((prescient-sort-length-enable nil)
	 (submodules '())
         (submodule "")
	 (submodule-name module))
    (while (not (string-equal submodule "import"))
      (setq module-contents (python-shell-internal-send-string
			     (format "import %s\n\nprint(' '.join(dir(%s)))" module submodule-name)))
      (setq module-contents-list (split-string module-contents))
      (setq module-contents-processed-list (remove-if 'exclude-modules-p module-contents-list))
      (setq module-list (cons "import" module-contents-processed-list))
      (setq submodule (completing-read "Select a submodule/function/class/constant: " module-list))
      (unless (string-equal submodule "import")
	(push submodule submodules))
      (setq submodule-name (format "%s.%s" module (mapconcat 'identity (reverse submodules) "."))))
    (if (null submodules) (setq final-submodule module)
      (setq final-submodule submodule-name))

    (let* ((module-contents (python-shell-internal-send-string
				  (format "import %s\n\nprint(' '.join(dir(%s)))" module final-submodule)))
	   (module-contents-list (split-string module-contents))

	   (module-contents-processed-list (remove-if 'exclude-modules-p module-contents-list))
	   (module-list (if (null module-contents-processed-list) module-contents-list module-contents-processed-list))

	   (final-import (completing-read "Select a function/class/constant: " module-list)))
      (format "from %s import %s" final-submodule final-import))))



(defun jlf/python-add-import ()
  "Create a Python import statement interactively."
  (interactive)
  (let* ((prescient-sort-length-enable nil)
	 (import-type (completing-read "Select import type: " '("from" "import")))
         (modules (list-modules))
         (module (completing-read "Select a module: " modules))
	 (import-statement ""))
    (if (string= import-type "import")
	(setq import-statement (import-type-import module))
      (setq import-statement (import-type-from module)))
    (let ((alias (when (y-or-n-p "Add an alias? ") (read-string "Enter alias: "))))
      (setq import-statement (if alias (concat import-statement (format " as %s" alias)) import-statement))
  (jlf/python-insert-import import-statement))))



(defun jlf/python-insert-import (text)
  "Inserts the given TEXT (python package) at the beginning of the current buffer and returns to the starting point."
  (save-excursion
    (goto-char (point-min))
    (evil-open-above 1)
    (insert text)
    (evil-normal-state)
    (py-isort-buffer))
  (save-buffer))


(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c j i") 'jlf/python-add-import)))
