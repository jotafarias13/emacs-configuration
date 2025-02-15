(use-package python
  :init
  (setq python-indent-guess-indent-offset nil)
  :config
  (setq python-shell-interpreter "python")
  (setq python-shell-completion-native-enable nil)
  (setq python-shell-interpreter-args "-ic \"from pprint import pprint\""))

;; (defun jlf/run-ruff-check-and-format-on-background ()
;;   "Run a shell COMMAND as a subprocess in the current buffer's file location."
;;   (interactive)
;;   (let* ((default-directory (file-name-directory (buffer-file-name)))
;; 	 (output-buffer "*Ruff Output*")
;; 	 (file-name (buffer-file-name))
;; 	 (command (concat "uv run ruff check --fix; uv run ruff format " file-name)))
;;     (with-current-buffer (get-buffer-create output-buffer)
;;       (erase-buffer))
;;     (start-process-shell-command
;;      "run-command-subprocess" output-buffer command)
;;     (message "Running command: %s" command)))

;; (with-eval-after-load 'python
;;   (define-key python-mode-map (kbd "C-c j u") 'jlf/run-ruff-check-and-format-on-background))

;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (add-hook 'after-save-hook 'jlf/run-ruff-check-and-format-on-background nil t)))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

;; (use-package blacken
;;   :hook (python-mode . blacken-mode)
;;   :init (setq blacken-line-length 79))

;; (use-package py-isort
;;   :init
;;   (setq py-isort-options '("--profile=black" "-l 79")))
;; (add-hook 'before-save-hook 'py-isort-before-save)

;; (defun python-ruff-format ()
;;   "Formats the current buffer using ruff (also sorts imports)."
;;   (interactive)
;;   (shell-command (concat "ruff format --line-length 79 " (buffer-file-name)))
;;   (shell-command (concat "ruff check --select I --fix " (buffer-file-name))))

;; (defun python-ruff-format-on-save ()
;;   "Runs python-ruff-format on save."
;;   (interactive)
;;   (when (eq major-mode 'python-mode)
;;     (condition-case err (python-ruff-format)
;;       (error (message "%s" (error-message-string err))))))

;; (add-hook 'before-save-hook 'python-ruff-format-on-save nil t)

;; (use-package flymake-ruff
;;   :hook (python-mode . flymake-ruff-load))

;; (use-package numpydoc
;;   :after yasnippet
;;   :init
;;   (setq numpydoc-insertion-style nil)
;;   (setq numpydoc-insertion-style 'yas)
;;   (setq numpydoc-insert-examples-block nil)
;;   :bind (:map python-mode-map
;; 	      ("C-c C-n" . numpydoc-generate))
;;   :after python)

;; (use-package importmagic
;;   :after python
;;   :bind (:map python-mode-map
;;               ("C-c j p" . importmagic-fix-symbol-at-point)
;;               ("C-c j a" . importmagic-fix-imports)
;;               ("C-c j s" . importmagic-fix-symbol))
;;   :config
;;   (add-hook 'python-mode-hook 'importmagic-mode))

(use-package pyimport
  :after python
  :bind (:map python-mode-map
	      ("C-c j r" . pyimport-remove-unused)))

(defun jlf/return-t()
  "Always returns true."
  t)

(defvar jlf/virtualenv-name ".venv")

(defun jlf/project-root (file-or-dir-name max-depth)
  "Find the project root directory containing FILE-OR-DIR-NAME,
     up to MAX-DEPTH levels."
  (let ((dir (file-name-parent-directory (or (buffer-file-name) default-directory))))
    (catch 'my-project-root
      (dotimes (i max-depth)
	(if (file-exists-p (concat dir file-or-dir-name))
	    (throw 'my-project-root dir)
	  (setq dir (file-name-parent-directory dir)))) nil)))

(defun jlf/find-project-root (&optional markers)
  "Find the first directory containing any of the specified MARKERS, searching backwards from the current file.
If MARKERS is nil, it defaults to (\".git\" \".venv\")."
  (let ((markers (or markers '(".git" ".venv")))
	(dir (file-name-directory (or buffer-file-name default-directory))))
    (catch 'found
      (while (and dir (not (string= dir "/")))
	(dolist (marker markers)
	  (when (file-exists-p (concat dir marker))
	    (throw 'found dir)))
	(setq dir (file-name-directory (directory-file-name dir))))
      nil)))

(defun jlf/python-venv-activate()
  "Activates virtual environment automatically.
     If there is a .venv folder in project-root, activate
     that environment. Else, if there is a .venv directory
     anywhere 3 directories upwards, activate that environment.
     Else, ask for user to select environment manually."
  (interactive)
  ;; (let* ((root (project-root (eglot--current-project)))
  (let* ((root (jlf/find-project-root))
	 (env (concat root jlf/virtualenv-name)))
    (if (file-directory-p env)
	(pyvenv-activate env)
      ;; (let ((other-root (jlf/project-root jlf/virtualenv-name 3)))
      (let ((other-root (jlf/find-project-root)))
	(if other-root
	    (pyvenv-activate (concat other-root jlf/virtualenv-name))
	  (call-interactively #'pyvenv-activate))))))

(defun jlf/python-venv-activate-workon()
  "Activates workon virtual environment automatically.
       If there is a .venv folder in $WORKON_HOME, activate
       that environment. Else, ask for user to select
       workon environment manually."
  (interactive)
  (let ((env (concat (pyvenv-workon-home) "/.venv")))
    (if (file-directory-p env)
	(pyvenv-activate env)
      (call-interactively #'pyvenv-workon))))


(defun jlf/python-venv-activate-ask()
  "Activates virtual environment with user input."
  (interactive)
  (call-interactively #'pyvenv-activate))


(defun jlf/python-run-python()
  "Opens buffer-dedicated python REPL buffer."
  (interactive)
  (run-python nil t t)
  (other-window -1))


(defun jlf/python-start-python()
  "Activates virtual enviroment and starts python REPL."
  (interactive)
  (jlf/python-venv-activate)
  (jlf/python-run-python))


(defun jlf/python-venv-deactivate()
  "Deactivates virtual environment."
  (interactive)
  (pyvenv-deactivate))


(defun jlf/python-kill-buffer-dedicated()
  "Kills dedicated python process attached to the current buffer."
  (interactive)
  (let ((kill-buffer-query-functions (list 'jlf/return-t))
	(process (concat "*Python[" (buffer-name) "]*")))
    (kill-buffer process)))


(defun jlf/python-kill-buffer-all()
  "Kills all python process."
  (interactive)
  (let ((kill-buffer-query-functions (list 'jlf/return-t)))
    (kill-matching-buffers "\\*Python" nil t)))


(defun jlf/python-end-python()
  "Deactivates virtual environment and kills python process."
  (interactive)
  (jlf/python-venv-deactivate)
  (jlf/python-kill-buffer-dedicated))

(defun jlf/install-requirements()
  "Creates virtualenv, activates it and installs project requirements."
  (interactive)
  (eshell nil)
  ;; (insert (concat "virtualenv " jlf/virtualenv-name))
  (insert (concat "uv " (substring jlf/virtualenv-name 1)))
  (execute-kbd-macro (kbd "<return>"))
  (sit-for 1.5)
  (kill-buffer (current-buffer))
  (jlf/python-venv-activate)
  (eshell nil)
  ;; (insert "pip install -r requirements.txt")
  (insert "uv pip install -r requirements.txt")
  (execute-kbd-macro (kbd "<return>")))

(defun jlf/python-add-import-manually (text)
  "Inserts the given TEXT (python package) at the beginning of the current buffer and returns to the starting point."
  (interactive "sEnter import statement: ")
  (save-excursion
    (goto-char (point-min))
    (evil-open-above 1)
    (insert text))
  (py-isort-buffer))

(defun jlf/python-open-project-env-file ()
  "Open the .env file in the root of the current project."
  (interactive)
  (let ((project-root (project-root (project-current t))))
    (find-file (concat project-root ".env"))))

(defun jlf/python-save-and-format-buffer ()
  "Save the current buffer and run a predefined shell command using the current file name."
  (interactive)
  (when buffer-file-name
    (save-buffer)
    (let ((command (format "uv run ruff format %1$s; uv run ruff check --fix %1$s > /dev/null 2>&1" (shell-quote-argument buffer-file-name))))
      (shell-command command))
    (revert-buffer t t)))

(define-key python-mode-map (kbd "C-x C-a") 'jlf/python-save-and-format-buffer)

(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c a") 'jlf/python-venv-activate)))
(global-set-key (kbd "C-c a") 'jlf/python-venv-activate)
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c w") 'jlf/python-venv-activate-workon)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c C-a") 'jlf/python-venv-activate-ask)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c p") 'jlf/python-run-python)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c s") 'jlf/python-start-python)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c d") 'jlf/python-venv-deactivate)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c o") 'jlf/python-kill-buffer-dedicated)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c m") 'jlf/python-kill-buffer-all)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c k") 'jlf/python-end-python)))
(global-set-key (kbd "C-c u") 'jlf/install-requirements)
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c f") 'flymake-show-buffer-diagnostics)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c j m") 'jlf/python-add-import-manually)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c j e") 'jlf/python-open-project-env-file)))

(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)

(load-file "config/python-remove-import.el")
(load-file "config/python-format-comment.el")
(load-file "config/python-insert-import.el")

;; (with-eval-after-load "eglot"

;;   (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

;;   (setq-default eglot-workspace-configuration
;;                 '((:pylsp . (:configurationSources ["flake8"] 
;;                              :plugins (:pycodestyle (:enabled nil) 
;;                                        :jedi_completion (:include_params t :fuzzy t)
;;                                        :mccabe (:enabled nil) 
;;                                        :pyflakes (:enabled nil)
;;                                        :flake8 (:enabled t :ignore ["E203" "W503"])))))))
