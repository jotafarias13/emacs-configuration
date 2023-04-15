(use-package python
  :init
  (setq python-indent-guess-indent-offset nil)
  :config
  (setq python-shell-interpreter "python"))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :init (setq blacken-line-length 79))

(use-package py-isort
  :init
  (setq py-isort-options '("--profile=black" "-l 79")))
(add-hook 'before-save-hook 'py-isort-before-save)

(use-package numpydoc
  :init
  (setq numpydoc-insertion-style nil)
  (setq numpydoc-insertion-style 'yas)
  (setq numpydoc-insert-examples-block nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate))
  :after python)

(defun jlf/return-t()
  "Always returns true."
  t)


(defun jlf/python-venv-activate()
  "Activates virtual environment automatically.
   If there is a .venv folder in project-root, activate
   that environment. Else, ask for user to select
   environment manually."
  (interactive)
  (let* ((root (project-root (eglot--current-project)))
         (env (concat root ".venv")))
    (if (file-directory-p env)
        (pyvenv-activate env)
      (call-interactively #'pyvenv-activate))))


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


(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c a") 'jlf/python-venv-activate)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c w") 'jlf/python-venv-activate-workon)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c C-a") 'jlf/python-venv-activate-ask)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c p") 'jlf/python-run-python)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c s") 'jlf/python-start-python)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c d") 'jlf/python-venv-deactivate)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c o") 'jlf/python-kill-buffer-dedicated)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c l") 'jlf/python-kill-buffer-all)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c k") 'jlf/python-end-python)))
(add-hook 'python-mode-hook #'(lambda () (define-key python-mode-map (kbd "C-c f") 'flymake-show-buffer-diagnostics)))

(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)

(with-eval-after-load "eglot"

  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"] 
                             :plugins (:pycodestyle (:enabled nil) 
                                       :jedi_completion (:include_params t :fuzzy t)
                                       :mccabe (:enabled nil) 
                                       :pycodestyle (:enabled nil)
                                       :pyflakes (:enabled nil)
                                       :flake8 (:enabled t)))))))