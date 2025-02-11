(load-file (concat user-emacs-directory "config/basic-config.el"))

(use-package which-key
  :config
  (progn
    (which-key-setup-side-window-right-bottom)
    (which-key-mode)))

(use-package ace-window
  :bind (("C-1" . other-window)
         ("C-2" . other-frame)))

;; Autocompletion in-buffer
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'company-mode-hook #'(lambda () (define-key company-active-map (kbd "<tab>") nil)))
  (add-hook 'company-mode-hook #'(lambda () (define-key company-active-map (kbd "TAB") nil)))
  (add-hook 'company-mode-hook #'(lambda () (define-key company-active-map (kbd "C-<return>") 'company-abort)))
  (add-hook 'company-mode-hook #'(lambda () (define-key company-active-map (kbd "<return>") 'company-complete-selection)))
  (add-hook 'company-mode-hook #'(lambda () (define-key company-active-map (kbd "C-j") 'company-select-next)))
  (add-hook 'company-mode-hook #'(lambda () (define-key company-active-map (kbd "C-k") 'company-select-previous)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2))

;; Melhora aparência do menu de autocompletion
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
    ;; (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "systemBlueColor")) ;; Fica melhor com o tema doom-moonlight
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "systemBlueColor") ;; Fica melhor com o tema dracula
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "systemIndigoColor")) ;; Fica melhor com o tema dracula

;; Adiciona informação extra nos buffers de ajuda
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; (defun jlf/olivetti-mode-setup ()
;;   (olivetti-mode)
;;   (olivetti-set-width 0.9))

;; (use-package olivetti
;;   :hook (org-mode . jlf/olivetti-mode-setup))

(use-package prescient
  :custom
  (prescient-sort-full-matches-first t))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-sort-commands '(:not swiper swiper-isearch ivy-switch-buffer ivy-completing-read))
  (ivy-prescient-retain-classic-highlighting t)
  :config (ivy-prescient-mode))

(use-package company-prescient
  :custom
  (company-prescient-sort-length-enable nil)
  :config
  (company-prescient-mode))

(defvar jlf/scratch-directory "~/Sync/Jota/Academico/Projetos/Emacs/scratch/")

(defun jlf/adjust-frame-size-and-position ()
  "Adjusts the size and position of the Emacs frame."
  (interactive)
  (let* ((screen-width (display-pixel-width))
         (screen-height (display-pixel-height))
         (new-width (/ screen-width 2))
         (new-height (/ screen-height 2))
         (new-left (- screen-width new-width))
         (new-top 0))
    (set-frame-size (selected-frame) new-width new-height t)
    (set-frame-position (selected-frame) new-left new-top)))

(defun jlf/scratch()
  (interactive)
  (jlf/adjust-frame-size-and-position)
  (find-file (concat jlf/scratch-directory "scratch.md"))
  (end-of-buffer)
  (evil-open-below 1)
  (insert "- "))

;; Possibilita a criação de bundles estilo TextMate
(use-package yasnippet
  :config (yas-global-mode 1))
 
;; Adiciona vários snippets úteis
(use-package yasnippet-snippets)

;; Diretório para adicionar snippets próprios
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; Interface Git
(use-package magit
  :custom 
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 'all))

(use-package psvn
  :ensure nil
  :load-path "~/.emacs.d/elpa/psvn/")

;; Não entrar no evil-mode quando abrir o svn-status-mode
(evil-set-initial-state 'svn-status-mode 'emacs)

(define-key dired-mode-map (kbd "C-c s") 'svn-status-update)
(define-key svn-status-mode-map (kbd "C-d") 'evil-scroll-down)
(define-key svn-status-mode-map (kbd "C-u") 'evil-scroll-up)
(define-key svn-status-mode-map (kbd "C-e") 'evil-scroll-line-down)
(define-key svn-status-mode-map (kbd "C-y") 'evil-scroll-line-up)

;; Remapear os comandos originais de 'w' e 'b'
(define-key svn-status-mode-map (kbd "C-w") 'svn-status-copy-current-line-info)
(define-key svn-status-mode-map (kbd "C-b") 'svn-status-blame)
(define-key svn-status-mode-map (kbd "w") 'evil-forward-word-begin)
(define-key svn-status-mode-map (kbd "b") 'evil-backward-word-begin)

;; Remapear o comando original de 'k'
(define-key svn-status-mode-map (kbd "C-k") 'svn-status-lock)
(define-key svn-status-mode-map (kbd "k") 'evil-previous-visual-line)
(define-key svn-status-mode-map (kbd "j") 'evil-next-visual-line)

;; Utiliza $PATH do terminal
(use-package exec-path-from-shell) ; torna o PATH do shell igual do temrinal
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(defun create-dedicated-eshell()
  (interactive)
  (let ((eshell-buffer-name (format "*eshell[%s]*" (buffer-name))))
    (eshell)))

(global-set-key (kbd "C-M-s") (lambda () (interactive) (create-dedicated-eshell)))
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.env.example\\'" . sh-mode))

;; AUCTeX
(use-package tex
  :ensure auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Ativa algumas configurações do AUCTeX para melhorar a escrita do código
(setq TeX-electric-sub-and-superscript t)
(setq LaTeX-electric-left-right-brace t)
(setq TeX-electric-math (cons "$" "$"))

;; Coloca LaTeX-Mk disponível via C-c C-c
;; SyncTeX é configurado no arquivo "~/.latexmkrc"
(eval-after-load "tex" (lambda ()
			 (push
			  '("LaTeX-Mk" "latexmk -pdf -pvc %s" TeX-run-TeX nil t
			    :help "Run LaTeX-Mk on file")
			  TeX-command-list)
			 (push
			  '("CleanAll" "latexmk -c" TeX-run-TeX nil t
			    :help "Files for deletion not found")
			  TeX-command-list)
			 (setq-default TeX-command-default "LaTeX-Mk")))

;; Usa Skim como visualizador padrão, habilita PDF Sync
;; Displayline do Skim é usado para pesquisa .tex -> .pdf
;; Opção -b grifa a lina atual e -g abre o Skim no background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(add-to-list
 'TeX-complete-list
 '("\\\\citeonline\\[[^]\n\\%]*\\]{\\([^{}\n\\%,]*\\)" 1 LaTeX-bibitem-list "}"))

(add-to-list
 'TeX-complete-list
 '("\\\\citeonline{\\([^{}\n\\%,]*\\)" 1 LaTeX-bibitem-list "}"))

(add-to-list
 'TeX-complete-list
 '("\\\\citeonline{\\([^{}\n\\%]*,\\)\\([^{}\n\\%,]*\\)" 2 LaTeX-bibitem-list))

(add-to-list
 'TeX-complete-list
 '("\\\\autoref{\\([^{}\n\\%,]*\\)" 1 LaTeX-label-list "}"))

;; Inicializa o modo servidor no Emacs para possibilitar a comunicação com o Skim
;; (server-start)

;; Habilita evil keybindings voltados para TeX
(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))
(setq evil-tex-toggle-override-m nil)
(setq evil-tex-toggle-override-t t)

;; Função personalizada para adicionar um novo item no itemize
(defun jlf/LaTeX-insert-item ()
  (interactive)
  (evil-open-below 1)
  (insert "\\item ")
  (indent-for-tab-command)
  (evil-append 1))

(add-hook 'LaTeX-mode-hook #'(lambda () (define-key LaTeX-mode-map (kbd "C-<return>") 'jlf/LaTeX-insert-item)))

;; Breadcrumb no topo do buffer (caminho do arquivo)
(defun jlf/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Adiciona funcionalidades de IDE para o Emacs
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . jlf/lsp-mode-setup)
  :hook (lsp-mode . flymake-mode)
  :init
  (setq lsp-keymap-prefix "C-c l") 
  (setq lsp-diagnostics-provider :none)
  :hook
  (c++-mode . lsp)
  (c-mode . lsp)
  (python-mode . lsp)
  :config
  (lsp-enable-which-key-integration t)
  ;; (setq lsp-pylsp-server-command "pylsp")  ;; Specify pylsp as the server
  ;; (lsp-register-custom-settings
  ;;  '(("pylsp.plugins.pylint.enabled" nil t)
  ;;    ("pylsp.plugins.flake8.enabled" nil t)
  ;;    ("pylsp.plugins.mccabe.enabled" nil t)
  ;;    ("pylsp.plugins.preload.enabled" nil t)
  ;;    ("pylsp.plugins.pydocstyle.enabled" nil t)
  ;;    ("pylsp.plugins.jedi_definition.enabled" nil t)
  ;;    ("pylsp.plugins.jedi_references.enabled" nil t)
  ;;    ("pylsp.plugins.jedi_signature_help.enabled" nil t)
  ;;    ("pylsp.plugins.jedi_hover.enabled" nil t)
  ;;    ("pylsp.plugins.jedi_symbols.enabled" nil t)
  ;;    ("pylsp.plugins.jedi_completion.enabled" nil t)
  ;;    ("pylsp.plugins.ruff.enabled" t)))
  )

;; Feature do clangd que possibilita a escolha do overload de uma função no company-box
(setq lsp-clients-clangd-args '("--completion-style=detailed" "--header-insertion=never"))

(setq lsp-headerline-breadcrumb-enable-diagnostics nil)
(setq lsp-diagnostics-provider 'flymake)
;; (setq lsp-diagnostics-provider :none)
(setq lsp-signature-render-documentation nil)

;; Pacote para adicionar explicação do código à medida que o cursor navega pelo buffer 
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Realiza integração do ivy no lsp-mode
(use-package lsp-ivy)

;; Configura o estilo de formatação de buffer para linguagens em C e similares
;; (setq c-default-style '(c++-mode  . “cc-mode”))
(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(c++-mode . "cc-mode")
	(other . "gnu")))

(with-eval-after-load 'lsp-mode
  ;; (setq lsp-enabled-clients '(ruff))
  ;; (setq lsp-disabled-clients '(pylsp))
  (setq lsp-enabled-clients '(ruff pylsp))
  (setq lsp-disabled-clients '())
  (setq lsp-pylsp-plugins-mypy-enabled t)
  (add-hook 'lsp-managed-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'lsp-format-buffer nil t))))

(load-file (concat user-emacs-directory "config/python-config.el"))

(load-file (concat user-emacs-directory "config/rust-config.el"))

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

(use-package emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; (use-package treemacs
;;   :config
;;   (treemacs-git-mode 'deferred)
;;   (treemacs-filewatch-mode t)
;;   (treemacs-peek-mode t)
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t d"   . treemacs-select-directory)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil))

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once))

;; (use-package treemacs-magit
;;   :after (treemacs magit))

;; (use-package treemacs-all-the-icons
;;   :after (treemacs treemacs-icons-dired))

(use-package tree-sitter)
(use-package tree-sitter-langs)
(add-hook 'python-mode-hook 'tree-sitter-mode)
(add-hook 'python-mode-hook #'(lambda () (tree-sitter-hl-mode)))

(use-package flymake-ruff
  :ensure t
  :hook (lsp-managed-mode . flymake-ruff-load))
  ;; :hook (python-mode . flymake-ruff-load))

(with-eval-after-load "flymake" 
  (set-face-attribute 'flymake-warning nil :underline nil)
  (set-face-attribute 'flymake-error nil :underline nil)
  (setq flymake-diagnostic-functions '(flymake-ruff)))

;; Funciona como um cliente LSP para Emacs, utilizado para escrever em LaTeX
(use-package eglot
  :hook 
  ;; (LaTeX-mode . eglot-ensure)
  ;; (python-mode . eglot-ensure))
  (LaTeX-mode . eglot-ensure))

;; Auxilia o Eglot a reconhecer projetos com arquivos em diretórios distintos

;; (defvar main-tex "defesa.tex")
(defvar main-tex "main.tex")

;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;; 	       '(python-mode . ("ruff" "server")))
;;   (add-hook 'after-save-hook 'eglot-format))

(defun jlf/latex-root (dir)
  (when-let ((root (locate-dominating-file dir main-tex)))
    (cons 'latex-module root)))

(add-hook 'project-find-functions #'jlf/latex-root)

(cl-defmethod project-root ((project (head latex-module)))
  (cdr project))

;; Tell project-root that directories with .venv folders are python project roots
;; (defun jlf/python-root (dir)
;;   (when-let ((root (locate-dominating-file dir jlf/virtualenv-name)))
;;     (cons 'python-module root)))

;; (add-hook 'project-find-functions #'jlf/python-root)

;; (cl-defmethod project-root ((project (head python-module)))
;;   (cdr project))

;; (with-eval-after-load "eglot"
;;   (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil :inherit 'unspecified)
;;   (set-face-attribute 'eglot-highlight-symbol-face nil :foreground "cyan"))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(defvar jlf/my-workspace-alist (list)
  "List of entries in workspace.")

(add-to-list 'jlf/my-workspace-alist '("Emacs" . (lambda () (jlf/my-workspace-find-file "~/.emacs.d/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Slip-Box" . (lambda () (jlf/my-workspace-find-file jlf/slipbox-directory))) t)
(add-to-list 'jlf/my-workspace-alist '("Doutorado" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/Academico/Pós-Graduação/UFRN/Doutorado/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Ledger" . (lambda () (find-file "~/Sync/Jota/Financeiro/Ledger/ledger.dat"))) t)
(add-to-list 'jlf/my-workspace-alist '("Lattes" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/Academico/Projetos/Lattes/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Python" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/Academico/Projetos/Python/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Rust" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/Academico/Projetos/Rust/"))) t)
(add-to-list 'jlf/my-workspace-alist '("NewGate" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/NewGate/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Inovall" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/Inovall/Code/DM-005/"))) t)

(defun jlf/my-workspace-find-file (FILE)
  (let ((default-directory FILE))
    (call-interactively
     (lambda (file-name)
       (interactive "fOpen File: ")
       (find-file file-name nil)))))

(defun jlf/my-workspace ()
  "Ferrameta para facilitar abertura de arquivos e diretórios dos projetos nos quais trabalho."
  (interactive)
  (let* ((my-workspace-list (mapcar 'car jlf/my-workspace-alist))
         (my-workspace (completing-read "WorkSpace: " (sort my-workspace-list (lambda (A B) (string-lessp A B))))))
    (if (assoc my-workspace jlf/my-workspace-alist)
        (funcall (cdr (assoc my-workspace jlf/my-workspace-alist)))
      (message "Invalid Argument!"))))

(global-set-key (kbd "C-+") 'jlf/my-workspace) ;; Keybinding para ferramenta MyWorkSpace

;; Pacotes necessários para utilização do PDF-Tools
(use-package let-alist)
(use-package tablist)

;; Necessário instalar o libpng e poppler (homebrew ou macports)
;; Configurar a variável PKG_CONFIG_PATH no Shell Profile (bash ou zsh)
;; O path deve ser onde se encontra a biblioteca do pkgconfig
;; export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig/"
;; Preferencialmente instalar o pdf-tools pelo MELPA (list-packages)
;; Executar o comando 'pdf-tools-install' antes de configurar o pacote
(use-package pdf-tools
  :pin manual ;; não sei a explicação
  :config
  (pdf-tools-install) ;; executa antes de configurar pela primeira vez
  ;; Centraliza na largura do PDF
  (setq-default pdf-view-display-size 'fit-width)
  ;; Anotar automaticamente os highlights
  ;; Comentado pois gera conflito com o org-noter-pdftools
  ;; (setq pdf-annot-activate-created-annotations t)
  ;; Configuração da pesquisa dentro do PDF buffer
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  ;; Ativa midnight-mode automaticamente para PDF's (inversão de cores)
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (pdf-view-midnight-minor-mode t)))) 

;; Função para otimizar os espaços laterais "em branco" do buffer
(defun guto/pdf-view-slice-vertical (&optional window)
  "Minha versão do slice para cortar só as laterais e deixar espaço vertical"
  (interactive)
  (let* ((bb (pdf-cache-boundingbox (pdf-view-current-page window)))
         (margin (max 0 (or pdf-view-bounding-box-margin 0)))
         (slice (list (- (nth 0 bb)
                         (/ margin 2.0))
                      (- (nth 1 bb)
                         (/ margin 1.0))
                      (+ (- (nth 2 bb) (nth 0 bb))
                         margin)
                      (+ (- (nth 3 bb) (nth 1 bb))
                         (* 4.0 margin)))))
    (apply 'pdf-view-set-slice
           (append slice (and window (list window))))))

;; Adiciona a função criada acima para o pdf-view-mode-map como "sv"
(define-key pdf-view-mode-map (kbd "sv") 'guto/pdf-view-slice-vertical)

;; Adiciona a função criada acima para o pdf-view-mode-map como "sv" dentro do evil-normal-mode
(with-eval-after-load "evil"
  (evil-define-key 'normal pdf-view-mode-map (kbd "sv") 'guto/pdf-view-slice-vertical))

;; Conserta o bug do pdf-tools ao utilizar o pacote evil (borda do buffer piscando)
(add-hook 'pdf-view-mode-hook
          (lambda ()
            (set (make-local-variable 'evil-normal-state-cursor) (list nil))
            (internal-show-cursor nil nil)))

;; Configura atalhos para movimentação de e para hyperlinks no PDF buffer
(with-eval-after-load "evil"
  (evil-define-key 'normal pdf-view-mode-map (kbd ";") 'pdf-history-backward)
  (evil-define-key 'normal pdf-view-mode-map (kbd ",") 'pdf-history-forward))

;; Salva a localização (página) do PDF para quando abrir novamente
;; A informação fica salva em ".pdf-view-restore" no mesmo diretório do Emacs "~/.emacs.d/"
(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

;; VARIÁVEL do diretório root dos arquivos do slip-box
(defvar jlf/slipbox-directory "~/Sync/Jota/Academico/Projetos/Slip-Box/"
  "Directory of slip-box files.")

;; Variável do diretório dos arquivos de referência slip-box
(defvar jlf/slipbox-refs-directory "~/Sync/Jota/Academico/Projetos/Slip-Box/Refs/"
  "Directory of slip-box ref files.")

;; Variável do diretório dos dailies do slip-box (fleeting notes do zettelkasten)
(defvar jlf/slipbox-dailies-directory "~/Sync/Jota/Academico/Projetos/Slip-Box/Dailies/"
  "Directory of slip-box dailies files.")

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename jlf/slipbox-directory))
  (org-roam-capture-templates
   '(("n" "Note File" plain "%?"
      :if-new (file+head "${slug}.org"
                         "#+TITLE: ${title}\n#+AUTHOR: %(print user-full-name)\n#+EMAIL: %(print user-mail-address)\n#+URL: %(print user-url)\n#+CREATED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+LAST_MODIFIED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+FILETAGS:\n\n* ")
      :unnarrowed t)))
  (org-roam-capture-ref-templates
   '(("r" "Roam Ref Protocol" plain "%?"
      :if-new (file+head "Refs/${slug}.org"
                         "#+TITLE: ${title}\n#+AUTHOR: %(print user-full-name)\n#+EMAIL: %(print user-mail-address)\n#+URL: %(print user-url)\n#+CREATED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+LAST_MODIFIED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+FILETAGS:\n\n* ")
      :unnarrowed t)))
  (org-roam-dailies-directory jlf/slipbox-dailies-directory)
  (org-roam-dailies-capture-templates
   '(("d" "Dailies" entry
      "* %?"
      :if-new (file+head "Dailies/%<%Y-%m-%d>.org"
                         "#+TITLE: %<%Y-%m-%d>\n\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup))


(with-eval-after-load "org-roam"

  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

  (cl-defmethod org-roam-node-filecitekey ((node org-roam-node))
    "Return the file CITE_KEY for the node."
    (org-roam-get-keyword "CITE_KEY" (org-roam-node-file node)))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (string-join (f-split dirs) "/"))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the hierarchy for the node."
    (let ((title (org-roam-node-title node))
          (olp (org-roam-node-olp node))
          (level (org-roam-node-level node))
          (filetitle (org-roam-node-filetitle node))
          (filecitekey (org-roam-node-filecitekey node)))
      (if filecitekey
          (concat
           (if (> level 0) (concat filecitekey " > "))
           (if (> level 1) (concat (string-join olp " > ") " > "))
           (if (= level 0) filecitekey title))
        (concat
         (if (> level 0) (concat filetitle " > "))
         (if (> level 1) (concat (string-join olp " > ") " > "))
         title)))))

(setq org-roam-node-display-template "${directories:10} ${hierarchy:*} ${tags:25} ${backlinkscount:6}")

(defun jlf/org-roam-node-exclude-add ()
  "Add ROAM_EXCLUDE property to node with value t."
  (interactive)
  (org-entry-put (point) "ROAM_EXCLUDE" "t"))

(advice-add 'org-noter-insert-note :after 'jlf/org-roam-node-exclude-add)

;; Função para atualizar campos em um org buffer. Usada para atualizar o #+LAST_MODIFIED
(defun jlf/org-update-field (REGEXP_FIELD NEW &optional ANYWHERE)
  "Update any field that starts at the beginning of a line in an org buffer. 
    REGEXP_FIELD is a string with regexp match to the desired field. Beware that, as it is a string, any time you use the escape character (\\) you need to insert two of them for the match to occur. For example, if you want to match the field #+LAST_MODIFIED: you need to pass #\\\\+LAST_MODIFIED: as a string to REGEXP_FIELD. 
    NEW is a string with the new value for the field. 
    If ANYWHERE is t, the match can occur anywhere inside the buffer. If it is nil or ommited, the match can only occur before the first heading."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (if (re-search-forward (concat "^" REGEXP_FIELD) (if ANYWHERE nil first-heading) t)
          (progn
            (if (looking-at-p " ")
                (forward-char)
              (insert " "))
            (delete-region (point) (line-end-position))
            (insert NEW))
        nil))))

;; Função para atualizar o campo #+LAST_MODIFIED em org buffers
(defun jlf/org-update-last-modified ()
  "Update #+LAST_MODIFIED field in org buffers."
  (when (derived-mode-p 'org-mode)
    (jlf/org-update-field "#\\+LAST_MODIFIED:" (format-time-string "[%d-%m-%Y %a %H:%M:%S]") nil)))

;; Hook para atualizar 
(add-hook 'before-save-hook 'jlf/org-update-last-modified)

;; org-roam-protocol
(require 'org-roam-protocol)

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; M-x org-roam-ui-mode

(use-package org-noter
  :custom
  (org-noter-notes-search-path (list jlf/slipbox-refs-directory))
  (org-noter-doc-split-fraction '(0.7 . 0.3))
  ;; (org-noter-insert-note-no-questions t)
  ;; (org-noter-hide-other nil)
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil))

;; Função para ajeitar o bug do visual-line-mode no org-noter
(defun zp/org-noter-visual-line-mode ()
  "Enable visual-line-mode in ‘org-noter’ notes.
Workaround to counter race conditions with the margins."
  (let ((parent (current-buffer))
        (refresh (lambda (parent)
                   (with-current-buffer parent
                     (visual-line-mode 'toggle)
                     (visual-line-mode 'toggle)))))
    (run-at-time "1 sec" nil refresh parent)
    (run-at-time "5 sec" nil refresh parent)))

(add-hook 'org-noter-notes-mode-hook #'zp/org-noter-visual-line-mode)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link)
  :custom
  ;; (org-pdftools-use-isearch-link t)
  (org-pdftools-use-freepointer-annot t))

(use-package org-noter-pdftools
  :after org-noter
  :custom
  (org-noter-pdftools-markup-pointer-color "yellow")
  (org-noter-pdftools-free-pointer-icon "Note")
  (org-noter-pdftools-free-pointer-color "yellow")
  :config
  ;; Configuração "extra" sugerida pelo próprio mantenedor do pacote
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package ivy-bibtex
  :custom
  (bibtex-completion-bibliography (list (concat jlf/slipbox-refs-directory "bibliography.bib")))
  (bibtex-completion-library-path (list jlf/slipbox-refs-directory))
  (bibtex-completion-find-note-functions '(orb-find-note-file)))

(add-to-list 'bibtex-completion-cite-commands "citeonline")

(use-package org-ref
  :after ivy-bibtex
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :custom
  (org-ref-default-bibliography (list (concat jlf/slipbox-refs-directory "bibliography.bib")))
  (org-ref-pdf-directory jlf/slipbox-refs-directory)
  (org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n")
  (org-ref-notes-directory jlf/slipbox-refs-directory)
  (org-ref-notes-function 'orb-edit-notes)
  :config
  ;; Adicionei essas funções pra deixar o org-ref na cara do ivy-bibtex
  (bibtex-completion-init) ;; primeiro precisa inicializar o ivy-bibtex
  ;; Em seguida faz esse comando pra deixar o org-ref com a cara do ivy-bibtex
  (ivy-configure 'org-ref-ivy-insert-cite-link
    :display-transformer-fn 'ivy-bibtex-display-transformer))


;; Functions to enable opening PDF with Skim when on citation
(defun jlf/bibtex-open-pdf(FILENAME)
  (interactive)
  (shell-command (concat "open -a Skim " FILENAME)))

(defun jlf/org-ref-open-skim-pdf-at-point ()
  "Open the pdf with Skim for bibtex key under point if it exists."
  (interactive)
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
       (results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (pdf-file (bibtex-completion-find-pdf key t))
       (bibtex-completion-pdf-open-function 'jlf/bibtex-open-pdf))
    (pcase (length pdf-file)
      (0
       (message "no pdf found for %s" key))
      (1
       (funcall bibtex-completion-pdf-open-function (car pdf-file)))
      (_
       (funcall bibtex-completion-pdf-open-function
	      (completing-read "pdf: " pdf-file))))))

(define-key org-mode-map (kbd "C-c p") 'jlf/org-ref-open-skim-pdf-at-point)

(use-package org-roam-bibtex
  :after org-roam
  :custom
  (orb-preformat-keywords
   '("=key=" "file" "title" "=type=" "author-or-editor" "year" "journal" "doi" "url" "keywords" "abstract"))
  :config
  (add-to-list 'org-roam-capture-templates
               '("b" "Bibliography Reference"))
  (add-to-list 'org-roam-capture-templates
               '("ba" "Article" plain
                 "%?"
                 :if-new (file+head "Refs/${=key=}.org"
                                    "#+TITLE: ${title}\n#+CITE_KEY: ${=key=}\n#+CREATED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+LAST_MODIFIED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+FILETAGS:\n\n* Info\n:PROPERTIES:\n:DOCUMENT_PATH: %(file-relative-name (orb-process-file-field \"${=key=}\") (print jlf/slipbox-refs-directory))\n:TYPE: %(capitalize \"${=type=}\")\n:AUTHOR: ${author-or-editor}\n:YEAR: ${year}\n:JOURNAL: ${journal}\n:DOI: %(if (string-equal \"${doi}\" \"\") \"---\" \"${doi}\")\n:URL: %(if (string-equal \"${url}\" \"\") \"---\" \"${url}\")\n:KEYWORDS: %(if (string-equal \"${keywords}\" \"\") \"---\" \"${keywords}\")\n%(if (string-equal \"${abstract}\" \"\") \":ABSTRACT: ---\\n\"):END:\n%(unless (string-equal \"${abstract}\" \"\") \":ABSTRACT:\\n${abstract}\\n:END:\\n\")\n* Summary\n\n* Takeaways\n- \n\n* Notes\n:PROPERTIES:\n:NOTER_DOCUMENT: %(file-relative-name (orb-process-file-field \"${=key=}\") (print jlf/slipbox-refs-directory))\n:END:\n")
                 :unnarrowed t))
  (add-to-list 'org-roam-capture-templates
               '("bb" "Book" plain
                 "%?"
                 :if-new (file+head "Refs/${=key=}.org"
                                    "#+TITLE: ${title}\n#+CITE_KEY: ${=key=}\n#+CREATED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+LAST_MODIFIED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+FILETAGS:\n\n* Info\n:PROPERTIES:\n:DOCUMENT_PATH: %(file-relative-name (orb-process-file-field \"${=key=}\") (print jlf/slipbox-refs-directory))\n:TYPE: %(capitalize \"${=type=}\")\n:AUTHOR: ${author-or-editor}\n:YEAR: ${year}\n:END:\n\n* Notes\n:PROPERTIES:\n:NOTER_DOCUMENT: %(file-relative-name (orb-process-file-field \"${=key=}\") (print jlf/slipbox-refs-directory))\n:END:\n")
                 :unnarrowed t)))


(org-roam-bibtex-mode)

(defun jlf/org-roam-add-bibliography (&optional CLIPBOARD-YANK)
  "Add bibliography entry to bibliography file.
If CLIPBOARD-YANK is non-nil, paste clipboard as the entry.
If CLIPBOARD-YANK is nil, only add the space for a new entry."
  (interactive)
  (find-file (concat jlf/slipbox-refs-directory "bibliography.bib"))
  (end-of-buffer)
  (evil-open-below 2)
  (evil-normal-state)
  (if CLIPBOARD-YANK
      (save-excursion (clipboard-yank)))
  (evil-scroll-line-to-center (line-number-at-pos)))

(defcustom org-research-keymap-prefix "C-c r"
  "The prefix for org-research key bindings."
  :type 'string
  :group 'org-research)

(defun org-research--key (key)
  (kbd (concat org-research-keymap-prefix " " key)))

(global-set-key (org-research--key "b") 'ivy-bibtex)
(global-set-key (org-research--key "t") 'org-noter)
(global-set-key (org-research--key "l") 'org-roam-node-insert)
(global-set-key (org-research--key "n") 'org-noter-insert-note)
(global-set-key (org-research--key "c") 'org-ref-insert-link)
(global-set-key (org-research--key "r") 'org-roam-buffer-display-dedicated)
(global-set-key (org-research--key "R") 'org-roam-buffer-toggle)
(global-set-key (org-research--key "f") 'org-roam-node-find)
(global-set-key (org-research--key "g") 'org-roam-graph)
(global-set-key (org-research--key "u") 'org-roam-ui-mode)
(global-set-key (org-research--key "d") 'org-roam-dailies-capture-today)
(global-set-key (org-research--key "a a") 'org-roam-alias-add)
(global-set-key (org-research--key "a r") 'org-roam-ref-add)
(global-set-key (org-research--key "a t") 'org-roam-tag-add)
(global-set-key (org-research--key "a e") 'jlf/org-roam-node-exclude-add)
(global-set-key (org-research--key "a b") (lambda () (interactive) (jlf/org-roam-add-bibliography t)))
(global-set-key (org-research--key "a B") 'jlf/org-roam-add-bibliography)

(use-package tabspaces
  ;; :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*")))

(tab-bar-mode 1)
(tabspaces-mode 1)

(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)


(defun jlf/create-new-tab-with-project ()
  (interactive)
  (let (
        (new-tab-name (read-string "Tab name: "))
        (project-dir-name (project-prompt-project-dir))
        )
    (switch-to-buffer "*scratch*")
    (tab-new)
    (dired project-dir-name)
    (tab-rename new-tab-name)
    ))

(define-key tabspaces-mode-map (kbd "C-c TAB p") #'jlf/create-new-tab-with-project)


;; Inherit the face of `doom-modeline-panel` for better appearance
(set-face-attribute 'tab-bar-tab nil :inherit 'mode-line-highlight :foreground nil :background nil)

;; Totally customize the format of the tab bar name
(defun my/tab-bar-format (tab i)
  (propertize
   (format
    (concat
     (if (eq (car tab) 'current-tab)
         "🔥 " "")
     "%s")
    (alist-get 'name tab))
   'face (list (append
                '(:foreground "#FFFFFF")
                (if (eq (car tab) 'current-tab)
                    '(:box nil)
                  '())))))

;; Replace the default tab bar function
(setq tab-bar-tab-name-format-function #'my/tab-bar-format)

(set-face-attribute 'tab-bar nil :foreground "#FFFFFF")


;; Open iTerm window in current buffer directory
(defun jlf/open-iterm-here ()
  "Open iTerm with current directory."
  (interactive)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (expand-file-name (buffer-file-name)))
               (expand-file-name default-directory))))
    (shell-command (format "open -a iTerm \"%s\"" dir))))

;; Add function to project key map
(define-key project-prefix-map (kbd "t") #'jlf/open-iterm-here)

(load-file (concat user-emacs-directory "config/org-config.el"))

;; brew install ledger
(use-package ledger-mode
  :init
  (setq ledger-clear-whole-transactions 1)
  :mode "\\.dat\\'")

;; retirar face de highlight
(with-eval-after-load "ledger"
  (set-face-attribute 'ledger-font-xact-highlight-face nil :extend nil :inherit nil))
