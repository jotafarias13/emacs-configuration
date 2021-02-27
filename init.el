;; CONFIGURAÇÕES DO EMACS


;; Diretório padrão de inicialização
(setq default-directory "~/.emacs.d/")
; (setq default-directory "/Users/Jota/Sync/Jota/Acadêmico/Projetos/C_C++/"


;; Tamanho da tela inicial
(add-to-list 'initial-frame-alist '(height . 1.0))
(add-to-list 'initial-frame-alist '(width . 1.0))


;; Remover mensagem de boas-vindas
(setq inhibit-startup-message t)


;; Remover menus
(tool-bar-mode -1)
(menu-bar-mode -1)


;; Remover barra de rolagem
(scroll-bar-mode -1)


;; Numerar linhas
(global-linum-mode t)

; Desabilita line-number para alguns modos
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Visual line mode sempre ativo
(global-visual-line-mode 1)


;; Indicar começo-fim de parênteses
(show-paren-mode 1)


;; Gerenciamento de arquivos de backup e autosave
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
(setq version-control t)
(setq delete-old-versions t)
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))


;; Tamanho da fonte
;(set-face-attribute 'default nil :height 150)
    

;; Gerenciador de pacotes
(require 'package)
(setq package-enable-at-startup nil) ; desabilitar início de ativação

; Repositório MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize) ; inicializar pacotes

; Instalação do use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;; Pacote Try
 (use-package try)
 

;; Temas
;(use-package dracula-theme ; tema dracula
;  :config (load-theme 'dracula t))

(use-package vscode-dark-plus-theme ; tema vscode dark+
  :config (load-theme 'vscode-dark-plus t))

;(use-package darktooth-theme)
;  :config (load-theme 'darktooth t))

;(use-package spacemacs-theme
;  :defer t
;  :init (load-theme 'spacemacs-dark t))

;; Pacote Which-Key
(use-package which-key
  :config
  (progn
    (which-key-setup-side-window-right-bottom)
   ; (which-key-setup-side-window-right)
    (which-key-mode)))



;; Pacote All the icons
(use-package all-the-icons)
;; (all-the-icons-install-fonts)
; M-x all-the-icons-install-fonts


;; Pacote Neotree
(use-package neotree
  :config
  (progn
     (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
  :bind (("C-\\" . 'neotree-toggle)))


;; Atalhos personalizados
(global-set-key (kbd "C-<tab>") 'other-window) ; mudar de janela com C-<tab>
(global-set-key (kbd "M-<up>") 'enlarge-window) ; aumentar a janela verticalmente com M-<up>
(global-set-key (kbd "M-<down>") 'shrink-window) ; diminuir a janela verticalmente com M-<down>
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally) ; aumentar a janela horizontalmente com M-<right>
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally) ; diminuir a janela horizontalmente com M-<left>
(global-set-key (kbd "C-=") 'set-mark-command) ; selecionar texto com C-=
(global-set-key (kbd "C-M-y") 'clipboard-yank) ; colar do clipboard


;; Funções para ir para diretório dired
(global-set-key (kbd "C-M-0") (lambda () (interactive) (find-file "~/.emacs.d/init.el" nil)))
(global-set-key (kbd "C-M-1") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Pós-Graduação/UFRN/Mestrado/Dissertação/Defesa/")))
(global-set-key (kbd "C-M-2") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Projetos/C_C++/")))
(global-set-key (kbd "C-M-s") (lambda () (interactive) (eshell nil)))


;; Altera o padrão para separação de sentenças para ser apenas um espaço
(setq sentence-end-double-space nil)


;; Troca dos comandos C e M
(when (eq system-type 'darwin) ; verifica se está no Mac
  (setq mac-command-modifier 'control)
  (setq mac-right-command-modifier 'meta))
;mac-function-modifier
;mac-control-modifier
;mac-command-modifier
;mac-option-modifier
;mac-right-command-modifier
;mac-right-control-modifier
;mac-right-option-modifier



;; Pacote Ace-window
(use-package ace-window
  :bind (("C-1" . ace-window)))


;; Pacote Flycheck
; (use-package flycheck
;   :init (global-flycheck-mode t))


;; Instalar company para auto completar
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; :after lsp-mode    ; Configurações do lsp-mode
  ;; :hook (lsp-mode . company-mode)
  ;; :bind (:map company-active-map
  ;;        ("<tab>" . company-complete-selection))
        ;; (:map lsp-mode-map
        ;;  ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

; Aplica o company no auctex
(use-package company-auctex
  :init
  (company-auctex-init))

(defun my-latex-mode-setup ()
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))

(use-package company-math
  :config
  (add-hook 'TeX-mode-hook 'my-latex-mode-setup)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (setq company-tooltip-align-annotations t))

; Company box para melhor aparência na integração com lsp-mode
(use-package company-box
  :hook (company-mode . company-box-mode))


;; Pacote YASnippet
(use-package yasnippet
  :config (yas-global-mode 1))
(use-package yasnippet-snippets)
(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ; snippets pessoais


;; Pacote exec-path-from-shell
(use-package exec-path-from-shell) ; torna o PATH do shell igual do temrinal
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; Pacote Magit
(use-package magit
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))  


;; Diminuir prompts yes/no
(fset 'yes-or-no-p 'y-or-n-p)


;; Enviar comandos custom para outro arquivo
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; Pacote AUCTEX
(use-package tex
  :ensure auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
; Ativa algumas configurações do AUCTeX para melhorar a escrita do código
(setq TeX-electric-sub-and-superscript t)
(setq LaTeX-electric-left-right-brace t)
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)

(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("LaTexMk" "latexmk -pdf -pvc %s" TeX-run-TeX nil t
      :help "Run LaTexMk on file")
    TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("CleanAll" "latexmk -c; rm -f *.bbl *.brf" TeX-run-TeX nil t
      :help "Files for deletion not found")
    TeX-command-list)))

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LaTexMk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

; Altera a cor dos headings (sections) do código latex para cyan ao invés de amarelo
(with-eval-after-load 'font-latex
  (set-face-attribute 'font-latex-sectioning-5-face nil :foreground "cyan"))

(server-start); start emacs in server mode so that skim can talk to it


;; Pacote de autocompletar coisas no minibuffer
(use-package ivy
  :diminish 
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

; Deixa o ivy visualmente mais prático e interessante
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))


;; Pacote para funcionar com o ivy
(use-package counsel
  :config
  (counsel-mode 1))


;; Pacote de pesquisar que substitui isearch e usa ivy
(use-package swiper)


;; Configurações do dired
; Adiciona o hook / para pesquisar usando a funcao dired-isearch-filenames-regexp
(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd "/") 'dired-isearch-filenames-regexp)
))

; Adiciona o hook pra quando terminar a pesquisa entrar no arquivo e pesquisar de novo
(add-hook 'isearch-mode-end-hook 
	  (lambda ()
	    (when (and (eq major-mode 'dired-mode)
		       (not isearch-mode-end-hook-quit))
	      (if (file-directory-p (dired-file-name-at-point)) (progn (dired-find-alternate-file) (dired-isearch-filenames-regexp))
		(dired-find-file)
	      ))))

; Adiciona o hook para quando pesquisar ir para cimao início do buffer antes
(add-hook 'isearch-mode-hook 
	  (lambda ()
	    (when (eq major-mode 'dired-mode)
	      (beginning-of-buffer))))

; Permite usar o comando dired-find-alternate-file que mata o buffer atual no lugar de criar outro
(put 'dired-find-alternate-file 'disabled nil)

;Define o Enter como sendo essa funcao
(eval-after-load "dired"
  (lambda ()
    (define-key dired-mode-map (kbd "<return>") 
      (lambda ()
	(interactive)
	(if (file-directory-p (dired-file-name-at-point)) (progn (dired-find-alternate-file)) (dired-find-file))))))

;Próximo item e item anterior
(define-key isearch-mode-map "\C-j" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-k" 'isearch-repeat-backward)


;; Pacote all-the-icons para o dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


;; Pacote rainbow para distinguir parênteses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "linkColor")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "systemBlueColor")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "Purple"))


;; Pacote evil e configurações
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


;; Pacote evil-tex para utilizar evil keybindings voltados para tex
(use-package evil-tex)
(add-hook 'LaTeX-mode-hook #'evil-tex-mode)


;; Pacote lsp-mode (transforma emacs numa IDE)
; Primeiramente, precisa instalar o clangd para funcionar com C/C++
; brew install llvm; Colocar clangd no $PATH;
; Criar o arquivo compile_commands.json no project root directory
; Utiliza o Makefile para gerar através de: compiledb -n make 
; Instala o compiledb com pip install compiledb
; Breadcrumb no topo do buffer (caminho do arquivo)
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :hook (c++-mode . lsp)
  :config
  (lsp-enable-which-key-integration t))

; lsp-ui para o editor ficar me explicando o código à medida que navego nele
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; ; lsp-treemacs para diagnóstico, referência de símbolo e símbolos em arquivo
;; (use-package lsp-treemacs
;;   :after (lsp treemacs))

; lsp-ivy para funcionalidades de procura do ivy dentro do lsp-mode
(use-package lsp-ivy)


;; Pacote para adicionar informação extra nos buffers de ajuda
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


;; Pacote eglot para servir como lsp para latex
(use-package eglot)
(defun my-latex-root (dir)
  (when-let ((root (locate-dominating-file dir "defesa.tex")))
    (cons 'latex-module root)))

(add-hook 'project-find-functions #'my-latex-root)

(cl-defmethod project-roots ((project (head latex-module)))
  (list (cdr project)))
