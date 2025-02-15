;; Diretório de inicialização
(setq default-directory "~/.emacs.d/")
;; (setq default-directory "~/")

;; Tamanho da janela de inicialização
(add-to-list 'initial-frame-alist '(height . 1.0))
(add-to-list 'initial-frame-alist '(width . 1.0))

;; Dados de usuário
(setq user-full-name "João Lucas Correia Barbosa de Farias")
(setq user-mail-address "fariasjota09@gmail.com")
(defvar user-url "https://github.com/jotafarias13")

;; Inicialização do gerenciador de pacotes padrão
(require 'package)

;; Repositórios 
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Inicialização de pacotes
(package-initialize)

;; Instalação do use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Coloca ":ensure t" em todos os pacotes
(require 'use-package)
(setq use-package-always-ensure t)

;; Remove de mensagem de boas-vindas
(setq inhibit-startup-message t)

;; Remove menus
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Remove barra de rolagem
(scroll-bar-mode -1)

;; Numerar linhas e colunas
(column-number-mode)
(global-display-line-numbers-mode t)

;; Desabilita line-number para alguns modos
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                pdf-view-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Configura a numeração das linhas para serem relativas a linha atual
(add-hook 'display-line-numbers-mode-hook
          #'(lambda () (setq display-line-numbers 'relative)))


;; Visual line mode sempre ativo
(global-visual-line-mode 1)

;; Indica começo-fim de parênteses
(add-hook 'after-init-hook (lambda () (show-paren-mode 1)))
;; Altera cor do parêntese e do background quando der match em um parêntese
(add-hook 'show-paren-mode-hook #'(lambda () (set-face-attribute 'show-paren-match nil :foreground "Magenta" :background "#595959")))
;; Obs: foi necessário fazer dessa forma por que o show-paren-mode-hook não estava funcionando como esperado.

;; Barra de modos (inferior) minimalista
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Pacote All the icons
(use-package all-the-icons)
;; Obs: é preciso instalar as fontes do pacote (executar apenas uma vez)
;; M-x all-the-icons-install-fonts

;; Altera o padrão para separação de sentenças para ser apenas um espaço
(setq sentence-end-double-space nil)

;; Diminui prompts yes/no para agilizar escolha
(fset 'yes-or-no-p 'y-or-n-p)

;; Mostra histórico de opções no minibuffer usando M-p
(setq history-length 25)
(setq savehist-mode 1)

;; Gerenciamento de arquivos de backup e autosave
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
(setq version-control t)
(setq delete-old-versions t)
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Envia comandos custom para outro arquivo
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Atalhos personalizados para manipular janelas, selecionar texto e usar colar do clipboard
(global-set-key (kbd "M-<up>") 'enlarge-window) ;; aumentar a janela verticalmente com M-<up>
(global-set-key (kbd "M-<down>") 'shrink-window) ;; diminuir a janela verticalmente com M-<down>
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally) ;; aumentar a janela horizontalmente com M-<right>
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally) ;; diminuir a janela horizontalmente com M-<left>
;; (global-set-key (kbd "C-=") 'set-mark-command) ;; selecionar texto com C-=
(global-set-key (kbd "C-M-y") 'clipboard-yank) ;; colar do clipboard

;; Atalhos para dired, para abrir o init.el e para abrir o eshell 
(global-set-key (kbd "C-M-0") (lambda () (interactive) (find-file "~/.emacs.d/Emacs.org" nil)))
;; (global-set-key (kbd "C-M-1") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Pós-Graduação/UFRN/Mestrado/Dissertação/Defesa/")))
;; (global-set-key (kbd "C-M-2") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Projetos/C++/")))
;; (global-set-key (kbd "C-M-3") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Projetos/Org/")))
(global-set-key (kbd "C-M-4") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/")))
(global-set-key (kbd "C-M-s") (lambda () (interactive) (eshell nil)))
(global-set-key (kbd "C-x C-r") 'eval-region)

(when (eq system-type 'darwin) ;; verifica se está no Mac
 (setq mac-command-modifier 'control)
 (setq mac-right-command-modifier 'meta))

;; ;; Tema doom-moonlight
;; (use-package doom-themes)
;; :init (load-theme 'doom-moonlight t)

;; (use-package dracula-theme)
;; ;; (load-theme 'doom-dracula t)
;; (load-theme 'dracula t)

(use-package vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))

;; Remove the border around the TODO word on org-mode files
(setq vscode-dark-plus-box-org-todo nil)

;; Do not set different heights for some org faces
(setq vscode-dark-plus-scale-org-faces nil)

;; Avoid inverting hl-todo face
(setq vscode-dark-plus-invert-hl-todo nil)

;; Configure current line highlighting style (works best with Emacs 28 or newer)
(setq vscode-dark-plus-render-line-highlight 'line)

;; Tamanho das fontes
(defvar jlf/default-font-size 150)
(defvar jlf/default-fixed-font-size 130)
(defvar jlf/default-variable-font-size 150)
(defvar jlf/monitor-font-size 230)
(defvar jlf/monitor-fixed-font-size 210)
(defvar jlf/monitor-variable-font-size 230)

;; Fontes utilizadas
;; É necessário baixar as fontes Fira Code e Inconsolata
;; As funções criadas com namespace 'sscreen' (switch-screen) possibilitam trocar os tamanhos das fontes para diferentes tipos de telas (sem alterar frame size ou modeline size)

(defvar sscreen--current-screen-type-index 1
  "Index of the current screen type according to sscreen--screen-types.")

(defvar sscreen-screen-types '("Default" "Monitor")
  "All screen types available for user setup.")

(defun sscreen-switch-screen-type ()
  "Switches screen type changing font sizes accordingly."
  (interactive)
  (let* ((frame-inhibit-implied-resize t)
         (screen-type-list (append sscreen-screen-types '("Custom")))
         (screen-type (completing-read "Screen " screen-type-list)))
    (pcase screen-type
      ("Monitor" 
       (progn
         (set-face-attribute 'default nil :family "Inconsolata" :height jlf/monitor-font-size)
         (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height jlf/monitor-fixed-font-size)
         (set-face-attribute 'variable-pitch nil :family "Inconsolata" :height jlf/monitor-variable-font-size :weight 'regular)))
      ("Custom" 
       (call-interactively
        (lambda (default-font-size fixed-font-size variable-font-size)
          (interactive "nDefault Font Size: \nnFixed Font Size: \nnVariable Font Size: ")
          (set-face-attribute 'default nil :family "Inconsolata" :height default-font-size)
          (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height fixed-font-size)
          (set-face-attribute 'variable-pitch nil :family "Inconsolata" :height variable-font-size :weight 'regular))))
      (_ 
       (progn
         (set-face-attribute 'default nil :family "Inconsolata" :height jlf/default-font-size)
         (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height jlf/default-fixed-font-size)
         (set-face-attribute 'variable-pitch nil :family "Inconsolata" :height jlf/default-variable-font-size :weight 'regular))))) 
  (doom-modeline-refresh-font-width-cache)) 

(defun sscreen--change-screen-type (screen-type)
  "Updates font sizes according to screen-type."
  (let ((frame-inhibit-implied-resize t)) 
    (pcase screen-type
      ("Monitor" 
       (progn
         (set-face-attribute 'default nil :family "Inconsolata" :height jlf/monitor-font-size)
         (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height jlf/monitor-fixed-font-size)
         (set-face-attribute 'variable-pitch nil :family "Inconsolata" :height jlf/monitor-variable-font-size :weight 'regular)))
      ("Custom" 
       (call-interactively
        (lambda (default-font-size fixed-font-size variable-font-size)
          (interactive "nDefault Font Size: \nnFixed Font Size: \nnVariable Font Size: ")
          (set-face-attribute 'default nil :family "Inconsolata" :height default-font-size)
          (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height fixed-font-size)
          (set-face-attribute 'variable-pitch nil :family "Inconsolata" :height variable-font-size :weight 'regular))))
      (_ 
       (progn
         (set-face-attribute 'default nil :family "Inconsolata" :height jlf/default-font-size)
         (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height jlf/default-fixed-font-size)
         (set-face-attribute 'variable-pitch nil :family "Inconsolata" :height jlf/default-variable-font-size :weight 'regular))))) 
  (doom-modeline-refresh-font-width-cache))

(defun sscreen-toggle-screen-type ()
  "Updates the index of the current screen type to the next value in sscreen-screen-types and calls sscreen--change-screen-type to change the font sizes accordingly."
  (interactive)
  (setq sscreen--current-screen-type-index (+ sscreen--current-screen-type-index 1))
  (if (>= sscreen--current-screen-type-index (length sscreen-screen-types))
      (setq sscreen--current-screen-type-index 0))
  (let ((screen-type (nth sscreen--current-screen-type-index sscreen-screen-types)))
    (sscreen--change-screen-type screen-type)))

;; Inicializar o emacs com o screen type "Default"
(add-hook 'after-init-hook (lambda () (sscreen--change-screen-type "Monitor")))

;; Keybinding para chamar a função
(global-set-key (kbd "M-+") 'sscreen-toggle-screen-type)

;; Need to download fira-code-symbols
;; https://github.com/jming422/fira-code-mode
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x" "lambda" "or" "and")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

;; Configuração do ivy (autocompletar no minibuffer)
(use-package ivy
  :diminish 
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-RET" . ivy-immediate-done)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Exibe ícones para todos os buffer no ivy
(use-package all-the-icons-ivy-rich
  :after ivy
  :init (all-the-icons-ivy-rich-mode 1))

;; Substitui comandos para funcionar melhor com ivy
(use-package counsel
  :bind (:map counsel-mode-map
              ;; ([remap switch-to-buffer] . counsel-switch-buffer)
              ([remap dired] . counsel-dired))
  :config
  (counsel-mode 1))

;; Adiciona informações sobre cada comando no ivy
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Ferramenta de pesquisa que substitui isearch e tem integração com ivy
(use-package swiper)

;; Extra config
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer) 
(global-set-key (kbd "C-x f") 'counsel-find-file) 
(global-set-key (kbd "C-x C-d") 'counsel-dired) 
(global-set-key (kbd "C-x C-k") 'all-the-icons-ivy-rich-kill-buffer)

;; Configura a exibição de itens do dired, a funcionalidade do dwim e alocação de itens deletados
(use-package dired
  :ensure nil
  :bind (
         ("C-x C-j" . dired-jump)
         ("C-=" . dired-create-empty-file))
  :custom
  (dired-listing-switches "-agho --group-directories-first")

  (dired-dwim-target t) ;; quando tem dois dired abertos, usa o segundo como path pra comandos do primeiro
  (delete-by-moving-to-trash t)) ;; move os itens deletados para o lixo do computador

;; Configura a manutenção de um único buffer do dired quando se abre arquivos ou diretórios
(use-package dired-single
  :after evil-collection
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))  ;; utiliza 'h' e 'l' para subir/descer na raiz de diretórios

;; Configurações adicionais do dired-single (diretamente do repositório do pacote)
(defun my-dired-init ()
  "Remaps some dired functions to use dired-single functions.\nBunch of stuff to run for dired, either immediately or when it's
         loaded."
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))


;; Configura 'H' para esconder/exibir dotfiles nos itens do diretório
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Configura o swiper para pesquisa no dired através do '/' 
(defun guto/dired-swiper ()
  "teste"
  (interactive)
  (swiper)
  (if (file-directory-p (dired-file-name-at-point))
      (progn
        (dired-single-buffer)
        (guto/dired-swiper))
    (dired-single-buffer)))

(with-eval-after-load "evil"
  (evil-define-key 'normal dired-mode-map (kbd "/") 'guto/dired-swiper)
  (evil-define-key 'normal dired-mode-map (kbd "SPC") 'dired-view-file))

;; ls do Mac não suporta a flag --dired
;; Instala o coreutils pelo homebrew
;; Coloca o path pro executável na variável 'insert-directory-program'
;; If it is on Apple Silicon (Mac-mini), use different path
(when (string= system-type "darwin")
  (if (string= emacs-build-system "Mac-mini")
      (setq dired-use-ls-dired t
            insert-directory-program "/opt/homebrew/bin/gls")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls")))


;; Adiciona ícones para os elementos do dired
(use-package all-the-icons-dired
  :hook 
  (dired-mode . all-the-icons-dired-mode)
  (all-the-icons-dired-mode . (lambda () (setq all-the-icons-dired-monochrome nil))))


;; Open pdf files using Skim from inside dired
(defun jlf/dired-open-pdf()
  (interactive)
  (dired-do-shell-command "open -a Skim" nil (dired-get-marked-files)))

(define-key dired-mode-map (kbd "C-c p") 'jlf/dired-open-pdf)

;; Melhora as funções de desfazer e refazer do evil
(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

;; Configura o evil-mode para simular o Vim no Emacs
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "m") (lambda () (interactive) (evil-open-below 1) (evil-normal-state)))
  (define-key evil-normal-state-map (kbd "M") (lambda () (interactive) (evil-open-above 1) (evil-normal-state)))
  (define-key evil-normal-state-map (kbd "g r") 'revert-buffer)
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "Z") (lambda () (interactive) (evil-force-normal-state) (evil-append-line 1)))

  ;; Atalhos para acessar teclas que precisam de shift
  ;; (define-key evil-insert-state-map (kbd "C--") (lambda () (interactive) (insert "_")))
  ;; (define-key evil-insert-state-map (kbd "C-=") (lambda () (interactive) (insert "+")))
  ;; (define-key evil-insert-state-map (kbd "C-[") (lambda () (interactive) (insert "{")))
  ;; (define-key evil-insert-state-map (kbd "C-]") (lambda () (interactive) (insert "}")))
  ;; (define-key evil-insert-state-map (kbd "C-;") (lambda () (interactive) (insert ":")))
  ;; (define-key evil-insert-state-map (kbd "C-'") (lambda () (interactive) (insert "\"")))
  ;; (define-key evil-insert-state-map (kbd "C-/") (lambda () (interactive) (insert "?")))
  ;; (define-key evil-insert-state-map (kbd "C-,") (lambda () (interactive) (insert "<")))
  ;; (define-key evil-insert-state-map (kbd "C-.") (lambda () (interactive) (insert ">")))
  ;; (define-key evil-insert-state-map (kbd "C-9") (lambda () (interactive) (insert "(")))
  ;; (define-key evil-insert-state-map (kbd "C-0") (lambda () (interactive) (insert ")")))

  ;; Configura a navegação para funcionar quando visual-line-mode não está ativado
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "gj" 'evil-next-line)
  (evil-global-set-key 'motion "gk" 'evil-previous-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; setup evil in minibuffer
(setq evil-want-minibuffer t)
(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (define-key evil-insert-state-local-map (kbd "C-j") 'next-line)
	    (define-key evil-insert-state-local-map (kbd "C-k") 'previous-line)
	    (define-key evil-insert-state-local-map (kbd "RET") 'exit-minibuffer)))

;; Aumenta a atuação dos keybindings do evil
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-company-use-tng nil)   ;; evita o bug de completion de funções do clangd
  :config
  (evil-collection-init))

;; Emula a ação surround do vim
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Adiciona "linha" como um text-obj (w,W,b,B etc)
(use-package evil-textobj-line)

;; Adiciona o comandos "gc" para comentar como uma ação (d,c,y etc)
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; Destaca a parte do texto onde um comando foi efetuado
(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-exchange)
(setq evil-exchange-key (kbd "gz"))
(setq evil-exchange-cancel-key (kbd "gZ"))
(evil-exchange-install)

;; Incrementa o número em uma dada linha
(use-package evil-numbers)
(with-eval-after-load "evil"
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; Programming languages compatible with org-babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (latex . t)
     (shell . t)
     (python . t))))

(defvar emacs-org-filename "Emacs.org")
(defvar emacs-org-filepath (expand-file-name (concat user-emacs-directory emacs-org-filename)))
(defvar emacs-config-dir-name "config/")
(defvar emacs-config-dir-path (expand-file-name (concat user-emacs-directory emacs-config-dir-name)))

(defvar emacs-config-basic-name "basic-config.org")
(defvar emacs-config-basic-path (concat emacs-config-dir-path emacs-config-basic-name))

(defvar emacs-config-python-name "python-config.org")
(defvar emacs-config-python-path (concat emacs-config-dir-path emacs-config-python-name))

(defvar emacs-config-rust-name "rust-config.org")
(defvar emacs-config-rust-path (concat emacs-config-dir-path emacs-config-rust-name))

(defvar emacs-config-org-name "org-config.org")
(defvar emacs-config-org-path (concat emacs-config-dir-path emacs-config-org-name))

(defvar emacs-config-files-list (list emacs-org-filepath
                                    emacs-config-basic-path
                                    emacs-config-python-path
                                    emacs-config-rust-path
                                    emacs-config-org-path))

;; Automatically calls org-babel-tangle everytime a org configuration file is saved, creating the config .el file
(defun jlf/org-babel-tangle-config ()
  (when (member (buffer-file-name) emacs-config-files-list)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jlf/org-babel-tangle-config))))
