;; Buffer de inicialização
(add-hook 'after-init-hook (lambda () (org-agenda nil "d") (delete-other-windows)))

;; Diretório de inicialização
(setq default-directory "~/.emacs.d/")

;; Tamanho da janela de inicialização
(add-to-list 'initial-frame-alist '(height . 1.0))
(add-to-list 'initial-frame-alist '(width . 1.0))

;; Dados de usuário
(setq user-full-name "João Lucas Correia Barbosa de Farias")
(setq user-mail-address "fariasjota09@gmail.com")
(defvar user-url "https://github.com/jotafarias13")

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
          '(lambda () (setq display-line-numbers 'relative)))


;; Visual line mode sempre ativo
(global-visual-line-mode 1)

;; Indica começo-fim de parênteses
(add-hook 'after-init-hook (lambda () (show-paren-mode 1)))
;; Altera cor do parêntese e do background quando der match em um parêntese
(add-hook 'show-paren-mode-hook '(lambda () (set-face-attribute 'show-paren-match nil :foreground "Magenta" :background "#595959")))
;; Obs: foi necessário fazer dessa forma por que o show-paren-mode-hook não estava funcionando como esperado.

;; Barra de modos (inferior) minimalista
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Pacote All the icons
(use-package all-the-icons)
;; Obs: é preciso instalar as fontes do pacote (executar apenas uma vez)
;; M-x all-the-icons-install-fonts

;; Gerenciamento de arquivos de backup e autosave
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
(setq version-control t)
(setq delete-old-versions t)
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Envia comandos custom para outro arquivo
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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

;; Gerencia atualização automática dos pacotes
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "10:00"))

;; Atalhos personalizados para manipular janelas, selecionar texto e usar colar do clipboard
(global-set-key (kbd "M-<up>") 'enlarge-window) ;; aumentar a janela verticalmente com M-<up>
(global-set-key (kbd "M-<down>") 'shrink-window) ;; diminuir a janela verticalmente com M-<down>
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally) ;; aumentar a janela horizontalmente com M-<right>
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally) ;; diminuir a janela horizontalmente com M-<left>
(global-set-key (kbd "C-=") 'set-mark-command) ;; selecionar texto com C-=
(global-set-key (kbd "C-M-y") 'clipboard-yank) ;; colar do clipboard

;; Atalhos para dired, para abrir o init.el e para abrir o eshell 
(global-set-key (kbd "C-M-0") (lambda () (interactive) (find-file "~/.emacs.d/Emacs.org" nil)))
(global-set-key (kbd "C-M-1") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Pós-Graduação/UFRN/Mestrado/Dissertação/Defesa/")))
(global-set-key (kbd "C-M-2") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Projetos/C++/")))
(global-set-key (kbd "C-M-3") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Projetos/Emacs/Org/")))
(global-set-key (kbd "C-M-4") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/")))
(global-set-key (kbd "C-M-s") (lambda () (interactive) (eshell nil)))

(when (eq system-type 'darwin) ;; verifica se está no Mac
  (setq mac-command-modifier 'control)
  (setq mac-right-command-modifier 'meta))

;; Tamanho das fontes
(defvar jlf/default-font-size 150)
(defvar jlf/default-fixed-font-size 130)
(defvar jlf/default-variable-font-size 150)
(defvar jlf/monitor-font-size 190)
(defvar jlf/monitor-fixed-font-size 170)
(defvar jlf/monitor-variable-font-size 190)

;; Fontes utilizadas
;; É necessário baixar as fontes Fira Code e Inconsolata
;; As funções criadas com namespace 'sscreen' (switch-screen) possibilitam trocar os tamanhos das fontes para diferentes tipos de telas (sem alterar frame size ou modeline size)

(defvar sscreen--current-screen-type-index 0
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
(add-hook 'after-init-hook (lambda () (sscreen--change-screen-type "Default")))

;; Keybinding para chamar a função
(global-set-key (kbd "M-+") 'sscreen-toggle-screen-type)

;; Tema doom-moonlight
(use-package doom-themes
  :init (load-theme 'doom-moonlight t))

(use-package which-key
  :config
  (progn
    (which-key-setup-side-window-right-bottom)
    (which-key-mode)))

(use-package ace-window
  :bind (("C-1" . ace-window)))

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

;; Adiciona informações sobre cada comando no ivy
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Substitui comandos para funcionar melhor com ivy
(use-package counsel
  :bind (:map counsel-mode-map
  ([remap switch-to-buffer] . counsel-switch-buffer)
  ([remap dired] . counsel-dired))
  :config
  (counsel-mode 1))

;; Ferramenta de pesquisa que substitui isearch e tem integração com ivy
(use-package swiper)

;; Autocompletion in-buffer
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "<tab>") nil)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "TAB") nil)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "C-<return>") 'company-abort)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "<return>") 'company-complete-selection)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "C-j") 'company-select-next)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "C-k") 'company-select-previous)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2))

;; Melhora aparência do menu de autocompletion
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Configura a exibição de itens do dired, a funcionalidade do dwim e alocação de itens deletados
(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
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
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"))



;; Adiciona ícones para os elementos do dired
(use-package all-the-icons-dired
  :hook 
  (dired-mode . all-the-icons-dired-mode)
  (all-the-icons-dired-mode . (lambda () (setq all-the-icons-dired-monochrome nil))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "systemBlueColor")) ;; Fica melhor com o tema doom-moonlight

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

(defun jlf/olivetti-mode-setup ()
  (olivetti-mode)
  (olivetti-set-width 0.9))

(use-package olivetti
  :hook (org-mode . jlf/olivetti-mode-setup))

;; Melhora as funções de desfazer e refazer do evil
(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t)
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

  ;; Configura a navegação para funcionar quando visual-line-mode não está ativado
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "gj" 'evil-next-line)
  (evil-global-set-key 'motion "gk" 'evil-previous-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

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

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC"))

(evilem-define (kbd "SPC f") (list 'evil-repeat-find-char
                                   'evil-repeat-find-char-reverse)
               :pre-hook (save-excursion
                           (setq evil-this-type 'inclusive)
                           (call-interactively #'evil-find-char))
               :bind ((evil-cross-lines t)))

(evilem-define (kbd "SPC t") (list 'evil-repeat-find-char
                                   'evil-repeat-find-char-reverse)
               :pre-hook (save-excursion
                           (setq evil-this-type 'inclusive)
                           (call-interactively #'evil-find-char-to))
               :bind ((evil-cross-lines t)))

(use-package prescient)

(use-package ivy-prescient
  :after counsel
  :config (ivy-prescient-mode))

(use-package company-prescient
  :custom
  (company-prescient-sort-length-enable nil)
  :config
  (company-prescient-mode))

;; Altera o padrão para separação de sentenças para ser apenas um espaço
(setq sentence-end-double-space nil)

;; Diminui prompts yes/no para agilizar escolha
(fset 'yes-or-no-p 'y-or-n-p)

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

;; Utiliza $PATH do terminal
(use-package exec-path-from-shell) ; torna o PATH do shell igual do temrinal
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

(add-hook 'LaTeX-mode-hook '(lambda () (define-key LaTeX-mode-map (kbd "C-<return>") 'jlf/LaTeX-insert-item)))

;; Instalação do clangd: brew install llvm
;; Instalação do compiledb: pip install compiledb

;; Breadcrumb no topo do buffer (caminho do arquivo)
(defun jlf/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Adiciona funcionalidades de IDE para o Emacs
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . jlf/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l") 
  (setq lsp-diagnostics-provider :none)
  :hook (c++-mode . lsp)
  :config
  (lsp-enable-which-key-integration t))

;; Feature do clangd que possibilita a escolha do overload de uma função no company-box
(setq lsp-clients-clangd-args '("--completion-style=detailed" "--header-insertion=never"))

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

;; Funciona como um cliente LSP para Emacs, utilizado para escrever em LaTeX
(use-package eglot
  :hook (LaTeX-mode . eglot-ensure))

;; Auxilia o Eglot a reconhecer projetos com arquivos em diretórios distintos

;; (defvar main-tex "defesa.tex")
(defvar main-tex "projeto-pesquisa.tex")

(defun jlf/latex-root (dir)
  (when-let ((root (locate-dominating-file dir main-tex)))
    (cons 'latex-module root)))

(add-hook 'project-find-functions #'jlf/latex-root)

(cl-defmethod project-root ((project (head latex-module)))
   (cdr project))

(defvar jlf/my-workspace-alist (list)
  "List of entries in workspace.")

(add-to-list 'jlf/my-workspace-alist '("Artigo" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/Academico/Artigos/2021/EJPC/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Dissertação C++" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/Academico/Projetos/C++/pancreasArtificial/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Dissertação TeX" . (lambda () (jlf/my-workspace-find-file "~/Sync/Jota/Academico/Pós-Graduação/UFRN/Mestrado/Dissertação/Defesa/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Emacs" . (lambda () (jlf/my-workspace-find-file "~/.emacs.d/"))) t)
(add-to-list 'jlf/my-workspace-alist '("Slip-Box" . (lambda () (jlf/my-workspace-find-file jlf/slipbox-directory))) t)
(add-to-list 'jlf/my-workspace-alist '("Agenda" . (lambda () (org-agenda nil "d") (delete-other-windows))) t)
(add-to-list 'jlf/my-workspace-alist '("Org" . (lambda () (jlf/my-workspace-find-file org-directory))) t)

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
         (my-workspace (completing-read "WorkSpace: " my-workspace-list)))
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
(evil-define-key 'normal pdf-view-mode-map (kbd ";") 'pdf-history-backward)
(evil-define-key 'normal pdf-view-mode-map (kbd ",") 'pdf-history-forward)

;; Salva a localização (página) do PDF para quando abrir novamente
;; A informação fica salva em ".pdf-view-restore" no mesmo diretório do Emacs "~/.emacs.d/"
(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

;; Variável do diretório root dos arquivos do slip-box
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
  
  ;; org-roam-server
  ;; (use-package org-roam-server
  ;;   :config
  ;;   (setq org-roam-server-host "127.0.0.1"
  ;;         org-roam-server-port 8080
  ;;         org-roam-server-authenticate nil
  ;;         org-roam-server-export-inline-images t
  ;;         org-roam-server-serve-files nil
  ;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
  ;;         org-roam-server-network-poll t
  ;;         org-roam-server-network-arrows nil
  ;;         org-roam-server-network-label-truncate t
  ;;         org-roam-server-network-label-truncate-length 60
  ;;         org-roam-server-network-label-wrap-length 20))

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

(use-package org-roam-bibtex
  :after org-roam
  :load-path "~/Sync/Jota/Academico/Projetos/Emacs/org-roam-bibtex-branch-v2/org-roam-bibtex/"
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
                                    "#+TITLE: ${title}\n#+CITE_KEY: ${=key=}\n#+CREATED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+LAST_MODIFIED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+FILETAGS:\n\n* Info\n:PROPERTIES:\n:DOCUMENT_PATH: %(file-relative-name (orb-process-file-field \"${=key=}\") (print jlf/slipbox-refs-directory))\n:TYPE: %(capitalize \"${=type=}\")\n:AUTHOR: ${author-or-editor}\n:YEAR: ${year}\n:JOURNAL: ${journal}\n:DOI: %(if (string-equal \"${doi}\" \"\") \"---\" \"${doi}\")\n:URL: %(if (string-equal \"${url}\" \"\") \"---\" \"${url}\")\n:KEYWORDS: %(if (string-equal \"${keywords}\" \"\") \"---\" \"${keywords}\")\n%(if (string-equal \"${abstract}\" \"\") \":ABSTRACT: ---\\n\"):END:\n%(unless (string-equal \"${abstract}\" \"\") \":ABSTRACT:\\n${abstract}\\n:END:\\n\")\n* Notes\n:PROPERTIES:\n:NOTER_DOCUMENT: %(file-relative-name (orb-process-file-field \"${=key=}\") (print jlf/slipbox-refs-directory))\n:END:\n")
                 :unnarrowed t))
  (add-to-list 'org-roam-capture-templates
               '("bb" "Book" plain
                 "%?"
                 :if-new (file+head "Refs/${=key=}.org"
                                    "#+TITLE: ${title}\n#+CITE_KEY: ${=key=}\n#+CREATED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+LAST_MODIFIED: [%<%d-%m-%Y %a %H:%M:%S>]\n#+FILETAGS:\n\n* Info\n:PROPERTIES:\n:DOCUMENT_PATH: %(file-relative-name (orb-process-file-field \"${=key=}\") (print jlf/slipbox-refs-directory))\n:TYPE: %(capitalize \"${=type=}\")\n:AUTHOR: ${author-or-editor}\n:YEAR: ${year}\n:END:\n\n* Notes\n:PROPERTIES:\n:NOTER_DOCUMENT: %(file-relative-name (orb-process-file-field \"${=key=}\") (print jlf/slipbox-refs-directory))\n:END:\n")
                 :unnarrowed t)))


(org-roam-bibtex-mode)

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
(global-set-key (org-research--key "d") 'org-roam-dailies-capture-today)
(global-set-key (org-research--key "a a") 'org-roam-alias-add)
(global-set-key (org-research--key "a r") 'org-roam-ref-add)
(global-set-key (org-research--key "a t") 'org-roam-tag-add)
(global-set-key (org-research--key "a e") 'jlf/org-roam-node-exclude-add)

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  (persp-state-default-file "~/.emacs.d/persp-state-session")
  (persp-modestring-short t)
  :bind (("C-x b" . persp-counsel-switch-buffer))
  :config
  (persp-mode))

(add-hook 'kill-emacs-hook #'persp-state-save)

;; Congifuração das fontes e faces
(defun jlf/org-font-setup ()

  ;; Substitui os hífens das listas por pontos (bullets)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Configura as faces dos headings
  (dolist (face '((org-document-title . 1.42)
                  (org-level-1 . 1.4)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :family "Inconsolata" :weight 'regular :width 'condensed :height (cdr face)))

  ;; Configura as faces de título e keywords
  (dolist (face '((org-document-info-keyword . 1.0)
                  (org-document-info . 1.0)))
    (set-face-attribute (car face) nil :family "Inconsolata" :weight 'regular :height (cdr face)))

  ;; Assegura que o que deve ser fixed-pitch no org-mode fique dessa forma
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  (custom-theme-set-faces
   'user
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-link ((t (:foreground "linkColor" :underline t))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))))

;; Gambiarra para alterar a face org-indent já que alterar no :config gera erro
(add-hook 'org-mode-hook '(lambda () (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))))

(defun jlf/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . jlf/org-mode-setup)
  :bind 
  ("C-c t" . counsel-org-tag)
  ("C-c a" . org-agenda)
  ("C-c d" . (lambda () (interactive) (org-todo "DONE"))) 
  ("C-c w" . (lambda () (interactive) (org-todo "DONE") (org-refile))) 
  :custom
  (org-startup-folded 'content)
  (org-directory "~/Sync/Jota/Academico/Projetos/Org/")
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t) 

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        (list (format "%sTarefas.org" org-directory)))
        ;; '("~/Sync/Jota/Academico/Projetos/Emacs/Org/Tarefas.org"))
  ;; "~/Sync/Jota/Academico/Projetos/Emacs/Org/Saude.org"))
  ;; "~/Projects/Code/emacs-from-scratch/OrgFiles/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
  ;;     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; (setq org-refile-targets
  ;;   '(("~/Sync/Jota/Academico/Projetos/Emacs/Org/Arquivado.org" :maxlevel . 1)
  ;;     ("~/Sync/Jota/Academico/Projetos/Emacs/Org/Tarefas.org" :maxlevel . 1)))

  (setq org-refile-targets
        '(("Arquivado.org" :maxlevel . 1)
          ("Tarefas.org" :maxlevel . 1)))

  ;; Salva os buffers de org depois de executar o refile
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
          ;; Tags customizadas
          (:endgroup)
          ("Saúde" . ?S)
          ("Consulta" . ?c)
          ("Exame" . ?e)
          ("Trabalho" . ?T)
          ("Mestrado" . ?m)
          ("Lazer" . ?L)
          ("Emacs" . ?E)))
  ;; ("batch" . ?b)
  ;; ("note" . ?n)
  ;; ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "TODO"
                  ((org-agenda-overriding-header "TODO Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Ações NEXT de baixo esforço (low-effort)
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (jlf/org-font-setup))

(use-package org-protocol
  :ensure nil
  :init
  (server-start)
  :config
  (add-to-list 'org-capture-templates
               '("p" "Protocol"))
  (add-to-list 'org-capture-templates
               '("pb" "Bookmark" plain
                 (file+function "~/Sync/Jota/Academico/Projetos/Org/Protocol/bookmarks.org" jlf/org-protocol--capture-template-headline-target)
                 "** %(if (string-empty-p \"\%:description\") \"\%^{Title: }\" \"\%:description\")\n:LINK: %:link\n:ACCESS: [%<%d-%m-%Y %a %H:%M:%S>]\n%^{Description: }"
                 :empty-lines 1
                 :immediate-finish t) t)
  (add-to-list 'org-capture-templates
               '("pr" "Read List" entry
                 (file "~/Sync/Jota/Academico/Projetos/Org/Protocol/read_list.org")
                 "* TODO %(if (string-empty-p \"\%:description\") \"\%^{Title: }\" \"\%:description\")\n:LINK: %:link\n:ACCESS: [%<%d-%m-%Y %a %H:%M:%S>]\n%^{Description: }"
                 :empty-lines 1
                 :immediate-finish t) t))

;; Função que retorna uma lista com todos os headings level 1 de um org-buffer
(defun jlf/org--return-level-1-headings ()
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (item)
      (when (= (org-element-property :level item) 1) (org-element-property :raw-value item)))))

;; Função que retorna o buffer-point para inserção de um novo favorito
(defun jlf/org-protocol--capture-template-headline-target ()
  (let* ((options (jlf/org--return-level-1-headings))
         (heading (completing-read "Bookmark Section: " (add-to-list 'options "* ADD NEW SECTION *"))))
    (if (string-equal heading "* ADD NEW SECTION *")
        (progn
          (call-interactively
           (lambda (new-bookmark-section)
             (interactive "sNew Bookmark Section: ")
             (goto-char (point-min))
             (re-search-forward "^* Others")
             (beginning-of-line)
             (newline)
             (previous-line)
             (insert (concat "* " new-bookmark-section "\n")))))
      (progn
        (goto-char (point-min))
        (re-search-forward (concat "^* " heading))
        (org-end-of-subtree)
        (org-return)))))

;; Melhora a integração do evil com org
(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Usa bullet points em vez de hífen
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Structure templates para as linguagens mais utilizadas em org-mode
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src C"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Configura as linguagens de programação a serem compatíveis com org-babel
(with-eval-after-load 'org
(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
    (C . t)
    (latex . t)
    (python . t))))

;; Exporta automaticamente o arquivo de saída associado aos blocos de código (tangle) toda vez que o arquivo .org for salvo
(defun jlf/org-babel-tangle-config ()
(when (string-equal (buffer-file-name) "/Users/Jota/.emacs.d/Emacs.org")
;; (when (string-equal (file-name-directory (buffer-file-name))
;;                     (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jlf/org-babel-tangle-config)))
