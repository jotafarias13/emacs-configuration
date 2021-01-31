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

; Adicionado automaticamente pelo MELPA


;; Pacote Try
 (use-package try)
 

;; Temas
; (use-package molokai-theme ; tema molokai
;   :config (load-theme 'molokai))

; (use-package rebecca-theme ; tema rebecca
;   :config (load-theme 'rebecca t))

(use-package dracula-theme ; tema dracula
  :config (load-theme 'dracula t))

; (use-package vscode-dark-plus-theme ; tema vscode dark+
;   :config (load-theme 'vscode-dark-plus t))

; (load-theme 'misterioso) ; tema misterioso
; (load-them 'tango-dark) ; tema tango-dark


;; Pacote Which-Key
(use-package which-key
  :config
  (progn
    (which-key-setup-side-window-right-bottom)
   ; (which-key-setup-side-window-right)
    (which-key-mode)))


;; Pacote Auto-Complete
(use-package auto-complete
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))


;; Pacote All the icons
(use-package all-the-icons)
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
(global-set-key (kbd "C-M-1") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Acadêmico/Pós-Graduação/UFRN/Mestrado/Dissertação/Defesa_emacs/")))
(global-set-key (kbd "C-M-2") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Acadêmico/Projetos/C_C++/")))


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


;; Pacote YASnippet
(use-package yasnippet
  :config (yas-global-mode 1))
(use-package yasnippet-snippets)


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

(server-start); start emacs in server mode so that skim can talk to it


;;Adiciona o comando de latexmk pra o auctex
;(use-package auctex-latexmk
;  :config
;  (auctex-latexmk-setup)
;  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


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

;; Pacote de pesquisar que substitui isearch e usa ivy
(use-package swiper)


;; Configurações do dired
; Impede o dired de criar buffers adicionais
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
(put 'dired-find-alternate-file 'disabled nil)

; Adiciona o hook / para pesquisar usando a função dired-isearch-filenames-regexp
(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd "/") 'dired-isearch-filenames-regexp)))

; Adiciona o hook para abrir o dired pesquisando
;(add-hook 'dired-mode-hook
;  (lambda () (dired-isearch-filenames-regexp)))

; Adiciona o hook pra quando terminar a pesquisa entrar no arquivo e pesquisar denovo
(add-hook 'isearch-mode-end-hook 
  (lambda ()
    (when (and (eq major-mode 'dired-mode)
      (not isearch-mode-end-hook-quit))
        (dired-find-file)
        (dired-isearch-filenames-regexp))))

; Adiciona o hook para quando pesquisar ir para cima no buffer antes
(add-hook 'isearch-mode-hook 
  (lambda ()
    (when (eq major-mode 'dired-mode)
      (beginning-of-buffer))))

