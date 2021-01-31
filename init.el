;; CONFIGURAÇÕES DO EMACS


;; Diretório padrão de inicialização
(setq default-directory "~/.emacs.d/")
; (setq default-directory "/Users/Jota/Sync/Jota/Acadêmico/Projetos/C_C++/"


;; Tamanho da tela inicial
(add-to-list 'initial-frame-alist '(height . 1.0))
(add-to-list 'initial-frame-alist '(width . 0.5))


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

; Adicionado automaticamente pelo MELPA


;; Pacote Try
 (use-package try
   :ensure t)


;; Temas
; (use-package molokai-theme ; tema molokai
;   :ensure t
;   :config (load-theme 'molokai))

; (use-package rebecca-theme ; tema rebecca
;   :ensure t
					;   :config (load-theme 'rebecca t))

(use-package dracula-theme ; tema dracula
  :ensure t
  :config (load-theme 'dracula t))

; (use-package vscode-dark-plus-theme ; tema vscode dark+
;   :ensure t
;   :config (load-theme 'vscode-dark-plus t))

; (load-theme 'misterioso) ; tema misterioso
; (load-them 'tango-dark) ; tema tango-dark


;; Pacote Which-Key
(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-setup-side-window-right-bottom)
   ; (which-key-setup-side-window-right)
    (which-key-mode)))


;; Pacote Auto-Complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))


;; Pacote All the icons
(use-package all-the-icons
  :ensure t)
; M-x all-the-icons-install-fonts


;; Pacote Neotree
(use-package neotree
  :ensure t
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
  :ensure t
  :bind (("C-1" . ace-window)))


;; Pacote Flycheck
; (use-package flycheck
;   :ensure t
;   :init (global-flycheck-mode t))


;; Pacote YASnippet
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))
; M-x package-install RET yasnippet-snippets


;; Pacote exec-path-from-shell
(use-package exec-path-from-shell ; torna o PATH do shell igual do temrinal
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; Pacote Magit
(use-package magit
  :ensure t)


;; Diminuir prompts yes/no
(fset 'yes-or-no-p 'y-or-n-p)


;; Enviar comandos custom para outro arquivo
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; Pacote AUCTEX
(use-package tex
  :ensure auctex)
;(use-package auctex-latexmk)
;(setq auctex-latexmk-inherit-TeX-PDF-mode t)
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
    '("latexmk" "latexmk -pdf -pvc %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("clean all" "latexmk -c; rm -f *.bbl *.brf" TeX-run-TeX nil t
      :help "Files for deletion not found")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))





