;; CONFIGURAÇÕES DO EMACS


;; Diretório padrão de inicialização
(setq default-directory "~/.emacs.d/")
; (setq default-directory "/Users/Jota/Sync/Jota/Acadêmico/Projetos/C_C++/"


;; Remover mensagem de boas-vindas
(setq inhibit-startup-message t)


;; Remover menus
(tool-bar-mode -1)
(menu-bar-mode -1)


;; Remover barra de rolagem
(scroll-bar-mode -1)


;; Numerar linhas
(global-linum-mode t)


;; Tamanho da fonte
;(set-face-attribute 'default nil :height 150)


;; Gerenciador de pacotes
(require 'package)
(setq package-enable-at-startup nil) ; desabilitar início de ativação

; Repositório MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize) ; inicializar pacotes

; Instalação do use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; Adicionado automaticamente pelo MELPA
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes '(use-package))
 '(custom-safe-themes
   '("2dff5f0b44a9e6c8644b2159414af72261e38686072e063aa66ee98a2faecf0e" "8f567db503a0d27202804f2ee51b4cd409eab5c4374f57640317b8fcbbd3e466" default))
 '(package-selected-packages
   '(yasnippet-snippets yasnippet flycheck ace-window all-the-icons neotree which-key molokai-theme try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

; (use-package dracula-theme ; tema dracula
;   :ensure t
;   :config (load-theme 'dracula))

(use-package vscode-dark-plus-theme ; tema vscode dark+
  :ensure t
  :config (load-theme 'vscode-dark-plus t))

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


;; Pacote Ace-window
(use-package ace-window
  :ensure t
  :bind (("M-<tab>" . ace-window)))


;; Pacote Flycheck
; (use-package flycheck
;   :ensure t
;   :init (global-flycheck-mode t))


;; Pacote YASnippet
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))
; M-x package-install RET yasnippet-snippets




