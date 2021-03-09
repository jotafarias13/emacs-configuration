;; CONFIGURAÇÕES DO EMACS



;; Buffer de inicialização
(add-hook 'after-init-hook (lambda () (org-agenda nil "d") (delete-other-windows)))


;; Diretório padrão de inicialização
(setq default-directory "~/.emacs.d/")
;; (setq default-directory "/Users/Jota/Sync/Jota/Acadêmico/Projetos/C_C++/"


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
(column-number-mode)
(global-display-line-numbers-mode t)


;; Desabilita line-number para alguns modos
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Visual line mode sempre ativo
(global-visual-line-mode 1)


;; Indicar começo-fim de parênteses
(add-hook 'after-init-hook (lambda () (show-paren-mode 1)))
(add-hook 'show-paren-mode-hook '(lambda () (set-face-attribute 'show-paren-match nil :foreground "Magenta" :background "#595959")))


;; Gerenciamento de arquivos de backup e autosave
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
(setq version-control t)
(setq delete-old-versions t)
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))



;; Fontes

;; You will most likely need to adjust this font size for your system!
(defvar jlf/default-font-size 120)
(defvar jlf/default-variable-font-size 120)

(set-face-attribute 'default nil :font "Fira Code" :height jlf/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height jlf/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height jlf/default-variable-font-size :weight 'regular)   


;; Gerenciador de pacotes
(require 'package)

;; Repositório MELPA
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize) ;; inicializar pacotes

;; Instalação do use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; Pacote para gerenciar atualização automática dos pacotes
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))


;; Pacote Try
 (use-package try)
 

;; Temas
;; (use-package vscode-dark-plus-theme ; tema vscode dark+
;;   :config (load-theme 'vscode-dark-plus t))
(use-package doom-themes
  :init (load-theme 'doom-moonlight t))


;; Pacote Which-Key
(use-package which-key
  :config
  (progn
    (which-key-setup-side-window-right-bottom)
    (which-key-mode)))



;; Pacote All the icons
(use-package all-the-icons)
;; (all-the-icons-install-fonts)
;; M-x all-the-icons-install-fonts


;; Pacote para barra de modos (inferior) minimalista
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


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
(global-set-key (kbd "C-M-3") (lambda () (interactive) (dired-jump nil "~/Sync/Jota/Academico/Projetos/Emacs/Org/")))
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
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "<tab>") nil)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "TAB") nil)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "C-<return>") 'company-abort)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "<return>") 'company-complete-selection)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "C-j") 'company-select-next)))
  (add-hook 'company-mode-hook '(lambda () (define-key company-active-map (kbd "C-k") 'company-select-previous)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; (define-key company-active-map (kbd "<tab>") nil)
;; (define-key company-active-map (kbd "TAB") nil)

;; ; Aplica o company no auctex
;; (use-package company-auctex
;;   :init
;;   (company-auctex-init))

;; (defun jlf/latex-mode-setup ()
;;   (setq-local company-backends
;;               (append '((company-math-symbols-latex company-latex-commands))
;;                       company-backends)))

;; (use-package company-math
;;   :config
;;   (add-hook 'TeX-mode-hook 'jlf/latex-mode-setup)
;;   (add-to-list 'company-backends 'company-math-symbols-unicode)
;;   (setq company-tooltip-align-annotations t))

;; ; Company box para melhor aparência na integração com lsp-mode
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))


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
    '("LaTeX-Mk" "latexmk -pdf -pvc %s" TeX-run-TeX nil t
      :help "Run LaTeX-Mk on file")
    TeX-command-list)))

(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("CleanAll" "latexmk -c; rm -f *.bbl *.brf" TeX-run-TeX nil t
      :help "Files for deletion not found")
    TeX-command-list)))

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LaTeX-Mk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

; Altera a cor dos headings (sections) do código latex para cyan ao invés de amarelo
;; (with-eval-after-load 'font-latex
;;   (set-face-attribute 'font-latex-sectioning-5-face nil :foreground "cyan"))

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

(use-package all-the-icons-ivy-rich
  :after ivy
  :init (all-the-icons-ivy-rich-mode 1))
;; (all-the-icons-install-fonts)

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))


;; Pacote para funcionar com o ivy
(use-package counsel
  :bind (:map counsel-mode-map
  ([remap switch-to-buffer] . counsel-switch-buffer))
  :config
  (counsel-mode 1))


;; Pacote de pesquisar que substitui isearch e usa ivy
(use-package swiper)


(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

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


;; Pacote all-the-icons para o dired (com ajuste para funcionar com install-all-the-fonts)
(use-package all-the-icons-dired
  :hook 
  (dired-mode . all-the-icons-dired-mode)
  (all-the-icons-dired-mode . (lambda () (setq all-the-icons-dired-monochrome nil))))


;; Pacote rainbow para distinguir parênteses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "linkColor")
  ;; (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "systemBlueColor")
  ;; (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "Purple"))
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "systemBlueColor")) ;; Fica melhor com o tema doom-moonlight
 


;; Pacote para melhorar as funções de desfazer e refazer do evil
(use-package undo-fu
  :init
  (global-undo-tree-mode -1)
  :config
  (add-hook 'evil-mode-hook '(lambda () (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)))
  (add-hook 'evil-mode-hook '(lambda () (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))))


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
  (define-key evil-normal-state-map (kbd "m") (lambda () (interactive) (evil-open-below 1) (evil-normal-state)))
  (define-key evil-normal-state-map (kbd "M") (lambda () (interactive) (evil-open-above 1) (evil-normal-state)))

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


;; Pacote para aumentar a atuação do evil para keybindings
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Pacote evil-tex para utilizar evil keybindings voltados para tex
(use-package evil-tex
  :after evil)
(add-hook 'LaTeX-mode-hook #'evil-tex-mode)



;; Pacote lsp-mode (transforma emacs numa IDE)
; Primeiramente, precisa instalar o clangd para funcionar com C/C++
; brew install llvm; Colocar clangd no $PATH;
; Criar o arquivo compile_commands.json no project root directory
; Utiliza o Makefile para gerar através de: compiledb -n make 
; Instala o compiledb com pip install compiledb
; Breadcrumb no topo do buffer (caminho do arquivo)
(defun jlf/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . jlf/lsp-mode-setup)
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
(use-package eglot
  :hook (LaTeX-mode . eglot-ensure))

(defun jlf/latex-root (dir)
  (when-let ((root (locate-dominating-file dir "defesa.tex")))
    (cons 'latex-module root)))

(add-hook 'project-find-functions #'jlf/latex-root)

(cl-defmethod project-roots ((project (head latex-module)))
  (list (cdr project)))







;; Configurações org-mode

;; Melhores Font-Faces
(defun jlf/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  ;; É necessário baixar a fonte Cantarell e instalar no computador
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
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
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))





;; Configuração Básica
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
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t) 

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Sync/Jota/Academico/Projetos/Emacs/Org/Tarefas.org"))
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

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
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

    ;; Low-effort next actions
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





;; Usar bullet em vez de hífen
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))






;; Pacote evil-org para melhorar compatibilidade evil-org
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;; Structue templates para as linguagens mais utilizadas em org-mode
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src C"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))


;; Org-Babel
;; Configura as linguagens de programação a serem compatíveis com org-babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (C . t)
      (python . t))))

;; Exporta automaticamente o arquivo de saída associado aos blocos de código (tangle) toda vez que o arquivo Emacs.org for salvo
(defun jlf/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jlf/org-babel-tangle-config)))





(defun jlf/olivetti-mode-setup ()
  (olivetti-mode)
  (olivetti-set-width 0.9))

(use-package olivetti
  :hook (org-mode . jlf/olivetti-mode-setup)) 
