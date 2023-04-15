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
(add-hook 'org-mode-hook #'(lambda () (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))))

(defvar jlf/org-directory "~/Sync/Jota/Academico/Projetos/Org/"
  "My Org directory.")

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
  (org-directory jlf/org-directory)
  (org-format-latex-options '(
                              :foreground default
                              :background default
                              :scale 1.7
                              :html-foreground "Black"
                              :html-background "Transparent"
                              :html-scale 1.7
                              :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t) 
  (jlf/org-font-setup))


(with-eval-after-load 'org  
  ;; Códigos finais

  ;; Variáveis
  (defvar jlf/org-directory "~/Sync/Jota/Academico/Projetos/Org/"
    "My Org directory.")

  (defvar jlf/org-agenda-directory (concat jlf/org-directory "Agenda/")
    "My Org Agenda directory.")

  (defvar jlf/org-calendar-directory (concat jlf/org-directory "Calendar/")
    "My Org Calendar directory.")


  ;; Templates
  (add-to-list 'org-capture-templates
               `("i" "Inbox" entry (file ,(concat jlf/org-agenda-directory "inbox.org"))
                 "* TODO %^{Description: }"
                 :immediate-finish t) t)

  (add-to-list 'org-capture-templates
               `("pi" "Inbox" entry (file ,(concat jlf/org-agenda-directory "inbox.org"))
                 "* TODO [[%:link][%:description]]\n%^{Description: }"
                 :immediate-finish t) t)


  ;; Org-log
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Org agenda view
  (setq org-agenda-block-separator "\n")
  (setq org-agenda-remove-tags t)

  ;; Agenda Custom
  (setq jlf/org-agenda-main-view
        `(" " "Agenda"
          ((agenda ""
                   ((org-agenda-span 'day)
                    (org-deadline-warning-days 60)))
           (todo "TODO"
                 ((org-agenda-overriding-header "To Refile")
                  (org-agenda-files '(,(concat jlf/org-agenda-directory "inbox.org")))))
           (todo "NEXT"
                 ((org-agenda-overriding-header "In Progress")
                  (org-agenda-files '(,(concat jlf/org-agenda-directory "projects.org")
                                      ,(concat jlf/org-agenda-directory "tasks.org")
                                      ,(concat jlf/org-agenda-directory "reading.org")))))
           (todo "TODO"
                 ((org-agenda-overriding-header "Projects")
                  (org-agenda-files '(,(concat jlf/org-agenda-directory "projects.org")))))
           (todo "TODO"
                 ((org-agenda-overriding-header "Tasks")
                  (org-agenda-files '(,(concat jlf/org-agenda-directory "tasks.org")))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))))

  (setq jlf/org-agenda-week-view
        `("d" "Week"
          ((agenda ""
                   ((org-agenda-span 'week)
                    (org-deadline-warning-days 60))))))

  (setq jlf/org-agenda-reading-view
        `("r" "Reading"
          ((todo "NEXT"
                 ((org-agenda-overriding-header "Next")
                  (org-agenda-files '(,(concat jlf/org-agenda-directory "reading.org")))))
           (todo "TODO"
                 ((org-agenda-overriding-header "All")
                  (org-agenda-files '(,(concat jlf/org-agenda-directory "reading.org"))))))))

  (setq org-agenda-custom-commands nil)
  (add-to-list 'org-agenda-custom-commands `,jlf/org-agenda-main-view)
  (add-to-list 'org-agenda-custom-commands `,jlf/org-agenda-week-view)
  (add-to-list 'org-agenda-custom-commands `,jlf/org-agenda-reading-view)

  (defun jlf/org-agenda-main ()
    "Open org-agenda in main view."
    (interactive)
    (org-agenda nil " ")
    (delete-other-windows))

  (global-set-key (kbd "C-M-_") 'jlf/org-agenda-main)

  ;; Agenda Files
  (require 'find-lisp)
  (setq org-agenda-files (find-lisp-find-files jlf/org-agenda-directory "\.org$"))

  ;; Agenda Refiles
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-refile-targets
        '(("projects.org" :maxlevel . 2)
          ("tasks.org" :level . 2)
          ("reading.org" :level . 1)))

  ;; Salva os buffers de org depois de executar o refile
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-refile :after 'org-save-all-org-buffers)

  ;; Agenda Tags
  (setq org-tag-alist
        '(("emacs" . ?e)
          ("lazer" . ?l)
          ("leitura" . ?L)
          ("saúde" . ?s)
          (:newline)
          ("doutorado" . ?d)
          ("financeiro" . ?f)
          ("biblioteca" . ?b)
          ("escrita" . ?E)))

  ;; Agenda TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  ;; Função para inbox capture
  (defun jlf/org-capture-inbox ()
    "Capture inbox item."
    (interactive)
    (org-capture nil "i"))

  (global-set-key (kbd "C-c i") 'jlf/org-capture-inbox)
  ;; (define-key org-agenda-mode-map "c" 'jlf/org-capture-inbox)

  ;; Função para processar TODO
  (defun jlf/org-agenda-process-inbox-item ()
    "Process single inbox item in org-agenda.
          First, set a priority. Then, set the effort. Next, choose between
          add a timestamp, a schedule, a deadline or no timestamp. Finally,
          refile the item."
    (interactive)
    (org-agenda-priority)
    (org-agenda-set-effort)
    (let ((time (completing-read "Time: " '("Timestamp" "Schedule" "Deadline" "No Timestamp"))))
      (pcase time
        ("Timestamp"
         (call-interactively 'org-time-stamp))
        ("Schedule"
         (call-interactively 'org-agenda-schedule))
        ("Deadline"
         (call-interactively 'org-agenda-deadline))
        ("No Timestamp"
         ())
        (_
         (message "Invalid input. Using 'No Timestamp' option."))))
    (org-agenda-refile))

  ;; (define-key org-agenda-mode-map "P" 'jlf/org-agenda-process-inbox-item)
  )

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
               '("pr" "Read List" plain
                 (file+function "~/Sync/Jota/Academico/Projetos/Org/Protocol/read_list.org" jlf/org-protocol--capture-template-headline-target)
                 "** TODO %(if (string-empty-p \"\%:description\") \"\%^{Title: }\" \"\%:description\")\n:LINK: %:link\n:ACCESS: [%<%d-%m-%Y %a %H:%M:%S>]\n%^{Description: }"
                 :empty-lines 1
                 :immediate-finish t) t))

;; Função que retorna uma lista com todos os headings level N de um org-buffer
(defun jlf/org--return-level-n-headings (N)
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (item)
      (when (= (org-element-property :level item) N) (org-element-property :raw-value item)))))

;; Função que retorna o buffer-point para inserção de um novo heading
(defun jlf/org-protocol--capture-template-headline-target ()
  (let* ((options (jlf/org--return-level-n-headings 1))
         (heading (completing-read "Section: " (add-to-list 'options "* ADD NEW SECTION *"))))
    (if (string-equal heading "* ADD NEW SECTION *")
        (call-interactively
         (lambda (new-section)
           (interactive "sNew Section: ")
           (goto-char (point-min))
           (if (re-search-forward "^* Others" nil t)
               (progn
                 (beginning-of-line)
                 (newline)
                 (previous-line))
             (progn
               (goto-char (point-max))
               (evil-open-below 2)
               (evil-normal-state)))
           (insert (concat "* " new-section "\n"))))
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
  ;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
  ;; (org-bullets-bullet-list '("❃" "❀" "✿" "❁" "✾" "※" "✤" "✥" "❖")))
  (org-bullets-bullet-list '("◉" "‣" "◦" "*" "●")))

;; Structure templates para as linguagens mais utilizadas em org-mode
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src C"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))
