;;; init.el --- My emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


;; 载入工具函数
(setq xy_utility-file
      (expand-file-name "utility.el" user-emacs-directory))
(when (file-exists-p xy_utility-file)
  (load xy_utility-file))


;; 统一实用使用 unix 风格的换行与 utf 编码。
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)


;; 将默认启动目录设置为配置文件所在目录，方便修改配置文件。
(setq default-directory user-emacs-directory)


;; 显示行号。
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;; 显示 80 字符列宽线。
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))


;; 禁止光标闪烁。
(blink-cursor-mode 0)


;; 自动更新文件的修改
(global-auto-revert-mode 1)


;; 设置系统时间为英文格式，避免 org mode 在插入时间的时候产生乱码。
(setq system-time-locale "C")


;; 避免 Emacs 产生备份文件与锁定文件。
(setq make-backup-files nil)
(setq create-lockfiles nil)


;; 设置一个清爽的界面。
(setq inhibit-splash-screen t)
(setq initial-scratch-message
      ";; Stop thinking with your fingers.")
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))


;; 让 Emacs 彻底安静。
(setq visible-bell 0)
(setq ring-bell-function 'ignore)


;; 禁止使用 "C-SPC"。
(global-unset-key (kbd "C-SPC"))


;; 默认不使用 TAB。
(setq-default indent-tabs-mode nil)


;; 用 y 和 n 替代 yes 和 no 的输入。
(defalias 'yes-or-no-p 'y-or-n-p)


;; 设置零时配置文件。
(setq custom-file
      (expand-file-name "xy_custom.el" user-emacs-directory))


;; 让屏幕的鼠标滑动更加顺滑。
(setq mouse-wheel-scroll-amount
      '(1                                ; 一次只滚动 1 行
        ((shift) . 2)                    ; 按住 Shift 滚动 2 行
        ((control). 3))                  ; 按住 Ctrl 滚动 3 行
      mouse-wheel-progressive-speed nil) ; 滚动过程不加速


;; 启用文本拖拽，就像 Word 一样。这样在写作的时候更加友好。
(setq  mouse-drag-and-drop-region t
       mouse-drag-and-drop-region-cut-when-buffers-differ t)


;; 保留最近打开文件的信息。
(require 'recentf)
(add-to-list 'recentf-exclude "recentf")
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 99)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)


;; 启用 org mode 做日常任务管理与速记
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-enforce-todo-dependencies t) ;; 使得任务之间彼此依赖
(setq org-log-into-drawer t) ;; 将任务日记放在 drawer 里
(setq org-log-done 'time) ;; 记录任务完成的时间
(setq org-agenda-span 'day) ;; 默认查看当天的任务
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t) ;; agenda 不显示已完成工作


;; 配置软件包安装。
(require 'package)

(setq package-user-dir
      (expand-file-name
       (format "xy_elpa-%s.%s"
               emacs-major-version
               emacs-minor-version)
       user-emacs-directory))

(setq package-archives
      `(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


;; 安装 use-package 以及一些除去快捷键外无需额外初始配置的插件。快捷键
;; 的配置统一由 general 插件配置。
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      '(color-theme-sanityinc-tomorrow
        use-package bind-key diminish
        undo-tree rainbow-mode expand-region crux general
        multiple-cursors swiper avy smex find-file-in-project
        magit))


;; 配置 use-package
(setq use-package-enable-imenu-support t) ;; 让 imenu 支持 use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; 使用 :diminish
(require 'bind-key)                ;; 使用 :bind
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; 设置主题。
(use-package spaceline
  :init
  (advice-add 'load-theme :after #'spaceline-emacs-theme)
  (defun light ()
    (interactive)
    (load-theme 'sanityinc-tomorrow-day t))
  (defun dark ()
    (interactive)
    (load-theme 'sanityinc-tomorrow-bright t))
  (when window-system
    (setq custom-safe-themes t)
    (load-theme 'sanityinc-tomorrow-bright t)))


;; 配置 vim 风格的快捷键。
(use-package evil
  :init
  (require 'evil)
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))


(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))


;; 没有弄明白这个插件的意义。
;; (use-package evil-matchit
;;   :ensure t
;;   :init
;;   (global-evil-matchit-mode 1))


(use-package find-file-in-project
  :init
  (setq ffip-use-rust-fd t))


;; 提示快捷键。
(use-package which-key
  :diminish
  :init
  (which-key-mode))


;; 代码补全。
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :init
  (eval-after-load 'company
    '(define-key company-active-map
       (kbd "C-c h") #'company-quickhelp-manual-begin))
  (company-quickhelp-mode))


;; 语法检查。
(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :after flycheck
  :init
   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;; 在切换窗口的时候让光标更加显眼。
(use-package beacon
  :init
  (setq beacon-size 10)
  (beacon-mode 1))


;; 在保存的时候自动清理。
(use-package whitespace-cleanup-mode
  :diminish
  :init
  (add-hook 'after-init-hook 'global-whitespace-cleanup-mode))


;; 编写 markdown 文件。
(use-package markdown-mode
  :after whitespace-cleanup-mode
  :init
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))
  :config
  (require 'org))


;; 配置写作的模式
(use-package olivetti
  :after markdown-mode
  :ensure auctex
  :init
  (defun znh/disable-word-wrap (&rest args)
    "Disable word wrap.  ARGS."
    (setq word-wrap nil))
  (advice-add 'olivetti-mode :after #'znh/disable-word-wrap)
  (add-hook 'markdown-mode-hook
            #'olivetti-mode)
  (add-hook 'LaTeX-mode-hook
            #'olivetti-mode)
  (add-hook 'org-mode-hook
            #'olivetti-mode)
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (add-to-list 'TeX-command-list
                           '("XeLaTeX" "%`xelatex%(mode)%' %t"
                             TeX-run-TeX nil t))
              (setq TeX-command-default "XeLaTeX")
              (setq TeX-save-query nil )
              (setq TeX-show-compilation t))))


(use-package deft
  :init
  (setq deft-new-file-format "%Y-%m-%dT%H%M"
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename nil
        deft-org-mode-title-prefix t
        deft-markdown-mode-title-level 1
        deft-default-extension "markdown"
        deft-extensions '("markdown" "org" "md")
        deft-auto-save-interval 60)
  (defcustom znh/notes_box "~/sliptex.xy.org/source/"
    "My sliptex directory."
    :type 'directory
    :group 'znh)
  (defun znh/notenum ()
    (interactive)
    (kill-new (file-name-base (buffer-file-name (current-buffer)))))
  (defun znh/get-note ()
    (interactive)
    (let* ((notes (directory-files
                   znh/notes_box nil
                   "^[0-9]\\{4\\}.*\.org$"))
           (nums (length notes))
           (ind (random nums)))
      (find-file (expand-file-name (nth ind notes) znh/notes_box))))
  (add-hook 'after-init-hook
            (lambda ()
              (when (file-exists-p znh/notes_box)
                (setq deft-directory znh/notes_box)))))


(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))


(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line))


(use-package counsel
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x))


(use-package smartparens
  :diminish
  :init
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))


(use-package highlight-escape-sequences
  :init
  (add-hook 'after-init-hook 'hes-mode))


;; 配置 R 语言编程环境。
(use-package ess
  :init
  (setq ess-use-flymake nil)
  (with-eval-after-load "ess-r-mode"
    (define-key ess-r-mode-map ";" #'ess-insert-assign)
    (define-key inferior-ess-r-mode-map ";" #'ess-insert-assign)))


(use-package highlight-parentheses
  :diminish
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))


(use-package anzu
  :diminish
  :init
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))


(use-package super-save
  :diminish
  :config
  (setq auto-save-default nil)
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 60)
  (add-to-list 'super-save-triggers 'next-line)
  (super-save-mode +1))


(use-package slime
  :init
  (setq common-lisp-hyperspec-root
        (concat "file://"
                (expand-file-name "HyperSpec/" user-emacs-directory)))
  (with-eval-after-load 'slime
    (when (executable-find "sbcl")
      (add-to-list 'slime-lisp-implementations
                   '(sbcl ("sbcl") :coding-system utf-8-unix)))
    (when (executable-find "ccl")
      (add-to-list 'slime-lisp-implementations
                   '(ccl ("ccl") :coding-system utf-8-unix)))))


;; pip install ipython jedi rope autopep8 yapf black flake8
(use-package elpy
  :init
  (with-eval-after-load 'python
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")
    (setq elpy-rpc-virtualenv-path 'current)
    (setq elpy-rpc-python-command "python")
    (setq flycheck-python-pycompile-executable elpy-rpc-python-command)
    (with-eval-after-load 'elpy
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
    (elpy-enable)))


(use-package js2-mode
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-minor-mode)))


(use-package js-comint
  :init
  (add-hook
   'js-mode-hook
   (lambda ()
     (define-key js-mode-map (kbd "C-x C-e") 'js-send-last-sexp)
     (define-key js-mode-map (kbd "C-c C-c") 'js-send-buffer)
     (define-key js-mode-map (kbd "C-c C-b") 'js-send-buffer))))


(use-package exec-path-from-shell
  :init
  (with-eval-after-load 'exec-path-from-shell
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                   "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var)))
  (when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))


(use-package undo-tree
  :diminish
  :init
  (global-undo-tree-mode))


(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package sml-mode)

;; Ruby
(use-package inf-ruby)
(use-package rspec-mode)
(use-package ruby-compilation)
(use-package ruby-hash-syntax)

(use-package robe
  :init
  (add-hook 'ruby-mode-hook 'robe-mode))
(use-package yard-mode
  :init
  (add-hook 'ruby-mode-hook 'yard-mode))

(use-package maxima
  :init
  (add-hook 'maxima-mode-hook #'maxima-hook-function)
  (add-hook 'maxima-inferior-mode-hook #'maxima-hook-function)
  (setq
   org-format-latex-options
   (plist-put org-format-latex-options :scale 2.0)
   maxima-display-maxima-buffer nil)
  :mode ("\\.mac\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode))


(use-package citre
  :init
  (require 'citre-config))

(use-package csv-mode)


(use-package imenu-list
  :init
  (setq imenu-list-focus-after-activation t) ;; 启动后自动跳转到 imenu
  (setq imenu-list-position 'left))

;; 设置一些命令的快捷方式。
(defalias 'rc 'revert-buffer-with-coding-system)
(defalias 'sc 'set-buffer-file-coding-system)
(defalias 'wl 'evil-window-move-far-right)
(defalias 'wh 'evil-window-move-far-left)
(defalias 'wk 'evil-window-move-very-top)
(defalias 'wj 'evil-window-move-very-bottom)
(defalias 'top-join-line 'crux-top-join-line)


;; 快捷键设置
(require 'general)

(general-define-key
 "M-o" 'other-window
 "C-o" 'crux-smart-open-line
 "C-x C-r" 'crux-recentf-find-file)


(global-unset-key (kbd "M-e"))

(general-define-key
 :prefix "M-e"
 "M-e" 'forward-sentence
 "a" 'backward-sentence
 ;; 快速搜索
 "s s" 'swiper
 "s i" 'imenu
 "s r" 'counsel-rg
 "f f" 'find-file-in-project-by-selected
 "b b" (lambda () (interactive) (switch-to-buffer nil))
 "'" 'imenu-list-smart-toggle
 ;; 快速选中内容。
 "e" 'er/expand-region
 ;; citre 相关快捷键
 "c j" 'citre-jump
 "c J" 'citre-jump-back
 "c p" 'citre-ace-peek
 "c ." 'citre-peek
 "c u" 'citre-update-this-tags-file
 ;; 操作括号。
 "p f" 'sp-forward-slurp-sexp
 "p b" 'sp-forward-barf-sexp
 "p s" 'sp-splice-sexp
 "p r" 'sp-raise-sexp
 "n w" 'widen
 "n n" 'narrow-to-region
 "D" 'crux-delete-file-and-buffer
 "R" 'crux-rename-file-and-buffer
 "j" 'avy-goto-subword-1
 "v" 'set-mark-command
 "u" 'undo-tree-undo
 "r" 'undo-tree-redo
 "%" 'evil-jump-item)


(when (file-exists-p custom-file)
  (load custom-file))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
