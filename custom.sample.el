;; 本文件保存的是一些需要依据不同系统进行调整的设置

(pyvenv-activate "d:/opt/pyvenv/dev/")


;; 分别设置中英文字体。Emacs 默认的中文字体不仅丑陋，还会让 Emacs 卡顿。
;; 设置中文和英文不同的缩放率可以让 Org Mode 的表格对齐。10 号字体舒服。
(when window-system
  (set-face-attribute 'default nil :font "MesloLGM NF 9")
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Noto Sans CJK SC Regular")))
  (setq face-font-rescale-alist '(("MesloLGM NF" . 1)
                                  ("Noto Sans CJK SC Regular" . 1.2))))


;; 手动设置一些程序的路径，尤其在 Windows 路径下
(setq diff-command "D:/opt/Git/usr/bin/diff3.exe")
(setq ediff-diff-program "D:/opt/Git/usr/bin/diff.exe")
(setq ediff-diff3-program "D:/opt/Git/usr/bin/diff3.exe")
(setq ffip-find-executable "d:/opt/emacs-27.2-x86_64/bin/fd.exe")


;; 默认激活的 Python 环境
(pyvenv-activate "d:/opt/pyvenv/dev/")


;; 笔记所在路径
(setq znh/notes_box "d:/org/org.xy.notes_box/notes/")

;; 配置 org mode 的默认笔记
(setq znh/notes_box "d:/org/org.xy.notes_box/notes/")
(setq znh/draft_file (concat znh/notes_box "draft.org"))
(setq org-default-notes-file znh/draft_file)
(setq znh/journal_file (concat znh/notes_box "journal.org"))
(setq znh/task_file (concat znh/notes_box "task.org"))
(setq org-agenda-files `(,znh/task_file))
(setq org-capture-templates
      `(("t" "Todo" entry (file+olp ,znh/task_file "Inbox")
         "* TODO %?\n  %i\n")
        ("T" "Todo with link" entry (file+olp ,znh/task_file "Inbox")
         "* TODO %?\n  %i\n  %a\n")
        ("n" "Note" entry (file+olp ,znh/draft_file "Inbox")
         "* %U\n %i\n %?\n")
        ("N" "Note with link" entry (file+olp ,znh/draft_file "Inbox")
         "* %U\n %a\n %i\n %?\n")
        ("j" "Journal" entry (file ,znh/journal_file)
         "* %u\n %?\n")))

;; sqlite3 的设置
(setq sql-sqlite-options '("-interactive"))


;; 配置样式
(custom-set-faces
 '(fixed-pitch ((t (:family "MesloLGM NF")))))
