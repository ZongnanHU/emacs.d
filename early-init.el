(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold  (* 20 1024 1024))))
