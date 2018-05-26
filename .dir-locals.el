((nil
  .
  ((indent-tabs-mode . nil)
   (eval
    .
    (progn
      (let ((top (locate-dominating-file default-directory ".dir-locals.el"))))

      (defun guile--manual-look-up (id mod)
        (message "guile--manual-look-up id=%s => %s mod=%s" id (symbol-name id) mod)
        (let ((info-lookup-other-window-flag
               geiser-guile-manual-lookup-other-window-p))
          (info-lookup-symbol (symbol-name id) 'scheme-mode))
        (when geiser-guile-manual-lookup-other-window-p
          (switch-to-buffer-other-window "*info*"))
        (search-forward (format "%s" id) nil t))

      (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

      (defun guix-switch-profile (&optional profile)
        "reset Emacs' environment by snarfing PROFILE/etc/profile"

        (defun matches-in-string (regexp string)
          "return a list of matches of REGEXP in STRING."
          (let ((matches))
            (save-match-data
              (string-match "^" "")
              (while (string-match regexp string (match-end 0))
                (push (or (match-string 1 string) (match-string 0 string)) matches)))
            matches))

        (interactive "fprofile: ")
        (let* ((output (shell-command-to-string (concat "GUIX_PROFILE= /bin/sh -x " profile "/etc/profile")))
               (exports (matches-in-string "^[+] export \\(.*\\)" output)))
          (mapcar (lambda (line) (apply #'setenv (split-string line "="))) exports )))
))))
 (scheme-mode
  .
  ((geiser-active-implementations . (guile))
   (eval
    .
    (progn
      (defun prefix-dir-locals-dir (elt)
        (concat (locate-dominating-file buffer-file-name ".dir-locals.el") elt))
      (mapcar
       (lambda (dir) (add-to-list 'geiser-guile-load-path dir))
       (mapcar
        #'prefix-dir-locals-dir
        '("scripts" "guile"))))))))
