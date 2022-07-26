;;; init-x11.el --- X11 setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(set-face-attribute 'default nil
                    :font (font-spec :name "PragmataPro Mono" :size 12))


(setq x-alt-keysym 'meta)


(menu-bar-mode -1)


(provide 'init-x11)
;;; init-x11.el ends here
