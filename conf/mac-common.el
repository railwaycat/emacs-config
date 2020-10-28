;; Nextstep only changes
(if (eq window-system 'ns)
    (progn
      ;; stop open new frame when use OS X's open
      (setq ns-pop-up-frames nil)
      ;; scroll one line at a time, mostly for ns-port
      (setq scroll-step 1)
      ;; (setq scroll-conservatively 10000)
      (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
      (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
      (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
      (setq visible-bell nil)
      )
  )

(if (eq window-system 'mac)
    (customize-set-value 'mac-mouse-wheel-smooth-scroll nil))
