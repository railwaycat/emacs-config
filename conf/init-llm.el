;;; init-llm.el --- LLM client setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ensure-package 'gptel)
(use-package gptel
  :init
  (load-file "~/.emacs.d/llm-config.el")
  :config
  (setq gptel-backend-openai
        (gptel-make-openai "OpenAI"
          :key gptel-openai-key
          :stream t
          :models gptel-openai-models))
  (setq gptel-backend-anthropic
        (gptel-make-anthropic "Claude"
          :key gptel-anthropic-key
          :stream t
          :models gptel-claude-models))
  (setq gptel-backend-ollama
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models gptel-ollama-models))
  (setq gptel-backend-gemini
        (gptel-make-gemini "Gemini"
          :key gptel-gemini-key
          :stream t))
  (setq gptel-backend-deepseek
        (gptel-make-deepseek "Deepseek"
          :key gptel-deepseek-key
          :stream t))
  (setq gptel-backend-moonshot
        (gptel-make-openai "Moonshot"
          :host "api.moonshot.cn"
          ;; :host "api.moonshot.ai"
          :key gptel-moonshot-key
          :stream t
          :models '(kimi-latest)))
  (setq gptel-backend gptel-backend-moonshot)
  (setq gptel-model (car (gptel-backend-models gptel-backend)))

  (defun gptel-switch-backend()
    "Switch between available gptel backends."
    (interactive)
    (let* ((backends '(("OpenAI" . gptel-backend-openai)
                       ("Claude" . gptel-backend-anthropic)
                       ("Ollama" . gptel-backend-ollama)
                       ("Gemini" . gptel-backend-gemini)
                       ("Deepseek" . gptel-backend-deepseek)
                       ("Moonshot" . gptel-backend-moonshot)
                       ))
           (choice (completing-read "Select backend: " (mapcar #'car backends))))
      (setq gptel-backend (symbol-value (cdr (assoc choice backends))))
      (setq gptel-model (car (gptel-backend-models gptel-backend)))
      (message "Switched to %s backend with model %s" choice gptel-model))))


(provide 'init-llm)
;;; init-llm.el ends here
