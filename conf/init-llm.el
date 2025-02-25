;;; init-llm.el --- LLM client setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'gptel)

(defcustom llm-config-file (expand-file-name "~/.emacs.d/llm-config.el")
  "Path to the LLM configuration file containing API keys and settings."
  :type 'string
  :group 'gptel)

(defcustom llm-backend-type 'ollama
  "The type of LLM backend to use."
  :type '(choice
          (const :tag "Ollama (Local)" ollama)
          (const :tag "Claude" claude)
          (const :tag "OpenAI" openai)
          (const :tag "Deepseek" deepseek))
  :group 'gptel)

(defcustom llm-ollama-host "localhost:11434"
  "Host address for Ollama server."
  :type 'string
  :group 'gptel)

(defcustom llm-ollama-model "deepseek-coder:6.7b"
  "The default model to use with Ollama backend."
  :type 'string
  :group 'gptel)

(defcustom llm-claude-model 'claude-3-7-sonnet-20250219
  "The default model to use with Claude backend."
  :type 'symbol
  :group 'gptel)

(defcustom llm-claude-models '(claude-3-5-haiku-20241022 claude-3-7-sonnet-20250219)
  "Available Claude models."
  :type '(repeat symbol)
  :group 'gptel)

(defcustom llm-openai-model 'o3-mini
  "The default model to use with OpenAI backend."
  :type 'symbol
  :group 'gptel)

(defcustom llm-openai-models '(o1-mini o3-mini)
  "Available OpenAI models."
  :type '(repeat symbol)
  :group 'gptel)

(defcustom llm-deepseek-model 'deepseek-reasoner
  "The default model to use with Deepseek backend."
  :type 'symbol
  :group 'gptel)

(defcustom llm-deepseek-models '(deepseek-reasoner deepseek-chat)
  "Available Deepseek models."
  :type '(repeat symbol)
  :group 'gptel)

(defun llm-load-config ()
  "Load LLM configuration from external file."
  (when (file-exists-p llm-config-file)
    (load llm-config-file nil t)))

(defun llm-setup-ollama ()
  "Configure Ollama backend."
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host llm-ollama-host
          :stream t
          :models (list (intern llm-ollama-model)))
        gptel-model (intern llm-ollama-model)))

(defun llm-setup-claude ()
  "Configure Claude backend."
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :key gptel-anthropic-key
          :stream t
          :models llm-claude-models)
        gptel-model llm-claude-model))

(defun llm-setup-openai ()
  "Configure OpenAI backend."
  (setq gptel-backend
        (gptel-make-openai "OpenAI"
          :key gptel-openai-key
          :stream t
          :models llm-openai-models)
        gptel-model llm-openai-model))

(defun llm-setup-deepseek ()
  "Configure Deepseek backend."
  (setq gptel-backend
        (gptel-make-openai "Deepseek"
          :key gptel-deepseek-key
          :stream t
          :models llm-deepseek-models
          :host "api.deepseek.com"
          :endpoint "/chat/completions")
          ;; :header (lambda () `(("Content-Type" . "application/json")))
          ;; :transformer #'gptel-openai-transformer
          ;; :completion-filter #'gptel-openai-filter)
        gptel-model llm-deepseek-model))


(defun llm-setup ()
  "Set up LLM backend based on configuration."
  (pcase llm-backend-type
    ('ollama (llm-setup-ollama))
    ('claude (llm-setup-claude))
    ('openai (llm-setup-openai))
    ('deepseek (llm-setup-deepseek))
    (_ (message "No LLM backend configured"))))

(defun llm-switch-backend (backend)
  "Switch to a different LLM backend.
BACKEND should be one of: 'ollama or 'claude"
  (interactive
   (list (intern (completing-read "Select backend: "
                                 '("ollama" "claude" "openai" "deepseek")
                                 nil t))))
  (setq llm-backend-type backend)
  (llm-setup)
  (message "Switched to %s backend" backend))

;; Initialize LLM setup
(llm-load-config)
(llm-setup)

(provide 'init-llm)
;;; init-llm.el ends here
