;;; init-llm.el --- LLM client setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;;;;;;;
;; An experiment, setup code for LLM in this file mainly from any of
;; the LLM service below.
;;;;;;;;;

(ensure-package 'gptel)

(defcustom llm-config-file (expand-file-name "~/.emacs.d/llm-config.el")
  "Path to the LLM configuration file containing API keys and settings."
  :type 'string
  :group 'gptel)

(defcustom llm-backends-config
  '((ollama         :name "Ollama (Local)"
                    :constructor gptel-make-ollama
                    :default-model "deepseek-coder:6.7b"
                    :host "localhost:11434")
    (claude         :name "Claude"
                    :constructor gptel-make-anthropic
                    :key-var gptel-anthropic-key
                    :default-model claude-3-7-sonnet-20250219
                    :available-models (claude-3-5-haiku-20241022 claude-3-7-sonnet-20250219))
    (openai         :name "OpenAI"
                    :constructor gptel-make-openai
                    :key-var gptel-openai-key
                    :default-model o3-mini
                    :available-models (o1-mini o3-mini))
    (gemini         :name "Gemini"
                    :constructor gptel-make-gemini
                    :key-var gptel-gemini-key
                    :default-model gemini-1.5-pro
                    :available-models (gemini-1.5-pro gemini-1.5-flash))
    (deepseek       :name "Deepseek"
                    :constructor gptel-make-openai
                    :key-var gptel-deepseek-key
                    :default-model deepseek-reasoner
                    :available-models (deepseek-reasoner deepseek-chat)
                    :host "api.deepseek.com"
                    :endpoint "/chat/completions")
    (kimi           :name "Kimi"
                    :constructor gptel-make-openai
                    :key-var gptel-kimi-key
                    :default-model kimi-latest
                    :available-models (kimi-latest)
                    :host "api.moonshot.cn"
                    :endpoint "/v1/chat/completions"))
  "A central alist defining all available LLM backends."
  :type '(alist :key-type symbol :value-type plist)
  :group 'gptel)

(defcustom llm-backend-type 'ollama
  "The type of LLM backend to use."
  :type `(choice ,@(mapcar (lambda (conf)
                             (let ((sym (car conf))
                                   (name (plist-get (cdr conf) :name)))
                               `(const :tag ,name ,sym)))
                           llm-backends-config))
  :group 'gptel)

(defun llm-load-config ()
  "Load LLM configuration from external file."
  (if (file-exists-p llm-config-file)
      (load-file llm-config-file)
    (message "LLM config file not found at: %s" llm-config-file)))

(defun llm-setup ()
  "Set up LLM backend based on `llm-backend-type`."
  (let* ((config (cdr (assoc llm-backend-type llm-backends-config)))
         (name (plist-get config :name))
         (constructor (plist-get config :constructor))
         (key-var (plist-get config :key-var))
         (key (if key-var (symbol-value key-var) nil))
         (default-model (plist-get config :default-model))
         (available-models (plist-get config :available-models))
         (host (plist-get config :host))
         (endpoint (plist-get config :endpoint)))

    (unless config
      (error "No configuration found for backend: %s" llm-backend-type))

    ;; Handle Ollama's unique string-based model format
    (when (eq llm-backend-type 'ollama)
      (setq available-models (list (intern default-model))
            default-model (intern default-model)))

    (let ((args `(,name
                  :stream t
                  ,@(when key `(:key ,key))
                  ,@(when available-models `(:models ,available-models))
                  ,@(when host `(:host ,host))
                  ,@(when endpoint `(:endpoint ,endpoint)))))
      (setq gptel-backend (apply constructor args)
            gptel-model default-model))))

(with-eval-after-load 'gptel
  (llm-load-config)
  (llm-setup))

(defun llm-switch-backend (backend)
  "Switch to a different LLM backend.
BACKEND should be one of the symbols defined in `llm-backends-config`."
  (interactive
   (list (intern (completing-read "Select backend: "
                                  (mapcar #'symbol-name (mapcar #'car llm-backends-config))
                                  nil t))))
  (setq llm-backend-type backend)
  (llm-setup)
  (message "Switched to %s backend" backend))

(provide 'init-llm)
;;; init-llm.el ends here
