# ellm - Emacs LLM Integration

A simple and flexible Emacs package for interacting with Large Language
Models (LLMs) without changing your workflow. Chat with AI assistants, get
code help, and maintain conversation history - all without leaving
Emacs.

## Features

- **Multi-provider support**: OpenAI, Anthropic, xAI, and Perplexity
- **Project-wise conversation management**: Persistent project-wise chat history with org-mode
  integration
- **Context-aware prompts**: Add code snippets and text as context for
  better responses
- **Flexible system messages**: Pre-configured prompts for different use
  cases (code review, generation, Q&A, etc...)
- **Export capabilities**: Convert conversations to HTML for sharing
- **Rating system**: Rate and organize your conversations

## Installation

### Prerequisites

- Emacs 29.1 or later
- `pandoc` (for markdown to org conversion)
- API keys for your preferred LLM providers

### Manual Installation

1.  Clone this repository:

``` bash
git clone https://github.com/Jef808/ellm.git
```

2.  Add to your Emacs configuration:

``` elisp
(add-to-list 'load-path "/path/to/ellm")
(require 'ellm)
```

### Doom Emacs

In `package.el`

``` elisp
(package! ellm
  :recipe (:host github
           :repo "jef808/ellm"
           :files ("ellm.el" "format_org.lua")))
```

In `config.el`

``` elisp
(defun my/get-ellm-api-key (provider)
  "Get API key for PROVIDER from a list."
  (let ((api-keys
         '((openai . "your-openai-key")
           (anthropic . "your-anthropic-key")
           (xai . "your-xai-key")
           (perplexity . "your-perplexity-key")
           ;; etc...
           )))
    (alist-get provider api-keys)))

(use-package! ellm
  :custom
  ellm-api-key #'my/get-ellm-api-key
  :config
  (ellm-setup-persistance)
  (global-ellm-mode))
```

## Quick Start

### 1. Set up API keys

Set environment variables for your preferred providers:

``` bash
export OPENAI_API_KEY="your-openai-key"
export ANTHROPIC_API_KEY="your-anthropic-key"
export XAI_API_KEY="your-xai-key"
export PERPLEXITY_API_KEY="your-perplexity-key"
# ... etc
```

Or, as in the doom emacs installation instructions,
customize `ellm-api-key` to use your own key retrieval function. 
For example,

``` elisp
(defun my/get-ellm-api-key (provider)
  (let ((api-keys
         '((openai . "your-openai-key")
           (anthropic . "your-anthropic-key")
           (xai . "your-xai-key")
           (perplexity . "your-perplexity-key")
           ;; etc...
           )))))
           
(setq ellm-api-key #'my/get-ellm-api-key)
```


### 2. Enable the mode

``` elisp
;; Enable globally
(global-ellm-mode 1)

;; Or just in specific modes
(add-hook 'prog-mode-hook #'ellm-mode)
```

### 3. Start chatting

- `C-c ; c` - Open up the config buffer to set your desired provider etc...
- `C-c ; n` - Start a new conversation
- `C-c ; N` - Continue conversation at point (inside an ellm conversation file)
- `C-c ; m` - Optionally, add marked region into context before making a prompt 

## Configuration

### Basic Setup

``` elisp
(use-package ellm
  :config
  ;; Enable persistence across emacs sessions
  (ellm-setup-persistance)
  (global-ellm-mode 1))
```

### System Messages

ellm comes with several pre-configured system messages for different
tasks:

- `default` - General assistant
- `code-generation` - Code writing help
- `code-review` - Code review and feedback
- `code-refactoring` - Code improvement suggestions
- `question-and-answer` - Technical Q&A
- `software-architect` - System design help

Switch between them with `C-c ; c` and navigating to "System Message" or:

``` elisp
(ellm-set-system-message 'code-generation)
```

### Custom System Messages

Add your own system messages:

``` elisp
(add-to-list 'ellm-system-messages
  '(my-custom-prompt :type string
    :value "You are a helpful assistant specialized in..."))
```

## Usage

### Basic Chat

1.  **Start a conversation**: `C-c ; n`
2.  **View conversations**: `C-c ; ;`
3.  **Continue at point**: Place cursor in a conversation and press
    `C-c ; N`

### Context-Aware Prompts

Add context to your prompts for better responses:

1.  **Select text/code** you want to include as context
2.  **Add to context**: `C-c ; m`
3.  **View context buffer**: `C-c ; x`
4.  **Chat with context**: `C-c ; n`
5.  **Remove region from context** `C-c ; d`
5.  **Clear all context**: `C-c ; C-M-k`

### Conversation Management

- **Rate conversations**: `C-c ; r` (1-5 stars, with color coding)
- **Export to HTML**: `C-c ; e`
- **Search conversations for keywords**: `C-c ; s`
- **Navigate messages within conversations file**: `C-c ; j/k` (next/previous)
- **Fold conversations within conversations file**: `C-c ; o`

### Configuration

Access the configuration menu with `C-c ; c`:

- Switch providers and models
- Adjust temperature and token limits
- Change system messages
- Toggle debug/test modes

## Key Bindings

| Key Binding | Command | Description |
|----|----|----|
| `C-c ; n` | `ellm-context-complete` | New conversation with context |
| `C-c ; N` | `ellm-chat-at-point` | Continue conversation at point |
| `C-c ; ;` | `ellm-show-conversations-buffer` | Show conversations |
| `C-c ; c` | `ellm-set-config` | Configuration menu |
| `C-c ; m` | `ellm-add-context-chunk` | Add selected text to context |
| `C-c ; d` | `ellm-remove-context-chunk` | Remove context at point |
| `C-c ; x` | `ellm-view-context-buffer` | View context buffer |
| `C-c ; C-M-k` | `ellm-clear-context` | Clear all context |
| `C-c ; r` | `ellm-org-rate-response-and-refresh` | Rate conversation |
| `C-c ; e` | `ellm-export-conversation` | Export to HTML |
| `C-c ; s` | `ellm-search-in-conversations` | Search conversations |
| `C-c ; j/k` | `ellm-org-next/previous-message` | Navigate messages |
| `C-c ; o` | `ellm-org-fold-conversations-buffer` | Fold all conversations |

## File Organization

Conversations are stored as org files in `~/.ellm/`:

``` example
~/.ellm/
├── conversations-general.org      # Default conversations
├── conversations-my-project.org   # Project-specific chats
└── conversations-other-proj.org   # Another project's chats
```

where the suffix is gotten from the active project.

Each conversation includes metadata (model, temperature, rating) and
full chat history.

## Supported Providers

| Provider   | Models                   | Notes            |
|------------|--------------------------|------------------|
| OpenAI     | GPT-4o, GPT-3.5-turbo    | Most reliable    |
| Anthropic  | Claude 3.5 Sonnet/Haiku  | Great for coding |
| xAI        | Grok Beta                | Newer option     |
| Perplexity | Sonar models             | Web-enhanced     |

## Tips & Tricks

### For Coding

1.  Use `code-generation` system message for new code
2.  Use `code-review` for feedback on existing code
3.  Use `mark-defun` and add relevant function defs as context before asking questions
4.  The programming language is detected from the major mode when making the prompt

### For Research

1.  Use `question-and-answer` system message for factual queries
2.  Add documentation or reference material as context
3.  Rate conversations to build a knowledge base
4.  Export important conversations for future reference or for sharing

## Troubleshooting

### Enable Debug Mode

``` elisp
(ellm-toggle-debug-mode)
```

This logs all requests and responses to the `*ellm-logs*` buffer.

### Test Mode

For development or testing with minimal token usage:

``` elisp
(ellm-toggle-test-mode)
```

(also available as a toggle in the config with `C-c ; c`)

This uses the smallest configured model with 10 output tokens.

### Possible Issues

Check the `*ellm-logs*` buffer and see what fits:

- **Rate limits**: Try again or switch providers
- **Context too long**: Clear context or remove some chunks
- **Org formatting issues**: Ensure pandoc is installed on your system

## Contributing

Pull requests are welcomed!

## License

This project is licensed under the MIT License - see the LICENSE
file for details.
