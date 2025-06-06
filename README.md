# amp.el

An Emacs interface for the [Amp CLI](https://github.com/sourcegraph/amp) - Sourcegraph's AI-powered coding assistant.

## Features

- **Interactive Terminal Interface**: Launch Amp CLI in a dedicated terminal buffer within Emacs
- **Multi-Project Support**: Automatically manages separate Amp sessions for different projects
- **Intelligent Project Detection**: Works with both `projectile` and `project.el` for project identification
- **Quick Code Actions**: Send selected code to Amp with predefined prompts
- **Smart Process Management**: Automatically handles multiple Amp processes and lets you choose between them
- **Auto-Installation**: Automatically installs Amp CLI via npm if not found

### Multi-Project Capabilities

amp.el excels at managing multiple projects simultaneously:

- **Project-Specific Sessions**: Each project gets its own `*amp-{project-name}*` buffer
- **Automatic Project Detection**: Uses projectile or project.el to identify current project
- **Session Switching**: When no Amp process exists for current project, you can choose from running sessions in other projects
- **Buffer Management**: Clean separation between different project contexts

### Quick Actions

- `amp--fix`: Send selected text with "fix this: " prefix
- `amp--improve`: Send selected text with "improve this: " prefix

## Installation

### Using straight.el

Add this to your Emacs configuration:

```elisp
(straight-use-package 
  '(amp :type git :host github :repo "shaneikenned/amp.el"))
```

### Manual Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/shaneikenned/amp.el.git
   ```

2. Add to your Emacs `load-path`:
   ```elisp
   (add-to-list 'load-path "/path/to/amp.el")
   (require 'amp)
   ```

## Usage

### Basic Usage

1. Run `M-x amp` to start Amp in the current project
2. If Amp CLI isn't installed, it will offer to install it via npm
3. The Amp CLI will open in a terminal buffer named `*amp-{project-name}*`

### Code Actions

1. Select code in any buffer
2. Run `M-x amp--fix` to send code with "fix this: " prefix
3. Run `M-x amp--improve` to send code with "improve this: " prefix

### Key Bindings (Optional)

Add these to your configuration for quick access:

```elisp
(global-set-key (kbd "C-c a a") 'amp)
(global-set-key (kbd "C-c a f") 'amp--fix)
(global-set-key (kbd "C-c a i") 'amp--improve)
```

## Requirements

- Emacs 28.1 or later
- Node.js and npm (for Amp CLI installation)
- Optional: `projectile` or `project.el` for enhanced project detection

## License

This project is licensed under the same terms as GNU Emacs.
