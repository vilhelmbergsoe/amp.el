# AGENT.md - amp.el Development Guide

## Commands
- No build/test/lint commands (single-file Emacs Lisp package)
- Test syntax: `emacs -batch -f check-parens amp.el`
- Load test: `emacs -batch -l amp.el`

## Code Style
- **Language**: Emacs Lisp
- **File naming**: All lowercase with hyphens, `.el` extension
- **Function naming**: Use `amp--` prefix for private functions, `amp-` for public
- **Variables**: Use `amp--` prefix for internal variables
- **Docstrings**: Required for all public functions, use triple quotes
- **Comments**: Use `;;; ` for major sections, `;; ` for explanations
- **Indentation**: Standard Emacs Lisp (2 spaces for function bodies)
- **Line length**: Keep under 80 characters when possible
- **Lexical binding**: Always use `;;; -*- lexical-binding: t; -*-`
- **Requires**: Place all requires at top of file after header
- **Error handling**: Use `condition-case` for error handling
- **Buffer names**: Format as `*amp-{project-name}*`
- **Interactive functions**: Mark with `;;;###autoload` for public commands
- **Package structure**: Single file, provide statement at end

## Project Structure
- Main functionality in `amp.el` (single file package)
- Uses `vterm` for terminal interaction
- Supports projectile and project.el for project detection
