# terragrunt-mode

A minor mode for Emacs that provides convenient commands and keybindings for working with Terragrunt, a thin wrapper for Terraform that provides extra tools for working with multiple Terraform modules.

## Features

- Quick access to common Terragrunt commands (plan, apply, destroy, init, etc.)
- Customizable key bindings
- Creates unique compilation buffers for each command execution, allowing multiple operations to run in parallel

## Installation

### Manual

Download `terragrunt-mode.el` to your Emacs load path and add the following to your Emacs configuration:

```elisp
(require 'terragrunt-mode)
```

### Using use-package with straight.el

```elisp
(use-package terragrunt-mode
  :straight (terragrunt-mode :type git :host github :repo "diasjorge/terragrunt-mode"))
```

### Using use-package with quelpa

```elisp
(use-package terragrunt-mode
  :quelpa (terragrunt-mode :fetcher github :repo "diasjorge/terragrunt-mode"))
```

## Usage

1. Open a Terragrunt file (`terragrunt.hcl`)
2. Enable the mode with `M-x terragrunt-mode`
3. Use the provided key bindings to execute Terragrunt commands

## Key Bindings

The default prefix for commands is `C-c e`. This can be customized by setting `terragrunt-keymap-prefix`.

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c e p`  | `terragrunt-plan` | Run `terragrunt plan` in the current directory |
| `C-c e a`  | `terragrunt-apply` | Run `terragrunt apply` in the current directory |
| `C-c e d`  | `terragrunt-destroy` | Run `terragrunt destroy` in the current directory |
| `C-c e i`  | `terragrunt-init` | Run `terragrunt init` in the current directory |
| `C-c e o`  | `terragrunt-output` | Run `terragrunt output` in the current directory |
| `C-c e u`  | `terragrunt-unlock` | Run `terragrunt force-unlock` in the current directory |

## Customization

You can customize the key binding prefix:

```elisp
(setq terragrunt-keymap-prefix "C-c t")  ; Change prefix to C-c t
```

## Requirements

- Emacs 24.3 or later
- Terragrunt must be installed and available in your system PATH

## License

MIT License. See the [LICENSE](./LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
