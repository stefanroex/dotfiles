# Dotfiles

Ansible-based macOS setup. Run `./install.sh` to bootstrap.

Roles in `roles/`: `applications` (brew casks, App Store), `development` (dev tools), `editors`, `macos_defaults` (system prefs), `terminal` (shell, dotfiles, CLI packages).

Each role follows standard Ansible structure: `tasks/`, `files/`, `defaults/`.

Editor configs: Neovim at `roles/editors/vim/files/`, Emacs at `roles/editors/emacs/files/emacs.d/`.
