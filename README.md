# fastbuild-lsp

A language server for [FASTBuild](https://fastbuild.org)

## Features

- [x] Syntax checking
- [x] Diagnostics
- [x] Semantic tokens
- [x] Hover
- [x] Autocompletion
- [x] Go to definition
- [x] Find references
- [x] Dynamic type checking
- [ ] Rename
- [ ] Code actions/suggestions

## Installation

This is highly experimental project

```sh
cargo install --git https://github.com/pinbraerts/fastbuild-lsp
```

## Usage

### [NeoVim](https://github.com/neovim/neovim)

#### Using [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')
if not configs.fastbuild_lsp then
  configs.fastbuild_lsp = {
    default_config = {
      cmd = { 'fastbuild-lsp' },
      filetypes = { 'fastbuild' },
      root_dir = lspconfig.util.find_git_ancestor,
      settings = {},
    },
  }
end
lspconfig.fastbuild_lsp.setup { }
```
