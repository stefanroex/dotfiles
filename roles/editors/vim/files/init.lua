vim.opt.clipboard = "unnamedplus"

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require("config.lazy")

-- set line numbers
vim.opt.nu = true

-- set tab size to 2 spaces
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true

-- disable wrapping
vim.opt.wrap = false

-- Use more colors
vim.opt.termguicolors = true

-- Keymaps
vim.keymap.set("n", "<leader>v", "<cmd>e ~/.config/nvim/init.lua<cr>");
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Go to Left Window", remap = true })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Go to Lower Window", remap = true })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Go to Upper Window", remap = true })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Go to Right Window", remap = true })
vim.keymap.set("n", "<leader><leader>", "<cmd>e #<cr>", { desc = "Switch to Other Buffer" })
vim.keymap.set("n", "<leader>z", "<cmd>bp<cr>", { desc = "Prev Buffer" })
vim.keymap.set("n", "<leader>x", "<cmd>bn<cr>", { desc = "Next Buffer" })
vim.keymap.set("n", "<leader>w", "<cmd>:bp<cr>:bd#<cr>", { desc = "Delete buffer (keep window)" })
vim.keymap.set("n", "<leader>q", "<cmd>:bd<cr>", { desc = "Delete buffer (close window)" })
vim.keymap.set({"n", "v"}, "<leader>cc", "gcc<esc>", { desc = "Comment code", remap = true })
vim.keymap.set("n", "<cr>", ':nohlsearch<cr>', { noremap = true, silent = true })
