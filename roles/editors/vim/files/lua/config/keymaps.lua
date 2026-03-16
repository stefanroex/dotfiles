local map = vim.keymap.set

local function reveal_in_finder()
  vim.fn.system({ "open", "-R", vim.fn.expand("%:p") })
end

local function open_on_github()
  local file = vim.fn.expand("%:.")
  local line = vim.fn.line(".")
  local remote = vim.fn.trim(vim.fn.system("git remote get-url origin"))
  local branch = vim.fn.trim(vim.fn.system("git rev-parse --abbrev-ref HEAD"))
  remote = remote:gsub("git@github.com:", "https://github.com/"):gsub("%.git$", "")
  vim.fn.system({ "open", remote .. "/blob/" .. branch .. "/" .. file .. "#L" .. line })
end

-- Windows
map("n", "<C-h>", "<C-w>h", { desc = "Go to left window", remap = true })
map("n", "<C-j>", "<C-w>j", { desc = "Go to lower window", remap = true })
map("n", "<C-k>", "<C-w>k", { desc = "Go to upper window", remap = true })
map("n", "<C-l>", "<C-w>l", { desc = "Go to right window", remap = true })

-- Buffers
map("n", "<leader><leader>", "<cmd>e #<cr>", { desc = "Switch to other buffer" })
map("n", "<leader>z", "<cmd>bp<cr>", { desc = "Prev buffer" })
map("n", "<leader>x", "<cmd>bn<cr>", { desc = "Next buffer" })
map("n", "<leader>w", "<cmd>bp<cr><cmd>bd#<cr>", { desc = "Delete buffer (keep window)" })
map("n", "<leader>q", "<cmd>bd<cr>", { desc = "Delete buffer (close window)" })

-- Editing
map({ "n", "v" }, "<leader>cc", "gcc<esc>", { desc = "Comment code", remap = true })

-- Navigation
map("n", "<leader>v", "<cmd>e " .. vim.fn.stdpath("config") .. "/init.lua<cr>", { desc = "Edit config" })
map("n", "<leader>o", reveal_in_finder, { desc = "Reveal in Finder" })
map("n", "<leader>go", open_on_github, { desc = "Open on GitHub" })

-- Clear search highlight with Enter in normal file buffers
vim.api.nvim_create_autocmd("FileType", {
  pattern = "*",
  callback = function()
    if vim.bo.buftype == "" then
      map("n", "<cr>", "<cmd>nohlsearch<cr>", { buffer = true, silent = true })
    end
  end,
})
