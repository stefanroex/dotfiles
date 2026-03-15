vim.opt.clipboard = "unnamedplus"
vim.opt.nu = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.termguicolors = true
vim.opt.updatetime = 250
vim.opt.signcolumn = "yes"
vim.opt.scrolloff = 8
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "clojure" },
  callback = function()
    vim.opt_local.iskeyword = "@,48-57,_,192-255"
  end,
})
