local home = os.getenv("HOME")
package.cpath = package.cpath .. ";" .. home .. "/.luarocks/lib/lua/5.1/?.so"
package.path = package.path .. ";" .. home .. "/.luarocks/share/lua/5.1/?.lua"

vim.opt.clipboard = "unnamedplus"
vim.opt.nu = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.updatetime = 250
vim.opt.signcolumn = "yes"
vim.opt.scrolloff = 8
vim.opt.ignorecase = true
vim.opt.smartcase = true