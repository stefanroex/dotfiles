return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  opts_extend = { "ensure_installed" },
  opts =  {
    ensure_installed = { 
      "bash",
      "c",
      "clojure",
      "diff",
      "html",
      "javascript",
      "json",
      "jsonc",
      "lua",
      "luadoc",
      "luap",
      "markdown",
      "markdown_inline",
      "printf",
      "python",
      "query",
      "regex",
      "rust",
      "toml",
      "tsx",
      "typescript",
      "vim",
      "vimdoc",
      "xml",
      "yaml",
    },
    highlight = { enable = true },
    indent = { enable = true },  
  },
  config = function(_, opts)
    if type(opts.ensure_installed) == "table" then
      opts.ensure_installed = opts.ensure_installed
    end
    require("nvim-treesitter.configs").setup(opts)
  end,
}
