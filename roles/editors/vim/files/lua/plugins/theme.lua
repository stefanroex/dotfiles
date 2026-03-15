return {
  {
    "embark-theme/vim",
    name = "embark",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd.colorscheme("embark")

      local red = "#F48FB1"
      local purple = "#D4BFFF"
      local blue = "#91DDFF"
      local default_fg = "#CBE3E7"

      -- defn/defn-/fn etc → red
      vim.api.nvim_set_hl(0, "@keyword.function", { fg = red })
      vim.api.nvim_set_hl(0, "@lsp.type.macro", { fg = red })

      -- Function name in defn → purple
      vim.api.nvim_set_hl(0, "@function", { fg = purple })

      -- Function calls → default text color (no special highlighting)
      vim.api.nvim_set_hl(0, "@function.call", { fg = default_fg })
      vim.api.nvim_set_hl(0, "@function.builtin", { fg = default_fg })
      vim.api.nvim_set_hl(0, "@function.macro", { fg = default_fg })
      vim.api.nvim_set_hl(0, "@function.method.call", { fg = default_fg })
      vim.api.nvim_set_hl(0, "@lsp.type.function", { fg = default_fg })
      vim.api.nvim_set_hl(0, "@lsp.typemod.function.definition", { fg = purple })

      -- Keyword namespaces → blue
      vim.api.nvim_set_hl(0, "@lsp.type.namespace", { fg = blue })
    end
  }
}
