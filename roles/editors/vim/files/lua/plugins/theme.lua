return {
  {
    "embark-theme/vim",
    name = "embark",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd.colorscheme("embark")

      local function fg(name)
        local hl = vim.api.nvim_get_hl(0, { name = name })
        return hl.fg and string.format("#%06x", hl.fg)
      end

      local red = fg("Function")
      local purple = fg("Type")
      local blue = fg("CursorLineNr")
      local green = fg("Keyword")
      local default_fg = fg("Normal")

      -- defn/defn-/fn etc → red
      vim.api.nvim_set_hl(0, "@keyword.function", { fg = red })
      vim.api.nvim_set_hl(0, "@keyword", { fg = red })
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

      -- Keywords (:foo) → green
      vim.api.nvim_set_hl(0, "@lsp.type.keyword", { fg = green })
      vim.api.nvim_set_hl(0, "@string.special.symbol", { fg = green })

      -- Events and operators → default text color
      vim.api.nvim_set_hl(0, "@lsp.type.event", { fg = default_fg })
      vim.api.nvim_set_hl(0, "@keyword.operator", { fg = default_fg })

      -- Keyword namespaces and types → blue
      vim.api.nvim_set_hl(0, "@lsp.type.namespace", { fg = blue })
      vim.api.nvim_set_hl(0, "@lsp.type.type", { fg = blue })
    end
  }
}
