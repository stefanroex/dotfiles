return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  opts = {
    spec = {
      { "<leader>l", group = "LSP" },
      { "<leader>g", group = "Git" },
      { "<leader>e", group = "Eval" },
      { "<leader>p", group = "Project" },
    },
  },
  keys = {
    {
      "<leader>?",
      function()
        require("which-key").show({ global = false })
      end,
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
}
