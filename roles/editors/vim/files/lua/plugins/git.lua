return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    opts = {
      kind = "replace",
    },
    keys = {
      { "<leader>gs", "<cmd>Neogit<cr>", desc = "Git status" },
      { "<leader>gl", "<cmd>Neogit log<cr>", desc = "Git log" },
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    opts = {},
    keys = {
      { "<leader>gb", "<cmd>Gitsigns blame<cr>", desc = "Git blame" },
    },
  },
}
