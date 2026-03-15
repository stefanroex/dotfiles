return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "hrsh7th/cmp-nvim-lsp",
    "j-hui/fidget.nvim",
    { "folke/lazydev.nvim", ft = "lua", opts = {} },
  },
  config = function()
    local cmp_lsp = require("cmp_nvim_lsp")
    local capabilities = vim.tbl_deep_extend(
      "force",
      {},
      vim.lsp.protocol.make_client_capabilities(),
      cmp_lsp.default_capabilities()
    )

    require("fidget").setup({})
    require("mason").setup()

    local function lsp_code_action(action_name)
      return function()
        vim.lsp.buf.code_action({
          filter = function(action) return action.title:find(action_name) ~= nil end,
          apply = true,
        })
      end
    end

    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("UserLspKeymaps", { clear = true }),
      callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)

        local opts = function(desc)
          return { buffer = ev.buf, desc = desc }
        end
        vim.keymap.set("n", "<leader>lR", vim.lsp.buf.rename, opts("Rename"))
        vim.keymap.set("n", "<leader>la", vim.lsp.buf.code_action, opts("Code action"))
        vim.keymap.set("n", "<leader>lr", vim.lsp.buf.references, opts("References"))
        vim.keymap.set("n", "<leader>ld", vim.diagnostic.open_float, opts("Line diagnostics"))
        vim.keymap.set("n", "<leader>lD", "<cmd>Telescope diagnostics<cr>", opts("All diagnostics"))
        vim.keymap.set("n", "<M-.>", vim.lsp.buf.definition, opts("Go to definition"))
        vim.keymap.set("n", "<M-,>", "<C-o>", opts("Jump back"))
        vim.keymap.set("n", "<M-?>", vim.lsp.buf.references, opts("Find references"))
        vim.keymap.set("n", "<leader>j", lsp_code_action("Add missing"), opts("Add missing require"))
        vim.keymap.set("n", "<leader>lc", lsp_code_action("Clean namespace"), opts("Clean namespace"))
        vim.keymap.set("n", "<leader>lf", lsp_code_action("Extract function"), opts("Extract function"))
        vim.keymap.set("n", "<leader>ll", lsp_code_action("Move to let"), opts("Move to let"))

        if client and client.supports_method("textDocument/documentHighlight") then
          vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
            buffer = ev.buf,
            callback = vim.lsp.buf.document_highlight,
          })
          vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
            buffer = ev.buf,
            callback = vim.lsp.buf.clear_references,
          })
        end

        if client and client.supports_method("textDocument/formatting") then
          vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = ev.buf,
            callback = function()
              vim.lsp.buf.format({ bufnr = ev.buf })
            end,
          })
        end
      end,
    })

    require('mason-lspconfig').setup({
      ensure_installed = {
        "lua_ls",
        "clojure_lsp",
        "ruff",
      },
      handlers = {
        function(server_name)
          require('lspconfig')[server_name].setup({
            capabilities = capabilities,
          })
        end,
        lua_ls = function()
          require('lspconfig').lua_ls.setup({
            capabilities = capabilities,
          })
        end
      }
    })
  end
}
