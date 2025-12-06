function is_haskell(block)
  if not block or block.t ~= "CodeBlock" then
    return false
  end
  for _, class in ipairs(block.classes) do
    if class == "haskell" or class == "haskell-top" or class:match("^haskell") then
      return true
    end
  end
  return false
end

function Pandoc(doc)
  local new_blocks = {}
  local i = 1

  while i <= #doc.blocks do
    local current = doc.blocks[i]

    if current.t == "Para" then
      -- collect all consecutive haskell code blocks following this paragraph
      local code_blocks = {}
      local j = i + 1
      while j <= #doc.blocks and is_haskell(doc.blocks[j]) do
        local code_block = doc.blocks[j]

        -- Ensure haskell-top blocks have haskell class for syntax highlighting
        local has_haskell_class = false
        for _, class in ipairs(code_block.classes) do
          if class == "haskell" then
            has_haskell_class = true
            break
          end
        end

        if not has_haskell_class then
          -- Add haskell class if not present
          table.insert(code_block.classes, 1, "haskell")
        end

        table.insert(code_blocks, code_block)
        j = j + 1
      end

      if #code_blocks > 0 then
        -- create a code div containing all the code blocks
        local code_div = pandoc.Div(code_blocks, {class = "code"})
        local row = pandoc.Div({current, code_div}, {class = "row"})
        table.insert(new_blocks, row)
        i = j -- skip past all the code blocks we just processed
      else
        -- no code blocks following, create empty code div
        local empty_div = pandoc.Div({}, {class = "code"})
        local row = pandoc.Div({current, empty_div}, {class = "row"})
        table.insert(new_blocks, row)
        i = i + 1
      end
    else
      -- not a paragraph, just pass through
      table.insert(new_blocks, current)
      i = i + 1
    end
  end

  return pandoc.Pandoc(new_blocks, doc.meta)
end
