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

function is_header(block)
  return block.t == "Header"
end

function Pandoc(doc)
  local new_blocks = {}
  local i = 1

  while i <= #doc.blocks do
    local non_code = {}

    --collect consecutive non code-blocks
    while i <= #doc.blocks 
      and not is_haskell(doc.blocks[i])
      and not is_header(doc.blocks[i])
      do
      table.insert(non_code, doc.blocks[i])
      i = i + 1
    end

    if #non_code ~= 0 then
      local code_blocks = {}

      --collect consecutive code-blocks
      while i <= #doc.blocks and is_haskell(doc.blocks[i]) do
        local code_block = doc.blocks[i]

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
        i = i + 1
      end

      if #code_blocks > 0 then
        -- create a code div containing all the code blocks
        local code_div = pandoc.Div(code_blocks, {class = "code"})
        local lhs = pandoc.Div(non_code, {class='lhs'})
        local row = pandoc.Div({lhs, code_div}, {class = "row"})
        table.insert(new_blocks, row)
      else
        -- no code blocks following, create empty code div
        local empty_div = pandoc.Div({}, {class = "code"})
        local lhs = pandoc.Div(non_code, {class='lhs'})
        local row = pandoc.Div({lhs, empty_div}, {class = "row"})
        table.insert(new_blocks, row)
      end
     
    else
      -- no non code blocks remaining, just add blocks as-is
      table.insert(new_blocks, doc.blocks[i])
      i = i + 1
    end
  end

  return pandoc.Pandoc(new_blocks, doc.meta)
end
