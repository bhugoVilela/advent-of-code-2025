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

function inspect(t)
  print('inspecting...')
  for key, value in pairs(t) do
    print("-", key, value)
  end
end

function is_pseudo_element(block)
  return paragraph_to_header(block) ~= nil
end

function paragraph_to_header(block)
  if block.t ~= "Para" or #block.content == 0 then
    return nil
  end

  local first = block.content[1]
  if not first.text then
    return nil
  end

  -- Check for [h1] through [h6] markers
  local level = first.text:match("^%[h(%d)%]$")
  if level then
    level = tonumber(level)
    if level >= 1 and level <= 6 then
      -- Create header with remaining content (skip the marker)
      local header_content = {table.unpack(block.content, 2)}
      return pandoc.Header(level, header_content)
    end
  end

  return nil
end

function Pandoc(doc)
  local new_blocks = {}
  local i = 1

  while i <= #doc.blocks do
    local non_code = {}

    local block = doc.blocks[i]
    local header = paragraph_to_header(block)
    if header then
      block = header
      doc.blocks[i] = block
    end

    --collect consecutive non code-blocks
    while i <= #doc.blocks 
      and not is_haskell(doc.blocks[i])
      and not is_pseudo_element(doc.blocks[i])
      and (not is_header(doc.blocks[i]) or doc.blocks[i].level > 3)
      do
      table.insert(non_code, doc.blocks[i])
      i = i + 1
    end

    -- if doc.blocks[i].t == 'Header' then
    --   inspect(doc.blocks[i])
    -- end

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

