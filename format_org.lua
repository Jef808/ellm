function escapeCode(content)
  -- Add a comma before every Org headline (*, **, ...) and #+end_src
  local escapedContent = content:gsub("^(%*)", ",%1"):gsub("\n(%#%+end_src)", "\n,%1")
  return escapedContent
end

function CodeBlock(elem)
  if elem.classes:includes('org') then
    -- Escape the content within the org-mode code block
    local escaped = escapeCode(elem.text)

    -- Return the modified code block with org-mode escape sequences
    return pandoc.RawBlock('org', '#+begin_src org\n' .. escaped .. '\n#+end_src')
  else
    return elem
  end
end

function Header(el)
  el.attributes['CUSTOM_ID'] = nil
  return el
end

return {
  { CodeBlock = CodeBlock },
  { Header = Header }
}
