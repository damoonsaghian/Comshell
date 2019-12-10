Editor = class()
function Editor:init(defaults)
  self.buffer = gditor.Buffer()
  self.view = geditor.View()
end

function Editor:prev_page()
end

function Editor:next_page()
end

function Editor:prev_item()
end

function Editor:next_item()
end

-- gspell
