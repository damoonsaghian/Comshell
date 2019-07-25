local function class(new_class)
  new_class.__index = new_class

  local mt = {
    __call = function(obj)
      setmetatable(obj, new_class)
      return obj
    end
  }
  setmetatable(new_class, mt)

  return new_class
end

return { class = class }
