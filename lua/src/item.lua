local Item = {}

function Item:new(name, sell_in, quality)
  return { name = name, sell_in = sell_in, quality = quality }
end

return Item
