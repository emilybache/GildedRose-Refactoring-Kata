local GildedRose = {}

function GildedRose:new(items)
  return {
    items = items,
    updateQuality = self.updateQuality,
  }
end

function GildedRose:updateQuality()
  for i, _ in pairs(self.items) do
    if self.items[i].name ~= "Aged Brie" and
        self.items[i].name ~= "Backstage passes to a TAFKAL80ETC concert" then
      if self.items[i].quality > 0 then
        if self.items[i].name ~= "Sulfuras, Hand of Ragnaros" then
          self.items[i].quality = self.items[i].quality - 1
        end
      end
    else
      if self.items[i].quality < 50 then
        self.items[i].quality = self.items[i].quality + 1

        if self.items[i].name == "Backstage passes to a TAFKAL80ETC concert" then
          if self.items[i].sell_in < 11 then
            if self.items[i].quality < 50 then
              self.items[i].quality = self.items[i].quality + 1
            end
          end
          if self.items[i].sell_in < 6 then
            if self.items[i].quality < 50 then
              self.items[i].quality = self.items[i].quality + 1
            end
          end
        end
      end
    end

    if self.items[i].name ~= "Sulfuras, Hand of Ragnaros" then
      self.items[i].sell_in = self.items[i].sell_in - 1
    end

    if self.items[i].sell_in < 0 then
      if self.items[i].name ~= "Aged Brie" then
        if self.items[i].name ~= "Backstage passes to a TAFKAL80ETC concert" then
          if self.items[i].quality > 0 then
            if self.items[i].name ~= "Sulfuras, Hand of Ragnaros" then
              self.items[i].quality = self.items[i].quality - 1
            end
          end
        else
          self.items[i].quality = self.items[i].quality - self.items[i].quality
        end
      else
        if self.items[i].quality < 50 then
          self.items[i].quality = self.items[i].quality + 1
        end
      end
    end
  end
  return self.items
end

return GildedRose
