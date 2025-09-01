local Item = require("src/item")
local GildedRose = require("src/gilded_rose")

local items = {
  Item:new("+5 Dexterity Vest", 10, 20),
  Item:new("Aged Brie", 2, 0),
  Item:new("Elixir of the Mongoose", 5, 7),
  Item:new("Sulfuras, Hand of Ragnaros", 0, 80),
  Item:new("Sulfuras, Hand of Ragnaros", -1, 80),
  Item:new("Backstage passes to a TAFKAL80ETC concert", 15, 20),
  Item:new("Backstage passes to a TAFKAL80ETC concert", 10, 49),
  Item:new("Backstage passes to a TAFKAL80ETC concert", 5, 49),

  -- This Conjured item does not work properly yet
  Item:new("Conjured Mana Cake", 3, 6),
};

local days = arg[1] or 2;
local gilded_rose = GildedRose:new(items);

print("OMGHAI!");
for day = 0, days do
  print("-------- day " .. day .. " --------");
  print("name, sellIn, quality");
  for _, item in pairs(items) do
    print(item.name .. ", " .. item.sell_in .. ", " .. item.quality);
  end
  gilded_rose:updateQuality();
  print("")
end
