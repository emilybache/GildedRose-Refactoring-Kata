require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do

  describe "update_quality" do
    it "does not change the name" do
      
    items = [
       Item.new(name="+5 Dexterity Vest", sell_in=10, quality=20),
       Item.new(name="Aged Brie", sell_in=2, quality=0),
       Item.new(name="Elixir of the Mongoose", sell_in=5, quality=7),
       Item.new(name="Sulfuras, Hand of Ragnaros", sell_in=0, quality=80),
       Item.new(name="Sulfuras, Hand of Ragnaros", sell_in=-1, quality=80),
       Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20),
       Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=49),
       Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49),
       Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=39),
       # Now it works perfectly
       Item.new(name="Conjured Mana Cake", sell_in=0, quality=6),
       Item.new(name="Conjured Mana Cake", sell_in=3, quality=6),
    ]
    
      GildedRose.update_quality()
      expect(items[0].quality).to eq 19
      expect(items[1].quality).to eq 1
      expect(items[2].quality).to eq 6
      expect(items[3].quality).to eq 80
      expect(items[4].quality).to eq 80
      expect(items[5].quality).to eq 21
      expect(items[6].quality).to eq 50
      expect(items[7].quality).to eq 50
      expect(items[8].quality).to eq 42
      expect(items[9].quality).to eq 2
      expect(items[10].quality).to eq 4
    end
  end

end
