require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do

  describe "#update_quality" do

    it "does not change the name" do
      # Item#initialize(name, sell_in, quality)
      items = [Item.new("foo", 0, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].name).to eq "foo"
    end

    it "degrades twice as fast when sell by date has passed" do
      items = [Item.new("foo", 1, 20)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 19
      expect(items[0].sell_in).to eq 0 # One day has passed, quality degraded by one
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 17 # Sellin has passed, quality degrade by two
    end

    it "never degrades quality to a negative number" do
      items = [Item.new("foo", 1, 1)]
      10.times { GildedRose.new(items).update_quality() }
      expect(items[0].sell_in).to eq -9
      expect(items[0].quality).to eq 0
    end

    it "'Aged Brie' actually increases in Quality the older it gets" do
      items = [Item.new("Aged Brie", 20, 10)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 11
      10.times { GildedRose.new(items).update_quality() }
      expect(items[0].quality).to eq 21
    end

    it "Quality of an item never goes above 50" do
      items = [Item.new("Aged Brie", 1, 49)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 50
      10.times { GildedRose.new(items).update_quality() }
      expect(items[0].quality).to eq 50
    end

    it "'Sulfuras', being a legendary item, never has to be sold or decreases in Quality" do
      items = [Item.new("Sulfuras, Hand of Ragnaros", 1, 49)]
      item = items.first
      GildedRose.new(items).update_quality()
      expect(items[0].sell_in).to eq 1
      expect(items[0].quality).to eq 49
    end

    it "'Backstage passes', like aged brie, increases in Quality as its SellIn value approaches" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 20, 10)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 11
    end
    
    it "'Backstage passes', quality increases by 2 when there are 10 days or less" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 10, 11)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 13
      expect(items[0].sell_in).to eq 9
      4.times {GildedRose.new(items).update_quality()}
      expect(items[0].quality).to eq 21
      expect(items[0].sell_in).to eq 5
    end

    it "'Backstage passes', quality increases by 3 when there are 5 days or less" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 5, 30)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 33
      expect(items[0].sell_in).to eq 4
    end

    it "'Backstage passes', quality drops to 0 after the concert" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 1, 30)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 33
      expect(items[0].sell_in).to eq 0
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 0
      expect(items[0].sell_in).to eq -1
    end

  end

end
