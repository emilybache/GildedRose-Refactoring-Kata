require_relative 'gilded_rose'
require_relative 'item'

describe GildedRose do
  describe "#update_quality" do
    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].name).to eq "foo"
    end

    it "updates quality of Aged Brie as it gets older" do
      items = [Item.new("Aged Brie", 5, 10)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 11
    end

    it "updates quality of Sulfuras, Hand of Ragnaros" do
      items = [Item.new("Sulfuras, Hand of Ragnaros", 5, 80)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 80
    end

    it "updates quality of Conjured item twice as fast" do
      items = [Item.new("Conjured", 5, 10)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 6
    end

    it "updates quality of Backstage passes based on sell-in value" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 12, 10)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 11
    end

    it "sets quality of Backstage passes to 0 after concert" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 0, 10)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 0
    end
  end

end
