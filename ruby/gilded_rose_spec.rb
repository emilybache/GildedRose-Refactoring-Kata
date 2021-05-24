require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do

  describe "#update_quality" do
    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].name).to eq "foo"
    end

    it "degrades quality at double speed when sell by date has passed" do
      items = [Item.new("foo", 0, 10)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 8
    end

    it "degrades quality at normal speed when sell by date hasn't passed" do
      items = [Item.new("foo", 1, 10)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 9
    end

    it "does not decrease quality when it's zero" do
      items = [Item.new("foo", 1, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 0
    end

    it "increase quality by 1 when item is Aged Brie and sell by date hasn't passed" do
      items = [Item.new("Aged Brie", 1, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 1
    end

    it "increase quality by 2 when item is Aged Brie and sell by date has passed" do
      items = [Item.new("Aged Brie", 0, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 2
    end

    it "does not increase quality when it's already 50" do
      items = [Item.new("Aged Brie", 0, 50)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 50
    end

    it "does not change quality and sell by date when it's a legendary item (Sulfuras)" do
      items = [Item.new("Sulfuras, Hand of Ragnaros", 0, 80)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 80
      expect(items[0].sell_in).to eq 0
    end

    it "increase quality by 1 when item is Backstage Passes and sell by date is bigger than 10" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 11, 1)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 2
    end
    
    it "increase quality by 2 when item is Backstage Passes and sell by date is less or equal 10" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 10, 1)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 3
    end

    it "increase quality by 3 when item is Backstage Passes and sell by date is less or equal 5" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 5, 1)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 4
    end

    it "sets quality to 0 when item is Backstage Passes and sell by date has passed" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 0, 50)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 0
    end

    it "degrades quality twice as fast for Conjured items" do
      items = [Item.new("Conjured sword", 10, 50), Item.new("Conjured axe", 0, 50)]
      GildedRose.new(items).update_quality()
      expect(items[0].quality).to eq 48
      expect(items[1].quality).to eq 46
    end
  end

end
