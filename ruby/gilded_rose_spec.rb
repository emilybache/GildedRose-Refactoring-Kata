require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do

  describe "#update_quality" do
    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.new(items).update_quality
      expect(items[0].name).to eq "foo"
    end
  end

  describe "#classic_item_update" do
    it "changed sell in" do
      items = [Item.new("foo", 1, 0)]
      GildedRose.new(items).update_quality
      expect(items[0].sell_in).to eq(0)
    end

    it "changed quality if sell in negative" do
      items = [Item.new("foo", -1, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(8)
    end

    it "changed quality if sell in positive" do
      items = [Item.new("foo", 1, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(9)
    end
  end

  describe "#aged_brie_update" do
    it "changed sell in" do
      items = [Item.new("Aged Brie", 1, 0)]
      GildedRose.new(items).update_quality
      expect(items[0].sell_in).to eq(0)
    end

    it "changed quality if sell in negative" do
      items = [Item.new("Aged Brie", -1, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(12)
    end

    it "changed quality if sell in positive" do
      items = [Item.new("Aged Brie", 1, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(11)
    end
  end

  describe "#sulfuras_hand_of_ragnaros_update" do
    it "changed sell in" do
      items = [Item.new("Sulfuras, Hand of Ragnaros", 1, 80)]
      GildedRose.new(items).update_quality
      expect(items[0].sell_in).to eq(1)
    end

    it "changed quality if sell in negative" do
      items = [Item.new("Sulfuras, Hand of Ragnaros", -1, 80)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(80)
    end

    it "changed quality if sell in positive" do
      items = [Item.new("Sulfuras, Hand of Ragnaros", 1, 80)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(80)
    end
  end

  describe "#backstage_passes_to_a_tafkal80etc_concert_update" do
    it "changed sell in" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 1, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].sell_in).to eq(0)
    end

    it "changed quality if sell in < 6" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 5, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(13)
    end

    it "changed quality if sell in < 11" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 10, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(12)
    end
  end

  describe "#conjured_mana_cake_update" do
    it "changed sell in" do
      items = [Item.new("Conjured Mana Cake", 1, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].sell_in).to eq(0)
    end

    it "changed quality if sell in negative" do
      items = [Item.new("Conjured Mana Cake", -1, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(6)
    end

    it "changed quality if sell in positive" do
      items = [Item.new("Conjured Mana Cake", 1, 10)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(8)
    end
  end


end
