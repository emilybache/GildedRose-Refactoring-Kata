require 'rspec'

require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do

  describe "#update_quality" do
    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].name).to eq "foo"
    end

    it 'decreases the sell_in and quality for regular items' do
      item = Item.new('Regular Item', 5, 10)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(9)
      expect(item.sell_in).to eq(4)
    end

    it 'decreases quality twice as fast when sell_in is negative for regular items' do
      item = Item.new('Regular Item', 0, 10)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(8)
    end

    it 'increases the quality of Aged Brie' do
      item = Item.new('Aged Brie', 5, 10)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(11)
    end

    it 'does not increase the quality of Aged Brie beyond 50' do
      item = Item.new('Aged Brie', 5, 50)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(50)
    end

    it 'increases the quality of Backstage passes as sell_in approaches' do
      item = Item.new('Backstage passes to a TAFKAL80ETC concert', 11, 20)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(21)
    end

    it 'increases the quality of Backstage passes by 2 when sell_in is 10 or less' do
      item = Item.new('Backstage passes to a TAFKAL80ETC concert', 10, 20)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(22)
    end

    it 'increases the quality of Backstage passes by 3 when sell_in is 5 or less' do
      item = Item.new('Backstage passes to a TAFKAL80ETC concert', 5, 20)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(23)
    end

    it 'sets the quality of Backstage passes to 0 after the concert' do
      item = Item.new('Backstage passes to a TAFKAL80ETC concert', 0, 20)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(0)
    end

    it 'does not decrease the quality of Sulfuras' do
      item = Item.new('Sulfuras, Hand of Ragnaros', 5, 80)
      gilded_rose = GildedRose.new([item])
      gilded_rose.update_quality
      expect(item.quality).to eq(80)
    end
  end

end
