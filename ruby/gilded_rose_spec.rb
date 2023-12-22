require 'rspec'
require_relative 'gilded_rose'

describe GildedRose do
  describe '#update_quality' do
    context 'with normal items' do
      it 'decreases quality and sell_in by 1' do
        items = [Item.new('Normal Item', 5, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(9)
        expect(items[0].sell_in).to eq(4)
      end

      it 'decreases quality twice as fast after sell by date' do
        items = [Item.new('Normal Item', 0, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(8)
      end

      it 'never sets quality to a negative value' do
        items = [Item.new('Normal Item', 5, 0)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(0)
      end
    end

    context 'with Aged Brie' do
      it 'increases in quality as it gets older' do
        items = [Item.new('Aged Brie', 5, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(11)
      end

      it 'increases in quality twice as fast after sell by date' do
        items = [Item.new('Aged Brie', 0, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(12)
      end

      it 'never sets quality above 50' do
        items = [Item.new('Aged Brie', 5, 50)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(50)
      end
    end

    context 'with Sulfuras' do
      it 'never changes quality or sell_in' do
        items = [Item.new('Sulfuras, Hand of Ragnaros', 5, 80)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(80)
        expect(items[0].sell_in).to eq(5)
      end
    end

    context 'with Backstage passes' do
      it 'increases quality by 1 when more than 10 days left' do
        items = [Item.new('Backstage passes to a TAFKAL80ETC concert', 15, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(11)
      end

      it 'increases quality by 2 when 10 days or less left' do
        items = [Item.new('Backstage passes to a TAFKAL80ETC concert', 10, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(12)
      end

      it 'increases quality by 3 when 5 days or less left' do
        items = [Item.new('Backstage passes to a TAFKAL80ETC concert', 5, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(13)
      end

      it 'drops quality to 0 after the concert' do
        items = [Item.new('Backstage passes to a TAFKAL80ETC concert', 0, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(0)
      end
    end

    context 'with Conjured items' do
      it 'degrades in quality twice as fast as normal items' do
        items = [Item.new('Conjured Mana Cake', 5, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(8)
      end

      it 'degrades in quality twice as fast after sell by date' do
        items = [Item.new('Conjured Mana Cake', 0, 10)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(6)
      end

      it 'never sets quality to a negative value' do
        items = [Item.new('Conjured Mana Cake', 5, 0)]
        gilded_rose = GildedRose.new(items)
        gilded_rose.update_quality
        expect(items[0].quality).to eq(0)
      end
    end
  end
end
