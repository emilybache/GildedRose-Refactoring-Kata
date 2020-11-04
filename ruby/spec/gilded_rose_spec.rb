require 'gilded_rose'

describe GildedRose do
let(:potato) { Item.new('potato', 15, 2)}
#let(:grain) {instance_double("item", name: "potato", :quality => 5)}
let(:sulfarus) { Item.new('Sulfuras, Hand of Ragnaros', 50, 80) }

  describe "#update_quality" do
    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.update_quality(items)
      expect(items[0].name).to eq "foo"
    end
    describe 'non-special item input' do
      it 'should decrese the quality of normal item' do
        items = [potato]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq (1)
      end

      it 'should decrese the sellIn of normal item' do
        items = [potato]
        GildedRose.update_quality(items)
        expect(items.first.sell_in).to eq (14)
      end

      it 'should decrease quality of normal items by 2 when sell_in date passes' do
        items = [Item.new("old potato", 0, 20)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq (18)
      end

      it 'should not decrease quality below 0' do
        items = [Item.new("old potato", 0, 0)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq (0)
      end
    end

    describe 'Aged Brie input' do
      it 'increases in quality as it ages' do
        items = [Item.new("Aged Brie", 25, 45)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 46
      end

      it 'cannot increase in quality beyond 50' do
        items = [Item.new("Aged Brie", 25, 50)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 50
      end
    end

    describe 'Sulfuras input' do
      it 'does not change' do
        items = [sulfarus]
        GildedRose.update_quality(items)
        expect(items[0].quality).to eq 80
        expect(items[0].sell_in).to eq 50
      end
    end

    describe 'backstage pass input' do
      it 'increases in quality by one when sellIn is > 10 days' do
        items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 15, 20)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 21
      end

      it 'increases in quality by 2 when sellin < 10 days' do
        items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 9, 20)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 22
      end

      it 'increases by 3 when sellin < 5' do
        items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 4, 20)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 23
      end

      it 'decreases to 0 when sellin == 0' do
        items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 0, 20)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 0
      end
    end
  end
  describe '#update_normal_quality' do
    it 'updates the quality of a normal item' do
      item_double = double :item, name: "potato", sellIn: 1, quality: 3
      expect(item_double).to receive(:quality=).with(2)
      GildedRose.update_normal_quality(item_double)
    end

    it 'does not update quality if quality == 0' do
      item_double = double :item, name: "potato", sellIn: 1, quality: 0
      expect(item_double).not_to receive(:quality=).with(-1)
      GildedRose.update_normal_quality(item_double)
    end

  end
 

  

end
