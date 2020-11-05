require 'gilded_rose'

describe GildedRose do
let(:potato) { Item.new('potato', 15, 2)}
let(:sulfarus) { Item.new('Sulfuras, Hand of Ragnaros', 50, 80) }

  describe "feature tests" do
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
        items = [Item.new("old potato", -1, 20)]
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
      it 'doubles the increase when sell_in below 1' do
        items = [Item.new("Aged Brie", 0, 20)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 22
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

    describe 'conjured input' do
      it "should degrade twice as fast as normal items" do
        items = [Item.new("Conjured Armour", 10, 15)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 13
      end
      
      it "should not degrade past 0" do
        items = [Item.new("Conjured Armour", 10, 1)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 0
      end

      it "should degrade by 4 when sell in has passed" do
          items = [Item.new("Conjured Armour", -1, 17)]
          GildedRose.update_quality(items)
          expect(items.first.quality).to eq 13
      end

      it "still should not degrade past 0 when sell in has passed" do
        items = [Item.new("Conjured Armour", -1, 3)]
        GildedRose.update_quality(items)
        expect(items.first.quality).to eq 0
      end 
    end


    
 
end
  describe '#update_normal_quality' do
    it 'updates the quality of a normal item' do
      item_double = double :item, name: "potato", sell_in: 1, quality: 3

      expect(item_double).to receive(:quality=).with(2)
      GildedRose.update_normal_quality(item_double)
    end

    it 'does not update quality if quality == 0' do
      item_double = double :item, name: "potato", sell_in: 1, quality: 0
      expect(item_double).not_to receive(:quality=).with(-1)
      GildedRose.update_normal_quality(item_double)
    end

  end

  describe '#selfarus?' do
    it 'returns true on a selfarus item' do
      sulfuras_double = double :selfarus, name: 'Sulfuras', sell_in: 50, quality: 80
      expect(GildedRose.sulfuras?(sulfuras_double)).to eq true
    end

    it 'returns false on a non sulfuras item' do
      item_double = double :item, name: "potato", sell_in: 1, quality: 0
      expect(GildedRose.sulfuras?(item_double)).to eq false
    end
  end

  describe '#special_item?' do
    it 'returns true for aged brie' do
      item_double = double :item, name: "Aged Brie"
      expect(GildedRose.special_item?(item_double)).to eq true
    end

    it 'returns true for backstage passes' do
      item_double = double :item, name: "Backstage passes to a TAFKAL80ETC concert"
      expect(GildedRose.special_item?(item_double)).to eq true
    end

    it 'returns true for sulfuras' do
      sulfuras_double = double :selfarus, name: 'Sulfuras'
      expect(GildedRose.special_item?(sulfuras_double)).to eq true
    end 

    it 'returns false on potato' do
      item_double = double :item, name: "potato"
      expect(GildedRose.special_item?(item_double)).to eq false
    end
  end

  describe '#update_backstage_quality' do
     it 'increases in quality by one when sellIn is > 10 days' do
      items_double = double :item, name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 15, quality: 20
      expect(items_double).to receive(:quality=).with(21)
      GildedRose.update_backstage_quality(items_double)
      end

      it 'increases in quality by 2 when sellin < 10 days' do
        items_double = double :item, name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 9, quality: 20
        expect(items_double).to receive(:quality=).with(21).twice
        GildedRose.update_backstage_quality(items_double)
        
      end

      it 'increases by 3 when sellin < 5' do
        items_double = double :item, name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 4, quality: 20
        expect(items_double).to receive(:quality=).with(21).exactly(3).times
        GildedRose.update_backstage_quality(items_double)
      end

      it 'decreases to 0 when sellin == 0' do
        items_double = double :item, name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 0, quality: 20
        expect(items_double).to receive(:quality=).with(0)
        GildedRose.update_backstage_quality(items_double)
      end
  end

  describe '#update_brie_quality' do
    it 'should add one to the quality each day ' do
      brie_double = double :item, name: "Aged Brie", sell_in: 30, quality:15
      expect(brie_double).to receive(:quality=).with(16)
      GildedRose.update_brie_quality(brie_double)
    end

    it 'should not let quality go past 50' do
      brie_double = double :item, name: "Aged Brie", sell_in: 30, quality:50
      expect(brie_double).not_to receive(:quality=).with(51)
      GildedRose.update_brie_quality(brie_double)
    end
    it 'should increase by two when sellin <= 0' do
      brie_double = double :item, name: "Aged Brie", sell_in: 0, quality:2
      expect(brie_double).to receive(:quality=).with(4)
      GildedRose.update_brie_quality(brie_double)
    end


  end

  describe '#brie?' do
    it 'returns true on a brie item' do
      brie_double = double :brie, name: 'Aged Brie', sell_in: 50, quality: 80
      expect(GildedRose.brie?(brie_double)).to eq true
    end

    it 'returns false on a non brie item' do
      item_double = double :item, name: "potato", sell_in: 1, quality: 0
      expect(GildedRose.sulfuras?(item_double)).to eq false
    end
  end

  describe '#backstage?' do
    it 'returns true on a backstage item' do
      backstage_double = double :item, name: "Backstage passes to a TAFKAL80ETC concert"
      expect(GildedRose.backstage?(backstage_double)).to eq true
    end

    it 'returns false on a non brie item' do
      item_double = double :item, name: "potato", sell_in: 1, quality: 0
      expect(GildedRose.backstage?(item_double)).to eq false
    end
  end

 

  

end
