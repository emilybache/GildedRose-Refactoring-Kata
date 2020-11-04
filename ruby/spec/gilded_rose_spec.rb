require 'gilded_rose'

describe GildedRose do
let(:potato) { Item.new('potato', 15, 2)}
  describe "#update_quality" do
    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.update_quality(items)
      expect(items[0].name).to eq "foo"
    end

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

end
