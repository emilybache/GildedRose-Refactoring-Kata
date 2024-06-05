require 'rspec'

require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do
  let(:items) { [Item.new("foo", 10, 20)] }
  let(:gilded_rose) { GildedRose.new(items) }

  it 'decreases quality and sell in' do
    gilded_rose.update_quality
    expect(items[0].sell_in).to eq(9)
    expect(items[0].quality).to eq(19)
  end

  it "not decrease quality below 0" do
    items[0].quality = 0
    gilded_rose.update_quality
    expect(items[0].quality).to eq(0) 
  end

  context "with Aged Brie" do
    let(:items) { [Item.new("Aged Brie", 10, 20)] }

    it 'quality never be more than 50' do
      items[0].quality = 50
      gilded_rose.update_quality
      expect(items[0].quality).to eq(50)
    end

    it 'increases proportionally to time until 10' do
      items[0].sell_in = 7
      gilded_rose.update_quality
      expect(items[0].quality).to eq(22)
    end

    it 'increases proportionally to time until 5' do
      items[0].sell_in = 5
      gilded_rose.update_quality
      expect(items[0].quality).to eq(25)
    end
  end

  context 'with Conjured' do
    let(:items) { [Item.new("Conjured", 10, 20)] }

    it 'decreases by 2' do
      gilded_rose.update_quality
      expect(items[0].quality).to eq(18)
    end
  end
end
