require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do
  describe "#update_quality" do
    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].name).to eq "foo"
    end

    it 'ages regular items correctly when sell_in exceeds quality' do
      items = [Item.new("foo", 4, 2)]
      updater = GildedRose.new(items)
      updater.update_quality()

      expect(items[0].sell_in).to eq 3
      expect(items[0].quality).to eq 1

      2.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq 1
      expect(items[0].quality).to eq 0 # does not go below zero

      3.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq -2 # does go below zero
      expect(items[0].quality).to eq 0 # does not go below zero
    end

    it 'ages regular items correctly when quality exceeds sell_in' do
      items = [Item.new("foo", 2, 8)]
      updater = GildedRose.new(items)
      2.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq 0
      expect(items[0].quality).to eq 6 # drops 1 quality per call when sell_in >= zero

      2.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq -2
      expect(items[0].quality).to eq 2 # drops 2 quality per call when sell_in < zero

      2.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq -4
      expect(items[0].quality).to eq 0 # does not go below zero
    end

    it 'ages brie' do
      items = [Item.new("Aged Brie", 1, 46)]
      updater = GildedRose.new(items)
      updater.update_quality()

      expect(items[0].sell_in).to eq 0
      expect(items[0].quality).to eq 47 # increases 1 quality per call when sell_in >= zero

      updater.update_quality()

      expect(items[0].sell_in).to eq -1
      expect(items[0].quality).to eq 49 # increases 2 quality per call when sell_in < zero

      2.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq -3
      expect(items[0].quality).to eq 50 # does not exceed 50
    end

    it "ages concert tickets" do
      items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 11, 29)]
      updater = GildedRose.new(items)
      updater.update_quality()

      expect(items[0].sell_in).to eq 10
      expect(items[0].quality).to eq 30 # increases 1 quality per call when sell_in > 10

      5.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq 5
      expect(items[0].quality).to eq 40 # incrases 2 quality per call when sell_in is (6..10)

      3.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq 2
      expect(items[0].quality).to eq 49 # incrases 3 quality per call when sell_in is (6..10)

      2.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq 0
      expect(items[0].quality).to eq 50 # does not exceed 50

      updater.update_quality()

      expect(items[0].sell_in).to eq -1
      expect(items[0].quality).to eq 0 # too slow, now they are worth nothing!
    end

    it "seems Sulfuras would find the gilded lily to be a... second hand shop" do
      items = [Item.new("Sulfuras, Hand of Ragnaros", 1, 80)]
      updater = GildedRose.new(items)
      2.times do
        updater.update_quality()
      end

      expect(items[0].sell_in).to eq 1 # does not change
      expect(items[0].quality).to eq 80 # always 80
    end
  end

end
