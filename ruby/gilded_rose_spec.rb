require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do
  describe "#update_quality" do
    def update_and_check_item(item_name:, initial_sell_in:, initial_quant:, output:)
      items = [Item.new(item_name, initial_sell_in, initial_quant)]
      GildedRose.new(items).update_quality()

      expect(items[0].to_s).to eq output
    end

    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.new(items).update_quality()
      expect(items[0].name).to eq "foo"
    end

    it "raises error if incorrect sell_in value is passed" do
      items = [Item.new("foo", "invalid", 0)]
      expect { GildedRose.new(items).update_quality() }.to raise_error(StandardError)
    end

    it "raises error if incorrect quality value is passed" do
      items = [Item.new("foo", 0, "invalid")]
      expect { GildedRose.new(items).update_quality() }.to raise_error(StandardError)
    end

    context "Standard Items" do
       it "decreases quality and sell_in by 1 while sell_in approaches 0" do
        update_and_check_item(item_name: "+5 Dexterity Vest", initial_sell_in: 10, initial_quant: 20, output: "+5 Dexterity Vest, 9, 19")
      end

      it "decreases quality by 2 when sell_in value reaches 0" do
        update_and_check_item(item_name: "+5 Dexterity Vest", initial_sell_in: 0, initial_quant: 10, output: "+5 Dexterity Vest, -1, 8")
      end

      it "checks for quality to never be less than 0" do
        update_and_check_item(item_name: "+5 Dexterity Vest", initial_sell_in: 10, initial_quant: 0, output: "+5 Dexterity Vest, 9, 0")
      end
    end

    context "Sulfuras, Hand of Ragnaros" do
      it "never changes it's sell_in value and quantity" do
        update_and_check_item(item_name: "Sulfuras, Hand of Ragnaros", initial_sell_in: 0, initial_quant: 80, output: "Sulfuras, Hand of Ragnaros, 0, 80")
        update_and_check_item(item_name: "Sulfuras, Hand of Ragnaros", initial_sell_in: -1, initial_quant: 80, output: "Sulfuras, Hand of Ragnaros, -1, 80")
      end
    end

    context "Aged Brie" do
      it "increases quality and decreases sell_in by 1" do
        update_and_check_item(item_name: "Aged Brie", initial_sell_in: 2, initial_quant: 0, output: "Aged Brie, 1, 1")
      end

      it "increases quality by 2 when sell in value reaches 0" do
        update_and_check_item(item_name: "Aged Brie", initial_sell_in: 0, initial_quant: 2, output: "Aged Brie, -1, 4")
      end

      it "checks for quality to never be more than 50" do
        update_and_check_item(item_name: "Aged Brie", initial_sell_in: 10, initial_quant: 50, output: "Aged Brie, 9, 50")
      end
    end


    context "Backstage passes to a TAFKAL80ETC concert" do
      it "quality increases by 1 when sell_in is neither be 10 or nor 5 " do
        update_and_check_item(item_name: "Backstage passes to a TAFKAL80ETC concert", initial_sell_in: 12, initial_quant: 15, output: "Backstage passes to a TAFKAL80ETC concert, 11, 16")
      end

      it "quality increases by 2 when sell_in 10" do
        update_and_check_item(item_name: "Backstage passes to a TAFKAL80ETC concert", initial_sell_in: 10, initial_quant: 15, output: "Backstage passes to a TAFKAL80ETC concert, 9, 17")
      end

      it "quality increases by 3 when sell_in 5" do
        update_and_check_item(item_name: "Backstage passes to a TAFKAL80ETC concert", initial_sell_in: 5, initial_quant: 15, output: "Backstage passes to a TAFKAL80ETC concert, 4, 18")
      end

      it "quality drops to 0 after the sell_in 0" do
        update_and_check_item(item_name: "Backstage passes to a TAFKAL80ETC concert", initial_sell_in: 0, initial_quant: 3, output: "Backstage passes to a TAFKAL80ETC concert, -1, 0")
      end

      it "quality should not be greater than 50" do
        update_and_check_item(item_name: "Backstage passes to a TAFKAL80ETC concert", initial_sell_in: 10, initial_quant: 49, output: "Backstage passes to a TAFKAL80ETC concert, 9, 50")
      end

      context "Conjured Items" do
        it "decreases quality twice as fast as normal items" do
          update_and_check_item(item_name: "Conjured Mana Cake", initial_sell_in: 3, initial_quant: 6, output: "Conjured Mana Cake, 2, 4")
        end

        it "decreases quality by 4 when sell_in reaches 0" do
          update_and_check_item(item_name: "Conjured Mana Cake", initial_sell_in: 0, initial_quant: 6, output: "Conjured Mana Cake, -1, 2")
        end

        it "does not decrease quality below 0" do
          update_and_check_item(item_name: "Conjured Mana Cake", initial_sell_in: 5, initial_quant: 1, output: "Conjured Mana Cake, 4, 0")
        end
      end
    end
  end
end
