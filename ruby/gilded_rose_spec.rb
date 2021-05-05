require File.join(File.dirname(__FILE__), "gilded_rose")

describe GildedRose do
  describe "#update_quality" do
    it "updates sell_in and quality of multiple items" do
      items = [
        Item.new("Dark Blade", 2, 3),
        Item.new("Aged Brie", 2, 3),
        Item.new("Backstage passes to a TAFKAL80ETC concert", 2, 3)
      ]
      GildedRose.new(items).update_quality
      expect(items[0].sell_in).to eq 1
      expect(items[0].quality).to eq 2
      expect(items[1].sell_in).to eq 1
      expect(items[1].quality).to eq 4
      expect(items[2].sell_in).to eq 1
      expect(items[2].quality).to eq 6
    end

    it "does not change the name" do
      items = [Item.new("foo", 0, 0)]
      GildedRose.new(items).update_quality
      expect(items[0].name).to eq "foo"
    end

    it "decreases sell_in by 1" do
      items = [Item.new("Dark Blade", 10, 5)]
      GildedRose.new(items).update_quality
      expect(items[0].sell_in).to eq 9

      items = [Item.new("Dark Blade", 0, 5)]
      GildedRose.new(items).update_quality
      expect(items[0].sell_in).to eq(-1)
    end

    it "decreases quality by 1" do
      items = [Item.new("Dark Sword", 10, 5)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq 4

      items = [Item.new("Dark Sword", 1, 5)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq 4
    end

    it "decreases quality down to 0" do
      items = [Item.new("Dark Sword", 10, 0)]
      GildedRose.new(items).update_quality
      expect(items[0].quality).to eq(0)
    end

    context "sell_in less than 1" do
      it "decreases quality by 2" do
        items = [Item.new("Dark Sword", 0, 10)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq(8)

        items = [Item.new("Dark Sword", -1, 10)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq(8)
      end

      it "decreases quality down to 0" do
        items = [Item.new("Dark Sword", 0, 0)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq(0)

        items = [Item.new("Dark Sword", 0, 1)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq(0)

        items = [Item.new("Dark Sword", 0, 2)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq(0)
      end
    end

    context "when process Aged Brie" do
      let(:item_name) { "Aged Brie" }
      it "increases quality by 1" do
        items = [Item.new(item_name, 10, 5)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq 6

        items = [Item.new(item_name, 1, 5)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq 6
      end

      it "updates quality up to 50" do
        items = [Item.new(item_name, 5, 50)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq 50
      end

      context "when sell_in is less than 1" do
        it "increases quality by 2" do
          items = [Item.new(item_name, 0, 5)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 7

          items = [Item.new(item_name, 0, 48)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50
        end

        it "updates quality up to 50" do
          items = [Item.new(item_name, 0, 49)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50

          items = [Item.new(item_name, 0, 50)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50
        end
      end
    end

    context "when process Backstage passes to a TAFKAL80ETC concert" do
      let!(:item_name) { "Backstage passes to a TAFKAL80ETC concert" }

      context "when sell_in is greater than to 10" do
        it "increases quality by 1" do
          items = [Item.new(item_name, 11, 5)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 6
        end

        it "increases quality up to 50" do
          items = [Item.new(item_name, 11, 50)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50
        end
      end

      context "when sell_in is greater than 5 and less than or equal to 10" do
        it "increases quality by 2" do
          items = [Item.new(item_name, 10, 5)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 7

          items = [Item.new(item_name, 6, 5)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 7
        end

        it "increases quality up to 50" do
          items = [Item.new(item_name, 10, 48)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50

          items = [Item.new(item_name, 10, 49)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50

          items = [Item.new(item_name, 10, 50)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50
        end
      end

      context "when sell_in is less than or equal to 5" do
        it "increases quality by 3" do
          items = [Item.new(item_name, 5, 5)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 8

          items = [Item.new(item_name, 5, 0)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 3

          items = [Item.new(item_name, 1, 5)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 8
        end

        it "increases quality up to 50" do
          items = [Item.new(item_name, 5, 47)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50

          items = [Item.new(item_name, 5, 48)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50

          items = [Item.new(item_name, 5, 49)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50

          items = [Item.new(item_name, 5, 50)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 50
        end
      end

      context "when sell_in is less than or equlal to 0" do
        it "sets quality to 0" do
          items = [Item.new(item_name, 0, 5)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 0

          items = [Item.new(item_name, -1, 5)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 0

          items = [Item.new(item_name, 0, 50)]
          GildedRose.new(items).update_quality
          expect(items[0].quality).to eq 0
        end
      end
    end

    context "when processing Sulfuras, Hand of Ragnaros" do
      let(:item_name) { "Sulfuras, Hand of Ragnaros" }

      it "does not change quality" do
        items = [Item.new(item_name, 1, 50)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq 50

        items = [Item.new(item_name, 0, 5)]
        GildedRose.new(items).update_quality
        expect(items[0].quality).to eq 5
      end

      it "does not change sell_in" do
        items = [Item.new(item_name, 1, 5)]
        GildedRose.new(items).update_quality
        expect(items[0].sell_in).to eq 1

        items = [Item.new(item_name, 0, 5)]
        GildedRose.new(items).update_quality
        expect(items[0].sell_in).to eq 0

        items = [Item.new(item_name, -1, 5)]
        GildedRose.new(items).update_quality
        expect(items[0].sell_in).to eq(-1)
      end
    end
  end
end
