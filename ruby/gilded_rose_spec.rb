require File.join(File.dirname(__FILE__), "gilded_rose")

describe GildedRose do
  describe "#update_quality" do
    def update_items(items)
      GildedRose.new(items).update_quality
    end

    def updated_items(items)
      update_items(items)
      items
    end

    it "updates sell_in and quality of multiple items" do
      items = updated_items([
        Item.new("Dark Blade", 2, 3),
        Item.new("Aged Brie", 2, 3),
        Item.new("Backstage passes to a TAFKAL80ETC concert", 2, 3)
      ])
      expect(items[0].sell_in).to eq 1
      expect(items[0].quality).to eq 2
      expect(items[1].sell_in).to eq 1
      expect(items[1].quality).to eq 4
      expect(items[2].sell_in).to eq 1
      expect(items[2].quality).to eq 6
    end

    it "does not change the name" do
      items = updated_items([Item.new("foo", 0, 0)])
      expect(items[0].name).to eq "foo"
    end

    it "decreases sell_in by 1" do
      items = updated_items([Item.new("Dark Blade", 10, 5)])
      expect(items[0].sell_in).to eq 9

      items = updated_items([Item.new("Dark Blade", 0, 5)])
      expect(items[0].sell_in).to eq(-1)
    end

    it "decreases quality by 1" do
      items = updated_items([Item.new("Dark Sword", 10, 5)])
      expect(items[0].quality).to eq 4

      items = updated_items([Item.new("Dark Sword", 1, 5)])
      expect(items[0].quality).to eq 4
    end

    it "decreases quality down to 0" do
      items = updated_items([Item.new("Dark Sword", 10, 0)])
      expect(items[0].quality).to eq(0)
    end

    context "sell_in less than 1" do
      it "decreases quality by 2" do
        items = updated_items([Item.new("Dark Sword", 0, 10)])
        expect(items[0].quality).to eq(8)

        items = updated_items([Item.new("Dark Sword", -1, 10)])
        expect(items[0].quality).to eq(8)
      end

      it "decreases quality down to 0" do
        items = updated_items([Item.new("Dark Sword", 0, 0)])
        expect(items[0].quality).to eq(0)

        items = updated_items([Item.new("Dark Sword", 0, 1)])
        expect(items[0].quality).to eq(0)

        items = updated_items([Item.new("Dark Sword", 0, 2)])
        expect(items[0].quality).to eq(0)
      end
    end

    context "when process Aged Brie" do
      let(:item_name) { "Aged Brie" }
      it "increases quality by 1" do
        items = updated_items([Item.new(item_name, 10, 5)])
        expect(items[0].quality).to eq 6

        items = updated_items([Item.new(item_name, 1, 5)])
        expect(items[0].quality).to eq 6
      end

      it "updates quality up to 50" do
        items = updated_items([Item.new(item_name, 5, 50)])
        expect(items[0].quality).to eq 50
      end

      context "when sell_in is less than 1" do
        it "increases quality by 2" do
          items = updated_items([Item.new(item_name, 0, 5)])
          expect(items[0].quality).to eq 7

          items = updated_items([Item.new(item_name, 0, 48)])
          expect(items[0].quality).to eq 50
        end

        it "updates quality up to 50" do
          items = updated_items([Item.new(item_name, 0, 49)])
          expect(items[0].quality).to eq 50

          items = updated_items([Item.new(item_name, 0, 50)])
          expect(items[0].quality).to eq 50
        end
      end
    end

    context "when process Backstage passes to a TAFKAL80ETC concert" do
      let!(:item_name) { "Backstage passes to a TAFKAL80ETC concert" }

      context "when sell_in is greater than to 10" do
        it "increases quality by 1" do
          items = updated_items([Item.new(item_name, 11, 5)])
          expect(items[0].quality).to eq 6
        end

        it "increases quality up to 50" do
          items = updated_items([Item.new(item_name, 11, 50)])
          expect(items[0].quality).to eq 50
        end
      end

      context "when sell_in is greater than 5 and less than or equal to 10" do
        it "increases quality by 2" do
          items = updated_items([Item.new(item_name, 10, 5)])
          expect(items[0].quality).to eq 7

          items = updated_items([Item.new(item_name, 6, 5)])
          expect(items[0].quality).to eq 7
        end

        it "increases quality up to 50" do
          items = updated_items([Item.new(item_name, 10, 48)])
          expect(items[0].quality).to eq 50

          items = updated_items([Item.new(item_name, 10, 49)])
          expect(items[0].quality).to eq 50

          items = updated_items([Item.new(item_name, 10, 50)])
          expect(items[0].quality).to eq 50
        end
      end

      context "when sell_in is less than or equal to 5" do
        it "increases quality by 3" do
          items = updated_items([Item.new(item_name, 5, 5)])
          expect(items[0].quality).to eq 8

          items = updated_items([Item.new(item_name, 5, 0)])
          expect(items[0].quality).to eq 3

          items = updated_items([Item.new(item_name, 1, 5)])
          expect(items[0].quality).to eq 8
        end

        it "increases quality up to 50" do
          items = updated_items([Item.new(item_name, 5, 47)])
          expect(items[0].quality).to eq 50

          items = updated_items([Item.new(item_name, 5, 48)])
          expect(items[0].quality).to eq 50

          items = updated_items([Item.new(item_name, 5, 49)])
          expect(items[0].quality).to eq 50

          items = updated_items([Item.new(item_name, 5, 50)])
          expect(items[0].quality).to eq 50
        end
      end

      context "when sell_in is less than or equlal to 0" do
        it "sets quality to 0" do
          items = updated_items([Item.new(item_name, 0, 5)])
          expect(items[0].quality).to eq 0

          items = updated_items([Item.new(item_name, -1, 5)])
          expect(items[0].quality).to eq 0

          items = updated_items([Item.new(item_name, 0, 50)])
          expect(items[0].quality).to eq 0
        end
      end
    end

    context "when processing Sulfuras, Hand of Ragnaros" do
      let(:item_name) { "Sulfuras, Hand of Ragnaros" }

      it "does not change quality" do
        items = updated_items([Item.new(item_name, 1, 50)])
        expect(items[0].quality).to eq 50

        items = updated_items([Item.new(item_name, 0, 5)])
        expect(items[0].quality).to eq 5
      end

      it "does not change sell_in" do
        items = updated_items([Item.new(item_name, 1, 5)])
        expect(items[0].sell_in).to eq 1

        items = updated_items([Item.new(item_name, 0, 5)])
        expect(items[0].sell_in).to eq 0

        items = updated_items([Item.new(item_name, -1, 5)])
        expect(items[0].sell_in).to eq(-1)
      end
    end

    context "when process nil" do
      it { expect { update_items(nil) }.to raise_error }
    end

    context "when process an array which contains nil" do
      it { expect { update_items([nil]) }.to raise_error }
    end
  end
end
