import { Item, GildedRose } from "@/gilded-rose";

describe("GildedRose", () => {
  describe("updateQuality", () => {
    describe("Aged Brie", () => {
      it("should increase quality when updateQuality is called", () => {
        const items = [new Item("Aged Brie", 10, 10)];
        const gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        expect(gildedRose.items[0].quality).toBe(11);
      });

      it("should not increase quality above 50", () => {
        const items = [new Item("Aged Brie", 10, 50)];
        const gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        expect(gildedRose.items[0].quality).toBe(50);
      });

      it("should increase quality by 2 after sellIn date has passed", () => {
        const items = [new Item("Aged Brie", 0, 10)];
        const gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        expect(gildedRose.items[0].quality).toBe(12);
      });
    });
  });
});
