import { GildedRose } from '@/gilded-rose';
import Item from "@/models/Item";

describe('Gilded Rose', () => {
    it.each`
    name           | quality | expectedQuality | sellIn | expectedSellIn | description
    ${'abc def'}   | ${0}    | ${0}            | ${8}   | ${7}           | ${"quality should not be negative"}
    ${'abc def'}   | ${0}    | ${0}            | ${0}   | ${-1}          | ${"sellIn should decrease"}
    ${'abc def'}   | ${8}    | ${6}            | ${0}   | ${-1}          | ${"quality should decrease twice after sellIn is negative"}
    ${'Aged Brie'} | ${0}    | ${1}            | ${8}   | ${7}           | ${"should increase quality by 1"}
    ${'Aged Brie'} | ${0}    | ${2}            | ${0}   | ${-1}          | ${"should increase quality by 1"}
    ${'Aged Brie'} | ${50}   | ${50}           | ${8}   | ${7}           | ${"quality should not be higher than 50"}
    ${'conjured'}  | ${0}    | ${0}            | ${8}   | ${7}           | ${"quality should not be negative"}
    ${'conjured'}  | ${0}    | ${0}            | ${0}   | ${-1}          | ${"quality should not be negative"}
    ${'conjured'}  | ${4}    | ${2}            | ${8}   | ${7}           | ${"quality should decrease by 2"}
    ${'conjured'}  | ${4}    | ${0}            | ${0}   | ${-1}          | ${"quality should decrease by 4 when sellIn is <= 0"}
    ${'Sulfuras, Hand of Ragnaros'} | ${10} | ${10} | ${10} | ${10}      | ${"should not change "}
    ${'Backstage passes to a TAFKAL80ETC concert'} | ${10}| ${11}| ${11} | ${10} | ${"quality increases by 1 for sellIn > 10"}
    ${'Backstage passes to a TAFKAL80ETC concert'} | ${10}| ${11}| ${30} | ${29} | ${"quality increases by 1 for sellIn > 10"}
    ${'Backstage passes to a TAFKAL80ETC concert'} | ${10}| ${12}| ${10} | ${9} | ${"quality increases by 2 for sellIn <= 10 && > 5"}
    ${'Backstage passes to a TAFKAL80ETC concert'} | ${10}| ${12}| ${6}  | ${5} | ${"quality increases by 2 for sellIn <= 10 && > 5"}
    ${'Backstage passes to a TAFKAL80ETC concert'} | ${10}| ${13}| ${5}  | ${4} | ${"quality increases by 3 for sellIn <= 5 && >= 0"}
    ${'Backstage passes to a TAFKAL80ETC concert'} | ${10}| ${13}| ${1}  | ${0} | ${"quality increases by 3 for sellIn <= 5 && > 0"}
    ${'Backstage passes to a TAFKAL80ETC concert'} | ${10}| ${0} | ${0}  | ${-1} | ${"quality is 0 if sellIn is exceeded"}
    ${'Backstage passes to a TAFKAL80ETC concert'} | ${10}| ${0} | ${-1}  | ${-2} | ${"quality is 0 if sellIn is exceeded"}
`("'$description' for '$name' with quality '$quality' and sellIn '$sellIn'", ({name, quality, expectedQuality, sellIn, expectedSellIn, description}) => {
      const item1 = new Item(name, sellIn, quality);
      const items = GildedRose.updateQuality([item1]);
      expect(items[0].name).toBe(name);
      expect(items[0].quality).toBe(expectedQuality);
      expect(items[0].sellIn).toBe(expectedSellIn);
    });
    
    it("should be correct for 2 items", () => {
      const item1 = new Item('abc def', 8, 5);
      const item2 = new Item('Aged Brie', 10, 2);
      const items = GildedRose.updateQuality([item1, item2]);
      expect(items[0].name).toBe(item1.name);
      expect(items[0].quality).toBe(4);
      expect(items[0].sellIn).toBe(7);
      expect(items[1].name).toBe(item2.name);
      expect(items[1].quality).toBe(3);
      expect(items[1].sellIn).toBe(9);
    });
});

