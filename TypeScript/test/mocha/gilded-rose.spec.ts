import { expect } from 'chai';
import { Item, GildedRose } from '@/gilded-rose';

describe('Gilded Rose original updateQuality', () => {

  //random test:)

  it('should foo', () => {
    //given
    const gildedRose = new GildedRose([new Item('foo', 0, 0)]);
    //when
    const items = gildedRose.updateQuality();
    //then
    expect(items[0].name).to.equal('foo');
  });

  //standard item rules

  it('standard item sellin date not reached quality should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 4, 8)]);
    //when
    const items = gildedRose.updateQuality();
    //then
    expect(items[0].quality).to.equal(7);
  });
  it('standard item sellin date not reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 4, 8)]);
    //when
    const items = gildedRose.updateQuality();
    //then
    expect(items[0].sellIn).to.equal(3);
  });
  it('standard item sellin date reached quality should decrease by 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 4, 8)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    const items4 = gildedRose.updateQuality();
    const items5 = gildedRose.updateQuality();
    //then
    expect(items5[0].quality).to.equal(2);
  });
  it('standard item sellin date reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 4, 8)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    const items4 = gildedRose.updateQuality();
    const items5 = gildedRose.updateQuality();
    //then
    expect(items5[0].sellIn).to.equal(-1);
  });
  it('standard item sellin date reached 0 quality reached quality should stay at 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 1, 8)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    const items4 = gildedRose.updateQuality();
    const items5 = gildedRose.updateQuality();
    const items6 = gildedRose.updateQuality();
    //then
    expect(items6[0].quality).to.equal(0);
  });
  it('standard item sellin date reached 0 quality reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 1, 8)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    const items4 = gildedRose.updateQuality();
    const items5 = gildedRose.updateQuality();
    const items6 = gildedRose.updateQuality();
    //then
    expect(items6[0].sellIn).to.equal(-5);
  });
  it('standard item sellin date not reached 0 quality reached quality should stay at 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 5, 1)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].quality).to.equal(0);
  });
  it('standard item sellin date not reached 0 quality reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 5, 1)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].sellIn).to.equal(2);
  });

  //sulfuras tests

  it('sulfuras item sellin date not reached quality should stay at 80', () => {
    //given
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 2, 80)]);
    //when
    const items = gildedRose.updateQuality();
    //then
    expect(items[0].quality).to.equal(80);
  });
  it('sulfuras item sellin date not reached sellin should stay at 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 2, 80)]);
    //when
    const items = gildedRose.updateQuality();
    //then
    expect(items[0].sellIn).to.equal(2);
  });
  it('sulfuras item sellin date reached quality should stay at 80', () => {
    //given
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', -4, 80)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].quality).to.equal(80);
  });
  it('sulfuras item sellin date reached sellin should stay at -4', () => {
    //given
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', -4, 80)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].sellIn).to.equal(-4);
  });

  //aged brie tests

  it('Aged brie item sellin date not reached quality should increase by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 4, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    //then
    expect(items1[0].quality).to.equal(14);
  });
  it('Aged brie item sellin date not reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 4, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    //then
    expect(items1[0].sellIn).to.equal(3);
  });
  it('Aged brie item sellin date reached quality should increase by 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].quality).to.equal(18);
  });
  it('Aged brie item sellin date reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].sellIn).to.equal(-2);
  });
  it('Aged brie item sellin date reached, quality reached 50, quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 1, 48)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].quality).to.equal(50);
  });
  it('Aged brie item sellin date reached, quality reached 50, sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].sellIn).to.equal(-2);
  });
  it('Aged brie item sellin date not reached, quality reached 50, quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 5, 48)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].quality).to.equal(50);
  });
  it('Aged brie item sellin date not reached, quality reached 50, sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 5, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].sellIn).to.equal(2);
  });

  //backstage passes tests

  it('Backstage pass item sellin date reached quality should go to 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].quality).to.equal(0);
  });
  it('Backstage pass item sellin date reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].sellIn).to.equal(-1);
  });
  it('Backstage pass item sellin date > 10 quality should increase by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 12, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].quality).to.equal(15);
  });
  it('Backstage pass item sellin date > 10 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 12, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].sellIn).to.equal(10);
  });
  it('Backstage pass item 10 >= sellin date > 5 quality should increase by 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 7, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].quality).to.equal(17);
  });
  it('Backstage pass item 10 >= sellin date > 5 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 7, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].sellIn).to.equal(5);
  });
  it('Backstage pass item 5 >= sellin date >= 0 quality should increase by 3', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 2, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].quality).to.equal(19);
  });
  it('Backstage pass item 5 >= sellin date >=0 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 2, 13)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].sellIn).to.equal(0);
  });

  it('Backstage pass item sellin date > 10 and quality reaches 50 quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 15, 49)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].quality).to.equal(50);
  });
  it('Backstage pass item sellin date > 10 and quality reaches 50 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 15, 49)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].sellIn).to.equal(13);
  });
  it('Backstage pass item 10 >= sellin date > 5 and quality reaches 50 quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 7, 49)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].quality).to.equal(50);
  });
  it('Backstage pass item 10 >= sellin date > 5 and quality reaches 50 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 7, 49)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].sellIn).to.equal(5);
  });
  it('Backstage pass item 5 >= sellin date >= 0 and quality reaches 50 quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 2, 48)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].quality).to.equal(50);
  });
  it('Backstage pass item 5 >= sellin date >=0 and quality reaches 50 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 2, 48)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    //then
    expect(items2[0].sellIn).to.equal(0);
  });

  //conjured items

  it('conjured item sellin date not reached quality should decrease by 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 4, 8)]);
    //when
    const items = gildedRose.updateQuality();
    //then
    expect(items[0].quality).to.equal(6);
  });
  it('conjured item sellin date not reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 4, 8)]);
    //when
    const items = gildedRose.updateQuality();
    //then
    expect(items[0].sellIn).to.equal(3);
  });
  it('conjured item sellin date reached quality should decrease by 4', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 4, 14)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    const items4 = gildedRose.updateQuality();
    const items5 = gildedRose.updateQuality();
    //then
    expect(items5[0].quality).to.equal(2);
  });
  it('conjured item sellin date reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 4, 14)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    const items4 = gildedRose.updateQuality();
    const items5 = gildedRose.updateQuality();
    //then
    expect(items5[0].sellIn).to.equal(-1);
  });
  it('conjured item sellin date reached 0 quality reached quality should stay at 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 1, 8)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    const items4 = gildedRose.updateQuality();
    const items5 = gildedRose.updateQuality();
    const items6 = gildedRose.updateQuality();
    //then
    expect(items6[0].quality).to.equal(0);
  });
  it('conjured item sellin date reached 0 quality reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 1, 8)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    const items4 = gildedRose.updateQuality();
    const items5 = gildedRose.updateQuality();
    const items6 = gildedRose.updateQuality();
    //then
    expect(items6[0].sellIn).to.equal(-5);
  });
  it('conjured item sellin date not reached 0 quality reached quality should stay at 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 5, 1)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].quality).to.equal(0);
  });
  it('conjured item sellin date not reached 0 quality reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 5, 1)]);
    //when
    const items1 = gildedRose.updateQuality();
    const items2 = gildedRose.updateQuality();
    const items3 = gildedRose.updateQuality();
    //then
    expect(items3[0].sellIn).to.equal(2);
  });
});

describe('Gilded Rose new updateQuality2', () => {

  //random test:)

  it('should foo', () => {
    //given
    const gildedRose = new GildedRose([new Item('foo', 0, 0)]);
    //when
    const items = gildedRose.updateQuality2();
    //then
    expect(items[0].name).to.equal('foo');
  });

  //standard item rules

  it('standard item sellin date not reached quality should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 4, 8)]);
    //when
    const items = gildedRose.updateQuality2();
    //then
    expect(items[0].quality).to.equal(7);
  });
  it('standard item sellin date not reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 4, 8)]);
    //when
    const items = gildedRose.updateQuality2();
    //then
    expect(items[0].sellIn).to.equal(3);
  });
  it('standard item sellin date reached quality should decrease by 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 4, 8)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    const items4 = gildedRose.updateQuality2();
    const items5 = gildedRose.updateQuality2();
    //then
    expect(items5[0].quality).to.equal(2);
  });
  it('standard item sellin date reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 4, 8)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    const items4 = gildedRose.updateQuality2();
    const items5 = gildedRose.updateQuality2();
    //then
    expect(items5[0].sellIn).to.equal(-1);
  });
  it('standard item sellin date reached 0 quality reached quality should stay at 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 1, 8)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    const items4 = gildedRose.updateQuality2();
    const items5 = gildedRose.updateQuality2();
    const items6 = gildedRose.updateQuality2();
    //then
    expect(items6[0].quality).to.equal(0);
  });
  it('standard item sellin date reached 0 quality reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 1, 8)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    const items4 = gildedRose.updateQuality2();
    const items5 = gildedRose.updateQuality2();
    const items6 = gildedRose.updateQuality2();
    //then
    expect(items6[0].sellIn).to.equal(-5);
  });
  it('standard item sellin date not reached 0 quality reached quality should stay at 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 5, 1)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].quality).to.equal(0);
  });
  it('standard item sellin date not reached 0 quality reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('ceva', 5, 1)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].sellIn).to.equal(2);
  });

  //sulfuras tests

  it('sulfuras item sellin date not reached quality should stay at 80', () => {
    //given
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 2, 80)]);
    //when
    const items = gildedRose.updateQuality2();
    //then
    expect(items[0].quality).to.equal(80);
  });
  it('sulfuras item sellin date not reached sellin should stay at 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 2, 80)]);
    //when
    const items = gildedRose.updateQuality2();
    //then
    expect(items[0].sellIn).to.equal(2);
  });
  it('sulfuras item sellin date reached quality should stay at 80', () => {
    //given
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', -4, 80)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].quality).to.equal(80);
  });
  it('sulfuras item sellin date reached sellin should stay at -4', () => {
    //given
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', -4, 80)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].sellIn).to.equal(-4);
  });

  //aged brie tests

  it('Aged brie item sellin date not reached quality should increase by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 4, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    //then
    expect(items1[0].quality).to.equal(14);
  });
  it('Aged brie item sellin date not reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 4, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    //then
    expect(items1[0].sellIn).to.equal(3);
  });
  it('Aged brie item sellin date reached quality should increase by 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].quality).to.equal(18);
  });
  it('Aged brie item sellin date reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].sellIn).to.equal(-2);
  });
  it('Aged brie item sellin date reached, quality reached 50, quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 1, 48)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].quality).to.equal(50);
  });
  it('Aged brie item sellin date reached, quality reached 50, sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].sellIn).to.equal(-2);
  });
  it('Aged brie item sellin date not reached, quality reached 50, quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 5, 48)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].quality).to.equal(50);
  });
  it('Aged brie item sellin date not reached, quality reached 50, sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Aged Brie', 5, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].sellIn).to.equal(2);
  });

  //backstage passes tests

  it('Backstage pass item sellin date reached quality should go to 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].quality).to.equal(0);
  });
  it('Backstage pass item sellin date reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 1, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].sellIn).to.equal(-1);
  });
  it('Backstage pass item sellin date > 10 quality should increase by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 12, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].quality).to.equal(15);
  });
  it('Backstage pass item sellin date > 10 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 12, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].sellIn).to.equal(10);
  });
  it('Backstage pass item 10 >= sellin date > 5 quality should increase by 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 7, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].quality).to.equal(17);
  });
  it('Backstage pass item 10 >= sellin date > 5 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 7, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].sellIn).to.equal(5);
  });
  it('Backstage pass item 5 >= sellin date >= 0 quality should increase by 3', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 2, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].quality).to.equal(19);
  });
  it('Backstage pass item 5 >= sellin date >=0 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 2, 13)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].sellIn).to.equal(0);
  });

  it('Backstage pass item sellin date > 10 and quality reaches 50 quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 15, 49)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].quality).to.equal(50);
  });
  it('Backstage pass item sellin date > 10 and quality reaches 50 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 15, 49)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].sellIn).to.equal(13);
  });
  it('Backstage pass item 10 >= sellin date > 5 and quality reaches 50 quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 7, 49)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].quality).to.equal(50);
  });
  it('Backstage pass item 10 >= sellin date > 5 and quality reaches 50 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 7, 49)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].sellIn).to.equal(5);
  });
  it('Backstage pass item 5 >= sellin date >= 0 and quality reaches 50 quality should stay at 50', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 2, 48)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].quality).to.equal(50);
  });
  it('Backstage pass item 5 >= sellin date >=0 and quality reaches 50 sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 2, 48)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    //then
    expect(items2[0].sellIn).to.equal(0);
  });

  //conjured items

  it('conjured item sellin date not reached quality should decrease by 2', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 4, 8)]);
    //when
    const items = gildedRose.updateQuality2();
    //then
    expect(items[0].quality).to.equal(6);
  });
  it('conjured item sellin date not reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 4, 8)]);
    //when
    const items = gildedRose.updateQuality2();
    //then
    expect(items[0].sellIn).to.equal(3);
  });
  it('conjured item sellin date reached quality should decrease by 4', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 4, 14)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    const items4 = gildedRose.updateQuality2();
    const items5 = gildedRose.updateQuality2();
    //then
    expect(items5[0].quality).to.equal(2);
  });
  it('conjured item sellin date reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 4, 14)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    const items4 = gildedRose.updateQuality2();
    const items5 = gildedRose.updateQuality2();
    //then
    expect(items5[0].sellIn).to.equal(-1);
  });
  it('conjured item sellin date reached 0 quality reached quality should stay at 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 1, 8)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    const items4 = gildedRose.updateQuality2();
    const items5 = gildedRose.updateQuality2();
    const items6 = gildedRose.updateQuality2();
    //then
    expect(items6[0].quality).to.equal(0);
  });
  it('conjured item sellin date reached 0 quality reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 1, 8)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    const items4 = gildedRose.updateQuality2();
    const items5 = gildedRose.updateQuality2();
    const items6 = gildedRose.updateQuality2();
    //then
    expect(items6[0].sellIn).to.equal(-5);
  });
  it('conjured item sellin date not reached 0 quality reached quality should stay at 0', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 5, 1)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].quality).to.equal(0);
  });
  it('conjured item sellin date not reached 0 quality reached sellin should decrease by 1', () => {
    //given
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 5, 1)]);
    //when
    const items1 = gildedRose.updateQuality2();
    const items2 = gildedRose.updateQuality2();
    const items3 = gildedRose.updateQuality2();
    //then
    expect(items3[0].sellIn).to.equal(2);
  });
});
