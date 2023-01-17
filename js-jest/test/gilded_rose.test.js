const { Shop, Item } = require('../src/gilded_rose')

describe('Gilded Rose', function () {
  it('should foo', function () {
    const gildedRose = new Shop([new Item('foo', 0, 0)])
    const items = gildedRose.updateQuality()
    expect(items[0].name).toBe('foo')
  })

  it('should return correct result', () => {
    const storeItems = [
      new Item('Test Potion', 10, 20),
      new Item('Aged Brie', 2, 0),
      new Item('Healing Salve', 5, 7),
      new Item('Sulfuras', 0, 80),
      new Item('Backstage passes', 15, 20),
    ]

    const expectedResult = [
      new Item('Test Potion', 9, 19),
      new Item('Aged Brie', 1, 1),
      new Item('Healing Salve', 4, 6),
      new Item('Sulfuras', 0, 80),
      new Item('Backstage passes', 14, 21),
    ]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })

  it('Should normal items quality never be below 0', () => {
    const storeItems = [new Item('Oblivion staff', 10, 0)]
    const expectedResult = [new Item('Oblivion staff', 9, 0)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })

  it('Should quality degrade twice as fast when the sellIn date passes', () => {
    const storeItems = [new Item('Oblivion staff', 0, 4)]
    const expectedResult = [new Item('Oblivion staff', -1, 2)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })

  it('Should the quality of an item can never be more than 50', () => {
    const storeItems = [new Item('Aged Brie', 1, 50)]
    const expectedResult = [new Item('Aged Brie', 0, 50)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })

  it('Should quality of an aged brie should increase by 1', () => {
    const storeItems = [new Item('Aged Brie', 1, 0)]
    const expectedResult = [new Item('Aged Brie', 0, 1)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })
})

describe('Backstage passes', () => {
  it("increases in Quality as it's SellIn value approaches", () => {
    const storeItems = [new Item('Backstage passes', 14, 0)]
    const expectedResult = [new Item('Backstage passes', 13, 1)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })

  it('Quality increases by 2 when there are 10 days or less', () => {
    const storeItems = [new Item('Backstage passes', 10, 0)]
    const expectedResult = [new Item('Backstage passes', 9, 2)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })

  it('Quality increases by 3 when there are 5 days or less', () => {
    const storeItems = [new Item('Backstage passes', 5, 0)]
    const expectedResult = [new Item('Backstage passes', 4, 3)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })

  it('Quality drops to 0 after concert', () => {
    const storeItems = [new Item('Backstage passes', 0, 30)]
    const expectedResult = [new Item('Backstage passes', -1, 0)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })
})
describe('Conjured items', () => {
  it('Should quality of conjured items decrease twice as fast', () => {
    const storeItems = [new Item('Conjured Test Cake', 10, 20)]
    const expectedResult = [new Item('Conjured Test Cake', 9, 18)]
    const gildedRose = new Shop(storeItems)
    const items = gildedRose.updateQuality()

    expect(items).toStrictEqual(expectedResult)
  })
})
