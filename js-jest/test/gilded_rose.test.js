const { RegularItem } = require('../src/gilded_rose')

// describe('Gilded Rose', function () {
//   it('should foo', function () {
//     const gildedRose = new Shop([new Item('foo', 0, 0)])
//     const items = gildedRose.updateQuality()
//     expect(items[0].name).toBe('fixme')
//   })
// })

describe('RegularItem', () => {
  const getRegularItemFactory = (itemProps) => () => new RegularItem({
    name: 'Mock RegularItem',
    sellIn: 5,
    quality: 25,
    ...itemProps
  })

  it('should throw an error if initialized with an invalid name', () => {
    expect(getRegularItemFactory({ name: '' })).toThrow()
  })

  it('should throw an error if initialized with an invalid sellIn property', () => {
    expect(getRegularItemFactory({ sellIn: '' })).toThrow()
  })

  it('should throw an error if initialized with a quality property < 0 OR > 50', () => {
    expect(getRegularItemFactory({ quality: -1 })).toThrow()
    expect(getRegularItemFactory({ quality: 51 })).toThrow()
  })
})
