import { RegularItem } from '../src/gilded_rose'

// describe('Gilded Rose', function () {
//   it('should foo', function () {
//     const gildedRose = new Shop([new Item('foo', 0, 0)])
//     const items = gildedRose.updateQuality()
//     expect(items[0].name).toBe('fixme')
//   })
// })

describe('RegularItem', () => {
  const mockProperties = {
    name: 'Mock RegularItem',
    sellIn: 5,
    quality: 25
  }
  const getRegularItemFactory = (itemProps) => () => new RegularItem({
    ...mockProperties,
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

  it('[updateQuality] should decrement sellIn by 1 and quality by 1 when sellIn is >= 0', () => {
    const mockRegularItem = new RegularItem(mockProperties)
    mockRegularItem.updateQuality()
    expect(mockRegularItem.quality).toEqual(mockProperties.quality - 1)
  })

  it('[updateQuality] should decrement sellIn by 1 and quality by 2 when sellIn is < 0', () => {
    const mockRegularItem = new RegularItem({ ...mockProperties, sellIn: -1 })
    mockRegularItem.updateQuality()
    expect(mockRegularItem.quality).toEqual(mockProperties.quality - 2)
  })

  it('[updateQuality] should not decrement quality to a negative number', () => {
    const mockRegularItem = new RegularItem({ ...mockProperties, quality: 0, sellIn: -1 })
    mockRegularItem.updateQuality()
    expect(mockRegularItem.quality).toBeGreaterThanOrEqual(0)
  })
})
