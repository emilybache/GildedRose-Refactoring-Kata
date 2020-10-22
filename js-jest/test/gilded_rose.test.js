import {
  Item,
  RegularItem,
  ConcertPass,
  AgedCheese,
  LegendaryItem,
  ConjuredItem,
  Shop,
  ShopV2
} from '../src/gilded_rose'

describe('Shop', function () {
  const mockItems = [
    new Item('+5 Dexterity Vest', 10, 20),
    new Item('Aged Brie', 2, 0),
    new Item('Elixir of the Mongoose', 5, 7),
    new Item('Sulfuras, Hand of Ragnaros', 0, 80),
    new Item('Sulfuras, Hand of Ragnaros', -1, 80),
    new Item('Backstage passes to a TAFKAL80ETC concert', 15, 20),
    new Item('Backstage passes to a TAFKAL80ETC concert', 10, 49),
    new Item('Backstage passes to a TAFKAL80ETC concert', 5, 49)
  ]

  // This test ensures the new logic matches the old one, until legacy code can be safely removed.
  it('results from Shop V1 and V2 should match after 100 iterations', () => {
    const gildedRose = new Shop(mockItems)
    const gildedRoseV2 = new ShopV2(mockItems)

    for (let day = 0; day < 100; day++) {
      gildedRose.items.forEach((itemV1, index) => {
        const itemV2 = gildedRoseV2.items[index]

        expect(itemV1.name).toEqual(itemV2.name)
        expect(itemV1.sellIn).toEqual(itemV2.sellIn)
        expect(itemV1.quality).toEqual(itemV2.quality)
      })
      gildedRose.updateQuality()
      gildedRoseV2.updateQuality()
    }
  })

  it('should initialize with an empty array when no items are provided', () => {
    const gildedRose = new Shop()
    expect(gildedRose.items.length).toEqual(0)
  })
})

describe('ShopV2', () => {
  it('should properly handle ConjuredItem', () => {
    const mockProps = {
      name: 'Conjured Mana Cake',
      sellIn: 3,
      quality: 6
    }
    const gildedRoseV2 = new ShopV2([new Item(mockProps.name, mockProps.sellIn, mockProps.quality)])
    const conjuredItem = gildedRoseV2.items[0]
    expect(conjuredItem.name).toEqual(mockProps.name)
    expect(conjuredItem.sellIn).toEqual(mockProps.sellIn)
    expect(conjuredItem.quality).toEqual(mockProps.quality)

    for (let day = 0; day < 5; day++) {
      expect(conjuredItem.sellIn).toEqual(mockProps.sellIn - day)

      const qualityModifier = day * conjuredItem.depreciationRate
      const expectedQuality = mockProps.quality - (qualityModifier)
      if (expectedQuality > 0) {
        expect(conjuredItem.quality).toEqual(expectedQuality)
      } else {
        expect(conjuredItem.quality).toEqual(0)
      }
      gildedRoseV2.updateQuality()
    }
  })

  it('should initialize with an empty array when no items are provided', () => {
    const gildedRoseV2 = new ShopV2()
    expect(gildedRoseV2.items.length).toEqual(0)
  })
})

describe('Item', () => {
  it('should initialize with a name, sellIn, and quality properties', () => {
    const mockProperties = {
      name: 'Mock RegularItem',
      sellIn: 5,
      quality: 25
    }
    const mockItem = new Item(mockProperties.name, mockProperties.sellIn, mockProperties.quality)
    expect(mockItem.name).toEqual(mockProperties.name)
    expect(mockItem.sellIn).toEqual(mockProperties.sellIn)
    expect(mockItem.quality).toEqual(mockProperties.quality)
  })
})

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
    const mockRegularItem = new RegularItem({ ...mockProperties })
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

  it('should never increment quality over 50', () => {
    const mockRegularItem = new RegularItem({ ...mockProperties, quality: 49 })
    mockRegularItem.depreciationRate = -1
    mockRegularItem.updateQuality()
    mockRegularItem.updateQuality()
    expect(mockRegularItem.quality).toEqual(50)
  })

  describe('RegularItem.isNameValid', () => {
    it('should return true only if "name" is a non-empty string', () => {
      const mockRegularItem = new RegularItem({ ...mockProperties })
      expect(mockRegularItem.isNameValid('')).toBeFalsy()
      expect(mockRegularItem.isNameValid('mock name')).toBeTruthy()
    })
  })

  describe('RegularItem.isSellInValid', () => {
    it('should return true only if "sellIn" is a number', () => {
      const mockRegularItem = new RegularItem({ ...mockProperties })
      expect(mockRegularItem.isSellInValid('')).toBeFalsy()
      expect(mockRegularItem.isSellInValid(-1)).toBeTruthy()
      expect(mockRegularItem.isSellInValid(0)).toBeTruthy()
      expect(mockRegularItem.isSellInValid(1)).toBeTruthy()
    })
  })

  describe('RegularItem.isSellInValid', () => {
    it('should return true only if "quality" is a number, between 0 and 50', () => {
      const mockRegularItem = new RegularItem({ ...mockProperties })
      expect(mockRegularItem.isQualityValid(-1)).toBeFalsy()
      expect(mockRegularItem.isQualityValid(51)).toBeFalsy()
      expect(mockRegularItem.isQualityValid(0)).toBeTruthy()
      expect(mockRegularItem.isQualityValid(25)).toBeTruthy()
      expect(mockRegularItem.isQualityValid(50)).toBeTruthy()
    })
  })

  describe('RegularItem.getDepreciationRate', () => {
    it('should return a normal depreciation rate when sellIn is >= 0', () => {
      const mockRegularItem = new RegularItem({ ...mockProperties })
      expect(mockRegularItem.getDepreciationRate()).toEqual(mockRegularItem.depreciationRate)
    })

    it('should return a doubled depreciation rate when sellIn is < 0', () => {
      const mockRegularItem = new RegularItem({ ...mockProperties, sellIn: -1 })
      expect(mockRegularItem.getDepreciationRate()).toEqual(mockRegularItem.depreciationRate * 2)
    })
  })

  describe('RegularItem.setQuality', () => {
    it('should set quality to whatever number is passed, unless it is over 50', () => {
      const mockRegularItem = new RegularItem({ ...mockProperties })
      mockRegularItem.setQuality(-1)
      expect(mockRegularItem.quality).toEqual(-1)
      mockRegularItem.setQuality(0)
      expect(mockRegularItem.quality).toEqual(0)
      mockRegularItem.setQuality(25)
      expect(mockRegularItem.quality).toEqual(25)
      mockRegularItem.setQuality(50)
      expect(mockRegularItem.quality).toEqual(50)
      mockRegularItem.setQuality(51)
      expect(mockRegularItem.quality).toEqual(50)
    })
  })
})

describe('ConcertPass', () => {
  const mockProperties = {
    name: 'Mock ConcertPass',
    sellIn: 5,
    quality: 25
  }

  it('should initialize with a negative depreciation rate (appreciation)', () => {
    const mockConcertPass = new ConcertPass(mockProperties)
    expect(mockConcertPass.depreciationRate).toBeLessThan(0)
    expect(mockConcertPass.depreciationRate).toEqual(-1)
  })

  it('should appreciate at normal rate when sellIn is > 10', () => {
    const mockConcertPass = new ConcertPass({ ...mockProperties, sellIn: 11 })
    mockConcertPass.updateQuality()
    expect(mockConcertPass.quality).toEqual(mockProperties.quality + 1)
  })

  it('should appreciate twice as fast when sellIn is <= 10', () => {
    const mockConcertPass = new ConcertPass({ ...mockProperties, sellIn: 10 })
    mockConcertPass.updateQuality()
    expect(mockConcertPass.quality).toEqual(mockProperties.quality + 2)
  })

  it('should appreciate three times as fast when sellIn is <= 5', () => {
    const mockConcertPass = new ConcertPass({ ...mockProperties })
    mockConcertPass.updateQuality()
    expect(mockConcertPass.quality).toEqual(mockProperties.quality + 3)
  })

  it('should have quality 0 when sellIn is < 0', () => {
    const mockConcertPass = new ConcertPass({ ...mockProperties, sellIn: -1 })
    mockConcertPass.updateQuality()
    expect(mockConcertPass.quality).toEqual(0)
  })

  describe('ConcertPass.getDepreciationRate', () => {
    it('should return "quality" when sellIn < 0', () => {
      const mockConcertPass = new ConcertPass({ ...mockProperties, sellIn: -1 })
      expect(mockConcertPass.getDepreciationRate()).toEqual(mockConcertPass.quality)
    })

    it('should return depreciationRate * 3 when sellIn <= 5', () => {
      const mockConcertPass = new ConcertPass({ ...mockProperties, sellIn: 5 })
      expect(mockConcertPass.getDepreciationRate()).toEqual(mockConcertPass.depreciationRate * 3)
      mockConcertPass.sellIn = 4
      expect(mockConcertPass.getDepreciationRate()).toEqual(mockConcertPass.depreciationRate * 3)
    })

    it('should return depreciationRate * 2 when sellIn <= 10', () => {
      const mockConcertPass = new ConcertPass({ ...mockProperties, sellIn: 10 })
      expect(mockConcertPass.getDepreciationRate()).toEqual(mockConcertPass.depreciationRate * 2)
      mockConcertPass.sellIn = 9
      expect(mockConcertPass.getDepreciationRate()).toEqual(mockConcertPass.depreciationRate * 2)
    })

    it('should return a normal depreciationRate when sellIn > 10', () => {
      const mockConcertPass = new ConcertPass({ ...mockProperties, sellIn: 11 })
      expect(mockConcertPass.getDepreciationRate()).toEqual(mockConcertPass.depreciationRate)
    })
  })
})

describe('AgedCheese', () => {
  const mockProperties = {
    name: 'Mock AgedCheese',
    sellIn: 5,
    quality: 25
  }

  it('should initialize with a negative depreciation rate (appreciation)', () => {
    const mockAgedCheese = new AgedCheese(mockProperties)
    expect(mockAgedCheese.depreciationRate).toBeLessThan(0)
    expect(mockAgedCheese.depreciationRate).toEqual(-1)
  })

  it('should appreciate at normal rate when sellIn is > 0', () => {
    const mockAgedCheese = new AgedCheese({ ...mockProperties })
    mockAgedCheese.updateQuality()
    expect(mockAgedCheese.quality).toEqual(mockProperties.quality + 1)
  })

  it('should appreciate twice as fast when sellIn is <= 0', () => {
    const mockAgedCheese = new AgedCheese({ ...mockProperties, sellIn: 0 })
    mockAgedCheese.updateQuality()
    expect(mockAgedCheese.quality).toEqual(mockProperties.quality + 2)
  })

  describe('AgedCheese.getDepreciationRate', () => {
    it('should return a depreciation rate (appreciation) twice as large when sellIn <= 0', () => {
      const mockAgedCheese = new AgedCheese({ ...mockProperties, sellIn: 0 })
      expect(mockAgedCheese.getDepreciationRate()).toEqual(mockAgedCheese.depreciationRate * 2)
      mockAgedCheese.sellIn = -1
      expect(mockAgedCheese.getDepreciationRate()).toEqual(mockAgedCheese.depreciationRate * 2)
    })
  })
})

describe('LegendaryItem', () => {
  const mockProperties = {
    name: 'Mock LegendaryItem',
    sellIn: 5,
    quality: 80
  }

  it('should be initialized with a depreciation rate equal to zero', () => {
    const mockLegendaryItem = new LegendaryItem({ ...mockProperties })
    expect(mockLegendaryItem.depreciationRate).toEqual(0)
  })

  it('should remain the same when the quality is updated', () => {
    const mockLegendaryItem = new LegendaryItem({ ...mockProperties })
    mockLegendaryItem.updateQuality()
    expect(mockLegendaryItem.quality).toEqual(mockProperties.quality)
    expect(mockLegendaryItem.sellIn).toEqual(mockProperties.sellIn)
  })

  describe('LegendaryItem.isQualityValid', () => {
    it('should return true only if quality is exactly 80', () => {
      const mockLegendaryItem = new LegendaryItem({ ...mockProperties })
      expect(mockLegendaryItem.isQualityValid('')).toBeFalsy()
      expect(mockLegendaryItem.isQualityValid(50)).toBeFalsy()
      expect(mockLegendaryItem.isQualityValid(80)).toBeTruthy()
    })
  })
})

describe('ConjuredItem', () => {
  const mockProperties = {
    name: 'Mock Item',
    sellIn: 5,
    quality: 25
  }
  it('should be initialized with a depreciation rate twice as large as a RegularItem', () => {
    const mockRegularItem = new RegularItem({ ...mockProperties })
    const mockConjuredItem = new ConjuredItem({ ...mockProperties })
    expect(mockConjuredItem.depreciationRate).toEqual(mockRegularItem.depreciationRate * 2)
  })
})
