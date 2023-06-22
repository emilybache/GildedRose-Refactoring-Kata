export class Item {
  name: string;
  sellIn: number;
  quality: number;

  constructor(name: string, sellIn: number, quality: number) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

export class GildedRose {
  private QUALITY_LIM = 50;
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality() {
    return this.items.map((item) => {
      if (this._isAgeable(item.name)) {
        return this._decayAgeableItem(item);
      } else if (this._isLegendary(item.name)) {
        return this._decayLegendaryItem(item);
      } else if (this._isBackstagePass(item.name)) {
        return this._decayBackstagePassItem(item);
      } else {
        return this._decayGeneralItem(item);
      }
    });
  }

  private _isAgeable = (name: string): boolean => name === "Aged Brie";

  private _isLegendary = (name: string): boolean =>
    name === "Sulfuras, Hand of Ragnaros";

  private _isBackstagePass = (name: string): boolean =>
    name === "Backstage passes to a TAFKAL80ETC concert";

  private _isConjured = (name: string): boolean => name.includes("Conjured");

  private _decayGeneralItem = (item: Item): Item => {
    const newItem = {
      name: item.name,
      quality: item.quality - this._getDecayAmount(item),
      sellIn: item.sellIn - 1,
    };
    return this._applyQualityLimitation(newItem);
  };

  private _decayLegendaryItem = (item: Item): Item => {
    return item;
  };

  private _decayAgeableItem = (item: Item): Item => {
    const newItem = {
      name: item.name,
      quality: item.quality + 1,
      sellIn: item.sellIn - 1,
    };
    return this._applyQualityLimitation(newItem);
  };

  private _decayBackstagePassItem = (item: Item): Item => {
    let newQuality;
    if (item.sellIn > 10) {
      newQuality = item.quality + 1;
    } else if (item.sellIn > 5) {
      newQuality = item.quality + 2;
    } else if (item.sellIn > 0) {
      newQuality = item.quality + 3;
    } else {
      newQuality = 0;
    }
    const newItem = {
      name: item.name,
      quality: newQuality,
      sellIn: item.sellIn - 1,
    };
    return this._applyQualityLimitation(newItem);
  };

  private _applyQualityLimitation = (item: Item): Item => {
    let newQuality = item.quality;
    if (item.quality > this.QUALITY_LIM) {
      newQuality = this.QUALITY_LIM;
    } else if (item.quality < 0) {
      newQuality = 0;
    }
    return {
      name: item.name,
      quality: newQuality,
      sellIn: item.sellIn,
    };
  };

  private _getDecayAmount = (item: Item): number => {
    const decayAmount = item.sellIn > 0 ? 1 : 2;
    if (this._isConjured(item.name)) {
      return decayAmount * 2;
    } else {
      return decayAmount;
    }
  };
}
