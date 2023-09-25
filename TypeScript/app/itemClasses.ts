export class Item {
  name: string;
  sellIn: number;
  quality: number;

  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

export class AgedBrie extends Item {
  constructor(sellIn, quality) {
    super("Aged Brie", sellIn, quality);
  }
  handleQuality() {
    if (this.quality >= 50 || this.quality === 0) return;
    if (this.sellIn === 0) {
      this.quality = this.quality / 2;
      return;
    }
    this.quality++;
  }

  handleSellIn() {
    this.sellIn--;
  }
}

export class Passes extends Item {
  constructor(sellIn, quality) {
    super("Backstage passes to a TAFKAL80ETC concert", sellIn, quality);
  }

  handleQuality() {
    if (this.quality >= 50 || this.quality === 0) return;

    if (this.sellIn === 0) {
      this.quality = 0;
      return;
    }

    if (6 <= this.sellIn && this.sellIn < 11) {
      this.quality += 2;
    }

    if (this.sellIn < 6) {
      this.quality += 3;
    }
  }

  handleSellIn() {
    this.sellIn--;
  }
}

export class Surfras extends Item {
  constructor(sellIn, quality) {
    super("Sulfuras, Hand of Ragnaros", sellIn, quality);
  }

  handleQuality() {}

  handleSellIn() {}
}
