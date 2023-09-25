export class Item {
  name: string;
  sellIn: number;
  quality: number;

  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }

  handleQuality() {
    //
  }

  handleSellIn() {
    //
  }
}

export class AgedBrie extends Item {
  constructor(sellIn, quality) {
    super("Aged Brie", sellIn, quality);
  }
  handleQuality() {
    //
  }

  handleSellIn() {
    //
  }
}

export class Passes extends Item {
  constructor(sellIn, quality) {
    super("Backstage passes to a TAFKAL80ETC concert", sellIn, quality);
  }

  handleQuality() {
    //
  }

  handleSellIn() {
    //
  }
}

export class Surfras extends Item {
  constructor(sellIn, quality) {
    super("Sulfuras, Hand of Ragnaros", sellIn, quality);
  }
  handleQuality() {
    //
  }

  handleSellIn() {
    //
  }
}
