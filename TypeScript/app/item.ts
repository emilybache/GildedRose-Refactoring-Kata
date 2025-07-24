export class Item {
  name: string;
  sellIn: number;
  quality: number;

  // can't edit this constructor because of the kata rules.
  // but I would change the constructor to take a javaScript object.
  // It makes the consumer code more readable.
  // e.g. new Item({ name: "standard item", sellIn: 0, quality: 2 })
  // instead of new Item("standard item", 0, 2)

  // I would also type the parameters, because now they are implicitly any.
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}
