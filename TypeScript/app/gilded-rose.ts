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

export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality(){
    for (let i = 0; i < this.items.length; i++){
      this.items[i]= this.updateItemQuality(this.items[i]);
    }
    return this.items;
  }

  private updateItemQuality(item: Item){
     let nItem;
    if(item.name == 'Aged Brie'){
      nItem = this.updateAgedBrieQuality(item);
    } else if (item.name == 'Backstage passes to a TAFKAL80ETC concert'){
      nItem =  this.updateBackstagepassesQuality(item);
    } else if (item.name == 'Conjured Mana Cake'){
      nItem =  this.updateConjuredManaCakeQuality(item)
    }else if (item.name == 'Elixir of the Mongoose'){
      nItem =  this.updateElixirOfTheMongooseQuality(item);
    } else if (item.name == '+5 Dexterity Vest') {
     nItem =  this.updatePlus5DexterityVestQuality(item);
    }else if(item.name ! =  'Sulfuras, Hand of Ragnaros'){
      nItem =  this.updateStandardItemQuality(item);
    }
   
    return nItem;
    
  }

  public updateAgedBrieQuality(item: Item){
    const newItem = new Item(item.name, item.sellIn, item.quality);
    if (newItem.quality < 50){
      newItem.quality++
    }
    if(newItem.sellIn < 0 && newItem.quality < 50){
      newItem.quality++
    }
    newItem.sellIn--;
    return this.qualityCeilingCheck(newItem);
  }

  private qualityCeilingCheck(item: Item) {
    if (item.quality > 50) {
      item.quality = 50;
    }
    return item;
  }

  public updateBackstagepassesQuality(item: Item) {
    const newItem = new Item(item.name, item.sellIn, item.quality);
      if (newItem.sellIn <= 0) {
        newItem.quality = 0;
      } else if (newItem.sellIn <= 5) {
        newItem.quality += 3;
      } else if (newItem.sellIn <= 10) {
        newItem.quality += 2;
      } else if (newItem.sellIn > 10){
        newItem.quality ++;
      }
      if (newItem.quality > 50) {
        newItem.quality = 50;
      }
    
    newItem.sellIn--;
    return this.qualityCeilingCheck(newItem);
  }

  public updateConjuredManaCakeQuality(item: Item){
    const newItem = new Item(item.name, item.sellIn, item.quality);
    if (newItem.quality > 0){
      newItem.quality -= 2;
    }
    if(newItem.sellIn < 0 && newItem.quality > 0){
      newItem.quality -= 2;
    }
    newItem.sellIn--;
    return this.qualityCeilingCheck(newItem);
  }
  public updateElixirOfTheMongooseQuality (item: Item){
    const newItem = new Item(item.name, item.sellIn, item.quality);
    if(newItem.quality > 0) {
      newItem.quality--;
    }
    if (newItem.sellIn < 0 && newItem.quality > 0){
      newItem.quality--
    }
    newItem.sellIn--;
    return this.qualityCeilingCheck(newItem);
  }

  public updatePlus5DexterityVestQuality (item:Item){
    const newItem = new Item(item.name, item.sellIn, item.quality);
    if (newItem.quality > 0){
      newItem.quality--;
    }
    if (newItem.sellIn < 0 && newItem.quality > 0){
      newItem.quality--;
    }
    newItem.sellIn--;
    
    return this.qualityCeilingCheck(newItem);
  }

   public updateStandardItemQuality(item: Item){
    const newItem = new Item(item.name, item.sellIn, item.quality);
    if (newItem.name !== 'Sulfuras, Hand of Ragnaros') {
      if (newItem.quality > 0) {
        newItem.quality--;
      }
  }
  return newItem;
}
}



