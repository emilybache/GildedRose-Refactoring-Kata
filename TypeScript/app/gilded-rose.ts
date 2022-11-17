import { throws } from "assert";
import {Item} from "./Item"


export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  updateQuality() {
    this.items.forEach(item => {
      this.update(item);
    });
    return this.items; 
  } 

  private update(item: Item){
    if(item.name == 'Sulfuras, Hand of Ragnaros') {
      return;
    }
    item.sellIn=item.sellIn-1;
    if(item.name=='Aged Brie'){
      this.increaseQuality(item); 
      if (item.sellIn<0) this.increaseQuality(item); 
    }else if ( item.name =='Backstage passes to a TAFKAL80ETC concert'){
      this.increaseQuality(item);
      if(item.sellIn<10) this.increaseQuality(item); 
      if(item.sellIn<5) this.increaseQuality(item); 
      if(item.sellIn<0) item.quality=0; 
    }else{
      this.decreaseQuality(item);
      if(item.sellIn <0) this.decreaseQuality(item); 
    }
  }


  private decreaseQuality(item :Item){
    if(item.quality>0){
      item.quality = item.quality - 1;
    }
  }

  private increaseQuality(item :Item){
    if(item.quality<50){
      item.quality = item.quality + 1;  
    }
  }
}
   
