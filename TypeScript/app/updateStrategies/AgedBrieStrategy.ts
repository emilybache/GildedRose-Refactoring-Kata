import Item from "./../models/Item";
import IUpdateStrategy from "./IUpdateStrategy";

export default class AgedBrieStrategy implements IUpdateStrategy {    
    constructor(private qualityFactor : number) {}

    updateItem(item: Item) : Item {
        let quality = Math.min(50, item.quality + 1 * this.qualityFactor);
        let sellIn = item.sellIn - 1;
        return new Item(item.name, sellIn, quality);
    }
}
