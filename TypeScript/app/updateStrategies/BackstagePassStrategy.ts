import Item from "./../models/Item";
import IUpdateStrategy from "./IUpdateStrategy";

export default class BackstagePassStrategy implements IUpdateStrategy {
    updateItem(item: Item) : Item {
        let increaseFactor = this.getIncreaseFactor(item.sellIn);
        let newQuality = 
            increaseFactor 
                ? Math.min(50, item.quality + increaseFactor) 
                : 0;
        let newSellIn = item.sellIn - 1;
        return new Item(item.name, newSellIn, newQuality);
    }

    private getIncreaseFactor(sellIn: number){
        if (sellIn > 10) return 1;
        if (sellIn <= 10 && sellIn > 5) return 2;
        if (sellIn <= 5 && sellIn > 0) return 3;
    }
}
