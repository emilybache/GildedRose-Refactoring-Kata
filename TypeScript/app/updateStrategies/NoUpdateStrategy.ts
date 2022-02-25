import Item from "@/models/Item";
import IUpdateStrategy from "./IUpdateStrategy";

export default class NoUpdateStrategy implements IUpdateStrategy {
    updateItem(item: Item) : Item {        
        return new Item(item.name, item.sellIn, item.quality);
    }
}