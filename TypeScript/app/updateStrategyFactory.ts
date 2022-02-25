import Item from "./models/Item";
import AgedBrieStrategy from "./updateStrategies/AgedBrieStrategy";
import BackstagePassStrategy from "./updateStrategies/BackstagePassStrategy";
import ConjuredStrategy from "./updateStrategies/conjuredStrategy";
import DefaultStrategy from "./updateStrategies/DefaultStrategy";
import IUpdateStrategy from "./updateStrategies/IUpdateStrategy";
import NoUpdateStrategy from "./updateStrategies/NoUpdateStrategy";

export class UpdateStrategyFactory {
    static getUpdateStrategy(item: Item): IUpdateStrategy {

        let qualityFactor = item.sellIn <= 0 ? 2 : 1;

        switch (item.name) {
            case 'Aged Brie':
                return new AgedBrieStrategy(qualityFactor);
            case 'Sulfuras, Hand of Ragnaros':
                return new NoUpdateStrategy();
            case 'Backstage passes to a TAFKAL80ETC concert':
                return new BackstagePassStrategy();
            case 'conjured':
                return new ConjuredStrategy(qualityFactor);
            default:
                return new DefaultStrategy(qualityFactor);
        }
    }
}