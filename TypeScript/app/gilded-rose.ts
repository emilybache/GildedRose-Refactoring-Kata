// class to create items with name, sell by date, and quality
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
// class that creates an array where items will be stored
export class GildedRose {
    items: Array<Item>;

    constructor(items = [] as Array<Item>) {
        this.items = items;
    }

    // Method for updating item quality
    updateQuality() {
        var maxQuality = 50;
        var minQuality = 0;
        var conjured = 'Conjured';
        for (let i = 0; i < this.items.length; i++) {
            if (this.items[i].name != 'Aged Brie' && this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert' 
            && this.items[i].name != 'Sulfuras, Hand of Ragnaros' && this.items[i].quality >minQuality) {
                    this.items[i].quality -=1;
            } else {
                if (this.items[i].quality < maxQuality) {
                    this.items[i].quality += 1;
                    if (this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
                        if (this.items[i].sellIn <= 10 && this.items[i].sellIn > 5) {
                            // quality goes up by 2 when days are 10 or less
                            if (this.items[i].quality < maxQuality) {
                                this.items[i].quality += 1;
                            }
                        }
                       // quality goes up by 3 when days are 5 of less
                        else if (this.items[i].sellIn <= 5) {
                            if (this.items[i].quality < maxQuality) {
                                this.items[i].quality += 2;
                            }
                        }
                    }
                }
            }
            // checking for legendary to subtract sell by date 
            if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
                this.items[i].sellIn -= 1;
            }
            
            if (this.items[i].sellIn < 0) {
                if (this.items[i].name != 'Aged Brie') {
                    if (this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
                        if (this.items[i].quality > minQuality) {
                            // move this if above line 57 if 
                            if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
                                this.items[i].quality -= 1;
                            }
                        }                        
                    // set quality to 0 after sellIn date passes
                    } else {
                        this.items[i].quality = 0;
                    }
                    //  aged brie increases in quality here
                } else if (this.items[i].name == 'Aged Brie') {
                    if (this.items[i].quality < maxQuality) {
                        this.items[i].quality += 1;
                    }
                }
            }
        }
        
        return this.items;
    }
}
