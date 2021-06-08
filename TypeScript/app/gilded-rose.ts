const maxQuality = 50;
const minQuality = 0;
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
        var conjured = 'Conjured';
        var ragnaros = 'Sulfuras, Hand of Ragnaros';
        var cheese = 'Aged Brie';
        var concertPass = 'Backstage passes to a TAFKAL80ETC concert';
        

        // for (let i = 0; i < this.items.length; i++) {
        this.items.forEach(item => {
            if(item.name == cheese){
                cheesyFunction(item);
            }

            else if(item.name == ragnaros){
                sulfurasHand(item);
            }

            else if(item.name == concertPass){
               backstagePass(item);
            }

            else{
              normalThing(item);
            }
            return item;
        })
        
        

    }

            // if (this.items[i].name != cheese && this.items[i].name != concertPass 
            // && this.items[i].name != ragnaros && this.items[i].quality > minQuality) {
            //         this.items[i].quality -=1;
            // } else {
            //     if (this.items[i].quality < maxQuality) {
            //         this.items[i].quality += 1;
            //         if (this.items[i].name == concertPass) {
            //             if (this.items[i].sellIn <= 10 && this.items[i].sellIn > 5) {
            //                 // quality goes up by 2 when days are 10 or less
            //                 if (this.items[i].quality < maxQuality) {
            //                     this.items[i].quality += 1;
            //                 }
            //             }
            //            // quality goes up by 3 when days are 5 of less
            //             else if (this.items[i].sellIn <= 5) {
            //                 if (this.items[i].quality < maxQuality) {
            //                     this.items[i].quality += 2;
            //                 }
            //             }
            //         }
            //     }
            // }
            // // checking for legendary to subtract sell by date 
            // if (this.items[i].name != ragnaros) {
            //     this.items[i].sellIn -= 1;
            // }
            
            // if (this.items[i].sellIn < 0) {
            //     if (this.items[i].name != cheese) {
            //         if (this.items[i].name != concertPass) {
            //             if (this.items[i].quality > minQuality) {
            //                 if (this.items[i].name != ragnaros) {
            //                     this.items[i].quality -= 1;
            //                 }
            //             }                        
            //         // set quality to 0 after sellIn date passes
            //         } else {
            //             this.items[i].quality = 0;
            //         }
            //         //  aged brie increases in quality here
            //     } else if (this.items[i].name == cheese) {
            //         if (this.items[i].quality < maxQuality) {
            //             this.items[i].quality += 1;
            //         }
            //     }
            //}
        

}// end class

function cheesyFunction(cheese){
    var cheesy = {
         name : cheese.name,
         quality : cheese.quality,
         sell : cheese.sellIn,
    }
    if(cheesy.quality < maxQuality){
        cheesy.quality += 1;
        cheesy.sell -= 1;
    }
    return cheesy; 
}

function sulfurasHand(hand){
    var sulfuras = {
        name : hand.name,
        quality : 80,
        sell : hand.sellIn,
    }
    return sulfuras;
}

function backstagePass(concertPass){
    const day1 = 10;
    const day2 = 5;
    var pass = {
        name : concertPass.name,
        quality : concertPass.quality,
        sell : concertPass.sellIn,
    }
    if (pass.quality < maxQuality){
        pass.quality += 1;
        if (pass.sell <= day1 && pass.sell > day2)
         pass.quality += 1;
         pass.sell - 1;
    }
    else if(pass.sell <= day2){
        pass.quality += 2;
        pass.sell - 1;
    }
    return pass;
}

function normalThing(thing){
    var item = {
        name : thing.name,
        quality : thing.quality,
        sell : thing.sellIn,
    }
    item.sell -= 1;
    item.quality -= 1;

    if(item.sell <= 0){
        item.sell -= 2;
    }
    if(item.quality <= 0){
        item.quality = 0;
    }
    return item;
}