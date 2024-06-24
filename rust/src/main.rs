mod gildedrose;

use gildedrose::{GildedRose, Item};

fn main() {
    let items = vec![
        Item::new("+5 Dexterity Vest", 10, 20),
        Item::new("Aged Brie", 2, 0),
        Item::new("Elixir of the Mongoose", 5, 7),
        Item::new("Sulfuras, Hand of Ragnaros", 0, 80),
        Item::new("Sulfuras, Hand of Ragnaros", -1, 80),
        Item::new("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        Item::new("Backstage passes to a TAFKAL80ETC concert", 10, 49),
        Item::new("Backstage passes to a TAFKAL80ETC concert", 5, 49),
        // this conjured item does not work properly yet
        Item::new("Conjured Mana Cake", 3, 6),
    ];
    let mut rose = GildedRose::new(items);

    println!("OMGHAI!");
    for i in 0..=30 {
        println!("-------- day {} --------", i);
        println!("name, sellIn, quality");
        for item in &rose.items {
            println!("{}", item);
        }
        println!();
        rose.update_quality();
    }
}
