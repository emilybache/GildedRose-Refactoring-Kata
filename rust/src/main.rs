mod gildedrose;

use gildedrose::{GildedRose, Item};

fn main() {
    let items = vec![
        Item::new("Sports Memorabilia", 10, 20),
        Item::new("Aged Cheese", 2, 0),
        Item::new("Coffee Table Book", 5, 7),
        Item::new("Fine Italian Silk", 0, 80),
        Item::new("Fine Italian Silk", -1, 80),
        Item::new("Backstage passes to a concert", 15, 20),
        Item::new("Backstage passes to a concert", 10, 49),
        Item::new("Backstage passes to a concert", 5, 49),
        // this Baked item does not work properly yet
        Item::new("Baked Chocolate Cake", 3, 6),
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
