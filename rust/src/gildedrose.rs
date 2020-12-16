use std::fmt::{self, Display};
pub struct Item {
    pub name: String,
    pub sell_in: i32,
    pub quality: i32,
}

impl Item {
    pub fn new(name: impl Into<String>, sell_in: i32, quality: i32) -> Item {
        Item {
            name: name.into(),
            sell_in,
            quality,
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}, {}", self.name, self.sell_in, self.quality)
    }
}

pub struct GildedRose {
    pub items: Vec<Item>,
}

impl GildedRose {
    pub fn new(items: Vec<Item>) -> GildedRose {
        GildedRose { items }
    }

    pub fn update_quality(&mut self) {
        for item in &mut self.items {
            if item.name != "Aged Brie" && item.name != "Backstage passes to a TAFKAL80ETC concert"
            {
                if item.quality > 0 {
                    if item.name != "Sulfuras, Hand of Ragnaros" {
                        if item.name == "Conjured Mana Cake" {
                            item.quality = item.quality - 2;
                        } else {
                            item.quality = item.quality - 1;
                        }
                    }
                }
            } else {
                if item.quality < 50 {
                    item.quality = item.quality + 1;

                    if item.name == "Backstage passes to a TAFKAL80ETC concert" {
                        if item.sell_in < 11 {
                            if item.quality < 50 {
                                item.quality = item.quality + 1;
                            }
                        }

                        if item.sell_in < 6 {
                            if item.quality < 50 {
                                item.quality = item.quality + 1;
                            }
                        }
                    }
                }
            }

            if item.name != "Sulfuras, Hand of Ragnaros" {
                item.sell_in = item.sell_in - 1;
            }

            if item.sell_in < 0 {
                if item.name != "Aged Brie" {
                    if item.name != "Backstage passes to a TAFKAL80ETC concert" {
                        if item.quality > 0 {
                            if item.name != "Sulfuras, Hand of Ragnaros" {
                                if item.name == "Conjured Mana Cake" {
                                    item.quality = item.quality - 2;
                                } else {
                                    item.quality = item.quality - 1;
                                }
                            }
                        }
                    } else {
                        item.quality = item.quality - item.quality;
                    }
                } else {
                    if item.quality < 50 {
                        item.quality = item.quality + 1;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    pub struct TestCase {
        pub iterations: i32,
        pub expected_result: Vec<Item>,
    }

    use super::{GildedRose, Item};

    #[test]
    pub fn test1() {
        let items = vec![
            Item::new("+5 Dexterity Vest", 10, 20),
            Item::new("Aged Brie", 2, 0),
            Item::new("Elixir of the Mongoose", 5, 7),
            Item::new("Sulfuras, Hand of Ragnaros", 0, 80),
            Item::new("Sulfuras, Hand of Ragnaros", -1, 80),
            Item::new("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            Item::new("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            Item::new("Backstage passes to a TAFKAL80ETC concert", 5, 49),
            Item::new("Conjured Mana Cake", 15, 40),
            Item::new("Conjured Mana Cake", 3, 46),
        ];
        let result_items_0 = vec![
            Item::new("+5 Dexterity Vest", -2, 6),
            Item::new("Aged Brie", -10, 22),
            Item::new("Elixir of the Mongoose", -7, 0),
            Item::new("Sulfuras, Hand of Ragnaros", 0, 80),
            Item::new("Sulfuras, Hand of Ragnaros", -1, 80),
            Item::new("Backstage passes to a TAFKAL80ETC concert", 3, 41),
            Item::new("Backstage passes to a TAFKAL80ETC concert", -2, 0),
            Item::new("Backstage passes to a TAFKAL80ETC concert", -7, 0),
            Item::new("Conjured Mana Cake", 3, 16),//expected behaviour sellin=15-12, quality=40-2*12=16
            Item::new("Conjured Mana Cake", -9, 4),//expected behaviour sellin=3-12=-9, quality=46-(3*2 + 9*4)=4
        ];
        let test_case_0 = TestCase {
            iterations: 12,
            expected_result: result_items_0
        };
        let mut rose = GildedRose::new(items);
        for i in 0..test_case_0.iterations {
            println!("-------- day {} --------", i);
            println!("name, sellIn, quality");
            for item in &rose.items {
                println!("{}", item);
            }
            println!();
            rose.update_quality();//not shown in stdout
        }

        for i in 0..rose.items.len() {
            
            println!("{}", rose.items[i].name);
            assert_eq!((rose.items[i].sell_in, rose.items[i].quality), (test_case_0.expected_result[i].sell_in, test_case_0.expected_result[i].quality));
        }
    }

    #[test]
    pub fn test2() {
        let items1 = vec![
            Item::new("+5 Dexterity Vest", 10, 20),
            Item::new("Aged Brie", 2, 0),
            Item::new("Elixir of the Mongoose", 5, 7),
            Item::new("Sulfuras, Hand of Ragnaros", 0, 80),
            Item::new("Sulfuras, Hand of Ragnaros", -1, 80),
            Item::new("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            Item::new("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            Item::new("Backstage passes to a TAFKAL80ETC concert", 5, 49),
            Item::new("Conjured Mana Cake", 15, 40),
            Item::new("Conjured Mana Cake", 3, 46),
        ];
        let result_items_1 = vec![
            Item::new("+5 Dexterity Vest", -19, 0),
            Item::new("Aged Brie", -27, 50),
            Item::new("Elixir of the Mongoose", -24, 0),
            Item::new("Sulfuras, Hand of Ragnaros", 0, 80),
            Item::new("Sulfuras, Hand of Ragnaros", -1, 80),
            Item::new("Backstage passes to a TAFKAL80ETC concert", -14, 0),
            Item::new("Backstage passes to a TAFKAL80ETC concert", -19, 0),
            Item::new("Backstage passes to a TAFKAL80ETC concert", -24, 0),
            Item::new("Conjured Mana Cake", -14, 0),//expected behaviour sellin=15-29=-14, quality=40-(15days*2 + 15days*4)=-50->0(should be zero)
            Item::new("Conjured Mana Cake", -26, 0),//expected behaviour sellin=3-29=-26, quality=46-(3days*2 + 26days*4)=46-110=-64->0
        ];
        let test_case_1 = TestCase {
            iterations: 29,
            expected_result: result_items_1
        };
        let mut rose1 = GildedRose::new(items1);
        for i in 0..test_case_1.iterations {
            println!("-------- day {} --------", i);
            println!("name, sellIn, quality");
            for item in &rose1.items {
                println!("{}", item);
            }
            println!();
            rose1.update_quality();//not shown in stdout
        }
        for i in 0..rose1.items.len() {
            
            println!("{}", rose1.items[i].name);
            assert_eq!((rose1.items[i].sell_in, rose1.items[i].quality), (test_case_1.expected_result[i].sell_in, test_case_1.expected_result[i].quality));
        }

        //assert_eq!("fixme", rose.items[0].name);
    }
}
