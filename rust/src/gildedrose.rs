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
        for i in 0..self.items.len() {
            if self.items[i].name != "Aged Brie" && self.items[i].name != "Backstage passes to a TAFKAL80ETC concert"
            {
                if self.items[i].quality > 0 {
                    if self.items[i].name != "Sulfuras, Hand of Ragnaros" {
                        self.items[i].quality = self.items[i].quality - 1;
                    }
                }
            } else {
                if self.items[i].quality < 50 {
                    self.items[i].quality = self.items[i].quality + 1;

                    if self.items[i].name == "Backstage passes to a TAFKAL80ETC concert" {
                        if self.items[i].sell_in < 11 {
                            if self.items[i].quality < 50 {
                                self.items[i].quality = self.items[i].quality + 1;
                            }
                        }

                        if self.items[i].sell_in < 6 {
                            if self.items[i].quality < 50 {
                                self.items[i].quality = self.items[i].quality + 1;
                            }
                        }
                    }
                }
            }

            if self.items[i].name != "Sulfuras, Hand of Ragnaros" {
                self.items[i].sell_in = self.items[i].sell_in - 1;
            }

            if self.items[i].sell_in < 0 {
                if self.items[i].name != "Aged Brie" {
                    if self.items[i].name != "Backstage passes to a TAFKAL80ETC concert" {
                        if self.items[i].quality > 0 {
                            if self.items[i].name != "Sulfuras, Hand of Ragnaros" {
                                self.items[i].quality = self.items[i].quality - 1;
                            }
                        }
                    } else {
                        self.items[i].quality = self.items[i].quality - self.items[i].quality;
                    }
                } else {
                    if self.items[i].quality < 50 {
                        self.items[i].quality = self.items[i].quality + 1;
                    }
                }
            }
        }
    }
}
// Todos tienen SellIn y Quality
// En general, Quality esta entre 0 y 50
// Una vez que pasa SellIn, Quality se degrada al doble de velocidad
// Aged Brie aumenta su Quality con el tiempo
// Sulfuras Nunca tiene SellIn o Quality
// Backstage passes también aumenta Quality a medida que se acerca SellIn
// -Aumenta 2 cuando faltan 10 días o menos, 3 cuando faltan 5 o menos
// -Es 0 cuando sellin==0
// Conjured bajan su calidad el doble de rápido
// Sulfuras siempre tiene quality en 80
#[cfg(test)]
mod tests {
    use super::{GildedRose, Item};


    #[test]
    pub fn test_aged_brie_quality_limits(){
        let items = vec![Item::new("Aged Brie", 2, 50)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(50, rose.items[0].quality);
    }
    #[test]
    pub fn test_aged_brie_quality_increase_doble(){
        let items = vec![Item::new("Aged Brie", 0, 40)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(42, rose.items[0].quality);
    }
    #[test]
    pub fn test_aged_brie_quality_increase(){
        let items = vec![Item::new("Aged Brie", 2, 40)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(41, rose.items[0].quality);
    }
    #[test]
    pub fn test_sulfuras_quality(){
        let items = vec![Item::new("Sulfuras, Hand of Ragnaros", 2, 80)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(80, rose.items[0].quality);
    }
    #[test]
    pub fn test_sulfuras_sellin(){
        let items = vec![Item::new("Sulfuras, Hand of Ragnaros", 2, 80)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(2, rose.items[0].sell_in);
    }
    #[test]
    pub fn test_backstage_passes_quality(){
        let items = vec![Item::new("Backstage passes to a TAFKAL80ETC concert", 15, 20)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(21, rose.items[0].quality);
    }
    #[test]
    pub fn test_backstage_passes_quality_10(){
        let items = vec![Item::new("Backstage passes to a TAFKAL80ETC concert", 10, 20)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(22, rose.items[0].quality);
    }
    #[test]
    pub fn test_backstage_passes_quality_5(){
        let items = vec![Item::new("Backstage passes to a TAFKAL80ETC concert", 5, 20)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(23, rose.items[0].quality);
    }
    #[test]
    pub fn test_backstage_passes_quality_0(){
        let items = vec![Item::new("Backstage passes to a TAFKAL80ETC concert", 0, 20)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(0, rose.items[0].quality);
    }
    #[test]
    pub fn test_normal_item_quality(){
        let items = vec![Item::new("foo", 2, 20)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(19, rose.items[0].quality);
    }
    #[test]
    pub fn test_normal_after_sellin(){
        let items = vec![Item::new("foo", 0, 20)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();
        assert_eq!(18, rose.items[0].quality);
    }
}
