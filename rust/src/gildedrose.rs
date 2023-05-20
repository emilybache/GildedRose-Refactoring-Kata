//Тепер структура Item тепер має приватні поля, оскільки вони вже не потребують зовнішнього доступу
//У методі GildedRose::update_quality() замість вкладених умов if-else застосовано збіги за допомогою match. Це полегшує розуміння логіки для кожного типу товару
//Я створила окремі методи update_aged_brie(), update_backstage_passes() та update_normal_item()
// для оновлення якості товару відповідно до його типу. Це зробило код більш читабельним та зменшило повторення 
//Також я додала метод update_sell_in(), який відповідає за оновлення поля sell_in для всіх типів товарів, крім "Sulfuras, Hand of Ragnaros"
use std::fmt::{self, Display};

pub struct Item {
    name: String,
    sell_in: i32,
    quality: i32,
}

impl Item {
    pub fn new(name: impl Into<String>, sell_in: i32, quality: i32) -> Self {
        Self {
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
    items: Vec<Item>,
}

impl GildedRose {
    pub fn new(items: Vec<Item>) -> Self {
        Self { items }
    }

    pub fn update_quality(&mut self) {
        for item in &mut self.items {
            match item.name.as_str() {
                "Aged Brie" => self.update_aged_brie(item),
                "Backstage passes to a TAFKAL80ETC concert" => {
                    self.update_backstage_passes(item)
                }
                "Sulfuras, Hand of Ragnaros" => {}
                _ => self.update_normal_item(item),
            }
            self.update_sell_in(item);
        }
    }

    fn update_aged_brie(&mut self, item: &mut Item) {
        if item.quality < 50 {
            item.quality += 1;
        }
        item.sell_in -= 1;
        if item.sell_in < 0 && item.quality < 50 {
            item.quality += 1;
        }
    }

    fn update_backstage_passes(&mut self, item: &mut Item) {
        if item.quality < 50 {
            item.quality += 1;
            if item.sell_in < 11 && item.quality < 50 {
                item.quality += 1;
            }
            if item.sell_in < 6 && item.quality < 50 {
                item.quality += 1;
            }
        }
        item.sell_in -= 1;
        if item.sell_in < 0 {
            item.quality = 0;
        }
    }

    fn update_normal_item(&mut self, item: &mut Item) {
        if item.quality > 0 {
            item.quality -= 1;
        }
        item.sell_in -= 1;
        if item.sell_in < 0 && item.quality > 0 {
            item.quality -= 1;
        }
    }

    fn update_sell_in(&mut self, item: &mut Item) {
        if item.name != "Sulfuras, Hand of Ragnaros" {
            item.sell_in -= 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{GildedRose, Item};

    #[test]
    pub fn foo() {
        let items = vec![Item::new("foo", 0, 0)];
        let mut rose = GildedRose::new(items);
        rose.update_quality();

        assert_eq!("fixme", rose.items[0].name);
    }
}
