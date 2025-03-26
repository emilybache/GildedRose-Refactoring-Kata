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

    pub fn update_quality(&mut self){
        self.quality = if self.name.contains("Sulfuras"){
            sulfuras_quality(self.quality, self.sell_in)
        }else if self.name.contains("Aged Brie"){
            brie_quality(self.quality, self.sell_in)
        }else if self.name.contains("Backstage passes"){
            backstage_quality(self.quality, self.sell_in)
        }else if self.name.contains("Conjured"){
            conjured_quality(self.quality, self.sell_in)
        }else{
            standard_quality(self.quality, self.sell_in)
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}, {}", self.name, self.sell_in, self.quality)
    }
}





pub fn brie_quality(quality: i32, sell_in: i32) -> i32 {
    if quality <= 0 || quality >= 50{
        return quality
    }
    if sell_in < 0 && quality <= 48{
        return quality + 2;
    }
    quality + 1
}

pub fn standard_quality(quality: i32, sell_in: i32) -> i32 {
    if quality <= 0 || quality >= 50{
        return quality;
    }
    if sell_in < 0 && quality >= 2{
        return quality - 2;
    }
    quality - 1
}

pub fn sulfuras_quality(quality: i32, sell_in: i32) -> i32 {
    return quality;
}

pub fn conjured_quality(quality: i32, sell_in: i32) -> i32 {
    if quality <= 0 || quality >= 50{
        return quality;
    }
    quality - 2
}

pub fn backstage_quality(quality: i32, sell_in: i32) -> i32 {
    if quality <= 0 || quality >= 50{
        return quality;
    }
    let updated_quality = match sell_in{
        10...6 => quality + 2,
        0...5 => quality + 3,
        _ => quality + 1,
    };
    if updated_quality >= 50{
        return 50;
    }
    return updated_quality;
}

pub struct GildedRose {
    pub items: Vec<Item>,
}

impl GildedRose {
    pub fn new(items: Vec<Item>) -> GildedRose {
        GildedRose { items }
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
    pub fn update_quality(&mut self) {
        for i in 0..self.items.len() {
            self.items[i].update_quality()
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
