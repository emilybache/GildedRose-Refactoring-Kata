package com.gildedrose;

public class Sulfuras extends Item {
// "Sulfuras", being a legendary item, never has to be sold or decreases in Quality	
	
	public Sulfuras(String name, int sellIn, int quality) {
		super(name, sellIn, quality);
	}

	public Item updateQuality(Item item) {
         return new Sulfuras(item.name, item.sellIn, item.quality);
    }
 }
