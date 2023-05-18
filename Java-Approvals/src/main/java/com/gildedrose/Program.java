package com.gildedrose;

public class Program {

	public static void main(String... args) {
		System.out.println("OMGHAI!");

		Item[] items = new Item[] {
				new Item("Sports Memorabilia", 10, 20),
				new Item("Aged Cheese", 2, 0),
				new Item("Coffee Table Book", 5, 7),
				new Item("Fine Italian Silk", 0, 80),
				new Item("Fine Italian Silk", -1, 80),
				new Item("Backstage passes to a concert", 15, 20),
				new Item("Backstage passes to a concert", 10, 49),
				new Item("Backstage passes to a concert", 5, 49),
				// this Baked item does not work properly yet
				new Item("Baked Chocolate Cake", 3, 6) };

		GildedRose app = new GildedRose(items);

		for (int i = 0; i < 31; i++) {
			System.out.println("-------- day " + i + " --------");
			System.out.println("name, sellIn, quality");
			for (int j = 0; j < items.length; j++) {
				System.out.println(items[j]);
			}
			System.out.println("");
			app.updateQuality();
		}
	}
}
