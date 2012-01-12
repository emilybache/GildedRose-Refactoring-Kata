
class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
    	this.items = items;
    }

	public static void main(String[] args) {
        System.out.println("OMGHAI!");

        Item[] items = new Item[] {
              new Item("+5 Dexterity Vest", 10, 20),
              new Item("Aged Brie", 2, 0),
              new Item("Elixir of the Mongoose", 5, 7),
              new Item("Sulfuras, Hand of Ragnaros", 0, 80),
              new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
              new Item("Conjured Mana Cake", 3, 6)
          };

        GildedRose app = new GildedRose(items);

        app.updateQuality();

    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++)
        {
            if (items[i].name != "Aged Brie" && items[i].name != "Backstage passes to a TAFKAL80ETC concert")
            {
                if (items[i].quality > 0)
                {
                    if (items[i].name != "Sulfuras, Hand of Ragnaros")
                    {
                        items[i].quality = items[i].quality - 1;
                    }
                }
            }
            else
            {
                if (items[i].quality < 50)
                {
                    items[i].quality = items[i].quality + 1;

                    if (items[i].name == "Backstage passes to a TAFKAL80ETC concert")
                    {
                        if (items[i].sellIn < 11)
                        {
                            if (items[i].quality < 50)
                            {
                                items[i].quality = items[i].quality + 1;
                            }
                        }

                        if (items[i].sellIn < 6)
                        {
                            if (items[i].quality < 50)
                            {
                                items[i].quality = items[i].quality + 1;
                            }
                        }
                    }
                }
            }

            if (items[i].name != "Sulfuras, Hand of Ragnaros")
            {
                items[i].sellIn = items[i].sellIn - 1;
            }

            if (items[i].sellIn < 0)
            {
                if (items[i].name != "Aged Brie")
                {
                    if (items[i].name != "Backstage passes to a TAFKAL80ETC concert")
                    {
                        if (items[i].quality > 0)
                        {
                            if (items[i].name != "Sulfuras, Hand of Ragnaros")
                            {
                                items[i].quality = items[i].quality - 1;
                            }
                        }
                    }
                    else
                    {
                        items[i].quality = items[i].quality - items[i].quality;
                    }
                }
                else
                {
                    if (items[i].quality < 50)
                    {
                        items[i].quality = items[i].quality + 1;
                    }
                }
            }
        }
    }

}

