package com.gildedrose;

import static org.junit.Assert.*;
import java.util.HashMap;

import org.junit.Test;

/**
 * Test the result of item.sellIn and item.quality after n days
 */
public class GildedRoseAllItemsTest {
    private Item[] itemsSample = new Item[]{new Item("+5 Dexterity Vest", 10, 20), //
            new Item("Aged Brie", 2, 0), //
            new Item("Elixir of the Mongoose", 5, 7), //
            new Item("Sulfuras, Hand of Ragnaros", 0, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            // this conjured item does not work properly yet
            new Item("Conjured Mana Cake", 3, 6) };

    private GildedRoseItem app = new GildedRoseItem(itemsSample);

    /**
     * Test if item.sellIn and item.quality is the same that expected after a number of day.
     *
     * @param items, contains values calculated by executeUpdateQuality function
     * @param map, contains values that we expect
     * @param message, to make difference between tests
     */
    public void assertItems(Item[] items, HashMap<String, int[]> map, String message) {
        for (Item item : items) {
            switch (item.name) {
                case "+5 Dexterity Vest":
                    assertArrayEquals(message, map.get("+5 Dexterity Vest"), new int[]{item.sellIn, item.quality});
                    break;
                case "Aged Brie":
                    assertArrayEquals(message, map.get("Aged Brie"), new int[]{item.sellIn, item.quality});
                    break;
                case "Elixir of the Mongoose":
                    assertArrayEquals(message, map.get("Elixir of the Mongoose"), new int[]{item.sellIn, item.quality});
                    break;
                case "Sulfuras, Hand of Ragnaros":
                    assertArrayEquals(message, map.get("Sulfuras, Hand of Ragnaros"), new int[]{item.sellIn, item.quality});
                    break;
                case "Backstage passes to a TAFKAL80ETC concert":
                    assertArrayEquals(message, map.get("Backstage passes to a TAFKAL80ETC concert"), new int[]{item.sellIn, item.quality});
                    break;
                case "Conjured Mana Cake":
                    assertArrayEquals(message, map.get("Conjured Mana Cake"), new int[]{item.sellIn, item.quality});
                    break;
                default:
                    System.out.println("oops");
            }
        }
    }

    /**
     * Execute n times the function update quality
     *
     * @param numberOfTimes, number of times that you want to execute that function
     */
    private void executeUpdateQuality(int numberOfTimes) {
        for(int i=0;i<numberOfTimes;i++){
            app.updateQuality();
        }
    }

    @Test
    public void test_after_1_day() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{9, 19});
                put("Aged Brie", new int[]{1, 1});
                put("Elixir of the Mongoose", new int[]{4, 6});
                put("Sulfuras, Hand of Ragnaros", new int[]{0, 80});
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{14, 21});
                put("Conjured Mana Cake", new int[]{2, 5});
            }
        };
        executeUpdateQuality(1);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_after_2_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{8, 18});
                put("Aged Brie", new int[]{0, 2});
                put("Elixir of the Mongoose", new int[]{3, 5});
                put("Sulfuras, Hand of Ragnaros", new int[]{0, 80});
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{13, 22});
                put("Conjured Mana Cake", new int[]{1, 4});
            }
        };
        executeUpdateQuality(2);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_after_3_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{7, 17});
                put("Aged Brie", new int[]{-1, 4});
                put("Elixir of the Mongoose", new int[]{2, 4});
                put("Sulfuras, Hand of Ragnaros", new int[]{0, 80});
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{12, 23});
                put("Conjured Mana Cake", new int[]{0, 3});
            }
        };
        executeUpdateQuality(3);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_after_4_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{6, 16});
                put("Aged Brie", new int[]{-2, 6});
                put("Elixir of the Mongoose", new int[]{1, 3});
                put("Sulfuras, Hand of Ragnaros", new int[]{0, 80});
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{11, 24});
                put("Conjured Mana Cake", new int[]{-1, 1});
            }
        };
        executeUpdateQuality(4);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_after_9_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{1, 11});
                put("Aged Brie", new int[]{-7, 16});
                put("Elixir of the Mongoose", new int[]{-4, 0});
                put("Sulfuras, Hand of Ragnaros", new int[]{0, 80});
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{6, 33});
                put("Conjured Mana Cake", new int[]{-6, 0});
            }
        };
        executeUpdateQuality(9);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_after_10_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{0, 10});
                put("Aged Brie", new int[]{-8, 18});
                put("Elixir of the Mongoose", new int[]{-5, 0});
                put("Sulfuras, Hand of Ragnaros", new int[]{0, 80});
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{5, 35});
                put("Conjured Mana Cake", new int[]{-7, 0});
            }
        };
        executeUpdateQuality(10);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_after_11_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{-1, 8});
                put("Aged Brie", new int[]{-9, 20});
                put("Elixir of the Mongoose", new int[]{-6, 0});
                put("Sulfuras, Hand of Ragnaros", new int[]{0, 80});
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{4, 38});
                put("Conjured Mana Cake", new int[]{-8, 0});
            }
        };
        executeUpdateQuality(11);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_after_25_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{-15, 0});
                put("Aged Brie", new int[]{-23, 48});
                put("Elixir of the Mongoose", new int[]{-20, 0});
                put("Sulfuras, Hand of Ragnaros", new int[]{0, 80});
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{-10, 0});
                put("Conjured Mana Cake", new int[]{-22, 0});
            }
        };
        executeUpdateQuality(25);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }
}
