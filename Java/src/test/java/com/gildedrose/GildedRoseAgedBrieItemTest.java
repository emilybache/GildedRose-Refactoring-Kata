package com.gildedrose;

import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.assertArrayEquals;

/**
 * Test the result of item.sellIn and item.quality after n days
 */
public class GildedRoseAgedBrieItemTest {
    private Item[] itemsSample = new Item[]{new Item("Aged Brie", 2, 0)};

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
                case "Aged Brie":
                    assertArrayEquals(message, map.get("Aged Brie"), new int[]{item.sellIn, item.quality});
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
    public void test_aged_brie_after_1_day() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Aged Brie", new int[]{1, 1});
            }
        };
        executeUpdateQuality(1);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_aged_brie_after_2_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Aged Brie", new int[]{0, 2});
            }
        };
        executeUpdateQuality(2);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_aged_brie_after_3_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Aged Brie", new int[]{-1, 4});
            }
        };
        executeUpdateQuality(3);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_aged_brie_after_4_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Aged Brie", new int[]{-2, 6});
            }
        };
        executeUpdateQuality(4);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_aged_brie_after_9_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Aged Brie", new int[]{-7, 16});
            }
        };
        executeUpdateQuality(9);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_aged_brie_after_10_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Aged Brie", new int[]{-8, 18});
            }
        };
        executeUpdateQuality(10);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_aged_brie_after_11_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Aged Brie", new int[]{-9, 20});
            }
        };
        executeUpdateQuality(11);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_aged_brie_after_25_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Aged Brie", new int[]{-23, 48});
            }
        };
        executeUpdateQuality(25);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }
}
