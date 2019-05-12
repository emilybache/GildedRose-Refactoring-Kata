package com.gildedrose;

import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.assertArrayEquals;

/**
 * Test the result of item.sellIn and item.quality after n days
 */
public class GildedRoseConjuredItemTest {
    private Item[] itemsSample = new Item[]{new Item("Conjured Mana Cake", 3, 6) };

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
    public void test_conjured_after_1_day() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Conjured Mana Cake", new int[]{2, 5});
            }
        };
        executeUpdateQuality(1);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_conjured_after_2_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Conjured Mana Cake", new int[]{1, 4});
            }
        };
        executeUpdateQuality(2);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_conjured_after_3_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
               put("Conjured Mana Cake", new int[]{0, 3});
            }
        };
        executeUpdateQuality(3);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_conjured_after_4_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Conjured Mana Cake", new int[]{-1, 1});
            }
        };
        executeUpdateQuality(4);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_conjured_after_9_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Conjured Mana Cake", new int[]{-6, 0});
            }
        };
        executeUpdateQuality(9);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_conjured_after_10_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Conjured Mana Cake", new int[]{-7, 0});
            }
        };
        executeUpdateQuality(10);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_conjured_after_11_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Conjured Mana Cake", new int[]{-8, 0});
            }
        };
        executeUpdateQuality(11);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_conjured_after_25_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Conjured Mana Cake", new int[]{-22, 0});
            }
        };
        executeUpdateQuality(25);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }
}
