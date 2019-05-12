package com.gildedrose;

import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.assertArrayEquals;

/**
 * Test the result of item.sellIn and item.quality after n days
 */
public class GildedRoseNormalItemTest {
    private Item[] itemsSample = new Item[]{new Item("+5 Dexterity Vest", 10, 20), //
            new Item("Elixir of the Mongoose", 5, 7)};

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
                case "Elixir of the Mongoose":
                    assertArrayEquals(message, map.get("Elixir of the Mongoose"), new int[]{item.sellIn, item.quality});
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
    public void test_normal_after_1_day() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{9, 19});
                put("Elixir of the Mongoose", new int[]{4, 6});
            }
        };
        executeUpdateQuality(1);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_normal_after_2_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{8, 18});
                put("Elixir of the Mongoose", new int[]{3, 5});
            }
        };
        executeUpdateQuality(2);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_normal_after_3_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{7, 17});
                put("Elixir of the Mongoose", new int[]{2, 4});
            }
        };
        executeUpdateQuality(3);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_normal_after_4_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{6, 16});
                put("Elixir of the Mongoose", new int[]{1, 3});
            }
        };
        executeUpdateQuality(4);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_normal_after_9_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{1, 11});
                put("Elixir of the Mongoose", new int[]{-4, 0});
            }
        };
        executeUpdateQuality(9);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_normal_after_10_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{0, 10});
                put("Elixir of the Mongoose", new int[]{-5, 0});
            }
        };
        executeUpdateQuality(10);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_normal_after_11_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{-1, 8});
                put("Elixir of the Mongoose", new int[]{-6, 0});
            }
        };
        executeUpdateQuality(11);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_normal_after_25_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("+5 Dexterity Vest", new int[]{-15, 0});
                put("Elixir of the Mongoose", new int[]{-20, 0});
            }
        };
        executeUpdateQuality(25);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }
}
