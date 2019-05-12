package com.gildedrose;

import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.assertArrayEquals;

/**
 * Test the result of item.sellIn and item.quality after n days
 */
public class GildedRoseBackStageItemTest {
    private Item[] itemsSample = new Item[]{new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20)};

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
                case "Backstage passes to a TAFKAL80ETC concert":
                    assertArrayEquals(message, map.get("Backstage passes to a TAFKAL80ETC concert"), new int[]{item.sellIn, item.quality});
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
    public void test_back_stage_after_1_day() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{14, 21});
            }
        };
        executeUpdateQuality(1);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_back_stage_after_2_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{13, 22});
            }
        };
        executeUpdateQuality(2);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_back_stage_after_3_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{12, 23});
            }
        };
        executeUpdateQuality(3);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_back_stage_after_4_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{11, 24});
            }
        };
        executeUpdateQuality(4);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_back_stage_after_9_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{6, 33});
            }
        };
        executeUpdateQuality(9);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_back_stage_after_10_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{5, 35});
            }
        };
        executeUpdateQuality(10);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_back_stage_after_11_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{4, 38});
            }
        };
        executeUpdateQuality(11);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }

    @Test
    public void test_back_stage_after_25_days() {
        HashMap<String,int[]> mapExpectedValued = new HashMap<String,int[]>(){
            {
                put("Backstage passes to a TAFKAL80ETC concert", new int[]{-10, 0});
            }
        };
        executeUpdateQuality(25);
        assertItems(itemsSample, mapExpectedValued, new Throwable().getStackTrace()[0].getMethodName());
    }
}
