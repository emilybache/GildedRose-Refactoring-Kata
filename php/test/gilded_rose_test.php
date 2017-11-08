<?php

require_once 'gilded_rose.php';

class GildedRoseTest extends PHPUnit\Framework\TestCase {

    function testFoo() {
        $items = array(new Item("foo", 0, 0));
        $gildedRose = new GildedRose($items);
        $gildedRose->update_quality();
        $this->assertEquals("foo", $items[0]->name);
    }

    function testItemToString() {
        $items = array(new Item("foo", 0, 0));
        $gildedRose = new GildedRose($items);
        $this->assertEquals("foo, 0, 0", (string)$items[0]);
    }

    function testSulfuras() {
    	$baseQuality = 80;
        $items = array(
        	new Item("Sulfuras, Hand of Ragnaros", 10, $baseQuality),
        	new Item("Sulfuras, Hand of Ragnaros", 0, $baseQuality),
        	new Item("Sulfuras, Hand of Ragnaros", -10, $baseQuality),
        );
        $gildedRose = new GildedRose($items);
        $gildedRose->update_quality();
        $this->assertEquals($baseQuality, $items[0]->quality);
        $this->assertEquals($baseQuality, $items[1]->quality);
        $this->assertEquals($baseQuality, $items[2]->quality);
    }

	function testSulfurasFewDays() {
    	$baseQuality = 80;

        $items = array(
        	new Item("Sulfuras, Hand of Ragnaros", 10, $baseQuality),
        	new Item("Sulfuras, Hand of Ragnaros", 0, $baseQuality),
        	new Item("Sulfuras, Hand of Ragnaros", -10, $baseQuality),
        );

        $gildedRose = new GildedRose($items);
        $gildedRose->update_quality();
        $gildedRose->update_quality();
        $gildedRose->update_quality();

        $this->assertEquals($baseQuality, $items[0]->quality);
        $this->assertEquals($baseQuality, $items[1]->quality);
        $this->assertEquals($baseQuality, $items[2]->quality);
    }

    function testAged() {
    	$baseQuality = 10;

        $items = array(
        	new Item("Aged Brie", 10, $baseQuality),
        	new Item("Aged Brie", 0, $baseQuality),
        	new Item("Aged Brie", -10, $baseQuality),
        );

        $gildedRose = new GildedRose($items);
        $gildedRose->update_quality();

        $this->assertEquals($baseQuality + 1, $items[0]->quality);
        $this->assertEquals($baseQuality + 2, $items[1]->quality);
        $this->assertEquals($baseQuality + 2, $items[2]->quality);
    }

    function testAgedFewDays() {
    	$baseQuality = 10;
		$days = 3;

        $items = array(
        	new Item("Aged Brie", 10, $baseQuality),
        	new Item("Aged Brie", 0, $baseQuality),
        	new Item("Aged Brie", -10, $baseQuality),
        );

        $gildedRose = new GildedRose($items);
        foreach (range(1, $days) as $value) {
        	$gildedRose->update_quality();
        }

        $this->assertEquals($baseQuality + 1 * $days, $items[0]->quality);
        $this->assertEquals($baseQuality + 2 * $days, $items[1]->quality);
        $this->assertEquals($baseQuality + 2 * $days, $items[2]->quality);
    }

    function testBackStage() {
    	$baseQuality = 10;

        $items = array(
			new Item("Backstage passes to a TAFKAL80ETC concert", 30, $baseQuality),
        	new Item("Backstage passes to a TAFKAL80ETC concert", 10, $baseQuality),
        	new Item("Backstage passes to a TAFKAL80ETC concert", 5, $baseQuality),
        	new Item("Backstage passes to a TAFKAL80ETC concert", 0, $baseQuality),
        	new Item("Backstage passes to a TAFKAL80ETC concert", -10, $baseQuality),
        );

        $gildedRose = new GildedRose($items);
        $gildedRose->update_quality();

        $this->assertEquals($baseQuality + 1, $items[0]->quality);
        $this->assertEquals($baseQuality + 2, $items[1]->quality);
        $this->assertEquals($baseQuality + 3, $items[2]->quality);

        $this->assertEquals(0, $items[3]->quality);
        $this->assertEquals(0, $items[4]->quality);
    }

	function testBackStageFewDays() {
    	$baseQuality = 10;
    	$days = 3;

        $items = array(
			new Item("Backstage passes to a TAFKAL80ETC concert", 30, $baseQuality),
        	new Item("Backstage passes to a TAFKAL80ETC concert", 10, $baseQuality),
        	new Item("Backstage passes to a TAFKAL80ETC concert", 5, $baseQuality),
        	new Item("Backstage passes to a TAFKAL80ETC concert", 0, $baseQuality),
        	new Item("Backstage passes to a TAFKAL80ETC concert", -10, $baseQuality),
        );

        $gildedRose = new GildedRose($items);
        foreach (range(1, $days) as $value) {
        	$gildedRose->update_quality();
        }

        $this->assertEquals($baseQuality + 1 * $days, $items[0]->quality);
        $this->assertEquals($baseQuality + 2 * $days, $items[1]->quality);
        $this->assertEquals($baseQuality + 3 * $days, $items[2]->quality);

        $this->assertEquals(0, $items[3]->quality);
        $this->assertEquals(0, $items[4]->quality);
    }

    function testCommonProduct() {
    	$baseQuality = 10;

        $items = array(
			new Item("Some non interested product", 30, $baseQuality),
        	new Item("Some non interested product", 0, $baseQuality),
        	new Item("Some non interested product", -10, $baseQuality),
        );

        $gildedRose = new GildedRose($items);
        $gildedRose->update_quality();

        $this->assertEquals($baseQuality - 1, $items[0]->quality);
        $this->assertEquals($baseQuality - 2, $items[1]->quality);
        $this->assertEquals($baseQuality - 2, $items[2]->quality);
    }

	function testCommonProductFewDays() {
    	$baseQuality = 10;
    	$days = 3;

        $items = array(
			new Item("Some non interested product", 30, $baseQuality),
        	new Item("Some non interested product", 0, $baseQuality),
        	new Item("Some non interested product", -10, $baseQuality),
        );

        $gildedRose = new GildedRose($items);
        foreach (range(1, $days) as $value) {
        	$gildedRose->update_quality();
        }

        $this->assertEquals($baseQuality - 1 * $days, $items[0]->quality);
        $this->assertEquals($baseQuality - 2 * $days, $items[1]->quality);
        $this->assertEquals($baseQuality - 2 * $days, $items[2]->quality);
    }
}
