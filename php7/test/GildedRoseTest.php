<?php

namespace App;

class GildedRoseTest extends \PHPUnit\Framework\TestCase
{

    public function test_random_item_then_quality_maximum()
    {
        $items = [new Item("Random item", 1, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 49);
        $this->assertEquals($items[0]->sell_in, 0);
    }

    public function test_random_item_then_sell_in_is_0_or_lower()
    {
        $items = [new Item("Random item", 0, 3)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 1);
        $this->assertEquals($items[0]->sell_in, -1);
    }

    public function test_random_item_then_sell_in_greater_then_0()
    {
        $items = [new Item("Random item", 1, 3)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 2);
        $this->assertEquals($items[0]->sell_in, 0);
    }

    public function test_backstage_then_quality_maximum_sell_in_lower_than_zero()
    {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", -5, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 0);
        $this->assertEquals($items[0]->sell_in, -6);
    }

    public function test_backstage_then_quality_maximum_sell_in_beetween_one_and_five()
    {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 1, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 50);
        $this->assertEquals($items[0]->sell_in, 0);
    }

    public function test_backstage_then_quality_maximum_sell_in_beetween_five_and_ten()
    {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 7, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 50);
        $this->assertEquals($items[0]->sell_in, 6);
    }

    public function test_backstage_then_quality_maximum_sell_in_greater_than_ten()
    {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 11, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 50);
        $this->assertEquals($items[0]->sell_in, 10);
    }

    public function test_backstage_then_sell_in_greater_than_ten()
    {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 11, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 2);
        $this->assertEquals($items[0]->sell_in, 10);
    }

    public function test_backstage_then_sell_in_beetween_five_and_ten()
    {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 7, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 3);
        $this->assertEquals($items[0]->sell_in, 6);
    }

    public function test_backstage_then_beetween_one_and_five()
    {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 5, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 4);
        $this->assertEquals($items[0]->sell_in, 4);
    }

    public function test_backstage_then_sell_in_is_zero()
    {
        $items = [new Item("Backstage passes to a TAFKAL80ETC concert", 0, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 0);
        $this->assertEquals($items[0]->sell_in, -1);
    }

    public function test_sulfuras_then_sell_in_greater_then_zero()
    {
        $items = [new Item("Sulfuras, Hand of Ragnaros", 1, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 80);
        $this->assertEquals($items[0]->sell_in, 1);
    }

    public function test_sulfuras_then_sell_in_zero()
    {
        $items = [new Item("Sulfuras, Hand of Ragnaros", 0, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 80);
        $this->assertEquals($items[0]->sell_in, 0);
    }

    public function test_sulfuras_then_sell_in_lower_than_zero()
    {
        $items = [new Item("Sulfuras, Hand of Ragnaros", -1, 10)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 80);
        $this->assertEquals($items[0]->sell_in, -1);
    }

    public function test_aged_brie_then_sell_in_greater_than_zero()
    {
        $items = [new Item("Aged Brie", 1, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 2);
        $this->assertEquals($items[0]->sell_in, 0);
    }

    public function test_aged_brie_then_sell_in_lower_than_zero()
    {
        $items = [new Item("Aged Brie", -1, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 3);
        $this->assertEquals($items[0]->sell_in, -2);
    }

    public function test_aged_brie_then_sell_id_is_zero()
    {
        $items = [new Item("Aged Brie", 0, 1)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 3);
        $this->assertEquals($items[0]->sell_in, -1);
    }

    public function test_aged_brie_then_quality_is_maximum()
    {
        $items = [new Item("Aged Brie", 1, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 50);
        $this->assertEquals($items[0]->sell_in, 0);
    }

    public function test_aged_brie_then_quality_maximum_and_sell_in_lower_than_zero()
    {
        $items = [new Item("Aged Brie", -1, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 50);
        $this->assertEquals($items[0]->sell_in, -2);
    }

    public function test_aged_brie_then_quality_maximum_and_sell_in_greater_than_zero()
    {
        $items = [new Item("Aged Brie", 1, 50)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 50);
        $this->assertEquals($items[0]->sell_in, 0);
    }

    public function test_aged_brie_then_quality_one_of_maximum()
    {
        $items = [new Item("Aged Brie", 10, 49)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 50);
        $this->assertEquals($items[0]->sell_in, 9);
    }

    public function test_conjured_then_sell_in_greater_than_zero()
    {
        $items = [new Item("Conjured Mana Cake", 1, 5)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 3);
        $this->assertEquals($items[0]->sell_in, 0);
    }

    public function test_conjured_then_sell_in_lower_than_zero()
    {
        $items = [new Item("Conjured Mana Cake", -1, 5)];
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals($items[0]->quality, 1);
        $this->assertEquals($items[0]->sell_in, -2);
    }
}