<?php

require_once 'gilded_rose.php';

/**
 * A PHP port from https://gist.github.com/trikitrok/4627e7586180e48fe24d
 */
class GildedRoseTest extends PHPUnit_Framework_TestCase
{
    /**
     * @var GildedRose
     */
    private $gildedRose;

    public function test_sulfurasIsInmutable()
    {
        $sulfuras = new Item("Sulfuras, Hand of Ragnaros", 0, 80);
        $this->gildedRose = $this->aGildedRoseWithItems($sulfuras);

        $this->afterDays(10);

        $this->assertItemsQuality(80, $sulfuras);
        $this->assertEquals(0, $sulfuras->sell_in);
    }


    public function test_sellInDecreasesByOneEachDay()
    {
        $notSulfuras = new Item("notSulfuras", 2, 0);
        $this->gildedRose = $this->aGildedRoseWithItems($notSulfuras);

        $this->afterDays(10);

        $this->assertEquals(-8, $notSulfuras->sell_in);
    }


    public function test_agedBrieQualityIncreasesByOneEachDayBeforeSellDate()
    {
        $agedBrie = new Item("Aged Brie", 2, 0);
        $this->gildedRose = $this->aGildedRoseWithItems($agedBrie);

        $this->afterDays(2);

        $this->assertItemsQuality(2, $agedBrie);
    }


    public function test_agedBrieQualityIncreasesByTwoEachDayAfterSellDate()
    {
        $agedBrie = new Item("Aged Brie", 0, 0);
        $this->gildedRose = $this->aGildedRoseWithItems($agedBrie);

        $this->afterDays(2);

        $this->assertItemsQuality(4, $agedBrie);
    }


    public function test_qualityCannotBeMoreThanFifty()
    {
        $agedBrie = new Item("Aged Brie", 2, 0);
        $this->gildedRose = $this->aGildedRoseWithItems($agedBrie);

        $this->afterDays(300);

        $this->assertItemsQuality(50, $agedBrie);
    }


    public function test_backstagePassesQualityIncreasesByOneEachDayBeforeTenDaysFromSellDate()
    {
        $backstagePasses = new Item("Backstage passes to a TAFKAL80ETC concert", 15, 0);
        $this->gildedRose = $this->aGildedRoseWithItems($backstagePasses);

        $this->afterDays(5);

        $this->assertItemsQuality(5, $backstagePasses);
    }


    public function test_backstagePassesQualityIncreasesByTwoEachDayBetweenTenAndFiveDaysBeforeSellDate()
    {
        $backstagePasses = new Item("Backstage passes to a TAFKAL80ETC concert", 15, 0);
        $this->gildedRose = $this->aGildedRoseWithItems($backstagePasses);

        $this->afterDays(7);

        $this->assertItemsQuality(9, $backstagePasses);
    }


    public function test_backstagePassesQualityIncreasesByThreeEachDayBetweenFiveDaysFromSellDateAndSellDate()
    {
        $backstagePasses = new Item("Backstage passes to a TAFKAL80ETC concert", 15, 0);
        $this->gildedRose = $this->aGildedRoseWithItems($backstagePasses);

        $this->afterDays(15);

        $this->assertItemsQuality(30, $backstagePasses);
    }


    public function test_backstagePassesQualityIsZeroAfterTheSellDate()
    {
        $backstagePasses = new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20);
        $this->gildedRose = $this->aGildedRoseWithItems($backstagePasses);

        $this->afterDays(16);

        $this->assertItemsQuality(0, $backstagePasses);
    }


    public function test_perishableItemsQualityDecreasesByOneEachDayBeforeSellDate()
    {
        $regularItem = new Item("+5 Dexterity Vest", 10, 20);
        $this->gildedRose = $this->aGildedRoseWithItems($regularItem);

        $this->afterDays(10);

        $this->assertItemsQuality(10, $regularItem);
    }


    public function test_perishableItemsQualityDecreasesByTwoEachDayAfterSellDate()
    {
        $perishableItem = new Item("+5 Dexterity Vest", 10, 20);
        $this->gildedRose = $this->aGildedRoseWithItems($perishableItem);

        $this->afterDays(15);

        $this->assertItemsQuality(0, $perishableItem);
    }


    public function test_perishableItemsQualityCannotBeLessThanZero()
    {
        $perishableItem = new Item("+5 Dexterity Vest", 10, 20);
        $this->gildedRose = $this->aGildedRoseWithItems($perishableItem);

        $this->afterDays(200);

        $this->assertItemsQuality(0, $perishableItem);
    }


    private function afterDays($numberOfDays)
    {
        for ($i = 0; $i < $numberOfDays; ++$i) {
            $this->gildedRose->update_quality();
        }
    }

    private function assertItemsQuality($quality, $item)
    {
        $this->assertEquals($quality, $item->quality);
    }

    private function aGildedRoseWithItems($items)
    {
        return new GildedRose(array($items));
    }
}
