<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{

    /* -- FUNCTION TEMPLATE COPY ME --
     public function testFoo(): void
     {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);
        $items = [];

        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();

     } // END TestFoo()
     */

    public function testSellInValueDegradesOverTime(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'Foo';
        $quality = 10;
        $sellIn = 10;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);
        $setItems = $gildedRose->getItems();

        // Make sure the data is the same as we insert
        $this->assertSame($name, $setItems[0]->name);
        $this->assertSame($sellIn, $setItems[0]->sellIn);
        $this->assertSame($quality, $setItems[0]->quality);

        // Update the items
        $gildedRose->updateQuality();
        $updatedItems = $gildedRose->getItems();

        $this->assertSame(($sellIn - 1), $updatedItems[0]->sellIn);


    } // END testSellInValueDegradesOverTime()

    public function testStdQualityValueDegradesOverTime(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'Foo';
        $quality = 10;
        $sellIn = 10;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);
        $setItems = $gildedRose->getItems();

        // Make sure the data is the same as we insert
        $this->assertSame($name, $setItems[0]->name);
        $this->assertSame($sellIn, $setItems[0]->sellIn);
        $this->assertSame($quality, $setItems[0]->quality);

        // Update the items
        $gildedRose->updateQuality();
        $updatedItems = $gildedRose->getItems();

        $this->assertSame(($quality - 1), $updatedItems[0]->quality);
    } // END testStdQualityValueDegradesOverTime()

    public function testAgedBrieQualityIncreaseOverTime(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = "Aged Brie";
        $sellIn = 10;
        $quality = 10;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);
        $setItems = $gildedRose->getItems();
        $this->assertSame($name, $setItems[0]->name);
        $this->assertSame($sellIn, $setItems[0]->sellIn);
        $this->assertSame($quality, $setItems[0]->quality);

        $gildedRose->updateQuality();
        $updatedItems = $gildedRose->getItems();

        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame(($sellIn - 1), $updatedItems[0]->sellIn);
        $this->assertSame(($quality + 1), $updatedItems[0]->quality);
    } // END testAgedBrieQualityIncreaseOverTime()

    public function testAgedBrieQualityIncreasePastSellInButUnderMax(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = "Aged Brie";
        $sellIn = 10;
        $quality = 38;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);

        $updatedItems = [];
        $previousQuality = $quality;

        for($i = 0; $i < $sellIn; $i++)
        {
            $gildedRose->updateQuality();
            $updatedItems = $gildedRose->getItems();
            $this->assertSame(($previousQuality + 1), $updatedItems[0]->quality);
            $previousQuality = $updatedItems[0]->quality;
        }

        $this->assertSame(0, $updatedItems[0]->sellIn);

        // Past the sell-in date
        for($i = 0; $i < 5; $i++)
        {
            $gildedRose->updateQuality();
            $updatedItems = $gildedRose->getItems();
            $this->assertLessThanOrEqual( $updatedItems[0]->quality, $previousQuality);
            $this->assertLessThanOrEqual(50, $updatedItems[0]->quality);
            $previousQuality = $updatedItems[0]->quality;
        }



    } // END testAgedBrieQualityIncreaseOverTime()

    public function testItemQualityNeverOver50(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = "Aged Brie";
        $sellIn = 10;
        $quality = 45;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();

        for($i = 0; $i < $sellIn; $i++)
        {
            $updatedItems = $gildedRose->getItems();
            $this->assertSame($name, $updatedItems[0]->name);
            $this->assertLessThanOrEqual(50, $updatedItems[0]->quality);
            $gildedRose->updateQuality();
        }


    } // END testItemQualityNeverOver50()

    public function testStdItemQualityNeverNegative(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'foo';
        $sellIn = 10;
        $quality = 5;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();

        for($i = 0; $i < $sellIn; $i++)
        {
            $updatedItems = $gildedRose->getItems();
            $this->assertSame($name, $updatedItems[0]->name);
            $this->assertGreaterThanOrEqual(0, $updatedItems[0]->quality);
            $gildedRose->updateQuality();
        }

    } // END testStdItemQualityNeverNegative()

    public function testStdItemQualityDegradesTwiceAsFastPastSellIn(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'foo';
        $sellIn = 10;
        $quality = 20;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);

        $previousQuality = $quality;

        $updatedItems = [];
        for($i = 0; $i < $sellIn; $i++)
        {
            $gildedRose->updateQuality();
            $updatedItems = $gildedRose->getItems();
            $this->assertSame(($previousQuality - 1), $updatedItems[0]->quality);
            $previousQuality = $updatedItems[0]->quality;
        }

        $this->assertSame(0, $updatedItems[0]->sellIn);

        for($i = 0; $i < 3; $i++)
        {
            $gildedRose->updateQuality();
            $updatedItems = $gildedRose->getItems();
            $this->assertSame(($previousQuality - 2), $updatedItems[0]->quality);
            $previousQuality = $updatedItems[0]->quality;
        }

    } // END testStdItemQualityDegradesTwiceAsFastPastSellIn()

    public function testSulfurasValuesNeverChange(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'Sulfuras, Hand of Ragnaros';
        $sellIn = 10;
        $quality = 80;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);

        $setItems = $gildedRose->getItems();

        // Make sure the data is the same as we insert
        $this->assertSame($name, $setItems[0]->name);
        $this->assertSame($sellIn, $setItems[0]->sellIn);
        $this->assertSame($quality, $setItems[0]->quality);

        $gildedRose->updateQuality();

        $updatedItems = $gildedRose->getItems();
        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame($sellIn, $updatedItems[0]->sellIn);
        $this->assertSame($quality, $updatedItems[0]->quality);

    } // END testSulfurasValuesNeverChange()

    public function testBackstagePassQualityIncreaseStandard(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'Backstage passes to a TAFKAL80ETC concert';
        $sellIn = 15;
        $quality = 10;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);

        $setItems = $gildedRose->getItems();

        // Make sure the data is the same as we insert
        $this->assertSame($name, $setItems[0]->name);
        $this->assertSame($sellIn, $setItems[0]->sellIn);
        $this->assertSame($quality, $setItems[0]->quality);

        $gildedRose->updateQuality();

        $updatedItems = $gildedRose->getItems();
        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame(($sellIn -1), $updatedItems[0]->sellIn);
        $this->assertSame(($quality + 1), $updatedItems[0]->quality);
    } // END testBackstagePassQualityIncreaseStandard()

    public function testBackstagePassQualityIncreaseBy2Within10Days(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'Backstage passes to a TAFKAL80ETC concert';
        $sellIn = 11;
        $quality = 10;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);

        $setItems = $gildedRose->getItems();

        // 11 days before
        $this->assertSame($name, $setItems[0]->name);
        $this->assertSame($sellIn, $setItems[0]->sellIn);
        $this->assertSame($quality, $setItems[0]->quality);

        $gildedRose->updateQuality();

        //10 days before
        $updatedItems = $gildedRose->getItems();
        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame(($sellIn -1), $updatedItems[0]->sellIn);
        $this->assertSame(($quality + 1), $updatedItems[0]->quality);

        $gildedRose->updateQuality();

        // 9 days before
        $updatedItems = $gildedRose->getItems();
        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame(($sellIn -2), $updatedItems[0]->sellIn);
        $this->assertSame(($quality + 3), $updatedItems[0]->quality);
    } // END testBackstagePassQualityIncreaseBy2Within10Days()

    public function testBackstagePassQualityIncreaseBy3Within5Days(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'Backstage passes to a TAFKAL80ETC concert';
        $sellIn = 6;
        $quality = 10;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);

        $setItems = $gildedRose->getItems();

        // 6 days before
        $this->assertSame($name, $setItems[0]->name);
        $this->assertSame($sellIn, $setItems[0]->sellIn);
        $this->assertSame($quality, $setItems[0]->quality);
        $gildedRose->updateQuality();

        //5 days before
        $updatedItems = $gildedRose->getItems();
        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame(($sellIn -1), $updatedItems[0]->sellIn);
        $this->assertSame(($quality + 2), $updatedItems[0]->quality);

        $gildedRose->updateQuality();

        // 4 days before
        $updatedItems = $gildedRose->getItems();
        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame(($sellIn -2), $updatedItems[0]->sellIn);
        $this->assertSame(($quality + 5), $updatedItems[0]->quality);
    } // END testBackstagePassQualityIncreaseBy3Within5Days()

    public function testBackstagePassQualityIs0AfterConcert(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = 'Backstage passes to a TAFKAL80ETC concert';
        $sellIn = 1;
        $quality = 10;

        $items = [
            new Item($name, $sellIn, $quality),
        ];

        $gildedRose = new GildedRose($items);

        $setItems = $gildedRose->getItems();

        // 1 days before
        $this->assertSame($name, $setItems[0]->name);
        $this->assertSame($sellIn, $setItems[0]->sellIn);
        $this->assertSame($quality, $setItems[0]->quality);
        $gildedRose->updateQuality();

        //0 days before
        $updatedItems = $gildedRose->getItems();
        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame(($sellIn -1), $updatedItems[0]->sellIn);
        $this->assertSame(($quality + 3), $updatedItems[0]->quality);

        $gildedRose->updateQuality();

        // 1 days after
        $updatedItems = $gildedRose->getItems();
        $this->assertSame($name, $updatedItems[0]->name);
        $this->assertSame(($sellIn -2), $updatedItems[0]->sellIn);
        $this->assertSame(0, $updatedItems[0]->quality);
    } // END testBackstagePassQualityIs0AfterConcert()


    /**
     * Conjured Items test
     */
    public function testConjuredItemQualityDegrade(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = "ConjuredFoo";
        $sellIn = 10;
        $quality = 20;

        $items = [
            new Item($name, $sellIn, $quality)
        ];

        $gildedRose = new GildedRose($items);

        $gildedRose->updateQuality();

        $conjuredItems = $gildedRose->getItems();
        $this->assertSame($name, $conjuredItems[0]->name);
        $this->assertSame(($sellIn - 1), $conjuredItems[0]->sellIn);
        $this->assertSame(($quality - 2), $conjuredItems[0]->quality);
    } // END testConjuredItemQualityDegrade()

    public function testConjuredAgedBrieQualityDegradeAsNormal(): void
    {
        echo ("Starting " . __FUNCTION__ . PHP_EOL);

        $name = "Conjured Aged Brie";
        $sellIn = 10;
        $quality = 20;

        $items = [
            new Item($name, $sellIn, $quality)
        ];

        $gildedRose = new GildedRose($items);

        $gildedRose->updateQuality();

        $conjuredItems = $gildedRose->getItems();
        $this->assertSame($name, $conjuredItems[0]->name);
        $this->assertSame(($sellIn - 1), $conjuredItems[0]->sellIn);
        $this->assertSame(($quality - 2), $conjuredItems[0]->quality);
    } // END testConjuredAgedBrieQualityDegradeAsNormal()

}
