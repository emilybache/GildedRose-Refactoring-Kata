<?php

/**
 * PHP version 7
 * 
 * @category PHP_Unit
 * @package  Drill_Field
 * @author   Povilas Brilius <pbrilius@gmail.com>
 * @license  eupl-1.1 https://help.github.com/en/github/creating-cloning-and-archiving-repositories/licensing-a-repository
 * @link     pbgroupeu.wordpress.com
 */
namespace App;

/**
 * Units cover up
 * 
 * @category PHP_Unit
 * @package  GildenRose
 * @author   Povilas Brilius <pbrilius@gmail.com>
 * @license  eupl-1.1 https://help.github.com/en/github/creating-cloning-and-archiving-repositories/licensing-a-repository
 * @link     pbgroupeu.wordpress.com
 */
class GildedRoseTest extends \PHPUnit\Framework\TestCase
{
    /**
     * Product, that is unaffected by processing
     *
     * @return void
     */
    public function testStableUnaffected()
    {
        /**
         * Stock items
         * 
         * @var Item[] $items
         */
        $items = [
            new Item('Sulfuras, Hand of Ragnaros', 2, 80),
            new Item('Sulfuras, Hand of Ragnaros', -1, 80),
        ];

        $interval = 6;
        $app = new GildedRose($items);

        for ($i = 0; $i < $interval; $i++) {
            $app->updateQuality();
        }

        $this->assertEquals(2, $app->getItems()[0]->sell_in);
        $this->assertEquals(-1, $app->getItems()[1]->sell_in);
        $this->assertEquals(80, $app->getItems()[0]->quality);
        $this->assertEquals(80, $app->getItems()[1]->quality);
    }

    /**
     * Test geometricc progression increasing quality
     *
     * @return void
     */
    public function testGeometricQualityProgression()
    {
        $items = [ 
            new Item('Backstage passes to a TAFKAL80ETC concert', 12, 15),
            new Item('Backstage passes to a TAFKAL80ETC concert', 7, 14),
            new Item('Backstage passes to a TAFKAL80ETC concert', 3, 18),
        ];

        $interval = 2;
        $app = new GildedRose($items);

        for ($i = 0;  $i < $interval; $i++) {
            $app->updateQuality();
        }

        $this->assertEquals(17, $app->getItems()[0]->quality);
        $this->assertEquals(18, $app->getItems()[1]->quality);
        $this->assertEquals(24, $app->getItems()[2]->quality);

        $this->assertEquals(10, $app->getItems()[0]->sell_in);
        $this->assertEquals(5, $app->getItems()[1]->sell_in);
        $this->assertEquals(1, $app->getItems()[2]->sell_in);
        
    }

    /**
     * Simple products quality and quantity test 
     *
     * @return void
     */
    public function testSimpleProducts()
    {
        $items = [
            new Item('Elixir of the Mongoose', 16, 24),
            new Item('Elixir of the Mongoose', 14, 32),
            new Item('Elixir of the Mongoose', 12, 36),
            new Item('Elixir of the Mongoose', 18, 25),
        ];

        $interval = 5;
        $app = new GildedRose($items);

        for ($i = 0; $i < $interval; $i++) {
            $app->updateQuality();
        }

        $this->assertEquals(11, $app->getItems()[0]->sell_in);
        $this->assertEquals(9, $app->getItems()[1]->sell_in);
        $this->assertEquals(7, $app->getItems()[2]->sell_in);
        $this->assertEquals(13, $app->getItems()[3]->sell_in);

        $this->assertEquals(19, $app->getItems()[0]->quality);
        $this->assertEquals(27, $app->getItems()[1]->quality);
        $this->assertEquals(31, $app->getItems()[2]->quality);
        $this->assertEquals(20, $app->getItems()[3]->quality);

    }

    /**
     * Conjured products test
     *
     * @return void
     */
    public function testFeatureConjuredProducts()
    {
        $items = [
            new Item('Conjured Mana Cake', 8, 18),
            new Item('Conjured Mana Cake', 6, 16),
            new Item('Conjured Mana Cake', 7, 20),
        ];

        $interval = 3;
        $app = new GildedRose($items);

        for ($i = 0; $i < $interval; $i++) {
            $app->updateQuality();
        }

        $this->assertEquals(12, $app->getItems()[0]->quality);
        $this->assertEquals(10, $app->getItems()[1]->quality);
        $this->assertEquals(14, $app->getItems()[2]->quality);

        $this->assertEquals(5, $app->getItems()[0]->sell_in);
        $this->assertEquals(3, $app->getItems()[1]->sell_in);
        $this->assertEquals(4, $app->getItems()[2]->sell_in);
    }

    /**
     * Maturing products like Aged Brie
     *
     * @return void
     */
    public function testMaturingProducts()
    {
        $items = [
            new Item('Aged Brie', 10, 14),
            new Item('Aged Brie', 11, 16),
            new Item('Aged Brie', 13, 21),
        ];

        $interval = 4;
        $app = new GildedRose($items);

        for ($i = 0; $i < $interval; $i++) {
            $app->updateQuality();
        }

        $this->assertEquals(18, $app->getItems()[0]->quality);
        $this->assertEquals(20, $app->getItems()[1]->quality);
        $this->assertEquals(25, $app->getItems()[2]->quality);

        $this->assertEquals(6, $app->getItems()[0]->sell_in);
        $this->assertEquals(7, $app->getItems()[1]->sell_in);
        $this->assertEquals(9, $app->getItems()[2]->sell_in);
    }

    /**
     * Test stage prouct jumps to 0 quality after sale pass
     *
     * @return void
     */
    public function testQualityDipToZero()
    {
        $items = [
            new Item('Backstage passes to a TAFKAL80ETC concert', 1, 14),
        ];

        $interval = 3;
        $app = new GildedRose($items);

        for ($i = 0; $i < $interval; $i++) {
            $app->updateQuality();
        }

        $this->assertEquals(0, $app->getItems()[0]->quality);
    }

    /**
     * Regular products max quality assessment value limit
     *
     * @return void
     */
    public function testQualityMaxLimitOnSimpleProduct()
    {
        $items = [
            new Item('Backstage passes to a TAFKAL80ETC concert', 20, 49),
            new Item('Backstage passes to a TAFKAL80ETC concert', 20, 48),
            new Item('Backstage passes to a TAFKAL80ETC concert', 20, 50),
        ];

        $interval = 3;
        $app = new GildedRose($items);

        for ($i = 0; $i < $interval; $i++) {
            $app->updateQuality();
        }

        $this->assertEquals(50, $app->getItems()[0]->quality);
        $this->assertEquals(50, $app->getItems()[1]->quality);
        $this->assertEquals(50, $app->getItems()[2]->quality);

    }

    /**
     * Test lower quality boundary of a product is prevalent
     *
     * @return void
     */
    public function testQualityLowerBoundaryOnRegularProduct()
    {
        $items = [
            new Item('Elixir of the Mongoose', 5, 1),
            new Item('Elixir of the Mongoose', 5, 2),
            new Item('Elixir of the Mongoose', 5, 3),
        ];

        $interval = 3;
        $app = new GildedRose($items);

        for ($i = 0; $i < $interval; $i++) {
            $app->updateQuality();
        }

        $this->assertEquals(0, $app->getItems()[0]->quality);
        $this->assertEquals(0, $app->getItems()[1]->quality);
        $this->assertEquals(0, $app->getItems()[2]->quality);
    }

}
