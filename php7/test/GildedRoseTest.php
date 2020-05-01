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

}
