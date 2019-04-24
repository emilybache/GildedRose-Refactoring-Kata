<?php

namespace App;

class GildedRoseTest extends \PHPUnit\Framework\TestCase {
    public function testFoo() {
        $items = array(new Item("foo", 0, 0));
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals("foo", $items[0]->name);
        $this->assertEquals("foo, -1, 0", (string)$items[0]);
    }

    /**
     * @dataProvider itemProvider
     */
    public function testItems(Item $item, $expected, $sellIn) {
        $rose = new GildedRose([$item]);

        while ($sellIn > 1) {
            $rose->updateQuality();
            $sellIn--;
        }

        $actual = $item->quality;
        $this->assertEquals($expected, $actual);
    }

    public function itemProvider() {
        // if you comment out any assignment/increment/decrement operation in updateQuality,
        // at least one of these tests should fail. If there isn't a failure, then perhaps
        // that line of code does not need to exist.
        return [
            'foo'         => ['item' => new Item('foo', 9, 10), 'expected' => 2, 'sell_in' => 9],
            'Aged Brie'   => ['item' => new Item('Aged Brie', 9, 10), 'expected' => 18, 'sell_in' => 9],
            'Backstage'   => ['item' => new Item('Backstage passes to a TAFKAL80ETC concert', 9, 10), 'expected' => 30, 'sell_in' => 9],
            'Backstage2'   => ['item' => new Item('Backstage passes to a TAFKAL80ETC concert', 9, 10), 'expected' => 0, 'sell_in' => 19],
            'Backstage3'   => ['item' => new Item('Backstage passes to a TAFKAL80ETC concert', 9, 10), 'expected' => 0, 'sell_in' => 19],
            'Sulfuras1'   => ['item' => new Item('Sulfuras, Hand of Ragnaros', 9, 10), 'expected' => 80, 'sell_in' => 9],
            'aged-foo'    => ['item' => new Item('foo', 3, 14), 'expected' => 1, 'sell_in' => 9],
            'qualityFoo'  => ['item' => new Item('foo', 3, 94), 'expected' => 39, 'sell_in' => 30],
            'qualityBrie' => ['item' => new Item('Aged Brie', 3, 94), 'expected' => 94, 'sell_in' => 30],
            'cheapBrie' => ['item' => new Item('Aged Brie', 3, 3), 'expected' => 50, 'sell_in' => 30],
        ];
    }
}
