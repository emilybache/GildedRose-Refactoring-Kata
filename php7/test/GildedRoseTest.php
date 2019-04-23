<?php

namespace App;

require_once __DIR__ . '/../vendor/autoload.php';

class GildedRoseTest extends \PHPUnit\Framework\TestCase {
    public function testFoo() {
        $items = array(new Item("foo", 0, 0));
        $gildedRose = new GildedRose($items);
        $gildedRose->updateQuality();
        $this->assertEquals("fixme", $items[0]->name);
    }
}
