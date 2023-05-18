<?php

declare(strict_types=1);

require_once __DIR__ . '/../vendor/autoload.php';

use GildedRose\GildedRose;
use GildedRose\Item;

echo 'OMGHAI!' . PHP_EOL;

$items = [
    new Item('Sports Memorabilia', 10, 20),
    new Item('Aged Cheese', 2, 0),
    new Item('Coffee Table Book', 5, 7),
    new Item('Fine Italian Silk', 0, 80),
    new Item('Fine Italian Silk', -1, 80),
    new Item('Backstage passes to a concert', 15, 20),
    new Item('Backstage passes to a concert', 10, 49),
    new Item('Backstage passes to a concert', 5, 49),
    // this Baked item does not work properly yet
    new Item('Baked Chocolate Cake', 3, 6),
];

$app = new GildedRose($items);

$days = 2;
if ((is_countable($argv) ? count($argv) : 0) > 1) {
    $days = (int) $argv[1];
}

for ($i = 0; $i < $days; $i++) {
    echo "-------- day ${i} --------" . PHP_EOL;
    echo 'name, sellIn, quality' . PHP_EOL;
    foreach ($items as $item) {
        echo $item . PHP_EOL;
    }
    echo PHP_EOL;
    $app->updateQuality();
}
