<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use GildedRose\StrategyResolver;
use PHPUnit\Framework\TestCase as CoreTestCase;

abstract class TestCase extends CoreTestCase
{
    public function assertSameItem(Item $expected, Item $actual, ?string $key = null): void
    {
        $actualKey = $key ?? $actual->name;

        $this->assertSame($expected->name, $actual->name, sprintf('Item\'s "name" is not equal to expected; expected: %s, actual: %s', $expected->name, $actual->name));
        $this->assertSame($expected->sell_in, $actual->sell_in, sprintf('Item\'s "sell in" is not equal to expected; expected: %d, actual: %d, key: %s', $expected->sell_in, $actual->sell_in, $actualKey));
        $this->assertSame($expected->quality, $actual->quality, sprintf('Item\'s "quality" is not equal to expected; expected: %d, actual: %d, key: %s', $expected->quality, $actual->quality, $actualKey));
    }

    /**
     * @param Item[] $expected
     * @param Item[] $actual
     */
    public function assertSameItems(array $expected, array $actual): void
    {
        foreach ($actual as $key => $actualItem) {
            $this->assertSameItem($expected[$key], $actualItem, $key);
        }
    }

    /**
     * @param Item[] $items
     */
    protected function initializeGlidedRose(array $items): GildedRose
    {
        return new GildedRose($items, new StrategyResolver());
    }
}
