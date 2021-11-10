<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\Item;
use PHPUnit\Framework\TestCase;

class ItemTest extends TestCase
{
    /**
     * @group Item
     */
    public function testItemCanBeCreated(): void
    {
        $item = new Item('foo', 1, 2);

        $this->assertSame('foo', $item->name);
        $this->assertSame(1, $item->sell_in);
        $this->assertSame(2, $item->quality);
    }

    /**
     * @group Item
     */
    public function testItemToStringIsCorrect(): void
    {
        $item = new Item('foo', 1, 2);

        $this->assertSame('foo, 1, 2', (string) $item);
    }
}
