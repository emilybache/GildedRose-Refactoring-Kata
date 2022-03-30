<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\Item;
use GildedRose\Services\AbstractDegradingStrategyFactory;
use PHPUnit\Framework\TestCase;

class DegradingStrategyTest extends TestCase
{
    use InitApp;

    public $degradingStrategyFactory;

    public $container;

    protected function setUp(): void
    {
        $this->init();
        $this->degradingStrategyFactory = $this->container->get(AbstractDegradingStrategyFactory::class);
    }

    public function testDefaultStrategy(): void
    {
        $item = $this->container->get(Item::class, ['Foo', 2, 3]);
        $this->getStrategy($item)->handle();
        $this->assertSame(2, $item->quality);
        $this->assertSame(1, $item->sell_in);
        $item = $this->container->get(Item::class, ['Foo', -1, 3]);
        $this->getStrategy($item)->handle();
        $this->assertSame(1, $item->quality);
        $this->assertSame(-2, $item->sell_in);
    }

    public function testAgedBrieStrategy(): void
    {
        $item = $this->container->get(Item::class, ['Aged Brie', 2, 30]);
        $this->getStrategy($item)->handle();
        $this->assertSame(31, $item->quality);
        $this->assertSame(1, $item->sell_in);
        for ($i = 0; $i < 10; $i++) {
            $this->getStrategy($item)->handle();
        }

        $this->assertSame(50, $item->quality);
    }

    /**
     * "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
     *  Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
     *  Quality drops to 0 after the concert
     * Yet the results are not correct under approvals/ , as the sell_in reaches 0 the quality stand still
     * and only drop to 0 when sell_in < 0
     * As same for increasing by 2 when sell_in reaches 10, it only increases by 2 when it's < 10
     * As same also for sell_in reaching 5 it does not increase by 3 only after sell_in is lower than 5
     */
    public function testBackstageStrategy(): void
    {
        $item = $this->container->get(Item::class, ['Backstage passes to a TAFKAL80ETC concert', 15, 20]);
        $this->getStrategy($item)->handle();
        $this->assertSame(21, $item->quality);
        $this->assertSame(14, $item->sell_in);
        $item->sell_in = 10;
        $this->getStrategy($item)->handle();
        $this->assertSame(23, $item->quality);

        $item->sell_in = 5;
        $this->getStrategy($item)->handle();
        $this->assertSame(26, $item->quality);

        $item->sell_in = 0;
        $this->getStrategy($item)->handle();
        $this->assertSame(0, $item->quality);
    }

    /**
     * "Conjured" items degrade in Quality twice as fast as normal items
     * though the results under approvals/ are not correct since it doesn't
     * degrade as mentioned in requirements, it degrades same as AgedBrie
     */
    public function testConjuredStrategy(): void
    {
        $item = $this->container->get(Item::class, ['Conjured Mana Cake', 3, 8]);
        $this->getStrategy($item)->handle();
        $this->assertSame(6, $item->quality);
        $this->assertSame(2, $item->sell_in);
        $item->sell_in = 1;
        $this->getStrategy($item)->handle();
        $this->assertSame(4, $item->quality);
        $item->sell_in = 0;
        $this->getStrategy($item)->handle();
        $this->assertSame(0, $item->quality);
    }

    public function testSulfurasStrategy(): void
    {
        $item = $this->container->get(Item::class, ['Sulfuras, Hand of Ragnaros', 5, 80]);
        $this->getStrategy($item)->handle();
        $this->assertSame(80, $item->quality);
        $this->assertSame(5, $item->sell_in);

        $item->sell_in = 1;
        $this->getStrategy($item)->handle();
        $this->assertSame(80, $item->quality);
        $this->assertSame(1, $item->sell_in);

        $item->sell_in = 0;
        $this->getStrategy($item)->handle();
        $this->assertSame(80, $item->quality);
        $this->assertSame(0, $item->sell_in);
    }

    private function getStrategy($item)
    {
        return $this->degradingStrategyFactory->getDegradingStrategy($item);
    }
}
