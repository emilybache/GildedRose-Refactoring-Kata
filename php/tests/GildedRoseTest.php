<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\GildedRose;
use GildedRose\Item;
use GildedRose\Services\AbstractDegradingStrategyFactory;
use PHPUnit\Framework\TestCase;

class GildedRoseTest extends TestCase
{
    use InitApp;

    private $items;

    private $app;

    protected function setUp(): void
    {
        $this->init();
        $this->items = [$this->container->get(Item::class, ['Aged Brie', 2, 0])];

        $this->bootstrap();
    }

    public function testApplicationIsInstanciatedCorrectly(): void
    {
        $this->assertNotNull($this->app);
        $this->assertInstanceOf(GildedRose::class, $this->app);
        $this->assertInstanceOf(AbstractDegradingStrategyFactory::class, $this->app->degradingStrategyFactory);
    }

    public function testItemsAreSetAndCorrect()
    {
        $this->assertIsArray($this->items);
        $this->assertIsArray($this->app->items);
        $this->assertContainsOnlyInstancesOf(Item::class, $this->app->items);
        $this->assertSame(new Item('Aged Brie', 2, 0), array_pop($this->items));
    }

    public function testUpdateQuality()
    {
        $this->app->updateQuality();
        $this->assertSame(new Item('Aged Brie', 1, 1), $this->app->items[0]);
    }
}
