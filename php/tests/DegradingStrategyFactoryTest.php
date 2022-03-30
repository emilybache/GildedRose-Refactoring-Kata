<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\Item;
use GildedRose\Services\AbstractDegradingStrategyFactory;
use GildedRose\Services\Strategies\ConjuredStrategy;
use GildedRose\Services\Strategies\DefaultStrategy;
use GildedRose\Services\Strategies\SulfurasStrategy;
use PHPUnit\Framework\TestCase;

class DegradingStrategyFactoryTest extends TestCase
{
    use InitApp;

    public $degradingStrategyFactory;

    public $container;

    protected function setUp(): void
    {
        $this->init();
        $this->degradingStrategyFactory = $this->container->get(AbstractDegradingStrategyFactory::class);
    }

    public function testStrategyFactoryIsLoaded(): void
    {
        $this->assertNotNull($this->degradingStrategyFactory);
        $this->assertInstanceOf(AbstractDegradingStrategyFactory::class, $this->degradingStrategyFactory);
    }

    public function testLoadsTheCorrectStrategy()
    {
        $item = $this->container->get(Item::class, ['Foo', 2, 3]);
        $this->assertInstanceOf(DefaultStrategy::class, $this->degradingStrategyFactory->getDegradingStrategy($item));

        $item = $this->container->get(Item::class, ['Sulfuras, Hand of Ragnaros', 0, 80]);
        $this->assertInstanceOf(SulfurasStrategy::class, $this->degradingStrategyFactory->getDegradingStrategy($item));

        $item = $this->container->get(Item::class, ['Conjured Mana Cake', 3, 6]);
        $this->assertInstanceOf(ConjuredStrategy::class, $this->degradingStrategyFactory->getDegradingStrategy($item));
    }
}
