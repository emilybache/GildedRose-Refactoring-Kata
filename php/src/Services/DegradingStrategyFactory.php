<?php

namespace GildedRose\Services;

use GildedRose\Core\Container;
use GildedRose\Item;
use GildedRose\Services\Strategies\AgedBrieStrategy;
use GildedRose\Services\Strategies\BackstageStrategy;
use GildedRose\Services\Strategies\BaseStrategy;
use GildedRose\Services\Strategies\ConjuredStrategy;
use GildedRose\Services\Strategies\DefaultStrategy;
use GildedRose\Services\Strategies\SulfurasStrategy;

/**
 * DegradingStrategy builder that returns strategy based on item following Factory Design pattern
 */
class DegradingStrategyFactory implements AbstractDegradingStrategyFactory
{
    private $container;

    public function __construct(Container $container)
    {
        $this->container = $container;
    }

    public function getDegradingStrategy(Item $item): BaseStrategy
    {
        switch ($item->name) {
            case self::AGED:
                return $this->container->get(AgedBrieStrategy::class, [
                    'item' => $item,
                ]);
                break;
            case self::BACKSTAGE:
                return $this->container->get(BackstageStrategy::class, [
                    'item' => $item,
                ]);
                break;
            case self::SULFURAS:
                return $this->container->get(SulfurasStrategy::class, [
                    'item' => $item,
                ]);
                break;
            case self::CONJURED:
                return $this->container->get(ConjuredStrategy::class, [
                    'item' => $item,
                ]);
                break;

            default:
                return $this->container->get(DefaultStrategy::class, [
                    'item' => $item,
                ]);
        }
    }
}
