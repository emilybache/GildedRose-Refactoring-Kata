<?php

declare(strict_types=1);

namespace GildedRose;

use GildedRose\Services\AbstractDegradingStrategyFactory;

final class GildedRose
{
    /**
     * @var Item[]
     */
    private $items;

    /**
     * Degrading Strategy Factory
     * @var AbstractDegradingStrategyFactory
     */
    private $degradingStrategyFactory;

    public function __construct(array $items, AbstractDegradingStrategyFactory $degradingStrategyFactory)
    {
        $this->items = $items;
        $this->degradingStrategyFactory = $degradingStrategyFactory;
    }

    public function __get($name)
    {
        return property_exists($this, $name) ? $this->{$name} : null;
    }

    /**
     * Handle Updating quality in cleaner way respecting multiple software engineering principles
     * Such as Solid/Dry etc
     */
    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            $this->degradingStrategyFactory->getDegradingStrategy($item)->handle();
        }
    }
}
