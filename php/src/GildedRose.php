<?php

declare(strict_types=1);

namespace GildedRose;

final class GildedRose
{
    /**
     * @var Item[]
     */
    private $items;

    /**
     * @var StrategyResolverInterface
     */
    private $strategyResolver;

    public function __construct(array $items, StrategyResolverInterface $strategyResolver)
    {
        $this->items = $items;
        $this->strategyResolver = $strategyResolver;
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {
            $sellInStrategy = $this->strategyResolver->getSellInStrategy($item);
            $sellInStrategy->updateSellIn($item);

            $qualityStrategy = $this->strategyResolver->getQualityStrategy($item);
            $qualityStrategy->updateQuality($item);
        }
    }
}
