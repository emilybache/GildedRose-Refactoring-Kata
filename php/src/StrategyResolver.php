<?php

namespace GildedRose;

use GildedRose\QualityStrategy\AgedBrieQualityStrategy;
use GildedRose\QualityStrategy\BackstagePassesQualityStrategy;
use GildedRose\QualityStrategy\BasicQualityStrategy;
use GildedRose\QualityStrategy\ConjuredQualityStrategy;
use GildedRose\QualityStrategy\QualityStrategyInterface;
use GildedRose\QualityStrategy\SulfurasQualityStrategy;
use GildedRose\SellInStrategy\BasicSellInStrategy;
use GildedRose\SellInStrategy\SellInStrategyInterface;
use GildedRose\SellInStrategy\SulfurasSellInStrategy;

class StrategyResolver implements StrategyResolverInterface
{
    public function getSellInStrategy(Item $item): SellInStrategyInterface
    {
        switch ($item->name) {
            case self::ITEM_NAME_SULFURAS:
                return new SulfurasSellInStrategy();
            default:
                break;
        }

        return new BasicSellInStrategy();
    }

    public function getQualityStrategy(Item $item): QualityStrategyInterface
    {
        switch ($item->name) {
            case self::ITEM_NAME_AGED_BRIE:
                return new AgedBrieQualityStrategy();
            case self::ITEM_NAME_SULFURAS:
                return new SulfurasQualityStrategy();
            case self::ITEM_NAME_BACKSTAGE_PASSES:
                return new BackstagePassesQualityStrategy();
            case self::ITEM_NAME_CONJURED:
                return new ConjuredQualityStrategy();
            default:
                break;
        }

        return new BasicQualityStrategy();
    }
}
