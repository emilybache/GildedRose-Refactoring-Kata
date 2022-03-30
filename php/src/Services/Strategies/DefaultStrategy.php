<?php

namespace GildedRose\Services\Strategies;

use GildedRose\Item;

/**
 * Behaves as a default Strategy and template for other strategies
 * based on Template Method Design pattern and factory method design pattern for setDegrateRate method
 * in a way to share common code and
 */
class DefaultStrategy implements BaseStrategy
{
    protected $degradeRate = 1;

    protected $item;

    public function __construct(Item $item)
    {
        $this->item = $item;
    }

    public function setDegradeRate()
    {
        if ($this->item->sell_in < 0) {
            $this->degradeRate *= 2;
        }
    }

    public function setSellIn(): Item
    {
        $this->item->sell_in--;
        return $this->item;
    }

    public function setQuality(): Item
    {
        $this->item->quality = ($this->item->quality - $this->degradeRate) < self::MIN_QUALITY_VALUE ?
            self::MIN_QUALITY_VALUE : $this->item->quality -= $this->degradeRate;
        return $this->item;
    }

    /**
     * Template Method that runs steps for all strategies in order
     * First we Downgrade the sellIn
     * Second We set the degradeRate/riseRate
     * Finally we Set the item quality based on degrate/rise Rate
     */
    final public function handle()
    {
        $this->setSellIn();
        $this->setDegradeRate();
        $this->setQuality();
    }
}
