<?php

declare(strict_types=1);

namespace GildedRose;

use Item;

final class GildedRose
{
    /**
     * @var Item[]
     */
    private $items;

    /**
     * @var int
     */
    private $qualityModifier = -1;

    /**
     * @var int
     */
    private $minQuality = 0;

    /**
     * @var int
     */
    private $maxQuality = 50;

    public function __construct(array $items)
    {
        $this->items = $items;
    }

    public function updateQuality(): void
    {
        // all this config could be stored on the item or collected from a database
        $ruleItems = [
            'Aged Brie' => 'aged',
            'Conjured Mana Cake' => 'conjured',
            'Backstage passes to a TAFKAL80ETC concert' => 'backstagePasses',
            'Sulfuras, Hand of Ragnaros' => 'legendary',
        ];

        // here i would set up a class for each type of item, it store validation in a database
        // potentially a global rules class might suit this
        $agedRules = [
            'quality' => 1,
            'minQuality' => -1,
        ];
        $conjuredRules = [
            'quality' => -2
        ];
        $backstagePassesRules = [
            'quality' => [
                'sell_in' => [
                    1 => 0,
                    6 => 3,
                    11 => 2,
                    'default' => 1,
                ],
            ],
        ];
        $legendaryRules = [];

        foreach ($this->items as $item) {
            if (isset($ruleItems[$item->name])) {
                $ruleArrayName = $ruleItems[$item->name] . "Rules";
                $item = $this->handleItemRules($item, $$ruleArrayName);
            } else {
                $qualityModifier = ($item->sell_in < 0) ? $this->qualityModifier * 2: $this->qualityModifier;
                if ($item->quality > $this->minQuality) {
                    $item->quality = $item->quality + $this->qualityModifier;
                }
                $item->sell_in = $item->sell_in - 1;
            }
        }
    }

    private function handleItemRules(Object $item, Array $rules)
    {
        if (!empty($rules)) {
            if (isset($rules['quality'])) {
                if (is_array($rules['quality'])) {
                    foreach ($rules['quality']['sell_in'] as $expiry => $ruleQuantityModifier) {
                        if ($item->sell_in < $expiry) {
                            if ($ruleQuantityModifier == 0) {
                                $qualityModifier = -$item->quality;
                            } else {
                                $qualityModifier = $ruleQuantityModifier;
                            }
                            break;
                        }
                    }
                } else {
                    $qualityModifier = $rules['quality'];
                }
            }

            // make sure default is set
            $qualityModifier = $qualityModifier ?? $this->qualityModifier;
            $qualityModifier = ($item->sell_in < 1) ? $qualityModifier * 2: $qualityModifier;

            $minQuality = $rules['minQuality'] ?? $this->minQuality;
            $maxQuality = $rules['maxQuality'] ?? $this->maxQuality;

            $item->quality += $qualityModifier;
            if ($item->quality > $maxQuality) {
                $item->quality = $maxQuality;
            } else if ($item->quality < $minQuality) {
                $item->quality = $minQuality;
            }

            $item->sell_in = $item->sell_in - 1;
        }
        return $item;
    } 
}
