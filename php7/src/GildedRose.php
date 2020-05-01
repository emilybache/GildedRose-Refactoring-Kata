<?php

/**
 * PHP version 7
 * 
 * @category Scientific_R&D
 * @package  Food_LIFO
 * @author   Povilas Brilius <pbrilius@gmail.com>
 * @license  eupl-1.1 https://help.github.com/en/github/creating-cloning-and-archiving-repositories/licensing-a-repository
 * @link     pbgroupeu.wordpress.com
 */

namespace App;

/**
 * Items processing unit
 * 
 * @category Accountancy
 * @package  Timeline_Graph
 * @author   Povilas Brilius <pbrilius@gmail.com>
 * @license  eupl-1.1 https://help.github.com/en/github/creating-cloning-and-archiving-repositories/licensing-a-repository
 * @link     pbgroupeu.wordpress.com
 */
final class GildedRose
{

    /**
     * Stock items
     *
     * @var array
     */
    private $_items = [];

    /**
     * Constructor with items
     *
     * @param array $items stock
     */
    public function __construct(array $items)
    {
        $this->_items = $items;
    }

    /**
     * Items getter
     *
     * @return array
     */
    public function getItems()
    {
        return $this->_items;
    }

    /**
     * Advance by a time frame of 1 day
     *
     * @return void
     */
    public function updateQuality()
    {
        foreach ($this->_items as $item) {
            if ($item->name === 'Sulfuras, Hand of Ragnaros') {
                continue;
            }

            if ($item->sell_in >= 0) {
                if ($item->name !== 'Aged Brie' && $item->name !== 'Backstage passes to a TAFKAL80ETC concert') {
                    if ($item->quality > 0) {
                        if (strstr($item->name, 'Conjured', true) === '') {
                            $item->quality -= 2;
                        } else {
                            $item->quality--;
                        }
                    }
                } else {
                    if ($item->quality < 50) {
                        if ($item->name === 'Aged Brie') {
                            $item->quality++;
                        } else if ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                            if ($item->sell_in <= 10 && $item->sell_in > 5) {
                                $item->quality += 2;
                            } else if ($item->sell_in <= 5) {
                                $item->quality += 3;
                            } else if ($item->sell_in > 10) {
                                $item->quality++;
                            }
                        }
                    }
                }
            } else {
                if ($item->name !== 'Aged Brie') {
                    if ($item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                        $item->quality = 0;
                    } else {
                        if ($item->quality > 0) {
                            $item->quality -= 2;
                        }
                    }
                } else {
                    if ($item->quality < 50) {
                        $item->quality++;
                    }
                }
            }

            $item->sell_in--;
        }
    }
}
