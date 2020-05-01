<?php

/**
 * PHP version 7
 * 
 * @category Stock_Items
 * @package  Storehouse
 * @author   Povilas Brilius <pbrilius@gmail.com>
 * @license  eupl-1.1 https://help.github.com/en/github/creating-cloning-and-archiving-repositories/licensing-a-repository
 * @link     pbgroupeu.wordpress.com
 */

namespace App;

/**
 * Stock item
 * 
 * @category LIFO
 * @package  Storehouse
 * @author   Povilas Brilius <pbrilius@gmail.com>
 * @license  eupl-1.1 https://help.github.com/en/github/creating-cloning-and-archiving-repositories/licensing-a-repository
 * @link     pbgroupeu.worpress.com
 */
final class Item
{

    public $name;
    public $sell_in;
    public $quality;

    /**
     * Std constructor
     *
     * @param string $name Label
     * @param integer $sell_in Sellout in days
     * @param integer $quality Qualitative assessment in integer type
     */
    function __construct(string $name, int $sell_in, int $quality)
    {
        $this->name    = $name;
        $this->sell_in = $sell_in;
        $this->quality = $quality;
    }

    /**
     * Typographed to string magic
     *
     * @return string
     */
    public function __toString()
    {
        return "{$this->name}, {$this->sell_in}, {$this->quality}";
    }
}
