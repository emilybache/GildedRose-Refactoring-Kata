<?php

declare(strict_types=1);

namespace Tests;

use GildedRose\Core\Container;
use GildedRose\GildedRose;

/**
 * Small Trait to speed up bootstrapping process and avoid Dry code
 */
trait InitApp
{
    public function init()
    {
        $this->container = new Container();
    }

    public function bootstrap()
    {
        $this->app = $this->container->get(GildedRose::class, [$this->items]);
    }
}
