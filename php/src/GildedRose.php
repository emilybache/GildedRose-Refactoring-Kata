<?php

declare(strict_types=1);

namespace GildedRose;


final class GildedRose
{
    /**
     * @var Item[]
     */
    private $items;

    public function __construct(array $items)
    {
        $this->items = $items;
    }

    public function updateQuality(): void
    {
        foreach ($this->items as $item) {

            $firstItem = new FirstProcessingRule($item);

            $firstItem->firstProcessingRule();

            $secondItem = new SecondProcessingRule($item);

            $secondItem->secondProcessingRule();

            $thirdItem = new ThirdProcessingRule($item);

            $thirdItem->thirdProcessingRule();
        }
    }

}
