<?php

declare(strict_types=1);

namespace GildedRose;

final class GildedRose
{
    /**
     * @var Item[]
     */
    private $items;

    private mixed $item;

    public function __construct(array $items)
    {
        $this->items = $items;
    }

    /*
    仕様解析
        [共通仕様]
        ・qualityはマイナスにならない、かつ50以上にならない
        ・sell_inの計算をしてからqualityの計算を行う
        [商品：Aged Brie]
        ・引数のsell_inを-1する
        ・計算後sell_inが0未満の場合、quality+2する
        ・計算後sell_inが0以上の場合、quality+1する
    */

    /**
     * メイン処理
     */
    public function updateQuality(): void
    {
        foreach ($this->items as $this->item) {
            if ($this->item->name === 'Aged Brie') {
                // 商品：Aged Brieの処理
                $this->agedBrie();
            }
        }
    }

    /**
     * 商品：Aged Brie計算処理
     */
    private function agedBrie(): void
    {
        $this->calcSellInSubtraction();
        $this->calcQualityAddition();

        // sell_inが0未満の場合、qualityを再加算する
        if ($this->item->sell_in < 0) {
            $this->calcQualityAddition();
        }
    }

    /**
     * sell_inの減算を行う
     */
    private function calcSellInSubtraction(): void
    {
        --$this->item->sell_in;
    }

    /**
     * qualityの加算を行う
     */
    private function calcQualityAddition(): void
    {
        // 50未満の場合計算
        if ($this->item->quality < 50) {
            ++$this->item->quality;
        }
    }
}

