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
        [商品：Sulfuras]
        ・sell_in、qualityどちらも変更しない
        [商品：Backstage passes]
        ・引数のsell_inを-1する
        ・引数のqualityを+1する
            ・計算後sell_inが10未満の場合、さらにquality+1する
            ・計算後sell_inが5未満の場合、さらにquality+1する
            ・計算後sell_inが0未満の場合、qualityを0にする
        [その他商品]
        ・引数のsell_inを-1する
        ・引数のqualityを-1する
            ・sell_inが0未満の場合、さらにquality-1する ★仕様書に記載なかったがコード上はこのようになっている
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
            } elseif ($this->item->name === 'Sulfuras, Hand of Ragnaros') {
                // 商品：Sulfurasの処理
                $this->sulfuras();
            } elseif ($this->item->name === 'Backstage passes to a TAFKAL80ETC concert') {
                // 商品：Backstage passesの処理
                $this->backstagePasses();
            } else {
                // その他商品の処理
                $this->others();
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
     * 商品：Sulfuras計算処理
     */
    private function sulfuras(): void
    {
        // 何もしない
    }

    /**
     * 商品：Backstage passes計算処理
     */
    private function backstagePasses(): void
    {
        $this->calcSellInSubtraction();
        $this->calcQualityAddition();

        // sell_inが10未満の場合、qualityを再加算する
        if ($this->item->sell_in < 10) {
            $this->calcQualityAddition();
        }
        // sell_inが5未満の場合、qualityを再加算する
        if ($this->item->sell_in < 5) {
            $this->calcQualityAddition();
        }
        // sell_inが0未満の場合、qualityを0する
        if ($this->item->sell_in < 0) {
            $this->item->quality = 0;
        }
    }

    /**
     * その他商品
     */
    private function others(): void
    {
        $this->calcSellInSubtraction();
        $this->calcQualitySubtraction();

        // sell_inが0未満の場合、sell_inを再減算する
        if ($this->item->sell_in < 0) {
            $this->calcQualitySubtraction();
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

    /**
     * qualityの減算を行う
     */
    private function calcQualitySubtraction(): void
    {
        // 1以上の場合計算
        if ($this->item->quality >= 1) {
            --$this->item->quality;
        }
    }
}

