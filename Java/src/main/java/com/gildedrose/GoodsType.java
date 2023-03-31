package com.gildedrose;

public enum GoodsType {

	AGED_BRIE("Aged Brie"), 
	BACK_STAGE_PASSES("Backstage passes to a TAFKAL80ETC concert"),
	SULFURAS("Sulfuras, Hand of Ragnaros"),
	CONJURED("Conjured Mana Cake"),
	;

	private final String goodsName;

	private GoodsType(String goodsName) {
		this.goodsName = goodsName;
	}

	public String getGoodsName() {
		return goodsName;
	}

}
