package com.gildedrose.factory;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import com.gildedrose.AgedBrie;
import com.gildedrose.BackStagePasses;
import com.gildedrose.Conjured;
import com.gildedrose.Goods;
import com.gildedrose.GoodsType;
import com.gildedrose.Generic;
import com.gildedrose.Sulfuras;

public class GoodsFactory {
	private static final Map<String, Supplier<Goods>> GOODS_SUPPLIER;

	static {
		final Map<String, Supplier<Goods>> goods = new HashMap<>();
		goods.put(GoodsType.AGED_BRIE.getGoodsName(), AgedBrie::new);
		goods.put(GoodsType.BACK_STAGE_PASSES.getGoodsName(), BackStagePasses::new);
		goods.put(GoodsType.SULFURAS.getGoodsName(), Sulfuras::new);
		goods.put(GoodsType.CONJURED.getGoodsName(), Conjured::new);

		GOODS_SUPPLIER = Collections.unmodifiableMap(goods);
	}

	public static Goods getGoods(String goodsType) {
		Supplier<Goods> goods = GOODS_SUPPLIER.get(goodsType);

		if (goods == null) {
			return new Generic();
		}
		return goods.get();
	}

	private GoodsFactory() {

	}
}