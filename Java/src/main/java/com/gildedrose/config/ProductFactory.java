package com.gildedrose.config;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.gildedrose.enums.ProductType;
import com.gildedrose.generic.ItemType;
import com.gildedrose.types.AgedBrie;
import com.gildedrose.types.Backstage;
import com.gildedrose.types.Conjured;
import com.gildedrose.types.Others;
import com.gildedrose.types.Sulfuras;

/***
 * ProductFactory defines the instance of each items based on description of the item
 * @author VIJAY G
 *
 */
public class ProductFactory {

	@SuppressWarnings("rawtypes")
	private static Map<ProductType, ItemType> productMap = new HashMap<>();
	static {
		productMap.put(ProductType.AGED_BRIE, new AgedBrie());
		productMap.put(ProductType.SULFURAS, new Sulfuras());
		productMap.put(ProductType.BACKSTAGE_PASSES, new Backstage());
		productMap.put(ProductType.CONJURED, new Conjured());
	}
	
	@SuppressWarnings("rawtypes")
	public static ItemType get(ProductType productType) {
		ItemType itemType = productMap.get(productType);
		if(Objects.isNull(itemType)) {
			return new Others();
		}
		return itemType;
	}

}
