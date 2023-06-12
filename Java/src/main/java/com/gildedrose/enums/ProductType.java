package com.gildedrose.enums;

/***
 * ProductType describes the type of Items
 * @author VIJAY G
 *
 */
public enum ProductType {
	
	AGED_BRIE("Aged Brie"), 
	BACKSTAGE_PASSES("Backstage passes to a TAFKAL80ETC concert"),
	SULFURAS("Sulfuras, Hand of Ragnaros"),
	CONJURED("Conjured Mana Cake");
	
	private final String productName;
	
	private ProductType(String productName) {
		this.productName = productName;
	}

	public static ProductType getEnumByString(String code){
        for(ProductType e : ProductType.values()){
            if(e.productName.equals(code)) return e.valueOf(e.name());
        }
        return null;
    }

}
