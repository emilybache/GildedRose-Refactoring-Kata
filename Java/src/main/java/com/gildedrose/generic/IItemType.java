package com.gildedrose.generic;

import com.gildedrose.Item;
import com.gildedrose.enums.CountType;
import com.gildedrose.enums.AttributeType;

/***
 * IItemType helps to update the quality of the item
 * on daily basis and type of item invokes
 * @author VIJAY G
 *
 * @param <T> - Type of item
 */
public interface IItemType<T> extends EventHandler {
	
	int MIN = 0;
	int MAX = 50;
	int INCREMENTOR = 1;
	
	default void updateQuality(Item item) {
		if(isValid(item, AttributeType.QUALITY) && CountType.PLUS.equals(getT().getQualityType())) {
			increaseQualityByValue(item, getIncrementor(item));
		} else if(isValid(item, AttributeType.QUALITY) && CountType.MINUS.equals(getT().getQualityType())) {
			decreaseQualityByValue(item, getIncrementor(item));
		}
		if(isValid(item, AttributeType.SELL) && CountType.PLUS.equals(getT().getSellType())) {
			increaseSellInByValue(item, getIncrementor(item));
		} else if(isValid(item, AttributeType.SELL) && CountType.MINUS.equals(getT().getSellType())) {
			decreaseSellInByValue(item, getIncrementor(item));
		}
		
		if(isNeutralized(item)) {
			neutralizeQuality(item);
		}
	}
	
	default int getIncrementor(Item item) {
		return INCREMENTOR;
	}
	
	default boolean isNeutralized(Item item) {
		return false;
	}
	
	boolean isValid(Item item, AttributeType dayType);
	
	ItemType<T> getT();

}
