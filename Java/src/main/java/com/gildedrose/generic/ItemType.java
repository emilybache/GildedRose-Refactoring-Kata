package com.gildedrose.generic;

import com.gildedrose.Item;
import com.gildedrose.enums.CountType;
import com.gildedrose.enums.AttributeType;

/***
 * ItemType provides as a template for all types of items, 
 * which can be appendable/detachable to the system at any time.
 * @author VIJAY G
 *
 * @param <T>
 */
public abstract class ItemType<T> implements IItemType<T> {
	
	private boolean validationRequired;
	private CountType sellType;
	private CountType qualityType;
	
	protected ItemType(boolean validationRequired, CountType sellType, CountType qualityType) {
		this.validationRequired = validationRequired;
		this.sellType = sellType;
		this.qualityType = qualityType;
	}
	
	public boolean isValid(Item item, AttributeType dayType) {
		boolean validationRequired = getT().isValidationRequired();
		if(!validationRequired) {
			return true;
		}
		if(dayType.equals(AttributeType.QUALITY) && CountType.PLUS.equals(getT().getQualityType())) {
			return item.quality < MAX;
		} else if(dayType.equals(AttributeType.QUALITY) && CountType.MINUS.equals(getT().getQualityType())) {
			return item.quality > MIN;
		}
		if(dayType.equals(AttributeType.SELL) && CountType.PLUS.equals(getT().getQualityType())) {
			return item.sellIn < MAX;
		} else if(dayType.equals(AttributeType.SELL) && CountType.MINUS.equals(getT().getQualityType())) {
			return item.sellIn > MIN;
		}
		return false;
	}

	public ItemType<T> getT() {
		return this;
	}

	public CountType getSellType() {
		return sellType;
	}

	public CountType getQualityType() {
		return qualityType;
	}

	public boolean isValidationRequired() {
		return validationRequired;
	}

}
