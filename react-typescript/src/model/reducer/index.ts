import { TItem } from '../../types';
import EEvents from '../EEvents';
import calculateQuality from '././calculateQuality';
import calculateSellIn from '././calculateSellIn';

function updateQuality(item: TItem): TItem {
  const calculatedQuality = calculateQuality(item);
  return { ...item, quality: calculatedQuality };
}

function updateSellIn(item: TItem): TItem {
    const calculatedSellIn = calculateSellIn(item);
    return { ...item, sellIn: calculatedSellIn };
}

function reducer(state: TItem[], action: any): TItem[] {
    if(action.type === EEvents.NEXT_DAY) {
        return state.map(item => {
            if(item.name.includes(action.payload.name)) {
                return updateQuality(updateSellIn(item));
            }
            return item;
        });
    }
    return state;
}

export default reducer;