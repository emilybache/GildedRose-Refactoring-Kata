import { TItem } from '../../types';
import updateQuality from './updateQuality';
import updateSellIn from './updateSellIn';

function getItemOnNextDay(item: TItem): TItem {
    const itemWithUpdatedSellIn = updateSellIn(item);
    const itemWithUpdatedQuality = updateQuality(itemWithUpdatedSellIn);
    return itemWithUpdatedQuality;
}

export default getItemOnNextDay;