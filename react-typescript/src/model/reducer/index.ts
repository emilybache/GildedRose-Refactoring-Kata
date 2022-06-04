import Item from '../../types';
import EEvents from '../EEvents';
import calculateQuality from '././calculateQuality';
import calculateSellIn from '././calculateSellIn';

function updateQuality(item: Item) {
  const calculatedQuality = calculateQuality(item);
  return { ...item, quality: calculatedQuality };
}

function updateSellIn(item: Item) {
    const calculatedSellIn = calculateSellIn(item);
    return { ...item, sellIn: calculatedSellIn };
}

function reducer(state: Item[], action: any): any {
    if(action.type === EEvents.SOLD_ITEM) {
        let targetItem = state.find((item) => (item.name.includes(action.payload.name)));
        if (targetItem !== undefined) {
            const calculatedItems = [...state];


        }
    }
    return state;
}

export default reducer;