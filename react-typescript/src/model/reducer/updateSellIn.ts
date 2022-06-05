import { TItem } from '../../types';

function updateSellIn (state: TItem): TItem {
    let calculatedSellIn = state.sellIn;

    if(state.name.toLowerCase().includes('sulfuras')) {
        return {...state, sellIn: calculatedSellIn};
    }

    return { ...state, sellIn: calculatedSellIn - 1 };
}

export default updateSellIn;