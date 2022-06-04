function updateSellIn (state: any): number {
    let calculatedSellIn = state.sellIn;

    if(state.name.includes("Sulfuras")) {
        return calculatedSellIn;
    }

    return calculatedSellIn - 1;
}

export default updateSellIn;