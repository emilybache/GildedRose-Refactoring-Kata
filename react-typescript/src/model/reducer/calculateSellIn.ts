function updateSellIn (state: any): number {
    let calculatedSellIn = state.sellIn;

    if(state.name.includes("Sulfuras")) {
        return calculatedSellIn;
    }

    calculatedSellIn = calculatedSellIn - 1;

    if(calculatedSellIn < 0) {
        return 0;
    }

    return calculatedSellIn;
}

export default updateSellIn;