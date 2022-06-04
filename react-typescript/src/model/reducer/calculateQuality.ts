import Item from "../../types"

function calculateQuality(state: Item): number {
    const sellInAmount = state.sellIn;
    
    let calculatedQuality = state.quality;
    let degradeRate = 1;
    let qualityIdentifier = 1;

    if(state.name.includes("Sulfuras")) {
        return 80;
    }

    if(state.name.includes("Aged Brie")) {
        qualityIdentifier = -1;
    }

    if(state.name.includes("Backstage passes")) {
        qualityIdentifier = -1;
        if(sellInAmount <= 10 && sellInAmount > 6) {
            degradeRate = 2;
        }
        if(sellInAmount <= 6 && sellInAmount > 0) {
            degradeRate = 3;
        }
        if(sellInAmount <= 0) {
            return 0;
        }
    }

    if(state.isConjured) {
        degradeRate = 2;
    }

    if(state.sellIn <= 0) {
        return calculatedQuality;
    }

    calculatedQuality = calculatedQuality - (qualityIdentifier * degradeRate);

    if(calculatedQuality > 50) {
        return 50;
    }

    if(calculatedQuality < 0) {
        return 0;
    }

    return calculatedQuality;
}

export default calculateQuality;