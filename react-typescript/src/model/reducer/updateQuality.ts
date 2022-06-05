import { TItem } from "../../types"
import clamp from "../../utils/clamp";

function updateQuality(item: TItem): TItem {
    const sellInAmount = item.sellIn;
    
    let calculatedQuality = item.quality;
    let degradeRate = 1;
    let qualityIdentifier = 1;

    if(item.name.toLowerCase().includes('sulfuras')) {
        return { ...item, quality: 80 };
    }

    if(item.name.toLowerCase().includes('aged brie')) {
        qualityIdentifier = -1;
    }

    if(item.name.toLowerCase().includes('backstage passes')) {
        qualityIdentifier = -1;
        if(sellInAmount <= 10 && sellInAmount > 6) {
            degradeRate = 2;
        }
        if(sellInAmount <= 6 && sellInAmount > 0) {
            degradeRate = 3;
        }
        if(sellInAmount <= 0) {
            return { ...item, quality: 0 };
        }
    }

    if(item.isConjured || item.sellIn < 0) {
        degradeRate = 2;
    }

    calculatedQuality = calculatedQuality - (qualityIdentifier * degradeRate);
    calculatedQuality = clamp(calculatedQuality, 0, 50);

    return { ...item, quality: calculatedQuality };
}

export default updateQuality;