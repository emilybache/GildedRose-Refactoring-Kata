import { useStore } from "../model";
import EEvents from "../model/EEvents";
import Item from "../types"

function useItem() {
    const { state, dispatch } = useStore();

    return {
        items: state,
        
        updateItem: (item: Item):void => {
            dispatch({
                type: EEvents.SOLD_ITEM,
                payload: {
                    name: item.name
                }
            });
        }
    };
}

export default useItem;