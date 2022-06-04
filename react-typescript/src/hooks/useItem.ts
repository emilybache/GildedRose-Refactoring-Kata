import { useStore } from "../model";
import EEvents from "../model/EEvents";
import { TItem } from "../types"

function useItem() {
    const { state, dispatch } = useStore();

    return {
        items: state,
        
        updateItem: (item: TItem):void => {
            dispatch({
                type: EEvents.NEXT_DAY,
                payload: {
                    name: item.name
                }
            });
        }
    };
}

export default useItem;