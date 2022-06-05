import { useStore } from "../model";
import {TItem} from "../types";
 
function useItem(item: TItem): TItem {
  const { state } = useStore();
  const itemFound = state.items.find(target => target.name.toLowerCase().includes(item.name.toLowerCase()));
  return itemFound === undefined ? item : itemFound;
}

export default useItem;