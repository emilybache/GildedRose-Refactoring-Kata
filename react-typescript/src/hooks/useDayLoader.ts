import { useStore } from "../model";
import {EActionTypes} from "../types";
 
function useDayLoader() {
  const { dispatch } = useStore();
  const loadNextDay = () => {
    dispatch({ type: EActionTypes.NEXT_DAY });
  };
  return { loadNextDay };
}

export default useDayLoader;