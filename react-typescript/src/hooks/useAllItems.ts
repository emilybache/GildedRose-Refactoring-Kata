import { useStore } from "../model";
 
function useAllItems() {
  const { state } = useStore();
  return { items: state.items };
}

export default useAllItems;