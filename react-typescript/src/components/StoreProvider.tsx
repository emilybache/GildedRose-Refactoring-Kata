import { useReducer } from "react";
import initialState from "../model/initialState";
import reducer from "../model/reducer";
import StoreContext from "../model/storeContext";

type TProps = {
    children: JSX.Element;
}

function StoreProvider({ children }: TProps): JSX.Element {
  const [state, dispatch] = useReducer(reducer, initialState);

  return (
    <StoreContext.Provider value={{ state, dispatch }}>
      { children }
    </StoreContext.Provider>
  );
}

export default StoreProvider;