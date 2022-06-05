import { createContext } from "react";
import { TContext } from "../types";
import initialState from "./initialState";

const StoreContext = createContext<TContext>({
    state: initialState,
    dispatch: () => {},
})

export default StoreContext;