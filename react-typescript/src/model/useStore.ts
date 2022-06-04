import { useReducer } from "react";
import initialState from "./initialState";
import reducer from "./reducer";

export default function useStore() {
    const [state, dispatch] = useReducer(reducer, initialState.items);

    return {
        state,
        dispatch
    }
}