import { useContext } from "react";
import StoreContext from "./storeContext";

export default function useStore() {
    return useContext(StoreContext);
}