import { useMemo } from "react";
import { useStore } from "../model";
import { TTable } from "../types";
import Conjured from "../components/Conjured";
import Quality from "../components/Quality";
import SellIn from "../components/SellIn";

function useTableController(): TTable {
    const { state } = useStore();
    const { items } = state;

    const data = useMemo(() => items, [items]);
    const columns = useMemo(() => [
                {
                    Header: 'Name',
                    accessor: 'name',

                },
                {
                    Header: 'Conjured',
                    accessor: 'isConjured',
                    Cell: ({ cell: { value }, index}: any) => (
                        {
                            ...Conjured(value, index),
                        }
                    ),
                },
                {
                    Header: 'Quality',
                    accessor: 'quality',
                    Cell: ({ cell: { value }, index}: any) => (
                        {
                            ...Quality(value, index),
                        }
                    ),
                },
                {
                    Header: 'Remaining Days',
                    accessor: 'sellIn',
                    Cell: ({ cell: { value }, index}: any) => (
                        {
                            ...SellIn(value, index),
                        }
                    ),
                },
    ], []);

    return {
        columns,
        data
    }
}

export default useTableController;