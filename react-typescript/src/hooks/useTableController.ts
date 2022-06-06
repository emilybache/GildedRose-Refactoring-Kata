import { useMemo } from "react";
import { useStore } from "../model";
import { TTable } from "../types";

function useTableController(): TTable {
    const { state } = useStore();
    const { items } = state;

    const data = useMemo(() => items, [items]);
    const columns = useMemo(() => [
        {
            Header: "Items",
            columns: [
                {
                    Header: 'Name',
                    accessor: 'name',
                },
                {
                    Header: 'Conjured',
                    accessor: 'isConjured',
                    Cell: ({ cell: { value } }: any) => { return value ? 'Yes' : 'No' },
                },
                {
                    Header: 'Quality',
                    accessor: 'quality'
                },
                {
                    Header: 'Days Left',
                    accessor: 'sellIn'
                }
            ],
        },
    ], []);

    return {
        columns,
        data
    }
}

export default useTableController;