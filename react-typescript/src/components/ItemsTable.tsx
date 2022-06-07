import { usePagination, useTable } from "react-table";
import styled from "styled-components";
import { TTable } from "../types";

const ItemsTable = styled.section`
  height: 80%;
    color: RGB(12, 16, 36);
    table {
        width: 100%;
        height: 100%;
    }
th, td, tr, thead,  {
    outline: none;
    border: none;
    border-collapse: collapse;
    text-align: left;
    font-size: 1.2rem;
  }
  p {
    padding: .5rem;
  }

  .button {
      align-self: center;
  }


  .data {
      height: 10%;
      font-weight: 300;
      font-size: 1.1rem;
  }
  th {
    background-color: RGB(245, 245, 243);
    font-size: .9rem;
    font-weight: 800;
    height: 5%;
  }

  td {
      height: 100%;
  }

  tr {
    font-weight: 100;
  }

  tr:nth-child(2n) {
    background-color: RGB(245, 245, 243);
  }
`;

function ItemTable({ columns, data }: TTable): JSX.Element {
    
    const {
        getTableProps,
        getTableBodyProps,
        headerGroups,
        rows,
        prepareRow,
    } = useTable({
        columns,
        data,
    }, usePagination);

    return (
        <ItemsTable {...getTableProps()}>
            <table>
            <thead className="header">
                {headerGroups.map(headerGroup => (
                    <tr>
                        {
                        headerGroup.headers.map(column => (
                            <th {...column.getHeaderProps()}>
                                <p>{column.render('Header')}</p>
                            </th>
                        ))
                        }
                    </tr>
                ))}
            </thead>

            <tbody {...getTableBodyProps()}>
                {rows.map(row => {
                    prepareRow(row)
                    return (
                        <tr {...row.getRowProps()}>
                            {row.cells.map(cell => {
                                return (
                                    <td {...cell.getCellProps()} className="data">
                                        <p>{cell.render('Cell')}</p>
                                    </td>
                                )
                            })}
                        </tr>
                    )
                })}
            </tbody>
            </table>
        </ItemsTable>
    )
}

export default ItemTable;