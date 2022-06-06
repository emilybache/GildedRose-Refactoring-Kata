import { useTable } from "react-table";
import styled from "styled-components";
import { TTable } from "../types";

function ItemTable({ columns, data }: TTable): JSX.Element {
    const ItemsTable = styled.table`
  @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;500&family=Ubuntu:wght@300;400;500&display=swap');
    height: 60%;
    border: 1px solid RGB(12, 16, 36);
    border-radius: 10px;
    min-height: 30%;
    color: RGB(12, 16, 36);
th, td, tr {
    font-family: 'Roboto', sans-serif;
    outline: none;
    border: none;
    border-collapse: collapse;
    text-align: center;
    font-size: 1.2rem;
  }


  th {
    font-weight: 300;
  }

  tr {
    font-weight: 100;
  }

  tr:nth-child(n):hover {
    background-color: #f2f2f2;
  }

  tr:nth-child(2n + 1) {
    background-color: RGB(245, 245, 243);
  }
`;
    const {
        getTableProps,
        getTableBodyProps,
        headerGroups,
        rows,
        prepareRow,
    } = useTable({
        columns,
        data,
    });

    return (
        <ItemsTable {...getTableProps()}>
   
          <thead className="header">
   
            {headerGroups.map(headerGroup => (
   
              <tr>
   
                {headerGroup.headers.map(column => (
   
                  <th {...column.getHeaderProps()}>
                    {column.render('Header')}
                    </th>
   
                ))}
   
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
   
                      <td
   
                        {...cell.getCellProps()}

   
                      >
   
                        {cell.render('Cell')}
   
                      </td>
   
                    )
   
                  })}
   
                </tr>
   
              )
   
            })}
   
          </tbody>
   
        </ItemsTable>
      )
}

export default ItemTable;