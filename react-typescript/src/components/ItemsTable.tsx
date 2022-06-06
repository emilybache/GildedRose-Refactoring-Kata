import { useTable } from "react-table";
import styled from "styled-components";
import { TTable } from "../types";

function ItemTable({ columns, data }: TTable): JSX.Element {
    const ItemsSection = styled.section`
  height: 85vh;
  ul {
    display: flex;
    flex-direction: row;
    flex-wrap: wrap;
    width: 100%;
    height: 100%;
    color: white;
    justify-content: center;
    align-items: center;
  }
  li {
    list-style: none;
    display: inline-block;
    height: 10%;
    width: 40%;
    border: 1px solid white;
    margin: 3% 3%;
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

        <table {...getTableProps()} style={{ border: 'solid 1px blue' }}>
   
          <thead>
   
            {headerGroups.map(headerGroup => (
   
              <tr>
   
                {headerGroup.headers.map(column => (
   
                  <th
   
                    {...column.getHeaderProps()}
   
                    style={{
   
                      borderBottom: 'solid 3px red',
   
                      background: 'aliceblue',
   
                      color: 'black',
   
                      fontWeight: 'bold',
   
                    }}
   
                  >
   
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
   
                        style={{
   
                          padding: '10px',
   
                          border: 'solid 1px gray',
   
                          background: 'papayawhip',
   
                        }}
   
                      >
   
                        {cell.render('Cell')}
   
                      </td>
   
                    )
   
                  })}
   
                </tr>
   
              )
   
            })}
   
          </tbody>
   
        </table>
   
      )
}

export default ItemTable;