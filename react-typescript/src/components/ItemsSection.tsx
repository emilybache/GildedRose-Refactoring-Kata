import { useMemo } from "react";
import styled from "styled-components";
import useAllItems from "../hooks/useAllItems";
import ItemsTable from './ItemsTable';
import Button from './Button';

function ItemsSection(): JSX.Element {
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

const { items } = useAllItems();
const data = useMemo(() => items, [items]);
const columns = useMemo(() => [
  {
    Header: "Details",
    columns: [
      {
        Header: 'Name',
        accessor: 'name',
      },
      {
        Header: 'Conjured',
        accessor: 'isConjured'
      }
    ],
  },
  {
    Header: "Item Info",
    columns: [
      {
        Header: 'Quality',
        accessor: 'quality'
      },
      {
        Header: 'Days Left',
        accessor: 'sellIn'
      }
    ],
  }
], []);

  return (
    <ItemsSection>
      <ItemsTable columns={columns} data={data}/>
      <Button />
    </ItemsSection>
  );
}

export default ItemsSection;