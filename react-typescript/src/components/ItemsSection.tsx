import styled from "styled-components";
import ItemsTable from './ItemsTable';
import useTableController from "../hooks/useTableController";
import TableHeader from "./TableHeader";

function ItemsSection(): JSX.Element {
  const ItemsSection = styled.section`
  display: flex;
  height: 80vh;
  flex-flow: column wrap;
  justify-content: flex-start;
  margin: 0 5%;
  border: 1px solid RGB(12, 16, 36);
  border-radius: 10px;
  role: 'region';
  text-align: left;
`;

const { columns, data } = useTableController();

  return (
    <ItemsSection>
      <TableHeader />
      <ItemsTable columns={columns} data={data} />
    </ItemsSection>
  );
}

export default ItemsSection;