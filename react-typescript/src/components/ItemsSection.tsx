import styled from "styled-components";
import ItemsTable from './ItemsTable';
import Button from './Button';
import useTableController from "../hooks/useTableController";

function ItemsSection(): JSX.Element {
  const ItemsSection = styled.section`
  height: 100vh;
  display: flex;
  flex-flow: column wrap;
  justify-content: center;
  margin: 0 5%;
  role: 'region';
`;

const { columns, data } = useTableController();

  return (
    <ItemsSection>
      <ItemsTable columns={columns} data={data} />
      <Button />
    </ItemsSection>
  );
}

export default ItemsSection;