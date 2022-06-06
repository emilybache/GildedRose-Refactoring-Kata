import styled from "styled-components";

const ItemsStyle = styled.div`
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

export default ItemsStyle;

