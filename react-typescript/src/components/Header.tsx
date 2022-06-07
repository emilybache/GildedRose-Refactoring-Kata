import styled from 'styled-components';

function Header(): JSX.Element {
    const Nav = styled.nav`
    display: flex;
    flex-flow: column wrap;
    justify-content: center;
    font-size: 1rem;
    align-items: center;
    height: 10vh;
    border-bottom: 1px solid RGB(12, 16, 36);
    margin-bottom: 2rem;
    span {
        font-size: 1.5rem;
        font-weight: 700;
        font-family: 'Ubuntu', sans-serif;
        color: RGB(12, 16, 36);
    }
    img {
       height: 2.5rem;
         width: 2.5rem; 
    }
    `
  return (
    <Nav>
        <span>
            <img src='https://i.postimg.cc/52nKHG6q/seeding.png' alt='Gilded Rose Logo'></img>
            <p>Gilded Rose</p>
        </span>
    </Nav>
  );
}

export default Header;
