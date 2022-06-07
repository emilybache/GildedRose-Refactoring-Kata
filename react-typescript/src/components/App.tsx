import { createGlobalStyle } from 'styled-components';
import ItemsSection from './ItemsSection';
import Header from './Header';

function App(): JSX.Element {

  const GlobalStyle = createGlobalStyle`
  * {
    margin: 0;
    padding: 0;
  }
  html, body {
    @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;500&family=Ubuntu:wght@300;400;500;700&display=swap');
    font-family: 'Roboto', sans-serif;
    background-color: RGB(255, 255, 253);
    color: RGB(227, 226, 215);
    width: 100%;
    height: 100%;
  }
  body {
    text-align: center;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }
`
  return (
    <section>
      <GlobalStyle />
      <Header />
      <ItemsSection />
    </section>
  );
}

export default App;
