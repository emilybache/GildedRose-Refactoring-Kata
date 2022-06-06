import { createGlobalStyle } from 'styled-components';
import ItemsSection from './ItemsSection';

function App(): JSX.Element {

  const GlobalStyle = createGlobalStyle`
  * {
    margin: 0;
    padding: 0;
  }
  html, body {
    background-color: RGB(255, 255, 253);
    color: RGB(227, 226, 215);
    width: 100%;
    height: 100%;
  }
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen",
        "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue",
        sans-serif;
    text-align: center;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }
`
  return (
    <section>
      <GlobalStyle />
      {/* <Header /> */}
      <ItemsSection />
    </section>
  );
}

export default App;
