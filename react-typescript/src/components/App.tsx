import useDayLoader from '../hooks/useDayLoader';
import './App.css';
import Items from './Items';

function App() {
  const { loadNextDay } = useDayLoader();
  return (
    <div className="App">
      <Items />
      <button onClick={() => loadNextDay()}></button>
    </div>
  );
}

export default App;
