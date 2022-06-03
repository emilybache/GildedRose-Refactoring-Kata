import './App.css';
import Item from './Item';

function App() {
  return (
    <div className="App">
      <Item name="+5 Dexterity Vest" sellIn={10} quality={20} isConjured={false} />
      <Item name="Aged Brie" sellIn={2} quality={0} isConjured={false} />
      <Item name="Elixir of the Mongoose" sellIn={5} quality={7} isConjured={false} />
      <Item name="Sulfuras, Hand of Ragnaros" sellIn={1} quality={80} isConjured={false} />
      <Item name="Backstage passes to a TAFKAL80ETC concert" sellIn={15} quality={20} isConjured={false} />
      <Item name="Conjured Mana Cake" sellIn={3} quality={6} isConjured={true} />
    </div>
  );
}

export default App;
