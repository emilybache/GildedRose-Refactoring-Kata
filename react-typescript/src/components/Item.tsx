import useItem from "../hooks/useItem";
import Item from "../types"

function Items() {
  const { items, updateItem } = useItem();
  return (
    <div className="Item">
      <ul>
        {items.map((item: Item) => {
          return (
          <li key={item.name}>
            Name: {item.name} Quality: {item.quality} SellIn: {item.sellIn} <button onClick={() => updateItem(item)}></button> 
          </li>
          );
        })}
      </ul>
    </div>
  );
};

export default Items;