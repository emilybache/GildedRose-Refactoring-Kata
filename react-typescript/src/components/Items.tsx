import useAllItems from "../hooks/useAllItems";
import { TItem } from "../types"

function Items() {
  const { items } = useAllItems();
  return (
    <div className="Item">
      <ul>
        {items.map((item: TItem) => {
          return (
          <li key={item.name}>
            Name: {item.name} Quality: {item.quality} SellIn: {item.sellIn}
          </li>
          );
        })}
      </ul>
    </div>
  );
};

export default Items;