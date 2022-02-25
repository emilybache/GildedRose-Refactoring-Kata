import Item from "./../models/Item";


export default interface IUpdateStrategy {
    updateItem(item: Item) : Item;
}
