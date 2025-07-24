const getUpdateBehaviorMock = jest.fn((item: Item) => new MockBehavior(item));
const updateMock = jest.fn((item: Item) => item);

jest.mock("@app/update-behaviors", () => ({
  getUpdateBehaviorFor: getUpdateBehaviorMock,
}));

import { GildedRose } from "@app/gilded-rose";
import { Item } from "@app/item";
import { IUpdateBehavior } from "@app/update-behaviors";

export class MockBehavior implements IUpdateBehavior {
  constructor(public item: Item) {}
  update() {
    return updateMock(this.item);
  }
}

describe("Gilded Rose", () => {
  it("should have an empty array as items when no constructor parameter is provided", () => {
    const gildedRose = new GildedRose();
    expect(gildedRose.items).toEqual([]);
  });

  it("should call the behavior resolver and update function for each item", () => {
    const item1 = new Item("item 1", 0, 0);
    const item2 = new Item("item 2", 0, 0);
    const gildedRose = new GildedRose([item1, item2]);

    gildedRose.updateQuality();

    expect(getUpdateBehaviorMock).toHaveBeenCalledWith(item1);
    expect(getUpdateBehaviorMock).toHaveBeenCalledWith(item2);

    expect(updateMock).toHaveBeenCalledWith(item1);
    expect(updateMock).toHaveBeenCalledWith(item2);
  });

  // to implement: "Conjured" items degrade in Quality twice as fast as normal items
});
