open GildedRose

Js.log("OMGHAI!")

let items: ref<array<Item.t>> = ref([
  Item.make(~name="+5 Dexterity Vest", ~sellIn=10, ~quality=20),
  Item.make(~name="Aged Brie", ~sellIn=2, ~quality=0),
  Item.make(~name="Elixir of the Mongoose", ~sellIn=5, ~quality=7),
  Item.make(~name="Sulfuras, Hand of Ragnaros", ~sellIn=0, ~quality=80),
  Item.make(~name="Sulfuras, Hand of Ragnaros", ~sellIn=-1, ~quality=80),
  Item.make(~name="Backstage passes to a TAFKAL80ETC concert", ~sellIn=15, ~quality=20),
  Item.make(~name="Backstage passes to a TAFKAL80ETC concert", ~sellIn=10, ~quality=49),
  Item.make(~name="Backstage passes to a TAFKAL80ETC concert", ~sellIn=5, ~quality=49),
  Item.make(~name="Conjured Mana Cake", ~sellIn=3, ~quality=6),
])

let days = Node.Process.argv->Belt.Array.get(2)->Belt.Option.mapWithDefault(31, int_of_string)

for i in 0 to days {
  Js.log("-------- day " ++ string_of_int(i) ++ " --------")
  Js.log("name, sellIn, quality")
  for j in 0 to Js.Array2.length(items.contents) - 1 {
    let item = items.contents[j]
    Js.log(item.name ++ ", " ++ string_of_int(item.sellIn) ++ ", " ++ string_of_int(item.quality))
  }
  Js.log("")
  items := updateQuality(items.contents)
}
