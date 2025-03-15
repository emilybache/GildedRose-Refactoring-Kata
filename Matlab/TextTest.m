function TextTest(nDays)

if nargin < 1
    nDays = 2;
end

items = [
    Item("+5 Dexterity Vest", 10, 20);
    Item("Aged Brie", 2, 0);
    Item("Elixir of the Mongoose", 5, 7);
    Item("Sulfuras, Hand of Ragnaros", 0, 80);
    Item("Sulfuras, Hand of Ragnaros", -1, 80);
    Item("Backstage passes to a TAFKAL80ETC concert", 15, 20);
    Item("Backstage passes to a TAFKAL80ETC concert", 10, 49);
    Item("Backstage passes to a TAFKAL80ETC concert", 5, 49);
    Item("Conjured Mana Cake", 3, 6)];


for day = 1:nDays
    fprintf("-------- day %d --------\n", day)
    disp("name, sellIn, quality")
    for i = 1:length(items)
        disp(items(i))
    end
    disp(" ")
    GildedRose(items).update_quality()
end

end


