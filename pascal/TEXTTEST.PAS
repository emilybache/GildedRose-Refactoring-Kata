program TextTests;

uses Rose;

var Items: ListOfItems;
    Last: Word;
    Days, Day: Integer;
    ErrorCode: Integer;
    DayStr: string;
    I: Word;
begin
    WriteLn('OMGHAI!');

    ResizeList(Items, 9);

    Last := 0;
    InitItem(Items.Elements^[Last], '+5 Dexterity Vest', 10, 20);
    Inc(Last);
    InitItem(Items.Elements^[Last], 'Aged Brie', 2, 0);
    Inc(Last);
    InitItem(Items.Elements^[Last], 'Elixir of the Mongoose', 5, 7);
    Inc(Last);
    InitItem(Items.Elements^[Last], 'Sulfuras, Hand of Ragnaros', 0, 80);
    Inc(Last);
    InitItem(Items.Elements^[Last], 'Sulfuras, Hand of Ragnaros', -1, 80);
    Inc(Last);
    InitItem(Items.Elements^[Last], 'Backstage passes to a TAFKAL80ETC concert', 15, 20);
    Inc(Last);
    InitItem(Items.Elements^[Last], 'Backstage passes to a TAFKAL80ETC concert', 10, 49);
    Inc(Last);
    InitItem(Items.Elements^[Last], 'Backstage passes to a TAFKAL80ETC concert', 5, 49);
    Inc(Last);
    { this Conjured item doesn't yet work properly }
    InitItem(Items.Elements^[Last], 'Conjured Mana Cake', 3, 6);
    Inc(Last);
    Items.Length := Last;

    Days := 2;
    if ParamCount > 0 then
    begin
        Val(ParamStr(1), Days, ErrorCode);
        Inc(Days);
    end;

    for Day := 0 to Days-1 do
    begin
        Str(Day, DayStr);
        WriteLn('-------- day ' + DayStr + ' --------');
        WriteLn('name, sellIn, quality');
        for I := 0 to Items.Length-1 do
        begin
            WriteLn(StrItem(Items.Elements^[I]));
        end;
        WriteLn('');

        UpdateQuality(Items);
    end;

    ClearList(Items);
end.
