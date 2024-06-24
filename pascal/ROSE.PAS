unit Rose;

interface

type
    Item = record { 260b memory }
        Name: string;
        SellIn: Integer;
        Quality: Integer;
    end;

    Items = array [0..251] of Item; { 64kb memory }

    ListOfItems = record
        Elements: ^Items;
        Length: Word;
    end;

procedure ResizeList(var List: ListOfItems; Size: Word);

procedure ClearList(var List: ListOfItems);

procedure InitItem(var Item: Item; Name: string; SellIn: Integer; Quality: Integer);

function StrItem(Item: Item): string;

procedure UpdateQuality(Items: ListOfItems);

implementation

procedure ResizeList(var List: ListOfItems; Size: Word);
begin
    List.Length := Size;
    GetMem(List.Elements, Size * SizeOf(Item));
end;

procedure ClearList(var List: ListOfItems);
begin
    FreeMem(List.Elements, List.Length * SizeOf(Item));
    List.Length := 0;
end;

procedure InitItem(var Item: Item; Name: string; SellIn: Integer; Quality: Integer);
begin
    Item.Name := Name;
    Item.SellIn := SellIn;
    Item.Quality := Quality;
end;

function StrItem(Item: Item): string;
var SellInStr: string;
    QualityStr: string;
begin
    Str(Item.SellIn, SellInStr);
    Str(Item.Quality, QualityStr);
    StrItem := Item.Name + ', ' + SellInStr + ', ' + QualityStr;
end;

procedure UpdateQuality(Items: ListOfItems);
var I: Word;
begin
    for I := 0 to Items.Length-1 do
    begin
        if (Items.Elements^[I].Name <> 'Aged Brie') and
           (Items.Elements^[I].Name <> 'Backstage passes to a TAFKAL80ETC concert') then
        begin
            if Items.Elements^[I].Quality > 0 then
            begin
                if Items.Elements^[I].Name <> 'Sulfuras, Hand of Ragnaros' then
                begin
                    Items.Elements^[I].Quality := Items.Elements^[I].Quality - 1;
                end;
            end;
        end
        else
        begin
            if Items.Elements^[I].Quality < 50 then
            begin
                Items.Elements^[I].Quality := Items.Elements^[I].Quality + 1;

                if Items.Elements^[I].Name = 'Backstage passes to a TAFKAL80ETC concert' then
                begin
                    if Items.Elements^[I].SellIn < 11 then
                    begin
                        if Items.Elements^[I].Quality < 50 then
                        begin
                            Items.Elements^[I].Quality := Items.Elements^[I].Quality + 1;
                        end;
                    end;

                    if Items.Elements^[I].SellIn < 6 then
                    begin
                        if Items.Elements^[I].Quality < 50 then
                        begin
                            Items.Elements^[I].Quality := Items.Elements^[I].Quality + 1;
                        end;
                    end;
                end;
            end;
        end;

        if Items.Elements^[I].Name <> 'Sulfuras, Hand of Ragnaros' then
        begin
            Items.Elements^[I].SellIn := Items.Elements^[I].SellIn - 1;
        end;

        if Items.Elements^[I].SellIn < 0 then
        begin
            if Items.Elements^[I].Name <> 'Aged Brie' then
            begin
                if Items.Elements^[I].Name <> 'Backstage passes to a TAFKAL80ETC concert' then
                begin
                    if Items.Elements^[I].Quality > 0 then
                    begin
                        if Items.Elements^[I].Name <> 'Sulfuras, Hand of Ragnaros' then
                        begin
                            Items.Elements^[I].Quality := Items.Elements^[I].Quality - 1;
                        end;
                    end;
                end
                else
                begin
                    Items.Elements^[I].Quality := Items.Elements^[I].Quality - Items.Elements^[I].Quality;
                end;
            end
            else
            begin
                if Items.Elements^[I].Quality < 50 then
                begin
                    Items.Elements^[I].Quality := Items.Elements^[I].Quality + 1;
                end;
            end;
        end;
    end;
end;

end.
