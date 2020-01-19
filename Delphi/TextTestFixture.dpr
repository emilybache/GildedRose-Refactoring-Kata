program TextTestFixture;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  GildedRose in 'GildedRose.pas',
  Item in 'Item.pas';

var
  Days, ErrorCode, I, J: Integer;
  Items: TObjectList<TItem>;
  App: TGildedRose;

begin
  try
    Writeln('OMGHAI!');

    Items := TObjectList<TItem>.Create;
    Items.Add(TItem.Create('+5 Dexterity Vest', 10, 20));
    Items.Add(TItem.Create('Aged Brie', 2, 0));
    Items.Add(TItem.Create('Elixir of the Mongoose', 5, 7));
    Items.Add(TItem.Create('Sulfuras, Hand of Ragnaros', 0, 80));
    Items.Add(TItem.Create('Sulfuras, Hand of Ragnaros', -1, 80));
    Items.Add(TItem.Create('Backstage passes to a TAFKAL80ETC concert', 15, 20));
    Items.Add(TItem.Create('Backstage passes to a TAFKAL80ETC concert', 10, 49));
    Items.Add(TItem.Create('Backstage passes to a TAFKAL80ETC concert', 5, 49));
    // this conjured item does not work properly yet
    Items.Add(TItem.Create('Conjured Mana Cake', 3, 6));

    App := TGildedRose.Create(Items);

    Days := 2;
    if ParamCount > 0 then
    begin
      Val(ParamStr(1), Days, ErrorCode);
      Inc(Days);
    end;

    for I := 0 to Days - 1 do
    begin
      Writeln(Format('-------- day %d --------', [I]));
      Writeln('name, sellIn, quality');
      for J := 0 to Items.Count - 1 do
        Writeln(Items[J].ToString);
      Writeln;

      App.UpdateQuality;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
