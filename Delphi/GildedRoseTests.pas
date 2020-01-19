unit GildedRoseTests;

interface
uses
  DUnitX.TestFramework,
  GildedRose,
  Item,
  System.Generics.Collections;

type
  [TestFixture]
  TGildedRoseTests = class(TObject)
  public
    [Test]
    procedure UpdateQuality_Never_ChangesTheItemName;
  end;

implementation

procedure TGildedRoseTests.UpdateQuality_Never_ChangesTheItemName;
var
  LItems: TObjectList<TItem>;
  LGildedRose: TGildedRose;
begin
  LItems := TObjectList<TItem>.Create;
  LItems.Add(TItem.Create('foo', 0, 0));
  LGildedRose := TGildedRose.Create(LItems);

  LGildedRose.UpdateQuality;

  Assert.AreEqual('fixme', LGildedRose.Items[0].Name);
end;

initialization
  TDUnitX.RegisterTestFixture(TGildedRoseTests);
end.
