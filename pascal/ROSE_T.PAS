{F+} { need to set Far Calls in Compiler Options too }
program Rose_T;

uses TPUnit, Rose;

var Items: ListOfItems;

procedure CreateItem;
begin
    ResizeList(Items, 1);
end;

procedure DisposeItem;
begin
    ClearList(Items);
end;

procedure Foo;
begin
    InitItem(Items.Elements^[0], 'foo', 0, 0);

    UpdateQuality(Items);

    AssertEqualsStr('name', 'fixme', Items.Elements^[0].Name);
end;

begin
    RunFixtures('foo', CreateItem, Foo, DisposeItem);
end.
