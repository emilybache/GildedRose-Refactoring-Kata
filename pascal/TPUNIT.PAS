{F+} { need to set Far Calls in Compiler Options too }
(* ------------------------------------------------------------------ *)
(* Minimalist xUnit implementation for Turbo Pascal in TP style.      *)
(* Version: 2.01                                                      *)
(* Language: Turbo Pascal 6.01                                        *)
(* Copyright: (c) 2010 Peter Kofler, www.code-cop.org                 *)
(* License: BSD, http://www.opensource.org/licenses/bsd-license.php   *)
(* ------------------------------------------------------------------ *)
unit TPUnit;

interface

{
  uses TPUnit;

  Tests are added as methods without arguments to the test
  program as usual and use asserts provided by the unit.
  The first failed assertion stops program execution.

  procedure TestAddition;
  begin
      AssertEquals('use asserts in tests', 2, 1 + 1);
  end;

  Due to the lack of introspection each test has to
  be called manually in the main body.

  begin
      RunTest('TestAddition', TestAddition);
  end.
}

type
    TestMethod = procedure;

{ Asserts }
procedure AssertEquals(Message: string; Expected, Actual: Longint);
procedure AssertEqualsStr(Message: string; Expected, Actual: string);
procedure AssertNotNil(Message: string; Actual: Pointer);
procedure AssertNil(Message: string; Actual: Pointer);
procedure AssertTrue(Message: string; Actual: Boolean);
procedure AssertFalse(Message: string; Actual: Boolean);
procedure Fail(Message: string);

{ Test Runner }
procedure RunTest(Name: string; Test: TestMethod);
procedure RunFixtures(Name: string; SetUp, Test, TearDown: TestMethod);
procedure Empty;

implementation

uses Crt;

procedure AssertEquals(Message: string; Expected, Actual: Longint);
var ExpectedStr, ActualStr: string;
begin
    if Expected <> Actual then
    begin
        Str(Expected, ExpectedStr);
        Str(Actual, ActualStr);
        Fail(Concat(Message, ' Expected ', ExpectedStr, ' but was ', ActualStr));
    end;
end;

procedure AssertEqualsStr(Message: string; Expected, Actual: string);
begin
    if Expected <> Actual then
    begin
        Fail(Concat(Message, ' Expected ', Expected, ' but was ', Actual));
    end;
end;

procedure AssertNotNil(Message: string; Actual: Pointer);
begin
    AssertFalse(Message, Actual = nil);
end;

procedure AssertNil(Message: string; Actual: Pointer);
begin
    AssertTrue(Message, Actual = nil);
end;

procedure AssertTrue(Message: string; Actual: Boolean);
begin
    if not Actual then
    begin
        Fail(Message);
    end;
end;

procedure AssertFalse(Message: string; Actual: Boolean);
begin
    AssertTrue(Message, not Actual);
end;

procedure Fail(Message: string);
begin
    TextColor(Red);
    WriteLn(' - FAILED');
    NormVideo;
    WriteLn(Message);

    Halt(1);
end;

procedure Empty;
begin
end;

procedure RunTest(Name: string; Test: TestMethod);
begin
    RunFixtures(Name, Empty, Test, Empty);
end;

procedure RunFixtures(Name: string; SetUp, Test, TearDown: TestMethod);
begin
    Write('TEST ', Name);
    SetUp;
    Test;
    TearDown;

    TextColor(Green);
    WriteLn(' - OK');
    NormVideo;
end;

begin
    Crt.ClrScr;
end.
