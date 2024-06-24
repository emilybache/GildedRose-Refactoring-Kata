unit Item;

interface

type
  TItem = class(TObject)
  private
    FName: string;
    FSellIn: Integer;
    FQuality: Integer;
  public
    constructor Create(const AName: string; const ASellIn, AQuality: Integer);
    function ToString: string; override;
    property Name: string read FName write FName;
    property SellIn: Integer read FSellIn write FSellIn;
    property Quality: Integer read FQuality write FQuality;
  end;

implementation

uses
  System.SysUtils;

{ TItem }

constructor TItem.Create(const AName: string; const ASellIn, AQuality: Integer);
begin
  inherited Create;
  FName := AName;
  FSellIn := ASellIn;
  FQuality := AQuality;
end;

function TItem.ToString: string;
begin
  Result := Format('%s, %d, %d', [Name, SellIn, Quality]);
end;

end.
