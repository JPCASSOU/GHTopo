// Ensemble de polypolylignes
unit UnitPolyPolyLine;
interface
{$INCLUDE CompilationParameters.inc}
uses
  StructuresDonnees,
  Common,
  Sysutils, Classes,
  UnitListesSimplesWithGeneriques;

type

{ TPolyPolyLigne }

 TPolyPolyLigne = class(TList)
  private
    FName: string;
    FCurrentPolyline: TListePoints3Df;
    procedure ClearListe();

  public
    function  Initialiser(const PolyName: string): boolean;
    procedure Finaliser();
    function GetName(): string;

    function  GetNbPolylines(): integer;
    function BeginPolyline(): boolean;
    procedure AddVertex(const QX, QY, QZ: double);
    function EndPolyline(): boolean;
    function  GetPolyline(const Idx: integer): TListePoints3Df;
    procedure RemovePolyline(const Idx: integer);

end;

implementation
uses
  DGCDummyUnit;

{ TPolyPolyLigne }

procedure TPolyPolyLigne.ClearListe();
begin
  while (self.count > 0) do RemovePolyline(0);
  self.Clear;
end;

function TPolyPolyLigne.Initialiser(const PolyName: string): boolean;
begin
  result := false;
  FCurrentPolyline := nil;
  FName  := PolyName;
  self.Clear;
end;

procedure TPolyPolyLigne.Finaliser();
begin
  try
    self.ClearListe();
  finally
    pass;
  end;
end;

function TPolyPolyLigne.GetName(): string;
begin
  result := FName;
end;

function TPolyPolyLigne.GetNbPolylines(): integer;
begin
  result := self.Count;
end;

function TPolyPolyLigne.BeginPolyline(): boolean;
var
  P: ^TListePoints3Df;
begin
  New(P);
  P^ := TListePoints3Df.Create;
  P^.ClearListe();
  self.Add(P);
  FCurrentPolyline := GetPolyline(GetNbPolylines() - 1);
end;

procedure TPolyPolyLigne.AddVertex(const QX, QY, QZ: double);
var V: TPoint3Df;
begin
  V.X := QX;
  V.Y := QY;
  V.Z := QZ;
  FCurrentPolyline.AddElement(V);
end;

function TPolyPolyLigne.EndPolyline(): boolean;
begin
  pass;
end;
function TPolyPolyLigne.GetPolyline(const Idx: integer): TListePoints3Df;
var
  P: ^TListePoints3Df;
begin
  P := self.Items[Idx];
  result := P^;
end;

procedure TPolyPolyLigne.RemovePolyline(const Idx: integer);
var
  P: ^TListePoints3Df;
begin
  P := self.Items[Idx];
  P^.ClearListe();
  Dispose(P);
  self.Delete(Idx);
end;

end.
          THairItem = class(TCollectionItem)
  private
    FLength: integer;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property Length: integer read FLength write FLength;
  end;

  { THairList }

  THairList = class(TCollection)
  private
    function GetItems(Index: integer): THairItem;
    procedure SetItems(Index: integer; AValue: THairItem);
  public
    constructor Create;
  public
    function Add: THairItem;
    function AddEx(length: integer): THairItem;
    property Items[Index: integer]: THairItem read GetItems write SetItems; default;
  end;

var
  hairs: THairList;

implementation

{ THairItem }

constructor THairItem.Create(ACollection: TCollection);
begin
  if Assigned(ACollection) {and (ACollection is THairList)} then
    inherited Create(ACollection);
end;

{ THairList }

function THairList.GetItems(Index: integer): THairItem;
begin
  Result := THairItem(inherited Items[Index]);
end;

procedure THairList.SetItems(Index: integer; AValue: THairItem);
begin
  Items[Index].Assign(AValue);
end;

constructor THairList.Create;
begin
  inherited Create(THairItem);
end;

function THairList.Add: THairItem;
begin
  Result := inherited Add as THairItem;
end;

function THairList.AddEx(length: integer): THairItem;
begin
  Result := inherited Add as THairItem;
  Result.Length := length;
end;
