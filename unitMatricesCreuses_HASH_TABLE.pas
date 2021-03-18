unit unitMatricesCreuses_HASH_TABLE;
// Stockage de matrices creuses sous forme de listes de tables de hachage
{$WARNING HASHTABLE storage method is very slow - Use the unit unitMatricesCreuses_LOT}
{$INCLUDE CompilationParameters.inc}

interface
// Tests effectués: OK mais extrêmement lent
uses
  Common,
  Classes, SysUtils, math,
  Graphics,
  contnrs
  ;

type TElementNonNulOfAnRow = record
  //IdxColumn  : Integer;
  Valeur     : double;
end;
// Liste des valeurs non-nulles d'une ligne
type

{ TListeNonZerosOfRow }

 TListeNonZerosOfRow = class(TFPHashList)
  strict private // indique que ce qui suit n'est pas visible à l'extérieur de la classe
    FMinIdxNonZero: integer;
    FMaxIdxNonZero: integer;
    function  FindValueByIndexCol(const Idx: integer;
                              out   BP: TElementNonNulOfAnRow;
                              out   InternalIdx: integer): boolean;

  public
    constructor Create(const NbCols: integer);
    property  MinIdxNonZero: integer read FMinIdxNonZero write FMinIdxNonZero; // Index de colonne du premier élément non nul
    property  MaxIdxNonZero: integer read FMaxIdxNonZero write FMaxIdxNonZero; // Index de colonne du deuxième élément non nul
                                    // Tri des veleurs par index de colonnes

    procedure SetValeur(const ACol: integer; const Value: double);
    function  GetValeur(const ACol: integer): double;

    procedure RemoveZeros();
    procedure ClearListe();
    function  GetNbElements(): integer; inline;
    //procedure SortValuesByColumns();
end;


type
{ TMatriceCreuse }

 TMatriceCreuse = class
   private
     FMatrixName: string;
     FMaxRow: integer;
     FMaxCol: integer;
     FListeLignes : array of TListeNonZerosOfRow;  // tableau 1D de listes, avec un item par ligne
     FLowIndexes  : array of Integer;              // tableau des premiers index de valeurs non nulles
     FHighIndexes : array of Integer;

   public
     function  Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
     procedure Finaliser();

     property  MaxRow: integer read FMaxRow;
     property  MaxCol: integer read FMaxCol;

     function  SetValeur(const I, J: integer; const V: double): boolean;
     function  GetValeur(const I, J: integer): double;
     // affichage de la matrice sous forme de texte
     procedure Lister();
     // affichage de la matrice sous forme d'image
     procedure CreerRepresentationMatriceEnImage(const QFileName: string);

     procedure RecenserLowIndexes();
     procedure RecenserHighIndexes();
     procedure RemoveAllZeroes();
end;

implementation



constructor TListeNonZerosOfRow.Create(const NbCols: integer);
begin
  self.FMinIdxNonZero  := 1 + NbCols;
  self.FMaxIdxNonZero  := -1;
  self.Clear;
end;

function TListeNonZerosOfRow.FindValueByIndexCol(const Idx: integer;
                                                 out   BP: TElementNonNulOfAnRow;
                                                 out   InternalIdx: integer): boolean;
var
  pE: ^TElementNonNulOfAnRow;
begin
  result := false;
  pE := Find(Format(FORMAT_NB_INTEGER, [Idx]));
  if (pE <> nil) then
  begin
    BP := pE^;
    InternalIdx := self.IndexOf(pE);
    exit(True);
  end;
end;

procedure TListeNonZerosOfRow.SetValeur(const ACol: integer; const Value: double);
var
  pE: ^TElementNonNulOfAnRow;
  BP: TElementNonNulOfAnRow;
  QInternalIdx: integer;
begin
  if (not FindValueByIndexCol(ACol, BP, QInternalIdx)) then
  begin
    New(pE);
    pE^.Valeur := Value;
    self.Add(format(FORMAT_NB_INTEGER, [ACol]), pE);
  end
  else
  begin
  end;
end;

function TListeNonZerosOfRow.GetValeur(const ACol: integer): double;
var
  pE: ^TElementNonNulOfAnRow;
  QInternalIdx: Integer;
  BP: TElementNonNulOfAnRow;
begin
  result := 0.00;
  pE := Find(Format(FORMAT_NB_INTEGER, [ACol]));
  if (pE <> nil) then
  begin
    Result := pE^.Valeur;
    //QInternalIdx := self.IndexOf(pE);
    //pE := self.Get(QInternalIdx);
    //Result := pE^.Valeur;
  end;
end;

procedure TListeNonZerosOfRow.RemoveZeros();
const QEPSILON = 1E-5;
begin
  pass;
end;

procedure TListeNonZerosOfRow.ClearListe();
begin
  self.Clear;
end;

function TListeNonZerosOfRow.GetNbElements(): integer;
begin
  Result := self.Count;
end;


(*



procedure TListeNonZerosOfRow.SortValuesByColumns();
var
  EWE: TElementNonNulOfAnRow;
  n: Integer;
begin
  self.FMinIdxNonZero  := -1;
  self.FMaxIdxNonZero  := -1;
  n := self.GetNbElements();
  if (n = 0) then Exit;
  //self.PurgerLesZeros();
  self.Pack;
  self.Sort(SortRowValuesByIdxCol);
  EWE := self.GetElement(0);
  self.FMinIdxNonZero := EWE.IdxColumn;
  EWE := self.GetElement(n - 1);
  self.FMaxIdxNonZero := EWE.IdxColumn;
end;
//*)
//******************************************************************************
{ TMatriceCreuse }



procedure TMatriceCreuse.CreerRepresentationMatriceEnImage(const QFileName: string);
var
  BMP: TBitmap;
  i, j: Integer;
begin
  AfficherMessageErreur(Format('%s.CreerRepresentationMatriceEnImage(%s) (%dx%d)', [ClassName, QFileName, FMaxRow, FMaxCol]));
  //Exit;
  BMP := TBitmap.Create;
  try
    BMP.Width  := 1 + FMaxCol;
    BMP.Height := 1 + FMaxRow;
    BMP.Canvas.Brush.Color := clAqua;
    BMP.Canvas.Pen.Color   := clRed;
    BMP.Canvas.FillRect(0, 0, BMP.Width, BMP.Height);
    for i := 1 to FMaxRow do
      for j := 1 to FMaxCol do
        if (not IsZero(GetValeur(i, j))) then BMP.Canvas.Pixels[j, i] := clRed;
    BMP.SaveToFile(QFileName);
  finally
    FreeAndNil(BMP);//BMP.Free;
  end;
end;



function TMatriceCreuse.GetValeur(const I, J: integer): double;
var
  BP: TElementNonNulOfAnRow;
  QInternalIdx: integer;
begin
  Result := FListeLignes[i].GetValeur(j);
end;
function TMatriceCreuse.SetValeur(const I, J: integer; const V: double): boolean;
var
  MyRow: TListeNonZerosOfRow;
  BP   : TElementNonNulOfAnRow;
  QInternalIdx: integer;
begin
  Result := false;
  QInternalIdx := -1;
  if (abs(V) > 1E-8) then
  begin
    FListeLignes[i].SetValeur(j, V);
  end;
end;


function TMatriceCreuse.Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
var
  i: Integer;
begin
  result := false;
  FMatrixName := Name;
  AfficherMessageErreur(Format('%s.Initialiser: %s (%d, %d)', [ClassName, FMatrixName, NbRows, NbCols]));
  try
    SetLength(FListeLignes, 0);
    SetLength(FListeLignes, 1 + NbRows);
    SetLength(FLowIndexes, 0);
    SetLength(FLowIndexes, 1 + NbRows);
    SetLength(FHighIndexes, 0);
    SetLength(FHighIndexes, 1 + NbRows);
    FMaxRow := NbRows;
    FMaxCol := NbCols;
    FListeLignes[0] := TListeNonZerosOfRow.Create(NbCols);
    FListeLignes[0].ClearListe();
    FListeLignes[0].SetValeur(0, 0.00);
    for i := 1 to High(FListeLignes) do
    begin
      FListeLignes[i] := TListeNonZerosOfRow.Create(NbCols);
      FListeLignes[i].ClearListe();
      //FListeLignes[i].Capacity := 4000;
    end;
    Result := true;
  except
  end;
end;
procedure TMatriceCreuse.Finaliser;
var
  i: Integer;
begin
  AfficherMessageErreur(Format('%s.Finaliser(%s)', [ClassName, FMatrixName]));
  for i := 0 to High(FListeLignes) do
  begin
    try
      FListeLignes[i].ClearListe();
    finally
      FreeAndNil(FListeLignes[i]);//FListeLignes[i].Free;
    end;
  end;
  SetLength(FListeLignes, 0);
  SetLength(FLowIndexes, 0);
  SetLength(FHighIndexes, 0);
end;

procedure TMatriceCreuse.RecenserLowIndexes();
var
  i, j: Integer;
begin
  for i:=1 to FMaxRow do
  begin
    for j:=1 to FMaxCol do
    begin
      if (Abs(self.GetValeur(j, i)) > 0) then
      begin
        FLowIndexes[i] := j;
        Break;
      end;
    end;
  end;
end;
procedure TMatriceCreuse.RecenserHighIndexes();
var
  i, j: Integer;
begin
  for i:= 1 to FMaxRow do
  begin
    for j:= FMaxCol downto 1 do
    begin
      if (Abs(self.GetValeur(j, i)) > 0) then
      begin
        FHighIndexes[i] := j;
        Break;
      end;
    end;
  end;
end;
(*
function TMatriceCreuse.GetLowIndex(const ARow: integer): integer;
begin
  Result := FLowIndexes[ARow];
end;
//*)
procedure TMatriceCreuse.Lister;
var
  R: TListeNonZerosOfRow;
  i, n, j: Integer;
  V: TElementNonNulOfAnRow;
  EWE: String;
begin
  AfficherMessageErreur(Format('%s.Lister(): Matrice: %s (%dx%d)', [ClassName, FMatrixName, FMaxRow, FMaxCol]));
  if (FMaxRow = 0) then Exit;
  for i := 0 to FMaxRow do
  begin
    R := FListeLignes[i];
    EWE := Format('Row %d: [%d -> %d] %d values: ', [i, R.MinIdxNonZero, R.MaxIdxNonZero, R.GetNbElements()]);
    n := R.GetNbElements();

    if (n > 0) then
    begin
      for j := 0 to n-1 do
      begin
        //V := R.GetElement(j);
        EWE += Format('%f, ', [V.Valeur]);
      end;
    end;
    //*)
    AfficherMessageErreur(EWE);
  end;
end;

procedure TMatriceCreuse.RemoveAllZeroes();
var
  i: Integer;
begin
  //exit;
  for i := 1 to High(FListeLignes) do
  begin
    FListeLignes[i].RemoveZeros();
  end;
end;

end.
