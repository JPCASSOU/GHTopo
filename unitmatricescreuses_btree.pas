unit unitmatricescreuses_btree;
// Stockage de matrices creuses sous forme d'arbre BTree
// Très lent
{$mode delphi}

interface
uses
  StructuresDonnees,
  Common,
  Classes, SysUtils, math,
  Graphics,
  UnitBinaryTree
  ;

type
{ TMatriceCreuse }

 TMatriceCreuse = class
   private
     FMatrixName: string;
     FMaxRow: integer;
     FMaxCol: integer;
     FListeLignes: array of Integer;
     FLowIndexes : array of Integer;
     FInternalBTree: TBinTree;

   public
     property  MaxRow: integer read FMaxRow;
     property  MaxCol: integer read FMaxCol;
     function  Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
     procedure Finaliser();

     //procedure TrierEtActualiser(const DoDeterminerIndicesMinMax: boolean);
     procedure Lister();
     function  SetValeur(const I, J: integer; const V: double): boolean;
     function  GetValeur(const I, J: integer): double;
     procedure CreerRepresentationMatriceEnImage(const QFileName: string);
     procedure RecenserLowIndexes();
     function  GetLowIndex(const ARow: integer): integer; inline;
end;

implementation

function SortRowValuesByIdxCol(Item1, Item2: TElementNonNulOfAnRow): integer;
var
  E1, E2: TElementNonNulOfAnRow;
begin
  E1 := Item1;
  E2 := Item2;
  if (E1.IdxColumn < E2.IdxColumn) then
    Result := -1
  else if (E1.IdxColumn = E2.IdxColumn) then
    Result := 0
  else
    Result := 1;
end;
(*
function SortRowValuesByIdxCol(Item1, Item2: Pointer): integer;
var
  E1, E2: ^TElementNonNulOfAnRow;
begin
  E1 :=Item1;
  E2 :=Item2;
  if (E1^.IdxColumn < E2^.IdxColumn) then
    Result := -1
  else if (E1^.IdxColumn = E2^.IdxColumn) then
    Result := 0
  else
    Result := 1;
end;
//*)
{ TListeNonZerosOfRow }


//******************************************************************************
{ TMatriceCreuse }



procedure TMatriceCreuse.CreerRepresentationMatriceEnImage(const QFileName: string);
var
  BMP: TBitmap;
  i, j: Integer;
begin
  AfficherMessage(Format('%s.CreerRepresentationMatriceEnImage(%s) (%dx%d)', [ClassName, QFileName, FMaxRow, FMaxCol]));
  Exit;
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
begin
  Result := FInternalBTree.GetValue(I, J);
end;
function TMatriceCreuse.SetValeur(const I, J: integer; const V: double): boolean;
begin
  FInternalBTree.SetValue(i, j, V);
end;


function TMatriceCreuse.Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
var
  WU : TElementNonNulOfAnRow;
  i: Integer;
begin
  result := false;
  FMatrixName := Name;
  AfficherMessageErreur(Format('%s.Initialiser: %s (%d, %d)', [ClassName, FMatrixName, NbRows, NbCols]));
  try
    FInternalBTree := TBinTree.create;
    SetLength(FListeLignes, 0);
    SetLength(FListeLignes, 1 + NbRows);
    SetLength(FLowIndexes, 0);
    SetLength(FLowIndexes, 1 + NbRows);
    FMaxRow := NbRows;
    FMaxCol := NbCols;
    WU.IdxColumn := 0;
    WU.Valeur    := 0;
    Result := true;
  except
  end;
end;
procedure TMatriceCreuse.Finaliser;
var
  i: Integer;
begin
  try
    AfficherMessageErreur(Format('%s.Finaliser(%s)', [ClassName, FMatrixName]));
    SetLength(FLowIndexes, 0);

  finally
    FreeAndNil(FInternalBTree);//FInternalBTree.Free;
  end;
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

function TMatriceCreuse.GetLowIndex(const ARow: integer): integer;
begin
  Result := FLowIndexes[ARow];
end;

procedure TMatriceCreuse.Lister;
var
  i, n, j: Integer;
  V: TElementNonNulOfAnRow;
  EWE: String;
begin
  AfficherMessageErreur(Format('%s.Lister(): Matrice: %s (%dx%d)', [ClassName, FMatrixName, FMaxRow, FMaxCol]));
end;



procedure TMatriceCreuse.TrierLesIdxColonnes();
var
  i: Integer;
begin
//  for i := 1 to MaxRow do FListeLignes[i].SortValuesByColumns();
end;

end.
