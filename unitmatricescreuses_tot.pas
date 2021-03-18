unit unitMatricesCreuses_TOT;
{$ERROR Please use the unit unitMatricesCreuses_LOT (faster than this unit)}
// Stockage de matrices creuses sous forme de tableaux de tableaux
// Fonctionne mais n'apporte rien de plus que la méthode LOL,
// tant en mobilisation mémoire qu'en temps de calcul (plus lent)

{$mode delphi}

interface

uses
  StructuresDonnees,
  Common,
  Classes, SysUtils, math,
  Graphics;


type TListeNonZerosOfRow = array of TElementNonNulOfAnRow;
type TListNonZeroCompare = function (Item1, Item2: TElementNonNulOfAnRow): Integer;


type
{ TMatriceCreuse }

 TMatriceCreuse = class
   private
     FNbreReallocationsColonnes: Int64;
     FMatrixName: string;
     FMaxRow: integer;
     FMaxCol: integer;
     FListeLignes: array of TListeNonZerosOfRow;
     FListeFirstIdxNonZeroOfRow: array of integer;
     FLowIndexes : array of Integer;


     function FindValueByRowCol(const MyRow: TListeNonZerosOfRow;
                                const ACol: integer;
                                out   BP: TElementNonNulOfAnRow;
                                out   QInternalIdx: integer): boolean;
     function TrierIdxColsOfARow(const Idx: integer): integer;

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
     procedure TrierLesIdxColonnes();
     function  ComposerUneRangee(const Idx: integer; const ARow: TArrayOfFloats): boolean;

end;

implementation

function SortRowValuesByIdxCol(Item1, Item2: TElementNonNulOfAnRow): integer;
begin
  if (Item1.IdxColumn < Item2.IdxColumn) then
    Result := -1
  else if (Item1.IdxColumn = item2.IdxColumn) then
    Result := 0
  else
    Result := 1;
end;

Procedure QuickSortArray(const MyArray: TListeNonZerosOfRow;
                         L, R : Longint;
                         const Compare: TListNonZeroCompare);
var
  I, J : Longint;
  P, Q: TElementNonNulOfAnRow;
begin
 if (length(MyArray) = 0) then Exit;
 repeat
   I := L;
   J := R;
   P := MyArray[ (L + R) div 2 ];
   repeat
     while Compare(P, MyArray[i]) > 0 do I := I + 1;
     while Compare(P, MyArray[J]) < 0 do J := J - 1;
     If I <= J then
     begin
       Q := MyArray[I];
       MyArray[I] := MyArray[J];
       MyArray[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if (J - L) < (R - I) then
   begin
     if (L < J) then
       QuickSortArray(MyArray, L, J, Compare);
     L := I;
   end
   else
   begin
     if (I < R) then
       QuickSortArray(MyArray, I, R, Compare);
     R := J;
   end;
 until L >= R;
end;



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





function TMatriceCreuse.Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
var
  WU : TElementNonNulOfAnRow;
  i: Integer;
begin
  result := false;
  FMatrixName := Name;
  FNbreReallocationsColonnes := 0;
  AfficherMessageErreur(Format('%s.Initialiser: %s (%d, %d)', [ClassName, FMatrixName, NbRows, NbCols]));
  try
    SetLength(FListeLignes, 0);
    SetLength(FListeLignes, 1 + NbRows);

    SetLength(FLowIndexes, 0);
    SetLength(FLowIndexes, 1 + NbRows);

    SetLength(FListeFirstIdxNonZeroOfRow, 0);
    SetLength(FListeFirstIdxNonZeroOfRow, 1 + NbRows);
    for i := 0 to High(FListeFirstIdxNonZeroOfRow) do FListeFirstIdxNonZeroOfRow[i] := 0;


    FMaxRow := NbRows;
    FMaxCol := NbCols;
    WU.IdxColumn := 0;
    WU.Valeur    := 0;
    SetLength(FListeLignes[0], 1);
    FListeLignes[0][0] := WU;
    for i := 1 to High(FListeLignes) do SetLength(FListeLignes[i], 0);
    Result := true;
    AfficherMessageErreur('-- OK');
  except
  end;
end;
procedure TMatriceCreuse.Finaliser;
var
  i: Integer;
begin
  AfficherMessageErreur(Format('%s.Finaliser(%s) - %d réallocations de colonnes', [ClassName, FMatrixName, FNbreReallocationsColonnes]));
  for i := 0 to High(FListeLignes) do SetLength(FListeLignes[i], 0);
  SetLength(FListeLignes, 0);
  SetLength(FLowIndexes, 0);
  SetLength(FListeFirstIdxNonZeroOfRow, 0);
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
  Result := FListeFirstIdxNonZeroOfRow[ARow];
end;

procedure TMatriceCreuse.Lister;
var
  R: TListeNonZerosOfRow;
  i, n, j: Integer;
  V: TElementNonNulOfAnRow;
  EWE: String;
begin
  AfficherMessageErreur(Format('%s.Lister(): Matrice: %s (%dx%d)', [ClassName, FMatrixName, FMaxRow, FMaxCol]));
  (*
  if (FMaxRow = 0) then Exit;
  for i := 0 to FMaxRow do
  begin
    R := FListeLignes[i];
    EWE := Format('Row %d: %d values: ', [i, R.GetNbElements()]);
    n := R.GetNbElements();
    if (n > 0) then
    begin
      for j := 0 to n-1 do
      begin
        V := R.GetElement(j);
        EWE += Format('M[%d, %d] = %f; ', [i, V.IdxColumn, V.Valeur]);
      end;
    end;
    AfficherMessageErreur(EWE);
  end;
  //*)
end;

function TMatriceCreuse.SetValeur(const I, J: integer; const V: double): boolean;
var
  MyRow: TListeNonZerosOfRow;
  BP   : TElementNonNulOfAnRow;
  QInternalIdx, QL: integer;
begin
  //AfficherMessageErreur(Format('%s.SetValue: %d, %d, %v', [ClassName, i, j, V]));
  Result := false;
  QInternalIdx := -1;
  if (abs(V) > 0) then
  begin
    MyRow := FListeLignes[i];
    // S'il y a déjà une valeur en M[i,j], on la remplace
    if (self.FindValueByRowCol(MyRow, J, BP, QInternalIdx)) then
    begin
      BP.Valeur := V;
      MyRow[QInternalIdx] := BP;

    end
    else // sinon on ajoute
    begin
      QL := Length(MyRow);
      SetLength(MyRow, QL + 1);
      Inc(FNbreReallocationsColonnes);
      BP.IdxColumn := j;
      BP.Valeur    := V;
      QL := High(MyRow);
      MyRow[QL] := BP;
      // Index min de non-zero changé ? On retrie
      if (j < FListeFirstIdxNonZeroOfRow[i]) then
        FListeFirstIdxNonZeroOfRow[i] := TrierIdxColsOfARow(i);
    end;
    FListeLignes[i] := MyRow; // Indispensable !!! FListeLignes n'est pas une liste de pointeurs !
    Result := true;
  end;
end;

function TMatriceCreuse.GetValeur(const I, J: integer): double;
var
  MyRow: TListeNonZerosOfRow;
  BP: TElementNonNulOfAnRow;
  QInternalIdx: integer;
begin
  Result := 0;
  //AfficherMessageErreur(Format('GetValeur %d, %d', [i, j]));
  MyRow := FListeLignes[i];
  if (self.FindValueByRowCol(MyRow, j, BP, QInternalIdx)) then Result := BP.Valeur;
end;

function TMatriceCreuse.TrierIdxColsOfARow(const Idx: integer): integer;
var
  MyRow: TListeNonZerosOfRow;
begin
  MyRow := FListeLignes[Idx];
  if (length(MyRow) = 0) then Exit(-1);
  QuickSortArray(MyRow, 0, High(MyRow), SortRowValuesByIdxCol);
  Result := MyRow[0].IdxColumn;
  FListeLignes[Idx] := MyRow;
end;

procedure TMatriceCreuse.TrierLesIdxColonnes();
var
  i: Integer;
begin
  for i := 1 to FMaxRow do FListeFirstIdxNonZeroOfRow[i] := TrierIdxColsOfARow(i);
end;

function TMatriceCreuse.FindValueByRowCol(const MyRow: TListeNonZerosOfRow;
                                          const ACol: integer;
                                          out   BP: TElementNonNulOfAnRow;
                                          out   QInternalIdx: integer): boolean;
  function FindDepth(const I1, I2: integer; const QIDX: integer):  integer;
  var
    PVT: integer;
    C1: TElementNonNulOfAnRow;
  begin
    // coupure en deux => calcul index médian
    PVT := (I2+I1) div 2;
    // début > fin >> sortie directe avec erreur
    if (I1 > I2) then Exit(-1);  // penser à utiliser Exit comme le return() du C
    C1 := MyRow[PVT];
    // comparaison. Si vrai -> sortie avec numéro d'index
    if (C1.IdxColumn = QIDX) then Exit(PVT);
    // sinon, recherche en profondeur avec un niveau supplémentaire
    if (QIDX < C1.IdxColumn) then Exit(FindDepth(I1, PVT-1, QIDX));
    Result := FindDepth(PVT+1, I2, QIDX);
  end;

var
  k, QN: integer;
begin
  Result      := false;
  QN := Length(MyRow);
  if (QN = 0) then Exit;
  QInternalIdx := -1;
  k := FindDepth(0, QN - 1, ACol);
  if (k >= 0) then
  begin
    QInternalIdx := k;
    BP     := MyRow[k]; //GetElement(i);
    Result := True;
    Exit;
  end;
end;

function TMatriceCreuse.ComposerUneRangee(const Idx: integer; const ARow: TArrayOfFloats): boolean;
var
  j: Integer;
begin
  for j := 1 to High(ARow) do self.SetValeur(Idx, J, ARow[j]);
end;

end.
