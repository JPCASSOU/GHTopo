unit unitMatricesCreuses_LOT;
{$NOTE    LOT storage method requires little memory space but is slower than Array2D storage}
{$WARNING Use the unit unitMatricesCreusesArray2D (which is very fast) for usual speleological networks}
// Stockage de matrices creuses sous forme de listes de tableaux (LOT)
// Avantages pour Citon II_20180728
// - Occupation mémoire: 19 Mo contre 31 Mo en LOL
// - Vitesse de factorisation: 00:01:46 contre 00:02:15 en LOL
{$INCLUDE CompilationParameters.inc}

interface
// Tests effectués:
// Un peu plus rapide que LOL
// ex: CitonII_20180728: LOL: 0 min 48 + 2 min 08
//                       LOT: 0 min 43 + 1 min 47


uses
  Common,
  Classes, SysUtils, math,
  Graphics
  ;

type TElementNonNulOfAnRow = record
  IdxColumn  : Integer;
  Valeur     : double;
end;
// Liste des valeurs non-nulles d'une ligne
type

{ TListeNonZerosOfRow }

 TListeNonZerosOfRow = class
  strict private // indique que ce qui suit n'est pas visible à l'extérieur de la classe
    FMinIdxNonZero: integer;
    FMaxIdxNonZero: integer;
    FValues       : array of TElementNonNulOfAnRow;
    FBiggest      : double;

  public
    constructor Create(const NbCols: integer);
    property  MinIdxNonZero: integer read FMinIdxNonZero write FMinIdxNonZero; // Index de colonne du premier élément non nul
    property  MaxIdxNonZero: integer read FMaxIdxNonZero write FMaxIdxNonZero; // Index de colonne du deuxième élément non nul

    function  GetNbElements(): integer; inline;
    function  GetElement(const Idx: integer): TElementNonNulOfAnRow; inline;
    procedure AddElement(const V: TElementNonNulOfAnRow);
    procedure PutElement(const Idx: integer; const V: TElementNonNulOfAnRow);
    procedure InsertElementAtFirst(const V: TElementNonNulOfAnRow);
    function  FindValueByIndexCol(const Idx: integer;
                                  out   BP: TElementNonNulOfAnRow;
                                  out   InternalIdx: integer): boolean;
    procedure ClearListe();
    procedure RemoveZeros();
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

constructor TListeNonZerosOfRow.Create(const NbCols: integer);
begin
  SetLength(FValues, 0);
  self.FMinIdxNonZero  := 1 + NbCols;
  self.FMaxIdxNonZero  := -1;
  self.FBiggest        := -1E24;
  //FLastIdxCol          := -1;
  //self.FLastInternalIdxFound := -1;
end;

function TListeNonZerosOfRow.GetNbElements(): integer;
begin
  Result := Length(FValues);
end;

function TListeNonZerosOfRow.GetElement(const Idx: integer): TElementNonNulOfAnRow;
begin
  Result := FValues[Idx];
end;

procedure TListeNonZerosOfRow.AddElement(const V: TElementNonNulOfAnRow);
begin
  SetLength(FValues, 1 + Length(FValues));
  FValues[High(FValues)] := V;
  FMaxIdxNonZero := max(FMaxIdxNonZero, V.IdxColumn);
  FBiggest       := max(FBiggest, V.Valeur);
  //FLastInternalIdxFound := Length(FValues) - 1;
  //FLastIdxCol           := V.IdxColumn;
end;

procedure TListeNonZerosOfRow.PutElement(const Idx: integer; const V: TElementNonNulOfAnRow);
begin
  FValues[Idx] := V;
  FMinIdxNonZero := min(FMinIdxNonZero, V.IdxColumn);
  FMaxIdxNonZero := max(FMaxIdxNonZero, V.IdxColumn);
  FBiggest       := max(FBiggest, V.Valeur);
  //FLastInternalIdxFound := Idx;
  //FLastIdxCol           := V.IdxColumn;
end;

procedure TListeNonZerosOfRow.InsertElementAtFirst(const V: TElementNonNulOfAnRow);
var i: integer;
begin
  SetLength(FValues, 1 + length(FValues));
  for i := High(FValues) downto 1 do FValues[i] := FValues[i-1];
  FValues[0] := V;
  FMinIdxNonZero := min(FMinIdxNonZero, V.IdxColumn);
  FBiggest       := max(FBiggest, V.Valeur);
  //FLastInternalIdxFound := 0;
  //FLastIdxCol           := V.IdxColumn;
end;

function TListeNonZerosOfRow.FindValueByIndexCol(const Idx: integer;
                                                 out   BP: TElementNonNulOfAnRow;
                                                 out   InternalIdx: integer): boolean;
var
  i, QN, n: Integer;
  // recherche par méthode récursive dichotomique (effectivement le plus rapide)
  function FindDepth(const I1, I2: integer; const QIDX: integer):  integer;
  var
    PVT: integer;
    C1: TElementNonNulOfAnRow;
  begin
    // début > fin >> sortie directe avec erreur
    if (I1 > I2) then Exit(-1);  // penser à utiliser Exit comme le return() du C
    // coupure en deux => calcul index médian
    PVT := (I2 + I1) div 2;
    C1  := self.GetElement(PVT);
    // comparaison. Si vrai -> sortie avec numéro d'index
    if (C1.IdxColumn = QIDX) then Exit(PVT);
    // sinon, recherche en profondeur avec un niveau supplémentaire
    if (QIDX < C1.IdxColumn) then Exit(FindDepth(I1, PVT-1, QIDX));
    Result := FindDepth(PVT+1, I2, QIDX);
  end;
begin
  Result      := false;
  QN := self.GetNbElements();
  if (QN = 0) then Exit;
  (*  Aucune amélioration significative de la vitesse de calcul
      en stockant les Idx des derniers éléments accédés
      (et même une légère augmentation ...)
  if (FLastIdxCol = Idx) then
  begin
    InternalIdx := FLastInternalIdxFound;
    BP := GetElement(FLastInternalIdxFound);
    Exit(True);
  end;
  //*)
  i := FindDepth(0, QN - 1, Idx);
  if (i >= 0) then
  begin
    InternalIdx := i;
    //FLastInternalIdxFound := i;
    //FLastIdxCol := Idx;
    BP     := GetElement(i);
    Exit(True);
  end;
  //*)
end;

procedure TListeNonZerosOfRow.ClearListe();
begin
  SetLength(FValues, 0);
end;

procedure TListeNonZerosOfRow.RemoveZeros();
begin
  pass;
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
  i, j, m, n: Integer;
  EWE: Double;
begin
  m := FMaxRow;
  n := FMaxCol;
  AfficherMessageErreur(Format('%s.CreerRepresentationMatriceEnImage(%s) (%dx%d)', [ClassName, QFileName, m, n]));
  BMP := TBitmap.Create;
  try
    BMP.Height  := 1 + m + 1;
    BMP.Width   := 1 + n + 1;
    BMP.Canvas.Brush.Color := clAqua;
    BMP.Canvas.Pen.Color   := clRed;
    BMP.Canvas.FillRect(0, 0, BMP.Width, BMP.Height);
    for i := 1 to m do
    begin
      for j := 1 to n do
      begin
        EWE := self.GetValeur(i, j);
        if (not IsZero(EWE)) then
          BMP.Canvas.Pixels[j, i] := IIF(EWE > 0, clRed, clGreen);
      end;
    end;
    BMP.SaveToFile(QFileName);
  finally
    FreeAndNil(BMP);//BMP.Free;
  end;
end;



function TMatriceCreuse.GetValeur(const I, J: integer): double;
var
  MyRow: TListeNonZerosOfRow;
  BP: TElementNonNulOfAnRow;
  QInternalIdx: integer;
begin
  Result := 0.00;
  MyRow := FListeLignes[i];
  if (MyRow.FindValueByIndexCol(j, BP, QInternalIdx)) then Result := BP.Valeur;
end;
function TMatriceCreuse.SetValeur(const I, J: integer; const V: double): boolean;
var
  MyRow: TListeNonZerosOfRow;
  BP   : TElementNonNulOfAnRow;
  QInternalIdx: integer;
begin
  Result := false;
  QInternalIdx := -1;
  //if (abs(V) > 0.00) then
  if (not IsZero(V)) then
  begin
    MyRow := FListeLignes[i];
    // Si j est en dehors des limites connues, on ajoute sans rechercher
    // (gain de 25 secondes avec Citon II version 20180721, 5260 x 4290)
    if (j < MyRow.MinIdxNonZero) then  // ajout à gauche
    begin
      BP.IdxColumn := j;
      BP.Valeur    := V;
      //MyRow.MinIdxNonZero := j;
      MyRow.InsertElementAtFirst(BP);
      exit(true);
    end
    else if (j > MyRow.MaxIdxNonZero) then // ajout à droite
    begin
      BP.IdxColumn := j;
      BP.Valeur    := V;
      //MyRow.MaxIdxNonZero := j;
      MyRow.AddElement(BP);
      exit(true);
    end
    else
    begin  // S'il y a déjà une valeur en M[i,j], on la remplace
      if (MyRow.FindValueByIndexCol(J, BP, QInternalIdx)) then
      begin
        BP.Valeur    := V;
        MyRow.PutElement(QInternalIdx, BP);
        Result := true;
      end ;
    end;
  end;
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
    SetLength(FListeLignes, 0);
    SetLength(FListeLignes, 1 + NbRows);
    SetLength(FLowIndexes, 0);
    SetLength(FLowIndexes, 1 + NbRows);
    SetLength(FHighIndexes, 0);
    SetLength(FHighIndexes, 1 + NbRows);
    FMaxRow := NbRows;
    FMaxCol := NbCols;
    WU.IdxColumn := 0;
    WU.Valeur    := 0.00;
    FListeLignes[0] := TListeNonZerosOfRow.Create(NbCols);
    FListeLignes[0].ClearListe();
    FListeLignes[0].AddElement(WU);
    for i := 1 to High(FListeLignes) do
    begin
      FListeLignes[i] := TListeNonZerosOfRow.Create(NbCols);
      FListeLignes[i].ClearListe();
    end;
    Result := true;
  except
  end;
end;
procedure TMatriceCreuse.Finaliser();
var
  i: Integer;
begin
  AfficherMessageErreur(Format('%s.Finaliser(%s)', [ClassName, FMatrixName]));
  for i := 0 to High(FListeLignes) do
  begin
    try
      FListeLignes[i].ClearListe();
    finally
      FreeAndNil(FListeLignes[i]);
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

procedure TMatriceCreuse.RemoveAllZeroes();
var
  i: Integer;
begin
  for i := 0 to High(FListeLignes) do
  begin
    FListeLignes[i].RemoveZeros();
  end;
end;

(*
function TMatriceCreuse.GetLowIndex(const ARow: integer): integer;
begin
  Result := FLowIndexes[ARow];
end;
//*)
procedure TMatriceCreuse.Lister();
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
    (*
    if (n > 0) then
    begin
      for j := 0 to n-1 do
      begin
        V := R.GetElement(j);
        EWE += Format('M[%d, %d] = %f; ', [i, V.IdxColumn, V.Valeur]);
      end;
    end;
    //*)
    //AfficherMessageErreur(EWE);
  end;
end;

end.
//********************************************************************************



