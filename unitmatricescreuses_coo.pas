unit unitMatricesCreuses_COO;
// Stockage de matrices creuses sous forme de tables de coordonnées
// Unité deprecated. Le texte ci-dessous ne doit pas être commenté
ZERO PLUS ZERO EGALE TETE A TOTO
//Raison de l'erreur: Unité deprecated

{$mode delphi}

interface

uses
  StructuresDonnees,
  Common,
  Classes, SysUtils, math,
  Graphics,
  UnitListesSimplesWithGeneriques;


type
{ TMatriceCreuse }

 TMatriceCreuse = class(TListeSimple<TElementNonNulMatrice>)
   private
     FMaxRow: integer;
     FMaxCol: integer;
     // listes des index extrêmes de premier et dernier non-zéro pour chaque ligne
     FListeNonZeroIndexesMin: TListOfIntegers;
     FListeNonZeroIndexesMax: TListOfIntegers;
     // déterminer, pour chaque ligne, les indices j mini et j maxi des valeurs non-zero
     procedure DeterminerIndicesNonZeroMinMax();
     function  FindInternalIdxByCle(const K: Int64): integer;
     function  GetInternalIndexByIJ(const I, J: integer): Int64; inline;


   public
     property  MaxRow: integer read FMaxRow;
     property  MaxCol: integer read FMaxCol;

     function  Initialiser(const MbRows, NbCols: integer): boolean;
     procedure Finaliser();

     procedure TrierEtActualiser(const DoDeterminerIndicesMinMax: boolean);
     procedure Lister();
     procedure AddValeur(const i, j: integer; const V: double);
     function  SetValeur(const I, J: integer; const V: double): boolean;
     function  GetValeur(const I, J: integer): double;

     function GetIdxFirstNonZeroForRow(const ARow: integer): integer;
     function GetIdxLastNonZeroForRow(const ARow: integer): integer;
     procedure CreerRepresentationMatriceEnImage(const QFileName: string);

     function CalcKeyByIJ(const I, J: integer): Int64; inline;

end;

implementation
const MULT_LIGNES: integer = 65536; //100 * 1000;

{ TMatriceCreuse }

function SortPositionsMatrice(Item1, Item2: Pointer): integer;
var
  E1, E2: ^Int64;
begin
  E1 :=Item1;
  E2 :=Item2;
  if (E1^ < E2^) then
    Result := -1
  else if (E1^ = E2^) then
    Result := 0
  else
    Result := 1;
end;

procedure TMatriceCreuse.AddValeur(const i, j: integer; const V: double);
var
  S: TElementNonNulMatrice;
begin
  if (IsZero(V)) then Exit;
  S.Position := CalcKeyByIJ(i, j);
  S.Valeur   := V;
  if (i > FMaxRow) then FMaxRow := i;
  if (j > FMaxCol) then FMaxCol := j;

  self.AddElement(S);
end;

function TMatriceCreuse.CalcKeyByIJ(const I, J: integer): Int64;
begin
  //Result := i * MULT_LIGNES + j;
  Result := i shl 16 + j;
end;

procedure TMatriceCreuse.Finaliser;
begin
  try
    self.ClearListe();
    FListeNonZeroIndexesMin.ClearListe();
    FListeNonZeroIndexesMax.ClearListe();
  finally
    FreeAndNil();//FListeNonZeroIndexesMin.Free;
    FreeAndNil(fli);//FListeNonZeroIndexesMax.Free;
  end;

end;

function TMatriceCreuse.FindInternalIdxByCle(const K: Int64): integer;
var
  BP: TElementNonNulMatrice;
  Q: Integer;
  // recherche par méthode récursive dichotomique
  function FindDepth(const I1, I2: integer; const QIDX: Int64): integer;
  var
    C1 : TElementNonNulMatrice;
    PVT: integer;
  begin
    //AfficherMessageErreur(Format('FindDepth: %d-%d, Niveau: %d', [I1, I2, QNiveau]));
  // coupure en deux => calcul index médian
    PVT := (I2+I1) div 2;
    // début > fin >> sortie directe avec erreur
    if (I1 > I2) then begin Result := -1; exit; end;//
    C1 := GetElement(PVT); //GetBasePoint(PVT);
    // comparaison. Si vrai >> sortie avec numéro d'index
    if (C1.Position = QIDX) then
    begin
      Result := PVT;
      Exit;
    end;
    // sinon, recherche en profondeur avec un niveau supplémentaire
    if (QIDX < C1.Position) then
    begin
      Result := FindDepth(I1, PVT-1, QIDX);
      Exit;
    end;
    Result := FindDepth(PVT+1, I2, QIDX);
  end;
begin
  Result := -1;
  Q := FindDepth(0, self.Count - 1, K);
  if (Q >= 0) then Result := Q;
end;

function TMatriceCreuse.GetIdxFirstNonZeroForRow(const ARow: integer): integer;
begin
  Result := FListeNonZeroIndexesMin.GetElement(ARow);
end;

function TMatriceCreuse.GetIdxLastNonZeroForRow(const ARow: integer): integer;
begin
  Result := FListeNonZeroIndexesMax.GetElement(ARow);
end;

function TMatriceCreuse.GetInternalIndexByIJ(const I, J: integer): Int64;
begin
  Result := FindInternalIdxByCle(CalcKeyByIJ(i, j));
end;


function TMatriceCreuse.GetValeur(const I, J: integer): double;
var
  n: Int64;
  WU: TElementNonNulMatrice;
begin
  n := GetInternalIndexByIJ(i, j);
  if (n = -1) then exit(0.00);
  WU := GetElement(n);
  Result := WU.Valeur;
end;

function TMatriceCreuse.Initialiser(const MbRows, NbCols: integer): boolean;
begin
  self.ClearListe();
  Result := false;
  try
    FListeNonZeroIndexesMin := TListOfIntegers.Create;
    FListeNonZeroIndexesMax := TListOfIntegers.Create;
    FListeNonZeroIndexesMin.ClearListe();
    FListeNonZeroIndexesMax.ClearListe();
    FMaxRow := 0;
    FMaxCol := 0;
    Result := True;
  except
    ;
  end;
end;
procedure TMatriceCreuse.DeterminerIndicesNonZeroMinMax();
var
  i, j, WUMin, WUMax: Integer;
  V: Double;
begin
  //  On vide purement et simplement ces listes
  FListeNonZeroIndexesMin.ClearListe();
  FListeNonZeroIndexesMax.ClearListe();
  // On ajoute une ligne zéro
  FListeNonZeroIndexesMin.AddElement(0);
  FListeNonZeroIndexesMax.AddElement(0);
  // et on recherche les index mini et maxi
  WUMin := 0;
  WUMax := 0;
  for i := 1 to FMaxRow do
  begin
    for j := 1 to FMaxCol do
    begin
      V := GetValeur(i, j);
      if (abs(V) > 0.00) then
      begin
        WUMin := j;
        Break;
      end;
    end;
    FListeNonZeroIndexesMin.AddElement(WUMin);
  end;
  for i := 1 to FMaxRow do
  begin
    for j := FMaxCol downto 1 do
    begin
      V := GetValeur(i, j);
      if (abs(V) > 0.00) then
      begin
        WUMax := j;
        Break;
      end;
    end;
    FListeNonZeroIndexesMax.AddElement(WUMax);
  end;
end;

procedure TMatriceCreuse.Lister;
var
  n, i, fi, li: Integer;
  WU: TElementNonNulMatrice;
begin
  Exit;
  AfficherMessageErreur(Format('Matrice creuse de taille %dx%d, avec %d non-zeros', [self.FMaxRow, self.FMaxCol, self.Count]));
  n := self.GetNbElements();
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    WU := GetElement(i);
    AfficherMessageErreur(Format('M[%d, %d] (%d) = %.3f', [WU.Position div MULT_LIGNES,
                                                           WU.Position mod MULT_LIGNES,
                                                           WU.Position, WU.Valeur]));
  end;
  // liste des index non zérios
  AfficherMessageErreur('Liste des index min et max de non-zéros');
  for i := 0 to FMaxRow do
  begin
    fi := GetIdxFirstNonZeroForRow(i);
    li := GetIdxLastNonZeroForRow(i);
    AfficherMessageErreur(Format('Ligne: %d - Mini: %d - Maxi: %d', [i, fi, li]));
  end;
end;
function TMatriceCreuse.SetValeur(const I, J: integer; const V: double): boolean;
var
  n: Int64;
  EWE: TElementNonNulMatrice;
begin
  result := false;
  n := GetInternalIndexByIJ(i, j);
  //AfficherMessageErreur(Format('%d %d %d', [i, j, n]));
  if (n >= 0) then
  begin
    EWE := GetElement(n);
    EWE.Valeur := V;
    self.PutElement(n, EWE);
    result := true;
  end
  else // sinon, on ajoute et on actualise
  begin
    self.AddValeur(i, j, V);
    self.TrierEtActualiser(false);
  end;
end;

procedure TMatriceCreuse.TrierEtActualiser(const DoDeterminerIndicesMinMax: boolean);
var
  EWE: TElementNonNulMatrice;
  l, c: Int64;
  n, i: Integer;
begin
  self.Sort(SortPositionsMatrice);
  FMaxRow := 0;
  FMaxCol := 0;
  n := self.Count;
  if (n = 0) then Exit;
  // recherche du nombre de lignes et colonnes
  for i := 0 to n - 1 do
  begin
    EWE := GetElement(i);
    l := EWE.Position div MULT_LIGNES;
    c := EWE.Position mod MULT_LIGNES;
    if (l > FMaxRow) then FMaxRow := l;
    if (c > FMaxCol) then FMaxCol := c;
  end;
  // recherche des indexes mini et maxi de non-zeros pour chaque ligne
  if (DoDeterminerIndicesMinMax) then DeterminerIndicesNonZeroMinMax();
  //Lister();
end;

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
    BMP.Free;
  end;
end;

end.

