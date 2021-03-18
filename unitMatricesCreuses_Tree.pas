// Testé OK mais très lent
unit unitMatricesCreuses_Tree;
{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  Common,
  Classes, SysUtils,
  AvgLvlTree;

type TMyData = record
    Col   : Integer;
    Value : double;
end;

type
{ TMatriceCreuse }

 TMatriceCreuse = class
   private
     FMatrixName    : string;
     FLastNodeUsed  : TAvgLvlTreeNode;

     FMaxRow: integer;
     FMaxCol: integer;

     FArrayOfAVLTrees: array of TAvgLvlTree;
     FLowIndexes  : array of Integer;             // tableau des premiers index de valeurs non nulles
     FHighIndexes : array of Integer;
     MyData  : TMyData;


     function FindValeur(const I, J: integer; out NodeFound: TAvgLvlTreeNode): boolean;
     function getRow(const Idx: integer): TAvgLvlTree;
     procedure Lister();


   public
     function  Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
     procedure Finaliser();
     property  MatrixName: string read FMatrixName;
     property  MaxRow: integer read FMaxRow;
     property  MaxCol: integer read FMaxCol;

     function  SetValeur(const I, J: integer; const V: double): boolean;
     function  GetValeur(const I, J: integer): double;
     // affichage de la matrice sous forme de texte
     // affichage de la matrice sous forme d'image
     procedure CreerRepresentationMatriceEnImage(const QFileName: string);

     procedure RecenserLowIndexes();
     procedure RecenserHighIndexes();
     procedure TrierLesIdxColonnes();
end;

implementation

uses
  Unit1; // pour AfficherMessages
const
  MULT_ROW = 65536;

function CompareMyData(Item1, Item2: Pointer): integer;
var
  E1, E2: ^TMyData;
  bidon1, bidon2: Integer;
begin
  E1 := Item1;
  E2 := Item2;

  bidon1 := E1^.Col;
  bidon2 := E2^.Col;
  if      (bidon1 < bidon2) then Result := -1
  else if (bidon1 = bidon2) then Result := 0
  else                           Result := 1;
end;
{ TMatriceCreuse }

function TMatriceCreuse.Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
var
  Valeur11: ^TMyData;
  i       : Integer;
begin
  result := false;
  FMatrixName := Name;
  AfficherMessageErreur(Format('%s.Initialiser: %s (%d, %d)', [ClassName, FMatrixName, NbRows, NbCols]));
  try
    FMaxRow := NbRows;
    FMaxCol := NbCols;
    setlength(FArrayOfAVLTrees, NbRows + 1);
    SetLength(FLowIndexes , NbRows + 1);
    SetLength(FHighIndexes, NbRows + 1);

    for i := 0 to High(FArrayOfAVLTrees) do
    begin
      FArrayOfAVLTrees[i] := TAvgLvlTree.Create(CompareMyData);
    end;

    //New(Valeur11);
    //Valeur11.RowCol := MULT_ROW * 1 + 1;
    //Valeur11.Value  := 666.00;
    //FLastNodeUsed := FAVLTree.Add(Valeur11);
    SetValeur(1, 1, 1.00);
    Result := true;
  except
  end;
end;
function  TMatriceCreuse.getRow(const Idx: integer): TAvgLvlTree;
begin
  try
    //if (Idx < 1) then Exception.Create('L''indice de ligne doit être compris entre 1 et n');
    Result := FArrayOfAVLTrees[Idx];
  except
  end;
end;

procedure TMatriceCreuse.Finaliser();
var
  i: Integer;
begin
  for i := 0 to High(FArrayOfAVLTrees) do
  begin
    //FArrayOfAVLTrees[i].
    FArrayOfAVLTrees[i].FreeAndClear;
  end;
end;

function TMatriceCreuse.SetValeur(const I, J: integer; const V: double): boolean;
var
  NodeFound: TAvgLvlTreeNode;
  P: ^TMyData;
begin
 // AfficherMessageErreur(Format('SetValeur(M(%d, %d) = %.3f', [i, j, v]));
  if (FindValeur(I, J, NodeFound)) then
  begin
    P := NodeFound.Data;
    P^.Value := V;
  end
  else
  begin
     New(P);
     P^.Col := J;
     P^.Value  := V;
     FArrayOfAVLTrees[i-1].Add(P);
  end;
end;

function TMatriceCreuse.GetValeur(const I, J: integer): double;
var
  NodeFound: TAvgLvlTreeNode;
  P: ^TMyData;
begin
  if (FindValeur(I, J, NodeFound)) then
  begin
    P := NodeFound.Data;
    Result := P^.Value;
  end
  else
  begin
    result := 0.00;
  end;
end;

procedure TMatriceCreuse.Lister();
begin

end;

function TMatriceCreuse.FindValeur(const I, J: integer; out NodeFound: TAvgLvlTreeNode): boolean;
var
  MyData : ^TMyData;
begin
  result := false;
  try
  New(MyData);
    //AfficherMessageErreur(Format('FindValeur: %d, %d', [i, j]));
    MyData^.Col :=  J;
    NodeFound := FArrayOfAVLTrees[i].Find(MyData);
    if (Assigned(NodeFound)) then
    begin
      Result := (NodeFound.Data <> nil);
    end;
  Dispose(MyData);

  except
    on E: Exception do AfficherMessageErreur(E.Message);
  end;
end;

procedure TMatriceCreuse.CreerRepresentationMatriceEnImage(const QFileName: string);
begin

end;

procedure TMatriceCreuse.RecenserLowIndexes();
var
  i, j: Integer;
begin
  for i:=1 to FMaxRow do
  begin
    for j:=1 to FMaxCol do
    begin
      if (Abs(self.GetValeur(j, i)) > 0.00) then
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

procedure TMatriceCreuse.TrierLesIdxColonnes();
begin

end;

end.

