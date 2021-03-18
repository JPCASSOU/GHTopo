unit unitMatricesCreusesArray2D;
{$NOTE This unit is very suitable for usual speleological networks, but requires much memory for huge systems (over 3000 nodes)}
{$INCLUDE CompilationParameters.inc}

interface
uses
  Common,
  Classes, SysUtils, math, Graphics
  ;


type
{ TMatriceCreuse }

 TMatriceCreuse = class
   private
     FMatrixName: string;
     FValues: array of array of single; // vérifier si ce type de réel suffit
     FMaxRow: integer;
     FMaxCol: integer;
     FLowIndexes  : array of Integer;              // tableau des premiers index de valeurs non nulles
     FHighIndexes : array of Integer;
   public
     function  Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
     procedure Finaliser();

     property  MaxRow: integer read FMaxRow;
     property  MaxCol: integer read FMaxCol;

     procedure SetValeur(const I, J: integer; const V: double); inline;
     function  GetValeur(const I, J: integer): double; inline;

     // affichage de la matrice sous forme de texte
     procedure Lister();
     // affichage de la matrice sous forme d'image
     procedure CreerRepresentationMatriceEnImage(const QFileName: string);

     procedure RecenserLowIndexes();
     procedure RecenserHighIndexes();
     procedure RemoveAllZeroes(); inline;
  end;

implementation



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
        EWE := GetValeur(i, j);
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
begin
  result := FValues[i, j];
end;
procedure TMatriceCreuse.SetValeur(const I, J: integer; const V: double);
begin
  FValues[i][j] := V;
end;


function TMatriceCreuse.Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
begin
  result := false;
  FMatrixName := Name;
  AfficherMessageErreur(Format('%s.Initialiser: %s (%d, %d)', [ClassName, FMatrixName, NbRows, NbCols]));
  try
    SetLength(FValues, 1 + NbRows, 1 + NbCols);
    SetLength(FLowIndexes, 0);
    SetLength(FLowIndexes, 1 + NbRows);
    SetLength(FHighIndexes, 0);
    SetLength(FHighIndexes, 1 + NbRows);
    FMaxRow := NbRows;
    FMaxCol := NbCols;
    Result := true;
  except
  end;
end;
procedure TMatriceCreuse.Finaliser();
var
  i: Integer;
begin
  AfficherMessageErreur(Format('%s.Finaliser(%s)', [ClassName, FMatrixName]));
  SetLength(FValues, 0, 0);
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
begin
  pass; // sans objet ici - Pour unification avec le LOL et le LOT
end;

procedure TMatriceCreuse.Lister();
var
  i, j, m, n: Integer;
  EWE: String;
  R: Double;
begin
  m := FMaxRow;
  n := FMaxCol;
  //if (m > 100) then Exit;
  AfficherMessageErreur(Format('%s.Lister(): Matrice: %s (%dx%d)', [ClassName, FMatrixName, m, n]));
  for i := 1 to m do
  begin
    EWE := Format('Row %d; ', [i]);
    for j := 1 to n do EWE += Format('%f; ', [GetValeur(i, j)]);
    AfficherMessageErreur(EWE);
  end;
end;

end.

