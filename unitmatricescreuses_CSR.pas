unit unitmatricescreuses_CSR;
// Stockage de matrices creuses sous forme de sparse matrix CSR
{$INCLUDE CompilationParameters.inc}

interface
uses
  Common,
  Classes, SysUtils, math;
type
{ TMatriceCreuse }

 TMatriceCreuse = class
   private
     FMatrixName: string;
     FMaxRow: integer;
     FMaxCol: integer;
     FArrayValues   : array of double;               // tableau A des valeurs non nulles
     FArrayI        : array of Integer;              // tableau I, de taille m + 1
     FArrayJ        : array of Integer;              // tableau J
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
     procedure TrierLesIdxColonnes();
end;

implementation

{ TMatriceCreuse }

function TMatriceCreuse.Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
begin
  SetLength(FArrayValues, 0);
  SetLength(FArrayI, NbRows + 1);
  SetLength(FArrayJ, 0);
  FArrayI[0] := 0;
end;

procedure TMatriceCreuse.Finaliser();
begin
  SetLength(FArrayValues, 0);
  SetLength(FArrayI, 0);
  SetLength(FArrayJ, 0);
end;

function TMatriceCreuse.SetValeur(const I, J: integer; const V: double): boolean;
begin

end;

function TMatriceCreuse.GetValeur(const I, J: integer): double;
begin

end;

procedure TMatriceCreuse.Lister();
begin

end;

procedure TMatriceCreuse.CreerRepresentationMatriceEnImage(const QFileName: string);
begin

end;

procedure TMatriceCreuse.RecenserLowIndexes();
begin
  pass;
end;

procedure TMatriceCreuse.RecenserHighIndexes();
begin
  pass;
end;

procedure TMatriceCreuse.TrierLesIdxColonnes();
begin
  pass;
end;

end.
