unit unitMatricesCreuses_FPTree;
// Stockage de matrices creuses sous forme de listes de listes (LOL)
{$INCLUDE CompilationParameters.inc}

interface
// Tests effectués:
// TArrayList: Fonctionne mais est 2x plus lent que TList
// TVector   : Fonctionne mais légèrement plus lent que TList
// Utilisation de TList.Capacity: Aucun gain de temps


uses
  //Common,
  Classes, SysUtils, math,
  Graphics//,
  //UnitListesSimplesWithGeneriques
  ;

type TElementNonNulOfAnRow = record
  IdxColumn  : Integer;
  Valeur     : double;
end;
// Liste des valeurs non-nulles d'une ligne
type



{ TMatriceCreuse }

 TMatriceCreuse = class
   private
     FMatrixName: string;
     FMaxRow: integer;
     FMaxCol: integer;

     FLowIndexes  : array of Integer;             // tableau des premiers index de valeurs non nulles
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
     procedure TrierLesIdxColonnes();
end;

implementation

{ TMatriceCreuse }

function TMatriceCreuse.Initialiser(const NbRows, NbCols: integer; const Name: string): boolean;
begin
  result := true;
  FMatrixName := Name;
  //AfficherMessageErreur(Format('%s.Initialiser: %s (%d, %d)', [ClassName, FMatrixName, NbRows, NbCols]));
  try
    FMaxRow := NbRows;
    FMaxCol := NbCols;
  finally
  end;
end;

procedure TMatriceCreuse.Finaliser();
begin

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

end;

procedure TMatriceCreuse.RecenserHighIndexes();
begin

end;

procedure TMatriceCreuse.TrierLesIdxColonnes();
begin

end;

end.

