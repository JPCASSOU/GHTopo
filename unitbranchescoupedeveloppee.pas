// 05/05/2016: Réacculturation - Les listes simples passent aux génériques
unit UnitBranchesCoupeDeveloppee;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  UnitListesSimplesWithGeneriques,
  ToporobotClasses2012,
  Classes, SysUtils;
type

{ TBrancheCoupeDeveloppee }
 TBrancheCoupeDeveloppee = class(TListeSimple<TUneVisee>)
    constructor Create;
  private
    FDocumentToporobot: TToporobotStructure2012;
    FNumeroSerie       : integer;
    FNumeroReseau      : integer;
    FNumeroBranche     : integer;
    FNomBranche        : string;
    FNumeroNoeudDepart : int64;
    FNumeroNoeudArrivee: int64;


    FDeltaP            : double;
    FDeltaZ            : double;
    // sens de dessin
    FSensTraceCoupe    : TSensTraceCoupeDev;

  public
    // on n'utilise pas ici les setters/getters (trop lourd)
    property  NumeroSerie        : integer read FNumeroSerie write FNumeroSerie;
    property  NumeroReseau:integer read FNumeroReseau write FNumeroReseau;
    property  NumeroBranche     : integer read FNumeroBranche write FNumeroBranche;
    property  NomBranche: string read FNomBranche write FNomBranche;

    property  NumeroNoeudDepart  : int64 read FNumeroNoeudDepart  write FNumeroNoeudDepart;
    property  NumeroNoeudArrivee : int64 read FNumeroNoeudArrivee write FNumeroNoeudArrivee;

    property  DeltaP             : double read FDeltaP write FDeltaP;
    property  DeltaZ             : double read FDeltaZ write FDeltaZ;

    property  SensTraceCoupe     : TSensTraceCoupeDev read FSensTraceCoupe write FSensTraceCoupe default stcdVERS_DROITE;

    //property  :integer read  write ;
    // passage du pointeur vers le document Toporobot
    procedure SetDocToporobotPointer(const T: TToporobotStructure2012);

    // ici, c'est la liste des visées, donc: getters et setters
    procedure ViderListeVisees();

    function  GetNbVisees(): integer;
    procedure addVisee(const V : TUneVisee);
    procedure addViseeByValeurs(const qIDStation: integer;
                                const qIDTerrain: string;
                                const qSecteur: integer;
                                const qTypeVisee: TTypeDeVisee;
                                const qCode, qExpe: integer;
                                const qLongueur, qAzimut, qPente: double;
                                const qLG, qLD, qHZ, qHN: double;
                                const qCommentaire: string);

    function  GetVisee(const Idx: integer): TUneVisee;
    procedure PutVisee(const Idx: integer; const V : TUneVisee);

    procedure CalcDeltaPZ();


end;

implementation

{ TBrancheCoupeDeveloppee }


constructor TBrancheCoupeDeveloppee.Create;
begin
  Inherited;
  FSensTraceCoupe     := stcdVERS_DROITE; // le dessin de la branche se fait vers la droite
end;



procedure TBrancheCoupeDeveloppee.ViderListeVisees;
begin
  self.ClearListe();
end;


function TBrancheCoupeDeveloppee.GetNbVisees: integer;
begin
  Result := self.GetNbElements();
end;

procedure TBrancheCoupeDeveloppee.addVisee(const V: TUneVisee);
begin
  self.AddElement(V);
end;

procedure TBrancheCoupeDeveloppee.addViseeByValeurs(const qIDStation: integer;
                                                    const qIDTerrain: string;
                                                    const qSecteur: integer;
                                                    const qTypeVisee: TTypeDeVisee;
                                                    const qCode, qExpe: integer;
                                                    const qLongueur, qAzimut, qPente: double;
                                                    const qLG, qLD, qHZ, qHN: double;
                                                    const qCommentaire: string);
var
  EWE : TUneVisee;
begin
  EWE := EmptyVisee('');
  EWE.IDTerrainStation := qIDTerrain;
  EWE.TypeVisee        := qTypeVisee;
  EWE.IDSecteur   := qSecteur;
  EWE.Code        := qCode;
  EWE.Expe        := qExpe;
  EWE.Longueur    := qLongueur;
  EWE.Azimut      := qAzimut;
  EWE.Pente       := qPente;
  EWE.LG          := qLG;
  EWE.LD          := qLD;
  EWE.HZ          := qHZ;
  EWE.HN          := qHN;
  EWE.Commentaires:= qCommentaire;
  self.AddElement(EWE);
end;

function TBrancheCoupeDeveloppee.GetVisee(const Idx: integer): TUneVisee;
begin
  Result := self.GetElement(Idx);
end;

procedure TBrancheCoupeDeveloppee.PutVisee(const Idx: integer;  const V: TUneVisee);
begin
  self.PutElement(Idx, V);
end;

procedure TBrancheCoupeDeveloppee.CalcDeltaPZ;
var
  Nb: Integer;
  i: Integer;
  V: TUneVisee;
  C: TCode;
  E: TExpe;
  AX, AY, AP, AZ: double;
begin
  self.FDeltaP := 0.001;
  self.FDeltaZ := 0.001;
  Nb := GetNbVisees;
  if (Nb > 0) then
  begin
    for i := 0 to nb - 1 do
    begin
       V := GetVisee(i);
       C := FDocumentToporobot.GetCodeByIndex(V.Code);
       E := FDocumentToporobot.GetExpeByIndex(V.Expe);
       // factorisation du code
       CalculerVisee(V, C, E,
                     AX, AY, 1.00,
                     AZ, AP);
       self.FDeltaP := self.FDeltaP + IIF((FSensTraceCoupe = stcdVERS_DROITE), V.DeltaP, -V.DeltaP);
       self.FDeltaZ := self.FDeltaZ + V.DeltaZ;
       PutVisee(i, V);
    end;
  end;
end;

procedure TBrancheCoupeDeveloppee.SetDocToporobotPointer(const T: TToporobotStructure2012);
begin
  FDocumentToporobot := T;
end;



end.

