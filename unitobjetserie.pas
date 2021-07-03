unit UnitObjetSerie;
// Date: 24/08/2012
// Contient l'objet TObjetSerie, utilisé par:
// - ToporobotClasses2012.pas
// - CodeCalculTopo.pas
// - Le cadre Series, nouvelle version
// 06/09/2014: Ajout des membres Accroissements
// 09/05/2016: Cet objet passe aux génériques
// 13/04/2021: Point de contrôle temporel (contrôle de version)
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  UnitListesSimplesWithGeneriques,
  Common,
  math,
  Classes, SysUtils, Graphics;
type

 { TObjSerie }
 TObjSerie = class(TListeSimple<TUneVisee>)
  private
    FIndexSerie  : TNumeroSerie;
    FSerieDep    : TNumeroSerie;
    FPtDep       : integer;
    FSerieArr    : TNumeroSerie;
    FPtArr       : integer;
    // NOUVEAU: Entrée rattachée à la série
    // Conventions:
    //   0 = Index d'entrée inconnu ou non renseigné
    //  -1 = Hérite du numéro d'entrée de sa série de rattachement
    //  -2 = <TODO>
    FNumeroEntrance : TNumeroEntrance;
    FReseau      : integer; // Réseau
    FChances     : integer;
    FObstacles   : integer;
    FCouleur     : TColor;
    FRaideur     : double;  // module de rigidité, égal à 1.00 par défaut
    FNomSerie    : String;
    FCommentaires: string;
    // accroissements
    FAccrX       : double;
    FAccrY       : double;
    FAccrZ       : double;
    FAccrP       : double;
    // condensat, utilisé notamment pour savoir si la série a été modifiée
    //FCondensat   : string;
  public
    procedure ClearStations();
    function  GetNbVisees(): integer;
    procedure AddVisee(const V: TUneVisee); overload;
    procedure AddVisee(const qNoSecteur: integer;
                       const qCode, qExpe: integer;
                       const qTypeVisee: TTypeDeVisee;
                       const qLong, qAz, qP: double;
                       const qLG, qLD, qHZ, qHN: double;
                       const qIDTerrain, qComments: string;
                       const qHorodatage: TDateTime;
                       const qTemperature, qHumidity: double); overload;
    function  GetVisee(const Idx: integer): TUneVisee;
    function  GetLastVisee(): TUneVisee;

    procedure PutVisee(const Idx: integer; const V: TUneVisee);
    procedure RemoveVisee(const Idx: integer);
    procedure RemoveLastVisee();


    procedure SetNomSerie(const S: string);
    procedure SetObsSerie(const S: string);
    procedure SetNoSerieDep(const N: TNumeroSerie); inline;
    procedure SetNoPointDep(const N: integer); inline;
    procedure SetNoSerieArr(const N: TNumeroSerie); inline;
    procedure SetNoPointArr(const N: integer);
    procedure SetNumeroEntrance(const N: TNumeroEntrance);
    procedure SetNumeroReseau(const N: TNumeroReseau);
    procedure SetChance(const N: integer);
    procedure SetObstacle(const N: integer);
    procedure SetCouleur(const C: TColor);
    procedure SetRaideur(const R: double);

    procedure SetChanceObstacle(const C, O: integer);
    procedure SetNomObsSerie(const N, S: string);
    procedure SetSeriePtArr(const S: TNumeroSerie; const P: integer);
    procedure SetSeriePtDep(const S: TNumeroSerie; const P: integer);
    procedure SetSeriePtExtremites(const Sd: TNumeroSerie; const Pd: integer;
                                   const Sa: TNumeroSerie; const Pa: integer);

    procedure SetNumeroSerie(const N: TNumeroSerie);
    function GetNumeroDeSerie(): TNumeroSerie;
    function GetNomSerie()  : string;
    function GetObsSerie()  : string;
    function GetNoSerieDep(): TNumeroSerie;
    function GetNoPointDep(): TNumeroStation;
    function GetNoSerieArr(): TNumeroSerie;
    function GetNoPointArr(): TNumeroStation;
    function GetNumeroEntrance(): TNumeroEntrance;
    function GetNumeroReseau(): TNumeroReseau;
    function GetChance()    : integer;
    function GetObstacle()  : integer;
    function GetCouleur()   : TColor;
    function GetRaideur()   : double;
    // accroissements
    procedure SetAccroissements(const x, y, z, p: Double);
    function GetAccroissementX(): double;
    function GetAccroissementY(): double;
    function GetAccroissementZ(): double;
    function GetAccroissementP(): double;
    // longueur de la série (calculée à la volée)
    function GetLongueurOfSerie(): double;
    // calcul d'erreur jusqu'à la station n
    function CalculIncertitude(const n: integer; out edx, edy, edz: double): boolean;
end;

// Copie d'une série avec création (sans ajout des antennes
function CreateAndCopySerie(const SerieSRC: TObjSerie; out SerieCible: TObjSerie): boolean;

implementation
{ TObjSerie }
procedure TObjSerie.ClearStations();
begin
  FAccrX := 0.00;
  FAccrY := 0.00;
  FAccrZ := 0.00;
  FAccrP := 0.00;
  self.ClearListe();
end;



function TObjSerie.GetNbVisees(): integer;
begin
  Result := self.GetNbElements();
end;

procedure TObjSerie.AddVisee(const V: TUneVisee); overload;
begin
  self.AddElement(V);
end;
procedure TObjSerie.AddVisee(const qNoSecteur: integer;
                             const qCode, qExpe: integer;
                             const qTypeVisee: TTypeDeVisee;
                             const qLong, qAz, qP: double;
                             const qLG, qLD, qHZ, qHN: double;
                             const qIDTerrain, qComments: string;
                             const qHorodatage: TDateTime;
                             const qTemperature, qHumidity: double); overload;
var
  WU: TUneVisee;
  QNoVisee: Integer;
begin
  QNoVisee := self.Count;// - 1;
  WU.setFrom(QNoVisee,
             qTypeVisee,
             qNoSecteur,
             qCode, qExpe,
             qLong, qAz, qP,
             qLG, qLD, qHZ, qHN,
             qHorodatage,
             qIDTerrain, qComments,
             qTemperature, qHumidity);
  self.AddVisee(WU);
end;


function TObjSerie.GetVisee(const Idx: integer): TUneVisee;
begin
  Result := self.GetElement(Idx);
end;

function TObjSerie.GetLastVisee(): TUneVisee;
var
  n: Integer;
begin
  n := self.GetNbVisees();
  Result := self.GetVisee(n - 1);
end;

procedure TObjSerie.PutVisee(const Idx: integer; const V: TUneVisee);
begin
  self.PutElement(Idx, V);
end;

procedure TObjSerie.RemoveVisee(const Idx: integer);
begin
  self.RemoveElement(Idx);
end;

procedure TObjSerie.RemoveLastVisee();
begin
  self.RemoveLastElement();
end;

function  TObjSerie.GetNumeroDeSerie(): TNumeroSerie;
begin
  Result := FIndexSerie;
end;
procedure TObjSerie.SetNumeroSerie(const N: TNumeroSerie);
begin
  FIndexSerie:=N;
end;

procedure TObjSerie.SetNomSerie(const S: string);
begin
  FNomSerie := S;
end;

procedure TObjSerie.SetObsSerie(const S: string);
begin
  FCommentaires := S;
end;

procedure TObjSerie.SetNomObsSerie(const N, S: string);
begin
  FNomSerie := N;
  FCommentaires := S;
end;

procedure TObjSerie.SetNoSerieDep(const N: TNumeroSerie); inline;
begin
  FSerieDep := N;
end;

procedure TObjSerie.SetNoPointDep(const N: integer); inline;
begin
  FPtDep := N;
end;

procedure TObjSerie.SetNoSerieArr(const N: TNumeroSerie); inline;
begin
  FSerieArr := N;
end;

procedure TObjSerie.SetNoPointArr(const N: integer);
begin
  if (N < 0) then FPtArr := self.GetNbVisees() - 1 else FPtArr := N;
end;

procedure TObjSerie.SetNumeroEntrance(const N: TNumeroEntrance);
begin
  FNumeroEntrance := N;
end;

procedure TObjSerie.SetSeriePtDep(const S: TNumeroSerie; const P: integer);
begin
  SetNoSerieDep(S);
  SetNoPointDep(P);
end;
procedure TObjSerie.SetSeriePtArr(const S: TNumeroSerie; const P: integer);
begin
  SetNoSerieArr(S);
  SetNoPointArr(P);
end;
procedure TObjSerie.SetSeriePtExtremites(const Sd: TNumeroSerie; const Pd: integer;
                                         const Sa: TNumeroSerie; const Pa: integer);
begin
  SetNoSerieDep(Sd);
  SetNoPointDep(Pd);
  SetNoSerieArr(Sa);
  SetNoPointArr(Pa);
end;



procedure TObjSerie.SetNumeroReseau(const N: TNumeroReseau);
begin
  FReseau := N;
end;

procedure TObjSerie.SetChance(const N: integer);
begin
  FChances := N;
end;

procedure TObjSerie.SetObstacle(const N: integer);
begin
  FObstacles := N;
end;
procedure TObjSerie.SetChanceObstacle(const C, O: integer);
begin
  FChances := C;
  FObstacles := O;
end;

procedure TObjSerie.SetCouleur(const C: TColor);
begin
  FCouleur := C;
end;
procedure TObjSerie.SetRaideur(const R: double);
const RAIDEUR_MINI = 0.1;
begin
  FRaideur := IIF(abs(R) < RAIDEUR_MINI, RAIDEUR_MINI, abs(R));
end;
function TObjSerie.GetNomSerie(): string;
begin
  Result := FNomSerie;
end;

function TObjSerie.GetObsSerie(): string;
begin
  Result := FCommentaires;
end;

function TObjSerie.GetNoSerieDep(): TNumeroSerie;
begin
  Result := FSerieDep;
end;

function TObjSerie.GetNoPointDep(): TNumeroStation;
begin
  Result := FPtDep;
end;

function TObjSerie.GetNoSerieArr(): TNumeroSerie;
begin
  Result := FSerieArr;

end;

function TObjSerie.GetNoPointArr(): TNumeroStation;
begin
  Result := FPtArr;

end;

function TObjSerie.GetNumeroEntrance(): TNumeroEntrance;
begin
  Result := FNumeroEntrance;
end;

function TObjSerie.GetNumeroReseau(): TNumeroReseau;
begin
  Result := FReseau;

end;

function TObjSerie.GetChance(): integer;
begin
  Result := FChances;

end;

function TObjSerie.GetObstacle(): integer;
begin
  Result := FObstacles;
end;

function TObjSerie.GetCouleur(): TColor;
begin
  Result := FCouleur;

end;

function TObjSerie.GetRaideur(): double;
begin
  Result := FRaideur;
end;

procedure TObjSerie.SetAccroissements(const x, y, z, p: Double);
begin
  FAccrX := x;
  FAccrY := y;
  FAccrZ := z;
  FAccrP := p;
end;

function TObjSerie.GetAccroissementX(): double;
begin
  Result := FAccrX;
end;

function TObjSerie.GetAccroissementY(): double;
begin
  Result := FAccrY;
end;

function TObjSerie.GetAccroissementZ(): double;
begin
  Result := FAccrZ;
end;

function TObjSerie.GetAccroissementP(): double;
begin
  Result := FAccrP;
end;

function TObjSerie.GetLongueurOfSerie(): double;
var
  MyVisee: TUneVisee;
  i, Nb: Integer;
begin
  Result := 0;
  Nb := self.GetNbVisees();
  if (0 = Nb) then Exit;
  for i := 0 to nb - 1 do
  begin
    MyVisee := GetVisee(i);
    Result += MyVisee.Longueur;
  end;
end;

// calcul d'incertitude
function TObjSerie.CalculIncertitude(const n: integer; out edx, edy, edz: double): boolean;
var
  QNbSts, i: Integer;
  QSqrtNbStations               : double;
  QIncertitudeRelativeLongueurs : double;
  QIncertitudeRelativeAzimuts   : double;
  QIncertitudeRelativePentes    : double;
  dr , rdtheta, rdphi, dt: double;
  VV: TUneVisee;
begin
  result := false;
  QNbSts := IIF(n = -1, self.GetNbVisees(), n);
  AfficherMessage(format('%s.CalculIncertitude(%d):', [ClassName, QNbSts]));
  QSqrtNbStations := sqrt(QNbSts);
  // valeurs exemple provisoires
  QIncertitudeRelativeLongueurs := 1/1000;
  QIncertitudeRelativeAzimuts   := degtorad(1/UNITE_ANGULAIRE_PAR_DEFAUT);
  QIncertitudeRelativePentes    := gradtorad(1/400.00);
  edx := 0.00;
  edy := 0.00;
  edz := 0.00;
  for i := 1 to QNbSts - 1 do
  begin
    VV := self.GetVisee(i);
    dr       := VV.Longueur * QIncertitudeRelativeLongueurs;
    rdtheta  := VV.Longueur * QIncertitudeRelativeAzimuts;
    rdphi    := VV.Longueur * QIncertitudeRelativePentes;
    dt       := Hypot3D(dr, rdtheta, rdphi);
  end;
  (*
  Sqrt_N := sqrt(QNbSts);
  edx /= Sqrt_N;
  edy /= Sqrt_N;
  edz /= Sqrt_N;
  //*)
end;

// Copie d'une série avec création (sans ajout des antennes
function CreateAndCopySerie(const SerieSRC: TObjSerie; out SerieCible: TObjSerie): boolean;
var
  MyVisee: TUneVisee;
  NbSts, i: Integer;
begin
  Result := false;
  SerieCible := TObjSerie.Create;
  try
    SerieCible.ClearStations();                                                             // Opération 1: Vider la liste des visées
    SerieCible.SetChanceObstacle(SerieSRC.GetChance(), SerieSRC.GetObstacle());             // Opération 2: Chances et obstacles
    SerieCible.SetSeriePtExtremites(SerieSRC.GetNoSerieDep(), SerieSRC.GetNoPointDep(),     // Opération 3: Extrémités de la série
                                    SerieSRC.GetNoSerieArr(), SerieSRC.GetNoPointArr());
    SerieCible.SetNumeroSerie(SerieSRC.GetNumeroDeSerie());                                 // Opération 4: Numéro de série
    SerieCible.SetNomObsSerie(SerieSRC.GetNomSerie(), SerieSRC.GetObsSerie());              // Opération 5: Nom et observations sur la série
    SerieCible.SetRaideur(SerieSRC.GetRaideur());                                           // Opération 6: Raideur
    SerieCible.SetCouleur(SerieSRC.GetCouleur());                                           // Opération 7: Couleur
    SerieCible.SetNumeroEntrance(SerieSRC.GetNumeroEntrance());                             // Opération 8: Entrée de rattachement
    SerieCible.SetNumeroReseau(SerieSRC.GetNumeroReseau());                                 // Opération 9: Réseau de rattachement
    NbSts := SerieSRC.GetNbVisees();                                                        // Opération 10: Traitement des visées
    if (NbSts > 0) then
    begin
      for i := 0 to NbSts - 1 do
      begin
        MyVisee := SerieSRC.GetVisee(i);
        SerieCible.AddVisee(MyVisee);
      end;
    end;
    result := True;
  except
    FreeAndNil(SerieCible);
  end;
end;


end.

