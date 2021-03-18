unit BZGraphesTypes;

{$mode delphi}

interface
uses
  Classes, SysUtils, math;

const MULT_SERIES: integer = 1000000;

// messages d'erreur
const
  ERR_GRAPHE_NO_ERROR         : integer =  0;
  ERR_GRAPHE_INITIALISATION   : integer = -1;

  ERR_GRAPHE_EMPTY_LIST_NODES : integer = -2;
  ERR_GRAPHE_EMPTY_LIST_ARCS  : integer = -3;

  ERR_GRAPHE_NODE_NOT_FOUND   : integer = -4;
  ERR_GRAPHE_ARC_NOT_FOUND    : integer = -5;

  ERR_GRAPHE_SAME_START_END   : integer = -6;

  ERR_GRAPHE_ARC_EXTR_DEB_NOT_FOUND     : integer = -7;
  ERR_GRAPHE_ARC_EXTR_FIN_NOT_FOUND     : integer = -8;

  ERR_GRAPHE_ITINERAIRE_NOT_FOUND       : integer = -9;



type TProcAfficherMessage = procedure(const Msg: string; const DoClear: boolean = false) of object;
type TProcOjObject        = procedure of object;
type TNumeroNoeud = type Integer;


type TNumeroArc   = type Integer;
type TIDStation = type Int64;
type TGrapheNoeud = record
  IDStation: TIDStation;
  X  : double;
  Y  : double;
  Z  : double;
  ListeArcsSortants    : array of TNumeroArc;
  ListeArcsEntrants    : array of TNumeroArc;
end;
type TGrapheArc = record
  IdxNoeudDepart  : TNumeroNoeud;
  IdxNoeudArrivee : TNumeroNoeud;
  Longueur        : double;   // le poids, ici la longueur de l'arc
  Azimut          : double;   // pour la détermination du cap de la visée suivante
  Pente           : double;
end;
type TGrapheLastError = record
  ErrCode: integer;
  ErrMsg : string;
end;

type TNodeDistances = record
  IdxNode   : TNumeroNoeud;
  Distance  : double;
end;




// Trier les GrapheStations par ZOrder
function SortGrapheNoeudsByIDStation(Item1, Item2: Pointer): Integer;

function GetAzimut(const dx, dy: Double; const Unite: double): double;
procedure GetBearingInc(const dx, dy, dz: double;
                        var Dist, Az, Inc: double;
                        const fUB, fUC: Double);

implementation
uses
  DGCDummyUnit; // pour contrer l'erreur 'Fin du source non trouvée'





// Trier les GrapheStations par ZOrder
function SortGrapheNoeudsByIDStation(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TGrapheNoeud;
begin
  E1 := Item1;
  E2 := Item2;
  if      (E1^.IDStation < E2^.IDStation) then Result := -1
  else if (E1^.IDStation = E2^.IDStation) then Result :=  0
  else                                         Result :=  1;
end;

function GetAzimut(const dx, dy: Double; const Unite: double): double;
const TWO_PI = 2 * PI;
var
  a: double;
begin
  a := ArcTan2(dy, dx + 1e-12);
  if (a < 0) then a := a + TWO_PI;
  a := 0.50 * PI - a;
  if (a < 0) then a := a + TWO_PI;
  Result := a * 0.50 * Unite / pi;
end;
// retourne la longueur, direction et pente pour dx, dy, dz
procedure GetBearingInc(const dx, dy, dz: double;
                        var Dist, Az, Inc: double;
                        const fUB, fUC: Double);
var
  dp: Double;
begin;
  dp   := Hypot(dx, dy);
  Dist := Hypot(dp,dz);
  Inc  := ArcTan2(dz, dp) * 0.5 * fUC / pi;
  Az   := GetAzimut(dx,dy, fUB);
end;


end.

