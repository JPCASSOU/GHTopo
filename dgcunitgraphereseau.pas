unit DGCUnitgraphereseau;
// Implémentation du graphe d'un réseau
// Indépendant de GHTopo
{$mode delphiunicode}

interface

uses
  DGCListesSimples,
  DGCBinaryTree,
  Classes, SysUtils, Math;
type TDGCIDSommet = type integer;
type TDGCIDArc    = type integer;
type TDGCListeArcs = array of TDGCIDArc;

type

{ TDGCGrapheSommet }

 TDGCGrapheSommet = record
  IDSommet: TDGCIDSommet;
  Flag    : byte;
  X       : double;
  Y       : double;
  Distance: double;
  ArcsVoisins: TDGCListeArcs;   // liste d'adjacence
  procedure AddArcVoisin(const A: TDGCIDArc);
  function  GetArcVoisin(const idx: integer): TDGCIDArc;
  procedure SetArcVoisin(const idx: integer; const A: TDGCIDArc);
 private
   function GetArcVoisinByID(const idx: TDGCIDArc): TDGCIDArc;
end;

type TDGCGrapheArc = record
  IDArc         : TDGCIDArc;
  SommetDepart  : TDGCIDSommet;
  SommetArrivee : TDGCIDSommet;
  Poids         : double;
  // autres valeurs: dx, dy, dz, etc ...
  //X       : double;
  //Y       : double;
end;
type TDGCListeDesSommets = class(TDGCListeSimple<TDGCGrapheSommet>)
  private
  public
end;

type TDGCListeDesArcs = class(TDGCListeSimple<TDGCGrapheArc>)
  private
  public
end;
type TDGCListeDesPredecesseurs = class(TDGCListeSimple<TDGCIDSommet>)
  private
  public
end;
type

{ TDGCGraphe }

 TDGCGraphe = class
  private
    FListeSommets: TDGCListeDesSommets;
    FListeArcs   : TDGCListeDesArcs;
    // liste des points de passages pour l'algo de recherche de plus court chemin
    FListePointsPassage: TDGCListeDesPredecesseurs;
    function CalcPlusCourtChemin(const S1, S2: TDGCIDSommet): double;
    function FindIdxArcByS1S2(const S1, S2: TDGCIDSommet): TDGCIDArc;
    function FindIdxSommetDistanceMini(): TDGCIDSommet;
  public
    function  Initialiser(): boolean;
    procedure Finaliser();
    procedure ClearAll();
    function  GetNbSommets(): integer;
    function  GetNbArcs(): integer;
    procedure AddSommet(const S: TDGCGrapheSommet); overload;
    procedure AddSommet(const NS: TDGCIDSommet; const QX, QY: double); overload;


    function  GetSommet(const Idx: integer): TDGCGrapheSommet;
    procedure SetSommet(const Idx: integer; const S: TDGCGrapheSommet);


    procedure AddArc(const A: TDGCGrapheArc); overload;
    procedure AddArc(const QIDArc : TDGCIDArc; const QSommetDepart, QSommetArrivee: TDGCIDSommet; const QPoids: double); overload;
    procedure AddArcAvecPoidsAutoCalcule(const QIDArc : TDGCIDArc; const QSommetDepart, QSommetArrivee: TDGCIDSommet);

    function  GetArc(const Idx: integer): TDGCGrapheArc;
    procedure SetArc(const Idx: integer; const A: TDGCGrapheArc);

    procedure RecenserListesAdjacences();

end;


implementation
uses
  mainform,
  DGCDummyUnit; // anti bug 'Fin du code source non trouvée';

const QINFINI: double = 1E24;
{ TDGCGrapheSommet }
procedure AfficherMessage(const s: string);
begin
  Form1.Memo1.Lines.add(s);
end;

procedure TDGCGrapheSommet.AddArcVoisin(const A: TDGCIDArc);
var
  n: Integer;
begin
  n := length(ArcsVoisins);
  SetLength(ArcsVoisins, n+1);
  ArcsVoisins[n] := A;
end;

function TDGCGrapheSommet.GetArcVoisin(const idx: integer): TDGCIDArc;
begin
  Result := ArcsVoisins[idx];
end;
function TDGCGrapheSommet.GetArcVoisinByID(const idx: TDGCIDArc): TDGCIDArc;
var
  n, i: Integer;
begin
  result := -1;
  n := Length(ArcsVoisins);
  if (0 = n) then exit;
  for i := 0 to n - 1 do if (idx = ArcsVoisins[i]) then exit(i);
end;

procedure TDGCGrapheSommet.SetArcVoisin(const idx: integer; const A: TDGCIDArc);
begin
  ArcsVoisins[idx] := A;
end;

{ TDGCGraphe }

function TDGCGraphe.Initialiser(): boolean;
begin
  Result := false;
  try
    FListeSommets       := TDGCListeDesSommets.Create;
    FListeArcs          := TDGCListeDesArcs.Create;
    FListePointsPassage := TDGCListeDesPredecesseurs.Create;
    ClearAll();
    Result := True;
  except
  end;
end;

procedure TDGCGraphe.Finaliser();
begin
   try
     ClearAll();
   finally
     FreeAndNil(FListeSommets);
     FreeAndNil(FListeArcs);
     FreeAndNil(FListePointsPassage);
   end;
end;

procedure TDGCGraphe.ClearAll();
begin
  FListeArcs.ClearListe();
  FListeSommets.ClearListe();
  FListePointsPassage.ClearListe();
end;

function TDGCGraphe.GetNbSommets(): integer;
begin
  Result := FListeSommets.GetNbElements();
end;

function TDGCGraphe.GetNbArcs(): integer;
begin
  Result := FListeArcs.GetNbElements();
end;

procedure TDGCGraphe.AddSommet(const S: TDGCGrapheSommet);
begin
  FListeSommets.AddElement(S);
end;

procedure TDGCGraphe.AddSommet(const NS: TDGCIDSommet; const QX, QY: double);
var
  S: TDGCGrapheSommet;
begin
  if (NS = -1) then S.IDSommet := Self.GetNbSommets() else S.IDSommet := NS;
  S.X    := QX;
  S.Y    := QY;
  S.Flag := -1;
  // init pour le Dijkstra
  if (0 = FListeSommets.GetNbElements()) then S.Distance := 0.00 else S.Distance := QINFINI;
  SetLength(S.ArcsVoisins, 0);
  FListeSommets.AddElement(S);
end;

function TDGCGraphe.GetSommet(const Idx: integer): TDGCGrapheSommet;
begin
  Result := FListeSommets.GetElement(Idx);
end;

procedure TDGCGraphe.SetSommet(const Idx: integer; const S: TDGCGrapheSommet);
begin
  FListeSommets.PutElement(Idx, S);
end;

procedure TDGCGraphe.AddArc(const A: TDGCGrapheArc);
begin
  FListeArcs.AddElement(A);
end;

procedure TDGCGraphe.AddArc(const QIDArc: TDGCIDArc; const QSommetDepart, QSommetArrivee: TDGCIDSommet; const QPoids: double);
var
  A: TDGCGrapheArc;
begin
  if (QIDArc = -1) then A.IDArc := FListeArcs.GetNbElements() else A.IDArc := QIDArc;
  A.SommetDepart  := QSommetDepart;
  A.SommetArrivee := QSommetArrivee;
  A.Poids         := QPoids;
  FListeArcs.AddElement(A);
end;

procedure TDGCGraphe.AddArcAvecPoidsAutoCalcule(const QIDArc: TDGCIDArc; const QSommetDepart, QSommetArrivee: TDGCIDSommet);
var
  A: TDGCGrapheArc;
  S1, S2: TDGCGrapheSommet;
begin
  if (QIDArc = -1) then A.IDArc := FListeArcs.GetNbElements() else A.IDArc := QIDArc;

  A.SommetDepart  := QSommetDepart;
  A.SommetArrivee := QSommetArrivee;
  S1 := GetSommet(A.SommetDepart);
  S2 := GetSommet(A.SommetArrivee);
  A.Poids         := Hypot(S2.X - S1.X, S2.Y - S1.Y);
  FListeArcs.AddElement(A);
end;

function TDGCGraphe.GetArc(const Idx: integer): TDGCGrapheArc;
begin
  Result := FListeArcs.GetElement(Idx);
end;

procedure TDGCGraphe.SetArc(const Idx: integer; const A: TDGCGrapheArc);
begin
  FListeArcs.PutElement(Idx, A);
end;

function TDGCGraphe.FindIdxArcByS1S2(const S1, S2: TDGCIDSommet): TDGCIDArc;
var
  n, i: Integer;
  A: TDGCGrapheArc;
begin
  Result := -1;
  n := self.GetNbArcs();
  for i := 0 to n - 1 do
  begin
    A := GetArc(i);
    //if ((S1 = A.SommetDepart) and (S2 = A.SommetArrivee)) then exit (A.IDArc); // graphes orientés
    if ((S1 = Min(A.SommetDepart, A.SommetArrivee)) and
        (S2 = Max(A.SommetDepart, A.SommetArrivee))) then exit (A.IDArc); // graphes non orientés

  end;
end;

procedure TDGCGraphe.RecenserListesAdjacences();
var
  NbSommets, NbArcs, s, a: Integer;
  MySommet: TDGCGrapheSommet;
  MyArc: TDGCGrapheArc;
begin
  NbSommets := GetNbSommets();
  NbArcs    := GetNbArcs();
  AfficherMessage(Format('%s.RecenserListesAdjacences: %d sommets, %d arcs',[classname, NbSommets, NbArcs]));

  for s := 0 to NbSommets - 1 do
  begin
    MySommet := GetSommet(s);
    for a := 0 to NbArcs - 1 do
    begin
      MyArc := GetArc(a);
      if (MySommet.IDSommet = MyArc.SommetDepart)  then MySommet.AddArcVoisin(-MyArc.IDArc);
      if (MySommet.IDSommet = MyArc.SommetArrivee) then MySommet.AddArcVoisin( MyArc.IDArc);
    end;
    SetSommet(s, MySommet);
  end;
end;

//Trouve_min(Q)
function TDGCGraphe.FindIdxSommetDistanceMini(): TDGCIDSommet;
var
  S: TDGCGrapheSommet;
  QMini: Double;
  n, i: Integer;
begin
  QMini := QINFINI;
  Result := -1;
  n := GetNbSommets;
  for i := 0 to n - 1 do
  begin
    S := GetSommet(i);
    if (S.Distance < QMini) then
    begin
      QMini := S.Distance;
      Result := i;
    end;
  end;
end;

function TDGCGraphe.CalcPlusCourtChemin(const S1, S2: TDGCIDSommet): double;
  procedure ReinitDijkstra();
  var
    n, i: Integer;
    S: TDGCGrapheSommet;
  begin
    n := GetNbSommets();
    S := GetSommet(0);
    S.Distance := 0.00;
    SetSommet(0, S);
    for i := 1 to n-1 do
    begin
      S := GetSommet(i);
      S.Distance := QINFINI;
      SetSommet(i, S);
    end;
  end;
begin
  Result := 0.00;
  FListePointsPassage.ClearListe();
  ReinitDijkstra();
  FListePointsPassage.AddElement(S1);
end;

end.



Mise à jour des distances

    On met à jour les distances entre s d e b {\displaystyle s_{deb}} s_{{deb}} et s 2 {\displaystyle s_{2}} s_{{2}} en se posant la question : vaut-il mieux passer par s 1 {\displaystyle s_{1}} s_{{1}} ou pas ?

maj_distances(s1,s2)
1 si d[s2] > d[s1] + Poids(s1,s2)      /* Si la distance de sdeb à s2 est plus grande que */
2                                      /* celle de sdeb à S1 plus celle de S1 à S2 */
3    alors
4        d[s2] := d[s1] + Poids(s1,s2) /* On prend ce nouveau chemin qui est plus court */
5        prédécesseur[s2] := s1        /* En notant par où on passe */




end.

