// Nouveau moteur de calcul par analyse de graphes
unit unitAnalyseReseau2;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  UnitListesSimplesWithGeneriques,
  ToporobotClasses2012,
  unitConteneurCodeCalcul,
  Common, UnitObjetSerie,
  Classes, SysUtils, math,
  U_TYPE_GRAPHES, U_BASE_GRAPHES, U_GRAPHES,
  Forms; // pour Application  ;
const MaxSuccPredOfSommet = 16;

type TIDSommet = type Int64;


type TSommetGraphe = packed record
  IdxInterne     : integer;
  IDSommet       : TIDSommet;  // IDInterne = 1000 * NumeroSerie + NumeroStation
  IsVisited      : boolean;
  Successeurs    : array[0 .. MaxSuccPredOfSommet] of TIDSommet;
  NbSuccesseurs  : byte;
  Predecesseurs  : array[0 .. MaxSuccPredOfSommet] of TIDSommet;
  NbPredecesseurs: byte;
  Coordinates    : TPoint3Df;
  Etiquette      : string;
end;
type TArcGraphe = packed record
  SommetDepart   : integer;
  SommetArrivee  : integer;
  IsVisite       : boolean;
  Etiquette      : string;
end;
type TArrayArcGraphe = array of TArcGraphe;

//*)
type

{ TGrapheDuReseau }

 TGrapheDuReseau = class(TConteneurCodeCalcul)
  private
    //FListeDesSommets: TListeSimple<TSommetGraphe>;
    //FListeDesArcs   : TListeSimple<TArcGraphe>;

    FListesAdjacence: array of TListOfIntegers;



    //procedure addViseeAtBranche(var BR: TBrancheXYZ; const V: TUneVisee);
    //procedure CalculerAccroissements;
    function FindArcsByIDSommets(const E1, E2: TIDSommet; out ARR: TArrayArcGraphe): integer;

    procedure PutArc(const Idx: integer; const S: TArcGraphe);

    function  RecenserSommetsEtArcs(): boolean;


    procedure TrierLesSommets();

    procedure AddArc(const S: TArcGraphe);
    function  getArc(const Idx: integer): TArcGraphe;
    function  getNbArcs(): integer;
    function  setMarkedArc(const Idx: integer; const B: boolean): boolean;

    function  CreerListeAdjacence(): boolean;
    procedure ViderListeAdjacence();
  public
    function  PostInitialisation(): boolean;
    procedure PreFinalisation();

    procedure Traiter(); // provisoire, pour éviter allees-venues entre unités

    procedure AddSommet(const S: TSommetGraphe; const DoSort: boolean);
    function  getSommet(const Idx: integer): TSommetGraphe;
    function  getSommetByIdSommet(const Idx: TIDSommet): TSommetGraphe;
    function FindSommetByIndex(const Idx: TIDSommet; out BP: TSommetGraphe; out IdxInternal: integer): boolean;
    function FindSommetBySerPt(const Ser, Pt: integer; out BP: TSommetGraphe; out IdxInternal: integer): boolean; inline;

    function AddPredecesseurAtSommetByID(const Idx: TIDSommet; const IDPredecesseur: TIDSommet): boolean;
    function AddSuccesseurAtSommetByID(const Idx: TIDSommet; const IDSuccesseur: TIDSommet): boolean;
    function SetMarkedSommetByID(const Idx: TIDSommet; const B: Boolean): boolean;


    procedure PutSommet(const Idx: integer; const S: TSommetGraphe);
    function  getNbSommets(): integer;
    procedure PrintSommet(const IdxSommet: TIDSommet);
end;
implementation



{ TGrapheDuReseau }
const SER_MULT = 10000;

function SortSommetsCriteria(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TSommetGraphe;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.IDSommet < E2.IDSommet) then
    Result:=-1
  else if (E1.IDSommet = E2.IDSommet ) then
    Result:=0
  else
    Result:=1;
end;


// arcs
procedure TGrapheDuReseau.AddArc(const S: TArcGraphe);
var
  P: TArcGraphe;
begin
  P := S;
  P.IsVisite := false;
  //FListeDesArcs.AddElement(P);
end;


function TGrapheDuReseau.getArc(const Idx: integer): TArcGraphe;
begin
  //result := FListeDesArcs.GetElement(Idx);
end;

function TGrapheDuReseau.getNbArcs(): integer;
begin
  result := 0;//result := FListeDesArcs.GetNbElements();
end;

procedure TGrapheDuReseau.PutArc(const Idx: integer; const S: TArcGraphe);
begin
  //FListeDesArcs.PutElement(Idx, S);
end;

function TGrapheDuReseau.setMarkedArc(const Idx: integer; const B: boolean): boolean;
begin
  Result := false;
  (*
  Result := false;
  P := getArc(Idx);
  P.IsVisite := B;
  PutArc(Idx, P);
  //*)
end;
function TGrapheDuReseau.FindArcsByIDSommets(const E1, E2: TIDSommet; out ARR: TArrayArcGraphe): integer;
begin
  Result := 0;
  (*
  NbFound := 0;
  n := FListeDesArcs.Count;
  if (n = 0) then exit(0);
  SetLength(ARR, 0);
  for i := 0 to n -1 do
  begin
    A := getArc(i);
    if ((A.SommetDepart = E1) and (A.SommetArrivee = E2)) then
    begin
      NbFound += 1 ;
      SetLength(Arr, NbFound);
      Arr[NbFound - 1] := A;
    end;
  end;
  Result := length(ARR);
  //*)
end;

// sommets
procedure TGrapheDuReseau.AddSommet(const S: TSommetGraphe; const DoSort: boolean);
begin
  (*
  P := S;

  if (FindSommetByIndex(S.IDSommet, Q, QIdxInternal)) then Exit;   // anti-doublon
  P.Coordinates   := MakeTPoint3Df(0.0, 0.0, 0.0);
  P.IsVisited     := false;
  P.IdxInterne    := FListeDesSommets.Count;
  P.NbPredecesseurs := 0;
  P.NbSuccesseurs   := 0;
  for i := 0 to High(P.Predecesseurs) do P.Predecesseurs[i] := 0;
  for i := 0 to High(P.Successeurs)   do P.Successeurs[i]   := 0;

  FListeDesSommets.AddElement(P);
  if (DoSort) then FListeDesSommets.Sort(SortSommetsCriteria);// TrierLesSommets();
  //*)
end;




function TGrapheDuReseau.getNbSommets: integer;
begin
  Result := 0;
  //result := FListeDesSommets.GetNbElements();
end;

function TGrapheDuReseau.getSommet(const Idx: integer): TSommetGraphe;
begin
  //Result := FListeDesSommets.GetElement(Idx);
end;

function TGrapheDuReseau.getSommetByIdSommet(const Idx: TIDSommet): TSommetGraphe;
begin
  //if (FindSommetByIndex(Idx, BP, QIdxInternal)) then exit(BP)              // Rappel: exit() est l'équivalent d'un return()
  //                                              else exit(getSommet(0));
end;

function TGrapheDuReseau.PostInitialisation: boolean;
begin
  Result := false;
  try
    AfficherMessage(format('%s.PostInitialisation()', [ClassName]));
    (*
    FListeDesSommets := TListeSimple<TSommetGraphe>.Create;
    FListeDesArcs    := TListeSimple<TArcGraphe>.Create;
    FListeDesSommets.ClearListe();
    FListeDesArcs.ClearListe();
    //*)


    result := True;
  except
  end;
  //*)
end;

procedure TGrapheDuReseau.PreFinalisation;
begin
  AfficherMessage(format('%s.PreFinalisation()', [ClassName]));
  try

    //FListeDesSommets.ClearListe();
    //FListeDesArcs.ClearListe();
    ViderListeAdjacence();
  finally
    //FListeDesSommets.Free;
    //FListeDesArcs.Free;
  end;
  //*)
end;

procedure TGrapheDuReseau.PutSommet(const Idx: integer; const S: TSommetGraphe);
begin
  //FListeDesSommets.PutElement(Idx, S);
end;

function TGrapheDuReseau.AddSuccesseurAtSommetByID(const Idx: TIDSommet; const IDSuccesseur: TIDSommet): boolean;
begin
  Result := false;
  (*

  Result := FindSommetByIndex(Idx, BP, QIdxInternal);
  //try
    if (Result) then
    begin
      n := BP.NbSuccesseurs;
      if (n > MaxSuccPredOfSommet) then raise Exception.CreateFmt('Le nombre d''items (%d) est supérieur à la capacité de la liste (%d éléments)', [n, MaxSuccPredOfSommet]);
      BP.Successeurs[n] := IDSuccesseur;
      BP.NbSuccesseurs += 1;
      PutSommet(QIdxInternal, BP);
      Result := True;
    end;

  //except
  //end;
  //*)
end;

function TGrapheDuReseau.CreerListeAdjacence: boolean;
var
  i, NbSommets, NbArcs, nv, j: Integer;
  Branche: TBrancheXYZ;
  Jonction: TJonctionXYZ;
  SS: TListOfIntegers;
  WU: String;
begin
  result := False;
  AfficherMessage(Format('%s.CreerListeAdjacence()', [ClassName]));
  NbSommets := GetNbJonctions();
  NbArcs    := GetNbBranches();
  AfficherMessage(Format('-- Le graphe contient %d sommets et %d arcs', [NbSommets, NbArcs]));
  if (NbSommets <= 1) then exit(false);
  SetLength(FListesAdjacence, NbSommets);
  AfficherMessage('-- Liste des sommets OK');
  for i := 0 to NbSommets - 1 do
  begin
    FListesAdjacence[i] := TListOfIntegers.Create;
    FListesAdjacence[i].ClearListe();
  end;
  AfficherMessage('--> Recensement');
  // parcours des branches
  for i := 0 to NbSommets - 1 do
  begin
    Jonction := GetJonction(i);
    for j := 1 to NbArcs - 1 do
    begin
      Branche  := GetBranche(j);
      if (Jonction.NoNoeud = Branche.NoeudDepart) then FListesAdjacence[i].AddElement(Branche.NoeudDepart);
      if (Jonction.NoNoeud = Branche.NoeudArrivee) then FListesAdjacence[i].AddElement(Branche.NoeudArrivee);
    end;       ;
  end;

  // affichage
  AfficherMessage('--------> Graphe de liste');
  for i := 0 to NbSommets - 1 do
  begin
    SS := FListesAdjacence[i];
    nv := SS.GetNbElements();
    WU := BoolToStr(nv = 0, '<Aucun voisin>', 'Voisins: ');
    for j := 0 to nv - 1 do WU += Format('%d, ', [SS.GetElement(j)]);

    AfficherMessage(Format('--> %d: %s', [i, WU]));
  end;
  result := True;
end;
procedure TGrapheDuReseau.ViderListeAdjacence;
var
  i, n: Integer;
begin
  AfficherMessage(Format('%s.ViderListeAdjacence()', [ClassName]));
  n := Length(FListesAdjacence);
  if (n = 0) then Exit;
  for i := 0 to n-1 do
  begin
    try
      FListesAdjacence[i].ClearListe();
    finally
      FreeAndNil(FListesAdjacence[i]);//FListesAdjacence[i].Free;
    end;

  end;
  SetLength(FListesAdjacence, 0);
end;

function TGrapheDuReseau.AddPredecesseurAtSommetByID(const Idx: TIDSommet; const IDPredecesseur: TIDSommet): boolean;
begin
  Result := false;
  (*
  Result := FindSommetByIndex(Idx, BP, QIdxInternal);
  //try
    if (Result) then
    begin
      n := BP.NbPredecesseurs;
      if (n > MaxSuccPredOfSommet) then raise Exception.CreateFmt('Le nombre d''items (%d) est supérieur à la capacité de la liste (%d éléments)', [n, MaxSuccPredOfSommet]);
      BP.Predecesseurs[n] := IDPredecesseur;
      BP.NbPredecesseurs += 1;
      PutSommet(QIdxInternal, BP);
      Result := True;
    end;

  //except
  //end;
  //*)
end;
function TGrapheDuReseau.SetMarkedSommetByID(const Idx: TIDSommet; const B: Boolean): boolean;
begin
  Result := false;
  (*
  Result := FindSommetByIndex(Idx, BP, QIdxInternal);
  if (Result) then
  begin
    BP.IsVisited := B;
    PutSommet(QIdxInternal, BP);
    Result := True;
  end;
  //*)
end;





procedure TGrapheDuReseau.PrintSommet(const IdxSommet: TIDSommet);
begin
  (*
  if (FindSommetByIndex(IdxSommet, BP, QIdxInternal)) then
  begin
    AfficherMessageErreur(Format('Sommet: %d', [BP.IDSommet]));
    AfficherMessageErreur('-----------------------------');
    AfficherMessageErreur(Format('Index interne    : %d', [BP.IdxInterne]));
    AfficherMessageErreur(Format('Label            : %s', [BP.Etiquette]));
    AfficherMessageErreur(Format('Coordonnees      : %.3f, %.3f, %.3f', [BP.Coordinates.X, BP.Coordinates.Y, BP.Coordinates.Z]));
    AfficherMessageErreur(Format('Sommet visité    : %s', [BoolToStr(BP.IsVisited, 'Oui', 'Non')]));

    WU := '';
    n := BP.NbPredecesseurs;
    if (n > 0) then
       for i := 0 to n - 1 do WU += Format('%d ', [BP.Predecesseurs[i]]);

    AfficherMessageErreur(Format('Liste des %d predecesseurs: [%s]', [BP.NbPredecesseurs, WU]));
    WU := '';
    n := BP.NbSuccesseurs;
    if (n > 0) then
       for i := 0 to n - 1 do WU += Format('%d ', [BP.Successeurs[i]]);
    AfficherMessageErreur(Format('Liste des %d successeurs  : [%s]', [BP.NbSuccesseurs, WU]));
  end
  else
    AfficherMessageErreur(Format('Sommet %d introuvable', [IdxSommet]));
  //*)
end;


function TGrapheDuReseau.RecenserSommetsEtArcs(): boolean;
begin
  result := false;
  (*
  result := false;
  WU := format('%s.RecenserSommetsEtArcs()', [ClassName]);

  FListeDesSommets.ClearListe();
  FListeDesArcs.ClearListe();
  //*)
end;



procedure TGrapheDuReseau.Traiter();
var
  Branche: TBrancheXYZ;
  UneVisee: TUneVisee;
begin
  ViderTableJonctionsBranches();
  UneVisee := EmptyVisee('');
  SetLength(Branche.PointsTopo, 0);              // N
  addViseeAtBranche(Branche, UneVisee);          // N
  Branche.NoBranche    := 0;
  Branche.NomBranche   :='Point 0';
  Branche.NoeudDepart  := 0;
  Branche.NoeudArrivee := 0;
  AddBranche(Branche);
  RecenserJonctions();
  RecenserBranches();
  // construction du graphe ici ...
  AfficherMessage('*****************************');
  AfficherMessage('');
  // ...
  if (CreerListeAdjacence()) then
  begin

  end;
  ViderListeAdjacence();
end;

procedure TGrapheDuReseau.TrierLesSommets;
begin
  //FListeDesSommets.Sort(SortSommetsCriteria());
end;



//******************************************************************************
function TGrapheDuReseau.FindSommetByIndex(const Idx: TIDSommet; out BP: TSommetGraphe; out IdxInternal: integer): boolean;
begin

end;
{
function TGrapheDuReseau.FindSommetByIndex(const Idx: TIDSommet; out BP: TSommetGraphe; out IdxInternal: integer): boolean;
var
  i, n: Integer;
  // recherche par méthode récursive dichotomique
  function FindDepth(const I1, I2: integer; const QIDX: TIDSommet):  TIDSommet;
  var
    PVT: integer;
    C1: TSommetGraphe;
  begin
    PVT := (I2+I1) div 2;                                           // coupure en deux => calcul index médian
    if (I1 > I2) then exit(-1);                                     // début > fin >> sortie directe avec erreur
    C1 := getSommet(PVT);
    if (C1.IDSommet = QIDX) then exit(PVT);                         // comparaison. Si vrai >> sortie avec numéro d'index
    if (QIDX < C1.IDSommet) then exit(FindDepth(I1, PVT-1, QIDX));  // sinon, recherche en profondeur avec un niveau supplémentaire
    Result := FindDepth(PVT+1, I2, QIDX);
  end;
begin
  Result := false;
  IdxInternal := 0;
  i := FindDepth(0, getNbSommets() - 1, TIDSommet(Idx));
  if (i >= 0) then
  begin
    BP     := getSommet(i);
    IdxInternal := i;
    exit(True);
  end;
  //*)
  (*
  n := FListeDesSommets.Count;
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    BP     := getSommet(i);
    if (BP.IDSommet = Idx) then exit(True);
  end;
  //*)
end;
//*}

function TGrapheDuReseau.FindSommetBySerPt(const Ser, Pt: integer; out BP: TSommetGraphe; out IdxInternal: integer): boolean; inline;
begin
  //Result := FindSommetByIndex(SER_MULT * Ser  + Pt, BP, IdxInternal);
  //AfficherMessageErreur(Format('[%s] %d.%d = %d - %s', [BoolToStr(Result, 'OK', 'KO'), Ser, Pt, BP.IDSommet, BP.Etiquette]));
end;



end.

