// Nouveau moteur de calcul par analyse de graphes
unit unitAnalyseReseau;

{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  ToporobotClasses2012,
  UnitListesSimplesWithGeneriques,
  Common, UnitObjetSerie,
  Classes, SysUtils;
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


type

{ TGrapheDuReseau }

 TGrapheDuReseau = class
  private
    FDocTopo: TToporobotStructure2012;
    FListeDesSommets: TListeSimple<TSommetGraphe>;
    FListeDesArcs   : TListeSimple<TArcGraphe>;


    function FindArcsByIDSommets(const E1, E2: TIDSommet; out ARR: TArrayArcGraphe): integer;
    procedure PutArc(const Idx: integer; const S: TArcGraphe);
    function RecenserSommetsEtArcs(): boolean;


    procedure TrierLesSommets();

    procedure AddArc(const S: TArcGraphe);
    function  getArc(const Idx: integer): TArcGraphe;
    function  getNbArcs(): integer;
    function  setMarkedArc(const Idx: integer; const B: boolean): boolean;
  public
    function Initialiser(const FD: TToporobotStructure2012): boolean;
    procedure Finaliser();

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
  FListeDesArcs.AddElement(P);
end;


function TGrapheDuReseau.getArc(const Idx: integer): TArcGraphe;
begin
  result := FListeDesArcs.GetElement(Idx);
end;

function TGrapheDuReseau.getNbArcs(): integer;
begin
  result := FListeDesArcs.GetNbElements();
end;

procedure TGrapheDuReseau.PutArc(const Idx: integer; const S: TArcGraphe);
begin
  FListeDesArcs.PutElement(Idx, S);
end;

function TGrapheDuReseau.setMarkedArc(const Idx: integer; const B: boolean): boolean;
var
  P: TArcGraphe;
begin
  Result := false;
  P := getArc(Idx);
  P.IsVisite := B;
  PutArc(Idx, P);
end;
function TGrapheDuReseau.FindArcsByIDSommets(const E1, E2: TIDSommet; out ARR: TArrayArcGraphe): integer;
var
  A: TArcGraphe;
  NbFound, n, i: Integer;
begin
  Result := 0;
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
end;

// sommets
procedure TGrapheDuReseau.AddSommet(const S: TSommetGraphe; const DoSort: boolean);
var
  P, Q: TSommetGraphe;
  i, QIdxInternal: Integer;

begin
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
end;




function TGrapheDuReseau.getNbSommets: integer;
begin
  result := FListeDesSommets.GetNbElements();
end;

function TGrapheDuReseau.getSommet(const Idx: integer): TSommetGraphe;
begin
  Result := FListeDesSommets.GetElement(Idx);
end;

function TGrapheDuReseau.getSommetByIdSommet(const Idx: TIDSommet): TSommetGraphe;
var
  BP: TSommetGraphe;
  QIdxInternal: integer;
begin
  if (FindSommetByIndex(Idx, BP, QIdxInternal)) then exit(BP)              // Rappel: exit() est l'équivalent d'un return()
                                                else exit(getSommet(0));
end;

procedure TGrapheDuReseau.PutSommet(const Idx: integer; const S: TSommetGraphe);
begin
  FListeDesSommets.PutElement(Idx, S);
end;

function TGrapheDuReseau.AddSuccesseurAtSommetByID(const Idx: TIDSommet; const IDSuccesseur: TIDSommet): boolean;
var
  BP: TSommetGraphe;
  QIdxInternal: integer;
  n: Byte;
begin
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
end;
function TGrapheDuReseau.AddPredecesseurAtSommetByID(const Idx: TIDSommet; const IDPredecesseur: TIDSommet): boolean;
var
  BP: TSommetGraphe;
  QIdxInternal: integer;
  n: Byte;
begin
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
end;
function TGrapheDuReseau.SetMarkedSommetByID(const Idx: TIDSommet; const B: Boolean): boolean;
var
  BP: TSommetGraphe;
  QIdxInternal: integer;
begin
  Result := FindSommetByIndex(Idx, BP, QIdxInternal);
  if (Result) then
  begin
    BP.IsVisited := B;
    PutSommet(QIdxInternal, BP);
    Result := True;
  end;
end;


//******************************************************************************
procedure TGrapheDuReseau.Finaliser;
begin
  AfficherMessage(Format('%s.Finaliser()', [ClassName]));
  try
    FListeDesSommets.ClearListe();
    FListeDesArcs.ClearListe();
  finally
    FreeAndNil(FListeDesSommets);//FListeDesSommets.Free;
    FreeAndNil(FListeDesArcs);//FListeDesArcs.Free;
  end;
end;

function TGrapheDuReseau.Initialiser(const FD: TToporobotStructure2012): boolean;
begin
  Result := false;
  AfficherMessage(Format('%s.Initialiser()', [ClassName]));
  FDocTopo := FD;
  FListeDesSommets := TListeSimple<TSommetGraphe>.Create;
  FListeDesArcs    := TListeSimple<TArcGraphe>.Create;
  try
    FreeAndNil(FListeDesSommets);//FListeDesSommets.ClearListe();
    FreeAndNil(FListeDesArcs);//FListeDesArcs.ClearListe();
    result := True;
  finally
  end;
end;

procedure TGrapheDuReseau.PrintSommet(const IdxSommet: TIDSommet);
var
  BP: TSommetGraphe;
  QIdxInternal, i: integer;
  n: Byte;
  WU: String;
begin
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
end;


function TGrapheDuReseau.RecenserSommetsEtArcs(): boolean;
var
  i, n , p, v, QIdxInternal: Integer;
  EE: TEntrance;
  MySommet, BP, S0, S1: TSommetGraphe;
  MyArc: TArcGraphe;
  SR: TObjSerie;
  MyVisee: TUneVisee;
  Q: Boolean;
  WU: String;
begin
  result := false;
  WU := format('%s.RecenserSommetsEtArcs()', [ClassName]);
  AfficherMessage(WU);
  AfficherMessageErreur(WU);
  FListeDesSommets.ClearListe();
  FListeDesArcs.ClearListe();
  // premier sommet
  MySommet.Etiquette := Format(FMTSERST, [1, 0]);
  MySommet.IDSommet  := SER_MULT * 1 + 0;
  //MyArc.SommetDepart  := 0;
  //MyArc.SommetArrivee := 1;
  //MyArc.Etiquette     := '1.0';
  AddSommet(MySommet, true);
  // d'abord les entrées
  n := FDocTopo.GetNbEntrees();
  for i := 0 to n - 1 do
  begin
    EE := FDocTopo.GetEntree(i);
    MySommet.IDSommet := SER_MULT * EE.eRefSer + EE.eRefSt;
    MySommet.Etiquette := Format('Entrance %d.%d', [EE.eRefSer, EE.eRefSt]);

    MyArc.SommetDepart := 0;
    Q := FindSommetByIndex(MySommet.IDSommet, BP, QIdxInternal);
    AddSommet(MySommet, true);
    Q := FindSommetByIndex(MySommet.IDSommet, BP, QIdxInternal);
    if (Q) then
    begin
      MyArc.SommetArrivee := BP.IdxInterne;
      MyArc.Etiquette := Format('Entrance %d', [i]);
      AddArc(MyArc);
    end;
  end;
  // ensuite, les points topo
  n := FDocTopo.GetNbSeries();
  AfficherMessage(inttostr(n));
  for i := 1 to n - 1 do
  begin

    SR := FDocTopo.GetSerie(i);
    p := SR.GetNbVisees();
    Q := FindSommetBySerPt(SR.GetNoSerieDep(), SR.GetNoPointDep(), BP, QIdxInternal);
    MySommet.IDSommet := SER_MULT * SR.GetNoSerieDep() + SR.GetNoPointDep();
    MySommet.Etiquette := Format('%d.%d [Start serie %d]', [SR.GetNoSerieDep(), SR.GetNoPointDep, SR.GetNumeroDeSerie()]);
    AddSommet(MySommet, true);
    MyArc.SommetDepart := BP.IdxInterne;
    if (p > 0) then
    begin
      for v := 1 to p - 1 do
      begin
        MyVisee := SR.GetVisee(v);
        if (v = p-1) then  // dernier point de la série ?
        begin
          MySommet.IDSommet := SER_MULT * SR.GetNoSerieArr() + SR.GetNoPointArr();
          MySommet.Etiquette := Format('%d.%d [End serie]', [SR.GetNumeroDeSerie(), SR.GetNoPointArr()]);
          AddSommet(MySommet, True);
          Q := FindSommetByIndex(MySommet.IDSommet, BP, QIdxInternal);
          if (Q) then
          begin
            MyArc.SommetArrivee := BP.IdxInterne;
            MyArc.Etiquette := 'Visee terminale of serie';
            AddArc(MyArc);
            MyArc.SommetDepart := MyArc.SommetArrivee;
          end;
        end
        else
        begin
          MySommet.IDSommet := SER_MULT * SR.GetNumeroDeSerie() + v;
          MySommet.Etiquette := Format('%d.%d', [SR.GetNumeroDeSerie(), v]);
          AddSommet(MySommet, True);
          Q := FindSommetByIndex(MySommet.IDSommet, BP, QIdxInternal);
          if (Q) then
          begin
            MyArc.SommetArrivee := BP.IdxInterne;
            MyArc.Etiquette := 'Visee';
            AddArc(MyArc);
            MyArc.SommetDepart := MyArc.SommetArrivee;
          end;
        end;
      end;
    end;
  end;
  // contrôle
  n := getNbSommets();
  AfficherMessageErreur(format('Liste des %d sommets', [n]));
  // tri des sommets
  for i := 0 to n - 1 do
  begin
    MySommet := getSommet(i);
    AfficherMessageErreur(Format('Sommet %d, %d, %d, %s', [i, MySommet.IdxInterne, MySommet.IDSommet, MySommet.Etiquette]));
  end;

  // contrôle
  n := getNbArcs();
  AfficherMessageErreur(format('Liste des %d arcs', [n]));
  for i := 0 to n - 1 do
  begin
    MyArc := getArc(i);
    AfficherMessage(format('%d -> %d', [MyArc.SommetDepart, MyArc.SommetArrivee]));
    S0 := getSommet(MyArc.SommetDepart);
    S1 := getSommet(MyArc.SommetArrivee);
    AfficherMessageErreur(Format('Arc %d, S1%d, %d, %s, ->, S%d, %d, %s), "%s"',
                                 [i,
                                  MyArc.SommetDepart, S0.IDSommet, S0.Etiquette,
                                  MyArc.SommetArrivee, S1.IDSommet, S1.Etiquette,
                                  MyArc.Etiquette]));
  end;
end;



procedure TGrapheDuReseau.Traiter();
begin
  RecenserSommetsEtArcs();
end;

procedure TGrapheDuReseau.TrierLesSommets;
begin
  FListeDesSommets.Sort(SortSommetsCriteria());
end;
//******************************************************************************
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

function TGrapheDuReseau.FindSommetBySerPt(const Ser, Pt: integer; out BP: TSommetGraphe; out IdxInternal: integer): boolean; inline;
begin
  Result := FindSommetByIndex(SER_MULT * Ser  + Pt, BP, IdxInternal);
  //AfficherMessageErreur(Format('[%s] %d.%d = %d - %s', [BoolToStr(Result, 'OK', 'KO'), Ser, Pt, BP.IDSommet, BP.Etiquette]));
end;

end.

