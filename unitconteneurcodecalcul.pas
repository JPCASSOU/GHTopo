unit unitConteneurCodeCalcul;
{.$ERROR Vérifier la concordance des unités angulaires pour les entrées}
// Conteneur commun au code de calcul et à l'analyse de graphe
// 13/06/2019: Point de contrôle temporel (contrôle de version)
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  {$IFDEF MULTI_THREADING}
   {$ERROR: Multithreading inadapté}
  {$ENDIF MULTI_THREADING}
  UnitListesSimplesWithGeneriques,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitClassPalette,
  Common, UnitObjetSerie,
  Classes, SysUtils, math, Graphics,
  frmDisplayMultiThreadProcessings, // pour le multithread
  Forms; // pour Application  ;

type

{ TConteneurCodeCalcul }

  TConteneurCodeCalcul = class
  strict private
    procedure ProcessBranche(const NoThread, Idx: integer);
  private  // visible uniquement dans la classe et non ds les classes héritées
    // callback pour suivi de progression
    FTableJonctions     : TTableJonctionsXYZ;
    FTableBranches      : TTableBranchesXYZ;

    procedure AfficherProgressionMonoThread(const Etape: string; const Done, Starting, Ending, Step: integer);

  protected // = visible dans les classes héritées
    FProcDispProgressionMonoThread: TProcDisplayProgression;
    FDlgMultithreadProcessing: TdlgDispMultiThreadProcessing;
    FDocTopo          : TToporobotStructure2012;
    FBDDEntites       : TBDDEntites;

    FPalette256       : TPalette256;
    FCouleurZMini     : TColor;
    FCouleurZMaxi     : TColor;
  public
    function  Initialiser(const FD: TToporobotStructure2012; const BDE: TBDDEntites): boolean;
    procedure Finaliser();
    procedure ViderTableJonctionsBranches();
    // version Noeuds et Branches
    procedure AddJunction(const Jnct: TJonctionXYZ);
    function  GetJonction(const NoJonction: integer): TJonctionXYZ;
    function  GetIdxJonctionBySerSt(const Serie, Station: integer): integer;
    function  GetNbJonctions(): integer;
    function  RecenserJonctions(): integer;
    procedure RecenserBranches();
    // gestions des branches (version moins 'propre' que les versions Python et Java, mais largement suffisante. A remplacer mais non prioritaire)
    procedure AddBranche(var LaBranche: TBrancheXYZ);
    procedure RemoveBranche(const NumBrch: integer);
    procedure PutBranche(const NoBr: integer; const LaBranche: TBrancheXYZ);
    function  GetBranche(const Idx: integer): TBrancheXYZ;
    function  GetNbBranches(): integer;

    procedure addViseeAtBranche(var BR: TBrancheXYZ; const V: TUneVisee);
    procedure CalculerAccroissements();
end;
implementation
uses
  DGCDummyUnit,
  Dialogs;
//******************************************************************************


function TConteneurCodeCalcul.Initialiser(const FD: TToporobotStructure2012;
                                          const BDE: TBDDEntites): boolean;
var
  WU, EWE: String;
begin
  Result := false;
  AfficherMessage(Format('%s.Initialiser()', [ClassName]));
  WU := {$I %DATE%} + ' ' + {$I %TIME%};
  EWE := Format('%s Compiler - Version: %s compiled %s', [ExtractFileName(Application.ExeName), GetGHTopoVersion(), WU]);
  AfficherMessage(EWE);
  AfficherMessageErreur(EWE);
  FDocTopo    := FD;
  FBDDEntites := BDE;
  FCouleurZMini := FBDDEntites.GetColorZMini();
  FCouleurZMaxi := FBDDEntites.GetColorZMaxi();
  FPalette256 := TPalette256.Create;
  FPalette256.GenerateTOPOROBOTPalette();
  // création des tables
  FTableJonctions := TTableJonctionsXYZ.Create;
  FTableBranches  := TTableBranchesXYZ.Create;
  ViderTableJonctionsBranches();
  // dialogue de progression
  FDlgMultithreadProcessing := TdlgDispMultiThreadProcessing.Create(Application);
  if (FDlgMultithreadProcessing.Initialiser(4)) then // nombre de threads: 4
  begin
    FProcDispProgressionMonoThread := FDlgMultithreadProcessing.AfficherProgressionOfMonoThread;
    FDlgMultithreadProcessing.Show;
    Application.ProcessMessages;
  end;
  result := True;
end;
procedure TConteneurCodeCalcul.Finaliser;
begin
  AfficherMessage(Format('%s.Finaliser()', [ClassName]));
  try
    ViderTableJonctionsBranches();
    FPalette256.Finaliser();
    FDlgMultithreadProcessing.Finaliser();
  finally
    FreeAndNil(FPalette256);
    FreeAndNil(FTableJonctions);//FTableJonctions.Free;
    FreeAndNil(FTableBranches);//FTableBranches.Free;
    FreeAndNil(FDlgMultithreadProcessing);
  end;
end;

procedure TConteneurCodeCalcul.AfficherProgressionMonoThread(const Etape: string; const Done, Starting, Ending, Step: integer);
begin
  if (0 = Done mod Step) then Exit;
  FDlgMultithreadProcessing.AfficherProgressionOfMonoThread(Etape, Done, Starting, Ending, Step);
end;

//******************************************************************************
// méthodes relatives aux jonctions
function TConteneurCodeCalcul.GetJonction(const NoJonction: integer): TJonctionXYZ;
begin
  Result := FTableJonctions.GetElement(NoJonction);
end;

function TConteneurCodeCalcul.GetNbJonctions: integer;
begin
  Result := FTableJonctions.GetNbElements();
end;

procedure TConteneurCodeCalcul.AddJunction(const Jnct: TJonctionXYZ);
begin
  FTableJonctions.AddElement(Jnct);
end;
//*************************************************************
// recherche de toutes les entrées, jonctions et culs de sacs
function TConteneurCodeCalcul.RecenserJonctions(): integer;
var
  Entree : TEntrance;
  Serie  : TObjSerie;
  Ser, St: integer;
  ListeJnct: TStringList;
  J    : TJonctionXYZ;
  WU: TGHStringArray;
begin

  AfficherMessage(Format('%s.RecenserJonctions',[self.ClassName]));
  if (Assigned(self.FProcDispProgressionMonoThread)) then self.FProcDispProgressionMonoThread('Recensement des jonctions', -1, -1, -1, 100);
  result := -1;
  AfficherMessage(GetResourceString(rsRECENSEM_JONC));
  ListeJnct := TStringList.Create;
  try
    ListeJnct.Clear;
    ListeJnct.Sorted     := True;
    ListeJnct.Duplicates := dupIgnore;
    // Les coordonnées d'accrochage des entrées de cavités sont forcément des jonctions
    AfficherMessage(Format('-- %d entrances', [FDocTopo.GetNbEntrances()]));
    for Ser:=0 to FDocTopo.GetNbEntrances() - 1 do
    begin
      Entree := FDocTopo.GetEntrance(Ser);
      ListeJnct.Add(Format(FMT_NDSER_PT,[Entree.eRefSer,Entree.eRefSt]));
    end; //*)
    AfficherMessage(Format('-- %d series', [FDocTopo.GetNbSeries]));
    // Les série/point de départ/arrivée sont forcément des jonctions (noeuds)
    for Ser:=0 to FDocTopo.GetNbSeries - 1 do
    begin
      Serie := FDocTopo.GetSerie(Ser);
      ListeJnct.Add(Format(FMT_NDSER_PT,[Serie.GetNoSerieDep,  Serie.GetNoPointDep]));
      ListeJnct.Add(Format(FMT_NDSER_PT,[Serie.GetNoSerieArr,  Serie.GetNoPointArr]));
    end;
    // Les départs de visées en antenne sont aussi des jonctions
    // en interne, l'ID de noeud est un littéral
    for St := 0 to ListeJnct.Count - 1 do
    begin
      J.IDJonction := ListeJnct.Strings[St];
      J.NoNoeud    := St;
      WU := Split(J.IDJonction, '-');
      J.NoSer      := StrToIntDef(WU[0], -1);
      J.NoSt       := StrToIntDef(WU[1], -1);
      J.NbRefs     := 0;
      AddJunction(J);
    end;
    Application.ProcessMessages;
    // On passe ici ? OK, c'est bon
    Result := GetNbJonctions();
    AfficherMessage(format('-- > %d junctions', [Result]));
  finally
    // on libère la table provisoire
    FreeAndNil(ListeJnct);//ListeJnct.Free;
  end;
end;

procedure TConteneurCodeCalcul.AddBranche(var LaBranche: TBrancheXYZ);
var
  UneVisee   : TUneVisee;
begin
  with FTableBranches do
  begin
    // créer la table des stations
    setlength(LaBranche.PointsTopo, 0);        //LaBranche.PointsTopo := TListePointsTopo.Create;
    UneVisee := EmptyVisee('');
    addViseeAtBranche(LaBranche, UneVisee); //LaBranche.PointsTopo.AddElement(UneVisee);
    FTableBranches.AddElement(LaBranche);
  end;
end;
function TConteneurCodeCalcul.GetBranche(const Idx: integer): TBrancheXYZ;
begin
  Result := FTableBranches.GetElement(Idx);
end;

procedure TConteneurCodeCalcul.RemoveBranche(const NumBrch: integer);   // OK
begin
  FTableBranches.RemoveElement(NumBrch);
end;

procedure TConteneurCodeCalcul.ViderTableJonctionsBranches;
begin
  try
    AfficherMessage(format('%s.ViderTableJonctionsBranches()', [ClassName]));
    FTableJonctions.ClearListe();
    FTableBranches.ClearListe();
  except
  end;
end;

procedure TConteneurCodeCalcul.PutBranche(const NoBr: integer; const LaBranche: TBrancheXYZ);
begin
  FTableBranches.PutElement(NoBr, LaBranche);
end;

procedure TConteneurCodeCalcul.addViseeAtBranche(var BR: TBrancheXYZ; const V: TUneVisee);
var
  n: Integer;
begin
  // Il n'y a pas d'opérations de suppression de visées dans un TBrancheXYZ
  // Un tableau Pascal suffit pour contenir les visées: il peut être
  // agrandi par un SetLength, qui agit comme le 'REDIM PRESERVE' du Basic.
  n := Length(BR.PointsTopo);
  SetLength(BR.PointsTopo, n + 1);
  n := High(BR.PointsTopo);
  BR.PointsTopo[n] := V;
  BR.NbPoints := 1 + n;
end;

function TConteneurCodeCalcul.GetIdxJonctionBySerSt(const Serie, Station: integer): integer;
var
  i   : integer;
  Jonc: TJonctionXYZ;
begin
  Result := -1;
  for i := 0 to GetNbJonctions - 1 do
  begin
    Jonc := GetJonction(i);
    if ((Serie = Jonc.NoSer) and (Station = Jonc.NoSt)) then Exit(i);       // nouvelle méthode
  end;
end;


function TConteneurCodeCalcul.GetNbBranches(): integer;
begin
  Result := FTableBranches.GetNbElements();
end;

// découpage des séries en branches
procedure TConteneurCodeCalcul.RecenserBranches;
var
  Entr : TEntrance;
  Serie: TObjSerie;
  Visee: TUneVisee;
  Ser, Vis: integer;
  l1, a1, p1: double;
  Br   : integer;
  Nd   , QNbSeries, QNbVisees: integer;
  Branche0,
  Branche1, QBranche: TBrancheXYZ;
  QDeltaX: double;
  QDeltaY: double;
  QDeltaZ: double;
  QPosPtZero: TPoint3Df;
begin
  AfficherMessage(GetResourceString(rsFINDING_BRANCHES));
  //FTableBranches.ClearListe();
  // première branche
  with Branche0 do
  begin
    NoSerie     :=1;
    NoEntranceRatt := 0;
    NoReseau    :=0;
    NoBranche   :=1;
    NomBranche  := Format('Branche %d',[1]);
    NoeudDepart := 0;
    NoeudArrivee:= 1;
    Rigidite    := 1.00;
    SetLength(PointsTopo, 0);
    DeltaX:=0.01;
    DeltaY:=0.01;
    DeltaZ:=0.01;
  end;
  AddBranche(Branche0);
  Br := 2;
  //***********************
  // balayage des séries
  QNbSeries := FDocTopo.GetNbSeries();
  for Ser := 1 to QNbSeries - 1 do
  begin
    if (Assigned(self.FProcDispProgressionMonoThread)) then self.FProcDispProgressionMonoThread(Format('Découpage en arcs de la série %d / %d', [Ser, QNbSeries - 1]), Ser, 0 , QNbSeries - 1, 100);
    Serie := FDocTopo.GetSerie(Ser);
    Nd := GetIdxJonctionBySerSt(Serie.GetNoSerieDep, Serie.GetNoPointDep);
    // début de série = nouvelle branche
    //++++++++++++++++++++
    // Numéro de série = ID de la série
    Branche0.NoSerie        := Serie.GetNumeroDeSerie();
    Branche0.NoReseau       := Serie.GetNumeroReseau();    // Réseau
    Branche0.NoEntranceRatt := Serie.GetNumeroEntrance();
    Branche0.NoBranche      := Br;
    Branche0.NomBranche     := Format('Branche %d',[Br]);
    Branche0.NoeudDepart    := Nd;
    Branche0.Rigidite       := Serie.GetRaideur();
    AddBranche(Branche0);
    QNbVisees := Serie.GetNbVisees();
    for Vis := 1 to QNbVisees - 1 do
    begin
      Visee := Serie.GetVisee(Vis);
      Nd    := GetIdxJonctionBySerSt(Serie.GetNumeroDeSerie(), Vis);
      Visee.NoVisee := Vis;

      //Visee.TypeGalerie := 0; // type de galerie (provisoire)
      Branche1 := GetBranche(Br);                    //AddBrStation(Br, Visee);
      addViseeAtBranche(Branche1, Visee);             //Branche1 := QGetBranche(Br);
      PutBranche(Br, Branche1);

      Branche0.PointsTopo := Branche1.PointsTopo;
      Branche0.NbPoints   := 1 + High(Branche0.PointsTopo); // Branche1.NbPoints;
      //if ((Nd > -1) AND (Vis < Serie.GetNbVisees() - 1)) then
      if ((Nd > -1) AND (Vis < QNbVisees - 1)) then
      begin
        Branche0.NoeudArrivee := Nd;
        PutBranche(Br, Branche0);
        Inc(Br);

        Branche0.NoBranche   := Br;
        Branche0.NomBranche  := Format('Branche %d',[Br]);
        Branche0.NoeudDepart := Nd;
        AddBranche(Branche0);
      end;
      // fin de série = fin de branche: cloturer la branche
      if (Vis = QNbVisees - 1) then
      begin
        Nd := GetIdxJonctionBySerSt(Serie.GetNoSerieArr(), Serie.GetNoPointArr());
        Branche0.NoeudArrivee := Nd;
        PutBranche(Br, Branche0);
        Inc(Br);
      end;
    end;
  end;

  //**********************************************************
  // ajout des barres fictives (entrées de cavités)
  AfficherMessage(GetResourceString(rsADDENTRANCES));
  // La première entrée est déjà prise en compte
  for Ser := 0 to FDocTopo.GetNbEntrances() - 1 do
  begin   // initialement: for i:=1 to ...
    Entr := FDocTopo.GetEntrance(Ser);
    Branche0.NoSerie        := Entr.eRefSer;
    Branche0.NoEntranceRatt := 0;
    Branche0.NoReseau       := 0;
    Branche0.NoeudDepart    := GetIdxJonctionBySerSt(1,0);
    Branche0.NoeudArrivee   := GetIdxJonctionBySerSt(Entr.eRefSer, Entr.eRefSt);
    Branche0.Rigidite       := 1000.0;
    QPosPtZero              := FDocTopo.GetPositionDuPointZero();
    QDeltaX                 := Entr.eXEntree - QPosPtZero.X;
    QDeltaY                 := Entr.eYEntree - QPosPtZero.Y;
    QDeltaZ                 := Entr.eZEntree - QPosPtZero.Z;
    Branche0.DeltaX         := QDeltaX; //Entr.eDeltaX;
    Branche0.DeltaY         := QDeltaY; //Entr.eDeltaY;
    Branche0.DeltaZ         := QDeltaZ; //Entr.eDeltaZ;
    //AfficherMessage(Format('--> %d: %f %f %f',[Ser, Entr.eDeltaX, Entr.eDeltaY, Entr.eDeltaZ]));
    Visee.Code := 0; //1
    Visee.Expe := 0; //1
    l1 := 0.0;    a1 := 0.00;    p1 := 0.00;
    GetBearingInc(QDeltaX, QDeltaY, QDeltaZ, l1, a1, p1, UNITE_ANGULAIRE_DU_CODE_ZERO, UNITE_ANGULAIRE_DU_CODE_ZERO); // Bug avec 400 grades
    Visee.NoVisee  := 1;
    Visee.Longueur := l1;
    Visee.Azimut   := a1;
    Visee.Pente    := p1;
    Visee.LD       := 0.00;
    Visee.LG       := Visee.LD;
    Visee.HZ       := Visee.LD;
    Visee.HN       := Visee.LD;
    Visee.Commentaires     := '';
    Visee.IDTerrainStation := Format(FMTSERST,[Entr.eRefSer, Entr.eRefSt]);
    Visee.TypeVisee        := tgENTRANCE;
    Visee.Commentaires     := SafeTruncateString(Entr.eNomEntree, 15);
    AddBranche(Branche0);
    QBranche := GetBranche(Br);    //;;;
    addViseeAtBranche(QBranche, Visee);                                 //AddBrStation(Br, Visee);
    PutBranche(Br, QBranche);
    Inc(Br);
  end;
  //**********************************************************
  AfficherMessage(GetResourceString(rsFINDING_BRANCHES) + ' OK');
end;

// calcul des accroissements sur les trois axes des branches
procedure TConteneurCodeCalcul.ProcessBranche(const NoThread, Idx: integer);
var
  Branche: TBrancheXYZ;
  DX, DY, DZ, DP: double;
  Vs: Integer;
  Visee: TUneVisee;
begin
  //Branche:= GetBranche(Br);
  Branche:= GetBranche(Idx);
    DX := 0.0; DY := 0.0; DZ := 0.0; DP := 0.0;
    for Vs := 0 to High(Branche.PointsTopo) do //Branche.PointsTopo.GetNbElements() - 1 do
    begin
      Visee := Branche.PointsTopo[Vs];
      CalculerVisee(Visee,
                    FDocTopo.GetCodeByNumero(Visee.Code),
                    FDocTopo.GetExpeByNumero(Visee.Expe),
                    DX, DY,
                    1.00,
                    DZ, DP);
      Branche.PointsTopo[Vs] := Visee; ////PutBrStation(Br, Vs, Visee);
    end; //for Vs:=0 to Branche.PointsTopo.Count-1 do begin
    Branche.DeltaX := DX;
    Branche.DeltaY := DY;
    Branche.DeltaZ := DZ;
  //PutBranche(Br, Branche);
  PutBranche(Idx, Branche);
end;

procedure TConteneurCodeCalcul.CalculerAccroissements();
var
  t              : TDateTime;
  NbBranches, Br : integer;

  Branche: TBrancheXYZ;
  LLL: double;
  WU: Boolean;
  Vs: integer;
  Visee: TUneVisee;
  DX, DY, DZ, DP: double;
begin

  t := Now();
  NbBranches := GetNbBranches();
  AfficherMessage(format('%s.CalculerAccroissements(%d branches) [%s]', [ClassName, NbBranches, MULTI_OR_MONO_THREADED]));
  if (Assigned(self.FProcDispProgressionMonoThread)) then self.FProcDispProgressionMonoThread('Calcul accroissements des branches', -1, -1, -1, 100);
  for Br:=1 to NbBranches - 1 do ProcessBranche(-1, Br);
  //****************************************************************************
  // Travaux non multithreadables
  // Nettoyage des branches à accroissement nul
  // cause d'un bug très sournois
  for Br := GetNbBranches() - 1 downto 1 do
  begin
    Branche := GetBranche(Br);
    LLL := Hypot3D(Branche.DeltaX, Branche.DeltaY, Branche.DeltaZ);
    //     1. Ne pas supprimer la branche 0-1   2. Lonqueur considérée comme nulle
    WU := (Branche.NoeudArrivee > 1)           and IsZero(LLL);
    if (WU) then RemoveBranche(Br);
  end;

end;


end.
