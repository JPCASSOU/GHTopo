unit UnitGraphes1;
// Calcul du chemin minimal dans un réseau GHTopo
{$mode delphi}
{$DEFINE NEW_VERSION}
interface
uses
  StructuresDonnees,
  Classes, SysUtils, math, Graphics,
  LazFileUtils,
  Common,
  UnitObjetSerie,
  UnitListesSimplesWithGeneriques,
  ToporobotClasses2012,
  UnitEntitesExtended,
  BZGraphesTypes,
  BZGraphesClasses,
  BZArrayClasses
  ;

type

{ TPathBetweenNodes }

 TPathBetweenNodes = record
  NomItineraire : string;
  Color         : TColor;
  SerieDepart   : TNumeroSerie;
  StationDepart : TNumeroStation;
  SerieArrivee  : TNumeroSerie;
  StationArrivee: TNumeroStation;
  ListeNoeuds   : array of TNumeroNoeud;
  LongueurParcours: double;
  function Initialiser(const QSerDep: TNumeroSerie; const QStDep: TNumeroStation;
                       const QSerArr: TNumeroSerie; const QStArr: TNumeroStation;
                       const QNomItineraire: string;
                       const C: TColor): boolean;
  procedure EmptyListeNoeuds();
  procedure AddNoeud(const N: TNumeroNoeud);
  function  GetNoeud(const Idx: integer): TNumeroNoeud;
  function  GetNbNoeuds(): integer;
  function  StationDepartToTIDBaseStation(): TIDBaseStation;
  function  StationArriveeToTIDBaseStation(): TIDBaseStation;
  function  IsSameStationsDepartArrivee(): boolean;
end;
//******************************************************************************

type

{ TPathFindingGraphe }

 TPathFindingGraphe = class(TBZGraphNode)
  strict private

    FDocuTopo: TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FEtendue: TRect2Df;
    FListeDesPlusCourtsChemins: TListeSimple<TPathBetweenNodes>;
    FLastError: TGrapheLastError;
    FAfficherGraphe      : TProcOjObject;
    function SetLastError(const QErrCode: integer; const QErrMsg: string): boolean;
  private
    function   Reset(): boolean;
    function   FindNoeudByIDStation(const IDS: TIDBaseStation; out ST: TBZClassNode; out IndexOf: TNumeroNoeud): boolean;
    procedure  SetMinMax();                // étendue du réseau
    procedure  PurgerDoublons();           // on jette les doublons
    procedure  PutNoeud(const Idx: integer; const QNoeud: TBZClassNode);
  public
    constructor Create;
    destructor Destroy;
    property  DocuTopo: TToporobotStructure2012 read FDocuTopo;
    property  BDDEntites: TBDDEntites read FBDDEntites;

    function  Initialiser(const FD : TToporobotStructure2012; const FE: TBDDEntites): boolean;
    procedure Finaliser();
    function  ConstruireGraphe(): boolean;
    // Les utilitaires
    function  GetLastError(): TGrapheLastError;
    function  FormatterTIDStation(const QId: TIDBaseStation): string;
    procedure ListerLesNoeuds(const Caption: string; const DoDisplayDependances: boolean = false);
    // Les noeuds
    procedure DisplaySommetsGraphe();
    procedure  BeginNodeList();  // initialise la liste des noeuds
      // QSerie et QStation forment le label du sommet au format '%d.%d'
    procedure  AddStation(const QSerie, QStation: integer; const QPosition: TPoint3Df; const QMetaData: string);
    function   GetStation(const Idx: integer): TBZClassNode;
    function   GetNbStations(): integer;
    procedure  EndNodesList();  // clôture cette liste et effectue les traitements préparatoires (tris, ...)
    function   AddArcBetweenStations(const Ser1, St1, Ser2, St2: integer; const Bidirectionnel: boolean = true; const Weight: double = 1.00): boolean;
    function   RemoveNode(const Ser1, St1: integer): boolean;
    function   RemoveArcBetweenStations(const Ser1, St1, Ser2, St2: integer;  const Bidirectionnel: boolean): boolean;

    // spécifique stations topo
    function RechercherPlusCourtChemin(var MyPath: TPathBetweenNodes): boolean;

    function SaveToFile(const Filename: RawByteString): boolean;
    function FindStationByXY(const QX, QY: double; out BP: TBZClassNode): boolean;
    // liste des plus courts chemins
    procedure AddItineraire(const P: TPathBetweenNodes);
    function  GetItineraire(const Idx: integer; out P: TPathBetweenNodes): boolean;
    procedure PutItineraire(const Idx: integer; const P: TPathBetweenNodes);
    procedure RemoveItineraire(const Idx: integer);
    procedure ClearAllItineraires();
    function  GetNbItineraires(): integer;

    function  LoadItinerairesFromFile(const Filename: RawByteString): boolean;
    procedure SaveItinerairesToFile(const Filename: RawByteString);
    // générer un objet Javascript contenant le graphe
    procedure ExporterGrapheEnJavascript(const DocTitle: string;
                                         const FileNameJS: RawByteString;
                                         const QIDStationDepart, QIDStationArrivee: string;
                                         const BackgroundColorCarte, CouleurCenterlines: TColor;
                                         const CanvasWidthInPixels, CanvasHeightInPixels, WidthMenuLateralInPixels: integer);
    function getBoundinxBox(): TRect2Df;
end;


implementation
uses
  DGCDummyUnit; // unité minimale pour contrer l'erreur 'Fin du source non trouvée'

{ TPathBetweenNodes }
function TPathBetweenNodes.Initialiser(const QSerDep: TNumeroSerie; const QStDep: TNumeroStation;
                                       const QSerArr: TNumeroSerie; const QStArr: TNumeroStation;
                                       const QNomItineraire: string;
                                       const C: TColor): boolean;
begin
  result := false;
  Color          := C;
  SerieDepart    := QSerDep;
  StationDepart  := QStDep;
  SerieArrivee   := QSerArr;
  StationArrivee := QStArr;
  NomItineraire  := QNomItineraire;
  LongueurParcours := 0.00;
  EmptyListeNoeuds();
  result          := True;
end;


procedure TPathBetweenNodes.EmptyListeNoeuds();
begin
  SetLength(self.ListeNoeuds, 0);
end;


procedure TPathBetweenNodes.AddNoeud(const N: TNumeroNoeud);
var
  Nb: Integer;
begin
  Nb := length(self.ListeNoeuds);
  SetLength(self.ListeNoeuds, Nb + 1);
  self.ListeNoeuds[Nb] := N;
end;

function TPathBetweenNodes.GetNoeud(const Idx: integer): TNumeroNoeud;
begin
  Result := self.ListeNoeuds[Idx];
end;

function TPathBetweenNodes.GetNbNoeuds(): integer;
begin
  result := Length(self.ListeNoeuds);
end;

function TPathBetweenNodes.StationDepartToTIDBaseStation(): TIDBaseStation;
begin
  Result := NB_MAXI_SERIES_PAR_CAVITE * Abs(self.SerieDepart) + MULTIPLICATEUR_STATION * Abs(self.StationDepart);
end;

function TPathBetweenNodes.StationArriveeToTIDBaseStation(): TIDBaseStation;
begin
  Result := NB_MAXI_SERIES_PAR_CAVITE * Abs(self.SerieArrivee) + MULTIPLICATEUR_STATION * Abs(self.StationArrivee);
end;

function TPathBetweenNodes.IsSameStationsDepartArrivee(): boolean;
begin
  Result := (self.SerieDepart = self.SerieArrivee) and (self.StationDepart = self.StationArrivee);
end;

//******************************************************************************
{ TPathFindingGraphe }
function TPathFindingGraphe.Reset(): boolean;
var
  MyPath: TPathBetweenNodes;
begin
  result := false;
  FListeDesPlusCourtsChemins.ClearListe();
  MyPath.Initialiser(1, 1, 1, 1, 'Parcours001', clBlue);
  FListeDesPlusCourtsChemins.AddElement(MyPath);
  SetLastError(ERR_GRAPHE_NO_ERROR, '');
  try
    Result := True;
    SetLastError(ERR_GRAPHE_NO_ERROR, '');
    AfficherMessage('Initialisation du graphe', True);
  except
    SetLastError(-1, 'Erreur d''initialisation');
  end;
end;

// Les utilitaires
function TPathFindingGraphe.SetLastError(const QErrCode: integer; const QErrMsg: string): boolean;
begin
  FLastError.ErrCode := QErrCode;
  FLastError.ErrMsg  := QErrMsg;
  result := (FLastError.ErrCode <> ERR_GRAPHE_NO_ERROR);
end;
function  TPathFindingGraphe.GetLastError(): TGrapheLastError;
begin
  result := FLastError;
end;
function TPathFindingGraphe.FormatterTIDStation(const QId: TIDBaseStation): string;
var
  Qser, QSt: integer;
begin
  ExtractSerStFromTIDStation(QId, Qser, QSt);
  result := format('%d.%d', [Qser, QSt]);
end;
procedure TPathFindingGraphe.SetMinMax();
var
  Nb, i: Integer;
  MyNoeud: TBZClassNode;
begin
  FEtendue.ResetBoundingBox();
  Nb := GetNbStations();
  for i := 0 to Nb -1 do
  begin
    MyNoeud := GetStation(i);
    FEtendue.UpdateBoundingBox(MyNoeud.Position);
  end;
end;

procedure TPathFindingGraphe.PurgerDoublons();
var
  EWE0, EWE1: TBZClassNode;
  i, Nb: integer;
begin
  Nb := self.Count;
  for i := Nb -1 downto 1 do
  begin
    EWE0 := GetStation(i-1);
    EWE1 := GetStation(i);
    if (EWE1.IDStation = EWE0.IDStation) then self.Delete(i);
  end;
end;

procedure TPathFindingGraphe.ListerLesNoeuds(const Caption: string; const DoDisplayDependances: boolean = false);
var
  Nb, i, s, QSr, QSt: Integer;
  MyStation: TBZClassNode;

begin
  Nb := self.GetNbStations();
  AfficherMessageErreur('----------------------------------------------------------');
  AfficherMessageErreur(Caption);
  AfficherMessageErreur(Format('%d noeuds (%f, %f) -> %f, %f', [Nb, self.FEtendue.X1, self.FEtendue.Y1, self.FEtendue.X2, self.FEtendue.X2]));
  for i := 0 to Nb - 1 do
  begin
    MyStation := self.GetStation(i);
    ExtractSerStFromTIDStation(MyStation.IDStation, QSr, QSt);
    AfficherMessageErreur(Format(' %d: %d: %d.%d %f, %f, %f',
                    [i, MyStation.IDStation, QSr, QSt,
                     MyStation.Position.X, MyStation.Position.Y, MyStation.Position.Z]));
  end;
  AfficherMessageErreur('');
end;


// Les noeuds
procedure TPathFindingGraphe.BeginNodeList();
begin
  Self.ClearListe();
end;

function TPathFindingGraphe.GetStation(const Idx: integer): TBZClassNode;
begin
  Result := self.Node[Idx];
end;

procedure TPathFindingGraphe.PutNoeud(const Idx: integer; const QNoeud: TBZClassNode);
begin
  self.Node[Idx] := QNoeud;
end;

constructor TPathFindingGraphe.Create;
begin
  inherited Create;
   FListeDesPlusCourtsChemins := TListeSimple<TPathBetweenNodes>.Create;
   FListeDesPlusCourtsChemins.ClearListe();
end;

destructor TPathFindingGraphe.Destroy;
begin
  FListeDesPlusCourtsChemins.ClearListe();
  FreeAndNil(FListeDesPlusCourtsChemins);
  inherited Destroy;
end;


function TPathFindingGraphe.Initialiser(const FD: TToporobotStructure2012; const FE: TBDDEntites): boolean;
begin
  result := false;
  AfficherMessageErreur(Format('%s.Initialiser()', [classname]));
  FDocuTopo   := FD;
  FBDDEntites := FE;
  result := true;
end;

procedure TPathFindingGraphe.Finaliser();
begin
  ;;
end;

function TPathFindingGraphe.FindStationByXY(const QX, QY: double; out BP: TBZClassNode): boolean;
var
  i, Nb: Integer;
  QDist, WU: Double;
  MyNoeud: TBZClassNode;
begin
  Result := false;
  Nb     := GetNbStations();
  if (Nb < 2) then exit;
  QDist := INFINI;
  for i := 0 to Nb - 1 do
  begin
    MyNoeud := GetStation(i);
    WU := (MyNoeud.Position.X - QX) ** 2 + (MyNoeud.Position.Y - QY) ** 2;
    if (WU < QDist) then
    begin
      Result := True;
      BP     := MyNoeud;
      QDist  := WU;
    end;
  end;
end;
procedure TPathFindingGraphe.DisplaySommetsGraphe();
var
  Nb, i: Integer;
  ST: TBZClassNode;
  QSR: TNumeroSerie;
  QST: TNumeroStation;
begin
  Nb := GetNbStations();
  AfficherMessageErreur(Format('%s.DisplaySommetsGraphe(%d)', [classname, Nb]));
  if (0 = Nb) then exit;
  for i := 0 to nb - 1 do
  begin
    ST := self.GetStation(i);
    ExtractSerStFromTIDStation(ST.IDStation, QSR, QST);
    AfficherMessageErreur(format('%d: %d.%d %.2f, %.2f', [i, QSR, QST, ST.Position.X, ST.Position.Y]));
  end;
end;

function TPathFindingGraphe.ConstruireGraphe(): boolean;
var
  MyEntrance: TEntrance;
  NbSeries, NumSerie: Integer;
  NbPts, i: integer;
  MySerie: TObjSerie;
  QSer0, QSer1: TNumeroSerie;
  QPt0, QPt1: TNumeroStation;
  QJonction: TJonctionXYZ;
  QBP1: TBaseStation;
begin
  result := Reset();
  if (not result) then exit;
  // Trier les entités par ID
  FBDDEntites.SortBySerSts();
  FBDDEntites.SortJonctions();
  AfficherMessageErreur('--> Construction du graphe');
  AfficherMessageErreur(Format('--> Noeuds(%d)', [FDocuTopo.GetNbSeries()]));
  // Passe 1: Les noeuds
  self.BeginNodeList();
  MyEntrance := FBDDEntites.GetEntrance(0);
  self.AddStation(1, 0, MyEntrance.ePosition, MyEntrance.eNomEntree);
  // les séries
  NbSeries := FDocuTopo.GetNbSeries();
  for NumSerie := 1 to NbSeries - 1 do
  begin
    MySerie := FDocuTopo.GetSerie(NumSerie);
    QSer0 := MySerie.GetNoSerieDep();
    QPt0  := myserie.GetNoPointDep();
    NbPts := MySerie.GetNbVisees();
    if (0 = NbPts) then Continue; // pas de visées --> []
    if (not FBDDEntites.FindJonctionBySerieSt(QSer0, QPt0, QJonction)) then
    begin
      SetLastError(ERR_GRAPHE_ARC_EXTR_DEB_NOT_FOUND, Format('Noeud départ %d %d.%d non trouvé', [Myserie.GetNumeroDeSerie(), Qser0, QPt0]));
      continue;
    end;
    self.AddStation(QJonction.NoSer, QJonction.NoSt, QJonction.Position, QJonction.ToString());
    // s'il y a un seule station dans la série, on attrappe la station d'arrivée eton crée un arc
    if (2 = NbPts) then
    begin
      QSer1 := MySerie.GetNoSerieArr();
      QPt1  := myserie.GetNoPointArr();
      if (not FBDDEntites.FindJonctionBySerieSt(QSer1, QPt1, QJonction)) then
      begin
        Continue;
      end;
      self.AddStation(QJonction.NoSer, QJonction.NoSt, QJonction.Position, QJonction.ToString());
      continue;
    end
    else        // les points topo intermédiaires
    begin
      for QPt1 := 1 to NbPts - 1 do
      begin
        Qser1 := MySerie.GetNumeroDeSerie();
        if (FBDDEntites.GetEntiteViseeFromSerSt(Qser1, QPt1, QBP1)) then
        begin
          self.AddStation(QSer1, QPt1,  QBP1.PosStation, QBP1.oCommentaires);
        end;
      end;
      // et on ajoute la station de fin de la série
      QSer1 := MySerie.GetNoSerieArr();
      QPt1  := myserie.GetNoPointArr();
      if (not FBDDEntites.FindJonctionBySerieSt(QSer1, QPt1, QJonction)) then Continue;
      self.AddStation(QJonction.NoSer, QJonction.NoSt, QJonction.Position, QJonction.ToString());
    end;
  end;
    self.EndNodesList();
    AfficherMessageErreur('--> Arcs');
    MyEntrance := FBDDEntites.GetEntrance(0);
    NbSeries := FDocuTopo.GetNbSeries(); // les séries
    for NumSerie := 1 to NbSeries - 1 do
    begin
      MySerie := FDocuTopo.GetSerie(NumSerie);
      QSer0 := MySerie.GetNoSerieDep();
      QPt0  := MySerie.GetNoPointDep();
      QSer1 := MySerie.GetNoSerieArr();
      QPt1  := MySerie.GetNoPointArr();
      NbPts := MySerie.GetNbVisees();
      if (0 = NbPts) then Continue; // pas de visées --> []
      if (1 = NbPts) then Continue; // pas de visées --> []
      // s'il y a un seule station dans la série, on attrappe la station d'arrivée et on crée un arc
      if (2 = NbPts) then
      begin
        QSer1 := MySerie.GetNoSerieArr();
        QPt1  := MySerie.GetNoPointArr();
        self.AddArcBetweenStations(QSer0, QPt0, QSer1, QPt1);
      end
      else
      begin
        QSer0 := MySerie.GetNoSerieDep();
        QPt0  := MySerie.GetNoPointDep();
        QSer1 := MySerie.GetNumeroDeSerie();
        for i := 1 to NbPts - 1 do
        begin
          QPt1  := i;
          self.AddArcBetweenStations(QSer0, QPt0, QSer1, QPt1);
          QSer0 := QSer1;
          QPt0  := QPt1;
        end;
        QSer0 := MySerie.GetNumeroDeSerie();
        QPt0  := MySerie.GetNbVisees() - 1;

        QSer1 := MySerie.GetNoSerieArr();
        QPt1  := MySerie.GetNoPointArr();
        self.AddArcBetweenStations(QSer0, QPt0, QSer1, QPt1);
      end;
    end;

  // contrôle
  //DisplaySommetsGraphe();
  Result := True;
end;

function TPathFindingGraphe.GetNbStations(): integer;
begin
  Result := self.Count;
end;

procedure TPathFindingGraphe.AddStation(const QSerie, QStation: integer; const QPosition: TPoint3Df; const QMetaData: string);
var
  FN: TBZClassNode;
begin
  FN := self.AddNode(MakeTIDBaseStation(QSerie, QStation, false), QPosition, QMetaData);
end;

procedure TPathFindingGraphe.EndNodesList();
var
  i, Nb: Integer;
begin
  Nb := GetNbStations();
  if (Nb > 0) then
  begin
    AfficherMessageErreur('EndNodesList()');
    self.Sort();            // AfficherMessageErreur('Tri()');
    self.PurgerDoublons();  // AfficherMessageErreur('Purge doublons()');
    self.SetMinMax();       // AfficherMessageErreur('SetMinMax()');
    SetLastError(ERR_GRAPHE_NO_ERROR, '');
  end
  else
    SetLastError(ERR_GRAPHE_EMPTY_LIST_NODES, 'La liste des noeuds est vide');
end;
function TPathFindingGraphe.FindNoeudByIDStation(const IDS: TIDBaseStation; out ST: TBZClassNode; out IndexOf: TNumeroNoeud): boolean;
  function FindDepth(const I1, I2: TNumeroNoeud; const QIDX: TIDBaseStation): TNumeroNoeud;
  var
    PVT: integer;
    C1: TBZClassNode;
  begin
    Result := -1;
    PVT := (I2 + I1) div 2;                      // coupure en deux => calcul index médian
    if (I1 > I2) then Exit(-1);                  // début > fin >> sortie directe avec erreur
    C1 := GetStation(PVT);                       //GetBasePoint(PVT);
    if (C1.IDStation = QIDX) then Exit(PVT);     // comparaison. Si vrai >> sortie avec numéro d'index
    if (QIDX < C1.IDStation) then                // sinon, recherche en profondeur avec un niveau supplémentaire
    begin
      Result := FindDepth(I1, PVT-1, QIDX);
      Exit;
    end;
    Result := FindDepth(PVT+1, I2, QIDX);
  end;
begin
  Result := false;
  IndexOf := FindDepth(0, GetNbStations() - 1, IDS);
  if (IndexOf = -1) then IndexOf := 0;
  ST := GetStation(IndexOf);
  Exit(True);
end;


function TPathFindingGraphe.AddArcBetweenStations(const Ser1, St1, Ser2, St2: integer; const Bidirectionnel: boolean = true; const Weight: double = 1.00): boolean;
var
  WU1, WU2: Boolean;
  function QAddArc(const QSer1, QSt1, QSer2, QSt2: integer): boolean;
  var
    Q1, Q2  : Boolean;
    BS1, BS2: TBZClassNode;
    FromIdxNode, ToIdxNode: integer;
    QLinkN1N2: TBZNodeLink;
  begin
    Result := false;
    Q1 := FindNoeudByIDStation(MakeTIDBaseStation(QSer1, QSt1, false), BS1, FromIdxNode);
    Q2 := FindNoeudByIDStation(MakeTIDBaseStation(QSer2, QSt2, false), BS2, ToIdxNode);
    if (Not Q1) then exit(SetLastError(ERR_GRAPHE_ARC_EXTR_DEB_NOT_FOUND, Format('***Début %d.%d de l''arc non trouvée', [Ser1, St1])));
    if (Not Q2) then exit(SetLastError(ERR_GRAPHE_ARC_EXTR_FIN_NOT_FOUND, Format('***Fin   %d.%d de l''arc non trouvée', [Ser2, St2])));
    Result := (Q1 and Q2);
    if (result) then QLinkN1N2 := BS1.AddNodeLink(ToIdxNode, Weight);
  end;
begin
  Result := false;
  WU1 := QAddArc(Ser1, St1, Ser2, St2);
  WU2 := QAddArc(Ser2, St2, Ser1, St1);
  result := (WU1 and WU2);
  if (not Result) then AfficherMessageErreur(Format('*** Arc %d.%d <-> %d.%d non inséré', [Ser1, St1, Ser2, St2]));
end;

function TPathFindingGraphe.RemoveNode(const Ser1, St1: integer): boolean;
var
  Q1: Boolean;
  FromIdxNode: TNumeroNoeud;
  BS1: TBZClassNode;
  i: integer;
  NbVoisins: Int64;
begin
  result := false;
  AfficherMessageErreur(format('%s.RemoveNode(%d.%d)', [classname, Ser1, St1]));
  Q1 := FindNoeudByIDStation(MakeTIDBaseStation(Ser1, St1, false), BS1, FromIdxNode);
  if (Not Q1) then exit(SetLastError(ERR_GRAPHE_ARC_EXTR_DEB_NOT_FOUND, Format('*** Noeud introuvable: %d.%d ***', [Ser1, St1])));
  AfficherMessageErreur(format('-- Noeud trouvé: %d.%d', [Ser1, St1]));
  NbVoisins := BS1.NodeLinkList.Count;
  AfficherMessageErreur(format('-- Parcours du noeud: %d - %d voisins', [BS1.IDStation, NbVoisins]));
  (*
  if (NbVoisins > 0) then
  begin
    for i := NbVoisins - 1 downto 0 do BS1.NodeLinkList.Delete(i);

  end;
  //*)
  BS1.NodeLinkList.ClearListe();

  self.Delete(FromIdxNode);
end;
// Fonctionne
function TPathFindingGraphe.RemoveArcBetweenStations(const Ser1, St1, Ser2, St2: integer; const Bidirectionnel: boolean): boolean;
var
  toto: boolean;
  function QRemoveArc(const QSer1, QSt1, QSer2, QSt2: integer): boolean;
  var
    Q1, Q2: Boolean;
    BS1: TBZClassNode;
    FromIdxNode, ToIdxNode: integer;
    i, NbRemoved: integer;
    NbVoisins: Int64;
    EWE: TIDBaseStation;
    BL2: TBZNodeLinkList;
    BS2: TBZNodeLink;
  begin
    result := false;
    AfficherMessageErreur(format('%s.RemoveArcBetweenStations(%d.%d -> %d.%d)', [classname, QSer1, QSt1, QSer2, QSt2]));
    Q1 := FindNoeudByIDStation(MakeTIDBaseStation(QSer1, QSt1, false), BS1, FromIdxNode);
    if (Not Q1) then exit(SetLastError(ERR_GRAPHE_ARC_EXTR_DEB_NOT_FOUND, Format('***Début %d.%d de l''arc non trouvée', [QSer1, QSt1])));
    AfficherMessageErreur(format('-- Point de départ trouvé: %d.%d', [QSer1, QSt1]));
    NbVoisins := BS1.NodeLinkList.Count;
    AfficherMessageErreur(format('-- Parcours du noeud: %d - %d voisins', [BS1.IDStation, NbVoisins]));
    if (0 = NbVoisins) then exit;
    NbRemoved := 0;
    for i := NbVoisins - 1 downto 0 do
    begin
      BL2 := BS1.NodeLinkList;
      BS2 := BS1.NodeLinkList.Items[i];
      AfficherMessageErreur(Format(' --- %d: Voisin %d: %d - %d.%d', [BS1.IDStation, i, BS2.Node.IDStation, QSer2, QSt2]));
      EWE := MakeTIDBaseStation(QSer2, QSt2, false);
      if (EWE = BS2.Node.IDStation) then BS1.NodeLinkList.Delete(i);
    end;
    NbVoisins := BS1.NodeLinkList.Count;
    AfficherMessageErreur(format('-- Parcours du noeud terminé: %d - %d voisins', [BS1.IDStation, NbVoisins]))
  end;
begin
  result := QRemoveArc(Ser1, St1, Ser2, St2); // sens aller
  if (Bidirectionnel) then result := QRemoveArc(Ser2, St2, Ser1, St1); // sens retour éventuel
end;



function TPathFindingGraphe.RechercherPlusCourtChemin(var MyPath: TPathBetweenNodes): boolean;
var
  QNoeudDepart, QNoeudArrivee: TBZClassNode;
  IdxNoeudDepart, IdxNoeudArrivee, NDD: TNumeroNoeud;
  Q1: Boolean;
  i, j: Integer;
  MyStation, StDepart, StArrivee: TBZClassNode;
  QNodeLink: TBZNodeLink;
  QDist: double;
  FShortestPath: TListOfIntegers;
  QNbC: Int64;
begin
  Result := false;
  AfficherMessageErreur(Format('%s.RechercherPlusCourtChemin(): From "%d.%d" To "%d.%d"', [ClassName, MyPath.SerieDepart, MyPath.StationDepart, MyPath.SerieArrivee, MyPath.StationArrivee]));
  SetLastError(0, '');
  FShortestPath := TListOfIntegers.Create;
  try
    FShortestPath.ClearListe();
    // Recherche des noeuds de départ et d'arrivée
    Q1 := FindNoeudByIDStation(MyPath.StationDepartToTIDBaseStation(), QNoeudDepart, IdxNoeudDepart);
    if (Not Q1) then Exit(SetLastError(ERR_GRAPHE_NODE_NOT_FOUND, Format('Noeud "%d.%d" introuvable', [MyPath.SerieDepart, MyPath.StationDepart])));
    Q1 := FindNoeudByIDStation(MyPath.StationArriveeToTIDBaseStation(), QNoeudArrivee, IdxNoeudArrivee);
    if (Not Q1) then Exit(SetLastError(ERR_GRAPHE_NODE_NOT_FOUND, Format('Noeud "%d.%d" introuvable', [MyPath.SerieArrivee, MyPath.StationArrivee])));
    //AfficherMessage(Format('%s.RechercherPlusCourtChemin(): %d: %d.%d -> %d: %d.%d', [ClassName, IdxNoeudDepart, FSerieDepart, FStationDepart, IdxNoeudArrivee, FSerieArrivee, FStationArrivee]));
    // Stations identiques -->[ ]
    if (MyPath.IsSameStationsDepartArrivee()) then Exit(SetLastError(ERR_GRAPHE_SAME_START_END, '-- Les stations de départ et arrivée sont identiques'));
    QDist := self.Dijkstra(IdxNoeudDepart, IdxNoeudArrivee, FShortestPath);
    QNbC  := FShortestPath.Count;
    if (QNbC > 2) then
    begin
      MyPath.EmptyListeNoeuds();
      MyPath.LongueurParcours := 0.00;
      for i := 0 to (QNbC - 1) do MyPath.AddNoeud(FShortestPath.GetElement(QNbC - 1 - i));
      // calcul de la longueur du parcours
      for i := 1 to MyPath.GetNbNoeuds() - 1 do
      begin
        StDepart  := self.GetStation(MyPath.GetNoeud(i-1));
        StArrivee := self.GetStation(MyPath.GetNoeud(i));
        MyPath.LongueurParcours += Hypot3D(StArrivee.Position.X - StDepart.Position.X,
                                           StArrivee.Position.Y - StDepart.Position.Y,
                                           StArrivee.Position.Z - StDepart.Position.Z);
      end;
    end
    else
    begin
      AfficherMessage('Aucun chemin trouvé');
    end;
    Result := (QNbC > 2);
    AfficherMessageErreur(BoolToStr(Result, 'Recherche OK', 'Aucun chemin'));
    FShortestPath.ClearListe();
  finally
    FreeAndNil(FShortestPath);
  end;
end;

procedure TPathFindingGraphe.AddItineraire(const P: TPathBetweenNodes);
begin
  FListeDesPlusCourtsChemins.AddElement(P);
end;
function TPathFindingGraphe.GetItineraire(const Idx: integer; out P: TPathBetweenNodes): boolean;
begin
  P := FListeDesPlusCourtsChemins.GetElement(Idx);
  Result := (P.GetNbNoeuds() > 2);
end;

procedure TPathFindingGraphe.PutItineraire(const Idx: integer; const P: TPathBetweenNodes);
begin
  FListeDesPlusCourtsChemins.PutElement(Idx, P);
end;

procedure TPathFindingGraphe.RemoveItineraire(const Idx: integer);
var
  Nb: LongInt;
begin
  Nb := FListeDesPlusCourtsChemins.GetNbElements();
  if ((Idx <= 0) OR (Idx >= Nb)) then exit;  // ne pas supprimer le chemin 0
  FListeDesPlusCourtsChemins.RemoveElement(Idx);
end;

procedure TPathFindingGraphe.ClearAllItineraires();
begin

end;

function TPathFindingGraphe.GetNbItineraires(): integer;
begin
  result := FListeDesPlusCourtsChemins.GetNbElements();
end;

function TPathFindingGraphe.SaveToFile(const Filename: RawByteString): boolean;
const
  FMT_NOEUD = '%d' + #9 + '%d' + #9 + '%d' + #9 + '%d' + #9 + '%s' + #9 + '%s' + #9 + '%s';
  FMT_ARC   = '%d' + #9 + '%d' + #9 + '%d' + #9 + '%d' + #9 + '%d.%d' + #9 + '%d.%d' + #9 + '%s';
var
  fp: TextFile;
  i, NbNoeuds: integer;
  a, NbA     , QNoStVoisine: integer;
  MyStation, StVoisine: TBZClassNode;
  QSR, QSRv: TNumeroSerie;
  QSt, QSTv: TNumeroStation;
  procedure WrtLn(const s: string);
  begin
    WriteLn(fp, s);
  end;
begin
  result := false;
  AfficherMessage(Format('%s.SaveToFile(%s)', [classname, Filename]));
  AssignFile(fp, FileName);
  try
    ReWrite(fp);
    // Les noeuds
    NbNoeuds := self.Count;
    WrtLn(Format('#Noeuds: %d', [NbNoeuds]));
    WrtLn('#IdxNoeud' + #9 + 'IDStation' + #9 + 'Serie' + #9 + 'Point' + #9 + 'X' + #9 + 'Y' + #9 + 'Z');
    for i := 0 to NbNoeuds - 1 do
    begin
      MyStation := GetStation(i);
      ExtractSerStFromTIDStation(MyStation.IDStation, QSR, QST);
      WrtLn(Format(FMT_NOEUD, [i, MyStation.IDStation, QSR, QST,
                               FormatterNombreOOo(MyStation.Position.X),
                               FormatterNombreOOo(MyStation.Position.Y),
                               FormatterNombreOOo(MyStation.Position.Z)]));
    end;
    // Les arcs
    WrtLn(Format('#Arcs: %d', [0]));
    WrtLn('#IdxNoeud' + #9 + 'IDStation' + #9 + 'IdxVoisin' + #9 + 'IDStationVoisin' + #9 + 'From' + #9 + 'To' + #9 + 'Distance');
    for i := 0 to NbNoeuds - 1 do
    begin
      MyStation := GetStation(i);
      ExtractSerStFromTIDStation(MyStation.IDStation, QSR, QST);
      NbA := MyStation.NodeLinkList.Count;
      if (NbA > 0) then
      begin
        for a := 0 to NbA - 1 do
        begin
          QNoStVoisine := MyStation.LinkNode[a].TargetNodeIndex;
          StVoisine := GetStation(QNoStVoisine);
          ExtractSerStFromTIDStation(MyStation.IDStation, QSR, QST);
          ExtractSerStFromTIDStation(StVoisine.IDStation, QSRv, QSTv);
          WrtLn(Format(FMT_ARC, [i, MyStation.IDStation, QNoStVoisine, StVoisine.IDStation,
                                   QSR, QST, QSRv, QSTv,
                                   FormatterNombreOOo(MyStation.LinkNode[a].Distance)]));
        end;
      end;
    end;
    result := True;
  finally
    CloseFile(fp);
  end;
end;

function TPathFindingGraphe.LoadItinerairesFromFile(const Filename: RawByteString): boolean;
var
  LS: TStringList;
  i, Nb: Integer;
  MyIti: TPathBetweenNodes;
  WU: Boolean;
  EWE: TGHStringArray;
begin
  result := false;
  if (not FileExistsUTF8(Filename)) then exit;
  AfficherMessage(Format('%s.LoadItinerairesFromFile: %s', [ClassName, Filename]));
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.LoadFromFile(Filename);
    Nb := LS.Count;
    if (Nb = 0) then exit;
    //1	1	335	12	255	128	0	Itineraire3
    for i := 0 to Nb - 1 do
    begin
      EWE := Split(Trim(LS.Strings[i]), #9);
      MyITI.SerieDepart    := StrToIntDef(EWE[0], 1);
      MyITI.StationDepart  := StrToIntDef(EWE[1], 0);
      MyITI.SerieArrivee   := StrToIntDef(EWE[2], 1);
      MyITI.StationArrivee := StrToIntDef(EWE[3], 1);
      MyIti.Color          := RGBToColor(StrToIntDef(EWE[4], 255) AND 255,
                                         StrToIntDef(EWE[5], 0  ) AND 255,
                                         StrToIntDef(EWE[6], 0  ) AND 255);
      MyIti.NomItineraire  := Trim(EWE[7]);
      WU := self.RechercherPlusCourtChemin(MyIti);
      if (WU) then self.AddItineraire(MyITI);
    end;
    result := True;
  finally
    LS.Clear;
    FreeAndNil(LS);
  end;
end;
procedure TPathFindingGraphe.SaveItinerairesToFile(const Filename: RawByteString);
var
  LS: TStringList;
  i, Nb: Integer;
  MyIti: TPathBetweenNodes;
  EWE: String;
begin
  Nb := GetNbItineraires();
  AfficherMessage(Format('%s.SaveItinerairesToFile: %d itineraires -> %s', [ClassName, Nb, Filename]));
  if (Nb <= 1) then Exit;
  LS := TStringList.Create;
  try
    LS.Clear;
    for i := 1 to Nb - 1 do
    begin
      GetItineraire(i, MyIti);
      EWE := Format('%d', [MyIti.SerieDepart]) + #9;
      EWE += Format('%d', [MyIti.StationDepart]) + #9;
      EWE += Format('%d', [MyIti.SerieArrivee]) + #9;
      EWE += Format('%d', [MyIti.StationArrivee]) + #9;
      EWE += Format('%d', [Red(MyIti.Color)]) + #9;
      EWE += Format('%d', [Green(MyIti.Color)]) + #9;
      EWE += Format('%d', [Blue(MyIti.Color)]) + #9;
      EWE += MyIti.NomItineraire;
      LS.Add(EWE);
    end;
    LS.SaveToFile(Filename);
  finally
    LS.Clear;
    FreeAndNil(LS);
  end;
end;


// Exporter le graphe en Javascript
procedure TPathFindingGraphe.ExporterGrapheEnJavascript(const DocTitle: string;
                                                        const FileNameJS: RawByteString;
                                                        const QIDStationDepart, QIDStationArrivee: string;
                                                        const BackgroundColorCarte, CouleurCenterlines: TColor;
                                                        const CanvasWidthInPixels, CanvasHeightInPixels, WidthMenuLateralInPixels: integer);
const
  MIME_TEXT_JAVASCRIPT       = 'text/javascript';

  FMT_POSITION_DIV_IN_PIXELS = '         %s:%dpx;';

  HTML_BUTTON_FORMAT = '<INPUT type="button" name="%s" style="width:%d%%;height:%dpx;" value="%s" onclick="%s()">';
  ETENDUE_X_MAX_FOR_DISP_LBL_STATIONS  = 300.00; // étendue max pour affichage des labels des stations
  HAUTEUR_FORM_SEARCH_IN_PIXELS        = 78;
  HAUTEUR_PNL_NAVIG_IN_PIXELS          = 160;
  HAUTEUR_FORM_SHORTEST_PATH_IN_PIXELS = 130;

  SIZE_STATION_MARKER                 = 2;
  NAMEREF_TITRE                       = 'Titre';
  NAMEREF_PANNEAU_ROADMAP             = 'PanneauRoadMap';
  NAMEREF_PANNEAU_BTNS_VUE            = 'PanneauBtnsVue';
  NAMEREF_FORM_SHORTEST_PATH          = 'PanneauShortestPath';
  NAMEREF_FORM_SEARCH                 = 'FormRechercheStation';
  NAMEREF_CANVAS_CONTAINER            = 'MyPlan';
  NAMEREF_CANVAS_2D                   = 'MyCanvas2D';
  NAMEREF_CANVAS_DC                   = 'AC_DC';

  JS_CONST_INFINITY                   = 'Infinity';
  JS_GLOBAL_VAR_FXMini                = 'FGlobalFXMini';
  JS_GLOBAL_VAR_FXMaxi                = 'FGlobalFXMaxi';
  JS_GLOBAL_VAR_FYMini                = 'FGlobalFYMini';
  JS_GLOBAL_VAR_FYMaxi                = 'FGlobalFYMaxi';
  JS_GLOBAL_VAR_FXCentreMap           = 'FGlobalFXCentreMap';
  JS_GLOBAL_VAR_FYCentreMap           = 'FGlobalFYCentreMap';
  JS_GLOBAL_VAR_FLastIdxFound         = 'FGlobalLastIdxFound';

  JS_GLOBAL_VAR_FGlobalNoeudCourant   = 'FGlobalNoeudCourant';

  JS_GLOBAL_VAR_MyContainer           = 'MyContainer';
  JS_GLOBAL_VAR_MyCanvas              = 'MyCanvas';
  JS_GLOBAL_VAR_MylbMouseCoords       = 'MylbMouseCoords';
  JS_GLOBAL_VAR_MylbRoadmapNbPoints   = 'MylbRoadmapNbPoints';
  JS_GLOBAL_VAR_MylsbRoadMap          = 'MylsbRoadMap';

  JS_GLOBAL_VAR_MylbDistNextPoint     = 'MylbDistNextPoint';
  JS_GLOBAL_VAR_MylbAzimutNextPoint   = 'MylbAzimutNextPoint';
  JS_GLOBAL_VAR_MylbPenteNextPoint    = 'MylbPenteNextPoint';

  JS_GLOBAL_VAR_MyeditFindWhat        = 'MyeditFindWhat';
  JS_GLOBAL_VAR_MyeditStationDepart   = 'MyeditStationDepart';
  JS_GLOBAL_VAR_MyeditStationArrivee  = 'MyeditStationArrivee';

  JS_FUNC_CoordsToPlan       = 'JSCoordsToPlan';
  TO_DECIMETERS = 10.00; // Optimisation: On génère des coordonnées entières en décimètres

  // accès aux éléments de la page
  // -- formulaire de localisation de station
  JS_DOCUMENT_MAP_CANVAS                            = 'MapCanvas2D';

  JS_DOCUMENT_FORM_NAME_SEARCH                      = 'frmSearchNode';

  JS_DOCUMENT_LSB_STATIONS_NAME                     = 'Stations';
  JS_DOCUMENT_FORM_editFindWhat                     = 'editFindWhat';
  JS_DOCUMENT_FORM_btnSearch                        = 'btnFindWhat';
  JS_DOCUMENT_FORM_btnSearch_ProcOnSubmit           = 'FindStationByEtiquette';
  // formulaire de recherche de plus court chemin
  JS_DOCUMENT_FORM_NAME_SHORTEST_PATH               = 'frmCalcShortestPath';
  JS_DOCUMENT_FORM_editStationDepart                = 'editStationDepart';
  JS_DOCUMENT_FORM_editStationArrivee               = 'editStationArrivee';
  JS_DOCUMENT_FORM_btnCalcShortestPath              = 'btnCalcShortestPath';
  JS_DOCUMENT_FORM_btnCalcShortestPath_ProcOnSubmit = 'CalcShortestPathBetweenGivenStations';
  // feuille de route
  JS_DOCUMENT_FORM_NAME_ROADMAP                     = 'frmRoadMap';
  JS_DOCUMENT_FORM_lbRoadmapNbPoints                = 'lbRoadmapNbPoints';
  JS_DOCUMENT_FORM_lbCanvasMousePos                 = 'lbCanvasMousePos';

  JS_DOCUMENT_FORM_lbDistNextPoint                  = 'lbDistNextPoint';
  JS_DOCUMENT_FORM_lbAzimutNextPoint                = 'lbAzimutNextPoint';
  JS_DOCUMENT_FORM_lbPenteNextPoint                 = 'lbPenteNextPoint';

  JS_DOCUMENT_FORM_lsbRoadMap                       = 'lsbRoadMap';
  JS_DOCUMENT_FORM_lsbRoadmap_ProcOnSelect          = 'CentrerCarteSurItem';

  JS_GRAPHE_CLASS_VAR_NAME    = 'JSGraphe';
  JS_THIS                     = 'this';
  JS_MATH_LIBRARY             = 'Math';

  JS_FUNCTION_INITIALISER     = 'Initialiser';
  JS_EVENT_LISTENER_Beforeunload = 'beforeunload';
  JS_FUNCTION_DistanceBetweenNodes     = 'DistanceBetweenNodes';
  JS_FUNCTION_GetNbStationsOfPathFound = 'GetNbStationsOfPathFound';
  JS_FUNCTION_AddIdxNoeudAtPathFound   = 'AddIdxNoeudAtPathFound';
  JS_FUNCTION_GetIdxNoeudOfPathFound   = 'GetIdxNoeudOfPathFound';
  JS_FUNCTION_CalcCheminMinimalBetweenTwoNodes = 'CalcCheminMinimalBetweenTwoNodes';
  JS_FUNCTION_DRAW_GRAPHE     = 'DrawGraphe';
  JS_VAR_FLAG_DO_DRAW_GRAPHE  = 'false';

  JS_FUNCTION_CentrerSurXY    = 'CentrerSurPointXY';
  JS_FUNCTION_NbNoeudsVisites = 'NbNoeudsVisites';
  JS_FUNCTION_TrouveMin       = 'TrouveMin';
  JS_FUNCTION_ListerRoadMap   = 'ListerRoadMap';

  JS_ARGS_ADD_NOEUD          = 'QIDNoeud, QX, QY, QZ, QListeVoisins';
  JS_ARGS_ADD_EDGE           = 'QIdxNoeudDepart, QIdxNoeudArrivee, QWeight';
  JS_VAR_IDX_ND_DEPART       = 'IdxNoeudDep';
  JS_VAR_IDX_ND_ARRIVEE      = 'IdxNoeudArr';
  JS_LOCAL_ListePreds        = 'ListePreds';
  JS_LOCAL_ListeDistances    = 'ListeDistances';
  JS_LOCAL_ListeNdsVisites   = 'ListeNoeudsVisites';

  JS_GRAPH_SHORTEST_PATH     = 'FShortestPath';
  JS_GRAPH_LISTE_NOEUDS      = 'FListeNoeuds';

  JS_VARNAME_PREVIOUS_NODE   = 'MyNodePrevious';
  JS_VARNAME_CURRENT_NODE    = 'MyNodeCurrent';
  JS_VARNAME_NEXT_NODE       = 'MyNodeNext';
  JS_VARNAME_CapToNext       = 'CapToNext';

  JS_VARNAME_NbNoeuds        = 'NbNoeuds';

  JS_VARNAME_dx   = 'dx';
  JS_VARNAME_dy   = 'dy';
  JS_VARNAME_dz   = 'dz';

  JS_VARNAME_OUT_Dist = 'Distance';
  JS_VARNAME_OUT_Az   = 'Azimut';
  JS_VARNAME_OUT_Inc  = 'Inclinaison';


  // Callbacks de la carte
  JS_CALLBACK_CANVAS_OnResizeCanvas     = 'OnResizeCanvas';
  JS_CALLBACK_CANVAS_OnClickCanvas      = 'OnClickCanvas';
  JS_CALLBACK_CANVAS_OnMouseDownCanvas  = 'OnMouseDownCanvas';
  // Utilitaires
  JS_SECTION_FUNCTIONS_UTILITAIRES      = 'fonctions utilitaires';
  JS_UTILITY_FUNC_GetAzimut      = 'JSGetAzimut';
  JS_UTILITY_FUNC_GetBearingInc  = 'JSGetBearingInc';

  FMT_GET_ELEMENT_BY_ID  = '  %-25s = document.getElementById("%s");';
  FMT_ADD_EVENT_LISTENER = '  %s.addEventListener("%s", %s, false);';

  FMT_EDIT_SETVALUE      = '  %s.text = %s;';

  // Fonctions de vue
  JS_VUE_ResetVue          = 'ResetVue';
  JS_VUE_ZoomPlus          = 'ZoomPlus';
  JS_VUE_ZoomMoins         = 'ZoomMoins';
  JS_VUE_PanVueU           = 'PanVueUp';
  JS_VUE_PanVueD           = 'PanVueDown';
  JS_VUE_PanVueL           = 'PanVueLeft';
  JS_VUE_PanVueR           = 'PanVueRight';
  JS_VUE_CentrerSurCourant = 'CentrerSurPointCourant';
  JS_VUE_PickStationDep    = 'PickStationDep';
  JS_VUE_PickStationArr    = 'PickStationArr';

  // Pour les gestures de tablettes (pincement, pointage au doigt

  JS_GLOBAL_VAR_EvtCache   = 'evtCache';
  JS_GLOBAL_VAR_PrevDiff   = 'prevDiff';

  JS_TACTILE_PROC_remove_event           = 'remove_event';
  JS_TACTILE_EVENT_pointerdown_handler   = 'tactile_pointerdown_handler';
  JS_TACTILE_EVENT_pointermove_handler   = 'tactile_pointermove_handler';
  JS_TACTILE_EVENT_pointerup_handler     = 'tactile_pointerup_handler';
var
  fp   : TextFile;
  i, Nb, j: integer;
  FCurrentClassName: string;
  FCurrentProcName : string;
  FWidthMenuLateralInPixels : integer;
  FCanvasWidthInPixels      : integer;
  FCanvasHeightInPixels     : integer;
  MyNoeud, StVoisine: TBZClassNode;
  EWE: String;
  NbVoisins: Int64;
  QLineWidth , QPlanWidth, QIdx: integer;
  SR: TObjSerie;
  {$INCLUDE JSExportGrapheNestedFunc.inc}
begin
  FCurrentProcName := '';
  FCurrentClassName:= '';
  AfficherMessage(Format('%s.ExporterGrapheEnJavascript(%s) - From "%s" To "%s"', [ClassName, FileNameJS, QIDStationDepart, QIDStationArrivee]));
  AssignFile(fp, FileNameJS);
  try
    ReWrite(fp);
    FCanvasWidthInPixels  := CanvasWidthInPixels;
    FCanvasHeightinPixels := CanvasHeightInPixels;
    FWidthMenuLateralInPixels := WidthMenuLateralInPixels;//FCanvasWidthInPercent := WidthMenuLateral;
    QPlanWidth  := FCanvasWidthInPixels  - FWidthMenuLateralInPixels;
    BeginHTML();
      BeginHEAD();
        BeginCSS_Styles();
          WrtLinFmt('  html, body { margin:%dpx; padding:%d; }', [0, 0]);
          // CSS du canevas
          BeginCSSClass(NAMEREF_CANVAS_CONTAINER);
            WrtLinFmt('         %s:%s;'     , ['position', 'absolute']);
            WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS , ['left'  , 0]);
            WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS , ['top'   , 0]);
            WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS , ['width' , QPlanWidth]);
            WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS , ['height', FCanvasHeightinPixels]);
            WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS , ['border', 0]);
            WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS , ['margin', 0]);
          EndCSSClass();
          BeginCSSClass(NAMEREF_CANVAS_2D);
            WrtLinFmt('         %s:%s;', ['float', 'left']);
            WrtLinFmt('         %s:%s;'  , ['background-color', QColorToHTMLColor(BackgroundColorCarte)]);
            WrtLinFmt('         %s:%dpx solid black;'     , ['border', 1]);
          EndCSSClass();
          // CSS de la zone de recherche
          PositionnerPanneau(NAMEREF_FORM_SEARCH,
                             QPlanWidth, 0,
                             FWidthMenuLateralInPixels, HAUTEUR_FORM_SEARCH_IN_PIXELS,
                             'none', clBlack, clLime);
          // CSS du panneau de navigation de la vue
          PositionnerPanneau(NAMEREF_PANNEAU_BTNS_VUE,
                             QPlanWidth, HAUTEUR_FORM_SEARCH_IN_PIXELS,
                             FWidthMenuLateralInPixels, HAUTEUR_PNL_NAVIG_IN_PIXELS,
                             'none', clBlack, clYellow);
          // CSS du panneau de recherche de plus court chemin
          PositionnerPanneau(NAMEREF_FORM_SHORTEST_PATH,
                             QPlanWidth, HAUTEUR_FORM_SEARCH_IN_PIXELS + HAUTEUR_PNL_NAVIG_IN_PIXELS,
                             FWidthMenuLateralInPixels, HAUTEUR_FORM_SHORTEST_PATH_IN_PIXELS,
                             'none', clBlack, clFuchsia);
          // CSS du reste de la zone de droite
          PositionnerPanneau(NAMEREF_PANNEAU_ROADMAP,
                             QPlanWidth, HAUTEUR_FORM_SEARCH_IN_PIXELS + HAUTEUR_PNL_NAVIG_IN_PIXELS + HAUTEUR_FORM_SHORTEST_PATH_IN_PIXELS,
                             FWidthMenuLateralInPixels, FCanvasHeightinPixels - HAUTEUR_FORM_SEARCH_IN_PIXELS - HAUTEUR_PNL_NAVIG_IN_PIXELS - HAUTEUR_FORM_SHORTEST_PATH_IN_PIXELS,
                             'none', clBlack, clAqua);
        EndCSS_Styles();
        BeginScriptSection();
          QBeginRedigerFonctionsUtilitaires();
            QRedigeFunctionGetAzimut();
            QRedigeFunctionGetBearingInc();
          QEndRedigerFonctionsUtilitaires();

          QRedigeClasseTJSGrapheNode();
          QBeginClasseTJSGraphe();
            QRedigeClassJSGrapheFunctionFindIdxNoeudNearestToXY();
            WriteLine('  // Le chemin');
            QRedigeClassJSGrapheFunctionsPathFound();
            QRedigeClassJSGrapheFunctionsGetBounds();
            QRedigeClassJSGrapheFunctionDistanceBetweenNodes();
            QRedigeClassJSGrapheFunctionDijkstra();
            QEndClasseTJSGraphe();
          QRedigeDeclGlobalVars();

          WriteLine('// Point d''entrée du programme');
          BeginJSFunction(JS_FUNCTION_INITIALISER, 'St1, St2');
          BeginJSEventListener(JS_EVENT_LISTENER_Beforeunload, 'Anti-reload (indispensable pour tablettes)');
            WriteLine('    e.preventDefault();');
            WriteLine('    e.returnValue = "";');
          EndJSEventListener();
          WriteLine('  // Accès aux contrôles');
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MyContainer        , NAMEREF_CANVAS_CONTAINER]);
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MyCanvas           , NAMEREF_CANVAS_2D]);
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MylbMouseCoords    , JS_DOCUMENT_FORM_lbCanvasMousePos]);
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MylbRoadmapNbPoints, JS_DOCUMENT_FORM_lbRoadmapNbPoints]);
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MylsbRoadMap       , JS_DOCUMENT_FORM_lsbRoadMap]);

          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MylbDistNextPoint  , JS_DOCUMENT_FORM_lbDistNextPoint]);
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MylbAzimutNextPoint, JS_DOCUMENT_FORM_lbAzimutNextPoint]);
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MylbPenteNextPoint , JS_DOCUMENT_FORM_lbPenteNextPoint]);

          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MyeditFindWhat       , JS_DOCUMENT_FORM_editFindWhat]);
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MyeditStationDepart  , JS_DOCUMENT_FORM_editStationDepart]);
          WrtLinFmt(FMT_GET_ELEMENT_BY_ID, [JS_GLOBAL_VAR_MyeditStationArrivee , JS_DOCUMENT_FORM_editStationArrivee]);
          // Init des événements des doigts sur le canevas
          QInitialiserEventsOfTactilePointer();
          //******************************************************************************
          WrtLinFmt(FMT_EDIT_SETVALUE, [JS_GLOBAL_VAR_MyeditFindWhat      , '1.1']);
          WrtLinFmt(FMT_EDIT_SETVALUE, [JS_GLOBAL_VAR_MyeditStationDepart  , QIDStationDepart]);
          WrtLinFmt(FMT_EDIT_SETVALUE, [JS_GLOBAL_VAR_MyeditStationArrivee , QIDStationArrivee]);

          WriteLine('  // Ajout des listeners');
          WrtLinFmt('  window.addEventListener("%s", %s, false);', ['resize', JS_CALLBACK_CANVAS_OnResizeCanvas]);
          WrtLinFmt(FMT_ADD_EVENT_LISTENER, [JS_GLOBAL_VAR_MyCanvas, 'click'    , JS_CALLBACK_CANVAS_OnClickCanvas]);
          WrtLinFmt(FMT_ADD_EVENT_LISTENER, [JS_GLOBAL_VAR_MyCanvas, 'mousedown', JS_CALLBACK_CANVAS_OnMouseDownCanvas]);

          WrtLinFmt('  // Ajout des %d noeuds', [GetNbStations()]);
          Nb := GetNbStations();
          for i := 0 to Nb -1 do
          begin
            EWE := '';
            MyNoeud := GetStation(i);
            NbVoisins := MyNoeud.NodeLinkList.Count;
            if (NbVoisins > 0) then
            begin
              for j := 0 to NbVoisins - 1 do
              begin
                StVoisine := GetStation(MyNoeud.LinkNode[j].TargetNodeIndex);
                EWE += Format('%d%s', [MyNoeud.LinkNode[j].TargetNodeIndex, IIF(j < (NbVoisins - 1), ',', '')]);
              end;
            end;
            WrtLinFmt('  %s.AddNoeud("%s", %d, %d, %d, [%s]);', [JS_GRAPHE_CLASS_VAR_NAME,
                                                                  FormatterTIDStation(MyNoeud.IDStation),
                                                                  ToJSCoord(MyNoeud.Position.X),
                                                                  ToJSCoord(MyNoeud.Position.Y),
                                                                  ToJSCoord(MyNoeud.Position.Z),
                                                                  EWE]);
          end;
          WrtLinFmt('  %s = %s.GetXMini();', [JS_GLOBAL_VAR_FXMini, JS_GRAPHE_CLASS_VAR_NAME]);
          WrtLinFmt('  %s = %s.GetXMaxi();', [JS_GLOBAL_VAR_FXMaxi, JS_GRAPHE_CLASS_VAR_NAME]);
          WrtLinFmt('  %s = %s.GetYMini();', [JS_GLOBAL_VAR_FYMini, JS_GRAPHE_CLASS_VAR_NAME]);
          WrtLinFmt('  %s = %s.GetYMaxi();', [JS_GLOBAL_VAR_FYMaxi, JS_GRAPHE_CLASS_VAR_NAME]);

          WrtLinFmt('  %s(St1, St2);', [JS_FUNCTION_CalcCheminMinimalBetweenTwoNodes]);
          WriteLine('  // et on fixe le noeud courant');
          WrtLinFmt('  %s = %s.GetNoeud(%d);', [JS_GLOBAL_VAR_FGlobalNoeudCourant, JS_GRAPHE_CLASS_VAR_NAME,1]);
          WrtLinFmt('  %s.innerHTML = "ID = " + %s.IDNoeud;', [JS_DOCUMENT_FORM_lbCanvasMousePos, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
          WrtLinFmt('} // %s', [JS_FUNCTION_INITIALISER]);
          WriteLine('//-------------------------------------------------------------------');
          QRedigeFunctionCalcCheminMinimalBetweenTwoNodes();
          WriteLine('//=== Interaction avec le canevas =========================');
          QLineWidth := 0;
          QRedigeFunctionListerRoadMap();
          QRedigeCallbackOnResizeCanvas();
          QRedigeCallbackOnClickCanvas();
          QRedigeCallbackOnMouseDownCanvas();
          JSSeparateur();
          QRedigeFunctionDrawGraphe();
          QRedigeFunctionsPanZoomPlan();
          QRedigeFunctionCentrerSurXY();
          //JSSeparateur('+', 100);
          WriteLine('//=== Interaction avec la page =========================');
          QRedigeFunctionProcOnSubmit();
          QRedigeFunctionBtnCalcShortestPathClick();
          QRedigeFunctionLsbRoadmapOnSelect();
          // les handlers de tactile
          QRedigerTactileHandlers();
        EndScriptSection();
      EndHEAD();
      BeginBODY(JS_FUNCTION_INITIALISER, QIDStationDepart, QIDStationArrivee);
        BeginDiv(NAMEREF_CANVAS_CONTAINER); // La carte
          WrtLinFmt('    <canvas id="%s">', [NAMEREF_CANVAS_2D]);                  // La carte
          WriteLine('    </canvas>');
        EndDiv();
        BeginDiv(NAMEREF_FORM_SEARCH);
          BeginForm(JS_DOCUMENT_FORM_NAME_SEARCH);
            JSMakeLabelledEdit(JS_DOCUMENT_FORM_editFindWhat, 'Localiser un point', JS_DOCUMENT_FORM_editFindWhat, QIDStationDepart);
            WrtLinFmt('      ' + HTML_BUTTON_FORMAT, [JS_DOCUMENT_FORM_btnSearch, 95, 32, 'Rechercher', JS_DOCUMENT_FORM_btnSearch_ProcOnSubmit]);
          EndForm();
        EndDiv();
        BeginDiv(NAMEREF_PANNEAU_BTNS_VUE); //   // Les boutons de navigation de la vue
          BeginTable(100, 100);
            BeginRow();
              JSMakeButtonInTable('btnResetVue'  , '&nwarr;'    , JS_VUE_ResetVue);
              JSMakeButtonInTable('btnPanUp'     , '&uarr;'     , JS_VUE_PanVueU);         //&larr;
              JSMakeButtonInTable('btn13'        , '---'        , '');
            EndRow();
            BeginRow();
              JSMakeButtonInTable('btnPanLeft'   , '&larr;'     , JS_VUE_PanVueL);
              JSMakeButtonInTable('btn22'        , '-O-'        , JS_VUE_CentrerSurCourant);
              JSMakeButtonInTable('btnPanRight'  , '&rarr;'     , JS_VUE_PanVueR);
            EndRow();
            BeginRow();
              JSMakeButtonInTable('btnZoomPlus'  , '+'          , JS_VUE_ZoomPlus);
              JSMakeButtonInTable('btnPanDown'   , '&darr;'     , JS_VUE_PanVueD);
              JSMakeButtonInTable('btnZoomMoins' , '-'          , JS_VUE_ZoomMoins);
            EndRow();
          EndTable();
        EndDiv();
        BeginDiv(NAMEREF_FORM_SHORTEST_PATH); // Saisie des points de départ et d'arrivée
          BeginForm(JS_DOCUMENT_FORM_NAME_SHORTEST_PATH);
            BeginTable(100, 100);
              BeginRow();
                WriteLine('<TD>');
                  JSMakeLabelledEdit('lbStationDepart' , 'Départ'  , JS_DOCUMENT_FORM_editStationDepart  , QIDStationDepart);
                WriteLine('</TD>');
                  JSMakeButtonInTable('btnPickStationDepart', '...', JS_VUE_PickStationDep);
              EndRow();
              BeginRow();
                WriteLine('<TD>');
                  JSMakeLabelledEdit('lbStationArrivee', 'Arrivée' , JS_DOCUMENT_FORM_editStationArrivee , QIDStationArrivee);
                WriteLine('</TD>');
                JSMakeButtonInTable('btnPickStationArrivee', '...' , JS_VUE_PickStationArr);
              EndRow();
              BeginRow();
                WriteLine('<TD>');
                  WrtLinFmt('    ' + HTML_BUTTON_FORMAT, [JS_DOCUMENT_FORM_btnCalcShortestPath, 95, 32, 'Rechercher', JS_DOCUMENT_FORM_btnCalcShortestPath_ProcOnSubmit]);
                WriteLine('</TD>');
              EndRow();
            EndTable();
          EndForm();
        EndDiv();
        BeginDiv(NAMEREF_PANNEAU_ROADMAP); //    // Feuille de route
          BeginForm(JS_DOCUMENT_FORM_NAME_ROADMAP);                      // la liste des items
            AddFormLabel(JS_DOCUMENT_FORM_lbRoadmapNbPoints, 'lbRoadmapNbPoints');
            WrtLinFmt('    <select id="%s" name="%s" style="width:%d%%" onChange="%s()"> </select><BR>', [JS_DOCUMENT_FORM_lsbRoadMap , JS_DOCUMENT_FORM_lsbRoadMap, 100, JS_DOCUMENT_FORM_lsbRoadmap_ProcOnSelect]);
            AddFormLabel(JS_DOCUMENT_FORM_lbCanvasMousePos   , '--');
            WriteLine('    <HR>');
            AddFormLabel(JS_DOCUMENT_FORM_lbAzimutNextPoint  , '--');
            WriteLine('    <BR>');
            AddFormLabel(JS_DOCUMENT_FORM_lbDistNextPoint    , '--');
            AddFormLabel(JS_DOCUMENT_FORM_lbPenteNextPoint   , '--');
          EndForm();
        EndDiv();
     EndBODY();
   EndHTML();
  finally
    Closefile(fp);
  end;
end;

function TPathFindingGraphe.getBoundinxBox(): TRect2Df;
begin
  result := FEtendue;
end;

end.
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure DrawPipistrelle(const TmpBuffer: TBGRABitmap);
const
  UnitLongueur: integer = 2;
var
  i, j: integer;
  TraX, TraY: integer;
  Pipistrelle: array[0..1, 0..13] of TPoint;
begin
  //AfficherMessage(' --> DrawPipistrelle');

  TraX := TmpBuffer.Width - UnitLongueur * 16;
  TraY := TmpBuffer.Height - UnitLongueur * 16;
  for i := 0 to 13 do
  begin
    Pipistrelle[0, i] := MakeTPoint(0, 0);
    Pipistrelle[1, i] := MakeTPoint(0, 0);
  end;
  Pipistrelle[0, 0]  := MakeTPoint(0, 6 * UnitLongueur);
  Pipistrelle[0, 1]  := MakeTPoint(0, -9 * UnitLongueur);
  Pipistrelle[0, 2]  := MakeTPoint(16 * UnitLongueur, -9 * UnitLongueur);
  Pipistrelle[0, 3]  := MakeTPoint(0, -5 * UnitLongueur);
  Pipistrelle[0, 4]  := MakeTPoint(3 * UnitLongueur, -6 * UnitLongueur);
  Pipistrelle[0, 5]  := MakeTPoint(8 * UnitLongueur, -1 * UnitLongueur);
  Pipistrelle[0, 6]  := MakeTPoint(13 * UnitLongueur, -1 * UnitLongueur);
  Pipistrelle[0, 7]  := MakeTPoint(8 * UnitLongueur, 4 * UnitLongueur);
  Pipistrelle[0, 9]  := MakeTPoint(2 * UnitLongueur, 2 * UnitLongueur);
  Pipistrelle[0, 8]  := MakeTPoint(0, 1 * UnitLongueur);
  Pipistrelle[0, 10] := MakeTPoint(4 * UnitLongueur, 8 * UnitLongueur);
  Pipistrelle[0, 11] := MakeTPoint(0, 6 * UnitLongueur);
  Pipistrelle[0, 12] := MakeTPoint(4 * UnitLongueur, -8 * UnitLongueur);
  Pipistrelle[0, 13] := MakeTPoint(5 * UnitLongueur, -8 * UnitLongueur);
  for i := 0 to 13 do
  begin
    Pipistrelle[1, i] := MakeTPoint(-Pipistrelle[0, i].X, Pipistrelle[0, i].Y);
    Pipistrelle[1, i] := MakeTPoint(Pipistrelle[1, i].X + TraX, TmpBuffer.Height - (Pipistrelle[1, i].Y + TraY));
    Pipistrelle[0, i] := MakeTPoint(Pipistrelle[0, i].X + TraX, TmpBuffer.Height - (Pipistrelle[0, i].Y + TraY));
  end;
  with TmpBuffer.CanvasBGRA do
  begin
    Pen.Color := clBlue;
    Pen.Width := 0;
    for j := 0 to 1 do
    begin
      MoveTo(Pipistrelle[j, 0]);
        LineTo(Pipistrelle[j, 1]);
        LineTo(Pipistrelle[j, 2]);
      MoveTo(Pipistrelle[j, 3]);
      for i :=  4 to  8 do LineTo(Pipistrelle[j, i]);
      MoveTo(Pipistrelle[j, 9]);
      for i := 10 to 11 do LineTo(Pipistrelle[j, i]);
      MoveTo(Pipistrelle[j, 4]);
      for i := 12 to 13 do LineTo(Pipistrelle[j, i]);
    end;
  end;
end;

