unit unitgraphes11;
// Calcul du chemin minimal dans un réseau GHTopo
{$mode delphi}
{$ERROR Ne fonctionne pas}
interface
uses
  Classes, SysUtils, math, Graphics,
  Common,
  StructuresDonnees,
  UnitObjetSerie,
  UnitListesSimplesWithGeneriques,
  ToporobotClasses2012,
  UnitEntitesExtended,
  BZGraphesTypes,
  BZGraphesClasses,
  BZArrayClasses
  ;

type TPathBetweenNodes = record
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
end;
//******************************************************************************

type TPathFindingGraphe = class(TBZGraphNode)
  strict private

    FDocuTopo: TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FXMini: double;
    FXMaxi: double;
    FYMini: double;
    FYMaxi: double;
    FListeDesPlusCourtsChemins: TListeSimple<TPathBetweenNodes>;
    FLastError: TGrapheLastError;
    FAfficherGraphe      : TProcOjObject;
    function SetLastError(const QErrCode: integer; const QErrMsg: string): boolean;
  private
    function   Reset(): boolean;
    function   FindNoeudByIDStation(const IDS: TIDStation; out ST: TBZClassNode; out IndexOf: TNumeroNoeud): boolean;
    procedure  SetMinMax();                // étendue du réseau
    procedure  PurgerDoublons();           // on jette les doublons
    procedure  PutNoeud(const Idx: integer; const QNoeud: TBZClassNode);
  public
    constructor Create;
    destructor Destroy;
    property  DocuTopo: TToporobotStructure2012 read FDocuTopo;
    property  BDDEntites: TBDDEntites read FBDDEntites;
    property  XMini: double read FXMini;
    property  YMini: double read FYMini;
    property  XMaxi: double read FXMaxi;
    property  YMaxi: double read FYMaxi;
    function  Initialiser(const FD : TToporobotStructure2012; const FE: TBDDEntites): boolean;
    procedure Finaliser();
    function  ConstruireGraphe(): boolean;
    // Les utilitaires
    function  GetLastError(): TGrapheLastError;
    function  FormatterTIDStation(const QId: TIDStation): string;
    procedure ListerLesNoeuds(const Caption: string; const DoDisplayDependances: boolean = false);
    // Les noeuds
    procedure  BeginNodeList();  // initialise la liste des noeuds
      // QSerie et QStation forment le label du sommet au format '%d.%d'
      procedure  AddStation(const QSerie, QStation: integer; const QX, QY, QZ: double; const QMetaData: string);
      function   GetStation(const Idx: integer): TBZClassNode;
      function   GetNbStations(): integer;
    procedure  EndNodesList();  // clôture cette liste et effectue les traitements préparatoires (tris, ...)
    function   AddArcBetweenStations(const Ser1, St1, Ser2, St2: integer; const Bidirectionnel: boolean = true; const Weight: double = 1.00): boolean;
    // spécifique stations topo
    function RechercherPlusCourtChemin(var MyPath: TPathBetweenNodes): boolean;

    function SaveToFile(const Filename: RawByteString): boolean;
    function FindStationByXY(const QX, QY: double; out BP: TBZClassNode): boolean;
    // liste des plus courts chemins
    procedure AddItineraire(const P: TPathBetweenNodes);
    function  GetItineraire(const Idx: integer; out P: TPathBetweenNodes): boolean;
    procedure PutItineraire(const Idx: integer; const P: TPathBetweenNodes);
    procedure RemoveItineraire(const Idx: integer);
    function  GetNbItineraires(): integer;

    function  LoadItinerairesFromFile(const Filename: RawByteString): boolean;
    procedure SaveItinerairesToFile(const Filename: RawByteString);
    // générer un objet Javascript contenant le graphe
    procedure ExporterGrapheEnJavascript(const DocTitle: string;
                                         const FileNameJS: RawByteString;
                                         const QIDStationDepart, QIDStationArrivee: string;
                                         const BackgroundColorCarte, CouleurCenterlines: TColor;
                                         const CanvasWidthInPixels, CanvasHeightInPixels, WidthMenuLateralInPixels: integer);
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
function TPathFindingGraphe.FormatterTIDStation(const QId: TIDStation): string;
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
  FXMini :=  Infinity;
  FYMini :=  Infinity;
  FXMaxi := -Infinity;
  FYMaxi := -Infinity;
  Nb := GetNbStations();
  for i := 0 to Nb -1 do
  begin
    MyNoeud := GetStation(i);
    FXMaxi := Max(FXMaxi, MyNoeud.X);
    FYMaxi := Max(FYMaxi, MyNoeud.Y);
    FXMini := Min(FXMini, MyNoeud.X);
    FYMini := Min(FYMini, MyNoeud.Y);
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
  AfficherMessageErreur(Format('%d noeuds (%f, %f) -> %f, %f', [Nb, self.XMini, self.YMini, self.XMaxi, self.YMaxi]));
  for i := 0 to Nb - 1 do
  begin
    MyStation := self.GetStation(i);
    ExtractSerStFromTIDStation(MyStation.IDStation, QSr, QSt);
    AfficherMessageErreur(Format(' %d: %d: %d.%d %f, %f, %f',
                    [i, MyStation.IDStation, QSr, QSt,
                     MyStation.X, MyStation.Y, MyStation.Z]));

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
  try
    AfficherMessageErreur(Format('%s.Initialiser()', [classname]));
    FDocuTopo   := FD;
    FBDDEntites := FE;
    result := true;
  except
  end;
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
    WU := (MyNoeud.X - QX) ** 2 + (MyNoeud.Y - QY) ** 2;
    if (WU < QDist) then
    begin
      Result := True;
      BP     := MyNoeud;
      QDist  := WU;
    end;
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
  self.AddStation(1, 0, MyEntrance.eXEntree, MyEntrance.eYEntree, MyEntrance.eZEntree, MyEntrance.eNomEntree);
  // les séries
  NbSeries := FDocuTopo.GetNbSeries();
  for NumSerie := 1 to NbSeries - 1 do
  begin
    MySerie := FDocuTopo.GetSerie(NumSerie);
    QSer0 := MySerie.GetNoSerieDep();
    QPt0  := myserie.GetNoPointDep();
    NbPts := MySerie.GetNbVisees();
    if (NbPts < 2) then Continue; // pas de visées, ou développement de la série nul --> []
    if (not FBDDEntites.FindJonctionBySerieSt(QSer0, QPt0, QJonction)) then
    begin
      SetLastError(ERR_GRAPHE_ARC_EXTR_DEB_NOT_FOUND, Format('Noeud départ %d %d.%d non trouvé', [Myserie.GetNumeroDeSerie(), Qser0, QPt0]));
      continue;
    end;
    self.AddStation(QJonction.NoSer, QJonction.NoSt, QJonction.X, QJonction.Y, QJonction.Z, QJonction.ToString());
    // s'il y a un seule station dans la série, on attrappe la station d'arrivée eton crée un arc
    if (2 = NbPts) then
    begin
      QSer1 := MySerie.GetNoSerieArr();
      QPt1  := myserie.GetNoPointArr();
      if (not FBDDEntites.FindJonctionBySerieSt(QSer1, QPt1, QJonction)) then Continue;
      self.AddStation(QJonction.NoSer, QJonction.NoSt, QJonction.X, QJonction.Y, QJonction.Z, QJonction.ToString());
      continue;
    end
    else
    begin
      // les points topo intermédiaires
      for QPt1 := 1 to NbPts - 1 do
      begin
        Qser1 := MySerie.GetNumeroDeSerie();
        if (FBDDEntites.GetEntiteViseeFromSerSt(Qser1, QPt1, QBP1)) then self.AddStation(QSer1, QPt1,  QBP1.PosStation.X, QBP1.PosStation.Y, QBP1.PosStation.Z, QBP1.oCommentaires);
      end;
      // et on ajoute la station de fin de la série
      QSer1 := MySerie.GetNoSerieArr();
      QPt1  := myserie.GetNoPointArr();
      if (not FBDDEntites.FindJonctionBySerieSt(QSer1, QPt1, QJonction)) then Continue;
      self.AddStation(QJonction.NoSer, QJonction.NoSt, QJonction.X, QJonction.Y, QJonction.Z, QJonction.ToString());
    end;
  end;
    self.EndNodesList();
    AfficherMessageErreur('--> Arcs');
    // Passe 2: Les arcs
    MyEntrance := FBDDEntites.GetEntrance(0);
    // les séries
    NbSeries := FDocuTopo.GetNbSeries();
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
    Result := True;
end;

function TPathFindingGraphe.GetNbStations(): integer;
begin
  Result := self.Count;
end;

procedure TPathFindingGraphe.AddStation(const QSerie, QStation: integer; const QX, QY, QZ: double; const QMetaData: string);
var
  FN: TBZClassNode;
begin
  // AddNode retourne le noeud ajouté
  FN := self.AddNode(MakeTIDBaseStation(QSerie, QStation, false), QX, QY, QZ, QMetaData);
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
function TPathFindingGraphe.FindNoeudByIDStation(const IDS: TIDStation; out ST: TBZClassNode; out IndexOf: TNumeroNoeud): boolean;
  function FindDepth(const I1, I2: TNumeroNoeud; const QIDX: TIDStation): TNumeroNoeud;
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
  ST     := GetStation(IndexOf);
  Exit(True);
  //else raise Exception.Create('Noeud introuvable pour l''étiquette ' + inttostr(IDS));
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
    Q1 := FindNoeudByIDStation(MakeTIDBaseStation(MyPath.SerieDepart, MyPath.StationDepart, false), QNoeudDepart, IdxNoeudDepart);
    if (Not Q1) then Exit(SetLastError(ERR_GRAPHE_NODE_NOT_FOUND, Format('Noeud "%d.%d" introuvable', [MyPath.SerieDepart, MyPath.StationDepart])));
    Q1 := FindNoeudByIDStation(MakeTIDBaseStation(MyPath.SerieArrivee, MyPath.StationArrivee, false), QNoeudArrivee, IdxNoeudArrivee);
    if (Not Q1) then Exit(SetLastError(ERR_GRAPHE_NODE_NOT_FOUND, Format('Noeud "%d.%d" introuvable', [MyPath.SerieArrivee, MyPath.StationArrivee])));
    //AfficherMessage(Format('%s.RechercherPlusCourtChemin(): %d: %d.%d -> %d: %d.%d', [ClassName, IdxNoeudDepart, FSerieDepart, FStationDepart, IdxNoeudArrivee, FSerieArrivee, FStationArrivee]));
    // Stations identiques -->[ ]
    Q1 := (MyPath.SerieDepart = MyPath.SerieArrivee) and (MyPath.StationDepart = MyPath.StationArrivee);
    if (Q1) then Exit(SetLastError(ERR_GRAPHE_SAME_START_END, '-- Les stations de départ et arrivée sont identiques'));
    //AfficherMessage('');
    //AfficherMessage('Début de la recherche du chemin le plus court');
    //AfficherMessage('==========================================================');
    QDist := self.Dijkstra(IdxNoeudDepart, IdxNoeudArrivee, FShortestPath);
    //AfficherMessage(Format('Fin la recherche du chemin le plus court: QDist = %.2f', [QDist]));
    //AfficherMessage('==========================================================');
    QNbC := FShortestPath.Count;
    if (QNbC > 2) then
    begin
      MyPath.EmptyListeNoeuds();
      MyPath.LongueurParcours := 0.00;
      for i := 0 to (QNbC - 1) do MyPath.AddNoeud(FShortestPath.GetElement(QNbC - 1 - i));
      // calcul de la longueur du parcours
      for i := 1 to MyPath.GetNbNoeuds() -1 do
      begin
        StDepart  := self.GetStation(MyPath.GetNoeud(i-1));
        StArrivee := self.GetStation(MyPath.GetNoeud(i));
        MyPath.LongueurParcours += Hypot3D(StArrivee.X - StDepart.X,
                                           StArrivee.Y - StDepart.Y,
                                           StArrivee.Z - StDepart.Z);

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
                               FormatterNombreOOo(MyStation.X),
                               FormatterNombreOOo(MyStation.Y),
                               FormatterNombreOOo(MyStation.Z)]));
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
    for i := 0 to Nb  - 1 do
    begin
      AfficherMessage(Format(' --> Calcul de %d: %s', [i, MyITI.NomItineraire]));
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
  FMT_POSITION_DIV_IN_PIXELS = '         %s:%dpx;';
  FMT_FLOOR_DIFF_TWO_VALUES  = '%s.floor(0.05 * (%s - %s))';

  HTML_BUTTON_FORMAT = '<INPUT type="button" name="%s" style="width:%d%%;height:%dpx;" value="%s" onclick="%s()">';
  ETENDUE_X_MAX_FOR_DISP_LBL_STATIONS  = 300.00; // étendue max pour affichage des labels des stations
  HAUTEUR_FORM_SEARCH_IN_PIXELS        = 78;
  HAUTEUR_PNL_NAVIG_IN_PIXELS          = 160;
  HAUTEUR_FORM_SHORTEST_PATH_IN_PIXELS = 120;

  SIZE_STATION_MARKER        = 2;
  NAMEREF_TITRE              = 'Titre';
  NAMEREF_PANNEAU_ROADMAP    = 'PanneauRoadMap';
  NAMEREF_PANNEAU_BTNS_VUE   = 'PanneauBtnsVue';
  NAMEREF_FORM_SHORTEST_PATH = 'PanneauShortestPath';
  NAMEREF_FORM_SEARCH        = 'FormRechercheStation';
  NAMEREF_CANVAS_CONTAINER   = 'MyPlan';
  NAMEREF_CANVAS_2D          = 'MyCanvas2D';
  NAMEREF_CANVAS_DC          = 'AC_DC';

  JS_SET_TITLE_DOC           = 'SetTitleDoc';

  JS_CONST_INFINITY          = 'Infinity';
  JS_GLOBAL_VAR_FXMini       = 'FGlobalFXMini';
  JS_GLOBAL_VAR_FXMaxi       = 'FGlobalFXMaxi';
  JS_GLOBAL_VAR_FYMini       = 'FGlobalFYMini';
  JS_GLOBAL_VAR_FYMaxi       = 'FGlobalFYMaxi';
  JS_GLOBAL_VAR_FXCentreMap  = 'FGlobalFXCentreMap';
  JS_GLOBAL_VAR_FYCentreMap  = 'FGlobalFYCentreMap';
  JS_GLOBAL_VAR_FLastIdxFound = 'FGlobalLastIdxFound';

  JS_GLOBAL_VAR_FGlobalNoeudCourant  = 'FGlobalNoeudCourant';

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
  MIME_TEXT_JAVASCRIPT       = 'text/javascript';

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
  JS_FUNCTION_DistanceBetweenNodes     = 'DistanceBetweenNodes';
  JS_FUNCTION_GetNbStationsOfPathFound = 'GetNbStationsOfPathFound';
  JS_FUNCTION_AddIdxNoeudAtPathFound   = 'AddIdxNoeudAtPathFound';
  JS_FUNCTION_GetIdxNoeudOfPathFound   = 'GetIdxNoeudOfPathFound';
  JS_FUNCTION_CalcCheminMinimalBetweenTwoNodes = 'CalcCheminMinimalBetweenTwoNodes';
  JS_FUNCTION_DRAW_GRAPHE     = 'DrawGraphe';
  JS_VAR_FLAG_DO_DRAW_GRAPHE  = 'false';



  JS_FUNCTION_CentrerSurPointXY    = 'CentrerSurPointXY';
  JS_FUNCTION_NbNoeudsVisites      = 'NbNoeudsVisites';
  JS_FUNCTION_TrouveMin            = 'TrouveMin';
  JS_FUNCTION_ListerRoadMap        = 'ListerRoadMap';

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

  JS_VARNAME_dx   = 'dx';
  JS_VARNAME_dy   = 'dy';
  JS_VARNAME_dz   = 'dz';


  JS_VAR_NbNoeuds            = 'NbNoeuds';

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
  JS_VUE_ResetVue        = 'ResetVue';
  JS_VUE_ZoomPlus        = 'ZoomPlus';
  JS_VUE_ZoomMoins       = 'ZoomMoins';
  JS_VUE_PanVueU         = 'PanVueUp';
  JS_VUE_PanVueD         = 'PanVueDown';
  JS_VUE_PanVueL         = 'PanVueLeft';
  JS_VUE_PanVueR         = 'PanVueRight';
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

  procedure WriteLine(const S: string); inline;
  begin
    WriteLn(fp, S);
  end;

  procedure WrtLinFmt(Const Fmt : String; const Args : Array of const);
  begin
    WriteLn(fp, Format(Fmt, Args));
  end;
  procedure WriteDebugLine(const S: string); inline;
  begin
    WriteLn(fp, Format('console.log(%s);', [S]));
  end;
  procedure WrtDebugFmtLin(const fmt: string; const Args : Array of const); inline;
  begin
    WriteDebugLine(Format(Fmt, Args));
  end;
  procedure jsSetLocalVar(const QVarName: string; const QExpr: string; const QComment: string = ''); overload;
  begin
    WrtLinFmt('  var %s = %s; // %s', [QVarName, QExpr, QComment]);
  end;
  procedure jsSetLocalVar(const QVarName: string; const QValue: integer; const QComment: string = ''); overload;
  begin
    WrtLinFmt('  var %s = %d; // %s', [QVarName, QValue, QComment]);
  end;
  procedure jsSetLocalVar(const QVarName: string; const QValue: double; const QComment: string = ''); overload;
  begin
    WrtLinFmt('  var %s = %.10f; // %s', [QVarName, QValue, QComment]);
  end;


  procedure JSSeparateur(const QMotif: Char = '='; const nb: integer = 80);
  begin
    WriteLine('//' + StringOfChar(QMotif, nb));
  end;
  procedure BeginDiv(const QID: string; const QIndent: integer = 2);
  begin
    WrtLinFmt('%s<div id="%s">', [StringOfChar(' ', QIndent), QID]);
  end;
  procedure EndDiv(const QIndent: integer = 2);
  begin
    WrtLinFmt('%s</div>', [StringOfChar(' ', QIndent)]);
  end;
  procedure BeginForm(const QName: string; const QIndent: integer = 4);
  begin
    WrtLinFmt('%s<form name="%s">', [StringOfChar(' ', QIndent), QName]);
  end;
  procedure EndForm(const QIndent: integer = 4);
  begin
    WrtLinFmt('%s</form>', [StringOfChar(' ', QIndent)]);
  end;
  procedure BeginTable(const WidthInPercent, HeightInPercent: integer; const QIndent: integer = 4); overload;
  begin
    WrtLinFmt('    %s<table style="width:%d%%;height:%d%%;">', [StringOfChar(' ', QIndent), WidthInPercent, HeightInPercent]);
  end;
  procedure BeginRow(const QIndent: integer = 6); overload;
  begin
    WrtLinFmt('    %s<tr>', [StringOfChar(' ', QIndent)]);
  end;
  procedure EndRow(const QIndent: integer = 6); overload;
  begin
    WrtLinFmt('    %s</tr>', [StringOfChar(' ', QIndent)]);
  end;
  procedure EndTable(const QIndent: integer = 4);
  begin
    WrtLinFmt('    %s</table>', [StringOfChar(' ', QIndent)]);
  end;
  procedure BeginJSFunction(const QProcName: string; const QParams: string = '');
  begin
    FCurrentProcName := QProcName;
    WrtLinFmt('// BeginJSFunction: *** ----- %s -- %s', [FCurrentClassName, FCurrentProcName]);
    WrtLinFmt('function %s(%s)', [FCurrentProcName, QParams]);
    WriteLine('{');
  end;
  procedure EndJSFunction();
  begin
    WrtLinFmt('} // EndJSFunction %s ', [FCurrentProcName]);
    JSSeparateur('-', 60);
  end;
  procedure BeginClassJSFunction(const QClassName, QMethod: string; const QParams: string = '');
  begin
    FCurrentClassName:= QClassName;
    FCurrentProcName := QMethod;
    WrtLinFmt('// BeginClassJSFunction: *** ----- %s -- %s', [FCurrentClassName, FCurrentProcName]);
    WrtLinFmt('  %s.%s = function(%s)', [FCurrentClassName, FCurrentProcName, QParams]);
    WriteLine('   {');

  end;
  procedure EndClassJSFunction();
  begin
    WrtLinFmt('  } // EndClassJSFunction // %s.%s', [FCurrentClassName, FCurrentProcName]);
    JSSeparateur('-', 60);
  end;
  procedure BeginJSEventListener(const EventName: string; const Desc: string = '');
  begin
    WrtLinFmt('  // Listener: %s - %s', [EventName, Desc]);
    WrtLinFmt('  %s.addEventListener("%s", function(e) {', ['window', EventName]);
  end;
  procedure EndJSEventListener();
  begin
    WriteLine('  });');
    JSSeparateur('-', 60);
  end;

  procedure jsFOR(const QVarCounter, QVarNb: string; const QStart: integer = 0; const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%sfor (var %s = %d; %s < %s; %s++) // -- FOR %s', [WU, QVarCounter, QStart, QVarCounter, QVarNb, QVarCounter, QTag]);
    WrtLinFmt('%s{', [WU]);
  end;
  procedure jsNEXT(const QVarCounter: string; const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%s} // -- NEXT %s ... %s', [WU, QVarCounter, QTag]);
  end;
  procedure jsWHILE(const QCondition: string; const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%swhile (%s) // -- WHILE %s', [WU, QCondition, QTag]);
    WrtLinFmt('%s{', [WU]);
  end;
  procedure jsWEND(const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%s} // -- WEND %s', [WU, QTag]);
  end;
  procedure jsIF(const QCondition: string; const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%sif (%s) // %s', [WU, QCondition, QTag]);
    WrtLinFmt('%s{', [WU]);
  end;
  procedure jsELSE(const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%s} else { // %s', [WU, QTag]);
  end;
  procedure jsENDIF(const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%s} // %s', [WU, QTag]);
  end;

  procedure jsSELECT_CASE(const QSelecteur: string; const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%sswitch(%s) { // %s', [WU, QSelecteur, QTag]);
  end;
  procedure jsCASE(const QItem: integer; const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%s  case %d: // %s', [WU, QItem, QTag]);
  end;
  procedure jsBREAK(const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%s    break;', [WU]);
  end;
  procedure jsCASE_ELSE(const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%s  default: // %s', [WU, QTag]);
  end;
  procedure jsEND_SELECT(const QTag: string = ''; const QIndent: integer = 4);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', QIndent);
    WrtLinFmt('%s} // switch %s', [WU, QTag]);
  end;

  function QColorToHTMLColor(const Couleur: TColor): string;
  begin
    Result := Format('#%.2X%.2X%.2X', [Red(Couleur), Green(Couleur), Blue(Couleur)]);
  end;
  function ToJSCoord(const X: double): integer;
  begin
    result := trunc(TO_DECIMETERS * X);
  end;
  procedure JSSetMemberAtThis(const Varname: string);
  begin
    WrtLinFmt('  %s.%s = Q%s;', [JS_THIS, Varname, Varname]);
  end;
  procedure JSMakeButtonInTable(const btnName, btnCaption, btnOnClick: string);
  var
    MyStyleButton: String;
  begin
    MyStyleButton := Format('width:%d%%;height:%d%%;', [100, 100]) +
                     format('font-family:%s,%s;', ['arial', 'sans-serif']) +
                     Format('font-size:%dpx;', [24]) +
                     Format('font-weight:%s;', ['bold']);

    WrtLinFmt('    <TD width="%d%%" height="%d%%"><INPUT type="button" name="%s" value="%s" style="%s" onclick="%s()"> </TD>',
               [33, 33, btnName, btnCaption, MyStyleButton, btnOnClick]);
  end;
  procedure JSMakeLabelledEdit(const LbName, LbCaption: string; const EditName, EditText: string);
  begin
    WrtLinFmt('    <LABEL for="%s">%s</LABEL><BR>'    , [LbName, LbCaption]);
    WrtLinFmt('    <INPUT type="text" id="%s" style="width:%d%%" name="%s" value="%s"><BR>', [EditName, 95, EditName, EditText]);
  end;
  procedure JSTraceLigne(const SX1, SY1, SX2, SY2: string);
  begin

    WrtLinFmt('  %s(%s, %s);', [JS_FUNC_CoordsToPlan, SX1, SY1]);
    WriteLine('  PS1X = PX;');
    WriteLine('  PS1Y = PY;');
    WrtLinFmt('  %s(%s, %s);', [JS_FUNC_CoordsToPlan, SX2, SY2]);
    WriteLine('  PS2X = PX;');
    WriteLine('  PS2Y = PY;');
    WrtLinFmt('  %s.beginPath();'         , [NAMEREF_CANVAS_DC]);
    WrtLinFmt('  %s.moveTo(PS1X, PS1Y);'  , [NAMEREF_CANVAS_DC]);
    WrtLinFmt('  %s.lineTo(PS2X, PS2Y);'  , [NAMEREF_CANVAS_DC]);
    WrtLinFmt('  %s.stroke();'            , [NAMEREF_CANVAS_DC]);
  end;

  procedure JSToto(const BeginOrEndPath, JS_FRM_EditStation, JS_Varname_Station, JS_Varname_IdxStation: string);
  begin
    WrtLinFmt('  var %s   = %s.value;', [JS_Varname_Station, JS_FRM_EditStation]);
    WrtLinFmt('  var %s = %s.FindIdxNoeudByCle(%s);', [JS_Varname_IdxStation, JS_GRAPHE_CLASS_VAR_NAME, JS_Varname_Station]);
    jsIF(Format('%s < 0', [JS_Varname_IdxStation]), '');
      WrtLinFmt('     window.alert("Station %s ''" + %s + "'' introuvable");', [BeginOrEndPath, JS_Varname_Station]);
      WriteLine('     return;');
    jsENDIF();
  end;
  procedure QDrawShortestPath(const DoDrawEtiquettes: boolean; const LineWidth: integer; const LC: TColor; const LAlpha: byte);
  begin

    WrtLinFmt('    %s.strokeStyle = "rgba(%d, %d, %d, %.0f)";',[NAMEREF_CANVAS_DC, Red(LC), Green(LC), Blue(LC), LAlpha/256.0]);
    WrtLinFmt('    %s.lineWidth   = %d;',[NAMEREF_CANVAS_DC, LineWidth]);

    WrtLinFmt('    var %s = %s.GetNoeud(%s.%s(0));',[JS_VARNAME_CURRENT_NODE, JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound]);
    WrtLinFmt('    %s.beginPath();',[NAMEREF_CANVAS_DC]);
    WrtLinFmt('    %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
    WriteLine('    PS1X = PX;');
    WriteLine('    PS1Y = PY;');
    WrtLinFmt('    %s.moveTo(PS1X, PS1Y);', [NAMEREF_CANVAS_DC]);
    jsFOR('i', 'Nb', 0, '', 4);
      WrtLinFmt('      var %s = %s.GetNoeud(%s.%s(i));', [JS_VARNAME_CURRENT_NODE, JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound]);
      WrtLinFmt('      %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
      WriteLine('      PS1X = PX;');
      WriteLine('      PS1Y = PY;');
      WrtLinFmt('      %s.lineTo(PS1X, PS1Y);', [NAMEREF_CANVAS_DC]);
      if (DoDrawEtiquettes) then WrtLinFmt('      %s.fillText(%s.IDNoeud, PX + %d, PY + %d);', [NAMEREF_CANVAS_DC, JS_VARNAME_CURRENT_NODE, SIZE_STATION_MARKER + 1, SIZE_STATION_MARKER + 1]);
    jsNEXT('i');
    WrtLinFmt('    %s.stroke();', [NAMEREF_CANVAS_DC]);
  end;

  procedure BeginCSSClass(const QCSSClass: string);
  begin
    WrtLinFmt('       #%s{', [QCSSClass]);
  end;
  procedure EndCSSClass();
  begin
    WriteLine('       }');
  end;

  procedure PositionnerPanneau(const QNamePanneau: string;
                               const QLeft, QTop, QWidth, QHeight: integer;
                               const QBorderStyle: string;
                               const QBorderColor, QBackGroundColor: TColor;
                               const QFontName: string   = 'Arial';
                               const QFontSize: integer  = 15;
                               const QFontAttr: string   = 'bold');
  begin
    BeginCSSClass(QNamePanneau);         //   WrtLinFmt('       #%s{', []);
      WrtLinFmt('         %s:%s;'       , ['position', 'absolute']);
      WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS, ['left'  , QLeft]);
      WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS, ['top'   , QTop]);
      WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS, ['width' , QWidth]);
      WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS, ['height', QHeight]);
      WrtLinFmt('         %s:%s;'       , ['border-style', QBorderStyle]);
      WrtLinFmt('         %s:%s;'       , ['color'     , QColorToHTMLColor(QBorderColor)]);
      WrtLinFmt('         %s:%s;'       , ['background', QColorToHTMLColor(QBackGroundColor)]);
      WrtLinFmt('         %s:%dpx %s;'  , ['font', QFontSize, QFontName]);
      WrtLinFmt('         %s:%s;'       , ['font-weight', QFontAttr]);
    EndCSSClass();
  end;
  // rédaction des routines JS
  procedure QRedigeClasseTJSGrapheNode();
  const
    CLASSNAME_TJSGrapheNode = 'TJSGrapheNode';
  begin
    BeginJSFunction(CLASSNAME_TJSGrapheNode, JS_ARGS_ADD_NOEUD);
      JSSetMemberAtThis('IDNoeud');
      JSSetMemberAtThis('X');
      JSSetMemberAtThis('Y');
      JSSetMemberAtThis('Z');
      JSSetMemberAtThis('ListeVoisins');
    EndJSFunction();
  end;
  procedure QBeginClasseTJSGraphe();
  begin

    WrtLinFmt('// La classe T%s', [JS_GRAPHE_CLASS_VAR_NAME]);
    WrtLinFmt('function T%s() {', [JS_GRAPHE_CLASS_VAR_NAME]);
    WrtLinFmt('  %s.%s  = new Array();', [JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
    WrtLinFmt('  %s.%s    = new Array();', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);

    WrtLinFmt('  %s.FXMini       = %d;', [JS_THIS, ToJSCoord(self.FXMini)]);
    WrtLinFmt('  %s.FYMini       = %d;', [JS_THIS, ToJSCoord(self.FYMini)]);

    WrtLinFmt('  %s.FXMaxi       = %d;', [JS_THIS, ToJSCoord(self.FXMaxi)]);
    WrtLinFmt('  %s.FYMaxi       = %d;', [JS_THIS, ToJSCoord(self.FYMaxi)]);
    BeginClassJSFunction(JS_THIS, 'Init');
      pass;
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, 'AddNoeud', JS_ARGS_ADD_NOEUD);
      WrtLinFmt('    %s.%s.push(new TJSGrapheNode(%s));', [JS_THIS, JS_GRAPH_LISTE_NOEUDS, JS_ARGS_ADD_NOEUD]);
      WrtLinFmt('    %s.FNbNoeuds = %s.%s.length;', [JS_THIS, JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, 'GetNoeud', 'Index');
      WrtLinFmt('    return %s.%s[Index];', [JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, 'GetNbNoeuds');
      WrtLinFmt('    return %s.%s.length;', [JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, 'FindIdxNoeudByCle', 'Cle');
      jsSetLocalVar('Nb', Format('%s.GetNbNoeuds()', [JS_THIS]), 'Nombre de noeuds (test)');
      WriteLine('    if (0 == Nb) return -1;');
      jsFOR('i', 'Nb', 0, '', 4);
        WrtLinFmt('      if (%s.%s[i].IDNoeud === Cle) return i;', [JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
      jsNEXT('i');
      WriteLine('    return -1;');
    EndClassJSFunction();
    WriteLine('  //----------------------------------------------------------');
  end;
  procedure QEndClasseTJSGraphe();
  begin
    WriteLine('} // End class' + JS_GRAPHE_CLASS_VAR_NAME);
    JSSeparateur('+', 132);
  end;

  procedure QRedigeDeclGlobalVars();
  const
    VAR_FMT_NUM   = 'var %s = %d;';
    VAR_FMT_UNDEF = 'var %s;';
  begin
    WriteLine('');
    WriteLine('// Variables globales');
    WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FXMini, 0]);
    WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FXMaxi, 0]);
    WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FYMini, 0]);
    WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FYMaxi, 0]);
    WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FXCentreMap, 0]);
    WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FYCentreMap, 0]);
    WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FLastIdxFound, -1]);
    WrtLinFmt('var %s = new T%s;', [JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME]);
    WriteLine('// Variables des contrôles');
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyContainer]);
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyCanvas]);
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MylbMouseCoords]);
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MylbRoadmapNbPoints]);
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MylsbRoadMap]);
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyeditFindWhat]);
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyeditStationDepart]);
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyeditStationArrivee]);
    WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_FGlobalNoeudCourant]);
    WriteLine('// Variables globales pour mettre en cache l''état de l''événement');
    WrtLinFmt('var %s = new Array();', [JS_GLOBAL_VAR_EvtCache]);
    WrtLinFmt('var %s = %d;', [JS_GLOBAL_VAR_PrevDiff, -1]);
    JSSeparateur('=', 120);
  end;
  procedure QRedigeClassJSGrapheFunctionFindIdxNoeudNearestToXY();
  const FMT_QDXY = '(%s - %s.%s[i].%s)';
  begin
    BeginClassJSFunction(JS_THIS, 'FindIdxNoeudNearestToXY', 'QX, QY');
      jsSetLocalVar('Result', -1);           //      WrtLinFmt('    var %s = %d;', ['Result', -1]);
      jsSetLocalVar('Nb', format('%s.GetNbNoeuds()', [JS_THIS]), 'cuicui');  ; //   WrtLinFmt('    var Nb = %s.GetNbNoeuds();', [JS_THIS]);
      WriteLine('    if (0 == Nb) return -1;');
      jsSetLocalVar('QDistance', JS_CONST_INFINITY, 'Infinité');//      WrtLinFmt('    var %s = %s;', ['QDistance', JS_CONST_INFINITY]);
      jsFOR('i', 'Nb', 0, '', 4);
        jsSetLocalVar('qdx', Format(FMT_QDXY, ['QX', JS_THIS, JS_GRAPH_LISTE_NOEUDS, 'X']), 'Qdx'); // WrtLinFmt('       var %s = (QX - %s.%s[i].%s);', ['qdx', JS_THIS, JS_GRAPH_LISTE_NOEUDS, 'X']);
        jsSetLocalVar('qdy', Format(FMT_QDXY, ['QY', JS_THIS, JS_GRAPH_LISTE_NOEUDS, 'Y']), 'Qdy'); // WrtLinFmt('       var %s = (QY - %s.%s[i].%s);', ['qdy', JS_THIS, JS_GRAPH_LISTE_NOEUDS, 'Y']);
        jsSetLocalVar('qdist', format('%s * %s  + %s * %s', ['qdx', 'qdx', 'qdy', 'qdy']), 'distance en plan');  //        WrtLinFmt('       var %s = %s * %s  + %s * %s;', ['qdist', 'qdx', 'qdx', 'qdy', 'qdy']);
        jsIF(format('%s < %s', ['qdist', 'QDistance']), '', 8);
          WrtLinFmt('          %s = %s;', ['QDistance', 'qdist']);
          WrtLinFmt('          %s = %s;', ['Result', 'i']);
        jsENDIF('', 8);
      jsNEXT('i');
      WriteLine('    return Result;');
    EndClassJSFunction();
  end;
  procedure QRedigeClassJSGrapheFunctionDijkstra();
  const FMT_FIND_IDX_NOEUD = '%s.FindIdxNoeudByCle(%s)';
  begin
    WriteLine('  // Algo de Dijkstra');
    BeginClassJSFunction(JS_THIS, 'Dijkstra', 'StationDepart, StationArrivee');
      jsSetLocalVar(JS_VAR_IDX_ND_DEPART , Format(FMT_FIND_IDX_NOEUD, [JS_THIS, 'StationDepart']) , 'Recherche départ');
      jsSetLocalVar(JS_VAR_IDX_ND_ARRIVEE, Format(FMT_FIND_IDX_NOEUD, [JS_THIS, 'StationArrivee']), 'Recherche arrivée');
      //WriteLine('    console.log("Dijkstra: From " + StationDepart + " To " + StationArrivee);');
      //WrtLinFmt('    console.log("Dijkstra: From " + %s.toString() + "  To " + %s.toString());', [JS_VAR_IDX_ND_DEPART, JS_VAR_IDX_ND_ARRIVEE]);
      WrtLinFmt('    if (%s == -1) return -1;', [JS_VAR_IDX_ND_DEPART]);
      WrtLinFmt('    if (%s == -1) return -2;', [JS_VAR_IDX_ND_ARRIVEE]);
      WrtLinFmt('    if (%s == %s) return -3;', [JS_VAR_IDX_ND_DEPART, JS_VAR_IDX_ND_ARRIVEE]);
      //WriteLine('    console.log("Dijkstra: 001");');
      WrtLinFmt('    %s.%s  = []; // vider le chemin le plus court', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
      WriteLine('    // Variables locales');
      jsSetLocalVar(JS_VARNAME_CURRENT_NODE, 'null');
      jsSetLocalVar('MyNodeVoisin', 'null');
      jsSetLocalVar('QIdxVoisin', 0);
      jsSetLocalVar('NbVoisins' , 0);
      jsSetLocalVar('QDistance' , 0.00);


      //WriteLine('    console.log("Dijkstra: 002");');

      jsSetLocalVar(JS_VAR_NbNoeuds, format('%s.GetNbNoeuds()', [JS_THIS]), 'miaou');

      jsSetLocalVar(JS_LOCAL_ListePreds,  'new Array()', 'Liste des prédécesseurs');
      jsFOR('i', JS_VAR_NbNoeuds, 0, '', 4);
        WrtLinFmt('      %s[i] = %d;', [JS_LOCAL_ListePreds, 0]);
      jsNEXT('i');

      jsSetLocalVar(JS_LOCAL_ListeDistances,  'new Array()', 'Liste des distances');
      jsFOR('i', JS_VAR_NbNoeuds, 0, '', 4);
        WrtLinFmt('   %s[i] = %s; ', [JS_LOCAL_ListeDistances, JS_CONST_INFINITY]);
      jsNEXT('i', '', 4);
      WrtLinFmt('    %s[%s] = 0;', [JS_LOCAL_ListeDistances, JS_VAR_IDX_ND_DEPART]);

      jsSetLocalVar(JS_LOCAL_ListeNdsVisites,  'new Array()', 'Liste des déjà vus');
      jsFOR('i', JS_VAR_NbNoeuds, 0, '', 4);
        WrtLinFmt('     %s[i] = false;', [JS_LOCAL_ListeNdsVisites]);
      jsNEXT('i');

      WriteLine('    // Retourne le nombre de noeuds visités');
      WrtLinFmt('    function %s() // Nested', [JS_FUNCTION_NbNoeudsVisites]);
      WriteLine('    {');
      WriteLine('      Result = 0;');
      jsFOR('i', JS_VAR_NbNoeuds, 0, '', 4);
        WrtLinFmt('     if (%s[i]) Result++;', [JS_LOCAL_ListeNdsVisites]);
      jsNEXT('i');
      WriteLine('      return Result;');
      WrtLinFmt('    } // nested function %s()', [JS_FUNCTION_NbNoeudsVisites]);

      WriteLine('    // Retourne le noeud le plus proche dans les noeuds non visités');
      WrtLinFmt('    function %s() // Nested', [JS_FUNCTION_TrouveMin]) ;
      WriteLine('    {');
      jsSetLocalVar('Result', -1);
      jsSetLocalVar('DistMini', JS_CONST_INFINITY);
      jsFOR('i', JS_VAR_NbNoeuds, 0, '', 6);
        jsIF(Format('!%s[i]', [JS_LOCAL_ListeNdsVisites]), '', 8);
          jsIF(Format('%s[i] < DistMini', [JS_LOCAL_ListeDistances]), '', 10);
            WrtLinFmt('            DistMini = %s[i];', [JS_LOCAL_ListeDistances]);
            WriteLine('            Result = i;');
          jsENDIF('', 10);
        jsENDIF('', 8);
      jsNEXT('i', '', 6);
      WriteLine('      return Result;');
      WrtLinFmt('    } // nested function %s()', [JS_FUNCTION_TrouveMin]);
      //WriteLine('    console.log("Dijkstra: 005");');
      //WriteLine('    console.log("Entre dans le while du sujet")');
      WriteLine('    // ');
      jsWHILE(Format('%s() < %s', [JS_FUNCTION_NbNoeudsVisites, JS_VAR_NbNoeuds]), 'Boucle while principale', 4);
        jsSetLocalVar('N1', format('%s()', [JS_FUNCTION_TrouveMin]), '');
        WrtLinFmt('       if (N1 == %s) break;', [JS_VAR_IDX_ND_ARRIVEE]);
        WrtLinFmt('       %s[N1] = true;', [JS_LOCAL_ListeNdsVisites]);
        WrtLinFmt('       %s = %s.GetNoeud(N1);', [JS_VARNAME_CURRENT_NODE, JS_THIS]);
        //WrtLinFmt('       console.log("Noeud: "+ N1.toString() + " - ID = " + %s.IDNoeud);',[JS_VAR_NbNoeuds]);
        WrtLinFmt('       NbVoisins = %s.ListeVoisins.length;', [JS_VARNAME_CURRENT_NODE]);
        jsIF('NbVoisins > 0', '', 8);
          jsFOR('k', 'NbVoisins', 0, '', 10);
            WrtLinFmt('           QIdxVoisin = %s.ListeVoisins[k];', [JS_VARNAME_CURRENT_NODE]);
            WriteLine('           // fonction MajDistances() déroulée ');
            WrtLinFmt('           var Poids = %s.%s(N1, QIdxVoisin);', [JS_THIS, JS_FUNCTION_DistanceBetweenNodes]);
            jsIF(Format('%s[QIdxVoisin] > (%s[N1] + Poids)', [JS_LOCAL_ListeDistances, JS_LOCAL_ListeDistances]), '', 12);
              WrtLinFmt('             %s[QIdxVoisin] = %s[N1] + Poids;', [JS_LOCAL_ListeDistances, JS_LOCAL_ListeDistances]);
              WrtLinFmt('             %s[QIdxVoisin] = N1;', [JS_LOCAL_ListePreds]);
            jsENDIF('', 12);
          jsNEXT('k', '', 10);
        jsENDIF('', 8);
      jsWEND('', 4);
      WriteLine('    // Et maintenant, on remonte');
      WrtLinFmt('    %s.%s = [];', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
      jsSetLocalVar('N2', JS_VAR_IDX_ND_ARRIVEE, '');
      jsWHILE(format('N2 != %s', [JS_VAR_IDX_ND_DEPART]), 'While de remontée', 4);
        WrtLinFmt('      %s.%s(N2);', [JS_THIS, JS_FUNCTION_AddIdxNoeudAtPathFound]);
        WrtLinFmt('      N2 = %s[N2];', [JS_LOCAL_ListePreds]);
      jsWEND('', 4);
    EndClassJSFunction();
  end;
  procedure QRedigeClassJSGrapheFunctionsPathFound();
  begin
    BeginClassJSFunction(JS_THIS, JS_FUNCTION_AddIdxNoeudAtPathFound, 'IdxNoeud');
      WriteLine('    // L''algo Dijkstra construit la liste des parcourus "à l''envers"');
      WriteLine('    // aussi, l''ajout se fait en début de tableau"');
      WrtLinFmt('    %s.%s.splice(%d, %d, IdxNoeud);', [JS_THIS, JS_GRAPH_SHORTEST_PATH, 0, 0]);
      WrtLinFmt('    // %s.%s.push(IdxNoeud);', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, JS_FUNCTION_GetIdxNoeudOfPathFound, 'IdxNoeud');
      WrtLinFmt('    return %s.%s[IdxNoeud];', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, JS_FUNCTION_GetNbStationsOfPathFound, '');
      WrtLinFmt('    return %s.%s.length;', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
    EndClassJSFunction();
  end;
  procedure QRedigeClassJSGrapheFunctionsGetBounds();
  begin
    WriteLine('  // GetBounds');
    BeginClassJSFunction(JS_THIS, 'GetXMini', '');
      WrtLinFmt('    return %s.FXMini;', [JS_THIS]);
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, 'GetYMini', '');
      WrtLinFmt('    return %s.FYMini;', [JS_THIS]);
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, 'GetXMaxi', '');
      WrtLinFmt('    return %s.FXMaxi;', [JS_THIS]);
    EndClassJSFunction();
    BeginClassJSFunction(JS_THIS, 'GetYMaxi', '');
      WrtLinFmt('    return %s.FYMaxi;', [JS_THIS]);
    EndClassJSFunction();
  end;
  procedure QRedigeClassJSGrapheFunctionDistanceBetweenNodes();
  const FMT_GET_NOEUD = '%s.GetNoeud(%s)';
  begin
    BeginClassJSFunction(JS_THIS, JS_FUNCTION_DistanceBetweenNodes, Format('%s, %s', [JS_VAR_IDX_ND_DEPART, JS_VAR_IDX_ND_ARRIVEE]));
      jsSetLocalVar('ST1', format(FMT_GET_NOEUD, [JS_THIS, JS_VAR_IDX_ND_DEPART]), '');   //   WrtLinFmt('    var ST1 = %s.GetNoeud(%s);', []);
      jsSetLocalVar('ST2', format(FMT_GET_NOEUD, [JS_THIS, JS_VAR_IDX_ND_ARRIVEE]), '');  //   WrtLinFmt('    var ST2 = %s.GetNoeud(%s);', []);

      jsSetLocalVar(JS_VARNAME_dx, 'ST2.X - ST1.X', '');
      jsSetLocalVar(JS_VARNAME_dy, 'ST2.Y - ST1.Y', '');
      jsSetLocalVar('QDist', format('%s.sqrt(%s * %s + %s * %s)', [JS_MATH_LIBRARY, JS_VARNAME_dx, JS_VARNAME_dx, JS_VARNAME_dy, JS_VARNAME_dy]), 'distance');

      WriteLine('    if (QDist < 0.01) return(0.01)');
      WriteLine('                 else return QDist; // sécurisation: aucun poids nul autorisé dans Dijkstra');
    EndClassJSFunction();
  end;
  procedure QRedigeFunctionListerRoadMap();
  begin
    BeginJSFunction(JS_FUNCTION_ListerRoadMap);
      WrtLinFmt('   var Nb = %s.%s.length;', [JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPH_SHORTEST_PATH]);
      WriteLine('   if (Nb == 0) return;');
      WriteLine('   var LongParcours = 0;');
      WrtLinFmt('   %s.options.length = Nb;', [JS_DOCUMENT_FORM_lsbRoadMap]);
      jsFOR('i', 'Nb', 0, '', 4);
        WrtLinFmt('     var %s = %s.GetNoeud(%s.%s(i));', [JS_VARNAME_CURRENT_NODE , JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound]);
        jsIF('i >= 1', '', 6);
          WrtLinFmt('       var %s = %s.GetNoeud(%s.%s(i-1));', [JS_VARNAME_PREVIOUS_NODE, JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound]);
          WrtLinFmt('       var dx = %s.X - %s.X;', [JS_VARNAME_CURRENT_NODE, JS_VARNAME_PREVIOUS_NODE]);
          WrtLinFmt('       var dy = %s.Y - %s.Y;', [JS_VARNAME_CURRENT_NODE, JS_VARNAME_PREVIOUS_NODE]);
          WrtLinFmt('       LongParcours += %s.sqrt(dx*dx + dy*dy);', [JS_MATH_LIBRARY]);
        jsENDIF('', 6);
        WrtLinFmt('     %s.options[i].value = %s.IDNoeud;', [JS_DOCUMENT_FORM_lsbRoadMap, JS_VARNAME_CURRENT_NODE]);
        WrtLinFmt('     %s.options[i].text = %s.IDNoeud;', [JS_DOCUMENT_FORM_lsbRoadMap, JS_VARNAME_CURRENT_NODE]);
      jsNEXT('i', '', 4);
      WrtLinFmt('     LongParcours = %s.ceil(LongParcours / 10);', [JS_MATH_LIBRARY]);
      WrtLinFmt('   My%s.innerHTML    = Nb.toString() + " stations (" + LongParcours.toFixed(2) +" m)";', [JS_DOCUMENT_FORM_lbRoadmapNbPoints]);
    EndJSFunction();
  end;
  procedure QRedigeFunctionCalcCheminMinimalBetweenTwoNodes();
  begin
    BeginJSFunction(JS_FUNCTION_CalcCheminMinimalBetweenTwoNodes, 'St1, St2');
      WrtLinFmt('  %s.Dijkstra(St1, St2);  ', [JS_GRAPHE_CLASS_VAR_NAME]);
      WrtLinFmt('  %s(%s);', [JS_FUNCTION_DRAW_GRAPHE, JS_VAR_FLAG_DO_DRAW_GRAPHE]);
      WrtLinFmt('  %s();', [JS_FUNCTION_ListerRoadMap]);
      WrtLinFmt('  %s(St1, St2);', [JS_SET_TITLE_DOC]);
    EndJSFunction();
  end;
  procedure QRedigeCallbackOnResizeCanvas();
  begin
    BeginJSFunction(JS_CALLBACK_CANVAS_OnResizeCanvas);
      WrtDebugFmtLin('"Entre dans %s"', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
      jsSetLocalVar('r', format('%s.clientHeight / %s.clientWidth',  [JS_GLOBAL_VAR_MyContainer, JS_GLOBAL_VAR_MyContainer]), 'rapport H/W');
      WrtLinFmt('  %s = %s.floor(%s + r * (%s - %s));', [JS_GLOBAL_VAR_FYMaxi, JS_MATH_LIBRARY, JS_GLOBAL_VAR_FYMini, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]);
      WrtLinFmt('  %s = %s.floor(0.50 * (%s + %s));', [JS_GLOBAL_VAR_FXCentreMap, JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]);
      WrtLinFmt('  %s = %s.floor(0.50 * (%s + %s));', [JS_GLOBAL_VAR_FYCentreMap, JS_MATH_LIBRARY, JS_GLOBAL_VAR_FYMaxi, JS_GLOBAL_VAR_FYMini]);
      WrtLinFmt('  %s(%s);', [JS_FUNCTION_DRAW_GRAPHE, JS_VAR_FLAG_DO_DRAW_GRAPHE]);
    EndJSFunction();
  end;
  procedure QRedigeCallbackOnClickCanvas();
  begin
    BeginJSFunction(JS_CALLBACK_CANVAS_OnClickCanvas);
      (*
      WrtLinFmt('  var %s = %s.GetNoeud(%d);', [JS_VARNAME_CURRENT_NODE , JS_GRAPHE_CLASS_VAR_NAME, 1]);
      WrtLinFmt('  var Nb = %s.FindIdxNoeudNearestToXY(%s.X, %s.Y);', [JS_GRAPHE_CLASS_VAR_NAME, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
      Wrtln(      ('  if (Nb >= 0)'));
      Wrtln(      ('  {'));
      WrtLinFmt('    var %s = %s.GetNoeud(%s);', ['QSt' , JS_GRAPHE_CLASS_VAR_NAME,'Nb']);
      WrtLinFmt('    window.alert("Station: " + %s.IDNoeud);', ['QSt']);
      Wrtln(      ('  }'));
      Wrtln(      ('  else'));
      Wrtln(      ('  {'));
      Wrtln(      ('    window.alert("Station introuvable");'));

      Wrtln(      ('  }'));
      WrtLinFmt('  var EWE = (Nb >= 0) ? "ID = " + %s.IDNoeud : "%s";', [JS_VARNAME_CURRENT_NODE, '']);
      //*)
    EndJSFunction();
  end;
  procedure QRedigeCallbackOnMouseDownCanvas();
  begin
    BeginJSFunction(JS_CALLBACK_CANVAS_OnMouseDownCanvas, 'e');
      jsSetLocalVar('QX', 'e.x');
      jsSetLocalVar('QY', 'e.y');
      jsSetLocalVar('qdx', Format('%s - %s', [JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), '');
      jsSetLocalVar('qdy', Format('%s - %s', [JS_GLOBAL_VAR_FYMaxi, JS_GLOBAL_VAR_FYMini]), '');

      jsSetLocalVar('rx', Format('qdx / %s.width' , [JS_GLOBAL_VAR_MyCanvas]), '');
      jsSetLocalVar('ry', Format('qdy / %s.height', [JS_GLOBAL_VAR_MyCanvas]), '');

      jsSetLocalVar('XM', Format(' rx * QX + %s', [JS_GLOBAL_VAR_FXMini]), '');
      jsSetLocalVar('YM', Format('-ry * QY + %s', [JS_GLOBAL_VAR_FYMaxi]), '');

      jsSetLocalVar('Nb', Format('%s.FindIdxNoeudNearestToXY(XM, YM)', [JS_GRAPHE_CLASS_VAR_NAME]), '// Positionnement du nouveau ');

      jsIF('Nb >= 0', '', 4);
         WrtLinFmt('   var %s = %s.GetNoeud(%s); // ', [JS_VARNAME_CURRENT_NODE , JS_GRAPHE_CLASS_VAR_NAME,'Nb']);
        //WrtLinFmt('    %s = %s.GetNoeud(%s); // ', [JS_GLOBAL_VAR_FGlobalNoeudCourant , JS_GRAPHE_CLASS_VAR_NAME,'Nb']);
        WrtLinFmt('    %s.innerHTML = "ID = " + %s.IDNoeud;', [JS_DOCUMENT_FORM_lbCanvasMousePos, JS_VARNAME_CURRENT_NODE]);
      jsELSE('', 4);
        WrtLinFmt('    %s.innerHTML = "%s";', [JS_DOCUMENT_FORM_lbCanvasMousePos, 'Not found']);
      jsENDIF('', 4);
    EndJSFunction();
  end;
  procedure QRedigeFunctionsPanZoomPlan();
  const
    QVarST1 = 'QNdDepart';
    QVarST2 = 'QNdArrivee';


    FMT_FACTOR_EWE = '  %s %s= Factor * EWE;';
    FMT_DELTAXY    = '  %s += Delta%s;';

  begin
    JSSeparateur('*', 100);
    WriteLine('// Zoom et pan vue');
    BeginJSFunction('PanVue', 'DeltaX, DeltaY');
      WrtLinFmt(FMT_DELTAXY, [JS_GLOBAL_VAR_FXMini, 'X']);
      WrtLinFmt(FMT_DELTAXY, [JS_GLOBAL_VAR_FXMaxi, 'X']);
      WrtLinFmt(FMT_DELTAXY, [JS_GLOBAL_VAR_FYMini, 'Y']);
      WrtLinFmt(FMT_DELTAXY, [JS_GLOBAL_VAR_FYMaxi, 'Y']);
      WrtLinFmt('  %s();', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
    EndJSFunction();
    BeginJSFunction('ZoomVue', 'Factor');
      jsSetLocalVar('EWE', Format(FMT_FLOOR_DIFF_TWO_VALUES, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), 'Fuck the Christ');
      WrtLinFmt(FMT_FACTOR_EWE, [JS_GLOBAL_VAR_FXMini, '+']);
      WrtLinFmt(FMT_FACTOR_EWE, [JS_GLOBAL_VAR_FXMaxi, '-']);
      WrtLinFmt(FMT_FACTOR_EWE, [JS_GLOBAL_VAR_FYMini, '+']);
      WrtLinFmt(FMT_FACTOR_EWE, [JS_GLOBAL_VAR_FYMaxi, '-']);
      WrtLinFmt('  %s();', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
    EndJSFunction();
    BeginJSFunction(JS_VUE_ResetVue, '');
      WrtLinFmt('  %s = %s.GetXMini();', [JS_GLOBAL_VAR_FXMini, JS_GRAPHE_CLASS_VAR_NAME]);
      WrtLinFmt('  %s = %s.GetXMaxi();', [JS_GLOBAL_VAR_FXMaxi, JS_GRAPHE_CLASS_VAR_NAME]);
      WrtLinFmt('  %s = %s.GetYMini();', [JS_GLOBAL_VAR_FYMini, JS_GRAPHE_CLASS_VAR_NAME]);
      WrtLinFmt('  %s = %s.GetYMaxi();', [JS_GLOBAL_VAR_FYMaxi, JS_GRAPHE_CLASS_VAR_NAME]);
      WrtLinFmt('  %s();', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
    EndJSFunction();
    BeginJSFunction(JS_VUE_PanVueU, '');
      jsSetLocalVar('EWE', Format(FMT_FLOOR_DIFF_TWO_VALUES, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), '');
      WriteLine('  PanVue(0, -EWE);');
    EndJSFunction();
    BeginJSFunction(JS_VUE_PanVueD, '');
      jsSetLocalVar('EWE', Format(FMT_FLOOR_DIFF_TWO_VALUES, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), '');
      WriteLine('  PanVue(0, EWE);');
    EndJSFunction();
    BeginJSFunction(JS_VUE_PanVueL, '');
      jsSetLocalVar('EWE', Format(FMT_FLOOR_DIFF_TWO_VALUES, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), '');
      WriteLine('  PanVue(EWE, 0);');
    EndJSFunction();
    BeginJSFunction(JS_VUE_PanVueR, '');
      jsSetLocalVar('EWE', Format(FMT_FLOOR_DIFF_TWO_VALUES, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), '');
      WriteLine('  PanVue(-EWE, 0);');
    EndJSFunction();
    BeginJSFunction(JS_VUE_ZoomPlus, '');
      WriteLine('  ZoomVue(1);');
     EndJSFunction();
    BeginJSFunction(JS_VUE_ZoomMoins, '');
      WriteLine('  ZoomVue(-1);');
    EndJSFunction();
    BeginJSFunction(JS_VUE_CentrerSurCourant, '');
      WrtLinFmt('  %s(%s.X, %s.Y);', [JS_FUNCTION_CentrerSurPointXY, JS_GLOBAL_VAR_FGlobalNoeudCourant, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
    EndJSFunction();
    BeginJSFunction(JS_VUE_PickStationDep);
      WrtLinFmt('    %s.value = %s.IDNoeud;', [JS_DOCUMENT_FORM_editStationDepart, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
    EndJSFunction();
    BeginJSFunction(JS_VUE_PickStationArr);
      WrtLinFmt('    %s.value = %s.IDNoeud;', [JS_DOCUMENT_FORM_editStationArrivee, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
    EndJSFunction();
    BeginJSFunction(JS_SET_TITLE_DOC, QVarST1 + ',' + QVarST2);
      WrtLinFmt('document.title = "%s entre " + %s + " et " + %s;', ['Parcours',  QVarST1, QVarST2]);
    EndJSFunction();
  end;
  procedure QRedigeFunctionCentrerSurXY();
  begin
    BeginJSFunction(JS_FUNCTION_CentrerSurPointXY, 'QX, QY');

      WrtDebugFmtLin('"Entre dans %s: CurrPt: " + %s.IDNoeud + ", QX = " + QX.toString() + " QY = " + QY.toString()', [JS_FUNCTION_CentrerSurXY, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
      jsSetLocalVar('QL', Format(FMT_FLOOR_DIFF_TWO_VALUES, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), '');
      jsSetLocalVar('QH', Format(FMT_FLOOR_DIFF_TWO_VALUES, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FYMaxi, JS_GLOBAL_VAR_FYMini]), '');
      jsSetLocalVar(JS_GLOBAL_VAR_FXMini, 'QX - QL');
      jsSetLocalVar(JS_GLOBAL_VAR_FXMaxi, 'QX + QL');
      jsSetLocalVar(JS_GLOBAL_VAR_FYMini, 'QY - QH');
      jsSetLocalVar(JS_GLOBAL_VAR_FYMaxi, 'QY + QH');
      WrtLinFmt('  %s();', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
    EndJSFunction();
  end;
  procedure QRedigeFunctionDrawGraphe();
    procedure QRedigeNestedConvFunc();
    begin
      WriteLine('  // Fonction nested de conversion');
      BeginJSFunction(JS_FUNC_CoordsToPlan, 'QX, QY');
        //WriteDebugLine('"Ceci est vérifié"');
        jsSetLocalVar('qdx', Format('%s - %s', [JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), '');
        jsSetLocalVar('qdy', Format('%s - %s', [JS_GLOBAL_VAR_FYMaxi, JS_GLOBAL_VAR_FYMini]), '');

        jsSetLocalVar('XM', Format('QX - %s', [JS_GLOBAL_VAR_FXMini]), '');
        jsSetLocalVar('YM', Format('QY - %s', [JS_GLOBAL_VAR_FYMini]), '');
        jsSetLocalVar('r' , Format('%s.width / qdx', [JS_GLOBAL_VAR_MyCanvas]), '');
        WrtLinFmt('    PX = %s.floor(r * XM);', [JS_MATH_LIBRARY]);
        WrtLinFmt('    PY = %s.height - %s.floor(r * YM);', [JS_GLOBAL_VAR_MyCanvas, JS_MATH_LIBRARY]);
      EndJSFunction();
      WriteLine('  // Fin fonctions nested');
    end;
  begin
    BeginJSFunction(JS_FUNCTION_DRAW_GRAPHE, 'DoDrawLabelStations');
      jsSetLocalVar('Nb', Format('%s.GetNbNoeuds()', [JS_GRAPHE_CLASS_VAR_NAME]), 'JS_GRAPHE_CLASS_VAR_NAME');
      WrtDebugFmtLin('"Entre dans %s: " + %s.toString()', [JS_FUNCTION_DRAW_GRAPHE, 'Nb']);
      WriteLine('  if (Nb == 0) return;');
      WrtLinFmt('  %s.width = %s.clientWidth;', [JS_GLOBAL_VAR_MyCanvas, JS_GLOBAL_VAR_MyContainer]);
      WrtLinFmt('  %s.height = %s.clientHeight;', [JS_GLOBAL_VAR_MyCanvas, JS_GLOBAL_VAR_MyContainer]);
      WrtLinFmt('  var %s = %s.getContext("%s");', [NAMEREF_CANVAS_DC, JS_GLOBAL_VAR_MyCanvas, '2d']);

      WrtLinFmt('  %s.strokeStyle = "rgba(%d, %d, %d, %.0f)";', [NAMEREF_CANVAS_DC, Red(CouleurCenterlines), Green(CouleurCenterlines), Blue(CouleurCenterlines), 1.0]);
      WrtLinFmt('  %s.lineWidth   = "%dpx";', [NAMEREF_CANVAS_DC, QLineWidth]);
      WrtLinFmt('  %s.fillStyle   = "rgba(200, 0, 0, 1)";', [NAMEREF_CANVAS_DC]);
      WrtLinFmt('  %s.font        = "%dpx %s";', [NAMEREF_CANVAS_DC, 12, 'sans-serif']);
      WriteLine('  // variables semi-globales');
      jsSetLocalVar('PX'  , 0);
      jsSetLocalVar('PY'  , 0);
      jsSetLocalVar('PS1X', 0);
      jsSetLocalVar('PS1Y', 0);
      jsSetLocalVar('PS2X', 0);
      jsSetLocalVar('PS2Y', 0);

      QRedigeNestedConvFunc();
      WriteLine('  // Traçage du graphe');
      jsSetLocalVar('QEtendueX', Format('%s - %s', [JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), 'Etendue du réseau en X');
      jsFOR('i', 'Nb', 0, '', 4);
        WrtLinFmt('    %s = %s.GetNoeud(i);', [JS_VARNAME_CURRENT_NODE, JS_GRAPHE_CLASS_VAR_NAME]);
        WrtLinFmt('    %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
        WriteLine('    PS1X = PX;');
        WriteLine('    PS1Y = PY;');
        WrtLinFmt('    %s.fillRect(PX - %d, PY - %d, %d, %d);', [NAMEREF_CANVAS_DC, SIZE_STATION_MARKER, SIZE_STATION_MARKER, 2 * SIZE_STATION_MARKER, 2 * SIZE_STATION_MARKER]);
        WrtLinFmt('    if (QEtendueX < %.0f) %s.fillText(%s.IDNoeud, PX + %d, PY + %d);', [10 * ETENDUE_X_MAX_FOR_DISP_LBL_STATIONS, NAMEREF_CANVAS_DC, JS_VARNAME_CURRENT_NODE, SIZE_STATION_MARKER + 1, SIZE_STATION_MARKER + 1]);
        jsSetLocalVar('NbVoisins', Format('%s.ListeVoisins.length', [JS_VARNAME_CURRENT_NODE]), '');
        jsIF('NbVoisins > 0', '', 4);
          jsFOR('k', 'NbVoisins', 0, '', 6);                 //WriteLine('      for (var k = 0; k < NbVoisins; k++)');
            WrtLinFmt('        var QIdxVoisin   = %s.ListeVoisins[k];', [JS_VARNAME_CURRENT_NODE]);
            WrtLinFmt('        var MyNodeVoisin = %s.GetNoeud(QIdxVoisin);', [JS_GRAPHE_CLASS_VAR_NAME]);
            WrtLinFmt('        %s(MyNodeVoisin.X, MyNodeVoisin.Y);', [JS_FUNC_CoordsToPlan]);
            WrtLinFmt('        %s.beginPath();'         , [NAMEREF_CANVAS_DC]);
            WrtLinFmt('        %s.moveTo(PS1X, PS1Y);'  , [NAMEREF_CANVAS_DC]);
            WrtLinFmt('        %s.lineTo(PX , PY);'     , [NAMEREF_CANVAS_DC]);
            WrtLinFmt('        %s.stroke();'            , [NAMEREF_CANVAS_DC]);
          jsNEXT('k', '', 6);
        jsENDIF('', 4);
      jsNEXT('i');
      WriteLine('  // On trace le réticule'); //
      WrtLinFmt('  %s.strokeStyle = "rgba(%d, %d, %d, %.0f)";', [NAMEREF_CANVAS_DC, Red(clGray), Green(clGray), Blue(clGray), 192/256.0]);
      WrtLinFmt('  %s.lineWidth   = "%dpx";', [NAMEREF_CANVAS_DC, QLineWidth]);

      JSTraceLigne(JS_GLOBAL_VAR_FXMini, JS_GLOBAL_VAR_FYCentreMap, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FYCentreMap);
      JSTraceLigne(JS_GLOBAL_VAR_FXCentreMap, JS_GLOBAL_VAR_FYMini, JS_GLOBAL_VAR_FXCentreMap, JS_GLOBAL_VAR_FYMaxi);
      WriteLine('  // Tracé du plus court chemin: ');

      WrtLinFmt('  Nb = %s.%s();', [JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetNbStationsOfPathFound]);
      jsIF('Nb > 2', '', 4); //WriteLine('  if ()');
        QDrawShortestPath(false, 7, clRed      , 128);
        QDrawShortestPath(false, 4, clYellow   , 192);
        QDrawShortestPath(true , 1, clMaroon   , 255);
      jsENDIF('', 4);
      WriteLine('  // et on met en évidence le dernier point trouvé');
      jsIF(Format('%s >= 0', [JS_GLOBAL_VAR_FLastIdxFound]), '', 4);
        WrtLinFmt('    %s.font        = "%dpx %s";', [NAMEREF_CANVAS_DC, 16, 'sans-serif']);
        WrtLinFmt('    %s = %s.GetNoeud(%s);', [JS_VARNAME_CURRENT_NODE, JS_GRAPHE_CLASS_VAR_NAME, JS_GLOBAL_VAR_FLastIdxFound]);
        WrtLinFmt('    %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
        WrtLinFmt('    %s.fillText(%s.IDNoeud, PX + %d, PY + %d);', [NAMEREF_CANVAS_DC, JS_VARNAME_CURRENT_NODE, SIZE_STATION_MARKER + 1, SIZE_STATION_MARKER + 1]);
     jsENDIF('', 4);
    EndJSFunction();
  end;
  // interaction avec la page
  procedure QRedigeFunctionProcOnSubmit();
  begin
    BeginJSFunction(JS_DOCUMENT_FORM_btnSearch_ProcOnSubmit);
      jsSetLocalVar('EWE', Format('%s.value', [JS_DOCUMENT_FORM_editFindWhat]), '');
      jsSetLocalVar('QIdx', Format('%s.FindIdxNoeudByCle(EWE)', [JS_GRAPHE_CLASS_VAR_NAME]), '');
      WrtDebugFmtLin('"Dans %s: QIdx = " + QIdx.toString()', [JS_DOCUMENT_FORM_btnSearch_ProcOnSubmit]);
      jsIF('QIdx >= 0', '', 4);
        WrtLinFmt('    %s = QIdx;', [JS_GLOBAL_VAR_FLastIdxFound]);
        WrtLinFmt('    %s = %s.GetNoeud(%s);', [JS_GLOBAL_VAR_FGlobalNoeudCourant, JS_GRAPHE_CLASS_VAR_NAME, JS_GLOBAL_VAR_FLastIdxFound]);
        WriteLine(Format('window.alert("Noeud trouvé: " + %s.IDNoeud);', [JS_GLOBAL_VAR_FGlobalNoeudCourant]));
        WrtLinFmt('    %s(%s.X, %s.Y);', [JS_FUNCTION_CentrerSurXY, JS_GLOBAL_VAR_FGlobalNoeudCourant, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
      jsELSE('', 4);
        WriteLine('    window.alert("Station " + EWE + " introuvable");');
      jsENDIF('', 4);
    EndJSFunction();
  end;
  procedure QRedigeFunctionBtnCalcShortestPathClick();
  begin
    BeginJSFunction(JS_DOCUMENT_FORM_btnCalcShortestPath_ProcOnSubmit);
      JSToto('départ'  , JS_DOCUMENT_FORM_editStationDepart , 'ST1', 'IdxSt1');
      JSToto('arrivée' , JS_DOCUMENT_FORM_editStationArrivee, 'ST2', 'IdxSt2');
      WrtLinFmt('  %s(%s, %s);', [JS_FUNCTION_CalcCheminMinimalBetweenTwoNodes, 'ST1', 'ST2']);
    EndJSFunction();
  end;
  procedure QRedigeFunctionLsbRoadmapOnSelect();
  const
    QVAR_QIdxPathPtCurr = 'QIdxPathPtCurr';
    QVAR_QIdxPathPtNext = 'QIdxPathPtNext';
  begin
    WriteLine('// Interaction avec la feuille de route');
    BeginJSFunction(JS_DOCUMENT_FORM_lsbRoadmap_ProcOnSelect, '');
      WriteDebugLine('"Entre dans: ' + JS_DOCUMENT_FORM_lsbRoadmap_ProcOnSelect + '"');
      jsSetLocalVar('QIdx', Format('%s.selectedIndex', [JS_DOCUMENT_FORM_lsbRoadMap]), 'Index de l''élément sélectionné');
      WriteDebugLine('"--> " + QIdx.toString()');
      jsSetLocalVar(QVAR_QIdxPathPtCurr, Format('%s.%s(QIdx)', [JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound]), 'Recherche du point courant');
      WrtLinFmt('   if (%s === -1) return;', [QVAR_QIdxPathPtCurr]);
      jsSetLocalVar(JS_VARNAME_CURRENT_NODE, Format('%s.GetNoeud(%s)', [JS_GRAPHE_CLASS_VAR_NAME, QVAR_QIdxPathPtCurr]), '');
      WriteLine('   // et centrage sur le point courant');
      jsSetLocalVar(JS_GLOBAL_VAR_FGlobalNoeudCourant, JS_VARNAME_CURRENT_NODE, 'Kill the Pope');

      WrtLinFmt('   %s(%s.X, %s.Y);', [JS_FUNCTION_CentrerSurXY, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);

      jsSetLocalVar(QVAR_QIdxPathPtNext, Format('%s.%s(QIdx+1)', [JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound]), 'Recherche du point suivant');
      WrtLinFmt('   if (%s === -1) return;', [QVAR_QIdxPathPtNext]);
      jsSetLocalVar(JS_VARNAME_NEXT_NODE, Format('%s.GetNoeud(%s)', [JS_GRAPHE_CLASS_VAR_NAME, QVAR_QIdxPathPtNext]), '');

      jsSetLocalVar(JS_VARNAME_CapToNext, Format('%s(%s.X - %s.X, %s.Y - %s.Y, %s.Z - %s.Z, %.2f, %.2f)',
                   [JS_UTILITY_FUNC_GetBearingInc,
                    JS_VARNAME_NEXT_NODE, JS_VARNAME_CURRENT_NODE,
                    JS_VARNAME_NEXT_NODE, JS_VARNAME_CURRENT_NODE,
                    JS_VARNAME_NEXT_NODE, JS_VARNAME_CURRENT_NODE,
                    360.00, 360.00]));
      WrtLinFmt('   %s.innerHTML    = "Next: " + %s.IDNoeud;', [JS_DOCUMENT_FORM_lbCanvasMousePos , JS_VARNAME_NEXT_NODE]);
      WrtLinFmt('   %s.innerHTML    = "Dist: " + %s.%s.toFixed(2);', [JS_DOCUMENT_FORM_lbDistNextPoint  , JS_VARNAME_CapToNext, JS_VARNAME_OUT_Dist]);
      WrtLinFmt('   %s.innerHTML    = "Cap : " + %s.%s.toFixed(2);', [JS_DOCUMENT_FORM_lbAzimutNextPoint, JS_VARNAME_CapToNext, JS_VARNAME_OUT_Az]);
      WrtLinFmt('   %s.innerHTML    = "Incl: " + %s.%s.toFixed(2);', [JS_DOCUMENT_FORM_lbPenteNextPoint , JS_VARNAME_CapToNext, JS_VARNAME_OUT_Inc]);
    EndJSFunction();
  end;
  // fonctions utilitaires
  procedure QBeginRedigerFonctionsUtilitaires();
  begin
    JSSeparateur();
    WrtLinFmt('// Les %s', [JS_SECTION_FUNCTIONS_UTILITAIRES]);
  end;
  procedure QEndRedigerFonctionsUtilitaires();
  begin
    JSSeparateur();
    WrtLinFmt('// Fin section %s', [JS_SECTION_FUNCTIONS_UTILITAIRES]);
    WriteLine('');
  end;

  procedure QRedigeFunctionGetAzimut();
  const
    VARNAME_TWO_PI = 'TWO_PI';
    VARNAME_a      = 'a';
  begin
    BeginJSFunction(JS_UTILITY_FUNC_GetAzimut, 'dx, dy, Unite');
      WrtLinFmt('  const %s = 2 * %s.PI;', [VARNAME_TWO_PI, JS_MATH_LIBRARY]);
      WrtLinFmt('  var %s = %s.atan2(dy, dx + 1e-12);', [VARNAME_a, JS_MATH_LIBRARY]);
      WrtLinFmt('  if (%s < 0) %s = %s + %s;', [VARNAME_a, VARNAME_a, VARNAME_a, VARNAME_TWO_PI]);
      WrtLinFmt('  %s = 0.50 * %s.PI - %s;', [VARNAME_a, JS_MATH_LIBRARY, VARNAME_a]);      // a := 0.50 * PI - a;
      WrtLinFmt('  if (%s < 0) %s = %s + %s;', [VARNAME_a, VARNAME_a, VARNAME_a, VARNAME_TWO_PI]);
      WrtLinFmt('  return %s * 0.50 * Unite / %s.PI;', [VARNAME_a, JS_MATH_LIBRARY]);
    EndJSFunction();
  end;
  procedure QRedigeFunctionGetBearingInc();
  const
    VARNAME_fUB  = 'fUB';
    VARNAME_fUC  = 'fUC';
    VARNAME_qdist = 'qdist';
    VARNAME_qdp   = 'qdp';
    VARNAME_qAz   = 'qaz';
    VARNAME_qInc  = 'qincl';
  var
    WU: String;
  begin
    WU := format('%s, %s, %s, %s, %s', [JS_VARNAME_dx  , JS_VARNAME_dy, JS_VARNAME_dz, VARNAME_fUB , VARNAME_fUC]);
    BeginJSFunction(JS_UTILITY_FUNC_GetBearingInc, WU);
      WrtLinFmt('  var %s = %s.hypot(%s, %s);', [VARNAME_qdp  , JS_MATH_LIBRARY, JS_VARNAME_dx , JS_VARNAME_dy]);
      WrtLinFmt('  var %s = %s.hypot(%s, %s);', [VARNAME_qdist, JS_MATH_LIBRARY, VARNAME_qdp, JS_VARNAME_dz]);
      WrtLinFmt('  var %s = %s.atan2(%s, %s) * 0.5 * %s / %s.PI;', [VARNAME_qInc , JS_MATH_LIBRARY, JS_VARNAME_dz, VARNAME_qdp, VARNAME_fUC, JS_MATH_LIBRARY]);    // Inc  := ArcTan2(dz, dp) * 0.5 * fUC / pi;
      WrtLinFmt('  var %s = %s(%s, %s, %s);', [VARNAME_qAz, JS_UTILITY_FUNC_GetAzimut, JS_VARNAME_dx, JS_VARNAME_dy, VARNAME_fUB]);
      WriteLine('  return {');
      WrtLinFmt('    %s: %s / 10.0,', [JS_VARNAME_OUT_Dist, VARNAME_qdist]);
      WrtLinFmt('    %s: %s,', [JS_VARNAME_OUT_Az  , VARNAME_qAz]);
      WrtLinFmt('    %s: %s', [JS_VARNAME_OUT_Inc , VARNAME_qInc]);
      WriteLine('  }; // return tuple');
    EndJSFunction();
  end;
  procedure QInitialiserEventsOfTactilePointer();
  begin
    WriteLine('  // Gestion des mouvements de doigts sur la tablette (gestures)');
    WriteLine('  // A implanter dans Initialiser() après assignation des contrôles via GetElementByID()');
    WriteLine('  // https://developer.mozilla.org/fr/docs/Web/API/Pointer_events/gestes_pincer_zoom');

    WrtLinFmt('  %s.onpointerdown   = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerdown_handler]);
    WrtLinFmt('  %s.onpointerup     = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerup_handler]);
    WrtLinFmt('  %s.onpointercancel = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerup_handler]);
    WrtLinFmt('  %s.onpointermove   = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointermove_handler]);
    WrtLinFmt('  %s.onpointerout    = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerup_handler]);
    WrtLinFmt('  %s.onpointerleave  = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerup_handler]);
    WriteLine('  // Fin Gestures');
  end;
  procedure QRedigerTactileHandlers();
  const
    ARG_EVENT   = 'event';
    COUNTER_VAR = 'i';
    VAR_CURRDIFF = 'currDiff';
  var
    WU: String;
  begin
    WriteLine('// Callbacks des handlers de tactile');
    BeginJSFunction(JS_TACTILE_PROC_remove_event, ARG_EVENT);
      // Supprime l'événement du cache
      WU := Format('%s.length', [ARG_EVENT]);
      jsFOR(COUNTER_VAR, WU, 0, '', 4);
        jsIF(Format('%s[%s].pointerId === %s.pointerId', [JS_GLOBAL_VAR_EvtCache, COUNTER_VAR, ARG_EVENT]), '', 6);
          WrtLinFmt('       %s.splice(%s, %d);', [JS_GLOBAL_VAR_EvtCache, COUNTER_VAR, 1]);
          jsBREAK(8);
        jsENDIF('', 6);
      jsNEXT(COUNTER_VAR, '', 4);
    EndJSFunction();
    // L'événement pointerdown signale le début d'une interraction de toucher.
    // OnMouseDown
    BeginJSFunction(JS_TACTILE_EVENT_pointerdown_handler, ARG_EVENT);
      // L'événement est mis en cache pour prendre en charge les gestes à 2 doigts
      WrtLinFmt('  %s.push(%s);', [JS_GLOBAL_VAR_EvtCache, ARG_EVENT]);
     // WrtLinFmt('  console.log("%s", %s);', [JS_TACTILE_EVENT_pointerdown_handler, ARG_EVENT]);
    EndJSFunction();
    // OnMouseMove
    BeginJSFunction(JS_TACTILE_EVENT_pointermove_handler, ARG_EVENT);
      //WrtLinFmt('  console.log("%s", %s);', [JS_TACTILE_EVENT_pointermove_handler, ARG_EVENT]);
      // Trouve le pointeur en cours dans le cache et le met à jour avec cet événement
       WU := Format('%s.length', [ARG_EVENT]);
       jsFOR(COUNTER_VAR, WU, 0, '', 4);
         jsIF(Format('%s.pointerId === %s[%s].pointerId', [ARG_EVENT,  JS_GLOBAL_VAR_EvtCache, COUNTER_VAR]), '', 6);       // if (ev.pointerId == evCache[i].pointerId) {
           WrtLinFmt('     %s[%s] = %s;', [JS_GLOBAL_VAR_EvtCache, COUNTER_VAR, ARG_EVENT]);                                  //    evCache[i] = ev;
           jsBREAK(8);
         jsENDIF('', 4);
       jsNEXT(COUNTER_VAR, '', 4);
       // Si deux pointeurs sont utilisés, vérifie le geste de pincement
       jsIF(Format('%s.length === %d', [ARG_EVENT, 2]), '', 4); //  if (evCache.length == 2) {
         WrtLinFmt('      var %s = %s.abs(%s[%d].clientX - %s[%d].clientX);', [VAR_CURRDIFF, JS_MATH_LIBRARY, JS_GLOBAL_VAR_EvtCache, 0, JS_GLOBAL_VAR_EvtCache, 1]);
         jsIF(Format('%s > %d', [JS_GLOBAL_VAR_PrevDiff, 0]), '', 6);
           jsIF(Format('%s > %s', [VAR_CURRDIFF, JS_GLOBAL_VAR_PrevDiff, 0]), 'La distance entre les deux pointeurs a augmenté', 8);
             //WrtLinFmt('  console.log("%s");', ['Zoom Plus']);
           jsENDIF('', 8);
           jsIF(Format('%s < %s', [VAR_CURRDIFF, JS_GLOBAL_VAR_PrevDiff, 0]), 'La distance entre les deux pointeurs a diminué', 8);
             //WrtLinFmt('  console.log("%s");', ['Zoom Moins']);
           jsENDIF('', 8);
         jsENDIF('', 6);
       jsENDIF('', 4);
       WrtLinFmt('%s = %s; // %s', [JS_GLOBAL_VAR_PrevDiff, VAR_CURRDIFF, 'Met en cache la distance pour les événements suivants']);
    EndJSFunction();
    // OnMouseUp
    BeginJSFunction(JS_TACTILE_EVENT_pointerup_handler, ARG_EVENT);
      //WrtLinFmt('  console.log("%s", %s);', [JS_TACTILE_EVENT_pointerup_handler, ARG_EVENT]);
      // Retire ce pointeur du cache et rétablit l'arrière-plan et
      WrtLinFmt('  %s(%s);', [JS_TACTILE_PROC_remove_event, ARG_EVENT]);
      // traitements
      //ev.target.style.border = "1px solid black";

      // Si le nombre de pointeurs restant est inférieur à deux, remet à zéro la différence
      WrtLinFmt('  if (%s.length < %d) %s = -1;', [JS_GLOBAL_VAR_EvtCache, 2, JS_GLOBAL_VAR_PrevDiff]);
    EndJSFunction();
  end;
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
    WriteLine('<!DOCTYPE html>');
    WrtLinFmt('<!-- Generated %s by %s version %s -->', [DateTimePascalToDateTimeSQL(Now), ApplicationName, GetGHTopoVersion()]);
    WriteLine('<HTML>');
    WriteLine('<HEAD>');
    WrtLinFmt('  <TITLE>%s</TITLE>', [DocTitle]);
    WrtLinFmt('  <META charset="%s" />', ['utf-8']);
    //WriteLine(Format('  <META name="%s" content="width=%s, initial-scale=%.1f">', ['viewport', 'device-width', 1.0]);
    // styles
    WriteLine('<STYLE>');
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
    WriteLine('</STYLE>');

    WrtLinFmt('  <script type="%s">', [MIME_TEXT_JAVASCRIPT]);
    WrtLinFmt('// %s generated by GHTopo at %s', [FileNameJS, DatePascalToDateSQL(Now())]);
    WriteLine('    // L''auteur de GHTopo est ultra-christianophobe');
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
    BeginJSEventListener('beforeunload', 'Anti-reload (indispensable pour tablettes)');
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
          EWE += Format('%d%s', [MyNoeud.LinkNode[j].TargetNodeIndex,
                                 IIF(j < (NbVoisins - 1), ',', '')]);
        end;
      end;
      WrtLinFmt('  %s.AddNoeud("%s", %d, %d, %d, [%s]);', [JS_GRAPHE_CLASS_VAR_NAME,
                                                            FormatterTIDStation(MyNoeud.IDStation),
                                                            ToJSCoord(MyNoeud.X),
                                                            ToJSCoord(MyNoeud.Y),
                                                            ToJSCoord(MyNoeud.Z),
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



    WriteLine('  </SCRIPT>');
    WriteLine('</HEAD>');
    WriteLine('<!-- Mise en page du document -->');
    WrtLinFmt('<BODY onLoad="%s(''%s'', ''%s'')">', [JS_FUNCTION_INITIALISER, QIDStationDepart, QIDStationArrivee]);
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
              JSMakeButtonInTable('btn666'        , '...'        , JS_VUE_PickStationDep);
          EndRow();
          BeginRow();
            WriteLine('<TD>');
              JSMakeLabelledEdit('lbStationArrivee', 'Arrivée' , JS_DOCUMENT_FORM_editStationArrivee , QIDStationArrivee);
            WriteLine('</TD>');
            JSMakeButtonInTable('btn777'        , '...'        , JS_VUE_PickStationArr);
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
        WrtLinFmt('    <LABEL id="%s" value="">%s</LABEL><BR>'    , [JS_DOCUMENT_FORM_lbRoadmapNbPoints, 'lbRoadmapNbPoints']);
        WrtLinFmt('    <select id="%s" name="%s" size="%d" style="width:%d%%" onChange="%s()"> </select><BR>', [JS_DOCUMENT_FORM_lsbRoadMap , JS_DOCUMENT_FORM_lsbRoadMap, 10, 100, JS_DOCUMENT_FORM_lsbRoadmap_ProcOnSelect]);
        WrtLinFmt('    <LABEL id="%s" value="">%s</LABEL><BR>'    , [JS_DOCUMENT_FORM_lbCanvasMousePos  , '--']);
        WriteLine('    <HR>');
        WrtLinFmt('    <LABEL id="%s" value="">%s</LABEL><BR>'    , [JS_DOCUMENT_FORM_lbAzimutNextPoint , '--']);
        WriteLine('    <BR>');
        WrtLinFmt('    <LABEL id="%s" value="">%s</LABEL><BR>'    , [JS_DOCUMENT_FORM_lbDistNextPoint   , '--']);
        WrtLinFmt('    <LABEL id="%s" value="">%s</LABEL><BR>'    , [JS_DOCUMENT_FORM_lbPenteNextPoint  , '--']);
      EndForm();
    EndDiv();
    WriteLine('  <noscript>' + 'JavaScript is required' + '</noscript>' );
    WriteLine('</BODY>');
    WriteLine('</HTML>');
  finally
    Closefile(fp);
  end;
end;



end.

