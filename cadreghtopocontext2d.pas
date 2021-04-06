unit CadreGHTopoContext2D;
// Cadre de dessin 2D pour la nouvelle version des données compîlées
// 05/11/2013: Modes de représentation OK (secteurs, réseaux, expés)
// 28/11/2013: Gestion des dégradés dans les onglets
// 17/01/2014: Nouveau dialogue Distance
// 23/01/2014: Calcul altitude d'un point de surface si maillage MNT OK
// 14/04/2014: Implémentation du support des cotes "spéléo" (-268, -140)
// 04/08/2014: Nouvelle procédure de dessin des entrées
// 20/11/2014: Support des largeurs de trait pour les polygonales (lié aux visées en antenne)
// 20/11/2014: Les visées en antenne deviennent une catégorie d'éléments affichés
// 13/05/2015: Possibilité optionnelle de TBGRABitmap
// 28/05/2015: Surbrillance de la série courante
// 19/06/2015: Noms des entrées sont affichés
// 22/02/2015: Recherche de stations OK. Si trouvée, un marqueur est affiché
// 06/05/2016: La méthode de zoom et pan est unifiée et alignée sur celle de GHCaveDraw (AutoCAD-like)
// 14/06/2016: Ajout du mode de visu 'Type de visées'
// 07/04/2017: Suppression du cadre 'Filtres', trop complexe
// 21/04/2017: Optimisations de l'affichage par tests IsPointInRect() et IsSegmentInRect()
// 12/11/2017: La barre de statut/infos du viewer passe en haut. Motif: Le bas de la fenêtre est souvent masqué
// 20/09/2018: Fix de l'affichage des cotes: les visées radiantes étaient également cotées
// 13/06/2019: Point de contrôle temporel (contrôle de version)
// 12/12/2019: Une seule variable pour la station de référence: FCurrentStation


{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  math, Types,
  ToporobotClasses2012,
  UnitEntitesExtended,
  unitCroquisTerrain,
  unitobjetserie,
  UnitClasseMaillage,
  UnitGraphes1,
  FastGEO,
  //types,
  dateutils,
  Graphics,
  Clipbrd
  , BGRABitmap
  , BGRABitmapTypes
  , BGRAGradients
  , UnitTGHTopoDrawDrawingContext,
  CallDialogsStdVersion,
  unitUtilsComposants, BZGraphesTypes, BZGraphesClasses,
  Classes, SysUtils, FileUtil, curredit, Forms, Dialogs, Controls,
  ExtCtrls, StdCtrls, ActnList, Menus, Buttons, LCLType, Spin;


// Si utilisation de GHTopo simplifé
{$IFDEF GHTOPO_SIMPLIFIE}
CONST FCurrIdxOnglet = 0;
{$ENDIF GHTOPO_SIMPLIFIE}

type

{ TGHTopoContext2DA }

  TGHTopoContext2DA = class(TFrame)
    acIsovaleurSurZStation: TAction;
    acMetaFiltreCode: TAction;
    acMetaFiltreExpe: TAction;
    acMetaFiltreReseau: TAction;
    acMetaFiltreSerie: TAction;
    acMetaFiltreTopoAnnee: TAction;
    acMetaFiltreTopoDuJour: TAction;
    acMetaFiltreVueCourante: TAction;
    acMetaFiltreZone: TAction;
    acMetaFiltreSecteur: TAction;
    acAddSerieHere: TAction;
    acSetCurrentBaseStation: TAction;
    acPanVue: TAction;
    acDistanceP2P: TAction;
    acMetaFiltreEntree: TAction;
    acDisplayVoisinageCurrStation: TAction;
    acCreerEntreeFromThisPoint: TAction;
    acAddserieEntreCesPoints: TAction;
    acLocaliserUneStation: TAction;
    acSaveCroquis: TAction;
    acOpenCroquis: TAction;
    acGetStationCoords: TAction;
    acMetaFiltreNamespace: TAction;
    acHighlightThisSerie: TAction;
    acHighLightThisCode: TAction;
    acHighLightThisExpe: TAction;
    acHighLightThisReseau: TAction;
    acHighLightThisNamespace: TAction;
    acHighLightThisEntrance: TAction;
    acContinueHere: TAction;

    acRemoveLastVisee: TAction;
    acDisplayMiniformVisee: TAction;
    acBtnPanNW: TAction;
    acBtnPanSW: TAction;
    acBtnPanSE: TAction;
    acBtnPanNE: TAction;
    acBtnPanEE: TAction;
    acBtnPanNN: TAction;
    acBtnPanSS: TAction;
    acBtnPanWW: TAction;
    acEditThisSerie: TAction;
    acDispPopUp: TAction;
    acDeleteThisPOI: TAction;
    acAddPOIHere: TAction;
    acZoomPlus: TAction;
    acZoomMoins: TAction;
    acTransmitBasePointForHighlighting: TAction;
    acZoomFenetre: TAction;
    acZoomTout: TAction;
    ActListCdrVue2D: TActionList;
    btnApplyMetaFiltre: TButton;
    btnHelpMetaFiltre: TButton;
    Button1: TButton;
    Button2: TButton;
    btnMoreInfos: TButton;
    btnCopyCoords: TButton;
    btnListeProfils: TButton;
    btnSerieCouranteDistoX: TButton;
    btnQSAddVisee: TButton;
    btnUnloadMNT: TButton;
    Button4: TButton;
    btnQSCloseMiniFormVisee: TButton;
    chkDrawMaillage: TCheckBox;
    btnLineContourColor: TColorButton;
    chkMetaFiltreActif: TCheckBox;
    editQSRight: TCurrencyEdit;
    editQSLongueur: TCurrencyEdit;
    editQSAzimut: TCurrencyEdit;
    editQSLeft: TCurrencyEdit;
    editQSPente: TCurrencyEdit;
    editQSCommentaire: TEdit;
    editIsoValeur: TCurrencyEdit;
    editMetaFiltre: TEdit;
    editQSUp: TCurrencyEdit;
    editQSDown: TCurrencyEdit;
    imgLstCtxtV2D: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbFiltres: TLabel;
    lbColorReseau: TStaticText;
    lbCouleurVisee: TStaticText;
    lbColorSecteur: TStaticText;
    lbColorSeance: TStaticText;
    lbCoordonnees: TStaticText;
    lbGCSMouse: TStaticText;
    lbInfos: TStaticText;
    lbMesures: TStaticText;
    lbAdditionalInfos: TStaticText;
    lbNomReseau: TStaticText;
    lbNomSecteur: TStaticText;
    lbModeTravail: TStaticText;
    lbNumSerie: TStaticText;
    lbInfosExpe: TStaticText;
    lbNumStation: TStaticText;
    lbIDTerrainStation: TStaticText;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    mnuHighLight: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuMetaFiltreStation: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlToast: TPanel;
    pnlQuickSaisieVisee: TPanel;
    pnlFiltres: TPanel;
    pnlFullInfos: TPanel;
    pnlMaillage: TPanel;
    PopUpCdrVue2D: TPopupMenu;
    lbCode: TStaticText;
    lbSerie: TStaticText;
    lbEntreeRattachement: TStaticText;
    lbMiscMaillage: TStaticText;
    lbLongViseesVisibles: TStaticText;
    lbTitreQuickSaisieVisee: TStaticText;
    editLineContourOpacity: TSpinEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Vue: TPaintBox;

    procedure acAddAnnotationHereExecute(Sender: TObject);
    procedure acAddPOIHereExecute(Sender: TObject);
    procedure acAddserieEntreCesPointsExecute(Sender: TObject);
    procedure acAddSerieHereExecute(Sender: TObject);
    procedure acAddViseeRadianteHereExecute(Sender: TObject);
    procedure acBtnPanEEExecute(Sender: TObject);
    procedure acBtnPanNNExecute(Sender: TObject);
    procedure acBtnPanSSExecute(Sender: TObject);
    procedure acBtnPanWWExecute(Sender: TObject);
    procedure acContinueHereExecute(Sender: TObject);
    procedure acCreerEntreeFromThisPointExecute(Sender: TObject);
    procedure acDeleteThisPOIExecute(Sender: TObject);
    procedure acDisplayMiniformViseeExecute(Sender: TObject);
    procedure acDisplayVoisinageCurrStationExecute(Sender: TObject);
    procedure acDispPopUpExecute(Sender: TObject);
    procedure acDistanceP2PExecute(Sender: TObject);
    procedure acGetStationCoordsExecute(Sender: TObject);
    procedure acHighLightThisCodeExecute(Sender: TObject);
    procedure acHighLightThisExpeExecute(Sender: TObject);
    procedure acHighlightThisSerieExecute(Sender: TObject);
    procedure acIsovaleurSurZStationExecute(Sender: TObject);
    procedure acLocaliserUneStationExecute(Sender: TObject);
    procedure acMetaFiltreCodeExecute(Sender: TObject);
    procedure acMetaFiltreEntreeExecute(Sender: TObject);
    procedure acMetaFiltreExpeExecute(Sender: TObject);
    procedure acMetaFiltreNamespaceExecute(Sender: TObject);
    procedure acMetaFiltreReseauExecute(Sender: TObject);
    procedure acMetaFiltreSecteurExecute(Sender: TObject);
    procedure acMetaFiltreSerieExecute(Sender: TObject);
    procedure acMetaFiltreTopoAnneeExecute(Sender: TObject);
    procedure acMetaFiltreTopoDuJourExecute(Sender: TObject);
    procedure acMetaFiltreVueCouranteExecute(Sender: TObject);
    procedure acMetaFiltreZoneExecute(Sender: TObject);
    procedure acOpenCroquisExecute(Sender: TObject);
    procedure acPanVueExecute(Sender: TObject);
    procedure acRemoveLastViseeExecute(Sender: TObject);
    procedure acSaveCroquisExecute(Sender: TObject);
    procedure acSetCurrentBaseStationExecute(Sender: TObject);
    procedure acSetCurrentSerieExecute(Sender: TObject);
    procedure acSupprimerSerieExecute(Sender: TObject);
    procedure acEditThisSerieExecute(Sender: TObject);
    procedure acTransmitBasePointForHighlightingExecute(Sender: TObject);
    procedure acZoomFenetreExecute(Sender: TObject);
    procedure acZoomMoinsExecute(Sender: TObject);
    procedure acZoomPlusExecute(Sender: TObject);
    procedure acZoomToutExecute(Sender: TObject);
    procedure btnApplyMetaFiltreClick(Sender: TObject);
    procedure btnCopyCoordsClick(Sender: TObject);
    procedure btnDefinirCommeActiveClick(Sender: TObject);
    procedure btnHelpMetaFiltreClick(Sender: TObject);
    procedure btnListeProfilsClick(Sender: TObject);
    procedure btnMoreInfosClick(Sender: TObject);
    procedure btnSerieCouranteDistoXClick(Sender: TObject);
    procedure btnUnloadMNTClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnQSAddViseeClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnQSCloseMiniFormViseeClick(Sender: TObject);
    procedure chkDrawMaillageChange(Sender: TObject);
    procedure chkMetaFiltreActifChange(Sender: TObject);
    procedure editMetaFiltreKeyPress(Sender: TObject; var Key: char);
    procedure lbInfosClick(Sender: TObject);
    procedure lbTitreQuickSaisieViseeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbTitreQuickSaisieViseeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure lbTitreQuickSaisieViseeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PopUpCdrVue2DPopup(Sender: TObject);

    procedure SetStationInfo(const S: TBaseStation);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton13MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton17Click(Sender: TObject);
    procedure VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure VuePaint(Sender: TObject);
  strict private
    // pour le dessin des étiquettes au survol du plan
    // très bonne rapidité avec la Coume
    // très bonne rapidité avec Langoiran et Citon
    (*
    FEtiquetteOnSurvolPlan_First: boolean;
    FEtiquetteOnSurvolPlan_Tampon: TImage;   // et non TBitmap
    FEtiquetteOnSurvolPlan_ox: integer;
    FEtiquetteOnSurvolPlan_oy: integer;
    FEtiquetteOnSurvolPlan_ow: integer;
    FEtiquetteOnSurvolPlan_oh: integer;
    FEtiquetteOnSurvolPlan_X1: integer;
    FEtiquetteOnSurvolPlan_Y1: integer;
    //*)
    // Overlay (à la manière des sites web merdiques)
    FModeFonctionnementGHTopoContext2D: TModeFonctionnementGHTopoContext2D;
    FOverlayed: boolean;


    procedure DisplayToast(const Msg: string);

    procedure QDeplacerVue(const QX, QY: integer);
     // conversion de coordonnées écran<>générales
    function  GetCoordsMonde(const PP: TPoint): TPoint2Df;
    function  GetCoordsPlan(const QX, QY: double): TPoint; overload;
    function  GetCoordsPlan(const PM: TPoint2Df): TPoint; overload; inline;
    function  GetRYMaxi(): double;

  private
    { private declarations }
    FRedessinInProcess   : boolean;
    FBDDEntites          : TBDDEntites;               // BDD entités
    FDocuTopo            : TToporobotStructure2012;   // interaction avec le document GHTopo
    {$IFDEF GHTOPO_SIMPLIFIE}
    FGraphe              : TPathFindingGraphe;
    FShortestPath        : TPathBetweenNodes;
    {$ENDIF GHTOPO_SIMPLIFIE}
    {$IFNDEF GHTOPO_SIMPLIFIE}
    FCurrIdxOnglet    : byte;
    {$ENDIF GHTOPO_SIMPLIFIE}

    FCroquisTerrain      : TCroquisTerrain;           // crobard de terrain
    FCanDraw             : Boolean;                   // La BDD entités est valide ?
    FAttenduSecondPoint  : boolean; // zoom, pan et ligne
    // modes de travail
    FModesTravail        : TModesTravail;

    // callbacks
    FProcActualiserAffichagesBDD: TProcOfObjectWithOneBoolParameter;         // pour actualisation de la liste des séries après un ajout sur le plan
    FProcPerformActionRecalcul  : TProcOfObjectWithOneBoolParameter;         // pour recalcul du réseau
    FProcUseMetaFiltre          : TProcedureOfObject;                        // pour transmission du contenu du Métafiltre
    FProcTransmitBasePoint      : TProcOfObjectWithATBaseStation;
    //FProcReconnectDistoX        : TProcOfObjectReturnsAsBoolean;             // pour reconnexion du DistoX
    // coordonnées souris en GCS
    FMyPos        : TPoint2Df;
    // surlignage de visées
    FModeSelectionListe : TModeSelectionListe;
    FDoHighLightCurrentItem: boolean; // mettre la série courante en surbrillance
    // station courante
    // La station courante ne peut être définie que dans les cas suivants:
    // 1. Au démarrage
    // 2. Explicitement par la commande acSetCurrentBaseStation()
    // 3. Explicitement par la commande LocaliserUneStationByCle()
    // 4. Par la commande d'insertion d'une série
    FCurrentStation                   : TBaseStation;
    FStationNearToMouse               : TBaseStation;
    FCurrentInternalIdxEntite         : integer;
    // Pour le DistoX
    FCurrentDistoXNumeroSerie         : TNumeroSerie;
    FCurrentDistoXNumeroStation       : TNumeroStation;
    FCurrentDistoXNumeroSecteur       : TNumeroSecteur;
    FCurrentDistoXNumeroCode          : TNumeroCode;
    FCurrentDistoXNumeroExpe          : TNumeroExpe;
    // marqueurs
    FCurrentMarker: TMarker;
    // onglets
    FArrOngletsParams : TArrOngletsParams;
    // gestion des zooms
    FZC1, FZC2    : TPoint2Df;
    FZP1, FZP2    : TPoint;
    FRegionCMini  : TPoint2Df;
    FRegionCMaxi  : TPoint2Df;
    // paramètres de vues
    FRappHLVue      : double;
    FRappScrReal    : double;
    FInvRappScrReal : double;
    // lasso de sélection
    FLassoDeSelection: TGeoPolygon2D;
    // Maillages
    FMyMaillage        : TMaillage;
    FMaillageDisplayed : boolean;
    procedure AppliquerLeFiltre(const FF: string);
    procedure DisplayFullInfosStation(const S: TBaseStation);
    procedure DisplayQuickAddVisee(const OS: TObjSerie);
    // onglets
    function  GetNomOnglet(): string;
    procedure InitCaptions();
    procedure QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
    procedure Rearmer();
    function  UpdateBaseStationWithOffset(const QE: TBaseStation; const QQX, QQY, QQZ: double): TBaseStation;
    procedure SetMarker(const M: TMarker);
    procedure SetNomOnglet(const S: string);
    function  GetBackGrdColorOnglet(const QIdxOnglet: integer): TColor;
    procedure SetBackGrdColorOnglet(const QIdxOnglet: integer; const Col: TColor);
    function  GetLargeurViseesOngletInPX(const QIdxOnglet: integer): integer;
    procedure SetLargeurViseesOngletInPX(const QIdxOnglet: integer; const qL: integer);
    function  GetLargeurViseesOngletInMM(const QIdxOnglet: integer): double;
    procedure SetLargeurViseesOngletInMM(const QIdxOnglet: integer; const qL: double);
    // quadrillage
    function  GetQuadrillageColor(): TColor;
    function  GetQuadrillageSpc(): double;
    procedure SetQuadrillageColor(const Col: TColor);
    procedure SetQuadrillageSpc(const Spacing: double);
    // dessin
    procedure RedessinEcran(const ImgWidth, ImgHeight: integer; const DoRecalcDegrades: boolean; const DoExportInFile: boolean = false; const QFileName: string = '');

  public
    { public declarations }
    property  CanDraw: boolean read FCanDraw write FCanDraw;
    function  Initialiser(const MF : TModeFonctionnementGHTopoContext2D;
                          const QDT: TToporobotStructure2012;
                          const QBDD: TBDDEntites;
                          const QCT: TCroquisTerrain;
                          {$IFDEF GHTOPO_SIMPLIFIE}
                          const QGraphe: TPathFindingGraphe;
                          const QShortestPath: TPathBetweenNodes;
                          {$ENDIF GHTOPO_SIMPLIFIE}
                          const QMaillage: TMaillage;
                          const QProcUseMetaFiltre: TProcedureOfObject;
                          const QProcRefresh: TProcOfObjectWithOneBoolParameter;
                          const QProcRecalculerReseau: TProcOfObjectWithOneBoolParameter;
                          const QProcTransmitBasePoint: TProcOfObjectWithATBaseStation): boolean;
    procedure Finaliser();
    procedure SetModeFonctionnement(const MF : TModeFonctionnementGHTopoContext2D);
     // extraire les pointeurs sur les bases internes
    function GetPtrBDDEntites(): TBDDEntites; inline;
    function GetPtrDocuTopo(): TToporobotStructure2012; inline;
    function GetPtrCroquisTerrain(): TCroquisTerrain; inline;
    function GetPtrMaillage(): TMaillage; inline;

    // assignation des callbacks
    procedure SetProcUseMetaFiltre(const P: TProcedureOfObject);
    procedure DeplacerVue(const QOffsetX, QOffsetY: double);
    procedure DeplacerVueInt(const DepX, DepY: integer);

    procedure SetViewLimits(const X1, Y1, X2, Y2: double; const DebugTag: string); overload;
    procedure SetViewLimits(const QCoinBasGauche, QCointHautDroit: TPoint2Df; const DebugTag: string); overload;

    // Retourne les limites du dessin, stockées dans l'onglet indexé par QIdxOnglet
    function  GetViewLimitsFromOnglet(const QIdxOnglet: integer; out QCoinBasGauche, QCointHautDroit: TPoint2Df): boolean;

    procedure CentrerVueSurPointXY(const QX, QY: double; const DoMarker: boolean; const LbMarker: string);

    procedure ZoomFact(const Fact: double);
    procedure MetaFiltrer(const F: string);
    procedure ResetVue();
    // Numéros internes d'items
    procedure SetCurrentNumeroCode(const S: TNumeroCode);
    procedure SetCurrentNumeroExpe(const S: TNumeroExpe);
    procedure SetCurrentNumeroEntrance(const S: TNumeroEntrance);
    procedure SetCurrentNumeroReseau(const S: TNumeroReseau);
    procedure SetCurrentNumeroSecteur(const S: TNumeroSecteur);
    procedure SetCurrentNumeroNamespace(const S: integer);
    procedure SetKrobardCurrentNumeroStyleAnnotation(const S: byte);
    procedure SetKrobardNumeroStylePolyligne(const S: TKrobardIDStylePolyligne);
    function  GetKrobardNumeroStyleAnnotation(): byte;
    function  GetKrobardNumeroStylePolyligne(): TKrobardIDStylePolyligne;

    // station courante
    function  GetCurrentStation(): TBaseStation;
    procedure SetCurrentStation(const E: TBaseStation; const DoRedraw: boolean);
    procedure SetCurrentStationBySerSt(const S: TNumeroSerie; const P: TNumeroStation = -1);
    // série courante pour le DistoX
    procedure SetCurrentNumeroSerieStationPourDistoX(const S: TNumeroSerie; const P: TNumeroStation);
    function  GetCurrentNumeroSeriePourDistoX(): TNumeroSerie;
    function  GetCurrentNumeroPointPourDistoX(): TNumeroStation;
    // flags
    procedure SetElementsDrawn(const ED: TSetElementsDrawn);
    function  GetElementsDrawn(): TSetElementsDrawn;
    procedure SetModeTravail(const MT: TModesTravail);
    // gestion des onglets
    procedure SetCurrentIdxOnglet(const Idx: byte);
    function  GetCurrentOngletIdx(): integer;
    function  GetNbOngletsVues(): integer;
    function  GetOngletByIndex(const Idx: integer): TVue2DParams;
    function  GetCurrentOnglet(): TVue2DParams;
    procedure PutOngletByIndex(const Idx: integer;
                               const O: TVue2DParams;
                               const DoRedrawVue: boolean);
    procedure SetHighLightCurrentItem(const B: boolean);
    // ajout
    function AjouterUneViseeALaSerieCourante(const QTypeVisee: TTypeDeVisee;
                                             const QLongueur, QAzimut, QPente, QLG, QLD, QHZ, QHN: double;
                                             const QIDLitteral, QCommentaire: string;
                                             const DoRefreshPlan: boolean): boolean;
    function AjouterUneAntenneALaStationCourante(const QLongueur, QAzimut, QPente: double; const DoRefreshPlan: boolean): boolean;

    procedure RollbackLastVisee();
    procedure ApplyMetaFiltre(const B: boolean; const QFiltre: string);
    procedure SetModeSelectionListe(const M: TModeSelectionListe); inline;
    // localiser une station
    procedure LocaliserUneStation();
    procedure LocaliserUneStationByCle(const K: string);
    // créer un timelapse
    procedure CreerTimeLapse(const QDirectory: TStringDirectoryFilename);
    // gestion du croquis de terrain
    function  CroquisDeTerrainIsReady(): boolean;
    procedure SaveCroquis(const QFilename: TStringDirectoryFilename);
    function  OpenCroquis(const QFilename: TStringDirectoryFilename): boolean;
    // lasso de sélection
    function GetLassoDeSelection(): TGeoPolygon2D;
    // chargement du maillage
    procedure DisplayPnlMaillage(const B: boolean);
    function ChargerMaillage(const QFileName: string): boolean;
    function IsMaillageDisplayed(): boolean;
    function IsMaillageValide(): boolean;
    {$IFDEF GHTOPO_SIMPLIFIE}
    procedure TraiterMesureIssueDuDistoX(const MV: TMesureViseeDistoX; const TV: TTypeViseeDistoX; const TagString: string = '');
    procedure SetShortestPath(const P: TPathBetweenNodes);
    {$ENDIF GHTOPO_SIMPLIFIE}
    procedure ExporterPlanEnImage(const ImgWidth, ImgHeight: integer; const DoRecalcDegrades: boolean; const QFileName: string);
    procedure RefreshDessin();

  end;

implementation
uses
  DGCDummyUnit;
{$R *.lfm}
function TGHTopoContext2DA.Initialiser(const MF: TModeFonctionnementGHTopoContext2D;
                                       const QDT: TToporobotStructure2012;
                                       const QBDD: TBDDEntites;
                                       const QCT: TCroquisTerrain;
                                       {$IFDEF GHTOPO_SIMPLIFIE}
                                       const QGraphe: TPathFindingGraphe;
                                       const QShortestPath: TPathBetweenNodes;
                                       {$ENDIF GHTOPO_SIMPLIFIE}
                                       const QMaillage: TMaillage;
                                       const QProcUseMetaFiltre: TProcedureOfObject;
                                       const QProcRefresh: TProcOfObjectWithOneBoolParameter;
                                       const QProcRecalculerReseau: TProcOfObjectWithOneBoolParameter;
                                       const QProcTransmitBasePoint: TProcOfObjectWithATBaseStation): boolean;
var
  C1, C2: TPoint3Df;
  NoOnglet: Integer;
  DbgTag: String;
  MyOnglet: TVue2DParams;
begin
  Result := false;
  AfficherMemoryUsage();
  FRedessinInProcess      := false;
  FModeSelectionListe     := mslSERIE;
  FModesTravail           := mtREADY;
  FDoHighLightCurrentItem := false;
  {$IFNDEF GHTOPO_SIMPLIFIE}
  FCurrIdxOnglet          := 0; // En GHTopo simplifié, il y a un seul onglet utilisé
  {$ENDIF GHTOPO_SIMPLIFIE}

  FModeFonctionnementGHTopoContext2D := MF;

  FOverlayed               := false;
  FCanDraw                 := False;
  //Panel1.DoubleBuffered    := true;
  pnlToast.Visible         := false;
  DisplayPnlMaillage(false);
  FCurrentMarker := MakeTMarker(false, 0.0, 0.0, clAqua, '');
  // pointeurs sur les tables
  FDocuTopo            := QDT;
  FCroquisTerrain      := QCT;
  FBDDEntites          := QBDD;
  {$IFDEF GHTOPO_SIMPLIFIE}
  FGraphe              := QGraphe;
  FShortestPath        := QShortestPath;
  {$ENDIF GHTOPO_SIMPLIFIE}

  FMyMaillage          := QMaillage;
  FMaillageDisplayed   := False;

  // Callbacks
  FProcUseMetaFiltre                   := QProcUseMetaFiltre;
  FProcActualiserAffichagesBDD         := QProcRefresh;
  FProcPerformActionRecalcul           := QProcRecalculerReseau;
  FProcTransmitBasePoint               := QProcTransmitBasePoint;
  // index courants
   FCurrentDistoXNumeroSerie     := 1;  // pour DistoX
  FCurrentDistoXNumeroStation   := 0;  // pour DistoX
  FCurrentInternalIdxEntite     := 0;
  //ShowMessage('6666-GHCDInit -- Init 3');
  // Le format Text, obsolète, pose de gros problèmes ici
  try
    SetCurrentNumeroEntrance(0);
    AfficherMessageErreur('---- 0001');

    SetCurrentNumeroReseau(0);
    AfficherMessageErreur('---- 0002');

    SetCurrentNumeroSecteur(0);
    AfficherMessageErreur('---- 0003');
    SetCurrentNumeroCode(0);
    AfficherMessageErreur('---- 0004');
    SetCurrentNumeroExpe(0);
    AfficherMessageErreur('---- 0005');

    SetCurrentStationBySerSt(1, 0);
    AfficherMessageErreur('---- 0006');

    SetCurrentNumeroSerieStationPourDistoX(1, -1);
    FStationNearToMouse := FCurrentStation;
  except
    ShowMessage('Erreur dans la fixation des items courants '      + #13#10 +
                'Cause probable: Document au format Text'          + #13#10 +
                'Solution: Enregistrer en *.xtb puis recharger');
  end;
  FBDDEntites.SetMinMax(false);
  C1 := FBDDEntites.GetCoinBasGauche();
  C2 := FBDDEntites.GetCoinHautDroit();
  FBDDEntites.CalcLengthReseau();
  FBDDEntites.SortByDepth();       // trier par profondeurs
  SetNomOnglet(rsCDR_VUE2D_TAB_VUE_INITIALE);
  SetQuadrillageColor(clSilver);
  SetQuadrillageSpc(ProposerEquidistanceDef(C1, C2, 100.00));
  SetBackGrdColorOnglet(FCurrIdxOnglet,clWhite);
  SetLargeurViseesOngletInPX(FCurrIdxOnglet, 1);
  SetLargeurViseesOngletInMM(FCurrIdxOnglet, 0.25);
  DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);

  SetViewLimits(C1.X, C1.Y, C2.X, C2.Y, DbgTag); // renseigne aussi l'onglet courant
  //***********************************
  // /!\ Ne pas aller plus loin dans le débogage si çà plante ici
  //ShowMessage('6666-GHCDInit -- Init 47');
  //***********************************
  ApplyMetaFiltre(True, '');                   // renseigne aussi l'onglet courant
  ResetVue();
  // initialiser le tableau d'onglets avec les paramètres de l'onglet 0
  MyOnglet                         := GetOngletByIndex(0);
  MyOnglet.ongDrawFastCenterline   := FBDDEntites.GetLongueurDuReseau() > LONGUEUR_LIMITE_RESEAU;
  MyOnglet.ongQdrType              := qtGRID;
  MyOnglet.ongDegradeAltMini       := FBDDEntites.GetColorZMini();
  MyOnglet.ongDegradeAltMaxi       := FBDDEntites.GetColorZMaxi();
  MyOnglet.ongModeRepresentation   := rgSEANCES;
  MyOnglet.ongFillOpacite          := 128;
  MyOnglet.ongName                 := Format(rsCDR_VUE2D_TAB_VUE, [0]);

  MyOnglet.ongTailleTexteIDStation := 1.80;
  MyOnglet.ongTailleTexteAltitudes := 1.80;
  MyOnglet.ongTailleTexteCotation  := 1.80;
  MyOnglet.ongTailleTexteEntrances := 2.00;
  MyOnglet.ongTailleTexteNoeuds    := 1.90;
  MyOnglet.ongTailleTextePOIs      := 2.10;

  MyOnglet.ongCouleurIDStation     := clBlack;
  MyOnglet.ongCouleurAltitudes     := clBlue;
  MyOnglet.ongCouleurCotation      := clBlue;
  MyOnglet.ongCouleurEntrances     := clGreen;
  MyOnglet.ongCouleurNoeuds        := clFuchsia;
  MyOnglet.ongCouleurPOIs          := clRed;

  MyOnglet.ongDoDispViseesNonRetenues  := True;
  MyOnglet.ongCouleurViseesNonRetenues := clSilver;

  {$IFDEF GHTOPO_SIMPLIFIE}
  //MyOnglet.ongElementsDrawn        := [edQuadrilles, edPolygonals, edStations, edIDStations, edJONCTIONS, edCrossSections, edANTENNES, edCROQUIS];
  MyOnglet.ongElementsDrawn        := [edQuadrilles, edPolygonals, edStations, edCrossSections, edANTENNES, edCROQUIS];
  DisplayPnlMaillage(false);
  btnSerieCouranteDistoX.Visible   := true;
  {$ELSE}
  MyOnglet.ongElementsDrawn        := [edQuadrilles, edPolygonals, edCrossSections, edANTENNES, edCROQUIS, edENTRANCE_MKS];
  // Initialisation des onglets restants
  for NoOnglet := 1 to High(FArrOngletsParams) do
  begin
    MyOnglet.ongName := Format(rsCDR_VUE2D_TAB_VUE, [NoOnglet]);
    PutOngletByIndex(NoOnglet, MyOnglet, False);
  end;
  btnSerieCouranteDistoX.Visible   := false;
  {$ENDIF GHTOPO_SIMPLIFIE}
  PutOngletByIndex(0, MyOnglet, True);
  // Panneau Maillage inhibé si le maillage n'est pas assigné
  DisplayPnlMaillage(FMyMaillage.IsValidMaillage());
  FCanDraw := True;

  InitCaptions();                       // Initialisation de l'interface
  SetLength(FLassoDeSelection, 0);      // lasso de sélection
  Vue.Invalidate;                       // redessiner
  AfficherMemoryUsage();                // afficher mémoire utilisée
  pnlQuickSaisieVisee.Visible := false; // cacher le miniform de visées
  // pour le dessin des étiquettes au survol du plan
  (*FEtiquetteOnSurvolPlan_First  := false;
  FEtiquetteOnSurvolPlan_Tampon := TImage.Create(self);
  FEtiquetteOnSurvolPlan_ox     := 0;
  FEtiquetteOnSurvolPlan_oy     := 0;
  FEtiquetteOnSurvolPlan_ow     := 0;
  FEtiquetteOnSurvolPlan_oh     := 0;
  FEtiquetteOnSurvolPlan_X1     := 0;
  FEtiquetteOnSurvolPlan_Y1     := 0;
  //*)
  //***************************************
  Result   := True;
  //except
 //end;
end;
procedure TGHTopoContext2DA.Finaliser();
begin
  try
    pass;

  finally
    //FreeAndNil(FEtiquetteOnSurvolPlan_Tampon);
  end;
end;




procedure TGHTopoContext2DA.SetModeFonctionnement(const MF: TModeFonctionnementGHTopoContext2D);
begin
  FModeFonctionnementGHTopoContext2D := MF;
  case FModeFonctionnementGHTopoContext2D of
    mfgcSURVEYING : FOverlayed := false;
    mfgcNAVIGATION: FOverlayed := true;
  end;
  Vue.Invalidate;
end;

// pointeurs sur les bases
function TGHTopoContext2DA.GetPtrBDDEntites(): TBDDEntites;
begin
  Result := FBDDEntites;
end;

function TGHTopoContext2DA.GetPtrDocuTopo(): TToporobotStructure2012;
begin
  Result := FDocuTopo;
end;
function TGHTopoContext2DA.GetPtrCroquisTerrain(): TCroquisTerrain; inline;
begin
  Result := FCroquisTerrain;
end;

//*****************************************************************************
procedure TGHTopoContext2DA.InitCaptions();
  procedure SetAcHint(const ACDC: TAction; const QCaption: string);
  begin
    ACDC.Caption := GetResourceString(QCaption);
    ACDC.Hint    := GetResourceString(QCaption);
  end;
begin
  SetAcHint(acAddSerieHere                    , rsCDR_VUE2D_ADD_SERIE_HERE);
  SetAcHint(acContinueHere                    , rsCDR_VUE2D_CONTINUE_SERIE_HERE);
  SetAcHint(acZoomTout                        , rsCDR_VUE2D_AC_ZOOM_ALL);
  SetAcHint(acZoomFenetre                     , rsCDR_VUE2D_AC_ZOOM_WINDOW);
  SetAcHint(acPanVue                          , rsCDR_VUE2D_AC_PAN_VUE);
  SetAcHint(acDistanceP2P                     , rsCDR_VUE2D_AC_PAN_VUE);

  SetAcHint(acDisplayVoisinageCurrStation     , rsCDR_VUE2D_DISP_STATIONS_VOISINES);
  SetAcHint(acAddserieEntreCesPoints          , rsCDR_VUE2D_ADDSERIE_BETWEEN_STATION);
  SetAcHint(acIsovaleurSurZStation            , rsCDR_VUE2D_ISOVALEUR_FROM_Z);
  SetAcHint(acLocaliserUneStation             , rsCDR_VUE2D_LOCALISER_STATION);
  //SetAcHint(acAddAnnotationHere               , rsCDR_VUE2D_ADD_ANNOTATION_HERE);
  SetAcHint(acSaveCroquis                     , rsCDR_VUE2D_AC_CROQUIS_SAVE);
  SetAcHint(acOpenCroquis                     , rsCDR_VUE2D_AC_CROQUIS_OPEN);
  SetAcHint(acDisplayMiniformVisee            , rsCDR_VUE2D_AC_DISP_MINIFORM_VISEE);
  SetAcHint(acSetCurrentBaseStation           , rsCDR_VUE2D_SET_ACTIVE_STATION);
  SetAcHint(acDispPopUp                       , rsCDR_VUE2D_DISP_POPUP);
  {$IFDEF GROS_MINET}  // fonctionnalités désactivées pour Gros_Minet
    acCreerEntreeFromThisPoint.Visible     := false;
    acOpenCroquis.Visible                  := false;
    acSaveCroquis.Visible                  := false;
    acAddserieEntreCesPoints.Visible       := false;
    acIsovaleurSurZStation.Visible         := False;

    acAddSerieHere.Visible                 := False;
    acContinueHere.Visible                 := False;
  {$ENDIF}
end;


procedure TGHTopoContext2DA.SetCurrentNumeroCode(const S: TNumeroCode);
begin
  FDocuTopo.SetCurrentNumeroCode(S);
  Vue.Invalidate;
end;
procedure TGHTopoContext2DA.SetCurrentNumeroExpe(const S: TNumeroExpe);
begin
  FDocuTopo.SetCurrentNumeroExpe(S);
  Vue.Invalidate;
end;

procedure TGHTopoContext2DA.SetCurrentNumeroEntrance(const S: TNumeroEntrance);
begin
  FDocuTopo.SetCurrentNumeroEntrance(S);
  Vue.Invalidate;
end;

procedure TGHTopoContext2DA.SetCurrentNumeroReseau(const S: TNumeroReseau);
begin
  FDocuTopo.SetCurrentNumeroReseau(S);
  Vue.Invalidate;
end;

procedure TGHTopoContext2DA.SetCurrentNumeroSecteur(const S: TNumeroSecteur);
begin
  FDocuTopo.SetCurrentNumeroSecteur(S);
  Vue.Invalidate;
end;

procedure TGHTopoContext2DA.SetCurrentNumeroNamespace(const S: integer);
begin
  FDocuTopo.SetCurrentNumeroNamespace(S);
  Vue.Invalidate;
end;

procedure TGHTopoContext2DA.SetKrobardCurrentNumeroStyleAnnotation(const S: byte);
begin
  FCroquisTerrain.SetCurrentIdxStyleAnnotation(S);
end;

procedure TGHTopoContext2DA.SetKrobardNumeroStylePolyligne(const S: TKrobardIDStylePolyligne);
begin
  pass;
end;

function TGHTopoContext2DA.GetKrobardNumeroStyleAnnotation(): byte;
begin
  Result := FCroquisTerrain.CurrentIdxStyleAnnotation;
end;

function TGHTopoContext2DA.GetKrobardNumeroStylePolyligne(): TKrobardIDStylePolyligne;
begin
  Result := FCroquisTerrain.CurrentIdxStylePolyligne;
end;


function TGHTopoContext2DA.GetCurrentStation(): TBaseStation;
begin
  Result := FCurrentStation;
end;
procedure TGHTopoContext2DA.SetCurrentStation(const E: TBaseStation; const DoRedraw: boolean);
begin
  FCurrentStation := E;
  FDocuTopo.SetCurrentNumeroSerie(FCurrentStation.Entite_Serie);
  FDocuTopo.SetCurrentIndexPointOfSerie(FCurrentStation.Entite_Station);
  SetStationInfo(FCurrentStation);
  //pnlCurrentStation.Caption := Trim(Format('%d.%d %s', [FCurrentStation.Entite_Serie, FCurrentStation.Entite_Station, FCurrentStation.IDTerrain]));
  Vue.Invalidate;
end;

procedure TGHTopoContext2DA.SetCurrentStationBySerSt(const S: TNumeroSerie; const P: TNumeroStation = -1);
var
  QMyCurrStation: TBaseStation;
  ST: Integer;
begin
  ST := IIF( (P = -1), 1, P);
  if (FBDDEntites.GetEntiteViseeFromSerSt(S, St, QMyCurrStation)) then SetCurrentStation(QMyCurrStation, false);
end;

procedure TGHTopoContext2DA.SetCurrentNumeroSerieStationPourDistoX(const S: TNumeroSerie; const P: TNumeroStation);
var
  MySerie: TObjSerie;
  QIdx: integer;
  MyVisee: TUneVisee;
begin
  if (FDocuTopo.GetSerieByNumeroSerie(S, MySerie, QIdx)) then                 // On attrappe du même coup la série demandée par S
  begin
    FCurrentDistoXNumeroSerie  := MySerie.GetNumeroDeSerie();                 // On fixe le numéro de série, avec acquittement via GerNumeroDeSerie()
    MyVisee := MySerie.GetLastVisee();                                        // On attrappe la dernière visée
    if (P < 1) then FCurrentDistoXNumeroStation := MySerie.GetNbVisees() - 1  // et on fixe les numéros de station
               else FCurrentDistoXNumeroStation := P;
    // et on renseigne les secteur, code et expé
    FCurrentDistoXNumeroSecteur       := MyVisee.IDSecteur;
    FCurrentDistoXNumeroCode          := MyVisee.Code;
    FCurrentDistoXNumeroExpe          := MyVisee.Expe;
    btnSerieCouranteDistoX.Caption    := Format('St: %d.%d', [FCurrentDistoXNumeroSerie , FCurrentDistoXNumeroStation]);
    btnSerieCouranteDistoX.Hint       := Format('Station actuelle pour les mesures au DistoX: %d.%d', [FCurrentDistoXNumeroSerie , FCurrentDistoXNumeroStation]);
  end;
end;
function TGHTopoContext2DA.GetCurrentNumeroSeriePourDistoX(): TNumeroSerie;
begin
  Result := FCurrentDistoXNumeroSerie;
end;

function TGHTopoContext2DA.GetCurrentNumeroPointPourDistoX(): TNumeroStation;
begin
  result := FCurrentDistoXNumeroStation;
end;

//******************************************************************************
// Un peu bancal mais au moins, on a tout le réseau dans la vue ;)
// Repris du ResetVue() de GHCaveDraw
procedure TGHTopoContext2DA.ResetVue();
var
  cnMini, cnMaxi   : TPoint3Df;
  PM0, PM1         : TPoint2Df;
  M, dx, dy, L     : double;
  PP               : TPoint;
begin
  if (not FCanDraw) then Exit;
  cnMini := FBDDEntites.GetCoinBasGauche();
  cnMaxi := FBDDEntites.GetCoinHautDroit();

  PP := MakeTPoint(Vue.Width, Vue.Height);
  PM1 := GetCoordsMonde(PP);
  PP := MakeTPoint(0, 0);
  PM0 := GetCoordsMonde(PP);

  DX := cnMaxi.X-cnMini.X;
  DY := cnMaxi.Y-cnMini.Y;
  M  := 0.02 * Hypot(DX, DY);
  L  := IIF(DX > DY, DX, DY);
  DX := PM1.X - PM1.X;
  DY := PM1.Y - PM0.Y;
  SetViewLimits(cnMini.X-M, cnMini.Y-M, cnMini.X + L + M, cnMini.Y + L + M, '');
  Vue.Invalidate;
end;

procedure TGHTopoContext2DA.SetStationInfo(const S: TBaseStation);
var
  MyNamespace            : TNameSpace;
  beuh                   : TPoint3Df;
  EWE                    : String;
  QIdxNameSpace, QNoSerie: integer;

begin
  beuh := FBDDEntites.GetDeltaXYZFromPositionSt0(S);
  lbInfos.Color            := IIF(S.IsPOI, clYellow, clDefault);
  acDeleteThisPOI.Visible  := S.IsPOI;
  DecomposeNumeroSerie(S.Entite_Serie, QIdxNameSpace, QNoSerie);
  MyNamespace := FDocuTopo.GetNameSpace(QIdxNameSpace);
  EWE := Format('%d.%d%s', [QNoSerie, S.Entite_Station, IIF(0 = QIdxNameSpace, '', '@' + MyNamespace.Nom)]);
  lbInfos.Caption := Trim(Format('%s (X = %s; Y = %s; Alt = %s; Z = %s ) %s',
                          [EWE,
                           FormatterNombreAvecSepMilliers(S.PosStation.X),
                           FormatterNombreAvecSepMilliers(S.PosStation.Y),
                           FormatterNombreAvecSepMilliers(S.PosStation.Z),
                           FormatterNombreAvecSepMilliers(beuh.Z),
                           S.oCommentaires]));
end;

procedure TGHTopoContext2DA.SpeedButton13Click(Sender: TObject);
begin
  pass;
end;

procedure TGHTopoContext2DA.SpeedButton13MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  QDeplacerVue(1, 0);
end;

procedure TGHTopoContext2DA.SpeedButton17Click(Sender: TObject);
begin

end;



procedure TGHTopoContext2DA.PopUpCdrVue2DPopup(Sender: TObject);
var
  QNoSerie, QIdxNamespace: integer;
  MyNamespace: TNameSpace;
  procedure S666(const ACDC: TAction; const QCaption: string);
  begin
    ACDC.Caption := GetResourceString(QCaption);
    ACDC.Hint    := GetResourceString(QCaption);
  end;
begin
  DecomposeNumeroSerie(FStationNearToMouse.Entite_Serie, QIdxNamespace, QNoSerie);
  MyNamespace := FDocuTopo.GetNameSpace(QIdxNamespace);
  mnuMetaFiltreStation.Caption := Format(GetResourcestring(rsCDR_VUE2D_MNU_FILTRES_STATION),
                                          [FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station, FStationNearToMouse.IDTerrain]);
  mnuHighLight.Caption         := Format(GetResourcestring(rsCDR_VUE2D_MNU_HIGHLIGHT_BY_STATION),
                                          [FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station, FStationNearToMouse.IDTerrain]);
  // titres du sous-menu
  S666(acZoomTout                 , rsCDR_VUE2D_AC_ZOOM_ALL);
  S666(acZoomFenetre              , rsCDR_VUE2D_AC_ZOOM_WINDOW);
  S666(acPanVue                   , rsCDR_VUE2D_AC_PAN_VUE);
  S666(acDistanceP2P              , rsCDR_VUE2D_AC_DISTANCE_BT_STATIONS);
  S666(acMetaFiltreSerie          , Format(rsCDR_VUE2D_AC_METAFILTRE_SERIE        , [QNoSerie]));
  S666(acMetaFiltreNamespace      , Format(rsCDR_VUE2D_AC_METAFILTRE_NAMESPACE    , [QIdxNamespace, MyNamespace.Nom]));
  S666(acMetaFiltreTopoDuJour     , Format(rsCDR_VUE2D_AC_METAFILTRE_TOPO_DU_JOUR , [DateToStr(FStationNearToMouse.DateLeve)]));
  S666(acMetaFiltreReseau         , Format(rsCDR_VUE2D_AC_METAFILTRE_RESEAU       , [FStationNearToMouse.eReseau]));
  S666(acMetaFiltreEntree         , Format(rsCDR_VUE2D_AC_METAFILTRE_ENTRANCE     , [FStationNearToMouse.eReseau]));
  S666(acMetaFiltreSecteur        , Format(rsCDR_VUE2D_AC_METAFILTRE_SECTEUR      , [FStationNearToMouse.eSecteur]));
  S666(acMetaFiltreCode           , Format(rsCDR_VUE2D_AC_METAFILTRE_CODE         , [FStationNearToMouse.eCode]));
  S666(acMetaFiltreExpe           , Format(rsCDR_VUE2D_AC_METAFILTRE_EXPE         , [FStationNearToMouse.eExpe]));
  S666(acMetaFiltreTopoAnnee      , Format(rsCDR_VUE2D_AC_METAFILTRE_YEAR         , [YearOf(FStationNearToMouse.DateLeve)]));
  S666(acMetaFiltreVueCourante    , rsCDR_VUE2D_AC_METAFILTRE_CURRENT_VIEW);
  S666(acIsovaleurSurZStation     , Format(rsCDR_VUE2D_AC_ISOVALEUR_Z             , [FStationNearToMouse.PosStation.Z]));
  // highlight
  S666(acHighlightThisSerie       , Format(rsCDR_VUE2D_AC_METAFILTRE_SERIE        , [QNoSerie]));
  S666(acHighLightThisCode        , Format(rsCDR_VUE2D_AC_METAFILTRE_CODE         , [FStationNearToMouse.eCode]));
  S666(acHighLightThisExpe        , Format(rsCDR_VUE2D_AC_METAFILTRE_EXPE         , [FStationNearToMouse.eExpe]));
  S666(acEditThisSerie            , Format(rsCDR_VUE2D_AC_EDIT_THIS_SERIE         , [QNoSerie]));
  // POI
  S666(acDeleteThisPOI            , rsCDR_VUE2D_AC_DELETE_THIS_POI);
  S666(acAddPOIHere               , rsCDR_VUE2D_AC_ADD_POI_HERE);
end;


procedure TGHTopoContext2DA.QDeplacerVue(const QX, QY: integer);
var
  WUX, WUY: Double;
begin
  WUX := -0.05 * (FRegionCMaxi.X - FRegionCMini.X) * QX;
  WUY := -0.05 * (FRegionCMaxi.Y - FRegionCMini.Y) * QY;
  DeplacerVue(WUX, WUY);
  self.RefreshDessin();
end;
procedure TGHTopoContext2DA.Button2Click(Sender: TObject);
begin
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.btnQSAddViseeClick(Sender: TObject);
begin
  AjouterUneViseeALaSerieCourante(tgDEFAULT,
                                  editQSLongueur.Value, editQSAzimut.Value, editQSPente.Value,
                                  editQSLeft.Value, editQSRight.Value, editQSUp.Value,editQSDown.Value,
                                  '',
                                  Trim(editQSCommentaire.Text),
                                  True);
  editQSLongueur.SetFocus;
end;

procedure TGHTopoContext2DA.Button3Click(Sender: TObject);
begin
  pass;
end;

procedure TGHTopoContext2DA.Button4Click(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon(Format('Supprimer la station %d.%d', [FCurrentDistoXNumeroSerie, FCurrentDistoXNumeroStation]))) then RollbackLastVisee();
end;

procedure TGHTopoContext2DA.btnQSCloseMiniFormViseeClick(Sender: TObject);
begin
  pnlQuickSaisieVisee.Visible := false;
end;

procedure TGHTopoContext2DA.acMetaFiltreSerieExecute(Sender: TObject);
var
  FF: String;
begin
  FF := Format('%s=%d',[rsMETAFILTRE_SERIE, FStationNearToMouse.Entite_Serie]);
  ApplyMetaFiltre(True, FF);
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.acMetaFiltreTopoAnneeExecute(Sender: TObject);
var
  YYYY,MM,DD: word;
  S1, S2, FF: string;
begin
  DecodeDate(FStationNearToMouse.DateLeve, YYYY, MM, DD);
  S1:=DateToStr(GetSecuredDate(YYYY, 01, 01));
  S2:=DateToStr(GetSecuredDate(YYYY, 12, 31));
  FF := Format('%s=(%s, %s)',[rsMETAFILTRE_DATE, S1, S2]);
  ApplyMetaFiltre(True, FF);
end;

procedure TGHTopoContext2DA.acMetaFiltreTopoDuJourExecute(Sender: TObject);
var
  YYYY, MM, DD: Word;
  S1, FF: String;
begin
  DecodeDate(FStationNearToMouse.DateLeve, YYYY, MM, DD);
  S1 := DateToStr(GetSecuredDate(YYYY, MM, DD));
  FF := Format('%s=%s',[rsMETAFILTRE_DATE, S1]);
  ApplyMetaFiltre(True, FF);
end;

procedure TGHTopoContext2DA.acMetaFiltreVueCouranteExecute(Sender: TObject);
var
  PP: TPoint;
  FC1, FC2: TPoint2Df;
  FF: String;
begin
  PP.X := 0;
  PP.Y := Vue.Height;
  FC1 := GetCoordsMonde(PP);
  PP.X := Vue.Width;
  PP.Y := 0;
  FC2 := GetCoordsMonde(PP);
  FF := Format('COORD_X=(%.2f, %.2f) & COORD_Y=(%.2f, %.2f)', [FC1.X, FC2.X, FC1.Y, FC2.Y]);
  ApplyMetaFiltre(True, FF);
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.acMetaFiltreZoneExecute(Sender: TObject);
begin
  SetModeTravail(mtMETAFILTRE_ZONE_PREMIER_COIN);
end;

procedure TGHTopoContext2DA.acOpenCroquisExecute(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
begin
  QFilename := '';
  if (DoDialogOpenFile(GetGHTopoDirectory(), 'Fichiers croquis (*.xml)|*.xml', '.xml', QFilename)) then
  begin
    OpenCroquis(QFilename);
  end;
end;

procedure TGHTopoContext2DA.acPanVueExecute(Sender: TObject);
begin
  self.SetModeTravail(mtPAN_PREMIER_POINT);
end;



procedure TGHTopoContext2DA.acRemoveLastViseeExecute(Sender: TObject);
var
  MySerie: TObjSerie;
  QIdx: integer;
begin
  if (GHTopoQuestionOuiNon('Supprimer la dernière visée')) then
  begin
    if (FDocuTopo.GetSerieByNumeroSerie(self.FCurrentDistoXNumeroSerie, MySerie, QIdx)) then
    begin
      if (MySerie.GetNbVisees() > 1) then
      begin
        MySerie.RemoveLastElement();
        if (assigned(FProcPerformActionRecalcul)) then FProcPerformActionRecalcul(False);
      end;
    end;
  end;
end;

procedure TGHTopoContext2DA.acSaveCroquisExecute(Sender: TObject);
var
  MyFolderQSave: TStringDirectoryFilename;
  WU: TDateTime;
  YYYY, MM, DD, HH, MN, SS, MS: word;
  MyQSaveFileName: TStringDirectoryFilename;
begin
  if (not FCroquisTerrain.IsReady) then Exit;
  MyFolderQSave := GetGHTopoDirectory() + MON_DOSSIER_CROQUIS + PathDelim;
  ForceDirectories(MyFolderQSave);
  WU := Now();
  DecodeDate(WU, YYYY, MM, DD);
  DecodeTime(WU, HH, MN, SS, MS);
  (*
  MyQSaveFileName := MyFolderQSave +
                     Format('%s_%.04d-%.02d-%.02d_%.02dh%.02dm%.02ds%.03dms_croquis.xml',
                            ['QSave',
                              YYYY, MM, DD,
                              HH, MN, SS, MS]);
                            //*)
  MyQSaveFileName := MyFolderQSave + 'Crobard001.xml';
  SaveCroquis(MyQSaveFileName);
end;

procedure TGHTopoContext2DA.acSetCurrentBaseStationExecute(Sender: TObject);
begin
  SetCurrentStation(FStationNearToMouse, True);
end;

procedure TGHTopoContext2DA.acSetCurrentSerieExecute(Sender: TObject);
begin
  pass;
end;

procedure TGHTopoContext2DA.SaveCroquis(const QFilename: TStringDirectoryFilename);
begin
  FCroquisTerrain.SaveToXML(QFilename);
end;
function  TGHTopoContext2DA.OpenCroquis(const QFilename: TStringDirectoryFilename): boolean;
begin
  Result := FCroquisTerrain.LoadFromXML(QFileName);
  self.RefreshDessin();
end;

function TGHTopoContext2DA.GetLassoDeSelection(): TGeoPolygon2D;
begin
  Result := FLassoDeSelection;
end;

procedure TGHTopoContext2DA.acSupprimerSerieExecute(Sender: TObject);
var
  MyNumeroSerie: TNumeroSerie;
  MySerie: TObjSerie;
  QInternaIdxSerie: integer;
begin
  MyNumeroSerie := self.FCurrentStation.Entite_Serie;
  if (FDocuTopo.GetSerieByNumeroSerie(MyNumeroSerie, MySerie, QInternaIdxSerie)) then
  begin
    if (GHTopoQuestionOuiNon(Format('Supprimer %d: %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]))) then
    begin
      FDocuTopo.RemoveSerie(QInternaIdxSerie);
    end;
  end
  else
  begin
    ShowMessage(GetResourceString(rsMATCHNOTFOUND));
  end;
end;

procedure TGHTopoContext2DA.acEditThisSerieExecute(Sender: TObject);
begin
  if (Assigned(FProcTransmitBasePoint)) then FProcTransmitBasePoint(FStationNearToMouse, tdoAcnBASESTATION_EDITSERIE);
end;

procedure TGHTopoContext2DA.acTransmitBasePointForHighlightingExecute(Sender: TObject);
begin
  if (Assigned(FProcTransmitBasePoint)) then FProcTransmitBasePoint(FStationNearToMouse, tdoAcnBASESTATION_DISPLAY);
end;

procedure TGHTopoContext2DA.acZoomFenetreExecute(Sender: TObject);
begin
  Self.SetModeTravail(mtZOOM_PREMIER_COIN);
end;

procedure TGHTopoContext2DA.acZoomMoinsExecute(Sender: TObject);
begin
  if (FCanDraw) then ZoomFact(1 - 0.05);
end;

procedure TGHTopoContext2DA.acZoomPlusExecute(Sender: TObject);
begin
  if (FCanDraw) then ZoomFact(1 + 0.05);
end;

procedure TGHTopoContext2DA.acZoomToutExecute(Sender: TObject);
begin
  self.ResetVue();
end;

procedure TGHTopoContext2DA.btnMoreInfosClick(Sender: TObject);
begin
  pnlFullInfos.Visible := false;
end;

procedure TGHTopoContext2DA.btnSerieCouranteDistoXClick(Sender: TObject);
var
  QIdxSelected: integer;
begin
  if (SelectionDansListe(FDocuTopo, mslSERIE, false, QIdxSelected)) then SetCurrentNumeroSerieStationPourDistoX(QIdxSelected, -1);
end;

procedure TGHTopoContext2DA.btnUnloadMNTClick(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Décharger le MNT')) then
  begin
    FMyMaillage.ResetMaillageTIN();
    DisplayPnlMaillage(FMyMaillage.IsValidMaillage());
    Vue.Invalidate;
  end;
end;

procedure TGHTopoContext2DA.Button1Click(Sender: TObject);
var
  EWE: TCaption;
begin
  EWE := editMetaFiltre.Text;
  if (DisplayAndEditFiltres(FDocuTopo, EWE)) then
  begin
    editMetaFiltre.Text := EWE;
    ApplyMetaFiltre(chkMetaFiltreActif.Checked, trim(editMetaFiltre.Text));
  end;
end;

procedure TGHTopoContext2DA.acLocaliserUneStationExecute(Sender: TObject);
begin
  LocaliserUneStation();
end;

procedure TGHTopoContext2DA.acAddserieEntreCesPointsExecute(Sender: TObject);
begin
   self.SetModeTravail(mtADD_SERIE_PREMIER_POINT);
end;

procedure TGHTopoContext2DA.acAddAnnotationHereExecute(Sender: TObject);
begin
  pass;
end;

procedure TGHTopoContext2DA.acAddPOIHereExecute(Sender: TObject);
var
  n: TNumeroSerie;
  QValue: String;
  SR: TObjSerie;
  QIdx: integer;
  QVisee: TUneVisee;
begin
  QValue := '';
  if (FDocuTopo.GetSerieByNumeroSerie(FStationNearToMouse.Entite_Serie, SR, QIdx)) then
  begin
    QVisee := SR.GetVisee(FStationNearToMouse.Entite_Station);
    if (InputQuery(Format('Nouveau POI depuis la station %d.%d', [FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station]), 'Point d''intérêt', QValue)) then
    begin
      if (QValue = '') then exit;
      QVisee.Commentaires := '$POI:' + QValue;
      SR.PutVisee(FStationNearToMouse.Entite_Station, QVisee);
    end;

  end;
end;

procedure TGHTopoContext2DA.acAddSerieHereExecute(Sender: TObject);
var
  MySerie            : TObjSerie;
  QCBG, QCHD         : TPoint2Df;
  QParamOnglet       : TVue2DParams;
  QValue, WU         : String;
  DbgTag             : string;
  n                  : TNumeroSerie;
  QMargeX, QMargeY   : double;
  MyBaseStation      : TBaseStation;
  QIdx               : integer;
begin
  n := 1 + FDocuTopo.getMaxIdxSerie();
  MyBaseStation := FStationNearToMouse;
  QValue := Format(FORMAT_NB_INTEGER, [n]);
  if (InputQuery(Format('Nouvelle série depuis la station %d.%d', [MyBaseStation.Entite_Serie, MyBaseStation.Entite_Station]), 'Numéro de série', QValue)) then
  begin
    n := StrToIntDef(Trim(QValue), 1 + FDocuTopo.getMaxIdxSerie());
    if (FDocuTopo.GetSerieByNumeroSerie(n, MySerie, QIdx)) then
    begin
      WU := Format(rsNUM_SERIE_ALREADY_ATTRIBUTED, [n]);
      if (not GHTopoQuestionOuiNon(GetResourceString(WU + ' - ' + rsALLOW_ATTRIBUTE_NUM_SERIE))) then Exit;
      n := -1;
    end;
    QParamOnglet := GetOngletByIndex(FCurrIdxOnglet);    // on récupère les paramètres de vue et d'onglet
    GetViewLimitsFromOnglet(FCurrIdxOnglet, QCBG, QCHD);
    QMargeX := 0.50 * (QCHD.X - QCBG.X);
    QMargeY := 0.50 * (QCHD.Y - QCBG.Y);
    if (FDocuTopo.CreateNewSerie(MyBaseStation.Entite_Serie, MyBaseStation.Entite_Station,
                                 n,
                                 Format('Série ajoutée par pointage en %d.%d', [MyBaseStation.Entite_Serie, MyBaseStation.Entite_Station]))) then
    begin
      MySerie.SetNoPointArr(-1);                              // attribution automatique du point d'arrivée
      MySerie := FDocuTopo.GetLastSerie();                    //on récupère la station nouvellement créée
      SetViewLimits(MyBaseStation.PosStation.X - QMargeX, MyBaseStation.PosStation.Y - QMargeY,
                    MyBaseStation.PosStation.X + QMargeX, MyBaseStation.PosStation.Y + QMargeY,
                    'Centrage');
      PutOngletByIndex(FCurrIdxOnglet, QParamOnglet, false);
      DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
      CentrerVueSurPointXY(MyBaseStation.PosStation.X, MyBaseStation.PosStation.Y, true, 'Nouvelle série ici');
      SetCurrentStation(MyBaseStation, True);
      SetCurrentNumeroSerieStationPourDistoX(MySerie.GetNumeroDeSerie(), -1);
      {$IFDEF GHTOPO_SIMPLIFIE}
      pass;
      {$ELSE}
      pnlQuickSaisieVisee.Visible := True;
      {$ENDIF}
    end
    else
    begin
      ShowMessage(GetResourceString(rsFAILED_TO_ADD_SERIE));
    end;
  end;
end;

function TGHTopoContext2DA.UpdateBaseStationWithOffset(const QE: TBaseStation; const QQX, QQY, QQZ: double): TBaseStation;
begin
  Result            := QE;
  Result.PosExtr0   := QE.PosStation;
  Result.PosStation := MakeTPoint3Df(Result.PosExtr0.X + QQX, Result.PosExtr0.Y + QQY, Result.PosExtr0.Z + QQZ);
  Result.PosOPG     := Result.PosExtr0;
  Result.PosOPD     := Result.PosExtr0;
  Result.PosPD      := Result.PosStation;
  Result.PosPG      := Result.PosStation;
end;
function TGHTopoContext2DA.AjouterUneViseeALaSerieCourante(const QTypeVisee: TTypeDeVisee;
                                                           const QLongueur, QAzimut, QPente, QLG, QLD, QHZ, QHN: double;
                                                           const QIDLitteral, QCommentaire: string;
                                                           const DoRefreshPlan: boolean): boolean;
var
  QX, QY, QZ, QP: double;
  CC: TCode;
  QInternalIdxserie, NbVisees: integer;
  MySerie: TObjSerie;
  MyBaseStation: TBaseStation;
  EE: TExpe;
  VS: TUneVisee;
begin
  Result := false;

  if (not FDocuTopo.GetSerieByNumeroSerie(FCurrentDistoXNumeroSerie, MySerie, QInternalIdxserie)) then
  begin
    ShowMessage('-- EE: Serie not found');
    Exit(false);
  end;

  NbVisees := MySerie.GetNbVisees();
  // Si le nombre de visées de la série courante est <= 1, on attrappe la station d'accrochage pour le paramétrage
  // (nota: Lors de la création d'une série, une visée zéro est toujours créée, donc NbVisées sera égal à 1 au minimum)
  if (NbVisees <= 1) then FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), MyBaseStation)
                     else FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNumeroDeSerie(), MySerie.GetNbVisees() - 1, MyBaseStation);
  MyBaseStation := GetCurrentStation();
  // c'est une visée de cheminement ?
  if (QTypeVisee in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON, tgSURFACE, tgTUNNEL, tgMINE]) then
  begin
    CC := FDocuTopo.GetCodeByNumero(MyBaseStation.eCode);
    EE := FDocuTopo.GetExpeByNumero(MyBaseStation.eExpe);
    VS.IDSecteur := MyBaseStation.eSecteur;
    VS.TypeVisee := QTypeVisee;
    VS.Code      := CC.IDCode;
    VS.Expe      := EE.IDExpe;
    VS.Longueur  := QLongueur;
    VS.Azimut    := QAzimut;
    VS.Pente     := QPente;
    VS.LG        := QLG;
    VS.LD        := QLD;
    VS.HZ        := QHZ;
    VS.HN        := QHN;
    VS.IDTerrainStation := QIDLitteral;
    VS.Commentaires := QCommentaire;
    MySerie.AddVisee(VS);                                                         // on ajoute la visée:
    MySerie.SetSeriePtArr(MySerie.GetNumeroDeSerie(), MySerie.GetNbVisees() - 1); // et on met à jour l'objet MySerie:
    // Pour éviter de devoir recalculer tout le réseau, on crée une nouvelle entité dans FBDDEntites;
    QX := 0.00; QY := 0.00; QZ := 0.00; QP := 0.00;
    CalculerVisee(VS, CC, EE, QX, QY, 1.00, QZ, QP);
    MyBaseStation := UpdateBaseStationWithOffset(MyBaseStation, QX, QY, QZ);
    MyBaseStation.Entite_Serie   := MySerie.GetNumeroDeSerie();
    MyBaseStation.Entite_Station := MySerie.GetNbVisees() - 1;
    MyBaseStation.IDTerrain      := QIDLitteral;
    MyBaseStation.oCommentaires  := QCommentaire;
    MyBaseStation.IsPOI          := false;
    MyBaseStation.Highlighted    := false;
    // on ajoute l'entité créée à la table des entités
    FBDDEntites.AddEntiteVisee(MyBaseStation);
    // On définit la nouvelle station courante
    MyBaseStation.Entite_Serie   := MySerie.GetNumeroDeSerie();
    MyBaseStation.Entite_Station := MySerie.GetNbVisees() - 1;
    // on actualise la base sans recompiler
    if (assigned(FProcActualiserAffichagesBDD)) then FProcActualiserAffichagesBDD(false);
    FBDDEntites.SetMinMax(false);                                            // on redéfinit les limites de vue
    SetCurrentStation(MyBaseStation, True);                                  // et on met à jour la station courante
    SetCurrentNumeroSerieStationPourDistoX(MySerie.GetNumeroDeSerie(), -1);  // ainsi que celle du DistoX
    // On centre le plan sur la nouvelle visée créée
    CentrerVueSurPointXY(FCurrentStation.PosStation.X, FCurrentStation.PosStation.Y, true, 'Station added');
  end;
  if (DoRefreshPlan) then RedessinEcran(Vue.Width, Vue.Height, true);
end;



function TGHTopoContext2DA.AjouterUneAntenneALaStationCourante(const QLongueur, QAzimut, QPente: double; const DoRefreshPlan: boolean): boolean;
var
  QX, QY, QZ, QP: double;
  CC: TCode;
  EE: TExpe;
  QNouvelleEntite: TBaseStation;
  VS: TUneVisee;
  VA: TViseeAntenne;
begin
  Result := false;
  CC := FDocuTopo.GetCodeByNumero(FCurrentDistoXNumeroCode);
  EE := FDocuTopo.GetExpeByNumero(FCurrentDistoXNumeroExpe);
  VA := MakeTViseeAntenne(FCurrentStation.eEntrance, FCurrentStation.eReseau,
                          FCurrentDistoXNumeroSecteur,
                          FCurrentDistoXNumeroSerie, FCurrentDistoXNumeroStation,
                          QLongueur, QAzimut, QPente);
  FDocuTopo.AddViseeAntenne(VA);
  QX := 0.00; QY := 0.00; QZ := 0.00; QP := 0.00;
  VS := MakeTUneVisee(0,
                      tgVISEE_RADIANTE,
                      FCurrentDistoXNumeroSecteur,
                      FCurrentDistoXNumeroCode,
                      FCurrentDistoXNumeroExpe,
                      QLongueur, QAzimut, QPente, 0.0, 0.0, 0.0, 0.0);
  CalculerVisee(VS, CC, EE, QX, QY, 1.0, QZ, QP);
  QNouvelleEntite := UpdateBaseStationWithOffset(FCurrentStation, QX, QY, QZ);
  QNouvelleEntite.Type_Entite    := tgVISEE_RADIANTE;
  QNouvelleEntite.Entite_Serie   := FCurrentDistoXNumeroSerie;
  QNouvelleEntite.Entite_Station := FCurrentDistoXNumeroStation;
  QNouvelleEntite.IDTerrain      := '';
  QNouvelleEntite.oCommentaires  := '';
  FBDDEntites.AddEntiteAntenne(QNouvelleEntite);
  if (DoRefreshPlan) then RedessinEcran(Vue.Width, Vue.Height, true);   // on actualise la base sans recompiler
  Result := True;
end;

procedure TGHTopoContext2DA.RollbackLastVisee();
var
  MySerie: TObjSerie;
  QIdx, n: integer;
begin
  AfficherMessage(format('%s.RollbackLastVisee: %d.%d', [ClassName, FCurrentDistoXNumeroSerie, FCurrentDistoXNumeroStation]));
  if (FDocuTopo.GetSerieByNumeroSerie(FCurrentDistoXNumeroSerie, MySerie, QIdx)) then
  begin
    n := MySerie.GetNbVisees();
    ShowMessageFmt('%d visées', [MySerie.GetNbVisees()]);
    if (n <= 1) then
    begin
      ShowMessage('Pas de visée à supprimer');
      exit;
    end;
    if (FCurrentDistoXNumeroStation <> (n - 1))then
    begin
      ShowMessageFmt('Impossible de supprimer une station intermédiaire %d - %d', [FCurrentDistoXNumeroStation, n - 1]);
      exit;
    end;
    MySerie.RemoveLastVisee();
    FProcPerformActionRecalcul(false);  // /!\ Recalcul indispensable !
  end;
end;




procedure TGHTopoContext2DA.acAddViseeRadianteHereExecute(Sender: TObject);
begin
  pass;
end;

procedure TGHTopoContext2DA.acBtnPanEEExecute(Sender: TObject);
begin
  QDeplacerVue(-1, 0);
end;

procedure TGHTopoContext2DA.acBtnPanNNExecute(Sender: TObject);
begin
  QDeplacerVue(0, -1);
end;

procedure TGHTopoContext2DA.acBtnPanSSExecute(Sender: TObject);
begin
  QDeplacerVue(0, 1);
end;

procedure TGHTopoContext2DA.acBtnPanWWExecute(Sender: TObject);
begin
  QDeplacerVue(1, 0);
end;

// behaviour of 'Continue here' of PocketTopo  - Même comportement que le 'Continuer ici' de PocketTopo
procedure TGHTopoContext2DA.acContinueHereExecute(Sender: TObject);
begin
  SetCurrentStation(FStationNearToMouse, false);
  SetCurrentNumeroSerieStationPourDistoX(FStationNearToMouse.Entite_Serie, -1);
  //pnlQuickSaisieVisee.Visible := True;
end;

procedure TGHTopoContext2DA.acCreerEntreeFromThisPointExecute(Sender: TObject);
var
  EE: TEntrance;
  WU: String;
  n: Integer;
begin
  WU := Format(FMTSERST, [FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station]);
  EE := MakeTEntrance('Entrance ' + WU,
                      WU,
                      FStationNearToMouse.PosStation.X, FStationNearToMouse.PosStation.Y, FStationNearToMouse.PosStation.Z,
                      FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station,
                      '');
  n := FDocuTopo.GetNbEntrances();
  if (EditerEntrance(FDocuTopo, n, FBDDEntites, nil, EE)) then
  begin
    FDocuTopo.AddEntrance(EE);
    n := FDocuTopo.GetNbEntrances();
    self.SetModeTravail(mtREADY);
  end;
end;

procedure TGHTopoContext2DA.acDeleteThisPOIExecute(Sender: TObject);
var
  SR: TObjSerie;
  ST: TUneVisee;
  QIdx: integer;
  EWE: String;
begin
  if (FDocuTopo.GetSerieByNumeroSerie(FStationNearToMouse.Entite_Serie, SR, QIdx)) then
  begin
    ST := SR.GetVisee(FStationNearToMouse.Entite_Station);
    EWE := StringReplace(ST.Commentaires, KEYWORD_POI_TODO, '', [rfIgnoreCase, rfReplaceAll]);
    if (GHTopoInputQuery('Supprimer un POI', Format('Nouveau commentaire pour %d.%d', [FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station]), EWE)) then
    begin
      ST.Commentaires := EWE;
      SR.PutVisee(FStationNearToMouse.Entite_Station, ST);
      ShowMessage('Recalculer le réseau pour actualiser la liste des POIs');
    end;
  end
  else
  begin
    ShowMessage('Station introuvable');
  end;
end;

procedure TGHTopoContext2DA.acDisplayMiniformViseeExecute(Sender: TObject);
begin
  pnlQuickSaisieVisee.Visible := Not pnlQuickSaisieVisee.Visible;
end;

procedure TGHTopoContext2DA.acDisplayVoisinageCurrStationExecute(Sender: TObject);
begin
  DisplayNearStationsAtPoint(FDocuTopo, FBDDEntites, FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station, RAYON_CAPTURE_NEAR_STATIONS);
end;

procedure TGHTopoContext2DA.acDispPopUpExecute(Sender: TObject);
var
  PP: TPoint;
  MyGHTopoMainWnd, MyPnlPlan, MyTabShtPlan, MyPageCtrl: TWinControl;
  QX, QY: Integer;
  EWE: String;
begin
  // La fenêtre contenant le cadre
  //                       >le conteneur du cadre (pnlPlan)
  //                       |     >l'onglet (tabVueEnPlan)
  //                       |     |     > le PageControl
  //                       |     |     |      > La fenêtre principale
  //                       |     |     |      |
  //                       V     V     V      V
  //MyGHTopoMainWnd := self.Parent.Parent.Parent.Parent;
  (*
  MyPnlPlan       := self.Parent;
  MyTabShtPlan    := MyPnlPlan.Parent;
  MyPageCtrl      := MyTabShtPlan.Parent;
  MyGHTopoMainWnd := MyPageCtrl.Parent;
  EWE := Format('%s: %d, %d' + #10, [MyPnlPlan.Name, MyPnlPlan.Left, MyPnlPlan.Top]);
  EWE += Format('%s: %d, %d' + #10, [MyTabShtPlan.Name, MyTabShtPlan.Left, MyTabShtPlan.Top]);
  EWE += Format('%s: %d, %d' + #10, [MyPageCtrl.Name, MyPageCtrl.Left, MyPageCtrl.Top]);
  EWE += Format('%s: %d, %d' + #10, [MyGHTopoMainWnd.Name, MyGHTopoMainWnd.Left, MyGHTopoMainWnd.Top]);
  //ShowMessage(EWE);
  QX := MyGHTopoMainWnd.Left +
        MyPageCtrl.Left +
        MyTabShtPlan.Left +
        MyPnlPlan.Left +
        self.Left +
        Panel1.Left;
  QY := MyGHTopoMainWnd.Top +
        MyPageCtrl.Top +
        MyTabShtPlan.Top +
        MyPnlPlan.Top +
        self.Top +
        Panel1.Top;
  PP := GetCoordsPlan(FCurrentStation.PosStation.X, FCurrentStation.PosStation.Y);
  PopUpCdrVue2D.PopUp(QX + PP.X, QY + PP.Y);
  //*)
  PP := GetCoordsPlan(FCurrentStation.PosStation.X, FCurrentStation.PosStation.Y);
  PP := Panel1.ClientToScreen(PP);
  PopUpCdrVue2D.PopUp(PP.X, PP.Y);
end;

procedure TGHTopoContext2DA.acDistanceP2PExecute(Sender: TObject);
begin
  self.SetModeTravail(mtDISTANCE_PREMIER_POINT);
end;

procedure TGHTopoContext2DA.acGetStationCoordsExecute(Sender: TObject);
var
  EWE: String;
  CT: TClipboard;
begin
  EWE := Format('%d.%d; %.3f; %.3f; %.3f; %s',
         [FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station,
          FStationNearToMouse.PosStation.X, FStationNearToMouse.PosStation.Y, FStationNearToMouse.PosStation.Z,
          FStationNearToMouse.IDTerrain
         ]);
  CT := TClipboard.Create(ctClipBoard);
  try
    CT.Clear;
    CT.AsText := EWE;
  finally
    FreeAndNil(CT);
  end;
end;

procedure TGHTopoContext2DA.acHighLightThisCodeExecute(Sender: TObject);
begin
  FBDDEntites.HighLightVisees(mslCODE, FStationNearToMouse.eCode);
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.acHighLightThisExpeExecute(Sender: TObject);
begin
  FBDDEntites.HighLightVisees(mslEXPE, FStationNearToMouse.eExpe);
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.acHighlightThisSerieExecute(Sender: TObject);
begin
  FBDDEntites.HighLightVisees(mslSERIE, FCurrentStation.Entite_Serie);
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.acMetaFiltreCodeExecute(Sender: TObject);
begin
  AppliquerLeFiltre(Format('%s=%d',[rsMETAFILTRE_CODE, FStationNearToMouse.eCode]));
end;

procedure TGHTopoContext2DA.acMetaFiltreEntreeExecute(Sender: TObject);
begin
  AppliquerLeFiltre(Format('%s=%d',[rsMETAFILTRE_ENTREE_RATT, FStationNearToMouse.eEntrance]));
end;
procedure TGHTopoContext2DA.acMetaFiltreExpeExecute(Sender: TObject);
begin
  AppliquerLeFiltre(Format('%s=%d',[rsMETAFILTRE_EXPE, FStationNearToMouse.eExpe]));
end;

procedure TGHTopoContext2DA.acMetaFiltreNamespaceExecute(Sender: TObject);
var
  QIdxNamespace, QNoSerie: integer;
begin
  DecomposeNumeroSerie(FStationNearToMouse.Entite_Serie, QIdxNamespace, QNoSerie);
  AppliquerLeFiltre(Format('%s=%d',[rsMETAFILTRE_NAMESPACE, QIdxNamespace]));
end;

procedure TGHTopoContext2DA.acMetaFiltreReseauExecute(Sender: TObject);
begin
  AppliquerLeFiltre(Format('%s=%d',[rsMETAFILTRE_RESEAU, FStationNearToMouse.eReseau]));
end;



procedure TGHTopoContext2DA.acMetaFiltreSecteurExecute(Sender: TObject);
begin
  AppliquerLeFiltre(Format('%s=%d',[rsMETAFILTRE_SECTEUR, FStationNearToMouse.eSecteur]));
end;

procedure TGHTopoContext2DA.AppliquerLeFiltre(const FF: string);
begin
  ApplyMetaFiltre(True, FF);
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.DisplayFullInfosStation(const S: TBaseStation);
var
  RS: TReseau;
  SC: TSecteur;
  QAltitudeEpicentre: double;
  L, A, P, G,D,H,B, dx, dy, dz: Double;
  EWE: TExpe;
  beuh: TPoint3Df;
  CDS: TCode;
  QIdxSerie: Integer;
  MySerie: TObjSerie;
  WU: String;
  QEntree: TEntrance;
begin
  +++
  dx := S.PosStation.X - S.PosExtr0.X;
  dy := S.PosStation.Y - S.PosExtr0.Y;
  dz := S.PosStation.Z - S.PosExtr0.Z;
  L := 0.00; A := 0.00; P := 0.00;
  GetBearingInc(dx, dy, dz, L,A,P, 360.0,360.0);
  lbInfos.Caption:=Format(rsVUE2D_FMT_INFOS_STATION_RAPIDE,
                            [S.IDTerrain,
                             S.Entite_Serie,
                             S.Entite_Station,
                             S.PosStation.X,
                             S.PosStation.Y,
                             S.PosStation.Z]);
  lbNumSerie.Caption         := IntToStr(S.Entite_Serie);
  lbNumStation.Caption       := IntToStr(S.Entite_Station);
  lbIDTerrainStation.Caption := S.IDTerrain;
  if (S.Type_Entite in [tgENTRANCE, tgFIXPOINT]) then
  begin
    lbInfosExpe.Caption     := '';
    lbMesures.Caption       := '';
    lbCouleurVisee.Color    := S.CouleurStd;
    lbColorReseau.Color     := clGray;
    lbColorReseau.Caption   := '';
    lbNomReseau.Caption     := '';
    lbColorSecteur.Color    := clGray;
    lbColorSecteur.Caption  := '';
    lbNomSecteur.Caption    := '';
    lbColorSeance.Color     := clGray;
    lbColorSeance.Caption   := '';
    lbCode.Caption          := '';
  end
  else
  begin
    G := Hypot2D((S.PosPG.X - S.PosStation.X), (S.PosPG.Y - S.PosStation.Y));
    D := Hypot2D((S.PosPD.X - S.PosStation.X), (S.PosPD.Y - S.PosStation.Y));
    H := S.PosPD.Z - S.PosStation.Z;
    B := S.PosStation.Z - S.PosPG.Z;

    RS  := FBDDEntites.GetReseau(S.eReseau);
    SC  := FBDDEntites.GetSecteur(S.eSecteur);
    EWE := FBDDEntites.GetExpeByIndex(S.eExpe);
    CDS := FBDDEntites.GetCodeByIndex(S.eCode);
    lbCouleurVisee.Color    := S.CouleurStd;
    lbColorReseau.Color     := RS.ColorReseau;
    lbColorReseau.Caption   := Format(FORMAT_NB_INTEGER,[S.eReseau]);
    lbNomReseau.Caption     := _AnsiToLCLStr(RS.NomReseau);
    lbColorSecteur.Color    := SC.CouleurSecteur;
    lbColorSecteur.Caption  := Format(FORMAT_NB_INTEGER,[S.eSecteur]);
    lbNomSecteur.Caption    := _AnsiToLCLStr(SC.NomSecteur);
    lbColorSeance.Color     := FBDDEntites.GetCouleurEntiteRGBByExpe(S);
    lbColorSeance.Caption   := Format(FORMAT_NB_INTEGER,[S.eExpe]);
    // infos sur le code
    lbCode.Caption          := MakeLibelleCode(CDS);//Format('%d - %.0f; %.0f', [CDS.IDCode, CDS.GradAz, CDS.GradInc]);
    // infos sur l'expé
    {$WARNING: TEXpe.DateExpe à implementer}
    WU := DateToStr(GetSecuredDate(EWE.AnneeExpe, EWE.MoisExpe, EWE.JourExpe));
    lbInfosExpe.Caption     := MakeLibelleExpe(EWE); //Format('Expe %d: %s (%s, %s)', [EWE.IDExpe, WU, EWE.Operateur, EWE.ClubSpeleo]);
    lbMesures.Caption       := Format(rsVUE2D_FMT_INFOS_MESURES, [L, A, P, G,D,H,B]);
    // infos sur la série
    if (S.Type_Entite = tgVISEE_RADIANTE) then
    begin
      lbSerie.Caption := Format('Antenne #%d', [S.Entite_Serie]);
    end
    else
    begin
      QIdxSerie := FDocuTopo.GetIdxSerieByNumero(S.Entite_Serie);
      if (QIdxSerie > 0)  then
      begin
        MySerie := FDocuTopo.GetSerie(QIdxSerie);
        lbSerie.Caption := Format('Serie %d - %s', [MySerie.GetNumeroDeSerie, _AnsiToLCLStr(MySerie.GetNomSerie)]);
      end;

    end;
  end;
  // coordonnées
  beuh := FBDDEntites.GetDeltaXYZFromPositionSt0(S);
  lbCoordonnees.Caption := Format(rsVUE2D_FMT_INFOS_COORDONNEES, [
                                 FormatterNombreAvecSepMilliers(S.PosStation.X),
                                 FormatterNombreAvecSepMilliers(S.PosStation.Y),
                                 FormatterNombreAvecSepMilliers(S.PosStation.Z),
                                 FormatterNombreAvecSepMilliers(beuh.X),
                                 FormatterNombreAvecSepMilliers(beuh.Y),
                                 FormatterNombreAvecSepMilliers(beuh.Z)
                                 ]);
  // entrée de rattachement
  QEntree := FBDDEntites.GetEntrance(S.eEntrance);
  lbEntreeRattachement.Caption := format('%d - %s', [S.eEntrance, QEntree.eNomEntree]);
  // distances au versant si maillage OK
  if (FMyMaillage.IsValidMaillage()) then
  begin
    if (FMyMaillage.CalcAltitudeMaillageAtPoint(S.PosStation.X, S.PosStation.Y, QAltitudeEpicentre)) then
      lbMiscMaillage.Caption := format('Altitude surface à l''aplomb: %.2f - Dist: %.2f m',
                                       [QAltitudeEpicentre, QAltitudeEpicentre - S.PosStation.Z])
    else
      lbMiscMaillage.Caption := 'Station hors maillage';
  end
  else
  begin
    lbMiscMaillage.Caption := 'Aucun maillage valide';
  end;
end;

procedure TGHTopoContext2DA.DisplayQuickAddVisee(const OS: TObjSerie);
begin
  pass;
end;
//******************************************************************************
procedure TGHTopoContext2DA.VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  FMT_STATION_1 = '%d.%d - X = %.3f, Y = %.3f, Z = %.3f';
var
  // pointer la station la plus proche
  QDist, QDistance      : double;
  QQ1, QQ2              , QIsStation: Boolean;
  QOffset, qC1, qC2, qD : TPoint2Df;
  ST1, ST2              : TBaseStation;
  MyAnnotation          : TKrobardAnnotation;
  DbgTag, WU            : String;
  MySerieARaccorder     : TObjSerie;
  QIdx, EWE, QLastIdxPolyline: integer;
  QEntrance1            , QEntrance2: TEntrance;
  QE: TBDDEntitesFindViseeOrEntrance;
  procedure ReinitFZC(const DoFC1, DoFC2: boolean);
  begin
    if (DoFC1) then
    begin
      FZC1 := FMyPos;
      FZP1 := MakeTPoint(X, Y);
    end;
    if (DoFC2) then
    begin
      FZC2 := FMyPos;
      FZP2 := MakeTPoint(X, Y);
    end;
  end;
begin
  if (not FCanDraw) then Exit;
  if (not FBDDEntites.GetStationOrEntranceFromXYZ(FMyPos.X, FMyPos.Y, 0.0, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, FStationNearToMouse, QEntrance1, QDistance, QE)) then Exit;
  SetStationInfo(FStationNearToMouse);
  acSetCurrentBaseStation.Caption := Format('Définir %d.%d comme station courante', [FStationNearToMouse.Entite_Serie, FStationNearToMouse.Entite_Station]);
  if (ssShift in Shift) then
  begin
    FBDDEntites.HighLightVisees(mslSERIE, FStationNearToMouse.Entite_Serie);
    self.RefreshDessin();
    exit;
  end;
  if (pnlFullInfos.Visible) then DisplayFullInfosStation(FStationNearToMouse);
  case FModesTravail of
    mtREADY:
      begin
        pass;
      end;
    mtPAN_PREMIER_POINT:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtPAN_SECOND_POINT);
      end;
    mtPAN_SECOND_POINT:
      begin
        ReinitFZC(False, True);
        QOffset := MakeTPoint2Df(FZC2.X - FZC1.X, FZC2.Y - FZC1.Y);
        DeplacerVue(QOffset.X, QOffset.Y);
        self.RefreshDessin();
        SetModeTravail(mtREADY);
      end;
    mtZOOM_PREMIER_COIN:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtZOOM_SECOND_COIN);
      end;
    mtZOOM_SECOND_COIN:
      begin
        ReinitFZC(False, True);
        DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
        SetViewLimits(FZC1.X, FZC1.Y, FZC2.X, FZC2.Y, DbgTag);
        self.RefreshDessin();
        SetModeTravail(mtREADY);
      end;
    mtMETAFILTRE_ZONE_PREMIER_COIN:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtMETAFILTRE_ZONE_SECOND_COIN);
      end;
    mtMETAFILTRE_ZONE_SECOND_COIN:
      begin
        ReinitFZC(False, True);
        qC1 := MakeTPoint2Df(FZC1.X, FZC1.Y);
        qC2 := MakeTPoint2Df(FZC2.X, FZC2.Y);
        qD  := MakeTPoint2Df(qC2.X - qC1.X, qc2.Y - qC1.Y);
        // si zone trop étroite, abandonner
        if (Abs(qD.X) < 1.00) or (Abs(qD.Y) < 1.00) then Exit;
        // échanger de manière à rendre indifférent le sens du rectangle de sélection
        if (qC2.X < qC1.X) then Swap(qC1.X, qC2.X);
        if (qC2.Y < qC1.Y) then Swap(qC1.Y, qC2.y);
        AppliquerLeFiltre(Format('COORD_X=(%.2f, %.2f) & COORD_Y=(%.2f, %.2f)', [qC1.X, qC2.X, qC1.Y, qC2.Y]));
        SetModeTravail(mtREADY);
      end;
    mtDISTANCE_PREMIER_POINT:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtDISTANCE_SECOND_POINT);
      end;
    mtDISTANCE_SECOND_POINT:
      begin
        ReinitFZC(False, True);
        SetModeTravail(mtREADY);
        QQ1 := FBDDEntites.GetStationOrEntranceFromXYZ(FZC1.X, FZC1.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST1, QEntrance1, QDistance, QE);
        QQ2 := FBDDEntites.GetStationOrEntranceFromXYZ(FZC2.X, FZC2.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST2, QEntrance2, QDistance, QE);
        if (QQ1 and QQ2) then
        begin
          DispDistanceEntreStations(FDocuTopo, FBDDEntites, ST1, ST2, 'Distance entre deux points topo', '');
          self.RefreshDessin();
        end;
      end;
    mtADD_SERIE_PREMIER_POINT:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtADD_SERIE_SECOND_POINT);
      end;
    mtADD_SERIE_SECOND_POINT:
      begin
        ReinitFZC(False, True);
        SetModeTravail(mtREADY);
        QQ1 := FBDDEntites.GetStationOrEntranceFromXYZ(FZC1.X, FZC1.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST1, QEntrance1, QDistance, QE);
        QQ2 := FBDDEntites.GetStationOrEntranceFromXYZ(FZC2.X, FZC2.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST2, QEntrance2, QDistance, QE);
        if (QQ1 and QQ2) then
        begin
          //ShowMessage('Insérer une série');
          //AfficherDistanceEntreStations(FBDDEntites, ST1, ST2);
        end;
      end;
    mtBOUCLER_PREMIER_POINT: // Raccordement graphique du terminus d'un cheminement sur une station d'un autre cheminement
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtBOUCLER_SECOND_POINT);
      end;
    mtBOUCLER_SECOND_POINT:
      begin
        ReinitFZC(False, True);
        SetModeTravail(mtREADY);
        QQ1 := FBDDEntites.GetStationOrEntranceFromXYZ(FZC1.X, FZC1.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST1, QEntrance1, QDistance, QE);
        QQ2 := FBDDEntites.GetStationOrEntranceFromXYZ(FZC2.X, FZC2.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST2, QEntrance2, QDistance, QE);
        if (QQ1 and QQ2) then
        begin
          EWE := DispDistanceEntreStations(self.FDocuTopo, self.FBDDEntites, ST1, ST2, 'Mise en place d''un bouclage', 'Recalculer le réseau');
          if ((EWE > 0) AND FDocuTopo.GetSerieByNumeroSerie(ST1.Entite_Serie, MySerieARaccorder, QIdx)) then
          begin
            QDist := Hypot3D(ST2.PosStation.X - ST1.PosStation.X,
                             ST2.PosStation.Y - ST1.PosStation.Y,
                             ST2.PosStation.Z - ST1.PosStation.Z);
            WU    := 'Accrocher le point:' + #10 +
                     Format(FMT_STATION_1, [ST1.Entite_Serie, ST1.Entite_Station, ST1.PosStation.X, ST1.PosStation.Y, ST1.PosStation.Z]) + #10 +
                     'Au point:' + #10 +
                     Format(FMT_STATION_1, [ST2.Entite_Serie, ST2.Entite_Station, ST2.PosStation.X, ST2.PosStation.Y, ST2.PosStation.Z]) + #10 +
                     Format('Distance: %.3f m', [QDist]);
            if (GHTopoQuestionOuiNon(WU)) then
            begin
              MySerieARaccorder.SetSeriePtArr(ST2.Entite_Serie, ST2.Entite_Station);
              if ((EWE = 2) and (GHTopoQuestionOuiNon('Le réseau va être recalculé'))) then
              begin
                if (assigned(FProcPerformActionRecalcul)) then FProcPerformActionRecalcul(False);
              end;
            end;
          end;
        end;
      end;
    // Extraction d'un profil MNT
    mtPROFIL_MNT_PREMIER_POINT:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtPROFIL_MNT_SECOND_POINT);
      end;
    mtPROFIL_MNT_SECOND_POINT:
      begin
        ReinitFZC(False, True);
        SetModeTravail(mtREADY);
        QQ1 := FBDDEntites.GetStationOrEntranceFromXYZ(FZC1.X, FZC1.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST1, QEntrance1, QDistance, QE);
        QQ2 := FBDDEntites.GetStationOrEntranceFromXYZ(FZC2.X, FZC2.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST2, QEntrance2, QDistance, QE);
        if (QQ1 and QQ2) then
        begin
          FMyMaillage.ExtractAndAddProfilTopo(FZC1, FZC2);
          self.RefreshDessin();
        end;
      end;
    mtNEW_ANNOTATION:
      begin
        if (not FCroquisTerrain.IsReady) then exit;
        QQ1 := FBDDEntites.GetStationOrEntranceFromXYZ(FMyPos.X, FMyPos.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, FCurrentInternalIdxEntite, ST1, QEntrance1, QDistance, QE);
        MyAnnotation.IDStyle := FCroquisTerrain.CurrentIdxStyleAnnotation;
        MyAnnotation.Position.IDBaseStation := MakeTIDBaseStation(ST1.Entite_Serie, ST1.Entite_Station, false);
        MyAnnotation.Position.Offset := MakeTPoint3Df(FMyPos.X - ST1.PosStation.X, FMyPos.Y - ST1.PosStation.Y, ST1.PosStation.Z);
        MyAnnotation.Texte  := '';
        if (EditerAnnotation(FCroquisTerrain, 0, MyAnnotation)) then
        begin
          SetModeTravail(mtREADY);
          FCroquisTerrain.AddAnnotation(MyAnnotation);
          MyAnnotation := FCroquisTerrain.GetAnnotation(FCroquisTerrain.GetNbAnnotations() - 1);
          self.RefreshDessin();
        end;
      end;
    mtNEW_POLYLINE:
      begin
        if (not FCroquisTerrain.IsReady) then exit;
        QLastIdxPolyline := FCroquisTerrain.FindIdxPolylineWithExtr2NearToXY(FMyPos.X, FMyPos.Y, 0.50);
        FCroquisTerrain.BeginPolyline(MakeTPoint3Df(FMyPos.X, FMyPos.Y, 0.00), QLastIdxPolyline);
      end;
    mtDELETE_ANNOTATION:
      begin
        if (FCroquisTerrain.TryDeleteAnnnotationNearToXY(FMyPos)) then self.RefreshDessin();
      end;
    mtDELETE_POLYLINE:
      begin
        if (FCroquisTerrain.TryDeletePolylineNearToXY(FMyPos)) then self.RefreshDessin();
      end;
    else
      pass;
  end;
end;

procedure TGHTopoContext2DA.VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PP: TPoint;
  QDistanceXYZ: Double;
  QDistance, QAzimut: double;
  QNb, QCurrentInternalIdxEntite, wwt, hht, LT: Integer;
  QIsStation: Boolean;
  QEntrance1: TEntrance;

  QEtiquetteOk   : boolean;
  QEtiquetteRect1: TRect;
  QEtiquetteRect2: TRect;
  QStationNearToMouse: TBaseStation;
  EWE: String;
  QE: TBDDEntitesFindViseeOrEntrance;
  procedure DrawFantomePolyline();
  var
    i, Nb: Integer;
    QPM: TPoint3Df;
    QPP: TPoint;
  begin
    Vue.Canvas.Pen.Color := clGray;
    Vue.Canvas.Pen.Width := 1;
    if (not FCroquisTerrain.IsReady) then exit;
    Nb := FCroquisTerrain.GetNbBufferVertexes();
    if (Nb < 2) then Exit;
    QPM := FCroquisTerrain.GetBufferVertex(0);
    QPP := GetCoordsPlan(QPM.X, QPM.Y);
    Vue.Canvas.MoveTo(QPP);
    for i := 1 to Nb - 1 do
    begin
      QPM := FCroquisTerrain.GetBufferVertex(i);
      QPP := GetCoordsPlan(QPM.X, QPM.Y);
      Vue.Canvas.LineTo(QPP);
    end;
  end;
  procedure DrawFantomeLasso();
  var
    i, Nb: Integer;
    QPP: TPoint;
  begin
    Vue.Canvas.Pen.Color := clSilver;
    Vue.Canvas.Pen.Width := 1;
    Nb := length(FLassoDeSelection);
    if (Nb < 3) then Exit;
    QPP := GetCoordsPlan(FLassoDeSelection[0].X, FLassoDeSelection[0].Y);
    Vue.Canvas.MoveTo(QPP);
    for i := 1 to Nb - 1 do
    begin
      QPP := GetCoordsPlan(FLassoDeSelection[i].X, FLassoDeSelection[i].Y);
      Vue.Canvas.LineTo(QPP);
    end;
    QPP := GetCoordsPlan(FLassoDeSelection[0].X, FLassoDeSelection[0].Y);
    Vue.Canvas.LineTo(QPP);
  end;

  procedure DrawLigne(const P1, P2: TPoint); inline;
  begin
    Vue.Canvas.Line(P1.X,P1.Y, P2.X, P2.Y);
  end;
  procedure DrawRectangle(const P1, P2: TPoint);
  begin
    Vue.Canvas.Brush.Style := bsClear;
    Vue.Canvas.Rectangle(P1.X, P1.Y, P2.X, P2.Y);
  end;
  procedure DrawRectangleBarre(const P1, P2: TPoint);
  begin
    Vue.Canvas.Brush.Style := bsClear;
    Vue.Canvas.Rectangle(P1.X, P1.Y, P2.X, P2.Y);
    Vue.Canvas.Line(P1.X, P1.Y, P2.X, P2.Y);
    Vue.Canvas.Line(P1.X, P2.Y, P2.X, P1.Y);
  end;

  // Code inactivé à conserver. Peut être réutilisé
  (*
  procedure DrawPolygonaleRapide();
  var
    QW, QH: Double;
    i, Nb: integer;
    QPP1, QPP2: TPoint;
    MyEntite: TBaseStation;

    QLimitesVue: TRect2Df;
    FP1, FP2: TPoint2Df;
    qdx, qdy: LongInt;
  begin
    Nb := FBDDEntites.GetNbEntitesVisees();
    // lenteurs constatées sur certains postes => au delà d'un certain seuil, on ne trace rien
    if (Nb > 40000) then Exit;
    // pour éviter de voir la silhouette fantome tronquée, on spécifie une marge
    // autour du rectangle de vue initial égale au déplacement maximal possible de la vue
    // ie: marges horizontales = largeur de la fenêtre vue initiale
    //     marges verticales   = hauteur de la fenêtre vue initiale
    QW := FRegionCMaxi.X - FRegionCMini.X;
    QH := FRegionCMaxi.Y - FRegionCMini.Y;
    QLimitesVue := MakeTRect2Df(FRegionCMini.X - QW, FRegionCMini.Y - QH,
                                FRegionCMaxi.X + QW, FRegionCMaxi.Y + QH);
    for i := 0 to Nb - 1 do
    begin
      MyEntite := FBDDEntites.GetEntiteVisee(i);
      if (Not (MyEntite.Type_Entite in [tgVISEE_RADIANTE, tgENTRANCE, tgFIXPOINT])) then
      begin
        FP1 := MakeTPoint2Df(MyEntite.PosExtr0.X, MyEntite.PosExtr0.Y);
        FP2 := MakeTPoint2Df(MyEntite.PosStation.X, MyEntite.PosStation.Y);
        if (SegmentInRectangle(FP1, FP2, QLimitesVue)) then
        begin
          QPP1 := GetCoordsPlan(FP1);
          QPP2 := GetCoordsPlan(FP2);
          //DrawLigne(QPP1, QPP2);
          qdx := FZP2.X - FZP1.X;
          qdy := FZP2.Y - FZP1.Y;
          QPP1.X := QPP1.X + qdx;
          QPP1.Y := QPP1.Y + qdy;
          QPP2.X := QPP2.X + qdx;
          QPP2.Y := QPP2.Y + qdy;
          DrawLigne(QPP1, QPP2);
        end;
      end;
    end;
  end;
  //*)
  procedure DrawShapeElastique(const QX, QY: integer; const SType: byte; const DoDrawSilhouetteFantome: boolean = false);
    procedure CuiCui();
    begin
      case SType of
        1: DrawLigne(FZP1, FZP2); //if (DoDrawSilhouetteFantome) then DrawPolygonaleRapide();
        2: DrawRectangle(FZP1, FZP2);
        3: DrawRectangleBarre(FZP1, FZP2);
      end;
    end;
  begin
    CuiCui();
    FZP2 := MakeTPoint(QX, QY);
    CuiCui();
  end;
  (*
  procedure QDrawEtiquette(const LBL: string);
  var
    TxtExtent: TSize;
  begin
    if (not FOverlayed) then
    begin
      if (QEtiquetteOk) then
      begin
        if (FEtiquetteOnSurvolPlan_First) then
        begin
          QEtiquetteRect1 := Rect(0, 0, FEtiquetteOnSurvolPlan_ow, FEtiquetteOnSurvolPlan_oh);
          QEtiquetteRect2 := Rect(FEtiquetteOnSurvolPlan_ox                            , FEtiquetteOnSurvolPlan_oy - FEtiquetteOnSurvolPlan_oh,
                                  FEtiquetteOnSurvolPlan_ox + FEtiquetteOnSurvolPlan_ow, FEtiquetteOnSurvolPlan_oy);
          Vue.Canvas.CopyRect(QEtiquetteRect2, FEtiquetteOnSurvolPlan_Tampon.Canvas, QEtiquetteRect1);
        end;
        {on récupère les dimensions du rectangle de texte}
        //case QE of
        //  tpVISEES   : EWE := Format(FMTSERST, [QStationNearToMouse.Entite_Serie, QStationNearToMouse.Entite_Station]);
        //  tpENTRANCES: EWE := Format(FMTSERST, [QEntrance1.eRefSer, QEntrance1.eRefSt]);  // Bug avec TextWidth au-delà d'un certain nombre de caractères
        //end;
        TxtExtent := FEtiquetteOnSurvolPlan_Tampon.Canvas.TextExtent(LBL);
        wwt := TxtExtent.cx + 8; //FEtiquetteOnSurvolPlan_Tampon.Canvas.TextWidth(LBL) + 8;
        hht := TxtExtent.cy; //FEtiquetteOnSurvolPlan_Tampon.Canvas.TextHeight(LBL);
        {mémorise ce que va écraser le rectangle}
        QEtiquetteRect1 := Rect(FEtiquetteOnSurvolPlan_X1, FEtiquetteOnSurvolPlan_Y1 - hht, FEtiquetteOnSurvolPlan_X1 + wwt, FEtiquetteOnSurvolPlan_Y1);
        QEtiquetteRect2 := Rect(0, 0, wwt, hht);
        FEtiquetteOnSurvolPlan_Tampon.Canvas.CopyRect(QEtiquetteRect2, Vue.Canvas, QEtiquetteRect1);
        Vue.Canvas.Pen.Color     := clYellow;
        Vue.Canvas.brush.style   := bsSolid;
        //case QE of
        //  tpVISEES   : Vue.Canvas.brush.color   := clCream;
        //  tpENTRANCES: Vue.Canvas.brush.color   := clLime;
        //end;
        Vue.Canvas.brush.color   := clCream;
        Vue.Canvas.Rectangle(QEtiquetteRect1);
        //Vue.Canvas.brush.style   := bsClear;
        Vue.Canvas.TextOut(FEtiquetteOnSurvolPlan_x1, FEtiquetteOnSurvolPlan_y1 - hht, LBL);

        {mémorise les coordonnées de ce rectangle}
        FEtiquetteOnSurvolPlan_ox := FEtiquetteOnSurvolPlan_x1;
        FEtiquetteOnSurvolPlan_oy := FEtiquetteOnSurvolPlan_y1;
        FEtiquetteOnSurvolPlan_ow := wwt;
        FEtiquetteOnSurvolPlan_oh := hht;
        FEtiquetteOnSurvolPlan_First := true;
      end; // if (QEtiquetteOk) then
    end; // if (not FOverlayed) then
  end;
  //*)
begin
  QDistanceXYZ := 0.00;
  QAzimut := 0.00;
  if (not FCanDraw) then Exit;
  PP := MakeTPoint(X, Y);
  FMyPos   := GetCoordsMonde(PP);

  //FEtiquetteOnSurvolPlan_X1 := PP.X;
  //FEtiquetteOnSurvolPlan_Y1 := PP.Y;
  QEtiquetteOk := (FBDDEntites.GetStationOrEntranceFromXYZ(FMyPos.X, FMyPos.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], false, QCurrentInternalIdxEntite, QStationNearToMouse, QEntrance1, QDistanceXYZ, QE));
  lbGCSMouse.Caption := Format('[%s], %s, %s',[Format(FMTSERST, [QStationNearToMouse.Entite_Serie, QStationNearToMouse.Entite_Station]),
                                             FormatterNombreAvecSepMilliers(FMyPos.X, 0), FormatterNombreAvecSepMilliers(FMyPos.Y, 0)]);
  //if (FModesTravail = mtDISTANCE_SECOND_POINT) then
  //  QDrawEtiquette(Format('Dist: %.3f m', [QDistanceXYZ]))
  //else
  //QDrawEtiquette(Format(FMTSERST, [QStationNearToMouse.Entite_Serie, QStationNearToMouse.Entite_Station]));


  Vue.Canvas.Pen.Color := clSilver;
  Vue.Canvas.Pen.Width := 0;
  case FModesTravail of
    mtREADY:
      begin
        FZP1 := MakeTPoint(X, Y);
        FZP2 := MakeTPoint(X, Y);
      end;
    mtPAN_SECOND_POINT           : DrawShapeElastique(X, Y, 1, True);
    mtBOUCLER_SECOND_POINT       : DrawShapeElastique(X, Y, 1);
    mtDISTANCE_SECOND_POINT      :
      begin
        DrawShapeElastique(X, Y, 1);
        QDistance := hypot(FMyPos.X - FZC1.X, FMyPos.Y - FZC1.Y);
        QAzimut   := GetAzimut(FMyPos.X - FZC1.X, FMyPos.Y - FZC1.Y, DEGRES_PAR_TOUR);
        lbAdditionalInfos.Caption := Format('%.2f m < %.2f°', [QDistance, QAzimut]);
      end;
    mtADD_SERIE_SECOND_POINT      : DrawShapeElastique(X, Y, 1);
    mtZOOM_SECOND_COIN            : DrawShapeElastique(X, Y, 2);
    mtMETAFILTRE_ZONE_SECOND_COIN : DrawShapeElastique(X, Y, 3);
    mtPROFIL_MNT_SECOND_POINT     : DrawShapeElastique(X, Y, 1);
    mtNEW_POLYLINE:
      begin
        if (not FCroquisTerrain.IsReady) then exit;
        if (ssLeft in Shift) then
        begin
          FCroquisTerrain.AddVertexAtBuffer(FMyPos.X, FMyPos.Y);
          DrawFantomePolyline();
        end;
      end;
    mtNEW_LASSO_SELECTION:
      begin
        if (ssLeft in Shift) then
        begin
          QNb := Length(FLassoDeSelection) + 1;
          SetLength(FLassoDeSelection, QNb);
          FLassoDeSelection[QNb - 1].X := FMyPos.X;
          FLassoDeSelection[QNb - 1].Y := FMyPos.Y;
          DrawFantomeLasso();
        end;
      end
  else
    pass;
  end;
end;

procedure TGHTopoContext2DA.VueMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case FModesTravail of
    mtNEW_POLYLINE:
      begin
        if (not FCroquisTerrain.isReady) then exit;
        FCroquisTerrain.EndPolyline();
        SetModeTravail(mtNEW_POLYLINE); // on est pret pour une autre polyligne
        self.RefreshDessin();
      end;
    mtNEW_LASSO_SELECTION:
      begin
        SetModeTravail(mtREADY);
      end
  else
    pass;
  end;
end;

// En Lazarus, cet événement n'est pas traité
procedure TGHTopoContext2DA.VueMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
const
  Q_WHEEL_VERTICAL   = 0;
  Q_WHEEL_HORIZONTAL = 1;
  Q_WHEEL_ZOOM       = 2;
var
  QOnglet            : TVue2DParams;
  QModeDeplVue       : byte;
  QZoom, Qdx, Qdy, QR: double;
  DbgTag             : String;
begin
  // Valeur de WheelDelta retournée: = 120 ; négatif en tirant, positif en poussant
  // Utilisation des touches: convention Inkscape
  // Shift: []         = zoom
  //        [ssShift]  = pan horizontal
  //        [ssCtrl]   = pan vertical
  //QModeStr := 'Pan vertical';
  AfficherMessage('Wheel: ' + inttostr(WheelDelta));
  if      (ssShift in Shift) then QModeDeplVue := Q_WHEEL_HORIZONTAL  // pan H
  else if (ssCtrl  in Shift) then QModeDeplVue := Q_WHEEL_VERTICAL    // pan V
  else                            QModeDeplVue := Q_WHEEL_ZOOM;       // zoom
  QOnglet := FArrOngletsParams[FCurrIdxOnglet];
  QZoom := 1.00;
  Qdx   := 0.00;
  Qdy   := 0.00;
  QR    := Hypot2D(QOnglet.ongC2.X - QOnglet.ongC1.X, QOnglet.ongC2.Y - QOnglet.ongC1.Y);
  case QModeDeplVue of
    Q_WHEEL_VERTICAL:
    begin
      Qdy := 0.05 * QR * IIF(WheelDelta < 0, -1.0, 1.0);
      DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
      SetViewLimits(QOnglet.ongC1.X, QOnglet.ongC1.Y + Qdy,
                    QOnglet.ongC2.X, QOnglet.ongC2.Y + Qdy,
                    DbgTag);
    end;
    Q_WHEEL_HORIZONTAL:
    begin
      Qdx := 0.05 * QR * IIF(WheelDelta < 0, -1.0, 1.0);
      DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
      SetViewLimits(QOnglet.ongC1.X + Qdx, QOnglet.ongC1.Y,
                    QOnglet.ongC2.X + Qdx, QOnglet.ongC2.Y,
                    DbgTag);
    end;
    Q_WHEEL_ZOOM:
    begin
      QZoom := 1 + 0.05 * IIF(WheelDelta < 0, -1.0, 1.0);
      ZoomFact(QZoom);
    end;
  end;
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.VuePaint(Sender: TObject);
begin
  RedessinEcran(Vue.Width, Vue.Height, False);
end;

// cette fonction retourne d'autres paramètres
function TGHTopoContext2DA.GetRYMaxi(): double;
var
  qdx: Double;
begin
  qdx := FRegionCMaxi.X - FRegionCMini.X;
  FRappHLVue      := Vue.Height / Vue.Width;            // rapport Hauteur/largeur de vue
  FRappScrReal    := Vue.Width / qdx;                   // rapport Ecran/Réel
  FInvRappScrReal := 1 / FRappScrReal;
  Result          := FRegionCMini.Y + qdx * FRappHLVue; // hauteur de visualisation
end;

//******************************************************************************
// lbFiltres
procedure TGHTopoContext2DA.ApplyMetaFiltre(const B: boolean; const QFiltre: string);
var
  FF: String;
  QDevelViseesVisibles: double;
  EWE: TCaption;
  QColor: TColor;
begin
  QDevelViseesVisibles := 0.00;
  Application.ProcessMessages;
  try
    EWE    := lbInfos.Caption;
    QColor := lbInfos.Color;
    lbInfos.Caption := 'Calcul du filtre en cours';
    lbInfos.Color   := clYellow;
    Application.ProcessMessages;
    AfficherMessage(Format('%s.ApplyMetaFiltre()', [ClassName]));
    FArrOngletsParams[FCurrIdxOnglet].ongVueFiltres := QFiltre;
    FF := IIF(B, QFiltre, '');

    FBDDEntites.MetaFiltre(FF, QDevelViseesVisibles);     // traitements
    lbLongViseesVisibles.Caption := Format(FORMAT_NB_REAL_0_DEC, [QDevelViseesVisibles]);
    FBDDEntites.SetMinMax(False);   // reset Mini et Maxi sans utilisation du filtre
    self.RefreshDessin();
    editMetaFiltre.Text := QFiltre; // restauration du filtre
  finally
    lbinfos.Caption := EWE;
    lbInfos.Color   := QColor;
  end;
end;

procedure TGHTopoContext2DA.btnApplyMetaFiltreClick(Sender: TObject);
begin
  ApplyMetaFiltre(chkMetaFiltreActif.Checked, trim(editMetaFiltre.Text));
end;


procedure TGHTopoContext2DA.btnCopyCoordsClick(Sender: TObject);
var
  CP: TClipboard;
begin
  CP := TClipboard.Create();
  try
    CP.AsText := lbCoordonnees.Caption;
  finally
    FreeAndNil(CP);//CP.Free;
  end;
end;

procedure TGHTopoContext2DA.btnDefinirCommeActiveClick(Sender: TObject);
begin
  SetCurrentStation(FStationNearToMouse, True);
end;

procedure TGHTopoContext2DA.btnHelpMetaFiltreClick(Sender: TObject);
begin
  //DisplayHelpSystem('METAFILTRE');
  DisplayToast('toto');
end;

procedure TGHTopoContext2DA.btnListeProfilsClick(Sender: TObject);
begin
  DisplayListeProfils(FDocuTopo, FMyMaillage);
  self.RefreshDessin();
end;

//******************************************************************************
// gestion des onglets
procedure TGHTopoContext2DA.PutOngletByIndex(const Idx: integer; const O: TVue2DParams; const DoRedrawVue: boolean);
var
  DbgTag: String;
begin
  FArrOngletsParams[Idx] := O;
  editMetaFiltre.Text := O.ongVueFiltres;
  DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
  if (DoRedrawVue) then  // acquittement par redessin
  begin
    FBDDEntites.CalcCouleursByDepth(O.ongDegradeAltMini, O.ongDegradeAltMaxi);
    SetViewLimits(O.ongC1, O.ongC2, DbgTag);
    self.RefreshDessin();
  end;
end;

procedure TGHTopoContext2DA.SetHighLightCurrentItem(const B: boolean);
begin
  if (FDoHighLightCurrentItem <> B) then
  begin
    FDoHighLightCurrentItem := B;
    self.RefreshDessin();
  end;
end;

procedure TGHTopoContext2DA.SetCurrentIdxOnglet(const Idx: byte);
var
  FCurrentOnglet: TVue2DParams;
  DbgTag: String;
begin
  {$IFNDEF GHTOPO_SIMPLIFIE}
  FCurrIdxOnglet := Idx;
  {$ENDIF GHTOPO_SIMPLIFIE}
  FCurrentOnglet := FArrOngletsParams[FCurrIdxOnglet];
  // mise à jour du champ lbFiltres
  AppliquerLeFiltre(FArrOngletsParams[FCurrIdxOnglet].ongVueFiltres);
  // recadrage
  DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
  SetViewLimits(FCurrentOnglet.ongC1, FCurrentOnglet.ongC2, DbgTag);
  // recalculer les dégradés
  FBDDEntites.CalcCouleursByDepth(FCurrentOnglet.ongDegradeAltMini, FCurrentOnglet.ongDegradeAltMaxi);
  self.RefreshDessin();
end;

function TGHTopoContext2DA.GetNbOngletsVues(): integer;
begin
  Result := 1 + High(FArrOngletsParams);
end;

function TGHTopoContext2DA.GetCurrentOngletIdx(): integer;
begin
  Result := FCurrIdxOnglet;
end;

function TGHTopoContext2DA.GetOngletByIndex(const Idx: integer): TVue2DParams;
begin
  Result := FArrOngletsParams[Idx];
end;

function TGHTopoContext2DA.GetCurrentOnglet(): TVue2DParams;
begin
  Result := FArrOngletsParams[FCurrIdxOnglet];
end;

procedure TGHTopoContext2DA.SetNomOnglet(const S: string);
begin
  FArrOngletsParams[FCurrIdxOnglet].ongName := S;
end;

procedure TGHTopoContext2DA.SetProcUseMetaFiltre(const P: TProcedureOfObject);
begin
  FProcUseMetaFiltre := P;
end;
function TGHTopoContext2DA.GetNomOnglet(): string;
begin
  Result := FArrOngletsParams[FCurrIdxOnglet].ongName;
end;
procedure TGHTopoContext2DA.SetBackGrdColorOnglet(const QIdxOnglet: integer; const Col: TColor);
begin
  FArrOngletsParams[QIdxOnglet].ongBackGround := Col;
end;

function TGHTopoContext2DA.GetLargeurViseesOngletInPX(const QIdxOnglet: integer): integer;
begin
  result := FArrOngletsParams[QIdxOnglet].ongViseesLargeurInPX;
end;

function TGHTopoContext2DA.GetLargeurViseesOngletInMM(const QIdxOnglet: integer): double;
begin
  result := FArrOngletsParams[QIdxOnglet].ongViseesLargeurInMM;
end;
procedure TGHTopoContext2DA.SetLargeurViseesOngletInPX(const QIdxOnglet: integer; const qL: integer);
begin
  FArrOngletsParams[QIdxOnglet].ongViseesLargeurInPX := qL;
end;
procedure TGHTopoContext2DA.SetLargeurViseesOngletInMM(const QIdxOnglet: integer; const qL: double);
begin
  FArrOngletsParams[QIdxOnglet].ongViseesLargeurInMM := qL;
end;
function TGHTopoContext2DA.GetBackGrdColorOnglet(const QIdxOnglet: integer): TColor;
begin
  Result := FArrOngletsParams[QIdxOnglet].ongBackGround;
end;

//******************************************************************************
// quadrillage
procedure TGHTopoContext2DA.SetQuadrillageSpc(const Spacing: double);
begin
  //FQuadrillageSpc := Spacing;
  FArrOngletsParams[FCurrIdxOnglet].ongQdrSpc := Spacing;
end;

function TGHTopoContext2DA.GetQuadrillageSpc(): double;
begin
  Result := FArrOngletsParams[FCurrIdxOnglet].ongQdrSpc;
end;

procedure TGHTopoContext2DA.SetQuadrillageColor(const Col: TColor);
begin
  if (Col <> FArrOngletsParams[FCurrIdxOnglet].ongQdrColor) then  FArrOngletsParams[FCurrIdxOnglet].ongQdrColor := Col;
end;

function TGHTopoContext2DA.GetQuadrillageColor(): TColor;
begin
  Result := FArrOngletsParams[FCurrIdxOnglet].ongQdrColor;
end;

procedure TGHTopoContext2DA.SetElementsDrawn(const ED: TSetElementsDrawn);
begin
  FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn := ED;
end;

function  TGHTopoContext2DA.GetElementsDrawn(): TSetElementsDrawn;
begin
  Result := FArrOngletsParams[FCurrIdxOnglet].ongElementsDrawn;
end;

procedure TGHTopoContext2DA.QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
 begin
   Vue.Canvas.Pen.Mode := QPM;
   Vue.Canvas.Pen.Style:= QPS;
   Vue.Canvas.Pen.Color:= QPC;
   Vue.Canvas.Pen.Width:= QPW;
 end;
 procedure TGHTopoContext2DA.Rearmer();
 begin
   FAttenduSecondPoint        := False;
   QSetPen(pmCopy, psSolid, 0, clGray);
 end;
procedure TGHTopoContext2DA.SetModeTravail(const MT: TModesTravail);
begin
  if (MT = FModesTravail) then Exit; // ne rien faire si le mode de travail est inchangé

  if ((not FCroquisTerrain.IsReady) AND // ne rien faire ici si le croquis de terrain est KO
      (MT in [mtNEW_ANNOTATION, mtSELECT_ANNOTATION, mtDELETE_ANNOTATION, mtNEW_POLYLINE, mtADD_VERTEX_POLYLINE, mtDELETE_POLYLINE])) then
  begin
    ShowMessage('Croquis de terrain HS');
    Exit;
  end;
  FModesTravail := MT;
  case FModesTravail of
     mtREADY: Rearmer();
     mtPAN_PREMIER_POINT,
     mtDISTANCE_PREMIER_POINT,
     mtADD_SERIE_PREMIER_POINT,
     mtBOUCLER_PREMIER_POINT,
     mtPROFIL_MNT_PREMIER_POINT:
      begin
        FZC1 := FMyPos;
        FZC2 := FMyPos;
        FAttenduSecondPoint := false;
        QSetPen(pmCopy, psSolid, 0, clGray);
      end;
      mtPAN_SECOND_POINT,
      mtDISTANCE_SECOND_POINT,
      mtADD_SERIE_SECOND_POINT,
      mtBOUCLER_SECOND_POINT,
      mtPROFIL_MNT_SECOND_POINT:
      begin
        FAttenduSecondPoint := true;
        QSetPen(pmNotXor, psSolid, 0, clGray); // pmNotXor;
      end;
      mtZOOM_PREMIER_COIN, mtMETAFILTRE_ZONE_PREMIER_COIN:
      begin
        FZC1 := FMyPos;
        FZC2 := FMyPos;
        FAttenduSecondPoint := false;
        QSetPen(pmCopy, psSolid, 0, clGray);
      end;
      mtZOOM_SECOND_COIN, mtMETAFILTRE_ZONE_SECOND_COIN:
      begin
        FAttenduSecondPoint := true;
        QSetPen(pmNotXor, psSolid, 0, clGray);// pmNotXor;
      end;
      mtNEW_LASSO_SELECTION:
      begin
        setlength(FLassoDeSelection, 0);
      end
  else
    pass;
  end;
  lbModeTravail.Caption    := ChooseString(Ord(MT),
                                       ['READY',                         //TModesTravail  = (mtREADY,
                                        'PAN_PT1', 'PAN_PT2',                    // mtPAN_PREMIER_POINT, mtPAN_SECOND_POINT,
                                        'ZOOM_C1', 'ZOOM_C2',                  //mtZOOM_PREMIER_COIN, mtZOOM_SECOND_COIN,
                                        'MF_ZONE', 'MF_ZONE',            //mtMETAFILTRE_ZONE_PREMIER_COIN, mtMETAFILTRE_ZONE_SECOND_COIN,
                                        'DISTANCE_PT1', 'DISTANCE_PT2',          //mtDISTANCE_PREMIER_POINT, mtDISTANCE_SECOND_POINT,
                                        'ADD_SERIE_PT1', 'ADD_SERIE_PT2', // mtADD_SERIE_PREMIER_POINT, mtADD_SERIE_SECOND_POINT,
                                        'RACCORDER_PT1', 'RACCORDER_PT2', //mtBOUCLER_PREMIER_POINT, mtBOUCLER_SECOND_POINT
                                        'PROFIL_MNT_PT1','PROFIL_MNT_PT1', //mtPROFIL_MNT_PREMIER_POINT, mtPROFIL_MNT_SECOND_POINT
                                        'ADD_NOTE', 'SELECT_NOTE', 'DEL_NOTE',  //mtNEW_ANNOTATION, mtSELECT_ANNOTATION, mtDELETE_ANNOTATION,
                                        'ADD_POLY', 'SELECT_POLY', 'DEL_POLY',
                                        'LASSO'
                                       ]);
  lbAdditionalInfos.Caption := ChooseString(Ord(MT),
                                       ['--',
                                        'Premier point', 'Second point',         // mtPAN_PREMIER_POINT, mtPAN_SECOND_POINT,
                                        'Premier coin', 'Second coin',           //mtZOOM_PREMIER_COIN, mtZOOM_SECOND_COIN,
                                        'Premier coin', 'Second coin',            //mtMETAFILTRE_ZONE_PREMIER_COIN, mtMETAFILTRE_ZONE_SECOND_COIN,
                                        'Premier point', 'Second point',           //mtDISTANCE_PREMIER_POINT, mtDISTANCE_SECOND_POINT,
                                        'Série à raccorder', 'Station de raccordement', // mtADD_SERIE_PREMIER_POINT, mtADD_SERIE_SECOND_POINT,
                                        'Premier point', 'Second point',              //mtBOUCLER_PREMIER_POINT, mtBOUCLER_SECOND_POINT
                                        'Premier point', 'Second point',             //mtPROFIL_MNT_PREMIER_POINT, mtPROFIL_MNT_SECOND_POINT
                                        'Au point', 'Pick near', 'Pick near',        //mtNEW_ANNOTATION, mtSELECT_ANNOTATION, mtDELETE_ANNOTATION,
                                        'Du point', 'Au point', 'Pick near',
                                        'Au point'
                                       ]);
end;



//******************************************************************************
procedure TGHTopoContext2DA.SetViewLimits(const X1, Y1, X2, Y2: double; const DebugTag: string);
const
  Epsilon = 1e-2;
var
  qX1, qX2, qY1, qY2: double;
  d1, d2: double;
begin
  AfficherMessage(Format('%s.SetViewLimits(%.2f, %.2f, %.2f, %.2f) - Tag: %s', [ClassName, X1, Y1, X2, Y2, DebugTag]));
  qX1 := Min(X1, X2);
  qY1 := Min(Y1, Y2);
  qX2 := Max(X1, X2);
  qY2 := Max(Y1, Y2);
  d1 := qX2 - qX1;
  d2 := qY2 - qY1;
  // si zone trop étroite, abandonner
  if (Abs(d1) < Epsilon) or (Abs(d2) < Epsilon) then Exit;
  // échanger de manière à rendre indifférent le sens du rectangle de sélection
  if (qX2 < qX1) then Swap(qX1, qX2);
  if (qY2 < qY1) then Swap(qY1, qY2);
  FRegionCMini := MakeTPoint2Df(qX1, qY1);
  FRegionCMaxi := MakeTPoint2Df(qX2, qY2);
  // Redéfinition de la hauteur maxi
  FRegionCMaxi.Y := GetRYMaxi();
  // on met tout çà dans l'onglet
  FArrOngletsParams[FCurrIdxOnglet].ongC1 := FRegionCMini;
  FArrOngletsParams[FCurrIdxOnglet].ongC2 := FRegionCMaxi;
end;
procedure TGHTopoContext2DA.SetViewLimits(const QCoinBasGauche, QCointHautDroit: TPoint2Df; const DebugTag: string);
begin
  SetViewLimits(QCoinBasGauche.X, QCoinBasGauche.Y, QCointHautDroit.X, QCointHautDroit.Y, DebugTag);
end;




procedure TGHTopoContext2DA.DeplacerVue(const QOffsetX, QOffsetY: double);
var
  DbgTag: String;
begin
  DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
  SetViewLimits(FRegionCMini.X - QOffsetX, FRegionCMini.Y - QOffsetY,
                FRegionCMaxi.X - QOffsetX, FRegionCMaxi.Y - QOffsetY,
                DbgTag);
end;


function TGHTopoContext2DA.GetViewLimitsFromOnglet(const QIdxOnglet: integer; out QCoinBasGauche, QCointHautDroit: TPoint2Df): boolean;
var
  EWE: TVue2DParams;
begin
  result := false;
  try
    EWE := FArrOngletsParams[QIdxOnglet];
    QCoinBasGauche  := EWE.ongC1;
    QCointHautDroit := EWE.ongC2;
    result := true;
  except
    pass;
  end;
end;



//******************************************************************************
//******************************************************************************


function TGHTopoContext2DA.GetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  Result.X :=  FInvRappScrReal * PP.X + FRegionCMini.X;
  Result.Y := -FInvRappScrReal * PP.Y + FRegionCMaxi.Y;
end;



function TGHTopoContext2DA.GetCoordsPlan(const QX, QY: double): TPoint;
begin
  Result.X := Round((QX - FRegionCMini.X) * FRappScrReal);
  Result.Y := Round((FRegionCMaxi.Y - QY) * FRappScrReal);
end;
function TGHTopoContext2DA.GetCoordsPlan(const PM: TPoint2Df): TPoint;
begin
  Result := GetCoordsPlan(PM.X, PM.Y);
end;

//******************************************************************************
procedure TGHTopoContext2DA.DeplacerVueInt(const DepX, DepY: integer);
var
  P : TPoint;
  dq, dx, dy: Double;
  DbgTag: String;
begin
  P.X := 0;
  P.Y := 0;
  FZC1:= GetCoordsMonde(P);
  P.X := Vue.Width   ; P.Y:=Vue.Height;
  FZC2:= GetCoordsMonde(P);

  dq  := 0.02 * (FZC2.X - FZC1.X);
  dx  :=  dq * DepX;
  dy  := -dq * DepY;
  DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
  SetViewLimits(FRegionCMini.X + dx, FRegionCMini.Y + dy,
                FRegionCMaxi.X + dx, FRegionCMaxi .Y+ dy,
                DbgTag);
  self.RefreshDessin();
end;




procedure TGHTopoContext2DA.SetModeSelectionListe(const M: TModeSelectionListe);
begin
  FModeSelectionListe := M;
end;
//------------------------------------------------------------------------------
// centrer le dessin sur le point trouvé
procedure TGHTopoContext2DA.CentrerVueSurPointXY(const QX, QY: double; const DoMarker: boolean; const LbMarker: string);
var
  C0, Offset: TPoint2Df;
  DbgTag: String;
begin
  try
    C0 := MakeTPoint2Df(0.5 * (FRegionCMini.X + FRegionCMaxi.X),
                        0.5 * (FRegionCMini.Y + FRegionCMaxi.Y));
    Offset.X := QX - C0.X;
    Offset.Y := QY - C0.Y;
    DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
    SetViewLimits(FRegionCMini.X + Offset.X, FRegionCMini.Y + Offset.Y,
                  FRegionCMaxi.X + Offset.X, FRegionCMaxi.Y + Offset.Y,
                  DbgTag);
    SetMarker(MakeTMarker(DoMarker, QX, QY, clAqua, LbMarker));
    RedessinEcran(Vue.Width, Vue.Height, False);
  except
  end;
end;
procedure TGHTopoContext2DA.SetMarker(const M: TMarker);
begin
  FCurrentMarker := M;
end;


procedure TGHTopoContext2DA.lbInfosClick(Sender: TObject);
begin
  pnlFullInfos.Visible := not pnlFullInfos.Visible;
  if (pnlFullInfos.Visible) then DisplayFullInfosStation(FStationNearToMouse);
end;

procedure TGHTopoContext2DA.lbTitreQuickSaisieViseeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  pass;
end;

procedure TGHTopoContext2DA.lbTitreQuickSaisieViseeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  pass;
end;

procedure TGHTopoContext2DA.lbTitreQuickSaisieViseeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  pass;
end;



procedure TGHTopoContext2DA.MetaFiltrer(const F: string);
var
  QDevelViseesVisibles: double;
begin
  FBDDEntites.MetaFiltre(F, QDevelViseesVisibles);
  lbLongViseesVisibles.Caption := Format(FORMAT_NB_REAL_3_DEC, [QDevelViseesVisibles]);
  FBDDEntites.SetMinMax(False);
  RedessinEcran(Vue.Width, Vue.Height, True);
end;

//******************************************************************************
procedure TGHTopoContext2DA.ZoomFact(const Fact: double);
var
  ZoomCentre: TPoint2Df;
  RX, RY: Double;
  DbgTag: String;
begin
  // point central
  ZoomCentre := MakeTPoint2Df(0.50 * (FRegionCMaxi.X + FRegionCMini.X),
                              0.50 * (FRegionCMaxi.Y + FRegionCMini.Y));
  // demi-rayon
  RX := 0.50 * (FRegionCMaxi.X - FRegionCMini.X) / Fact;
  RY := 0.50 * (FRegionCMaxi.Y - FRegionCMini.Y) / Fact;
  DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
  self.SetViewLimits(ZoomCentre.X - RX, ZoomCentre.Y - RY,
                     ZoomCentre.X + RX, ZoomCentre.Y + RY,
                     DbgTag);
  self.RefreshDessin();
end;

procedure TGHTopoContext2DA.LocaliserUneStation();
var
  EWE: String;
begin
  EWE := '';
  if (InputQuery('Localiser une station', 'ID station', EWE)) then LocaliserUneStationByCle(EWE);
end;
procedure TGHTopoContext2DA.LocaliserUneStationByCle(const K: string);
var
  EWE: String;
  QMyBaseStation: TBaseStation;
begin
  EWE := Trim(K);
  if (FBDDEntites.FindStationByCle(false, EWE, QMyBaseStation)) then
  begin
    EWE := Format(FMTSERST, [QMyBaseStation.Entite_Serie, QMyBaseStation.Entite_Station]);
    self.CentrerVueSurPointXY(QMyBaseStation.PosStation.X, QMyBaseStation.PosStation.Y, True, EWE);
    self.SetCurrentStation(QMyBaseStation, True);
  end
  else
  begin
    ShowMessage(rsMATCHNOTFOUND);
  end;
end;


procedure TGHTopoContext2DA.CreerTimeLapse(const QDirectory: TStringDirectoryFilename);
var
  NbSeances, i: Integer;
  QMetaFiltre: String;
  QFilename  : string;
  MyExpe: TExpe;
  procedure CreerImg(const QFiltre: string; const QFileOutput: string);
  begin
    AfficherMessage(Format('--> %s -> %s', [QFiltre, QFileOutput]));
    ApplyMetaFiltre(True, QMetaFiltre);
    RedessinEcran(Vue.Width, Vue.Height, false, True, QFileOutput);
    Application.ProcessMessages;
  end;
begin
  AfficherMessage(Format('%s.CreerTimeLapse("%s")', [ClassName, QDirectory]));
  ForceDirectories(QDirectory);         // forcer la création du dossier
  NbSeances := FBDDEntites.GetNbExpes();
  i := 0;
  while (i < NbSeances) do
  begin
    QMetaFiltre := Format('%s < %d', [rsMETAFILTRE_EXPE, i]);
    MyExpe    := FBDDEntites.GetExpe(i);
    {$WARNING: TEXpe.DateExpe à implementer}

    QFilename := QDirectory + DirectorySeparator + Format('Img_Expe_%d_%.4d-%.2d-%.2d.png', [i, MyExpe.AnneeExpe, MyExpe.MoisExpe, MyExpe.JourExpe]);
    if (not FRedessinInProcess) then
    begin
      CreerImg(QMetaFiltre, QFilename);
      Inc(i);
    end
    else
      AfficherMessage('** Is busy **');
  end;
end;

function TGHTopoContext2DA.CroquisDeTerrainIsReady(): boolean;
begin
  Result := FCroquisTerrain.IsReady;
end;

procedure TGHTopoContext2DA.chkMetaFiltreActifChange(Sender: TObject);
begin
  ApplyMetaFiltre(chkMetaFiltreActif.Checked, trim(editMetaFiltre.Text));
end;

procedure TGHTopoContext2DA.editMetaFiltreKeyPress(Sender: TObject; var Key: char);
begin
  case Ord(Key) of
    VK_RETURN: ApplyMetaFiltre(chkMetaFiltreActif.Checked, trim(editMetaFiltre.Text));
    VK_F1    : DisplayHelpSystem('METAFILTRE');
  else;
    pass;
  end;
end;

//******************************************************************************
function TGHTopoContext2DA.GetPtrMaillage(): TMaillage;
begin
  result := FMyMaillage;
end;
procedure TGHTopoContext2DA.chkDrawMaillageChange(Sender: TObject);
begin
  FMaillageDisplayed := (chkDrawMaillage.Checked and FMyMaillage.IsValidMaillage());
  self.RefreshDessin();
end;



procedure TGHTopoContext2DA.acIsovaleurSurZStationExecute(Sender: TObject);
begin
  ShowMessageFmt('%d entrées', [FBDDEntites.GetNbEntrances()]);

  try
    if (FMyMaillage.IsValidMaillage() and FMaillageDisplayed) then
    begin
      editIsoValeur.Value := FCurrentStation.PosStation.Z;
      self.RefreshDessin();
    end;
  except
    pass;
  end;
end;
procedure TGHTopoContext2DA.DisplayPnlMaillage(const B: boolean);
begin
  pnlMaillage.Visible := B;
end;

function TGHTopoContext2DA.ChargerMaillage(const QFileName: string): boolean;
begin
  Result  := FMyMaillage.LoadMaillage(QFileName);
  DisplayPnlMaillage(Result);
end;
function TGHTopoContext2DA.IsMaillageDisplayed(): boolean;
begin
  Result := FMaillageDisplayed and FMyMaillage.IsValidMaillage();
end;
function TGHTopoContext2DA.IsMaillageValide(): boolean;
begin
  Result := FMyMaillage.IsValidMaillage();
end;

// Version actuelle de RedessinEcran
procedure TGHTopoContext2DA.RedessinEcran(const ImgWidth, ImgHeight: integer;
                                          const DoRecalcDegrades: boolean;
                                          const DoExportInFile: boolean = false;
                                          const QFileName: string = '');
var
  MyOnglet: TVue2DParams;
  BGC: TBGRAPixel;
  TmpBuffer: TGHTopoDrawingContext;
begin
  if (FRedessinInProcess) then exit; // sémaphore 'FRedessinInProcess' est armé ? = On quitte (le dessin est en cours)
  FRedessinInProcess := True;        // Armement du sémaphore
  MyOnglet := GetCurrentOnglet();
  BGC := BGRA(Red(MyOnglet.ongBackGround), Green(MyOnglet.ongBackGround), Blue(MyOnglet.ongBackGround), 255);
  // recalculer les dégradés si DoRecalcDegrades est armé
  if (DoRecalcDegrades) then FBDDEntites.CalcCouleursByDepth(MyOnglet.ongDegradeAltMini, MyOnglet.ongDegradeAltMaxi);
  TmpBuffer:= TGHTopoDrawingContext.Create(ImgWidth, ImgHeight, BGC);
  try
    if (Not TmpBuffer.Initialiser(FDocuTopo,
                                  FBDDEntites,
                                  FCroquisTerrain,
                                  FMyMaillage,
                                  MyOnglet,
                                  FModeSelectionListe,
                                  FDoHighLightCurrentItem)) then exit;
    TmpBuffer.SetBounds(FRegionCMini.X, FRegionCMini.Y, FRegionCMaxi.X, FRegionCMaxi.Y);
    TmpBuffer.BeginDrawing();
      if (edQuadrilles     in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawQuadrillage(MyOnglet.ongQdrType, MyOnglet.ongQdrColor, MyOnglet.ongQdrSpc);
      if (edANTENNES       in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawAntennes();
      if (edCrossSections  in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawSections();
      if (edPolygonals     in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawPolygonals(MyOnglet.ongDrawFastCenterline);
      if (edFillGalerie    in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawGaleries(MyOnglet.ongDrawFastCenterline);
      if (edWalls          in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawGaleries(MyOnglet.ongDrawFastCenterline);
      if (edStations       in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawStations();
      if (edIDStations     in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawIDStations();
      if (edJONCTIONS      in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawJonctions();
      if (edAltitudes      in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawCotation(false);
      if (edCotes          in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawCotation(true);
      if (edPOI            in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawPOIs();
      if (edENTRANCE_MKS   in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawEntrancesOrPointEntities(edENTRANCE_NAMES in MyOnglet.ongElementsDrawn);
      if (edCROQUIS        in MyOnglet.ongElementsDrawn) then TmpBuffer.DrawCroquisTerrain();
      TmpBuffer.DrawMaillage(chkDrawMaillage.Checked, editIsoValeur.Value, btnLineContourColor.ButtonColor, editLineContourOpacity.Value);
       // uniquement en mode écran
       if (not DoExportInFile) then TmpBuffer.DrawCurrentStationTopo(FCurrentStation);
       // Overlay éventuel
       if (FOverlayed) then
       begin
         TmpBuffer.DrawOverlay();
         {$IFDEF GHTOPO_SIMPLIFIE}
         TmpBuffer.DrawShortestPath(FGraphe, FShortestPath);
         {$ENDIF GHTOPO_SIMPLIFIE}


       end;
       DrawPipistrelle(TmpBuffer);
    TmpBuffer.EndDrawing();
    if (DoExportInFile) then TmpBuffer.SaveToFile(QFileName) else TmpBuffer.Draw(Vue.Canvas, 0, 0, True);
  finally
    FreeAndNil(TmpBuffer);
    FRedessinInProcess := False; // désarmement du sémaphore d'occupation
  end;
end;
procedure TGHTopoContext2DA.ExporterPlanEnImage(const ImgWidth, ImgHeight: integer; const DoRecalcDegrades: boolean; const QFileName: string);
begin
  RedessinEcran(ImgWidth, ImgHeight, DoRecalcDegrades, True, QFileName);
end;

procedure TGHTopoContext2DA.RefreshDessin();
begin
  Vue.Invalidate;
end;
// affiche un message toast
procedure TGHTopoContext2DA.DisplayToast(const Msg: string);
var
  QW, QH, QL, QT: Integer;
begin

  //try
  pnlToast.Visible := false;
  QW := trunc(0.7 * self.Width);
  QH := self.Height div 8;
  QL := (self.Width - QW) div 2;
  QT := (self.Height - QH) div 2;
  pnlToast.Left   := QL;
  pnlToast.Top    := QT;
  pnlToast.Width  := QW;
  pnlToast.Height := QH;
  pnlToast.Color  := clYellow;
  pnlToast.ParentFont := false;
  pnlToast.Font.Style := [fsBold];
  pnlToast.Caption    := Msg;
  pnlToast.Visible:= true;
  application.ProcessMessages;
  Sleep(2000);
  pnlToast.Visible:= false;
  //finally
  //end;
  Vue.Invalidate;
end;
{$IFDEF GHTOPO_SIMPLIFIE}
// Spécifique DistoX
procedure TGHTopoContext2DA.TraiterMesureIssueDuDistoX(const MV: TMesureViseeDistoX; const TV: TTypeViseeDistoX; const TagString: string = '');
var
  MySerie: TObjSerie;
begin


  case TV of
    tvdRADIANTE   : AjouterUneAntenneALaStationCourante(MV.Longueur, MV.Azimut, MV.Pente, True);
    tvdCHEMINEMENT:
      begin
        AjouterUneViseeALaSerieCourante(tgDEFAULT, MV.Longueur, MV.Azimut, MV.Pente, 0.00, 0.00, 0.00, 0.00, '', TagString, True);
        DisplayToast(Format('Ajoute visée à %d.%d: %.3f, %.3f, %.3f',
                     [FCurrentDistoXNumeroSerie, FCurrentDistoXNumeroStation,
                      MV.Longueur, MV.Azimut, MV.Pente]));

      end;
    //tvdEMULATED   : AfficherMessageErreur(Format('Mesure émulée: %.3f, %.3f, %.3f', [V.Longueur, V.Azimut, V.Pente]));
  end;
end;
procedure TGHTopoContext2DA.SetShortestPath(const P: TPathBetweenNodes);
begin
  FShortestPath := P;
end;
{$ENDIF GHTOPO_SIMPLIFIE}

end.
