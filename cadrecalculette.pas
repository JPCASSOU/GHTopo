unit CadreCalculette;
// Calculette et convertisseur de coordonnées
// 07/01/2014: Désactivation provisoire du support des déclinaisons magnétiques
// 06/02/2014: Remplacement de l'évaluateur d'expressions
// 10/09/2014: Importantes améliorations d'ergonomie.
// 10/09/2014: L'évaluateur d'expressions devient un objet
// 09/10/2014: Nombreux bugs fixés
//             Les nombres formatés sont automatiquement convertis
// 15/01/2015: Nouvel onglet Interpréteur Pascal
// 05/02/2015: Adaptation de l'interface pour les tablettes
// 13/12/2015: Support export KML et GPX du tableau des données converties
// 06/06/2016: Tableau: Support des conversions depuis le datum spécifié dans la ligne courante
// 16/03/2021: Suppression de l'onglet 'Résolution de triangles' (inutilisé)
// 16/03/2021: Ajout de l'onglet 'Textures'

//*)
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Graphics,
  math,
  UnitListesSimplesWithGeneriques,
  unitUtilsComposants,
  CallDialogsStdVersion,
  ConvertisseurJPC, // API JPC
  UnitTSAGeoMag,   // calculateur de déclinaison
  UnitKMLExport,   // export KML
  UnitLeafletExport,
  FastGEO,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitClasseMaillage,
  UnitFichesTopo,
  GenerationFichesPointsTopo,
  //UnitObjetSerie,
  //unitProfilTopo,
  {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
  SVGCanvasUnit,
  evaluateur_expressions,
  DGCTypes,
  CadreDGCDrawingContext,
  DGCClassSectionOfBeam,
  //DGCClassTriangleQuelconque,
  CadrePascalScript, CadreTextures,
  Classes, SysUtils, FileUtil,
  curredit,
  SynEdit,
  //SynHighlighterPas,
  ubarcodes, Forms, Dialogs,
  Controls, ComCtrls, StdCtrls, Grids, EditBtn, ExtCtrls, PairSplitter, Menus,
  LCLType, Types, Clipbrd,
  ActnList, Buttons,
  PdfDoc,
  PdfFonts,
  PdfTypes, PReport, uPSComponent_Default
  ;

const DEFAULT_OSM_LAYER = 'Markers0';
// pour l'export KML
type TLigneCoordonnees = record
  Etiquette           : string;
  OSMLayer            : string;
  Designation         : string;
  Description         : string;
  Observations        : string;
  Photo               : string;
  XSource        : double;
  YSource        : double;
  ZSource        : double;
  XCible         : double;
  YCible         : double;
end;



type

{ TCdrCalculette }

  TCdrCalculette = class(TFrame)
    acGrdDonneesRemoveLigne: TAction;
    acGrdDonneesRemoveColonne: TAction;
    acQRCodeCoordPoint: TAction;
    acReverseSystCoordonnees: TAction;
    acCalculer: TAction;
    acEchangerValeurs: TAction;
    acConvertir: TAction;
    acCopierTableau: TAction;
    acColler: TAction;
    acOuvrirCSV: TAction;
    acConvertirTableau: TAction;
    acCalculDeclimag: TAction;
    acCopyTableauDeclimag: TAction;
    acCalculDeclimagTableau: TAction;
    acExportCSV: TAction;
    acExportListePointsKML: TAction;
    acQRCodeListeCoords: TAction;
    acCopyCoordinates: TAction;
    acGenerateQRCodeFromTexte: TAction;
    acExportQRCodeToSVG: TAction;
    acExportOSMLeaflet: TAction;
    acPSExecute: TAction;
    acPSStop: TAction;
    acLoadPSScript: TAction;
    acSavePSScript: TAction;
    acMakeTINPointsFromSynEdit: TAction;
    acOSMAddLayer: TAction;
    acOSMDeleteLayer: TAction;
    acOSMRemoveAllLayers: TAction;
    acOSMExtractLayersFromTableau: TAction;
    acOSMEditLayer: TAction;
    acCopyLayers: TAction;
    acPasteLayers: TAction;
    acVisuCoordsSurCarte: TAction;
    ActionList1: TActionList;
    btnConvertDMS2DegDec: TButton;
    btnCreateGUID: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    btnSaveMaillage: TButton;
    btnTrianguler: TButton;
    btnMaillageImporterXYZ: TButton;
    Button4: TButton;
    btnBackGrdInertieFigure: TColorButton;
    CdrDGCDrawingContext1: TCdrDGCDrawingContext;
    CdrDGCDrawingContext2: TCdrDGCDrawingContext;
    CdrPascalScript1: TCdrPascalScript;
    CdrTextures1: TCdrTextures;
    chkAutoIncrement: TCheckBox;
    chkPremLigneIsTitres: TCheckBox;
    cmbColAdditionalOSMLayers: TComboBox;
    cmbColDescription: TComboBox;
    cmbColDesignation: TComboBox;
    cmbColEtiquettes: TComboBox;
    cmbColObservations: TComboBox;
    cmbColPhoto: TComboBox;
    cmbColXCible: TComboBox;
    cmbColXSource: TComboBox;
    cmbColYCible: TComboBox;
    cmbColYSource: TComboBox;
    cmbColZSource: TComboBox;
    cmbFormatExport: TComboBox;
    cmbSeparateur: TComboBox;
    cmbSystCibleJPC: TComboBox;
    cmbSystSourceJPC: TComboBox;
    editDocumentTitle: TEdit;
    editLatitudeDMS: TEdit;
    editLongitudeDMS: TEdit;
    editGUID: TEdit;
    editX_Cible1JPC: TCurrencyEdit;
    editX_Source1JPC: TCurrencyEdit;
    editY_Cible1JPC: TCurrencyEdit;
    editY_Source1JPC: TCurrencyEdit;
    grbxAdditionalLayers: TGroupBox;
    grdCoordonneesSection: TStringGrid;
    grdMaillageCoordsXYZ: TStringGrid;
    grdDonnees: TStringGrid;
    HeaderControl1: THeaderControl;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label3: TLabel;
    Label31: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbConversionsTableau: TStaticText;
    lbConversionsUnitaires: TStaticText;
    lbDocumentTitle: TLabel;
    lbHintGrdConversions: TStaticText;
    lbInertieI: TStaticText;
    lbInertieII: TStaticText;
    lbIxy: TStaticText;
    lbMaillageNbVertex: TLabel;
    lbMaillageNbTriangles: TLabel;
    lbPerimetre: TStaticText;
    lbCentroidX: TStaticText;
    lbAire: TStaticText;
    lbIxGx: TStaticText;
    lbCentroidY: TStaticText;
    lbAnglePrincipal: TStaticText;
    lbIyGy: TStaticText;
    lbListeVariables: TStaticText;
    lbProcessEnCours: TLabel;
    lbStatutConvertisseur: TStaticText;
    lbSystCible: TLabel;
    lbSystemesCoordonnees: TStaticText;
    lbSystSource: TLabel;
    lbValeursCalculees: TLabel;
    lbValeursEntree: TLabel;
    lbValeursX: TLabel;
    lbValeursY: TLabel;
    lsbAdditionalLayers: TListBox;
    lsbVariables: TListBox;
    editTextToEncodeQR: TMemo;
    PageControl2: TPageControl;
    PairSplitter10: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitter3: TPairSplitter;
    PairSplitter7: TPairSplitter;
    PairSplitter8: TPairSplitter;
    PairSplitter9: TPairSplitter;
    PairSplitterSide13: TPairSplitterSide;
    PairSplitterSide14: TPairSplitterSide;
    PairSplitterSide15: TPairSplitterSide;
    PairSplitterSide16: TPairSplitterSide;
    PairSplitterSide17: TPairSplitterSide;
    PairSplitterSide18: TPairSplitterSide;
    PairSplitterSide19: TPairSplitterSide;
    PairSplitterSide20: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    PairSplitterSide5: TPairSplitterSide;
    PairSplitterSide6: TPairSplitterSide;
    Panel1: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pnlListeVariables: TPanel;
    pnlProgressionProcess: TPanel;
    ProgressBar1: TProgressBar;
    PSImport_DateUtils1: TPSImport_DateUtils;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton2: TSpeedButton;
    btnPaste: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    SpeedButton27: TSpeedButton;
    SpeedButton28: TSpeedButton;
    SpeedButton29: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton30: TSpeedButton;
    SpeedButton31: TSpeedButton;
    SpeedButton32: TSpeedButton;
    SpeedButton33: TSpeedButton;
    SpeedButton4: TSpeedButton;
    lbCopyrightNotesConvertisseur: TStaticText;
    lbPosCursorSynEdit: TStaticText;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    grdMaillageTriangles: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tabShtTextures: TTabSheet;
    tabShtPascalScript: TTabSheet;
    tabShtMNT: TTabSheet;
    tabShtInerties: TTabSheet;

    VueQRCode: TBarcodeQR;
    lbListeDesFonctions: TStaticText;
    lbListeCalculs: TStaticText;
    lbQRCodeGenerator: TStaticText;
    lsbFonctions: TListBox;
    lsbLastCalculs: TListBox;
    editAltitude: TCurrencyEdit;
    editAnneeArrivee: TCurrencyEdit;
    editAnneeDepart: TCurrencyEdit;
    editDateDeclimag: TDateEdit;
    editDeclinaison: TCurrencyEdit;
    editExpression: TEdit;
    editLatitude: TCurrencyEdit;
    editLongitude: TCurrencyEdit;
    grdDeclinaisons: TStringGrid;
    lbPromptExpression: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    lbDeclimagUnitaire: TStaticText;
    lbDeclimagTableau: TStaticText;
    lbExpression: TStaticText;
    Long: TLabel;
    Long1: TLabel;
    Long2: TLabel;
    Long3: TLabel;
    Long4: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel10: TPanel;
    Panel3: TPanel;
    pnlQRCode: TPanel;
    Panel9: TPanel;
    pnlTableauDeclimag: TPanel;
    Panel8: TPanel;
    lbDescDeclimagCalculator: TStaticText;
    lbErreurCalcul: TStaticText;
    PopupMenu1: TPopupMenu;
    tabShtGenerateurQRCode: TTabSheet;
    tabShtCalculatrice: TTabSheet;
    tabShtConvertisseur: TTabSheet;
    tabShtDeclimag: TTabSheet;
    procedure acCalculDeclimagExecute(Sender: TObject);
    procedure acCalculDeclimagTableauExecute(Sender: TObject);
    procedure acCalculerExecute(Sender: TObject);
    procedure acCollerExecute(Sender: TObject);
    procedure acConvertirExecute(Sender: TObject);
    procedure acConvertirTableauExecute(Sender: TObject);
    procedure acCopierTableauExecute(Sender: TObject);
    procedure acCopyCoordinatesExecute(Sender: TObject);
    procedure acCopyLayersExecute(Sender: TObject);
    procedure acCopyTableauDeclimagExecute(Sender: TObject);
    procedure acEchangerValeursExecute(Sender: TObject);
    procedure acExportCSVExecute(Sender: TObject);
    procedure acExportListePointsKMLExecute(Sender: TObject);
    procedure acExportQRCodeToSVGExecute(Sender: TObject);
    procedure acGenerateQRCodeFromTexteExecute(Sender: TObject);
    procedure acOSMEditLayerExecute(Sender: TObject);
    procedure acOSMExtractLayersFromTableauExecute(Sender: TObject);
    procedure acOSMAddLayerExecute(Sender: TObject);
    procedure acOSMDeleteLayerExecute(Sender: TObject);
    procedure acOSMRemoveAllLayersExecute(Sender: TObject);
    procedure acOuvrirCSVExecute(Sender: TObject);
    procedure acPasteLayersExecute(Sender: TObject);
    procedure acQRCodeCoordPointExecute(Sender: TObject);
    procedure acQRCodeListeCoordsExecute(Sender: TObject);
    procedure acGrdDonneesRemoveColonneExecute(Sender: TObject);
    procedure acGrdDonneesRemoveLigneExecute(Sender: TObject);
    procedure acReverseSystCoordonneesExecute(Sender: TObject);
    procedure acSavePSScriptExecute(Sender: TObject);
    procedure acVisuCoordsSurCarteExecute(Sender: TObject);
    procedure btnCreateGUIDClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnConvertDMS2DegDecClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cmbColAdditionalOSMLayersChange(Sender: TObject);
    procedure cmbColDescriptionChange(Sender: TObject);
    procedure cmbColObservationsChange(Sender: TObject);
    procedure cmbColPhotoChange(Sender: TObject);
    procedure editExpressionEnter(Sender: TObject);
    procedure editLatitudeDMSEnter(Sender: TObject);
    procedure editLatitudeDMSExit(Sender: TObject);
    procedure editLongitudeDMSEnter(Sender: TObject);
    procedure editLongitudeDMSExit(Sender: TObject);

    procedure editTextToEncodeQRKeyPress(Sender: TObject; var Key: char);

    procedure grdDonneesHeaderClick(Sender: TObject; IsColumn: Boolean;  Index: Integer);
    procedure HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure Label26Click(Sender: TObject);
    procedure Label34Click(Sender: TObject);
    procedure lsbAdditionalLayersDblClick(Sender: TObject);
    procedure lsbAdditionalLayersDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbFonctionsClick(Sender: TObject);
    procedure lsbFonctionsDblClick(Sender: TObject);

    procedure cmbColEtiquettesChange(Sender: TObject);
    procedure cmbColXCibleChange(Sender: TObject);
    procedure cmbColXSourceChange(Sender: TObject);
    procedure cmbColYCibleChange(Sender: TObject);
    procedure cmbColYSourceChange(Sender: TObject);
    procedure cmbSystSourceJPCChange(Sender: TObject);
    procedure editExpressionKeyPress(Sender: TObject; var Key: char);
    procedure lsbLastCalculsDblClick(Sender: TObject);


    procedure PageControl1Change(Sender: TObject);
    procedure PairSplitter8ChangeBounds(Sender: TObject);
    procedure Panel6Resize(Sender: TObject);

    procedure SpeedButton24Click(Sender: TObject);
    // spécifique GHTopo
    ///////////////////////

    procedure btnMaillageImporterXYZClick(Sender: TObject);
    procedure btnSaveMaillageClick(Sender: TObject);
    procedure btnTriangulerClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    // Chemin du mailleur (programme EXE Triangle http://www.cs.cmu.edu/~quake/triangle.html)
    FMonMailleurExterne: TStringDirectoryFilename;

    // Pour traitement de fichiers textes séparés par espaces
    // Mettre cet objet en pseudo-global permet d'y faire des opérations
    // sans avoir à recharger le fichier
    {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    FDocuTopo  : TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FMyMaillage: TMaillage;
    FLesFichesTopo: TFichesTopo;
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
    //FContenuFichierTexte  : TStringList;
    FEvaluateurExpressions: TEvaluateurExpressions;
    FConversionUtils     : TConversionSysteme;
    FCalculateurDeclimag : TSAGeoMag;
    FConvertisseurActif       : boolean;
    FCalculateurDeclimagActif : boolean;
    FLigneDep                 : integer; // ligne de départ des données

    FColonneOSMLayer          : integer;
    FColonneEtiquette         : integer;
    FColonneDesignation       : integer;
    FColonneXSource           : integer;
    FColonneYSource           : integer;
    FColonneZSource           : integer;
    FColonneDescription       : integer;
    FColonneObservations      : integer;
    FColonneXCible            : integer;
    FColonneYCible            : integer;
    FColonnePhoto             : integer;

    // Couches additionnelles
    FOSMAdditionalLayers      : TListeOfGISLayers;

    procedure AjouterDerniereExpressionValable(const Expr: string);
    function CheckExternalPrograms(): boolean;
    function CheckLatLonInMetropole(const QLat, QLon: double): boolean;
    procedure ClearComboColonnes(const cmb: TComboBox; const Idx: integer);
    procedure DessinerCercleDeMohr(const S: TDGCSectionOfBeam);
    procedure DessinerSection(const S: TDGCSectionOfBeam);

    procedure ExportListePointsVersCarto(const FormatExportGIS     : TFormatExportGIS;
                                         const QFileName           : string;
                                         const FirstLineIsTitreCol : Boolean;
                                         const LabelsAutoIncrement : Boolean;
                                         const SystSource          : TLabelSystemesCoordsEPSG);

    function InitialiseEvaluateurExpressions(): boolean;
    function InitialiseCalculateurDeclimag(): boolean;
    function InitialiseConvertisseur(): boolean;
    procedure ListerCaracteristiquesSections(const S: TDGCSectionOfBeam);

    procedure RefactorerTitresDesColonnes();
    procedure RelisterAdditionalLayers();
    function  RemplirComboColonnes(const Cmb: TComboBox; const QLibColonne: string): integer;

    procedure RemplirLesComboboxDepuisGrille();
    procedure ResetGrdCoordsSection();
    procedure ResetOSMLayers();

    procedure SetFocusForPageIdx(const QActiveTabIndex: integer);
    procedure SetLabelsValeursDemandees(const Idx: integer);

    procedure FaireUnCalcul(const Expr: string);
    procedure ListerLesVariables();
    { private declarations }
    // bac à sable des couleurs
    procedure QProcTransmitCoords(const P: TDGCPoint2D);

    {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
    procedure GenererPagesPointsTopo(const QFileName: string; const QNb: integer);
    procedure ListerMaillage();
    {$ENDIF}
  public
    { public declarations }
    {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    function Initialiser(const QTabIndex: integer;
                         const QD: TToporobotStructure2012;
                         const QBDDEntites: TBDDEntites;
                         const QMaillage: TMaillage): boolean;
    {$ELSE}
    function Initialiser(const T: integer): boolean;
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
    procedure Finaliser();
    procedure SetTabIndex(const T: integer);
    procedure SetCoordonneesA(const P: TPoint2Df);
    procedure SetDefaultExpressionA(const S: string);
    function GetCoordonneesA(): TPoint2Df;
    function GetDeclimagA(): double;
    function GetResultatCalculA(): double;
    function GetCurrentCodeEPSGNomSysteme(): TLabelSystemesCoordsEPSG;


  end;
var
  CdrCalculette: TCdrCalculette;
implementation
{$R *.lfm}
uses
  DGCDummyUnit;



const
  TAB_INDEX_QRCODE = 3;
//******************************************************************************

function TCdrCalculette.CheckExternalPrograms(): boolean;
begin
  result := false;
end;



//******************************************************************************


//------------------------------------------------------------------------------
procedure TCdrCalculette.RemplirLesComboboxDepuisGrille();
begin
  FColonneOSMLayer     := RemplirComboColonnes(cmbColAdditionalOSMLayers   , 'Couche');
  FColonneEtiquette    := RemplirComboColonnes(cmbColEtiquettes            , 'Etiquette');
  FColonneDesignation  := RemplirComboColonnes(cmbColDesignation           , 'Designation');

  FColonneXSource      := RemplirComboColonnes(cmbColXSource               , 'X');
  FColonneYSource      := RemplirComboColonnes(cmbColYSource               , 'Y');
  FColonneZSource      := RemplirComboColonnes(cmbColZSource               , 'Z');

  FColonneDescription  := RemplirComboColonnes(cmbColDescription           , 'Description');
  FColonneObservations := RemplirComboColonnes(cmbColObservations          , 'Observations');

  FColonneXCible       := RemplirComboColonnes(cmbColXCible                , 'Lat');
  FColonneYCible       := RemplirComboColonnes(cmbColYCible                , 'Lon');
  FColonnePhoto        := RemplirComboColonnes(cmbColPhoto                 , 'Photo');
end;



{$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
function TCdrCalculette.Initialiser(const QTabIndex: integer;
                                    const QD: TToporobotStructure2012;
                                    const QBDDEntites: TBDDEntites;
                                    const QMaillage: TMaillage): boolean;
{$else}
function TCdrCalculette.Initialiser(const T: integer): boolean;
{$endif CALCULETTE_EMBEDDED_IN_GHTOPO}

var
  QActiveTabIndex: integer;
  StaticVarIdx: integer;
  i: Integer;
  WU: Boolean;
  procedure S777(const AC: TAction; const CaptionHint: string);
  begin
    AC.Caption := '';
    AC.Hint    := GetResourceString(CaptionHint);
  end;
begin
  Result := False;
  StaticVarIdx := 0;
  //try
    FLigneDep := 1;
    // Programmes externes
    FMonMailleurExterne := GetGHTopoDirectory() + 'triangulateur.exe';
    CheckExternalPrograms();
    // pages visibles
    for i := 0 to PageControl1.PageCount - 1 do PageControl1.Pages[i].TabVisible := true;
    //tabShtInerties.TabVisible := false;
    //tabShtDeclimag.TabVisible := True;

    PageControl1.ActivePageIndex := 0;
    {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
      FDocuTopo   := QD;
      FBDDEntites := QBDDEntites;
      FMyMaillage := QMaillage;
      FLesFichesTopo := nil;
      tabShtMNT.Visible            := true;
    {$else}
      tabShtMNT.TabVisible            := false;
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
    // Barre de pnlProgressionProcess;
    pnlProgressionProcess.Visible := false;
    lbProcessEnCours.Caption := '';
    ParametrerProgressbar(ProgressBar1, 0, 100, 0);
    // Liste des couches additionnelles
    FOSMAdditionalLayers := TListeOfGISLayers.Create;
    ResetOSMLayers();
    RelisterAdditionalLayers();
    // onglet évaluateur
    lbListeDesFonctions.Caption := GetResourceString(rsDLG_CALC_LSB_FUNCTIONS);
    lbListeCalculs.Caption      := GetResourceString(rsDLG_CALC_LSB_CALCULS);

    lbExpression.Caption        := GetResourceString(rsDLG_CALC_EXPRESSION);
    lbPromptExpression.Caption  := GetResourceString(rsDLG_CALC_EXPR);
    tabShtCalculatrice.Caption  := GetResourceString(rsDLG_CALC_TAB_CAL);
    tabShtCalculatrice.TabVisible := InitialiseEvaluateurExpressions();
    lbListeVariables.Caption      := GetResourceString(rsDLG_CALC_LSB_VARIABLES);

    // liste des variables (si utilisation de fpexprpars: activé, sinon désactivé et invisible)
    ListerLesVariables();
    // onglet convertisseur de coordonnées
    lbCopyrightNotesConvertisseur.Caption := GetResourceString(rsCOORDS_CONVERTER_AUTHOR);
    lbSystemesCoordonnees.Caption   := GetResourceString(rsDLG_CALC_SYSCOORDS);
    lbConversionsTableau.Caption    := GetResourceString(rsDLG_CALC_COORDS_TABLEAU);
    lbConversionsUnitaires.Caption  := GetResourceString(rsDLG_CALC_COORDS_UNITAIRE);
    lbSystSource.Caption            := GetResourceString(rsDLG_CALC_LB_SYST_SOURCE);
    lbSystCible.Caption             := GetResourceString(rsDLG_CALC_LB_SYST_CIBLE);
    tabShtConvertisseur.Caption     := GetResourceString(rsDLG_CALC_CDR_CONVERT);
    tabShtPascalScript.Caption      := GetResourceString(rsDLG_CALC_TAB_PASCAL_SCRIPT);
    S777(acConvertir               , rsDLG_CALC_BTN_CONVERT);
    S777(acEchangerValeurs         , rsDLG_CALC_BTN_ECHANGE);
    S777(acReverseSystCoordonnees  , rsDLG_CALC_BTN_ECHANGE);
    S777(acQRCodeListeCoords       , rsDLG_CALC_TAB_COORDS_QRCODE_LISTE);
    S777(acConvertir               , rsDLG_CALC_BTN_CONVERT);
    S777(acOuvrirCSV               , rsDLG_CALC_BTN_OPEN_CSV);
    S777(acExportCSV               , rsDLG_CALC_BTN_EXPORT_CSV);
    S777(acExportListePointsKML    , rsDLG_CALC_BTN_EXPORT_GIS);
    S777(acQRCodeCoordPoint        , rsDLG_CALC_BTN_QRCODE_PT);
    S777(acQRCodeListeCoords       , rsDLG_CALC_BTN_QRCODE_SEL);
    S777(acColler                  , rsDLG_CALC_BTN_PASTE);
    S777(acCopierTableau           , rsBTN_COPIER_TABLEAU);
    S777(acConvertirTableau        , rsDLG_CALC_BTN_CONVERT_TABLEAU);
    S777(acCopyCoordinates         , rsDLG_CALC_BTN_COPY_COORDS_ISOLEES);
    S777(acGenerateQRCodeFromTexte , rsDLG_CALC_BTN_QRCODE_FROM_TEXT);
    S777(acExportQRCodeToSVG       , rsDLG_CALC_BTN_QRCODE_TO_SVG);
    S777(acPSExecute               , rsDLG_CALC_BTN_PASCAL_SCRIPT_DO_RUN);
    // couches
    S777(acOSMRemoveAllLayers      , rsDLG_CALC_ACN_OSM_REMOVE_ALL_LAYERS);
    S777(acOSMAddLayer             , rsDLG_CALC_ACN_OSM_ADD_LAYER);
    S777(acOSMEditLayer            , rsDLG_CALC_ACN_OSM_EDIT_LAYER);
    S777(acOSMDeleteLayer          , rsDLG_CALC_ACN_OSM_REMOVE_LAYER);
    S777(acOSMExtractLayersFromTableau        , rsDLG_CALC_ACN_OSM_EXTRACT_LAYERS);
    s777(acCopyLayers              , rsDLG_CALC_ACN_OSM_COPY_LAYERS);
    s777(acPasteLayers             , rsDLG_CALC_ACN_OSM_PASTE_LAYERS);

    WU := InitialiseConvertisseur();
    tabShtConvertisseur.Enabled := WU;
    if (WU) then lbStatutConvertisseur.Color := clGreen;

    RemplirCombosSystemesCoordonnees(FConversionUtils, cmbSystSourceJPC, False, 3);
    RemplirCombosSystemesCoordonnees(FConversionUtils, cmbSystCibleJPC, False, 0);
    SetLabelsValeursDemandees(0);
    // onglet déclinaison magnétique
    lbDeclimagUnitaire.Caption      := GetResourceString(rsDLG_CALC_DECL_UNITAIRE);
    lbDeclimagTableau.Caption       := GetResourceString(rsDLG_CALC_DECL_TABLEAU);
    tabShtDeclimag.Caption          := GetResourceString(rsDLG_CALC_CDR_DECLIMAG);
    FCalculateurDeclimagActif       := InitialiseCalculateurDeclimag();
    tabShtDeclimag.Enabled          := FCalculateurDeclimagActif;
    if (FCalculateurDeclimagActif) then
    begin
      lbDescDeclimagCalculator.Caption := FCalculateurDeclimag.GetDescriptionDeclimag();
      lbDescDeclimagCalculator.Color   := clGreen;

    end;
    tabShtDeclimag.TabVisible := FCalculateurDeclimagActif;
    // onglet QRCode
    tabShtGenerateurQRCode.Caption  := GetResourceString(rsDLG_CALC_TAB_QRCODE);
    tabShtGenerateurQRCode.Enabled  := True;
    //----------------------
    editDateDeclimag.Date := Now();
    // grille de conversion en rafale
    lbDocumentTitle.caption  := GetResourceString(rsLB_DOCUMENT_TITLE);

    cmbFormatExport.Items.Clear;
    cmbFormatExport.Items.Add(GetResourceString(rsEXPORT_SIG_LEAFLET));
    cmbFormatExport.Items.Add(GetResourceString(rsEXPORT_SIG_GOOGLE_KML));
    cmbFormatExport.ItemIndex := 0;

    // la grille est éditable et on peut l'utiliser pour des conversions
    grdDonnees.Options := grdDonnees.Options + [goColSizing, goEditing];
    grdDonnees.ColCount :=  MAX_SIZE_PARAM_ARRAY;
    grdDonnees.RowCount := 20;
    // proposition d'un entete par défaut
    for i := 0 to grdDonnees.ColCount - 1 do grdDonnees.Cells[i, 0] := Format('Col %d', [i]);
    StaticVarIdx := 0; grdDonnees.Cells[ 0, 0] := '';

    FColonneOSMLayer         :=  1;  grdDonnees.Cells[ 1, 0] := 'Couche';
    FColonneEtiquette        :=  2;  grdDonnees.Cells[ 2, 0] := 'Etiquette';
    FColonneDesignation      :=  3;  grdDonnees.Cells[ 3, 0] := 'Designation';

    FColonneXSource          :=  4;  grdDonnees.Cells[ 4, 0] := 'X';
    FColonneYSource          :=  5;  grdDonnees.Cells[ 5, 0] := 'Y';
    FColonneZSource          :=  6;  grdDonnees.Cells[ 6, 0] := 'Z';

    FColonneDescription      :=  7;  grdDonnees.Cells[ 7, 0] := 'Description';
    FColonneObservations     :=  8;  grdDonnees.Cells[ 8, 0] := 'Observations';

    FColonneXCible           :=  9;  grdDonnees.Cells[ 9, 0] := 'Latitude';
    FColonneYCible           := 10;  grdDonnees.Cells[10, 0] := 'Longitude';
    FColonnePhoto            := 11;  grdDonnees.Cells[11, 0] := 'Photo';

    for i := 0 to grdDonnees.ColCount - 1 do grdDonnees.ColWidths[i] := 140;
    for i := 1 to grdDonnees.RowCount - 1 do grdDonnees.Cells[0,i] := IntToStr(i);

    RemplirLesComboboxDepuisGrille();


    // aide pour la grille de conversion
    lbHintGrdConversions.Caption := GetResourceString(rsDLG_CALC_HINT_GRD_CONVERSIONS);


    // caractéristiques de sections
    tabShtInerties.Caption       := GetResourceString(rsDLG_CALC_TAB_CARACTERISTIQUES_SECTIONS);

    ResetGrdCoordsSection();
    // Interpréteur PascalScript;
    {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}

    CdrPascalScript1.Initialiser(FEvaluateurExpressions,
                                 FDocuTopo,
                                 FBDDEntites,
                                 FMyMaillage);
    QActiveTabIndex              := QTabIndex;
    {$ELSE}
    CdrPascalScript1.Initialiser(FEvaluateurExpressions);
    CdrTextures1.Initialiser();
    QActiveTabIndex   := 0;
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
    PageControl1.ActivePageIndex := QActiveTabIndex;
    SetFocusForPageIdx(QActiveTabIndex);
    result := True;
  //except
  //end;
end;

 {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
procedure TCdrCalculette.ListerMaillage();
var
  NbVertex, i, NbTriangles: Integer;
  MyVertex: TMNTVertex;
  MyTriangle: TMNTTriangleABC;
begin
  AfficherMessage(Format('%s.ListerMaillage(%d)', [ClassName, FMyMaillage.GetNbVertex()]));
  NbVertex    := FMyMaillage.GetNbVertex();
  NbTriangles := FMyMaillage.GetNbTriangles();
  lbMaillageNbVertex.Caption    := format('%d vertex'   , [NbVertex]);
  lbMaillageNbTriangles.Caption := format('%d triangles', [NbTriangles]);

  grdMaillageCoordsXYZ.RowCount := 1 + NbVertex;
  grdMaillageCoordsXYZ.ColCount := 4;
  grdMaillageCoordsXYZ.ColWidths[0] := 50;
  for i := 1 to grdMaillageCoordsXYZ.ColCount - 1  do
  begin
    grdMaillageCoordsXYZ.Cells[i, 0]  := chr(87 + i);
    grdMaillageCoordsXYZ.ColWidths[i] := 110;
  end;

  grdMaillageTriangles.RowCount := 1 + NbTriangles;
  grdMaillageTriangles.ColCount := 4;
  grdMaillageTriangles.ColWidths[0] := 50;
  for i := 1 to grdMaillageTriangles.ColCount - 1  do
  begin
    grdMaillageTriangles.Cells[i, 0]  := chr(64 + i);
    grdMaillageTriangles.ColWidths[i] := 60;
  end;
  if (NbVertex > 0) then
  begin
    for i := 0 to NbVertex - 1 do
    begin
      MyVertex := FMyMaillage.GetVertex(i);
      grdMaillageCoordsXYZ.Cells[0, i+1] := format('%d', [i]);
      grdMaillageCoordsXYZ.Cells[1, i+1] := FormatterNombreOOo(MyVertex.X, 3);
      grdMaillageCoordsXYZ.Cells[2, i+1] := FormatterNombreOOo(MyVertex.Y, 3);
      grdMaillageCoordsXYZ.Cells[3, i+1] := FormatterNombreOOo(MyVertex.Z, 3);
    end;
  end;
  if (NbTriangles > 0) then
  begin
    for i := 0 to NbTriangles - 1 do
    begin
      MyTriangle := FMyMaillage.GetTriangle(i);
      grdMaillageTriangles.Cells[0, i+1] := format('%d', [i]);
      grdMaillageTriangles.Cells[1, i+1] := Format('%d', [MyTriangle.PointA]);
      grdMaillageTriangles.Cells[2, i+1] := Format('%d', [MyTriangle.PointB]);
      grdMaillageTriangles.Cells[3, i+1] := Format('%d', [MyTriangle.PointC]);
    end;
  end;
end;
{$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}

procedure TCdrCalculette.SetFocusForPageIdx(const QActiveTabIndex: integer);
begin
  (*
  case (QActiveTabIndex) of
    0: editExpression.SetFocus;
    1: editX_Source1JPC.SetFocus;
    2: editLatitude.SetFocus;
    3: editTextToEncodeQR.SetFocus;
    4: editPascalScript.SetFocus;
  end;
  //*)
end;

procedure TCdrCalculette.ListerLesVariables();
var
  n, i: Integer;
  V: TVariable;
begin
  AfficherMessage(format('%s.ListerLesVariables', [ClassName]));

  lsbVariables.Items.Clear;
  n := FEvaluateurExpressions.GetNbVariables();
  AfficherMessage(Format('%d variables', [n]));
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    V := FEvaluateurExpressions.GetVariable(i);
    lsbVariables.Items.add(Format('$%s = %s', [V.Nom, FloatToStr(V.Valeur)]));
  end;
  lsbVariables.ItemIndex := 0;
end;



procedure TCdrCalculette.SetTabIndex(const T: integer);
begin
  PageControl1.ActivePageIndex := T;
end;

procedure TCdrCalculette.Finaliser();
begin
  try
    FEvaluateurExpressions.Finaliser;
    FConversionUtils.Finaliser;
    FCalculateurDeclimag.Finaliser;
    //FContenuFichierTexte.Clear;
  finally
    FreeAndNil(FEvaluateurExpressions);//FEvaluateurExpressions.Free;
    FreeAndNil(FConversionUtils);//FConversionUtils.Free;
    FreeAndNil(FCalculateurDeclimag);//FCalculateurDeclimag.Free;
    //FreeAndNil(FContenuFichierTexte);//FContenuFichierTexte.Free;
  end;
end;



procedure TCdrCalculette.grdDonneesHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  showmessage(Format('%s: %d', [BoolToStr(IsColumn, 'Colonne', 'OVNI'), Index]));
end;

procedure TCdrCalculette.HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbAdditionalLayers.Invalidate;;
end;

procedure TCdrCalculette.Label26Click(Sender: TObject);
begin

end;

procedure TCdrCalculette.Label34Click(Sender: TObject);
begin

end;


procedure TCdrCalculette.lsbAdditionalLayersDblClick(Sender: TObject);
begin
  acOSMEditLayer.Execute;
end;

procedure TCdrCalculette.lsbAdditionalLayersDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  QLayer: TGISLayer;
  procedure DessineItem(const bg,tc: TColor);
  begin
    ResetColorRow(lsbAdditionalLayers, ARect, bg, tc);
    DrawColTexte(lsbAdditionalLayers, ARect, HeaderControl1.Sections.Items[0], False, Format(FORMAT_NB_INTEGER,[QLayer.SymboleStyle]));
    DrawColTexte(lsbAdditionalLayers, ARect, HeaderControl1.Sections.Items[1], True , Format(FORMAT_NB_REAL_2_DEC,[QLayer.SymbolSize]));
    DrawColRectColoreWithTexte(lsbAdditionalLayers, ARect, HeaderControl1.Sections.Items[2], True, bg, QLayer.SymbolColor, '');
    DrawColTexte(lsbAdditionalLayers, ARect, HeaderControl1.Sections.Items[3], True, QLayer.LayerTitle);
    DrawColTexte(lsbAdditionalLayers, ARect, HeaderControl1.Sections.Items[4], True, QLayer.LayerVarName);
    DrawColTexte(lsbAdditionalLayers, ARect, HeaderControl1.Sections.Items[5], True, QLayer.LayerDescription);
  end;
begin
  QLayer := FOSMAdditionalLayers.GetElement(Index);
  if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clWhite, clBlack);
end;

procedure TCdrCalculette.lsbFonctionsClick(Sender: TObject);
begin

end;

function TCdrCalculette.InitialiseEvaluateurExpressions(): boolean;
var
  i, n: Integer;
  EWE: String;
begin
  result := true;
  try
    lsbFonctions.Clear;
    FEvaluateurExpressions := TEvaluateurExpressions.Create;
    Result := FEvaluateurExpressions.Initialiser();
    n := FEvaluateurExpressions.GetNbreDeFonctions();
    AfficherMessage(format('%d functions', [n]));
    if (n > 0) then
    begin
      for i := 0 to n - 1 do
      begin
        EWE := FEvaluateurExpressions.GetNomFonction(i);
        lsbFonctions.Items.Add(EWE);
      end;
    end;
  except
  end;
end;

function TCdrCalculette.InitialiseConvertisseur(): boolean;
begin
  Result := False;
  try
    FConversionUtils := TConversionSysteme.Create;
    FConvertisseurActif := FConversionUtils.Initialiser;
    Result := True;
  except
  end;
end;

function TCdrCalculette.InitialiseCalculateurDeclimag(): boolean;
begin
  AfficherMessage('-- InitialiseCalculateurDeclimag');
  Result := False;
  FCalculateurDeclimag := TSAGeoMag.Create;
  try
    Result := FCalculateurDeclimag.Initialiser('');
  finally
  end;
end;
//******************************************************************************
procedure TCdrCalculette.SetLabelsValeursDemandees(const Idx: integer);
begin
  lbValeursX.Caption := 'X (m)';
  lbValeursY.Caption := 'Y (m)';
  if (Idx = 0) then
  begin
    lbValeursX.Caption := 'Lat. (Deg. dec.)';
    lbValeursY.Caption := 'Long. (Deg. dec.)';
  end;
end;





procedure TCdrCalculette.FaireUnCalcul(const Expr: string);
var
  Err: Integer;
  S: String;
  R: Double;
  DescErr: string;
begin
  Err := 0;
  S := Trim(Expr);
  R := FEvaluateurExpressions.Evaluate(S, Err, DescErr);
  if (Err = 0) then
  begin
    editExpression.Text    := FloatToStr(R);
    AjouterDerniereExpressionValable(Expr);
    lbErreurCalcul.Caption := 'OK';
    ListerLesVariables();
  end
  else
  begin
    editExpression.Text    := Expr;
    lbErreurCalcul.Caption := Format('Erreur %d - %s', [Err, DescErr]);
  end;
end;

procedure TCdrCalculette.AjouterDerniereExpressionValable(const Expr: string);
const
  NB_MAX_CALCULS = 500;
var
  Nb: Integer;
begin
  // dernier calcul en tête de liste
  lsbLastCalculs.Items.Insert(0, Expr);
  Nb := lsbLastCalculs.Count;
  if (Nb > NB_MAX_CALCULS) then lsbLastCalculs.Items.Delete(Nb - 1);
end;

function TCdrCalculette.RemplirComboColonnes(const Cmb: TComboBox; const QLibColonne: string): integer;
var
  qc: Integer;
  EWE: String;
begin
  Result := 0;
  if (QLibColonne =  '') then Exit(0);
  // nettoyer la liste et affecte des noms de colonne Colonne0, Colonne1, ...
  ClearComboColonnes(Cmb, 0);
  // FLigneDep > 0 = la ligne 0 du tableau contient les titres de colonnes
  if (FLigneDep > 0) then
  begin
    // Remplacer les items de combo par les noms de colonne du tableau
    for qc := 0 to grdDonnees.ColCount - 1 do
    begin
      EWE := Trim(grdDonnees.Cells[qc, 0]);
      if (EWE <> '') then cmb.Items[qc] := EWE;
    end;
    // rechercher les noms de colonnes contenant QLibColonne
    for qc := 0 to grdDonnees.ColCount - 1 do
    begin
      EWE := Lowercase(Trim(grdDonnees.Cells[qc, 0]));
      if (Pos(Lowercase(QLibColonne), EWE) > 0) then
      begin
        Result := qc;
        break;
      end;
    end;
    cmb.ItemIndex := Result;
  end;
end;
procedure TCdrCalculette.RefactorerTitresDesColonnes();
var
  NbC: Integer;
  i: Integer;
  EWE: String;
begin
  NbC := grdDonnees.ColCount;
  for i := 1 to NbC -1 do
  begin
    EWE := Trim(grdDonnees.Cells[i, 0]);
    if (EWE = '') then grdDonnees.Cells[i, 0] := Format('Colonne%d',[i]);
  end;
end;


procedure TCdrCalculette.ClearComboColonnes(const cmb: TComboBox; const Idx: integer);
var
  q: Integer;
begin
  cmb.Clear;
  for q := 0 to grdDonnees.ColCount - 1  do cmb.Items.add(format('Colonne_%d', [q]));
  cmb.ItemIndex := Idx;
end;

//******************************************************************************
procedure TCdrCalculette.SetDefaultExpressionA(const S: string);
begin
  editExpression.Text := Trim(S);
end;

function TCdrCalculette.GetResultatCalculA(): double;
begin
  Result := ConvertirEnNombreReel(trim(editExpression.Text), 0.00);
end;


function TCdrCalculette.GetDeclimagA(): double;
begin
  Result := editDeclinaison.Value;
end;

function TCdrCalculette.GetCoordonneesA(): TPoint2Df;
begin
  Result := MakeTPoint2Df(editX_Cible1JPC.Value, editY_Cible1JPC.Value);
end;

function TCdrCalculette.GetCurrentCodeEPSGNomSysteme(): TLabelSystemesCoordsEPSG;
begin
  Result := FConversionUtils.GetCodeEPSGNomSysteme(cmbSystCibleJPC.ItemIndex);
end;



procedure TCdrCalculette.SetCoordonneesA(const P: TPoint2Df);
begin
  editX_Source1JPC.Value := P.X;
  editY_Source1JPC.Value := P.Y;
end;
//******************************************************************************

function TCdrCalculette.CheckLatLonInMetropole(const QLat, QLon: double): boolean;
begin
  // Elle est comprise entre les latitudes 42°19'46" N et 51°5'47" N, ainsi que les longitudes 4°46' O et 8°14'42" E
  result := InRange(QLon, -5.0, 8.50) and InRange(QLat, 42.00, 51.50);
end;

procedure TCdrCalculette.ExportListePointsVersCarto(const FormatExportGIS: TFormatExportGIS;
                                                    const QFileName: string;
                                                    const FirstLineIsTitreCol: Boolean;
                                                    const LabelsAutoIncrement: Boolean;
                                                    const SystSource         : TLabelSystemesCoordsEPSG);
const
  KML_FOLDER_MARKERS = 'Marqueurs';
var
  KMLExport: TKMLExport;
  NoLigneDep: integer;
  Nb: Integer;
  procedure QExportKML();
  var
    i: integer;
    EWE: TLigneCoordonnees;
    P1, P2: TProjUV;
  begin
    KMLExport := TKMLExport.Create;
    try
      if (KMLExport.Initialiser(QFileName, Trim(editDocumentTitle.Text))) then
      begin
        pnlProgressionProcess.Visible := true;
        KMLExport.BeginFolder(2, KML_FOLDER_MARKERS);
        ProgressBar1.Min := NoLigneDep;
        ProgressBar1.Max := Nb - 1;

        for i := NoLigneDep to Nb - 1 do
        begin
          if ((i mod 10) = 0) then
          begin
            ProgressBar1.Position := i;
            lbProcessEnCours.caption := format('Export ligne %d', [i]);
          end;
          EWE.Etiquette            := IIF(LabelsAutoIncrement, Format('Label%d', [i]), Trim(grdDonnees.Cells[FColonneEtiquette, i]));
          EWE.OSMLayer             := Trim(grdDonnees.Cells[FColonneOSMLayer     , i]);
          EWE.Designation          := Trim(grdDonnees.Cells[FColonneDesignation  , i]);
          EWE.Description          := Trim(grdDonnees.Cells[FColonneDescription  , i]);
          EWE.Observations         := Trim(grdDonnees.Cells[FColonneObservations , i]);
          EWE.Photo                := Trim(grdDonnees.Cells[FColonnePhoto       , i]);
          P1.U := ConvertirEnNombreReel(grdDonnees.Cells[FColonneXSource, i], 0.00);
          P1.V := ConvertirEnNombreReel(grdDonnees.Cells[FColonneYSource, i], 0.00);
          P2 := FConversionUtils.ConversionSyst1ToSyst2EPSG(SystSource.CodeEPSG, CODE_EPSG_WGS84, P1);
          if (CheckLatLonInMetropole(P2.U, P2.V)) then
          begin
            EWE.XSource := P1.U;
            EWE.YSource := P1.V;
            EWE.ZSource := ConvertirEnNombreReel(grdDonnees.Cells[FColonneZSource, i], 0.00);;
            EWE.XCible  := P2.U;
            EWE.YCible  := P2.V;
            KMLExport.AddMarker( EWE.XSource, EWE.YSource, EWE.ZSource, EWE.XCible, EWE.YCible, EWE.Etiquette, EWE.Designation, EWE.Description, EWE.Observations, EWE.Photo);
          end;
        end; // for i := NoLigneDep to Nb - 1 do
        KMLExport.EndFolder(2, KML_FOLDER_MARKERS);
        KMLExport.Finaliser();
      end; // if (KMLExport.Initialiser(QFileName)) then
    finally
      FreeAndNil(KMLExport);
      lbProcessEnCours.Caption := rsDONE_ANY_PROCESS;
      pnlProgressionProcess.Visible := false;
    end;
  end;  // procedure QExportKML();
  procedure QExportOSM();
  var
    OSMExport: TLeafletExport;
    EWE: TLigneCoordonnees;
    P1, P2: TProjUV;
    i: Integer;
    QColor: TColor;
    QGisLayer: TGISLayer;
    WU: String;
  begin
    Randomize;
    OSMExport := TLeafletExport.Create;
    try
      pnlProgressionProcess.Visible := True;
      P1.U := ConvertirEnNombreReel(grdDonnees.Cells[FColonneXSource, NoLigneDep], 0.00);
      P1.V := ConvertirEnNombreReel(grdDonnees.Cells[FColonneYSource, NoLigneDep], 0.00);
      P2 := FConversionUtils.ConversionSyst1ToSyst2EPSG(SystSource.CodeEPSG, CODE_EPSG_WGS84, P1);
      if (OSMExport.Initialiser(QFileName,
                                Trim(editDocumentTitle.Text),
                                78, Screen.Height - 220, P2.U, P2.V, false)) then
      begin
        ProgressBar1.Min := NoLigneDep;
        ProgressBar1.Max := Nb - 1;
        // AddLayer doit être appelé avant WriteHeader

        OSMExport.AddLayer(false, True, 'PonctualPlacemarks', 'Entrances or placemarks', '', '', OSM_MARKER_STYLE_CIRCLE, 10.00, clRed, 0.50);
        for i := 0 to FOSMAdditionalLayers.Count - 1 do
        begin
          QGisLayer := FOSMAdditionalLayers.GetElement(i);
          OSMExport.AddLayer(false, true, QGisLayer.LayerVarName, QGisLayer.LayerTitle, QGisLayer.LayerAttribution, '', QGisLayer.SymboleStyle, QGisLayer.SymbolSize, QGisLayer.SymbolColor, QGisLayer.SymbolOpacity / 256);
        end;
        OSMExport.WriteHeader();
        OSMExport.BeginConditionalSection(True);
          for i := NoLigneDep to Nb - 1 do
          begin
            if ((i mod 10) = 0) then
            begin
              ProgressBar1.Position := i;
              lbProcessEnCours.caption := format('Export ligne %d', [i]);
            end;
            EWE.Etiquette       := IIF(LabelsAutoIncrement, Format('Label%d', [i]), Trim(grdDonnees.Cells[FColonneEtiquette, i]));
            EWE.Designation     := OSMExport.RealEscapeString(Trim(grdDonnees.Cells[FColonneDesignation , i]));
            EWE.Description     := OSMExport.RealEscapeString(Trim(grdDonnees.Cells[FColonneDescription , i]));
            EWE.Observations    := OSMExport.RealEscapeString(Trim(grdDonnees.Cells[FColonneObservations, i]));
            EWE.Photo           := OSMExport.RealEscapeString(Trim(grdDonnees.Cells[FColonnePhoto       , i]));
            P1.U := ConvertirEnNombreReel(grdDonnees.Cells[FColonneXSource, i], 0.00);
            P1.V := ConvertirEnNombreReel(grdDonnees.Cells[FColonneYSource, i], 0.00);
            P2 := FConversionUtils.ConversionSyst1ToSyst2EPSG(SystSource.CodeEPSG, CODE_EPSG_WGS84, P1);
            if (CheckLatLonInMetropole(P2.U, P2.V) AND CheckOSMLayerVarName(grdDonnees.Cells[FColonneOSMLayer, i], EWE.OSMLayer)) then
            begin
              EWE.XSource := P1.U;
              EWE.YSource := P1.V;
              EWE.ZSource := ConvertirEnNombreReel(grdDonnees.Cells[FColonneZSource, i], 0.00);;
              EWE.XCible  := P2.U;
              EWE.YCible  := P2.V;
              AfficherMessageErreur(Format('AddMarker: %s', [EWE.OSMLayer]));
              OSMExport.AddMarker(EWE.OSMLayer,
                                  EWE.XSource,
                                  EWE.YSource,
                                  EWE.ZSource,
                                  EWE.XCible, EWE.YCible,
                                  EWE.Etiquette, EWE.Designation,
                                  EWE.Description, EWE.Observations,
                                  EWE.Photo);
            end;
          end;
          OSMExport.FlushAllMarkers();
        OSMExport.EndConditionalSection();
        OSMExport.WriteFooter();
        OSMExport.Finaliser();
      end;
    finally
      FreeAndNil(OSMExport);
      lbProcessEnCours.Caption := rsDONE_ANY_PROCESS;
      pnlProgressionProcess.Visible := false;
    end;
  end;
begin
  // première ligne
  NoLigneDep := IIF(FirstLineIsTitreCol, 1, 0);
  Nb := grdDonnees.RowCount - 1;
  case FormatExportGIS of
    gisOSM: QExportOSM();
    gisKML: QExportKML();
  end;
end;


procedure TCdrCalculette.editExpressionKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) then
  begin
    FaireUnCalcul(editExpression.Text);
    editExpression.SelStart := length(editExpression.Text);
    editExpression.SelLength:= 0;
  end;
end;


procedure TCdrCalculette.lsbLastCalculsDblClick(Sender: TObject);
var
  idx, l: Integer;
  WU, EWE: String;
begin
  // rappel d'un calcul: recopie l'expression dans la zone de texte
  idx := lsbLastCalculs.ItemIndex;
  if (idx >=0) then
  begin
    EWE := lsbLastCalculs.Items[idx];
    // possibilité d'ajout: si un underscore est ajouté en fin de la formule courante,
    // on ajoute la ligne sélectionnée à la formule
    WU := Trim(editExpression.Text);
    l  := length(WU);
    if (WU[l] = '_') then
    begin
      System.Delete(WU, l, 1); // on efface le caractère de continuation
      WU += EWE;               // on ajoute à l'expression existante
    end
    else
    begin
      WU := EWE;               // sinon, on remplace l'expression
    end;
    editExpression.Text := WU;
  end;
end;





procedure TCdrCalculette.PageControl1Change(Sender: TObject);
begin
  (*
  case PageControl1.ActivePageIndex of
    0: editExpression.SetFocus;                // calculette
    1: editX_Source1JPC.SetFocus;              // convertisseur
    2: editLatitude.SetFocus;                  // déclimag
    3: editTextToEncodeQR.SetFocus;            // QRCodes
    4: editPascalScript.SetFocus;              // Interpréteur Pascal
  end;
  //*)
end;

procedure TCdrCalculette.PairSplitter8ChangeBounds(Sender: TObject);
begin

end;

procedure TCdrCalculette.Panel6Resize(Sender: TObject);
var
  CmbColCoordsXPosLeft, WidthCmbCoords, CmbColCoordsYPosLeft: Integer;
begin
  CmbColCoordsXPosLeft := cmbColXSource.Left;
  WidthCmbCoords := (Panel6.Width - CmbColCoordsXPosLeft) div 2 - 8;
  CmbColCoordsYPosLeft :=  CmbColCoordsXPosLeft + WidthCmbCoords + 8;
  cmbColXSource.Left := CmbColCoordsXPosLeft;
  cmbColXCible.Left  := CmbColCoordsXPosLeft;

  cmbColYSource.Left  := CmbColCoordsYPosLeft;
  cmbColYCible.Left   := CmbColCoordsYPosLeft;

  cmbColYSource.Width := WidthCmbCoords;
  cmbColXSource.Width := WidthCmbCoords;
  cmbColYSource.Width := WidthCmbCoords;
  cmbColXCible.Width  := WidthCmbCoords;
  cmbColYCible.Width  := WidthCmbCoords;
end;







procedure TCdrCalculette.QProcTransmitCoords(const P: TDGCPoint2D);
begin
  showmessagefmt('%f %f', [P.X, P.Y]);
end;



procedure TCdrCalculette.SpeedButton24Click(Sender: TObject);
begin
  editTextToEncodeQR.CopyToClipboard;
end;

procedure TCdrCalculette.acGrdDonneesRemoveLigneExecute(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Supprimer cette ligne')) then grdDonnees.DeleteRow(grdDonnees.Row);
end;



procedure TCdrCalculette.acReverseSystCoordonneesExecute(Sender: TObject);
var
  toto: Integer;
begin
  toto := cmbSystSourceJPC.ItemIndex;
  cmbSystSourceJPC.ItemIndex := cmbSystCibleJPC.ItemIndex;
  cmbSystCibleJPC.ItemIndex  := toto;
end;

procedure TCdrCalculette.acSavePSScriptExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QIdx: integer;
begin

end;

procedure TCdrCalculette.acVisuCoordsSurCarteExecute(Sender: TObject);
const
  MAIN_LAYER = 'PonctualPlacemarks';
var
  MyCodeEPSG: TLabelSystemesCoordsEPSG;
  P1, P2: TProjUV;
  OSMExport: TLeafletExport;
  QFileName: TStringDirectoryFilename;
  EWE: TLigneCoordonnees;
  WU_Titre: String;
begin
  MyCodeEPSG:= FConversionUtils.GetCodeEPSGNomSysteme(cmbSystCibleJPC.ItemIndex);
  P1.U := editX_Cible1JPC.Value;
  P1.V := editY_Cible1JPC.Value;

  P2 := FConversionUtils.ConversionSyst1ToSyst2EPSG(MyCodeEPSG.CodeEPSG, CODE_EPSG_WGS84, P1);
  //showmessagefmt('Lat = %.8f, Lon = %.8f', [P2.U, P2.V]);
  // créer un miniLeaflet
  QFileName := GetGHTopoDirectory() + '_MyPoint.htm';
  OSMExport := TLeafletExport.Create;
  try
    WU_Titre := Format('Localisation point %s, %s', [FormatterNombreAvecSepMilliers(P1.U, 0), FormatterNombreAvecSepMilliers(P1.V, 0)]);
    if (OSMExport.Initialiser(QFileName, WU_Titre, 78, Screen.Height - 220, P2.U, P2.V, false)) then
    begin
      // AddLayer doit être appelé avant WriteHeader
      OSMExport.AddLayer(false, True, MAIN_LAYER, '', '', '', OSM_MARKER_STYLE_DEFAULT, 10.00, clRed, 128);
      OSMExport.WriteHeader();
      OSMExport.BeginConditionalSection(True);
        EWE.OSMLayer        := MAIN_LAYER;
        EWE.Etiquette       := '1 - MyPoint';
        EWE.Designation     := '';
        EWE.Description     := '';
        EWE.Observations    := '';
        EWE.Photo           := '';
        if (CheckLatLonInMetropole(P2.U, P2.V))  then
        begin
          EWE.XSource := P1.U;
          EWE.YSource := P1.V;
          EWE.ZSource := 0.00;
          EWE.XCible  := P2.U;
          EWE.YCible  := P2.V;
          OSMExport.AddMarker(EWE.OSMLayer,
                              EWE.XSource,
                              EWE.YSource,
                              EWE.ZSource,
                              EWE.XCible, EWE.YCible,
                              EWE.Etiquette, EWE.Designation,
                              EWE.Description, EWE.Observations,
                              EWE.Photo);
        end;
        OSMExport.FlushAllMarkers();
      OSMExport.EndConditionalSection();
      OSMExport.WriteFooter();
      OSMExport.Finaliser();
      ShowMessageFmt('Ouvrir le fichier %s dans votre navigateur', [QFileName]);
    end;
  finally
    FreeAndNil(OSMExport);
  end;
end;

procedure TCdrCalculette.btnCreateGUIDClick(Sender: TObject);
begin
  editGUID.Text := GetGUID();
end;


procedure TCdrCalculette.btnPasteClick(Sender: TObject);
begin
  editTextToEncodeQR.PasteFromClipboard;
end;





procedure TCdrCalculette.acGrdDonneesRemoveColonneExecute(Sender: TObject);
begin
 if (GHTopoQuestionOuiNon('Supprimer cette colonne')) then grdDonnees.DeleteCol(grdDonnees.Col);
end;


procedure TCdrCalculette.acCalculerExecute(Sender: TObject);
begin
  FaireUnCalcul(editExpression.Text);
end;

procedure TCdrCalculette.acCalculDeclimagExecute(Sender: TObject);
begin
  editDeclinaison.Value := FCalculateurDeclimag.CalculerDeclinaison(editLatitude.Value,
                                                                    editLongitude.Value,
                                                                    editAltitude.Value,
                                                                    editDateDeclimag.Date);
end;

procedure TCdrCalculette.acCalculDeclimagTableauExecute(Sender: TObject);
var
  NbValeurs: Integer;
  AnneeCourante: LongInt;
  i: Integer;
  EWE: TDateTime;
  Decl: Double;
begin
  if (not FCalculateurDeclimagActif) then Exit;
  try
    i := editAnneeArrivee.AsInteger - editAnneeDepart.AsInteger;
    NbValeurs := i + 1;
    AnneeCourante := editAnneeDepart.AsInteger;
    if (i < 0) then
    begin
       ShowMessage('L''année de fin doit être supérieure à l''année de début');
       Exit;
    end;
    with grdDeclinaisons do
    begin
      ColCount     := 2;
      RowCount     := 1 + NbValeurs;
      Cells[0, 0]  := 'Année';
      Cells[1, 0]  := 'Déclinaison';
      ColWidths[0] := 70;
      ColWidths[1] := 100;
      for i := 1 to NbValeurs do
      begin
        EWE := GetSecuredDate(AnneeCourante, 1, 1);
        Cells[0,i] := Format(FORMAT_NB_INTEGER, [AnneeCourante]);
        Decl := FCalculateurDeclimag.CalculerDeclinaison(editLongitude.Value,
                                                         editLatitude.Value,
                                                         editAltitude.Value,
                                                         EWE);
        Cells[1, i] := Format('%.4f', [Decl]);
        Inc(AnneeCourante, 1);
      end;
    end;

  except

  end;
  //*)
end;

procedure TCdrCalculette.acCollerExecute(Sender: TObject);
var
  EWE: char;
begin
  AfficherMessage(Format('%s.Coller', [self.ClassName]));
  //try
    EWE := ChooseChar(cmbSeparateur.ItemIndex, [#9, ';', ',', ':', '|']);
    //AfficherMessage(Format(FORMAT_NB_INTEGER, [000]));
    // \t Tabulation; Point virgule, Virgule: Deux points| Barre
    GRDCollerDepuisClipBoard(grdDonnees, chkPremLigneIsTitres.Checked, EWE);
    //AfficherMessage(Format(FORMAT_NB_INTEGER, [001]));
    // A partir d'ici, on n'a plus besoin du presse-papier ni de la liste provisoire
    // On travaille directement sur la grille
    // garnir les combobox
    RemplirLesComboboxDepuisGrille();
    RefactorerTitresDesColonnes();
    //AfficherMessage(Format(FORMAT_NB_INTEGER, [004]));
  //except
  //end;
end;

procedure TCdrCalculette.acConvertirExecute(Sender: TObject);
var
  P1, P2: TProjUV;
begin
  // si syst source = syst cible --> []
  if (cmbSystSourceJPC.ItemIndex = cmbSystCibleJPC.ItemIndex) then Exit;
  try
    P1.U := editX_Source1JPC.Value;
    P1.V := editY_Source1JPC.Value;
  except
    P1.U := DegMinSec2DegDec(editX_Source1JPC.Text);
    P1.V := DegMinSec2DegDec(editY_Source1JPC.Text);
  end;
  P2 := FConversionUtils.ConversionSyst1ToSyst2Index(cmbSystSourceJPC.ItemIndex,
                                                     cmbSystCibleJPC.ItemIndex,
                                                     P1);
  editX_Cible1JPC.Value := P2.U;
  editY_Cible1JPC.Value := P2.V;
  // pour les cases Long et Lat du calcul de déclinaison
  editLongitude.Value:= P2.U;
  editLatitude.Value := P2.V;
end;

procedure TCdrCalculette.acConvertirTableauExecute(Sender: TObject);
var
  P1, P2: TProjUV;
  i: Integer;
  WU: Boolean;
begin
  if (cmbSystSourceJPC.ItemIndex = cmbSystCibleJPC.ItemIndex) then
  begin
    ShowMessage('Les systèmes de coordonnées source et cible sont identiques');
    Exit;
  end;

  // récupération des index de colonnes
  FColonneXSource := cmbColXSource.ItemIndex;
  FColonneYSource := cmbColYSource.ItemIndex;
  FColonneZSource := cmbColZSource.ItemIndex;


  FColonneXCible  := cmbColXCible.ItemIndex;
  FColonneYCible  := cmbColYCible.ItemIndex;
  FColonnePhoto   := cmbColPhoto.ItemIndex;
  WU := (FColonneXSource = FColonneXCible) OR // colonne sources et destination identiques
        (FColonneYSource = FColonneYCible) OR // colonne sources et destination identiques
        (FColonneXSource = FColonneYCible) OR
        (FColonneYSource = FColonneXCible) OR
        (FColonneXSource = FColonneYSource) OR // colonne XY sources identiques
        (FColonneXCible  = FColonneYCible);     // colonne XY sources identiques
  if (WU) then
  begin
    ShowMessageFmt('Les colonnes %d, %d, %d, %d se marchent dessus', [FColonneXSource, FColonneXCible,FColonneYSource,FColonneYCible]);
    Exit;
  end;
  FLigneDep := IIF(chkPremLigneIsTitres.Checked, 1, 0);
  for i := FLigneDep to grdDonnees.RowCount - 1 do
  begin
    try
      P1.U := ConvertirEnNombreReel(grdDonnees.Cells[FColonneXSource, i], 0.00);
      P1.V := ConvertirEnNombreReel(grdDonnees.Cells[FColonneYSource, i], 0.00);
      P2   := FConversionUtils.ConversionSyst1ToSyst2Index(cmbSystSourceJPC.ItemIndex, cmbSystCibleJPC.ItemIndex, P1);
      grdDonnees.Cells[FColonneXCible, i] := FormatterNombreOOo(P2.U, 8, false);
      grdDonnees.Cells[FColonneYCible, i] := FormatterNombreOOo(P2.V, 8, false);
    except
      grdDonnees.Cells[FColonneXCible, i] := 'Err. X';
      grdDonnees.Cells[FColonneYCible, i] := 'Err. Y';
    end;
  end;
end;

procedure TCdrCalculette.acCopierTableauExecute(Sender: TObject);
begin
  grdDonnees.CopyToClipboard();
end;

procedure TCdrCalculette.acCopyCoordinatesExecute(Sender: TObject);
const
  QSep = ';';
var
  EWE: String;
  MyClipBoard: TClipboard;
begin
  MyClipBoard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    // contenu
    EWE := editX_Source1JPC.Text + QSep +
           editY_Source1JPC.Text + QSep +
           editX_Cible1JPC.Text  + QSep +
           editY_Cible1JPC.Text;
    MyClipBoard.AsText := EWE;
  finally
    FreeAndNil(MyClipBoard);//MyClipBoard.Free;
  end;
end;

procedure TCdrCalculette.acCopyLayersExecute(Sender: TObject);
var
  MyClipBoard: TClipboard;
  i, NbLayers: Integer;
  EWE: String;
  MyLayer: TGISLayer;
begin
  NbLayers := FOSMAdditionalLayers.GetNbElements();
  if (0 = NbLayers) then exit;
  MyClipBoard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    // contenu
    EWE := Format('Layers of "%s" (%d layers)' + #13#10, [editDocumentTitle.Text, NbLayers]);
    EWE += 'ID'               + TAB +
           'LayerVarName'     + TAB +
           'LayerTitle'       + TAB +
           'SymboleStyle'     + TAB +
           'SymbolSize'       + TAB +
           'SymbolColor'      + TAB +
           'SymbolOpacity'    + TAB +
           'LayerDescription' + TAB +
           'LayerAttribution' + TAB +
           #13#10;
    for i := 0 to NbLayers - 1 do
    begin
      MyLayer := FOSMAdditionalLayers.GetElement(i);
      EWE += Format('%d' + #9 +         // ID
                    '%s' + #9 +         // LayerVarName    : string;
                    '%s' + #9 +         // LayerTitle      : string;
                    '%d' + #9 +         // SymboleStyle: integer;
                    '%s' + #9 +       // SymbolSize : double;
                    '%d' + #9 +         // SymbolColor: TColor;
                    '%d' + #9 +         // SymbolOpacity      : byte;
                    '%s' + #9 +         // LayerDescription: string;
                    '%s' + #9 +         // LayerAttribution        : string;
                    #13#10,
                    [i,
                     MyLayer.LayerVarName,
                     MyLayer.LayerTitle,
                     MyLayer.SymboleStyle,
                     FormatterNombreOOo(MyLayer.SymbolSize, 2),
                     MyLayer.SymbolColor,
                     MyLayer.SymbolOpacity,
                     MyLayer.LayerDescription,
                     MyLayer.LayerAttribution
                    ]);
    end;
    MyClipBoard.AsText := EWE;
  finally
    FreeAndNil(MyClipBoard);//MyClipBoard.Free;
  end;
end;
procedure TCdrCalculette.acPasteLayersExecute(Sender: TObject);
var
  MyClipboard: TClipboard;
  ValuesArray: TStringList;
  Nb, i      : Integer;
  EWE        : TGHStringArray;
  MyLayer    : TGISLayer;
begin
  if (not GHTopoQuestionOuiNon('Le presse-papiers doit contenir des couches valides - Continuer')) then exit;
  MyClipBoard := TClipboard.Create(ctClipboard);
  ValuesArray := TStringList.Create;
  try
   ValuesArray.Clear;
   if (MyClipBoard.HasFormat(CF_TEXT)) then
   begin
     SplitToTStringList(MyClipboard.AsText, #10, ValuesArray, True, dupIgnore);
     Nb := ValuesArray.Count;
     showmessage(inttostr(Nb));
     if (Nb = 0) then exit;
     // affichage dans le tableau
     for i := 0 to Nb - 1 do
     begin
       EWE := Split(Trim(ValuesArray[i]), TAB);
       // ID	LayerVarName	LayerTitle	SymboleStyle	SymbolSize	SymbolColor	SymbolOpacity	LayerDescription	LayerAttribution
       // 0    1         2          3   4         5          6      7      8
       // 0	Markers0	Couche00	0	10.00	16711680	255
       MyLayer.LayerVarName     := Trim(EWE[1]);
       MyLayer.LayerTitle       := Trim(EWE[2]);
       if (('' = MyLayer.LayerVarName) or ('' = MyLayer.LayerTitle)) then continue;
       MyLayer.SymboleStyle     := StrToIntDef(EWE[3], 0);
       MyLayer.SymbolSize       := ConvertirEnNombreReel(EWE[4], 10.00);
       MyLayer.SymbolColor      := TColor(StrToIntDef(EWE[5], $FF0000));
       MyLayer.SymbolOpacity    := StrToIntDef(EWE[6], 255);
       MyLayer.LayerDescription := Trim(EWE[7]);
       MyLayer.LayerAttribution := Trim(EWE[8]);
       FOSMAdditionalLayers.AddElement(MyLayer);
     end;
    end else
    begin
      ShowMessage('Presse-papiers vide ou invalide');
    end;
    ValuesArray.Clear;
    RelisterAdditionalLayers();
  finally
    FreeAndNil(ValuesArray);
    FreeAndNil(MyClipBoard);
  end;
end;

procedure TCdrCalculette.acCopyTableauDeclimagExecute(Sender: TObject);
begin
  grdDeclinaisons.CopyToClipboard();
end;

procedure TCdrCalculette.acEchangerValeursExecute(Sender: TObject);
var
  EWE: double;
begin
  EWE := editX_Source1JPC.Value;
  editX_Source1JPC.Value := editX_Cible1JPC.Value;
  editX_Cible1JPC.Value  := EWE;
  EWE := editY_Source1JPC.Value;
  editY_Source1JPC.Value := editY_Cible1JPC.Value;
  editY_Cible1JPC.Value  := EWE;
end;

procedure TCdrCalculette.acExportCSVExecute(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFilename := 'Tableau1.csv';
  if (DoDialogSaveFile('Fichier CSV (*.csv)|*.csv', '.csv', QFilename, QFilterIndex)) then
  begin
    grdDonnees.SaveToCSVFile(QFilename, SEPARATOR_TAB, chkPremLigneIsTitres.Checked);
  end;
end;



procedure TCdrCalculette.acExportListePointsKMLExecute(Sender: TObject);
var
  QFilters: String;
  QFileName: TStringDirectoryFilename;
  QIdxFilter: integer;
  MyCodeEPSG: TLabelSystemesCoordsEPSG;
begin
  if (not GHTopoQuestionOuiNon('Utilisation: Sélectionner les colonnes contenant l''étiquette et les coordonnnées' + #13#10 +
                         Format('Colonne pour X: %s', [cmbColXSource.Text]) + #13#10 +
                         Format('Colonne pour Y: %s', [cmbColYSource.Text]) + #13#10 +
                         Format('Colonne pour Z: %s', [cmbColZSource.Text]) + #13#10 +
                         Format('Colonne pour étiquette: %s', [cmbColEtiquettes.Text]) + #13#10 +
                         'Continuer')) then Exit;

  // Avertissement: Régénération des couches
  case MessageDlg(GetResourceString(rsMSG_REGEN_ADDITIONAL_LAYER), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
    mrYes    : acOSMExtractLayersFromTableauExecute(Self);
    mrNo     : pass;
    mrCancel : Exit;
  end;
  QIdxFilter := 0;
  case cmbFormatExport.ItemIndex of
    0: QFilters := 'Cartes OSM Leaflet (*.htm)|*.htm';
    1: QFilters := 'Fichiers Google Earth (*.kml)|*.kml';
  end;


  if (DoDialogSaveFile(QFilters, IIF(0 = cmbFormatExport.ItemIndex, '*.htm', '*.kml'), QFileName, QIdxFilter)) then
  begin
    FColonneOSMLayer       := cmbColAdditionalOSMLayers.ItemIndex;
    FColonneEtiquette      := cmbColEtiquettes.ItemIndex;
    FColonneDesignation    := cmbColDesignation.ItemIndex;
    FColonneXSource        := cmbColXSource.ItemIndex;
    FColonneYSource        := cmbColYSource.ItemIndex;
    FColonneZSource        := cmbColZSource.ItemIndex;
    FColonneDescription    := cmbColDescription.ItemIndex;
    FColonneObservations   := cmbColObservations.ItemIndex;





    MyCodeEPSG:= FConversionUtils.GetCodeEPSGNomSysteme(cmbSystSourceJPC.ItemIndex);
    ExportListePointsVersCarto(TFormatExportGIS(cmbFormatExport.ItemIndex),
                               QFileName,
                               chkPremLigneIsTitres.Checked,
                               chkAutoIncrement.Checked,
                               MyCodeEPSG);
    ShowMessage(GetResourceString(rsDONE_ANY_PROCESS));
  end;
end;


procedure TCdrCalculette.acExportQRCodeToSVGExecute(Sender: TObject);
const
  CoteCarre = 05;
  NOM_STYLE_CADRE       = 'Cadre';
  NOM_STYLE_CARRE_NOIR  = 'CarreNoir';
  NOM_STYLE_CARRE_BLANC = 'CarreBlanc';
var
  SVGCanvas: TSVGCanvas;
  QFileName: TStringDirectoryFilename;
  NbCarres: Integer;
  L, H: Integer;
  QRMatrix: TMatriceResultat;
  i, j: Integer;
  EWE: String;
  QFilterIndex: integer;
begin
  if (Not DoDialogSaveFile('Fichiers SVG |*.svg', '.svg', QFileName, QFilterIndex)) then Exit;
  VueQRCode.DoMonochrome := FALSE;  // ne pas utiliser le mode monochrome
  VueQRCode.Text := editTextToEncodeQR.Text;
  VueQRCode.Generate;
  SVGCanvas := TSVGCanvas.Create;
  try
    NbCarres := VueQRCode.GetNbQRTaille;
    // en-tetes
    SVGCanvas.Commentaire := '';
    SVGCanvas.Scale       := 1.00;
    // maxi et mini
    L := CoteCarre * NbCarres;
    H := L;
    if (Not SVGCanvas.InitializeDocument(QFileName,
                                         False,
                                         'TextEncoded ' + VueQRCode.Text,
                                         Format('%s: QRCode', [ExtractFileName(QFileName)]),
                                         0, 0, L, H,
                                         nil)) then Exit;
    // patterns
    SVGCanvas.BeginPatternsSection;
      // les patterns ici
    SVGCanvas.EndPatternsSection;
    // section de styles
    SVGCanvas.BeginStylesSection;
      // styles de cube englobant
      SVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_CADRE, clGray, 255, 0.1, psSolid, clWhite, 128, bsClear, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'cadre');
      // style par défaut: gris
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_00' , clWhite, 255, 0.1, psSolid, clWhite, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'data: Blanc');
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_01' , clBlack, 255, 0.1, psSolid, clBlack, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'data: Noir');
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_16' , clWhite, 255, 0.1, psSolid, clWhite, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'carres de controle (zones blanches)');
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_17' , clBlack, 255, 0.1, psSolid, clBlack, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'carres de controle (zones noires)');
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_20' , clBlack, 255, 0.1, psSolid, clBlack, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'carre en M[8,8]: noir');
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_21' , clBlack, 255, 0.1, psSolid, clBlack, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'carre en M[8,8]: noir');
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_32' , clWhite, 255, 0.1, psSolid, clWhite, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'zones de detrompage: blanc');
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_33' , clBlack, 255, 0.1, psSolid, clBlack, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'zones de detrompage: noir');
      SVGCanvas.WriteStyleLinePolygoneTexte('Carre_65' , clBlack, 255, 0.1, psSolid, clBlack, 128, bsSolid, DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], 'carres en M[6, 8]');
    SVGCanvas.EndStylesSection;
    // dessin
    SVGCanvas.BeginLayer('Principal');
      SVGCanvas.BeginGroupe('QRCode', '', 10.00, 10.00);
        QRMatrix := VueQRCode.GetMatrixQRResultat;
        for i := 0 to NbCarres -1 do
          for j := 0 to NbCarres -1 do
          begin
            SVGCanvas.DrawRectangle(Format('Carre_%.2d', [QRMatrix[i,j]]),
                                    CoteCarre * i    , H - CoteCarre * j,
                                    CoteCarre * (i+1), H - CoteCarre * (j+1));
          end;
      SVGCanvas.EndGroupe('QRCode');
    SVGCanvas.EndLayer('Principal');
    // cloture
    SVGCanvas.FinalizeDocument;
  finally
    FreeAndNil(SVGCanvas);
  end;
end;

procedure TCdrCalculette.acGenerateQRCodeFromTexteExecute(Sender: TObject);
const MARKER_GUID = '$guid:';
var
  EWE: TCaption;
begin
  EWE := Trim(editTextToEncodeQR.Text);
  EWE := StringReplace(EWE, MARKER_GUID, GetGUID(), [rfReplaceAll, rfIgnoreCase]);
  VueQRCode.Text := EWE;
  VueQRCode.Generate;
end;

procedure TCdrCalculette.acOSMEditLayerExecute(Sender: TObject);
var
  MyLayer: TGISLayer;
begin
  if (lsbAdditionalLayers.Count = 0) then Exit;
  MyLayer := FOSMAdditionalLayers.GetElement(lsbAdditionalLayers.ItemIndex);
  if (EditerGisLayer(MyLayer)) then
  begin
    FOSMAdditionalLayers.PutElement(lsbAdditionalLayers.ItemIndex, MyLayer);
    lsbAdditionalLayers.Invalidate;
  end;
end;

procedure TCdrCalculette.ResetOSMLayers();
begin
  FOSMAdditionalLayers.ClearListe();
  FOSMAdditionalLayers.AddGISLayer(0, 10.00, clBlue, 255, DEFAULT_OSM_LAYER, 'Couche00');
end;

procedure TCdrCalculette.acOSMExtractLayersFromTableauExecute(Sender: TObject);
var
  Nb, i: Integer;
  QColor: TColor;
begin
  Randomize;
  // Ne pas resetter la liste des couches. On ajoute à l'existant
  Nb := grdDonnees.RowCount;
  if (Nb = 0) then exit;
  for i := FLigneDep to Nb -1 do
  begin
    QColor := RGBToColor(Trunc(Random * 256),
                         Trunc(Random * 256),
                         Trunc(Random * 256));
    FOSMAdditionalLayers.AddGISLayer(1, 10.00, QColor, 255, grdDonnees.Cells[FColonneOSMLayer, i], grdDonnees.Cells[FColonneOSMLayer, i]);
  end;
  RelisterAdditionalLayers();
end;




procedure TCdrCalculette.acOSMAddLayerExecute(Sender: TObject);
begin
  pass;
  //if (InputQuery('Couches additionnelles', 'Nouvelle couche', QLayer)) then
  //begin
  //  FOSMAdditionalLayers.Add(Trim(QLayer));
  //  RelisterAdditionalLayers();
  // end;
end;

procedure TCdrCalculette.acOSMDeleteLayerExecute(Sender: TObject);
var
  QIdx: Integer;
  QLayer: TGISLayer;
begin
  QIdx := lsbAdditionalLayers.ItemIndex;
  if (QIdx < 0) then exit;
  if (0 = FOSMAdditionalLayers.GetNbElements()) then exit;
  QLayer := FOSMAdditionalLayers.GetElement(QIdx);
  if (GHTopoQuestionOuiNon(Format(rsDLG_CALC_ACN_OSM_CONFIRM_DELETE_LAYER, [QIdx, QLayer.LayerVarName]))) then
  begin
    FOSMAdditionalLayers.RemoveElement(QIdx);
    RelisterAdditionalLayers();
  end;
end;
procedure TCdrCalculette.acOSMRemoveAllLayersExecute(Sender: TObject);
begin
  if (not GHTopoQuestionOuiNon(rsDLG_CALC_ACN_OSM_CONFIRM_CLEAR_LAYERS)) then Exit;
  ResetOSMLayers();
  RelisterAdditionalLayers();
end;

procedure TCdrCalculette.RelisterAdditionalLayers();
var
  i, Nb: Integer;
  QLayer : TGISLayer;
begin
  lsbAdditionalLayers.Clear;
  Nb := FOSMAdditionalLayers.Count;
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    QLayer := FOSMAdditionalLayers.GetElement(i);
    lsbAdditionalLayers.Items.Add(QLayer.LayerVarName);
  end;
  lsbAdditionalLayers.ItemIndex := 0;
  grbxAdditionalLayers.Caption  := format('%d couches additionnelles', [FOSMAdditionalLayers.GetNbElements()]);
end;

procedure TCdrCalculette.acOuvrirCSVExecute(Sender: TObject);
var
  EWE: char;
  QFileName: TStringDirectoryFilename;
begin
  if (DoDialogOpenFile('Fichiers CSV(*.csv)|*.csv', '.csv', QFileName)) then
  begin
    EWE := ChooseChar(cmbSeparateur.ItemIndex, [#9, ';', ',', ':', '|']);
    // \t Tabulation; Point virgule, Virgule: Deux points| Barre
    grdDonnees.LoadFromCSVFile(QFileName, EWE, True, 0, True);
    RemplirLesComboboxDepuisGrille();
    RefactorerTitresDesColonnes();
  end;
end;



procedure TCdrCalculette.acQRCodeCoordPointExecute(Sender: TObject);
begin
  editTextToEncodeQR.Lines.Add(editX_Cible1JPC.Text);
  editTextToEncodeQR.Lines.Add(editY_Cible1JPC.Text);
  VueQRCode.Text := Trim(editTextToEncodeQR.Text);
  VueQRCode.Generate;
  PageControl1.ActivePageIndex := TAB_INDEX_QRCODE;
end;

procedure TCdrCalculette.acQRCodeListeCoordsExecute(Sender: TObject);
var
  i, j, Nb: Integer;
  MySelection: TGridRect;
  EWE: String;
begin
  Nb := grdDonnees.RowCount;
  MySelection := grdDonnees.Selection;
  for i := MySelection.Top to MySelection.Bottom do
  begin
    EWE := '';
    for j := MySelection.Left to MySelection.Right do  EWE += (Trim(grdDonnees.Cells[j, i]) + '; ');
    editTextToEncodeQR.Lines.Add(EWE);
  end;
  VueQRCode.Text := Trim(editTextToEncodeQR.Text);
  VueQRCode.Generate;
  PageControl1.ActivePageIndex := TAB_INDEX_QRCODE;
end;



procedure TCdrCalculette.btnConvertDMS2DegDecClick(Sender: TObject);
begin
  editX_Source1JPC.Value := DegMinSec2DegDec(editLatitudeDMS.Text);
  editY_Source1JPC.Value := DegMinSec2DegDec(editLongitudeDMS.Text);
end;

procedure TCdrCalculette.ResetGrdCoordsSection();
var
  i: Integer;
begin
  grdCoordonneesSection.RowCount := 666;
  grdCoordonneesSection.Cells[0, 0] := 'Vertex';
  grdCoordonneesSection.Cells[1, 0] := 'X';
  grdCoordonneesSection.Cells[2, 0] := 'Y';
  for i := 1 to grdCoordonneesSection.RowCount - 1 do
  begin
    grdCoordonneesSection.Cells[0, i] := Format('%d', [i]);
    grdCoordonneesSection.Cells[1, i] := '';
    grdCoordonneesSection.Cells[2, i] := '';
  end;
end;



procedure TCdrCalculette.ListerCaracteristiquesSections(const S: TDGCSectionOfBeam);
var
  i, Nb: Integer;
  MyVertex, EWE: TDGCPoint2D;
begin
  Nb := S.GetNbVertex();
  //grdCoordonneesSection.RowCount := Nb + 1;
  ResetGrdCoordsSection();
  for i := 0 to Nb - 1 do
  begin
    MyVertex := S.GetVertex(i);
    grdCoordonneesSection.Cells[1, i+1] := FormatterNombreOOo(MyVertex.X, 3);
    grdCoordonneesSection.Cells[2, i+1] := FormatterNombreOOo(MyVertex.Y, 3);
  end;
  lbCentroidX.Caption       := FormatterNombreOOo(S.Centroid.X, 3);
  lbCentroidY.Caption       := FormatterNombreOOo(S.Centroid.Y, 3);
  lbAire.Caption            := FormatterNombreOOo(S.Area, 3);
  lbPerimetre.Caption       := FormatterNombreOOo(S.Perimeter, 3);
  lbIxGx.Caption            := FormatterNombreOOo(S.InertiaXGX, 3);
  lbIyGy.Caption            := FormatterNombreOOo(S.InertiaYGY, 3);
  lbIxy.Caption             := FormatterNombreOOo(S.InertiaXY, 3);
  lbAnglePrincipal.Caption  := FormatterNombreOOo(radtodeg(S.MainAngleInertia), 3);
  lbInertieI.Caption        := FormatterNombreOOo(S.MainInertia_I, 3);
  lbInertieII.Caption       := FormatterNombreOOo(S.MainInertia_II, 3);
end;

procedure TCdrCalculette.DessinerCercleDeMohr(const S: TDGCSectionOfBeam);
var
  QIdxStyleSheetAxes, QIdxStyleSheetCercle, QIdxStyleSheetMoments, QIdxStyleSheetEpures: Integer;
  FFF: UnicodeString;
  R, C, Ey: ValReal;
  BB: TDGCBoundingBox;
begin
  Ey :=  0.5 * abs(S.MainInertia_II - S.MainInertia_I);
  BB.X1 :=   0.9 * S.MainInertia_I;
  BB.Y1 := -(1.1 * Ey);
  BB.X2 :=   1.1 * S.MainInertia_II;
  BB.Y2 :=   1.1 * Ey;
  CdrDGCDrawingContext2.Initialiser(BB.X1, BB.Y1, BB.X2, BB.Y2,
                                    true, btnBackGrdInertieFigure.ButtonColor);
  CdrDGCDrawingContext2.SetProcOnClick(nil); //self.GetCoordsPointClicked);
  CdrDGCDrawingContext2.BeginDrawing();

    CdrDGCDrawingContext2.BeginGroupe('CercleMohr');

      CdrDGCDrawingContext2.AddStyleSheet('Axes',
                                             clBlack   , 255, psSolid, 1, 0.5,
                                             clBlack   , 128, bsClear,
                                             'Arial', clBlack, 255, 20, 2.5, [fsBold],
                                             '');
      CdrDGCDrawingContext2.AddStyleSheet('Epures',
                                             clGreen, 255, psSolid, 1, 0.5,
                                             clGreen, 128, bsClear,
                                             'Arial', clBlack, 255, 30, 2.5, [fsBold],
                                             '');
      CdrDGCDrawingContext2.AddStyleSheet('Cercle',
                                             clRed, 255, psSolid, 2, 0.5,
                                             clYellow, 128, bsClear,
                                             'Arial', clRed, 255, 25, 2.5, [fsBold],
                                             '');

      CdrDGCDrawingContext2.AddStyleSheet('Moments',
                                             clBlue, 255, psSolid, 3, 0.5,
                                             clBlue, 128, bsClear,
                                             'Arial', clBlue, 255, 25, 2.5, [fsBold],
                                             '');

      QIdxStyleSheetAxes     := CdrDGCDrawingContext2.GetNbStyleSheets() - 4;
      QIdxStyleSheetEpures   := CdrDGCDrawingContext2.GetNbStyleSheets() - 3;
      QIdxStyleSheetCercle   := CdrDGCDrawingContext2.GetNbStyleSheets() - 2;
      QIdxStyleSheetMoments  := CdrDGCDrawingContext2.GetNbStyleSheets() - 1;

      CdrDGCDrawingContext2.AddInfiniteLine(QIdxStyleSheetAxes, tdgcVERTICAL_LINE  , Math.Min(S.InertiaXGX, S.InertiaYGY), 0.00);
      CdrDGCDrawingContext2.AddInfiniteLine(QIdxStyleSheetAxes, tdgcHORIZONTAL_LINE, Math.Min(S.InertiaYGY, S.InertiaYGY), 0.00);

      C :=  abs(0.5 * (S.InertiaYGY + S.InertiaXGX));
      R :=  Hypot(0.5 * (S.InertiaYGY - S.InertiaXGX), S.InertiaXY);
      CdrDGCDrawingContext2.AddEllipse(QIdxStyleSheetCercle, C, 0, R, R);
      CdrDGCDrawingContext2.AddTexte(QIdxStyleSheetCercle, S.MainInertia_I  - 100.00, 0, 3, 0.00, 'I.I');
      CdrDGCDrawingContext2.AddTexte(QIdxStyleSheetCercle, S.MainInertia_II + 100.00, 0, 1, 0.00, 'I.II');



      CdrDGCDrawingContext2.AddLine(QIdxStyleSheetEpures, C, 0, Math.Max(S.InertiaXGX, S.InertiaYGY), -S.InertiaXY);
      CdrDGCDrawingContext2.AddLine(QIdxStyleSheetEpures, C, 0, Math.Min(S.InertiaXGX, S.InertiaYGY),  S.InertiaXY);

      CdrDGCDrawingContext2.AddLine(QIdxStyleSheetMoments, Math.Max(S.InertiaXGX, S.InertiaYGY), 0, Math.Max(S.InertiaXGX, S.InertiaYGY), -S.InertiaXY);
      CdrDGCDrawingContext2.AddLine(QIdxStyleSheetMoments, Math.Min(S.InertiaXGX, S.InertiaYGY), 0, Math.Min(S.InertiaXGX, S.InertiaYGY),  S.InertiaXY);
      CdrDGCDrawingContext2.AddTexte(QIdxStyleSheetMoments, Math.Max(S.InertiaXGX, S.InertiaYGY), -S.InertiaXY + 100.0, 1, 0.00, 'Ixy');

    CdrDGCDrawingContext2.EndGroupe('CercleMohr');
    CdrDGCDrawingContext2.Flush();
  CdrDGCDrawingContext2.EndDrawing();
  FFF := CdrDGCDrawingContext2.GetLastError();
  if (FFF <> '') then ShowMessage(FFF);

end;

procedure TCdrCalculette.DessinerSection(const S: TDGCSectionOfBeam);
var
  i, QIdxStyleSheet, Nb: Integer;
  MySommet: TDGCPoint2D;
  FFF: UnicodeString;
  BB: TDGCBoundingBox;
begin
  BB := S.GetBounds();
  Nb := S.GetNbVertex();
  CdrDGCDrawingContext1.Initialiser(-100, -100, 500, 500, True, btnBackGrdInertieFigure.ButtonColor);
  CdrDGCDrawingContext1.SetProcOnClick(nil); //self.GetCoordsPointClicked);
  CdrDGCDrawingContext1.BeginDrawing();
    CdrDGCDrawingContext1.BeginGroupe('IPN');
      CdrDGCDrawingContext1.AddStyleSheet('IPN',
                                             clRed, 255, psSolid, 2, 0.5,
                                             clYellow, 128, bsSolid,
                                             'Arial', clBlack, 255, 20, 2.5, [fsBold],
                                             '');
      QIdxStyleSheet := CdrDGCDrawingContext1.GetNbStyleSheets() - 1;
      CdrDGCDrawingContext1.BeginPolygon(QIdxStyleSheet, 'MonIPN');

      for i := 0 to Nb - 1 do
      begin
        MySommet := S.GetVertex(i);
        CdrDGCDrawingContext1.AddVertex(MySommet.X, MySommet.Y);
        CdrDGCDrawingContext1.AddTexte(QIdxStyleSheet, MySommet.X + 2, MySommet.Y + 2, 0, 0, Inttostr(i));
      end;
      CdrDGCDrawingContext1.EndPolygon();


      CdrDGCDrawingContext1.AddEllipse(QIdxStyleSheet, S.Centroid.X, S.Centroid.Y, 8, 8, '');
      CdrDGCDrawingContext1.AddTexte(QIdxStyleSheet,
                                     S.Centroid.X + 10,
                                     S.Centroid.Y + 10,
                                     0, 0,
                                     Format('%.3f, %.3f - A = %.3f', [S.Centroid.X, S.Centroid.Y, S.Area]));
    CdrDGCDrawingContext1.EndGroupe('IPN');
    CdrDGCDrawingContext1.Flush();
  CdrDGCDrawingContext1.EndDrawing();
  FFF := CdrDGCDrawingContext1.GetLastError();
  if (FFF <> '') then ShowMessage(FFF);
end;

procedure TCdrCalculette.Button1Click(Sender: TObject);
const
  //TX = -50; TY = -100;
  TX = 0; TY = 0;

var
  MySection: TDGCSectionOfBeam;
  BB: TDGCBoundingBox;
  Nb, i, QIdxStyleSheet: Integer;
  MySommet: TDGCPoint2D;
begin
  MySection := TDGCSectionOfBeam.Create;
  try
    if (MySection.Initialiser()) then
    begin
      MySection.AddVertex(  0,   0);
      MySection.AddVertex(320,   0);
      MySection.AddVertex(320,  20);
      MySection.AddVertex( 60,  20);
      MySection.AddVertex( 60, 280);
      MySection.AddVertex(110, 280);
      MySection.AddVertex(110, 300);
      MySection.AddVertex(  0, 300);
      MySection.AddVertex(  0, 280);
      MySection.AddVertex( 50, 280);
      MySection.AddVertex( 50,  20);
      MySection.AddVertex(  0,  20);
      //*)
      MySection.CalcBounds();
      // centre de gravité, inerties, aires, etc ...
      MySection.CalcCaracteristiques();
      ListerCaracteristiquesSections(MySection);
      // dessiner la section
      DessinerSection(MySection);
      DessinerCercleDeMohr(MySection);
      MySection.Finaliser();
    end;
  finally
    FreeAndNil(MySection);
  end;
  CdrDGCDrawingContext1.SetProcPickCoords(QProcTransmitCoords);
end;

procedure TCdrCalculette.Button2Click(Sender: TObject);
var
  MySection: TDGCSectionOfBeam;
  i: Integer;
begin
  MySection := TDGCSectionOfBeam.Create;
  try
    if (MySection.Initialiser()) then
    begin
      // récupérer les valeurs du tableau
      for i := 1 to grdCoordonneesSection.RowCount - 1 do
      begin
        if (Trim(grdCoordonneesSection.Cells[1, i] + grdCoordonneesSection.Cells[2, i]) = '') then break;
        MySection.AddVertex(ConvertirEnNombreReel(grdCoordonneesSection.Cells[1, i], 0.00),
                            ConvertirEnNombreReel(grdCoordonneesSection.Cells[2, i], 0.00));
      end;
      MySection.CalcBounds();
      // centre de gravité, inerties, aires, etc ...
      MySection.CalcCaracteristiques();
      ListerCaracteristiquesSections(MySection);
      // dessiner la section
      DessinerSection(MySection);
      DessinerCercleDeMohr(MySection);
      MySection.Finaliser();
    end;
  finally
    FreeAndNil(MySection);
  end;

end;

procedure TCdrCalculette.Button3Click(Sender: TObject);
var
  EWE: string;
  WU: TGHStringArray;
  i: Integer;
begin
  //CdrDGCDrawingContext1.SetModeTravail(mtgcsPICK_COORDS);
  //ShowMessage('666');
  EWE := '666 toto     "daech must die"     "miaou"';
  if (InputQuery('Découpage de texte', 'Texte', EWE)) then
  begin
    WU := SplitWithQuotedFields(EWE, ' ');
    AfficherMessageErreur(EWE);
    for i := 0 to high(EWE) do AfficherMessageErreur(WU[i]);
  end;

end;



procedure TCdrCalculette.cmbColAdditionalOSMLayersChange(Sender: TObject);
begin
  FColonneOSMLayer := cmbColAdditionalOSMLayers.ItemIndex;
end;


procedure TCdrCalculette.editExpressionEnter(Sender: TObject);
begin
  editExpression.SelStart  := length(editExpression.Text);
  editExpression.SelLength := 0;
end;

procedure TCdrCalculette.editLatitudeDMSEnter(Sender: TObject);
begin
  editLatitude.Color := clYellow;
end;

procedure TCdrCalculette.editLatitudeDMSExit(Sender: TObject);
begin
  editLatitude.Color := clWhite;
end;

procedure TCdrCalculette.editLongitudeDMSEnter(Sender: TObject);
begin
  editLatitude.Color := clYellow;
end;

procedure TCdrCalculette.editLongitudeDMSExit(Sender: TObject);
begin
  editLatitude.Color := clWhite;
end;




procedure TCdrCalculette.editTextToEncodeQRKeyPress(Sender: TObject;
  var Key: char);
begin
  if (Key = #13) then
  begin
    VueQRCode.Text := Trim(editTextToEncodeQR.Text);
    VueQRCode.Generate;
  end;
end;



procedure TCdrCalculette.lsbFonctionsDblClick(Sender: TObject);
var
  Expr: TCaption;
  EWE: String;
begin
  Expr := editExpression.Text;                               // récupère l'expression en cours d'écriture
  EWE  := Trim(lsbFonctions.Items[lsbFonctions.ItemIndex]);  // attrappe la fonction
  EWE += '(';                                                // ajoute la parenthèse ouvrante de la fonction
  Expr += EWE;                                               // poursuit la composition
  editExpression.Text := Expr;                               // met à jour la textbox
  editExpression.SetFocus;                                   // et s'y rend
end;

procedure TCdrCalculette.cmbColEtiquettesChange(Sender: TObject);
begin
  FColonneEtiquette  := cmbColEtiquettes.ItemIndex;
end;
procedure TCdrCalculette.cmbColDescriptionChange(Sender: TObject);
begin
  FColonneDescription := cmbColDescription.ItemIndex;
end;
procedure TCdrCalculette.cmbColObservationsChange(Sender: TObject);
begin
  FColonneObservations := cmbColObservations.ItemIndex;
end;

procedure TCdrCalculette.cmbColPhotoChange(Sender: TObject);
begin
  FColonnePhoto        := cmbColPhoto.ItemIndex;
end;

procedure TCdrCalculette.cmbColXSourceChange(Sender: TObject);
begin
  FColonneXSource := cmbColXSource.ItemIndex;
end;
procedure TCdrCalculette.cmbColYSourceChange(Sender: TObject);
begin
  FColonneYSource := cmbColYSource.ItemIndex;
end;
procedure TCdrCalculette.cmbColXCibleChange(Sender: TObject);
begin
  FColonneXCible := cmbColXCible.ItemIndex;
end;


procedure TCdrCalculette.cmbColYCibleChange(Sender: TObject);
begin
  FColonneYCible := cmbColYCible.ItemIndex;
end;



procedure TCdrCalculette.cmbSystSourceJPCChange(Sender: TObject);
begin
  SetLabelsValeursDemandees(cmbSystSourceJPC.ItemIndex);
end;

//******************************************************************************

procedure TCdrCalculette.btnMaillageImporterXYZClick(Sender: TObject);
var
  TD: TOpenDialog;
begin
  {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
  TD := TOpenDialog.Create(self);
  try
    TD.InitialDir := GetGHTopoDirectory();
    TD.Filter     := 'Fichiers XYZ|*.xyz|Fichiers CSV|*.csv';
    if (TD.Execute) then
    begin
      FMyMaillage.ImporterXYZ(TD.FileName);
      ListerMaillage();
    end;
  finally
    FreeAndNil(TD);
  end;
  {$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}
end;

procedure TCdrCalculette.btnSaveMaillageClick(Sender: TObject);
var
  TD: TSaveDialog;
begin
  {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
  TD := TSaveDialog.Create(self);
  try
   TD.Options := TD.Options + [ofOverwritePrompt];
   TD.InitialDir := GetGHTopoDirectory();
   TD.FileName := 'Maillage_001.mai';
   TD.Filter   := 'Maillages GHTopo (*.mai)|*.mai';
   if (TD.Execute) then
   begin
     FMyMaillage.SaveMaillageTIN(TD.FileName);
   end;
  finally
    FreeAndNil(TD);
  end;
  {$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}
end;

procedure TCdrCalculette.btnTriangulerClick(Sender: TObject);
begin
  {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
  FMyMaillage.TriangulerAvecUnOutilExterne(GetGHTopoDirectory() + 'Triangulateur.exe');
  //FMyMaillage.Trianguler();
  ListerMaillage();
  // FMyMaillage.Trianguler();
  {$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}
end;

procedure TCdrCalculette.Button4Click(Sender: TObject);
begin
  {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
  ListerMaillage();
  {$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}
end;


////////////////////////////////////////////////////////////////////////////////
// Spécifique GHTopo
{$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
// fiches topo
//******************************************************************************

procedure TCdrCalculette.GenererPagesPointsTopo(const QFileName: string; const QNb: integer);
var
  LSD: TEnsembleFichesPointsTopo;
begin
  AfficherMessage(Format('%s.GenererPagesPointsTopo: %s - %d', [ClassName, QFileName, QNb]));
  LSD := TEnsembleFichesPointsTopo.Create;
  try
    LSD.Initialise();
    LSD.CreerNouvellesFiches(QNB, QFileName);
  finally
    FreeAndNil(LSD);// LSD.Free;
  end;
end;
// maillages
//*****************************************************************************




{$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}


///*****************************************************************************
// Interpréteur PascalScript



end.
//******************************************************************************
(*
procedure TCdrCalculette.ParserContenuTexte();
var
  WU: String;
  i, j, Nb: Integer;
  MySeparateurs: TSysCharSet;
  EWE: TGHStringArray;
begin
  Nb := FContenuFichierTexte.Count;
  grdDataSeparetedBySpaces.RowCount := Nb + 1;
  grdDataSeparetedBySpaces.ColCount := 1 + High(EWE);
  MySeparateurs := GetSeparateursFromPnl();
  for i := 0 to Nb - 1 do
  begin
    WU := StringReplace(FContenuFichierTexte.Strings[i], '\t', #9, [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, '\r', ' ', [rfReplaceAll, rfIgnoreCase]);
    EWE := DecoupeChaine(WU,
                         MySeparateurs, [],
                         chkRetirerGuillemets.Checked);
    for j := 0 to High(EWE) do grdDataSeparetedBySpaces.Cells[j, i+1] := EWE[j];
  end;
end;
//*)
(*
procedure TCdrCalculette.btnSupprimerDoublonsClick(Sender: TObject);
var
  EWE1, EWE2: String;
  i, j, NbDoublonsRemoved: Integer;
begin
  if (grdDataSeparetedBySpaces.RowCount < 2) then Exit;
  NbDoublonsRemoved := 0;
  for i := grdDataSeparetedBySpaces.RowCount  - 1 downto 1 do
  begin
    EWE1 := '';
    for j := 1 to grdDataSeparetedBySpaces.ColCount - 1 do EWE1 += grdDataSeparetedBySpaces.Cells[j, i-1];
    EWE2 := '';
    for j := 1 to grdDataSeparetedBySpaces.ColCount - 1 do EWE2 += grdDataSeparetedBySpaces.Cells[j, i];
    if (EWE1 = EWE2) then
    begin
      grdDataSeparetedBySpaces.DeleteRow(i);
      NbDoublonsRemoved += 1;
    end;
  end;
  ShowMessageFmt('%d doublons supprimés', [NbDoublonsRemoved]);
end;
//*)
(*
function TCdrCalculette.GetSeparateursFromPnl(): TSysCharSet;
var
  WU: String;
  PersoSep: Char;
begin
  Result := [];
  WU := Trim(editPersonalSeparator.Text) + ' ';
  PersoSep := WU[1];

  if (chkSepTab.Checked)       then Result := Result + [#9]    else Result := Result - [#9];
  if (chkSepVirgule.Checked)   then Result := Result + [',']   else Result := Result - [','];
  if (chkSepPeriod.Checked)    then Result := Result + [';']   else Result := Result - [';'];
  if (chkSepSemicolon.Checked) then Result := Result + [':']   else Result := Result - [':'];
  if (chkSepOther.Checked)     then Result := Result + [PersoSep]
                               else Result := Result - [PersoSep];
  // le séparateur espace doit être armé en dernier
  if (chkSepEspace.Checked)    then Result := Result + [' ']  else Result := Result - [' '];
end;
//*)
(*

procedure TCdrCalculette.btnOuvrirTXTClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
begin
  if (DoDialogOpenFile(rsTXT_FILE_FILTER, '.txt', QFileName)) then
  begin
    FContenuFichierTexte.Clear;
    FContenuFichierTexte.LoadFromFile(QFileName);
    ParserContenuTexte;
  end;
end;
//*)


(*
   printf('%.6f', [CalcDistanceJaroWinkler('CUICUI','MIAOU')]);
  printf('%.6f', [CalcDistanceJaroWinkler('DWAYNE','DYAWNE')]);
  printf('%.6f', [CalcDistanceJaroWinkler('NICHES','NIHCES')]);
  printf('%.6f', [CalcDistanceJaroWinkler('MARTHA','MARHTAS')]);
  printf('%.6f', [CalcDistanceJaroWinkler('DIXON','DICKSONX')]);
  printf('%.6f', [CalcDistanceJaroWinkler('JELLYFISH','SMELLYFISH')]);
  printf('%.6f', [CalcDistanceJaroWinkler('Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch','Llanfairpwllgwyngyllgocherychwyrndrobwllllantysiliogogogoch')]);
  printf('%.6f', [CalcDistanceJaroWinkler('Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch','Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch')]);

  printf('%d', [CalcDistanceDamerauLevenshtein('CUICUI','MIAOU')]);
  printf('%d', [CalcDistanceDamerauLevenshtein('DWAYNE','DYAWNE')]);
  printf('%d', [CalcDistanceDamerauLevenshtein('NICHES','NIHCES')]);
  printf('%d', [CalcDistanceDamerauLevenshtein('MARTHA','MARHTAS')]);
  printf('%d', [CalcDistanceDamerauLevenshtein('DIXON','DICKSONX')]);
  printf('%d', [CalcDistanceDamerauLevenshtein('JELLYFISH','SMELLYFISH')]);
  printf('%d', [CalcDistanceDamerauLevenshtein('Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch','Llanfairpwllgwyngyllgocherychwyrndrobwllllantysiliogogogoch')]);
  printf('%d', [CalcDistanceDamerauLevenshtein('Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch','Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch')]);
  //*)
(*
procedure TCdrCalculette.sclDegradeChange(Sender: TObject);
var
  r, d, QPosition: Extended;
  EWE: TColor;
begin
  r := sclDegrade.Position / 255;
  d := editDegradeXFin.Value - editDegradeXDebut.Value;

  QPosition := editDegradeXDebut.Value + r * d;
  lbDegradeXCourant.Caption := Format('%.2f', [QPosition]);

  EWE := GetColorDegrade(QPosition, editDegradeXDebut.Value, editDegradeXFin.Value,
                         btnDegradeDebut.ButtonColor, btnDegradeFin.ButtonColor);
  pnlDegrade.Color := EWE;
end;


//*)
