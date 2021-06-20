// GHTopo pour Raspberry
// Version 'Ultra-lite' de GHTopo
// 26/09/2019: Actualisation
// 25/09/2019: Toutes les palettes de couleurs sont supprimées de GHTopo
//             à l'exception de celle de TBDDEntites
//             et des palettes locales: TCdrExpe, TdlgSelectElement
// 10/05/2020: Mise à jour de tous les cadres
// 06/04/2021: Unification des Load** et Save**
unit frmRPIMainWnd;
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Graphics,
  LazFileUtils,
  ToporobotClasses2012,
  CodeCalculTopo,
  UnitEntitesExtended,
  UnitGraphes1,
  ConvertisseurJPC,
  CallDialogsStdVersion,
  cadreNavigateurSeries,
  CadreSerieIndependant,
  UnitObjetSerie,
  CadreListesSimples,
  unitCroquisTerrain,
  UnitClasseMaillage,
  CadreListeViseesEnAntenne,
  CadreDistoXLazSerial,
  CadreGHTopoContext2D,
  BZGraphesTypes, BZGraphesClasses, CadreViseesAntenne,
  Classes, SysUtils,
  Forms, Controls, Dialogs, ActnList, Menus, ComCtrls, ExtCtrls, StdCtrls, Buttons, PairSplitter, EditBtn, FileCtrl, SynEdit;
const
  ECRAN_RPI_WIDTH  = 1024;  // écran 8 ''
  ECRAN_RPI_HEIGHT =  600;
type TModeUseMiniDialogSaveXTB = (aesNONE, aesNEW_DOC, aesSAVE_AS);
type

{ TRPIMainWnd }

 TRPIMainWnd = class(TForm)
    acQuit: TAction;
    acNew: TAction;
    acOpen: TAction;
    acQSave: TAction;
    acCompileExt: TAction;
    acPanVue: TAction;
    acLocaliserStation: TAction;
    acCheckDatabase: TAction;
    acParametrerVue: TAction;
    acNewPolyligne: TAction;
    acNewAnnotation: TAction;
    acDeletePolyline: TAction;
    acDeleteAnnotation: TAction;
    acDemanderUneMesure: TAction;
    acSetModeTravailNone: TAction;
    acHelp: TAction;
    acStatistiques: TAction;
    acCopierConsoleErreur: TAction;
    acClearConsoleErreur: TAction;
    acSaveAs: TAction;
    acExportGIS: TAction;
    acExporterGraphe: TAction;
    acViderCroquis: TAction;
    acVue3D: TAction;
    acZoomMoins: TAction;
    acZoomPlus: TAction;
    acZoomFenetre: TAction;
    acZoomTout: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    btnNewDocCancel: TButton;
    btnQuickOpenCancel: TButton;
    btnNewDocOK: TButton;
    btnQuickOpenOK: TButton;
    btnStyleObjet0: TStaticText;
    btnStyleObjet1: TStaticText;
    btnStyleObjet10: TStaticText;
    btnStyleObjet11: TStaticText;
    btnStyleObjet2: TStaticText;
    btnStyleObjet3: TStaticText;
    btnStyleObjet4: TStaticText;
    btnStyleObjet5: TStaticText;
    btnStyleObjet6: TStaticText;
    btnStyleObjet7: TStaticText;
    btnStyleObjet8: TStaticText;
    btnStyleObjet9: TStaticText;
    btnExchange: TButton;
    btnStationDepart: TButton;
    btnStationArrivee: TButton;
    CdrDistoXLazserial1: TCdrDistoXLazserial;
    CdrListesSimples1: TCdrListesSimples;
    CdrListeViseesEnAntenne1: TCdrListeViseesEnAntenne;
    CdrNavigateurSeries1: TCdrNavigateurSeries;
    CdrSerieIndependant1: TCdrSerieIndependant;
    chkModeFonctionnement: TCheckBox;
    GHTopoContext2DA1: TGHTopoContext2DA;
    lbNextWptAzimut1: TStaticText;
    lsbPathRoadMap: TComboBox;
    editCurrentDirectory: TDirectoryEdit;
    editFileName: TEdit;
    lbFichier: TLabel;
    lbTopographie: TLabel;
    lsbFichiersEnCours: TFileListBox;

    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbNbPathStations: TLabel;
    lbEtape: TLabel;
    lbMemoryUsage: TStaticText;
    lsbJournal: TListBox;
    PageControl1: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pnlAdditionalMenu: TPanel;
    pnlMainMenu: TPanel;
    pnlAcquitteVisee: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pnlNavigation: TPanel;
    pnlCmdsVue: TPanel;
    pnlCmdsVue1: TPanel;
    pnlOutilsCroquis: TPanel;
    pnlProgression: TPanel;
    pnlCommandes: TPanel;
    pnlPlan: TPanel;
    pnlSaveAs: TPanel;
    pnlQuickOpen: TPanel;
    ProgressBar1: TProgressBar;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    memoErreurs: TSynEdit;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    lbNextWpt: TStaticText;
    lbNextWptAzimut: TStaticText;
    tabShtMenu: TTabSheet;
    tabshtJournal: TTabSheet;
    tabShtAntennes: TTabSheet;
    tabShtSeries: TTabSheet;
    tabShtListesSimples: TTabSheet;
    tabShtDistoX: TTabSheet;
    tabShtVuePlan: TTabSheet;

    procedure acCheckDatabaseExecute(Sender: TObject);
    procedure acDeleteAnnotationExecute(Sender: TObject);
    procedure acDeletePolylineExecute(Sender: TObject);
    procedure acExporterGrapheExecute(Sender: TObject);
    procedure acExportGISExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure acLocaliserStationExecute(Sender: TObject);
    procedure acNewAnnotationExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acNewPolyligneExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPanVueExecute(Sender: TObject);
    procedure acParametrerVueExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure acCompileExtExecute(Sender: TObject);
    procedure acDemanderUneMesureExecute(Sender: TObject);
    procedure acQSaveExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acSetModeTravailNoneExecute(Sender: TObject);
    procedure acStatistiquesExecute(Sender: TObject);
    procedure acViderCroquisExecute(Sender: TObject);
    procedure acVue3DExecute(Sender: TObject);
    procedure acZoomFenetreExecute(Sender: TObject);
    procedure acZoomMoinsExecute(Sender: TObject);
    procedure acZoomPlusExecute(Sender: TObject);
    procedure acZoomToutExecute(Sender: TObject);
    procedure btnExchangeClick(Sender: TObject);
    procedure btnModeTopoNavigChange(Sender: TObject);
    procedure btnNewDocCancelClick(Sender: TObject);
    procedure btnNewDocOKClick(Sender: TObject);
    procedure btnQuickOpenCancelClick(Sender: TObject);
    procedure btnQuickOpenOKClick(Sender: TObject);
    procedure btnRecalcParcoursClick(Sender: TObject);
    procedure btnStationArriveeClick(Sender: TObject);
    procedure btnStationDepartClick(Sender: TObject);

    procedure btnStyleObjet0Click(Sender: TObject);
    procedure btnStyleObjet1Click(Sender: TObject);
    procedure btnStyleObjet2Click(Sender: TObject);
    procedure btnStyleObjet3Click(Sender: TObject);
    procedure btnStyleObjet4Click(Sender: TObject);
    procedure btnStyleObjet5Click(Sender: TObject);
    procedure btnStyleObjet6Click(Sender: TObject);
    procedure btnStyleObjet7Click(Sender: TObject);
    procedure btnStyleObjet8Click(Sender: TObject);
    procedure btnStyleObjet9Click(Sender: TObject);

    procedure Button4Click(Sender: TObject);
    procedure chkModeFonctionnementChange(Sender: TObject);
    procedure editFileNameChange(Sender: TObject);
    procedure editFileNameKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbEtapeClick(Sender: TObject);
    procedure lsbPathRoadMapChange(Sender: TObject);
    procedure lsbPathRoadMapClick(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure mnuSetMainWndDefaultClick(Sender: TObject);
    procedure mnuSetMainWnd1024x640Click(Sender: TObject);
    procedure mnuSetMainWnd800x600Click(Sender: TObject);
    procedure mnuSetMainWnd640x480Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure pnlNavigationClick(Sender: TObject);

  strict private
    FModeFonctionnementGHTopoContext2D: TModeFonctionnementGHTopoContext2D;
    FModeUseMiniDialogSaveXTB: TModeUseMiniDialogSaveXTB;
    FCurrentDocTopoName: TStringDirectoryFilename;
    procedure CentrerPnlAcquitteVisee();
    procedure DisplayBearingToNext(const IdxWpt: integer);
    function  RecalculerShortestPath(const StationDepart, StationArrivee: string): boolean;
    procedure SetModeFonctionnement(const MF: TModeFonctionnementGHTopoContext2D);
    procedure SetSizeMainWnd(const w, h: integer);

  private
    FConvertisseurCoordonnees : TConversionSysteme;
    FDocumentToporobot        : TToporobotStructure2012;
    FBDDEntites               : TBDDEntites;
    FGraphe                   : TPathFindingGraphe;
    FShortestPath             : TPathBetweenNodes;
    FMaillage                 : TMaillage;
    FCroquisTerrain           : TCroquisTerrain;
    FCurrentSystemeEPSG       : TLabelSystemesCoordsEPSG;
    FCurrentStation           : TToporobotIDStation;
    FCurrentNomFichierXTB     : TStringDirectoryFilename;
    FDocTopoOpenedAndReady    : boolean; // doit être visible de partout
    function  InitialiserGHTopoRPC(): boolean;
    procedure FinaliserGHTopoRPC();
    procedure InitCaptions();
    procedure DispProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
    procedure QuickSaveHorodatee();
    procedure ReactualiserTables(const B: boolean);

    procedure SetCurrentEPSG(const C: TLabelSystemesCoordsEPSG);
    procedure SetCurrentEPSGByCode(const C: integer);
    procedure SetPanelStyles(const QCurrStyleIdx: integer);
    procedure SetVisibiliteProgressBar(const B: boolean);

    function  InitNewDocument(): boolean;
    function  RestoreLastDocument(): boolean;
    // Gestion de la topo
    function  ChargerLaTopoExt(const FC: TStringDirectoryFileName): boolean;
    function  ChargerLeKrobard(const FC: TStringDirectoryFileName): boolean;
    function  CalculerLeReseauExt(): boolean;
    procedure RecalculerEtActualiser(const DoReinitDistoX: boolean);
    procedure CloseAllDocuments();
    // Onglet Séries
    procedure PreparerFrontalSeries();
    procedure PreparerListesSimples();

    procedure PreparerVue2D(const MF: TModeFonctionnementGHTopoContext2D; const FBD: TBDDEntites);
    procedure PreparerCadreViseesAntennes(const FD: TToporobotStructure2012);
    procedure CreateNewSerie();
    procedure GotoSerie();
    function  GotoSerieDisAllowed(): Boolean;
    procedure ImplementerModifsSerie();
    // Spécifique au DistoX
    function  PreparerCadreDistoX(): boolean;
    procedure PasserLaViseeAuCadreUtilisateur(const MV: TMesureViseeDistoX; const TV: TTypeViseeDistoX);
  public
    function  OuvrirLaTopo(const FData, FKrobard: TStringDirectoryFilename; const ForceReinitNewDoc: boolean): boolean;
end;

var
  RPIMainWnd: TRPIMainWnd;

implementation
uses
  DGCDummyUnit;
{$R *.lfm}
// dossier de sauvegarde du document courant
const BACKUP_FOLDER_CURRENT_DOC_TOPO = '0000_Backup_Topo';
const BACKUP_ROOT_FILENAME           = 'BackUp00';

procedure TRPIMainWnd.SetSizeMainWnd(const w, h: integer);
begin
  if ((h < 0) or (h < 0)) then
  begin
    self.Width  := Screen.DesktopWidth;
    self.Height := Screen.DesktopHeight;
  end
  else
  begin
    self.Width  := w;
    self.Height := h;
  end;
end;


procedure TRPIMainWnd.DisplayBearingToNext(const IdxWpt: integer);
var
  n: Integer;
  WptCurr, WptNext: TBZClassNode;
  qdx, qdy, qdz, QDist, QAz, QIncl: Double;
begin
  lbNextWpt.Caption       := '---';
  lbNextWptAzimut.Caption := '---';
  n := FShortestPath.GetNbNoeuds();

  if (0 = n) then Exit;

  if (IdxWpt = (n-1)) then
  begin
    lbNextWpt.Caption := 'Finish';
    Exit;
  end;

  WptCurr := FGraphe.GetStation(FShortestPath.GetNoeud(IdxWpt));
  WptNext := FGraphe.GetStation(FShortestPath.GetNoeud(IdxWpt+1));
  lbNextWpt.Caption := WptNext.ToString();
  qdx := WptNext.Position.X - WptCurr.Position.X;
  qdy := WptNext.Position.Y - WptCurr.Position.Y;
  qdz := WptNext.Position.Z - WptCurr.Position.Z;
  GetBearingInc(qdx, qdy, qdz, QDist, QAz, QIncl, 360.00, 360.00);
  lbNextWptAzimut.Caption:= format('%.1f°', [QAz]);
  // et on centre la carte sur le point
  showmessagefmt('Next: %d - %f, %f', [WptNext.IDStation, WptNext.Position.X, WptNext.Position.Y]);
  GHTopoContext2DA1.CentrerVueSurPointXY(WptNext.Position.X, WptNext.Position.Y, True, WptNext.ToString());
  // sans modifier la station courante
  //  self.SetCurrentStation(QMyBaseStation, True);

end;

procedure TRPIMainWnd.FormShow(Sender: TObject);
begin
  //{$IFDEF RASPBERRY_PI}
  SetSizeMainWnd(ECRAN_RPI_WIDTH, ECRAN_RPI_HEIGHT);
  //{$ENDIF}
end;

procedure TRPIMainWnd.lbEtapeClick(Sender: TObject);
begin

end;

procedure TRPIMainWnd.lsbPathRoadMapChange(Sender: TObject);
begin
  DisplayBearingToNext(lsbPathRoadMap.ItemIndex);
end;



procedure TRPIMainWnd.lsbPathRoadMapClick(Sender: TObject);
var
  Nb: Integer;
  MyNode: TBZClassNode;
  QSr: TNumeroSerie;
  QSt: TNumeroStation;
begin
  Nb := FShortestPath.GetNbNoeuds();
  if ((FModeFonctionnementGHTopoContext2D = mfgcNAVIGATION) AND (Nb > 2))then
  begin
    MyNode := FGraphe.GetStation(FShortestPath.GetNoeud(lsbPathRoadMap.ItemIndex));
    GHTopoContext2DA1.CentrerVueSurPointXY(MyNode.Position.X, MyNode.Position.Y, True, MyNode.ToString());
  end;
end;

procedure TRPIMainWnd.MenuItem14Click(Sender: TObject);
begin
  CdrDistoXLazserial1.SetProcTransmitMesureDistoX(PasserLaViseeAuCadreUtilisateur);
end;

procedure TRPIMainWnd.MenuItem16Click(Sender: TObject);
begin
  RegisterChineseUser();
end;

procedure TRPIMainWnd.mnuSetMainWndDefaultClick(Sender: TObject);
begin
  SetSizeMainWnd(-1, -1);
end;

procedure TRPIMainWnd.mnuSetMainWnd1024x640Click(Sender: TObject);
begin
  SetSizeMainWnd(1024, 600);
end;

procedure TRPIMainWnd.mnuSetMainWnd800x600Click(Sender: TObject);
begin
  SetSizeMainWnd(800, 600);
end;

procedure TRPIMainWnd.mnuSetMainWnd640x480Click(Sender: TObject);
begin
  SetSizeMainWnd(640, 480);
end;

procedure TRPIMainWnd.PageControl1Change(Sender: TObject);
var
  MySerie: TObjSerie;
begin
  case PageControl1.ActivePageIndex of
    3: begin// page des visées en antennes
      MySerie := CdrSerieIndependant1.GetCurrentSerie();
      CdrListeViseesEnAntenne1.SetCurrentInternalIdxSerie(MySerie.GetNumeroDeSerie());
    end;
  else
    pass;
  end;
end;

procedure TRPIMainWnd.pnlNavigationClick(Sender: TObject);
begin

end;

procedure TRPIMainWnd.acQuitExecute(Sender: TObject);
begin
  QuickSaveHorodatee();
  self.Close;
end;

procedure TRPIMainWnd.acCompileExtExecute(Sender: TObject);
var
  QCurrentSerieDistoX: TNumeroSerie;
  QCurrentPointDistoX: TNumeroStation;
  QCBG, QCHD         : TPoint2Df;
begin
  QCurrentSerieDistoX   := GHTopoContext2DA1.GetCurrentNumeroSeriePourDistoX();
  QCurrentPointDistoX   := GHTopoContext2DA1.GetCurrentNumeroPointPourDistoX();

  GHTopoContext2DA1.GetViewLimitsFromOnglet(0, QCBG, QCHD);
  RecalculerEtActualiser(False);

  GHTopoContext2DA1.SetViewLimits(QCBG, QCHD, '');
  GHTopoContext2DA1.SetCurrentNumeroSerieStationPourDistoX(QCurrentSerieDistoX, QCurrentPointDistoX);
  GHTopoContext2DA1.SetCurrentStationBySerSt(QCurrentSerieDistoX, QCurrentPointDistoX);
end;

procedure TRPIMainWnd.acDemanderUneMesureExecute(Sender: TObject);
begin
  CdrDistoXLazserial1.DemanderUneMesure();
end;

procedure TRPIMainWnd.acQSaveExecute(Sender: TObject);
begin
  QuickSaveHorodatee();
  ShowMessage(GetResourceString(rsDONE_ANY_PROCESS));
end;

procedure TRPIMainWnd.acSaveAsExecute(Sender: TObject);
begin
  pnlSaveAs.Visible := false;
  pnlQuickOpen.Visible := false;
  QuickSaveHorodatee();
  editFileName.Text              := FCurrentDocTopoName;
  editCurrentDirectory.Directory := GetGHTopoDirectory() + MON_DOSSIER_RPI_DOCS;
  pnlSaveAs.Visible              := True;
  FModeUseMiniDialogSaveXTB      := aesSAVE_AS;
end;

procedure TRPIMainWnd.acSetModeTravailNoneExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtREADY);
end;

procedure TRPIMainWnd.acStatistiquesExecute(Sender: TObject);
begin
  DisplayStatistiquesExt(FDocumentToporobot, FBDDEntites);
end;

procedure TRPIMainWnd.acViderCroquisExecute(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon(rsVIDER_CROQUIS)) then
  begin
    FCroquisTerrain.ViderLesListesObjets();
    GHTopoContext2DA1.RefreshDessin();
  end;
end;

procedure TRPIMainWnd.acVue3DExecute(Sender: TObject);
begin
  DisplayVue3DGDIExt(FBDDEntites, FDocumentToporobot, FMaillage, GHTopoContext2DA1.GetCurrentOnglet());
end;

procedure TRPIMainWnd.QuickSaveHorodatee();
var
  MyFolderQSave: TStringDirectoryFilename;
  MyQSaveData, MyQSaveKrobard, QBackUpFolder: TStringDirectoryFilename;
  procedure QSave(const QData, QKrobard: TStringDirectoryFilename);
  begin
    FDocumentToporobot.SaveToXTB(QData, mtabEXTENDEDTAB, tfWINDOWS);
    FCroquisTerrain.SaveToXML(QKrobard);
  end;
begin
  MyFolderQSave := GetGHTopoDirectory() + MON_DOSSIER_QSAVES + PathDelim;
  ForceDirectories(MyFolderQSave);
  // sauvegarde horodatée
  MyQSaveData    := MyFolderQSave + MakeFilenameFromDate('QSave_Data', Now(), 'xtb');
  MyQSaveKrobard := MyFolderQSave + MakeFilenameFromDate('QSave_Krobard', Now(), 'xml');
  QSave(MyQSaveData, MyQSaveKrobard);
  // sauvegarde de la topo courante
  QBackUpFolder := GetGHTopoDirectory() + BACKUP_FOLDER_CURRENT_DOC_TOPO;
  ForceDirectories(QBackUpFolder);
  MyQSaveData    := QBackUpFolder + PathDelim + BACKUP_ROOT_FILENAME + '.xtb';
  MyQSaveKrobard := QBackUpFolder + PathDelim + BACKUP_ROOT_FILENAME + '.xml';
  // TODO: Sauvegarde sur une autre unité
  QSave(MyQSaveData, MyQSaveKrobard);
end;

procedure TRPIMainWnd.acOpenExecute(Sender: TObject);
begin
  pnlSaveAs.Visible := false;
  pnlQuickOpen.Visible := false;
  if (FDocTopoOpenedAndReady AND (not GHTopoQuestionOuiNon(rsWARN_FILE_ALREADY_OPEN))) then Exit;
  // TODO: Ajouter -dUseCThreads dans les options personnalisées du projet
  //       avant de valider ce contournement
  (*
  if (DoDialogOpenFileGHTopoSimplifie(QFileData)) then
  begin
    lbEtape.Caption := 'Chargement de ' + QFileData;
    Application.ProcessMessages;
    QFileKrobard := ChangeFileExt(QFileData, '.xml');
    if (OuvrirLaTopo(QFileData, QFileKrobard, false)) then
    begin
      lbEtape.Caption := 'Ready';
      FCurrentDocTopoName := ExtractFileNameOnly(QFileData);
		end;
  end;
  //*)
  // Solution de contournement du bug X Window: boite de dialogue intégrée
  // lister les fichiers
  lsbFichiersEnCours.Directory :=  GetGHTopoDirectory() + MON_DOSSIER_RPI_DOCS;
  lsbFichiersEnCours.UpdateFileList;
  pnlQuickOpen.Visible := True;
end;

procedure TRPIMainWnd.acNewPolyligneExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtNEW_POLYLINE);
end;

procedure TRPIMainWnd.acLocaliserStationExecute(Sender: TObject);
var
  QIDStation: string;
begin
  QIDStation := '';// Utiliser la station courante;
  if (GHTopoInputQuery('Recherche de station', 'ID station', QIDStation)) then
  begin
    GHTopoContext2DA1.LocaliserUneStationByCle(QIDStation);
  end;
end;

procedure TRPIMainWnd.acNewAnnotationExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtNEW_ANNOTATION);
end;

procedure TRPIMainWnd.acNewExecute(Sender: TObject);
begin
  pnlSaveAs.Visible := false;
  pnlQuickOpen.Visible := false;
  FModeUseMiniDialogSaveXTB := aesNEW_DOC;
  editFileName.Text := 'Nouveau_document';
  editCurrentDirectory.Enabled   := false;
  editCurrentDirectory.Directory := GetGHTopoDirectory() + MON_DOSSIER_RPI_DOCS;
  pnlSaveAs.Visible := true;
end;

procedure TRPIMainWnd.acCheckDatabaseExecute(Sender: TObject);
begin
  if (FDocumentToporobot.CheckerLesDonneesTopo() > 0) then
  begin
    CdrListesSimples1.ListerListesSimples(mbddCHECK_ERRORS);
    PageControl1.ActivePageIndex := 1;
  end;
end;

procedure TRPIMainWnd.acDeleteAnnotationExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtDELETE_ANNOTATION);
end;

procedure TRPIMainWnd.acDeletePolylineExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtDELETE_POLYLINE);
end;

procedure TRPIMainWnd.acExporterGrapheExecute(Sender: TObject);
var
  QDocTitle: string;
  QDepart, QArrivee: string;
  QFilename: RawByteString;
  QBG, QCC: TColor;
  QW, QH, QLMenu: integer;
begin
  // export graphique du graphe
  QDocTitle := 'Graphe de ' + FDocumentToporobot.GetNomEtude();

  QFilename := GetGHTopoDirectory() +
               MakeFilenameFromDate('00_Graphe_LT_', Now(), 'htm');
  QDepart   := Trim(btnStationDepart.Caption);
  QArrivee  := trim(btnStationArrivee.Caption);
  QLMenu := 200;
  QW := Screen.Monitors[0].Width - 20;
  QH := Screen.Monitors[0].Height;
  QBG := clCream;
  QCC := clMaroon;

  FGraphe.ExporterGrapheEnJavascript(QDocTitle, QFilename,  QDepart, QArrivee, QBG, QCC, QW, QH, QLMenu);
end;

procedure TRPIMainWnd.acExportGISExecute(Sender: TObject);
begin
  if (FDocTopoOpenedAndReady) then DisplayExportSIG(FDocumentToporobot, FBDDEntites);
end;

procedure TRPIMainWnd.acHelpExecute(Sender: TObject);
begin
  DisplayHelpSystem('', True);
end;

procedure TRPIMainWnd.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FinaliserGHTopoRPC();
end;

procedure TRPIMainWnd.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  if (MessageDlg(EnlevePerluete(GetResourceString(rsGHTOPO_QUIT)), mtConfirmation, [mbYes, mbNo],0)=mrYES) then CanClose := True;
end;

procedure TRPIMainWnd.FormCreate(Sender: TObject);
begin
  self.Font.Style := [];
  if (not InitialiserGHTopoRPC()) then
  begin
    ShowMessage('Echec de démarrage de GHTopoRPI');
    FinaliserGHTopoRPC();
    Application.Terminate;
  end;
  InitCaptions();
end;

procedure TRPIMainWnd.FormResize(Sender: TObject);
begin
  CentrerPnlAcquitteVisee();
end;

procedure TRPIMainWnd.CentrerPnlAcquitteVisee();
const MG = 40;
begin
  if (pnlAcquitteVisee.Visible) then
  begin
    pnlAcquitteVisee.Width   := self.ClientWidth - 2 * MG;
    pnlAcquitteVisee.Top     := (self.ClientHeight - pnlAcquitteVisee.Height) div 2;
    pnlAcquitteVisee.Left    := MG;
  end;
end;

function TRPIMainWnd.InitialiserGHTopoRPC(): boolean;
var QPT0: TPoint3Df;
begin
  Result := false;
  chkModeFonctionnement.Checked  := false;
  chkModeFonctionnement.Caption  := 'TOPO';
  FModeUseMiniDialogSaveXTB := aesNONE;
  pnlSaveAs.Visible         := false;
  pnlQuickOpen.Visible      := false;
  pnlAcquitteVisee.Visible  := false;
  self.FormStyle := fsStayOnTop;
  // onglets
  //tabShtMenu.Color              := clRed;
  tabShtVuePlan.Caption         := GetResourceString(rsGHTOPO_RPI_MAIN_WND_TAB_VUE2D);
  tabShtSeries.Caption          := GetResourceString(rsGHTOPO_RPI_MAIN_WND_TAB_SERIES);
  tabShtListesSimples.Caption   := GetResourceString(rsGHTOPO_RPI_MAIN_WND_TAB_LISTES);
  tabShtDistoX.Caption          := GetResourceString(rsGHTOPO_RPI_MAIN_WND_TAB_DISTOX);
  tabShtAntennes.Caption        := GetResourceString(rsGHTOPO_RPI_MAIN_WND_TAB_ANTENNES);
  SetVisibiliteProgressBar(false);
  FCurrentStation.aSerie        := 1;
  FCurrentStation.aStation      := 0;
  FCurrentStation.eIdxNameSpace := 0;
  FCurrentStation.aIDTerrain    := '';
  self.Constraints.MaxWidth     := FRM_CONSTRAINT_MAX_WIDTH;
  self.Constraints.MaxHeight    := FRM_CONSTRAINT_MAX_HEIGHT;
  FDocTopoOpenedAndReady := false;  // prêt à dessiner le plan

  DefaultFormatSettings.DecimalSeparator := '.'; // point décimal
  PreparerCadreDistoX();                         // Préparation du cadre DistoX
  // Création des tables
  FCurrentNomFichierXTB     := '';
  FDocumentToporobot := TToporobotStructure2012.Create;
  FCroquisTerrain    := TCroquisTerrain.Create;
  FBDDEntites        := TBDDEntites.Create;
  FMaillage          := TMaillage.Create;
  FGraphe            := TPathFindingGraphe.Create;
  FConvertisseurCoordonnees := TConversionSysteme.Create;
  try
    FConvertisseurCoordonnees.Initialiser();
    FCurrentSystemeEPSG := FConvertisseurCoordonnees.GetEPSGSystemeFromCodeEPSG(2154);
    FDocumentToporobot.ReInitialiser(True);
    FCroquisTerrain.Initialiser(FBDDEntites,'MyCroquis001.xml');
    QPT0.Empty();
    FBDDEntites.Initialiser(QPT0, clAqua, clMaroon);
    FMaillage.Initialiser(FBDDEntites);
    if (Not RestoreLastDocument()) then InitNewDocument();
    Result := True;
  except
    pass;
  end;
  SetModeFonctionnement(mfgcSURVEYING);
  FShortestPath.Initialiser(1,1, 1,1, 'Vers la sortie', clRed);
end;
procedure TRPIMainWnd.FinaliserGHTopoRPC();
var
  fp: TextFile;
begin
  FCurrentStation.aSerie   := 0;
  FCurrentStation.aStation := 0;
  FDocTopoOpenedAndReady := false;  // prêt à dessiner le plan
  try
    GHTopoContext2DA1.CanDraw := false;
    FDocumentToporobot.Finaliser();
    FBDDEntites.Finaliser();
    FCroquisTerrain.Finaliser();
    FConvertisseurCoordonnees.Finaliser();
    FMaillage.Finaliser();
    // sauvegarder le nom du dernier document ouvert
    AssignFile(fp,  GetGHTopoDirectory() + GHTOPO_RPI_INI_FILENAME);
    ReWrite(fp);
    WriteLn(fp, FCurrentNomFichierXTB);
    CdrDistoXLazserial1.Finaliser();
  finally
    Closefile(fp);
    FreeAndNil(FDocumentToporobot); // FDocumentToporobot.Free;
    FreeAndNil(FBDDEntites); // FBDDEntites.Free;
    FreeAndNil(FCroquisTerrain);
    FreeAndNil(FConvertisseurCoordonnees); // FConvertisseurCoordonnees.Free;
    FreeAndNil(FMaillage);
  end;
end;
// Spécifique cadre DistoX
function TRPIMainWnd.PreparerCadreDistoX(): boolean;
begin
  result := CdrDistoXLazserial1.Initialiser(PasserLaViseeAuCadreUtilisateur);
end;
procedure TRPIMainWnd.PasserLaViseeAuCadreUtilisateur(const MV: TMesureViseeDistoX; const TV: TTypeViseeDistoX);
begin
  if (TV = tvdCHEMINEMENT) then
  begin
    pnlAcquitteVisee.Visible := true;
    pnlAcquitteVisee.Caption := Format('%.3f, %.3f, %.3f', [MV.Longueur, MV.Azimut, MV.Pente]);
    GHTopoContext2DA1.TraiterMesureIssueDuDistoX(MV, TV);
    pnlAcquitteVisee.Visible := false;
  end
  else
  begin
    GHTopoContext2DA1.TraiterMesureIssueDuDistoX(MV, TV);
  end;
end;

{ TRPIMainWnd }
procedure TRPIMainWnd.InitCaptions();
  procedure SetAcHint(const A: TAction; const H: string);
  var QS: String;
  begin
    QS := GetResourceString(H);
    A.Caption := QS;
    A.Hint    := EnlevePerluete(QS);
  end;
begin
   lbFichier.Caption := GetResourceString(rsMNU_FILE);
    SetAcHint(acNew                   , rsNEW);
    SetAcHint(acOpen                  , rsOPEN);
    SetAcHint(acQSave                 , rsQUICK_SAVE);
    SetAcHint(acSaveAs                , rsSAVEAS);
    SetAcHint(acQuit                  , rsGHTOPO_QUIT);
   lbTopographie.Caption:= GetResourceString(rsMNU_TOPOGRAPHIE);
    SetAcHint(acCompileExt            , rsCOMPILE);
    SetAcHint(acCheckDatabase         , rsCHECKBASE);
    SetAcHint(acVue3D                 , rsVUE3D);
    SetAcHint(acViderCroquis          , rsVIDER_CROQUIS);
    SetAcHint(acExportGIS             , rsEXPORT_GIS);
    //SetAcHint(acRendu3D          , GetResourceString(rsRENDU3D));
    //SetAcHint(acStats            , GetResourceString(rsSTATISTIQUES));
    //SetAcHint(acNodesCoordinates , GetResourceString(rsNODESCOORDINATES));
    //SetAcHint(acMaillagesUtils   , GetResourceString(rsMAILLAGES_UTILS));
    //SetAcHint(acFenetreViseesRayonnantes   , GetResourceString(rsGESTION_VISEES_RADIANTES));
  //mnuHelp.Caption    := GetResourceString(rsMNU_HELP);
    SetAcHint(acHelp                  , rsABOUT);
  SetAcHint(acClearConsoleErreur      , rsCLEAR_CONSOLE_ERREUR);
  SetAcHint(acCopierConsoleErreur     , rsCOPY_CONSOLE_ERREUR);
  //*)
  (*
  mnuFenetre.Caption    := GetResourceString(rsMNU_WINDOW);
    SetAcHint(acFenetreConsole             , GetResourceString(rsWND_CONSOLE));
    SetAcHint(acFenetreSeries              , GetResourceString(rsWND_DATABASE));
    SetAcHint(acFenetreVueEnPlan           , GetResourceString(rsWND_PLAN));
    SetAcHint(acFenetreListesSimples       , GetResourceString(rsWND_LISTES_SIMPLES));

  mnuOutils.Caption     :=  GetResourceString(rsMNU_TOOLS);
    SetAcHint(acToolCalculette    , GetResourceString(rsDLG_CALC_TITLE));
  //*)
  // onglet Vue 2D
  SetAcHint(acZoomTout          , rsCDR_VUE2D_AC_ZOOM_ALL);
  SetAcHint(acZoomFenetre       , rsCDR_VUE2D_AC_ZOOM_WINDOW);
  SetAcHint(acPanVue            , rsCDR_VUE2D_AC_PAN_VUE);
  SetAcHint(acZoomPlus          , rsCDR_VUE2D_AC_ZOOM_PLUS);
  SetAcHint(acZoomMoins         , rsCDR_VUE2D_AC_ZOOM_MOINS);
  SetAcHint(acLocaliserStation  , rsCDR_VUE2D_AC_LOCALISER_STATION);
  // distoX
  SetAcHint(acDemanderUneMesure , rsCDR_VUE2D_AC_REQUEST_MESURE_DISTOX);
  // croquis
  SetAcHint(acSetModeTravailNone, rsCDR_VUE2D_AC_SET_MODE_TRAVAIL_NONE);
  SetAcHint(acNewAnnotation     , rsCDR_VUE2D_AC_NEW_ANNOTATION);
  SetAcHint(acNewPolyligne      , rsCDR_VUE2D_AC_NEW_POLYLINE);
  SetAcHint(acDeleteAnnotation  , rsCDR_VUE2D_AC_DELETE_ANNOTATION);
  SetAcHint(acDeletePolyline    , rsCDR_VUE2D_AC_DELETE_POLYLINE);
end;


function TRPIMainWnd.InitNewDocument(): boolean;
var
  QFileData, QFileKrobard: TStringDirectoryFilename;
begin
  result := False;
  if (FDocTopoOpenedAndReady) then begin
    if (not GHTopoQuestionOuiNon('Un document est déjà ouvert - Continuer')) then Exit;
  end;
  FCurrentDocTopoName := 'QCaveStub';
  QFileData      := GetGHTopoDirectory() + FCurrentDocTopoName + '.xtb';
  QFileKrobard   := GetGHTopoDirectory() + FCurrentDocTopoName + '.xml';
  Result         := OuvrirLaTopo(QFileData, QFileKrobard, True);
end;
function TRPIMainWnd.RestoreLastDocument(): boolean;
var
  QBackUpFolder, QFileData, QFileKrobard: TStringDirectoryFilename;
begin
  result := false;
  QBackUpFolder := GetGHTopoDirectory() + BACKUP_FOLDER_CURRENT_DOC_TOPO;
  AfficherMessage(Format('%s.RestoreLastDocument() from %s ', [ClassName, QBackUpFolder]));
  if (Not DirectoryExists(QBackUpFolder)) then
  begin
    ForceDirectories(QBackUpFolder);
    exit(false);
  end;
  FCurrentDocTopoName := BACKUP_ROOT_FILENAME;
  QFileData      := QBackUpFolder + PathDelim + FCurrentDocTopoName + '.xtb';
  QFileKrobard   := QBackUpFolder + PathDelim + FCurrentDocTopoName + '.xml';
  Result := OuvrirLaTopo(QFileData, QFileKrobard, false);
end;

function TRPIMainWnd.OuvrirLaTopo(const FData, FKrobard: TStringDirectoryFilename; const ForceReinitNewDoc: boolean): boolean;
var
  EPSG: TLabelSystemesCoordsEPSG;
  procedure RegenererEbaucheKrobard(const FC: TStringDirectoryFilename);
  var
    MyKrobard: TCroquisTerrain;
  begin
    MyKrobard := TCroquisTerrain.Create;
    try
      MyKrobard.Initialiser(FBDDEntites, FC);
      MyKrobard.SaveToXML(FC);
      MyKrobard.Finaliser();
    finally
      FreeAndNil(MyKrobard);
    end;
  end;
begin
  Result := false;
  if (ForceReinitNewDoc) then
  begin
    RegenererEbaucheXTB(FData, DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);
    RegenererEbaucheKrobard(FKrobard);
  end
  else
  begin
    if (not FileExistsUTF8(FData)) then
    begin
      showMessageFmt(GetResourceString(rsMSG_FILENOTFOUND), [FData]);
      Exit;
    end;
  end;
  try
    CloseAllDocuments();
    Result := ChargerLaTopoExt(FData);
    Result := ChargerLeKrobard(FKrobard);
    EPSG   := FDocumentToporobot.GetCodeEPSGSystemeCoordonnees();
    SetCurrentEPSGByCode(EPSG.CodeEPSG);
    result := True;
  except
  end;
end;
// fermer tous les documents
procedure TRPIMainWnd.CloseAllDocuments();
begin
  if Not(FDocTopoOpenedAndReady) then Exit;
  FDocTopoOpenedAndReady := False;
  FDocumentToporobot.ClearListeSeries();          // vidage des séries
  FDocumentToporobot.ViderTablesSimples;        // vidage autres tables
  FGraphe.Initialiser(FDocumentToporobot, FBDDEntites);
  FShortestPath.Initialiser(1, 1, 1, 1, '', clGray);
  self.Caption := MakeTitleMainWindowGHTopo('Untitled');
end;


// Chargement et calcul de la topo, et préparation des frontaux
function TRPIMainWnd.ChargerLaTopoExt(const FC: TStringDirectoryFileName): boolean;
begin
  Result := false;
  FDocumentToporobot.ReInitialiser(True);
  // renseigner les champs From et To à 1.1
  btnStationDepart.Caption  := '1.1';
  btnStationArrivee.Caption := '1.1';
  FShortestPath.Initialiser(1, 1,  1, 1, '', clRed);
  SetModeFonctionnement(mfgcSURVEYING);
  // activer le menu de sauvegarde
  acQSave.Visible := True;
  FDocumentToporobot.SetDatabaseName(FCurrentNomFichierXTB);
  // vérifier si c'est un fichier Text
  if (Pos('.text', LowerCase(FC))>0) then
  begin
    if (FDocumentToporobot.LoadFichierText(FC) < 0) then
    begin
      ShowMessage('Le fichier comporte des erreurs - Voir les consoles');
      FDocumentToporobot.Finaliser();   // Echec = on détruit l'objet
      Exit;
    end;
  end
  else if (Pos('.gtx', FC) > 0) then // fichier GHTopo XML ?
  begin
    if (FDocumentToporobot.LoadFromXML(FC) < 0) then
    begin
      ShowMessage('Le fichier comporte des erreurs. Il doit être audité dans un éditeur XML');
      FDocumentToporobot.Finaliser();
      Exit;
    end;
  end
  else
  begin // sinon, c'est un fichier supposé Tab ou XTB
    if (FDocumentToporobot.LoadFromXTB(FC) < 0) then
    begin
      ShowMessage('Le fichier comporte des erreurs - Voir le rapport');
      DisplayTextEditor(GetGHTopoDirectory() + ChangeFileExt(ExtractFileName(FDocumentToporobot.GetDatabaseName), '.err'), True);
      // Echec = on détruit l'objet
      FDocumentToporobot.Finaliser();
      Exit;
    end;
  end;
  self.Caption := MakeTitleMainWindowGHTopo(ExtractFileName(FC));
  RecalculerEtActualiser(True);
  self.Top  := 0;
  self.Left := 0;
  // tout est OK ?
  Result := True;
  FCurrentNomFichierXTB := FC;
  Application.ProcessMessages;
end;
function TRPIMainWnd.ChargerLeKrobard(const FC: TStringDirectoryFileName): boolean;
begin
  result := false;
  if (not(FileExistsUTF8(FC))) then Exit;
  Result := FCroquisTerrain.LoadFromXML(FC);
end;

procedure TRPIMainWnd.SetCurrentEPSGByCode(const C: integer);
begin
  SetCurrentEPSG(FConvertisseurCoordonnees.GetEPSGSystemeFromCodeEPSG(C));
end;

procedure TRPIMainWnd.SetCurrentEPSG(const C: TLabelSystemesCoordsEPSG);
begin
  FCurrentSystemeEPSG := C;
end;

procedure TRPIMainWnd.ReactualiserTables(const B: boolean);
begin
  AfficherMessage('ReactualiserTables()');
  CdrSerieIndependant1.RefreshTableaux();
  CdrListesSimples1.ListerListesSimples();
end;

procedure TRPIMainWnd.PreparerVue2D(const MF: TModeFonctionnementGHTopoContext2D; const FBD: TBDDEntites);
var
  FC: TCroquisTerrain;
begin
  if (FDocTopoOpenedAndReady) then
  begin
    GHTopoContext2DA1.Initialiser(MF,
                                  FDocumentToporobot,
                                  FBDDEntites,
                                  FCroquisTerrain,
                                  FGraphe,
                                  FShortestPath,
                                  FMaillage,
                                  nil,                     // QProcUseMetaFiltre: TProcedureOfObject;
                                  ReactualiserTables,      // QProcRefresh: TProcOfObjectWithOneBoolParameter;
                                  RecalculerEtActualiser,  // const QProcRecalculerReseau: TProcedureOfObject): boolean;
                                  nil);                    //  const QProcTransmitBasePoint: TProcOfObjectWithATBaseStation): boolean;
  end;
  GHTopoContext2DA1.CanDraw := FDocTopoOpenedAndReady;
  FC := GHTopoContext2DA1.GetPtrCroquisTerrain();
  SetPanelStyles(FC.CurrentIdxStylePolyligne);
end;

procedure TRPIMainWnd.PreparerCadreViseesAntennes(const FD: TToporobotStructure2012);
begin
  CdrListeViseesEnAntenne1.Initialiser(FD, 1);
end;

procedure TRPIMainWnd.RecalculerEtActualiser(const DoReinitDistoX: boolean);
var
  QCBG, QCHD: TPoint2Df;
begin
  FDocTopoOpenedAndReady := CalculerLeReseauExt();
  PreparerFrontalSeries();
  PreparerListesSimples();
  PreparerVue2D(FModeFonctionnementGHTopoContext2D, FBDDEntites);
  if (DoReinitDistoX) then PreparerCadreDistoX();
  PreparerCadreViseesAntennes(FDocumentToporobot);
  PageControl1.ActivePageIndex := 0; // on se pose sur le plan
end;
procedure TRPIMainWnd.SetPanelStyles(const QCurrStyleIdx: integer);
var
  FC: TCroquisTerrain;
  procedure MiouMiou(const Miou: TStaticText; const SP: TKrobardStylePolyligne; const QSelected: boolean);
  var
    EWE: Boolean;
  begin
    EWE := (SP.Closed OR SP.Filled);
    Miou.Caption      := IIF(QSelected, 'X', '');
    Miou.Hint         := Format('%s [Poly%s]', [SP.Name, IIF(EWE, 'gone', 'ligne')]);
    if (EWE) then
    begin
      Miou.BorderStyle  := sbsSingle;
      Miou.Color        := SP.FillColor;
      Miou.BorderWidth  := 5;
    end
    else
    begin
      Miou.BorderStyle  := sbsNone;
      Miou.Color        := SP.LineColor;
      Miou.BorderWidth  := 1;
    end;
  end;
begin
  FC := GHTopoContext2DA1.GetPtrCroquisTerrain();
  FC.SetCurrentIdxStylePolyligne(QCurrStyleIdx);
  MiouMiou(btnStyleObjet0, FC.GetStylePolyligne(0), (FC.CurrentIdxStylePolyligne = 0));
  MiouMiou(btnStyleObjet1, FC.GetStylePolyligne(1), (FC.CurrentIdxStylePolyligne = 1));
  MiouMiou(btnStyleObjet2, FC.GetStylePolyligne(2), (FC.CurrentIdxStylePolyligne = 2));
  MiouMiou(btnStyleObjet3, FC.GetStylePolyligne(3), (FC.CurrentIdxStylePolyligne = 3));
  MiouMiou(btnStyleObjet4, FC.GetStylePolyligne(4), (FC.CurrentIdxStylePolyligne = 4));

  MiouMiou(btnStyleObjet5, FC.GetStylePolyligne(5), (FC.CurrentIdxStylePolyligne = 5));
  MiouMiou(btnStyleObjet6, FC.GetStylePolyligne(6), (FC.CurrentIdxStylePolyligne = 6));
  MiouMiou(btnStyleObjet7, FC.GetStylePolyligne(7), (FC.CurrentIdxStylePolyligne = 7));
  MiouMiou(btnStyleObjet8, FC.GetStylePolyligne(8), (FC.CurrentIdxStylePolyligne = 8));
  MiouMiou(btnStyleObjet9, FC.GetStylePolyligne(9), (FC.CurrentIdxStylePolyligne = 9));
end;

//******************************************************************************
// onglet Séries // frontalSeries
procedure TRPIMainWnd.PreparerFrontalSeries();
begin
   CdrNavigateurSeries1.Initialiser(FDocumentToporobot,
                                    self.GotoSerieDisAllowed,
                                    self.GotoSerie,
                                    self.CreateNewSerie,
                                    self.ImplementerModifsSerie);
end;

procedure TRPIMainWnd.PreparerListesSimples();
begin
  CdrListesSimples1.InitialiserListeSimple(FDocumentToporobot, FBDDEntites, FMaillage, mbddENTRANCES);
end;

procedure TRPIMainWnd.GotoSerie();
var
  n: Integer;
  MySerie: TObjSerie;
begin
  n := CdrNavigateurSeries1.GetIdxSerie();
  MySerie := FDocumentToporobot.GetSerie(n);
  CdrSerieIndependant1.Initialise(FDocumentToporobot, FBDDEntites, MySerie, n);
end;
function TRPIMainWnd.GotoSerieDisAllowed(): Boolean;
begin
  Result := CdrSerieIndependant1.IsDataModified();
end;
procedure TRPIMainWnd.CreateNewSerie();
begin
  FDocumentToporobot.CreateNewSerie(1,0, -1, GetResourceString(rsNOUVELLE_SERIE));
end;
procedure TRPIMainWnd.ImplementerModifsSerie();
var
  n, QNbErreurs: Integer;
  MySerie: TObjSerie;
  //QOldIdxSerie: TNumeroSerie;
begin
  n := CdrNavigateurSeries1.GetIdxSerie();
  if (n = 0) then // DONE: la série 0 n'est pas modifiable
  begin
    ShowMessage(rsMSG_SERIE_INITIALISATION);
    Exit;
  end;
  MySerie := FDocumentToporobot.GetSerie(n);
  //QOldIdxSerie := MySerie.GetNumeroDeSerie();  // sauvegarder l'ancien numéro de série
  //ShowMessageFmt('Série %d sélectionnée: %d: %s', [n, MySerie.GetIndexSerie(), MySerie.GetNomSerie()]);
  if (CdrSerieIndependant1.ImplementerModifs(QNbErreurs)) then
  begin
    CdrSerieIndependant1.RefreshTableaux();
    MySerie := FDocumentToporobot.GetSerie(n);
    // ici, on ne reliste pas la table des séries.
    // comme une série est un pointeur,
    // les modifications apparaîtront dans la liste au prochain clic
    //CdrListeSeries1.Relister(0);
    // si le numéro de série a changé, on liste les antennes
    //if (QOldIdxSerie <> MySerie.GetNumeroDeSerie()) then
    //begin
      //CdrAntennes1.RemplirTableau();
    //end;
  end;
end;
//******************************************************************************
//type TProcDisplayProgression = procedure (const Etape: string; const Done, Starting, Ending: integer) of object;
procedure  TRPIMainWnd.DispProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
begin
  lbEtape.Caption       := Etape;
  ProgressBar1.Min      := Starting;
  ProgressBar1.Max      := Ending;
  ProgressBar1.Position := Done;
end;

function TRPIMainWnd.CalculerLeReseauExt(): boolean;
var
  CodeCalcul: TCodeDeCalcul;
begin
  Result := False;
  Application.ProcessMessages;
  SetVisibiliteProgressBar(true);
  CodeCalcul := TCodeDeCalcul.Create;
  try
    CodeCalcul.Initialiser(FDocumentToporobot, FBDDEntites, DispProgression);
    Result    := CodeCalcul.CalculComplet(False);
    Result    := True;
  finally
    CodeCalcul.Finaliser();
    lbEtape.Caption := GetResourceString(rsDONE_CALCUL);
    SetVisibiliteProgressBar(false);
    FreeAndNil(CodeCalcul); //.Free;
  end;
end;

procedure TRPIMainWnd.SetVisibiliteProgressBar(const B: boolean);
begin
  pnlProgression.Visible := B;
end;
//******************************************************************************
procedure TRPIMainWnd.acZoomFenetreExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.SetModeTravail(mtZOOM_PREMIER_COIN);
end;

procedure TRPIMainWnd.acZoomMoinsExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.ZoomFact(1 - 0.05);
end;

procedure TRPIMainWnd.acZoomPlusExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.ZoomFact(1 + 0.05);
end;
procedure TRPIMainWnd.acZoomToutExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.ResetVue();
end;

procedure TRPIMainWnd.btnExchangeClick(Sender: TObject);
var
  EWE: TCaption;
begin
  EWE := btnStationDepart.Caption;
  btnStationDepart.Caption := btnStationArrivee.Caption;
  btnStationArrivee.Caption := EWE;
  RecalculerShortestPath(btnStationDepart.Caption, btnStationArrivee.Caption);
end;

procedure TRPIMainWnd.btnModeTopoNavigChange(Sender: TObject);
begin

end;


procedure TRPIMainWnd.btnNewDocCancelClick(Sender: TObject);
begin
  FModeUseMiniDialogSaveXTB := aesNONE;
  pnlSaveAs.Visible := false;
end;

procedure TRPIMainWnd.btnNewDocOKClick(Sender: TObject);
var
  QFileNameData, QFileNameKrobard: String;
begin
  case FModeUseMiniDialogSaveXTB of
    aesNONE: pass;
    aesNEW_DOC:
    begin
      QFileNameData    := editCurrentDirectory.Directory + PathDelim + trim(editFileName.Text) + '.xtb';
      QFileNameKrobard := editCurrentDirectory.Directory + PathDelim + trim(editFileName.Text) + '.xml';
      if (FDocTopoOpenedAndReady) then
      begin
        if (not GHTopoQuestionOuiNon('Un document est déjà ouvert - Continuer')) then Exit;
      end;
      OuvrirLaTopo(QFileNameData, QFileNameKrobard, true);
      FModeUseMiniDialogSaveXTB := aesNONE;
      pnlSaveAs.Visible := false;
    end;
    aesSAVE_AS:
    begin
      QFileNameData    := editCurrentDirectory.Directory + PathDelim + trim(editFileName.Text) + '.xtb';
      QFileNameKrobard := editCurrentDirectory.Directory + PathDelim + trim(editFileName.Text) + '.xml';
      if (FileExistsUTF8(QFileNameData)) then
      begin
        if (not GHTopoQuestionOuiNon(Format(rsWARN_FILE_ALREADY_EXISTS, [QFileNameData]))) then
        begin
          FModeUseMiniDialogSaveXTB := aesNONE;
          pnlSaveAs.Visible := false;
          FCurrentDocTopoName := ExtractFileNameOnly(QFileNameData);
        end;
      end;
      FDocumentToporobot.SaveToXTB(QFileNameData, mtabEXTENDEDTAB, tfWINDOWS);
      FCroquisTerrain.SaveToXML(QFileNameKrobard);
      FModeUseMiniDialogSaveXTB := aesNONE;
      pnlSaveAs.Visible := false;
    end;
  end;
end;

procedure TRPIMainWnd.btnQuickOpenCancelClick(Sender: TObject);
begin
  pnlQuickOpen.Visible := false;
end;

procedure TRPIMainWnd.btnQuickOpenOKClick(Sender: TObject);
var
  QFileData, QFileKrobard: TStringDirectoryFilename;
begin
  // Contournement du bug X Window: boite de dialogue intégrée
  // TODO: Ajouter -dUseCThreads dans les options personnalisées du projet
  //       avant de valider ce contournement
  QFileData    := 'Topo001.xtb';
  QFileKrobard := 'Topo001.xml';
  if (0 = lsbFichiersEnCours.Count) then exit;
  SetVisibiliteProgressBar(True);

  QFileData    := lsbFichiersEnCours.FileName;
  QFileKrobard := ChangeFileExt(QFileData, '.xml');
  pnlQuickOpen.Visible := false;
  lbEtape.Caption := 'Chargement de ' + QFileData;
  Application.ProcessMessages;
  if (OuvrirLaTopo(QFileData, QFileKrobard, false)) then
  begin
    lbEtape.Caption := 'Ready';
    FCurrentDocTopoName := ExtractFileNameOnly(QFileData);
	end;
  SetVisibiliteProgressBar(false);
end;

procedure TRPIMainWnd.btnRecalcParcoursClick(Sender: TObject);
begin

end;



procedure TRPIMainWnd.btnStationDepartClick(Sender: TObject);
var
  QStation: TBaseStation;
  EWE: String;
begin
  QStation:= GHTopoContext2DA1.GetCurrentStationNearToMousePos();
  EWE := QStation.toString();
  if (SaisirIDStation('Départ', EWE)) then
  begin
    btnStationDepart.Caption := EWE;
    if (RecalculerShortestPath(EWE, btnStationArrivee.Caption)) then  btnStationDepart.Caption := EWE;
    SetModeFonctionnement(mfgcNAVIGATION);
  end;
end;
procedure TRPIMainWnd.btnStationArriveeClick(Sender: TObject);
var
  EWE: TCaption;
  QStation: TBaseStation;
begin
  QStation:= GHTopoContext2DA1.GetCurrentStationNearToMousePos();
  EWE := QStation.toString();
  if (SaisirIDStation('Arrivée', EWE)) then
  begin
    btnStationArrivee.Caption := EWE;
    if (RecalculerShortestPath(btnStationDepart.Caption, EWE)) then  btnStationArrivee.Caption := EWE;
    SetModeFonctionnement(mfgcNAVIGATION);
  end;
end;

procedure TRPIMainWnd.acPanVueExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.SetModeTravail(mtPAN_PREMIER_POINT);
end;

procedure TRPIMainWnd.acParametrerVueExecute(Sender: TObject);
var
  MyOnglet: TVue2DParams;
begin
  if (not GHTopoContext2DA1.CanDraw) then exit;
  MyOnglet := GHTopoContext2DA1.GetOngletByIndex(0);
  if (ParametrerOngletVue2D(MyOnglet)) then  GHTopoContext2DA1.PutOngletByIndex(0, MyOnglet, True);
end;

procedure TRPIMainWnd.btnStyleObjet0Click(Sender: TObject);
begin
  SetPanelStyles(0);
end;

procedure TRPIMainWnd.btnStyleObjet1Click(Sender: TObject);
begin
  SetPanelStyles(1);
end;

procedure TRPIMainWnd.btnStyleObjet2Click(Sender: TObject);
begin
  SetPanelStyles(2);
end;

procedure TRPIMainWnd.btnStyleObjet3Click(Sender: TObject);
begin
  SetPanelStyles(3);
end;

procedure TRPIMainWnd.btnStyleObjet4Click(Sender: TObject);
begin
  SetPanelStyles(4);
end;

procedure TRPIMainWnd.btnStyleObjet5Click(Sender: TObject);
begin
  SetPanelStyles(5);
end;

procedure TRPIMainWnd.btnStyleObjet6Click(Sender: TObject);
begin
  SetPanelStyles(6);
end;

procedure TRPIMainWnd.btnStyleObjet7Click(Sender: TObject);
begin
  SetPanelStyles(7);
end;

procedure TRPIMainWnd.btnStyleObjet8Click(Sender: TObject);
begin
  SetPanelStyles(8);
end;

procedure TRPIMainWnd.btnStyleObjet9Click(Sender: TObject);
begin
  SetPanelStyles(9);
end;

procedure TRPIMainWnd.Button4Click(Sender: TObject);
begin
  FBDDEntites.ExporterAntennesNuagePoints(GetGHTopoDirectory() + '_Nuageoints.ply');
end;

procedure TRPIMainWnd.chkModeFonctionnementChange(Sender: TObject);
begin
  if (chkModeFonctionnement.Checked) then SetModeFonctionnement(mfgcNAVIGATION)
                                     else SetModeFonctionnement(mfgcSURVEYING);
end;

procedure TRPIMainWnd.SetModeFonctionnement(const MF: TModeFonctionnementGHTopoContext2D);
var
  EWE: TBaseStation;
begin
  FModeFonctionnementGHTopoContext2D := MF;
  case FModeFonctionnementGHTopoContext2D of
    mfgcSURVEYING :
    begin
      chkModeFonctionnement.Caption := 'TOPO';
      pnlOutilsCroquis.Visible := True;
      pnlNavigation.Visible    := false;
      // et on centre sur la station courante
      EWE := GHTopoContext2DA1.GetCurrentStation();
      GHTopoContext2DA1.CentrerVueSurPointXY(EWE.PosStation.X, EWE.PosStation.Y, True, FCurrentStation.ToString());
    end;
    mfgcNAVIGATION:
    begin
      chkModeFonctionnement.Caption := 'NAVIG';
      pnlOutilsCroquis.Visible := false;
      pnlProgression.Visible   := True;
      lbEtape.caption          := 'Construction du graphe';
      FGraphe.Initialiser(FDocumentToporobot, FBDDEntites);
      FGraphe.ConstruireGraphe();
      RecalculerShortestPath(btnStationDepart.Caption, btnStationArrivee.Caption);
      pnlNavigation.Visible    := true;
      pnlProgression.Visible   := false;
    end;
  end;
  GHTopoContext2DA1.SetModeFonctionnement(FModeFonctionnementGHTopoContext2D);
end;
function TRPIMainWnd.RecalculerShortestPath(const StationDepart, StationArrivee: string): boolean;
var
  EE1, EE2: TBaseStation;
  Q1, Q2  , EWE: Boolean;
  Nb, i   : Integer;
  MyNode  : TBZClassNode;
begin
  result := false;
  Q1 := FBDDEntites.FindStationByCle(false, Trim(StationDepart) , EE1);
  Q2 := FBDDEntites.FindStationByCle(false, Trim(StationArrivee), EE2);
  if (not Q1) then
  begin
    ShowMessageFmt(GetResourceString(rsMSG_GRAPHE_POINT_NOT_FOUND), ['début', StationDepart]);
    Exit;
  end;
  if (not Q2) then
  begin
    ShowMessageFmt(GetResourceString(rsMSG_GRAPHE_POINT_NOT_FOUND), ['fin', StationArrivee]);
    Exit;
  end;
  FShortestPath.Initialiser(EE1.Entite_Serie, EE1.Entite_Station, EE2.Entite_Serie, EE2.Entite_Station, 'Vers la sortie', clRed);
  EWE := FGraphe.RechercherPlusCourtChemin(FShortestPath);
  Nb := FShortestPath.GetNbNoeuds();
  lbNbPathStations.caption := format('%d stations', [Nb]);
  lsbPathRoadMap.Clear;
  if (Nb > 2) then
  begin
    for i := 0 to Nb - 1 do
    begin
      MyNode := FGraphe.GetStation(FShortestPath.GetNoeud(i));
      lsbPathRoadMap.Items.add(MyNode.ToString());
    end;
    lsbPathRoadMap.ItemIndex := 0;
    DisplayBearingToNext(0);
    GHTopoContext2DA1.SetShortestPath(FShortestPath);
    GHTopoContext2DA1.RefreshDessin();
    result := True;
  end;
end;



procedure TRPIMainWnd.editFileNameChange(Sender: TObject);
begin
  pass;
end;

procedure TRPIMainWnd.editFileNameKeyPress(Sender: TObject; var Key: char);
begin
  if (not (Key in ['0' .. '9', '-', '_', 'A' .. 'Z', 'a'..'z'])) then Key := chr(0);
end;
end.


