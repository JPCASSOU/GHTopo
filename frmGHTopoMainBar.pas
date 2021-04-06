unit frmGHTopoMainBar;

// 06/10/2016: Nouveau frontal (demandé par Gros Minet)
// 26/12/2016: Refonte complète des listes d'actions et de la barre d'outils
// 20/02/2020: Point de contrôle temporel (contrôle de version)

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue

  Common,
  StructuresDonnees,
  ToporobotClasses2012,
  unitCroquisTerrain,
  UnitEntitesExtended,
  UnitClasseMaillage,
  UnitGraphes1,
  CodeCalculTopo,
  ConvertisseurJPC,
  UnitFichesTopo,
  CallDialogsStdVersion,
  unitUtilsComposants, UnitObjetSerie,
  IniFiles,
  LazFileUtils,
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, Menus, ActnList, ComCtrls, StdCtrls, ExtCtrls, Buttons, LCLType;

type

{ TGHTopoMainMenuBar }

 TGHTopoMainMenuBar = class(TForm)
    ac3DView: TAction;
    acFenetreSeries: TAction;
    acFenetreVueEnPlan: TAction;
    acAnalyseGrapheReseau: TAction;
    acFenetreViseesRayonnantes: TAction;
    acFenetreListesSimples: TAction;
    acLastFile1: TAction;
    acMaintenanceDocument: TAction;
    acQSave: TAction;
    acAjouterSeriesDepuisAutreDoc: TAction;
    acRedefineGCS: TAction;
    acSimplifyAntennesOfAll: TAction;
    acSimplifyAntennesOfSerie: TAction;
    acSimplifyAntennesOfStation: TAction;
    acRecalcDeclimagsOfExpes: TAction;
    acLoadMNT: TAction;
    acExportPlanSVG: TAction;
    acUtilFusionTopos: TAction;
    acCheckBase: TAction;
    acCompileExt: TAction;
    acCoupeDeveloppee: TAction;
    acDisplayErrorsXTB: TAction;
    acEditeur: TAction;
    acExportGHCaveDraw: TAction;
    acExportGIS: TAction;
    acExportPocketTopoText: TAction;
    acExportTherion: TAction;
    acExportVTopo: TAction;
    acFenetreConsole: TAction;
    acGenererDossierTherion: TAction;
    acHelpSystem: TAction;
    acInfosCavite: TAction;
    acMaillagesUtils: TAction;
    acNew: TAction;
    acOuvrirTopoExt: TAction;
    acPrint: TAction;
    acQuit: TAction;
    acRecentDocuments: TAction;
    acReload: TAction;
    acRendu3D: TAction;
    acSaveAs: TAction;
    acStats: TAction;
    acFermer: TAction;
    chkDoExportX3DV: TCheckBox;
    Image1: TImage;
    MainMenuActionsList: TActionList;
    acToolCalculette: TAction;
    ImageList1: TImageList;
    lbOperationEnCours: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    mnuExportTopoEnX3D: TMenuItem;
    mnuOutils: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem3: TMenuItem;
    mnuSandbox: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem45: TMenuItem;
    mnuSeparateurFusionTopos: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuFenetre: TMenuItem;
    mnuFichier: TMenuItem;
    mnuHelp: TMenuItem;
    mnuTopographie: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlTollBarFichier: TPanel;
    pnlTollBarOutils: TPanel;
    pnlTollBarFichierUtils: TPanel;
    pnlToolBarCompile: TPanel;
    pnlToolBar: TPanel;
    pnlProgression: TPanel;
    ProgressBar1: TProgressBar;
    SpeedButton11: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
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
    SpeedButton33: TSpeedButton;
    SpeedButton34: TSpeedButton;
    SpeedButton35: TSpeedButton;
    SpeedButton36: TSpeedButton;
    SpeedButton37: TSpeedButton;
    SpeedButton39: TSpeedButton;
    SpeedButton40: TSpeedButton;
    SpeedButton43: TSpeedButton;
    SpeedButton44: TSpeedButton;
    lbCurrentStation: TStaticText;
    SpeedButton45: TSpeedButton;
    SpeedButton46: TSpeedButton;
    SpeedButton47: TSpeedButton;
    SpeedButton9: TSpeedButton;
    lbCurrentCodeEPSG: TStaticText;
    procedure ac3DViewExecute(Sender: TObject);
    procedure acAjouterSeriesDepuisAutreDocExecute(Sender: TObject);
    procedure acAnalyseGrapheReseauExecute(Sender: TObject);
    procedure acExport2DODGExecute(Sender: TObject);
    procedure acExportPlanSVGExecute(Sender: TObject);
    procedure acFenetreListesSimplesExecute(Sender: TObject);
    procedure acFenetreSeriesExecute(Sender: TObject);
    procedure acFenetreViseesRayonnantesExecute(Sender: TObject);
    procedure acFenetreVueEnPlanExecute(Sender: TObject);
    procedure acHelpNewsExecute(Sender: TObject);
    procedure acLastFile1Execute(Sender: TObject);
    procedure acLoadMNTExecute(Sender: TObject);
    procedure acMaintenanceDocumentExecute(Sender: TObject);
    procedure acNodesCoordinatesExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acQSaveExecute(Sender: TObject);
    procedure acRecalcDeclimagsOfExpesExecute(Sender: TObject);
    procedure acRedefineGCSExecute(Sender: TObject);
    procedure acReloadExecute(Sender: TObject);
    procedure acSandBoxExecute(Sender: TObject);
    procedure acSimplifyAntennesOfStationExecute(Sender: TObject);
    procedure acSimplifyAntennesOfAllExecute(Sender: TObject);
    procedure acSimplifyAntennesOfSerieExecute(Sender: TObject);
    procedure acSnapShotExecute(Sender: TObject);
    procedure acUtilFusionToposExecute(Sender: TObject);
    procedure acCheckBaseExecute(Sender: TObject);
    procedure acCompileExtExecute(Sender: TObject);
    procedure acCoupeDeveloppeeExecute(Sender: TObject);
    procedure acExportGHCaveDrawExecute(Sender: TObject);
    procedure acExportGISExecute(Sender: TObject);
    procedure acExportPocketTopoTextExecute(Sender: TObject);
    procedure acExportTherionExecute(Sender: TObject);
    procedure acExportVTopoExecute(Sender: TObject);
    procedure acFenetreConsoleExecute(Sender: TObject);
    procedure acFermerExecute(Sender: TObject);
    procedure acFindstationExecute(Sender: TObject);
    procedure acGenererDossierTherionExecute(Sender: TObject);
    procedure acHelpSystemExecute(Sender: TObject);
    procedure acInfosCaviteExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acOuvrirTopoExtExecute(Sender: TObject);

    procedure acQuitExecute(Sender: TObject);
    procedure acRendu3DExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acStatsExecute(Sender: TObject);
    procedure acToolCalculetteExecute(Sender: TObject);
    procedure acVue3DFiltreeExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCurrentCodeEPSGClick(Sender: TObject);
    procedure MenuItem45Click(Sender: TObject);
    procedure MenuItem60Click(Sender: TObject);

    procedure mnuExportTopoEnX3DClick(Sender: TObject);
    procedure RecalculerEtActualiser(const P: boolean);
  private
    // BDD mémoire localisées
    FDocumentToporobot        : TToporobotStructure2012;
    FBDDEntites               : TBDDEntites;
    FCroquisTerrain           : TCroquisTerrain;
    FMonMaillage              : TMaillage;
    FConvertisseurCoordonnees : TConversionSysteme;

    // Système EPSG courant
    FCurrentSystemeEPSG       : TLabelSystemesCoordsEPSG;
    // série/station courants
    FCurrentStation           : TToporobotIDStation;
    FCurrentNomFichierXTB     : TStringDirectoryFileName; // Nom du fichier, sans le chemin
    FLastFichierTopoOpened    : TStringDirectoryFilename; // Dernier doc utilisé, chemin compris
    FDocTopoOpenedAndReady    : boolean; // doit être visible de partout
    FListeRecentFiles         : TStringList;
    // paramètres FTP
    FFTPParameters            : TFTPParameters;
    procedure ActiverMenus(const R90Mode: boolean);
    procedure AfficherProgression(const Etape: string; const Done, Starting, Ending: integer);
    function  CalculerLeReseauExt(): boolean;
    function  ChargerLaTopoExt(const FC: TStringDirectoryFileName; const DoCalc: boolean): boolean;
    procedure CloseAllDocuments();
    procedure CompilerLeReseauExt(const BoolDummy: boolean);


    procedure InitLesCaptions();
    procedure InitNewDocument(const DoConfirmation: boolean);
    procedure PreparerFrontal();
    procedure PreparerVue2D(const QFileName: TStringDirectoryFilename);
    function  OuvrirLaTopo(const F: TStringDirectoryFilename; const ForceReinitNewDoc: boolean): boolean;
    function  LoadSettings(): boolean;
    procedure SaveSettings();

    function  GetCurrentStation(): TToporobotIDStation;
    procedure SetCurrentStation(const S: TToporobotIDStation); overload;
    procedure SetCurrentStation(const S: TNumeroSerie; const P: integer; const L: string = ''); overload;

    procedure SetCurrentEPSGByCode(const C: integer);
    procedure SetCurrentEPSG(const C: TLabelSystemesCoordsEPSG);
    // export en X3D
    procedure ExporterCaviteMaillage(const QBDD: TBDDEntites; const QMaillage: TMaillage; const OutputFilename: TStringDirectoryFilename);
    procedure TransmitGotoError(const Ser: TNumeroSerie; const St: TNumeroStation);
    procedure CentrerBasepointSurPlan(const BP: TToporobotIDStation);
    procedure CentrerXYSurPlan(const QX, QY: double; const TagString: string);
  public
    { public declarations }
  end;

var
  GHTopoMainMenuBar: TGHTopoMainMenuBar;

implementation

{$R *.lfm}


{$DEFINE DISP_FRM_VUEENPLAN}
{.$UNDEF  DISP_FRM_VUEENPLAN}


uses
  unitAnalyseReseau2
    {$IFDEF MSWINDOWS}
      {$IFDEF DISP_FRM_JOURNAL},  frmJournal   {$ENDIF}
    {$ENDIF}         // console de suivi
    , frmLesListesSimples    // gestion des listes simples
    , frmFrontalSeries       // frontal liste des séries
    , frmVuePlan2D           // vue en plan
    ;

const
  EXT_X3D  = '.x3d';
  EXT_X3DV = EXT_X3D + 'v';



// fichier contenant les derniers docs utilisés
CONST LAST_DOCS_OPENDED_FILENAME = '00_GHTopo_Last_Files_Opened.lst';
{ TGHTopoMainMenuBar }

//******************************************************************************

procedure TGHTopoMainMenuBar.SetCurrentStation(const S: TToporobotIDStation);
begin
  SetCurrentStation(S.aSerie, S.aStation, S.aIDTerrain);
end;

procedure TGHTopoMainMenuBar.SetCurrentStation(const S: TNumeroSerie; const P: integer; const L: string = '');
begin
  FCurrentStation := MakeTToporobotIDStation(0, S, P, L);
  lbCurrentStation.Caption := Format(FMTSERST, [FCurrentStation.aSerie, FCurrentStation.aStation]);
end;

function TGHTopoMainMenuBar.GetCurrentStation(): TToporobotIDStation;
begin
  Result := FCurrentStation;
end;


//*)

procedure TGHTopoMainMenuBar.AfficherProgression(const Etape: string; const Done, Starting, Ending: integer);
begin
  if (Done mod 200 <> 0) then Exit;
  lbOperationEnCours.Caption := Etape;
  try
    ProgressBar1.Min := Starting;
    ProgressBar1.Max := Ending;
    ProgressBar1.Position := Done;
    Application.ProcessMessages;
  except
    pass;
  end;
end;



procedure TGHTopoMainMenuBar.FormCreate(Sender: TObject);
begin
  
  FCurrentNomFichierXTB  := '';
  FLastFichierTopoOpened := '';
  FCurrentStation := MakeTToporobotIDStation(0, 1, 0, '');

  // paramètres FTP
  FFTPParameters.HostName    := '';
  FFTPParameters.Port        := '';
  FFTPParameters.User        := '';


  // prêt à dessiner le plan
  FDocTopoOpenedAndReady := false;
  // point décimal
  DefaultFormatSettings.DecimalSeparator := '.';
  // liste des fichiers récents
  FListeRecentFiles := TStringList.Create;
  // création du conteneur ToporobotStructure
  FCurrentNomFichierXTB      := '';
  FDocumentToporobot         := TToporobotStructure2012.Create;
  FCroquisTerrain            := TCroquisTerrain.Create;
  FBDDEntites                := TBDDEntites.Create;
  FMonMaillage               := TMaillage.Create();
  FDocumentToporobot.ReInitialiser(True);
  //TODO: A voir si c'est indispensable
  //FBDDEntites.Initialiser(MakeTPoint3Df(0,0,0), clBlue, clMaroon);
  // création des fenêtres permanentes
  {$IFDEF MSWINDOWS}
     {$IFDEF DISP_FRM_JOURNAL} dlgProcessing      := TdlgProcessing.Create(Application);   {$ENDIF}
  {$ENDIF}
  frmGestionSeries := TfrmGestionSeries.Create(Application);  // frontal: Les séries
  frmListesSimples := TfrmListesSimples.Create(Application);  // frontal: Les listes (expés, codes, ...)
  frmVueEnPlan     := TfrmVueEnPlan.Create(Application);      // vue en plan

  AfficherMessage(GetResourceString(rsCHOOSELANGUAGE));
  // init captions
  afficherMessage(GetResourceString(rsMSGASSIGNCAPTIONS));
  InitLesCaptions();
  ActiverMenus(False);
  FConvertisseurCoordonnees := TConversionSysteme.Create;
  FConvertisseurCoordonnees.Initialiser();
  FCurrentSystemeEPSG := FConvertisseurCoordonnees.GetEPSGSystemeFromCodeEPSG(DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);
end;

procedure TGHTopoMainMenuBar.FormDestroy(Sender: TObject);
begin
  try
    FDocumentToporobot.Finaliser();
    FBDDEntites.Finaliser();
    FCroquisTerrain.Finaliser();
    FMonMaillage.Finaliser();
    FConvertisseurCoordonnees.Finaliser();
    FListeRecentFiles.Clear;

  finally
    FreeAndNil(FDocumentToporobot);//FDocumentToporobot.Free;
    FreeAndNil(FBDDEntites);//FBDDEntites.Free;
    FreeAndNil(FCroquisTerrain);
    FreeAndNil(FMonMaillage);//FMonMaillage.Free;
    FreeAndNil(FConvertisseurCoordonnees);//FConvertisseurCoordonnees.Free;
    FreeAndNil(FListeRecentFiles);//FListeRecentFiles.Free;
  end;
  inherited;
end;


procedure TGHTopoMainMenuBar.FormResize(Sender: TObject);
begin
  self.ClientHeight := pnlToolBar.Top + pnlToolBar.Height + 4 + lbCurrentStation.Height;
end;


procedure TGHTopoMainMenuBar.FormShow(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
begin
  {$IFDEF MSWINDOWS}
     {$IFDEF DISP_FRM_JOURNAL}
        {$IfDEF GROS_MINET}
          dlgProcessing.Hide;
        {$ELSE}
          dlgProcessing.Show;
        {$ENDIF}
     {$ENDIF}
  {$ENDIF}

  SetCurrentStation(1,0);
  self.Top:=0;
  self.left:=0;
  self.Width   := Screen.Width - 8;
  self.ClientHeight  := pnlToolBar.Height + 2 + pnlProgression.Height + 2;
  self.Caption := MakeTitleMainWindowGHTopo('Untitled');
  self.mnuTopographie.Visible:=False;
  // lecture des préférences
  LoadSettings();
  // instanciation du contexte topo compilée
  FBDDEntites.Initialiser(FDocumentToporobot.GetPositionDuPointZero(), clAqua, clGreen);
  FCroquisTerrain.Initialiser(FBDDEntites, 'MyCroquis001.xml');
  FMonMaillage.Initialiser(FBDDEntites);
  if (System.Paramcount = 0) then // Lancement de GHTopo de manière classique
  begin
    // Nouveau comportement: Affiche le sélecteur de fichiers au démarrage
    QFileName := '';
    Application.ProcessMessages;
    if (DoDialogOpenFileGHTopo(True, FListeRecentFiles, QFileName)) then
    begin
      lbOperationEnCours.Caption := 'Chargement de ' + QFileName;
      Application.ProcessMessages;
      if (OuvrirLaTopo(QFileName, false)) then
      begin
        FListeRecentFiles.Insert(0, QFileName);
        lbOperationEnCours.Caption := 'Ready';
	  end;
    end
    else  // Force la création d'un nouveau document
    begin
      OuvrirLaTopo(QFileName, true);
    end;
  end else
  begin
    QFileName := Trim(System.ParamStr(1));
    // Tentative d'ouverture avec création d'un nouveau doc si échec
    if (not OuvrirLaTopo(QFileName, False)) then OuvrirLaTopo(QFileName, True);
  end;

end;

procedure TGHTopoMainMenuBar.lbCurrentCodeEPSGClick(Sender: TObject);
begin
  acRedefineGCSExecute(self);
end;

procedure TGHTopoMainMenuBar.MenuItem45Click(Sender: TObject);
begin
  // Recherche dans la base
  SearchInGHTopoDatabase(FDocumentToporobot, FBDDEntites, 'toto', nil);

  if (GHTopoQuestionOuiNon('Quitter IMMEDIATEMENT GHTopo')) then Application.Terminate;
end;

procedure TGHTopoMainMenuBar.MenuItem60Click(Sender: TObject);
const
  FTP_PASSWORD        = 'G40+g41-f84';
  FTP_ROOT_DIRECTORY  = '/www/0000_TotoDummy';

var
  QValues: array of string;
  QFileNameSRC, QDirectoryTGT, QDirectorySRC, QDS: TStringDirectoryFilename;
  QDirectoryACreer    :TStringDirectoryFilename;
  QErrCode: integer;
  QErrMsg: string;
begin
  QDirectoryACreer    := '00000_AAA'; // Type
  ClearConsoleErreur();
  // Test de la connexion
  while (not FTP_TestConnexion(FFTPParameters, FTP_PASSWORD, QErrCode, QErrMsg)) do
  begin
    if (not GHTopoQuestionOuiNon(Format('Echec de connexion: [%d - %s] - Réessayer', [QErrCode, QErrMsg]))) then Exit;
  end;


  QDirectorySRC := GetGHTopoDirectory() + QDirectoryACreer;
  if (not DirectoryExists(QDirectorySRC)) then ShowMessage(QDirectorySRC);

  QDirectoryTGT := FTP_ROOT_DIRECTORY;

  FTP_CreateFolder(FFTPParameters, FTP_PASSWORD, QDirectoryTGT, QDirectoryACreer);
  FTP_CreateFolder(FFTPParameters, FTP_PASSWORD, QDirectoryTGT, '0000001');
  FTP_CreateFolder(FFTPParameters, FTP_PASSWORD, QDirectoryTGT, '0000001' + '/' + '01');
  FTP_CreateFolder(FFTPParameters, FTP_PASSWORD, QDirectoryTGT, '0000001' + '/' + '011');
  FTP_CreateFolder(FFTPParameters, FTP_PASSWORD, QDirectoryTGT, '0000004');
  ShowMessage('Après création des dossiers' + QDirectoryTGT);

  QDirectoryTGT  := FTP_ROOT_DIRECTORY + '/' + QDirectoryACreer;
  ShowMessage(' 001:' + QDirectoryTGT);
  QDS := QDirectorySRC + PathDelim;
  ShowMessage(' 002:' + QDirectoryTGT);

  SetLength(QValues, 4);
  ShowMessage(' 003:' + QDirectoryTGT);

  QValues[0] := QDS + 'Latresne_1-1_1-51.pdf';
  QValues[1] := QDS + 'MNTCoume1.mai';
  QValues[2] := QDS + 'FichesStations_001.pdf';
  QValues[3] := QDS + 'GrapheJS_Bidon.htm';
  ShowMessage(' 004:' + QDirectoryTGT);

  ShowMessage('Avant FTP_UploadMultiplesFiles(): ' + QDirectoryTGT);

  FTP_UploadMultiplesFiles(FFTPParameters, FTP_PASSWORD, QValues, QDirectoryTGT);
  ShowMessage('Après FTP_UploadMultiplesFiles(): ' + QDirectoryTGT);
  //*)
  (*
  SetLength(QValues, 3);
  QValues[0] := FFTPParameters.HostName;
  QValues[1] := FFTPParameters.Port;
  QValues[2] := FFTPParameters.User;

  if (InputQuery('Paramètres serveur FTP',
                 ['Host', 'Port', 'User'],
                 QValues)) then
  begin
    FFTPParameters.HostName := QValues[0];
    FFTPParameters.Port     := QValues[1];
    FFTPParameters.User     := QValues[2];
  end;
  //*)

end;

procedure TGHTopoMainMenuBar.mnuExportTopoEnX3DClick(Sender: TObject);
var
  QIdx: integer;
  QFileName: TStringDirectoryFilename;
begin
  QFileName := 'Cavite001' + EXT_X3D;
  if (DoDialogSaveFile('Volumes X3D (format XML)|*' + EXT_X3D, EXT_X3D, QFileName, QIdx)) then
  begin
    ExporterCaviteMaillage(FBDDEntites, FMonMaillage, QFileName);
  end;
end;




{*
var
  MyX3DOutPut: TSceneX3D;
  procedure TraceCavite(const MyCaveName: string);
  var
    v: Integer;
    E: TBaseStation;
    S: TExpe;
    C: TColor;
  begin
   for v := 0 to QBDD.GetNbEntitesVisees() - 1 do
   begin
     E := QBDD.GetEntiteVisee(v);
     S := QBDD.GetExpeByIndex(E.eExpe);
     C := QBDD.GetPalette256().GetColorByIndex(S.IdxCouleur);
     MyX3DOutPut.TraceMorceauTube(E, MakeTColorRGBA(C, 255));
   end;
  end;
  procedure TracerMaillage(const NomMaillage: string);
  var
    t, Nb: Integer;
  begin
   QMaillage.ExportX3D();
   if (not QMaillage.IsValidMaillage()) then Exit;
    MyX3DOutPut.BeginShape(NomMaillage);
      MyX3DOutPut.DescribeShapeMaterial(NomMaillage, MakeTColorRGBA(clGreen, 50));
      MyX3DOutPut.BeginIndexedFaceSetSection(false, false);
        MyX3DOutPut.BeginIndexFaceSetList();
          Nb := QMaillage.GetNbTriangles();
          for t := 0 to Nb - 1 do MyX3DOutPut.Write3DFace(QMaillage.GetTriangle(t));
        MyX3DOutPut.EndIndexFaceSetList();
        MyX3DOutPut.BeginCoordinatesList();
          Nb := QMaillage.GetNbVertex();
          for t := 0 to Nb - 1 do MyX3DOutPut.WriteVertex(QMaillage.GetVertex(t));
        MyX3DOutPut.EndCoordinatesList();
      MyX3DOutPut.EndIndexedFaceSetSection();
    MyX3DOutPut.EndShape(NomMaillage);
  end;
begin
  AfficherMessage(format('%s.ExportX3D(): %d entites, Output file: %s',
                        [ClassName,
                         QBDD.GetNbEntitesVisees(),
                         OutputFilename
                        ]));
  MyX3DOutPut := TSceneX3D.Create;
  try
    if (MyX3DOutPut.Initialiser(OutputFilename, 'MyCavite')) then
    begin
      MyX3DOutPut.TraceCube('MonCube', QBDD.GetCoinBasGauche(), QBDD.GetCoinHautDroit());
      TraceCavite('MyCavite');
      TracerMaillage('MyMaillage');
    end;
    MyX3DOutPut.Finaliser();
  finally
    FreeAndNil(MyX3DOutPut);
  end;
end;
// }

procedure TGHTopoMainMenuBar.ac3DViewExecute(Sender: TObject);
var
  MyFiltre: String;
begin
  if (FDocTopoOpenedAndReady) then
  begin
    MyFiltre := ''; //GHTopoContext2DA1.GetFiltreOfCurrentOnglet;
    DisplayVue3DGDIExt(FBDDEntites,
                       FDocumentToporobot,
                       frmVueEnPlan.GetMaillage(),
                       frmVueEnPlan.GetParamsOngletCourant());
  end;
end;

procedure TGHTopoMainMenuBar.acAjouterSeriesDepuisAutreDocExecute(Sender: TObject);
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  acQSaveExecute(self);   // sauvegarde de sécurité
  DisplayImportFromOtherDocumentGHTopo(FDocumentToporobot, FConvertisseurCoordonnees);
  acQSaveExecute(self);
end;

procedure TGHTopoMainMenuBar.acAnalyseGrapheReseauExecute(Sender: TObject);
var
  MyGraphe: TPathFindingGraphe;
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  MyGraphe := TPathFindingGraphe.Create;
  if (MyGraphe.Initialiser(FDocumentToporobot, FBDDEntites)) then
  begin
    if (MyGraphe.ConstruireGraphe()) then
    begin
      DisplayGrapheOfReseau(FBDDEntites, MyGraphe);
    end;
  end;
end;









procedure TGHTopoMainMenuBar.acCheckBaseExecute(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
     {$IFDEF DISP_FRM_JOURNAL}
       dlgProcessing.memoErreurs.Lines.Clear;
       dlgProcessing.ShowOnTop;
     {$ENDIF}
  {$ENDIF}
  FDocumentToporobot.CheckerLesDonneesTopo();
  frmListesSimples.SetModeBDD(mbddCHECK_ERRORS);

end;


procedure TGHTopoMainMenuBar.RecalculerEtActualiser(const P: boolean);
begin
  ActiverMenus(True);
  if (P) then FDocTopoOpenedAndReady := CalculerLeReseauExt();
  AfficherMessage('999');
  PreparerFrontal();                         // préparer le frontal
  PreparerVue2D(FCurrentNomFichierXTB);     // préparer la vue 2D
end;

procedure TGHTopoMainMenuBar.acCompileExtExecute(Sender: TObject);
begin
  CompilerLeReseauExt(false);
end;
// Le paramètre BoolDummy est bidon ici.
// Pour compatibilité avec la version correspondante de GHTopoRPI
procedure TGHTopoMainMenuBar.CompilerLeReseauExt(const BoolDummy: boolean);
var
  EWE: TVue2DParams;
  procedure MiouMiou(const Miou: TFormStyle); inline;
  begin
    {$IFDEF MSWINDOWS}
      {$IFDEF DISP_FRM_JOURNAL}
      dlgProcessing.FormStyle := Miou;
      {$ENDIF}
    {$ENDIF}
  end;
begin
  // on récupère les limites de dessin courantes
  if (FDocTopoOpenedAndReady) then EWE := frmVueEnPlan.GetParamsOngletCourant();
  MiouMiou(fsStayOnTop);
  RecalculerEtActualiser(true);
  MiouMiou(fsNormal);
  if (FDocTopoOpenedAndReady) then frmVueEnPlan.PutParamsOngletCourant(EWE);
end;

procedure TGHTopoMainMenuBar.acCoupeDeveloppeeExecute(Sender: TObject);
begin
  if (FDocTopoOpenedAndReady) then AfficherEditeurCoupeDeveloppee(FDocumentToporobot);
end;

procedure TGHTopoMainMenuBar.acExport2DODGExecute(Sender: TObject);
begin
  pass;
end;

procedure TGHTopoMainMenuBar.acExportPlanSVGExecute(Sender: TObject);
begin
  if (FDocTopoOpenedAndReady) then frmVueEnPlan.DemanderExportPlanToSVG();
end;

procedure TGHTopoMainMenuBar.acExportGHCaveDrawExecute(Sender: TObject);
var
  WU: TStringDirectoryFilename;
  QFilterIndex: integer;
  procedure MiouMiou(const QBDD: TBDDEntites; const FicGCP: string);
  begin
    QBDD.SetProcDisplayProgression(nil);
    QBDD.ExportForGHCaveDraw(FicGCP, '');
  end;
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  WU := ChangeFileExt(FCurrentNomFichierXTB, '.gcp');
  if (DoDialogSaveFile('GHCaveDraw Polygonal (*.gcp)|*.gcp', '.gcp', WU, QFilterIndex)) then MiouMiou(FBDDEntites, WU);
end;

procedure TGHTopoMainMenuBar.acExportGISExecute(Sender: TObject);
begin
  if (FDocTopoOpenedAndReady) then DisplayExportSIG(FDocumentToporobot, FBDDEntites);
end;

procedure TGHTopoMainMenuBar.acExportPocketTopoTextExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QIdxFilter: integer;
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  QFileName := ChangeFileExt(FCurrentNomFichierXTB, '.Text');
  if (DoDialogSaveFile('Toporobot TEXT (*.Text)|*.Text', '.Text', QFileName, QIdxFilter)) then
  begin
    FDocumentToporobot.ExporterVersToporobotTEXT(QFileName, SEUIL_LONGUEUR_MAXI_TOPOROBOT, false);
    FCurrentNomFichierXTB := QFileName;
    self.Caption := MakeTitleMainWindowGHTopo(FCurrentNomFichierXTB);
  end;
end;

procedure TGHTopoMainMenuBar.acExportTherionExecute(Sender: TObject);
var
  WU: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  WU := ExtractFileNameOnly(FDocumentToporobot.GetDatabaseName);
  if (DoDialogSaveFile('Therion centerlines (*.th)|*.th', '.th', WU, QFilterIndex)) then
  begin
    FDocumentToporobot.ExportVersTherionTH(WU);
  end;
end;

procedure TGHTopoMainMenuBar.acExportVTopoExecute(Sender: TObject);
var
  QFilterIndex: integer;
  WU: TStringDirectoryFilename;
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  WU := ExtractFileNameOnly(FDocumentToporobot.GetDatabaseName);
  if (DoDialogSaveFile('Visual Topo (*.tro)|*.tro', '.tro', WU, QFilterIndex)) then
    FDocumentToporobot.ExportVisualTopo(WU, false);
end;

procedure TGHTopoMainMenuBar.acFenetreConsoleExecute(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF DISP_FRM_JOURNAL}
      PositionnerFenetre(dlgProcessing, 10, 120, 500, Screen.Height - 230);
      {$IFDEF GROS_MINET}
         dlgProcessing.Show;
      {$ELSE}
         dlgProcessing.SetFocus;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

procedure TGHTopoMainMenuBar.acFenetreListesSimplesExecute(Sender: TObject);
begin
  PositionnerFenetre(frmListesSimples, 10, 140, Screen.Width - 500, Screen.Height - 300);
  frmListesSimples.SetFocus;
end;

procedure TGHTopoMainMenuBar.acFenetreSeriesExecute(Sender: TObject);
begin
  PositionnerFenetre(frmGestionSeries, 20, 120, Screen.Width - 300, Screen.Height - 260);
  frmGestionSeries.SetFocus;
end;

procedure TGHTopoMainMenuBar.acFenetreViseesRayonnantesExecute(Sender: TObject);
begin
  DispGestionDesViseesRayonnantes(FDocumentToporobot, FBDDEntites);
end;

procedure TGHTopoMainMenuBar.acFenetreVueEnPlanExecute(Sender: TObject);
begin
   {$IFDEF DISP_FRM_VUEENPLAN}
     PositionnerFenetre(frmVueEnPlan, 10, 120, Screen.Width - 20, Screen.Height - 200);
     frmVueEnPlan.SetFocus;
   {$ENDIF}
end;
procedure TGHTopoMainMenuBar.acFermerExecute(Sender: TObject);
begin
  case MessageDlg(GetResourceString(rsSAVECHANGES), mtConfirmation, [mbYES, mbNO, mbCancel], 0) of
    mrYes:
      begin
        acSaveAsExecute(self);
        InitNewDocument(false);
      end;
    mrNo:
      InitNewDocument(True);
    mrCancel:
      pass;
  end;
end;

procedure TGHTopoMainMenuBar.acFindstationExecute(Sender: TObject);
var
  QDoMatchExact: Boolean;
  QFindWhat: string;
  EWE: TBaseStation;
begin
  QDoMatchExact := false;
  if (DispDlgFindIDTerrain(FDocumentToporobot, QDoMatchExact, QFindWhat)) then
  begin
    if (FBDDEntites.FindStationByCle(QDoMatchExact, QFindWhat, EWE)) then
    begin
      // TODO Vue en plan:GHTopoContext2DA1.SetCurrentEntite(EWE);
      // TODO Vue en plan:GHTopoContext2DA1.CentrerVueSurPointXY(EWE.Une_Station_2_X, EWE.Une_Station_2_Y, True, EWE.oIDLitteral);
    end
    else ShowMessage(GetResourceString(rsMATCHNOTFOUND));
  end
  else
  begin
    ShowMessage(GetResourceString(rsMATCHNOTFOUND));
  end;
end;


procedure TGHTopoMainMenuBar.acGenererDossierTherionExecute(Sender: TObject);
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  with TSelectDirectoryDialog.Create(Application) do
  begin
    InitialDir := GetGHTopoDirectory();
    Options := Options + [ofOverwritePrompt];
    if (Execute) then
    begin
      FDocumentToporobot.GenererDossierTherion(FileName, 1000, 10.00);
    end;
  end;
end;

procedure TGHTopoMainMenuBar.acHelpNewsExecute(Sender: TObject);
begin
  pass;
end;

procedure TGHTopoMainMenuBar.acHelpSystemExecute(Sender: TObject);
begin
  DisplayHelpSystem('');
  {.$IFDEF CHINESE_MESSAGES}
  //RegisterChineseUser();
  {.$ENDIF}
end;



procedure TGHTopoMainMenuBar.acInfosCaviteExecute(Sender: TObject);
var
  LS: TStringList;
  WU: TStringDirectoryFilename;
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  LS := TStringList.Create;
  try
    FDocumentToporobot.GetInfosCavite(LS);
    if (LS.Count > 0) then
    begin
      WU := GetGHTopoDirectory() + '_miaou.txt';
      LS.SaveToFile(WU);
      DisplayTextEditor(WU, False);
    end;
  finally
    FreeAndNil(LS);//LS.Free;
  end;
end;

procedure TGHTopoMainMenuBar.acLastFile1Execute(Sender: TObject);
begin
  if (FDocTopoOpenedAndReady AND (not GHTopoQuestionOuiNon(rsDO_RELOAD_DOCUMENT))) then Exit;
  OuvrirLaTopo(acLastFile1.Caption, false);
end;

procedure TGHTopoMainMenuBar.acLoadMNTExecute(Sender: TObject);
begin
  if (FDocTopoOpenedAndReady) then frmVueEnPlan.DemanderChargementDeMNT();
end;

procedure TGHTopoMainMenuBar.acMaintenanceDocumentExecute(Sender: TObject);
var
  EPSG: TLabelSystemesCoordsEPSG;
begin
  DispMaintenanceDocument(FDocumentToporobot, FBDDEntites, FConvertisseurCoordonnees, CompilerLeReseauExt);
  EPSG := FDocumentToporobot.GetCodeEPSGSystemeCoordonnees();
  SetCurrentEPSGByCode(EPSG.CodeEPSG);
end;

procedure TGHTopoMainMenuBar.acNewExecute(Sender: TObject);
begin
  InitNewDocument(True);
end;

procedure TGHTopoMainMenuBar.acNodesCoordinatesExecute(Sender: TObject);
begin

end;

procedure TGHTopoMainMenuBar.InitNewDocument(const DoConfirmation: boolean);
var
  QFileName: TStringDirectoryFilename;
begin
  if (FDocTopoOpenedAndReady and DoConfirmation) then
  begin
    if (not GHTopoQuestionOuiNon('Un document est déjà ouvert - Continuer')) then Exit;
  end;
  (*
  if (FDocTopoOpenedAndReady) then begin
    if (not GHTopoQuestionOuiNon('Un document est déjà ouvert - Continuer')) then Exit;
  end;
  QFileName   := GetGHTopoDirectory() + 'QCaveStub.xtb';
  CloseAllDocuments;
  if (not FileExistsUTF8(QFileName)) then RegenererEbaucheXTB(QFileName);
  ChargerLaTopoExt(QFilename, True);
  ActiverMenus(True);
  //*)
  QFileName   := GetGHTopoDirectory() + 'QCaveStub.xtb';
  OuvrirLaTopo(QFileName, true);
  // TODO: Faire en sorte qu'un gros point apparaisse à titre d'invite sur le plan.
end;
function TGHTopoMainMenuBar.OuvrirLaTopo(const F: TStringDirectoryFilename; const ForceReinitNewDoc: boolean): boolean;
var
  QFileName: TStringDirectoryFilename;
begin
  Result := false;
  if (ForceReinitNewDoc) then
  begin
    QFileName := GetGHTopoDirectory() + 'QCaveStub.xtb';
    RegenererEbaucheXTB(QFileName, DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);
  end
  else
  begin
    QFileName := F;
    if (not FileExistsUTF8(QFileName)) then
    begin
      showMessageFmt(GetResourceString(rsMSG_FILENOTFOUND), [F]);
      Exit;
    end;
  end;
  CloseAllDocuments();
  FLastFichierTopoOpened := QFileName;
  ChargerLaTopoExt(QFileName, true);
  AfficherMessage('PRET');
  SetCurrentEPSG( FDocumentToporobot.GetCodeEPSGSystemeCoordonnees());
  ActiverMenus(True);
  Result := true;
end;


procedure TGHTopoMainMenuBar.acOuvrirTopoExtExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
begin
  if (FDocTopoOpenedAndReady AND (not GHTopoQuestionOuiNon(rsWARN_FILE_ALREADY_OPEN))) then Exit;
  QFileName := '';
  if (DoDialogOpenFileGHTopo(False, FListeRecentFiles, QFileName)) then
  begin
    lbOperationEnCours.Caption := 'Chargement de ' + QFileName;
    Application.ProcessMessages;
    if (OuvrirLaTopo(QFileName, false)) then
    begin
      FListeRecentFiles.Insert(0, QFileName);
      lbOperationEnCours.Caption := 'Ready';
	  end;
  end;
end;




procedure TGHTopoMainMenuBar.acPrintExecute(Sender: TObject);
begin
  DisplayCentreImpressionExt(FDocumentToporobot,
                             FBDDEntites,
                             FCroquisTerrain,
                             FMonMaillage,
                             frmVueEnPlan.GetParamsOngletCourant());
end;

procedure TGHTopoMainMenuBar.acQSaveExecute(Sender: TObject);
var
  MyFolderQSave: TStringDirectoryFilename;
  MyQSaveFileName: TStringDirectoryFilename;
begin
  MyFolderQSave := GetGHTopoDirectory() + MON_DOSSIER_QSAVES + PathDelim;
  ForceDirectories(MyFolderQSave);
  MyQSaveFileName := MyFolderQSave + MakeFilenameFromDate('QSave_', Now(), 'xtb');
  ShowMessage('QSave: ' + MyQSaveFileName);
  FDocumentToporobot.SaveToXTB(MyQSaveFileName, mtabEXTENDEDTAB, tfWINDOWS);
end;

procedure TGHTopoMainMenuBar.acRecalcDeclimagsOfExpesExecute(Sender: TObject);
begin
  FDocumentToporobot.RecalculerDeclinaisonsMagnetiquesExpes();
end;

procedure TGHTopoMainMenuBar.acRedefineGCSExecute(Sender: TObject);
var
  EWE: TLabelSystemesCoordsEPSG;
begin
  EWE := FCurrentSystemeEPSG;
  if (SelectCoordinatesSystem(FConvertisseurCoordonnees, EWE)) then
  begin
    FDocumentToporobot.SetCodeEPSGSystemeCoordonnees(EWE);     // on définit dans la BDD
    EWE := FDocumentToporobot.GetCodeEPSGSystemeCoordonnees(); // et on récupère (acquittement)
    SetCurrentEPSG(EWE);
  end;
end;

procedure TGHTopoMainMenuBar.acQuitExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TGHTopoMainMenuBar.acReloadExecute(Sender: TObject);
begin
  if (FDocTopoOpenedAndReady AND (not GHTopoQuestionOuiNon(rsDO_RELOAD_DOCUMENT))) then Exit;
  OuvrirLaTopo(FCurrentNomFichierXTB, True);
end;

procedure TGHTopoMainMenuBar.acRendu3DExecute(Sender: TObject);
begin
  {$IFDEF USE_VIEWER_OPENGL}
    if (FDocTopoOpenedAndReady) then DisplayVue3DOpenGLExt(FBDDEntites, FMonMaillage, '');
  {$ELSE}
    ShowMessage('Viewer OpenGL disabled');
  {$ENDIF}
end;

procedure TGHTopoMainMenuBar.acSandBoxExecute(Sender: TObject);
begin

end;

procedure TGHTopoMainMenuBar.acSimplifyAntennesOfStationExecute(Sender: TObject);
var
  QBasepoint: String;
  QSer: TNumeroSerie;
  QPt: integer;
begin
  QBasepoint := '1.1';
  if (InputQuery('Simplification antennes d''une station', 'Point topo', QBasepoint)) then
  QBasePoint := Trim(QBasePoint);
  QSer := 0;
  QPt  := 0;
  begin
    if (not FDocumentToporobot.FindPtTopoByCle(false, QBasePoint, QSer, QPt)) then exit;
    FDocumentToporobot.ResetMarkersToDelete();
    FDocumentToporobot.SetProcDisplayProgression(nil);
    FDocumentToporobot.SimplifyAntennesOfStation(QSer, QPt, 0.66);
    CompilerLeReseauExt(false);
  end;
end;

procedure TGHTopoMainMenuBar.acSimplifyAntennesOfAllExecute(Sender: TObject);
var
  Nb, i, EWE: Integer;
  WU: String;
  MySerie: TObjSerie;
begin
  Nb := FDocumentToporobot.GetNbSeries();
  FDocumentToporobot.SetProcDisplayProgression(nil);
  if (Nb = 0) then Exit;
  FDocumentToporobot.ResetMarkersToDelete();
  for i := 1 to Nb - 1 do
  begin
    MySerie := FDocumentToporobot.GetSerie(i);
    FDocumentToporobot.SimplifyAntennesOfSerie(MySerie, 0.55);
  end;
  EWE := FDocumentToporobot.PurgerTableAntennes();
  WU  := Format('%d antennes removed', [EWE]);
  AfficherMessageErreur(WU);
  CompilerLeReseauExt(false);
  ShowMessage(WU);
end;

procedure TGHTopoMainMenuBar.acSimplifyAntennesOfSerieExecute(Sender: TObject);
var
  QToto, WU: string;
  EWE, QIdx: Integer;
  MySerie: TObjSerie;
begin
  QToto := '1';
  if (InputQuery('Simplification antennes d''une série', 'Série', QToto)) then
  begin
    EWE := StrToIntDef(QToto, 1);
    if (FDocumentToporobot.GetSerieByNumeroSerie(EWE, MySerie, QIdx)) then
    begin
      FDocumentToporobot.SetProcDisplayProgression(nil);
      FDocumentToporobot.ResetMarkersToDelete();
      FDocumentToporobot.SimplifyAntennesOfSerie(MySerie, 0.55);
      EWE := FDocumentToporobot.PurgerTableAntennes();
      WU := Format('%d antennes removed', [EWE]);
      AfficherMessageErreur(WU);
      CompilerLeReseauExt(false);
      ShowMessage(WU);
    end
    else
      ShowMessage('Série introuvable');
  end;
end;

procedure TGHTopoMainMenuBar.acSaveAsExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  // sauvegarde horodatée (prévention d'un bug vicieux de TOpenDialog
  acQSaveExecute(self);
  //if (not FDocTopoOpenedAndReady) then Exit;
  QFileName := FCurrentNomFichierXTB;
  if (DoDialogSaveFile(GetResourceString(rsGHTOPO_FILE_FILTER_WO_TEXT), '.xtb', QFileName, QFilterIndex)) then
  begin
    lbOperationEnCours.Caption := 'Sauvegarde de ' + QFileName;
    Application.ProcessMessages;
    case QFilterIndex of
      1: FDocumentToporobot.SaveToXTB(QFileName,mtabEXTENDEDTAB, tfWINDOWS);
      2: FDocumentToporobot.SaveToXML(QFileName); // futur format standard de GHTopo
      3: FDocumentToporobot.SaveToXTB(QFileName,mtabTOPOROBOT, tfMAC);
      //4: FDocumentToporobot.SaveToJSON(QFileName);
    end; // case FilterIndex
    // actualisation du nouveau nom de fichier
    FCurrentNomFichierXTB := QFileName;
    self.Caption := MakeTitleMainWindowGHTopo(FCurrentNomFichierXTB);
    lbOperationEnCours.Caption := 'Ready';
  end;
end;

procedure TGHTopoMainMenuBar.acSnapShotExecute(Sender: TObject);
begin

end;

procedure TGHTopoMainMenuBar.acUtilFusionToposExecute(Sender: TObject);
begin
  (*
  ShowMessage('Implanter boutons Up/Down pour la liste des docs fusionnés' + #13#10 +
              'Voir avec la méthodo d''espaces de noms où une cavité est de la forme' + #13#10 +
              '123.45@Reseau_Trombe ou 12.56@"Puits de la Râpe"');
  //*)
  DispFusionTopographies(FDocumentToporobot, FConvertisseurCoordonnees);
end;


procedure TGHTopoMainMenuBar.acStatsExecute(Sender: TObject);
begin
  if (FDocTopoOpenedAndReady) then DisplayStatistiquesExt(FDocumentToporobot, FBDDEntites);
end;



procedure TGHTopoMainMenuBar.acToolCalculetteExecute(Sender: TObject);
var
  QCoords: TPoint2Df;
  QResultatCalcul, QDeclimag: double;
begin
  QCoords         := MakeTPoint2Df(0.00, 0.00);
  QResultatCalcul := 0.00;
  QDeclimag       := 0.00;
  DisplayCalculette(FDocumentToporobot, FBDDEntites, FMonMaillage, frmVueEnPlan.GetLassoDeSelection(), '', 0, QCoords, QResultatCalcul, QDeclimag);
end;

procedure TGHTopoMainMenuBar.acVue3DFiltreeExecute(Sender: TObject);
begin
  if (FBDDEntites.GetNbEntitesMetafiltrees() < 3) then
  begin
    ShowMessage(GetResourceString(rsMSG_METAFILTRE_ALL_REJECTED));
    Exit;
  end;
  DisplayVue3DGDIExt(FBDDEntites,
                     FDocumentToporobot,
                     frmVueEnPlan.GetMaillage(),
                     frmVueEnPlan.GetParamsOngletCourant());
end;

//******************************************************************************
procedure TGHTopoMainMenuBar.InitLesCaptions();
  procedure SetAcHint(const A: TAction; const H: string);
  var
    QS: String;
  begin
    QS := GetResourceString(H);
    A.Caption := QS;
    A.Hint    := EnlevePerluete(QS);
  end;
begin
   mnuFichier.Caption := GetResourceString(rsMNU_FILE);
    SetAcHint(acNew              ,rsNEW);
    SetAcHint(acOuvrirTopoExt    ,rsOPEN);
    SetAcHint(acSaveAs           ,rsSAVEAS);
    SetAcHint(acPrint            ,rsPRINT);
    SetAcHint(acEditeur          ,rsEDITTAB);
    SetAcHint(acExportVTopo      ,rsVTOPO);
    SetAcHint(acExportTherion    ,rsEXPORT_THERION);
    SetAcHint(acExportPocketTopoText ,rsEXPORT_TOPOROBOT);
    SetAcHint(acExportPlanSVG    , rsEXPORT_SVG );
    SetAcHint(acAjouterSeriesDepuisAutreDoc,rsAJOUT_SERIES_DEPUIS_AUTRE_DOC);
    SetAcHint(acGenererDossierTherion,rsGENERER_DOSSIER_THERION);
    SetAcHint(acExportGIS        ,rsEXPORT_GIS);
    SetAcHint(acExportGHCaveDraw ,rsGHCAVEDRAW);
    SetAcHint(acRecentDocuments  ,rsRECENT_DOCS);
    SetAcHint(acReload           ,rsRELOAD);
    SetAcHint(acDisplayErrorsXTB ,rsERRINFOXTB);
    SetAcHint(acFermer           ,rsCLOSE);
    SetAcHint(acQSave            ,rsQUICK_SAVE);
    SetAcHint(acQuit             ,rsGHTOPO_QUIT);

  mnuTopographie.Caption := GetResourceString(rsMNU_TOPOGRAPHIE);
    SetAcHint(acCheckBase        ,rsCHECKBASE);
    SetAcHint(acCompileExt       ,rsCOMPILE);
    SetAcHint(ac3DView           ,rsVUE3D);
    SetAcHint(acRendu3D          ,rsRENDU3D);
    SetAcHint(acStats            ,rsSTATISTIQUES);
    SetAcHint(acRecalcDeclimagsOfExpes, rsRECALCUL_DECLIMAG);
    SetAcHint(acLoadMNT          , rsCDR_VUE2D_AC_LOAD_MNT);
    //SetAcHint(acNodesCoordinates ,rsNODESCOORDINATES);

    SetAcHint(acMaillagesUtils   ,rsMAILLAGES_UTILS);
    SetAcHint(acFenetreViseesRayonnantes   ,rsGESTION_VISEES_RADIANTES);
    SetAcHint(acRedefineGCS                ,rsREDEF_COORD_SYSTEME);
  mnuFenetre.Caption    := GetResourceString(rsMNU_WINDOW);
    SetAcHint(acFenetreConsole             ,rsWND_CONSOLE);
    SetAcHint(acFenetreSeries              ,rsWND_DATABASE);
    SetAcHint(acFenetreVueEnPlan           ,rsWND_PLAN);
    SetAcHint(acFenetreListesSimples       ,rsWND_LISTES_SIMPLES);

  mnuOutils.Caption     := GetResourceString(rsMNU_TOOLS);
    SetAcHint(acToolCalculette    ,rsDLG_CALC_TITLE);

  mnuHelp.Caption       := GetResourceString(rsMNU_HELP);
    SetAcHint(acHelpSystem       ,rsABOUT);
//    SetAcHint(acHelpNews         ,rsHLPNEWS));
end;

procedure TGHTopoMainMenuBar.FormActivate(Sender: TObject);
begin
  //self.WindowState := ws;
end;


// activer/désactiver options de menus
procedure TGHTopoMainMenuBar.ActiverMenus(const R90Mode: boolean);
begin
  acSaveAs.Visible           := R90Mode;
  acPrint.Visible            := R90Mode;
  acEditeur.Visible          := R90Mode;
  acExportVTopo.Visible      := R90Mode;
  acExportGIS.Visible        := R90Mode;
  acExportTherion.Visible    := R90Mode;
  acGenererDossierTherion.Visible := R90Mode;
  acExportGHCaveDraw.Visible := R90Mode;
  acReload.Visible           := R90Mode;
  acDisplayErrorsXTB.Visible := R90Mode;
  mnuTopographie.Visible     := true;
  ac3DView.Visible           := R90Mode;
  acCompileExt.Visible       := R90Mode;
  acRendu3D.Visible          := R90Mode;
  acStats.Visible            := R90Mode;
  acInfosCavite.Visible      := R90Mode;
  acCoupeDeveloppee.Visible  := R90Mode;
  mnuFenetre.Visible         := True;
  acExportPocketTopoText.Visible := R90Mode;
  acCheckBase.Visible        := True;

  acFenetreConsole.Visible   := True;
  acFenetreVueEnPlan.Visible := R90Mode;
  acFenetreSeries.Visible    := R90Mode;
  acFenetreListesSimples.Visible        := R90Mode;
  acFenetreViseesRayonnantes.Visible    := R90Mode;
  mnuSeparateurFusionTopos.Visible      := R90Mode;
  acExportGHCaveDraw.Visible            := R90Mode;
  acMaintenanceDocument.Visible         := R90Mode;
  acExportPocketTopoText.Visible        := R90Mode;
  acRedefineGCS.Visible                 := True;
  // Bac à sable
  mnuSandbox.Visible                    := True;
  {$IFDEF GROS_MINET}
    acExportTherion.Visible               := false;
    acGenererDossierTherion.Visible       := false;
    mnuSandbox.Visible                    := false; // Bac à sable désactivé
    //acExportGHCaveDraw.Visible            := false;
  {$ENDIF}
end;




//******************************************************************************
function TGHTopoMainMenuBar.CalculerLeReseauExt(): boolean;
var
  CodeCalcul: TCodeDeCalcul;
  T0, T1: TDateTime;
begin
  Result := False;
  Application.ProcessMessages;
  AfficherMessage(Format('%s.CalculerLeReseauExt() [%s]', [ClassName, MULTI_OR_MONO_THREADED]));
  ProgressBar1.Visible := true;
  CodeCalcul := TCodeDeCalcul.Create;
  T0 := Now();
  try
    //ShowMessage('555');
    {$IfDef MULTI_THREADING}
    AfficherMessageErreur(Format('-- Calcul parallélisé sur %d threads', [NB_MAX_THREADS]));
    {$ELSE}
    AfficherMessageErreur('-- Calcul non parallélisé');
    {$ENDIF}
    AfficherMessageErreur('');

    CodeCalcul.Initialiser(FDocumentToporobot, FBDDEntites);
    //ShowMessage('556');


    //ShowMessage('557');

    //CodeCalcul.SetProcDisplayProgression(self.AfficherProgression);
    Result    := CodeCalcul.CalculComplet(False);
    //ShowMessage('665');


    AfficherMessageErreur(Format('%s.CalculerLeReseauExt: %s', [Classname, IIF(Result, 'OK', 'KO')]));

    //ShowMessage('666');

    CodeCalcul.Finaliser();
    Application.ProcessMessages;
    Result := True;
    ///ShowMessage('777');
  finally

    lbOperationEnCours.Caption := GetResourceString(rsDONE_CALCUL);
    ProgressBar1.Visible := false;
    FreeAndNil(CodeCalcul);//CodeCalcul.Free;
  end;
  T1 := Now();
  AfficherMessage('Calcul lancé à ' + DateTimePascalToDateTimeSQL(T0, True));
  AfficherMessage('Calcul terminé à ' + DateTimePascalToDateTimeSQL(T1, True));
  AfficherMessage('Temps de calcul: ' + DatePascalToDateSQL(T1 - T0));
    // calculer les dégradés
  ///ShowMessage('888');
  FBDDEntites.CalcCouleursByDepth(FBDDEntites.GetColorZMini(), FBDDEntites.GetColorZMaxi());
end;
// fermer tous les documents
procedure TGHTopoMainMenuBar.CloseAllDocuments();
begin
  if Not(FDocTopoOpenedAndReady) then Exit;
  frmVueEnPlan.GHTopoContext2DA1.CanDraw := false;

  AfficherMessage('CloseAllDocuments');
  FDocTopoOpenedAndReady := False;
  FDocumentToporobot.ClearListeSeries();          // vidage des séries
  FDocumentToporobot.ViderTablesSimples;        // vidage autres tables
  FCroquisTerrain.ViderLesListesObjets();
  //CloseVues;
  ActiverMenus(False);
  self.Caption := MakeTitleMainWindowGHTopo('Untitled');
end;


procedure TGHTopoMainMenuBar.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

    frmVueEnPlan.GHTopoContext2DA1.CanDraw := false;
    FDocTopoOpenedAndReady := false;
    SaveSettings();



end;






//******************************************************************************
// charger la topo
// Chargement de la topo
function TGHTopoMainMenuBar.ChargerLaTopoExt(const FC: TStringDirectoryFileName; const DoCalc: boolean): boolean;
begin
  Result := false;
  AfficherMessage(Format('%s.ChargerLaTopoExt: %s', [ClassName, FC]));
  FDocumentToporobot.ReInitialiser(True);
  {$IFDEF MSWINDOWS}
     {$IFDEF DISP_FRM_JOURNAL}
       dlgProcessing.lsbJournal.Clear;
       dlgProcessing.memoErreurs.Lines.Clear;
     {$ENDIF}
  {$ENDIF}

  // activer le menu de sauvegarde
  acSaveAs.Visible := True;
  FDocumentToporobot.SetDatabaseName(FCurrentNomFichierXTB);
  // vérifier si c'est un fichier Text
  if (Pos('.text', LowerCase(FC))>0) then
  begin
    if (FDocumentToporobot.LoadFichierText(FC) < 0) then
    begin
      //showmessage('gmj');
      AfficherMessageErreur('Le fichier comporte des erreurs - Voir les consoles');
      FDocumentToporobot.Finaliser;   // Echec = on détruit l'objet
      Exit;
    end;
  end
  else if (Pos('.gtx', FC) > 0) then // fichier GHTopo XML ?
  begin
    if (FDocumentToporobot.LoadFromXML(FC) < 0) then
    begin
      AfficherMessageErreur('Le fichier comporte des erreurs. Il doit être audité dans un éditeur XML');
      FDocumentToporobot.Finaliser;
      Exit;
    end;
  end
  else if (Pos('.txt', FC) > 0) then // fichier PocketTopo ?
  begin
    if (FDocumentToporobot.LoadFromPocketTopoTXT(FC) < 0) then
    begin
      AfficherMessageErreur('Le fichier comporte des erreurs. Il doit être audité dans un éditeur XML');
      FDocumentToporobot.Finaliser;
      Exit;
    end;
  end
  //*)
  else
  begin // sinon, c'est un fichier supposé Tab ou XTB
    if (FDocumentToporobot.LoadFromXTB(FC)<0) then
    begin
      AfficherMessageErreur('Le fichier comporte des erreurs - Voir le rapport');
      DisplayTextEditor(GetGHTopoDirectory() + ChangeFileExt(ExtractFileName(FDocumentToporobot.GetDatabaseName), '.err'), True);
      // Echec = on détruit l'objet
      FDocumentToporobot.Finaliser;
      Exit;
    end;
  end;
  //ShowMessageFmt('%d series', [FDocumentToporobot.GetNbSeries()]);
  self.Caption := MakeTitleMainWindowGHTopo(ExtractFileName(FC));
  RecalculerEtActualiser(true);

  self.Top  := 0;
  self.Left := 0;
  {$IFDEF MSWINDOWS}
    {$IFDEF DISP_FRM_JOURNAL} dlgProcessing.FormStyle:=fsNormal; {$ENDIF}
  {$ENDIF}
  // tout est OK ?
  Result := True;
  FCurrentNomFichierXTB := FC;
  Application.ProcessMessages;
end;


// callback appelé par la liste des erreurs de check
procedure TGHTopoMainMenuBar.TransmitGotoError(const Ser: TNumeroSerie; const St: TNumeroStation);
begin

end;

procedure TGHTopoMainMenuBar.CentrerBasepointSurPlan(const BP: TToporobotIDStation);
begin

  frmVueEnPlan.CentrerBasePointSurPlan(BP);

end;

procedure TGHTopoMainMenuBar.CentrerXYSurPlan(const QX, QY: double; const TagString: string);
begin
  frmVueEnPlan.CentrerXYSurPlan(QX, QY, TagString);
end;

//******************************************************************************
// préparation du frontal de la BDD
procedure TGHTopoMainMenuBar.PreparerFrontal();
begin
  Application.ProcessMessages;
  //showmessage('Egorger 200 curetons');
  AfficherMessage('Préparation du frontal de la base de données');
  AfficherMessage(Format('%d entrees; %d reseaux; %d secteurs; %d codes; %d expes; %d series; %d antennes',
                         [FDocumentToporobot.GetNbEntrances(),
                          FDocumentToporobot.GetNbReseaux(),
                          FDocumentToporobot.GetNbSecteurs(),
                          FDocumentToporobot.GetNbCodes(),
                          FDocumentToporobot.GetNbExpes(),
                          FDocumentToporobot.GetNbSeries(),
                          FDocumentToporobot.GetNbAntennes()]));

  // frontal des séries
  frmGestionSeries.Initialiser(FDocumentToporobot, FBDDEntites);
  //showmessage('Egorger 200 curetons 2');

  frmListesSimples.Initialiser(FDocumentToporobot, FBDDEntites, FMonMaillage,
                              TransmitGotoError,
                              CentrerBasepointSurPlan,
                              CentrerXYSurPlan);
  //showmessage('Egorger 200 curetons 3');

  // ....

  // et on affiche les frontals
  frmGestionSeries.Show;
  frmListesSimples.Show;
end;


procedure TGHTopoMainMenuBar.PreparerVue2D(const QFileName: TStringDirectoryFilename);
begin
  if (not frmVueEnPlan.InitialiserVue2D(FDocumentToporobot,
                                        FCroquisTerrain,
                                        FBDDEntites,
                                        FMonMaillage,
                                        QFileName,
                                        CompilerLeReseauExt)) then
  begin
    ShowMessage('Echec démarrage vue en plan');
  end;
  frmVueEnPlan.Show;
end;


//******************************************************************************

procedure TGHTopoMainMenuBar.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := GHTopoQuestionOuiNon(GetResourceString(rsGHTOPO_QUIT));
end;

// gestion des préférences
function TGHTopoMainMenuBar.LoadSettings(): boolean;
var
  INI: TIniFile;
  S, WU: String;
  EWE: LongInt;
  procedure QSetWndBounds(const Wnd: TForm);
  begin
    Wnd.Position := poDesigned;
    Wnd.Left   := INI.ReadInteger(INI_SETTINGS_WINDOWS_POSITIONS, Wnd.Name + 'Left'  , Wnd.Left);
    Wnd.Top    := INI.ReadInteger(INI_SETTINGS_WINDOWS_POSITIONS, Wnd.Name + 'Top'   , Wnd.Top);
    Wnd.Width  := INI.ReadInteger(INI_SETTINGS_WINDOWS_POSITIONS, Wnd.Name + 'Width' , Wnd.Width);
    Wnd.Height := INI.ReadInteger(INI_SETTINGS_WINDOWS_POSITIONS, Wnd.Name + 'Height', Wnd.Height);
  end;
begin
  Result := false;
  acLastFile1.Visible := false;
  // Create the object, specifying the the ini file that contains the settings
  INI := TINIFile.Create(GetGHTopoDirectory() + GHTOPO_STD_INI_FILENAME);

  // Put reading the INI file inside a try/finally block to prevent memory leaks
  try
    S := Trim(INI.ReadString(INI_SECTION_LAST_FILE_OPENED, INI_KEY_LAST_FILE_OPENED, ''));
    if (S <> '') then
    begin
      acLastFile1.Caption := S;
      acLastFile1.Visible := True;
    end;
    // les fenêtres
    QSetWndBounds(frmGestionSeries);
    QSetWndBounds(frmVueEnPlan);
    {$IFDEF MSWINDOWS} QSetWndBounds(dlgProcessing); {$ENDIF}
    QSetWndBounds(frmListesSimples);
    // le système de coordonnées par défaut
    EWE := INI.ReadInteger(INI_CURRENT_GCS, INI_CODE_EPSG, DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);
    SetCurrentEPSGByCode(EWE);
    // les fichiers récents
    FListeRecentFiles.Clear;
    WU := GetGHTopoDirectory() + LAST_DOCS_OPENDED_FILENAME;
    if (FileExistsUTF8(WU)) then FListeRecentFiles.LoadFromFile(WU);
    // les paramètres FTP
    FFTPParameters.HostName := INI.ReadString(INI_SECTION_FTP_SETTINGS, INI_FTP_HOSTNAME, DEFAULT_FTP_HOSTNAME);
    FFTPParameters.Port     := INI.ReadString(INI_SECTION_FTP_SETTINGS, INI_FTP_PORT    , DEFAULT_FTP_PORT);
    FFTPParameters.User     := INI.ReadString(INI_SECTION_FTP_SETTINGS, INI_FTP_User    , DEFAULT_FTP_USER);
    Result := true;
  finally
    FreeAndNil(INI);//INI.Free;     // After the ini file was used it must be freed to prevent memory leaks.
  end;
end;



procedure TGHTopoMainMenuBar.SaveSettings();
var
  INI: TIniFile;
  procedure QSaveWindowPosSize(const Wnd: TForm);
  begin
    INI.WriteInteger(INI_SETTINGS_WINDOWS_POSITIONS, Wnd.Name + 'Left'     , Wnd.Left);
    INI.WriteInteger(INI_SETTINGS_WINDOWS_POSITIONS, Wnd.Name + 'Top'      , Wnd.Top);
    INI.WriteInteger(INI_SETTINGS_WINDOWS_POSITIONS, Wnd.Name + 'Width'    , Wnd.Width);
    INI.WriteInteger(INI_SETTINGS_WINDOWS_POSITIONS, Wnd.Name + 'Height'   , Wnd.Height);
  end;
begin
  FListeRecentFiles.SaveToFile(GetGHTopoDirectory() + LAST_DOCS_OPENDED_FILENAME);
  // Create the object, specifying the the ini file that contains the settings
  INI := TINIFile.Create(GetGHTopoDirectory() + GHTOPO_STD_INI_FILENAME);
  try
    if (FLastFichierTopoOpened <> '') then INI.WriteString(INI_SECTION_LAST_FILE_OPENED, INI_KEY_LAST_FILE_OPENED, FLastFichierTopoOpened);
    QSaveWindowPosSize(frmGestionSeries);
    QSaveWindowPosSize(frmVueEnPlan);
    {$IFDEF MSWINDOWS} QSaveWindowPosSize(dlgProcessing); {$ENDIF}
    QSaveWindowPosSize(frmListesSimples);

    // CodeEPSG
    INI.WriteInteger(INI_CURRENT_GCS, INI_CODE_EPSG, FCurrentSystemeEPSG.CodeEPSG);
    INI.WriteString(INI_CURRENT_GCS, INI_NOM_EPSG, FCurrentSystemeEPSG.NomEPSG);
    // Paramètres serveur FTP
    INI.WriteString(INI_SECTION_FTP_SETTINGS, INI_FTP_HOSTNAME, FFTPParameters.HostName);
    INI.WriteString(INI_SECTION_FTP_SETTINGS, INI_FTP_PORT    , FFTPParameters.Port);
    INI.WriteString(INI_SECTION_FTP_SETTINGS, INI_FTP_User    , FFTPParameters.User);
  finally
    FreeAndNil(INI);//INI.Free; // After the ini file was used it must be freed to prevent memory leaks.
  end;
end;

procedure TGHTopoMainMenuBar.SetCurrentEPSGByCode(const C: integer);
begin
  SetCurrentEPSG(FConvertisseurCoordonnees.GetEPSGSystemeFromCodeEPSG(C));
end;

procedure TGHTopoMainMenuBar.SetCurrentEPSG(const C: TLabelSystemesCoordsEPSG);
begin
  FCurrentSystemeEPSG := C;
  lbCurrentCodeEPSG.Caption := format('EPSG:%d', [FCurrentSystemeEPSG.CodeEPSG]);
  lbCurrentCodeEPSG.Hint := FCurrentSystemeEPSG.NomEPSG;
end;

//******************************************************************************
procedure TGHTopoMainMenuBar.ExporterCaviteMaillage(const QBDD: TBDDEntites; const QMaillage: TMaillage; const OutputFilename: TStringDirectoryFilename);
begin
  pass;
end;

end.
