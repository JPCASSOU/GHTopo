unit unitGHTopoRPIMainWnd;
// Version simplifiée de GHTopo pour Raspberry PI avec écran 5 pouces
// 13/10/2016 : GHTopo ouvre un document vierge au démarrage
{$ERROR Inutilisé}
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitClassPalette,
  UnitListesSimplesWithGeneriques,
  CodeCalculTopo,
  ConvertisseurJPC,
  Common,
  CallDialogsStdVersion,
  unitUtilsComposants,
  CadreGHTopoContext2D,
  CadreListesSimples,
  CadreParamsOngletsVue2D,
  CadreViseesAntenne,
  CadreLesSeriesRPI,
  //cadreDistoX,
  //CadreEditSeriesFlatMode,
  math,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, ActnList, ComCtrls, ExtCtrls, Buttons, StdCtrls, PairSplitter, Grids, Types;

type

  { TGHTopoRPIMainWnd }

  TGHTopoRPIMainWnd = class(TForm)
    ac3DView: TAction;
    acCalibrerDistoX: TAction;
    acCapturePlan: TAction;
    acChargerMaillage: TAction;
    acCheckBase: TAction;
    acCompileExt: TAction;
    acDisplayDiagrammes: TAction;
    acDisplayListeSeries: TAction;
    acDisplayParametresVue: TAction;
    acDistance: TAction;
    acEditeurCoupeDeveloppee: TAction;
    acExport2DODG: TAction;
    acExport2DSVG: TAction;
    acExportDXF1: TAction;
    acFenetreConsole: TAction;
    acFermer: TAction;
    acFindstation: TAction;
    aclstVisu2D: TActionList;
    acMetaFiltre: TAction;
    acNew: TAction;
    acNil1: TAction;
    acOuvrirTopoExt: TAction;
    acPanVue: TAction;
    acPrint1: TAction;
    acQuit: TAction;
    acRefreshVue: TAction;
    acSandBox: TAction;
    acSaveAs: TAction;
    acStatistiques: TAction;
    acStats: TAction;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    Action5: TAction;
    acAboutGHTopo: TAction;
    acAddAnnotation: TAction;
    acRemoveAnnotation: TAction;
    Action6: TAction;
    acVerrouillerEcran: TAction;
    acnLstMainWnd: TActionList;
    acVue3DFiltree: TAction;
    acVue3DOpenGLFiltree: TAction;
    acZoomAll: TAction;
    acZoomMoins: TAction;
    acZoomPlus: TAction;
    acZoomWindow: TAction;
    btninterpreterRafaleDeMesures: TButton;
    btnUnlockClearPwd: TButton;
    btnUnlockFour: TButton;
    btnUnlockOne: TButton;
    btnUnlockThree: TButton;
    btnUnlockTry: TButton;
    btnUnlockTwo: TButton;
    Button1: TButton;
    Button10: TButton;
    btn800x480: TButton;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CdrAntennes1: TCdrAntennes;
    //CdrDistoX1: TCdrDistoX;
    CdrListesSimples1: TCdrListesSimples;
    cdrParamsOngletVue2D1: TcdrParamsOngletVue2D;
    CdrSeriesRPI1: TCdrSeriesRPI;
    cmbSystemesCoordonnees: TComboBox;
    Edit1: TEdit;
    editCommentaireEtude: TEdit;
    editNomEtude: TEdit;
    GHTopoContext2DA1: TGHTopoContext2DA;
    imglstMainWnd: TImageList;
    imglstVue2D: TImageList;
    lbCarUnlock: TStaticText;
    lbCommentaireEtude: TLabel;
    lbNomEtude: TLabel;
    lbSystemeDeCoordonnees: TLabel;
    memoRafaleActuelle: TMemo;
    memoToutesMesures: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlButtons: TPanel;
    pnlEcranDeverrouillage: TPanel;
    pnlParamsVue: TPanel;
    pnlToolBars: TPanel;
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
    SpeedButton19: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    StaticText1: TStaticText;
    TabSheet1: TTabSheet;
    tabshtDistoX: TTabSheet;
    tabshtGeneral: TTabSheet;
    tabShtListesSimples: TTabSheet;
    tabshtSeries: TTabSheet;
    tabshtViseesEnAntenne: TTabSheet;
    tabShtVueEnPlan: TTabSheet;
    procedure ac3DViewExecute(Sender: TObject);
    procedure acAboutGHTopoExecute(Sender: TObject);
    procedure acAddAnnotationExecute(Sender: TObject);
    procedure acCompileExtExecute(Sender: TObject);
    procedure acDisplayParametresVueExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acOuvrirTopoExtExecute(Sender: TObject);
    procedure acPanVueExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure acRemoveAnnotationExecute(Sender: TObject);
    procedure acSauvegrdExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acStatistiquesExecute(Sender: TObject);
    procedure Action5Execute(Sender: TObject);
    procedure acVerrouillerEcranExecute(Sender: TObject);
    procedure acZoomAllExecute(Sender: TObject);
    procedure acZoomMoinsExecute(Sender: TObject);
    procedure acZoomPlusExecute(Sender: TObject);
    procedure acZoomWindowExecute(Sender: TObject);
    procedure btn800x480Click(Sender: TObject);
    procedure btninterpreterRafaleDeMesuresClick(Sender: TObject);
    procedure btnUnlockClearPwdClick(Sender: TObject);
    procedure btnUnlockFourClick(Sender: TObject);
    procedure btnUnlockOneClick(Sender: TObject);
    procedure btnUnlockThreeClick(Sender: TObject);
    procedure btnUnlockTryClick(Sender: TObject);
    procedure btnUnlockTwoClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);

    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure lbDistoReadyClick(Sender: TObject);
    procedure lsbErreursClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure pnlEcranDeverrouillageClick(Sender: TObject);
    procedure pnlParamsVueClick(Sender: TObject);
    procedure pnlToolBarsClick(Sender: TObject);
    procedure tabshtSeriesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    { private declarations }
    // clé de déverrouillage
    FDocTopoOpenedAndReady: boolean;
    FNomFichierXTB    : TStringDirectoryFileName;
    FDocumentToporobot: TToporobotStructure2012;
    FTopoCompilee     : TBDDEntites;
    FCurrParams2D     : TVue2DParams;

    FC1, FC2          : TPoint2Df;
    FConvertisseurCoordonnees : TConversionSysteme;
    procedure ActiverMenus(const R90Mode: boolean);
    procedure AttrapperViseeDistoX;
    function CalculerLeReseauExt: boolean;
    function ChargerLaTopoExt(const FC: TStringDirectoryFileName; const DoCalc: boolean): boolean;
    procedure CloseAllDocuments();
    procedure InitLesCaptions();
    procedure InitNewDocument();
    procedure PreparerDistoX();
    procedure PreparerFrontal();
    procedure PreparerVue2D();
    procedure RecalculerEtActualiser(const P: boolean);
  public
    { public declarations }
  end;

var
  GHTopoRPIMainWnd: TGHTopoRPIMainWnd;

implementation

{$IFDEF MSWINDOWS}
uses
  frmJournal;         // console de suivi
{$ENDIF}

{$R *.lfm}
const
  INDEX_ONGLET_VUE_EN_PLAN = 4;
  PASSWD_DEVERROUILLAGE_ECRAN = '1234';
procedure TGHTopoRPIMainWnd.FormCreate(Sender: TObject);
begin
  // prêt à dessiner le plan
  FDocTopoOpenedAndReady := false;
  // point décimal
  DefaultFormatSettings.DecimalSeparator := '.';
  // création du conteneur ToporobotStructure
  FNomFichierXTB     := '';
  FDocumentToporobot := TToporobotStructure2012.Create;
  FDocumentToporobot.ReInitialiser(True);
  FTopoCompilee      := TBDDEntites.Create;
  {$IFDEF MSWINDOWS}
     dlgProcessing      := TdlgProcessing.Create(Application);
  {$ENDIF}

  //FListeQRStations   := TEnsembleFichesPointsTopo.Create;
  // création des fenêtres permanentes
  //{$IFDEF MSWINDOWS}                     dlgProcessing      := TdlgProcessing.Create(Application); {$ENDIF}

  //FVuePM             := TdlgProjectManagerWdScr.Create(Application);
  //dlgPickingVisees  := TdlgPickingVisees.Create(Application);
  //FVueEnPlanExt      := TfrmVisu2DExtended.Create(Application);
  AfficherMessage(GetResourceString(rsCHOOSELANGUAGE));
  // init captions
  afficherMessage(GetResourceString(rsMSGASSIGNCAPTIONS));
  InitLesCaptions();
  ActiverMenus(False);
  // OngletsVues
  tabshtGeneral.Caption            := GetResourceString(rsTBS_GENERAL);
  tabShtListesSimples.Caption      := GetResourceString(rsTBS_LISTES_SIMPLES);
    lbNomEtude.Caption             := GetResourceString(rsTBS_GENERAL_NOM);
    lbCommentaireEtude.Caption     := GetResourceString(rsTBS_GENERAL_OBS);
    lbSystemeDeCoordonnees.Caption := GetResourceString(rsTBS_GENERAL_SYST);
  tabshtSeries.Caption             := GetResourceString(rsTBS_SERIES_FLAT_TABLE);
  tabshtViseesEnAntenne.Caption    := GetResourceString(rsTBS_ANTENNES);
  tabshtDistoX.Caption             := GetResourceString(rsTBS_DISTO_X);




  //tabShtLesSeries.Caption   := GetResourceString(rsTBS_SERIES);
  //tabShtAntennes.Caption    := GetResourceString(rsTBS_ANTENNES);
  //tabShtSeriesFlat.Caption  := GetResourceString(rsTBS_SERIES_FLAT_TABLE);
  tabShtVueEnPlan.Caption   := GetResourceString(rsTBS_SERIES_VUE_EN_PLAN);
  //tabshtMaintenance.Caption := GetResourceString(rsTBS_MAINTENANCE);
  //FPalette256 := TPalette256.Create;
  //FPalette256.GenerateTOPOROBOTPalette;
  FConvertisseurCoordonnees := TConversionSysteme.Create;
  FConvertisseurCoordonnees.Initialiser;
end;

procedure TGHTopoRPIMainWnd.FormShow(Sender: TObject);
begin
  self.Top:=0;
  self.left:=0;
  self.Width   := Screen.Width - 8;
  self.Height  := Screen.Height - 76;
  pnlEcranDeverrouillage.Visible := false;
  {$IFDEF MSWINDOWS}
  dlgProcessing.Show;
  {$ENDIF}
  //PreparerFrontal();
  InitNewDocument();
  // cadre du DistoX
  tabshtDistoX.Visible := CdrDistoX1.InitialiserCadreDistoX();

end;



//procedure TGHTopoRPIMainWnd

procedure TGHTopoRPIMainWnd.InitLesCaptions;
  procedure SetAcHint(const A: TAction; const H: string);
  begin
    A.Hint    := GetResourceString(H);
    A.Caption := A.Hint;
  end;
var
  WU: String;
begin
   //mnuFichier.Caption := GetResourceString(rsMNU_FILE);
    SetAcHint(acNew              , GetResourceString(rsNEW));
    SetAcHint(acOuvrirTopoExt    , GetResourceString(rsOPEN));
    SetAcHint(acSaveAs           , GetResourceString(rsSAVEAS));
    SetAcHint(acFermer           , GetResourceString(rsCLOSE));

    SetAcHint(acQuit             , GetResourceString(rsGHTOPO_QUIT));

  //mnuTopographie.Caption:= GetResourceString(rsMNU_TOPOGRAPHIE);
    SetAcHint(acCheckBase        , GetResourceString(rsCHECKBASE));
    SetAcHint(acCompileExt       , GetResourceString(rsCOMPILE));
    SetAcHint(ac3DView           , GetResourceString(rsVUE3D));
end;

procedure TGHTopoRPIMainWnd.ac3DViewExecute(Sender: TObject);
var
  EWE: String;
begin
  if (not FDocTopoOpenedAndReady) then exit;
  EWE := GHTopoContext2DA1.GetFiltreOfCurrentOnglet();
  DisplayVue3DGDIExt(FTopoCompilee, EWE);
end;

procedure TGHTopoRPIMainWnd.acAboutGHTopoExecute(Sender: TObject);
begin
  DisplayHelpSystem('');
end;

procedure TGHTopoRPIMainWnd.acAddAnnotationExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtADD_ANNOTATION);
end;

procedure TGHTopoRPIMainWnd.acCompileExtExecute(Sender: TObject);
var
  WU: Boolean;
  QC1, QC2: TPoint2Df;
begin
  // on récupère les limites de dessin courantes
  if (FDocTopoOpenedAndReady) then
  begin
    WU := GHTopoContext2DA1.GetViewlimits(0, QC1, QC2);
  end;
  CalculerLeReseauExt;
  FDocTopoOpenedAndReady := GHTopoContext2DA1.Initialiser(FDocumentToporobot, FTopoCompilee, nil, RecalculerEtActualiser); //, FListeQRStations);
  if (FDocTopoOpenedAndReady and  WU)then
  begin
    GHTopoContext2DA1.SetViewLimits(QC1, QC2);
  end;
  ShowMessage('Calcul terminé');
end;

procedure TGHTopoRPIMainWnd.acDisplayParametresVueExecute(Sender: TObject);
begin
  pnlParamsVue.Visible := not pnlParamsVue.Visible;
end;

procedure TGHTopoRPIMainWnd.acNewExecute(Sender: TObject);
begin
  InitNewDocument();
end;

procedure TGHTopoRPIMainWnd.acOuvrirTopoExtExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
begin
  if (FDocTopoOpenedAndReady AND (not GHTopoQuestionOuiNon(rsWARN_FILEALREADYOPEN))) then Exit;

  QFileName := GetGHTopoDirectory() + 'Topographie1.xtb';
  if (DoDialogOpenFile(GetResourceString(rsGHTOPO_FILE_FILTER_W_TEXT), '.xtb', QFileName)) then
  begin
    CloseAllDocuments();
    ChargerLaTopoExt(QFilename, GHTopoQuestionOuiNon(rsMSG_QUESTION_RECALCULER));
    AfficherMessage('PRET');
    ActiverMenus(True);
    PageControl1.ActivePageIndex := INDEX_ONGLET_VUE_EN_PLAN;
  end;
end;

procedure TGHTopoRPIMainWnd.acPanVueExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.GetCanDraw) then GHTopoContext2DA1.SetModeTravail(mtPAN_PREMIER_POINT);
end;

procedure TGHTopoRPIMainWnd.acQuitExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TGHTopoRPIMainWnd.acRemoveAnnotationExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtSELECT_ANNOTATION);
end;

procedure TGHTopoRPIMainWnd.acSauvegrdExecute(Sender: TObject);
begin

end;

procedure TGHTopoRPIMainWnd.acSaveAsExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  if (not FDocTopoOpenedAndReady) then Exit;
  QFileName := FNomFichierXTB;
  if (DoDialogSaveFile(GetResourceString(rsGHTOPO_FILE_FILTER_WO_TEXT), '.xtb', QFileName, QFilterIndex)) then
  begin
    case QFilterIndex of
      1: FDocumentToporobot.SaveToFile(QFileName,mtabEXTENDEDTAB, tfWINDOWS);
      2: FDocumentToporobot.SaveToXML(QFileName); // futur format standard de GHTopo
      3: FDocumentToporobot.SaveToFile(QFileName,mtabTOPOROBOT, tfMAC);
    end; // case FilterIndex
    // actualisation du nouveau nom de fichier
    FNomFichierXTB := QFileName;
    self.Caption := MakeTitleMainWindowGHTopo(FNomFichierXTB);
  end;
end;

procedure TGHTopoRPIMainWnd.acStatistiquesExecute(Sender: TObject);
begin
  DisplayStatistiquesExt(FDocumentToporobot, FTopoCompilee);
end;

procedure TGHTopoRPIMainWnd.Action5Execute(Sender: TObject);
begin

end;


procedure TGHTopoRPIMainWnd.RecalculerEtActualiser(const P: boolean);
begin
  if (P) then FDocTopoOpenedAndReady := CalculerLeReseauExt();
  // préparer le frontal
  PreparerFrontal();
  // préparer la vue 2D
  PreparerVue2D();
  //rendre visibles toutes les options des menus
  ActiverMenus(True);
  // et on se positionne sur la vue en plan
  PageControl1.ActivePageIndex := INDEX_ONGLET_VUE_EN_PLAN;


end;

procedure TGHTopoRPIMainWnd.tabshtSeriesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin

end;


// activer/désactiver options de menus
procedure TGHTopoRPIMainWnd.ActiverMenus(const R90Mode: boolean);
begin
  acSaveAs.Visible           := R90Mode;
  acCheckBase.Visible        := R90Mode;
  ac3DView.Visible           := R90Mode;
  acCompileExt.Visible       := R90Mode;
  acStats.Visible            := R90Mode;
  acCalibrerDistoX.Visible   := True;    // Plus aucun lien avec un doc topo
  acFenetreConsole.Visible   := True;
  PageControl1.ActivePageIndex := 2;
end;

procedure TGHTopoRPIMainWnd.acVerrouillerEcranExecute(Sender: TObject);
begin
  pnlEcranDeverrouillage.SetBounds(0, 0, self.ClientWidth, self.ClientHeight);
  pnlEcranDeverrouillage.Visible := True;
end;

procedure TGHTopoRPIMainWnd.acZoomAllExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.GetCanDraw) then GHTopoContext2DA1.ResetVue();
end;

procedure TGHTopoRPIMainWnd.acZoomMoinsExecute(Sender: TObject);
begin
  GHTopoContext2DA1.ZoomFact(1.0 - 0.05);
end;

procedure TGHTopoRPIMainWnd.acZoomPlusExecute(Sender: TObject);
begin
  GHTopoContext2DA1.ZoomFact(1.0 + 0.05);
end;

procedure TGHTopoRPIMainWnd.acZoomWindowExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.GetCanDraw) then GHTopoContext2DA1.SetModeTravail(mtZOOM_PREMIER_COIN);
end;

procedure TGHTopoRPIMainWnd.btn800x480Click(Sender: TObject);
begin
  self.Width  := 796;
  self.Height := 470;
end;

//* code à réorganiser par la suite
procedure TGHTopoRPIMainWnd.btninterpreterRafaleDeMesuresClick(Sender: TObject);
const
  ToleranceLongueurs : double = 0.20;
  ToleranceAngles    : double = 1.500;
  COLONNE_LONGUEUR   : integer = 0;
  COLONNE_AZIMUT     : integer = 1;
  COLONNE_PENTE      : integer = 2;
var
  RafaleDeMesures : TListeSimple<TMesureViseeDistoX>;
  MesuresAMoyenner: TListeSimple<TMesureViseeDistoX>;
  MDX             , MDX0, MDX1, MDX2, MyMesureDistoX: TMesureViseeDistoX;
  n, i, j: Integer;
  WU, QFileName: String;
  EWE: TStringArray;
  DeltaLongueur, DeltaAzimut, DeltaPente: double;
  QQ: Boolean;
  QLongueur, QAzimut, QPente: float;
begin
  // désactiver l'acquisition
  CdrDistoX1.SetWaitingMesuresDistoX(false);
  ShowMessage('0001');
  // préparer le mémo tampon
  memoRafaleActuelle.Clear;
  n := CdrDistoX1.getNbMesuresDistoX();
  ShowMessagefmt('0002: %d mesures', [n]);
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    MyMesureDistoX := CdrDistoX1.GetViseefromDistoX(i) ;
    WU := format(FORMAT_NB_INTEGER + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC,
                 [i,
                  MyMesureDistoX.Longueur,
                  MyMesureDistoX.Azimut,
                  MyMesureDistoX.Pente
                 ]);
    memoRafaleActuelle.Lines.Add(WU);
  end;
  n := memoRafaleActuelle.Lines.Count;

  AfficherMessage(Format('%s.TraiterRafaleDeMesures(%d lignes)', [ClassName, n]));
  if (n = 0) then Exit;
  PageControl1.ActivePageIndex := INDEX_ONGLET_VUE_EN_PLAN;
  MesuresAMoyenner := TListeSimple<TMesureViseeDistoX>.Create;
  RafaleDeMesures  := TListeSimple<TMesureViseeDistoX>.Create;
  try
    RafaleDeMesures.ClearListe();
    MesuresAMoyenner.ClearListe();
    DeltaLongueur := 666.00;
    DeltaAzimut   := 666.00;
    DeltaPente    := 666.00;
    // première passe = traduction du tampon
    for i := 0 to n - 1 do
    begin
      AfficherMessage(WU);
      WU := Trim(memoRafaleActuelle.Lines[i]);
      if (WU = '') then Continue;
      if (WU[1] = '#') then Continue;
      WU := StringReplace(WU, ' ', ';', [rfReplaceAll, rfIgnoreCase]);
      WU := StringReplace(WU, #9, ';', [rfReplaceAll, rfIgnoreCase]);
      EWE := Split(WU, ';');

      AfficherMessageErreur('--' + WU);
      MDX.Block              := 0;
      MDX.Segment            := 0;
      MDX.IsMarked           := false;
      MDX.DistoXSerialNumber := StrToIntDef(EWE[0], 0);
      MDX.HexaData           := EWE[1];
      //MDX.TimeStamp          := ConvertirEnNombreReel(EWE[2], Now());

      MDX.Longueur           := ConvertirEnNombreReel(EWE[COLONNE_LONGUEUR], -1.00);
      MDX.Azimut             := ConvertirEnNombreReel(EWE[COLONNE_AZIMUT]  , -1.00);
      MDX.Pente              := ConvertirEnNombreReel(EWE[COLONNE_PENTE]   , -666.00);
      MDX.TypeMesure         := tmdxUNKNOWN;

      RafaleDeMesures.AddElement(MDX);
    end;
    // contrôle
    for i := 0 to RafaleDeMesures.GetNbElements() - 1 do
    begin
      MDX := RafaleDeMesures.GetElement(i);
      AfficherMessageErreur(Format('%d: %s - L=%.3f, A=%.3f, P=%.2f', [i, Date(MDX.TimeStamp),
                                                                      MDX.Longueur, MDX.Azimut, MDX.Pente]));
    end;
    // seconde passe
    i := 2;
    while (i < RafaleDeMesures.GetNbElements()) do
    begin
      MDX0 := RafaleDeMesures.GetElement(i - 2);
      MDX1 := RafaleDeMesures.GetElement(i - 1);
      MDX2 := RafaleDeMesures.GetElement(i);
      DeltaLongueur := maxvalue([abs(MDX2.Longueur - MDX0.Longueur),
                                 abs(MDX2.Longueur - MDX1.Longueur),
                                 abs(MDX1.Longueur - MDX0.Longueur)]);
      DeltaAzimut   := maxvalue([abs(MDX2.Azimut - MDX0.Azimut),
                                 abs(MDX2.Azimut - MDX1.Azimut),
                                 abs(MDX1.Azimut - MDX0.Azimut)]);
      DeltaPente    := maxvalue([abs(MDX2.Pente - MDX0.Pente),
                                 abs(MDX2.Pente - MDX1.Pente),
                                 abs(MDX1.Pente - MDX0.Pente)]);

      QQ := (DeltaLongueur < ToleranceLongueurs) and
            (DeltaAzimut   < ToleranceAngles) and
            (DeltaPente    < ToleranceAngles);
      AfficherMessageErreur(Format('dx = %.3f, dy=%.3f, dz=%.3f', [DeltaLongueur, DeltaAzimut, DeltaPente]));
      if (QQ) then
      begin
        QLongueur := mean([MDX0.Longueur, MDX1.Longueur, MDX2.Longueur]);
        QAzimut   := mean([MDX0.Azimut, MDX1.Azimut, MDX2.Azimut]);
        QPente    := mean([MDX0.Pente, MDX1.Pente, MDX2.Pente]);
        // on affiche la lomgueur moyenne
        AfficherMessageErreur(Format('*** VISEE *** L = %.3f; Az = %.3f, P = %.3f', [QLongueur, QAzimut, QPente]));
        // on ajoute la visée
        GHTopoContext2DA1.AjouterUneViseeALaStationCourante(tgDEFAULT,
                                                            QLongueur, QAzimut, QPente,
                                                            0.5, 0.5, 0.5, 0.5,
                                                            '',
                                                            '');
      end
      else
      begin
        // dépiler la mesure
        //RafaleDeMesures.RemoveElement(0);
        GHTopoContext2DA1.AjouterUneViseeALaStationCourante(tgVISEE_RADIANTE,
                                                            MDX2.Longueur, MDX2.Azimut, MDX2.Pente,
                                                            0.0, 0.0, 0.0, 0.0,
                                                            '',
                                                            '');
      end;
      i += 1;

    end;

    RafaleDeMesures.ClearListe();
    MesuresAMoyenner.ClearListe();
    // on sauvegarde la topo
    QFileName := Format('CurrentTopo_%s.csv', [DatePascalToDateHeureCondensee(Now())]);
    FDocumentToporobot.SaveToFile(QFileName, mtabEXTENDEDTAB, tfWINDOWS);
    // et on vide le mémo
    memoRafaleActuelle.Clear;
    // les mesures DistoX
    CdrDistoX1.ViderLesMesuresDistoX();
    // avant de réactiver l'attente de mesures
    CdrDistoX1.SetWaitingMesuresDistoX(true);
  finally
    RafaleDeMesures.Free;
    MesuresAMoyenner.Free;
  end;
end;

procedure TGHTopoRPIMainWnd.btnUnlockClearPwdClick(Sender: TObject);
begin
  lbCarUnlock.Caption := '';
end;

procedure TGHTopoRPIMainWnd.btnUnlockFourClick(Sender: TObject);
begin
  lbCarUnlock.Caption := lbCarUnlock.Caption + '4';
end;

procedure TGHTopoRPIMainWnd.btnUnlockOneClick(Sender: TObject);
begin
  lbCarUnlock.Caption := lbCarUnlock.Caption + '1';
end;

procedure TGHTopoRPIMainWnd.btnUnlockThreeClick(Sender: TObject);
begin
  lbCarUnlock.Caption := lbCarUnlock.Caption + '3';
end;

procedure TGHTopoRPIMainWnd.btnUnlockTryClick(Sender: TObject);
begin
  if (trim(lbCarUnlock.Caption) = PASSWD_DEVERROUILLAGE_ECRAN) then pnlEcranDeverrouillage.Visible := false;
  lbCarUnlock.Caption := '';
end;

procedure TGHTopoRPIMainWnd.btnUnlockTwoClick(Sender: TObject);
begin
  lbCarUnlock.Caption := lbCarUnlock.Caption + '2';
end;


//----------------------------------

procedure TGHTopoRPIMainWnd.Button10Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 5;
end;

procedure TGHTopoRPIMainWnd.Button11Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 6;
end;



procedure TGHTopoRPIMainWnd.Button1Click(Sender: TObject);
var
  WU: TVue2DParams;
begin
  WU  := cdrParamsOngletVue2D1.GetValuesOnglet2D;
  GHTopoContext2DA1.SetModeRepresentation(WU.ongModeRepresentation);
  GHTopoContext2DA1.PutOngletByIndex(0, WU);
  GHTopoContext2DA1.RedessinEcran(True);
  pnlParamsVue.Visible := false;
end;

procedure TGHTopoRPIMainWnd.Button2Click(Sender: TObject);
begin
  DisplayClavierVirtuel(Edit1, mkbdvALPHA_MINUSCULES);
end;

procedure TGHTopoRPIMainWnd.Button4Click(Sender: TObject);
var
  EWE: double;
begin
  EWE := 3.1415926535;
  DisplayPaveNumVirtuel(EWE);
end;

procedure TGHTopoRPIMainWnd.Button5Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TGHTopoRPIMainWnd.Button6Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 1;
end;

procedure TGHTopoRPIMainWnd.Button7Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 2;
end;

procedure TGHTopoRPIMainWnd.Button8Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 3;
end;

procedure TGHTopoRPIMainWnd.Button9Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := INDEX_ONGLET_VUE_EN_PLAN;
end;

procedure TGHTopoRPIMainWnd.InitNewDocument();
var
  QFileName: String;
begin
  if (FDocTopoOpenedAndReady) then begin
    if (not GHTopoQuestionOuiNon('Un document est déjà ouvert - Continuer')) then Exit;
  end;
  CloseAllDocuments;
  // L'assistant provoque une erreur inexplicable (Invalid handle file) lors du chargement
  // --> contournement via un fichier ébauche
  QFileName   := GetGHTopoDirectory() + 'QCaveStub.xtb';
  if (not FileExistsUTF8(QFileName)) then RegenererEbaucheXTB(QFileName);
  ChargerLaTopoExt(QFilename, True);
  ActiverMenus(True);
end;

procedure TGHTopoRPIMainWnd.lbDistoReadyClick(Sender: TObject);
begin

end;

procedure TGHTopoRPIMainWnd.lsbErreursClick(Sender: TObject);
begin

end;

procedure TGHTopoRPIMainWnd.PageControl1Change(Sender: TObject);
begin

end;

procedure TGHTopoRPIMainWnd.Panel1Click(Sender: TObject);
begin

end;

procedure TGHTopoRPIMainWnd.Panel2Click(Sender: TObject);
begin

end;

procedure TGHTopoRPIMainWnd.pnlEcranDeverrouillageClick(Sender: TObject);
begin

end;

procedure TGHTopoRPIMainWnd.pnlParamsVueClick(Sender: TObject);
begin

end;

procedure TGHTopoRPIMainWnd.pnlToolBarsClick(Sender: TObject);
begin

end;

// fermer tous les documents
procedure TGHTopoRPIMainWnd.CloseAllDocuments;
begin
  if Not(FDocTopoOpenedAndReady) then Exit;
  AfficherMessage('CloseAllDocuments');
  FDocTopoOpenedAndReady := False;
  FDocumentToporobot.ClearListeSeries;          // vidage des séries
  FDocumentToporobot.ViderTablesSimples;        // vidage autres tables
  //CloseVues;
  ActiverMenus(False);
  self.Caption := MakeTitleMainWindowGHTopo('Untitled');
end;

procedure TGHTopoRPIMainWnd.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=False;
  if (MessageDlg(EnlevePerluete(GetResourceString(rsGHTOPO_QUIT)), mtConfirmation, [mbYes, mbNo],0)=mrYES) then
  begin
    try
      FDocumentToporobot.Finaliser;
      FTopoCompilee.Finaliser();
      //FMonMaillage.Finaliser();
      FConvertisseurCoordonnees.Finaliser();
    finally
      FDocumentToporobot.Free;
      FTopoCompilee.Free;
      //FMonMaillage.Free;
      FPalette256.Free;
      FConvertisseurCoordonnees.Free;
    end;
    AfficherMessage(GetResourceString(rsEND_OF_GHTOPO));
    CanClose:=True;
  end;
end;

//******************************************************************************
function TGHTopoRPIMainWnd.CalculerLeReseauExt: boolean;
var
  CodeCalcul: TCodeDeCalcul;
  EWE: String;
begin
  Result := False;
  Application.ProcessMessages;
  AfficherMessage('-- Calcul du reseau (nouvelle version)');
  CodeCalcul := TCodeDeCalcul.Create;
  try
    CodeCalcul.Initialiser(FDocumentToporobot, '');
    CodeCalcul.SetProcDisplayProgression(nil); //self.AfficherProgression);
    Result    := CodeCalcul.CalculComplet(FTopoCompilee, False);
    CodeCalcul.SetProcDisplayProgression(nil);
    CodeCalcul.Finaliser();

  finally
    CodeCalcul.Free;

  end;
end;



//******************************************************************************
// charger la topo
// Chargement de la topo
function TGHTopoRPIMainWnd.ChargerLaTopoExt(const FC: TStringDirectoryFileName; const DoCalc: boolean): boolean;
begin
  Result := false;
  AfficherMessage(Format('%s.ChargerLaTopoExt: %s', [ClassName, FNomFichierXTB]));

  // activer le menu de sauvegarde
  acSaveAs.Visible := True;
  FDocumentToporobot.SetDatabaseName(FNomFichierXTB);
  // vérifier si c'est un fichier Text
  if (Pos('.text', LowerCase(FC))>0) then
  begin
    if (FDocumentToporobot.LoadFichierText(FC) < 0) then
    begin
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
  else if (Pos('.txt', FC) > 0) then // fichier PocketTopo Txt?
  begin
    ShowMessage(FC);
    if (FDocumentToporobot.LoadFromPocketTopoTXT(FC) < 0) then
    begin
      AfficherMessageErreur('Le fichier comporte des erreurs - Voir les consoles');
      FDocumentToporobot.Finaliser;  // Echec = on détruit l'objet
      Exit;
    end;
  end
  else
  begin // sinon, c'est un fichier supposé Tab ou XTB
    if (FDocumentToporobot.LoadFichierTab(FC)<0) then
    begin
      AfficherMessageErreur('Le fichier comporte des erreurs - Voir le rapport');
      DisplayTextEditor(GetGHTopoDirectory() + ChangeFileExt(ExtractFileName(FDocumentToporobot.GetDatabaseName), '.err'), True);
      // Echec = on détruit l'objet
      FDocumentToporobot.Finaliser;
      Exit;
    end;
  end;
  //ShowMessage(DatabaseName);
  self.Caption := MakeTitleMainWindowGHTopo(ExtractFileName(FC));
  FDocTopoOpenedAndReady := CalculerLeReseauExt();
  // préparer le frontal
  PreparerFrontal();
  // préparer la vue 2D
  PreparerVue2D();
  //rendre visibles toutes les options des menus
  ActiverMenus(True);

  self.Top  := 0;
  self.Left := 0;
  // tout est OK ?
  FNomFichierXTB := FC;
  self.Caption := FC;
  Application.ProcessMessages;
end;
procedure TGHTopoRPIMainWnd.AttrapperViseeDistoX();
var
  n, NumSerieDistoX: Integer;
  EWE: TMesureViseeDistoX;
  WU: String;
begin

  n := CdrDistoX1.getNbMesuresDistoX();
  if (n = 0) then Exit;
  EWE := CdrDistoX1.GetViseefromDistoX(n-1);
  WU := format('%d - %f, %f, %f', [n, EWE.Longueur, EWE.Azimut, EWE.Pente]);
  memoToutesMesures.Lines.Add(WU);
  NumSerieDistoX := CdrDistoX1.GetNumeroSerieDistoX();

  //#DistoX;	Hexadata trame;	Horodate;	L	A	P

  WU := format(FORMAT_NB_INTEGER + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC +#9 + FORMAT_NB_REAL_3_DEC,
               [2395, // on n'a pas besoin du numéro de DistoX
                EWE.Longueur,
                EWE.Azimut,
                EWE.Pente
               ]);
  AfficherMessageErreur(WU);
  memoRafaleActuelle.Lines.Add(WU);


end;

// préparation du frontal de la BDD
procedure TGHTopoRPIMainWnd.PreparerFrontal();
begin
  Application.ProcessMessages;
  AfficherMessage('Préparation du frontal de la base de données');
  AfficherMessage(Format('%d entrees; %d reseaux; %d secteurs; %d codes; %d expes; %d series; %d antennes',
                         [FDocumentToporobot.GetNbEntrees,
                          FDocumentToporobot.GetNbReseaux,
                          FDocumentToporobot.GetNbSecteurs,
                          FDocumentToporobot.GetNbCodes,
                          FDocumentToporobot.GetNbExpes,
                          FDocumentToporobot.GetNbSeries,
                          FDocumentToporobot.GetNbAntennes]));
  // onglet General
  editNomEtude.Text         := _AnsiToLCLStr(FDocumentToporobot.GetNomEtude);
  editCommentaireEtude.Text := _AnsiToLCLStr(FDocumentToporobot.GetCommentairesEtude);
  // combo des systèmes de coordonnées
  RemplirCombosSystemesCoordonnees(FConvertisseurCoordonnees,
                                   cmbSystemesCoordonnees, True,
                                   FDocumentToporobot.GetCodeEPSGSystemeCoordonnees().CodeEPSG);
  // cadre des listes simples (codes, expés, ...);
  CdrListesSimples1.Initialiser(FDocumentToporobot, FPalette256, mbddENTRANCES);
  // préparation de l'onglet Séries
  //CdrListeSeries1.Initialise(FDocumentToporobot, AfficherUneSerie, ApplyModificationSerie, [tbsAPPLY, tbsADD, tbsSORT, tbsCSV, tbsHELP]);

  // table des antennes
  CdrAntennes1.Initialise(FDocumentToporobot);


  // liste des séries en mode flat
  CdrSeriesRPI1.Initialiser(FDocumentToporobot, 1);
  // DistoX


end;
procedure TGHTopoRPIMainWnd.PreparerVue2D();
var
  o: Integer;
  Ong: TVue2DParams;
begin
  pnlParamsVue.Visible := false;
  GHTopoContext2DA1.Initialiser(FDocumentToporobot, FTopoCompilee, nil, RecalculerEtActualiser);
  // préparation des OngletsVues
  FCurrParams2D := GHTopoContext2DA1.GetOngletByIndex(0);
  cdrParamsOngletVue2D1.SetValuesOnglet2D(FCurrParams2D);
  GHTopoContext2DA1.SetModeRepresentation(FCurrParams2D.ongModeRepresentation);
  // les captions
  //InitLesCaptions;
  // les panneaux
  //pnlListeSeries.Visible := false;
  //pnlDiagrammes.Visible := false;
end;

procedure TGHTopoRPIMainWnd.PreparerDistoX();
begin

end;

end.

