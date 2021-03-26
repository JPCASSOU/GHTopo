unit frmVuePlan2D;
// 04/03/2020: Point de contrôle temporel (contrôle de version)

{$INCLUDE CompilationParameters.inc}

{$IFDEF GHTOPO_SIMPLIFIE}
   {$ERROR Le mode GHTOPO_SIMPLIFIE est valable exclusivement pour le projet GHTopoRPI}
{$ENDIF}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue

  Classes, SysUtils,
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  UnitEntitesExtended,
  unitclassemaillage,
  unitCroquisTerrain,
  UnitGraphes1,
  CadreGHTopoContext2D,
  FastGEO,
  CallDialogsStdVersion,
  unitUtilsComposants,
  CadreListesPourVueGraphique,
  UnitObjetSerie,
  CadreDGCDrawingContext,
  FileUtil, curredit, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Buttons, ActnList, PairSplitter, StdCtrls, ExtDlgs;

type

  { TfrmVueEnPlan }

  TfrmVueEnPlan = class(TForm)
    acF2DZoomAll: TAction;
    acF2DZoomPlus: TAction;
    acF2DZoomMoins: TAction;
    acF2DPanVue: TAction;
    acF2DZoomFenetre: TAction;
    acF2DParamOnglet: TAction;
    acF2DRendu3D: TAction;
    acF2DStatistiques: TAction;
    acF2DDistance: TAction;
    acF2DVue3D: TAction;
    acF2DPrintPlan: TAction;
    acF2DLocaliserStation: TAction;
    acShowFenetreSeries: TAction;
    acShowFenetreListeSimples: TAction;
    acShowFenetreConsole: TAction;
    acCreerTimelapse: TAction;
    acSaveAs: TAction;
    acFermerUnCheminement: TAction;
    acLoadMaillage: TAction;
    acF2DExportDXF: TAction;
    acF2DExportSVG: TAction;
    acF2DCalculerLeReseau: TAction;
    acF2DExtractProfilTerrain: TAction;
    acNewPolyligne: TAction;
    acNewAnnotation: TAction;
    acDeletePolyline: TAction;
    acDeleteAnnotation: TAction;
    acSaveCroquis: TAction;
    acOpenCroquis: TAction;
    acExportPlanEnPNG: TAction;
    ActionList1: TActionList;
    btnStyleObjet1: TStaticText;
    btnStyleObjet10: TStaticText;
    btnStyleObjet2: TStaticText;
    btnStyleObjet3: TStaticText;
    btnStyleObjet4: TStaticText;
    btnStyleObjet5: TStaticText;
    btnStyleObjet6: TStaticText;
    btnStyleObjet7: TStaticText;
    btnStyleObjet9: TStaticText;
    CdrListesPourVisualisateurs1: TCdrListesPourVisualisateurs;
    chkPOI: TCheckBox;
    chkAltitudes: TCheckBox;
    chkAntennes: TCheckBox;
    chkCotes: TCheckBox;
    chkEntranceNames: TCheckBox;
    chkEntrances: TCheckBox;
    chkIDStations: TCheckBox;
    chkNoeuds: TCheckBox;
    chkParois: TCheckBox;
    chkPolygonales: TCheckBox;
    chkRemplissage: TCheckBox;
    chkSections: TCheckBox;
    chkStations: TCheckBox;
    GHTopoContext2DA1: TGHTopoContext2DA;
    ImageList1: TImageList;
    lbEtapeProgression: TLabel;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    OngletsVues: TTabControl;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pnlProgression: TPanel;
    pnlStylesPolylines: TPanel;
    pnlOutilsDessin: TPanel;
    ProgressBar1: TProgressBar;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    SpeedButton27: TSpeedButton;
    SpeedButton28: TSpeedButton;
    SpeedButton29: TSpeedButton;
    SpeedButton31: TSpeedButton;
    SpeedButton32: TSpeedButton;
    SpeedButton33: TSpeedButton;
    SpeedButton34: TSpeedButton;
    SpeedButton35: TSpeedButton;
    SpeedButton36: TSpeedButton;
    SpeedButton38: TSpeedButton;
    SpeedButton39: TSpeedButton;
    SpeedButton4: TSpeedButton;
    lbStatutCroquisTerrain: TStaticText;
    btnStyleObjet0: TStaticText;
    SpeedButton40: TSpeedButton;
    SpeedButton41: TSpeedButton;
    btnStyleObjet8: TStaticText;
    SpeedButton42: TSpeedButton;
    SpeedButton43: TSpeedButton;
    procedure acCreerTimelapseExecute(Sender: TObject);
    procedure acDeleteAnnotationExecute(Sender: TObject);
    procedure acDeletePolygonExecute(Sender: TObject);
    procedure acDeletePolylineExecute(Sender: TObject);

    procedure acExportPlanEnPNGExecute(Sender: TObject);
    procedure acF2DCalculerLeReseauExecute(Sender: TObject);
    procedure acF2DDistanceExecute(Sender: TObject);
    procedure acF2DExportSVGExecute(Sender: TObject);
    procedure acF2DExtractProfilTerrainExecute(Sender: TObject);
    procedure acF2DLocaliserStationExecute(Sender: TObject);
    procedure acF2DPanVueExecute(Sender: TObject);
    procedure acF2DParamOngletExecute(Sender: TObject);
    procedure acF2DPrintPlanExecute(Sender: TObject);
    procedure acF2DZoomAllExecute(Sender: TObject);
    procedure acF2DZoomFenetreExecute(Sender: TObject);
    procedure acF2DZoomMoinsExecute(Sender: TObject);
    procedure acF2DZoomPlusExecute(Sender: TObject);
    procedure acF2DRendu3DExecute(Sender: TObject);
    procedure acF2DStatistiquesExecute(Sender: TObject);
    procedure acF2DVue3DExecute(Sender: TObject);
    procedure acFermerUnCheminementExecute(Sender: TObject);
    procedure acLoadMaillageExecute(Sender: TObject);
    procedure acNewAnnotationExecute(Sender: TObject);
    procedure acNewPolyligneExecute(Sender: TObject);
    procedure acOpenCroquisExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acSaveCroquisExecute(Sender: TObject);
    procedure acShowFenetreConsoleExecute(Sender: TObject);
    procedure acShowFenetreListeSimplesExecute(Sender: TObject);
    procedure acShowFenetreSeriesExecute(Sender: TObject);
    procedure btnStyleObjet0Click(Sender: TObject);
    procedure btnStyleObjet10Click(Sender: TObject);
    procedure btnStyleObjet1Click(Sender: TObject);
    procedure btnStyleObjet2Click(Sender: TObject);
    procedure btnStyleObjet3Click(Sender: TObject);
    procedure btnStyleObjet4Click(Sender: TObject);
    procedure btnStyleObjet5Click(Sender: TObject);
    procedure btnStyleObjet6Click(Sender: TObject);
    procedure btnStyleObjet7Click(Sender: TObject);
    procedure btnStyleObjet8Click(Sender: TObject);
    procedure btnStyleObjet9Click(Sender: TObject);

    procedure chkAltitudesChange(Sender: TObject);
    procedure chkAntennesChange(Sender: TObject);
    procedure chkCotesChange(Sender: TObject);
    procedure chkEntranceNamesChange(Sender: TObject);
    procedure chkEntrancesChange(Sender: TObject);
    procedure chkIDStationsChange(Sender: TObject);
    procedure chkNoeudsChange(Sender: TObject);
    procedure chkParoisChange(Sender: TObject);
    procedure chkPOIChange(Sender: TObject);
    procedure chkPolygonalesChange(Sender: TObject);
    procedure chkRemplissageChange(Sender: TObject);
    procedure chkSectionsChange(Sender: TObject);
    procedure chkStationsChange(Sender: TObject);

    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure OngletsVuesChange(Sender: TObject);
    procedure VueClick(Sender: TObject);
  private
    { private declarations }
    FBDDEntites    : TBDDEntites;
    FDocTopo       : TToporobotStructure2012;
    FMaillageMNT   : TMaillage;
    FCurrentNomFichierXTB: TStringDirectoryFilename;
    FProcRecalculerReseau: TProcOfObjectWithOneBoolParameter;
    procedure ChangeAttrElementsAffiches(const QIdx: TElementDrawn; const QChecked: boolean);
    procedure DispAttrElementsAffiches(const EA: TSetElementsDrawn);
    procedure PerformActionWithBaseStation(const BP: TBaseStation; const TodoAction: TTodoAction);
    procedure InitCaptions();
    procedure PreparerVues();
    procedure ProcDisplayProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
    procedure SetPanelStyles(const QCurrStyleIdx: integer);

  public
    { public declarations }
    function InitialiserVue2D(const D: TToporobotStructure2012;
                              const C: TCroquisTerrain;
                              const P: TBDDEntites;
                              //const G: TPathFindingGraphe;
                              //const CheminMinimal: TPathBetweenNodes;
                              const M: TMaillage;
                              const QFileName: TStringDirectoryFilename;
                              const QProcRecalcul: TProcOfObjectWithOneBoolParameter): boolean;
    procedure Finaliser();

    function  GetParamsOngletCourant(): TVue2DParams;
    procedure PutParamsOngletCourant(const O: TVue2DParams);
    function  GetMaillage(): TMaillage;
    function  GetLassoDeSelection(): TGeoPolygon2D;
    procedure CentrerBasepointSurPlan(const BP: TToporobotIDStation);
    procedure CentrerXYSurPlan(const QX, QY: double; const TagString: string);
    procedure DemanderChargementDeMNT();
    procedure DemanderExportPlanToSVG();
  end;

var
  frmVueEnPlan: TfrmVueEnPlan;

implementation
{$R *.lfm}

{ TfrmVueEnPlan }
uses
  frmFrontalSeries
  , frmLesListesSimples
  , frmJournal
  ;

procedure TfrmVueEnPlan.ProcDisplayProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
begin
  if (0 = Done MOD Step) then
  begin
    lbEtapeProgression.Caption := Etape;
    ProgressBar1.Min := Starting;
    ProgressBar1.Max := Ending;
    ProgressBar1.Position := Done;
    Application.ProcessMessages;
  end;
end;

procedure TfrmVueEnPlan.acF2DDistanceExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.SetModeTravail(mtDISTANCE_PREMIER_POINT);
end;



procedure TfrmVueEnPlan.acF2DExportSVGExecute(Sender: TObject);
begin
  DemanderExportPlanToSVG();
end;

procedure TfrmVueEnPlan.acF2DExtractProfilTerrainExecute(Sender: TObject);
begin

  if (GHTopoContext2DA1.CanDraw and GHTopoContext2DA1.IsMaillageDisplayed()) then
  begin
    GHTopoContext2DA1.SetModeTravail(mtPROFIL_MNT_PREMIER_POINT);
    GHTopoContext2DA1.RefreshDessin();
  end
  else
    ShowMessage('Maillage MNT invalide, non chargé ou non tracé');
end;

procedure TfrmVueEnPlan.acCreerTimelapseExecute(Sender: TObject);
var
  QDirectory: string;
begin
  if (SelectDirectory(GetResourceString(rsCDR_VUE2D_CREATE_TIMELAPSE),
                      GetGHTopoDirectory(),
                      QDirectory)) then
  begin
    GHTopoContext2DA1.CreerTimeLapse(QDirectory);
  end;
end;

procedure TfrmVueEnPlan.acDeleteAnnotationExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtDELETE_ANNOTATION);
end;

procedure TfrmVueEnPlan.acDeletePolygonExecute(Sender: TObject);
begin

end;

procedure TfrmVueEnPlan.acDeletePolylineExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtDELETE_POLYLINE);
end;



procedure TfrmVueEnPlan.acExportPlanEnPNGExecute(Sender: TObject);
const
  IMG_WIDTH_MIN = 1600;
  IMG_WIDTH_MAX = 32000;
var
  TD: TSavePictureDialog;
  C1, C2: TPoint3Df;
  dx, dy, r: Double;
  IW: Integer;
  IH: Int64;
  EWE: string;
begin
  EWE := '12000';
  if (InputQuery('Export du plan en image', 'Largeur de l''image', EWE)) then
  begin
    IW := StrToIntDef(Trim(EWE), IMG_WIDTH_MIN);
    if (IW < IMG_WIDTH_MIN) then IW := IMG_WIDTH_MIN;
    if (IW > IMG_WIDTH_MAX) then IW := IMG_WIDTH_MAX;
    TD := TSavePictureDialog.Create(self);
    try
      TD.InitialDir := GetGHTopoDirectory();
      TD.FileName   := 'ImgPlan001.png';
      TD.DefaultExt := '.png';
      TD.Options    := TD.Options + [ofOverwritePrompt];
      if (TD.Execute) then
      begin
        C1 := FBDDEntites.GetCoinBasGauche();
        C2 := FBDDEntites.GetCoinHautDroit();
        dx := C2.X - C1.X;
        dy := C2.Y - C1.Y;
        r := dy / dx;
        IH := trunc(IW * r);
        GHTopoContext2DA1.ExporterPlanEnImage(IW, IH, true, TD.FileName);
        ShowMessage(GetResourceString(rsDONE_ANY_PROCESS));
      end;
    finally
      FreeAndNil(TD);
    end;
  end;
end;

procedure TfrmVueEnPlan.acF2DCalculerLeReseauExecute(Sender: TObject);
begin
  if (Assigned(FProcRecalculerReseau)) then FProcRecalculerReseau(false);
end;
procedure TfrmVueEnPlan.acF2DLocaliserStationExecute(Sender: TObject);
begin
  GHTopoContext2DA1.LocaliserUneStation();
end;

procedure TfrmVueEnPlan.acF2DPanVueExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.SetModeTravail(mtPAN_PREMIER_POINT);
end;

procedure TfrmVueEnPlan.acF2DParamOngletExecute(Sender: TObject);
var
  n: Integer;
  MyOnglet: TVue2DParams;
begin
  if (not GHTopoContext2DA1.CanDraw) then exit;
  n := GHTopoContext2DA1.GetCurrentOngletIdx();
  MyOnglet := GHTopoContext2DA1.GetOngletByIndex(n);
  if (ParametrerOngletVue2D(MyOnglet)) then GHTopoContext2DA1.PutOngletByIndex(n, MyOnglet, True);
end;

procedure TfrmVueEnPlan.acF2DPrintPlanExecute(Sender: TObject);
begin
  DisplayCentreImpressionExt(FDocTopo,
                             FBDDEntites,
                             GHTopoContext2DA1.GetPtrCroquisTerrain(),
                             {$IFNDEF GHTOPO_SIMPLIFIE}
                             FMaillageMNT,
                             {$ENDIF GHTOPO_SIMPLIFIE}
                             GetParamsOngletCourant());
end;

procedure TfrmVueEnPlan.acF2DZoomAllExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.ResetVue();
end;

procedure TfrmVueEnPlan.acF2DZoomFenetreExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.SetModeTravail(mtZOOM_PREMIER_COIN);
end;

procedure TfrmVueEnPlan.acF2DZoomMoinsExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.ZoomFact(1 - 0.05);
end;

procedure TfrmVueEnPlan.acF2DZoomPlusExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.ZoomFact(1 + 0.05);
end;

procedure TfrmVueEnPlan.acShowFenetreConsoleExecute(Sender: TObject);
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

procedure TfrmVueEnPlan.acShowFenetreListeSimplesExecute(Sender: TObject);
begin
  PositionnerFenetre(frmListesSimples, 10, 140, Screen.Width - 500, Screen.Height - 300);
  frmListesSimples.SetFocus;
end;

procedure TfrmVueEnPlan.acShowFenetreSeriesExecute(Sender: TObject);
begin
  PositionnerFenetre(frmGestionSeries, 20, 120, Screen.Width - 300, Screen.Height - 260);
  frmGestionSeries.SetFocus;
end;




procedure TfrmVueEnPlan.btnStyleObjet0Click(Sender: TObject);
begin
  SetPanelStyles(0);
end;

procedure TfrmVueEnPlan.btnStyleObjet10Click(Sender: TObject);
begin
  SetPanelStyles(10);
end;

procedure TfrmVueEnPlan.btnStyleObjet1Click(Sender: TObject);
begin
  SetPanelStyles(1);
end;

procedure TfrmVueEnPlan.btnStyleObjet2Click(Sender: TObject);
begin
  SetPanelStyles(2);
end;

procedure TfrmVueEnPlan.btnStyleObjet3Click(Sender: TObject);
begin
  SetPanelStyles(3);
end;

procedure TfrmVueEnPlan.btnStyleObjet4Click(Sender: TObject);
begin
  SetPanelStyles(4);
end;

procedure TfrmVueEnPlan.btnStyleObjet5Click(Sender: TObject);
begin
  SetPanelStyles(5);
end;

procedure TfrmVueEnPlan.btnStyleObjet6Click(Sender: TObject);
begin
  SetPanelStyles(6);
end;

procedure TfrmVueEnPlan.btnStyleObjet7Click(Sender: TObject);
begin
  SetPanelStyles(7);
end;

procedure TfrmVueEnPlan.btnStyleObjet8Click(Sender: TObject);
begin
  SetPanelStyles(8);
end;

procedure TfrmVueEnPlan.btnStyleObjet9Click(Sender: TObject);
begin
  SetPanelStyles(9);
end;



procedure TfrmVueEnPlan.ChangeAttrElementsAffiches(const QIdx: TElementDrawn; const QChecked: boolean);
var
  n: Integer;
  MyOnglet: TVue2DParams;
  procedure MiouMiou(const Miou: TElementDrawn);
  begin
    if (QChecked) then Include(MyOnglet.ongElementsDrawn, Miou) else Exclude(MyOnglet.ongElementsDrawn, Miou);
  end;
begin
  if (not GHTopoContext2DA1.CanDraw) then exit;
  n := GHTopoContext2DA1.GetCurrentOngletIdx();
  MyOnglet := GHTopoContext2DA1.GetOngletByIndex(n);
  MiouMiou(QIdx);
  GHTopoContext2DA1.PutOngletByIndex(n, MyOnglet, True);
end;

procedure TfrmVueEnPlan.DispAttrElementsAffiches(const EA: TSetElementsDrawn);
begin
  chkPolygonales.Checked      := edPolygonals      in EA;
  chkSections.Checked         := edCrossSections   in EA;
  chkRemplissage.Checked      := edFillGalerie     in EA;
  chkAltitudes.Checked        := edAltitudes       in EA;
  chkAntennes.Checked         := edANTENNES        in EA;
  chkAltitudes.Checked        := edAltitudes       in EA;
  chkCotes.Checked            := edCotes           in EA;
  chkEntrances.Checked        := edENTRANCE_MKS    in EA;
  chkEntranceNames.Checked    := edENTRANCE_NAMES  in EA;
  chkStations.Checked         := edStations        in EA;
  chkIDStations.Checked       := edIDStations      in EA;
  chkNoeuds.Checked           := edJONCTIONS       in EA;
  chkParois.Checked           := edWalls           in EA;
end;

procedure TfrmVueEnPlan.chkAltitudesChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edAltitudes, chkAltitudes.Checked);
end;
procedure TfrmVueEnPlan.chkAntennesChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edANTENNES, chkAntennes.Checked);
end;

procedure TfrmVueEnPlan.chkCotesChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edCotes, chkCotes.Checked);
end;

procedure TfrmVueEnPlan.chkEntranceNamesChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edENTRANCE_NAMES, chkEntranceNames.Checked);
end;

procedure TfrmVueEnPlan.chkEntrancesChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edENTRANCE_MKS, chkEntrances.Checked);
end;

procedure TfrmVueEnPlan.chkIDStationsChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edIDStations, chkIDStations.Checked);
end;

procedure TfrmVueEnPlan.chkNoeudsChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edJONCTIONS, chkNoeuds.Checked);
end;

procedure TfrmVueEnPlan.chkParoisChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edWalls, chkParois.Checked);
end;

procedure TfrmVueEnPlan.chkPOIChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edPOI, chkPOI.Checked);
end;

procedure TfrmVueEnPlan.chkPolygonalesChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edPolygonals, chkPolygonales.Checked);
end;

procedure TfrmVueEnPlan.chkRemplissageChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edFillGalerie, chkRemplissage.Checked);
end;

procedure TfrmVueEnPlan.chkSectionsChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edCrossSections, chkSections.Checked);
end;

procedure TfrmVueEnPlan.chkStationsChange(Sender: TObject);
begin
  ChangeAttrElementsAffiches(edStations, chkStations.Checked);
end;



procedure TfrmVueEnPlan.acF2DRendu3DExecute(Sender: TObject);
var
  O: TVue2DParams;
begin
  {$IFDEF USE_VIEWER_OPENGL}
  if (GHTopoContext2DA1.CanDraw) then
  begin
    O := GHTopoContext2DA1.GetCurrentOnglet();
    DisplayVue3DOpenGLExt(FBDDEntites, FMaillageMNT, O.ongVueFiltres);
    GHTopoContext2DA1.RefreshDessin();
  end;
  {$ELSE}
    ShowMessage('Viewer OpenGL disabled');
  {$ENDIF}
end;

procedure TfrmVueEnPlan.acF2DStatistiquesExecute(Sender: TObject);
begin
  DisplayStatistiquesExt(FDocTopo, FBDDEntites);
end;

procedure TfrmVueEnPlan.acF2DVue3DExecute(Sender: TObject);
var
  O: TVue2DParams;
  M: TMaillage;
  G: TToporobotStructure2012;
begin
  O := GHTopoContext2DA1.GetCurrentOnglet();
  M := GHTopoContext2DA1.GetPtrMaillage();
  G := GHTopoContext2DA1.GetPtrDocuTopo();
  DisplayVue3DGDIExt(FBDDEntites, G, M, O);
  GHTopoContext2DA1.RefreshDessin();
end;

procedure TfrmVueEnPlan.acFermerUnCheminementExecute(Sender: TObject);
begin
  if (GHTopoContext2DA1.CanDraw) then GHTopoContext2DA1.SetModeTravail(mtBOUCLER_PREMIER_POINT);
end;

procedure TfrmVueEnPlan.acLoadMaillageExecute(Sender: TObject);
begin
  DemanderChargementDeMNT();
end;


procedure TfrmVueEnPlan.acNewAnnotationExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtNEW_ANNOTATION);
end;


procedure TfrmVueEnPlan.acNewPolyligneExecute(Sender: TObject);
begin
  GHTopoContext2DA1.SetModeTravail(mtNEW_POLYLINE);
end;

procedure TfrmVueEnPlan.acOpenCroquisExecute(Sender: TObject);
var
  FC: TCroquisTerrain;
  QFilename: TStringDirectoryFilename;
begin
  QFilename := '';
  FC := GHTopoContext2DA1.GetPtrCroquisTerrain();

  if (DoDialogOpenFile(GetGHTopoDirectory(), 'Croquis (*.xml)|*.xml', '.xml', QFilename)) then
  begin
    FC.LoadFromXML(QFilename);
    GHTopoContext2DA1.SetModeTravail(mtREADY);
  end;
end;

procedure TfrmVueEnPlan.acSaveAsExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := FCurrentNomFichierXTB;
  if (DoDialogSaveFile(GetResourceString(rsGHTOPO_FILE_FILTER_WO_TEXT), '.xtb', QFileName, QFilterIndex)) then
  begin
    case QFilterIndex of
      1: FDocTopo.SaveToXTB(QFileName,mtabEXTENDEDTAB, tfWINDOWS);
      2: FDocTopo.SaveToXML(QFileName); // futur format standard de GHTopo
      3: FDocTopo.SaveToXTB(QFileName,mtabTOPOROBOT, tfMAC);
    end; // case FilterIndex
    // actualisation du nouveau nom de fichier
    FCurrentNomFichierXTB := QFileName;
    self.Caption := MakeTitleMainWindowGHTopo(FCurrentNomFichierXTB);
  end;
end;

procedure TfrmVueEnPlan.acSaveCroquisExecute(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
  QIdx: integer;
  FC: TCroquisTerrain;
begin
  QFilename := 'croquis001.xml';
  if (DoDialogSaveFile('Fichier croquis (*.xml)|*.xml', '.xml', QFilename, QIdx)) then
  begin
    FC := GHTopoContext2DA1.GetPtrCroquisTerrain();
    FC.SaveToXML(QFilename);
  end;
end;



procedure TfrmVueEnPlan.Finaliser();
begin
  pass;
  //ShowMessageFmt('%s.Finaliser()', [classname]);
end;

function TfrmVueEnPlan.GetParamsOngletCourant(): TVue2DParams;
var
  n: Integer;
begin
  n := GHTopoContext2DA1.GetCurrentOngletIdx();
  Result := GHTopoContext2DA1.GetOngletByIndex(n);
end;

procedure TfrmVueEnPlan.PutParamsOngletCourant(const O: TVue2DParams);
var
  n: Integer;
begin
  n := GHTopoContext2DA1.GetCurrentOngletIdx();
  GHTopoContext2DA1.PutOngletByIndex(n, O, True);
end;

function TfrmVueEnPlan.GetMaillage(): TMaillage;
begin
  Result := FMaillageMNT;
end;

function TfrmVueEnPlan.GetLassoDeSelection(): TGeoPolygon2D;
begin
  Result := GHTopoContext2DA1.GetLassoDeSelection();
end;

procedure TfrmVueEnPlan.CentrerBasepointSurPlan(const BP: TToporobotIDStation);
begin
  GHTopoContext2DA1.LocaliserUneStationByCle(Format(FMTSERST, [BP.aSerie, BP.aStation]));
end;

procedure TfrmVueEnPlan.CentrerXYSurPlan(const QX, QY: double; const TagString: string);
begin
  GHTopoContext2DA1.CentrerVueSurPointXY(QX, QY, True, TagString);
end;

procedure TfrmVueEnPlan.DemanderChargementDeMNT();
var
  QFilename: TStringDirectoryFilename;
begin
  QFilename:= '';
  if (GHTopoContext2DA1.IsMaillageValide()) then
  begin
    GHTopoContext2DA1.DisplayPnlMaillage(True);
    if (Not GHTopoQuestionOuiNon('Un maillage valide existe. Le remplacer')) then exit;
  end;

  if (DoDialogOpenFile('Fichiers maillage (*.mai)|*.mai', '.mai', QFilename)) then
  begin
    GHTopoContext2DA1.ChargerMaillage(QFilename);
  end;
end;


procedure TfrmVueEnPlan.DemanderExportPlanToSVG();
var
  QFilename: TStringDirectoryFilename;
  O: TVue2DParams;
  QFilterIndex: integer;
begin
  QFilename := 'Plan2D_SVG.svg';
  if (DoDialogSaveFile('Scalable Vectors Graphics (SVG)|*.svg', '.svg', QFilename, QFilterIndex)) then
  begin
    pnlProgression.Visible := True;
    O := GHTopoContext2DA1.GetCurrentOnglet();
    FBDDEntites.ExportVue2DSVG(QFilename, false, O, ProcDisplayProgression);
    pnlProgression.Visible := False;
  end;
end;

procedure TfrmVueEnPlan.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
end;

procedure TfrmVueEnPlan.FormCreate(Sender: TObject);
begin
  pass;
end;

procedure TfrmVueEnPlan.FormResize(Sender: TObject);
begin
  PairSplitterSide1.Width := self.Width - 400;
end;

procedure TfrmVueEnPlan.FormWindowStateChange(Sender: TObject);
const
  GHTOPO_MAIN_BAR_HEIGHT = 90;
  DESKTOP_TASKBAR_HEIGHT = 80;
begin
  if (self.WindowState = wsMaximized) then
  begin
    self.Left   := 0;
    self.Top    := GHTOPO_MAIN_BAR_HEIGHT; // 1 + Hauteur de la fenêtre principale
    self.Width  := Screen.Width;
    self.Height := Screen.Height - self.Top - DESKTOP_TASKBAR_HEIGHT - 4;
  end;
end;

procedure TfrmVueEnPlan.InitCaptions();
  procedure SetAcHint(const ACDC: TAction; const QCaption: string);
  var
    WU: String;
  begin
    WU := EnlevePerluete(GetResourceString(QCaption));
    ACDC.Caption := WU;
    ACDC.Hint    := WU;
  end;
begin
  SetAcHint(acSaveAs                     , rsSAVEAS);
  SetAcHint(acF2DPrintPlan               , rsVUE2D_PRINTING);
  SetAcHint(acF2DRendu3D                 , rsVUE2D_VUE3D_OPENGL);
  SetAcHint(acF2DVue3D                   , rsVUE2D_VUE3D_GDI);

  SetAcHint(acF2DZoomAll                 , rsCDR_VUE2D_AC_ZOOM_ALL);
  SetAcHint(acF2DZoomFenetre             , rsCDR_VUE2D_AC_ZOOM_WINDOW);
  SetAcHint(acF2DPanVue                  , rsCDR_VUE2D_AC_PAN_VUE);
  SetAcHint(acF2DZoomPlus                , 'Zoom +');
  SetAcHint(acF2DZoomMoins               , 'Zoom -');


  SetAcHint(acF2DParamOnglet             , rsCDR_VUE2D_AC_PARAM_ONGLET);
  //SetAcHint(acAddSerieHere             , rsCDR_VUE2D_ADD_SERIE_HERE);
  SetAcHint(acF2DDistance                , rsCDR_VUE2D_AC_DISTANCE_BT_STATIONS);
  SetAcHint(acF2DLocaliserStation        , rsCDR_VUE2D_LOCALISER_STATION);

  SetAcHint(acF2DStatistiques            , rsSTATISTIQUES_TITRE);

  SetAcHint(acShowFenetreSeries          , rsWND_DATABASE);
  SetAcHint(acShowFenetreListeSimples    , rsWND_LISTES_SIMPLES);
  SetAcHint(acShowFenetreConsole         , rsWND_CONSOLE);
  SetAcHint(acCreerTimelapse             , rsCDR_VUE2D_CREATE_TIMELAPSE);


  SetAcHint(acFermerUnCheminement        , rsCDR_VUE2D_CLOSE_BOUCLE);
  SetAcHint(acF2DExportSVG               , rsEXPORT_SVG);
  SetAcHint(acF2DCalculerLeReseau        , rsCOMPILE);

  SetAcHint(acLoadMaillage               , rsCDR_VUE2D_AC_LOAD_MNT);
  SetAcHint(acF2DExtractProfilTerrain    , rsCDR_VUE2D_EXTRACT_PROFIL_FROM_MNT);

  SetAcHint(acOpenCroquis                , rsCDR_VUE2D_AC_CROQUIS_OPEN);
  SetAcHint(acSaveCroquis                , rsCDR_VUE2D_AC_CROQUIS_SAVE);

  SetAcHint(acNewAnnotation              , rsCDR_VUE2D_AC_CROQUIS_NEW_ANNOTATION);
  SetAcHint(acNewPolyligne               , rsCDR_VUE2D_AC_CROQUIS_NEW_POLYLIGNE);
  SetAcHint(acDeleteAnnotation           , rsCDR_VUE2D_AC_CROQUIS_DELETE_ANNOTATION);
  SetAcHint(acDeletePolyline             , rsCDR_VUE2D_AC_CROQUIS_DELETE_POLYLIGNE);

end;

procedure TfrmVueEnPlan.PerformActionWithBaseStation(const BP: TBaseStation; const TodoAction: TTodoAction);
var
  MySerie: TObjSerie;
  QIdx: integer;
begin
  case TodoAction of
    tdoAcnBASESTATION_DISPLAY:
    begin
      ShowMessage(Format('En attente: Traitement de %d.%d', [BP.Entite_Serie, BP.Entite_Station]));
      //FBDDEntites.HighlightSeries();
    end;
    tdoAcnBASESTATION_EDITSERIE:
    begin
      if (FDocTopo.GetSerieByNumeroSerie(BP.Entite_Serie, MySerie, QIdx)) then
      begin
        if (DisplayUneSerie(FDocTopo, FBDDEntites, QIdx, MySerie)) then
        begin
          pass; //FDocumentToporobot.CheckerLesDonneesTopo();
        end;
      end;
    end;
  end;
end;

function TfrmVueEnPlan.InitialiserVue2D(const D: TToporobotStructure2012;
                                        const C: TCroquisTerrain;
                                        const P: TBDDEntites;
                                        //const G: TPathFindingGraphe;
                                        //const CheminMinimal: TPathBetweenNodes;
                                        const M: TMaillage;
                                        const QFileName: TStringDirectoryFilename;
                                        const QProcRecalcul: TProcOfObjectWithOneBoolParameter): boolean;

begin
  result := false;
  FBDDEntites := P;
  FDocTopo    := D;
  FMaillageMNT:= M;
  FCurrentNomFichierXTB := QFileName;
  pnlProgression.Visible:= false;
  {$ifdef GROS_MINET}
    pnlOutilsDessin.Visible := false;
  {$else}
    pnlOutilsDessin.Visible := (C.IsReady);
  {$endif}
  // init cadre vue 2D
  InitCaptions();
  GHTopoContext2DA1.Initialiser(mfgcSURVEYING,
                                FDocTopo,               // const QDT: TToporobotStructure2012;
                                FBDDEntites,            // const QBDD: TBDDEntites;
                                C,                      //  const QCT: TCroquisTerrain;
                                //G, CheminMinimal,
                                FMaillageMNT,           // const QMaillage: TMaillage;
                                nil,                    // const QProcUseMetaFiltre: TProcedureOfObject;
                                nil,                    // const QProcRefresh: TProcOfObjectWithOneBoolParameter;
                                QProcRecalcul,
                                PerformActionWithBaseStation);         // const QProcRecalculerReseau: TProcOfObjectWithOneBoolParameter): boolea

  PreparerVues();
  CdrListesPourVisualisateurs1.InitialiseListeSelection(FDocTopo,
                                                        FBDDEntites,
                                                        GHTopoContext2DA1,
                                                        mslSERIE,
                                                        1,
                                                        false, true);
  FProcRecalculerReseau := QProcRecalcul;
  result := true;
end;







procedure TfrmVueEnPlan.OngletsVuesChange(Sender: TObject);
var
  MyOnglet: TVue2DParams;
begin
  GHTopoContext2DA1.SetCurrentIdxOnglet(OngletsVues.TabIndex);
  MyOnglet := GHTopoContext2DA1.GetCurrentOnglet();
  DispAttrElementsAffiches(MyOnglet.ongElementsDrawn);
end;

procedure TfrmVueEnPlan.VueClick(Sender: TObject);
begin

end;


procedure TfrmVueEnPlan.PreparerVues();
var
  o: Integer;
  Ong: TVue2DParams;
  FC: TCroquisTerrain;
begin
  FC := GHTopoContext2DA1.GetPtrCroquisTerrain();
  OngletsVues.Tabs.Clear;       // préparation des OngletsVues
  for o := 0 to GHTopoContext2DA1.GetNbOngletsVues() - 1 do
  begin
    Ong := GHTopoContext2DA1.GetOngletByIndex(o);
    OngletsVues.Tabs.Add(Ong.ongName);
  end;
  OngletsVues.TabIndex := 0;
  // indicateur croquis de terrain
  lbStatutCroquisTerrain.Color := IIF(GHTopoContext2DA1.CroquisDeTerrainIsReady(), clGreen, clRed);
  // préparer le panneau de styles de polylignes/annotations du croquis
  SetPanelStyles(FC.CurrentIdxStylePolyligne);
  // et renseigner les checkbox éléments dessinés
  Ong := GHTopoContext2DA1.GetOngletByIndex(0);
  DispAttrElementsAffiches(Ong.ongElementsDrawn);

end;
// panneau des styles de polylignes/textes
procedure TfrmVueEnPlan.SetPanelStyles(const QCurrStyleIdx: integer);
var
  FC: TCroquisTerrain;
  i: Integer;
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
  for i := 0 to 9 do MiouMiou(TStaticText(self.FindComponent(Format('btnStyleObjet%d', [i]))), FC.GetStylePolyligne(i), (FC.CurrentIdxStylePolyligne = i));
  (*
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
  //*)
end;

end.

