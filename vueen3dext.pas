unit VueEn3DExt;

// Date: 07/11/2013
// Statut: Opérationnel
// Appel par passage de pointeur sur BDD entités: OK
// Appel par chargement de fichier: OK
// 27/04/2016: Nombreuses corrections et mises à jour

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees, Common,
  UnitEntitesExtended,
  UnitClasseMaillage,
  ToporobotClasses2012,
  CallDialogsStdVersion,
  unitUtilsComposants,
  CadreVue3DExt,
  Classes, SysUtils, FileUtil, curredit, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ActnList, Buttons, ComCtrls, LCLType;

type TModeEditValues = (medNONE, medORIENTATION, medELEVATION, medZOOM, medMAGNIFICATION);
type

  { TfrmVue3DExt }

  TfrmVue3DExt = class(TForm)
    acProjXY: TAction;
    acProjYZ: TAction;
    acProjXZ: TAction;
    acProjIso: TAction;
    acParamVue3D: TAction;
    acExporterSVG: TAction;
    acRegenerer: TAction;
    acRedessiner: TAction;
    acExportNuageDePoints: TAction;
    ActionList2: TActionList;
    BitBtn1: TBitBtn;
    btnDetourerMNT: TButton;
    btnExportGCP: TButton;
    btnSetMagnZ: TButton;
    btnSetPhi: TButton;
    btnSetTheta: TButton;
    btnSetZoom: TButton;
    Button4: TButton;
    Button5: TButton;
    CdrVue3DExt1: TCdrVue3DExt;
    chkFiltrer: TCheckBox;
    editMagnificationZ: TCurrencyEdit;
    editPhi: TCurrencyEdit;
    editTheta: TCurrencyEdit;
    editValue: TCurrencyEdit;
    editFiltres3D: TEdit;
    editZoom: TCurrencyEdit;
    grbxParamsMaillage: TGroupBox;
    ImageList2: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbNomValeur: TLabel;
    lbStatusMaillage: TStaticText;
    lbValueMin: TStaticText;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlSetValue: TPanel;
    sclValeur: TScrollBar;
    SpeedButton1: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    lbValueMax: TStaticText;
    procedure acExporterSVGExecute(Sender: TObject);
    procedure acExportNuageDePointsExecute(Sender: TObject);
    procedure acParamVue3DExecute(Sender: TObject);
    procedure acProjIsoExecute(Sender: TObject);
    procedure acProjXYExecute(Sender: TObject);
    procedure acProjXZExecute(Sender: TObject);
    procedure acProjYZExecute(Sender: TObject);
    procedure acRedessinerExecute(Sender: TObject);
    procedure btnColorZMiniMaillageClick(Sender: TObject);
    procedure btnDetourerMNTClick(Sender: TObject);
    procedure btnExportGCPClick(Sender: TObject);
    procedure btnSetThetaClick(Sender: TObject);
    procedure btnSetPhiClick(Sender: TObject);
    procedure btnSetMagnZClick(Sender: TObject);
    procedure btnSetZoomClick(Sender: TObject);

    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure chkDrawVolumesChange(Sender: TObject);
    procedure cmbRepresentationChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure sclValeurChange(Sender: TObject);
  strict private
    FModeEditValues: TModeEditValues;
    procedure SetPanelEditValues(const M: TModeEditValues; const QValue: double);
    procedure SetThetaPhi(const QTheta, QPhi: double);

  private
    FVue3DParams: TVue3DParams;
    { private declarations }
    FDocTopo   : TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FMyMaillage: TMaillage;
    procedure Redessiner();
    procedure RefreshControlesParametresVue();
  public
    { public declarations }
    function InitialiseByPointer(const QBDDEntites  : TBDDEntites;
                                 const QDocTopo     : TToporobotStructure2012;
                                 const QMaillage    : TMaillage;
                                 const QVue2DParams : TVue2DParams): boolean;

    // libération du contexte
    procedure Finaliser();
  end;

var
  frmVue3DExt: TfrmVue3DExt;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TfrmVue3DExt }

procedure TfrmVue3DExt.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Finaliser();
end;

procedure TfrmVue3DExt.FormShow(Sender: TObject);
var
  MH, MV: Integer;

begin
  MH := 40;
  MV := 40;
  self.Top    := MV;
  self.Left   := MH;
  self.Width  := Screen.Width  - 2 * MH;
  self.Height := Screen.Height - 5 * MH;

  AfficherMessage('Init Vue 3D GDI');

  btnExportGCP.Caption := GetResourceString(rsVUE3D_BTN_EXPORT_GCP);
  self.Width    := Screen.Width  - 200;
  self.Height   := Screen.Height - 160;
  self.Position := poScreenCenter;
end;

procedure TfrmVue3DExt.Panel1Click(Sender: TObject);
begin

end;

function TfrmVue3DExt.InitialiseByPointer(const QBDDEntites  : TBDDEntites;
                                          const QDocTopo     : TToporobotStructure2012;
                                          const QMaillage    : TMaillage;
                                          const QVue2DParams : TVue2DParams): boolean;
var
  QDevelViseesVisibles: double;
begin
  Result := False;
  FDocTopo           := QDocTopo;
  FBDDEntites        := QBDDEntites;
  FMyMaillage := QMaillage;
  FVue3DParams.Filtres             := QVue2DParams.ongVueFiltres;
  FVue3DParams.ElementsDrawn       := [edPolygonals, edBounds, edReferentiel, edENTRANCE_MKS];
  FVue3DParams.ModeRepresentation  := rgSEANCES; //QVue2DParams.ongModeRepresentation;

  FVue3DParams.Theta               := 45.00;
  FVue3DParams.Phi                 := 32.00;
  btnExportGCP.Enabled             := false;
  FVue3DParams.FovOrZoom           :=  1.00;
  FVue3DParams.CoefMagnification   :=  1.00;
  FVue3DParams.ColorBackGround     := clWhite;
  FVue3DParams.LineCube.SetAttributes(clBlue, 192, 1, 0.05);
  FVue3DParams.FillOpacity         := QVue2DParams.ongFillOpacite;
  FVue3DParams.ViseesLargeur       := QVue2DParams.ongViseesLargeurInPX;
  FVue3DParams.ColorZMiniReseau    := QVue2DParams.ongDegradeAltMiniReseau;
  FVue3DParams.ColorZMaxiReseau    := QVue2DParams.ongDegradeAltMaxiReseau;

  FVue3DParams.ColorZMiniMNT       := QVue2DParams.ongDegradeAltMiniMNT;
  FVue3DParams.ColorZMaxiMNT       := QVue2DParams.ongDegradeAltMaxiMNT;

  FVue3DParams.DoFiltrer           := True; //chkFiltrer.Checked;

  editFiltres3D.Text       := FVue3DParams.Filtres;

  editTheta.Value          := FVue3DParams.Theta;
  editPhi.Value            := FVue3DParams.Phi;
  editZoom.Value           := FVue3DParams.FovOrZoom;
  editMagnificationZ.Value := FVue3DParams.CoefMagnification;


  // paramètres de la vue 3D
  CdrVue3DExt1.InitialiseVue3D(FBDDEntites,
                               FMyMaillage,
                               FVue3DParams,
                               RefreshControlesParametresVue);
  try

    Result := True;
    FModeEditValues := medNONE;
    pnlSetValue.Visible := false;
  finally
  end;
end;


procedure TfrmVue3DExt.sclValeurChange(Sender: TObject);
begin
  editValue.Value := sclValeur.Position / 100.00;
end;

// callback pour rafraichissement des contrôles
procedure TfrmVue3DExt.RefreshControlesParametresVue();
begin
  editTheta.OnChange          := nil;
  editPhi.OnChange            := nil;
  editZoom.OnChange           := nil;
  editMagnificationZ.OnChange := nil;
  editTheta.Value             := CdrVue3DExt1.GetTheta();
  editPhi.Value               := CdrVue3DExt1.GetPhi();
  editZoom.Value              := CdrVue3DExt1.GetZoom();
  editMagnificationZ.Value    := CdrVue3DExt1.GetFactZ();
  btnExportGCP.Enabled        := (0 = trunc(editPhi.Value));
end;

procedure TfrmVue3DExt.btnExportGCPClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QIdxFilter: integer;
  QPhi      : Double;
begin
  QPhi := CdrVue3DExt1.GetPhi;
  if (Trunc(QPhi) > 0) then
  begin
    ShowMessage(GetResourceString(rsMSG_EXPORT_GCP_PHI_MUST_BE_ZERO));
    exit();
  end;
  if (DoDialogSaveFile('Fichier GCP|*.gcp|Tous|*.*', '.gcp', QFileName, QIdxFilter)) then
  begin
    CdrVue3DExt1.ExportGCP(QFileName);
  end;
end;

procedure TfrmVue3DExt.btnSetThetaClick(Sender: TObject);
begin
  SetPanelEditValues(medORIENTATION, editTheta.Value);
end;

procedure TfrmVue3DExt.btnSetPhiClick(Sender: TObject);
begin
  SetPanelEditValues(medELEVATION, editPhi.Value);
end;

procedure TfrmVue3DExt.btnSetMagnZClick(Sender: TObject);
begin
  SetPanelEditValues(medMAGNIFICATION, editMagnificationZ.Value);
end;

procedure TfrmVue3DExt.btnSetZoomClick(Sender: TObject);
begin
  SetPanelEditValues(medZOOM, editZoom.Value);
end;

procedure TfrmVue3DExt.Button4Click(Sender: TObject);
  function MiouMiou(const Miou: TCurrencyEdit): double;
  begin
     Miou.Value := editValue.Value;
     Result := Miou.Value;
  end;
begin
  case FModeEditValues of
    medNONE         : pass;
    medORIENTATION  : FVue3DParams.Theta             := MiouMiou(editTheta);
    medELEVATION    : FVue3DParams.Phi               := MiouMiou(editPhi);
    medZOOM         : FVue3DParams.FovOrZoom         := MiouMiou(editZoom);
    medMAGNIFICATION: FVue3DParams.CoefMagnification := MiouMiou(editMagnificationZ);
  end;
  SetPanelEditValues(medNONE, -1);
  Redessiner();
end;

procedure TfrmVue3DExt.Button5Click(Sender: TObject);
begin
  SetPanelEditValues(medNONE, -1);
end;

procedure TfrmVue3DExt.Button6Click(Sender: TObject);
var
  P: TVue3DParams;
begin
  P := CdrVue3DExt1.GetParamsVue3D();
  CdrVue3DExt1.SetAndRedessVue3D(P);
end;

procedure TfrmVue3DExt.chkDrawVolumesChange(Sender: TObject);
begin

end;

procedure TfrmVue3DExt.cmbRepresentationChange(Sender: TObject);
begin

end;



procedure TfrmVue3DExt.acRedessinerExecute(Sender: TObject);
begin
  Redessiner();
end;

procedure TfrmVue3DExt.btnColorZMiniMaillageClick(Sender: TObject);
begin

end;

procedure TfrmVue3DExt.btnDetourerMNTClick(Sender: TObject);
var
  BB: TMNTBoundingBox;
begin
  if (not FMyMaillage.IsValidMaillage()) then exit;
  BB.C1 := FBDDEntites.GetCoinBasGauche();
  BB.C2 := FBDDEntites.GetCoinHautDroit();
  BB.C1.Z := -666;
  BB.C2.Z := 8848;

  FMyMaillage.DisableDisplayOutOfBoundingBox(BB);
end;


procedure TfrmVue3DExt.acExporterSVGExecute(Sender: TObject);
var
  EWE: TStringDirectoryFilename;
  beuh: String;
  DoXHTML, QDoExportMNT: Boolean;
  QFilterIndex: integer;
  QParam3D: TVue3DParams;
begin
  QParam3D := CdrVue3DExt1.GetParamsVue3D();
  QDoExportMNT := FMyMaillage.IsValidMaillage();
  if (QDoExportMNT) then
  begin
    case MessageDlg('Le maillage 3D sera exporté', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: QDoExportMNT := true;
      mrNo : QDoExportMNT := false;
      mrCancel: exit;
    end;
  end;
  EWE := 'SVG_3D_001.svg';
  if (DoDialogSaveFile('Scalable Vector Graphics (*.svg)|*.svg|' +
                       'XHTML document (*.xhtml)|*.xhtml',
                       '.svg', EWE, QFilterIndex)) then
  begin
    beuh := ExtractFileExt(EWE);
    DoXHTML := (Pos(beuh, 'xht') > 0);
    CdrVue3DExt1.ExporterVue3DEnSVG_TSVGCanvas(EWE, DoXHTML, QParam3D, QDoExportMNT, True); // TODO: En attente pour les profils
  end;
end;

procedure TfrmVue3DExt.acExportNuageDePointsExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := 'NuagePoints001.csv';
  if (DoDialogSaveFile('Fichiers PLY (*.ply)|*.ply|Tous|*.*', '.ply', QFileName, QFilterIndex)) then
  begin
    FBDDEntites.ExporterAntennesNuagePoints(QFileName);
  end;
end;
procedure TfrmVue3DExt.SetThetaPhi(const QTheta, QPhi: double);
begin
  FVue3DParams.Theta := QTheta;
  FVue3DParams.Phi   := QPhi;
  editTheta.Value    := FVue3DParams.Theta;
  editPhi.Value      := FVue3DParams.Phi;
  btnExportGCP.Enabled := (0 = trunc(QPhi));
  Redessiner();
end;

procedure TfrmVue3DExt.acParamVue3DExecute(Sender: TObject);
var
  WU: TVue3DParams;
begin
  WU  := CdrVue3DExt1.GetParamsVue3D();
  if (ParametrerOngletVue3D(WU)) then CdrVue3DExt1.SetAndRedessVue3D(WU);
end;

procedure TfrmVue3DExt.acProjIsoExecute(Sender: TObject);
begin
  SetThetaPhi(45.0, 32.0);
end;

procedure TfrmVue3DExt.acProjXYExecute(Sender: TObject);
begin
  SetThetaPhi(270.0, 90.0);
end;

procedure TfrmVue3DExt.acProjXZExecute(Sender: TObject);
begin
  SetThetaPhi(270.0, 0.0);
end;

procedure TfrmVue3DExt.acProjYZExecute(Sender: TObject);
begin
  SetThetaPhi(0.0, 0.0);
end;

procedure TfrmVue3DExt.Finaliser();
begin
  CdrVue3DExt1.FinaliseVue3D();
end;

procedure TfrmVue3DExt.SetPanelEditValues(const M: TModeEditValues; const QValue: double);
const FATXE = 26;
  procedure MiouMiou(const QMin, QMax: double; const QPrompt: string; const QY: integer);
  begin
    pnlSetValue.Left    := Panel3.Left  + 2;
    pnlSetValue.Width   := Panel3.Width - 2;
    pnlSetValue.Top     := Panel3.Top   + QY;
    lbNomValeur.Caption := QPrompt;
    editValue.Value     := QValue;
    lbValueMax.Caption  := Format(FORMAT_NB_REAL_3_DEC, [QMax]);
    lbValueMin.Caption  := Format(FORMAT_NB_REAL_3_DEC, [QMin]);

    sclValeur.Position  := Trunc(100 * QValue);
    sclValeur.Min       := Trunc(100 * QMin);
    sclValeur.Max       := Trunc(100 * QMax);
    pnlSetValue.Visible := True;
  end;
begin
  FModeEditValues := M;
  case M of
    medNONE         : pnlSetValue.Visible := false;
    medORIENTATION  : MiouMiou(0.00, 360.00, 'Orientation'   , editTheta.Top           + FATXE);
    medELEVATION    : MiouMiou(0.00,  90.00, 'Elevation'     , editPhi.Top             + FATXE);
    medZOOM         : MiouMiou(0.10,  10.00, 'Zoom'          , editZoom.Top            + FATXE);
    medMAGNIFICATION: MiouMiou(0.50,  16.00, 'Magnification' , editMagnificationZ.Top  + FATXE);
  end;
end;

procedure TfrmVue3DExt.Redessiner();
begin
  CdrVue3DExt1.SetAndRedessVue3D(FVue3DParams);
end;

end.

