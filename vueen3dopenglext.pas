unit VueEn3DOpenGLExt;

// Date: 07/11/2013
// Statut: Opérationnel
// Appel par passage de pointeur sur BDD entités: OK
// Appel par chargement de fichier: En cours
// 01/06/2015 : Support des maillages de surface
// 15/04/2021 : Magnification Z OK
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees, Common, UnitEntitesExtended, UnitClasseMaillage,
  unitUtilsComposants,
  CadreVue3DOpenGLExt,
  CallDialogsStdVersion, Classes, SysUtils,
  FileUtil, curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls;

type

  { TfrmVueEn3DOpenGLExt }

  TfrmVueEn3DOpenGLExt = class(TForm)
    btnDetourerMNT: TButton;
    btnParametrerVue: TButton;
    Button2: TButton;
    Button3: TButton;
    CdrVue3DOpenGLExt1: TCdrVue3DOpenGLExt;
    chkFiltrer: TCheckBox;
    editFOV: TCurrencyEdit;
    editPhi: TCurrencyEdit;
    editTheta: TCurrencyEdit;
    grbxParamsMaillage: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbStatusMaillage: TStaticText;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    sclFOV: TScrollBar;
    sclPhi: TScrollBar;
    sclTheta: TScrollBar;
    procedure btnParametrerVueClick(Sender: TObject);
    procedure btnDetourerMNTClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure FormShow(Sender: TObject);
    procedure sclPhiChange(Sender: TObject);
    procedure sclThetaChange(Sender: TObject);
    procedure sclFOVChange(Sender: TObject);
  private
    { private declarations }
    FBDDEntites : TBDDEntites;
    FMyMaillage : TMaillage;
    procedure InitialiserVue();
    procedure ReconstruireScene();
    procedure RefreshControlesParametresVue();
    procedure AfficherInfosMaillage();
  public
    { public declarations }
    function InitialiseByPointers(const QBDD: TBDDEntites; const QM: TMaillage; const QFiltres: string): boolean;

    // libération du contexte
    procedure Finaliser;
  end;

var
  frmVueEn3DOpenGLExt: TfrmVueEn3DOpenGLExt;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TfrmVueEn3DOpenGLExt }

procedure TfrmVueEn3DOpenGLExt.InitialiserVue();
var
  QPV: TVue3DParams;
begin
  QPV.ElementsDrawn       := [edBounds, edPolygonals, edVolumes];
  QPV.Theta               := 45.00;
  QPV.Phi                 := 32.00;
  QPV.FovOrZoom           := 60.00;
  QPV.ColorCube           := clRed;
  QPV.ColorBackGround     := clBlack;
  QPV.ColorReferentiel    := clMaroon;

  QPV.CoefMagnification   := 1.00;
  QPV.ModeRepresentation  := rgSEANCES;            // mode de dessin par défaut = séances
  QPV.ColorZMiniReseau    := clGreen;
  QPV.ColorZMaxiReseau    := clMaroon;
  // le maillage
  QPV.ColorZMiniMNT       := clGreen;
  QPV.ColorZMaxiMNT       := clMaroon;
  QPV.MaillageOpacity     := 128;
  QPV.MaillageModeDessin  := M3D_MESH;
  QPV.MaillageUseDegrades := false;
  CdrVue3DOpenGLExt1.InitialiserVue3D(QPV,
                                      FBDDEntites,
                                      FMyMaillage,
                                      '',
                                      RefreshControlesParametresVue);
  sclTheta.Position := trunc(QPV.Theta);
  sclPhi.Position   := trunc(QPV.Phi);
  sclFOV.Position   := trunc(QPV.FovOrZoom);
  grbxParamsMaillage.Enabled        := FMyMaillage.IsValidMaillage();

  CdrVue3DOpenGLExt1.SetParamsVue3D(QPV, True);
end;
procedure TfrmVueEn3DOpenGLExt.ReconstruireScene();
var
  QPV: TVue3DParams;
begin
  QPV := CdrVue3DOpenGLExt1.GetParamsVue3D();
  CdrVue3DOpenGLExt1.SetParamsVue3D(QPV, True);
end;



procedure TfrmVueEn3DOpenGLExt.RefreshControlesParametresVue;
begin
  pass; // Indispensable
end;



procedure TfrmVueEn3DOpenGLExt.btnParametrerVueClick(Sender: TObject);
var
  QPV: TVue3DParams;
begin
  QPV := CdrVue3DOpenGLExt1.GetParamsVue3D();
  //showmessage('Maillage: '+ inttostr(ord(QPV.MaillageModeDessin)));
  if (ParametrerOngletVue3D(QPV)) then
  begin
    QPV.DoFiltrer  := false;
    QPV.Theta      := sclTheta.Position;
    QPV.Phi        := sclPhi.Position;
    QPV.FovOrZoom  := sclFOV.Position;
    CdrVue3DOpenGLExt1.SetParamsVue3D(QPV, True);
   // showmessage('Maillage: '+ inttostr(ord(QPV.MaillageModeDessin)));
  end;
end;

procedure TfrmVueEn3DOpenGLExt.btnDetourerMNTClick(Sender: TObject);
var
  BB: TMNTBoundingBox;
begin
  if (not FMyMaillage.IsValidMaillage()) then exit;
  BB.C1 := FBDDEntites.GetCoinBasGauche();
  BB.C2 := FBDDEntites.GetCoinHautDroit();
  BB.C1.Z := -666;
  BB.C2.Z := 8848;
  FMyMaillage.DisableDisplayOutOfBoundingBox(BB);
  ReconstruireScene();
end;



procedure TfrmVueEn3DOpenGLExt.Button2Click(Sender: TObject);
begin
  ReconstruireScene();
end;



procedure TfrmVueEn3DOpenGLExt.AfficherInfosMaillage;
var
  BF: TColor;
  EWE: String;
  QMaillage : TMaillage;
begin
  BF := clSilver;
  QMaillage := CdrVue3DOpenGLExt1.GetMaillage();
  if (QMaillage.IsValidMaillage()) then
  begin
    BF := clGreen;
    case QMaillage.GetTypeMaillage() of
      tmREGULAR_GRID : EWE := Format('Grid %dx%d', [QMaillage.GetNbCellsX(), QMaillage.GetNbCellsY()]);
      tmTRIANGLES    : EWE := Format('TIN: %d vertex, %d triangles', [QMaillage.GetNbVertex(), QMaillage.GetNbTriangles()]);
    else
      EWE := 'Unknown MNT type';
    end;
  end
  else
  begin
    BF := clRed;
  end;
  ShowMessage(EWE);
  lbStatusMaillage.Color := BF;
  lbStatusMaillage.Caption := EWE;
end;






procedure TfrmVueEn3DOpenGLExt.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Finaliser;
end;



procedure TfrmVueEn3DOpenGLExt.FormShow(Sender: TObject);
begin
  self.Width  := Screen.Width  - 160;
  self.Height := Screen.Height - 200;
  self.Left   := 80;
  self.Top    := 50;
  self.Position := poDesigned;
  InitialiserVue();
end;


procedure TfrmVueEn3DOpenGLExt.sclPhiChange(Sender: TObject);
var
  QPV: TVue3DParams;
begin
  QPV := CdrVue3DOpenGLExt1.GetParamsVue3D();
  QPV.Phi := sclPhi.Position;
  editPhi.Value := QPV.Phi;
  CdrVue3DOpenGLExt1.SetParamsVue3D(QPV, false);
end;

procedure TfrmVueEn3DOpenGLExt.sclThetaChange(Sender: TObject);
var
  QPV: TVue3DParams;
begin
  QPV := CdrVue3DOpenGLExt1.GetParamsVue3D();
  QPV.Theta := sclTheta.Position;
  editTheta.Value := QPV.Theta;
  CdrVue3DOpenGLExt1.SetParamsVue3D(QPV, false);
end;

procedure TfrmVueEn3DOpenGLExt.sclFOVChange(Sender: TObject);
var
  QPV: TVue3DParams;
begin
  QPV := CdrVue3DOpenGLExt1.GetParamsVue3D();
  QPV.FovOrZoom := sclFOV.Position;
  editFOV.Value := QPV.FovOrZoom;
  CdrVue3DOpenGLExt1.SetParamsVue3D(QPV, false);
end;





function TfrmVueEn3DOpenGLExt.InitialiseByPointers(const QBDD: TBDDEntites; const QM: TMaillage; const QFiltres: string): boolean;
var
  QDevelViseesVisibles: double;
begin
  Result := False;
  try
    FMyMaillage := QM;
    FBDDEntites := QBDD;
    result := True;
  except
  end;
end;



procedure TfrmVueEn3DOpenGLExt.Finaliser;
begin
  CdrVue3DOpenGLExt1.FinaliserVue3D;
end;

end.

