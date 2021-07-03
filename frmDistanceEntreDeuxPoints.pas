unit frmDistanceEntreDeuxPoints;
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  StructuresDonnees,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitObjetSerie,
  CadreListeNearStations,
  Clipbrd, LCLType,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

  { TdlgDistanceBetweenTwoStations }

  TdlgDistanceBetweenTwoStations = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnSwapStations: TButton;
    btnCopyDistanceAngles: TButton;
    CdrListeNearestStations1: TCdrListeNearestStations;
    chkDoPerformAdditionalAction: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbDeltaX: TStaticText;
    lbDeltaY: TStaticText;
    lbDeltaZ: TStaticText;
    lbDistance: TStaticText;
    lbAzimut: TStaticText;
    lbNamespaceSt2: TStaticText;
    lbPente: TStaticText;
    lbDistanceXY: TStaticText;
    lbSerieOfStation1: TStaticText;
    lbSerieOfStation2: TStaticText;
    lbXStation2: TStaticText;
    lbYStation1: TStaticText;
    lbYStation2: TStaticText;
    lbZStation1: TStaticText;
    lbZStation2: TStaticText;
    lbStation1: TStaticText;
    lbXStation1: TStaticText;
    lbStation2: TStaticText;
    lbIDStation1: TStaticText;
    lbIDStation2: TStaticText;
    Panel1: TPanel;
    lbDescroAction: TStaticText;
    lbNamespaceSt1: TStaticText;
    Panel2: TPanel;
    procedure btnCopyDistanceAnglesClick(Sender: TObject);
    procedure btnSwapStationsClick(Sender: TObject);
  private
    FDocTopo   : TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FStation1, FStation2: TBaseStation;
    procedure PutStation1InForm(const S: TBaseStation);
    procedure PutStation2InForm(const S: TBaseStation);
    procedure PutDistsInForm();

  public
    function Initialiser(const FD: TToporobotStructure2012;
                         const FE: TBDDEntites;
                         const S1, S2: TBaseStation;
                         const DescriptionAction: string;
                         const CaptionAdditionalAction: string): boolean;
    function DoAllowAdditionalAction(): boolean;
    function GetStation1(): TBaseStation;
    function GetStation2(): TBaseStation;

    procedure Finaliser();
  end;

var
  dlgDistanceBetweenTwoStations: TdlgDistanceBetweenTwoStations;

implementation


{$R *.lfm}

{ TdlgDistanceBetweenTwoStations }


procedure TdlgDistanceBetweenTwoStations.btnSwapStationsClick(Sender: TObject);
var
  Tmp: TBaseStation;
begin
  Tmp := FStation1;
  FStation1 := FStation2;
  FStation2 := Tmp;
  PutStation1InForm(FStation1);
  PutStation2InForm(FStation2);
  PutDistsInForm();

end;

procedure TdlgDistanceBetweenTwoStations.btnCopyDistanceAnglesClick(Sender: TObject);
var
  MyClipBoard: TClipboard;
begin
  MyClipBoard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    MyClipBoard.AsText := Trim(lbDistance.Caption) + #9 +
                          Trim(lbAzimut.Caption) + #9 +
                          Trim(lbPente.Caption) + #9 +
                          Trim(lbDistanceXY.Caption);
  finally
    FreeAndNil(MyClipBoard);
  end;
end;

procedure TdlgDistanceBetweenTwoStations.PutStation1InForm(const S: TBaseStation);
var
  QIdxNameSpace, QNoSerie: integer;
  MyNamespace: TNameSpace;
begin
  DecomposeNumeroSerie(S.Entite_Serie, QIdxNameSpace, QNoSerie);
  MyNamespace := FDocTopo.GetNameSpace(QIdxNameSpace);
  lbNamespaceSt1.Caption := MyNamespace.Nom;
  lbStation1.caption   := Format(FMTSERST, [QNoSerie, S.Entite_Station]);
  lbIDStation1.Caption := S.IDTerrain;
  lbXStation1.Caption  := FormatterNombreAvecSepMilliers(S.PosStation.X);
  lbYStation1.Caption  := FormatterNombreAvecSepMilliers(S.PosStation.Y);
  lbZStation1.Caption  := FormatterNombreAvecSepMilliers(S.PosStation.Z);
end;

procedure TdlgDistanceBetweenTwoStations.PutStation2InForm(const S: TBaseStation);
var
  QIdxNameSpace, QNoSerie: integer;
  MyNamespace: TNameSpace;
begin
  DecomposeNumeroSerie(S.Entite_Serie, QIdxNameSpace, QNoSerie);
  MyNamespace := FDocTopo.GetNameSpace(QIdxNameSpace);
  lbNamespaceSt2.Caption := MyNamespace.Nom;
  lbStation2.caption   := Format(FMTSERST, [QNoSerie, S.Entite_Station]);
  lbIDStation2.Caption := S.IDTerrain;
  lbXStation2.Caption  := FormatterNombreAvecSepMilliers(S.PosStation.X);
  lbYStation2.Caption  := FormatterNombreAvecSepMilliers(S.PosStation.Y);
  lbZStation2.Caption  := FormatterNombreAvecSepMilliers(S.PosStation.Z);
  // cadre near stations - A initialiser avec la station d'extrémité S2
  CdrListeNearestStations1.Initialiser(FDocTopo, FBDDEntites, FStation2, false, 30.00);
end;

procedure TdlgDistanceBetweenTwoStations.PutDistsInForm();
var
  QDist, QAz, QPente, dp: Double;
  QV: TPoint3Df;
begin
  QV.setFrom(FStation2.PosStation.X - FStation1.PosStation.X,
             FStation2.PosStation.Y - FStation1.PosStation.Y,
             FStation2.PosStation.Z - FStation1.PosStation.Z);
  dp := QV.getProjHZ();
  GetBearingInc(UNITE_ANGULAIRE_PAR_DEFAUT, UNITE_ANGULAIRE_PAR_DEFAUT, QV, QDist, QAz, QPente);
  lbDeltaX.Caption := Format(FORMAT_NB_REAL_3_DEC, [QV.X]);
  lbDeltaY.Caption := Format(FORMAT_NB_REAL_3_DEC, [QV.Y]);
  lbDeltaZ.Caption := Format(FORMAT_NB_REAL_3_DEC, [QV.Z]);

  lbDistance.Caption   := Format(FORMAT_NB_REAL_3_DEC, [QDist]);
  lbAzimut.Caption     := Format(FORMAT_NB_REAL_6_DEC, [QAz]);
  lbPente.Caption      := Format(FORMAT_NB_REAL_6_DEC, [QPente]);
  lbDistanceXY.Caption := Format(FORMAT_NB_REAL_3_DEC, [dp]);
end;

function TdlgDistanceBetweenTwoStations.Initialiser(const FD: TToporobotStructure2012;
                                                    const FE: TBDDEntites;
                                                    const S1, S2: TBaseStation;
                                                    const DescriptionAction: string;
                                                    const CaptionAdditionalAction: string): boolean;

var
  QQ1, QQ2: Boolean;
  MySerieOfStation1, MySerieOfStation2: TObjSerie;
  QIdx1, QIdx2: integer;
  procedure S666(const L: TStaticText; const SR: TObjSerie);
  begin
    L.Caption := Format('%d - %s', [SR.GetNumeroDeSerie(), SR.GetNomSerie()]);
  end;
begin
  result := False;
  // description de l'action (calcul de distance, accrochage d'une série)
  lbDescroAction.Caption := DescriptionAction;
  FDocTopo    := FD;
  FBDDEntites := FE;
  FStation1   := S1;
  FStation2   := S2;
  QQ1 := FDocTopo.GetSerieByNumeroSerie(FStation1.Entite_Serie, MySerieOfStation1, QIdx1);
  QQ2 := FDocTopo.GetSerieByNumeroSerie(FStation2.Entite_Serie, MySerieOfStation2, QIdx2);

  S666(lbSerieOfStation1, MySerieOfStation1);
  S666(lbSerieOfStation2, MySerieOfStation2);
  PutStation1InForm(FStation1);
  PutStation2InForm(FStation2);
  PutDistsInForm();
  // action additionnelle facultative
  chkDoPerformAdditionalAction.Visible := false;
  if (CaptionAdditionalAction <> '') then
  begin
    chkDoPerformAdditionalAction.Caption := CaptionAdditionalAction;
    chkDoPerformAdditionalAction.Checked := True;
    chkDoPerformAdditionalAction.Visible := True;
  end;
  result := true;
end;

function TdlgDistanceBetweenTwoStations.DoAllowAdditionalAction(): boolean;
begin
  result := chkDoPerformAdditionalAction.Checked;
end;

function TdlgDistanceBetweenTwoStations.GetStation1(): TBaseStation;
begin
  result := FStation1;
end;

function TdlgDistanceBetweenTwoStations.GetStation2(): TBaseStation;
begin
  result := FStation2;
end;

procedure TdlgDistanceBetweenTwoStations.Finaliser();
begin
  CdrListeNearestStations1.Finaliser();
end;

end.

