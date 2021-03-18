unit dlgListeNearViseesTo;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  ToporobotClasses2012,
  StructuresDonnees,
  UnitEntitesExtended,
  Common,
  CallDialogsStdVersion,
  CadreListeNearStations, UnitObjetSerie, Classes, SysUtils, FileUtil,
  curredit, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, ComCtrls,
  LCLType, ExtCtrls;

type

  { TfrmListeNearViseesTo }

  TfrmListeNearViseesTo = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    btnBouclerSurCetteStation: TButton;
    CdrListeNearestStations1: TCdrListeNearestStations;
    chkDoMatchExact: TCheckBox;
    editIDTerrain: TEdit;
    editRayonCapture: TCurrencyEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnOKClick(Sender: TObject);
    procedure btnBouclerSurCetteStationClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);

  private
    { private declarations }
    FDocuTopo        : TToporobotStructure2012;
    FBDDEntites      : TBDDEntites;
    FCurrBaseStation : TBaseStation;
    FRayonDeCapture  : double;
  public
    { public declarations }
    function  Initialiser(const TD: TToporobotStructure2012;
                          const BDD: TBDDEntites;
                          const QS: TNumeroSerie;
                          const QP: integer;
                          const R: double;
                          const QModeSelection : boolean): boolean;
    procedure Finaliser();
    function GetResultEntite(): TBaseStation;
  end;

var
  frmListeNearViseesTo: TfrmListeNearViseesTo;

implementation

{$R *.lfm}

{ TfrmListeNearViseesTo }

procedure TfrmListeNearViseesTo.btnOKClick(Sender: TObject);
begin
  try
    if (CdrListeNearestStations1.GetNbStationsFound() > 0) then
      FCurrBaseStation := CdrListeNearestStations1.GetNearEntiteSelected();
  except
    FCurrBaseStation.Entite_Serie   := -1;
    FCurrBaseStation.Entite_Station := -1;
  end;//*)
end;

procedure TfrmListeNearViseesTo.btnBouclerSurCetteStationClick(Sender: TObject);
var
  MySerie: TObjSerie;
  QIdx: integer;
  BP: TBaseStation;
begin
  if (CdrListeNearestStations1.GetNbStationsFound() > 0) then
  begin
    BP := CdrListeNearestStations1.GetNearEntiteSelected();
    if (FDocuTopo.GetSerieByNumeroSerie(FCurrBaseStation.Entite_Serie, MySerie, QIdx)) then
    if (GHTopoQuestionOuiNon(Format('Accrocher la station %d.%d à la station %d.%d', [MySerie.GetNumeroDeSerie(), MySerie.GetNbVisees() - 1,
                                                                                      BP.Entite_Serie, BP.Entite_Station]))) then
    begin
      MySerie.SetSeriePtArr(BP.Entite_Serie, BP.Entite_Station);
    end;
  end;
end;

procedure TfrmListeNearViseesTo.Button2Click(Sender: TObject);
var
  FBaseStation: TBaseStation;
begin
  if (FBDDEntites.FindStationByCle(chkDoMatchExact.Checked, Trim(editIDTerrain.Text), FBaseStation)) then
  begin
    CdrListeNearestStations1.SetCurrBaseStation(FBaseStation);
    CdrListeNearestStations1.Relister(chkDoMatchExact.Checked, editRayonCapture.Value);
  end
  else
  begin
    ShowMessage(rsMATCHNOTFOUND);
  end;
end;



function TfrmListeNearViseesTo.Initialiser(const TD: TToporobotStructure2012;
                                           const BDD: TBDDEntites;
                                           const QS: TNumeroSerie;
                                           const QP: integer;
                                           const R: double;
                                           const QModeSelection : boolean): boolean;
var
  QPositionPtZero: TPoint3Df;
begin
  self.Caption := 'Recherche de stations proches';
  AfficherMessage(Format('%s.Initialiser(%d.%d)', [ClassName, QS, QP]));
  FDocuTopo := TD;
  AfficherMessage('001');
  FBDDEntites := BDD;
  AfficherMessage('002');
  if (QModeSelection) then
  begin
    btnBouclerSurCetteStation.Visible := false;
    btnOK.Visible  := True;
    btnCancel.Kind := bkCancel;
  end
  else
  begin
    btnBouclerSurCetteStation.Visible := True;
    btnOK.Visible  := False;
    btnCancel.Kind := bkClose;
  end;
  AfficherMessage('003');
  // on recherche la station de base ; si station introuvable = ce qui suit n'a pas de sens et on quitte direct
  if (Not FBDDEntites.GetEntiteViseeFromSerSt(QS, QP, FCurrBaseStation)) then Exit(false);
  FRayonDeCapture := R; // distance d'accrochage
  editIDTerrain.Text := Format(FMTSERST, [QS, QP]);
  editRayonCapture.Value := FRayonDeCapture;
  CdrListeNearestStations1.Initialiser(FDocuTopo,
                                       FBDDEntites, FCurrBaseStation,
                                       chkDoMatchExact.Checked,
                                       editRayonCapture.Value);
  Result := True;     // Passage ici ? OK
end;

procedure TfrmListeNearViseesTo.Finaliser();
begin
  pass;
end;



function TfrmListeNearViseesTo.GetResultEntite(): TBaseStation;
begin
  Result := FCurrBaseStation;
end;

end.


