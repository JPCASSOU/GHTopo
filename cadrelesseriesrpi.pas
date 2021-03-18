unit CadreLesSeriesRPI;
{$INCLUDE CompilationParameters.inc}
{$ERROR: Voir si c'est réellement utilisé}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Classes, SysUtils,
  FileUtil, curredit,
  ToporobotClasses2012,
  CallDialogsStdVersion,
  Forms, Controls, ExtCtrls, StdCtrls, ActnList, Buttons, Grids, Menus,
  CadreSerieIndependant, UnitObjetSerie;

type

  { TCdrSeriesRPI }

  TCdrSeriesRPI = class(TFrame)
    acnLstCdrRPISeries: TActionList;
    acGotoNext: TAction;
    acGotoPred: TAction;
    acGotoFirst: TAction;
    acGotoLast: TAction;
    acFindSerie: TAction;
    acApplyModifs: TAction;
    acNewSerie: TAction;
    acGoto10Prev: TAction;
    acGoto10Next: TAction;
    acRemoveSerie: TAction;
    CdrSerieIndependant1: TCdrSerieIndependant;
    imgLstCdrRPISeries: TImageList;
    Panel1: TPanel;
    pnlButtons: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    lbIndexSerie: TStaticText;
    procedure acApplyModifsExecute(Sender: TObject);
    procedure acFindSerieExecute(Sender: TObject);
    procedure acGoto10NextExecute(Sender: TObject);
    procedure acGoto10PrevExecute(Sender: TObject);
    procedure acGotoFirstExecute(Sender: TObject);
    procedure acGotoLastExecute(Sender: TObject);
    procedure acGotoNextExecute(Sender: TObject);
    procedure acGotoPredExecute(Sender: TObject);
    procedure acNewSerieExecute(Sender: TObject);
    procedure acRemoveSerieExecute(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { private declarations }
    FDocuTopo: TToporobotStructure2012;
    FCurrentInternalIdxSerie: integer;
    procedure GotoSerieByInternalIdx(const QIdx: integer);
    procedure GotoSerieByOffset(const Offset: integer);
  public
    { public declarations }
    function Initialiser(const FD: TToporobotStructure2012; const QIndexSerie: integer): boolean;
  end;

implementation

{$R *.lfm}

{ TCdrSeriesRPI }

procedure TCdrSeriesRPI.GotoSerieByOffset(const Offset: integer);
var
  QIdxSerie, n: integer;
begin
  QIdxSerie := FCurrentInternalIdxSerie + Offset;
  n := FDocuTopo.GetNbSeries();
  if (n = 0) then Exit;
  if (QIdxSerie < 0) then QIdxSerie := 0;
  if (QIdxSerie >= (n - 1)) then QIdxSerie := n - 1;
  GotoSerieByInternalIdx(QIdxSerie);
end;
procedure TCdrSeriesRPI.GotoSerieByInternalIdx(const QIdx: integer);
var
  EWE: TObjSerie;
begin
  FCurrentInternalIdxSerie := QIdx;
  EWE := FDocuTopo.GetSerie(QIdx);
  CdrSerieIndependant1.Initialise(FDocuTopo, EWE, EWE.GetNumeroDeSerie());
  lbIndexSerie.Caption := format(FORMAT_NB_INTEGER, [EWE.GetNumeroDeSerie()]);
end;

procedure TCdrSeriesRPI.acApplyModifsExecute(Sender: TObject);
begin
  CdrSerieIndependant1.ImplementerModifs();
  CdrSerieIndependant1.RefreshTableaux();
end;

procedure TCdrSeriesRPI.acFindSerieExecute(Sender: TObject);
var
  WU: Integer;
begin
  WU := SelectionDansListe(FDocuTopo, mslSERIE, FCurrentInternalIdxSerie, True);
  GotoSerieByInternalIdx(WU);
end;

procedure TCdrSeriesRPI.acGoto10NextExecute(Sender: TObject);
begin
  GotoSerieByOffset( 10);
end;

procedure TCdrSeriesRPI.acGoto10PrevExecute(Sender: TObject);
begin
  GotoSerieByOffset(-10);
end;

procedure TCdrSeriesRPI.acGotoFirstExecute(Sender: TObject);
begin
  GotoSerieByInternalIdx(0);
end;

procedure TCdrSeriesRPI.acGotoLastExecute(Sender: TObject);
begin
  GotoSerieByInternalIdx(FDocuTopo.GetNbSeries() - 1);
end;

procedure TCdrSeriesRPI.acGotoNextExecute(Sender: TObject);
begin
  GotoSerieByOffset( 1);
end;

procedure TCdrSeriesRPI.acGotoPredExecute(Sender: TObject);
begin
  GotoSerieByOffset(-1);
end;

procedure TCdrSeriesRPI.acNewSerieExecute(Sender: TObject);
begin
  FDocuTopo.CreateNewSerie(1, 0);
  GotoSerieByInternalIdx(FDocuTopo.GetNbSeries() - 1);
end;

procedure TCdrSeriesRPI.acRemoveSerieExecute(Sender: TObject);
var
  EWE: TObjSerie;
  WU: String;
begin
  EWE := FDocuTopo.GetSerie(FCurrentInternalIdxSerie);
  WU := Format('Supprimer la série #%d (%d - %s)', [FCurrentInternalIdxSerie, EWE.GetNumeroDeSerie(), EWE.GetNomSerie()]);

  if (GHTopoQuestionOuiNon(WU)) then
  begin
    FDocuTopo.RemoveSerie(FCurrentInternalIdxSerie);
    GotoSerieByInternalIdx(0);
  end;
end;




function TCdrSeriesRPI.Initialiser(const FD: TToporobotStructure2012; const QIndexSerie: integer): boolean;
var
  QInternalIdx: integer;
  QObjetSerie: TObjSerie;
begin
  FCurrentInternalIdxSerie := 0;
  FDocuTopo := FD;
  FDocuTopo.GetSerieByIdxSerie (QIndexSerie, QObjetSerie, FCurrentInternalIdxSerie);
  CdrSerieIndependant1.Initialise(FD, QObjetSerie, QIndexSerie);
  lbIndexSerie.Caption := format(FORMAT_NB_INTEGER, [QObjetSerie.GetNumeroDeSerie()]);
end;

procedure TCdrSeriesRPI.Panel1Click(Sender: TObject);
begin

end;



end.

