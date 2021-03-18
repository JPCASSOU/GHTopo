unit frmFrontalSeries;
// 13/06/2019: Point de contrôle temporel (contrôle de version)

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Classes, SysUtils,
  ToporobotClasses2012,
  unitobjetserie,
  UnitEntitesExtended,
  cadreNavigateurSeries,
  CadreSerieIndependant,
  FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  {TfrmGestionSeries }

  TfrmGestionSeries = class(TForm)
    CdrNavigateurSeries1: TCdrNavigateurSeries;
    CdrSerieIndependant1: TCdrSerieIndependant;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
    FDocuTopo: TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    procedure CreateNewSerie();
    procedure PreparerCadreSerie(const S: TObjSerie; const QIdx: integer);
    function  GotoSerieDisAllowed(): Boolean;
    procedure ImplementerModifsSerie();
    procedure GotoSerie();
  public
    { public declarations }
    function Initialiser(const B: TToporobotStructure2012; const E: TBDDEntites): boolean;
    procedure Finaliser();

  end;

var
  frmGestionSeries: TfrmGestionSeries;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TfrmGestionSeries }

procedure TfrmGestionSeries.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
end;

procedure TfrmGestionSeries.GotoSerie();
var
  n: Integer;
  MySerie: TObjSerie;
begin
  n := CdrNavigateurSeries1.GetIdxSerie();
  MySerie := FDocuTopo.GetSerie(n);
  PreparerCadreSerie(MySerie, n);
  affichermessage(format(FORMAT_NB_INTEGER, [n]));
end;

function TfrmGestionSeries.GotoSerieDisAllowed(): Boolean;
begin
  Result := CdrSerieIndependant1.IsDataModified();
end;

procedure TfrmGestionSeries.ImplementerModifsSerie();
var
  n, QOldIdxSerie: Integer;
  QNbErreurs     : integer;
  EWE: TObjSerie;
begin
  QNbErreurs := 0;
  n := CdrNavigateurSeries1.GetIdxSerie();
  if (n = 0) then // DONE: la série 0 n'est pas modifiable
  begin
    ShowMessage(rsMSG_SERIE_INITIALISATION);
    Exit;
  end;
  // sauvegarder l'ancien numéro de série
  EWE := FDocuTopo.GetSerie(n);
  QOldIdxSerie := EWE.GetNumeroDeSerie();
  if (CdrSerieIndependant1.ImplementerModifs(QNbErreurs)) then
  begin
    CdrSerieIndependant1.RefreshTableaux();
    EWE := FDocuTopo.GetSerie(n);
    // TODO: Réattribution des antennes en cas de changement du numéro de la série
    if (QOldIdxSerie <> EWE.GetNumeroDeSerie()) then
    begin
      // TODO: A valider
      //FDocuTopo.ReAttribuerViseesRayonnantesDeUneSerie(EWE, QOldIdxSerie);
    end;
  end;
end;

procedure TfrmGestionSeries.CreateNewSerie();
begin
  FDocuTopo.CreateNewSerie(1,0, -1, GetResourceString(rsNOUVELLE_SERIE));
end;

procedure TfrmGestionSeries.Finaliser();
begin
  //ShowMessageFmt('%s.Finaliser()', [classname]);
end;



function TfrmGestionSeries.Initialiser(const B: TToporobotStructure2012;
                                       const E: TBDDEntites): boolean;
const QH = 110;
begin
  result := false;
  FDocuTopo := B;
  FBDDEntites:= E;
  self.Caption := FDocuTopo.GetNomEtude() + ': Séries';
  self.Left := 4;
  self.Top  := QH;
  self.Height := Screen.Height - QH - 120 ;
  CdrNavigateurSeries1.Initialiser(FDocuTopo,
                                   self.GotoSerieDisAllowed,
                                   self.GotoSerie,
                                   self.CreateNewSerie,
                                   self.ImplementerModifsSerie);
  result := True;
end;
procedure TfrmGestionSeries.PreparerCadreSerie(const S: TObjSerie; const QIdx: integer);
begin
  CdrSerieIndependant1.Initialise(FDocuTopo, FBDDEntites, S, QIdx);
end;

end.

