unit frmEditSerie;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitObjetSerie,
  CadreSerieIndependant,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls;

type

{ TdlgEditSerie }

 TdlgEditSerie = class(TForm)
    btnClose: TBitBtn;
    CdrSerieIndependant1: TCdrSerieIndependant;
    chkDontCloseIfErrors: TCheckBox;
    Panel1: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
    FFaireQuelqueChose: boolean;
  public
    { public declarations }
    function PrepareDialog(const FD: TToporobotStructure2012;
                           const PD: TBDDEntites;
                           const MySerie: TObjSerie;
                           const QIdx   : integer): boolean;
    function DoPerform(): boolean;
  end;

var
  dlgEditSerie: TdlgEditSerie;

implementation
uses
  DGCDummyUnit,
  CallDialogsStdVersion
  ;

{$R *.lfm}

procedure TdlgEditSerie.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  QNbErreurs: integer;
begin
  CanClose := false;
  if (GHTopoQuestionOuiNon(rsCDR_SERIES_VALID_SERIE)) then
  begin
    FFaireQuelqueChose := CdrSerieIndependant1.ImplementerModifs(QNbErreurs);
    if (QNbErreurs > 0) then CanClose := not chkDontCloseIfErrors.Checked
                        else CanClose := True;
  end
  else
    CanClose := True;
end;

function TdlgEditSerie.PrepareDialog(const FD     : TToporobotStructure2012;
                                     const PD     : TBDDEntites;
                                     const MySerie: TObjSerie;
                                     const QIdx   : integer): boolean;
begin
  Result := false;
  self.Caption := Format(GetResourceString(rsCDR_SERIES_EDIT_SERIE), [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie(), QIdx]);
  FFaireQuelqueChose := false;
  try
    CdrSerieIndependant1.Initialise(FD, PD, MySerie, QIdx);
    Result := True;
  except
    pass;
  end;
end;



function TdlgEditSerie.DoPerform(): boolean;
begin
  Result := FFaireQuelqueChose;
end;

end.

