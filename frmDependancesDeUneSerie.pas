unit frmDependancesDeUneSerie;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils,
  StructuresDonnees,
  UnitListesSimplesWithGeneriques,
  Common,
  ToporobotClasses2012, UnitObjetSerie,
  FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TdlgDependancesSerie }

  TdlgDependancesSerie = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    lbNbDependances: TLabel;
    lsbSeriesDependantes: TListBox;
    lbSerie: TStaticText;
    lbMessageConfirmation: TStaticText;
  private
    FDocTopo          : TToporobotStructure2012;
    FListeDependances : TListOfIntegers;

    procedure ListerDependancesSerie(const IdxSerie: integer);
  public
    function Initialiser(const FD: TToporobotStructure2012;
                         const IdxSerie: integer): boolean;
    procedure Finaliser();
  end;

var
  dlgDependancesSerie: TdlgDependancesSerie;

implementation

{$R *.lfm}

{ TdlgDependancesSerie }

procedure TdlgDependancesSerie.ListerDependancesSerie(const IdxSerie: integer);
var
  MySerie, EWE: TObjSerie;
  n, i, QInternalIdx: Integer;
  WU: String;
begin
  AfficherMessage(format('ListerDependancesSerie: %d', [IdxSerie]));
  MySerie := FDocTopo.GetSerie(IdxSerie);
  self.Caption := 'Supprimer une série';
  n := 0;
  lbSerie.Caption := Format('%d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
  if (FDocTopo.FindIndexesSeriesRattachedAtSerie(IdxSerie, FListeDependances)) then
  begin
    lsbSeriesDependantes.Clear;
    n := FListeDependances.GetNbElements();
    for i := 0 to n - 1 do
    begin
      FDocTopo.GetSerieByNumeroSerie(FListeDependances.GetElement(i), EWE, QInternalIdx);
      WU := Format('%d: %d.%d -> %d.%d %s', [EWE.GetNumeroDeSerie(),
                                              EWE.GetNoSerieDep(), EWE.GetNoPointDep(),
                                              EWE.GetNoSerieArr(), EWE.GetNoPointArr(),
                                              EWE.GetNomSerie()]);
      lsbSeriesDependantes.Items.Add(WU);
    end;
    lsbSeriesDependantes.ItemIndex := 0;
  end;
  lbNbDependances.caption := format('%d dépendances', [n]);
end;

function TdlgDependancesSerie.Initialiser(const FD: TToporobotStructure2012;
                                          const IdxSerie: integer): boolean;
begin
  result   := False;
  FDocTopo := FD;
  lbMessageConfirmation.Caption := Format(GetResourceString(rsMSG_WARN_DELETE_ITEM), [IdxSerie]);
  FListeDependances := TListOfIntegers.Create;
  try
    ListerDependancesSerie(IdxSerie);
    result   := True;
  except
  end;
end;

procedure TdlgDependancesSerie.Finaliser();
begin
  try
    FListeDependances.Clear;
  finally
    FreeAndNil(FListeDependances);//FListeDependances.Free;
  end;
end;

end.

