unit frmSelectionDeSerie;

{$mode delphi}

interface

uses
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  CadreListeSeries,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

  { TdlgSelectSerie }

  TdlgSelectSerie = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CdrListeSeries1: TCdrListeSeries;
    Panel1: TPanel;
  private

  public
    function Initialiser(const FD: TToporobotStructure2012; const NumSerie: TNumeroSerie): boolean;
    procedure Finaliser();
    function  GetNumSerie(): TNumeroSerie;
  end;

var
  dlgSelectSerie: TdlgSelectSerie;

implementation

{$R *.lfm}

{ TdlgSelectSerie }

function TdlgSelectSerie.Initialiser(const FD: TToporobotStructure2012; const NumSerie: TNumeroSerie): boolean;
begin
  Result := CdrListeSeries1.Initialise(FD, nil, nil, [], false);
end;

procedure TdlgSelectSerie.Finaliser();
begin
  CdrListeSeries1.Finalise();
end;

function TdlgSelectSerie.GetNumSerie(): TNumeroSerie;
begin
  Result := CdrListeSeries1.GetSelectedNumSerie();
end;

end.

