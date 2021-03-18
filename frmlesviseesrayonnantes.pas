unit frmLesViseesRayonnantes;
{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils,
  Common,
  StructuresDonnees,
  ToporobotClasses2012,
  UnitEntitesExtended,
  CadreViseesAntenne,
  FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

  { TfrmGestionViseesRayonnantes }

  TfrmGestionViseesRayonnantes = class(TForm)
    BitBtn1: TBitBtn;
    CdrAntennes1: TCdrAntennes;
    Panel1: TPanel;
    procedure acViderListeAntennesExecute(Sender: TObject);
  private
    { private declarations }
    FDocuTopo  : TToporobotStructure2012;
    FBDDEntites: TBDDEntites;

  public
    { public declarations }
    function Initialiser(const B: TToporobotStructure2012;
                         const E: TBDDEntites): boolean;
    procedure Finaliser();
  end;

var
  frmGestionViseesRayonnantes: TfrmGestionViseesRayonnantes;

implementation

{$R *.lfm}

{ TfrmGestionViseesRayonnantes }


procedure TfrmGestionViseesRayonnantes.Finaliser;
begin
  ShowMessageFmt('%s.Finaliser()', [classname]);
end;

procedure TfrmGestionViseesRayonnantes.acViderListeAntennesExecute(Sender: TObject);
begin

end;

function TfrmGestionViseesRayonnantes.Initialiser(const B: TToporobotStructure2012; const E: TBDDEntites): boolean;
begin
  self.Caption := GetResourceString(rsGESTION_VISEES_RADIANTES);
  result := false;
  try
    FDocuTopo   := B;
    FBDDEntites := E;
    CdrAntennes1.Initialise(FDocuTopo);
    result := true;
  except
    ;
  end;
end;



end.

