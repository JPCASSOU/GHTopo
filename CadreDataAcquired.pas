unit CadreDataAcquired;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  ToporobotClasses2012,
  UnitEntitesExtended,
  +++unitDistoX2,
  Classes, SysUtils, FileUtil, Forms, Controls;

type

  { TCdrDataAcquired }

  TCdrDataAcquired = class(TFrame)
  private
    FDocTopo      : TToporobotStructure2012;
    FBDDEntites   : TBDDEntites;
    FPiloteDistoX : TPilotageDistoX;
  public
    function Initialiser(const FD: TToporobotStructure2012; const FE: TBDDEntites): boolean;
  end;

implementation

{$R *.lfm}

{ TCdrDataAcquired }

function TCdrDataAcquired.Initialiser(const FD: TToporobotStructure2012; const FE: TBDDEntites): boolean;
begin
  result := false;
  FDocTopo      := FD;
  FBDDEntites   := FE;

  result := True;
end;

end.

