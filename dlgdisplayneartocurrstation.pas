unit dlgDisplayNearToCurrStation;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  StructuresDonnees,
  UnitEntitesExtended,
  FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TfrmListeStationsNearToCurrent }

  TfrmListeStationsNearToCurrent = class(TForm)
  private
    { private declarations }
    FMyBDDEntites: TBDDEntites;
    FMyStation: TEntiteEtendue;
  public
    { public declarations }
    function Initialiser(const FBDD: TBDDEntites;
                         const MyStation: TEntiteEtendue): boolean;
  end;

var
  frmListeStationsNearToCurrent: TfrmListeStationsNearToCurrent;

implementation

{$R *.lfm}

{ TfrmListeStationsNearToCurrent }

function TfrmListeStationsNearToCurrent.Initialiser(const FBDD: TBDDEntites; const MyStation: TEntiteEtendue): boolean;
begin
  result := false;


end;

end.

