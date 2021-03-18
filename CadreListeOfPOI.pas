unit CadreListeOfPOI;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  UnitClasseMaillage,
  ToporobotClasses2012,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TCdrListeOfPOI }

  TCdrListeOfPOI = class(TFrame)
    lbNbPOI: TLabel;
    lsbPOI: TListBox;
  private
    FDocTopo: TToporobotStructure2012;
    procedure ListerLesPOI();
  public
    function Initialiser(const FD: TToporobotStructure2012): boolean;
  end;

implementation

{$R *.lfm}

{ TCdrListeOfPOI }

procedure TCdrListeOfPOI.ListerLesPOI();
var
  i, Nb: Integer;
  MyPOI: TPointOfInterest;
begin
  lsbPOI.Clear;
  Nb := FDocTopo.GetNbPointsOfInterests();
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MyPOI := FDocTopo.GetPointOfInterest(i);
    lsbPOI.Items.Add(Format('%d.%d - %s', [MyPOI.Serie, MyPOI.Station, MyPOI.Description]));
  end;
  lsbPOI.ItemIndex := 0;
end;

function TCdrListeOfPOI.Initialiser(const FD: TToporobotStructure2012): boolean;
begin
  Result := false;
  try
    FDocTopo := FD;
    ListerLesPOI();
    Result   := True;
  except
    pass;
  end;
end;

end.

