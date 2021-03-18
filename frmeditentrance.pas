unit frmEditEntrance;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls
  , StructuresDonnees
  , ToporobotClasses2012
  , Common
  , UnitEntitesExtended
  , UnitClasseMaillage
  , CadreEntrance
  ;
type

  { TdlgEditEntrance }

  TdlgEditEntrance = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CdrEntreeCavites1: TCdrEntreeCavites;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QEntrance: TEntrance; const QCaption: string; const B: TBDDEntites; const M: TMaillage): boolean;
    function GetAEntrance(): TEntrance;
  end;

var
  dlgEditEntrance: TdlgEditEntrance;

implementation


{$R *.lfm}

{ TdlgEditEntrance }

procedure TdlgEditEntrance.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

function TdlgEditEntrance.PrepareDialog(const FD: TToporobotStructure2012;
                                        const QIdx: integer;
                                        const QEntrance: TEntrance;
                                        const QCaption: string;
                                        const B: TBDDEntites;
                                        const M: TMaillage): boolean;
begin
  Result := false;
  self.Caption := QCaption;
  Result := CdrEntreeCavites1.Initialiser(FD, B, M, QEntrance, QIdx);
end;



function TdlgEditEntrance.GetAEntrance: TEntrance;
begin
  Result := CdrEntreeCavites1.GetEntrance();
end;

end.

