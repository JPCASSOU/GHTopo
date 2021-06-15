unit frmEditReseau;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls
  , StructuresDonnees
  , ToporobotClasses2012, CadreReseau
  ;

type

  { TdlgEditReseau }

  TdlgEditReseau = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CdrReseaux1: TCdrReseaux;
    Panel1: TPanel;
  private
    { private declarations }
  public
    { public declarations }
    function PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QReseau: TReseau; const QCaption: string): boolean;
    function GetAReseau(): TReseau;
  end;

var
  dlgEditReseau: TdlgEditReseau;

implementation

{$R *.lfm}

{ TdlgEditReseau }

function TdlgEditReseau.PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QReseau: TReseau; const QCaption: string): boolean;
begin
  self.Caption := QCaption;
  Result := CdrReseaux1.Initialiser(QReseau, QIdx);
end;

function TdlgEditReseau.GetAReseau: TReseau;
begin
  Result := CdrReseaux1.GetReseauFromForm();
end;

end.

