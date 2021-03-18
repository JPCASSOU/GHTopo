unit frmEditCode;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons
  , StructuresDonnees
  , ToporobotClasses2012
  , Common
  , CadreCode;

type

  { TdlgEditCode }

  TdlgEditCode = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CdrCode1: TCdrCode;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }

  public
    { public declarations }
    function PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QCode: TCode; const QCaption: string): boolean;
    function GetACode(): TCode;
  end;

var
  dlgEditCode: TdlgEditCode;

implementation

{$R *.lfm}

{ TdlgEditCode }

procedure TdlgEditCode.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

function TdlgEditCode.PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QCode: TCode; const QCaption: string): boolean;
begin
  self.Caption := QCaption;
  CdrCode1.SetCode(QCode, True);
end;



function TdlgEditCode.GetACode: TCode;
begin
  Result := CdrCode1.GetCodeFromForm;
end;

end.

