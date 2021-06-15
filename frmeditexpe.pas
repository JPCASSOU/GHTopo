unit frmEditExpe;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons
  , StructuresDonnees
  , ToporobotClasses2012
  , Common
  , CadreExpe
  ;

type

  { TdlgEditExpe }

  TdlgEditExpe = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CdrExpe1: TCdrExpe;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QExpe: TExpe; const QCaption: string): boolean;
    function GetAExpe(): TExpe;
  end;

var
  dlgEditExpe: TdlgEditExpe;

implementation

{$R *.lfm}

{ TdlgEditExpe }

procedure TdlgEditExpe.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

function TdlgEditExpe.PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QExpe: TExpe; const QCaption: string): boolean;
begin
  Self.Caption := QCaption;
  result := CdrExpe1.Initialiser(FD, QExpe);
end;

function TdlgEditExpe.GetAExpe(): TExpe;
begin
  Result := CdrExpe1.GetExpeFromForm();
end;
end.

