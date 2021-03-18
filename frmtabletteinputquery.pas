unit frmTabletteInputQuery;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TdlgInputQuery }

  TdlgInputQuery = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    editValue: TEdit;
    lbPrompt: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetCaptionPromptValue(const ACaption, APrompt, AValue: string);
    function  GetValue: string;
  end;

var
  dlgInputQuery: TdlgInputQuery;

implementation

{$R *.lfm}

{ TdlgInputQuery }

procedure TdlgInputQuery.FormShow(Sender: TObject);
begin
  self.Position := poScreenCenter;
  editValue.SetFocus;
end;

procedure TdlgInputQuery.SetCaptionPromptValue(const ACaption, APrompt, AValue: string);
begin
  self.Caption         := ACaption;
  lbPrompt.Caption     := APrompt;
  editValue.Text       := AValue;
end;

function TdlgInputQuery.GetValue: string;
begin
  Result := editValue.Text;
end;


end.

