unit frmQuestionOuiNonWithAction;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

type

  { TdlgQuestionOuiNonWithAction }

  TdlgQuestionOuiNonWithAction = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    chkDoAction: TCheckBox;
    lbQuestion: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public
    function Initialiser(const Title, Msg1, Msg2: string; const DoAction: boolean): boolean;
    function GetChecked(): boolean;
  end;

var
  dlgQuestionOuiNonWithAction: TdlgQuestionOuiNonWithAction;

implementation

{$R *.lfm}

{ TdlgQuestionOuiNonWithAction }

procedure TdlgQuestionOuiNonWithAction.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

function TdlgQuestionOuiNonWithAction.Initialiser(const Title, Msg1, Msg2: string; const DoAction: boolean): boolean;
begin
  result := false;
  try
    self.Caption          := GetResourceString(Title);
    lbQuestion.Caption    := GetResourceString(Msg1);
    chkDoAction.Caption   := GetResourceString(Msg2);
    chkDoAction.Checked   := DoAction;
    result := True;
  except
    pass;
  end;
end;

function TdlgQuestionOuiNonWithAction.GetChecked(): boolean;
begin
  Result := chkDoAction.Checked;
end;

end.

