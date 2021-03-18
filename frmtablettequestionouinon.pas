unit frmTabletteQuestionOuiNon;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TdlgTabletteQuestionOuiNon }

  TdlgTabletteQuestionOuiNon = class(TForm)
    btnYes: TBitBtn;
    btnNO: TBitBtn;
    lbPrompt: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetPrompt(const S: string);
  end;

var
  dlgTabletteQuestionOuiNon: TdlgTabletteQuestionOuiNon;

implementation

{$R *.lfm}

{ TdlgTabletteQuestionOuiNon }

procedure TdlgTabletteQuestionOuiNon.FormShow(Sender: TObject);
begin
  self.Position := poScreenCenter;

end;

procedure TdlgTabletteQuestionOuiNon.SetPrompt(const S: string);
var
  H1, HT: integer;
  EWE: TSize;
  LGMini: Integer;
  NbLignes : integer;
  function getNbLignesInText(const MyStr: string): integer;
  var
    p: SizeInt;
    qS: string;
  begin
    Result := 1;
    qS := Trim(MyStr);
    while (true) do
    begin
      p := pos(#13, qS);
      if (p > 0) then
      begin
        Result += 1;
        qS := Copy(qS, p+1, Length(qS));
      end
      else
        break;
    end;
  end;
begin
  lbPrompt.Caption := S;
  NbLignes := getNbLignesInText(S);
  //lbPrompt.AdjustFontForOptimalFill;
  EWE := lbPrompt.Canvas.TextExtent(S);
  //lbPrompt.Width  := EWE.cx;
  //lbPrompt.Height := EWE.cy;
  LGMini := 4 + btnYes.Width + 4 + btnNO.Width + 4;
  H1     := 4 + EWE.cx + 4;
  if (H1 < LGMini) then HT := LGMini else HT := H1;
  self.Width  := HT;
  self.Height := 4 + btnYes.Height + 4 + lbPrompt.Height * NbLignes + 4;
end;

end.

