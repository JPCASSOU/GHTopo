unit frmTabletteSaisieValeurs;
// Saisie de valeurs à la tablette (sans utiliser le clavier virtuel)

{$INCLUDE CompilationParameters.inc}

interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type

  { TdlgSaisieValeursNum }

  TdlgSaisieValeursNum = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    lbPrompt: TLabel;
    Panel1: TPanel;
    lbValue: TStaticText;
    lbOldValue: TStaticText;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FHasAlreadyDecSep: boolean;
    { private declarations }
    procedure AddChar(const D: string);

  public
    { public declarations }
    procedure SetValueNum(const V: double);
    procedure SetPrompt(const S: string);
    function  GetValueNum: double;
  end;

var
  dlgSaisieValeursNum: TdlgSaisieValeursNum;

implementation

{$R *.lfm}

{ TdlgSaisieValeursNum }

procedure TdlgSaisieValeursNum.FormShow(Sender: TObject);
begin
  self.Position := poScreenCenter;
  FHasAlreadyDecSep := False;
  lbValue.Caption   := '';
end;

procedure TdlgSaisieValeursNum.AddChar(const D: string);
begin
  lbValue.Caption := lbValue.Caption + D;
end;

procedure TdlgSaisieValeursNum.SetValueNum(const V: double);
begin
  lbOldValue.Caption := Format(FORMAT_NB_REAL_3_DEC, [V]);
end;

procedure TdlgSaisieValeursNum.SetPrompt(const S: string);
begin
  lbPrompt.Caption := Format(FORMAT_STRING, [S]);
end;

function TdlgSaisieValeursNum.GetValueNum: double;
begin
  Result := ConvertirEnNombreReel(lbValue.Caption, 0);
end;

procedure TdlgSaisieValeursNum.Button14Click(Sender: TObject);
begin
  lbValue.Caption := '';
  FHasAlreadyDecSep := False;
end;

procedure TdlgSaisieValeursNum.Button1Click(Sender: TObject);
begin
  AddChar('1');
end;

procedure TdlgSaisieValeursNum.Button2Click(Sender: TObject);
begin
  AddChar('2');
end;

procedure TdlgSaisieValeursNum.Button3Click(Sender: TObject);
begin
  AddChar('3');
end;

procedure TdlgSaisieValeursNum.Button4Click(Sender: TObject);
begin
  AddChar('4');
end;

procedure TdlgSaisieValeursNum.Button5Click(Sender: TObject);
begin
  AddChar('5');
end;

procedure TdlgSaisieValeursNum.Button6Click(Sender: TObject);
begin
  AddChar('6');
end;

procedure TdlgSaisieValeursNum.Button7Click(Sender: TObject);
begin
  AddChar('7');
end;

procedure TdlgSaisieValeursNum.Button8Click(Sender: TObject);
begin
  AddChar('8');
end;

procedure TdlgSaisieValeursNum.Button9Click(Sender: TObject);
begin
  AddChar('9');
end;

procedure TdlgSaisieValeursNum.Button10Click(Sender: TObject);
begin
  // signe moins efface la valeur
  lbValue.Caption := '-';
  FHasAlreadyDecSep := False;
end;

procedure TdlgSaisieValeursNum.Button11Click(Sender: TObject);
begin
  AddChar('0');
end;

procedure TdlgSaisieValeursNum.Button12Click(Sender: TObject);
  procedure EWE();
  begin
    FHasAlreadyDecSep := (Pos(DefaultFormatSettings.DecimalSeparator, lbValue.Caption) > 0);
  end;
begin
  EWE();
  if (FHasAlreadyDecSep) then Exit;
  AddChar(DefaultFormatSettings.DecimalSeparator);
  EWE();
end;

procedure TdlgSaisieValeursNum.Button13Click(Sender: TObject);
var
  EWE: TCaption;
  L: Integer;
begin
  EWE := lbValue.Caption;
  L := Length(EWE);
  if (L > 0) then
  begin
    system.Delete(EWE, L, 1);
    lbValue.Caption := EWE;
  end;
end;

end.

