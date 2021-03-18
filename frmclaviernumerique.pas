unit frmclaviernumerique;
{$mode delphi}

interface

uses
  StructuresDonnees,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, curredit;
type TFlagsInternes     = set of (fiIntegersOnly, fiMinusDone, fiFirstCaractereDone, fiDecimalSeparatorDone);

type TdlgClavierNumerique = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnClear: TButton;
    btn_0: TButton;
    btn_1: TButton;
    btn_2: TButton;
    btn_3: TButton;
    btn_4: TButton;
    btn_5: TButton;
    btn_6: TButton;
    btn_7: TButton;
    btn_8: TButton;
    btn_9: TButton;
    btn_Dot: TButton;
    btn_Minus: TButton;
    Button1: TButton;
    btnDeleteLastChar: TButton;
    Button3: TButton;
    editValue: TEdit;
    pnlClavierNumerique: TPanel;
    procedure btnClearClick(Sender: TObject);
    procedure btn_0Click(Sender: TObject);
    procedure btn_1Click(Sender: TObject);
    procedure btn_2Click(Sender: TObject);
    procedure btn_3Click(Sender: TObject);
    procedure btn_4Click(Sender: TObject);
    procedure btn_5Click(Sender: TObject);
    procedure btn_6Click(Sender: TObject);
    procedure btn_7Click(Sender: TObject);
    procedure btn_8Click(Sender: TObject);
    procedure btn_9Click(Sender: TObject);
    procedure btn_DotClick(Sender: TObject);
    procedure btn_MinusClick(Sender: TObject);
    procedure btnDeleteLastCharClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  // pour le clavier numérique virtuel
    FPaveNumModeSaisie: TPaveNumModeSaisie;
    FFlagsInternes : TFlagsInternes;
    procedure AddCharacter(const C: char);

  public
    function Initialiser(const Title: string; const ModeSaisie: TPaveNumModeSaisie; const Value: string): boolean;
    function AsInteger(): integer;
    function AsValue(): double;
    function AsString(): string;

  end;

var
  dlgClavierNumerique: TdlgClavierNumerique;

implementation

{$R *.lfm}



procedure TdlgClavierNumerique.btn_0Click(Sender: TObject);
begin
  AddCharacter('0');
end;

procedure TdlgClavierNumerique.btnClearClick(Sender: TObject);
begin
  editValue.Text := '';
  Exclude(FFlagsInternes, fiMinusDone);
  Exclude(FFlagsInternes, fiFirstCaractereDone);
  Exclude(FFlagsInternes, fiDecimalSeparatorDone);
end;

procedure TdlgClavierNumerique.btn_1Click(Sender: TObject);
begin
  AddCharacter('1');
end;

procedure TdlgClavierNumerique.btn_2Click(Sender: TObject);
begin
  AddCharacter('2');
end;

procedure TdlgClavierNumerique.btn_3Click(Sender: TObject);
begin
  AddCharacter('3');
end;

procedure TdlgClavierNumerique.btn_4Click(Sender: TObject);
begin
  AddCharacter('4');
end;

procedure TdlgClavierNumerique.btn_5Click(Sender: TObject);
begin
  AddCharacter('5');
end;

procedure TdlgClavierNumerique.btn_6Click(Sender: TObject);
begin
  AddCharacter('6');
end;

procedure TdlgClavierNumerique.btn_7Click(Sender: TObject);
begin
  AddCharacter('7');
end;

procedure TdlgClavierNumerique.btn_8Click(Sender: TObject);
begin
  AddCharacter('8');
end;

procedure TdlgClavierNumerique.btn_9Click(Sender: TObject);
begin
  AddCharacter('9');
end;

procedure TdlgClavierNumerique.btn_DotClick(Sender: TObject);
var
  WU: Char;
begin
  if (fiIntegersOnly in FFlagsInternes) then exit;
  WU := '.';
  if (not (fiDecimalSeparatorDone in FFlagsInternes)) then
  begin
    // le premier caractère est un point -> on ajoute un zéro devant
    if ('' = trim(editValue.Text)) then AddCharacter('0');
    AddCharacter(WU);
    Include(FFlagsInternes, fiDecimalSeparatorDone);
  end;
end;

procedure TdlgClavierNumerique.btn_MinusClick(Sender: TObject);
begin
  if ((not (fiMinusDone in FFlagsInternes)) and ('' = Trim(editValue.Text))) then
  begin
    AddCharacter('-');
    Include(FFlagsInternes, fiMinusDone);
  end;
end;

procedure TdlgClavierNumerique.btnDeleteLastCharClick(Sender: TObject);
var
  n: Integer;
  EWE: TCaption;
begin
  EWE := Trim(editValue.Text);
  n := length(EWE);
  if (n > 0) then system.Delete(EWE, n, 1)
             else Exclude(FFlagsInternes, fiFirstCaractereDone);
  editValue.Text      := EWE;
  editValue.SelStart  := Length(editValue.Text)-1;
  editValue.SelLength := 0;
  editValue.SetFocus;
end;

procedure TdlgClavierNumerique.FormShow(Sender: TObject);
begin
  editValue.SetFocus;
end;

procedure TdlgClavierNumerique.AddCharacter(const C: char);
var
  WU: TCaption;
begin
  if (not (fiFirstCaractereDone in FFlagsInternes)) then editValue.Text := '';
  WU := Trim(editValue.Text) + C;
  editValue.Text      := WU;
  editValue.SelStart  := Length(editValue.Text)-1;
  editValue.SelLength := 0;
  editValue.SetFocus;
  Include(FFlagsInternes, fiFirstCaractereDone);
end;


function TdlgClavierNumerique.Initialiser(const Title: string; const ModeSaisie: TPaveNumModeSaisie; const Value: string): boolean;

begin
  result := false;
  FPaveNumModeSaisie := pnmREAL;
  btn_Dot.Caption    := DefaultFormatSettings.DecimalSeparator;
  btn_Minus.Visible  := true;
  btn_Dot.Visible    := True;
  editValue.Text     := Value;
  FFlagsInternes     := [];
  if (length(editValue.Text) > 0) then Include(FFlagsInternes, fiFirstCaractereDone);
  case ModeSaisie of
    pnmINTEGER:
      begin
        Include(FFlagsInternes, fiIntegersOnly);
        btn_Dot.Visible    := false;
      end;
    pnmREAL:
      begin

      end;
    pnmTOPOROBOT_STATION:
      begin
        btn_Dot.Visible    := True;
        btn_Minus.Visible  := false;
        btn_Dot.Caption    := '.';
      end;
  end;
  result := true;
end;

function TdlgClavierNumerique.AsInteger(): integer;
begin
  Result := StrToIntDef(editValue.Text, 0);
end;

function TdlgClavierNumerique.AsValue(): double;
begin
  StringReplace(editValue.Text, '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]);
  StringReplace(editValue.Text, ',', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]);
  Result := StrToFloatDef(editValue.Text, 0.00);
end;
function TdlgClavierNumerique.AsString(): string;
begin
  Result := Trim(editValue.Text);
end;
end.

