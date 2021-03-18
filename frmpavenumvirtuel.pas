unit frmPaveNumVirtuel;
{$ERROR Inutilisé}

// Clavier numérique virtuel pour l'écran tactile d'une tablette ou d'un Raspberry
// Justification:
// 1. Les claviers des OS sont inadaptés
// 2. Ce clavier contient une calculatrice intégrée
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  evaluateur_expressions,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, StdCtrls;

type

  { TdlgPaveNumVirtuel }

  TdlgPaveNumVirtuel = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnPow10: TButton;
    btnRAZTextBox: TButton;
    btnCos: TButton;
    btnTan: TButton;
    btnXPowY: TButton;
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
    btn_CloseParenth: TButton;
    btn_Divide: TButton;
    btn_Dot: TButton;
    btn_Egal: TButton;
    btn_Neg: TButton;
    btn_Minus: TButton;
    btn_openParenth: TButton;
    btn_Plus: TButton;
    btn_Product: TButton;
    btnBackSpace: TButton;
    btnPI: TButton;
    btnSin: TButton;
    btnExp1: TButton;
    btnInvX: TButton;
    btnAsin: TButton;
    btnAcos: TButton;
    btnAtan: TButton;
    btnExp: TButton;
    btnAnswer: TButton;
    btnNthRoot: TButton;
    Button8: TButton;
    Button9: TButton;
    expLog10: TButton;
    btnLog: TButton;
    Button7: TButton;
    editFormula: TEdit;
    pnlFonctions: TPanel;
    pnlClavierNumerique: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnAcosClick(Sender: TObject);
    procedure btnAnswerClick(Sender: TObject);
    procedure btnAsinClick(Sender: TObject);
    procedure btnAtanClick(Sender: TObject);
    procedure btnCosClick(Sender: TObject);
    procedure btnExp1Click(Sender: TObject);
    procedure btnExpClick(Sender: TObject);
    procedure btnLogClick(Sender: TObject);
    procedure btnNthRootClick(Sender: TObject);
    procedure btnPIClick(Sender: TObject);
    procedure btnRAZTextBoxClick(Sender: TObject);
    procedure btnSinClick(Sender: TObject);
    procedure btnTanClick(Sender: TObject);
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
    procedure btn_CloseParenthClick(Sender: TObject);
    procedure btn_DivideClick(Sender: TObject);
    procedure btn_DotClick(Sender: TObject);
    procedure btn_EgalClick(Sender: TObject);
    procedure btn_MinusClick(Sender: TObject);
    procedure btn_NegClick(Sender: TObject);
    procedure btnBackSpaceClick(Sender: TObject);
    procedure btn_openParenthClick(Sender: TObject);
    procedure btn_PlusClick(Sender: TObject);
    procedure btn_ProductClick(Sender: TObject);
    procedure btnXPowYClick(Sender: TObject);
    procedure btnInvXClick(Sender: TObject);
    procedure btnPow10Click(Sender: TObject);
    procedure expLog10Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAnswer: string;
    procedure AddCharacter(const C: char);
    procedure AddTexte(const S: string);

    { private declarations }
  public
    { public declarations }
    procedure Initialiser(const V: double);
    function GetValue(): double;
  end;

var
  dlgPaveNumVirtuel: TdlgPaveNumVirtuel;

implementation

{$R *.lfm}
procedure TdlgPaveNumVirtuel.AddCharacter(const C: char);
var
  S: Char;
begin
  editFormula.Text := editFormula.Text + C;
end;

procedure TdlgPaveNumVirtuel.AddTexte(const S: string);
begin
  editFormula.Text := editFormula.Text + S;
end;

procedure TdlgPaveNumVirtuel.btnAcosClick(Sender: TObject);
begin
  AddTexte('acosd(');
end;

procedure TdlgPaveNumVirtuel.BitBtn1Click(Sender: TObject);
begin

end;

procedure TdlgPaveNumVirtuel.btnAnswerClick(Sender: TObject);
begin
  editFormula.Text := editFormula.Text + FAnswer;
end;

procedure TdlgPaveNumVirtuel.btnAsinClick(Sender: TObject);
begin
  AddTexte('asind(');
end;

procedure TdlgPaveNumVirtuel.btnAtanClick(Sender: TObject);
begin
  AddTexte('atand(');
end;

procedure TdlgPaveNumVirtuel.btnRAZTextBoxClick(Sender: TObject);
begin
  editFormula.Text := '';
end;

procedure TdlgPaveNumVirtuel.btnSinClick(Sender: TObject);
begin
  AddTexte('sind(');
end;

procedure TdlgPaveNumVirtuel.btnTanClick(Sender: TObject);
begin
  AddTexte('tand(');
end;




procedure TdlgPaveNumVirtuel.btn_EgalClick(Sender: TObject);
var
  WU: Double;
begin
  WU := EvaluationRapideExpression(editFormula.Text);
  editFormula.text := FloatToStr(WU);
  FAnswer := FloatToStr(WU);
end;

procedure TdlgPaveNumVirtuel.btn_MinusClick(Sender: TObject);
begin
  AddCharacter('-');
end;


procedure TdlgPaveNumVirtuel.btn_NegClick(Sender: TObject);
begin
  AddCharacter('-');
end;

procedure TdlgPaveNumVirtuel.btn_openParenthClick(Sender: TObject);
begin
  AddCharacter('(');
end;

procedure TdlgPaveNumVirtuel.btn_PlusClick(Sender: TObject);
begin
  AddCharacter('+');
end;

procedure TdlgPaveNumVirtuel.btn_ProductClick(Sender: TObject);
begin
  AddCharacter('*');
end;

procedure TdlgPaveNumVirtuel.expLog10Click(Sender: TObject);
begin
  AddTexte('log(');
end;

procedure TdlgPaveNumVirtuel.FormShow(Sender: TObject);
begin
  self.left := 8;
  self.top  := 8;
end;

procedure TdlgPaveNumVirtuel.btnXPowYClick(Sender: TObject);
begin
  AddTexte('^');
end;

procedure TdlgPaveNumVirtuel.btnInvXClick(Sender: TObject);
begin
  AddTexte('1/(');
end;

procedure TdlgPaveNumVirtuel.btnLogClick(Sender: TObject);
begin
  AddTexte('ln(');
end;

procedure TdlgPaveNumVirtuel.btnNthRootClick(Sender: TObject);
begin
  addtexte('^(1/(');
end;

procedure TdlgPaveNumVirtuel.btnPow10Click(Sender: TObject);
begin
  AddTexte('*10^(');
end;

procedure TdlgPaveNumVirtuel.btnBackSpaceClick(Sender: TObject);
var
  WU: TCaption;
  n: Integer;
begin
  WU := editFormula.Text;
  n := length(WU);
  system.Delete(WU, n, 1);
  editFormula.Text := WU;
end;

procedure TdlgPaveNumVirtuel.btnCosClick(Sender: TObject);
begin
  AddTexte('cosd(');
end;

procedure TdlgPaveNumVirtuel.btnExp1Click(Sender: TObject);
begin
  AddTexte('e');
end;

procedure TdlgPaveNumVirtuel.btnExpClick(Sender: TObject);
begin
  AddTexte('exp(');
end;

procedure TdlgPaveNumVirtuel.btnPIClick(Sender: TObject);
begin
  AddTexte('pi');
end;

function TdlgPaveNumVirtuel.GetValue: double;
begin
  Result := EvaluationRapideExpression(editFormula.Text);
end;

procedure TdlgPaveNumVirtuel.Initialiser(const V: double);
begin
  FAnswer := '';
  editFormula.Text := FloatToStr(V);
end;

function TdlgPaveNumVirtuel.GetValue(): double;
begin

end;

procedure TdlgPaveNumVirtuel.btn_0Click(Sender: TObject);
begin
  AddCharacter('0');
end;

procedure TdlgPaveNumVirtuel.btn_1Click(Sender: TObject);
begin
  AddCharacter('1');
end;

procedure TdlgPaveNumVirtuel.btn_2Click(Sender: TObject);
begin
  AddCharacter('2');
end;

procedure TdlgPaveNumVirtuel.btn_3Click(Sender: TObject);
begin
  AddCharacter('3');
end;

procedure TdlgPaveNumVirtuel.btn_4Click(Sender: TObject);
begin
  AddCharacter('4');
end;

procedure TdlgPaveNumVirtuel.btn_5Click(Sender: TObject);
begin
  AddCharacter('5');
end;

procedure TdlgPaveNumVirtuel.btn_6Click(Sender: TObject);
begin
  AddCharacter('6');
end;

procedure TdlgPaveNumVirtuel.btn_7Click(Sender: TObject);
begin
  AddCharacter('7');
end;

procedure TdlgPaveNumVirtuel.btn_8Click(Sender: TObject);
begin
  AddCharacter('8');
end;

procedure TdlgPaveNumVirtuel.btn_9Click(Sender: TObject);
begin
  AddCharacter('9');
end;

procedure TdlgPaveNumVirtuel.btn_CloseParenthClick(Sender: TObject);
begin
  AddCharacter(')');
end;

procedure TdlgPaveNumVirtuel.btn_DivideClick(Sender: TObject);
begin
  AddCharacter('/');
end;

procedure TdlgPaveNumVirtuel.btn_DotClick(Sender: TObject);
begin
  AddCharacter('.');
end;

end.

