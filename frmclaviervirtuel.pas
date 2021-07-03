unit frmClavierVirtuel;
// Clavier virtuel pour l'écran tactile d'une tablette ou d'un Raspberry
// Justification: Les claviers des OS sont inadaptés
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  unitAutoCompletionSpeleo,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

{ TdlgClavierVirtuel }

 TdlgClavierVirtuel = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnAddPOI: TButton;
    btn_1: TButton;
    btn_0: TButton;
    btn_Aa: TButton;
    btn_gt: TButton;
    btn_lt: TButton;
    btn_Hashtag: TButton;
    btn_SimpleQuote: TButton;
    btn_Point_Interrogation: TButton;
    btn_Comma: TButton;
    btn_Period: TButton;
    btn_Semicolon: TButton;
    btn_DbleQuote: TButton;
    btn_openParenth: TButton;
    btn_CloseParenth: TButton;
    btn_Dollar: TButton;
    btn_Xx: TButton;
    btn_Cc: TButton;
    btn_Vv: TButton;
    btn_Bb: TButton;
    btn_Nn: TButton;
    btn_Kk1: TButton;
    btn_Ll1: TButton;
    btn_Arobase: TButton;
    btn_Underscore: TButton;
    btn_Rr: TButton;
    btn_Ww: TButton;
    btn_Tt: TButton;
    btn_Yy: TButton;
    btn_Uu: TButton;
    btn_Ii: TButton;
    btn_Oo: TButton;
    btn_Pp: TButton;
    btn_Ss: TButton;
    btn_Gg: TButton;
    btn_Hh: TButton;
    btn_Jj: TButton;
    btn_Kk: TButton;
    btn_Qq: TButton;
    btn_Dd: TButton;
    btn_Ff: TButton;
    btn_Ll: TButton;
    btn_Mm: TButton;
    btn_Zz: TButton;
    btn_Egal: TButton;
    btn_Product: TButton;
    btn_Divide: TButton;
    btn_Plus: TButton;
    btn_Dot: TButton;
    btn_Minus: TButton;
    btn_2: TButton;
    btn_3: TButton;
    btn_4: TButton;
    btn_5: TButton;
    btn_6: TButton;
    btn_7: TButton;
    btn_8: TButton;
    btn_9: TButton;
    btn_Ee: TButton;
    btn_Space: TButton;
    Button1: TButton;
    btnRAZTextBox: TButton;
    btnSuggestionsOK: TButton;
    btnSuggestionsCancel: TButton;
    btnSuggestions: TButton;
    editTexte: TEdit;
    lbPrompt: TLabel;
    lsbSuggestions: TListBox;
    pnlSuggestions: TPanel;
    pnlCommandes: TPanel;
    pnlPartieAlpha: TPanel;
    pnlPonctuation: TPanel;
    pnlClavierNumerique: TPanel;
    Shape1: TShape;
    btnCapsLock: TToggleBox;
    procedure btnAddPOIClick(Sender: TObject);
    procedure btnRAZTextBoxClick(Sender: TObject);
    procedure btnSuggestionsClick(Sender: TObject);
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
    procedure btn_AaClick(Sender: TObject);
    procedure btn_BbClick(Sender: TObject);
    procedure btn_CcClick(Sender: TObject);
    procedure btn_ArobaseClick(Sender: TObject);
    procedure btn_CloseParenthClick(Sender: TObject);
    procedure btn_CommaClick(Sender: TObject);
    procedure btn_DbleQuoteClick(Sender: TObject);
    procedure btn_gtClick(Sender: TObject);
    procedure btn_Point_InterrogationClick(Sender: TObject);
    procedure btn_ltClick(Sender: TObject);
    procedure btn_NnClick(Sender: TObject);
    procedure btn_openParenthClick(Sender: TObject);
    procedure btn_PeriodClick(Sender: TObject);
    procedure btn_SemicolonClick(Sender: TObject);
    procedure btn_HashtagClick(Sender: TObject);
    procedure btn_SimpleQuoteClick(Sender: TObject);
    procedure btn_SpaceClick(Sender: TObject);
    procedure btn_UnderscoreClick(Sender: TObject);
    procedure btn_VvClick(Sender: TObject);
    procedure btn_DollarClick(Sender: TObject);
    procedure btn_XxClick(Sender: TObject);
    procedure btn_DdClick(Sender: TObject);
    procedure btn_DivideClick(Sender: TObject);
    procedure btn_DotClick(Sender: TObject);
    procedure btn_EeClick(Sender: TObject);
    procedure btn_EgalClick(Sender: TObject);
    procedure btn_FfClick(Sender: TObject);
    procedure btn_HhClick(Sender: TObject);
    procedure btn_IiClick(Sender: TObject);
    procedure btn_JjClick(Sender: TObject);
    procedure btn_KkClick(Sender: TObject);
    procedure btn_LlClick(Sender: TObject);
    procedure btn_MinusClick(Sender: TObject);
    procedure btn_OoClick(Sender: TObject);
    procedure btn_PlusClick(Sender: TObject);
    procedure btn_PpClick(Sender: TObject);
    procedure btn_ProductClick(Sender: TObject);
    procedure btn_QqClick(Sender: TObject);
    procedure btn_RrClick(Sender: TObject);
    procedure btn_SsClick(Sender: TObject);
    procedure btn_TtClick(Sender: TObject);
    procedure btn_UuClick(Sender: TObject);
    procedure btn_GgClick(Sender: TObject);
    procedure btn_MmClick(Sender: TObject);
    procedure btn_WwClick(Sender: TObject);
    procedure btn_YyClick(Sender: TObject);
    procedure btn_ZzClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnSuggestionsOKClick(Sender: TObject);
    procedure btnSuggestionsCancelClick(Sender: TObject);
    procedure editTexteChange(Sender: TObject);
    procedure editTexteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    { private declarations }
    FAutoCompletionSpeleo: TAutoCompletionSpeleo;
    procedure AddCharacter(const C: char);
    procedure PrepareListeSuggestions();
    procedure SetFocusAtTextbox();
  public
    { public declarations }
    function  Initialiser(const ACaption, APrompt: string; var AValue: string): boolean;
    procedure Finaliser();
    function  GetValue(): string;
  end;

var
  dlgClavierVirtuel: TdlgClavierVirtuel;

implementation

{$R *.lfm}

{ TdlgClavierVirtuel }
function TdlgClavierVirtuel.Initialiser(const ACaption, APrompt: string; var AValue: string): boolean;
begin
  result := false;
  pnlSuggestions.Visible := false;
  pnlSuggestions.Top     := pnlPonctuation.Top;
  pnlSuggestions.Left    := pnlPonctuation.Left;
  FAutoCompletionSpeleo := TAutoCompletionSpeleo.create;
  result := FAutoCompletionSpeleo.Initialiser();
  try
    self.Caption       := ACaption;
    lbPrompt.Caption   := APrompt;
    editTexte.Text     := AValue;
    PrepareListeSuggestions();
    result := True;
  except
  end;
end;

procedure TdlgClavierVirtuel.Finaliser();
begin
  try
    FAutoCompletionSpeleo.Finaliser();
  finally
    FreeAndNil(FAutoCompletionSpeleo);
  end;
end;

function TdlgClavierVirtuel.GetValue(): string;
begin
  Result := trim(editTexte.Text);
end;




procedure TdlgClavierVirtuel.AddCharacter(const C: char);
var
  S: Char;
begin
  editTexte.Text := editTexte.Text + C;
end;

procedure TdlgClavierVirtuel.btnRAZTextBoxClick(Sender: TObject);
begin
  editTexte.Text := '';
end;

procedure TdlgClavierVirtuel.btnSuggestionsClick(Sender: TObject);
begin
  pnlSuggestions.Visible := True;
end;

procedure TdlgClavierVirtuel.btnAddPOIClick(Sender: TObject);
begin
  editTexte.Text := UpperCase(KEYWORD_POI_TODO) + editTexte.Text;
end;





procedure TdlgClavierVirtuel.btn_0Click(Sender: TObject);
begin
  AddCharacter('0');
end;

procedure TdlgClavierVirtuel.btn_1Click(Sender: TObject);
begin
  AddCharacter('1');
end;

procedure TdlgClavierVirtuel.btn_2Click(Sender: TObject);
begin
  AddCharacter('2');
end;

procedure TdlgClavierVirtuel.btn_3Click(Sender: TObject);
begin
  AddCharacter('3');
end;

procedure TdlgClavierVirtuel.btn_4Click(Sender: TObject);
begin
  AddCharacter('4');
end;

procedure TdlgClavierVirtuel.btn_5Click(Sender: TObject);
begin
  AddCharacter('5');
end;

procedure TdlgClavierVirtuel.btn_6Click(Sender: TObject);
begin
  AddCharacter('6');
end;

procedure TdlgClavierVirtuel.btn_7Click(Sender: TObject);
begin
  AddCharacter('7');
end;

procedure TdlgClavierVirtuel.btn_8Click(Sender: TObject);
begin
  AddCharacter('8');
end;

procedure TdlgClavierVirtuel.btn_9Click(Sender: TObject);
begin
  AddCharacter('9');
end;

procedure TdlgClavierVirtuel.btn_AaClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'A', 'a'));
end;

procedure TdlgClavierVirtuel.btn_BbClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'B', 'b'));
end;

procedure TdlgClavierVirtuel.btn_CcClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'C', 'c'));
end;

procedure TdlgClavierVirtuel.btn_CloseParenthClick(Sender: TObject);
begin
  AddCharacter(')');
end;

procedure TdlgClavierVirtuel.btn_CommaClick(Sender: TObject);
begin
  AddCharacter(',');
end;

procedure TdlgClavierVirtuel.btn_DbleQuoteClick(Sender: TObject);
begin
 AddCharacter('"');
end;

procedure TdlgClavierVirtuel.btn_XxClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'X', 'x'));
end;

procedure TdlgClavierVirtuel.btn_DdClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'D', 'd'));
end;

procedure TdlgClavierVirtuel.btn_DivideClick(Sender: TObject);
begin
  AddCharacter('/');
end;

procedure TdlgClavierVirtuel.btn_DotClick(Sender: TObject);
begin
  AddCharacter('.');
end;

procedure TdlgClavierVirtuel.btn_EeClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'E', 'e'));
end;

procedure TdlgClavierVirtuel.btn_EgalClick(Sender: TObject);
begin
  AddCharacter('=');
end;

procedure TdlgClavierVirtuel.btn_FfClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'F', 'f'));
end;

procedure TdlgClavierVirtuel.btn_IiClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'I', 'i'));
end;

procedure TdlgClavierVirtuel.btn_JjClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'J', 'j'));
end;

procedure TdlgClavierVirtuel.btn_KkClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'K', 'k'));
end;

procedure TdlgClavierVirtuel.btn_LlClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'L', 'l'));
end;

procedure TdlgClavierVirtuel.btn_ltClick(Sender: TObject);
begin
  AddCharacter('<');
end;

procedure TdlgClavierVirtuel.btn_MinusClick(Sender: TObject);
begin
  AddCharacter('-');
end;

procedure TdlgClavierVirtuel.btn_ArobaseClick(Sender: TObject);
begin
  AddCharacter('@');
end;

procedure TdlgClavierVirtuel.btn_OoClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'O', 'o'));
end;

procedure TdlgClavierVirtuel.btn_openParenthClick(Sender: TObject);
begin
  AddCharacter('(');
end;

procedure TdlgClavierVirtuel.btn_PeriodClick(Sender: TObject);
begin
  AddCharacter(';');
end;

procedure TdlgClavierVirtuel.btn_PlusClick(Sender: TObject);
begin
  AddCharacter('+');
end;

procedure TdlgClavierVirtuel.btn_PpClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'P', 'p'));
end;

procedure TdlgClavierVirtuel.btn_ProductClick(Sender: TObject);
begin
  AddCharacter('*');
end;

procedure TdlgClavierVirtuel.btn_QqClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'Q', 'q'));
end;

procedure TdlgClavierVirtuel.btn_RrClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'R', 'r'));
end;

procedure TdlgClavierVirtuel.btn_SemicolonClick(Sender: TObject);
begin
  AddCharacter(':');
end;

procedure TdlgClavierVirtuel.btn_HashtagClick(Sender: TObject);
begin
  AddCharacter('#');
end;

procedure TdlgClavierVirtuel.btn_SimpleQuoteClick(Sender: TObject);
begin
  AddCharacter('''');
end;

procedure TdlgClavierVirtuel.btn_SpaceClick(Sender: TObject);
begin
  AddCharacter(' ');
end;

procedure TdlgClavierVirtuel.btn_SsClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'S', 's'));
end;

procedure TdlgClavierVirtuel.btn_TtClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'T', 't'));
end;

procedure TdlgClavierVirtuel.btn_UnderscoreClick(Sender: TObject);
begin
  AddCharacter('_');
end;

procedure TdlgClavierVirtuel.btn_UuClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'U', 'u'));
end;

procedure TdlgClavierVirtuel.btn_VvClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'V', 'v'));
end;

procedure TdlgClavierVirtuel.btn_DollarClick(Sender: TObject);
begin
  AddCharacter('$');
end;

procedure TdlgClavierVirtuel.btn_WwClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'W', 'w'));
end;

procedure TdlgClavierVirtuel.btn_MmClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'M', 'm'));
end;

procedure TdlgClavierVirtuel.btn_NnClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'N', 'n'));
end;

procedure TdlgClavierVirtuel.btn_GgClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'G', 'g'));
end;

procedure TdlgClavierVirtuel.btn_gtClick(Sender: TObject);
begin
  AddCharacter('>');
end;

procedure TdlgClavierVirtuel.btn_Point_InterrogationClick(Sender: TObject);
begin
  AddCharacter('?');
end;

procedure TdlgClavierVirtuel.btn_HhClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'H', 'h'));
end;

procedure TdlgClavierVirtuel.btn_YyClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'Y', 'y'));
end;

procedure TdlgClavierVirtuel.btn_ZzClick(Sender: TObject);
begin
  AddCharacter(IIF(btnCapsLock.Checked, 'Z', 'z'));
end;

procedure TdlgClavierVirtuel.Button1Click(Sender: TObject);
var
  WU: TCaption;
  n: Integer;
begin
  WU := editTexte.Text;
  n := length(WU);
  system.Delete(WU, n, 1);
  editTexte.Text := WU;
end;

procedure TdlgClavierVirtuel.btnSuggestionsOKClick(Sender: TObject);
var
  n: Integer;
  EWE: TCaption;
begin
  n := lsbSuggestions.ItemIndex;
  if (n > 0) then
  begin
    EWE := Trim(editTexte.Text); // supprime du même coup les espaces superflus
    EWE += ' ' + FAutoCompletionSpeleo.getElement(n);
    editTexte.Text := EWE;
  end;
  pnlSuggestions.Visible := false;
  SetFocusAtTextbox();
end;

procedure TdlgClavierVirtuel.btnSuggestionsCancelClick(Sender: TObject);
begin
  pnlSuggestions.Visible := false;
  SetFocusAtTextbox();
end;

procedure TdlgClavierVirtuel.editTexteChange(Sender: TObject);
var
  EWE: TCaption;
  n: Integer;
begin
  EWE := editTexte.Text;
  n := length(EWE);
  // deux espaces consécutifs = ouvre une boite de suggestions
  if ((n > 1) and (EWE[n - 1] = ' ')) then
  begin
    System.Delete(EWE, n, 1); // supprime le deuxième espace
    pnlSuggestions.Visible := True;
    lsbSuggestions.SetFocus;
  end;
end;

procedure TdlgClavierVirtuel.editTexteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  pass;
end;



procedure TdlgClavierVirtuel.SetFocusAtTextbox();
begin
  editTexte.SetFocus;
  editTexte.SelStart  := Length(editTexte.Text);
  editTexte.SelLength := 0;
end;


procedure TdlgClavierVirtuel.PrepareListeSuggestions();
var
  i, Nb: Integer;
begin
  btnSuggestions.Enabled := false;
  btnSuggestions.Hint    := 'Suggestions';

  lsbSuggestions.Sorted := false;
  lsbSuggestions.Clear;
  Nb := FAutoCompletionSpeleo.getNbElements();
  if (Nb = 0) then exit;
  for i := 0 to Nb -1 do lsbSuggestions.Items.add(FAutoCompletionSpeleo.getElement(i));
  lsbSuggestions.ItemIndex := 0;
  btnSuggestions.Enabled := true;
end;




end.

