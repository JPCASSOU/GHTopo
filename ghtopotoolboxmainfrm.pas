unit GHTopoToolboxMainFrm;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  CallDialogsStdVersion,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CadreCalculette;

type

  { TForm1 }

  TForm1 = class(TForm)
    CdrCalculette1: TCdrCalculette;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var
  MH, MV: Integer;
begin
  MH := 40;
  MV := 40;
  self.Top := MV;
  self.Left:= MH;
  self.Width    := Screen.Width  - 2 * MH;
  self.Height   := Screen.Height - 5 * MV;
  self.Position := poScreenCenter;
  self.Caption := GetResourceString(rsDLG_CALC_TITLE);
  {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
  if (Not CdrCalculette1.Initialiser(FMyTabIndex, FDocuTopo, FBDDEntites, FMaillage)) then
  begin
    ShowMessage('Calculette inoperante');
    self.Close;
  end;
  lbDocTopoName.Caption := IIF(Assigned(FDocuTopo), 'Classe du document: ' + FDocuTopo.ClassName, '---');
  {$ELSE CALCULETTE_EMBEDDED_IN_GHTOPO}
  if (Not CdrCalculette1.Initialiser(0)) then
  begin
    ShowMessage('Calculette inoperante');
    self.Close;
  end;
  {$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  if (GHTopoQuestionOuiNon('Quitter GHTopoToolBox')) then CanClose := true;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

end.

