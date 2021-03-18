unit frmJournal;
// Date: 19/04/2012
// Statut: Fonctionnel
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Classes,
  Common,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls, PairSplitter;

type

  { TdlgProcessing }

  TdlgProcessing = class(TForm)
    btnCopyLigne: TSpeedButton;
    btnCopyMemoErreurs: TSpeedButton;
    btnSaveJournal: TSpeedButton;
    btnSaveJournal1: TSpeedButton;
    editLigneSelectionnee: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lsbJournal: TListBox;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    pnlJournal: TPanel;
    pnlErreursWarnings: TPanel;
    memoErreurs: TSynEdit;
    lbMemoryUsage: TStaticText;
    btnClearConsole: TSpeedButton;
    procedure btnClearConsoleClick(Sender: TObject);
    procedure btnCopyMemoErreursClick(Sender: TObject);

    procedure btnSaveJournal1Click(Sender: TObject);
    procedure btnSaveJournalClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure lsbJournalClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure CopierJournalErreurs();
  end; 

var
  dlgProcessing: TdlgProcessing;

implementation

{$R *.lfm}
uses
  DGCDummyUnit,          // anti-bug 'Fin du code-source non trouvée
  CallDialogsStdVersion; // ne pas le mettre ds la zone 'interface' (bugs en cascade sinon)
procedure TdlgProcessing.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=False;
  ShowMessage(_AnsiToLCLStr(rsNOCANCLOSEWND));
end;

procedure TdlgProcessing.FormShow(Sender: TObject);
begin
  {$IFDEF GHTOPO_SIMPLIFIE}
  {$ELSE}
  {$ENDIF}
end;
procedure TdlgProcessing.btnCopyMemoErreursClick(Sender: TObject);
begin
  CopierJournalErreurs();
end;

procedure TdlgProcessing.btnClearConsoleClick(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Effacer la console')) then memoErreurs.Clear;
end;


procedure TdlgProcessing.btnSaveJournal1Click(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := Format('Errors_%s.txt', [DatePascalToDateHeureCondensee(Now)]);
  if (DoDialogSaveFile('Fichier texte|*.txt', '.txt',  QFileName, QFilterIndex)) then
    memoErreurs.Lines.SaveToFile(QFileName);
end;

procedure TdlgProcessing.btnSaveJournalClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := Format('Journal_%s.txt', [DatePascalToDateHeureCondensee(Now)]);
  if (DoDialogSaveFile('Fichier texte|*.txt', '.txt',  QFileName, QFilterIndex)) then
    lsbJournal.Items.SaveToFile(QFileName);
end;

procedure TdlgProcessing.CopierJournalErreurs;
begin
  memoErreurs.SelectAll;
  memoErreurs.CopyToClipboard;
end;

procedure TdlgProcessing.lsbJournalClick(Sender: TObject);
begin
  editLigneSelectionnee.Text := lsbJournal.Items[lsbJournal.ItemIndex];
end;


end.

