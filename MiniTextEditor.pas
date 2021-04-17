unit MiniTextEditor;
// Petit éditeur de texte incorporé
// 24/08/2012
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  CallDialogsStdVersion, Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, StdCtrls;

type

  { TdlgEditeur }

  TdlgEditeur = class(TForm)
    acSaveAs: TAction;
    acQuit: TAction;
    acCopyAllText: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuEdition: TMenuItem;
    MenuItem5: TMenuItem;
    mnuFile: TMenuItem;
    EditorText: TSynEdit;
    procedure acCopyAllTextExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    function InitialiseEditor(const FileTexte: string; const EditMode: Boolean): boolean; overload;
    function InitialiseEditor(const QText: string): boolean; overload;
  end;

var
  dlgEditeur: TdlgEditeur;

implementation

{$R *.lfm}

{ TdlgEditeur }

procedure TdlgEditeur.FormShow(Sender: TObject);
  procedure SetAcHint(const ACDC: TAction; const QCaption: string);
  begin
    ACDC.Caption := GetResourceString(QCaption);
    ACDC.Hint    := GetResourceString(QCaption);
  end;
begin
  mnuFile.Caption  := GetResourceString(rsMNU_FILE);
    SetAcHint(acSaveAs      , rsSAVEAS);
    SetAcHint(acQuit        , rsQUIT_EDITOR);
  mnuEdition.Caption  := GetResourceString(rsMNU_EDITION);
    SetAcHint(acCopyAllText , rsCOPY_ALL_TEXT_TO_CLIPBOARD);
end;


procedure TdlgEditeur.acQuitExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TdlgEditeur.acCopyAllTextExecute(Sender: TObject);
begin
  EditorText.CopyToClipboard;
end;





procedure TdlgEditeur.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  CanClose := GHTopoQuestionOuiNon(EnlevePerluete(rsQUIT_EDITOR));
end;

function TdlgEditeur.InitialiseEditor(const FileTexte: string; const EditMode: Boolean): boolean;
begin
  Result := False;
  try
    if (FileTexte = '') then
      EditorText.Lines.Clear
    else
      EditorText.Lines.LoadFromFile(FileTexte);
    Result := True;
  finally
  end;
end;

function TdlgEditeur.InitialiseEditor(const QText: string): boolean;
begin
  Result := False;
  try
    EditorText.Lines.Clear;
    EditorText.Text := QText;
    Result := True;
  finally
  end;
end;

end.

