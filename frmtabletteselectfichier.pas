unit frmTabletteSelectFichier;
// Sélecteur de fichiers pour la version Tablette de GHTopo

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  Buttons, FileCtrl, EditBtn, StdCtrls, ComCtrls, PairSplitter;

type

  { TdlgTabletteSelectFichier }

  TdlgTabletteSelectFichier = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cmbFileFilters: TFilterComboBox;
    editFileName: TEdit;
    ImageList1: TImageList;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    shlvFichiers: TShellListView;
    shtwDossiers: TShellTreeView;

    procedure FormShow(Sender: TObject);
    procedure shlvFichiersChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    { private declarations }
    FMode : integer;
  public
    { public declarations }
    function Initialiser(const QMode: byte; const QInitialDir, QFilter, QDefaultExt: string; var QFileName: string): boolean;
    function GetFileName: string;
  end;

var
  dlgTabletteSelectFichier: TdlgTabletteSelectFichier;

implementation
uses
  CallDialogsStdVersion;

{$R *.lfm}

{ TdlgTabletteSelectFichier }

procedure TdlgTabletteSelectFichier.FormShow(Sender: TObject);
begin
  self.Position := poScreenCenter;
end;




procedure TdlgTabletteSelectFichier.shlvFichiersChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin

  editFileName.Text := shlvFichiers.Root + Item.Caption;
end;

function TdlgTabletteSelectFichier.Initialiser(const QMode: Byte;
                                               const QInitialDir, QFilter, QDefaultExt: string;
                                               var QFileName: string): boolean;
begin

  Result := False;
  FMode  := QMode;
  case qMode of
    1: self.Caption := 'Ouvrir';
    2: self.Caption := 'Sauvegarder sous';
  end;
  // filtres
  cmbFileFilters.Filter := QFilter;
  cmbFileFilters.ItemIndex := 0;
  // dossier racine
  shtwDossiers.Root := QInitialDir;
  // liste
end;

function TdlgTabletteSelectFichier.GetFileName: string;
begin
  Result := Trim(editFileName.Text);
end;

end.

