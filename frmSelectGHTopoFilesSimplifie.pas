unit frmSelectGHTopoFilesSimplifie;
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, FileCtrl, EditBtn, StdCtrls;

type  TdlgSelectGHTopoFilesSimplifie = class(TForm)
    btnOK: TBitBtn;
    btnOK1: TBitBtn;
    editInitialDir: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    lsbFichiers: TFileListBox;
    Panel1: TPanel;
    procedure editInitialDirChange(Sender: TObject);
  private
    FFileName: TStringDirectoryFilename;
  public
    function Initialiser(): boolean;
    procedure Finaliser();
    function GetFileName(): TStringDirectoryFilename;
  end;

var
  dlgSelectGHTopoFilesSimplifie: TdlgSelectGHTopoFilesSimplifie;

implementation

{$R *.lfm}

{ TdlgSelectGHTopoFilesSimplifie }

procedure TdlgSelectGHTopoFilesSimplifie.editInitialDirChange(Sender: TObject);
begin
  lsbFichiers.Directory := editInitialDir.Directory;
end;

function TdlgSelectGHTopoFilesSimplifie.Initialiser(): boolean;
var
  EWE: TStringDirectoryFilename;
begin
  Result := false;
  //try
    //AfficherMessage('** 001');
    EWE := GetGHTopoDirectory() + MON_DOSSIER_RPI_DOCS;
    //AfficherMessage('** 002');
    ForceDirectories(EWE);
    //AfficherMessage('** 003');
    editInitialDir.Directory := EWE;
    //AfficherMessage('** 004');

    lsbFichiers.Directory := editInitialDir.Directory;
    //AfficherMessage('** 005');

    lsbFichiers.Mask := '*.xtb'; //GetResourceString(rsGHTOPO_FILE_FILTER_W_TEXT);

    Result := True;
  //except
  //  pass;
  //end;
end;

procedure TdlgSelectGHTopoFilesSimplifie.Finaliser();
begin
  pass;
end;

function TdlgSelectGHTopoFilesSimplifie.GetFileName(): TStringDirectoryFilename;
begin
  Result := lsbFichiers.FileName;
end;

end.

