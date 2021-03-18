unit dlgFusionnerFichiersXTB;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  StdCtrls, EditBtn, FileCtrl;

type

  { TdlgFusionFichiers }

  TdlgFusionFichiers = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    DirectoryEdit1: TDirectoryEdit;
    FileListBox1: TFileListBox;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dlgFusionFichiers: TdlgFusionFichiers;

implementation

{$R *.lfm}

end.

