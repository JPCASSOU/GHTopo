unit dlgMaillages;

// Utilitaires de maillages

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  UnitEntitesExtended,
  UnitClasseMaillage, CallDialogsStdVersion, Classes, SysUtils,
  FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmMaillagesMNT }

  TfrmMaillagesMNT = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FMyMaillage: TMaillage;

  public
    { public declarations }
    procedure FinaliserDlgMaillage;
    function InitialiserDlgMaillage(const BE: TBDDEntites): boolean;

  end;

var
  frmMaillagesMNT: TfrmMaillagesMNT;

implementation

{$R *.lfm}

{ TfrmMaillagesMNT }

procedure TfrmMaillagesMNT.Button1Click(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  BMP: TBitmap;
begin
  if (DoDialogOpenFile('Fichier maillage VisualTopo (*.sur)|*.sur|Tous|*.*', '.sur', QFileName)) then
  begin
    FMyMaillage.ChargerMaillageDepuisSUR(QFileName);
    BMP := FMyMaillage.MakeImageFromGRD(True);
    PaintBox1.Canvas.StretchDraw(Rect(0,0,PaintBox1.Width, PaintBox1.Height), BMP);
  end;
end;

procedure TfrmMaillagesMNT.Button2Click(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  BMP: TBitmap;
begin
  if (DoDialogOpenFile('Fichier maillage SRTM (*.HGT)|*.hgt|Tous|*.*', '.hgt', QFileName)) then
  begin
    FMyMaillage.ChargerMaillageDepuisHGT(QFileName);
    BMP := FMyMaillage.MakeImageFromGRD(false);
    PaintBox1.Canvas.StretchDraw(Rect(0,0,PaintBox1.Width, PaintBox1.Height), BMP);
  end;
end;

procedure TfrmMaillagesMNT.Button3Click(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  if (DoDialogSaveFile('Fichier maillage Visual Topo (*.sur)|*.sur|Tous|*.*', '.sur', QFileName, QFilterIndex)) then
  begin
    FMyMaillage.ExportToSUR(QFileName);
  end;
end;

procedure TfrmMaillagesMNT.FormCreate(Sender: TObject);
begin

end;

function TfrmMaillagesMNT.InitialiserDlgMaillage(const BE: TBDDEntites): boolean;
begin
  Result := False;
  FMyMaillage := TMaillage.Create(Application);
  try
    Result := FMyMaillage.Initialiser(BE);
  except
  end;
end;

procedure TfrmMaillagesMNT.FinaliserDlgMaillage;
begin
  try
    FMyMaillage.Finaliser;
  finally
    //FMyMaillage.Free;
  end;

end;

end.

