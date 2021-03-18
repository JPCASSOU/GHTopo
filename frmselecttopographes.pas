unit frmSelectTopographes;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils,
  CallDialogsStdVersion,
  Common,
  FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

type

  { TdlgSelectTopographes }

  TdlgSelectTopographes = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
		btnAddTopographe: TButton;
		btnSupprimerTopographe: TButton;
    lsbTopographes: TListBox;
		lbLocalisationFichierTopographes: TStaticText;
    procedure BitBtn1Click(Sender: TObject);
		procedure btnAddTopographeClick(Sender: TObject);
		procedure btnSupprimerTopographeClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    procedure Initialise();
    function  GetTopographe(): string;
  end;

var
  dlgSelectTopographes: TdlgSelectTopographes;

implementation

{$R *.lfm}

{ TdlgSelectTopographes }
const FICHIER_TOPOGRAPHES = 'Topographes.txt';

procedure TdlgSelectTopographes.BitBtn1Click(Sender: TObject);
begin

end;

procedure TdlgSelectTopographes.btnAddTopographeClick(Sender: TObject);
var
	EWE, MyTopographe: String;
begin
  MyTopographe :='';
  if (InputQuery('Ajout d''un topographe', 'Nom', MyTopographe)) then
  begin
    EWE := GetGHTopoDirectory() + FICHIER_TOPOGRAPHES;
    lsbTopographes.Items.Add(Trim(MyTopographe));
    lsbTopographes.Items.SaveToFile(EWE);
	end;
end;

procedure TdlgSelectTopographes.btnSupprimerTopographeClick(Sender: TObject);
var
  EWE: String;
begin
  if (GHTopoQuestionOuiNon('Supprimer') and (lsbTopographes.ItemIndex >= 0)) then
  begin
    EWE := GetGHTopoDirectory() + FICHIER_TOPOGRAPHES;
    lsbTopographes.Items.Delete(lsbTopographes.ItemIndex);
    lsbTopographes.Items.SaveToFile(EWE);
    lsbTopographes.ItemIndex := 0;
	end;
end;

function TdlgSelectTopographes.GetTopographe: string;
var
  n, i: Integer;
begin
  Result := '';
  n := lsbTopographes.Count;
  if (n = 0) then exit('');
  for i := 0 to n - 1 do
  begin
    if (lsbTopographes.Selected[i]) then Result += Trim(lsbTopographes.Items[i]) + '; ';
  end;
  // et on supprime le ; de fin
  n := Length(Result);
  System.Delete(Result, n - 1, 1);
end;

procedure TdlgSelectTopographes.Initialise;
var
  EWE: String;
begin
  EWE := GetGHTopoDirectory() + FICHIER_TOPOGRAPHES;
  lbLocalisationFichierTopographes.Caption := FICHIER_TOPOGRAPHES;
  lsbTopographes.Clear;
  lsbTopographes.Items.Add('Gros Minet');
  try
    if (FileExists(EWE)) then lsbTopographes.Items.LoadFromFile(EWE);
  except
  end;
  lsbTopographes.ItemIndex := 0;
end;

end.

