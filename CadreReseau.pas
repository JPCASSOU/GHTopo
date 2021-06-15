unit CadreReseau;
// Date: 26/04/2012
// Statut: OK
// 04/10/2013: Conversions UTF8 <> ANSI fixed
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common, StructuresDonnees,
  CallDialogsStdVersion,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, curredit;

type

  { TCdrReseaux }

  TCdrReseaux = class(TFrame)
    cmbTypeReseau: TComboBox;
    editNomReseau: TEdit;
    editObsReseau: TEdit;
    editIdxReseau: TCurrencyEdit;
    lbObsReseau: TLabel;
    lbNomReseau: TLabel;
    lbColorReseau: TLabel;
    lbIdxReseau: TLabel;
    lbTypeReseau: TLabel;
    btnColorReseau: TStaticText;
    procedure btnColorReseauClick(Sender: TObject);
  private
    { private declarations }
    { Déclarations privées }
    procedure InitCaptions();
  public
    { public declarations }
    function  Initialiser(const MyReseau: TReseau; const QIdx: integer): boolean;
    function  GetReseauFromForm(): TReseau;
  end;

implementation

{$R *.lfm}
function TCdrReseaux.Initialiser(const MyReseau: TReseau; const QIdx: integer): boolean;
begin
  result := false;
  InitCaptions();
  editIdxReseau.AsInteger := QIdx;
  btnColorReseau.Color    := MyReseau.ColorReseau;
  editNomReseau.Text      := _AnsiToLCLStr(MyReseau.NomReseau);
  editObsReseau.Text      := _AnsiToLCLStr(MyReseau.ObsReseau);
  cmbTypeReseau.ItemIndex := MyReseau.TypeReseau;
  result := true;
end;

procedure TCdrReseaux.InitCaptions();
begin
  lbIdxReseau.Caption    := GetResourceString(rsCDR_RESEAU_LBIDX); ;
  lbNomReseau.Caption    := GetResourceString(rsCDR_RESEAU_NAME);
  lbObsReseau.Caption    := GetResourceString(rsSELECT_LISTE_OBSERV);
  lbColorReseau.Caption  := GetResourceString(rsCOLOR);
  lbTypeReseau.Caption   := GetResourceString(rsCDR_RESEAU_TYPE);
  with cmbTypeReseau do
  begin
    Clear;
    Items.Add(GetResourceString(rsCDR_RESEAU_CB0));
    Items.Add(GetResourceString(rsCDR_RESEAU_CB1));
    Items.Add(GetResourceString(rsCDR_RESEAU_CB2));
    Items.Add(GetResourceString(rsCDR_RESEAU_CB3));
    Items.Add(GetResourceString(rsCDR_RESEAU_CB4));
    Items.Add(GetResourceString(rsCDR_RESEAU_CB5));
    Items.Add(GetResourceString(rsCDR_RESEAU_CB6));
    ItemIndex := 0;
  end;
end;

procedure TCdrReseaux.btnColorReseauClick(Sender: TObject);
begin
  btnColorReseau.Color := ChooseColor(btnColorReseau.Color);
end;

function  TCdrReseaux.GetReseauFromForm(): TReseau;
begin
  //IdxReseau     := editIdxReseau.AsInteger;
  Result.ColorReseau   := btnColorReseau.Color;
  Result.NomReseau     := _LCLStrToAnsi(Trim(editNomReseau.Text));
  Result.ObsReseau     := _LCLStrToAnsi(Trim(editObsReseau.Text));
  Result.TypeReseau    := cmbTypeReseau.ItemIndex;
end;
end.
