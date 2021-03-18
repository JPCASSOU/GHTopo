unit CadreFiltres;
// Saisie du MétaFiltre
// Date: 26/06/2012
// Statut: Nouveauté - Développement en cours
// 07/04/2017: Module abandonné: s'est avéré complexe et peu utile.
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  CallDialogsStdVersion,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type TProcWithoutParams = procedure(const B: boolean) of object;
type

  { TCdrMetaFiltre }

  TCdrMetaFiltre = class(TFrame)
    btnHelpFiltres: TButton;
    btnApply: TButton;
    chkActif: TCheckBox;
    editFiltres: TEdit;
    lbFiltres: TLabel;
    Panel1: TPanel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnHelpFiltresClick(Sender: TObject);
    procedure chkActifChange(Sender: TObject);
    procedure editFiltresKeyPress(Sender: TObject; var Key: char);
  private
    FProcApplyMetaFiltre : TProcWithoutParams;
  public
    { public declarations }
    procedure SetProcApplyMetaFiltre(const FP: TProcWithoutParams);
    procedure ActiverMetaFiltre(const B: boolean);
    function  IsMetafiltreActif(): boolean;
    procedure SetFiltre(const MyFiltre: string);
    function  GetFiltre(): string;

  end;

implementation

{$R *.lfm}

{ TCdrMetaFiltre }

procedure TCdrMetaFiltre.btnHelpFiltresClick(Sender: TObject);
begin
  DisplayHelpSystem('METAFILTRE');
end;

procedure TCdrMetaFiltre.chkActifChange(Sender: TObject);
begin
   FProcApplyMetaFiltre(chkActif.Checked);
end;

procedure TCdrMetaFiltre.editFiltresKeyPress(Sender: TObject; var Key: char);
begin
  try
    if (Key = #13) then FProcApplyMetaFiltre(chkActif.Checked);
  except
  end;
end;

procedure TCdrMetaFiltre.ActiverMetaFiltre(const B: boolean);
begin
  chkActif.Checked := True;
end;

procedure TCdrMetaFiltre.btnApplyClick(Sender: TObject);
begin
  try
    FProcApplyMetaFiltre(chkActif.Checked);
  except
    ;
  end;
end;

procedure TCdrMetaFiltre.SetProcApplyMetaFiltre(const FP: TProcWithoutParams);
begin
  FProcApplyMetaFiltre := FP;
end;

procedure TCdrMetaFiltre.SetFiltre(const MyFiltre: string);
begin
  lbFiltres.Caption  := GetResourceString(rsCDR_METAFILTRE_PROMPT);
  chkActif.Caption   := GetResourceString(rsCDR_METAFILTRE_CHK_ACTIF);
  editFiltres.Text := MyFiltre;
end;

function TCdrMetaFiltre.GetFiltre: string;
begin
  Result := Trim(editFiltres.Text);
end;

function TCdrMetaFiltre.IsMetafiltreActif: boolean;
begin
  Result := chkActif.Checked;
end;


end.

