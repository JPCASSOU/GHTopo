unit CadreSecteur;
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees, Common,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, StdCtrls, Dialogs;
type
  TCdrSecteur = class(TFrame)
    btnColor: TColorButton;
    editIdxSecteur: TCurrencyEdit;
    editNomSecteur: TEdit;
    lbCouleur: TLabel;
    lbNomSecteur: TLabel;
    lbIDSecteur: TLabel;
  private
   FIdx: integer;
   FSecteur : TSecteur;
   procedure InitCaptions;
   procedure PutSecteurInForm;
  public
    procedure SetSecteur(const S: TSecteur; const QIdx: integer; const DoInitCaptions: boolean);
    function  GetSecteurFromForm(): TSecteur;
  end;

implementation

{$R *.lfm}
procedure TCdrSecteur.InitCaptions;
begin
  lbIDSecteur.Caption   := GetResourceString(rsCDR_SECTEUR_LBIDX);
  lbNomSecteur.Caption  := GetResourceString(rsCDR_SECTEUR_NAME);
  lbCouleur.Caption     := GetResourceString(rsCDR_SECTEUR_COLOUR);
end;

procedure TCdrSecteur.PutSecteurInForm;
begin
  editIdxSecteur.AsInteger := FIdx;
  editNomSecteur.Text      := _AnsiToLCLStr(FSecteur.NomSecteur);
  btnColor.ButtonColor     := FSecteur.CouleurSecteur;
end;

procedure TCdrSecteur.SetSecteur(const S: TSecteur; const QIdx: integer; const DoInitCaptions: boolean);
begin
  FIdx := QIdx;
  FSecteur := S;
  if (DoInitCaptions) then InitCaptions;
  PutSecteurInForm;
end;

function TCdrSecteur.GetSecteurFromForm: TSecteur;
begin
  Result.NomSecteur     := _LCLStrToAnsi(Trim(editNomSecteur.Text));
  Result.CouleurSecteur := btnColor.ButtonColor;
end;
end.
