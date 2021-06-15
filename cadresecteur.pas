unit CadreSecteur;
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees, Common,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, StdCtrls, Dialogs;
type

  { TCdrSecteur }

  TCdrSecteur = class(TFrame)
    btnColor: TColorButton;
    editIdxSecteur: TCurrencyEdit;
    editNomSecteur: TEdit;
    lbCouleur: TLabel;
    lbNomSecteur: TLabel;
    lbIDSecteur: TLabel;
  private
   procedure InitCaptions();
  public
    function Initialiser(const MySecteur: TSecteur; const QIdx: integer): boolean;
    function  GetSecteurFromForm(): TSecteur;
  end;

implementation

{$R *.lfm}
function TCdrSecteur.Initialiser(const MySecteur: TSecteur; const QIdx: integer): boolean;
begin
  result := false;
  InitCaptions();
  editIdxSecteur.AsInteger := QIdx;
  editNomSecteur.Text      := _AnsiToLCLStr(MySecteur.NomSecteur);
  btnColor.ButtonColor     := MySecteur.CouleurSecteur;
  result := true;
end;
procedure TCdrSecteur.InitCaptions;
begin
  lbIDSecteur.Caption   := GetResourceString(rsCDR_SECTEUR_LBIDX);
  lbNomSecteur.Caption  := GetResourceString(rsCDR_SECTEUR_NAME);
  lbCouleur.Caption     := GetResourceString(rsCDR_SECTEUR_COLOUR);
end;




function TCdrSecteur.GetSecteurFromForm(): TSecteur;
begin
  Result.NomSecteur     := _LCLStrToAnsi(Trim(editNomSecteur.Text));
  Result.CouleurSecteur := btnColor.ButtonColor;
end;
end.
