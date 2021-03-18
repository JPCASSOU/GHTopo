unit frmEditSecteur;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons
  , StructuresDonnees
  , ToporobotClasses2012, CadreSecteur
  ;

type

  { TdlgEditSecteur }

  TdlgEditSecteur = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CdrSecteur1: TCdrSecteur;
    dlgEditSecteur: TPanel;
  private
    { private declarations }
  public
    { public declarations }
    function PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QSecteur: TSecteur; const QCaption: string): boolean;
    function GetASecteur(): TSecteur;
  end;

var
  dlgEditSecteur: TdlgEditSecteur;

implementation

{$R *.lfm}

{ TdlgEditSecteur }

function TdlgEditSecteur.PrepareDialog(const FD: TToporobotStructure2012; const QIdx: integer; const QSecteur: TSecteur; const QCaption: string): boolean;
begin
  Result := false;
  self.Caption := QCaption;
  try
    CdrSecteur1.SetSecteur(QSecteur, QIdx, True);
    Result := True;
  except
    ;
  end;
end;

function TdlgEditSecteur.GetASecteur: TSecteur;
begin
  Result := CdrSecteur1.GetSecteurFromForm;
end;

end.

