unit unitItineraires;

{$INCLUDE CompilationParameters.inc}
{$ERROR Inutilisé}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  BZGraphesTypes,
  UnitListesSimplesWithGeneriques,
  Classes, SysUtils, Graphics;

implementation

type

{ TItineraire }

 TItineraire = class
  private
    FProfilName   : string;
    FProfilColor  : TColor;
    FExtremite1   : TPoint2Df;
    FExtremite2   : TPoint2Df;
    FListePointsDePassage: TListeSimple<TIDStation>;

  public
    function Initialiser(const QSerDepart : TNumeroSerie; const QStDepart : TNumeroStation;
                         const QSerArrivee: TNumeroSerie; const QSArriveet: TNumeroStation;
                         const NomItineraire: string): boolean;
    procedure Finaliser();
end;

{ TItineraire }

function TItineraire.Initialiser(const QSerDepart: TNumeroSerie; const QStDepart: TNumeroStation; const QSerArrivee: TNumeroSerie; const QSArriveet: TNumeroStation; const NomItineraire: string): boolean;
begin
  result := false;
  FListePointsDePassage := TListeSimple<TIDStation>.Create;
  try

  except
  end;
end;

procedure TItineraire.Finaliser();
begin
  try
    FListePointsDePassage.ClearListe();
  finally
    FreeAndNil(FListePointsDePassage);
  end;
end;

end.
