unit dlgEditerPOI;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, curredit;

type

{ TfrmEditerPOI }

 TfrmEditerPOI = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cmbPOIStatut: TComboBox;
    editPOIIdTerrain: TEdit;
    editPOISerie: TCurrencyEdit;
    editPOIDescription: TEdit;
    editPOIPoint: TCurrencyEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    FDocTopo: TToporobotStructure2012;
    FPOI    : TPointOfInterest;
  public
    function Initialiser(const FD: TToporobotStructure2012; const POI: TPointOfInterest): boolean;
    function GetPOI(): TPointOfInterest;
  end;

var
  frmEditerPOI: TfrmEditerPOI;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TfrmEditerPOI }

procedure TfrmEditerPOI.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

function TfrmEditerPOI.Initialiser(const FD: TToporobotStructure2012; const POI: TPointOfInterest): boolean;
begin
  result := false;
  self.Caption := 'Points d''intérêt';
  try
    FDocTopo := FD;
    FPOI     := POI;

    editPOISerie.AsInteger     := FPOI.Serie;
    editPOIPoint.AsInteger     := FPOI.Station;
    editPOIIdTerrain.Text      := FPOI.LabelTerrain;
    cmbPOIStatut.Clear;
    cmbPOIStatut.Items.Add('Inconnu');
    cmbPOIStatut.Items.Add('A faire');
    cmbPOIStatut.Items.Add('Traité');
    cmbPOIStatut.ItemIndex     := Ord(FPOI.Statut);

    editPOIDescription.Text    := FPOI.Description;
    Result := True;
  except

  end;
end;

function TfrmEditerPOI.GetPOI(): TPointOfInterest;
begin
  Result := FPOI;

  Result.Serie                 := editPOISerie.AsInteger;
  Result.Station               := editPOIPoint.AsInteger;
  Result.LabelTerrain          := Trim(editPOIIdTerrain.Text);

  Result.Statut                := TPOIStatut(cmbPOIStatut.ItemIndex);
  Result.Description           := Trim(editPOIDescription.Text);
end;

end.
(*
Le travail au noir
- Exploite le travailleur
* Temps de travail non limité
* Salaire à discrétion de l'employeur(quand il est versé)
* Soumission à l'employeur
* Destruction des droits sociaux
* L'argent liquide est de plus en plus inutilisable:
--- Est de plus en plus refusé par les commerçants
--- Les paiements en espèces auprès des professionnels ne peuvent pas excéder 1000 euros, et ce plafond est appelé à diminuer
--- Il est en fait très difficile d'écouler des sommes du niveau d'un salaire (1500 euros), surtout pour un célibataire,
    et il y a des loyers, des impôts, des charges, des assurances à régler, dont les seules méthodes de paiement sont scripturales.
    Le travailleur au noir a pour seule option de faire transiter son salaire par dépôt sur compte, or l'intégrale des dépôts
    d'espèces (sur TOUS les comptes bancaires) dépassant 10.000 euros sur un mois fait lever le drapeau rouge par le banquier
    avec saisine de TRACFIN.
--- Cf RETEX du confinement, durant lequel la majorité des DAB étaient indisponibles

- Désactive les assurances
* Pas d'assurance accident +++
* Pas d'assurance maladie ++++
* Aucune assurance ne fonctionne +++++
(rappel: En cadre légal, les assurances couvrant la vie d'un ouvrage sont:
  - la garantie dommages-ouvrage
  - la garantie décennale
  - la multirisque habitation + responsabilité civile)

- Détruit la santé du travailleur
* Par non fourniture d'équipements de protection (EPI)
* Par méconnaissance de la toxicité des produits utilisés
* Par incompétence professionnelle

- Isole le travailleur de son entourage +++
* Par réduction des plages de liberté +++ => les amis se détournent du travailleur, moins de vie de famille
* Par risque d'accident surmultiplié
* Par altération de la réputation

- Produit un travail de mauvaise qualité ++++
* Par incompétence ++++
* Par manque de qualifications +++
* Par utilisation de produits dangereux et/ou de mauvaise qualité ++

- Met en danger le donneur d'ordre ++++
* Par le caractère délictuel du travail illégal +++
* Par la désactivation des assurances = risque de surendettement transmissible aux héritiers, sans possibilité d'adition [renonciation à la succession] ++++
* Par les malfaçons, quasi systématiques en travail illégal ++
* Par les surcoûts à long terme (travaux à refaire) +++

- Est truffé de pièges à court, moyen et long terme
* Risque de dénonciations
* Risque de non-paiement des salaires (notion de rémunération négative) +++
* Aucune couverture par les assurances, même 30 ans après les travaux ++++
* Redressements fiscaux et sociaux avec une probabilité de 100% +++
* Les travaux ne sont pas valorisables en cas de revente du bien +++
* Des propositions de loi sont régulièrement déposées par les députés, dont la plus critique est l'obligation d'expertise pour les travaux de structure,
  électricité et plomberie. Expertise payante (plusieurs milliers d'euros) pouvant conduire à la déconstruction de l'ouvrage +++++

- Constitue ni plus ni moins un crime contre l'Etat
* Par soustraction de cotisations
* Par concurrence déloyale
* Par blanchiment d'argent
//*)

//*)
