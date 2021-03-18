unit CadreParamsOngletsVue2D;
{$ERROR Unité Inutilisé}
{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  StructuresDonnees,
  unitUtilsComposants,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Spin, Dialogs;

type

  { TcdrParamsOngletVue2D }

  TcdrParamsOngletVue2D = class(TFrame)
    btnBGColor: TColorButton;
    btnDegradeStart: TColorButton;
    btnDegradeStop: TColorButton;
    btnQdrColor: TColorButton;
    chkJonctions: TCheckBox;
    chkAltitudes: TCheckBox;
    chkAntennes: TCheckBox;
    chkCotes: TCheckBox;
    chkEntrances: TCheckBox;
    chkAnnotations: TCheckBox;
    chkIDStations: TCheckBox;
    chkParois: TCheckBox;
    chkPolygonales: TCheckBox;
    chkQuadrillage: TCheckBox;
    chkRemplissage: TCheckBox;
    chkSections: TCheckBox;
    chkStations: TCheckBox;
    cmbRepresentation: TComboBox;
    editLargeurTraitPolygonale: TSpinEdit;
    editQdrSpc: TComboBox;
    grbxDegradeAltitudes: TGroupBox;
    grbxElementsAffiches: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lbCoteMaxi: TLabel;
    lbCoteMini: TLabel;
    lbRepresentationMode: TLabel;
    lbX1Y1: TStaticText;
    lbX2Y2: TStaticText;
    procedure chkAntennesChange(Sender: TObject);
    procedure chkJonctionsChange(Sender: TObject);
  private
    FCurrParams2D: TVue2DParams;
    function GetChksElementsDrawn: TElementsDrawn;

    procedure SetChksElementsDrawn(const E: TElementsDrawn);

    { private declarations }
  public
    { public declarations }
    procedure SetValuesOnglet2D(const O: TVue2DParams);
    function GetValuesOnglet2D: TVue2DParams;
  end;

implementation

{$R *.lfm}

procedure TcdrParamsOngletVue2D.SetValuesOnglet2D(const O: TVue2DParams);
begin
  // sauvegarde paramètres
  FCurrParams2D := O;

  // éléments à dessiner
  SetChksElementsDrawn(O.ongElementsDrawn);

  // autres valeurs
  editQdrSpc.Text   := Format(FORMAT_NB_REAL_0_DEC,[O.ongQdrSpc]);
  btnBGColor.Color  := O.ongBackGround;
  btnQdrColor.Color := O.ongQdrColor;
  // TODO: Type quadrillage
  //cmbTypeQuadrillage.ItemIndex := Ord(O.ongQdrType);

  editLargeurTraitPolygonale.Value := O.ongViseesLargeur;
  // sauvegarde temp des coordonnées de la zone d'affichage (non modifiables)
  //FC1.X := O.ongX1;  FC1.Y := O.ongY1;
  //FC2.X := O.ongX2;  FC2.Y := O.ongY2;

  lbX1Y1.Caption := Format('Du coin: %.0f - %.0f', [O.ongX1, O.ongY1]);
  lbX2Y2.Caption := Format('Au coin: %.0f - %.0f', [O.ongX2, O.ongY2]);
  // dégradé
  btnDegradeStart.ButtonColor := O.ongDegradeStart;
  btnDegradeStop.ButtonColor  := O.ongDegradeStop;
  lbRepresentationMode.Caption := GetResourceString(rsVUE2D_REPRESENTATION_MODE);
  InitialiserComboBoxRepresentationVisees(cmbRepresentation, O.ongModeRepresentation);
end;
procedure TcdrParamsOngletVue2D.SetChksElementsDrawn(const E: TElementsDrawn);
begin
  chkPolygonales.Checked := (edPolygonals     in E);
  chkStations.Checked    := (edStations       in E);
  chkAltitudes.Checked   := (edAltitudes      in E);
  chkCotes.Checked       := (edCotes          in E);
  chkIDStations.Checked  := (edIDStations     in E);
  chkParois.Checked      := (edWalls          in E);
  chkRemplissage.Checked := (edFillGalerie    in E);
  chkSections.Checked    := (edCrossSections  in E);
  chkQuadrillage.Checked := (edQuadrilles     in E);
  chkEntrances.Checked   := (edENTRANCES      in E);
  chkAntennes.Checked    := (edANTENNES       in E);
  chkAntennes.Checked    := (edANNOTATIONS    in E);
  chkJonctions.Checked   := (edJONCTIONS      in E);
end;

procedure TcdrParamsOngletVue2D.chkAntennesChange(Sender: TObject);
begin

end;

procedure TcdrParamsOngletVue2D.chkJonctionsChange(Sender: TObject);
begin

end;

function TcdrParamsOngletVue2D.GetChksElementsDrawn(): TElementsDrawn;
begin
  Result := [];
  if (chkEntrances.Checked)   then Result := Result + [edENTRANCES]     else Result := Result - [edENTRANCES];
  if (chkStations.Checked)    then Result := Result + [edStations]      else Result := Result - [edStations];
  if (chkAltitudes.Checked)   then Result := Result + [edAltitudes]     else Result := Result - [edAltitudes];
  if (chkCotes.Checked)       then Result := Result + [edCotes]         else Result := Result - [edCotes];
  if (chkParois.Checked)      then Result := Result + [edWalls]         else Result := Result - [edWalls];
  if (chkIDStations.Checked)  then Result := Result + [edIDStations]    else Result := Result - [edIDStations];
  if (chkSections.Checked)    then Result := Result + [edCrossSections] else Result := Result - [edCrossSections];
  if (chkRemplissage.Checked) then Result := Result + [edFillGalerie]   else Result := Result - [edFillGalerie];
  if (chkQuadrillage.Checked) then Result := Result + [edQuadrilles]    else Result := Result - [edQuadrilles];
  if (chkPolygonales.Checked) then Result := Result dqsfs+ [edPolygonals]    else Result := Result - [edPolygonals];
  if (chkAntennes.Checked)    then Result := Result + [edANTENNES]      else Result := Result - [edANTENNES];
  if (chkAnnotations.Checked) then Result := Result + [edANNOTATIONS]   else Result := Result - [edANNOTATIONS];
  if (chkJonctions.Checked)   then Result := Result + [edJONCTIONS]     else Result := Result - [edJONCTIONS];
end;
function TcdrParamsOngletVue2D.GetValuesOnglet2D(): TVue2DParams;
begin
  // récup des params non modifiés ou masqués
  Result := FCurrParams2D;
  with Result do
  begin
    ongBackGround      := btnBGColor.ButtonColor;    // couleur de fond
    ongElementsDrawn   := GetChksElementsDrawn();    // éléments à dessiner
    ongQdrColor        := btnQdrColor.ButtonColor;   // quadrillage
    ongQdrType         := qtGRID;
    ongQdrSpc          := ConvertirEnNombreReel(Trim(editQdrSpc.Text), 10.00);
    if (ongQdrSpc < 10.00) then ongQdrSpc := 10.00; // valeur minimale de l'espacement
    // récupération des coordonnées de zone non modifiables
    //ongX1 := FC1.X;   ongY1 := FC1.Y;
    //ongX2 := FC2.X;   ongY2 := FC2.Y;
    // dégradé
    ongDegradeStart := btnDegradeStart.ButtonColor;
    ongDegradeStop  := btnDegradeStop.ButtonColor;
    // largeur de trait
    ongViseesLargeur := editLargeurTraitPolygonale.Value;
    // mode de représentation
    ongModeRepresentation := TModeRepresentationGaleries(cmbRepresentation.ItemIndex);
  end;
end;

end.

