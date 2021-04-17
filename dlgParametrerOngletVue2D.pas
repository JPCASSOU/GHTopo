unit dlgParametrerOngletVue2D;
// Date: 28/08/2012
// Dialogue pour paramétrage des onglets de la vue 2D
// 20/11/2014: Support des largeurs de trait pour les polygonales (lié aux visées en antenne)
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  CallDialogsStdVersion, unitUtilsComposants,
  Common,
  StructuresDonnees, Classes, SysUtils, FileUtil, curredit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, Spin, ExtCtrls, ColorBox, ComCtrls;

type

  { TfrmParametrerOngletVue2D }

  TfrmParametrerOngletVue2D = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnBGColor: TColorButton;
    btnCouleurTexteEntrees: TColorButton;
    btnCouleurTextePOIs: TColorButton;
    btnCouleurTexteNoeuds: TColorButton;
    btnCouleurCotation3: TColorButton;
    btnDegradeZMaxiReseau: TColorButton;
    btnDegradeZMiniReseau: TColorButton;
    btnQdrColor: TColorButton;
    btnCubeColor: TColorButton;
    btnDegradeZMaxiMNT: TColorButton;
    btnDegradeZMiniMNT: TColorButton;
    btnReferentielColor: TColorButton;
    chkDegradeAltitudesMNT: TCheckBox;
    chkQuadrillage: TCheckBox;
    chkFastDrawCenterline: TCheckBox;
    chkDoDrawViseesNonRetenues: TCheckBox;
    chkEntranceNames: TCheckBox;
    chkPOI: TCheckBox;
    chkCubeBounds: TCheckBox;
    chkNoeuds: TCheckBox;
    chkAntennes: TCheckBox;
    chkCroquis: TCheckBox;
    chkEntrances: TCheckBox;
    chkCotes: TCheckBox;
    chkAltitudes: TCheckBox;
    chkIDStations: TCheckBox;
    chkParois: TCheckBox;
    chkPolygonales: TCheckBox;
    chkRemplissage: TCheckBox;
    chkSections: TCheckBox;
    chkStations: TCheckBox;
    chkVolumesVisibles: TCheckBox;
    btnCouleurTexteAltitudes: TColorButton;
    btnCouleurTexteCotation: TColorButton;
    btnCouleurTexteIDStations: TColorButton;
    btnColorViseesNonRetenues: TColorButton;
    cmbModeDessinMaillage: TComboBox;
    cmbRepresentationGaleries: TComboBox;
    cmbTypeQuadrillage: TComboBox;
    editQdrSpc: TComboBox;
    editFactZ: TCurrencyEdit;
    editTailleTexteCotation: TCurrencyEdit;
    editTailleTexteIDStations: TCurrencyEdit;
    editTailleTexteAltitudes: TCurrencyEdit;
    editTailleTexteEntrees: TCurrencyEdit;
    editLargeurTraitCenterlineInMM: TCurrencyEdit;
    editTailleTexteNoeuds: TCurrencyEdit;
    editTailleTextePOIs: TCurrencyEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbCoteMaxi: TLabel;
    lbCoteMaxi1: TLabel;
    lbCoteMini: TLabel;
    lbCoteMini1: TLabel;
    lbRepresentationMode: TLabel;
    lbX1Y1: TStaticText;
    lbX2Y2: TStaticText;
    PageControl1: TPageControl;
    editLargeurTraitCenterlineInPX: TSpinEdit;
    sclFillOpacity: TTrackBar;
    tabShtGeneral: TTabSheet;
    tabShtQuadrillage: TTabSheet;
    tabShtGaleries: TTabSheet;
    tabShtMNT: TTabSheet;
    trkbMaillageOpacity: TTrackBar;
    procedure chkEntrancesChange(Sender: TObject);
    procedure cmbRepresentationGaleriesChange(Sender: TObject);
  private
    FC1, FC2: TPoint2Df; // sauvegarde des coordonnées de zone

    { private declarations }
    FModeParametrage: TModeDialogParamVues;
    FCurrParams3D: TVue3DParams;
    FCurrParams2D: TVue2DParams;
    FCurrCoupeDeveloppeeParams: TCoupeDeveloppeeParams;
    procedure SetChksElementsDrawn(const E: TSetElementsDrawn);
    function  GetChksElementsDrawn: TSetElementsDrawn;
    procedure SetPanelParamVisible;

  public
    { public declarations }
    function  Initialiser(): boolean;
    procedure SetModeParametrageDialog(const Q: TModeDialogParamVues);
    procedure SetValuesOnglet2D(const O: TVue2DParams);
    procedure SetValuesOnglet3D(const O: TVue3DParams);
    procedure SetValuesCoupeDeveloppee(const O: TCoupeDeveloppeeParams);


    function  GetValuesOnglet2D(): TVue2DParams;
    function  GetValuesOnglet3D(): TVue3DParams;
    function  GetValuesCoupeDeveloppee(): TCoupeDeveloppeeParams;
  end;

var
  frmParametrerOngletVue2D: TfrmParametrerOngletVue2D;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}





procedure TfrmParametrerOngletVue2D.chkEntrancesChange(Sender: TObject);
begin
  chkEntranceNames.Enabled := chkEntrances.Checked;
end;

procedure TfrmParametrerOngletVue2D.cmbRepresentationGaleriesChange(Sender: TObject);
begin

end;





procedure TfrmParametrerOngletVue2D.SetChksElementsDrawn(const E: TSetElementsDrawn);
begin
  chkPolygonales.Checked      := (edPolygonals     in E);
  chkStations.Checked         := (edStations       in E);
  chkAltitudes.Checked        := (edAltitudes      in E);
  chkCotes.Checked            := (edCotes          in E);
  chkIDStations.Checked       := (edIDStations     in E);
  chkParois.Checked           := (edWalls          in E);
  chkRemplissage.Checked      := (edFillGalerie    in E);
  chkSections.Checked         := (edCrossSections  in E);
  chkQuadrillage.Checked      := (edQuadrilles     in E);
  chkEntrances.Checked        := (edENTRANCE_MKS   in E);
    chkEntranceNames.Checked  := (edENTRANCE_MKS   in E);
  chkEntranceNames.Checked    := (edENTRANCE_NAMES in E);
  chkAntennes.Checked         := (edANTENNES       in E);
  chkCroquis.Checked          := (edCROQUIS        in E);
  chkNoeuds.Checked           := (edJONCTIONS      in E);
  chkVolumesVisibles.Checked  := (edVolumes        in E);
  chkPOI.Checked              := (edPOI            in E);
  chkCubeBounds.Checked       := (edBounds         in E);
end;


function TfrmParametrerOngletVue2D.GetChksElementsDrawn: TSetElementsDrawn;
begin
  Result := [];
  if (chkEntrances.Checked)       then Result := Result + [edENTRANCE_MKS]
                                  else Result := Result - [edENTRANCE_MKS];
  if (chkEntranceNames.Checked)   then Result := Result + [edENTRANCE_NAMES]
                                  else Result := Result - [edENTRANCE_NAMES];

  chkEntranceNames.Checked := chkEntrances.Checked;
  if (chkStations.Checked)        then Result := Result + [edStations]
                                  else Result := Result - [edStations];
  if (chkAltitudes.Checked)       then Result := Result + [edAltitudes]
                                  else Result := Result - [edAltitudes];
  if (chkCotes.Checked)           then Result := Result + [edCotes]
                                  else Result := Result - [edCotes];
  if (chkParois.Checked)          then Result := Result + [edWalls]
                                  else Result := Result - [edWalls];
  if (chkIDStations.Checked)      then Result := Result + [edIDStations]
                                  else Result := Result - [edIDStations];
  if (chkSections.Checked)        then Result := Result + [edCrossSections]
                                  else Result := Result - [edCrossSections];
  if (chkRemplissage.Checked)     then Result := Result + [edFillGalerie]
                                  else Result := Result - [edFillGalerie];
  if (chkQuadrillage.Checked)     then Result := Result + [edQuadrilles]
                                  else Result := Result - [edQuadrilles];
  if (chkPolygonales.Checked)     then Result := Result + [edPolygonals]
                                  else Result := Result - [edPolygonals];
  if (chkAntennes.Checked)        then Result := Result + [edANTENNES]
                                  else Result := Result - [edANTENNES];
  if (chkCroquis.Checked)         then Result := Result + [edCROQUIS]
                                  else Result := Result - [edCROQUIS];
  if (chkNoeuds.Checked)          then Result := Result + [edJONCTIONS]
                                  else Result := Result - [edJONCTIONS];
  if (chkVolumesVisibles.Checked) then Result := Result + [edVolumes]
                                  else Result := Result - [edVolumes];
  if (chkPOI.Checked)             then Result := Result + [edPOI]
                                  else Result := Result - [edPOI];
  if (chkCubeBounds.Checked)      then Result := Result + [edBounds]
                                  else Result := Result - [edBounds];
end;

procedure TfrmParametrerOngletVue2D.SetPanelParamVisible;
var
  EWE: String;
begin
  EWE := 'Parametres pour la ';
  case FModeParametrage of
    mpdVUE2D            : self.Caption := EWE + 'Vue en Plan';
    mpdVUE3D            : self.Caption := EWE + 'Vue 3D';
    mpdCOUPE_DEVELOPPEE : self.Caption := EWE + 'Coupe developpee';
  end;
  (*
  pnlParametresSpecifiques2D.Visible       := false;
  pnlParametresSpecifiques3D.Visible       := false;
  pnlParametresSpecifiquesCoupeDev.Visible := false;
  pnlDegradeAltitudes.Visible              := false;
  case FModeParametrage of
    mpdVUE2D            : pnlParametresSpecifiques2D.Visible := true;
    mpdVUE3D            : pnlParametresSpecifiques3D.Visible := true;
    mpdCOUPE_DEVELOPPEE : pnlParametresSpecifiquesCoupeDev.Visible := true;
  end;
  case FModeParametrage of
    mpdVUE2D,
    mpdVUE3D            : pnlDegradeAltitudes.Visible := true;
    mpdCOUPE_DEVELOPPEE : pnlDegradeAltitudes.Visible := false;
  end;
  //*)
end;

function TfrmParametrerOngletVue2D.Initialiser(): boolean;
begin
  result := false;
  self.Position := poScreenCenter;
  InitialiserComboBoxQuadrillage(cmbTypeQuadrillage, qtGRID);
  InitialiserComboBoxRepresentationVisees(cmbRepresentationGaleries, rgSEANCES);
  InitialiserComboBoxModeDessinMaillage(cmbModeDessinMaillage, M3D_NONE);


  SetPanelParamVisible();

  tabShtGeneral.Caption      := 'Général';
  tabShtQuadrillage.Caption  := 'Quadrillage';
  tabShtGaleries.Caption     := 'Galeries';
  tabShtMNT.Caption          := 'MNT';
  result := True;
end;

procedure TfrmParametrerOngletVue2D.SetModeParametrageDialog(const Q: TModeDialogParamVues);
begin
  FModeParametrage := Q;

end;



procedure TfrmParametrerOngletVue2D.SetValuesOnglet3D(const O: TVue3DParams);
begin
  self.Caption := 'Settings vue 3D';

  FCurrParams3D := O;                 // sauvegarde paramètres
  cmbRepresentationGaleries.ItemIndex  := ord(O.ModeRepresentation);
  SetChksElementsDrawn(O.ElementsDrawn);
  editFactZ.Value              := O.CoefMagnification;
  // doit-on dessiner les volumes
  chkVolumesVisibles.Checked   := (edVolumes in  O.ElementsDrawn);
  // couleurs
  btnCubeColor.ButtonColor     := O.ColorCube;
  btnBGColor.ButtonColor       := O.ColorBackGround;
  btnReferentielColor.Color    := O.ColorReferentiel;




  btnDegradeZMiniReseau.ButtonColor  := O.ColorZMiniReseau;
  btnDegradeZMaxiReseau.ButtonColor  := O.ColorZMaxiReseau;

  // MNT
  chkDegradeAltitudesMNT.Checked     := O.MaillageUseDegrades;
  trkbMaillageOpacity.Position       := O.MaillageOpacity;
  cmbModeDessinMaillage.ItemIndex    := Ord(O.MaillageModeDessin);
  btnDegradeZMiniMNT.ButtonColor     := O.ColorZMiniMNT;
  btnDegradeZMaxiMNT.ButtonColor     := O.ColorZMaxiMNT;

  // opacité des remplissages
  sclFillOpacity.Position               := O.FillOpacity;
  // épaisseurs trait
  editLargeurTraitCenterlineInPX.Value  := O.ViseesLargeur;
end;


procedure TfrmParametrerOngletVue2D.SetValuesOnglet2D(const O: TVue2DParams);
begin
  self.Caption := 'Settings vue 2D';
  // tracé rapide de la centerline
  chkFastDrawCenterline.Checked     := O.ongDrawFastCenterline;

  cmbTypeQuadrillage.ItemIndex      := Ord(O.ongQdrType);
  // sauvegarde paramètres
  FCurrParams2D := O;

  // éléments à dessiner
  SetChksElementsDrawn(O.ongElementsDrawn);

  // autres valeurs
  editQdrSpc.Text                       := Format(FORMAT_NB_REAL_0_DEC,[O.ongQdrSpc]);
  btnBGColor.ButtonColor                := O.ongBackGround;
  btnQdrColor.ButtonColor               := O.ongQdrColor;

  editLargeurTraitCenterlineInPX.Value  := O.ongViseesLargeurInPX;
  editLargeurTraitCenterlineInMM.Value  := O.ongViseesLargeurInMM;

  // sauvegarde temp des coordonnées de la zone d'affichage (non modifiables)
  FC1 := O.ongC1; //  FC1.X := O.ongX1;  FC1.Y := O.ongY1;
  FC2 := O.ongC2; // FC2.X := O.ongX2;  FC2.Y := O.ongY2;
  lbX1Y1.Caption := Format('Du coin: %.0f - %.0f', [FC1.X, FC1.Y]);
  lbX2Y2.Caption := Format('Au coin: %.0f - %.0f', [FC2.X, FC2.Y]);
  // dégradé
  btnDegradeZMiniReseau.ButtonColor     := O.ongDegradeAltMiniReseau;
  btnDegradeZMaxiReseau.ButtonColor     := O.ongDegradeAltMaxiReseau;

  btnDegradeZMiniMNT.ButtonColor        := O.ongDegradeAltMiniMNT;
  btnDegradeZMaxiMNT.ButtonColor        := O.ongDegradeAltMaxiMNT;

  // opacité des remplissages
  sclFillOpacity.Position               := O.ongFillOpacite;
  // mode de représentation
  cmbRepresentationGaleries.ItemIndex   := ord(O.ongModeRepresentation);

  // taille et couleur des textes
  editTailleTexteIDStations.Value       := O.ongTailleTexteIDStation;
  editTailleTexteAltitudes.Value        := O.ongTailleTexteAltitudes;
  editTailleTexteCotation.Value         := O.ongTailleTexteCotation;
  editTailleTexteEntrees.Value          := O.ongTailleTexteEntrances;
  editTailleTexteNoeuds.Value           := O.ongTailleTexteNoeuds;
  editTailleTextePOIs.Value             := O.ongTailleTextePOIs;

  btnCouleurTexteIDStations.ButtonColor := O.ongCouleurIDStation;
  btnCouleurTexteAltitudes.ButtonColor  := O.ongCouleurAltitudes;
  btnCouleurTexteCotation.ButtonColor   := O.ongCouleurCotation;
  btnCouleurTexteEntrees.ButtonColor    := O.ongCouleurEntrances;
  btnCouleurTexteNoeuds.ButtonColor     := O.ongCouleurNoeuds;
  btnCouleurTextePOIs.ButtonColor       := O.ongCouleurPOIs;

  chkDoDrawViseesNonRetenues.Checked    := O.ongDoDispViseesNonRetenues;
  btnColorViseesNonRetenues.ButtonColor := O.ongCouleurViseesNonRetenues;
end;

function TfrmParametrerOngletVue2D.GetValuesOnglet2D(): TVue2DParams;
begin
  // récup des params non modifiés ou masqués
  Result := FCurrParams2D;
  with Result do
  begin
    ongBackGround         := btnBGColor.ButtonColor;             // couleur de fond
    ongElementsDrawn      := GetChksElementsDrawn();             // éléments à dessiner
    ongDrawFastCenterline := chkFastDrawCenterline.Checked;      // tracé rapide
    // quadrillage
    ongQdrColor        := btnQdrColor.ButtonColor;
    ongQdrType         := TQdrType(cmbTypeQuadrillage.ItemIndex);
    ongQdrSpc          := ConvertirEnNombreReel(Trim(editQdrSpc.Text), 10.00);
    if (ongQdrSpc < 10.00) then ongQdrSpc := 10.00; // valeur minimale de l'espacement
    // récupération des coordonnées de zone non modifiables
    ongC1 := FC1;
    ongC2 := FC2;
    // dégradé
    ongDegradeAltMiniReseau  := btnDegradeZMiniReseau.ButtonColor;
    ongDegradeAltMaxiReseau  := btnDegradeZMaxiReseau.ButtonColor;

    ongDegradeAltMiniMNT     := btnDegradeZMiniMNT.ButtonColor;
    ongDegradeAltMaxiMNT     := btnDegradeZMaxiMNT.ButtonColor;

    // largeur de trait
    ongViseesLargeurInPX := editLargeurTraitCenterlineInPX.Value;
    ongViseesLargeurInMM := editLargeurTraitCenterlineInMM.Value;
    // opacité des remplissages
    ongFillOpacite   := sclFillOpacity.Position;
    // mode de representation
    ongModeRepresentation   := TModeRepresentationGaleries(cmbRepresentationGaleries.ItemIndex);
    // taille et couleur des textes
    ongTailleTexteIDStation := editTailleTexteIDStations.Value;
    ongTailleTexteAltitudes := editTailleTexteAltitudes.Value;
    ongTailleTexteCotation  := editTailleTexteCotation.Value;

    ongCouleurIDStation     := btnCouleurTexteIDStations.ButtonColor;
    ongCouleurAltitudes     := btnCouleurTexteAltitudes.ButtonColor;
    ongCouleurCotation      := btnCouleurTexteCotation.ButtonColor;

    // visées non retenues par le MétaFiltre
    ongDoDispViseesNonRetenues  := chkDoDrawViseesNonRetenues.Checked;
    ongCouleurViseesNonRetenues := btnColorViseesNonRetenues.ButtonColor;

  end;
end;

function TfrmParametrerOngletVue2D.GetValuesOnglet3D(): TVue3DParams;
begin
  // récup des params non modifiés ou masqués
  Result                      := FCurrParams3D;
  // couleur de fond
  Result.ElementsDrawn        := GetChksElementsDrawn();              // éléments à dessiner
  Result.CoefMagnification    := editFactZ.Value;                 // facteur Z
  Result.ColorBackGround      := btnBGColor.ButtonColor;
  Result.ColorReferentiel     := btnReferentielColor.ButtonColor;
  Result.ColorCube            := btnCubeColor.ButtonColor;            // cube
  Result.ColorZMiniReseau     := btnDegradeZMiniReseau.ButtonColor;         // dégradés
  Result.ColorZMaxiReseau     := btnDegradeZMaxiReseau.ButtonColor;


  Result.FillOpacity          := sclFillOpacity.Position;             // opacité des remplissages
  Result.ViseesLargeur        := editLargeurTraitCenterlineInPX.Value;    // largeur de trait
  Result.ModeRepresentation   := TModeRepresentationGaleries(cmbRepresentationGaleries.ItemIndex);

  Result.MaillageUseDegrades  := chkDegradeAltitudesMNT.Checked;
  Result.MaillageOpacity      := trkbMaillageOpacity.Position;
  Result.MaillageModeDessin   := TMNTModeDessinMaillage(cmbModeDessinMaillage.ItemIndex);

  Result.ColorZMiniMNT        := btnDegradeZMiniMNT.ButtonColor;         // dégradés
  Result.ColorZMaxiMNT        := btnDegradeZMaxiMNT.ButtonColor;
end;


//------------------------------------------------------------------------------
function TfrmParametrerOngletVue2D.GetValuesCoupeDeveloppee(): TCoupeDeveloppeeParams;
begin
  // récupération des valeurs non modifiées ou masquées
  Result := FCurrCoupeDeveloppeeParams;
  Result.ElementsDrawn := GetChksElementsDrawn;
  Result.BackGround    := btnBGColor.ButtonColor;
  Result.QdrColor      := btnQdrColor.ButtonColor;
  Result.QdrSpc        := ConvertirEnNombreReel(editQdrSpc.Text, 10.00);
  Result.ColorZMini    := btnDegradeZMiniReseau.ButtonColor;
  Result.ColorZMaxi    := btnDegradeZMaxiReseau.ButtonColor;
  result.ViseesLargeur := editLargeurTraitCenterlineInPX.Value;

end;
procedure TfrmParametrerOngletVue2D.SetValuesCoupeDeveloppee(const O: TCoupeDeveloppeeParams);
begin
  FCurrCoupeDeveloppeeParams := O;
  SetChksElementsDrawn(FCurrCoupeDeveloppeeParams.ElementsDrawn);
  btnBGColor.ButtonColor := FCurrCoupeDeveloppeeParams.BackGround;
  btnQdrColor.ButtonColor:= FCurrCoupeDeveloppeeParams.QdrColor;
  editQdrSpc.Text := Format(FORMAT_NB_REAL_0_DEC, [FCurrCoupeDeveloppeeParams.QdrSpc]);
  btnDegradeZMiniReseau.ButtonColor := FCurrCoupeDeveloppeeParams.ColorZMini;
  btnDegradeZMaxiReseau.ButtonColor := FCurrCoupeDeveloppeeParams.ColorZMaxi;
  editLargeurTraitCenterlineInPX.Value := FCurrCoupeDeveloppeeParams.ViseesLargeur;
end;

end.

