unit CadreCode;
// Date: 16/05/2013
// Statut: OK
// 04/10/2013: Conversions UTF8 <> ANSI fixed
// 05/08/2014: PutCodeInForm(): Fixation de cas oubliés dans les unités de clino
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common, StructuresDonnees,
  CallDialogsStdVersion,
  Classes, SysUtils, FileUtil, Forms, Controls,
  StdCtrls, ExtCtrls, curredit;

type

  { TCdrCode }

  TCdrCode = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    btnHelpErrTourillon: TButton;
    btnPropalerCorrectionAzimuts: TButton;
    btnPropalerCorrectionPentes: TButton;
    Button3: TButton;
    Button4: TButton;
    btnHelpBoules: TButton;
    cmbUB: TComboBox;
    cmbUC: TComboBox;
    cmbAzimutDirecte: TComboBox;
    cmbPenteDirecte: TComboBox;
    cmbPosZero: TComboBox;
    editDiamBoule1: TCurrencyEdit;
    editDiamBoule2: TCurrencyEdit;
    editLongDevReference: TCurrencyEdit;
    editDeniveleReference: TCurrencyEdit;
    editErreurTourillon: TCurrencyEdit;
    editAzCo: TCurrencyEdit;
    editAzErrMax: TCurrencyEdit;
    editAzErrMaxAz: TCurrencyEdit;
    editAzTest: TCurrencyEdit;
    editIncCo: TCurrencyEdit;
    editFactLong: TCurrencyEdit;
    editIncErrMax: TCurrencyEdit;
    editIncErrMaxInc: TCurrencyEdit;
    editIncTest: TCurrencyEdit;
    editPsiL: TCurrencyEdit;
    editPsiAz: TCurrencyEdit;
    editPsiP: TCurrencyEdit;
    editAngleLimite: TCurrencyEdit;
    editNoCode: TCurrencyEdit;
    editCommentaires: TEdit;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    lbDiametreBoules: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    lbErreurTourillon: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lbAzCorrige: TStaticText;
    lbCosAz: TLabel;
    lbCosInc: TLabel;
    lblObs1: TLabel;
    lbMesureVerticaleInvDir: TLabel;
    lbErrAz: TStaticText;
    lbErrInc: TStaticText;
    lblLimit: TLabel;
    lblPrecision: TLabel;
    lblObs: TLabel;
    lblZero: TLabel;
    lblLength: TLabel;
    lbMesureHorizontaleInvDir: TLabel;
    lblClino: TLabel;
    lblCompas: TLabel;
    lblNCode: TLabel;
    lbCodeComp: TStaticText;
    lbCodeIncl: TStaticText;
    lbPenteCorrigee: TStaticText;
    pnlPropalCorrPentes: TPanel;
    pnlFonctionsCorrection: TPanel;
    procedure btnHelpBoulesClick(Sender: TObject);
    procedure btnHelpErrTourillonClick(Sender: TObject);
    procedure btnPropalerCorrectionPentesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cmbUBChange(Sender: TObject);
    procedure cmbUCChange(Sender: TObject);
    procedure editAzCoChange(Sender: TObject);
  private

    procedure InitCaptions();
    procedure PutCodeInForm(const MyCode: TCode);

    { private declarations }
  public
    { public declarations }
    function  Initialiser(const C: TCode): boolean;
    function  GetCodeFromForm(): TCode;
  end; 

implementation

{$R *.lfm}

function TCdrCode.Initialiser(const C: TCode): boolean;
begin
  result := false;
  InitCaptions();
  PutCodeInForm(C);
  result := true;
end;




procedure TCdrCode.Button1Click(Sender: TObject);
var
  EWE: TParamFoncCorrectionAngulaire;
  WU: Double;
begin
  EWE.setFrom(editAzCo.Value, editAzErrMax.Value, editAzErrMaxAz.Value);
  WU  := CalcAzimutCorrige(editAzTest.Value, UNITE_ANGULAIRE_DU_CODE_ZERO, EWE);
  lbAzCorrige.Caption := Format('%.4f', [WU]);
end;

procedure TCdrCode.btnHelpErrTourillonClick(Sender: TObject);
begin
  DisplayHelpSystem('ERREUR_TOURILLON');
end;

procedure TCdrCode.btnHelpBoulesClick(Sender: TObject);
begin
  DisplayHelpSystem('BOULES_CIBLES');
end;

procedure TCdrCode.btnPropalerCorrectionPentesClick(Sender: TObject);
begin
  pnlPropalCorrPentes.Visible := True;
end;

procedure TCdrCode.Button2Click(Sender: TObject);
var
  EWE: TParamFoncCorrectionAngulaire;
  WU: Double;
begin
  EWE.setFrom(editIncCo.Value, editIncErrMax.Value, editIncErrMaxInc.Value);
  WU  := CalcPenteCorrigee(editIncTest.Value, UNITE_ANGULAIRE_PAR_DEFAUT, EWE);
  lbPenteCorrigee.Caption := Format('%.4f', [WU]);
end;

procedure TCdrCode.Button3Click(Sender: TObject);
begin
  pnlPropalCorrPentes.Visible := false;
end;

procedure TCdrCode.Button4Click(Sender: TObject);
var
  EWE: Double;
begin
  EWE := CalcCorrectionAngulaire(self.GetCodeFromForm(),
                                 editLongDevReference.Value,
                                 editDeniveleReference.Value);
  editIncCo.Value := EWE;
end;

procedure TCdrCode.cmbUBChange(Sender: TObject);
begin
  case cmbUB.ItemIndex of
    0: lbErrAz.Caption:='Az (gr)';
    1: lbErrAz.Caption:='Az (°)';
  end;
end;

procedure TCdrCode.cmbUCChange(Sender: TObject);
begin
  case cmbUC.ItemIndex of
    0: lbErrInc.Caption:='Inc (gr)';  // 400
    1: lbErrInc.Caption:='Inc (°)';   // 360
    2: lbErrInc.Caption:='Inc (%)';   // 370
    3: lbErrInc.Caption:='Z (m)';     // 380
    4: lbErrInc.Caption:='dz (m)';    // 380
  else lbErrInc.Caption:='Inc (gr)';
  end;
end;

procedure TCdrCode.editAzCoChange(Sender: TObject);
begin

end;


procedure TCdrCode.InitCaptions;
  procedure SetCmbDirInv(var CB: TComboBox);
  begin
    CB.Clear;
    CB.Items.Add(GetResourceString(rsCDR_CODES_VDIRECT));
    CB.Items.Add(GetResourceString(rsCDR_CODES_VINVERSE));
    CB.ItemIndex := 0;
  end;
begin

  lblNCode.Caption  := GetResourceString(rsCDR_CODES_NUMERO);

  // unité boussole
  lblCompas.Caption := GetResourceString(rsCDR_CODES_GRADCOMPAS);
  cmbUB.Clear;
  cmbUB.Items.Add(GetResourceString(rsCDR_CODES_CMBUNIT_0));
  cmbUB.Items.Add(GetResourceString(rsCDR_CODES_CMBUNIT_1));
  lblClino.Caption  := GetResourceString(rsCDR_CODES_GRADCLINO);

  // unité clino
  with cmbUC do begin
    Clear;
    Items.Add(GetResourceString(rsCDR_CODES_CMBUNIT_0));
    Items.Add(GetResourceString(rsCDR_CODES_CMBUNIT_1));
    Items.Add(GetResourceString(rsCDR_CODES_CMBUNIT_2));
    Items.Add(GetResourceString(rsCDR_CODES_CMBUNIT_3));
    Items.Add(GetResourceString(rsCDR_CODES_CMBUNIT_4));
  end;
  // visées directes/inverse
  lbMesureHorizontaleInvDir.Caption  := GetResourceString(rsCDR_CODES_VISEE_HZ);
  lbMesureVerticaleInvDir.Caption    := GetResourceString(rsCDR_CODES_VISEE_VT);
  SetCmbDirInv(cmbAzimutDirecte);
  SetCmbDirInv(cmbPenteDirecte);
  lblLength.Caption:=GetResourceString(rsCDR_CODES_FACT);
  lblZero.Caption  :=GetResourceString(rsCDR_CODES_POSZERO);
  with cmbPosZero do begin
    Clear;
    Items.Add(GetResourceString(rsCDR_CODES_CMBZERO_0));
    Items.Add(GetResourceString(rsCDR_CODES_CMBZERO_1));
    Items.Add(GetResourceString(rsCDR_CODES_CMBZERO_2));
  end;
  lblLimit.Caption     := GetResourceString(rsCDR_CODES_ANGLIMIT);
  lblPrecision.Caption := GetResourceString(rsCDR_CODES_PRECISION);
  lblObs.Caption       := GetResourceString(rsSELECT_LISTE_OBSERV);
  lbErreurTourillon.Caption := GetResourceString(rsCDR_CODES_ERREUR_TOURILLON);
  lbDiametreBoules.Caption  := GetResourceString(rsCDR_CODES_DIAM_BOULES_CIBLES);
  // fonctions de correction
  lbCosAz.Caption  := '* cos(Azimut0 +';
  lbCosInc.Caption := '* cos(Pente0 +';
  pnlPropalCorrPentes.Visible    := false;
  {$IFDEF GROS_MINET}
  pnlFonctionsCorrection.Enabled := false;
  {$else}
  pnlFonctionsCorrection.Enabled := true;

  {$endif}
end;

function TCdrCode.GetCodeFromForm(): TCode;
var
  c: TCode;
begin
  C.IDCode:=editNoCode.AsInteger;
  //C.IDCode:=StrToInt(editNoCode.Text);
  case cmbUB.ItemIndex of
    0: C.GradAz := GRADES_PAR_TOUR;
    1: C.GradAz := DEGRES_PAR_TOUR;
  end;
  case cmbUC.ItemIndex of
    0: C.GradInc := GRADES_PAR_TOUR;
    1: C.GradInc := DEGRES_PAR_TOUR;
    2: C.GradInc := 370.00;
    3: C.GradInc := 380.00;
    4: C.GradInc:= UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR;
  end;
  // visée directe ou inverse: azimuts: retrancher 10 du code azimut
  if (cmbAzimutDirecte.ItemIndex = 1) then C.GradAz  := C.GradAz - 10.0;
  // visée directe ou inverse: pentes
  if (cmbPenteDirecte.ItemIndex  = 1) then C.GradInc := C.GradInc - 10.0;

  // position du zéro
  case cmbPosZero.ItemIndex of
    0: C.Gradinc := C.Gradinc - 1.0; // nadiral
    1: ;                         // horizontal
    2: C.Gradinc := C.Gradinc + 1.0; // zénithal
  end;

  C.PsiL  :=editPsiL.Value;      // précision des mesures
  C.PsiAz :=editPsiAZ.Value;
  C.PsiP  :=editPsiP.Value;
  // TODO: Support de FactLong a valider
  C.FactLong  := 1.00; // editFactLong.Value;
  C.AngLimite := editAngleLimite.Value;
  // type de galeries -- DEPRECATED
  //C.ReservedInt := 0;//cmbTypeGalerie.ItemIndex;
  //-----------------
  C.Commentaire:= _LCLStrToAnsi(editCommentaires.Text);
  // erreur de tourillon
  C.ErreurTourillon   := editErreurTourillon.Value;
  // boules-cibles
  C.DiametreBoule1    := editDiamBoule1.Value / 1000.0;
  C.DiametreBoule2    := editDiamBoule2.Value / 1000.0;

  // fonctions de correction
  C.ParamsFuncCorrAz.setFrom(editAzCo.Value, editAzErrMax.Value, editAzErrMaxAz.Value);
  C.ParamsFuncCorrInc.setFrom(editIncCo.Value, editIncErrMax.Value, editIncErrMaxInc.Value);
  Result := C;
end;


procedure TCdrCode.PutCodeInForm(const MyCode: TCode);
var
  ucc: integer;
  procedure S666(const QCmb: TComboBox;
                 const QIdx: integer;
                 const QLbl: TStaticText;
                 const QCaption: string);
  begin
    QCmb.ItemIndex := QIdx;
    QLbl.Caption   := QCaption;
  end;
  procedure S777(const QCmb1: TComboBox;
                 const QIdx1: integer;
                 const QLbl: TStaticText;
                 const QCaption: string;
                 const QCmb2: TComboBox;
                 const QIdx2: integer);
  begin
    QCmb1.ItemIndex := QIdx1;
    QLbl.Caption    := QCaption;
    QCmb2.ItemIndex := QIdx2;
  end;
begin;
  // valeurs internes
  lbCodeComp.Caption := Format(FORMAT_NB_INTEGER,[Round(MyCode.GradAz)]);
  lbCodeIncl.Caption := Format(FORMAT_NB_INTEGER,[Round(MyCode.GradInc)]);

  // visées directes par défaut
  cmbAzimutDirecte.ItemIndex := 0;
  cmbPenteDirecte.ItemIndex  := 0;
  //---------------------------------
  editNoCode.AsInteger:=MyCode.IDCode;
  // Graduation du compas
  ucc:=Round(MyCode.GradAz);
  case ucc of
    389, 399, 400: S666(cmbUB, 0, lbErrAz, 'Az (gr)');
    349, 359, 360: S666(cmbUB, 1, lbErrAz, 'Az (°)');
    // visées inverses
    //350, 390:
    350: S777(cmbUB, 1, lbErrAz, 'Az (°)' , cmbAzimutDirecte, 1);
    390: S777(cmbUB, 0, lbErrAz, 'Az (gr)', cmbAzimutDirecte, 1);
  else
    cmbUB.ItemIndex:=0;
  end;
  // Graduation du clinomètre
  ucc:=Round(MyCode.GradInc);
  // DONE: Cas oubliés dans les unités de clino
  case ucc of
    399, 400, 401: S666(cmbUC, 0, lbErrInc, 'Inc (gr)');
    359, 360, 361: S666(cmbUC, 1, lbErrInc, 'Inc (deg)');
    370: S666(cmbUC, 2, lbErrInc, 'Inc (%)');
    380: S666(cmbUC, 3, lbErrInc, 'Inc (m)');
    UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR:
         S666(cmbUC, 4, lbErrInc, 'dz (m)');
    // visées inverses
    350: S777(cmbUC, 1, lbErrInc, 'Inc (deg)', cmbPenteDirecte, 1);
    390: S777(cmbUC, 0, lbErrInc, 'Inc (gr)', cmbPenteDirecte, 1);
    UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV:
         S777(cmbUC, 4, lbErrInc, 'dz (m)', cmbPenteDirecte, 1);
  else
    S666(cmbUC, 0, lbErrInc, 'Inc (gr)');
  end;
  // Position du zéro
  case ucc of
    359, 399: cmbPosZero.ItemIndex := 0;  // nadiral
    360, 400: cmbPosZero.ItemIndex := 1;  // horizontal
    361, 401: cmbPosZero.ItemIndex := 2;  // zénithal
  else
    cmbPosZero.ItemIndex := 1;
  end;
  // Facteur de correction des longueurs
  editFactLong.Value := 1.00; //MyCode.FactLong;
  // modifié 26.08.04
  // Angle Limite
  editAngleLimite.Value := MyCode.AngLimite;
  // Précision des instruments
  editPsiL.Value  :=MyCode.PsiL;
  editPsiAZ.Value :=MyCode.PsiAZ;
  editPsiP.Value  :=MyCode.PsiP;
  // Erreur de tourillon
  editErreurTourillon.Value := MyCode.ErreurTourillon;
  // Boules-cibles
  editDiamBoule1.Value := MyCode.DiametreBoule1 * 1000.0;
  editDiamBoule2.Value := MyCode.DiametreBoule2 * 1000.0;
  // Commentaires
  editCommentaires.Text  :=_AnsiToLCLStr(MyCode.Commentaire);
  // Fonctions de correction
  editAzCo.Value         := MyCode.ParamsFuncCorrAz.Co;
  editAzErrMax.Value     := MyCode.ParamsFuncCorrAz.ErreurMax;
  editAzErrMaxAz.Value   := MyCode.ParamsFuncCorrAz.PosErrMax;
  editIncCo.Value        := MyCode.ParamsFuncCorrInc.Co;
  editIncErrMax.Value    := MyCode.ParamsFuncCorrInc.ErreurMax;
  editIncErrMaxInc.Value := MyCode.ParamsFuncCorrInc.PosErrMax;
end;



end.

