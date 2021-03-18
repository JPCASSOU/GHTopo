unit frmMiniConvertisseur;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  ConvertisseurJPC,
  unitUtilsComposants,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, curredit;

type

  { TdlgMiniConvertisseur }

  TdlgMiniConvertisseur = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    btnCalculer: TButton;
    cmbSystCibleJPC: TComboBox;
    cmbSystSourceJPC: TComboBox;
    editX_Cible1JPC: TCurrencyEdit;
    editX_Source1JPC: TCurrencyEdit;
    editY_Cible1JPC: TCurrencyEdit;
    editY_Source1JPC: TCurrencyEdit;
    lbSystCible: TLabel;
    lbSystSource: TLabel;
    lbValeursCalculees: TLabel;
    lbValeursEntree: TLabel;
    lbValeursX: TLabel;
    lbValeursY: TLabel;
    Panel2: TPanel;
    Panel7: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure btnCalculerClick(Sender: TObject);
    procedure cmbSystCibleJPCChange(Sender: TObject);
    procedure cmbSystSourceJPCChange(Sender: TObject);
    procedure editX_Source1JPCKeyPress(Sender: TObject; var Key: char);
    procedure editY_Source1JPCKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    FConversionUtils : TConversionSysteme;
    procedure SetLabelsValeursDemandees(const Idx: integer);
  public
    function Initialiser(const EPSGIn, EPSGOut: integer; var CoordsX, CoordsY: double): boolean;

  end;

var
  dlgMiniConvertisseur: TdlgMiniConvertisseur;

implementation

{$R *.lfm}

{ TdlgMiniConvertisseur }
procedure TdlgMiniConvertisseur.SetLabelsValeursDemandees(const Idx: integer);
begin
  lbValeursX.Caption := 'X (m)';
  lbValeursY.Caption := 'Y (m)';
  if (Idx = 0) then
  begin
    lbValeursX.Caption := 'Lat. (Deg. dec.)';
    lbValeursY.Caption := 'Long. (Deg. dec.)';
  end;
end;

procedure TdlgMiniConvertisseur.Button1Click(Sender: TObject);
var
  toto: Integer;
begin
  toto := cmbSystSourceJPC.ItemIndex;
  cmbSystSourceJPC.ItemIndex := cmbSystCibleJPC.ItemIndex;
  cmbSystCibleJPC.ItemIndex  := toto;
end;

procedure TdlgMiniConvertisseur.btnCalculerClick(Sender: TObject);
var
    P1, P2: TProjUV;
begin
  // si syst source = syst cible --> []
  if (cmbSystSourceJPC.ItemIndex = cmbSystCibleJPC.ItemIndex) then Exit;
  try
    P1.U := editX_Source1JPC.Value;
    P1.V := editY_Source1JPC.Value;
  except
    P1.U := DegMinSec2DegDec(editX_Source1JPC.Text);
    P1.V := DegMinSec2DegDec(editY_Source1JPC.Text);
  end;
  P2 := FConversionUtils.ConversionSyst1ToSyst2Index(cmbSystSourceJPC.ItemIndex,
                                                     cmbSystCibleJPC.ItemIndex,
                                                     P1);
  editX_Cible1JPC.Value := P2.U;
  editY_Cible1JPC.Value := P2.V;
end;

procedure TdlgMiniConvertisseur.cmbSystCibleJPCChange(Sender: TObject);
begin
  SetLabelsValeursDemandees(cmbSystCibleJPC.ItemIndex);
end;

procedure TdlgMiniConvertisseur.cmbSystSourceJPCChange(Sender: TObject);
begin
  SetLabelsValeursDemandees(cmbSystSourceJPC.ItemIndex);
end;

procedure TdlgMiniConvertisseur.editX_Source1JPCKeyPress(Sender: TObject;
  var Key: char);
begin
  if (Key in ['0' .. '9', '.', ',', '-', #8, #13]) then btnCalculerClick(self);
end;

procedure TdlgMiniConvertisseur.editY_Source1JPCKeyPress(Sender: TObject;
  var Key: char);
begin
  if (Key in ['0' .. '9', '.', ',', '-', #8, #13]) then btnCalculerClick(self);
end;

procedure TdlgMiniConvertisseur.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

function TdlgMiniConvertisseur.Initialiser(const EPSGIn, EPSGOut: integer; var CoordsX, CoordsY: double): boolean;
var
  IdxSrc, IdxDest: Integer;
begin
  self.Caption := GetResourceString(rsDLG_CALC_CDR_CONVERT);
  Result := false;
  FConversionUtils := TConversionSysteme.Create;
  try
    Result := FConversionUtils.Initialiser();
    if (not result) then Exit;
    IdxSrc  := FConversionUtils.GetIndexSystemeByCodeEPSG(EPSGIn);
    IdxDest := FConversionUtils.GetIndexSystemeByCodeEPSG(EPSGOut);

    lbSystSource.Caption            := GetResourceString(rsDLG_CALC_LB_SYST_SOURCE);
    lbSystCible.Caption             := GetResourceString(rsDLG_CALC_LB_SYST_CIBLE);
    lbValeursEntree.Caption         := GetResourceString(rsDLG_CALC_LB_COORDS_EN_ENTREE);
    lbValeursCalculees.Caption      := GetResourceString(rsDLG_CALC_LB_COORDS_EN_SORTIE);

    RemplirCombosSystemesCoordonnees(FConversionUtils, cmbSystSourceJPC, False, IdxSrc);
    RemplirCombosSystemesCoordonnees(FConversionUtils, cmbSystCibleJPC, False, IdxDest);
    SetLabelsValeursDemandees(0);
    editX_Source1JPC.Value := CoordsX;
    editY_Source1JPC.Value := CoordsY;
    editX_Cible1JPC.Value := 0.00;
    editY_Cible1JPC.Value := 0.00;
    Result := True;
  except
  end;
end;

end.

