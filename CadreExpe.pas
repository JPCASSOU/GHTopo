unit CadreExpe;
// Date: 16/05/2013
// Statut: OK
// 04/10/2013: Conversions UTF8 <> ANSI fixed
// 06/05/2016: Les dates sont saisies par un TDatePicker
// 02/01/2017: Les dates sont saisies via un TMaskdit. TDatePicker est une bouse.
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  StructuresDonnees,
  CallDialogsStdVersion,
  UnitClassPalette,
  ToporobotClasses2012,

  // pour la déclinaison magnétique
  {$IFDEF MSWINDOWS}
    UnitWrapperDeclimag,
  {$ENDIF}
  {$IFDEF LINUX}
    UnitWrapperDeclimagLinux,
  {$ENDIF}
  math, Classes, SysUtils, FileUtil, Forms, Dialogs, Controls, StdCtrls,
  Buttons, EditBtn, MaskEdit, curredit;

type

  { TCdrExpe }

  TCdrExpe = class(TFrame)
    btnCalcDeclimag: TButton;
    Button1: TButton;
    Button2: TButton;
    cmbModeDecl: TComboBox;
    cmbUnitDeclimag: TComboBox;
    editDateSeance: TDateEdit;
    editCouleur: TCurrencyEdit;
    editDeclimag: TCurrencyEdit;
    editSpeleometre: TEdit;
    editSpeleographe: TEdit;
    editNoExpe: TCurrencyEdit;
    lblColor: TLabel;
    lblObs: TLabel;
    lblDeclimag: TLabel;
    lblSpeleometre: TLabel;
    lblSpeleographe: TLabel;
    lblDate: TLabel;
    lblSeance: TLabel;
    editComment: TMemo;
    lbColorSeance: TStaticText;
    lbInfosDiverses: TStaticText;
    procedure btnCalcDeclimagClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cmbModeDeclChange(Sender: TObject);
    procedure cmbUnitDeclimagChange(Sender: TObject);
    procedure editCouleurKeyPress(Sender: TObject; var Key: char);
    procedure lbColorSeanceClick(Sender: TObject);
  private
    FMyDocTopo: TToporobotStructure2012;
    procedure InitCaptions();
    procedure SetLbColor(const IdxColor: integer);
  public
    function  Initialiser(const D: TToporobotStructure2012; const MyExpe: TExpe): boolean;
    function  GetExpeFromForm(): TExpe;
  end;
  // TODO: Afficher dans editDeclimag la valeur de la déclinaison
  // calculée par WMM Geomag
  // TODO: Activer la possibilité d'utiliser le mode manuel pour la déclimag


implementation
uses
  DGCDummyUnit;

{$R *.lfm}
// appelée une seule fois dans leurs feuilles respectives
// --> initialisation du cadre effectuée en même temps
function TCdrExpe.Initialiser(const D: TToporobotStructure2012; const MyExpe: TExpe): boolean;
var
  WU: TDateTime;
begin
  result := false;
  FMyDocTopo := D;
  InitCaptions();
  editNoExpe.AsInteger    := MyExpe.IDExpe;
  WU := GetSecuredDate(MyExpe.AnneeExpe, MyExpe.MoisExpe, MyExpe.JourExpe);
  editDateSeance.Text     := DateToStr(WU, DefaultFormatSettings);
  editSpeleometre.Text    := GetResourceString(MyExpe.Operateur);
  editSpeleographe.Text   := GetResourceString(MyExpe.ClubSpeleo);
  editDeclimag.Value      := MyExpe.DeclinaisonInDegrees;
  //editInclinaison.Value   := MyExpe.Inclinaison;
  SetLbColor(MyExpe.IdxCouleur);
  cmbModeDecl.ItemIndex   := Ord(MyExpe.ModeDecl);
  editComment.Text        := _AnsiToLCLStr(Trim(MyExpe.Commentaire));
  result := true;
end;

procedure TCdrExpe.lbColorSeanceClick(Sender: TObject);
var
  EWE: Integer;
begin
  EWE := SelectionCouleurToporobot(editCouleur.AsInteger);
  SetLbColor(EWE);
end;

procedure TCdrExpe.editCouleurKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then SetLbColor(editCouleur.AsInteger);
end;



procedure TCdrExpe.btnCalcDeclimagClick(Sender: TObject);
var
  QLat, QLon, QDeclimag: double;
  DescEPSG: TLabelSystemesCoordsEPSG;
  EWE: String;
  MyCentroide: TPoint3Df;
begin
  QDeclimag   := 0.00;
  MyCentroide := FMyDocTopo.CalcCentroideEntrees();
  DescEPSG    := FMyDocTopo.GetCodeEPSGSystemeCoordonnees();
  if (ConversionCoordonneesIsoleesEPSG(DescEPSG.CodeEPSG, 4326, MyCentroide.X, MyCentroide.Y, QLat, QLon)) then
  begin
    EWE := format('%.2f, %.2f, %.2f - Lat = %.6f, Lon = %.6f - EPSG:%d',
                  [MyCentroide.X, MyCentroide.Y, MyCentroide.Z,
                   QLat, QLon,
                   DescEPSG.CodeEPSG]);
    lbInfosDiverses.Caption := EWE;
    if (CalcDeclimagIsolee(QLat, QLon, MyCentroide.Z, editDateSeance.Date, QDeclimag)) then
    begin
      case cmbUnitDeclimag.ItemIndex of
        0: editDeclimag.Value := QDeclimag;
        1: editDeclimag.Value := degtograd(QDeclimag);
      end;
    end
    else
      lbInfosDiverses.Caption := 'Echec du calcul de la déclinaison magnétique';
  end
  else
  begin
    lbInfosDiverses.Caption := 'Calcul de la déclinaison impossible: réseau non géoréférencé';
  end;
end;

procedure TCdrExpe.Button1Click(Sender: TObject);
var
  EWE: string;
begin
  EWE := editSpeleometre.Text;
  if (SelectTopographes(EWE)) then editSpeleometre.Text := EWE;
end;

procedure TCdrExpe.Button2Click(Sender: TObject);
var
  EWE: string;
begin
  EWE := editSpeleographe.Text;
  if (SelectTopographes(EWE)) then editSpeleographe.Text := EWE;
end;

procedure TCdrExpe.cmbModeDeclChange(Sender: TObject);
begin
  btnCalcDeclimag.Enabled := (cmbModeDecl.ItemIndex = Ord(cdmMANUEL));
end;

procedure TCdrExpe.cmbUnitDeclimagChange(Sender: TObject);
begin

end;

procedure TCdrExpe.InitCaptions();
begin
 lblSeance.Caption       := GetResourceString(rsCDR_EXPE_SEANCE);
 lblDate.Caption         := GetResourceString(rsCDR_EXPE_DATE);
 lblSpeleometre.Caption  := GetResourceString(rsCDR_EXPE_SPELEOMETRE);
 lblSpeleographe.Caption := GetResourceString(rsCDR_EXPE_SPELEOGRAPHE);
 lblDeclimag.Caption     := GetResourceString(rsCDR_EXPE_DECLIMAG);
 //lblInclin.Caption       := GetResourceString(rsCDR_EXPE_INCLIN);
 lblColor.Caption        := GetResourceString(rsCOLOR);
 lblObs.Caption          := GetResourceString(rsSELECT_LISTE_OBSERV);
 btnCalcDeclimag.Caption := GetResourceString(rsDLG_CALC_DOCALC);
 cmbUnitDeclimag.Enabled := false;
 cmbModeDecl.Enabled     := True;
 cmbUnitDeclimag.Clear;
 cmbUnitDeclimag.Items.Add(GetResourceString(rsDESC_UNITE_ANGULAIRE_DEGRES));
 cmbUnitDeclimag.Items.Add(GetResourceString(rsDESC_UNITE_ANGULAIRE_GRADES));
 cmbUnitDeclimag.ItemIndex := 0;
end;


function TCdrExpe.GetExpeFromForm(): TExpe;
var
  E: TExpe;
  YYYY, MM, DD: word;
  WU: TDateTime;
begin
  with E do
  begin
    IDExpe      := editNoExpe.AsInteger;
    WU := StrToDate(Trim(editDateSeance.Text), DefaultFormatSettings);
    DecodeDate(WU, YYYY, MM, DD);
    {$WARNING: TEXpe.DateExpe à implementer}
    JourExpe    := DD;
    MoisExpe    := MM;
    AnneeExpe   := YYYY;
    Operateur   := _LCLStrToAnsi(editSpeleometre.Text);
    ClubSpeleo  := _LCLStrToAnsi(editSpeleographe.Text);
    //Inclinaison := editInclinaison.Value;

    IdxCouleur  := editCouleur.AsInteger;
    ModeDecl    := TModeCalculDeclimag(cmbModeDecl.ItemIndex);
    DeclinaisonInDegrees := editDeclimag.Value;
    Commentaire := _LCLStrToAnsi(editComment.Text);
  end;
  Result := E;
end;
procedure TCdrExpe.SetLbColor(const IdxColor: integer);
var
  P: TPalette256;
begin
  editCouleur.AsInteger:=IdxColor;
  P := TPalette256.Create;
  try
    P.GenerateTOPOROBOTPalette();
    lbColorSeance.Color   := P.GetColorByIndex(IdxColor);
    editCouleur.AsInteger := IdxColor;
    P.Finaliser();
  finally
    FreeAndNil(P); //P.Free;
  end;
end;
end.

