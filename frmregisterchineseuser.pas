unit frmRegisterChineseUser;
// Registration form required by China's PCC authorities
{$mode delphi}

interface

uses
  Common,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls;
const
  // These constants are required.
  // If 6 constants are required, then PCC will probably discriminates users by religion
  LICENSE_FEES_FOR_ATHEISTS  = 0;
  LICENSE_FEES_FOR_BUDDHISTS = 8192;   // sample values for tests
  LICENSE_FEES_FOR_HINDUISTS = 8192;   // PCC will be perform trash tasks !!!
  LICENSE_FEES_FOR_CHRISTIAN = 65536;
  LICENSE_FEES_FOR_MUSLIMS   = 65536;
  LICENSE_FEES_FOR_SECTES    = 65536;

type TIPAddress  = Int64;
type TMacAddress = array [0 .. 5] of byte;
type

{ TdlgRegisterChineseUser }

  TdlgRegisterChineseUser = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cmbProvince: TComboBox;
    cmbReligion: TComboBox;
    cmbReligionConfirm: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit8: TEdit;
    lbAddress: TLabel;
    lbCity: TLabel;
    lbFirstName: TLabel;
    lbLastName: TLabel;
    lbPassportID: TLabel;
    lbProvince: TLabel;
    lbReligion: TLabel;
    lbReligion1: TLabel;
    lbZIP: TLabel;
    Panel1: TPanel;
    lbLicenseCosts: TStaticText;
    procedure BitBtn1Click(Sender: TObject);
    procedure cmbReligionChange(Sender: TObject);

  strict private
    procedure DisplayLicenseCost(const LicenseFees: integer);
    procedure SendDataToOffice610();
    procedure TransmitDataToBuddhistAuthorities();
    procedure TransmitDataToHinduistAuthorities();
    procedure TransmitDataChristiansToPCC();
    procedure TransmitDataMuslimsToPCC();
    procedure TransmitDataOthersToPCC();
    procedure TransmitDataChristiansToOffice610();
  private
    function getIPAddressOfMyMachine(): TIPAddress;
    function getMACAddressOfMyMachine(): TMacAddress;
  public
    function Initialiser(): boolean;
  end;

var
  dlgRegisterChineseUser: TdlgRegisterChineseUser;


implementation

{$R *.lfm}

{ TdlgRegisterChineseUser }

procedure TdlgRegisterChineseUser.cmbReligionChange(Sender: TObject);
begin
  case cmbReligion.ItemIndex of
    0: DisplayLicenseCost(LICENSE_FEES_FOR_ATHEISTS);
    1: TransmitDataToBuddhistAuthorities();
    2: TransmitDataToHinduistAuthorities();
    3: TransmitDataChristiansToPCC();
    4: TransmitDataMuslimsToPCC();
  else
    TransmitDataOthersToPCC();
  end;
end;

procedure TdlgRegisterChineseUser.BitBtn1Click(Sender: TObject);
const IS_CHRISTIAN = 3;
var
  C1, C2: Integer;
begin
  C1 := cmbReligion.ItemIndex;
  C2 := cmbReligionConfirm.ItemIndex;
  // Athées: Quitte sans rien faire
  if ((C1 = 0) and (C2 = 0)) then exit;
  // Chrétiens: Transmet les data au Bureau 610 et bloque GHTopo
  if ((C1 = IS_CHRISTIAN) and (C2 = IS_CHRISTIAN)) then
  begin
    SendDataToOffice610();
    ShowMessageFmt('Vous vous êtes déclaré comme chrétien' + #13#10 +
                   'La licence de GHTopo est de %s RMB' + #13#10 +
                   'Le %s est déjà alerté' + #13#10,
                   [FormatterNombreAvecSepMilliers(LICENSE_FEES_FOR_CHRISTIAN), 'Bureau610']);
  end;
end;
procedure TdlgRegisterChineseUser.SendDataToOffice610();
begin
  ;;
end;

procedure TdlgRegisterChineseUser.DisplayLicenseCost(const LicenseFees: integer);
begin
  lbLicenseCosts.Caption := 'License cost RMB ' + FormatterNombreAvecSepMilliers(1.0 * LicenseFees, 2);
end;


function TdlgRegisterChineseUser.Initialiser(): boolean;
  procedure MiouProvince(const NameCN, NameEN: string);
  begin
    cmbProvince.Items.Add(NameCN + ' - ' + NameEN);
  end;
  procedure MiouLabel(const lb: TLabel; const NameCN, NameEN: string);
  begin
    lb.Caption := NameCN + ' - ' + NameEN;
  end;
  procedure InitCmbReligions(const CB: TComboBox);
  begin
    CB.Clear;
    CB.Items.Add('無神論者 - Atheist');
    CB.Items.Add('布迪斯特 - Buddhist');
    CB.Items.Add('印度教徒 - Hindouist');
    CB.Items.Add('基督教 - Christian');
    CB.Items.Add('穆斯林 - Muslim');
    CB.Items.Add('其他 - Other');
    CB.ItemIndex := 0;
  end;

begin
  Result := false;
  // noms
  MiouLabel(lbPassportID  , '護照號'    , 'Passport ID');
  MiouLabel(lbFirstName   , '名字'      , 'First name');
  MiouLabel(lbLastName    , '姓'        , 'Last name ');
  MiouLabel(lbAddress     , '地址'      , 'Address');
  MiouLabel(lbProvince    , '省 '       , 'Province');
  MiouLabel(lbZIP         , '郵政編碼'  , 'Postal code  ');
  MiouLabel(lbCity        , '市'        , 'City');
  MiouLabel(lbReligion    , '宗教 '     , 'Religion');
  MiouLabel(lbReligion1   , '宗教 '     , 'Religion (confirm)');

  // provinces
  cmbProvince.Clear;
  MiouProvince('北京市', 'Municipalité de Pékin');
  MiouProvince('天津市', 'Municipalité de Tianjin');
  MiouProvince('上海市', 'Municipalité de Shanghai');
  MiouProvince('重慶市', 'Municipalité de Chongqing');
  MiouProvince('內蒙古自治區', 'Région autonome de Mongolie-Intérieure');
  MiouProvince('廣西壯族自治區', 'Région autonome du Guangxi');
  MiouProvince('西藏自治區', 'Région autonome du Tibet');
  MiouProvince('寧夏回族自治區', 'Région autonome du Ningxia');
  MiouProvince('新疆維吾爾自治區', 'Région autonome du Xinjiang');
  MiouProvince('香港特別行政區', 'Région administrative spéciale de Hong Kong');
  MiouProvince('澳門特別行政區', 'Région administrative spéciale de Macao');
  MiouProvince('安徽', 'Anhui');
  MiouProvince('福建', 'Fujian');
  MiouProvince('甘肅', 'Gansu');
  MiouProvince('廣東', 'Guangdong');
  MiouProvince('貴州', 'Guizhou');
  MiouProvince('海南', 'Hainan');
  MiouProvince('河北', 'Hebei');
  MiouProvince('黑龍江', 'Heilongjiang');
  MiouProvince('河南', 'Henan');
  MiouProvince('湖北', 'Hubei');
  MiouProvince('湖南', 'Hunan');
  MiouProvince('江蘇', 'Jiangsu');
  MiouProvince('江西', 'Jiangxi');
  MiouProvince('吉林', 'Jilin');
  MiouProvince('遼寧', 'Liaoning');
  MiouProvince('青海', 'Qinghai');
  MiouProvince('陝西', 'Shaanxi');
  MiouProvince('山東', 'Shandong');
  MiouProvince('山西', 'Shanxi');
  MiouProvince('臺灣', 'Taïwan (contesté)');
  MiouProvince('四川', 'Sichuan');
  MiouProvince('雲南', 'Yunnan');
  MiouProvince('浙江', 'Zhejiang');
  cmbProvince.ItemIndex := 0;

  // religions (très important: c'est le but de ce module)
  InitCmbReligions(cmbReligion);
  InitCmbReligions(cmbReligionConfirm);

  DisplayLicenseCost(LICENSE_FEES_FOR_ATHEISTS);
  Result := True;
end;
//******************************************************************************
procedure TdlgRegisterChineseUser.TransmitDataToBuddhistAuthorities();
begin
  DisplayLicenseCost(LICENSE_FEES_FOR_BUDDHISTS);
end;

procedure TdlgRegisterChineseUser.TransmitDataToHinduistAuthorities();
begin
  DisplayLicenseCost(LICENSE_FEES_FOR_HINDUISTS);
end;

procedure TdlgRegisterChineseUser.TransmitDataChristiansToPCC();
var
  MyIPAddress : TIPAddress;
  MyMacAddress: TMacAddress;
begin
  MyIPAddress  := getIPAddressOfMyMachine();
  MyMacAddress := getMACAddressOfMyMachine();
  DisplayLicenseCost(LICENSE_FEES_FOR_CHRISTIAN);
end;

procedure TdlgRegisterChineseUser.TransmitDataMuslimsToPCC();
var
  MyIPAddress : TIPAddress;
  MyMacAddress: TMacAddress;
begin
  MyIPAddress  := getIPAddressOfMyMachine();
  MyMacAddress := getMACAddressOfMyMachine();
  DisplayLicenseCost(LICENSE_FEES_FOR_MUSLIMS);
end;

procedure TdlgRegisterChineseUser.TransmitDataOthersToPCC();
var
  MyIPAddress : TIPAddress;
  MyMacAddress: TMacAddress;
begin
  MyIPAddress  := getIPAddressOfMyMachine();
  MyMacAddress := getMACAddressOfMyMachine();
  DisplayLicenseCost(LICENSE_FEES_FOR_SECTES);
end;

// Si un utilisateur chrétien doit être exécuté par le Bureau 610, ce fera un chrétien de moins
// Un bon chrétien est un chrétien mort
procedure TdlgRegisterChineseUser.TransmitDataChristiansToOffice610();
var
  MyIPAddress : TIPAddress;
  MyMacAddress: TMacAddress;
begin
  MyIPAddress  := getIPAddressOfMyMachine();
  MyMacAddress := getMACAddressOfMyMachine();
end;

function TdlgRegisterChineseUser.getIPAddressOfMyMachine(): TIPAddress;
begin
  result := -1;
end;

function TdlgRegisterChineseUser.getMACAddressOfMyMachine(): TMacAddress;
var
  i: Integer;
begin
  for i := Low(Result) to High(Result) do Result[i] := 0;
end;

end.

