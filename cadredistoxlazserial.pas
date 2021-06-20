unit CadreDistoXLazSerial;
// Ports séries virtuels sous Linux:
// Ouvrir un terminal en root, taper:
// # sudo usermod -a -G dialout <nom d'utilisateur>
// Dans un autre terminal (ou après avoir quitter le mode root):
// $socat -d -d PTY PTY
// ou bien:
// $socat PTY,link=/tmp/ttyS0,raw,echo=0 PTY,link=/tmp/ttyS1,raw,echo=0
// qui retourne des lignes de la forme
//
// 2019/12/29 14:39:50 socat[21911] N PTY is /dev/pts/10
// 2019/12/29 14:39:50 socat[21911] N PTY is /dev/pts/13
// 2019/12/29 14:39:50 socat[21911] N starting data transfer
// où, dans cet exemple,
// Le port à spécifier dans GHTopo est ici /dev/pts/13
// et on envoie les données dans /dev/pts/10
//
// pour armer deux ports virtuels

// Dans cette version, on trasnmet un contexte TLazSerial
// (typiquement celui de CadreDistoXNew)
// au lieu d'en créer un à la volée
// Contrôle temporel: 01/01/2020

{Bluetooth sous Linux
1. Activer les peripheriques bluetooth

rfkill unblock bluetooth && rfkill list bluetooth

2. Verifier que le device Bluetooth du PC/Portable est bien present et UP

hciconfig

3. Lister les périphériques Bluetooth

hcitool inq
hcitool scan

Un DistoX doit apparaître dans une ligne de la forme
00:13:43:08:09:63	DistoX-2395

4.Appairer la connexion (facultatif):

bluetooth-agent <pincode> <adresse_MAC_device>
Ex: bluetooth-agent 0000 00:13:43:08:09:63

La ligne suivante doit apparaître
Pincode request for device /org/bluez/7416/hci0/dev_00_13_43_08_09_63
Agent has been released

5. Activer la connexion
rfcomm connect hci0 <adresse_MAC_device>
ex: rfcomm connect hci0 00:13:43:08:09:63

La ligne suivante doit apparaître:
Connected /dev/rfcomm0 to 00:13:43:08:09:63 on channel 1

Votre DistoX est armé et prêt à être utilisé.
}
//-----------------------------------------------------------------------------
{$mode delphi}{$H+}
{$DEFINE NOT_EMBEDDED_IN_GHTOPO}
{$UNDEF NOT_EMBEDDED_IN_GHTOPO}
interface

uses
  {$IFDEF NOT_EMBEDDED_IN_GHTOPO}
  DataStructuresDistoXCommPort,
  {$ELSE}
  StructuresDonnees,
  {$ENDIF NOT_EMBEDDED_IN_GHTOPO}
  Common,
  UnitListesSimplesWithGeneriques,
  unitUtilsComposants,
  math,
  Dialogs,
  CallDialogsStdVersion,
  Graphics,
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, PairSplitter, ComCtrls, LazSerial, LazSynaSer, SynEdit, curredit, LCLType;

const
  FV = 24000.00;
  FM = 16384.00;

  MAX_IT = 500;
  EPS    = 1E-8;
  HEX_TO_RAD = 2 * PI / 65536;

  INVALID_HANDLE_VALUE = THandle(-1);

  DISTOX_COMMAND_EXTINCTION              : byte = 52;  // $34: extinction du Disto
  DISTOX_COMMAND_READ_MEMORY_AT_ADDRESS  : byte = 56;  // $38: b111000 ; conflict with measure trigging command

  DISTOX_COMMAND_MODE_CALIBRATION_ON     : byte = 49;  // $30: b110001:
  DISTOX_COMMAND_MODE_CALIBRATION_OFF    : byte = 48;  // $31: b110000:
  DISTOX_COMMAND_LASER_ON                : byte = 54;  // $36: b110110: Laser ON
  DISTOX_COMMAND_LASER_OFF               : byte = 55;  // $37: b110111: Laser OFF
  DISTOX_COMMAND_SCAN_CONTINU_ON         : byte = 54;  // en attente TODO: Valeurs a confirmer
  DISTOX_COMMAND_SCAN_CONTINU_OFF        : byte = 55;  // en attente
  ///!\ Le mode Silencieux ne transmet pas les données
  //DISTOX_COMMAND_BEEP_ON                = $33;  // $33: Silent ON  = à ne pas utiliser
  //DISTOX_COMMAND_BEEP_OFF               = $32;  // $33: Silent OFF
  DISTOX_COMMAND_TRIG_DATA_SHOT          : byte =  56;  // $38: lecture d'une visée;    conflict with measure trigging command
  DISTOX_COMMAND_BOOTLOADER_READ_DATA_AT_ADDRESS : byte =  58;  // $38: b111010 ;

const NB_MESURES_ACQUISES_POUR_UNE_VISEE = 3;

type TBufferOf8Bytes        = array[0..7]   of byte;
type TBufferBootLoaderBlock = array[0..263] of byte;


type

{ TCdrDistoXLazserial }

 TCdrDistoXLazserial = class(TFrame)
    btnDemanderMesure: TButton;
    btnHelp: TButton;
    btnListerMesures: TButton;
    btnQSAddVisee: TButton;
    btnRemoveLastVisee: TButton;
    Button2: TButton;
    Button3: TButton;
    chkAutodetectViseesCheminement: TCheckBox;
    cmbPortsCom: TComboBox;
    editQSAzimut: TCurrencyEdit;
    editQSLongueur: TCurrencyEdit;
    editQSPente: TCurrencyEdit;
    hcColsTitres: THeaderControl;
    lbAzimut: TLabel;
    lbTamponLine0: TLabel;
    lbPort: TLabel;
    lbLongueur: TLabel;
    lbPente: TLabel;
    LazSerial1: TLazSerial;
    lbConnexionStatus: TStaticText;
    lbDistoXName: TStaticText;
    lbTamponLine1: TLabel;
    lbTamponLine2: TLabel;
    lsbMesures: TListBox;
    PageControlDistoX: TPageControl;
    btnDXScanOnOff: TToggleBox;
    pnlTamponMesures: TPanel;
    pnlQuickSaisieVisee: TPanel;
    pnlUtilitaires: TPanel;
    btnOpenCloseConnexion: TToggleBox;
    SynEdit1: TSynEdit;
    tabShtListeMesures: TTabSheet;
    tabShtJournal: TTabSheet;
    tabShtMisc: TTabSheet;
    procedure btnDXScanOnOffChange(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnListerMesuresClick(Sender: TObject);
    procedure btnOpenCloseConnexionChange(Sender: TObject);
    procedure btnQSAddViseeClick(Sender: TObject);
    procedure btnSendBytesClick(Sender: TObject);
    procedure btnTestProcTransmitMesureClick(Sender: TObject);
    procedure btnDemanderMesureClick(Sender: TObject);
    procedure btnRemoveLastViseeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chkAutodetectViseesCheminementChange(Sender: TObject);
    procedure editQSLongueurChange(Sender: TObject);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure LazSerial1RxData(Sender: TObject);
    procedure LazSerial1Status(Sender: TObject; Reason: THookSerialReason; const Value: string);
    procedure lsbMesuresDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);

  strict private
    FDistoX_Actif : boolean;
    // Tableau des n dernières mesures acquises
    FLesDernieresMesures    : array[0 .. NB_MESURES_ACQUISES_POUR_UNE_VISEE - 1]  of TMesureViseeDistoX;
    // En provenance de TPiloteDistoX. On regroupe tout ici
    FLastMesureVisee        : TMesureViseeDistoX;
    FDistoAutoDetectViseesCheminement: boolean; // Le DistoX est en détection automatique de visées

    FDistoXSerialNumber     : TDistoXSerialNumber;
    FDistoXVersionFirmware  : TDistoXVersionFirmware;
    FDistoXVersionHardware  : TDistoXVersionHardware;

    FOldType: integer;
    FOldX   : integer;
    FOldY   : integer;
    FOldZ   : Integer;
    // Callback pour le traitement de la visée reçue
    FProcTransmitMesureDistoX: TProcTransmitMesureDistoX;
    function DistoX_GetDescriptionDevice(): string;
    function DistoX_ExtractNumSerie()       : TDistoXSerialNumber;
    function DistoX_ExtractVersionFirmWare(): TDistoXVersionFirmware;
    function DistoX_ExtractVersionHardware(): TDistoXVersionHardware;
    // commandes du DistoX
    function  MakeBackupLine(const M: TMesureViseeDistoX): string;
    function  DistoX_Acknowledge(const QTypeData: byte): boolean;
    function  DistoX_LireEtDecoderBuffer8Bytes(out MyOP: byte;
                                               out MyMesureVisee: TMesureViseeDistoX;
                                               out MyVectorData, MyGCalibrationData, MyMCalibrationData: TVectorDataDistoX): boolean;
    function  DistoX_SendReadCommandAtAddress(const QCmd: byte; const QAddress: word): boolean;
    function  DistoX_ReadBufferOf8BytesAtAddress(const QAddress: word; out MyBuffer: TBufferOf8Bytes): boolean;
    procedure ReceptionnerEtTraiterVisee(const M: TMesureViseeDistoX);

    procedure SendBytes(const Bytes: array of Byte);
    procedure SendText(const S: string);

    procedure SetModeCalibrationOnOff(const b: boolean);
    procedure SetDistoXScanContinuous(const b: boolean);

    procedure ListerLesPorts();
    // tampon de mesures
    procedure EnfilerMesureDansTampon(const M: TMesureViseeDistoX);
    procedure InitTamponMesures();
    // =========================================================================
  private
    // Station courante
    // Liste des mesures DistoX
    FListeMesuresTopoDistoX: TListeMesuresTopoDistoX;
    // Nom du fichier backup
    FBackUpMesuresFileName: string;
    // PRECISION DES MESURES TOPO
    FUniteDuDistoX                   : double;
    FDistoXAccuracyLongueurs         : double;          // longueur
    FDistoXAccuracyAzimuts           : double;          // azimut
    FDistoXAccuracyPentes            : double;          // inclinaison
    FDoAutoDetectViseeCheminement      : boolean;
    function  Connecter(): integer;
    procedure AddMesureVisee(const M: TMesureViseeDistoX); overload;

    function  GetMesureVisee(const Idx: integer): TMesureViseeDistoX;
    function  GetNbMesuresVisees(): integer;

    procedure PutMesureVisee(const Idx: integer; const M: TMesureViseeDistoX);
    procedure QAfficherMessage(const Msg: string);
    procedure RemoveLastMesuresVisees(const Nb: integer);
    procedure RemoveMesureVisee(const Idx: integer);
    procedure SaveMesuresDistoX(const QFilename: string);
    procedure SetAccuracies(const AccLong, AccAz, AccP: double);
    procedure ListerLesMesures();

  public
    function  Initialiser(const P: TProcTransmitMesureDistoX): boolean;
    procedure SetProcTransmitMesureDistoX(const P: TProcTransmitMesureDistoX);
    procedure Finaliser();
    // Connecte ou reconnecte le DistoX
    procedure ReConnecterLeDistoX();
    procedure Deconnecter();
    procedure DemanderUneMesure();

  end;

implementation
uses
  DGCDummyUnit;
{$R *.lfm}


const
  HC_COL_HORODATAGE   = 190;
  HC_COL_DEVICE       =  80;
  HC_COL_TYPE_MESURE  = 130;
  HC_COL_LONGUEUR     =  70;
  HC_COL_AZIMUT       =  70;
  HC_COL_PENTE        =  70;

function MakeEmptyTBufferOf8Bytes(): TBufferOf8Bytes;
var
  i: Integer;
begin
  for i := 0 to High(Result) do Result[i] := 0;
end;
function MakeEmptyTBufferBootLoaderBlock(): TBufferBootLoaderBlock;
var
  i: Integer;
begin
  for i := 0 to High(Result) do Result[i] := 0;
end;
{ TCdrDistoXLazserial }
function TCdrDistoXLazserial.MakeBackupLine(const M: TMesureViseeDistoX): string;
begin
  Result := Format('%s; %d;  %10.3f; %9.3f; %9.3f;', [DateTimePascalToDateTimeSQL(M.TimeStamp), M.Device, M.Longueur, M.Azimut, M.Pente]);
end;


procedure TCdrDistoXLazserial.SetProcTransmitMesureDistoX(const P: TProcTransmitMesureDistoX);
begin
  FProcTransmitMesureDistoX := P;
end;

procedure TCdrDistoXLazserial.InitTamponMesures();
var
  i: Integer;
  procedure MiouMiou(const LB: TLabel; const idx: integer);
  begin
    LB.Caption := Format('%d: %.3f : %.3f : %.3f', [idx ,FLesDernieresMesures[idx].Longueur, FLesDernieresMesures[idx].Azimut, FLesDernieresMesures[idx].Pente]);
  end;
begin
  for i := 0 to NB_MESURES_ACQUISES_POUR_UNE_VISEE - 1 do
  begin
    FLesDernieresMesures[i].Device          := FDistoXSerialNumber;
    FLesDernieresMesures[i].TimeStamp       := Now();
    FLesDernieresMesures[i].Longueur        :=  0.00;
    FLesDernieresMesures[i].Azimut          :=  0.00;
    FLesDernieresMesures[i].Pente           :=  0.00;
  end;
  MiouMiou(lbTamponLine0, 0);
  MiouMiou(lbTamponLine1, 1);
  MiouMiou(lbTamponLine2, 2);
end;

function TCdrDistoXLazserial.Initialiser(const P: TProcTransmitMesureDistoX): boolean;
begin
  result := false;
  pnlUtilitaires.Visible             := false;
  FDistoX_Actif                      := false;

  FDoAutoDetectViseeCheminement      := true;
  chkAutodetectViseesCheminement.Checked := FDoAutoDetectViseeCheminement;
  FUniteDuDistoX                     := 360.00;
  FDistoXSerialNumber                := 0;
  FDistoXVersionFirmware             := 0;
  FDistoXVersionHardware             := 0;
  FOldType                           := 0;
  FOldX                              := 0;
  FOldY                              := 0;
  FOldZ                              := 0;
  lbPort.Caption                     := 'Port';

  FDistoAutoDetectViseesCheminement  := True;
  //btnDXScanOnOff.Checked             := FDistoXIsScanning; // Le DistoX est en scan continu ?
  //btnDXScanOnOff.Caption             := 'DistoX Scan ' + IIF(FDistoXIsScanning, 'ON', 'OFF');
  FBackUpMesuresFileName             := '_BackupMesuresDistoX.csv';

  ListerLesPorts();
  tabShtJournal.Caption              := 'Console';
  tabShtMisc.Caption                 := 'Divers';

  SetProcTransmitMesureDistoX(P);
  InitTamponMesures();
  FListeMesuresTopoDistoX := TListeMesuresTopoDistoX.Create;
  try
    hcColsTitres.Sections[0].Width := HC_COL_HORODATAGE;
    hcColsTitres.Sections[1].Width := HC_COL_DEVICE;
    hcColsTitres.Sections[2].Width := HC_COL_TYPE_MESURE;
    hcColsTitres.Sections[3].Width := HC_COL_LONGUEUR;
    hcColsTitres.Sections[4].Width := HC_COL_AZIMUT;
    hcColsTitres.Sections[5].Width := HC_COL_PENTE;
    FListeMesuresTopoDistoX.ClearListe();
    ListerLesMesures();
    SynEdit1.Clear;
    result := True;
  except
  end;
  PageControlDistoX.ActivePageIndex  := 0;
end;


procedure TCdrDistoXLazserial.Finaliser();
begin
  Deconnecter();
  try
    FListeMesuresTopoDistoX.ClearListe();
  finally
    FreeAndNil(FListeMesuresTopoDistoX);
  end;
end;

function TCdrDistoXLazserial.Connecter(): integer;
var
  EWE: String;
begin
  Result := -1;
  QAfficherMessage('Connexion en cours (durée prévisible: une trentaine de secondes ...');
  Deconnecter(); // par sécurité
  FDistoX_Actif := false;
  try
    LazSerial1.Open;
    FDistoXSerialNumber     := DistoX_ExtractNumSerie();
    FDistoXVersionFirmware  := DistoX_ExtractVersionFirmWare();
    FDistoXVersionHardware  := DistoX_ExtractVersionHardware();
    EWE := Format('DistoX%d - FW = %d - HW = %d', [FDistoXSerialNumber, FDistoXVersionFirmware, FDistoXVersionHardware]);
    QAfficherMessage(EWE);
    lbDistoXName.Caption := EWE;
    Result := LazSerial1.SynSer.LastError;
    QAfficherMessage(Format('Dernier retour: %d', [Result]));
    FDistoX_Actif := (0 = Result);
    SetModeCalibrationOnOff(false);   // L'armement de la connexion du DistoX peut activer le mode CAL. Ceci le désactive
    // (commande manuelle sur le disto:
    {
    Les touches et combinaisons de touches suivantes peuvent être utilisées pour modifier les différents paramètres en
appuyant pendant deux secondes.
MEM : changer l’unité de distance (m/ft/inch)
REF : référence permanente de distance face avant
CLR : mise hors tension
MEM et SMART: changer l’unité d'angle: degré/grade
MEM et FUNC : mode silencieux on/off
REF et MOINS : bip on/off
REF et PLUS : rétroéclairage de l'écran on/off
REF et FUNC : réf. distance : arrière boitier/embout
CLR et SMART : mode calibrage on/off
CLR et MEM : effacement des données non transmises
CLR et FUNC : Bluetooth on/off
CLR et MOINS : verrouillage sur DistoX2 éteint
    // }
    QAfficherMessage(BoolToStr(FDistoX_Actif, 'Connecté', 'Déconnecté'));
  except
    QAfficherMessage(Format('*** Erreur %d - %s', [LazSerial1.SynSer.LastError, LazSerial1.SynSer.LastErrorDesc]));
    Deconnecter();
  end;
end;

procedure TCdrDistoXLazserial.Deconnecter();
begin
  FDistoX_Actif := false;
  LazSerial1.Close;
end;

procedure TCdrDistoXLazserial.SetAccuracies(const AccLong, AccAz, AccP: double);
begin
  FDistoXAccuracyLongueurs  := AccLong;
  FDistoXAccuracyAzimuts    := AccAz;
  FDistoXAccuracyPentes     := AccP;
end;

procedure TCdrDistoXLazserial.ListerLesMesures();
var
  Nb, i: Integer;
begin
  lsbMesures.Clear;
  Nb := GetNbMesuresVisees();
  if (Nb = 0) then exit;
  for i := 0 to Nb - 1 do lsbMesures.Items.Add('');
  lsbMesures.ItemIndex := Nb-1;
end;



procedure TCdrDistoXLazserial.AddMesureVisee(const M: TMesureViseeDistoX);
var
  EWE: String;
begin
  if (M.Longueur > 0.00001) then
  begin
    EnfilerMesureDansTampon(M);
    FListeMesuresTopoDistoX.AddElement(M);
    // ajouter mesure à la liste
    EWE := MakeBackupLine(M);
    lsbMesures.Items.Add(EWE);
    lsbMesures.ItemIndex := lsbMesures.Count - 1;
    tabShtListeMesures.Caption := format('Mesures [%d]', [lsbMesures.Items.Count]);
    QAfficherMessage(EWE); // et à la console
  end;
end;

function TCdrDistoXLazserial.GetMesureVisee(const Idx: integer): TMesureViseeDistoX;
begin
  Result := FListeMesuresTopoDistoX.GetElement(Idx);
end;

procedure TCdrDistoXLazserial.PutMesureVisee(const Idx: integer; const M: TMesureViseeDistoX);
begin
  FListeMesuresTopoDistoX.PutElement(Idx, M);
end;

procedure TCdrDistoXLazserial.RemoveMesureVisee(const Idx: integer);
begin
  FListeMesuresTopoDistoX.RemoveElement(Idx);
end;

procedure TCdrDistoXLazserial.RemoveLastMesuresVisees(const Nb: integer);
var
  i: Integer;
begin
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    if (0 = GetNbMesuresVisees()) then exit;
    self.RemoveMesureVisee(GetNbMesuresVisees() - 1);
  end;
end;

function TCdrDistoXLazserial.GetNbMesuresVisees(): integer;
begin
  Result := FListeMesuresTopoDistoX.GetNbElements();
end;

procedure TCdrDistoXLazserial.SaveMesuresDistoX(const QFilename: string);
var
  fp: TextFile;
  EWE: String;
  Nb, i: Integer;
  M: TMesureViseeDistoX;
begin
  Nb := GetNbMesuresVisees();
  if (Nb = 0) then Exit;
  AssignFile(fp, QFilename);
  try
    Rewrite(fp);
    Writeln(fp, QFilename + ' ' + DateTimePascalToDateTimeSQL(Now));
    EWE := 'Horodate; Numero DistoX; Serie; Station; TypeVisee; Longueur; Azimut; Pente;';
    WriteLn(fp, EWE);
    for i := 0 to Nb - 1 do
    begin
      M := GetMesureVisee(i);
      EWE := MakeBackupLine(M);
      WriteLn(fp, EWE);
    end;
  finally
    Closefile(fp);
  end;
end;
//******************************************************************************
procedure TCdrDistoXLazserial.EnfilerMesureDansTampon(const M: TMesureViseeDistoX);
var
  i: Integer;
  procedure MiouMiou(const LB: TLabel; const idx: integer);
  begin
    LB.Caption := Format('%d: %.3f : %.3f : %.3f', [idx ,FLesDernieresMesures[idx].Longueur, FLesDernieresMesures[idx].Azimut, FLesDernieresMesures[idx].Pente]);
  end;
begin
  // Etat initial:
  //[ 0  :  1  :  2  ]
  //[444 : 555 : 666 ]
  // On décale les mesures:
  for i := 0 to NB_MESURES_ACQUISES_POUR_UNE_VISEE - 2 do FLesDernieresMesures[i] := FLesDernieresMesures[i+1];
  //[ 0  :  1  :  2  ]
  //[555 : 666 : ??? ]
  // et met la mesure  dans la dernière case
  //[ 0  :  1  :  2  ]
  //[555 : 666 : 888 ]
  FLesDernieresMesures[NB_MESURES_ACQUISES_POUR_UNE_VISEE - 1] := M;
  MiouMiou(lbTamponLine0, 0);
  MiouMiou(lbTamponLine1, 1);
  MiouMiou(lbTamponLine2, 2);
end;

procedure TCdrDistoXLazserial.ReceptionnerEtTraiterVisee(const M: TMesureViseeDistoX);
var
  MyMesure: TMesureViseeDistoX;
  n: Integer;
begin
  MyMesure := M;
  if (MyMesure.Longueur > 0.0001) then
  begin
    MyMesure.Device          := FDistoXSerialNumber;
    AddMesureVisee(MyMesure);
    // Cas où le nombre de mesures est inférieur à la taille du tampon: C'est une visée radiante
    n := GetNbMesuresVisees();
    if (GetNbMesuresVisees() < NB_MESURES_ACQUISES_POUR_UNE_VISEE) then
    begin
      if (Assigned(FProcTransmitMesureDistoX)) then FProcTransmitMesureDistoX(MyMesure, tvdRADIANTE);
      exit;
    end;
    if (FDistoAutoDetectViseesCheminement) then // Si on est en mode scan, on ne fait qu'ajouter la visée aux visées radiantes
    begin
      if (CalcAzimutMoyenOfTMesuresViseeDistoX(360.00, 360.00, 0.05, 1.00, FLesDernieresMesures, MyMesure)) then
      begin
        QAfficherMessage('');
        QAfficherMessage(Format('*** Visée de cheminement *** %.3f : %.3f : %.3f',[ MyMesure.Longueur, MyMesure.Azimut, MyMesure.Pente]));
        if (Assigned(FProcTransmitMesureDistoX)) then FProcTransmitMesureDistoX(MyMesure, tvdCHEMINEMENT);
        InitTamponMesures();  // et on vide le tampon
      end
      else
      begin
        if (Assigned(FProcTransmitMesureDistoX)) then FProcTransmitMesureDistoX(MyMesure, tvdRADIANTE);
      end;
    end
    else
    begin
      if (Assigned(FProcTransmitMesureDistoX)) then FProcTransmitMesureDistoX(MyMesure, tvdRADIANTE);
    end; //  if (FDistoAutoDetectViseesCheminement)
  end; //  if (MyMesure.Longueur > 0.0001) then
  Application.ProcessMessages;
end;
//******************************************************************************


procedure TCdrDistoXLazserial.LazSerial1RxData(Sender: TObject);
var
  QMyOp: byte;
  MyMesureDistoX: TMesureViseeDistoX;
  QVectorData, GCalibrationData, QMCalibrationData: TVectorDataDistoX;
begin
  if (FDistoX_Actif) then
  begin
    DistoX_LireEtDecoderBuffer8Bytes(QMyOp, MyMesureDistoX, QVectorData, GCalibrationData, QMCalibrationData);
    ReceptionnerEtTraiterVisee(MyMesureDistoX);
  end;
end;


procedure TCdrDistoXLazserial.LazSerial1Status(Sender: TObject; Reason: THookSerialReason; const Value: string);
begin
  case Reason of
    HR_SerialClose: // 0
      begin
        //QAfficherMessage(Format('%s: [%d] - %s', ['HR_Serial_Close', Ord(Reason), Value]));
        lbConnexionStatus.caption := Format('%s est fermé', [Value]);
      end;
    HR_Connect:     //
      begin
        //QAfficherMessage(Format('%s: [%d] - %s', ['HR_Connect', Ord(Reason), Value]));
        lbConnexionStatus.caption := Format('%s est ouvert', [Value]);
      end;
    HR_CanRead: // <-- Freeze aléatoire de l'application
      begin
        //lbConnexionStatus.caption := Format('%s: [%d] - %s', ['HR_CanRead', Ord(Reason), Value]);
      end;
    HR_CanWrite:
      begin
        //lbConnexionStatus.caption := Format('%s: [%d] - %s', ['HR_CanWrite', Ord(Reason), Value]);
      end;
    HR_ReadCount:
      begin
        //lbConnexionStatus.caption := Format('%s: [%d] - %s', ['HR_ReadCount', Ord(Reason), Value]);
      end;
    HR_WriteCount:
      begin
        //lbConnexionStatus.caption := Format('%s: [%d] - %s', ['HR_WriteCount', Ord(Reason), Value]);
      end;
    HR_Wait:
      begin
        lbConnexionStatus.caption := Format('%s; [%d] - %s', ['HR_Wait', Ord(Reason), Value]);
      end;
  end;
  //Application.ProcessMessages;
end;

procedure TCdrDistoXLazserial.lsbMesuresDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  V: TMesureViseeDistoX;
  EWE: String;
  BGC: TColor;
  procedure DessineItem(const bg,tc: TColor);
  begin
    with lsbMesures do
    begin
      ResetColorRow(lsbMesures, ARect, bg, tc);
      DrawColTexte(lsbMesures, ARect, hcColsTitres.Sections.Items[0], False,  DateTimePascalToDateTimeSQL(V.TimeStamp, True));
      DrawColTexte(lsbMesures, ARect, hcColsTitres.Sections.Items[1], True, Format('%d', [V.Device]));
      DrawColTexte(lsbMesures, ARect, hcColsTitres.Sections.Items[2], True, EWE);
      DrawColTexte(lsbMesures, ARect, hcColsTitres.Sections.Items[3], True, FormatterNombreOOo(V.Longueur, 3));
      DrawColTexte(lsbMesures, ARect, hcColsTitres.Sections.Items[4], True, FormatterNombreOOo(V.Azimut, 3));
      DrawColTexte(lsbMesures, ARect, hcColsTitres.Sections.Items[5], True, FormatterNombreOOo(V.Pente, 3));
    end;
  end;
begin
  if (0 = GetNbMesuresVisees()) then exit;
  V := GetMesureVisee(Index);
  EWE := ''; //ChooseString(ord(V.TypeViseeDistoX), ['UNDEFINED', 'RADIANTE', 'CHEMINEMENT', 'MOYENNEE']);
  BGC := clWhite;
  if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(BGC, clBlack);
end;


//------------------------------------------------------------------------------

procedure TCdrDistoXLazserial.QAfficherMessage(const Msg: string);
begin
  {$IFDEF LINUX}
    WriteLn(Msg);
    //{$IFDEF RASPBERRY_PI}
    SynEdit1.Append(Msg);
    SynEdit1.CaretY := SynEdit1.lines.Count;
    //{$ENDIF}

  {$ENDIF}
  {$IFDEF MSWINDOWS}
  SynEdit1.Append(Msg);
  SynEdit1.CaretY := SynEdit1.lines.Count;
  {$ENDIF}
  Application.ProcessMessages;
end;


procedure TCdrDistoXLazserial.ReConnecterLeDistoX();
const
  TIMEOUT_IN_SECONDS = 10;
var
  MyPort: TCaption;
begin
  MyPort := trim(cmbPortsCom.Text);
  InitTamponMesures();
  LazSerial1.Device := MyPort;
  LazSerial1.SynSer.AtTimeout       := TIMEOUT_IN_SECONDS * 1000;
  LazSerial1.SynSer.DeadlockTimeout := TIMEOUT_IN_SECONDS * 1000;
  Connecter();
end;



procedure TCdrDistoXLazserial.btnQSAddViseeClick(Sender: TObject);
var
  MyMesureDistoX: TMesureViseeDistoX;
begin
  MyMesureDistoX.Device          := FDistoXSerialNumber;
  MyMesureDistoX.TimeStamp       := Now();
  MyMesureDistoX.Longueur        := editQSLongueur.Value;
  MyMesureDistoX.Azimut          := editQSAzimut.Value;
  MyMesureDistoX.Pente           := editQSPente.Value;
  ReceptionnerEtTraiterVisee(MyMesureDistoX);

end;

procedure TCdrDistoXLazserial.btnSendBytesClick(Sender: TObject);
begin
  DemanderUneMesure();
end;

procedure TCdrDistoXLazserial.btnTestProcTransmitMesureClick(Sender: TObject);
begin
  pass;
end;


procedure TCdrDistoXLazserial.btnDemanderMesureClick(Sender: TObject);
begin
  DemanderUneMesure();
end;
procedure TCdrDistoXLazserial.btnRemoveLastViseeClick(Sender: TObject);
begin
  FListeMesuresTopoDistoX.RemoveLastElement();
  ListerLesMesures();
end;

procedure TCdrDistoXLazserial.Button2Click(Sender: TObject);
var
  DossierHorodatage, QFilename : TStringDirectoryFilename;
begin
  DossierHorodatage := ExtractFilePath(ParamStr(0)) + '0_HorodatageMesures';
  ForceDirectories(DossierHorodatage);
  QFilename := DossierHorodatage + PathDelim + MakeFilenameFromDate('Sauvegrd_data_DistoX_', Now(), 'csv');
  SaveMesuresDistoX(QFilename);
end;

procedure TCdrDistoXLazserial.Button3Click(Sender: TObject);
begin
  pnlUtilitaires.Visible := not pnlUtilitaires.Visible;
end;

procedure TCdrDistoXLazserial.chkAutodetectViseesCheminementChange(Sender: TObject);
begin
  FDoAutoDetectViseeCheminement := chkAutodetectViseesCheminement.Checked;
end;

procedure TCdrDistoXLazserial.editQSLongueurChange(Sender: TObject);
begin

end;

procedure TCdrDistoXLazserial.hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbMesures.Invalidate;
end;


procedure TCdrDistoXLazserial.btnDXScanOnOffChange(Sender: TObject);
begin
  //FDistoXIsScanning := btnDXScanOnOff.Checked;
  //SetDistoXScanContinuous(FDistoXIsScanning);
  //btnDXScanOnOff.Caption := 'DistoX Scan ' + IIF(FDistoXIsScanning, 'ON', 'OFF');
end;



procedure TCdrDistoXLazserial.btnHelpClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    DisplayHelpSystem('DISTOX_CONNEXION_WINDOWS');
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
    DisplayHelpSystem('DISTOX_CONNEXION_LINUX');
  {$ENDIF LINUX}
end;

procedure TCdrDistoXLazserial.btnListerMesuresClick(Sender: TObject);
begin
  ListerLesMesures();
end;

procedure TCdrDistoXLazserial.btnOpenCloseConnexionChange(Sender: TObject);
begin
  if (btnOpenCloseConnexion.Checked) then
  begin
    {$IFDEF LINUX}
    ReConnecterLeDistoX()
    {$ELSE}
    showmessage('Connexion')
    {$ENDIF};
    // MAJ du titre du bouton
    btnOpenCloseConnexion.Caption := 'Déconnecter';
  end else
  begin
    {$IFDEF LINUX}
    Deconnecter()
    {$ELSE}
    showmessage('Déconnexion');
    {$ENDIF};
    // MAJ du titre du bouton
    btnOpenCloseConnexion.Caption := 'Connecter';
  end;
end;

//==============================================================================
// Tout ceci provient du pilote DistoX.
function TCdrDistoXLazserial.DistoX_ExtractNumSerie(): TDistoXSerialNumber;
var
  MyBuffer: TBufferOf8Bytes;
begin
  if (FDistoXSerialNumber = 0) then          // si FDistoXSerialNumber = 0, on fait une tentative d'extraction;
  begin
    MyBuffer := MakeEmptyTBufferOf8Bytes();
    if (DistoX_ReadBufferOf8BytesAtAddress($8008, MyBuffer)) then
      FDistoXSerialNumber := (MyBuffer[3] + MyBuffer[4] shl 8) AND $FFFF; // retourne 2395, 2329 et 4152 pour les DistoX de JPC
  end;
  Result := FDistoXSerialNumber;
end;
// version de firmware
function TCdrDistoXLazserial.DistoX_ExtractVersionFirmWare(): TDistoXVersionFirmware;
var
  MyBuffer: TBufferOf8Bytes;
begin
  if (FDistoXVersionFirmware = 0) then // si FDistoXVersionFirmware = 0, on fait une tentative d'extraction;
  begin
    MyBuffer := MakeEmptyTBufferOf8Bytes();
    if (DistoX_ReadBufferOf8BytesAtAddress($E000, MyBuffer)) then
      FDistoXVersionFirmware := (100 * MyBuffer[3] + MyBuffer[4]) AND $FFFF;
  end;
  Result := FDistoXVersionFirmware;
end;
// version de hardware
function TCdrDistoXLazserial.DistoX_ExtractVersionHardware(): TDistoXVersionHardware;
var
  MyBuffer: TBufferOf8Bytes;
begin
  if (FDistoXVersionHardware = 0) then
  begin
    MyBuffer := MakeEmptyTBufferOf8Bytes();
    if (DistoX_ReadBufferOf8BytesAtAddress($E004, MyBuffer)) then
      FDistoXVersionHardware := (10 * MyBuffer[3] + MyBuffer[4]) AND $FFFF;     // byte 3 = major; byte 4 = minor, bytes 5 et 6 = 0
  end;
  Result := FDistoXVersionHardware;
end;
function TCdrDistoXLazserial.DistoX_GetDescriptionDevice(): string;
begin
  Result := Format('Port: %s - Device: %d - Hardware: %d - Firmware: %d',
                   [LazSerial1.Device,
                    DistoX_ExtractNumSerie(),
                    DistoX_ExtractVersionHardware(),
                    DistoX_ExtractVersionFirmWare()
                   ]);
end;
// Fonctions bas niveau de lecture/écriture
function TCdrDistoXLazserial.DistoX_Acknowledge(const QTypeData: byte): boolean;
var
  MyByte: Byte;
begin
  result := false;
  begin
    MyByte := (QTypeData and $80) or $55;
    LazSerial1.SynSer.SendByte(MyByte);
    Result := (0 = LazSerial1.SynSer.LastError);
  end;
end;

function TCdrDistoXLazserial.DistoX_SendReadCommandAtAddress(const QCmd: byte; const QAddress: word): boolean;
var
  MyBuffer: TBufferOf8Bytes;
begin
  //if (not self.SynSer.CanWrite(FTimeOutInMilliseconds)) then exit(false);
  MyBuffer    := MakeEmptyTBufferOf8Bytes();
  MyBuffer[0] :=  QCmd;                       // DISTOX_COMMAND_READ_DATA = 56; // 00111000
  MyBuffer[1] :=  QAddress and $FF;           // Address & 0xFF
  MyBuffer[2] := (QAddress shr 8) and $FF;    // (Address >> 8) & 0xFF
  LazSerial1.SynSer.SendBuffer(@MyBuffer, 3);
  result := (0 = LazSerial1.SynSer.LastError);
end;

function TCdrDistoXLazserial.DistoX_ReadBufferOf8BytesAtAddress(const QAddress: word; out MyBuffer: TBufferOf8Bytes): boolean;
begin
  Result := false;
  QAfficherMessage('ReadBufferOf8BytesAtAddress');
  if (DistoX_SendReadCommandAtAddress(DISTOX_COMMAND_READ_MEMORY_AT_ADDRESS, QAddress)) then
  begin
    LazSerial1.SynSer.RecvBuffer(@MyBuffer, 8);
    Result := (0 = LazSerial1.SynSer.LastError);
  end;
end;
// Dans cette fonction, on extrait une trame de 8 octets
// S'il s'agit de données de mesures topo, on acquitte.
// En cas de succès de l'acquittement, on décode
// En cas de succès du décodage, l'appelant peut utiliser la visée
// 01/01/2020: Ceci est validé
function TCdrDistoXLazserial.DistoX_LireEtDecoderBuffer8Bytes(out MyOP: byte;
                                                              out MyMesureVisee: TMesureViseeDistoX;
                                                              out MyVectorData, MyGCalibrationData, MyMCalibrationData: TVectorDataDistoX): boolean;
const MAX_WORD = 65535;
var
  i          : Integer;
  MyBuffer   : TBufferOf8Bytes;
  QTypeData  : Byte;
  QX, QY, QZ, Dist: word;
  P          : double;
  QMesureIValide: Boolean;
  function CheckRanges(const FX, FY, FZ: word): boolean; inline;
  begin
    Result := InRange(FX, 0, MAX_WORD) and InRange(FY, 0, MAX_WORD) and InRange(FZ, 0, MAX_WORD);
  end;
begin
  Result := false;
  begin
    try
      MyBuffer := MakeEmptyTBufferOf8Bytes();
      LazSerial1.SynSer.RecvBuffer(@MyBuffer, 8);
      //MyMesureVisee.DistoXSerialNumber := FDistoXSerialNumber;
      MyMesureVisee.Longueur := 0.00;
      MyMesureVisee.Azimut   := 0.00;
      MyMesureVisee.Pente    := 0.00;
      //MyMesureVisee.HexaData := '';
      QTypeData := MyBuffer[0];
      MyOp       := (QTypeData and $3F);
      // Ce sont des données ?
      if (MyOp < $20) then
      begin
        if (DistoX_Acknowledge(QTypeData)) then   // Indispensable juste après le test (QOp < $20), sinon, KO
        begin
          QX := MyBuffer[1] + (MyBuffer[2] shl 8);
          QY := MyBuffer[3] + (MyBuffer[4] shl 8);
          QZ := MyBuffer[5] + (MyBuffer[6] shl 8);
          if ((Foldtype <> QTypeData) and (FoldX <> QX) and (FoldY <> QY) and (FoldZ <> QZ)) then
          begin
            case (MyOp) of
              1: // données topo
              begin
                // distances exprimées en mm si < 100 m, sinon en cm
                Dist := QX + ((QTypeData and $40) shr 10);     // OK
                MyMesureVisee.Longueur := Dist * 0.001; // TODO: Cas où Dist > 100 m
                myMesureVisee.Azimut   := radtodeg(QY * HEX_TO_RAD);                   // OK
                P  := radtodeg(QZ * HEX_TO_RAD);
                if (Not InRange(P, -0.001, 0.25 * UNITE_ANGULAIRE_PAR_DEFAUT + 0.001)) then P := P - UNITE_ANGULAIRE_PAR_DEFAUT;
                myMesureVisee.Pente := P;
                //myMesureVisee.HexaData := Datagram;
                myMesureVisee.TimeStamp:= Now();
                // On vire les visées de longueur nulle (qui seront rejetées par GHTopo de toutes façons)
                QMesureIValide := (MyMesureVisee.Longueur > 0.001);
                if (QMesureIValide) then FLastMesureVisee := MyMesureVisee;
                Result := (QMesureIValide);
              end;
              2:  // G Calibration
              begin
                MyGCalibrationData.X := QX;
                MyGCalibrationData.Y := QY;
                MyGCalibrationData.Z := QZ;
                MyGCalibrationData.TimeStamp := Now();
                Result := CheckRanges(QX, QY, QZ);
              end;
              3:  // M Calibration
              begin
                MyMCalibrationData.X := QX;
                MyMCalibrationData.Y := QY;
                MyMCalibrationData.Z := QZ;
                MyMCalibrationData.TimeStamp := Now();
                Result := CheckRanges(QX, QY, QZ);
              end;
              4: // Vecteurs
              begin
                MyVectorData.X := QX;
                MyVectorData.Y := QY;
                MyVectorData.Z := QZ;
                MyVectorData.TimeStamp := Now();
                Result := CheckRanges(QX, QY, QZ);
              end
            else
              pass;
            end;
            Foldtype := QTypeData;
            FoldX    := QX;
            FoldY    := QY;
            FoldZ    := QZ;
          end; // if ((Foldtype <> QTypeData) and (FoldX ...
        end; //  if (Acknowledge(qtype) = PROTO_OK)) then
      end; // if (QOp < $20) then
    except
    end;
  end;
  Application.ProcessMessages;
end;

// utilitaires
procedure TCdrDistoXLazserial.DemanderUneMesure();
begin
  //LazSerial1.SynSer.SendByte(DISTOX_COMMAND_TRIG_DATA_SHOT);

  btnQSAddViseeClick(self);
end;



procedure TCdrDistoXLazserial.SendText(const S: string);
var
  n, i: Integer;
  QBuffer: array of char; // Un array dynamique est un pointeur
begin
  n := length(S);
  if (0 = n) then exit;
  SetLength(QBuffer, n);
  // La numérotation des caractères d'une chaîne démarre à 1, et non zéro
  // Chaine | 0 | A | B | C | D | ....
  // Buffer | A | B | C | D | ....
  for i := 1 to n do QBuffer[i-1] := S[i];
  LazSerial1.SynSer.SendBuffer(QBuffer, n);
  LazSerial1.SynSer.Flush;
  SetLength(QBuffer, 0);
end;

procedure TCdrDistoXLazserial.SetModeCalibrationOnOff(const b: boolean);
var
  EWE: byte;
begin
  EWE := IIF(b, DISTOX_COMMAND_MODE_CALIBRATION_ON, DISTOX_COMMAND_MODE_CALIBRATION_OFF);
  if (b) then LazSerial1.SynSer.SendByte(DISTOX_COMMAND_MODE_CALIBRATION_ON)
         else LazSerial1.SynSer.SendByte(DISTOX_COMMAND_MODE_CALIBRATION_OFF);
end;

procedure TCdrDistoXLazserial.SetDistoXScanContinuous(const b: boolean);
var
  QOpCodeStr: string;
  QOpCode: byte;
begin
  if (GHTopoQuestionOuiNon('Envoi d''une commande au DistoX - Utiliser avec prudence - Continuer')) then
  begin
    QOpCodeStr := Format('%d', [DISTOX_COMMAND_TRIG_DATA_SHOT]);
    if (InputQuery('Envoi d''une commande au DistoX', 'Opcode', QOpCodeStr)) then
    begin
      QOpCode := (StrToIntDef(QOpCodeStr, DISTOX_COMMAND_TRIG_DATA_SHOT) AND 255);
      ShowMessageFmt('La commande %d va être envoyée au DistoX', [QOpCode]);
    end;
  end;
  //if (b) then LazSerial1.SynSer.SendByte(DISTOX_COMMAND_SCAN_CONTINU_ON)
  //       else LazSerial1.SynSer.SendByte(DISTOX_COMMAND_SCAN_CONTINU_OFF);
end;

procedure TCdrDistoXLazserial.ListerLesPorts();
var
  i: Integer;
begin
  cmbPortsCom.Clear;
  {$IFDEF MSWINDOWS}
  for i := 0 to 31 do cmbPortsCom.Items.add(format('COM%d:', [i]));
  cmbPortsCom.ItemIndex := 4; // sous W10
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  // Ports Bluetooth
  for i := 0 to 7 do cmbPortsCom.Items.add(format('/dev/rfcomm%d', [i]));
  // Ports virtuels
  for i := 0 to 15 do cmbPortsCom.Items.add(format('/dev/pts/%d', [i]));
  cmbPortsCom.ItemIndex := 0;
  {$ENDIF LINUX}
end;



procedure TCdrDistoXLazserial.SendBytes(const Bytes: array of Byte);
var
  n, i: Integer;
  QBuffer: array of byte;
begin
  n := length(Bytes);
  SetLength(QBuffer, n);
  for i := 0 to n - 1  do QBuffer[i] := Bytes[i];
  LazSerial1.SynSer.SendBuffer(QBuffer, n);
  SetLength(QBuffer, 0);
end;

end.
