unit UnitMinimalCOM;
{$MODE DELPHI}
//{$INCLUDE CompilationParameters.inc}

interface

uses
  //{$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Classes, SysUtils, math,
  //LazSerial,
  CPort,
  Forms;
const
  SIZE_DISTOX_SEND_STD_BUFFER_COMMAND       = 1;
  SIZE_DISTOX_RECV_STD_BUFFER_DATA          = 8;       // data standard
  SIZE_DISTOX_RECV_BOOTLOADER_BUFFER        = 264;     // mode bootloader du DistoX

  FV = 24000.00;
  FM = 16384.00;

  MAX_IT = 500;
  EPS    = 1E-8;
  HEX_TO_RAD = 2 * PI / 65536;

  INVALID_HANDLE_VALUE = THandle(-1);

  DISTOX_COMMAND_EXTINCTION             =  52;  // $34: extinction du Disto
  DISTOX_COMMAND_READ_MEMORY_AT_ADDRESS =  56;  // $38: b111000 ; conflict with measure trigging command

  DISTOX_COMMAND_START_CALIBRATION      =  49;  // $30: b110001:
  DISTOX_COMMAND_STOP_CALIBRATION       =  48;  // $31: b110000:
  DISTOX_COMMAND_LASER_ON               =  54;  // $36: b110110: Laser ON
  DISTOX_COMMAND_LASER_OFF              =  55;  // $37: b110111: Laser OFF
  ///!\ Le mode Silencieux ne transmet pas les données
  //DISTOX_COMMAND_BEEP_ON                = $33;  // $33: Silent ON  = à ne pas utiliser
  //DISTOX_COMMAND_BEEP_OFF               = $32;  // $33: Silent OFF
  DISTOX_COMMAND_TRIG_DATA_SHOT         =  56;  // $38: lecture d'une visée;    conflict with measure trigging command
  DISTOX_COMMAND_BOOTLOADER_READ_DATA_AT_ADDRESS =  58;  // $38: b111010 ;


// pour le DistoX
type TNumeroSerie = integer;


type TBuffer8Bytes = array[0..7] of byte;
type TBufferBootLoaderBlock = array[0..263] of byte;

type TProcComOnError = procedure(Sender: TObject; Errors: TComErrors) of object;


type

{ TDistoX2Connexion }
  TDistoX2Connexion = class(TComPort)
  strict private
    procedure QAfficherMessage(const Msg: string);
    function ReceiveBufferOf8Bytes(const B: TBuffer8Bytes; const Count: integer = SIZE_DISTOX_RECV_STD_BUFFER_DATA): boolean;
    function SendBufferOf8Bytes(const B: TBuffer8Bytes; const Count: integer): boolean;
    function SendByte(const B: byte): boolean;

  private
    //FIsCommBusy             : boolean;

    FLastMesureVisee        : TMesureViseeDistoX;

    FDistoXSerialNumber     : integer;
    FDistoXVersionFirmware  : integer;
    FDistoXVersionHardware  : integer;



    FOldType: integer;
    FOldX   : integer;
    FOldY   : integer;
    FOldZ   : Integer;
    FProcOfObjectWithOneStringParameter: TProcOfObjectWithOneStringParameter;
    FProcOfObjectWithAShot: TProcOfObjectWithAShot;


    function Acknowledge(const QTypeData: byte): boolean;
    function ExtractNumSerieFromDistoX(): integer;
    function ExtractVersionFirmWareFromDistoX(): integer;
    function ExtractVersionHardwareFromDistoX(): integer;


    function SendReadCommandAtAddress(const QCmd: byte; const QAddress: word): boolean;
    function ReadBuffer8BytesAtAddress(const QAddress: word; out MyBuffer: TBuffer8Bytes): boolean;

    //function ReadBootloaderBufferAtAddress(const QAddress: word; out MyBuffer: TBufferBootLoaderBlock): boolean;
    //function SendBootloaderReadCommandAtAddress(const QCmd: byte; const QAddress: word): boolean;
    //function GetAnBlock(const NumeroBlock: integer; out Block: TBufferBootLoaderBlock): boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy();
    function    InitialiserEtConnecter(const DeviceCom: string;
                                       const TimeOutInSecondes: integer;
                                       const ProcTransmitMessage  : TProcOfObjectWithOneStringParameter;
                                       const ProcOfObjectWithAShot: TProcOfObjectWithAShot;
                                       // callbacks
                                       const ProcComOnError: TProcComOnError;
                                       const ProcComBeforeOpen, ProcComAfterOpen, ProcComBeforeClose, ProcComAfterClose: TNotifyEvent;
                                       const ProcComOnRXData: TNotifyEvent;
                                       out   ComErrorCode: integer; out   ComErrorMessage: string): integer;

    procedure SetProcTransmitShot(const PS: TProcOfObjectWithAShot);
    function  GetProcTransmitShot(): TProcOfObjectWithAShot;


    procedure Deconnecter();
    procedure Finaliser();
    procedure SetAcquisitionContinue(const OnOff: boolean);
    function  LireEtDecoderBuffer8Bytes(out MyOP: byte;
                                        out MyMesureVisee: TMesureViseeDistoX;
                                        out MyVectorData, MyGCalibrationData, MyMCalibrationData: TVectorDataDistoX): boolean;
    function  LireEtDecoderBlock(const NumBlock: integer; out MyBufferBlock: TBufferBootLoaderBlock): boolean;
    procedure SetModeCalibrationOnOff(const b: boolean);

    function  GetDescriptionDevice(): string;
    procedure DemanderUneMesure();
    // /!\ Le mode Silence ne transmet pas les données - Fonction désactivée
    //procedure SetModeSilenceOnOff(const b: boolean);

    function  TestProcTransmitMesure(): integer;
    function  GetLastShotDistoX(): TMesureViseeDistoX;
end;



implementation
uses
  DGCDummyUnit;
function MakeEmptyTBuffer8Bytes(): TBuffer8Bytes;
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

{ TDistoX2Connexion }

procedure TDistoX2Connexion.QAfficherMessage(const Msg: string);
begin
  if (Assigned(FProcOfObjectWithOneStringParameter)) then FProcOfObjectWithOneStringParameter(Msg);
end;

function TDistoX2Connexion.SendBufferOf8Bytes(const B: TBuffer8Bytes; const Count: integer): boolean;
begin
  result := false;
  ;//pass; // TODO A implementer
end;

function TDistoX2Connexion.SendByte(const B: byte): boolean;
begin
  result := false;
  ;//pass; // TODO A implementer
end;

function TDistoX2Connexion.ReceiveBufferOf8Bytes(const B: TBuffer8Bytes; const Count: integer = SIZE_DISTOX_RECV_STD_BUFFER_DATA): boolean;
begin
  result := false
  ; //pass; // TODO A implementer
end;

function TDistoX2Connexion.Acknowledge(const QTypeData: byte): boolean;
var
  MyByte: Byte;
begin
  result := false;
  MyByte := (QTypeData and $80) or $55;
  Result := self.SendByte(MyByte);
end;



function TDistoX2Connexion.InitialiserEtConnecter(const DeviceCom: string;
                                                  const TimeOutInSecondes: integer;
                                                  const ProcTransmitMessage  : TProcOfObjectWithOneStringParameter;
                                                  const ProcOfObjectWithAShot: TProcOfObjectWithAShot;
                                                  // callbacks
                                                  const ProcComOnError: TProcComOnError;
                                                  const ProcComBeforeOpen, ProcComAfterOpen, ProcComBeforeClose, ProcComAfterClose: TNotifyEvent;
                                                  const ProcComOnRXData: TNotifyEvent;
                                                  out   ComErrorCode: integer;
                                                  out   ComErrorMessage: string): integer;

begin
  //ShowMessage(Format('Device: %s - Timeout: %d', [DeviceCom, TimeOutInSecondes]));
  result := -1;
  ComErrorCode    := -1;
  ComErrorMessage := '';
  self.Port        := DeviceCom;
  self.BaudRate    := br9600;
  self.DataBits    := dbEight;
  self.Parity.Bits         := prNone;
  self.Parity.Check        := false;
  self.Parity.Replace      := false;
  self.Parity.ReplaceChar  := #0;

  self.Events              := [evRxChar,evTxEmpty,evRxFlag,evRing,evBreak,evCTS,evDSR,evError,evRLSD,evRx80Full];
  self.SyncMethod          := smThreadSync;
  // timeouts
  self.Timeouts.ReadInterval        := -1;
  self.Timeouts.ReadTotalConstant   := 0;
  self.Timeouts.ReadTotalMultiplier := 0;
  self.Timeouts.WriteTotalConstant  := 1000; // A voir
  self.Timeouts.WriteTotalMultiplier:= 100;
  // buffers
  self.Buffer.InputSize                  := SIZE_DISTOX_RECV_STD_BUFFER_DATA;
  self.Buffer.OutputSize                 := SIZE_DISTOX_SEND_STD_BUFFER_COMMAND;
  // callbacks
  self.OnError                           := ProcComOnError;
  self.OnBeforeOpen                      := ProcComBeforeOpen;
  self.OnBeforeClose                     := ProcComBeforeClose;
  self.OnAfterOpen                       := ProcComAfterOpen;
  self.OnAfterClose                      := ProcComAfterClose;
  self.OnRxFlag                          := ProcComOnRXData;
  // On tente d'ouvrir
  try
    self.Open;
  except
    on E: EComPort do
    begin
      ComErrorCode    := E.Code;
      ComErrorMessage := E.Message;
    end;
  end;

  //****************************************************************************
  (*

  FProcOfObjectWithOneStringParameter  := ProcTransmitMessage;
  FProcOfObjectWithAShot               := ProcOfObjectWithAShot;
  QAfficherMessage('Tentative de connexion');


  FCommPort := DeviceCom;
  FTimeOutInMilliseconds := 1000 * TimeOutInSecondes;
  QAfficherMessage('--> Device: ' + FCommPort);
  QAfficherMessage(format('--> Timeout = %d', [FTimeOutInMilliseconds]));


  self.OnRxData := nil;
  self.OnStatus := nil;

  QAfficherMessage('--> Cloture du port si déjà ouvert');
  Deconnecter();

  QAfficherMessage('--> Ouverture du port');
  try
    self.Open;
    // Ces deux événements doivent être paramétrés APRES ouverture du port
    //self.OnRxData := ProcOnRXData;
    //self.OnStatus := ProcOnStatus;

    Application.ProcessMessages;
    Result := self.SynSer.LastError;
    QAfficherMessage(Format('Dernier retour: %d', [self.SynSer.LastError]));

  except
    QAfficherMessage(Format('*** Erreur %d - %s', [self.SynSer.LastError, self.SynSer.LastErrorDesc]));
    Deconnecter();
  end;
  //*)
end;

procedure TDistoX2Connexion.SetProcTransmitShot(const PS: TProcOfObjectWithAShot);
begin
  FProcOfObjectWithAShot := PS;
end;

function TDistoX2Connexion.GetProcTransmitShot(): TProcOfObjectWithAShot;
begin
  Result := FProcOfObjectWithAShot;
end;


procedure TDistoX2Connexion.Deconnecter();
begin
  Application.ProcessMessages;
  try
    QAfficherMessage('Fermeture forcée du port');
    self.Close;
  finally
    Application.ProcessMessages;
  end;
end;

procedure TDistoX2Connexion.Finaliser();
begin
  Application.ProcessMessages;
  try
    self.Close;
  finally
  end;
end;

procedure TDistoX2Connexion.SetAcquisitionContinue(const OnOff: boolean);
begin
  QAfficherMessage(Format('%s.SetAcquisitionContinue(%s)', [ClassName, BoolToStr(OnOff, 'ON', 'OFF')]));
end;



// Dans cette fonction, on extrait une trame de 8 octets
// S'il s'agit de données de mesures topo, on acquitte.
// En cas de succès de l'acquittement, on décode
// En cas de succès du décodage, l'appelant peut utiliser la visée
function TDistoX2Connexion.LireEtDecoderBuffer8Bytes(out MyOP: byte;
                                                     out MyMesureVisee: TMesureViseeDistoX;
                                                     out MyVectorData, MyGCalibrationData, MyMCalibrationData: TVectorDataDistoX): boolean;
const MAX_WORD = 65535;
var
  EWE: String;
  i: Integer;
  MyBuffer : TBuffer8Bytes;
  QTypeData: Byte;
  QX, QY, QZ, Dist: word; //SmallInt;
  P: float;
  QMesureIValide: Boolean;
  function CheckRanges(const FX, FY, FZ: word): boolean; inline;
  begin
    Result := InRange(FX, 0, MAX_WORD) and InRange(FY, 0, MAX_WORD) and InRange(FZ, 0, MAX_WORD);
  end;
begin
  Result := false;
  if (self.ReceiveBufferOf8Bytes(MyBuffer)) then
  begin
    try
      MyMesureVisee.DistoXSerialNumber := FDistoXSerialNumber;
      MyMesureVisee.Longueur := 0.00;
      MyMesureVisee.Azimut   := 0.00;
      MyMesureVisee.Pente    := 0.00;
      MyMesureVisee.HexaData := '';
      EWE := '';
      for i := 0 to 7 do EWE += Format('%.2X', [MyBuffer[i]]);
      QTypeData := MyBuffer[0];
      MyOp       := (QTypeData and $3F);
      // Ce sont des données ?
      if (MyOp < $20) then
      begin
        if (Acknowledge(QTypeData)) then   // Indispensable juste après le test (QOp < $20), sinon, KO
        begin
          QX := MyBuffer[1] + (MyBuffer[2] shl 8);
          QY := MyBuffer[3] + (MyBuffer[4] shl 8);
          QZ := MyBuffer[5] + (MyBuffer[6] shl 8);
          if ((Foldtype <> QTypeData) and (FoldX    <> QX) and (FoldY    <> QY) and (FoldZ <> QZ)) then
          begin
            case (MyOp) of
              1: // données topo
              begin
                // distances exprimées en mm si < 100 m, sinon en cm
                Dist := QX + ((QTypeData and $40) shr 10);     // OK
                //myMesureVisee.TypeMesure := op;
                MyMesureVisee.IsViseeCheminement := false;
                MyMesureVisee.SerieCourante   := -1;
                MyMesureVisee.StationCourante := -1;

                myMesureVisee.Longueur := Dist * 0.001; // TODO: Cas où Dist > 100 m
                myMesureVisee.Azimut   := radtodeg(QY * HEX_TO_RAD);                   // OK
                P  := radtodeg(QZ * HEX_TO_RAD);
                if (Not InRange(P, -0.001, 0.25 * UNITE_ANGULAIRE_PAR_DEFAUT + 0.001)) then P := P - UNITE_ANGULAIRE_PAR_DEFAUT;
                myMesureVisee.Pente := P;
                myMesureVisee.HexaData := EWE;
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
              4:
              begin // Vecteurs
                MyVectorData.X := QX;
                MyVectorData.Y := QY;
                MyVectorData.Z := QZ;
                MyVectorData.TimeStamp := Now();
                Result := CheckRanges(QX, QY, QZ);
              end
            else
              ;
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
  //*)
end;

function TDistoX2Connexion.LireEtDecoderBlock(const NumBlock: integer; out MyBufferBlock: TBufferBootLoaderBlock): boolean;
begin
  Result := false;
  MyBufferBlock := MakeEmptyTBufferBootLoaderBlock();
  (*
  if (self.SynSer.CanRead(FTimeOutInMilliseconds)) then
  begin
    try
      self.SynSer.RecvBuffer(@MyBufferBlock, 256);
    except
    end;
  end;
  //*)
end;

procedure TDistoX2Connexion.SetModeCalibrationOnOff(const b: boolean);
begin
  if (B) then  self.SendByte(DISTOX_COMMAND_START_CALIBRATION)
         else  self.SendByte(DISTOX_COMMAND_STOP_CALIBRATION);
end;
// /!\ Le mode Silence ne transmet pas les données - Fonction désactivée
(*
procedure TDistoX2Connexion.SetModeSilenceOnOff(const b: boolean);
begin
   if (B) then  self.SynSer.SendByte(DISTOX_COMMAND_BEEP_OFF)
          else  self.SynSer.SendByte(DISTOX_COMMAND_BEEP_ON);
end;
//*)
function TDistoX2Connexion.GetDescriptionDevice(): string;
begin
  Result := Format('Port: %s - Device: %d - Hardware: %d - Firmware: %d',
                   [self.Port,
                    ExtractNumSerieFromDistoX(),
                    ExtractVersionHardwareFromDistoX(),
                    ExtractVersionFirmWareFromDistoX()
                   ]);
end;

procedure TDistoX2Connexion.DemanderUneMesure();
begin
  self.SendByte(DISTOX_COMMAND_TRIG_DATA_SHOT);
end;

function TDistoX2Connexion.TestProcTransmitMesure(): integer;
var
  QP, QAz, QL: double;
  QHorodate: TDateTime;
begin
  Result := -1;
  if (not Assigned(FProcOfObjectWithAShot)) then Exit(-2);
  QHorodate := Now();
  QL  := 22.92;
  QAz := 233.61;
  QP  :=-45.0;
  FProcOfObjectWithAShot(QHorodate, QL, QAz, QP);
end;

function TDistoX2Connexion.GetLastShotDistoX(): TMesureViseeDistoX;
begin
  Result := FLastMesureVisee;
end;





// Fonctions bas niveau de lecture/écriture
function TDistoX2Connexion.SendReadCommandAtAddress(const QCmd: byte; const QAddress: word): boolean;
var
  MyBuffer: TBuffer8Bytes;
begin
  MyBuffer := MakeEmptyTBuffer8Bytes();
  MyBuffer[0] :=  QCmd;                       // DISTOX_COMMAND_READ_DATA = 56; // 00111000
  MyBuffer[1] :=  QAddress and $FF;           //  Address & 0xFF
  MyBuffer[2] := (QAddress shr 8) and $FF;    // (Address >> 8) & 0xFF
  Result := self.SendBufferOf8Bytes(MyBuffer, 3);
end;
// Fonctions bas niveau de lecture/écriture

function TDistoX2Connexion.ReadBuffer8BytesAtAddress(const QAddress: word; out MyBuffer: TBuffer8Bytes): boolean;
begin
  result := false;
  MyBuffer := MakeEmptyTBuffer8Bytes();
  if (SendReadCommandAtAddress(DISTOX_COMMAND_READ_MEMORY_AT_ADDRESS, QAddress)) then
  begin
    result := self.ReceiveBufferOf8Bytes(MyBuffer, 8);
  end;
end;

constructor TDistoX2Connexion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProcOfObjectWithAShot              := nil;
  FProcOfObjectWithOneStringParameter := nil;
  FDistoXSerialNumber    := -1;
  FDistoXVersionFirmware := -1;
  FDistoXVersionHardware := -1;
  FOldType := 0;
  FOldX    := 0;
  FOldY    := 0;
  FOldZ    := 0;
  // buffer

end;

destructor TDistoX2Connexion.Destroy();
begin
  inherited Destroy;
end;

// numéro de série du DistoX
function TDistoX2Connexion.ExtractNumSerieFromDistoX(): integer;
var
  MyBuffer: TBuffer8Bytes;
begin
  if (FDistoXSerialNumber = -1) then          // si FDistoXSerialNumber = - 1, on fait une tentative d'extraction;
  begin
    MyBuffer := MakeEmptyTBuffer8Bytes();
    if (ReadBuffer8BytesAtAddress($8008, MyBuffer)) then  FDistoXSerialNumber := MyBuffer[3] + MyBuffer[4] shl 8; // retourne 2395 ou 2329 pour les DistoX de JPC
  end;
  Result := FDistoXSerialNumber;
end;
// version de firmware
function TDistoX2Connexion.ExtractVersionFirmWareFromDistoX(): integer;
var
  MyBuffer: TBuffer8Bytes;
begin
  if (FDistoXVersionFirmware = -1) then // si FDistoXVersionFirmware = - 1, on fait une tentative d'extraction;
  begin
    MyBuffer := MakeEmptyTBuffer8Bytes();
    if (ReadBuffer8BytesAtAddress($E000, MyBuffer)) then FDistoXVersionFirmware := 100 * MyBuffer[3] + MyBuffer[4];
  end;
  Result := FDistoXVersionFirmware;
end;
// version de hardware
function TDistoX2Connexion.ExtractVersionHardwareFromDistoX(): integer;
var
  MyBuffer: TBuffer8Bytes;
begin
  if (FDistoXVersionHardware = -1) then
  begin
    MyBuffer := MakeEmptyTBuffer8Bytes();
    if (ReadBuffer8BytesAtAddress($E004, MyBuffer)) then FDistoXVersionHardware := 10 * MyBuffer[3] + MyBuffer[4];     // byte 3 = major; byte 4 = minor, bytes 5 et 6 = 0
  end;
  Result := FDistoXVersionHardware;
end;
//******************************************************************************
//*******************************************************************************
// BOOTLOADER
// lecture d'un block
(*
function TDistoX2Connexion.GetAnBlock(const NumeroBlock: integer; out Block: TBufferBootLoaderBlock): boolean;
var
  QOldMode: TModeAcquisitionData;
  QAddress: Integer;
begin
  result := false;
  QOldMode := FModeAcquisitionData;
  ShowMessagefmt('GetAnBlock %d', [NumeroBlock]);
  FModeAcquisitionData := acqBLOCK;
  try
    QAddress := $B400 + NumeroBlock * 264;
    ShowMessageFmt('Addr: %X',[QAddress]);
    ReadBootloaderBufferAtAddress(QAddress, Block);
    ShowMessage('OK');
    result := True;
  finally
    FModeAcquisitionData := QOldMode;
  end;
end;


function TDistoX2Connexion.ReadBootloaderBufferAtAddress(const QAddress: word; out MyBuffer: TBufferBootLoaderBlock): boolean;
begin
  result := false;
  MyBuffer := MakeEmptyTBufferBootLoaderBlock();
  if (SendBootloaderReadCommandAtAddress(DISTOX_COMMAND_BOOTLOADER_READ_DATA_AT_ADDRESS, QAddress)) then
  begin
    ShowMessage('ReadBootloaderBufferAtAddress');
    self.SynSer.RecvBuffer(@MyBuffer, 264);
    ShowMessage('ReadBootloaderBufferAtAddress OK');
    Result := (0 = self.SynSer.LastError);
  end;
end;
function TDistoX2Connexion.SendBootloaderReadCommandAtAddress(const QCmd: byte; const QAddress: word): boolean;
var
  MyBuffer: TBuffer8Bytes;
begin
  MyBuffer := MakeEmptyTBuffer8Bytes();
  MyBuffer[0] :=  QCmd;                        //DISTOX_COMMAND_READ_DATA = 56; // 00111000
  // encore une bizarrerie du DistoX ... adresses sur 24 bits
  //MyBuffer[1] :=  QAddress and $FF;           //  Address & 0xFF
  //MyBuffer[2] := (QAddress shr 8) and $FF;    // (Address >> 8) & 0xFF
  MyBuffer[1] := (QAddress shr 8) and $FF;           //  Address & 0xFF
  MyBuffer[2] := (QAddress shr 16) and $FF;

  self.SynSer.SendBuffer(@MyBuffer, 3);
  result := (0 = self.SynSer.LastError);
end;
//*)
end.
