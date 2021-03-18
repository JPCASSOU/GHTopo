// Fichier PrototypesTDistoXConnexion.inc
++
{$WARNING  Passe dans PrototypesTDistoXConnexion.inc}
const
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


type
{ TDistoX2Connexion }

  TDistoX2Connexion = class(TLazSerial)
  private
    FCommPort               : string;
    FIsCommBusy             : boolean;
    FDistoXSerialNumber     : integer;
    FDistoXVersionFirmware  : integer;
    FDistoXVersionHardware  : integer;
    FOldType: integer;
    FOldX   : integer;
    FOldY   : integer;
    FOldZ   : Integer;
    FTimeOutInMilliseconds: integer;
    function Acknowledge(const QTypeData: byte): boolean;
    function ExtractNumSerieFromDistoX(): integer;
    function ExtractVersionFirmWareFromDistoX(): integer;
    function ExtractVersionHardwareFromDistoX(): integer;
    function SendReadCommandAtAddress(const QCmd: byte; const QAddress: word): boolean;
    function ReadBuffer8BytesAtAddress(const QAddress: word; out MyBuffer: TBuffer8Bytes): boolean;
  public
    function  InitialiserEtConnecter(const DeviceCom: string; const TimeOutInSecondes: integer): boolean;
    procedure Deconnecter();
    procedure Finaliser();
    function  IsBusy(): boolean;
    procedure SetBusy(const B: boolean);
    function  LireEtDecoderBuffer8Bytes(out MyOP: byte;
                                        out MyMesureVisee: TMesureViseeDistoX;
                                        out MyVectorData, MyGCalibrationData, MyMCalibrationData: TVectorDataDistoX): boolean;
    function  LireEtDecoderBlock(const NumBlock: integer; out MyBufferBlock: TBufferBootLoaderBlock): boolean;
    function  GetLastError(): integer;
    procedure SetModeCalibrationOnOff(const b: boolean);

    function  GetDescriptionDevice(): string;
    procedure DemanderUneMesure();
    // /!\ Le mode Silence ne transmet pas les données - Fonction désactivée
    //procedure SetModeSilenceOnOff(const b: boolean);
end;
{$NOTE -- Debut de la section implementation commun a toutes les variantes}
implementation
{$NOTE ---- Fonction locale MakeEmptyTBuffer8Bytes() }
function MakeEmptyTBuffer8Bytes(): TBuffer8Bytes;
var
  i: Integer;
begin
  for i := 0 to High(Result) do Result[i] := 0;
end;
{$NOTE ---- Fonction locale MakeEmptyTBufferBootLoaderBlock() }
function MakeEmptyTBufferBootLoaderBlock(): TBufferBootLoaderBlock;
var
  i: Integer;
begin
  for i := 0 to High(Result) do Result[i] := 0;
end;
{$WARNING  Sort de PrototypesTDistoXConnexion.inc}
