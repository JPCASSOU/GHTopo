unit UnitMinimalCOM_ARM;
{$MODE DELPHI}
{$NOTE  Ceci est specifique au Raspberry Pi 3}
{$ERROR Installer les libs de dev: sudo apt-get install libbluetooth-dev}
interface
uses
  StructuresDonnees,
  Classes, SysUtils, math;
{$INCLUDE PrototypesTDistoXConnexion.inc}

{ TDistoX2Connexion }

constructor TDistoX2Connexion.Create(sender: TObject);
begin
  inherited Create;
end;

function TDistoX2Connexion.Acknowledge(const QTypeData: byte): boolean;
var
  MyByte: Byte;
begin
  result := false;
  (*
  if (self.SynSer.CanWrite(FTimeOutInMilliseconds)) then
  begin
    MyByte := (QTypeData and $80) or $55;
    self.SynSer.SendByte(MyByte);
    Result := (0 = self.synser.LastError);
  end;
  //*)
end;

function TDistoX2Connexion.InitialiserEtConnecter(const DeviceCom: string; const TimeOutInSecondes: integer): boolean;
begin
  //ShowMessage(Format('Device: %s - Timeout: %d', [DeviceCom, TimeOutInSecondes]));
  result := false;
  FIsCommBusy := True;
  // fermeture
  //self.CloseSocket;
  //WriteLn(Format('%s.Initialiser(): Port "%s" - TimeOut: %d secondes)', [self.ClassName, DeviceCom, TimeOutInSecondes]));
  FTimeOutInMilliseconds := 1000 * TimeOutInSecondes;
  FCommPort := DeviceCom;
  FDistoXSerialNumber    := -1;
  FDistoXVersionFirmware := -1;
  FDistoXVersionHardware := -1;
  FOldType := 0;
  FOldX    := 0;
  FOldY    := 0;
  FOldZ    := 0;
  (*
  Self.SynSer.RaiseExcept:=true;
  //Self.Connect(FCommPort);
  self.Device := FCommPort;
  self.BaudRate := br__9600;
  self.DataBits := db8bits;
  self.Parity   := pNone;
  self.StopBits := sbOne;
  self.Open;

  //self..Config(9600, 8, 'N', 0, false, false);


  //WriteLn('Connexion OK');
  Result := (0 = self.SynSer.LastError);
  //*)
  FIsCommBusy := false;
end;


procedure TDistoX2Connexion.Deconnecter();
begin
  try
    //self.Close;
  finally
  end;
end;

procedure TDistoX2Connexion.Finaliser();
begin
  //WriteLn(Format('%s.Finaliser("%s")', [self.ClassName, self.GetDescriptionDevice()]));
  try
    //self.CloseSocket;
    //self.Active := false;
  finally
  end;
end;

function TDistoX2Connexion.IsBusy(): boolean;
begin
  Result := FIsCommBusy;
end;

procedure TDistoX2Connexion.SetBusy(const B: boolean);
begin
  FIsCommBusy := B;
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
  function CheckRanges(const FX, FY, FZ: word): boolean; inline;
  begin
    Result := InRange(FX, 0, MAX_WORD) and InRange(FY, 0, MAX_WORD) and InRange(FZ, 0, MAX_WORD);
  end;
begin
  Result := false;
  (*
  if (self.SynSer.CanRead(FTimeOutInMilliseconds)) then
  begin
    try
      self.SynSer.RecvBuffer(@MyBuffer, 8);
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

            case MyOp of
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
               // if (Not InRange(P, -0.001, (90.001)) then P := P - 360.00;
               if (Not InRange(P, -0.001, 0.25 * 360.00 + 0.001)) then P := P - 360.00;
               myMesureVisee.Pente := P;
               myMesureVisee.HexaData := EWE;
               myMesureVisee.TimeStamp:= Now();
                // On vire les visées de longueur nulle (qui seront rejetées par GHTopo de toutes façons)
               Result := (MyMesureVisee.Longueur > 0.001);
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
  MyBufferBlock := MakeEmptyTBufferBootLoaderBlock();
  Result := false;
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

function TDistoX2Connexion.GetLastError(): integer;
begin
  result := -1; //self.SynSer.LastError;
end;

procedure TDistoX2Connexion.SetModeCalibrationOnOff(const b: boolean);
begin
  (*
  if (B) then  self.SynSer.SendByte(DISTOX_COMMAND_START_CALIBRATION)
         else  self.SynSer.SendByte(DISTOX_COMMAND_STOP_CALIBRATION);
  //*)
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
                   [FCommPort,
                    ExtractNumSerieFromDistoX(),
                    ExtractVersionHardwareFromDistoX(),
                    ExtractVersionFirmWareFromDistoX()
                   ]);
end;

procedure TDistoX2Connexion.DemanderUneMesure();
begin
  FIsCommBusy := True;
  //  self.SynSer.SendByte(DISTOX_COMMAND_TRIG_DATA_SHOT);
end;





// Fonctions bas niveau de lecture/écriture
function TDistoX2Connexion.SendReadCommandAtAddress(const QCmd: byte; const QAddress: word): boolean;
var
  MyBuffer: TBuffer8Bytes;
begin
  MyBuffer := MakeEmptyTBuffer8Bytes();
  MyBuffer[0] :=  QCmd;                        //DISTOX_COMMAND_READ_DATA = 56; // 00111000
  MyBuffer[1] :=  QAddress and $FF;           //  Address & 0xFF
  MyBuffer[2] := (QAddress shr 8) and $FF;    // (Address >> 8) & 0xFF
  (*
  self.SynSer.SendBuffer(@MyBuffer, 3);
  result := (0 = self.SynSer.LastError);
  //*)
end;
// Fonctions bas niveau de lecture/écriture

function TDistoX2Connexion.ReadBuffer8BytesAtAddress(const QAddress: word; out MyBuffer: TBuffer8Bytes): boolean;
begin
  result := false;
  MyBuffer := MakeEmptyTBuffer8Bytes();
  if (SendReadCommandAtAddress(DISTOX_COMMAND_READ_MEMORY_AT_ADDRESS, QAddress)) then
  begin
    //self.SynSer.RecvBuffer(@MyBuffer, 8);
    //Result := (0 = self.SynSer.LastError);
  end;
end;

// numéro de série du DistoX
function TDistoX2Connexion.ExtractNumSerieFromDistoX(): integer;
var
  MyBuffer: TBuffer8Bytes;
begin
  FIsCommBusy := True;
  if (FDistoXSerialNumber = -1) then          // si FDistoXSerialNumber = - 1, on fait une tentative d'extraction;
  begin
    MyBuffer := MakeEmptyTBuffer8Bytes();
    if (ReadBuffer8BytesAtAddress($8008, MyBuffer)) then  FDistoXSerialNumber := MyBuffer[3] + MyBuffer[4] shl 8; // retourne 2395 ou 2329 pour les DistoX de JPC
  end;
  Result := FDistoXSerialNumber;
  FIsCommBusy := false;
end;
// version de firmware
function TDistoX2Connexion.ExtractVersionFirmWareFromDistoX(): integer;
var
  MyBuffer: TBuffer8Bytes;
begin
  FIsCommBusy := True;
  if (FDistoXVersionFirmware = -1) then // si FDistoXVersionFirmware = - 1, on fait une tentative d'extraction;
  begin
    MyBuffer := MakeEmptyTBuffer8Bytes();
    if (ReadBuffer8BytesAtAddress($E000, MyBuffer)) then FDistoXVersionFirmware := 100 * MyBuffer[3] + MyBuffer[4];
  end;
  Result := FDistoXVersionFirmware;
  FIsCommBusy := false;
end;
// version de hardware
function TDistoX2Connexion.ExtractVersionHardwareFromDistoX(): integer;
var
  MyBuffer: TBuffer8Bytes;
begin
  FIsCommBusy := True;
  if (FDistoXVersionHardware = -1) then
  begin
    MyBuffer := MakeEmptyTBuffer8Bytes();
    if (ReadBuffer8BytesAtAddress($E004, MyBuffer)) then FDistoXVersionHardware := 10 * MyBuffer[3] + MyBuffer[4];     // byte 3 = major; byte 4 = minor, bytes 5 et 6 = 0
  end;
  Result := FDistoXVersionHardware;
  FIsCommBusy := false;
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
