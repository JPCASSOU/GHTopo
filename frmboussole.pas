unit frmBoussole;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, CadreBoussole;

type

  { TdlgBoussole }

  TdlgBoussole = class(TForm)
    BitBtn1: TBitBtn;
    btnBackColor: TColorButton;
    btnReticuleColor: TColorButton;
    btnRoseColor: TColorButton;
    CdrBoussole1: TCdrBoussole;
    lbUniteAzimut: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    lbPhidgets: TPanel;
    RadioGroup1: TRadioGroup;
    sclAzimut: TScrollBar;
    sclInclinX: TScrollBar;
    sclInclinY: TScrollBar;
    StaticText1: TStaticText;
    lbPhidgetsStatus: TStaticText;
    procedure btnBackColorColorChanged(Sender: TObject);
    procedure btnReticuleColorColorChanged(Sender: TObject);
    procedure btnRoseColorColorChanged(Sender: TObject);
    procedure sclInclinXChange(Sender: TObject);
    procedure sclInclinYChange(Sender: TObject);
    procedure SetUniteBoussoleClino(const UB, UC: double);
    procedure RadioGroup1Click(Sender: TObject);
    procedure sclAzimutChange(Sender: TObject);
  private


  public
    function Initialiser(): boolean;
  end;

var
  dlgBoussole: TdlgBoussole;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TdlgBoussole }
function TdlgBoussole.Initialiser(): boolean;
begin
  SetUniteBoussoleClino(360.00, 360);
  result := true;
end;

procedure TdlgBoussole.sclAzimutChange(Sender: TObject);
begin
  CdrBoussole1.SetAzimut(1.00 * sclAzimut.Position);
end;

procedure TdlgBoussole.SetUniteBoussoleClino(const UB, UC: double);
var
  EWE: Int64;
begin
  CdrBoussole1.Initialiser(UB, UC, btnBackColor.ButtonColor, btnRoseColor.ButtonColor, btnReticuleColor.ButtonColor);
  CdrBoussole1.SetAzimut(0.00);
  sclAzimut.Max := trunc(UB);
  sclAzimut.Position := 0;
  EWE := trunc(UC / 4);
  sclInclinX.Min := -EWE;
  sclInclinX.Max :=  EWE;
  sclInclinX.Position := 0;
  sclInclinY.Min := -EWE;
  sclInclinY.Max :=  EWE;
  sclInclinY.Position := 0;
  lbUniteAzimut.Caption := format('%.0f', [UB]);
end;


procedure TdlgBoussole.btnBackColorColorChanged(Sender: TObject);
begin
  CdrBoussole1.SetBackColor(btnBackColor.ButtonColor);
end;
procedure TdlgBoussole.btnReticuleColorColorChanged(Sender: TObject);
begin
   CdrBoussole1.SetReticuleColor(btnReticuleColor.ButtonColor);
end;
procedure TdlgBoussole.btnRoseColorColorChanged(Sender: TObject);
begin
    CdrBoussole1.SetRoseColor(btnRoseColor.ButtonColor);
end;

procedure TdlgBoussole.sclInclinXChange(Sender: TObject);
begin
  CdrBoussole1.SetInclinaisonX(1.00 * sclInclinX.Position);
end;

procedure TdlgBoussole.sclInclinYChange(Sender: TObject);
begin
  CdrBoussole1.SetInclinaisonY(1.00 * sclInclinY.Position);
end;

procedure TdlgBoussole.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: SetUniteBoussoleClino(360, 360);
    1: SetUniteBoussoleClino(400, 400);
    2: SetUniteBoussoleClino(666, 666);
  end;
end;

end.

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
unit MainFrm;

{$mode delphi}{$H+}
{$DEFINE USE_DATA_HANDLER}
{$UNDEF USE_DATA_HANDLER}


interface

uses
  Classes, SysUtils,
  Lazarusphidget21, PhidgetUtils,
  UnitListesSimplesWithGeneriques,
  CadreBoussole, cadreclino,
  windows,
  Math,
  FileUtil, curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnConnecter: TButton;
    btnDeconnecter: TButton;
    Button1: TButton;
    CdrBoussole1: TCdrBoussole;
    CdrClino1: TCdrClino;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Panel3: TPanel;
    pnlCommandes: TPanel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    Timer1: TTimer;
    btnAcquisitionEnCours: TToggleBox;
    trkbarTimerInterval: TTrackBar;
    procedure btnAcquisitionEnCoursChange(Sender: TObject);
    procedure btnConnecterClick(Sender: TObject);
    procedure btnDeconnecterClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure pnlCommandesClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure trkbarTimerIntervalChange(Sender: TObject);
  private
    { private declarations }
    FSpatial   : CPhidgetSpatialHandle;
    FSpatialReady: boolean;
    FSpatialReading: boolean;

    FMagneticFieldMinX, FMagneticFieldMinY, FMagneticFieldMinZ: CDouble;
    FMagneticFieldMaxX, FMagneticFieldMaxY, FMagneticFieldMaxZ: CDouble;

    FAccelerationMinX, FAccelerationMinY, FAccelerationMinZ   : CDouble;
    FAccelerationMaxX, FAccelerationMaxY, FAccelerationMaxZ   : CDouble;

    FAngularRateMinX, FAngularRateMinY, FAngularRateMinZ      : CDouble;
    FAngularRateMaxX, FAngularRateMaxY, FAngularRateMaxZ      : CDouble;

    // datarate
    FDataRateInMilliSeconds: CInteger;
    FDataRateMin, FDataRateMax: CInteger;
    // liste des mesures à moyenner ou à intégrer
    FListeMesuresAIntegrer: TListOfEnsembledeMesures3x3x3;
    procedure DoAcquisitionValeurs();
    procedure ExtractMiniMaxiValues();

    function ConnectPhidget(): boolean;
    procedure CloseAndDestroyPhidget();

    procedure DisplayInfosDevice();

    function IsValidAcceleration(const Axis: integer; const X: CDouble): boolean;

    function IsValidMagneticField(const Axis: integer; const X: CDouble): boolean;
    function IsValidAngularRate(const Axis: integer; const X: CDouble): boolean;

    function IsValidAccelerationVector(const X, Y, Z: CDouble): boolean;
    function IsValidMagneticFieldVector(const X, Y, Z: CDouble): boolean;
    function IsValidAngularRateVector(const X, Y, Z: CDouble): boolean;

    function IsAllValidVectors(const Mx, My, Mz, Gx, Gy, Gz, Ax, Ay, Az: double): boolean; overload;
    function IsAllValidVectors(const SM: TEnsembledeMesures3x3x3): boolean; overload;



    procedure MoyennerLesMesures();
    procedure IntegrerSurPeriode();
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.ExtractMiniMaxiValues();
var
  QMagneticFieldX, QMagneticFieldY, QMagneticFieldZ: CDouble;

begin
end;



procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAndDestroyPhidget();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSpatialReady := false;
  FSpatialReading := false;
  FListeMesuresAIntegrer := TListOfEnsembledeMesures3x3x3.Create;
  FListeMesuresAIntegrer.Reset();
  FListeMesuresAIntegrer.ClearListe();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  try
    CloseAndDestroyPhidget();
    FListeMesuresAIntegrer.ClearListe();
    FListeMesuresAIntegrer.Reset();
  finally
    FListeMesuresAIntegrer.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  CdrBoussole1.Initialiser();
  CdrClino1.Initialiser();

end;








function TForm1.IsValidAcceleration(const Axis: integer; const X: CDouble): boolean;
begin
  case Axis of
    0: Result := InRange(X, FAccelerationMinX, FAccelerationMaxX);
    1: Result := InRange(X, FAccelerationMinY, FAccelerationMaxY);
    2: Result := InRange(X, FAccelerationMinZ, FAccelerationMaxZ);
  end;
end;

function TForm1.IsValidAccelerationVector(const X, Y, Z: CDouble): boolean;
begin
  Result := IsValidAcceleration(0, X) and
            IsValidAcceleration(1, Y) and
            IsValidAcceleration(2, Z);
end;



function TForm1.IsValidAngularRate(const Axis: integer; const X: CDouble): boolean;
begin
  case Axis of
    0: Result := InRange(X, FAngularRateMinX, FAngularRateMaxX);
    1: Result := InRange(X, FAngularRateMinY, FAngularRateMaxY);
    2: Result := InRange(X, FAngularRateMinZ, FAngularRateMaxZ);
  end;
end;
function TForm1.IsValidAngularRateVector(const X, Y, Z: CDouble): boolean;
begin
  Result := IsValidAngularRate(0, X) and
            IsValidAngularRate(1, Y) and
            IsValidAngularRate(2, Z);
end;

function TForm1.IsValidMagneticField(const Axis: integer; const X: CDouble): boolean;
begin
  // Due to time for internal calibration, the compass sometimes returns the large constant PUNK_DBL
  if (SameValue(X, PUNK_DBL)) then exit(false);
  case Axis of
    0: Result := InRange(X, FMagneticFieldMinX, FMagneticFieldMaxX);
    1: Result := InRange(X, FMagneticFieldMinY, FMagneticFieldMaxY);
    2: Result := InRange(X, FMagneticFieldMinZ, FMagneticFieldMaxZ);
  end;
end;
function TForm1.IsValidMagneticFieldVector(const X, Y, Z: CDouble): boolean;
begin
  Result := IsValidMagneticField(0, X) and
            IsValidMagneticField(1, Y) and
            IsValidMagneticField(2, Z);
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

procedure TForm1.pnlCommandesClick(Sender: TObject);
begin

end;

procedure TForm1.MoyennerLesMesures;
var
  ValeursMoyennes: TEnsembledeMesures3x3x3;
begin
  try
    StaticText3.Caption := format('%d', [FListeMesuresAIntegrer.GetNbElements()]);
    if (FListeMesuresAIntegrer.Moyenner(ValeursMoyennes, false)) then;
    begin

      StaticText1.Caption := Format('%.6f', [ValeursMoyennes.Bearing]);
      StaticText2.Caption := Format('%.6f', [ValeursMoyennes.Tilt]);

    end;
  finally
  end;
end;

procedure TForm1.IntegrerSurPeriode;
var
  Vitesses, CstesIntegration1,
  Positions, CstesIntegration2: TEnsembledeMesures3x3x3;
begin
  FListeMesuresAIntegrer.IntegrerSurIntervalleDeTemps(Vitesses, CstesIntegration1);
  StaticText4.Caption := format('%.8f', [Vitesses.Acceleration.ValueX]);
  StaticText5.Caption := format('%.8f', [Vitesses.Acceleration.ValueY]);
  StaticText6.Caption := format('%.8f', [Vitesses.Acceleration.ValueZ]);

  FListeMesuresAIntegrer.SurIntegrerSurIntervalleDeTemps(Positions, CstesIntegration2);
  StaticText7.Caption := format('%.8f', [Positions.Acceleration.ValueX]);
  StaticText8.Caption := format('%.8f', [Positions.Acceleration.ValueY]);
  StaticText9.Caption := format('%.8f', [Positions.Acceleration.ValueZ]);
  //*)
end;


function TForm1.IsAllValidVectors(const Mx, My, Mz, Gx, Gy, Gz, Ax, Ay, Az: double): boolean;
begin
  Result := IsValidMagneticFieldVector(Mx, My, Mz) and
            IsValidAccelerationVector(Gx, Gy, Gz) and
            IsValidAngularRateVector(Ax, Ay, Az);
end;

function TForm1.IsAllValidVectors(const SM: TEnsembledeMesures3x3x3): boolean;
begin
  Result := IsAllValidVectors(SM.MagneticField.ValueX, SM.MagneticField.ValueY, SM.MagneticField.ValueZ,
                              SM.Acceleration.ValueX, SM.Acceleration.ValueY, SM.Acceleration.ValueZ,
                              SM.AngularRate.ValueX, SM.AngularRate.ValueY, SM.AngularRate.ValueZ);
end;



procedure TForm1.Timer1Timer(Sender: TObject);
begin
  {$IFDEF USE_DATA_HANDLER}
  ;
  {$ELSE}
  DoAcquisitionValeurs();
  {$ENDIF}
end;

procedure TForm1.trkbarTimerIntervalChange(Sender: TObject);
begin
  Timer1.Interval := trkbarTimerInterval.Position;
end;




procedure TForm1.btnAcquisitionEnCoursChange(Sender: TObject);
begin
  FSpatialReading := btnAcquisitionEnCours.Checked;
  if ((FSpatialReady) and (not FSpatialReading)) then
  begin
    MoyennerLesMesures();

    IntegrerSurPeriode();

    // et on vide la liste pour une autre mesure
    FListeMesuresAIntegrer.ClearListe();

  end;
end;

procedure TForm1.btnConnecterClick(Sender: TObject);
var
  QPhid: CPhidgetHandle;
  phid: CPhidgetHandle;
  H_CPhidget_open: FARPROC;
  R, EWE: CInteger;
  QDeviceVersion, QSerialNumber: CInteger;
  i, nb: Integer;

begin
  if (FSpatialReady) then Exit;     // déjà connecté -->[]


  FSpatialReady := ConnectPhidget();
  if (FSpatialReady) then
  begin
    DisplayInfosDevice();
  end
  else
  begin
    memo1.Lines.Add('device not found');
  end;

end;

procedure TForm1.btnDeconnecterClick(Sender: TObject);
begin
  CloseAndDestroyPhidget();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CPhidgetSpatial_zeroGyro(FSpatial);
end;




function TForm1.ConnectPhidget: boolean;
const
  TIMEOUT_DELAY_IN_SECONDS = 10;
var
  R: CInteger;
begin
  memo1.Lines.Add(format('%s.ConnectPhidget()', [ClassName]));
  FSpatial := 0;
  FSpatialReady := false;
	//create the spatial object
	R := CPhidgetSpatial_create(@FSpatial);
  if (R <> 0) then exit(false);

  memo1.Lines.Add(format('Return value of %s = %d', ['CPhidgetSpatial_create', R]));
  R := CPhidget_set_OnAttach_Handler(FSpatial, @AttachHandler, nil);
  R := CPhidget_set_OnDetach_Handler(FSpatial, @DetachHandler, nil);
  R := CPhidget_set_OnError_Handler(FSpatial, @ErrorHandler, nil);
  {$IFDEF USE_DATA_HANDLER}
  R := CPhidgetSpatial_set_OnSpatialData_Handler(FSpatial, @FuncSpatialDataHandler, nil);
  {$ELSE}
  R := CPhidgetSpatial_set_OnSpatialData_Handler(FSpatial, nil , nil);
  {$ENDIF USE_DATA_HANDLER}
  R := CPhidget_open(FSpatial, -1);
  if (R <> 0) then exit(false);

  memo1.Lines.Add(format('Return value of %s = %d', ['CPhidgetSpatial_open', R]));
  R := CPhidget_waitForAttachment(FSpatial, 1000 * TIMEOUT_DELAY_IN_SECONDS);
  if (R <> 0) then  exit(false);

  memo1.Lines.Add(format('Return value of %s = %d', ['CPhidget_waitForAttachment', R]));
  // on vide la liste des mesures
  //FListeMesuresAIntegrer.ClearListe();
  result := true;
end;

procedure TForm1.DisplayInfosDevice;
var
  QDeviceVersion, QSerialNumber, EWE: CInteger;
  QDeviceName, QLibraryVersion, QDeviceType, QDeviceLabel: PChar;

begin

  memo1.Lines.Add('Library');
  memo1.Lines.Add('=========');
  EWE := CPhidget_getLibraryVersion(@QLibraryVersion);
  memo1.Lines.Add(Format(' version: %d - %s', [EWE, QLibraryVersion]));
  memo1.Lines.Add('');

  memo1.Lines.Add('Device properties');
  memo1.Lines.Add('==================');
  memo1.Lines.Add('');
  EWE := CPhidget_getDeviceName(FSpatial, @QDeviceName);
  memo1.Lines.Add(Format(' Device Name : %d - %s', [EWE, QDeviceName]));

  EWE := CPhidget_getDeviceType(FSpatial, @QDeviceType);
  memo1.Lines.Add(Format(' Device Type : %d - %s', [EWE, QDeviceType]));

  CPhidget_setDeviceLabel(FSpatial, @QDeviceLabel);

  EWE := CPhidget_getDeviceLabel(FSpatial, @QDeviceLabel);
  memo1.Lines.Add(Format(' Device Label : %d - %s', [EWE, QDeviceLabel]));

  EWE := CPhidget_getDeviceVersion(FSpatial, @QDeviceVersion);
  memo1.Lines.Add(Format(' Device Version : %d - %d', [EWE, QDeviceVersion]));
  EWE := CPhidget_getSerialNumber(FSpatial, @QSerialNumber);
  memo1.Lines.Add(Format(' Serial number: %d - %d', [EWE, QSerialNumber]));
  // data rates
  memo1.Lines.Add('');
  memo1.Lines.Add('Data Rate');
  CPhidgetSpatial_getDataRate(FSpatial, @FDataRateInMilliSeconds);
  CPhidgetSpatial_getDataRateMin(FSpatial, @FDataRateMin);
  CPhidgetSpatial_getDataRateMax(FSpatial, @FDataRateMax);


  memo1.Lines.Add(Format('%d < %d < %d', [FDataRateMin, FDataRateInMilliSeconds, FDataRateMax]));

  memo1.Lines.Add('');
  memo1.Lines.Add('Values ranges');
  CPhidgetSpatial_getMagneticFieldMin(FSpatial, 0, @FMagneticFieldMinX);
  CPhidgetSpatial_getMagneticFieldMin(FSpatial, 1, @FMagneticFieldMinY);
  CPhidgetSpatial_getMagneticFieldMin(FSpatial, 2, @FMagneticFieldMinZ);

  CPhidgetSpatial_getMagneticFieldMax(FSpatial, 0, @FMagneticFieldMaxX);
  CPhidgetSpatial_getMagneticFieldMax(FSpatial, 1, @FMagneticFieldMaxY);
  CPhidgetSpatial_getMagneticFieldMax(FSpatial, 2, @FMagneticFieldMaxZ);


  CPhidgetSpatial_getAccelerationMin(FSpatial, 0, @FAccelerationMinX);
  CPhidgetSpatial_getAccelerationMin(FSpatial, 1, @FAccelerationMinY);
  CPhidgetSpatial_getAccelerationMin(FSpatial, 2, @FAccelerationMinZ);

  CPhidgetSpatial_getAccelerationMax(FSpatial, 0, @FAccelerationMaxX);
  CPhidgetSpatial_getAccelerationMax(FSpatial, 1, @FAccelerationMaxY);
  CPhidgetSpatial_getAccelerationMax(FSpatial, 2, @FAccelerationMaxZ);

  CPhidgetSpatial_getAngularRateMin(FSpatial, 0, @FAngularRateMinX);
  CPhidgetSpatial_getAngularRateMin(FSpatial, 1, @FAngularRateMinY);
  CPhidgetSpatial_getAngularRateMin(FSpatial, 2, @FAngularRateMinZ);

  CPhidgetSpatial_getAngularRateMax(FSpatial, 0, @FAngularRateMaxX);
  CPhidgetSpatial_getAngularRateMax(FSpatial, 1, @FAngularRateMaxY);
  CPhidgetSpatial_getAngularRateMax(FSpatial, 2, @FAngularRateMaxZ);

  memo1.Lines.Add(Format('%.5f < MagneticFieldX < %.5f', [FMagneticFieldMinX, FMagneticFieldMaxX]));
  memo1.Lines.Add(Format('%.5f < MagneticFieldY < %.5f', [FMagneticFieldMinY, FMagneticFieldMaxY]));
  memo1.Lines.Add(Format('%.5f < MagneticFieldZ < %.5f', [FMagneticFieldMinZ, FMagneticFieldMaxZ]));

  memo1.Lines.Add(Format('%.5f < AccelerationX < %.5f', [FAccelerationMinX, FAccelerationMaxX]));
  memo1.Lines.Add(Format('%.5f < AccelerationY < %.5f', [FAccelerationMinY, FAccelerationMaxY]));
  memo1.Lines.Add(Format('%.5f < AccelerationZ < %.5f', [FAccelerationMinZ, FAccelerationMaxZ]));

  memo1.Lines.Add(Format('%.5f < AngularRateX < %.5f', [FAngularRateMinX, FAngularRateMaxX]));
  memo1.Lines.Add(Format('%.5f < AngularRateY < %.5f', [FAngularRateMinY, FAngularRateMaxY]));
  memo1.Lines.Add(Format('%.5f < AngularRateZ < %.5f', [FAngularRateMinZ, FAngularRateMaxZ]));

  memo1.Lines.Add('====================================================');



end;

procedure TForm1.DoAcquisitionValeurs;
var
  QMx, QMy, QMz, QGx, QGy, QGz, QAx, QAy, QAz: CDouble;
  MM: TEnsembledeMesures3x3x3;
begin

  if (FSpatialReady) then
  begin
    CPhidgetSpatial_getMagneticField(FSpatial, 0, @QMx);
    CPhidgetSpatial_getMagneticField(FSpatial, 1, @QMy);
    CPhidgetSpatial_getMagneticField(FSpatial, 2, @QMz);

    CPhidgetSpatial_getAcceleration(FSpatial, 0, @QGx);
    CPhidgetSpatial_getAcceleration(FSpatial, 1, @QGy);
    CPhidgetSpatial_getAcceleration(FSpatial, 2, @QGz);

    CPhidgetSpatial_getAngularRate(FSpatial, 0, @QAx);
    CPhidgetSpatial_getAngularRate(FSpatial, 1, @QAy);
    CPhidgetSpatial_getAngularRate(FSpatial, 2, @QAz);
    MM := MakeTEnsembledeMesures3x3x3(QMx,
                                      QMy,
                                      QMz,
                                      QGx, // * ACCEL_PESANTEUR,
                                      QGy, // * ACCEL_PESANTEUR,
                                      QGz, // * ACCEL_PESANTEUR,
                                      QAx,
                                      QAy,
                                      QAz,
                                      Now());
    if (IsAllValidVectors(MM)) then
    begin
      CdrBoussole1.SetBearing(MM.Bearing); //MM.Tilt;
      CdrClino1.SetTilt(MM.Tilt);
      if (FSpatialReading) then
      begin
        FListeMesuresAIntegrer.AddMesure(MM);
        if (FListeMesuresAIntegrer.GetLastMesure(MM)) then
        begin;
          memo2.Lines.Add(Format('--  MM = %.5f, %.5f, %.5f - G = %.5f, %.5f, %.5f - A = %.5f, %.5f, %.5f - Azimut = %.5f - Tilt = %.5f',
                               [MM.MagneticField.ValueX, MM.MagneticField.ValueY, MM.MagneticField.ValueZ,
                                MM.Acceleration.ValueX, MM.Acceleration.ValueY, MM.Acceleration.ValueZ,
                                MM.AngularRate.ValueX, MM.AngularRate.ValueY, MM.AngularRate.ValueZ,
                                MM.Bearing, MM.Tilt]));
        end;
      end;
    end;
  end;
end;

procedure TForm1.CloseAndDestroyPhidget;
var
  EWE: CInteger;
begin
  if (FSpatialReady) then
  begin
    memo1.Lines.Add('Fermeture et libération');
    try
      FSpatialReading := false;
      try
        EWE := CPhidgetSpatial_set_OnSpatialData_Handler(FSpatial, nil , nil);
        memo1.Lines.Add(Format('CPhidgetSpatial_set_OnSpatialData_Handler(): %d', [EWE]));
        EWE := CPhidget_close(FSpatial);
        memo1.Lines.Add(Format('CPhidget_close(): %d', [EWE]));
	      EWE := CPhidget_delete(FSpatial);
        memo1.Lines.Add(Format('CPhidget_delete(): %d', [EWE]));
      except
        memo1.Lines.Add('CPhidget déjà fermé');
      end;
    finally
      FSpatialReady   := false;  // et on désactive dans tous les cas
    end;
  end;
end;
end.


