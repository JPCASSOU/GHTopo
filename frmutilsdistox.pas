unit frmUtilsDistoX;
// Calibration d'un Disto X
// C'est aussi un exercice de Bluetooth

(*
// write data to port
    function WriteData(data: string): integer;
    function WriteBuffer(var buf; size: integer): integer;
    function WriteByte(const B: Byte): integer;

function TLazSerial.WriteByte(const B: byte): Integer;
begin
  try
    Result := 0;
    if FSynSer.Handle=INVALID_HANDLE_VALUE then
       ComException('can not write to a closed port.');
    FSynSer.SendByte(B);
  except
  end;
end;


//*)

{$INCLUDE CompilationParameters.inc}

interface

uses
  ++
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  //windows,
  StructuresDonnees,
  Common,
  //ToporobotClasses2012,
  CallDialogsStdVersion,
  unitUtilsComposants,
  //LazSerial,
  Classes, SysUtils,
  FileUtil, curredit, LazSerial, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, Grids, ComCtrls, Spin,
  LCLType, Clipbrd,
  {$IFDEF MSWINDOWS}
    unitdistox, types;
  {$ENDIF}
  {$IFDEF LINUX}
    unitDistoXLinux;
  {$ENDIF}
type

  { TdlgUtilsDistoX }

  TdlgUtilsDistoX = class(TForm)
    btnCalcCalibration: TButton;
    btnClose: TBitBtn;
    btnControleCalibrateur: TButton;
    btnLireMemoireMesuresDistoX: TButton;
    btnMesure: TButton;
    btnOpenDistoX: TButton;
    btnCloseDistoX: TButton;
    btnOuvrirCSV: TButton;
    btnStopDLDataDistoX: TButton;
    btnViderListeVisees: TButton;
    btnSaveDonneesBrutes: TButton;
    btnSaveValeursCalibDistoX: TButton;
    btnSendParamsCalibrationToDistoX: TButton;
    Button1: TButton;
    btnSauvegarder: TButton;
    Button2: TButton;
    btnCopyMemoText: TButton;
    btnReadParamCalibration: TButton;
    btnCheckCalibration: TButton;
    Button3: TButton;
    btnGetNumSerieDistoX: TButton;
    Button6: TButton;
    chkNonLinearCalibration: TCheckBox;
    chkCalibrationMode: TCheckBox;
    chkLaserMode: TCheckBox;
    chkReadingData: TCheckBox;
    cmbPorts: TComboBox;
    editBlockDebut: TSpinEdit;
    editBlockFin: TSpinEdit;
    editIntervalleLecture: TCurrencyEdit;
    grdMesuresGMDistoX: TStringGrid;
    grdViseesDistoX: TStringGrid;
    imgLabelBouse: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbDistoXCalibrationMode: TStaticText;
    lbDistoXLaserStatus: TStaticText;
    lbDistoXReadingData: TStaticText;
    lbOperationEnCours: TLabel;
    lbQualiteCalibration: TStaticText;
    memoMesuresBrutes: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    lbDistoReady: TStaticText;
    lbActionsEnCours: TStaticText;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlDistoXActive: TPanel;
    pnlCalibration: TPanel;
    ProgressBar1: TProgressBar;
    TabSheet1: TTabSheet;
    tabShtMesuresBrutes: TTabSheet;
    tbshtCalibration: TTabSheet;
    tbshtViseesDistoX: TTabSheet;
    procedure btnCheckCalibrationClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyMemoTextClick(Sender: TObject);
    procedure btnMesureClick(Sender: TObject);
    procedure btnOpenDistoXClick(Sender: TObject);
    procedure btnCloseDistoXClick(Sender: TObject);
    procedure btnExtinctDistoXClick(Sender: TObject);
    procedure btnOuvrirCSVClick(Sender: TObject);
    procedure btnSaveDonneesBrutesClick(Sender: TObject);
    procedure btnSaveTableauGHTopoClick(Sender: TObject);
    procedure btnSaveTableauViseesRayonnantesClick(Sender: TObject);
    procedure btnSauvegarderClick(Sender: TObject);
    procedure btnStopDLDataDistoXClick(Sender: TObject);
    procedure btnViderListeViseesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnSaveValeursCalibDistoXClick(Sender: TObject);
    procedure btnSaveValeursViseesDistoXClick(Sender: TObject);
    procedure btnAfficherDonneesExempleClick(Sender: TObject);
    procedure btnCalcCalibrationClick(Sender: TObject);
    procedure btnSendParamsCalibrationToDistoXClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnReadParamCalibrationClick(Sender: TObject);
    procedure btnLireMemoireMesuresDistoXClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnGetNumSerieDistoXClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure chkCalibrationModeChange(Sender: TObject);
    procedure chkLaserModeChange(Sender: TObject);
    procedure chkReadingDataChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure imgLabelBouseClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure pnlDistoXActiveClick(Sender: TObject);
    procedure tbshtCalibrationContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tbshtViseesDistoXContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    //procedure LazSerial1Status(Sender: TObject; Reason: THookSerialReason; const Value: string);
  private
    { private declarations }
    fMatriceA: TMatrix3x3;
    fMatriceB: TMatrix3x3;
    fMatriceC: TMatrix3x3;
    // document GHTopo
    //FDocuTopo: TToporobotStructure2012;
    // Pilote du DistoX
    FPilotageDistoX: TPilotageDistoX;
    //FPiloteDistoXOK: boolean;
    // Com DLL
    //FCommPortOpened :boolean;
    //FBufferStr      : string;
    //FNbBytesRead: integer;
    //-----------------------
    // pour GHTopo DistoX
    procedure AfficherProgressOperationMemoireDistoX(const Etape: string; const Done, Starting, Ending: Integer);
    procedure AfficherGrdMesuresDistoX();
    procedure PutMatrixInGrid(const grd: TStringGrid; const M: TMatrix3x3);
  public
    { public declarations }
    function InitialiserDialogDistoX(): boolean;
    procedure FinaliserCalib;
  end;

var
  dlgUtilsDistoX: TdlgUtilsDistoX;

implementation

{$R *.lfm}

{ TdlgUtilsDistoX }
const
  NUM_COL_IDTERRAIN = 1;
  NUM_COL_SECTEUR = 2;
  NUM_COL_TYPEGAL = 3;
  NUM_COL_CODE = 1 + NUM_COL_TYPEGAL;
  NUM_COL_EXPE = 1 + NUM_COL_CODE;
  NUM_COL_L = 1 + NUM_COL_EXPE;
  NUM_COL_A = 2 + NUM_COL_EXPE;
  NUM_COL_P = 3 + NUM_COL_EXPE;
  NUM_COL_LG = 4 + NUM_COL_EXPE;
  NUM_COL_LD = 5 + NUM_COL_EXPE;
  NUM_COL_HZ = 6 + NUM_COL_EXPE;
  NUM_COL_HN = 7 + NUM_COL_EXPE;
  NUM_COL_OBS = 8 + NUM_COL_EXPE;
  NUM_COL_HORODATE = 1 + NUM_COL_OBS;
  NUM_COL_TRAME    = 1 + NUM_COL_HORODATE;
  WDTH_COL_SECTEUR = 40;
  WDTH_COL_CODE = 40;
  WDTH_COL_EXPE = 40;
  WDTH_COL_L = 120;
  WDTH_COL_AZ = 80;
  WDTH_COL_P = 80;
  WDTH_COL_LG = 40;
  WDTH_COL_LD = WDTH_COL_LG;
  WDTH_COL_HZ = WDTH_COL_LG;
  WDTH_COL_HN = WDTH_COL_LG;
  WDTH_COL_OBS = 160;
  WDTH_COL_HORODATE = 200;
  WDTH_COL_TRAME    = 320;

const
  COLOR_VOYANT_INACTIVE = clYellow;
  COLOR_VOYANT_READY    = clGreen;
  COLOR_VOYANT_FAILED   = clRed;

procedure TdlgUtilsDistoX.btnCloseClick(Sender: TObject);
begin
  self.close;
end;

procedure TdlgUtilsDistoX.btnCopyMemoTextClick(Sender: TObject);
var
  MyClipBoard: TClipboard;
begin
  MyClipBoard := TClipboard.Create(ctClipboard);
  MyClipBoard.AsText := memoMesuresBrutes.Text;
end;







procedure TdlgUtilsDistoX.btnMesureClick(Sender: TObject);
begin
  if (FPilotageDistoX.Active) then
  begin
    chkReadingData.Checked := True;
    FPilotageDistoX.DemanderUneMesure();
  end;
end;




procedure TdlgUtilsDistoX.btnOpenDistoXClick(Sender: TObject);
var
  EWE: String;
  i: Integer;
  WU: Boolean;
begin
  EWE := trim(cmbPorts.Text);
  FPilotageDistoX.SetLazSerialDevice(EWE);
  FPilotageDistoX.SetLabelActionsProcessing(lbActionsEnCours);
  FPilotageDistoX.SetProcProgression(AfficherProgressOperationMemoireDistoX);
  for i := 1 to 10 do
  begin
    lbActionsEnCours.Caption := Format('Connexion au DistoX: Tentative %d / %d', [i, 10]);
    FPilotageDistoX.OpenDistoXConnexion();
    WU := FPilotageDistoX.Active;
    if (WU) then
    begin
      SetVoyantMode(lbDistoReady, WU);
      pnlDistoXActive.Enabled := WU;
      Exit;
    end;
    Sleep(2000);
    SetVoyantMode(lbDistoReady, false);
    pnlDistoXActive.Enabled := false;
  end;
end;

procedure TdlgUtilsDistoX.btnCloseDistoXClick(Sender: TObject);
begin
  FPilotageDistoX.CloseDistoXConnexion();
  SetVoyantMode(lbDistoReady, FPilotageDistoX.Active);
  pnlDistoXActive.Enabled := false;
end;

procedure TdlgUtilsDistoX.btnExtinctDistoXClick(Sender: TObject);
begin
  FPilotageDistoX.ExtinctDevice;
  SetVoyantMode(lbDistoReady, false);
  pnlDistoXActive.Enabled := false;
end;

procedure TdlgUtilsDistoX.btnOuvrirCSVClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
begin
  QFileName := '';
  if (GHTopoQuestionOuiNon('Données non enregistrées - Continuer')) then
  begin
    if (DoDialogOpenFile('Fichiers CSV (*.csv)|*.csv|Tous (*.*)|*.*', '.csv', QFileName)) then
    begin
      FPilotageDistoX.ChargerDonneesViseesBrutes(QFileName);
      AfficherGrdMesuresDistoX();
      //FPilotageDistoX.afficherViseesDansGrille(grdViseesDistoX);
    end;
  end;
end;

procedure TdlgUtilsDistoX.btnSaveDonneesBrutesClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := Format('DistoX_Data_%s.txt', [DatePascalToDateHeureCondensee(Now())]);
  if (DoDialogSaveFile('Fichier texte (*.txt)|*.txt', '.txt', QFileName, QFilterIndex)) then
  begin
    memoMesuresBrutes.Lines.SaveToFile(QFileName);
  end;
end;

procedure TdlgUtilsDistoX.btnSaveTableauGHTopoClick(Sender: TObject);
begin

end;

procedure TdlgUtilsDistoX.btnSaveTableauViseesRayonnantesClick(Sender: TObject);
begin

end;



procedure TdlgUtilsDistoX.btnSauvegarderClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := Format('Donnees_DistoX_%s.csv', [DatePascalToDateHeureCondensee(Now())]);
  if (DoDialogSaveFile('Fichiers csv|*.csv', '.csv', QFileName, QFilterIndex)) then
  begin
    FPilotageDistoX.SauverDonneesViseesBrutes(QFileName);
  end;
end;






procedure TdlgUtilsDistoX.Button1Click(Sender: TObject);
begin
  FPilotageDistoX.GetDataGMFromGrid(grdMesuresGMDistoX);
end;

procedure TdlgUtilsDistoX.Button2Click(Sender: TObject);
begin
  AfficherGrdMesuresDistoX();
end;

procedure TdlgUtilsDistoX.Button3Click(Sender: TObject);
var
  MyBuffer: TBootloaderModeDataBuffer;
begin
  //FPilotageDistoX.BootloaderReadAnPacket264($B400, MyBuffer);
end;

procedure TdlgUtilsDistoX.btnReadParamCalibrationClick(Sender: TObject);
begin
  FPilotageDistoX.ReadCalibrationParametersFromDistoX();
end;

procedure TdlgUtilsDistoX.btnGetNumSerieDistoXClick(Sender: TObject);
begin
  Showmessage(Format('DistoX%d - Firmware = %.2f - Hardware = %d',
                                     [FPilotageDistoX.ExtractNumSerieFromDistoX(),
                                      FPilotageDistoX.ExtractVersionFirmWareFromDistoX() / 100.0,
                                      FPilotageDistoX.ExtractVersionHardwareFromDistoX()]));
end;


procedure TdlgUtilsDistoX.btnLireMemoireMesuresDistoXClick(Sender: TObject);
var
  V0, V1: Integer;
begin
  V0 := editBlockDebut.Value;
  V1 := editBlockFin.Value;
  if (V1 < V0) then
  begin
    Swap(V0, V1);
    editBlockDebut.Value := V0;
    editBlockFin.Value   := V1;
  end;
  btnStopDLDataDistoX.Enabled := True;
  btnLireMemoireMesuresDistoX.Enabled := false;
  PageControl1.ActivePageIndex := 0;
  try
    FPilotageDistoX.ReadDataSection(V0, V1);
    AfficherGrdMesuresDistoX();
  finally
    btnLireMemoireMesuresDistoX.Enabled := True;
    btnStopDLDataDistoX.Enabled := False;
  end;
end;

procedure TdlgUtilsDistoX.btnSaveValeursCalibDistoXClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := Format('Calib_DistoX_%s.csv', [DatePascalToDateHeureCondensee(Now())]);
  if (DoDialogSaveFile('Fichiers csv|*.csv', '.csv', QFileName, QFilterIndex)) then
  begin
    grdMesuresGMDistoX.SaveToCSVFile(QFileName, #9, True)
  end;
end;

procedure TdlgUtilsDistoX.btnSaveValeursViseesDistoXClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := Format('Valeurs_DistoX_%s.csv', [DatePascalToDateHeureCondensee(Now())]);
  if (DoDialogSaveFile('Fichiers csv|*.csv', '.csv', QFileName, QFilterIndex)) then
  begin
    grdViseesDistoX.SaveToCSVFile(QFileName, #9, True)
  end;
end;

procedure TdlgUtilsDistoX.btnAfficherDonneesExempleClick(Sender: TObject);
begin

end;

procedure TdlgUtilsDistoX.btnCalcCalibrationClick(Sender: TObject);
var
  EWE: Double;
begin
  FPilotageDistoX.SetNonLinearCalibration(chkNonLinearCalibration.Checked);
  FPilotageDistoX.CalculerCalibration();
  EWE := FPilotageDistoX.GetDelta;
  lbQualiteCalibration.Caption := Format(FORMAT_NB_REAL_3_DEC, [EWE]);
end;

procedure TdlgUtilsDistoX.btnCheckCalibrationClick(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Ceci effacera les données de calibration en cours - Continuer')) then
  begin
    FPilotageDistoX.CheckMethodeCalibration();
    FPilotageDistoX.SetDataGMInGrid(grdMesuresGMDistoX);
  end;
end;


procedure TdlgUtilsDistoX.btnSendParamsCalibrationToDistoXClick(Sender: TObject);
var
  ErrNo: Integer;
begin
  if (FPilotageDistoX.IsReadyForCalibrate) then
  begin
    if (GHTopoQuestionOuiNon('Ceci modifiera la mémoire du DistoX - Continuer')) then
    begin
      //ErrNo := FPilotageDistoX.SendCalibrationParametersToDistoX;
    end;
  end
  else
  begin
    ShowMessage('Calibration incorrecte');
  end;
  //*)
end;

procedure TdlgUtilsDistoX.btnStopDLDataDistoXClick(Sender: TObject);
begin
  btnLireMemoireMesuresDistoX.Enabled := True;
  btnStopDLDataDistoX.Enabled         := false;
end;

procedure TdlgUtilsDistoX.btnViderListeViseesClick(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Vider la liste des stations')) then
  begin
    FPilotageDistoX.ViderListeMesuresTopo(True);
    AfficherGrdMesuresDistoX();
  end;
end;




procedure TdlgUtilsDistoX.Button6Click(Sender: TObject);
begin
  grdViseesDistoX.CopyToClipboard();
end;





procedure TdlgUtilsDistoX.chkCalibrationModeChange(Sender: TObject);
var
  EWE: Boolean;
begin
  if (FPilotageDistoX.Active) then
  begin
    // désarmer la lecture des données
    FPilotageDistoX.SetReadingTopoData(False);
    chkReadingData.Checked := false;
    FPilotageDistoX.SetLaserOnOff(false);
    chkLaserMode.Checked   := false;
    // la suite
    EWE := chkCalibrationMode.Checked;
    FPilotageDistoX.SetCalibrationOnOff(EWE);
    if (EWE) then
    begin
      PageControl1.ActivePageIndex := 1;
    end
    else
    begin
      PageControl1.ActivePageIndex := 0;
    end;
    SetVoyantMode(lbDistoXCalibrationMode, FPilotageDistoX.IsCalibrationON());
  end;
end;

procedure TdlgUtilsDistoX.chkLaserModeChange(Sender: TObject);
begin
  if (FPilotageDistoX.Active) then FPilotageDistoX.SetLaserOnOff(chkLaserMode.Checked);
  SetVoyantMode(lbDistoXLaserStatus, FPilotageDistoX.IsLaserON());
end;

procedure TdlgUtilsDistoX.chkReadingDataChange(Sender: TObject);
begin
  FPilotageDistoX.SetReadingTopoData(chkReadingData.Checked);
  SetVoyantMode(lbDistoXReadingData, FPilotageDistoX.IsAcquiringTopoData());
  btnLireMemoireMesuresDistoX.Enabled := not (chkReadingData.Checked);
end;




procedure TdlgUtilsDistoX.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try
    FPilotageDistoX.CloseDistoXConnexion();
  except
  end;
end;

procedure TdlgUtilsDistoX.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := false;
  if (GHTopoQuestionOuiNon('Vous devez rallumer l''appareil distant - Continuer')) then CanClose := True;
end;

procedure TdlgUtilsDistoX.FormCreate(Sender: TObject);
begin

end;

procedure TdlgUtilsDistoX.imgLabelBouseClick(Sender: TObject);
begin
  ShowMessage('Titre décerné à la technologie Bluetooth dans son ensemble');
end;






procedure TdlgUtilsDistoX.PutMatrixInGrid(const grd: TStringGrid; const M: TMatrix3x3);
var
  i, j: integer;
  EWE: string;
begin
  AfficherMessage('Matrice');
  for i := 1 to 3 do
  begin
    EWE := '';
    for j := 1 to 3 do
    begin
       grd.Cells[j, i] := Format('%.8f', [M[i, j]]);
       EWE += Format('M%d%d = %8f, ', [i, j, M[i, j]]);

    end;
    AfficherMessage(EWE);
  end;

end;

procedure TdlgUtilsDistoX.tbshtCalibrationContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TdlgUtilsDistoX.tbshtViseesDistoXContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin

end;



procedure TdlgUtilsDistoX.AfficherGrdMesuresDistoX();
var
  i, Nb: Integer;
  EWE: TMesureViseeDistoX;
begin
  Nb := FPilotageDistoX.GetNbMesuresAcquises();
  if (Nb = 0) then Exit;
  grdViseesDistoX.RowCount := Nb + 1;
  for i := 0 to Nb-1 do
  begin
    EWE := FPilotageDistoX.GetMesureVisee(i);
    grdViseesDistoX.Cells[0, i+1] := Format(FORMAT_NB_INTEGER  , [i]);
    grdViseesDistoX.Cells[1, i+1] := Format(FORMAT_STRING  , [DateTimeToStr(EWE.TimeStamp)]);
    grdViseesDistoX.Cells[2, i+1] := Format(FORMAT_NB_INTEGER  , [EWE.DistoXSerialNumber]);
    grdViseesDistoX.Cells[3, i+1] := Format('%.2d:%.2d'  , [EWE.Block, EWE.Segment]);
    grdViseesDistoX.Cells[4, i+1] := Format(FORMAT_NB_REAL_3_DEC, [EWE.Longueur]);
    grdViseesDistoX.Cells[5, i+1] := Format(FORMAT_NB_REAL_3_DEC, [EWE.Azimut]);
    grdViseesDistoX.Cells[6, i+1] := Format(FORMAT_NB_REAL_3_DEC, [EWE.Pente]);
    grdViseesDistoX.Cells[7, i+1] := Format(FORMAT_STRING, [EWE.HexaData]);
  end;
end;

procedure TdlgUtilsDistoX.AfficherProgressOperationMemoireDistoX(const Etape: string; const Done, Starting, Ending: Integer);
begin
  ProgressBar1.Min := Starting;
  ProgressBar1.Max := Ending;
  ProgressBar1.Position := Done;
  lbOperationEnCours.Caption := Etape;
end;


function TdlgUtilsDistoX.InitialiserDialogDistoX(): boolean;
const
  NB_MAX_VISEES_DISTOX = 2000;
var
  MatResultat: TMatrix3x3;
  i: Integer;
begin
  //FDocuTopo := DT;

  self.Caption := Format(GetResourceString(rsUTILS_DISTOX_CAPTION), ['']);
  //FCommPortOpened := False;
  pnlDistoXActive.Enabled := false;
  Result := False;
  try
     //RemplirComboBoxCOMPortsWindows(cmbPorts, 6);
     // combobox vitesse
     (*
     cmbBaudrate.Clear;
     cmbBaudrate.Items.add('110');   // br___110
     cmbBaudrate.Items.add('300');   // br___300
     cmbBaudrate.Items.add('600');   // br___600
     cmbBaudrate.Items.add('1200');  // br__1200
     cmbBaudrate.Items.add('2400');  // br__2400
     cmbBaudrate.Items.add('4800');  // br__4800
     cmbBaudrate.Items.add('9600');  // br__9600
     cmbBaudrate.Items.add('14400'); // br_14400
     cmbBaudrate.Items.add('19200'); // br_19200
     cmbBaudrate.Items.add('38400'); // br_38400
     cmbBaudrate.Items.add('56000'); // br_56000
     cmbBaudrate.Items.add('57600'); // br_57600
     cmbBaudrate.Items.add('115200');   // br115200
     cmbBaudrate.Items.add('128000');   // br128000
     cmbBaudrate.Items.add('230400');   // br230400
     cmbBaudrate.Items.add('256000');   // br256000
     cmbBaudrate.Items.add('460800');   // br460800
     cmbBaudrate.Items.add('921600');   // br921600
     cmbBaudrate.ItemIndex := 6; // pour 9600
     // combobox Parité    TParity=(pNone,pOdd,pEven,pMark,pSpace);
     cmbParity.Clear;
     cmbParity.Items.Add('Non');
     cmbParity.Items.Add('Paire');
     cmbParity.Items.Add('Impaire');
     cmbParity.Items.Add('Mark');
     cmbParity.Items.Add('Space');
     cmbParity.ItemIndex := 0;
     // combobox bits     TDataBits=(db8bits,db7bits,db6bits,db5bits);
     cmbDataBits.Clear;
     for i := 0 to 3 do cmbDataBits.Items.Add(Format('%d bits', [8 - i]));
     cmbDataBits.ItemIndex := 0;
     // combobox stopbits  TStopBits=(sbOne,sbOneAndHalf,sbTwo);
     cmbStopBits.Clear;
     cmbStopBits.Items.Add('1.0 bits');
     cmbStopBits.Items.Add('1.5 bits');
     cmbStopBits.Items.Add('2.0 bits');
     cmbStopBits.ItemIndex := 0;

     // combobox Protocole   TFlowControl=(fcNone,fcXonXoff,fcHardware);
     cmbProtocole.Clear;
     cmbProtocole.Items.Add('Aucun');
     cmbProtocole.Items.Add('XON/XOFF');
     cmbProtocole.Items.Add('Hardware');
     cmbProtocole.ItemIndex := 0;
     //*)
          // grille de valeurs G et M
     grdMesuresGMDistoX.ColCount := 1 + 3 + 3 + 3;
     grdMesuresGMDistoX.RowCount := 1 + NB_MAX_VISEES_DISTOX;
     for i := 1 to NB_MAX_VISEES_DISTOX do grdMesuresGMDistoX.Cells[0, i] := Format(FORMAT_NB_INTEGER, [i]);
     // voyants
     SetVoyantMode(lbDistoXCalibrationMode, false);
     SetVoyantMode(lbDistoXReadingData    , false);
     SetVoyantMode(lbDistoXLaserStatus    , false);
     SetVoyantMode(lbDistoReady           , false);
     // grille de données pour GHTopo
     grdViseesDistoX.RowCount := GHTOPO_GRID_NB_LINES;
     // EWE := Format('[%s] Mesure %d: (Tir %d): L = %.3f, Az = %.3f, P = %.3f - %s',
     grdViseesDistoX.ColCount := 8;
     grdViseesDistoX.FixedCols:= 0;
     grdViseesDistoX.ColWidths[0] := 60;
     grdViseesDistoX.ColWidths[1] := LARGEUR_COLONNES_HORODATE;
     grdViseesDistoX.ColWidths[2] := LARGEUR_COLONNES_DISTOX_ID;
     grdViseesDistoX.ColWidths[3] := LARGEUR_COLONNES_BLOCK_SEGMENT;
     grdViseesDistoX.ColWidths[4] := LARGEUR_COLONNES_LAP;
     grdViseesDistoX.ColWidths[5] := LARGEUR_COLONNES_LAP;
     grdViseesDistoX.ColWidths[6] := LARGEUR_COLONNES_LAP;
     grdViseesDistoX.ColWidths[7] := LARGEUR_COLONNES_TRAME_HEXA;
     // Sous Delphi, utiliser l'option goAlwaysShowEditor
     grdViseesDistoX.Options := [goAlwaysShowEditor, goAutoAddRows,
                                 goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
                                 goRangeSelect, goEditing, goTabs]; // ,goAlwaysShowEditor
     grdViseesDistoX.Cells[0, 0] := '';
     grdViseesDistoX.Cells[1, 0] := 'Horodate';
     grdViseesDistoX.Cells[2, 0] := 'Appareil';
     grdViseesDistoX.Cells[3, 0] := 'Block';
     grdViseesDistoX.Cells[4, 0] := 'Longueur';
     grdViseesDistoX.Cells[5, 0] := 'Azimut';
     grdViseesDistoX.Cells[6, 0] := 'Pente';
     grdViseesDistoX.Cells[7, 0] := 'Trame';
     for i := 1 to grdViseesDistoX.RowCount - 1 do grdViseesDistoX.Cells[0, i] := Format(FORMAT_NB_INTEGER, [i]);
     // bac à sable
     (*
     fMatriceA := makeMatrix3x3ByValues(12,  44, 13,
                                       81, -57, 18,
                                       65,  35, -24);
     fMatriceB := makeMatrix3x3ByValues(45, -78, 13,
                                       47,  15, -9,
                                       -84,  4, -7);

     PutMatrixInGrid(grdMatriceA, fMatriceA);
     PutMatrixInGrid(grdMatriceB, fMatriceB);
     AfficherMessage('Produit matriciel A*B');
     MatResultat := ProduitMat3x3(fMatriceA, fMatriceB);
     PutMatrixInGrid(grdMatriceResultat, MatResultat);
     AfficherMessage('Inversion de A');
     MatResultat := InvMat3x3(fMatriceA);
     PutMatrixInGrid(grdMatriceC, MatResultat);
     AfficherMessage('Produit matriciel A*A^-1 = I');
     MatResultat := ProduitMat3x3(fMatriceA, MatResultat);
     PutMatrixInGrid(grdMatriceResultat, MatResultat);
     //*)
     result  := True;
     // démarrage du pilote du DistoX
     {$IFDEF MSWINDOWS}
     FPilotageDistoX := TPilotageDistoX.Create(Application);
     {$ELSE}
     FPilotageDistoX := TPilotageDistoX.Create;
     {$ENDIF}
     // onglet Visées
     PageControl1.ActivePageIndex := 0;

  except
  end;
end;



procedure TdlgUtilsDistoX.FinaliserCalib;
begin
  try

  finally
    FreeAndNil(FPilotageDistoX);//FPilotageDistoX.Free;
  end;
end;

//------------------------------------------------------------------------------


procedure TdlgUtilsDistoX.PageControl1Change(Sender: TObject);
begin

end;

procedure TdlgUtilsDistoX.Panel1Click(Sender: TObject);
begin

end;

procedure TdlgUtilsDistoX.pnlDistoXActiveClick(Sender: TObject);
begin

end;

end.

