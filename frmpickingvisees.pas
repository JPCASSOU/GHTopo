unit frmPickingVisees;
// Fenêtre de sélection de visées issues de tableurs ou d'un DistoX

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, Common, StructuresDonnees, ToporobotClasses2012, unitUtilsComposants,
  {$IFDEF MSWINDOWS}
    ++nunitDistoXWindows,
  {$ENDIF}
  {$IFDEF LINUX}
    unitDistoXLinux,
  {$ENDIF}
  SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs,
  Grids, ActnList, Buttons, StdCtrls, Menus, ExtCtrls;

type

  { TdlgPickingVisees }

  TdlgPickingVisees = class(TForm)
    acArmerDistoX: TAction;
    acSaveGrilleToFile: TAction;
    acOuvrirVisees: TAction;
    acAcquerirMesure: TAction;
    ActionList1: TActionList;
    btnDemarrerDistoX: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    chkModeScan: TCheckBox;
    cmbPorts: TComboBox;
    editIntervalleScan: TCurrencyEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    lbActionsEnCours: TStaticText;
    lbDistoReady: TStaticText;
    grdDonneesVisees: TStringGrid;
    lbDistoXReadingData: TStaticText;
    lbDistoXScanningActive: TStaticText;
    timerScanTimer: TTimer;
    procedure acAcquerirMesureExecute(Sender: TObject);
    procedure acArmerDistoXExecute(Sender: TObject);
    procedure acOpenCSVExecute(Sender: TObject);
    procedure acOuvrirViseesExecute(Sender: TObject);
    procedure acSaveGrilleToFileExecute(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure chkModeScanChange(Sender: TObject);
    procedure editIntervalleScanChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure grdDonneesViseesDblClick(Sender: TObject);
    procedure timerScanTimerTimer(Sender: TObject);
  private
    { private declarations }
    FMyDocToporobot: TToporobotStructure2012;
    FPilotageDistoX: TPilotageDistoX;
    FDistoX_OK     : boolean;
    FModeScanner   : boolean;

  public
    { public declarations }
    function Initialiser(const FD: TToporobotStructure2012): boolean;

  end;

//var
//  dlgPickingVisees: TdlgPickingVisees;

implementation

{$R *.lfm}
uses
  CallDialogsStdVersion;

{ TdlgPickingVisees }

procedure TdlgPickingVisees.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
   CanClose:=False;
  ShowMessage(_AnsiToLCLStr(rsNOCANCLOSEWND));
end;

procedure TdlgPickingVisees.grdDonneesViseesDblClick(Sender: TObject);
var
  EWE: String;
begin
  EWE := grdDonneesVisees.Cells[4, grdDonneesVisees.Row];
  if (Trim(EWE) <> 'X') then grdDonneesVisees.Cells[4, grdDonneesVisees.Row] := 'X'
                        else grdDonneesVisees.Cells[4, grdDonneesVisees.Row] := EWE;
end;

procedure TdlgPickingVisees.timerScanTimerTimer(Sender: TObject);
begin
  if (FModeScanner) then
  begin
    if (FPilotageDistoX.IsReady) then FPilotageDistoX.TrigMesure;
    Application.ProcessMessages;
  end;
end;




procedure TdlgPickingVisees.acArmerDistoXExecute(Sender: TObject);
var
  EWE: String;
begin
  if (FDistoX_OK) then
  begin
    try
      FPilotageDistoX.SetReadingTopoData(False);
      SetVoyantMode(lbDistoXReadingData, FPilotageDistoX.GetStatutReadingData);
      FPilotageDistoX.CloseLazSerial;
      SetVoyantMode(lbDistoReady, FPilotageDistoX.IsReady);
      FDistoX_OK := False;
      acArmerDistoX.Caption := 'Ouvrir le DistoX';
    except
    end;
  end
  else
  begin
    try
      EWE := trim(cmbPorts.Text);
      FPilotageDistoX.SetLazSerialDevice(EWE);
      FPilotageDistoX.SetLabelActionsProcessing(lbActionsEnCours);
      FPilotageDistoX.SetOutputDataViseesGrid(grdDonneesVisees);
      FPilotageDistoX.SetOutputDataGMGrid(nil);

      FPilotageDistoX.OpenLazSerial;
      SetVoyantMode(lbDistoReady, FPilotageDistoX.IsReady);
      FPilotageDistoX.SetReadingTopoData(True);
      SetVoyantMode(lbDistoXReadingData, FPilotageDistoX.GetStatutReadingData);
      FDistoX_OK := True;
      acArmerDistoX.Caption := 'Fermer le DistoX';
    except
    end;
  end;
end;

procedure TdlgPickingVisees.acAcquerirMesureExecute(Sender: TObject);
begin
  if (FPilotageDistoX.IsReady) then FPilotageDistoX.TrigMesure;
end;

procedure TdlgPickingVisees.acOpenCSVExecute(Sender: TObject);
begin

end;

procedure TdlgPickingVisees.acOuvrirViseesExecute(Sender: TObject);
begin

end;

procedure TdlgPickingVisees.acSaveGrilleToFileExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  QFileName := Format('DistoX001_%s.csv', [DatePascalToDateHeureCondensee(Now())]);
  if (DoDialogSaveFile('Fichier CSV |*.csv|Tous|*.*', '.csv', QFileName, QFilterIndex)) then
  begin
    GRDSauverUnTableauEnCSV(grdDonneesVisees, QFileName);
  end;
end;

procedure TdlgPickingVisees.Button4Click(Sender: TObject);
var
  WU: Cardinal;
begin
  WU := trunc(editIntervalleScan.Value * 1000);
  if (WU < 5) then WU := 5;
  timerScanTimer.Interval := WU;
end;

procedure TdlgPickingVisees.chkModeScanChange(Sender: TObject);
begin
  FModeScanner := chkModeScan.Checked;
  SetVoyantMode(lbDistoXScanningActive, FModeScanner);
end;

procedure TdlgPickingVisees.editIntervalleScanChange(Sender: TObject);
begin

end;



procedure TdlgPickingVisees.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try
    FModeScanner := False;
    FPilotageDistoX.CloseLazSerial;
  finally
  end;
end;

function TdlgPickingVisees.Initialiser(const FD: TToporobotStructure2012): boolean;
var
  i, NbPorts: Integer;
begin
  result := False;
  try
    // Document TOPOROBOT
    FMyDocToporobot := FD;
    self.Caption    := FMyDocToporobot.GetDatabaseName;
    // grille
    grdDonneesVisees.ColCount := 5;
    grdDonneesVisees.RowCount := 2000;
    grdDonneesVisees.Cells[0, 0] := 'No';
    grdDonneesVisees.Cells[1, 0] := 'Long';
    grdDonneesVisees.Cells[2, 0] := 'Azimut';
    grdDonneesVisees.Cells[3, 0] := 'Incl';
    grdDonneesVisees.Cells[4, 0] := 'Selected';

    for i := 1 to grdDonneesVisees.RowCount - 1 do grdDonneesVisees.Cells[0, i] := Format(FORMAT_NB_INTEGER, [i]);
    // ports COM
    NbPorts := 32;
    cmbPorts.Clear;
    cmbPorts.DropDownCount := NbPorts;
    for i := 0 to NbPorts - 1 do cmbPorts.Items.Add(Format('COM%d', [i]));
    cmbPorts.ItemIndex := 5;   // pour DistoX
    // pilote du DistoX
    {$IFDEF MSWINDOWS}
      FPilotageDistoX := TPilotageDistoX.Create(Application);
    {$ENDIF}
    {$IFDEF LINUX}
      FPilotageDistoX := TPilotageDistoX.Create;
    {$ENDIF}
    {$IFDEF MACOSX}
      FPilotageDistoX := TPilotageDistoX.Create;
    {$ENDIF}
    FDistoX_OK      := False;

    Result          := True;
    // mode Scanner
    editIntervalleScan.Value := 8.00; // intervalle 8 secondes
    FModeScanner    := False;
    SetVoyantMode(lbDistoXScanningActive, FModeScanner);
    timerScanTimer.Interval := trunc(editIntervalleScan.Value * 1000);


  except

  end;
end;

end.

