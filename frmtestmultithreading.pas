unit frmTestMultithreading;
{$INCLUDE CompilationParameters.inc}
{$WARNING Inadapté}
interface
uses
  SyncObjs,
  BZParallelThread,
  //GHTopoMultiThreading3,
  StructuresDonnees,
  ToporobotClasses2012,
  Common,
  Classes, SysUtils, math, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, curredit;
type

{ TTableAntennes }

 TThreadedTableAntennes = class(TThreadList)
  private

  public
    procedure AddElement(const E: TViseeAntenne);
    function GetElement(const Idx: integer): TViseeAntenne;
    function GetNbElements(): integer;
    procedure ClearListe();
end;

//==============================================================================
Type

{ TThreadViseesEnAntennesProcessing }

  TThreadViseesEnAntennesProcessing = class(TThread)
private
  FProcProgression: TProcOfObjectUsesInteger;
  FIdxStart       : integer;
  FIdxEnd         : integer;
  FNoThread       : integer;
  FTableAntennes  : TThreadedTableAntennes;
  FAFinished       : boolean;

protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants
  procedure Execute; override;
public
  property Terminated; // cette propriété est 'protected' dans TThread
  Constructor Create(const TA: TThreadedTableAntennes;
                     const NoThread: integer;
                     const IdxStart, IdxEnd: integer;
                     const P: TProcOfObjectUsesInteger);
  function AttendPour(): integer;
  property AFinished: boolean read FAFinished write FAFinished;
end;

//==============================================================================

type

  { TdlgMultiThreading }

  TdlgMultiThreading = class(TForm)
    Button1: TButton;
    editNbCores: TCurrencyEdit;
    editNbThreads: TCurrencyEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbNbSeries: TStaticText;
    lbNbAntennes: TStaticText;
    lsbMessages: TListBox;
    ProgressBar0: TProgressBar;
    ProgressBar1: TProgressBar;
    lbProcessing: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    procedure Button1Click(Sender: TObject);
  private
    FDocTopo: TToporobotStructure2012;
    FListeThreadeeAntennes: TThreadedTableAntennes;



    function CopyListeAntennesFromFDocTopo(): integer;
    procedure DispNbElementsBdd();
    procedure QDispMessage(const Msg: string);

    procedure TraiterUneAntenne(Idx: integer);

    procedure ProcInfoProgresssion(Sender: TObject; Index: Integer);
    procedure ProcInfoProgresssion2(const IDThread: integer; const QStart, QEnd, QDone: integer);
    procedure ProcTraiterAntenne(Sender: TObject; Index: Integer; Data : Pointer);
  protected
    MaCS: TCriticalSection;
  public
    function Initialiser(const FD: TToporobotStructure2012): boolean;

    procedure Finaliser();
  end;

var
  dlgMultiThreading: TdlgMultiThreading;

implementation
uses DGCDummyUnit;

{$R *.lfm}
//==============================================================================
{ TThreadViseesEnAntennesProcessing }
procedure TThreadViseesEnAntennesProcessing.Execute;
  function CalcViseeAntenne(const VA: TViseeAntenne; out   OutEE: TBaseStation): boolean;
  var
    VS      : TUneVisee;
    DX, DY, DZ, DP: double;
    EE: TBaseStation;
    MyCode: TCode;
    MyExpe: TExpe;
    q: Integer;
    miou: ValReal;
  begin
    result := false;
    for q := 0 to 2666 do miou := VA.Longueur * sin(VA.Pente);

    {
    DX := 0.00; DY := 0.00; DZ := 0.00;
    if (FBDDEntites.GetEntiteViseeFromSerSt(VA.SerieDepart, VA.PtDepart, EE)) then
    begin
      DX := EE.PosStation.X;
      DY := EE.PosStation.Y;
      DZ := EE.PosStation.Z;
      // les codes et expés de l'antenne héritent de ceux de la station d'accrochage
      MyCode := FDocTopo.GetCodeByNumero(EE.eCode);
      MyExpe := FDocTopo.GetExpeByNumero(EE.eExpe);
    end
    else
    begin
      exit(false);
    end;
    //*)
    VS.IDSecteur       := 0; // VA.Secteur; // TODO: Secteur à implémenter
    VS.Code            := EE.eCode;
    VS.Expe            := EE.eExpe;
    VS.Longueur        := VA.Longueur;
    VS.Azimut          := VA.Azimut;
    VS.Pente           := VA.Pente;
    VS.LG := 0.00;
    VS.LG := 0.00;
    VS.HZ := 0.00;
    VS.HN := 0.00;
    VS.Commentaires     := VA.Commentaires;
    VS.IDTerrainStation := VA.IDTerrainStation;
    VS.TypeVisee        := tgVISEE_RADIANTE;
    CalculerVisee(VS,
                  MyCode, MyExpe,
                  DX, DY,
                  1.00,
                  DZ, DP);

    DX := EE.PosStation.X + VS.DeltaX;
    DY := EE.PosStation.Y + VS.DeltaY;
    DZ := EE.PosStation.Z + VS.DeltaZ;
    //******************************
    // TODO: Dans GHCaveDraw, les antennes ne doivent servir que de guides et
    //       n'ont pas à être capturées
    OutEE.Entite_Serie    := -VA.SerieDepart; // -QNo;
    OutEE.Entite_Station  :=  VA.PtDepart;//0;
    // code et expé (hérités de ceux de la station d'accrochage)
    OutEE.eCode       := EE.eCode;
    OutEE.eExpe       := EE.eExpe;
    OutEE.eSecteur    := VA.Secteur;
    OutEE.eEntrance   := VA.EntranceRatt;
    OutEE.eReseau     := VA.Reseau;
    OutEE.Type_Entite := tgVISEE_RADIANTE;
    OutEE.DateLeve    := Now();
    OutEE.Enabled           := True;   // drapeau
    // données originales
    OutEE.oLongueur       := VA.Longueur;
    OutEE.oAzimut         := VA.Azimut;
    OutEE.oPente          := VA.Pente;
    OutEE.oLG             := 0.00;
    OutEE.oLD             := 0.00;
    OutEE.oHZ             := 0.00;
    OutEE.oHN             := 0.00;
    // centerline
    OutEE.PosExtr0   := EE.PosStation;
    OutEE.PosStation := MakeTPoint3Df(DX, DY, DZ);
    // habillage
    OutEE.PosOPG := EE.PosStation;
    OutEE.PosOPD := EE.PosStation;
    OutEE.PosPG  := MakeTPoint3Df(DX, DY, DZ);
    OutEE.PosPD  := MakeTPoint3Df(DX, DY, DZ);
    // couleur par défaut
    //OutEE.CouleurDegrade:= FPalette256.GetColorByIndex(MyExpe.IdxCouleur); //clGray ;
    OutEE.CouleurDegrade  := clGray ;

    // commentaires
    OutEE.IDTerrain     := VA.IDTerrainStation;
    OutEE.oCommentaires := VA.Commentaires;
    OutEE.IsPOI         := false;
    // passage ici = OK
    }
    result := true;
  end;
var
  i: Integer;
  QVA: TViseeAntenne;
  QEntite: TBaseStation;
  WU: Boolean;
begin
  while (not Terminated) do
  begin

    for i := FIdxStart to FIdxEnd do
    begin
      if (assigned(FProcProgression)) then FProcProgression(FNoThread, FIdxStart, FIdxEnd, i);
      QVA    := FTableAntennes.GetElement(i);
      WU := CalcViseeAntenne(QVA, QEntite);
      if (WU) then
      begin
        //FTableAntennes.AddElement(QEntite);
      end;
    end;
  end;
  self.Terminate;


end;


constructor TThreadViseesEnAntennesProcessing.Create(const TA: TThreadedTableAntennes; const NoThread: integer; const IdxStart, IdxEnd: integer; const P: TProcOfObjectUsesInteger);
var
  QIdxMax: Integer;
begin
  FTableAntennes  := TA;
  FreeOnTerminate := False;
  FNoThread       := NoThread;
  FIdxStart       := IdxStart;
  FIdxEnd         := IdxEnd;
  FProcProgression := P;
  inherited Create(false); // false -> exécution immédiate
end;

function TThreadViseesEnAntennesProcessing.AttendPour(): integer;
begin
  Result := 0;
  self.Terminate;
end;



{ TTheadedTableAntennes }

procedure TThreadedTableAntennes.AddElement(const E: TViseeAntenne);
var
  pE: ^TViseeAntenne;
begin
  New(pE);
  pE^ := E;
  self.LockList.Add(pE);
end;

function TThreadedTableAntennes.GetElement(const Idx: integer): TViseeAntenne;
var
  pE: ^TViseeAntenne;
begin
  pE := Self.LockList.Items[Idx];
  Result := pE^;
end;

function TThreadedTableAntennes.GetNbElements(): integer;
begin
  result := Self.LockList.Count;
end;

procedure TThreadedTableAntennes.ClearListe();
var
  Nb, i: Integer;
begin
  Nb := self.LockList.Count;
  if (Nb = 0) then exit;
  for i := 0 to Nb-1 do
  begin
    try
     Dispose(self.LockList[i]);
    finally
    end;
  end;
  self.LockList.Clear;
end;

{ TdlgMultiThreading }
//***********************************************

procedure TdlgMultiThreading.ProcInfoProgresssion(Sender : TObject; Index : Integer);
begin
  pass;
  //lbProcessing.Caption := IntToStr(Index);
  Application.ProcessMessages; // Indispensable ici
end;

procedure TdlgMultiThreading.ProcInfoProgresssion2(const IDThread: integer; const QStart, QEnd, QDone: integer);
begin
  if (0 = (QDone mod 1000)) then
  begin
    QDispMessage(format('Thread: %d - %d to %d - Done: %d', [IDThread, QStart, QEnd, QDone]));//lbProcessing.Caption := inttostr(IDThread);
    //Application.ProcessMessages;

  end;
end;

procedure TdlgMultiThreading.ProcTraiterAntenne(Sender: TObject; Index: Integer; Data: Pointer);
var
  MyAntenne: TViseeAntenne;
  q: Integer;
  miou: ValReal;
begin
  //maCS.Acquire; // Aucune perte de temps avec cette fonction
  try
    //FL := FListeThreadeeAntennes.LockList;
    MyAntenne := FListeThreadeeAntennes.GetElement(Index);
    // traitement sur la visée
    for q := 0 to 2666 do
    begin

      miou := MyAntenne.Longueur * sin(MyAntenne.Pente);
      //FListeThreadeeAntennes.UnlockList;
    end;
    ProcInfoProgresssion(sender, Index);
  finally
    //maCS.Release;  // Aucune perte de temps avec cette fonction
  end;
end;
//************************************************
procedure TdlgMultiThreading.TraiterUneAntenne(Idx: integer);
var
  MyAntenne: TViseeAntenne;
  q: Integer;
  miou: ValReal;
begin
  MyAntenne := FDocTopo.GetViseeAntenne(Idx);
  for q := 0 to 2666 do
  begin
    miou := MyAntenne.Longueur * sin(MyAntenne.Pente);
  end;
  ProcInfoProgresssion(self, Idx);
end;

procedure TdlgMultiThreading.Button1Click(Sender: TObject);
const
  NB_MAX_THREADS = 4;
var
  QNbThreads: LongInt;
  QNbAntennes, i, NumeroThread, EWE: Integer;
  QChunkSize, lIdxStart, lIdxFinish: integer;
  t0, t1, t2, t3: TDateTime;
  VA, VB: TViseeAntenne;

  MyThreadArray  : array  of TThreadViseesEnAntennesProcessing;
begin

  QNbAntennes := FDocTopo.GetNbAntennes();
  QNbThreads := editNbThreads.AsInteger;
  QDispMessage(format('%d coeurs', [GetNbCoresProcessor()]));
  QDispMessage('Copie des antennes dans la table multithreadée');
  QNbAntennes := CopyListeAntennesFromFDocTopo();
  i := 666;
  VA := FListeThreadeeAntennes.GetElement(i);
  VB := FDocTopo.GetViseeAntenne(i);

  QDispMessage(Format('%d éléments copiés - Exemple: %f, %f, %f', [QNbAntennes, VA.Longueur, VA.Azimut, VA.Pente]));
  QDispMessage(Format('%d éléments copiés - Exemple: %f, %f, %f', [QNbAntennes, VB.Longueur, VB.Azimut, VB.Pente]));

  QDispMessage('Traitement des antennes:');
  QDispMessage(Format('Monothreadé: %d items', [QNbAntennes]));
  t0 := Now();
  for i := 0 to QNbAntennes - 1 do TraiterUneAntenne(i);
  t1 := Now();
  QDispMessage(Format('Multithreadé: %d items sur %d threads', [QNbAntennes, QNbThreads]));
  t2 := Now();
  //*************
  // 1. répartition du travail entre les threads et démarrage
  SetLength(MyThreadArray, QNbThreads);

  QChunkSize := ceil(QNbAntennes / QNbThreads);
  for NumeroThread := 0 to QNbThreads - 1 do
  begin
    lIdxStart  := (NumeroThread + 0) * QChunkSize + 1;
    lIdxFinish := (NumeroThread + 1) * QChunkSize;

    if (lIdxFinish > (QNbAntennes - 1)) then lIdxFinish := QNbAntennes - 1;
    QDispMessage(Format('%d: %d -> %d', [NumeroThread, lIdxStart, lIdxFinish]));
    MyThreadArray[NumeroThread] := TThreadViseesEnAntennesProcessing.Create(FListeThreadeeAntennes, NumeroThread, lIdxStart, lIdxFinish, ProcInfoProgresssion2);

  end;
  QDispMessage('Attente des threads');
  // 2.  Attente des threads
  for NumeroThread := 0 to QNbThreads - 1 do
  begin
    if (not MyThreadArray[NumeroThread].Terminated) then Sleep(20);
  end;
  //*)
  // 3. Attente des fins de traitement par les threads
  for NumeroThread := 0 to QNbThreads - 1 do
  begin
    EWE := MyThreadArray[NumeroThread].AttendPour();
    QDispMessage(Format('Thread %d achieved with %d status code', [NumeroThread, EWE]));
  end;
  // 4. libération des threads
  QDispMessage('00');
  for NumeroThread := 0 to QNbThreads - 1 do
  begin
    QDispMessage(Format('Libération du thread %d', [NumeroThread]));
    try
      FreeAndNil(MythreadArray[NumeroThread]);
    finally

    end;
  end;
  SetLength(MyThreadArray, 0);
  Application.ProcessMessages;
  t3 := Now();
  QDispMessage('Monothreadé : ' + DateTimePascalToDateTimeSQL(t1 - t0));
  QDispMessage('Multithreadé: ' + DateTimePascalToDateTimeSQL(t3 - t2));
end;

procedure TdlgMultiThreading.DispNbElementsBdd();
begin
  lbNbSeries.Caption   := IntToStr(FDocTopo.GetNbSeries());
  lbNbAntennes.Caption := IntToStr(FDocTopo.GetNbAntennes());
end;

procedure TdlgMultiThreading.QDispMessage(const Msg: string);
begin
  lsbMessages.Items.add(Msg);
  lsbMessages.ItemIndex := lsbMessages.Count - 1;
end;

function TdlgMultiThreading.Initialiser(const FD: TToporobotStructure2012): boolean;
begin
  Result := False;
  FDocTopo := FD;
  FListeThreadeeAntennes := TThreadedTableAntennes.Create;
  FListeThreadeeAntennes.ClearListe();

  editNbCores.AsInteger   := GetNbCoresProcessor();
  editNbThreads.AsInteger := editNbCores.AsInteger;
  DispNbElementsBdd();
  QDispMessage('Prêt');
  Result := True;
end;
function TdlgMultiThreading.CopyListeAntennesFromFDocTopo(): integer;
var
  i, Nb: Integer;
  VA: TViseeAntenne;
begin
  Nb := FDocTopo.GetNbAntennes();
  QDispMessage(Format('%s.CopyListeAntennesFromFDocTopo(): %d', [classname, Nb]));
  if (0 = Nb) then exit;
  for i := 0 to Nb -1 do
  begin
    VA := FDocTopo.GetViseeAntenne(i);
    FListeThreadeeAntennes.AddElement(VA);
  end;
  Result := FListeThreadeeAntennes.GetNbElements();
end;


procedure TdlgMultiThreading.Finaliser();
begin
  FListeThreadeeAntennes.ClearListe();
  FreeAndNil(FListeThreadeeAntennes);
end;
//******************************************************************************
end.
