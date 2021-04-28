unit frmTestMultithreading;
{$INCLUDE CompilationParameters.inc}
{$ERROR Inadapté, difficulté ++++ }
interface
uses
  StructuresDonnees,
  ToporobotClasses2012,
  Common,
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, curredit;

type TProgressProc = procedure(aIndex :integer) of object;
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
  FCurrentIndex: integer;
  FProgressProc: TProgressProc;
  procedure SyncProgressProc();

protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants
  procedure Execute; override;
public
  Constructor Create(const aProgressProc: TProgressProc);

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
    RunningThreads :integer;  // Nombre de threads en cours
    StartTime      :integer;
    FDocTopo: TToporobotStructure2012;



    function CopyListeAntennesFromFDocTopo(): integer;
    procedure DispNbElementsBdd();
    procedure OnThreadProgress(aIndex: integer);
    procedure QDispMessage(const Msg: string);
    procedure ThreadTerminated(Sender: TObject);
  public
    function Initialiser(const FD: TToporobotStructure2012): boolean;

    procedure Finaliser();
  end;

var
  dlgMultiThreading: TdlgMultiThreading;

  ThreadedTableAntennes : TThreadedTableAntennes; //TThreadList;
  CurIndex              : integer;

implementation
uses DGCDummyUnit;

{$R *.lfm}

procedure TThreadViseesEnAntennesProcessing.SyncProgressProc();
begin
  FProgressProc(FCurrentIndex);
end;

//==============================================================================
{ TThreadViseesEnAntennesProcessing }
procedure TThreadViseesEnAntennesProcessing.Execute;
var
  QVA     :TViseeAntenne;
  QEntite :TBaseStation;
  Index: LongInt;

  //--------------------------------------------------------------------------------------------------
  function CalcViseeAntenne(aQVA :TViseeAntenne; var QEntite :TBaseStation): boolean;
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
    Sleep(Random(4) *1000);  // Pour test, 0 à 3 sec.
    result := True;
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
begin
  while not self.Terminated do
  begin
    // Prochain élément à traiter
    Index := InterlockedIncrement(CurIndex);
    FCurrentIndex := Index;
    //try
      // Terminé ?
      if Index >= ThreadedTableAntennes.LockList.Count then Exit;
      //...non
      QVA := ThreadedTableAntennes.GetElement(Index);// LockList.Items[Index];
      QVA := TViseeAntenne(ThreadedTableAntennes.LockList.Items[Index]^);

    //finally
      ThreadedTableAntennes.UnlockList;
    //end;

    //Synchronisation obligatoire !
    Synchronize(SyncProgressProc);

    if CalcViseeAntenne(QVA, QEntite) then pass;
      //TableAntennes.Add(QEntite);
  end;
end;


constructor TThreadViseesEnAntennesProcessing.Create(const aProgressProc: TProgressProc);
begin
  inherited Create(false);
  FProgressProc   := aProgressProc;
  FreeOnTerminate := TRUE;
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
procedure TdlgMultiThreading.Button1Click(Sender: TObject);
var
  i , QNbAntennes:integer;
  T :TThreadViseesEnAntennesProcessing;
  VA, VB: TViseeAntenne;
begin
  Button1.Enabled := FALSE;
  StartTime       := GetTickCount64;
  QNbAntennes := FDocTopo.GetNbAntennes();

  QDispMessage(format('%d coeurs', [GetNbCoresProcessor()]));
  QDispMessage('Copie des antennes dans la table multithreadée');
  QNbAntennes := CopyListeAntennesFromFDocTopo();
  i := 666;
  VA := ThreadedTableAntennes.GetElement(i);
  VB := FDocTopo.GetViseeAntenne(i);
  QDispMessage(Format('%d éléments copiés - Exemple: %f, %f, %f', [QNbAntennes, VA.Longueur, VA.Azimut, VA.Pente]));
  QDispMessage(Format('%d éléments copiés - Exemple: %f, %f, %f', [QNbAntennes, VB.Longueur, VB.Azimut, VB.Pente]));

  CurIndex       := -1;
  RunningThreads := TThreadViseesEnAntennesProcessing.ProcessorCount;

  QDispMessage(Format('Multithreadé: %d items sur %d threads', [QNbAntennes, RunningThreads]));
  for i := 0 to RunningThreads-1 do
  begin
    T := TThreadViseesEnAntennesProcessing.Create(OnThreadProgress);
    T.OnTerminate := ThreadTerminated;
  end;
  Button1.Enabled := True;

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
  ThreadedTableAntennes.ClearListe();

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
    ThreadedTableAntennes.AddElement(VA);
  end;
  Result := ThreadedTableAntennes.GetNbElements();
end;


procedure TdlgMultiThreading.Finaliser();
begin
  ThreadedTableAntennes.ClearListe();
end;

procedure TdlgMultiThreading.OnThreadProgress(aIndex :integer);
begin
  QDispMessage(Format('Traitement de l''élément %d', [aIndex]));
end;

procedure TdlgMultiThreading.ThreadTerminated(Sender: TObject);
begin
  Dec(RunningThreads);

  // Terminé ?
  if RunningThreads = 0 then
  begin
    Button1.Enabled := TRUE;
    QDispMessage(Format('Terminé en %fs', [(GetTickCount64 - StartTime) /1000]));
  end;
end;
//******************************************************************************
initialization
  Randomize; // Pour test
  ThreadedTableAntennes := TThreadedTableAntennes.Create;//TThreadList.Create;

finalization
  ThreadedTableAntennes.Free;

end.


////////////////////////////////////////////////////////////////////////////////
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TViseeAntenne = TObject;  // Pour test
  TBaseStation  = TObject;  // Pour test

  TProgressProc = procedure(aIndex :integer) of object;

  TThreadViseesEnAntennesProcessing = class(TThread)
  private
    FProgressProc :TProgressProc;
  protected
    procedure Execute; override;
  public
    Constructor Create(const aProgressProc: TProgressProc);
  end;

  TdlgMultiThreading = class(TForm)
    Button1 :TButton;
    Memo1   :TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    RunningThreads :integer;  // Nombre de threads en cours
    StartTime      :integer;

    procedure OnThreadProgress(aIndex :integer);
    procedure ThreadTerminated(Sender :TObject);
  end;

var
  dlgMultiThreading: TdlgMultiThreading;

implementation

{$R *.dfm}

var
  TableAntennes :TThreadList;
  CurIndex      :integer;

{ TThreadViseesEnAntennesProcessing }

constructor TThreadViseesEnAntennesProcessing.Create(const aProgressProc: TProgressProc);
begin
  inherited Create;

  FProgressProc   := aProgressProc;
  FreeOnTerminate := TRUE;
end;

procedure TThreadViseesEnAntennesProcessing.Execute;
var
  Index   :integer;
  QVA     :TViseeAntenne;
  QEntite :TBaseStation;

  //--------------------------------------------------------------------------------------------------
  function CalcViseeAntenne(aQVA :TViseeAntenne; var QEntite :TBaseStation) :boolean;
  begin
    Sleep(Random(4) *1000);  // Pour test, 0 à 3 sec.
  end;
  //--------------------------------------------------------------------------------------------------

begin
  while not Terminated do
  begin
    // Prochain élément à traiter
    Index := InterlockedIncrement(CurIndex);

    with TableAntennes.LockList do
    try
      // Terminé ?
      if Index >= Count then Exit;
      //...non
      QVA := Items[Index];
    finally
      TableAntennes.UnlockList;
    end;

    //Synchronisation obligatoire !
    if Assigned(FProgressProc) then
      Synchronize(procedure
                  begin
                    FProgressProc(Index);
                  end);

    if CalcViseeAntenne(QVA, QEntite) then
      TableAntennes.Add(QEntite);
  end;
end;

procedure TdlgMultiThreading.Button1Click(Sender: TObject);
var
  i :integer;
  T :TThread;
begin
  Button1.Enabled := FALSE;

  StartTime      := GetTickCount;
  CurIndex       := -1;
  RunningThreads := TThread.ProcessorCount;

  for i := 0 to RunningThreads-1 do
  begin
    T := TThreadViseesEnAntennesProcessing.Create(OnThreadProgress);
    T.OnTerminate := ThreadTerminated;
  end;
end;

procedure TdlgMultiThreading.FormCreate(Sender: TObject);
begin
  // Remplissage pour test, les objets ne sont pas libérés !
  TableAntennes.Clear;

  for var i := 0 to 49 do
    TableAntennes.Add(TViseeAntenne.Create);
end;

procedure TdlgMultiThreading.OnThreadProgress(aIndex :integer);
begin
  Memo1.Lines.Add(Format('Traitement de l''élément %d', [aIndex]));
end;

procedure TdlgMultiThreading.ThreadTerminated(Sender: TObject);
begin
  Dec(RunningThreads);

  // Terminé ?
  if RunningThreads = 0 then
  begin
    Button1.Enabled := TRUE;
    Memo1.Lines.Add(Format('Terminé en %fs', [(GetTickCount -StartTime) /1000]));
  end;
end;

initialization
  Randomize; // Pour test
  TableAntennes := TThreadList.Create;

finalization
  TableAntennes.Free;

end.
