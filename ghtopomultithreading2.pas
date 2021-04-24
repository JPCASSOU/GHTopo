unit GHTopoMultiThreading2;
{$ERROR Ne pas utiliser}
{$INCLUDE CompilationParameters.inc}

interface
uses
  //StructuresDonnees
  //, Common
   Classes
  , SysUtils
  , math
  , LCLProc, LCLType, LCLIntf // Utiliser les TCriticalSection de la LCL, et non celles de la RTL
  //, SyncObjs
  ; //

// Pour le callback du thread
type TThreadProcOfObjectWithOneIntParameter = procedure(const NoThread, Idx: integer) of object;
// Thread perso
type  TMyThread = class(TThread)
private
  FIdxThread: integer;
  FAFinished: boolean;
  FCriticalSection: TCriticalSection;
  FCallBack: TThreadProcOfObjectWithOneIntParameter; // callback pour le traitement
  FIdxStart, FIdxEnd: integer;
protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants
  procedure Execute; override;
public
  constructor Create(const QIdxThread: integer; const C: TCriticalSection; const F: TThreadProcOfObjectWithOneIntParameter; const QIdxStart, QIdxEnd: integer);
  property    AFinished: boolean read FAFinished write FAFinished;
end;

//******************************************************************************
// Liste des threads
type  TListOfThreads = class
  private
    FListeOfThreads : array of TMyThread;
    FUseCriticalSections: boolean;
    FCriticalSection: TCriticalSection;
    FNbItems        : integer;
    FCallbackProcessing: TThreadProcOfObjectWithOneIntParameter; // traitement unitaire
    procedure CheckFinished();
  public


    function  InitialiserEtLancer(const NbThreads: integer; const UseCriticalSections: boolean; const P: TThreadProcOfObjectWithOneIntParameter; const QNbItems: integer; const StartIndex: integer = 0): boolean;
    procedure Finaliser();
    function  GetNbThreads(): integer;
end;

implementation
uses DGCDummyUnit; // unité vide

{ TListOfThreads }
function TListOfThreads.InitialiserEtLancer(const NbThreads: integer; const UseCriticalSections: boolean; const P: TThreadProcOfObjectWithOneIntParameter; const QNbItems: integer; const StartIndex: integer = 0): boolean;
var
  i, QIdxDeb, QIdxEnd, QChunkSize, QIdxMax: Integer;
begin
  result := false;
  FNbItems := QNbItems;
  FCallbackProcessing := P;
  FUseCriticalSections:= UseCriticalSections;

  SetLength(FListeOfThreads, NbThreads);
  if (FUseCriticalSections) then
  begin
    InitializeCriticalSection(FCriticalSection);
    //FCriticalSection := TCriticalSection.Create;
  end;
  QChunkSize := ceil(QNbItems / NbThreads);
  QIdxMax := QNbItems - 1;
  for i := Low(FListeOfThreads) to High(FListeOfThreads) do
  begin
    QIdxDeb := i     * QChunkSize + StartIndex;
    QIdxEnd := (i+1) * QChunkSize + StartIndex;
    if (QIdxEnd > (QIdxMax + StartIndex)) then QIdxEnd := QIdxMax + StartIndex;
    //AfficherMessageErreur(Format('%d éléments à traiter sur %d threads: Thread %d: De %d à %d (%d)', [QNbItems, NbThreads, i, QIdxDeb, QIdxEnd, QIdxEnd - QIdxDeb + 1]));
    FListeOfThreads[i] := TMyThread.Create(i, FCriticalSection, FCallbackProcessing, QIdxDeb, QIdxEnd);
  end;
  result := True;
end;

procedure TListOfThreads.CheckFinished();
var
  AllFinished: Boolean;
  i: Integer;
begin
  // wait till all threads finished
  repeat
    AllFinished := true;
    for i:=Low(FListeOfThreads) to High(FListeOfThreads) do
      if (not FListeOfThreads[i].AFinished) then AllFinished := false;
  until AllFinished;
  //*)
end;



procedure TListOfThreads.Finaliser();
var
  i: Integer;
begin
  //AfficherMessageErreur('Attente de finalisation');
  CheckFinished();
  //AfficherMessageErreur('Libération des threads');
  for i := Low(FListeOfThreads) to High(FListeOfThreads) do
  begin
    try
      FreeAndNil(FListeOfThreads[i]);
    finally
    end;
  end;
  if (FUseCriticalSections) then DeleteCriticalSection(FCriticalSection);
  //if (FUseCriticalSections) then FCriticalSection.Free;
  SetLength(FListeOfThreads, 0);
end;

function TListOfThreads.GetNbThreads(): integer;
begin
  Result := Length(FListeOfThreads);
end;

//******************************************************************************
{ TMyThread }
procedure TMyThread.Execute;
var
  i: Integer;
begin
  FAFinished := false;
  // le traitement ici
  // Les sections critiques sont coûteuses +++
  try
    EnterCriticalSection(FCriticalSection);
    for i:= FIdxStart to FIdxEnd do
    begin
      FCallback(FIdxThread, i); // fonction de callback effectuant le traitement
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
  FAFinished:=true;
end;

constructor TMyThread.Create(const QIdxThread: integer; const C: TCriticalSection; const F: TThreadProcOfObjectWithOneIntParameter; const QIdxStart, QIdxEnd: integer);
begin
  FIdxThread := QIdxThread;
  FCriticalSection := C;
  FCallBack := F;
  FIdxStart    := QIdxStart;
  FIdxEnd      := QIdxEnd;
  inherited Create(false); // false -> exécution immédiate
end;
end.
