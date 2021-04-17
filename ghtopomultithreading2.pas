unit GHTopoMultiThreading2;
{$INCLUDE CompilationParameters.inc}
{$ASSERTIONS ON}
interface
uses
  StructuresDonnees
  , Common
  , Classes
  , SysUtils
  {$IFDEF USE_MATRIX_DATASTRUCTURE_LIST_OF_LISTS}
  , unitMatricesCreuses_LOT                // OK, un peu plus rapide que LOL,
  {$ELSE}
  , unitMatricesCreusesArray2D
  {$ENDIF}
  , LCLProc, LCLType, LCLIntf; // Utiliser les TCriticalSection de la LCL, et non celles de la RTL
  ; //


type

{ TMyThread }

 TMyThread = class(TThread)
private
  FAFinished: boolean;
  FCriticalSection: TCriticalSection;
  FCallBack: TProcOfObjectWithOneIntParameter;
  FStart, FEnd: integer;
protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants
  procedure Execute; override;
public
  procedure SetCriticalSection(const C: TCriticalSection);
  procedure SetCallBack(const F: TProcOfObjectWithOneIntParameter);
  procedure SetStartEnd(const QStart, QEnd: integer);
  property  AFinished: boolean read FAFinished write FAFinished;
end;

type TMyThreadCalcViseesAntennes = class(TThread)
private
  FAFinished: boolean;
  FCriticalSection: TCriticalSection;
protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants
  procedure Execute; override;
public
  procedure SetCriticalSection(const C: TCriticalSection);
  property  AFinished: boolean read FAFinished write FAFinished;
end;

type

{ TListOfThreads }

 TListOfThreads = class
  private
    FCriticalSection: TCriticalSection;
    FListeOfThreads: array of TMyThread;
    procedure CheckFinished();

  public
    function  Initialiser(const NbThreads: integer): boolean;
    procedure Finaliser;
    function  GetNbThreads(): integer;
end;



implementation

{ TListOfThreads }

function TListOfThreads.Initialiser(const NbThreads: integer): boolean;
var
  i: Integer;
begin
  result := false;
  SetLength(FListeOfThreads, NbThreads);
  InitializeCriticalSection(FCriticalSection);
  for i :=Low(FListeOfThreads) do High(FListeOfThreads) do
  begin
    FListeOfThreads[i] := TMyThread.Create(false);
    FListeOfThreads[i].SetCriticalSection(FCriticalSection);
  end;
end;

procedure TListOfThreads.CheckFinished();
var
  AllFinished: Boolean;
begin
   // wait till all threads finished
  repeat
    AllFinished := true;
    for i:=Low(FListeOfThreads) to High(FListeOfThreads) do
      if (not FListeOfThreads[i].AFinished) then AllFinished := false;
  until AllFinished;
end;
procedure TListOfThreads.Finaliser;
var
  i: Integer;
begin
  for i := Low(FListeOfThreads) do High(FListeOfThreads) do
  begin
    try
      FreeAndNil(FListeOfThreads[i]);
    finally
    end;
  end;
  DeleteCriticalSection(FCriticalSection);
  SetLength(FListeOfThreads, 0);
end;

function TListOfThreads.GetNbThreads(): integer;
begin
  Result := Length(FListeOfThreads);
end;




//******************************************************************************
{ TMyThread }

procedure TMyThread.Execute;
begin
  FAFinished := false;
  // le traitement ici
  for i:= FStart to FEnd  do begin
    EnterCriticalSection(FCriticalSection);
    try
      FCallback(i); // fonction de callback effectuant le traitement
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  end;
  FAFinished:=true;
end;

procedure TMyThread.SetCriticalSection(const C: TCriticalSection);
begin
  FCriticalSection := C;
end;

procedure TMyThread.SetCallBack(const F: TProcOfObjectWithOneIntParameter);
begin
  FCallBack := F;
end;

procedure TMyThread.SetStartEnd(const QStart, QEnd: integer);
begin
  FStart  := QStart;
  FEnd    := QEnd;
end;

end.



