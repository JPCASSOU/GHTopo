unit GHTopoMultiThreading3;
{$ERROR Ne pas utiliser}
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  BZParallelThread,
  MTProcs
  , StructuresDonnees
  , UnitListesSimplesWithGeneriques
  , Common
  , Classes
  , SysUtils
  , math
  ;
  //, LCLProc, LCLType, LCLIntf; // Utiliser les TCriticalSection de la LCL, et non celles de la RTL
type TCallbackProgression = procedure(const Index: integer; const Position: integer) of object;
type

{ TTraitementParallele }

 TTraitementParallele = class
  private
    FNbThreads: integer;
    FIdxDeb   : integer;
    FIdxFin   : integer;
    FCurrentThread: integer;
    FCurrentCursor: integer;
    FCallBackTraitement : TThreadProcOfObjectWithOneIntParameter; // callback pour le traitement
    FCallbackProgression: TCallbackProgression;
    procedure ProcessParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
    procedure UpdateStatus();
  public
    function  Initialiser(const NbThreads: integer;
                          const IdxDeb, IdxFin: integer;
                          const PT: TThreadProcOfObjectWithOneIntParameter;
                          const PC: TCallbackProgression): boolean;
    procedure Finaliser();
    procedure Processing();
end;





implementation


{ TTraitementParallele }

procedure TTraitementParallele.ProcessParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
  QNbItems, QChunkSize, QIdxDeb, QIdxEnd, i: Integer;
begin
  QNbItems := FIdxFin - FIdxDeb + 1;
  QChunkSize := ceil(QNbItems / FNbThreads);


  QIdxDeb := FIdxDeb + QChunkSize * Index;
  QIdxEnd := FIdxDeb + QChunkSize * (Index + 1);
  if (QIdxEnd > (FIdxFin)) then QIdxEnd := FIdxFin;


  for i := QIdxDeb to QIdxEnd do
  begin
    //ProcThreadPool.EnterPoolCriticalSection;
      FCurrentThread := Index;
      FCurrentCursor := i;
      FCallBackTraitement(Index, i);
      //if (0 = (i MOD 1000)) then Item.Thread.Synchronize(Item.Thread, UpdateStatus);                           //FCallbackProgression(Index, i);
    //ProcThreadPool.LeavePoolCriticalSection;
  end;


end;

procedure TTraitementParallele.UpdateStatus();
begin
  FCallbackProgression(FCurrentThread, FCurrentCursor);
end;

function TTraitementParallele.Initialiser(const NbThreads: integer;
                                          const IdxDeb, IdxFin: integer;
                                          const PT: TThreadProcOfObjectWithOneIntParameter;
                                          const PC: TCallbackProgression): boolean;
begin
  result := false;
  try
    FNbThreads            := NbThreads;
    FIdxDeb               := IdxDeb;
    FIdxFin               := IdxFin;
    FCallBackTraitement   := PT;
    FCallbackProgression  := PC;
    result := True;
  except
  end;
end;

procedure TTraitementParallele.Finaliser();
begin

end;

procedure TTraitementParallele.Processing();
begin
  ProcThreadPool.DoParallel(self.ProcessParallel, 0, FNbThreads-1, nil, FNbThreads);
end;



end.


// a simple parallel procedure
procedure DoSomethingParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
  i: Integer;
begin
  writeln(Index);
  for i:=1 to Index*1000000 do ; // do some work
end;

begin
  ProcThreadPool.DoParallel(@DoSomethingParallel,1,5,nil); // address, startindex, endindex, optional data
end.
