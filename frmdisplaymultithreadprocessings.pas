unit frmDisplayMultiThreadProcessings;

{$mode objfpc}{$H+}

{$ERROR: Inutilisé }
interface

uses
  //MyMatrixThreads,
  Classes, SysUtils, Math,
  Common,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type TThreadProgressBar = record
  IdxThread : integer;
  lbIDThread: TLabel;
  pbThread  : TProgressBar;
end;


type

{ TdlgDispMultiThreadProcessing }

  TdlgDispMultiThreadProcessing = class(TForm)
    lbMonoThreadEtape: TLabel;
    lsbEtapesTraitement: TListBox;
    lsbMessages: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pgrBarMonothread: TProgressBar;
    procedure FormShow(Sender: TObject);
  private
    FNbThreads : integer; // max 8 threads
  public
    function  Initialiser(const NbThreads: integer): boolean;
    procedure Finaliser();
    procedure QAfficherMessage(const Msg: string);
    procedure AfficherProgressionOfMonoThread(const Etape: string; const Done, Starting, Ending, Step: integer);


    procedure SaveLog(const F: string);
    procedure AddLibelleEtape(const Msg: string);

  end;

var
  dlgDispMultiThreadProcessing: TdlgDispMultiThreadProcessing;

implementation

{$R *.lfm}

procedure TdlgDispMultiThreadProcessing.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

{ TdlgDispMultiThreadProcessing }
function TdlgDispMultiThreadProcessing.Initialiser(const NbThreads: integer): boolean;
var
  i: Integer;
  EWE: TLabel;
  PB: TProgressBar;
  procedure MiouMiou(const idx: integer; const L: TLabel; const P: TProgressBar);
  begin
    if (idx > NbThreads) then L.Caption := '---'
                         else L.Caption := Format('Thread %d', [idx]);
    P.Smooth  := true;
    P.Min     := 0;
    P.Max     := 100;
    P.Position:= 0;
  end;
begin
  result := false;
  lsbEtapesTraitement.Clear;
  FNbThreads := Max(NbThreads, 8);
  (*
  MiouMiou(1, lbThread1, pgbThread1);
  MiouMiou(2, lbThread2, pgbThread2);
  MiouMiou(3, lbThread3, pgbThread3);
  MiouMiou(4, lbThread4, pgbThread4);
  MiouMiou(5, lbThread5, pgbThread5);
  MiouMiou(6, lbThread6, pgbThread6);
  MiouMiou(7, lbThread7, pgbThread7);
  MiouMiou(8, lbThread8, pgbThread8);
  //*)
  // ShowMessage('556-1-2');
  result := true;

end;

procedure TdlgDispMultiThreadProcessing.Finaliser();
var
  i: integer;
begin
  ;;
end;


procedure TdlgDispMultiThreadProcessing.QAfficherMessage(const Msg: string);
begin
  try
    lsbMessages.Items.Add(Msg);
    lsbMessages.ItemIndex := lsbMessages.Count - 1;
  except
  end;
end;

procedure TdlgDispMultiThreadProcessing.AfficherProgressionOfMonoThread(const Etape: string; const Done, Starting, Ending, Step: integer);
begin
  if (0 = Done MOD Step) then
  lbMonoThreadEtape.Caption := Etape;
  try
    pgrBarMonothread.Min := Starting;
    pgrBarMonothread.Max := Ending;
    pgrBarMonothread.Position := Done;
    Application.ProcessMessages;
  except
   ;;
  end;
end;




procedure TdlgDispMultiThreadProcessing.DispProgressionOfProcess(const IDThread: integer; const QStart, QEnd, QDone: integer);
var
  EWE: String;
  procedure MiouMiou(const L: TLabel; const P: TProgressBar; const s: string; const QMin, QMax, QPos: integer);
  begin
    exit;
    L.Caption := S;
    P.Min     := QMin;
    P.Max     := QMax;
    P.Position:= QPos;
    L.Refresh;
    P.Refresh;
  end;
begin
  exit;
  //EWE := format('Thread %d: %d of %d -> %d (%d elts) ', [IDThread, QDone, QStart, QEnd, QEnd - QStart]);
  (*
  //QAfficherMessage(EWE);
  begin
    case IDThread of
      1: MiouMiou(lbThread1, pgbThread1, EWE, QStart, QEnd, QDone);
      2: MiouMiou(lbThread1, pgbThread2, EWE, QStart, QEnd, QDone);
      3: MiouMiou(lbThread3, pgbThread3, EWE, QStart, QEnd, QDone);
      4: MiouMiou(lbThread4, pgbThread4, EWE, QStart, QEnd, QDone);
      5: MiouMiou(lbThread5, pgbThread5, EWE, QStart, QEnd, QDone);
      6: MiouMiou(lbThread6, pgbThread6, EWE, QStart, QEnd, QDone);
      7: MiouMiou(lbThread7, pgbThread7, EWE, QStart, QEnd, QDone);
      8: MiouMiou(lbThread8, pgbThread8, EWE, QStart, QEnd, QDone);
    end;
  end;
  //*)
  //Application.ProcessMessages;  // Interdit en multithread
end;

procedure TdlgDispMultiThreadProcessing.SaveLog(const F: string);
begin
  lsbMessages.Items.SaveToFile(F);
end;

procedure TdlgDispMultiThreadProcessing.AddLibelleEtape(const Msg: string);
begin
  lsbEtapesTraitement.Items.Add(Msg);
  lsbEtapesTraitement.ItemIndex := lsbEtapesTraitement.Count - 1;

end;



end.

