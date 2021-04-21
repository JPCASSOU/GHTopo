unit frmTestMultithreading;

{$mode delphi}

interface

uses
  GHTopoMultiThreading2,
  StructuresDonnees,
  ToporobotClasses2012,
  Common, UnitObjetSerie,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, curredit;

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
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
  private
    FDocTopo: TToporobotStructure2012;
    procedure DispNbElementsBdd();
    procedure QDispMessage(const Msg: string);
    procedure TraiterUneAntenne(const NoThread, Idx: integer);
    procedure TraiterUneSerie(const NoThread, Idx: integer);
  public
    function Initialiser(const FD: TToporobotStructure2012): boolean;

  end;

var
  dlgMultiThreading: TdlgMultiThreading;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TdlgMultiThreading }



procedure TdlgMultiThreading.TraiterUneAntenne(const NoThread, Idx: integer);
var
  MyAntenne: TViseeAntenne;
  q: Integer;
  miou: ValReal;
begin
  MyAntenne := FDocTopo.GetViseeAntenne(Idx);
  for q := 0 to 2666 do
  begin
    miou := MyAntenne.Longueur * sin(MyAntenne.Pente);
    // Procedure multithreadée = ne JAMAIS interagir avec le thread principal (dont les widgets)
    //if (0 = (Idx mod 1000)) then QDispMessage(Format('Thread %d: %d - %.2f', [NoThread, Idx, MyAntenne.Longueur]));
  end;
end;

procedure TdlgMultiThreading.TraiterUneSerie(const NoThread, Idx: integer);
var
  q: Integer;
  miou: ValReal;
  MySerie: TObjSerie;
  V: TUneVisee;
  CC: TCode;
  EE: TExpe;
  QX, QY, QAZ, QAP: double;
begin
  MySerie := FDocTopo.GetSerie(Idx);
  for q := 0 to MySerie.GetNbVisees() - 1 do
  begin
    V := MySerie.GetVisee(q);
    EE := FDocTopo.GetExpeByNumero(V.Expe);
    CC := FDocTopo.GetCodeByNumero(V.Code);
    CalculerVisee(V, CC, EE, QX, QY, 1, QAZ, QAP);
  end;
  //QDispMessage(Format('Thread %d: %d - %.2f', [NoThread, Idx, MyAntenne.Longueur]));
end;

procedure TdlgMultiThreading.Button1Click(Sender: TObject);
var
  QListeThreads: TListOfThreads;
  QNbThreads: LongInt;
  QNbAntennes, i, QNbSeries: Integer;
  t0, t1, t2, t3: TDateTime;
begin

  QNbAntennes := FDocTopo.GetNbAntennes();
  QNbThreads := editNbThreads.AsInteger;

  QDispMessage('Traitement des antennes:');
  QDispMessage(Format('Monothreadé: %d items', [QNbAntennes]));
  t0 := Now();
  for i := 0 to QNbAntennes - 1 do TraiterUneAntenne(-1, i);

  t1 := Now();
  QDispMessage(Format('Multithreadé: %d items sur %d threads', [QNbAntennes, QNbThreads]));
  t2 := Now();
  QListeThreads := TListOfThreads.Create;
  try
    if (QListeThreads.InitialiserEtLancer(QNbThreads, false, TraiterUneAntenne, QNbAntennes)) then
    begin
      QListeThreads.Finaliser();
    end;
  finally
    FreeAndNil(QListeThreads);
  end;
  t3 := Now();
  QDispMessage('Monothreadé : ' + DateTimePascalToDateTimeSQL(t1 - t0));
  QDispMessage('Multithreadé: ' + DateTimePascalToDateTimeSQL(t3 - t2));
  QDispMessage('-----');
  QDispMessage('Traitement des séries:');
  QNbSeries := FDocTopo.GetNbSeries();
  QDispMessage(Format('Monothreadé: %d séries', [QNbSeries]));
  t0 := Now();
  for i := 0 to QNbSeries - 1 do TraiterUneSerie(-1, i);
  t1 := Now();
  QDispMessage(Format('Multithreadé: %d items sur %d threads', [QNbSeries, QNbThreads]));
  t2 := Now();
  QListeThreads := TListOfThreads.Create;
  try
    if (QListeThreads.InitialiserEtLancer(QNbThreads, false, TraiterUneSerie, QNbSeries)) then
    begin
      QListeThreads.Finaliser;
    end;
  finally
    FreeAndNil(QListeThreads);
  end;
  t3 := Now();
  QDispMessage('Monothreadé : ' + DateTimePascalToDateTimeSQL(t1 - t0));
  QDispMessage('Multithreadé: ' + DateTimePascalToDateTimeSQL(t3 - t2));


end;

procedure TdlgMultiThreading.ToggleBox1Change(Sender: TObject);
begin

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
  //Application.ProcessMessages;
end;

function TdlgMultiThreading.Initialiser(const FD: TToporobotStructure2012): boolean;
begin
  Result := False;
  FDocTopo := FD;
  editNbCores.AsInteger   := GetNbCoresOfProcessor();
  editNbThreads.AsInteger := 2 * editNbCores.AsInteger;

  DispNbElementsBdd();
  QDispMessage('Prêt');
  Result := True;
end;

end.

