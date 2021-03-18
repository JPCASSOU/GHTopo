unit CadreHistoDepth;

{$INCLUDE CompilationParameters.inc}
// Date: 16/01/2014
interface
uses
  Common,
  UnitEntitesExtended, StructuresDonnees,
  Graphics,
  Classes, SysUtils, FileUtil,
  TAGraph, TASeries, TACustomSeries,
  Forms, Controls,
  StdCtrls;

type

  { TCdrHistoAltitudesExt }

  TCdrHistoAltitudesExt = class(TFrame)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    procedure BarChart1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure QRotate(ASeries: TBasicPointSeries);
  private
    { private declarations }
    { private declarations }
    FBDDEntites   : TBDDEntites;
    FFiltres      : string;
    FNbBarres    : integer;
    FLongMini     : double;
    FColorBarreLine : TColor;
    FColorBarreFill : TColor;

  public
    { public declarations }
    function Initialise(const DT: TBDDEntites;
                        const Filtres: string;
                        const NbBarres: integer;
                        const LMini: double;
                        const BarresLineColor: TColor;
                        const BarresFillColor: TColor): boolean;
  end;

implementation

{$R *.lfm}

{ TCdrHistoAltitudesExt }

procedure TCdrHistoAltitudesExt.BarChart1Click(Sender: TObject);
begin

end;

procedure TCdrHistoAltitudesExt.Button1Click(Sender: TObject);
begin

end;

procedure TCdrHistoAltitudesExt.CheckBox1Change(Sender: TObject);
begin
end;

procedure TCdrHistoAltitudesExt.QRotate(ASeries: TBasicPointSeries);
var
  t: Integer;
begin
  with ASeries do begin
    t := AxisIndexX;
    AxisIndexX := AxisIndexY;
    AxisIndexY := t;
  end;
end;

function TCdrHistoAltitudesExt.Initialise(const DT: TBDDEntites;
                                          const Filtres: string;
                                          const NbBarres: integer;
                                          const LMini: double;
                                          const BarresLineColor: TColor;
                                          const BarresFillColor: TColor): boolean;
var
  i: Integer;
  EWE: TVecteurDouble;
  DeltaZ, Z0, PZ: double;
begin
  Result := False;
  AfficherMessage('Diagramme avec TChart');
  try
    FFiltres      := Filtres;
    FNbBarres    := NbBarres;
    FLongMini     := LMini;
    FColorBarreLine  := BarresLineColor;
    FColorBarreFill  := BarresFillColor;
    DT.MetaFiltre(FFiltres);
    // SetMinMax indispensable ici
    DT.SetMinMax(True);
    //AfficherMessage('002');
    DT.ParseDepthHistogramme(FNbBarres, FLongMini, False);
    //AfficherMessage('003');

    // préparation du diagramme
    //DiagramAltitudes.ClearSeries;

    Chart1BarSeries1.Clear;
    EWE := DT.GetClassesRepartDepthDiagram();
    DeltaZ := DT.GetCoinHautDroit.Z - DT.GetCoinBasGauche.Z;
    Z0     := DT.GetCoinBasGauche.Z;
    PZ     := DeltaZ / High(EWE);
    AfficherMessage(Format('%.0f -> %.0f - %.0f',[DT.GetCoinHautDroit.Z, DT.GetCoinBasGauche.Z, DeltaZ]));
    for i:= 0 to High(EWE) do
    begin
      //BarChart1.AddBar(Format(FORMAT_NB_REAL_0_DEC,[Z0 + i * PZ]), Trunc(EWE[i]), BarresFillColor);
      //BarChart1.AddBar('', Trunc(EWE[i]), BarresFillColor);
      //Chart1BarSeries1.Add(EWE[i], Format(FORMAT_NB_REAL_0_DEC,[Z0 + i * PZ]), BarresFillColor);
      Chart1BarSeries1.Add(EWE[i], Format('Barre%d',[i]), BarresFillColor);
    end;
    // rotation pour mettre le graphe à l'horizontale
    QRotate(Chart1BarSeries1);
    Result := True;
  except
  end;
end;

end.

