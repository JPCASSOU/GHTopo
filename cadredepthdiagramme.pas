unit CadreDepthDiagramme;
// 14/10/2019: Utilise le nouveau contexte 2D TCdrDrawingContextGCS
// 15/10/2019: OK

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  math,
  StructuresDonnees,
  Common,
  DGCTypes,
  CadreDGCDrawingContext,
  UnitEntitesExtended,
  Classes, SysUtils, Graphics, Forms, Controls, StdCtrls, ExtCtrls, Buttons;

type

  { TCdrDepthDiagramme }

  TCdrDepthDiagramme = class(TFrame)
    CdrDGCDrawingContext1: TCdrDGCDrawingContext;
    lbZMax: TLabel;
    lbZMin: TLabel;
    Panel1: TPanel;
    sclPourcentageSeuilHightLight: TScrollBar;
  private
    FBDDEntites: TBDDEntites;
    FFiltres            : String;
    FLongMini           : Double;
    FBarresLineColor    : TDGCColor;
    FBarresFillColor    : TDGCColor;
    FBarresFontColor    : TDGCColor;
    FNbBarres           : integer;
    FPourcentageSeuilHightLight: Double;
    FClassesRepartDepthDiagram : TVecteurDouble;

  public
    function Initialiser(const DT: TBDDEntites;
                         const Filtres: string;
                         const NbBarres: integer;
                         const LMini: double;
                         const BarresLineColor: TColor;
                         const BarresFillColor: TColor;
                         const PourcentageSeuilHightLight: double): boolean;
    procedure Finaliser();
    procedure SetNbBarres(const N: integer);
    procedure SetColorBarres(const BC, BF: TColor);
    procedure SetNombreColorBarres(const n: integer; const FG, BG: TColor);

    procedure SetPourcentageSeuilHightLight(const L: double);
    procedure SetLongueurMini(const L: double);
    procedure SetFiltre(const F: string);
    function  RecalculerDiagramme(): Boolean;
    procedure RedessinerDiagramme();
    procedure ExporterSVG(const FN: TStringDirectoryFilename);


  end;

implementation

{$R *.lfm}

{ TCdrDepthDiagramme }

function TCdrDepthDiagramme.Initialiser(const DT: TBDDEntites;
                                        const Filtres: string;
                                        const NbBarres: integer;
                                        const LMini: double;
                                        const BarresLineColor: TColor; const BarresFillColor: TColor;
                                        const PourcentageSeuilHightLight: double): boolean;
var
  EWE: double;
  BGC: TDGCColor;
begin
  result := false;
  BGC.setFrom(clWhite, 255);
  EWE := 20.00;
  try
    FBDDEntites := DT;
    SetFiltre(Filtres);
    SetNbBarres(NbBarres);
    SetLongueurMini(LMini);
    SetPourcentageSeuilHightLight(PourcentageSeuilHightLight);
    SetColorBarres(BarresLineColor, BarresFillColor);
    // SetMinMax indispensable ici
    FBDDEntites.SetMinMax(True);
    RecalculerDiagramme();
    result := CdrDGCDrawingContext1.Initialiser(-EWE, -EWE, EWE, EWE, False, BGC);
  except
  end;
end;

procedure TCdrDepthDiagramme.Finaliser();
begin
  pass;
end;
procedure TCdrDepthDiagramme.SetNbBarres(const N: integer);
begin
  FNbBarres := N;
end;

procedure TCdrDepthDiagramme.SetColorBarres(const BC, BF: TColor);
begin
  FBarresLineColor.setFrom(BC, 255);
  FBarresFillColor.setFrom(BF, 128);
  FBarresFontColor.setFrom(clBlack, 255);
end;
procedure TCdrDepthDiagramme.SetNombreColorBarres(const n: integer; const FG, BG: TColor);
begin
  SetNbBarres(n);
  SetColorBarres(FG, BG);
  RecalculerDiagramme();
end;

procedure TCdrDepthDiagramme.SetPourcentageSeuilHightLight(const L: double);
begin
  FPourcentageSeuilHightLight := L;
  sclPourcentageSeuilHightLight.Min :=   0;
  sclPourcentageSeuilHightLight.Max := 100;
  sclPourcentageSeuilHightLight.Position := Trunc(FPourcentageSeuilHightLight * 100);
end;

procedure TCdrDepthDiagramme.SetLongueurMini(const L: double);
begin
  FLongMini := L;
end;



procedure TCdrDepthDiagramme.SetFiltre(const F: string);
var
  QDevelViseesVisibles: double;
begin
  FFiltres := F;
  FBDDEntites.MetaFiltre(FFiltres, QDevelViseesVisibles);
end;

function TCdrDepthDiagramme.RecalculerDiagramme(): Boolean;
var
  QDevelViseesVisibles: double;
begin
  result := false;
  try
    FBDDEntites.MetaFiltre(FFiltres, QDevelViseesVisibles);
    Result := FBDDEntites.ParseDepthHistogramme(FNbBarres, FLongMini, True);
    FClassesRepartDepthDiagram := FBDDEntites.GetClassesRepartDepthDiagram();
  except
  end;
end;

procedure TCdrDepthDiagramme.RedessinerDiagramme();
var
  i: Integer;
  CBG, CHD          : TPoint3Df;
  PourcentageMax    : double;
  LargZoneDiagramme : double;
  HauteurZoneDiagramme, QH, QZ: double;
  FB: TDGCBoundingBox;
  BGC, ColorLineAxes       , ColorBrushAxes    ,  ColorFontAxes  : TDGCColor;
begin
  BGC.setFrom(clWhite, 255);
  ColorLineAxes.setFrom(clSilver , 255);
  ColorBrushAxes.setFrom(clWhite , 128 );
  ColorFontAxes.setFrom(clBlack, 255);




  CBG := FBDDEntites.GetCoinBasGauche();
  CHD := FBDDEntites.GetCoinHautDroit();
  lbZMax.Caption := Format(FORMAT_NB_REAL_0_DEC, [CHD.Z]);
  lbZMin.Caption := Format(FORMAT_NB_REAL_0_DEC, [CBG.Z]);

  // /!\ Les valeurs de FClassRepartDepthDiagram[] sont comprises entre 0.00 et 1.00
  PourcentageMax := -1.00;
  for i := 0 to FNbBarres - 1 do PourcentageMax := Max(PourcentageMax, FClassesRepartDepthDiagram[i]);
  LargZoneDiagramme    := PourcentageMax * 200.00;
  HauteurZoneDiagramme := CHD.Z - CBG.Z;

  CdrDGCDrawingContext1.SetViewLimits(-15.00, CBG.Z, LargZoneDiagramme + 10.00, CHD.Z, '');

  CdrDGCDrawingContext1.BeginDrawing();
    CdrDGCDrawingContext1.AddStyleSheet('Barres',
                                        FBarresLineColor, psSolid, 1, 0.05,
                                        FBarresFillColor, bsSolid,
                                        FBarresFontColor, [fsBold], DEFAULT_FONT_NAME, 15, 3.0,  '');
    CdrDGCDrawingContext1.AddStyleSheet('AxeV',
                                        ColorLineAxes , psSolid, 1, 0.05,
                                        ColorBrushAxes, bsClear,
                                        ColorFontAxes , [], DEFAULT_FONT_NAME, 15, 3.0,  '');



    FB := CdrDGCDrawingContext1.GetVueBounds();
    QH := HauteurZoneDiagramme / FNbBarres;
    for i := 0 to FNbBarres - 1 do
    begin
      QZ := CBG.Z + i * QH;
      if (FClassesRepartDepthDiagram[i] > (PourcentageMax * FPourcentageSeuilHightLight)) then
      begin
        CdrDGCDrawingContext1.AddTexte(1, FB.X1 + 1.00, QZ, 0, 0, Format(FORMAT_NB_REAL_0_DEC, [CBG.Z +  i * (CHD.Z - CBG.Z) / FNbBarres]));
      end;
      CdrDGCDrawingContext1.AddRectangle(1, 0.00, QZ, FClassesRepartDepthDiagram[i] * LargZoneDiagramme, QZ + QH);
    end;
    // textes
    CdrDGCDrawingContext1.SetBrushColorAttributes(BGC, bsSolid);
    CdrDGCDrawingContext1.SetFontColorAttributes('Arial', FBarresFontColor, 15, [fsbold]);
    for i := 0 to FNbBarres - 1 do
    begin
      QZ := CBG.Z + (i - 1) * QH;
      if (FClassesRepartDepthDiagram[i] > (PourcentageMax * FPourcentageSeuilHightLight)) then
      begin
        CdrDGCDrawingContext1.AddTexte(1, FB.X1 + 1.00, QZ, 0, 0, Format(FORMAT_NB_REAL_0_DEC, [CBG.Z +  i * (CHD.Z - CBG.Z) / FNbBarres]));
      end;
    end;
    CdrDGCDrawingContext1.AddInfiniteLine(2, tdgcVERTICAL_LINE, 0.00, 0.00);
  CdrDGCDrawingContext1.EndDrawing();
end;

procedure TCdrDepthDiagramme.ExporterSVG(const FN: TStringDirectoryFilename);
begin
  CdrDGCDrawingContext1.ExportSVG(FN);
end;

end.


