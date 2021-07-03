unit CadreRoseDiagramme;

// 15/10/2019: Validé. Utilise le nouveau contexte 2D TCdrDrawingContextGCS

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  math,
  StructuresDonnees,
  UnitEntitesExtended,
  DGCTypes,
  CadreDGCDrawingContext,
  Classes, SysUtils, Graphics, Forms, Controls;

type

{ TCdrRoseDiagramme }

 TCdrRoseDiagramme = class(TFrame)
    CdrDGCDrawingContext1: TCdrDGCDrawingContext;
  private
    FBDDEntites       : TBDDEntites;
    FNbPetales        : integer;
    FLongueurMiniVisee: double;
    FPetalesLineColor : TDGCColor;
    FPetalesFillColor : TDGCColor;
    FPetalesFontColor : TDGCColor;
    FFiltres          : string;
    FClassesRepartRoseDiagram: TVecteurDouble;
  public
    function Initialiser(const DT: TBDDEntites;
                         const Filtres: string;
                         const NbPetales: integer;
                         const LMini: double;
                         const PetalesLineColor: TColor; const PetalesFillColor: TColor;
                         const PetalesLineOpacity: byte = 255;   const PetalesFillOpacity: byte = 128): boolean;
    procedure Finaliser();
    procedure SetFiltres(const s: string);
    procedure SetNbPetales(const n: integer);
    procedure SetColorPetales(const FG, BG: TColor);
    procedure SetNombreColorPetales(const n: integer; const FG, BG: TColor);

    procedure SetLongueurMiniVisee(const l: double);
    function  RecalculerDiagramme(): boolean;
    procedure RedessinerDiagramme();
    procedure ExporterSVG(const FN: TStringDirectoryFilename);
  end;

implementation
CONST RAYON_ROSE_DIAGRAMME: double = 100.00;
CONST MARGE               : double = 10.00;

{$R *.lfm}

{ TCdrRoseDiagramme }

function TCdrRoseDiagramme.Initialiser(const DT: TBDDEntites; const Filtres: string;
                             const NbPetales: integer; const LMini: double;
                             const PetalesLineColor: TColor; const PetalesFillColor: TColor;
                             const PetalesLineOpacity: byte = 255;   const PetalesFillOpacity: byte = 128): boolean;
var
  EWE: Double;
  BGC: TDGCColor;
begin
  result := false;
  BGC.setFrom(clWhite, 255);
  try
    FBDDEntites := DT;
    SetFiltres(Filtres);
    SetColorPetales(PetalesLineColor, PetalesFillColor);
    SetNbPetales(NbPetales);
    SetLongueurMiniVisee(LMini);
    EWE := RAYON_ROSE_DIAGRAMME + MARGE;
    RecalculerDiagramme();
    result := CdrDGCDrawingContext1.Initialiser(-EWE, -EWE, EWE, EWE, True, BGC);
  except

  end;
end;

procedure TCdrRoseDiagramme.Finaliser();
begin
  CdrDGCDrawingContext1.Finaliser();

end;

procedure TCdrRoseDiagramme.SetFiltres(const s: string);
var
  QDevelVisees: double;
begin
  FFiltres := s;
  FBDDEntites.MetaFiltre(FFiltres, QDevelVisees);
end;
procedure TCdrRoseDiagramme.SetNbPetales(const n: integer);
begin
  FNbPetales := n;
end;
procedure TCdrRoseDiagramme.SetLongueurMiniVisee(const l: double);
begin
  FLongueurMiniVisee := l;
end;

procedure TCdrRoseDiagramme.SetColorPetales(const FG, BG: TColor);
begin
  FPetalesLineColor.setFrom(FG, 255);
  FPetalesFillColor.setFrom(BG, 192);
  FPetalesFontColor.setFrom(clBlack, 255);
end;

procedure TCdrRoseDiagramme.SetNombreColorPetales(const n: integer; const FG, BG: TColor);
begin
  SetNbPetales(n);
  SetColorPetales(FG, BG);
  RecalculerDiagramme();
end;

function TCdrRoseDiagramme.RecalculerDiagramme(): boolean;
var
  QDevelViseesVisibles: double;
begin
  result := false;
  try
    FBDDEntites.MetaFiltre(FFiltres, QDevelViseesVisibles);
    Result := FBDDEntites.ParseRoseDiagram(FNbPetales, FLongueurMiniVisee);
    FClassesRepartRoseDiagram := FBDDEntites.GetClassesRepartRoseDiagram();
  except
  end;
end;

procedure TCdrRoseDiagramme.RedessinerDiagramme();
const
  PI_SUR_2: double = pi / 2;
var
  QMax666: double;
  pipi, QInterval, EWE: ValReal;
  i, j, IdxStyleReticule: Integer;
  Ang1, Ang2, RC: double;
  s1, c1, s2, c2: double;
  BGC, ColorLineAxes       , ColorBrushAxes    ,  ColorFontAxes  : TDGCColor;
begin
  BGC.setFrom(clWhite, 255);
  ColorLineAxes.setFrom(clSilver , 255);
  ColorBrushAxes.setFrom(clWhite , 128 );
  ColorFontAxes.setFrom(clBlack, 255);
  QInterval := pi / FNbPetales;
  QMax666 := -1.00;
  for i := 0 to FNbPetales - 1 do QMax666 := Max(QMax666, FClassesRepartRoseDiagram[i]);


  CdrDGCDrawingContext1.BeginDrawing();
    CdrDGCDrawingContext1.AddStyleSheet('Barres',
                                        FPetalesLineColor, psSolid, 1, 0.05,
                                        FPetalesFillColor, bsSolid,
                                        FPetalesFontColor, [fsBold], DEFAULT_FONT_NAME, 15, 3.0,  '');
    CdrDGCDrawingContext1.AddStyleSheet('Axes',
                                        ColorLineAxes , psSolid, 1, 0.05,
                                        ColorBrushAxes, bsClear,
                                        ColorFontAxes , [], DEFAULT_FONT_NAME, 15, 3.0,  '');
    for j := 0 to 1 do // deux demi-diagrammes centralement symétriques
    begin
      EWE := PI * (1 + j);
      for i := 0 to FNbPetales - 1 do
      begin
        Ang1 := EWE + (PI_SUR_2 - i * QInterval);
        Ang2 := EWE + (PI_SUR_2 - (i + 1) * QInterval);
        sincos(Ang1, s1, c1);
        sincos(Ang2, s2, c2);
        RC := FClassesRepartRoseDiagram[i] * RAYON_ROSE_DIAGRAMME;
        CdrDGCDrawingContext1.AddTriangle(1,
                                          0.0, 0.0,
                                          RC * c1, RC * s1,
                                          RC * c2, RC * s2,
                                          Format('%.f', [100 * FClassesRepartRoseDiagram[i]]));
      end;
    end;
    // dessin du réticule
    IdxStyleReticule := 2;
    CdrDGCDrawingContext1.AddEllipse(IdxStyleReticule, 0.0, 0.0, RAYON_ROSE_DIAGRAMME, RAYON_ROSE_DIAGRAMME);
    CdrDGCDrawingContext1.AddEllipse(IdxStyleReticule, 0.0, 0.0, 0.5 * RAYON_ROSE_DIAGRAMME, 0.5 * RAYON_ROSE_DIAGRAMME);

    CdrDGCDrawingContext1.AddLine(IdxStyleReticule, 0.0 - RAYON_ROSE_DIAGRAMME, 0.0, 0.0 + RAYON_ROSE_DIAGRAMME, 0.0);
    CdrDGCDrawingContext1.AddLine(IdxStyleReticule, 0.0, 0.0 - RAYON_ROSE_DIAGRAMME, 0.0, 0.0 + RAYON_ROSE_DIAGRAMME);
  CdrDGCDrawingContext1.EndDrawing();
end;

procedure TCdrRoseDiagramme.ExporterSVG(const FN: TStringDirectoryFilename);
begin
  CdrDGCDrawingContext1.ExportSVG(FN);
end;

end.

