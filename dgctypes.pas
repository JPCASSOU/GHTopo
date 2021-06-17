unit DGCTypes;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, math, graphics;
const DGC_DEFAULT_PEN_WIDTH_IN_MM = 0.025;
const DIEU_AU_CARRE = -1; // dieu étant imaginaire pur, son carré vaut -1 ;-))))
const DGC_DEFAULT_FONT_NAME = 'Arial';

type

{ TDGCPoint2D }

 TDGCPoint2D = record
  X:  double;
  Y:  double;
  procedure setFrom(const QX, QY: double);
  procedure Empty();
end;
type TDGCArrayPoints2D = array of TDGCPoint2D;
type TDGCPoint2Dh = record  // coordonnées 2D homogènes
  X:  double;
  Y:  double;
  H:  double;
  procedure setFrom(const QX, QY: double; const QH: double = 1.0);
  procedure Empty();
end;
type TDGCBoundingBox = record
  X1:  double;
  Y1:  double;
  X2:  double;
  Y2:  double;
  procedure setFrom(const QX1, QY1, QX2, QY2: double);
  procedure Empty();
end;
type TDGCMatrix3x3 = array[0..2, 0..2] of double;

//Elements dessinés
type  TDGCOptions = set of (dgcoX_AXIS, dgcoY_AXIS,
                            dgcoPRI_X_GRID, dgcoPRI_Y_GRID,
                            dgcoSEC_X_GRID, dgcoSEC_Y_GRID,
                            dgcoCOORDS_RIVE_X, dgcoCOORDS_RIVE_Y
                           );

//Modes de travail spécifiques
type  TDGCModesTravail  = (mtgcsREADY,
                           mtgcsPAN_PREMIER_POINT, mtgcsPAN_SECOND_POINT,
                           mtgcsZOOM_PREMIER_COIN, mtgcsZOOM_SECOND_COIN,
                           mtgcsPICK_COORDS
                          );
// Types d'éléments de la liste d'affichage
type TDGCTypeDessinObjet = (
                   tdoNONE,
                   tdoENTITY_SEGMENT,       // TDGCSegment
                   tdoENTITY_INFINITE_LINE, // TDGCInfiniteLine
                   tdoENTITY_RECTANGLE,     // TDGCRectangle;
                   tdoENTITY_ELLIPSE,       // TDGCEllipse
                   tdoENTITY_TRIANGLE,      // TDGCTriangle
                   tdoENTITY_POLYLINE,      // TDGCPolyline
                   tdoENTITY_POLYGONE,      // TDGCPolygon
                   tdoENTITY_CURVE,         // TDGCCurve
                   tdoENTITY_TEXT,          // TDGCText
                   tdoCMD_SET_PEN,          // TDGCPen
                   tdoCMD_SET_BRUSH,        // TDGCBrush
                   tdoCMD_SET_FONT,         // TDGCFont
                   tdoCMD_SVG_BEGIN_GROUPE,
                   tdoCMD_SVG_END_GROUPE
                   );

type TDGCDessinObjet = record
  IdxGroupe      : integer;
  Name           : string;
  TypeDessinObjet: TDGCTypeDessinObjet;
  DessinObjet    : TObject;
  function GetDescroDessinObjet(): string;
end;
type

{ TDGCStyleSheet }

 TDGCStyleSheet = record
  Stylename        : string;
  Description      : string;
  // Crayon
  PenColor         : TColor;
  PenOpacity       : byte;
  PenStyle         : TPenStyle;
  PenWidthInPX     : byte;
  PenWidthInMM     : double;

  // Brosse
  BrushColor       : TColor;
  BrushOpacity     : byte;
  BrushStyle       : TBrushStyle;

  // Fontes
  FontName         : string;
  FontColor        : TColor; // Utiliser la couleur de Brush
  FontOpacity      : byte;
  FontSizeInPts    : integer;
  FontSizeInMM     : double;
  FontStyle        : TFontStyles;
  procedure setPen(const QPenColor: TColor; const QPenOpacity : byte; const QPenStyle: TPenStyle; const QPenWidthInPX: byte; const QPenWidthInMM: double);
  procedure setBrush(const QBrushColor: TColor; const QBrushOpacity: byte; const BrushStyle: TBrushStyle);
  procedure setFont(const QFontName: string; const QFontColor: TColor; const QFontOpacity: byte; const QFontStyle: TFontStyles; const QFontSizeInPts: integer; const QFontSizeInMM: double);
  procedure setDefault();
end;


// Types internes
type TTypeCurve = (tdgcBEZIER_CURVE, tdgcSPLINE_CURVE);
type TInfiniteLineOrientation = (tdgcVERTICAL_LINE, tdgcHORIZONTAL_LINE);
// Arc de courbe de Bézier
// On stocke les tangentes ET les points de contrôle

type TDGCBezierArc = record
  PT1    : TDGCPoint2D; // extremité courbe 1
  TangP1 : TDGCPoint2D; // Tangente en P1
  //PC1    : TDGCPoint2D; // Point de contrôle PC2
  PT2    : TDGCPoint2D; // extremité courbe 2
  TangP2 : TDGCPoint2D; // Tangente en P2
  //PC2    : TDGCPoint2D; // Point de contrôle PC2
end;


// pour les pointeurs sur fonctions de conversion
type TFunctionGetCoordsMonde = function(const PP: TPoint): TDGCPoint2D of object;
type TFunctionGetCoordsPlan  = function(const PM: TDGCPoint2D): TPoint of object;

// callbacks
type TProcTransmitCoords     = procedure(const P: TDGCPoint2D) of object;
//******************************************************************************
implementation
uses
  DGCDummyUnit; // pour limiter le bug de 'Fin de code source non trouvée

{ TDGCStyleSheet }

procedure TDGCStyleSheet.setPen(const QPenColor: TColor; const QPenOpacity: byte; const QPenStyle: TPenStyle; const QPenWidthInPX: byte; const QPenWidthInMM: double);
begin
  self.PenColor         := QPenColor;
  self.PenOpacity       := QPenOpacity;
  self.PenStyle         := QPenStyle;
  self.PenWidthInPX     := QPenWidthInPX;
  self.PenWidthInMM     := QPenWidthInMM;
end;

procedure TDGCStyleSheet.setBrush(const QBrushColor: TColor; const QBrushOpacity: byte; const BrushStyle: TBrushStyle);
begin
  self.BrushColor       := QBrushColor;
  self.BrushOpacity     := QBrushOpacity;
  self.BrushStyle       := BrushStyle;
end;

procedure TDGCStyleSheet.setFont(const QFontName: string; const QFontColor: TColor; const QFontOpacity: byte; const QFontStyle: TFontStyles; const QFontSizeInPts: integer; const QFontSizeInMM: double);
begin
  self.FontName         := QFontName;
  self.FontColor        := QFontColor;
  self.FontOpacity      := QFontOpacity;
  self.FontSizeInPts    := QFontSizeInPts;
  self.FontSizeInMM     := QFontSizeInMM;
  self.FontStyle        := QFontStyle;
end;

procedure TDGCStyleSheet.setDefault();
begin
  self.setPen(clBlack, 255, psSolid, 0, DGC_DEFAULT_PEN_WIDTH_IN_MM);
  self.setBrush(clWhite, 255, bsSolid);
  self.setFont(DGC_DEFAULT_FONT_NAME, clBlack, 255, [], 10, 1.20);
end;

{ TDGCPoint2D }

procedure TDGCPoint2D.setFrom(const QX, QY: double);
begin
  self.X := QX;
  self.Y := QY;
end;

procedure TDGCPoint2D.Empty();
begin
  self.X := 0.00;
  self.Y := 0.00;
end;

{ TDGCPoint2Dh }

procedure TDGCPoint2Dh.setFrom(const QX, QY: double; const QH: double = 1.0);
begin
  self.X := QX;
  self.Y := QY;
  self.H := QH;
end;

procedure TDGCPoint2Dh.Empty();
begin
  self.X := 0.00;
  self.Y := 0.00;
  self.H := 1.00;
end;

{ TDGCBoundingBox }

procedure TDGCBoundingBox.setFrom(const QX1, QY1, QX2, QY2: double);
begin
  self.X1 := QX1;
  self.Y1 := QY1;
  self.X2 := QX2;
  self.Y2 := QY2;

end;

procedure TDGCBoundingBox.Empty();
begin
  self.X1 := 0.00;
  self.Y1 := 0.00;
  self.X2 := 0.00;
  self.Y2 := 0.00;
end;



{ TDGCDessinObjet }

function TDGCDessinObjet.GetDescroDessinObjet(): string;
var
  LS: TStringList;
begin
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.Add('tdoNONE');
    LS.Add('tdoENTITY_SEGMENT');
    LS.Add('tdoENTITY_INFINITE_LINE');
    LS.Add('tdoENTITY_RECTANGLE');
    LS.Add('tdoENTITY_ELLIPSE');
    LS.Add('tdoENTITY_TRIANGLE');
    LS.Add('tdoENTITY_POLYLINE');
    LS.Add('tdoENTITY_POLYGONE');
    LS.Add('tdoENTITY_CURVE');
    LS.Add('tdoENTITY_TEXT');
    LS.Add('tdoCMD_SET_PEN');
    LS.Add('tdoCMD_SET_BRUSH');
    LS.Add('tdoCMD_SET_FONT');
    LS.Add('tdoCMD_SVG_BEGIN_GROUPE');
    LS.Add('tdoCMD_SVG_END_GROUPE');

    Result := LS.Strings[Ord(self.TypeDessinObjet)];
  finally
    FreeAndNil(LS) ;
  end;
end;

end.

