unit DGCTypes;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, math, graphics;
const DGC_DEFAULT_PEN_WIDTH_IN_MM = 0.025;
const DIEU_AU_CARRE = -1; // dieu étant imaginaire pur, son carré vaut -1 ;-))))
const DGC_DEFAULT_FONT_NAME = 'Arial';

// TGHTopoColor: Unifie tous les types de couleur et leurs fonctions
type

{ TDGCColor }

 TDGCColor = record
    Red     : byte;
    Green   : byte;
    Blue    : byte;
    Alpha   : byte;
    function  getFloatBlue(): double;  // retourne un réel entre 0 et 1
    function  getFloatGreen(): double;
    function  getFloatRed(): double;
    function  getFloatAlpha(): double;

    procedure setFrom(const R, G, B: byte; const A: byte = 255); overload;
    procedure setFrom(const C: TColor; const A: byte = 255); overload;
    function  toTColor(): TColor;
    procedure setFromStrGHTopoColor(const S: string; const qDefault: TColor);
    function  toStrGHTopoColor(): string;
    procedure setOpacity(const O: byte);
    function  toHTMLColor(): string;
    function  toKMLColor(): string;
    function  toSVGColor(): string;
    function  toGrayScale(): byte;
end;

type

{ TDGCPoint2D }

 TDGCPoint2D = record
  X:  double;
  Y:  double;
  procedure setFrom(const QX, QY: double);
  procedure Empty();
  function  getNorme(): double;
end;
type TDGCBezierArc = record
  PT1    : TDGCPoint2D; // extremité courbe 1
  TangP1 : TDGCPoint2D; // Tangente en P1
  PT2    : TDGCPoint2D; // extremité courbe 2
  TangP2 : TDGCPoint2D; // Tangente en P2
end;

type

{ TDGCArrayPoints2D }

 TDGCArrayPoints2D = record
  M: array of TDGCPoint2D;
  procedure Empty();
  procedure SetCapacity(const N: integer);
  function  GetNbElements(): integer;
  function  GetElement(const Idx: integer): TDGCPoint2D;
  procedure SetElement(const Idx: integer; const P: TDGCPoint2D); overload;
  procedure SetElement(const Idx: integer; const QX, QY: double); overload;

  procedure AddElement(const P: TDGCPoint2D); overload;
  procedure AddElement(const QX, QY: double); overload;
end;
type

{ TListDGCBezierArcs }

 { TDGCListBezierArcs }

 TDGCListBezierArcs = record
  M: array of TDGCBezierArc;
  procedure Empty();
  procedure SetCapacity(const N: integer);
  function  GetNbElements(): integer;
  function  GetElement(const Idx: integer): TDGCBezierArc;
  procedure SetElement(const Idx: integer; const P: TDGCBezierArc); overload;
  procedure AddElement(const P: TDGCBezierArc); overload;

end;


type

{ TDGCPoint2Dh }

 TDGCPoint2Dh = record  // coordonnées 2D homogènes
  X:  double;
  Y:  double;
  H:  double;
  procedure setFrom(const QX, QY: double; const QH: double = 1.0);
  procedure Empty();
  function  getNorme(): double;
end;
type

{ TDGCBoundingBox }

 TDGCBoundingBox = record
  X1:  double;
  Y1:  double;
  X2:  double;
  Y2:  double;
  procedure setFrom(const QX1, QY1, QX2, QY2: double);
  procedure Empty();
  procedure Reset();
  procedure upDate(const QX, QY: double); overload;
  procedure upDate(const P: TDGCPoint2D); overload;
  procedure upDate(const P: TDGCPoint2Dh); overload;
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
  PenColor         : TDGCColor;
  PenStyle         : TPenStyle;
  PenWidthInPX     : byte;
  PenWidthInMM     : double;

  // Brosse
  BrushColor       : TDGCColor;
  BrushStyle       : TBrushStyle;

  // Fontes
  FontName         : string;
  FontColor        : TDGCColor; // Utiliser la couleur de Brush
  FontSizeInPts    : integer;
  FontSizeInMM     : double;
  FontStyle        : TFontStyles;
  procedure setPen(const QPenColor: TDGCColor; const QPenStyle: TPenStyle; const QPenWidthInPX: byte; const QPenWidthInMM: double);
  procedure setBrush(const QBrushColor: TDGCColor;  const BrushStyle: TBrushStyle);
  procedure setFont(const QFontName: string; const QFontColor: TDGCColor; const QFontStyle: TFontStyles; const QFontSizeInPts: integer; const QFontSizeInMM: double);
  procedure setDefault();
end;

 type

 { TDGCLayer }

 TDGCLayer = record
   Name        : string;
   Color       : TDGCColor;

   AcadColorIdx: integer;

   procedure setFrom(const QName: string; const QAcadIdxColor: integer); overload;
   procedure setFrom(const QName: string; const QAcadIdxColor: integer; const QR, QG, QB, QA: byte); overload;



 end;


// Types internes
type TTypeCurve = (tdgcBEZIER_CURVE, tdgcSPLINE_CURVE);
type TInfiniteLineOrientation = (tdgcVERTICAL_LINE, tdgcHORIZONTAL_LINE);
// Arc de courbe de Bézier
// On stocke les tangentes ET les points de contrôle




// pour les pointeurs sur fonctions de conversion
type TFunctionGetCoordsMonde = function(const PP: TPoint): TDGCPoint2D of object;
type TFunctionGetCoordsPlan  = function(const PM: TDGCPoint2D): TPoint of object;

// callbacks
type TProcTransmitCoords     = procedure(const P: TDGCPoint2D) of object;
//******************************************************************************
implementation
uses
  DGCDummyUnit; // pour limiter le bug de 'Fin de code source non trouvée

{ TDGCLayer }

procedure TDGCLayer.setFrom(const QName: string; const QAcadIdxColor: integer);
begin
  self.Name         := QName;
  self.AcadColorIdx := QAcadIdxColor;
  self.Color.setFrom(255, 0, 0, 255);
  //...........
end;

procedure TDGCLayer.setFrom(const QName: string; const QAcadIdxColor: integer; const QR, QG, QB, QA: byte);
begin
  self.Name         := QName;
  self.AcadColorIdx := QAcadIdxColor;
  self.Color.setFrom(QR, QG, QB, QA);
end;

{ TDGCListBezierArcs }

procedure TDGCListBezierArcs.Empty();
begin
  setLength(self.M, 0);
end;

procedure TDGCListBezierArcs.SetCapacity(const N: integer);
begin
  SetLength(self.M, N);
end;

function TDGCListBezierArcs.GetNbElements(): integer;
begin
  Result := length(self.M);
end;

function TDGCListBezierArcs.GetElement(const Idx: integer): TDGCBezierArc;
begin
  result := self.M[Idx];
end;

procedure TDGCListBezierArcs.SetElement(const Idx: integer; const P: TDGCBezierArc);
begin
  self.M[Idx] := P;
end;

procedure TDGCListBezierArcs.AddElement(const P: TDGCBezierArc);
var
  n: Integer;
begin
  n := length(self.M);
  setlength(self.M, n+1);
  self.M[n] := P;

end;

//******************************************************************************
{ TDGCArrayPoints2D }
procedure TDGCArrayPoints2D.Empty();
begin
  setLength(self.M, 0);
end;

procedure TDGCArrayPoints2D.SetCapacity(const N: integer);
begin
  SetLength(self.M, N);
end;

function TDGCArrayPoints2D.GetNbElements(): integer;
begin
  Result := length(self.M);
end;

function TDGCArrayPoints2D.GetElement(const Idx: integer): TDGCPoint2D;
begin
  result := self.M[Idx];
end;

procedure TDGCArrayPoints2D.SetElement(const Idx: integer; const P: TDGCPoint2D);
begin
  self.M[Idx] := P;
end;

procedure TDGCArrayPoints2D.SetElement(const Idx: integer; const QX, QY: double);
var
  P2d: TDGCPoint2D;
begin
  P2D.setFrom(QX, QY);
  self.SetElement(Idx, P2d);
end;

procedure TDGCArrayPoints2D.AddElement(const P: TDGCPoint2D);
var
  n: Integer;
begin
  n := length(self.M);
  setlength(self.M, n+1);
  self.M[n] := P;
end;

procedure TDGCArrayPoints2D.AddElement(const QX, QY: double);
var
  P2d: TDGCPoint2D;
begin
  P2D.setFrom(QX, QY);
  self.AddElement(P2d);
end;

{ TDGCColor }
{ TGHTopoColor }

procedure TDGCColor.setFrom(const R, G, B: byte; const A: byte = 255);
begin
  self.Red   := R AND 255;
  self.Green := G AND 255;
  self.Blue  := B AND 255;
  self.Alpha := A AND 255;
end;

procedure TDGCColor.setFrom(const C: TColor; const A: byte = 255);
begin
  self.setFrom(Graphics.Red(C), Graphics.Green(C), Graphics.Blue(C), A);
end;

function TDGCColor.toTColor(): TColor;
begin
  result := RGBToColor(self.Red, self.Green, self.Blue);
end;
// récupère les RGB depuis les représentations $RRRGGGBBB et #RRRGGGBBB
procedure TDGCColor.setFromStrGHTopoColor(const S: string; const qDefault: TColor);
begin
  if (length(S) < 10) then // la notation Pascal exige 1 + 3*3 caractères
    self.setFrom(clDefault, 255)
  else
    self.setFrom(StrToIntDef(Copy(S, 2, 3), Graphics.Red(qDefault)),
                 StrToIntDef(Copy(S, 5, 3), Graphics.Green(qDefault)),
                 StrToIntDef(Copy(S, 8, 3), Graphics.Blue(qDefault)),
                 255)
end;


function TDGCColor.toStrGHTopoColor(): string;
begin
  Result := Format('$%.3d%.3d%.3d', [self.Red, self.Green, self.Blue]);
end;

procedure TDGCColor.setOpacity(const O: byte);
begin
  self.Alpha := O and 255;
end;

function TDGCColor.toHTMLColor(): string;
begin
  Result := format('#%.2X%.2X%.2X', [self.Red, self.Green, self.Blue]);
end;

function TDGCColor.toKMLColor(): string;
begin
  Result := Format('<color>%.2X%.2X%.2X%.2X</color>',[self.Alpha, self.Blue, self.Green, self.Red]);
end;

function TDGCColor.toSVGColor(): string;
const m = 1 / 256.0;
begin
  Result := Format(' rgb(%.2f%%, %.2f%%, %.2f%%)', [self.Red * m , self.Green * m , self.Blue * m]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
end;

function TDGCColor.toGrayScale(): byte; //Convertit une couleur en niveaux de gris; méthode NTSC.
begin
  Result := round(0.30 * self.Red + 0.59 * self.Green + 0.11 * self.Blue);
end;

function TDGCColor.getFloatRed(): double;
begin
  Result := self.Red / 256.0;
end;
function TDGCColor.getFloatGreen(): double;
begin
  Result := self.Green / 256.0;
end;
function TDGCColor.getFloatBlue(): double;
begin
  Result := self.Blue / 256.0;
end;
function TDGCColor.getFloatAlpha(): double;
begin
  Result := self.Alpha / 256.0;
end;

{ TDGCStyleSheet }
procedure TDGCStyleSheet.setPen(const QPenColor: TDGCColor; const QPenStyle: TPenStyle; const QPenWidthInPX: byte; const QPenWidthInMM: double);
begin
  self.PenColor         := QPenColor;
  self.PenStyle         := QPenStyle;
  self.PenWidthInPX     := QPenWidthInPX;
  self.PenWidthInMM     := QPenWidthInMM;
end;

procedure TDGCStyleSheet.setBrush(const QBrushColor: TDGCColor;  const BrushStyle: TBrushStyle);
begin
  self.BrushColor       := QBrushColor;
  self.BrushStyle       := BrushStyle;
end;

procedure TDGCStyleSheet.setFont(const QFontName: string; const QFontColor: TDGCColor; const QFontStyle: TFontStyles; const QFontSizeInPts: integer; const QFontSizeInMM: double);
begin
  self.FontName         := QFontName;
  self.FontColor        := QFontColor;
  self.FontSizeInPts    := QFontSizeInPts;
  self.FontSizeInMM     := QFontSizeInMM;
  self.FontStyle        := QFontStyle;
end;

procedure TDGCStyleSheet.setDefault();
var CGT: TDGCColor;
begin
  CGT.setFrom(clBlack, 255);
  self.setPen(CGT, psSolid, 0, DGC_DEFAULT_PEN_WIDTH_IN_MM);
  CGT.setFrom(clWhite, 255);
  self.setBrush(CGT, bsSolid);
  CGT.setFrom(clBlack, 255);
  self.setFont(DGC_DEFAULT_FONT_NAME, CGT, [], 10, 1.20);
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

function TDGCPoint2D.getNorme(): double;
begin
  result := hypot(self.X, self.Y);
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

function TDGCPoint2Dh.getNorme(): double;
begin
  result := hypot(self.X, self.Y);
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

procedure TDGCBoundingBox.Reset();
begin
  self.setfrom(Infinity, Infinity, -Infinity, -Infinity);
end;

procedure TDGCBoundingBox.upDate(const QX, QY: double);
begin
  self.X1 := Min(self.X1, QX);
  self.Y1 := Min(self.Y1, QY);
  self.X2 := Max(self.X2, QX);
  self.Y2 := Max(self.Y2, QY);
end;

procedure TDGCBoundingBox.upDate(const P: TDGCPoint2D);
begin
  self.upDate(P.X, P.Y);
end;

procedure TDGCBoundingBox.upDate(const P: TDGCPoint2Dh);
begin
  self.upDate(P.X, P.Y);
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

