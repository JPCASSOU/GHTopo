unit frmPSDrawing2D;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CadreDGCDrawingContext,
  DGCTypes;

type

  { TdlgDispGraphisme2D }

  TdlgDispGraphisme2D = class(TForm)
    CdrDGCDrawingContext1: TCdrDGCDrawingContext;
  private
    FCurrentIdxStyleSheet : integer;
    FPolylineClosed       : boolean;

  public
    function Initialiser(const w, h: integer): boolean;
    procedure Finaliser();
    procedure SetBackgroundColor(const R, G, B, A: byte);
    function  AddStyleSheet(const QStylename: string;
                            const QPenColorR, QPenColorG, QPenColorB, QPenOpacity: byte; const QPenWidtdhInPX: byte;
                            const QBshColorR, QBshColorG, QBshColorB, QBshOpacity: byte; const QBshStyle: byte;
                            const QFntName: string; const QFntColorR, QFntColorG, QFntColorB, QFntOpacity: byte;
                            const QFntSizeInPX: byte; const QFntStyle: byte):Integer;
    procedure SetStyleSheet(const Idx: integer);
    function  BeginPolyline(const QName: string; const Closed: boolean): boolean;
    procedure AddVertex(const QX, QY: double);
    procedure EndPolyline();
    function  AddTexte(const QX, QY, AngleOrientation: double; const QAlignment: byte; const Texte: string): boolean;
    function  AddEllipse(const QX, QY, QR1, QR2: double): boolean;
    function  AddRectangle(const QX1, QY1, QX2, QY2: double): boolean;
    function  AddLigne(const QX1, QY1, QX2, QY2: double): boolean;

end;

var
  dlgDispGraphisme2D: TdlgDispGraphisme2D;

implementation

{$R *.lfm}

{ TdlgDispGraphisme2D }

function TdlgDispGraphisme2D.Initialiser(const w, h: integer): boolean;
var
  CGT: TDGCColor;
begin
  self.Position     := poScreenCenter;
  self.ClientWidth  := w;
  self.ClientHeight := h;
  CGT.setFrom(clWhite, 255);
  Result := CdrDGCDrawingContext1.Initialiser(-100, -100, 100, 100, True, CGT);
  FCurrentIdxStyleSheet := 0;
  FPolylineClosed       := false;
  try
    //CdrDGCDrawingContext1.AddStyleSheet(');
  except
    ShowMessage('Erreur FGC');
  end;
end;

procedure TdlgDispGraphisme2D.Finaliser();
begin
  CdrDGCDrawingContext1.ClearDrawing();
end;

procedure TdlgDispGraphisme2D.SetBackgroundColor(const R, G, B, A: byte);
begin
  try
    CdrDGCDrawingContext1.SetBackgroundColor(R, G, B, A);
    CdrDGCDrawingContext1.RefreshVue();
  except

  end;
end;

function TdlgDispGraphisme2D.AddStyleSheet(const QStylename: string;
                                           const QPenColorR, QPenColorG, QPenColorB, QPenOpacity: byte; const QPenWidtdhInPX: byte;
                                           const QBshColorR, QBshColorG, QBshColorB, QBshOpacity: byte; const QBshStyle: byte;
                                           const QFntName: string; const QFntColorR, QFntColorG, QFntColorB, QFntOpacity: byte;
                                           const QFntSizeInPX: byte; const QFntStyle: byte):Integer;
var
  QPenStyles: TPenStyle;
  QBshStyles: TBrushStyle;
  QFntStyles: TFontStyles;
  QPenColor, QBrushColor, QFontColor: TDGCColor;
begin
  Result := -1;
  QPenStyles := psSolid;
  QBshStyles := bsClear;
  case QBshStyle of
    0: QBshStyles := bsClear;
    1: QBshStyles := bsSolid;
  end;
  QFntStyles := [];
  case QFntStyle of
    1: Include(QFntStyles, fsBold);
  end;
  QPenColor.setFrom(QPenColorR, QPenColorG, QPenColorB, QPenOpacity);
  QBrushColor.setFrom(QBshColorR, QBshColorG, QBshColorB, QBshOpacity);
  QFontColor.setFrom(QFntColorR, QFntColorG, QFntColorB, QFntOpacity);
  CdrDGCDrawingContext1.AddStyleSheet(QStylename,
                                      QPenColor  , QPenStyles, QPenWidtdhInPX, 0.00,
                                      QBrushColor, QBshStyles,
                                      QFontColor , QFntStyles,
                                      QFntName, QFntSizeInPX,  2.00, '');


  // et retourne l'index de la dernière StyleSheet créée
  Result := CdrDGCDrawingContext1.GetNbStyleSheets() - 1;
end;

procedure TdlgDispGraphisme2D.SetStyleSheet(const Idx: integer);
var
  SS: TDGCStyleSheet;
  Nb: Integer;
begin
  FCurrentIdxStyleSheet := Idx;
  if (FCurrentIdxStyleSheet < 0) then FCurrentIdxStyleSheet := 0;
  Nb := CdrDGCDrawingContext1.GetNbStyleSheets() - 1;
  if (FCurrentIdxStyleSheet > Nb) then FCurrentIdxStyleSheet := Nb;
  SS := CdrDGCDrawingContext1.GetStyleSheet(FCurrentIdxStyleSheet);
  CdrDGCDrawingContext1.PutStyleSheet(FCurrentIdxStyleSheet, SS);
end;

function TdlgDispGraphisme2D.BeginPolyline(const QName: string; const Closed: boolean): boolean;
begin
  FPolylineClosed := Closed;
  if (FPolylineClosed) then Result := CdrDGCDrawingContext1.BeginPolygon(FCurrentIdxStyleSheet, QName)
                       else Result := CdrDGCDrawingContext1.BeginPolyline(FCurrentIdxStyleSheet, QName);
end;
procedure TdlgDispGraphisme2D.AddVertex(const QX, QY: double);
begin
  CdrDGCDrawingContext1.AddVertex(QX, QY);
end;
procedure TdlgDispGraphisme2D.EndPolyline();
begin
  if (FPolylineClosed) then CdrDGCDrawingContext1.EndPolygon()
                       else CdrDGCDrawingContext1.EndPolyline();
end;

function TdlgDispGraphisme2D.AddTexte(const QX, QY, AngleOrientation: double; const QAlignment:byte; const Texte: string): boolean;
begin
  Result := CdrDGCDrawingContext1.AddTexte(FCurrentIdxStyleSheet, QX, QY, QAlignment, AngleOrientation, Texte);
end;

function TdlgDispGraphisme2D.AddEllipse(const QX, QY, QR1, QR2: double): boolean;
begin
  result := CdrDGCDrawingContext1.AddEllipse(FCurrentIdxStyleSheet, QX, QY, QR1, QR2);
end;

function TdlgDispGraphisme2D.AddRectangle(const QX1, QY1, QX2, QY2: double): boolean;
begin
  result := CdrDGCDrawingContext1.AddRectangle(FCurrentIdxStyleSheet, QX1, QY1, QX2, QY2);

end;

function TdlgDispGraphisme2D.AddLigne(const QX1, QY1, QX2, QY2: double): boolean;
begin
  result := CdrDGCDrawingContext1.AddLine(FCurrentIdxStyleSheet, QX1, QY1, QX2, QY2);
end;

end.

// graphismes
// procedure TdlgDispGraphisme2D.MoveTo(const QX, QY: double);
function  BeginPolyline(const QIdxStyleSheet: integer; const QDoFlush: boolean=false): boolean;
   procedure AddVertex(const QX, QY: double);
   procedure EndPolyline();
   function  BeginPolygon(const QIdxStyleSheet: integer;
                          const QName: string = 'Polygone';
                          const QDoFlush: boolean=false): boolean;
   procedure EndPolygon();
   function  BeginCurve(const QIdxStyleSheet: integer;
                        const QName: string = 'Curve';
                        const QFilled: boolean = false;
                        const QDoFlush: boolean = false): boolean;
end.

