unit PostScriptCanvasUnit;

interface
uses
  StructuresDonnees,
  SysUtils,
  Classes,
  Common,
  Graphics;

// Fontes pour PostScript
type TFontPSProperties = record
  Name  : string;
  Size  : integer;
  Height: integer;
  Color : TColor;
  Style : TFontStyles;
end;
type TPenPSProperties = record
  Color   : TColor;
  fWidth  : double;
end;
type TBrushPSProperties = record
  Color   : TColor;
  Opacity : byte;
end;

type

{ TPostScriptCanvas }

TPostScriptCanvas = class
    constructor Create;
    destructor  Destroy; override;
  private
    FpPSFILE: TextFile;
    FNbWrittenLines: integer;
    FCommentaire: string;
    FScale: double;

    FFont : TFontPSProperties;
    FPen  : TPenPSProperties;
    FBrush: TBrushPSProperties;
    FXMin,
    FXMax,
    FYMin,
    FYMax : Double;
    procedure DefineColor(const C: TColor); inline;
    procedure DefinePenColorWidth(const P: TPenPSProperties);
    procedure DefineBrushColorAlpha(const B: TBrushPSProperties);
    procedure DrawBorderedPolygon(const Points: array of TPoint2Df);
    function GCSToPSCoord(const QX, QY: double): TPoint2Df;
    procedure LineTo(const X, Y: Double);
    procedure MoveTo(const X, Y: Double);
    procedure SetScale(const E: double);
    procedure TextOut(const X, Y: Double; const Text: string);
    procedure WriteLine(const S: string);
    procedure SetDrawingBounds(const X1, Y1, X2, Y2: Double);
  public
    function  InitializeDocument(const QFileName: string; const DoXHTML: boolean; const QTitre, QDescro: string;
                                 const X1, Y1, X2, Y2: Double): boolean;
    procedure FinalizeDocument();
    // groupes
    procedure BeginGroupe(const GroupName, Description: string; const dx, dy: double);
    procedure BeginSubGroupe(const MainGroupName, SubGroupName, Description, SVGStyle: string);
    procedure EndSubGroupe(const MainGroupName, SubGroupName: string);
    procedure EndGroupe(const GroupName: string);
    // définition des patterns
    procedure BeginPatternsSection();
    procedure EndPatternsSection();
    // définition des feuilles de styles
    procedure BeginStylesSection();
    procedure EndStylesSection();
    procedure WriteStyleSolidLine(const Nomstyle: string; const CouleurLigne: TColor; const LargeurLigne: double; const DescroStyle: string);
    procedure WriteStyleSolidPolygone(const Nomstyle: string; const CouleurLigne, CouleurPolygone: TColor; const LargeurLigne: double; const DescroStyle: string);
    procedure WriteStyleText(const Nomstyle: string; const QFontName: string; const QFontAttr: TFontStyles; const QFontColor: TColor; const QFontSize: double; const DescroStyle: string);
    // section dessin (contenu)
    procedure BeginDrawingSection();
    procedure EndDrawingSection();
    // définition des couleurs et fontes
    procedure SetPen(const Value: TPenPSProperties);
    function  GetPen: TPenPSProperties;
    procedure SetBrush(const Value: TBrushPSProperties);
    function  GetBrush(): TBrushPSProperties;
    procedure SetFont(const Value: TFontPSProperties);
    procedure SetDefaultPen(const Value: TPenPSProperties);
    procedure SetDefaultBrush(const Value: TBrushPSProperties);

    //****************************
    // définition de commentaires
    procedure WriteCommentaire(const s: string);
    procedure WriteCommand(const s: string);
    procedure WriteVerbatimLine(const s: string);

    // définition de couches
    procedure BeginLayer(const LayerName: string);
    procedure EndLayer(const LayerName: string);
    // routines de dessin
    procedure DrawPoint(const CSSClass: string; const X,Y: Double);
    procedure DrawLine(const CSSClass: string; const X1, Y1, X2, Y2: Double);
    procedure DrawCircle(const CSSClass: string; const XC, YC, R: double);
    procedure DrawArc(const CSSClass: string; const XC, YC, R, Ang0, Ang1: double; const DoPie: boolean);
    procedure DrawPolylign(const CSSClass: string; const Points: TArrayPoints2Df);
    procedure DrawPolygon(const CSSClass: string; const Points: TArrayPoints2Df);
    procedure DrawPolygoneInscrit(const CSSClass: string; const N: integer; const X0, Y0, R: double);

    procedure DrawPolygonSansStyle(const CouleurLigne, CouleurRemplissage: TColor; const LigneWidth: double; const Points: TArrayPoints2Df; const Closed: boolean);
    procedure DrawPolygonOrPolyline(const CSSClass: string; const Points: TArrayPoints2Df; const Closed: boolean);


    procedure DrawText(const CSSClass: string;  const Alignment: byte; const X,Y, Rotation: Double; const Text: string);
    procedure DrawImage(const FichierImage: string; const X, Y: double; const W, H: double);

    function  SVGColor(const C:TColor):string;
    // conversion de coordonnées repère GHCD vers repère SVG
    function  CoordsGCSToSVG(const MyPt: TPoint2Df): TPoint2Df;
    // dessin d'un  rectangle
    function  DrawRectangle(const QClassStyle: string; const X1, Y1, X2, Y2: double): boolean;
    // dessin d'un symbole
    function  DrawSymbole(const IDSymbole: string; const X, Y, L, H, Rotate: double): boolean;
    // tracé de courbes de Bézier à arcs multiples
    function  DrawBezierCurve(const QClassStyle: string; const BC : TArrayPoints2Df; const Closed: boolean): boolean;
    //*******************
    property  Commentaire: string read FCommentaire write FCommentaire;
    property  Scale: double read FScale write FScale;
//*******************
end;

implementation


procedure DisplayMsg(const Str: string);
begin
  ;//WriteLn(Str);
end;


constructor TPostScriptCanvas.Create;
begin
 inherited Create;
 DisplayMsg(Format('%s.Create',[ClassName]));
 try

 except
 end;
end;


destructor TPostScriptCanvas.Destroy;
begin
 DisplayMsg(Format('%s.Free',[ClassName]));
 inherited Destroy;
end;

procedure TPostScriptCanvas.WriteLine(const S: string);
begin
  WriteLn(FpPSFILE, s);
end;

procedure TPostScriptCanvas.SetDrawingBounds(const X1, Y1, X2, Y2: Double);
begin
  FXMin:=X1;
  FYMin:=Y1;
  FXMax:=X2;
  FYMax:=Y2;
end;

//**** Définition des pinceaux et couleurs
function TPostScriptCanvas.GetPen: TPenPSProperties;
begin
  Result:=FPen;
end;
procedure TPostScriptCanvas.SetPen(const Value: TPenPSProperties);
begin
  FPen:=Value;
end;
procedure TPostScriptCanvas.SetBrush(const Value: TBrushPSProperties);
begin
  FBrush := Value;
end;

function  TPostScriptCanvas.GetBrush: TBrushPSProperties;
begin
  Result:=FBrush;
end;

procedure TPostScriptCanvas.SetFont(const Value: TFontPSProperties);
begin
  WriteLine('/Helvetica findfont');
  WriteLine(Format('%d scalefont setfont',[Value.Size]));

end;

procedure TPostScriptCanvas.SetDefaultPen(const Value: TPenPSProperties);
begin

end;

procedure TPostScriptCanvas.SetDefaultBrush(const Value: TBrushPSProperties);
begin

end;





procedure TPostScriptCanvas.WriteCommand(const s: string);
begin
  WriteLine(Trim(S));
end;

procedure TPostScriptCanvas.WriteVerbatimLine(const s: string);
begin

end;

//****************************
procedure TPostScriptCanvas.WriteCommentaire(const s: string);
begin
  WriteLine(Format('%% %s %%',[s]));
end;
//****************************
function TPostScriptCanvas.InitializeDocument(const QFileName: string; const DoXHTML: boolean; const QTitre, QDescro: string;
                                             const X1, Y1, X2, Y2: Double): boolean;
  procedure DefinePSMacro(const Alias, Instrs, Comments: string);
  begin
    WriteLine(Format('/%s {%s} def %% %s',[Alias, Instrs, Comments]));
  end;
begin
  Result:=False;
  SetDrawingBounds(X1, Y1, X2, Y2);



  FNbWrittenLines:=0;
  assignFile(FpPSFILE, QFileName);
  try
    ReWrite(FpPSFILE);
    //-------------------------------
    // écriture de l'en tête ici
    // ------------------------------
    WriteLine('%!PS-Adobe-3.0');
    WriteLine('%% PostScript File generated by GHTopo %%');
    WriteLine(Format('%%%% File    : %s %%%%',[QFileName]));
    WriteLine(Format('%%%% Title   : %s %%%%',[QTitre]));

    WriteLine(Format('%%%% Date    : %s %%%%',[DateToStr(Now)]));
    WriteLine(Format('%%%% Comments: %s %%%%',[QDescro]));
    WriteLine(Format('%%%% BoundingBox: %.2f %.2f %.2f %.2f', [FXMin, FYMin, FXMax, FYMax]));

    WriteLine(Format('%%%%%s%%%%',[StringOfChar('=',80)]));
    // écriture des alias
    WriteLine('% Alias list');
    DefinePSMacro('m', 'moveto', '');
    DefinePSMacro('l', 'lineto', '');
    DefinePSMacro('srgb', 'setrgbcolor', '');

    // le dessin
    WriteLine('');
    WriteLine('%% Drawing %%');
    WriteLine('');
    Result:=True;
  except
    DisplayMsg('Error initializing text file');
    CloseFile(FpPSFILE);
  end;
end;


procedure TPostScriptCanvas.BeginGroupe(const GroupName, Description: string; const dx, dy: double);
begin

end;

procedure TPostScriptCanvas.BeginSubGroupe(const MainGroupName, SubGroupName, Description, SVGStyle: string);
begin

end;

procedure TPostScriptCanvas.EndSubGroupe(const MainGroupName, SubGroupName: string);
begin

end;

procedure TPostScriptCanvas.EndGroupe(const GroupName: string);
begin

end;

procedure TPostScriptCanvas.BeginPatternsSection();
begin

end;

procedure TPostScriptCanvas.EndPatternsSection();
begin

end;

procedure TPostScriptCanvas.BeginStylesSection();
begin

end;

procedure TPostScriptCanvas.EndStylesSection();
begin

end;

procedure TPostScriptCanvas.WriteStyleSolidLine(const Nomstyle: string; const CouleurLigne: TColor; const LargeurLigne: double; const DescroStyle: string);
begin

end;

procedure TPostScriptCanvas.WriteStyleSolidPolygone(const Nomstyle: string; const CouleurLigne, CouleurPolygone: TColor; const LargeurLigne: double; const DescroStyle: string);
begin

end;

procedure TPostScriptCanvas.WriteStyleText(const Nomstyle: string; const QFontName: string; const QFontAttr: TFontStyles; const QFontColor: TColor; const QFontSize: double; const DescroStyle: string);
begin

end;

procedure TPostScriptCanvas.BeginDrawingSection();
begin
  ;
end;

procedure TPostScriptCanvas.EndDrawingSection();
begin
  ;
end;

procedure TPostScriptCanvas.FinalizeDocument();
begin
  try
    writeLine('showpage');
    WriteLine('%EOF');
  finally
    CloseFile(FpPSFILE);
  end;
end;

function TPostScriptCanvas.GCSToPSCoord(const QX, QY: double): TPoint2Df;
var
  dx, dy: double;
  x1, y1: double;
begin
  dx := FXMax - FXMin;
  dy := FYMax - FYMin;
  x1 := QX - FXMin;
  y1 := QY - FYMin;
  Result.setFrom(592 * x1 / dx, 592 * y1 / dx);
end;

// définition de couches
procedure TPostScriptCanvas.BeginLayer(const LayerName: string);
begin
  WriteLine(Format('%% Begin Layer: %s %%',[LayerName]));
  WriteLine('%AI5_BeginLayer');

end;
procedure TPostScriptCanvas.EndLayer(const LayerName: string);
begin
  WriteLine('%AI5_EndLayer--');
  WriteLine(Format('%% End Layer: %s %%',[LayerName]));
end;
//********* routines de dessin
procedure TPostScriptCanvas.MoveTo(const X,Y: Double);
var
  PP: TPoint2Df;
begin
  WriteLine('newpath');
  PP := GCSToPSCoord(X, Y);
  WriteLine(Format('  %f %f m',[PP.X, PP.Y]));
end;
// mise à l'échelle
procedure TPostScriptCanvas.SetScale(const E: double);
begin
  FScale := E;
end;

procedure TPostScriptCanvas.LineTo(const X,Y: Double);  (* peu utilisé *)
var
  PP: TPoint2Df;
begin
  PP := GCSToPSCoord(X, Y);
  WriteLine(Format('  %f %f l',[PP.X,PP.Y]));
end;

procedure TPostScriptCanvas.DrawPoint(const CSSClass: string; const X, Y: Double);
begin
;
end;
procedure TPostScriptCanvas.DrawCircle(const CSSClass: string; const XC, YC, R: double);
var
  PP: TPoint2Df;
begin
  PP := GCSToPSCoord(XC, YC);
  WriteLine('newpath');
  WriteLine(Format('%f %f %f 0 360 arc',[PP.X, PP.Y, R]));
  DefinePenColorWidth(FPen);
  WriteLine(Format('%f setlinewidth',[FPen.fWidth]));
  WriteLine('stroke');
end;

procedure TPostScriptCanvas.DrawArc(const CSSClass: string; const XC, YC, R, Ang0, Ang1: double; const DoPie: boolean);
begin

end;

procedure TPostScriptCanvas.DrawLine(const CSSClass: string; const X1, Y1, X2, Y2: Double);
var
  PP1, PP2: TPoint2Df;
begin
  PP1 := GCSToPSCoord(X1, Y1);
  PP2 := GCSToPSCoord(X2, Y2);
  WriteLine('newpath');
  WriteLine(Format('  %f %f m',[PP1.X, PP1.Y]));
  WriteLine(Format('  %f %f l',[PP2.X, PP2.Y]));
  DefinePenColorWidth(FPen);
  WriteLine('stroke');
end;

procedure TPostScriptCanvas.DrawPolylign(const CSSClass: string; const Points: TArrayPoints2Df);
var
  i: integer;
  PP: TPoint2Df;
begin
  if (High(Points)<1) then Exit;
  WriteLine('newpath');
  PP := GCSToPSCoord(Points[0].X, Points[0].Y);
  WriteLine(Format('  %f %f m',[PP.X, PP.Y]));
  for i:=1 to High(Points) do
  begin
    PP := GCSToPSCoord(Points[i].X, Points[i].Y);
    WriteLine(Format('  %f %f l',[PP.X, PP.Y]));
  end;
  DefinePenColorWidth(FPen);
  WriteLine('stroke');
end;

procedure TPostScriptCanvas.DrawPolygon(const CSSClass: string; const Points: TArrayPoints2Df);
var
  i: integer;
  PP: TPoint2Df;
begin
  if (High(Points)<1) then Exit;
  //WriteLine('newpath');
  PP := GCSToPSCoord(Points[0].X, Points[0].Y);
  WriteLine(Format('  %f %f m',[PP.X, PP.Y]));
  for i:=1 to High(Points) do
  begin
    PP := GCSToPSCoord(Points[i].X, Points[i].Y);
    WriteLine(Format('  %f %f l',[PP.X, PP.Y]));
  end;
  DefineBrushColorAlpha(FBrush);
end;

procedure TPostScriptCanvas.DrawPolygoneInscrit(const CSSClass: string; const N: integer; const X0, Y0, R: double);
begin

end;

procedure TPostScriptCanvas.DrawPolygonSansStyle(const CouleurLigne, CouleurRemplissage: TColor; const LigneWidth: double; const Points: TArrayPoints2Df; const Closed: boolean);
begin

end;

procedure TPostScriptCanvas.DrawPolygonOrPolyline(const CSSClass: string; const Points: TArrayPoints2Df; const Closed: boolean);
begin

end;

procedure TPostScriptCanvas.DrawText(const CSSClass: string; const Alignment: byte; const X, Y, Rotation: Double; const Text: string);
begin

end;

procedure TPostScriptCanvas.DrawImage(const FichierImage: string; const X, Y: double; const W, H: double);
begin

end;

function TPostScriptCanvas.SVGColor(const C: TColor): string;
begin

end;

function TPostScriptCanvas.CoordsGCSToSVG(const MyPt: TPoint2Df): TPoint2Df;
begin

end;

function TPostScriptCanvas.DrawRectangle(const QClassStyle: string; const X1, Y1, X2, Y2: double): boolean;
begin

end;

function TPostScriptCanvas.DrawSymbole(const IDSymbole: string; const X, Y, L, H, Rotate: double): boolean;
begin

end;

function TPostScriptCanvas.DrawBezierCurve(const QClassStyle: string; const BC: TArrayPoints2Df; const Closed: boolean): boolean;
begin

end;

procedure TPostScriptCanvas.DrawBorderedPolygon(const Points: array of TPoint2Df);
var
  i: integer;
  PP: TPoint2Df;
begin
  if (High(Points)<1) then Exit;
  WriteLine('newpath');
  PP := GCSToPSCoord(Points[0].X, Points[0].Y);
  WriteLine(Format('  %f %f m',[PP.X, PP.Y]));
  for i:=1 to High(Points) do
  begin
    PP := GCSToPSCoord(Points[i].X, Points[i].Y);
    WriteLine(Format('  %f %f l',[PP.X, PP.Y]));
  end;
  WriteLine('gsave');
  DefineBrushColorAlpha(FBrush);

  WriteLine('grestore');
  DefinePenColorWidth(FPen);

  WriteLine('closepath');
  WriteLine('stroke');
end;

procedure TPostScriptCanvas.TextOut(const X,Y: Double; const Text: string);
var
  PP: TPoint2Df;
begin
  PP := GCSToPSCoord(X, Y);
  WriteLine(Format('%f %f moveto (%s) show',[PP.X,PP.Y,Text]));
end;
procedure TPostScriptCanvas.DefineColor(const C: TColor); inline;
begin
  WriteLine(Format('%f %f %f srgb',[GetFloatRValue(C),
                                    GetFloatGValue(C),
                                    GetFloatBValue(C)]));
end;
procedure TPostScriptCanvas.DefinePenColorWidth(const P: TPenPSProperties);
begin
  DefineColor(P.Color);
  WriteLine(Format('%f setlinewidth',[P.fWidth]));
end;
procedure TPostScriptCanvas.DefineBrushColorAlpha(const B: TBrushPSProperties);
begin
  DefineColor(B.Color);
  WriteLine('fill');
end;

end.

