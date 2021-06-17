unit UnitTGHTopoDrawDrawingContext;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees
  , ToporobotClasses2012
  , UnitEntitesExtended
  , unitCroquisTerrain
  //{$IFDEF GHTOPO_SIMPLIFIE}
  , UnitGraphes1, BZGraphesTypes, BZGraphesClasses
  //{$ENDIF GHTOPO_SIMPLIFIE}
  , UnitClasseMaillage
  , Common
  , Graphics
  , sysutils
  , math
  , types
  , LCLType
  , BGRABitmap
  , BGRABitmapTypes
  , BGRAGradients
  //, unitUtilsComposants
  ;
type

{ TGHTopoDrawingContext }

 TGHTopoDrawingContext = class(TBGRABitmap)
  strict private
    // limites du dessin
    FRegionDeDessin: TRect2Df;
    // paramètres internes pour le calcul des coordonnées
    FRappScrReal    : double;
    FInvRappScrReal : double;
    // sauvegarde des pinceaux et crayons
    FOldPenStyle    : TPenStyle;
    FOldPenWidth    : byte;
    FOldPenColor    : TColor;
    FOldPenOpacity  : byte;

    FOldBrushStyle  : TBrushStyle;
    FOldBrushColor  : TColor;
    FOldBrushOpacity: byte;
    // fontes
    FOldFontName    : string;
    FOldFontColor   : TColor;
    FOldFontStyle   : TFontStyles;
    FOldFontHeight  : integer;

    // transformations de coordonnées

    function  QGetCoordsMonde(const PP: TPoint): TPoint2Df;
    function  QGetCoordsPlan(const PM: TPoint2Df): TPoint; overload;
    function  QGetCoordsPlan(const QX, QY: double): TPoint; overload;
    function  QGetCouleur(const QQE: TBaseStation): TColor;
    procedure DrawTexteCroquis(const X, Y: double; const Attr: TTexteAttributs; const Texte: string);
  private
    FVue2DParams           : TVue2DParams;            // éléments dessinés
    FDocuTopo              : TToporobotStructure2012;
    FBDDEntites            : TBDDEntites;
    FKrobard               : TCroquisTerrain;
    FMaillageMNT           : TMaillage;
    FDoHighLightCurrentItem: boolean;
    FModeSelectionListe    : TModeSelectionListe;
    procedure DrawRotatedTexte(const X, Y, Angle: integer; const Texte: string);
    procedure TraceVers(const X, Y: Double; const Drawn: boolean);
  public
    // initialisation du dessin
    function Initialiser(const FD: TToporobotStructure2012;
                         const FB: TBDDEntites;
                         const FK: TCroquisTerrain;
                         const FM: TMaillage;
                         const VP: TVue2DParams;
                         const ModeSelectionListe : TModeSelectionListe;
                         const DoHiglightSerie: boolean): boolean;
    // limites du dessin
    procedure SetBounds(const QX1, QY1, QX2, QY2: double);
    // paramètres de la vue
    //procedure SetVue2DParams(const P: TVue2DParams);
    // primitives de dessin
    procedure DrawAsterisk(const Centre: TPoint2Df; const Rayon, DemiEpaisseur: double; const FillColor: TColor);
    procedure DrawCenteredRectangle(const Centre: TPoint2Df; const HalfWidth, HalfHeight: double; const Angle: double);
    procedure DrawRectangle(const C1, C2: TPoint2Df; const Filled: boolean);
    procedure DrawTriangle(const P1, P2, P3: TPoint2Df; const Linecolor, FillColor: TColor; const LineOpacity, FillOpacity: byte; const WL: integer);
    procedure DrawShape(const x, y: Double; const TypeSymbole: byte; const L, H: integer);
    procedure DrawQuad(const P1, P2, P3, P4: TPoint2Df);
    // balises de début et fin de dessin
    procedure BeginDrawing();
    procedure EndDrawing();

    // gestion des brosses, crayons et fontes
    procedure DefineBrosse(const qStyle: TBrushStyle; const qColor: TColor; const qOpacity: byte);
    procedure DefineBrosseEtCrayon(const qBrushStyle: TBrushStyle; const qBrushColor: TColor; const qBrushOpacity: byte; const qPenStyle: TPenStyle; const qPenWidth: integer; const qPenColor: TColor; const qPenOpacity: byte);
    procedure DefineCrayon(const qStyle: TPenStyle; const qWidth: integer; const qColor: TColor; const qOpacity: byte);
    procedure DefineFonte(const QFontName: string; const QFontColor: TColor; const QFontStyle: TFontStyles; const QFontHeight: integer);
    procedure RestoreBrosse();
    procedure RestoreBrosseEtCrayon();
    procedure RestoreCrayon();
    procedure RestoreFonte();
    // dessin des objets
    procedure DrawQuadrillage(const QdrType: TQdrType; const QdrColor: TColor; const QdrSpacing: double);
    procedure DrawSegment(const X1, Y1, X2, Y2: Double);
    procedure DrawTexte(const X, Y: Double; const Alignement: integer; const T: string);
    // Dessin des objets GHTopo
    procedure DrawCadre();
    procedure DrawPolygonals(const DoDrawFast: boolean);
    procedure DrawSections();
    procedure DrawStations();
    procedure DrawAntennes();
    procedure DrawCotation(const DoDelta: boolean);
    procedure DrawIDStations();
    procedure DrawJonctions();
    procedure DrawPOIs();
    procedure DrawGaleries(const DoDrawFast: boolean);
    procedure DrawEntrancesOrPointEntities(const DoDrawNames: boolean);
    procedure DrawCroquisTerrain();
    procedure DrawCurrentStationTopo(const QCurrentStation: TBaseStation);
    procedure DrawMaillage(const QMaillageDisplayed: boolean; const QIsovaleur: double; const ContourLinesColor: TColor; const ContourLinesOpacity: byte);

    //{$IFDEF GHTOPO_SIMPLIFIE}
    procedure DrawOverlay();
    procedure DrawShortestPath(const FG: TPathFindingGraphe; const FP: TPathBetweenNodes);
    //{$ENDIF GHTOPO_SIMPLIFIE}



end;



implementation
uses
  DGCDummyUnit; // prévient le bug 'Fin du code source non trouvée'

function TGHTopoDrawingContext.Initialiser(const FD: TToporobotStructure2012;
                                           const FB: TBDDEntites;
                                           const FK: TCroquisTerrain;
                                           const FM: TMaillage;
                                           const VP: TVue2DParams;
                                           const ModeSelectionListe : TModeSelectionListe;
                                           const DoHiglightSerie: boolean): boolean;
begin
  result := false;
  try
    FVue2DParams  := VP;
    FDocuTopo     := FD;
    FBDDEntites   := FB;
    FKrobard      := FK;
    FMaillageMNT  := FM;
    FDoHighLightCurrentItem := DoHiglightSerie;
    FModeSelectionListe     := ModeSelectionListe;
    result        := True;
  finally

  end;
end;


// transformations de coordonnées
function TGHTopoDrawingContext.QGetCoordsPlan (const PM: TPoint2Df): TPoint;
begin
  Result.X := Round(( PM.X - FRegionDeDessin.X1) * FRappScrReal);
  Result.Y := Round((FRegionDeDessin.Y2 - PM.Y) * FRappScrReal);
end;
function TGHTopoDrawingContext.QGetCoordsPlan(const QX, QY: double): TPoint;
begin
  Result.X := Round(( QX - FRegionDeDessin.X1) * FRappScrReal);
  Result.Y := Round((FRegionDeDessin.Y2 - QY) * FRappScrReal);
end;

function TGHTopoDrawingContext.QGetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  Result.X:=  FInvRappScrReal * PP.X + FRegionDeDessin.X1;
  Result.Y:= -FInvRappScrReal * PP.Y + FRegionDeDessin.Y2 ;
end;

// limites du dessin
procedure TGHTopoDrawingContext.SetBounds(const QX1, QY1, QX2, QY2: double);
var
  RappHLVue: Extended;
  dx: Extended;
begin

  FRegionDeDessin.setfrom(QX1, QY1, QX2, QY2);
  dx := FRegionDeDessin.X2 - FRegionDeDessin.X1;
  // calcul du rapport Hauteur/largeur de vue
  RappHLVue       := Self.Height / self.Width;
  // calcul du rapport Ecran/Réel
  FRappScrReal    := self.Width / dx;
  FInvRappScrReal := 1 / FRappScrReal;
  // comme le repère est orthonormé, on doit recalculer le YMaxi
  FRegionDeDessin.Y2 := FRegionDeDessin.Y1 + dx * RappHLVue;
end;
// primitives de dessin
procedure TGHTopoDrawingContext.DrawCenteredRectangle(const Centre: TPoint2Df; const HalfWidth, HalfHeight: double; const Angle: double);
var
  AD, sa, ca: extended;
  PPP: array[0..3] of TPoint;
  function TransformCoin(const dx, dy: double): TPoint;
  var
    EWE: TPoint2Df;
  begin
    EWE.X := Centre.X + (ca * dX - sa * dY);
    EWE.Y := Centre.Y + (sa * dX + ca * dY);
    Result := QGetCoordsPlan(EWE);
  end;
begin
  AD := degtorad(Angle);
  SinCos(AD, sa, ca);
  PPP[0] := TransformCoin(-HalfWidth, -HalfHeight);
  PPP[1] := TransformCoin( HalfWidth, -HalfHeight);
  PPP[2] := TransformCoin( HalfWidth,  HalfHeight);
  PPP[3] := TransformCoin(-HalfWidth,  HalfHeight);
  self.CanvasBGRA.Polygon(PPP);
end;

procedure TGHTopoDrawingContext.DrawAsterisk(const Centre: TPoint2Df; const Rayon, DemiEpaisseur: double; const FillColor: TColor);
begin
  DefineBrosseEtCrayon(bsSolid, FillColor, 255, psSolid, 0, FillColor, 255);
    self.CanvasBGRA.Brush.Texture:= nil;
    DrawCenteredRectangle(Centre, Rayon, DemiEpaisseur, 0);
    DrawCenteredRectangle(Centre, Rayon, DemiEpaisseur, 60);
    DrawCenteredRectangle(Centre, Rayon, DemiEpaisseur, 120);
  RestoreBrosseEtCrayon();
end;

procedure TGHTopoDrawingContext.DrawRectangle(const C1, C2: TPoint2Df; const Filled: boolean);
var
  PP1, PP2: TPoint;
begin
  PP1 := QGetCoordsPlan(C1);
  PP2 := QGetCoordsPlan(C2);
  self.CanvasBGRA.Rectangle(PP1.X, PP1.Y, PP2.X, PP2.Y, Filled);
end;

// dessin d'un triangle
// très utilisé => procédure distincte
procedure TGHTopoDrawingContext.DrawTriangle(const P1, P2, P3: TPoint2Df;
                                             const Linecolor, FillColor: TColor;
                                             const LineOpacity, FillOpacity: byte;
                                             const WL: integer);
var
  Sommets: array[0..2] of TPoint;
begin
  Sommets[0] := QGetCoordsPlan(P1);
  Sommets[1] := QGetCoordsPlan(P2);
  Sommets[2] := QGetCoordsPlan(P3);
  self.DefineBrosseEtCrayon(bsSolid, FillColor, FillOpacity, psSolid, WL, Linecolor, LineOpacity);
    self.CanvasBGRA.Brush.Texture:= nil;
    self.CanvasBGRA.Polygon(Sommets);
  self.RestoreBrosseEtCrayon();
end;

// déplacement de la plume
procedure TGHTopoDrawingContext.TraceVers(const X, Y: Double; const Drawn: boolean);
var
  PP: TPoint;
begin
  PP := QGetCoordsPlan(X, Y);
  if (Drawn) then self.CanvasBGRA.LineTo(PP.X, PP.Y)
             else self.CanvasBGRA.MoveTo(PP.X, PP.Y);

end;



procedure TGHTopoDrawingContext.DrawSegment(const X1, Y1, X2, Y2: Double);
begin
  if (SegmentInRectangle(X1, Y1, X2, Y2, FRegionDeDessin, True)) then
  begin
    TraceVers(X1, Y1, false);
    TraceVers(X2, Y2, True);
  end;
end;
//******************************************************************************
// dessin d'un texte
procedure TGHTopoDrawingContext.DrawTexte(const X, Y: Double; const Alignement: integer; const T: string);
var
  PP, PosTXT: TPoint;
  ExTxt: TSize;
  PM : TPoint2Df;
begin
  PM.setFrom(X, Y);
  if (not PointInRectangle(PM, FRegionDeDessin)) then exit;
  PP := QGetCoordsPlan(PM);
  ExTxt :=  self.CanvasBGRA.TextExtent(T);
  // 7 8 9
  // 4 5 6
  // 1 2 3
  //   0
  case Alignement of
    0,1: PosTXT := MakeTPoint(PP.X, PP.Y - ExTxt.cy);
    2  : PosTXT := MakeTPoint(PP.X - ExTxt.cx div 2, PP.Y - ExTxt.cy);
    3  : PosTXT := MakeTPoint(PP.X - ExTxt.cx, PP.Y - ExTxt.cy);
    4  : PosTXT := MakeTPoint(PP.X, PP.Y - ExTxt.cy div 2);
    5  : PosTXT := MakeTPoint(PP.X - ExTxt.cx div 2, PP.Y - ExTxt.cy div 2);
    6  : PosTXT := MakeTPoint(PP.X - ExTxt.cx, PP.Y - ExTxt.cy div 2);
    7  : PosTXT := MakeTPoint(PP.X, PP.Y);
    8  : PosTXT := MakeTPoint(PP.X - ExTxt.cx div 2, PP.Y);
    9  : PosTXT := MakeTPoint(PP.X - ExTxt.cx, PP.Y);
  else
    PosTXT := MakeTPoint(PP.X, PP.Y - ExTxt.cy);
  end;
  self.CanvasBGRA.TextOut(PosTXT.X, PosTXT.Y, T);
end;
// Le TCanvas de Lazarus possède un paramètre 'Orientation' (absent du TCanvas de Delphi 5)
// qui n'est rien d'autre que l'angle de rotation en 1/10 de degré
// 3 instructions toutes simples LOL !
procedure TGHTopoDrawingContext.DrawRotatedTexte(const X, Y, Angle: integer; const Texte: string);
begin
  self.CanvasBGRA.Font.Orientation := Angle * 10; // angle en 1/10e de degré
  self.CanvasBGRA.TextOut(X, Y, Texte);
  self.CanvasBGRA.Font.Orientation := 0; // penser à remettre à l'horizontale XD
end;

procedure TGHTopoDrawingContext.DrawShape(const x, y: Double; const TypeSymbole: byte; const L, H: integer);
var
  PP: TPoint;
  dl: Integer;
  dh: Integer;
begin
  if (PointInRectangle(x, y, FRegionDeDessin)) then
  begin
    PP := QGetCoordsPlan(X, Y);
    dl := L shr 1;
    dh := H shr 1;
    self.CanvasBGRA.Brush.Texture := nil;
    case TypeSymbole of
      0: self.CanvasBGRA.Rectangle(PP.X-dl,PP.Y-dh,PP.X+dh+1,PP.Y+dh+1);
      1: self.CanvasBGRA.EllipseC(PP.X, PP.Y, dl+1, dh+1);
    end;
  end;
end;
procedure TGHTopoDrawingContext.DrawQuad(const P1, P2, P3, P4: TPoint2Df);
var
  PP: array[0..3] of TPoint;
begin
  if (PointInRectangle(P1, FRegionDeDessin) OR
      PointInRectangle(P2, FRegionDeDessin) OR
      PointInRectangle(P3, FRegionDeDessin) OR
      PointInRectangle(P4, FRegionDeDessin)) then
  begin
    PP[0] := QGetCoordsPlan(P1);
    PP[1] := QGetCoordsPlan(P2);
    PP[2] := QGetCoordsPlan(P3);
    PP[3] := QGetCoordsPlan(P4);
    self.CanvasBGRA.Polygon(PP);
  end;
end;

//******************************************************************************
procedure TGHTopoDrawingContext.BeginDrawing();
begin
  // crayon, brosse et fontes par défaut
  self.CanvasBGRA.Brush.Texture := nil;
  self.CanvasBGRA.Brush.Style := bsSolid;
  self.CanvasBGRA.Brush.Color := clWhite;
  self.CanvasBGRA.Pen.Style   := psSolid;
  self.CanvasBGRA.Pen.Color   := clBlack;
  self.CanvasBGRA.Pen.Width   := 0;
  self.CanvasBGRA.Font.Color  := clBlack;
  self.CanvasBGRA.Font.Style  := [];
  self.CanvasBGRA.Font.Height := 8;
end;



procedure TGHTopoDrawingContext.EndDrawing();
begin
  //DrawPipistrelle(self);
end;
// définir crayons, brosse et fontes
procedure TGHTopoDrawingContext.DefineCrayon(const qStyle: TPenStyle;
                                             const qWidth: integer;
                                             const qColor: TColor;
                                             const qOpacity: byte);
begin
  FOldPenStyle    := self.CanvasBGRA.Pen.Style;
  FOldPenColor    := self.CanvasBGRA.Pen.Color;
  FOldPenOpacity  := self.CanvasBGRA.Pen.Opacity;
  FOldPenWidth    := self.CanvasBGRA.Pen.Width;
  self.CanvasBGRA.Pen.Style   := qStyle;
  self.CanvasBGRA.Pen.Width   := qWidth;
  self.CanvasBGRA.Pen.Color   := qColor;
  self.CanvasBGRA.Pen.Opacity := qOpacity;
end;
procedure TGHTopoDrawingContext.DefineFonte(const QFontName: string; const QFontColor: TColor; const QFontStyle: TFontStyles; const QFontHeight: integer);
begin
  // sauvegarde de l'ancienne fonte (utiliser RestoreFont() pour la rappeler)
  FOldFontName   := self.CanvasBGRA.Font.Name;
  FOldFontStyle  := self.CanvasBGRA.Font.Style;
  FOldFontColor  := self.CanvasBGRA.Font.Color;
  FOldFontHeight := self.CanvasBGRA.Font.Height;

  self.CanvasBGRA.Font.Name    := QFontName;
  self.CanvasBGRA.Font.Color   := QFontColor;
  self.CanvasBGRA.Font.Style   := QFontStyle;
  self.CanvasBGRA.Font.Height  := QFontHeight; // TS.FontHeight;
  self.CanvasBGRA.Font.Opacity := 255;

end;


procedure TGHTopoDrawingContext.DefineBrosse(const qStyle: TBrushStyle;
                                             const qColor: TColor;
                                             const qOpacity: byte);
begin
  FOldBrushStyle    := self.CanvasBGRA.Brush.Style;
  FOldBrushColor    := self.CanvasBGRA.Brush.Color;
  FOldBrushOpacity  := self.CanvasBGRA.Brush.Opacity;
  self.CanvasBGRA.Brush.Texture := nil;
  self.CanvasBGRA.Brush.Style   := qStyle;
  self.CanvasBGRA.Brush.Color   := qColor;
  self.CanvasBGRA.Brush.Opacity := qOpacity;
end;

procedure TGHTopoDrawingContext.DefineBrosseEtCrayon(const qBrushStyle: TBrushStyle; const qBrushColor: TColor; const qBrushOpacity: byte;
                                                     const qPenStyle  : TPenStyle; const qPenWidth: integer; const qPenColor: TColor; const qPenOpacity: byte);
begin
  DefineBrosse(qBrushStyle, qBrushColor, qBrushOpacity);
  DefineCrayon(qPenStyle, qPenWidth, qPenColor, qPenOpacity);
end;
procedure TGHTopoDrawingContext.RestoreBrosse();
begin
  self.CanvasBGRA.Brush.Style    := FOldBrushStyle;
  self.CanvasBGRA.Brush.Color    := FOldBrushColor;
  self.CanvasBGRA.Brush.Opacity  := FOldBrushOpacity;
  //self.CanvasBGRA.Brush.Texture  := FOldBrushTexture;
 end;

procedure TGHTopoDrawingContext.RestoreFonte();
begin
  self.CanvasBGRA.Font.Name      := FOldFontName;
  self.CanvasBGRA.Font.Color     := FOldFontColor;
  self.CanvasBGRA.Font.Style     := FOldFontStyle;
  self.CanvasBGRA.Font.Height    := FOldFontHeight;
  self.CanvasBGRA.Font.Opacity   := 255;        // pour mémoire (non utilisé)
end;

procedure TGHTopoDrawingContext.RestoreCrayon();
begin
  self.CanvasBGRA.Pen.Style      := FOldPenStyle;
  self.CanvasBGRA.Pen.Width      := FOldPenWidth;
  self.CanvasBGRA.Pen.Color      := FOldPenColor;
  self.CanvasBGRA.Pen.Opacity    := FOldPenOpacity;
end;

procedure TGHTopoDrawingContext.RestoreBrosseEtCrayon();
begin
  RestoreBrosse();
  RestoreCrayon();
end;
//******************************************************************************
procedure TGHTopoDrawingContext.DrawQuadrillage(const QdrType: TQdrType; const QdrColor: TColor; const QdrSpacing: double);
var
  tt: Int64;
  A: Double;
  S: String;
begin
  AfficherMessage(format('DrawQuadrillage: %f %f, %f %f, %f', [FRegionDeDessin.X1, FRegionDeDessin.Y1, FRegionDeDessin.X2, FRegionDeDessin.Y2, QdrSpacing]));
  DefineCrayon(psSolid, 0, QdrColor, 255);
  DefineFonte('Arial', clBlack, [], FONT_HEIGHT_QUADRILLES);

  tt := trunc(FRegionDeDessin.X1 / QdrSpacing);
  A  := QdrSpacing * tt;
  while (A < FRegionDeDessin.X2) do
  begin
    DrawSegment(A, FRegionDeDessin.Y1, A, FRegionDeDessin.Y2);
    S := Format(FORMAT_NB_REAL_0_DEC,[A]);
    DrawTexte(A, FRegionDeDessin.Y2, 8, S);
    A := A + QdrSpacing;
  end;
  tt := trunc(FRegionDeDessin.Y1 / QdrSpacing);
  A  := QdrSpacing * tt;
  while (A < FRegionDeDessin.Y2) do
  begin
    DrawSegment(FRegionDeDessin.X1, A, FRegionDeDessin.X2, A);
    S := Format(FORMAT_NB_REAL_0_DEC,[A]);
    DrawTexte(FRegionDeDessin.X1, A, 4, S);
    A := A + QdrSpacing;
  end;
  RestoreCrayon();
end;

//******************************************************************************
//******************************************************************************
function TGHTopoDrawingContext.QGetCouleur(const QQE: TBaseStation): TColor;
begin
  case FVue2DParams.ongModeRepresentation of
    rgENTRANCES    : Result := FBDDEntites.GetCouleurEntiteRGBByEntrance(QQE);
    rgRESEAUX      : Result := FBDDEntites.GetCouleurEntiteRGBByReseau(QQE);
    rgSECTEURS     : Result := FBDDEntites.GetCouleurEntiteRGBBySecteur(QQE);
    rgSEANCES      : Result := FBDDEntites.GetCouleurEntiteRGBByExpe(QQE);
    rgDEPTH        : Result := FBDDEntites.CalcColorDegradeByAltitude(QQE.PosStation.Z);
    rgTYPES_VISEES : Result := ChooseColorByTypeEntite(QQE.Type_Entite);
  else
    Result := clGray;
  end;
end;



procedure TGHTopoDrawingContext.DrawPolygonals(const DoDrawFast: boolean);
var
  i, NbE, CN    : integer;
  QNoSerie      : integer;
  QIdxNamespace : integer;
  E             : TBaseStation;
  CA            : TNumeroEntrance;
  CS            : TNumeroSerie;
  CC            : TNumeroCode;
  CE            : TNumeroExpe;
  CR            : TNumeroReseau;
  CZ            : TNumeroSecteur;
  procedure MiouMiou(const Miou: TBaseStation);
  begin
    DefineCrayon(psSolid, 8, clRed, 255);
    DrawSegment(Miou.PosExtr0.X, Miou.PosExtr0.Y, Miou.PosStation.X, Miou.PosStation.Y);
    DefineCrayon(psSolid, 6, clBlue, 192);
    DrawSegment(Miou.PosExtr0.X, Miou.PosExtr0.Y, Miou.PosStation.X, Miou.PosStation.Y);
  end;
begin
  CA := FDocuTopo.GetCurrentNumeroEntrance();
  CS := FDocuTopo.GetCurrentNumeroSerie();
  CC := FDocuTopo.GetCurrentNumeroCode();
  CE := FDocuTopo.GetCurrentNumeroExpe();
  CR := FDocuTopo.GetCurrentNumeroReseau();
  CZ := FDocuTopo.GetCurrentNumeroSecteur();
  CN := FDocuTopo.GetCurrentNumeroNamespace();
  AfficherMessage(' --> DrawPolygonals: ' + inttostr(CN));
  NbE := FBDDEntites.GetNbEntitesVisees();
  if (DoDrawFast) then
  begin
    DefineCrayon(psSolid, 1, clRed, 255);
    for i := 0 to NbE - 1 do   // TODO: A vérifier
    begin
      E := FBDDEntites.GetEntiteVisee(i);
      DrawSegment(E.PosExtr0.X, E.PosExtr0.Y, E.PosStation.X, E.PosStation.Y);
    end;
  end else
  begin
    for i := 0 to NbE - 1 do   // TODO: A vérifier
    begin
      //try
        E := FBDDEntites.GetEntiteVisee(i);
        if (E.Enabled) then
        begin
          DefineCrayon(psSolid, IIF(E.Highlighted, 5, FVue2DParams.ongViseesLargeurInPX), QGetCouleur(E), 255);
          DrawSegment(E.PosExtr0.X, E.PosExtr0.Y, E.PosStation.X, E.PosStation.Y);
        end
        else
        begin
          if (FVue2DParams.ongDoDispViseesNonRetenues) then
          begin
            DefineCrayon(psSolid, FVue2DParams.ongViseesLargeurInPX, FVue2DParams.ongCouleurViseesNonRetenues, 255);
            DrawSegment(E.PosExtr0.X, E.PosExtr0.Y, E.PosStation.X, E.PosStation.Y);
          end;
        end;
        if (FDoHighLightCurrentItem) then
        begin
          DecomposeNumeroSerie(E.Entite_Serie, QIdxNamespace, QNoSerie);
          case FModeSelectionListe of
            mslENTRANCES         : if (E.eEntrance    = CA) then MiouMiou(E);
            mslRESEAUX           : if (E.eReseau      = CR) then MiouMiou(E);
            mslSECTEURS          : if (E.eSecteur     = CZ) then MiouMiou(E);
            mslCODE              : if (E.eCode        = CC) then MiouMiou(E);
            mslEXPE              : if (E.eExpe        = CE) then MiouMiou(E);
            mslSERIE             : if (QNoSerie       = CS) then MiouMiou(E);
            mslTYPE_GALERIE      : pass; //if (E.Entite_Serie = CS) then MiouMiou(E);
            mslDATE              : pass; //if (E.Entite_Serie = CS) then MiouMiou(E);
            mslNAMESPACES        : if (QIdxNamespace  = CN) then MiouMiou(E);
          end;
        end;
      //except
      //end;
    end;
  end;
end;
procedure TGHTopoDrawingContext.DrawSections();
var
  i:      integer;
  E:      TBaseStation;
begin
  DefineCrayon(psSolid, 0, clSilver, 255);
  for i := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do
  begin
    E := FBDDEntites.GetEntiteVisee(i);
    if (E.Enabled) then DrawSegment(E.PosPD.X, E.PosPD.Y, E.PosPG.X, E.PosPG.Y);
  end;
  RestoreCrayon();
end;
procedure TGHTopoDrawingContext.DrawStations();
var
  i:  integer;
  E:  TBaseStation;
  R : integer;
begin
  R := 2;
  DefineBrosseEtCrayon(bsSolid, clRed, 128, psSolid, 0, clRed, 255);
  for i := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do
  begin
    E := FBDDEntites.GetEntiteVisee(i);
    if (not E.Enabled) then  Continue;
    if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;
    DrawShape(E.PosStation.X, E.PosStation.Y, 1, R, R);
  end;
  RestoreBrosseEtCrayon();
end;
procedure TGHTopoDrawingContext.DrawAntennes();
var
  i: Integer;
  E: TBaseStation;
begin
  DefineCrayon(psSolid, 0, clSilver, 255);
  for i := 0 to FBDDEntites.GetNbEntitesAntennes() - 1 do
  begin
    E := FBDDEntites.GetEntiteAntenne(i);
    if (E.Enabled) then DrawSegment(E.PosExtr0.X, E.PosExtr0.Y, E.PosStation.X, E.PosStation.Y);
  end;
  RestoreCrayon();
end;
procedure TGHTopoDrawingContext.DrawIDStations();
var
  i:  integer;
  E:  TBaseStation;
begin
  DefineBrosseEtCrayon(bsSolid, FVue2DParams.ongBackGround, 128, psSolid, 0, clBlack, 255);
  DefineFonte(DEFAULT_FONT_NAME, clMaroon, [fsBold], FONT_HEIGHT_IDSTATIONS);
  for i := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do
  begin
    try
      E := FBDDEntites.GetEntiteVisee(i);
      if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;
      if (not E.Enabled) then Continue;
      DrawTexte(E.PosStation.X + 0.3, E.PosStation.Y + 0.3, 1, E.toString());
    except
    end;
  end;
  RestoreBrosseEtCrayon();
  RestoreFonte();
end;
procedure TGHTopoDrawingContext.DrawCotation(const DoDelta: boolean);
var
  i:  integer;
  E:  TBaseStation;
  QQ: TPoint3Df;
  WU: Double;
begin
  DefineBrosseEtCrayon(bsSolid, FVue2DParams.ongBackGround, 128, psSolid, 0, clBlack, 255);
  DefineFonte(DEFAULT_FONT_NAME, clBlue, [fsItalic], FONT_HEIGHT_COTATION);
  for i := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do
  begin
    try
      E := FBDDEntites.GetEntiteVisee(i);
      if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;
      if (not E.Enabled) then Continue;

      QQ := FBDDEntites.GetDeltaXYZFromPositionSt0(E);
      WU := IIF(DoDelta, QQ.Z, E.PosStation.Z);
      DrawTexte(E.PosStation.X - 0.3, E.PosStation.Y - 0.3, 7, Format('%.1f', [WU]));
    except
    end;
  end;
  RestoreBrosseEtCrayon();
  RestoreFonte();
end;
procedure TGHTopoDrawingContext.DrawJonctions();
const
  R = 3;
var
  n, i: Integer;
  J : TJonctionXYZ;
begin
  n := FBDDEntites.GetNbJonctions();
  if (n = 0) then exit;
  DefineBrosseEtCrayon(bsSolid, FVue2DParams.ongBackGround, 128, psSolid, 0, clBlack, 255);
  DefineFonte(DEFAULT_FONT_NAME, clMaroon, CanvasBGRA.Font.Style + [fsBold], FONT_HEIGHT_NOEUDS);
  for i := 0 to n - 1 do
  begin
    J  := FBDDEntites.GetJonction(i);
    DrawShape(J.Position.X, J.Position.Y, 1, R, R);
    DrawTexte(J.Position.X - 0.3, J.Position.Y - 0.3, 7, MakeLabelNoeud(J));
  end;
  RestoreBrosseEtCrayon();
  RestoreFonte();
end;
procedure TGHTopoDrawingContext.DrawPOIs();
var
  i:  integer;
  E:  TBaseStation;
  R : integer;
begin
  R := 4;
  DefineFonte('Arial', clBlue, [fsBold], 16);
  for i := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do
  begin
    try
      E := FBDDEntites.GetEntiteVisee(i);
      if (not E.Enabled) then  Continue;
      if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;
      if (E.IsPOI) then
      begin
        DefineBrosseEtCrayon(bsSolid, clBlue, 255, psSolid, 0, clBlue, 255);
        DrawShape(E.PosStation.X, E.PosStation.Y, 1, R shl 1, R shl 1);
        RestoreBrosseEtCrayon();
        DefineBrosseEtCrayon(bsSolid, clAqua, 255, psSolid, 0, clAqua, 255);
        DrawShape(E.PosStation.X, E.PosStation.Y, 1, R, R);
        RestoreBrosseEtCrayon();
        DrawTexte(E.PosStation.X + 1.0, E.PosStation.Y + 1.0, 1, E.toString());
      end;
    except
    end;
  end;
  RestoreFonte();
end;
procedure TGHTopoDrawingContext.DrawGaleries(const DoDrawFast: boolean);
var
  E:      TBaseStation;
  i, QNb:      integer;
  q1, q2: boolean;
  QColorQuad: TColor;
  S1, S2, S3, S4: TPoint2Df;
begin
  q1 := (edFillGalerie in FVue2DParams.ongElementsDrawn);
  q2 := (edWalls       in FVue2DParams.ongElementsDrawn);
  QNb := FBDDEntites.GetNbEntitesVisees();
  if (QNb = 0) then Exit;
  if (DoDrawFast) then
  begin
    DefineBrosseEtCrayon(bsSolid, clBlue, FVue2DParams.ongFillOpacite, psClear, 0, clBlue, FVue2DParams.ongFillOpacite);
    for i := 0 to QNb - 1 do
    begin
      E := FBDDEntites.GetEntiteVisee(i);
      if (E.Type_Entite in [tgSURFACE, tgENTRANCE])  then Continue;       // topo de surface et cheminement spéciaux: sans objet
      if (q1) then
      begin // remplissages
        S1.setFrom(E.PosOPG.X, E.PosOPG.Y);
        S2.setFrom(E.PosOPD.X, E.PosOPD.Y);
        S3.setFrom(E.PosPD.X , E.PosPD.Y);
        S4.setFrom(E.PosPG.x , E.PosPG.Y);
        DrawQuad(S1, S2, S3, S4);
      end;
      if (q2) then
      begin // parois
        DrawSegment(E.PosOPG.X, E.PosOPG.Y, E.PosPG.X, E.PosPG.Y);
        DrawSegment(E.PosOPD.X, E.PosOPD.Y, E.PosPD.X, E.PosPD.Y);
      end;
    end;
  end
  else
  begin
    for i := 0 to QNb - 1 do
    begin
      E := FBDDEntites.GetEntiteVisee(i);
      if (E.Type_Entite in [tgSURFACE, tgENTRANCE])  then Continue;       // topo de surface et cheminement spéciaux: sans objet
      if (not E.Enabled) then  Continue;                                  // toutes visées hors topo de surface
      QColorQuad := QGetCouleur(E);
      if (q1) then
      begin // remplissages
        DefineBrosseEtCrayon(bsSolid, QColorQuad, FVue2DParams.ongFillOpacite, psClear, 0, QColorQuad, FVue2DParams.ongFillOpacite);
        S1.setFrom(E.PosOPG.X, E.PosOPG.Y);
        S2.setFrom(E.PosOPD.X, E.PosOPD.Y);
        S3.setFrom(E.PosPD.X , E.PosPD.Y);
        S4.setFrom(E.PosPG.x , E.PosPG.Y);
        DrawQuad(S1, S2, S3, S4);
        RestoreBrosseEtCrayon();
      end;
      if (q2) then
      begin // parois
        DefineBrosseEtCrayon(bsSolid, QColorQuad, FVue2DParams.ongFillOpacite, psSolid, 0, QColorQuad, (FVue2DParams.ongFillOpacite + 64) AND 255);
        DrawSegment(E.PosOPG.X, E.PosOPG.Y, E.PosPG.X, E.PosPG.Y);
        DrawSegment(E.PosOPD.X, E.PosOPD.Y, E.PosPD.X, E.PosPD.Y);
        RestoreBrosseEtCrayon();
      end;
    end;
  end;
end;
procedure TGHTopoDrawingContext.DrawEntrancesOrPointEntities(const DoDrawNames: boolean);
const
  R666 = 4;
var
  i, n:      integer;
  E:      TEntrance;
  R777: Int64;
begin
  n := FBDDEntites.GetNbEntrances();
  if (n = 0) then exit;
  if (DoDrawNames) then DefineFonte(DEFAULT_FONT_NAME, clBlue, [fsBold], FONT_HEIGHT_ENTRANCES);
  DefineBrosseEtCrayon(bsSolid, clFuchsia, 128, psSolid, 0, clBlue, 255);
  R777 := Trunc(1.15 * R666);
  for i := 0 to n - 1 do
  begin
    E := FBDDEntites.GetEntrance(i);
    DefineBrosse(bsSolid, clFuchsia, 64);
    DrawShape(E.ePosition.X, E.ePosition.Y, 1, R666, R666);
    DefineBrosse(bsSolid, FVue2DParams.ongBackGround, 64);
    if (DoDrawNames) then DrawTexte(E.ePosition.X, E.ePosition.Y, 1, GetResourceString(E.eNomEntree));
  end; // for
  DefineBrosseEtCrayon(bsSolid, FVue2DParams.ongBackGround, 128, psSolid, 0, clBlack, 255);
  RestoreFonte();
end;
// cadre périmétrique
procedure TGHTopoDrawingContext.DrawCadre();
var
  C1, C2: TPoint3Df;
  P1, P2: TPoint2Df;
begin
  DefineBrosseEtCrayon(bsClear, FVue2DParams.ongBackGround, 0, psSolid, 0, clRed, 255);
  C1 := FBDDEntites.GetCoinBasGauche();
  C2 := FBDDEntites.GetCoinHautDroit();
  P1.setFrom(C1.X, C1.Y);
  P2.setFrom(C2.X, C2.Y);
  DrawRectangle(P1, P2, false);
end;
//******************************************************************************
// Krobard
procedure TGHTopoDrawingContext.DrawTexteCroquis(const X, Y: double; const Attr: TTexteAttributs; const Texte: string);
begin
  DefineFonte(Attr.FontName, Attr.FontColor, [fsBold], 16);
  CanvasBGRA.Font.Orientation := Attr.AngleRot * 100;
    DrawTexte(X, Y, Attr.Alignement, Texte);
  CanvasBGRA.Font.Orientation := 0;
  RestoreFonte();
end;
procedure TGHTopoDrawingContext.DrawCroquisTerrain();
var
  NbAnnotations, NbPolys, IdxAnn, IdxPoly: Integer;
  procedure TracerAnnotation(const MyAnnotation: TKrobardAnnotation);
  var
    QStyleAnn: TTexteAttributs;
    PM: TPoint3Df;
    // TODO: A factoriser
    function InterpreterAnnotation(): string;
    var
      P1, EWE: TPoint3Df;
      aSerie, aStation: TIDBaseStation;
    begin
      Result   := MyAnnotation.Texte;
      aSerie   := MyAnnotation.Position.IDBaseStation div NB_MAXI_SERIES_PAR_CAVITE;
      aStation := (MyAnnotation.Position.IDBaseStation mod NB_MAXI_SERIES_PAR_CAVITE) div 10;
      P1.Empty();
      FBDDEntites.CalcCoordinatesFromBasePtAndOffset(MyAnnotation.Position.IDBaseStation, P1, EWE);
      Result := StringReplace(Result, '%z', Format('Alt %.2f', [EWE.Z]), [rfIgnoreCase, rfReplaceAll]);
      Result := StringReplace(Result, FORMAT_STRING, Format(FMTSERST, [aSerie, aStation]), [rfIgnoreCase, rfReplaceAll]);
    end;
  begin
    try

      if (FBDDEntites.CalcCoordinatesFromBasePtAndOffset(MyAnnotation.Position.IDBaseStation, MyAnnotation.Position.Offset, PM)) then
      begin
        if (not PointInRectangle(PM.X, PM.Y, FRegionDeDessin)) then Exit;
        QStyleAnn := FKrobard.GetStyleAnnotation(MyAnnotation.IDStyle);
        DrawTexteCroquis(PM.X, PM.Y, QStyleAnn, InterpreterAnnotation());
      end;
    except
      pass;
    end;
  end;
  procedure TracerPoly(const MyPoly: TKrobardPolyligne; const DoDrawBoundingBox: boolean = false);
  var
    QStylePoly: TKrobardStylePolyligne;
    Nb, i: Integer;
    PV: TKrobardPolyVertex;
    QPolygon: array of TPoint;
    PM: TPoint3Df;
    P1, P2: TPoint2Df;
  begin
    // TODO:
    // Bug inexplicable et exaspérant: Si on ne met pas un AfficherMessage() de la forme
    // AfficherMessageErreur(format('Draw courbe (%d)', [Nb])): le tracé est OK
    // AfficherMessageErreur(format('Draw courbe (%d)', [0])) : le tracé est OK
    // AfficherMessageErreur('toto'); : Pas de tracé
    // Degré de frustration sur l'échelle de Néron: 32 ( = 120 chrétiens égorgés lors des jeux du cirque )
    AfficherMessageErreur(format('Draw poly (%d)', [Nb]));  // Indispensable, sinon le tracé de la courbe ne se fait pas !!!
    Nb := Length(MyPoly.Sommets);
    if (Nb < 2) then Exit;
    QStylePoly := FKrobard.GetStylePolyligne(MyPoly.IDStyle);
    // rejeter les objets dégénérés ou hors fenêtre de vue
    if (not IntersectRectangles(MyPoly.BoundingBox, FRegionDeDessin)) then Exit;
    SetLength(QPolygon, Nb);
    try  // indispensable pour Windows 10
      if (DoDrawBoundingBox) then  // bounding box
      begin
        DefineCrayon(psDash, 0, clGray, 255);
        P1.setFrom(MyPoly.BoundingBox.X1, MyPoly.BoundingBox.Y1);
        P2.setFrom(MyPoly.BoundingBox.X2, MyPoly.BoundingBox.Y2);
        DrawRectangle(P1, P2, false);
      end;
      DefineBrosseEtCrayon(bsSolid, QStylePoly.FillColor, QStylePoly.FillOpacity, QStylePoly.LineStyle, QStylePoly.LineWidth, QStylePoly.LineColor, QStylePoly.LineOpacity);
      for i := 0 to Nb - 1 do
      begin
        PV := MyPoly.Sommets[i];
        if (FBDDEntites.CalcCoordinatesFromBasePtAndOffset(PV.IDBaseStation, PV.Offset, PM)) then QPolygon[i] := QGetCoordsPlan(PM.X, PM.Y);
      end;
      if (QStylePoly.Closed) then CanvasBGRA.Polygon(QPolygon) else CanvasBGRA.Polyline(QPolygon);

    except
      AfficherMessageErreur('**** ERREUR TRACE POLY ***');
      pass; //on E: Exception do ShowMessage(E.Message);
    end;
  end;
begin
  NbAnnotations := FKrobard.GetNbAnnotations();
  NbPolys       := FKrobard.GetNbPolylines();
  AfficherMessage(Format('*** DrawCroquis(): %d; %d styles ; %d ann. %d poly',
                        [FKrobard.GetNbStylesAnnotations(), FKrobard.GetNbStylesPolyLines(),
                         NbAnnotations, NbPolys]));
  if (not FKrobard.IsReady) then Exit;
  if (NbAnnotations > 0) then
  begin
    CanvasBGRA.Brush.Color := FVue2DParams.ongBackGround;
    for IdxAnn := 0 to NbAnnotations - 1 do TracerAnnotation(FKrobard.GetAnnotation(IdxAnn));
  end;
  if (NbPolys > 0) then
  begin
    for IdxPoly := 0 to NbPolys - 1 do TracerPoly(FKrobard.GetPolyline(IdxPoly));
  end;
end;

procedure TGHTopoDrawingContext.DrawCurrentStationTopo(const QCurrentStation: TBaseStation);
const
  DEMI_COTE = 6;
  DECALAGE_TEXTE_IN_M = 0.30;
var
  PP: TPoint;
  PM: TPoint2Df;
  QTxt: String;
begin
  try
    PM.setFrom(QCurrentStation.PosStation.X, QCurrentStation.PosStation.Y);
    if (PointInRectangle(PM, FRegionDeDessin)) then
    begin
      DefineBrosseEtCrayon(bsSolid, clYellow, 128, psSolid, 0, clAqua, 255);
       DefineFonte(DEFAULT_FONT_NAME, clBlue, [fsBold, fsUnderline], FONT_HEIGHT_ENTRANCES);
        DrawShape(PM.X, PM.Y, 1, DEMI_COTE, DEMI_COTE);
        QTxt  := MakeLibelleStationTopo(QCurrentStation);// Format('%d.%d - %s', [FCurrentEntite.Entite_Serie, FCurrentEntite.Entite_Station, FCurrentEntite.oIDLitteral]);
        DrawTexte(PM.X + DECALAGE_TEXTE_IN_M, PM.Y - DECALAGE_TEXTE_IN_M, 7, QTxt);
       RestoreFonte();
      RestoreBrosseEtCrayon();
    end;
  except
    pass;
  end;
end;
//******************************************************************************
procedure TGHTopoDrawingContext.DrawMaillage(const QMaillageDisplayed: boolean; const QIsovaleur: double; const ContourLinesColor: TColor; const ContourLinesOpacity: byte);
var
  Isovaleurs: TArrayOfFloats;
begin
  if (not assigned(FMaillageMNT)) then Exit;
  SetLength(Isovaleurs, 1);
  Isovaleurs[0] := QIsovaleur;
  //try
    if (FMaillageMNT.IsValidMaillage() and QMaillageDisplayed) then
    begin
      FMaillageMNT.TracerMaillage(self,
                                  QGetCoordsPlan,
                                  10.00,
                                  IsoValeurs,
                                  ContourLinesColor,
                                  ContourLinesOpacity,
                                  clFuchsia,
                                  false,
                                  false,
                                  FRegionDeDessin);
    end;
  //except
  //end;
end;
{.$IFDEF GHTOPO_SIMPLIFIE}
procedure TGHTopoDrawingContext.DrawOverlay();
var
  Q: Integer;
begin
  DefineBrosseEtCrayon(bsSolid, clMaroon, 64, psSolid, 0, clBlue, 255);
  self.CanvasBGRA.Rectangle(0,0,self.CanvasBGRA.Width, self.CanvasBGRA.Height, True);
  // on trace un réticule
  Q := self.CanvasBGRA.Height div 2;
  self.CanvasBGRA.MoveTo(0, Q);
  self.CanvasBGRA.LineTo(self.CanvasBGRA.Width, Q);
  Q := self.CanvasBGRA.Width div 2;
  self.CanvasBGRA.MoveTo(Q, 0);
  self.CanvasBGRA.LineTo(Q, self.CanvasBGRA.Height);
end;

procedure TGHTopoDrawingContext.DrawShortestPath(const FG: TPathFindingGraphe; const FP: TPathBetweenNodes);
var
  i, Nb: Integer;
  MyNode: TBZClassNode;
  PP: array of TBZClassNode;
  procedure MiouMiou(const qStyle: TPenStyle; const qWidth: integer; const qColor: TColor; const qOpacity: byte);
  var
    s: Integer;
  begin
    DefineCrayon(qStyle, qWidth, qColor, qOpacity);
    TraceVers(PP[0].Position.X, PP[0].Position.Y, false);
    for s := 1 to High(PP) do TraceVers(PP[s].Position.X, PP[s].Position.Y, True);
  end;
  procedure DrwStations();
  var
    s: Integer;
  begin
    DefineBrosseEtCrayon(bsSolid, clRed, 192, pssolid, 1, clRed, 255);
    for s := 1 to High(PP) do self.DrawShape(PP[s].Position.X, PP[s].Position.Y, 1, 8, 8);
  end;
begin
  Nb := FP.GetNbNoeuds();
  AfficherMessage(Format('%s.DrawShortestPath: %d noeuds', [classname, Nb]));
  if (Nb < 2) then exit;
  SetLength(PP, Nb);
  // préparation du tableau de tracé (le tracé se fait en deux passes)
  for i := 0 to Nb - 1 do PP[i] := FG.GetStation(FP.GetNoeud(i));
  MiouMiou(psSolid, 7, clBlue, 255);
  MiouMiou(psSolid, 5, clYellow, 255);
  MiouMiou(psSolid, 1, clRed, 255);
  DrwStations();
  SetLength(PP, 0);
end;
{.$ENDIF GHTOPO_SIMPLIFIE}
end.
