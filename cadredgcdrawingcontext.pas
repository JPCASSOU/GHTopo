unit CadreDGCDrawingContext;
// Cadre générique de dessin 2D en coordonnées GCS indépendant de GHTopo
// Doit remplacer à terme les contextes d'affichage des diagrammes d'orientation et d'altimétrie
// ainsi que le visualisateur de la carte du frontal BDD 6 Minutes
// Ceci est aussi un exercice sur l'approche 'Listes d'Affichage'
// Avantages:
// - Solution élégante
// - Souplesse du TList
// Gros inconvénient: Lenteur avec de nombreux graphismes (> 1000)
// 11/10/2019: Retour aux coordonnées GCS (plus lent mais plus logique et polyvalent
// 15/10/2019: Ajout des fonctionnalités suivantes:
// - Support des repères non orthonormaux
// - Deux paires de fonctions de conversions, chaque paire étant appelée par pointeur
// 16/10/2019: Indépendance totale de GHTopo - Feuilles de style

{$mode delphiunicode}

interface

uses
  Math, Types,
  Graphics,
  BGRABitmap,
  DGCTypes,
  DGCUtilityFunctions,
  DGCStylesPenBrushFont,
  DGCPrimitivesSimples,
  DGCPrimitivesComplexes,
  DGCUnitListeStylesObjets,
  DGCUnitListeAffichage,
  SVGCanvasUnit,
  Dialogs,
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, ActnList;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+ Cadre de dessin                                                            +
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
type
{ TCdrDGCDrawingContext }

 TCdrDGCDrawingContext = class(TFrame)
    aclstDGCGraphicCtxt: TActionList;
    acPanVue: TAction;
    acPanLeft: TAction;
    acPanRight: TAction;
    acPanUp: TAction;
    acPanDown: TAction;
    acExportSVG: TAction;
    acZoomFenetre: TAction;
    acZoomTout: TAction;
    acZoomMoins: TAction;
    acZoomPlus: TAction;
    imglstDGCGraphicCtxt: TImageList;
    lbMousePosition: TStaticText;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    Vue: TPaintBox;
    pnlVue: TPanel;
    procedure acExportSVGExecute(Sender: TObject);
    procedure acPanDownExecute(Sender: TObject);
    procedure acPanLeftExecute(Sender: TObject);
    procedure acPanRightExecute(Sender: TObject);
    procedure acPanUpExecute(Sender: TObject);
    procedure acPanVueExecute(Sender: TObject);
    procedure acZoomFenetreExecute(Sender: TObject);
    procedure acZoomMoinsExecute(Sender: TObject);
    procedure acZoomPlusExecute(Sender: TObject);
    procedure acZoomToutExecute(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure VueClick(Sender: TObject);
    procedure VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure VuePaint(Sender: TObject);
  strict private
    FLastError: string;
    FTmpBuffer: TBGRABitmap;
    // repère orthonormal ?
    FIsOrthonormal: boolean;
    FRx, FRy: double;
    // fonctions de conversion
    GetCoordsMonde: TFunctionGetCoordsMonde;
    GetCoordsPlan : TFunctionGetCoordsPlan;
    // options du dessin
    FDessinOptions: TDGCOptions;
    // modes de travail
    FModeTravail: TDGCModesTravail;
    FAttenduSecondPoint: boolean; // pour pan et zoom
    // position courante
    FCurrentMousePos: TDGCPoint2D;
    // couleurs et styles: brosse, crayon
    FBackGroundColor  : TColor;
    FBackGroundOpacity: byte;
    // étendue originale
    FOriginalCoinBasGauche: TDGCPoint2D;
    FOriginalCoinHautDroit: TDGCPoint2D;

    // gestion des zooms
    FZC1, FZC2    : TDGCPoint2D;
    FZP1, FZP2    : TPoint;
    FRegionCMini  : TDGCPoint2D;
    FRegionCMaxi  : TDGCPoint2D;
    // paramètres de vues
    FRappHLVue      : double;
    FRappScrReal    : double;
    FInvRappScrReal : double;
    // statut du dessin
    FDessinReady    : boolean;
    FDessinEnCours  : boolean;

    // Objets temporaires
    FTemporaryPolyLineGon: TDGCPolyLineGon;
    FMakingPolyLineGon: boolean;
    // Drapeaux divers:
    FCurveFilled: Boolean; // Init facultatif, valeur définie par BeginCurve() via un paramètre obligatoire
    // liste des objets à tracer
    FListOfEntities: TDGCListeAffichage;
    // Liste des styles
    FListOfStyles  : TDGCListeStylesSheets;
    FCurrentStyleSheet : integer;

  private
    FProcOnClick    : TProcedureOfObject;
    FProcPickCoords : TProcTransmitCoords;
    function GetCoordsMondeOrthonormal(const PP: TPoint): TDGCPoint2D;
    function GetCoordsPlanOrthonormal(const PM: TDGCPoint2D): TPoint;
    function GetCoordsMondeNonOrthonormal(const PP: TPoint): TDGCPoint2D;
    function GetCoordsPlanNonOrthonormal(const PM: TDGCPoint2D): TPoint;


    function GetRYMaxi(): double;
    procedure RedessinVue();
    // /!\ Les objets doivent être ici créés par l'appelant
    function AddPolyLine(const P: TDGCPolyline; const QName: string; const DoFlush: boolean): boolean;
    function AddPolygon(const P: TDGCPolygon; const QName: string; const DoFlush: boolean): boolean;
    function AddCurve(const P: TDGCCurve; const QName: string; const DoFlush: boolean): boolean;

    function QBeginPoly(const QIdxStyleSheet: integer;
                        const QName: string; const QDoFlush: boolean): boolean;
    procedure QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
    procedure Rearmer();
  public
    function  Initialiser(const X1, Y1, X2, Y2: double; const IsOrthonormal: boolean; const BackColor: TColor = clWhite): boolean;
    procedure Finaliser();
    function GetCurrentPosition(): TDGCPoint2D;
    procedure SetProcOnClick(const P: TProcedureOfObject);
    procedure SetProcPickCoords(const P: TProcTransmitCoords);
    procedure ClearDrawing();
    //modes de travail
    procedure SetDessinOptions(const Q: TDGCOptions);
    function GetDessinOptions(): TDGCOptions;
    procedure SetModeTravail(const MT: TDGCModesTravail);
    function  GetModeTravail(): TDGCModesTravail;
    // feuilles de styles
    function GetNbStyleSheets(): integer;
    function  GetStyleSheet(const Idx: integer): TDGCStyleSheet;
    procedure PutStyleSheet(const Idx: integer; const FS: TDGCStyleSheet);
    function MakeTDGCStyleSheet(const QStylename: string;
                                const QPenColor: TColor;
                                const QPenOpacity: byte;
                                const QPenStyle: TPenStyle;
                                const QPenWidthInPX: byte;
                                const QPenWidthInMM: byte;
                                const QBrushColor: TColor;
                                const QBrushOpacity: byte;
                                const QBrushStyle: TBrushStyle;
                                const QFontName: string;
                                const QFontColor: TColor;
                                const QFontOpacity: byte;
                                const QFontSizeInPts: integer;
                                const QFontSizeInMM: double;
                                const QFontStyle: TFontStyles;
                                const QDescription: string = ''): TDGCStyleSheet;
    procedure AddStyleSheet(const OS: TDGCStyleSheet); overload;
    procedure AddStyleSheet(const QStylename        : string;// Crayon
                             const QPenColor         : TColor;
                             const QPenOpacity       : byte;
                             const QPenStyle         : TPenStyle;
                             const QPenWidthInPX     : byte;
                             const QPenWidthInMM     : double;
                             const QBrushColor       : TColor;
                             const QBrushOpacity     : byte;
                             const QBrushStyle       : TBrushStyle;
                             const QFontName         : string;
                             // SVG utilise les attributs de ligne et de remplissage pour tracer les textes
                             const QFontColor        : TColor; // Utilisé par GDI uniquement
                             const QFontOpacity      : byte;
                             const QFontSizeInPts: integer;
                             const QFontSizeInMM: double;
                             const QFontStyle        : TFontStyles;
                             const QDescription       : string = ''); overload;
    // définition des styles de crayon et brosses
    // méthode déconseillée - Utiliser les feuilles de style
    procedure SetBackgroundColor(const C: TColor; const QOpacity: byte = 255);
    function  GetBackgroundColor(): TColor;
    procedure SetPenColorAttributes(const C: TColor; const QOpacity: byte = 255; const QWidthInPx: integer = 0; const QWidthInMM: double = 0.05; const QStyle: TPenStyle = psSolid);
    procedure SetBrushColorAttributes(const C: TColor; const QOpacity: byte = 255; const QStyle: TBrushStyle = bsSolid);
    procedure SetFontColorAttributes(const QFontName:string; const C: TColor; const QOpacity: byte; const QHeight: byte; const QStyle: TFontStyles);
    procedure SetFontName(const QFontName: string);
    procedure SetFontColor(const C: TColor; const QOpacity: byte = 255);
    procedure SetFontHeight(const QHeightInPoints: byte)  ; overload;    // en points
    procedure SetFontHeight(const QHeightInMeters: double); overload;    // en mètres
    procedure SetFontStyle(const QStyle: TFontStyles);




    procedure ResetVue(const DoRefresh: boolean); // redessine avec l'étendue originale en conservant la liste d'affichage
    procedure SetViewLimits(const X1, Y1, X2, Y2: double; const DebugTag: string = '');
    function  GetVueBounds(): TDGCBoundingBox;
    procedure DeplacerVue(const QOffsetX, QOffsetY: double);
    procedure ResizeVue();
    procedure RefreshVue();

    // ajout  des éléments graphiques
    function GetNbEntities(): integer;
    function  AddLine(const QIdxStyleSheet: integer;
                             const X1, Y1, X2, Y2: double;
                             const QName: string = 'Line';
                             const DoFlush: boolean = false): boolean;
    function  AddEllipse(const QIdxStyleSheet: integer;
                             const X1, Y1, R1, R2: double;
                             const QName: string = 'Ellipse';
                             const DoFlush: boolean = false): boolean;
    function  AddTriangle(const QIdxStyleSheet: integer;
                              const X1, Y1, X2, Y2, X3, Y3: double;
                              const QName: string = 'Triangle';
                              const DoFlush: boolean = false): boolean;
    function  AddRectangle(const QIdxStyleSheet: integer;
                              const X1, Y1, X2, Y2: double;
                              const QName: string = 'Rectangle';
                              const DoFlush: boolean = false): boolean;
    function  AddTexte(const QIdxStyleSheet: integer;
                              const X1, Y1: double;
                              const QAlignment: byte;
                              const QOrientation: double;
                              const QText: string;
                              const DoFlush: boolean = false): boolean;
    function  AddInfiniteLine(const QIdxStyleSheet: integer;
                              const QOrientation: TInfiniteLineOrientation;
                              const QX, QY: double;
                              const QName: string='Line';
                              const DoFlush: boolean=false): boolean;
    function  BeginPolyline(const QIdxStyleSheet: integer;
                            const QName: string = 'PolyLine';
                            const QDoFlush: boolean=false): boolean;
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
    procedure EndCurve();
    // procedure SetListCapacity(const N: integer); // Inutile: pas de gain de temps significatif
    procedure BeginDrawing();
    procedure EndDrawing();
    procedure BeginGroupe(const GN: string);
    procedure EndGroupe(const GN: string);

    procedure Flush();
    function  GetLastError(): string;
    function  GetEntite(const Idx: Integer): TDGCDessinObjet;
    function  RemoveEntite(const Idx: integer): boolean;

    // export en SVG
    procedure ExportSVG(const QFilename: RawByteString);
  end;

implementation
uses
  DGCDummyUnit; // Anti-erreur 'Fin du conde source non trouvée'

{$R *.lfm}
function TCdrDGCDrawingContext.Initialiser(const X1, Y1, X2, Y2: double; const IsOrthonormal: boolean; const BackColor: TColor = clWhite): boolean;
begin
  Result := false;
  FProcOnClick   := nil;
  FProcPickCoords:= nil;
  FIsOrthonormal := IsOrthonormal;
  if (FIsOrthonormal) then
  begin
    GetCoordsMonde := GetCoordsMondeOrthonormal;
    GetCoordsPlan  := GetCoordsPlanOrthonormal;
  end else
  begin
    GetCoordsMonde := GetCoordsMondeNonOrthonormal;
    GetCoordsPlan  := GetCoordsPlanNonOrthonormal;
  end;

  FModeTravail        := mtgcsREADY;
  FAttenduSecondPoint := false;
  FLastError          := '';
  SetBackgroundColor(BackColor);
  SetDessinOptions([]);
  FDessinReady        := false;
  FDessinEnCours      := False;
  FMakingPolyLineGon  := false;
  FCurveFilled        := false;;
  FListOfEntities := TDGCListeAffichage.Create;
  FListOfStyles   := TDGCListeStylesSheets.Create;
  try
    FListOfEntities.ClearListe();
    FListOfStyles.ClearListe(True);
    FCurrentStyleSheet := 0;
    FOriginalCoinBasGauche := MakeTDGCPoint2D(X1, Y1);
    FOriginalCoinHautDroit := MakeTDGCPoint2D(X2, Y2);
    self.ResetVue(false);
    //Label1.Caption:= format('%f, %f -> %f, %f', [FRegionCMini.X, FRegionCMini.Y, FRegionCMaxi.X, FRegionCMaxi.Y]);
    FDessinReady := True;
    result := True;
    Vue.Invalidate;
  except

  end;
end;
procedure TCdrDGCDrawingContext.Finaliser();
begin
  try
    FListOfStyles.ClearListe(false);
    FListOfEntities.ClearListe();
  finally
    FreeAndNil(FListOfStyles);
    FreeAndNil(FListOfEntities);
  end;
end;

function TCdrDGCDrawingContext.GetCurrentPosition(): TDGCPoint2D;
begin
  Result := FCurrentMousePos;
end;

procedure TCdrDGCDrawingContext.SetProcOnClick(const P: TProcedureOfObject);
begin
  FProcOnClick := P;
end;

procedure TCdrDGCDrawingContext.SetProcPickCoords(const P: TProcTransmitCoords);
begin
  FProcPickCoords := P;
end;

procedure TCdrDGCDrawingContext.ClearDrawing();
begin
  FListOfStyles.ClearListe(True);
  FListOfEntities.ClearListe();
  vue.Invalidate;
end;

procedure TCdrDGCDrawingContext.SetDessinOptions(const Q: TDGCOptions);
begin
  FDessinOptions := Q;
end;

function TCdrDGCDrawingContext.GetDessinOptions(): TDGCOptions;
begin
  result := FDessinOptions;
end;

procedure TCdrDGCDrawingContext.SetModeTravail(const MT: TDGCModesTravail);
begin
  if (MT = FModeTravail) then Exit; // ne rien faire si le mode de travail est inchangé
  FModeTravail := MT;
  case FModeTravail of
    mtgcsREADY: Rearmer();
    mtgcsPAN_PREMIER_POINT: // pan et distance
    begin
      FZC1 := FCurrentMousePos;
      FZC2 := FCurrentMousePos;
      FAttenduSecondPoint := false;
      QSetPen(pmCopy, psSolid, 0, clGray);
    end;
    mtgcsPAN_SECOND_POINT:
    begin
      FAttenduSecondPoint := true;
      QSetPen(pmNotXor, psSolid, 0, clGray);
    end;
    mtgcsZOOM_PREMIER_COIN:
    begin
      FZC1 := FCurrentMousePos;
      FZC2 := FCurrentMousePos;
      FAttenduSecondPoint := false;
      QSetPen(pmCopy, psSolid, 0, clGray);
    end;
    mtgcsZOOM_SECOND_COIN:
    begin
      FAttenduSecondPoint := true;
      QSetPen(pmNotXor, psSolid, 0, clGray);
    end;
  else
    nop;
  end;
end;

function TCdrDGCDrawingContext.GetModeTravail(): TDGCModesTravail;
begin
  Result := FModeTravail;
end;

function TCdrDGCDrawingContext.GetNbStyleSheets(): integer;
begin
  Result := FListOfStyles.GetNbElements();
end;

function TCdrDGCDrawingContext.GetStyleSheet(const Idx: integer): TDGCStyleSheet;
begin
  Result := FListOfStyles.GetElement(Idx);
end;

procedure TCdrDGCDrawingContext.PutStyleSheet(const Idx: integer; const FS: TDGCStyleSheet);
begin
  FListOfStyles.PutElement(Idx, FS);
end;

function  TCdrDGCDrawingContext.MakeTDGCStyleSheet(const QStylename: string;
                                                   const QPenColor: TColor;
                                                   const QPenOpacity: byte;
                                                   const QPenStyle: TPenStyle;
                                                   const QPenWidthInPX: byte;
                                                   const QPenWidthInMM: byte;
                                                   const QBrushColor: TColor;
                                                   const QBrushOpacity: byte;
                                                   const QBrushStyle: TBrushStyle;
                                                   const QFontName: string;
                                                   const QFontColor: TColor;
                                                   const QFontOpacity: byte;
                                                   const QFontSizeInPts: integer;
                                                   const QFontSizeInMM: double;
                                                   const QFontStyle: TFontStyles;
                                                   const QDescription: string = ''): TDGCStyleSheet;
begin
  with Result do
  begin
    Stylename      := QStylename;
    PenColor       := QPenColor;
    PenOpacity     := QPenOpacity;
    PenStyle       := QPenStyle;
    PenWidthInPX   := QPenWidthInPX;
    PenWidthInMM   := QPenWidthInMM;
    BrushColor     := QBrushColor;
    BrushOpacity   := QBrushOpacity;
    BrushStyle     := QBrushStyle;
    FontName       := QFontName;
    FontColor      := QFontColor;
    FontOpacity    := QFontOpacity;
    FontSizeInPts  := QFontSizeInPts;
    FontSizeInMM   := QFontSizeInMM;
    FontStyle      := QFontStyle;
    Description    := QDescription;
  end;
end;
procedure TCdrDGCDrawingContext.AddStyleSheet(const OS: TDGCStyleSheet);
begin
  FListOfStyles.AddElement(OS);
end;

procedure TCdrDGCDrawingContext.AddStyleSheet(const QStylename: string;
                                               const QPenColor: TColor;
                                               const QPenOpacity: byte;
                                               const QPenStyle: TPenStyle;
                                               const QPenWidthInPX     : byte;
                                               const QPenWidthInMM     : double;
                                               const QBrushColor: TColor;
                                               const QBrushOpacity: byte;
                                               const QBrushStyle: TBrushStyle;
                                               const QFontName: string;
                                               const QFontColor: TColor;
                                               const QFontOpacity: byte;
                                               const QFontSizeInPts: integer;
                                               const QFontSizeInMM: double;
                                               const QFontStyle: TFontStyles;
                                               const QDescription: string = '');
var
  MyStyle: TDGCStyleSheet;
  n: Integer;
begin
  n := FListOfStyles.GetNbElements() - 1;
  with MyStyle do
  begin
    Stylename      := QStylename;
    PenColor       := QPenColor;
    PenOpacity     := QPenOpacity;
    PenStyle       := QPenStyle;
    PenWidthInPX   := QPenWidthInPX;
    PenWidthInMM   := QPenWidthInMM;
    BrushColor     := QBrushColor;
    BrushOpacity   := QBrushOpacity;
    BrushStyle     := QBrushStyle;
    FontName       := QFontName;
    FontColor      := QFontColor;
    FontOpacity    := QFontOpacity;
    FontSizeInPts  := QFontSizeInPts;
    FontSizeInMM   := QFontSizeInMM;
    FontStyle      := QFontStyle;
    Description    := Format('Style%d: %s', [n, QDescription]);
  end;
  FListOfStyles.AddElement(MyStyle);
end;



procedure TCdrDGCDrawingContext.SetBackgroundColor(const C: TColor; const QOpacity: byte = 255);
begin
  FBackGroundColor   := C;
  FBackGroundOpacity := QOpacity;
end;

function TCdrDGCDrawingContext.GetBackgroundColor(): TColor;
begin
  result := FBackGroundColor;
end;

procedure TCdrDGCDrawingContext.SetPenColorAttributes(const C: TColor; const QOpacity: byte = 255; const QWidthInPx: integer = 0; const QWidthInMM: double = 0.05; const QStyle: TPenStyle = psSolid);
var
  MyPen: TDGCPen;
begin
  {$NOTE Redéfinition à la volée déconseillée - Utiliser les feuilles de style}
  MyPen := TDGCPen.Create;
  MyPen.Couleur := C;
  MyPen.Opacity := QOpacity;
  MyPen.WidthInPX   := QWidthInPx;
  MyPen.WidthInMM   := QWidthInMM;

  MyPen.Style   := QStyle;
  FListOfEntities.AddElement(tdoCMD_SET_PEN, '', MyPen);
end;

procedure TCdrDGCDrawingContext.SetBrushColorAttributes(const C: TColor; const QOpacity: byte; const QStyle: TBrushStyle = bsSolid);
var
  MyBrush: TDGCBrush;
begin
  {$NOTE Redéfinition à la volée déconseillée - Utiliser les feuilles de style}
  MyBrush := TDGCBrush.Create;
  MyBrush.Couleur  := C;
  MyBrush.Opacity  := QOpacity;
  MyBrush.Style    := QStyle;
  FListOfEntities.AddElement(tdoCMD_SET_BRUSH, '', MyBrush);
end;

procedure TCdrDGCDrawingContext.SetFontColorAttributes(const QFontName:string; const C: TColor; const QOpacity: byte; const QHeight: byte; const QStyle: TFontStyles);
var
  MyFont: TDGCFont;
begin
  {$NOTE Redéfinition à la volée déconseillée - Utiliser les feuilles de style}
  MyFont := TDGCFont.Create;
  MyFont.FontName := QFontName;
  MyFont.Couleur  := C;
  MyFont.Opacity  := QOpacity;
  MyFont.Height   := QHeight;
  MyFont.Style    := QStyle;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TCdrDGCDrawingContext.SetFontName(const QFontName: string);
var
  MyFont: TDGCFont;
begin
  {$NOTE Redéfinition à la volée déconseillée - Utiliser les feuilles de style}
  MyFont := TDGCFont.Create;
  MyFont.FontName := QFontName;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TCdrDGCDrawingContext.SetFontColor(const C: TColor; const QOpacity: byte = 255);
var
  MyFont: TDGCFont;
begin
  {$NOTE Redéfinition à la volée déconseillée - Utiliser les feuilles de style}
  MyFont := TDGCFont.Create;
  MyFont.Couleur  := C;
  MyFont.Opacity  := QOpacity;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TCdrDGCDrawingContext.SetFontHeight(const QHeightInPoints: byte);
var
  MyFont: TDGCFont;
begin
  {$NOTE Redéfinition à la volée déconseillée - Utiliser les feuilles de style}
  MyFont := TDGCFont.Create;
  MyFont.Height   := QHeightInPoints;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TCdrDGCDrawingContext.SetFontHeight(const QHeightInMeters: double);
var
  MyFont: TDGCFont;
  PP0, PP1: TPoint;
begin
  {$NOTE Redéfinition à la volée déconseillée - Utiliser les feuilles de style}
  PP0 := GetCoordsPlan(MakeTDGCPoint2D(0.0, 0.0));
  PP1 := GetCoordsPlan(MakeTDGCPoint2D(QHeightInMeters, QHeightInMeters));

  MyFont := TDGCFont.Create;
  MyFont.Height   := PP1.y - PP0.y;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;


procedure TCdrDGCDrawingContext.SetFontStyle(const QStyle: TFontStyles);
var
  MyFont: TDGCFont;
begin
  {$NOTE Redéfinition à la volée déconseillée - Utiliser les feuilles de style}
  MyFont := TDGCFont.Create;
  MyFont.Style    := QStyle;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TCdrDGCDrawingContext.ResetVue(const DoRefresh: boolean);
begin
  self.SetViewLimits(FOriginalCoinBasGauche.X, FOriginalCoinBasGauche.Y,
                     FOriginalCoinHautDroit.X, FOriginalCoinHautDroit.Y,
                     '');
  if (DoRefresh) then vue.Invalidate;
end;



procedure TCdrDGCDrawingContext.RedessinVue();
var
  R: TRect;
  MyEntity: TDGCDessinObjet;
  Nb, i: Integer;
  T0, T1: TDateTime;
  HH, MM, SS, MS: word;
  // feuilles de style
  procedure QUseStyleSheet(const QIdxStyleSheet: integer);
  var
    MyStyleSheet: TDGCStyleSheet;
  begin
    if (QIdxStyleSheet = DIEU_AU_CARRE) then Exit;
    MyStyleSheet := FListOfStyles.GetElement(QIdxStyleSheet);
    FTmpBuffer.CanvasBGRA.Pen.Color      := MyStyleSheet.PenColor;
    FTmpBuffer.CanvasBGRA.Pen.Opacity    := MyStyleSheet.PenOpacity;
    FTmpBuffer.CanvasBGRA.Pen.Width      := MyStyleSheet.PenWidthInPX;
    FTmpBuffer.CanvasBGRA.Pen.Style      := MyStyleSheet.PenStyle;
    FTmpBuffer.CanvasBGRA.Brush.Color    := MyStyleSheet.BrushColor;
    FTmpBuffer.CanvasBGRA.Brush.Opacity  := MyStyleSheet.BrushOpacity;
    FTmpBuffer.CanvasBGRA.Brush.Style    := MyStyleSheet.BrushStyle;
    FTmpBuffer.CanvasBGRA.Font.Name      := MyStyleSheet.FontName;
    FTmpBuffer.CanvasBGRA.Font.Height    := MyStyleSheet.FontSizeInPts;
    FTmpBuffer.CanvasBGRA.Font.Color     := MyStyleSheet.FontColor;
    FTmpBuffer.CanvasBGRA.Font.Opacity   := MyStyleSheet.FontOpacity;
    FTmpBuffer.CanvasBGRA.Font.Style     := MyStyleSheet.FontStyle;
  end;
  // dessin des objets
  procedure QSetPenStyle(const E: TDGCPen);
  begin
    FTmpBuffer.CanvasBGRA.Pen.Color    := E.Couleur;
    FTmpBuffer.CanvasBGRA.Pen.Opacity  := E.Opacity;
    FTmpBuffer.CanvasBGRA.Pen.Width    := E.WidthInPX;
    FTmpBuffer.CanvasBGRA.Pen.Style    := E.Style;
  end;
  procedure QSetBrushStyle(const E: TDGCBrush);
  begin
    FTmpBuffer.CanvasBGRA.Brush.Color    := E.Couleur;
    FTmpBuffer.CanvasBGRA.Brush.Opacity  := E.Opacity;
    FTmpBuffer.CanvasBGRA.Brush.Style    := E.Style;
  end;
  procedure QSetFontStyle(const E: TDGCFont);
  begin
    FTmpBuffer.CanvasBGRA.Font.Name     := E.FontName;
    FTmpBuffer.CanvasBGRA.Font.Height   := E.Height;
    FTmpBuffer.CanvasBGRA.Font.Color    := E.Couleur;
    FTmpBuffer.CanvasBGRA.Font.Opacity  := E.Opacity;
    FTmpBuffer.CanvasBGRA.Font.Style    := E.Style;
  end;
  procedure QDrawSegment(const E: TDGCSegment);
  var
    PP1, PP2: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    PP1 := GetCoordsPlan(E.P1);
    PP2 := GetCoordsPlan(E.P2);
    FTmpBuffer.CanvasBGRA.MoveTo(PP1.x, PP1.y);
    FTmpBuffer.CanvasBGRA.LineTo(PP2.x, PP2.y);
  end;
  procedure QDrawRectangle(const E: TDGCRectangle);
  var
    PP1, PP2: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    PP1 := GetCoordsPlan(E.P1);
    PP2 := GetCoordsPlan(E.P2);
    FTmpBuffer.CanvasBGRA.Rectangle(PP1.x, PP1.y, PP2.x, PP2.y);
  end;
  procedure QDrawInfiniteHVLine(const E: TDGCInfiniteLine);
  var
    PP1, PP2: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    case E.Orientation of
     tdgcVERTICAL_LINE:
       begin
         PP1 := GetCoordsPlan(MakeTDGCPoint2D(E.PositionCentre.X, FRegionCMini.Y));
         PP2 := GetCoordsPlan(MakeTDGCPoint2D(E.PositionCentre.X, FRegionCMaxi.Y));
       end;
     tdgcHORIZONTAL_LINE:
       begin
         PP1 := GetCoordsPlan(MakeTDGCPoint2D(FRegionCMini.X, E.PositionCentre.Y));
         PP2 := GetCoordsPlan(MakeTDGCPoint2D(FRegionCMaxi.X, E.PositionCentre.Y));
       end;
    end;
    FTmpBuffer.CanvasBGRA.MoveTo(PP1.x, PP1.y);
    FTmpBuffer.CanvasBGRA.LineTo(PP2.x, PP2.y);
  end;
  procedure QDrawEllipse(const E: TDGCEllipse);
  var
    PPC, PPR: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    PPC := GetCoordsPlan(E.Centre);
    PPR := GetCoordsPlan(MakeTDGCPoint2D(E.Centre.X + E.Rayon1, E.Centre.Y + E.Rayon2));
    FTmpBuffer.CanvasBGRA.EllipseC(PPC.X, PPC.Y, PPR.X - PPC.X, PPR.Y - PPC.Y);
  end;
  procedure QDrawTriangle(const E: TDGCTriangle);
  var
    PP1, PP3, PP2: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    PP1 := GetCoordsPlan(E.P1);
    PP2 := GetCoordsPlan(E.P2);
    PP3 := GetCoordsPlan(E.P3);
    FTmpBuffer.CanvasBGRA.Polygon([PP1, PP2, PP3]);
  end;
  procedure QDrawPolyline(const E: TDGCPolyline);
  var
    i, Nb: integer;
    PP: array of TPoint;
  begin
    Nb := E.GetNbVertex();
    if (Nb < 2) then Exit;
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);

    Setlength(PP, Nb);
    for i := 0 to Nb - 1 do PP[i] := GetCoordsPlan(E.GetVertex(i));
    FTmpBuffer.CanvasBGRA.Polyline(PP);
  end;
  procedure QDrawPolygon(const E: TDGCPolygon);
  var
    i, Nb: integer;
    PP: array of TPoint;
  begin
    Nb := E.GetNbVertex();
     if (Nb < 2) then Exit;
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
     Setlength(PP, Nb);
    for i := 0 to Nb - 1 do PP[i] := GetCoordsPlan(E.GetVertex(i));
    FTmpBuffer.CanvasBGRA.Polygon(PP);
  end;

  procedure QDrawCurve(const E: TDGCCurve);
  var
    i, Nb: integer;
    procedure QDrawBezierArc(const B: TDGCBezierArc; const QContinuous: boolean);
    var
      QArcBezier: array[0..3] of TPoint;
    begin
      QArcBezier[0] := GetCoordsPlan(B.PT1);
      QArcBezier[1] := GetCoordsPlan(MakeTDGCPoint2D(B.PT1.X + B.TangP1.X, B.PT1.Y + B.TangP1.Y));
      QArcBezier[2] := GetCoordsPlan(MakeTDGCPoint2D(B.PT2.X + B.TangP2.X, B.PT2.Y + B.TangP2.Y));
      QArcBezier[3] := GetCoordsPlan(B.PT2);
      FTmpBuffer.CanvasBGRA.PolyBezier(QArcBezier, E.Filled, QContinuous);
    end;
  begin
    Nb := E.GetNbArcs();
    if (Nb = 0) then Exit;
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    for i := 0 to Nb - 1 do QDrawBezierArc(E.GetBezierArc(i), i > 0);
    // TODO:Implémenter cf GHCaveDraw
  end;
  procedure QDrawText(const E: TDGCText);
  var
    QOldOrientation: Integer;
    ExTxt: TSize;
    PP, PosTXT: TPoint;
  begin
    QOldOrientation := FTmpBuffer.CanvasBGRA.Font.Orientation;
    FTmpBuffer.CanvasBGRA.Font.Orientation := Trunc(E.Orientation); // TFont.Orientation est ici en déci-degrés)
    PP := GetCoordsPlan(E.Position);
    ExTxt := FTmpBuffer.CanvasBGRA.TextExtent(E.Text);
    // 7 8 9
    // 4 5 6
    // 1 2 3
    //   0
    case E.Alignment of
      0,1: PosTXT := DGCMakeTPoint(PP.X, PP.Y - ExTxt.cy);
      2  : PosTXT := DGCMakeTPoint(PP.X - ExTxt.cx div 2, PP.Y - ExTxt.cy);
      3  : PosTXT := DGCMakeTPoint(PP.X - ExTxt.cx, PP.Y - ExTxt.cy);
      4  : PosTXT := DGCMakeTPoint(PP.X, PP.Y - ExTxt.cy div 2);
      5  : PosTXT := DGCMakeTPoint(PP.X - ExTxt.cx div 2, PP.Y - ExTxt.cy div 2);
      6  : PosTXT := DGCMakeTPoint(PP.X - ExTxt.cx, PP.Y - ExTxt.cy div 2);
      7  : PosTXT := DGCMakeTPoint(PP.X, PP.Y);
      8  : PosTXT := DGCMakeTPoint(PP.X - ExTxt.cx div 2, PP.Y);
      9  : PosTXT := DGCMakeTPoint(PP.X - ExTxt.cx, PP.Y);
    else
      PosTXT := DGCMakeTPoint(PP.X, PP.Y - ExTxt.cy);
    end;
    (*
    if (DoDrawTextExt) then
    begin
      self.CanvasBGRA.Brush.Color := clAqua;     // modification d'un seul paramètre de brosse ou crayon
      self.CanvasBGRA.Pen.Color   := clBlue;     // modification d'un seul paramètre de brosse ou crayon
      self.CanvasBGRA.Rectangle(PosTXT.X, PosTXT.y,
                               PosTXT.X + self.CanvasBGRA.TextExtent(s).cX,
                               PosTXT.Y + self.CanvasBGRA.TextExtent(s).cY);
      self.CanvasBGRA.Rectangle(PP.X, PP.Y, PP.X + 2, PP.Y + 2); // débogage: Tracé du point de base
      self.CanvasBGRA.TextOut(PosTXT.X, PosTXT.Y, S);
    end else begin
      self.CanvasBGRA.TextOut(PosTXT.X, PosTXT.Y, S);
    end;
    //*)
    FTmpBuffer.CanvasBGRA.TextOut(PosTXT.X, PosTXT.Y, E.Text);
    FTmpBuffer.CanvasBGRA.Font.Orientation := QOldOrientation;
  end;
begin
  if (not FDessinReady) then Exit;
  if (FDessinEnCours) then Exit; // FDessinEnCours est un flag pour protéger la reconstruction du dessin
  FDessinEnCours := True;
  FTmpBuffer := TBGRABitmap.Create(vue.Width, vue.Height);
  try
    T0 := Now();
    R.Left   := Vue.Left;
    R.Top    := Vue.Top;
    R.Bottom := Vue.Top  + Vue.Height;
    R.Right  := Vue.Left + Vue.Width;
    FTmpBuffer.CanvasBGRA.Brush.Color := FBackGroundColor;
    FTmpBuffer.CanvasBGRA.Brush.Opacity := 255;
    FTmpBuffer.CanvasBGRA.Brush.Style   := bsSolid;
    FTmpBuffer.CanvasBGRA.FillRect(R);
    // on dessine ici: exécution de la liste d'affichage
    Nb := getNbEntities();
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do
      begin
        MyEntity := FListOfEntities.GetElement(i);
        case MyEntity.TypeDessinObjet of
          tdoNONE                 : nop;
          tdoENTITY_SEGMENT       : QDrawSegment(MyEntity.DessinObjet as TDGCSegment);
          tdoENTITY_Ellipse       : QDrawEllipse(MyEntity.DessinObjet  as TDGCEllipse);
          tdoENTITY_TRIANGLE      : QDrawTriangle(MyEntity.DessinObjet as TDGCTriangle);
          tdoENTITY_RECTANGLE     : QDrawRectangle(MyEntity.DessinObjet as TDGCRectangle);
          tdoENTITY_POLYLINE      : QDrawPolyline(MyEntity.DessinObjet as TDGCPolyline);
          tdoENTITY_POLYGONE      : QDrawPolygon(MyEntity.DessinObjet as TDGCPolygon);
          tdoENTITY_CURVE         : QDrawCurve(MyEntity.DessinObjet as TDGCCurve);
          tdoENTITY_TEXT          : QDrawText(MyEntity.DessinObjet as TDGCText);
          tdoCMD_SET_PEN          : QSetPenStyle(MyEntity.DessinObjet as TDGCPen);
          tdoCMD_SET_BRUSH        : QSetBrushStyle(MyEntity.DessinObjet as TDGCBrush);
          tdoCMD_SET_FONT         : QSetFontStyle(MyEntity.DessinObjet as TDGCFont);
          tdoENTITY_INFINITE_LINE : QDrawInfiniteHVLine(MyEntity.DessinObjet as TDGCInfiniteLine);
          tdoCMD_SVG_BEGIN_GROUPE : nop;
          tdoCMD_SVG_END_GROUPE   : nop;
        else
          nop;
        end;
      end;
    end;
    T1 := Now();
    FTmpBuffer.Draw(vue.Canvas, 0, 0, True);
    DecodeTime(t1-t0, HH, MM, SS, MS);
    //Label1.Caption:= format('%f, %f -> %f, %f - %d entites - Temps: %.2d:%.2d:%.2d.%.3d', [FRegionCMini.X, FRegionCMini.Y, FRegionCMaxi.X, FRegionCMaxi.Y, Nb, HH, MM, SS, MS]);
  finally
    FreeAndNil(FTmpBuffer);
    FDessinEnCours := False;
  end;
end;
function TCdrDGCDrawingContext.GetNbEntities(): integer;
begin
  Result := FListOfEntities.Count;
end;



procedure TCdrDGCDrawingContext.VuePaint(Sender: TObject);
begin
  RedessinVue();
end;



procedure TCdrDGCDrawingContext.FrameResize(Sender: TObject);
begin
  ResizeVue();
end;

procedure TCdrDGCDrawingContext.SpeedButton21Click(Sender: TObject);
begin

end;

procedure TCdrDGCDrawingContext.VueClick(Sender: TObject);
begin

end;



procedure TCdrDGCDrawingContext.acZoomToutExecute(Sender: TObject);
begin
  ResetVue(true);
end;

procedure TCdrDGCDrawingContext.acZoomPlusExecute(Sender: TObject);
var
  EWE: TDGCBoundingBox;
  d: float;
begin
  EWE := GetVueBounds();
  d   := 0.025 * Hypot(EWE.X2 - EWE.X1, EWE.Y2 - EWE.Y1);
  SetViewLimits(EWE.X1 + d, EWE.Y1 + d, EWE.X2 - d, EWE.Y2 - d);
  Vue.Invalidate;
end;


procedure TCdrDGCDrawingContext.acZoomFenetreExecute(Sender: TObject);
begin
  SetModeTravail(mtgcsZOOM_PREMIER_COIN);
end;

procedure TCdrDGCDrawingContext.acZoomMoinsExecute(Sender: TObject);
var
  EWE: TDGCBoundingBox;
  d: float;
begin
  EWE := GetVueBounds();
  d   := -0.025 * Hypot(EWE.X2 - EWE.X1, EWE.Y2 - EWE.Y1);
  SetViewLimits(EWE.X1 + d, EWE.Y1 + d, EWE.X2 - d, EWE.Y2 - d);
  Vue.Invalidate;
end;



procedure TCdrDGCDrawingContext.acPanVueExecute(Sender: TObject);
begin
  SetModeTravail(mtgcsPAN_PREMIER_POINT);
end;

procedure TCdrDGCDrawingContext.acPanUpExecute(Sender: TObject);
var
  EWE: TDGCBoundingBox;
begin
  EWE := GetVueBounds();
  DeplacerVue(0.00, -0.05 * (EWE.Y2 - EWE.Y1));
end;

procedure TCdrDGCDrawingContext.acPanDownExecute(Sender: TObject);
var
  EWE: TDGCBoundingBox;
begin
  EWE := GetVueBounds();
  DeplacerVue(0.00, 0.05 * (EWE.Y2 - EWE.Y1));
end;

procedure TCdrDGCDrawingContext.acExportSVGExecute(Sender: TObject);
var
  t: TDateTime;
  YYYY, MM, DD, HH, MN, SS, MS: word;
  DirectoryDestSVGs: UnicodeString;
begin
  t := Now();
  DecodeDate(t, YYYY, MM, DD);
  DecodeTime(t, HH, MN, SS, MS);
  DirectoryDestSVGs := ExtractFilePath(paramstr(0)) + 'SVGs';
  ForceDirectories(DirectoryDestSVGs);
  self.ExportSVG(DirectoryDestSVGs + PathDelim + Format('svg_%.4d-%.2d-%.2d_%.2dh%.2dm%.2ds%.3dms.svg', [YYYY, MM, DD, HH, MN, SS, MS]));
end;



procedure TCdrDGCDrawingContext.acPanLeftExecute(Sender: TObject);
var
  EWE: TDGCBoundingBox;
begin
  EWE := GetVueBounds();
  DeplacerVue(-0.05 * (EWE.X2 - EWE.X1), 0.00);
end;

procedure TCdrDGCDrawingContext.acPanRightExecute(Sender: TObject);
var
  EWE: TDGCBoundingBox;
begin
  EWE := GetVueBounds();
  DeplacerVue(0.05 * (EWE.X2 - EWE.X1), 0.00);
end;



procedure TCdrDGCDrawingContext.VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ReinitFZC(const DoFC1, DoFC2: boolean);
  begin
    if (DoFC1) then
    begin
      FZC1 := FCurrentMousePos;
      FZP1 := Point(X, Y);
    end;
    if (DoFC2) then
    begin
      FZC2 := FCurrentMousePos;
      FZP2 := Point(X, Y);
    end;
  end;
var
  QOffset: TDGCPoint2D;
begin
  if (not FDessinReady) then Exit;
  if (assigned(FProcOnClick)) then FProcOnClick();
  case FModeTravail of
    mtgcsREADY: nop;
    mtgcsPICK_COORDS:
      begin
        if (Assigned(FProcPickCoords)) then FProcPickCoords(FCurrentMousePos);
      end;
    mtgcsPAN_PREMIER_POINT:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtgcsPAN_SECOND_POINT);
      end;
    mtgcsPAN_SECOND_POINT:
      begin
        ReinitFZC(False, True);
        QOffset := MakeTDGCPoint2D(FZC2.X - FZC1.X, FZC2.Y - FZC1.Y);
        DeplacerVue(QOffset.X, QOffset.Y);
        SetModeTravail(mtgcsREADY);
        Vue.Invalidate;
      end;
    mtgcsZOOM_PREMIER_COIN:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtgcsZOOM_SECOND_COIN);
      end;

    mtgcsZOOM_SECOND_COIN:
      begin
        ReinitFZC(False, True);
        SetViewLimits(FZC1.X, FZC1.Y, FZC2.X, FZC2.Y, '');
        SetModeTravail(mtgcsREADY);
        Vue.Invalidate;
      end;
  else
    nop;
  end;
end;

procedure TCdrDGCDrawingContext.VueMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  procedure QDrawGhostLigne(const P1, P2: TPoint); inline;
  begin
    Vue.Canvas.Line(P1.X,P1.Y, P2.X, P2.Y);
  end;
  procedure QDrawGhostRectangle(const P1, P2: TPoint);
  begin
    Vue.Canvas.Brush.Style := bsClear;
    Vue.Canvas.Rectangle(P1.X, P1.Y, P2.X, P2.Y);
  end;
  procedure QDrawGhostRectangleBarre(const P1, P2: TPoint);
  begin
    Vue.Canvas.Brush.Style := bsClear;
    Vue.Canvas.Rectangle(P1.X, P1.Y, P2.X, P2.Y);
    Vue.Canvas.Line(P1.X, P1.Y, P2.X, P2.Y);
    Vue.Canvas.Line(P1.X, P2.Y, P2.X, P1.Y);
  end;
  procedure QDrawShapeElastique(const QX, QY: integer; const SType: byte);
    procedure CuiCui();
    begin
      case SType of
        1: QDrawGhostLigne(FZP1, FZP2);
        2: QDrawGhostRectangle(FZP1, FZP2);
        3: QDrawGhostRectangleBarre(FZP1, FZP2);
      end;
    end;
  begin
    CuiCui();
    FZP2 := Point(QX, QY);
    CuiCui();
  end;
begin
  if (not FDessinReady) then exit;
  FCurrentMousePos := GetCoordsMonde(Point(X, Y));
  lbMousePosition.Caption := format('X = %.2f, Y = %.2f', [FCurrentMousePos.X, FCurrentMousePos.Y]);
  Vue.Canvas.Pen.Color := clSilver;
  Vue.Canvas.Pen.Width := 0;
  case FModeTravail of
    mtgcsREADY:
      begin
        FZP1 := Point(X, Y);
        FZP2 := Point(X, Y);
      end;
    mtgcsPAN_SECOND_POINT: QDrawShapeElastique(X, Y, 1);
    mtgcsZOOM_SECOND_COIN: QDrawShapeElastique(X, Y, 2);
  else
    nop;
  end;
end;

procedure TCdrDGCDrawingContext.VueMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  nop;
end;

function TCdrDGCDrawingContext.GetCoordsMondeOrthonormal(const PP: TPoint): TDGCPoint2D;
begin
  Result.X :=  FInvRappScrReal * PP.X + FRegionCMini.X;
  Result.Y := -FInvRappScrReal * PP.Y + FRegionCMaxi.Y;
end;
function TCdrDGCDrawingContext.GetCoordsPlanOrthonormal(const PM: TDGCPoint2D): TPoint;
begin
  Result.X := Round((PM.X - FRegionCMini.X) * FRappScrReal);
  Result.Y := Round((FRegionCMaxi.Y - PM.Y) * FRappScrReal);
end;

function TCdrDGCDrawingContext.GetCoordsMondeNonOrthonormal(const PP: TPoint): TDGCPoint2D;
var
  rx, ry: Double;
begin
  rx := (FRegionCMaxi.X - FRegionCMini.X) / Vue.Width;
  ry := (FRegionCMaxi.Y - FRegionCMini.Y) / Vue.Height;

  Result.X := FRegionCMini.X + rx * PP.X;         // OK
  Result.Y := FRegionCMini.Y + ry * (Vue.Height - PP.Y);
end;

function TCdrDGCDrawingContext.GetCoordsPlanNonOrthonormal(const PM: TDGCPoint2D): TPoint;
begin
  Result.x := round((Vue.Width) * (PM.X - FRegionCMini.X) * FRx);
  Result.y := Vue.Height - (trunc(Vue.Height * (PM.Y - FRegionCMini.Y) * FRy));
end;


// cette fonction retourne d'autres paramètres
function TCdrDGCDrawingContext.GetRYMaxi(): double;
var
  qdx: Double;
begin
  qdx := FRegionCMaxi.X - FRegionCMini.X;
  FRappHLVue := Vue.Height / Vue.Width;                         // calcul du rapport Hauteur/largeur de vue
  FRappScrReal := Vue.Width / qdx;                              // calcul du rapport Ecran/Réel
  FInvRappScrReal := 1 / FRappScrReal;
  Result := FRegionCMini.Y + qdx * FRappHLVue;                  // calcul de la hauteur de visualisation
end;




procedure TCdrDGCDrawingContext.SetViewLimits(const X1, Y1, X2, Y2: double; const DebugTag: string = '');
const
  Epsilon = 1e-2;
var
  qX1, qX2, qY1, qY2: double;
  d1, d2: double;
begin
  qX1 := Min(X1, X2);
  qY1 := Min(Y1, Y2);
  qX2 := Max(X1, X2);
  qY2 := Max(Y1, Y2);

  d1 := qX2 - qX1;
  d2 := qY2 - qY1;
  // si zone trop étroite, abandonner
  if (Abs(d1) < Epsilon) or (Abs(d2) < Epsilon) then Exit;
  FRegionCMini := MakeTDGCPoint2D(qX1, qY1);
  FRegionCMaxi := MakeTDGCPoint2D(qX2, qY2);
  if (FIsOrthonormal) then
  begin
    FRegionCMaxi.Y := GetRYMaxi();
    FRx := -1.0;
    FRy := -1.0;
  end else
  begin
    FRx   := 1 / (FRegionCMaxi.X - FRegionCMini.X);
    FRy   := 1 / (FRegionCMaxi.Y - FRegionCMini.Y);
  end;
end;
procedure TCdrDGCDrawingContext.ResizeVue();
begin
  SetViewLimits(FRegionCMini.X, FRegionCMini.Y,
                FRegionCMaxi.X, FRegionCMaxi.Y, '');
  vue.Invalidate;
end;
procedure TCdrDGCDrawingContext.RefreshVue();
begin
  vue.Invalidate;
end;

procedure TCdrDGCDrawingContext.DeplacerVue(const QOffsetX, QOffsetY: double);
begin
  SetViewLimits(FRegionCMini.X - QOffsetX, FRegionCMini.Y - QOffsetY,
                FRegionCMaxi.X - QOffsetX, FRegionCMaxi.Y - QOffsetY,
                '');
  Vue.Invalidate;
end;



function TCdrDGCDrawingContext.GetVueBounds(): TDGCBoundingBox;
begin
  Result.X1 := FRegionCMini.X;
  Result.Y1 := FRegionCMini.Y;
  Result.X2 := FRegionCMaxi.X;
  Result.Y2 := FRegionCMaxi.Y;
end;

function TCdrDGCDrawingContext.AddInfiniteLine(const QIdxStyleSheet: integer;
                                               const QOrientation: TInfiniteLineOrientation;
                                               const QX, QY: double;
                                               const QName: string = 'Line';
                                               const DoFlush: boolean = false): boolean;
var
  E: TDGCInfiniteLine;
begin
  E := TDGCInfiniteLine.Create(QIdxStyleSheet, QOrientation, MakeTDGCPoint2D(QX, QY));
  Result := FListOfEntities.AddElement(tdoENTITY_INFINITE_LINE, '', E);
end;

// liste d'affichage pour les objets: Ajout des éléments
function TCdrDGCDrawingContext.AddLine(const QIdxStyleSheet: integer;
                                       const X1, Y1, X2, Y2: double; const QName: string = 'Line'; const DoFlush: boolean = false): boolean;
var
  MySegment: TDGCSegment;
begin
  MySegment := TDGCSegment.Create(QIdxStyleSheet, MakeTDGCPoint2D(X1, Y1), MakeTDGCPoint2D(X2, Y2));
  Result := FListOfEntities.AddElement(tdoENTITY_SEGMENT, QName, MySegment);
end;
function TCdrDGCDrawingContext.AddEllipse(const QIdxStyleSheet: integer;
                                          const X1, Y1, R1, R2: double; const QName: string = 'Ellipse'; const DoFlush: boolean = false): boolean;
var
  MyEllipse: TDGCEllipse;
begin
  MyEllipse := TDGCEllipse.Create(QIdxStyleSheet, MakeTDGCPoint2D(X1, Y1), R1, R2);
  Result := FListOfEntities.AddElement(tdoENTITY_Ellipse, QName, MyEllipse);
 end;
function TCdrDGCDrawingContext.AddTriangle(const QIdxStyleSheet: integer;
                                           const X1, Y1, X2, Y2, X3, Y3: double; const QName: string = 'Triangle'; const DoFlush: boolean = false): boolean;
var
  MyTriangle: TDGCTriangle;
begin
  if (not FDessinReady) then Exit;
  MyTriangle := TDGCTriangle.Create(QIdxStyleSheet, MakeTDGCPoint2D(X1, Y1), MakeTDGCPoint2D(X2, Y2), MakeTDGCPoint2D(X3, Y3));
  Result := FListOfEntities.AddElement(tdoENTITY_TRIANGLE, QName, MyTriangle);
end;

function TCdrDGCDrawingContext.AddRectangle(const QIdxStyleSheet: integer;
                                            const X1, Y1, X2, Y2: double; const QName: string; const DoFlush: boolean): boolean;
var
  MyRectangle: TDGCRectangle;
begin
  MyRectangle := TDGCRectangle.Create(QIdxStyleSheet, MakeTDGCPoint2D(X1, Y1), MakeTDGCPoint2D(X2, Y2));
  Result := FListOfEntities.AddElement(tdoENTITY_RECTANGLE, QName, MyRectangle);
end;

// /!\ Ici, l'objet TDGCPolyLineGon doit être créé par l'appelant
function TCdrDGCDrawingContext.AddPolyLine(const P: TDGCPolyline; const QName: string; const DoFlush: boolean): boolean;
begin
  Result := FListOfEntities.AddElement(tdoENTITY_POLYLINE, QName, P);
end;
function TCdrDGCDrawingContext.AddPolygon(const P: TDGCPolygon; const QName: string; const DoFlush: boolean): boolean;
begin
  Result := FListOfEntities.AddElement(tdoENTITY_POLYGONE, QName, P);
end;

function TCdrDGCDrawingContext.AddCurve(const P: TDGCCurve; const QName: string; const DoFlush: boolean): boolean;
begin
  Result := FListOfEntities.AddElement(tdoENTITY_CURVE, QName, P);
end;



function TCdrDGCDrawingContext.QBeginPoly(const QIdxStyleSheet: integer;
                                          const QName: string; const QDoFlush: boolean): boolean;
begin
  result := false;
  if (FMakingPolyLineGon) then begin FLastError := 'Polygone précédent non clôturé par EndPolygon()'; Exit; end;
  FTemporaryPolyLineGon := TDGCPolyLineGon.Create(QIdxStyleSheet, QName, QDoFlush);
  try
    FMakingPolyLineGon := True;
    Result := True;
  except
  end;
end;

procedure TCdrDGCDrawingContext.BeginDrawing();
begin
  FListOfStyles.ClearListe(True);
  FListOfEntities.ClearListe();
  nop;
end;
procedure TCdrDGCDrawingContext.EndDrawing();
begin
  nop;
end;

procedure TCdrDGCDrawingContext.BeginGroupe(const GN: string);
var
  G: TDGCBeginGroupeSVG;
begin
  G := TDGCBeginGroupeSVG.Create(GN);
  FListOfEntities.AddElement(tdoCMD_SVG_BEGIN_GROUPE, 'Begin' + GN, G);
end;

procedure TCdrDGCDrawingContext.EndGroupe(const GN: string);
var
  G: TDGCEndGroupeSVG;
begin
  G := TDGCEndGroupeSVG.Create(GN);
  FListOfEntities.AddElement(tdoCMD_SVG_END_GROUPE, 'End' + GN, G);
end;

// Ajout de polyligne/polygone
procedure TCdrDGCDrawingContext.AddVertex(const QX, QY: double);
begin
  if (not FMakingPolyLineGon) then
  begin
    FLastError := format('%s.AddVertex(%f, %f) sans %s.BeginPolyline()', [ClassName, QX, QY, ClassName]);
    Exit;
  end;
  if (not assigned(FTemporaryPolyLineGon)) then
  begin
    FLastError := format('Champ %s.FTemporaryPolyLineGon non initialisé', [ClassName]);
    Exit;
  end;
  FTemporaryPolyLineGon.AddVertex(MakeTDGCPoint2D(QX, QY));
end;
function TCdrDGCDrawingContext.BeginPolyline(const QIdxStyleSheet: integer;
                                             const QName: string = 'PolyLine'; const QDoFlush: boolean = false): boolean;
begin
  Result := QBeginPoly(QIdxStyleSheet, QName, QDoFlush);
end;
function TCdrDGCDrawingContext.BeginPolygon(const QIdxStyleSheet: integer;
                                            const QName: string = 'Polygone'; const QDoFlush: boolean = false): boolean;
begin
  Result := QBeginPoly(QIdxStyleSheet, QName, QDoFlush);
end;

function TCdrDGCDrawingContext.BeginCurve(const QIdxStyleSheet: integer;
                                          const QName: string = 'Curve';
                                          const QFilled: boolean = false;
                                          const QDoFlush: boolean = false): boolean;
begin
  FCurveFilled := QFilled;
  Result := QBeginPoly(QIdxStyleSheet, QName, QDoFlush);
end;


procedure TCdrDGCDrawingContext.EndPolyline();
var
  MyPolyLine: TDGCPolyline;
  Nb, i: Integer;
begin
  if (not FMakingPolyLineGon) then begin FLastError := format('%s.EndPolyline() sans %s.BeginPolyline()', [ClassName, ClassName]); Exit; end;
  Nb := FTemporaryPolyLineGon.GetNbVertex();
  if (Nb < 3) then begin FLastError := Format('Invalid polygon "%s", %d vertex', [FTemporaryPolyLineGon.Name, FTemporaryPolyLineGon.GetNbVertex()]); exit; end;
  try
    MyPolyLine := TDGCPolyline.Create(FTemporaryPolyLineGon.IdxStyleSheet, FTemporaryPolyLineGon.Name, FTemporaryPolyLineGon.DoFlush);
    for i := 0 to Nb - 1 do MyPolyLine.AddVertex(FTemporaryPolyLineGon.GetVertex(i));
    MyPolyLine.SetBoundingBox();
    AddPolyLine(MyPolyLine, FTemporaryPolyLineGon.Name, FTemporaryPolyLineGon.DoFlush);
    FMakingPolyLineGon := false;  // avant de désarmer le flag FMakingPolyLineGon
  finally
    FreeAndNil(FTemporaryPolyLineGon);
  end;
end;

procedure TCdrDGCDrawingContext.EndPolygon();
var
  MyPolygon : TDGCPolygon;
  i, Nb: Integer;
begin
  if (not FMakingPolyLineGon) then begin FLastError := format('%s.EndPolygon() without %s.BeginPolygon()', [ClassName, ClassName]); Exit; end;
  Nb := FTemporaryPolyLineGon.GetNbVertex();
  if (Nb < 3) then begin FLastError := Format('Invalid Polygone "%s", %d vertex', [FTemporaryPolyLineGon.Name, FTemporaryPolyLineGon.GetNbVertex()]); exit; end;
  try
    MyPolygon := TDGCPolygon.Create(FTemporaryPolyLineGon.IdxStyleSheet, FTemporaryPolyLineGon.Name, FTemporaryPolyLineGon.DoFlush);
    for i := 0 to Nb - 1 do MyPolygon.AddVertex(FTemporaryPolyLineGon.GetVertex(i));
    MyPolygon.SetBoundingBox();
    AddPolygon(MyPolygon, FTemporaryPolyLineGon.Name, FTemporaryPolyLineGon.DoFlush);
    FMakingPolyLineGon := false;  // avant de désarmer le flag FMakingPolyLineGon
  finally
    FreeAndNil(FTemporaryPolyLineGon);
  end;
end;

procedure TCdrDGCDrawingContext.EndCurve();
var
  i, Nb: Integer;
  MyCurve: TDGCCurve;
begin
  if (not FMakingPolyLineGon) then begin FLastError := format('%s.EndCurve() without %s.BeginCurve()', [ClassName, ClassName]); Exit; end;
  Nb := FTemporaryPolyLineGon.GetNbVertex();
  if (Nb < 2) then begin FLastError := Format('Invalid curve "%s", %d vertex', [FTemporaryPolyLineGon.Name, FTemporaryPolyLineGon.GetNbVertex()]); exit; end;
  try
    MyCurve := TDGCCurve.Create(FTemporaryPolyLineGon.IdxStyleSheet,
                                FTemporaryPolyLineGon.Name,
                                FTemporaryPolyLineGon.DoFlush,
                                tdgcBEZIER_CURVE,
                                FCurveFilled);
    for i := 0 to Nb - 1 do MyCurve.AddVertex(FTemporaryPolyLineGon.GetVertex(i));
    if (MyCurve.MakeBezierCurve(true)) then AddCurve(MyCurve, FTemporaryPolyLineGon.Name, FTemporaryPolyLineGon.DoFlush);
    FMakingPolyLineGon := false;  // avant de désarmer le flag FMakingPolyLineGon
  finally
    FreeAndNil(FTemporaryPolyLineGon);
  end;
end;

function TCdrDGCDrawingContext.AddTexte(const QIdxStyleSheet: integer;
                                        const X1, Y1: double;
                                        const QAlignment: byte;
                                        const QOrientation: double;   // ici en degrés
                                        const QText: string;
                                        const DoFlush: boolean): boolean;
var
  MyTexte: TDGCText;
begin
  MyTexte := TDGCText.Create(QIdxStyleSheet, MakeTDGCPoint2D(X1, Y1), QAlignment, QOrientation, QText);
  result  := FListOfEntities.AddElement(tdoENTITY_TEXT, 'Text', MyTexte);
end;

// Rafraichit l'affichage
procedure TCdrDGCDrawingContext.Flush();
begin
  Vue.Invalidate;
end;

function TCdrDGCDrawingContext.GetLastError(): string;
begin
  Result := FLastError;
end;

function TCdrDGCDrawingContext.GetEntite(const Idx: Integer): TDGCDessinObjet;
begin
  Result := FListOfEntities.GetElement(Idx);
end;

function  TCdrDGCDrawingContext.RemoveEntite(const Idx: integer): boolean;
begin
  result := FListOfEntities.RemoveElement(Idx);
end;

// spécifiques aux zooms et consorts
procedure TCdrDGCDrawingContext.QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
begin
  Vue.Canvas.Pen.Mode := QPM;
  Vue.Canvas.Pen.Style:= QPS;
  Vue.Canvas.Pen.Color:= QPC;
  Vue.Canvas.Pen.Width:= QPW;
end;
procedure TCdrDGCDrawingContext.Rearmer();
begin
  FAttenduSecondPoint        := False;
  QSetPen(pmCopy, psSolid, 0, clGray);
end;

//==============================================================================
// Export en SVG
procedure TCdrDGCDrawingContext.ExportSVG(const QFilename: RawByteString);
var
  FSVGCanvas: TSVGCanvas;
  Nb, i: Integer;
  MyObject: TDGCDessinObjet;
  procedure QSVGSegment(const E: TDGCSegment);
  var
    QStyleSheet: TDGCStyleSheet;
  begin
    QStyleSheet := FListOfStyles.GetElement(E.IdxStyleSheet);

    FSVGCanvas.DrawLine(QStyleSheet.Stylename, E.P1.X, E.P1.Y, E.P2.X, E.P2.Y);
  end;
  procedure QSVGRectangle(const E: TDGCRectangle);
  var
    QStyleSheet: TDGCStyleSheet;
  begin
    QStyleSheet := FListOfStyles.GetElement(E.IdxStyleSheet);

    FSVGCanvas.DrawRectangle(QStyleSheet.Stylename, E.P1.X, E.P1.Y, E.P2.X, E.P2.Y);
  end;
  procedure QSVGTriangle(const E: TDGCTriangle);
  var
    QStyleSheet: TDGCStyleSheet;
  begin
    QStyleSheet := FListOfStyles.GetElement(E.IdxStyleSheet);
    FSVGCanvas.DrawTriangle(QStyleSheet.Stylename, E.P1.X, E.P1.Y, E.P2.X, E.P2.Y, E.P3.X, E.P3.Y);
  end;

  procedure QSVGEllipse(const E: TDGCEllipse);
  var
    QStyleSheet: TDGCStyleSheet;
  begin
    QStyleSheet := FListOfStyles.GetElement(E.IdxStyleSheet);
    FSVGCanvas.DrawCircle(QStyleSheet.Stylename, E.Centre.X, E.Centre.Y, E.Rayon1, E.Rayon2);
  end;




  procedure QSVGBeginGroupe(const E: TDGCBeginGroupeSVG);
  begin
    FSVGCanvas.BeginGroupe(E.Name);
  end;
  procedure QSVGEndGroupe(const E: TDGCEndGroupeSVG);
  begin
    FSVGCanvas.EndGroupe(E.Name);
  end;
  procedure QSVGMakeCSSStylesheet(const E: TDGCStyleSheet);
  begin
    FSVGCanvas.WriteStyleLinePolygoneTexte(E.Stylename,
                                           E.PenColor, E.PenOpacity, E.PenWidthInMM, E.PenStyle,
                                           E.BrushColor, E.BrushOpacity, E.BrushStyle,
                                           E.FontName, E.FontSizeInMM, E.FontColor, E.FontOpacity, E.FontStyle,
                                           E.Description);
  end;
  procedure QSVGPolylineWithCSS(const P: TDGCPolyline);
  var
    QNbV, v: Integer;
    QStyleSheet: TDGCStyleSheet;
    QVertex: TDGCPoint2D;
  begin
    QNbV := P.GetNbVertex();
    if (0 = QNbV) then exit;
    QStyleSheet := FListOfStyles.GetElement(P.IdxStyleSheet);
    FSVGCanvas.BeginListeVertex();
      for v := 0 to QNbV - 1 do
      begin
        QVertex := P.GetVertex(v);
        FSVGCanvas.AddVertex(QVertex.X, QVertex.Y);
      end;
    FSVGCanvas.EndListeVertex();
    FSVGCanvas.DrawPolylign(QStyleSheet.Stylename, false);
  end;
  procedure QSVGPolygoneWithCSS(const P: TDGCPolygon);
  var
    QNbV, v: Integer;
    QStyleSheet: TDGCStyleSheet;
    QVertex: TDGCPoint2D;
  begin
    QNbV := P.GetNbVertex();
    if (0 = QNbV) then exit;
    QStyleSheet := FListOfStyles.GetElement(P.IdxStyleSheet);
    FSVGCanvas.BeginListeVertex();
      for v := 0 to QNbV - 1 do
      begin
        QVertex := P.GetVertex(v);
        FSVGCanvas.AddVertex(QVertex.X, QVertex.Y);
      end;
    FSVGCanvas.EndListeVertex();
    FSVGCanvas.DrawPolygon(QStyleSheet.Stylename, false);
  end;
  procedure QSVGCurveWithCSS(const P: TDGCCurve);
  var
    QNbV, v: Integer;
    QStyleSheet: TDGCStyleSheet;
    VX  : TDGCPoint2D;
    MyArc: TDGCBezierArc;
  begin

    QNbV := P.GetNbVertex();
    if (0 = QNbV) then exit;
    QStyleSheet := FListOfStyles.GetElement(P.IdxStyleSheet);
    //FSVGCanvas.BeginListeBezierArcs();
    (* version autolissée
    FSVGCanvas.BeginListeVertex();
      for v := 0 to QNbV - 1 do
      begin
        VX := P.GetVertex(v);
        FSVGCanvas.AddVertex(VX.X, VX.Y);
        (*QArc := P.GetBezierArc(v);
        FSVGCanvas.AddBezierArc(QArc.PT1.X, QArc.PT1.Y,
                                QArc.PT1.X + QArc.TangP1.X, QArc.PT1.Y + QArc.TangP1.Y,
                                QArc.PT2.X + QArc.TangP2.X, QArc.PT2.Y + QArc.TangP2.Y,
                                QArc.PT2.X, QArc.PT2.Y);

      end;
    FSVGCanvas.EndListeVertex();
    //*)
    //ShowMessage(inttostr(QNbV));
    if (P.MakeBezierCurve(true)) then
    begin
      FSVGCanvas.BeginListeBezierArcs();
        QNbV := P.GetNbArcs();
        for v := 0 to QNbV - 1 do
        begin
          MyArc := P.GetBezierArc(v);
          FSVGCanvas.AddBezierArc(MyArc.PT1.X, MyArc.PT1.Y,
                                  MyArc.PT1.X + MyArc.TangP1.X, MyArc.PT1.Y + MyArc.TangP1.Y,
                                  MyArc.PT2.X + MyArc.TangP2.X, MyArc.PT2.Y + MyArc.TangP2.Y,
                                  MyArc.PT2.X, MyArc.PT2.Y);
        end;
      FSVGCanvas.EndListeBezierArc();
      FSVGCanvas.DrawBezierCurve(QStyleSheet.Stylename, false, false);
    end;
  end;
  procedure QSVGTexte(const P: TDGCText);
  var
    QStyleSheet: TDGCStyleSheet;
    WU: Integer;
  begin
    WU := ifthen(DIEU_AU_CARRE = P.IdxStyleSheet, 0, P.IdxStyleSheet);
    QStyleSheet := FListOfStyles.GetElement(WU);
    FSVGCanvas.DrawTexte(QStyleSheet.Stylename, P.Alignment, P.Position.X, P.Position.Y, P.Orientation / 10.0, P.Text);
  end;
begin
  Nb := FListOfEntities.GetNbElements();
  if (0 = Nb) then Exit;
  FSVGCanvas := TSVGCanvas.Create;
  try
    if (not FSVGCanvas.InitializeDocument(QFilename,
                                          false,  // Dessin dans une page HTML ?
                                          'Test SVG',
                                          'Generated by ' + self.ClassName,
                                          FRegionCMini.X, FRegionCMini.Y, FRegionCMaxi.X, FRegionCMaxi.Y,
                                          nil)) then Exit;
    // envoi des feuilles de style
    FSVGCanvas.BeginStylesSection();
      Nb := FListOfStyles.GetNbElements();
      for i := 0 to Nb - 1 do QSVGMakeCSSStylesheet(FListOfStyles.GetElement(i));
    FSVGCanvas.EndStylesSection();
    // envoi des entitées depuis la liste d'affichage
    Nb := FListOfEntities.GetNbElements();
    for i := 0 to Nb - 1 do
    begin
      MyObject := FListOfEntities.GetElement(i);
      case MyObject.TypeDessinObjet of
        tdoNONE                 : nop;
        tdoENTITY_SEGMENT       : QSVGSegment(MyObject.DessinObjet as TDGCSegment);
        tdoENTITY_INFINITE_LINE : nop;
        tdoENTITY_RECTANGLE     : QSVGRectangle(MyObject.DessinObjet as TDGCRectangle);
        tdoENTITY_ELLIPSE       : QSVGEllipse(MyObject.DessinObjet as TDGCEllipse);
        tdoENTITY_TRIANGLE      : QSVGTriangle(MyObject.DessinObjet as TDGCTriangle);
        tdoENTITY_POLYLINE      : QSVGPolylineWithCSS(MyObject.DessinObjet as TDGCPolyline);
        tdoENTITY_POLYGONE      : QSVGPolygoneWithCSS(MyObject.DessinObjet as TDGCPolygon);
        tdoENTITY_CURVE         : QSVGCurveWithCSS(MyObject.DessinObjet as TDGCCurve);
        tdoENTITY_TEXT          : QSVGTexte(MyObject.DessinObjet as TDGCText);
        tdoCMD_SET_PEN          : nop;
        tdoCMD_SET_BRUSH        : nop;
        tdoCMD_SET_FONT         : nop;
        tdoCMD_SVG_BEGIN_GROUPE : QSVGBeginGroupe(MyObject.DessinObjet as TDGCBeginGroupeSVG);
        tdoCMD_SVG_END_GROUPE   : QSVGEndGroupe(MyObject.DessinObjet as TDGCEndGroupeSVG);
      end;
    end;
    // finaliser
    FSVGCanvas.FinalizeDocument();
  finally
    FreeAndNil(FSVGCanvas);
  end;

end;



end.


