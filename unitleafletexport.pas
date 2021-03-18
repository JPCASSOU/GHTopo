unit UnitLeafletExport;

// 08/04/2020: Point de contrôle temporel (contrôle de version)
// A copier vers GHCaveDraw
{$mode delphi}
{$DEFINE POUR_ALAIN_DOLE}
{.$UNDEF POUR_ALAIN_DOLE}

interface

uses
  Classes, SysUtils, Graphics;
const // Ne pas définir de type énuméré afin d'éviter une dépendance avec GHTopo
  OSM_MARKER_STYLE_DEFAULT = 0;
  OSM_MARKER_STYLE_CIRCLE  = 1;
  OSM_MARKER_STYLE_SQUARE  = 2;
  OSM_MARKER_STYLE_DELTA   = 3;
  OSM_MARKER_STYLE_NABLA   = 4;
  OSM_MARKER_STYLE_LOSANGE = 5;
type TLFStringArray = array of string;
type TOSMLayer = record
  LayerIsAMap     : boolean;
  LayerDisplayed  : boolean;
  LayerVarName    : string;
  LayerTitle      : string;
  LayerAttribution: string;
  LayerURL        : string;
  LayerMarkerStyle: integer;
  LayerMarkerSize : double;
  LayerMarkerOpacity: double;
  LayerMarkerColor: TColor;
end;

const
  JS_TMARKER_CLASSNAME           = 'TJSMarker';
  JS_TMARKER_FIELD_NativeCoordoX = 'NativeCoordoX';
  JS_TMARKER_FIELD_NativeCoordoY = 'NativeCoordoY';
  JS_TMARKER_FIELD_NativeCoordoZ = 'NativeCoordoZ';
  JS_TMARKER_ARG_LIST            = 'QLayerName,  Q' + JS_TMARKER_FIELD_NativeCoordoX +
                                             ',  Q' + JS_TMARKER_FIELD_NativeCoordoY +
                                             ',  Q' + JS_TMARKER_FIELD_NativeCoordoZ +
                                             ',  QLat, QLon, QEtiquette, QDesignation, QDescription, QObservations, QTypeMarker, QSize, QLineColor, QFillColor, QFillOpacity, QPhoto';
  JS_TMARKERLIST_CLASSNAME  = 'TListeMarkers';
  JS_TMARKERLIST_INSTANCE   = 'FListeMarkers';
  JS_TMARKERLIST_METHOD_FindMarkerByEtiquette  = 'FindMarkerByEtiquette';
  JS_MARKER_LOCAL_VARNAME   = 'MyMarker';

  // accès aux éléments de la page
  NAMEREF_TITRE                       = 'Titre';
  NAMEREF_PANNEAU_LATERAL             = 'PanneauLateral';
  NAMEREF_PANNEAU_SEARCH              = 'PanneauRecherche';
  NAMEREF_PANNEAU_PICK_COORDONNEES    = 'PanneauPickCoordonnees';
  NAMEREF_MAP                         = 'MyMap';


  JS_DOCUMENT_FORM_SEARCH_NAME                = 'frmSearchNode';
  JS_DOCUMENT_FORM_LISTE_NAME                 = 'frmListeNoeuds';
  JS_DOCUMENT_FORM_PICK_COORDS                = 'frmPickCoords';
  JS_DOCUMENT_FORM_editPickCoords             = 'editPickCoords';
  JS_DOCUMENT_LSBMARKER_NAME                  = 'lsbMarkers';
  JS_DOCUMENT_FORM_editFindWhat               = 'editFindWhat';
  JS_DOCUMENT_FORM_lbCoordinates              = 'lbCoordinates';
  JS_DOCUMENT_FORM_btnSearch                  = 'btnFindWhat';
  JS_DOCUMENT_FORM_btnSearch_ProcOnSubmit     = 'FindStationByEtiquette';

  JS_DOCUMENT_FORM_lsbMarkers_Name         = 'document.' + JS_DOCUMENT_FORM_LISTE_NAME + '.' + JS_DOCUMENT_LSBMARKER_NAME;
  JS_DOCUMENT_FORM_lsbMarkers_ProcOnSelect = 'CentrerCarteSurItem';
  // Callbacks de la carte
  JS_MAP_CALLBACK_MapOnClick               = 'MyMapOnClick';
  JS_MAP_CALLBACK_MapOnMouseMove           = 'MyMapOnMouseMove';
const
  JS_PREFIX_OF_TYPE_MARKER           = 'TypeMarkerOf';
  JS_PREFIX_OF_SIZE_MARKER           = 'SizeMarkerOf';
  JS_PREFIX_OF_COLOR_VAR_MARKER      = 'ColorMarkerOf';
  JS_PREFIX_OF_OPACITY_VAR_MARKER    = 'OpacityMarkerOf';
  JS_VAR_LAYER_OPACITY               = 'QLayerOpacity';

//*******************************************************************************
type

{ TListeOSMLayers }

 TListeOSMLayers = class(TList)
  private
    function RemoveElement(const Idx: integer): boolean;
  public

    procedure ClearListe();
    procedure AddElement(const L: TOSMLayer);
    function  GetElement(const Idx: integer): TOSMLayer;
    procedure PutElement(const Idx: integer; const L: TOSMLayer);
    function  GetNbElements(): integer;
    function  GetNbTileMaps(): integer;
    function  GetNbOverlays(): integer;
end;

//******************************************************************************
// Génération du document-programme HTML incorporant une carte OSM
type

{ TLeafletExport }

  TLeafletExport = class
  strict private
    FP                 : TextFile;
    FCurrJSFuncName    : string;
    FListeLayers       : TListeOSMLayers;  // contient les couches
    FCurrentIndentation: string;
    procedure BeginBody(const OnLoadProc: string);
    procedure BeginCSS_Styles();
    procedure BeginDiv(const DivID: string);
    procedure BeginForm(const FormID: string);
    procedure EndDiv();
    procedure BeginHEAD(const DocTitle: string);
    procedure BeginScriptSection();
    procedure EndBody();
    procedure EndCSS_Styles();
    procedure EndForm();

    procedure EndHEAD();
    procedure EndScriptSection();
    procedure WriteLine(const S: string);
    procedure WrtLinFmt(const Fmt: String; const Args: array of const);
    procedure IndentLine(const ResetIndent: boolean = false);
    procedure UnIndentLine(const ResetIndent: boolean = false);
    procedure BeginCSSClass(const QCSSClassName: string);
    procedure EndCSSClass();
    procedure AddLayer(const L: TOSMLayer); overload;
    // pour écriture directe de code JS (à des fins de test notamment)
    procedure BeginJSVerbatim(const Desc: string);
    procedure JSVerbatim(const L: string);
    procedure EndJSVerbatim();


    procedure BeginJSFunction(const JSProcname: string; const Args: string);
    procedure EndJSFunction();
    function  HasAlreadyLayerVarName(const L: string): boolean;

    procedure jsIF(const QCondition: string; const QTag: string='');
    procedure jsELSE(const QTag: string = '');
    procedure jsENDIF(const QTag: string = '');

    procedure jsFOR(const QVarCounter, QVarNb: string; const QStart: integer=0; const QTag: string = '');
    procedure jsNEXT(const QVarCounter: string; const QTag: string='');

    procedure jsWHILE(const QCondition: string; const QTag: string='');
    procedure jsWEND(const QTag: string = '');

    procedure jsSELECT_CASE(const QSelecteur: string; const QTag: string = '');
    procedure jsCASE(const QItem: integer; const QTag: string='');
    procedure jsBREAK();
    procedure jsCASE_ELSE(const QTag: string='');
    procedure jsEND_SELECT(const QTag: string='');
  private
    FUseLocalJSLibraries: boolean;
    FNbMarkers         : integer;
    FDocTitle          : string;
    FMapWidthInPercent : integer;          // en pourcentage de la largeur d'écran
    FMapHeightinPixels : integer;          // en pixels
    FCentroideLat, FCentroideLon : Double; // la carte sera centrée sur ces coordonnées
    FCurrentLineWidth    : double;      // styles courants
    FCurrentLineColor    : TColor;

    FCurrentLineOpacity  : double;
    FCurrentFillColor    : TColor;

    FCurrentFillOpacity  : double;
    FCurrentTagString    : string;
    FCurrentObjectName   : string;

    procedure DefineVarLayer(const L: TOSMLayer);
    procedure WriteSectionSeparator();
    procedure WriteProcSeparator();

  public
    function RealEscapeString(const S: string): string;
    function  Initialiser(const QFilename: string;
                          const QDocTitle: string;
                          const QMapWidthInPercent, QMapHeightinPixels: integer;
                          const QCentroideLat, QCentroideLon: Double;
                          const UseLocalJSLibraries: boolean): boolean;

    procedure Finaliser();
    procedure BeginHTML();
    procedure EndHTML();
    // construction du document-programme, déclaration des fonctions JavaScript
    procedure WriteHeader();
    procedure WriteFooter();


    // gestion des couches
    procedure AddLayer(const QLayerIsAMap, QLayerDisplayed: boolean;
                       const QLayerName : string;
                       const QLayerTitle: string;
                       const QLayerAttribution: string;
                       const QURL: String;
                       const QLayerMarkerStyle: integer;
                       const QLayerMarkerSize: double;
                       const QLayerMarkerColor: TColor;
                       const QLayerMarkerOpacity: double); overload;
    function  GetLayer(const Idx: integer): TOSMLayer;
    function  GetNbLayers(): integer;
    // feuilles de style
    procedure DefineStylePoly(const StyleName: string; const LineWidth: double; const LineColor, FillColor: TColor; const LineOpacity, FillOpacity: byte);
    // objets graphiques: marqueurs, polylignes et polygones
    procedure AddMarker( const QLayerName: string;
                         const QNativeCoordX, QNativeCoordY, QNativeCoordZ, Lat, Lon: double;
                         const QEtiquette, QDesignation, QDescription, QObservations, QPhoto: string);
    procedure FlushAllMarkers();
    procedure BeginPolygon(const QLayerName: string; const QPolyName: string; const QStyleURL: string);
    procedure AddVertex(const Lat, Lon, Alt: double; const IsLast: boolean);
    procedure EndPolygon();
    procedure BeginPolyline(const QLayerName: string; const QPolyName: string; const QStyleURL: string);
    procedure EndPolyline();
    // utilisé pour structurer le document
    procedure BeginConditionalSection(const B: boolean); //; const VariableName: string);
    procedure EndConditionalSection();
end;

implementation
uses
  DGCDummyUnit;
const
  NB_LAYERS_FOND_CARTE    = 2;

  URL_LEAFLET_LIBRARY        = 'https://unpkg.com/leaflet';
  //LEAFLET_VERSION_NUMBER     = '1.3.1';
  LEAFLET_VERSION_NUMBER     = '1.6.0';

  URL_LEAFLET_JS             = URL_LEAFLET_LIBRARY + '@' + LEAFLET_VERSION_NUMBER + '/dist/leaflet.js';  //'http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.js';
  URL_LEAFLET_CSS            = URL_LEAFLET_LIBRARY + '@' + LEAFLET_VERSION_NUMBER + '/dist/leaflet.css'; //'http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.css';
  URL_LOCAL_LEAFLET_JS       = './Leaflet/leaflet.js';
  URL_LOCAL_LEAFLET_CSS      = './Leaflet/leaflet.css';

  MIME_TEXT_JAVASCRIPT       = 'text/javascript';

  OSM_MAPNIK_VAR_LAYER       = 'LayerOsmMapnik';
  OSM_MAPNIK_MAP_TITLE       = 'OpenStreetMap';
  OSM_MAPNIK_URL_TILES       = 'https://{s}.tile.osm.org/{z}/{x}/{y}.png';
  OSM_MAPNIK_ATTRIBUTION     = '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors';

  OSM_TOPO_VAR_LAYER         = 'LayerOsmTopo';
  OSM_TOPO_MAP_TITLE         = 'OpenTopoMap';
  OSM_TOPO_URL_TILES         = 'https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png';
  OSM_TOPO_ATTRIBUTION       = 'Map data: &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, ' +
                               '<a href="http://viewfinderpanoramas.org">SRTM</a> | ' +
                               'Map style: &copy; <a href="https://opentopomap.org">OpenTopoMap</a> '+
                               '(<a href="https://creativecommons.org/licenses/by-sa/3.0/">CC-BY-SA</a>)';

  GOOGLE_SAT_VAR_LAYER       = 'LayerGoogleSat';
  GOOGLE_SAT_TITLE           = 'Google Satellite';
  GOOGLE_SAT_URL_TILES       = 'https://www.google.com/maps/vt?lyrs=s@189&gl=cn&x={x}&y={y}&z={z}';
  GOOGLE_SAT_ATTRIBUTION     = 'Google Satellite';

  DUMMY_MAP_VAR_LAYER        = 'LayerDummyMap';
  DUMMY_MAP_TITLE            = 'Pas de carte';
  DUMMY_MAP_TILES            = '';
  DUMMY_MAP_ATTRIBUTION      = 'Blank map';



  JS_FUNCTION_INITIALISER    = 'Initialiser';

  JS_FUNCTION_CREATE_MARKER  = 'CreateMarker';
  JS_FUNCTION_DRAW_MARKER    = 'DrawMarker';

  JS_FUNCTION_CREATE_POLYGON   = 'CreatePolygon';
  JS_FUNCTION_CREATE_POLYLINE  = 'CreatePolyline';


// pour se rendre indépendant des unités de GHTopo
function QFormatterNombreReel(const Value: double; const NbDecs: integer = 2): string;
var
  EWE: String;
begin
  EWE := Format('%%.%df', [NbDecs]);
  // remplacer les points par des virgules
  Result := Format(EWE, [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
end;
function QColorToHTMLColor(const Couleur: TColor): string;
var
  R, V, B: byte;
begin
  R := Red(Couleur);
  V := Green(Couleur);
  B := Blue(Couleur);
  Result := Format('#%.2X%.2X%.2X', [R, V, B]);
end;
function QIIF(const Condition: boolean; const V1, V2: string): string; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function QIIF(const Condition: boolean; const V1, V2: Integer): Integer; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function QIIF(const Condition: boolean; const V1, V2: Double): Double; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;

function QColorToHTML(const C: TColor): string;
begin
  Result := Format('"#%.2X%.2X%.2X"', [Red(C), Green(C), Blue(C)]);
end;

{ TListeOSMLayers }
procedure TListeOSMLayers.ClearListe();
var
  i, n: Integer;
begin
  n := self.Count;
  if (n > 0) then
  for i := Count-1 downto 0 Do
  begin
    if (self.Items[i] <> Nil) then Dispose(self.Items[i]); // Libération
    self.Delete(i);                                        // Suppression de l'élément
  end;
end;

procedure TListeOSMLayers.AddElement(const L: TOSMLayer);
var
  pE: ^TOSMLayer;
begin
  New(pE);
  pE^ := L;
  self.Add(pE);
end;
function TListeOSMLayers.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;


function TListeOSMLayers.GetElement(const Idx: integer): TOSMLayer;
begin

  Result := TOSMLayer(Items[Idx]^);
end;

procedure TListeOSMLayers.PutElement(const Idx: integer; const L: TOSMLayer);
begin
  TOSMLayer(Items[Idx]^) := L;
end;


function TListeOSMLayers.GetNbElements(): integer;
begin
  Result := self.Count;
end;

function TListeOSMLayers.GetNbTileMaps(): integer;
var
  Nb, i: Integer;
  L: TOSMLayer;
begin
  result := 0;
  Nb := self.Count;
  if (0 = Nb) then exit;
  for i := 0 to Nb - 1 do
  begin
    L := GetElement(i);
    if (L.LayerIsAMap) then Result += 1;
  end;
end;

function TListeOSMLayers.GetNbOverlays(): integer;
var
  Nb, i: Integer;
  L: TOSMLayer;
begin
  result := 0;
  Nb := self.Count;
  if (0 = Nb) then exit;
  for i := 0 to Nb - 1 do
  begin
    L := GetElement(i);
    if (not L.LayerIsAMap) then Result += 1;
  end;
end;

//==============================================================================
// TLeafletExport
function TLeafletExport.RealEscapeString(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;'  , [rfReplaceAll]);     // esperluète en premier sinon toutes les entités suivantes vont être cannabisées ;-)
  Result := StringReplace(S, '"', '&quot;' , [rfReplaceAll]);
  Result := StringReplace(S, '<', '&lt;'   , [rfReplaceAll]);
  Result := StringReplace(S, '>', '&gt;'   , [rfReplaceAll]);
end;

procedure TLeafletExport.WriteLine(const S: string);
begin
  Writeln(FP, FCurrentIndentation + S);
end;
procedure TLeafletExport.WrtLinFmt(const Fmt: String; const Args: array of const);
begin
  WriteLn(FP, Format(Fmt, Args));
end;
procedure TLeafletExport.IndentLine(const ResetIndent: boolean = false);
var
  n: Integer;
begin
  n := length(FCurrentIndentation) + 2;
  if (ResetIndent) then n := 0;
  FCurrentIndentation := StringOfChar(' ', n);
end;

procedure TLeafletExport.UnIndentLine(const ResetIndent: boolean = false);
var
  n: Integer;
begin
  n := length(FCurrentIndentation) - 2;
  if (n < 0) then n := 0;
  if (ResetIndent) then n := 0;
  FCurrentIndentation := StringOfChar(' ', n);
end;

procedure TLeafletExport.BeginCSSClass(const QCSSClassName: string);
begin
  WrtLinFmt(' #%s{', [QCSSClassName]);
  IndentLine();
end;

procedure TLeafletExport.EndCSSClass();
begin
  WriteLine(' }');
  UnIndentLine();
end;

procedure TLeafletExport.jsFOR(const QVarCounter, QVarNb: string; const QStart: integer = 0; const QTag: string = '');
begin
  WrtLinFmt('for (var %s = %d; %s < %s; %s++) // -- FOR %s', [QVarCounter, QStart, QVarCounter, QVarNb, QVarCounter, QTag]);
  WriteLine('{');
  IndentLine();
end;
procedure TLeafletExport.jsNEXT(const QVarCounter: string; const QTag: string = '');
begin
  UnIndentLine();
  WrtLinFmt('} // -- NEXT %s ... %s', [QVarCounter, QTag]);
end;
procedure TLeafletExport.jsWHILE(const QCondition: string; const QTag: string = '');
begin
  WrtLinFmt('while (%s) // -- WHILE %s', [QCondition, QTag]);
  WriteLine('{');
  IndentLine();
end;
procedure TLeafletExport.jsWEND(const QTag: string = '');
begin
  UnIndentLine();
  WrtLinFmt('} // -- WEND %s', [QTag]);
end;
procedure TLeafletExport.jsIF(const QCondition: string; const QTag: string = '');
begin
  WrtLinFmt('if (%s) // %s', [QCondition, QTag]);
  WriteLine('{');
  IndentLine();
end;
procedure TLeafletExport.jsELSE(const QTag: string = '');
begin
  UnIndentLine();
  WrtLinFmt('} else { // %s', [QTag]);
  IndentLine();
end;
procedure TLeafletExport.jsENDIF(const QTag: string = '');
begin
  UnIndentLine();
  WrtLinFmt('} // %s', [QTag]);
end;

procedure TLeafletExport.jsSELECT_CASE(const QSelecteur: string; const QTag: string = '');
begin
  WrtLinFmt('switch(%s) { // %s', [QSelecteur, QTag]);
  IndentLine();
end;
procedure TLeafletExport.jsCASE(const QItem: integer; const QTag: string = '');
begin
  WrtLinFmt('case %d: // %s', [QItem, QTag]);
end;
procedure TLeafletExport.jsBREAK();
begin
  WriteLine('break;');
end;
procedure TLeafletExport.jsCASE_ELSE(const QTag: string = '');
begin
  WrtLinFmt('default: // %s', [QTag]);
end;
procedure TLeafletExport.jsEND_SELECT(const QTag: string = '');
begin
  UnIndentLine();
  WrtLinFmt('} // switch %s', [QTag]);
end;

function TLeafletExport.GetNbLayers(): integer;
begin
  Result := FListeLayers.GetNbElements();
end;
function TLeafletExport.Initialiser(const QFilename: string;
                                    const QDocTitle: string;
                                    const QMapWidthInPercent, QMapHeightinPixels: integer;
                                    const QCentroideLat, QCentroideLon: Double;
                                    const UseLocalJSLibraries: boolean): boolean;
var
  i: Integer;
  QLayerMapOpacity: Double;
begin
  Result := false;
  FCurrentIndentation := '';
  FCurrJSFuncName     := '';
  FNbMarkers          := 0;
  FUseLocalJSLibraries:= UseLocalJSLibraries;
  FMapWidthInPercent  := QMapWidthInPercent;
  FMapHeightinPixels  := QMapHeightinPixels;

  FCurrentLineColor   := clRed;
  FCurrentLineOpacity := 255;
  FCurrentFillColor   := clRed;
  FCurrentFillOpacity := 128;
  // Liste des couches
  FListeLayers        := TListeOSMLayers.Create;
  FListeLayers.ClearListe();
  QLayerMapOpacity    := FCurrentFillOpacity / 256.0;

  AssignFile(FP, QFileName);
  try
    ReWrite(fp);
    AddLayer(True, True, OSM_MAPNIK_VAR_LAYER      , OSM_MAPNIK_MAP_TITLE    , OSM_MAPNIK_ATTRIBUTION         , OSM_MAPNIK_URL_TILES    , OSM_MARKER_STYLE_DEFAULT, 10.00, clRed   , QLayerMapOpacity);
    AddLayer(True, True, OSM_TOPO_VAR_LAYER        , OSM_TOPO_MAP_TITLE      , OSM_TOPO_ATTRIBUTION           , OSM_TOPO_URL_TILES      , OSM_MARKER_STYLE_DEFAULT, 10.00, clGreen , QLayerMapOpacity);
    AddLayer(True, True, GOOGLE_SAT_VAR_LAYER      , GOOGLE_SAT_TITLE        , GOOGLE_SAT_ATTRIBUTION         , GOOGLE_SAT_URL_TILES    , OSM_MARKER_STYLE_DEFAULT, 10.00, clBlue  , QLayerMapOpacity);
    AddLayer(True, True, DUMMY_MAP_VAR_LAYER       , DUMMY_MAP_TITLE         , DUMMY_MAP_ATTRIBUTION          , DUMMY_MAP_TILES         , OSM_MARKER_STYLE_DEFAULT, 10.00, clBlue  , QLayerMapOpacity);
    result := true;
  except
    Exit(false);
  end;
  FDocTitle        := QDocTitle;
  FCentroideLat    := QCentroideLat;
  FCentroideLon    := QCentroideLon;
end;



procedure TLeafletExport.Finaliser();
begin
  try
    CloseFile(FP);
    FListeLayers.ClearListe();
  finally
    FreeAndNil(FListeLayers);
  end;
end;

procedure TLeafletExport.BeginHTML();
begin
  WriteLine('<!DOCTYPE html>');
  WrtLinFmt('<!-- Generated %s -->', [DateTimeToStr(Now)]);
  WriteLine('<HTML>');
end;

procedure TLeafletExport.EndHTML();
begin
  WriteLine('</HTML>');
end;

procedure TLeafletExport.BeginCSS_Styles();
begin
   WriteLine('<!-- Styles -->');
   WriteLine('  <STYLE>');
end;
procedure TLeafletExport.EndCSS_Styles();
begin
  WriteLine('  </STYLE>');
end;
procedure TLeafletExport.BeginHEAD(const DocTitle: string);
begin
  WriteLine('  <HEAD>');
  WrtLinFmt('    <TITLE>%s</TITLE>', [DocTitle]);
  WrtLinFmt('    <META charset="%s" />', ['utf-8']);
  WrtLinFmt('    <META name="%s" content="width=%s, initial-scale=%.1f">', ['viewport', 'device-width', 1.0]);
end;
procedure TLeafletExport.EndHEAD();
begin
  WriteLine('  </HEAD>');
end;
procedure TLeafletExport.BeginScriptSection();
const
  FMT_MIME_SCRIPT = '  <script type="%s" src="%s"></script>';
begin
  WriteLine('<!-- Scripts JS de la Leaflet -->');
  if (FUseLocalJSLibraries) then WrtLinFmt(FMT_MIME_SCRIPT, [MIME_TEXT_JAVASCRIPT, URL_LOCAL_LEAFLET_JS])
                            else WrtLinFmt(FMT_MIME_SCRIPT, [MIME_TEXT_JAVASCRIPT, URL_LEAFLET_JS]);
  WriteLine('<!-- Scripts JS internes -->');
  WrtLinFmt('  <SCRIPT type="%s">', [MIME_TEXT_JAVASCRIPT]);
end;
procedure TLeafletExport.EndScriptSection();
begin
  WriteLine('  </SCRIPT>');
end;
procedure TLeafletExport.BeginBody(const OnLoadProc: string);
begin
  WriteLine('<!-- Mise en page du document -->');
  WrtLinFmt('<BODY onLoad="%s()">', [OnLoadProc]);
end;
procedure TLeafletExport.EndBody();
begin
  WriteLine('</BODY>');
end;

procedure TLeafletExport.BeginDiv(const DivID: string);
begin
  WrtLinFmt('  <div id="%s">', [DivID]);
end;
procedure TLeafletExport.EndDiv();
begin
  WriteLine('  </div>');
end;
procedure TLeafletExport.BeginForm(const FormID: string);
begin
  WrtLinFmt('  <form name="%s">', [FormID]);
end;
procedure TLeafletExport.EndForm();
begin
  WriteLine('  </form>');
end;
procedure TLeafletExport.BeginJSFunction(const JSProcname: string; const Args: string);
begin
  FCurrJSFuncName := JSProcname;
  WrtLinFmt('function %s(%s)', [FCurrJSFuncName, Args]);
  WriteLine('{');
  IndentLine();
end;
procedure TLeafletExport.EndJSFunction();
begin
  WrtLinFmt('} // %s', [FCurrJSFuncName]);
  UnIndentLine();
  WriteProcSeparator();
end;

procedure TLeafletExport.DefineVarLayer(const L: TOSMLayer);
const
  FMT_VAR_BOOL_DISP_LAYER = 'var DoDisplay%s = %s;';
begin
  if (L.LayerIsAMap) then
  begin
    WrtLinFmt('var %s =  L.tileLayer(''%s'',' , [L.LayerVarName, L.LayerURL]);
    WriteLine('{');
    IndentLine();
      WrtLinFmt('attribution: ''%s'',', [L.LayerAttribution]);
      WrtLinFmt('maxZoom: %d'         , [21]);
      UnIndentLine();
    WriteLine('});');
  end else
  begin
     WrtLinFmt('var %s = new L.LayerGroup(); // %s', [L.LayerVarName, L.LayerTitle]);
  end;
  WrtLinFmt(FMT_VAR_BOOL_DISP_LAYER, [L.LayerVarName, BoolToStr(L.LayerDisplayed, 'true', 'false')]);
  WrtLinFmt('%s.addLayer(%s);', [NAMEREF_MAP, L.LayerVarName]);
end;

procedure TLeafletExport.WriteSectionSeparator();
begin
  WriteLine('//' + StringOfChar('*', 132));
end;

procedure TLeafletExport.WriteProcSeparator();
begin
  WriteLine('//' + StringOfChar('-', 132));
end;



procedure TLeafletExport.DefineStylePoly(const StyleName: string; const LineWidth: double; const LineColor, FillColor: TColor;  const LineOpacity, FillOpacity: byte);
begin
  FCurrentLineWidth   := LineWidth;
  FCurrentLineColor   := LineColor;
  FCurrentFillColor   := FillColor;
  FCurrentLineOpacity := 1.0 * LineOpacity / 256.0;
  FCurrentFillOpacity := 1.0 * FillOpacity / 256.0;
end;

// function CreateMarker(QMyMap, QL, QLayerName, QLat, QLon, QAlt, QEtiquette, QDesignation, QDescription, QObservations, QSize, QLineColor, QFillColor, QFillOpacity)
procedure TLeafletExport.AddMarker(const QLayerName: string;
                                   const QNativeCoordX, QNativeCoordY, QNativeCoordZ, Lat, Lon: double;
                                   const QEtiquette, QDesignation, QDescription, QObservations, QPhoto: string);

begin
  // QLayerName,  QNativeCoordX, QNativeCoordY, QNativeCoordZ, QLat, QLon, QEtiquette, QDesignation, QDescription, QObservations, QTypeMarker, QSize, QLineColor, QFillColor, QFillOpacity, QPhoto'
  //         CreateMarker(1, 423278.540, 6416307.620, 14.000, 44.79071599, -0.49979704, "Entrée Foussat I.1", "Entrée Foussat I.1", "", "Coordonnées provisoires", TypeMarkerOk1, SizeMarkerOf1, ColorMarkerOf1, ColorMarkerOf1, QLayerOpacity, "1");
  //                          0   2   3   4   5   6   7   8     9     10     11  12  13  14  15  16   17

  WrtLinFmt('%s(%s, %s, %s, %s, %s, %s, "%s", "%s", "%s", "%s", %s, %s, %s, %s, %s, "%s");', [
                   JS_FUNCTION_CREATE_MARKER,                        //0                  CreateMarker(1, 423278.540, 6416307.620, 14.000, 44.79071599, -0.49979704, "1.0", "Entrée Foussat I.1", "1", "1", TypeMarkerOf1, SizeMarkerOf1, ColorMarkerOf1, ColorMarkerOf1, QLayerOpacity, "1");
                   QLayerName,                                       // 2
                   QFormatterNombreReel(QNativeCoordX, 3),           // 3
                   QFormatterNombreReel(QNativeCoordY, 3),           // 4
                   QFormatterNombreReel(QNativeCoordZ, 3),           // 5
                   QFormatterNombreReel(Lat, 8),                     // 6
                   QFormatterNombreReel(Lon, 8),                     // 7
                   RealEscapeString(QEtiquette),                     // 8
                   RealEscapeString(QDesignation),                   // 9
                   RealEscapeString(QDescription),                   // 10
                   RealEscapeString(QObservations),                  // 11
                   JS_PREFIX_OF_TYPE_MARKER + QLayerName,            // 12
                   JS_PREFIX_OF_SIZE_MARKER + QLayerName,            // 13
                   JS_PREFIX_OF_COLOR_VAR_MARKER + QLayerName,       // 14             //QColorToHTML(QLineColor),
                   JS_PREFIX_OF_COLOR_VAR_MARKER + QLayerName,       // 15   //QColorToHTML(QFillColor),
                   JS_PREFIX_OF_OPACITY_VAR_MARKER + QLayerName,     // 16
                   RealEscapeString(QPhoto)                          // 17
                   ]);
  FNbMarkers += 1;
end;
procedure TLeafletExport.FlushAllMarkers();
begin
  WriteLine('// Flushing all markers');
  WrtLinFmt('var Nb = %s.GetNbMarkers();', [JS_TMARKERLIST_INSTANCE]);
  jsFOR('i', 'Nb', 0, '');
    WrtLinFmt('var %s = %s.GetMarker(i);', [JS_MARKER_LOCAL_VARNAME, JS_TMARKERLIST_INSTANCE]);
    WrtLinFmt('DrawMarker(MyMap, L, %s);', [JS_MARKER_LOCAL_VARNAME]);
  jsNEXT('i', '');
  WriteLine('// Filling listbox');
  WrtLinFmt('%s.length = Nb;', [JS_DOCUMENT_FORM_lsbMarkers_Name]);
  jsFOR('i', 'Nb', 0, '');
    WrtLinFmt('var %s = %s.GetMarker(i);', [JS_MARKER_LOCAL_VARNAME, JS_TMARKERLIST_INSTANCE]);
    WrtLinFmt('%s.options[i].text = %s.Etiquette + " - " + %s.Designation;', [JS_DOCUMENT_FORM_lsbMarkers_Name, JS_MARKER_LOCAL_VARNAME, JS_MARKER_LOCAL_VARNAME]);
  jsNEXT('i', '');
end;
procedure TLeafletExport.AddVertex(const Lat, Lon, Alt: double; const IsLast: boolean);
begin
  WrtLinFmt('[%s, %s]%s', [QFormatterNombreReel(Lat, 8), QFormatterNombreReel(Lon, 8), QIIF(IsLast, '', ',')]);
end;

procedure TLeafletExport.BeginPolygon(const QLayerName: string; const QPolyName: string; const QStyleURL: string);
begin
  FCurrentTagString  := QStyleURL;
  FCurrentObjectName := QPolyName;
  WrtLinFmt('%s(%s, L, %s, "%s", "%s",  "%s", %s, [',
                   [JS_FUNCTION_CREATE_POLYGON,
                    NAMEREF_MAP,
                    QLayerName,
                    QPolyName,
                    QColorToHTMLColor(FCurrentLineColor),
                    QColorToHTMLColor(FCurrentFillColor),
                    QFormatterNombreReel(FCurrentFillOpacity)]);
  IndentLine();
end;
procedure TLeafletExport.EndPolygon();
begin
  UnIndentLine();
  WriteLine(']);');
end;
procedure TLeafletExport.BeginPolyline(const QLayerName: string; const QPolyName: string; const QStyleURL: string);
begin
  FCurrentTagString  := QStyleURL;
  FCurrentObjectName := QPolyName;
  WrtLinFmt('      %s(%s, L, %s, "%s", "%s",  "%s", %s, [',
                   [JS_FUNCTION_CREATE_POLYLINE,
                    NAMEREF_MAP,
                    QLayerName,
                    QPolyName,
                    QColorToHTMLColor(FCurrentLineColor),
                    QColorToHTMLColor(FCurrentFillColor),
                    QFormatterNombreReel(FCurrentFillOpacity)]);
  IndentLine();
end;
procedure TLeafletExport.EndPolyline();
begin
  UnIndentLine();
  WriteLine('      ]);');
end;
procedure TLeafletExport.WriteHeader();
const
  FMT001 = '       ''%s'': %s%s';
  VAR_baseLayers = 'baseLayers';
  VAR_overlays   = 'overlays';
  HAUTEUR_TITRE_IN_PIXELS = 40;
  HAUTEUR_FORM_SEARCH_IN_PIXELS = 70;
  HAUTEUR_PNL_COORDS_IN_PIXELS  = 100;
  VAR_QDLR = 'QDemiLargeurRectangle';
  VAR_QDHR = 'QDemiHauteurRectangle';
var
  URL_OSM_Contributors, WU: String;
  EWE: TLFStringArray;

  procedure QDefineAllVarLayers();
  var
    i: Integer;
  begin
    WrtLinFmt('       // Define %d layers', [GetNbLayers()]);
    for i := 0 to GetNbLayers() - 1 do DefineVarLayer(GetLayer(i));
  end;

  procedure QDefineColorsOfLayers();
  var
    i, QNbLayers: Integer;
    QLayer: TOSMLayer;
  begin
    QNbLayers := GetNbLayers();
    UnIndentLine(True);
    WrtLinFmt('// Define colors for %d layers', [QNbLayers]);
    WrtLinFmt('var %s = %s;', [JS_VAR_LAYER_OPACITY, QFormatterNombreReel(0.55)]);
    IndentLine();
    for i := 0 to QNbLayers - 1 do
    begin
      QLayer := GetLayer(i);
      WrtLinFmt('var %s = %s; ', [JS_PREFIX_OF_COLOR_VAR_MARKER  + QLayer.LayerVarName, QColorToHTML(QLayer.LayerMarkerColor)]);
    end;
    WriteLine('');
    UnIndentLine();
    WrtLinFmt('// Define opacity for %d layers', [QNbLayers]);
    IndentLine();
    for i := 0 to QNbLayers - 1 do
    begin
      QLayer := GetLayer(i);
      WrtLinFmt('var %s = %s; ', [JS_PREFIX_OF_OPACITY_VAR_MARKER +  QLayer.LayerVarName, QFormatterNombreReel(QLayer.LayerMarkerOpacity)]);
    end;
    UnIndentLine();
    WrtLinFmt('// Define styles for %d layers', [QNbLayers]);
    IndentLine();
    for i := 0 to QNbLayers - 1 do
    begin
      QLayer := GetLayer(i);
      WrtLinFmt('var %s = %d; ', [JS_PREFIX_OF_TYPE_MARKER + QLayer.LayerVarName, QLayer.LayerMarkerStyle]);
    end;
    UnIndentLine();
    WrtLinFmt('// Define symbols size for %d layers', [QNbLayers]);
    for i := 0 to QNbLayers - 1 do
    begin
      QLayer := GetLayer(i);
      WrtLinFmt('var %s = %s; ', [JS_PREFIX_OF_SIZE_MARKER + QLayer.LayerVarName, QFormatterNombreReel(QLayer.LayerMarkerSize)]);
    end;
    UnIndentLine(True);
  end;
  procedure QDefineListeLayers(const QIsMap: boolean; const NameVarListe: string);
  var
    QNbLaysOfType: integer;
    QNbC, i, q: Integer;
    QLayer: TOSMLayer;
  begin
    UnIndentLine(True);
    WrtLinFmt('var %s = {', [NameVarListe]);
    IndentLine();
    QNbLaysOfType := QIIF(QIsMap, FListeLayers.GetNbTileMaps(), FListeLayers.GetNbOverlays());
    QNbC := 0;
    for q := 0 to GetNbLayers() - 1 do
    begin
      QLayer := GetLayer(q);
      if (QIsMap = QLayer.LayerIsAMap) then
      begin
        QNbC +=1;
        WrtLinFmt('"%s": %s%s', [QLayer.LayerTitle, QLayer.LayerVarName, QIIF(QNbC < QNbLaysOfType, ', ', '')]);
      end;
    end;
    UnIndentLine();
    WriteLine('};');
  end;

  procedure QJSFuncCreatePolygonPolyline(const IsPolygonThenPolyline: Boolean);
  var
    FN: String;
  begin
     FN := QIIF(IsPolygonThenPolyline, JS_FUNCTION_CREATE_POLYGON, JS_FUNCTION_CREATE_POLYLINE);
     BeginJSFunction(FN, 'QMyMap, QL, QLayer, QNomPoly, QLineColor, QFillColor, QFillOpacity, QListeVertex');
      WrtLinFmt('QL.poly%s(QListeVertex, {', [QIIF(IsPolygonThenPolyline, 'gon', 'line')]);
      IndentLine();
        WriteLine('color: QLineColor,');
        WrtLinFmt('weight: %s,', [QFormatterNombreReel(QIIF(IsPolygonThenPolyline, 0.00, FCurrentLineWidth), 2)]);
        WriteLine('opacity: 1.00,');
        WriteLine('fillColor: QFillColor,');
        WriteLine('fillOpacity: QFillOpacity');
      UnIndentLine();
      WriteLine('}).bindPopup(QNomPoly).addTo(QLayer);');
     EndJSFunction();
  end;

  procedure QJSFuncCreateMarker();
  begin
    WriteLine('    // Creation de marqueur');
    BeginJSFunction(JS_FUNCTION_CREATE_MARKER, JS_TMARKER_ARG_LIST);
      WrtLinFmt('%s.AddMarker(%s)', [JS_TMARKERLIST_INSTANCE, JS_TMARKER_ARG_LIST]);
    EndJSFunction();
  end;
  procedure QJSFuncDrawMarker();
  const
    LOCAL_VAR_QMyMap        = 'QMyMap';
    LOCAL_VAR_QMyMarker     = 'QMyMarker';
    LOCAL_VAR_QContentPopUp = 'QContentPopUp';
    ANGLE_FOR_ONE_METER_IN_DEGREES = 8.994 * 1E-6; // = ArcTan(1/6 370 000) en degrés
    procedure QDefineColors();
    begin
      WrtLinFmt('color:%s.LineColor,'        , [LOCAL_VAR_QMyMarker]);
      WrtLinFmt('fillColor:%s.FillColor,'    , [LOCAL_VAR_QMyMarker]);
      WrtLinFmt('fillOpacity:%s.FillOpacity,', [LOCAL_VAR_QMyMarker]);
    end;
    function QBindPopUp(): string;
    begin
      result := Format('.bindPopup(%s).addTo(%s.LayerName);', [LOCAL_VAR_QContentPopUp, LOCAL_VAR_QMyMarker]);
    end;
  begin
    // création de marqueurs
    WriteLine('    // Dessin de marqueur');
    BeginJSFunction(JS_FUNCTION_DRAW_MARKER, Format('%s, QL, %s', [LOCAL_VAR_QMyMap, LOCAL_VAR_QMyMarker]));
      WrtLinFmt('var %s = %s * %s.Size;'       , [VAR_QDLR, QFormatterNombreReel(ANGLE_FOR_ONE_METER_IN_DEGREES, 10), LOCAL_VAR_QMyMarker]);
      WrtLinFmt('var %s = %s * %s.Size * 0.71;', [VAR_QDHR, QFormatterNombreReel(ANGLE_FOR_ONE_METER_IN_DEGREES, 10), LOCAL_VAR_QMyMarker]);
      WrtLinFmt('var %s = "";', [LOCAL_VAR_QContentPopUp]);
      jsIF(Format('%s.Photo.length > %d', [LOCAL_VAR_QMyMarker, 3]), '');
        WrtLinFmt('%s += "<IMG width=''%d'' SRC= ''" + %s.Photo + "''</IMG>";', [LOCAL_VAR_QContentPopUp, 300, LOCAL_VAR_QMyMarker]);
      jsENDIF('');
	    WrtLinFmt('%s += "<B>" + %s.Designation + "</B><BR>" +', [LOCAL_VAR_QContentPopUp, LOCAL_VAR_QMyMarker]);
      IndentLine();
  	  WrtLinFmt('%s.Etiquette +', [LOCAL_VAR_QMyMarker]);
      WriteLine('"<HR>" +');
      WrtLinFmt('"<P>" + %s.Description  + "</P>" +', [LOCAL_VAR_QMyMarker]);
      WrtLinFmt('"<P>" + %s.Observations + "</P>" +', [LOCAL_VAR_QMyMarker]);
      WriteLine('"<HR>" +');
      WrtLinFmt('"X = " + %s.%s + '            , [LOCAL_VAR_QMyMarker, JS_TMARKER_FIELD_NativeCoordoX]);
      WrtLinFmt('" - Y = " + %s.%s + '       , [LOCAL_VAR_QMyMarker, JS_TMARKER_FIELD_NativeCoordoY]);
      WrtLinFmt('" - Z = " + %s.%s;'         , [LOCAL_VAR_QMyMarker, JS_TMARKER_FIELD_NativeCoordoZ]);
      WrtLinFmt('%s += "<HR>" +', [LOCAL_VAR_QContentPopUp]);
      WrtLinFmt('"Lat: " + %s.Lat.toFixed(%d) + " - Lon: " + %s.Lon.toFixed(%d);' , [LOCAL_VAR_QMyMarker, 8, LOCAL_VAR_QMyMarker, 8]);
      UnIndentLine();
      jsSELECT_CASE(Format('%s.TypeMarker', [LOCAL_VAR_QMyMarker]), '');
        jsCASE(0, 'defaut');
        jsCASE(1, 'cercle');
          IndentLine();
            WrtLinFmt('QL.circle([%s.Lat, %s.Lon], {', [LOCAL_VAR_QMyMarker, LOCAL_VAR_QMyMarker]);
            QDefineColors();
            WrtLinFmt('radius:%s.Size', [LOCAL_VAR_QMyMarker]);
            WriteLine('})' + QBindPopUp());
            jsBREAK();
          UnIndentLine();

        jsCASE(2, 'carré');
          IndentLine();
            WrtLinFmt('QL.rectangle([[%s.Lat - %s, %s.Lon - %s], [%s.Lat + %s, %s.Lon + %s * 0.71]], {',
                         [LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker, VAR_QDLR,
                          LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker, VAR_QDLR]);
            QDefineColors();
            WriteLine('})' + QBindPopUp());
            jsBREAK();
          UnIndentLine();
        jsCASE(3, 'triangle');
          IndentLine();
            WrtLinFmt('QL.polygon([[%s.Lat + %s, %s.Lon], [%s.Lat - 0.5 * %s, %s.Lon - %s], [%s.Lat - 0.5 * %s, %s.Lon + %s]], {',
                         [LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker,
                          LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker, VAR_QDLR,
                          LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker, VAR_QDLR]);
            QDefineColors();
            WriteLine('})' + QBindPopUp());
            jsBREAK();
          UnIndentLine();
        jsCASE(4, 'nabla');
          IndentLine();
            WrtLinFmt('QL.polygon([[%s.Lat - %s, %s.Lon], [%s.Lat + 0.5 * %s, %s.Lon + %s], [%s.Lat + 0.5 * %s, %s.Lon - %s]], {',
                             [LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker,
                              LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker, VAR_QDLR,
                              LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker, VAR_QDLR]);
            QDefineColors();
            WriteLine('        })' + QBindPopUp());
            jsBREAK();
          UnIndentLine();
        jsCASE(5, 'losange');
          IndentLine();
            WrtLinFmt('QL.polygon([[%s.Lat - %s, %s.Lon], [%s.Lat,  %s.Lon + %s], [%s.Lat + %s, %s.Lon], [%s.Lat , %s.Lon - %s]], {',
                                [LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker,
                                 LOCAL_VAR_QMyMarker, LOCAL_VAR_QMyMarker, VAR_QDLR,
                                 LOCAL_VAR_QMyMarker, VAR_QDHR, LOCAL_VAR_QMyMarker,
                                 LOCAL_VAR_QMyMarker, LOCAL_VAR_QMyMarker, VAR_QDLR]);
            QDefineColors();
            WriteLine('})' + QBindPopUp());
            jsBREAK();
          UnIndentLine();

        jsCASE_ELSE('');
          IndentLine();
            WrtLinFmt('QL.marker([%s.Lat, %s.Lon])%s', [LOCAL_VAR_QMyMarker, LOCAL_VAR_QMyMarker, QBindPopUp()]);
          UnIndentLine();
        jsEND_SELECT('');
    EndJSFunction();
  end;
  procedure DeclareGlobalVariable(const varName, commentaire: string);
  begin
    WrtLinFmt('var %s; // %s', [varName, commentaire]);
  end;
  procedure DefineGlobalObject(const objname, objecttype, commentaire: string);
  begin
    WrtLinFmt('var %s = new %s; // %s', [objname, objecttype, commentaire]);
  end;
  procedure DefineClassTMarker();
    procedure MiouMiou(const Miou: string);
    begin
      WrtLinFmt('this.%s = Q%s;', [Miou, Miou]);
    end;
  begin
    BeginJSFunction(JS_TMARKER_CLASSNAME, JS_TMARKER_ARG_LIST);
      MiouMiou('LayerName');
      MiouMiou(JS_TMARKER_FIELD_NativeCoordoX);
      MiouMiou(JS_TMARKER_FIELD_NativeCoordoY);
      MiouMiou(JS_TMARKER_FIELD_NativeCoordoZ);
      MiouMiou('Lat');
      MiouMiou('Lon');
      MiouMiou('Etiquette');
      MiouMiou('Designation');
      MiouMiou('Description');
      MiouMiou('Observations');
      MiouMiou('TypeMarker');
      MiouMiou('Size');
      MiouMiou('LineColor');
      MiouMiou('FillColor');
      MiouMiou('FillOpacity');
      MiouMiou('Photo');
    EndJSFunction();
  end;
  procedure DefineClassMarkerList();
  const
    QNAME_ARG_INDEX           = 'Index';
    QNAME_FIELD_NB_ELEMENTS   = 'this.FNbElements';
    QNAME_FIELD_LISTE_MARKERS = 'this.FListeMarkers';

  begin
    UnIndentLine(True);
    WrtLinFmt('function %s() // %s', [JS_TMARKERLIST_CLASSNAME, 'Base de données locale contenant la liste des marqueurs ']);      //
    WriteLine('{');
    IndentLine();
      WrtLinFmt('%s = %d;', [QNAME_FIELD_NB_ELEMENTS, 0]);
      WrtLinFmt('%s = new Array(); // %s', [QNAME_FIELD_LISTE_MARKERS, ' (en JS, un tableau équivaut à un TList)']);
      WriteLine('this.Init = function()');
      WriteLine('{');
      WriteLine('}');
      WrtLinFmt('this.AddMarker = function(%s)', [JS_TMARKER_ARG_LIST]);
      WriteLine('{');
      WrtLinFmt('  %s[%s] = new %s(%s); ', [QNAME_FIELD_LISTE_MARKERS, QNAME_FIELD_NB_ELEMENTS, JS_TMARKER_CLASSNAME, JS_TMARKER_ARG_LIST]);
      WrtLinFmt('  %s = %s.length; ', [QNAME_FIELD_NB_ELEMENTS, QNAME_FIELD_LISTE_MARKERS]);
      WriteLine('}');

      WrtLinFmt('this.GetMarker = function(%s)', [QNAME_ARG_INDEX]);
      WriteLine('{');
      WrtLinFmt('  return %s[%s]; ', [QNAME_FIELD_LISTE_MARKERS, QNAME_ARG_INDEX]);
      WriteLine('}');
      WriteLine('this.GetNbMarkers = function()');
      WriteLine('{');
      WrtLinFmt('  return %s.length; ', [QNAME_FIELD_LISTE_MARKERS]);
      WriteLine('}');


      WrtLinFmt('this.%s = function(FindWhat)', [JS_TMARKERLIST_METHOD_FindMarkerByEtiquette]);
      WriteLine('{');
      IndentLine();
      WriteLine('var Result = -1;');
      WrtLinFmt('var Nb =  %s.length;', [QNAME_FIELD_LISTE_MARKERS]);
      WriteLine('if (Nb == 0) return -1;');
      jsFOR('i', 'Nb', 0, '');
        WrtLinFmt('var %s = %s[i];', [JS_MARKER_LOCAL_VARNAME, QNAME_FIELD_LISTE_MARKERS]);
        WriteLine('var QFindWhat = FindWhat.toLowerCase();');
        WrtLinFmt('var EWE = %s.Etiquette +', [JS_MARKER_LOCAL_VARNAME]);
        WrtLinFmt('          %s.Designation +', [JS_MARKER_LOCAL_VARNAME]);
        WrtLinFmt('          %s.Description;', [JS_MARKER_LOCAL_VARNAME]);
        WriteLine('EWE = EWE.toLowerCase();');
        WriteLine('if (EWE.indexOf(QFindWhat) >= 0) return i;');
      jsNEXT('i', '');
      WriteLine('}');
      UnIndentLine();

    WrtLinFmt('} // End of %s class', [JS_TMARKERLIST_CLASSNAME]);
    UnIndentLine();
    WriteSectionSeparator();
  end;
begin
  try
    BeginHTML();
      BeginHEAD('Report sur OpenStreetMap: ' + FDocTitle);
      if (FUseLocalJSLibraries) then WrtLinFmt('  <link rel="%s" href="%s" />', ['stylesheet', URL_LOCAL_LEAFLET_CSS])
                                else WrtLinFmt('  <link rel="%s" href="%s" />', ['stylesheet', URL_LEAFLET_CSS]);

      BeginCSS_Styles();
        WrtLinFmt('  html, body { margin:%dpx; padding:%d; }', [0, 0]);
        // CSS de la barre de titre
        BeginCSSClass(NAMEREF_TITRE);
          WrtLinFmt('%s:%s;'     , ['color', QColorToHTMLColor(clWhite)]);
          WrtLinFmt('%s:%dpx %s;', ['font', 28, 'Arial']);
          WrtLinFmt('%s:%s;'     , ['font-weight', 'bold']);
          WrtLinFmt('%s:%dpx;'   , ['height', HAUTEUR_TITRE_IN_PIXELS]);
          WrtLinFmt('%s:%s;'     , ['background', QColorToHTMLColor(clMaroon)]);
        EndCSSClass();
        // CSS de la zone de recherche
        BeginCSSClass(NAMEREF_PANNEAU_SEARCH);
          WrtLinFmt('%s:%s;'     , ['float', 'right']);
          WrtLinFmt('%s:%s;'     , ['color', QColorToHTMLColor(clBlack)]);
          WrtLinFmt('%s:%dpx %s;', ['font', 15, 'Arial']);
          WrtLinFmt('%s:%s;'     , ['font-weight', 'bold']);
          WrtLinFmt('%s:%s;'     , ['background', QColorToHTMLColor(clLime)]);
          WrtLinFmt('%s:%s;'     , ['border-style', 'none']);
          WrtLinFmt('%s:%d%%;'   , ['width', 100 - FMapWidthInPercent]);
          WrtLinFmt('%s:%dpx;'   , ['height', HAUTEUR_FORM_SEARCH_IN_PIXELS]);
        EndCSSClass();
        // CSS du menu latéral
        BeginCSSClass(NAMEREF_PANNEAU_LATERAL);
          WrtLinFmt('%s:%s;'     , ['float', 'right']);
          WrtLinFmt('%s:%s;'     , ['color', QColorToHTMLColor(clBlack)]);
          WrtLinFmt('%s:%dpx %s;', ['font', 15, 'Arial']);
          WrtLinFmt('%s:%s;'     , ['font-weight', 'bold']);
          WrtLinFmt('%s:%s;'     , ['background', QColorToHTMLColor(clAqua)]);
          WrtLinFmt('%s:%s;'     , ['border-style', 'none']);
          WrtLinFmt('%s:%d%%;'   , ['width', 100 - FMapWidthInPercent]);
          WrtLinFmt('%s:%dpx;'   , ['height', FMapHeightinPixels - HAUTEUR_TITRE_IN_PIXELS - HAUTEUR_FORM_SEARCH_IN_PIXELS - HAUTEUR_PNL_COORDS_IN_PIXELS]);
        EndCSSClass();
        // CSS du panneau de coordonnées
        BeginCSSClass(NAMEREF_PANNEAU_PICK_COORDONNEES);
          WrtLinFmt('%s:%s;'     , ['float', 'right']);
          WrtLinFmt('%s:%s;'     , ['color', QColorToHTMLColor(clBlack)]);
          WrtLinFmt('%s:%dpx %s;', ['font', 15, 'Arial']);
          WrtLinFmt('%s:%s;'     , ['font-weight', 'bold']);
          WrtLinFmt('%s:%s;'     , ['background', QColorToHTMLColor(clYellow)]);
          WrtLinFmt('%s:%s;'     , ['border-style', 'none']);
          WrtLinFmt('%s:%d%%;'   , ['width', 100 - FMapWidthInPercent]);
          WrtLinFmt('%s:%dpx;'   , ['height', HAUTEUR_PNL_COORDS_IN_PIXELS]);
        WriteLine('       }');
        // CSS de la carte
        BeginCSSClass(NAMEREF_MAP);
          WrtLinFmt('%s:%s;', ['float', 'left']);
          WrtLinFmt('%s:%d%%;', ['width', FMapWidthInPercent]);
          WrtLinFmt('%s:%dpx;', ['height', FMapHeightinPixels - HAUTEUR_TITRE_IN_PIXELS]);
          WrtLinFmt('%s:%dpx;'     , ['border', 0]);
          WrtLinFmt('%s:%dpx;'     , ['margin', 0]);
        EndCSSClass();
      EndCSS_Styles();
      // Scripts Javascript
      BeginScriptSection();
        // déclaration des variables globales
        WriteLine('    // Classe contenant les marqueurs');
        DefineClassTMarker();
        DefineClassMarkerList();
        WriteLine('    // Variables globales');


        DeclareGlobalVariable(NAMEREF_MAP, 'Pointeur sur la carte');

        DefineGlobalObject(JS_TMARKERLIST_INSTANCE, JS_TMARKERLIST_CLASSNAME, 'Liste des markers');
        {$IFDEF POUR_ALAIN_DOLE}
        // mini-converter coordonnées LT3
        WriteSectionSeparator();
        WriteLine('// Spécifique Alain DOLE, qui bosse avec un système de coordonnées datant du Moyen Age');
        BeginJSFunction('ConvertLatLonToLT3', 'QL, QLat, QLon');
          WriteLine('    var V00824 = 0.08248325676;  ');
          WriteLine('    var V06959 = 0.6959127966;  ');
          WriteLine('    var MToKM = 0.001;');
          WriteLine('    var latitude  = Math.PI * QLat / 180.0; ');
          WriteLine('    var longitude = Math.PI * QLon / 180.0;');
          WriteLine('    var lambert = 0.5 * Math.log( (1+Math.sin(latitude)) / (1-Math.sin(latitude)) ) - V00824 / 2 * Math.log( (1.0 + (V00824 * Math.sin(latitude))) / (1.0 - (V00824 * Math.sin(latitude))) );');
          WriteLine('    var R = 11947992.52 * Math.exp(-V06959 * lambert);');
          WriteLine('    var gamma = V06959 * (longitude - 0.040792344);');
          WriteLine('    // Pour renvoyer un résultat composite, créer un objet');
          WriteLine('    var Result = {');
          WriteLine('      X: MToKM * (600000.0 + R * Math.sin(gamma)),');
          WriteLine('      Y: MToKM * (3000000.0 + 6791905.085 - R * Math.cos(gamma))');
          WriteLine('    }; // Result');
          WriteLine('    return Result;');
        EndJSFunction();
        WriteLine('// Fin Spécifique Alain DOLE');
        WriteSectionSeparator();
        {$ENDIF POUR_ALAIN_DOLE}
        // création de polygones silhouettes
        QJSFuncCreatePolygonPolyline(true);         // Polygones
        QJSFuncCreatePolygonPolyline(false);        // Polylignes
        QJSFuncDrawMarker();
        QJSFuncCreateMarker();                      // Markers
        // Fonction JS d'initialisation
        UnIndentLine(True);
        WriteLine('//===============================================================================');
        WriteLine('// Initialisation');
        WrtLinFmt('function %s()', [JS_FUNCTION_INITIALISER]);
        WriteLine('{');
        IndentLine();
          // définir les couleurs des couches
          QDefineColorsOfLayers();
          WriteLine('// Define center of the map and zoom factor');
          WrtLinFmt('%s = L.map(''%s'').setView([%s, %s], %d); // La variable %s est globale',
                               [NAMEREF_MAP, NAMEREF_MAP,
                                QFormatterNombreReel(FCentroideLat, 8),
                                QFormatterNombreReel(FCentroideLon, 8),
                                15,
                                NAMEREF_MAP]);
          // Callback de la carte
          WrtLinFmt('%s.on("%s", %s);', [NAMEREF_MAP, 'click'    , JS_MAP_CALLBACK_MapOnClick]);
          WrtLinFmt('%s.on("%s", %s);', [NAMEREF_MAP, 'mousemove', JS_MAP_CALLBACK_MapOnMouseMove]);
          // Les couches
          QDefineAllVarLayers();
          // Les fonds de cartes
          QDefineListeLayers(True , VAR_baseLayers);
          QDefineListeLayers(false, VAR_overlays);
          WrtLinFmt('L.control.layers(%s, %s).addTo(%s);', [VAR_baseLayers, VAR_overlays, NAMEREF_MAP]);
          WrtLinFmt('L.control.scale().addTo(%s); // %s', [NAMEREF_MAP, 'Ajout de l''échelle']); // map.addLayer(osmLayer);
          WriteSectionSeparator();
          UnIndentLine(True);
          IndentLine();
          WriteLine('/* ***  BEGIN DATA *** */');
          Exit;
      // Pas de EndJSFunction() ici: ;
  except
  end;
end;
procedure TLeafletExport.WriteFooter();  // ok
const
  LOCAL_VAR_001    = 'QLatLon';
  LOCAL_VAR_latlng = 'latlng';
  LOCAl_VAR_EWE    = 'EWE';
begin
        UnIndentLine(True);
        IndentLine();
        WriteLine('/* ***  END DATA *** */');
        UnIndentLine();
        WrtLinFmt('} // **** End of %s function', [JS_FUNCTION_INITIALISER]);
        UnIndentLine(True);
        WriteSectionSeparator();
        WriteLine('// Callback functions');
        // Centrer sur point
        BeginJSFunction('CentrerSurPoint', 'QLat, QLon, QZoom');
	        WrtLinFmt('%s.setView([QLat, QLon], QZoom);', [NAMEREF_MAP]);
        EndJSFunction();
        BeginJSFunction(JS_DOCUMENT_FORM_lsbMarkers_ProcOnSelect, '');
          WrtLinFmt('var QIdx = %s.selectedIndex;', [JS_DOCUMENT_FORM_lsbMarkers_Name]);
          WrtLinFmt('var %s = %s.GetMarker(QIdx);', [JS_MARKER_LOCAL_VARNAME, JS_TMARKERLIST_INSTANCE]);
          WrtLinFmt('CentrerSurPoint(%s.Lat, %s.Lon, %s.getZoom());', [JS_MARKER_LOCAL_VARNAME, JS_MARKER_LOCAL_VARNAME, NAMEREF_MAP]);
          WrtLinFmt('var %s = L.latLng(%s.Lat, %s.Lon);', [LOCAL_VAR_latlng, JS_MARKER_LOCAL_VARNAME, JS_MARKER_LOCAL_VARNAME]);
          WrtLinFmt('var %s = "<B>" + %s.Etiquette + " - " + %s.Designation + "</B><BR>" +', [LOCAl_VAR_EWE, JS_MARKER_LOCAL_VARNAME, JS_MARKER_LOCAL_VARNAME]);
          WrtLinFmt('         "X = " + %s.%s + "<BR>" +', [JS_MARKER_LOCAL_VARNAME, JS_TMARKER_FIELD_NativeCoordoX, 0]);
          WrtLinFmt('         "Y = " + %s.%s + "<BR>" +', [JS_MARKER_LOCAL_VARNAME, JS_TMARKER_FIELD_NativeCoordoY, 0]);
          WrtLinFmt('         "Z = " + %s.%s + "<HR>" +' , [JS_MARKER_LOCAL_VARNAME, JS_TMARKER_FIELD_NativeCoordoZ,0]);
          WrtLinFmt('         "Lat: " + %s.Lat.toFixed(%d) + " - Lon: " + %s.Lon.toFixed(%d);' , [JS_MARKER_LOCAL_VARNAME, 8, JS_MARKER_LOCAL_VARNAME, 8]);
          WrtLinFmt('%s.openPopup(%s, %s);', [NAMEREF_MAP, LOCAl_VAR_EWE, LOCAL_VAR_latlng]);
	      EndJSFunction();
        WriteLine('// Callbacks de la carte');
        BeginJSFunction(JS_MAP_CALLBACK_MapOnClick, 'event');
          WrtLinFmt('var %s = event.latlng;', [LOCAL_VAR_001]);
          {$IFDEF POUR_ALAIN_DOLE}
          WrtLinFmt('var LB = document.getElementById("%s");', [JS_DOCUMENT_FORM_editPickCoords]);
          WrtLinFmt('var %s = ConvertLatLonToLT3(L, %s.lat, %s.lng);', [LOCAl_VAR_EWE, LOCAL_VAR_001, LOCAL_VAR_001]);
          WrtLinFmt('LB.value = %s.X.toFixed(%d) + "%s" + %s.Y.toFixed(%d);', [LOCAl_VAR_EWE, 3, ', ', LOCAl_VAR_EWE, 3]);
          {$ELSE}
          WrtLinFmt('LB.value = "%s" + %s.lat.toFixed(%d) + "%s" + %s.lng.toFixed(%d));', ['Lat = ', LOCAL_VAR_001, 8, ' - Lon = ', LOCAL_VAR_001, 8]);
          {$ENDIF POUR_ALAIN_DOLE}
        EndJSFunction();
        BeginJSFunction(JS_MAP_CALLBACK_MapOnMouseMove, 'event');
          WrtLinFmt('var %s = event.latlng;', [LOCAL_VAR_001]);
          WrtLinFmt('var LB = document.getElementById("%s");', [JS_DOCUMENT_FORM_lbCoordinates]);
          {$IFDEF POUR_ALAIN_DOLE}
          WrtLinFmt('var %s = ConvertLatLonToLT3(L, %s.lat, %s.lng);', [LOCAl_VAR_EWE, LOCAL_VAR_001, LOCAL_VAR_001]);
          WrtLinFmt('LB.innerHTML = "%s" + %s.X.toFixed(%d) + "%s" + %s.Y.toFixed(%d);', ['X = ', LOCAl_VAR_EWE, 3, ' - Y = ', LOCAl_VAR_EWE, 3]);
          {$ELSE}
          WrtLinFmt('LB.innerHTML = "%s" + %s.lat.toFixed(%d) + "%s" + %s.lng.toFixed(%d));', ['Lat = ', LOCAL_VAR_001, 8, ' - Lon = ', LOCAL_VAR_001, 8]);
          {$ENDIF POUR_ALAIN_DOLE}
        EndJSFunction();
        // Rechercher un item
        BeginJSFunction(JS_DOCUMENT_FORM_btnSearch_ProcOnSubmit, '');
          WrtLinFmt('var EWE  = %s.value;', [JS_DOCUMENT_FORM_editFindWhat]);
          WrtLinFmt('var QIdx = %s.%s(EWE);', [JS_TMARKERLIST_INSTANCE, JS_TMARKERLIST_METHOD_FindMarkerByEtiquette]);
          jsIF('QIdx >= 0', '');
            WrtLinFmt('var %s = %s.GetMarker(QIdx);', [JS_MARKER_LOCAL_VARNAME, JS_TMARKERLIST_INSTANCE]);
            WrtLinFmt('CentrerSurPoint(%s.Lat, %s.Lon, %s.getZoom());', [JS_MARKER_LOCAL_VARNAME, JS_MARKER_LOCAL_VARNAME, NAMEREF_MAP]);
            WrtLinFmt('%s.selectedIndex = QIdx;', [JS_DOCUMENT_FORM_lsbMarkers_Name]);
          jsELSE('');
            WriteLine('window.alert("Elément " + EWE + " introuvable");');
          jsENDIF('');
        EndJSFunction();
      EndScriptSection();
    EndHEAD();
    BeginBody(JS_FUNCTION_INITIALISER);
      // Le titre
      BeginDiv(NAMEREF_TITRE);
      WrtLinFmt('    %s', [FDocTitle]);
      EndDiv();
      // La carte .
      BeginDiv(NAMEREF_MAP);
      EndDiv();
      // La zone de recherche
      BeginDiv(NAMEREF_PANNEAU_SEARCH);
        BeginForm(JS_DOCUMENT_FORM_SEARCH_NAME);
          WrtLinFmt('    <LABEL for="%s" style="width:%d%%">%s<BR></LABEL>'    , [JS_DOCUMENT_FORM_editFindWhat, 100, 'Localiser un point']);
          WrtLinFmt('    <INPUT type="text" id="%s" style="width:%d%%" name="%s">'    , [JS_DOCUMENT_FORM_editFindWhat, 100, JS_DOCUMENT_FORM_editFindWhat]);
          WriteLine('    <BR>');
          WrtLinFmt('    <INPUT type="button" name="%s" value="%s" onclick="%s()">'  , [JS_DOCUMENT_FORM_btnSearch, 'Rechercher', JS_DOCUMENT_FORM_btnSearch_ProcOnSubmit]);
        EndForm();
      EndDiv();
      // Le menu latéral
      BeginDiv(NAMEREF_PANNEAU_LATERAL);
        WrtLinFmt('    Liste des %d éléments <BR>', [FNbMarkers]);
        BeginForm(JS_DOCUMENT_FORM_LISTE_NAME);
          WrtLinFmt('      <select name="%s" size="%d" style="width:%d%%" onChange="%s()">', [JS_DOCUMENT_LSBMARKER_NAME, 28, 100, JS_DOCUMENT_FORM_lsbMarkers_ProcOnSelect]);
          WriteLine('      </select>');
        EndForm();
      EndDiv();
      // Le panneau de picking
      BeginDiv(NAMEREF_PANNEAU_PICK_COORDONNEES);
        BeginForm(JS_DOCUMENT_FORM_PICK_COORDS);
          WrtLinFmt('      <LABEL id="%s" name="%s" style="width:%d%%">%s</LABEL>'    , [JS_DOCUMENT_FORM_lbCoordinates, JS_DOCUMENT_FORM_lbCoordinates, 100, '--']);
          WrtLinFmt('      <INPUT type="text" id="%s" style="width:%d%%" name="%s">'    , [JS_DOCUMENT_FORM_editPickCoords, 100, JS_DOCUMENT_FORM_editPickCoords]);
        EndForm();
      EndDiv();
    EndBody();
  EndHTML();
end;

// style = "width: 100%;
function TLeafletExport.HasAlreadyLayerVarName(const L: string): boolean;
var
  n, i: Integer;
  MyLayer: TOSMLayer;
  LL: String;
begin
  LL := LowerCase(L);
  result := false;
  n := GetNbLayers();
  if (0 = n) then exit(false);
  for i := 0 to n - 1 do
  begin
    MyLayer := GetLayer(i);
    if (LowerCase(MyLayer.LayerVarName) = LL) then exit(True);
  end;
end;

procedure TLeafletExport.AddLayer(const L: TOSMLayer);
begin
  FListeLayers.AddElement(L);
end;
// utilisé notamment pour tester les nouvelles fonctions
// sans avoir à les sauvegarder ailleurs
procedure TLeafletExport.BeginJSVerbatim(const Desc: string);
begin
  WriteLine(StringOfChar('/', 132));
  WriteLine('// VERBATIM: ' + Desc);
  WriteLine(StringOfChar('/', 132));
end;

procedure TLeafletExport.JSVerbatim(const L: string);
begin
  WriteLine(L);
end;

procedure TLeafletExport.EndJSVerbatim();
begin
  WriteLine(StringOfChar('/', 132));
  WriteLine('// END VERBATIM:');
  WriteLine(StringOfChar('/', 132));
end;

procedure TLeafletExport.AddLayer(const QLayerIsAMap, QLayerDisplayed: boolean;
                                  const QLayerName : string;
                                  const QLayerTitle: string;
                                  const QLayerAttribution: string;
                                  const QURL: String;
                                  const QLayerMarkerStyle: integer;
                                  const QLayerMarkerSize: double;
                                  const QLayerMarkerColor: TColor;
                                  const QLayerMarkerOpacity: double); overload;
var
  L: TOSMLayer;
  function EnsureLayerVarName(const LVN: string): string;
  const DUMMY_LAYER = 'Layer00001';
  var
    i, n: Integer;
  begin
    Result := Trim(LVN);
    n := length(Result);
    if (n = 0) then exit(DUMMY_LAYER);
    // On vire tout ce qui n'est pas valable dans un nom de variable
    for i := 1 to n do
    begin
      if (not (Result[i] in ['0' .. '9', 'a' .. 'z', 'A' .. 'Z'])) then System.Delete(Result, i, 1);
    end;
    // épurage + un autre test de longueur nulle
    Result := Trim(Result); n := length(Result);
    if (n = 0) then exit(DUMMY_LAYER);
    // Si le premier caractère est un chiffre, on ajoute une lettre en tête
    if (Result[1] in ['0' .. '9']) then System.Insert('L', Result, 1);
    // épurage et sortie
    Result := Trim(Result);
    n := length(Result);   // MAJ du nombre de caractères
  end;
begin
  L.LayerIsAMap      := QLayerIsAMap;
  L.LayerDisplayed   := QLayerDisplayed;
  L.LayerVarName     := EnsureLayerVarName(QLayerName);
  L.LayerTitle       := QLayerTitle;
  L.LayerAttribution := QLayerAttribution;
  L.LayerURL         := QURL;
  L.LayerMarkerStyle := QLayerMarkerStyle;
  L.LayerMarkerSize  := QLayerMarkerSize;
  L.LayerMarkerColor := QLayerMarkerColor;
  L.LayerMarkerOpacity := QLayerMarkerOpacity;
  if (not HasAlreadyLayerVarName(L.LayerVarName)) then FListeLayers.AddElement(L);
end;

function TLeafletExport.GetLayer(const Idx: integer): TOSMLayer;
begin
  Result := FListeLayers.GetElement(Idx);
end;

procedure TLeafletExport.BeginConditionalSection(const B: boolean);
var
  EWE: String;
begin
  WrtLinFmt('      if (%s)', [BoolToStr(b, 'true', 'false')]);
  WriteLine('      {');
end;

procedure TLeafletExport.EndConditionalSection();
begin
  WrtLinFmt('      }; // if (%s)', ['']);
end;


end.

