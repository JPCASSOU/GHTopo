unit unitExportGIS;
{$INCLUDE CompilationParameters.inc}
{$ERROR Unite deprecated}
// 29/03/2020: Unité inutilisée
// 26/02/2015: Factorisation de code
// 21/08/2017: Suppression de l'export OSM (inutile et bogué)
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  StructuresDonnees,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitSilhouetteGalerie,
  ConvertisseurJPC,
  UnitClassPalette, UnitObjetSerie,
  Graphics, math,
  Classes, SysUtils;

// export KML: Références
const
  KML_OPENGIS_WEBSITE     = 'http://www.opengis.net/kml/2.2';
  KML_GOOGLE_KML_WEBSITE  = 'http://www.google.com/kml/ext/2.2';
  W3C_W3_WEBSITE          = 'http://www.w3.org/2005/Atom';
  W3C_XML_SCHEMA_WEBSITE  = 'http://www.w3.org/2001/XMLSchema-instance';
  GPX_TOPOGRAPHIX_WEBSITE = 'http://www.topografix.com/GPX/1/0';
const
  STYLE_CENTERLINE_ITEM          = 'StyleCenterlineItem';
  STYLE_SILHOUETTE_ITEM          = 'StyleSilhouetteItem';
  STYLE_CENTERLINE_PAR_DEFAUT    = 'StyleCenterlineParDefault';
  STYLE_SILHOUETTE_PAR_DEFAUT    = 'StyleSilhouetteParDefault';
const
  KML_DOCUMENT                   = 'Document';
  KML_PLACEMARK                  = 'Placemark';
  KML_DESCRIPTION                = 'description';
  KML_LINESTRING                 = 'LineString';
  KML_COORDINATES                = 'coordinates';
  KML_POINT                      = 'Point';
  KML_LINE_STYLE                 = 'LineStyle';
  KML_POLY_STYLE                 = 'PolyStyle';
  KML_POLYGON                    = 'Polygon';
  KML_OUTERBOUNDARYIS            = 'outerBoundaryIs';
  KML_LINEARRING                 = 'LinearRing';


type

{ TExportGIS }

 TExportGIS = class
  strict private
    Ffp                : TextFile;
    FCurrentIndentation: integer;
    FMapWidth          : integer;
    FMapHeight         : integer;
    function EWE(): string; inline;
  private
    FDocuTopo         : TToporobotStructure2012;
    FBDDEntites       : TBDDEntites;
    FConvertisseurEPSG: TConversionSysteme;

    FGISOutputFormat  : TGISOutputFormat;
    FGISColorByItems  : TGISColorByItems;
    FGISOutputMode    : TGISOutputMode;

    FFileName         : string;
    FFiltres          : string;
    FCouleurDefaut    : TColor;
    FUseColorsGroupes : boolean;
    FCodeEPSGOrigine  : integer;
    FCentroideLonLat  : TProjUV;



    procedure DessinerMarkerEntrance(const QEntrance: TEntrance);

    procedure KML_WritePolygonStyle(const IDStyle: string; const LineColor, FillColor: TColorRGBA; const LineWidth: double);
    procedure KML_WritePolyLineStyle(const IDStyle: string; const LineColor: TColorRGBA; const LineWidth: double);

    procedure WriteKMLValue(const K, V: string);
    procedure CalcCentroideEntities();
    procedure BeginXMLSection(const KW: string);
    procedure EndXMLSection(const KW: string);

    procedure BeginCenterlines();
    procedure EndCenterlines();

    procedure BeginPlacemarks();
    procedure EndPlacemarks();

    procedure BeginPolyligne();
    procedure AddVertex(const X, Y: double);
    procedure EndPolyligne();

  public
    function Initialiser(const PtrDocTopo     : TToporobotStructure2012;
                         const PtrBDDEntites  : TBDDEntites;
                         const PtrCoordConv   : TConversionSysteme;
                         const CodeEPSGOrigine: integer;
                         const GISOutputFormat: TGISOutputFormat;
                         const GISOutputMode  : TGISOutputMode;
                         const GISColorByItems: TGISColorByItems;
                         const CouleurDefault: TColor;
                         const QMapWidth, QMapHeight: integer;
                         const QFileName: TStringDirectoryFilename): boolean;
    procedure Finaliser();
    procedure WriteHeader(const DoSpecifyPoint0: boolean; const QLon, QLat: double);
    procedure WriteFooter();
    procedure BeginExportGIS();
    procedure EndExportGIS();

    procedure WriteConvertedPoint(const PolyMode: TPolyMode; const ET: TBaseStation);
    procedure WriteSegmentShot(const ET: TBaseStation);
    procedure WritePoint(const QLat, QLon, QAlt: double;
                         const EtiquetteTitre: string;
                         const EtiquetteDescro: string;
                         const OSM_IDNode: Int64);
//    procedure WriteOSMEntrance(const E: TEntrance; const OSM_IDNode: Int64);

    procedure WriteLigne(const S: string);
    procedure WriteCommentaire(const S: string);
    procedure WriteAEntrance(const E: TEntrance; const TagInt64: Int64);

    procedure DessinerPolygoneSilhouette(const QSilhouette: TSilhouetteGalerie);

    procedure ExporterEntrances();
    procedure ExporterCenterlines();
    procedure ExporterSilhouettes();
end;


implementation
const
  MAP_NAMEREF    =  'MyMap';

procedure TExportGIS.CalcCentroideEntities();
var
  PM: TProjUV;
  P1, P2: TPoint3Df;
begin
  if (not Assigned(FBDDEntites)) then Exit;

  P1 := FBDDEntites.GetCoinBasGauche();
  P2 := FBDDEntites.GetCoinHautDroit();
  PM.U := 0.50 * (P1.X + P2.X);
  PM.V := 0.50 * (P1.Y + P2.Y);
  FCentroideLonLat := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, PM);

end;

procedure TExportGIS.BeginXMLSection(const KW: string);
begin
  FCurrentIndentation := FCurrentIndentation + 2;
  writeln(Ffp, Format('%s<%s>', [StringOfChar(' ', FCurrentIndentation), KW]));
end;

procedure TExportGIS.EndXMLSection(const KW: string);
begin
  writeln(Ffp, Format('%s</%s>', [StringOfChar(' ', FCurrentIndentation), KW]));
  FCurrentIndentation := FCurrentIndentation - 2;
  if (FCurrentIndentation < 0) then FCurrentIndentation := 0;
end;

procedure TExportGIS.BeginCenterlines();
begin
  pass;
end;

procedure TExportGIS.EndCenterlines();
begin
  pass;
end;

procedure TExportGIS.BeginPlacemarks();
begin
  pass;
end;

procedure TExportGIS.EndPlacemarks();
begin
  pass;

end;

procedure TExportGIS.BeginPolyligne();
begin
  pass;
end;

procedure TExportGIS.AddVertex(const X, Y: double);
begin
  pass;
end;

procedure TExportGIS.EndPolyligne();
begin
  pass;
end;

{ TExportGIS }
// en-tête de fichiers et déf des styles
procedure TExportGIS.WriteHeader(const DoSpecifyPoint0: boolean; const QLon, QLat: double);
const
  ENCODAGE_XML    = 'UTF-8';
  URL_LEAFLET_LIB = 'http://cdn.leafletjs.com/leaflet/v0.7.7/';
  URL_LEAFLET_JS  = URL_LEAFLET_LIB + 'leaflet.js';
  URL_LEAFLET_CSS = URL_LEAFLET_LIB + 'leaflet.css';
  URL_OSM_TILES   = 'http://{s}.tile.osm.org/{z}/{x}/{y}.png';
var
  URL_OSM_Contributors : string;
  MyStyleCenterlineName: string;
  MyStyleSilhouetteName: String;
  ii    : Integer;
  QFillColor, QLineColor: TColorRGBA;
  WU: TColor;
  MyEntrance: TEntrance;
  MyReseau: TReseau;
  procedure MakeStyle(const MyLineStyleName, MyFillStyleName: string;  const MyLineColor, MyFillColor: TColorRGBA; const MyLineWidth, MyFillWidth: integer);
  begin
    if (gisSILHOUETTES in FGISOutputMode) then KML_WritePolyLineStyle(MyLineStyleName, MyLineColor, MyLineWidth);
    if (gisCENTERLINES in FGISOutputMode) then KML_WritePolygonStyle(MyFillStyleName, MyLineColor, MyFillColor, MyFillWidth);
  end;
begin
  FCurrentIndentation := 0;
  QLineColor       := MakeTColorRGBA(FCouleurDefaut, 255);
  QFillColor       := MakeTColorRGBA(FCouleurDefaut, 128);
  case FGISOutputFormat of
   gisKML: // KML: KO pour les silhouettes
     begin // KML // On définit aussi les styles d'objets polygones =
       WriteLn(Ffp, FormatterVersionEncodageXML('1.0', ENCODAGE_XML));
       WriteLn(Ffp, Format('<kml xmlns="%s">',[KML_OPENGIS_WEBSITE]));
       BeginXMLSection(KML_DOCUMENT); //       WriteLn(Ffp, '  <Document>');
       WriteLn(Ffp, Format('  <!-- Generated by GHTopo Toolbox version %s - Mode: %d Date: %s -->', [GetGHTopoVersion(), Ord(FGISColorByItems), DateTimeToStr(Now())]));
       // style de ligne par défaut
       MakeStyle(STYLE_CENTERLINE_PAR_DEFAUT, STYLE_SILHOUETTE_PAR_DEFAUT, QLineColor, QFillColor, 2, 0);
       // styles de polygones
       if (Assigned(FBDDEntites)) then
       begin
         case FGISColorByItems of
           gisCOLOR_UNIQUE: pass;
           gisCOLOR_ENTRANCES:
             begin
               for ii := 0 to FBDDEntites.GetNbEntrances() - 1 do
               begin
                 MyEntrance := FBDDEntites.GetEntrance(ii);
                 WU  := MyEntrance.eCouleur;
                 QLineColor := MakeTColorRGBA(WU, 255);
                 QFillColor := MakeTColorRGBA(WU, 128);
                 MyStyleCenterlineName := Format('%s%d', [STYLE_CENTERLINE_ITEM, ii]);
                 MyStyleSilhouetteName := Format('%s%d', [STYLE_SILHOUETTE_ITEM, ii]);
                 MakeStyle(MyStyleCenterlineName, MyStyleSilhouetteName, QLineColor, QFillColor, 2, 0);
               end;
             end;
           gisCOLOR_RESEAUX:
             begin
               for ii := 0 to FBDDEntites.GetNbReseaux() - 1 do
               begin
                 MyReseau := FBDDEntites.GetReseau(ii);
                 WU  := MyReseau.ColorReseau;
                 QLineColor := MakeTColorRGBA(WU, 255);
                 QFillColor := MakeTColorRGBA(WU, 128);
                 MyStyleCenterlineName := Format('%s%d', [STYLE_CENTERLINE_ITEM, ii]);
                 MyStyleSilhouetteName := Format('%s%d', [STYLE_SILHOUETTE_ITEM, ii]);
                 MakeStyle(MyStyleCenterlineName, MyStyleSilhouetteName, QLineColor, QFillColor, 2, 0);
               end;
             end;
         end;
         begin
           pass;
         end;
       end;
     end;
   gisGPX:
     begin // GPX
       WriteLn(Ffp, FormatterVersionEncodageXML('1.0', ENCODAGE_XML)); //<?xml version="1.0" encoding="ISO-8859-1"?>');
       WriteLn(Ffp, Format('<gpx version="%.1f" creator="%s" xmlns:xsi="%s" xmlns="%s">',
                          [1.1, // version xml
                           rsGHTOPOEXENAME, // logiciel auteur
                           W3C_XML_SCHEMA_WEBSITE,
                           GPX_TOPOGRAPHIX_WEBSITE
                          ]));
     end;
  end;
end;

procedure TExportGIS.WriteLigne(const S: string);
begin
  writeln(Ffp, S);
end;

procedure TExportGIS.WriteCommentaire(const S: string);
begin
  case FGISOutputFormat of
    gisKML,
    gisGPX            : WriteLigne(Format('%s<!-- %s -->', [EWE(), XMLStrToPasStr(S)]));
  else
    pass;
  end;
end;



procedure TExportGIS.WriteAEntrance(const E: TEntrance; const TagInt64: Int64);
var
  P1, P2: TProjUV;
begin
  P1.U := E.eXEntree;
  P1.V := E.eYEntree;
  if (IsZero(E.eXEntree) or IsZero(E.eYEntree)) then Exit;
  P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
  WritePoint(P2.U, P2.V, E.eZEntree, E.eNomEntree, Format(FMTSERST, [E.eRefSer, E.eRefSt]), TagInt64);
end;
procedure TExportGIS.WriteFooter();
begin
  case FGISOutputFormat of
    gisKML:
      begin // KML
        EndXMLSection(KML_DOCUMENT);    //        WriteLn(Ffp, '  </Document>');
        WriteLn(Ffp, '</kml>');
      end;
    gisGPX:
      begin // GPX
        WriteLn(Ffp, ' </gpx>');
      end;
  end;
end;

// encodage du style
procedure TExportGIS.KML_WritePolygonStyle(const IDStyle: string;
                                           const LineColor, FillColor: TColorRGBA;
                                           const LineWidth: double);

begin
  Writeln(Ffp, Format('%s<Style id="%s">', [EWE(), IDStyle]));
  FCurrentIndentation := FCurrentIndentation + 2;
  BeginXMLSection(KML_LINE_STYLE);
    WriteLn(Ffp, Format('%s%s', [EWE(), KMLColor(LineColor.R, LineColor.G, LineColor.B, LineColor.A)]));
    WriteLn(Ffp, Format('%s<width>%.1f</width>', [EWE(), LineWidth]));
  EndXMLSection(KML_LINE_STYLE);
  BeginXMLSection(KML_POLY_STYLE);
    WriteLn(Ffp, Format('%s%s', [EWE(), KMLColor(FillColor.R, FillColor.G, FillColor.B, FillColor.A)]));
  EndXMLSection(KML_POLY_STYLE);
  WriteLn(Ffp, Format('%s</%s>', [EWE(), 'Style']));
  FCurrentIndentation := FCurrentIndentation - 2;
end;
// encodage du style
procedure TExportGIS.KML_WritePolyLineStyle(const IDStyle: string;
                                            const LineColor: TColorRGBA;
                                            const LineWidth: double);
begin
  Writeln(Ffp, Format('%s<Style id="%s">', [EWE(), IDStyle]));
  BeginXMLSection(KML_LINE_STYLE);
    WriteLn(Ffp, Format('%s%s', [EWE(), KMLColor(LineColor.R, LineColor.G, LineColor.B, LineColor.A)]));
    WriteLn(Ffp, Format('%s<width>%.1f</width>', [EWE(), LineWidth]));
  EndXMLSection(KML_LINE_STYLE);
  WriteLn(Ffp, Format('%s</%s>', [EWE(), 'Style']));
  FCurrentIndentation := FCurrentIndentation - 2;
end;



procedure TExportGIS.WriteKMLValue(const K, V: string);
begin
 WriteLn(Ffp, Format('     <%s>%s</%s>',[K, PasStrToXMLStr(V), K]));
end;

procedure TExportGIS.WritePoint(const QLat, QLon, QAlt: double;
                                const EtiquetteTitre: string;
                                const EtiquetteDescro: string;
                                const OSM_IDNode: Int64);
var
  WU: String;
begin
  case FGISOutputFormat of
    gisKML:
     begin
       BeginXMLSection(KML_PLACEMARK);
         WriteKMLValue('name', XMLStrToPasStr(EtiquetteTitre));
         BeginXMLSection(KML_DESCRIPTION); //         WriteLigne('      <description>');
           WriteLigne('         <![CDATA[');
           WriteLigne(Format('         <h1>%s</h1>', [EtiquetteTitre]));
           WU := StringReplace(EtiquetteDescro, #9, '<BR>', [rfReplaceAll, rfIgnoreCase]);
           WriteLigne('         <P>' + WU + '<P>');
           WriteLigne('         ]]>');
         EndXMLSection(KML_DESCRIPTION);     //           WriteLigne('      </description>');
         BeginXMLSection(KML_POINT); //         WriteLigne('      <Point>');
           WriteKMLValue('coordinates', Format('%s, %s, %s', [ensureConvertLongOrLatToXML(QLon), ensureConvertLongOrLatToXML(QLat), ensureConvertLongOrLatToXML(QAlt)]));
         EndXMLSection(KML_POINT); //         WriteLigne('      <Point>');
       EndXMLSection(KML_PLACEMARK);
     end;
    gisGPX:
     begin
       WriteLigne(Format('    <wpt lat="%.10f" lon="%.10f">', [QLat, QLon]));
       WriteKMLValue('ele', Format(FORMAT_NB_REAL_2_DEC, [QAlt]));
       WriteKMLValue('name', XMLStrToPasStr(EtiquetteTitre));
       WriteLigne('    </wpt>');
     end;

  end; // case
end;


procedure TExportGIS.WriteConvertedPoint(const PolyMode: TPolyMode;
                                         const ET: TBaseStation);
var
 CPColor: TColor;
 R   : Double;
 P1, P2: TProjUV;
 PT1: TProjUV;
 PT2: TProjUV;
 MyEntrance: TEntrance;
 MyReseau: TReseau;
begin
  CPColor := FCouleurDefaut; //IIF(DoUseColors, ET.ColorEntite, );
  // éliminer visées trop courtes et trop longues
  R:=Hypot3D(ET.PosStation.X - ET.PosExtr0.X,
             ET.PosStation.Y - ET.PosExtr0.Y,
             ET.PosStation.Z - ET.PosExtr0.Z
           );
  if (R > SEUIL_LONGUEUR_MAXI_TOPOROBOT) then Exit;
  // éliminer les entrées
  if (ET.Type_Entite = tgENTRANCE) then Exit;
  // export point
  case FGISOutputFormat of
   gisKML:
     begin // KML; sans objet
        case PolyMode of
          tpmENTETE_POLY:
            begin
              BeginXMLSection(KML_PLACEMARK);     //WriteLn(Ffp, '  <Placemark>');
              case FGISColorByItems of
                gisCOLOR_UNIQUE   : WriteLigne(Format('         <styleUrl>#%s</styleUrl>', [STYLE_CENTERLINE_PAR_DEFAUT]));
                gisCOLOR_ENTRANCES: WriteLigne(Format('         <styleUrl>#%s%d</styleUrl>', [STYLE_CENTERLINE_ITEM, ET.eEntrance]));
                gisCOLOR_RESEAUX  : WriteLigne(Format('         <styleUrl>#%s%d</styleUrl>', [STYLE_CENTERLINE_ITEM, ET.eReseau]));
              end;
            end;
          tpmSTART_POLY:
            begin
              BeginXMLSection(KML_LINESTRING); //              WriteLn(Ffp, '    <LineString>');
                WriteLn(Ffp, '      <extrude>1</extrude>');
                WriteLn(Ffp, '      <tessellate>1</tessellate>');
                WriteLn(Ffp, '      <altitudeMode>relativeToGround</altitudeMode>');
                BeginXMLSection(KML_COORDINATES); //           WriteLn(Ffp, '      <coordinates>');
                  P1.U := ET.PosExtr0.X;
                  P1.V := ET.PosExtr0.Y;
                  P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
                  WriteLn(Ffp, Format('%s%s, %s, %.2f', [StringOfChar(' ', FCurrentIndentation + 2),
                                                         ensureConvertLongOrLatToXML(P2.V),
                                                         ensureConvertLongOrLatToXML(P2.U),
                                                         0.01]));
            end;
          tpmPOINT_POLY:
            begin
              P1.U := ET.PosExtr0.X;
              P1.V := ET.PosExtr0.Y;
              P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
              WriteLn(Ffp, Format('%s%s, %s, %.2f', [StringOfChar(' ', FCurrentIndentation + 2),
                                                     ensureConvertLongOrLatToXML(P2.V),
                                                     ensureConvertLongOrLatToXML(P2.U),
                                                     0.01]));
            end;
          tpmEND_POLY:
            begin
              EndXMLSection(KML_COORDINATES);
              EndXMLSection(KML_LINESTRING);
              EndXMLSection(KML_PLACEMARK);
            end;
        end;
     end;
   gisGPX:
     begin // GPX
       // séries
       case PolyMode of
         tpmENTETE_POLY: //0
           begin  // en-tete de track/série
             WriteLn(Ffp, '    <trk> <trkseg>');
           end;
         tpmSTART_POLY: //1
           begin // début de polyligne (série)
             P1.U := ET.PosExtr0.X;
             P1.V := ET.PosExtr0.Y;
             P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
             WriteLn(Ffp, Format('        <trkpt lat="%s" lon="%s"></trkpt>',
                                [ensureConvertLongOrLatToXML(P2.U),
                                 ensureConvertLongOrLatToXML(P2.V)
                                ]));
            end;
         tpmPOINT_POLY: //2
           begin // point de polyligne (série)
             P1.U := ET.PosStation.X;
             P1.V := ET.PosStation.Y;
             P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
             WriteLn(ffp, Format('        <trkpt lat="%s" lon="%s"></trkpt>', [
                                ensureConvertLongOrLatToXML(P2.U),
                                ensureConvertLongOrLatToXML(P2.V),
                                ET.Entite_Serie, ET.Entite_Station]));
          end;
         tpmEND_POLY:   //3
           begin // GPX: balise de cloture
            WriteLn(ffp, '    </trkseg> </trk>');
           end;
       end;
     end;
  end; // case
end;
procedure TExportGIS.WriteSegmentShot(const ET: TBaseStation);
var
  P1, P2: TProjUV;
  PT1: TProjUV;
  PT2: TProjUV;
begin
  case FGISOutputFormat of
    gisKML:
      begin
        pass;
      end;
    gisGPX:
      begin
        WriteLn(Ffp, '    <trk> <trkseg>');
        P1.U := ET.PosExtr0.X;
        P1.V := ET.PosExtr0.Y;
        P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
        WriteLn(Ffp, Format('        <trkpt lat="%s" lon="%s"></trkpt>', [
                               ensureConvertLongOrLatToXML(P2.U),
                               ensureConvertLongOrLatToXML(P2.V)]));
        P1.U := ET.PosStation.X;
        P1.V := ET.PosStation.Y;
        P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
        WriteLn(Ffp, Format('        <trkpt lat="%s" lon="%s"></trkpt>', [
                               ensureConvertLongOrLatToXML(P2.U),
                               ensureConvertLongOrLatToXML(P2.V)]));
        WriteLn(Ffp, '    </trkseg> </trk>');
      end;
  otherwise
    ;
  end;
end;

//******************************************************************************

procedure TExportGIS.Finaliser();
begin
  try
  finally
    CloseFile(Ffp);
  end;
end;

function TExportGIS.Initialiser(const PtrDocTopo    : TToporobotStructure2012;
                               const PtrBDDEntites  : TBDDEntites;
                               const PtrCoordConv   : TConversionSysteme;
                               const CodeEPSGOrigine: integer;
                               const GISOutputFormat: TGISOutputFormat;
                               const GISOutputMode  : TGISOutputMode;
                               const GISColorByItems: TGISColorByItems;
                               const CouleurDefault: TColor;
                               const QMapWidth, QMapHeight: integer;
                               const QFileName: TStringDirectoryFilename): boolean;
begin
  Result := false;
  FCurrentIndentation := 0;
  try
    FDocuTopo         := PtrDocTopo;
    FBDDEntites       := PtrBDDEntites;
    FMapWidth         := QMapWidth;  //1200;
    FMapHeight        := QMapHeight; // 900;

    FFileName         := QFileName;

    FGISOutputFormat  := GISOutputFormat;
    FGISOutputMode    := GISOutputMode;
    FGISColorByItems  := GISColorByItems;

    FCouleurDefaut    := CouleurDefault;
    // filtres
    FFiltres          := '';
    // convertisseur
    FConvertisseurEPSG := PtrCoordConv;
    FCodeEPSGOrigine := CodeEPSGOrigine;
    AssignFile(FFp, QFileName);
    ReWrite(Ffp);
    Result := True;
    WriteHeader(false, -1.0, -1.0);
  except
  end;
end;

function TExportGIS.EWE(): string;
begin
  result := StringOfChar(' ', FCurrentIndentation + 2);
end;

procedure TExportGIS.DessinerMarkerEntrance(const QEntrance: TEntrance);
begin
  WriteAEntrance(QEntrance, 0);
end;


//----------------------------------------------------------------------------
// dessiner le polygone de silhouette
procedure TExportGIS.DessinerPolygoneSilhouette(const QSilhouette: TSilhouetteGalerie);
const
  DELTA_Z_SURFACE = 1.00;
var
  ii : integer;
  PP: TPoint2Df;
  P1, P2: TProjUV;
  Nb: LongInt;
begin
   // construction du polygone
   if (not QSilhouette.BuildPolygoneParois) then Exit;
   Nb := QSilhouette.GetNbPointsPolygoneParois();
   if (Nb < 3) then Exit;
   case FGISOutputFormat of
    gisKML:
      begin // KML
        BeginXMLSection(KML_PLACEMARK); //WriteLigne(           '       <Placemark>');
        //MyExportGIS.WriteLigne(Format('       <!-- Polygone: %s (%d sommets) -->',['MyPolygone', Nb]));
        WriteLigne(Format('%s<name>%s</name>', [EWE(), QSilhouette.GetNomSilhouette()]));
        case FGISColorByItems of
          gisCOLOR_UNIQUE    : WriteLigne(Format('%s<styleUrl>#%s</styleUrl>', [EWE(), STYLE_SILHOUETTE_PAR_DEFAUT]));
          gisCOLOR_ENTRANCES : WriteLigne(Format('%s<styleUrl>#%s%d</styleUrl>', [EWE(),STYLE_SILHOUETTE_ITEM, QSilhouette.GetNumeroEntrance()]));
          gisCOLOR_RESEAUX   : WriteLigne(Format('%s<styleUrl>#%s%d</styleUrl>', [EWE(), STYLE_SILHOUETTE_ITEM, QSilhouette.GetNumeroReseau()]));
        else
          pass;
        end;
        BeginXMLSection(KML_POLYGON); //WriteLigne(           '         <Polygon>');                  //WriteLigne(Format('           <tesselate>%d</tesselate>', [1]));

        WriteLigne(Format('%s<extrude>%d</extrude>', [EWE(), 1]));
        WriteLigne(Format('%s<altitudeMode>%s</altitudeMode>', [EWE(), 'relativeToGround']));
        BeginXMLSection(KML_OUTERBOUNDARYIS);//        WriteLigne(           '           <outerBoundaryIs>');
        BeginXMLSection(KML_LINEARRING); //WriteLigne(           '             <LinearRing>');
        BeginXMLSection(KML_COORDINATES); //Write(Ffp,            '               <coordinates>');
        for ii := 0 to Nb - 1 do
        begin
          PP := QSilhouette.GetPointPolygoneParois(ii);
          P1.U := PP.X;
          P1.V := PP.Y;
          P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
          WriteLigne(Format('%s%s,%s,%s', [EWE(),
                                           ensureConvertLongOrLatToXML(P2.V),
                                           ensureConvertLongOrLatToXML(P2.U),
                                           ensureConvertAltitudeToXML(DELTA_Z_SURFACE)
                             ]));   //Lon, Lat
        end;
        EndXMLSection(KML_COORDINATES);                //        </coordinates>');
        EndXMLSection(KML_LINEARRING);                 //       </LinearRing>');
        EndXMLSection(KML_OUTERBOUNDARYIS);             //      </outerBoundaryIs>');
        EndXMLSection(KML_POLYGON);                     //     </Polygon>');
        EndXMLSection(KML_PLACEMARK);                   //    </Placemark>'
      end;
    gisGPX:
      begin // GPX
        //MyExportGIS.WriteLigne(Format('<!-- Polygone: %s (%d sommets) -->',['MyPolygone', Nb]));
        WriteLigne('    <trk> <trkseg>');
        for ii := 0 to Nb - 1 do
        begin
          PP := QSilhouette.GetPointPolygoneParois(ii);
          P1.U := PP.X;
          P1.V := PP.Y;
          P2 := FConvertisseurEPSG.ConversionSyst1ToSyst2EPSG(FCodeEPSGOrigine, CODE_EPSG_WGS84, P1);
          WriteLigne(Format('        <trkpt lat="%s" lon="%s"></trkpt>', [
                                        ensureConvertLongOrLatToXML(P2.U),
                                        ensureConvertLongOrLatToXML(P2.V)]))    //QLat, QLon
        end;
        WriteLigne('    </trkseg> </trk>');
      end;
  end;
end;

procedure TExportGIS.ExporterEntrances();
begin

end;

procedure TExportGIS.BeginExportGIS();
var
  QDevelViseesVisibles: double;
  EWE: TLabelSystemesCoordsEPSG;
begin
  QDevelViseesVisibles := 0.00;
  AfficherMessage(Format('%s.PreparerExport(%s)',[self.ClassName, FFileName]));
  AfficherMessage(Format('--> Filtres: %s',[FFiltres]));
  AfficherMessage(Format('--> Systeme de coordonnees: EPSG:%d',[FCodeEPSGOrigine]));
  EWE := FConvertisseurEPSG.GetEPSGSystemeFromCodeEPSG(FCodeEPSGOrigine);
  AfficherMessage(Format('--> Systeme coordonnees: %d = %s',[EWE.CodeEPSG, EWE.NomEPSG]));
  AfficherMessage(Format('--> Export vers: %s', [FFileName]));
  // Un coup de MétaFiltre pour commencer
  FBDDEntites.MetaFiltre(FFiltres, QDevelViseesVisibles);
  FBDDEntites.SetMinMax(False);
  // Tri par séries et stations (peut-être inutile)
  FBDDEntites.SortBySerSts();
end;
procedure TExportGIS.EndExportGIS();
begin
  WriteFooter();
end;

procedure TExportGIS.ExporterCenterlines();
var
  QNbSeries, i : Integer;
  QNbVisees, s : integer;
  QNbEntrances : integer;
  MySerie: TObjSerie;
  MyVisee: TUneVisee;
  MyEntrance: TEntrance;
  BP: TBaseStation;
begin
  FBDDEntites.SortBySerSts();     // Trier par ordre croissant
  // centerlines
  BeginExportGIS();
    BeginCenterlines();
      QNbSeries := FDocuTopo.GetNbSeries();
      for i := 1 to QNbSeries - 1 do
      begin
        MySerie := FDocuTopo.GetSerie(i);
        QNbVisees := MySerie.GetNbVisees();
        if (FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), BP)) then
        begin
          BeginPolyligne();
            AddVertex(BP.PosStation.X, BP.PosStation.Y);
            for s := 1 to QNbVisees - 1 do
            begin
              MyVisee := MySerie.GetVisee(s);
              if (FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNumeroDeSerie(), s, BP)) then
              begin
                AddVertex(BP.PosStation.X, BP.PosStation.Y);
              end;
            end;
          EndPolyligne();
        end;
      end;
    EndCenterlines();
    // entrées
    BeginPlaceMarks();
      QNbEntrances := FDocuTopo.GetNbEntrances();
      for i := 1 to QNbEntrances - 1 do DessinerMarkerEntrance(FBDDEntites.GetEntrance(i));
    EndPlaceMarks();
  EndExportGIS();
  (*
  QNbEntites := FBDDEntites.GetNbEntitesVisees();
  E1 := FBDDEntites.GetEntiteVisee(1);
  WriteConvertedPoint(tpmENTETE_POLY, E1);
  WriteConvertedPoint(tpmSTART_POLY, E1);
  for i:= 2 to QNbEntites - 1 do
  begin
    E0 := FBDDEntites.GetEntiteVisee(i-1);
    E1 := FBDDEntites.GetEntiteVisee(i);
    if (E1.Type_Entite = tgENTRANCE) then Continue;
    if (E1.Type_Entite = tgVISEE_RADIANTE) then Continue;
    if (IsSameSerie(E0, E1)) then
    begin
      WriteConvertedPoint(tpmPOINT_POLY, E1);
    end else
    begin
      WriteConvertedPoint(tpmEND_POLY, E1); // pour GPX uniquement: balise de cloture
      WriteConvertedPoint(tpmENTETE_POLY, E1);
      WriteConvertedPoint(tpmSTART_POLY, E1);
    end;
  end;
  WriteConvertedPoint(tpmEND_POLY, E1); // pour GPX uniquement: balise de cloture
  end;

  //*)
end;

procedure TExportGIS.ExporterSilhouettes();
var
  QNbEntites, i: Integer;
  MySilhouette: TSilhouetteGalerie;
  MyColorEntite: TColor;
  E0, E1: TBaseStation;
  PtsParois, PtsParois2: TPtsSectionTransversale;
  NbreSilhouettes: Int64;
begin
  NbreSilhouettes := 0;
  QNbEntites := FBDDEntites.GetNbEntitesVisees();
  if (QNbEntites < 3) then Exit;
  MySilhouette := TSilhouetteGalerie.Create;
  try
    MySilhouette.ClearSilhouette(FCouleurDefaut, FCouleurDefaut);
    // premier polygone
    E1 := FBDDEntites.GetEntiteVisee(1);
    MyColorEntite := FCouleurDefaut;//GetCouleurEntiteRGBByExpe(E1);
    MySilhouette.ClearSilhouette(MyColorEntite, MyColorEntite);
    PtsParois.ParoiGaucheX := E1.PosOPG.X;
    PtsParois.ParoiGaucheY := E1.PosOPG.Y;
    PtsParois.ParoiDroiteX := E1.PosOPD.X;
    PtsParois.ParoiDroiteY := E1.PosOPD.Y;
    PtsParois.PosStation   := E1.PosStation;
    MySilhouette.AddPtsParois(PtsParois);
    AfficherMessage(format('-- %d entités',[QNbEntites]));
    for i:= 2 to QNbEntites - 1 do
    begin
      try
        //if (Assigned(FProcDispProgression)) then FProcDispProgression(Format('Entite %d / %d', [i, QNbEntites]), i, 1, QNbEntites);
        E0 := FBDDEntites.GetEntiteVisee(i-1);
        E1 := FBDDEntites.GetEntiteVisee(i);
        PtsParois.PosStation   := E1.PosStation;
        PtsParois.ParoiDroiteX := E1.PosPD.X;
        PtsParois.ParoiDroiteY := E1.PosPD.Y;
        PtsParois.ParoiGaucheX := E1.PosPG.X;
        PtsParois.ParoiGaucheY := E1.PosPG.Y;
        if (IsSameSerie(E0, E1)) then
        begin
          MySilhouette.AddPtsParois(PtsParois);
          MySilhouette.SetNomSilhouette(Format('Serie %d', [E1.Entite_Serie]));
          MySilhouette.SetIdxEntranceReseau(E1.eEntrance, E1.eReseau);
        end else
        begin
          DessinerPolygoneSilhouette(MySilhouette);
          NbreSilhouettes := 1 + NbreSilhouettes;
          // on purge la silhouette courante pour en accueillir la suivante
          MyColorEntite := FCouleurDefaut;
          MySilhouette.ClearSilhouette(MyColorEntite, MyColorEntite);
          PtsParois2.ParoiGaucheX := E1.PosOPG.X;
          PtsParois2.ParoiGaucheY := E1.PosOPG.Y;
          PtsParois2.ParoiDroiteX := E1.PosPD.X;
          PtsParois2.ParoiDroiteY := E1.PosPD.Y;
          PtsParois2.PosStation   := E1.PosStation;
          MySilhouette.AddPtsParois(PtsParois2);
        end;
      except
         on E: Exception do AfficherMessage(E.Message);
      end;
    end;  // for

    // dernier polygone
    DessinerPolygoneSilhouette(MySilhouette);
    AfficherMessage('--> Mode silhouettes DONE');
    NbreSilhouettes := 1 + NbreSilhouettes;
    MySilhouette.ClearSilhouette(clWhite, clWhite);
  finally
    FreeAndNil(MySilhouette);//MySilhouette.Free;
  end;
end;

end.

// ==============================================================================

//******************************************************************************
// Exportations
// semble OK - A vérifier - Couleurs à implémenter.
// GPX: Semble OK
// KML: Semble OK
// GPX only: Export des visées rayonnantes OK
// Leaflet: OK pour les silhouettes - TODO: Implementer les entrées
procedure TBDDEntites.ExportForCarto(const FileName: TStringDirectoryFilename;
                                     const Convertisseur: TConversionSysteme;
                                     const QSystemeGeogEPSG: integer;
                                     const GISOutputFormat    : TGISOutputFormat;
                                     const GISOutputMode      : TGISOutputMode;
                                     const GISColorByItems    : TGISColorByItems;
                                     const Filtres: string;
                                     const PrefixStations: string;
                                     const CouleurDefaut: TColor;
                                     const WithEntrances: boolean);

var
  toto: String;
  FConvertisseurEPSG: TConversionSysteme;
  MyExportGIS: TExportGIS;
begin
  toto := '';
  if (gisCENTERLINES in GISOutputMode) then toto := toto + 'gisCENTERLINES, ';
  if (gisSILHOUETTES in GISOutputMode) then toto := toto + 'gisSILHOUETTES';
  // affectation du convertisseur
  FConvertisseurEPSG := Convertisseur;
  AfficherMessageErreur(Format('%s.ExportForCarto(%s): %s', [ClassName, FileName, toto]));
  //CouleurDefaut := clRed;
  // controle paramètres
  //{$ERROR Reconditionnement en cours}
  // ouverture fichier

  // TODO:A déplacer (utilise TToporobotStructure)
  try
    MyExportGIS := TExportGIS.Create;
    if (not MyExportGIS.Initialiser(F
                            FConvertisseurEPSG,
                            QSystemeGeogEPSG,
                            GISOutputFormat,
                            GISOutputMode,
                            GISColorByItems,
                            CouleurDefaut,
                            1200, 900,
                            FileName)) then Exit;

                const PtrBDDEntites  : TBDDEntites;
                         const PtrCoordConv   : TConversionSysteme;
                         const CodeEPSGOrigine: integer;
                         const GISOutputFormat: TGISOutputFormat;
                         const GISOutputMode  : TGISOutputMode;
                         const GISColorByItems: TGISColorByItems;
                         const CouleurDefault: TColor;
                         const QMapWidth, QMapHeight: integer;


    MyExportGIS.BeginExportGIS();
      case GISOutputMode of
        gisCENTERLINES: MyExportGIS.ExporterCenterlines();
        gisSILHOUETTES: MyExportGIS.ExporterSilhouettes();
      end;
      if (WithEntrances) then MyExportGIS.ExporterEntrances();
    MyExportGIS.EndExportGIS();
    MyExportGIS.Finaliser();
  finally
    FreeAndNil(MyExportGIS);//MyExportGIS.Free;
  end;

