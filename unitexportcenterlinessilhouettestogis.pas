unit unitExportCenterlinesSilhouettesToGIS;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, Graphics,
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitListesSimplesWithGeneriques,
  UnitKMLExport,
  UnitLeafletExport,
  UnitExportGeoJSON,
  UnitDXFDrawing,
  UnitObjetSerie,
  ConvertisseurJPC
  ;

type TExportCenterLinesSilhouettes = set of (expgisCENTERLINES, expgisSILHOUETTES, expgisENTRANCES, expgisPOIs, expgisNODES);
type TColoredBy = (expgisCOLOR_BY_DEFAULT, expgisCOLOR_BY_ENTRANCES, expgisCOLOR_BY_RESEAUX, expgisCOLOR_BY_SEANCES);


type

{ TExportGIS }

 TExportGIS = class
  private
    FConvertisseur                : TConversionSysteme;
    FDocTopo                      : TToporobotStructure2012;
    FDocumentTitle                : string;
    FBDDEntites                   : TBDDEntites;
    FProcProgression              : TProcDisplayProgression;
    FExportCenterLinesSilhouettes : TExportCenterLinesSilhouettes;
    FColoredBy                    : TColoredBy;
    FCenterlineColor              : TColor;
    FCenterlineOpacity            : byte;
    FSilhouetteColor              : TColor;
    FSilhouetteOpacity            : byte;
    FMyEPSG                       : TLabelSystemesCoordsEPSG;
    FCenterlineWidth              : double;

    function ExtractAndConvertBasePointBySerSt(const Ser: TNumeroSerie; const St: integer; out QLat, QLon, QAlt: double): boolean;
    function MakeSilhouetteOfSerie(const S: TObjSerie; out QPolygoneSilhouette: TArrayOfTProjUV): boolean;


  public
    function Initialiser(const QConvertisseur                : TConversionSysteme;
                          const QDocTopo                     : TToporobotStructure2012;
                          const QDocumentTitle               : string;
                          const QBDDEntites                  : TBDDEntites;
                          const QProcDisplayProgression      : TProcDisplayProgression;
                          const QExportCenterLinesSilhouettes: TExportCenterLinesSilhouettes;
                          const QColoredBy                   : TColoredBy;
                          const QCenterlineColor             : TColor;
                          const QCenterlineOpacity           : byte;
                          const QCenterlineWidth             : double;
                          const QSilhouetteColor             : TColor;
                          const QSilhouetteOpacity           : byte): boolean;
    procedure Finaliser();

    function  ExporterToKML(const QFilename: TStringDirectoryFilename): boolean;
    function  ExporterToOSM(const QFilename: TStringDirectoryFilename;
                            const MapWidthInPercent, MapHeightInPixels: integer;
                            const UseLocalJSLibraries: boolean): boolean;
    function  ExporterToGeoJSON(const QFilename: TStringDirectoryFilename): boolean;
    function  ExporterToDXF(const QFilename: TStringDirectoryFilename): boolean;

end;

implementation
uses
  DGCDummyUnit;



{ TExportGIS }

function TExportGIS.Initialiser(const QConvertisseur               : TConversionSysteme;
                                const QDocTopo                     : TToporobotStructure2012;
                                const QDocumentTitle: string;
                                const QBDDEntites                  : TBDDEntites;
                                const QProcDisplayProgression      : TProcDisplayProgression;
                                const QExportCenterLinesSilhouettes: TExportCenterLinesSilhouettes;
                                const QColoredBy                   : TColoredBy;
                                const QCenterlineColor             : TColor;
                                const QCenterlineOpacity           : byte;
                                const QCenterlineWidth             : double;
                                const QSilhouetteColor             : TColor;
                                const QSilhouetteOpacity           : byte): boolean;
begin
  Result := false;
  try
    AfficherMessage(Format('%s.Initialiser()', [ClassName]));
    FConvertisseur   := QConvertisseur;
    FDocTopo         := QDocTopo;
    FDocumentTitle   := QDocumentTitle;
    FBDDEntites      := QBDDEntites;
    FProcProgression := QProcDisplayProgression;
    FExportCenterLinesSilhouettes := QExportCenterLinesSilhouettes;
    FColoredBy                    := QColoredBy;

    FCenterlineColor              := QCenterlineColor;
    FCenterlineWidth              := QCenterlineWidth;
    FCenterlineOpacity            := QCenterlineOpacity;

    FSilhouetteColor              := QSilhouetteColor;
    FSilhouetteOpacity            := QSilhouetteOpacity;

    FMyEPSG                       := FConvertisseur.GetEPSGSystemeFromCodeEPSG(FDocTopo.GetCodeEPSGSystemeCoordonnees().CodeEPSG);
    result := true;
  except
    pass;
  end;
end;

procedure TExportGIS.Finaliser();
begin
  pass;
end;
function TExportGIS.ExtractAndConvertBasePointBySerSt(const Ser: TNumeroSerie; const St: integer; out QLat, QLon, QAlt: double): boolean;
var
  EE: TBaseStation;
  PP, MyPoint: TProjUV;
begin
  Result := FBDDEntites.GetEntiteViseeFromSerSt(Ser, St, EE);
  if (Result) then
  begin
    MyPoint.U := EE.PosStation.X;
    MyPoint.V := EE.PosStation.Y;
    PP := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, 4326, MyPoint);
    QLat := PP.U;
    QLon := PP.V;
    QAlt := EE.PosStation.Z;
  end;
end;

function TExportGIS.MakeSilhouetteOfSerie(const S: TObjSerie; out QPolygoneSilhouette: TArrayOfTProjUV): boolean;
var
  NbVisees, QIR, i, QIL: Integer;
  NumSerie  : TNumeroSerie;
  MyStation : TBaseStation;
  MyPoint   : TProjUV;
  QListeVx  : TListeSimple<TProjUV>;
begin
  result := false;
  NbVisees := S.GetNbVisees();
  NumSerie := S.GetNumeroDeSerie();
  //AfficherMessageErreur(Format('MakeSilhouetteOfSerie(%s - %d visees', [S.GetNomSerie(), NbVisees]));
  if (NbVisees < 2) then Exit(false);
  QListeVx := TListeSimple<TProjUV>.Create;
  try
    QListeVx.ClearListe();
    for i := 1 to NbVisees - 1 do
    begin
      if (not FBDDEntites.GetEntiteViseeFromSerSt(NumSerie, i, MyStation)) then continue;
      if (1 = i) then
      begin
        MyPoint.U := MyStation.PosOPD.X;
        MyPoint.V := MyStation.PosOPD.Y;
        MyPoint := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, 4326, MyPoint);
        //QListeVx.AddElement(MyPoint);
      end;
      MyPoint.U := MyStation.PosPD.X;
      MyPoint.V := MyStation.PosPD.Y;
      MyPoint := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, 4326, MyPoint);
      QListeVx.AddElement(MyPoint);
    end;
    for i := NbVisees - 1 downto 1 do
    begin
      if (not FBDDEntites.GetEntiteViseeFromSerSt(NumSerie, i, MyStation)) then continue;
      if (1 = i) then
      begin
        MyPoint.U := MyStation.PosOPG.X;
        MyPoint.V := MyStation.PosOPG.Y;
        MyPoint := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, 4326, MyPoint);
        //QListeVx.AddElement(MyPoint);
      end;
      MyPoint.U := MyStation.PosPG.X;
      MyPoint.V := MyStation.PosPG.Y;
      MyPoint := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, 4326, MyPoint);
      QListeVx.AddElement(MyPoint);
    end;
    // et on ferme le polygone
    QListeVx.AddElement(QListeVx.GetElement(0));
    SetLength(QPolygoneSilhouette, QListeVx.GetNbElements());
    for i := 0 to QListeVx.GetNbElements() - 1 do QPolygoneSilhouette[i] := QListeVx.GetElement(i);
    result := true;
  finally
    FreeAndNil(QListeVx);
  end;
end;

function TExportGIS.ExporterToKML(const QFilename: TStringDirectoryFilename): boolean;
const
  KML_FOLDER_CENTERLINES          = 'centerlines';
  KML_FOLDER_ENTRANCES            = 'entrances';
  KML_STYLE_CENTERLINE_BY_DEFAULT = 'DefaultStyleCenterline';
var
  DC: TKMLExport;
  procedure QExporterCenterLines();
  var
    i, Nb, s, NbSts: Integer;
    MySerie: TObjSerie;
    QLat, QLon, QAlt: double;
    EWE: String;
  begin
    Nb := FDocTopo.GetNbSeries();

    AfficherMessage(Format ('--> Centerlines: %d series', [Nb]));
    DC.DefineStylePoly(KML_STYLE_CENTERLINE_BY_DEFAULT, FCenterlineWidth, FCenterlineColor, FSilhouetteColor, FCenterlineOpacity, FSilhouetteOpacity);
    DC.BeginFolder(4, KML_FOLDER_CENTERLINES);
    DC.WriteCommentaire(Format('Centerlines: %d series', [Nb]));
    for i := 1 to Nb - 1 do
    begin
      MySerie := FDocTopo.GetSerie(i);
      //AfficherMessage(Format('Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]));
      EWE := Format('Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
      DC.WriteCommentaire(EWE);
      DC.BeginPolyline(Format('Serie%d', [MySerie.GetNumeroDeSerie()]), KML_STYLE_CENTERLINE_BY_DEFAULT);
      if (ExtractAndConvertBasePointBySerSt(MySerie.GetNoSerieDep(),
                                            MySerie.GetNoPointDep(),
                                            QLat, QLon, QAlt))
      then DC.AddVertex(QLat, QLon, QAlt, False);
      NbSts := MySerie.GetNbVisees();
      for s := 1 to NbSts - 1 do
      begin
        if (ExtractAndConvertBasePointBySerSt(MySerie.GetNumeroDeSerie(), s, QLat, QLon, QAlt))
        then DC.AddVertex(QLat, QLon, QAlt, true);
      end;
      DC.EndPolyline();
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 1, Nb, 10);
    end;
    DC.EndFolder(4, KML_FOLDER_CENTERLINES);
  end;
  procedure QExporterSilhouettes();
  var
    i, Nb: Integer;
    QPolygoneSilhouette: array of TProjUV;
    MySerie: TObjSerie;
    EWE: String;
  begin
    Nb := FDocTopo.GetNbSeries();
    AfficherMessage(Format ('--> Silhouettes: %d series', [Nb]));
    SetLength(QPolygoneSilhouette, 0);
    if (Nb < 2) then exit;
    for i := 1 to Nb - 1 do
    begin
      MySerie := FDocTopo.GetSerie(i);
      EWE := Format('Silhouettes: Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
      if (MakeSilhouetteOfSerie(MySerie, QPolygoneSilhouette)) then
      begin

      end;
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 0, Nb, 10);
    end;
  end;
  procedure QExporterEntrances();
  var
    i, Nb: Integer;
    MyEntrance: TEntrance;
    MyPoint, PP: TProjUV;
    EWE: String;
  begin
    Nb := FDocTopo.GetNbEntrances();
    AfficherMessage(Format ('--> Entrances: %d entrances', [Nb]));
    DC.BeginFolder(4, KML_FOLDER_ENTRANCES);
    for i := 0 to Nb - 1 do
    begin
      MyEntrance := FDocTopo.GetEntrance(i);
      EWE := Format('%d: %s', [i, MyEntrance.eNomEntree]);

      MyPoint.U  := MyEntrance.eXEntree;
      MyPoint.V  := MyEntrance.eYEntree;

      PP := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, 4326, MyPoint);

      DC.AddMarker(MyEntrance.eXEntree, MyEntrance.eYEntree, MyEntrance.eZEntree, PP.U, PP.V, MyEntrance.eNomEntree, format(FMTSERST, [MyEntrance.eRefSer, MyEntrance.eRefSt]), '', '', '');

      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 0, Nb, 10);
    end;
    DC.EndFolder(4, KML_FOLDER_ENTRANCES);
  end;
  procedure QExporterPOIs();
   begin
     //AfficherMessage(Format ('--> Points of interest: %d POIs', [Nb]));
   end;
   procedure QExporterNodes();
   var
     Nb: Integer;
   begin
     Nb := FBDDEntites.GetNbJonctions();
     AfficherMessage(Format ('--> Nodes: %d POIs', [Nb]));

   end;
begin
  result := false;
  AfficherMessage(Format('%s.ExporterToKML(%s)', [ClassName, QFilename]));
  FBDDEntites.SortBySerSts();  // Trier par séries et stations
  DC := TKMLExport.Create;
  try

    if (DC.Initialiser(QFilename, FDocumentTitle)) then
    begin
      if (expgisCENTERLINES in FExportCenterLinesSilhouettes) then QExporterCenterLines();
      if (expgisSILHOUETTES in FExportCenterLinesSilhouettes) then QExporterSilhouettes();
      if (expgisENTRANCES   in FExportCenterLinesSilhouettes) then QExporterEntrances();
      if (expgisPOIs        in FExportCenterLinesSilhouettes) then QExporterPOIs();
      if (expgisNODES       in FExportCenterLinesSilhouettes) then QExporterNodes();


      DC.Finaliser();
    end;

  finally
    FreeAndNil(DC);
  end;
  FBDDEntites.SortByDepth();   // et retrier la table

end;

function TExportGIS.ExporterToOSM(const QFilename: TStringDirectoryFilename;
                                  const MapWidthInPercent, MapHeightInPixels: integer;
                                  const UseLocalJSLibraries: boolean):boolean;
const
  OSM_STYLE_CENTERLINE_BY_DEFAULT = 'DefaultStyleCenterline';
  OSM_STYLE_SILHOUETTE_BY_DEFAULT = 'DefaultStyleSilhouettes';

var
  DC: TLeafletExport;
  ID_LAYER_CENTERLINES: integer;
  ID_LAYER_ENTRANCES  : integer;
  ID_LAYER_SILHOUETTES: integer;
  ID_LAYER_POIs       : integer;
  ID_LAYER_NODES      : integer;

  C1, C2: TPoint3Df;
  MyCentroide, PP: TProjUV;

  procedure QExporterEntrances();
  var
    i, Nb      : Integer;
    MyPoint, PP: TProjUV;
    EWE        : String;
    MyEntrance : TEntrance;
    QLayer: TOSMLayer;
  begin
    Nb := FDocTopo.GetNbEntrances();
    QLayer := DC.GetLayer(ID_LAYER_ENTRANCES);
    DC.BeginConditionalSection(True);
      for i := 0 to Nb - 1 do
      begin
        MyEntrance := FDocTopo.GetEntrance(i);
        EWE := Format('%d: %s', [i, MyEntrance.eNomEntree]);
        MyPoint.U  := MyEntrance.eXEntree;
        MyPoint.V  := MyEntrance.eYEntree;

        PP := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, CODE_EPSG_WGS84, MyPoint);
        DC.AddMarker(QLayer.LayerVarName,
                     MyEntrance.eXEntree,
                     MyEntrance.eYEntree,
                     MyEntrance.eZEntree,
                     PP.U, PP.V,
                     format(FMTSERST, [MyEntrance.eRefSer, MyEntrance.eRefSt]),
                     MyEntrance.eNomEntree,
                     '',
                     MyEntrance.eObserv,
                     ''  // pas de photo
                     );
        if (assigned(FProcProgression)) then FProcProgression(EWE, i, 0, Nb, 10);
      end;
    DC.FlushAllMarkers();
    DC.EndConditionalSection();
  end;
  procedure QExporterCenterLines();
  var
    i, Nb, NbSts, s : Integer;
    MySerie         : TObjSerie;
    QLat, QLon, QAlt: double;
    EWE             : String;
    QLayer: TOSMLayer;
  begin
    Nb := FDocTopo.GetNbSeries();
    QLayer := DC.GetLayer(ID_LAYER_CENTERLINES);
    DC.BeginConditionalSection(True);
    for i := 1 to Nb - 1 do
    begin
      MySerie := FDocTopo.GetSerie(i);
      NbSts := MySerie.GetNbVisees();
      if (Nb < 2) then Continue;
      EWE := Format('Centerlines: Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
      DC.BeginPolyline(QLayer.LayerVarName, Format('Serie%d', [MySerie.GetNumeroDeSerie()]), OSM_STYLE_CENTERLINE_BY_DEFAULT);
        if (ExtractAndConvertBasePointBySerSt(MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), QLat, QLon, QAlt))
        then DC.AddVertex(QLat, QLon, QAlt, False);
        NbSts := MySerie.GetNbVisees();
        for s := 1 to NbSts - 1 do
        begin
          if (ExtractAndConvertBasePointBySerSt(MySerie.GetNumeroDeSerie(), s, QLat, QLon, QAlt))
          then DC.AddVertex(QLat, QLon, QAlt, s = (NbSts-1));
        end;
      DC.EndPolyline();
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 1, Nb, 10);
    end;
    DC.EndConditionalSection();
  end;
  procedure QExporterSilhouettes();
  var
    i, Nb, v: Integer;
    QPolygoneSilhouette: array of TProjUV;
    QLayer: TOSMLayer;
    MySerie: TObjSerie;
    EWE: String;
  begin
    Nb := FDocTopo.GetNbSeries();
    QLayer := DC.GetLayer(ID_LAYER_SILHOUETTES);
    DC.BeginConditionalSection(True);

    AfficherMessage(Format ('--> Silhouettes: %d series', [Nb]));
    SetLength(QPolygoneSilhouette, 0);
    if (Nb < 2) then exit;
    for i := 1 to Nb - 1 do
    begin
      MySerie := FDocTopo.GetSerie(i);
      EWE := Format('Silhouettes: Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
      if (MakeSilhouetteOfSerie(MySerie, QPolygoneSilhouette)) then
      begin
        DC.BeginPolygon(QLayer.LayerVarName, Format('Serie: %d: %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]), '');
          for v := 0 to High(QPolygoneSilhouette) do DC.AddVertex(QPolygoneSilhouette[v].U, QPolygoneSilhouette[v].V, 0.00, v = High(QPolygoneSilhouette));
        DC.EndPolygon();
      end;
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 1, Nb, 10);
    end;
    DC.EndConditionalSection();
  end;
  procedure QExporterPOIs();
  var
    i, Nb: Integer;
    QLayer: TOSMLayer;
    MyPOI: TPointOfInterest;
    EWE: String;
    QLat, QLon, QAlt: double;
  begin
    Nb := FDocTopo.GetNbPointsOfInterests();
    AfficherMessage(Format ('--> Points of interest: %d POIs', [Nb]));
    if (0 = Nb) then exit;
    QLayer := DC.GetLayer(ID_LAYER_POIs);
    DC.BeginConditionalSection(True);
      for i := 0 to Nb - 1 do
      begin
        MyPOI := FDocTopo.GetPointOfInterest(i);
        EWE := Format('POIs: %d: %d.%d: %s', [i, MyPOI.Serie, MyPOI.Station, MyPOI.Description]);
        if (ExtractAndConvertBasePointBySerSt(MyPOI.Serie, MyPOI.Station, QLat, QLon, QAlt)) then
        begin
          DC.AddMarker(QLayer.LayerVarName,
                       MyPOI.Coordinates.X, MyPOI.Coordinates.Y, MyPOI.Coordinates.Z,
                       QLat, QLon,
                       format(FMTSERST, [MyPOI.Serie, MyPOI.Station]),
                       MyPOI.Description,
                       Format('X = %s, Y = %s, Z = %s', [FormatterNombreAvecSepMilliers(MyPOI.Coordinates.X), FormatterNombreAvecSepMilliers(MyPOI.Coordinates.Y), FormatterNombreAvecSepMilliers(MyPOI.Coordinates.Z)]),
                       '',
                       '' // pas de photo
                     );
        end;
        if (assigned(FProcProgression)) then FProcProgression(EWE, i, 0, Nb, 1);
      end;
    DC.FlushAllMarkers();
    DC.EndConditionalSection();
  end;
  procedure QExporterNodes();
  var
    Nb, i: Integer;
    MyNode: TJonctionXYZ;
    QLayer: TOSMLayer;
    MyPoint: TProjUV;
    EWE: String;
  begin
    Nb := FBDDEntites.GetNbJonctions();
    AfficherMessage(Format ('--> Nodes: %d nodes', [Nb]));
    QLayer := DC.GetLayer(ID_LAYER_NODES);
    DC.BeginConditionalSection(True);
      for i := 0 to Nb - 1 do
      begin
        MyNode := FBDDEntites.GetJonction(i);
        EWE := Format('Jonctions: %d: %d%d', [i, MyNode.NoSer, MyNode.NoSt]);
        MyPoint.U  := MyNode.X;
        MyPoint.V  := MyNode.Y;

        PP := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, CODE_EPSG_WGS84, MyPoint);
        DC.AddMarker(QLayer.LayerVarName,
                     MyNode.X, MyNode.Y, MyNode.Z,
                     PP.U, PP.V,
                     format(FMTSERST, [MyNode.NoSer, MyNode.NoSt]),
                     MyNode.IDJonction,
                     Format('X = %s, Y = %s, Z = %s', [FormatterNombreAvecSepMilliers(MyNode.X), FormatterNombreAvecSepMilliers(MyNode.Y), FormatterNombreAvecSepMilliers(MyNode.Z)]),
                     '',
                     '' // pas de photo
                     );
        if (assigned(FProcProgression)) then FProcProgression(EWE, i, 0, Nb, 100);
      end;
    DC.FlushAllMarkers();
    DC.EndConditionalSection();
  end;
begin
  result := false;
  AfficherMessage(Format('%s.ExporterToOSM(%s)', [ClassName, QFilename, FDocumentTitle]));
  FBDDEntites.SortBySerSts();  // Trier par séries et stations
  DC := TLeafletExport.Create;
  try
    C1 := FBDDEntites.GetCoinBasGauche();
    C2 := FBDDEntites.GetCoinHautDroit();
    // calcul du centroide
    MyCentroide.U := 0.50 * (C1.X + C2.X);
    MyCentroide.V := 0.50 * (C1.Y + C2.Y);
    PP := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, CODE_EPSG_WGS84, MyCentroide);
    if (DC.Initialiser(QFilename, FDocumentTitle, MapWidthInPercent, MapHeightInPixels, PP.U, PP.V, UseLocalJSLibraries)) then
    begin
      DC.DefineStylePoly(OSM_STYLE_CENTERLINE_BY_DEFAULT, FCenterlineWidth, FCenterlineColor, FSilhouetteColor, FCenterlineOpacity, FSilhouetteOpacity);
      DC.AddLayer(false, true, 'entrances'  , 'Entrees'           , 'GHTopo', ''    , OSM_MARKER_STYLE_CIRCLE ,  5.00, clRed    , 0.50);
      ID_LAYER_ENTRANCES   := DC.GetNbLayers() - 1;
      DC.AddLayer(false, true, 'centerlines', 'Polygonales'       , 'GHTopo', ''    , OSM_MARKER_STYLE_CIRCLE ,  5.00, clBlue   , 0.50);
      ID_LAYER_CENTERLINES := DC.GetNbLayers() - 1;
      DC.AddLayer(false, true, 'silhouettes', 'Silhouettes'       , 'GHTopo', ''    , OSM_MARKER_STYLE_CIRCLE ,  5.00, clGreen  , 0.50);
      ID_LAYER_SILHOUETTES := DC.GetNbLayers() - 1;
      DC.AddLayer(false, true, 'POIs'       , 'Points d''intérêt' , 'GHTopo', ''    , OSM_MARKER_STYLE_DELTA  ,  5.00, clRed    , 0.50);
      ID_LAYER_POIs        := DC.GetNbLayers() - 1;
      DC.AddLayer(false, true, 'noeuds'     , 'Noeuds'            , 'GHTopo', ''    , OSM_MARKER_STYLE_LOSANGE,  2.50, clYellow , 0.50);
      ID_LAYER_NODES       := DC.GetNbLayers() - 1;
      DC.WriteHeader();
      if (expgisCENTERLINES in FExportCenterLinesSilhouettes) then QExporterCenterLines();
      if (expgisSILHOUETTES in FExportCenterLinesSilhouettes) then QExporterSilhouettes();
      if (expgisENTRANCES   in FExportCenterLinesSilhouettes) then QExporterEntrances();
      if (expgisPOIs        in FExportCenterLinesSilhouettes) then QExporterPOIs();
      if (expgisNODES       in FExportCenterLinesSilhouettes) then QExporterNodes();
      DC.WriteFooter();
      DC.Finaliser();
    end;
  finally
    FreeAndNil(DC);
  end;
  FBDDEntites.SortByDepth();   // et retrier la table
end;


function TExportGIS.ExporterToGeoJSON(const QFilename: TStringDirectoryFilename): boolean;
var
  DC: TGeoJSONExport;
  ID_LAYER_CENTERLINES: integer;
  ID_LAYER_ENTRANCES  : integer;
  ID_LAYER_SILHOUETTES: integer;
  ID_LAYER_POIs       : integer;
  ID_LAYER_NODES      : integer;
  MyPointCG: TProjUV;
  // calcul des centroides
  procedure QCalcCentroid();
  var
    C1, C2: TPoint3Df;
  begin
    C1 := FBDDEntites.GetCoinBasGauche();
    C2 := FBDDEntites.GetCoinHautDroit();
    MyPointCG.U := 0.50 * (C1.X + C2.X);
    MyPointCG.V := 0.50 * (C1.Y + C2.Y);
    MyPointCG := FConvertisseur.ConversionSyst1ToSyst2EPSG(FMyEPSG.CodeEPSG, 4326, MyPointCG);
  end;
  procedure QExporterCenterLines();
  var
    Nb, i, s, NbSts: Integer;
    MySerie: TObjSerie;
    EWE: String;
    QLat, QLon, QAlt: double;
  begin
    Nb := FDocTopo.GetNbSeries();
    for i := 1 to Nb - 1 do
    begin
      MySerie := FDocTopo.GetSerie(i);
      NbSts := MySerie.GetNbVisees();
      if (Nb < 2) then Continue;
      EWE := Format('Centerlines: Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
      DC.BeginPolyline(Format('Serie%d', [MySerie.GetNumeroDeSerie()]), '');
        if (ExtractAndConvertBasePointBySerSt(MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), QLat, QLon, QAlt))
        then DC.AddVertex(QLat, QLon, QAlt, False);


        for s := 1 to NbSts - 1 do
        begin
          if (ExtractAndConvertBasePointBySerSt(MySerie.GetNumeroDeSerie(), s, QLat, QLon, QAlt))
          then DC.AddVertex(QLat, QLon, QAlt, s = (NbSts-1));
        end;
      DC.EndPolyline(i = (Nb - 1));
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 1, Nb, 10);
    end;
     //****************
  end;
  procedure QExporterSilhouettes();
  var
    i, v, Nb: Integer;
    QPolygoneSilhouette: array of TProjUV;
    MySerie: TObjSerie;
    EWE: String;
  begin
    Nb := FDocTopo.GetNbSeries();
    AfficherMessage(Format ('--> Silhouettes: %d series', [Nb]));
    SetLength(QPolygoneSilhouette, 0);
    if (Nb < 2) then exit;
    for i := 1 to Nb - 1 do
    begin
      MySerie := FDocTopo.GetSerie(i);
      EWE := Format('Silhouettes: Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
      if (MakeSilhouetteOfSerie(MySerie, QPolygoneSilhouette)) then
      begin
        DC.BeginPolygon(Format('Serie: %d: %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]), '');
          for v := 0 to High(QPolygoneSilhouette) do DC.AddVertex(QPolygoneSilhouette[v].U, QPolygoneSilhouette[v].V, 0.00, v = High(QPolygoneSilhouette));
        DC.EndPolygon(i = (Nb - 1));
      end;
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 1, Nb, 10);
    end;
  end;
begin
  result := false;
  AfficherMessage(Format('%s.ExporterToGeoJSON(%s)', [ClassName, QFilename, FDocumentTitle]));
  FBDDEntites.SortBySerSts();  // Trier par séries et stations
  DC := TGeoJSONExport.Create;
  try
    QCalcCentroid();
    AfficherMessageErreur(Format('--> Centroid: Lon: %.8f, Lat: %.8f', [MyPointCG.V, MyPointCG.U]));
    if (DC.Initialiser(QFilename,'Cavite001', MyPointCG.V, MyPointCG.U, clRed, 255)) then
    begin
      DC.AddLayer('entrances');
      DC.AddLayer('centerlines');
      DC.AddLayer('silhouettes');
      DC.AddLayer('POIs');
      DC.AddLayer('noeuds');
      DC.WriteHeader();
      // le dessin ici
      if (expgisCENTERLINES in FExportCenterLinesSilhouettes) then QExporterCenterLines();
      //if (expgisSILHOUETTES in FExportCenterLinesSilhouettes) then QExporterSilhouettes();

      DC.WriteFooter();
      DC.Finaliser();
    end;
  finally
    FreeAndNil(DC);
  end;
  FBDDEntites.SortByDepth();   // et retrier la table
end;


function TExportGIS.ExporterToDXF(const QFilename: TStringDirectoryFilename): boolean;
const
  LAYER_0               = 'Layer0';                ID_LAYER_0              = 0;
  LAYER_ENTRANCES       = 'ENTRANCES';             ID_LAYER_ENTRANCES      = 1;
  LAYER_CENTERLINES     = 'CENTERLINES';           ID_LAYER_CENTERLINES    = 2;
  LAYER_RADIANT_SHOTS   = 'RADIANTSHOTS';          ID_LAYER_RADIANT_SHOTS  = 3;
  LAYER_CROSS_SECTIONS  = 'CROSSSECTIONS';         ID_LAYER_CROSS_SECTIONS = 4;
  LAYER_STATIONS        = 'STATIONS';              ID_LAYER_STATIONS       = 5;
  LAYER_SILHOUETTES     = 'SILHOUETTES';           ID_LAYER_SILHOUETTES    = 6;
  LAYER_NOEUDS          = 'NOEUDS';                ID_LAYER_NOEUDS         = 7;
var
  MyExport: TDXFExport;
  C1, C2: TPoint3Df;
  procedure QExporterCenterLines();
  var
    i, Nb, NbSts, s : Integer;
    MySerie         : TObjSerie;
    QLayer: TDXFLayer;
    EE, VR: TBaseStation;
    EWE: String;
  begin
    Nb := FDocTopo.GetNbSeries();
    QLayer := MyExport.GetLayer(ID_LAYER_CENTERLINES);
    for i := 1 to Nb - 1 do
    begin
      MySerie := FDocTopo.GetSerie(i);
      EWE := Format('Centerlines: Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
      MyExport.BeginPolyline(QLayer.LayerName);
        if (FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), EE))
        then MyExport.AddVertex(QLayer.LayerName, EE.PosStation.X, EE.PosStation.Y, EE.PosStation.Z);
        NbSts := MySerie.GetNbVisees();
        for s := 1 to NbSts - 1 do
        begin
          if (FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNumeroDeSerie(), s, EE)) then
            MyExport.AddVertex(QLayer.LayerName, EE.PosStation.X, EE.PosStation.Y, EE.PosStation.Z);
        end;
      MyExport.EndPolyline(QLayer.LayerName);
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 1, Nb, 100);
    end;
    // sections
    QLayer := MyExport.GetLayer(ID_LAYER_CROSS_SECTIONS);
    for i := 1 to Nb - 1 do
    begin
      MySerie := FDocTopo.GetSerie(i);
      EWE := Format('Sections: Serie: %d - %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
      NbSts := MySerie.GetNbVisees();
      for s := 1 to NbSts - 1 do
      begin
        if (FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNumeroDeSerie(), s, EE)) then
        MyExport.DrawLine(QLayer.LayerName, EE.PosPD.X, EE.PosPD.Y, EE.PosPD.Z, EE.PosPG.X, EE.PosPG.Y, EE.PosPG.Z);
      end;
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 1, Nb, 100);
    end;
    // visées radiantes
    QLayer := MyExport.GetLayer(ID_LAYER_RADIANT_SHOTS);
    Nb := FBDDEntites.GetNbEntitesAntennes();
    if (Nb > 0) then
    begin
      for i := 1 to Nb - 1 do
      begin
        VR := FBDDEntites.GetEntiteAntenne(i);
        MyExport.DrawLine(QLayer.LayerName, VR.PosExtr0.X, VR.PosExtr0.Y, VR.PosExtr0.Z, VR.PosStation.X, VR.PosStation.Y, VR.PosStation.Z);
        if (assigned(FProcProgression) and (0 = i mod 2000)) then
        begin
          EWE := Format('Radiantes: %d / %d', [i, Nb]);
          FProcProgression(EWE, i, 1, Nb, 1000);
        end;
      end;
    end;
  end;
  procedure QExporterNodes();
  const
    QR = 1.00;
  var
    Nb, i: Integer;
    MyNode: TJonctionXYZ;
    QLayer: TDXFLayer;
    EWE: String;
  begin
    Nb := FBDDEntites.GetNbJonctions();
    AfficherMessage(Format ('--> Nodes: %d nodes', [Nb]));
    QLayer := MyExport.GetLayer(ID_LAYER_NOEUDS);
    for i := 0 to Nb - 1 do
    begin
      MyNode := FBDDEntites.GetJonction(i);
      EWE := Format('Jonctions: %d: %d%d', [i, MyNode.NoSer, MyNode.NoSt]);
      MyExport.DrawCircle(QLayer.LayerName, MyNode.X, MyNode.Y, MyNode.Z, QR);
      //MyExport.DrawTexte(QLayer.LayerName, MyNode.X + QR, MyNode.Y + QR, MyNode.Z, 3.00, Format(FMTSERST, [MyNode.NoSer, MyNode.NoSt]));
      if (assigned(FProcProgression)) then FProcProgression(EWE, i, 0, Nb, 100);
    end;
  end;
begin
  result := false;
  AfficherMessage(Format('%s.ExporterToDXF(%s)', [ClassName, QFilename, FDocumentTitle]));
  MyExport := TDXFExport.Create;
  FBDDEntites.SortBySerSts();  // Trier par séries et stations
  try
    if (MyExport.Initialiser(QFilename)) then
    begin
      C1 := FBDDEntites.GetCoinBasGauche();
      C2 := FBDDEntites.GetCoinHautDroit();
      MyExport.SetLimitesDessin(C1, C2);
      MyExport.AddLayer(LAYER_0             , 32, 0);
      MyExport.AddLayer(LAYER_ENTRANCES     , 64, 1);
      MyExport.AddLayer(LAYER_CENTERLINES   , 80, 5);
      MyExport.AddLayer(LAYER_RADIANT_SHOTS , 64, 1);
      MyExport.AddLayer(LAYER_CROSS_SECTIONS, 64, 1);
      MyExport.AddLayer(LAYER_STATIONS      , 64, 1);
      MyExport.AddLayer(LAYER_SILHOUETTES   , 84, 1);
      MyExport.AddLayer(LAYER_NOEUDS        , 83, 1);
      MyExport.writeHeader();
      if (expgisCENTERLINES in FExportCenterLinesSilhouettes) then QExporterCenterLines();
      //if (expgisSILHOUETTES in FExportCenterLinesSilhouettes) then QExporterSilhouettes();
      //if (expgisENTRANCES   in FExportCenterLinesSilhouettes) then QExporterEntrances();
      //if (expgisPOIs        in FExportCenterLinesSilhouettes) then QExporterPOIs();
      if (expgisNODES       in FExportCenterLinesSilhouettes) then QExporterNodes();
      MyExport.writeFooter();
      MyExport.Finaliser();
    end;
  finally
    FreeAndNil(MyExport);
  end;
  FBDDEntites.SortByDepth();   // et retrier la table
end;

end.

