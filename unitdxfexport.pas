unit unitDXFExport;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics;

// Génération du document-programme HTML incorporant une carte OSM
type  TDXFExport = class
  private

    FDocTitle          : string;


    FP                 : TextFile;
    FCentroideLat, FCentroideLon : Double; // la carte sera centrée sur ces coordonnées
    FCurrentLineWidth    : double;      // styles courants
    FCurrentLineColor    : TColor;

    FCurrentLineOpacity  : double;
    FCurrentFillColor    : TColor;

    FCurrentFillOpacity  : double;
    FCurrentTagString    : string;
    FCurrentObjectName   : string;
    procedure WriteLine(const S: string);
    procedure WriteSectionSeparator();
  public
    function  Initialiser(const QFilename: string;
                          const QDocTitle: string;
                          const QCentroideLat, QCentroideLon: Double): boolean;
    procedure Finaliser();
    // construction du document-programme, déclaration des fonctions JavaScript
    procedure WriteHeader();
    procedure WriteFooter();
    // gestion des couches
    (*
    procedure AddLayer(const L: TOSMLayer); overload;
    procedure AddLayer(const QLayerIsAMap, QLayerDisplayed: boolean;
                       const QLayerName : string;
                       const QLayerTitle: string;
                       const QLayerAttribution: string;
                       const QURL: String;
                       const QLayerMarkerStyle: integer;
                       const QLayerMarkerSize: double;
                       const QLayerMarkerColor: TColor); overload;

    function  GetLayer(const Idx: integer): TOSMLayer;
    function  GetNbLayers(): integer;
    //*)
    // feuilles de style
    //procedure DefineStylePoly(const StyleName: string; const LineWidth: double; const LineColor, FillColor: TColor; const LineOpacity, FillOpacity: byte);
    // objets graphiques: marqueurs, polylignes et polygones
    procedure AddMarker(const QLayerName: string;
                        const QNativeCoordX, QNativeCoordY, QNativeCoordZ;
                        const QEtiquette: string);
    procedure BeginPolygon(const QLayerName: string; const QPolyName: string; const QStyleURL: string);
    procedure AddVertex(const Lat, Lon, Alt: double; const IsLast: boolean);
    procedure EndPolygon();
    procedure BeginPolyline(const QLayerName: string; const QPolyName: string; const QStyleURL: string);
    procedure EndPolyline();
    // utilisé pour structurer le document
end;

implementation

{ TDXFExport }

procedure TDXFExport.WriteLine(const S: string);
begin
  WriteLn(FP, S);
end;

procedure TDXFExport.WriteSectionSeparator();
begin

end;

function TDXFExport.Initialiser(const QFilename: string; const QDocTitle: string; const QCentroideLat, QCentroideLon: Double): boolean;
begin
  AssignFile(FP, QFilename);

end;

procedure TDXFExport.Finaliser();
begin
  try
    CloseFile(FP);
  finally
  end;

end;

procedure TDXFExport.WriteHeader();
begin

end;

procedure TDXFExport.WriteFooter();
begin

end;

procedure TDXFExport.AddMarker(const QLayerName: string; const QNativeCoordX, QNativeCoordY, QNativeCoordZ; const QEtiquette: string);
begin

end;

procedure TDXFExport.BeginPolygon(const QLayerName: string; const QPolyName: string; const QStyleURL: string);
begin

end;

procedure TDXFExport.AddVertex(const Lat, Lon, Alt: double; const IsLast: boolean);
begin

end;

procedure TDXFExport.EndPolygon();
begin

end;

procedure TDXFExport.BeginPolyline(const QLayerName: string; const QPolyName: string; const QStyleURL: string);
begin

end;

procedure TDXFExport.EndPolyline();
begin

end;

end.

