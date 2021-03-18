unit UnitDXFDrawing;

{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  Common,
  Classes, SysUtils;

type TDXFLayer = record
  LayerName       : string;
  AcadColor       : integer;
  LineWidth       : double;
end;

type TListeDXFLayers = class(TList)
  private

  public
    procedure ClearListe();
    procedure AddElement(const L: TDXFLayer);
    function  GetElement(const Idx: integer): TDXFLayer;
    procedure PutElement(const Idx: integer; const L: TDXFLayer);
    function  RemoveElement(const Idx: integer): boolean;
    function  GetNbElements(): integer;
end;

type

{ TDXFExport }

  TDXFExport = class
  private
    FpFDXF: TextFile;
    FCoinBasGauche: TPoint3Df;
    FCoinHautDroit: TPoint3Df;

    FListeLayers: TListeDXFLayers;




    procedure WrtDXFExtMax();
    procedure WrtDXFExtMin();
    procedure WrtDXFLimMax();
    procedure WrtDXFLimMin();


    procedure wrtLn(const S: string);
  public
    function  Initialiser(const F: string): boolean;
    procedure Finaliser();
    procedure SetLimitesDessin(const C1, C2: TPoint3Df);
    procedure AddLayer(const QLayerName: string; const AcadColor: integer; const LineWidth: double);
    function  GetLayer(const QIdx: integer): TDXFLayer;
    // header et footer
    procedure writeHeader();
    procedure writeFooter();
    procedure writeLayer(const DXFLayer: TDXFLayer);
    procedure BeginSection(const NomSection: string; const Tag: integer);
    procedure EndSection(const NomSection: string);



    // dessins
    procedure DrawCircle(const QLayer: string; const qX, qY, qZ, qR: double);
    procedure DrawLine(const QLayer: string; const qX1, qY1, qZ1, qX2, qY2, qZ2: double);
    procedure DrawTexte(const QLayer: string; const qX1, qY1, qZ1: double; const QTextH: double; const QText: string);
    procedure BeginPolyline(const QLayer: string);
    procedure AddVertex(const QLayer: string; const X, Y, Z: double);
    procedure EndPolyline(const QLayer: string);

end;

implementation
uses
  DGCDummyUnit;
function QRealEscapeString(const S: string): string;
begin
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
end;

// pour se rendre indépendant des unités de GHTopo
function QFormatterNombreReel(const Value: double; const NbDecs: integer = 6): string;
var
  EWE: String;
begin
  EWE := Format('%%.%df', [NbDecs]);
  // remplacer les points par des virgules
  Result := Format(EWE, [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
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


{ TListeDXFLayers }
procedure TListeDXFLayers.ClearListe();
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

procedure TListeDXFLayers.AddElement(const L: TDXFLayer);
var
  pE: ^TDXFLayer;
begin
  New(pE);
  pE^ := L;
  self.Add(pE);
end;
function TListeDXFLayers.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;

function TListeDXFLayers.GetElement(const Idx: integer): TDXFLayer;
begin

  Result := TDXFLayer(Items[Idx]^);
end;

procedure TListeDXFLayers.PutElement(const Idx: integer; const L: TDXFLayer);
begin
  TDXFLayer(Items[Idx]^) := L;
end;


function TListeDXFLayers.GetNbElements(): integer;
begin
  Result := self.Count;
end;



//******************************************************************************
// TDXFExport
// dessin d'entités DXF
procedure TDXFExport.wrtLn(const S: string);
begin
  Write(FpFDXF, S + #13+#10);
end;

function TDXFExport.Initialiser(const F: string): boolean;
begin
  Result := False;
  try
    FListeLayers := TListeDXFLayers.Create;
    FListeLayers.ClearListe();
    AssignFile(FpFDXF, F);
    ReWrite(FpFDXF);
    Result := True;
  except

  end;
end;

procedure TDXFExport.SetLimitesDessin(const C1, C2: TPoint3Df);
begin
  FCoinBasGauche := C1;
  FCoinHautDroit := C2;
end;



procedure TDXFExport.Finaliser();
begin
  try
    FListeLayers.ClearListe();
  finally
    CloseFile(FpFDXF);
    FreeAndNil(FListeLayers);
  end;
end;

procedure TDXFExport.BeginSection(const NomSection: string; const Tag: integer);
begin
  wrtLn('   0');
  wrtLn('SECTION');
  wrtLn(Format('  %d', [Tag]));
  wrtLn(UpperCase(NomSection));
end;


procedure TDXFExport.EndSection(const NomSection: string);
begin
  wrtLn('  0');
  wrtLn('ENDSEC');
end;

procedure TDXFExport.AddLayer(const QLayerName: string; const AcadColor: integer; const LineWidth: double);
var
  MyLayer: TDXFLayer;
begin
  MyLayer.LayerName := QLayerName;
  MyLayer.AcadColor := AcadColor;
  MyLayer.LineWidth := LineWidth;
  FListeLayers.AddElement(MyLayer);
end;

function TDXFExport.GetLayer(const QIdx: integer): TDXFLayer;
begin
  Result := FListeLayers.GetElement(QIdx);
end;

procedure TDXFExport.DrawCircle(const QLayer: string; const qX, qY, qZ, qR: double);
begin
  wrtLn('  0');
  wrtLn('CIRCLE');
  wrtLn('8');
  wrtLn(QLayer);
  wrtLn(' 10');
  wrtLn(QFormatterNombreReel(qX));
  wrtLn(' 20');
  wrtLn(QFormatterNombreReel(qY));
  wrtLn(' 30');
  wrtLn(QFormatterNombreReel(qZ));
  wrtLn(' 40');
  wrtLn(QFormatterNombreReel(qR));
  //wrtLn('  0');

end;

procedure TDXFExport.WrtDXFExtMin();
begin

  wrtLn('9');
  wrtLn('$EXTMIN');
  wrtLn('10');
  wrtLn(QFormatterNombreReel(FCoinBasGauche.X));
  wrtLn('20');
  wrtLn(QFormatterNombreReel(FCoinBasGauche.Y));
  wrtLn('30');
  wrtLn(QFormatterNombreReel(FCoinBasGauche.Z));
  //wrtLn('  0');
end;

procedure TDXFExport.WrtDXFExtMax();
begin

  wrtLn('9');
  wrtLn('$EXTMAX');
  wrtLn('10');
  wrtLn(QFormatterNombreReel(FCoinHautDroit.X));
  wrtLn('20');
  wrtLn(QFormatterNombreReel(FCoinHautDroit.Y));
  wrtLn('30');
  wrtLn(QFormatterNombreReel(FCoinHautDroit.Z));
  //wrtLn('  0');
end;
procedure TDXFExport.WrtDXFLimMin();
begin

  wrtLn('9');
  wrtLn('$LIMMIN');
  wrtLn('10');
  wrtLn(QFormatterNombreReel(FCoinBasGauche.X));
  wrtLn('20');
  wrtLn(QFormatterNombreReel(FCoinBasGauche.Y));
  wrtLn('30');
  wrtLn(QFormatterNombreReel(FCoinBasGauche.Z));
  //wrtLn('  0');
end;

procedure TDXFExport.WrtDXFLimMax();
begin
  wrtLn('9');
  wrtLn('$LIMMAX');
  wrtLn('10');
  wrtLn(QFormatterNombreReel(FCoinHautDroit.X));
  wrtLn('20');
  wrtLn(QFormatterNombreReel(FCoinHautDroit.Y));
  wrtLn('30');
  wrtLn(QFormatterNombreReel(FCoinHautDroit.Z));
  //wrtLn('  0');
end;

procedure TDXFExport.writeHeader();
const
  AUTOCAD_VERSION = 'AC1006';
    (*
  Release 5 / Version 2.0 	AC1.50
Release 6 / Version 2.1 	AC2.10
Release 7 / Version 2.5 	AC1002
Release 8 / Version 2.6 	AC1003
Release 9 	AC1004
Release 10 	AC1006
Release 11/12 	AC1009
Release 13 	AC1012
Release 14 	AC1014
Release 2000/0i/2 	AC1015
Release 2004/5/6 	AC1018
Release 2007/8/9 	AC1021
Release 2010/11/12 	AC1024
Release 2013/14 	AC1027
//*)
var
  i, NbLayers: Integer;
begin
  wrtLn('  0');                       // 0
  wrtLn('SECTION');                   // SECTION
  wrtLn('  2');                       //   2
  wrtLn('HEADER');                    // HEADER
  wrtLn('  9');                       //   9
  wrtLn('$ACADVER');                  // $ACADVER
  wrtLn('  1');                       //   1
  wrtLn(AUTOCAD_VERSION);                    // AC1006

  WrtDXFExtMin();
  WrtDXFExtMax();
  //WrtDXFLimMin();
  //WrtDXFLimMax();
  wrtLn('  0');                       //   0
  wrtLn('ENDSEC');                    // ENDSEC
  wrtLn('  0');                       //   0
  wrtLn('SECTION');                   // SECTION
  wrtLn('  2');                       //   2
  wrtLn('TABLES');                    // TABLES
  wrtLn('  0');                       //   0
  wrtLn('TABLE');                     // TABLE
  wrtLn('  2');                       //   2
  wrtLn('LAYER');                     // LAYER
  wrtLn('  0');                       //   0
  NbLayers := FListeLayers.GetNbElements();
  for i := 0 to NbLayers - 1 do writeLayer(FListeLayers.GetElement(i));
  wrtLn('ENDTAB');
  wrtLn('  0');                       //
  wrtLn('ENDSEC');
  wrtLn('  0');                       //
  wrtLn('SECTION');
  wrtLn('  2');                       //
  wrtLn('ENTITIES');
  //wrtLn('  0');                       //

end;

procedure TDXFExport.writeFooter();
begin
  wrtLn('  0');
  wrtLn('ENDSEC');
  wrtLn('  0');
  wrtLn('EOF');
end;

procedure TDXFExport.writeLayer(const DXFLayer: TDXFLayer);
begin
  wrtLn('LAYER');                     // LAYER
  wrtLn('  2');                       //   2
  wrtLn(DXFLayer.LayerName);
  wrtLn(' 70');
  wrtLn(Format('    %d', [DXFLayer.AcadColor]));
  wrtLn(' 62 ');
  wrtLn(QFormatterNombreReel(DXFLayer.LineWidth));
  wrtLn('  6');
  wrtLn('CONTINUOUS');
  wrtLn('  0');
end;

// dessin des objets de base
procedure TDXFExport.DrawTexte(const QLayer: string; const qX1, qY1, qZ1: double; const QTextH: double; const QText: string);
begin
  wrtLn('  0');
  wrtLn('TEXT');
  wrtLn('8');
  wrtLn(QLayer);
  wrtLn(' 10');
  wrtLn(QFormatterNombreReel(qX1));
  wrtLn(' 20');
  wrtLn(QFormatterNombreReel(qY1));
  wrtLn(' 30');
  wrtLn(QFormatterNombreReel(qZ1));
  wrtLn(' 40');
  wrtLn(QFormatterNombreReel(QTextH));
  wrtLn(' 1');
  wrtLn(QText);
  wrtLn('  0');
end;

procedure TDXFExport.DrawLine(const QLayer: string; const qX1, qY1, qZ1, qX2, qY2, qZ2: double);
begin
  wrtLn('  0');
  wrtLn('LINE');
  wrtLn('8');
  wrtLn(QLayer);
  wrtLn(' 10');
  wrtLn(QFormatterNombreReel(qX1));
  wrtLn(' 20');
  wrtLn(QFormatterNombreReel(qY1));
  wrtLn(' 30');
  wrtLn(QFormatterNombreReel(qZ1));
  wrtLn(' 11');
  wrtLn(QFormatterNombreReel(qX2));
  wrtLn(' 21');
  wrtLn(QFormatterNombreReel(qY2));
  wrtLn(' 31');
  wrtLn(QFormatterNombreReel(qZ2));
  //wrtLn('  0');
end;

procedure TDXFExport.BeginPolyline(const QLayer: string);
begin
   wrtLn('  0');
   wrtLn('POLYLINE');
   wrtLn('8');
   wrtLn(QLayer);
   wrtLn(' 10');
   wrtLn(QFormatterNombreReel(0));
   wrtLn(' 20');
   wrtLn(QFormatterNombreReel(0));
   wrtLn(' 30');
   wrtLn(QFormatterNombreReel(0));
   wrtLn(' 39');
   wrtLn(QFormatterNombreReel(0));
   wrtLn(' 70');
   wrtLn(Format('  %d', [0]));   // Polyline flag (bit-coded); default is 0:
                                 // 1 = This is a closed polyline (or a polygon mesh closed in the M direction).
                                 // 2 = Curve-fit vertices have been added.
                                 // 4 = Spline-fit vertices have been added.
                                 // 8 = This is a 3D polyline.
                                 // 16 = This is a 3D polygon mesh.
                                 // 32 = The polygon mesh is closed in the N direction.
                                 // 64 = The polyline is a polyface mesh.
                                 // 128 = The linetype pattern is generated continuously around the vertices of this polyline.

end;

procedure TDXFExport.AddVertex(const QLayer: string; const X, Y, Z: double);
begin
  wrtLn('  0');
  wrtLn('VERTEX');
  wrtLn('8');
  wrtLn(QLayer);
  wrtLn(' 10');
  wrtLn(QFormatterNombreReel(X));
  wrtLn(' 20');
  wrtLn(QFormatterNombreReel(Y));
  wrtLn(' 30');
  wrtLn(QFormatterNombreReel(Z));
end;

procedure TDXFExport.EndPolyline(const QLayer: string);
begin
  wrtLn('  0');
  wrtLn('SEQEND');
  wrtLn('8');
  wrtLn(QLayer);
  //wrtLn('  0');
end;
end.

