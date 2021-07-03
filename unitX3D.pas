unit unitX3D;

interface
{$INCLUDE CompilationParameters.inc}
uses
  StructuresDonnees,
  Common,
  Graphics,
  Classes, SysUtils;

type

{ TSceneX3D }

 TSceneX3D = class
  strict private
    FOutput: TextFile;
    FFichierX3D: TStringDirectoryFilename;

    procedure BeginScene(const NameScene: string);
    procedure EndScene();
    procedure WriteFooter();
    procedure WriteHeader();
    procedure WriteLine(const S: string);
  public
    function  Initialiser(const FileName: TStringDirectoryFilename; const NameScene: string): boolean;
    procedure Finaliser();

    procedure BeginShape(const NameShape: string);
    procedure DescribeShapeMaterial(const NomShape: string; const Color: TGHTopoColor);
    procedure EndShape(const NameShape: string);

    procedure BeginIndexedFaceSetSection(const QName: string; const DoColorPerVertex, DoSolid: boolean);


    procedure BeginCoordinatesList(const QName: string);
    procedure AddVertexCoordinates(const X, Y, Z: double; const IsLastVertex: boolean = false); overload;
    procedure EndCoordinatesList();

    procedure EndIndexedFaceSetSection();
    procedure BeginIndexFaceSetList();

    // Un faceset est de la forme WriteLine('0 1 2 3 4 5 0 -1');
    // Ne pas refermer (AV ne doit pas répéter le premier index
    procedure AddIndexedFaceSet(const AV: array of integer);
    procedure EndIndexFaceSetList();



    procedure TraceCube(const NomCube: string; const CBG, CHD: TPoint3Df);
    procedure TraceMorceauTube(const E: TBaseStation; const CouleurTube: TGHTopoColor);
end;


implementation
const
  BALISE_SCENE                          = 'Scene';
  BALISE_SHAPE                          = 'Shape';
  BALISE_APPEARANCE                     = 'Appearance';
  BALISE_INDEXED_FACE_SET               = 'IndexedFaceSet';
  SHAPE_NAME_MNT                        = 'MNT';

{ TSceneX3D }

function TSceneX3D.Initialiser(const FileName: TStringDirectoryFilename; const NameScene: string): boolean;
begin
  result := false;
  FFichierX3D := FileName;
  AfficherMessage(format('%s.Initialiser(): %s', [ClassName, FFichierX3D]));
  AssignFile(FOutput, FFichierX3D);
  try
    ReWrite(FOutput);
    WriteHeader();
      BeginScene(NameScene);
    result := true;
  except
  end;
end;

procedure TSceneX3D.Finaliser();
begin
  try
      EndScene();
    WriteFooter();
  finally
    Closefile(FOutput);
  end;
end;
//******************************************************************************
procedure TSceneX3D.WriteLine(const S: string); inline;
begin
  WriteLn(FOutput, S);
end;
procedure TSceneX3D.WriteHeader();
begin
  WriteLine(FormatterVersionEncodageXML('1.0', 'UTF-8'));
  WriteLine('<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.2//EN"');
  WriteLine('"http://www.web3d.org/specifications/x3d-3.2.dtd">');
  WriteLine('<X3D profile="Interchange" version="3.2"');
  WriteLine('   xmlns:xsd="http://www.w3.org/2001/XMLSchema-instance"');
  WriteLine('   xsd:noNamespaceSchemaLocation="http://www.web3d.org/specifications/x3d-3.2.xsd">');
end;
procedure TSceneX3D.WriteFooter();
begin
  WriteLine('</X3D>');
end;
//******************************************************************************
procedure TSceneX3D.BeginScene(const NameScene: string);
const
  NavigationInfo_Type_FMT_ITEM = '    "%s"%s';
  NavigationInfo_Type_EXAMINE = 'EXAMINE';
  NavigationInfo_Type_WALK    = 'WALK';
  NavigationInfo_Type_FLY     = 'FLY';
  NavigationInfo_Type_ANY     = 'ANY';
begin
  WriteLine(Format('  <%s>', [BALISE_SCENE]));
  WriteLine(Format('    <NavigationInfo type=''"%s" "%s" "%s" "%s"''/>',
                   [NavigationInfo_Type_EXAMINE, NavigationInfo_Type_WALK, NavigationInfo_Type_FLY, NavigationInfo_Type_ANY]));
end;

procedure TSceneX3D.EndScene();
begin
  WriteLine(Format('  </%s>', [BALISE_SCENE]))
end;
procedure TSceneX3D.BeginShape(const NameShape: string);
begin
  WriteLine(Format('    <%s>', [BALISE_SHAPE]))
end;
procedure TSceneX3D.EndShape(const NameShape: string);
begin
  WriteLine(Format('    </%s>', [BALISE_SHAPE]))
end;
procedure TSceneX3D.DescribeShapeMaterial(const NomShape: string; const Color: TGHTopoColor);
var
  qr, qg, qb, qt: double;
begin
  qr := Color.Red   / 255.0;
  qg := Color.Green / 255.0;
  qb := Color.Blue  / 255.0;
  qt := 1.0 - (Color.Alpha / 255.0);
  WriteLine(Format('     <%s>', [BALISE_APPEARANCE]));
  WriteLine(Format('        <Material emissiveColor=''%s, %s, %s'' transparency=''%s''/>',
                            [FormatterNombreWithDotDecimal(qr, 3),
                             FormatterNombreWithDotDecimal(qg, 3),
                             FormatterNombreWithDotDecimal(qb, 3),
                             FormatterNombreWithDotDecimal(qt, 2)]));            // emissiveColor='0 0 1'/>
  WriteLine(Format('     </%s>', [BALISE_APPEARANCE]));
end;

//******************************************************************************
// section Facesets
procedure TSceneX3D.BeginIndexedFaceSetSection(const QName: string; const DoColorPerVertex, DoSolid: boolean);
var
  B1, B2: String;
begin
  B1 := BoolToStr(DoColorPerVertex, 'true', 'false');
  B2 := BoolToStr(DoSolid         , 'true', 'false');
  // crease angle = lissage (compris entre 0 et pi)
  Write(FOutput, Format('      <%s DEF=''%s'' creaseAngle=''%s'' colorPerVertex=''%s'' solid=''%s'' ', [BALISE_INDEXED_FACE_SET, QName, FormatterNombreWithDotDecimal(pi, 3), B1, B2]));
end;
procedure TSceneX3D.BeginIndexFaceSetList();
begin
  WriteLine('   coordIndex=''');
end;
procedure TSceneX3D.AddIndexedFaceSet(const AV: array of integer);
var
  EWE: String;
  n, i: Integer;
begin
  EWE := '';
  n := length(AV);
  for i := 0 to n - 1 do EWE += Format('%d', [AV[i]]);
  // Pour refermer le faceset
  EWE += Format('%d %d', [AV[0], -1]);
  WriteLine(EWE);
end;

procedure TSceneX3D.EndIndexFaceSetList();
begin
  WriteLine('    ''>');
end;

procedure TSceneX3D.EndIndexedFaceSetSection();
begin
  WriteLine(Format('      </%s>', [BALISE_INDEXED_FACE_SET]));
end;
procedure TSceneX3D.BeginCoordinatesList(const QName: string);
begin
  WriteLine(Format('    <Coordinate DEF=''%s'' point=''', [QName]));
end;
//******************************************************************************
// section Vertex
procedure TSceneX3D.AddVertexCoordinates(const X, Y, Z: double; const IsLastVertex: boolean = False); overload;
begin
  WriteLine(Format('       %s %s %s%s',
                  [FormatterNombreWithDotDecimal(X, 3),
                   FormatterNombreWithDotDecimal(Y, 3),
                   FormatterNombreWithDotDecimal(Z, 3),
                   IIF(IsLastVertex, ',' , '')
                   ]));
end;
procedure TSceneX3D.EndCoordinatesList();
begin
  WriteLine('    ''/>');
end;



procedure TSceneX3D.TraceCube(const NomCube: string; const CBG, CHD: TPoint3Df);
var ColorCube: TGHTopoColor;
begin
  BeginShape(NomCube);
    ColorCube.setFrom(clBlue, 255);
    DescribeShapeMaterial(NomCube, ColorCube);
    WriteLine(Format('      <IndexedLineSet DEF=''%s'' coordIndex=''%s''>', ['ILS', '0 1 2 3 0 4 5 6 7 4 -1 1 5 -1 2 6 -1 3 7 -1']));
      WriteLine(Format('        <Coordinate DEF=''%s'' point=''', ['CoordsCorners']));
        AddVertexCoordinates(CBG.X, CBG.Y, CBG.Z);
        AddVertexCoordinates(CHD.X, CBG.Y, CBG.Z);
        AddVertexCoordinates(CHD.X, CHD.Y, CBG.Z);
        AddVertexCoordinates(CBG.X, CHD.Y, CBG.Z);
        AddVertexCoordinates(CBG.X, CBG.Y, CHD.Z);
        AddVertexCoordinates(CHD.X, CBG.Y, CHD.Z);
        AddVertexCoordinates(CHD.X, CHD.Y, CHD.Z);
        AddVertexCoordinates(CBG.X, CHD.Y, CHD.Z, True);
      WriteLine('      ''/>');
    WriteLine('      </IndexedLineSet>');
  EndShape(NomCube);
end;


//******************************************************************************
procedure TSceneX3D.TraceMorceauTube(const E: TBaseStation; const CouleurTube: TGHTopoColor);
var
  EWE: String;
  q, r, s: Integer;
begin
  if (E.Enabled AND (E.Type_Entite in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON, tgTUNNEL, tgMINE])) then
  begin
    EWE := Format('Tube%d-%d', [E.Entite_Serie, E.Entite_Station]);
    BeginShape(EWE);
      DescribeShapeMaterial(SHAPE_NAME_MNT, CouleurTube);
      BeginIndexedFaceSetSection('001', false, false);
        BeginIndexFaceSetList();
          AddIndexedFaceSet([0, 1, 2, 3, 4, 5]);
          AddIndexedFaceSet([6, 11, 10, 9, 8, 7]);
          for q := 0 to 5 do
          begin
            r := q mod 6;
            s := (q + 1) mod 6;
            AddIndexedFaceSet([r, r + 6, s + 6, s]);
          end;
        EndIndexFaceSetList();
        BeginCoordinatesList('Coords001');
          AddVertexCoordinates(E.PosExtr0.X, E.PosExtr0.Y, E.PosOPD.Z);
          AddVertexCoordinates(E.PosOPD.X, E.PosOPD.Y, 0.5 * (E.PosOPG.Z + E.PosExtr0.Z));
          AddVertexCoordinates(E.PosOPD.X, E.PosOPD.Y, 0.5 * (E.PosOPD.Z + E.PosExtr0.Z));
          AddVertexCoordinates(E.PosExtr0.X, E.PosExtr0.Y, E.PosOPD.Z);
          AddVertexCoordinates(E.PosOPG.X, E.PosOPG.Y, 0.5 * (E.PosOPD.Z + E.PosExtr0.Z));
          AddVertexCoordinates(E.PosOPG.X, E.PosOPG.Y, 0.5 * (E.PosOPG.Z + E.PosExtr0.Z));

          AddVertexCoordinates(E.PosStation.X, E.PosStation.Y, E.PosPG.Z);
          AddVertexCoordinates(E.PosPD.X, E.PosPD.Y, 0.5 * (E.PosPG.Z + E.PosStation.Z));
          AddVertexCoordinates(E.PosPD.X, E.PosPD.Y, 0.5 * (E.PosPD.Z + E.PosStation.Z));
          AddVertexCoordinates(E.PosStation.X, E.PosStation.Y, E.PosPD.Z);
          AddVertexCoordinates(E.PosPG.X, E.PosPG.Y, 0.5 * (E.PosPD.Z + E.PosStation.Z));
          AddVertexCoordinates(E.PosPG.X, E.PosPG.Y, 0.5 * (E.PosPG.Z + E.PosStation.Z), True);
        EndCoordinatesList();
      EndIndexedFaceSetSection();
    EndShape(EWE);
  end;
end;

end.

