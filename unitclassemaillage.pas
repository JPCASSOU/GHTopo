// Unité en vigueur 21/01/2014
// 23/01/2014 : Calculs d'altitudes Z = f(x, y)
// 01/06/2015 : Intégration dans GHTopoFPC OK
// 01/06/2015 : Courbes de niveau avec équidistance
// 05/06/2015 : Les maillages 'grille' sont systématiquement convertis en TIN au chargement
// 05/06/2015 : Remplacement des tableaux dynamiques Vertexes et Triangles par des listes génériques
// 05/06/2015 : Transformation d'un maillage 'grille' en maillage 'TIN' OK
// 09/06/2015 : Import des maillages VisualTopo (SUR)
// 25/04/2016 : Révision de cette unité: ajout des () aux fonctions sans paramètres
// 03/07/2018 : Export X3D  (ébauche)
// 07/09/2018 : Profil topographique (classe TProfilTopo): OK pour extraction
// 17/09/2018 : Accélération du tracé des maillages: élimination des éléments hors fenêtre de vue

unit UnitClasseMaillage;
interface
{$INCLUDE CompilationParameters.inc}
uses
  StructuresDonnees,
  UnitListesSimplesWithGeneriques,
  UnitPolyPolyLine,
  UnitEntitesExtended,
  unitProfilTopo,
  UnitTriangulationDelaunay,
  Common,
  math,
  unitX3D,
  BGRABitmap,
  GL, GLU, //OpenGLContext,
  FastGEO,
  LazFileUtils,
  SysUtils,Classes, Graphics, Dialogs;
// intersection droite/triangle
type TIntersectPlanTriangle = record
  X1, Y1, Z1: double;
  X2, Y2, Z2: double;
end;

type TOpenGLColor = array[0..3] of GLFloat;
 // couleurs OpenGL
type TGLColor = record
  R: GLFloat;
  G: GLFloat;
  B: GLFloat;
  A: GLFloat;
end;

type

{ TMaillage }

 TMaillage = class
   private
     FNomDuMaillage : string;
     FBDDEntites    : TBDDEntites; // // pour construction des transects
     ZMatrix        : TMatrix;
     FTableVertex   : TMNTTableVertex;
     FTableTriangles: TMNTTableTriangles;
     FTypeMaillage  : TMNTTypeMaillage;
     FMaillageValide        : boolean;
     FModeDessinMaillage    : TMNTModeDessinMaillage;

     FCoordsMini: TPoint3Df;
     FCoordsMaxi: TPoint3Df;
     FNbVertexX : integer;
     FNbVertexY : integer;
     FNomTypeMaillage: string;
     FPasX      : double;
     FPasY      : double;

     // couleurs et transparence
     FDoUseDegrades        : boolean;
     FColorMin             : TColor;
     FColorMax             : TColor;
     FOpacity              : byte;
     // pointeur sur fonctions de conversion pour le canevas
     FProcGCSToSRC : TProcGCSToSRC;
     // liste des extractions de profils de terrain
     FListeProfilsTopo: TListeProfilsTopo;

     procedure RecenserTrianglesVoisins();
     procedure ReinitBoundinxBoxWithoutData();
     // les deux types de maillages sont convertis 'in fine' en maillages 'Triangles'
     function ChargerResultatsDuMailleur(const BaseFileName: TStringDirectoryFilename): boolean;
     function ConvertitMaillageGridEnTIN(): boolean;

     function GetCoordosPlan(const PM: TPoint3Df): TPoint;
     function GetCoordosVertexToPlan(const PM: TMNTVertex): TPoint;


     function GetTriangleIdxContenantPoint(const QX, QY: double): integer;
     function IntersectPlanVerticalAndTriangle(const QT: TMNTTriangleABC; const ExtrProfil1, ExtrProfil2: TPoint3Df; out QIntersect: TIntersectPlanTriangle): boolean;
     function IsValidTriangle(const TR: TMNTTriangleABC): boolean;
     function  TriangleContainsPoint(const T: TMNTTriangleABC; const QX, QY: double): boolean;
     procedure SetTypeMaillage(const B: TMNTTypeMaillage; const PrgmLineTag: integer);

     procedure UpdateMiniMaxi(const V: TMNTVertex);
   public
     function  Initialiser(const BE: TBDDEntites): boolean;
     procedure Finaliser();

     procedure ResetMaillageTIN();

     function  GetNomMaillage(): string;
     function  ReDimMaillageGRD(): int64; // redimensionnement d'un MNT à pas régulier
     function  IsValidMaillage(): boolean;
     procedure ConstruireMaillageTriangleWire(const QScale, QMagn: double);
     procedure ConstruireMaillageTriangleSolide(const QScale, QMagn: double);

     procedure ConstruireMaillages(const QScale, QMagn: double);
     procedure CalculerTINNormales(const QScale, QMagn: double);
     function  LoadMaillage(const FichierMaillage: string): boolean;
     function  SaveMaillageTIN(const FichierMaillage: string): boolean;

     function ImporterXYZ(const FichierXYZ: string; const DoPreserve: boolean = false): boolean;

     function  ChargerMaillageDepuisHGT(const FichierHGT: string): boolean;
     function  ChargerMaillageDepuisSUR(const FichierSUR: string): boolean; // maillage Visual Topo
     // MNT grille: Transformation en image
     function  MakeImageFromGRD(const DoYReverse: boolean): TBitmap;
     procedure ExportToSUR(const FichierSUR: string);

     // getters et setters vertex
     procedure AddVertex(const V : TMNTVertex); overload; inline;
     procedure AddVertex(const QX, QY, QZ: double); overload;
     function  GetVertex(const Idx: integer): TMNTVertex; inline;
     procedure PutVertex(const Idx: integer; const V: TMNTVertex); inline;
     function  GetNbVertex(): integer; inline;
     //procedure SetMiniMaxi();
     // getters et setters triangles
     procedure AddTriangle(const V : TMNTTriangleABC); inline;
     function  GetTriangle(const Idx: integer): TMNTTriangleABC; inline;
     procedure PutTriangle(const Idx: integer; const V: TMNTTriangleABC); inline;
     function  GetNbTriangles(): integer; inline;

     // pour les MNT grilles
     // n'a de sens que si FTypeMaillage = tmREGULAR_GRID
     function  GetNbCellsX(): integer;
     function  GetNbCellsY(): integer;

     // setters et getters
     // mode de dessin
     function  GetModeDessinMaillage(): TMNTModeDessinMaillage; inline;
     procedure SetModeDessinMaillage(const M: TMNTModeDessinMaillage); inline;

     // couleurs
     procedure SetUsingDegrade(const B: boolean);
     function  GetUsingDegrade(): boolean;
     function  GetMinCouleur(): TColor; inline;
     procedure SetMinCouleur(const C: TColor); inline;
     function  GetMaxCouleur(): TColor; inline;
     procedure SetMaxCouleur(const C: TColor); inline;
     // alpha
     procedure SetAlphaBlending(const A: byte); inline;
     function  GetAlphaBlending(): byte; inline;
     // membres à lecture seule
     // vertex et triangles

     // transformation d'un triangle TTriangulation en TGeoTriangle;
     function GetGeoTriangle2D(const QT: TMNTTriangleABC): TGeoTriangle2D;
     function GetGeoTriangle3D(const QT: TMNTTriangleABC): TGeoTriangle3D;
     // calcul du BB d'un triangle
     // -----------------------------------------------------------------------------
     function CalcBoundingBoxTriangle(const T: TMNTTriangleABC): TMNTTriangleABC;
     // -- type de maillage
     function  GetTypeMaillage(): TMNTTypeMaillage; inline;

     // -- points extrêmes du maillage
     function  GetCoordsMini(): TPoint3Df; inline;
     function  GetCoordsMaxi(): TPoint3Df; inline;
     // -- Maillages à pas régulier


     // calcul de la distance au versant
     function CalcDistAzPAuVersant(const QX, QY, QZ: double; out QL, QAz, QP: double): boolean;
     function CalcDistanceAuVersant(const QX, QY, QZ: double): double; overload;
     function CalcDistanceAuVersant(const PT: TPoint3Df): double; overload;
     // tracé du maillage sur un canevas.
     // Paramètres:
     // Cnv: Un TCanvas
     // QP: Une fonction de conversion TPoint2Df vers TPoint
     // Isovaleur: Une altitude; -1 = ne pas calculer
     procedure TracerMaillage(const Bmp                : TBGRABitmap;
                              const QP                 : TProcGCSToSRC;
                              const Equidistance       : double;
                              const Isovaleurs         : TArrayOfFloats;
                              const LineContourColor   : TGHTopoColor;
                              const IsovaleurColor     : TGHTopoColor;
                              const DoDrawTriangles    : boolean;
                              const DoDrawVertex       : boolean;
                              const viewBox            : TRect2Df);
     // altitude d'un point du maillage
     function CalcAltitudeMaillageAtPoint(const QX, QY: double; out QZ: double): boolean;
     procedure ExportX3D(const QFichierX3D: TStringDirectoryFilename);
     // extraction d'un profil topographique entre deux points
     function  ExtractAndAddProfilTopo(const Pt1, Pt2: TPoint2Df; const QProfilLineAttr: TLineAttributes; const QProfilName: string = ''): boolean;
     procedure AddProfilTopo(const P: TProfilTopo); inline;
     function  GetProfilTopo(const Idx: integer): TProfilTopo; inline;
     procedure PutProfilTopo(const Idx: integer; const P: TProfilTopo); inline;
     procedure RemoveProfilTopo(const Idx: integer);
     procedure ClearProfilsTopo();

     function  GetNbProfilsTopo(): integer; inline;
     procedure ListerProfilsTopo();
     // générer un maillage à pas régulier
     // la méthode utilisée est abrutie, conne, bête et méchante:
     // a) Extraction de la Bounding-Box du maillage
     // b) Calcul de l'interdistance en X et Y
     // c) Deux boucles imbriquées, dans lesquelles on calcule l'altitude du point

     // A voir: possibilité d'extraire un 'mouchoir à carreaux' d'orientation quelconque
     // Ici, on extrait des profils équidistants
     function  ConstruireMaillageAPasRegulier(const NbMaillesX, NbMaillesY: integer): boolean;

     procedure SaveProfilsToFile(const QFilename: string);
     function  LoadProfilsFromFile(const QFilename: string): boolean;

     function Trianguler(const NbPointsATrianguler: integer = -1): boolean;

     function QualifierTriangle(var ATriangle: TMNTTriangleABC): boolean;
     // travail avec un mailleur externe
     function TriangulerAvecUnOutilExterne(const ProgramMailleurExterne: TStringDirectoryFilename): boolean;
     function GenererFichierPourMailleurExterne(const QFilename: TStringDirectoryFilename): boolean;
     // supprimer les sommets extérieurs à un polygone
     function ElimineVertexOutOfPolygone(const P: TGeoPolygon2D): boolean;
     // désactiver l'affichage en dehors d'une bounding-box
     procedure DisableDisplayOutOfBoundingBox(const BB: TMNTBoundingBox);
     // reset de la visibilité des objets
     procedure ResetVisibilityOfObjects(const B: boolean);
     // affecter les couleurs aux objets
     procedure AffecterCouleursAuxObjets(const QDoDegrade: boolean);

     // extraire une isovaleur sous forme d'une polypolyligne
     function ExtraireIsovaleurAsPolyPolyline(const Altitude: double; out PP: TPolyPolyLigne): boolean;

end;


implementation
uses
  DGCDummyUnit;




// couleur OpenGL
// Retourne sous forme de couleur OpenGL la couleur passée en argument
function MakeTGLColorBlending(const Coul: TColor; const Alpha: Double): TGLColor;
const
  m = 1/256;
begin
  Result.R := Red(Coul)* m;
  Result.G := Green(Coul)* m;
  Result.B := Blue(Coul)* m;
  Result.A := Alpha * m;
end;





// TMaillage
function TMaillage.IsValidTriangle(const TR: TMNTTriangleABC): boolean;
const
  MAX_LONGUEUR_COTE = 250.00;
var
  AB, BC, CA: Boolean;
  VA, VB, VC: TMNTVertex;
  a, b, c: double;
begin
  // sommets invalides ?
  if ((TR.PointA < 0) or (TR.PointB < 0) or (TR.PointC < 0)) then Exit(false);
  // sommets confondus ?
  AB := 0 = (TR.PointA - TR.PointB);
  BC := 0 = (TR.PointB - TR.PointC);
  CA := 0 = (TR.PointC - TR.PointA);
  if (AB OR BC OR CA) then Exit(false);
  // triangle trop grand
  VA := GetVertex(TR.PointA);
  VB := GetVertex(TR.PointB);
  VC := GetVertex(TR.PointC);

  a := Hypot(VB.Position.X - VC.Position.X, VB.Position.Y - VC.Position.Y);
  b := Hypot(VA.Position.X - VC.Position.X, VA.Position.Y - VC.Position.Y);
  c := Hypot(VA.Position.X - VB.Position.X, VA.Position.Y - VB.Position.Y);

  if ((a > MAX_LONGUEUR_COTE) or (b > MAX_LONGUEUR_COTE) or (c > MAX_LONGUEUR_COTE)) then exit(false);

  Result := True;
end;

// intersection d'un triangle
procedure TMaillage.CalculerTINNormales(const QScale, QMagn: double);
var
  i, Nb: integer;
  V1, V2, W: TPoint3Df;
  MM: double;
  VA, VB, VC, VV: TMNTVertex;
  TR: TMNTTriangleABC;
begin
  MM := QScale * QMagn;
  AfficherMessage(Format('%s.CalculerTINNormales: %d vertex, %d triangles',[ClassName, GetNbVertex, GetNbTriangles]));
  //FTableVertex.ClearListe;
  Nb := GetNbVertex;
  for i:=0 to Nb -1 do
  begin
    VV := GetVertex(i);
    VV.Normale.Empty();
    PutVertex(i, VV);
  end;
  Nb := GetNbTriangles;
  for i:=0 to Nb - 1 do
  begin
    TR := GetTriangle(i);
    with TR do
      begin
        // vecteurs v1 et v2
        VA := GetVertex(PointA);
        VB := GetVertex(PointB);
        VC := GetVertex(PointC);
        V1.setFrom(MM * (VB.Position.X - VA.Position.X),
                   MM * (VB.Position.Y - VA.Position.Y),
                   MM * (VB.Position.Z - VA.Position.Z));

        V2.setFrom(MM * (VC.Position.X - VA.Position.X),
                   MM * (VC.Position.Y - VA.Position.Y),
                   MM * (VC.Position.Z - VA.Position.Z));

        // produit vectoriel: normale du triangle
        W.setFrom(V1.Y * V2.Z - V1.Z * V2.Y,
                  V1.Z * V2.X - V1.X * V2.Z,
                  V1.X * V2.Y - V1.Y * V2.X);

        VA.AddAtNormale(W.X, W.Y, W.Z);
        VB.AddAtNormale(W.X, W.Y, W.Z);
        VC.AddAtNormale(W.X, W.Y, W.Z);
        PutVertex(PointA, VA);
        PutVertex(PointB, VB);
        PutVertex(PointC, VC);
      end;
  end;
  // normalisation
  Nb := GetNbVertex();
  for i:= 0 to Nb - 1 do
  begin
    VV := GetVertex(i);
    VV.Normalize();
    PutVertex(i, VV);
  end;
end;
procedure TMaillage.ConstruireMaillages(const QScale, QMagn: double);
begin
 AfficherMessage(Format('%s.ConstruireMaillages: %d',[ClassName, Ord(FTypeMaillage)]));
 AfficherMessage(Format('Bounds: (%.2f, %.2f, %.2f) -> (%.2f, %.2f, %.2f)',
                        [FCoordsMini.X, FCoordsMini.Y, FCoordsMini.Z,
                         FCoordsMaxi.X, FCoordsMaxi.Y, FCoordsMaxi.Z]));
  //CalculerTINNormales(QScale, QMagn);
  AffecterCouleursAuxObjets(FDoUseDegrades);
  case FModeDessinMaillage of
    M3D_NONE          : pass;
    M3D_WIRE_FRAME    : ConstruireMaillageTriangleWire(QScale, QMagn);
    M3D_MESH          : ConstruireMaillageTriangleSolide(QScale, QMagn);
  end;
end;

function TMaillage.Initialiser(const BE: TBDDEntites): boolean;
begin
  Result := false;
  FMaillageValide   := false;
  FTableVertex      := TMNTTableVertex.Create;
  FTableTriangles   := TMNTTableTriangles.Create;
  FListeProfilsTopo := TListeProfilsTopo.Create;
  try
    ResetMaillageTIN();
    FNomDuMaillage := 'Maillage01';
    AfficherMessage(Format('%s.Initialiser: %s',[ClassName, FNomDuMaillage]));
    FBDDEntites := BE;
    AfficherMessage(IIF(Assigned(FBDDEntites),
                        'Maillage lié à un TBDDEntites - Extractions de profils possibles',
                        'Maillage indépendant'));

    SetLength(ZMatrix,1, 1);
    FDoUseDegrades    := true;
    FColorMin         := clGreen;
    FColorMax         := clMaroon;
    FOpacity          := 128;
    Result := true;

  except
  end;
end;

procedure TMaillage.ResetMaillageTIN();
begin
  FMaillageValide := false;
  ReinitBoundinxBoxWithoutData();
  FTableVertex.ClearListe;
  FTableTriangles.ClearListe;
  FListeProfilsTopo.ClearListeProfils();
end;

procedure TMaillage.Finaliser();
begin
  AfficherMessage(Format('%s.Finaliser',[ClassName]));
  try
    ResetMaillageTIN();
  finally
    FreeAndNil(FTableVertex);//FTableVertex.Free;
    FreeAndNil(FTableTriangles);//FTableTriangles.Free;
    FreeAndNil(FListeProfilsTopo);//FListeProfilsTopo.Free;
  end;
end;



function TMaillage.GetNomMaillage(): string;
begin
  Result := FNomDuMaillage;
end;

function TMaillage.ReDimMaillageGRD(): int64;
var
  cx, cy: integer;
begin
  cx := 1 + FNbVertexX ;
  cy := 1 + FNbVertexY;
  AfficherMessage(Format('%s.ReDimMaillage: %d, %d',[ClassName, cx, cy]));
  try
    Result:=-2;
    SetLength(ZMatrix,1, 1);
    SetLength(ZMatrix,   cy, cx);
    //ShowMessageFmt('%d %d', [FNbLignesX,FNbLignesY]);
    Result:= FNbVertexX * FNbVertexY;
  finally
  end;
end;


procedure TMaillage.AffecterCouleursAuxObjets(const QDoDegrade: boolean);
var
  NbVertex, i: Integer;
  MyVertex: TMNTVertex;
begin
  //AfficherMessageErreur(Format('AffecterCouleursAuxObjets: Coul: %X, %X, Z: %f, %f - %s', [FColorMin, FColorMax, FCoordsMini.Z, FCoordsMaxi.Z, Booltostr(QDoDegrade, 'Dégradé', 'Uni')]));
  NbVertex := GetNbVertex();
  if (NbVertex = 0) then exit;
  for i := 0 to NbVertex - 1 do
  begin
    MyVertex := GetVertex(i);
    MyVertex.Couleur := IIF (QDoDegrade,
                             GetColorDegrade(MyVertex.Position.Z, FCoordsMini.Z, FCoordsMaxi.Z, FColorMin, FColorMax),
                             FColorMin);
    PutVertex(i, MyVertex);

  end;
end;
// Fonctionne mais lent ++
procedure TMaillage.RecenserTrianglesVoisins();
var
  i, j, n: Integer;
  MyTriangle, QTriVoisin: TMNTTriangleABC;
  Q1, Q2, Q3: Boolean;
begin
  n := GetNbTriangles();
  AfficherMessageErreur(format('%s.RecenserTrianglesVoisins(%d)', [ClassName, n]));
  if (0 = n) then exit;
  for i := 0 to n - 1 do
  begin
    MyTriangle := GetTriangle(i);
    MyTriangle.VoisinAB := -1;
    MyTriangle.VoisinBC := -1;
    MyTriangle.VoisinCA := -1;
    if (i MOD 10000 = 0) then AfficherMessageErreur(Format('Triangle %d', [i]));
    // recherche des voisins (tous les triangles sont orientés CCW, normale vers le haut)
    for j := 0 to n - 1 do
    begin
      QTriVoisin := GetTriangle(j);

      Q1 := (MyTriangle.PointA = QTriVoisin.PointA) AND (MyTriangle.PointB = QTriVoisin.PointC);
      Q2 := (MyTriangle.PointA = QTriVoisin.PointB) AND (MyTriangle.PointB = QTriVoisin.PointA);
      Q3 := (MyTriangle.PointA = QTriVoisin.PointC) AND (MyTriangle.PointB = QTriVoisin.PointB);
      if (Q1 OR Q2 OR Q3) then MyTriangle.VoisinAB := j;

      Q1 := (MyTriangle.PointB = QTriVoisin.PointA) AND (MyTriangle.PointC = QTriVoisin.PointC);
      Q2 := (MyTriangle.PointB = QTriVoisin.PointB) AND (MyTriangle.PointC = QTriVoisin.PointA);
      Q3 := (MyTriangle.PointB = QTriVoisin.PointC) AND (MyTriangle.PointC = QTriVoisin.PointB);
      if (Q1 OR Q2 OR Q3) then MyTriangle.VoisinBC := j;

      Q1 := (MyTriangle.PointC = QTriVoisin.PointA) AND (MyTriangle.PointA = QTriVoisin.PointC);
      Q2 := (MyTriangle.PointC = QTriVoisin.PointB) AND (MyTriangle.PointA = QTriVoisin.PointA);
      Q3 := (MyTriangle.PointC = QTriVoisin.PointC) AND (MyTriangle.PointA = QTriVoisin.PointB);

      if (Q1 OR Q2 OR Q3) then MyTriangle.VoisinCA := j;
    end;
    PutTriangle(i, MyTriangle);
  end;
  // contrôle
  AfficherMessageErreur(Format('Liste des %d triangles et leurs voisins', [n]));
  exit;
  for i := 0 to 666 - 1 do
  begin
    MyTriangle := GetTriangle(i);
    AfficherMessageErreur(Format('%d, %d, %d, %d', [i, MyTriangle.VoisinAB, MyTriangle.VoisinBC, MyTriangle.VoisinCA]));

  end;
end;

function TMaillage.ExtraireIsovaleurAsPolyPolyline(const Altitude: double; out PP: TPolyPolyLigne): boolean;
var
  n, i, nv, v: Integer;
  QFirstIdxTriangle: TMNTNumeroTriangle;
  PL: TListePoints3Df;
  VX: TPoint3Df;              //   function GetTriangle(const Idx: integer): TMNTTriangleABC; inline;
  QListeTrianglesVisites: array of boolean;
  QTriangle: TMNTTriangleABC;
  AreteAB, AreteBC, AreteCA: TGeoSegment3D;
  function FindFirstIdxTriangleContainsAltitude(): TMNTNumeroTriangle;
  var
    QTri: TMNTTriangleABC;
    t: Integer;
  begin
    result := -1;
    for t := 0 to n - 1 do
    begin
      QTri := GetTriangle(t);
      if (IsInRange(Altitude, QTri.BoundingBox.C1.Z, QTri.BoundingBox.C2.Z)) then Exit(t);
    end;
  end;
begin
  result := false;
  Exit;

  (*

  ClearConsoleErreur();
  n := self.GetNbTriangles();
  AfficherMessageErreur(Format('%s.ExtraireIsovaleurAsPolyPolyline: Alt: %.0f - %d triangles', [classname, Altitude, n]));
  // liste des triangles visités
  if (0 = n) then exit;
  // On recense les triangles voisins
  self.RecenserTrianglesVoisins();
  // Le tableau des triangles visités
  SetLength(QListeTrianglesVisites, n);
  for i := 0 to n-1 do QListeTrianglesVisites[i] := false;
  // Recherche du premier triangle contenant cette altitude
  QFirstIdxTriangle := FindFirstIdxTriangleContainsAltitude();
  if (QFirstIdxTriangle > 0) then
  begin
    QTriangle := GetTriangle(QFirstIdxTriangle);
    AfficherMessageErreur(Format('Triangle %d : %.2f, %.2f', [QFirstIdxTriangle, QTriangle.BoundingBox.C1.Z, QTriangle.BoundingBox.C2.Z]));

    // recherche des intersections entre le plan de cote Z et les côtés du triangle
    AreteAB := QMakeGeoSegment(QTriangle.PointA, QTriangle.PointB);
    AreteBC := QMakeGeoSegment(QTriangle.PointB, QTriangle.PointC);
    AreteCA := QMakeGeoSegment(QTriangle.PointC, QTriangle.PointA);
    if (IntersectPlanHorizontalSegment(Altitude, AreteAB, QX, QY)) then
    begin
      PP := TPolyPolyLigne.Create;
      PP.Initialiser(Format('Isovaleur %f', [Altitude]));
    end;
  try

    n := PP.GetNbPolylines();
    AfficherMessageErreur(format('Polypolyligne "%s": %d polylignes', [PP.GetName(), n]));
    PP.BeginPolyline();
    end;


  end;

   //*)


   (*





  PP := TPolyPolyLigne.Create;
  try
    PP.Initialiser(Format('Isovaleur %f', [Altitude]));
    n := PP.GetNbPolylines();
    AfficherMessageErreur(format('Polypolyligne "%s": %d polylignes', [PP.GetName(), n]));
    PP.BeginPolyline();
      PP.AddVertex(402938.63, 3090857.00, 833.00);
      PP.AddVertex(402859.22, 3090864.00, 800.00);
      PP.AddVertex(402848.38, 3090835.50, 800.00);
      PP.AddVertex(402839.34, 3090801.50, 800.00);
      PP.AddVertex(402833.94, 3090778.25, 800.00);
      PP.AddVertex(402841.16, 3090756.75, 800.00);
      PP.AddVertex(402857.41, 3090738.75, 800.00);
      PP.AddVertex(402884.47, 3090729.75, 800.00);
      PP.AddVertex(402927.81, 3090721.00, 800.00);
      PP.AddVertex(402949.47, 3090710.25, 800.00);
      PP.AddVertex(402980.13, 3090712.00, 800.00);
      PP.AddVertex(402994.59, 3090737.00, 800.00);
      PP.AddVertex(402987.38, 3090771.00, 800.00);
      PP.AddVertex(402978.34, 3090806.75, 800.00);
      PP.AddVertex(402992.78, 3090835.50, 800.00);
    PP.EndPolyline();
    n := PP.GetNbPolylines();
    AfficherMessageErreur(format('Polypolyligne "%s": %d polylignes', [PP.GetName(), n]));
    PP.BeginPolyline();
      PP.AddVertex(403003.63, 3090862.25, 800.00);
      PP.AddVertex(403016.25, 3090885.50, 800.00);
      PP.AddVertex(403023.47, 3090908.75, 800.00);
      PP.AddVertex(403027.09, 3090941.00, 800.00);
      PP.AddVertex(403012.63, 3090946.25, 800.00);
    PP.EndPolyline();
    PP.BeginPolyline();
      PP.AddVertex(402983.75, 3090937.50, 800.00);
      PP.AddVertex(402958.47, 3090933.75, 800.00);
      PP.AddVertex(402935.03, 3090924.75, 800.00);
      PP.AddVertex(402920.59, 3090916.00, 800.00);
      PP.AddVertex(402902.53, 3090905.25, 800.00);
      PP.AddVertex(402886.28, 3090891.00, 800.00);
      PP.AddVertex(402870.03, 3090874.75, 800.00);
      PP.AddVertex(402879.06, 3090815.75, 810.00);
      PP.AddVertex(402886.28, 3090794.25, 810.00);
      PP.AddVertex(402900.72, 3090772.75, 810.00);
      PP.AddVertex(402920.59, 3090756.75, 810.00);
      PP.AddVertex(402944.03, 3090744.25, 810.00);
    PP.EndPolyline();
    //*)
    (*
    // contrôle
    n := PP.GetNbPolylines();
    AfficherMessageErreur(format('Polypolyligne "%s": %d polylignes', [PP.GetName(), n]));
    //PP.RemovePolyline(1);
    n := PP.GetNbPolylines();
    AfficherMessageErreur(format('Polypolyligne "%s": %d polylignes', [PP.GetName(), n]));

    for i := 0 to n - 1 do
    begin
      PL := PP.GetPolyline(i);
      nv := PL.GetNbElements();
      AfficherMessageErreur(Format('Polyligne #%d: %d vertex', [i, nv]));
      for v := 0 to nv - 1 do
      begin
        VX := PL.GetElement(v);
        AfficherMessageErreur(format('Vertex: %d@%d - %.3f, %.3f, %.3f', [v, i, VX.X, VX.Y, VX.Z]));
      end;

    end;
    //AfficherMessageErreur('001');
    PP.Finaliser();
    n := PP.GetNbPolylines();
    AfficherMessageErreur(format('Polypolyligne "%s": %d polylignes', [PP.GetName(), n]));

    //****************
    SetLength(QListeTrianglesVisites, 0);
  finally
    //FreeAndNil(PP);
  end;
  //*)
end;









procedure TMaillage.SetModeDessinMaillage(const M: TMNTModeDessinMaillage);
begin
  FModeDessinMaillage := M;
end;

procedure TMaillage.SetUsingDegrade(const B: boolean);
begin
  FDoUseDegrades := B;
end;

function TMaillage.GetUsingDegrade(): boolean;
begin
  result := FDoUseDegrades;
end;

procedure TMaillage.SetTypeMaillage(const B: TMNTTypeMaillage; const PrgmLineTag: integer);
var
  EWE: String;
begin
  EWE := ChooseString(Ord(B), ['tmUNKNOWN', 'tmREGULAR_GRID', 'tmTRIANGLES']);
  AfficherMessageErreur(Format('--> SetMaillageValide: %s - Ligne: %d', [EWE, PrgmLineTag]));

  FTypeMaillage := B;
end;

procedure TMaillage.UpdateMiniMaxi(const V: TMNTVertex);
begin
  if (V.Position.X < FCoordsMini.X) then FCoordsMini.X := V.Position.X;
  if (V.Position.Y < FCoordsMini.Y) then FCoordsMini.Y := V.Position.Y;
  if (V.Position.Z < FCoordsMini.Z) then FCoordsMini.Z := V.Position.Z;

  if (V.Position.X > FCoordsMaxi.X) then FCoordsMaxi.X := V.Position.X;
  if (V.Position.Y > FCoordsMaxi.Y) then FCoordsMaxi.Y := V.Position.Y;
  if (V.Position.Z > FCoordsMaxi.Z) then FCoordsMaxi.Z := V.Position.Z;
end;



function TMaillage.GetModeDessinMaillage(): TMNTModeDessinMaillage;
begin
  Result := FModeDessinMaillage;
end;

function TMaillage.GetNbCellsX(): integer;
begin
  Result := -1;
  if (FTypeMaillage = tmREGULAR_GRID) then Result := FNbVertexX - 1;
end;

function TMaillage.GetNbCellsY(): integer;
begin
  Result := -1;
  if (FTypeMaillage = tmREGULAR_GRID) then Result := FNbVertexY - 1;
end;




function TMaillage.IsValidMaillage(): boolean;
begin
  try
    Result := FMaillageValide;
  except
    Result := false;
  end;
end;





function TMaillage.GetTypeMaillage(): TMNTTypeMaillage;
begin
  Result := FTypeMaillage;
end;



//*)
// distance au versant; retourne long, azimut et pente
function TMaillage.CalcDistAzPAuVersant(const QX, QY, QZ: double; out QL, QAz, QP: double): boolean;
var
  WU: TGeoFloat;
  i, j: Integer;
  S: TPoint3Df;
  _dx, _dy, _dz: double;
  Qdx, Qdy, Qdz: Double;
  EWE: TMNTTriangleABC;
begin
  WU := 1e18;
  Result := False;
  if (not FMaillageValide) then Exit;
  case FTypeMaillage of
    tmUNKNOWN: exit;
    tmTRIANGLES:
      begin
      end;
  otherwise
    pass;
  end;
end;

function TMaillage.CalcDistanceAuVersant(const QX, QY, QZ: double): double;
begin
  result := -1.00;
end;
// teste si le triangle T contient le point
function TMaillage.TriangleContainsPoint(const T: TMNTTriangleABC; const QX, QY: double): boolean;
var
  S1, S2, S3: TMNTVertex;
begin
  Result := False;
  S1 := GetVertex(T.PointA);
  S2 := GetVertex(T.PointB);
  S3 := GetVertex(T.PointC);
  Result := PointInTriangle(QX, QY, S1.Position.X, S1.Position.Y, S2.Position.X, S2.Position.Y, S3.Position.X, S3.Position.Y);
end;


// retourne le index du triangle contenant le point (X, Y) ou -1 si échec
function TMaillage.GetTriangleIdxContenantPoint(const QX, QY: double): integer;
var
  i, n: Integer;
  EWE : TMNTTriangleABC;
begin
  Result := -1;
  n := self.GetNbTriangles();
  if (n = 0) then exit(-1);
  // Faire toujours partir les index de triangles à partir de 1 et non de zéro
  for i := 1 to n - 1 do
  begin
    EWE := GetTriangle(i);
    if (TriangleContainsPoint(EWE, QX, QY)) then Exit(i);
  end;
end;
// calcul de l'altitude du point (x, y)
function TMaillage.CalcAltitudeMaillageAtPoint(const QX, QY: double; out QZ: double): boolean;
var
  n : integer;
  S1, S2, S3: TMNTVertex;
  EWE: TMNTTriangleABC;
begin
  //AfficherMessage(Format('%s.CalcAltitudeMaillageAtPoint(%.2f, %.2f)', [ClassName, QX, QY]));
  Result := False;
  n := GetTriangleIdxContenantPoint(QX, QY);
  //AfficherMessage(Format('Triangle %d trouve', [n]));
  if (n >= 0) then
  begin
    EWE := GetTriangle(n);
    S1 := GetVertex(EWE.PointA);
    S2 := GetVertex(EWE.PointB);
    S3 := GetVertex(EWE.PointC);
    QZ := GeoCalcAltitudeXYInTriangle(S1.Position.X, S1.Position.Y, S1.Position.Z,
                                      S2.Position.X, S2.Position.Y, S2.Position.Z,
                                      S3.Position.X, S3.Position.Y, S3.Position.Z,
                                      QX, QY);
    Result := True;
  end;
end;


procedure TMaillage.ExportX3D(const QFichierX3D: TStringDirectoryFilename);
const
  SHAPE_NAME_MNT  = 'MNT';
var
  QStdColorMNT: TGHTopoColor;
  NbVertex, NbTriangles, i: Integer;
  MyX3D: TSceneX3D;
  NameScene: String;
  procedure QWriteVertex(const V: TMNTVertex; const IsLast: boolean);
  begin
    MyX3D.AddVertexCoordinates(V.Position.X, V.Position.Y, V.Position.Z, IsLast);
  end;

  procedure QWrite3DFace(const T: TMNTTriangleABC);
  begin
    MyX3D.AddIndexedFaceSet([T.PointA, T.PointB, T.PointC]);
  end;
begin
  QStdColorMNT.setFrom(clLime, 128);
  NameScene   := 'MNT';
  NbVertex    := GetNbVertex();
  NbTriangles := GetNbTriangles();
  AfficherMessage(format('%s.ExportX3D(): %d vertex, %d triangles, Output file: %s', [ClassName, NbVertex, NbTriangles, QFichierX3D]));
  MyX3D := TSceneX3D.Create;
  try
    MyX3D.Initialiser(QFichierX3D, NameScene);
      MyX3D.DescribeShapeMaterial(SHAPE_NAME_MNT, QStdColorMNT);
      MyX3D.BeginIndexedFaceSetSection('MNTTriangles', false, false);
         MyX3D.BeginIndexFaceSetList();
           for i := 0 to NbTriangles-1 do QWrite3DFace(GetTriangle(i));
         MyX3D.EndIndexFaceSetList();
         MyX3D.BeginCoordinatesList('MNTVertex');
           for i := 0 to NbVertex - 1 do QWriteVertex(GetVertex(i), i = (NbVertex - 1));
         MyX3D.EndCoordinatesList();
      MyX3D.EndIndexedFaceSetSection();
    MyX3D.Finaliser();
  finally
    FreeAndNil(MyX3D);
  end;

end;



function TMaillage.CalcDistanceAuVersant(const PT: TPoint3Df): double;
begin
  Result := CalcDistanceAuVersant(PT.X, PT.Y, PT.Z);
end;
//******************************************************************************
function TMaillage.GetMinCouleur(): TColor;
begin
  Result := FColorMin;
end;

procedure TMaillage.SetMinCouleur(const C: TColor);
begin
  FColorMin := C;
end;

function TMaillage.GetMaxCouleur(): TColor;
begin
  Result := FColorMax;
end;

procedure TMaillage.SetMaxCouleur(const C: TColor);
begin
  FColorMax := C;
end;

procedure TMaillage.SetAlphaBlending(const A: byte);
begin
  FOpacity := A;
end;

function TMaillage.GetAlphaBlending(): byte;
begin
  Result := FOpacity;
end;

function TMaillage.GetCoordsMini(): TPoint3Df;
begin
  Result := FCoordsMini;
end;

function TMaillage.GetCoordsMaxi(): TPoint3Df;
begin
  Result := FCoordsMaxi;
end;


// MNT à TIN (triangles)

procedure TMaillage.ConstruireMaillageTriangleWire(const QScale, QMagn: double);
var
  i, Nb: integer;
  MM, FZ: double;
  QZMin, QZMax : double;
  TR   : TMNTTriangleABC;
  VA, VB, VC: TMNTVertex;

  procedure QAffecterCouleur(const QC: TColor; const QA: byte);
  var
    c: TGLColor;
  begin
    c :=  MakeTGLColorBlending(QC, QA);
    glColor4fv(@c);
  end;
begin
  AfficherMessage(Format('%s.ConstruireMaillageTriangleWire: %d triangles, %d vertex',[ClassName, GetNbTriangles, GetNbVertex]));
  AfficherMessage(Format('-- > Alpha = %d',[FOpacity]));
  AfficherMessage(Format('-- > Scale = %f',[QScale]));
  AfficherMessage(Format('-- > Magn  = %f',[QMagn]));
  Nb := GetNbTriangles();
  if (0 = Nb) then exit;
  MM := QScale;
  FZ := QScale * QMagn;

  QZMin := FCoordsMini.Z * FZ;
  QZMax := FCoordsMaxi.Z * FZ;
  for i := 0 to Nb - 1 do
  begin
    TR := GetTriangle(i);
    if (Not TR.Displayed) then continue;
    VA := GetVertex(TR.PointA);
    VB := GetVertex(TR.PointB);
    VC := GetVertex(TR.PointC);
    glBegin(GL_LINE_LOOP);
      if (FDoUseDegrades) then
      begin
        QAffecterCouleur(VA.Couleur, FOpacity);
        glVertex3f(VA.Position.X * MM, VA.Position.Y * MM, VA.Position.Z * FZ);
        QAffecterCouleur(VB.Couleur, FOpacity);
        glVertex3f(VB.Position.X * MM, VB.Position.Y * MM, VB.Position.Z * FZ);
        QAffecterCouleur(VC.Couleur, FOpacity);
        glVertex3f(VC.Position.X * MM, VC.Position.Y * MM, VC.Position.Z * FZ);
        QAffecterCouleur(VA.Couleur, FOpacity);
        glVertex3f(VA.Position.X * MM, VA.Position.Y * MM, VA.Position.Z * FZ);
      end
      else
      begin
        QAffecterCouleur(FColorMin, FOpacity);
        glVertex3f(VA.Position.X * MM, VA.Position.Y * MM, VA.Position.Z * FZ);
        glVertex3f(VB.Position.X * MM, VB.Position.Y * MM, VB.Position.Z * FZ);
        glVertex3f(VC.Position.X * MM, VC.Position.Y * MM, VC.Position.Z * FZ);
        glVertex3f(VA.Position.X * MM, VA.Position.Y * MM, VA.Position.Z * FZ);
      end;
    glEnd;
  end;
end;


// MNT à TIN (triangles)
procedure TMaillage.ConstruireMaillageTriangleSolide(const QScale, QMagn: double);
var
  i: integer;
  MM, FZ: double;
  TR: TMNTTriangleABC;
  procedure QAffecterCouleur(const QC: TColor; const QA: byte);
  var
    c: TGLColor;
  begin
    c :=  MakeTGLColorBlending(QC, QA);
    glColor4fv(@c);
  end;

  procedure QDessinerTriangle(const QVA, QVB, QVC: TMNTVertex);
  begin
    if (Not TR.Displayed) then exit;
    if (FDoUseDegrades) then
    begin
      QAffecterCouleur(QVA.Couleur, FOpacity);
      glNormal3F(QVA.Normale.X, QVA.Normale.Y, QVA.Normale.Z);
      glVertex3f(QVA.Position.X * MM, QVA.Position.Y * MM, QVA.Position.Z * FZ);

      QAffecterCouleur(QVB.Couleur, FOpacity);
      glNormal3F(QVB.Normale.X, QVB.Normale.Y, QVB.Normale.Z);
      glVertex3f(QVB.Position.X * MM, QVB.Position.Y * MM, QVB.Position.Z * FZ);

      QAffecterCouleur(QVC.Couleur, FOpacity);
      glNormal3F(QVC.Normale.X, QVC.Normale.Y, QVC.Normale.Z);
      glVertex3f(QVC.Position.X * MM, QVC.Position.Y * MM, QVC.Position.Z * FZ);

    end
    else
    begin
      QAffecterCouleur(FColorMin, FOpacity);
      glNormal3F(QVA.Normale.X, QVA.Normale.Y, QVA.Normale.Z);
      glVertex3f(QVA.Position.X * MM, QVA.Position.Y * MM, QVA.Position.Z * FZ);
      glNormal3F(QVB.Normale.X, QVB.Normale.Y, QVB.Normale.Z);
      glVertex3f(QVB.Position.X * MM, QVB.Position.Y * MM, QVB.Position.Z * FZ);
      glNormal3F(QVC.Normale.X, QVC.Normale.Y, QVC.Normale.Z);
      glVertex3f(QVC.Position.X * MM, QVC.Position.Y * MM, QVC.Position.Z * FZ);
    end;
  end;
begin
  CalculerTINNormales(QScale, QMagn);
  AfficherMessage('ConstruireMaillageTriangleSolide:');
  AfficherMessage(Format('--> Couleur max: %d %d %d %d',[Red(FColorMin), Green(FColorMin), Blue(FColorMin), FOpacity]));
  AfficherMessage(Format('--> Couleur min: %d %d %d %d',[Red(FColorMax), Green(FColorMax), Blue(FColorMax), FOpacity]));
  glBegin(GL_TRIANGLES);
  MM := QScale;
  FZ := QScale * QMagn;
  // recto
  for i := 0 to GetNbTriangles - 1 do
  begin
    TR := GetTriangle(i);
    QDessinerTriangle(GetVertex(TR.PointA),
                      GetVertex(TR.PointB),
                      GetVertex(TR.PointC));
  end;
  // verso
  for i :=0 to GetNbTriangles - 1 do
  begin
    TR := GetTriangle(i);
    QDessinerTriangle(GetVertex(TR.PointC),
                      GetVertex(TR.PointB),
                      GetVertex(TR.PointA));
  end;
  glEnd; // GL_TRIANGLES;
end;

function TMaillage.GetCoordosVertexToPlan(const PM: TMNTVertex): TPoint;
var
  EWE : TPoint2Df;
begin
  EWE.setFrom(PM.Position.X, PM.Position.Y);
  Result := FProcGCSToSRC(EWE);
end;
function TMaillage.GetCoordosPlan(const PM: TPoint3Df): TPoint;
var
  EWE : TPoint2Df;
begin
  EWE.setFrom(PM.X, PM.Y);
  Result := FProcGCSToSRC(EWE);
end;



procedure TMaillage.TracerMaillage(const Bmp                : TBGRABitmap;
                                    const QP                 : TProcGCSToSRC;
                                    const Equidistance       : double;
                                    const Isovaleurs         : TArrayOfFloats;
                                    const LineContourColor   : TGHTopoColor;
                                    const IsovaleurColor     : TGHTopoColor;
                                    const DoDrawTriangles    : boolean;
                                    const DoDrawVertex       : boolean;
                                    const viewBox            : TRect2Df);
var
  TR: TMNTTriangleABC;
  i: Integer;
  VX1, VX2, VX3: TMNTVertex;
  PP1, PP2, PP3: TPoint;
  PM3: TPoint3Df;
  QT: TGeoTriangle3D;
  I1, I2: TGeoPoint3D;
  PP: TPoint;
  QZ: Double;
  NbC, Nb: Integer;
  MyIsovaleur: Double;
  MyProfilTopo: TProfilTopo;
  PPP: TPoint3DfOrderedByP;
  CGT: TGHTopoColor;
  procedure QDefineCrayon(const qStyle: TPenStyle; const qWidth: integer; const QColor: TGHTopoColor);
  begin
    bmp.CanvasBGRA.Pen.Style   := qStyle;
    bmp.CanvasBGRA.Pen.Width   := qWidth;
    bmp.CanvasBGRA.Pen.Color   := qColor.toTColor();
    bmp.CanvasBGRA.Pen.Opacity := QColor.Alpha;
  end;
begin
  AfficherMessage(Format('%s.TracerMaillage(%d vertex, %d triangles)', [ClassName, GetNbVertex(), GetNbTriangles()]));

  FProcGCSToSRC := QP;
  //try
    AfficherMessage(Format('Coords mini: %.0f, %.0f, %.0f', [FCoordsMini.X, FCoordsMini.Y, FCoordsMini.Z]));
    AfficherMessage(Format('Coords mini: %.0f, %.0f, %.0f', [FCoordsMaxi.X, FCoordsMaxi.Y, FCoordsMaxi.Z]));
    AfficherMessage(Format('viewBox    : %.0f, %.0f -> %.0f, %.0f', [viewBox.X1, viewBox.Y1, viewBox.X2, viewBox.Y2]));

    if (DoDrawVertex) then
    begin
      AfficherMessage('--> Draw vertex');
      CGT.setFrom(clBlue, 255);
      QDefineCrayon(psSolid, 0, CGT);
      bmp.CanvasBGRA.Brush.Color := clAqua;
      for i := 0 to GetNbVertex - 1 do
      begin
        VX1 := GetVertex(i);
        if (viewBox.ContainsPoint(VX1.Position)) then
        begin
          PP1 := GetCoordosVertexToPlan(VX1);
          bmp.CanvasBGRA.EllipseC(PP1.X, PP1.Y, 2, 2);
          bmp.CanvasBGRA.Font.Height := 10;
          bmp.CanvasBGRA.TextOut(PP1.X + 3, PP1.Y + 3, Format(FORMAT_NB_INTEGER, [VX1.ID]));
        end;
      end;
    end;
    // représentation des triangles
    if (DoDrawTriangles) then
    begin
      AfficherMessage('--> Draw triangles');
      CGT.setFrom(clYellow, 128);
      QDefineCrayon(psSolid, 0, CGT);

      for i:= 0 to GetNbTriangles - 1 do
      begin
        TR := GetTriangle(i);
        VX1 := GetVertex(TR.PointA);
        VX2 := GetVertex(TR.PointB);
        VX3 := GetVertex(TR.PointC);
        if (viewBox.ContainsPoint(VX1.Position) OR
            viewBox.ContainsPoint(VX2.Position) OR
            viewBox.ContainsPoint(VX3.Position)) then
        begin
          PP1 := GetCoordosVertexToPlan(VX1);
          PP2 := GetCoordosVertexToPlan(VX2);
          PP3 := GetCoordosVertexToPlan(VX3);
          bmp.CanvasBGRA.MoveTo(PP1);
          bmp.CanvasBGRA.LineTo(PP2);
          bmp.CanvasBGRA.LineTo(PP3);
          bmp.CanvasBGRA.LineTo(PP1);
        end;
      end;
    end;
    // tracé des isovaleurs
    MyIsovaleur := Isovaleurs[0];
    // courbes de niveaux générales
    for i:= 0 to GetNbTriangles - 1 do
    begin
      TR := GetTriangle(i);

      VX1 := GetVertex(TR.PointA);
      VX2 := GetVertex(TR.PointB);
      VX3 := GetVertex(TR.PointC);

      if (viewBox.ContainsPoint(VX1.Position) OR
          viewBox.ContainsPoint(VX2.Position) OR
          viewBox.ContainsPoint(VX3.Position)) then
      begin
        QT := GetGeoTriangle3D(TR);
        QZ := 10*Trunc(TR.BoundingBox.C1.Z / 10) + 0.01 - 10.00;

        while(QZ < TR.BoundingBox.C2.Z) do
        begin
          NbC := Trunc(QZ);
          QDefineCrayon(psSolid, IIF((NbC mod 5 = 0), 2, 0), LineContourColor);
          if (IntersectPlanHorizontalTriangle(QZ, QT, I1, I2)) then
          begin
            PM3.setFrom(I1.X, I1.Y, MyIsovaleur);
            PP1  := GetCoordosPlan(PM3);
            PM3.setFrom(I2.X, I2.Y, MyIsovaleur);
            PP2  := GetCoordosPlan(PM3);
            bmp.CanvasBGRA.MoveTo(PP1.X, PP1.Y);
            bmp.CanvasBGRA.LineTo(PP2.X, PP2.Y);
          end;
          QZ  += 2.00;
          //NbC += 1;
        end;
      end;
    end; // for i:= 0 to GetNbTriangles - 1 do
    // isovaleur spécifiée
    if (MyIsovaleur > 0.00) then
    begin
      QDefineCrayon(psSolid, 0, IsovaleurColor);
      for i:= 0 to GetNbTriangles() - 1 do
      begin
        TR := GetTriangle(i);
        QT := GetGeoTriangle3D(TR);
        if (IntersectPlanHorizontalTriangle(MyIsovaleur + 1e-03, QT, I1, I2)) then
        begin
          PM3.setFrom(I1.X, I1.Y, MyIsovaleur);
          PP1  := GetCoordosPlan(PM3);
          PM3.setFrom(I2.X, I2.Y, MyIsovaleur);
          PP2  := GetCoordosPlan(PM3);
          bmp.CanvasBGRA.MoveTo(PP1.X, PP1.Y);
          bmp.CanvasBGRA.LineTo(PP2.X, PP2.Y);
        end;
      end;
    end;
    // profils topo
    Nb := self.GetNbProfilsTopo();
    AfficherMessage(Format('%d profils', [Nb]));
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do
      begin
        MyProfilTopo := GetProfilTopo(i);
        QDefineCrayon(psSolid, MyProfilTopo.LineAttributes.LineWidthInPixels,
                               MyProfilTopo.LineAttributes.ColorRGBA);
        AfficherMessage(Format('Profil %d - %d points', [i, MyProfilTopo.GetNbPointsProfilTN()]));
        if (MyProfilTopo.GetNbPointsProfilTN() > 0) then
        begin
          PPP := MyProfilTopo.GetPointProfilTN(0);
          PM3.setFrom(PPP.X, PPP.Y, PPP.Z);
          PP1  := GetCoordosPlan(PM3);
          PPP  := MyProfilTopo.GetPointProfilTN(MyProfilTopo.GetNbPointsProfilTN() - 1);
          PM3.setFrom(PPP.X, PPP.Y, PPP.Z);
          PP2  := GetCoordosPlan(PM3);
          bmp.CanvasBGRA.MoveTo(PP1.X, PP1.Y);
          bmp.CanvasBGRA.LineTo(PP2.X, PP2.Y);
        end;
      end;
    end;
  //except
  //end;
end;

// transformation d'un triangle TTriangulation en TGeoTriangle;
function TMaillage.GetGeoTriangle2D(const QT: TMNTTriangleABC): TGeoTriangle2D;
var
 V1, V2, V3: TMNTVertex;
begin
  V1 := GetVertex(QT.PointA);
  V2 := GetVertex(QT.PointB);
  V3 := GetVertex(QT.PointC);

  Result[1].x := V1.Position.X;
  Result[1].y := V1.Position.Y;

  Result[2].x := V2.Position.X;
  Result[2].y := V2.Position.Y;

  Result[3].x := V3.Position.X;
  Result[3].y := V3.Position.Y;
end;
// transformation d'un triangle TTriangulation en TGeoTriangle;
function TMaillage.GetGeoTriangle3D(const QT: TMNTTriangleABC): TGeoTriangle3D;
var
 V1, V2, V3: TMNTVertex;
begin
 V1 := GetVertex(QT.PointA);
 V2 := GetVertex(QT.PointB);
 V3 := GetVertex(QT.PointC);

 Result[1].x := V1.Position.X;
 Result[1].y := V1.Position.Y;
 Result[1].z := V1.Position.Z;


 Result[2].x := V2.Position.X;
 Result[2].y := V2.Position.Y;
 Result[2].z := V2.Position.Z;


 Result[3].x := V3.Position.X;
 Result[3].y := V3.Position.Y;
 Result[3].z := V3.Position.Z;
end;

function TMaillage.CalcBoundingBoxTriangle(const T: TMNTTriangleABC): TMNTTriangleABC;
const
  INF = 1E24;
  procedure WU(const P: integer);
  var
    VX: TMNTVertex;
  begin
    VX := GetVertex(P);
    if (VX.Position.X < Result.BoundingBox.C1.X) then Result.BoundingBox.C1.X := VX.Position.X;
    if (VX.Position.Y < Result.BoundingBox.C1.Y) then Result.BoundingBox.C1.Y := VX.Position.Y;
    if (VX.Position.Z < Result.BoundingBox.C1.Z) then Result.BoundingBox.C1.Z := VX.Position.Z;
    if (VX.Position.X > Result.BoundingBox.C2.X) then Result.BoundingBox.C2.X := VX.Position.X;
    if (VX.Position.Y > Result.BoundingBox.C2.Y) then Result.BoundingBox.C2.Y := VX.Position.Y;
    if (VX.Position.Z > Result.BoundingBox.C2.Z) then Result.BoundingBox.C2.Z := VX.Position.Z;
  end;
begin
  Result := T;
  Result.BoundingBox.C1.setFrom( INF,  INF,  INF);
  Result.BoundingBox.C2.setFrom(-INF, -INF, -INF);
  WU(T.PointA);
  WU(T.PointB);
  WU(T.PointC);
end;
// conversion d'un maillage SRTM HGT en maillage normal
// Modèle: SRTM3
// Résolution: 3 secondes d'arc soit (93 m) * sind(Latitude), avec sind: sinus d'unn angle en degrés
// La latitude et longitude du carré se déduisent du nom de fichier
// qui est de la forme ABBCDDD (eg: N42E131.hgt)
// avec:
// A   = N ou S
// BB  = latitude (00..89)
// C   = E ou W
// DDD = Longitude (000..179)
function TMaillage.ChargerMaillageDepuisHGT(const FichierHGT: string): boolean;
var
  fps: file of byte;
  fp : File;
  FF, A, BB, C, DDD: String;
  TailleFichier: Int64;
  EWE: double;
  NbCarreaux: Int64;
  Lat0: double;
  Long0: double;
  Buffer: array[0 .. 2401] of byte;
  BuffCount: integer;
  i, j: Integer;
  WU  : SmallInt;
  Cur: Integer;
  TailleBuffer: Integer;
  QAlt: Double;
  MyBitMap: TBitmap;
  h: double;
  Cz: Int64;

begin
  SetTypeMaillage(tmUNKNOWN, 1087);
  Result := False;
  AfficherMessage(Format('%s.ChargerMaillageDepuisHGT: %s', [ClassName, FichierHGT]));
  FF := ExtractFileName(FichierHGT);
  A  := Copy(FF, 1, 1);
  BB := Copy(FF, 2, 2);
  C  := Copy(FF, 4, 1);
  DDD:= Copy(FF, 5, 3);
  Lat0  := ConvertirEnNombreReel(BB, -1.00);
  Long0 := ConvertirEnNombreReel(DDD, -181.00);
  // taille du fichier et nombre de lignes/colonnes + contrôles initiaux
  AssignFile(fps, FichierHGT);
  try
    Reset(fps);
    TailleFichier := FileSize(fps);
    // la racine carrée de la demi-taille du fichier doit être strictement entière
    EWE := Sqrt(TailleFichier / 2.0);
    if (Abs(frac(EWE)) > 0) then Exit;
    NbCarreaux := Trunc(EWE);
    AfficherMessage(Format('A=%s, BB=%s, C=%s, DDD=%s, FS = %d, NC = %d', [A, BB, C, DDD, TailleFichier, NbCarreaux]));
    SetTypeMaillage(tmREGULAR_GRID, 1107);
    NbCarreaux := Trunc(EWE);
    FNbVertexX := 1201;
    FNbVertexY := 1201;
  finally
    CloseFile(fps);
  end;

  AssignFile(fp, FichierHGT);
  try
    Reset(fp, 1); // fichiers non typés -> deuxième paramètre indispensable
    // 92.76... = 3'' * circonférence équatoriale de la Terre en m / (3600'' * 360°)
    FPasX := 0.50 * (92.76851852 * sin(Lat0 * PIDiv180) +
                     92.76851852 * sin((1+Lat0) * PIDiv180));
    FPasY := 92.7685;
    SetLength(ZMatrix, FNbVertexX, FNbVertexY);
    TailleBuffer := Sizeof(Buffer);
    AfficherMessage(Format('-- Buffer de %d octets', [TailleBuffer]));
    FCoordsMaxi.Z := -1E24;
    FCoordsMini.Z := +1E24;
    for i := 0 to FNbVertexY - 1 do
    begin
      {$I-}
      BlockRead(fp, Buffer, TailleBuffer, BuffCount);
      {$I+}
      for j := 0 to FNbVertexX - 1 do
      begin
        Cur := j * 2;
        WU := Buffer[Cur] * 256 + Buffer[Cur+1];
        QAlt := WU;
        if (QAlt< -450.00) then QAlt := -450.00;
        if (QAlt > FCoordsMaxi.Z) then FCoordsMaxi.Z := QAlt;
        if (QAlt < FCoordsMini.Z) then FCoordsMini.Z := QAlt;
        // inverser les indices pour retrouver les bonnes coordonnées
        ZMatrix[i, j] := QAlt;
      end;
    end;
    AfficherMessage(Format('%f, %f', [FCoordsMini.Z, FCoordsMaxi.Z]));
  finally
    CloseFile(fp);
  end;
end;
function TMaillage.LoadMaillage(const FichierMaillage: string): boolean;
var
  NbLignesLues : Int64;
  VV           : TMNTVertex;
  TR           : TMNTTriangleABC;
  i, j         : integer;
  pFileMAI     : TextFile;
  W    : TPoint3Df;
  PrmsLn       : TGHStringArray;
  EWE, MyLigne: String;
  QQ: Integer;
  function LireLigne(): string;
  var
    TmpBuff: string;
  begin
    Readln(PFileMAI,TmpBuff);
    Result := Trim(TmpBuff);
    NbLignesLues += 1;
  end;
begin
  AfficherMessage(Format('%s.LireFichierMaillage: %s',[ClassName, FichierMaillage]));

  NbLignesLues := 0;
  FNbVertexX   := 0;
  FNbVertexY   := 0;
  ResetMaillageTIN();
  FListeProfilsTopo.ClearListeProfils();
  // Maillage KO par défaut
  FMaillageValide := false;
  // opaque par défaut
  FOpacity := 128;
  Result := false;
  if Not (FileExistsUTF8(FichierMaillage)) then Exit;
  AssignFile(PFileMAI,FichierMaillage);
  try
    // Lecture du fichier
    Reset(PFileMAI);
    // Titre du maillage
    FNomDuMaillage := LireLigne();
    AfficherMessage('--> Nom du maillage: ' + FNomDuMaillage);
    // Type de maillage
    EWE := UpperCase(LireLigne());
    if      (EWE = 'REGULAR_GRID') then SetTypeMaillage(tmREGULAR_GRID, 1197)
    else if (EWE = 'TRIANGLES')    then SetTypeMaillage(tmTRIANGLES, 1198)
    else                                SetTypeMaillage(tmUNKNOWN, 1199);
    case FTypeMaillage of
      tmREGULAR_GRID:
      begin
        AfficherMessage('--> Type de maillage: MNT à pas régulier');
        //Couleur
        FColorMax       := StrToIntDef(LireLigne(), clMaroon);
        FColorMin       := StrToIntDef(LireLigne(), clGreen);
        // Nombre de lignes
        FNbVertexX := StrToIntDef(LireLigne(), 0);
        FNbVertexY := StrToIntDef(LireLigne(), 0);
        AfficherMessage(Format('--> Taille: %d  x %d', [FNbVertexX, FNbVertexY]));
        // FNbVertexY OU FNbVertexX nuls => maillage invalide --> [ ]
        if (FNbVertexY * FNbVertexX = 0) then Exit;
        // Coordonnées mini
        FCoordsMini.X := ConvertirEnNombreReel(LireLigne(), 0.00);
        FCoordsMini.Y := ConvertirEnNombreReel(LireLigne(), 0.00);

        // Coordonnées maxi
        FCoordsMaxi.X := ConvertirEnNombreReel(LireLigne(), 0.00);
        FCoordsMaxi.Y := ConvertirEnNombreReel(LireLigne(), 0.00);
        AfficherMessage(Format('--> Etendue X: %.2f - %.2f', [FCoordsMini.X, FCoordsMaxi.X]));
        AfficherMessage(Format('--> Etendue Y: %.2f - %.2f', [FCoordsMini.Y, FCoordsMaxi.Y]));
        // redimensionner les tableaux
        SetLength(ZMatrix,  FNbVertexY, FNbVertexX);
        // lecture d'une donnée obsolète
        EWE := LireLigne();
        // chargement de la table
        for i := 0 to FNbVertexY - 1 do
        begin
          AfficherMessage(Format('--> Lecture ligne: %d', [i]));
          for j := 0 to FNbVertexX - 1do
           begin
             ZMatrix[i,j] := ConvertirEnNombreReel(LireLigne(), 0.00);
             if (ZMatrix[i,j] > FCoordsMaxi.Z) then FCoordsMaxi.Z := ZMatrix[i,j];
             if (ZMatrix[i,j] < FCoordsMini.Z) then FCoordsMini.Z := ZMatrix[i,j];
           end;
        end;
        EWE := ChooseString(Ord(FTypeMaillage), ['Inconnu', 'Grille', 'Triangles']);
        AfficherMessage(Format('Maillage "%s" de %d lignes x %d colonnes ; %d lignes lues', [EWE, FNbVertexY, FNbVertexX, NbLignesLues]));
        FNomTypeMaillage:=Format('Grille: (%d * %d)', [FNbVertexY, FNbVertexX]);
        // construire le TIN
        ConvertitMaillageGridEnTIN();
        Result := True;
      end;
    tmTRIANGLES:
      begin
        SetLength(ZMatrix, 1, 1);   // initialiser tableaux inutilisés
        AfficherMessageErreur('--> Type de maillage: Réseau irrégulier de triangles');
        //Couleur
        FColorMax       := StrToIntDef(LireLigne(), clMaroon);
        FColorMin       := StrToIntDef(LireLigne(), clGreen);
        // section  Vertex
        EWE := LireLigne();  // skip ligne VERTEX
        EWE := LireLigne();  // ligne contenant le nombre de sommets (inutilisé)
        AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
        AfficherMessageErreur('--> Lecture section VERTEX');
        QQ := 0;
        repeat //for i:=0 to Nb - 1 do
          try
            MyLigne  := LireLigne();
            if (Trim(MyLigne) = '') then Continue;
            PrmsLn := Split(MyLigne, FIELD_SEPARATOR_TAB); //Split(LireLigne(), SEPARATOR_TAB);
            QQ     := StrToInt(PrmsLn[0]);
            if (QQ >= 0) then
            begin
              VV.ID  := StrToIntDef(PrmsLn[0], 0);
              VV.Position.setFrom(ConvertirEnNombreReel(PrmsLn[1], -1.00),
                                  ConvertirEnNombreReel(PrmsLn[2], -1.00),
                                  ConvertirEnNombreReel(PrmsLn[3], -1.00));
              VV.Normale.Empty();
              VV.Couleur := FColorMin;
              VV.Displayed := true;
              AddVertex(VV);
            end;
          except
            Continue;
          end;
        until (QQ = -1); //end;
        AfficherMessage(Format('--> %d vertex',[GetNbVertex()]));
        //EWE := LireLigne();   // lire le '-1'
        EWE := LireLigne();     // lire le TRIANGLES
        EWE := LireLigne();     // ligne contenant le nombre de triangles (inutilisé)
        AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
        QQ := 0;
        repeat //for i := 0 to Nb - 1 do
          try
            MyLigne := LireLigne();
            if (Trim(MyLigne) = '') then Continue;
            PrmsLn  := Split(MyLigne, FIELD_SEPARATOR_TAB);  // nombre de triangles
            QQ      := StrToIntDef(PrmsLn[0], -1);
            if (QQ >=0) then
            begin
              TR.Numero    := StrToIntDef(PrmsLn[0],0);
              TR.PointA    := StrToIntDef(PrmsLn[1],0);
              TR.PointB    := StrToIntDef(PrmsLn[2],0);
              TR.PointC    := StrToIntDef(PrmsLn[3],0);
              TR.Displayed := True;
              TR.Couleur   := FColorMin;
              TR := CalcBoundingBoxTriangle(TR);
              if (IsValidTriangle(TR)) then AddTriangle(TR)
                                       else AfficherMessageErreur(Format('--->Triangle dégénéré: %d - %d %d %d',[i,TR.PointA, TR.PointB,TR.PointC]));
            end;
          except
            Continue;
          end;
        until (QQ = -1); //end;
        AfficherMessageErreur(Format('--> %d triangles',[GetNbTriangles()]));
        FNomTypeMaillage:=Format('MNT Triangles: (%d)',[GetNbTriangles()]);

        // La BoundingBox est calculée lors de l'ajout d'un vertex AddVertex()
        AfficherMessageErreur('Maillage TIN OK');
        FMaillageValide := True;
        AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
        result := True;
      end;
    tmUNKNOWN:
       begin
         // initialiser tableaux inutilisés
         SetLength(ZMatrix,   1,1);
         FMaillageValide := false;
         Result := false;
       end;
    end;
    // contrôle
    AfficherMessageErreur('Etendue du maillage:');
    AfficherMessageErreur(Format('--> X: De %.0f a %.0f', [FCoordsMini.X, FCoordsMaxi.X]));
    AfficherMessageErreur(Format('--> Y: De %.0f a %.0f', [FCoordsMini.Y, FCoordsMaxi.Y]));
    AfficherMessageErreur(Format('--> Z: De %.0f a %.0f', [FCoordsMini.Z, FCoordsMaxi.Z]));
  finally
    CloseFile(pFileMAI);
  end;
end;

function TMaillage.SaveMaillageTIN(const FichierMaillage: string): boolean;
const
  FMTColor    = '$%.2X%.2X%.2X';
  FMTVertex   = FORMAT_NB_INTEGER + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC;
  //4262	1742	1957	1743	0
  FMTTriangle = FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER;

var
  fp: Textfile;
  QNbTriangles, QNbVertex, i: Integer;
  MyTriangle: TMNTTriangleABC;
  MyVertex: TMNTVertex;
begin
  QNbVertex     := self.GetNbVertex();
  QNbTriangles  := self.GetNbTriangles();
  AfficherMessage(format('%s.SaveMaillage(): %d vertex, %d triangles dans %s', [ClassName, QNbVertex, QNbTriangles, FichierMaillage]));
  if (0 = QNbVertex * QNbTriangles) then Exit;
  assignfile(fp, FichierMaillage);
  try
    Rewrite(fp);
    WriteLn(fp, Format('TIN %s created %s', [FNomDuMaillage, DateTimeToStr(Now)]));
    WriteLn(fp, 'TRIANGLES'); // TODO TIN uniquement
    WriteLn(fp, Format(FMTColor, [red(FColorMin), green(FColorMin), blue(FColorMin)]));
    WriteLn(fp, Format(FMTColor, [red(FColorMax), green(FColorMax), blue(FColorMax)]));
    WriteLn(fp, 'VERTEX');
    WriteLn(fp, Format(FORMAT_NB_INTEGER, [QNbVertex]));
    for i := 0 to QNbVertex - 1 do
    begin
      MyVertex := self.GetVertex(i);
      WriteLn(fp, Format(FMTVertex, [MyVertex.ID, MyVertex.Position.X, MyVertex.Position.Y, MyVertex.Position.Z]));
    end;
    WriteLn(fp, Format(FORMAT_NB_INTEGER, [-1]));
    WriteLn(fp, 'TRIANGLE');
    WriteLn(fp, Format(FORMAT_NB_INTEGER, [QNbTriangles]));
    for i := 0 to QNbTriangles - 1 do
    begin
      MyTriangle := self.GetTriangle(i);
      WriteLn(fp, Format(FMTTriangle, [MyTriangle.Numero, MyTriangle.PointA, MyTriangle.PointB, MyTriangle.PointC, 0]));
    end;
    WriteLn(fp, Format(FORMAT_NB_INTEGER, [-1]));

  finally
    closefile(fp);
  end;
end;

function TMaillage.ImporterXYZ(const FichierXYZ: string; const DoPreserve: boolean = false): boolean;
var
  fp: TextFile;
  MyLigne: String;
  EWE: TGHStringArray;
begin
  result := false;
  AfficherMessage(Format('%s.ImporterXYZ: %s', [ClassName, FichierXYZ]));
  //X	Y	Z
  // 526691.76887604	6484683.31611499	180.000
  self.FTableTriangles.ClearListe(); // il faudra retrianguler dans tous les cas
  if (not DoPreserve) then self.FTableVertex.ClearListe();
  AssignFile(fp, FichierXYZ);
  try
    ReSet(fp);
    ReadLn(fp, MyLigne);
    while (not eof(fp)) do
    begin
      ReadLn(fp, MyLigne);
      MyLigne := StringReplace(MyLigne, ';', #9, [rfReplaceAll]);

      EWE := split(MyLigne, #9);
      AddVertex(ConvertirEnNombreReel(EWE[0], 0.00),
                ConvertirEnNombreReel(EWE[1], 0.00),
                ConvertirEnNombreReel(EWE[2], 0.00));
    end;
    result := (GetNbVertex() > 0)
  finally
    Closefile(fp);
  end;

end;

// lire un maillage VisualTopo
function TMaillage.ChargerMaillageDepuisSUR(const FichierSUR: string): boolean;
var
  fp: TextFile;
  i, j: integer;
  EWE: TGHStringArray;
  dummy: String;
  MaLigne: String;
  QAT: String;
  QIN: double;
  function LireLigne(): string;
  var
    WU: string;
  begin
    ReadLn(fp, WU);
    Result := Trim(WU);
  end;
begin
  AfficherMessage(Format('%s.ChargerMaillageDepuisSUR: %s', [ClassName, FichierSUR]));
  ResetMaillageTIN();
  // Maillage KO par défaut
  FMaillageValide := false;
  // opaque par défaut
  FOpacity := 128;
  Result := False;
  SetTypeMaillage(tmREGULAR_GRID, 1364);
  AssignFile(fp, FichierSUR);
  try
    Reset(fp);
    // Ligne 1: Surface 390.000 410.020 0.070 287 3085.000 3095.080 0.090 113
    EWE := Split(LireLigne(), ' ');
    FCoordsMini.X := 1000 * ConvertirEnNombreReel(EWE[1], -1.00);
    FCoordsMaxi.X := 1000 * ConvertirEnNombreReel(EWE[2], -1.00);
    FPasX         := 1000 * ConvertirEnNombreReel(EWE[3], -1.00);
    FNbVertexX    := StrToIntDef(EWE[4], -1);
    FCoordsMini.Y := 1000 * ConvertirEnNombreReel(EWE[5], -1.00);
    FCoordsMaxi.Y := 1000 * ConvertirEnNombreReel(EWE[6], -1.00);
    FPasY         := 1000 * ConvertirEnNombreReel(EWE[7], -1.00);
    FNbVertexY    := StrToIntDef(EWE[8], -1);
    AfficherMessageErreur(Format('Sur X: De %.2f a %.2f; Spc = %.2f; NbVertex: %d', [FCoordsMini.X, FCoordsMaxi.X, FPasX, FNbVertexX]));
    AfficherMessageErreur(Format('Sur Y: De %.2f a %.2f; Spc = %.2f; NbVertex: %d', [FCoordsMini.Y, FCoordsMaxi.Y, FPasY, FNbVertexY]));
    // Quelques tests
    if (0 = FNbVertexX * FNbVertexY) then Exit; // nombre de lignes ou colonnes nul -> []
    // matrice
    SetLength(ZMatrix, 1, 1);
    //SetLength(ZMatrix, FNbVertexX, FNbVertexY);
    // redimensionner les tableaux
    SetLength(ZMatrix,  FNbVertexY, FNbVertexX);
    // Ligne 2: vide
    MaLigne := LireLigne();
    // Ligne 3 et suivantes
    for i := 0 to FNbVertexY - 1 do
    begin
      MaLigne := LireLigne();
      for j := 0 to FNbVertexX - 1 do
      begin
        QAT := ExtractFirstElementOfStr(MaLigne, ' ');
        QIN := ConvertirEnNombreReel(QAT, -500.00);
        if (QIN < FCoordsMini.Z) then FCoordsMini.Z := QIN;
        if (QIN > FCoordsMaxi.Z) then FCoordsMaxi.Z := QIN;
        // /!\ En format SUR, le YMin est dans ZMatrix[m-1, n]
        ZMatrix[FNbVertexY - i - 1, j] := QIN;
      end;
    end;
    AfficherMessageErreur(Format('Alt: %.2f a %.2f', [FCoordsMini.Z, FCoordsMaxi.Z]));
    AfficherMessage('OK');
    Result := True;
  finally
    CloseFile(fp);
  end;
  try
    FMaillageValide := ConvertitMaillageGridEnTIN();
  except
  end;
end;

// convertit un MNT grille en MNT triangles
function TMaillage.ConvertitMaillageGridEnTIN(): boolean;
var
  TR: TMNTTriangleABC;
  VX, VV: TMNTVertex;
  QIdxVertex : Int64;
  i, j: Integer;
  QX, QY: double;
  QEspcX, QEspcY: double;
  EWE : double;
  QIdxTriangle: Integer;
  QIdxVertexA: Integer;
  QIdxVertexB: Integer;
  QIdxVertexC: Integer;
  QIdxVertexD: Integer;
  dx, dy     : double;
  QMini, QMaxi: TPoint3Df;
  Q0: Integer;
begin
  Result := False;
  AfficherMessage(Format('%s.ConvertitMaillageGridEnTIN', [ClassName]));
  SetTypeMaillage(tmTRIANGLES, 1591);
  ResetMaillageTIN();
  // construction des vertex
  QX := FCoordsMini.X;
  QY := FCoordsMini.Y;
  dx := FCoordsMaxi.X - FCoordsMini.X;
  dy := FCoordsMaxi.Y - FCoordsMini.Y;
  QEspcX := dx / (FNbVertexX - 1);
  QEspcY := dy / (FNbVertexY - 1);
  AfficherMessage(Format('--> Grille de vertex de FNbVertexY = %d lignes x FNbVertexX = %d colonnes = %d vertex attendus', [FNbVertexY, FNbVertexX, FNbVertexY * FNbVertexX]));
  AfficherMessage(Format('--> De (%.2f, %.2f) a (%.2f, %.2f)', [ FCoordsMini.X,  FCoordsMini.Y,  FCoordsMaxi.X,  FCoordsMaxi.Y]));
  AfficherMessage(Format('--> L = %.2f, H = %.2f, ex = %.2f, ey = %.2f', [dx, dy, QEspcX, QEspcY]));
  QIdxVertex := 0;
  for i := 0 to FNbVertexY - 1 do
  begin
    QY := FCoordsMini.Y + QEspcY * i;
    for j := 0 to FNbVertexX - 1 do
    begin
      QX := FCoordsMini.X + QEspcX * j;
      VV.setFrom(QIdxVertex, QX, QY, ZMatrix[i][j], FColorMin);
      AddVertex(VV);
      QIdxVertex += 1;
    end;
  end;
  AfficherMessage(Format('--> Construction des %d vertex OK', [GetNbVertex]));
  // contrôle: mini et maxi des vertex. A factoriser
  QMini.setFrom( INFINI,  INFINI,  INFINI);
  QMaxi.setFrom(-INFINI, -INFINI, -INFINI);
  for i := 0 to GetNbVertex - 1 do
  begin
    VV := GetVertex(i);
    if (VV.Position.X < QMini.X) then QMini.X := VV.Position.X;
    if (VV.Position.Y < QMini.Y) then QMini.Y := VV.Position.Y;
    if (VV.Position.Z < QMini.Z) then QMini.Z := VV.Position.Z;
    if (VV.Position.X > QMaxi.X) then QMaxi.X := VV.Position.X;
    if (VV.Position.Y > QMaxi.Y) then QMaxi.Y := VV.Position.Y;
    if (VV.Position.Z > QMaxi.Z) then QMaxi.Z := VV.Position.Z;
  end;
  AfficherMessage(Format('--> De (%.2f, %.2f, %.2f) a (%.2f, %.2f, %.2f)', [ QMini.X,  QMini.Y, QMini.Z, QMaxi.X,  QMaxi.Y, QMaxi.Z]));
  // 73800 triangles
  QIdxTriangle := 0;
  AfficherMessage(Format('i va de 0 a %d-1; j va de 0 a %d-1; - Reserved: %d, %d', [FNbVertexY, FNbVertexX, high(ZMatrix[0]), high(ZMatrix[1])]));
  for i := 1 to FNbVertexY - 1 do
  begin
    for j := 1 to FNbVertexX - 1 do
    begin
      try
        QIdxVertexA := FNbVertexX * (i - 1) + (j - 1);
        QIdxVertexB := FNbVertexX * (i - 1) + (j);
        QIdxVertexC := FNbVertexX * (i)     + (j - 1);
        QIdxVertexD := FNbVertexX * (i)     + (j);
        TR.Numero := QIdxTriangle;
        TR.PointA := QIdxVertexA;  TR.PointB := QIdxVertexB;  TR.PointC := QIdxVertexC;
        TR := CalcBoundingBoxTriangle(TR);
        AddTriangle(TR);
        QIdxTriangle += 1;
        TR.Numero := QIdxTriangle;
        TR.PointA := QIdxVertexB;  TR.PointB := QIdxVertexD;  TR.PointC := QIdxVertexC;
        TR := CalcBoundingBoxTriangle(TR);
        AddTriangle(TR);
        QIdxTriangle += 1;
      except
      end;
    end;
  end;
  AfficherMessage(Format('--> Construction des %d triangles OK', [GetNbTriangles]));
  AfficherMessage(Format('---> %d vertex (tableau de 0 .. %d vertex)', [QIdxVertex, GetNbVertex]));
  Result := True;
end;
//******************************************************************************
procedure TMaillage.AddVertex(const V: TMNTVertex);
begin
  UpdateMiniMaxi(V);
  FTableVertex.AddElement(V);
end;

procedure TMaillage.AddVertex(const QX, QY, QZ: double);
var
  V: TMNTVertex;
begin
  V.ID := GetNbVertex();
  V.Position.setFrom(QX, QY, QZ);
  V.Normale.Empty();
  UpdateMiniMaxi(V);
  FTableVertex.AddElement(V);
end;

function TMaillage.GetVertex(const Idx: integer): TMNTVertex;
begin
  Result := FTableVertex.GetElement(Idx);
end;
procedure TMaillage.PutVertex(const Idx: integer; const V: TMNTVertex);
begin
  FTableVertex.PutElement(Idx, V);
end;
function TMaillage.GetNbVertex(): integer;
begin
  Result := FTableVertex.GetNbElements();
end;

//------------------------------------------------------------------------------
procedure TMaillage.AddTriangle(const V: TMNTTriangleABC);
begin
  FTableTriangles.AddElement(V);
end;
function TMaillage.GetNbTriangles(): integer;
begin
  Result := FTableTriangles.GetNbElements;
end;
procedure TMaillage.PutTriangle(const Idx: integer; const V: TMNTTriangleABC);
begin
  FTableTriangles.PutElement(Idx, V);
end;
function TMaillage.GetTriangle(const Idx: integer): TMNTTriangleABC;
begin
  Result := FTableTriangles.GetElement(Idx);
end;
//------------------------------------------------------------------------------
// MNT à pas réguliers: Construction d'images
function TMaillage.MakeImageFromGRD(const DoYReverse: boolean): TBitmap;
var
  h: Double;
  i, j: Integer;
  Cz: Int64;
  WU: Integer;
begin
  // feedback: crée une image de taille n*n, en niveaux de gris
  AfficherMessage('Construction image');
  Result := TBitmap.Create;
  try
    Result.Width  := FNbVertexX;
    Result.Height := FNbVertexY;
    h := FCoordsMaxi.Z; // - FCoordsMini.Z;
    for i := 0 to FNbVertexY - 1 do
    begin
      WU := IIF(DoYReverse, FNbVertexY - i - 1, i);
      for j := 0 to FNbVertexX - 1 do
      begin
        Cz := round(256 * (ZMatrix[i, j] - FCoordsMini.Z) / h);
        Result.Canvas.Pixels[j, WU] := RGBToColor(Cz, Cz, Cz);
      end;
    end;
    //AfficherMessage('Sauvegarde image');
    //Result.SaveToFile(ChangeFileExt(FichierHGT, '.png'));
    AfficherMessage('Finished');
  finally
  //  MyBitMap.Free;
  end;
end;
// MNT à pas régulier: export format SUR
// Test OK
procedure TMaillage.ExportToSUR(const FichierSUR: string);
var
  fp: TextFile;
  QSpcX, QSpcY: double;
  i, j: Integer;
  QIN : double;
  EWE: String;
  procedure WriteLine(const S: string);
  begin
    WriteLn(fp, S);
  end;
begin
  AfficherMessage(Format('%s.ExportToSUR: %s - %d x %d', [ClassName, FichierSUR, FNbVertexX, FNbVertexY]));
  AssignFile(fp, FichierSUR);
  try
    ReWrite(fp);
    //Surface 390.000 410.020 0.070 287 3085.000 3095.080 0.090 113
    QSpcX := (FCoordsMaxi.X - FCoordsMini.X) / (FNbVertexX - 1);
    QSpcY := (FCoordsMaxi.Y - FCoordsMini.Y) / (FNbVertexY - 1);

    WriteLine(Format('Surface %.3f %.3f %.3f %d %.3f %.3f %.3f %d',
                     [FCoordsMini.X / 1000, FCoordsMaxi.X / 1000, QSpcX / 1000, FNbVertexX,
                      FCoordsMini.Y / 1000, FCoordsMaxi.Y / 1000, QSpcY / 1000, FNbVertexY
                     ]));
    WriteLine('');
    for i := 0 to FNbVertexY - 1 do
    begin
      EWE := '';
      for j := 0 to FNbVertexX - 1 do
      begin
        QIN := ZMatrix[FNbVertexY - i - 1, j];
        // /!\ En format SUR, le YMin est dans ZMatrix[m-1, n]
        EWE := EWE + Format('%.0f ', [QIN]);
      end;
      WriteLine(EWE);
    end;
  finally
    closefile(fp);
  end;
  Reset(fp);
end;

// extraction des coordonnées d'intersection d'un plan vertical et d'un triangle
// QT: Triangle traversé
// QX1, QX2, QY1, QY2: Coordonnées des extrémités de la trace horizontale du plan
function TMaillage.IntersectPlanVerticalAndTriangle(const QT: TMNTTriangleABC; const ExtrProfil1, ExtrProfil2: TPoint3Df; out QIntersect: TIntersectPlanTriangle): boolean;
var
  P1, P2, P3: TMNTVertex;
  function CalcZ(const AP1, AP2: TMNTVertex; const QIx, QIy: double): double;
  var
    QR1, QR2, m: double;
    dx, dy, dz : double;
  begin
    dx := QIx - AP1.Position.X;
    dy := QIy - AP1.Position.y;
    QR1 := Hypot(AP2.Position.X - AP1.Position.X, AP2.Position.Y - AP1.Position.Y);
    QR2 := Hypot(dx, dy);
    dz  := AP2.Position.Z - AP1.Position.Z;
    m   := QR2 / QR1;
    Result := AP1.Position.Z + m * dz;
  end;
  function CalcIntersect(const AP1, AP2, AP3: TMNTVertex) : boolean;
  var
    IX1, IY1: TGeoFloat;
  begin
    Result := False;
    // premier côté ?
    if Intersect(AP1.Position.X, AP1.Position.Y,
                 AP2.Position.X, AP2.Position.Y,
                 ExtrProfil1.X, ExtrProfil1.Y,
                 ExtrProfil2.X, ExtrProfil2.Y,
                 IX1, IY1) then
    begin
      QIntersect.X1 := IX1;
      QIntersect.Y1 := IY1;
      QIntersect.Z1 := CalcZ(AP1, AP2, IX1, IY1);
      // coté suivant ?
      if Intersect(AP2.Position.X, AP2.Position.Y, AP3.Position.X, AP3.Position.Y,
                   ExtrProfil1.X, ExtrProfil1.Y,
                   ExtrProfil2.X, ExtrProfil2.Y,
                   IX1, IY1)
      then
      begin
        QIntersect.X2 := IX1;
        QIntersect.Y2 := IY1;
        QIntersect.Z2 := CalcZ(AP2, AP3, IX1, IY1);
      end else
      begin
        Intersect(AP3.Position.X, AP3.Position.Y, AP1.Position.X, AP1.Position.Y,
                  ExtrProfil1.X, ExtrProfil1.Y,
                  ExtrProfil2.X, ExtrProfil2.Y,
                  IX1, IY1);
        QIntersect.X2 := IX1;
        QIntersect.Y2 := IY1;
        QIntersect.Z2 := CalcZ(AP3, AP1, IX1, IY1);
      end;
      Result := True;
    end;
  end;
begin
  Result := False;
  P1 := GetVertex(QT.PointA);
  P2 := GetVertex(QT.PointB);
  P3 := GetVertex(QT.PointC);
  (*
  AfficherMessage(Format('P1 =(%.2f, %.2f, %.2f), P2 = (%.2f, %.2f, %.2f), P3 = (%.2f, %.2f, %.2f)',
                              [P1.X, P1.Y, P1.Z,
                               P2.X, P2.Y, P2.Z,
                               P3.X, P3.Y, P3.Z
                              ]));
  //*)
  if      CalcIntersect(P1, P2, P3) then Exit(True)
  else if CalcIntersect(P2, P3, P1) then Exit(True)
  else if CalcIntersect(P3, P1, P2) then Exit(True);
end;
(*
function TMaillage.IntersectionTriangleSegment(const P1, P2: TPoint3Df; const Triangle: TMNTTriangleABC; var Intersects: TArrayPoints3Df): boolean;
var
  // plan vertical d'intersection
  DemiPlanTriangle1: TGeoTriangle3D;
  DemiPlanTriangle2: TGeoTriangle3D;

  AreteAB      : TGeoLine3D;
  AreteBC      : TGeoLine3D;
  AreteCA      : TGeoLine3D;

  MyVertexA  , MyVertexB, MyVertexC: TVertex;

  IPtArete     : TGeoPoint3D;
  function MakeArete(const V1, V2: TVertex): TGeoLine3D;
  begin
    Result[1].X := V1.X;  Result[1].Y := V1.Y; Result[1].z := V1.Z;
    Result[2].X := V2.X;  Result[2].Y := V2.Y; Result[2].z := V2.Z;
  end;
  procedure AppendIntersect(const IP: TGeoPoint3D);
  var
    QIntersectPt: TPoint3Df;
    n: Integer;
  begin
    AfficherMessageErreur('');
    AfficherMessageErreur('---- AppendIntersect()');
    n := Length(Intersects);
    SetLength(Intersects, n+1);
    QIntersectPt := MakeTPoint3Df(IP.x, IP.y, IP.z);
    Intersects[n-1] := QIntersectPt;
  end;
begin

  result := false;

    // Plan de profil, composé de deux triangles
    DemiPlanTriangle1[1].x := P1.X; DemiPlanTriangle1[1].Y := P1.y; DemiPlanTriangle1[1].z := 10000.00;//P2.Z;
    DemiPlanTriangle1[2].x := P1.X; DemiPlanTriangle1[2].Y := P1.y; DemiPlanTriangle1[2].z := -1000.00; //P1.Z;
    DemiPlanTriangle1[3].x := P2.X; DemiPlanTriangle1[3].Y := P2.y; DemiPlanTriangle1[3].z := -1000.00;

    DemiPlanTriangle2[1].x := P1.X; DemiPlanTriangle2[1].Y := P1.y; DemiPlanTriangle2[1].z := 10000.00;
    DemiPlanTriangle2[2].x := P2.X; DemiPlanTriangle2[2].Y := P2.y; DemiPlanTriangle2[2].z := -1000.00;
    DemiPlanTriangle2[3].x := P2.X; DemiPlanTriangle2[3].Y := P2.y; DemiPlanTriangle2[3].z := 10000.00;

    // sommets du triangle
    MyVertexA := GetVertex(Triangle.PointA);
    MyVertexB := GetVertex(Triangle.PointB);
    MyVertexC := GetVertex(Triangle.PointC);
    AfficherMessageErreur(Format('%d: %d: %f, %f, %f - %d: %f, %f, %f - %d: %f, %f, %f',
                                 [Triangle.Numero,
                                  Triangle.PointA, MyVertexA.X, MyVertexA.Y, MyVertexA.Z,
                                  Triangle.PointB, MyVertexB.X, MyVertexB.Y, MyVertexB.Z,
                                  Triangle.PointC, MyVertexC.X, MyVertexC.Y, MyVertexC.Z]));

    // arêtes
    AreteAB := MakeArete(MyVertexA, MyVertexB);
    AreteBC := MakeArete(MyVertexB, MyVertexC);
    AreteCA := MakeArete(MyVertexC, MyVertexA);
    AfficherMessageErreur(Format('AB: %f, %f, %f -> %f, %f, %f -- BC: %f, %f, %f -> %f, %f, %f -- CA: %f, %f, %f -> %f, %f, %f',
                                 [AreteAB[1].X, AreteAB[1].Y, AreteAB[1].Z, AreteAB[2].X, AreteAB[2].Y, AreteAB[2].Z,
                                  AreteBC[1].X, AreteBC[1].Y, AreteBC[1].Z, AreteBC[2].X, AreteBC[2].Y, AreteBC[2].Z,
                                  AreteCA[1].X, AreteCA[1].Y, AreteCA[1].Z, AreteCA[2].X, AreteCA[2].Y, AreteCA[2].Z
                                 ]));

    if (Intersect(AreteAB, DemiPlanTriangle1, IPtArete)) then AppendIntersect(IPtArete);
    if (Intersect(AreteBC, DemiPlanTriangle1, IPtArete)) then AppendIntersect(IPtArete);
    if (Intersect(AreteCA, DemiPlanTriangle1, IPtArete)) then AppendIntersect(IPtArete);

    if (Intersect(AreteAB, DemiPlanTriangle2, IPtArete)) then AppendIntersect(IPtArete);
    if (Intersect(AreteBC, DemiPlanTriangle2, IPtArete)) then AppendIntersect(IPtArete);
    if (Intersect(AreteCA, DemiPlanTriangle2, IPtArete)) then AppendIntersect(IPtArete);

  Result := (length(Intersects) > 0);
end;
//*)

function TMaillage.ExtractAndAddProfilTopo(const Pt1, Pt2: TPoint2Df; const QProfilLineAttr: TLineAttributes; const QProfilName: string = ''): boolean;
var
  Nb, i, k, NbVisees, v: Integer;
  TR: TMNTTriangleABC;
  QPt1, QPt2: TPoint3Df;
  EWE: String;
  wu: Boolean;
  IntersectPlanTriangle: TIntersectPlanTriangle;
  PP: TPoint3DfOrderedByP;
  MyProfilTopo: TProfilTopo;
  MyVisee: TBaseStation;
  PPP1: TPoint2Df;
begin
  result := false;
  QPt1.setFrom(Pt1.X, Pt1.Y, FCoordsMini.Z);
  QPt2.setFrom(Pt2.X, Pt2.Y, FCoordsMaxi.Z);
  EWE := Format('%s.ExtractProfilTopo(%.2f, %.2f, %.2f -> %.2f, %.2f, %.2f)', [ClassName, QPt1.X, QPt1.Y, QPt1.Z, QPt2.X, QPt2.Y, QPT2.Z]);
  AfficherMessage(EWE);
  AfficherMessageErreur(EWE);
  if (FBDDEntites = nil) then
  begin
    AfficherMessageErreur('*** Maillage indépendant - Fonctionnalité non supportée ***');
    Exit;
  end;
  MyProfilTopo := TProfilTopo.Create;
  try
    MyProfilTopo.Initialiser(Pt1, Pt2);
    Nb := self.GetNbTriangles();
    for i := 1 to Nb - 1 do
    begin
      try
        TR := GetTriangle(i);
        WU := IntersectPlanVerticalAndTriangle(TR, QPt1, QPt2, IntersectPlanTriangle);
        if (WU) then
        begin
          PP.P := Hypot(IntersectPlanTriangle.X1 - QPt1.X, IntersectPlanTriangle.Y1 - QPt1.Y);
          PP.X := IntersectPlanTriangle.X1;
          PP.Y := IntersectPlanTriangle.Y1;
          PP.Z := IntersectPlanTriangle.Z1;
          MyProfilTopo.AddPointProfilTN(PP);
          PP.P := Hypot(IntersectPlanTriangle.X2 - QPt1.X, IntersectPlanTriangle.Y2 - QPt1.Y);
          PP.X := IntersectPlanTriangle.X2;
          PP.Y := IntersectPlanTriangle.Y2;
          PP.Z := IntersectPlanTriangle.Z2;
          MyProfilTopo.AddPointProfilTN(PP);
        end;
      except
        pass;
      end;
    end;
    MyProfilTopo.SortProfilTerrainByAbscisses();
    MyProfilTopo.EpurageProfilTN();
    Nb := MyProfilTopo.GetNbPointsProfilTN();
    if (Nb > 0) then // extraction des transects: galeries recoupées
    begin
      MyProfilTopo.ProfilName  := IIF('' = QProfilName,
                                      Format('Profil_%d', [FListeProfilsTopo.GetNbElements() + 1]),
                                      QProfilName);
      MyProfilTopo.LineAttributes := QProfilLineAttr;
      NbVisees := FBDDEntites.GetNbEntitesVisees();
      for v := 0 to NbVisees - 1 do
      begin
        MyVisee := FBDDEntites.GetEntiteVisee(v);
        if (MyVisee.Entite_Serie > 0) AND (Intersect(MyVisee.PosExtr0.X, MyVisee.PosExtr0.Y,
                                                     MyVisee.PosStation.X, MyVisee.PosStation.Y,
                                                     Pt1.X, Pt1.Y, Pt2.X, Pt2.Y)) then
        begin
          PPP1 := MyProfilTopo.GetExtremite1();
          MyVisee.TagDouble := Hypot(MyVisee.PosStation.X - PPP1.X, MyVisee.PosStation.Y - PPP1.Y);
          MyProfilTopo.AddConduitRecoupe(MyVisee);
        end;
      end;
      AddProfilTopo(MyProfilTopo);
      Result := True;
    end;
  except
    FreeAndNil(MyProfilTopo);//MyProfilTopo.Free;
  end;
end;

procedure TMaillage.AddProfilTopo(const P: TProfilTopo);
begin
  P.SortConduitsRecoupesBySerieStation();
  FListeProfilsTopo.AddElement(P);
end;

function TMaillage.GetProfilTopo(const Idx: integer): TProfilTopo;
begin
  Result := FListeProfilsTopo.GetElement(Idx);
end;
procedure TMaillage.PutProfilTopo(const Idx: integer; const P: TProfilTopo);
begin
  FListeProfilsTopo.PutElement(Idx, P);
end;
procedure TMaillage.RemoveProfilTopo(const Idx: integer);
begin
  AfficherMessageErreur(Format('%s.RemoveProfilTopo()', [ClassName]));
  FListeProfilsTopo.RemoveProfil(Idx);
end;

procedure TMaillage.ClearProfilsTopo();
begin
  FListeProfilsTopo.ClearListe();
end;

function TMaillage.GetNbProfilsTopo(): integer;
begin
  result := FListeProfilsTopo.GetNbElements();
end;

procedure TMaillage.ListerProfilsTopo();
var
  Nb, i, NbPts, j: Integer;
  MyProfilTopo: TProfilTopo;
  Pt: TPoint3DfOrderedByP;
begin
  Nb := FListeProfilsTopo.GetNbElements();
  AfficherMessageErreur(Format('%s.ListerProfilsTopo(): %d profils', [ClassName, Nb]));
  if (Nb = 0) then Exit;
  for i := 0 to Nb -1 do
  begin
    MyProfilTopo := GetProfilTopo(i);
    NbPts := MyProfilTopo.GetNbPointsProfilTN();
    //AfficherMessageErreur(Format('Profil: %d - %s - %d points', [i, MyProfilTopo.ProfilName, NbPts]));
    if (NbPts > 0) then
    begin
      for j := 0 to NbPts - 1 do
      begin
        Pt := MyProfilTopo.GetPointProfilTN(j);
        AfficherMessageErreur(Format('  %d; %.2f, %.2f', [j, Pt.P, Pt.Z]));
      end;
    end;
  end;
end;

function TMaillage.ConstruireMaillageAPasRegulier(const NbMaillesX, NbMaillesY: integer): boolean;
var
  C1, C2: TPoint3Df;
begin
  result := false;
  AfficherMessageErreur(Format('%s.ConstruireMaillageAPasRegulier(): %d x %d', [ClassName, NbMaillesX, NbMaillesY]));
  C1 := self.GetCoordsMini();
  C2 := self.GetCoordsMaxi();

end;

procedure TMaillage.SaveProfilsToFile(const QFilename: string);
var
  fp: TextFile;
  i, Nb: Integer;
  EWE: String;
  PP: TProfilTopo;
  PT1, PT2: TPoint2Df;
  CGT: TGHTopoColor;
begin
  AfficherMessageErreur(Format('%s.SaveProfilsToFile(): %s', [ClassName, QFilename]));
  Nb := GetNbProfilsTopo();
  if (Nb = 0) then Exit;
  EWE := FORMAT_NB_INTEGER + FIELD_SEPARATOR_TAB +
         FORMAT_NB_INTEGER + FIELD_SEPARATOR_TAB + FORMAT_NB_INTEGER + FIELD_SEPARATOR_TAB + FORMAT_NB_INTEGER + FIELD_SEPARATOR_TAB + // couleur
         FORMAT_NB_INTEGER + FIELD_SEPARATOR_TAB + // opacité
         FORMAT_NB_INTEGER + FIELD_SEPARATOR_TAB + // largeur ligne en pixel
         FORMAT_NB_REAL_2_DEC + FIELD_SEPARATOR_TAB + // largeur ligne en mm
         FORMAT_NB_REAL_2_DEC + FIELD_SEPARATOR_TAB +
         FORMAT_NB_REAL_2_DEC + FIELD_SEPARATOR_TAB +           // extr 1
         FORMAT_NB_REAL_2_DEC + FIELD_SEPARATOR_TAB +
         FORMAT_NB_REAL_2_DEC + FIELD_SEPARATOR_TAB +           // extr 2
         FORMAT_STRING;
  AssignFile(fp, QFilename);
  try
    Rewrite(fp);
    for i := 0 to Nb - 1 do
    begin
      PP := GetProfilTopo(i);
      PT1 := PP.GetExtremite1();
      PT2 := PP.GetExtremite2();
      CGT := PP.LineAttributes.ColorRGBA;
      writeln(fp, Format(EWE, [i, CGT.Red, CGT.Green, CGT.Blue, CGT.Alpha,
                               PP.LineAttributes.LineWidthInPixels, PP.LineAttributes.LineWidthInMillimeters,
                               PT1.X, PT1.Y,
                               PT2.X, PT2.Y,
                               PP.ProfilName
                              ]));
    end;
  finally
    CloseFile(fp);
  end;
end;
// ajoute les profils
function TMaillage.LoadProfilsFromFile(const QFilename: string): boolean;
var
  fp: TextFile;
  WU, QNom: string;
  AR: TGHStringArray;
  PT1, PT2: TPoint2Df;
  PP: TProfilTopo;
  n: Integer;
  QProfilLineAttr: TLineAttributes;
begin
  result := false;
  AfficherMessageErreur(Format('%s.LoadProfilsFromFile(): %s', [ClassName, QFilename]));
  AssignFile(fp, QFilename);
  try
    Reset(fp);
    while (not eof(fp)) do
    begin
      ReadLn(fp, WU);
      AR := Split(WU, FIELD_SEPARATOR_TAB);
      //0   1   2    3   4  5   6     7          8            9            10         11
      //0	0	0	255	255 1 0.015 403080.31	3091003.68	403405.57	3090831.37	Profil_1

      QProfilLineAttr.SetAttributes(StrToIntDef(AR[1], 0),
                                    StrToIntDef(AR[2], 0),
                                    StrToIntDef(AR[3], 255),
                                    StrToIntDef(AR[4], 255),
                                    StrToIntDef(AR[5], 0) and 255,
                                    ConvertirEnNombreReel(AR[6], 0.015));
      PT1.setFrom(ConvertirEnNombreReel(AR[7] , 0.00),
                  ConvertirEnNombreReel(AR[8] , 0.00));
      PT2.setFrom(ConvertirEnNombreReel(AR[9] , 0.00),
                  ConvertirEnNombreReel(AR[10], 0.00));
      QNom := Trim(AR[11]);
      if (ExtractAndAddProfilTopo(PT1, PT2, QProfilLineAttr, QNom)) then
      begin
        (*
        n := GetNbProfilsTopo() - 1;
        PP := GetProfilTopo(n);
        PP.ProfilColor := QColor;
        PP.ProfilName  := QNom;
        //*)
      end;
    end; // while not eof(fp)
    result := True;
  finally
    CloseFile(fp);
  end;
end;

function TMaillage.Trianguler(const NbPointsATrianguler: integer = -1): boolean;
var
  MyTriangulationDelaunay: TTriangulationDelaunay;
  NbVertex, i, NbTriangles, NbTrianglesRejetes: Integer;
  MyVertex: TMNTVertex;
  QPoint  : TPoint3Df;
  MyTriangle, QTriangle: TMNTTriangleABC;
begin
  result := false;
  AfficherMessage(Format('%s.Trianguler()', [ClassName]));
  AfficherMessage(Format('-- %d vertex', [GetNbVertex()]));
  AfficherMessage(Format('Etendue: %.2f, %.2f, %.2f -> %.2f, %.2f, %.2f', [FCoordsMini.X, FCoordsMini.Y, FCoordsMini.Z, FCoordsMaxi.X, FCoordsMaxi.Y, FCoordsMaxi.Z]));
  NbVertex := IIF(NbPointsATrianguler = -1, GetNbVertex(), NbPointsATrianguler);
  if (NbVertex < 3) then Exit(false);
  MyTriangulationDelaunay := TTriangulationDelaunay.Create;
  try
    if (MyTriangulationDelaunay.Initialiser(FCoordsMini, FCoordsMaxi)) then
    begin
      AfficherMessageErreur('*** Init OK');
      NbTriangles := MyTriangulationDelaunay.GetNbTriangles();
      NbVertex    := MyTriangulationDelaunay.GetNbPoints();
      AfficherMessageErreur(format('Initialisation terminée: %d vertex, %d triangles', [NbVertex, NbTriangles]));
      for i := 0 to GetNbVertex() - 1 do
      begin
        MyVertex := GetVertex(i);
        AfficherMessageErreur(Format('%d: %f %f %f', [i, MyVertex.Position.X, MyVertex.Position.Y, MyVertex.Position.Z]));
        QPoint.setFrom(MyVertex.Position.X, MyVertex.Position.Y, MyVertex.Position.Z);
        MyTriangulationDelaunay.AddAPoint(QPoint, false);
      end;

      //MyTriangulationDelaunay.Mesh();
      //*)
      NbTriangles := MyTriangulationDelaunay.GetNbTriangles();
      NbVertex    := MyTriangulationDelaunay.GetNbPoints();
      AfficherMessage(format('Triangulation terminée %d vertex, %d triangles', [NbVertex, NbTriangles]));

      if (NbVertex > 0) then
      begin
        FTableVertex.ClearListe();
        for i := 0 to NbVertex-1 do
        begin
          QPoint := MyTriangulationDelaunay.GetPoint(i);
          MyVertex.ID := i;
          MyVertex.Position.setFrom(QPoint.X, QPoint.Y, QPoint.Z);
          MyVertex.Normale.Empty();
          self.AddVertex(MyVertex);
        end;
      end;

      if (NbTriangles > 0) then
      begin
        NbTrianglesRejetes := 0;
        FtableTriangles.ClearListe();
        for i := 0 to NbTriangles - 1 do
        begin
          QTriangle := MyTriangulationDelaunay.GetTriangle(i);
          // On jette les triangles invalides
          if (QualifierTriangle(QTriangle)) then self.AddTriangle(QTriangle)
                                            else NbTrianglesRejetes += 1;
        end;
      end;
      AfficherMessageErreur(format('Triangulation terminée %d vertex, %d triangles (%d rejetés)', [NbVertex, NbTriangles, NbTrianglesRejetes]));

    end;
    MyTriangulationDelaunay.Finaliser();
    result := True;
  finally
    FreeAndNil(MyTriangulationDelaunay);//MyTriangulationDelaunay.Free;
  end;
end;

function TMaillage.QualifierTriangle(var ATriangle: TMNTTriangleABC): boolean;
const
  MAX_COTE_TRIANGLE = 250.00;
var
  VAB, VAC, VN, UZ: TPoint3Df;
  PA, PB, PC: TMNTVertex;
  lAB, lBC, lCA: Double;
begin
   Result := false;
   if (not IsValidTriangle(ATriangle)) then Exit;
   UZ.setFrom(0, 0, 1);
   PA := GetVertex(ATriangle.PointA);
   PB := GetVertex(ATriangle.PointB);
   PC := GetVertex(ATriangle.PointC);
   lAB := Hypot3D(PA.Position.X - PB.Position.X, PA.Position.Y - PB.Position.Y, PA.Position.Z - PB.Position.Z);
   lBC := Hypot3D(PB.Position.X - PC.Position.X, PB.Position.Y - PC.Position.Y, PB.Position.Z - PC.Position.Z);
   lCA := Hypot3D(PC.Position.X - PA.Position.X, PC.Position.Y - PA.Position.Y, PC.Position.Z - PA.Position.Z);
   if (IsZero(lAB) OR  IsZero(lBC) OR IsZero(lCA)) then Exit;
   if ((lAB > MAX_COTE_TRIANGLE) OR  (lBC > MAX_COTE_TRIANGLE) OR (lCA > MAX_COTE_TRIANGLE)) then Exit;

   // orientation du triangle
   VAB.setFrom(PB.Position.X - PA.Position.X, PB.Position.Y - PA.Position.Y, PB.Position.Z - PA.Position.Z);
   VAC.setFrom(PC.Position.X - PA.Position.X, PC.Position.Y - PA.Position.Y, PC.Position.Z - PA.Position.Z);

   // produit vectoriel: normale du triangle
   VN.setFrom(VAB.Y * VAC.Z - VAB.Z * VAC.Y,
              VAB.Z * VAC.X - VAB.X * VAC.Z,
              VAB.X * VAC.Y - VAB.Y * VAC.X);
   // Si la composante Z du vecteur normal est négative, échanger B et C
   if (VN.Z < 0) then Swap(ATriangle.PointB, ATriangle.PointC);
   Result := True;
end;
// pour le mailleur externe
// aide du mailleur, disponible sur http://www.cs.cmu.edu/~quake/triangle.html
 (*
 triangle [-prq__a__uAcDjevngBPNEIOXzo_YS__iFlsCQVh] input_file
   -p  Triangulates a Planar Straight Line Graph (.poly file).
   -r  Refines a previously generated mesh.
   -q  Quality mesh generation.  A minimum angle may be specified.
   -a  Applies a maximum triangle area constraint.
   -u  Applies a user-defined triangle constraint.
   -A  Applies attributes to identify triangles in certain regions.
   -c  Encloses the convex hull with segments.
   -D  Conforming Delaunay:  all triangles are truly Delaunay.
   -j  Jettison unused vertices from output .node file.
   -e  Generates an edge list.
   -v  Generates a Voronoi diagram.
   -n  Generates a list of triangle neighbors.
   -g  Generates an .off file for Geomview.
   -B  Suppresses output of boundary information.
   -P  Suppresses output of .poly file.
   -N  Suppresses output of .node file.
   -E  Suppresses output of .ele file.
   -I  Suppresses mesh iteration numbers.
   -O  Ignores holes in .poly file.
   -X  Suppresses use of exact arithmetic.
   -z  Numbers all items starting from zero (rather than one).
   -o2 Generates second-order subparametric elements.
   -Y  Suppresses boundary segment splitting.
   -S  Specifies maximum number of added Steiner points.
   -i  Uses incremental method, rather than divide-and-conquer.
   -F  Uses Fortune's sweepline algorithm, rather than d-and-c.
   -l  Uses vertical cuts only, rather than alternating cuts.
   -s  Force segments into mesh by splitting (instead of using CDT).
   -C  Check consistency of final mesh.
   -Q  Quiet:  No terminal output except errors.
   -V  Verbose:  Detailed information on what I'm doing.
   -h  Help:  Detailed instructions for Triangle.
  //*)
function TMaillage.TriangulerAvecUnOutilExterne(const ProgramMailleurExterne: TStringDirectoryFilename): boolean;
const
  BASE_FILENAME = '~Maillage001';
var
  VertexInputFilePourMailleur: TStringDirectoryFilename;
begin
  AfficherMessage(Format('%s.TriangulerAvecUnOutilExterne: %s', [ClassName, ProgramMailleurExterne]));
  AfficherMessageErreur(Format('%s.TriangulerAvecUnOutilExterne: %s', [ClassName, ProgramMailleurExterne]));
  result := false;
  AfficherMessageErreur('--> 00: Test du mailleur ' + ProgramMailleurExterne);
  if (not FileExistsUTF8(ProgramMailleurExterne)) then
  begin
    AfficherMessage('*** Mailleur introuvable - Arrêt');
    exit;
  end;
  VertexInputFilePourMailleur := GetGHTopoDirectory() + BASE_FILENAME + '.node';
  AfficherMessageErreur('--> 01: Génération du fichier pour le mailleur: ' + VertexInputFilePourMailleur);

  if (not GenererFichierPourMailleurExterne(VertexInputFilePourMailleur)) then
  begin
    AfficherMessage('*** Echec en génération du fichier pour le mailleur');
    exit;
  end;
  AfficherMessageErreur('--> 02: Calcul par le mailleur: ' + VertexInputFilePourMailleur);
  AfficherMessageErreur(VertexInputFilePourMailleur);
  if (0 <> RunExternalProgram(ProgramMailleurExterne, '', VertexInputFilePourMailleur)) then
  begin
    AfficherMessage('*** Erreur du mailleur');
    exit;
  end;

  AfficherMessageErreur('--> 03: Récupération des résultats');
  ChargerResultatsDuMailleur(GetGHTopoDirectory() + BASE_FILENAME);

end;
// générer le fichier .node = Séparateur: ESPACE
function TMaillage.GenererFichierPourMailleurExterne(const QFilename: TStringDirectoryFilename): boolean;
var
  fp: TextFile;
  QNbVertex, i: Integer;
  MyVertex: TMNTVertex;
begin
  result := false;
  QNbVertex := GetNbVertex();
  AfficherMessage(Format('%s.GenererFichierPourMailleurExterne: %d vertex ->%s', [ClassName, QNbVertex, QFilename]));

  if (QNbVertex = 0) then Exit;
  AssignFile(fp, QFilename);
  try
    ReWrite(fp);
    WriteLn(fp, Format('%d %d %d %d', [QNbVertex, 2, 1, 1]));
    for i := 0 to QNbVertex - 1 do
    begin
      MyVertex := GetVertex(i);
      //WriteLn(fp, Format('%d %.2f %.2f %.2f %d', [i, MyVertex.Position.X, MyVertex.Position.Y, MyVertex.Position.Z, 1]));
      writeln(fp, MyVertex.MakeLigneForMailleur(i));
    end;
    Result := True;
  finally
    CloseFile(fp);
  end;
end;

function TMaillage.ElimineVertexOutOfPolygone(const P: TGeoPolygon2D): boolean;
var
  MyVertex: TMNTVertex;
  i: Integer;
begin
  AfficherMessage(Format('%s.ElimineVertexOutOfPolygone: %d vertex', [ClassName, GetNbVertex()]));
  result := false;
  if (self.GetNbVertex() = 0) then exit(false);
  for i := self.GetNbVertex() downto 0 do
  begin
    MyVertex := GetVertex(i);
    if (not PointInPolygon(MyVertex.Position.X, MyVertex.Position.Y, P)) then FTableVertex.RemoveElement(i);
  end;
  Result := (GetNbVertex() >= 3);

end;

procedure TMaillage.DisableDisplayOutOfBoundingBox(const BB: TMNTBoundingBox);
var
  i, NbVertex, NbTriangles: Integer;
  MyVertex: TMNTVertex;
  MyTriangle: TMNTTriangleABC;
  function QVertexInBoundingBox(const V: TMNTVertex): boolean;
  begin
    Result := IsInRange(V.Position.X, BB.C1.X, BB.C2.X) AND
              IsInRange(V.Position.Y, BB.C1.Y, BB.C2.Y) AND
              IsInRange(V.Position.Z, BB.C1.Z, BB.C2.Z);
  end;
  function QTriangleInBoundingBox(const T: TMNTTriangleABC): boolean;
  var
    PA, PB, PC: TMNTVertex;
  begin
    PA := GetVertex(T.PointA);
    PB := GetVertex(T.PointB);
    PC := GetVertex(T.PointC);
    Result := QVertexInBoundingBox(PA) AND
              QVertexInBoundingBox(PB) AND
              QVertexInBoundingBox(PC);
  end;
begin
  AfficherMessage(Format('%s.DisableDisplayOutOfBoundingBox(%.2f, %.2f, %.2f -> %.2f, %.2f, %.2f',
                        [ClassName, BB.C1.X, BB.C1.Y, BB.C1.Z,   BB.C2.X, BB.C2.Y, BB.C2.Z]));
  NbVertex    := GetNbVertex();
  if (0 = NbVertex) then Exit;
  NbTriangles := GetNbTriangles();
  if (0 = NbTriangles) then Exit;
  for i := 0 to NbVertex - 1 do
  begin
    MyVertex := GetVertex(I);
    MyVertex.Displayed := QVertexInBoundingBox(MyVertex);
    PutVertex(i, MyVertex);
  end;
  for i := 0 to NbTriangles  -1 do
  begin
    MyTriangle := GetTriangle(I);
    MyTriangle.Displayed := QTriangleInBoundingBox(MyTriangle);
    PutTriangle(i, MyTriangle);
  end;
end;

procedure TMaillage.ResetVisibilityOfObjects(const B: boolean);
var
  NbVertex, NbTriangles, i: Integer;
  MyTriangle: TMNTTriangleABC;
  MyVertex: TMNTVertex;
begin
  AfficherMessage(Format('%s.ResetVisibilityOfObjects(%sVISIBLE)', [ClassName, booltostr(B, '', 'IN')]));
  NbVertex    := GetNbVertex();
  if (0 = NbVertex) then Exit;
  NbTriangles := GetNbTriangles();
  if (0 = NbTriangles) then Exit;
  for i := 0 to NbVertex do
  begin
    MyVertex := GetVertex(I);
    MyVertex.Displayed := B;
    PutVertex(i, MyVertex);
  end;
  for i := 0 to NbTriangles do
  begin
    MyTriangle := GetTriangle(I);
    MyTriangle.Displayed := B;
    PutTriangle(i, MyTriangle);
  end;
end;

procedure TMaillage.ReinitBoundinxBoxWithoutData();
begin
  FCoordsMini.setFrom( INFINI,  INFINI,  INFINI);
  FCoordsMaxi.setFrom(-INFINI, -INFINI, -INFINI);
end;

function TMaillage.ChargerResultatsDuMailleur(const BaseFileName: TStringDirectoryFilename): boolean;
var
  fp: TextFile;
  MyLigne: string;
  OutputMailleur_ele, OutputMailleur_node: TStringDirectoryFilename;
  Values: TGHStringArray;
  MyTriangle: TMNTTriangleABC;
  NbTrs: LongInt;
  i: Integer;
begin
  AfficherMessage(Format('%s.ChargerResultatsDuMailleur: BaseName: %s', [ClassName, BaseFileName]));
  result := false;
  OutputMailleur_ele   := BaseFileName + '.1.ele';
  OutputMailleur_node   := BaseFileName + '.1.node';

  AfficherMessageErreur(Format('%s.ChargerResultatsDuMailleur: BaseName: %s', [ClassName, BaseFileName]));
  AfficherMessage('-- Fichiers générés par le mailleur');

  AfficherMessageErreur(OutputMailleur_ele);
  AfficherMessageErreur(OutputMailleur_node);
  FTableTriangles.ClearListe();
  AfficherMessage(Format('--> Lecture triangles depuis %s',[OutputMailleur_ele]));
  AssignFile(fp, OutputMailleur_ele);
  try
    Reset(fp);
    // 1ère ligne
    ReadLn(fp, MyLigne);
    Values     := Split(SupprimeEspacesMultiples(MyLigne), ' ');
    NbTrs      := StrToIntDef(Values[0],0);
    if (NbTrs = 0) then Exit;
    for i:=1 to NbTrs do
    begin
      try
        ReadLn(fp, MyLigne);
        Values     := Split(SupprimeEspacesMultiples(MyLigne), ' ');
        MyTriangle.Numero      := StrToIntDef(Values[0], -1);
        MyTriangle.PointA      := StrToIntDef(Values[1], -1);
        MyTriangle.PointB      := StrToIntDef(Values[2], -1);
        MyTriangle.PointC      := StrToIntDef(Values[3], -1);
        if (IsValidTriangle(MyTriangle)) then AddTriangle(MyTriangle);
      except
        pass;
      end;
    end;
    AfficherMessage(Format('%d triangles lus', [GetNbTriangles()]));
  finally
    CloseFile(fp);
  end;

end;

end.


/ =============================================================================
// Triangulation du réseau de points
// Utilise le programme Triangle.exe
procedure TTriangulation.Trianguler(var MyApp: TApplication);
const
  TRIANGULATOR_EXE_NAME    = 'triangulator.exe';
  INPUT_TRIANGULATOR_FILE  = 'o1.node';
  OUTPUT_TRIANGULATOR_FILE = 'o1.1.ele';

var
  TriangulatorExeName: string;
  Ligne              : string;
  F1: string;
  i, NbTrs , QNbVertex: integer;
  V : TPoint3Df;
  Tr: TTriangle;
  pF: TextFile;
  Values: TStringArray;
  PerMax : Double;
begin
  FCanDrawCalibration:=False;
  PerMax := 400.00; //* périmètre des triangles dégénérés
  AfficherMessage(Format('%s.Trianguler',[ClassName]));
  QNbVertex := GetNbVertex();
  if (QNbVertex = 0) then
  begin
    AfficherMessage('--> Pas de sommets valables - Arrêt');
    Exit;
  end;


  // calculer le maillage
  //F1 := ExtractFilePath(ParamStr(0)) + OUTPUT_TRIANGULATOR_FILE;

  F1 := Format('%s %s', [ExtractFilePath(ParamStr(0)) + TRIANGULATOR_EXE_NAME, F1]);
  AfficherMessage('--> Calcul du maillage');
  //AfficherMessage('--> '+F1);
  TriangulatorExeName := ExtractFilePath(ParamStr(0)) + TRIANGULATOR_EXE_NAME;
  RunExtrnPrgm(Application, TriangulatorExeName, F1);

  AfficherMessage('--> Maillage calculé ');
  // récupérer le résultat et le charger
  F1:= ExtractFilePath(ParamStr(0)) + OUTPUT_TRIANGULATOR_FILE;
  AfficherMessage(Format('--> Lecture triangles depuis %s',[F1]));
  AssignFile(pF, F1);
  try
    Reset(pF);
    // 1ère ligne
    ReadLn(pF, Ligne);
    Values     := Split(SupprimeEspacesMultiples(Ligne), ' ');
    NbTrs      := StrToIntDef(Values[0],0);
    if (NbTrs = 0) then Exit;
    for i:=1 to NbTrs do
    begin
      try
        ReadLn(pF, Ligne);

        Values     := Split(SupprimeEspacesMultiples(Ligne), ' ');
        Tr.ID      := StrToIntDef(Values[0],0);
        Tr.VertexA := StrToIntDef(Values[1],0);
        Tr.VertexB := StrToIntDef(Values[2],0);
        Tr.VertexC := StrToIntDef(Values[3],0);
        AddTriangle(Tr);
        (*
        AfficherMessage(Format('Triangle %d: %d %d %d',
                               [Tr.ID,
                                Tr.VertexA,
                                Tr.VertexB,
                                Tr.VertexC ]));


        //*)

      except
        ;
      end;
    end;

    // purge des triangles dégénérés
    AfficherMessage(Format('--> Purge des triangles dégénérés (p > %.2f m)',[PerMax]));
    RemoveDegeneratedTriangles(PerMax);
    FCanDrawCalibration := True;
  finally
    CloseFile(pF);
  end;
end;

