unit DGCPrimitivesComplexes;
// Définition des classes pour les primitives complexes de dessin
// 31/10/2019: Courbes de Bézier OK

{$mode delphiunicode}

interface

uses
  DGCTypes,
  DGCAncestorPrimitives,
  DGCUtilityFunctions,
  Classes, SysUtils, Math;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+ Le polygone et la polyligne                                                +
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
type TDGCPolyLineGon = class(TDGCPrimitive)
 private
   //FIdxStyleSheet: integer;
   FBoundingBox : TDGCBoundingBox;
   FName: string;
   FDoFlush: boolean;
   FListeVertex: TDGCArrayPoints2D; // array of TDGCPoint2D;

 public
   constructor Create(const QIdxStyleSheet: integer; const QName: string; const QDoFlush: boolean);
   destructor  Destroy();
   //property IdxStyleSheet: integer read FIdxStyleSheet;
   property Name    : string read FName write FName;
   property DoFlush : boolean read FDoFlush write FDoFlush;
   function AddVertex(const V: TDGCPoint2D): boolean;
   function GetVertex(const Idx: integer): TDGCPoint2D;
   function GetNbVertex(): integer;
   procedure SetBoundingBox(); override;
   function GetBoundingBox(): TDGCBoundingBox;

end;
type TDGCPolyline = class(TDGCPolyLineGon)
end;

type TDGCPolygon = class(TDGCPolyLineGon)
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+ La courbe de Bézier                                                        +
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


type

{ TDGCCurve }
 // courbe lissée (spline ou Bezier)
 TDGCCurve = class(TDGCPrimitive)
  private
    FTypeCurve  : TTypeCurve;
    FName       : string;
    FFilled     : boolean;
    FDoFlush    : boolean;
    FListeVertex: TDGCArrayPoints2D; // array of TDGCPoint2D;
    FListeArcs  : TDGCListBezierArcs;
    procedure AddBezierArc(const BA: TDGCBezierArc);
  public
    constructor Create(const QIdxStyleSheet: integer; const QName: string; const QDoFlush: boolean; const QTypeCurve: TTypeCurve; const QFilled: boolean);
    destructor Destroy();
    //property IdxStyleSheet: integer read FIdxStyleSheet;
    property TypeCurve : TTypeCurve read FTypeCurve write FTypeCurve;
    //property Name      : string  read FName write FName;
    property DoFlush   : boolean read FDoFlush write FDoFlush;
    property Filled    : boolean read FFilled write FFilled;
    function AddVertex(const V: TDGCPoint2D): boolean;
    function GetVertex(const Idx: integer): TDGCPoint2D;
    function GetNbVertex(): integer;
    function GetNbArcs(): integer;
    function GetBezierArc(const Idx: integer): TDGCBezierArc;
    procedure SetBoundingBox(); override;
    function MakeBezierCurve(const ByDefault: boolean): boolean;
end;

implementation
uses
  DGCDummyUnit; // pour éviter le bug de 'Fin de code source non trouvée
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+ Le polygone et la polyligne                                                +
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor TDGCPolyLineGon.Create(const QIdxStyleSheet: integer; const QName: string; const QDoFlush: boolean);
begin
  inherited Create(QIdxStyleSheet, QName);
  //FIdxStyleSheet := QIdxStyleSheet;
  FName    := QName;
  FDoFlush := QDoFlush;
  FListeVertex.Empty(); // SetLength(FListeVertex, 0);
  FBoundingBox.Reset();
end;

destructor TDGCPolyLineGon.Destroy();
begin
  FListeVertex.Empty();// SetLength(FListeVertex, 0);
  inherited Destroy;
end;

procedure TDGCPolyLineGon.SetBoundingBox();
var
  i: Integer;
  P: TDGCPoint2D;
begin
  FBoundingBox.Reset();

  //UpdateBoundingBox(FBoundingBox, GetVertex(0), True);
  for i := 0 to GetNbVertex() - 1 do FBoundingBox.upDate(GetVertex(i));
end;

function TDGCPolyLineGon.AddVertex(const V: TDGCPoint2D): boolean;
var
  n: Integer;
begin
  result := false;
  FListeVertex.AddElement(V);
  (*
  n := Length(FListeVertex);
  SetLength(FListeVertex, n + 1);
  FListeVertex[n] := V;//*)

end;

function TDGCPolyLineGon.GetVertex(const Idx: integer): TDGCPoint2D;
begin
  Result := FListeVertex.GetElement(Idx);//[Idx];
end;

function TDGCPolyLineGon.GetNbVertex(): integer;
begin
  Result := FListeVertex.GetNbElements();//length(FListeVertex);
end;

function TDGCPolyLineGon.GetBoundingBox(): TDGCBoundingBox;
begin
  result := self.FBoundingBox;
end;

procedure TDGCCurve.AddBezierArc(const BA: TDGCBezierArc);
begin
  FListeArcs.AddElement(BA);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+ La courbe de Bézier                                                        +
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor TDGCCurve.Create(const QIdxStyleSheet: integer; const QName: string; const QDoFlush: boolean; const QTypeCurve: TTypeCurve; const QFilled: boolean);
begin
  inherited Create(QIdxStyleSheet, QName);
  FFilled   := QFilled;
  FDoFlush  := QDoFlush;
  FListeVertex.Empty();
  FListeArcs.Empty();
end;

destructor TDGCCurve.Destroy();
begin
  FListeVertex.Empty();
  FListeArcs.Empty();
end;

function TDGCCurve.AddVertex(const V: TDGCPoint2D): boolean;
var
  n: Integer;
begin
  result := false;
  self.FListeVertex.AddElement(V);
  result := (FListeVertex.GetNbElements() > 0); //length(FListeVertex) > 0;
end;

function TDGCCurve.GetVertex(const Idx: integer): TDGCPoint2D;
begin
  result := FListeVertex.GetElement(Idx); //[Idx];
end;

function TDGCCurve.GetNbVertex(): integer;
begin
  Result := FListeVertex.GetNbElements();// length(FListeVertex);
end;

function TDGCCurve.GetNbArcs(): integer;
begin
  Result := FListeArcs.GetNbElements();
end;

function TDGCCurve.GetBezierArc(const Idx: integer): TDGCBezierArc;
begin
  Result := FListeArcs.GetElement(Idx);
end;


// MakeBezierCurve(): Construit une courbe de Bézier
// ByDefault: Mode de lissage: Toujours à True - Non utilisé ici
{$NOTE %LINE% : ByDefault parameter unused, reserved for future use}
function TDGCCurve.MakeBezierCurve(const ByDefault: boolean): boolean;
const
  PI_2: double = PI / 2.0;
var
  i, QNbPts: Integer;

  AB       : TDGCBezierArc;
  A0, A1   : TDGCBezierArc;
  V1, V2   : TDGCPoint2D;
  Delta    : TDGCPoint2D;
  R0, R1   : double;
  AngleTg1, AngleTg2 : double;
begin
  Result := false;
  FListeArcs.Empty();
  QNbPts := self.GetNbVertex();
  if (QNbPts < 2) then exit;
  Delta.Empty();
  FListeArcs.SetCapacity(QNbPts - 1);
  //SetLength(FListeArcs, QNbPts - 1);
  // TODO: C'est dans ce secteur qu'il faut gérer le mode ByDefault
  for i := 1 to QNbPts-1 do
  begin
    V1 := self.GetVertex(i-1);
    V2 := self.GetVertex(i);
    // arc de courbe
    AB.PT1 := V1;
    AB.PT2 := V2;

    Delta.setFrom(V2.X - V1.X, V2.Y - V1.Y);
    AB.TangP1.setFrom(Delta.X / 3, Delta.Y / 3);
    R1 := Delta.getNorme() / 3;

    AB.TangP2.X := -AB.TangP1.X;
    AB.TangP2.Y := -AB.TangP1.Y;

    FListeArcs.SetElement(i-1, AB);//[i-1] := AB;
  end;
  // Passe 2: Calcul des tangentes (construction par défaut)
  for i := 1 to FListeArcs.GetNbElements() - 1 do //High(FListeArcs) do
  begin
    A0 := FListeArcs.GetElement(i-1); //[i-1];
    A1 := FListeArcs.GetElement(i); //[i];
    (*
    R1 := Hypot(A0.TangP2.X, A0.TangP2.Y);
    AngleTg2 := DGCGetAngleBissecteur(-A0.TangP2.X, -A0.TangP2.Y,
                                       A1.TangP1.X,  A1.TangP1.Y) - PI_2;
    FListeArcs[i-1].TangP2.X := R1 * cos(AngleTg2);
    FListeArcs[i-1].TangP2.Y := R1 * sin(AngleTg2);

    R2 := Hypot(A1.TangP1.X, A1.TangP1.Y);

    AngleTg1 := AngleTg2 + PI;
    FListeArcs[i].TangP1.X := R2 * cos(AngleTg1);
    FListeArcs[i].TangP1.Y := R2 * sin(AngleTg1);
    //*)
    R0 := A0.TangP2.getNorme();
    R1 := A1.TangP1.getNorme();
    AngleTg2 := DGCGetAngleBissecteur(-A0.TangP2.X, -A0.TangP2.Y,
                                       A1.TangP1.X,  A1.TangP1.Y) - PI_2;

    FListeArcs.M[i-1].TangP2.setFrom( cos(AngleTg2) * R0,  sin(AngleTg2) * R0);
    //AngleTg1 := AngleTg2 + PI;
    //FListeArcs[i].TangP1.setFrom(R2 * cos(AngleTg1), R2 * sin(AngleTg1);
    FListeArcs.M[i-1].TangP1.setFrom(-cos(AngleTg2) * R1, -sin(AngleTg2) * R1);   // cos(x + pi) = -cos(x) et sin(x + pi) = -sin(x)

  end;
  // Passe 3: Coordonnées des points de contrôle
  (*
  for i := 0 to High(FListeArcs) do
  begin
    AB := FListeArcs[i];
    AB.PC1 := MakeTDGCPoint2D(AB.PT1.X + AB.TangP1.X,
                              AB.PT1.Y + AB.TangP1.Y);
    AB.PC2 := MakeTDGCPoint2D(AB.PT2.X + AB.TangP2.X,
                              AB.PT2.Y + AB.TangP2.Y);
    FListeArcs[i] := AB;
  end;
  //*)
  // TODO: Calculer la boundingbox
  //CResult := FDocDessin.CalcBoundingBoxCourbe(CResult);
  //******************************************************
  Result := true;
end;

procedure  TDGCCurve.SetBoundingBox();
var
  i, Nb: integer;
  procedure QBoundingBoxArc(const A: TDGCBezierArc);
  var
    s: integer;
    Bezier: TDGCArrayPoints2D;
    P0, P1, P2, P3, PT, BBS: TDGCPoint2D;
    ErrCode: integer;
  begin
    P0 := A.PT1;
    P1.setFrom(A.PT1.X + A.TangP1.X, A.PT1.Y + A.TangP1.Y);
    P2.setFrom(A.PT2.X + A.TangP2.X, A.PT2.Y + A.TangP2.Y);
    P3 := A.PT2;

    DGCCalcBezierCurve(P0, P1, P2, P3, 20, Bezier);
    for s := 0 to Bezier.GetNbElements() - 1 do //; High(Bezier) do
    begin
      //BBS := Bezier.GetElement(s);
      //PT.setFrom(Bezier[s].X, Bezier[s].Y);
      FBoundingBox.upDate(Bezier.GetElement(s)); //PT);
    end;
  end;
begin
  Nb := FListeArcs.GetNbElements();//Length(FListeArcs);
  FBoundingBox.Reset();
  for i := 0 to Nb - 1  do QBoundingBoxArc(FListeArcs.GetElement(i));
end;
end.
//******************************************************************************
// FIN DU CODE DE L'UNITE
//******************************************************************************

