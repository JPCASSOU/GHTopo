//Credit to Paul Bourke (pbourke@swin.edu.au) for the original Fortran 77 Program :))
//Conversion to Visual Basic by EluZioN (EluZioN@casesladder.com)
//Conversion from VB to Delphi6 by Dr Steve Evans (steve@lociuk.com)
///////////////////////////////////////////////////////////////////////////////
//June 2002 Update by Dr Steve Evans (steve@lociuk.com): Heap memory allocation
//added to prevent stack overflow when MaxVertices and MaxTriangles are very large.
//Additional Updates in June 2002:
//Bug in InCircle function fixed. Radius r := Sqrt(rsqr).
//Check for duplicate points added when inserting new point.
//For speed, all points pre-sorted in x direction using quicksort algorithm and
//triangles flagged when no longer needed. The circumcircle centre and radius of
//the triangles are now stored to improve calculation time.
///////////////////////////////////////////////////////////////////////////////
//You can use this code however you like providing the above credits remain in tact
//注意：点和三角形都数组都是从1开始计数的
{$mode delphi}{$H+}

unit UnitTriangulationDelaunay2;

interface

uses
  StructuresDonnees,
  Common,
  SysUtils, classes, Graphics,

  UnitListesSimplesWithGeneriques,
  Types, math;

//Set these as applicable
Const
  MaxVertices = 500000;
  MaxTriangles = 1000000;
  ExPtTolerance = 0.000001;  //小于这个被认为是同一点



Type
  TCastArray = Array [0..2,0..2,0..2] of Integer;
  TVectorL3D = Array [0..2] of Double;
  TVectorL3I = Array [0..2] of Integer;

  PPointPair = ^TPointPair;
  TPointPair = record
    x1,y1,
    x2,y2: Double
  end;
  //单条等值线
  TLevel = record
    FZ: Double;
    Points: TList;
  end;



  //Created Triangles, vv# are the vertex pointers(点的索引)
  dTriangle = record
    vv0: LongInt;
    vv1: LongInt;
    vv2: LongInt;
    PreCalc: Integer;
    xc,yc,r: Double;   //三角形外接圆圆心坐标和半径
  end;

  TDVertexs = array[0..MaxVertices] of TPoint3Df;
  PVertexs = ^TDVertexs;

  TDTriangles = array[0..MaxTriangles] of dTriangle;
  PTriangles = ^TDTriangles;

  TDCompletes = array [0..MaxTriangles] of Boolean;
  PCompletes = ^TDCompletes;

  TDEdges = array[0..2,0..MaxTriangles * 3] of LongInt;
  PEdges = ^TDEdges;

  { TDelaunay }

type

  { TTriangulationDelaunay }

  TTriangulationDelaunay = class
  private
    { Private declarations }
    FzLow,
    FzHigh: Double;

    FVertexs: PVertexs;
    FTriangles: PTriangles;
    FTriangleCount: Integer;
    FPointCount: Integer;  //Variable for total number of points (vertices)

    //FListeVertex: TListeVertex;

    FLimitesDuMNT    : TRect2Df;
    FLevels: Array of TLevel;
    procedure QuickSort(var aVertexs: PVertexs; Low,High: Integer);
    function InCircle(xp, yp, x1, y1, x2, y2, x3, y3: Double;
             var xc: Double; var yc: Double; var r: Double; j: Integer): Boolean;


    Function WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
    Function Triangulate(nVert: Integer): Integer;
    procedure ClearVertex();
    //procedure ClearTriangles();

    //procedure ScatterContour(ZCount: Integer; Z: Array of double);

    procedure AddPoint(const x,y,z: double; const DoMesh: boolean);
  public
    { Public declarations }

    constructor Create;
    destructor Destroy; override;
    function   Initialiser(const QCoinBasGauche, QCoinHautDroit: TPoint3Df): boolean;
    procedure  Finaliser();
    procedure  ResetListes();

    function   GetLimitesDuMNT(): TRect2Df;
    function   GetNbPoints(): integer;
    function   GetNbTriangles(): integer;

    function   GetPoint(const Idx: integer): TPoint3Df;
    procedure  AddAPoint(const P: TPoint3Df; const DoMesh: boolean);

    function   GetTriangle(const Idx: integer): TMNTTriangleABC;
    procedure  Mesh();
  end;

implementation


function SortEntiteByX(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TPoint3Df;
begin
  E1 := Item1;
  E2 := Item2;
  if      (E1^.X < E2^.X) then Result := -1
  else if (E1^.X = E2^.X) then Result :=  0
  else                         Result :=  1;
end;

constructor TTriangulationDelaunay.Create;
begin
  //Initiate total points to 1, using base 0 causes problems in the functions
  FPointCount    := 1;
  FTriangleCount := 0;
  FzLow          := 0;
  FzHigh         := 0;
  //Allocate memory for arrays
  GetMem(FVertexs, sizeof(FVertexs^));
  GetMem(FTriangles, sizeof(FTriangles^));
end;

destructor TTriangulationDelaunay.Destroy;
begin
  try
    //Free memory for arrays
    FreeMem(FVertexs, sizeof(FVertexs^));
    FreeMem(FTriangles, sizeof(FTriangles^));
    //FListeVertex.ClearListe();
  finally
    //FListeVertex.Free;
  end;
end;
procedure TTriangulationDelaunay.ResetListes();
begin
  ClearVertex();
end;

function TTriangulationDelaunay.GetLimitesDuMNT(): TRect2Df;
begin
  result := FLimitesDuMNT;
end;

function TTriangulationDelaunay.GetNbPoints(): integer;
begin
  Result := FPointCount;
end;

function TTriangulationDelaunay.GetNbTriangles(): integer;
begin
  Result := FTriangleCount;
end;

function TTriangulationDelaunay.GetPoint(const Idx: integer): TPoint3Df;
begin
  Result := FVertexs^[Idx];
end;

procedure TTriangulationDelaunay.AddAPoint(const P: TPoint3Df; const DoMesh: boolean);
begin
  self.AddPoint(P.X, P.Y, P.Z, DoMesh);
end;

function TTriangulationDelaunay.GetTriangle(const Idx: integer): TMNTTriangleABC;
var
  EWE: dTriangle;
begin
  EWE := self.FTriangles^[Idx];
  Result.Numero := Idx;
  Result.PointA := EWE.vv0;
  Result.PointB := EWE.vv1;
  Result.PointC := EWE.vv2;
end;

function TTriangulationDelaunay.Initialiser(const QCoinBasGauche, QCoinHautDroit: TPoint3Df): boolean;
begin
  Result := false;
  ClearVertex();

  AfficherMessageErreur(Format('%s.Initialiser:', [classname]));
  try
    self.ResetListes();
    FLimitesDuMNT.X1 := QCoinBasGauche.X;
    FLimitesDuMNT.Y1 := QCoinBasGauche.Y;
    FLimitesDuMNT.X2 := QCoinHautDroit.X;
    FLimitesDuMNT.Y2 := QCoinHautDroit.Y;
    //AddPoint(QCoinBasGauche.X, QCoinBasGauche.Y, QCoinBasGauche.Z, false);
    //AddPoint(QCoinHautDroit.X, QCoinHautDroit.Y, QCoinBasGauche.Z, false);
    //Mesh();

    AfficherMessageErreur(Format('-- (%f, %f) -> (%f, %f)', [FLimitesDuMNT.X1, FLimitesDuMNT.Y1, FLimitesDuMNT.X2, FLimitesDuMNT.Y2]));
    result := True;
  except

  end;
end;
procedure TTriangulationDelaunay.Finaliser();
begin
  pass;
end;



// Ajoute un point
procedure TTriangulationDelaunay.AddPoint(const x,y,z: double; const DoMesh: boolean);
var
  i: Integer;
  SamePoint: Boolean;
begin
  AfficherMessageErreur(Format('%s.AddPoint: %.2f %.2f %.2f (%d pts)', [classname, x, y, z, FPointCount]));
  //Check for duplicate points
  SamePoint := false;
  i := 1;
  while i < FPointCount do
  begin
    If (IsZero(x - FVertexs^[i].X) and IsZero(y - FVertexs^[i].Y)) Then SamePoint:= true;
    Inc(i);
  end;

  if FzLow > z then
    FzLow:= z
  else if FzHigh < z then
    FzHigh:= z;

  if not SamePoint  then
  begin
    //Set Vertex coordinates
    FVertexs^[FPointCount].X := x;
    FVertexs^[FPointCount].Y := y;
    FVertexs^[FPointCount].Z := z;
    //Increment the total number of points
    FPointCount := FPointCount + 1;
    if (DoMesh) then self.Mesh();
  end;
end;



procedure TTriangulationDelaunay.Mesh();
begin
  afficherMessage('Mesh()');
  QuickSort(FVertexs,1,FPointCount-1);
  If (FPointCount > 3) Then
  FTriangleCount := Triangulate(FPointCount-1); //-1); //'Returns number of triangles created.
end;




procedure TTriangulationDelaunay.QuickSort(var aVertexs: PVertexs; Low,High: Integer);
//Sort all points by x
  procedure DoQuickSort(var aVertexs: PVertexs; iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid   : Double;
    T     : TPoint3Df;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := aVertexs^[(Lo + Hi) div 2].X;
    repeat
      while aVertexs^[Lo].x < Mid do Inc(Lo);
      while aVertexs^[Hi].x > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := aVertexs^[Lo];
        aVertexs^[Lo] := aVertexs^[Hi];
        aVertexs^[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then DoQuickSort(aVertexs, iLo, Hi);
    if Lo < iHi then DoQuickSort(aVertexs, Lo, iHi);
  end;
begin
  DoQuickSort(aVertexs, Low, High);
end;



//真正构建三角网(nVert：点的个数)
function TTriangulationDelaunay.Triangulate(nVert: Integer): Integer;
//Takes as input NVERT vertices in arrays Vertex()
//Returned is a list of NTRI triangular faces in the array
//Triangle(). These triangles are arranged in clockwise order.
var
  Completes: PCompletes;
  Edges: PEdges;
  Nedge: LongInt;

  //For Super Triangle  一个包括所有点的外包三角形
  xmin: Double;
  xmax: Double;
  ymin: Double;
  ymax: Double;
  xmid: Double;
  ymid: Double;
  dx: Double;
  dy: Double;
  dmax: Double;

  //General Variables
  i : Integer;
  j : Integer;
  k : Integer;
  ntri : Integer;
  xc : Double;
  yc : Double;
  r : Double;
  inc : Boolean;  //是否在外接圆中
begin
  //Allocate memory
  GetMem(Completes, sizeof(Completes^));
  GetMem(Edges, sizeof(Edges^));



  //Find the maximum and minimum vertex bounds.
  //This is to allow calculation of the bounding triangle
  xmin :=  INFINI;
  ymin :=  INFINI;
  xmax := -INFINI;
  ymax := -INFINI;
  (*
  xmin := FVertexs^[1].x;
  ymin := FVertexs^[1].y;
  xmax := xmin;
  ymax := ymin;
  For i := 2 To nvert do
  //*)
  For i := 1 To nvert do
  begin
  If FVertexs^[i].x < xmin Then xmin := FVertexs^[i].x;
  If FVertexs^[i].x > xmax Then xmax := FVertexs^[i].x;
  If FVertexs^[i].y < ymin Then ymin := FVertexs^[i].y;
  If FVertexs^[i].y > ymax Then ymax := FVertexs^[i].y;
  end;

  dx := xmax - xmin;
  dy := ymax - ymin;
  If dx > dy Then
    dmax := dx
  Else
    dmax := dy;

  xmid := Trunc((xmax + xmin) / 2);
  ymid := Trunc((ymax + ymin) / 2);

  //Set up the supertriangle
  //This is a triangle which encompasses all the sample points.
  //The supertriangle coordinates are added to the end of the
  //vertex list. 注意：The supertriangle is the first triangle in
  //the triangle list.

  FVertexs^[nvert + 1].x := (xmid - 2 * dmax);
  FVertexs^[nvert + 1].y := (ymid - dmax);
  FVertexs^[nvert + 2].x := xmid;
  FVertexs^[nvert + 2].y := (ymid + 2 * dmax);
  FVertexs^[nvert + 3].x := (xmid + 2 * dmax);
  FVertexs^[nvert + 3].y := (ymid - dmax);
  FTriangles^[1].vv0 := nvert + 1;
  FTriangles^[1].vv1 := nvert + 2;
  FTriangles^[1].vv2 := nvert + 3;
  FTriangles^[1].Precalc := 0;

  Completes[1] := False;
  ntri := 1;

  //Include each point one at a time into the existing mesh
  For i := 1 To nvert do
  begin
    Nedge := 0;
    //Set up the edge buffer.
    //If the point (Vertex(i).x,Vertex(i).y) lies inside the circumcircle then the
    //three edges of that triangle are added to the edge buffer.
    j := 0;
    repeat
      j := j + 1;
      If Completes^[j] <> True Then
      begin
        inc := InCircle(FVertexs^[i].x, FVertexs^[i].y, FVertexs^[FTriangles^[j].vv0].x,
                        FVertexs^[FTriangles^[j].vv0].y, FVertexs^[FTriangles^[j].vv1].x,
                        FVertexs^[FTriangles^[j].vv1].y, FVertexs^[FTriangles^[j].vv2].x,
                        FVertexs^[FTriangles^[j].vv2].y, xc, yc, r,j);
            //Include this if points are sorted by X
        If (xc + r) < FVertexs[i].x Then  //
          completes[j] := True          //
        Else If inc Then
        begin
          Edges^[1, Nedge + 1] := FTriangles^[j].vv0;
          Edges^[2, Nedge + 1] := FTriangles^[j].vv1;
          Edges^[1, Nedge + 2] := FTriangles^[j].vv1;
          Edges^[2, Nedge + 2] := FTriangles^[j].vv2;
          Edges^[1, Nedge + 3] := FTriangles^[j].vv2;
          Edges^[2, Nedge + 3] := FTriangles^[j].vv0;
          Nedge := Nedge + 3;
          FTriangles^[j].vv0 := FTriangles^[ntri].vv0;
          FTriangles^[j].vv1 := FTriangles^[ntri].vv1;
          FTriangles^[j].vv2 := FTriangles^[ntri].vv2;
          FTriangles^[j].PreCalc:=FTriangles^[ntri].PreCalc;
          FTriangles^[j].xc:=FTriangles^[ntri].xc;
          FTriangles^[j].yc:=FTriangles^[ntri].yc;
          FTriangles^[j].r:=FTriangles^[ntri].r;
          FTriangles^[ntri].PreCalc:=0;
          Completes^[j] := Completes^[ntri];
          j := j - 1;
          ntri := ntri - 1;
        End;//else
      End;  //if
    until j>=ntri; //repeat

    // Tag multiple edges
    // Note: if all triangles are specified anticlockwise then all
    // interior edges are opposite pointing in direction.
    For j := 1 To Nedge - 1 do
      If Not (Edges^[1, j] = 0) And Not (Edges^[2, j] = 0) Then
        For k := j + 1 To Nedge do
          If Not (Edges^[1, k] = 0) And Not (Edges^[2, k] = 0) Then
            If Edges^[1, j] = Edges^[2, k] Then
              If Edges^[2, j] = Edges^[1, k] Then
              begin
                Edges^[1, j] := 0;
                Edges^[2, j] := 0;
                Edges^[1, k] := 0;
                Edges^[2, k] := 0;
              End;

    //  Form new triangles for the current point
    //  Skipping over any tagged edges.
    //  All edges are arranged in clockwise order.
    For j := 1 To Nedge do
      If Not (Edges^[1, j] = 0) And Not (Edges^[2, j] = 0) Then
      begin
        ntri := ntri + 1;
        FTriangles^[ntri].vv0 := Edges^[1, j];
        FTriangles^[ntri].vv1 := Edges^[2, j];
        FTriangles^[ntri].vv2 := i;
        FTriangles^[ntri].PreCalc:=0;
        Completes^[ntri] := False;
      End;

  end; //the first for

  //Remove triangles with supertriangle vertices
  //These are triangles which have a vertex number greater than NVERT
  i:= 0;
  repeat
    i := i + 1;
    If (FTriangles^[i].vv0 > nvert) Or (FTriangles^[i].vv1 > nvert) Or (FTriangles^[i].vv2 > nvert) Then
    begin
      FTriangles^[i].vv0 := FTriangles^[ntri].vv0;
      FTriangles^[i].vv1 := FTriangles^[ntri].vv1;
      FTriangles^[i].vv2 := FTriangles^[ntri].vv2;
      i := i - 1;
      ntri := ntri - 1;
    End;
  until i>=ntri;

  Result := ntri;

  //Free memory
  FreeMem(Completes, sizeof(Completes^));
  FreeMem(Edges, sizeof(Edges^));
End;



function TTriangulationDelaunay.InCircle(xp, yp, x1, y1, x2, y2, x3, y3: Double;
    var xc: Double; var yc: Double; var r: Double; j: Integer): Boolean;
//Return TRUE if the point (xp,yp) lies inside the circumcircle
//made up by points (x1,y1) (x2,y2) (x3,y3)
//The circumcircle centre is returned in (xc,yc) and the radius r
//NOTE: A point on the edge is inside the circumcircle
var
  eps: Double;
  m1: Double;
  m2: Double;
  mx1: Double;
  mx2: Double;
  my1: Double;
  my2: Double;
  dx: Double;
  dy: Double;
  rsqr: Double;
  drsqr: Double;
begin
  eps:= 0.000001;
  Result := False;

  //Check if xc,yc and r have already been calculated
  if  FTriangles^[j].PreCalc=1 then
  begin
    xc := FTriangles^[j].xc;
    yc := FTriangles^[j].yc;
    r  := FTriangles^[j].r;
    rsqr := r*r;
    dx := xp - xc;
    dy := yp - yc;
    drsqr := dx * dx + dy * dy;
  end
  else
  begin
    If (Abs(y1 - y2) < eps) And (Abs(y2 - y3) < eps) Then
    begin
      AfficherMessageErreur('INCIRCUM - F - Points are coincident !!');
      Exit;
    end;

    If Abs(y2 - y1) < eps Then
    begin
      m2 := -(x3 - x2) / (y3 - y2);
      mx2 := (x2 + x3) / 2;
      my2 := (y2 + y3) / 2;
      xc := (x2 + x1) / 2;
      yc := m2 * (xc - mx2) + my2;
    end
    Else If Abs(y3 - y2) < eps Then
    begin
      m1 := -(x2 - x1) / (y2 - y1);
      mx1 := (x1 + x2) / 2;
      my1 := (y1 + y2) / 2;
      xc := (x3 + x2) / 2;
      yc := m1 * (xc - mx1) + my1;
    end
    Else
    begin
      m1 := -(x2 - x1) / (y2 - y1);
      m2 := -(x3 - x2) / (y3 - y2);
      mx1 := (x1 + x2) / 2;
      mx2 := (x2 + x3) / 2;
      my1 := (y1 + y2) / 2;
      my2 := (y2 + y3) / 2;
      if (m1-m2)<>0 then  //se
      begin
        xc := (m1 * mx1 - m2 * mx2 + my2 - my1) / (m1 - m2);
        yc := m1 * (xc - mx1) + my1;
      end
      else
      begin
        xc:= (x1+x2+x3)/3;
        yc:= (y1+y2+y3)/3;
      end;
    end;//else

    dx := x2 - xc;
    dy := y2 - yc;
    rsqr := dx * dx + dy * dy;
    r := Sqrt(rsqr);
    dx := xp - xc;
    dy := yp - yc;
    drsqr := dx * dx + dy * dy;

    //store the xc,yc and r for later use
    FTriangles^[j].PreCalc:=1;
    FTriangles^[j].xc:=xc;
    FTriangles^[j].yc:=yc;
    FTriangles^[j].r:=r;
  end;  //the big else

  If drsqr <= rsqr Then Result := True;
end;







function TTriangulationDelaunay.WhichSide(xp, yp, x1, y1, x2, y2: Double): Integer;
//Determines which side of a line the point (xp,yp) lies.
//The line goes from (x1,y1) to (x2,y2)
//Returns -1 for a point to the left
//         0 for a point on the line
//        +1 for a point to the right
var
  equation: Double;
begin
  equation := ((yp - y1) * (x2 - x1)) - ((y2 - y1) * (xp - x1));

  If equation > 0 Then
     WhichSide := -1
  Else If equation = 0 Then
     WhichSide := 0
  Else
     WhichSide := 1;
End;











procedure TTriangulationDelaunay.ClearVertex();
begin
  FzLow         := 0;
  FzHigh        := 0;
  FPointCount   := 1;
  FTriangleCount:= 0;
end;




(*

procedure TTriangulationDelaunay.ScatterContour(ZCount: Integer; Z: array of double);
var
  i,j,m: Integer;
  Deside: Integer;
  CastTab : TCastArray;

  sH      : TVectorL3I;
  H,xH,yH : TVectorL3D;

  TempD1,TempD2,dMin,dMax: Double ;
  x1,x2,y1,y2: Double; //等值点坐标

  ARecord: PPointPair; //记录点对

  //插值计算
  Function xSec(p1,p2:Integer): Double;
  Begin
    result:= (H[p2]*xH[p1]-H[p1]*xH[p2])/(H[p2]-H[p1]);
  End;

  Function ySec(p1,p2:Integer): Double;
  Begin
    result:= (H[p2]*yH[p1]-H[p1]*yH[p2])/(H[p2]-H[p1]);
  End;

begin
  //分配记录等值线的数组
  for i:= 0 to Length(FLevels)-1 do
    if Assigned(FLevels[i].Points) then
      FLevels[i].Points.Free;
  SetLength(FLevels,ZCount);
  for i:= 0 to ZCount-1 do
  begin
    FLevels[i].FZ:= Z[i];
    FLevels[i].Points:= TList.Create;
  end;

  //每个三角行内出现等值点的情况映射，有27种情况
  //这27种情况是根据三角形的三个顶点高程与等值点
  //的大小比较得来得，每个点有三种情况：大、小、等
  //0..19 为 对各种情况的处理方法，有20种
  CastTab[0,0,0]:= 0; CastTab[0,0,1]:= 0; CastTab[0,0,2]:= 1;
  CastTab[0,1,0]:= 0; CastTab[0,1,1]:= 2; CastTab[0,1,2]:= 3;
  CastTab[0,2,0]:= 4; CastTab[0,2,1]:= 5; CastTab[0,2,2]:= 6;

  CastTab[1,0,0]:= 0; CastTab[1,0,1]:= 7; CastTab[1,0,2]:= 8;
  CastTab[1,1,0]:= 9; CastTab[1,1,1]:= 10; CastTab[1,1,2]:= 9;
  CastTab[1,2,0]:= 8; CastTab[1,2,1]:= 7; CastTab[1,2,2]:= 0;

  CastTab[2,0,0]:= 6; CastTab[2,0,1]:= 5; CastTab[2,0,2]:= 4;
  CastTab[2,1,0]:= 3; CastTab[2,1,1]:= 2; CastTab[2,1,2]:= 0;
  CastTab[2,2,0]:= 1; CastTab[2,2,1]:= 0; CastTab[2,2,2]:= 0;

  for i:= 1 to FTriangleCount do
  begin

    //获得三角形三个顶点中的最小值和最大值
    TempD1:= min(FVertexs^[FTriangles^[i].vv0].Z,FVertexs^[FTriangles^[i].vv1].Z);
    TempD2:= min(FVertexs^[FTriangles^[i].vv1].Z,FVertexs^[FTriangles^[i].vv2].Z);
    dMin:= min(TempD1,TempD2);
    TempD1:= max(FVertexs^[FTriangles^[i].vv0].Z,FVertexs^[FTriangles^[i].vv1].Z);
    TempD2:= max(FVertexs^[FTriangles^[i].vv1].Z,FVertexs^[FTriangles^[i].vv2].Z);
    dMax:= max(TempD1,TempD2);

    for j:= 0 to ZCount-1 do
      if (Z[j] >= dMin) And (Z[j] <= dMax) Then
      begin

        H[0] := FVertexs^[FTriangles^[i].vv0].Z-Z[j];
        xH[0]:= FVertexs^[FTriangles^[i].vv0].X;
        yH[0]:= FVertexs^[FTriangles^[i].vv0].Y;
        H[1] := FVertexs^[FTriangles^[i].vv1].Z-Z[j];
        xH[1]:= FVertexs^[FTriangles^[i].vv1].X;
        yH[1]:= FVertexs^[FTriangles^[i].vv1].Y;
        H[2] := FVertexs^[FTriangles^[i].vv2].Z-Z[j];
        xH[2]:= FVertexs^[FTriangles^[i].vv2].X;
        yH[2]:= FVertexs^[FTriangles^[i].vv2].Y;

        for m:= 0 to 2 do
          If H[m] > 0 Then
            sH[m]:= 1
          Else If H[m]<0 Then
            sH[m]:= -1
          Else
            sH[m]:= 0;

        Deside := CastTab[sH[0]+1 ,sH[1]+1, sH[2]+1];

        If NOT(deside = 0) Then // 0的情况不处理
        begin
          Case deside Of
                  1:  begin
                        x1:= xSec(0,2);
                        y1:= ySec(0,2);
                        x2:= xSec(1,2);
                        y2:= ySec(1,2);
                      end;
                  2:  begin
                        x1:= xH[1];
                        y1:= yH[1];
                        x2:= xH[2];
                        y2:= yH[2];
                      end;
                  3:  begin
                        x1:= xH[1];
                        y1:= yH[1];
                        x2:= xSec(0,2);
                        y2:= ySec(0,2);
                      end;
                  4:  begin
                        x1:= xSec(0,1);
                        y1:= ySec(0,1);
                        x2:= xSec(1,2);
                        y2:= ySec(1,2);
                      end;
                  5:  Begin
                        x1:= xH[2];
                        y1:= yH[2];
                        x2:= xSec(0,1);
                        y2:= ySec(0,1);
                      End;
                  6:  Begin
                        x1:= xSec(0,1);
                        y1:= ySec(0,1);
                        x2:= xSec(0,2);
                        y2:= ySec(0,2);
                      End;
                  7:  Begin
                        x1:= xH[0];
                        y1:= yH[0];
                        x2:= xH[2];
                        y2:= yH[2];
                      End;
                  8:  Begin
                        x1:= xH[0];
                        y1:= yH[0];
                        x2:= xSec(1,2);
                        y2:= ySec(1,2);
                      End;
                  9:  Begin
                        x1:= xH[0];
                        y1:= yH[0];
                        x2:= xH[1];
                        y2:= yH[1];
                      End;
                  10: begin         //there is some argument here
                        x1:= xH[0];
                        y1:= yH[0];
                        x2:= xH[2];
                        y2:= yH[2];
                      end;
                end;//----case

          //此处获得该三角形内的等值点
          New(ARecord);
          ARecord^.x1:= x1;
          ARecord^.y1:= y1;
          ARecord^.x2:= x2;
          ARecord^.y2:= y2;
          FLevels[j].Points.Add(ARecord);
        end; //if not(deside)
      end;// if Z[]
  end;
end;
//*)

end.
