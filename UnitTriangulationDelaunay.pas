unit UnitTriangulationDelaunay;
// 16/01/2019: Fonctionne très mal: bugs exaspérants, randomisés, inconstants
//
{$mode delphi}
{$DEFINE USES_TLISTESIMPLES_POINTS}
{$UNDEF  USES_TLISTESIMPLES_POINTS}
{$DEFINE USES_TLISTESIMPLES_TRIANGLES}
{$UNDEF  USES_TLISTESIMPLES_TRIANGLES}

interface

uses
  StructuresDonnees,
  Common,
  {$IFDEF USES_TLISTESIMPLES_POINTS}
  UnitListesSimplesWithGeneriques,
  {$ENDIF}
  {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
  UnitListesSimplesWithGeneriques,
  {$ENDIF}
  math,
  Classes, SysUtils;

type PPoint3Df = ^TPoint3Df;
type TEquationDroite = record // équation de droite ax+by+c=0
       a,b,c:double;
end;

type
  PTriangle=^TTriangle;
  TTriangle = record
            p1,p2,p3: PPoint3Df;       // les trois sommets
            t1,t2,t3: PTriangle;       // les trois triangles ayant comme arretes communes celles
                                       // oposées respectivement à p1 p2 p3
            d1,d2,d3: TEquationDroite; // equations des arretes oposées respectivement à p1 p2 p3
            c       : TPoint2Df;       // centre du cercle circonscrit
            r       : double;          // rayon au carré du cercle circonscrit
           end;

{$IFDEF USES_TLISTESIMPLES_POINTS}
type TListePoints = class(TListeSimple<PPoint3Df>)
  private
  public
end;
{$ENDIF}
{$IFDEF USES_TLISTESIMPLES_TRIANGLES}
type TListeTriangles = class(TListeSimple<PTriangle>)
  private
  public
end;
{$ENDIF}

const MAX_VERTEX    = 30000;
const MAX_TRIANGLES = 60000;
type

{ TTriangulationDelaunay }

 TTriangulationDelaunay = class
  private
    {$IFDEF USES_TLISTESIMPLES_POINTS}
    FListePoints    : TListePoints;
    {$ELSE}
    FListePoints    : array[0 .. MAX_VERTEX] of PPoint3Df;
    {$ENDIF}

    {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
    FListeTriangles: TListeTriangles;
    {$ELSE}
    FListeTriangles : array[0 .. MAX_TRIANGLES] of PTriangle;
    {$ENDIF}
    FNbPoints       : integer;
    FNbTriangles    : integer;
    FLimitesDuMNT   : TRect2Df;
    FNbErreurs      : integer;
    function AddTriangle(const t: TTriangle; out TriangleAdded: PTriangle): boolean;
    function AddPoint(const QX, QY, QZ: Double; out PointAdded: PPoint3Df): boolean;
    function Algorythm_Delaunay(const x, y, z: double): boolean;
    function DelTriangle(const p: ptriangle; out T: TTriangle): boolean;
    function FindIdxOfPoint(const P: PPoint3Df): integer;
    function FindTriangle(const p: PPoint3Df; out PT: PTriangle): boolean;
    procedure ResetListes();
    function PointIsNotTooNearExisting(const P: TPoint3Df): boolean;


  public
    function  Initialiser(const QCoinBasGauche, QCoinHautDroit: TPoint3Df): boolean;
    procedure Finaliser();
    function  GetLimitesDuMNT(): TRect2Df;


    function  GetNbPoints(): integer;
    function  GetNbTriangles(): integer;

    procedure AddAPoint(const P: TPoint3Df; const DoMesh: boolean);
    function  GetPoint(const Idx: integer): TPoint3Df;
    function  GetTriangle(const Idx: integer): TMNTTriangleABC;
    function  GetNbErreursTriangulation(): integer;

end;

implementation
//calcule la distance au carré entre p1 et p2
function sqdistance(const p1, p2: PPoint3Df): double;
begin
 result := (p1.X-p2.X)*(p1.X-p2.X) + (p1.y-p2.y)*(p1.y-p2.y);
end;

// crée un triangle
function CreateTriangle(const p1, p2, p3: PPoint3Df; out MyTriangle: TTriangle): boolean;
var
 diviseur: double;
 d1,d2,d3: TEquationDroite;
 c       : TPoint2Df;
begin
 Result := false;
 // calcul de la droite media de [p1;p2]
 d1.a:=-(p1.X-p2.X);
 d1.b:=-(p1.Y-p2.Y);
 d1.c:=-(d1.a*(p1.x+p2.X) / 2) - (d1.b*(p1.y+p2.Y) / 2);
 // calcul de la droite media de [p2;p3]
 d2.a:=-(p2.X-p3.X);
 d2.b:=-(p2.Y-p3.Y);
 d2.c:=-(d2.a*(p2.x+p3.X) / 2) - (d2.b*(p2.y+p3.Y) / 2);
 // calcul de la droite media de [p1;p3]
 d3.a:=-(p1.X-p3.X);
 d3.b:=-(p1.Y-p3.Y);
 d3.c:=-(d3.a*(p1.x+p3.X) / 2) - (d3.b*(p1.y+p3.Y) / 2);

 // calcul du point d'inter de d1 et d2
 diviseur := d1.a*d2.b-d2.a*d1.b;

  if (IsZero(diviseur)) then Exit(false);   // = 0 si les trois points sont alignés


 c.X:=(d1.b*d2.c-d2.b*d1.c) / (diviseur);
 c.y:=(d2.a*d1.c-d1.a*d2.c) / (diviseur);

 MyTriangle.r:= sqdistance(@c, p1);

 MyTriangle.p1 := p1;
 MyTriangle.p2 := p2;
 MyTriangle.p3 := p3;

 // calcul de la droite [p1;p2]
 d3.a:=-(p1.y-p2.y);
 d3.b:= (p1.x-p2.x);
 d3.c:=-(d3.a*p1.x) - (d3.b*p1.y);
 // calcul de la droite [p2;p3]
 d1.a:=-(p2.y-p3.y);
 d1.b:= (p2.x-p3.x);
 d1.c:=-(d1.a*p2.x) - (d1.b*p2.y);
 // calcul de la droite [p1;p3]
 d2.a:=-(p1.y-p3.y);
 d2.b:= (p1.x-p3.x);
 d2.c:=-(d2.a*p1.x) - (d2.b*p1.y);

 MyTriangle.d1:=d1;
 MyTriangle.d2:=d2;
 MyTriangle.d3:=d3;
 MyTriangle.t1:=nil;
 MyTriangle.t2:=nil;
 MyTriangle.t3:=nil;
 MyTriangle.c:=c;

 Result := True;
end;

// est-ce que p1 et p2 sont du même coté de d ?
function ptsMemeCotedeD(const p1,p2: PPoint3Df; const d: TEquationDroite):boolean;
begin
 result:=(sign(d.a*p1.x+d.b*p1.y+d.c)*sign(d.a*p2.x+d.b*p2.y+d.c)>0)
end;

// p est-il dans t
function PtInTriangle(const p: PPoint3Df; const t: PTriangle):boolean;
begin
 result := ptsMemeCotedeD(p,t.p1,t.d1) and
           ptsMemeCotedeD(p,t.p2,t.d2) and
           ptsMemeCotedeD(p,t.p3,t.d3);
end;
// retourne le sommet de t qui n'est ni a ni b
function Get3rdPoint(const t: PTriangle; const a,b: PPoint3Df; out ThirdPoint: PPoint3Df): boolean;
begin
 result := false;
 if t=nil then exit(false);
 if (( t.p1 = a) and ( t.p2 = b)) or (( t.p1 = b) and ( t.p2 = a)) then ThirdPoint := t.p3 ;
 if (( t.p2 = a) and ( t.p3 = b)) or (( t.p2 = b) and ( t.p3 = a)) then ThirdPoint := t.p1 ;
 if (( t.p3 = a) and ( t.p1 = b)) or (( t.p3 = b) and ( t.p1 = a)) then ThirdPoint := t.p2 ;
 Result := True;
end;

//  retourne le triangle voisin de t par l'arete [a,b]
function GetTriangleVoisin(const t: PTriangle; const a,b: PPoint3Df; out TriangleVoisin: ptriangle): Boolean;
begin
 result:=false;
 if ( t = nil) then exit(false);
 if (( t.p1 = a) and ( t.p2 =b)) or (( t.p1 =b) and ( t.p2 = a)) then TriangleVoisin := t.t3;
 if (( t.p2 = a) and ( t.p3 =b)) or (( t.p2 =b) and ( t.p3 = a)) then TriangleVoisin := t.t1;
 if (( t.p3 = a) and ( t.p1 =b)) or (( t.p3 =b) and ( t.p1 = a)) then TriangleVoisin := t.t2;
 Result := True;
end;

procedure actualise_voisin(tv, t: PTriangle;const a,b: PPoint3Df);
begin
 if tv=nil then exit;
	if ((tv.p1=a) and (tv.p2=b)) or ((tv.p1=b) and (tv.p2=a)) then tv.t3:=t
	else
  if ((tv.p1=a) and (tv.p3=b)) or ((tv.p1=b) and (tv.p3=a)) then tv.t2:=t
	else tv.t1:=t;
end;


// inverse les deux triangles d'arrete commune [a,b]
function swaptriangles(t1, t2: PTriangle; const a, b: PPoint3Df): boolean;
var
 p1,p2,p3,p4:PPoint3Df;
 v1,v2,v3,v4:PTriangle;
 foo: Boolean;
begin
 Result := false;
 p1 := a;
 p3 := b;
 foo := Get3rdPoint(t1,a,b, p2);
 foo := Get3rdPoint(t2,a,b, p4);

 foo := GetTriangleVoisin(t1,p1,p2, v1);
 foo := GetTriangleVoisin(t1,p3,p2, v2);
 foo := GetTriangleVoisin(t2,p3,p4, v3);
 foo := GetTriangleVoisin(t2,p1,p4, v4);

 if (not CreateTriangle(p1,p2,p4, t1^)) then Exit(false);
 if (not CreateTriangle(p3,p2,p4, t2^)) then Exit(false);


 t1.t1:=t2;
 t1.t2:=v4;
 t1.t3:=v1;

 t2.t1:=t1;
 t2.t2:=v3;
 t2.t3:=v2;
 actualise_voisin(v1,t1,p1,p2);
 actualise_voisin(v2,t2,p2,p3);
 actualise_voisin(v3,t2,p3,p4);
 actualise_voisin(v4,t1,p4,p1);
 Result := True;
end;



// applique la 2ème règle de Delaunay, aucun point dans le cercle circonscrit
procedure realigntriangle(t: PTriangle);
var
 t1,t2,t3:ptriangle;
 p1,p2,p3:PPoint3Df;
begin
 if t=nil then exit;
// AfficherMessageErreur('realign');
 try
   Get3rdPoint(t.t1,t.p2,t.p3, p1);
   Get3rdPoint(t.t2,t.p1,t.p3, p2);
   Get3rdPoint(t.t3,t.p1,t.p2, p3);
   begin

     // test avec les trois voisins
     if (p1<>nil) and (sqdistance(@t.c,p1)<t.r) then
      begin
       swaptriangles(t,t.t1,t.p2,t.p3);
       realigntriangle(t.t2);
       realigntriangle(t.t3);
      end
     else
     if (p2<>nil) and (sqdistance(@t.c,p2)<t.r) then
      begin
       swaptriangles(t,t.t2,t.p1,t.p3);
       realigntriangle(t.t1);
       realigntriangle(t.t3);
      end
     else
     if (p3<>nil) and (sqdistance(@t.c,p3)<t.r) then
      begin
       swaptriangles(t,t.t3,t.p1,t.p2);
       realigntriangle(t.t1);
       realigntriangle(t.t2);
      end;
   end;

 except
   AfficherMessageErreur('Echec en realign');
 end;
end;


// atribut les triangles t1,t2,t3 à t
procedure defineTriangle(t: PTriangle; const t1,t2,t3: PTriangle);
begin
 t^.t1:=t1; t^.t2:=t2; t^.t3:=t3;
end;

///*****************************************************************************
{ TTriangulationDelaunay }

function TTriangulationDelaunay.Initialiser(const QCoinBasGauche, QCoinHautDroit: TPoint3Df): boolean;
var
  p1, p2, p3, p4: PPoint3Df;
  dummy: PTriangle;
  QTriangle1, QTriangle2: TTriangle;

  {$IFDEF USES_TLISTESIMPLES_TRIANGLES}QTriangle0, QTriangle1: PTriangle;  {$ENDIF}
begin
 AfficherMessageErreur(Format('%s.Initialiser:', [classname]));
 result := false;
 try
    {$IFDEF USES_TLISTESIMPLES_POINTS}
    FListePoints := TListePoints.Create;
    {$ENDIF}
    {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
    FListeTriangles := TListeTriangles.Create;
    {$ENDIF}
    self.ResetListes();
    FLimitesDuMNT.X1 := QCoinBasGauche.X;
    FLimitesDuMNT.Y1 := QCoinBasGauche.Y;
    FLimitesDuMNT.X2 := QCoinHautDroit.X;
    FLimitesDuMNT.Y2 := QCoinHautDroit.Y;

    AfficherMessageErreur(Format('-- (%f, %f) -> (%f, %f)', [FLimitesDuMNT.X1, FLimitesDuMNT.Y1, FLimitesDuMNT.X2, FLimitesDuMNT.Y2]));

    // crée les quatres points, coins de l'image
    Addpoint(QCoinBasGauche.X, QCoinBasGauche.Y, QCoinBasGauche.Z, p1);
    Addpoint(QCoinHautDroit.X, QCoinBasGauche.Y, QCoinBasGauche.Z, p2);
    Addpoint(QCoinHautDroit.X, QCoinHautDroit.Y, QCoinBasGauche.Z, p3);
    AddPoint(QCoinBasGauche.X, QCoinHautDroit.Y, QCoinBasGauche.Z, p4);

    // cre les deux triangles initiaux
    if (createtriangle(p1,p2,p3, QTriangle1)) then addtriangle(QTriangle1, dummy);
    if (createtriangle(p1,p3,p4, QTriangle2)) then addtriangle(QTriangle2, dummy);

    // lie les deux premiers triangles
    {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
    QTriangle0 := FListeTriangles.GetElement(0);
    QTriangle1 := FListeTriangles.GetElement(1);
    QTriangle0.t1 := QTriangle1;
    QTriangle1.t3 := QTriangle0;
    FListeTriangles.PutElement(0, QTriangle0);
    FListeTriangles.PutElement(1, QTriangle1);

    {$ELSE}
    FListeTriangles[0].t1:=FListeTriangles[1];
    FListeTriangles[1].t3:=FListeTriangles[0];

    {$ENDIF}
    result := true;
  except
    AfficherMessageErreur('Echec de démarrage');
  end;

end;
procedure TTriangulationDelaunay.Finaliser();
begin
  AfficherMessageErreur(Format('%s.Finaliser:', [classname]));
  try
    self.ResetListes();

  finally
    {$IFDEF USES_TLISTESIMPLES_POINTS}
    FreeAndNil();//FListePoints.Free;
    {$ENDIF}
    {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
    FreeAndNil();//FListeTriangles.Free;
    {$ENDIF}
  end;

end;
procedure TTriangulationDelaunay.ResetListes();
begin
  {$IFDEF USES_TLISTESIMPLES_POINTS}
  FListePoints.ClearListe();
  {$ELSE}
  //SetLength(FListePoints, 0);
  {$ENDIF}
  {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
  FListeTriangles.ClearListe();
  {$ELSE}
  //SetLength(FListeTriangles, 0);
  {$ENDIF}

  FNbPoints    := 0;
  FNbTriangles := 0;
  FNbErreurs   := 0;
end;
// pour éviter des bugs, vérifier si le point candidat est suffisamment éloigné
function TTriangulationDelaunay.PointIsNotTooNearExisting(const P: TPoint3Df): boolean;
const
  DIST_MINI    = 0.5;
  SQ_DIST_MINI = DIST_MINI * DIST_MINI;
var
  i, Nb: Integer;
  PP: TPoint3Df;
  dMin: Double;
  d: ValReal;
begin
  dMin := INFINI;
  result := false;
  {$IFDEF USES_TLISTESIMPLES_POINTS}
  Nb := FListePoints.GetNbElements();
  {$else}
  Nb := Length(FListePoints);
  {$endif};
  if (Nb = 0) then exit(false);
  for i := 0 to Nb-1 do
  begin
    PP := GetPoint(i);
    d  := sqr(P.X - PP.x) + sqr(P.y - PP.y);
    if (d < dMin) then dMin := d;
  end;
  Result := (dmin > SQ_DIST_MINI);
end;


function TTriangulationDelaunay.GetNbErreursTriangulation(): integer;
begin
 result := FNbErreurs;
end;



function TTriangulationDelaunay.GetLimitesDuMNT(): TRect2Df;
begin
  result := FLimitesDuMNT;
end;

function TTriangulationDelaunay.GetNbPoints(): integer;
begin
  {$IFDEF USES_TLISTESIMPLES_POINTS}
  result := FListePoints.GetNbElements();
  {$ELSE}
  result := FNbPoints; //length(FListePoints);
  {$ENDIF}
end;

function TTriangulationDelaunay.GetNbTriangles(): integer;
begin
  {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
  result := FListeTriangles.GetNbElements();
  {$ELSE}
  result := FNbTriangles;// length(FListeTriangles);
  {$ENDIF}
end;

procedure TTriangulationDelaunay.AddAPoint(const P: TPoint3Df; const DoMesh: boolean);
var
  S: Boolean;
begin
  //s := PointInRectangle(P, FLimitesDuMNT);
  //if (s) then
  Algorythm_Delaunay(P.X, P.Y, P.Z);  // OK ? On ajoute
end;

function TTriangulationDelaunay.GetPoint(const Idx: integer): TPoint3Df;
var
  PP: PPoint3Df;
begin
  {$IFDEF USES_TLISTESIMPLES_POINTS}
  PP := FListePoints.GetElement(Idx);
  Result := PP^;
  {$ELSE}
  Result := FListePoints[Idx]^;
  {$ENDIF}
end;

function TTriangulationDelaunay.GetTriangle(const Idx: integer): TMNTTriangleABC;
var
  TT: PTriangle;
begin
  {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
  TT := FListeTriangles.GetElement(Idx);
  {$else}
  TT := FListeTriangles[Idx];
  {$endif}
  Result.Numero := Idx;
  Result.PointA := FindIdxOfPoint(TT.p1);
  Result.PointB := FindIdxOfPoint(TT.p2);
  Result.PointC := FindIdxOfPoint(TT.p3);
end;

// ajoute un point à listepoint
function TTriangulationDelaunay.AddPoint(const QX, QY, QZ: Double; out PointAdded: PPoint3Df): boolean;
begin
  result := false;
  {$IFDEF USES_TLISTESIMPLES_POINTS}
    New(PointAdded);
    PointAdded^.X := QX;
    PointAdded^.Y := QY;
    PointAdded^.Z := QZ;
    FListePoints.AddElement(PointAdded);

  {$else}
  inc(FNbPoints);
  //SetLength(FListePoints, FNbPoints);
  New(FListePoints[FNbPoints-1]);
  FListePoints[FNbPoints-1]^.setFrom(QX, QY, QZ);
  PointAdded := FListePoints[FNbPoints - 1];
  {$ENDIF}
  result := true;
end;
// ajoute un point à listepoint
function TTriangulationDelaunay.FindIdxOfPoint(const P: PPoint3Df): integer;
var
  Nb, i: Integer;
begin
  result := -1;
  {$IFDEF USES_TLISTESIMPLES_POINTS}
  Nb := FListePoints.GetNbElements();
  if (Nb = 0) then Result := -1;
  for i := 0 to Nb-1 do
  begin
    if (P = FListePoints.GetElement(i)) then Exit(i);
  end;
  {$ELSE}
  Nb := FNbPoints;//length(FListePoints);
  if (Nb = 0) then Result := -1;
  for i := 0 to Nb-1 do
  begin
    if (P = FListePoints[i]) then Exit(i);
  end;
  {$ENDIF}
end;

//ajoute un triangle à listetriangle
function TTriangulationDelaunay.AddTriangle(const t: TTriangle; out TriangleAdded: PTriangle): boolean;
begin
 Result := false;
 try
 {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
   New(TriangleAdded);
   TriangleAdded^ := t;
   FListeTriangles.AddElement(TriangleAdded);

 {$ELSE}

   inc(FNbTriangles);
   //setlength(FListeTriangles, FMaxTriangle);
   new(FListeTriangles[FNbTriangles-1]);
   FListeTriangles[FNbTriangles-1]^:= t;
   TriangleAdded := FListeTriangles[FNbTriangles-1];


 {$ENDIF}
   result := True;
   //AfficherMessageErreur('AddTriangle()' + booltostr(result, 'OK', 'KO'));
 except
   Result := false;
 end;
end;

// extract et supprime le triangle n et ses références dans les autres triangles
function TTriangulationDelaunay.DelTriangle(const p: ptriangle; out T: TTriangle): boolean;
var
 i:integer;
 n, Idxfound: integer;
 EWE: PTriangle;
begin
  Result := false;
  Idxfound := -1;
  {$IFDEF USES_TLISTESIMPLES_TRIANGLES}
     // on recherche le triangle dans la liste
     n := FListeTriangles.GetNbElements();
     if (n = 0) then Exit(false);
     for i := 0 to n-1 do
     begin
     if (p = FListeTriangles.GetElement(i)) then
     begin
       Idxfound := i;
       break;
     end;
    end;
    if (Idxfound = -1) then Exit(false);
    T := P^;     // retourne la structure pointé par p

    // efface le triangle dans la liste
    FListeTriangles.RemoveElement(Idxfound);
    // remplace les références à p par nil dans le reste de la liste
    n := FListeTriangles.GetNbElements();
    if (n = 0) then Exit(false);
    for i:= 0 to FMaxTriangle-1 do
    begin
      EWE := FListeTriangles.GetElement(i);
      if (EWE.t1 = p) then EWE.t1 := nil;
      if (EWE.t2 = p) then EWE.t2 := nil;
      if (EWE.t3 = p) then EWE.t3 := nil;
      FListeTriangles.PutElement(i, EWE);
    end;

  {$ELSE}


  // on recherche le triangle dans la liste
  n := Length(FListeTriangles);
  for i := 0 to n-1 do
  begin
   if (p = FListeTriangles[i]) then
   begin
     Idxfound := i;
     break;
   end;
  end;
  if (Idxfound = -1) then Exit(false);
  // retourne la structure pointé par p
  T := p^;
  // libere la mémoire
  dispose(FListeTriangles[Idxfound]);
  // efface le triangle dans la liste
  dec(FNbTriangles);
  for i:= Idxfound to FNbTriangles - 1 do FListeTriangles[i]:=FListeTriangles[i+1];
  //SetLength(FListeTriangles, FMaxTriangle);  // setlength() préserve les données du tableau (cf REDIM PRESERVE du Basic)
  // remplace les références à p par nil dans le reste de la liste
  for i:=0 to FNbTriangles-1 do
  begin
   if (FListeTriangles[i].t1 = p) then FListeTriangles[i].t1 := nil;
   if (FListeTriangles[i].t2 = p) then FListeTriangles[i].t2 := nil;
   if (FListeTriangles[i].t3 = p) then FListeTriangles[i].t3 := nil;
  end;
  Result := True;
  {$ENDIF}
end;

// cherche et extract le triangle contenant p
function TTriangulationDelaunay.FindTriangle(const p: PPoint3Df; out PT: PTriangle): boolean;
var
 i, Nb :integer;
 t :ptriangle;
begin
 result := false;
 {$IFDEF USES_TLISTESIMPLES_POINTS}
 Nb := FListeTriangles.GetNbElements();
 for i :=0 to Nb - 1 do
 begin
   PT :=  FListeTriangles.GetElement(i);
   if (PtInTriangle(p, PT)) then exit(True);
 end;
 {$ELSE}
 for i :=0 to FNbTriangles-1 do
 begin
   if (PtInTriangle(p, FListeTriangles[i])) then
   begin
     PT:= FListeTriangles[i];
     exit(true);
   end;
 end;
 {$endif}
end;

// applique la 1er règle de Delaunay, detruire et construire
function TTriangulationDelaunay.Algorythm_Delaunay(const x, y, z: double): boolean;
var
 ptri:PTriangle;
 oldtri, QTriangle1, QTriangle2, QTriangle3:TTriangle;   // ancien triangle
 t1,t2,t3:PTriangle; // les trois nouveaux triangles
 p1,p2,p3,p4: PPoint3Df; // les trois sommets + le nouveau au centre
 v1,v2,v3:PTriangle; // les trois voisins
 IsOK: boolean;


 procedure Cuicui(const b: boolean; const Etape: string; out Succeeded: boolean);
 var
   EWE: String;
 begin
   Succeeded := b;
   //EWE := Format('%s %s', [Etape, IIF(b, 'OK', 'KO')]);
   //AfficherMessageErreur(EWE);
 end;

begin
 //AfficherMessageErreur(Format('%s.Algorythm_Delaunay: %f, %f, %f', [ClassName, x, y, z]));
 result := false;
 try
   Cuicui(AddPoint(x,y,z, p4), 'Ajout du point', IsOK);
   Cuicui(FindTriangle(p4, ptri), 'Recherche triangle', IsOK);

   //supprime le triangle précédent
   Cuicui(DelTriangle(ptri, oldtri), 'Delete this triangle', IsOK);
   //if (not IsOK) then Exit;


   //AfficherMessageErreur('002');
   // récupère les trois sommets et les trois voisins
   p1 := oldtri.p1;
   p2 := oldtri.p2;
   p3 := oldtri.p3;

   v1 := oldtri.t1;
   v2 := oldtri.t2;
   v3 := oldtri.t3;

   // en construit 3 à la place
   if (CreateTriangle(p4,p2,p3, QTriangle1)) then AddTriangle(QTriangle1, t1);
   if (createtriangle(p4,p3,p1, QTriangle2)) then AddTriangle(QTriangle2, t2);
   if (createtriangle(p4,p1,p2, QTriangle3)) then AddTriangle(QTriangle3, t3);

     // attribut les triangles voisins à t1 t2 et t3
     defineTriangle(t1,v1,t2,t3);
     defineTriangle(t2,v2,t3,t1);
     defineTriangle(t3,v3,t1,t2);

     // fait pointer les voisins vers les trois triangles
     if v1<>nil then actualise_voisin(v1,t1,t1.p2,t1.p3);
     if v2<>nil then actualise_voisin(v2,t2,t2.p2,t2.p3);
     if v3<>nil then actualise_voisin(v3,t3,t3.p2,t3.p3);
     // réaligne les 3 triangles en appliquant la 2ème règle

       realigntriangle(t1);
       realigntriangle(t2);
       realigntriangle(t3);

   Result := True;
 except
   FNbErreurs += 1;
   //AfficherMessageErreur(Format('Failed %s.Algorythm_Delaunay: %f, %f, %f: Triangle %d', [ClassName, x, y, z, GetNbTriangles()]));
   //AfficherMessageErreur(Format('* %.2f < %.2f < %.2f', [FLimitesDuMNT.X1, X, FLimitesDuMNT.X2]));
   //AfficherMessageErreur(Format('* %.2f < %.2f < %.2f', [FLimitesDuMNT.Y1, Y, FLimitesDuMNT.Y2]));
 end;
end;
end.
