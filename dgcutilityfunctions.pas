unit DGCUtilityFunctions;
// Fonctions utilitaires

{$mode delphiunicode}

interface

uses
  DGCTypes,
  Classes, SysUtils, Graphics, Math;
procedure nop; inline; // analogue à l'instruction 'pass' du Python
function   DGCMakeCSSStyle(const S: TDGCStyleSheet): string;
// calcul de l'angle moyen de deux segments, avec la bonne orientation
function   DGCGetAngleBissecteur(const X1, Y1, X2, Y2: double): double;
// produit vectoriel éventuellement normalisé
function   DGCProduitVectoriel(const Vect1, Vect2: TDGCPoint2Dh; const Normalized: Boolean):TDGCPoint2Dh;
// calcul d'une courbe de Bézier - Retourne un tableau de points
function   DGCCalcBezierCurve(const P0, P1, P2, P3: TDGCPoint2D; const NbSubdivs: integer; out AP: TDGCArrayPoints2D): boolean;
// pour se rendre indépendant de GHTopo
function   DGCMakeTPoint(const QX, QY: integer): TPoint;

implementation
uses
  DGCDummyUnit; // pour éviter le bug de 'Fin de code source non trouvée


procedure nop; inline; // analogue à l'instruction 'pass' du Python
begin
  ;; // ne fait rien
end;
function DGCMakeCSSStyle(const S: TDGCStyleSheet): string;
begin
  result := 'MakeCSSStyle: Mock for ' + S.Stylename;
end;


// calcul de l'angle moyen de deux segments, avec la bonne orientation
function DGCGetAngleBissecteur(const X1, Y1, X2, Y2: double): double;
var
  V1, V2, W: TDGCPoint2Dh;
begin
  V1.setFrom(X1, Y1, 0.00);
  V2.setFrom(X2, Y2, 0.00);
  W.setFrom(0.00, 0.00, 1.00);
  // produits vectoriels
  v1 := DGCProduitVectoriel(v1,w,True);
  v2 := DGCProduitVectoriel(v2,w,True);
  //composition vectorielle
  W.setFrom(v1.x + v2.X, v1.y + v2.Y, v1.H + v2.H);
  // angles
  Result := ArcTan2(w.y+1e-12, w.x+1e-12);
end;


// produit vectoriel éventuellement normalisé
function DGCProduitVectoriel(const Vect1, Vect2: TDGCPoint2Dh; const Normalized: Boolean): TDGCPoint2Dh;
var
  v: TDGCPoint2Dh;
  r: double;
begin
  v.X:=Vect1.Y*Vect2.H-Vect1.H*Vect2.Y;
  v.Y:=Vect1.H*Vect2.X-Vect1.X*Vect2.H;
  v.H:=Vect1.X*Vect2.Y-Vect1.Y*Vect2.X;
  if Normalized then
  begin
    r := sqrt(Sqr(v.x)+sqr(v.y)+sqr(v.H))+1e-12;
    v.X := v.x/r;
    v.y := v.y/r;
    v.H := v.H/r;
  end;
  Result := v;
end;

// calcul d'une courbe de Bézier - Retourne un tableau de points
function DGCCalcBezierCurve(const P0, P1, P2, P3: TDGCPoint2D; const NbSubdivs: integer; out AP: TDGCArrayPoints2D): boolean;
var
  i: integer;
  PasT, t : double;
  A, B, C, D, Q1, Q2, Q3, T2, T3: double;

begin
  PasT := 1/NbSubdivs;
  AP.Empty();
  AP.SetCapacity(1+NbSubdivs);//  SetLength(AP, 1+NbSubdivs);
  t := 0;
  // coefs
  for i:= 0 to AP.GetNbElements() - 1 do //High(AP) do
  begin
    Q1 := 1 - t;
    Q2 := Q1 * Q1;
    Q3 := Q2 * Q1;
    T2 := t * t;
    T3 := T2 * t;
    A := Q3; //Q*Q*Q;
    B := 3 * Q2 * t;
    C := 3 * Q1 * T2; //T * T;
    D := T3; //T * T * T;
    AP.SetElement(i, A * P0.X + B * P1.X + C * P2.X + D * P3.X,
                     A * P0.Y + B * P1.Y + C * P2.Y + D * P3.Y);
    //AP[i].setFrom(A * P0.X + B * P1.X + C * P2.X + D * P3.X,
    //              A * P0.Y + B * P1.Y + C * P2.Y + D * P3.Y);
    t += PasT;
  end;
end;

function DGCMakeTPoint(const QX, QY: integer): TPoint;
begin
  Result.X := QX;
  Result.y := QY;
end;


end.

