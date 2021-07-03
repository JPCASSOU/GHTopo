unit DGCClassSectionOfBeam;
// Calcul de caractéristiques de sections droites d'une poutre:
// - aire
// - centre de gravité
// - limites
// - moments quadratiques
// - produits de surface
// - directions principales
{$mode delphiunicode}
interface

uses
  Classes, SysUtils, math,
  DGCTypes;

type

{ TDGCSectionOfBeam }

 TDGCSectionOfBeam = class(TList)
  private
    FBounds    : TDGCBoundingBox;
    FPerimetre : double;
    FArea      : double;
    FCentroid  : TDGCPoint2D;
    FInertiaOx : double;
    FInertiaOy : double;
    FInertiaOxy: double;
    FInertiaPolar: double;
    FAnglePrincipalInertie: double;
    FMainInertia_I  : double;
    FMainInertia_II : double;
    FRayonGirationMin: double;
    FRayonGirationMax: double;
    procedure CalcPerimeter();
    procedure CalcArea();
    procedure CalcCentroid();
    procedure ClearListe();
    procedure CalcInertias();
  public
    function  Initialiser(): boolean;
    procedure Finaliser();
    function  GetNbVertex(): integer;
    function  AddVertex(const V: TDGCPoint2D): boolean; overload;
    function  AddVertex(const VX, VY: double): boolean; overload;
    procedure CalcBounds();
    function  GetBounds(): TDGCBoundingBox;
    function  GetVertex(const Idx: integer): TDGCPoint2D;
    function  RemoveVertex(const Idx: integer): boolean;
    procedure CalcCaracteristiques();

    property  Area            : double read FArea;
    property  Perimeter       : double read FPerimetre;
    property  Centroid        : TDGCPoint2D read FCentroid;
    property  InertiaXGX      : double read FInertiaOx;
    property  InertiaYGY      : double read FInertiaOy;
    property  InertiaXY       : double read FInertiaOxy;
    property  InertiaPolar    : double read FInertiaPolar;
    property  MainAngleInertia: double read FAnglePrincipalInertie;
    property  MainInertia_I   : double read FMainInertia_I;
    property  MainInertia_II  : double read FMainInertia_II;
    property  RayonGirationMin: double read FRayonGirationMin;
    property  RayonGirationMax: double read FRayonGirationMax;

end;

implementation
uses
  DGCDummyUnit;

// Classes utilitaires: Liste des vertex
function TDGCSectionOfBeam.GetNbVertex(): integer;
begin
  Result := self.Count;
end;

procedure TDGCSectionOfBeam.ClearListe();
var
  i, Nb: Integer;
begin
  Nb := self.Count;
  if (0 = Nb) then Exit;
  for i := 0 to Nb - 1 do
  begin
    try
      Dispose(self.Items[i]);
    except
    end;
  end;
  self.Clear();
end;


function TDGCSectionOfBeam.Initialiser(): boolean;
begin
  result := false;
  try
    FArea       := 0.00;
    FPerimetre  := 0.00;
    FCentroid.Empty();
    FInertiaOx  := 0.00;
    FInertiaOy  := 0.00;
    FInertiaOxy := 0.00;
    FInertiaPolar := 0.00;
    FAnglePrincipalInertie := 0.00;
    FMainInertia_I  := 0.00;
    FMainInertia_II := 0.00;

    FRayonGirationMin := 0.00;
    FRayonGirationMax := 0.00;

    self.ClearListe();
    result      := true;
  except
  end;
end;

procedure TDGCSectionOfBeam.CalcPerimeter();
var
  i, j, n: Integer;
  V1, V2: TDGCPoint2D;
begin
  FPerimetre := 0.00;
  n     := GetNbVertex();
  for i := 0 to n - 1 do
  begin
    j := Ifthen(n = (i+1), 0, i+1);
    V1 := GetVertex(i);
    V2 := GetVertex(j);
    FPerimetre += hypot(V2.X - V1.X, V2.Y - V1.Y);
  end;
end;

procedure TDGCSectionOfBeam.CalcArea();
var
  n, i, j: Integer;
  V1, V2: TDGCPoint2D;
begin
  FArea := 0.00;
  n     := GetNbVertex();
  for i := 0 to n - 1 do
  begin
    j := Ifthen(n = (i+1), 0, i+1);
    V1 := GetVertex(i);
    V2 := GetVertex(j);
    FArea += V1.X * V2.Y - V2.X * V1.Y;
  end;
  FArea:= 0.5 * Abs(FArea);
end;
//centre de gravité
procedure TDGCSectionOfBeam.CalcCentroid();
var
  Q6A: Double;
  n, i, j: Integer;
  V1, V2: TDGCPoint2D;
begin
  if (IsZero(FArea)) then exit;
  Q6A := 1 / (6 * FArea);
  FCentroid.X := 0.00;
  FCentroid.Y := 0.00;
  n := GetNbVertex();
  for i := 0 to n - 1 do
  begin
    j  := Ifthen(n = (i+1), 0, i+1);
    V1 := GetVertex(i);
    V2 := GetVertex(j);
    FCentroid.X += (V1.X + V2.X) * (V1.X * V2.Y - V2.X * V1.Y);
    FCentroid.Y += (V1.Y + V2.Y) * (V1.X * V2.Y - V2.X * V1.Y);
  end;
  FCentroid.setFrom(Q6A * FCentroid.X, Q6A * FCentroid.Y);
end;
// calcul des moments quadratiques
procedure TDGCSectionOfBeam.CalcInertias();
var
  V1, V2: TDGCPoint2D;
  WU: Double;
  n, i, j: Integer;
  Q1, Q2 : double;
begin
  if (IsZero(FArea)) then exit;
  FInertiaOx  := 0.00;
  FInertiaOy  := 0.00;
  FInertiaOxy := 0.00;
  n := GetNbVertex();
  for i := 0 to n - 1 do
  begin
    j := Ifthen(n = (i+1), 0, i+1);
    V1 := GetVertex(i);
    V2 := GetVertex(j);
    WU := (V1.X * V2.Y - V2.X * V1.Y);
    FInertiaOy  += WU * (V1.X ** 2 +  V1.X * V2.X + V2.X ** 2);
    FInertiaOx  += WU * (V1.Y ** 2 +  V1.Y * V2.Y + V2.Y ** 2);
    FInertiaOxy += WU *     (V1.X * V2.Y +
                         2 * V1.X * V1.Y +
                         2 * V2.X * V2.Y +
                             V2.X * V1.Y);
  end;
  // moments IxOx et IyOy par rapport au repère général
  FInertiaOx  := (FInertiaOx  / 12); //- FArea * sqr(FCentroid.Y);
  FInertiaOy  := (FInertiaOy  / 12); //- FArea * sqr(FCentroid.X);
  // produit de surface
  FInertiaOxy := (FInertiaOxy / 24);
  // transport des moments vers le centroide
  // IxOx = IxGx + S.Yg ** 2, avec S: aire de la section et Yg ordonnée du CDG
  // IxGx = IxOx + S.Xg ** 2, avec S: aire de la section et Xg abscisse du CDG
  // IxGy = IxOy + S.Xg.Yg
  FInertiaOx  := FInertiaOx  - FArea * FCentroid.Y ** 2;
  FInertiaOy  := FInertiaOy  - FArea * FCentroid.X ** 2;
  FInertiaOxy := FInertiaOxy - FArea * (FCentroid.X * FCentroid.Y);
  // moment quadratique polaire: Io = IxGx + IyGy
  FInertiaPolar := FInertiaOx + FInertiaOy;
  // Moments d'inertie principaux, axes principaux
  // cf cours https://campus.mines-douai.fr/pluginfile.php/9832/mod_resource/content/0/Chapitre4_webLatex/co/C3_MomentsPrincipaux.html
  //          https://solennepro.free.fr/RSA2/co/sections%20de%20poutres.html
  WU := FInertiaOy - FInertiaOx;
  FAnglePrincipalInertie := ifthen (IsZero(WU), 0.00, 0.50 * ArcTan(2 * FInertiaOxy / WU));

  Q1 := 0.50 * (FInertiaOx + FInertiaOy);
  Q2 := sqrt((0.50 * (FInertiaOx - FInertiaOy)) ** 2 + FInertiaOxy ** 2);

  FMainInertia_I   := Q1 - Q2;
  FMainInertia_II  := Q1 + Q2;
  // rayons de giration mini et maxi
  FRayonGirationMin := sqrt(FMainInertia_I / FArea);
  FRayonGirationMax := sqrt(FMainInertia_I / FArea);

end;

procedure TDGCSectionOfBeam.CalcCaracteristiques();
begin
  CalcBounds();
  CalcPerimeter();
  CalcArea();
  CalcCentroid();
  CalcInertias();
end;

procedure TDGCSectionOfBeam.Finaliser();
begin
  self.ClearListe();
end;
function  TDGCSectionOfBeam.AddVertex(const V: TDGCPoint2D): boolean; overload;
var
  pS : ^TDGCPoint2D;
begin
  result := false;
  try
    New(pS);
    pS^ := V;
    self.Add(pS);
    Result := True;
  except
  end;
end;
function  TDGCSectionOfBeam.AddVertex(const VX, VY: double): boolean; overload;
var
  EWE: TDGCPoint2D;
begin
  EWE.setFrom(VX, VY);
  Result := AddVertex(EWE);
end;

procedure TDGCSectionOfBeam.CalcBounds();
var
  i, Nb: Integer;
begin
  FBounds.Reset();
  Nb := GetNbVertex();
  if (0 = Nb) then exit;
  for i := 0 to Nb - 1 do FBounds.upDate(getVertex(i));
end;

function TDGCSectionOfBeam.GetBounds(): TDGCBoundingBox;
begin
  result := FBounds;
end;


function TDGCSectionOfBeam.GetVertex(const Idx: integer): TDGCPoint2D;
var
  pS : ^TDGCPoint2D;
begin
  pS := self.Items[Idx];
  Result := pS^;
end;

function TDGCSectionOfBeam.RemoveVertex(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
  finally
    self.Delete(Idx);
    Result := True;
  end;
end;
end.

