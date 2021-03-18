unit unitResolutionTriangles;

{$mode delphi}

interface

uses
  Classes, SysUtils, Math;

type

{ TResolutionDeTriangle }

 TResolutionDeTriangle = class
  strict private
    FLongueurA: double;
    FLongueurB: double;
    FLongueurC: double;

    FAngleA   : double;
    FAngleB   : double;
    FAngleC   : double;

  private
    // loi des cosinus -> retourne les longueurs a, b, c opposées aux sommets A, B, C
    function LoiCosinusA(): double;
    function LoiCosinusB(): double;
    function LoiCosinusC(): double;
    // loi des sinus
    // demi périmètre
    function s(): double;
    // aire
    function Aire(): double;

  public
    function Resoudre(): boolean;

end;

implementation

{ TResolutionDeTriangle }

function TResolutionDeTriangle.LoiCosinusA(): double;
begin
  Result := sqr(FLongueurB) + sqr(FLongueurC) - 2 * FLongueurB * FLongueurC * cos(FAngleA);
end;

function TResolutionDeTriangle.LoiCosinusB(): double;
begin
  Result := sqr(FLongueurC) + sqr(FLongueurA) - 2 * FLongueurC * FLongueurA * cos(FAngleB);
end;
function TResolutionDeTriangle.LoiCosinusC(): double;
begin
  Result := sqr(FLongueurA) + sqr(FLongueurB) - 2 * FLongueurA * FLongueurB * cos(FAngleC);
end;

function TResolutionDeTriangle.s(): double;
begin
  result := (FLongueurA + FLongueurB + FLongueurC) / 2.0;
end;

function TResolutionDeTriangle.Aire(): double;
var
  p: Double;
begin
  p := self.s();
  Result := sqrt(P * (P - FLongueurA) * (P - FLongueurB) * (P - FLongueurC));
end;

function TResolutionDeTriangle.Resoudre(): boolean;
begin

end;

end.

