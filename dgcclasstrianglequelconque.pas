unit DGCClassTriangleQuelconque;

{$mode delphi}

interface

uses
  Classes, SysUtils, math,
  DGCTypes;
type TModeResolutionTriangle = (mrtTROIS_COTES,
                                mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_abC,
                                mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_bcA,
                                mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_caB,
                                mrtCOTE_A_et_ANGLES_BC_CONNUS,
                                mrtCOTE_B_et_ANGLES_CA_CONNUS,
                                mrtCOTE_C_et_ANGLES_AB_CONNUS,
                                mrtERROR);
type

{ TTriangleQuelconque }

 TTriangleQuelconque = class
  private
    FLongueurA, FLongueurB, FLongueurC : double;
    FAngleA   , FAngleB   , FAngleC    : double;
    FArea      : double;
    FPerimeter : double;
    FElementsConnus: TModeResolutionTriangle;
    FLastError : string;
    function Aire(const a, b, c: double): double;
    function AngleAFromLongueursABC(const a, b, c: double): double;
    function AngleBFromLongueursABC(const a, b, c: double): double;
    function AngleCFromLongueursABC(const a, b, c: double): double;

    function CoteOpposeFromAngleEtCotesAdjacents_abC(const a, b, C: double): double;
    function CoteOpposeFromAngleEtCotesAdjacents_bcA(const b, c, A: double): double;
    function CoteOpposeFromAngleEtCotesAdjacents_caB(const c, a, B: double): double;

    function Perimetre(const a, b, c: double): double;
    function DetermineModeResolutionTriangle(const s: string): TModeResolutionTriangle;
  public
    property LongueurA: double read FLongueurA write FLongueurA;
    property LongueurB: double read FLongueurB write FLongueurB;
    property LongueurC: double read FLongueurC write FLongueurC;

    property AngleA   : double read FAngleA write FAngleA;
    property AngleB   : double read FAngleB write FAngleB;
    property AngleC   : double read FAngleC write FAngleC;

    property Area     : double read FArea;
    property Perimeter: double read FPerimeter;


    constructor Create(const CodeEltsConnus: TModeResolutionTriangle; const QLongueurA, QLongueurB, QLongueurC, QAngleA, QAngleB, QAngleC: double);
    function Resoudre(): boolean;
    function GetLastError(): string;
end;

implementation

{ TTriangleQuelconque }

constructor TTriangleQuelconque.Create(const CodeEltsConnus: TModeResolutionTriangle; const QLongueurA, QLongueurB, QLongueurC, QAngleA, QAngleB, QAngleC: double);
begin
  inherited Create;
  FElementsConnus := CodeEltsConnus;
  FLongueurA := QLongueurA;
  FLongueurB := QLongueurB;
  FLongueurC := QLongueurC;

  FAngleA    := QAngleA;
  FAngleB    := QAngleB;
  FAngleC    := QAngleC;

  FArea      := 0.00;
  FPerimeter := 0.00;

  FLastError := '';

end;

function TTriangleQuelconque.Resoudre(): boolean;
var
  EWE: Boolean;
  DT: TModeResolutionTriangle;
begin
  result := false;
  FLastError := '';

  case FElementsConnus of
    mrtTROIS_COTES:
      begin
        EWE := IsZero(FLongueurA) AND IsZero(FLongueurB) AND IsZero(FLongueurC);
        if (EWE) then
        begin
          FLastError := Format('Les trois côtés sont nuls: %f, %f, %f', [FLongueurA, FLongueurB, FLongueurC]);
          exit;
        end;
        EWE := (FLongueurA > 0.00) AND (FLongueurB > 0.00) AND (FLongueurC > 0.00);
        if (EWE) then
        begin
          FLastError := 'Trois côtés connus';
          FAngleA    := AngleAFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
          FAngleB    := AngleBFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
          FAngleC    := AngleCFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
          FPerimeter := Perimetre(FLongueurA, FLongueurB, FLongueurC);
          FArea      := Aire(FLongueurA, FLongueurB, FLongueurC);
          exit(True);
        end;
      end;
    mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_abC:
      begin
        FLastError := Format('a (%.3f), b (%.3f) et C (%.3f) connus', [FLongueurA, FLongueurB, radtodeg(FAngleC)]);
        FLongueurC := CoteOpposeFromAngleEtCotesAdjacents_abC(FLongueurA, FLongueurB, FAngleC);
        FAngleA    := AngleAFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
        FAngleB    := AngleBFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
      end;
    mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_bcA:
      begin
        FLastError := Format('b (%.3f), c (%.3f) et A (%.3f) connus', [FLongueurB, FLongueurC, radtodeg(FAngleA)]);
        FLongueurA := CoteOpposeFromAngleEtCotesAdjacents_bcA(FLongueurB, FLongueurC, FAngleA);
        FAngleB    := AngleBFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
        FAngleC    := AngleCFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
      end;
    mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_caB:
      begin
        FLastError := Format('c (%.3f), a (%.3f) et B (%.3f) connus', [FLongueurC, FLongueurA, radtodeg(FAngleB)]);
        FLongueurB := CoteOpposeFromAngleEtCotesAdjacents_caB(FLongueurC, FLongueurA, FAngleB);
        FAngleA    := AngleAFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
        FAngleC    := AngleCFromLongueursABC(FLongueurA, FLongueurB, FLongueurC);
      end;
    mrtCOTE_A_et_ANGLES_BC_CONNUS:
      begin
        FLastError := Format('a (%.3f), B (%.3f) et C (%.3f) connus', [FLongueurA, radtodeg(FAngleB), radtodeg(FAngleC)]);
        FAngleA    := pi - (FAngleB + FAngleC);
        FLongueurB := FLongueurA * sin(FAngleB) / sin(FAngleB + FAngleC);
        FLongueurC := FLongueurA * sin(FAngleC) / sin(FAngleB + FAngleC);

      end;
    mrtCOTE_B_et_ANGLES_CA_CONNUS:
      begin
        FLastError := Format('b (%.3f), C (%.3f) et A (%.3f) connus', [FLongueurB, radtodeg(FAngleC), radtodeg(FAngleA)]);
        FAngleB    := pi - (FAngleC + FAngleA);
        FLongueurC := FLongueurB * sin(FAngleC) / sin(FAngleC + FAngleA);
        FLongueurA := FLongueurB * sin(FAngleA) / sin(FAngleC + FAngleA);
      end;
    mrtCOTE_C_et_ANGLES_AB_CONNUS:
      begin
        FLastError := Format('c (%.3f), A (%.3f) et B (%.3f) connus', [FLongueurC, radtodeg(FAngleA), radtodeg(FAngleB)]);
        FAngleC    := pi - (FAngleA + FAngleB);
        FLongueurA := FLongueurC * sin(FAngleA) / sin(FAngleA + FAngleB);
        FLongueurB := FLongueurC * sin(FAngleB) / sin(FAngleA + FAngleB);
      end;
  else
    begin
      FLastError := 'Mode non implémenté';
      exit(false);
    end;
  end;
  FPerimeter := Perimetre(FLongueurA, FLongueurB, FLongueurC);
  FArea      := Aire(FLongueurA, FLongueurB, FLongueurC);
  result     := true;
end;

function TTriangleQuelconque.GetLastError(): string;
begin
  result := FLastError;
end;

function TTriangleQuelconque.AngleAFromLongueursABC(const a, b, c: double): double;
begin
  result := ArcCos((b ** 2 + c ** 2 - a ** 2) / (2 * b * c));
end;
function TTriangleQuelconque.AngleBFromLongueursABC(const a, b, c: double): double;
begin
  result := ArcCos((c ** 2 + a ** 2 - b ** 2) / (2 * c * a));
end;
function TTriangleQuelconque.AngleCFromLongueursABC(const a, b, c: double): double;
begin
  result := ArcCos((a ** 2 + b ** 2 - c ** 2) / (2 * a * b));
end;

function TTriangleQuelconque.CoteOpposeFromAngleEtCotesAdjacents_bcA(const b, c, A: double): double;
begin
  Result := sqrt(b ** 2 + c ** 2 - 2 * b * c * cos(A));
end;
function TTriangleQuelconque.CoteOpposeFromAngleEtCotesAdjacents_caB(const c, a, B: double): double;
begin
  Result := sqrt(c ** 2 + a ** 2 - 2 * c * a * cos(B));
end;
function TTriangleQuelconque.CoteOpposeFromAngleEtCotesAdjacents_abC(const a, b, C: double): double;
begin
  Result := sqrt(a ** 2 + b ** 2 - 2 * a * b * cos(C))
end;

function TTriangleQuelconque.Perimetre(const a, b, c: double): double;
begin
  Result := FLongueurA + FLongueurB + FLongueurC;
end;

function TTriangleQuelconque.DetermineModeResolutionTriangle(const s: string): TModeResolutionTriangle;
begin
  result := mrtERROR;
  if (s = 'abc') then exit(mrtTROIS_COTES);
  if (s = 'abC') then exit(mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_abC);
  if (s = 'bcA') then exit(mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_bcA);
  if (s = 'caB') then exit(mrtCOTE_OPPOSE_FROM_COTES_ADJACENTS_caB);
  if (s = 'BaC') then exit(mrtCOTE_A_et_ANGLES_BC_CONNUS);
  if (s = 'CbA') then exit(mrtCOTE_B_et_ANGLES_CA_CONNUS);
  if (s = 'AcB') then exit(mrtCOTE_C_et_ANGLES_AB_CONNUS);
end;

function TTriangleQuelconque.Aire(const a, b, c: double): double;
var
  p: Double;
begin
  p := (FLongueurA + FLongueurB + FLongueurC) / 2.0;
  Result := sqrt(p * (p - a) * (p - b) * (p - c));
end;



end.
