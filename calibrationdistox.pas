unit CalibrationDistoX;
//------------------------------------------------------------------------------
// DistoX Calibration Utilities
// PROPRIETARY UNIT - (c) Beat HEEB heeb@speleo.ch - Adapted by JP CASSOU
//------------------------------------------------------------------------------
{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  Common,
  math,
  CalculMatriciel3x3,
  Classes, SysUtils,
  grids;

type

{ TCalibrationDistoX }

 TCalibrationDistoX = class
  private
    FListeMesuresDeG: TList;
    FListeMesuresDeM: TList;
    fMatrix_aG  : TMatrix3x3;
    fMatrix_aM  : TMatrix3x3;
    fVecteur_bG : TPoint3Df;
    fVecteur_bM : TPoint3Df;
    fDelta      : double;
    fNbIters    : integer;
    function Check: boolean;
    procedure CheckOverflow(var M: TMatrix3x3; var V: TPoint3Df);
    procedure OptVectors(const gr, mr: TPoint3Df; const alpha: double; out gx, mx: TPoint3Df);
    procedure setJeuDeDonneesExemple;
    procedure TurnVectors(const gxp, mxp, gr, mr: TPoint3Df; out gx, mx: TPoint3Df);
    procedure ViderListes;

  public
    procedure AddMesureG(const MG: TPoint3Df);
    procedure AddMesureM(const MG: TPoint3Df);
    procedure AddMesuresGMByValues(const gx, gy, gz, mx, my, mz: double);
    function  GetMesureG(const Idx: integer): TPoint3Df;
    function  GetMesureM(const Idx: integer): TPoint3Df;

    function GetNbMesuresG: integer;
    function GetNbMesuresM: integer;

    procedure Optimise;

    function GetMatrix_aG: TMatrix3x3;
    function GetMatrix_aM  : TMatrix3x3;
    function GetVecteur_bG : TPoint3Df;
    function GetVecteur_bM : TPoint3Df;
    function GetDelta      : double;
    function GetNbIters    : integer;

    function GetDataFromGrid(const grd: TStringGrid): boolean;
    function SetDataInGrid(const grd: TStringGrid): boolean;

    function Initialiser: boolean;
    function Finaliser: boolean;
end;

implementation

{ TCalibrationDistoX }





//*****************************************************************************
function TCalibrationDistoX.Initialiser: boolean;
begin
  AfficherMessage(Format('%s.Initialiser()', [ClassName]));
  Result := false;
  try

  except
  end;
end;

function TCalibrationDistoX.Finaliser: boolean;
begin
  try
    ViderListes;
  finally

    AfficherMessage(Format('%s.Finaliser()', [ClassName]));
  end;
end;

end.

