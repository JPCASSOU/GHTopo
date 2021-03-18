unit SolverHugeSystem;

{$mode delphi}

interface

uses
  StructuresDonnees,
  Classes, SysUtils,
  SparSolver;

type

{ TMySolver }

 TMySolver = class
  private
    FSolveOK : boolean;
    FFileName: string;
  public
    function  Initialiser(const TailleSystemeEq: integer; const QFilename: string): boolean;
    function  Resoudre(): boolean;
    procedure AddValueAtMatrixA(const i, j: integer; const Value: double);
    procedure AddValueAtVectorB(const i: integer; const Value: double);
    function  GetXValue(const i: integer): double;
    procedure SaveMatrixAToFile();
    function  RestoreAMatrixFromFile(): boolean;
    procedure Finaliser();

end;


implementation

{ TMySolver }

function TMySolver.Initialiser(const TailleSystemeEq: integer; const QFilename: string): boolean;
begin
  FFileName := QFilename;
  Result := InitStruc(TailleSystemeEq);
end;

function TMySolver.Resoudre(): boolean;
begin
  Result := Solve1();
end;

procedure TMySolver.AddValueAtMatrixA(const i, j: integer; const Value: double);
begin
  AddLHS(i, j, Value);
end;

procedure TMySolver.AddValueAtVectorB(const i: integer; const Value: double);
begin
  AddRHS(i, Value);
end;

function TMySolver.GetXValue(const i: integer): double;
begin
  GetAnswer(i, Result);
end;

procedure TMySolver.SaveMatrixAToFile();
begin
  WriteMatrixAsMTX(FFileName);
end;

function TMySolver.RestoreAMatrixFromFile(): boolean;
begin
  result := RestoreMatrixFromFile(FFileName);
end;



procedure TMySolver.Finaliser();
begin
  ReleaseStruc();
end;

end.

