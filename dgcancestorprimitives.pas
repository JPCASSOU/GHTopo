unit DGCAncestorPrimitives;
// Classe Ancêtre de toutes les primitives

{$mode delphiunicode}

interface

uses
  DGCTypes,
  DGCUtilityFunctions,
  Classes, SysUtils, Graphics, math;


type

{ TDGCPrimitive }

 TDGCPrimitive = class
  private // visible dans cette classe et non ses descendants
    FIdxStyleSheet: integer;
    FName         : string;
  protected // visible dans cette classe et ses descendants
    FBoundingBox  : TDGCBoundingBox;
  public
    constructor Create(const QIdxStyleSheet: integer; const QName: string);
    property IdxStyleSheet: integer read FIdxStyleSheet;
    property Name: string read FName;
    procedure SetBoundingBox(); virtual; abstract; // à définir par les descendants
    function GetBoundingBox(): TDGCBoundingBox;
end;




implementation

{ TDGCPrimitive }
constructor TDGCPrimitive.Create(const QIdxStyleSheet: integer; const QName: string);
begin
  inherited Create;
  FIdxStyleSheet := QIdxStyleSheet;
  FName          := QName;
end;

function TDGCPrimitive.GetBoundingBox(): TDGCBoundingBox;
begin
  result := FBoundingBox;
end;

end.

