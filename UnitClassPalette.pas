unit UnitClassPalette;
// Date: 19/04/2012
// Statut: Fonctionnel
// L'objet TPalette256 est conçu autour de la palette TOPOROBOT
// par conséquent, seules les deux premières méthodes sont utilisées
// 22/07/2015: Epurage du code
// 13/06/2019: Point de contrôle temporel (contrôle de version)

{$mode DELPHI}{$H+}
{$PACKRECORDS 1}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Graphics,
  SysUtils, Classes;

type TArray256Colors = array[0..255] of TColor;
//const COLORS_TABLE_SIZE = 256;
//type TArray256Colors = array of TColor;

function MacColorToPCColor(const MC: TMacintoshColor): TColor; overload; inline;
function MacColorToPCColor(const mR, mG, mB: word):TColor; overload; inline;
function PCColorToMacColor(const PCC: TColor): TMacintoshColor; deprecated;

type TPalette256 = class
  private
    FColorArray : TArray256Colors;
    procedure ViderTableCouleurs();
  public
    //procedure GenerateWeb216Palette(); deprecated;
    //procedure GenerateACADPalette(); deprecated;
    //procedure GenerateGrayScalePalette(); deprecated;
    procedure GenerateTOPOROBOTPalette();
    function  GetColorByIndex(const Idx: Integer):TColor;
    procedure Finaliser();
end;

implementation
// Conversions couleurs PC<>Mac
function MacColorToPCColor(const MC: TMacintoshColor): TColor; overload;
begin
  Result := MacColorToPCColor(MC.R, MC.G, MC.B);
end;
function MacColorToPCColor(const mR, mG, mB: word):TColor; overload;
begin
  Result := RGBToColor(mR shr 8, mG shr 8, mB shr 8);
end;
function PCColorToMacColor(const PCC: TColor): TMacintoshColor;
begin
  Result.R := Red(PCC) * 256;
  Result.G := Green(PCC) * 256;
  Result.B := Blue(PCC) * 256;
end;


procedure TPalette256.GenerateTOPOROBOTPalette();
const
  M4369  = 4369;
  M13107             = 13107;
  TWO_TIMES_M13107   = 2 * M13107;
  THREE_TIMES_M13107 = 3 * M13107;
  FOUR_TIMES_M13107  = 4 * M13107;
  FIVE_TIMES_M13107  = 5 * M13107;
  M48059 = 48059;
  M56797 = 56797;
  M61166 = 61166;
var
  i,j,k,q,r: integer;
  procedure Cuicui(const qr, qg, qb: byte);
  var
    WU, t: Integer;
  begin
    q := q + 1; FColorArray[q] := MacColorToPCColor(qr * M61166, qg * M61166, qb * M61166);
    q := q + 1; FColorArray[q] := MacColorToPCColor(qr * M48059, qg * M48059, qb * M48059);
    r := 0;
    for t := 1 to 7 do
    begin
      q := q + 1;
      r := r + IIF((t mod 2 <>0), M4369, 2 * M4369);
      WU := M48059 - r;
      FColorArray[q]:=MacColorToPCColor(qr * WU, qg * WU, qb * WU);
    end;
  end;
begin
  AfficherMessage(Format('%s.GenerateTOPOROBOTPalette',[ClassName]));
  ViderTableCouleurs();
  FColorArray[0] := clWhite;
  FColorArray[1] := clBlack;
  FColorArray[2] := MacColorToPCColor(30583, 30583 , 30583);
  FColorArray[3] := MacColorToPCColor(21845, 21845 , 21845);
  FColorArray[4] := MacColorToPCColor(65535, 65535 , 0);
  FColorArray[5] := MacColorToPCColor(65535, 26214 , 0); {5}
  FColorArray[6] := MacColorToPCColor(56797, 0     , 0); {6}
  FColorArray[7] := MacColorToPCColor(65535, 0     , 39321); {7}
  FColorArray[8] := MacColorToPCColor(26214, 0     , 39321); {8}
  FColorArray[9] := MacColorToPCColor(0    , 0     , 56797); {9}
  FColorArray[10]:= MacColorToPCColor(0    , 39321 , 65535); {10}
  FColorArray[11]:= MacColorToPCColor(0    , M61166, 0); {11}
  FColorArray[12]:= MacColorToPCColor(0    , 26214 , 0); {12}
  for i:=1 to 2 do
    FColorArray[12+i]:=MacColorToPCColor(M13107*(i+1), M13107 * i, M13107 * (i-1));
  FColorArray[15] := MacColorToPCColor(M48059, M48059, M48059);
  for i:=16 to 19 do  FColorArray[i] := MacColorToPCColor(FIVE_TIMES_M13107, FIVE_TIMES_M13107, M13107 * (20-i));
  // palette calculée
  q := 19;
  for i:=4 downto 2 do
    for j:=5 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FIVE_TIMES_M13107, i*M13107, j*M13107); end;
  q := 36;
  for i := 5 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FIVE_TIMES_M13107, M13107, i*M13107); end;
  for i := 5 downto 4 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FIVE_TIMES_M13107, 0, i*M13107); end;
  for i := 2 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FIVE_TIMES_M13107, 0, i*M13107); end;
  for j:=5 downto 0 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FOUR_TIMES_M13107, j*M13107, k*M13107); end;
  for j:=5 downto 3 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(THREE_TIMES_M13107,j*M13107, k*M13107); end;
  for k := 5 downto 2 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(THREE_TIMES_M13107,TWO_TIMES_M13107, k*M13107); end;
  q := q + 1;
  FColorArray[q] := MacColorToPCColor(THREE_TIMES_M13107,2*M13107, 0);
  for j:=1 downto 0 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(THREE_TIMES_M13107,j*M13107, k*M13107); end;
  for j:=5 downto 1 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(TWO_TIMES_M13107,j*M13107, k*M13107); end;
  q:=q-1;
  for k:=5 downto 4  do begin  q := q + 1; FColorArray[q]:=MacColorToPCColor(TWO_TIMES_M13107,0, k*M13107); end;
  for k:=2 downto 0  do begin  q := q + 1; FColorArray[q]:=MacColorToPCColor(TWO_TIMES_M13107,0, k*M13107); end;
  for j:=5 downto 0 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(M13107,j*M13107, k*M13107); end;
  for j:=5 downto 4 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(0,j*M13107, k*M13107); end;
  for k:=4 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(0, THREE_TIMES_M13107, k*M13107); end;
  for k:=5 downto 1 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(0, TWO_TIMES_M13107, k*M13107); end;
  for k:=5 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(0, M13107, k*M13107); end;
  for k:=5 downto 1 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(0, 0, k*M13107); end;
  Cuicui(1, 0, 0);
  Cuicui(0, 1, 0);
  Cuicui(0, 0, 1);
  r := M61166; FColorArray[q + 1] := MacColorToPCColor(r,r,r);
  r := M56797; FColorArray[q + 2] := MacColorToPCColor(r,r,r);
  r := 43690 ; FColorArray[q + 3] := MacColorToPCColor(r,r,r);
  r := 34952 ; FColorArray[q + 4] := MacColorToPCColor(r,r,r);
  for i:=1 to 3 do begin
    r:=r div 2;
    FColorArray[q+4+i]:=MacColorToPCColor(r,r,r);
    //AfficherMessageErreur(inttostr(q+4+i)); OK
  end;
end;
function TPalette256.GetColorByIndex(const Idx: Integer): TColor;
begin
  Result := clSilver;
  if (IsInRange(Idx, 0, High(self.FColorArray))) then Result := self.FColorArray[Idx];
end;

procedure TPalette256.Finaliser();
begin
  pass;
end;

procedure TPalette256.ViderTableCouleurs();
var
  i: Integer;
begin
  for i:= 0 to High(self.FColorArray) - 1 do FColorArray[i] := clSilver;
end;

// palettes non utilisées par GHTopo
(*
procedure TPalette256.GenerateGrayScalePalette();
var
  i: byte;
begin
 ViderTableCouleurs();
 for i:=0 to High(FColorArray) do FColorArray[i] := RGBToColor(i,i,i);
end;

procedure TPalette256.GenerateACADPalette();
var
  i: integer;
begin
  ViderTableCouleurs();
  for i:=0 to 255 do FColorArray[i] := Acad2RGB(i);
end;
procedure TPalette256.GenerateWeb216Palette();
var
  i, j, k  : integer;
  r,g,b    : byte;
  IdxColor : integer;
begin
  ViderTableCouleurs();
  for i:=1 to 6 do
    for j:=1 to 6 do
      for k:=1 to 6 do
        begin
          IdxColor:=(i-1) * 36 + (j-1) * 6 + (k-1);
          r := 255 - (i-1) * 51;
          g := 255 - (j-1) * 51;
          b := 255 - (k-1) * 51;
          FColorArray[IdxColor] := RGBToColor(r,g,b);
        end;
end;
//*)

end.

