unit CadreBoussole;
// Commun à GHTopo et aux utilitaires Phidgets
{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, math, Types,
  BGRABitmap;
type

  { TCdrBoussole }

  TCdrBoussole = class(TFrame)
    pnlBoussole: TPanel;
    lbInclinX: TStaticText;
    lbInclinY: TStaticText;
    Vue: TPaintBox;
    procedure FrameResize(Sender: TObject);
    procedure VuePaint(Sender: TObject);
  strict private
    FTmpBuffer: TBGRABitmap;
    FUNITE_Azimut       : double;
    FUNITE_Azimut_SUR_2 : double;
    FUNITE_Azimut_SUR_4 : double;
    TWO_PI_SUR_UNITE_Azimut       : double;
    FUNITE_Inclinaison  : double;
    FUNITE_Inclinaison_SUR_4 : double;
    TWO_PI_SUR_UNITE_Inclinaison  : double;



    //FTableSinus  : array of double;
    //FTableCosinus: array of double;
    FDrawInProcess: boolean;
    FRegionXMini  :double;
    FRegionXMaxi  :double;
    FRegionYMini  :double;
    FRegionYMaxi  :double;
    // paramètres de vues
    FRappHLVue      : double;
    FRappScrReal    : double;
    FInvRappScrReal : double;
    function  AzimutToAngleRad(const A: double): double;
    function InclinaisonToAngleRad(const A: double): double;
    procedure InitTablesTrigo();
    procedure DessinerBoussole();
    function  GetRYMaxi: double;
    function  GetCoordsPlan(const QX, QY: double): TPoint;
    procedure SetViewLimits(const X1, Y1, X2, Y2: double);
  private
    FBackColor: TColor;
    FRoseColor: TColor;
    FReticuleColor: TColor;
    FAzimut: double;
    FInclinaisonX: double;
    FInclinaisonY: double;
    FInclinaison : double;
  public
    function  Initialiser(const UniteAzimut, UniteInclinaison: double; const BackColor, RoseColor, ReticuleColor: TColor): boolean;
    procedure SetAzimut(const Az: double);
    procedure SetInclinaisonX(const P: double);
    procedure SetInclinaisonY(const P: double);
    procedure SetAzIncl(const Az, PX, PY: double);
    procedure SetBackColor(const C: TColor);
    procedure SetRoseColor(const C: TColor);
    procedure SetReticuleColor(const C: TColor);
  end;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TCdrBoussole }
const DEMI_TAILLE_BOUSSOLE = 120.00;
const RAYON_CADRAN_BOUSSOLE = DEMI_TAILLE_BOUSSOLE - 10.00;

procedure TCdrBoussole.VuePaint(Sender: TObject);
begin
  DessinerBoussole();
end;

procedure TCdrBoussole.FrameResize(Sender: TObject);
begin
  SetViewLimits(FRegionXMini, FRegionYMini, FRegionXMaxi, FRegionYMaxi);
  vue.Invalidate;
end;

function TCdrBoussole.AzimutToAngleRad(const A: double): double;
begin
  Result := (FUNITE_Azimut_SUR_4 - A) * TWO_PI_SUR_UNITE_Azimut;
end;
function TCdrBoussole.InclinaisonToAngleRad(const A: double): double;
begin
  Result := A * TWO_PI_SUR_UNITE_Inclinaison;
end;
procedure TCdrBoussole.InitTablesTrigo();
var
  i, n: Integer;
  a: Double;
begin
  n := trunc(FAzimut);
  (*
  SetLength(FTableSinus  , n);
  SetLength(FTableCosinus, n);

  for i := 0 to high(FTableSinus) do
  begin
    a := AzimutToAngleRad(i * 1.00);
    FTableCosinus[i] := cos(a);
    FTableSinus[i]   := sin(a);
  end;
  //*)
end;



function TCdrBoussole.GetRYMaxi: double;
begin
  // calcul du rapport Hauteur/largeur de vue
  FRappHLVue := Vue.Height / Vue.Width;
  // calcul du rapport Ecran/Réel
  FRappScrReal := Vue.Width / (FRegionXMaxi - FRegionXMini);
  FInvRappScrReal := 1 / FRappScrReal;
  // calcul de la hauteur de visualisation
  Result := FRegionYMini + (FRegionXMaxi - FRegionXMini) * FRappHLVue;
end;

function TCdrBoussole.GetCoordsPlan(const QX, QY: double): TPoint;
begin
  Result.X := Round((QX - FRegionXMini) * FRappScrReal);
  Result.Y := Round((FRegionYMaxi - QY) * FRappScrReal);
end;

procedure TCdrBoussole.SetViewLimits(const X1, Y1, X2, Y2: double);
const
  Epsilon = 1e-2;
var
  d1, d2: double;
begin
  d1 := X2 - X1;
  d2 := Y2 - Y1;
  // si zone trop étroite, abandonner
  if (Abs(d1) < Epsilon) or (Abs(d2) < Epsilon) then Exit;
  // échanger de manière à rendre indifférent le sens du rectangle de sélection
  FRegionXMini := X1;
  FRegionXMaxi := X2;
  FRegionYMini := Y1;
  FRegionYMaxi := Y2;
  // Redéfinition de la hauteur maxi
  FRegionYMaxi := GetRYMaxi;
end;

function TCdrBoussole.Initialiser(const UniteAzimut, UniteInclinaison: double; const BackColor, RoseColor, ReticuleColor: TColor): boolean;
begin
  Result := false;
  FBackColor := BackColor;
  FRoseColor := RoseColor;
  FReticuleColor := ReticuleColor;
  FUNITE_Azimut := UniteAzimut;
  FUNITE_Azimut_SUR_2 := FUNITE_Azimut / 2;
  FUNITE_Azimut_SUR_4 := FUNITE_Azimut / 4;
  TWO_PI_SUR_UNITE_Azimut   := 2 * PI / FUNITE_Azimut;

  FUNITE_Inclinaison := UniteInclinaison;
  FUNITE_Inclinaison_SUR_4 := FUNITE_Inclinaison / 4;
  TWO_PI_SUR_UNITE_Inclinaison   := 2 * PI / FUNITE_Inclinaison;

  FDrawInProcess := false;
  InitTablesTrigo();

  SetViewLimits(-DEMI_TAILLE_BOUSSOLE, -DEMI_TAILLE_BOUSSOLE, DEMI_TAILLE_BOUSSOLE, DEMI_TAILLE_BOUSSOLE);
  self.DoubleBuffered := true;
  SetAzimut(0.00);
  Result := true;
end;

procedure TCdrBoussole.SetAzimut(const Az: double);
begin
  FAzimut := Az;
  Vue.Invalidate;
end;

procedure TCdrBoussole.SetInclinaisonX(const P: double);
begin
  FInclinaisonX := P;
  lbInclinX.Caption := format('%.3f', [FInclinaisonX]);
  Vue.Invalidate;
end;

procedure TCdrBoussole.SetInclinaisonY(const P: double);
begin
  FInclinaisonY := P;
  lbInclinY.Caption := format('%.3f', [FInclinaisonY]);
  Vue.Invalidate;
end;

procedure TCdrBoussole.SetAzIncl(const Az, PX, PY: double);
begin
  FAzimut       := AZ;
  FInclinaisonX := PX;
  FInclinaisonY := PY;
  lbInclinX.Caption := format('%.3f', [FInclinaisonX]);
  lbInclinY.Caption := format('%.3f', [FInclinaisonY]);
  Vue.Invalidate;
end;


procedure TCdrBoussole.SetBackColor(const C: TColor);
begin
  FBackColor := C;
  Vue.Invalidate;
end;

procedure TCdrBoussole.SetRoseColor(const C: TColor);
begin
  FRoseColor := C;
  Vue.Invalidate;
end;

procedure TCdrBoussole.SetReticuleColor(const C: TColor);
begin
  FReticuleColor := C;
  Vue.Invalidate;
end;

//******************************************************************************
procedure TCdrBoussole.DessinerBoussole();
var
   R: TRect;
   procedure DrawCercle(const QX, QY, QR: double);
   var
     PC1, PC2: TPoint;
   begin
     PC1 := GetCoordsPlan(QX - QR, QY - QR);
     PC2 := GetCoordsPlan(QX + QR, QY + QR);
     FTmpBuffer.CanvasBGRA.Ellipse(PC1.X, PC1.Y, PC2.X, PC2.Y);
   end;
   procedure DrawFlecheNord(const az: double);
   const WIDTH_FLECHE_NORD = 0.15;
   var
     Ang: Double;
     PNX0, PNY0, PNX1, PNY1, PNX2, PNY2, PNX3, PNY3: ValReal;
     PC0, PC1, PC2, PC3: TPoint;
     PP : array[0..2] of TPoint;
   begin
     Ang := AzimutToAngleRad(Az);
     PNX0 := 0.00 + RAYON_CADRAN_BOUSSOLE * cos(Ang);
     PNY0 := 0.00 + RAYON_CADRAN_BOUSSOLE * sin(Ang);
     Ang := Ang + PI * 0.5;

     PNX1 := 0.00 + WIDTH_FLECHE_NORD * RAYON_CADRAN_BOUSSOLE * cos(Ang);
     PNY1 := 0.00 + WIDTH_FLECHE_NORD * RAYON_CADRAN_BOUSSOLE * sin(Ang);
     Ang := Ang + PI;
     PNX2 := 0.00 + WIDTH_FLECHE_NORD * RAYON_CADRAN_BOUSSOLE * cos(Ang);
     PNY2 := 0.00 + WIDTH_FLECHE_NORD * RAYON_CADRAN_BOUSSOLE * sin(Ang);
     Ang := Ang - PI * 0.5;
     PNX3 := 0.00 + RAYON_CADRAN_BOUSSOLE * cos(Ang);
     PNY3 := 0.00 + RAYON_CADRAN_BOUSSOLE * sin(Ang);
     PC0 := GetCoordsPlan(PNX0, PNY0);
     PC1 := GetCoordsPlan(PNX1, PNY1);
     PC2 := GetCoordsPlan(PNX2, PNY2);
     PC3 := GetCoordsPlan(PNX3, PNY3);
     // partie sud de l'aiguille
     PP[0] := PC3;
     PP[1] := PC2;
     PP[2] := PC1;
     FTmpBuffer.CanvasBGRA.Brush.Color := clYellow;
     FTmpBuffer.CanvasBGRA.Brush.Opacity := 128;
     FTmpBuffer.CanvasBGRA.Pen.Color := clMaroon;
     FTmpBuffer.CanvasBGRA.Pen.Width := 0;
     FTmpBuffer.CanvasBGRA.Polygon(PP);
     // partie Nord de l'aiguille
     PP[0] := PC0;
     PP[1] := PC1;
     PP[2] := PC2;
     FTmpBuffer.CanvasBGRA.Brush.Color   := clBlue;
     FTmpBuffer.CanvasBGRA.Pen.Color := clNavy;
     FTmpBuffer.CanvasBGRA.Pen.Width := 0;
     FTmpBuffer.CanvasBGRA.Polygon(PP);
   end;
   // dessiner les graduations à la mode de CHAIX-nous
   procedure DrawGraduations();
   const
     LG = RAYON_CADRAN_BOUSSOLE / 80;
   var
     i, p, n: integer;
     a, rg: Double;
     ca, sa: ValReal;
     PNX0, PNY0, PNX1, PNY1: double;
     PC0, PC1: TPoint;
   begin
     FTmpBuffer.CanvasBGRA.Pen.Color   := clGray;
     FTmpBuffer.CanvasBGRA.Pen.Opacity := 255;
     FTmpBuffer.CanvasBGRA.Pen.Width   := 0;
     n := Trunc(FUNITE_Azimut);
     p := 0;
     for i := 0 to n - 1 do
     begin
       p += 1;
       if (p > 5) then p := 1;
       rg := LG * (p + 1);
       a := AzimutToAngleRad(FAzimut + i * 1.00);
       ca := cos(A)   ; sa := sin(a);
       PNX0 := 0.00 + RAYON_CADRAN_BOUSSOLE * ca;
       PNY0 := 0.00 + RAYON_CADRAN_BOUSSOLE * sa;
       PC0  := GetCoordsPlan(PNX0, PNY0);
       PNX1 := 0.00 + (RAYON_CADRAN_BOUSSOLE - rg) * ca;
       PNY1 := 0.00 + (RAYON_CADRAN_BOUSSOLE - rg) * sa;
       PC1  := GetCoordsPlan(PNX1, PNY1);
       FTmpBuffer.CanvasBGRA.MoveTo(PC0);
       FTmpBuffer.CanvasBGRA.LineTo(PC1);
     end;
   end;
   procedure DrawReticule();
   var
     P1, P2: TPoint;
   begin
     FTmpBuffer.CanvasBGRA.Pen.Width := 0;
     FTmpBuffer.CanvasBGRA.Pen.Color := FReticuleColor;

     P1 := GetCoordsPlan(-RAYON_CADRAN_BOUSSOLE, 0);
     P2 := GetCoordsPlan( RAYON_CADRAN_BOUSSOLE, 0);
     FTmpBuffer.CanvasBGRA.MoveTo(P1); FTmpBuffer.CanvasBGRA.LineTo(P2);
     P1 := GetCoordsPlan(0, -RAYON_CADRAN_BOUSSOLE);
     P2 := GetCoordsPlan(0,  RAYON_CADRAN_BOUSSOLE);
     FTmpBuffer.CanvasBGRA.MoveTo(P1); FTmpBuffer.CanvasBGRA.LineTo(P2);
   end;
   procedure DrawValeur();
   var
     P1: TPoint;
     EWE: String;
     RT: TSize;
   begin
     P1 := GetCoordsPlan(0, RAYON_CADRAN_BOUSSOLE);
     //P2 := GetCoordsPlan( RAYON_CADRAN_BOUSSOLE, 0);
     FTmpBuffer.CanvasBGRA.Brush.Color := FBackColor;
     FTmpBuffer.CanvasBGRA.Font.Height := 20;
     FTmpBuffer.CanvasBGRA.Font.Style  := [fsBold];
     EWE := Format('%.3f', [FAzimut]);
     RT  := FTmpBuffer.CanvasBGRA.TextExtent(EWE);
     FTmpBuffer.CanvasBGRA.TextOut(P1.X - RT.cx div 2, P1.Y - 4 - RT.cy, EWE);
   end;
   procedure DrawBulle();
   const
     RB = 20;
   var
     rx, ry, ax, ay: Double;
     P1: TPoint;
     AngIncMax: float;
   begin
     FTmpBuffer.CanvasBGRA.Pen.Width := 0;
     FTmpBuffer.CanvasBGRA.Pen.Style := psSolid;
     FTmpBuffer.CanvasBGRA.Pen.Opacity := 255;

     FTmpBuffer.CanvasBGRA.Brush.Color   := clRed;
     FTmpBuffer.CanvasBGRA.Brush.Opacity := 64;
     ax := FInclinaisonX / FUNITE_Inclinaison_SUR_4;
     ay := FInclinaisonY / FUNITE_Inclinaison_SUR_4;
     AngIncMax := arctan2(ay, ax);

     rx := RAYON_CADRAN_BOUSSOLE * ax; // * cos(AngIncMax);
     ry := RAYON_CADRAN_BOUSSOLE * ay; // * sin(AngIncMax);

     P1 := GetCoordsPlan(rx, ry);
     FTmpBuffer.CanvasBGRA.EllipseC(P1.X, P1.Y, RB, RB);
     FTmpBuffer.CanvasBGRA.Pen.Color   := clBlue;
     FTmpBuffer.CanvasBGRA.MoveTo(P1.X - RB, P1.Y);
     FTmpBuffer.CanvasBGRA.LineTo(P1.X + RB, P1.Y);
     FTmpBuffer.CanvasBGRA.MoveTo(P1.X, P1.Y - RB);
     FTmpBuffer.CanvasBGRA.LineTo(P1.X, P1.Y + RB);
   end;
begin
  if (FDrawInProcess) then Exit;
  try
    FDrawInProcess := True;
    try
      FTmpBuffer := TBGRABitmap.Create(vue.Width, vue.Height);
      FTmpBuffer.CanvasBGRA.Font.Style  := [];
      FTmpBuffer.CanvasBGRA.Font.Height := 10;

      R.Left   := Vue.Left;
      R.Top    := Vue.Top;
      R.Bottom := Vue.Top  + Vue.Height;
      R.Right  := Vue.Left + Vue.Width;
      FTmpBuffer.CanvasBGRA.Pen.Width := 0;
      FTmpBuffer.CanvasBGRA.Pen.Style := psSolid;
      FTmpBuffer.CanvasBGRA.Pen.Opacity := 255;
      //FTmpBuffer.CanvasBGRA.Pen.Mode  := pmCopy;

      FTmpBuffer.CanvasBGRA.Brush.Color := FBackColor;
      FTmpBuffer.CanvasBGRA.Brush.Opacity := 255;
      FTmpBuffer.CanvasBGRA.Brush.Style   := bsSolid;
      FTmpBuffer.CanvasBGRA.FillRect(R);

      FTmpBuffer.CanvasBGRA.Pen.Color := clRed;
      FTmpBuffer.CanvasBGRA.Pen.Width := 2;
      FTmpBuffer.CanvasBGRA.Brush.Color := FRoseColor;

      // cercles
      DrawCercle(0.0, 0.0, RAYON_CADRAN_BOUSSOLE);
      // flèche du Nord
      DrawFlecheNord(FAzimut);

      // graduations
      DrawGraduations();
      // réticule
      DrawReticule();
      // valeur
      DrawValeur();
      // bulle de niveau
      DrawBulle();
      FTmpBuffer.Draw(vue.Canvas, 0, 0, True);

    except;
      ;;
    end;
    FDrawInProcess := false;
  finally
    FreeAndNil(FTmpBuffer);
    FDrawInProcess := False;
  end;
end;
end.

