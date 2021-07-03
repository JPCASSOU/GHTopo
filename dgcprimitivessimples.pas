unit DGCPrimitivesSimples;
// Définition des classes pour les primitives simples de dessin
{$mode delphiunicode}

interface

uses
  DGCTypes,
  DGCUtilityFunctions,
  DGCAncestorPrimitives,
  Classes, SysUtils, Graphics, math;

// Le groupe d'objets
type TDGCBeginGroupeSVG = class
  private
    FName: string;
    procedure SetBoundingBox();
  public
    constructor Create(const QName: string);
    property Name: string read FName;
end;

type TDGCEndGroupeSVG = class
  private
    FName: string;
  public
    constructor Create(const QName: string);
    property Name: string read FName;
end;

// Le segment
type TDGCSegment = class(TDGCPrimitive)
  private
    FP1: TDGCPoint2D;
    FP2: TDGCPoint2D;
    FBoundingBox: TDGCBoundingBox;
  public
    constructor Create(const QIdxStyleSheet: integer; const P1, P2: TDGCPoint2D; const QName: string = '');
    property    P1: TDGCPoint2D read FP1 write FP1;
    property    P2: TDGCPoint2D read FP2 write FP2;
    procedure   SetBoundingBox(); override;
    function    GetBoundingBox(): TDGCBoundingBox;
end;

// La ligne infinie
type TDGCInfiniteLine = class(TDGCPrimitive)
  private
    FOrientation : TInfiniteLineOrientation;
    FPositionCentre: TDGCPoint2D;
  public
    constructor Create(const QIdxStyleSheet: integer; const QOrientation: TInfiniteLineOrientation; const QPositionCentre: TDGCPoint2D; const QName: string = '');
    property    Orientation    : TInfiniteLineOrientation read FOrientation write FOrientation;
    property    PositionCentre : TDGCPoint2D read FPositionCentre write FPositionCentre;
end;

// Le rectangle
type TDGCRectangle = class(TDGCPrimitive)
  private
    FP1: TDGCPoint2D;
    FP2: TDGCPoint2D;
    FBoundingBox: TDGCBoundingBox;
  public
    constructor Create(const QIdxStyleSheet: integer; const P1, P2: TDGCPoint2D; const QName: string = '');
    property    P1: TDGCPoint2D read FP1 write FP1;
    property    P2: TDGCPoint2D read FP2 write FP2;
    procedure   SetBoundingBox(); override;
    function    GetBoundingBox(): TDGCBoundingBox;
end;

// L'ellipse
type TDGCEllipse = class(TDGCPrimitive)
  private
    FCentre: TDGCPoint2D;
    FRayon1: Double;
    FRayon2: Double;
    FBoundingBox: TDGCBoundingBox;
  public
    constructor Create(const QIdxStyleSheet: integer; const QCentre: TDGCPoint2D; const QR1, QR2: Double; const QName: string = '');
    property    Centre: TDGCPoint2D  read FCentre write FCentre;
    property    Rayon1: Double read FRayon1 write FRayon1;
    property    Rayon2: Double read FRayon2 write FRayon2;
    procedure   SetBoundingBox(); override;
    function    GetBoundingBox(): TDGCBoundingBox;

end;

// Le triangle
type  TDGCTriangle = class(TDGCPrimitive)
  private
    FBoundingBox: TDGCBoundingBox;
    FP1: TDGCPoint2D;
    FP2: TDGCPoint2D;
    FP3: TDGCPoint2D;
  public
    constructor Create(const QIdxStyleSheet: integer; const P1, P2, P3: TDGCPoint2D; const QName: string = '');
    property    P1: TDGCPoint2D read FP1 write FP1;
    property    P2: TDGCPoint2D read FP2 write FP2;
    property    P3: TDGCPoint2D read FP3 write FP3;
    procedure   SetBoundingBox(); override;
    function    GetBoundingBox(): TDGCBoundingBox;
end;
// Le texte
type

{ TDGCText }

 TDGCText = class(TDGCPrimitive)
  private
    FPosition    : TDGCPoint2D;
    FAlignment   : byte; // point d'accrochage aka clavier téléphonique
    FOrientation : integer; // angle de rotation en décidegrés
    FText        : string;
  public
    constructor Create(const QIdxStyleSheet: integer; const P: TDGCPoint2D; const A: byte; const O: double; const S: string; const QName: string = '');
    property    Position     : TDGCPoint2D   read FPosition write FPosition;
    property    Alignment    : byte     read FAlignment write FAlignment;
    property    Orientation  : integer  read FOrientation write FOrientation;
    property    Text         : string read FText    write FText   ;
    procedure   SetBoundingBox(); override;
    function    GetBoundingBox(): TDGCBoundingBox;
end;


implementation
uses
  DGCDummyUnit; // pour éviter le bug de 'Fin de code source non trouvée
//******************************************************************************
//* Primitives simples                                                         *
//******************************************************************************
// Le début de groupe
constructor TDGCBeginGroupeSVG.Create(const QName: string);
begin
  inherited Create;
  FName := QName;
end;
procedure TDGCBeginGroupeSVG.SetBoundingBox();
begin
  nop;
end;
// La fin de groupe
constructor TDGCEndGroupeSVG.Create(const QName: string);
begin
  inherited Create;
  FName := QName;
end;



//------------------------------------------------------------------------------
// Le segment
constructor TDGCSegment.Create(const QIdxStyleSheet: integer; const P1, P2: TDGCPoint2D; const QName: string = '');
begin
  inherited Create(QIdxStyleSheet, QName);
  FP1 := P1;
  FP2 := P2;

  self.SetBoundingBox();
end;
procedure TDGCSegment.SetBoundingBox();
begin
  FBoundingBox.setFrom(self.FP1.X, self.FP1.Y, self.FP2.X, self.FP2.Y);
end;

function TDGCSegment.GetBoundingBox(): TDGCBoundingBox;
begin
  result := FBoundingBox;
end;
//------------------------------------------------------------------------------
// La ligne infinie
constructor TDGCInfiniteLine.Create(const QIdxStyleSheet: integer; const QOrientation: TInfiniteLineOrientation; const QPositionCentre: TDGCPoint2D; const QName: string = '');
begin
  inherited Create(QIdxStyleSheet, QName);
  FOrientation    := QOrientation;
  FPositionCentre := QPositionCentre;
end;

//------------------------------------------------------------------------------
// Le rectangle
constructor TDGCRectangle.Create(const QIdxStyleSheet: integer; const P1, P2: TDGCPoint2D; const QName: string = '');
begin
  inherited Create(QIdxStyleSheet, QName);
  FP1 := P1;
  FP2 := P2;
  self.SetBoundingBox();
end;

procedure TDGCRectangle.SetBoundingBox();
begin
  FBoundingBox.setFrom(self.FP1.X, self.FP1.Y, self.FP2.X, self.FP2.Y);
end;

function TDGCRectangle.GetBoundingBox(): TDGCBoundingBox;
begin
  Result := FBoundingBox;
end;

//------------------------------------------------------------------------------
// L'ellipse
constructor TDGCEllipse.Create(const QIdxStyleSheet: integer; const QCentre: TDGCPoint2D; const QR1, QR2: Double; const QName: string = '');
begin
  inherited Create(QIdxStyleSheet, QName);
  FCentre := QCentre;
  FRayon1 := QR1;
  FRayon2 := QR2;
  SetBoundingBox();
end;

procedure TDGCEllipse.SetBoundingBox();
var
  C1, C2: TDGCPoint2D;
begin
  C1.setFrom(FCentre.X - FRayon1, FCentre.Y - FRayon2);
  C2.setFrom(FCentre.X + FRayon1, FCentre.Y + FRayon2);
  FBoundingBox.setFrom(C1.X, C1.Y, C2.X, C2.Y);
end;
function TDGCEllipse.GetBoundingBox(): TDGCBoundingBox;
begin
  result := self.FBoundingBox;
end;

//------------------------------------------------------------------------------
// Le triangle
constructor TDGCTriangle.Create(const QIdxStyleSheet: integer; const P1, P2, P3: TDGCPoint2D; const QName: string = '');
begin
  inherited Create(QIdxStyleSheet, QName);
  FP1 := P1;
  FP2 := P2;
  FP3 := P3;
  SetBoundingBox();
end;
procedure TDGCTriangle.SetBoundingBox();
begin
  FBoundingBox.Reset();
  FBoundingBox.upDate(FP1);
  FBoundingBox.upDate(FP2);
  FBoundingBox.upDate(FP3);
end;


function TDGCTriangle.GetBoundingBox(): TDGCBoundingBox;
begin
  result := self.FBoundingBox;
end;
//------------------------------------------------------------------------------
// Le texte
// Orientation en degrés
constructor TDGCText.Create(const QIdxStyleSheet: integer; const P: TDGCPoint2D; const A: byte; const O: double; const S: string; const QName: string = '');
begin
  inherited Create(QIdxStyleSheet, QName);
  FPosition    := P;
  FAlignment   := A;
  FOrientation := Trunc(O * 10);
  FText        := S;
end;

procedure TDGCText.SetBoundingBox();
begin
  nop;
end;

function TDGCText.GetBoundingBox(): TDGCBoundingBox;
begin
  Result:= self.FBoundingBox;
end;

end.

