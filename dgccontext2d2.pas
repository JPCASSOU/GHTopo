unit dgccontext2d2;

// Composant lpk de dessin
// KO pour le moment (évenement Invalidate jamais appelé)
{$ERROR Développement du mode composant suspendu (très difficile)}

{$mode delphiunicode}

interface

uses
  DGCTypes,
  DGCUtilityFunctions,
  DGCStylesPenBrushFont,
  DGCUnitListeAffichage, DGCUnitListeStylesObjets,
  DGCPrimitivesSimples, DGCPrimitivesComplexes,
  BGRABitmap,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, math;

type

{ TDGCContext2D }

 TDGCContext2D = class(TPaintBox)
  strict private
    FRx, FRy: double;
    FAttenduSecondPoint: boolean; // pour pan et zoom
    GetCoordsMonde: TFunctionGetCoordsMonde;
    GetCoordsPlan : TFunctionGetCoordsPlan;

    // position courante
    FCurrentMousePos: TDGCPoint2D;
    // gestion des zooms
    FZC1, FZC2    : TDGCPoint2D;
    FZP1, FZP2    : TPoint;
    FRegionCMini  : TDGCPoint2D;
    FRegionCMaxi  : TDGCPoint2D;
    // paramètres de vues
    FRappHLVue      : double;
    FRappScrReal    : double;
    FInvRappScrReal : double;
    // flags
    FDessinReady          : boolean; // le dessin est prêt ?
    FDessinEnCours        : boolean; // Le redessin est toujours en cours ?  (pour éviter pbs liés à la réentrance)
    FCurveFilled          : Boolean; // Init facultatif, valeur définie par BeginCurve() via un paramètre obligatoire
    FTemporaryPolyLineGon : TDGCPolyLineGon;
    FMakingPolyLineGon    : boolean; // BeginPolygon() appelé ?
    // Drapeaux divers:

    // liste des objets à tracer (liste d'affichage)
    FListOfEntities: TDGCListeAffichage;
    // Liste des styles
    FListOfStyles  : TDGCListeStylesSheets;
    FCurrentStyleSheet : integer;

    procedure Flush();

    procedure QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
    procedure Rearmer();
    procedure RedessinVue();


    // dessin d'un réticule

  private
    FLastError: string;
    FTmpBuffer: TBGRABitmap;
    //------------------------------------------------------------
    // paramètres publiés
    FIsOrthonormal    : boolean;                   // repère orthonormal ?
    FBackGroundColor  : TColor;                //couleur de fond
    FBackGroundOpacity: byte;

    // étendue originale (publiée via)
    FOriginalCoinBasGauche: TDGCPoint2D;
    FOriginalCoinHautDroit: TDGCPoint2D;

    FDessinOptions    : TDGCOptions;           // options de dessin: réticules, etc ...
    FModeTravail      : TDGCModesTravail;      // modes de travail

    //------------------------------------------------------------
    // fonctions de conversion



    //------------------------------------------------------------
    // setters publiés
    procedure SetBackGroundColor(AValue: TColor);
    procedure SetBackGroundOpacity(AValue: byte);
    procedure SetDessinOptions(AValue: TDGCOptions);

    procedure SetOriginalCoinHautDroit(const P: TDGCPoint2D);
    procedure SetOriginalCoinBasGauche(const P: TDGCPoint2D);
    procedure SetIsOrthonormal(AValue: boolean);

    procedure SetXMax(AValue: double);
    procedure SetXMin(AValue: double);
    procedure SetYMax(AValue: double);
    procedure SetYMin(AValue: double);

    //--------------------------------------------------------------------------
    function GetCoordsMondeOrthonormal(const PP: TPoint): TDGCPoint2D;
    function GetCoordsPlanOrthonormal(const PM: TDGCPoint2D): TPoint;
    function GetCoordsMondeNonOrthonormal(const PP: TPoint): TDGCPoint2D;
    function GetCoordsPlanNonOrthonormal(const PM: TDGCPoint2D): TPoint;
    function GetRYMaxi(): double;



  protected
    procedure Paint; override;

  public

    constructor Create(AOwner: TComponent); override;
    function GetLastError(): string;
    procedure SetModeTravail(const MT: TDGCModesTravail);
    function GetModeTravail(): TDGCModesTravail;
    function GetCurrentPosition(): TDGCPoint2D;



    function GetCoordonneesMonde(const X, Y: integer): TDGCPoint2D;
    procedure SetViewLimits(const X1, Y1, X2, Y2: double; const DebugTag: string = '');
    procedure ResetVue(const DoRefresh: boolean);

    // feuilles de styles
    function  GetNbStyleSheets(): integer;
    function  GetStyleSheet(const Idx: integer): TDGCStyleSheet;
    procedure PutStyleSheet(const Idx: integer; const FS: TDGCStyleSheet);
    function MakeTDGCStyleSheet(const QStylename: string;
                                const QPenColor: TColor;
                                const QPenOpacity: byte;
                                const QPenStyle: TPenStyle;
                                const QPenWidth: byte;
                                const QBrushColor: TColor;
                                const QBrushOpacity: byte;
                                const QBrushStyle: TBrushStyle;
                                const QFontName: string;
                                const QFontColor: TColor;
                                const QFontOpacity: byte;
                                const QFontSizeInPts: integer;
                                const QFontSizeInMM: double;
                                const QFontStyle: TFontStyles;
                                const QDescription: string = ''): TDGCStyleSheet;
    procedure AddStyleSheet(const OS: TDGCStyleSheet); overload;
    procedure AddStyleSheet(const QStylename        : string;// Crayon
                             const QPenColor         : TColor;
                             const QPenOpacity       : byte;
                             const QPenStyle         : TPenStyle;
                             const QPenWidth         : byte;
                             const QBrushColor       : TColor;
                             const QBrushOpacity     : byte;
                             const QBrushStyle       : TBrushStyle;
                             const QFontName         : string;
                             const QFontColor        : TColor;
                             const QFontOpacity      : byte;
                             const QFontSizeInPts: integer;
                             const QFontSizeInMM: double;
                             const QFontStyle        : TFontStyles;
                             const QDescription       : string = ''); overload;
    // Liste d'affichage
    function GetNbEntities(): integer;





    // définition des styles de crayon et brosses
    procedure SetPenColorStyle(const C: TColor; const QOpacity: byte = 255; const QWidth: integer = 0; const QStyle: TPenStyle = psSolid);
    procedure SetBrushColorStyle(const C: TColor; const QOpacity: byte = 255; const QStyle: TBrushStyle = bsSolid);
    procedure SetFontColorStyle(const QFontName:string; const C: TColor; const QOpacity: byte; const QHeight: byte; const QStyle: TFontStyles);
    procedure SetFontName(const QFontName: string);
    procedure SetFontColor(const C: TColor; const QOpacity: byte = 255);
    procedure SetFontHeight(const QHeightInPoints: byte)  ; overload;    // en points
    procedure SetFontHeight(const QHeightInMeters: double); overload;    // en mètres
    procedure SetFontStyle(const QStyle: TFontStyles);



    procedure ClearDrawing();
     // pour le support ultérieur des transformation affines
    procedure dgcLoadIdentity();
    function  dgcMakeTransformationMatrix(const AnglerotInDegrees: double;
                                          const Translation: TDGCPoint2D;
                                          const Scaling    : TDGCPoint2D): TDGCMatrix3x3;
    procedure dgcPushMatrix(const M: TDGCMatrix3x3);  // similaire au glPopMatrix
    function  dgcPopMatrix(): TDGCMatrix3x3;
  published
    property XMin : double read FOriginalCoinBasGauche.X write SetXMin; // default -100.00;
    property YMin : double read FOriginalCoinBasGauche.Y write SetYMin; // default -100.00;
    property XMax : double read FOriginalCoinHautDroit.X write SetXMax; // default  100.00;
    property YMax : double read FOriginalCoinHautDroit.Y write SetYMax; // default  100.00;

    property IsOrthonormal    : boolean read FIsOrthonormal     write SetIsOrthonormal     default True;
    property BackGroundColor  : TColor  read FBackGroundColor   write SetBackGroundColor   default clWhite;
    property BackGroundOpacity: byte    read FBackGroundOpacity write SetBackGroundOpacity default 255;

    property DessinOptions: TDGCOptions read FDessinOptions write SetDessinOptions;
    //property ModeTravail  : TDGCModesTravail read FModeTravail write SetModeTravail;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GHComposants',[TDGCContext2D]);
end;

{ TDGCContext2D }
constructor TDGCContext2D.Create(AOwner: TComponent);
// *** construction du composant ***
begin
  inherited Create(AOwner); // on hérite
  // drapeaux
  FDessinReady        := false;      // le dessin est prêt ?
  FDessinEnCours      := False;      // Le redessin est toujours en cours ?  (pour éviter pbs liés à la réentrance)
  FMakingPolyLineGon  := false;      // BeginPolygon() appelé ?
  FCurveFilled        := false;
  FAttenduSecondPoint := false;
  FLastError := 'Drapeaux armés';
  SetIsOrthonormal(True);
  FLastError := 'Fonctions de conversion';
  if (FIsOrthonormal) then
  begin
    GetCoordsMonde := GetCoordsMondeOrthonormal;
    GetCoordsPlan  := GetCoordsPlanOrthonormal;
    FLastError += 'orthonormales ';
  end else
  begin
    GetCoordsMonde := GetCoordsMondeNonOrthonormal;
    GetCoordsPlan  := GetCoordsPlanNonOrthonormal;
    FLastError += 'non orthogonales ';
  end;
  FLastError += 'définies';
  SetDessinOptions([dgcoX_AXIS, dgcoY_AXIS]);
  SetModeTravail(mtgcsREADY);
  SetBackGroundColor(clWhite);
  SetBackGroundOpacity(255);
  SetXMin(-110.00);
  SetYMin(-120.00);
  SetXMax(130.00);
  SetYMax(140.00);
  FLastError := Format('Zone de dessin définie: %f .. %f ->  %f .. <%f',
                       [FOriginalCoinBasGauche.X, FOriginalCoinBasGauche.Y, FOriginalCoinHautDroit.X, FOriginalCoinHautDroit.Y]);
  FListOfEntities := TDGCListeAffichage.Create;
  FLastError      := 'Liste d''affichage créée';
  FListOfStyles   := TDGCListeStylesSheets.Create;
  FLastError      := 'Feuilles de style créées';
  //try
    FListOfEntities.ClearListe();
    FLastError := 'Liste d''affichage purgée';
    FListOfStyles.ClearListe(True);
    FLastError := Format('Feuilles de style purgées: %d', [GetNbStyleSheets()]);
    FCurrentStyleSheet := 0;
    //self.ResetVue(false);
    //Label1.Caption:= format('%f, %f -> %f, %f', [FRegionCMini.X, FRegionCMini.Y, FRegionCMaxi.X, FRegionCMaxi.Y]);
    FLastError := Format('Prêt à appeler %s.Invalidate', [classname]);
    FDessinReady := True;
    self.Invalidate;
  //except

  //end;

end;

procedure TDGCContext2D.SetViewLimits(const X1, Y1, X2, Y2: double; const DebugTag: string = '');
const
  Epsilon = 1e-2;
var
  qX1, qX2, qY1, qY2: double;
  d1, d2: double;
begin
  qX1 := Min(X1, X2);
  qY1 := Min(Y1, Y2);
  qX2 := Max(X1, X2);
  qY2 := Max(Y1, Y2);

  d1 := qX2 - qX1;
  d2 := qY2 - qY1;
  // si zone trop étroite, abandonner
  if (Abs(d1) < Epsilon) or (Abs(d2) < Epsilon) then Exit;
  FRegionCMini := MakeTDGCPoint2D(qX1, qY1);
  FRegionCMaxi := MakeTDGCPoint2D(qX2, qY2);
  if (FIsOrthonormal) then
  begin
    FRegionCMaxi.Y := GetRYMaxi();
    FRx := -1.0;
    FRy := -1.0;
  end else
  begin
    FRx   := 1 / (FRegionCMaxi.X - FRegionCMini.X);
    FRy   := 1 / (FRegionCMaxi.Y - FRegionCMini.Y);
  end;
  FLastError := Format('Est passé dans %s.SetViewLimits(%f, %f, %f, %f)', [Classname,
                        FRegionCMini.X, FRegionCMini.Y, FRegionCMaxi.X, FRegionCMaxi.Y]);
end;
procedure TDGCContext2D.ResetVue(const DoRefresh: boolean);
begin
  self.SetViewLimits(FOriginalCoinBasGauche.X, FOriginalCoinBasGauche.Y,
                     FOriginalCoinHautDroit.X, FOriginalCoinHautDroit.Y,
                     '');
  FLastError := Format('Est passé dans %s.ResetVue(%f, %f, %f, %f)', [Classname,
                        FOriginalCoinBasGauche.X, FOriginalCoinBasGauche.Y, FOriginalCoinHautDroit.X, FOriginalCoinHautDroit.Y]);
  if (DoRefresh) then self.Invalidate;

end;

//* Setters de propriétés publiées *********************************************

procedure TDGCContext2D.SetIsOrthonormal(AValue: boolean);
begin
  if FIsOrthonormal=AValue then Exit;
  FIsOrthonormal:=AValue;
  self.Invalidate;
end;

procedure TDGCContext2D.SetXMin(AValue: double);
begin
  FOriginalCoinBasGauche.X := AValue;
  SetViewLimits(FOriginalCoinBasGauche.X, FOriginalCoinBasGauche.Y, FOriginalCoinHautDroit.X, FOriginalCoinHautDroit.X);
  //self.Invalidate;
end;
procedure TDGCContext2D.SetXMax(AValue: double);
begin
  FOriginalCoinHautDroit.X := AValue;
  SetViewLimits(FOriginalCoinBasGauche.X, FOriginalCoinBasGauche.Y, FOriginalCoinHautDroit.X, FOriginalCoinHautDroit.X);
  //self.Invalidate;
end;

procedure TDGCContext2D.SetYMin(AValue: double);
begin
  FOriginalCoinBasGauche.Y := AValue;
  SetViewLimits(FOriginalCoinBasGauche.X, FOriginalCoinBasGauche.Y, FOriginalCoinHautDroit.X, FOriginalCoinHautDroit.X);
end;

procedure TDGCContext2D.SetYMax(AValue: double);
begin
  FOriginalCoinHautDroit.Y := AValue;
  SetViewLimits(FOriginalCoinBasGauche.X, FOriginalCoinBasGauche.Y, FOriginalCoinHautDroit.X, FOriginalCoinHautDroit.X);
end;



procedure TDGCContext2D.SetBackGroundOpacity(AValue: byte);
begin
  if FBackGroundOpacity=AValue then Exit;
  FBackGroundOpacity:=AValue;
end;

procedure TDGCContext2D.SetDessinOptions(AValue: TDGCOptions);
begin
  if FDessinOptions=AValue then Exit;
  FDessinOptions:=AValue;
end;


procedure TDGCContext2D.SetOriginalCoinHautDroit(const P: TDGCPoint2D);
begin
  FOriginalCoinHautDroit := P;
end;

procedure TDGCContext2D.SetOriginalCoinBasGauche(const P: TDGCPoint2D);
begin
  FOriginalCoinBasGauche := P;
end;



procedure TDGCContext2D.SetBackGroundColor(AValue: TColor);
begin
  if FBackGroundColor=AValue then Exit;
  FBackGroundColor:=AValue;
end;
//******************************************************************************
procedure TDGCContext2D.SetModeTravail(const MT: TDGCModesTravail);
begin
  if (MT = FModeTravail) then Exit; // ne rien faire si le mode de travail est inchangé
  FModeTravail := MT;
  case FModeTravail of
    mtgcsREADY: Rearmer();
    mtgcsPAN_PREMIER_POINT: // pan et distance
    begin
      FZC1 := FCurrentMousePos;
      FZC2 := FCurrentMousePos;
      FAttenduSecondPoint := false;
      QSetPen(pmCopy, psSolid, 0, clGray);
    end;
    mtgcsPAN_SECOND_POINT:
    begin
      FAttenduSecondPoint := true;
      QSetPen(pmNotXor, psSolid, 0, clGray);
    end;
    mtgcsZOOM_PREMIER_COIN:
    begin
      FZC1 := FCurrentMousePos;
      FZC2 := FCurrentMousePos;
      FAttenduSecondPoint := false;
      QSetPen(pmCopy, psSolid, 0, clGray);
    end;
    mtgcsZOOM_SECOND_COIN:
    begin
      FAttenduSecondPoint := true;
      QSetPen(pmNotXor, psSolid, 0, clGray);
    end;
  else
    nop;
  end;
end;

function TDGCContext2D.GetModeTravail(): TDGCModesTravail;
begin
  Result := FModeTravail;
end;

function TDGCContext2D.GetCurrentPosition(): TDGCPoint2D;
begin
  Result := FCurrentMousePos;
end;




//******************************************************************************
function TDGCContext2D.GetCoordsMondeOrthonormal(const PP: TPoint): TDGCPoint2D;
begin
  Result.X :=  FInvRappScrReal * PP.X + FRegionCMini.X;
  Result.Y := -FInvRappScrReal * PP.Y + FRegionCMaxi.Y;
end;
function TDGCContext2D.GetCoordsPlanOrthonormal(const PM: TDGCPoint2D): TPoint;
begin
  Result.X := Round((PM.X - FRegionCMini.X) * FRappScrReal);
  Result.Y := Round((FRegionCMaxi.Y - PM.Y) * FRappScrReal);
end;

function TDGCContext2D.GetCoordsMondeNonOrthonormal(const PP: TPoint): TDGCPoint2D;
var
  rx, ry: Double;
begin
  rx := (FRegionCMaxi.X - FRegionCMini.X) / self.Width;
  ry := (FRegionCMaxi.Y - FRegionCMini.Y) / self.Height;

  Result.X := FRegionCMini.X + rx * PP.X;         // OK
  Result.Y := FRegionCMini.Y + ry * (self.Height - PP.Y);
end;

function TDGCContext2D.GetCoordsPlanNonOrthonormal(const PM: TDGCPoint2D): TPoint;
begin
  Result.x := round((Self.Width) * (PM.X - FRegionCMini.X) * FRx);
  Result.y := Self.Height -
              (trunc(Self.Height * (PM.Y - FRegionCMini.Y) * FRy));
end;
// cette fonction retourne d'autres paramètres
function TDGCContext2D.GetRYMaxi(): double;
var
  qdx: Double;
begin
  qdx := FRegionCMaxi.X - FRegionCMini.X;
  if (0 = Self.Width) then
  begin
    FRappHLVue := 1.0;
    FRappScrReal := 1.0;
  end else
  begin
    FRappHLVue := Self.Height / Self.Width;         // calcul du rapport Hauteur/largeur de Self
    FRappScrReal := Self.Width / qdx;               // calcul du rapport Ecran/Réel
  end;
  FInvRappScrReal := 1 / FRappScrReal;
  Result := FRegionCMini.Y + qdx * FRappHLVue;    // calcul de la hauteur de visualisation
end;

procedure TDGCContext2D.Paint;
var
  R: TRect;
  procedure QDrawAxisX(const Y: double);
  var
    PP1, PP2: TPoint;
  begin
    PP1 := GetCoordsPlan(MakeTDGCPoint2D(FRegionCMini.X, Y));
    PP2 := GetCoordsPlan(MakeTDGCPoint2D(FRegionCMaxi.X, Y));
    FTmpBuffer.CanvasBGRA.MoveTo(PP1);
    FTmpBuffer.CanvasBGRA.LineTo(PP2);
  end;
  procedure QDrawAxisY(const X: double);
  var
    PP1, PP2: TPoint;
  begin
    PP1 := GetCoordsPlan(MakeTDGCPoint2D(X, FRegionCMini.Y));
    PP2 := GetCoordsPlan(MakeTDGCPoint2D(X, FRegionCMaxi.Y));
    FTmpBuffer.CanvasBGRA.MoveTo(PP1);
    FTmpBuffer.CanvasBGRA.LineTo(PP2);
  end;
begin
  // En mode Exécution, ne passe JAMAIS ici, alors qu'il le devrait
  FLastError := Format('Passe dans l''événement %s.Paint', [classname]);
  if (csDesigning in ComponentState) then
  begin
    self.Canvas.Brush.Color := FBackGroundColor;
    FTmpBuffer := TBGRABitmap.Create(Self.Width, Self.Height);
    try
      R.Left   := 0;
      R.Top    := 0;
      R.Bottom := R.Top  + Self.Height;
      R.Right  := R.Left + Self.Width;


      FTmpBuffer.CanvasBGRA.Brush.Color   := FBackGroundColor;
      FTmpBuffer.CanvasBGRA.Brush.Style   := bsSolid;
      FTmpBuffer.CanvasBGRA.Brush.Opacity := FBackGroundOpacity;
      FTmpBuffer.CanvasBGRA.FillRect(R);

      FTmpBuffer.CanvasBGRA.Pen.Width := 0;
      if ((dgcoX_AXIS in self.FDessinOptions) OR
          (dgcoY_AXIS in self.FDessinOptions)) then
      begin
        FTmpBuffer.CanvasBGRA.Pen.Style := psDashDot;
        FTmpBuffer.CanvasBGRA.Pen.Color := clGray;
        if (dgcoX_AXIS in self.FDessinOptions) then QDrawAxisX(0.00);
        if (dgcoY_AXIS in self.FDessinOptions) then QDrawAxisY(0.00);
      end;
      FTmpBuffer.CanvasBGRA.Pen.Style := psSolid;
      FTmpBuffer.CanvasBGRA.Pen.Color := clBlack;
      FTmpBuffer.Draw(self.Canvas, 0, 0, True);
    finally
      FreeAndNil(FTmpBuffer);
    end;
    (*
    with self.Canvas do
    begin
      Pen.Style := psDash;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(0, 0, Self.Width - 1, Self.Height - 1);
      //Line(0,0,Self.Width-1,Self.Height-1);
      //Line(Self.Width-1,0,0,Self.Height-1);
    end;  //*)
    exit;
  end;
  FLastError := 'Paint() en mode exécution';
  if (Assigned(OnPaint)) then
  begin
    self.Canvas.Font := self.Font;
    self.Canvas.Brush.Color := self.Color;
    // On dessine ici
    FLastError := 'Prêt à dessiner';
    //RedessinVue();
    inherited Paint;
  end
  else
  begin
    FLastError := 'Evénement OnPaint du composant non assigné'
  end;
end;

//******************************************************************************
// spécifiques aux zooms et consorts
procedure TDGCContext2D.QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
begin
  self.Canvas.Pen.Mode := QPM;
  self.Canvas.Pen.Style:= QPS;
  self.Canvas.Pen.Color:= QPC;
  self.Canvas.Pen.Width:= QPW;
end;
procedure TDGCContext2D.Rearmer();
begin
  FAttenduSecondPoint        := False;
  QSetPen(pmCopy, psSolid, 0, clGray);
end;
// Rafraichit l'affichage
procedure TDGCContext2D.Flush();
begin
  self.Invalidate;
end;

function TDGCContext2D.GetLastError(): string;
begin
  Result := FLastError;
end;
//******************************************************************************
procedure TDGCContext2D.ClearDrawing();
begin
  FLastError:= 'Passe dans ClearDrawing';
  FListOfStyles.ClearListe(True);
  FLastError:= 'Passe dans ClearDrawing 1';
  FListOfEntities.ClearListe();
  FLastError:= 'Passe dans ClearDrawing 2';
  ResetVue(True);
  FLastError:= 'Passe dans ClearDrawing 3';
end;

//******************************************************************************
procedure TDGCContext2D.dgcLoadIdentity();
begin

end;

function TDGCContext2D.dgcMakeTransformationMatrix(const AnglerotInDegrees: double; const Translation: TDGCPoint2D; const Scaling: TDGCPoint2D): TDGCMatrix3x3;
begin

end;

procedure TDGCContext2D.dgcPushMatrix(const M: TDGCMatrix3x3);
begin

end;

function TDGCContext2D.dgcPopMatrix(): TDGCMatrix3x3;
begin

end;

function TDGCContext2D.GetCoordonneesMonde(const X, Y: integer): TDGCPoint2D;
begin
  Result := GetCoordsMonde(Point(X, Y));
end;

//******************************************************************************
//* Feuilles de Styles                                                         *
//******************************************************************************
function TDGCContext2D.GetNbStyleSheets(): integer;
begin
  Result := FListOfStyles.GetNbElements();
end;

function TDGCContext2D.GetStyleSheet(const Idx: integer): TDGCStyleSheet;
begin
  Result := FListOfStyles.GetElement(Idx);
end;

procedure TDGCContext2D.PutStyleSheet(const Idx: integer; const FS: TDGCStyleSheet);
begin
  FListOfStyles.PutElement(Idx, FS);
end;

function  TDGCContext2D.MakeTDGCStyleSheet(const QStylename: string;
                                                   const QPenColor: TColor;
                                                   const QPenOpacity: byte;
                                                   const QPenStyle: TPenStyle;
                                                   const QPenWidth: byte;
                                                   const QBrushColor: TColor;
                                                   const QBrushOpacity: byte;
                                                   const QBrushStyle: TBrushStyle;
                                                   const QFontName: string;
                                                   const QFontColor: TColor;
                                                   const QFontOpacity: byte;
                                                   const QFontSizeInPts: integer;
                                                   const QFontSizeInMM: double;
                                                   const QFontStyle: TFontStyles;
                                                   const QDescription: string = ''): TDGCStyleSheet;
begin
  with Result do
  begin
    Stylename      := QStylename;

    PenColor       := QPenColor;
    PenOpacity     := QPenOpacity;
    PenStyle       := QPenStyle;
    PenWidth       := QPenWidth;

    BrushColor     := QBrushColor;
    BrushOpacity   := QBrushOpacity;
    BrushStyle     := QBrushStyle;

    FontName       := QFontName;
    FontColor      := QFontColor;
    FontOpacity    := QFontOpacity;
    FontSizeInPts  := QFontSizeInPts;
    FontSizeInMM   := QFontSizeInMM;
    FontStyle      := QFontStyle;


    Description    := QDescription;
  end;
  Result.CSSStyle  := MakeCSSStyle(Result);
end;
procedure TDGCContext2D.AddStyleSheet(const OS: TDGCStyleSheet);
begin
  FListOfStyles.AddElement(OS);
end;

procedure TDGCContext2D.AddStyleSheet(const QStylename: string;
                                               const QPenColor: TColor;
                                               const QPenOpacity: byte;
                                               const QPenStyle: TPenStyle;
                                               const QPenWidth: byte;
                                               const QBrushColor: TColor;
                                               const QBrushOpacity: byte;
                                               const QBrushStyle: TBrushStyle;
                                               const QFontName: string;
                                               const QFontColor: TColor;
                                               const QFontOpacity: byte;
                                               const QFontSizeInPts: integer;
                                               const QFontSizeInMM: double;
                                               const QFontStyle: TFontStyles;
                                               const QDescription: string = '');
var
  MyStyle: TDGCStyleSheet;
begin
  with MyStyle do
  begin
    Stylename      := QStylename;
    PenColor       := QPenColor;
    PenOpacity     := QPenOpacity;
    PenStyle       := QPenStyle;
    PenWidth       := QPenWidth;

    BrushColor     := QBrushColor;
    BrushOpacity   := QBrushOpacity;
    BrushStyle     := QBrushStyle;

    FontName       := QFontName;
    FontColor      := QFontColor;
    FontOpacity    := QFontOpacity;
    FontSizeInPts  := QFontSizeInPts;
    FontSizeInMM   := QFontSizeInMM;
    FontStyle      := QFontStyle;
    Description    := QDescription;
  end;
  MyStyle.CSSStyle := MakeCSSStyle(MyStyle);
  FListOfStyles.AddElement(MyStyle);
end;

//******************************************************************************
//* Définition de crayon, brosse et fontes sans passer par les feuilles de style
//******************************************************************************
procedure TDGCContext2D.SetPenColorStyle(const C: TColor; const QOpacity: byte; const QWidth: integer; const QStyle: TPenStyle);
var
  MyPen: TDGCPen;
begin
  MyPen := TDGCPen.Create;
  MyPen.Couleur := C;
  MyPen.Opacity := QOpacity;
  MyPen.Width   := QWidth;
  MyPen.Style   := QStyle;
  FListOfEntities.AddElement(tdoCMD_SET_PEN, '', MyPen);
end;

procedure TDGCContext2D.SetBrushColorStyle(const C: TColor; const QOpacity: byte; const QStyle: TBrushStyle = bsSolid);
var
  MyBrush: TDGCBrush;
begin
  MyBrush := TDGCBrush.Create;
  MyBrush.Couleur  := C;
  MyBrush.Opacity  := QOpacity;
  MyBrush.Style    := QStyle;
  FListOfEntities.AddElement(tdoCMD_SET_BRUSH, '', MyBrush);
end;

procedure TDGCContext2D.SetFontColorStyle(const QFontName:string; const C: TColor; const QOpacity: byte; const QHeight: byte; const QStyle: TFontStyles);
var
  MyFont: TDGCFont;
begin
  MyFont := TDGCFont.Create;
  MyFont.FontName := QFontName;
  MyFont.Couleur  := C;
  MyFont.Opacity  := QOpacity;
  MyFont.Height   := QHeight;
  MyFont.Style    := QStyle;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TDGCContext2D.SetFontName(const QFontName: string);
var
  MyFont: TDGCFont;
begin
  MyFont := TDGCFont.Create;
  MyFont.FontName := QFontName;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TDGCContext2D.SetFontColor(const C: TColor; const QOpacity: byte = 255);
var
  MyFont: TDGCFont;
begin
  MyFont := TDGCFont.Create;
  MyFont.Couleur  := C;
  MyFont.Opacity  := QOpacity;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TDGCContext2D.SetFontHeight(const QHeightInPoints: byte);
var
  MyFont: TDGCFont;
begin
  MyFont := TDGCFont.Create;
  MyFont.Height   := QHeightInPoints;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

procedure TDGCContext2D.SetFontHeight(const QHeightInMeters: double);
var
  MyFont: TDGCFont;
  PP0, PP1: TPoint;
begin
  PP0 := GetCoordsPlan(MakeTDGCPoint2D(0.0, 0.0));
  PP1 := GetCoordsPlan(MakeTDGCPoint2D(QHeightInMeters, QHeightInMeters));

  MyFont := TDGCFont.Create;
  MyFont.Height   := PP1.y - PP0.y;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;


procedure TDGCContext2D.SetFontStyle(const QStyle: TFontStyles);
var
  MyFont: TDGCFont;
begin
  MyFont := TDGCFont.Create;
  MyFont.Style    := QStyle;
  FListOfEntities.AddElement(tdoCMD_SET_FONT, '', MyFont);
end;

//******************************************************************************
//* Redessin
//******************************************************************************
procedure TDGCContext2D.RedessinVue();
var
  R: TRect;
  MyEntity: TDGCDessinObjet;
  Nb, i: Integer;
  T0, T1: TDateTime;
  HH, MM, SS, MS: word;
  // feuilles de style
  procedure QUseStyleSheet(const QIdxStyleSheet: integer);
  var
    MyStyleSheet: TDGCStyleSheet;
  begin
    if (QIdxStyleSheet = DIEU_AU_CARRE) then Exit;
    MyStyleSheet := FListOfStyles.GetElement(QIdxStyleSheet);
    FTmpBuffer.CanvasBGRA.Pen.Color    := MyStyleSheet.PenColor;
    FTmpBuffer.CanvasBGRA.Pen.Opacity  := MyStyleSheet.PenOpacity;
    FTmpBuffer.CanvasBGRA.Pen.Width    := MyStyleSheet.PenWidth;
    FTmpBuffer.CanvasBGRA.Pen.Style    := MyStyleSheet.PenStyle;

    FTmpBuffer.CanvasBGRA.Brush.Color    := MyStyleSheet.BrushColor;
    FTmpBuffer.CanvasBGRA.Brush.Opacity  := MyStyleSheet.BrushOpacity;
    FTmpBuffer.CanvasBGRA.Brush.Style    := MyStyleSheet.BrushStyle;

    FTmpBuffer.CanvasBGRA.Font.Name     := MyStyleSheet.FontName;
    FTmpBuffer.CanvasBGRA.Font.Height   := MyStyleSheet.FontSizeInPts;
    FTmpBuffer.CanvasBGRA.Font.Color    := MyStyleSheet.FontColor;
    FTmpBuffer.CanvasBGRA.Font.Opacity  := MyStyleSheet.FontOpacity;
    FTmpBuffer.CanvasBGRA.Font.Style    := MyStyleSheet.FontStyle;

  end;


  // dessin des objets
  procedure QSetPenStyle(const E: TDGCPen);
  begin
    FTmpBuffer.CanvasBGRA.Pen.Color    := E.Couleur;
    FTmpBuffer.CanvasBGRA.Pen.Opacity  := E.Opacity;
    FTmpBuffer.CanvasBGRA.Pen.Width    := E.Width;
    FTmpBuffer.CanvasBGRA.Pen.Style    := E.Style;
  end;
  procedure QSetBrushStyle(const E: TDGCBrush);
  begin
    FTmpBuffer.CanvasBGRA.Brush.Color    := E.Couleur;
    FTmpBuffer.CanvasBGRA.Brush.Opacity  := E.Opacity;
    FTmpBuffer.CanvasBGRA.Brush.Style    := E.Style;
  end;
  procedure QSetFontStyle(const E: TDGCFont);
  begin
    FTmpBuffer.CanvasBGRA.Font.Name     := E.FontName;
    FTmpBuffer.CanvasBGRA.Font.Height   := E.Height;
    FTmpBuffer.CanvasBGRA.Font.Color    := E.Couleur;
    FTmpBuffer.CanvasBGRA.Font.Opacity  := E.Opacity;
    FTmpBuffer.CanvasBGRA.Font.Style    := E.Style;
  end;
  procedure QDrawSegment(const E: TDGCSegment);
  var
    PP1, PP2: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    PP1 := GetCoordsPlan(E.P1);
    PP2 := GetCoordsPlan(E.P2);
    FTmpBuffer.CanvasBGRA.MoveTo(PP1.x, PP1.y);
    FTmpBuffer.CanvasBGRA.LineTo(PP2.x, PP2.y);
  end;
  procedure QDrawRectangle(const E: TDGCRectangle);
  var
    PP1, PP2: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    PP1 := GetCoordsPlan(E.P1);
    PP2 := GetCoordsPlan(E.P2);
    FTmpBuffer.CanvasBGRA.Rectangle(PP1.x, PP1.y, PP2.x, PP2.y);
  end;
  procedure QDrawInfiniteHVLine(const E: TDGCInfiniteLine);
  var
    PP1, PP2: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    case E.Orientation of
     tdgcVERTICAL_LINE:
       begin
         PP1 := GetCoordsPlan(MakeTDGCPoint2D(E.PositionCentre.X, FRegionCMini.Y));
         PP2 := GetCoordsPlan(MakeTDGCPoint2D(E.PositionCentre.X, FRegionCMaxi.Y));
       end;
     tdgcHORIZONTAL_LINE:
       begin
         PP1 := GetCoordsPlan(MakeTDGCPoint2D(FRegionCMini.X, E.PositionCentre.Y));
         PP2 := GetCoordsPlan(MakeTDGCPoint2D(FRegionCMaxi.X, E.PositionCentre.Y));
       end;
    end;
    FTmpBuffer.CanvasBGRA.MoveTo(PP1.x, PP1.y);
    FTmpBuffer.CanvasBGRA.LineTo(PP2.x, PP2.y);
  end;
  procedure QDrawEllipse(const E: TDGCEllipse);
  var
    PPC, PPR: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    PPC := GetCoordsPlan(E.Centre);
    PPR := GetCoordsPlan(MakeTDGCPoint2D(E.Centre.X + E.Rayon1, E.Centre.Y + E.Rayon2));
    FTmpBuffer.CanvasBGRA.EllipseC(PPC.X, PPC.Y, PPR.X - PPC.X, PPR.Y - PPC.Y);
  end;
  procedure QDrawTriangle(const E: TDGCTriangle);
  var
    PP1, PP3, PP2: TPoint;
  begin
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    PP1 := GetCoordsPlan(E.P1);
    PP2 := GetCoordsPlan(E.P2);
    PP3 := GetCoordsPlan(E.P3);
    FTmpBuffer.CanvasBGRA.Polygon([PP1, PP2, PP3]);
  end;
  procedure QDrawPolyline(const E: TDGCPolyline);
  var
    i, Nb: integer;
    PP: array of TPoint;
  begin
    Nb := E.GetNbVertex();
    if (Nb < 2) then Exit;
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);

    Setlength(PP, Nb);
    for i := 0 to Nb - 1 do PP[i] := GetCoordsPlan(E.GetVertex(i));
    FTmpBuffer.CanvasBGRA.Polyline(PP);
  end;
  procedure QDrawPolygon(const E: TDGCPolygon);
  var
    i, Nb: integer;
    PP: array of TPoint;
  begin
    Nb := E.GetNbVertex();
     if (Nb < 2) then Exit;
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
     Setlength(PP, Nb);
    for i := 0 to Nb - 1 do PP[i] := GetCoordsPlan(E.GetVertex(i));
    FTmpBuffer.CanvasBGRA.Polygon(PP);
  end;

  procedure QDrawCurve(const E: TDGCCurve);
  var
    i, Nb: integer;
    PP: array of TPoint;
  begin
    Nb := E.GetNbVertex();
    if (Nb < 2) then Exit;
    if (FCurrentStyleSheet <> E.IdxStyleSheet) then QUseStyleSheet(E.IdxStyleSheet);
    Setlength(PP, Nb);
    for i := 0 to Nb - 1 do PP[i] := GetCoordsPlan(E.GetVertex(i));
    // TODO:Implémenter cf GHCaveDraw
    FTmpBuffer.CanvasBGRA.PolyBezier(PP, E.Filled, True);
  end;
  procedure QDrawText(const E: TDGCText);
  var
    QOldOrientation: Integer;
    PP: TPoint;
  begin
    QOldOrientation := FTmpBuffer.CanvasBGRA.Font.Orientation;
    FTmpBuffer.CanvasBGRA.Font.Orientation := Trunc(E.Orientation * 10);
    PP := GetCoordsPlan(E.Position);
    FTmpBuffer.CanvasBGRA.TextOut(PP.X, PP.Y, E.Text);
    FTmpBuffer.CanvasBGRA.Font.Orientation := QOldOrientation;
  end;

begin
  if (not FDessinReady) then Exit;
  if (FDessinEnCours) then Exit; // FDessinEnCours est un flag pour protéger la reconstruction du dessin
  FDessinEnCours := True;
  FTmpBuffer := TBGRABitmap.Create(self.Width, self.Height);
  try
    T0 := Now();
    R.Left   := 0;
    R.Top    := 0;
    R.Bottom := self.Top  + self.Height;
    R.Right  := self.Left + self.Width;
    FTmpBuffer.CanvasBGRA.Brush.Color := FBackGroundColor;
    FTmpBuffer.CanvasBGRA.Brush.Opacity := 255;
    FTmpBuffer.CanvasBGRA.Brush.Style   := bsSolid;
    FTmpBuffer.CanvasBGRA.FillRect(R);
    // on dessine ici: exécution de la liste d'affichage
    Nb := getNbEntities();
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do
      begin
        MyEntity := FListOfEntities.GetElement(i);
        case MyEntity.TypeDessinObjet of
          tdoNONE                 : nop;
          tdoENTITY_SEGMENT       : QDrawSegment(MyEntity.DessinObjet as TDGCSegment);
          tdoENTITY_Ellipse       : QDrawEllipse(MyEntity.DessinObjet  as TDGCEllipse);
          tdoENTITY_TRIANGLE      : QDrawTriangle(MyEntity.DessinObjet as TDGCTriangle);
          tdoENTITY_RECTANGLE     : QDrawRectangle(MyEntity.DessinObjet as TDGCRectangle);
          tdoENTITY_POLYLINE      : QDrawPolyline(MyEntity.DessinObjet as TDGCPolyline);
          tdoENTITY_POLYGONE      : QDrawPolygon(MyEntity.DessinObjet as TDGCPolygon);
          tdoENTITY_CURVE         : QDrawCurve(MyEntity.DessinObjet as TDGCCurve);
          tdoENTITY_TEXT          : QDrawText(MyEntity.DessinObjet as TDGCText);
          tdoCMD_SET_PEN          : QSetPenStyle(MyEntity.DessinObjet as TDGCPen);
          tdoCMD_SET_BRUSH        : QSetBrushStyle(MyEntity.DessinObjet as TDGCBrush);
          tdoCMD_SET_FONT         : QSetFontStyle(MyEntity.DessinObjet as TDGCFont);
          tdoENTITY_INFINITE_LINE : QDrawInfiniteHVLine(MyEntity.DessinObjet as TDGCInfiniteLine);
          tdoCMD_SVG_BEGIN_GROUPE : nop;
          tdoCMD_SVG_END_GROUPE   : nop;
        else
          nop;
        end;
      end;
    end;
    T1 := Now();
    FTmpBuffer.Draw(self.Canvas, 0, 0, True);
    DecodeTime(t1-t0, HH, MM, SS, MS);
    //Label1.Caption:= format('%f, %f -> %f, %f - %d entites - Temps: %.2d:%.2d:%.2d.%.3d', [FRegionCMini.X, FRegionCMini.Y, FRegionCMaxi.X, FRegionCMaxi.Y, Nb, HH, MM, SS, MS]);
  finally
    FreeAndNil(FTmpBuffer);
    FDessinEnCours := False;
  end;
end;

function TDGCContext2D.GetNbEntities(): integer;
begin
  Result := FListOfEntities.Count;
end;





end.
