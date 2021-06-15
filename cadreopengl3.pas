unit CadreOpenGL3;

{$mode delphi}

interface

uses

  Classes, SysUtils, Forms, Controls, Graphics,
  UnitListesSimplesWithGeneriques,
  GL, glu,
  OpenGLContext,
  Dialogs;
// mode de perspective
type TTypePerspective = (tpPERSP_CONIQUE, tpPERSP_AXONOMETRIQUE);
// callback pour messages de debug
type TProcDispMsg = procedure(const Msg: string) of object;
// couleurs OpenGL
type TGLPoint3Df = record
     X: GLfloat;
     Y: GLfloat;
     Z: GLfloat;
end;

type TGLColor = record
  R: GLFloat;
  G: GLFloat;
  B: GLFloat;
  A: GLFloat;
  procedure SetRGBA(const C: TColor; const A: byte = 255); overload;
  procedure SetRGBA(const R, G, B: byte; const A: byte = 255); overload;
end;

Type TListeAffichage = record
  ID           : TGLuint;
  Name         : string;
  Description  : string;
end;

type

{ TTableListesAffichage }

 TTableListesAffichage = class(TListeSimple<TListeAffichage>)    // les listes d'affichage
   private
   public
     procedure ClearTableListesAffichage();
     function  FindListeByName(const QName: string; out LA: TListeAffichage): boolean;
end;




//******************************************************************************
type

  { TCdrOpenGL3 }

  TCdrOpenGL3 = class(TFrame)
    OGLContext: TOpenGLControl;
    procedure OGLContextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OGLContextPaint(Sender: TObject);
    procedure OGLContextResize(Sender: TObject);
  strict private
    FTypePerspective: TTypePerspective;
    FDoDraw    : boolean;
    FBackground: TGLColor;
    FCoinBasGauche: TGLPoint3Df;
    FCoinHautDroit: TGLPoint3Df;

    FTheta: double;
    FPhi  : double;
    FZoom : double;
    FCoefMagnificationZ: double;

    FAnglePetitTriangle: double;

    procedure CallListeAffichage(const Idx: integer);
    procedure DessinerListeAffichage(const Idx: integer);


    function  FitCoords(const QX, QY, QZ: double): TGLPoint3Df;
  private
    FProcDispMessage: TProcDispMsg;
    FTableListesAffichage: TTableListesAffichage;
  public
    property  Background: TGLColor read FBackground write FBackground;
    function  Initialiser(const ProcDispMsg: TProcDispMsg): boolean;
    procedure Finaliser();
    procedure SetSceneBounds(const X1, Y1, Z1, X2, Y2, Z2: double);
    procedure SetTypePerspective(const TP: TTypePerspective);

    procedure SetTheta(const A: double);
    procedure SetPhi(const A: double);
    procedure SetZoom(const A: double);
    procedure SetMagnificationZ(const A: double);


    procedure ClearAllListesAffichage();
    procedure CreateANewListeAffichage(const QName, QDescription: string);
    function  getNbListesAffichages(): integer;
    procedure BeginListeAffichage(const Idx: integer); overload;
    procedure BeginListeAffichage(const QName: string); overload;
    procedure EndListeAffichage();
    procedure BeginSetOfTriangles();
    procedure AddTriangle(const X1, Y1, Z1,  X2, Y2, Z2,  X3, Y3, Z3: double);
    procedure EndSetOfTriangles();
    procedure BeginScene();

    procedure EndScene();
    procedure ReconstruitVue();

    procedure MiniAnimation();
    procedure MakeBoundingBoxScene();
    procedure MakeRepereOrigine(const Taille: double);

  end;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TTableListesAffichage }

procedure TTableListesAffichage.ClearTableListesAffichage();
var
  n, i: Integer;
begin
  n := GetNbElements();
  if (n > 0) then
  begin
    for i := 0 to n-1 do glDeleteLists(i, 1);
  end;
end;

function TTableListesAffichage.FindListeByName(const QName: string; out LA: TListeAffichage): boolean;
var
  EWE: String;
  n, i: Integer;
begin
  result := false;
  n := GetNbElements();
  if (0 = n) then exit(false);
  EWE := LowerCase(QName);
  for i := 0 to n - 1 do
  begin
    LA := GetElement(i);
    if (Pos(EWE, LowerCase(LA.Name)) > 0) then exit(True);
  end;
end;

{ TGLColor }

procedure TGLColor.SetRGBA(const C: TColor; const A: byte = 255);
begin
  SetRGBA(Red(C), Green(C), Blue(C), A);
end;

procedure TGLColor.SetRGBA(const R, G, B: byte; const A: byte);
begin
  self.R := R / 256.0;
  self.G := G / 256.0;
  self.B := B / 256.0;
  self.A := A / 256.0;
end;

{ TCdrOpenGL3 }







function TCdrOpenGL3.Initialiser(const ProcDispMsg: TProcDispMsg): boolean;
const
  DEPTH_COMPOSANTE_PIXEL = 8;
begin
  result  := false;
  FDoDraw := false;
  FTheta  := 0.00;
  FPhi    := 0.00;
  FZoom   := 1.00;
  FCoefMagnificationZ := 1.00;

  FTypePerspective:= tpPERSP_CONIQUE;

  FAnglePetitTriangle := 0.00;
  FProcDispMessage := ProcDispMsg;
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.Initialiser(P)', [ClassName]));

  FTableListesAffichage := TTableListesAffichage.Create;


  ClearAllListesAffichage();

  FBackground.SetRGBA(clBlue);

  // contexte OpenGL
  OGLContext.DepthBits := 4 * DEPTH_COMPOSANTE_PIXEL;
  OGLContext.AlphaBits := DEPTH_COMPOSANTE_PIXEL;
  OGLContext.RedBits   := DEPTH_COMPOSANTE_PIXEL;
  OGLContext.GreenBits := DEPTH_COMPOSANTE_PIXEL;
  OGLContext.BlueBits  := DEPTH_COMPOSANTE_PIXEL;
  result := true;
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.Initialiser() OK', [ClassName]));
end;

procedure TCdrOpenGL3.SetSceneBounds(const X1, Y1, Z1, X2, Y2, Z2: double);
begin
  FCoinBasGauche.X := X1;
  FCoinBasGauche.Y := Y1;
  FCoinBasGauche.Z := Z1;

  FCoinHautDroit.X := X2;
  FCoinHautDroit.Y := Y2;
  FCoinHautDroit.Z := Z2;
end;


procedure TCdrOpenGL3.Finaliser();
begin
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.Finaliser()', [ClassName]));
  FTableListesAffichage.ClearTableListesAffichage();
  FTableListesAffichage.ClearListe();
  FreeAndNil(FTableListesAffichage);
end;

procedure TCdrOpenGL3.SetTypePerspective(const TP: TTypePerspective);
begin
  FTypePerspective := TP;
end;

procedure TCdrOpenGL3.SetTheta(const A: double);
begin
  FTheta := A;
end;

procedure TCdrOpenGL3.SetPhi(const A: double);
begin
  FPhi := A;
end;

procedure TCdrOpenGL3.SetZoom(const A: double);
begin
  FZoom := A;
  ReconstruitVue();
end;

procedure TCdrOpenGL3.SetMagnificationZ(const A: double);
begin
  FCoefMagnificationZ := A;
end;

procedure TCdrOpenGL3.ClearAllListesAffichage();
begin
  FProcDispMessage(format('%s.ClearAllListesAffichage()', [ClassName]));
  //FTableListesAffichage.ClearTableListesAffichage();
  FTableListesAffichage.ClearListe();
  FProcDispMessage(' -- OK');
end;







// listes d'affichage
procedure TCdrOpenGL3.CreateANewListeAffichage(const QName, QDescription: string);
var
  LA : TListeAffichage;
  i: Integer;
begin
  LA.ID          := glGenLists(1);
  LA.Name        := QName;
  LA.Description := QDescription;
  FTableListesAffichage.AddElement(LA);
  FProcDispMessage(format('%s.CreateANewListeAffichage(): %d listes', [ClassName, FTableListesAffichage.GetNbElements()]));
  // Contrôle
  for i := 0 to FTableListesAffichage.GetNbElements() - 1 do
  begin
    LA := FTableListesAffichage.GetElement(i);
    FProcDispMessage(format('Liste %d: L: %d - N: %s - D: %s', [i, LA.ID, LA.Name, LA.Description]));
  end;
end;

procedure TCdrOpenGL3.BeginSetOfTriangles();
begin
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.BeginSetOfTriangles()', [ClassName]));
end;

procedure TCdrOpenGL3.AddTriangle(const X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3: double);
var
  P1, P2, P3: TGLPoint3Df;
begin
  //glColor3f(1.0, 0, 0);
  glBegin(GL_POLYGON);
    P1 := FitCoords(X1, Y1, Z1);
    P2 := FitCoords(X2, Y2, Z2);
    P3 := FitCoords(X3, Y3, Z3);
    glVertex3f(P1.X, P1.Y, P1.Z);
    glVertex3f(P2.X, P2.Y, P2.Z);
    glVertex3f(P3.X, P3.Y, P3.Z);
  glEnd();
end;

procedure TCdrOpenGL3.EndSetOfTriangles();
begin
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.EndSetOfTriangles()', [ClassName]));
end;

procedure TCdrOpenGL3.BeginScene();
var
  i, n: Integer;
  LA: TListeAffichage;
begin
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.BeginScene()', [ClassName]));
  // détruire les listes (sinon fuite mémoire)
  n := getNbListesAffichages();
  FProcDispMessage(format('-- >Purge %d displaylists', [n]));
  if (0 = n) then exit;
  for i := 0 to n -1 do
  begin
    LA := FTableListesAffichage.GetElement(i);
    FProcDispMessage(format('Deleting %d: %d %s', [i, LA.ID, LA.Name]));
    glDeleteLists(LA.ID, 1);
  end;
end;

procedure TCdrOpenGL3.EndScene();
begin
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.EndScene()', [ClassName]));
  OGLContext.Invalidate;
end;

procedure TCdrOpenGL3.BeginListeAffichage(const Idx: integer);
var
  MyListeAffichage: TListeAffichage;
begin
  MyListeAffichage := FTableListesAffichage.GetElement(Idx);
  FProcDispMessage(format('--- %s.BeginListeAffichage(Idx: %d, L: %d)', [ClassName, Idx, MyListeAffichage.ID]));
  glNewList(MyListeAffichage.ID, GL_COMPILE);
end;

procedure TCdrOpenGL3.BeginListeAffichage(const QName: string);
var
  MyListeAffichage: TListeAffichage;
begin
  if (FTableListesAffichage.FindListeByName(QName, MyListeAffichage)) then
    glNewList(MyListeAffichage.ID, GL_COMPILE)
  else
    FProcDispMessage('Echec liste affichaqge');
end;

procedure TCdrOpenGL3.EndListeAffichage();
begin
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.EndListeAffichage()', [ClassName]));
  glEndList();
end;
procedure TCdrOpenGL3.CallListeAffichage(const Idx: integer);
var
  MyListeAffichage: TListeAffichage;
begin
  MyListeAffichage := FTableListesAffichage.GetElement(Idx);
  FProcDispMessage(format('%s.CallListeAffichage(Idx: %d, L: %d, N = %s)', [ClassName, Idx, MyListeAffichage.ID, MyListeAffichage.Name]));
  glCallList(MyListeAffichage.ID);
end;


function TCdrOpenGL3.getNbListesAffichages(): integer;
begin
  Result := FTableListesAffichage.GetNbElements();
end;

//******************************************************************************
procedure TCdrOpenGL3.OGLContextPaint(Sender: TObject);
begin
  ReconstruitVue();
end;



procedure TCdrOpenGL3.OGLContextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  ;;
end;

procedure TCdrOpenGL3.OGLContextResize(Sender: TObject);
begin
  OGLContext.Invalidate;
end;

//******************************************************************************

//******************************************************************************

// lignes

// triangles
// Callback de dessin
procedure TCdrOpenGL3.ReconstruitVue();
var
  ErrCode: TGLuint;
  i: Integer;
  procedure SetPerspectiveParameters();
  var
    QRatio: Extended;
    QLeft, QRight, QBottom, QTop, QNear, QFar: GLdouble;
  begin
    QRatio := OGLContext.Width / OGLContext.Height;

    QLeft     :=  -10.0;
    QRight    :=   10.0;
    QTop      :=   10.0 * QRatio;
    QBottom   :=  -10.0 * QRatio;
    QNear     :=  -0.100;
    QFar      :=  -1000.00;

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    case FTypePerspective of
      tpPERSP_CONIQUE:
        begin
          FProcDispMessage('SetPerspectiveParameters() ' + 'Perspective');
          glFrustum(QLeft, QRight, QBottom, QTop, QNear, QFar);
        end;
      tpPERSP_AXONOMETRIQUE:
        begin
          glOrtho(QLeft, QRight, QBottom, QTop, QNear, QFar);
          FProcDispMessage('SetPerspectiveParameters() ' + 'Orthographic');
        end;
    end;
  end;
begin
    // /!\ Eviter les AfficherMessage sauf en débogage: ils ralentissent considérablement l'affichage 3D
  //AfficherMessage(Format('%s.ReconstruitVue()',[ClassName]));
  //if (Not FDoDraw) then Exit;
  glViewport(0,0, OGLContext.Width,  OGLContext.Height);  // fenêtre de vue
  FDoDraw := (getNbListesAffichages() > 0);
  if (Assigned(FProcDispMessage)) then FProcDispMessage(format('%s.ReconstruitVue(nb listes: %d)', [ClassName, getNbListesAffichages()]));

  if (FDoDraw) then
  begin
    glClearColor(FBackground.R, FBackground.G, FBackground.B, FBackground.A);
    glEnable(GL_DEPTH_TEST);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glClearDepth(1.0);
    glDepthFunc(GL_LESS);
    //glShadeModel(GL_SMOOTH);
    //glEnable(GL_CULL_FACE);
    //glEnable(GL_BLEND);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    SetPerspectiveParameters();

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
      //glTranslatef(0.0, 0.0, -FZoom);
      //glPushMatrix(); // PushMatrix général de la scène
        glLoadIdentity();


        glColor4f(0.0, 1.0, 0.0, 1.0);
        CallListeAffichage(0);
        glColor4f(0.0, 0.0, 1.0, 1.0);

        CallListeAffichage(1); // cube
        // le reste de la scène ici
        glColor4f(1.0, 0, 0, 0.5);
        CallListeAffichage(2);
        // un petit triangle qui tourne
        (*
        glTranslatef(-0.5, -0.5, 0);
        glPushMatrix();
          glLoadIdentity();
            gltranslatef(0,0,-0.25);
            glRotatef(FAnglePetitTriangle, 0,0,1);
            glscalef(0.5, 0.5, 0.5);
            glColor4f(0, 0, 0.5, 0.5);
          //CallListeAffichage(1);
        glPopMatrix();
        //*)
      //glPopMatrix(); // PopMatrix général
    glFlush(); // On envoie
    ErrCode := glGetError;
    if (ErrCode <> 0) then FProcDispMessage(format('*** OpenGL error: %d - %s',[ErrCode, glGetString(ErrCode)])); //rsOPENGLERROR);
    OGLContext.SwapBuffers; // échange buffer
  end;
end;

procedure TCdrOpenGL3.DessinerListeAffichage(const Idx: integer);
var
  LA : TListeAffichage;
begin
  LA := FTableListesAffichage.GetElement(Idx);
  //glDisable(GL_LIGHTING);
  //glDisable(GL_LIGHT0);
  glCallList(LA.ID);
end;

procedure TCdrOpenGL3.MiniAnimation();
var
  i: Integer;
begin
  FAnglePetitTriangle := 0.00;
  for i := 0 to 360 do
  begin
    ReconstruitVue();
    FAnglePetitTriangle := i * 1.00;
    sleep(10);
  end;
end;

// TODO: Calculs à implémenter (probablement mettre sur [-1, 1] x [-1, 1] x [-1, 1])
function TCdrOpenGL3.FitCoords(const QX, QY, QZ: double): TGLPoint3Df;
const MULTIPLICATEUR = 1 /1000.0;
var
  FEtendue: TGLPoint3Df;
  FP1, FP2: TGLPoint3Df;
begin
  (*
  // calcul de l'étendue
  FEtendue.X :=  FCoinHautDroit.X - FCoinBasGauche.X;
  FEtendue.Y :=  FCoinHautDroit.Y - FCoinBasGauche.Y;
  FEtendue.Z := (FCoinHautDroit.Z - FCoinBasGauche.Z) * FCoefMagnificationZ;

  // mise au format des coordonnées
  FP1.X :=  QX - FCoinBasGauche.X;
  FP1.Y :=  QY - FCoinBasGauche.Y;
  FP1.Z := (QZ - FCoinBasGauche.Z) * FCoefMagnificationZ;

  // et on translate d'une demi-dimension
  Result.X := MULTIPLICATEUR * (-0.50 * FEtendue.X + FP1.X);
  Result.Y := MULTIPLICATEUR * (-0.50 * FEtendue.Y + FP1.Y);
  Result.Z := MULTIPLICATEUR * (-0.50 * FEtendue.Z + FP1.Z);

  //*)
  Result.X := QX;
  Result.Y := QY;
  Result.Z := QZ;

end;
//******************************************************************************
procedure TCdrOpenGL3.MakeBoundingBoxScene();
var
  i: integer;
  t: array[0..1] of GLFloat;
  Q1, Q2: Double;
  Fcs1, Fcs2: TGLPoint3Df;
begin
  FProcDispMessage(Format('C1: %.3f, %.2f, %.3f C2: %.3f, %.3f, %.3f', [FCoinBasGauche.X, FCoinBasGauche.Y, FCoinBasGauche.Z, FCoinHautDroit.X, FCoinHautDroit.Y, FCoinHautDroit.Z]));
  Fcs1 := FitCoords(FCoinBasGauche.X, FCoinBasGauche.Y, FCoinBasGauche.Z);
  Fcs2 := FitCoords(FCoinHautDroit.X, FCoinHautDroit.Y, FCoinHautDroit.Z);
  FProcDispMessage(Format('C1: %.3f, %.2f, %.3f C2: %.3f, %.3f, %.3f', [Fcs1.X, Fcs1.Y, Fcs1.Z, Fcs2.X, Fcs2.Y, Fcs2.Z]));

  for i:=0 to 1 do
  begin
    glBegin(GL_LINE_LOOP);
     glVertex3d(Fcs1.X, Fcs1.Y, t[i]);
     glVertex3d(Fcs2.X, Fcs1.Y, t[i]);
     glVertex3d(Fcs2.X, Fcs2.Y, t[i]);
     glVertex3d(Fcs1.X, Fcs2.Y, t[i]);
    glEnd();
  end;
  glBegin(GL_LINES);
    glVertex3d(Fcs1.X, Fcs1.Y, t[0]);
    glVertex3d(Fcs1.X, Fcs1.Y, t[1]);
    glVertex3d(Fcs2.X, Fcs1.Y, t[0]);
    glVertex3d(Fcs2.X, Fcs1.Y, t[1]);
    glVertex3d(Fcs2.X, Fcs2.Y, t[0]);
    glVertex3d(Fcs2.X, Fcs2.Y, t[1]);
    glVertex3d(Fcs1.X, Fcs2.Y, t[0]);
    glVertex3d(Fcs1.X, Fcs2.Y, t[1]);
  glEnd();
end;

procedure TCdrOpenGL3.MakeRepereOrigine(const Taille: double);
begin
  glBegin(GL_TRIANGLES);
    glVertex3d(0.0, 0.0, 0.0);
    glVertex3d(Taille, 0.0, 0.0);
    glVertex3d(0, Taille, 0.0);
  glEnd();
end;


end.

