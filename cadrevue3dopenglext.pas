unit CadreVue3DOpenGLExt;

{$INCLUDE CompilationParameters.inc}


// Date: 08/11/2013
//------------------------------------------------------------------------------
// Nécessite l'installation du composant LazarusOpenGL
// ouvrir le paquet $(LAZARUS)/components/opengl/lazopenglcontext.lpk
// et l'installer.
//------------------------------------------------------------------------------
// Statut: Opérationnel - Des détails à revoir
// Couleurs par mode de représentation: OK
// Paramétrage angles et couleurs: OK
// 01/06/2015 : Support des maillages de surface (OK avec les TIN, OK avec les grid)
// 25/04/2016 : Les maillages MNT deviennent un objet interne (aucune liaison avec ceux de la fenêtre principale de GHTopo)
// 23/11/2018 : Petites corrections, notamment pour les réseaux très étendus
// 12/12/2018 : Unification des paramètres de vue 3D, suppression de nombreuses variables, corrections
// 04/05/2020 : Pointage temporel
// 15/04/2021 : Magnification Z OK
interface
uses
  StructuresDonnees,
  Common,
  CalculMatriciel3x3,
  UnitEntitesExtended,
  UnitClasseMaillage,
  GL, glu,
  math,
  Dialogs,
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics;
// couleurs OpenGL
type TGLColor = record
  R: GLFloat;
  G: GLFloat;
  B: GLFloat;
  A: GLFloat;
end;
type

{ TCdrVue3DOpenGLExt }

 TCdrVue3DOpenGLExt = class(TFrame)
    OpenGLControl1: TOpenGLControl;
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
  strict private
    { private declarations }
    FBDDEntites: TBDDEntites;
    FMyMaillage: TMaillage;

    FDoDraw        : boolean;              // dessin OK
    FProcRefreshControls: TProcRefreshControlsBy3DView;         // callback pour rafraichissement
    FVue3DParams  : TVue3DParams;                                 // paramètres de vue 3D
    FCoin1, FCoin2: TPoint3Df; // encombrement
    Fcs1, Fcs2    : TPoint3Df; // valeurs ci-dessus mises à l'échelle
    FExcentr      : TPoint3Df; // décalage
    // listes d'affichage
    FglListCUBE          : TGLuint;       // cube
    FglListREFERENTIEL   : TGLuint;       // référentiel
    FglListPOLYGONALS    : TGLuint;       // polygonales de la cavité
    FglListRADIANT_SHOTS : TGLuint;       // visées radiantes
    FglListVOLUMES       : TGLuint;       // volumes de la cavité
    FglListMAILLAGE      : TGLuint;       // maillage
    // caractéristiques de lumière
    Flight_ambient      : array[0..3] of GLfloat; // lumière ambiante
    // variables géométriques en entrée
    function InitContextOpenGL(): boolean;
    function ConstruireScene(): boolean;
    procedure MakeCUBE();
    procedure MakePOLYGONALES(const M: double);
    procedure MakeRADIANT_SHOTS(const M: double);
    procedure MakeVOLUMES(const M: double);
    procedure MakeMAILLAGE(const M: double);
    procedure ReconstruitVue();
  public
    { public declarations }
    function  InitialiserVue3D(const QPV : TVue3DParams;
                               const QBDD: TBDDEntites;
                               const QMaillage : TMaillage;
                               const MyFiltre: string;
                               const FP: TProcRefreshControlsBy3DView): boolean;
    procedure FinaliserVue3D();
    function  GetMaillage(): TMaillage;
    procedure SetParamsVue3D(const QVue3DParams: TVue3DParams; const DoRegenMaillage: boolean);
    function  GetParamsVue3D(): TVue3DParams;
    procedure SetColorsReseauByDepth(const QColorZMini, QColorZMaxi: TColor);
    function  GetTheta: double;
    function  GetPhi  : double;
    function  GetFactZ: double;
    function  GetColorZMiniReseau: TColor;
    function  GetColorZMaxiReseau: TColor;

    function  GetColorZMiniMNT   : TColor;
    function  GetColorZMaxiMNT   : TColor;

    function  GetFiltres(): string;
    function  GetDoFiltrer(): boolean;
    //procedure SetTransparenceMaillage(
    procedure SetParamsMaillage(const TM: TMNTModeDessinMaillage; const DoUseDegrade: boolean; const ColorZMini, ColorZMaxi: TColor; const Opacity: byte);
  end;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}
const
  LOW_INDEX = 0;
{$DEFINE OPENGL_ANCIENNE_METHODE}
{.$UNDEF OPENGL_ANCIENNE_METHODE}
{$IFDEF OPENGL_ANCIENNE_METHODE}
const MULTIPLICATEUR = 1/10000;// 1/10000
{$ELSE}
const MULTIPLICATEUR = 1.0;
{$ENDIF OPENGL_ANCIENNE_METHODE}
 // couleur OpenGL
 function TColorToGLColor(const Coul: TColor; const Alpha: byte = 255): TGLColor;
 const
   m = 1/256;
 begin
   Result.R := Red(Coul)   * m;
   Result.G := Green(Coul) * m;
   Result.B := Blue(Coul)  * m;
   Result.A := Alpha       * m;
 end;
 // Retourne sous forme de couleur Pascal la couleur OpenGL passée en argument
 function TGLColorToTColor(const Coul: TGLColor): TColor;
 const
   m = 256;
 begin
   Result := RGBToColor(Round(Coul.R * m), Round(Coul.G * m), Round(Coul.B * m));
 end;
 function TGHTopoColorToGLColor(const Coul: TGHTopoColor): TGLColor;
 const
   m = 1/256;
 begin
   Result.R := Coul.Red   * m;
   Result.G := Coul.Green * m;
   Result.B := Coul.Blue  * m;
   Result.A := Coul.Alpha * m;
 end;
 // Retourne sous forme de couleur Pascal la couleur OpenGL passée en argument
 function TGLColorToTGHTopoColor(const Coul: TGLColor): TGHTopoColor;
 const
   m = 256;
 begin
   Result.setFrom(Round(Coul.R * m), Round(Coul.G * m), Round(Coul.B * m), Round(Coul.A * m));
 end;
{ TCdrVue3DOpenGLExt }

function TCdrVue3DOpenGLExt.InitialiserVue3D(const QPV       : TVue3DParams;
                                             const QBDD      : TBDDEntites;
                                             const QMaillage : TMaillage;
                                             const MyFiltre  : string;
                                             const FP: TProcRefreshControlsBy3DView): boolean;
var
  QDevelViseesVisibles: double;
begin
  AfficherMessage(Format('%s.InitialiseVue3D()', [ClassName]));
  Result      := False;
  // pointeur sur la BDD
  FBDDEntites := QBDD;
  // pointeur sur le maillage
  FMyMaillage := QMaillage;
  // fonction de rappel
  FProcRefreshControls := FP;
  // désarmement des drapeaux
  FDoDraw      := False;              // dessin OK

  // paramétrage vue
  FVue3DParams := QPV;
  // chargement entités
  try
    // préparation tables temporaires
    FBDDEntites.MetaFiltre(MyFiltre, QDevelViseesVisibles);
    FBDDEntites.SetMinMax(True);                                                                      // mini et maxi avec filtres
    FBDDEntites.CalcCouleursByDepth(FVue3DParams.ColorZMiniReseau, FVue3DParams.ColorZMaxiReseau);    // dégradé de couleurs
    // cube enveloppe
    FCoin1 := FBDDEntites.GetCoinBasGauche();
    FCoin2 := FBDDEntites.GetCoinHautDroit();
    // démarrage du contexte OpenGL
    Result := InitContextOpenGL();
    Result := True;
  except
  end;
end;

procedure TCdrVue3DOpenGLExt.FinaliserVue3D();
begin
  pass;
end;





function TCdrVue3DOpenGLExt.GetTheta: double;
begin
  Result := FVue3DParams.Theta;
end;

function TCdrVue3DOpenGLExt.GetPhi: double;
begin
  Result := FVue3DParams.Phi;
end;



function TCdrVue3DOpenGLExt.GetFactZ: double;
begin
  Result := FVue3DParams.CoefMagnification;
end;

function TCdrVue3DOpenGLExt.GetColorZMiniReseau: TColor;
begin
  Result := FVue3DParams.ColorZMiniReseau;
end;

function TCdrVue3DOpenGLExt.GetColorZMaxiReseau: TColor;
begin
  Result := FVue3DParams.ColorZMaxiReseau;
end;

function TCdrVue3DOpenGLExt.GetColorZMiniMNT: TColor;
begin
  Result := FVue3DParams.ColorZMiniMNT;
end;

function TCdrVue3DOpenGLExt.GetColorZMaxiMNT: TColor;
begin
  Result := FVue3DParams.ColorZMaxiMNT;
end;



function TCdrVue3DOpenGLExt.GetFiltres(): string;
begin

end;

function TCdrVue3DOpenGLExt.GetMaillage(): TMaillage;
begin
  Result := FMyMaillage;
end;

function TCdrVue3DOpenGLExt.GetDoFiltrer(): boolean;
begin

end;




procedure TCdrVue3DOpenGLExt.SetColorsReseauByDepth(const QColorZMini, QColorZMaxi: TColor);
begin
  AfficherMessage(Format('%s.SetColorsByDepth($%X, $%X)', [ClassName, QColorZMini, QColorZMaxi]));
  FVue3DParams.ColorZMiniReseau       := QColorZMini;
  FVue3DParams.ColorZMaxiReseau       := QColorZMaxi;
  FBDDEntites.CalcCouleursByDepth(FVue3DParams.ColorZMiniReseau, FVue3DParams.ColorZMaxiReseau);
end;

procedure TCdrVue3DOpenGLExt.SetParamsVue3D(const QVue3DParams: TVue3DParams; const DoRegenMaillage: boolean);
var
  QDevelViseesVisibles: double;
begin
  QDevelViseesVisibles:= 0.00;
  FVue3DParams := QVue3DParams;
  if (FVue3DParams.DoFiltrer) then
  begin
    FBDDEntites.MetaFiltre(FVue3DParams.Filtres, QDevelViseesVisibles);
    FBDDEntites.SetMinMax(True);
  end;
  //ConstruireScene();
  if (DoRegenMaillage) then  SetParamsMaillage(FVue3DParams.MaillageModeDessin,
                                               FVue3DParams.MaillageUseDegrades,
                                               FVue3DParams.ColorZMiniMNT,
                                               FVue3DParams.ColorZMaxiMNT,
                                               FVue3DParams.MaillageOpacity)
                       else  ReconstruitVue();
end;

function TCdrVue3DOpenGLExt.GetParamsVue3D(): TVue3DParams;
begin
  result := FVue3DParams;
end;


procedure TCdrVue3DOpenGLExt.OpenGLControl1Paint(Sender: TObject);
begin
  ReconstruitVue();
end;

procedure TCdrVue3DOpenGLExt.OpenGLControl1Resize(Sender: TObject);
begin
  OpenGLControl1.Invalidate;
end;

//******************************************************************************
function TCdrVue3DOpenGLExt.InitContextOpenGL(): boolean;
const
  B = 0.2;
  DEPTH_COMPOSANTE_PIXEL = 8;
begin
  AfficherMessage(Format('%s.InitContextOpenGL()',[ClassName]));
  FDoDraw := False;
  Result  := False;
  OpenGLControl1.DepthBits := 4 * DEPTH_COMPOSANTE_PIXEL;
  OpenGLControl1.AlphaBits := DEPTH_COMPOSANTE_PIXEL;
  OpenGLControl1.RedBits   := DEPTH_COMPOSANTE_PIXEL;
  OpenGLControl1.GreenBits := DEPTH_COMPOSANTE_PIXEL;
  OpenGLControl1.BlueBits  := DEPTH_COMPOSANTE_PIXEL;
  try
    if (OpenGLControl1.MakeCurrent) then
    begin
      // lumière ambiante
      Flight_ambient[0]:=B ;
      Flight_ambient[1]:=B ;
      Flight_ambient[2]:=B ;
      Flight_ambient[3]:=1.0;

      FVue3DParams.ColorBackGround  := clBlack;
      FVue3DParams.LineReferentiel.SetAttributes(clBlue, 255, 0, 0.05);

      FVue3DParams.FovOrZoom  := 60.00;
      FVue3DParams.Theta      := 45.00;
      FVue3DParams.Phi        := 32.00;

      // construction de la scène
      FDoDraw := ConstruireScene();
      Result  := FDoDraw;
    end;
  except
  end;
end;
// construction de la scène
function TCdrVue3DOpenGLExt.ConstruireScene(): boolean;
begin
  //ShowMessage('ConstruireScene()');
  AfficherMessage(Format('%s.ConstruireScene', [ClassName]));
  Result := False;
  //try
    // Le filtrage n'est plus du ressort de cette fonction
    //FBDDEntites.MetaFiltre(Filtres);
    // détruire les listes (sinon fuite mémoire)
    AfficherMessage('-- >Purge displaylists');
    glDeleteLists(FglListCUBE         , 1);
    glDeleteLists(FglListREFERENTIEL  , 1);
    glDeleteLists(FglListPOLYGONALS   , 1);
    glDeleteLists(FglListRADIANT_SHOTS, 1);
    glDeleteLists(FglListVOLUMES      , 1);
    glDeleteLists(FglListMAILLAGE     , 1);
    // cube enveloppe
    AfficherMessage('-- >Cube enveloppe');
    FglListCUBE := glGenLists(1);
    glNewList(FglListCUBE, GL_COMPILE);
      MakeCUBE();
    glEndList();
    AfficherMessage(Format('FglListCUBE = %d', [FglListCUBE]));
    // polygonales
    AfficherMessage('-- >Polygonales');
    FglListPOLYGONALS:=glGenLists(1);
    glNewList(FglListPOLYGONALS, GL_COMPILE);
      MakePOLYGONALES(MULTIPLICATEUR);
    glEndList;
    // visées radiantes
    AfficherMessage('-- >Visees Radiantes');
    FglListRADIANT_SHOTS := glGenLists(1);
    glNewList(FglListRADIANT_SHOTS, GL_COMPILE);
      MakeRADIANT_SHOTS(MULTIPLICATEUR);
    glEndList();
    // volumes des conduits
    AfficherMessage('-- >Conduits');
    FglListVOLUMES := glGenLists(1);
    glNewList(FglListVOLUMES, GL_COMPILE);
      MakeVOLUMES(MULTIPLICATEUR);
    glEndList;
    // maillage
    AfficherMessage('-- >Maillage');
    FglListMAILLAGE := glGenLists(1);
    glNewList(FglListMAILLAGE, GL_COMPILE);
      MakeMAILLAGE(MULTIPLICATEUR);
    glEndList;

    AfficherMessage('Construction des listes OK');
    Result := True;
  //except
  //end;
end;


//------------------------------------------------------------------------------
// éléments de la scène
// construction des éléments de la vue
procedure TCdrVue3DOpenGLExt.MakeCUBE();
var
  i: integer;
  t: array[0..1] of GLFloat;
  Q1, Q2: Double;
begin
    Fcs1.setFrom(FCoin1.X * MULTIPLICATEUR,
                 FCoin1.Y * MULTIPLICATEUR,
                 FCoin1.Z * MULTIPLICATEUR * FVue3DParams.CoefMagnification);
    Fcs2.setFrom(FCoin2.X * MULTIPLICATEUR,
                 FCoin2.Y * MULTIPLICATEUR,
                 FCoin2.Z * MULTIPLICATEUR * FVue3DParams.CoefMagnification);
    // TODO: A revoir
    Q1 := Fcs2.Z;
    Q2 := Fcs1.Z + 0.20 * Hypot2D(Fcs2.X - Fcs1.X, Fcs2.Y - Fcs1.Y);
    Fcs2.Z := Max(Q1, Q2);
  t[0] := Fcs1.Z;  t[1] := Q1; //Fcs2.Z;
  for i:= 0 to 1 do
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

procedure TCdrVue3DOpenGLExt.MakePOLYGONALES(const M: double);
var
  i, Nb : Integer;
  BP   : TBaseStation;
  c     : TGLColor;
  FZ: Double;
begin
  FZ := M * FVue3DParams.CoefMagnification;
  Nb := FBDDEntites.GetNbEntitesVisees();
  AfficherMessage(Format('%s.MakePOLYGONALES(%d entities)', [ClassName, Nb]));
  glBegin(GL_LINES);
  for i:= LOW_INDEX to Nb - 1 do
   begin
     BP := FBDDEntites.GetEntiteVisee(i);
     if (BP.Enabled) then
     begin
       case BP.Type_Entite of
         tgDEFAULT,
         tgFOSSILE,
         tgVADOSE,
         tgENNOYABLE,
         tgSIPHON,
         tgTUNNEL,
         tgMINE     : c := TGHTopoColorToGLColor(FBDDEntites.GetColorViseeFromModeRepresentation(FVue3DParams.ModeRepresentation, BP));
         tgSURFACE  : c := TColorToGLColor(clGray);
         tgVISEE_RADIANTE: c := TColorToGLColor(clSilver);
       else
         Continue;
       end;
       glColor3d(c.R, c.G, c.B);
       glVertex3d(BP.PosExtr0.X * M, BP.PosExtr0.Y * M, BP.PosExtr0.Z * FZ);
       glVertex3d(BP.PosStation.X * M, BP.PosStation.Y * M, BP.PosStation.Z * FZ);
     end;
   end;
  glEnd;
end;
procedure TCdrVue3DOpenGLExt.MakeRADIANT_SHOTS(const M: double);
var
  i, Nb: Integer;
  BP: TBaseStation;
  QGrayScale, FZ: double;
begin
  FZ := M * FVue3DParams.CoefMagnification;
  Nb := FBDDEntites.GetNbEntitesAntennes();
  if (Nb = 0) then Exit;
  AfficherMessage(Format('%s.MakeRADIANT_SHOTS(%d entities)', [ClassName, Nb]));
  QGrayScale := 0.33;
  glBegin(GL_LINES);
    glColor3f(QGrayScale, QGrayScale, QGrayScale);
    for i:= LOW_INDEX to Nb - 1 do
    begin
      BP := FBDDEntites.GetEntiteAntenne(i);
      glVertex3d(BP.PosExtr0.X * M, BP.PosExtr0.Y * M, BP.PosExtr0.Z * FZ);
      glVertex3d(BP.PosStation.X * M, BP.PosStation.Y * M, BP.PosStation.Z * FZ);
    end;
  glEnd();
end;

procedure TCdrVue3DOpenGLExt.MakeVOLUMES(const M: double);
const
  NB_FACETTES = 14;

var
  i: integer;
  FZ: double;
  Vertex  : array[1..NB_FACETTES] of TPoint3Df;
  Normales: array[1..NB_FACETTES] of TPoint3Df;
  function GetNormale(const dx, dy, dz: double):TPoint3Df;
  const EPSILON=1e-5;
  var
    l: double;
  begin
    l:=Hypot3D(dx+EPSILON,dy+EPSILON,dz+EPSILON);
    Result.X:=dx/l;
    Result.Y:=dy/l;
    Result.Z:=dz/l;
  end;
  procedure PutVertex(const q: integer);
  begin
    glNormal3d(Normales[q].X, Normales[q].Y, Normales[q].Z);
    glVertex3d(Vertex[q].X, Vertex[q].Y, Vertex[q].Z);;
  end;
  procedure DrawTube(const EE: TBaseStation);
  var
    c: TGLColor;
    p,q,r: integer;
    z1, z2: Double;
    v1, v2: TPoint3Df;
  begin

    // vertex
    (*
                V4 ---------------------------- V10
               /  \
              /     \
             /        \
            V5          \ V3                V11           V9
             !            !
             !            !
             !            !
             !            !
            V6           V2                 V12           V8
             \          /
              \       /
               \    /
                \ /
                V1                               V7

    Triangles 'pairs'

    V1 V7 V2
    V2 V8 V3
    V3 V9 V4
    V4 V10 V5
    V5 V11 V6
    V6 V12 V1

    Triangles impairs

    V2 V7 V8
    V3 V8 V9
    V4 V9 V10
    V5 V10 V11
    V6 V11 V12
    V1 V12 V7
    //*)
    // calcul des sommets
    with EE do
    begin
      if (Not Enabled) then Exit;
      if (Not EE.IsInCaveOrTunnel()) then Exit;

      z1 := 0.50 * (PosOPG.Z + PosExtr0.Z);
      z2 := 0.50 * (PosOPD.Z + PosExtr0.Z);

      Vertex[1].setfrom(PosExtr0.X * M, PosExtr0.Y * M, PosOPG.Z * FZ);
      Vertex[2].setfrom(PosOPD.X * M, PosOPD.Y * M, z1 * FZ);
      Vertex[3].setfrom(Vertex[2].X, Vertex[2].Y, z2 * FZ);
      Vertex[4].setfrom(Vertex[1].X, Vertex[1].Y, PosOPD.Z * FZ);
      Vertex[5].setfrom(PosOPG.X * M, PosOPG.Y * M, z2 * FZ);
      Vertex[6].setfrom(Vertex[5].X, Vertex[5].Y, z1 * FZ);

      z1 := 0.50 * (PosPG.Z + PosStation.Z);
      z2 := 0.50 * (PosPD.Z + PosStation.Z);

      Vertex[7].setfrom(PosStation.X * M, PosStation.Y * M, PosPG.Z * FZ);
      Vertex[8].setfrom(PosPD.X * M, PosPD.Y * M, z1 * FZ);
      Vertex[9].setfrom(Vertex[8].X, Vertex[8].Y, z2 * FZ);
      Vertex[10].setfrom(Vertex[7].X, Vertex[7].Y, PosPD.Z * FZ);
      Vertex[11].setfrom(PosPG.X * M, PosPG.Y * M, z2 * FZ);
      Vertex[12].setfrom(Vertex[11].X, Vertex[11].Y, z1 * FZ);
      // calcul des normales
      Normales[1].setfrom(0.0, 0.0, -1.0);
      Normales[2]:=GetNormale(PosOPD.X - PosExtr0.X,
                              PosOPD.Y - PosExtr0.Y,
                              PosOPG.Z - PosExtr0.Z);
      Normales[3]:=GetNormale(PosOPD.X - PosExtr0.x,
                              PosOPD.Y - PosExtr0.Y,
                              PosOPD.Z - PosExtr0.Z);
      Normales[4].setfrom(0.0, 0.0, 1.0);
      Normales[5]:=GetNormale(PosOPG.X - PosExtr0.X,
                              PosOPG.Y - PosExtr0.Y,
                              PosOPD.Z - PosExtr0.Z);
      Normales[6]:=GetNormale(PosOPG.X - PosExtr0.x,
                              PosOPG.Y - PosExtr0.Y,
                              PosOPG.Z - PosExtr0.Z);
      Normales[7] .setfrom(0.0, 0.0, -1.0);
      Normales[8]:=GetNormale(PosPD.X - PosStation.X,
                              PosPD.Y - PosStation.Y,
                              PosPG.Z - PosStation.Z);
      Normales[9]:=GetNormale(PosPD.X - PosStation.X,
                              PosPD.Y - PosStation.Y,
                              PosPD.Z - PosStation.Z);
      Normales[10] .setfrom(0.0, 0.0, 1.0);
      Normales[11]:=GetNormale(PosPG.X - PosStation.X,
                               PosPG.Y - PosStation.Y,
                               PosPD.Z - PosStation.Z);
      Normales[12]:=GetNormale(PosPG.X - PosStation.X,
                               PosPG.Y - PosStation.Y,
                               PosPG.Z - PosStation.Z);
      V1.setfrom(Vertex[2].X - Vertex[6].X,
                          Vertex[2].Y - Vertex[6].Y,
                          Vertex[2].Z - Vertex[6].Z);
      V2.setfrom(Vertex[5].X - Vertex[6].X,
                          Vertex[5].Y - Vertex[6].Y,
                          Vertex[5].Z - Vertex[6].Z);
      Normales[13] := ProduitVectoriel(V1,V2,True);
      V1.setfrom(Vertex[8].X - Vertex[12].X,
                          Vertex[8].Y - Vertex[12].Y,
                          Vertex[8].Z - Vertex[12].Z);
      V2.setfrom(Vertex[9].X - Vertex[12].X,
                          Vertex[9].Y - Vertex[12].Y,
                          Vertex[9].Z - Vertex[12].Z);
      Normales[14]:=ProduitVectoriel(V1,V2,True);
    end;
    c := TGHTopoColorToGLColor(FBDDEntites.GetColorViseeFromModeRepresentation(FVue3DParams.ModeRepresentation, EE));
    glMaterialfv(GL_FRONT, GL_AMBIENT, addr(c));
    glBegin(GL_TRIANGLES);
    for p := 1 to 6 do       // Triangles pairs
    begin
      q := p + 6;
      r := p + 1;
      if (r > 6) then r := 1;
      PutVertex(p);
      PutVertex(q);
      PutVertex(r);
    end;
    for q := 7 to 12 do   // Triangles impairs
    begin
      p := q - 5;
      if ( p > 6) then p := 1;
      r := p+6;
      PutVertex(p);
      PutVertex(q);
      PutVertex(r);
    end;
    glEnd;
    // abouts (OK)
    glBegin(GL_TRIANGLE_FAN);
     glNormal3d(Normales[13].X, Normales[13].Y, Normales[13].Z);
     //glVertex3d(Vertex[1].X, Vertex[1].Y, Vertex[1].Z);
     for p:=1 to 6 do glVertex3d(Vertex[p].X, Vertex[p].Y, Vertex[p].Z);
    glEnd;    //***** ok
     glBegin(GL_TRIANGLE_FAN);
     glNormal3d(Normales[14].X, Normales[14].Y, Normales[14].Z);
     for p:=12 downto 7 do glVertex3d(Vertex[p].X, Vertex[p].Y, Vertex[p].Z);
    glEnd;
  end;
begin
 FZ := M * FVue3DParams.CoefMagnification;
 AfficherMessage(Format('%s.DrawGLConduits(%d entities)', [ClassName, FBDDEntites.GetNbEntitesVisees()]));
 for i := LOW_INDEX to FBDDEntites.GetNbEntitesVisees() - 1 do DrawTube(FBDDEntites.GetEntiteVisee(i));
end;

procedure TCdrVue3DOpenGLExt.MakeMAILLAGE(const M: double);
begin
  AfficherMessage(Format('%s.MakeMaillage(%d triangles, %d vertex)', [ClassName, FMyMaillage.GetNbTriangles, FMyMaillage.GetNbVertex]));
  FMyMaillage.ConstruireMaillages(M, FVue3DParams.CoefMagnification);     //1.00
end;

// affichage de la vue
procedure TCdrVue3DOpenGLExt.ReconstruitVue();
var
  ErrCode: TGLuint;
  cc, BGglColor: TGLColor;
  procedure QDrawScene();
  var
    QEchelle: Extended;
  begin
    // La matrice du modèle
    glMatrixMode (GL_MODELVIEW);
    glLoadIdentity();
    // Fin des transformations du modèle
    {$IFDEF OPENGL_ANCIENNE_METHODE}
    //------------- transformations d'ensemble
    glTranslated(0,0, -(Fcs2.Z - Fcs1.Z) *  10.00); // FMagnification * 10
    glRotated(-(90 - FVue3DParams.Phi)  , 1,0,0);
    glRotated(FVue3DParams.Theta, 0,0,1);
    {$ELSE}
    gluLookAt(FExcentr.X, FExcentr.Y, FExcentr.Z,
              FExcentr.X, FExcentr.Y, FExcentr.Z,
              0, 0, -1);
    //Le plus intéressant à remarquer est la ligne suivante : 1)gluLookAt(0.3,0.3,0.3,0.0,0.0,0.0,0.3,-0.3,0.3);
    //  En effet, la fonction gluLookAt permet la gestion de la caméra, et donc a fortiori la bonne visualisation de la scène.
    //  Elle a 9 paramètres que l’on va détailler :
    // -les trois premiers servent à placer la caméra sur le point (x,y,z) spécifié ;
    // -les trois suivants servent à orienter la caméra vers le point (x,y,z) spécifié ; ici la caméra est orientée sur le point (0.0,0.0,0.0).
    // -les trois derniers servent à ce que la caméra visualise la direction (x,y,z) spécifiée ; ici la caméra visualise (0.3,-0.3,0.3).

    {$ENDIF OPENGL_ANCIENNE_METHODE}
    AfficherMessageErreur(Format('glTranslated(%.4f, %.4f, %.4f)', [-FExcentr.X, -FExcentr.Y, -FExcentr.Z]));
     // calcul d'excentrement = le cube englobant est centré en (0,0,0)
    glTranslated(-FExcentr.X,
                 -FExcentr.Y,
                 -FExcentr.Z );//* FVue3DParams.CoefMagnification);
    QEchelle := 1.00;
    //glScalef(QEchelle, QEchelle, QEchelle * FVue3DParams.CoefMagnification);
    // le dessin ici
    //************************************
    glDisable(GL_BLEND);
    glDisable(GL_COLOR_MATERIAL);
    // dessin d'objets volumiques
    glLightfv (GL_LIGHT0, GL_AMBIENT, @Flight_ambient); // lumière ambiante
    glEnable(GL_LIGHTING);   	                        // Active l'éclairage
    glEnable(GL_LIGHT0);
    // dessin des volumes cavité
    //glEnable(GL_COLOR_MATERIAL);
    if (edVolumes in FVue3DParams.ElementsDrawn) then glCallList(FglListVOLUMES);
    // dessin du maillage
    //AfficherMessage('-- >Maillage: ' + booltostr(FMyMaillage.IsValidMaillage, 'OK', 'KO'));
    if (FMyMaillage.IsValidMaillage() and (edMaillages in FVue3DParams.ElementsDrawn)) then
    begin
      try
        case FMyMaillage.GetModeDessinMaillage of
          M3D_MESH:       // transparent
          begin
            //glDepthMask(GL_FALSE);
            glEnable(GL_COLOR_MATERIAL);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            glEnable(GL_BLEND);
            glCallList(FglListMAILLAGE);
            glDisable(GL_BLEND);
            glDisable(GL_COLOR_MATERIAL);
          end;
          M3D_WIRE_FRAME:  // fil de fer
          begin
            glDisable(GL_LIGHTING);
            glCallList(FglListMAILLAGE);
          end;
        else
          pass;
        end; //case
      except
      end;
    end; // if (FMyMaillage.IsValidMaillage) then
    // dessin d'objets filaires
    glDisable(GL_LIGHTING);
    glDisable(GL_LIGHT0);
    // le cube enveloppe
    cc := TColorToGLColor(FVue3DParams.LineCube.toTColor());
    glColor3d(cc.R, cc.G, cc.B);
    if (edBounds      in FVue3DParams.ElementsDrawn) then glCallList(FglListCUBE);
    if (edPolygonals  in FVue3DParams.ElementsDrawn) then glCallList(FglListPOLYGONALS);
    if (edANTENNES    in FVue3DParams.ElementsDrawn) then glCallList(FglListRADIANT_SHOTS);
  end;
begin
  // /!\ Eviter les AfficherMessage sauf en débogage: ils ralentissent considérablement l'affichage 3D
  //AfficherMessage(Format('%s.ReconstruitVue()',[ClassName]));
  if (Not FDoDraw) then Exit;
  // on fait en sorte que le réseau est centré en (0,0,0) de la scène OpenGL
  {$IFDEF OPENGL_ANCIENNE_METHODE}
  FExcentr.setFrom(0.50 * (Fcs2.X - Fcs1.X) + Fcs1.X,
                   0.50 * (Fcs2.Y - Fcs1.Y) + Fcs1.Y,
                   0.50 * (Fcs2.Z - Fcs1.Z) + Fcs1.Z);
  {$ELSE}



  {$ENDIF OPENGL_ANCIENNE_METHODE}
  // Préparation du contexte OpenGL
  glViewport(0,0,OpenGLControl1.Width, OpenGLControl1.Height);  // fenêtre de vue
  glEnable(GL_DEPTH_TEST);
  BGglColor := TColorToGLColor(FVue3DParams.ColorBackGround);
  glClearColor(BGglColor.R, BGglColor.G, BGglColor.B, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glClearDepth(1.0);
  glDepthFunc(GL_LESS);
  glShadeModel(GL_SMOOTH);
  glEnable(GL_CULL_FACE);

  {$IFDEF OPENGL_ANCIENNE_METHODE}
     glMatrixMode (GL_PROJECTION);
     glLoadIdentity();
     gluPerspective(FVue3DParams.FovOrZoom, OpenGLControl1.Width / OpenGLControl1.Height, 0.001, 100.0);    // Fonctionne bien
  {$ELSE}


  {$ENDIF OPENGL_ANCIENNE_METHODE}
  // On dessine la scène
  QDrawScene();
  // on envoie
  glFlush();
  ErrCode := glGetError;
  if (ErrCode <> 0) then AfficherMessage(Format('OpenGL error: %d - %s',[ErrCode, glGetString(ErrCode)])); //rsOPENGLERROR);
  // échange buffer
  OpenGLControl1.SwapBuffers;
  // actualisation des coontrôles
  if (Assigned(FProcRefreshControls)) then FProcRefreshControls();
end;
procedure TCdrVue3DOpenGLExt.SetParamsMaillage(const TM: TMNTModeDessinMaillage;
                                               const DoUseDegrade: boolean;
                                               const ColorZMini, ColorZMaxi: TColor;
                                               const Opacity: byte);
begin

  FMyMaillage.SetModeDessinMaillage(TM);
  FMyMaillage.SetMinCouleur(ColorZMini);
  FMyMaillage.SetMaxCouleur(ColorZMaxi);
  FMyMaillage.SetAlphaBlending(Opacity);
  FMyMaillage.SetUsingDegrade(DoUseDegrade);
  if (FVue3DParams.MaillageModeDessin in [M3D_WIRE_FRAME, M3D_MESH]) then FVue3DParams.ElementsDrawn := FVue3DParams.ElementsDrawn + [edMaillages]
                                                                     else FVue3DParams.ElementsDrawn := FVue3DParams.ElementsDrawn - [edMaillages];
  ConstruireScene();
  ReconstruitVue();
end;

end.

