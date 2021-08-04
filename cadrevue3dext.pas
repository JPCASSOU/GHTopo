unit CadreVue3DExt;

{$INCLUDE CompilationParameters.inc}
// Date: 07/11/2013
// Statut: Opérationnel en interne
// 07/04/2014: Export SVG OK
// 08/04/2014: Nouvelle version de Export SVG utilisant SVGCanvasUnit
// 11/02/2015: Paramètres de vue 3D sont groupés dans un TVue3DParams
// 27/04/2016: support du drag/drop souris pour les paramètres de vue 3D (aka GHTopo Delphi)
interface

uses
  StructuresDonnees,
  Common,
  Classes,
  CalculMatriciel3x3,
  UnitEntitesExtended,
  UnitListesSimplesWithGeneriques,
  unitProfilTopo,
  UnitClasseMaillage,
  //FPimage, fpvectorial, fpvutils,    // Penser à installer le paquet fpvectorial (dans $(LAZARUS)/components/fpvectorial/)
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  SysUtils, FileUtil, Forms, Controls,  ExtCtrls,
  Graphics,
  BGRABitmap,
  SVGCanvasUnit;
// types locaux
type TViseesTopo2D = record
  ColorEntite       : TColor;                 // ID unique de l'entité
  Type_Entite       : TTypeDeVisee;             // Type d'entité
  DateLeve          : TDateTime;
  Sub_System        : integer;                 // Sous-réseau
  Une_Station_1_X   : double;                  // Extrémités des visées
  Une_Station_1_Y   : double;
  Une_Station_2_X   : double;                // Extrémités des visées
  Une_Station_2_Y   : double;
  CoordEcrSt1X      : integer;
  CoordEcrSt2X      : integer;
  CoordEcrSt2Y      : integer;
  CoordEcrSt1Y      : integer;
  Depth             : Double;                // Profondeur (distance à l'observateur);
  Drawn             : boolean;
  Highlighted       : boolean;
end;
type TPoint2DDepth = record
  X    : double;
  Y    : double;
  Depth: double;
end;
type TArrayOfTPoint2DDepth = array of TPoint2DDepth;
type TTableAUtiliser = (teuVISEES, teuANTENNES);

//******************************************************************************
type

  { TCdrVue3DExt }

  TCdrVue3DExt = class(TFrame)
    pnlVue: TPanel;
    Vue: TPaintBox;
    procedure VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VuePaint(Sender: TObject);
  private
    { private declarations }
    // position de la souris (début et fin)
    FPos0: TPoint;
    FBDDEntites: TBDDEntites;
    // callback pour rafraichissement
    FProcRefreshControls: TProcRefreshControlsBy3DView;
    // drapeaux
    FDoDraw        : boolean;         // dessin OK
    // paramètres de la vue 3D
    FVue3DParams: TVue3DParams;
    // tables de précalcul
    FCubeEnglobant : array[1..8] of TPoint;
    FTableViseesCenterlines  : array of TViseesTopo2D; // polygonale
    FTableViseesRadiantes    : array of TViseesTopo2D; // radiantes
    FTableVertexProjetesMaillage: TArrayOfTPoint2DDepth;


    //FTableQuads          : array of TQuad;         // table des facettes
    FTableTubesCavite    : TListePortionsTube; // array of TQuad;         // table des facettes
    // variables géométriques calculées
    FLookAt              : TPoint3Df;      // point d'observation
    FOffset              : TPoint3Df;      // décalage après centrage
    FLongueurDiagonale   : double;         // longueur de la diagonale du cube enveloppe de la cavité
    FTransfoInt          : TPoint;
    FRapportME           : double;
    // éléments de la matrice de transformation
    FMatAux: array[1..8] of Double;

    // maillage
    FMyMaillage          : TMaillage;
    // canevas SVG
    FSVGCanvas: TSVGCanvas;

    procedure CalcProjPolygonale(const DoDestTCanvas: boolean; const DoCalcViseesRadiantes: boolean);
    procedure DrawVolumesCavite(const QBmp: TBGRABitmap);
    procedure DrawCube(const QBmp: TBGRABitmap);
    procedure DrawDraftPolygonale();
    procedure DrawEntrances(const QBmp: TBGRABitmap; const DoDrawNames: boolean);
    procedure DrawPolygonales(const QBmp: TBGRABitmap; const EntitiesColored: boolean);
    procedure DrawReferentiel(const QBmp: TBGRABitmap);
    function  Get2DDepthCoordinatesFromXYZ(const QX, QY, QZ: double): TPoint2DDepth;
    function  Get2DDepthCoordinatesFromP3D(const QP: TPoint3Df): TPoint2DDepth;

    function  GetScreenCoordinates(const X, Y: double; const DoConvertTCanvasCoords: boolean): TPoint;
    function MakePortionTubeVisee(const E: TBaseStation; out APortionTube: TPortionTubeVisee): boolean;
    //function MakeFPColor(const C: TColor): TFPColor;
    procedure MakeVolumesCavite();
    function  PrecalculVolumesCavite(const DoDestTCanvas: boolean): boolean;
    procedure QSortDatasByDepth();
    procedure RedessinVue();
    procedure CalcOrigineEtGrandeDiagonale();

    procedure SetParamTransformation(const QTheta, QPhi, QZoom, QMagnification: double; const DoDestTCanvas: boolean; const DoCalcViseesRadiantes: boolean);
    function  PrecalculerMaillage(): boolean;
    procedure TracerMaillage();
    procedure TracerProfils(const TmpBuffer: TBGRABitmap);
    function  IsPointInCube(const QX, QY, QZ: double): boolean; overload;
    function  IsPointInCube(const QP: TPoint3Df): boolean; overload;


  public
    { public declarations }
    function  InitialiseVue3D(const QBDDEntites   : TBDDEntites;
                              const QMyMaillage   : TMaillage;
                              const QVue3DParams  : TVue3DParams;
                              const PC            : TProcRefreshControlsBy3DView): boolean;


    procedure FinaliseVue3D();
    procedure SetAndRedessVue3D(const P: TVue3DParams); overload;
    function  GetTheta()         : double;
    function  GetPhi()           : double;
    function  GetZoom()          : double;
    function  GetFactZ()         : double;
    function  GetColorZMiniReseau(): TColor;
    function  GetColorZMaxiReseau(): TColor;
    function  GetColorZMiniMNT()    : TColor;
    function  GetColorZMaxiMNT()    : TColor;

    function  GetFiltres()       : string;
    function  GetDoFiltrer()     : boolean;

    procedure SetParamsVue3D(const P: TVue3DParams);
    function  GetParamsVue3D(): TVue3DParams;
    //procedure ExporterVue3DEnSVG(const FileName: string; const FM: TModeRepresentationGaleries);
    procedure ExporterVue3DEnSVG_TSVGCanvas(const FileName: string;
                                            const DoMakeXHTML: boolean;
                                            const QParams3D: TVue3DParams;
                                            const QDoExportMNT: boolean;
                                            const QDoExportProfils: boolean);
    procedure ExportGCP(const QFileName: TStringDirectoryFilename);

  end;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

const
  LOW_INDEX      = 0;// anciennement 1
  REFERENTIEL_RSZ = 40;
  REFERENTIEL_RX0 = 5 + REFERENTIEL_RSZ;
  REFERENTIEL_LBL_X  = 'X';
  REFERENTIEL_LBL_Y  = 'Y';
  REFERENTIEL_LBL_Z  = 'Z';



{ TCdrVue3DExt }

function TCdrVue3DExt.InitialiseVue3D(const QBDDEntites   : TBDDEntites;
                                      const QMyMaillage   : TMaillage;
                                      const QVue3DParams  : TVue3DParams;
                                      const PC            : TProcRefreshControlsBy3DView): boolean;
var
  QDevelViseesVisibles: double;
  i: Integer;
begin
  try
    Result       := False;
    FDoDraw      := false;
    for i := Low(FCubeEnglobant) to High(FCubeEnglobant) do FCubeEnglobant[i] := MakeTPoint(0, 0);
    for i := Low(FMatAux) to High(FMatAux) do FMatAux[i] := 0.00;

    AfficherMessage(Format('%s.InitialiseVue3D()', [ClassName]));
    FProcRefreshControls := PC;                 // procédure de rappel
    FBDDEntites          := QBDDEntites;        // pointeur sur les données calculées
    FMyMaillage          := QMyMaillage;        // maillage
    SetLength(FTableViseesCenterlines,0);
    SetLength(FTableViseesCenterlines, FBDDEntites.GetNbEntitesVisees() + 1);
    SetLength(FTableViseesRadiantes  , 0);
    SetLength(FTableViseesRadiantes  , FBDDEntites.GetNbEntitesAntennes() + 1);
    SetLength(FTableVertexProjetesMaillage, 0);
    FTableTubesCavite := TListePortionsTube.Create;
    FTableTubesCavite.ClearListe();

    if (QVue3DParams.DoFiltrer) then FBDDEntites.MetaFiltre(QVue3DParams.Filtres, QDevelViseesVisibles);

    SetAndRedessVue3D(QVue3DParams);
    FVue3DParams := QVue3DParams;
    AfficherMemoryUsage();
    Result := True;
  except
  end;
end;

procedure TCdrVue3DExt.SetAndRedessVue3D(const P: TVue3DParams); overload;
var
  QDevelViseesVisibles: double;
  C1, C2: TPoint3Df;
begin
  // filtres
  //if (P.DoFiltrer) then FBDDEntites.MetaFiltre(P.Filtres, QDevelViseesVisibles);
  FBDDEntites.SetMinMax(P.DoFiltrer);
  CalcOrigineEtGrandeDiagonale();
  // calcul des dégradés (ssi les couleurs changent)
  if ((P.ColorZMiniReseau <> FVue3DParams.ColorZMiniReseau) OR (P.ColorZMaxiReseau <> FVue3DParams.ColorZMaxiReseau)) then
  begin
    FBDDEntites.CalcCouleursByDepth(P.ColorZMiniReseau, P.ColorZMaxiReseau);
  end;
  FVue3DParams := P;

  // paramétrage angles et zooms
  SetParamTransformation(FVue3DParams.Theta,
                         FVue3DParams.Phi,
                         FVue3DParams.FovOrZoom,
                         FVue3DParams.CoefMagnification,
                         True,
                         (edANTENNES in FVue3DParams.ElementsDrawn));

  if (edVolumes in FVue3DParams.ElementsDrawn) then
  begin
    MakeVolumesCavite();                 // Construction des Volumes
    PrecalculVolumesCavite(True);        // Précalcul
  end;
  //PrecalculerMaillage();
  QSortDatasByDepth();                 // trier la topo
  Vue.Invalidate;                      // redessin
  //RedessinVue;
end;

procedure TCdrVue3DExt.FinaliseVue3D();
begin
  try
    FTableTubesCavite.ClearListe();
  finally
    FreeAndNil(FTableTubesCavite);//FTableTubesCavite.Free;
  end;
end;

function TCdrVue3DExt.GetTheta(): double;
begin
  Result := FVue3DParams.Theta;
end;

function TCdrVue3DExt.GetPhi(): double;
begin
  Result := FVue3DParams.Phi;
end;

function TCdrVue3DExt.GetZoom(): double;
begin
  Result := FVue3DParams.FovOrZoom;
end;

function TCdrVue3DExt.GetFactZ(): double;
begin
  Result := FVue3DParams.CoefMagnification;
end;

function TCdrVue3DExt.GetColorZMiniReseau(): TColor;
begin
  Result := FVue3DParams.ColorZMiniReseau;
end;

function TCdrVue3DExt.GetColorZMaxiReseau(): TColor;
begin
   Result := FVue3DParams.ColorZMaxiReseau;
end;

function TCdrVue3DExt.GetColorZMiniMNT(): TColor;
begin
  Result := FVue3DParams.ColorZMiniMNT;
end;

function TCdrVue3DExt.GetColorZMaxiMNT(): TColor;
begin
  Result := FVue3DParams.ColorZMaxiMNT;
end;


function TCdrVue3DExt.GetFiltres(): string;
begin
  Result := FVue3DParams.Filtres;
end;

function TCdrVue3DExt.GetDoFiltrer(): boolean;
begin
  Result := FVue3DParams.DoFiltrer;
end;

procedure TCdrVue3DExt.SetParamsVue3D(const P: TVue3DParams);
begin
  FVue3DParams := P;

end;



function TCdrVue3DExt.GetParamsVue3D(): TVue3DParams;
begin
  Result := FVue3DParams;
end;



// Calcul de la grande diagonale et mise de l'origine au centre du cube englobant
procedure TCdrVue3DExt.CalcOrigineEtGrandeDiagonale();
var
  cnMini, cnMaxi: TPoint3Df;
begin
  cnMini := FBDDEntites.GetMetafilteredCoinBasGauche();
  cnMaxi := FBDDEntites.GetMetafilteredCoinHautDroit();
  //AfficherMessage(Format('%s.ResetParamTransformation()', [ClassName]));
  with FBDDEntites do begin
    FLongueurDiagonale := Hypot3D(cnMaxi.X-cnMini.X,
                                  cnMaxi.Y-cnMini.Y,
                                  cnMaxi.Z-cnMini.Z);
    FOffset.setFrom(-(cnMini.X+ 0.5*(cnMaxi.X - cnMini.X)),
                    -(cnMini.Y+ 0.5*(cnMaxi.Y - cnMini.Y)),
                    -(cnMini.Z+ 0.5*(cnMaxi.Z - cnMini.Z)));

  end;
  //AfficherMessage('--- 0.01');
  //SetParamTransformation(45.00, 32.00, 1.00, 1.00, True, True);
end;
// paramètres de transformation angles en degrés
procedure TCdrVue3DExt.SetParamTransformation(const QTheta, QPhi, QZoom, QMagnification: double;
                                              const DoDestTCanvas: boolean; const DoCalcViseesRadiantes: boolean);
const
  Marge = 5;
var
  c: TPoint2DDepth;
  cnMini, cnMaxi: TPoint3Df;
  MiouTheta, MiouPhi: ValReal;
begin
  FVue3DParams.FovOrZoom  := QZoom;
  FVue3DParams.Theta      := QTheta;
  FVue3DParams.Phi        := QPhi;
  FVue3DParams.CoefMagnification := QMagnification;
  MiouTheta := FVue3DParams.Theta * DEG_TO_RAD;
  MiouPhi   := FVue3DParams.Phi   * DEG_TO_RAD;

  FMatAux[1] := Sin(MiouTheta);
  FMatAux[2] := Sin(MiouPhi);                     // | -sin(A)           +cos(A)         0           0 | | x |   | X |
  FMatAux[3] := Cos(MiouTheta);                   // |                                                 | |   |   |   |
  FMatAux[4] := Cos(MiouPhi);                     // | -cos(A).sin(P)    -sin(A).sin(P)  cos(A)      0 | | y |   | Y |
  FMatAux[5] := FMatAux[3] * FMatAux[2];          // |                                                 |*|   | = |   |
  FMatAux[6] := FMatAux[1] * FMatAux[2];          // | -cos(A).cos(P)    -sin(A).cos(P)  -sin(P)     R | | z |   | Z |
  FMatAux[7] := FMatAux[3] * FMatAux[4];          // |                                                 | |   |   |   |
  FMatAux[8] := FMatAux[1] * FMatAux[4];          // | 0                 0               0           1 | | 1 |   | 1 |

  FTransfoInt.X := 0;
  FTransfoInt.Y := 0;
  FTransfoInt.X :=  Vue.Width  div 2;
  FTransfoInt.Y :=  Vue.Height div 2;
  FRapportME    := (Vue.Width/FLongueurDiagonale) *  FVue3DParams.FovOrZoom;

  CalcProjPolygonale(DoDestTCanvas, DoCalcViseesRadiantes);
  // direction d'observation
  FLookAt.setFrom(Cos(MiouPhi)*Cos(MiouTheta),
                  Cos(MiouPhi)*sin(MiouTheta),
                  sin(MiouPhi));
  // cube
  cnMini := FBDDEntites.GetCoinBasGauche;
  cnMaxi := FBDDEntites.GetCoinHautDroit;
  c := Get2DDepthCoordinatesFromXYZ(cnMini.X, cnMini.Y, cnMini.Z);
  FCubeEnglobant[1] := GetScreenCoordinates(c.X, C.Y, DoDestTCanvas);
  c := Get2DDepthCoordinatesFromXYZ(cnMaxi.X, cnMini.Y, cnMini.Z);
  FCubeEnglobant[2] := GetScreenCoordinates(c.X, C.Y, DoDestTCanvas);
  c :=Get2DDepthCoordinatesFromXYZ(cnMaxi.X, cnMaxi.Y, cnMini.Z);
  FCubeEnglobant[3] := GetScreenCoordinates(c.X, C.Y, DoDestTCanvas);
  c := Get2DDepthCoordinatesFromXYZ(cnMini.X, cnMaxi.Y, cnMini.Z);
  FCubeEnglobant[4] := GetScreenCoordinates(c.X, C.Y, DoDestTCanvas);
  c :=Get2DDepthCoordinatesFromXYZ(cnMini.X, cnMini.Y, cnMaxi.Z);
  FCubeEnglobant[5] := GetScreenCoordinates(c.X, C.Y, DoDestTCanvas);
  c := Get2DDepthCoordinatesFromXYZ(cnMaxi.X, cnMini.Y, cnMaxi.Z);
  FCubeEnglobant[6] := GetScreenCoordinates(c.X, C.Y, DoDestTCanvas);
  c := Get2DDepthCoordinatesFromXYZ(cnMaxi.X, cnMaxi.Y, cnMaxi.Z);
  FCubeEnglobant[7] := GetScreenCoordinates(c.X, C.Y, DoDestTCanvas);
  c := Get2DDepthCoordinatesFromXYZ(cnMini.X, cnMaxi.Y, cnMaxi.Z);
  FCubeEnglobant[8] := GetScreenCoordinates(c.X, C.Y, DoDestTCanvas);
  // précalculer les volumes (inutile ici)
  FDoDraw := True; //PrecalculVolumesCavite(DoDestTCanvas);
end;


procedure TCdrVue3DExt.VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then FPos0 := MakeTPoint(x, y);
end;





procedure TCdrVue3DExt.VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  if (Shift = [ssLeft]) then
  begin
    dx := (X - (self.Width div 2));
    dy := (Y - (self.Height div 2));

    FVue3DParams.Theta := -(X - (self.Width div 2)); //-(X - FPos0.X);
    if (FVue3DParams.Theta > 360.0) then FVue3DParams.Theta := 0.0;
    if (FVue3DParams.Theta <   0.0) then FVue3DParams.Theta := 360.0;

    FVue3DParams.Phi   :=  Y - (self.Height div 2); //Y - FPos0.Y;
    if (FVue3DParams.Phi >  90.0) then FVue3DParams.Phi :=  90.0;
    if (FVue3DParams.Phi <   0.0) then FVue3DParams.Phi :=   0.0;


    SetParamTransformation(FVue3DParams.Theta, FVue3DParams.Phi, FVue3DParams.FovOrZoom, FVue3DParams.CoefMagnification, True, False);
    DrawDraftPolygonale();
    if (Assigned(FProcRefreshControls)) then FProcRefreshControls();

  end;
end;

procedure TCdrVue3DExt.VueMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    SetAndRedessVue3D(FVue3DParams);
  except
    pass;
  end;
end;




function TCdrVue3DExt.GetScreenCoordinates (const X,Y: double; const DoConvertTCanvasCoords: boolean): TPoint;
begin
  if (DoConvertTCanvasCoords) then
  begin
    Result.X := round(X * FRapportME) + FTransfoInt.X;
    Result.Y := Vue.Height - (round(Y * FRapportME) + FTransfoInt.Y);
  end
  else
  begin
    Result.X := round(X * FRapportME) + FTransfoInt.X;
    Result.Y := round(Y * FRapportME) + FTransfoInt.Y;
  end;
end;

procedure TCdrVue3DExt.VuePaint(Sender: TObject);
begin
  RedessinVue();
end;

// calcul de la polygonale projetée
// TODO: Fixer proprement les erreurs de vérif d'étendue
procedure TCdrVue3DExt.CalcProjPolygonale(const DoDestTCanvas: boolean; const DoCalcViseesRadiantes: boolean);
  procedure MiouMiou(const Miou: TTableAUtiliser);
  var
    i, NbV:integer;
    V: TViseesTopo2D;
    E : TBaseStation;
    d1: double;
    PtOut: TPoint2DDepth;
    P: TPoint;
  begin
    case Miou of
      teuVISEES  :
        begin
          NbV := FBDDEntites.GetNbEntitesVisees();
        end;
      teuANTENNES:
        begin
          NbV := FBDDEntites.GetNbEntitesAntennes();
          if ((NbV = 0) OR (not DoCalcViseesRadiantes)) then Exit;
        end;
    end;
    //AfficherMessage(Format('%d entities', [NbV]));
    for i := LOW_INDEX to NbV - 1 do
    begin
      try
        case Miou of
          teuVISEES  : E := FBDDEntites.GetEntiteVisee(i);
          teuANTENNES: E := FBDDEntites.GetEntiteAntenne(i);
        end;
        if (Miou = teuVISEES) then
          V.ColorEntite := FBDDEntites.GetColorViseeFromModeRepresentation(FVue3DParams.ModeRepresentation, E).toTColor()
        else
          V.ColorEntite := clGray;
        V.Type_Entite  := E.Type_Entite;
        V.DateLeve     := E.DateLeve;
        PtOut := Get2DDepthCoordinatesFromP3D(E.PosExtr0);
        V.Une_Station_1_X := PtOut.X;
        V.Une_Station_1_Y := PtOut.Y;
        P := GetScreenCoordinates(PtOut.X, PtOut.Y, DoDestTCanvas);
        V.CoordEcrSt1X:=P.X;
        V.CoordEcrSt1Y:=P.Y;
        d1:=PtOut.Depth;
        PtOut := Get2DDepthCoordinatesFromP3D(E.PosStation);
        V.Une_Station_2_X := PtOut.X;
        V.Une_Station_2_Y := PtOut.Y;
        V.Depth           := 0.50 * (d1+PtOut.Depth);
        P := GetScreenCoordinates(PtOut.X, PtOut.Y, DoDestTCanvas);
        V.CoordEcrSt2X    := P.X;
        V.CoordEcrSt2Y    := P.Y;
        V.Drawn           := E.Enabled;
        case Miou of
          teuVISEES:
          begin
            V.Highlighted := E.Highlighted;
            FTableViseesCenterlines[i] := V;
          end;
          teuANTENNES:
          begin
            V.Highlighted := false;
            FTableViseesRadiantes[i]   := V;
          end;
        end;

      except
        case Miou of // désactivation de l'affichage de l'élément en cas d'erreur
          teuVISEES  : FTableViseesCenterlines[i].Drawn := false;
          teuANTENNES: FTableViseesRadiantes[i].Drawn := false;
        end;
        // TODO: Fixer proprement les erreurs de vérif d'étendue
       end;
    end;
  end;
begin
  MiouMiou(teuVISEES);
  if (DoCalcViseesRadiantes) then MiouMiou(teuANTENNES);
end;
// export de la coupe projetée en GCP  GHCaveDraw Centerline
procedure TCdrVue3DExt.ExportGCP(const QFileName: TStringDirectoryFilename);
var
  fp: TextFile;
  i, NbVisees, NbAntennes: Integer;
  EWE  : TBaseStation;
  V    : TViseesTopo2D;
  PtOutExtr1, PtOutExtr2: TPoint2DDepth;
  PD, PG                : TPoint2DDepth;
  QID: Int64;
  procedure WrtLn(const S: string); inline;
  begin
    writeln(fp, S);
  end;
begin
  //if (Not FDoDraw) then Exit;
  if (Abs(FVue3DParams.Phi) > 0.05) then Exit;

  NbVisees   := FBDDEntites.GetNbEntitesVisees();
  NbAntennes := FBDDEntites.GetNbEntitesAntennes();
  AfficherMessage(Format('%s.ExportGCP(): %s - %d shots - %d radiants', [ClassName, QFileName, NbVisees, NbAntennes]));
  AssignFile(fp, QFileName);
  try
    ReWrite(fp);

    WrtLn(Format('# Coupe projetee: %s - %s', [QFileName, DateTimeToStr(Now())]));
    WrtLn(Format('# Theta = %s; Phi = %s; Magn = %s', [
                   FormatterNombreWithDotDecimal(FVue3DParams.Theta, 2),
                   FormatterNombreWithDotDecimal(FVue3DParams.Phi  , 2),
                   FormatterNombreWithDotDecimal(FVue3DParams.CoefMagnification, 3)]));

    WrtLn('# Base points');
    WrtLn('');
    WriteLn(fp, 'basepoints');
    for i := 0 to NbVisees - 1 do
    begin
      EWE := FBDDEntites.GetEntiteVisee(i);
      // NOTA: Les visées en antennes ne sont pas prises en compte par
      //       les fonctions de recherche et d'indexation de GHCaveDraw

      QID := EWE.getGHCaveDrawIDPtCenterline();
      PtOutExtr1 :=  Get2DDepthCoordinatesFromP3D(EWE.PosExtr0);
      PtOutExtr2 :=  Get2DDepthCoordinatesFromP3D(EWE.PosStation);
      WrtLn(Format(GCD_FMT_BASEPOINTS,
                  [QID  ,
                   EWE.IDTerrain,
                   EWE.Type_Entite,
                   FBDDEntites.GetCouleurEntiteByExpe(EWE).toTColor(), //EWE.ColorEntite,
                   PtOutExtr1.X, PtOutExtr1.Y, EWE.PosStation.Z,
                   PtOutExtr2.X, PtOutExtr2.Y, EWE.PosStation.Z,
                   EWE.PosPG.X, EWE.PosPG.Y, EWE.PosOPG.Z,        // EWE.X2PG, EWE.Y2PG,  EWE.Z1PB,
                   EWE.PosPD.X, EWE.PosPD.Y, EWE.PosOPD.Z         // EWE.X2PD, EWE.Y2PD,  EWE.Z1PH
                   ]));

    end;
    // antennes
    if (NbAntennes > 0) then
    begin
      for i := 0 to NbAntennes - 1 do
      begin
        EWE := FBDDEntites.GetEntiteAntenne(i);
        // NOTA: Les visées en antennes ne sont pas prises en compte par
        //       les fonctions de recherche et d'indexation de GHCaveDraw
        QID := EWE.getGHCaveDrawIDPtAntenne(i);
        PtOutExtr1 :=  Get2DDepthCoordinatesFromP3D(EWE.PosExtr0);
        PtOutExtr2 :=  Get2DDepthCoordinatesFromP3D(EWE.PosStation);
        WrtLn(Format(GCD_FMT_BASEPOINTS,
                    [QID  ,
                     EWE.IDTerrain,
                     EWE.Type_Entite,
                     FBDDEntites.GetCouleurEntiteByExpe(EWE).toTColor(), //EWE.ColorEntite,
                     PtOutExtr1.X, PtOutExtr1.Y, EWE.PosStation.Z,
                     PtOutExtr2.X, PtOutExtr2.Y, EWE.PosStation.Z,
                     EWE.PosPG.X, EWE.PosPG.Y, EWE.PosOPG.Z,        // EWE.X2PG, EWE.Y2PG,  EWE.Z1PB,
                     EWE.PosPD.X, EWE.PosPD.Y, EWE.PosOPD.Z         // EWE.X2PD, EWE.Y2PD,  EWE.Z1PH
                     ]));
      end;
    end;
    WriteLn(fp, 'endbasepoints');
  finally
    Closefile(fp);
  end;
end;



// fabrication des volumes de la scène

function TCdrVue3DExt.MakePortionTubeVisee(const E: TBaseStation; out APortionTube: TPortionTubeVisee): boolean;
var
  P1, P2, P3, P4: TPoint3Df;
  QC: TGHTopoColor;
  function MakeQuad(const QP1, QP2, QP3, QP4: TPoint3Df): TQuad;
  var
    Q: TQuad;
  begin
    Result.Depth := 0.00;
    Result.VertexA := QP1;
    Result.VertexB := QP2;
    Result.VertexC := QP3;
    Result.VertexD := QP4;
  end;
begin
  result := false;
  if (E.Type_Entite in [tgENTRANCE, tgSURFACE, tgVISEE_RADIANTE]) then Exit(false);
  QC := FBDDEntites.GetColorViseeFromModeRepresentation(FVue3DParams.ModeRepresentation, E);
  APortionTube.CouleurTube := QC;
  APortionTube.IdxExpe     := E.eExpe;
  APortionTube.IdxReseau   := E.eReseau;
  APortionTube.IdxSecteur  := E.eSecteur;
  APortionTube.Type_Entite := E.Type_Entite;
  APortionTube.Drawn       := E.Enabled;
  APortionTube.Highlighted := E.Highlighted;
  if (not APortionTube.Drawn) then Exit(false);
  with E do
  begin
    // paroi gauche Quad 1
    P1.setfrom(PosPG.X , PosPG.Y , PosPG.Z);   // X2PG, Y2PG, Z2PB);
    P2.setfrom(PosOPG.X, PosOPG.Y, PosOPG.Z);  // X1PG, Y1PG, Z1PB);
    P3.setfrom(PosOPG.X, PosOPG.Y, PosOPD.Z);  // X1PG, Y1PG, Z1PH);
    P4.setfrom(PosPG.X , PosPG.Y , PosPD.Z);   // X2PG, Y2PG, Z2PH);
    APortionTube.Facettes[1] := MakeQuad(P1, P2, P3, P4);
    // paroi droite Quad 2
    P1.setfrom(PosOPD.X, PosOPD.Y, PosOPG.Z);  // X1PD, Y1PD, Z1PB);
    P2.setfrom(PosPD.X , PosPD.Y , PosPG.Z);   // X2PD, Y2PD, Z2PB);
    P3.setfrom(PosPD.X , PosPD.Y , PosPD.Z);   // X2PD, Y2PD, Z2PH);
    P4.setfrom(PosOPD.X, PosOPD.Y, PosOPD.Z);  // X1PD, Y1PD, Z1PH);
    APortionTube.Facettes[2] := MakeQuad(P1, P2, P3, P4);
    // paroi de dessus Quad 3
    P1.setfrom(PosOPD.X, PosOPD.Y, PosOPD.Z);  // X1PD, Y1PD, Z1PH);
    P2.setfrom(PosPD.X , PosPD.Y , PosPD.Z);   // X2PD, Y2PD, Z2PH);
    P3.setfrom(PosPG.X , PosPG.Y , PosPD.Z);   // X2PG, Y2PG, Z2PH);
    P4.setfrom(PosOPG.X, PosOPG.Y, PosOPD.Z);  // X1PG, Y1PG, Z1PH);
    APortionTube.Facettes[3] := MakeQuad(P1, P2, P3, P4);
    // paroi de dessous Quad 4
    P1.setfrom(PosOPG.X, PosOPG.Y, PosOPG.Z);  // X1PG, Y1PG, Z1PB);
    P2.setfrom(PosPG.X , PosPG.Y , PosPG.Z);   // X2PG, Y2PG, Z2PB);
    P3.setfrom(PosPD.X , PosPD.Y , PosPG.Z);   // X2PD, Y2PD, Z2PB);
    P4.setfrom(PosOPD.X, PosOPD.Y, PosOPG.Z);  // X1PD, Y1PD, Z1PB);
    APortionTube.Facettes[4] := MakeQuad(P1, P2, P3, P4);
    // paroi de face
    P1.setfrom(PosOPG.X, PosOPG.Y, PosOPG.Z);  // X1PG, Y1PG, Z1PB);
    P2.setfrom(PosOPD.X, PosOPD.Y, PosOPG.Z);  // X1PD, Y1PD, Z1PB);
    P3.setfrom(PosOPD.X, PosOPD.Y, PosOPD.Z);  // X1PD, Y1PD, Z1PH);
    P4.setfrom(PosOPG.X, PosOPG.Y, PosOPD.Z);  // X1PG, Y1PG, Z1PH);
    APortionTube.Facettes[5] := MakeQuad(P1, P2, P3, P4);
    // paroi de derrière
    P1.setfrom(PosPD.X , PosPD.Y , PosPG.Z);
    P2.setfrom(PosPG.X , PosPG.Y , PosPG.Z);
    P3.setfrom(PosPG.X , PosPG.Y , PosPD.Z);
    P4.setfrom(PosPD.X , PosPD.Y , PosPD.Z);   // X2PD, Y2PD, Z2PH);
    APortionTube.Facettes[6] := MakeQuad(P1, P2, P3, P4);
    result := True;
  end; //with E do begin
end;

procedure TCdrVue3DExt.MakeVolumesCavite();
var
  i: integer;
  P1, P2, P3, P4: TPoint3Df;
  E: TBaseStation;
  //QC : TColor;
  PTC: TPortionTubeVisee;
begin
  FTableTubesCavite.ClearListe();
  for i := LOW_INDEX to FBDDEntites.GetNbEntitesVisees() - 1 do
  begin
    E  := FBDDEntites.GetEntiteVisee(i);
    //QC := FBDDEntites.GetColorViseeFromModeRepresentation(FVue3DParams.ModeRepresentation, E).toTColor();
    if (MakePortionTubeVisee(E, PTC)) then FTableTubesCavite.AddElement(PTC);
  end;
end;
// précalcul des volumes
function TCdrVue3DExt.PrecalculVolumesCavite(const DoDestTCanvas: boolean): boolean;
var
  i, Nb: integer;
  MyPortionTube: TPortionTubeVisee;
  procedure CalcPortionTube(var PTC: TPortionTubeVisee);
  var
    k: Integer;
    procedure Calc3DQuad(var QD: TQuad);
    var
      PP: TPoint2DDepth;
      U, V, W1, W2, M: TPoint3Df;
      NW, PS: Double;
    begin
      with QD do
      begin
        PP := Get2DDepthCoordinatesFromP3D(VertexA);
        Vertex2D_A := GetScreenCoordinates(PP.X, PP.Y, DoDestTCanvas);
        PP := Get2DDepthCoordinatesFromP3D(VertexB);
        Vertex2D_B := GetScreenCoordinates(PP.X, PP.Y, DoDestTCanvas);
        PP := Get2DDepthCoordinatesFromP3D(VertexC);
        Vertex2D_C := GetScreenCoordinates(PP.X, PP.Y, DoDestTCanvas);
        PP := Get2DDepthCoordinatesFromP3D(VertexD);
        Vertex2D_D := GetScreenCoordinates(PP.X, PP.Y, DoDestTCanvas);
        //test de visibilité
        U.setfrom(VertexB.X-VertexA.X, VertexB.Y-VertexA.Y, VertexB.Z-VertexA.Z);
        V.setfrom(VertexC.X-VertexA.X, VertexC.Y-VertexA.Y, VertexC.Z-VertexA.Z);
        W1:= ProduitVectoriel(U, V);
        U.setfrom(VertexC.X-VertexA.X, VertexC.Y-VertexA.Y, VertexC.Z-VertexA.Z);
        V.setfrom(VertexD.X-VertexA.X, VertexD.Y-VertexA.Y, VertexD.Z-VertexA.Z);
        W2:= ProduitVectoriel(U, V);
        W1.X := W1.X + W2.X;
        W1.Y := W1.Y + W2.Y;
        W1.Z := W1.Z + W2.Z;
        NW := 1e-12 + Hypot3D(W1.X, W1.Y, W1.Z);
        PS := W1.X * FLookAt.X+
              W1.Y * FLookAt.Y+
              W1.Z * FLookAt.Z;
        FacetteVisible := ((PS/NW) > 0.00);
        // calcul de profondeur
        M.setFrom(0.25*(VertexA.X+VertexB.X+VertexC.X+VertexD.X),
                  0.25*(VertexA.Y+VertexB.Y+VertexC.Y+VertexD.Y),
                  0.25*(VertexA.Z+VertexB.Z+VertexC.Z+VertexD.Z));
        Depth := -FMatAux[7] * M.X
                 -FMatAux[8] * M.Y
                 -FMatAux[2] * M.Z;
      end;
    end; // with QD
  begin
    PTC.DepthField := 0.00;
    for k := 1 to 6 do
    begin
      Calc3DQuad(PTC.Facettes[k]);
      PTC.DepthField += PTC.Facettes[k].Depth;
    end;
    PTC.DepthField /= 6;
  end;
begin
  result := false;
  Nb := FTableTubesCavite.GetNbElements();
  if (Nb = 0) then exit(True);
  for i := LOW_INDEX to Nb - 1 do
  begin
    try
      MyPortionTube := FTableTubesCavite.GetElement(i);
      CalcPortionTube(MyPortionTube);
    except
      // TODO: Fixer proprement les erreurs de vérif d'étendue
      // En cas d'erreur, on désactive l'affichage de l'élément
      MyPortionTube.Drawn := false;
    end;
    FTableTubesCavite.PutElement(i, MyPortionTube);
  end;
  result := true;
end; // precalcul

//*****************************
// tri en profondeur
procedure TCdrVue3DExt.QSortDatasByDepth();
  procedure QSortByDepth(var FPolygonale: array of TViseesTopo2D; lidx, ridx: integer);
  var
    k, e, mid: integer;
    Buffer   : TViseesTopo2D;
  begin
    with self do
    begin
      if (lidx >= ridx) then Exit;
      mid := (lidx + ridx) div 2;
      Buffer := FPolygonale[lidx];
      FPolygonale[lidx]:=FPolygonale[mid];
      FPolygonale[mid]:=Buffer;
      e:=lidx;
      for k:=lidx+1 to ridx do
      begin
        if (FPolygonale[k].Depth > FPolygonale[lidx].Depth)  then
        begin
          Inc(e);
          Buffer := FPolygonale[e];
          FPolygonale[e]:=FPolygonale[k];
          FPolygonale[k]:=Buffer;
        end;
      end;
      Buffer := FPolygonale[lidx];
      FPolygonale[lidx]:=FPolygonale[e];
      FPolygonale[e]:=Buffer;
      QSortByDepth(FPolygonale,lidx, e-1);
      QSortByDepth(FPolygonale,e+1, ridx);
    end;
  end;
begin
  QSortByDepth(FTableViseesCenterlines, LOW_INDEX, FBDDEntites.GetNbEntitesVisees() - 1);    // tri de la polygonale;
  FTableTubesCavite.SortByDepthField();                                  // tri des volumes
end;
//******************************************************************************
procedure TCdrVue3DExt.DrawCube(const QBmp: TBGRABitmap);
var
  i: integer;
begin
  FVue3DParams.LineCube.SetTBGRAPen(QBmp.CanvasBGRA.Pen);
  for i := 2 to 4 do
  begin
    QBmp.CanvasBGRA.MoveTo(FCubeEnglobant[i]);
    QBmp.CanvasBGRA.LineTo(FCubeEnglobant[i+4]);
  end;
  QBmp.CanvasBGRA.MoveTo(FCubeEnglobant[1]);
  for i := 2 to 4 do QBmp.CanvasBGRA.LineTo(FCubeEnglobant[i]);
  QBmp.CanvasBGRA.LineTo(FCubeEnglobant[1]);
  for i := 5 to 8 do QBmp.CanvasBGRA.LineTo(FCubeEnglobant[i]);
  QBmp.CanvasBGRA.LineTo(FCubeEnglobant[5]);
end;
procedure TCdrVue3DExt.DrawEntrances(const QBmp: TBGRABitmap; const DoDrawNames: boolean);
const R666 = 3;
var
  i, Nb: integer;
  ETT: TBaseStation;
  MyEntrance: TEntrance;
  PtOut: TPoint2DDepth;
  P: TPoint;
begin
  Nb := FBDDEntites.GetNbEntrances();
  if (Nb = 0) then exit;
  CalcProjPolygonale(True, True); // indispensable: refresh
  //AfficherMessage('  --> Entrees');
  QBmp.CanvasBGRA.Pen.Width := 0;
  QBmp.CanvasBGRA.Pen.Color:=clRED;
  QBmp.CanvasBGRA.Font.Height   := 14;
  QBmp.CanvasBGRA.Font.Style    := [fsBold];
  QBmp.CanvasBGRA.Font.Color    := clBlue;
  QBmp.CanvasBGRA.Brush.Opacity := 255;
  for i := 0 to Nb - 1 do
  begin
    MyEntrance := FBDDEntites.GetEntrance(i);
    if (IsPointInCube(MyEntrance.ePosition)) then
    begin
      PtOut :=  Get2DDepthCoordinatesFromP3D(MyEntrance.ePosition);
      P := GetScreenCoordinates(PtOut.X, PtOut.Y, True);
      QBmp.CanvasBGRA.Brush.Color:=clFuchsia;
      QBmp.CanvasBGRA.EllipseC(P.x, P.y, R666, R666);
      QBmp.CanvasBGRA.Brush.Color := clWhite;
      if (DoDrawNames) then QBmp.CanvasBGRA.TextOut(P.X + R666 + 2, P.Y - R666 - QBmp.CanvasBGRA.Font.Height, MyEntrance.eNomEntree);
    end;
  end;
end;

procedure TCdrVue3DExt.DrawPolygonales(const QBmp: TBGRABitmap; const EntitiesColored: boolean);
var
  i, NbV: integer;
begin
  // centerlines
  QBmp.CanvasBGRA.Pen.Color := clBlack;
  NbV := FBDDEntites.GetNbEntitesVisees();
  for i := 1 to NbV - 1 do    // TODO: A revoir
  begin
    QBmp.CanvasBGRA.Pen.Width := 0;
    with FTableViseesCenterlines[i] do
    begin
      if (Not Drawn) then Continue;
      if (Type_Entite = tgEntrance)       then Continue;
      if (EntitiesColored) then
      begin
        if (Highlighted) then
        begin
          QBmp.CanvasBGRA.Pen.Width := FVue3DParams.ViseesLargeur + 3;
          QBmp.CanvasBGRA.Pen.Color := clGray;
          QBmp.CanvasBGRA.MoveTo(CoordEcrSt1X, CoordEcrSt1Y);
          QBmp.CanvasBGRA.LineTo(CoordEcrSt2X, CoordEcrSt2Y);
        end;
        QBmp.CanvasBGRA.Pen.Color := ColorEntite;
        QBmp.CanvasBGRA.Pen.Width := FVue3DParams.ViseesLargeur;
      end;

      QBmp.CanvasBGRA.MoveTo(CoordEcrSt1X, CoordEcrSt1Y);
      QBmp.CanvasBGRA.LineTo(CoordEcrSt2X, CoordEcrSt2Y);
    end;
  end;
  // visées radiantes
  NbV := FBDDEntites.GetNbEntitesAntennes();
  if ((edANTENNES in FVue3DParams.ElementsDrawn) AND (NbV > 0)) then
  begin
    QBmp.CanvasBGRA.Pen.Color := clGray;
    QBmp.CanvasBGRA.Pen.Width := 0;
    for i := 1 to FBDDEntites.GetNbEntitesAntennes() - 1 do
    begin
      with FTableViseesRadiantes[i] do
      begin
        if (Not Drawn) then Continue;
        if (EntitiesColored and (Type_Entite = tgVISEE_RADIANTE)) then
        begin
          QBmp.CanvasBGRA.MoveTo(CoordEcrSt1X, CoordEcrSt1Y);
          QBmp.CanvasBGRA.LineTo(CoordEcrSt2X, CoordEcrSt2Y);
        end;
      end;
    end;
  end;
end;
procedure TCdrVue3DExt.DrawReferentiel(const QBmp: TBGRABitmap);
var
  rYo       : integer;
  X1, X2, X3: TPoint3Df;
  R1, R2, R3: TPoint2Df;
  Rp        : TPoint;
begin
  // calculs préliminaires
  X1.setfrom(1.0, 0.0, 0.0);
  X2.setfrom(0.0, 1.0, 0.0);
  X3.setfrom(0.0, 0.0, 1.0);

  // transformations en repère local (ne pas utiliser Get2DDepthCoordinates())
  R1.X := -X1.X * FMatAux[1] +
           X1.Y * FMatAux[3];
  R1.Y := -X1.X * FMatAux[5] +
          -X1.Y * FMatAux[6] +
           X1.Z * FVue3DParams.CoefMagnification * FMatAux[4];
  R2.X := -X2.X * FMatAux[1] +
           X2.Y * FMatAux[3];
  R2.Y := -X2.X * FMatAux[5] +
          -X2.Y * FMatAux[6] +
           X2.Z * FVue3DParams.CoefMagnification * FMatAux[4];
  R3.X := -X3.X * FMatAux[1] +
           X3.Y * FMatAux[3];
  R3.Y := -X3.X * FMatAux[5] +
          -X3.Y * FMatAux[6] +
           X3.Z * FVue3DParams.CoefMagnification * FMatAux[4];

  rYo := QBmp.Height - REFERENTIEL_RSZ shl 1;
  QBmp.CanvasBGRA.Brush.Color := FVue3DParams.ColorBackGround;
  QBmp.CanvasBGRA.Font.Color  := clBlue;
  QBmp.CanvasBGRA.Pen.Width   := 0;
  QBmp.CanvasBGRA.Pen.Color   := clRed;
  QBmp.CanvasBGRA.MoveTo(REFERENTIEL_RX0, rYo);
  Rp.x := REFERENTIEL_RX0 + round(REFERENTIEL_RSZ * R1.X);
  Rp.y := rYo - round(REFERENTIEL_RSZ * R1.Y);
  QBmp.CanvasBGRA.LineTo(Rp.X, Rp.Y);
  QBmp.CanvasBGRA.TextOut(Rp.x+2, Rp.y+2, REFERENTIEL_LBL_X);

  QBmp.CanvasBGRA.MoveTo(REFERENTIEL_RX0, rYo);
  Rp.x := REFERENTIEL_RX0 + round(REFERENTIEL_RSZ * R2.X);
  Rp.y := rYo - round(REFERENTIEL_RSZ * R2.Y);
  QBmp.CanvasBGRA.LineTo(Rp.X, Rp.Y);
  QBmp.CanvasBGRA.TextOut(Rp.x+2, Rp.y+2, REFERENTIEL_LBL_Y);

  QBmp.CanvasBGRA.MoveTo(REFERENTIEL_RX0, rYo);
  Rp.x := REFERENTIEL_RX0 + round(REFERENTIEL_RSZ * R3.X);
  Rp.y := rYo - round(REFERENTIEL_RSZ * R3.Y);
  QBmp.CanvasBGRA.LineTo(Rp.X, Rp.Y);
  QBmp.CanvasBGRA.TextOut(Rp.x+2, Rp.y+2, REFERENTIEL_LBL_Z);
end;

function TCdrVue3DExt.Get2DDepthCoordinatesFromXYZ(const QX, QY, QZ: double): TPoint2DDepth;
var
  QPP: TPoint3Df;
begin
  QPP.setFrom(QX+FOffset.X, QY+FOffset.Y, QZ+FOffset.Z);
  Result.X := -(QPP.X) * FMatAux[1] +
               (QPP.Y) * FMatAux[3];
  Result.Y := -(QPP.X) * FMatAux[5]
              -(QPP.Y) * FMatAux[6]+
               (FVue3DParams.CoefMagnification*(QPP.Z)) * FMatAux[4];
  Result.Depth:= (-(QPP.X) * FMatAux[7]
                  -(QPP.Y) * FMatAux[8]
                  -(QPP.Z) * FMatAux[2]);
end;

function TCdrVue3DExt.Get2DDepthCoordinatesFromP3D(const QP: TPoint3Df): TPoint2DDepth;
begin
  result := Get2DDepthCoordinatesFromXYZ(QP.X, QP.Y, QP.Z);
end;

procedure TCdrVue3DExt.DrawVolumesCavite(const QBmp: TBGRABitmap);
var
  i, k: integer;
  MyPortionTube: TPortionTubeVisee;
  procedure DrawQuad(const QD: TQuad);

  var
    P: array[0..3] of TPoint;
    v: integer;
  begin
     if (not QD.FacetteVisible)   then exit;
     P[0]:= QD.Vertex2D_A;  P[1]:= QD.Vertex2D_B;
     P[2]:= QD.Vertex2D_C;  P[3]:= QD.Vertex2D_D;

     QBmp.CanvasBGRA.Pen.Color := clBlack;
     QBmp.CanvasBGRA.Pen.Width := 0;
     QBmp.CanvasBGRA.Brush.Color:= MyPortionTube.CouleurTube.toTColor();
     QBmp.CanvasBGRA.MoveTo(P[0]);
     for v:=1 to 3 do QBmp.CanvasBGRA.LineTo(P[v].X,P[v].Y);
     QBmp.CanvasBGRA.Polygon(P);
  end;
begin
  QBmp.CanvasBGRA.Brush.Opacity := FVue3DParams.FillOpacity;
  for i := 1 to FTableTubesCavite.GetNbElements() - 1 do
  begin
    MyPortionTube := FTableTubesCavite.GetElement(i);
    if (MyPortionTube.Drawn) then
    begin
      for k := 1 to 6 do DrawQuad(MyPortionTube.Facettes[k]);
    end;
  end;
end;

// tracé rapide de la polygonale
// /!\ Ne pas utiliser d'optimisations de niveau supérieur à 2
// (sinon crash SIGILL en -O3 et plus)
procedure TCdrVue3DExt.DrawDraftPolygonale();
var
  //TmpBuffer: TBGRABitmap;
  TmpBuffer: TBitmap;
  R        : TRect;
  i, NbV   : Integer;
  procedure QDrawCube();
  var
    s: Integer;
  begin
    TmpBuffer.Canvas.Pen.Width := 0;
    TmpBuffer.Canvas.Pen.Color:= clgray;
    for s := 2 to 4 do TmpBuffer.Canvas.Line(FCubeEnglobant[s].x  ,  FCubeEnglobant[s].y,
                                             FCubeEnglobant[s+4].x, FCubeEnglobant[s+4].y);
    TmpBuffer.Canvas.MoveTo(FCubeEnglobant[1].X, FCubeEnglobant[1].Y);
    for s := 2 to 4 do TmpBuffer.Canvas.LineTo(FCubeEnglobant[s].x, FCubeEnglobant[s].y);
    TmpBuffer.Canvas.LineTo(FCubeEnglobant[1].x, FCubeEnglobant[1].y);
    for s := 5 to 8 do TmpBuffer.Canvas.LineTo(FCubeEnglobant[s].x, FCubeEnglobant[s].y);
    TmpBuffer.Canvas.LineTo(FCubeEnglobant[5].x, FCubeEnglobant[5].y); //FCubeEnglobant[5]);
  end;
  procedure QDrawReferentiel();
  var
    rYo       : integer;
    X1, X2, X3: TPoint3Df;
    R1, R2, R3: TPoint2Df;
    Rp        : TPoint;
    EWE: Double;
  begin
    // calculs préliminaires
    EWE := FVue3DParams.CoefMagnification * FMatAux[4];
    X1.setfrom(1.0, 0.0, 0.0);
    X2.setfrom(0.0, 1.0, 0.0);
    X3.setfrom(0.0, 0.0, 1.0);
    // transformations en repère local (ne pas utiliser Get2DDepthCoordinates())
    R1.X := -X1.X * FMatAux[1] +
             X1.Y * FMatAux[3];
    R1.Y := -X1.X * FMatAux[5] +
            -X1.Y * FMatAux[6] +
             X1.Z * EWE;
    R2.X := -X2.X * FMatAux[1] +
             X2.Y * FMatAux[3];
    R2.Y := -X2.X * FMatAux[5] +
            -X2.Y * FMatAux[6] +
             X2.Z * EWE; //FVue3DParams.CoefMagnification * FMatAux[4];
    R3.X := -X3.X * FMatAux[1] +
             X3.Y * FMatAux[3];
    R3.Y := -X3.X * FMatAux[5] +
            -X3.Y * FMatAux[6] +
             X3.Z * EWE; // FVue3DParams.CoefMagnification * FMatAux[4];

    rYo := TmpBuffer.Height - REFERENTIEL_RSZ shl 1;
    TmpBuffer.Canvas.Font.Color  := clBlue;
    TmpBuffer.Canvas.Pen.Width   := 0;
    TmpBuffer.Canvas.Pen.Color   := clRed;
    TmpBuffer.Canvas.MoveTo(REFERENTIEL_RX0, rYo);
    Rp.x := REFERENTIEL_RX0 + round(REFERENTIEL_RSZ * R1.X);
    Rp.y := rYo - round(REFERENTIEL_RSZ * R1.Y);
    TmpBuffer.Canvas.LineTo(Rp.X, Rp.Y);
    TmpBuffer.Canvas.TextOut(Rp.x+2, Rp.y+2, REFERENTIEL_LBL_X);

    TmpBuffer.Canvas.MoveTo(REFERENTIEL_RX0, rYo);
    Rp.x := REFERENTIEL_RX0 + round(REFERENTIEL_RSZ * R2.X);
    Rp.y := rYo - round(REFERENTIEL_RSZ * R2.Y);
    TmpBuffer.Canvas.LineTo(Rp.X, Rp.Y);
    TmpBuffer.Canvas.TextOut(Rp.x+2, Rp.y+2, REFERENTIEL_LBL_Y);

    TmpBuffer.Canvas.MoveTo(REFERENTIEL_RX0, rYo);
    Rp.x := REFERENTIEL_RX0 + round(REFERENTIEL_RSZ * R3.X);
    Rp.y := rYo - round(REFERENTIEL_RSZ * R3.Y);
    TmpBuffer.Canvas.LineTo(Rp.X, Rp.Y);
    TmpBuffer.Canvas.TextOut(Rp.x+2, Rp.y+2, REFERENTIEL_LBL_Z);
  end;
begin
  if Not(FDoDraw) then Exit;
  try
    //TmpBuffer := TBGRABitmap.Create(Vue.Width, Vue.Height);
    TmpBuffer := TBitmap.Create;
    R.Left   := Vue.Left;
    R.Top    := Vue.Top;
    R.Bottom := Vue.Top  + Vue.Height;
    R.Right  := Vue.Left + Vue.Width;
    //*)

    TmpBuffer.Width  := Vue.Width;
    TmpBuffer.Height := Vue.Height;
    TmpBuffer.Canvas.Brush.Color := FVue3DParams.ColorBackGround;
    TmpBuffer.Canvas.Pen.Color   := clBlack;
    TmpBuffer.Canvas.Pen.Width   := 0;
    TmpBuffer.Canvas.FillRect(R);
    NbV := Length(FTableViseesCenterlines);
    if (0 = NbV) then Exit;
    for i := 1 to NbV - 1 do    // TODO: A revoir
    begin
      if (not FTableViseesCenterlines[i].Drawn) then Continue;
      TmpBuffer.Canvas.Line(FTableViseesCenterlines[i].CoordEcrSt1X, FTableViseesCenterlines[i].CoordEcrSt1Y,
                            FTableViseesCenterlines[i].CoordEcrSt2X, FTableViseesCenterlines[i].CoordEcrSt2Y);
    end;
    QDrawCube();
    QDrawReferentiel();
    vue.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally
    FreeAndNil(TmpBuffer);
  end;
end;

// redessin de la vue
procedure TCdrVue3DExt.RedessinVue();
var
  TmpBuffer: TBGRABitmap;
  R        : TRect;
begin

  if (Not FDoDraw) then Exit;
  try
    TmpBuffer := TBGRABitmap.Create(Vue.Width, Vue.Height);
      R.Left:= Vue.Left;
      R.Top := Vue.Top;
      R.Bottom :=Vue.Top+Vue.Height;
      R.Right  :=Vue.Left+Vue.Width;
      TmpBuffer.CanvasBGRA.Pen.Width := 0;

      //CanvasBGRA.Pen.Mode:=pmCopy;
      TmpBuffer.CanvasBGRA.Brush.Color   := FVue3DParams.ColorBackGround;
      TmpBuffer.CanvasBGRA.Brush.Opacity := 255; // opaque par défaut
      TmpBuffer.CanvasBGRA.FillRect(R);
      DrawPolygonales(TmpBuffer, true); // polygonales
      if (edVolumes        in FVue3DParams.ElementsDrawn) then DrawVolumesCavite(TmpBuffer);   // dessin des volumes
      if (edENTRANCE_MKS   in FVue3DParams.ElementsDrawn) then DrawEntrances(TmpBuffer, edENTRANCE_NAMES   in FVue3DParams.ElementsDrawn);       // entrées
      if (edBounds         in FVue3DParams.ElementsDrawn) then DrawCube(TmpBuffer);            // cube (en dernier)

      TracerProfils(TmpBuffer);                        // Profils Topo

      if (edReferentiel    in FVue3DParams.ElementsDrawn) then DrawReferentiel(TmpBuffer);     // dessin du référentiel
      //AfficherMessage('Vue 3D ok');
    //DrawBandeauDeuil(TmpBuffer.CanvasBGRA);          // bandeau de deuil
    TmpBuffer.Draw(Vue.Canvas, 0, 0, True);
    if (Assigned(FProcRefreshControls)) then FProcRefreshControls();
  finally
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
  end;
end;

//******************************************************************************
// Réseaux  : OK (feuilles de styles)
// Secteurs : OK (feuilles de styles)
// Expés    : OK (feuilles de styles)
// Profondeurs: OK (couleur à la volée)
// Gris     : OK (feuilles de styles)
procedure TCdrVue3DExt.ExporterVue3DEnSVG_TSVGCanvas(const FileName: string;
                                                     const DoMakeXHTML: boolean;
                                                     const QParams3D: TVue3DParams;
                                                     const QDoExportMNT: boolean;
                                                     const QDoExportProfils: boolean);
const
  NOM_GROUPE_CUBE_ENGLOBANT     = 'CubeEnglobant';
  NOM_GROUPE_MAILLAGE           = 'Maillage';
  NOM_GROUPE_CAVITE             = 'Cavite001';
  NOM_GROUPE_SERIE              = 'Serie%d';
  NOM_GROUPE_PROFILS            = 'ListeProfils';
  NOM_STYLE_PROFIL_DEFAUT       = 'ProfilDefaut';

  NOM_STYLE_SILHOUETTE_DEFAUT   = 'SilhouetteDefaut';
  NOM_STYLE_FOND_CUBE_ENGLOBANT = 'FondCubeEnglobant';
  NOM_STYLE_CUBE_ENGLOBANT      = 'CubeEnglobant';
  NOM_STYLE_MAILLAGE            = 'Maillage';
  FMT_STYLE_ENTRANCE            = 'Entrance%d';
  FMT_STYLE_RESEAU              = 'Reseau%d';
  FMT_STYLE_SECTEUR             = 'Secteur%d';
  FMT_STYLE_SEANCE              = 'Seance%d';

var
  QViewBoxXMini: integer;
  QViewBoxYMini: integer;
  QViewBoxXMaxi: integer;
  QViewBoxYMaxi: integer;
  procedure CalcViewBox();
  const
    QQMax = 2000000000;
  var
    EWE, PC: TPoint;
    i, Nb: Integer;
    MyVertex: TMNTVertex;
    PP: TPoint2DDepth;
  begin
    QViewBoxXMini :=  QQMax;
    QViewBoxYMini :=  QQMax;
    QViewBoxXMaxi := -QQMax;
    QViewBoxYMaxi := -QQMax;
    // Cube englobant = contient tout le dessin
    for i := 1 to 8 do
    begin
      EWE := FCubeEnglobant[i];
      if (EWE.x > QViewBoxXMaxi) then QViewBoxXMaxi := EWE.x;
      if (EWE.x < QViewBoxXMini) then QViewBoxXMini := EWE.x;
      if (EWE.y > QViewBoxYMaxi) then QViewBoxYMaxi := EWE.y;
      if (EWE.y < QViewBoxYMini) then QViewBoxYMini := EWE.y;
    end;
  end;
  procedure DrawCube();
  var
    i: integer;
  begin
    //AfficherMessage('  --> Cube');
    FSVGCanvas.BeginGroupe(NOM_GROUPE_CUBE_ENGLOBANT, '', 0.00, 0.00);
      FSVGCanvas.BeginListeVertex();
        FSVGCanvas.AddVertex(FCubeEnglobant[1].X, FCubeEnglobant[1].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[2].X, FCubeEnglobant[2].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[3].X, FCubeEnglobant[3].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[4].X, FCubeEnglobant[4].Y);
      FSVGCanvas.EndListeVertex();
      FSVGCanvas.DrawPolygon(NOM_STYLE_FOND_CUBE_ENGLOBANT, false);

      FSVGCanvas.BeginListeVertex();
        FSVGCanvas.AddVertex(FCubeEnglobant[5].X, FCubeEnglobant[5].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[6].X, FCubeEnglobant[6].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[7].X, FCubeEnglobant[7].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[8].X, FCubeEnglobant[8].Y);
      FSVGCanvas.EndListeVertex();
      FSVGCanvas.DrawPolygon(NOM_STYLE_CUBE_ENGLOBANT, false);
      for i := 1 to 4 do
      begin
        FSVGCanvas.DrawLine(NOM_STYLE_CUBE_ENGLOBANT,
                           FCubeEnglobant[i].x, FCubeEnglobant[i].y,
                           FCubeEnglobant[i+4].X, FCubeEnglobant[i+4].Y);
      end;
    FSVGCanvas.EndGroupe(NOM_GROUPE_CUBE_ENGLOBANT);
  end;
  //----------------------------------------------------------------------------

  procedure DrawCavite();
  var
    i, k: integer;
    MyPortionTube: TPortionTubeVisee;
    procedure DessinerQuad(const QD: TQuad);
    var
      P: TArrayPoints2Df;
      v: integer;
      r,g,b: byte;
      QX, QY: double;
      QStyle: string;
    begin
      SetLength(P, 4);
      if (Not QD.FacetteVisible) then Exit;
      case QParams3D.ModeRepresentation of
        rgENTRANCES: QStyle := Format(FMT_STYLE_ENTRANCE, [MyPortionTube.IdxEntrance]);
        rgRESEAUX  : QStyle := Format(FMT_STYLE_RESEAU, [MyPortionTube.IdxReseau]);
        rgSECTEURS : QStyle := Format(FMT_STYLE_SECTEUR, [MyPortionTube.IdxSecteur]);
        rgSEANCES  : QStyle := Format(FMT_STYLE_SEANCE, [MyPortionTube.IdxExpe]);
        rgGRAY     : QStyle := NOM_STYLE_SILHOUETTE_DEFAUT;
      end;
      FSVGCanvas.BeginListeVertex();
        FSVGCanvas.AddVertex(QD.Vertex2D_A.x, QD.Vertex2D_A.y);
        FSVGCanvas.AddVertex(QD.Vertex2D_B.x, QD.Vertex2D_B.y);
        FSVGCanvas.AddVertex(QD.Vertex2D_C.x, QD.Vertex2D_C.y);
        FSVGCanvas.AddVertex(QD.Vertex2D_D.x, QD.Vertex2D_D.y);
      FSVGCanvas.EndListeVertex();
      if (QParams3D.ModeRepresentation = rgDEPTH) then FSVGCanvas.DrawPolygonWithoutStyle(clBlack, MyPortionTube.CouleurTube.toTColor(), 0.1, True, false)
                                                  else FSVGCanvas.DrawPolygon(QStyle, false);
    end;
  begin
    FSVGCanvas.BeginGroupe(NOM_GROUPE_CAVITE, '', 0.00, 0.00);
    for i := 1 to FTableTubesCavite.GetNbElements() - 1 do
    begin
      MyPortionTube := FTableTubesCavite.GetElement(i);
      if (MyPortionTube.Drawn) then
        for k := 1 to 6 do DessinerQuad(MyPortionTube.Facettes[k]);
    end;
    FSVGCanvas.EndGroupe(NOM_GROUPE_CAVITE);
  end;
  //----------------------------------------------------------------------------
  procedure ListerStylesEntrances();
  var
    i, Nb: Integer;
    EWE  : TEntrance;
    NS   : String;
    WU: TColor;
  begin
    Nb := FBDDEntites.GetNbEntrances();
    for i := 0 to Nb - 1 do
    begin
      EWE := FBDDEntites.GetEntrance(i);
      NS  := Format(FMT_STYLE_ENTRANCE, [i]);
      WU  := FBDDEntites.GetCouleurEntiteByIdxEntrance(i).toTColor();
      FSVGCanvas.WriteStyleLinePolygoneTexte(NS,
                                             clBlack, 255, 0.1, psSolid,
                                             WU,
                                             QParams3D.FillOpacity, bsSolid,
                                             DEFAULT_FONT_NAME, 3.0, clBlack, 255, [fsBold], EWE.eNomEntree);
    end;
  end;
  procedure ListerStylesReseaux();
  var
    i, Nb: Integer;
    EWE  : TReseau;
    NS   : String;
    WU: TColor;
  begin
    Nb := FBDDEntites.GetNbReseaux();
    for i := 0 to Nb - 1 do
    begin
      EWE := FBDDEntites.GetReseau(i);
      NS  := Format(FMT_STYLE_RESEAU, [i]);
      WU  := FBDDEntites.GetCouleurEntiteByIdxReseau(i).toTColor();
      FSVGCanvas.WriteStyleLinePolygoneTexte(NS,
                                             clBlack, 255, 0.1, psSolid,
                                             WU, QParams3D.FillOpacity, bsSolid,
                                             DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], EWE.NomReseau);

    end;
  end;
  procedure ListerStylesSecteurs();
  var
    i, Nb: Integer;
    EWE  : TSecteur;
    NS   : String;
    WU   : TColor;
  begin
    Nb := FBDDEntites.GetNbSecteurs();
    for i := 0 to Nb - 1 do
    begin
      EWE := FBDDEntites.GetSecteur(i);
      NS  := Format(FMT_STYLE_SECTEUR, [i]);
      WU := FBDDEntites.GetCouleurEntiteByIdxSecteur(i).toTColor();
      FSVGCanvas.WriteStyleLinePolygoneTexte(NS,
                                             clBlack, 255, 0.02, psSolid,
                                             WU, QParams3D.FillOpacity, bsSolid,
                                             DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], EWE.NomSecteur);
    end;
  end;
  procedure ListerStylesSeances();
  var
    i, Nb: Integer;
    EWE  : TExpe;
    NS   : String;
    WU: TColor;
  begin
    Nb := FBDDEntites.GetNbExpes();
    for i := 0 to Nb - 1 do
    begin
      EWE := FBDDEntites.GetExpe(i);
      NS  := Format(FMT_STYLE_SEANCE, [EWE.IDExpe]);
      WU :=  FBDDEntites.GetCouleurEntiteByIdxExpe(EWE.IdxCouleur).toTColor();
      FSVGCanvas.WriteStyleLinePolygoneTexte(NS,
                                             clBlack, 255, 0.1, psSolid,
                                             WU, QParams3D.FillOpacity, bsSolid,
                                             DEFAULT_FONT_NAME, 3.0, clBlack, 255, [], EWE.Commentaire);
    end;
  end;
  procedure MiouMiou(const Miou: boolean);
  begin
    SetParamTransformation(FVue3DParams.Theta, FVue3DParams.Phi, FVue3DParams.FovOrZoom, FVue3DParams.CoefMagnification, Miou, True);
    CalcProjPolygonale(Miou, True);
    PrecalculVolumesCavite(Miou);
  end;
  procedure DrawMaillage();
  var
    i, NbTriangles: Integer;
    MyTriangle: TMNTTriangleABC;
    MyVertexA, MyVertexB, MyVertexC: TMNTVertex;
    VA, VB, VC: TPoint2DDepth;
    PPA, PPB, PPC: TPoint;
  begin
    AfficherMessage('DrawMaillage(): Maillage ' + IIF(FMyMaillage.IsValidMaillage(), 'OK', 'KO'));

    if (not FMyMaillage.IsValidMaillage()) then Exit;
    FSVGCanvas.BeginGroupe(NOM_GROUPE_MAILLAGE, '', 0.00, 0.00);
    NbTriangles := FMyMaillage.GetNbTriangles();
    AfficherMessage('-- Précalcul du maillage');
    if (not PrecalculerMaillage()) then Exit;
    for i := 0 to NbTriangles - 1 do
    begin
      try
        MyTriangle := FMyMaillage.GetTriangle(i);
        if (MyTriangle.Displayed) then
        begin
          VA := FTableVertexProjetesMaillage[MyTriangle.PointA];
          VB := FTableVertexProjetesMaillage[MyTriangle.PointB];
          VC := FTableVertexProjetesMaillage[MyTriangle.PointC];

          PPA := GetScreenCoordinates(VA.X, VA.Y, false);
          PPB := GetScreenCoordinates(VB.X, VB.Y, false);
          PPC := GetScreenCoordinates(VC.X, VC.Y, false);
          FSVGCanvas.BeginListeVertex();
            FSVGCanvas.AddVertex(PPA.X, PPA.Y);
            FSVGCanvas.AddVertex(PPB.X, PPB.Y);
            FSVGCanvas.AddVertex(PPC.X, PPC.Y);
          FSVGCanvas.EndListeVertex();
          FSVGCanvas.DrawPolygon(NOM_STYLE_MAILLAGE, false);
        end;
      except
        pass;
      end;
    end;
    FSVGCanvas.EndGroupe(NOM_GROUPE_MAILLAGE);

  end;
  procedure DrawProfils();
  var
    i, NbProfils: Integer;
    MyProfilTopo: TProfilTopo;
    procedure QTracerProfil(const QProfil: TProfilTopo);
    var
      NbP, p: Integer;
      MyPointProfil: TPoint3DfOrderedByP;
      PP: TPoint2DDepth;
      PPA: TPoint;
    begin
      NbP := QProfil.GetNbPointsProfilTN();

      FSVGCanvas.BeginListeVertex();
        MyPointProfil := QProfil.GetPointProfilTN(0);
        PP := Get2DDepthCoordinatesFromXYZ(MyPointProfil.X, MyPointProfil.Y, MyPointProfil.Z);
        PPA := GetScreenCoordinates(PP.X, PP.Y, false);
        FSVGCanvas.AddVertex(PPA.X, PPA.Y);
        for p := 1 to NbP - 1 do
        begin
          MyPointProfil := QProfil.GetPointProfilTN(p);
          PP := Get2DDepthCoordinatesFromXYZ(MyPointProfil.X, MyPointProfil.Y, MyPointProfil.Z);
          PPA := GetScreenCoordinates(PP.X, PP.Y, false);
          FSVGCanvas.AddVertex(PPA.X, PPA.Y);
        end;
      FSVGCanvas.EndListeVertex();
      FSVGCanvas.DrawPolylign(NOM_STYLE_PROFIL_DEFAUT, false);
    end;
  begin
    NbProfils := FMyMaillage.GetNbProfilsTopo();
    AfficherMessage(Format('DrawProfils(): %d Profils %s', [NbProfils, BoolToStr(FMyMaillage.IsValidMaillage(), 'OK', 'KO')]));
    if (0 = NbProfils) then exit;
    if (not FMyMaillage.IsValidMaillage()) then Exit;

    FSVGCanvas.BeginGroupe(NOM_GROUPE_PROFILS);

        FSVGCanvas.AddVertex(FCubeEnglobant[1].X, FCubeEnglobant[1].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[2].X, FCubeEnglobant[2].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[3].X, FCubeEnglobant[3].Y);
        FSVGCanvas.AddVertex(FCubeEnglobant[4].X, FCubeEnglobant[4].Y);

      for i := 0 to NbProfils - 1 do
      begin;
        MyProfilTopo := FMyMaillage.GetProfilTopo(i);
        QTracerProfil(MyProfilTopo);
      end;

    FSVGCanvas.EndGroupe(NOM_GROUPE_PROFILS);
  end;

begin
  if (Not FDoDraw) then Exit;
  AfficherMessage(Format('%s.ExporterVue3DEnSVG: %s ; %.0f, %.0f', [ClassName,FileName, FVue3DParams.Theta, FVue3DParams.Phi]));
  FSVGCanvas := TSVGCanvas.Create;
  try
    MiouMiou(False); // calcul pour SVG
      // en-tetes
      FSVGCanvas.Commentaire := '';
      FSVGCanvas.Scale       := 1.00;
      CalcViewBox();
      AfficherMessage(Format('-- ViewBox:  %d, %d -> %d, %d', [QViewBoxXMini, QViewBoxYMini, QViewBoxXMaxi, QViewBoxYMaxi]));
      if (Not FSVGCanvas.InitializeDocument(FileName,
                                           DoMakeXHTML,
                                           'Vue 3D by ' + ApplicationName,
                                           Format('%s: Vue 3D (%.0f deg / %.0f deg)', [ExtractFileName(FileName), FVue3DParams.Theta, FVue3DParams.Phi]),
                                           QViewBoxXMini, QViewBoxYMini, QViewBoxXMaxi, QViewBoxYMaxi,
                                           nil)) then Exit;
      FSVGCanvas.BeginPatternsSection;                                                              // patterns
      FSVGCanvas.EndPatternsSection;                                                                // les patterns ici
      FSVGCanvas.BeginStylesSection;                                                                 // section de styles
        FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_CUBE_ENGLOBANT,
                                               QParams3D.LineCube.toTColor(), QParams3D.LineCube.getOpacity(), QParams3D.LineCube.LineWidthInMillimeters, psSolid,
                                               clWhite, 128, bsClear,
                                               DEFAULT_FONT_NAME, DEFAULT_FONT_HEIGHT_IN_MM ,
                                               clBlack, 255, [],
                                               'bounding box cube');   // styles de cube englobant

        FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_SILHOUETTE_DEFAUT,
                                               clBlack, 255, 0.1, psSolid,
                                               clGray, QParams3D.FillOpacity, bsSolid,
                                               DEFAULT_FONT_NAME, DEFAULT_FONT_HEIGHT_IN_MM, clBlack, 255, [], 'Default silhouettes');

        FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_FOND_CUBE_ENGLOBANT,
                                               clBlack, 255, 0.1, psSolid,
                                               clSilver, 128, bsSolid,
                                               DEFAULT_FONT_NAME, DEFAULT_FONT_HEIGHT_IN_MM, clBlack, 255, [], 'Background');

        FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_MAILLAGE,
                                               QParams3D.LineMaillage.toTColor(), QParams3D.LineMaillage.getOpacity(), QParams3D.LineMaillage.LineWidthInMillimeters, psSolid,
                                               clOlive, 64, bsSolid,
                                               DEFAULT_FONT_NAME, DEFAULT_FONT_HEIGHT_IN_MM, clBlack, 255, [], 'Maillage');
        FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_PROFIL_DEFAUT,
                                               clGreen, 192, DEFAULT_PEN_WIDTH_IN_MM, psSolid,
                                               clGreen, 255, bsClear,
                                               DEFAULT_FONT_NAME, DEFAULT_FONT_HEIGHT_IN_MM, clBlack, 255, [], 'Profils');





        case QParams3D.ModeRepresentation of          // les styles de galeries ici
          rgENTRANCES: ListerStylesEntrances();
          rgRESEAUX  : ListerStylesReseaux();
          rgSECTEURS : ListerStylesSecteurs();
          rgSEANCES  : ListerStylesSeances();
        end;
      FSVGCanvas.EndStylesSection();
      FSVGCanvas.BeginDrawingSection();
        DrawCube();                 // cube
        DrawCavite();               // dessin proprement dit
        if (QDoExportMNT)     then DrawMaillage(); // Maillage
        if (QDoExportProfils) then DrawProfils();
      FSVGCanvas.EndDrawingSection();
      FSVGCanvas.FinalizeDocument();         // cloture
    MiouMiou(True);               // restitution du contexte
  finally
    FreeAndNil(FSVGCanvas);//FSVGCanvas.Free;
  end;
end;
function TCdrVue3DExt.PrecalculerMaillage(): boolean;
var
  NbVertexes, i: Integer;
  MyVertex: TMNTVertex;
  procedure QSortByDepth(lidx, ridx: integer);
  var
    k, e, mid: integer;
    Buffer: TPoint2DDepth;
  begin
    with self do
    begin
      if (lidx >= ridx) then Exit;
      mid := (lidx + ridx) div 2;
      Buffer := FTableVertexProjetesMaillage[lidx];
      FTableVertexProjetesMaillage[lidx]:=FTableVertexProjetesMaillage[mid];
      FTableVertexProjetesMaillage[mid]:=Buffer;
      e:=lidx;
      for k:=lidx+1 to ridx do
      begin
        if (FTableVertexProjetesMaillage[k].Depth > FTableVertexProjetesMaillage[lidx].Depth)  then
        begin
          Inc(e);
          Buffer := FTableVertexProjetesMaillage[e];
          FTableVertexProjetesMaillage[e]:=FTableVertexProjetesMaillage[k];
          FTableVertexProjetesMaillage[k]:=Buffer;
        end;
      end;
      Buffer := FTableVertexProjetesMaillage[lidx];
      FTableVertexProjetesMaillage[lidx]:=FTableVertexProjetesMaillage[e];
      FTableVertexProjetesMaillage[e]:=Buffer;
      QSortByDepth(lidx, e-1);
      QSortByDepth(e+1, ridx);
    end;
  end;
begin
  result := false;
  AfficherMessage(Format('%s.PrecalculerMaillage(): Maillage %sVALIDE', [classname, IIF(FMyMaillage.IsValidMaillage(), '', 'IN')]));
  if (not FMyMaillage.IsValidMaillage()) then Exit;
  try
    NbVertexes := FMyMaillage.GetNbVertex();
    SetLength(FTableVertexProjetesMaillage, NbVertexes);
    for i := 0 to NbVertexes - 1 do
    begin
      MyVertex := FMyMaillage.GetVertex(i);
      FTableVertexProjetesMaillage[i] := Get2DDepthCoordinatesFromP3D(MyVertex.Position);
    end;
    //QSortByDepth(0, NbVertexes - 1);
    result := true;
  except
    pass;
  end;
end;

procedure TCdrVue3DExt.TracerMaillage();
begin
  AfficherMessage(Format('%s.TracerMaillage(): Maillage %sVALIDE', [classname, IIF(FMyMaillage.IsValidMaillage(), '', 'IN')]));
  if (not FMyMaillage.IsValidMaillage()) then Exit;
end;
procedure TCdrVue3DExt.TracerProfils(const TmpBuffer: TBGRABitmap);
var
  i, Nb: Integer;
  procedure QTracerProfil(const QProfil: TProfilTopo);
  var
    NbP, p: Integer;
    MyPointProfil: TPoint3DfOrderedByP;
    PP: TPoint2DDepth;
    PC: TPoint;
  begin
    NbP := QProfil.GetNbPointsProfilTN();
    QProfil.LineAttributes.SetTBGRAPen(TmpBuffer.CanvasBGRA.Pen);
    MyPointProfil := QProfil.GetPointProfilTN(0);
    PP := Get2DDepthCoordinatesFromXYZ(MyPointProfil.X, MyPointProfil.Y, MyPointProfil.Z);
    PC := GetScreenCoordinates(PP.X, PP.Y, True);
    TmpBuffer.CanvasBGRA.MoveTo(PC);
    for p := 1 to NbP - 1 do
    begin
      MyPointProfil := QProfil.GetPointProfilTN(p);
      PP := Get2DDepthCoordinatesFromXYZ(MyPointProfil.X, MyPointProfil.Y, MyPointProfil.Z);
      PC := GetScreenCoordinates(PP.X, PP.Y, True);
      TmpBuffer.CanvasBGRA.LineTo(PC);
    end;
  end;
begin
  if (not FMyMaillage.IsValidMaillage()) then Exit;
  Nb := FMyMaillage.GetNbProfilsTopo();
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do QTracerProfil(FMyMaillage.GetProfilTopo(i));
end;



function TCdrVue3DExt.IsPointInCube(const QX, QY, QZ: double): boolean;
var
  cnMini, cnMaxi: TPoint3Df;
begin
  cnMini := FBDDEntites.GetCoinBasGauche;
  cnMaxi := FBDDEntites.GetCoinHautDroit;
  result := IsInRange(QX, cnMini.X, cnMaxi.X) and
            IsInRange(QY, cnMini.Y, cnMaxi.Y) and
            IsInRange(QZ, cnMini.Z, cnMaxi.Z);
end;

function TCdrVue3DExt.IsPointInCube(const QP: TPoint3Df): boolean;
begin
  self.IsPointInCube(QP.X, QP.Y, QP.Z);
end;


end.

