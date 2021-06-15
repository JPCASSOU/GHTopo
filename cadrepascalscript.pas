unit CadrePascalScript;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Graphics,
  dateutils,
  math,
  LazFileUtils,
  FastGEO,
  CallDialogsStdVersion,
  frmPSDrawing2D,
  evaluateur_expressions,
  {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitClasseMaillage,
  UnitFichesTopo,

  //GenerationFichesPointsTopo,
  UnitObjetSerie,
  //unitProfilTopo,
  {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
  Classes, SysUtils, Forms, Controls, Buttons, ActnList, ExtCtrls, PairSplitter, StdCtrls, Dialogs, ComCtrls,
  SynEdit, SynHighlighterPas,
  //Ipfilebroker,
  uPSComponent,
  uPSCompiler,
  uPSRuntime,
  uPSComponent_Default,
  uPSUtils,
  ubarcodes,
  BGRABitmap,
  Clipbrd,
  UnitRemoteFunctions
  ;

//type TVectorResult = array[0..31] of string;
type

  { TCdrPascalScript }

  TCdrPascalScript = class(TFrame)
    acLoadPSScript: TAction;
    acPSExecute: TAction;
    acPSStop: TAction;
    acSavePSScript: TAction;
    acNewScript: TAction;
    acCopyOutput: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    AcnLstPascalScript: TActionList;
    editPascalScript: TSynEdit;
    editPSOutPut: TSynEdit;
    ImgLstPascalScript: TImageList;
    lbStatusPSScript: TStaticText;
    lsbPSFunctions: TListBox;
    lsbPSMessages: TListBox;
    memoHelp: TMemo;
    PageControlMsgsOutputs: TPageControl;
    PairSplitter4: TPairSplitter;
    PairSplitter6: TPairSplitter;
    PairSplitterSide11: TPairSplitterSide;
    PairSplitterSide12: TPairSplitterSide;
    PairSplitterSide7: TPairSplitterSide;
    PairSplitterSide8: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlEditor: TPanel;
    pnlButtons: TPanel;
    PSCustomPlugin1: TPSCustomPlugin;
    PSImport_Classes1: TPSImport_Classes;
    PSScriptDebugger1: TPSScriptDebugger;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SynPasSyn1: TSynPasSyn;
    tabShtCompiloMessages: TTabSheet;
    tabShtConsoleOutput: TTabSheet;
    procedure acCopyOutputExecute(Sender: TObject);
    procedure acLoadPSScriptExecute(Sender: TObject);
    procedure acNewScriptExecute(Sender: TObject);
    procedure acPSExecuteExecute(Sender: TObject);
    procedure acPSStopExecute(Sender: TObject);
    procedure acSavePSScriptExecute(Sender: TObject);
    procedure lsbPSFunctionsDblClick(Sender: TObject);
    procedure lsbPSFunctionsSelectionChange(Sender: TObject; User: boolean);
    procedure PSScriptDebugger1AfterExecute(Sender: TPSScriptDebugger);
    procedure PSScriptDebugger1Compile(Sender: TPSScriptDebugger);

    procedure PSScriptDebugger1Line(Sender: TObject);

  strict private
    FEvaluateurExpressions: TEvaluateurExpressions;
    FpTextFiles: Array[0..9] of TextFile;   // Pour les fonctions fopen et fclose
    // aide en ligne des fonction
    FOnLineHelpOfFuncs: TStringList;
    // la série courante
    FCurrentSerie: TObjSerie;
    FCurrentSerieAssigned: boolean;


    procedure DisplayHelpForProc(const MyProc: TPSRegProc);

    // vers la console
    procedure cls();

    procedure DisplaySystemInfos();



    procedure printf(const FMT: string; const Argv: array of const);
    procedure DispPSOutput(const Msg: string);
    // éval expressions
    function  EvalExpr(const Expr: string): double;
    // opérations sur les fichiers (10 descripteurs max)
    function  fopen(const Descr: integer; const QFileName: string; const Mode: Char): boolean;
    function  feof(const Descr: integer): boolean;
    procedure fprint(const Descr: integer; const QLine: string);
    procedure fprintf(const Descr: integer; const FMT: string; const Argv: array of const);
    function  freadln(const Descr: integer): string;
    procedure fclose(const Descr: integer);
    procedure fcloseall();
    procedure ForceDirectory(const Dir: string);
    function  GetPathDelimiter(): string;



    procedure RecenserAdditionalProcs(Sender: TPSScriptDebugger);
    // spécifique incorporé dans GHTopo
    {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    // Fiches des stations
    function  BeginListeFichesStations(): boolean;
    procedure AddFicheStation(const QSr, QSt: integer);
    procedure ExportFichesToPDF(const QFilename: string; const QCompressed: boolean);
    function  GetNbFichesStations(): integer;
    procedure EndListeFichesStations();
    // BDD cavité
    function  DT_GetNbreStationsOfSerieByIdx(const Idx: integer): integer;
    function  DT_GetNbreStationsOfSerieByNoSerie(const NoSerie: integer): integer;
    function  DT_GetNumeroEtNomDeSerieByIdx(const Idx: integer; out QNumSerie: TNumeroSerie; out QNomSerie: string; out QNbVisees: integer): boolean;
    function  DT_ModifyEntrance(const QIdx: integer; const RefSer, RefSt: integer; const XEntree, YEntree, ZEntree: double; const R, G, B: integer; const NomEntree, IDTerrain, Observ: string): boolean;
    //function  DT_ExtractDataFromVisee(const QNumeroSerie: TNumeroSerie; const QNumeroStation: TNumeroStation; out V: array of string): boolean;

    function  DT_BeginNewSerie(const QNumeroSerie: integer;
                               const QNoEntrance, QNoReseau: integer;
                               const QSerDep, QPtDep, QSerArr, QPtArr: integer;
                               const QName: string): boolean;
    procedure DT_RemoveSerieByNumSerie(const QNumeroSerie: integer);
    procedure DT_RemoveSerieByIdx(const Idx: integer);

    function  DT_AddNewVisee(const QExpe, QCode: integer;
                             const QTypeVisee, QSecteur: integer;
                             const QLong, QAz, QPente, QLG, QLD, QHZ, QHN: double;
                             const QIDTerrain, QObserv: string): boolean;


    procedure DT_EndNewSerie();
    // Entrées
    procedure DT_AddNewEntrance(const RefSer, RefSt: integer; const XEntree, YEntree, ZEntree: double; const R, G, B: integer; const NomEntree, IDTerrain, Observ: string);
    // Réseaux
    procedure DT_AddNewReseau(const QTypeReseau: integer; const R, G, B: integer; const QNomReseau, QObsReseau: string);


    // Coordonnées d'une station
    function  BDE_ExtractCoordsFromSerSt(const QSerie: TNumeroSerie; const QStation: TNumeroStation; out QX, QY, QZ: double): boolean;
    // Maillages
    function  MNT_LoadMaillage(const QFilenameMAI: string): integer;
    function  MNT_GetNbVertex(): integer;
    function  MNT_GetNbTriangles(): integer;
    procedure MNT_Unload();
    procedure MNT_AddProfil(const ProfilName: string; const X1, Y1, X2, Y2: double);
    function  MNT_GetNbProfils(): integer;
    procedure MNT_RemoveProfil(const Idx: integer);
    procedure MNT_ClearProfils();
    function  MNT_ExtractBounds(out QX1, QY1, QZ1, QX2, QY2, QZ2: double): boolean;
    function  MNT_ExtractAltitudeFromXY(const QX, QY: double; out QZ: double): boolean;
    (*
    X1, X1: Origine du premier profil
    X2, Y2: Extrémité du premier profil
    X3, Y3: Un point non aligné aux deux précédents; la zone couverte est un rectangle
            contenant les points 1, 2 et 3
    NbX   : Nombre de profils sur x
    NbY   : Nombre de profils sur y

    Sens:
    1: Sur X
    2: Sur Y
    4: Les deux
    //*)
    function MNT_GenerateSetOfProfils(const X1, Y1, X2, Y2, X3, Y3: double;
                                      const NbX, NbY: integer; const Sens: byte): boolean;
    procedure MNT_CheckerAltimetrieEntrees(const DoAdjustAtMNT: boolean; const DeltaZMax: double);
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
    // display graphisme
    function  PS2D_BeginDrawing(const w, h: integer): boolean;
    procedure PS2D_EndAndDisplayDrawing();
    procedure PS2D_SetBackgroundColor(const R, G, B, A: byte);
    
    function  PS2D_AddStyleSheet(const QStylename: string;
                                 const QPenColorR, QPenColorG, QPenColorB, QPenOpacity: byte; const QPenWidtdhInPX: byte;
                                 const QBshColorR, QBshColorG, QBshColorB, QBshOpacity: byte; const QBshStyle: byte;
                                 const QFntName: string; const QFntColorR, QFntColorG, QFntColorB, QFntOpacity: byte;
                                 const QFntSizeInPX: byte; const QFntStyle: byte): integer;
    procedure PS2D_SetStyleSheet(const Idx: integer);
    function  PS2D_BeginPolyline(const QName: string; const Closed: boolean): boolean;
    procedure PS2D_AddVertex(const QX, QY: double);
    procedure PS2D_EndPolyline();
    function  PS2D_AddEllipse(const QX, QY, QR1, QR2: double): boolean;
    function  PS2D_AddRectangle(const QX1, QY1, QX2, QY2: double): boolean;
    function  PS2D_AddLine(const QX1, QY1, QX2, QY2: double): boolean;

    function  PS2D_AddTexte(const QX, QY, AngleOrientation: double; const QAlignment: byte; const Texte: string): boolean;
    // FTP transfert
    function  FTP_BeginConnexion(const QHost: string; const QPort: integer; const QUser, QPassword: string;  out QReturnCode: integer; out QReturnMsg: string): boolean;
    function  FTP_CreateDirectory(const PathFromRoot, QDirectory: TStringDirectoryFilename): boolean;
    procedure FTP_SetRemoteDirectory(const PathFromRoot: TStringDirectoryFilename);
    function  FTP_GetFileName(const Idx: integer): String;
    function  FTP_GetNbFilesOfLast(): integer;
    function  FTP_ListFilenamesOf(const Path: string): integer;
    function  FTP_SendFile(const QFilename: string): boolean;
    procedure FTP_EndConnexion();
    function  ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean; register;


    // QRCode
    procedure QRCode_Generate(const ImgWidth: integer; const QFilenamePNG: string; const QTextToEncode: string);

  private
    FPSDrawing2D: TdlgDispGraphisme2D;
    {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    FDocuTopo  : TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FMyMaillage: TMaillage;
    FLesFichesTopo: TFichesTopo;
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
    FTransfertFTP: TRemoteServicesFTP;
    procedure ListerPSFunctions();
    procedure DispPSCompilerMsg(const Msg: string);
    procedure NewScript(const ScriptName: string = 'NewScript01');

    ////////////////////////////////////////////////////////////////////////////////
    procedure InitCaptions();
  public
    {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    function Initialiser(const QEvaluateurExpr: TEvaluateurExpressions;
                         const QD: TToporobotStructure2012;
                         const QBDDEntites: TBDDEntites;
                         const QMaillage: TMaillage): boolean;
    {$else}
    function Initialiser(const QEvaluateurExpr: TEvaluateurExpressions): boolean;
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
end;
implementation
uses
  DGCDummyUnit;
CONST DOSSIER_DE_MES_SCRIPTS = '0000_ScriptsPerso';

{$R *.lfm}
function psCompilerBeforeOutput(Sender: TPSPascalCompiler): Boolean;
var
  s:string;
begin
  Result:=True;
  (*
  if MakePSSyntaxDoc(Sender, s, sdmXml) then
    with TStringList.create do
    begin
      Text := s;
      SaveToFile(GetGHTopoDirectory() + 'PSMySyntax.xml');
      Free;
    end;             //*)
end;

function TCdrPascalScript.ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean; register;
{ the OnUses callback function is called for each "uses" in the script.
  It's always called with the parameter 'SYSTEM' at the top of the script.
  For example: uses ii1, ii2;
  This will call this function 3 times. First with 'SYSTEM' then 'II1' and then 'II2'.
}
begin
  if Name = 'SYSTEM' then
  begin

    //Sender.AddDelphiFunction('procedure MyOwnFunction(Data: string)');
    { This will register the function to the script engine. Now it can be used from within the script. }


    Result := True;
  end else
    Result := False;
end;

procedure TCdrPascalScript.QRCode_Generate(const ImgWidth: integer; const QFilenamePNG: string; const QTextToEncode: string);
const MARGE_PERIMETRIQUE = 2;
var
  QR: TBarcodeQR;
  M: TMatriceResultat;
  i, j, n: Integer;
  TailleCarre: double;
  BMP: TBGRABitmap;
  procedure QDrawCarre(const u, v: integer);
  var
    TC: double;
  begin
    BMP.CanvasBGRA.Brush.Color := IIF(M[u, v] = 0, clWhite, clBlack);
    BMP.CanvasBGRA.Pen.Color   := BMP.CanvasBGRA.Brush.Color;
    TC := MARGE_PERIMETRIQUE * TailleCarre;
    BMP.CanvasBGRA.Rectangle(round(  u   * TailleCarre + TC), round(  v   * TailleCarre + TC),
                         round((u+1) * TailleCarre + TC), round((v+1) * TailleCarre + TC));

  end;
begin
  DispPSOutput(Format('MakeQRCode(%d, %s, %s)', [ImgWidth, QFilenamePNG, QTextToEncode]));
  if ('' = trim(QTextToEncode)) then exit;
  QR := TBarcodeQR.Create(nil);
  try
    QR.ECCLevel := eBarcodeQR_ECCLevel_H;
    QR.Text     := mysqli_real_escape_string(QTextToEncode);
    QR.Generate;
    QR.MakeMatriceResultat;
    M := QR.GetMatrixQRResultat;
    n := QR.GetNbQRTaille;
    TailleCarre := ImgWidth / (MARGE_PERIMETRIQUE + n + MARGE_PERIMETRIQUE);
    DispPSOutput(inttostr(n));
    BMP := TBGRABitmap.Create(ImgWidth + 2, ImgWidth + 2);
    try
      BMP.CanvasBGRA.Pen.Color  := clBlack;
      BMP.CanvasBGRA.Pen.Width  := 0;
      BMP.CanvasBGRA.Pen.Style  := psSolid;

      BMP.CanvasBGRA.Brush.Color  := clWhite;
      BMP.CanvasBGRA.Brush.Style  := bsSolid;
      BMP.CanvasBGRA.Brush.Opacity := 255;
      BMP.CanvasBGRA.FillRect(0, 0, BMP.Width, BMP.Height);
      //BMP.SaveToClipboardFormat(ctImage);


      for i := 0 to n - 1 do
        for j := 0 to n - 1 do
          QDrawCarre(i, j);

      //Clipboard.Assign(BMP);
      BMP.SaveToFile(QFilenamePNG);
      //BMP.SaveToFile(QFilenamePNG);

    finally
      FreeAndNil(BMP);
    end;


  finally
    FreeAndNil(QR);
  end;
end;

// wrappers pour fonctions standard
// f en préfixe des fonctions, convention C aka fabs(x)
function fabs(const X: Double): Double; inline;
begin
  result := abs(X);
end;

function fexp(X: Double): Double; inline;
begin
  result := exp(X);
end;
function farctan(X: Double): Double; inline;
begin
  result := arctan(X);
end;
function farctan2(Y, X: Double): Double; inline;
begin
  result := arctan2(Y, X);
end;
function fln(X: Double): Double; inline;
begin
  result := ln(X);
end;
function flog(X: Double): Double; inline;
begin
  result := log10(X);
end;

////////////////////////////////////////////////////////////////////////////////

{ TCdrPascalScript }
{$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
function TCdrPascalScript.Initialiser(const QEvaluateurExpr: TEvaluateurExpressions;
                                      const QD: TToporobotStructure2012;
                                      const QBDDEntites: TBDDEntites;
                                      const QMaillage: TMaillage): boolean;
{$else}
function TCdrPascalScript.Initialiser(const QEvaluateurExpr: TEvaluateurExpressions): boolean;
{$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
var
  i: Integer;
begin
  result := false;
  FEvaluateurExpressions := QEvaluateurExpr;
  FOnLineHelpOfFuncs := TStringList.Create;
  FOnLineHelpOfFuncs.Sorted := false;
  FOnLineHelpOfFuncs.Clear;
  FPSDrawing2D           := nil;
  {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    FCurrentSerieAssigned := false;
    FLesFichesTopo       := nil;
    FDocuTopo   := QD;
    FBDDEntites := QBDDEntites;
    FMyMaillage := QMaillage;
  {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
  PSScriptDebugger1.Compile;
  PSScriptDebugger1.ClearBreakPoints;
  InitCaptions();
  result := true;
end;
procedure TCdrPascalScript.RecenserAdditionalProcs(Sender: TPSScriptDebugger);
var
  EWE: String;
begin
 // sender.Defines.Clear;
  //sender.Defines.Add('TGHStringArray = array[0 .. 63] of string');



  sender.AddFunction(@pass                     , 'procedure pass;');
  sender.AddFunction(@GetGHTopoVersion         , 'function GetGHTopoVersion(): string;');
  sender.AddFunction(@GetGHTopoDirectory       , 'function GetGHTopoDirectory(): string');
  sender.AddFunction(@FileExists               , 'function FileExists(const NF: string): boolean;');
  sender.AddFunction(@ExtractFileExt           , 'function ExtractFileExt(const FileName: string): string;');
  sender.AddFunction(@ExtractFileName          , 'function ExtractFileName(const FileName: string): string;');
  sender.AddFunction(@GetGUID                  , 'function GetGUID(): string;');

  //sender.AddFunction(@split                    , 'function Split(const MyStr: string; const Sep: char): TGHStringArray;');

  sender.AddFunction(@Format                   , 'function format(Const Fmt : String; const Args : Array of const) : String;');
  sender.AddFunction(@Format                   , 'function sprintf(Const Fmt : String; const Args : Array of const) : String;');
  sender.AddFunction(@KMLColor                 , 'function KMLColor(const R, G, B, A: byte): string;');
  sender.AddFunction(@ConversionCoordonneesIsoleesEPSG, 'function ConversionCoordonneesIsoleesEPSG(const Src, Tgt: integer; const PointInX, PointInY: double; out PointOutX, PointOutY: double): boolean;');
  sender.AddFunction(@ConvertirEnNombreReel    , 'function StrToFloatDef(const S: string; const Default: double): double;');
  sender.AddFunction(@FormatterIDTerrainStation, 'function FormatterIDTerrainStation(const S: string): string;');
  sender.AddFunction(@CalculerDeclinaisonMagnetique, 'function CalculerDeclinaisonMagnetique(const Lat, Lon, Alt: double; const YYYY, MM, DD: word): double;');

  // boites de dialogue
  sender.AddFunction(@ShowMessage              , 'procedure ShowMessage(const msg: string);');
  sender.AddFunction(@ShowMessage              , 'procedure MsgBox(const msg: string);');
  sender.AddFunction(@GHTopoQuestionOuiNon     , 'function  QuestionOuiNon(const Msg: string): boolean;');
  sender.AddFunction(@GHTopoInputQuery         , 'function  InputQuery(const ACaption, APrompt: string; var QValeur: string): boolean;');
  sender.AddFunction(@GHTopoInputPasswordQuery , 'function  InputPasswordQuery(const ACaption, APrompt: string; var QPWD: string): boolean;');

  sender.AddFunction(@DoDialogSaveFile         , 'function  DialogSaveFile(const QFilter: string; const QDefaultExt: string; var QFileName: string; out idxFilter: integer): boolean;');
  sender.AddFunction(@DoDialogOpenFile         , 'function  DialogOpenFile(const QFilter: string; const QDefaultExt: string; var QFileName: string): boolean;');
  sender.AddFunction(@SelectionCouleurToporobot, 'function  SelectionCouleurToporobot(const OldIdx: integer): Integer;');
  sender.AddFunction(@SelectColor              , 'function  SelectColor(): string;');
  // fonctions math additionnelles : certaines fonctions ne sont pas acceptées telles quelles
  sender.AddFunction(@tan                      , 'function tan(x : double) : double;'); // added by JPC
  sender.AddFunction(@arcsin                   , 'function arcsin(x : double) : double;'); // added by JPC
  sender.AddFunction(@arccos                   , 'function arccos(x : double) : double;'); // added by JPC
  sender.AddFunction(@farctan                  , 'Function arctan(x : double) : double;'); // added by JPC
  sender.AddFunction(@farctan2                 , 'Function arctan2(y, x : double) : double;'); // added by JPC
  sender.AddFunction(@sinh                     , 'Function sinh(x : double) : double;'); // added by JPC
  sender.AddFunction(@cosh                     , 'Function cosh(x : double) : double;'); // added by JPC
  sender.AddFunction(@tanh                     , 'Function tanh(x : double) : double;'); // added by JPC
  sender.AddFunction(@fexp                     , 'function exp(x : double) : double;');
  sender.AddFunction(@fln                      , 'function ln(x : double) : double;');
  sender.AddFunction(@flog                     , 'function log(x : double) : double;');
  {$IFNDEF RASPBERRY_PI} // FPC pour ARM ne propose pas ces fonctions
  sender.AddFunction(@floor64                  , 'function floor(x : double) : Int64;');
  sender.AddFunction(@ceil64                   , 'function ceil(x : double) : Int64;');
  {$ENDIF}
  sender.AddFunction(@fabs                     , 'function fabs(x : double) : double;');
  sender.AddFunction(@power                    , 'function power(const X, Y: double): double;');
  sender.AddFunction(@power                    , 'function pow(const X, Y: double): double;');
  sender.AddFunction(@radtodeg                 , 'function radtodeg(x : double) : double;');
  sender.AddFunction(@degtorad                 , 'function degtorad(x : double) : double;');
  sender.AddFunction(@radtograd                , 'function radtograd(x : double) : double;');
  sender.AddFunction(@gradtorad                , 'function gradtorad(x : double) : double;');
  sender.AddFunction(@degtograd                , 'function degtograd(x : double) : double;');
  sender.AddFunction(@gradtodeg                , 'function gradtodeg(x : double) : double;');
  sender.AddFunction(@DegMinSec2DegDec         , 'function DegMinSec2DegDec(const S: string): double;');
  // trigo en degrés
  sender.AddFunction(@sind                     , 'function sind(x : double) : double;');
  sender.AddFunction(@cosd                     , 'function cosd(x : double) : double;');
  sender.AddFunction(@tand                     , 'function tand(x : double) : double;');
  // trigo en grades
  sender.AddFunction(@sing                     , 'function sing(x : double) : double;');
  sender.AddFunction(@cosg                     , 'function cosg(x : double) : double;');
  sender.AddFunction(@tang                     , 'function tang(x : double) : double;');


  sender.AddFunction(@Now                      , 'function Now(): double;');
  sender.AddFunction(@Now                      , 'function Maintenant(): double;');

  sender.AddFunction(@DateTimePascalToDateTimeSQL , 'function DateTimePascalToDateTimeSQL(const MyDate: double): string;');
  sender.AddFunction(@DateTimeSQLToDateTimePascal , 'function DateTimeSQLToDateTimePascal(const MyDateSQL: string): double;');
  sender.AddFunction(@ConvertirEnNombreReel       , 'function ConvertirEnNombreReel(const S: string; const Default: Extended): Extended');
  sender.AddFunction(@FormatterNombreOOo          , 'function FormatterNombreOOo(const Value: Double; const NbDecs: integer; const IgnoreNulls: boolean): string;');

  sender.AddFunction(@Hypot2D                     , 'function Hypot2D(const DX, DY: Double): Double;');
  sender.AddFunction(@Hypot3D                     , 'function Hypot3D(const DX, DY, DZ: Double): Double;');
  sender.AddFunction(@GetAzimut                   , 'function GetAzimut(const dx, dy: double; const Unite: double): double;');
  sender.AddFunction(@ConvertirEnFiltreSeries     , 'function ConvertirEnFiltreSeries(const S: string): string;');


  sender.AddFunction(@CalcDistanceJaroWinkler     , 'function CalcDistanceJaroWinkler(const S1, S2: string): double;');
  sender.AddFunction(@CalcDistanceDamerauLevenshtein, 'function CalcDistanceDamerauLevenshtein(const S1, S2: string): integer;');
  // infos système (mémoire, proc, HDD, etc ...);
  sender.AddMethod(self, @TCdrPascalScript.DisplaySystemInfos, 'procedure DisplaySystemInfos();');
  // évaluation d'expressions
  sender.AddMethod(self, @TCdrPascalScript.EvalExpr    , 'function Eval(const Expr: string): double;');
  // ces fonctions utilisent des variables privées de TCdrPascalScript;
  sender.AddMethod(self, @TCdrPascalScript.DispPSOutput, 'procedure print(const Msg: string);');
  sender.AddMethod(self, @TCdrPascalScript.printf      , 'procedure printf(const Fmt : String; const Args : Array of const): String;');
  sender.AddMethod(self, @TCdrPascalScript.feof        , 'function  feof(const Descr: integer): boolean;');
  sender.AddMethod(self, @TCdrPascalScript.fopen       , 'function  fopen(const Descr: integer; const QFileName: string; const Mode: Char): boolean;');
  sender.AddMethod(self, @TCdrPascalScript.fprint      , 'procedure fwriteln(const Descr: integer; const QLine: string);');
  sender.AddMethod(self, @TCdrPascalScript.fprintf     , 'procedure fprintf(const Descr: integer; const FMT: string; const Args: array of const);');
  sender.AddMethod(self, @TCdrPascalScript.freadln     , 'function  freadln(const Descr: integer): string;');
  sender.AddMethod(self, @TCdrPascalScript.fclose      , 'procedure fclose(const Descr: integer)');
  sender.AddMethod(self, @TCdrPascalScript.fcloseall   , 'procedure fcloseall()');
  sender.AddMethod(self, @TCdrPascalScript.cls         , 'procedure cls()');
  sender.AddMethod(self, @TCdrPascalScript.cls         , 'procedure ClrScr()');
  sender.AddMethod(self, @TCdrPascalScript.ForceDirectory, 'procedure ForceDirectory(const Dir: string);');
  sender.AddMethod(self, @TCdrPascalScript.GetPathDelimiter, 'function  GetPathDelimiter(): string;');
  sender.AddMethod(self, @TCdrPascalScript.GetPathDelimiter, 'function  PathDelim(): string;');

  {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNomEtude                 , 'function  GetNomEtude(): string;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNbNameSpaces             , 'function  GetNbNameSpaces(): integer;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNbSeries                 , 'function  GetNbSeries(): integer;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNbEntrances              , 'function  GetNbEntrees(): integer;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNbReseaux                , 'function  GetNbReseaux(): integer;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNbExpes                  , 'function  GetNbExpes(): integer;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNbCodes                  , 'function  GetNbCodes(): integer;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNbSecteurs               , 'function  GetNbSecteurs(): integer;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.GetNbAntennes               , 'function  GetNbAntennes(): integer;');
    sender.AddMethod(FDocuTopo, @TToporobotStructure2012.CheckerLesDonneesTopo       , 'procedure CheckerLesDonneesTopo();');



    sender.AddMethod(self     , @TCdrPascalScript.DT_GetNbreStationsOfSerieByIdx     , 'function  GetNbStationsOfSerieByIdx(const Idx: integer): integer;');
    sender.AddMethod(self     , @TCdrPascalScript.DT_GetNbreStationsOfSerieByNoSerie , 'function  GetNbStationsOfSerieByNoSerie(const NumeroSerie: integer): integer;');
    sender.AddMethod(self     , @TCdrPascalScript.DT_GetNumeroEtNomDeSerieByIdx      , 'function DT_GetNumeroEtNomDeSerieByIdx(const Idx: integer; out QNumSerie: integer; out QNomSerie: string; out QNbVisees: integer): boolean;');
    sender.AddMethod(self     , @TCdrPascalScript.DT_BeginNewSerie                   , 'function DT_BeginNewSerie(const QNumeroSerie: integer; ' +
                                                                                       'const QNoEntrance, QNoReseau: integer; ' +
                                                                                       'const QSerDep, QPtDep, QSerArr, QPtArr: integer; ' +
                                                                                       'const QName: string): boolean;');

    sender.AddMethod(self     , @TCdrPascalScript.DT_AddNewVisee                     , 'function DT_AddNewVisee(const QExpe, QCode: integer; ' +
                                                                                       'const QTypeVisee, QSecteur: integer; ' +
                                                                                       'const QLong, QAz, QPente, QLG, QLD, QHZ, QHN: double; ' +
                                                                                       'const QIDTerrain, QObserv: string): boolean;');

    sender.AddMethod(self     , @TCdrPascalScript.DT_EndNewSerie                     , 'procedure DT_EndNewSerie();');
    sender.AddMethod(self     , @TCdrPascalScript.DT_RemoveSerieByNumSerie           , 'procedure DT_RemoveSerieByNumSerie(const QNumeroSerie: integer);');
    sender.AddMethod(self     , @TCdrPascalScript.DT_RemoveSerieByIdx                , 'procedure DT_RemoveSerieByIdx(const Idx: integer);');
    sender.AddMethod(self     , @TCdrPascalScript.DT_AddNewEntrance                  , 'procedure DT_AddNewEntrance(const RefSer, RefSt: integer; const XEntree, YEntree, ZEntree: double; const R, G, B: integer; const NomEntree, IDTerrain, Observ: string);');
    sender.AddMethod(self     , @TCdrPascalScript.DT_ModifyEntrance                  , 'function  DT_ModifyEntrance(const QIdx: integer; const RefSer, RefSt: integer; const XEntree, YEntree, ZEntree: double; const R, G, B: integer; const NomEntree, IDTerrain, Observ: string): boolean;');
    sender.AddMethod(self     , @TCdrPascalScript.DT_AddNewReseau                    , 'procedure DT_AddNewReseau(const QTypeReseau: integer; const R, G, B: integer; const QNomReseau, QObsReseau: string); ');

    // BDD entités
    sender.AddMethod(self     , @TCdrPascalScript.BDE_ExtractCoordsFromSerSt         , 'function  BDE_ExtractCoordsFromSerSt(const QSerie, QStation: integer; out QX, QY, QZ: double): boolean;');


    // fiches PDF de stations
    sender.AddMethod(self     , @TCdrPascalScript.BeginListeFichesStations           , 'function  BeginListeFichesStations(): boolean;');
    sender.AddMethod(self     , @TCdrPascalScript.AddFicheStation                    , 'procedure AddFicheStation(const QSr, QSt: integer);');
    sender.AddMethod(self     , @TCdrPascalScript.GetNbFichesStations                , 'function  GetNbFichesStations(): integer;');
    sender.AddMethod(self     , @TCdrPascalScript.ExportFichesToPDF                  , 'procedure ExportFichesToPDF(const QFilename: string; const QCompressed: boolean);');
    sender.AddMethod(self     , @TCdrPascalScript.EndListeFichesStations             , 'procedure EndListeFichesStations();');
    // maillages
    sender.AddMethod(self     , @TCdrPascalScript.MNT_LoadMaillage                   , 'function  MNT_LoadMaillage(const QFilenameMAI: string): integer;');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_GetNbVertex                    , 'function  MNT_GetNbVertex(): integer;');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_GetNbTriangles                 , 'function  MNT_GetNbTriangles(): integer;');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_Unload                         , 'procedure MNT_Unload();');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_AddProfil                      , 'procedure MNT_AddProfil(const ProfilName: string; const X1, Y1, X2, Y2: double);');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_RemoveProfil                   , 'procedure MNT_RemoveProfil(const Idx: integer);');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_GetNbProfils                   , 'function  MNT_GetNbProfils(): integer;');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_ClearProfils                   , 'procedure MNT_ClearProfils();');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_ExtractBounds                  , 'function  MNT_ExtractBounds(out QX1, QY1, QZ1, QX2, QY2, QZ2: double): boolean;');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_ExtractAltitudeFromXY          , 'function  MNT_ExtractAltitudeFromXY(const QX, QY: double; out QZ: double): boolean');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_GenerateSetOfProfils           , 'function  MNT_GenerateSetOfProfils(const X1, Y1, X2, Y2, X3, Y3: double; const NbX, NbY: integer; const Sens: byte): boolean;');
    sender.AddMethod(self     , @TCdrPascalScript.MNT_CheckerAltimetrieEntrees       , 'procedure MNT_CheckerAltimetrieEntrees(const DoAdjustAtMNT: boolean; const DeltaZMax: double);');
  {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
  // QRCode
  sender.AddMethod(self       , @TCdrPascalScript.QRCode_Generate                    , 'procedure QRCode_Generate(const ImgWidth: integer; const QFilenamePNG: string; const QTextToEncode: string);');
  // display graphisme
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_BeginDrawing                  , 'function  PS2D_BeginDrawing(const w, h: integer): boolean;');
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_EndAndDisplayDrawing          , 'procedure PS2D_EndAndDisplayDrawing();');

  sender.AddMethod(self     , @TCdrPascalScript.PS2D_SetBackgroundColor            , 'procedure PS2D_SetBackgroundColor(const R, G, B, A: byte);');
  EWE := 'function  PS2D_AddStyleSheet(const QStylename: string; ' +
         'const QPenColorR, QPenColorG, QPenColorB, QPenOpacity: byte; const QPenWidtdhInPX: byte;' +
         'const QBshColorR, QBshColorG, QBshColorB, QBshOpacity: byte; const QBshStyle: byte;' +
         'const QFntName: string; const QFntColorR, QFntColorG, QFntColorB, QFntOpacity: byte;' +
         'const QFntSizeInPX: byte; const QFntStyle: byte): integer;';

  sender.AddMethod(self     , @TCdrPascalScript.PS2D_AddStyleSheet                 , EWE);
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_SetStyleSheet                 , 'procedure PS2D_SetStyleSheet(const Idx:integer);');

  sender.AddMethod(self     , @TCdrPascalScript.PS2D_BeginPolyline                 , 'function  PS2D_BeginPolyline(const QName: string; const Closed: boolean): boolean;');
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_AddVertex                     , 'procedure PS2D_AddVertex(const QX, QY: double);');
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_EndPolyline                   , 'procedure PS2D_EndPolyline();');
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_AddTexte                      , 'function  PS2D_AddTexte(const QX, QY, AngleOrientation: double; const QAlignment:byte; const Texte: string): boolean;');
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_AddEllipse                    , 'function  PS2D_AddEllipse(const QX, QY, QR1, QR2: double): boolean;');
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_AddRectangle                  , 'function  PS2D_AddRectangle(const QX1, QY1, QX2, QY2: double): boolean;');
  sender.AddMethod(self     , @TCdrPascalScript.PS2D_AddLine                       , 'function  PS2D_AddLine(const QX1, QY1, QX2, QY2: double): boolean;');
  // transferts FTP
  sender.AddMethod(self     , @TCdrPascalScript.FTP_BeginConnexion                 , 'function  FTP_BeginConnexion(const QHost: string; const QPort: integer; const QUser, QPassword: string;  out QReturnCode: integer; out QReturnMsg: string): boolean;');
  sender.AddMethod(self     , @TCdrPascalScript.FTP_EndConnexion                   , 'procedure FTP_EndConnexion();');
  sender.AddMethod(self     , @TCdrPascalScript.FTP_GetFileName                    , 'function  FTP_GetFileName(const Idx: integer): String;');
  sender.AddMethod(self     , @TCdrPascalScript.FTP_GetNbFilesOfLast               , 'function  FTP_GetNbFilesOfLast(): integer;');
  sender.AddMethod(self     , @TCdrPascalScript.FTP_ListFilenamesOf                , 'function  FTP_ListFilenamesOf(const Path: string): integer;');
  sender.AddMethod(self     , @TCdrPascalScript.FTP_SendFile                       , 'function  FTP_SendFile(const QFilename: string): boolean;');
  sender.AddMethod(self     , @TCdrPascalScript.FTP_SetRemoteDirectory             , 'procedure FTP_SetRemoteDirectory(const D: string);');
  //sender.AddMethod(self     , @TCdrPascalScript.FTP_, '');
  //sender.AddMethod(self     , @TCdrPascalScript.FTP_, '');
  //sender.AddMethod(self     , @TCdrPascalScript.FTP_, '');
end;

//********************************************************************************
procedure TCdrPascalScript.PSScriptDebugger1AfterExecute(Sender: TPSScriptDebugger);
begin
  DispPSOutput('*******************************');
  DispPSOutput('*** Script terminated      ****');
  lbStatusPSScript.Caption := 'DONE';
end;

procedure TCdrPascalScript.PSScriptDebugger1Compile(Sender: TPSScriptDebugger);
begin
  lsbPSMessages.Clear;
  lbStatusPSScript.Caption := 'COMPILING';
  RecenserAdditionalProcs(Sender);
  ListerPSFunctions();
  lbStatusPSScript.Caption := 'COMPILED';
end;



procedure TCdrPascalScript.PSScriptDebugger1Line(Sender: TObject);
begin
  Application.ProcessMessages;
  lbStatusPSScript.Caption := IIF(PSScriptDebugger1.Running, 'RUNNING', 'READY');
end;

procedure TCdrPascalScript.lsbPSFunctionsDblClick(Sender: TObject);
var
  EWE: String;
begin
  EWE := Trim(lsbPSFunctions.Items[lsbPSFunctions.ItemIndex] + '(');
  editPascalScript.InsertTextAtCaret(EWE);
  editPascalScript.CaretX := editPascalScript.CaretX + Length(EWE);
  editPascalScript.SelStart := editPascalScript.CaretX;
  editPascalScript.SelEnd   := editPascalScript.CaretX;
  //editPascalScript.SetFocus;
end;

procedure TCdrPascalScript.lsbPSFunctionsSelectionChange(Sender: TObject; User: boolean);
var
  n: Integer;
  EWE: String;
begin
  n := lsbPSFunctions.ItemIndex;
  if (n = -1) then exit;
  EWE := FOnLineHelpOfFuncs.Strings[n];
  memoHelp.Text := EWE;
end;

procedure TCdrPascalScript.acSavePSScriptExecute(Sender: TObject);
var
  QFileName, EWE: TStringDirectoryFilename;
  TD: TSaveDialog;
begin
  QFileName := '';
  EWE := GetGHTopoDirectory() + DOSSIER_DE_MES_SCRIPTS;
  ForceDirectories(EWE);
  TD := TSaveDialog.create(self);
  try
    TD.InitialDir := EWE;
    TD.Options    := [ofOverwritePrompt];
    TD.Filter     := 'Pascal Script (*.pas)|*.pas';
    TD.DefaultExt := '.pas';
    TD.FileName := 'Script001.pas';
    if (TD.Execute) then editPascalScript.Lines.SaveToFile(TD.FileName);
  finally
    FreeAndNil(TD);
  end;
end;

procedure TCdrPascalScript.acLoadPSScriptExecute(Sender: TObject);
var
  QFileName, EWE: TStringDirectoryFilename;
  TD: TOpenDialog;
begin
  QFileName := '';
  EWE := GetGHTopoDirectory() + DOSSIER_DE_MES_SCRIPTS;
  ForceDirectories(EWE);
  TD := TOpenDialog.create(self);
  try
    TD.InitialDir := EWE;
    //TD.Options    := of;
    TD.Filter     := 'Pascal Script (*.pas)|*.pas';
    TD.DefaultExt := '.pas';
    if (TD.Execute) then editPascalScript.Lines.LoadFromFile(TD.FileName);
  finally
    FreeAndNil(TD);
  end;
end;

procedure TCdrPascalScript.acCopyOutputExecute(Sender: TObject);
begin
  editPSOutPut.CopyToClipboard;
end;

procedure TCdrPascalScript.acNewScriptExecute(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Effacer le script courant')) then NewScript();
end;

procedure TCdrPascalScript.acPSExecuteExecute(Sender: TObject);
var
  n, i: integer;
  CompilationOK, ResExec: Boolean;
  EWE: String;
  function PreprocessLine(const S: string): string;
  begin
    Result := S;
    // opérateurs du C convertis en opérateurs Pascal
    Result := StringReplace(Result, '==', '=', [rfReplaceAll]);
    Result := StringReplace(Result, '!=', '<>', [rfReplaceAll]);
    // commentaires C
    Result := StringReplace(Result, '/*', '(*', [rfReplaceAll]);
    Result := StringReplace(Result, '*/', '*)', [rfReplaceAll]);

  end;
begin
  PageControlMsgsOutputs.ActivePageIndex := 0;
  // récupérer le script
  n := editPascalScript.Lines.Count;
  if (0 = n) then Exit;

  PSScriptDebugger1.ClearBreakPoints;
  PSScriptDebugger1.Script.Clear;
  PSScriptDebugger1.Comp.OnBeforeOutput := psCompilerBeforeOutput;
  for i := 0 to n - 1 do  PSScriptDebugger1.Script.Add(PreprocessLine(editPascalScript.Lines[i]));
  CompilationOK := PSScriptDebugger1.Compile;
  n := PSScriptDebugger1.CompilerMessageCount;
  if (n > 0) then
  begin
    for i:= 0 to  n - 1 do DispPSCompilerMsg(PSScriptDebugger1.CompilerMessages[i].MessageToString);
  end;
  if (CompilationOK) then
  begin
    PageControlMsgsOutputs.ActivePageIndex := 1;
    // sauvegarde intermédiaire seulement si le script compile OK
    EWE := GetGHTopoDirectory() + DOSSIER_DE_MES_SCRIPTS + '/' + MakeFilenameFromDate('QSaveScript_', Now(), 'pas');
    editPascalScript.Lines.SaveToFile(EWE);
    ResExec := (PSScriptDebugger1.Exec.RunScript and (PSScriptDebugger1.Exec.ExceptionCode = erNoError));
    if (not ResExec) then
    begin
      EWE := Format('Run-time error: %d - %s', [PSScriptDebugger1.ExecErrorCode, PSScriptDebugger1.ExecErrorToString]);
      lsbPSMessages.Items.Add(EWE);
    end;
    ShowMessage('Script terminé');
    lbStatusPSScript.Caption := 'READY';
  end
  else
  begin
    PageControlMsgsOutputs.ActivePageIndex := 0;
    ShowMessage('Erreur dans le script');
    if (PSScriptDebugger1.CompilerMessageCount > 0) then
    begin
      for i:= 0 to PSScriptDebugger1.CompilerMessageCount - 1 do
          DispPSCompilerMsg(PSScriptDebugger1.CompilerErrorToStr(i));
    end;
    lsbPSMessages.ItemIndex := lsbPSMessages.Count - 1;
    // se positionner au droit de l'erreur
    editPascalScript.CaretY := PSScriptDebugger1.CompilerMessages[0].Row;
    editPascalScript.CaretX := PSScriptDebugger1.CompilerMessages[0].Col;
    editPascalScript.SetFocus;
  end;
end;

procedure TCdrPascalScript.acPSStopExecute(Sender: TObject);
begin
  //try
  // if (PSScriptDebugger1.Exec.) then PSScriptDebugger1.Stop;
    PSScriptDebugger1.Exec.Pause;
    PSScriptDebugger1.Exec.Stop;
    if (not PSScriptDebugger1.Running) then
    begin
      DispPSOutput('*******************************');
      DispPSOutput('*** Script stopped by user ****');
      lbStatusPSScript.Caption := 'READY';
    end;
  //except
    pass;
  //end;
end;



procedure TCdrPascalScript.ListerPSFunctions();
var
  n, p: LongInt;
  i, j: Integer;
  WU: TPSRegProc;
  EWE: TPSParameterDecl;
  QAT: TbtString;
  QQ: String;
  ResultType: TPSType;
  QReturnType: string;
begin

  FOnLineHelpOfFuncs.Clear;
  n := PSScriptDebugger1.Comp.GetRegProcCount;
  DispPSOutput(inttostr(n));
  if (n = 0) then exit;
  for i := 0 to n - 1 do
  begin
    WU := PSScriptDebugger1.Comp.GetRegProc(i);
    QReturnType:= '';
    (*
    try
      ResultType := WU.Decl.Result;

      case ResultType.BaseType of
        btReturnAddress: QReturnType := 'void';
        btU8           : QReturnType := 'byte';
        btS8           : QReturnType := 'short';
        btU16          : QReturnType := 'unsigned int16';
        btS16          : QReturnType := 'int16';
        btU32          : QReturnType := 'cardinal';
        btS32          : QReturnType := 'integer';
        btSingle       : QReturnType := 'single';
        btDouble       : QReturnType := 'double';
        btExtended     : QReturnType := 'extended';
        btString       : QReturnType := 'string';
        btRecord       : QReturnType := 'record';
        btArray        : QReturnType := 'array';
        btPointer      : QReturnType := 'pointer';
        btPChar        : QReturnType := 'char *';
        btResourcePointer: QReturnType := 'resource *';
        btVariant        : QReturnType := 'unsigned byte';
      else
        QReturnType  := '<object or unknown>';

      end;

    except
      on E: Exception do DispPSOutput('Fuck the Christ ' + WU.Name + ' ' + E.Message);
    end;
    //*)
    QAT := format('%s %s(', [QReturnType, WU.Name]);
    p := WU.Decl.ParamCount;
    if (p > 0) then
    begin
      for j := 0 to p - 1 do
      begin
        EWE := WU.Decl.Params[j];
        QQ := IIF(j < (p-1), '; ', '');
        try
          QAT := QAT + format('%s: %s%s', [LowerCase(EWE.Name), LowerCase(EWE.aType.Name), QQ]);
        except
          QAT := QAT + '<Undocumented>; ';
        end;
      end;
    end
    else
    begin
      pass;
    end;
    QAT += ');';
    FOnLineHelpOfFuncs.Add(QAT);
  end;
  // et on trie
  FOnLineHelpOfFuncs.Sort;
  // avant de peupler la Listbox
  lsbPSFunctions.Sorted := false;
  lsbPSFunctions.Clear;
  for i := 0 to n - 1 do
  begin
    lsbPSFunctions.Items.Add(FOnLineHelpOfFuncs.Strings[i]);
  end;
  lsbPSFunctions.ItemIndex := 0;
end;

procedure TCdrPascalScript.DispPSCompilerMsg(const Msg: string);
begin
  lsbPSMessages.Items.Add(msg);
  lsbPSMessages.ItemIndex := lsbPSMessages.Items.Count - 1;
end;

procedure TCdrPascalScript.NewScript(const ScriptName: string = 'NewScript01');
const
  QNameMainProc = 'Main';
  QName_MAX_SIZE_PARAM_ARRAY = 'MAX_SIZE_PARAM_ARRAY';
  QNameTGHStringArray        = 'TGHStringArray';
  QGlobalVarSR               = 'SR';
begin
  editPascalScript.Lines.Clear;
  editPascalScript.Lines.Add(format('program %s;', [ScriptName]));
  editPascalScript.Lines.Add(format('procedure %s();', [QNameMainProc]));
  editPascalScript.Lines.Add('var');
  editPascalScript.Lines.Add('  i, Nb: integer;');
  editPascalScript.Lines.Add('begin');
  editPascalScript.Lines.Add('  (* Your code here *)');
  editPascalScript.Lines.Add('end;');
  editPascalScript.Lines.Add('(* *************************************** *)');
  editPascalScript.Lines.Add('(* Don''t edit after this                  *)');
  editPascalScript.Lines.Add('begin');
  editPascalScript.Lines.Add(format('  %s();', [QNameMainProc]));
  editPascalScript.Lines.Add('end.');
end;

procedure TCdrPascalScript.InitCaptions();
  procedure SetAcHint(const ACDC: TAction; const QCaption: string);
  begin
    ACDC.Caption := GetResourceString(QCaption);
    ACDC.Hint    := GetResourceString(QCaption);
  end;
begin
  SetAcHint(acCopyOutput      , rsTAB_PS_AC_COPY_OUTPUT);
  SetAcHint(acPSExecute       , rsTAB_PS_AC_RUN);
  SetAcHint(acPSStop          , rsTAB_PS_AC_STOP);
  SetAcHint(acLoadPSScript    , rsTAB_PS_AC_OPEN);
  SetAcHint(acSavePSScript    , rsTAB_PS_AC_SAVE);
  SetAcHint(acNewScript       , rsTAB_PS_AC_NEW);

  tabShtCompiloMessages.Caption := GetResourceString(rsTAB_PS_TabShtMessagesCompilo_CAPTION);
  tabShtConsoleOutput.Caption   := GetResourceString(rsTAB_PS_TabShtConsoleOutput_CAPTION);
end;

procedure TCdrPascalScript.DispPSOutput(const Msg: string);
begin
  self.editPSOutPut.Lines.add(msg);
  self.editPSOutPut.CaretY := self.editPSOutPut.Lines.Count - 1;
  self.editPSOutPut.MoveCaretToVisibleArea;
  Application.ProcessMessages;
end;

function TCdrPascalScript.EvalExpr(const Expr: string): double;
var
  Err: Integer;
  DescErr: string;
begin
  Err := 0;
  DescErr:= '';
  Result := FEvaluateurExpressions.Evaluate(Expr, Err, DescErr);
end;


procedure TCdrPascalScript.DisplayHelpForProc(const MyProc: TPSRegProc);
var
  p: LongInt;
begin
  //ShowMessage('DisplayHelpForProc()');
  memoHelp.Clear;
  memoHelp.Lines.Add(MyProc.Name);
  p := MyProc.Decl.ParamCount;
end;
////////////////////////////////////////////////////////////////////////////////
// wrappers pour fonctions utilitaires
// (doivent figurer dans TCdrCalculette en raison de la liste des descripteurs de fichiers
procedure TCdrPascalScript.fprint(const Descr: integer; const QLine: string);
begin
  WriteLn(FpTextFiles[Descr], QLine);
end;


procedure TCdrPascalScript.fprintf(const Descr: integer; const FMT: string; const Argv: array of const);
begin
  WriteLn(FpTextFiles[Descr], Format(FMT, Argv));
end;


function TCdrPascalScript.feof(const Descr: integer): boolean;
begin
  Result := EOF(FpTextFiles[Descr]);
end;

function TCdrPascalScript.freadln(const Descr: integer): string;
var
  QLine: string;
begin
  ReadLn(FpTextFiles[Descr], QLine);
  result := QLine;
end;
function TCdrPascalScript.fopen(const Descr: integer; const QFileName: string; const Mode: Char): boolean;
begin
  result := false;
  try
    if (not InRange(Descr, Low(FpTextFiles), high(FpTextFiles))) then exit;
    AssignFile(FpTextFiles[Descr], QFileName);
    case Mode of
      'a': Append(FpTextFiles[Descr]);
      'r': Reset(FpTextFiles[Descr]);
      'w': ReWrite(FpTextFiles[Descr]);
    end;
    result := true;
  except

  end;
end;

procedure TCdrPascalScript.fclose(const Descr: integer);
begin
  try
    CloseFile(FpTextFiles[Descr]);
  finally
  end;
end;

procedure TCdrPascalScript.fcloseall();
var
  i: Integer;
begin
  for i := 0 to high(FpTextFiles) do self.fclose(i);
end;

procedure TCdrPascalScript.ForceDirectory(const Dir: string);
begin
  ForceDirectories(Dir);
end;

function TCdrPascalScript.GetPathDelimiter(): string;
begin
  Result := PathDelim;
end;


////////////////////////////////////////////////////////////////////////////////
procedure TCdrPascalScript.cls();
begin
  editPSOutPut.Clear;
end;

procedure TCdrPascalScript.DisplaySystemInfos();
const
  FMT_ECRANS_RESOL = 'Display #%d: %dx%d - %s';
var
  i, NbMonitors, W, H: Integer;
  WU: String;
  DateCompilation: TDateTime;
  M: TMonitor;
  YYYY, MM, DD, HH, MN, SS, MS: Word;
begin
  DispPSOutput('System informations:');
  DispPSOutput('========================================');
  DispPSOutput('Processor:');
  DispPSOutput('-----------');
  DispPSOutput('Target CPU: ' + {$I %FPCTARGETCPU%});
  DispPSOutput(Format('Nb of cores: %d', [GetNbCoresProcessor()]));
  DispPSOutput('');
  NbMonitors := Screen.MonitorCount;
  DispPSOutput('Display:');
  DispPSOutput('----------');
  DispPSOutput(Format('%d monitors', [NbMonitors]));
  for i:= 0 to NbMonitors - 1 do
  begin
    M := Screen.Monitors[i];
    W := M.Width;
    H := M.Height;
    DispPSOutput(Format( FMT_ECRANS_RESOL, [M.MonitorNum,W, H, BoolToStr(M.Primary, 'Primary', 'Auxiliary')]));
  end;
  DispPSOutput('');
  DispPSOutput('Operating system:');
  DispPSOutput('------------------------');
  {$IFDEF MSWINDOWS}
    DispPSOutput(Format('OS: %s', ['Microsoft Windows']));
  {$ENDIF}
  {$IFDEF LINUX}
    DispPSOutput(Format('OS: %s', ['Linux']));      //TODO: Obtenir la version de Linux
  {$ENDIF}
  DispPSOutput('');
  DispPSOutput(ApplicationName);
  DispPSOutput('-------');
  DispPSOutput('Target OS: ' + {$I %FPCTARGETOS%});
  DateCompilation      := FileDateToDateTime(FileAge(ParamStr(0)));
  DecodeDateTime(DateCompilation, YYYY, MM, DD, HH, MN, SS, MS);

  DispPSOutput(Format(GetResourceString(rsGHTOPOVERSION), [DD, MM, YYYY, HH, MN, SS]));
  DispPSOutput(GetResourceString(rsTYPE_INTERFACE_DESKTOP));
  DispPSOutput(GetResourceString(rsCOORDS_CONVERTER_AUTHOR));
  DispPSOutput('FPC Compiler: ' + {$I %FPCVERSION%});
  WU := 'Compilation: ' + {$I %DATE%} + ' ' + {$I %TIME%};
  DispPSOutput(WU);
  DispPSOutput('');
  {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    DispPSOutput('Multithreading: ' + MULTI_OR_MONO_THREADED);
    {$IFDEF USE_MATRIX_DATASTRUCTURE_LIST_OF_LISTS}
    WU := 'Array of TList<float32>';
    {$ELSE}
    WU := 'Array of arrays of float32';
    {$ENDIF}
    DispPSOutput('Data structures for matrix storage: ' + WU);
    DispPSOutput('OpenGL Viewer: ' + {$IFDEF USE_VIEWER_OPENGL} 'En' {$ELSE} 'Dis' {$ENDIF} + 'abled');
  {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
  DispPSOutput('========================================');
end;

procedure TCdrPascalScript.printf(const FMT: string; const Argv: array of const);
begin
  //self.editPSOutPut.Lines.Add(Format(FMT, Argv));
  DispPSOutput(Format(FMT, Argv));
end;
////////////////////////////////////////////////////////////////////////////////
// Spécifique GHTopo
{$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}


//******************************************************************************
// Les fiches topo
function TCdrPascalScript.BeginListeFichesStations(): boolean;
begin
  result := false;
  FLesFichesTopo := TFichesTopo.Create;
  try
    FLesFichesTopo.Initialiser(FBDDEntites, nil);
    Result := true;
  except
  end;
end;

procedure TCdrPascalScript.AddFicheStation(const QSr, QSt: integer);
begin
  FLesFichesTopo.AddStationBySerieStation(QSr, QSt);
end;

procedure TCdrPascalScript.ExportFichesToPDF(const QFilename: string; const QCompressed: boolean);
begin
  FLesFichesTopo.ImprimerLesFiches(QFilename, QCompressed);
end;

function TCdrPascalScript.GetNbFichesStations(): integer;
begin
  Result := FLesFichesTopo.GetNbStations();
end;

procedure TCdrPascalScript.EndListeFichesStations();
begin
  try
    FLesFichesTopo.Finaliser();
  finally
    FreeAndNil(FLesFichesTopo);
  end;
end;
//******************************************************************************
// la BDD topo
function TCdrPascalScript.DT_GetNbreStationsOfSerieByIdx(const Idx: integer): integer;
var
  SR: TObjSerie;
begin
  SR := FDocuTopo.GetSerie(Idx);
  Result := SR.GetNbVisees();
end;

function TCdrPascalScript.DT_GetNbreStationsOfSerieByNoSerie(const NoSerie: integer): integer;
var
  SR: TObjSerie;
  QIdx: integer;
begin
  if (not FDocuTopo.GetSerieByNumeroSerie(NoSerie, SR, QIdx)) then exit(-1);
  Result := SR.GetNbVisees();
end;

function TCdrPascalScript.DT_GetNumeroEtNomDeSerieByIdx(const Idx: integer; out QNumSerie: TNumeroSerie; out QNomSerie: string; out QNbVisees: integer): boolean;
var
  SR: TObjSerie;
  QIdx: integer;
begin
  Result := false;
  SR := FDocuTopo.GetSerie(Idx);
  try
    QNumSerie := SR.GetNumeroDeSerie();
    QNomSerie := SR.GetNomSerie();
    QNbVisees := SR.GetNbVisees();
    result := true;
  except
  end;
end;
{
function TCdrPascalScript.DT_ExtractDataFromVisee(const QNumeroSerie: TNumeroSerie; const QNumeroStation: TNumeroStation; out V: array of string): boolean;
var
  SR: TObjSerie;
  MyVisee: TUneVisee;
  QIdx, i: integer;
begin
  Result := false;
  if (not FDocuTopo.GetSerieByNumeroSerie(QNumeroSerie, SR, QIdx)) then exit;
  //for i := 0 to high(V) do V[i] := '';
  MyVisee:= SR.GetVisee(QNumeroStation);
  (*
  V.Add(Format('%d', [QNumeroSerie]));
  V.Add(Format('%d', [QNumeroStation]));
  V.Add(Format('%d', [MyVisee.Code]));
  V.Add(Format('%d', [MyVisee.Expe]));
  V.Add(FormatterNombreOOo(MyVisee.Longueur, 3));
  V.Add(FormatterNombreOOo(MyVisee.Azimut  , 3));
  V.Add(FormatterNombreOOo(MyVisee.Pente   , 3));
  V.Add(MyVisee.Commentaires);
  //*)
  //SetLength(V, 8);

  V[0] := Format('%d', [QNumeroSerie]);
  V[1] := Format('%d', [QNumeroStation]);
  V[2] := Format('%d', [MyVisee.Code]);
  V[3] := Format('%d', [MyVisee.Expe]);
  V[4] := FormatterNombreOOo(MyVisee.Longueur, 3);
  V[5] := FormatterNombreOOo(MyVisee.Azimut  , 3);
  V[6] := FormatterNombreOOo(MyVisee.Pente, 3);
  V[7] := MyVisee.Commentaires;
  //*)
end;
///}

procedure TCdrPascalScript.DT_RemoveSerieByNumSerie(const QNumeroSerie: integer);
var
  SR: TObjSerie;
  QIdx: integer;
begin
  if (FDocuTopo.GetSerieByNumeroSerie(QNumeroSerie, SR, QIdx)) then FDocuTopo.RemoveSerie(QIdx);
  DispPSOutput(Format('Série numéro %d supprimée', [QNumeroSerie]));
end;

procedure TCdrPascalScript.DT_RemoveSerieByIdx(const Idx: integer);
begin
  FDocuTopo.RemoveSerie(Idx);
  DispPSOutput(Format('Série d''index %d supprimée', [Idx]));
end;

function TCdrPascalScript.DT_BeginNewSerie(const QNumeroSerie: integer;
                                           const QNoEntrance, QNoReseau: integer;
                                           const QSerDep, QPtDep, QSerArr, QPtArr: integer;
                                           const QName: string): boolean;
var
  EWE: TNumeroSerie;
  QInternaIdxSerie: integer;
begin
  result := false;
  FCurrentSerieAssigned := false;
  if (FDocuTopo.GetSerieByNumeroSerie(QNumeroSerie, FCurrentSerie, QInternaIdxSerie)) then
  begin
    DispPSOutput(format('*** Série existante: %d - "%s" - Les visées seront ajoutées', [FCurrentSerie.GetNumeroDeSerie(), FCurrentSerie.GetNomSerie()]));
    FCurrentSerieAssigned := True;
    result := True;
  end
  else
  begin
    EWE := IIF(QNumeroSerie < 1, FDocuTopo.getMaxIdxSerie() + 1, QNumeroSerie);
    DispPSOutput(format('*** Série créée: %d - "%s"', [EWE, QName]));
    FCurrentSerie := TObjSerie.Create;
    try
      FCurrentSerie.ClearStations();
      FCurrentSerie.AddVisee(EmptyVisee(''));
      FCurrentSerie.SetNumeroSerie(EWE);
      FCurrentSerie.SetNomObsSerie(QName, '');
      FCurrentSerie.SetChanceObstacle(0, 0);
      FCurrentSerie.SetNumeroEntrance(QNoEntrance);
      FCurrentSerie.SetNumeroReseau(QNoReseau);
      FCurrentSerie.SetSeriePtDep(QSerDep, QPtDep);
      FCurrentSerie.SetSeriePtArr(QSerArr, QPtArr);
      FCurrentSerieAssigned := True;
      result := True;
    except
      FreeAndNil(FCurrentSerie);
    end;
  end;
end;
function TCdrPascalScript.DT_AddNewVisee(const QExpe, QCode: integer;
                                         const QTypeVisee, QSecteur: integer;
                                         const QLong, QAz, QPente, QLG, QLD, QHZ, QHN: double;
                                         const QIDTerrain, QObserv: string): boolean;
begin
  result := false;
  if (not FCurrentSerieAssigned) then
  begin
    DispPSOutput('*** DT_BeginNewSerie() non appelé ***');
    exit;
  end;
  FCurrentSerie.AddVisee(QSecteur, QCode, QExpe, TTypeDeVisee(QTypeVisee), QLong, QAz, QPente, QLG, QLD, QHZ, QHN, QIDTerrain, QObserv);
end;

procedure TCdrPascalScript.DT_EndNewSerie();
var
  QSrArr: TNumeroSerie;
  QPtArr: TNumeroStation;

begin
  if (not FCurrentSerieAssigned) then
  begin
    DispPSOutput('*** DT_BeginNewSerie() non appelé ***');
    exit;
  end;
  QSrArr := FCurrentSerie.GetNoSerieArr();
  QPtArr := FCurrentSerie.GetNoPointArr();
  if (QSrArr = -1) then FCurrentSerie.SetNoSerieArr(FCurrentSerie.GetNumeroDeSerie());
  if (QPtArr = -1) then FCurrentSerie.SetNoPointArr(FCurrentSerie.GetNbVisees() - 1);
  FDocuTopo.AddSerie(FCurrentSerie);
end;

procedure TCdrPascalScript.DT_AddNewEntrance(const RefSer, RefSt: integer; const XEntree, YEntree, ZEntree: double; const R, G, B: integer; const NomEntree, IDTerrain, Observ: string);
var
  EE: TEntrance;
begin
  EE.eRefSer  := RefSer;
  EE.eRefSt   := RefSt;
  EE.eXEntree := XEntree;
  EE.eYEntree := YEntree;
  EE.eZEntree := ZEntree;
  EE.eCouleur := RGBToColor(R and 255, G and 255, B and 255);
  EE.eNomEntree  := NomEntree;
  EE.eIDTerrain  := IDTerrain;
  EE.eObserv     := Observ;
  FDocuTopo.AddEntrance(EE);
end;
function TCdrPascalScript.DT_ModifyEntrance(const QIdx: integer; const RefSer, RefSt: integer; const XEntree, YEntree, ZEntree: double; const R, G, B: integer; const NomEntree, IDTerrain, Observ: string): boolean;
var
  EE: TEntrance;
  Nb: Integer;
begin
  result := false;
  Nb := FDocuTopo.GetNbEntrances();
  if (Nb = 0) then Exit;
  if (QIdx >= Nb) then exit;
  try
    EE := FDocuTopo.GetEntrance(QIdx);
    EE.eRefSer  := RefSer;
    EE.eRefSt   := RefSt;
    EE.eXEntree := XEntree;
    EE.eYEntree := YEntree;
    EE.eZEntree := ZEntree;
    EE.eCouleur := RGBToColor(R and 255, G and 255, B and 255);
    EE.eNomEntree  := NomEntree;
    EE.eIDTerrain  := IDTerrain;
    EE.eObserv     := Observ;
    FDocuTopo.PutEntrance(QIdx, EE);
    result := True;
  except
  end;
end;

procedure TCdrPascalScript.DT_AddNewReseau(const QTypeReseau: integer; const R, G, B: integer; const QNomReseau, QObsReseau: string);
var
  RR: TReseau;
begin
  RR.TypeReseau   := QTypeReseau;
  RR.ColorReseau  := RGBToColor(R and 255, G and 255, B and 255);
  RR.NomReseau    := QNomReseau;
  RR.ObsReseau    := QObsReseau;
  FDocuTopo.AddReseau(RR);
end;



//******************************************************************************
// La BDD Entités
function TCdrPascalScript.BDE_ExtractCoordsFromSerSt(const QSerie: TNumeroSerie; const QStation: TNumeroStation; out QX, QY, QZ: double): boolean;
var
  EE: TBaseStation;
begin
  Result := FBDDEntites.GetEntiteViseeFromSerSt(QSerie, QStation, EE);
  if (not Result) then exit;
  QX := EE.PosStation.X;
  QY := EE.PosStation.Y;
  QZ := EE.PosStation.Z;
end;
//******************************************************************************
// Le maillage
function TCdrPascalScript.MNT_LoadMaillage(const QFilenameMAI: string): integer;
begin
  result := -1;
  if (Not Assigned(FMyMaillage))         then exit(-64);
  if (not FileExistsUTF8(QFilenameMAI))  then exit(-2);
  if (FMyMaillage.IsValidMaillage())     then exit(1); // LoadMaillage(GetGHTopoDirectory() + 'castet_miu.mai');
  Result := IIF(FMyMaillage.LoadMaillage(QFilenameMAI), 0, -1);
end;


function TCdrPascalScript.MNT_GetNbVertex(): integer;
begin
  result := FMyMaillage.GetNbVertex();
end;

function TCdrPascalScript.MNT_GetNbTriangles(): integer;
begin
  result := FMyMaillage.GetNbTriangles();
end;

procedure TCdrPascalScript.MNT_Unload();
begin
  FMyMaillage.ResetMaillageTIN();
end;

procedure TCdrPascalScript.MNT_AddProfil(const ProfilName: string; const X1, Y1, X2, Y2: double);
var
  WU: TLineAttributes;
begin
  if (FMyMaillage.IsValidMaillage()) then
  begin
    WU.SetAttributes(clRed, 255, 1, 0.015);
    FMyMaillage.ExtractAndAddProfilTopo(MakeTPoint2Df(X1, Y1), MakeTPoint2Df(X2, Y2), WU, ProfilName);
  end;
end;

procedure TCdrPascalScript.MNT_RemoveProfil(const Idx: integer);
begin
  if (FMyMaillage.IsValidMaillage()) then FMyMaillage.RemoveProfilTopo(Idx);
end;
function  TCdrPascalScript.MNT_GetNbProfils(): integer;
begin
  Result := -1;
  if (FMyMaillage.IsValidMaillage()) then result := FMyMaillage.GetNbProfilsTopo();
end;
procedure TCdrPascalScript.MNT_ClearProfils();
begin
  if (FMyMaillage.IsValidMaillage()) then FMyMaillage.ClearProfilsTopo();
end;

function TCdrPascalScript.MNT_ExtractBounds(out QX1, QY1, QZ1, QX2, QY2, QZ2: double): boolean;
var
  C2, C1: TPoint3Df;
begin
  result := false;
  if (not FMyMaillage.IsValidMaillage()) then exit;
  C1  := FMyMaillage.GetCoordsMini();
  C2  := FMyMaillage.GetCoordsMaxi();
  QX1 := C1.X;
  QY1 := C1.Y;
  QZ1 := C1.Z;
  QX2 := C2.X;
  QY2 := C2.Y;
  QZ2 := C2.Z;
  result := true;
end;
function  TCdrPascalScript.MNT_ExtractAltitudeFromXY(const QX, QY: double; out QZ: double): boolean;
begin
  result := false;
  if (not FMyMaillage.IsValidMaillage()) then exit;
  Result := FMyMaillage.CalcAltitudeMaillageAtPoint(QX, QY, QZ);
end;

function TCdrPascalScript.MNT_GenerateSetOfProfils(const X1, Y1, X2, Y2, X3, Y3: double;
                                                   const NbX, NbY: integer;
                                                   const Sens: byte): boolean;
var
  Nx, Ny, qdxH, qdyH, qdxL, qdyL: double;
  RectZone: array[0..3] of TPoint2Df;
  AlphaH, AlphaL: double;
  caH, saH, caL, saL: ValReal;
  QX1, QY1, QX2, QY2, QH, QL, QPasX, QPasY: double;
  procedure MiouXX();
  var
    i: Integer;
  begin
    //----------------------------------------
    //----------------------------------------
    //----------------------------------------
    //----------------------------------------
    for i := 0 to NbY - 1 do
    begin
      QX1 := RectZone[0].X + i * QPasY * caH;
      QY1 := RectZone[0].Y + i * QPasY * saH;
      QX2 := RectZone[1].X + i * QPasY * caH;
      QY2 := RectZone[1].Y + i * QPasY * saH;
      MNT_AddProfil(format('Profil Y%d',[i]), QX1, QY1, QX2, QY2);
    end;
    result := True; // Result du MNT_GenerateSetOfProfils() et non celui de MiouXX()
  end;
  procedure MiouYY();
  var
    i: Integer;
  begin
    //  |  |  |  |  |  |  |  |  |  |  |  |  |  |
    //  |  |  |  |  |  |  |  |  |  |  |  |  |  |
    //  |  |  |  |  |  |  |  |  |  |  |  |  |  |
    //  |  |  |  |  |  |  |  |  |  |  |  |  |  |
    for i := 0 to NbX - 1 do
    begin
      QX1 := RectZone[0].X + i * QPasX * caL;
      QY1 := RectZone[0].Y + i * QPasX * saL;
      QX2 := RectZone[3].X + i * QPasX * caL;
      QY2 := RectZone[3].Y + i * QPasX * saL;
      MNT_AddProfil(format('Profil X%d',[i]), QX1, QY1, QX2, QY2);
    end;
    result := True; // Result du MNT_GenerateSetOfProfils() et non celui de MiouYY()
  end;
begin
  result := false;
  //Villars: 526832, 6484928 , 526920, 6484893, 526916, 6484974
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,X3,Y3, Nx, Ny);
  qdxL := X2 - X1;
  qdyL := Y2 - Y1;
  AlphaL := arctan2(qdyL, qdxL);
  qdxH := X3 - Nx;
  qdyH := Y3 - Ny;
  AlphaH := arctan2(qdyH, qdxH);
  caH := cos(AlphaH);
  saH := sin(AlphaH);
  caL := cos(AlphaL);
  saL := sin(AlphaL);
  // Transport du vecteur normal vers l'origine
  RectZone[0] := MakeTPoint2Df(X1, Y1);
  RectZone[1] := MakeTPoint2Df(X2, Y2);
  RectZone[2] := MakeTPoint2Df(X2 + qdxH, Y2 + qdyH);
  RectZone[3] := MakeTPoint2Df(X1 + qdxH, Y1 + qdyH);
  QH := Hypot2D(qdxH, qdyH);
  QL := Hypot2D(qdxL, qdyL);
  QPasX := QL / NbX;
  QPasY := QH / NbY;
  case Sens of
    1: MiouXX(); // sens -----
    2: MiouYY(); // sens | | |
    3: begin     // X et Y +-+-+-
         MiouXX();
         MiouYY();
       end;
  end;
end;

procedure TCdrPascalScript.MNT_CheckerAltimetrieEntrees(const DoAdjustAtMNT: boolean; const DeltaZMax: double);
begin
  if (FMyMaillage.IsValidMaillage()) then
  begin
    FDocuTopo.CheckerAltimetrieEntrancesByMNT(FMyMaillage, DoAdjustAtMNT, DeltaZMax);
    DispPSOutput(Format('MNT_CheckerAltimetrieEntrees(): %.2f', [DeltaZMax]));
  end
  else
    DispPSOutput('*** Maillage invalide ou non chargé ***');
end;

{$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
// Graphisme 2D
function TCdrPascalScript.PS2D_BeginDrawing(const w, h: integer): boolean;
begin
  result := false;
  FPSDrawing2D  := TdlgDispGraphisme2D.Create(self);
  try
    if (FPSDrawing2D.Initialiser(w, h)) then
    begin
      Result := true;
    end;
  except
    FreeAndNil(FPSDrawing2D);
  end;
end;
procedure TCdrPascalScript.PS2D_EndAndDisplayDrawing();
begin
  if (Assigned(FPSDrawing2D)) then
  begin
    FPSDrawing2D.ShowModal;
    FreeAndNil(FPSDrawing2D);
  end;
end;

procedure TCdrPascalScript.PS2D_SetBackgroundColor(const R, G, B, A: byte);
begin
  FPSDrawing2D.SetBackgroundColor(R, G, B, A);
end;

function TCdrPascalScript.PS2D_AddStyleSheet(const QStylename: string;
                                            const QPenColorR, QPenColorG, QPenColorB, QPenOpacity: byte; const QPenWidtdhInPX: byte;
                                            const QBshColorR, QBshColorG, QBshColorB, QBshOpacity: byte; const QBshStyle: byte;
                                            const QFntName: string; const QFntColorR, QFntColorG, QFntColorB, QFntOpacity: byte;
                                            const QFntSizeInPX: byte; const QFntStyle: byte): integer;
begin
  Result := FPSDrawing2D.AddStyleSheet(QStylename,
                                       QPenColorR, QPenColorG, QPenColorB, QPenOpacity, QPenWidtdhInPX,
                                       QBshColorR, QBshColorG, QBshColorB, QBshOpacity, QBshStyle,
                                       QFntName, QFntColorR, QFntColorG, QFntColorB, QFntOpacity, QFntSizeInPX, QFntStyle);
end;
procedure TCdrPascalScript.PS2D_SetStyleSheet(const Idx: integer);
begin
  FPSDrawing2D.SetStyleSheet(Idx);
end;
function TCdrPascalScript.PS2D_BeginPolyline(const QName: string; const Closed: boolean): boolean;
begin
  Result := FPSDrawing2D.BeginPolyline(QName, Closed);
end;

procedure TCdrPascalScript.PS2D_AddVertex(const QX, QY: double);
begin
  FPSDrawing2D.AddVertex(QX, QY);
end;

procedure TCdrPascalScript.PS2D_EndPolyline();
begin
  FPSDrawing2D.EndPolyline();
end;
function TCdrPascalScript.PS2D_AddTexte(const QX, QY, AngleOrientation: double; const QAlignment:byte; const Texte: string): boolean;
begin
  Result := FPSDrawing2D.AddTexte(QX, QY, AngleOrientation, QAlignment, Texte);
end;

function TCdrPascalScript.PS2D_AddEllipse(const QX, QY, QR1, QR2: double): boolean;
begin
  result := FPSDrawing2D.AddEllipse(QX, QY, QR1, QR2);
end;

function TCdrPascalScript.PS2D_AddRectangle(const QX1, QY1, QX2, QY2: double): boolean;
begin
  result := FPSDrawing2D.AddRectangle(QX1, QY1, QX2, QY2);
end;

function TCdrPascalScript.PS2D_AddLine(const QX1, QY1, QX2, QY2: double): boolean;
begin
  result := FPSDrawing2D.AddLigne(QX1, QY1, QX2, QY2);
end;
//fonctionnalités de téléversement / téléchargement
(*
Host: ftp4.phpnet.org
Port: 21
User: synclinal65
Pwd :  *******
LogonType: 1
TimeZone: 0
EncodingType: Auto
BypassProxy: 0
//*)
function TCdrPascalScript.FTP_BeginConnexion(const QHost: string; const QPort: integer; const QUser, QPassword: string;  out QReturnCode: integer; out QReturnMsg: string): boolean;
var
  EWE: String;
begin
  Result := false;
  EWE := Format('%s.FTP_BeginConnexion()', [ClassName]);
  AfficherMessage(EWE);
  DispPSOutput(EWE);
  FTransfertFTP := TRemoteServicesFTP.Create();
  try
    Result := FTransfertFTP.SetParamsAndConnect(QHost, QPort, QUser, QPassword, QReturnCode, QReturnMsg);
  except
  end;
end;

function  TCdrPascalScript.FTP_CreateDirectory(const PathFromRoot, QDirectory: TStringDirectoryFilename): boolean;
begin
  Result := FTransfertFTP.CreateDirectory(PathFromRoot, QDirectory);
end;

procedure TCdrPascalScript.FTP_SetRemoteDirectory(const PathFromRoot: TStringDirectoryFilename);
begin
  FTransfertFTP.SetCurrentRemoteDirectory(PathFromRoot);
end;

function  TCdrPascalScript.FTP_ListFilenamesOf(const Path: string): integer;
begin
  Result := FTransfertFTP.ListFilenamesOf(Path);
end;

function TCdrPascalScript.FTP_SendFile(const QFilename: string): boolean;
var
  EWE: String;
begin
  EWE := Format('%s.FTP_SendFile(): %s to %s', [ClassName, QFilename, FTransfertFTP.GetCurrentRemoteDirectory()]);
  AfficherMessage(EWE);
  DispPSOutput(EWE);
  if (not FileExistsUTF8(QFilename)) then
  begin
    DispPSOutput('-- Fichier inexistant: ' + QFilename);
    exit;
  end;
  result := FTransfertFTP.SendFile(QFilename);
  DispPSOutput('-- Transfert: ' + BoolToStr(result, 'OK', 'KO'));


end;

function  TCdrPascalScript.FTP_GetNbFilesOfLast(): integer;
begin
  Result := FTransfertFTP.GetNbFilesOfLast();
end;

function  TCdrPascalScript.FTP_GetFileName(const Idx: integer): String;
begin
  Result := FTransfertFTP.GetFileName(Idx);
end;
procedure TCdrPascalScript.FTP_EndConnexion();
var
  EWE: string;
begin
  EWE := Format('%s.FTP_EndConnexion()', [ClassName]);
  AfficherMessage(EWE);
  DispPSOutput(EWE);
  try
    FTransfertFTP.Finaliser();
  finally
    FreeAndNil(FTransfertFTP);
  end;
end;
end.




