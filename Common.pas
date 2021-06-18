unit Common;
{.$ERROR Finaliser et valider le support des boules-cibles}
{$INCLUDE CompilationParameters.inc}
// Fonctions communes
// Date: 19/04/2013
// Statut: Fonctionnel
// Des détails à revoir
// 04/10/2013: Centralisation de la conversion de chaines ANSI <> UTF8

// 18/07/2014: Ajout de ConvertirEnNombreReel(), qui supporte infifféremment le point ou la virgule
// 15/12/2014: Ajout de fonctions pour le DistoX
// 28/01/2015: Ajout de qques fonctions dont GetDescTypeVisee()
// 11/04/2015: Adaptation pour Linux : OK
// 21/05/2015: Ajout de la fonction ExtractIDTerrainFromComment()
// 06/06/2016: Corrections. Fonction DegMinSec -> DegDec
// 08/08/2016: Nettoyage de code dont les fonctions de couleurs
// 23/05/2017: TStringArray est un type de données de la FCL 3.0.2. Remplacé par TGHStringArray
// 20/06/2017: Correction dans function CalculerVisee()
// 29/11/2018: Refactoration complète de l'unité. Plus de 20 fonctions et 600 lignes de code supprimées
// 26/03/2021: Point de contrôle temporel (contrôle de version)
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees
  , CalculMatriciel3x3
  , SysUtils
  , Classes
  , Grids, Clipbrd, LCLType, types // pour les grilles et le presse-papiers
  , Process
  , math
  , LazFileUtils
  , Graphics
  , Forms
  //, unitUtilsCPU
  , LConvEncoding
  , sha1
  , FastGEO
  {$IFDEF MSWINDOWS}
  , windows
  {$ENDIF}
  ;


//type TTabStopArray  = array of const; // pour lignes à colonnage fixe
const ARRAY_OF_FILTERS: array[0..22] of String = (
      rsMETAFILTRE_NIL,         // 0
      rsMETAFILTRE_ALL,         // 1
      rsMETAFILTRE_ID,          // 2
      rsMETAFILTRE_LENGTH,      // 3
      rsMETAFILTRE_AZIMUTH,     // 4
      rsMETAFILTRE_PENTE,       // 5
      rsMETAFILTRE_DATE,        // 6
      rsMETAFILTRE_COLOR,       // 7
      rsMETAFILTRE_X,           // 8
      rsMETAFILTRE_Y,           // 9
      rsMETAFILTRE_Z,           // 10
      rsMETAFILTRE_LARGEUR,     // 11
      rsMETAFILTRE_HAUTEUR,     // 12
      rsMETAFILTRE_DATES,       // 13
      rsMETAFILTRE_COLORS,      // 14
      rsMETAFILTRE_SERIE,       // 15
      rsMETAFILTRE_RESEAU,      // 16
      rsMETAFILTRE_CODE,        // 17
      rsMETAFILTRE_EXPE,        // 18
      rsMETAFILTRE_TYPEVISEE,   // 19
      rsMETAFILTRE_SECTEUR,     // 20
      rsMETAFILTRE_ENTREE_RATT, // 21
      rsMETAFILTRE_NAMESPACE    // 22
      );

// pseudo-mot clé ne faisant rien, équivalent de l'instruction pass du Python
// utilisé dans les embranchements sans action à faire de structures de contrôle
procedure pass; inline;

// fonctions utilitaires (console, mémoire)
procedure AfficherMessage(const Msg: string; const DoCRLF: boolean = true);
procedure AfficherMessageErreur(const Msg: string);
procedure ClearConsole();
procedure ClearConsoleErreur();

function  GetNbCoresProcessor(): Cardinal;
// Uniquement pour les Raspberry et consorts
procedure DimensionnerEtCentrerFenetre(const FRM: TForm);
procedure AfficherMemoryUsage();

procedure ExecuteExternalProgram(const MyCommandLine: string);
function  MakeTitleMainWindowGHTopo(const S: string): string;
function  DescribeCriticiteErreur(const c: TCriticiteMessaqeErreurGHTopo): string;
function  DescribeTableExamineeByCode(const c: TTableExaminee): string;
function  ConvertTextFile(const InputFileName, OutputFilename: TStringDirectoryFilename;  const InputFormat, OutputFormat: TTextFileFormat): boolean; // Convertir un fichier texte vers le format désiré


// gestion du dossier de GHTopo
function GetGHTopoVersion(): string;
// Dossier de GHTopo - Inclut aussi le séparateur (eg: C:\GHTopo\)
function GetGHTopoDirectory(): TStringDirectoryFilename;   // attraper le dossier de GHTopo (remplace ExtractFilePath)
function GetLastDirectoryUsed(): TStringDirectoryFilename; // attraper le dossier du dernier doc utilisé
function ExtractDirectoryOfGHTopoDocument(const FN: TStringDirectoryFilename): TStringDirectoryFilename; inline;   // extraction du dossier contenant le doc courant
function EnsureMakeFilename(const S: TStringDirectoryFilename): TStringDirectoryFilename;     // construire un nom de fichier valide en virant les accents et caractères spéciaux

// makers de structures
function MakeTPoint(const QX, QY: integer): TPoint;
function MakeTPointCoupeDeveloppee(const QP, QZ: double): TPointCoupeDeveloppee;
function MakeTMarker(const D: boolean; const QX, QY: double; const C: TColor; const S: string): TMarker;
function MakeTIDBaseStation(const S: TNumeroSerie; const P: integer; const IsAntenne: boolean): TIDBaseStation;

function MakeTStationMatchFound(const S, P: integer; const M: string): TStationMatchFound;


// fonctions communes:
//--------------------
procedure Swap(var V1, V2: integer); overload;
procedure Swap(var V1, V2: Double); overload;
procedure Swap(var V1, V2: string); overload;
procedure Swap(var V1, V2: TMNTNumeroTriangle); overload;
procedure Swap(var V1, V2: TMNTNumeroVertex); overload;

// If immédiats
function IIF(const Condition: boolean; const V1, V2: boolean): boolean; inline; overload;
function IIF(const Condition: boolean; const V1, V2: integer): integer; inline; overload;
function IIF(const Condition: boolean; const V1, V2: double): double; inline; overload;
function IIF(const Condition: boolean; const V1, V2: TColor): TColor; inline; overload;
function IIF(const Condition: boolean; const V1, V2: String): String; inline; overload;
function IIF(const Condition: boolean; const V1, V2: char): char; inline; overload;

// check de valeurs; les bornes sont comprises
function IsInRange(const Value, MinValue, MaxValue: double): Boolean; inline; overload;
function IsInRange(const Value, MinValue, MaxValue: integer): Boolean; inline; overload;
// fonctions graphiques
function IntersectRectangles(const R1, R2: TRect2Df): boolean;
// Fonctions de split et de choix dans des arrays
//**********************
function EmptyTGHStringArray(): TGHStringArray; inline;
// extraction de paramètres d'une chaine
function Split(const MyStr: string; const Sep: char):TGHStringArray;
// Version qui envoie le résultat dans un TStringList ouvert
function SplitToTStringList(const MyStr: string; const Sep: char; var LSR: TStringList; const QSorted: boolean; const QDuplicates: TDuplicates): boolean;
function ExtractStringsToTStringArray(Separators, WhiteSpace: TSysCharSet;   const MyStr: string): TGHStringArray;
function DecoupeChaineEspacesGuillemets(const S: string; const DoRemoveQuotes: boolean): TGHStringArray;
function DecoupeChaine(const S: string; const Separators, WhiteSpace: TSysCharSet; const DoRemoveQuotes: boolean): TGHStringArray;
// extraire les champs d'une chaîne formattée en colonnes fixes (like Toporobot Text)
function SplitFixedColLine(const MyLine: string; const Tabs: array of integer): TGHStringArray;        // splitter une ligne à positions fixes contenues dans Tabs
// extraire les champs d'une chaîne de la forme abcd def   "daech must die" "miaou" toto
function SplitWithQuotedFields(const S: string; const Separator: char; const AQuoteChar: char = '"'): TGHStringArray;

function MakeTStringArray2DFromStr(const S: string; const SepCol: char): TGHStringArray2D;             // obtenir un tableau 2D à partir d'une string

function ExtractFirstElementOfStr(var Str: string; const Sep: string): string;                         // extraction et dépilage du premier élément d'une string
function IndexOfString(const S: string; const IsEqual: boolean; const Strs: array of string): integer;
function ChooseString(const Index: integer; const Strs: array of string; const Default: string = '---'): string;
function ChooseChar(const Index: integer; const Strs: array of char; const Default: char = #32): char;
// fonctions de chaines
//**********************
function _AnsiToLCLStr(const S: string): string; inline;
function _LCLStrToAnsi(const S: string): string; inline;
function GetResourceString(const RS: string): string; inline;
function IncrementString(const S0: string): string;                                           // fonction d'incrémentation lexicographique
function EnlevePerluete(const s: string): string; inline;
function SafeTruncateString(const Str: string; const L: integer): String;
function RemoveDiacritiques(const S: string): string;      // remplace les diacritiques
function FormatterFixedColumn(const S: string; const W: integer; const Aligment: TAlignment): string;    // sortie formatée pour colonnages fixes
function SupprimeEspacesMultiples(const S: string): string;

// fonctions XML
//**********************
// version du XML et encodage
function FormatterVersionEncodageXML(const version: string; const encodage: string): string; inline;
//function ensureConvertLongOrLatToXML(const Value: double): string;       // formatage des nombres pour KML et OSM
//function ensureConvertAltitudeToXML(const Value: double): string;
function PasStrToXMLStr(const S: string): string; // neutralise les entités HTML (pour le support du XML)
function XMLStrToPasStr(const S: string): string;

// fonction de formattage de nombres
//**********************
function FormatterNombreAvecSepMilliers(const X: double; const NbDecs: integer = 3): string; //formatter nombre avec séparateurs de milliers
function PasNumberToOOoCalcFormula(const V: double): string;                  // convertir un  nombre Pascal en nombre OOo Calc
// formatage OOo des nombres
function FormatterNombreOOo(const Value: double; const NbDecs: integer = 3; const IgnoreNulls: boolean = false): string;
// formatage des nombres pour les formats d'échanges imposant le point décimal
function FormatterNombreWithDotDecimal(const Value: double; const NbDecs: integer = 3; const IgnoreNulls: boolean = false): string;

// convertir en nombre des valeurs de type 123 456 789.10 ou 1245-45785
function ConvertirEnNombreReel(const S: string; const Default: double): double;



// fonctions de dates
//**********************
function GetAnneeISO(const QAnnee: word): word;                         // obtenir l'année std depuis année abrégées
function GetSecuredDate(Year, Month, Day: Word): TDateTime;            // encodage/Décodage sécurisé des dates
function DateYYYYMMDDToDateSQL(const YYYY, MM, DD: word): string; inline;
function DatePascalToDateSQL(const MyDate: TDateTime): string;           // dates SQL et vice-versa
function DateSQLToDatePascal(const MyDateSQL: string): TDateTime;
function DateTimeSQLToDateTimePascal(const MyDateSQL: string): TDateTime;
function DateTimePascalToDateTimeSQL(const MyDate: TDateTime; const WithMilliseconds: boolean = true): string;
function DatePascalToDateHeureCondensee(const QDate: TDateTime): string;        // Dates condensées AAAAMMJJ_HHMNSSMS
function FormatterDateHeureToporobot(const D: TDateTime; const WithMilliseconds: boolean = false): string;              // date au format JJ/MM/AA HH:MM:SS




// fonctions de couleurs
//**********************
// Passent un TColor [0; 255] x [0; 255] x [0; 255] en [0; 1] x [0; 1] x [0; 1]
function GetNTSCGrayScale(const C: TColor): Byte; inline;
function GetFloatRValue(const C: TColor): double; inline;
function GetFloatGValue(const C: TColor): double; inline;
function GetFloatBValue(const C: TColor): double; inline;

function SVGColor(const C: TColor): string;
//function KMLColor(const C: TColor; const A: byte = 255): string; overload; inline;
function KMLColor(const R, G, B, A: byte): string; inline;


function Acad2RGB(const n : integer) : tColor; deprecated; // palette par défaut d'Autocad
function RGB2Acad(const C: TColor) : Byte; deprecated;

function GetColorDegrade(const z, zmin, zmax: Double; const Coul1, Coul2: TColor): TColor;

// couleur RGBA au format HTML
function ColorToHTMLColor(const Couleur: TColor): string;
function ColorHTMLToColor(const CouleurHTML: string): TColor;
function ColorToGHTopoColor(const C: TColor): string;
function GHTopoColorToColorDef(const C: String; const Default: TColor): TColor;
// intervalle de couleurs
function ChooseColorInColorArray(const QIdx: integer; const AC: array of TColor; const DefaultColor: TColor = clSilver): TColor;
function ChooseColorFromInterval(const Value: double; const Intervalles: array of double; const AC: array of TColor; const DefaultColor: TColor = clSilver): TColor;
//function GLColorToTColor(const C: TGLCol
//********************
// fonctions de calculs trigo et sur les visées
function Hypot2D(const DX, DY: Double): Double; inline;
function Hypot3D(const DX, DY, DZ: Double): Double; inline;
function DistanceBetweenTwoTPoint3Df(const P1, P2: TPoint3Df): double;

function DegMinSec2DegDec(const S: string): double;
function CalculerAngleBissecteur(const dX1, dY1, dX2, dY2: double): double;  // calcul de l'angle bissecteur de deux segments

function GetAzimut(const dx, dy: Double; const Unite: double): double;
procedure GetBearingInc(const dx, dy, dz: double; var Dist, Az, Inc: double; const fUB, fUC: Double); // Azimut et pente
procedure CalculerVisee(var MaVisee: TUneVisee;       // calcul d'une visée
                        const CC: TCode; const EE: TExpe;
                        var QCoordsCourantes: TPoint3Df;
                        var QChainageHZ: double);


function GetTypeDeVisee(const T: integer): TTypeDeVisee;          // retourne le type de visé
function GetDescTypeVisee(const T: TTypeDeVisee): string;         // et sa description
// extraire le libellé d'une étiquette de terrain depuis un commentaire  - L'étiquette commence par un '@' et ne comporte pas d'espace
function ExtractIDTerrainFromComment(const S: string): string;

function CalcCorrectionAngulaire(const CC: TCode; const LongDev, DeltaZReference: double): double;    // calcul de la constante de correction des mesures verticales en fonction de la longueur du cheminement
function CalcAzimutCorrige(const AzBrut: double;
                           const UniteBoussole: double;                    // 360 = degré, 400 = grade
                           const ParamsFunc: TParamFoncCorrectionAngulaire): double;
function CalcPenteCorrigee(const PenteBrute: double;
                           const UniteClino: double;                    // 360 = degré, 400 = grade
                           const ParamsFunc: TParamFoncCorrectionAngulaire): double;

// fonctions sur les entités GHTopo
//*********************************
function  IsSameSerie(const E0, E1: TBaseStation): boolean;
function  DecomposeStationToporobot(const S: string): TToporobotIDStation;
procedure DecomposeNumeroSerie(const QNumSerie: TNumeroSerie; out QIdxNameSpace, QNoSerie: integer);
procedure DecomposeNumeroCode(const QNumCode: TNumeroCode; out QIdxNameSpace, QNoCode: integer);
procedure DecomposeNumeroExpe(const QNumExpe: TNumeroExpe; out QIdxNameSpace, QNoExpe: integer);

function  GetNamespaceOfSerie(const N: TNumeroSerie): integer; inline;
function  GetNamespaceOfCode(const N: TNumeroCode): integer; inline;
function  GetNamespaceOfExpe(const N: TNumeroExpe): integer; inline;
function  GetNamespaceOfEntrance(const N: TNumeroEntrance): integer; inline;
function  GetNamespaceOfReseau(const N: TNumeroReseau): integer; inline;
function  GetNamespaceOfSecteur(const N: TNumeroSecteur): integer; inline;
function  MakeLabelNoeud(const ND: TJonctionXYZ): String;



function  ExtractSerStFromTIDStation(const QIDBaseStation: TIDBaseStation; out QSR: TNumeroSerie; out QST: TNumeroStation): boolean;
function  ChooseColorByTypeEntite(const TE: TTypeDeVisee): TColor;
function  FormatterIDTerrainStation(const S: string): string;

// fonctions de calcul de coordonnées et déclinaison magnétique (calcul ponctuel sur couple de valeur ou procédural sur tableaux)
function  ConversionCoupleCoordonneesIsoleesEPSG(const Src, Tgt: integer; const PointIn: TPoint2Df; out PointOut: TPoint2Df): boolean;
function  ConversionCoordonneesIsoleesEPSG(const Src, Tgt: integer; const PointInX, PointInY: double; out PointOutX, PointOutY: double): boolean;
function  CalcDeclimagIsolee(const Lat, Lon, Alt: double; const MyDate: TDate; out DecliMagValue: double): boolean;    // calcul d'une déclinaison magnétique isolée
function  CalculerDeclinaisonMagnetique(const Lat, Lon, Alt: double; const YYYY, MM, DD: word): double;

// fonctions sur les grilles: Ont toutes été retirées: TStringGrid contient des méthodes équivalentes
{$NOTE GRDCCopierUnTableau(), GRDChargerDepuisFichier(), GRDSauverUnTableauEnCSV(), GRDRemplirDepuisStringList: replaced by equivalent methods in TStringGrid v > 1.6.0}
function  GRDCollerDepuisClipBoard(const Grille: TStringGrid; const HasTitres: boolean; const QSeparateur: char = #9): boolean;

// fonctions espacement de grilles et équidistance
function  ProposerEquidistanceDef(const C1, C2: TPoint3Df; const Defaut: double): Double; // proposer une équidistance en fonction de l'étendue du réseau
function  EnsureSetQuadrillageSpacing(const S: double): double; inline;                   // définir l'équidistance d'une grille

// fonctions diverses
function  ChooseFilter(const s90: string): integer; inline;     //retourne: -1 = Filtre erronné;  0 = Valeur simple  ; 1 = Intervalle
// appel d'un programme externe
function  RunExternalProgram(const ProgName: string; const Params: string; const FileToOpen: string): integer;
// création d'un GUID
function  GetGUID(): string;
// construction de la ligne d'entêtes d'une section pour export formats tabulaires
function  MakeHeaderRubriquesOfAnRow(const Separateur: char; const Section: integer; const Rubriques: array of string): string;
// calcul de condensat
function  CalcSHA1(const S: string): string;
// échappement des guillemets et autres. Même nom que la fonction php.
function  mysqli_real_escape_string(const S: string): string;
//******************************************************************************
// calcul d'un azimut moyen
function  CalcAzimutMoyen(const GradAz: double; const ArrAzimuts: array of double): double;
// azimut proche d'un azimut donné
// Azimut    : Azimut à tester
// Azimutref : Azimut de référence
// Tolerance : Tolérance angulaire en unités d'angle
function  IsNearToAzimut(const GradAz: double; const Azimut, AzimutRef: double; const ToleranceAngulaire: double): boolean;
function  IsNearToInclinaison(const GradInc: double; const Inclinaison, InclinaisonRef: double; const ToleranceAngulaire: double): boolean;
function  IsNearToLongueur(const Longueur, LongueurRef: double; const ToleranceLongueur: double): boolean;

function  CalcAzimutMoyenOfTMesuresViseeDistoX(const GradAz, GradInc: double;
                                               const ToleranceDistances: double;
                                               const ToleranceAngulaire: double;
                                               const ArrVisees: array of TMesureViseeDistoX;
                                               out   ViseeMoyenne: TMesureViseeDistoX): boolean;
// fabrication des lignes Code, Expe, Secteurs, ...
function  MakeToporobotTabLineOfExpe(const IdxTABSection: integer; const Expe: TExpe): string;
function  MakeToporobotTabLineOfCode(const IdxTABSection: integer; const ModeSaveTab: TModeSaveTAB; const Code: TCode): string;

// crée un nom de fichier avec la date indiquée, pour les fichiers de backup notamment
function  MakeFilenameFromDate(const Prefix: string; const MyDate: TDateTime; const Extension: string): TStringDirectoryFilename;
// crée un nom valide pour une couche OSM
function  CheckOSMLayerVarName(const QLayerNameSubmitted: string; out QLayerNameValidated: string): boolean;

// trigo en degrés et grades
function  sing(X: double): double;
function  cosg(X: double): double;
function  tang(X: double): double;
function  sind(X: double): double;
function  cosd(X: double): double;
function  tand(X: double): double;

// conversion en filtre et visse-versa
function  ConvertirEnFiltreSeries(const S: string): string;
// coefficient de concordance entre deux textes
function CalcDistanceJaroWinkler(const s1,s2: string):double;
// Distance de Damerau-Levenshtein
function CalcDistanceDamerauLevenshtein(const S1, S2: string): integer;
//******************************************************************************

// Surcharge d'opérateurs (non supportés en mode Delphi)
//Operator + (V1, V2: TPoint3Df) Result : TPoint3Df;
//Operator - (V1, V2: TPoint3Df) Result : TPoint3Df;

//******************************************************************************
// Upload unitaire d'un fichier
function FTP_TestConnexion(const QFTPParams: TFTPParameters; const QPassWord: string; out QReturnCode: integer; out QReturnMsg: string): boolean;
function FTP_CreateFolder(const QFTPParams: TFTPParameters; const QPassWord: string; const QPath, QDirectory: TStringDirectoryFilename): boolean;
function FTP_UploadUniqueFile(const QFTPParams: TFTPParameters; const QPassWord: string; const QFileNameSRC: TStringDirectoryFilename; const QDirectoryTGT: TStringDirectoryFilename): boolean;
function FTP_UploadMultiplesFiles(const QFTPParams: TFTPParameters; const QPassWord: string; const QFilesSRC: array of TStringDirectoryFilename; const QDirectoryTGT: TStringDirectoryFilename): boolean;
function FTP_UploadFolder(const QFTPParams: TFTPParameters; const QPassWord: string; const QDirectorySRC, QDirectoryTGT: TStringDirectoryFilename): boolean;




implementation
uses
  {$IFDEF MSWINDOWS}
    {$IFDEF GHTOPO_SIMPLIFIE}
       frmRPIMainWnd,
    {$ELSE}
       frmJournal,

    {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
     {$IFDEF RASPBERRY_PI}
       frmRPIMainWnd,
     {$ENDIF}
  {$ENDIF}
  UnitRemoteFunctions,
  UnitTSAGeoMag,

  ConvertisseurJPC in './UnitesMutualisees/convertisseurjpc.pas'; // API JPC

function FTP_TestConnexion(const QFTPParams: TFTPParameters; const QPassWord: string; out QReturnCode: integer; out QReturnMsg: string): boolean;
var
  TD: TRemoteServicesFTP;
begin
  AfficherMessageErreur('Test de la connexion');
  result := false;
  TD := TRemoteServicesFTP.Create;
  try
    Result := TD.SetParamsAndConnect(QFTPParams.HostName, StrToIntDef(QFTPParams.Port, 21), QFTPParams.User, QPassWord, QReturnCode, QReturnMsg);
  finally
    FreeAndNil(TD);
  end;
end;

function FTP_CreateFolder(const QFTPParams: TFTPParameters; const QPassWord: string; const QPath, QDirectory: TStringDirectoryFilename): boolean;
var
  TD: TRemoteServicesFTP;
  QReturnCode: integer;
  QReturnMsg: string;
begin
  result := false;
  TD := TRemoteServicesFTP.Create;
  try
    if (TD.SetParamsAndConnect(QFTPParams.HostName, StrToIntDef(QFTPParams.Port, 21), QFTPParams.User, QPassWord, QReturnCode, QReturnMsg)) then
    begin
      TD.CreateDirectory(QPath, QDirectory);
    end;
    TD.Finaliser();
    result := True;
  finally
    FreeAndNil(TD);
  end;

end;

//******************************************************************************
// Upload unitaire d'un fichier
function FTP_UploadUniqueFile(const QFTPParams: TFTPParameters; const QPassWord: string; const QFileNameSRC: TStringDirectoryFilename; const QDirectoryTGT: TStringDirectoryFilename): boolean;
var
  EWE: array of TStringDirectoryFilename;
begin
  Result := false;
  Setlength(EWE, 1);
  EWE[0] := QFileNameSRC;
  Result := FTP_UploadMultiplesFiles(QFTPParams, QPassWord, EWE, QDirectoryTGT);
end;

function FTP_UploadMultiplesFiles(const QFTPParams: TFTPParameters; const QPassWord: string; const QFilesSRC: array of TStringDirectoryFilename; const QDirectoryTGT: TStringDirectoryFilename): boolean;
var
  TD: TRemoteServicesFTP;
  QReturnCode: integer;
  QReturnMsg: string;
  i, Nb: integer;
begin
  AfficherMessageErreur('FTP_UploadMultiplesFiles(): ' + QDirectoryTGT);
  result := false;
  TD := TRemoteServicesFTP.Create;
  try
    if (TD.SetParamsAndConnect(QFTPParams.HostName, StrToIntDef(QFTPParams.Port, 21), QFTPParams.User, QPassWord, QReturnCode, QReturnMsg)) then
    begin
      TD.SetCurrentRemoteDirectory(QDirectoryTGT);
      Nb := length(QFilesSRC);
      for i := 0 to Nb-1 do TD.SendFile(QFilesSRC[i]);
    end;
    TD.Finaliser();
    result := True;
  finally
    FreeAndNil(TD);
  end;
end;
function FTP_UploadFolder(const QFTPParams: TFTPParameters; const QPassWord: string; const QDirectorySRC, QDirectoryTGT: TStringDirectoryFilename): boolean;
var
  TD: TRemoteServicesFTP;
  QReturnCode, QIndent: integer;
  QReturnMsg: string;
  QDir: TStringDirectoryFilename;
  (*
  Count:=0;
  If FindFirst ('*',faAnyFile,Info)=0 then
    begin
    Repeat
      Inc(Count);
      With Info do
        begin
        If (Attr and faDirectory) = faDirectory then
          Write('Dir : ');
        Writeln (Name:40,Size:15);
        end;
    Until FindNext(info)<>0;
    FindClose(Info);
    end;
  Writeln ('Finished search. Found ',Count,' matches');
  //*)
  procedure ParcourirUnDossier(const QFPath, QFSubPath: string);
  var
    EWE: string;
    SearchPath: string;
    SearchRec : SysUtils.TSearchRec;
    LocalName : string;
    QRT: TStringDirectoryFilename;
  begin
    SearchPath := QFPath + DirectorySeparator;
    EWE := StringOfChar(' ', QIndent);
    AfficherMessageErreur(EWE + '+-->' + QFPath + DirectorySeparator + QFSubPath);
    QIndent += 2;
    if QFSubPath <> '' then SearchPath += QFSubPath + DirectorySeparator;
    SearchPath += '*';

    if (SysUtils.FindFirst(SearchPath, faAnyFile and faDirectory, SearchRec) = 0) then
    begin
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          LocalName := SearchRec.Name;
          if QFSubPath <> '' then LocalName := QFSubPath + DirectorySeparator + LocalName;
          if(SearchRec.Attr and faDirectory) = faDirectory then
          begin // on passe dans un sous-dossier
            QRT := QDirectoryTGT + '/' + StringReplace(QFPath, DirectorySeparator, '/', [rfReplaceAll]);
            ///TD.SetCurrentRemoteDirectory(QRT);
            ParcourirUnDossier(QFPath, LocalName);
          end
          else
          begin // traitement sur un fichier
            AfficherMessageErreur(EWE + '+-- ' + LocalName);//Lignes.Add(LocalName);

            ///TD.SendFile(LocalName);
          end;
        end;
      until SysUtils.FindNext(SearchRec) <> 0;
      QIndent -= 2;
    end;
    SysUtils.FindClose(SearchRec);
  end;
begin
  QIndent := 0;
  result := false;
  QDir   := ExtractFileName(QDirectorySRC);
  ClearConsoleErreur();
  AfficherMessageErreur(Format('Envoi de "%s" sur %s', [QDirectorySRC, QDirectoryTGT]));
  TD := TRemoteServicesFTP.Create;
  try
    if (TD.SetParamsAndConnect(QFTPParams.HostName, StrToIntDef(QFTPParams.Port, 21), QFTPParams.User, QPassWord, QReturnCode, QReturnMsg)) then
    begin
      TD.SetCurrentRemoteDirectory(QDirectoryTGT);
      ParcourirUnDossier(QDirectorySRC, '');
    end;
    TD.Finaliser();
    result := True;
  finally
    FreeAndNil(TD);
  end;
end;


//******************************************************************************
// fonctions utilitaires (console, mémoire)
//******************************************************************************
// pseudo-mot clé ne faisant rien
procedure pass; inline;    // void pass()
begin                      // {
  ;; //asm NOP end;        // __asm__{ NOP}
end;                       // }


// non factorisation de ce code pour éviter une dépendance
procedure AfficherMessage(const Msg: string; const DoCRLF: boolean = true);
var
  WU: String;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF GHTOPO_SIMPLIFIE}
     with RPIMainWnd do begin
    {$ELSE}
     with dlgProcessing do begin
    {$ENDIF}
       try
         if (not Assigned(lsbJournal.Items)) then Exit;
         WU := TimeToStr(Now()) +' | ' + AnsiToUtf8(Msg);
         if (DoCRLF) then lsbJournal.Items.Add(WU)
                     else lsbJournal.Items[lsbJournal.Count - 1] := WU;
         if (lsbJournal.Items.Count > MAX_LINES_LOG) then  lsbJournal.Items.Delete(0);
         lsbJournal.ItemIndex := lsbJournal.Items.Count-1;
       except
         //  Application.ProcessMessages désactivé:
         // si la fenêtre Journal est recouverte, une erreur 'Indice hors limites' se produit
         // Ce bug ne se produit pas si cette fenêtre est partiellement visible
         //Application.ProcessMessages;
       end;
     end; // with {RPIMainWnd | dlgProcessing }
  {$ENDIF}
  {$IFDEF LINUX}
    WriteLn(StdOut, DateTimePascalToDateTimeSQL(Now()) + ': ' + AnsiToUtf8(Msg));
    {$IFDEF RASPBERRY_PI}
    with RPIMainWnd do begin
      try
        if (not Assigned(lsbJournal.Items)) then Exit;
        WU := TimeToStr(Now()) +' | ' + AnsiToUtf8(Msg);
        if (DoCRLF) then lsbJournal.Items.Add(WU)
                    else lsbJournal.Items[lsbJournal.Count - 1] := WU;
        if (lsbJournal.Items.Count > MAX_LINES_LOG) then  lsbJournal.Items.Delete(0);
        lsbJournal.ItemIndex := lsbJournal.Items.Count-1;
      except
        //  Application.ProcessMessages désactivé:
        // si la fenêtre Journal est recouverte, une erreur 'Indice hors limites' se produit
        // Ce bug ne se produit pas si cette fenêtre est partiellement visible
        //Application.ProcessMessages;
      end;
    end;
    {$ENDIF RASPBERRY_PI}
  {$ENDIF}
end;

// non factorisation de ce code pour éviter une dépendance
procedure AfficherMessageErreur(const Msg: string);
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF GHTOPO_SIMPLIFIE}
    with RPIMainWnd do begin
    {$ELSE}
    with dlgProcessing do begin
    {$ENDIF}
      try
        if (not Assigned(memoErreurs.Lines)) then Exit;
        memoErreurs.Lines.Add(Msg);
        memoErreurs.CaretY := memoErreurs.Lines.Count - 1;
        Application.ProcessMessages;
      except
      end;
    end; // with {RPIMainWnd | dlgProcessing }
  {$ENDIF}
  {$IFDEF LINUX}
    WriteLn(StdErr, DateTimePascalToDateTimeSQL(Now()) + ': ' + AnsiToUtf8(msg));
    {$IFDEF RASPBERRY_PI}
      with RPIMainWnd do begin
        try
          if (not Assigned(memoErreurs.Lines)) then Exit;
          memoErreurs.Lines.Add(Msg);
          memoErreurs.CaretY := memoErreurs.Lines.Count - 1;
          Application.ProcessMessages;
        except
        end;
      end; // with {RPIMainWnd | dlgProcessing }
    {$ENDIF}
  {$ENDIF}
end;

procedure ClearConsole();
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF GHTOPO_SIMPLIFIE}
     with RPIMainWnd do begin
    {$ELSE}
     with dlgProcessing do begin
    {$ENDIF}
       try
         if (not Assigned(lsbJournal.Items)) then Exit;
         lsbJournal.Clear;
       except
         //  Application.ProcessMessages désactivé:
         // si la fenêtre Journal est recouverte, une erreur 'Indice hors limites' se produit
         // Ce bug ne se produit pas si cette fenêtre est partiellement visible
         //Application.ProcessMessages;
       end;
     end; // with {RPIMainWnd | dlgProcessing }
  {$ENDIF}
  {$IFDEF LINUX}
    pass; // WriteLn(AnsiToUtf8(Msg));
  {$ENDIF}
  pass;
end;

procedure ClearConsoleErreur();
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF GHTOPO_SIMPLIFIE}
     with RPIMainWnd do begin
    {$ELSE}
     with dlgProcessing do begin
    {$ENDIF}
       try
         if (not Assigned(memoErreurs.Lines)) then Exit;
         memoErreurs.Clear;
         Application.ProcessMessages;
       except
       end;
     end; // with {RPIMainWnd | dlgProcessing }
  {$ENDIF}
  pass;
end;
procedure AfficherMemoryUsage();
{$IFDEF MSWINDOWS}
var
  M: TMEMORYSTATUS;
  EWE: THeapStatus;
  WU: String;
{$ENDIF MSWINDOWS}
begin
   {$IFDEF MSWINDOWS}
     try
       M.dwLength := sizeof(TMEMORYSTATUS);
       GlobalMemoryStatus(M);
       EWE := GetHeapStatus;
       WU := Format(GetResourceString(rsGHTOPO_MEMORY_USAGE),
             [EWE.TotalAddrSpace / 1.0,
              EWE.TotalAllocated / 1.0,
              EWE.TotalFree / 1.0]);

       {$IFDEF GHTOPO_SIMPLIFIE}RPIMainWnd{$ELSE}dlgProcessing{$ENDIF}.lbMemoryUsage.Caption := WU;
       Application.ProcessMessages;
     except
       pass;
     end;
  {$ENDIF}
  {$IFDEF LINUX}
     pass;
  {$ENDIF}
end;

procedure DimensionnerEtCentrerFenetre(const FRM: TForm);
begin
  {$IFDEF GHTOPO_SIMPLIFIE}
    {$IFDEF RASPBERRY_PI}
    FRM.Position := poScreenCenter;
    FRM.Width    := Min(Screen.Width  - 16, Min(FRM.Width , 800));
    FRM.Height   := Min(Screen.Height - 40, Min(FRM.Height, 600));
    {$ENDIF}
  {$ENDIF}
end;


// exécuter un programme externe
procedure ExecuteExternalProgram(const MyCommandLine: string);
begin
  with TProcess.Create(Application) do   (* Création du processus *)
  begin
    try
      CommandLine := MyCommandLine;             (* Définition de la commande à exécuter *)
      Options := Options + [poWaitOnExit];      (* On Attendra la fin du programme externe avant de continuer *)
      Execute;                                  (* Tout est prêt pour l'exécution *)
    finally
      Free;
    end;
  end;
end;
function MakeTitleMainWindowGHTopo(const S: string): string;
const
  FMT_GHTOPO_TITLE: string = '%s - %s %s [%s]';
var
  EWE, WU: String;
begin
  {$IFDEF GHTOPO_SIMPLIFIE}
    WU := 'LT';
    {$IFDEF RASPBERRY_PI}
    WU += ' ' + 'RpiPI';
    {$ELSE}
    WU += ' ' + 'x86';
    {$ENDIF RASPBERRY_PI}
    EWE    := Format('Version: %s', [{$I %DATE%}]);
  {$ELSE}
    EWE    := Format('Built: %s %s', [{$I %DATE%}, {$I %TIME%}]);
    {$IFDEF GROS_MINET}
      WU := 'GM';
    {$ELSE}
      WU := 'FX';
    {$ENDIF GROS_MINET}
  {$ENDIF GHTOPO_SIMPLIFIE}
  Result := Format(FMT_GHTOPO_TITLE, [S, rsGHTOPOEXENAME, WU, EWE]);
end;
function DescribeCriticiteErreur(const c: TCriticiteMessaqeErreurGHTopo): string;
begin
  Result := ChooseString(Ord(c), [GetResourceString(rsCRITICITE_ERROR_INFORMATION),
                                  GetResourceString(rsCRITICITE_ERROR_WARNING),
                                  GetResourceString(rsCRITICITE_ERROR_ERROR),
                                  GetResourceString(rsCRITICITE_ERROR_CRITICAL),
                                  GetResourceString(rsCRITICITE_ERROR_FIXED)]);
end;

function DescribeTableExamineeByCode(const c: TTableExaminee): string;
begin
  Result := ChooseString(Ord(c), [GetResourceString(rsCHECKED_TABLE_ENTRANCES),
                                  GetResourceString(rsCHECKED_TABLE_RESEAUX),
                                  GetResourceString(rsCHECKED_TABLE_SECTEURS),
                                  GetResourceString(rsCHECKED_TABLE_CODES),
                                  GetResourceString(rsCHECKED_TABLE_EXPES),
                                  GetResourceString(rsCHECKED_TABLE_SERIES)]);
end;
// Convertir un fichier texte vers le format désiré
// cette variante utilise un TStringList
function ConvertTextFile(const InputFileName, OutputFilename: TStringDirectoryFilename;
                         const InputFormat, OutputFormat: TTextFileFormat): boolean;
var
  FO   : TextFile;
  ENDL : string;   // fin de ligne
  ALine: string;
  i    : integer;
  LS   : TStringList;
begin
  Result := False;
  if Not(FileExistsUTF8(InputFileName)) then Exit;
  case OutputFormat of
    tfWINDOWS: ENDL := #13+#10;
    tfUNIX   : ENDL := #10;
    tfMAC    : ENDL := #13;
  end;
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.LoadFromFile(InputFileName);
    AssignFile(FO, OutputFilename);
    ReWrite(FO);
    try
      try
        for i:=0 to LS.Count-1 do
        begin
          ALine := LS.Strings[i]+ENDL; // ne PAS purger la chaîne !!!
          Write(FO, ALine);
        end;
        Result := True;
      except
      end;
    finally
      CloseFile(FO);
    end;
    LS.Clear;
    Result := True;
  finally
    FreeAndNil(LS);
  end;
end;
//******************************************************************************
// gestion du dossier de GHTopo
//******************************************************************************
function GetGHTopoVersion(): string;
begin
  Result := Format('%.10f', [pi]);
end;

function GetGHTopoDirectory(): TStringDirectoryFilename;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

function GetLastDirectoryUsed(): TStringDirectoryFilename;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

function ExtractDirectoryOfGHTopoDocument(const FN: TStringDirectoryFilename): TStringDirectoryFilename; inline;
begin
  Result := ExtractFileDir(FN);
end;
function EnsureMakeFilename(const S: TStringDirectoryFilename): TStringDirectoryFilename;
var
  Lin: String;
  L: Integer;
  procedure RemplacerChar(const Caracts: string);
  var
    w: Integer;
    C: Integer;
    QC: Char;
    Nb: Integer;
  begin
    Nb := Length(Caracts);
    for C := 1 to Nb do
    begin
      QC := Caracts[C];
      for w := 1 to L do if (Lin[w] = QC) then Lin[w] := '_';
    end;
  end;
begin
  Lin := Trim(RemoveDiacritiques(S)); //
  L   := Length(Lin);
  if (L = 0) then begin Result :=''; Exit; end;
  // on déquille certains caractères spéciaux en remplaçant par des '_'
  RemplacerChar('^& #\"''(=+*/<>)[:,;!.%?]{~|`@}$');
  Result := Lin;
end;
//******************************************************************************
// makers de structures
//******************************************************************************
function MakeTPoint(const QX, QY: integer): TPoint;
begin
  Result.X := QX;
  Result.Y := QY;
end;

function MakeTPointCoupeDeveloppee(const QP, QZ: double): TPointCoupeDeveloppee;
begin
  Result.P := QP;
  Result.Z := QZ;
end;
function MakeTMarker(const D: boolean; const QX, QY: double; const C: TColor; const S: string): TMarker;
begin
  Result.Displayed := D;
  Result.X := QX;
  Result.Y := QY;
  Result.Couleur := C;
  Result.Caption := Trim(S);
end;


{$WARNING: TEXpe.DateExpe à implementer}



function MakeTStationMatchFound(const S, P: integer; const M: string): TStationMatchFound;
begin
  Result.Serie   := S;
  Result.Station := P;
  Result.Match   := M;
end;




//******************************************************************************
// fonctions communes:
//******************************************************************************
// Fonctions d'échange
procedure Swap(var V1, V2: integer); overload;
var Tmp: integer;
begin
  Tmp  := V1;
  V1   := V2;
  V2   := Tmp;
end;
procedure Swap(var V1, V2: Double); overload;
var Tmp: Double;
begin
  Tmp  := V1;
  V1   := V2;
  V2   := Tmp;
end;
procedure Swap(var V1, V2: string); overload;
var Tmp: string;
begin
  Tmp  := V1;
  V1   := V2;
  V2   := Tmp;
end;
procedure Swap(var V1, V2: TMNTNumeroTriangle); overload;
var Tmp: TMNTNumeroTriangle;
begin
  Tmp  := V1;
  V1   := V2;
  V2   := Tmp;
end;
procedure Swap(var V1, V2: TMNTNumeroVertex); overload;
var Tmp: TMNTNumeroVertex;
begin
  Tmp  := V1;
  V1   := V2;
  V2   := Tmp;
end;

// IF immédiats
function IIF(const Condition: boolean; const V1, V2: integer): integer; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function IIF(const Condition: boolean; const V1, V2: double): double; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function IIF(const Condition: boolean; const V1, V2: boolean): boolean; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;

function IIF(const Condition: boolean; const V1, V2: TColor): TColor; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;

function IIF(const Condition: boolean; const V1, V2: String): String; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;

function IIF(const Condition: boolean; const V1, V2: char): char; overload;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function IsInRange(const Value, MinValue, MaxValue: double): Boolean; overload;
begin
  Result := ((Value >= MinValue) and (Value <= MaxValue));
end;
function IsInRange(const Value, MinValue, MaxValue: integer): Boolean; overload;
begin
  Result := ((Value >= MinValue) and (Value <= MaxValue));
end;
// test d'intersection de deux rectangles
function IntersectRectangles(const R1, R2: TRect2Df): boolean;
var
  BoundingBox: TRect2Df;  // boite englobante
  LB, HB     : double;
  LM, HM     : double;
begin
  Result:=False;
  BoundingBox.X1 := Math.Min(R1.X1, R2.X1);
  BoundingBox.Y1 := Math.Min(R1.Y1, R2.Y1);

  BoundingBox.X2 := Math.Max(R1.X2, R2.X2);
  BoundingBox.Y2 := Math.Max(R1.Y2, R2.Y2);

  LB:= BoundingBox.X2 - BoundingBox.X1;
  HB:= BoundingBox.Y2 - BoundingBox.Y1;

  LM:=(R1.X2 - R1.X1) + (R2.X2 - R2.X1);
  HM:=(R1.Y2 - R1.Y1) + (R2.Y2 - R2.Y1);
  Result:= (LM > LB) and (HM > HB);
end;





//******************************************************************************
// Fonctions de split et de choix dans des arrays
//******************************************************************************
function EmptyTGHStringArray(): TGHStringArray;
var
  i: Integer;
begin
  for i := 0 to High(Result) do Result[i]:='';
end;
// Version à conserver: ExtractStringsToTStringArray() semble supprimer les items vides
function Split(const MyStr: string; const Sep: char): TGHStringArray;
var
  pn   : integer;
  ps   : integer;
  S    : string;
begin
  Result := EmptyTGHStringArray();
  S      := MyStr;
  ps     := 0;
  try
    pn:=0;
    repeat
      if (pn > High(Result)) then Break;
      ps := Pos(Sep, S);
      Result[pn] := Trim(Copy(s,0, ps-1));
      Inc(pn);
      s := Copy(s, 1+ps, Length(s));
    until ps = 0;
    Result[pn-1] := Trim(s);
  except
  end;
end;
// Version qui envoie le résultat dans un TStringList
function SplitToTStringList(const MyStr: string; const Sep: char; var LSR: TStringList; const QSorted: boolean; const QDuplicates: TDuplicates): boolean;
var
  S1, S2 : string;
  P: SizeInt;
begin
  Result := false;
  S1     :=  '';
  S2     := Trim(MyStr);
  if (S2 = '') then exit(false);
  try
    if (not Assigned(LSR)) then raise Exception.Create('SplitToTStringList: Le paramètre LSR doit être créé par TStringList.Create');
    LSR.Sorted     := QSorted;
    LSR.Duplicates := QDuplicates;
    LSR.Clear;
    repeat
      P  := Pos(Sep, S2);
      s1 := trim(Copy(s2,1,P - 1));
      s2 := Trim(Copy(s2, P + 1,Length(s2)));
      if (Length(S1) > 0) then LSR.Add(s1);
    until P =0;
    if (Length(S2) > 0) then LSR.Add(s2);
    Result := (LSR.Count > 0);
  except
  end;
end;

function ExtractStringsToTStringArray(Separators, WhiteSpace: TSysCharSet; const MyStr: string): TGHStringArray;
var
  b, c : pchar;
  quoted : char;
  IdxResultArray: Integer;
  procedure SkipWhitespace;
  begin
    while (c^ in Whitespace) do inc (c);
  end;
  procedure AddString;
  var
    l : integer;
    s : string;
  begin
    l := c-b;
    if (l > 0) then
    begin
      setlength(s, l);
      move (b^, s[1],l);
      Result[IdxResultArray] := s;
      IdxResultArray += 1;
    end;
  end;
begin
  Result:= EmptyTGHStringArray();
  IdxResultArray := 0;
  c := PChar(MyStr);
  Quoted := #0;
  Separators := Separators + [#13, #10] - ['"'];
  SkipWhitespace;
  b := c;
  while (c^ <> #0) do
  begin
    if (c^ = Quoted) then
    begin
      if ((c+1)^ = Quoted) then inc (c) else Quoted := #0;
    end
    else if ((Quoted = #0) and (c^ in ['"'])) then Quoted := c^;
    if (Quoted = #0) and (c^ in Separators) then
    begin
      AddString;
      inc (c);
      SkipWhitespace;
      b := c;
    end
    else
      inc (c);
  end;
  if (c <> b) then AddString;
end;

function DecoupeChaine(const S: string;
                       const Separators, WhiteSpace: TSysCharSet;
                       const DoRemoveQuotes: boolean): TGHStringArray;
var
  i: Integer;
  function RemoveQuote(const S: string): string;
  begin
    Result := StringReplace(S, '"', '', [rfReplaceAll]);
  end;
begin
  Result := ExtractStringsToTStringArray(Separators, WhiteSpace, S);
  if (DoRemoveQuotes) then
  begin
    for i := 0 to high(Result) do Result[i] := RemoveQuote(Result[i]);
  end;
end;

function DecoupeChaineEspacesGuillemets(const S: string; const DoRemoveQuotes: boolean): TGHStringArray;
var
  i: Integer;
  function RemoveQuote(const S: string): string;
  begin
    Result := StringReplace(S, '"', '', [rfReplaceAll]);
  end;
begin
  Result := ExtractStringsToTStringArray([' '], [], S);
  if (DoRemoveQuotes) then
  begin
    for i := 0 to high(Result) do Result[i] := RemoveQuote(Result[i]);
  end;
end;
// extraire les champs d'une chaîne formattée en colonnes fixes (like Toporobot Text)
function SplitFixedColLine(const MyLine: string; const Tabs: array of integer): TGHStringArray;
var
  pn   : integer;
  ps   : integer;
  S    : string;
begin
  Result := EmptyTGHStringArray();
  for pn := 0 to High(Tabs)-1 do begin
    Result[pn] := Copy(MyLine, Tabs[pn], (Tabs[pn+1] - Tabs[pn]));
  end;
end;

// extraire les champs d'une chaîne de la forme abcd def   "daech must die" "miaou" toto
function SplitWithQuotedFields(const S: string; const Separator: char; const AQuoteChar: char = '"'): TGHStringArray;
var
  LS: TStringList;
  n, i: Integer;
begin
  Result := EmptyTGHStringArray();
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.QuoteChar := AQuoteChar;
    LS.Delimiter := Separator;
    LS.DelimitedText := S;
    n := Min(LS.Count, Length(Result));
    if (n > 0) then
    begin
      for i := 0 to n - 1 do Result[i] := LS.Strings[i];
    end;
    LS.Clear;
  finally
    FreeAndNil(LS);
  end;
end;
//*)

// obtenir un tableau 2D à partir d'une string
function MakeTStringArray2DFromStr(const S: string; const SepCol: char): TGHStringArray2D;
var
  i,j: integer;
  P : integer;
  S1: string;
  MesLignes: TGHStringArray;
begin
  //NbLignes := 0;
  // pour ne pas être perturbé par les débuts de ligne
  P := Pos(SepCol,S);
  if (P>0) then S1 := '$' + SepCol + S + SepCol+'$';
  S1 := Trim(S);
  // purger
  for i:=0 to MAX_SIZE_PARAM_ARRAY do
    for j:=0 to MAX_SIZE_PARAM_ARRAY do
      Result[i,j] := '';
  // virer les retour-chariots
  P := 0;
  P := Pos(#13, S1);
  while (P>0) do begin
    System.Delete(S1, P, 1);
    P:=Pos(#13, S1);
  end;
  MesLignes := Split(S, #10);
  for i:=0 to MAX_SIZE_PARAM_ARRAY do AfficherMessage(Format('>>GetStringArray2D: MesLignes[%d] = %s',[i, MesLignes[i]]));
  // découper les lignes
  for i:=0 to MAX_SIZE_PARAM_ARRAY do Result[i] := Split(MesLignes[i], SepCol);
end;

function ChooseColorInColorArray(const QIdx: integer; const AC: array of TColor; const DefaultColor: TColor = clSilver): TColor;
begin
  Result := IIF(IsInRange(QIdx, 0, High(AC)), AC[QIdx], DefaultColor);
end;
// couleurs en fonction d'un intervalle
// ------------------------------------------------------------------------
//   default  | Color 0 | Color 1 | Color 2 | Color 3 | Color 4 | Color 5
// -----------0---------1---------2---------3---------4---------5----------
function ChooseColorFromInterval(const Value: double; const Intervalles: array of double; const AC: array of TColor; const DefaultColor: TColor = clSilver): TColor;
var
  n, p, i: Integer;
begin
  Result := DefaultColor;
  n := High(Intervalles);
  p := High(AC);
  if (n <> p) then exit;
  if (Value < Intervalles[0]) then Exit;
  if (Value > Intervalles[n]) then Exit(AC[n]);
  for i := 0 to n - 1 do
  begin
    if (IsInRange(Value, Intervalles[i], Intervalles[i+1])) then Exit(AC[i]);
  end;
end;

// extraction et dépilage du premier élément d'une string
function ExtractFirstElementOfStr(var Str: string; const Sep: string): string;
var
  ps, n: Integer;
begin
  Result := '';
  try
    Str := Trim(Str) + Sep;
    ps:=Pos(Sep, Str);
    if (ps > 0) then
    begin
      Result := Copy(Str, 1, ps - 1);
      Str := Copy(Str, ps+1, Length(Str));
    end;
    // et on vire le séparateur de fin s'il existe
    n := length(Str);
    if ((n > 0) and (Str[n] = Sep)) then System.Delete(Str, n, 1);
  except
  end;
end;
function IndexOfString(const S: string; const IsEqual: boolean; const Strs: array of string): integer;
var
  i: integer;
  C: boolean;
begin
  Result := -1;
  try
    for i := Low(Strs) to High(Strs) do
    begin
      if (IsEqual) then C := (LowerCase(S) = LowerCase(Strs[i]))     // Strs[i] est identique à S
                   else C := (Pos(S, Strs[i]) > 0);                  // Strs[i] contient S
      if (C) then Exit(i);
    end;
  except
    Result:=-1;
  end;
end;

// choisir une chaine en fonction d'une valeur
function ChooseString(const Index: integer; const Strs: array of string; const Default: string = '---'): string;
begin
  try
    if (Index < 0) or (Index > High(Strs)) then Exit(Default);//Format('** Erroneous index: %d **',[Index]);
    Result := Strs[Index];
  finally
  end;
end;

function ChooseChar(const Index: integer; const Strs: array of char; const Default: char = #32): char;
begin
  try
    if (Index < 0) or (Index > High(Strs)) then Exit(Default);//Format('** Erroneous index: %d **',[Index]);
    Result := Strs[Index];
  finally
  end;
end;

//******************************************************************************
// fonctions de chaines
//******************************************************************************
function _AnsiToLCLStr(const S: string): string; inline;
begin
  Result := system.AnsiToUtf8(S); // pour Lazarus < 1.6
end;
function _LCLStrToAnsi(const S: string): string; inline;
begin
  Result := System.Utf8ToAnsi(S); // pour Lazarus < 1.6
end;

// fonction sécurisée d'utilisation de resoursestrings
function GetResourceString(const RS: string): string; inline;
begin
  Result := _AnsiToLCLStr(RS);
end;
// fonction d'incrémentation lexicographique
// Si le string ne comporte que des lettres, on ajoute un indice à la fin
// Si le string comporte un préfixe et un indice, on incrémente l'indice
function IncrementString(const S0: string): string;
var
  i,q: integer;
  Groupe: string;
  Prefix: string;
  Reste : string;
  Index : integer;
  procedure DecomposeLitteral();
  const
    Seps =':./-_,;';
  var
    a,b: integer;
  begin
    Groupe:='';
    for a:=1 to length(Seps) do begin
      b:=Pos(Seps[a], S0);
      if (b > 0) then begin // si le séparateur est trouvé, on sort le résultat
        Groupe:=Copy(S0, 1, b-1)+Seps[a];
        Break;
      end;
    end;
    Reste := IIF(Groupe = '', S0, Copy(S0, b+1, Length(S0)));
  end;
begin
  Result := '';  // Si chaine vide, on renvoie rien
  if (S0 = '') then Exit;
  // rechercher un groupe (série, etc ...)  séparé par un ":", ".", "/", "-", "_"
  DecomposeLitteral();
  // rechercher un chiffre
  Q := -1;
  for i:=1 to Length(Reste) do begin
    if IsInRange(Ord(Reste[i]), Ord('0'), Ord('9')+1) then begin
      Q:=i;
      Break;
    end;
  end;
  // Si chiffre trouvé:
  if Q>0 then begin
    Prefix:=Copy(Reste, 1, Q-1);
    Index :=StrToIntDef(Copy(Reste, Q, Length(Reste)-Q+1),0);
  end else begin
    Prefix:=Trim(Reste);
    Index:=0;
  end;
  Result := Format('%s%s%d',[Groupe, Prefix, Index+1]);
end;

// enlève les perluettes
function EnlevePerluete(const s: string): string;
begin
  result := StringReplace(s, '&', '', [rfReplaceAll, rfIgnoreCase]);
end;
// troncature sécurisée d'un texte à la longueur L
function SafeTruncateString(const Str: string; const L: integer): String;
begin
 if Length(Str)>L then
   Result := Trim(Copy(Str, 1, L))
 else
   Result := Trim(Str);
end;
function RemoveDiacritiques(const S: string): string;
begin
  //Result := AnsiToUtf8(S);
  Result := S;
  // pour le français
  Result := StringReplace(Result, #195#169, 'e', [rfIgnoreCase, rfReplaceAll]);   //é
  Result := StringReplace(Result, #195#168, 'e', [rfIgnoreCase, rfReplaceAll]);   //è
  Result := StringReplace(Result, #195#170, 'e', [rfIgnoreCase, rfReplaceAll]);   //ê
  Result := StringReplace(Result, #195#171, 'e', [rfIgnoreCase, rfReplaceAll]);   //ë
  Result := StringReplace(Result, #195#160, 'a', [rfIgnoreCase, rfReplaceAll]);   //à
  Result := StringReplace(Result, #195#162, 'a', [rfIgnoreCase, rfReplaceAll]);   //â
  Result := StringReplace(Result, #195#164, 'a', [rfIgnoreCase, rfReplaceAll]);   //ä
  Result := StringReplace(Result, #195#174, 'i', [rfIgnoreCase, rfReplaceAll]);   //î
  Result := StringReplace(Result, #195#175, 'i', [rfIgnoreCase, rfReplaceAll]);   //î
  Result := StringReplace(Result, #195#180, 'o', [rfIgnoreCase, rfReplaceAll]);   // ô
  Result := StringReplace(Result, #195#182, 'o', [rfIgnoreCase, rfReplaceAll]);   // ö
  Result := StringReplace(Result, #195#185, 'u', [rfIgnoreCase, rfReplaceAll]);       //'ù'
  Result := StringReplace(Result, #195#187, 'u', [rfIgnoreCase, rfReplaceAll]);    // 'û'
  Result := StringReplace(Result, #195#188, 'u', [rfIgnoreCase, rfReplaceAll]);  //'ü'
  Result := StringReplace(Result, #195#167, 'c', [rfIgnoreCase, rfReplaceAll]);      // ç
end;

// sortie formatée pour colonnages fixes
function FormatterFixedColumn(const S: string; const W: integer; const Aligment: TAlignment): string;
var
  L, LL: Integer;
  G, D: Integer;
  EWE: String;
begin
  EWE := Trim(S);
  LL := Length(EWE);
  L := W - LL;
  if (LL > W) then EWE := Copy(EWE, 1, W); // tronque si S est plus grand que la colonne
  case Aligment of
    taLeftJustify   : G := 0;
    taRightJustify  : G := L;
    taCenter        : G := L div 2;
  end;
  case Aligment of
    taLeftJustify   : D := L;
    taRightJustify  : D := 0;
    taCenter        : D := (L div 2) - 1;
  end;
  Result := StringOfChar(#32, G) + EWE + StringOfChar(#32, D);
end;



function SupprimeEspacesMultiples(const S: string): string;
var
  P: integer;
begin
  Result := Trim(S);
  P:=Pos('  ', Result);
  if (P = 0) then Exit;
  while (P > 0) do
  begin
    System.Delete(Result, P, 1);
    P := Pos('  ', Result);
  end;
end;

//******************************************************************************
// fonctions XML
//******************************************************************************
// version du XML et encodage
function FormatterVersionEncodageXML(const version: string; const encodage: string): string; inline;
begin
  Result := Format('<?xml version="%s" encoding="%s"?>', [version, encodage]);
end;
function ensureConvertLongOrLatToXML(const Value: double): string;
begin
  Result := Format('%.10f', [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
end;
function ensureConvertAltitudeToXML(const Value: double): string;
begin
  Result := Format(FORMAT_NB_REAL_3_DEC, [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
end;
function PasStrToXMLStr(const S: string): string;
begin
  Result := S; // nothing
end;

function XMLStrToPasStr(const S: string): string;
begin
  Result := S; // nothing
end;
// neutralise les entités HTML (pour le support du XML)
(*
function EscapeXMLEntities(const S: string): string;
begin
  Result := StringReplace(Result, '&', XML_CHAR_AMP, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(S, '<', XML_CHAR_LT, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '<', XML_CHAR_LT, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '>', XML_CHAR_GT, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '"', XML_CHAR_QUOT, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '''', XML_CHAR_APOS, [rfIgnoreCase, rfReplaceAll]);
end;
function EscapeXMLEntities(const S: string): string;
begin
  Result := StringReplace(Result, '&', XML_CHAR_AMP, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(S, '<', XML_CHAR_LT, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '<', XML_CHAR_LT, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '>', XML_CHAR_GT, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '"', XML_CHAR_QUOT, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '''', XML_CHAR_APOS, [rfIgnoreCase, rfReplaceAll]);
end;
//*)

//******************************************************************************
// fonction de formattage de nombres
//******************************************************************************
// sécurisé avec AnsiToUTF8
function FormatterNombreAvecSepMilliers(const X: double; const NbDecs: integer = 3): string;
var
  EWE: String;
begin
  EWE := Format('%%.%dn', [NbDecs]);
  Result := Format(EWE, [X]);
  Result := StringReplace(Result, DefaultFormatSettings.ThousandSeparator, ' ', [rfReplaceAll]);
end;
// convertir un  nombre Pascal en nombre OOo Calc
function  PasNumberToOOoCalcFormula(const V: double): string;
var
  EWE: String;
begin
  EWE := '=' + FloatToStr(V);
  Result := StringReplace(EWE, DefaultFormatSettings.DecimalSeparator, ',', [rfReplaceAll, rfIgnoreCase]);
end;
// formatage OOo des nombres
function FormatterNombreOOo(const Value: double; const NbDecs: integer = 3; const IgnoreNulls: boolean = false): string;
var
  EWE: String;
begin
  EWE := Format('%%.%df', [NbDecs]);
  // remplacer les points par des virgules
  Result := Format(EWE, [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, ',', [rfReplaceAll, rfIgnoreCase]);
  if (IgnoreNulls and SameValue(Value, 0)) then Result := '';
end;

// formatage des nombres pour les formats d'échanges imposant le point décimal
function FormatterNombreWithDotDecimal(const Value: double; const NbDecs: integer = 3; const IgnoreNulls: boolean = false): string;
var
  EWE: String;
begin
  EWE := Format('%%.%df', [NbDecs]);
  Result := Format(EWE, [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
end;

// convertir en nombre des valeurs de type 123 456 789.10 ou 1245-45785
function ConvertirEnNombreReel(const S: string; const Default: double): double;
var
  st: String;
  m1: double;
  i : integer;
  c : char;
  procedure S666(const QC: char);
  begin
    if (st[1] = '-') then m1 := -1.00;
    if (st[1] = QC) then system.Delete(st, 1, 1);
  end;
begin
  try
    Result := Default;
    m1 := 1.00;
    st := Trim(s);  // on nettoie la chaîne
    if (st = '') then Exit(Default);
    // on vire les guillemets
    st := StringReplace(st, '"', ' ', [rfIgnoreCase, rfReplaceAll]);
    S666('''');       // on vire l'apostrophe de début
    S666('-');        // sauvegarde du signe
    S666('+');        // on vire le '+' de tête
    // balayage avec nettoyage et remplacement du point décimal
    ST := StringReplace(ST, ' ', '', [rfReplaceAll]);
    ST := StringReplace(ST, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
    ST := StringReplace(ST, '.', DefaultFormatSettings.DecimalSeparator, [rfIgnoreCase, rfReplaceAll]);
    ST := StringReplace(ST, ',', DefaultFormatSettings.DecimalSeparator, [rfIgnoreCase, rfReplaceAll]);
    // on ne retient que les chiffres
    result := StrToFloat(ST) * m1;
  except
    Result := Default;
  end;
end;

//******************************************************************************
// fonctions de dates
//******************************************************************************
function GetAnneeISO(const QAnnee: word): word;
begin
  try
    Result := QAnnee;
    if (QAnnee >  1900 + ANNEE_PIVOT) then Exit; // année sur quatre chiffres -->[]
    Result := IIF(QAnnee <= ANNEE_PIVOT, QAnnee + 2000, QAnnee + 1900);
  except
    Result := 1899;
  end;
end;
// encodage/Décodage sécurisé des dates
function  GetSecuredDate(Year, Month, Day: Word): TDateTime;
var
  YYYY: Word;
begin
  try
    YYYY := Year;
    if (Year < 100) then YYYY := IIF(Year <= ANNEE_PIVOT, Year + 2000, Year + 1900);
    Result := EncodeDate(YYYY, Month, Day);
  except
    result := Now();
  end;
end;
// Dates SQL
function DateYYYYMMDDToDateSQL(const YYYY, MM, DD: word): string; inline;
begin
  Result := Format('%.4d-%.2d-%.2d', [YYYY, MM, DD]);
end;

function DatePascalToDateSQL(const MyDate: TDateTime): string;
var
  YYYY, MM, DD: word;
begin
  DecodeDate(MyDate, YYYY, MM, DD);
  Result := DateYYYYMMDDToDateSQL(YYYY, MM, DD);
end;

function DateSQLToDatePascal(const MyDateSQL: string): TDateTime;
var
  YYYY, MM, DD: word;
  QDateSQL: String;
begin
  QDateSQL := Trim(MyDateSQL);
  try
    YYYY := StrToInt(Copy(QDateSQL, 1, 4));
    // années sur deux chiffres
    if (YYYY < 100) then
    begin
      if (YYYY < 50) then  YYYY := 2000 + YYYY
                     else  YYYY := 1900 + YYYY;
    end;
    MM   := StrToInt(Copy(QDateSQL, 6, 2));
    DD   := StrToInt(Copy(QDateSQL, 9, 2));
    Result := EncodeDate(YYYY, MM, DD);
  except
    Result := TDateTime(0.0);
  end;
end;
function DateTimeSQLToDateTimePascal(const MyDateSQL: string): TDateTime;
var
  YYYY, MM, DD  : word;
  HH, MN, SS, MS: word;
  QDate, QTime  : TDateTime;
  QDateSQL: String;
begin
  QDateSQL := Trim(MyDateSQL);
  try
    YYYY := StrToInt(Copy(QDateSQL, 1, 4));
    // années sur deux chiffres
    if (YYYY < 100) then
    begin
      if (YYYY < 50) then  YYYY := 2000 + YYYY
                     else  YYYY := 1900 + YYYY;
    end;
    MM   := StrToInt(Copy(QDateSQL, 6, 2));
    DD   := StrToInt(Copy(QDateSQL, 9, 2));
    // 0        1111111112
    // 1234567890123456789
    // 2017-01-02 12:34:56

    HH   := StrToInt(Copy(QDateSQL, 12, 2));
    MN   := StrToInt(Copy(QDateSQL, 15, 2));
    SS   := StrToInt(Copy(QDateSQL, 18, 2));
    MS   := 0;
    QDate := EncodeDate(YYYY, MM, DD);
    QTime := EncodeTime(HH, MN, SS, MS);
    Result := ComposeDateTime(QDate,QTime);
  except
    Result := Now();
  end;
end;

function DateTimePascalToDateTimeSQL(const MyDate: TDateTime; const WithMilliseconds: boolean = true): string;
var
  YYYY, MM, DD: word;
  HH, MN, SS, MS: word;
begin
  DecodeDate(MyDate, YYYY, MM, DD);
  DecodeTime(MyDate, HH, MN, SS, MS);
  Result := Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [YYYY, MM, DD, HH, MN, SS]);
  if (WithMilliseconds) then Result += Format('.%.3d', [MS]);
end;

// Dates condensées AAAAMMJJ_HHMNSSMS
function DatePascalToDateHeureCondensee(const QDate: TDateTime): string;
var
  YYYY, MM, DD: word;
  HH, MN, SS, MS: word;
begin
  DecodeDate(QDate, YYYY, MM, DD);
  DecodeTime(QDate, HH, MN, SS, MS);
  Result := Format('%.4d-%.2d-%.2d_%.2dh%.2dm%.2ds%.3dms', [YYYY, MM, DD, HH, MN, SS, MS]);
end;

function FormatterDateHeureToporobot(const D: TDateTime; const WithMilliseconds: boolean = false): string;      // date au format JJ/MM/AA HH:MM:SS
var
  JJ, MM, AA, HH, MN, SS, MS: word;
begin
  DecodeDate(D, AA, MM, JJ);
  AA := AA mod 100; // ne retenir que les deux derniers chiffres
  DecodeTime(D, HH, MN, SS, MS);
  Result := Format('%.2d/%.2d/%.2d %.2d:%.2d:%.2d', [JJ, MM, AA, HH, MN, SS]);
  if (WithMilliseconds) then Result += Format('.%.3d', [MS]);
end;





//******************************************************************************
// fonctions de couleurs
//******************************************************************************
//Convertit une couleur en niveaux de gris; méthode NTSC.
function GetNTSCGrayScale(const C: TColor): Byte;
begin
  Result := round(0.30 * Red(C) + 0.59 * Green(C) + 0.11 * Blue(C));
end;
// fonctions de couleurs recadrées sur intervalle [0.00; 1.00]
function GetFloatRValue(const C: TColor): double;
begin
  Result := Red(C) / 256.0;
end;
function GetFloatGValue(const C: TColor): double;
begin
  Result := Green(C) / 256.0;
end;
function GetFloatBValue(const C: TColor): double;
begin
  Result := Blue(C) / 256.0;
end;
function SVGColor(const C: TColor): string;
var
  R,G,B: Double;
begin
  R := 100.0 * GetFloatRValue(C);
  G := 100.0 * GetFloatGValue(C);
  B := 100.0 * GetFloatBValue(C);
  //Result:=Format('#%X%X%X',[R,G,B]);
  Result:=Format(' rgb(%.2f%%, %.2f%%, %.2f%%)', [R,G,B]);
end;


function KMLColor(const R, G, B, A: byte): string; inline;
begin
  Result := Format('<color>%.2X%.2X%.2X%.2X</color>',[A, B, G, R]);
end;





function Acad2RGB(const n : integer) : tColor; // palette par défaut d'Autocad
var r,g,b,
    c,d,u : integer;
    C1 : Tcolor;  // RGB(r, g ,b)
    const StandCol : array[0..9] of tcolor =
        (clBlack ,clRed,$0001E1F3{clYellow},$0000C800{clLime},$00FDE302{clAqua},
         clBlue,clFuchsia,clBlack,clGray,clLtGray);
    const FullPalete : array[0..23] of tcolor =
        ($0000FF,$0040FF,$0080FF,$00C0FF,
         $00FFFF,$00FFC0,$00FF80,$00FF40,
         $00FF00,$40FF00,$80FF00,$C0FF00,
         $FFFF00,$FFC000,$FF8000,$FF4000,
         $FF0000,$FF0040,$FF0080,$FF00C0,
         $FF00FF,$C000FF,$8000FF,$4000FF);//..$0000FF (retour au début)
begin
  c := n mod 256; // au cas ou ?
  if (c < 10) then exit(StandCol[c]);

  d := ((c-10) div 10);// 0..23
  // Couleur de base à corriger
  C1 := FullPalete[d mod 24];
  // Correction:--------------------------------
  d := c div 10; // dizaines
  u := c-d*10; // unités
  // séparation des couleurs RGB
  b := (C1 and $FF0000) shr 16;
  g := (C1 and $00FF00) shr 8;
  r := (C1 and $0000FF);
  //Plus clair pour les impairs
  if ((u div 2)*2<>u) then
  begin
    b := b + ((255-b) div 2);
    g := g + ((255-g) div 2);
    r := r + ((255-r) div 2);
  end;
  // Plus foncé si u grand
  b := b*4 div (u+4);
  g := g*4 div (u+4);
  r := r*4 div (u+4);
  // Couleur corrigée:---------------------------
  C1 := RGBToColor(r,g,b);
  result := C1;
end;
function RGB2Acad(const C: TColor): Byte; deprecated;
var dRMin, dGmin, dBmin  :integer;
    CouleurR, CouleurG, CouleurB: integer;
    ACcolor,  dColor, res       : integer;
    ACcolorR, ACcolorG, ACcolorB: integer;
begin
    Result:=0;
    dRmin := 99999999;
    dGmin := 99999999;
    dBmin := 99999999;
    CouleurR := Red(C);
    CouleurG := Green(C);
    CouleurB := Blue(C);
    for res := 1 to 255 do
    begin
      ACcolor := Acad2RGB(res);
      ACcolorR := Red(ACcolor);
      ACcolorG := Red(ACcolor);
      ACcolorB := Blue(ACcolor);
      dColor := abs(ACcolorR - CouleurR)+
                abs(ACcolorG - CouleurG)+
                abs(ACcolorB - CouleurB);
      if (dColor < dRmin) then
      begin
          dRmin := dColor;
          result := res;
      end;
    end;
end;

// dégradé de couleurs
function GetColorDegrade(const z, zmin, zmax: Double;
                         const Coul1, Coul2: TColor): TColor;
var
  D: Double;
  H: Double;
  DR, DG, DB : integer; // et non byte !!!
begin
  if (Z < ZMin) then Exit(Coul1);
  if (Z > zmax) then Exit(Coul2);
  D := zmax - zmin;
  if (Abs(D) < 1e-8) then Exit(Coul1);
  H := (z - zmin) / D;
  DR := Red(Coul2)   - Red(Coul1);
  DG := Green(Coul2) - Green(Coul1);
  DB := Blue(Coul2)  - Blue(Coul1);
  Result := RGBToColor(Red(Coul1)   + Trunc(DR * H),
                       Green(Coul1) + Trunc(DG * H),
                       Blue(Coul1)  + Trunc(DB * H));
end;
function ColorToHTMLColor(const Couleur: TColor): string;
begin
  Result := Format('#%.2X%.2X%.2X', [Red(Couleur), Green(Couleur), Blue(Couleur)]);
end;
function ColorToPascalColor(const Couleur: TColor): string;
begin
  Result := Format('$%.2X%.2X%.2X', [Red(Couleur), Green(Couleur), Blue(Couleur)]);
end;
function ColorHTMLToColor(const CouleurHTML: string): TColor;
var
  WU: String;
begin
  WU      := Trim(CouleurHTML);
  if (WU = '') then exit(clGray);
  WU[1]   := '$';
  Result  := StringToColorDef(WU, clGray);
end;
function ColorToGHTopoColor(const C: TColor): string;
begin
  Result := Format('$%.3d%.3d%.3d', [Red(C), Green(C), Blue(C)]);
end;

function GHTopoColorToColorDef(const C: String; const Default: TColor): TColor;
var
  R, G, B: byte;
begin
  if (length(C) < 10) then Exit(clGray); // la notation Pascal exige 1 + 3*3 caractères
  R := StrToIntDef(Copy(C, 2, 3), Red(Default));
  G := StrToIntDef(Copy(C, 5, 3), Green(Default));
  B := StrToIntDef(Copy(C, 8, 3), Blue(Default));
  Result := RGBToColor(R, G, B);
end;

//******************************************************************************
// fonctions de calculs trigo et sur les visées
//******************************************************************************
function Hypot2D(const DX, DY: Double): Double;
begin
  Result := Sqrt(dx*dx + dy*dy);
end;
function Hypot3D(const DX, DY, DZ: Double): Double;
begin
  Result := Sqrt(dx*dx + dy*dy +dz*dz);
end;

function DistanceBetweenTwoTPoint3Df(const P1, P2: TPoint3Df): double;
begin
  result := Hypot3D(P2.X - P1.X, P2.Y - P1.Y, P2.Z - P1.Z);
end;

function DegMinSec2DegDec(const S: string): double;
var
  WU: String;
  EWE: TGHStringArray;
  i: Integer;
  dd, mn, ss, Signe: double;
  QAT: Boolean;
begin
  Result := -361.00;
  try
    WU := Trim(UpperCase(S));
    // S'il y a des NaN, on sort direct
    if (Pos('NAN', WU) > 0) then exit;
    // extraction du signe
    QAT := (Pos('-', WU) > 0) or
           (Pos('S', WU) > 0) or
           (Pos('W', WU) > 0) or
           (Pos('O', WU) > 0);
    Signe := IIF(QAT, -1.0, 1.0);
    // nettoyage
    for i := length(WU) downto 1 do
    begin
      if (not(WU[i] in ['0' .. '9', chr(194), '-', '.', ',', '''', '"', 'N', 'M', 'S', 'O', 'W', 'E', 'D'])) then
        system.Delete(WU, i, 1);
    end;
    //SigneLongitude := IIF((Pos('O', WU) > 0) or (Pos('W', WU) > 0) or (Pos('S', WU) > 0), -1, 1);
    WU := StringReplace(WU, ',', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);

    WU := StringReplace(WU, chr(194), 'd', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, 'd', ':', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, '''', ':', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, '"', ':', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, '''''', ':', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, 'd', ':', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, 'N', '', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, 'S', '', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, 'E', '', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, 'O', '', [rfReplaceAll, rfIgnoreCase]);
    WU := StringReplace(WU, 'W', '', [rfReplaceAll, rfIgnoreCase]);
    EWE := Split(WU, ':');
    AfficherMessageErreur(Format('"%s"', [EWE[0]]));
    AfficherMessageErreur(Format('"%s"', [EWE[1]]));
    AfficherMessageErreur(Format('"%s"', [EWE[2]]));

    dd := Abs(ConvertirEnNombreReel(EWE[0], 0.00));
    mn := Abs(ConvertirEnNombreReel(EWE[1], 0.00));
    ss := Abs(ConvertirEnNombreReel(EWE[2], 0.00));
    Result := Signe * (dd + mn/60 + ss/3600);
  except
    Result := -361.00;
  end;
end;

// calcul de l'angle bissecteur de deux segments
function CalculerAngleBissecteur(const dX1, dY1, dX2, dY2: double): double;
var
  V1, V2, W: TPoint3Df;
begin
  // vecteur V1           vecteur V2        vecteur w
  V1.setFrom(dX1, dY1, 0.00);
  V2.setFrom(dX2, dY2, 0.00);
  W.setFrom(0, 0, 1);
  // produits vectoriels
  v1 := ProduitVectoriel(v1,w,True);
  v2 := ProduitVectoriel(v2,w,True);
  //composition vectorielle
  w.setFrom(v1.x + v2.X, v1.y + v2.Y, v1.z + v2.z);
  // angles
  Result := ArcTan2(w.y+1e-12, w.x +1e-12);
end;
// retourne un azimut
// TODO: Revoir cette fonction (bug avec les grades)
function GetAzimut(const dx, dy: Double; const Unite: double): double;
const TWO_PI = 2 * PI;
var
  a: double;
begin
  a := ArcTan2(dy, dx + 1e-12);
  if (a < 0) then a := a + TWO_PI;
  a := 0.50 * PI - a;
  if (a < 0) then a := a + TWO_PI;
  Result := a * 0.50 * Unite / pi;
end;
// retourne la longueur, direction et pente pour dx, dy, dz
procedure GetBearingInc(const dx, dy, dz: double;  var Dist, Az, Inc: double; const fUB, fUC: Double);
var
  dp: Double;
begin;
  dp   := Hypot2D(dx, dy);
  Dist := Hypot2D(dp,dz);
  Inc  := ArcTan2(dz, dp) * 0.5 * fUC / pi;
  Az   := GetAzimut(dx,dy, fUB);
end;
// calcul des accroissements pour une visée
procedure CalculerVisee(var MaVisee: TUneVisee;  const CC: TCode; const EE: TExpe; var QCoordsCourantes: TPoint3Df; var QChainageHZ: double);
const
  TWO_PI = 2*PI;
  PI_SUR_2 = pi / 2;
var
  LeCode: TCode;
  LaExpe: TExpe;
  ubb   : integer;
  ucc   : integer;
  udd   : double;
  RX, RY, RZ, RP: double;
  UB, UC, LP: double;
  Az1: double;
  Pente1, QCorrectionPente, QLongueurVisee, QDeclinaison: double;
  procedure CalcDXDY(const QLP, QAz1: double; out QRx, QRy: double);
  begin
    QRX := QLP * Sin(QAz1);
    QRY := QLP * Cos(QAz1);
  end;
begin
  LeCode := CC;
  LaExpe := EE;
  QLongueurVisee := (MaVisee.Longueur * CC.FactLong) +
                    0.50 * (CC.DiametreBoule1 + CC.DiametreBoule2); // ajouter le rayon des deux boules-cibles
  ubb := Round(LeCode.GradAz);
  ucc := Round(LeCode.GradInc);
  udd := 400.00;
  if (ubb = 0) then LeCode.GradAz  := DEGRES_PAR_TOUR;
  if (ucc = 0) then LeCode.GradInc := DEGRES_PAR_TOUR;
  // NOTA: La déclinaison magnétique est positive si Nm est à droite de Ng, négative sinon
  // Elle s'AJOUTE à l'azimut, et est TOUJOURS EN DEGRES
  QDeclinaison := LaExpe.DeclinaisonInDegrees;

  case ubb of
    359, 360: begin // visées directes en degrés
       UB := TWO_PI / DEGRES_PAR_TOUR;
       Az1 := CalcAzimutCorrige(MaVisee.Azimut,
                                DEGRES_PAR_TOUR,
                                LeCode.ParamsFuncCorrAz);
       Az1 := Az1 * UB + QDeclinaison * DEG_TO_RAD;
    end;
    399, 400: begin // visées directes en grades
       UB := TWO_PI / GRADES_PAR_TOUR;
       Az1 := CalcAzimutCorrige(MaVisee.Azimut,
                                GRADES_PAR_TOUR,
                                LeCode.ParamsFuncCorrAz);
       Az1:= Az1 * UB + QDeclinaison * DEG_TO_RAD;
    end;
    349, 350: begin  // visées inverses en degrés
       UB := TWO_PI / DEGRES_PAR_TOUR;
       Az1 := CalcAzimutCorrige(MaVisee.Azimut, DEGRES_PAR_TOUR, LeCode.ParamsFuncCorrAz);
       Az1 := Az1 * UB + QDeclinaison * DEG_TO_RAD;
       Az1 := PI+Az1;
    end;
    389, 390: begin // visées inverses en grades
       UB  := TWO_PI / GRADES_PAR_TOUR;
       Az1 := CalcAzimutCorrige(MaVisee.Azimut, GRADES_PAR_TOUR, LeCode.ParamsFuncCorrAz);
       Az1 := Az1 * UB + QDeclinaison * DEG_TO_RAD;
       Az1 := PI+Az1;
    end;
  end;
  (* Déterminer si on travaille en zénithal*)
  ucc := Round(LeCode.GradInc);
  // Correction erreurs systématiques des pentes
  QCorrectionPente := LeCode.ParamsFuncCorrInc.Co;
  case ucc of
    360, 400: begin // zéro à l'horizontale
      UC := TWO_PI/ucc;     // DONE: FIXED: 'ubb' remplacé par 'ucc'
      // corrections d'erreurs
      Pente1 := CalcPenteCorrigee(MaVisee.Pente, ucc, LeCode.ParamsFuncCorrInc);
      Pente1 := Pente1 * UC;
      LP := QLongueurVisee * Cos(Pente1);
      CalcDXDY(LP, Az1, RX, RY);
      RZ := QLongueurVisee * Sin(Pente1);
    end;
    361, 401: begin // zéro zénithal
      UC := TWO_PI/(ucc - 1);
      Pente1 := PI_SUR_2 - ((QCorrectionPente + MaVisee.Pente) * UC);
      LP := QLongueurVisee * Cos(Pente1);
      CalcDXDY(LP, Az1, RX, RY);
      RZ := QLongueurVisee * Sin(Pente1);
    end;
    359, 399: begin // zéro nadiral
      UC := TWO_PI/(ucc + 1);
      Pente1 := -(PI_SUR_2 - ((QCorrectionPente + MaVisee.Pente) * UC));
      LP := QLongueurVisee * Cos(Pente1);
      CalcDXDY(LP, Az1, RX, RY);
      RZ := QLongueurVisee * Sin(Pente1); // Fixé le 31/05.
    end;
    370: begin // DONE: Pourcentages sont exprimés sous la forme 59.11 pour 59.11%
      udd:= ArcTan(MaVisee.Pente / 100.0);
      LP := QLongueurVisee * Cos(udd);
      CalcDXDY(LP, Az1, RX, RY);
      RZ := QLongueurVisee * Sin(udd);
    end;
    380: // dans ce mode, les données sont 'Longueur VISEE et DENIVELEE
    begin // différences d'altitude, stockées ds Visee.Pente
      try
        LP := sqrt(QLongueurVisee * QLongueurVisee - MaVisee.Pente * MaVisee.Pente);
        CalcDXDY(LP, Az1, RX, RY);
        RZ := MaVisee.Pente;
      except
        AfficherMessage(Format('** ERROR ** Shot: %s: L=%.2f AZ=%.2f P=%.2f - RX = %.2f - RY = %.2f - LP = %.2f',
                               [MaVisee.IDTerrainStation,
                                QLongueurVisee, MaVisee.Azimut, MaVisee.Pente,
                                RX, RY, LP]));
      end;
    end;
    350, 390:
    begin // visées inverses en degrés/grades (ucc = UniteClino -10);
      UC := TWO_PI/(ucc + 10);
      Pente1 := -(QCorrectionPente + MaVisee.Pente) * UC;
      LP := QLongueurVisee * Cos(Pente1);
      CalcDXDY(LP, Az1, RX, RY);
      RZ := QLongueurVisee * Sin(Pente1);
    end;
    UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR,
    UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV: // support du lasermètre
    begin
      LP := QLongueurVisee;                   // longueur projetée est dans Long
      CalcDXDY(LP, Az1, RX, RY);
      RZ := MaVisee.Pente;                    // dénivelée est dans Pente
      if (ucc = UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV) then RZ := -RZ;     // visée inverse ?
    end;
  else
    begin // zéro horizontal par défault
      UC := TWO_PI / DEGRES_PAR_TOUR;
      Pente1 := (QCorrectionPente + MaVisee.Pente) * UC;
      LP := QLongueurVisee * Cos(Pente1);
      CalcDXDY(LP, Az1, RX, RY);
      RZ := QLongueurVisee * Sin(Pente1);
    end;
  end;
  //==============
  RP := Hypot(RX, RY);
  RZ += CC.ErreurTourillon; // On applique in fine la correction de l'erreur de tourillons
  QCoordsCourantes.X += RX;
  QCoordsCourantes.Y += RY;
  QCoordsCourantes.Z += RZ;
  QChainageHZ        += RP;
   MaVisee.AccroissXYZ.setFrom(RX, RY, RZ);
  MaVisee.AccroissP := RP;
end;
function GetTypeDeVisee(const T: integer): TTypeDeVisee;
begin
  try
    Result := TTypeDeVisee(T);
  except
    Result := tgDEFAULT;
  end;
end;

function GetDescTypeVisee(const T: TTypeDeVisee): string;
begin
  Result := ChooseString(Ord(T), [
                         'DEFAULT',
                         'ENTRANCE',
                         'FOSSILE',
                         'VADOSE',
                         'ENNOYABLE',
                         'SIPHON',
                         'FIXPOINT',
                         'SURFACE',
                         'TUNNEL',
                         'MINE',
                         'TYPE_ENTITES_VISEES_ANTENNE'
                        ]);
end;



// extraire le libellé d'une étiquette de terrain depuis un commentaire
// l'étiquette commence par un '@' et ne comporte pas d'espace
// L'étiquette est passée en majuscules
function ExtractIDTerrainFromComment(const S: string): string;
var
  p0: SizeInt;
begin
  Result := '';
  p0 := Pos('@', S);
  if (p0 = 0) then Exit;
  Result := Trim(Copy(S, p0+1, Length(S) - p0));
  Result := UpperCase(Result);
  p0 := Pos(' ', Result);
  if (p0 = 0) then Exit;
  Result := Copy(Result, 1, p0 - 1);
end;
// calcul de la constante de correction des mesures verticales en fonction de la longueur du cheminement
function CalcCorrectionAngulaire(const CC: TCode; const LongDev, DeltaZReference: double): double;
var
  dz: Double;
  WU: float;
begin
  if (IsZero(LongDev)) then Exit(0.00);
  dz := DeltaZReference + CC.ErreurTourillon;
  WU := arcsin(dz / LongDev);
  case Trunc(CC.GradInc) of
    360: Result := radtodeg(WU);
    400: Result := radtograd(WU);
  otherwise
    Result := 0.00; // en attente du support d'autres unités
  end;
end;
function CalcAzimutCorrige(const AzBrut: double;
                           const UniteBoussole: double;                    // 360 = degré, 400 = grade
                           const ParamsFunc: TParamFoncCorrectionAngulaire): double;
const
  TWO_PI = 2 * PI;
var
  AzC       : double;
  UnitToRad : double;
  Quadrant  : double;
  TermeTrigo: double;
begin
  //AfficherMessageErreur(Format('%.3f - %.2f - %.6f, %.6f, %.6f', [AzBrut, UniteBoussole, ParamsFunc.Co, ParamsFunc.ErreurMax, ParamsFunc.PosErrMax]));
  Result     := AzBrut;
  // erreur systématique constante: mauvais positionnement de l'aiguille aimantée
  AzC        := AzBrut + ParamsFunc.Co;
  UnitToRad  := TWO_PI / UniteBoussole;
  Quadrant   := UniteBoussole / 4;
  // prise en compte de l'erreur d'excentrement de l'axe de la boussole
  // PosErreuMax: Angle sous lequel est observée l'erreur la plus importante
  // ErreurMax: Valeur de l'erreur maximale
  TermeTrigo := UnitToRad * (Quadrant - (AzC - ParamsFunc.PosErrMax));
  Result     := AzC + ParamsFunc.ErreurMax * sin(TermeTrigo);
  if (Result > UniteBoussole) then Result := Result - UniteBoussole;
end;

function CalcPenteCorrigee(const PenteBrute: double;
                           const UniteClino: double;                    // 360 = degré, 400 = grade
                           const ParamsFunc: TParamFoncCorrectionAngulaire): double;
const
  TWO_PI = 2 * PI;
var
  UnitToRad  : double;
  TermeTrigo : double;
  PenteC     : double;
begin
  Result     := PenteBrute;
  // erreur systématique constante: mauvais positionnement du limbe sur la couronne du clinomètre
  PenteC     := PenteBrute + ParamsFunc.Co;
  UnitToRad  := TWO_PI / UniteClino;
  // prise en compte de l'erreur d'excentrement de l'axe de la couronne du clinomètre
  // PosErreuMax: Angle sous lequel est observée l'erreur la plus importante
  // ErreurMax: Valeur de l'erreur maximale
  TermeTrigo := UnitToRad * (PenteC - ParamsFunc.PosErrMax);
  // projection de l'erreur max sur la perpendiculaire à la visée
  // Ex: Sachant PosErrMax = 0, on considère que ErrMax est maximum pour une pente nulle
  //     et son influence varie avec le cos de (PenteC - ParamsFunc.PosErrMax), donc de la pente.
  Result     := PenteC + ParamsFunc.ErreurMax * abs(cos(TermeTrigo));
  //AfficherMessageErreur(Format('CalcPenteCorrigee:  Co = %.3f, CMax = %.3f - Incl = %.3f - PenteC = %.3f', [ParamsFunc.Co, ParamsFunc.ErreurMax, PenteBrute, Result]));
end;

//******************************************************************************
// fonctions sur les entités GHTopo
//******************************************************************************
function IsSameSerie(const E0, E1: TBaseStation): boolean;
begin
  Result:= (E0.Entite_Serie        =  E1.Entite_Serie) and
           (E0.Entite_Station      = (E1.Entite_Station - 1));

end;
function DecomposeStationToporobot(const S: string): TToporobotIDStation;
var
  EWE: TGHStringArray;
begin
  result.eIdxNameSpace := 0;
  result.aSerie:= -1;
  result.aStation:= -1;
  if (Pos('.', S) = 0) then Exit;
  EWE := Split(S, '.');
  try
    Result.aSerie := StrToIntDef(EWE[0], -1);
    Result.aStation := StrToIntDef(EWE[1], -1);
  except
  end;
end;
// extraire l'ID d'espace de noms et le numéro de série originel
procedure DecomposeNumeroSerie(const QNumSerie: TNumeroSerie; out QIdxNameSpace, QNoSerie: integer);
begin
  QIdxNameSpace := QNumSerie div NB_MAXI_SERIES_PAR_CAVITE;
  QNoSerie      := QNumSerie mod NB_MAXI_SERIES_PAR_CAVITE;
end;
procedure DecomposeNumeroCode(const QNumCode: TNumeroCode; out QIdxNameSpace, QNoCode: integer);
begin
  QIdxNameSpace := QNumCode div NB_MAXI_SERIES_PAR_CAVITE;
  QNoCode       := QNumCode mod NB_MAXI_SERIES_PAR_CAVITE;
end;
procedure DecomposeNumeroExpe(const QNumExpe: TNumeroExpe; out QIdxNameSpace, QNoExpe: integer);
begin
  QIdxNameSpace := QNumExpe div NB_MAXI_SERIES_PAR_CAVITE;
  QNoExpe       := QNumExpe mod NB_MAXI_SERIES_PAR_CAVITE;
end;

function GetNamespaceOfSerie(const N: TNumeroSerie): integer; inline;
begin
  Result := N div NB_MAXI_SERIES_PAR_CAVITE;
end;
function GetNamespaceOfCode(const N: TNumeroCode): integer; inline;
begin
  Result := N div NB_MAXI_SERIES_PAR_CAVITE;
end;
function GetNamespaceOfExpe(const N: TNumeroExpe): integer; inline;
begin
  Result := N div NB_MAXI_SERIES_PAR_CAVITE;
end;
function GetNamespaceOfEntrance(const N: TNumeroEntrance): integer; inline;
begin
  Result := N div NB_MAXI_SERIES_PAR_CAVITE;
end;
function GetNamespaceOfReseau(const N: TNumeroReseau): integer; inline;
begin
  Result := N div NB_MAXI_SERIES_PAR_CAVITE;
end;
function GetNamespaceOfSecteur(const N: TNumeroSecteur): integer; inline;
begin
  Result := N div NB_MAXI_SERIES_PAR_CAVITE;
end;

function MakeLabelNoeud(const ND: TJonctionXYZ): String;
var
  QIdxNameSpace, QNoSerie: TNumeroSerie;
  WU, EWE: String;
begin
  DecomposeNumeroSerie(ND.NoSer, QIdxNameSpace, QNoSerie);
  WU  := IIF(QIdxNameSpace = 0, '', Format('@%d', [QIdxNameSpace]));
  EWE := IIF(ND.IDJonction = '', '', ' - ' + ND.IDJonction);
  //Result := Format('%d.%d%s%s', [QNoSerie, ND.oNoSt, WU, EWE]);
  Result := Format('%d.%d%s', [QNoSerie, ND.NoSt, WU]);
end;

function ExtractSerStFromTIDStation(const QIDBaseStation: TIDBaseStation; out QSR: TNumeroSerie; out QST: TNumeroStation): boolean;
var
  QRT: LongInt;
begin
  DivMod(Abs(QIDBaseStation), NB_MAXI_SERIES_PAR_CAVITE, QSR, QRT);
  QST := QRT div MULTIPLICATEUR_STATION;
end;
function MakeTIDBaseStation(const S: TNumeroSerie; const P: integer; const IsAntenne: boolean): TIDBaseStation;
begin
  Result := NB_MAXI_SERIES_PAR_CAVITE * Abs(S) + MULTIPLICATEUR_STATION * Abs(P);
  if (IsAntenne) then Result := -Result;
end;
function ChooseColorByTypeEntite(const TE: TTypeDeVisee): TColor;
begin
  case TE of
    tgDEFAULT,
    tgFOSSILE   : result := clRed;
    tgVADOSE    : result := clBlue;
    tgENNOYABLE : result := clNavy;
    tgSIPHON    : result := clFuchsia;
    tgSURFACE   : result := clLime;
    tgTUNNEL    : result := clMaroon;
    tgMINE      : result := clTeal;
    tgVISEE_RADIANTE: result := clGray;
  else
    result := clBlack;
  end;
end;

function FormatterIDTerrainStation(const S: string): string;
begin
  Result := GetResourceString(Trim(S));
end;




//******************************************************************************
// fonctions de calcul de coordonnées et déclinaison magnétique
// (calcul ponctuel sur couple de valeur ou procédural sur tableaux)
//******************************************************************************


// conversion pour un couple de coordonnées isolées (version EPSG)
function ConversionCoupleCoordonneesIsoleesEPSG(const Src, Tgt: integer; const PointIn: TPoint2Df; out PointOut: TPoint2Df): boolean;
var
  WU, EWE: TProjUV;
  TC: TConversionSysteme;
begin
  Result := False;
  TC := TConversionSysteme.Create;
  try
    if (TC.Initialiser) then
    begin
      WU.U := PointIn.X;
      WU.V := PointIn.Y;

      EWE := TC.ConversionSyst1ToSyst2EPSG(Src, Tgt, WU);
      PointOut.X := EWE.U;
      PointOut.Y := EWE.V;
    end;
    TC.Finaliser;
    Result := True;
  finally
    FreeAndNil(TC);
  end;
end;

function ConversionCoordonneesIsoleesEPSG(const Src, Tgt: integer; const PointInX, PointInY: double; out PointOutX, PointOutY: double): boolean;
var
  PIN, POUT: TPoint2Df;
begin
  PIN.setFrom(PointInX, PointInY);
  POUT.Empty();
  Result := ConversionCoupleCoordonneesIsoleesEPSG(src, Tgt, PIN, POUT);
  PointOutX := POUT.X;
  PointOutY := POUT.Y;
end;

// calcul d'une déclinaison magnétique isolée
function CalcDeclimagIsolee(const Lat, Lon, Alt: double; const MyDate: TDate; out DecliMagValue: double): boolean;
var
  FCalculateurDeclimag: TSAGeoMag;
begin
  result := false;
  FCalculateurDeclimag := TSAGeoMag.Create;
  try
    FCalculateurDeclimag.Initialiser('');
    DecliMagValue := FCalculateurDeclimag.CalculerDeclinaison(Lat, Lon, Alt, MyDate);
    FCalculateurDeclimag.Finaliser();
    result := True;
  finally
    FreeAndNil(FCalculateurDeclimag);
  end;
end;
function CalculerDeclinaisonMagnetique(const Lat, Lon, Alt: double; const YYYY, MM, DD: word): double;
var
  DT: TDateTime;
begin
  Result := 0.00;
  DT := EncodeDate(YYYY, MM, DD);
  CalcDeclimagIsolee(Lat, Lon, Alt, DT, Result);
end;

//******************************************************************************
// fonctions sur les grilles: Ont toutes été retirées:
// TStringGrid contient des méthodes équivalentes
//******************************************************************************
function GRDCollerDepuisClipBoard(const Grille: TStringGrid; const HasTitres: boolean; const QSeparateur: char = #9): boolean;
var
  LesLignes: TStringList;
  EWE: String;
  P: SizeInt;
  Lin : String;
  procedure GRDRemplirDepuisStringList();
  var
    i, j: Integer;
    ValeursColonnes: TGHStringArray;
    R: Integer;
  begin
    Grille.ColCount := 1 + MAX_SIZE_PARAM_ARRAY + 2; // nb de colonnes = taille d'un TStringArray
    Grille.RowCount := IIF(HasTitres, LesLignes.Count, LesLignes.Count + 1);
    // on n'implémente pas un tableur => grille brute
    Grille.FixedCols   := 1;
    Grille.FixedRows   := 1;
    for i:= 0 to LesLignes.Count -1 do
    begin
      ValeursColonnes := Split(LesLignes[i], QSeparateur);
      if (HasTitres) then R := i else R := i+1;
      // renumérotation des lignes
      Grille.Cells[0, R] := IntToStr(i);
      for j := 0 to High(ValeursColonnes) do Grille.Cells[j+1, R] := ValeursColonnes[j];
    end;
  end;
begin
  Result := False;
  LesLignes := TStringList.Create;
  try
    LesLignes.Clear;
    try
      EWE := Trim(ClipBoard.AsText) + #13+#10;
      // découpage du texte
      repeat
        P := Pos(#10, EWE);
        Lin := Trim(Copy(EWE, 0, P-1));
        EWE := Copy(EWE, P+1, Length(EWE));
        if (Lin <> '') then LesLignes.Add(Lin);  // on zappe les lignes vides
      until (P = 0);
      if (LesLignes.Count = 0) then Exit(false);
      // remplissage du tableau
      GRDRemplirDepuisStringList();
    except
    end;
    LesLignes.Clear;
    Result := True;
  finally
    FreeAndNil(LesLignes);//LesLignes.Free;
    //ClipBoard.Free; // ne pas libérer le presse-papiers
  end;
end;

//******************************************************************************
// fonctions espacement de grilles et équidistance
//******************************************************************************
// Suggestion d'une équidistance en fonction de l'etendue totale du réseau
function ProposerEquidistanceDef(const C1, C2: TPoint3Df; const Defaut: double): Double;
var
  d: double;
begin
  Result:=Defaut;
  try
    d:=Hypot3D(C2.X - C1.X,
               C2.Y - C1.Y,
               C2.Z - C1.Z);
    d:=d/10;
    Result:=d;
    if IsInRange(d,    0.00,   10.00) then Result := 10.00;
    if IsInRange(d,   10.00,   25.00) then Result := 25.00;
    if IsInRange(d,   25.00,   50.00) then Result := 50.00;
    if IsInRange(d,   50.00,  100.00) then Result := 100.00;
    if IsInRange(d,  100.00,  200.00) then Result := 200.00;
    if IsInRange(d,  200.00,  250.00) then Result := 250.00;
    if IsInRange(d,  250.00,  500.00) then Result := 500.00;
    if IsInRange(d,  500.00, 1000.00) then Result := 1000.00;
    if IsInRange(d, 1000.00, 2000.00) then Result := 2000.00;
    if IsInRange(d, 2000.00, 5000.00) then Result := 5000.00;
    if IsInRange(d, 5000.00,10000.00) then Result := 10000.00;
    if IsInRange(d,10000.00,20000.00) then Result := 20000.00;
    // protection 'anti-nul'
    if (Result < 5.00) then Result := 5.00;
  except
    Result:=Defaut;
  end;
end;

// définir l'équidistance d'une grille
function EnsureSetQuadrillageSpacing(const S: double): double; inline;
const
  EQUIDISTANCE_MINI = 10.00;
begin
  Result := IIF(S < EQUIDISTANCE_MINI, EQUIDISTANCE_MINI, S);
end;

//******************************************************************************
// fonctions diverses
//******************************************************************************
//function makeToporobotDeclimag(const decl: double): double;
//begin
//  Result := -degtograd(decl);
//end;
// Détermine le type de filtre:
//    -1 = Filtre erronné
//     0 = Valeur simple
//     1 = Intervalle
function ChooseFilter(const s90: string): integer;
begin
  Result := IndexOfString(s90, False, ARRAY_OF_FILTERS);
end;
//******************************************************************************
// appel d'un programme externe
function RunExternalProgram(const ProgName: string; const Params: string; const FileToOpen: string): integer;
var
  AProcess: TProcess;                  // This is where our program starts to run
begin
// Now we will create the TProcess object, and
  Result := 0;
  // On vérifie si le programme existe
  if (not FileExistsUTF8(ProgName)) then exit(-2);
  AProcess := TProcess.Create(nil);
  try
    try
      // Tell the new AProcess what the command to execute is.
      // Let's use the FreePascal compiler
      AProcess.CommandLine := ProgName + ' ' + Params + ' ' + FileToOpen;
      // We will define an option for when the program
      // is run. This option will make sure that our program
      // does not continue until the program we will launch
      // has stopped running.
      //AProcess.Options := AProcess.Options + [poWaitOnExit];

      // Now that AProcess knows what the commandline is
      // we will run it.
      AProcess.Execute;
      Result := 0;
    except
      Result := 1;
      AfficherMessageErreur('*** Error calling external program ***');
    end;
  finally
    // This is not reached until ppc386 stops running.
    FreeAndNil(AProcess);//AProcess.Free;
  end;
end;
// création d'un GUID
function GetGUID(): string;                  // ['{1CBF26D7-BABA-4FF1-AE68-2D E1 96 9C F9 53}']
var
  MyGUID: TGUID;
  i: Integer;
begin
  Result := '';
  if (CreateGUID(MyGUID) = 0) then
  begin
    Result := Format('%.8X-', [MyGUID.time_low]) +
              Format('%.4X-', [MyGUID.time_mid]) +
              Format('%.4X-', [MyGUID.time_hi_and_version]) +
              Format('%.2X%.2X-', [MyGUID.clock_seq_hi_and_reserved, MyGUID.clock_seq_low]);
    for i := 0 to High(MyGUID.node) do Result += Format('%.2X', [MyGUID.node[i]]);
  end;
end;
// construction de la ligne d'entêtes d'une section pour export formats tabulaires
function MakeHeaderRubriquesOfAnRow(const Separateur: char; const Section: integer; const Rubriques: array of string): string;
var
  n, i: Integer;
begin
  n := High(Rubriques);
  if (n = 0) then Exit('');
  Result := Format('%d', [Section]) + Separateur;
  for i := 0 to n do
  begin
    Result += Rubriques[i] + Separateur;
  end;
end;

function CalcSHA1(const S: string): string;
begin
  Result := SHA1Print(SHA1String(S));
end;

// échappement des guillemets et autres. Même nom que la fonction php.
// OK pour les guillemets "
function mysqli_real_escape_string(const S: string): string;
begin
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
end;
// calcul de l'azimut moyen par la méthode du pseudo-cheminement
function CalcAzimutMoyen(const GradAz: double; const ArrAzimuts: array of double): double;
var
  i, n: Integer;
  Qx, Qy: double;
  TwoPi_Sur_GradAz: double;
  sa, ca, Theta, Grad_Az_Sur_4: double;
begin
  result := 0.00;
  n := length(ArrAzimuts);
  if (n = 0) then exit;
  TwoPi_Sur_GradAz := TWO_PI / GradAz;
  Grad_Az_Sur_4 := 0.25 * GradAz;
  Qx := 0.0; Qy := 0.0;
  for i := 0 to n - 1 do
  begin
    Theta := (Grad_Az_Sur_4 - ArrAzimuts[i]) * TwoPi_Sur_GradAz;
    sincos(Theta, sa, ca);
    Qx += ca;
    Qy += sa;
  end;
  Result := GetAzimut(Qx, Qy, GradAz);
end;
// azimut proche d'un azimut donné
// GradAz    : Nombre de graduation d'un tour complet: 360 = deg, 400 = grades, ou autre valeur
// Azimut    : Azimut à tester
// Azimutref : Azimut de référence
// Tolerance : Tolérance angulaire en unités d'angle

function IsNearToAzimut(const GradAz: double; const Azimut, AzimutRef: double; const ToleranceAngulaire: double): boolean;
var
  TwoPi_Sur_GradAz: double;
  Grad_Az_Sur_4   : double;
  sa , ca , Theta : double;
  sar, car, Thetar: double;
  RayonTolerance  , qdx, qdy: double;
  qdr: float;
begin
  result := false;
  TwoPi_Sur_GradAz := TWO_PI / GradAz;
  Grad_Az_Sur_4    := 0.25 * GradAz;
  Theta  := (Grad_Az_Sur_4 - Azimut)    * TwoPi_Sur_GradAz;
  Thetar := (Grad_Az_Sur_4 - AzimutRef) * TwoPi_Sur_GradAz;
  RayonTolerance := sin(ToleranceAngulaire * TwoPi_Sur_GradAz);
  sincos(Theta , sa , ca);
  sincos(Thetar, sar, car);
  qdx := sa - sar;
  qdy := ca - car;
  qdr := hypot(qdx, qdy);
  Result := (qdr < RayonTolerance);
end;
function IsNearToInclinaison(const GradInc: double; const Inclinaison, InclinaisonRef: double; const ToleranceAngulaire: double): boolean;
var
  TwoPi_Sur_GradInc: double;
  sa , ca , Phi : double;
  sar, car, Phir: double;
  RayonTolerance  , qdx, qdy: double;
  qdr: double;
begin
  result := false;
  TwoPi_Sur_GradInc := TWO_PI / GradInc;
  Phi  := Inclinaison    * TwoPi_Sur_GradInc;
  Phir := InclinaisonRef * TwoPi_Sur_GradInc;
  RayonTolerance := sin(ToleranceAngulaire * TwoPi_Sur_GradInc);
  sincos(Phi , sa , ca);
  sincos(Phir, sar, car);
  qdx := sa - sar;
  qdy := ca - car;
  qdr := hypot(qdx, qdy);
  Result := (qdr < RayonTolerance);
end;
function IsNearToLongueur(const Longueur, LongueurRef: double; const ToleranceLongueur: double): boolean;
begin
  Result := (Abs(Longueur - LongueurRef) <= ToleranceLongueur);
end;

//******************************************************************************
// Calcul de l'azimut moyen et test de quasi-colinéarité d'une série de mesures DX
// retourne VRAI si les mesures sont dans les tolérances
function CalcAzimutMoyenOfTMesuresViseeDistoX(const GradAz, GradInc: double;
                                              const ToleranceDistances: double;
                                              const ToleranceAngulaire: double;
                                              const ArrVisees: array of TMesureViseeDistoX;
                                              out   ViseeMoyenne: TMesureViseeDistoX): boolean;
var
  i, n: Integer;
  QArrProjXYZ: array of TPoint3Df;  // projections sur XYZ
  QViseeDX: TMesureViseeDistoX;
  QProjXYZ, QProjViseeMoyenne: TPoint3Df;
  ToleranceAngulaireMiseEnDistance: double;
  QR: Double;
  QX, QY, QZ: double;
  TwoPi_Sur_GradAz, TwoPi_Sur_GradInc, Grad_Az_Sur_4, Grad_Inc_Sur_4: double;
  WU1, WU2: String;
  QConditionLongueursOK, QConditionAngulaireOK: Boolean;
  QDL: ValReal;
  function CalcDXDYDZ(const QL, QAz, QInc: double): TPoint3Df;
  var
    QLP: double;
  begin
    QLP      := QL  * cos(QInc);
    Result.X := QLP * Sin(QAz);
    Result.Y := QLP * Cos(QAz);
    Result.Z := QL  * sin(QInc);
  end;
begin
  result := false;
  ViseeMoyenne.Longueur         := -1.00;        // Valeur d'erreur conventionnelle pour les longueurs
  ViseeMoyenne.Azimut           :=  0.00;
  ViseeMoyenne.Pente            :=  0.00;
  ViseeMoyenne.TimeStamp        := Now();
  n := Length(ArrVisees);
  AfficherMessageErreur(Format('CalcAzimutMoyenOfTMesuresViseeDistoX(%.2f, %.2f, %.2f, %.5f - n = %d)',
                              [GradAz, GradInc, ToleranceDistances, ToleranceAngulaire, n]));
  if (n = 0) then exit(false); // Tableau de mesures vide -> on sort
  if (n = 1) then // Une seule mesure -> ViséeMoyenne devient une visée radiante
  begin
    ViseeMoyenne := ArrVisees[0];
    Exit(false);
  end;
  // relister les mesures
  AfficherMessageErreur(Format('%d mesures', [n]));
  for i := 0 to n - 1 do AfficherMessageErreur(Format('%d: %.3f, %.3f, %.3f', [i, ArrVisees[i].Longueur, ArrVisees[i].Azimut, ArrVisees[i].Pente]));
  AfficherMessageErreur('================================');
  TwoPi_Sur_GradAz  := TWO_PI / GradAz;
  TwoPi_Sur_GradInc := TWO_PI / GradInc;
  Grad_Az_Sur_4     := 0.25 * GradAz;
  Grad_Inc_Sur_4    := 0.25 * GradInc;
  // calcul des projections des visées sur XYZ
  SetLength(QArrProjXYZ, n);
  // Calcul de la visée moyenne
  QX := 0.00;
  QY := 0.00;
  QZ := 0.00;
  for i := 0 to n - 1 do
  begin
    QArrProjXYZ[i] := CalcDXDYDZ(ArrVisees[i].Longueur, ArrVisees[i].Azimut * TwoPi_Sur_GradAz, ArrVisees[i].Pente * TwoPi_Sur_GradInc);
    QX += QArrProjXYZ[i].X;
    QY += QArrProjXYZ[i].Y;
    QZ += QArrProjXYZ[i].Z;
  end;
  // Longueur, azimut et pentes moyens
  QX := QX / n;
  QY := QY / n;
  QZ := QZ / n;
  GetBearingInc(QX, QY, QZ, ViseeMoyenne.Longueur, ViseeMoyenne.Azimut, ViseeMoyenne.Pente, GradAz, GradInc);
  // On projette la visée moyenne dans un repère local
  QProjViseeMoyenne := CalcDXDYDZ(ViseeMoyenne.Longueur, ViseeMoyenne.Azimut * TwoPi_Sur_GradAz, ViseeMoyenne.Pente * TwoPi_Sur_GradInc);
  AfficherMessageErreur(Format('Visée moyenne: %.2f; %.2f; %.2f - %.3f; %.3f; %.3f',
                             [ViseeMoyenne.Longueur, ViseeMoyenne.Azimut, ViseeMoyenne.Pente,
                              QProjViseeMoyenne.X,  QProjViseeMoyenne.Y,  QProjViseeMoyenne.Z]));
  // On transforme la tolérance angulaire en rayon de capture
  ToleranceAngulaireMiseEnDistance := ViseeMoyenne.Longueur * sin(ToleranceAngulaire * DEG_TO_RAD);
  // Parcours de la liste des projetées
  for i := 0 to n - 1 do
  begin
    // Test sur les longueurs
    QDL := abs(ArrVisees[i].Longueur - ViseeMoyenne.Longueur);
    QConditionLongueursOK := (QDL < ToleranceDistances);
    // La nouvelle origine est l'extrémité de la visée moyenne
    QProjXYZ.setFrom(QArrProjXYZ[i].X - QProjViseeMoyenne.X,
                     QArrProjXYZ[i].Y - QProjViseeMoyenne.Y,
                     QArrProjXYZ[i].Z - QProjViseeMoyenne.Z);
    QR  := Hypot3D(QProjXYZ.X, QProjXYZ.Y, QProjXYZ.Z);

    // Test de proximité des extrémités des visées avec celle de la visée moyenne
    QConditionAngulaireOK := (QR < ToleranceAngulaireMiseEnDistance);

    WU1 := IIF(QConditionLongueursOK, 'OK', 'NotOK');
    WU2 := IIF(QConditionAngulaireOK, 'OK', 'NotOK');

    AfficherMessageErreur(Format('%d: %.2f; %.2f; %.2f - dl = %.2f, r = %.3f, r0 = %.3f - Longueurs: %s, angles: %s', [i, QProjXYZ.X, QProjXYZ.Y, QProjXYZ.Z, QDL ,QR, ToleranceAngulaireMiseEnDistance, WU1, WU2]));
    Result := (QConditionLongueursOK and QConditionAngulaireOK);
  end;
end;

function MakeToporobotTabLineOfExpe(const IdxTABSection: integer; const Expe: TExpe): string;
begin
   Result := Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                    FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                    FORMAT_STRING+TAB+FORMAT_STRING+TAB+
                    FORMAT_NB_INTEGER+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                    FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+ // anciennement '%f'+TAB+FORMAT_NB_INTEGER+TAB+
                    FORMAT_STRING,
                           [IdxTABSection,
                            Expe.IDExpe,
                            Expe.JourExpe, Expe.MoisExpe, Expe.AnneeExpe, {$WARNING: TEXpe.DateExpe à implementer}
                            Expe.Operateur, Expe.ClubSpeleo,
                            Ord(Expe.ModeDecl),
                            Expe.DeclinaisonInDegrees,
                            0, Expe.IdxCouleur, //Inclinaison, Couleur,
                            Expe.Commentaire
                           ]);
end;

function MakeToporobotTabLineOfCode(const IdxTABSection: integer; const ModeSaveTab: TModeSaveTAB; const Code: TCode): string;
const
  LINE_CODE    = FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_STRING;
  FMT_PARAMS_CORR_FUNC = FORMAT_NB_REAL_6_DEC + TAB + FORMAT_NB_REAL_6_DEC + TAB + FORMAT_NB_REAL_6_DEC;
begin
  with Code do
  begin
    case ModeSaveTAB of
      mtabEXTENDEDTAB:
        //-1	1	360.00	360.00	0.05	1.00	1.00	1.00	100.00		5314011	0.000000	0.000000	0.000000	0.000000	0.000000	0.000000
        Result := Format(LINE_CODE + TAB + FORMAT_NB_INTEGER + TAB +
                     FMT_PARAMS_CORR_FUNC + TAB +
                     FMT_PARAMS_CORR_FUNC + TAB +
                     FORMAT_NB_REAL_3_DEC, //+ TAB +
                     // FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC
                       [IdxTABSection,
                        IDCode,
                        GradAz, GradInc,
                        PsiL, PsiAz, PsiP,
                        FactLong,
                        AngLimite,
                        Commentaire,
                        0, //ReservedInt,
                        ParamsFuncCorrAz.Co, ParamsFuncCorrAz.ErreurMax, ParamsFuncCorrAz.PosErrMax,
                        ParamsFuncCorrInc.Co, ParamsFuncCorrInc.ErreurMax, ParamsFuncCorrInc.PosErrMax,
                        ErreurTourillon //, DiametreBoule1, DiametreBoule2;
                       ]);
      mtabTOPOROBOT:
        Result := Format(LINE_CODE,
                       [IdxTABSection,
                        IDCode,
                        GradAz, GradInc,
                        0.10, 1.0, 1.0, //PsiL, PsiAz, PsiP,
                        100.00, 0.00, //FactLong, AngLimite,
                        SafeTruncateString(Commentaire, 50)
                       ]);
    end;
  end; //with Code
end;

// crée un nom de fichier avec la date indiquée, pour les fichiers de backup notamment
function MakeFilenameFromDate(const Prefix: string; const MyDate: TDateTime; const Extension: string): TStringDirectoryFilename;
var
  YYYY, MM, DD, HH, MN, SS, MS: word;
  EWE: String;
begin
  DecodeDate(MyDate, YYYY, MM, DD);
  DecodeTime(MyDate, HH, MN, SS, MS);
  EWE    := StringReplace(Extension, '.', '', [rfReplaceAll]);
  Result := Prefix +
            Format('%.4d-%.2d-%.2d_%.2dh%.2dmn%.2dsec%.3dms', [YYYY, MM, DD, HH, MN, SS, MS]) +
            '.' + EWE;
end;

// crée un nom valide pour une couche OSM
function CheckOSMLayerVarName(const QLayerNameSubmitted: string; out QLayerNameValidated: string): boolean;
var
  s: Integer;
begin
  result := false;
  QLayerNameValidated := '';
  if (QLayerNameSubmitted = '') then exit;
  for s := 1 to Length(QLayerNameSubmitted) do
  begin
    if (QLayerNameSubmitted[s] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_']) then QLayerNameValidated += QLayerNameSubmitted[s];
  end;
  Result := (QLayerNameValidated <> '');
end;
// fonctions trigo en degrés et grades
function sing(X: double): double;
begin
  Result := sin(X * GON_TO_RAD);
end;
function cosg(X: double): double;
begin
  Result := cos(X * GON_TO_RAD);
end;
function tang(X: double): double;
begin
  Result := tan(X * GON_TO_RAD);
end;
function sind(X: double): double;
begin
  Result := sin(X * DEG_TO_RAD);
end;
function cosd(X: double): double;
begin
  Result := cos(X * DEG_TO_RAD);
end;
function tand(X: double): double;
begin
  Result := tan(X * DEG_TO_RAD);
end;
// conversion en filtre et visse-versa
function ConvertirEnFiltreSeries(const S: string): string;
var
  LSR: TStringList;
  MySTR: String;
  EWE, S1, S2: LongInt;
  i: Integer;
  WU: TGHStringArray;
begin
  Result := '';
  LSR := TStringList.Create;
  try
    if (SplitToTStringList(Trim(S), ';', LSR, True, dupIgnore)) then
    begin
      for i := 0 to LSR.Count - 1 do
      begin
        MySTR := Trim(LSR.Strings[i]);
        EWE := StrToIntDef(MySTR, -1);
        if (EWE = -1) then
        begin
          // on recherche s'il y a une expression de la forme 666-999
          WU := Split(MySTR, '-');
          //for k := 0 to high(WU) do AfficherMessageErreur(Format('WU[%d] = "%s"', [k, WU[k]]));
          S1 := StrToIntDef(WU[0], 0);
          S2 := StrToIntDef(WU[1], 0);
          if ((S1 * S2) > 0) then  // S1 .ET. S2 non nuls ->  Intervalle trouvé
          begin
            if (S1 > S2) then Swap(S1, S2); // inversion éventuelle des bornes
            if (Abs(S2 - S1) > 0) then
              Result += Format('SERIE=(%d, %d)', [S1, S2])
            else // Bornes identiques => on coche l'élément S1
              Result += Format('SERIE=%d', [S1]);
          end;
        end
        else
        begin
          if (EWE > 0) then Result += Format('SERIE=%d', [EWE]);
        end;
        Result += ';';
      end;
    end;
  finally
    LSR.Clear;
    FreeAndNil(LSR);
  end;
end;

//******************************************************************************
// Distance de Jaro entre deux textes
// (= coefficient de concordance; permet de proposer des items)
function CalcDistanceJaroWinkler(const s1, s2: string): double;
var
  L, L1, L2: integer;
  i, k: integer;
  match_distance, matches, trans:integer;
  bs1, bs2: array of boolean; //used to avoid getmem, max string length is 255
begin
  L1 := length(s1);
  L2 := length(s2);
  if ((0 = L1) OR (0 = L2)) then Exit(-1.00);

  L  := max(L1, L2);

  SetLength(bs1, L); for i := 0 to L - 1 do bs1[i] := false;
  SetLength(bs2, L); for i := 0 to L - 1 do bs2[i] := false;

  match_distance := (max(L1, L2) div 2) - 1;
  matches := 0;
  trans := 0;
  for i := 1 to L1 do
  begin
    for k := max(1,i-match_distance) to min(i+match_distance,L2) do
    begin
      if bs2[k] then continue;
      if s1[i]<>s2[k] then continue;
      bs1[i]:=true;
      bs2[k]:=true;
      inc(matches);
      break;
    end;
  end;
  if matches=0 then exit(0);
  k:=1;
  for i := 1 to L1 do
  begin
    if (bs1[i]=false) then continue;
    while (bs2[k]=false) do inc(k);
    if s1[i]<>s2[k] then inc(trans);
    inc(k);
  end;
  trans := trans div 2;
  result:=((matches/L1)+(matches/L2)+((matches-trans)/matches))/3;
end;
// Distance de Damerau-Levenshtein
function CalcDistanceDamerauLevenshtein(const S1, S2: string): integer;
var
  // d est un tableau de longueurChaine1+1 rangées et longueurChaine2+1 colonnes
  // d est indexé à partir de 0, les chaînes à partir de 1
  D: array of array of integer;
  LengthS1, LengthS2, i, j: Integer;
  CoutSubst: integer;
  B1, B2, B3: Boolean;
  function qminimum(const A: array of integer): integer;
  var
    c: Integer;
  begin
    Result := MaxInt;
    for c := 0 to High(A) do
    begin
      if (A[c] < Result) then Result := A[c];
    end;
  end;
begin
  result := -1;
  LengthS1 := length(S1);
  LengthS2 := length(S2);
  if (0 = LengthS1 * LengthS2) then exit;
  if (S1 = S2) then Exit(0);
  SetLength(D, LengthS1 + 1, LengthS2 + 1); //  déclarer entier d[0..longueurChaine1, 0..longueurChaine2]
  for i := 0 to LengthS1 do D[i, 0] := i;
  for j := 0 to LengthS2 do D[0, j] := j;

  for i := 1 to LengthS1 do
  begin
    for j := 1 to LengthS2 do
    begin
      CoutSubst := IIF(S1[i] = S2[j], 0, 1);
      D[i, j] := qminimum([
                             d[i-1, j  ] + 1,                 // effacement du nouveau caractère de chaine1
                             d[i,   j-1] + 1,                 // insertion dans chaine1 du nouveau caractère de chaine2
                             d[i-1, j-1] + CoutSubst          // substitution
                          ]);
      //si(i > 1 et j > 1 et chaine1[i] = chaine2[j-1] et chaine1[i-1] = chaine2[j]) alors
      B1 := (i > 1) AND (j > 1); //
      B2 := S1[i]   = S2[j-1];
      B3 := S1[i-1] = S2[j];
      if (B1 AND B2 AND B3) then
      begin
         D[i, j] := qminimum([
                              d[i, j],
                              d[i-2, j-2] + CoutSubst// transposition
                             ]);
      end;
    end;
  end;
  Result := D[LengthS1, LengthS2];
end;

function GetNbCoresProcessor(): Cardinal;
{$IFDEF MSWINDOWS}
var
  lpSysInfo: TSystemInfo;
begin
  //lpSysInfo := nil;
  GetSystemInfo({%H-}lpSysInfo);
  Result := lpSysInfo.dwNumberOfProcessors;
end;
{$ELSE}
begin
  //Result := 1;
  result := sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ENDIF}


(*var
  MyCPU: TCPU;
begin
  Result := 1;
  MyCPU := TCPU.Create;
  try
    Result := MyCPU.Count;
  finally
    FreeAndNil(MyCPU);
  end;
end;
//*)




end.
//******************************************************************************
// Surcharge d'opérateurs

Operator + (V1: TPoint3Df; V2: TPoint3Df) Result : TPoint3Df;
begin
  R.X := V1.X + V2.X;
  R.Y := V1.Y + V2.Y;
  R.Z := V1.Z + V2.Z;
end;
Operator - (V1: TPoint3Df; V2: TPoint3Df) Result : TPoint3Df;
begin
  R.X := V1.X - V2.X;
  R.Y := V1.Y - V2.Y;
  R.Z := V1.Z - V2.Z;
end;
end.
