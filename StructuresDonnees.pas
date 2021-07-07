unit StructuresDonnees;
// Date: 19/04/2012
// Statut: Fonctionnel
// Les fichiers propriétaires de travail (e.g.: *.top) sont
// alignés sur 4 octets pour compatibilité WinCE

//{$mode delphi}{$H+}
//{$PACKRECORDS 4}
// 04/10/2013 : Conversions UTF8 <> ANSI fixed
// 14/04/2014: TElementsDrawn: Elément ajouté: Altitudes
// 12/02/2015: Ajout de deux attributs à TEntiteEtendue
// 22/04/2015: Suppression des champs d'index pour les entrées, réseaux, secteurs
// 20/07/2016: Les étiquettes de terrain et alias de stations topo sont gérés dans une table à part entière
// 29/11/2018: Type TStation supprimé -
// 13/06/2019: Point de contrôle temporel (contrôle de version)
// 17/05/2021: Mise à profit des pseudo-classes (des record qui contiennent des méthodes) de FPC 3.1
// 21/06/2021: Utilitaires de couleurs unifiés (TGHTopoColor)
{$INCLUDE CompilationParameters.inc}
interface
uses
  Classes
  , math
  , SysUtils
  , Graphics
  , FastGEO
  , BGRACanvas
  ;

{$IFDEF MULTI_THREADING}
CONST MULTI_OR_MONO_THREADED = 'Multithreaded';
{$ELSE MULTI_THREADING}
CONST MULTI_OR_MONO_THREADED = 'Monothreaded';
{$ENDIF MULTI_THREADING}

DEFAULT_PEN_WIDTH_IN_MM      = 0.025;
DEFAULT_FONT_HEIGHT_IN_UNITS = 14;
DEFAULT_FONT_HEIGHT_IN_MM    = 3.0; // mm

CONST FRM_CONSTRAINT_MAX_WIDTH  = 0;
CONST FRM_CONSTRAINT_MAX_HEIGHT = 0;
const MAX_WORD_VALUE            = 65535;

CONST
  CRIMINELS_CONTRE_L_HUMANITE = True;
  HITLER     = CRIMINELS_CONTRE_L_HUMANITE;  // sans commentaires
  JESUS      = HITLER;
  MAHOMET    = JESUS;
// formats de nombres
const
  FORMAT_STRING           = '%s';
  FORMAT_NB_INTEGER       = '%d';

  FORMAT_NB_REAL_0_DEC    = '%.0f';
  FORMAT_NB_REAL_2_DEC    = '%.2f';
  FORMAT_NB_REAL_3_DEC    = '%.3f';
  FORMAT_NB_REAL_6_DEC    = '%.6f';
// rayon maximal de capture pour
const MAX_DISTANCE_CAPTURE = 40.00;
// constantes angulaires
const TWO_PI: double = 2 * 3.1415926535897932384626;
// rayon de capture pour les stations proches
const RAYON_CAPTURE_NEAR_STATIONS = 25.00;
// unité angulaire par defaut
const
  DEGRES_PAR_TOUR          = 360.00;
  GRADES_PAR_TOUR          = 400.00;
  SATANGRADES_PAR_TOUR     = 666.00; // lol
  DEG_TO_RAD               = PI / 180;
  GON_TO_RAD               = PI / 200;
const
  QUADRANT_DEGRES = DEGRES_PAR_TOUR / 4.0;
  QUADRANT_GRADES = GRADES_PAR_TOUR / 4;
  ANGLE_JANE_BRIKIN_DEGRES = DEGRES_PAR_TOUR / 2;
  ANGLE_JANE_BRIKIN_GRADES = GRADES_PAR_TOUR / 2;



// A adapter en fonction de l'utilisateur
const UNITE_ANGULAIRE_PAR_DEFAUT  : double = 360.00;
const UNITE_ANGULAIRE_DU_CODE_ZERO: double = 360.00;
// année pivot
const ANNEE_PIVOT: word = 50; // année 1950 = pivot pour les années sur deux chiffres
// nombre max de séries par cavité
const NB_MAXI_SERIES_PAR_CAVITE  = 100000;
const MULTIPLICATEUR_STATION     = 10;

// dossiers de service
const MON_DOSSIER_CHIFFRES_TOPO  = '0_Mes_Donnees_Topo';
const MON_DOSSIER_CROQUIS        = '0_MesCroquis';
const MON_DOSSIER_QSAVES         = '0_Sauvegardes_Horodatees';
const MON_DOSSIER_EXPORT_GIS     = '0_Exports_GIS';

const MON_DOSSIER_RPI_DOCS       = '0_Docs_GHTopoRPI';
// fichier INI
CONST GHTOPO_STD_INI_FILENAME    = 'GHTopo_settings.ini';
CONST GHTOPO_RPI_INI_FILENAME    = 'GHTopoRPI_settings.ini';

// mot-clé pour indiquer un point d'intérêt
CONST KEYWORD_POI_TODO           = '$poi:';
CONST KEYWORD_POI_DONE           = '$done:';

const
  INI_CURRENT_GCS                = 'CurrentCoordinatesSystem';
  INI_CODE_EPSG                  = 'CodeEPSG';
  INI_NOM_EPSG                   = 'NomEPSG';

  INI_SETTINGS_WINDOWS_POSITIONS = 'WindowsPositions';
  INI_SECTION_USER_FOLDERS       = 'UserAdditionalFolders';
  INI_SECTION_RECENT_FILES       = 'RecentFiles';
  INI_SECTION_LAST_FILE_OPENED   = 'LastFile';
  INI_SECTION_FTP_SETTINGS       = 'RemoteFTP';
  INI_KEY_LAST_FILE_OPENED       = 'LastOpened';

  INI_FTP_HOSTNAME               = 'FTPHost';     DEFAULT_FTP_HOSTNAME = 'ftp4.phpnet.org';
  INI_FTP_PORT                   = 'FTPPort';     DEFAULT_FTP_PORT     = '21';
  INI_FTP_User                   = 'FTPUser';     DEFAULT_FTP_USER     = 'synclinal65';

  NB_MAX_RECENT_FILES            = 10;

// pour GHCaveDraw
type TIDBaseStation = type Int64;

// spécifique à certains utilisateurs
{$IFDEF GROS_MINET}
  const DFT_OPERATEUR    = 'CLEMENT Sylvestre';
  const DFT_CLUB_SPELEO  = 'SC Comminges';
  const DFT_REPORTER     = '';
{$ELSE}
  const DFT_OPERATEUR    = 'MyUser';
  const DFT_CLUB_SPELEO  = 'MyClub';
  const DFT_REPORTER     = '';
{$ENDIF}


// TGHTopoColor: Unifie tous les types de couleur et leurs fonctions
type TGHTopoColor = record
  Red     : byte;
  Green   : byte;
  Blue    : byte;
  Alpha   : byte;
  function  getFloatBlue(): double;  // retourne un réel entre 0 et 1
  function  getFloatGreen(): double;
  function  getFloatRed(): double;
  function  getFloatAlpha(): double;

  procedure setFrom(const R, G, B: byte; const A: byte = 255); overload;
  procedure setFrom(const C: TColor; const A: byte = 255); overload;
  function  toTColor(): TColor;
  procedure setFromStrGHTopoColor(const S: string; const qDefault: TColor);
  function  toStrGHTopoColor(): string;
  procedure setOpacity(const O: byte);
  function  toHTMLColor(): string;
  function  toKMLColor(): string;
  function  toSVGColor(): string;
  function  toGrayScale(): byte;

end;

// type de chaînes pour les noms de fichier
type TStringDirectoryFilename = type RawByteString; //type UnicodeString;
// types d'entiers pour les numéros (qui peuvent être différents de leurs index dans les tables)
type TNumeroEntrance = type Integer;
type TNumeroReseau   = type integer;
type TNumeroSecteur  = type integer;
type TNumeroSerie    = type Integer;
type TNumeroStation  = type Integer;
type TNumeroExpe     = type integer;
type TNumeroCode     = type integer;
type TIdxCouleur     = type Integer; // index sur palette de couleurs TOPOROBOT

type TAxeCoordonnees = (axisX, axisY, axisZ);
// fontes pour la vue en plan
const
  FONT_HEIGHT_COTATION   = 12;
  FONT_HEIGHT_IDSTATIONS = 12;
  FONT_HEIGHT_NOEUDS     = 14;
  FONT_HEIGHT_ENTRANCES  = 16;
  FONT_HEIGHT_QUADRILLES = 12;
const
  // 01/10/2013: Nouveaux instruments
  // code instruments pour les lasermètres-clinomètres de type bâtiment
  // Données recueillies: - Lp (longueur projetée de la visée) -> Longueur
  //                      - dZ (dénivellation de la visée)     -> Pente
  //                      - Angle                              -> Va dans les commentaires
  UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR = 800; // Stanley TLM330
  UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV = UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR - 10;
const INFINI: double = 1E30;
const // fichiers temporaires
  TEMP_COORDS_FILE = '_TmpCoords.xyz';
const
  // Codes EPSG des systèmes de coordonnées
  CODE_EPSG_WGS84                      = 4326;
  CODE_EPSG_LAMBERT_93                 = 2154;
  // anciens codes Lambert
  CODE_EPSG_LAMBERT_I_NTF_DEPRECATED   = 27561; //  'NTF (Paris) / Lambert North France' Obsolete
  CODE_EPSG_LAMBERT_II_NTF_DEPRECATED  = 27562; //  'NTF (Paris) / Lambert Centre France'
  CODE_EPSG_LAMBERT_III_NTF_DEPRECATED = 27563; //  'NTF (Paris) / Lambert South France'
  CODE_EPSG_LAMBERT_IV_NTF_DEPRECATED  = 27564; //  'NTF (Paris) / Lambert Corsica'
  // codes actuels
  CODE_EPSG_LAMBERT_I                  = 27571;
  CODE_EPSG_LAMBERT_II                 = 27572;
  CODE_EPSG_LAMBERT_III                = 27573;
  CODE_EPSG_LAMBERT_IV                 = 27574;
  // lambert 9 zones
  CODE_EPSG_CC_ZONE_0                  = 3940;
  // codes UTM
  CODE_EPSG_UTM_ZONE_0_NORTH           = 32600;
  CODE_EPSG_UTM_ZONE_0_SOUTH           = 32700;
  CODE_EPSG_GOOGLE                     = 379009;
  {$IFDEF GROS_MINET}
  DEFAULT_SYSTEME_COORDONNEES_NOM       = 'Lambert III'; // évolutif en LT93
  DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG = CODE_EPSG_LAMBERT_III; // évolutif en LT93 (2154)
  {$ELSE}
  {$WARNING: Repasser en LT93 par défaut à la prochaine LTS}
  DEFAULT_SYSTEME_COORDONNEES_NOM       = 'Lambert III'; //'Lambert 93'; // évolutif en LT93
  DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG = CODE_EPSG_LAMBERT_III; // CODE_EPSG_LAMBERT_93
  {$ENDIF GROS_MINET}
const

  SEUIL_LONGUEUR_NULLE                  = 0.001;
  SEUIL_LONGUEUR_MAXI_TOPOROBOT         = 320.00;
  SEUIL_LRUD_WARN_LENGTH                = 60.00;
  TAB                      = #9;
  SEPARATOR_TAB            : char = #9;
  MAX_LINES_LOG            = 1000; // nb max de lignes de l'historique des opérations
  MAX_LINES_ERRORS_LOG     = 2000; // nb max de lignes de l'historique des opérations


  DEFAULT_FONT_NAME        = 'Arial';
  NAME_FOR_DEFAULT_FONT    = 'font_default';

// couleur du réseau par défaut:
const
  COULEUR_RESEAU_0             = $000080FF;   // orange
  COULEUR_VISEE_ANTENNE        = clGray;      // couleur des visées en antenne

// export GHCaveDraw
const
  FMT_BASEPOINTS = '    %d'+TAB + FORMAT_STRING + // ID de la visée
        TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER + // attributs
        TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC +  // point de départ
        TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC +  // point d'arrivée
        TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC +  // point droit  (avec Z du sol)
        TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC;   // point gauche (avec Z du plafond)
//----------------------------------
// format standard TOPOROBOT Série + Point
const FMTSERST       = '%d.%d';   // notation standard GHTopo 123.456
const FMT_NDSER_PT   = '%d-%d';   // pour la liste des jonctions
const FMTSERST_VTOPO = '%d_%d';   // pour Visual Topo
const FMTSERSTID    = FMTSERST + ': %s';
///*****************************************************************************
// seuil au-delà duquel il faut tracer les réseaux en mode rapide
const LONGUEUR_LIMITE_RESEAU = 160 * 1000;


// mode ajout, modif,
type TModeAjoutOuModif = (mamAJOUT, mamMODIFICATION);

// tableau de valeurs réelles
type TArrayOfFloats   = array of double;
type TArrayOfIntegers = array of Integer;

//***************************************************************************
// Espaces de noms
const NAMESPACE_NAME_BY_DEFAULT = 'Namespace00';
const NAMESPACE_DESC_BY_DEFAULT = 'Espace de noms par défaut';
const NAMESPACE_COLOR_BY_DEFAULT: TColor = clred; //TGHTopoColor = TGHTopoColor.setFrom(clRed);


type TIdxNamespace = type Integer;
type

{ TNameSpace }

 TNameSpace = record
  Couleur     : TGHTopoColor;
  Nom         : string;
  Description : string;
  procedure setFrom(const QCouleur: TGHTopoColor; const QNom, QDescription: string);
end;
type TGISLayer = record
  SymboleStyle       : integer; // 0: rien, 1: Cercle, 2: Carré, 3: Etoile, etc ...
  SymbolSize         : double;
  SymbolColor        : TGHTopoColor;
  LayerVarName       : string;
  LayerTitle         : string;
  LayerDescription   : string;
  LayerAttribution   : string;
  procedure setFrom(const QSymbolStyle  :  integer; // 0: rien, 1: Cercle, 2: Carré, 3: Etoile, etc ...
                    const QSymbolSize    : double;
                    const QSymbolColor   : TGHTopoColor;
                    const QLayerVarName, QLayerTitle, QLayerDescription, QLayerAttribution: string);
end;


// pour support du DistoX
type TDistoX2PacketOfBytes   = array[0..3] of byte;
type TDistoX2MemoryMap       = array[0..65535] of byte;
type TErrorConstruireUneViseeValide = set of (errCVV_NOT_ENOUGHT_NB_SHOTS, errCVV_OUTBOUNDS_LONGUEUR, errCVV_OUTBOUNDS_AZIMUT, errCVV_OUTBOUNDS_PENTE, errCVV_OTHER);

// pour le parcours des graphes
type TModeExplorerGraphe = (mpgEN_LARGEUR, mpgEN_PROFONDEUR);
// pour la recherche de stations topo
type TStationMatchFound = record
  Serie        : TNumeroSerie;
  Station      : TNumeroStation;
  Match        : string;
  procedure setFrom(const QSerie: TNumeroSerie; const QStation: TNumeroStation; const QMatch: string);
end;
type TArrayStationsMatchFound = array of TStationMatchFound;
// pour export VRML, X3D
type TModeExportFormat3D = (me3dX3D, me3dVRML, me3dDXF);

// pour l'export ODG:

// type de projection (systeme de coordonnées)
type TCodeEPSG = type Integer;
type TLabelSystemesCoordsEPSG = record
  CodeEPSG: TCodeEPSG;
  NomEPSG : string;
end;
// type de projection (systeme de coordonnées)


// type de points pour Proj4s
type TProjUV = Record
  U: Double;
  V: Double;
End;
type TArrayOfTProjUV = array of TProjUV;
type TProjXY = TProjUV;

type TTypeDeVisee = (tgDEFAULT,         // X défaut = galerie fossile
                     tgENTRANCE,
                     tgFOSSILE,
                     tgVADOSE,
                     tgENNOYABLE,
                     tgSIPHON,
                     tgFIXPOINT,
                     tgSURFACE,
                     tgTUNNEL,
                     tgMINE,
                     tgVISEE_RADIANTE);
type TModeTOptionsCenterLine = set of (mclCENTERLINE, mclCROSS_SECTIONS, mclSTATIONS, mclLBL_STATIONS);
// vecteurs
type TVecteurDouble = array of double;
// tableaux de strings
const MAX_SIZE_PARAM_ARRAY         =  63;
const MAX_NB_LIGNES_TSTRINGARRAY2D = 511;
// TStringArray est un nouveau type de données de FPC 3.0.2
type TGHStringArray   = array[0 ..MAX_SIZE_PARAM_ARRAY] of string;
// tableau 2D utilisé par le frontal de saisie des points topo
type TGHStringArray2D = array[0 .. MAX_NB_LIGNES_TSTRINGARRAY2D] of TGHStringArray;

type TErrorVisee = set of (errvSECTEUR, errvTYPEVISEE,
                           errvCODE, errvEXPE, errvLONGUEUR_NULLE,
                           errvLONGUEUR_OVERFLOW, errvAZIMUT, errvPENTE,
                           errvLG, errvLD, errvHZ, errvHN,
                           errWarnLG, errWarnLD, errWarnHZ, errWarnHN);
// pour la sélection du mode vue 3D: GDI ou OpenGL
type TTypeVue3D = (tv3dGDI, tv3dOPENGL);
// pour la sélection dans les dialogues listes
type TModeSelectionListe = (mslENTRANCES, mslRESEAUX, mslSECTEURS, mslCODE, mslEXPE, mslSERIE, mslTYPE_GALERIE, mslDATE,  // pour la BDD
                            mslNAMESPACES);

// graphiques 2D: éléments dessinés
type TElementDrawn     = (edPolygonals, edStations, edIDStations, edAltitudes, edCotes,
                          edWalls, edCrossSections, edFillGalerie,
                          edQuadrilles, edReferentiel, edBounds, edVolumes, edMaillages,
                          edENTRANCE_MKS, edENTRANCE_NAMES,
                          edCROQUIS, edANTENNES,
                          edJONCTIONS, edPOI);
type TSetElementsDrawn = set of TElementDrawn;
// mode de fonctionnement de TGHTopoContext2DA
type TModeFonctionnementGHTopoContext2D = (mfgcSURVEYING, mfgcNAVIGATION);
// mode de représentation: par séances, réseaux, gris
type TModeRepresentationGaleries = (rgENTRANCES, rgRESEAUX, rgSECTEURS, rgSEANCES, rgGRAY, rgDEPTH, rgTYPES_VISEES);
type TBDDEntitesFindViseeOrEntrance =  (tpVISEES, tpENTRANCES);
type TBDDEntitesFindTablesAParcourir = set of TBDDEntitesFindViseeOrEntrance;
// attributs de texte
type

{ TTexteAttributs }

 TTexteAttributs = record
  StyleName    : string;
  FontName     : string;
  FontColor    : TColor;
  BackColor    : TColor;
  FontStyle    : TFontStyles;
  HauteurTexte : double;
  Alignement   : byte;  // position du texte; cf organisation d'un clavier numérique pour le point d'accrochage
  AngleRot     : integer;
  procedure setFrom(const QStyleName, QFontName: string;
                    const QFontColor, QBackColor : TColor;
                    const QFontStyle: TFontStyles;
                    const QHauteurTexte: double;
                    const QAlignement: byte;  // position du texte; cf organisation d'un clavier numérique pour le point d'accrochage
                    const QAngleRot: integer);
  procedure setDefault();
end;



type TMatrix2x2       = array[1..2, 1..2] of double;
type TMatrix3x3       = array[1..3, 1..3] of double;
type TMatrix          = array of array of double;
type TIncidenceMatrix = array of array of ShortInt;  // seules valeurs: -1, 0, +1

type

{ TPoint3Df }

 TPoint3Df = record
  X:  double;
  Y:  double;
  Z:  double;
  procedure setFrom(const QX, QY, QZ: double);
  procedure Empty();
  function  getNorme(): double;
  function  getProjHZ(): double;
  function getAzimut(const Unite: double): double;
  function getInclinaison(const Unite: double): double;
end;
type

{ TPoint2Df }

 TPoint2Df = record
  X:  double;
  Y:  double;
  procedure setFrom(const QX, QY: double);
  function  getNorme(): double;
  procedure Empty();
end;
(*
type TPoint2DfTagged = record
  X: double;
  Y: double;
  Tag: integer;
end;
//*)
// vertex avec un attribut de tri
// (typiquement utilisé pour la construction de profils topo)
type

{ TPoint3DfOrderedByP }

 TPoint3DfOrderedByP = record
  X:  double;
  Y:  double;
  Z:  double;
  P:  double;
  procedure setFrom(const QX, QY, QZ, QP: double);
end;

// type rectangle
type   TRect2Df = record
  X1: double;
  Y1: double;
  X2: double;
  Y2: double;
  procedure setFrom(const QX1, QY1, QX2, QY2: double);
  procedure ResetBoundingBox();
  procedure UpdateBoundingBox(const QX, QY: double); overload;
  procedure UpdateBoundingBox(const PT: TPoint2Df); overload;
  procedure UpdateBoundingBox(const PT: TPoint3Df); overload;
  function  ContainsPoint(const QX, QY: double): boolean; overload;
  function  ContainsPoint(const P: TPoint2Df): boolean; overload;
  function  ContainsPoint(const P: TPoint3Df): boolean; overload;
  function  ContainsSegment(const P1, P2: TPoint2Df; const PartiallyInRectangle: boolean = true): boolean; overload;
  function  ContainsSegment(const X1, Y1, X2, Y2: double; const PartiallyInRectangle: boolean): boolean; overload;
  function  ContainsQuad(const P1, P2, P3, P4: TPoint2Df): boolean;
end;


type TArrayPoints2Df = array of TPoint2Df;
type TArrayPoints3Df = array of TPoint3Df;



// couple de points de sections transversale
// contenant les coordonnées X, Y des points extremes d'une section transversale
type TPtsSectionTransversale = record
  PosStation  : TPoint3Df;
  ParoiGaucheX: double;
  ParoiGaucheY: double;
  ParoiDroiteX: double;
  ParoiDroiteY: double;
end;

// format de sauvegarde:
// mtabEXTENDEDTAB =  format étendu XTB, natif de GHTopo
// mtabTOPOROBOT   =  pour compat(err)ibilité TOPOROBOT
type TModeSaveTAB =(mtabEXTENDEDTAB, mtabTOPOROBOT);

// gestion des fins de lignes
// des standards PC, UNIX, Macintache
type TTextFileFormat=(tfWINDOWS, tfUNIX, tfMAC);
//***************************************************************************
//****************************
// Types de données TOPOROBOT
//****************************
// entrées
type

{ TEntrance }

 TEntrance    = record
  eNomEntree: string;
  eIDTerrain: string;
  ePosition : TPoint3Df;
  eRefSer   : TNumeroSerie;
  eRefSt    : TNumeroStation;
  eCouleur  : TGHTopoColor;
  eObserv   : string;
  procedure setFrom(const NomEntree, IDTerrain: string; const QPosition: TPoint3Df; const RefSer: TNumeroSerie; const RefSt: TNumeroStation; const Couleur: TGHTopoColor; const Observ: string);
  function  toLineXTBSection6(const QIdx: integer): string;
  function  toLineXTBSection5(const QIdx: integer): string;


end;

// réseau ou secteur spéléologique
// Section -8 du fichier xtb
// Une série ne peut faire partie de plusieurs réseaux.

type

{ TReseau }

 TReseau = record
  ColorReseau  : TGHTopoColor;
  TypeReseau   : integer; // type de réseau:
  NomReseau    : string;
  ObsReseau    : string;
  procedure setFrom(const QC: TGHTopoColor; const QT: integer; const QNom, QObs: string);
  function  toLineXTB(const QIdx: integer): string;
end;

// secteurs
type

{ TSecteur }

 TSecteur = record
  CouleurSecteur : TGHTopoColor;
  NomSecteur     : string;
  procedure setFrom(const C: TGHTopoColor; const Nom: string);
  function  toLineXTB(const QIdx: integer): string;
end;
// expés
type TModeCalculDeclimag = (cdmAUTOMATIQUE, cdmMANUEL);
type

{ TExpe }

 TExpe = record
  IDExpe      : TNumeroExpe;
  {$WARNING: TEXpe.DateExpe à implementer}
  JourExpe    : word; {$WARNING: Remplacer ces trois champs par un TDateTime}
  MoisExpe    : word;
  AnneeExpe   : word;
  //DateExpe    : TDateTime;
  Operateur   : String;
  ClubSpeleo  : string;
  ModeDecl    : TModeCalculDeclimag;
  DeclinaisonInDegrees : double;
  IdxCouleur  : TIdxCouleur;
  Commentaire : string;
  procedure setFrom(const QNumeroExpe: TNumeroExpe;
                    const YYYY, MM, DD: word;
                    const QIdxCouleur: TIdxCouleur;
                    const QModeDecl   : TModeCalculDeclimag;
                    const QDeclinaisonInDegrees: double;
                    const QSpeleometre, QSpeleographe: string;
                    const QCommentaires: string);
  function getLibelle(): string;
  function getDateStr(): string;
  function getTDateTime(): TDateTime;
  function toLineXTB(): string;

end;
// fonctions trigonométriques de correction angulaire
type

{ TParamFoncCorrectionAngulaire }

 TParamFoncCorrectionAngulaire = record
  Co        : double; // constante d'erreur systématique (e.g: cercle d'un compas bien centré mais avec le zéro non positionné sur le barreau aimanté)
  ErreurMax : double; // erreur maximale
  PosErrMax : double; // Angle (azimut ou pente) correspondant à l'erreur maximale
  procedure Empty();
  procedure setFrom(const QCo, QErrMax, QPosErrMax: double);
end;

// codes
type

{ TCode }

 TCode = record
    IDCode           : TNumeroCode;
    GradAz           : double;
    GradInc          : double;
    PsiL             : double;
    PsiAz            : double;
    PsiP             : double;
    FactLong         : double;
    AngLimite        : double;
    ErreurTourillon  : double;  // NOUVEAU 09/2018: Support de l'erreur de tourillon (typiquement lors du travail aux cônes)
    DiametreBoule1   : double;  // NOUVEAU 09/2019: Support du travail aux boules-cibles, en mettant une boule sur un support
    DiametreBoule2   : double;  // (typiquement une boule sur un cône)
    Commentaire      : string;
    // Fonctions de correction
    ParamsFuncCorrAz : TParamFoncCorrectionAngulaire;
    ParamsFuncCorrInc: TParamFoncCorrectionAngulaire;
    function  getLibelle(): string;
    procedure setUsualParameters(const NumeroCode: TNumeroCode;
                                 const GradAz, GradInc, FactLongueur: double;
                                 const PsiL, PsiAz, PsiP: double;
                                 const DiametreBoule1, DiametreBoule2, ErreurTourillon: double;
                                 const Commentaires: string);
    function ExplainCodeAzimut(): string;
    function ExplainCodePente(): string;
    function toLineXTB(const ModeSaveTab: TModeSaveTAB): string;

end;
// sens de dessin des séries en coupe développée
type TSensTraceCoupeDev = (stcdVERS_DROITE, stcdVERS_GAUCHE);
// visées
type  TUneVisee = record
    NoVisee   : integer; // Indispensable pour les branches et les antennes             //
    TypeVisee : TTypeDeVisee;                                                           //   TypeVisee           : TTypeDeVisee;
    IDSecteur : TNumeroSecteur;                                                         //   stSecteur           : TNumeroSecteur;
    Code      : TNumeroCode;                                                            //   stCode              : TNumeroCode;
    Expe      : TNumeroExpe;                                                            //   stExpe              : TNumeroExpe;
    Longueur  : double;                                                                 //   Longueur            : double;
    Azimut    : double;                                                                 //   Azimut              : double;
    Pente     : double;                                                                 //   Pente               : double;
    LD        : double;                                                                 //   LD                  : double;
    LG        : double;                                                                 //   LG                  : double;
    HZ        : double;                                                                 //   HZ                  : double;
    HN        : double;                                                                 //   HN                  : double;
    // Données supplémentaires enregistrées par le nouvel appareil BRIC4 https://www.caveexploration.org/gear/bric4
    Horodatage : TDateTime;
    Temperature: double;
    Humidity   : double;
    IDTerrainStation: string;                                                           //   IDTerrainStation    : string;
    Commentaires    : string;                                                           //   Commentaire         : string;
    AccroissXYZ     : TPoint3Df;                                                        //
    AccroissP       : double;                                                           //
    procedure setFrom(const QNoVisee          : integer;
                      const QTypeVisee        : TTypeDeVisee;
                      const QIdSecteur        : TNumeroSecteur;
                      const QCode             : TNumeroCode;
                      const QExpe             : TNumeroExpe;
                      const QL, QAz, QP       : double;
                      const QLG, QLD, QHZ, QHN: double;
                      const QHorodatage       : TDateTime;
                      const QIDStation        : string = '';
                      const QCommentaire      : string = '';
                      const QTemperature      : double = 0.00;
                      const QHumidity         : double = 0.00);
    procedure Empty(const Commentaire: string = '');
    function  toLineXTB(const ModeSaveTab: TModeSaveTAB; const QNumeroDeSerie: TNumeroSerie; const QNumeroStation: TNumeroStation): string;
    procedure setLongAzInc(const L, A, P: double); overload;
    procedure setLongAzInc(const L, A, P: string); overload;
    procedure setLRUD(const L, R, U, D: double); overload;
    procedure setLRUD(const L, R, U, D: string); overload;
    function  DebugString(): string;
end;
// visées en antenne
// Nota: Les codes et expés d'une visée radiante héritent de ceux de la station d'accrochage
type

{ TViseeAntenne }

 TViseeAntenne = record
   EntranceRatt        : TNumeroEntrance;
   Reseau              : TNumeroReseau;
   Secteur             : TNumeroSecteur;
   SerieDepart         : TNumeroSerie;
   PtDepart            : integer;
   Longueur            : double;      //         'Longueur
   Azimut              : double;      //         'Azimut
   Pente               : double;      //         'Pente
   MarkedForDelete     : boolean;
   procedure setLongAzInc(const L, A, P: double); overload;
   procedure setLongAzInc(const L, A, P: string); overload;
   procedure setFrom(const QEntranceRatt    : TNumeroEntrance;
                     const QReseau          : TNumeroReseau;
                     const QSecteur         : TNumeroSecteur;
                     const QSerieDepart     : TNumeroSerie;
                     const QPtDepart        : integer;
                     const QL, QAz, QP      : double;
                     const Marked           : boolean = false);
   function toLineXTB(const QIdx: integer): string;
   function DebugString(): string;

end;
type

{ TViseeAntenneFound }

 TViseeAntenneFound = record
  Idx      : Int64;
  VA       : TViseeAntenne;
  Position : TPoint3Df;
  dp       : double;
  AzimutAbsolu: double;
  procedure setFrom(const QIdx: Int64; const QVA: TViseeAntenne; const QPosition : TPoint3Df; const Qdp : double; const QAzimutAbsolu: double);

end;

type TArrayOfTViseeAntenne = array of TViseeAntenneFound;


// couple série station au format TOPOROBOT + ID de terrain
type TToporobotIDStation = record
  eIdxNameSpace: integer;
  aSerie: TNumeroSerie;
  aStation: integer;
  aIDTerrain: string;
  procedure setFrom(const QNameSpace: Integer; const QSerie: TNumeroSerie; const QStation: integer; const QIDTerrain: string = ''); overload;
  procedure setFrom(const S: string); overload;
  function  ToString(): string;
end;
type TArrayOfTToporobotIDStation = array of TToporobotIDStation;

// Station proche de l'extrémité d'une série
type TStationProcheOfEndSerie = record
  BaseStation       : TToporobotIDStation;
  NearestStation    : TToporobotIDStation;
  LongueurSerie     : double;
  Distance          : double;
  ObservLastStation : string;
  Statut            : string;
end;
type TSerieEncadree = record
  NumeroSerie    : TNumeroSerie;
  NomSerie       : string;
  StationDebut   : TToporobotIDStation;
  StationFin     : TToporobotIDStation;
end;

// structure pour croquis et export GHCavedraw
type

{ TBaseStation }

 TBaseStation = record
   // serie et point
   IDTerrain         : string;
   Entite_Serie      : TNumeroSerie;            // Série
   Entite_Station    : TNumeroStation;          // Station
   Enabled           : boolean;     // activé par le MétaFiltre
   IsPOI             : boolean;  // l'entité est un point d'intérêt (POI) ?
   // secteurs, type de visée, réseaux, codes, expés
   Type_Entite       : TTypeDeVisee;  // byte dans GHCaveDraw
   // valeur calculée: Couleur en fonction de la profondeur
   // stockée ici en raison du grand nombre de calculs pour le dégradé
   CouleurStd        : TColor;
   CouleurDegrade    : TColor;

   eEntrance         : integer;
   eSecteur          : integer;
   eReseau           : integer;
   eCode             : TNumeroCode;
   eExpe             : TNumeroExpe;
   DateLeve          : TDateTime;
   // valeurs initiales: Long, Az, P
   oLongueur         : double;
   oAzimut           : double;
   oPente            : double;
   oLG               : double;
   oLD               : double;
   oHZ               : double;
   oHN               : double;
   PosExtr0          : TPoint3Df;    // valeurs calculées: centerline
   PosStation        : TPoint3Df;
   PosOPG            : TPoint3Df;   // valeurs calculées: silhouette
   PosOPD            : TPoint3Df;
   PosPG             : TPoint3Df;
   PosPD             : TPoint3Df;
   // champs pour le même usage que l'attribut Tag des TForm.
   TagInteger           : Int64;
   TagDouble            : double;
   // Mise en évidence de certains éléments (hors MétaFiltre)
   Highlighted          : boolean;
   // champs texte => en fin de ligne
   oCommentaires        : string;
   procedure Empty();
   function toString()  : string;
   function toStringWithIDTerrain(): string;

   function IsInNaturalCave(): boolean;
   function IsInCaveOrTunnel(): boolean;
   function IsInSerie(): boolean;
   function getGHCaveDrawIDPtCenterline(): int64;
   function getGHCaveDrawIDPtAntenne(const NbViseesEnAntenne: integer): int64;
   function getLibelleStationTopo(): string;
   function DebugString(): string;
   function toTextForQRCode(): string;
   procedure setLongAzInc(const L, A, P: double); overload;
   procedure setLRUD(const L, R, U, D: double);
end;
//*)


// angle
type TUniteAngles = (uaRADIANS, uaDEGRES, uaGRADES);

// modes d'exportation pour logiciels de carto
// gisNONE: le FilterIndex du TOpenDialog démarre à 1 et non 0
type TGISOutputMode     = set of (gisCENTERLINES, gisSILHOUETTES);
type TGISColorByItems   = (gisCOLOR_UNIQUE, gisCOLOR_ENTRANCES, gisCOLOR_RESEAUX);

const // Ne pas définir de type énuméré afin d'éviter une dépendance avec GHTopo
  OSM_MARKER_STYLE_DEFAULT = 0;
  OSM_MARKER_STYLE_CIRCLE  = 1;
  OSM_MARKER_STYLE_SQUARE  = 2;
  OSM_MARKER_STYLE_DELTA   = 3;
  OSM_MARKER_STYLE_NABLA   = 4;
  OSM_MARKER_STYLE_LOSANGE = 5;
type TFormatExportGIS = (gisOSM, gisKML, gisGeoJSON, gisDXF);



type TPolyMode = (tpmENTETE_POLY, tpmSTART_POLY, tpmPOINT_POLY, tpmEND_POLY);
// marqueurs sur la carte
type

{ TGHTopoMarker }

 TGHTopoMarker = record
  Displayed: boolean;
  X        : double;
  Y        : double;
  Couleur  : TGHTopoColor;
  Caption  : string;
  procedure setFrom(const QD: boolean; const QX, QY: double; const QC: TGHTopoColor; const QS: string); overload;
  procedure setFrom(const QD: boolean; const QX, QY: double; const QC: TColor; const QS: string); overload;

end;




// Couleurs Macintosh
type TMacintoshColor = record
  R : word;
  G : word;
  B : word;
end;


// pour stats spéléometriques
type TVentilationSpeleometrie = record
  Fossiles  : double;
  Vadoses   : double;
  Ennoyables: double;
  Siphons   : double;
  Tunnels   : double;
  Filons    : double;
  Speciaux  : double;
end;
type TTableauVentilationSpeleometrie = array of TVentilationSpeleometrie;

// spéléométrie simplifiée: Dév, XYZ Maxi et mini, Nb de visées
type TSpeleometrieReseauOuSecteur = record
  DonneesValides   : boolean;
  IDReseauSecteur  : integer;
  NomReseauSecteur : string;
  Developpement    : double;
  CoordMini        : TPoint3Df;
  CoordMaxi        : TPoint3Df;
  Etendue          : TPoint3Df;
  NbVisees         : integer;
end;
type TArraySpeleometrieReseauOuSecteur = array of TSpeleometrieReseauOuSecteur;

type TProcWithFilenameParameter = procedure (const Msg: TStringDirectoryFilename);
// procédure d'affichage de la progression d'une opération
type TProcDisplayProgression              = procedure (const Etape: string; const Done, Starting, Ending, Step: integer) of object;
//type TProcOfObject = procedure of object;
type TProcOfObjectWithOneBoolParameter    = procedure(const P: boolean) of object;
type TProcOfObjectWithOneIntParameter     = procedure(const P: integer) of object;
type TThreadProcOfObjectWithOneIntParameter = procedure(const NoThread, Idx: integer) of object;

type TProcOfObjectWithTNumeroSerie        = procedure(const N: TNumeroSerie) of object;
type TProcOfObjectReturnsAsBoolean        = function(): boolean of object;
type TProcOfObjectWithOneStringParameter  = procedure (const Msg: string) of object;
type TProcOfObjectWithFilenameParameter   = procedure (const Msg: TStringDirectoryFilename) of object;
type TProcOfObjectWithAShot               = procedure (out Horodate: TDateTime; out QLongueur, QAzimut, QPente: double) of object;
// pour éviter un nombre inconsidéré de callbacks utilisant un TBaseStation, on définit une liste d'actions possibles
// enum TTodoAction, enrichissable à volonté
type TTodoAction                          = (tdoAcnBASESTATION_DISPLAY, tdoAcnBASESTATION_EDITSERIE);
type TProcOfObjectWithATBaseStation       = procedure (const BP: TBaseStation; const TodoAction: TTodoAction) of object;

// onglets pour les vues 2D
const NB_MAX_ONGLETS = 10;
type  TModesTravail  = (mtREADY,
                        mtPAN_PREMIER_POINT, mtPAN_SECOND_POINT,
                        mtZOOM_PREMIER_COIN, mtZOOM_SECOND_COIN,
                        mtMETAFILTRE_ZONE_PREMIER_COIN, mtMETAFILTRE_ZONE_SECOND_COIN,
                        mtDISTANCE_PREMIER_POINT, mtDISTANCE_SECOND_POINT,
                        mtADD_SERIE_PREMIER_POINT, mtADD_SERIE_SECOND_POINT,
                        mtBOUCLER_PREMIER_POINT, mtBOUCLER_SECOND_POINT,
                        mtPROFIL_MNT_PREMIER_POINT, mtPROFIL_MNT_SECOND_POINT,
                        mtNEW_ANNOTATION, mtSELECT_ANNOTATION, mtDELETE_ANNOTATION,
                        mtNEW_POLYLINE, mtADD_VERTEX_POLYLINE, mtDELETE_POLYLINE,
                        mtNEW_LASSO_SELECTION
                        );
type  TQdrType       = (qtNONE, qtGRID, qtCROSS, qtPOINTS);

type TVue2DParams  = record
  ongName: string;
  ongC1                 : TPoint2Df;
  ongC2                 : TPoint2Df;
  ongVueFiltres         : string;
  ongBackGround         : TColor;
  ongQdrSpc             : double;
  ongQdrType            : TQdrType;
  ongDrawFastCenterline : boolean;
  ongQdrColor           : TColor;
  ongElementsDrawn      : TSetElementsDrawn;

  ongDegradeAltMiniReseau: TColor;
  ongDegradeAltMaxiReseau: TColor;

  ongDegradeAltMiniMNT  : TColor;
  ongDegradeAltMaxiMNT  : TColor;

  ongViseesLargeurInPX  : integer;
  ongViseesLargeurInMM  : double;

  ongModeRepresentation : TModeRepresentationGaleries;

  ongFillOpacite         : byte;  // 00..255
  ongTailleTexteIDStation: double;
  ongTailleTexteAltitudes: double;
  ongTailleTexteCotation : double;
  ongTailleTexteEntrances: double;
  ongTailleTexteNoeuds   : double;
  ongTailleTextePOIs     : double;

  ongCouleurIDStation    : TColor;
  ongCouleurAltitudes    : TColor;
  ongCouleurCotation     : TColor;
  ongCouleurEntrances    : TColor;
  ongCouleurNoeuds       : TColor;
  ongCouleurPOIs         : TColor;
  ongDoDispViseesNonRetenues : boolean;
  ongCouleurViseesNonRetenues: TColor; // couleur des visées non retenues par le MétaFiltre
end;
// Maillages
type TMNTNumeroTriangle     = type Int64;
type TMNTNumeroVertex       = type Int64;
type TMNTModeDessinMaillage = (M3D_NONE, M3D_WIRE_FRAME, M3D_MESH);
type TMNTGridArray          = array of array of Double;
type TMNTTypeMaillage       = (tmUNKNOWN, tmREGULAR_GRID, tmTRIANGLES);
type TMNTBoundingBox  = record
  C1: TPoint3Df;
  C2: TPoint3Df;
end;
type

{ TMNTVertex }

 TMNTVertex = record
  ID         : TMNTNumeroVertex;
  Couleur    : TColor;
  Position   : TPoint3Df;
  Normale    : TPoint3Df;
  Displayed  : boolean;
  procedure setFrom(const QID: Int64; const QX, QY, QZ: double; const C: TColor); overload;
  procedure AddAtPosition(const QX, QY, QZ: double);
  procedure AddAtNormale(const QX, QY, QZ: double);
  procedure Normalize();
  function  MakeLigneForMailleur(const Idx: integer): string;
end;

type TMNTTriangleABC = record
   Numero     : TMNTNumeroTriangle;
   PointA     : TMNTNumeroVertex;
   PointB     : TMNTNumeroVertex;
   PointC     : TMNTNumeroVertex;
   VoisinAB   : TMNTNumeroTriangle;
   VoisinBC   : TMNTNumeroTriangle;
   VoisinCA   : TMNTNumeroTriangle;
   Displayed  : boolean;
   Couleur    : TColor;
   BoundingBox: TMNTBoundingBox;
end;
type

{ TLineAttributes }

 TLineAttributes = record
  ColorRGBA   : TGHTopoColor;
  LineWidthInPixels     : integer;
  LineWidthInMillimeters: double;
  procedure SetAttributes(const R, G, B, A: byte; const WdthPX: integer; const WdthMM: double); overload;
  procedure SetAttributes(const CC: TColor; const AA: byte; const WdthPX: integer; const WdthMM: double); overload;
  procedure SetTBGRAPen(const P: TBGRAPen);
  function  toTColor(): TColor;
  procedure fromTColor(const C: TColor);
  function  getOpacity(): byte;
  procedure setOpacity(const O: byte);
end;
type TVue3DParams  = record
  Name                   : string;
  Filtres                : string;
  DoFiltrer              : boolean;
  ElementsDrawn          : TSetElementsDrawn;
  Theta                  : Double;         // angle de rotation   (degré)
  Phi                    : double;         // angle d'inclinaison (degré)
  FovOrZoom              : double;         // angle de champ (OpenGL) ou Zoom
  CoefMagnification      :  double;
  ColorBackGround        : TColor;

  LineCube               : TLineAttributes;
  LineReferentiel        : TLineAttributes;
  LineVisees             : TLineAttributes;
  LineMaillage           : TLineAttributes;
  LineProfils            : TLineAttributes;

  ModeRepresentation     : TModeRepresentationGaleries;
  FillOpacity            : byte;
  ColorZMiniReseau       : TColor;
  ColorZMaxiReseau       : TColor;
  ColorZMiniMNT          : TColor;
  ColorZMaxiMNT          : TColor;
  ViseesLargeur          : integer;

  MaillageModeDessin     : TMNTModeDessinMaillage;
  MaillageUseDegrades    : boolean;
  MaillageProfilsDisplay : boolean;
  MaillageOpacity        : byte;
end;
type TCoupeDeveloppeeParams  = record
  Name : string;
  BackGround: TColor;
  ModeRepresentation : TModeRepresentationGaleries;
  ElementsDrawn      : TSetElementsDrawn;
  QdrSpc             : double;
  QdrType            : TQdrType;
  QdrColor           : TColor;
  ColorZMini         : TColor;
  ColorZMaxi         : TColor;
  ViseesLargeur      : integer;
end;
{$IFDEF GHTOPO_SIMPLIFIE}
type TArrOngletsParams = array[0..0] of TVue2DParams;
{$ELSE}
type TArrOngletsParams = array[0..NB_MAX_ONGLETS] of TVue2DParams;
{$ENDIF GHTOPO_SIMPLIFIE}

// mode de fonctionnement des navigateurs de la BDD
type TModeBDD = (mbddDISABLED,
                 mbddENTRANCES,
                 mbddRESEAUX,
                 mbddSECTEURS,
                 mbddCODES,
                 mbddEXPES,
                 mbddPOI,
                 mbddCHECK_ERRORS,
                 mbddNOEUDS,
                 mbddNAMESPACES,
                 mbddPROXIMITES,
                 mbddSERIES_ENCADREES // séries dont le numéro est différent du numéro de série de la station d'arrivée (= bouclages)
                 );



// procédures de conversions GCS->Ecran
type TProcGCSToSRC = function(const PM: TPoint2Df): TPoint of Object;
type TProcRefreshControlsBy3DView = procedure of object;
// demander aux modules parents d'effectuer une action sur l'appelant
type TProcActionModuleParentSurAppelant = procedure of object;




// **********************************************
//  Types de données pour les outils graphiques
// **********************************************
// Pour les coupes developpees
type

{ TPointCoupeDeveloppee }

 TPointCoupeDeveloppee = record
  P: double;
  Z: double;
  procedure setFrom(const QP, QZ: double);
end;
// Coupe développée
type TTypeJonction = (tjENTRANCE, tjCARREFOUR, tjEXTREMITE_CONDUIT);
type TJonctionCoupeDeveloppee = record
  NumeroJonction : Int64;
  TypeJonction   : TTypeJonction;
  IDJonction     : string;
  NoSerie        : integer;
  NoPoint        : integer;
  Abscisse       : double;
  Cote           : double;
end;

// sections du fichier d'aide
// objet Section
type THelpFileSection = record
  Index   : Integer;
  Topics  : string;
  Title   : string;
  Texte   : string;
end;

// mode de paramétrage pour la boite de dialogue du paramétrage des vues
type TModeDialogParamVues = (mpdVUE2D, mpdVUE3D, mpdCOUPE_DEVELOPPEE);

// Pour les grilles de stations

const
  GHTOPO_GRID_NB_LINES           = 1001; // 1 + Capa mémoire du DistoX
  LARGEUR_COLONNES_DISTOX_ID     = 60;
  LARGEUR_COLONNES_BLOCK_SEGMENT = 80;
  LARGEUR_COLONNES_HORODATE      = 200;
  LARGEUR_COLONNES_LAP           = 100;
  LARGEUR_COLONNES_LRUD          = 50;
  LARGEUR_COLONNES_TRAME_HEXA    = 320;

type TNodeNum = 0 .. maxLongint;
// Jonctions Toporobot
type

{ TJonctionXYZ }

  TJonctionXYZ = record
   NoNoeud     : TNodeNum;
   NoSer       : TNumeroSerie;
   NoSt        : integer;
   Position    : TPoint3Df;
   NbRefs      : integer;
   IDJonction  : string;
   function ToString(): string;
   procedure setPositionFromXYZ(const QX, QY, QZ: double);
end;

// Table des branches
type TBrancheXYZ = record
  NoEntranceRatt: integer;
  NoSerie       : integer;
  NoReseau      : integer;
  NoBranche     : integer;
  NomBranche    : string;
  NoeudDepart   : TNodeNum;
  NoeudArrivee  : TNodeNum;
  Rigidite      : double;  // module de raideur, égal à 1.00 par défaut
  PointsTopo    : array of TUneVisee;
  NbPoints      : integer;
  Accroissement : TPoint3Df;
  CoordsDepart  : TPoint3Df;
  CoordsArrivee : TPoint3Df;
end;




type TModeClavierVirtuel = (mkbdvALPHA_MINUSCULES, mkbdvALPHA_MAJUSCULES, mkbdvINTEGER, mkbdvDOUBLE);

// pour le DistoX

type TModeAttenteDonneesRX = (mrxNONE, mrxDEPILAGE_MESURE);
// pour le DistoX

type TProtoSerialError = (PROTO_OK,
                          PROTO_READ, PROTO_WRITE,
                          PROTO_COMMAND,  PROTO_ADDR,  PROTO_PACKET,
                          PROTO_CONNECT, PROTO_TIMEOUT, PROTO_MAX);

// pour le XML
const
  XML_CHAR_LT       = '&lt;';   // &lt; -> "<" (lt = lower than = inférieur à = "<" )
  XML_CHAR_GT       = '&gt;';   // &gt; -> ">" (gt = greater than = supérieur à = ">")
  XML_CHAR_AMP      = '&amp;';  // &amp; -> "&" (comme dit fraoustin - amp = ampersand)
  XML_CHAR_APOS     = '&apos;'; // &apos; -> ' (apos = apostrophe)
  XML_CHAR_QUOT     = '&quot;'; // &quot; -> " (quot = quotation mark = quillemet)
// types d'événement
type TNewEvent = procedure (Sender: TObject; Param1: string) of object;

// modes de remplacement
type TModeRemplacementStations = (mrsSECTEURS, mrsCODES, mrsEXPES);
type TModeRemplacementAntennes = set of (mraSERIES, mraRESEAUX, mraSECTEURS, mraCODES, mraEXPES);

// quads pour vue 3D
type TQuad = record
   VertexA           : TPoint3Df;
   VertexB           : TPoint3Df;
   VertexC           : TPoint3Df;
   VertexD           : TPoint3Df;
   //Normale           : TPoint3Df;
   FacetteVisible    : boolean;
   Depth             : Double;
   Vertex2D_A        : TPoint;
   Vertex2D_B        : TPoint;
   Vertex2D_C        : TPoint;
   Vertex2D_D        : TPoint;
end;
const NBFACESBYVISEE = 6;
type TPortionTubeVisee = record
   Visible           : Boolean;
   Drawn             : Boolean; // dessiné en fonction du MétaFiltre;
   CouleurTube       : TGHTopoColor;
   Type_Entite       : TTypeDeVisee;
   IdxEntrance       : integer;
   IdxReseau         : integer;
   IdxSecteur        : integer;
   IdxExpe           : integer;
   Facettes: array[1 .. NBFACESBYVISEE] of TQuad;
   DepthField        : double;
   Highlighted       : boolean;
end;

// Filtres nommés
type

{ TFiltrePersonnalise }

 TFiltrePersonnalise = record
  Nom            : string;
  Couleur        : TGHTopoColor;
  Expression     : string;
  Description    : string;
  procedure setFrom(const QNom: string; const QCouleur: TGHTopoColor; const QExpression, QDescription: string);
  function toLineXTB(const QIdx: integer): string;

end;
// Points d'intérêt
type TPOIStatut = (poiUNKNOWN, poiTODO, poiDONE);
type

{ TPointOfInterest }

 TPointOfInterest = record
  Statut        : TPOIStatut;
  Serie         : TNumeroSerie;
  Station       : integer;
  Coordinates   : TPoint3Df; // pour accélérer certains calculs
  Couleur       : TColor;
  LabelTerrain  : string;
  Description   : string;
  function toString(): string;
end;
type TCriticiteMessaqeErreurGHTopo = (cmeNOTE, cmeWARNING, cmeERROR, cmeCRITICAL, cmeERROR_AUTOFIXED);
type TTableExaminee                = (tmeENTRANCES, tmeRESEAUX, tmeSECTEURS, tmeCODES, tmeEXPES, tmeSERIES);
type TMessaqeErreurGHTopoCompiler = record
  TableExaminee : TTableExaminee;
  Index         : Int64;
  Criticite     : TCriticiteMessaqeErreurGHTopo;
  Couleur       : TColor;
  Message       : string;
end;

// constantes des filtres
const
  kFLT_NIL         = 0;      //  NIL               NIL
  kFLT_ALL         = 1;      //  ALL               ALL
  kFLT_ID          = 2;      //  ID                ID
  kFLT_LONGUEUR    = 3;      //  LONGUEUR          LONGUEUR
  kFLT_AZIMUT      = 4;      //  AZIMUT            AZIMUT
  kFLT_PENTE       = 5;      //  PENTE             PENTE
  kFLT_DATE        = 6;      //  DATE              DATE
  kFLT_COULEUR     = 7;      //  COULEUR           COULEUR
  kFLT_X           = 8;      //  X                 X
  kFLT_Y           = 9;      //  Y                 Y
  kFLT_Z           = 10;     //  Z                 Z
  kFLT_LARGEUR     = 11;     //  LARGEUR           LARGEUR
  kFLT_HAUTEUR     = 12;     //  HAUTEUR           HAUTEUR
  kFLT_DATES       = 13;     //  DATES             DATES
  kFLT_COULEURS    = 14;     //  COULEURS          COULEURS
  kFLT_SERIE       = 15;     //  SERIE;
  kFLT_RESEAU      = 16;     //  Réseau
  kFLT_CODE        = 17;     //  code
  kFLT_EXPE        = 18;     //  Séance (expé)
  kFLT_TYPEVISEE   = 19;     //  type de visée
  kFLT_SECTEUR     = 20;     // secteur
  kFLT_ENTRANCE_RATT = 21;   // entrée de rattachement
  kFLT_NAMESPACE     = 22;   // espace de noms


  //-----------------------------------------------

// pour les croquis (simplifié: polygones et polylignes. On ne fait pas un GHCaveDraw !
type TKrobardIDStylePolyligne  = type integer;
type TKrobardIDStyleAnnotation = type integer;
type TKrobardPolyVertex = record
  IDBaseStation: TIDBaseStation;
  Offset       : TPoint3Df;
end;
type TKrobardStylePolyligne = record
  Name       : string;
  LineColor  : TColor;
  LineOpacity: byte;
  LineStyle  : TPenStyle;
  LineWidth  : integer;
  FillColor  : TColor;
  FillOpacity: byte;
  Closed     : boolean;
  Filled     : boolean;
end;
type TKrobardAnnotation = record
  IDStyle  : TKrobardIDStyleAnnotation;
  Position : TKrobardPolyVertex;
  Alignment: byte;
  Texte    : string;
end;
type TKrobardPolyligne = record
  IDStyle: TKrobardIDStylePolyligne;
  Sommets: array of TKrobardPolyVertex;
  BoundingBox: TRect2Df;
end;

// spécifique au DistoX
type TTypeViseeDistoX = (tvdRADIANTE, tvdCHEMINEMENT);
type TDistoXSerialNumber     = type word;
type TDistoXVersionFirmware  = type word;
type TDistoXVersionHardware  = type word;


type

{ TMesureViseeDistoX }

 TMesureViseeDistoX = record
  TimeStamp       : TDateTime; // horodatage de la visée
  TypeMesure      : TTypeViseeDistoX;
  Device          : TDistoXSerialNumber;
  Longueur        : double;
  Azimut          : double;
  Pente           : double;
  procedure setFrom(const QTimeStamp: TDateTime; const QDevice: TDistoXSerialNumber; const QLongueur, QAzimut, QPente: double; const QTypeMesure: TTypeViseeDistoX = tvdRADIANTE);
  procedure setTypeMesure(const TM: TTypeViseeDistoX);
  procedure setTimestamp(const T: TDateTime);
  procedure setDistoXSerialNumber(const D: TDistoXSerialNumber);
  procedure Empty(const QDevice: TDistoXSerialNumber);
  function  DebugString(): string;
  function  toBackupLine(): string;
end;

type

{ TMesureBruteDistoX }

 TMesureBruteDistoX = record
  Longueur        : double;
  Azimut          : double;
  Pente           : double;
  DeltaXYZ        : TPoint3Df;
  procedure setFrom(const QLongueur, QAzimut, QPente: double);

  procedure Empty();
  function  DebugString(): string;
end;




type

{ TVectorDataDistoX }

  TVectorDataDistoX = record
  X : word;
  Y : word;
  Z : word;
  TimeStamp  : TDateTime; // horodatage de la mesure
  procedure setFrom(const QX, QY, QZ: word); overload;
  procedure setFrom(const QX, QY, QZ: word; const QTimeStamp: TDateTime); overload;
  function  CheckRanges(): boolean;
end;
type TDataCalibrationDistoX = record
  MX: word;
  MY: word;
  MZ: word;
  GX: word;
  GY: word;
  GZ: word;
  TimeStamp  : TDateTime; // horodatage de la mesure
end;
type TArrayMesureViseeDistoX             = array of TMesureViseeDistoX;
type TProcTransmitMesureDistoX           = procedure (const MV: TMesureViseeDistoX) of object;

// callback pour transfert de série
type TCallbackGotoErrorSerie       = procedure(const Ser: TNumeroSerie; const St: TNumeroStation) of object;

type TProcOfObjectWithBaseStation  = procedure(const BS: TToporobotIDStation) of object;
type TProcOfObjectWithXY           = procedure(const QX, QY: double; const TagString: string) of object;

// pour le multithread
type TProcOfObjectUsesInteger = procedure(const IDThread: integer; const QStart, QEnd, QDone: integer) of object;
const NB_MAX_THREADS = 4;
// pour le clavier numérique
type TPaveNumModeSaisie = (pnmINTEGER, pnmREAL, pnmTOPOROBOT_STATION);
{ TFTPParameters }
type TFTPParameters = record
  HostName: string;
  User    : string;
  Port    : string;
  procedure setFrom(const QHost, QUser, QPort: string);
end;

implementation
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common;

{ TFiltrePersonnalise }

procedure TFiltrePersonnalise.setFrom(const QNom: string; const QCouleur: TGHTopoColor; const QExpression, QDescription: string);
begin
  self.Nom         := QNom;
  self.Couleur     := QCouleur;
  self.Expression  := QExpression;
  self.Description := QDescription;
end;

function TFiltrePersonnalise.toLineXTB(const QIdx: integer): string;
begin
  Result := Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
             FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
             FORMAT_STRING + TAB +
             FORMAT_STRING + TAB + FORMAT_STRING,
             [-16, QIdx,
              self.Couleur.Red, self.Couleur.Green, self.Couleur.Blue,
              self.Nom,
              self.Expression, self.Description
              ]);
end;

{ TPoint3DfOrderedByP }

procedure TPoint3DfOrderedByP.setFrom(const QX, QY, QZ, QP: double);
begin
  self.X := QX;
  self.Y := QY;
  self.Z := QZ;
  self.P := QP;
end;

{ TVectorDataDistoX }

procedure TVectorDataDistoX.setFrom(const QX, QY, QZ: word);
begin
  self.X := QX;
  self.Y := QY;
  self.Z := QZ;
  self.TimeStamp := Now();
end;

procedure TVectorDataDistoX.setFrom(const QX, QY, QZ: word; const QTimeStamp: TDateTime);
begin
  self.X := QX;
  self.Y := QY;
  self.Z := QZ;
  self.TimeStamp := QTimeStamp;
end;

function TVectorDataDistoX.CheckRanges(): boolean;
begin
  Result := InRange(self.X, 0, MAX_WORD_VALUE) and InRange(self.Y, 0, MAX_WORD_VALUE) and InRange(self.Z, 0, MAX_WORD_VALUE);
end;

{ TPointCoupeDeveloppee }

procedure TPointCoupeDeveloppee.setFrom(const QP, QZ: double);
begin
  self.P := QP;
  self.Z := QZ;
end;

{ TMesureBruteDistoX }

procedure TMesureBruteDistoX.setFrom(const QLongueur, QAzimut, QPente: double);
begin
  self.Longueur := QLongueur;
  self.Azimut   := QAzimut;
  self.Pente    := QPente;
  Self.DeltaXYZ.Empty();
end;

procedure TMesureBruteDistoX.Empty();
begin
  self.setFrom(0.0, 0.0, 0.0);
  self.Longueur := 0.00;
  self.Azimut   := 0.00;
  self.Pente    := 0.00;
  self.DeltaXYZ.Empty();
end;

function TMesureBruteDistoX.DebugString(): string;
begin
  Result := Format('Mesure brute: %.3f, %.3f, %.3f', [self.Longueur, self.Azimut, self.Pente]);
end;



{ TMesureViseeDistoX }

procedure TMesureViseeDistoX.setFrom(const QTimeStamp: TDateTime; const QDevice: TDistoXSerialNumber; const QLongueur, QAzimut, QPente: double; const QTypeMesure: TTypeViseeDistoX = tvdRADIANTE);
begin
  self.TimeStamp   := QTimeStamp;
  self.TypeMesure  := QTypeMesure;
  self.Device      := QDevice;
  self.Longueur    := QLongueur;
  self.Azimut      := QAzimut;
  self.Pente       := QPente;
end;

procedure TMesureViseeDistoX.setTypeMesure(const TM: TTypeViseeDistoX);
begin
  self.TypeMesure := TM;
end;

procedure TMesureViseeDistoX.setTimestamp(const T: TDateTime);
begin
  self.TimeStamp := T;
end;

procedure TMesureViseeDistoX.setDistoXSerialNumber(const D: TDistoXSerialNumber);
begin
  self.Device  := D;
end;

procedure TMesureViseeDistoX.Empty(const QDevice: TDistoXSerialNumber);
begin
  setFrom(Now(), QDevice, 0.00, 0.00, 0.00)
end;

function TMesureViseeDistoX.DebugString(): string;
begin
  Result := Format('%s - DistoX%d - %.3f, %.3f, %.3f', [DateTimePascalToDateTimeSQL(self.TimeStamp), self.Device, self.Longueur, self.Azimut, self.Pente]);
end;

function TMesureViseeDistoX.toBackupLine(): string;
begin
  Result := Format('%s; %d;  %10.3f; %9.3f; %9.3f;', [DateTimePascalToDateTimeSQL(Self.TimeStamp), Self.Device, Self.Longueur, Self.Azimut, Self.Pente]);
end;

{ TGHTopoMarker }

procedure TGHTopoMarker.setFrom(const QD: boolean; const QX, QY: double; const QC: TGHTopoColor; const QS: string);
begin
  self.Displayed := QD;
  self.X := QX;
  self.Y := QY;
  self.Couleur := QC;
  self.Caption := Trim(QS);
end;

procedure TGHTopoMarker.setFrom(const QD: boolean; const QX, QY: double; const QC: TColor; const QS: string);
begin
  self.Displayed := QD;
  self.X := QX;
  self.Y := QY;
  self.Couleur.setFrom(QC, 255);
  self.Caption := Trim(QS);
end;

{ TGISLayer }

procedure TGISLayer.setFrom(const QSymbolStyle: integer; const QSymbolSize: double; const QSymbolColor: TGHTopoColor; const QLayerVarName, QLayerTitle, QLayerDescription, QLayerAttribution: string);
begin
  self.SymboleStyle             := QSymbolStyle;
  self.SymbolSize               := QSymbolSize;
  self.SymbolColor              := QSymbolColor;
  self.LayerVarName             := QLayerVarName;
  self.LayerTitle               := QLayerTitle;
  self.LayerDescription         := QLayerDescription;
  self.LayerAttribution         := QLayerAttribution;
end;

{ TNameSpace }

procedure TNameSpace.setFrom(const QCouleur: TGHTopoColor; const QNom, QDescription: string);
begin
  self.Couleur     := QCouleur;
  self.Nom         := QNom;
  self.Description := QDescription;
end;

{ TGHTopoColor }

procedure TGHTopoColor.setFrom(const R, G, B: byte; const A: byte = 255);
begin
  self.Red   := R AND 255;
  self.Green := G AND 255;
  self.Blue  := B AND 255;
  self.Alpha := A AND 255;
end;

procedure TGHTopoColor.setFrom(const C: TColor; const A: byte = 255);
begin
  self.setFrom(Graphics.Red(C), Graphics.Green(C), Graphics.Blue(C), A);
end;

function TGHTopoColor.toTColor(): TColor;
begin
  result := RGBToColor(self.Red, self.Green, self.Blue);
end;
// récupère les RGB depuis les représentations $RRRGGGBBB et #RRRGGGBBB
procedure TGHTopoColor.setFromStrGHTopoColor(const S: string; const qDefault: TColor);
begin
  if (length(S) < 10) then // la notation Pascal exige 1 + 3*3 caractères
    self.setFrom(clDefault, 255)
  else
    self.setFrom(StrToIntDef(Copy(S, 2, 3), Graphics.Red(qDefault)),
                 StrToIntDef(Copy(S, 5, 3), Graphics.Green(qDefault)),
                 StrToIntDef(Copy(S, 8, 3), Graphics.Blue(qDefault)),
                 255)
end;


function TGHTopoColor.toStrGHTopoColor(): string;
begin
  Result := Format('$%.3d%.3d%.3d', [self.Red, self.Green, self.Blue]);
end;

procedure TGHTopoColor.setOpacity(const O: byte);
begin
  self.Alpha := O and 255;
end;

function TGHTopoColor.toHTMLColor(): string;
begin
  Result := format('#%.2X%.2X%.2X', [self.Red, self.Green, self.Blue]);
end;

function TGHTopoColor.toKMLColor(): string;
begin
  Result := Format('<color>%.2X%.2X%.2X%.2X</color>',[self.Alpha, self.Blue, self.Green, self.Red]);
end;

function TGHTopoColor.toSVGColor(): string;
const m = 1 / 256.0;
begin
  Result := Format(' rgb(%.2f%%, %.2f%%, %.2f%%)', [self.Red * m , self.Green * m , self.Blue * m]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
end;



function TGHTopoColor.toGrayScale(): byte; //Convertit une couleur en niveaux de gris; méthode NTSC.
begin
  Result := round(0.30 * self.Red + 0.59 * self.Green + 0.11 * self.Blue);
end;

function TGHTopoColor.getFloatRed(): double;
begin
  Result := self.Red / 256.0;
end;
function TGHTopoColor.getFloatGreen(): double;
begin
  Result := self.Green / 256.0;
end;
function TGHTopoColor.getFloatBlue(): double;
begin
  Result := self.Blue / 256.0;
end;
function TGHTopoColor.getFloatAlpha(): double;
begin
  Result := self.Alpha / 256.0;
end;

{ TStationMatchFound }

procedure TStationMatchFound.setFrom(const QSerie: TNumeroSerie; const QStation: TNumeroStation; const QMatch: string);
begin
  Self.Serie   := QSerie;
  Self.Station := QStation;
  Self.Match   := QMatch;
end;

{ TTexteAttributs }

procedure TTexteAttributs.setFrom(const QStyleName, QFontName: string; const QFontColor, QBackColor: TColor; const QFontStyle: TFontStyles; const QHauteurTexte: double; const QAlignement: byte; const QAngleRot: integer);
begin
  self.StyleName    := QStyleName;
  self.FontName     := QFontName;
  self.FontColor    := QFontColor;
  self.BackColor    := QBackColor;
  self.FontStyle    := QFontStyle;
  self.HauteurTexte := QHauteurTexte;
  self.Alignement   := QAlignement;  // position du texte; cf organisation d'un clavier numérique pour le point d'accrochage
  self.AngleRot     := QAngleRot;
end;

procedure TTexteAttributs.setDefault();
begin
  setFrom('Style0', DEFAULT_FONT_NAME, clBlack, clWhite, [], 1.25, 0, 0);
end;

{ TViseeAntenneFound }

procedure TViseeAntenneFound.setFrom(const QIdx: Int64; const QVA: TViseeAntenne; const QPosition: TPoint3Df; const Qdp: double; const QAzimutAbsolu: double);
begin
  self.Idx      := QIdx;
  self.VA       := QVA;
  self.Position := QPosition;
  self.dp       := Qdp;
  self.AzimutAbsolu := QAzimutAbsolu;
end;

{ TParamFoncCorrectionAngulaire }

procedure TParamFoncCorrectionAngulaire.Empty();
begin
  self.setFrom(0.00, 0.00, 0.00);
end;

procedure TParamFoncCorrectionAngulaire.setFrom(const QCo, QErrMax, QPosErrMax: double);
begin
  self.Co        := QCo;
  self.ErreurMax := QErrMax;
  self.PosErrMax := QPosErrMax;
end;

{ TCode }

function TCode.getLibelle(): string;
var
  QIdxNameSpace, QNoCode: integer;
  WU: String;
begin
  DecomposeNumeroCode(self.IDCode, QIdxNameSpace, QNoCode);
  WU := IIF(QIdxNameSpace = 0, '', Format('@%d', [QIdxNameSpace]));
  Result := Format('%d%s - %s', [QNoCode, WU, '']);
end;

procedure TCode.setUsualParameters(const NumeroCode: TNumeroCode; const GradAz, GradInc, FactLongueur: double; const PsiL, PsiAz, PsiP: double; const DiametreBoule1, DiametreBoule2, ErreurTourillon: double; const Commentaires: string);
begin
  self.IDCode               := NumeroCode;
  self.Commentaire          := Commentaires;
  self.GradAz               := GradAz;
  self.GradInc              := GradInc;
  self.FactLong             := FactLongueur;
  self.PsiL                 := PsiL;
  self.PsiAz                := PsiAz;
  self.PsiP                 := PsiP;
  self.AngLimite            := 0.00;
  self.DiametreBoule1       := DiametreBoule1;
  self.DiametreBoule2       := DiametreBoule2;
  self.ErreurTourillon      := ErreurTourillon;
  self.ParamsFuncCorrAz.Empty();
  self.ParamsFuncCorrInc.Empty();
end;

function TCode.ExplainCodeAzimut(): string;
var
  ubb: Int64;
  DescUBB, SensVisee: String;
begin
  ubb := Round(self.GradAz);
  Result := Format('%d: ', [ubb]);
  case ubb of
    359, 360: begin // visées directes en degrés
       DescUBB   := GetResourceString(rsDESC_UNITE_ANGULAIRE_DEGRES);
       SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);

    end;
    399, 400: begin // visées directes en grades
       DescUBB   := GetResourceString(rsDESC_UNITE_ANGULAIRE_GRADES);
       SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
    end;
    349, 350: begin  // visées inverses en degrés
       DescUBB   := GetResourceString(rsDESC_UNITE_ANGULAIRE_DEGRES);
       SensVisee := GetResourceString(rsDESC_VISEE_INVERSE);
    end;
    389, 390: begin // visées inverses en grades
       DescUBB   := GetResourceString(rsDESC_UNITE_ANGULAIRE_GRADES);
       SensVisee := GetResourceString(rsDESC_VISEE_INVERSE);
    end;
  else
    begin
      DescUBB   := Format(FORMAT_NB_REAL_3_DEC, [self.GradAz]);
      SensVisee := GetResourceString('??');
    end;
  end;
  result += DescUBB + ', ' + SensVisee;
end;

function TCode.ExplainCodePente(): string;
var
  ucc: Int64;
  DescUCC, PosZero, SensVisee: String;
begin
  result := '';
  ucc := Trunc(self.GradInc);
  Result := Format('%d: ', [ucc]);
  case ucc of
    360:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_ANGULAIRE_DEGRES);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_HZ);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;

    350:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_ANGULAIRE_DEGRES);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_HZ);
        SensVisee := GetResourceString(rsDESC_VISEE_INVERSE);
      end;
    370:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_PENTES_PERCENT);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_HZ);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;
    380: // mode plongée: Longueur, Dénivelé
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_PENTE_DENIVELE);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_HZ);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;
    390:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_ANGULAIRE_GRADES);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_HZ);
        SensVisee := GetResourceString(rsDESC_VISEE_INVERSE);
      end;
    400:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_ANGULAIRE_GRADES);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_HZ);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;
    359:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_ANGULAIRE_DEGRES);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_NADIRAL);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;
    399:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_ANGULAIRE_GRADES);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_NADIRAL);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;
    361:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_ANGULAIRE_DEGRES);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_ZENITHAL);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;
    401:
      begin
        DescUCC   := GetResourceString(rsDESC_UNITE_ANGULAIRE_GRADES);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_ZENITHAL);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;
    UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR:
      begin
        DescUCC   := GetResourceString(rsDESC_DEVICE_TLM330);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_HZ);
        SensVisee := GetResourceString(rsDESC_VISEE_DIRECTE);
      end;
    UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV:
      begin
        DescUCC   := GetResourceString(rsDESC_DEVICE_TLM330);
        PosZero   := GetResourceString(rsDESC_POS_ZERO_HZ);
        SensVisee := GetResourceString(rsDESC_VISEE_INVERSE);
      end;
    else
      begin
        DescUCC   := Format(FORMAT_NB_REAL_3_DEC, [self.GradAz]);
        PosZero   := '??';
        SensVisee := '??';
      end;
  end;
  result += DescUCC + ', ' + SensVisee + ', ' + PosZero;
end;

function TCode.toLineXTB(const ModeSaveTab: TModeSaveTAB): string;
const
  LINE_CODE    = FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_STRING;
  FMT_PARAMS_CORR_FUNC = FORMAT_NB_REAL_6_DEC + TAB + FORMAT_NB_REAL_6_DEC + TAB + FORMAT_NB_REAL_6_DEC;
begin
  with self do
  begin
    case ModeSaveTAB of
      mtabEXTENDEDTAB:
        //-1	1	360.00	360.00	0.05	1.00	1.00	1.00	100.00		5314011	0.000000	0.000000	0.000000	0.000000	0.000000	0.000000
        Result := Format(LINE_CODE + TAB + FORMAT_NB_INTEGER + TAB +
                     FMT_PARAMS_CORR_FUNC + TAB +
                     FMT_PARAMS_CORR_FUNC + TAB +
                     FORMAT_NB_REAL_3_DEC, //+ TAB +
                     // FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC
                       [-1,
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
                       [-1,
                        IDCode,
                        GradAz, GradInc,
                        0.10, 1.0, 1.0, //PsiL, PsiAz, PsiP,
                        100.00, 0.00, //FactLong, AngLimite,
                        SafeTruncateString(Commentaire, 50)
                       ]);
    end;
  end; //with Code
end;


{ TRect2Df }

procedure TRect2Df.setFrom(const QX1, QY1, QX2, QY2: double);
begin
  self.X1 := QX1;
  self.Y1 := QY1;
  self.X2 := QX2;
  self.Y2 := QY2;
end;

procedure TRect2Df.ResetBoundingBox();
begin
  self.setfrom(INFINI, INFINI, -INFINI, -INFINI);
end;

procedure TRect2Df.UpdateBoundingBox(const QX, QY: double);
begin
   self.X1 := Min(self.X1, QX);
   self.Y1 := Min(self.Y1, QY);
   self.X2 := Max(self.X2, QX);
   self.Y2 := Max(self.Y2, QY);
end;

procedure TRect2Df.UpdateBoundingBox(const PT: TPoint2Df);
begin
  self.UpdateBoundingBox(PT.X, PT.Y);
end;

procedure TRect2Df.UpdateBoundingBox(const PT: TPoint3Df);
begin
  self.UpdateBoundingBox(PT.X, PT.Y);
end;

function TRect2Df.ContainsPoint(const QX, QY: double): boolean;
begin
  result := InRange(QX, self.X1, self.X2) AND
            InRange(QY, self.Y1, self.Y2);
end;

function TRect2Df.ContainsPoint(const P: TPoint2Df): boolean;
begin
  Result := self.ContainsPoint(P.X, P.Y);
end;

function TRect2Df.ContainsPoint(const P: TPoint3Df): boolean;
begin
  Result := self.ContainsPoint(P.X, P.Y);
end;
function TRect2Df.ContainsSegment(const P1, P2: TPoint2Df; const PartiallyInRectangle: boolean = true): boolean;
begin
  if (PartiallyInRectangle) then Result := self.ContainsPoint(P1) or  self.ContainsPoint(P2)
                            else Result := self.ContainsPoint(P1) and self.ContainsPoint(P2);
end;

function TRect2Df.ContainsSegment(const X1, Y1, X2, Y2: double; const PartiallyInRectangle: boolean): boolean;
begin
  if (PartiallyInRectangle) then Result := self.ContainsPoint(X1, Y1) or  self.ContainsPoint(X2, Y2)
                            else Result := self.ContainsPoint(X1, Y1) and self.ContainsPoint(X2, Y2);
end;

function TRect2Df.ContainsQuad(const P1, P2, P3, P4: TPoint2Df): boolean;
begin
  Result := self.ContainsPoint(P1) OR
            self.ContainsPoint(P2) OR
            self.ContainsPoint(P3) OR
            self.ContainsPoint(P4);
end;


{ TFTPParameters }

procedure TFTPParameters.setFrom(const QHost, QUser, QPort: string);
begin
  self.HostName := QHost;
  self.User     := QUser;
  self.Port     := QPort;

end;

{ TUneVisee }

procedure TUneVisee.setFrom(const QNoVisee: integer;
                            const QTypeVisee: TTypeDeVisee;
                            const QIdSecteur: TNumeroSecteur;
                            const QCode: TNumeroCode; const QExpe: TNumeroExpe;
                            const QL, QAz, QP: double;
                            const QLG, QLD, QHZ, QHN: double;
                            const QHorodatage       : TDateTime;
                            const QIDStation        : string = '';
                            const QCommentaire      : string = '';
                            const QTemperature      : double = 0.00;
                            const QHumidity         : double = 0.00);

begin
  self.Horodatage:= QHorodatage;
  self.NoVisee   := QNoVisee;
  self.TypeVisee := QTypeVisee;
  self.IDSecteur := QIdSecteur;
  self.Code      := QCode;
  self.Expe      := QExpe;
  self.setLongAzInc(QL, QAz, QP);
  self.setLRUD(QLG, QLD, QHZ, QHN);
  self.IDTerrainStation  := QIDStation;
  self.Commentaires      := QCommentaire;
  self.AccroissXYZ.Empty();
  self.AccroissP := 0.00;
  //self.AccroissZ := 0.00;
  self.Temperature       := QTemperature;
  self.Humidity          := QHumidity;
  self.Horodatage        := QHorodatage;
end;

procedure TUneVisee.Empty(const Commentaire: string = '');
begin
  self.setFrom(0, tgDEFAULT, 0, 1, 1,  0.001, 0.00, 0.00,  0.00, 0.00, 0.00, 0.00, Now(), '', Commentaire);
end;

function TUneVisee.toLineXTB(const ModeSaveTab: TModeSaveTAB; const QNumeroDeSerie: TNumeroSerie; const QNumeroStation: TNumeroStation): string;
const
  LINE_STATION = FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_STRING;
begin
  with self do
  begin
    case ModeSaveTAB of
      mtabEXTENDEDTAB:
      begin
        Result := Format(LINE_STATION + TAB + FORMAT_STRING + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER +
                         TAB + FORMAT_STRING + TAB + FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC,
                        [QNumeroDeSerie, QNumeroStation,
                         Code, Expe,
                         Longueur, Azimut, Pente,
                         LG, LD, HZ, HN,
                         Commentaires,
                         IDTerrainStation,
                         TypeVisee,
                         IDSecteur,
                         FormatterHorodatage(Horodatage), //Horodatage,
                         Temperature, Humidity
                        ]);
      end;
      mtabTOPOROBOT:
      begin
        Result := Format(LINE_STATION,
                        [QNumeroDeSerie, QNumeroStation,
                         Code, Expe,
                         Longueur, Azimut, Pente,
                         LG, LD, HZ, HN,
                         Commentaires
                        ]);
      end;
    end;
  end; //  with Station do begin
end;

procedure TUneVisee.setLongAzInc(const L, A, P: double);
begin
  self.Longueur := L;
  self.Azimut   := A;
  self.Pente    := P;
end;

procedure TUneVisee.setLongAzInc(const L, A, P: string);
begin
  self.Longueur := ConvertirEnNombreReel(L, 0.00);
  self.Azimut   := ConvertirEnNombreReel(A, 0.00);
  self.Pente    := ConvertirEnNombreReel(P, 0.00);
end;

procedure TUneVisee.setLRUD(const L, R, U, D: double);
begin
  self.LG := L;
  self.LD := R;
  self.HZ := U;
  self.HN := D;
end;

procedure TUneVisee.setLRUD(const L, R, U, D: string);
begin
  self.LG := ConvertirEnNombreReel(L, 0.00);
  self.LD := ConvertirEnNombreReel(R, 0.00);
  self.HZ := ConvertirEnNombreReel(U, 0.00);
  self.HN := ConvertirEnNombreReel(D, 0.00);
end;

function TUneVisee.DebugString(): string;
begin
  result := format('L = %.3f, Az = %.3f, P = %.3f', [self.Longueur, self.Azimut, self.Pente]);
end;



{ TExpe }

procedure TExpe.setFrom(const QNumeroExpe: TNumeroExpe;
                        const YYYY, MM, DD: word;
                        const QIdxCouleur: TIdxCouleur;
                        const QModeDecl: TModeCalculDeclimag;
                        const QDeclinaisonInDegrees: double;
                        const QSpeleometre, QSpeleographe: string;
                        const QCommentaires: string);
begin
  self.IDExpe     := QNumeroExpe;
  self.AnneeExpe  := YYYY;
  self.MoisExpe   := MM;
  self.JourExpe   := DD;
  self.IdxCouleur := QIdxCouleur;
  self.ModeDecl   := QModeDecl;
  self.DeclinaisonInDegrees:= QDeclinaisonInDegrees;
  self.Operateur  := QSpeleometre;
  self.ClubSpeleo := QSpeleographe;
  self.Commentaire:= QCommentaires;
end;

function TExpe.getLibelle(): string;
var
  QIdxNameSpace, QNoExpe: TNumeroExpe;
  WU: String;
begin
  DecomposeNumeroExpe(Self.IDExpe, QIdxNameSpace, QNoExpe);
  WU := IIF(QIdxNameSpace = 0, '', Format('@%d', [QIdxNameSpace]));
  Result := Format('%d%s - %s', [QNoExpe, WU, '']);
end;

function TExpe.getDateStr(): string;
begin
  result := DateToStr(GetSecuredDate(self.AnneeExpe, self.MoisExpe, self.JourExpe));
end;

function TExpe.getTDateTime(): TDateTime;
begin
  result := GetSecuredDate(self.AnneeExpe, self.MoisExpe, self.JourExpe);
end;

function TExpe.toLineXTB(): string;
begin
  Result := Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                    FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                    FORMAT_STRING+TAB+FORMAT_STRING+TAB+
                    FORMAT_NB_INTEGER+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                    FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+ // anciennement '%f'+TAB+FORMAT_NB_INTEGER+TAB+
                    FORMAT_STRING,
                           [-2,
                            Self.IDExpe,
                            Self.JourExpe, Self.MoisExpe, Self.AnneeExpe, {$WARNING: TEXpe.DateExpe à implementer}
                            Self.Operateur, Self.ClubSpeleo,
                            Ord(Self.ModeDecl),
                            Self.DeclinaisonInDegrees,
                            0, Self.IdxCouleur, //Inclinaison, Couleur,
                            Self.Commentaire
                           ]);
end;


{ TEntrance }

procedure TEntrance.setFrom(const NomEntree, IDTerrain: string; const QPosition: TPoint3Df; const RefSer: TNumeroSerie; const RefSt: TNumeroStation; const Couleur: TGHTopoColor; const Observ: string);
begin
  self.eNomEntree := NomEntree;
  self.eIDTerrain := IDTerrain;
  self.ePosition  := QPosition;
  self.eRefSer    := RefSer;
  self.eRefSt     := RefSt;
  self.eCouleur   := Couleur;
  self.eObserv    := Observ;
end;

function TEntrance.toLineXTBSection6(const QIdx: integer): string;
begin
  Result := Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_STRING + TAB + FORMAT_STRING + TAB +
                   FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB +
                   FORMAT_STRING + TAB + FORMAT_STRING,
                  [-6, QIdx + 1,//i+1,
                   self.eNomEntree,
                   self.eIDTerrain,
                   self.eRefSer,  self.eRefSt,
                   self.ePosition.X, self.ePosition.Y, self.ePosition.Z,
                   self.eObserv,
                   self.eCouleur.toStrGHTopoColor()
                   ]);

end;

function TEntrance.toLineXTBSection5(const QIdx: integer): string;
begin
  Result := Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB +
                   FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_STRING,
                          [-5, QIdx + 1, //i+1
                          self.ePosition.X, self.ePosition.Y, self.ePosition.Z,
                          self.eRefSer, self.eRefSt,
                          self.eObserv
                          ]);
end;

{ TPoint2Df }

procedure TPoint2Df.setFrom(const QX, QY: double);
begin
  self.X := QX;
  self.Y := QY;
end;

function TPoint2Df.getNorme(): double;
begin
  result := hypot(self.X, self.Y);
end;

procedure TPoint2Df.Empty();
begin
  self.X := 0.00;
  self.Y := 0.00;
end;

{ TPoint3Df }

procedure TPoint3Df.setFrom(const QX, QY, QZ: double);
begin
  self.X := QX;
  self.Y := QY;
  self.Z := QZ;
end;

procedure TPoint3Df.Empty();
begin
  self.X := 0.00;
  self.Y := 0.00;
  self.Z := 0.00;
end;

function TPoint3Df.getNorme(): double;
begin
  result := sqrt(sqr(self.x)+sqr(self.y)+sqr(self.z))
end;

function TPoint3Df.getProjHZ(): double;
begin
  result := hypot(self.X, self.Y);
end;

function TPoint3Df.getAzimut(const Unite: double): double;
var
  a: double;
begin
  a := ArcTan2(self.Y, self.X + 1e-12);
  if (a < 0) then a := a + TWO_PI;
  a := 0.50 * PI - a;
  if (a < 0) then a := a + TWO_PI;
  Result := a * 0.50 * Unite / PI;
end;

function TPoint3Df.getInclinaison(const Unite: double): double;
var
  dP: Double;
begin
  dP := self.getProjHZ();
  Result := ArcTan2(self.Z, dp) * 0.5 * Unite / pi;
end;

{ TMNTVertex }

procedure TMNTVertex.setFrom(const QID: Int64; const QX, QY, QZ: double; const C: TColor); overload;
begin
  self.ID := QID;
  self.Couleur := C;
  self.Position.setFrom(QX, QY, QZ);
  self.Normale.Empty();
end;

procedure TMNTVertex.AddAtPosition(const QX, QY, QZ: double);
begin
  self.Position.X += QX;
  self.Position.Y += QY;
  self.Position.Z += QZ;
end;

procedure TMNTVertex.AddAtNormale(const QX, QY, QZ: double);
begin
  self.Normale.X += QX;
  self.Normale.Y += QY;
  self.Normale.Z += QZ;
end;

procedure TMNTVertex.Normalize();
var
  WU: Double;
begin
  WU := sqrt(sqr(self.Normale.X) + sqr(self.Normale.Y) + sqr(self.Normale.Z));
  if (not IsZero(WU)) then
  begin
    self.Normale.X := self.Normale.X / WU;
    self.Normale.Y := self.Normale.Y / WU;
    self.Normale.Z := self.Normale.Z / WU;
  end;
end;

function TMNTVertex.MakeLigneForMailleur(const Idx: integer): string;
begin
  Result := Format('%d %.2f %.2f %.2f %d', [Idx, self.Position.X, self.Position.Y, self.Position.Z, 1]);
end;

{ TColorRGBA }


{ TSecteur }

procedure TSecteur.setFrom(const C: TGHTopoColor; const Nom: string);
begin
  self.CouleurSecteur  := C;
  self.NomSecteur      := Nom;
end;

function TSecteur.toLineXTB(const QIdx: integer): string;
begin
  Result := Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_STRING,
                       [-10,
                        QIdx, self.CouleurSecteur.Red, self.CouleurSecteur.Green, self.CouleurSecteur.Blue,
                        self.NomSecteur
                        ]);
end;

{ TReseau }

procedure TReseau.setFrom(const QC: TGHTopoColor; const QT: integer; const QNom, QObs: string);
begin
  self.ColorReseau  := QC;
  self.TypeReseau   := QT;
  self.NomReseau    := QNom;
  self.ObsReseau    := qObs;
end;

function TReseau.toLineXTB(const QIdx: integer): string;
begin
  Result := Format(FORMAT_NB_INTEGER +TAB +FORMAT_NB_INTEGER+TAB+
                   FORMAT_NB_INTEGER +TAB +FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                   FORMAT_NB_INTEGER +TAB +
                   FORMAT_STRING + TAB + FORMAT_STRING,
                       [-8,
                        QIdx,
                        self.ColorReseau.Red, self.ColorReseau.Green, self.ColorReseau.Blue, self.TypeReseau,
                        self.NomReseau, self.ObsReseau
                        ]);
end;



{ TLineAttributes }



procedure TLineAttributes.SetAttributes(const R, G, B, A: byte; const WdthPX: integer; const WdthMM: double);
begin
  self.ColorRGBA.setFrom(R, G, B, A);
  self.LineWidthInPixels      := WdthPX;
  self.LineWidthInMillimeters := WdthMM;
end;

procedure TLineAttributes.SetAttributes(const CC: TColor; const AA: byte; const WdthPX: integer; const WdthMM: double);
begin
  self.ColorRGBA.setFrom(CC);
  self.ColorRGBA.setOpacity(AA);
  self.LineWidthInPixels      := WdthPX;
  self.LineWidthInMillimeters := WdthMM;
end;

procedure TLineAttributes.SetTBGRAPen(const P: TBGRAPen);
begin
  P.Color   := self.ColorRGBA.toTColor();
  P.Opacity := self.getOpacity();
  P.Width   := self.LineWidthInPixels;;
end;

function TLineAttributes.toTColor(): TColor;
begin
  result := RGBToColor(self.ColorRGBA.Red, self.ColorRGBA.Green, self.ColorRGBA.Blue);
end;

procedure TLineAttributes.fromTColor(const C: TColor);
begin
  self.ColorRGBA.setFrom(C);
end;

function TLineAttributes.getOpacity(): byte;
begin
  result := self.ColorRGBA.Alpha;
end;

procedure TLineAttributes.setOpacity(const O: byte);
begin
  self.ColorRGBA.Alpha := O and 255;
end;

{ TViseeAntenne }

procedure TViseeAntenne.setLongAzInc(const L, A, P: double);
begin
  self.Longueur := L;
  self.Azimut   := A;
  self.Pente    := P;
end;

procedure TViseeAntenne.setLongAzInc(const L, A, P: string);
begin
  self.Longueur := ConvertirEnNombreReel(L, 0.00);
  self.Azimut   := ConvertirEnNombreReel(A, 0.00);
  self.Pente    := ConvertirEnNombreReel(P, 0.00);
end;

procedure TViseeAntenne.setFrom(const QEntranceRatt: TNumeroEntrance;
                                const QReseau: TNumeroReseau;
                                const QSecteur: TNumeroSecteur;
                                const QSerieDepart: TNumeroSerie;
                                const QPtDepart: integer;
                                const QL, QAz, QP: double;
                                const Marked: boolean);
begin
  Self.EntranceRatt     := QEntranceRatt;
  Self.Reseau           := QReseau;
  Self.Secteur          := QSecteur;
  Self.SerieDepart      := QSerieDepart;
  Self.PtDepart         := QPtDepart;
  Self.Longueur         := QL;
  Self.Azimut           := QAz;
  Self.Pente            := QP;
  Self.MarkedForDelete  := Marked;
end;

function TViseeAntenne.toLineXTB(const QIdx: integer): string;
begin
  //-9	1	2	1102	10	101	101	GHLMF	12.56	289.00	-14.00	le chat dans la souricière
  result := Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+
                              TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + // série, point
                              TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + // code, expé
                              TAB + FORMAT_STRING +          // ID terrain
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_STRING ,
                             [-9,
                             QIdx,
                             self.Reseau, self.Secteur,
                             self.SerieDepart, self.PtDepart,
                             0, 0, //self.Code, self.Expe,           // Visées en antenne: Code et expés hérité de leur station d'accrochage
                             '', //self.IDTerrainStation,
                             self.Longueur, self.Azimut, self.Pente,
                             ''  //self.Commentaires
                     ]);
end;

function TViseeAntenne.DebugString(): string;
begin
  result := format('L = %.3f, Az = %.3f, P = %.3f', [self.Longueur, self.Azimut, self.Pente]);
end;

{ TPointOfInterest }

function TPointOfInterest.toString(): string;
begin
  Result := Format(FMTSERST, [self.Serie, self.Station]);
end;

{ TBaseStation }

procedure TBaseStation.Empty();
begin
  with self do
  begin
    eCode       := 0;
    eExpe       := 0;
    eSecteur    := 0;
    eReseau     := 0;
    eEntrance   := 0;
    Type_Entite := tgSURFACE; //2
    DateLeve    := Now();
    Entite_Serie    := 0;
    Entite_Station  := 0;
    // drapeau
    Enabled         := false;
    // données originales
    oLongueur       := 0.00;
    oAzimut         := 0.00;
    oPente          := 0.00;
    oLG             := 0.00;
    oLD             := 0.00;
    oHZ             := 0.00;
    oHN             := 0.00;
    // centerline
    PosExtr0.Empty();
    PosStation.Empty();
    // habillage
    PosOPD.Empty();
    PosOPG.Empty();
    PosPD.Empty();
    PosPG.Empty();
    CouleurStd      := clBlue;
    CouleurDegrade  := clWhite;
    IsPOI           := false;
    // commentaires
    IDTerrain       := '';
    oCommentaires   := '';
  end;
end;

function TBaseStation.toString(): string;
begin
  Result := Format(FMTSERST, [self.Entite_Serie, self.Entite_Station]);
end;

function TBaseStation.toStringWithIDTerrain(): string;
begin
  Result := Format(FMTSERSTID, [Self.Entite_Serie, Self.Entite_Station, Self.IDTerrain]);
end;

function TBaseStation.IsInNaturalCave(): boolean;
begin
  Result := (Self.Type_Entite in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON]);
end;

function TBaseStation.IsInCaveOrTunnel(): boolean;
begin
  Result := (Self.Type_Entite in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON, tgTUNNEL, tgMINE]);
end;

function TBaseStation.IsInSerie(): boolean;
begin
  Result := (Self.Type_Entite in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON, tgTUNNEL, tgMINE, tgSURFACE]) AND
            (Self.Entite_Serie > 0);
end;

function TBaseStation.getGHCaveDrawIDPtCenterline(): int64;
begin
  Result := NB_MAXI_SERIES_PAR_CAVITE * Self.Entite_Serie + MULTIPLICATEUR_STATION * Self.Entite_Station;
end;

function TBaseStation.getGHCaveDrawIDPtAntenne(const NbViseesEnAntenne: integer): int64;
begin
  if (self.Type_Entite = tgVISEE_RADIANTE) then  // une protection supplémentaire
  begin
    Result := NB_MAXI_SERIES_PAR_CAVITE * Abs(Self.Entite_Serie) + MULTIPLICATEUR_STATION * Abs(Self.Entite_Station);
    Result := 0 - Result;
  end;
end;

function TBaseStation.getLibelleStationTopo(): string;
var
  QIdxNameSpace, QNoSerie: TNumeroSerie;
  WU, EWE: String;
begin
  result := '';
  if (self.Type_Entite = tgVISEE_RADIANTE) then Exit;
  DecomposeNumeroSerie(self.Entite_Serie, QIdxNameSpace, QNoSerie);

  WU := IIF(QIdxNameSpace = 0, '', Format('@%d', [QIdxNameSpace]));
  EWE := IIF(self.IDTerrain ='', '', ' - ' + self.IDTerrain);
  Result := Format('%d.%d%s%s', [QNoSerie, self.Entite_Station, WU, EWE]);
end;

function TBaseStation.DebugString(): string;
const
  FMT_STATION_1 = '%d.%d - X = %.3f, Y = %.3f, Z = %.3f';
begin
  Format(FMT_STATION_1, [self.Entite_Serie, self.Entite_Station, self.PosStation.X, self.PosStation.Y, self.PosStation.Z]);
end;

function TBaseStation.toTextForQRCode(): string;
begin
  result := Format('%d.%d - X = %s, Y = %s, Z = %s',
                   [self.Entite_Serie, self.Entite_Station,
                    FormatterNombreAvecSepMilliers(self.PosStation.X, 2),
                    FormatterNombreAvecSepMilliers(self.PosStation.Y, 2),
                    FormatterNombreAvecSepMilliers(self.PosStation.Z, 2)
                    ]);
end;

procedure TBaseStation.setLongAzInc(const L, A, P: double);
begin
  self.oLongueur := L;
  self.oAzimut   := A;
  self.oPente    := P;
end;

procedure TBaseStation.setLRUD(const L, R, U, D: double);
begin
  self.oLG := L;
  self.oLD := R;
  self.oHZ := U;
  self.oHN := D;
end;


{ TJonctionXYZ }

function TJonctionXYZ.ToString(): string;
begin
  Result := Format(FMTSERST, [self.NoSer, self.NoSt]);
end;

procedure TJonctionXYZ.setPositionFromXYZ(const QX, QY, QZ: double);
begin
  self.Position.setFrom(QX, QY, QZ);
end;

{ TToporobotIDStation }

procedure TToporobotIDStation.setFrom(const QNameSpace: Integer; const QSerie: TNumeroSerie; const QStation: integer; const QIDTerrain: string);
begin
  self.eIdxNameSpace:= QNameSpace;
  self.aSerie       := QSerie;
  self.aStation     := QStation;
  self.aIDTerrain   := QIDTerrain;
end;

procedure TToporobotIDStation.setFrom(const S: string);
var
  EWE: TGHStringArray;
begin
  self.eIdxNameSpace := 0;
  self.aSerie:= -1;
  self.aStation:= -1;
  if (Pos('.', S) = 0) then Exit;
  EWE := Split(S, '.');
  try
    self.aSerie := StrToIntDef(EWE[0], -1);
    self.aStation := StrToIntDef(EWE[1], -1);
  except
  end;
end;


function TToporobotIDStation.ToString(): string;
begin
  Result := Format(FMTSERST, [self.aSerie, self.aStation]);
end;

//*****************
end.

