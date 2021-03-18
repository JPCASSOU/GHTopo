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



{$INCLUDE CompilationParameters.inc}
interface
uses
  Classes
  , SysUtils
  , Graphics
  , FastGEO
  ;

{$IFDEF MULTI_THREADING}
CONST MULTI_OR_MONO_THREADED = 'Multithreaded';
{$ELSE MULTI_THREADING}
CONST MULTI_OR_MONO_THREADED = 'Monothreaded';
{$ENDIF MULTI_THREADING}



CONST FRM_CONSTRAINT_MAX_WIDTH  = 0;
CONST FRM_CONSTRAINT_MAX_HEIGHT = 0;

CONST
  CRIMINELS_CONTRE_L_HUMANITE = True;
  HITLER     = CRIMINELS_CONTRE_L_HUMANITE;
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

// mot-clé pour infiquer un point d'intérêt
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
type TFTPParameters = record
  HostName: string;
  User    : string;
  Port    : string;
end;
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
const NAMESPACE_COLOR_BY_DEFAULT: TColor = clred;

type TIdxNamespace = type Integer;
type TNameSpace = record
  Couleur     : TColor;
  Nom         : string;
  Description : string;
end;
type TGISLayer = record
  SymboleStyle       : integer; // 0: rien, 1: Cercle, 2: Carré, 3: Etoile, etc ...
  SymbolSize         : double;
  SymbolColor        : TColor;
  SymbolOpacity      : byte;
  LayerVarName       : string;
  LayerTitle         : string;
  LayerDescription   : string;
  LayerAttribution   : string;
end;


// pour support du DistoX
type TDistoX2PacketOfBytes   = array[0..3] of byte;
type TDistoX2MemoryMap       = array[0..65535] of byte;
type TErrorConstruireUneViseeValide = set of (errCVV_NOT_ENOUGHT_NB_SHOTS, errCVV_OUTBOUNDS_LONGUEUR, errCVV_OUTBOUNDS_AZIMUT, errCVV_OUTBOUNDS_PENTE, errCVV_OTHER);

// pour le parcours des graphes
type TModeExplorerGraphe = (mpgEN_LARGEUR, mpgEN_PROFONDEUR);
// pour la recherche de stations topo
type TStationMatchFound = record
  Serie        : integer;
  Station      : Integer;
  Match        : string;
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
type TTexteAttributs = record
  StyleName    : string;
  FontName     : string;
  FontColor    : TColor;
  BackColor    : TColor;
  FontStyle    : TFontStyles;
  HauteurTexte : double;
  Alignement   : byte;  // position du texte; cf organisation d'un clavier numérique pour le point d'accrochage
  AngleRot     : integer;
end;



type TMatrix2x2       = array[1..2, 1..2] of double;
type TMatrix3x3       = array[1..3, 1..3] of double;
type TMatrix          = array of array of double;
type TIncidenceMatrix = array of array of ShortInt;  // seules valeurs: -1, 0, +1

type TPoint3Df = record
  X:  double;
  Y:  double;
  Z:  double;
end;
type TPoint2Df = record
  X:  double;
  Y:  double;
end;
type TPoint2DfTagged = record
  X: double;
  Y: double;
  Tag: integer;
end;
// vertex avec un attribut de tri
// (typiquement utilisé pour la construction de profils topo)
type TPoint3DfOrderedByP = record
  X:  double;
  Y:  double;
  Z:  double;
  P:  double;
end;
type TPointOfConduitTransected = record
  X:  double;
  Y:  double;
  Z:  double;
  P:  double;
  Color  : TColor;
  Caption: string;
end;

type TArrayPoints2Df = array of TPoint2Df;
type TArrayPoints3Df = array of TPoint3Df;

// couleur TOPOROBOT: couple Index - Couleur, évitant un appel de TPalette256
type TToporobotCouleur = record
  Index  : integer;
  Couleur: TColor;
end;

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
type TEntrance    = record
  eNomEntree: string;
  eIDTerrain: string;
  eXEntree  : double;
  eYEntree  : double;
  eZEntree  : double;
  eRefSer   : integer;
  eRefSt    : integer;
  eCouleur  : TColor;
  eObserv   : string;
end;

// réseau ou secteur spéléologique
// Section -8 du fichier xtb
// Une série ne peut faire partie de plusieurs réseaux.

type TReseau = record
  ColorReseau  : TColor;
  TypeReseau   : integer; // type de réseau:
  NomReseau    : string;
  ObsReseau    : string;
end;

// secteurs
type TSecteur = record
  NomSecteur     : string;
  CouleurSecteur : TColor;
end;
// expés
type TModeCalculDeclimag = (cdmAUTOMATIQUE, cdmMANUEL);
type TExpe = record
  IDExpe      : TNumeroExpe;
  {$WARNING: TEXpe.DateExpe à implementer}
  JourExpe    : integer; {$WARNING: Remplacer ces trois champs par un TDateTime}
  MoisExpe    : integer;
  AnneeExpe   : integer;
  DateExpe    : TDateTime;
  Operateur   : String;
  ClubSpeleo  : string;
  ModeDecl    : TModeCalculDeclimag;
  DeclinaisonInDegrees : double;
  IdxCouleur  : TIdxCouleur;
  Commentaire : string;
end;
// fonctions trigonométriques de correction angulaire
type TParamFoncCorrectionAngulaire = record
  Co        : double; // constante d'erreur systématique (e.g: cercle d'un compas bien centré mais avec le zéro non positionné sur le barreau aimanté)
  ErreurMax : double; // erreur maximale
  PosErrMax : double; // Angle (azimut ou pente) correspondant à l'erreur maximale
end;

// codes
type TCode = record
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
end;
// sens de dessin des séries en coupe développée
type TSensTraceCoupeDev = (stcdVERS_DROITE, stcdVERS_GAUCHE);
// visées
// TODO: Fusionnet TUneVisee et TUneStation
type TUneVisee = record
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
    IDTerrainStation: string;                                                           //   IDTerrainStation    : string;
    Commentaires    : string;                                                           //   Commentaire         : string;
    DeltaX    : double;                                                                 //
    DeltaY    : double;                                                                 //
    DeltaZ    : double;                                                                 //
    DeltaP    : double;                                                                 //
    AccroissP : double;                                                                 //
    AccroissZ : double;                                                                 // PtArrivee           : integer;    //        'Arrivée visée
end;
// visées en antenne
// Nota: Les codes et expés d'une visée radiante héritent de ceux de la station d'accrochage
type
  pViseeAntenne = ^TViseeAntenne;
  TViseeAntenne = record
   EntranceRatt        : TNumeroEntrance;
   Reseau              : TNumeroReseau;
   Secteur             : TNumeroSecteur;
   SerieDepart         : TNumeroSerie;
   PtDepart            : integer;
   IDTerrainStation    : string;
   Longueur            : double;      //         'Longueur
   Azimut              : double;      //         'Azimut
   Pente               : double;      //         'Pente
   MarkedForDelete     : boolean;
   Commentaires        : string;
end;
type TViseeAntenneFound = record
  Idx : Int64;
  VA  : TViseeAntenne;
  dx  : double;
  dy  : double;
  dz  : double;
  dp  : double;
  AzimutAbsolu: double;
end;

type TArrayOfTViseeAntenne = array of TViseeAntenneFound;


// couple série station au format TOPOROBOT + ID de terrain
type  TToporobotIDStation = record
  eIdxNameSpace: integer;
  aSerie: TNumeroSerie;
  aStation: integer;
  aIDTerrain: string;
  function ToString(): string;
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
type TBaseStation = record
   // serie et point
   //IDStation         : TIDBaseStation;
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
end;
//*)

// coordonnées WGS84
type TPointWGS84 = record
  Longitude : double;
  Latitude  : double;
end;
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
type TKMLPlaceMark = record
  Latitude  : double;
  Longitude : double;
  Etiquette : string;
end;


type TPolyMode = (tpmENTETE_POLY, tpmSTART_POLY, tpmPOINT_POLY, tpmEND_POLY);
// marqueurs sur la carte
type TMarker = record
  Displayed: boolean;
  X        : double;
  Y        : double;
  Couleur  : TColor;
  Caption  : string;
  //...
end;

// couleurs 32 bits
type TColorRGBA = record
  R: byte;
  G: byte;
  B: byte;
  A: byte;
end;

type TPoint3DVRML = record
  X: Int64;
  Y: Int64;
  Z: Int64;
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
// type rectangle
type TRect2Df = record
  X1: double;
  Y1: double;
  X2: double;
  Y2: double;
end;
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


// procédure d'affichage de la progression d'une opération
type TProcDisplayProgression = procedure (const Etape: string; const Done, Starting, Ending, Step: integer) of object;
//type TProcOfObject = procedure of object;
type TProcOfObjectWithOneBoolParameter    = procedure(const P: boolean) of object;
type TProcOfObjectWithOneIntParameter     = procedure(const P: integer) of object;
type TProcOfObjectWithTNumeroSerie        = procedure(const N: TNumeroSerie) of object;
type TProcOfObjectReturnsAsBoolean        = function(): boolean of object;
type TProcOfObjectWithOneStringParameter  = procedure (const Msg: string) of object;
type TProcOfObjectWithAShot               = procedure (out Horodate: TDateTime; out QLongueur, QAzimut, QPente: double) of object;
// pour éviter un nombre inconsidéré de callbacks utilisant un TBaseStation, on définit une liste d'actions possibles
// enum TTodoAction, enrichissable à volonté
type TTodoAction                          = (tdoAcnBASESTATION_DISPLAY, tdoAcnBASESTATION_EDITSERIE);
type TProcOfObjectWithATBaseStation       = procedure (const BP: TBaseStation; const TodoAction: TTodoAction) of object;



//type TOpenGLColor = array[0..3] of GLFloat;
type TIsoHypse = record
  Cote: double;
  Color: TColor;
end;
type TMaillageVertex = record
  ID         : integer;
  X          : double;
  Y          : double;
  Z          : double;
  NormX      : double;
  NormY      : double;
  NormZ      : double;
  Norme      : double;
end;

// Paramètres EPSG
type TParametresEPSG = record
  CodeEPSG   : integer;
  Parameters : string;
  Comments   : string;
end;

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
  ongDegradeAltMini     : TColor;
  ongDegradeAltMaxi     : TColor;

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
type TMNTModeDessinMaillage = (M3D_NONE, M3D_WIRE_FRAME, M3D_MESH);
type TMNTGridArray          = array of array of Double;
type TMNTTypeMaillage       = (tmUNKNOWN, tmREGULAR_GRID, tmTRIANGLES);
type TMNTBoundingBox  = record
  C1: TPoint3Df;
  C2: TPoint3Df;
end;
type TMNTVertex = record
  ID         : Int64;
  X          : double;
  Y          : double;
  Z          : double;
  NormX      : double;
  NormY      : double;
  NormZ      : double;
  Norme      : double;
  Couleur    : TColor;
  Displayed  : boolean;
end;

type TMNTTriangleABC = record
   Numero     : Int64;
   PointA     : integer;
   PointB     : integer;
   PointC     : integer;
   Displayed  : boolean;
   Couleur    : TColor;
   BoundingBox: TMNTBoundingBox;
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
  ColorCube              : TColor;
  ColorReferentiel       : TColor;
  FillOpacity            : byte;
  ModeRepresentation     : TModeRepresentationGaleries;
  ColorZMini             : TColor;
  ColorZMaxi             : TColor;
  ViseesLargeur          : integer;

  MaillageModeDessin     : TMNTModeDessinMaillage;
  MaillageOpacity        : byte;
  MaillageUseDegrades    : boolean;
  MaillageColorZMini     : TColor;
  MaillageColorZMaxi     : TColor;
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
// coordonnées

type TsvgpsPoint2Df = record
  X : double;
  Y : double;
end;
type TsvgpsArrayPts2Df = array of TsvgpsPoint2Df;

// Fontes pour PostScript
// Pour les coupes developpees
type TPointCoupeDeveloppee = record
  P: double;
  Z: double;
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
   X           : double;
   Y           : double;
   Z           : double;
   NbRefs      : integer;
   IDJonction  : string;
   function ToString(): string;

end;
//*)


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
  DeltaX        : double;
  DeltaY        : double;
  DeltaZ        : double;
  XDepart       : double;
  YDepart       : double;
  ZDepart       : double;
  XArrivee      : double;
  YArrivee      : double;
  ZArrivee      : double;
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
   CouleurTube       : TColor;
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
type TFiltrePersonnalise = record
  NomFiltre      : string;
  CouleurFiltre  : TColor;
  Expression     : string;
  Description    : string;
end;
// Points d'intérêt
type TPOIStatut = (poiUNKNOWN, poiTODO, poiDONE);
type TPointOfInterest = record
  Statut        : TPOIStatut;
  Serie         : TNumeroSerie;
  Station       : integer;
  Coordinates   : TPoint3Df; // pour accélérer certains calculs
  Couleur       : TColor;
  LabelTerrain  : string;
  Description   : string;
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
type TTypeViseeDistoX = (tvdRADIANTE, tvdCHEMINEMENT, tvdMOYENNEE);
type TDistoXSerialNumber     = type word;
type TDistoXVersionFirmware  = type word;
type TDistoXVersionHardware  = type word;

type TMesureViseeDistoX = record
  //TypeViseeDistoX : TTypeViseeDistoX;
  Device          : TDistoXSerialNumber;
  Longueur        : double;
  Azimut          : double;
  Pente           : double;
  TimeStamp       : TDateTime; // horodatage de la visée
end;
type TVectorDataDistoX = record
  X : integer;
  Y : integer;
  Z : integer;
  TimeStamp  : TDateTime; // horodatage de la mesure
end;
type TDataCalibrationDistoX = record
  MX: integer;
  MY: integer;
  MZ: integer;
  GX: integer;
  GY: integer;
  GZ: integer;
  TimeStamp  : TDateTime; // horodatage de la mesure
end;
type TArrayMesureViseeDistoX             = array of TMesureViseeDistoX;
type TProcTransmitMesureDistoX           = procedure (const MV: TMesureViseeDistoX; const TV: TTypeViseeDistoX) of object;

// callback pour transfert de série
type TCallbackGotoErrorSerie       = procedure(const Ser: TNumeroSerie; const St: TNumeroStation) of object;

type TProcOfObjectWithBaseStation  = procedure(const BS: TToporobotIDStation) of object;
type TProcOfObjectWithXY           = procedure(const QX, QY: double; const TagString: string) of object;

// pour le multithread
type TProcOfObjectUsesInteger = procedure(const IDThread: integer; const QStart, QEnd, QDone: integer) of object;
const NB_MAX_THREADS = 2;
// pour le clavier numérique
type TPaveNumModeSaisie = (pnmINTEGER, pnmREAL, pnmTOPOROBOT_STATION);


implementation

{ TJonctionXYZ }

function TJonctionXYZ.ToString(): string;
begin
  Result := Format(FMTSERST, [self.NoSer, self.NoSt]);
end;





{ TToporobotIDStation }

function TToporobotIDStation.ToString(): string;
begin
  Result := Format(FMTSERST, [self.aSerie, self.aStation]);
end;

end.

