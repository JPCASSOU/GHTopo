unit ToporobotClasses2012;
{$INCLUDE CompilationParameters.inc}
//****************************************************
// Projet     : GHTopo
// Modules    : Classe pour gestion des données TOPOROBOT
// Licence    : General Public License - WITHOUT WARRANTY
// Auteur     : JP CASSOU
// OS         : Windows NT - Linux
// Langage    : Free-Pascal 3.0.x sous Lazarus
//-----------------------------------------------------
// Eléments du cahier des charges:
// - Les données sont stockées sous format texte
// - Gestion de la base entièrement en mémoire sous forme d'une arborescence
// - Les listes simples sont des TList dont les membres sont des 'record'.
// - Les séries sont des objets autonomes stockés dans une liste de type TList
//   L'accès aux membres de l'objet se fait uniquement par getters et setters
// - La création de nouveaux items se fait par des fonctions
//   de la forme AddObjet(const MonObjet)
// - Le processus d'édition d'un objet est le suivant:
//   1. Extraction d'un objet (expé, code, entrée, série, antenne, réseau) depuis sa liste
//      via une fonction de la forme GetObjet(const Idx: integer): TMonObjet
//   2. Utilisation de l'objet et modification de ses propriétés
//   3. Mise à jour de l'objet dans sa liste
//      via une fonction de la forme PutObjet(const Idx: integer; const Obj: TMonObjet);
// - Support du format Toporobot TAB en lecture-écriture
// - Support du format XTB ( = format TAB avec sections additionnelles)
// - Lecture des fichiers Text (format considéré comme obsolète et peu robuste.
//                              L'écriture dans ce format ne figure pas dans le CdC)
// - Evolutivité vers un nouveau format de type XML
// - Exportation vers d'autres logiciels
//   * Visual Topo (indispensable) - La méthode TOPOROBOT est incompatible avec la notation alphanumérique
//   * Therion (en cours de validation)
//-----------------------------------------------------
// Module     : ToporobotClasses:
//            : Structure des données d'un dossier Toporobot
// Compatibilité intégrale avec le standard Toporobot 1994.
// 22/08/2012 : Refonte du noyau de GHTopo (ToporobotClasses)
//              en séparant le SGBD et le code de calcul
// 24/08/2012 : Opérationnel
// 14/02/2013 : Réacculturation au code de GHTopo. Petites corrections
//              Préparation de fonctions de recherche plein texte dans les listes
// 03/05/2013 : Réorganisation du fichier de paramètres de compilation
// 13/06/2013 : Quelques modifications dont ExtractFileNameOnly
// 14/06/2013 : Implantation du nouveau format GHTopo XML
// 03/10/2013 : DONE: Implémentation du support des lasermètres-clinomètres type Stanley TLM330
// 04/10/2013 : Conversions UTF8 <> ANSI fixed
// 23/10/2013 : Fonctions de recherche dans les tables
// 24/10/2013 : Support des secteurs, utilisés au niveau visée
// 25/10/2013 : Découplage des listes simples de l'unité ToporobotClasses - Vont dans UnitListesSimples.pas
// 10/11/2013 : Export en CSV au niveau des tables - DONE: Squelette des fonctions OK + En-tête séries/
// 11/11/2013 : Check des données: Longueurs OK, Azimuts OK, Pentes: Clinomètres std et lasermètre TLM330: OK (95% des configurations).
// 15/11/2013 : DONE: Ajout des secteurs dans le format XML
// **** CHANGEMENT DANS LE FORMAT XTB
// 14/02/2014 : Ajout du support de la colonne 'Secteur' de la section AntennaShots
// 30/06/2014 : Harmonisation avec la version Java. L'ajout de nouveaux éléments préparamétrés se fait ici.
// 29/07/2014 : Stockage des composantes des couleurs dans le format XML pour éviter les complications de conversions
// 20/11/2014 : Ajout de la fonction HasStationForAntennaShot()
// 08/12/2014 : Export au format TOPOROBOT Text (pour PocketTopo)
// 16/12/2014 : Fixation d'un bug dans LoadFileText (fonction deprecated)
// 03/02/2015 : Suppression de stations et de leurs visées rayonnantes associées
// 13/02/2015 : Fixation d'un bug dans les chargeurs de fichier (entrée 0 absente = doit être créée)
// 31/03/2015 : Correction dans SaveToTab (section -9)
// 12/05/2015 : Changement de numéro de série met à jour les visées rayonnantes
// 13/05/2015 : Correction dans l'export VTopo
// 18/05/2015 : corrections dans l'export/import Toporobot Text
// 21/05/2015 : Nettoyage des antennes: Suppression des antennes accrochées à un point de la forme nnnn.0
// 29/05/2015 : Correction dans le chargement/sauvegrd XML: Pb de secteurs
// 04/06/2015 : On passe aux génériques pour certaines listes ^^
// 16/07/2015 : Fonctions de correction d'erreur systématique des instruments: OK avec les Grottes du Roy
// 22/02/2016 : Fonction FindPtTopoByCle(): Recherche par occurrence exacte ou texte contenant la clé
// 24/04/2016 : Suppression de PutSerie(); inutile: un TObjSerie est un pointeur, toute modif validée dans un TObjSerie est répercutée dans la base.
// 20/07/2016 : Les étiquettes de terrain et alias de stations topo sont gérés dans une table à part entière
// 25/12/2016 : Tables FTableStylesAnnotations et FTableAnnotations pour les annotations
// 04/08/2017 : Support des notes de version
// 20/09/2018 : Section nouvelle: Filtres personnalisés
// 26/11/2018 : Support des annotations supprimé: inutilisé en pratique; passe sous contrôle de TCroquisTerrain situé dans unitCroquisTerrain.pas
// 29/01/2019 : Toutes les libérations d'objets se font par FreeAndNil(MonObjet) au lieu de MonObjet.Free
// 31/01/2019 : Export vers la version 5.08 de Visual Topo
// 15/02/2019 : Support des directives INCLUDE pour les listes simples et les visées en antenne
// 01/01/2020 : Calcul des LRUD depuis les visées radiantes
// 03/08/2020 : Point de contrôle temporel (contrôle de version)
//
//
//-----------------------------------------------------
//                     Lecture
//                     |  Ajout
//                     |  |  Modif
//                     |  |  |  Suppr
//                     |  |  |  |  Count
//                     |  |  |  |  |  Tri
//                     |  |  |  |  |  |  Recherche
// Table entrées       X  X  X  X  X  0  X
// Table réseaux       X  X  X  X  X  0  X
// Table secteurs      X  X  X  X  X  0  X
// Table expés         X  X  X  X  X  X  X
// Table codes         X  X  X  X  X  X  X
// Table antennes      X  X  X  X  X  0  0
// Table séries        X  X  X  X  X  X  X
//        |-Stations   X  X  X  X  X  0  0
// Table Antennes      X  X  X  X  X  0  0
//
// LoadFromTab()       OK
// LoadFichierText     OK
// SaveToFile()        OK
// LoadFromXML         OK
// SaveToXML           OK
// Export THERION      OK   /!\ A valider, tests OK à raison de la lecture par Loch
// Export Visual Topo: OK   /!\ Visual Topo est très peu tolérant.

// Export Therion: Etat au 05/03/2014:
// - Toporabot OK
// - Charentais OK
// - Saint Marcel OK
// - Shuanghe OK
// TODO: Gestion des projections géographiques
// 10/06/2014: Correction dans le format XML (les éléments 0 ne sont pas exportés et sont reconstruits avant lecture)
// 07/10/2016 : Mise en place d'un callback pour affichage de la progression (refonte de l'interface)
// 13/10/2016 : Fixation d'un crash lié à l'absence de la série numéro 1.
//              Remède: La série est recréée automatiquement avec avertissement.
// 27/07/2017 : Unification des getters et setters pour séries, codes, expés
//              les numéros de série, d'expé, de code, de réseau et de secteur deviennent des types scalaires (entiers)
// 13/06/2019 : Point de contrôle temporel (contrôle de version)
// 24/09/2019 : Dans les visées radiantes, les codes et expés héritent de ceux de la station d'accrochage. Ces champs sont supprimés dans TViseeAntenne
// 09/10/2019 : Ajout de ExportListeAntennesCSV(): Export des visées en antenne au format CSV


interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  UnitListesSimplesWithGeneriques,
  unitobjetserie,

  FileUtil, dateutils,
  UnitClassPalette,
  math,
  Classes,
  SysUtils,
  Graphics,
  UnitTSAGeoMag,
  DOM, XMLWrite, XMLRead;
// REFONTE DU CONTENEUR TTOPOROBOTSTRUCTURE
//------------------------------------------------------------------------------
type

{ TToporobotStructure2012 }

  TToporobotStructure2012 = class
  private
    // callback pour suivi de progression
    FProcDispProgression: TProcDisplayProgression;
    // général
    FDatabaseName             : TStringDirectoryFilename;
    FNomEtude                 : string; // nom du complexe      ; ex: Puts dets Tachous
    FCommentaireEtude         : string;
    FSystemeDeCoordonneesCodeEPSG : TLabelSystemesCoordsEPSG;

    // coordonnées et station de référence
    FPositionPointZero: TPoint3Df;
    FRefSer        : integer;
    FRefPt         : integer;
    // listes simples
    FTableNameSpaces          : TTableNamespaces;  // espaces de noms       ; ex: TP19
    FListeMessagesErreur      : TListeMessagesErreur;
    FTableEntrances           : TTableEntrances;   //TODO: Ajouter un champ 'ID littéral'
    FTableSecteurs            : TTableSecteurs;
    FTableReseaux             : TTableReseaux;
    FTableExpes               : TTableExpes;
    FTableCodes               : TTableCodes;
    FTableViseesAntenne       : TTableViseesAntenne;
    FTableFiltresPersonnalises: TTableFiltresPersonnalises;       // filtres personnalisés
    FTablePointsOfInterest    : TListePointsOfInterest;           // Points d'intérêt

    // liste des IDs terrain
    FTableIDsTerrain   : TTableIDTerrain;

    // liste des séries
    FListeDesSeries : TList;
    // dernier texte cherché
    FLastFindTextWhat      : string;
    // dernières occurrences trouvées
    FLastFoundIdxEntrances : integer;
    FLastFoundIdxReseaux   : integer;
    FLastFoundIdxSecteurs  : integer;
    FLastFoundIdxExpes     : integer;
    FLastFoundIdxCodes     : integer;
    FLastFoundIdxSeries    : integer;

    // série et station courants
    FCurrentNumeroSerie    : TNumeroSerie;
    FCurrentIndexStation   : integer;
    // pour mises en surbrillance
    FCurrentNameSpace      : integer;
    FCurrentNumeroEntrance : integer;
    FCurrentNumeroExpe     : integer;
    FCurrentNumeroCode     : integer;
    FCurrentNumeroReseau   : integer;
    FCurrentNumeroSecteur  : integer;

    function VerifieSerie1(): boolean;
    procedure AddMessageErreur(const M: TMessaqeErreurGHTopoCompiler); overload;
    procedure AddMessageErreur(const T: TTableExaminee; const QCriticite: TCriticiteMessaqeErreurGHTopo; const QIndex: Int64; const QMsg: string); overload;

  public
    constructor Create;
    procedure ReInitialiser(const DoCreateItemsZero: boolean);
    procedure Finaliser();
    function  GetMessageErreur(const Idx: integer): TMessaqeErreurGHTopoCompiler;
    function  GetNbMessagesErreur(): integer;
    procedure SetCurrentNumeroSerie(const S: TNumeroSerie);
    function  GetCurrentNumeroSerie(): TNumeroSerie;
    procedure SetCurrentIndexPointOfSerie(const S: integer);
    procedure SetCurrentIndexSeriePoint(const S, P: integer);
    function  GetCurrentIndexPointOfSerie(): integer;

    // gestion des espaces de noms
    procedure AddNameSpace(const NS: TNameSpace); inline; overload;
    procedure AddNameSpace(const ANom: string; const ACouleur: TColor; const ADescription: string); overload;
    function  GetNameSpace(const NumNameSpace: integer): TNameSpace; inline;
    procedure PutNameSpace(const NoNameSpace: integer; const ANameSpace: TNameSpace); inline;
    function  GetNbNameSpaces(): integer; inline;

    procedure SetCurrentNumeroEntrance(const C: TNumeroEntrance);
    procedure SetCurrentNumeroCode(const C: TNumeroCode);
    procedure SetCurrentNumeroExpe(const C: TNumeroExpe);
    procedure SetCurrentNumeroReseau(const C: TNumeroReseau);
    procedure SetCurrentNumeroSecteur(const C: TNumeroSecteur);
    procedure SetCurrentNumeroNamespace(const C: integer);


    function  GetCurrentNumeroEntrance(): TNumeroEntrance;
    function  GetCurrentNumeroCode(): TNumeroCode;
    function  GetCurrentNumeroExpe(): TNumeroExpe;
    function  GetCurrentNumeroReseau(): TNumeroReseau;
    function  GetCurrentNumeroSecteur(): TNumeroSecteur;
    function  GetCurrentNumeroNamespace(): integer;

    procedure SetProcDisplayProgression(const ProcProgression: TProcDisplayProgression);
    function  GetProcDisplayProgression(): TProcDisplayProgression;
    procedure ViderTablesSimples();
    procedure ViderListeViseesAntennes();
    // s'agit-il d'un document GHTopo étendu ?
    function IsGHTopoCodesInstrumentsExtended(): boolean;
    // chargement document topo
    function  LoadFichierTab(const FichierTAB: TStringDirectoryFilename): integer;
    function  LoadFromXML(const FichierXML: TStringDirectoryFilename): integer;
    function  LoadFromPocketTopoTXT(const FichierTXT: TStringDirectoryFilename): integer; deprecated;
    function  LoadFichierText(const FichierText: TStringDirectoryFilename): integer; deprecated;
    // LoadFromPocketTopoTXT() a été supprimé: non utilisé; PocketTopo exporte en Toporobot Text
    // sauvegarde document
    procedure SaveToFile(const FichierTAB: TStringDirectoryFilename;
                         const ModeSaveTAB: TModeSaveTAB;
                         const TextFileFormat: TTextFileFormat);
    procedure SaveToXML(const FichierXML: TStringDirectoryFilename);
    //procedure SaveToJSON(const Filename: TStringDirectoryFilename);
    // section General
    procedure SetNomEtude(const S: string);
    procedure SetCommentairesEtude(const S: string);
    procedure SetDatabaseName(const S: string);
    function  GetNomEtude(): string;
    function  GetCommentairesEtude(): string;
    procedure SetRefSeriePoint(const Ser, Pt: integer);
    // systèmes de coordonnées
    procedure SetCodeEPSGSystemeCoordonnees(const CodeEPSG: TLabelSystemesCoordsEPSG); overload;
    procedure SetCodeEPSGSystemeCoordonnees(const qCodeEPSG: integer; const qNomEPSG: string); overload;
    function  GetCodeEPSGSystemeCoordonnees(): TLabelSystemesCoordsEPSG;
    // gestion des entrées
    procedure AddEntrance(const AEntrance: TEntrance);
    function  GetEntrance(const NumEntrance: Integer): TEntrance;
    procedure PutEntrance(const NoEntrance: integer; const AEntrance: TEntrance);
    function  RemoveEntrance(const Idx: integer): boolean; inline;
    function  GetNbEntrances(): integer;
    //    function  getNextValidIdxEntrance(): integer;
    function  CreateNewEntrance(): boolean;
    function  GetLastEntrance(): TEntrance;
    function  CalcCentroideEntrees(): TPoint3Df;

    // gestion des réseaux
    procedure AddReseau(const AReseau: TReseau); inline;
    function  GetReseau(const NumReseau: integer): TReseau;
    procedure PutReseau(const NoReseau: integer; const AReseau: TReseau); inline;
    function  RemoveReseau(const Idx: integer): boolean; inline;
    function  GetNbReseaux(): integer; inline;
    function  CreateNouveauReseau(): boolean;
    function  GetLastReseau(): TReseau;
    function  HasNumeroReseau(const Idx: integer): boolean;


    // filtres personnalisés
    procedure AddFiltrePerso(const FiltrePerso: TFiltrePersonnalise); inline;
    function  GetFiltrePerso(const NumFiltrePerso: integer): TFiltrePersonnalise; inline;
    procedure PutFiltrePerso(const NoFiltrePerso: integer; const AFiltrePerso: TFiltrePersonnalise); inline;
    function  RemoveFiltrePerso(const Idx: integer): boolean; inline;
    function  GetNbFiltresPersos(): integer; inline;
    function  GetLastFiltrePerso(): TFiltrePersonnalise;
    procedure ViderListeFiltres();
    // points d'intérêt (POI)
    procedure AddPointOfInterest(const APOI: TPointOfInterest); inline;
    function  GetNbPointsOfInterests(): integer; inline;
    function  GetPointOfInterest(const Idx: integer): TPointOfInterest; inline;
    procedure PutPointOfInterest(const Idx: integer; const APOI: TPointOfInterest); inline;
    function  RemovePointOfInterest(const Idx: integer): boolean; inline;
    procedure RecenserPointsOfInterest();

    // gestion des secteurs
    procedure AddSecteur(const ASecteur: TSecteur); inline;
    function  GetSecteur(const NumSecteur: integer): TSecteur;
    procedure PutSecteur(const NoSecteur: integer; const ASecteur: TSecteur); inline;
    function  RemoveSecteur(const Idx: integer): boolean; inline;
    function  GetNbSecteurs(): integer; inline;
    function  CreateNouveauSecteur()  : boolean;
    function  GetLastSecteur(): TSecteur;
    function  HasNumeroSecteur(const Idx: integer): boolean;

    // gestion des expés
    function  MakeExpe0(const N: TNumeroExpe; const C: string): TExpe;
    procedure AddExpe(const LaExpe: TExpe); inline;
    function  GetExpe(const Idx: integer): TExpe; inline;
    procedure PutExpe(const Idx: integer; const LaExpe: TExpe); inline;
    function  RemoveExpe(const Idx: integer): boolean; inline;
    function  GetExpeByNumero(const NumeroExpe: TNumeroExpe): TExpe; inline;
    function  GetNbExpes(): integer; inline;
    function  CreateNouveauExpe()  : boolean;
    function  GetLastExpe(): TExpe;

    // gestion des codes
    function  MakeCode0(const N: TNumeroExpe; const C: string): TCode;
    procedure AddCode(const LeCode: TCode); inline;
    function  RemoveCode(const Idx: integer): boolean; inline;
    procedure PutCode(const Idx: integer; const LeCode: TCode); inline;
    function  GetCode(const Idx: integer): TCode; inline;
    function  GetCodeByNumero(const NumeroCode: TNumeroCode): TCode; inline;
    function  GetNbCodes(): integer;
    function  CreateNouveauCode(): boolean;
    function  GetLastCode(): TCode;

    // gestion des antennes
    procedure AddViseeAntenne(const VA: TViseeAntenne); inline;
    function  GetViseeAntenne(const No: integer): TViseeAntenne; inline;
    procedure PutViseeAntenne(const No: integer; const VA: TViseeAntenne); inline;
    function  RemoveViseeAntenne(const No: integer): boolean; inline;
    function  RemoveLastViseeAntenne(): boolean; inline;

    function  GetNbAntennes(): integer; inline;
    function  NettoyerAntennes(const longMax: double; const qIdxSerie, qIdxStation: integer): integer;
    function  ExtractViseesAntennesOfStation(const NoSerie: TNumeroSerie;
                                             const NoStation: Integer;
                                             out   ArrAntennesFound: TArrayOfTViseeAntenne): integer;
    // vérifier si la visée en antenne possède une station de rattachement
    // utilise l'IDTerrain, la série et le point
    // /!\ Modifie la visée passée en paramètre
    function  HasStationForAntennaShot(var VA: TViseeAntenne): boolean;
    // OuvrirAntennesDepuisPocketTopo() supprimé: inutilisé
    // gestion des séries
    procedure ClearListeSeries();
    function  CreateNewSerie(const SD, PD: integer; const QNumSerie: TNumeroSerie; const QNomSerie: string): boolean;

    function  GetNbSeries(): integer; inline;
    procedure AddSerie(const S: TObjSerie);
    function  GetSerie(const Idx: integer): TObjSerie;  // /!\ Retourne un POINTEUR sur un objet TObjSerie
    function  GetLastSerie(): TObjSerie;
    function  GetSerieByNumeroSerie(const Idx: TNumeroSerie; out SR: TObjSerie; out InternalIdxSerie: integer): boolean;
    function  FindIndexesSeriesRattachedAtSerie(const Idx: integer; out LS: TListOfIntegers): boolean;
    function  RemoveSerie(const Idx: integer): boolean;
    // prochains index valides
    function  getMaxIdxCode(): TNumeroCode;
    function  getMaxIdxExpe(): TNumeroExpe;
    function  getMaxIdxSerie() : TNumeroSerie;
    function  getMaxIdxSecteur(): integer;
    function  getMaxIdxReseau(): integer;

    // recherche
    function  FindIdxEntranceByText(const S: string): integer;
    function  FindIdxReseauByText(const S: string): integer;
    function  FindIdxSecteurByText(const S: string): integer;
    function  FindIdxCodeByText(const S: string): integer;
    function  FindIdxExpeByText(const S: string): integer;
    function  FindIdxSerieByText(const S: string): integer;

    function  SearchTextInTableSeriesStations(const S: string; out SearchResults: TArrayOfTToporobotIDStation): boolean;

    // procédures de tri
    procedure SortExpes();
    procedure SortCodes();
    procedure SortSeries();
    function  SortAntennesByLengths(): integer;
    function  SortAntennesBySerieStation(): integer;

                                                // coordonnées par défaut
    function  GetDatabaseName: TStringDirectoryFilename;
    procedure SetDefaultCoords(const X, Y, Z: double); overload;
    procedure SetDefaultCoords(const P0: TPoint3Df); overload;
    function  GetPositionDuPointZero(): TPoint3Df;
    // fonctions de recherche
    function FindPtTopoByCle(const DoMatchExact: boolean; var Cle: string; var Sr: TNumeroSerie; var Pt: integer): boolean;
    function HasPtTopoBySerSt(const qSr, qPt: integer): boolean;
    // aller à un index d'enregistrement depuis son numéro (pouvant être différent de l'index)
    function GetIdxEntreeByNumero(const N: TNumeroEntrance): integer; inline; // simple wrapper
    function GetIdxReseauByNumero(const N: TNumeroReseau): integer; inline; // simple wrapper
    function GetIdxSecteurByNumero(const N: TNumeroSecteur): integer; inline; // simple wrapper
    function GetIdxCodeByNumero(const N: TNumeroCode): integer;
    function GetIdxExpeByNumero(const N: TNumeroExpe): integer;
    function GetIdxSerieByNumero(const qIdx: TNumeroSerie): integer;
    function GetRefPoint(): integer;
    function GetRefSerie(): integer;
    // infos générales sur les données de la cavité
    function GetInfosCavite(const LS: TStrings): integer;
    // export vers autres logiciels
    procedure ExportVisualTopo(const FichierTRO: TStringDirectoryFilename; const DoExportViseesRadiantes: boolean);  // Visual Topo
    procedure ExporterVersToporobotTEXT(const QFileName: TStringDirectoryFilename;
                                        const LongueurMaxAntenne: double;
                                        const DoExportAntennaShots: boolean); deprecated;   // Toporobot TEXT
    // génération d'un dossier complet Therion
    procedure GenererThConfig(const THConfig, QFichierTH, QFichierXVI: TStringDirectoryFilename; const QScale: integer; const QGridSize: double);
    procedure GenererDossierTherion(const DossierTherion: TStringDirectoryFilename; const QScale: integer; const QGridSize: double);
    procedure ExportVersTherionTH(const FichierTH: TStringDirectoryFilename);  // Therion .th file
    // test d'existence des éléments
    function  ExistsIdxCode(const Idx: integer): boolean; inline;
    function  ExistsIdxExpe(const Idx: integer): boolean; inline;
    function  ExistsIdxReseau(const Idx: integer): boolean; inline;
    function  ExistsIdxEntrance(const Idx: integer): boolean; inline;

    function  GetLastFindText(): string; inline;
    // export de listes en CSV
    procedure ExportListeEntreesCSV(const FileName: TStringDirectoryFilename);
    procedure ExportListeReseauxCSV(const FileName: TStringDirectoryFilename);
    procedure ExportListeSecteursCSV(const FileName: TStringDirectoryFilename);
    procedure ExportListeCodesCSV(const FileName: TStringDirectoryFilename);
    procedure ExportListeExpesCSV(const FileName: TStringDirectoryFilename);
    procedure ExportListeEntetesSeries(const FileName: TStringDirectoryFilename);
    procedure ExportListePOIToCSV(const FileName: TStringDirectoryFilename);
    procedure ExportListeErreursToCSV(const FileName: TStringDirectoryFilename);
    procedure ExportSeriesCSV(const QFileName: TStringDirectoryFilename);
    procedure ExportListeAntennesCSV(const FileName: TStringDirectoryFilename);

    // check des données
    function  CheckerLesDonneesTopo(): Integer;
    procedure CheckerLesNamespaces();
    procedure CheckerLesEntrees();
    procedure CheckerLesSeries();
    procedure CheckerLesAntennes();
    procedure CheckerLesCodes();
    procedure CheckerLesExpes();
    function  CheckSerie(const MySerie: TObjSerie; const QIdx: integer): integer; // retourne: -1 si série vide; numéro de visée si erreur, 0 si OK
    function  CheckVisee(const V: TUneVisee; out MsgErr: string): TErrorVisee;
    // recalcul des déclinaisons magnétiques
    procedure RecalculerDeclinaisonsMagnetiquesExpes();
    // supprimme toutes les visées rayonnantes d'une station
    function RemoveViseesRayonnantesAssocieesAStation(const qSer, qSt: integer): integer;
    // renommer les points d'origine des visées rayonnantes associées à une station
    function  MoveAntennesToAutreStation(const OldStation, NewStation: TToporobotIDStation; const DoDelete: boolean): integer; overload;
    function  MoveAntennesToAutreStation(const OldSerie: TNumeroSerie; const OldPoint: integer;
                                         const NewSerie: TNumeroSerie; const NewPoint: integer;
                                         const DoDelete: boolean): integer; overload;

    function ReAttribuerViseesRayonnantesDeUneSerie(const SerieCourante: TObjSerie; const OldNumSerie: integer): integer;
    function UpdateSerPtExtremsSeriesRattachees(const OldNumSerie,NewNumSerie: integer): integer;

    // gestion des IDs terrain
    procedure RecenserLesIDsTerrain();
    function  FindIdxIDTerrainByText(const Str: string): integer;
    procedure AddLabelTerrain(const ALabelTerrain: TToporobotIDStation);
    function  GetLabelTerrain(const NumLabelTerrain: integer): TToporobotIDStation;
    function  GetNbLabelTerrain: integer;
    procedure PutLabelTerrain(const NoLabelTerrain: integer; const ALabelTerrain: TToporobotIDStation);
    function  RemoveLabelTerrain(const Idx: integer): boolean;

    // ajout de données depuis fichiers CSV
    procedure AddEntrancesFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
    procedure AddReseauxFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
    procedure AddSecteursFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
    procedure AddCodesFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
    procedure AddExpesFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
    procedure AddAntennesFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);

    // réattribution des codes, réseaux et expés pour les visées en antennes
    function  ReattribuerCodesExpesViseesAntennes(): boolean;
    // fonctions de remplacement d'index
    procedure ReplaceIndexSeriesInEntrances(const OldIndex, NewIndex: TNumeroSerie);
    procedure ReplaceIndexInStations(const OldIndex, NewIndex: integer; const ModeRemplacement: TModeRemplacementStations);
    procedure ReplaceIndexInAntennes(const OldIndex, NewIndex: integer;  const ModeRemplacement: TModeRemplacementAntennes);
    procedure ReplaceIdxCodes(const OldIndex, NewIndex: TNumeroCode);
    procedure ReplaceIdxExpes(const OldIndex, NewIndex: TNumeroExpe);
    procedure ReplaceIdxSeries(const OldIndex, NewIndex: TNumeroSerie);
    // simplifie les visées en antenne d'une station (typiquement lors des scans de parois)
    procedure SimplifyAntennesOfAllReseau(const Tolerance: double);
    procedure SimplifyAntennesOfStation(const QSerie: TNumeroSerie; const QPoint: integer; const Tolerance: double);
    procedure SimplifyAntennesOfSerie(const QSerie: TObjSerie; const Tolerance: double);

    function  PurgerTableAntennes(): integer; inline;
    procedure ResetMarkersToDelete(); inline;
    // calcul des LRUDData d'une station depuis les visées radiantes
    function CalcLRUD_AllStationsOfTheSerie(const MySerie: TObjSerie): boolean;
    function CalcLRUD_AStationOfTheSerie(const MySerie: TObjSerie; const QNumeroStation: integer): boolean;
    function CalcLRUD_AStationOfSerieByNumSerSt(const QNumeroSerie: TNumeroSerie; const QNumeroStation: integer): boolean;
    // recherche de doublons
    function HasDoublonsInNumsSeries(const NS: TNumeroSerie; out QInternalIdx: integer): boolean;
end;
//------------------------------------------------------------------
function RegenererEbaucheXTB(const NomEbauche: string; const DefaultCodeEPSG: integer): boolean;


implementation
uses
    DGCDummyUnit; // Anti-erreur 'Fin du conde source non trouvée'
// Inclusion des constantes GtxKey
{$INCLUDE GTX_Keys.inc}

const FMTSTS = 's%d_%d';

// Régénération d'un stub topo
function RegenererEbaucheXTB(const NomEbauche: string; const DefaultCodeEPSG: integer): boolean;
const
  QFMT_SERIE    = FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                  FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                  FORMAT_NB_INTEGER + TAB +
                  FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                  FORMAT_STRING + TAB +
                  FORMAT_STRING + TAB +
                  FORMAT_NB_INTEGER + TAB + FORMAT_NB_REAL_2_DEC;
  QLINE_STATION = FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                  FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                  FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                  FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                  FORMAT_STRING + TAB +
                  FORMAT_STRING + TAB +
                  FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER;

var
  fp: TextFile;
  MyExpe: TExpe;
  MyCode: TCode;
  procedure WriteLigne(const L: string);
  begin
    WriteLn(fp, L);
  end;
  procedure WriteVisee(const QIdxSerie, QNoSt, QCode, QExpe: integer;
                       const QLong, QAz, QPente, QLG, QLD, QHZ, QHN: double;
                       const QCommentaire, QIDTerrain: string;
                       const QTypeVisee, QIDSecteur: integer);
  begin
    WriteLigne(Format(QLINE_STATION,
                                  [QIdxSerie, QNoSt,
                                   QCode, QExpe,
                                   QLong, QAz, QPente,
                                   QLG, QLD, QHZ, QHN,
                                   QCommentaire,
                                   QIDTerrain,
                                   QTypeVisee,
                                   QIDSecteur
                                  ]));
  end;
begin
  result := false;
  AfficherMessage(Format('RegenererEbaucheXTB: %s', [NomEbauche]));
  //MyExpe :=
  AssignFile(fp, NomEbauche);
  try
    ReWrite(fp);
    WriteLigne('-20' + #9 + DatePascalToDateSQL(Now()) + #9 + TimeToStr(Now()) + #9 + 'CaveStub.xtb');
    WriteLigne('-19' + #9 + 'Ebauche de document - Document stub');
    WriteLigne('-15' + #9 + format(FORMAT_NB_INTEGER, [DefaultCodeEPSG]));
    WriteLigne('-6'  + #9 + '1' + #9 + 'Entree principale');
    WriteLigne('-5'  + #9 + '1' + #9 + '0.00' + #9 + '0.00' + #9 + '0.00' + #9 + '1' + #9 + '0');
    MyExpe := MakeTExpe(1, YearOf(Now), MonthOf(Now), DayOf(Now), 39, cdmAUTOMATIQUE, 0.00, 'Team1', 'Team1', 'Expe01');
    WriteLigne(MakeToporobotTabLineOfExpe(-2, MyExpe));
    MyCode := MakeUsualTCode(1, UNITE_ANGULAIRE_DU_CODE_ZERO, UNITE_ANGULAIRE_DU_CODE_ZERO, 1.00, 0.01, 1.00, 1.00, 0.00, 0.00, 0.00, 'Code01');
    WriteLigne(MakeToporobotTabLineOfCode(-1, mtabEXTENDEDTAB, MyCode));
    //WriteLigne('-2'  + #9 + '1' + #9 + '12' + #9 + '11' + #9 + '2014' + #9 + 'Equipe1' + #9 + 'Equipe1' + #9 + '0' + #9 + '0.00' + #9 + '0' + #9 + '39');
    //WriteLigne('-1'  + #9 + '1' + #9 + QDefaultAnglesUnitsStr + #9 + QDefaultAnglesUnitsStr + #9 + '0.01' + #9 + '1.00' + #9 + '1.00' + #9 + '0.00' + #9 + '0.00' + #9 + 'Code01' + #9 + '0');
    //1	-1	1	0	1	2	2	0	0	Serie principale	A modifier	0	0.01
    WriteLigne(Format(QFMT_SERIE, [1, -1,
                                   1, 0, 1, 2,
                                   2,
                                   0, 0,
                                   'Serie principale', '', 0, 0.01]));

    WriteVisee(1, 0, 1, 1, 0.01, 135.00, -45.00, 0.02, 0.02, 0.02, 0.02, 'Point 1.0', '1.0', 0, 0);
    WriteVisee(1, 1, 1, 1, 0.05, 135.00, -45.00, 0.02, 0.02, 0.02, 0.02, '', '1.1', 0, 0);
    WriteVisee(1, 2, 1, 1, 0.05, 135.00, -45.00, 0.02, 0.02, 0.02, 0.02, '', '1.2', 0, 0);
    result := true;
  finally
    CloseFile(fp);
  end;
end;

// Routines internes non incorporées dans les objets
function SortCodesCriteria(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TCode;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.IDCode < E2.IDCode) then      Result := -1
  else if (E1.IDCode = E2.IDCode) then Result :=  0
  else                                 Result :=  1;
end;
// Trier les expés par ordre croissant
function SortExpesCriteria(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TExpe;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.IDExpe < E2.IDExpe) then      Result := -1
  else if (E1.IDExpe = E2.IDExpe) then Result :=  0
  else                                 Result :=  1;
end;
// Trier les séries par ordre croissant
function SortSerieCriteria(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TObjSerie;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.GetNumeroDeSerie() < E2.GetNumeroDeSerie()) then       Result := -1
  else if (E1.GetNumeroDeSerie() = E2.GetNumeroDeSerie()) then  Result :=  0
  else                                                          Result :=  1;
end;

// Trier les antennes par ordre de longueur croissante
function SortViseeAntenneCriteriaLength(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TViseeAntenne;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.Longueur < E2.Longueur) then       Result := -1
  else if (E1.Longueur = E2.Longueur) then  Result :=  0
  else                                      Result :=  1;
end;
function SortViseeAntenneCriteriaSerSt(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TViseeAntenne;
  QSerStE1, QSerStE2: Int64;
begin
  E1:=Item1;
  E2:=Item2;
  QSerStE1 := NB_MAXI_SERIES_PAR_CAVITE * Integer(E1.SerieDepart) + E1.PtDepart;
  QSerStE2 := NB_MAXI_SERIES_PAR_CAVITE * Integer(E2.SerieDepart) + E2.PtDepart;
  if (QSerStE1 < QSerStE2) then       Result := -1
  else if (QSerStE1 = QSerStE2) then  Result :=  0
  else                                Result :=  1;
end;

// TToporobotStructure2012
constructor TToporobotStructure2012.Create;//(AOwner: TObject);
begin
  inherited Create;
  // current
  FCurrentNameSpace     := 0;
  FCurrentNumeroEntrance:= 0;
  FCurrentNumeroSerie   := 0;
  FCurrentIndexStation  := 0;
  FCurrentNumeroExpe    := 0;
  FCurrentNumeroCode    := 0;
  FCurrentNumeroReseau  := 0;
  FCurrentNumeroSecteur := 0;
  // point zéro
  FPositionPointZero := MakeTPoint3Df(0.00, 0.00, 0.00);
  AfficherMessage(Format('%s.Create', [self.ClassName]));
  FListeDesSeries := TList.Create;
  FListeDesSeries.Clear;
  // tables simples
  FTableNameSpaces            := TTableNamespaces.Create;
  FListeMessagesErreur        := TListeMessagesErreur.Create;
  FTableEntrances             := TTableEntrances.Create;
  FTableReseaux               := TTableReseaux.Create;
  FTableSecteurs              := TTableSecteurs.Create;
  FTableExpes                 := TTableExpes.Create;
  FTableCodes                 := TTableCodes.Create;
  FTableViseesAntenne         := TTableViseesAntenne.Create;
  FTableIDsTerrain            := TTableIDTerrain.Create;
  FTableFiltresPersonnalises  := TTableFiltresPersonnalises.Create;
  FTablePointsOfInterest      := TListePointsOfInterest.Create;
  AfficherMessage('-- Tables created');
end;



procedure TToporobotStructure2012.ReInitialiser(const DoCreateItemsZero: boolean);
var
  UneEntree       : TEntrance;
  UneSerie        : TObjSerie;
begin
  ClearListeSeries();
  ViderTablesSimples();
  // numéros d'items courants
  FCurrentNumeroSerie    := 0;
  FCurrentNumeroExpe     := 0;
  FCurrentNumeroCode     := 0;
  FCurrentNumeroReseau   := 0;
  FCurrentNumeroSecteur  := 0;
  // dernières occurrences trouvées
  FLastFindTextWhat      := '';
  FLastFoundIdxEntrances := -1;
  FLastFoundIdxReseaux   := -1;
  FLastFoundIdxSecteurs  := -1;
  FLastFoundIdxExpes     := -1;
  FLastFoundIdxCodes     := -1;
  FLastFoundIdxSeries    := -1;
  if (Not DoCreateItemsZero) then Exit;
  // Général
  SetNomEtude('SansNom');
  SetCodeEPSGSystemeCoordonnees(DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG, DEFAULT_SYSTEME_COORDONNEES_NOM);
  SetCommentairesEtude('');
  // Coordonnées du point zéro = celles de l'entrée principale
  SetDefaultCoords(0.00, 0.00, 0.00);
  if (self.GetNbEntrances() > 0) then
  begin
    UneEntree := GetEntrance(0);
    SetDefaultCoords(UneEntree.eXEntree, UneEntree.eYEntree, UneEntree.eZEntree);
  end
  else
  begin
    CreateNewEntrance();
  end;

  AddReseau(MakeTReseau(COULEUR_RESEAU_0, 0, rsMAIN_NETWORK, '')); // Réseau 0
  AddSecteur(MakeTSecteur(clSilver, 'Undefined'));                 // Secteur 0
  AddExpe(MakeExpe0(0, 'Expe0'));                                  // Expé 0
  AddCode(MakeCode0(0, 'Code0'));                                  // Code 0
  // série 0
  UneSerie := TObjSerie.Create;
  try
    UneSerie.ClearStations();                                  // Opération 1: Vider la liste des visées
    UneSerie.SetChanceObstacle(0,0);                           // Opération 2: Chances et obstacles
    UneSerie.SetSeriePtExtremites(0,0,1,0);                    // Opération 3: Extrémités de la série
    UneSerie.SetNumeroSerie(0);                                // Opération 4: Numéro de série
    UneSerie.SetNomObsSerie(rsMSG_SERIE_INITIALISATION, '');   // Opération 5: Nom et observations sur la série
    UneSerie.SetRaideur(1.00);                                 // Opération 6: Raideur
    UneSerie.SetCouleur(clBlack);                              // Opération 7: Couleur
    uneserie.SetNumeroEntrance(0);                             // Opération 8: Entrée de rattachement
    UneSerie.SetNumeroReseau(0);                               // Opération 9: Réseau de rattachement
    // Opération 10: Traitement des visées
    UneSerie.AddVisee(EmptyVisee('Point 1.0'));
    AddSerie(UneSerie);
  except
  end;
end;


procedure TToporobotStructure2012.ViderTablesSimples();
begin
  AfficherMessage(Format('%s.Vidage des tables simples', [Classname]));
  try
    FTableNameSpaces.ClearListe();
    FListeMessagesErreur.ClearListe();
    FTableReseaux.ClearListe();
    FTableSecteurs.ClearListe();
    FTableCodes.ClearListe();
    FTableEntrances.ClearListe();
    FTableExpes.ClearListe();
    FTableViseesAntenne.ClearListe();
    FTableIDsTerrain.ClearListe();
    FTableFiltresPersonnalises.ClearListe();
    FTablePointsOfInterest.ClearListe();
  except
    AfficherMessage('Echec purge des tables simples');
  end;
end;
procedure TToporobotStructure2012.ViderListeViseesAntennes();
begin
  FTableViseesAntenne.ClearListe();
end;

// A invoquer avant libération
// SIGSEGV inexpliqué si implanté dans le destructeur
procedure TToporobotStructure2012.Finaliser();
begin
  try
    AfficherMessageErreur('001');
    ViderTablesSimples();
    AfficherMessageErreur('002');
    AfficherMessage('Finaliser ViderTablesSimples()');
    ClearListeSeries();
    AfficherMessageErreur('Fuck the Christ 003');
  finally
    FreeAndNil(FListeDesSeries);//FListeDesSeries.Free;
    AfficherMessageErreur('004');
    // libération des tables simples
    FreeAndNil(FTableNameSpaces);
    FreeAndNil(FListeMessagesErreur);//FListeMessagesErreur.Free;
    FreeAndNil(FTableEntrances);//FTableEntrees.Free;
    FreeAndNil(FTableReseaux);//FTableReseaux.Free;
    FreeAndNil(FTableSecteurs);//FTableSecteurs.Free;
    FreeAndNil(FTableExpes);//FTableExpes.Free;
    AfficherMessageErreur('010');
    FreeAndNil(FTableCodes);//FTableCodes.Free;
    FreeAndNil(FTableViseesAntenne);//FTableViseesAntenne.Free;
     AfficherMessageErreur('011');
    FreeAndNil(FTableIDsTerrain);//FTableIDsTerrain.Free;
     AfficherMessageErreur('012');
    FreeAndNil(FTableFiltresPersonnalises);//FTableFiltresPersonnalises.Free;
     AfficherMessageErreur('013');
    FreeAndNil(FTablePointsOfInterest);//FTablePointsOfInterest.Free;
    AfficherMessageErreur('020');
  end;
end;


function TToporobotStructure2012.GetPositionDuPointZero(): TPoint3Df;
begin
  Result := FPositionPointZero;
end;
procedure TToporobotStructure2012.SetDefaultCoords(const X, Y, Z: double); overload;
begin
  FPositionPointZero := MakeTPoint3Df(X, Y, Z);
end;

procedure TToporobotStructure2012.SetDefaultCoords(const P0: TPoint3Df); overload;
begin
  FPositionPointZero := P0;
end;

function TToporobotStructure2012.GetRefSerie(): integer;
begin
  Result := FRefSer;
end;

function TToporobotStructure2012.GetRefPoint(): integer;
begin
  Result := FRefPt;
end;

procedure TToporobotStructure2012.SetRefSeriePoint(const Ser, Pt: integer);
begin
  FRefSer := Ser;
  FRefPt  := Pt;
end;

procedure TToporobotStructure2012.SetNomEtude(const S: string);
begin
  FNomEtude := S;
end;


procedure TToporobotStructure2012.SetProcDisplayProgression(const ProcProgression: TProcDisplayProgression);
begin
  FProcDispProgression := ProcProgression;
end;
function TToporobotStructure2012.GetProcDisplayProgression(): TProcDisplayProgression;
begin
  Result := FProcDispProgression;
end;

procedure TToporobotStructure2012.SetCommentairesEtude(const S: string);
begin
  FCommentaireEtude := S;
end;

//**************************
// espaces de noms
procedure TToporobotStructure2012.AddNameSpace(const NS: TNameSpace);
begin
  FTableNameSpaces.AddElement(NS);
end;
procedure TToporobotStructure2012.AddNameSpace(const ANom: string; const ACouleur: TColor; const ADescription: string); overload;
var
  NS: TNameSpace;
begin
  NS.Nom         := ANom;
  NS.Description := ADescription;
  NS.Couleur     := ACouleur;
  FTableNameSpaces.AddElement(NS);
end;

function TToporobotStructure2012.GetNameSpace(const NumNameSpace: integer): TNameSpace;
begin
  result := FTableNameSpaces.GetElement(NumNameSpace);
end;

procedure TToporobotStructure2012.PutNameSpace(const NoNameSpace: integer; const ANameSpace: TNameSpace);
begin
  FTableNameSpaces.PutElement(NoNameSpace, ANameSpace);
end;

function TToporobotStructure2012.GetNbNameSpaces(): integer;
begin
  result := FTableNameSpaces.GetNbElements();
end;

// la station topo courante
procedure TToporobotStructure2012.SetCurrentIndexPointOfSerie(const S: integer);
var
  SR: TObjSerie;
  QInternalIdxSerie: integer;
begin
  if (S = -1) then // -1 = on attrape la dernière station de la série courante
  begin
    GetSerieByNumeroSerie(FCurrentNumeroSerie, SR, QInternalIdxSerie);
    FCurrentIndexStation := SR.GetNbVisees() - 1;
  end
  else
  begin
    FCurrentIndexStation := S;
  end;
end;

procedure TToporobotStructure2012.SetCurrentNumeroSerie(const S: TNumeroSerie);
begin
  FCurrentNumeroSerie := S;
end;

procedure TToporobotStructure2012.SetCurrentIndexSeriePoint(const S, P: integer);
begin
  self.SetCurrentNumeroSerie(S);
  self.SetCurrentIndexPointOfSerie(P);
end;

procedure TToporobotStructure2012.SetCurrentNumeroCode(const C: TNumeroCode);
begin
  FCurrentNumeroCode := C;
end;

procedure TToporobotStructure2012.SetCurrentNumeroExpe(const C: TNumeroExpe);
begin
  FCurrentNumeroExpe := C;
end;

procedure TToporobotStructure2012.SetCurrentNumeroReseau(const C: TNumeroReseau);
begin
  FCurrentNumeroReseau := C;
end;

procedure TToporobotStructure2012.SetCurrentNumeroSecteur(const C: TNumeroSecteur);
begin
  FCurrentNumeroSecteur := C;
end;

procedure TToporobotStructure2012.SetCurrentNumeroNamespace(const C: integer);
begin
  FCurrentNameSpace := C;
end;

function TToporobotStructure2012.GetCurrentNumeroEntrance(): TNumeroEntrance;
begin
  Result := FCurrentNumeroEntrance;
end;

function TToporobotStructure2012.GetCurrentIndexPointOfSerie(): integer;
begin
  Result := FCurrentIndexStation;
end;

procedure TToporobotStructure2012.SetCurrentNumeroEntrance(const C: TNumeroEntrance);
begin
  FCurrentNumeroEntrance := C;
end;

function TToporobotStructure2012.GetCurrentNumeroCode(): TNumeroCode;
begin
  Result := FCurrentNumeroCode;
end;

function TToporobotStructure2012.GetCurrentNumeroExpe(): TNumeroExpe;
begin
  Result := FCurrentNumeroExpe;
end;

function TToporobotStructure2012.GetCurrentNumeroReseau(): TNumeroReseau;
begin
  Result := FCurrentNumeroReseau;
end;

function TToporobotStructure2012.GetCurrentNumeroSecteur(): TNumeroSecteur;
begin
  Result := FCurrentNumeroSecteur;
end;

function TToporobotStructure2012.GetCurrentNumeroNamespace(): integer;
begin
  Result := FCurrentNameSpace;
end;

function TToporobotStructure2012.GetCurrentNumeroSerie(): TNumeroSerie;
begin
  Result := FCurrentNumeroSerie;
end;

//******************************************************************************
function TToporobotStructure2012.GetNomEtude(): string;
begin
  Result := FNomEtude;
end;


function TToporobotStructure2012.GetCommentairesEtude(): string;
begin
  Result := FCommentaireEtude;
end;
procedure TToporobotStructure2012.SetDatabaseName(const S: string);
begin
  FDatabaseName:=S;
end;


function TToporobotStructure2012.GetDatabaseName: TStringDirectoryFilename;
begin
  Result := FDatabaseName;
end;
//**************************
// système de coordonnées EPSG
function TToporobotStructure2012.GetCodeEPSGSystemeCoordonnees(): TLabelSystemesCoordsEPSG;
begin
  Result := FSystemeDeCoordonneesCodeEPSG;
end;

procedure TToporobotStructure2012.SetCodeEPSGSystemeCoordonnees(const CodeEPSG: TLabelSystemesCoordsEPSG); overload;
begin
  FSystemeDeCoordonneesCodeEPSG := CodeEPSG;
end;

procedure TToporobotStructure2012.SetCodeEPSGSystemeCoordonnees(const qCodeEPSG: integer; const qNomEPSG: string); overload;
begin
  FSystemeDeCoordonneesCodeEPSG.CodeEPSG := qCodeEPSG;
  FSystemeDeCoordonneesCodeEPSG.NomEPSG  := qNomEPSG;
end;

procedure TToporobotStructure2012.ClearListeSeries();
var
  ii, Nb: Integer;
  OO: TObjSerie;
begin
  Nb := GetNbSeries();
  AfficherMessage(Format('-- %s.ClearListeSeries(%d)',[self.ClassName, Nb]));

  if (0 = Nb) then exit;
  for ii := 0 to FListeDesSeries.Count - 1 do
  begin
    // cette construction n'est pas valide:
    // et elle est inutile: un objet est un POINTEUR
    // OO := TObjSerie(FListeDesSeries.Items[ii]);
    // OO.Free;
    // La simple libération par Dispose suffit
    OO := GetSerie(ii);
    AfficherMessageErreur(inttostr(ii));
    OO.ClearStations();
    //FreeAndNil(OO);
    Dispose(FListeDesSeries.Items[ii]);
  end;
  FListeDesSeries.Clear;
end;
function TToporobotStructure2012.CreateNewSerie(const SD, PD: integer;
                                                const QNumSerie: TNumeroSerie;
                                                const QNomSerie: string): boolean;
var
  nb, QIdx: Integer;
  SR: TObjSerie;
  NSR, QSR: TObjSerie;
begin
  Result := False;
  NSR := TObjSerie.Create;
  try
    NSR.ClearStations();          // Opération 1: Vider la liste des visées
    NSR.SetChanceObstacle(0, 0);  // Opération 2: Chances et obstacles
    // Opération 4: Numéro de série
    nb := GetNbSeries;
    SR := GetSerie(nb - 1);
    if      (GetSerieByNumeroSerie(QNumSerie, QSR, QIdx)) then NSR.SetNumeroSerie(1 + getMaxIdxSerie())
    else if (QNumSerie < 0) then  NSR.SetNumeroSerie(1 + getMaxIdxSerie())
    else     NSR.SetNumeroSerie(QNumSerie);

    NSR.SetSeriePtExtremites(SD, PD, NSR.GetNumeroDeSerie(), 0);  // Opération 3: Extrémités de la série
    NSR.SetNomObsSerie(QNomSerie, '');  // Opération 5: Nom et observations sur la série
    NSR.SetRaideur(1.00);                            // Opération 6: Raideur
    NSR.SetCouleur(SR.GetCouleur);                   // Opération 7: Couleur
    NSR.SetNumeroEntrance(SR.GetNumeroEntrance());   // Opération 8: Entrée de rattachement
    NSR.SetNumeroReseau(SR.GetNumeroReseau());       // Opération 9: Réseau de rattachement
    // Opération 10: Traitement des visées
    NSR.AddVisee(0, 1, 1,
                tgDEFAULT,
                0.001, 0.00, 0.00,
                0.00, 0.00, 0.00, 0.00,
                '', '');
    self.AddSerie(NSR);
    result := True;
  except
    FreeAndNil(NSR);
  end;
end;
function TToporobotStructure2012.GetNbSeries(): integer;
begin
  Result := FListeDesSeries.Count;
end;

procedure TToporobotStructure2012.AddSerie(const S: TObjSerie);
var
  pS: ^TObjSerie;
begin
  New(pS);
  pS^ := S;
  FListeDesSeries.Add(pS);
end;

// /!\ Retourne un POINTEUR sur un objet TObjSerie
// i.e: Toutes les modifications apportées à l'objet retourné par cette fonction
//      sont validées
// Rappel: En Pascal, un TObject EST un pointeur
function TToporobotStructure2012.GetSerie(const Idx: integer): TObjSerie;
var
  pS: ^TObjSerie;
begin
  pS := FListeDesSeries.Items[Idx];
  Result := pS^;
end;
//*)
function TToporobotStructure2012.GetSerieByNumeroSerie(const Idx: TNumeroSerie; out SR: TObjSerie; out InternalIdxSerie: integer): boolean;
var
  i, Nb: Integer;
begin
  Result := false;
  Nb := GetNbSeries;
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    SR := GetSerie(i);
    InternalIdxSerie := i;
    if (Idx = SR.GetNumeroDeSerie()) then Exit(True);
  end;
end;



function TToporobotStructure2012.RemoveSerie(const Idx: integer): boolean;
var
  MySerie: TObjSerie;
  Nb     : Integer;
begin
  Result := False;
  Nb := GetNbSeries();
  try
    try
      MySerie := self.GetSerie(Idx);
      //if (MySerie.GetNumeroDeSerie() < 25000) then
      //begin
      MySerie.ClearStations();
      FreeAndNil(MySerie);//MySerie.Free;
      //end;
      Dispose(FListeDesSeries.Items[Idx]);
      Result := True;
    except
      AfficherMessageErreur(format('%s.RemoveSerie(%d): ****', [ClassName, Idx]));
    end;
  finally
    FListeDesSeries.Delete(Idx);
  end;
end;

function TToporobotStructure2012.GetLastSerie(): TObjSerie;
begin
  Result := GetSerie(FListeDesSeries.Count - 1);
end;

// fonctions de recherche
function TToporobotStructure2012.FindIdxEntranceByText(const S: string): integer;
var
  i, N, P : integer;
  EWE: TEntrance;
begin
  Result := -1;
  N := GetNbEntrances() - 1;
  P := IIF((FLastFoundIdxEntrances = -1), 0, FLastFoundIdxEntrances);
  for i := P to N do
  begin
    EWE := GetEntrance(i);
    if (Pos(LowerCase(S), LowerCase(EWE.eNomEntree + ' ' + EWE.eObserv)) > 0) then
    begin
      FLastFindTextWhat       := S;                                       // mémorise la clé pour recherche de l'occurrence suivante
      FLastFoundIdxEntrances  := i + 1;                                   // mémorise l'indice de parcours
      if (FLastFoundIdxEntrances > N) then FLastFoundIdxEntrances := -1;  // on boucle depuis le début de la liste
      Result := i;
      Exit;
    end;
  end;
  if (Result = -1) then FLastFoundIdxEntrances := -1;
end;
function TToporobotStructure2012.FindIdxReseauByText(const S: string): integer;
var
  i, N, P : integer;
  EWE: TReseau;
begin
  Result := -1;
  N := GetNbReseaux() - 1;
  P := IIF((FLastFoundIdxReseaux = -1), 0, FLastFoundIdxReseaux);
  for i := P to N do
  begin
    EWE := GetReseau(i);
    if (Pos(LowerCase(S), LowerCase(EWE.NomReseau + ' ' + EWE.ObsReseau)) > 0) then
    begin
      FLastFindTextWhat    := S;                                          // mémorise la clé pour recherche de l'occurrence suivante
      FLastFoundIdxReseaux := i + 1;                                      // mémorise l'indice de parcours
      if (FLastFoundIdxReseaux > N) then FLastFoundIdxReseaux := -1;      // on boucle depuis le début de la liste
      exit(i);
    end;
  end;
  if (Result = -1) then FLastFoundIdxReseaux := -1;
end;

function TToporobotStructure2012.FindIdxSecteurByText(const S: string): integer;
var
  i, N, P : integer;
  EWE: TSecteur;
begin
  Result := -1;
  N := GetNbSecteurs() - 1;
  P := IIF((FLastFoundIdxSecteurs = -1), 0, FLastFoundIdxSecteurs);
  for i := P to N do
  begin
    EWE := GetSecteur(i);
    if (Pos(LowerCase(S), LowerCase(EWE.NomSecteur)) > 0) then
    begin
      FLastFindTextWhat     := S;                                       // mémorise la clé pour recherche de l'occurrence suivante
      FLastFoundIdxSecteurs := i + 1;                                   // mémorise l'indice de parcours
      if (FLastFoundIdxSecteurs > N) then FLastFoundIdxSecteurs := -1;  // on boucle depuis le début de la liste
      exit(i);
    end;
  end;
  if (Result = -1) then FLastFoundIdxSecteurs := -1;
end;


function TToporobotStructure2012.FindIdxExpeByText(const S: string): integer;
var
  i, N, P : integer;
  EWE     : TExpe;
  zdar    : String;
  Q       : String;
begin
  Result := -1;
  N := GetNbExpes() - 1;
  P := IIF((FLastFoundIdxExpes = -1), 0, FLastFoundIdxExpes);
  for i := P to N do
  begin
    EWE := GetExpe(i);
    // DONE: Recherche sur date, équipe et commentaire à implémenter
    {$WARNING: TEXpe.DateExpe à implementer}
    Q := DateToStr(GetSecuredDate(EWE.AnneeExpe, EWE.MoisExpe, EWE.JourExpe));
    zdar := Format('%s %s  %s  %s', [Q, EWE.Operateur, EWE.ClubSpeleo, EWE.Commentaire]);
    if (Pos(LowerCase(S), LowerCase(zdar)) > 0) then
    begin
      FLastFindTextWhat   := S;                                       // mémorise la clé pour recherche de l'occurrence suivante
      FLastFoundIdxExpes  := i + 1;                                   // mémorise l'indice de parcours
      if (FLastFoundIdxExpes > N) then FLastFoundIdxExpes := -1;      // on boucle depuis le début de la liste
      exit(i);
    end;
  end;
  if (Result = -1) then FLastFoundIdxExpes := -1;
end;
function TToporobotStructure2012.FindIdxCodeByText(const S: string): integer;
var
  i, N, P : integer;
  EWE     : TCode;
begin
  Result := -1;
  N := GetNbCodes() - 1;
  P := IIF((FLastFoundIdxCodes = -1), 0, FLastFoundIdxCodes);
  for i := P to N do
  begin
    EWE := GetCode(i);
    if (Pos(LowerCase(S), LowerCase(EWE.Commentaire)) > 0) then
    begin
      FLastFindTextWhat  := S;                                         // mémorise la clé pour recherche de l'occurrence suivante
      FLastFoundIdxCodes := i + 1;                                     // mémorise l'indice de parcours
      if (FLastFoundIdxCodes > N) then FLastFoundIdxCodes := -1;       // on boucle depuis le début de la liste
      exit(i);
    end;
  end;
  if (Result = -1) then FLastFoundIdxCodes := -1;
end;
function TToporobotStructure2012.FindIdxSerieByText(const S: string): integer;
var
  EWE : TObjSerie;
  i, N, P: Integer;
  QIdx, IdxAChercher: LongInt;
begin
  Result := -1;
  // on fait une recherche sur le numéro de série
  QIdx := 0;
  IdxAChercher := StrToIntDef(trim(S), -1);
  if (GetSerieByNumeroSerie(IdxAChercher, EWE, QIdx)) then Exit(QIdx);
  N := GetNbSeries - 1;
  P := IIF((FLastFoundIdxSeries = -1), 0, FLastFoundIdxSeries);
  for i := P to N do
  begin
    EWE := GetSerie(i);
    if (Pos(LowerCase(S), LowerCase(EWE.GetNomSerie)) > 0) then
    begin
      FLastFindTextWhat   := S;                                      // mémorise la clé pour recherche de l'occurrence suivante
      FLastFoundIdxSeries := i + 1;                                  // mémorise l'indice de parcours
      if (FLastFoundIdxSeries > N) then FLastFoundIdxSeries := -1;   // on boucle depuis le début de la liste
      exit(i);
    end;
  end;
  if (Result = -1) then FLastFoundIdxSeries := -1;
end;

function TToporobotStructure2012.SearchTextInTableSeriesStations(const S: string; out SearchResults: TArrayOfTToporobotIDStation ): boolean;
const FMT_MATCH_FOUND = '%d.%d: %s';
var
  QTextToFind: String;
  QResultsCount, i, NbS, j: Integer;
  SR: TObjSerie;
  ST: TUneVisee;
  function HasOccurrenceOf(const QS: string): boolean;
  var
    EWE: String;
  begin
    EWE := lowercase(trim(QS));
    result := (pos(QTextToFind, EWE) > 0);
    if (result) then
    begin
      Inc(QResultsCount);
    end;
  end;
  procedure IncreaseResultsArray(const QSer: TNumeroSerie; const QSt: TNumeroStation; const QIDTerrain: string);
  var
    n: Integer;
  begin
    n := length(SearchResults);
    SetLength(SearchResults, n + 1);
    n := High(SearchResults);
    SearchResults[n].aSerie        := QSer;
    SearchResults[n].aStation      := QSt;
    SearchResults[n].eIdxNameSpace := 0;
    SearchResults[n].aIDTerrain    := QIDTerrain;
  end;
begin
  QResultsCount := 0;
  result := false;
  SetLength(SearchResults, 0);
  QTextToFind := lowercase(S);
  AfficherMessageErreur(Format('%s.SearchTextInTableSeriesStations: %s', [ClassName, S]));
  AfficherMessageErreur('========== Search results ===========');
  NbS := GetNbSeries();
  for i := 0 to NbS - 1 do
  begin
    SR := GetSerie(i);
    if (HasOccurrenceOf(SR.GetNomSerie() + SR.GetObsSerie())) then AfficherMessageErreur(Format(FMT_MATCH_FOUND, [SR.GetNumeroDeSerie(), 0, SR.GetNomSerie()]));
    for j := 1 to SR.GetNbVisees() - 1 do
    begin
      ST := SR.GetVisee(j);
      if (HasOccurrenceOf(ST.IDTerrainStation + ST.Commentaires)) then
      begin
        IncreaseResultsArray(SR.GetNumeroDeSerie(), j, ST.IDTerrainStation);
        AfficherMessageErreur(Format(FMT_MATCH_FOUND, [SR.GetNumeroDeSerie(), j, ST.IDTerrainStation + ST.Commentaires]));
      end;
    end;
  end;
  AfficherMessageErreur('=====================================');
  AfficherMessageErreur(format('%d matches found', [QResultsCount]));
end;

// fonctions de tri
procedure TToporobotStructure2012.SortExpes();
begin
  AfficherMessage(Format('%s.SortExpes: Tri des seances',[ClassName]));
  FTableExpes.Sort(SortExpesCriteria);
end;

procedure TToporobotStructure2012.SortCodes();
begin
  AfficherMessage(Format('%s.SortCodes: Tri des codes',[ClassName]));
  FTableCodes.Sort(SortCodesCriteria);
end;
// Ne pas utiliser ici le Sort intégré dans le TList de la série
// TList.Sort travaille sur toute la liste, or il faut trier ici entre 1 et n-1b uniquement.
procedure TToporobotStructure2012.SortSeries();
  procedure QSortSeries(lidx, ridx: integer);
  var
    k, e, mid: integer;
    QSer1, QSer2: TObjSerie;

  begin
    if (lidx >= ridx) then Exit;
    mid := (lidx + ridx) div 2;
    FListeDesSeries.Exchange(lidx, ridx);
    e:=lidx;
    for k:=lidx+1 to ridx do
    begin
      QSer1 := GetSerie(k);
      QSer2 := GetSerie(lidx);
      if (QSer1.GetNumeroDeSerie() < QSer2.GetNumeroDeSerie())  then
      begin
        Inc(e);
        FListeDesSeries.Exchange(e, k);
      end;
    end;
    FListeDesSeries.Exchange(lidx, e);
    QSortSeries(lidx, e-1);
    QSortSeries(e+1, ridx);
  end;
begin
  AfficherMessage(Format('%s.SortSeries: Tri des series',[ClassName]));
  QSortSeries(1, GetNbSeries() - 1);
end;

function  TToporobotStructure2012.SortAntennesByLengths(): integer;
begin
  Result := -1;
  try
    AfficherMessage(Format('%s.SortAntennesByLengths: Tri des antennes',[ClassName]));
    FTableViseesAntenne.Sort(SortViseeAntenneCriteriaLength());
    result := FTableViseesAntenne.GetNbElements();
  except
  end;
end;

function  TToporobotStructure2012.SortAntennesBySerieStation(): integer;
begin
  Result := -1;
  try
    AfficherMessage(Format('%s.SortAntennesBySerieStation: Tri des antennes',[ClassName]));
    FTableViseesAntenne.Sort(SortViseeAntenneCriteriaSerSt());
    result := FTableViseesAntenne.GetNbElements();
  except
  end;
end;

// charger depuis fichier Tab ou XTB
// /!\ Ne pas faire les conversions en UTF8 ici:
// Le format interne de GHTopo est ANSI pour compatibilité avec Windows.
// Les directives INCLUDE ne fonctionnent pas pour les séries
function TToporobotStructure2012.LoadFichierTab(const FichierTAB: TStringDirectoryFilename): integer;
var
  EPSGDefault : TLabelSystemesCoordsEPSG;
  MyFiltre    : TFiltrePersonnalise;
  NoLigneTAB, i: integer;
  yyy, mmm, ddd: Word;
  // variables de lignes
  LigneTab: String;
  PrmsLn, QArrStrg: TGHStringArray;
  ErrMsg, QStr: String;
  // liste provisoire pour lecture de la section -6
  ProvListeEntrees: TStringList;
  // série
  UneSerie: TObjSerie;
  NbErrorsInLoading: Integer;
  blaireau, QN, QFichierCorrect: Integer;
  MyNameSpace: TNameSpace;
  UneEntree: TEntrance;

  procedure WriteWarning(const Msg: string);
  begin
    AfficherMessageErreur(Msg);
  end;
  // Vérifie s'il y a une instruction INCLUDE
  // (qui doit être impérativement en début de ligne)
  // et retourne Vrai si OK en passant le nom de fichier
  // Le fichier à inclure doit être dans le même dossier que le fichier principal
  function HasInstructionInclude(const QMyLigne: string; out QFichierInclude: TStringDirectoryFilename): boolean;
  const
    DIRECTIVE_INCLUDE = '$INCLUDE:';
  var
    P, Q: SizeInt;
    EWE: String;
    QMyDir: TStringDirectoryFilename;
  begin
    Result := False;
    if (1 = Pos(DIRECTIVE_INCLUDE, QMyLigne)) then
    begin
      AfficherMessageErreur('HasInstructionInclude: ' + QMyLigne);
      // extraction du chemin
      QMyDir := ExtractFilePath(FichierTAB);
      // extraction du nom du fichier
      P := Pos(':', QMyLigne);
      Q := Length(QMyLigne) - P + 1;
      EWE := Trim(Copy(QMyLigne, P+1, Q));
      // assemblage du chemin et du nom de fichier spécifié
      QFichierInclude := QMyDir + EWE;
      AfficherMessageErreur(QFichierInclude);
      // et on vérifie si le fichier existe
      Result := FileExists(QFichierInclude);
    end;
  end;

  // Procédure récursive de lecture
  // Retourne 0 si aucune erreur
  //         -1 si échec de lecture du fichier ppal
  //         -2 si fichier INCLUDE introuvable
  //         -3 si échec de lecture du fichier INCLUDE
  function LireUnFichier(const QFilename: string): integer;
  var
    pTAB                : TextFile;
    QFileIsToporobotTabStrict: boolean;
    Ex: Integer;
    Prefix1, Prefix2    : Integer;
    QMonFichierAInclure: TStringDirectoryFilename;
    qVisee : TUneVisee;
    // items de tables simples
    UneEntree: TEntrance;
    UnReseau : TReseau;
    UnSecteur: TSecteur;
    UneExpe  : TExpe;
    UnCode   : TCode;
    UneViseeAntenne: TViseeAntenne;
    MyNameSpace: TNameSpace;
    // il y a des séries ?
    //HasSeries: boolean;
    function LireLigne: string;
    var
      Lign: string;
    begin
      ReadLn(pTab, Lign);
      Inc(NoLigneTAB);
      Result:= Lign;// PurgerAccents(Lign);
    end;
  begin
    Result := -1;
    QFileIsToporobotTabStrict := Pos('tab', LowerCase(ExtractFileExt(FichierTAB))) > 0;
    AssignFile(pTAB, QFilename);
    try
      ReSet(pTAB);
      while Not Eof(pTAB) do
      begin
        try // traitement local d'exceptions dans la lecture des lignes
          // réinitialisation du message d'erreur par défaut
          ErrMsg  := rsRD_TAB_MSG_ERR;
          Prefix1 := 0;
          LigneTab := LireLigne;
          if (HasInstructionInclude(LigneTab, QMonFichierAInclure)) then
          begin
            Result := LireUnFichier(QMonFichierAInclure);
          end;
          if (Trim(LigneTab)='')    then Prefix1 := -100  // lignes vides
          else if (LigneTab[1]='#') then Prefix1 := -1000 // commentaires sur une ligne
          else if LigneTab[1]='{' then    // commentaires sur plusieurs lignes
          begin
            AfficherMessage(Format(rsRD_TAB_D_MULTI_OBS,[NoLigneTAB]));
            while Not (LigneTab[1]='}') do LigneTab := LireLigne; /// PurgerAccents(LireLigne);
            AfficherMessage(Format(rsRD_TAB_F_MULTI_OBS,[NoLigneTAB]));
          end
          else begin
            PrmsLn   := split(LigneTab, SEPARATOR_TAB);
            Prefix1  := StrToIntDef(PrmsLn[0], -110);
          end;
          // routage selon le préfixe
          case Prefix1 of
            -9999: Break; // Arrêt forcé du traitement (utiliser avec précaution)
            -1000: AfficherMessage(Format(rsRD_TAB_LN_OBS,[NoLigneTab]));   // commentaire
             -900: ; // balise de pause
             -110: ; // Ligne invalide; ignorée
             -100: ; // ignorer les lignes vides
              -20: AfficherMessage(Format(rsRD_TAB_LASTSAVES,[PrmsLn[1], PrmsLn[2]])); // horodatage
              -19: begin
                     self.SetNomEtude(Trim(PrmsLn[1])); // Nouvelle section: Nom de l'étude
                     self.SetCommentairesEtude(Trim(PrmsLn[2]));
                   end;

              -18: begin // Nouvelle section: Espace de noms (pour les synthèses)
                     //00  01 02       03       04     05 06
                     //-18	0	255 	0 	0 	Pont_de_Gerbaut	Gouffre du Pont de Gerbaut
                     MyNameSpace.Couleur := RGBToColor(StrToIntDef(Trim(PrmsLn[2]), 255),
                                                       StrToIntDef(Trim(PrmsLn[3]), 0),
                                                       StrToIntDef(Trim(PrmsLn[4]), 0));
                     MyNameSpace.Nom          := Trim(PrmsLn[5]);
                     MyNameSpace.Description  := Trim(PrmsLn[6]);
                     AfficherMessageErreur('Namespace trouvé:' + MyNameSpace.Nom);
                     self.AddNameSpace(MyNameSpace);
                   end;
              -16: begin // Nouvelle section: Filtres personnalisés

                //#### #-16	IDFiltre	NomFiltre	CouleurFiltre_R	CouleurFiltre_G	CouleurFiltre_B	FiltreExpr	Description
                     //-16	0	Aquatopo	0	128	0	SERIE=227
                     MyFiltre.CouleurFiltre :=  RGBToColor(StrToIntDef(PrmsLn[2], 0),
                                                           StrToIntDef(PrmsLn[3], 0),
                                                           StrToIntDef(PrmsLn[4], 0));
                     MyFiltre.NomFiltre     := Trim(PrmsLn[5]);
                     MyFiltre.Expression    := Trim(PrmsLn[6]);
                     MyFiltre.Description   := Trim(PrmsLn[7]);
                     AddFiltrePerso(MyFiltre);  //*)

                   end;
              -15: begin // Nouvelle section: Système de coordonnées :
                     EPSGDefault.CodeEPSG := StrToIntDef(Trim(PrmsLn[1]), DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);
                     EPSGDefault.NomEPSG  := '';
                     SetCodeEPSGSystemeCoordonnees(EPSGDefault);
                   end;
              -10: begin // Nouvelle section: secteurs
                     UnSecteur.CouleurSecteur := RGBToColor(StrToIntDef(PrmsLn[2], 0),
                                                 StrToIntDef(PrmsLn[3], 0),
                                                 StrToIntDef(PrmsLn[4], 0));
                     UnSecteur.NomSecteur     := Trim(PrmsLn[5]);
                     AddSecteur(UnSecteur);
                   end;
               -9: begin  // visées en antenne
                     UneViseeAntenne.Reseau           := StrToIntDef(PrmsLn[2], 0);
                     UneViseeAntenne.Secteur          := StrToIntDef(PrmsLn[3], 0);
                     UneViseeAntenne.SerieDepart      := StrToIntDef(PrmsLn[4], 0);
                     UneViseeAntenne.PtDepart         := StrToIntDef(PrmsLn[5], 0);
                     UneViseeAntenne.Longueur         := ConvertirEnNombreReel(PrmsLn[9], 0.00);
                     UneViseeAntenne.Azimut           := ConvertirEnNombreReel(PrmsLn[10], 0.00);
                     UneViseeAntenne.Pente            := ConvertirEnNombreReel(PrmsLn[11], 0.00);
                     UneViseeAntenne.MarkedForDelete  := False;
                     AddViseeAntenne(UneViseeAntenne);
                   end;
               -8: begin  // réseaux
                     UnReseau.ColorReseau := RGBToColor(StrToIntDef(PrmsLn[2], 0),
                                                        StrToIntDef(PrmsLn[3], 0),
                                                        StrToIntDef(PrmsLn[4], 0));
                     UnReseau.TypeReseau  := StrToIntDef(PrmsLn[5], 0);
                     UnReseau.NomReseau   := Trim(PrmsLn[6]);
                     UnReseau.ObsReseau   := Trim(PrmsLn[7]);
                     AddReseau(UnReseau);
                   end;
               -7: ; // Classeurs (désactivé)
               -6: begin  // Section -6: Entrée
                     ProvListeEntrees.Add(PrmsLn[2] + '|' + PrmsLn[3] + '|' + PrmsLn[10] );
                   end;
               -5: begin  // Entrées
                     //nombre d'entrées nul = on définit l'entrée par défaut
                     if (0 = GetNbEntrances()) then
                     begin
                       SetDefaultCoords(ConvertirEnNombreReel(PrmsLn[2], 0.00),
                                        ConvertirEnNombreReel(PrmsLn[3], 0.00),
                                        ConvertirEnNombreReel(PrmsLn[4], 0.00));
                       SetRefSeriePoint(StrToIntDef(PrmsLn[5],1), StrtoIntDef(PrmsLn[6], 0));
                     end;
                     //Commentaires:=PrmsLn[7];
                     // Ajouter les entrées
                     // le nombre d'entrées retenu est celui décompté dans
                     // la section -5
                     //eNumEntree:= GetNbEntrees + 1;
                     // On ajoute l'entrée récupérée en -6
                     try
                       QN := GetNbEntrances();
                       QStr := ProvListeEntrees.Strings[QN];
                       QArrStrg   := Split(QStr, '|');
                       UneEntree.eNomEntree := Trim(QArrStrg[0]);
                       UneEntree.eIDTerrain := Trim(QArrStrg[1]);
                       UneEntree.eCouleur   := GHTopoColorToColorDef(Trim(QArrStrg[2]), clGray);
                       ///eCouleur   := Col
                     except
                       AfficherMessage(Format(rsWARNINGENTRYADDED,[QN]));
                       UneEntree.eNomEntree := Format(rsRD_TAB_ENTRANCE,[QN]);
                       UneEntree.eCouleur   := clRed;
                     end;
                     UneEntree.eXEntree  := ConvertirEnNombreReel(PrmsLn[2], 0.00);
                     UneEntree.eYEntree  := ConvertirEnNombreReel(PrmsLn[3], 0.00);
                     UneEntree.eZEntree  := ConvertirEnNombreReel(PrmsLn[4], 0.00);


                     //------------
                     // entrées non géoréférencées ?
                     if (UneEntree.eXEntree < 100.00) or (UneEntree.eYEntree < 100.00) or (UneEntree.eZEntree < 500.00)
                     then WriteWarning(Format(GetResourceString(rsRD_TAB_ENTR_NOGEOREF), [NoLigneTAB, QN, UneEntree.eNomEntree]));
                     UneEntree.eRefSer   := StrToIntDef(PrmsLn[5],1);
                     UneEntree.eRefSt    := StrtoInt(PrmsLn[6]);
                     if ((UneEntree.eRefSer < 1) or (UneEntree.eRefSt < 0)) then
                        WriteWarning(Format(rsRD_TAB_ENTR_BADLINK, [NoLigneTAB, QN, UneEntree.eNomEntree, UneEntree.eRefSer, UneEntree.eRefSt]));
                     UneEntree.eObserv   := PrmsLn[7];
                     AddEntrance(UneEntree);
                   end;
               -4: pass;
               -3: pass;
               -2: begin // Expés
                     UneExpe.IDExpe := StrToInt(PrmsLn[1]);
                     if (UneExpe.IDExpe <= 0) then
                     begin
                        ErrMsg:=rsRD_TAB_BAD_TRIP;
                        raise Exception.Create(ErrMsg);
                     end;
                     {$WARNING: TEXpe.DateExpe à implementer - }
                     UneExpe.JourExpe      := StrToIntDef(PrmsLn[2],1);
                     UneExpe.MoisExpe      := StrToIntDef(PrmsLn[3],1);
                     UneExpe.AnneeExpe     := StrToIntDef(PrmsLn[4],2000);
                     //UneExpe.DateExpe      := GetSecuredDate(StrToIntDef(PrmsLn[4],2000),
                     //                                        StrToIntDef(PrmsLn[3],1),
                     //                                        StrToIntDef(PrmsLn[2],1));

                     if (UneExpe.JourExpe=0) or (UneExpe.MoisExpe=0) or (UneExpe.AnneeExpe=0) then
                     begin
                       WriteWarning(Format(rsRD_TAB_BAD_DATE,
                                          [NoLigneTAB]));
                       DecodeDate(Now(), yyy, mmm, ddd);
                       UneExpe.JourExpe := ddd;
                       UneExpe.MoisExpe := mmm;
                       UneExpe.AnneeExpe:= yyy;
                     end;
                     {$WARNING: End TEXpe.DateExpe à implementer}
                     UneExpe.Operateur     := PrmsLn[5];             // spéléomètre
                     UneExpe.ClubSpeleo    := PrmsLn[6];             // spéléographe
                     {$WARNING A revoir et vérifier. Si c'est un TAB pur, calcul de la déclimag en automatique}
                     if (QFileIsToporobotTabStrict) then
                     begin
                       UneExpe.ModeDecl              :=  cdmAUTOMATIQUE;   // déclinaison auto ?
                       UneExpe.DeclinaisonInDegrees  := -ConvertirEnNombreReel(PrmsLn[8], 0.00); // déclinaison Toporobot = -déclinaison VisualTopo/GHTopo
                     end else
                     begin
                       UneExpe.ModeDecl      := TModeCalculDeclimag(StrToIntDef(PrmsLn[7], 1));   // déclinaison auto ?
                       UneExpe.DeclinaisonInDegrees  := ConvertirEnNombreReel(PrmsLn[8], 0.00); // déclinaison
                     end;
                     //Inclinaison   := ConvertirEnNombreReel(PrmsLn[9], 0.00); // correction clino x10
                     UneExpe.IdxCouleur    := StrToInt(PrmsLn[10]);  // couleur
                     UneExpe.Commentaire   := PrmsLn[11];            // commentaire
                     AddExpe(UneExpe);
                   end;
               -1: begin // Codes
                     UnCode.IDCode     := StrToInt(PrmsLn[1]);    // ID Code
                     if (UnCode.IDCode <= 0) then
                     begin
                        //WriteWarning(Format('WARNING ! (%d) - Numéro de Code incorrect (Valeur: %d) - Mis à %d',[NoLigneTAB, IDCode, FNbCodes]));
                        ErrMsg:=rsRD_TAB_BAD_CODE;
                        raise Exception.Create(ErrMsg);
                     end;
                     UnCode.GradAz     := ConvertirEnNombreReel(PrmsLn[2], UNITE_ANGULAIRE_DU_CODE_ZERO);  // unité boussole
                     UnCode.GradInc    := ConvertirEnNombreReel(PrmsLn[3], UNITE_ANGULAIRE_DU_CODE_ZERO);  // unite  CLINO
                     UnCode.PsiL       := ConvertirEnNombreReel(PrmsLn[4], 0.01);  // precision longueur
                     UnCode.PsiAz      := ConvertirEnNombreReel(PrmsLn[5], 0.1);  // precision azimut
                     UnCode.PsiP       := ConvertirEnNombreReel(PrmsLn[6], 0.1);  // precision pente
                     {$WARNING: Support de FactLong a valider}
                     UnCode.FactLong   := 1.00;
                     //UnCode.FactLong   := ConvertirEnNombreReel(PrmsLn[7], 1.00)/100;  // Facteur des longueurs
                     UnCode.AngLimite  := ConvertirEnNombreReel(PrmsLn[8], 0.00);  // angle limite
                     UnCode.Commentaire := PrmsLn[9];              // commentaire
                     //ReservedInt := StrToIntDef(PrmsLn[10],0);
                     UnCode.ParamsFuncCorrAz  := MakeTParamFoncCorrectionAngulaire(ConvertirEnNombreReel(PrmsLn[11], 0.00),
                                                                                   ConvertirEnNombreReel(PrmsLn[12], 0.00),
                                                                                   ConvertirEnNombreReel(PrmsLn[13], 0.00));
                     UnCode.ParamsFuncCorrInc := MakeTParamFoncCorrectionAngulaire(ConvertirEnNombreReel(PrmsLn[14], 0.00),
                                                                                   ConvertirEnNombreReel(PrmsLn[15], 0.00),
                                                                                   ConvertirEnNombreReel(PrmsLn[16], 0.00));
                     UnCode.ErreurTourillon   := ConvertirEnNombreReel(PrmsLn[17], 0.00);
                     // provisoire: Diamètre des boules-cibles
                     UnCode.DiametreBoule1 := 0.00;
                     UnCode.DiametreBoule2 := 0.00;
                     AddCode(UnCode);
                   end;
               0: ; // pas de section 0
             // end
           otherwise  // on est dans les séries !
             // Si le préfixe 2 (2e colonne) =-1 =>nouvelle série
             Prefix1 := StrToInt(PrmsLn[0]);
             if (Prefix1 > 0) then
             begin
               Prefix2 := StrToInt(PrmsLn[1]);
               if (Prefix2 = -1) then
               begin
                 if (Prefix1 = 1) then    // si c'est la première série, on crée
                 begin
                   UneSerie := TObjSerie.Create;
                   UneSerie.ClearStations();
                 end
                 else  // sinon on ferme la série courante et on crée la suivante
                 begin
                   self.AddSerie(UneSerie);
                   UneSerie := TObjSerie.Create;
                 end;
                 UneSerie.SetNumeroSerie(TNumeroSerie(Prefix1));
                 UneSerie.SetSeriePtExtremites(StrToInt(PrmsLn[2]),
                                               StrToInt(PrmsLn[3]),
                                               StrToInt(PrmsLn[4]),
                                               StrToInt(PrmsLn[5]));
                 // NbPoints: Non utilisé
                 UneSerie.SetChanceObstacle(StrToInt(PrmsLn[7]), StrToInt(PrmsLn[8]));
                 UneSerie.SetNomSerie(PrmsLn[9]);
                 UneSerie.SetObsSerie(PrmsLn[10]);
                 UneSerie.SetNumeroReseau(StrToIntDef(PrmsLn[11],0));
                 UneSerie.SetRaideur(ConvertirEnNombreReel(Prmsln[12], 1.02));        // raideur de la série
                 UneSerie.SetNumeroEntrance(StrToIntDef(Prmsln[13], 0));               // entrée de rattachement
               end
               else
               begin
                 qVisee.TypeVisee   := tgDEFAULT;
                 qVisee.Code        := StrToInt(PrmsLn[2]);
                 qVisee.Expe        := StrToInt(PrmsLn[3]);
                 qVisee.Longueur    := ConvertirEnNombreReel(PrmsLn[4], 0.00);
                 if (qVisee.Longueur < 0.00) then
                 begin
                   WriteWarning(Format(rsRD_TAB_NEG_LONG, [NoLigneTAB, qVisee.Longueur]));
                   qVisee.Longueur := Abs(qVisee.Longueur);
                 end;
                 qVisee.Azimut      := ConvertirEnNombreReel(PrmsLn[5], 0.00);
                 qVisee.Pente       := ConvertirEnNombreReel(PrmsLn[6], 0.00);
                 qVisee.LG          := ConvertirEnNombreReel(PrmsLn[7], 0.00);  //LG
                 qVisee.LD          := ConvertirEnNombreReel(PrmsLn[8], 0.00);  //LD
                 qVisee.HZ          := ConvertirEnNombreReel(PrmsLn[9], 0.00);
                 qVisee.HN          := ConvertirEnNombreReel(PrmsLn[10], 0.00);
                 qVisee.Commentaires:= PrmsLn[11];
                 qVisee.IDTerrainStation := FormatterIDTerrainStation(PrmsLn[12]);
                 blaireau    := StrToIntDef(PrmsLn[13], 0);
                 qVisee.TypeVisee   := GetTypeDeVisee(blaireau); // galerie fossile supposée
                 qVisee.IDSecteur   := StrToIntDef(PrmsLn[14], 0);
                 UneSerie.AddVisee(qVisee);
               end;
             end   // if Prefix2
           end; // case Prefix1

           Result := 0;
        except
          Inc(NbErrorsInLoading);
          WriteWarning('');
          WriteWarning(Format(rsRD_ERROR_LN,[NoLigneTab, ErrMsg]));
          WriteWarning(Format(rsRD_CONTNT_LN,[LigneTab]));
          WriteWarning(rsRD_TAB_FIELD_VALUES);
          for Ex := 0 to High(PrmsLn) do
          begin
            WriteWarning(Format(' PrmsLn[%.2d] = "%s"',[Ex, PrmsLn[Ex]]));
            WriteWarning('');
          end;
        end;
      end; // while Not Eof(pTAB) do begin
    finally
      CloseFile(pTAB);
    end;
  end;
begin
  Result := -1;
  AfficherMemoryUsage();
  AfficherMessage(Format('%s.LoadFichierTAB(%s)',[ClassName, FichierTAB]));

  // exemple d'affichage à l'exécution du numero de la ligne (utile pour déboguer)
  //AfficherMessage(  {$INCLUDE %FILE%} );
  //AfficherMessage(  {$INCLUDE %LINE%} );
  // nom étude par défaut
  SetNomEtude('Etude001');
  SetCommentairesEtude('');

  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;
  SetCodeEPSGSystemeCoordonnees(EPSGDefault);
  ReInitialiser(True);
  NoLigneTAB := 0;
  NbErrorsInLoading := 0;

  ProvListeEntrees := TStringList.Create;
  ProvListeEntrees.Clear;
  // Lecture du fichier
  QFichierCorrect := LireUnFichier(FichierTAB);
  // On ferme la dernière série
  self.AddSerie(UneSerie);
  // Espaces de noms: s'il y en a aucun, on en crée un par défaut
  if (0 = GetNbNameSpaces()) then
  begin
    AfficherMessage('Aucun espace de noms trouvé - Création d''un namespace par défaut');
    AddNameSpace(NAMESPACE_NAME_BY_DEFAULT, NAMESPACE_COLOR_BY_DEFAULT, NAMESPACE_DESC_BY_DEFAULT);
  end;
  AfficherMessageErreur(Format('%d namespaces', [GetNbNameSpaces()]));
  for i := 0 to GetNbNameSpaces() - 1 do
  begin
    MyNameSpace := GetNameSpace(i);
    AfficherMessageErreur(Format('%d: %s', [i, MyNameSpace.Nom]));
  end;
  //**************************
  self.SetDatabaseName(ExtractFileNameWithoutExt(FichierTAB));
  VerifieSerie1();                // Cause courante de plantages: Absence de la série 1
  CheckerLesDonneesTopo();
  RecenserLesIDsTerrain();
  RecenserPointsOfInterest();
  SortCodes();
  SortExpes();
  //SortSeries(); TODO  Trier les séries ?
  UneEntree := GetEntrance(0);
  self.SetRefSeriePoint(UneEntree.eRefSer, UneEntree.eRefSt);
  self.SetDefaultCoords(UneEntree.eXEntree, UneEntree.eYEntree, UneEntree.eZEntree);
  //****************************
  Result := self.GetNbSeries();

  // suppression d la liste provisoire des entrées
  try
    ProvListeEntrees.Clear;
  finally
    FreeAndNil(ProvListeEntrees);
  end;
  AfficherMemoryUsage();
end;

function TToporobotStructure2012.VerifieSerie1(): boolean;
var
  QSR, MySerie: TObjSerie;
  i, nb, QIdx: integer;
begin
  result := false;
  AfficherMessage(Format('%s.VerifieSerie1', [ClassName]));
  AfficherMessageErreur(Format('Entre dans %s.VerifieSerie1: %d séries', [ClassName, GetNbSeries()]));
  if (GetSerieByNumeroSerie(1, QSR, QIdx)) then
  begin
    AfficherMessageErreur('*** Série 1 correcte ***');
    Exit(True);
  end;
  // si la série 1 est inexistante, on la crée
  // en supprimant la série bâtarde 1
  try
    self.RemoveSerie(1);
  except
    AfficherMessageErreur('*** Série 1 supprimée mais non libérée ***');
  end;
  AfficherMessageErreur('-> Série 1 inexistante. Recréée');
  MySerie := TObjSerie.Create;
  MySerie.ClearStations();
  MySerie.SetNumeroSerie(1);
  MySerie.AddVisee( 0, 0, 0, tgDEFAULT , 0.0100, 45.00, -45.00, 0.0, 0.0, 0.0, 0.0, '', '');
  MySerie.AddVisee( 0, 0, 0, tgDEFAULT , 0.0125, 45.00, -45.00, 0.0, 0.0, 0.0, 0.0, '', '');

  MySerie.SetSeriePtExtremites(MySerie.GetNumeroDeSerie(), 0, MySerie.GetNumeroDeSerie(), MySerie.GetNbVisees() - 1);
  MySerie.SetChanceObstacle(0, 0);
  MySerie.SetNomObsSerie('Serie 1 reconstruite', 'Serie 1 absente dans le fichier original - Recréée');
  self.AddSerie(MySerie);
  self.SortSeries();     // et on trie la table
  // controle (facultatif)
  (*
  nb := GetNbSeries();
  AfficherMessageErreur('Le document comporte une série 1 absente qui a été reconstruite');
  AfficherMessageErreur(format('Liste des %d séries', [nb]));
  for i := 0 to nb - 1 do
  begin
    MySerie := GetSerie(i);
    AfficherMessageErreur(Format('%d - %d - %s - %d', [i, MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie(), MySerie.GetNbVisees()]));
  end;
  //*)
  result := True;
end;


// Statut: Opérationnel mais les commentaires des séries et stations sont ignorés
// Le format .Text est deprecated et supporté en lecture seule par GHTopo
// Il est surtout destiné à PocketTopo
// DONE: Entrées sans série de départ sont ignorées
// @deprecated
// Shuanghe: OK
// Charentais OK
// Carrière de Bellegarde avec antennes: OK
function TToporobotStructure2012.LoadFichierText(const FichierText: TStringDirectoryFilename): integer; deprecated;
const
  // colonnes des champs
  COL_COMMENT     = 00; // parenthèse de commentaire éventuel
  COL_NUM_SECTION = 01; // numéro de section ou numéro de série
  COL_NUM_ITEM    = 02; // numéro d'item (expé, code, point topo)

  COL_SESSION     = 03; // numéro de session (inutilisé)
  COL_CODE        = 04; // code attaché à la station
  COL_EXPE        = 05; // numéro de séance

  COL_VAR_DATA    = 06; // colonne suivant les colonnes de préfixes et index
  COL_NOMENTREE   = COL_VAR_DATA; // nom de l'entrée
  COL_XENTREE     = COL_VAR_DATA;
  COL_YENTREE     = COL_XENTREE + 1;
  COL_ZENTREE     = COL_YENTREE + 1;
  COL_ENT_SER     = COL_ZENTREE + 1;
  COL_ENT_ST      = COL_ENT_SER + 1;

  COL_DATE_EXPE   = COL_VAR_DATA;
  COL_SPELEOMETRE = COL_DATE_EXPE + 1;
  COL_SPELEOGRAPH = COL_SPELEOMETRE + 1;
  COL_MODEDECL    = COL_SPELEOGRAPH + 1;
  COL_DECL_VALUE  = COL_MODEDECL + 1;
  COL_INCL_VALUE  = COL_DECL_VALUE + 1;
  COL_COULEUR     = COL_INCL_VALUE + 1;

  COL_U_AZIMUT    = COL_VAR_DATA;
  COL_U_PENTE     = COL_U_AZIMUT + 1;
  COL_PSI_L       = COL_U_PENTE + 1;
  COL_PSI_AZ      = COL_PSI_L +1;
  COL_PSI_P       = COL_PSI_AZ + 1;
  //COL_OLD_DECL    = COL_PSI_P  + 1;
  COL_FACT_LONG   = COL_PSI_P + 1;
  COL_ANG_LIMITE  = COL_FACT_LONG + 1;

  COL_SER_DEP     = COL_VAR_DATA;
  COL_PT_DEP      = COL_SER_DEP + 1;
  COL_SER_ARR     = COL_PT_DEP + 1;
  COL_PT_ARR      = COL_SER_ARR + 1;
  COL_NB_PTS      = COL_PT_ARR + 1;
  COL_CHANCE      = COL_NB_PTS + 1;
  COL_OBSTACLE    = COL_CHANCE + 1;

  COL_LONG        = COL_VAR_DATA;
  COL_AZ          = COL_LONG + 1;
  COL_P           = COL_AZ + 1;
  COL_LG          = COL_P + 1;
  COL_LD          = COL_LG + 1;
  COL_HZ          = COL_LD + 1;
  COL_HN          = COL_HZ + 1;

  TEMPORARYTABFILE = '~GhtopoTemp.txt';

var
  EPSGDefault : TLabelSystemesCoordsEPSG;
  // fichier d'erreur
  pTEXT        : TextFile;
  // liste provisoire pour entrées
  ProvListeEntrees: TStringList;
  // séries, réseaux, etc
  UneEntree : TEntrance;
  UneExpe : TExpe;
  UnCode  : TCode;
  UneSerie: TObjSerie;
  qVisee  : TUneVisee;
  // ligne de données
  NoLigneTEXT,
  NbErrorsInLoading: integer;
  LigneTEXT: string;
  PrmsLn   : TGHStringArray;
  // la ligne contient une parenthèse de tête ?
  HasParenthesis: boolean;
  // messages d'erreur
  ErrMsg  : string;
  // Préfixes de section
  Prefix1,
  Prefix2 : integer;

  // divers
  str         : string;
  llanfair_pg : string;
  // dates
  yyy, mmm, ddd: word;
  // Index courant
  //IdxCourant: integer;
  IdxCourantStation: integer;
  NomDeLaSerie: String;
  WU: String;
  mioumiou: Boolean;
  IdxCourant, QN: Integer;

  myViseeAntenne: TViseeAntenne;
  // Conversion de lignes Mac > PC
  procedure ConvertTextMacToPC(var Lin: string);
  begin
    // convertir accentués
    Lin := StringReplace(Lin, #136, 'a', [rfReplaceAll]);
    Lin := StringReplace(Lin, #142, 'e', [rfReplaceAll]);
    Lin := StringReplace(Lin, #143, 'e', [rfReplaceAll]);
    Lin := StringReplace(Lin, #148, 'i', [rfReplaceAll]);
    Lin := StringReplace(Lin, #137, 'a', [rfReplaceAll]);
    Lin := StringReplace(Lin, #144, 'e', [rfReplaceAll]);
    Lin := StringReplace(Lin, #144, 'e', [rfReplaceAll]);
    Lin := StringReplace(Lin, #141, 'c', [rfReplaceAll]);
  end;
  // extraire une visée en antenne
  function ExtraireViseeEnAntenne(const qStr: string;
                                  const qIdxReseau: integer;
                                  const qIdxSerie: integer;
                                  const qIdxPoint: integer;
                                  const qMyVisee: TUneVisee;
                                  var qVA : TViseeAntenne): boolean;
  var
    myStr: String;
    CrO, CrF: integer;
    EWE: TGHStringArray;
  begin
    Result := False;
    myStr := Trim(qStr);
    try
      // Visées en antenne sont de la forme [%.2f %.2f %.2f]
      // crochets indispensables
      // recherche de crochets et suppression
      CrO := Pos('[', myStr);
      if (CrO = 0) then Exit;
      if (CrO > 0) then System.Delete(myStr, CrO, 1);
      CrF := Pos(']', myStr);
      if (CrF > 0) then System.Delete(myStr, CrF, 1);
      EWE := Split(myStr, ' ');
      qVA.SerieDepart      := qIdxSerie;
      qVA.PtDepart         := qIdxPoint;
      //qVA.IDTerrainStation := qMyVisee.IDTerrainStation;
      //qVA.Code             := qMyVisee.Code;
      //qVA.Expe             := qMyVisee.Expe;
      qVA.Reseau           := qIdxReseau;
      qVA.Secteur          := qMyVisee.IDSecteur;
      //qVA.Commentaires     := '';
      qVA.Longueur         := ConvertirEnNombreReel(Trim(EWE[0]), -1.00);
      qVA.Azimut           := ConvertirEnNombreReel(Trim(EWE[1]), 0.00);
      qVA.Pente            := ConvertirEnNombreReel(Trim(EWE[2]), 0.00);
      qVA.MarkedForDelete  := false;
      //AfficherMessage(Format('%f, %f, %f',[qVA.Longueur, qVA.Azimut, qVA.Pente]));
      Result := (qVA.Longueur > 0.00);
    except
    end;
  end;
  // afficher avertissement
  procedure WriteWarning(const wm: string);
  begin
    AfficherMessage(wm);
  end;
begin
  //self.ClearListeSeries();
  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;

  WU := Format('%s.LoadFichierText: %s', [ClassName, FichierText]);
  AfficherMessage(WU);
  AfficherMessageErreur(WU);
  Result := -1;
  // Fichier introuvable ? OK, On sort
  if not FileExists(FichierText) then  begin Result:=-32; Exit; end;
  // nom étude par défaut
  SetNomEtude('Etude001');
  SetCommentairesEtude('Issued from Toporobot Text format (fixed columns)');
  SetCodeEPSGSystemeCoordonnees(EPSGDefault);


  try // try..finally niveau 0
     //*************************************************************************
     // Pour pallier à tout problème de formats de fichiers textes,
     // et notamments ceux liés aux fins de ligne,
     // on fait une conversion.
     // Pour économiser une variable, on utilise une variable utilisée + tard
     LigneTEXT := AnsiToUtf8(ExtractFilePath(FichierText)+ TEMPORARYTABFILE);
     ConvertTextFile(FichierText, LigneTEXT, tfMAC, tfWINDOWS);
     AfficherMessage(GetResourceString(rsSEPARATOR_LINE));
     // nom de la base
     SetDatabaseName(ExtractFileNameWithoutExt(FichierText)); // Laz 1.8: ExtractFileNameWithoutExt() remplace ExtractFileNameOnly()
     // Initialisation des listes
     ReInitialiser(True);
     AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
     // code EPSG par défaut
     SetCodeEPSGSystemeCoordonnees(EPSGDefault);

     // début du véritable traitement
     // liste provisoire pour lecture des sections -6
     ProvListeEntrees:=TStringList.Create;
     // ouverture fichier Text
     AssignFile(pTEXT, LigneTEXT);
     AfficherMessage('--> Lecture du fichier');
     AfficherMessage(Format('--> %d expés, %d codes', [GetNbExpes(), GetNbCodes()]));

     try // niveau 1: pour le fichier Text
       Reset(pTEXT);
       // lire tout le fichier
       NoLigneTEXT:=0;
       NbErrorsInLoading:=0;
       ErrMsg:= rsRD_TAB_MSG_ERR;
       IdxCourant := -1;
       while Not Eof(pTEXT) do
       begin
         try // traitement local d'exceptions dans la lecture des lignes
           Prefix1:=0;
           Inc(NoLigneTEXT);
           ReadLn(pTEXT, LigneTEXT);
           //AfficherMessage('--> ' + LigneTEXT);
           //AfficherMessage(Format('Ligne %d - %s', [NoLigneTEXT, LigneTEXT]));
           //AfficherMessage(LigneTEXT);
           //ConvertTextMacToPC(LigneTEXT);
           if Trim(LigneTEXT)=''    then Prefix1:=-100       // lignes vides
           else if LigneTEXT[1]='#' then Prefix1:=-1000     // commentaires sur une ligne
           else
           begin
             // Commentaire = parenthèse de tête présente
             HasParenthesis := (LigneTEXT[1] = '(');
             //if (HasParenthesis) then Continue;
             // attraper le préfixe de section (en évitant une décomposition)
             str := Trim(Copy(LigneTEXT, 2, 5));
             Prefix1   := StrToIntDef(str, -110);

             if (HasParenthesis) then  // On zappe les commentaires (difficultés d'implémentation)
             begin
               //Continue;
               // classique décomposition ...
               PrmsLn      := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80,100]);
               llanfair_pg := Trim(PrmsLn[COL_VAR_DATA]);
                case Prefix1 of
                 -5: begin // entrées

                       UneEntree := GetEntrance(IdxCourant);
                       // on concatène les lignes
                       UneEntree.eObserv := UneEntree.eObserv + llanfair_pg;

                       PutEntrance(IdxCourant, UneEntree);
                     end;
                 -4: ;     // sessions (obsolète)
                 -3: ;     // section non affectée
                 -2: begin // expés
                       IdxCourant := GetNbExpes() - 1;
                       UneExpe    := GetExpe(IdxCourant);
                       // on concatène les lignes

                       UneExpe.Commentaire := UneExpe.Commentaire + llanfair_pg;
                       PutExpe(IdxCourant, UneExpe);
                     end;
                 -1: begin // code
                       IdxCourant := GetNbCodes() - 1;
                       UnCode     := GetCode(IdxCourant);
                       // on concatène les lignes
                       UnCode.Commentaire := UnCode.Commentaire + llanfair_pg;
                       PutCode(IdxCourant, UnCode);
                     end;
               else  // séries
                 // Zapper cette section
                 begin
                   //Continue;
                   if (Prefix1 > 0) then
                   begin
                     Prefix2:=StrToInt(PrmsLn[COL_NUM_ITEM]);
                     if (Prefix2 = -1) then
                     begin // tête de série
                       IdxCourant := GetNbSeries - 1;
                       UneSerie := GetSerie(IdxCourant);
                       UneSerie.SetObsSerie(UneSerie.GetObsSerie + llanfair_pg);
                     end else
                     begin // sinon, station
                       //Continue;
                       IdxCourant := GetNbSeries - 1;
                       IdxCourantStation := UneSerie.GetNbVisees - 1;
                       qVisee := UneSerie.GetVisee(IdxCourantStation);
                       if (ExtraireViseeEnAntenne(llanfair_pg,
                                                  UneSerie.GetNumeroReseau(),
                                                  UneSerie.GetNumeroDeSerie(),
                                                  IdxCourantStation,
                                                  qVisee,
                                                  myViseeAntenne)) then
                       begin
                         AddViseeAntenne(myViseeAntenne);
                         llanfair_pg := '';
                       end;
                       qVisee.Commentaires := qVisee.Commentaires + llanfair_pg;
                       UneSerie.PutVisee(IdxCourantStation, qVisee);
                     end;
                   end;
                 end;
               end;
               // on zappe tout le reste ...
               Continue;
             end;  // if HasParenthesis
             case Prefix1 of

                -9999: begin // Arrêt forcé du traitement
                             // utiliser cette balise avec de grandes précautions
                             // et de préférence à la fin d'une série
                         WriteWarning('');
                         WriteWarning(Format(rsRD_TAB_STOP_9999, [NoLigneTEXT]));
                         WriteWarning('');
                         Break;
                       end;
                -1000: AfficherMessage(Format(GetResourceString(rsRD_TAB_LN_OBS),[NoLigneTEXT]));  // commentaire
                 -900: Continue; // balise de pause
                 -110: Continue; // Ligne invalide; ignorée
                 -100: Continue; // ignorer les lignes vides
                   -7: Continue; //AfficherMessage(GetResourceString(rsRD_TAB_CLASSEURS)); // Section -7 : Classeurs -- Ignoré par GHTopo
                   -6: begin  // Section -6: Entrée
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80, 100]);
                         ProvListeEntrees.Add(PrmsLn[COL_NOMENTREE]);
                         Continue;
                       end;
                   -5: begin  // Entrées
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 37, 49, 61, 67, 73, 80]);
                         //nombre d'entrées nul = on définit l'entrée par défaut
                         // TODO: Revoir cette zone
                         if (0 = self.GetNbEntrances()) then
                         begin
                           SetDefaultCoords(ConvertirEnNombreReel(PrmsLn[COL_XENTREE], 0.00),
                                            ConvertirEnNombreReel(PrmsLn[COL_YENTREE], 0.00),
                                            ConvertirEnNombreReel(PrmsLn[COL_ZENTREE], 0.00)
                                           );

                           SetRefSeriePoint(StrToIntDef(PrmsLn[COL_ENT_SER],1),
                                            StrtoIntDef(PrmsLn[COL_ENT_ST], 0)
                                           );

                         end;
                         // Ajouter les entrées
                         // le nombre d'entrées retenu est celui décompté dans
                         // la section -5
                         with UneEntree do
                         begin
                           QN := GetNbEntrances();
                           eIDTerrain := '';  // non supporté par TOPOROBOT
                           // On ajoute l'entrée récupérée en -6
                           try
                             eNomEntree:=ProvListeEntrees.Strings[QN];
                           except
                             AfficherMessage(Format(rsWARNINGENTRYADDED,[QN]));
                             eNomEntree:= Format(rsRD_TAB_ENTRANCE,[QN]);
                           end;
                           eXEntree  := ConvertirEnNombreReel(PrmsLn[COL_XENTREE], 0.00);
                           eYEntree  := ConvertirEnNombreReel(PrmsLn[COL_YENTREE], 0.00);
                           eZEntree  := ConvertirEnNombreReel(PrmsLn[COL_ZENTREE], 0.00);
                           // couleurs non supportées = couleur par défault
                           eCouleur  := clGray;
                           //------------
                           // entrées non géoréférencées ?
                           if (eXEntree < 100.00) or
                              (eYEntree < 100.00) or
                              (eZEntree < 100.00)
                           then WriteWarning(Format(GetResourceString(rsRD_TAB_ENTR_NOGEOREF),
                                                   [NoLigneTEXT,QN, eNomEntree]));
                           eRefSer   := StrToIntDef(PrmsLn[COL_ENT_SER],1);
                           eRefSt    := StrtoInt(PrmsLn[COL_ENT_ST]);
                           if ((eRefSer < 1) or (eRefSt < 0)) then
                                  WriteWarning(Format(rsRD_TAB_ENTR_BADLINK,
                                       [NoLigneTEXT, QN, eNomEntree, eRefSer, eRefSt]));
                           eObserv   := '';
                         end; // with UneEntree
                         // n'ajouter l'entrée que si elle comporte une RefSerie non nulle
                         mioumiou := (UneEntree.eRefSer > 0);
                         if (mioumiou) then self.AddEntrance(UneEntree);
                         // index courant
                         IdxCourant := GetNbEntrances() - 1;
                         Continue;
                       end;
                   -4: Continue;  // sessions; ignoré
                   -3: Continue;  // réservé; ignoré
                   -2: begin // Expés
                         //Ajoute l'expé dans la liste
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 36, 50, 64, 65, 73, 77, 81,100]);
                         with UneExpe do
                         begin
                           IDExpe        := StrToInt(PrmsLn[COL_NUM_ITEM]);
                           if (IDExpe<=0) then begin
                              ErrMsg:=rsRD_TAB_BAD_TRIP;
                              raise Exception.Create(ErrMsg);
                           end;
                           // décomposition de la date
                           {$WARNING: TEXpe.DateExpe à implementer}
                           str := Trim(PrmsLn[COL_DATE_EXPE]);
                           JourExpe      := StrToIntDef(Copy(STR, 0, 2), 0);
                           MoisExpe      := StrToIntDef(Copy(STR, 4, 2), 1);
                           AnneeExpe     := StrToIntDef(Copy(STR, 7, 2), 1);
                           if (JourExpe = 0) or (MoisExpe = 0) or (AnneeExpe = 0) then
                           begin
                             WriteWarning(Format(rsRD_TAB_BAD_DATE,
                                                [NoLigneTEXT]));
                             DecodeDate(Now, yyy, mmm, ddd);
                             JourExpe := ddd;
                             MoisExpe := mmm;
                             AnneeExpe:= yyy;
                           end;
                           {$WARNING: End TEXpe.DateExpe à implementer}
                           Operateur     := PrmsLn[COL_SPELEOMETRE];             // spéléomètre
                           ClubSpeleo    := PrmsLn[COL_SPELEOGRAPH];             // spéléographe
                           ModeDecl      := TModeCalculDeclimag(StrToIntDef(PrmsLn[COL_MODEDECL], 1));   // déclinaison auto ?
                           DeclinaisonInDegrees  := ConvertirEnNombreReel(PrmsLn[COL_DECL_VALUE], 0.00); // déclinaison
                           //Inclinaison   := ConvertirEnNombreReel(PrmsLn[COL_INCL_VALUE], 0.00); // correction clino x10
                           IdxCouleur    := StrToIntDef(PrmsLn[COL_COULEUR], 0);  // couleur
                           Commentaire   := ''; //PrmsLn[11];            // commentaire

                         end;  // with UneExpe
                         AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
                         AddExpe(UneExpe);
                         Continue;
                       end;
                   -1: begin // Codes
                         //                                                           U_AZ
                         //                                                            |   U_P
                         //                                                            |   |   PSI_L
                         //                                                            |   |   |   PSI_A
                         //                                                            |   |   |   |   PSI_P
                         //                                                            |   |   |   |   |   FACT
                         //                                                            |   |   |   |   |   |   ANG_LIM
                         //                                                            |   |   |   |   |   |   |
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);
                         with UnCode do
                         begin
                           IDCode     := StrToInt(PrmsLn[COL_NUM_ITEM]);    // ID Code
                           if (IDCode<=0) then
                           begin
                             ErrMsg:=rsRD_TAB_BAD_CODE;
                             raise Exception.Create(ErrMsg);
                           end;
                           GradAz     := ConvertirEnNombreReel(PrmsLn[COL_U_AZIMUT], DEGRES_PAR_TOUR);  // unité boussole
                           GradInc    := ConvertirEnNombreReel(PrmsLn[COL_U_PENTE] , DEGRES_PAR_TOUR);  // unite  CLINO
                           PsiL       := ConvertirEnNombreReel(PrmsLn[COL_PSI_L], 0.01);  // precision longueur
                           PsiAz      := ConvertirEnNombreReel(PrmsLn[COL_PSI_AZ], 0.5);  // precision azimut
                           PsiP       := ConvertirEnNombreReel(PrmsLn[COL_PSI_P], 0.5);  // precision pente
                           // TODO: Support de FactLong a valider
                           FactLong   := 1.00; // ConvertirEnNombreReel(PrmsLn[COL_FACT_LONG], 1.00)/100.00;  // Facteur des longueurs
                           AngLimite  := ConvertirEnNombreReel(PrmsLn[COL_ANG_LIMITE], 0.00);  // angle limite
                           ErreurTourillon  := 0.00;
                           DiametreBoule1   := 0.00;
                           DiametreBoule2   := 0.00;
                           ParamsFuncCorrAz := MakeTParamFoncCorrectionAngulaire(0.00, 0.00, 0.00);   // non supporté par LimeLight
                           ParamsFuncCorrAz := MakeTParamFoncCorrectionAngulaire(0.00, 0.00, 0.00);

                           Commentaire:= ''; //PrmsLn[9];              // commentaire
                         end; // with UnCode
                         AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
                         AddCode(UnCode);
                         Continue;
                       end;
                    0: begin
                         Continue;  // pas de section 0
                       end
             else // on est dans les séries
               begin
                 PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80, 100]);
                 // Si le préfixe 2 (2e colonne) =-1 =>nouvelle série
                 Prefix1 := StrToInt(PrmsLn[COL_NUM_SECTION]);
                 if (Prefix1 > 0) then
                 begin
                   Prefix2 := StrToInt(PrmsLn[COL_NUM_ITEM]);
                   // C'est le header de série ?
                   // les headers de série comportent deux lignes !
                   if (Prefix2 = -2) then
                   begin
                     // première ligne
                     NomDeLaSerie := Trim(PrmsLn[COL_VAR_DATA]);
                     //AfficherMessage(Format('Une serie a ajouter: Prefix1: %d - Prefix2 = %d - %s',[Prefix1, Prefix2, NomDeLaSerie]));
                     // Lecture de la 2e ligne
                     Inc(NoLigneTEXT);
                     ReadLn(pTEXT, LigneTEXT);

                     // décomposition
                     //*******************************************************************************
                     //                                                           SD
                     //                                                            |   PD
                     //                                                            |   |   SA
                     //                                                            |   |   |   PA
                     //                                                            |   |   |   |   NB
                     //                                      Serie                 |   |   |   |   |   CH
                     //                                                            |   |   |   |   |   |   OBS
                     //                                                            |   |   |   |   |   |   |
                     PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);
                     //AfficherMessage(Format('%d %d - %s', [Prefix1, Prefix2,  UneSerie.GetNomSerie]));
                     // si c'est la première série, on crée
                     //AfficherMessage(LigneTEXT);
                     //AfficherMessage(PrmsLn[1]);
                     Prefix1 := StrToInt(PrmsLn[1]);
                     if (Prefix1 = 1) then
                     begin
                       UneSerie := TObjSerie.Create;
                       UneSerie.SetNumeroSerie(1);
                       //AfficherMessage(Format('Prefix = %d - Serie 1 creee',[Prefix1]));
                     end
                     else  // sinon on ferme la série courante et on crée la suivante
                     begin
                       self.AddSerie(UneSerie);
                       UneSerie := TObjSerie.Create;
                     end;
                     with UneSerie do
                     begin
                       SetNumeroSerie(TNumeroSerie(Prefix1));
                       SetNomSerie(NomDeLaSerie);
                       SetSeriePtExtremites(StrToInt(PrmsLn[COL_SER_DEP]),
                                            StrToInt(PrmsLn[COL_PT_DEP]),
                                            StrToInt(PrmsLn[COL_SER_ARR]),
                                            StrToInt(PrmsLn[COL_PT_ARR])
                                           );
                       SetChanceObstacle(StrToInt(PrmsLn[COL_CHANCE]),
                                         StrToInt(PrmsLn[COL_OBSTACLE]));
                       SetNumeroReseau(0); // non supporté par TOPOROBOT
                       SetObsSerie('');
                       SetRaideur(1.00); // Raideur: Non supporté par TOPOROBOT
                     end; //with UneSerie do
                   end else
                   begin // on lit les visées
                     //                 ('-->'+LigneTab);
                     //*******************************************************************************
                     //                                                           Long
                     //                                                            |   Azimut
                     //                                                            |   |   Pente
                     //                                      Parenthèse            |   |   |   LG
                     //                                      | Serie               |   |   |   |   LD
                     //                                      | |   Station         |   |   |   |   |   HZ
                     //                                      | |   |  Code         |   |   |   |   |   |   HN
                     //                                      | |   |  |    |Session|   |   |   |   |   |   |
                     //                                      | |   |  |    | Expé  |   |   |   |   |   |   |
                     PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);

                     with qVisee do
                     begin
                       TypeVisee   := tgDEFAULT;
                       Code        := StrToInt(PrmsLn[COL_CODE]);
                       Expe        := StrToInt(PrmsLn[COL_EXPE]);
                       Longueur    := ConvertirEnNombreReel(PrmsLn[COL_LONG], 0.00);
                       if (Longueur < 0.00) then
                       begin
                         WriteWarning(Format(rsRD_TAB_NEG_LONG, [NoLigneTEXT, Longueur]));
                         Longueur:=Abs(Longueur);
                       end;
                       Azimut      := ConvertirEnNombreReel(PrmsLn[COL_AZ], 0.00);
                       Pente       := ConvertirEnNombreReel(PrmsLn[COL_P], 0.00);
                       LG          := ConvertirEnNombreReel(PrmsLn[COL_LG], 0.00);  //LG
                       LD          := ConvertirEnNombreReel(PrmsLn[COL_LD], 0.00);  //LD
                       HZ          := ConvertirEnNombreReel(PrmsLn[COL_HZ], 0.00);
                       HN          := ConvertirEnNombreReel(PrmsLn[COL_HN], 0.00);
                       Commentaires:= '';
                       IDSecteur   := 0;
                       IDTerrainStation := '';   // Non supporté par TOPOROBOT
                       TypeVisee        := tgDEFAULT; // Non supporté par TOPOROBOT
                     end; // with qVisee do
                     UneSerie.AddVisee(qVisee);
                   end;   // if Prefix2
                 end; // case
               end;
             end; //  case Prefix1 of
           end;
           IdxCourantStation := UneSerie.GetNbVisees - 1;
           // imprimer la ligne traitée
           //AfficherMessage('Ligne traitée');
         except
           AfficherMessageErreur(Format('[%d] %s', [NoLigneTEXT, LigneTEXT]));
         end;
       end;  //while Not Eof(pTEXT) do
       // On ferme la dernière série
       AddSerie(UneSerie);
       //--------------------------------------------------------------------------
       //**************************
       self.SetDatabaseName(ExtractFileNameWithoutExt(FichierText));
       VerifieSerie1();                // Cause courante de plantages: Absence de la série 1
       CheckerLesDonneesTopo();
       RecenserLesIDsTerrain();
       RecenserPointsOfInterest();
       SortCodes();
       SortExpes();
       //SortSeries(); TODO  Trier les séries ?
       UneEntree := GetEntrance(0);
       self.SetRefSeriePoint(UneEntree.eRefSer, UneEntree.eRefSt);
       self.SetDefaultCoords(UneEntree.eXEntree, UneEntree.eYEntree, UneEntree.eZEntree);
       Result := GetNbSeries();
       AfficherMessage('Fichier TEXT OK');
     finally
       CloseFile(pTEXT);
     end; // niveau 1: pour le fichier Text
  finally
    FreeAndNil(ProvListeEntrees);//ProvListeEntrees.Free;
  end; //try..finally niveau 0
end;

// charger un fichier PocketTopo TXT
function TToporobotStructure2012.LoadFromPocketTopoTXT(const FichierTXT: TStringDirectoryFilename): integer;  deprecated;
{$INCLUDE funcimportpockettopotxt.inc}

//******************************************************************************
procedure TToporobotStructure2012.SaveToFile(const FichierTAB: TStringDirectoryFilename; const ModeSaveTAB: TModeSaveTAB; const TextFileFormat: TTextFileFormat);
const

  LINE_STATION = FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_STRING;

  FMT_LINE_NAMESPACE   = FORMAT_NB_INTEGER  + TAB + FORMAT_NB_INTEGER + TAB +
                         '%d ' + TAB + '%d ' + TAB + '%d ' + TAB + // couleur
                         FORMAT_STRING  + TAB + FORMAT_STRING;     // noms
var
  ENDL     : string;
  Sr, St, i: integer;
  QNbFiltresPerso   : integer;
  QNbNamespaces, QNbEntrees, QNbReseaux, QNbSecteurs  : integer;
  QNbCodes, QNbExpes: integer;
  QNbAntennes       : integer;

  pTAB: TextFile;
  Entrance: TEntrance;
  Serie: TObjSerie;
  Station: TUneVisee;
  s : string;
  R : TReseau;
  SC: TSecteur;
  VA: TViseeAntenne;
  EPSG: TLabelSystemesCoordsEPSG;
  MyFiltrePerso: TFiltrePersonnalise;
  MyNamespace: TNameSpace;
  procedure WrtLn(const Str: string); inline;
  begin
    Write(pTAB, Str+ENDL);
  end;
  procedure WrtCommentaire(const Str: string); inline;
  begin
    if (ModeSaveTAB = mtabEXTENDEDTAB) then Write(pTAB, '#### '+Str+ENDL);
  end;
begin
  case TextFileFormat of  // mise en place des fins de lignes
    tfWINDOWS: begin s:='Windows';   ENDL := #13+#10; end;
    tfMAC    : begin s:='Macintosh'; ENDL := #13    ; end;
    tfUNIX   : begin s:='Unix';      ENDL := #10    ; end;
  end;
  AfficherMessage(Format('TToporobotStructure.SaveFile(%s) as %s format',[FichierTAB, s]));
  try
    // Sauvegarder ancien fichier

    AssignFile(pTAB, FichierTAB);
    ReWrite(pTAB);
    AfficherMessage('   Saving header');
    // Sections ajoutées au format TOPOROBOT
    // Fichiers XTB uniquement
    if (ModeSaveTAB = mtabEXTENDEDTAB) then
    begin
      // Section -20: Date de dernières modifications, version du fichier et du logiciel
      AfficherMessage('--> Horodating saves at #20 section');

      WrtLn(Format(FORMAT_NB_INTEGER + TAB +
                   FORMAT_STRING + TAB +
                   FORMAT_STRING + TAB +
                   FORMAT_STRING,
                   [-20,
                    DateToStr(Now), TimeToStr(Now),
                    self.FDataBaseName
                   ]));
      {$IFDEF GROS_MINET}
      WrtLn('# Sylvestre CLEMENT');
      {$ELSE}
      WrtLn('# (fr) L''auteur de GHTopo est christianophobe et hait le Christ');
      WrtLn('# (en) The author of GHTopo hates Christ');
      WrtLn('# (de) Der Autor von GHTopo hasst Christus');
      WrtLn('# (nl) De Autor van GHTopo haat Christus');
      WrtLn('# (es) El autor de GHTopo odia a Cristo');
      WrtLn('# (it) L''autore di GHTopo odia Cristo');
      WrtLn('# (pt) O autor de GHTopo odeia Cristo');
      WrtLn('# (va) Movit omnes auctor GHTopo odit Christum');
      {$ENDIF}
      //WrtLn('# ');

      // Sections -19 : Libellé et commentaires de l'étude (XTB uniquement)
      AfficherMessage('--> Writing general infos at #-19 to #-18 sections');
      WrtLn(Format(FORMAT_NB_INTEGER + TAB +
                    FORMAT_STRING + TAB + FORMAT_STRING,
                   [-19, self.FNomEtude, self.FCommentaireEtude]));
      // Section -18: Espaces de noms (XTB uniquement);
      AfficherMessage('--> Saving namespace at #-18 section');
      WrtLn('');
      QNbNamespaces := self.GetNbNameSpaces();
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -18, ['IDNamespace', 'CouleurNamespace_R', 'CouleurNamespace_G', 'CouleurNamespace_B', 'Namespace', 'Description']));

      WrtCommentaire(Format('%d namespaces', [QNbNamespaces]));
      WrtLn('');
      if (QNbNamespaces > 0) then
      begin
        for i := 0 to QNbNamespaces - 1 do
        begin
          MyNamespace := self.GetNameSpace(i);
          WrtLn(Format(FMT_LINE_NAMESPACE, [-18, i,
                                            Red(MyNamespace.Couleur), Green(MyNamespace.Couleur), Blue(MyNamespace.Couleur),
                                            MyNamespace.Nom, MyNamespace.Description]));
        end;
      end
      else WrtLn(Format(FMT_LINE_NAMESPACE, [-18, 0,
                                             Red(NAMESPACE_COLOR_BY_DEFAULT), Green(NAMESPACE_COLOR_BY_DEFAULT), Blue(NAMESPACE_COLOR_BY_DEFAULT),
                                             NAMESPACE_NAME_BY_DEFAULT, NAMESPACE_DESC_BY_DEFAULT]));
      WrtLn('');
      //************************************************************************
      // Section -16: Filtres personnalisés (XTB uniquement);
      AfficherMessage('--> Saving filters at #-16 section');
      WrtLn('');
      QNbFiltresPerso := GetNbFiltresPersos();
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -16, ['IDFiltre', 'NomFiltre', 'CouleurFiltre_R', 'CouleurFiltre_G', 'CouleurFiltre_B', 'FiltreExpr', 'Description']));
      WrtCommentaire(Format('%d filtres perso', [QNbFiltresPerso]));

      if (QNbFiltresPerso > 0) then
      begin
        for i := 0 to QNbFiltresPerso - 1 do
        begin
          MyFiltrePerso := self.GetFiltrePerso(i);
          WrtLn(Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                       FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                       FORMAT_STRING + TAB +
                       FORMAT_STRING + TAB + FORMAT_STRING,
                       [-16, i,
                        Red(MyFiltrePerso.CouleurFiltre), Green(MyFiltrePerso.CouleurFiltre), Blue(MyFiltrePerso.CouleurFiltre),
                        MyFiltrePerso.NomFiltre,
                        MyFiltrePerso.Expression, MyFiltrePerso.Description
                        ]));
        end;
      end;
      // Section -15: Système de coordonnées géographiques (XTB uniquement)
      // TODO: Désactivé pour l'instant - A réimplémenter
      AfficherMessage('--> Writing coordinates system at #-15 section');
      WrtLn('');
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -15, ['Code EPSG', 'Coordinates system description']));
      //WrtCommentaire('#-15'+ TAB + 'Code EPSG' + TAB + 'Coordinates system description');
      // -15	31563	LT3	Lambert 3 France
      WrtLn('');
      EPSG := GetCodeEPSGSystemeCoordonnees;

      WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER + TAB + FORMAT_STRING,  [-15, EPSG.CodeEPSG, EPSG.NomEPSG]));


      //*************************************
      // Section -10: Secteurs (Format XTB uniquement)
      AfficherMessage('--> Saving #-10 Sectors section');
      WrtLn('');
      WrtCommentaire('Sectors list');
      WrtLn('');
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -10, ['IdxSecteur', 'Color R', 'Color G', 'Color B', 'NomSecteur']));
      WrtLn('');
      QNbSecteurs := GetNbSecteurs();
      if (QNbSecteurs > 1) then
      begin
        for i:=1 to QNbSecteurs - 1 do
        begin
          SC := GetSecteur(i);
          WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_STRING,
                       [-10,
                        i,
                        Red(SC.CouleurSecteur),
                        Green(SC.CouleurSecteur),
                        Blue(SC.CouleurSecteur),
                        SC.NomSecteur
                        ]));
        end;
      end;
      // Section -9: Visées en antennes (Format XTB uniquement)
      QNbAntennes := self.GetNbAntennes();
      if (QNbAntennes > 0) then
      begin
        AfficherMessage('--> Saving #-9 Antenna shots section');

        WrtLn('');
        WrtCommentaire('Antenna-shots list');
        WrtLn('');
        WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -9, ['ID', 'ID Reseau', 'ID Secteur', 'Serie', 'Point', 'Code (unused)', 'Expe (unused)', 'IDTerrain', 'Longueur', 'Azimut', 'Pente', 'Observ.']));
        WrtLn('');

        for i := 1 to QNbAntennes - 1 do
        begin
          VA := GetViseeAntenne(i);
          if (VA.Longueur > 0.001) then
          begin
            //-9	1	2	1102	10	101	101	GHLMF	12.56	289.00	-14.00	le chat dans la souricière
            WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+
                              TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + // série, point
                              TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + // code, expé
                              TAB + FORMAT_STRING +          // ID terrain
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_STRING ,
                     [-9, i, VA.Reseau, VA.Secteur,
                             VA.SerieDepart, VA.PtDepart,
                             0, 0, //VA.Code, VA.Expe,           // Visées en antenne: Code et expés hérité de leur station d'accrochage
                             '', //VA.IDTerrainStation,
                             VA.Longueur, VA.Azimut, VA.Pente,
                             ''  //VA.Commentaires
                     ]));
          end;
        end;

      end;

      // Section -8: Réseaux (Format XTB uniquement)
      AfficherMessage('--> Saving #-8 Networks section');
      WrtLn('');
      WrtCommentaire('Networks list');
      WrtLn('');
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -9, ['IdxReseau', 'Color R', 'Color G', 'Color B', 'TypeReseau', 'NomReseau', 'ObsReseau']));
      WrtLn('');
      QNbReseaux := GetNbReseaux();
      if (QNbReseaux > 1) then
      begin
        for i:=1 to QNbReseaux - 1 do
        begin
          R:=GetReseau(i);
          WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_NB_INTEGER +TAB+
                       FORMAT_STRING + TAB +
                       FORMAT_STRING,
                       [-8,
                        i,
                        Red(R.ColorReseau),
                        Green(R.ColorReseau),
                        Blue(R.ColorReseau),
                        R.TypeReseau,
                        R.NomReseau,
                        R.ObsReseau
                        ]));
        end;
      end;
    end; //  if (ModeSaveTAB = mtabEXTENDEDTAB) then begin
    // Classeur (débrayé)
    //WriteLn(pTAB, Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER,[-7,1,1,1,1]));
    //WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER,[-7,1,1,1,1]));
    // Sauvegarde des entrées
    AfficherMessage('--> Saving #-6 Entrances section');
    WrtLn('');
    WrtCommentaire('Entrances list');
    WrtLn('');
    WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -6, ['ID Entrance', 'NomEntrance', 'Serie', 'Station', 'X Entrance', 'Y Entrance', 'Z Entrance', 'Observ']));
    WrtLn('');
    QNbEntrees := GetNbEntrances();
    for i := 0 to QNbEntrees - 1 do
    begin
      Entrance := GetEntrance(i);
      AfficherMessage(Format('%d/%d: %d.%d:  %s', [i, QNbEntrees, Entrance.eRefSer, Entrance.eRefSt, Entrance.eNomEntree]));
      WrtLn(Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_STRING + TAB + FORMAT_STRING + TAB +
                   FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB +
                   FORMAT_STRING + TAB + FORMAT_STRING,
                  [-6, i+1,
                   Entrance.eNomEntree,
                   Entrance.eIDTerrain,
                   Entrance.eRefSer,  Entrance.eRefSt,
                   Entrance.eXEntree, Entrance.eYEntree, Entrance.eZEntree,
                   Entrance.eObserv,
                   ColorToGHTopoColor(Entrance.eCouleur)
                   ]));
    end;

    AfficherMessage('--> Saving #-5 Deprecated Entrances section');
    WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -5, ['X Entrance', 'Y Entrance', 'Z Entrance', 'Serie', 'Station', 'Observ', '*** Deprecated section. Data moved to #6 section ***']));
    WrtLn('');
    for i:=0 to QNbEntrees - 1 do
    begin
      Entrance := GetEntrance(i);
      WrtLn(Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB +
                   FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_STRING,
                          [-5, i+1,
                          Entrance.eXEntree,
                          Entrance.eYEntree,
                          Entrance.eZEntree,
                          Entrance.eRefSer,
                          Entrance.eRefSt,
                          Entrance.eObserv
                          ]));
    end;
    WrtLn('');
    WrtCommentaire('Trips list');
    WrtLn('');
    AfficherMessage('--> Saving Expes');
    WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -2, ['IDExpe', 'Day', 'Month', 'Year', 'Team', 'Surveyor', 'Modedecl', 'Declination', 'dummy', 'IdxColor', 'Observ.']));
    WrtLn('');
    QNbExpes := GetNbExpes();
    if (QNbExpes > 0) then
    begin
      for  i:= 1 to QNbExpes - 1 do
      begin
        WrtLn(MakeToporobotTabLineOfExpe(-2, GetExpe(i)));
      end;
    end;
    // Codes
    //-1	999	400.00	400.00	0.00	0.00	0.00	100.00	-100.00	Fixpunkt

    AfficherMessage('--> Saving Codes');
    WrtLn('');
    WrtCommentaire('Instruments codes list');
    WrtLn('');
    QNbCodes := GetNbCodes();
    if (QNbCodes > 1) then
    begin
      for i:=1 to QNbCodes - 1 do
      begin
        WrtLn(MakeToporobotTabLineOfCode(-1, ModeSaveTAB, GetCode(i)));
      end;
      WrtLn('');
    end;
    // Séries
    case ModeSaveTAB of
      mtabEXTENDEDTAB: AfficherMessage('--> Saving Series at mtabEXTENDEDTAB mode');
      mtabTOPOROBOT  : AfficherMessage('--> Saving Series at mtabTOPOROBOT mode');
    end;
    WrtLn('');
    WrtCommentaire('Series and stations');
    WrtLn('');
    for Sr:=1 to GetNbSeries() - 1 do
    begin
      WrtLn('');
      Serie := GetSerie(Sr);
      //AfficherMessage(Format('---> %d : %s (%d pts)',[Serie.GetNumeroDeSerie(), Serie.GetNomSerie, Serie.GetNbVisees]));
      with Serie do
      begin
        case ModeSaveTAB of
          mtabEXTENDEDTAB:
            WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_STRING+TAB+FORMAT_STRING+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                                 FORMAT_NB_INTEGER,
                                 [GetNumeroDeSerie(), -1,
                                  GetNoSerieDep(), GetNoPointDep(),
                                  GetNoSerieArr(), GetNoPointArr(),
                                  GetNbVisees() - 1,
                                  GetChance(), GetObstacle(),
                                  GetNomSerie(),
                                  GetObsSerie(),
                                  GetNumeroReseau(),
                                  GetRaideur(),
                                  GetNumeroEntrance()
                                 ]));
          mtabTOPOROBOT:
            WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_STRING,
                                 [GetNumeroDeSerie(), -1,
                                  GetNoSerieDep, GetNoPointDep,
                                  GetNoSerieArr, GetNoPointArr,
                                  GetNbVisees - 1,
                                  GetChance, GetObstacle,
                                  SafeTruncateString(GetNomSerie, 20)
                                 ]));
        end;
        for St := 0 to Serie.GetNbVisees - 1 do
        begin
          Station:=Serie.GetVisee(St);
          with Station do begin
            case ModeSaveTAB of
              mtabEXTENDEDTAB:
              begin
                WrtLn(Format(LINE_STATION + TAB + FORMAT_STRING + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER,
                                [Serie.GetNumeroDeSerie(), St,
                                 Code, Expe,
                                 Longueur, Azimut, Pente,
                                 LG, LD, HZ, HN,
                                 Commentaires,
                                 IDTerrainStation,
                                 TypeVisee,
                                 IDSecteur
                                ]));
              end;
              mtabTOPOROBOT:
              begin
                WrtLn(Format(LINE_STATION,
                                [Serie.GetNumeroDeSerie(), St,
                                 Code, Expe,
                                 Longueur, Azimut, Pente,
                                 LG, LD, HZ, HN,
                                 Commentaires
                                ]));
              end;
            end;
          end;
        end;
      end;
    end;
    AfficherMessage('TToporobotStructure.SaveToFile OK');
  finally
    CloseFile(pTAB);
  end;
end;


//* Méthodes relatives aux entrées
procedure TToporobotStructure2012.AddEntrance(const AEntrance: TEntrance);
begin
  FTableEntrances.AddElement(AEntrance);
end;
function TToporobotStructure2012.GetEntrance(const NumEntrance: Integer): TEntrance;
begin
  try
    Result := FTableEntrances.GetElement(NumEntrance);
  except
    Result := FTableEntrances.GetElement(0);
  end;
end;

procedure TToporobotStructure2012.PutEntrance(const NoEntrance: integer; const AEntrance: TEntrance);
begin
  FTableEntrances.PutElement(NoEntrance, AEntrance);
end;
function TToporobotStructure2012.RemoveEntrance(const Idx: integer): boolean; inline;
begin
  Result := FTableEntrances.RemoveElement(Idx);
end;
function TToporobotStructure2012.GetNbEntrances(): integer;
begin
  Result := FTableEntrances.GetNbElements();
end;



function TToporobotStructure2012.CreateNewEntrance(): boolean;
var
  nb: Integer;
  EWE: TEntrance;
begin
  result := false;
  try
    nb  := GetNbEntrances();
    EWE := GetEntrance(nb - 1);
    EWE.eNomEntree   := 'Nouvelle entree';
    self.AddEntrance(EWE);
    result := true;
    //AfficherMessage(Format('%d entrées', [GetNbEntrances()]));
  except
  end;
end;


// méthodes relatives aux réseaux
procedure TToporobotStructure2012.AddReseau(const AReseau: TReseau);
begin
  FTableReseaux.AddElement(AReseau);
end;
function TToporobotStructure2012.GetReseau(const NumReseau: integer): TReseau;
begin
  try
    Result := FTableReseaux.GetElement(NumReseau);
  except
    Result := FTableReseaux.GetElement(0);
  end;
end;
procedure TToporobotStructure2012.PutReseau(const NoReseau: integer; const AReseau: TReseau);
begin
  FTableReseaux.PutElement(NoReseau, AReseau);
end;
function TToporobotStructure2012.RemoveReseau(const Idx: integer): boolean; inline;
begin
  Result := FTableReseaux.RemoveElement(Idx);
end;
function TToporobotStructure2012.GetNbReseaux(): integer;
begin
  Result := FTableReseaux.GetNbElements;
end;

// méthodes relatives aux labels de terrain
procedure TToporobotStructure2012.AddLabelTerrain(const ALabelTerrain: TToporobotIDStation);
begin
  FTableIDsTerrain.AddElement(ALabelTerrain);
end;


function TToporobotStructure2012.GetLabelTerrain(const NumLabelTerrain: integer): TToporobotIDStation;
begin
  Result := FTableIDsTerrain.GetElement(NumLabelTerrain);
end;



procedure TToporobotStructure2012.PutLabelTerrain(const NoLabelTerrain: integer; const ALabelTerrain: TToporobotIDStation);
begin
  FTableIDsTerrain.PutElement(NoLabelTerrain, ALabelTerrain);
end;


function TToporobotStructure2012.RemoveLabelTerrain(const Idx: integer): boolean;
begin
  Result := FTableIDsTerrain.RemoveElement(Idx);
end;


function TToporobotStructure2012.GetNbLabelTerrain: integer;
begin
  Result := FTableIDsTerrain.GetNbElements;
end;





function TToporobotStructure2012.CreateNouveauReseau(): boolean;
var
  nb: Integer;
  EWE: TReseau;
begin
  Result := false;
  try
    nb  := GetNbReseaux;
    EWE := GetReseau(nb - 1);
    EWE.NomReseau   := 'Nouveau reseau';
    EWE.ObsReseau   := '';
    self.AddReseau(EWE);
    result := true;
  except
    pass;
  end;
end;

function TToporobotStructure2012.GetLastReseau(): TReseau;
var
  Nb: Integer;
begin
  Nb := self.GetNbReseaux - 1;
  Result := GetReseau(Nb);
end;

function TToporobotStructure2012.GetLastEntrance(): TEntrance;
var
  Nb: Integer;
begin
  Nb := self.GetNbEntrances() - 1;
  Result := GetEntrance(Nb);
end;

function TToporobotStructure2012.CalcCentroideEntrees(): TPoint3Df;
var
  NbEntrances, Nb, i: Integer;
  MyEntrance: TEntrance;
  QX0, QY0, QZ0: double;
begin
  NbEntrances := GetNbEntrances();
  MyEntrance  := GetEntrance(0);
  Result      := MakeTPoint3Df( MyEntrance.eXEntree, MyEntrance.eYEntree, MyEntrance.eZEntree);
  Nb := 0;
  QX0 := 0.00;  QY0 := 0.00; QZ0 := 0.00;

  for i := 0 to NbEntrances - 1 do
  begin
    try
      MyEntrance := GetEntrance(i);
      if (IsZero(MyEntrance.eXEntree) or IsZero(MyEntrance.eYEntree)) then Continue;
      QX0 += MyEntrance.eXEntree;
      QY0 += MyEntrance.eYEntree;
      QZ0 += MyEntrance.eZEntree;
      Nb  += 1;
    except
    end;
  end;
  if (Nb <> 0) then
  begin
    Result.X := QX0 / Nb;
    Result.Y := QY0 / Nb;
    Result.Z := QZ0 / Nb;
  end;
end;

function TToporobotStructure2012.HasNumeroReseau(const Idx: integer): boolean;
var
  n: Integer;
begin
  n := GetNbReseaux();
  Result := (Idx < n);
end;


// Méthodes relatives aux secteurs
procedure TToporobotStructure2012.AddSecteur(const ASecteur: TSecteur);
begin
  FTableSecteurs.AddElement(ASecteur);
end;
function TToporobotStructure2012.GetSecteur(const NumSecteur: integer): TSecteur;
begin
  try
    Result := FTableSecteurs.GetElement(NumSecteur);
  except
    Result := FTableSecteurs.GetElement(0);
  end;
end;

procedure TToporobotStructure2012.PutSecteur(const NoSecteur: integer; const ASecteur: TSecteur);
begin
  FTableSecteurs.PutElement(NoSecteur, ASecteur);
end;


function TToporobotStructure2012.RemoveSecteur(const Idx: integer): boolean; inline;
begin
  result := FTableSecteurs.RemoveElement(Idx);
end;
function TToporobotStructure2012.GetNbSecteurs(): integer;
begin
  Result := FTableSecteurs.GetNbElements;
end;
function TToporobotStructure2012.CreateNouveauSecteur(): boolean;
var
  nb: Integer;
  EWE: TSecteur;
begin
  result := false;
  try
    nb  := GetNbSecteurs();
    EWE := GetSecteur(nb - 1);
    EWE.NomSecteur   := 'Nouveau secteur';
    self.AddSecteur(EWE);
    result := true;
  except
  end;
end;

function TToporobotStructure2012.GetLastSecteur(): TSecteur;
var
  Nb: Integer;
begin
  Nb := self.GetNbSecteurs - 1;
  Result := GetSecteur(Nb);
end;

function TToporobotStructure2012.HasNumeroSecteur(const Idx: integer): boolean;
var
  n: Integer;
begin
  n := GetNbSecteurs();
  Result := (Idx < n);
end;

function TToporobotStructure2012.MakeExpe0(const N: TNumeroExpe; const C: string): TExpe;
var
  YYYY, MM, DD: word;
begin
  DecodeDate(Now(), YYYY, MM, DD);
  {$WARNING: TEXpe.DateExpe à implementer}
  with Result do
  begin
    IDExpe        := N;
    JourExpe      := DD;
    MoisExpe      := MM;
    AnneeExpe     := YYYY;
    Operateur     := DFT_OPERATEUR;            // spéléomètre
    ClubSpeleo    := DFT_CLUB_SPELEO;          // spéléographe
    ModeDecl      := cdmAUTOMATIQUE;           // déclinaison auto ?
    DeclinaisonInDegrees := 0.00;              // déclinaison
    IdxCouleur    := 1;                        // couleur
    Commentaire   := C;                        // commentaire
  end;
end;
function TToporobotStructure2012.MakeCode0(const N: TNumeroExpe; const C: string): TCode;
begin
  with Result do
  begin
   IDCode          := N;
   GradAz          := UNITE_ANGULAIRE_DU_CODE_ZERO;
   GradInc         := UNITE_ANGULAIRE_DU_CODE_ZERO;
   PsiL            := 0.00;
   PsiAz           := 0.00;
   PsiP            := 0.00;
   FactLong        := 1.00;
   AngLimite       := 0.00;
   ErreurTourillon := 0.00;
   DiametreBoule1  := 0.00;
   DiametreBoule2  := 0.00;
   Commentaire     := C;
   ParamsFuncCorrAz  := MakeTParamFoncCorrectionAngulaire(0.00, 0.00, 0.00);
   ParamsFuncCorrInc := MakeTParamFoncCorrectionAngulaire(0.00, 0.00, 0.00);
  end;
end;

// méthodes relatives aux expés
//------------------------------------------
procedure TToporobotStructure2012.AddExpe(const LaExpe: TExpe);
begin
  FTableExpes.AddElement(LaExpe);
end;
procedure TToporobotStructure2012.PutExpe(const Idx: integer; const LaExpe: TExpe);
begin
  FTableExpes.PutElement(Idx, LaExpe);
end;
function TToporobotStructure2012.GetExpe(const Idx: integer): TExpe;
begin
  Result := FTableExpes.GetElement(Idx);
end;
function  TToporobotStructure2012.GetExpeByNumero(const NumeroExpe: TNumeroExpe): TExpe;
begin
  Result := FTableExpes.GetElementByIndex(NumeroExpe);
end;
function TToporobotStructure2012.GetNbExpes(): integer;
begin
  Result := FTableExpes.GetNbElements;
end;

function TToporobotStructure2012.CreateNouveauExpe(): boolean;
var
  EWE: TExpe;
  nb: Integer;
begin
  result := false;
  try
    nb  := GetNbExpes;
    EWE := GetExpe(nb - 1);
    EWE.IDExpe       := 1 + getMaxIdxExpe();
    EWE.Commentaire  := 'Nouvelle expe';
    self.AddExpe(EWE);
    result := true;
  except
  end;
end;

function TToporobotStructure2012.GetLastExpe(): TExpe;
var
  Nb: Integer;
begin
  Nb := self.GetNbExpes - 1;
  Result := GetExpe(Nb);
end;



function TToporobotStructure2012.RemoveExpe(const Idx: integer): boolean;
begin
  Result := FTableExpes.RemoveElement(Idx);
end;
// méthodes relatives aux codes
//------------------------------------------
procedure TToporobotStructure2012.AddCode(const LeCode: TCode);
begin
  FTableCodes.AddElement(LeCode);
end;
function TToporobotStructure2012.GetCode(const Idx: integer): TCode;
begin
  Result := FTableCodes.GetElement(Idx);
end;
//------------------------------------------
function  TToporobotStructure2012.GetCodeByNumero(const NumeroCode: TNumeroCode): TCode;
begin
  Result := FTableCodes.GetElementByIndex(NumeroCode);
end;
procedure TToporobotStructure2012.PutCode(const Idx: integer; const LeCode: TCode);
begin
  FTableCodes.PutElement(Idx, LeCode);
end;
function  TToporobotStructure2012.RemoveCode(const Idx: integer): boolean; inline;
begin
  FTableCodes.RemoveElement(Idx);
end;
function TToporobotStructure2012.GetNbCodes(): integer;
begin
  Result := FTableCodes.GetNbElements();
end;




function TToporobotStructure2012.CreateNouveauCode(): boolean;

var
  nb: Integer;
  EWE: TCode;
begin
  result := false;
  try
    nb  := GetNbCodes;
    EWE := GetCode(nb - 1);
    EWE.IDCode      := 1 + getMaxIdxCode;
    EWE.Commentaire := 'Nouveau code';
    self.AddCode(EWE);
    result := true;
  except
  end;
end;

function TToporobotStructure2012.GetLastCode(): TCode;
var
  Nb: Integer;
begin
  Nb := self.GetNbCodes - 1;
  Result := GetCode(Nb);
end;

// méthodes relatives aux antennes
procedure TToporobotStructure2012.AddViseeAntenne(const VA: TViseeAntenne);
begin
  FTableViseesAntenne.AddElement(VA);
end;


function  TToporobotStructure2012.GetViseeAntenne(const No: integer): TViseeAntenne;
begin
  Result := FTableViseesAntenne.GetElement(No);
end;
procedure TToporobotStructure2012.PutViseeAntenne(const No: integer; const VA: TViseeAntenne);

begin
  FTableViseesAntenne.PutElement(No, VA);
end;

function TToporobotStructure2012.ReattribuerCodesExpesViseesAntennes(): boolean;
var
  PreviousViseeAntenne, MyViseeAntenne: TViseeAntenne;
  Nb, i, QIdxSerie: Integer;
  WU, SameBasePt: Boolean;
  MySerie: TObjSerie;
  MyStation: TUneVisee;
  T: TDateTime;
begin
  result := false;
  AfficherMessage(format('%s.ReattribuerCodesExpesViseesAntennes()', [ClassName]));
  Nb := GetNbAntennes();
  if (Nb <= 1) then Exit(false);
  MyViseeAntenne := GetViseeAntenne(0);
  WU := GetSerieByNumeroSerie(MyViseeAntenne.SerieDepart, MySerie, QIdxSerie);
  MyStation := MySerie.GetVisee(MyViseeAntenne.PtDepart);
  T := now();
  AfficherMessage(DateTimeToStr(T));
  for i := 1 to Nb - 1 do
  begin
    try
      PreviousViseeAntenne := GetViseeAntenne(i-1);
      MyViseeAntenne       := GetViseeAntenne(i);
      // les extractions de série/stations sont coûteuses et ne doivent être effectuées
      // que si les basepoint des antennes n et n-1 diffèrent
      SameBasePt := (MyViseeAntenne.SerieDepart = PreviousViseeAntenne.SerieDepart) and
                    (MyViseeAntenne.PtDepart    = PreviousViseeAntenne.PtDepart);
      if ((i = 1) or (not SameBasePt)) then
      begin
        WU := GetSerieByNumeroSerie(MyViseeAntenne.SerieDepart, MySerie, QIdxSerie);
        MyStation := MySerie.GetVisee(MyViseeAntenne.PtDepart);
      end;
      // on reporte les code, expé, réseau et secteur de la station d'accrochage
      MyViseeAntenne.Reseau  := MySerie.GetNumeroReseau();
      //MyViseeAntenne.Code    := MyStation.Code;
      //MyViseeAntenne.Expe    := MyStation.Expe;
      MyViseeAntenne.Secteur := MyStation.IDSecteur;
      // et on actualise
      PutViseeAntenne(i, MyViseeAntenne);
      Result := True;
    except
      ;
    end;
  end;
  AfficherMessage(TimeToStr(Now() - T));
end;




function TToporobotStructure2012.RemoveViseeAntenne(const No: integer): boolean;
begin
  Result := FTableViseesAntenne.RemoveElement(No);
end;

function TToporobotStructure2012.RemoveLastViseeAntenne(): boolean;
begin
  Result := FTableViseesAntenne.RemoveLastElement();
end;


function TToporobotStructure2012.GetNbAntennes(): integer;
begin
  Result := FTableViseesAntenne.GetNbElements;
end;
// supprimer les antennes orphelines
// et celles dont le numéro serie/station est spécifié (si qIdxSerie > 0)
// longMax: Longueur au-delà de laquelle la visée est supprimée
function TToporobotStructure2012.NettoyerAntennes(const longMax: double; const qIdxSerie, qIdxStation: integer): integer;
var
  EWE: TViseeAntenne;
  n: Integer;
  B0, B1, B2, B3, B4: boolean;
begin
  Result := 0;
  AfficherMessage(Format('%s.NettoyerAntennes: %.3f (origine: %d.%d)', [self.ClassName, longMax, qIdxSerie, qIdxStation]));
  for n := FTableViseesAntenne.Count - 1 downto 0 do
  begin
    EWE := GetViseeAntenne(n);
    B0  := (0 = EWE.PtDepart);
    B1  := not IsInRange(EWE.Longueur, 0.01, longMax);
    B2  := Not HasStationForAntennaShot(EWE);
    B3  := false;
    if (Assigned(FProcDispProgression) and (n mod 1000 = 0)) then FProcDispProgression('Antennes', n, 0, FTableViseesAntenne.Count, 500);
    if (qIdxSerie > 0) then B3 := (EWE.SerieDepart = qIdxSerie) and (EWE.PtDepart = qIdxStation);
    if (B0 or B1 or B2 or B3) then
    begin
      RemoveViseeAntenne(n);
      AfficherMessageErreur(Format('Antenne retiree: %d.%d', [EWE.SerieDepart, EWE.PtDepart]));
      Result += 1;
    end;
  end;
  FTableViseesAntenne.Pack;
  AfficherMessage(Format('%d antennes supprimées', [Result]));
end;

// existence d'éléments correspondant à un index ?
(*
function TToporobotStructure2012.ExistsIdxEntrance(const Idx: TNumEntrance): boolean;
begin
  Result := FTableEntrees.ExistsElement(Integer(Idx));
end;
//*)


function TToporobotStructure2012.ExistsIdxCode(const Idx: integer): boolean;
begin
  Result := FTableCodes.ExistsElement(Idx);
end;

function TToporobotStructure2012.ExistsIdxExpe(const Idx: integer): boolean;
begin
  Result := FTableExpes.ExistsElement(Idx);
end;

function TToporobotStructure2012.ExistsIdxReseau(const Idx: integer): boolean;
begin
  Result := (Idx < FTableReseaux.GetNbElements());
end;

function TToporobotStructure2012.ExistsIdxEntrance(const Idx: integer): boolean;
begin
  Result := (Idx < FTableEntrances.GetNbElements());
end;



function TToporobotStructure2012.GetLastFindText(): string;
begin
  Result := FLastFindTextWhat;
end;

procedure TToporobotStructure2012.ExportListeEntreesCSV(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  i, Nb: Integer;
  myEntrance: TEntrance;
begin
  Nb := GetNbEntrances();
  AfficherMessage(Format('%s.ExportListeEntreesCSV: %d', [ClassName, Nb]));
  if (Nb > 0) then
  begin
    try
      AssignFile(fp, FileName);
      ReWrite(fp);
      WriteLn(fp, 'No' + #9 + 'Nom' + #9 +
                  'X' + #9 + 'Y' + #9 + 'Z' + #9 +
                  'Serie' + #9 + 'Point' + #9 +
                  'Observ');
      for i := 0 to Nb - 1 do
      begin
        myEntrance := GetEntrance(i);
        WriteLn(fp, Format(FORMAT_NB_INTEGER + #9 + FORMAT_STRING + #9 +
                           FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 +
                           FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
                           FORMAT_STRING,
                           [i, myEntrance.eNomEntree,
                            myEntrance.eXEntree, myEntrance.eYEntree, myEntrance.eZEntree,
                            myEntrance.eRefSer, myEntrance.eRefSt,
                            myEntrance.eObserv
                           ]));
      end;
    finally
      CloseFile(fp);
    end;
  end;
end;

procedure TToporobotStructure2012.ExportListeReseauxCSV(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  i, Nb: Integer;
  myReseau: TReseau;
begin
  Nb := GetNbReseaux;
  AfficherMessage(Format('%s.ExportListeReseauxCSV: %d', [ClassName, Nb]));
  if (Nb > 0) then
  begin
    try
      AssignFile(fp, FileName);
      ReWrite(fp);
      WriteLn(fp, 'No' + #9 + 'Type' + #9 + 'Couleur' + #9 +
                  'Nom' + #9 + 'Observ');
      for i := 0 to Nb -1 do
      begin
        myReseau := GetReseau(i);
        WriteLn(fp, Format(FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 + '#%X' + #9 +
                           FORMAT_STRING + #9 + FORMAT_STRING,
                           [i,
                            myReseau.TypeReseau, myReseau.ColorReseau,
                            myReseau.NomReseau,
                            myReseau.ObsReseau
                           ]));
      end;
    finally
      CloseFile(fp);
    end;
  end;
end;

procedure TToporobotStructure2012.ExportListeSecteursCSV(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  i, Nb: Integer;
  mySecteur: TSecteur;
begin
  Nb := GetNbSecteurs;
  AfficherMessage(Format('%s.ExportListeSecteursCSV: %d', [ClassName, Nb]));
  if (Nb > 0) then
  begin
    try
      AssignFile(fp, FileName);
      ReWrite(fp);
      WriteLn(fp, 'No' + #9 + 'Type' + #9 + 'Couleur' + #9 +
                  'Nom');
      for i := 0 to Nb -1 do
      begin
        mySecteur := GetSecteur(i);
        WriteLn(fp, Format(FORMAT_NB_INTEGER + #9 + '#%X' + #9 + FORMAT_STRING,
                           [i, mySecteur.CouleurSecteur,
                            mySecteur.NomSecteur
                           ]));
      end;
    finally
      CloseFile(fp);
    end;
  end;
end;

procedure TToporobotStructure2012.ExportListeCodesCSV(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  i, Nb: Integer;
  myCode: TCode;
begin
  Nb := GetNbCodes();
  AfficherMessage(Format('%s.ExportListeCodesCSV: %d', [ClassName, Nb]));
  if (Nb > 0) then
  begin
    try
      AssignFile(fp, FileName);
      ReWrite(fp);
      WriteLn(fp, FileName);
      writeln(fp,  'No' + #9 + 'UniteBoussole' + #9 + 'CorrectionAzimut' +  #9 + 'UniteClino' + #9 + 'CorrectionPente' + #9 + 'Observ');
      for i := 0 to Nb -1 do
      begin
        myCode := GetCode(i);
        //TODO: Export codes à faire
        WriteLn(fp, Format(FORMAT_NB_INTEGER + #9 +
                           FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 +
                           FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 +
                           FORMAT_STRING,
                           [myCode.IDCode,
                            myCode.GradAz , myCode.ParamsFuncCorrAz.Co,
                            myCode.GradInc, myCode.ParamsFuncCorrInc.Co,
                            myCode.Commentaire
                           ]));
      end;
    finally
      CloseFile(fp);
    end;
  end;
end;

procedure TToporobotStructure2012.ExportListeExpesCSV(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  i, Nb: Integer;
  myExpe: TExpe;
begin
  Nb := GetNbExpes;
  AfficherMessage(Format('%s.ExportListeExpesCSV: %d', [ClassName, Nb]));

  if (Nb > 0) then
  begin
    try
      AssignFile(fp, FileName);
      ReWrite(fp);
      WriteLn(fp, 'No' + #9 + 'Couleur' + #9 + 'Date' + #9 + 'Equipe' + #9 +
                  'Observ');
      for i := 0 to Nb -1 do
      begin
        myExpe := GetExpe(i);
        {$WARNING: TEXpe.DateExpe à implementer}
        WriteLn(fp, Format(FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
                           '%.2d/%.2d/%.4d' + #9 +
                           '%s, %s' + #9 +
                           FORMAT_STRING,
                           [myExpe.IDExpe, myExpe.IdxCouleur,
                            myExpe.JourExpe, myExpe.MoisExpe, myExpe.AnneeExpe,   // DatePascalToDateSQL(myExpe.DateExpe),
                            myExpe.Operateur, myExpe.ClubSpeleo,
                            myExpe.Commentaire
                           ]));
      end;
    finally
      CloseFile(fp);
    end;
  end;
end;



procedure TToporobotStructure2012.ExportListePOIToCSV(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  Nb, i: Integer;
  MyPOI: TPointOfInterest;
begin
  Nb := GetNbPointsOfInterests();
  AfficherMessage(Format('%s.ExportListePOIToCSV: %d', [ClassName, Nb]));
  if (Nb > 0) then
  begin
    try
      AssignFile(fp, FileName);
      ReWrite(fp);
      WriteLn(fp, 'No' + #9 + 'Serie' + #9 + 'Station' + #9 +
                  //'OffsetX' + #9 + 'OffsetY' + #9 + 'OffsetZ' + #9 +
                  'Couleur' + #9 + 'IDTerrain' + #9 +
                  'Description');
      for i := 0 to Nb -1 do
      begin
        MyPOI := GetPointOfInterest(i);
        WriteLn(fp, Format(FORMAT_NB_INTEGER + #9 +
                           FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
                           //FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 +
                           FORMAT_STRING + #9 + FORMAT_STRING + #9 +
                           FORMAT_STRING,
                           [i,
                            MyPOI.Serie, MyPOI.Station,
                            //MyPOI.Coordinates.X, MyPOI.Coordinates.Y, MyPOI.Coordinates.Z,
                            ColorToHTMLColor(MyPOI.Couleur), MyPOI.LabelTerrain,
                            MyPOI.Description
                           ]));
      end;
    finally
      CloseFile(fp);
    end;
  end;

end;
procedure TToporobotStructure2012.ExportListeErreursToCSV(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  Nb, i: Integer;
  MyErreur: TMessaqeErreurGHTopoCompiler;
begin
  Nb := GetNbMessagesErreur();
  AfficherMessage(Format('%s.ExportListeErreursToCSV: %d', [ClassName, Nb]));
  if (Nb > 0) then
  begin
    try
      AssignFile(fp, FileName);
      ReWrite(fp);
      WriteLn(fp, 'No' + #9 +
                  'Table' + #9 + 'Index' + #9 +
                  'Criticite' + #9 + 'Couleur' + #9 +
                  'Description');
      for i := 0 to Nb -1 do
      begin
        MyErreur := GetMessageErreur(i);
        WriteLn(fp, Format(FORMAT_NB_INTEGER + #9 +
                           FORMAT_STRING + #9 + FORMAT_NB_INTEGER + #9 +
                           FORMAT_STRING + #9 + FORMAT_STRING + #9 +
                           FORMAT_STRING,
                           [i,
                            DescribeTableExamineeByCode(MyErreur.TableExaminee), MyErreur.Index,
                            DescribeCriticiteErreur(MyErreur.Criticite), ColorToHTMLColor(MyErreur.Couleur),
                            MyErreur.Message
                           ]));
      end;
    finally
      CloseFile(fp);
    end;
  end;

end;


procedure TToporobotStructure2012.ExportListeEntetesSeries(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  i: integer;
  S: TObjSerie;
  procedure Wrtln(const S: string);
  begin
    WriteLn(fp, S);
  end;
  procedure WrtItem(const S: TObjSerie);
  const
    FMT666 = FORMAT_NB_INTEGER + #9 + FORMAT_STRING + #9 +
             FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
             FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
             FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
             FORMAT_NB_INTEGER;
  begin
    WrtLn(Format(FMT666,
                 [S.GetNumeroDeSerie(),
                  S.GetNomSerie(),
                  S.GetNumeroEntrance(), S.GetNumeroReseau(),
                  S.GetNoSerieDep(), S.GetNoPointDep(),
                  S.GetNoSerieArr(), S.GetNoSerieArr(),
                  S.GetNbVisees()
                 ]));
  end;
begin
  AfficherMessage(Format('%s.ExportListeEntetesSeries', [ClassName]));
  AssignFile(fp, FileName);
  try
    Rewrite(fp);
    Wrtln('Serie' + #9 +
          'Nom serie' + #9 +
          'Entree' + #9 +
          'Reseau' + #9 +
          'Ser Dep' + #9 +
          'Pt Dep' + #9 +
          'Ser Arr' + #9 +
          'Pt Arr' + #9 +
          'NbPts');
    for i := 0 to GetNbSeries - 1 do
    begin
      S := GetSerie(i);
      WrtItem(S);
    end;
  finally
    CloseFile(fp);
  end;
end;



//------------------------------------------------------------------------------
// check des données
// retourne le nombre d'erreurs
function TToporobotStructure2012.CheckerLesDonneesTopo(): Integer;
begin
  Result := 0;
  AfficherMessage(Format('%s.CheckerLesDonneesTopo',[ClassName]));
  FListeMessagesErreur.ClearListe();
  CheckerLesNamespaces();
  CheckerLesCodes();
  CheckerLesExpes();
  CheckerLesEntrees();        // scan des entrées
  CheckerLesSeries();         // scan des séries
  CheckerLesAntennes();
  Result := FListeMessagesErreur.GetNbElements();
end;

procedure TToporobotStructure2012.CheckerLesNamespaces();
var
  Nb: Integer;
begin
  Nb := GetNbNameSpaces();
  if (0 = Nb) then AddNameSpace(NAMESPACE_NAME_BY_DEFAULT, NAMESPACE_COLOR_BY_DEFAULT, NAMESPACE_DESC_BY_DEFAULT)
end;

procedure TToporobotStructure2012.CheckerLesEntrees();
var
  UneEntree: TEntrance;
  Nb, i: Integer;
begin
  Nb := GetNbEntrances();
  AfficherMessage(Format('%s.CheckerLesEntrees: %d entrances',[ClassName, Nb]));
  if (Nb = 0) then
  begin
    //UneEntree.eNumEntree := 0;
    UneEntree.eNomEntree := 'Entree par default';
    UneEntree.eXEntree   := self.FPositionPointZero.X;
    UneEntree.eYEntree   := self.FPositionPointZero.Y;
    UneEntree.eZEntree   := self.FPositionPointZero.Z;
    UneEntree.eRefSer    := 1;
    UneEntree.eRefSt     := 0;
    UneEntree.eObserv    := 'Entree reparee';
    self.AddEntrance(UneEntree);
    AddMessageErreur(tmeENTRANCES, cmeNOTE, 0, 'Entrée 0 ajoutée');
  end;
  for i := 0 to Nb - 1 do
  begin
    UneEntree := GetEntrance(i);
    if (IsZero(UneEntree.eXEntree)) then AddMessageErreur(tmeENTRANCES, cmeERROR, i, Format('%s: Coordonnée X nulle: %.2f', [UneEntree.eNomEntree, UneEntree.eXEntree]));
    if (IsZero(UneEntree.eYEntree)) then AddMessageErreur(tmeENTRANCES, cmeERROR, i, Format('%s: Coordonnée Y nulle: %.2f', [UneEntree.eNomEntree, UneEntree.eYEntree]));
    if (IsZero(UneEntree.eZEntree)) then AddMessageErreur(tmeENTRANCES, cmeERROR, i, Format('%s: Coordonnée Z nulle: %.2f', [UneEntree.eNomEntree, UneEntree.eZEntree]));
  end;
end;
// 21/08/2017: Ajout de la détection des doublons dans les numéros de séries
procedure TToporobotStructure2012.CheckerLesSeries();
var
  MySerieCourante, MySerieErreur: TObjSerie;
  i, Nb  , QQ, NbreDeStations, EWE: Integer;
begin
  Nb := GetNbSeries();
  NbreDeStations := 0;
  AfficherMessageErreur(Format('-- %s.CheckerLesSeries: %d series',[ClassName, Nb]));
  for i := 1 to Nb - 1 do
  begin

    MySerieCourante    := GetSerie(i);
    // détection des doublons
    if (HasDoublonsInNumsSeries(MySerieCourante.GetNumeroDeSerie(), EWE)) then
    begin
      MySerieErreur := GetSerie(EWE);
      AddMessageErreur(tmeSERIES, cmeCRITICAL, MySerieCourante.GetNumeroDeSerie(), Format(GetResourceString(rsCHK_MSG_DOUBLONS_SERIES),
                                [EWE, MySerieErreur.GetNumeroDeSerie()  , MySerieErreur.GetNomSerie(),
                                 i  , MySerieCourante.GetNumeroDeSerie(), MySerieCourante.GetNomSerie()]));
    end;
    NbreDeStations     += MySerieCourante.GetNbVisees();
    QQ := CheckSerie(MySerieCourante, i);
  end;



  if (NbreDeStations < 2) then AddMessageErreur(tmeSERIES, cmeWARNING, 0, Format(GetResourceString(rsCHK_MSG_SERIE_NB_STATIONS_WARNING), [NbreDeStations]))
                          else AddMessageErreur(tmeSERIES, cmeNOTE   , 0, Format(GetResourceString(rsCHK_MSG_SERIE_NB_TOTAL_STATIONS), [NbreDeStations]));
end;


// // retourne: -1 si série vide; numéro de visée si erreur, 0 si OK
function TToporobotStructure2012.CheckSerie(const MySerie: TObjSerie; const QIdx: integer): integer;
const
  SEUIL_SERIE_LONGUEUR_NULLE = 0.009;
  SEUIL_SERIE_TRES_COURTE = 0.10;
  MSG_ERR_ANGLE_INVALIDE  = '-- %d.%d -  %s invalide = %.3f a.u.';
  MSG_ERR_LRUD_TOO_BIG    = '-- %d.%d -  %s grand = %.3f m';
  MSG_ERR_ITEM_INEXISTANT = '-- %d.%d - %s inexistant = %d';
  MSG_ERR_ITEM_INEXISTANT_CORRECTED = MSG_ERR_ITEM_INEXISTANT + ' - Nouvelle valeur %d';
var
  i, N: Integer;
  MyVisee: TUneVisee;
  YaErreur: TErrorVisee;
  MessageRetour: string;
  NS, SD, SA, PD, PA, NbEntrances: integer;
  QLongueurSerie: Double;
  SerieRacoordeeAUneEntree: Boolean;
  MyEntrance: TEntrance;
begin
  Result := -1;
  SerieRacoordeeAUneEntree := false;
  N := MySerie.GetNbVisees;
  //try
    // détection des auto-boucles (séries se refermant sur elles-mêmes)
    NS := MySerie.GetNumeroDeSerie();
    SD := MySerie.GetNoSerieDep;
    SA := MySerie.GetNoSerieArr;
    PD := MySerie.GetNoPointDep;
    PA := MySerie.GetNoPointArr;

    if ((NS = SA) and (PA < (N - 1)))then  AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(GetResourceString(rsCHK_MSG_AUTOLOOP), [SA, N - 1, NS, PA]));
    if ((NS = SA) and (PA > (N - 1)))then  AddMessageErreur(tmeSERIES, cmeERROR  , NS, Format(GetResourceString(rsCHK_MSG_SERIE_PB_STATION_ARRIVEE), [NS, SA, PA, SA, N-1]));


    // détection des séries non raccordées
    if ((SD = SA) and (SD = NS) and (NS <> 1)) then
    begin
      // on vérifie d'abord si la série est raccordée à une entrée
      NbEntrances := GetNbEntrances();
      for i := 0 to NbEntrances - 1 do
      begin
        MyEntrance := GetEntrance(i);
        if (SD = MyEntrance.eRefSer) then
        begin
          SerieRacoordeeAUneEntree := True;
          break;
        end;
      end;
      if (SerieRacoordeeAUneEntree) then
        AddMessageErreur(tmeSERIES, cmeNOTE, NS, Format(GetResourceString(rsCHK_MSG_SERIE_CNX_ENTREE), [NS, MyEntrance.eRefSer, MyEntrance.eRefSt, MyEntrance.eNomEntree]))
      else
        AddMessageErreur(tmeSERIES, cmeERROR, NS, Format(GetResourceString(rsCHK_MSG_SERIE_ORPHELINE), [NS, SD, PD, SA, PA]));
    end;
    // scan de la table des visées
    QLongueurSerie := 0.00;
    if (N = 0) then
    begin
      // série sans visées
      AddMessageErreur(tmeSERIES, cmeCRITICAL, NS, Format(GetResourceString(rsCHK_MSG_SERIE_WITHOUT_SHOTS), [NS]));
      Exit(-1);
    end;
    for i := 0 to N - 1 do
    begin
      MyVisee := MySerie.GetVisee(i);
      QLongueurSerie += MyVisee.Longueur;
      YaErreur := CheckVisee(MyVisee, MessageRetour);
      if (YaErreur <> []) then
      begin
        //AfficherMessageErreur('');
        AfficherMessageErreur(Format('*** [%s] In serie %d - Shot %d ***', [MessageRetour, MySerie.GetNumeroDeSerie(), i]));
        // visée 0
        if (i = 0) then
        begin
          if (errvSECTEUR   in YaErreur) then
          begin
            MyVisee.IDSecteur := 0;
            MySerie.PutVisee(0, MyVisee);
            AddMessageErreur(tmeSERIES, cmeERROR_AUTOFIXED, NS, Format(MSG_ERR_ITEM_INEXISTANT_CORRECTED, [NS, i, 'Secteur', MyVisee.IDSecteur, 0]));
          end;
          if (errvTYPEVISEE in YaErreur) then
          begin
            MyVisee.TypeVisee := tgDEFAULT;
            MySerie.PutVisee(0, MyVisee);
            AddMessageErreur(tmeSERIES, cmeERROR_AUTOFIXED, NS, Format(MSG_ERR_ITEM_INEXISTANT_CORRECTED, [NS, i, 'Type', MyVisee.TypeVisee, Ord(tgDEFAULT)]));
          end;
          if (errvCODE      in YaErreur) then
          begin
            MyVisee.Code := 1;
            MySerie.PutVisee(0, MyVisee);
            AddMessageErreur(tmeSERIES, cmeERROR_AUTOFIXED, NS, Format(MSG_ERR_ITEM_INEXISTANT_CORRECTED, [NS, i, 'Code', MyVisee.Code, 1]));
          end;
          if (errvEXPE      in YaErreur) then
          begin
            MyVisee.Expe := 1;
            MySerie.PutVisee(0, MyVisee);
            AddMessageErreur(tmeSERIES, cmeERROR_AUTOFIXED, NS, Format(MSG_ERR_ITEM_INEXISTANT_CORRECTED, [NS, i, 'Expe', MyVisee.Expe, 1]));
          end;
          if (MyVisee.Longueur < SEUIL_LONGUEUR_NULLE) then
          begin
            MyVisee.Longueur := SEUIL_LONGUEUR_NULLE;
            MySerie.PutVisee(0, MyVisee);
            AddMessageErreur(tmeSERIES, cmeERROR_AUTOFIXED, NS, Format('%d.%d - Visée de longueur nulle mise à %.3f m', [NS, i, MyVisee.Longueur]));
          end;
        end
        else
        begin
          if (errvSECTEUR        in YaErreur) then AddMessageErreur(tmeSERIES, cmeERROR, NS, Format(MSG_ERR_ITEM_INEXISTANT, [NS, i, 'Secteur', MyVisee.IDSecteur]));
          if (errvTYPEVISEE      in YaErreur) then AddMessageErreur(tmeSERIES, cmeERROR, NS, Format(MSG_ERR_ITEM_INEXISTANT, [NS, i, 'Type', MyVisee.TypeVisee]));
          if (errvCODE           in YaErreur) then AddMessageErreur(tmeSERIES, cmeERROR, NS, Format(MSG_ERR_ITEM_INEXISTANT, [NS, i, 'Code', MyVisee.Code]));
          if (errvEXPE           in YaErreur) then AddMessageErreur(tmeSERIES, cmeERROR, NS, Format(MSG_ERR_ITEM_INEXISTANT, [NS, i, 'Expe', MyVisee.Expe]));
          if (errvLONGUEUR_NULLE in YaErreur) then AddMessageErreur(tmeSERIES, cmeERROR, NS, Format('%d.%d - Visée de longueur nulle: %.3f m', [NS, i, MyVisee.Longueur]));
        end;

        if (errvLONGUEUR_OVERFLOW  in YaErreur) then AfficherMessageErreur(Format('%d.%d - Length = %.3f m too big', [NS, i, MyVisee.Longueur]));

        if (errvAZIMUT    in YaErreur) then AddMessageErreur(tmeSERIES, cmeERROR, NS, Format(MSG_ERR_ANGLE_INVALIDE, [NS, i, 'Azimut', MyVisee.Azimut]));
        if (errvPENTE     in YaErreur) then AddMessageErreur(tmeSERIES, cmeERROR, NS, Format(MSG_ERR_ANGLE_INVALIDE, [NS, i, 'Inclinaison', MyVisee.Pente]));

        if (errvLG        in YaErreur) then AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(MSG_ERR_LRUD_TOO_BIG, [NS, i, 'LG', MyVisee.LG]));
        if (errvLD        in YaErreur) then AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(MSG_ERR_LRUD_TOO_BIG, [NS, i, 'LD', MyVisee.LD]));
        if (errvHZ        in YaErreur) then AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(MSG_ERR_LRUD_TOO_BIG, [NS, i, 'HZ', MyVisee.HZ]));
        if (errvHN        in YaErreur) then AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(MSG_ERR_LRUD_TOO_BIG, [NS, i, 'HN', MyVisee.HN]));
        // warnings: les LRUD data dépassent rarement 30 m
        if (errWarnLG     in YaErreur) then AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(MSG_ERR_LRUD_TOO_BIG, [NS, i, 'LG', MyVisee.LG]));
        if (errWarnLD     in YaErreur) then AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(MSG_ERR_LRUD_TOO_BIG, [NS, i, 'LD', MyVisee.LD]));
        if (errWarnHZ     in YaErreur) then AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(MSG_ERR_LRUD_TOO_BIG, [NS, i, 'HZ', MyVisee.HZ]));
        if (errWarnHN     in YaErreur) then AddMessageErreur(tmeSERIES, cmeWARNING, NS, Format(MSG_ERR_LRUD_TOO_BIG, [NS, i, 'HN', MyVisee.HN]));
      end;
    end;
    // longueur de série très faible -> avertir (il peut s'agir d'une série fictive, typiquement pour relier deux séries qui se croisent)
    if (QLongueurSerie < SEUIL_SERIE_TRES_COURTE) then  AddMessageErreur(tmeSERIES, cmeWARNING, NS,  Format(GetResourceString(rsCHK_MSG_VISEE_TRES_COURTE), [NS, N,  QLongueurSerie, SEUIL_SERIE_TRES_COURTE]));
    // longueur de série quasi nulle -> considérer comme une erreur
    if (QLongueurSerie < SEUIL_SERIE_LONGUEUR_NULLE) then  AddMessageErreur(tmeSERIES, cmeCRITICAL, NS, Format(GetResourceString(rsCHK_MSG_SERIE_DEV_NUL), [NS, N]));
  //except

  //end;
end;

function TToporobotStructure2012.CheckVisee(const V: TUneVisee; out MsgErr: string): TErrorVisee;
var
  MyCode: TCode;
  MyExpe: TExpe;
  WU: Int64;
  miou: double;
  CC: Integer;
  EE: Integer;
  MaxAzimutAccepte: double;
  function EWE(const QMsg: string; const QMini, QMaxi: double; const QUnit: string): string;
  begin
    Result := Format(rsCHECK_VISEE_VALID_INTERVAL + #13#10, [QMsg, QMini, QUnit, QMaxi, QUnit]);
  end;
begin
  MsgErr := '';
  Result := [];
  // on teste d'abord ce qui est indépendant des expés et codes
  // types de visée
  if (not IsInRange(Ord(V.TypeVisee), 0, Ord(tgVISEE_RADIANTE))) then
  begin
    MsgErr += Format(rsCHECK_VISEE_VALID_TYPE_VISEE + #13#10, [0, Ord(tgVISEE_RADIANTE)]);
    Result += [errvTYPEVISEE];
  end;
  //AfficherMessageErreur('--- CheckVisee: 002');
  // visée de longueur nulle
  if (V.Longueur < SEUIL_LONGUEUR_NULLE) then
  begin
    MsgErr += Format('Longueur = %.2f m', [V.Longueur]);
    Result += [errvLONGUEUR_NULLE];
  end;
  //AfficherMessageErreur('--- CheckVisee: 003');

  if (V.Longueur > SEUIL_LONGUEUR_MAXI_TOPOROBOT) then
  begin
    MsgErr += Format('Longueur = %.2f m', [V.Longueur]);
    Result += [errvLONGUEUR_OVERFLOW];
  end;
  // LRUD
  if (not IsInRange(V.LG, 0.00, SEUIL_LONGUEUR_MAXI_TOPOROBOT)) then
  begin
    MsgErr += EWE('Left distance', 0.00, SEUIL_LONGUEUR_MAXI_TOPOROBOT, 'm');
    Result += [errvLG];
  end;
  if (not IsInRange(V.LD, 0.00, SEUIL_LONGUEUR_MAXI_TOPOROBOT)) then
  begin
    MsgErr += EWE('Right distance', 0.00, SEUIL_LONGUEUR_MAXI_TOPOROBOT, 'm');
    Result += [errvLD];
  end;
  if (not IsInRange(V.HZ, 0.00, SEUIL_LONGUEUR_MAXI_TOPOROBOT)) then
  begin
    MsgErr += EWE('Upper distance', 0.00, SEUIL_LONGUEUR_MAXI_TOPOROBOT, 'm');
    Result += [errvHZ];
  end;
  if (not IsInRange(V.HN, 0.00, SEUIL_LONGUEUR_MAXI_TOPOROBOT)) then
  begin
    MsgErr += EWE('Downer distance', 0.00, SEUIL_LONGUEUR_MAXI_TOPOROBOT, 'm');
    Result += [errvHN];
  end;


  // LRUD warning
  if (V.LG > SEUIL_LRUD_WARN_LENGTH) then
  begin
    MsgErr += Format('Warning: %.3f exceeds %.3f', [V.LG, SEUIL_LRUD_WARN_LENGTH]);
    Result += [errWarnLG];
  end;
  if (V.LD > SEUIL_LRUD_WARN_LENGTH) then
  begin
    MsgErr += Format('Warning: %.3f exceeds %.3f', [V.LD, SEUIL_LRUD_WARN_LENGTH]);
    Result += [errWarnLD];
  end;
  if (V.HZ > SEUIL_LRUD_WARN_LENGTH) then
  begin
    MsgErr += Format('Warning: %.3f exceeds %.3f', [V.HZ, SEUIL_LRUD_WARN_LENGTH]);
    Result += [errWarnHZ];
  end;
  if (V.HN > SEUIL_LRUD_WARN_LENGTH) then
  begin
    MsgErr += Format('Warning: %.3f exceeds %.3f', [V.HN, SEUIL_LRUD_WARN_LENGTH]);
    Result += [errWarnHN];
  end;


  // existence de secteurs, codes et expés
  if (GetIdxSecteurByNumero(V.IDSecteur) = -1) then Result := Result + [errvSECTEUR];
  CC := GetIdxCodeByNumero(V.Code);
  EE := GetIdxExpeByNumero(V.Expe);
  if ((CC = -1) OR (EE = -1)) then
  begin
    if (CC = -1) then Result += [errvCODE];
    if (EE = -1) then Result += [errvEXPE];
    AfficherMessageErreur(Format('  -- Invalid code (%d) or trip (%d) -> check aborted', [CC, EE]));
    Exit;
  end;
  // codes, secteur et expés
  MyCode := GetCodeByNumero(V.Code);
  MyExpe := GetExpeByNumero(V.Expe);
  // azimuts
  case Trunc(MyCode.GradAz) of
    349, 350, 359, 360, 361: MaxAzimutAccepte := 360.00;
    389, 390, 399, 400, 401: MaxAzimutAccepte := 400.00;
  else
    MaxAzimutAccepte := 400.00;
  end;
  if (not IsInRange(V.Azimut, 0.00, MaxAzimutAccepte)) then
  begin
    MsgErr += EWE('Azimuth', 0.00, MaxAzimutAccepte, 'a.u.');
    Result += [errvAZIMUT];
  end;
  // pentes (le gros morceau)
  WU := Trunc(MyCode.GradInc);
  miou := MyCode.GradInc / 4; // quart de cercle dans l'unité de GradInc
  case WU of
    360, 400: // clinos classiques degré et grades
    begin
      if (not IsInRange(V.Pente, -miou, +miou)) then
      begin
        MsgErr += EWE('Inclination', -miou, +miou, 'a.u.');
        Result += [errvPENTE];
      end;
    end;
    790, 800:  // Lasermètres Stanley TLM330, directes et inverses
     begin
      if (not IsInRange(V.Pente, -SEUIL_LONGUEUR_MAXI_TOPOROBOT, +SEUIL_LONGUEUR_MAXI_TOPOROBOT)) then
      begin
        MsgErr += EWE('Difference in height', -miou, +miou, 'a.u.');
        Result += [errvPENTE];
      end;
    end;
  else
    ;
  end;
end;

procedure TToporobotStructure2012.RecalculerDeclinaisonsMagnetiquesExpes();
var
  CalculateurDeclimag : TSAGeoMag;
  i, NbE: Integer;
  EE: TEntrance;
  EX: TExpe;
  Lat, Lon, Decl: double;
  PTI, PTO: TPoint2Df;
  EPSG: TLabelSystemesCoordsEPSG;
  MyCentroide: TPoint3Df;
begin
  AfficherMessage(Format('%s.recalculerDeclinaisonsMagnetiquesExpes',[ClassName]));
  AfficherMessageErreur(Format('%s.recalculerDeclinaisonsMagnetiquesExpes',[ClassName]));
  CalculateurDeclimag := TSAGeoMag.Create;
  try
    // Etape 0: Démarrage du calculateur de déclinaison
    if (not CalculateurDeclimag.Initialiser('')) then
    begin
      AfficherMessage('Calculateur de déclinaisons magnétiques inopérant - Arrêt');
      exit;
    end;
    // calcul du centroïde des entrées
    MyCentroide := CalcCentroideEntrees();
    // qu'on convertit en WGS84
    PTI := MakeTPoint2Df(MyCentroide.X, MyCentroide.Y);
    PTO := MakeTPoint2Df(0.00, 0.00);
    // si la conversion réussit: Etape 2: Calcul des déclinaisons pour les expés
    EPSG := GetCodeEPSGSystemeCoordonnees();
    if (ConversionCoupleCoordonneesIsoleesEPSG(EPSG.CodeEPSG, CODE_EPSG_WGS84, PTI, PTO)) then
    begin
      Lat := PTO.X; Lon := PTO.Y;
      AfficherMessageErreur(Format('-- X = %.2f, Y = %.2f, Z = %.f - Lat = %.8f, Lon = %.8f', [MyCentroide.X, MyCentroide.Y, MyCentroide.Z, Lat, Lon]));
      AfficherMessage('');
      NbE := GetNbExpes();
      AfficherMessage(Format('-- Declinaisons pour les %d expes', [NbE]));
      {$WARNING: TEXpe.DateExpe à implementer}
      for i := 1 to NbE - 1 do
      begin
        EX   := GetExpe(i);
        EX.ModeDecl    := cdmAUTOMATIQUE;                           // on force en automatique
        // DONE: Vérifier le signe des déclinaisons dans CalculerDeclinaison();
        // Les déclinaisons dont toujours en degrés
        EX.DeclinaisonInDegrees := CalculateurDeclimag.CalculerDeclinaison(lat, lon, MyCentroide.Z, GetSecuredDate(EX.AnneeExpe, EX.MoisExpe, EX.JourExpe));
        AfficherMessageErreur(Format('Expe: %d: %.4d-%.2d-%.2d: %.8f', [EX.IDExpe, EX.AnneeExpe, EX.MoisExpe, EX.JourExpe, EX.DeclinaisonInDegrees]));
        PutExpe(i, EX);                                // et on met à jour la table
      end;
    end;
    CalculateurDeclimag.Finaliser;
  finally
    FreeAndNil(CalculateurDeclimag);//CalculateurDeclimag.Free;
  end;
end;

//***********************************
// recherche de l'index correspondant à un élément

function TToporobotStructure2012.GetIdxEntreeByNumero(const N: TNumeroEntrance): integer;
begin
  Result := n;
end;
function TToporobotStructure2012.GetIdxReseauByNumero(const N: TNumeroReseau): integer;
begin
  Result := n;
end;
function TToporobotStructure2012.GetIdxSecteurByNumero(const N: TNumeroSecteur): integer;
begin
  Result := n;
end;
function TToporobotStructure2012.GetIdxCodeByNumero(const N: TNumeroCode): integer;
var
  i  : integer;
  EWE: TCode;
begin
  Result := -1;
  for i := 0 to GetNbCodes - 1 do
  begin
    EWE := GetCode(i);
    if (EWE.IDCode = N) then
    begin
      Result := i;
      Break;
    end;
  end;
end;
function TToporobotStructure2012.GetIdxExpeByNumero(const N: TNumeroExpe): integer;
var
  i  : integer;
  EWE: TExpe;
begin
  Result := -1;
  for i := 0 to GetNbExpes - 1 do
  begin
    EWE := GetExpe(i);
    if (EWE.IDExpe = N) then Exit(i);
  end;
end;



function TToporobotStructure2012.GetIdxSerieByNumero(const qIdx: TNumeroSerie): integer;
var
  Nb: Integer;
  i: Integer;
  EWE: TObjSerie;
begin
  Result := -1;
  Nb := self.GetNbSeries;
  if (Nb = 0) then exit;
  try
    for i := 0 to Nb - 1 do
    begin
      EWE := self.GetSerie(i);
      if (qIdx = EWE.GetNumeroDeSerie()) then Exit(i);
    end;
  except
  end;
end;

//------------------------------------------------------------------------------
function TToporobotStructure2012.HasPtTopoBySerSt(const qSr, qPt: integer): boolean;
var
  S, P: Integer;
  mySerie: TObjSerie;
  myVisee: TUneVisee;
  EWE: Integer;
begin
  Result := False;
  for S := 0 to GetNbSeries - 1 do
  begin
    mySerie := GetSerie(S);
    EWE := mySerie.GetNumeroDeSerie;
    for P := 0 to mySerie.GetNbVisees - 1 do
    begin
      myVisee := mySerie.GetVisee(P);
      if ((qSr = EWE) and (qPt = P)) then
      begin
         Result := True;
         AfficherMessage(Format('Station found in %d.%d', [mySerie.GetNumeroDeSerie(), P]));
         Exit;
      end;
    end;
  end;
end;
// recherche du couple série station d'après ID littéral
function  TToporobotStructure2012.FindPtTopoByCle(const DoMatchExact: boolean;
                                                  var Cle: string;
                                                  var Sr: TNumeroSerie;
                                                  var Pt: integer): boolean;
var
  //Clef: string;
  S, P: integer;
  Q, R: string;
  mySerie: TObjSerie;
  myPoint : TUneVisee;
  WU: SizeInt;
  qSer: Integer;
  qSt: Integer;
  N: Integer;
  S1, S2: String;
  QFound: Boolean;
  QListFound: TStringList;
begin
  Result := False;
  Cle := UpperCase(Trim(Cle));
  // analyse de la clé
  // la clé contient un point --> recherche par série/station
  WU := Pos('.', Cle);
  if (WU > 0) then
  begin
    N := Length(Cle);
    S1 := Trim(Copy(Cle, 0, WU - 1));
    S2 := Trim(Copy(Cle, WU + 1, N));
    qSer := StrToIntDef(S1, 0);
    qSt  := StrToIntDef(S2, 0);
    if (HasPtTopoBySerSt(qSer, qSt)) then
    begin
      Sr := qSer;
      Pt := qSt;
      Exit(True);
    end;
  end;
  // station non trouvee ? Tentative de Recherche par clé
  Sr := -1;
  Pt := -1;
  Result := False;
  for S := 0 to GetNbSeries - 1 do
  begin
    mySerie := GetSerie(S);
    for P := 0 to mySerie.GetNbVisees - 1 do
    begin
      myPoint := mySerie.GetVisee(P);
      // recherche sur IDTerrain et commentaires
      Q  := UpperCase(Trim(myPoint.IDTerrainStation + '|' + myPoint.Commentaires));  // recherche sur une partie de la clé
      R  := UpperCase(Trim(myPoint.IDTerrainStation)); // recherche exacte
      QFound := IIF(DoMatchExact, (R = Cle), (Pos(Cle,  Q) > 0));
      if (QFound) then
      begin
         Sr := mySerie.GetNumeroDeSerie();
         Pt := P;
         Cle := Q;
         Exit(True);
      end;
    end;
  end;
end;

function TToporobotStructure2012.FindIndexesSeriesRattachedAtSerie(const Idx: integer; out LS: TListOfIntegers): boolean;
var
  n, i: Integer;
  MySerie, QSerie: TObjSerie;
  QNumSer: TNumeroSerie;
begin
  Result := false;
  AfficherMessage(Format('%s.FindSeriesRattachedAtSerie: %d', [ClassName, Idx]));
  LS.ClearListe(); // par sécurité
  n := self.GetNbSeries();
  MySerie := GetSerie(Idx);  // attrapper la série courante
  QNumSer := MySerie.GetNumeroDeSerie();
  LS.AddElement(QNumSer);    // on accroche en tête la série courante
  for i := 0 to n - 1 do
  begin
    QSerie := GetSerie(i);
    if (QNumSer = QSerie.GetNoSerieDep()) then LS.AddElement(QSerie.GetNumeroDeSerie());
    if (QSerie.GetNoSerieArr() = QNumSer) then LS.AddElement(QSerie.GetNumeroDeSerie());
  end;
  Result := (LS.Count > 0);
end;

// Infos sur les données topo
// ne nécessite pas de calcul topo
// c'est du fichier de log -> messages en anglais uniquement
function TToporobotStructure2012.GetInfosCavite(const LS: TStrings): integer;
const
  FMTLNENTREE = FORMAT_NB_INTEGER+
                TAB+FORMAT_NB_REAL_2_DEC+
                TAB+FORMAT_NB_REAL_2_DEC+
                TAB+FORMAT_NB_REAL_2_DEC+TAB + FORMAT_STRING;
  FMTLNSER    = FORMAT_NB_INTEGER+
                TAB+FORMAT_NB_INTEGER+
                TAB+FORMAT_NB_INTEGER+
                TAB+FORMAT_NB_INTEGER+
                TAB+FORMAT_NB_INTEGER+TAB+'%-60s'+
                TAB+FORMAT_NB_INTEGER+
                TAB+FORMAT_NB_REAL_2_DEC+
                TAB+FORMAT_NB_REAL_2_DEC;
var
  i, n: integer;
  E: TEntrance;
  Ser: TObjSerie;
  V: TUneVisee;
  D1, DevTotal: double;
  VolumeTotal : double;
  VolumeSerie : double;
  NbPtsTotal: Integer;
begin
  Result:=-1;
  AfficherMessage(Format('%s.GetCaveInfoSeries()',[ClassName]));
  LS.Clear;
  try
    LS.Add(Format('File created  %s - %s',[DateToStr(Now), TimeToStr(Now)]));
    LS.Add('');
    LS.Add('Database : ' + FDatabaseName);
    LS.Add('');
    LS.Add('Survey: ' + GetNomEtude);
    LS.Add('');
    LS.Add(Format('List of %d entrances', [GetNbEntrances()]));
    LS.Add('');
    for i:=0 to GetNbEntrances() - 1 do begin
      E := GetEntrance(i);
      LS.Add(Format(FMTLNENTREE, [i, E.eXEntree, E.eYEntree, E.eZEntree, E.eNomEntree]));
    end;
    LS.Add('');
    LS.Add(Format('List of %d series', [GetNbSeries]));
    LS.Add('');
    LS.Add('No'+TAB+'S.Dep'+TAB+'Pt.Dep'+TAB+'S.Arr'+TAB+'Pt.Arr'+TAB+
           'Serie name'+TAB+'Nb points'+TAB+'Length'+
           TAB + 'Estimated volume');
    LS.Add('');
    DevTotal:=0.00;
    NbPtsTotal:=0;
    VolumeTotal:=0.00;
    for i:=0 to GetNbSeries() - 1 do
    begin
      Ser:=GetSerie(i);
      // calcul de la longueur, de la surface et du volume de la série
      D1:=0.00;
      VolumeSerie:=0.00;
      for n:=0 to Ser.GetNbVisees() - 1 do
      begin
        V:=Ser.GetVisee(n);
        // les sections considérées pour les volumes
        // sont des ellipses    (A = pi . a . b)
        case V.TypeVisee of
          tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON: // galeries naturelles
          begin;
            D1          += V.Longueur; // X += N -> X := X + N
            VolumeSerie += PI * (V.HZ+V.HN) * (V.LD + V.LG) * V.Longueur;
          end;
        else
          pass;
        end;
      end;
      VolumeTotal   += VolumeSerie;
      DevTotal      += D1;
      NbPtsTotal    += Ser.GetNbVisees() - 1;
      LS.Add(Format(FMTLNSER,
                    [Ser.GetNumeroDeSerie(),
                     Ser.GetNoSerieDep, Ser.GetNoPointDep,
                     Ser.GetNoSerieArr, Ser.GetNoPointArr,
                     Ser.GetNomSerie,
                     Ser.GetNbVisees - 1,
                     D1, VolumeSerie]));
    end;  // for i:=0 to TableSeries.Count-1
    LS.Add('');
    LS.Add(Format('Total length: %.2f m (%d stations) - Estimated Volume: %.1f m3',
                  [DevTotal, NbPtsTotal, VolumeTotal]));
    Result:=1;
  except
  end;
end;

// l'export vers Visual Topo est désormais du ressort de cette classe
// 27/05/2015: Petit fix de l'unité des azimuts (Degrés décimaux = 'Degd' et non 'Deg')
// 29/01/2019: Adaptation à la version 5.08 de VTopo
procedure TToporobotStructure2012.ExportVisualTopo(const FichierTRO: TStringDirectoryFilename;
                                                   const DoExportViseesRadiantes: boolean);
const
  DEG_DEC = 'Degd';
  GRADS   = 'Gra';
  VTOPO_VERSION = 'Version 5.08';
  CLITO_VULCAIN = 'Vulc';
  CLITO_STD     = 'Clino';
  CLITO_DENIV   = 'Deniv';
var
  pTRO: TextFile;
  MyPalette256: TPalette256;
  Entree: TEntrance;
  DefautCouleur: TColor;
  Serie: TObjSerie;
  CurrStation, PrevStation: TUneVisee;
  FromStationID, ToStationID: String;
  S777: String;
  S666: String;
  NoSer: Integer;
  NoSt, NbA, j: Integer;
  Code: TCode;
  Expe: TExpe;
  EWE: String;
  ArrAntennesFound: TArrayOfTViseeAntenne;
  MyAntenne: TViseeAntenne;
  QNumeroDeSerie: TNumeroSerie;
  Toto: Boolean;
  procedure WriteLnParam(const C: TCode; const E: TExpe);
  var
    UB, UC, Clino   : string;
    QSensViseeAzimut: string;
    QSensViseePente : string;
    QSensViseeLRUD  , Fuck_The_Christ: string;
    CC: TColor;
    Decl: double;
  begin
    writeLn(pTRO,'');
    //Param Deca Deg Clino Deg 1.0300 Dir,Dir,Dir Std

    case Trunc(C.GradAz) of
      349, 350, 360: UB := DEG_DEC;
      389, 390, 400: UB := GRADS;
    else
      UB := GRADS;
    end;
    case Trunc(C.GradAz) of
      350, 390:
        QSensViseeAzimut := 'Inv';
    else
        QSensViseeAzimut := 'Dir';
    end;
    QSensViseeLRUD := QSensViseeAzimut;

    QSensViseePente := 'Dir';
    case Trunc(C.GradInc) of
       350: begin
              Clino := CLITO_STD; //'Clino';
              UC    := DEG_DEC;
              QSensViseePente := 'Inv';
            end;
       360: begin
              Clino := CLITO_STD; //'Clino';
              UC    := DEG_DEC;
            end;
       361: begin
              Clino := CLITO_VULCAIN; //'Vulc';
              UC    := DEG_DEC;
            end;
       400: begin
              Clino := CLITO_STD; //'Clino';
              UC    := GRADS;
            end;
       401: begin
              Clino := CLITO_VULCAIN; //'Vulc';
              UC    := GRADS;
            end;
       380: begin
              Clino := CLITO_DENIV; //'Deniv'; // anciennement: Prof
              UC    := '';
            end;
       UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR,
       UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV:
            begin
              Clino := CLITO_DENIV; //'Deniv'; // anciennement: Prof
              UC    := '';
            end;

    end;

    Decl:= E.DeclinaisonInDegrees; // (E.Declinaison/10) * 400.0 / C.GradAz;
    CC  := MyPalette256.GetColorByIndex(E.IdxCouleur);

    Fuck_The_Christ := Format('Param Deca %s ', [UB]);
    Fuck_The_Christ += Format('%s ', [ Trim(Clino + ' ' + UC)]);


    Fuck_The_Christ += Format('%.4f ', [Decl]);

    Fuck_The_Christ += Format('%s,%s,%s ', [QSensViseeAzimut, QSensViseePente, QSensViseeLRUD]);
    Fuck_The_Christ += Format('%s ', ['Arr']);
    Fuck_The_Christ += Format('%d,%d,%d ', [Red(CC), Green(CC), Blue(CC)]);
    {$WARNING: TEXpe.DateExpe à implementer}
    Fuck_The_Christ += Format('%.2d/%.2d/%.4d ', [E.JourExpe, E.MoisExpe, E.AnneeExpe]);
    Fuck_The_Christ += Format('%s ', ['M']);
    Fuck_The_Christ += Format(';%s', ['']);
    WriteLn(pTRO, Fuck_The_Christ);
    writeln(pTRO,'');
  end;
begin
  AfficherMessage(Format('%s.ExportVisualTopo(%s)',[ClassName, FichierTRO]));
  setlength(ArrAntennesFound, 0);
  // démarrage de la palette
  MyPalette256 := TPalette256.Create;
  MyPalette256.GenerateTOPOROBOTPalette();
  // ouverture fichier VTopo
  AssignFile(pTRO, FichierTRO);
  try

    ReWrite(pTRO);
    WriteLn(pTRO, VTOPO_VERSION);
    WriteLn(pTRO, '');
    // Entrée
    Entree := GetEntrance(0);
    //----------------------------------------------------------------------------------
    Serie := GetSerie(1);
    EWE := Format(FMTSERST_VTOPO,[Serie.GetNoSerieDep, Serie.GetNoPointDep]);
    // Trou CDS03,0.000,0.000,0,
    WriteLn(pTRO, Format('Trou %s,%.3f,%.3f,%f,', [Entree.eNomEntree, Entree.eXEntree / 1000, Entree.eYEntree / 1000, Entree.eZEntree]));
    WriteLn(pTRO, Format('Entree %s',[Format(FMTSERST_VTOPO, [Entree.eRefSer, Entree.eRefSt])]));
    WriteLn(pTRO, Format('Club %s',[rsGHTOPOEXENAME]));
    DefautCouleur:= clRED;
    WriteLn(pTRO, Format('Couleur %d,%d,%d',[Red(DefautCouleur), Green(DefautCouleur), Blue(DefautCouleur)]));

    // première ligne de données
    WriteLn(pTRO,'Param Deca Degd Clino Deg 0.0000 Dir,Dir,Dir Inc Std --/--/---- M');
    Serie := GetSerie(1);
    WriteLn(pTRO, Format('%10s %10s %.1f   %.1f %.1f %.1f %.2f %.2f %.2f %s',
                               [EWE, EWE,
                                0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 1.00,
                                'Entrée ' + EWE]));
    // exportation proprement dite
    for NoSer:=1 to GetNbSeries() - 1 do
    begin
      Serie := GetSerie(NoSer);
      QNumeroDeSerie := Serie.GetNumeroDeSerie();
      AfficherMessage(Format('--> Export serie: %d - %s',[Serie.GetNumeroDeSerie(), Serie.GetNomSerie]));
      FromStationID  := Format(FMTSERST_VTOPO, [Serie.GetNoSerieDep(), Serie.GetNoPointDep()]);
      for NoSt := 1 to Serie.GetNbVisees() - 1 do
      begin
        CurrStation := Serie.GetVisee(NoSt);
        Code := GetCodeByNumero(CurrStation.Code);
        Expe := GetExpeByNumero(CurrStation.Expe);
        if ((NoSer=1) and (NoSt=1)) then WriteLnParam(Code, Expe);
        if (CurrStation.Code <> PrevStation.Code) OR (CurrStation.Expe <> PrevStation.Expe)
        then WriteLnParam(Code, Expe);
        ToStationID:=Format(FMTSERST_VTOPO,[Serie.GetNumeroDeSerie(), NoSt]);
        if (NoSt = Serie.GetNbVisees - 1) then ToStationID := Format(FMTSERST_VTOPO,[Serie.GetNoSerieArr, Serie.GetNoPointArr]);
        S777 := IIF((Trim(CurrStation.Commentaires)='')     , '', Format(FORMAT_STRING,[CurrStation.Commentaires]));
        S666 := IIF((Trim(CurrStation.IDTerrainStation) =''), '', Format('[%s]',[CurrStation.IDTerrainStation]));

        if (Trim(CurrStation.Commentaires)='') and (Trim(CurrStation.IDTerrainStation)='')
        then S777 := ''
        else S777 := ';' + S666 + S777 + ';';
        WriteLn(pTRO, Format('%10s %10s %.2f   %.2f %.2f %.2f %.2f %.2f %.2f N I * %s',
                             [FromStationID,  ToStationID,
                              CurrStation.Longueur, CurrStation.Azimut, CurrStation.Pente,
                              CurrStation.LG, CurrStation.LD, CurrStation.HZ, CurrStation.HN,
                              S777]));
        PrevStation    := CurrStation;
        FromStationID  := ToStationID;
        // Traitement des visées radiantes
        if (DoExportViseesRadiantes) then
        begin
          // ignorer les antennes de la dernière visée dont la série arrive sur un point connu
          Toto := True;
          if (NoSt = Serie.GetNbVisees() - 1) then Toto := (NoSt = Serie.GetNoPointArr());

          NbA := ExtractViseesAntennesOfStation(QNumeroDeSerie, NoSt, ArrAntennesFound);
          //202_7      *                         3.60  266.00  -20.00      *      *      *      * N I M
          //202_7      *                         3.90  211.00  -26.00      *      *      *      * N I M
          if (Toto And (NbA > 0)) then
          begin
            for j := 0 to NbA - 1 do
            begin
              MyAntenne := ArrAntennesFound[j].VA;
              FromStationID:=Format(FMTSERST_VTOPO,[MyAntenne.SerieDepart, MyAntenne.PtDepart]);
              WriteLn(pTRO, Format('%10s %10s %.2f   %.2f %.2f     *      *      *      * N I M %s',
                               [FromStationID, '        *',
                                MyAntenne.Longueur, MyAntenne.Azimut, MyAntenne.Pente,  '']));
            end;
          end;
        end; // if (DoExportViseesRadiantes) then
      end; // for NoSt:=1 to Serie.GetNbVisees() - 1 do
    end; // for NoSer:=1 to GetNbSeries() - 1 do

    // footpage
    WriteLn(pTRO,'');
    WriteLn(pTRO,'[Configuration 4.9]');
    WriteLn(pTRO,'');
    WriteLn(pTRO,'Visual Topo=2,3,-1,-1,-1,-1,1,1,801,610');
    WriteLn(pTRO,'Options=1,1');
    WriteLn(pTRO,'Calcul=0,1,-1,-1,-1,-1,22,22,844,449');
    WriteLn(pTRO,'Options=26,1,0,1,0');
    WriteLn(pTRO,'ExportDxf=0,100,391,1,6,7,4,4,3,3,2,7,9');
    WriteLn(pTRO,'Colonnes=8.00,8.00,8.00,8.00,8.00,8.00,8.00,8.00,8.00,'+
                 '8.00,8.00,2.00,4.00,0.38,8.00,8.00,8.00,8.00,8.00,'+
                 '0.38,0.00,0.00');
    WriteLn(pTRO,'Options=384,1.00,4,0,2,6914,1');
    AfficherMessage(Format('%s.ExportVTOPO(%s) OK',[ClassName, FichierTRO]));
    MyPalette256.Finaliser();
  finally
    CloseFile(pTRO);
    FreeAndNil(MyPalette256);//Palette256.Free;
  end;
end;

procedure TToporobotStructure2012.GenererThConfig(const THConfig, QFichierTH, QFichierXVI: TStringDirectoryFilename;
                                                  const QScale: integer;
                                                  const QGridSize: double);
var
  fp: TextFile;
begin
  // encoding  utf-8
  // source Toporabot1_TLM330_accents_secteurs.th
  // export model -fmt loch # Toporabot1_TLM330_accents_secteurs.lox
  assignFile(fp, THConfig);
  try
    ReWrite(fp);
    WriteLn(fp, 'encoding ' + 'utf-8');
    WriteLn(fp, 'source ' + ExtractFileName(QFichierTH));
    WriteLn(fp, 'export model -fmt loch');
	WriteLn(fp, 'export model -fmt kml');
    WriteLn(fp, 'layout xvi-export');
    WriteLn(fp, Format('  scale %d %d', [1, QScale]));
    WriteLn(fp, Format('  grid-size %.0f %.0f %.0f %s', [QGridSize, QGridSize, QGridSize, 'm']));
    WriteLn(fp, 'endlayout ');
    //WriteLn(fp, 'layout map_layout');
    //WriteLn(fp, 'endlayout ');

    WriteLn(fp, Format('export map -fmt %s -layout xvi-export -o %s', ['xvi', QFichierXVI]));
    //export map -o map.pdf -layout map_layout
    (*
    layout map_layout
    endlayout

   export map -fmt xvi -layout xvi-export -o Polygonale.xvi
    export map -o map.pdf -layout map_layout
    //*)
  finally
    CloseFile(fp);
  end;
end;



procedure TToporobotStructure2012.GenererDossierTherion(const DossierTherion: TStringDirectoryFilename;
                                                        const QScale: integer;
                                                        const QGridSize: double);
const
  NOM_CENTERLINE = 'centerlines';
var
  EWE         : TStringDirectoryFilename;
  FichierTH   : TStringDirectoryFilename;
  FichierXVI  : TStringDirectoryFilename;
  FichierTHConfig: String;
begin
  AfficherMessage(Format('%s.GenererDossierTherion: %s', [ClassName, DossierTherion]));
  ForceDirectories(DossierTherion);
  EWE := DossierTherion + PathDelim + NOM_CENTERLINE;
  FichierTH  := EWE + '.th';
  FichierXVI := EWE + '.xvi';
  FichierTHConfig :=  DossierTherion + PathDelim + 'thconfig';
  GenererThConfig(FichierTHConfig, FichierTH, FichierXVI, QScale, QGridSize);
  ExportVersTherionTH(FichierTH);
end;




procedure TToporobotStructure2012.CheckerLesAntennes();
var
  Nb: Integer;
begin
  Nb := GetNbAntennes();
  AfficherMessage(Format('-- %s.CheckerLesAntennes: %d visees rayonnantes',[ClassName, Nb]));

end;
// TODO: Contrôles à implémenter
procedure TToporobotStructure2012.CheckerLesCodes();
var
  i, Nb: Integer;
  MyCode: TCode;
  function CheckerUnCode(const QC: TCode): boolean;
  begin

  end;
begin
  Nb := GetNbCodes();
  AfficherMessage(Format('-- %s.CheckerLesCodes: %d codes',[ClassName, Nb]));
  if (0 = Nb) then
  begin
    MakeCode0(0, 'Code 0');
    MakeCode0(1, 'Code 1 regenerated');
    Exit;
  end;
end;

// TODO: Contrôles à implémenter
procedure TToporobotStructure2012.CheckerLesExpes();
var
  i, Nb: Integer;
  MyExpe: TExpe;
  function CheckerUneExpe(const QC: TExpe): boolean;
  begin

  end;
begin
  Nb := GetNbExpes();
  AfficherMessage(Format('-- %s.CheckerLesExpes: %d expés',[ClassName, Nb]));
  if (0 = Nb) then
  begin
    MakeExpe0(0, 'Expe 0');
    MakeExpe0(1, 'Expe 1 regenerated');
    Exit;
  end;
end;

// TODO: Export vers Thérion A valider
procedure TToporobotStructure2012.ExportVersTherionTH(const FichierTH: TStringDirectoryFilename);
const
  TH_KW_CENTERLINE     = 'centerline';
  TH_KW_ENDCENTERLINE  = 'end' + TH_KW_CENTERLINE;
var
  THF      : TextFile;
  MySerie  : TObjSerie;
  MyFixPt  : TEntrance;
  // Station de rattachement courante
  MaStationDeRattachement: string;
  i           , Nb: integer;
  MyGCS: TLabelSystemesCoordsEPSG;
  procedure WrtLn(const Line: string);
  begin
    WriteLn(THF, Line);
  end;
  // définition d'un en-tete de séance
  procedure DefineExpe(const E: TExpe);
  begin
    WrtLn(Format('       # Expe %d: %s',[E.IDExpe, E.Commentaire]));
    {$WARNING: TEXpe.DateExpe à implementer}
    WrtLn(Format('       date %.4d.%.2d.%.2d',[ E.AnneeExpe,  E.MoisExpe,  E.JourExpe]));
    WrtLn('       #-------------------');
  end;
  // definition d'un code - Valeur de retour par défaut: -1, sinon, un angle droit
  function DefineCode(const C: TCode): double;
  const
    UNITE_ANGULAIRE_GRADES   = 'grads';
    UNITE_ANGULAIRE_DEGRES   = 'degrees';
    DIRECT_VISEE             = '';
    BACKSIDE_VISEE           = 'back';
    MODE_DENIVELE            = 'depthchange';
    MODE_DIVING              = 'diving';
  var
    UnitCompass   : string;
    UnitClino     : string;
    IsBackCompass : string;
    IsBackClino   : string;
    InstrVertical : string; // mesures altimétriques
    DepthMode     : boolean; // si DepthMode=True, ne pas spécifier les unités de clino
    InstrLongueur : string;
    ModeMesures   : string; // normal = longueurs; diving = profondimètres
  begin
    Result := 0.00;
    WrtLn(Format('       # Code %d: %s',[C.IDCode, C.Commentaire]));
    InstrLongueur := 'tape';
    ModeMesures   := 'normal';
    // unités de la boussole
    case Trunc(C.GradAz) of
      359, 360: begin  UnitCompass := UNITE_ANGULAIRE_DEGRES;   IsBackCompass := DIRECT_VISEE; end; // visées directes en degrés
      399, 400: begin  UnitCompass := UNITE_ANGULAIRE_GRADES;   IsBackCompass := DIRECT_VISEE; end; // visées directes en grades
      349, 350: begin  UnitCompass := UNITE_ANGULAIRE_DEGRES;   IsBackCompass := BACKSIDE_VISEE; end; // visées inverses en degrés
      389, 390: begin  UnitCompass := UNITE_ANGULAIRE_GRADES;   IsBackCompass := BACKSIDE_VISEE; end;  // visées inverses en grades
    else        begin UnitCompass  := UNITE_ANGULAIRE_DEGRES;   IsBackCompass := DIRECT_VISEE; end;
    end;
    WrtLn('       units compass ' + UnitCompass);
    // unités du clinomètres
    InstrVertical := 'clino';
    IsBackClino   := DIRECT_VISEE;
    DepthMode     := false;
    case Trunc(C.GradInc) of
      350: begin   UnitClino := UNITE_ANGULAIRE_DEGRES;        IsBackClino := BACKSIDE_VISEE; end; // zéro à l'horizontale degré inverse
      390: begin   UnitClino := UNITE_ANGULAIRE_GRADES;        IsBackClino := BACKSIDE_VISEE; end; // zéro à l'horizontale grade inverse
      360: begin   UnitClino := UNITE_ANGULAIRE_DEGRES;                                       end; // zéro à l'horizontale degré direct
      400: begin   UnitClino := UNITE_ANGULAIRE_GRADES;                                       end; // zéro à l'horizontale grade direct
      361: begin   UnitClino := UNITE_ANGULAIRE_DEGRES;        Result    :=   90.00;          end; // zéro zénithal degré
      401: begin   UnitClino := UNITE_ANGULAIRE_GRADES;        Result    :=  100.00;          end; // zéro zénithal grade
      359: begin   UnitClino := UNITE_ANGULAIRE_DEGRES;        Result    :=  -90.00;          end; // zéro nadiral degré
      399: begin   UnitClino := UNITE_ANGULAIRE_GRADES;        Result    := -100.00;          end; // zéro nadiral grade
      370: begin   pass;                                                                      end; // pourcentages
      380: begin
             InstrVertical := MODE_DENIVELE;
             ModeMesures   := MODE_DIVING;
             DepthMode     := True;
           end; // différences d'altitudes
      UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV:
           begin
             InstrVertical := MODE_DENIVELE;
             ModeMesures   := MODE_DIVING;
             IsBackClino   := BACKSIDE_VISEE;
             DepthMode     := True;
           end;
      UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR:
           begin
             InstrVertical := MODE_DENIVELE;
             ModeMesures   := MODE_DIVING;
             DepthMode     := True;
           end;
    else
      begin
        UnitClino   := UNITE_ANGULAIRE_DEGRES;
        IsBackClino := '';
      end;
    end;
    if (Not DepthMode) then WrtLn('       units clino ' + UnitClino);
    WrtLn(Format('       data %s from to %s %scompass %s%s right left ceiling floor',
          [ModeMesures,
           InstrLongueur,
           IsBackCompass,
           IsBackClino,
           InstrVertical
          ]));
    WrtLn('       #-------------------');
    WrtLn('');
  end;
  // définition d'une série
  procedure DefineSerie(const S: TObjSerie; const N: integer; const StationDeRattachement: string = '');
  var
    Qdr, Incl    : double;
    MyExpe       : TExpe;
    MyCode       : TCode;
    MyStation    : TUneVisee;
    St           : integer;
    OldIdxExpe   : integer;
    OldIdxCode   : integer;
    OldIDStation : string;
    CurIDStation : string;
    Tag, foo, bar, QS0, QS1: string; // Commentaires et ID littéraux de stations
    miou: Int64;
    lulu: Double;
  begin
    AfficherMessage(Format('----> Serie %d: %s',[S.GetNumeroDeSerie(), S.GetNomSerie]));
    WrtLn('');
    WrtLn(Format('    # Serie: %d - %s',[S.GetNumeroDeSerie(), S.GetNomSerie]));
    WrtLn('    #========================');
    WrtLn('    ' + TH_KW_CENTERLINE);
    WrtLn('');
    // stations
    OldIdxExpe  := -1;
    OldIdxCode  := -1;

    OldIDStation:= Format(FMTSTS, [S.GetNoSerieDep, S.GetNoPointDep]);
    // première station
    MyStation   := S.GetVisee(1);
    MyExpe      := GetExpeByNumero(MyStation.Expe);
    MyCode      := GetCodeByNumero(MyStation.Code);
    OldIdxExpe  := MyExpe.IDExpe;
    OldIdxCode  := MyCode.IDCode;
    DefineExpe(MyExpe);
    Qdr := DefineCode(MyCode);
    MyStation := S.GetVisee(1);

    OldIDStation := Format(FMTSTS, [S.GetNoSerieDep(), S.GetNoPointDep()]);
    CurIDStation := Format(FMTSTS, [S.GetNumeroDeSerie(), 0]);
    if (OldIDStation <> CurIDStation) then WrtLn(Format('       %s %s %.2f %.2f %.2f  %.2f %.2f  %.2f %.2f  %s',
                                     [OldIDStation, CurIDStation,
                                      0.005, MyStation.Azimut, MyStation.Pente,
                                      0.00, 0.00, 0.00, 0.00, '']));


    for St := 1 to S.GetNbVisees - 1 do
    begin
      MyStation := S.GetVisee(St);
      //if NoSt=Serie.PointsTopo.Count-1 then ToStationID:=Format(FMTSERSTVTOPO,[S.SerieArr, S.PtArr])
      if (St = S.GetNbVisees - 1) then
        CurIDStation := Format(FMTSTS, [S.GetNoSerieArr, S.GetNoPointArr])
      else
        CurIDStation := Format(FMTSTS, [S.GetNumeroDeSerie(), St]);
      MyExpe      := GetExpeByNumero(MyStation.Expe);
      MyCode      := GetCodeByNumero(MyStation.Code);
      if (MyExpe.IDExpe <> OldIdxExpe) then
      begin
        DefineExpe(MyExpe);
        OldIdxExpe  := MyExpe.IDExpe;
      end;

      if (MyCode.IDCode <> OldIdxCode) then
      begin
        Qdr := DefineCode(MyCode);
        OldIdxCode  := MyCode.IDCode;
      end;
      if Trunc(Qdr)=0 then
        Incl := MyStation.Pente
      else begin
        if (Qdr>1.00) then Incl := Qdr - MyStation.Pente
                      else Incl := MyStation.Pente + Qdr;
      end;
      // Lasermètres de bâtiment => Utilisation des longueurs projetées
      miou := Trunc(MyCode.GradInc);
      if ((miou = UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV) or
          (miou = UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR)) then
        lulu := Hypot2D(MyStation.Longueur, MyStation.Pente)
      else
        lulu := MyStation.Longueur;
      // ID Littéraux
      Tag := '';
      if (MyStation.IDTerrainStation <> '') then Tag := Format('# Label: %s ',[MyStation.IDTerrainStation]);
      if (MyStation.Commentaires <> '')     then Tag := Tag + Format('# Observ: %s', [MyStation.Commentaires]);
      WrtLn(Format('       %s %s %.2f %.2f %.2f  %.2f %.2f  %.2f %.2f  %s',
                   [OldIDStation, CurIDStation,
                    lulu, MyStation.Azimut, Incl,
                    MyStation.LG, MyStation.LD, MyStation.HZ, MyStation.HN,
                    Tag]));
      //--------------------------------------------------------------
      OldIDStation := CurIDStation;
    end;
    WrtLn('');
    WrtLn('    ' + TH_KW_ENDCENTERLINE);
    foo := Format(FMTSTS, [S.GetNoSerieDep, S.GetNoPointDep]);
    bar := Format('%s@SERIE%d',[foo, S.GetNoSerieDep]);
  end;
begin
  AfficherMessage(Format('%s.ExportToTherion(%s)',[ClassName, FichierTH]));
  AssignFile(THF, FichierTH);
  try
    ReWrite(THF);
    AfficherMessage('--> Header');
    // encodage
    WrtLn('encoding  iso8859-2');
    WrtLn('');
    // date et nom du dossier
    WrtLn(StringOfChar('#', 80));
    WrtLn('## Data file for Therion generated by GHTopo');
    WrtLn('## Folder: ' + FDatabaseName);
    WrtLn('## Date  : ' + DatePascalToDateHeureCondensee(Now()));
    WrtLn(StringOfChar('#', 80));
    WrtLn('');
    // centerlines et consorts
    WrtLn(Format('survey Survey001 -title "%s"',[GetNomEtude]));

    // points fixes
    AfficherMessage('--> Fixed points');
    WrtLn('  # Section Entrances and Fixed points');
    WrtLn('  ' + TH_KW_CENTERLINE);
    // système de coordonnées
    MyGCS := GetCodeEPSGSystemeCoordonnees();
    WrtLn('  # Coordinates system');
    WrtLn(format('  cs EPSG:%d # %s', [MyGCS.CodeEPSG, MyGCS.NomEPSG]));
    // entrées et points fixes
    Nb := GetNbEntrances();
    for i := 0 to  Nb - 1 do
    begin
      MyFixPt := GetEntrance(i);
      WrtLn(Format('    fix %s %.2f %.2f %.2f # %s',
                   [ Format(FMTSTS, [MyFixPt.eRefSer, MyFixPt.eRefSt]),
                     MyFixPt.eXEntree,
                     MyFixPt.eYEntree,
                     MyFixPt.eZEntree,
                     MyFixPt.eNomEntree
                   ]));
    end;
    WrtLn('  ' + TH_KW_ENDCENTERLINE);
    WrtLn('  # End section Entrances and Fixed points');
    WrtLn('  # Section SERIES');
    // séries
    AfficherMessage('--> Series');
    MaStationDeRattachement := '';
    Nb := GetNbSeries();
    for i := 1 to Nb - 1 do
    begin
      MySerie := GetSerie(i);
      DefineSerie(MySerie, i, MaStationDeRattachement);
    end;
    WrtLn('  # End section SERIES');
    WrtLn('endsurvey');
  finally
    CloseFile(THF);
  end;
end;

// ouverture d'un fichier XML (gtx)
function  TToporobotStructure2012.LoadFromXML(const FichierXML: TStringDirectoryFilename): integer;
var
  EPSGDefault: TLabelSystemesCoordsEPSG;
  MyDocXML   : TXMLDocument;
  GHTopoRoot : TDOMElement;
  // items de tables simples
  UnNameSpace           : TNameSpace;
  UnFiltrePerso         : TFiltrePersonnalise;
  UneEntree             : TEntrance;
  UnReseau              : TReseau;
  UnSecteur             : TSecteur;
  UneExpe               : TExpe;
  UnCode                : TCode;
  UneViseeAntenne       : TViseeAntenne;
  UneSerie              : TObjSerie;
  UneVisee              : TUneVisee;
  // noeuds
  EWE, WU, WOK, QAT     : TDOMNode;
  // listes DOM
  ListeRubriquesGeneral : TDOMNodeList;
  ListeDesFiltres       : TDOMNodeList;
  ListeDesEntrees       : TDOMNodeList;
  ListeDesReseaux       : TDOMNodeList;
  ListeDesSecteurs      : TDOMNodeList;
  ListeDesExpes         : TDOMNodeList;
  ListeDesCodes         : TDOMNodeList;
  ListeDesSeries        : TDOMNodeList;
  ListeDesAntennes      : TDOMNodeList;
  ListeStations         : TDOMNodeList;
  ListeNamespaces       : TDOMNodeList;
  QMyDate: TDateTime;
  St, i: Integer;
  qSerDepart, qPtDepart, qSerArrivee, qPtArrivee: Integer;
  qIdxSerie: TNumeroSerie;
  procedure KWYXZ(const S666: string); inline;
  begin
    AfficherMessage(Format('-- Found "%s" section', [S666]));
  end;
  function QExtractTextContent(var QQQ: TDOMNode): string; inline;
  begin
    Result := Trim(PasStrToXMLStr(QQQ.TextContent));
  end;

  function ExtractStringOfNode(var QN: TDOMNode; const XMLKey: string): string;
  var
    QIN1: TDOMNode;
  begin
    QIN1 := QN.Attributes.GetNamedItem(XMLKey);
    Result := QExtractTextContent(QIN1);
  end;
  function ExtractIntOfNode(var QN: TDOMNode; const XMLKey: string; const QDefaultValue: integer): integer;
  var
    QIN1: TDOMNode;
  begin
    QIN1 := QN.Attributes.GetNamedItem(XMLKey);
    Result := StrToIntDef(QIN1.TextContent, QDefaultValue);
  end;
  function ExtractByteOfNode(var QN: TDOMNode; const XMLKey: string; const QDefaultValue: byte): byte;
  var
    QIN1: TDOMNode;
  begin
    QIN1 := QN.Attributes.GetNamedItem(XMLKey);
    Result := StrToIntDef(QIN1.TextContent, QDefaultValue);
  end;
  function ExtractRealOfNode(var QN: TDOMNode; const XMLKey: string; const QDefaultValue: double): double;
  var
    QIN1: TDOMNode;
  begin
    QIN1 := QN.Attributes.GetNamedItem(XMLKey);
    Result := ConvertirEnNombreReel(QIN1.TextContent, QDefaultValue);
  end;
  function ExtractColorOfNode(var QN: TDOMNode): TColor;
  var
    qR, qG, qB: Byte;
  begin
    qR := ExtractByteOfNode(QN, GTX_ATTR_COLOR_R, 255);
    qG := ExtractByteOfNode(QN, GTX_ATTR_COLOR_G, 255);
    qB := ExtractByteOfNode(QN, GTX_ATTR_COLOR_B, 255);
    Result := RGBToColor(qR, qG, qB);
  end;
begin
  Result := -1;
  AfficherMemoryUsage();
  AfficherMessage(Format('%s.LoadFromXML(%s)', [ClassName, FichierXML]));
   // nom étude par défaut
  SetNomEtude('Etude001');
  SetCommentairesEtude('Issued from XML format');
  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;

  SetCodeEPSGSystemeCoordonnees(EPSGDefault);
  // réinit du document
  self.ReInitialiser(True);
  MyDocXML := TXMLDocument.Create;
  try
    ReadXMLFile(MyDocXML, FichierXML);
    GHTopoRoot := MyDocXML.DocumentElement;
    // section General
    AfficherMessage('-> Reading section: ' + GTX_KEY_SECTION_GENERAL);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_GENERAL);
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    //ShowMessageFmt('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
    if (EWE <> Nil) then
    begin
      ListeRubriquesGeneral := EWE.ChildNodes;
      WU := ListeRubriquesGeneral.Item[0];
      SetNomEtude(ExtractStringOfNode(WU, GTX_ATTR_NOM_ETUDE));
      EPSGDefault.CodeEPSG := ExtractIntOfNode(WU, GTX_ATTR_COORDS_SYSTEM_EPSG, DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);
      EPSGDefault.NomEPSG  := ExtractStringOfNode(WU, GTX_ATTR_COORDS_SYSTEM_NOM);
      SetCodeEPSGSystemeCoordonnees(EPSGDefault);
      SetCommentairesEtude(ExtractStringOfNode(WU, GTX_ATTR_COMMENTAIRES_ETUDE));
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // espaces de noms
    AfficherMessage('-> Reading section: ' + GTX_KEY_SECTION_NAMESPACES);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_NAMESPACES);
    if (EWE <> Nil) then
    begin
      ListeNamespaces := EWE.ChildNodes;
      for i := 0 to ListeNamespaces.Count - 1 do
      begin
        try
          WU := ListeNamespaces.Item[i];
          UnNameSpace.Couleur      := ExtractColorOfNode(WU);
          UnNameSpace.Nom          := ExtractStringOfNode(WU, GTX_ATTR_NAMESPACE_NAME);
          UnNameSpace.Description  := ExtractStringOfNode(WU, GTX_ATTR_NAMESPACE_DESCRIPTION);
          self.AddNameSpace(UnNameSpace);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // filtres personnalisés
    AfficherMessage('-> Reading section: ' + GTX_KEY_SECTION_FILTERS);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_FILTERS);
    if (EWE <> Nil) then
    begin
      ListeDesFiltres := EWE.ChildNodes;
      for i := 0 to ListeDesFiltres.Count - 1 do
      begin
        try
          WU := ListeDesFiltres.Item[i];
          UnFiltrePerso.NomFiltre      := ExtractStringOfNode(WU, GTX_ATTR_FILTER_NAME);
          UnFiltrePerso.CouleurFiltre  := ExtractColorOfNode(WU);
          UnFiltrePerso.Expression     := ExtractStringOfNode(WU, GTX_ATTR_FILTER_EXPRESSION);
          UnFiltrePerso.Description    := ExtractStringOfNode(WU, GTX_ATTR_FILTER_DESCRIPTION);
          self.AddFiltrePerso(UnFiltrePerso);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les entrées
    AfficherMessage('-> Reading section: ' + GTX_KEY_SECTION_ENTRANCES);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_ENTRANCES);
    if (EWE <> Nil) then
    begin
      ListeDesEntrees := EWE.ChildNodes;
      for i := 0 to ListeDesEntrees.Count - 1 do
      begin
        try
          WU := ListeDesEntrees.Item[i];
          UneEntree.eIDTerrain  := ExtractStringOfNode(WU, GTX_ATTR_ENTRANCE_IDTERRAIN);
          UneEntree.eNomEntree  := ExtractStringOfNode(WU, GTX_ATTR_ENTRANCE_NAME);
          UneEntree.eRefSer     := ExtractIntOfNode(WU, GTX_ATTR_ENTRANCE_REFSERIE, 1);
          UneEntree.eRefSt      := ExtractIntOfNode(WU, GTX_ATTR_ENTRANCE_REFPT, 0);
          UneEntree.eXEntree    := ExtractRealOfNode(WU, GTX_ATTR_ENTRANCE_X, -1.00);
          UneEntree.eYEntree    := ExtractRealOfNode(WU, GTX_ATTR_ENTRANCE_Y, -1.00);
          UneEntree.eZEntree    := ExtractRealOfNode(WU, GTX_ATTR_ENTRANCE_Z, -1.00);
          UneEntree.eCouleur    := GHTopoColorToColorDef(ExtractStringOfNode(WU, GTX_ATTR_ENTRANCE_COLOR), clBlue);
          UneEntree.eObserv     := ExtractStringOfNode(WU, GTX_ATTR_ENTRANCE_OBS);
          self.AddEntrance(UneEntree);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les réseaux
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_RESEAUX);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_RESEAUX);
    if (EWE <> Nil) then
    begin
      ListeDesReseaux := EWE.ChildNodes;
      for i := 0 to ListeDesReseaux.Count - 1 do
      begin
        try
          WU := ListeDesReseaux.Item[i];
          UnReseau.ColorReseau := ExtractColorOfNode(WU);
          UnReseau.TypeReseau  := ExtractIntOfNode(WU, GTX_ATTR_RESEAU_TYPE, 0);
          UnReseau.NomReseau   := ExtractStringOfNode(WU, GTX_ATTR_RESEAU_NAME);
          UnReseau.ObsReseau   := ExtractStringOfNode(WU, GTX_ATTR_RESEAU_OBS);
          self.AddReseau(UnReseau);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    //ShowMessageFmt('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
    // lister les secteurs
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_SECTEURS);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_SECTEURS);
    if (EWE <> Nil) then
    begin
      ListeDesSecteurs := EWE.ChildNodes;
      for i := 0 to ListeDesSecteurs.Count - 1 do
      begin
        try
          WU := ListeDesSecteurs.Item[i];
          UnSecteur.CouleurSecteur := ExtractColorOfNode(WU);
          UnSecteur.NomSecteur     := ExtractStringOfNode(WU, GTX_ATTR_SECTEUR_NAME);
          self.AddSecteur(UnSecteur);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les codes
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_CODES);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_CODES);
    if (EWE <> Nil) then
    begin
      ListeDesCodes := EWE.ChildNodes;
      for i := 0 to ListeDesCodes.Count - 1 do
      begin
        try
          WU := ListeDesCodes.Item[i];
          UnCode.IDCode                      := ExtractIntOfNode(WU, GTX_ATTR_CODE_NUMERO, 0);
          UnCode.FactLong                    := ExtractRealOfNode(WU, GTX_ATTR_CODE_FACT_LONG, 1.00);
          UnCode.GradAz                      := ExtractRealOfNode(WU, GTX_ATTR_CODE_UCOMPASS, 360.00);
          UnCode.GradInc                     := ExtractRealOfNode(WU, GTX_ATTR_CODE_UCLINO, 360.00);
          UnCode.AngLimite                   := ExtractRealOfNode(WU, GTX_ATTR_CODE_ANGLIMITE, 0.00);
          UnCode.PsiL                        := ExtractRealOfNode(WU, GTX_ATTR_CODE_PSI_L, 0.01);
          UnCode.PsiAz                       := ExtractRealOfNode(WU, GTX_ATTR_CODE_PSI_A, 0.01);
          UnCode.PsiP                        := ExtractRealOfNode(WU, GTX_ATTR_CODE_PSI_P, 0.01);
          UnCode.ErreurTourillon             := ExtractRealOfNode(WU, GTX_ATTR_CODE_ERROR_TOURILLON, 0.00);
          UnCode.DiametreBoule1              := ExtractRealOfNode(WU, GTX_ATTR_CODE_DIAM_BOULE1, 0.00);
          UnCode.DiametreBoule2              := ExtractRealOfNode(WU, GTX_ATTR_CODE_DIAM_BOULE2, 0.00);
          UnCode.ParamsFuncCorrAz.Co         := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_AZ_CO          , 0.00);
          UnCode.ParamsFuncCorrAz.ErreurMax  := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_AZ_ERR_MAX     , 0.00);
          UnCode.ParamsFuncCorrAz.PosErrMax  := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_AZ_POS_ERR_MAX , 0.00);
          UnCode.ParamsFuncCorrInc.Co        := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_INC_CO         , 0.00);
          UnCode.ParamsFuncCorrInc.ErreurMax := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_INC_ERR_MAX    , 0.00);
          UnCode.ParamsFuncCorrInc.PosErrMax := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_INC_POS_ERR_MAX, 0.00);
          UnCode.Commentaire   := ExtractStringOfNode(WU, GTX_ATTR_CODE_OBS);
          self.AddCode(UnCode);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    //ShowMessageFmt('%s: %s', [{$I %FILE%}, {$I %LINE%}]);

    // lister les expés
    AfficherMessage('======================');
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_EXPES);
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_EXPES);
    if (EWE <> Nil) then
    begin
      ListeDesExpes := EWE.ChildNodes;
      for i := 0 to ListeDesExpes.Count - 1 do
      begin
        try
          WU := ListeDesExpes.Item[i];
          UneExpe.IDExpe        := ExtractIntOfNode(WU, GTX_ATTR_EXPE_NUMERO, 0);
          {$WARNING: TEXpe.DateExpe à implementer}
          //UneExpe.DateExpe := ;
          QMyDate               := DateSQLToDatePascal(ExtractStringOfNode(WU, GTX_ATTR_EXPE_DATE));

          UneExpe.AnneeExpe     := YearOf(QMyDate);
          UneExpe.MoisExpe      := MonthOf(QMyDate);
          UneExpe.JourExpe      := DayOf(QMyDate);
          UneExpe.IdxCouleur    := ExtractIntOfNode(WU, GTX_ATTR_EXPE_IDXCOLOR, 0);
          UneExpe.Operateur     := ExtractStringOfNode(WU, GTX_ATTR_EXPE_SURVEY1);
          UneExpe.ClubSpeleo    := ExtractStringOfNode(WU, GTX_ATTR_EXPE_SURVEY2);
          UneExpe.ModeDecl      := TModeCalculDeclimag(ExtractIntOfNode(WU, GTX_ATTR_EXPE_MODEDECL, 0));
          UneExpe.DeclinaisonInDegrees := ExtractRealOfNode(WU, GTX_ATTR_EXPE_DECLINAT, 0.00);
          UneExpe.Commentaire   := ExtractStringOfNode(WU, GTX_ATTR_EXPE_OBS);
          self.AddExpe(UneExpe);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les séries
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_SERIES);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_SERIES);
    if (EWE <> Nil) then
    begin
      ListeDesSeries := EWE.ChildNodes;
      AfficherMessage(Format('%d séries', [ListeDesSeries.Count]));
      for i := 0 to ListeDesSeries.Count - 1 do
      begin
        UneSerie := TObjSerie.Create;
        try
          UneSerie.ClearStations;
          WU  := ListeDesSeries.Item[i];
          qIdxSerie    := TNumeroSerie(ExtractIntOfNode(WU, GTX_ATTR_SERIE_Numero, 0));
          UneSerie.SetNumeroSerie(qIdxSerie);
          UneSerie.SetNomSerie(ExtractStringOfNode(WU, GTX_ATTR_SERIE_NAME));
          UneSerie.SetObsSerie(ExtractStringOfNode(WU, GTX_ATTR_SERIE_OBS));
          //AfficherMessageErreur(Format('%s: %s - %s', [{$I %FILE%}, {$I %LINE%}, UneSerie.GetNomSerie()]));
          qSerDepart  := ExtractIntOfNode(WU, GTX_ATTR_SERIE_SERDEP, 0);

          qPtDepart   := ExtractIntOfNode(WU, GTX_ATTR_SERIE_PTDEP , 0);
          qSerArrivee := ExtractIntOfNode(WU, GTX_ATTR_SERIE_SERARR, 0);
          qPtArrivee  := ExtractIntOfNode(WU, GTX_ATTR_SERIE_PTARR , 0);
          UneSerie.SetSeriePtExtremites(qSerDepart, qPtDepart, qSerArrivee, qPtArrivee);
          UneSerie.SetChance(ExtractIntOfNode(WU, GTX_ATTR_SERIE_CHANCE, 0));
          UneSerie.SetObstacle(ExtractIntOfNode(WU, GTX_ATTR_SERIE_OBSTACLE, 0));
          UneSerie.SetNumeroEntrance(TNumeroEntrance(ExtractIntOfNode(WU, GTX_ATTR_SERIE_ENTRANCE, 0)));
          UneSerie.SetNumeroReseau(TNumeroReseau(ExtractIntOfNode(WU, GTX_ATTR_SERIE_RESEAU, 0)));
          UneSerie.SetRaideur(ExtractRealOfNode(WU, GTX_ATTR_SERIE_RAIDEUR, 1.00));
          UneSerie.SetCouleur(clBlue);

          // les stations
          QAT := WU.FindNode(GTX_KEY_STATIONS);
          if (QAT <> NIL) then
          begin
            ListeStations := QAT.ChildNodes;
            //AfficherMessage(Format('Série %d %s - %d stations', [UneSerie.GetIndexSerie, UneSerie.GetNomSerie, ListeStations.Count]));
            for St := 0 to ListeStations.Count - 1 do
            begin
              try
                WOK := ListeStations.Item[St];
                UneVisee.NoVisee := St;
                UneVisee.IDTerrainStation := ExtractStringOfNode(WOK, GTX_ATTR_VISEE_LBL);
                UneVisee.IDSecteur        := ExtractIntOfNode(WOK, GTX_ATTR_VISEE_SECTEUR, 0);
                UneVisee.TypeVisee        := TTypeDeVisee(ExtractIntOfNode(WOK, GTX_ATTR_VISEE_TYPE, 0));

                UneVisee.Code             := TNumeroCode(ExtractIntOfNode(WOK, GTX_ATTR_VISEE_CODE, 0));
                UneVisee.Expe             := TNumeroExpe(ExtractIntOfNode(WOK, GTX_ATTR_VISEE_EXPE, 0));


                UneVisee.Longueur         := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_LONG, 0.00);
                UneVisee.Azimut           := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_AZ  , 0.00);
                UneVisee.Pente            := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_P   , 0.00);

                UneVisee.LG               := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_LG  , 0.00);
                UneVisee.LD               := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_LD  , 0.00);
                UneVisee.HZ               := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_HZ  , 0.00);
                UneVisee.HN               := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_HN  , 0.00);

                UneVisee.Commentaires     := ExtractStringOfNode(WOK, GTX_ATTR_VISEE_OBS);

                UneSerie.AddVisee(UneVisee);
              except
              end;
            end; // for stations
            self.AddSerie(UneSerie);
          end;
        except // try séries
        end;
      end;  // for séries
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les antennes
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_ANTENNAS);
    if (EWE <> Nil) then
    begin
      ListeDesAntennes := EWE.ChildNodes;
      for i := 0 to ListeDesAntennes.Count - 1 do
      begin
        try
          if (i mod 2000 = 0) then AfficherMessage(Format('Antenne %d of %d', [i, ListeDesAntennes.Count]), false);
          WU := ListeDesAntennes.Item[i];
          //UneViseeAntenne.IDTerrainStation   := ExtractStringOfNode(WU, GTX_KEY_ANTENNA_LABEL);
          UneViseeAntenne.Reseau             := ExtractIntOfNode(WU, GTX_KEY_ANTENNA_NETWORK  , 0);
          UneViseeAntenne.Secteur            := ExtractIntOfNode(WU, GTX_KEY_ANTENNA_SECTEUR  , 0);
          UneViseeAntenne.SerieDepart        := ExtractIntOfNode(WU, GTX_KEY_ANTENNA_SERIE    , 0);
          UneViseeAntenne.PtDepart           := ExtractIntOfNode(WU, GTX_KEY_ANTENNA_POINT    , 0);
          UneViseeAntenne.Longueur           := ExtractRealOfNode(WU, GTX_KEY_ANTENNA_LONG, 0.00);
          UneViseeAntenne.Azimut             := ExtractRealOfNode(WU, GTX_KEY_ANTENNA_AZIMUT, 0.00);
          UneViseeAntenne.Pente              := ExtractRealOfNode(WU, GTX_KEY_ANTENNA_PENTE, 0.00);
          //UneViseeAntenne.Commentaires       := ExtractStringOfNode(WU, GTX_KEY_ANTENNA_OBS);
          UneViseeAntenne.MarkedForDelete    := false;
          self.AddViseeAntenne(UneViseeAntenne);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    //**************************
    self.SetDatabaseName(ExtractFileNameWithoutExt(FichierXML));
    VerifieSerie1();                // Cause courante de plantages: Absence de la série 1
    CheckerLesDonneesTopo();
    RecenserLesIDsTerrain();
    RecenserPointsOfInterest();
    SortCodes();
    SortExpes();
    //SortSeries(); TODO  Trier les séries ?
    UneEntree := GetEntrance(0);
    self.SetRefSeriePoint(UneEntree.eRefSer, UneEntree.eRefSt);
    self.SetDefaultCoords(UneEntree.eXEntree, UneEntree.eYEntree, UneEntree.eZEntree);
    //****************************
    Result := self.GetNbSeries();
  finally
    FreeAndNil(MyDocXML);//MyDocXML.Free;
  end;
end;




// vérifier si la visée en antenne possède une station de rattachement
// utilise l'IDTerrain, la série et le point
// Modifie la visée passée en paramètre
function TToporobotStructure2012.HasStationForAntennaShot(var VA: TViseeAntenne): boolean;
var
  SR: TObjSerie;
  VS: TUneVisee;
  i, j: Integer;
  WU1, WU2: String;
begin
  Result := False;
  for i := 1 to GetNbSeries - 1 do
  begin
    SR := GetSerie(i);
    for j := 0 to SR.GetNbVisees - 1 do
    begin
      VS := SR.GetVisee(j);
      // test sur l'ID de série et station
      if ((VA.SerieDepart = SR.GetNumeroDeSerie()) and (VA.PtDepart = j)) then
      begin
        //VA.IDTerrainStation := VS.IDTerrainStation;
        //VA.Code        := VS.Code;
        //VA.Expe        := VS.Expe;
        VA.Secteur     := VS.IDSecteur;
        VA.Reseau      := SR.GetNumeroReseau();
        //VA.IDTerrainStation := '';
        Result := True;
        Exit;
      end;
    end; // for j := 0 to SR.GetNbVisees - 1 do
  end; //  for i := 1 to GetNbSeries - 1 do
end;
// extraire les antennes de la station NoSerie.NoStation
// et l'envoie dans le tableau ArrAntennesFound
// retourne le nombre d'antennes, ou 0 si aucunne antenne
function TToporobotStructure2012.ExtractViseesAntennesOfStation(const NoSerie: TNumeroSerie;
                                                                const NoStation: Integer;
                                                                out   ArrAntennesFound: TArrayOfTViseeAntenne): integer;
begin
  Result := FTableViseesAntenne.ExtractAntennesFromBasePoint(NoSerie, NoStation, False, ArrAntennesFound);
end;




// pour le moment, on exporte au format Text de Toporobot
// Statut: OK, mais des détails à revoir.
// Entrées multiples OK
// Nota: Support du TLM330 non implémenté volontairement
//*)
procedure TToporobotStructure2012.ExporterVersToporobotTEXT(const QFileName: TStringDirectoryFilename;
                                                            const LongueurMaxAntenne: double;
                                                            const DoExportAntennaShots: boolean);
                                                            deprecated 'Le format TOPOROBOT Text est peu pratique';
var
  fp: TextFile;
  EWE: String;
  myEntree: TEntrance;
  i, Nb, j, NbV: Integer;
  myExpe: TExpe;
  myCode: TCode;
  mySerie: TObjSerie;
  myVisee: TUneVisee;
  NbA: Integer;
  k: Integer;
  myAntenne: TViseeAntenne;
  q: Integer;
  zdar: String;
  function makeColonnesEnTete(const IsCommentaire: boolean;
                              const Section, ID1, ID2, ID3, ID4: integer): string;
  begin
    Result := IIF(IsCommentaire, '(', ' ') +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [Section]), 5, taRightJustify) +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [ID1])    , 6, taRightJustify) +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [ID2])    , 4, taRightJustify) +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [ID3])    , 4, taRightJustify) +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [ID4])    , 4, taRightJustify);
  end;
begin
  AfficherMessage(Format('%s.ExporterVersToporobotTEXT: %s', [ClassName, QFileName]));
  AssignFile(fp, QFileName);
  try
    ReWrite(fp);
    // section -6 = entrée
    Nb := GetNbEntrances();
    for i := 0 to Nb - 1 do
    begin
      myEntree := self.GetEntrance(i);
      EWE := makeColonnesEnTete(false, -6, i + 1, 1, 1, 1) + ' ' + myEntree.eNomEntree;
      WriteLn(fp, EWE);
    end;
    for i := 0 to Nb - 1 do
    begin
      myEntree := self.GetEntrance(i);
      // section -5 = Coordonnées
      //-5     1   1   1   1   629000.72   178000.68     1777.71     1     0
      EWE := makeColonnesEnTete(false, -5, i+1, 1, 1, 1) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myEntree.eXEntree]), 12, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myEntree.eYEntree]), 12, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myEntree.eZEntree]), 12, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER,    [myEntree.eRefSer]), 6, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER,    [myEntree.eRefSt]), 6, taRightJustify);
      WriteLn(fp, EWE);
    end;

    // section -4: Horodatage
    //-4     1   1   1   1 14/12/08 11:47:22  Guest
    for i := 1 to 2 do
    begin
      EWE := makeColonnesEnTete(false, -4, i, 1, 1, 1) +
             FormatterFixedColumn(FormatterDateHeureToporobot(Now), 18, taRightJustify) +
             '  ' + 'Guest';
      WriteLn(fp, EWE);
    end;
    // Section -3: Inutilisée
    EWE := makeColonnesEnTete(false, -3, 1, 1, 1, 1);
    WriteLn(fp, EWE);
    // Section -2: Expés
    // -2     1   1   1   1 25/08/07  MCH           AH            1    0.00   0   1
    Nb := self.GetNbExpes;
    for i := 1 to Nb - 1 do
    begin
      myExpe := GetExpe(i);
      {$WARNING: TEXpe.DateExpe à implementer}
      EWE := makeColonnesEnTete(false, -2, myExpe.IDExpe, 1, 1, 1) +
             FormatterFixedColumn(Format('%.2d/%.2d/%.2d', [myExpe.JourExpe, myExpe.MoisExpe, myExpe.AnneeExpe mod 100]), 9, taRightJustify) +
             '  ' +
             FormatterFixedColumn(myExpe.Operateur, 13, taLeftJustify) + ' ' +
             FormatterFixedColumn(myExpe.ClubSpeleo, 13, taLeftJustify) + ' ' +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [0]), 2, taLeftJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 7, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [0]), 4, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [1]), 4, taRightJustify);
      WriteLn(fp, EWE);
    end;
    // Section -1: Codes
    //-1     1   1   1   1  360.00  360.00    0.05    1.00    1.00  100.00  100.00
    Nb := self.GetNbCodes;
    for i := 1 to Nb - 1 do
    begin
      myCode := GetCode(i);
      EWE := makeColonnesEnTete(false, -1, myCode.IDCode, 1, 1, 1) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myCode.GradAz]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myCode.GradInc]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.05]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [1.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [1.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [100.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [100.00]), 8, taRightJustify);
      WriteLn(fp, EWE);
    end;
    // Sections > 0 = séries
    Nb := self.GetNbSeries;
    for i := 1 to Nb - 1 do
    begin
      mySerie := GetSerie(i);
      //1    -2   1   1   1 GALERIE D'ENTREE D10H10
      EWE := makeColonnesEnTete(false, mySerie.GetNumeroDeSerie(), -2, 1, 1, 1) +
             ' ' + mySerie.GetNomSerie;
      WriteLn(fp, EWE);
      //1    -1   1   1   1       1       0       1       6       6       3       0
      EWE := makeColonnesEnTete(false, mySerie.GetNumeroDeSerie(), -1, 1, 1, 1) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNoSerieDep]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNoPointDep]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNoSerieArr]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNoPointArr]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNbVisees - 1]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetChance]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetObstacle]), 8, taRightJustify);
      WriteLn(fp, EWE);
      nbV := mySerie.GetNbVisees;
      EWE := makeColonnesEnTete(false, mySerie.GetNumeroDeSerie(), 0, 1, 1, 1) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify);
      WriteLn(fp, EWE);
      for j := 1 to nbV -1 do   // et non 'for j = 0 to nbV - 1'
      begin
        myVisee := mySerie.GetVisee(j);
        EWE := makeColonnesEnTete(false, mySerie.GetNumeroDeSerie(), j, 1, myVisee.Code, myVisee.Expe) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.Longueur]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.Azimut]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.Pente]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.LG]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.LD]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.HZ]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.HN]), 8, taRightJustify);
        WriteLn(fp, EWE);

        // export des commentaires
        q := 0;
        zdar := Trim(myVisee.Commentaires);
        if (zdar <> '') then
        begin
          EWE := makeColonnesEnTete(true, mySerie.GetNumeroDeSerie(), j, 0, myVisee.Code, myVisee.Expe) + ' ' + zdar;
          WriteLn(fp, EWE);
          q+=1;
        end;
        // exporter ici les visées en antenne
        if (DoExportAntennaShots) then
        begin
          NbA := GetNbAntennes;
          if (NbA > 0) then
          begin
            for k := 0 to NbA - 1 do
            begin
              myAntenne := GetViseeAntenne(k);
              if ((myAntenne.SerieDepart = mySerie.GetNumeroDeSerie()) and
                  (myAntenne.PtDepart    = j) and
                  (myAntenne.Longueur < LongueurMaxAntenne)) then
              begin
                // PocketTopo stocke les antennes dans les commentaires du point d'accrochage
                EWE := makeColonnesEnTete(true, myAntenne.SerieDepart, myAntenne.PtDepart, q, 0, 0) + //myAntenne.Code, myAntenne.Expe) +
                       Format(' [%.2f %.1f %.1f]', [myAntenne.Longueur, myAntenne.Azimut, myAntenne.Pente]);
                WriteLn(fp, EWE);
                q += 1;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    CloseFile(fp);
  end;
end;

procedure TToporobotStructure2012.ExportListeAntennesCSV(const FileName: TStringDirectoryFilename);
var
  fp: TextFile;
  QNbAntennes, i: Integer;
  VA: TViseeAntenne;
  EWE: String;
begin
  AfficherMessage(Format('%s.ExportListeAntennesCSV: %s', [ClassName, FileName]));
  QNbAntennes := self.GetNbAntennes();
  if (0 = QNbAntennes) then exit;
  AssignFile(fp, FileName);
  try
    Rewrite(fp);
    WriteLn(fp, 'No' + #9 + 'Serie' + #9 + 'Point' + #9 + 'Longueur' + #9 + 'Azimut' + #9 + 'Pente' + #9 + 'Obs');
    for i := 1 to QNbAntennes - 1 do
    begin
      VA := GetViseeAntenne(i);
      if (VA.Longueur > 0.001) then
      begin
        EWE := Format('%d', [i]) + #9 +
               Format('%d', [VA.SerieDepart]) + #9 +
               Format('%d', [VA.PtDepart]) + #9 +
               FormatterNombreOOo(VA.Longueur, 3) + #9 +
               FormatterNombreOOo(VA.Azimut  , 3) + #9 +
               FormatterNombreOOo(VA.Pente   , 3);
        Writeln(fp, EWE);
      end;
    end;
  finally
    CloseFile(fp);
  end;
end;



// export brut des séries (sans entête) pour construction de tableaux croisés dynamiques sur tableur
procedure TToporobotStructure2012.ExportSeriesCSV(const QFileName: TStringDirectoryFilename);
const
  FMT666 = FORMAT_NB_INTEGER + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_NB_INTEGER + #9 +
           FORMAT_NB_INTEGER + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING + #9 +
           FORMAT_STRING;

var
  fp: TextFile;
  EWE, QNomSerie: String;
  NbSeries, i, nv, v: Integer;
  MySerie: TObjSerie;
  MyReseau: TReseau;
  MyEntrance: TEntrance;
  MyVisee: TUneVisee;
  MySecteur: TSecteur;
begin

  AfficherMessage(Format('%s.ExportSeriesCSV: %s', [ClassName, QFileName]));
  AssignFile(fp, QFileName);
  try
    ReWrite(fp);
    NbSeries := GetNbSeries();
    // entête
    EWE := 'Serie' + #9 +
           'Entree' + #9 +
           'Reseau' + #9 +
           'Secteur'  + #9 +
           'Type'  + #9 +
           'Code' + #9 +
           'Expe' + #9 +
           'Longueur' + #9 +
           'Azimut' + #9 +
           'Pente' + #9 +
           'LG' + #9 +
           'LD' + #9 +
           'HZ' + #9 +
           'HN' + #9 +
           'IDTerrain' + #9 +
           'Commentaires';
    WriteLn(fp, EWE);
    for i := 0 to NbSeries - 1 do
    begin
      MySerie    := GetSerie(i);
      QNomSerie  := MySerie.GetNomSerie();
      MyReseau   := GetReseau(MySerie.GetNumeroReseau());
      MyEntrance := GetEntrance(MySerie.GetNumeroEntrance());
      nv := MySerie.GetNbVisees();
      for v := 0 to nv - 1 do
      begin
        MyVisee := MySerie.GetVisee(v);
        MySecteur := GetSecteur(MyVisee.IDSecteur);
        EWE := Format(FMT666, [
                  MySerie.GetNumeroDeSerie(),         //       EWE := 'Serie' + #9 +
                  MyEntrance.eNomEntree,              //'Entree' + #9 +
                  MyReseau.NomReseau,                 //'Reseau' + #9 +
                  MySecteur.NomSecteur,               //'Secteur'  + #9 +
                  GetDescTypeVisee(MyVisee.TypeVisee), // 'Type'  + #9 +
                  MyVisee.Code, MyVisee.Expe,
                  FormatterNombreOOo(MyVisee.Longueur, 3, false),
                  FormatterNombreOOo(MyVisee.Azimut, 3, false),
                  FormatterNombreOOo(MyVisee.Pente, 3, false),
                  FormatterNombreOOo(MyVisee.LG, 3, false),
                  FormatterNombreOOo(MyVisee.LD, 3, false),
                  FormatterNombreOOo(MyVisee.HZ, 3, false),
                  FormatterNombreOOo(MyVisee.HN, 3, false),
                  MyVisee.IDTerrainStation,
                  MyVisee.Commentaires
                  ]);
        // remplacer les doubles quotes par une paire de simples quotes
        // (doubles quotes sont interprétées par Calc)
        EWE := StringReplace(EWE, '"', '''''', [rfReplaceAll]);
        WriteLn(fp, EWE);
      end;
    end;
  finally
    CloseFile(fp);
  end;
end;



// vérifie si le document GHTopo comporte des codes instruments étendus
function TToporobotStructure2012.IsGHTopoCodesInstrumentsExtended(): boolean;
var
  i, Nb   : Integer;
  myCode  : TCode;
  EWE     : Boolean;
  miou    : Int64;
begin
  AfficherMessage(Format('%s.IsGHTopoCodesInstrumentsExtended', [ClassName]));
  Result := false;
  Nb := GetNbCodes;
  for i := 1 to Nb - 1 do
  begin
    myCode := GetCode(i);
    miou   := Trunc(myCode.GradInc);
    EWE := (miou = 350) or
           (miou = 359) or
           (miou = 360) or
           (miou = 361) or
           (miou = 380) or
           (miou = 390) or
           (miou = 399) or
           (miou = 400) or
           (miou = 401);
    EWE := Not EWE;
    AfficherMessage(Format('%d: %d - %.2f - %s', [i, myCode.IDCode, myCode.GradInc, BoolToStr(EWE, 'VRAI', 'FAUX')]));

    Result := Result OR EWE;
  end;
  AfficherMessage(Format('Le document %s des codes instruments GHTopo', [BoolToStr(Result, 'CONTIENT', 'NE CONTIENT PAS')]));
end;
// supprimer les visées en antenne associées à une station,
function TToporobotStructure2012.RemoveViseesRayonnantesAssocieesAStation(const qSer, qSt: integer): integer;
var
  EWE: TViseeAntenne;
  Nb: Integer;
  B3: Boolean;
begin
  Result := 0;
  AfficherMessage(Format('%s.RemoveViseeRayonnanteAssocieeAStation: %d.%d', [self.ClassName, qSer, qSt]));
  Nb := 0;
  while Nb < FTableViseesAntenne.Count - 1 do
  begin
    EWE := GetViseeAntenne(Nb);
    B3 := False;
    if (qSer > 0) then B3 := (EWE.SerieDepart = qSer) and (EWE.PtDepart = qSt);
    if (B3) then
    begin
      RemoveViseeAntenne(Nb);
      AfficherMessageErreur(Format('Antenne retiree: %d.%d', [EWE.SerieDepart, EWE.PtDepart]));
      Result += 1;
      Nb -= 1;
    end;
    Nb += 1;
  end;
  AfficherMessage(Format('%d antennes supprimées', [Result]));
end;
// renommer les points d'origine des visées rayonnantes associées à une station
function TToporobotStructure2012.MoveAntennesToAutreStation(const OldStation, NewStation: TToporobotIDStation;
                                                            const DoDelete: boolean): integer;
var
  MyAntenne: TViseeAntenne;
  Nb, i: Integer;
begin
  result := 0;
  Nb := GetNbAntennes();
  if (nb = 0) then exit;
  // mêmes stations ? --> []
  if ((OldStation.aSerie    = NewStation.aSerie) AND
      (OldStation.aStation  = NewStation.aStation)) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MyAntenne := GetViseeAntenne(i);
    if ((MyAntenne.SerieDepart = OldStation.aSerie) and
        (MyAntenne.PtDepart    = OldStation.aStation)) then
    begin
      MyAntenne.SerieDepart := NewStation.aSerie;
      MyAntenne.PtDepart    := NewStation.aStation;
      PutViseeAntenne(i, MyAntenne);
      Result += 1;
    end;
  end;
end;

function TToporobotStructure2012.MoveAntennesToAutreStation(const OldSerie: TNumeroSerie; const OldPoint: integer;
                                                            const NewSerie: TNumeroSerie; const NewPoint: integer;
                                                            const DoDelete: boolean): integer;
var
  OldStation, NewStation: TToporobotIDStation;
begin
  OldStation.aSerie   := OldSerie;     OldStation.aStation := OldPoint;
  NewStation.aSerie   := NewSerie;     NewStation.aStation := NewPoint;
  Result := MoveAntennesToAutreStation(OldStation, NewStation, DoDelete);
end;




procedure TToporobotStructure2012.RecenserLesIDsTerrain();
var
  MyLabelTerrain: TToporobotIDStation;
  i, j, NbV       : Integer;
  MySerie         : TObjSerie;
  EWE             : TUneVisee;
  WU              : String;
  NbSts           : Integer;
  LL              : double;
begin
  AfficherMessage(Format('%s.RecenserLesIDsTerrain', [ClassName]));
  FTableIDsTerrain.ClearListe();
  // Cause fréquente d'erreurs: absence de la série 1
  NbSts := 0;
  //AfficherMessage(Format('%d séries', [GetNbSeries()]));
  for i := 0 to GetNbSeries - 1 do
  begin
    MySerie := GetSerie(i);
    NbV := MySerie.GetNbVisees;
    // calcul des longueurs de séries
    LL := 0.00;
    for j := 0 to NbV - 1 do
    begin
      EWE := MySerie.GetVisee(j);
      LL += EWE.Longueur;
    end;
    //AfficherMessage(Format('%d: %d: %s - %d visées (%.2f m)', [i, MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie(), NbV, LL]));
    NbSts += NbV;
    if (NbV > 0) then
    begin
      for j := 0 to NbV - 1 do
      begin
        EWE := MySerie.GetVisee(j);
        WU  := Trim(UpperCase(EWE.IDTerrainStation));
        MyLabelTerrain := MakeTToporobotIDStation(0, MySerie.GetNumeroDeSerie(), j , WU);
        if (WU <> '') then AddLabelTerrain(MyLabelTerrain);
      end;
    end;
  end;
end;

function TToporobotStructure2012.FindIdxIDTerrainByText(const Str: string): integer;
var
  i, p, Nb: Integer;
  S: String;
  LT: TToporobotIDStation;
begin
  Result := -1;
  S := UpperCase(Trim(Str));
  Nb := FTableIDsTerrain.Count;
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    LT := GetLabelTerrain(i);
    p := Pos(S, UpperCase(LT.aIDTerrain));
    if (p > 0) then Exit(i);
  end;
end;
//------------------------------------------------------------------------------
function TToporobotStructure2012.ReAttribuerViseesRayonnantesDeUneSerie(const SerieCourante: TObjSerie; const OldNumSerie: integer): integer;
var
  i, Nb: Integer;
begin
  Result := 0;
  Nb := SerieCourante.GetNbVisees;
  if (Nb = 0) then Exit;
  for i := 0 to Nb-1 do Result += MoveAntennesToAutreStation(OldNumSerie, i, SerieCourante.GetNumeroDeSerie(), i, false);
end;

// réattribuer les codes série.station d'une série modifiée
function TToporobotStructure2012.UpdateSerPtExtremsSeriesRattachees(const OldNumSerie, NewNumSerie: integer): integer;
var
  i: Integer;
  MySerie: TObjSerie;
  SerieAModifier: Boolean;
  MyEntree: TEntrance;
begin
  Result := 0;
  AfficherMessage(Format('%s.UpdateSerPtExtremsSeriesRattachees(%d -> %d)', [ClassName, OldNumSerie, NewNumSerie]));
  // entrées
  for i := 0 to GetNbEntrances() - 1 do
  begin
    MyEntree := GetEntrance(i);
    if (OldNumSerie = 1) then Continue;
    if (OldNumSerie = MyEntree.eRefSer) then
    begin
      AfficherMessage(Format('Entree: %d - %s: Noeud %d.%d renomme en %.d.%d',
                             [i, //MyEntree.eNumEntree,
                              MyEntree.eNomEntree,
                              MyEntree.eRefSer, MyEntree.eRefSt,
                              NewNumSerie, MyEntree.eRefSt
                             ]));
      MyEntree.eRefSer := NewNumSerie;
      PutEntrance(i, MyEntree);
    end;
  end;
  // séries
  for i := 1 to GetNbSeries - 1 do
  begin
    SerieAModifier := false;
    MySerie := GetSerie(i);
    if (OldNumSerie = MySerie.GetNoSerieDep) then
    begin
      AfficherMessage(Format('Serie: %d - %s: Depart %d.%d renomme en %.d.%d',
                             [MySerie.GetNumeroDeSerie(),
                              MySerie.GetNomSerie,
                              MySerie.GetNoSerieDep, MySerie.GetNoPointDep,
                              NewNumSerie, MySerie.GetNoPointDep
                             ]));
      MySerie.SetNoSerieDep(NewNumSerie);
      SerieAModifier := True;
    end;
    if (OldNumSerie = MySerie.GetNoSerieArr) then
    begin
      AfficherMessage(Format('Serie: %d - %s: Arrivee %d.%d renomme en %.d.%d',
                             [MySerie.GetNumeroDeSerie(),
                              MySerie.GetNomSerie,
                              MySerie.GetNoSerieArr, MySerie.GetNoPointArr,
                              NewNumSerie, MySerie.GetNoPointArr
                             ]));
      MySerie.SetNoSerieArr(NewNumSerie);
      SerieAModifier := True;
    end;
    if (SerieAModifier) then
    begin
      Result += 1;
    end;
  end;
end;

// filtres perso
procedure TToporobotStructure2012.AddFiltrePerso(const FiltrePerso: TFiltrePersonnalise);
begin
  FTableFiltresPersonnalises.AddElement(FiltrePerso);
end;

function TToporobotStructure2012.GetFiltrePerso(const NumFiltrePerso: integer): TFiltrePersonnalise;
begin
  Result := FTableFiltresPersonnalises.GetElement(NumFiltrePerso);
end;

procedure TToporobotStructure2012.PutFiltrePerso(const NoFiltrePerso: integer; const AFiltrePerso: TFiltrePersonnalise);
begin
  FTableFiltresPersonnalises.PutElement(NoFiltrePerso, AFiltrePerso);
end;

function TToporobotStructure2012.RemoveFiltrePerso(const Idx: integer): boolean;
begin
  result :=FTableFiltresPersonnalises.RemoveElement(Idx);
end;

function TToporobotStructure2012.GetNbFiltresPersos(): integer;
begin
  Result := FTableFiltresPersonnalises.GetNbElements();
end;

function TToporobotStructure2012.GetLastFiltrePerso(): TFiltrePersonnalise;
var
  Nb: Integer;
begin
  Nb := self.GetNbFiltresPersos() - 1;
  Result := GetFiltrePerso(Nb);
end;

procedure TToporobotStructure2012.ViderListeFiltres();
begin
  FTablePointsOfInterest.ClearListe();
end;
// Points d'intérêt
procedure TToporobotStructure2012.AddPointOfInterest(const APOI: TPointOfInterest);
begin
  FTablePointsOfInterest.AddElement(APOI);
end;

function TToporobotStructure2012.GetPointOfInterest(const Idx: integer): TPointOfInterest;
begin
  Result := FTablePointsOfInterest.GetElement(Idx);
end;

procedure TToporobotStructure2012.PutPointOfInterest(const Idx: integer; const APOI: TPointOfInterest);
begin
  FTablePointsOfInterest.PutElement(Idx, APOI);
end;

function TToporobotStructure2012.RemovePointOfInterest(const Idx: integer): boolean;
var
  MyPOI: TPointOfInterest;
  MySerie: TObjSerie;
  QIdxSerie: integer;
  MyStation: TUneVisee;
begin
  result := false;
  MyPOI := GetPointOfInterest(Idx);
  AfficherMessageErreur(format('Fuck the Christ: %s.RemovePointOfInterest(%d): ', [ClassName, Idx]));
  if (GetSerieByNumeroSerie(MyPOI.Serie, MySerie, QIdxSerie)) then
  begin
    AfficherMessageErreur('--001');
    MyStation := MySerie.GetVisee(MyPOI.Station);
    AfficherMessageErreur(format('--002: %f, %f, %f - %s', [MyStation.Longueur, MyStation.Azimut, MyStation.Pente, MyStation.Commentaires]));
    // Basculer le POI à DONE
    MyStation.Commentaires := StringReplace(MyStation.Commentaires, KEYWORD_POI_TODO, KEYWORD_POI_DONE, [rfIgnoreCase]);
    AfficherMessageErreur(format('--003: %f, %f, %f - %s', [MyStation.Longueur, MyStation.Azimut, MyStation.Pente, MyStation.Commentaires]));
    MySerie.PutVisee(MyPOI.Station, MyStation);
    // et supprimer le POI de la liste
    AfficherMessageErreur('Le Christ doit être pendu');
    FTablePointsOfInterest.RemoveElement(Idx);
    result := True;
  end;
end;

procedure TToporobotStructure2012.RecenserPointsOfInterest();
var
  POI: TPointOfInterest;
  NbSts, i, j, NbV: Integer;
  MySerie: TObjSerie;
  MyVisee: TUneVisee;
  MyPOI  : TPointOfInterest;
  WU: String;
  P1, P2: SizeInt;
  EWE1, EWE2: Boolean;
begin
  AfficherMessage(Format('%s.RecenserPointsOfInterest(%d series)', [ClassName, GetNbSeries()]));
  FTablePointsOfInterest.ClearListe();
  NbSts := 0;
  for i := 0 to GetNbSeries() - 1 do
  begin
    MySerie := GetSerie(i);
    NbV := MySerie.GetNbVisees();
    if (NbV > 0) then
    begin
      for j := 0 to NbV - 1 do
      begin
        MyVisee := MySerie.GetVisee(j);
        WU := Trim(LowerCase(MyVisee.IDTerrainStation + MyVisee.Commentaires));
        P1   := Pos(KEYWORD_POI_TODO, WU);
        P2   := Pos(KEYWORD_POI_DONE, WU);
        EWE1 := (P1 > 0);
        EWE2 := (P2 > 0);
        MyPOI.Statut := poiUNKNOWN;
        if (EWE1 OR EWE2) then
        begin
          MyPOI.Couleur      := clGray;
          if (EWE1) then
          begin
            MyPOI.Statut       := poiTODO;
            MyPOI.Couleur      := clAqua;
            MyPOI.Description  := Trim(StringReplace(MyVisee.Commentaires, KEYWORD_POI_TODO, '', [rfReplaceAll, rfIgnoreCase]));
          end else if (EWE2) then
          begin
            MyPOI.Statut       := poiDONE;
            MyPOI.Couleur      := clGreen;
            MyPOI.Description  := Trim(StringReplace(MyVisee.Commentaires, KEYWORD_POI_DONE, '', [rfReplaceAll, rfIgnoreCase]));
          end;
          MyPOI.LabelTerrain := MyVisee.IDTerrainStation;
          MyPOI.Serie        := MySerie.GetNumeroDeSerie();
          MyPOI.Station      := j;

          MyPOI.Coordinates  := MakeTPoint3Df(0.00, 0.00, 0.00);
          self.AddPointOfInterest(MyPOI);
        end;
      end;
    end;
  end;
end;

function TToporobotStructure2012.GetNbPointsOfInterests(): integer;
begin
  Result := FTablePointsOfInterest.GetNbElements();
end;


//******************************************************************************
// Attrapper le premier IDx libre
function TToporobotStructure2012.getMaxIdxExpe(): TNumeroExpe;
var
  nb, i: Integer;
  EWE: TExpe;
begin
  result := -1;
  nb     := GetNbExpes();
  for i := 1 to nb - 1 do
  begin
    EWE := GetExpe(i);
    Result := Max(Result, EWE.IDExpe);
  end;
end;
function TToporobotStructure2012.getMaxIdxCode(): TNumeroCode;
var
  nb, i: Integer;
  EWE  : TCode;
begin
  result := -1;
  nb     := getNbCodes();
  for i := 1 to nb - 1 do
  begin
    EWE := getCode(i);
    Result := Max(Result, EWE.IDCode);
  end;
end;
// retourne le prochain index de série libre
function TToporobotStructure2012.getMaxIdxSerie(): TNumeroSerie;
var
  nb, i: Integer;
  EWE: TObjSerie;
  WU: TEntrance;
begin
  Result := -1;
  nb     := self.GetNbSeries();
  for i  := 0 to nb -1 do
  begin
    EWE := GetSerie(i);
    Result := Max(Result, EWE.GetNumeroDeSerie());
  end;
end;


function TToporobotStructure2012.getMaxIdxReseau(): integer;
begin
  result := GetNbReseaux();
end;

function TToporobotStructure2012.getMaxIdxSecteur(): integer;
begin
  result := GetNbSecteurs();
end;
// import de données depuis des fichiers CSV
procedure TToporobotStructure2012.AddEntrancesFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
begin
  AfficherMessage(Format('%s.AddEntrancesFromFile: %s', [ClassName, FileName]));
end;
procedure TToporobotStructure2012.AddCodesFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
begin
  AfficherMessage(Format('%s.AddCodesFromFile: %s', [ClassName, FileName]));
end;
procedure TToporobotStructure2012.AddExpesFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
begin
  AfficherMessage(Format('%s.AddExpesFromFile: %s', [ClassName, FileName]));
end;
procedure TToporobotStructure2012.AddReseauxFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
begin
  AfficherMessage(Format('%s.AddReseauxFromFile: %s', [ClassName, FileName]));
end;
procedure TToporobotStructure2012.AddSecteursFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
begin
  AfficherMessage(Format('%s.AddSecteursFromFile: %s', [ClassName, FileName]));
end;
procedure TToporobotStructure2012.AddAntennesFromFile(const FileName: TStringDirectoryFilename; const DoReplaceExisting: boolean);
begin
  AfficherMessage(Format('%s.AddAntennesFromFile: %s', [ClassName, FileName]));
end;

// remplacement d'index
procedure TToporobotStructure2012.ReplaceIndexSeriesInEntrances(const OldIndex, NewIndex: TNumeroSerie);
var
  Nb, i: Integer;
  EWE: TEntrance;
begin
  AfficherMessage(Format('%s.ReplaceIndexSeriesInEntrances: %d -> %d', [ClassName, OldIndex, NewIndex]));
  Nb := GetNbEntrances();
  for i := 0 to Nb - 1 do
  begin
    EWE := GetEntrance(i);
    if (EWE.eRefSer = OldIndex) then
    begin
      EWE.eRefSer := NewIndex;
      PutEntrance(i, EWE);
    end;
  end;
end;

procedure TToporobotStructure2012.ReplaceIndexInStations(const OldIndex, NewIndex: integer; const ModeRemplacement: TModeRemplacementStations);
var
  EWE: String;
  NbSeries, i, NbVisees, j: Integer;
  MySerie: TObjSerie;
  MyVisee: TUneVisee;
begin
  EWE := ChooseString(Ord(ModeRemplacement), ['mrpSECTEURS', 'mrpCODES', 'mrpEXPES']);
  AfficherMessage(Format('%s.ReplaceIndexInStations: %d -> %d - %s', [ClassName, OldIndex, NewIndex, EWE]));
  NbSeries := GetNbSeries();
  for i := 0 to NbSeries - 1 do
  begin
    MySerie := GetSerie(i);
    NbVisees := MySerie.GetNbVisees();
    for j := 0 to NbVisees - 1 do
    begin
      MyVisee := MySerie.GetVisee(j);
      case ModeRemplacement of
        mrsSECTEURS: if (MyVisee.IDSecteur = OldIndex) then MyVisee.IDSecteur := NewIndex;
        mrsCODES   : if (MyVisee.Code = OldIndex)      then MyVisee.Code      := NewIndex;
        mrsEXPES   : if (MyVisee.Expe = OldIndex)      then MyVisee.Expe      := NewIndex;
      end;
      MySerie.PutVisee(j, MyVisee);
    end;
  end;
end;
procedure TToporobotStructure2012.ReplaceIndexInAntennes(const OldIndex, NewIndex: integer; const ModeRemplacement: TModeRemplacementAntennes);
var
  EWE: String;
  Nb, i: Integer;
  MyAntenne: TViseeAntenne;
begin
  AfficherMessage(Format('%s.ReplaceIndexInAntennes: %d -> %d', [Classname, OldIndex, NewIndex]));
  Nb := GetNbAntennes();
  if (Nb = 0) then Exit;
  for i := 0 to Nb -1 do
  begin
    MyAntenne := GetViseeAntenne(i);
    if (mraRESEAUX       in ModeRemplacement) then
      if (MyAntenne.Reseau = OldIndex) then  MyAntenne.Reseau   := NewIndex;
    if (mraSECTEURS      in ModeRemplacement) then
      if (MyAntenne.Secteur = OldIndex) then MyAntenne.Secteur  := NewIndex;
    if (mraSERIES        in ModeRemplacement) then
      if (MyAntenne.SerieDepart = OldIndex) then MyAntenne.SerieDepart   := NewIndex;
    PutViseeAntenne(i, MyAntenne);
  end;
end;
procedure TToporobotStructure2012.ReplaceIdxCodes(const OldIndex, NewIndex: TNumeroCode);
var
  Nb, i: Integer;
  EWE: TCode;
begin
  AfficherMessage(Format('%s.ReplaceIdxCodes: %d -> %d', [Classname, OldIndex, NewIndex]));
  Nb := GetNbCodes();
  for i := 0 to Nb - 1 do
  begin
    EWE := GetCode(i);
    if (EWE.IDCode = OldIndex) then
    begin
      EWE.IDCode := NewIndex;
      PutCode(i, EWE);
    end;
  end;
end;

procedure TToporobotStructure2012.ReplaceIdxExpes(const OldIndex, NewIndex: TNumeroExpe);
var
  EWE: TExpe;
  Nb, i: Integer;
begin
  AfficherMessage(Format('%s.ReplaceIdxExpes: %d -> %d', [ClassName, OldIndex, NewIndex]));
  Nb := GetNbCodes();
  for i := 0 to Nb - 1 do
  begin
    EWE := GetExpe(i);
    if (EWE.IDExpe = OldIndex) then
    begin
      EWE.IDExpe := NewIndex;
      PutExpe(i, EWE);
    end;
  end;
end;



procedure TToporobotStructure2012.ReplaceIdxSeries(const OldIndex, NewIndex: TNumeroSerie);
var
  Nb, i: Integer;
  MySerie: TObjSerie;
begin
  AfficherMessage(Format('%s.ReplaceIdxSeries: %d -> %d', [ClassName, OldIndex, NewIndex]));
  Nb := GetNbSeries();
  for i := 0 to Nb - 1 do
  begin
    MySerie := GetSerie(i);
    if (OldIndex = MySerie.GetNumeroDeSerie()) then MySerie.SetNumeroSerie(NewIndex);
    if (OldIndex = MySerie.GetNoSerieDep())    then MySerie.SetNoSerieDep(NewIndex);
    if (OldIndex = MySerie.GetNoSerieArr())    then MySerie.SetNoSerieArr(NewIndex);
  end;
end;



// liste des messages d'erreur issues du checkup de la base
function TToporobotStructure2012.GetNbMessagesErreur(): integer;
begin
  if (not assigned(FListeMessagesErreur)) then exit(0);
  Result := FListeMessagesErreur.GetNbElements();
end;

function TToporobotStructure2012.GetMessageErreur(const Idx: integer): TMessaqeErreurGHTopoCompiler;
begin
  if (not Assigned(FListeMessagesErreur)) then Exit;
  if (0 = FListeMessagesErreur.GetNbElements()) then
  begin
    Result.Couleur    := clGreen;
    Result.Criticite  := cmeNOTE;
    Result.Message    := 'Inconnu';
  end
  else Result := FListeMessagesErreur.GetElement(Idx);
end;

procedure TToporobotStructure2012.AddMessageErreur(const M: TMessaqeErreurGHTopoCompiler);
begin
  FListeMessagesErreur.AddElement(M);
end;

procedure TToporobotStructure2012.AddMessageErreur(const T: TTableExaminee; const QCriticite: TCriticiteMessaqeErreurGHTopo; const QIndex: Int64; const QMsg: string);
var
  M: TMessaqeErreurGHTopoCompiler;
begin
  M.TableExaminee := T;
  M.Criticite     := QCriticite;
  M.Index         := QIndex;
  M.Couleur       := ChooseColorInColorArray(Ord(M.Criticite), [clGreen, clYellow, clFuchsia, clRed, clAqua]);
  M.Message       := QMsg;
  FListeMessagesErreur.AddElement(M);
end;

// Lecture/écriture au format XML
// (futur format standard de GHTopo)
procedure TToporobotStructure2012.SaveToXML(const FichierXML: TStringDirectoryFilename);
const
  IDX_MINI = 1;
var
  i: Integer;
  MyDocXML          : TXMLDocument;
  GHTopoRoot        : TDOMElement;
  SectionGeneral    : TDOMElement;
  SectionNamespaces : TDOMElement;
  SectionFiltres    : TDOMElement;
  SectionEntrances, SectionReseaux, SectionSecteurs   : TDOMElement;
  SectionCodes, SectionSeances                        : TDOMElement;
  SubSection, SubSubSection, SubSubSubSection         : TDOMElement;
  SectionSeries         : TDOMElement;
  SectionAntennes       : TDOMElement;
  E  : TEntrance;
  R  : TReseau;
  SC : TSecteur;
  C  : TCode;
  EX : TExpe;
  SER: TObjSerie;
  ST : TUneVisee;
  ANT: TViseeAntenne;
  EPSG: TLabelSystemesCoordsEPSG;
  //NV: TReleaseNotes;
  FF: TFiltrePersonnalise;
  S, QNbAntennes, QNbFiltres, QNbNameSpaces: Integer;
  SS: TNameSpace;
  procedure KWYXZ(const S666: string);
  begin
    AfficherMessage(Format('-- Writing "%s" section', [S666]));
  end;
begin
  AfficherMessage(Format('%s.SaveToXML(%s)', [ClassName, FichierXML]));
  MyDocXML := TXMLDocument.Create;
  try
    // racine du document = nom du logiciel
    GHTopoRoot := MyDocXML.CreateElement('GHTopo');
    MyDocXML.AppendChild(GHTopoRoot);
    // section Général
    SectionGeneral := MyDocXML.CreateElement(GTX_KEY_SECTION_GENERAL);
      GHTopoRoot.AppendChild(SectionGeneral);
      SubSection := MyDocXML.CreateElement(GTX_KEY_CAVITE);
      //SubSection.SetAttribute(GTX_ATTR_NAMESPACE, PasStrToXMLStr(GetNameSpace()));
      SubSection.SetAttribute(GTX_ATTR_NOM_ETUDE, PasStrToXMLStr(GetNomEtude()));
      EPSG := GetCodeEPSGSystemeCoordonnees;
      SubSection.SetAttribute(GTX_ATTR_COORDS_SYSTEM_EPSG, Format(FORMAT_NB_INTEGER, [EPSG.CodeEPSG]));
      SubSection.SetAttribute(GTX_ATTR_COORDS_SYSTEM_NOM, PasStrToXMLStr(EPSG.NomEPSG));
      SubSection.SetAttribute(GTX_ATTR_COMMENTAIRES_ETUDE, PasStrToXMLStr(GetCommentairesEtude));
      SectionGeneral.AppendChild(SubSection);
    // section Espaces de noms
    QNbNameSpaces := self.GetNbNameSpaces();
    if (QNbNameSpaces > 0) then
    begin
      SectionNamespaces := MyDocXML.CreateElement(GTX_KEY_SECTION_NAMESPACES);
      GHTopoRoot.AppendChild(SectionNamespaces);
      KWYXZ(GTX_KEY_SECTION_NAMESPACES);
      for i := 0 to QNbNameSpaces - 1 do
      begin
        SS := GetNameSpace(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_NAMESPACE);
          SubSection.SetAttribute(GTX_ATTR_NAMESPACE             , Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_NAMESPACE_NAME        , PasStrToXMLStr(SS.Nom));
          SubSection.SetAttribute(GTX_ATTR_COLOR_R     , Format(FORMAT_NB_INTEGER, [Red(SS.Couleur)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_G     , Format(FORMAT_NB_INTEGER, [Green(SS.Couleur)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_B     , Format(FORMAT_NB_INTEGER, [Blue(SS.Couleur)]));
          SubSection.SetAttribute(GTX_ATTR_NAMESPACE_DESCRIPTION , PasStrToXMLStr(SS.Description));
        SectionNamespaces.AppendChild(SubSection);
      end;
    end;

    // section Filtres perso
    QNbFiltres := self.GetNbFiltresPersos();
    if (QNbFiltres > 0) then
    begin
      SectionFiltres := MyDocXML.CreateElement(GTX_KEY_SECTION_FILTERS);
      GHTopoRoot.AppendChild(SectionFiltres);
      KWYXZ(GTX_KEY_SECTION_FILTERS);
      for i := 0 to QNbFiltres - 1 do
      begin
        FF := GetFiltrePerso(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_FILTER);
          SubSection.SetAttribute(GTX_ATTR_FILTER_IDX        , Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_FILTER_NAME       , PasStrToXMLStr(FF.NomFiltre));
          SubSection.SetAttribute(GTX_ATTR_COLOR_R           , Format(FORMAT_NB_INTEGER, [Red(FF.CouleurFiltre)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_G           , Format(FORMAT_NB_INTEGER, [Green(FF.CouleurFiltre)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_B           , Format(FORMAT_NB_INTEGER, [Blue(FF.CouleurFiltre)]));
          SubSection.SetAttribute(GTX_ATTR_FILTER_EXPRESSION , PasStrToXMLStr(FF.Expression));
          SubSection.SetAttribute(GTX_ATTR_FILTER_DESCRIPTION, PasStrToXMLStr(FF.Description));
        SectionFiltres.AppendChild(SubSection);
      end;
    end;
    // section Entrées
    SectionEntrances := MyDocXML.CreateElement(GTX_KEY_SECTION_ENTRANCES);
      GHTopoRoot.AppendChild(SectionEntrances);
      KWYXZ(GTX_KEY_SECTION_ENTRANCES);
      for i:= 0 to GetNbEntrances() - 1 do
      begin
        E := GetEntrance(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_ENTRANCE);
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_IDX      , Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_IDTERRAIN, Format(FORMAT_STRING, [PasStrToXMLStr(E.eIDTerrain)]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_NAME     , Format(FORMAT_STRING, [PasStrToXMLStr(E.eNomEntree)]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_REFSERIE , Format(FORMAT_NB_INTEGER, [E.eRefSer]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_REFPT    , Format(FORMAT_NB_INTEGER, [E.eRefSt]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_X, Format(FORMAT_NB_REAL_3_DEC, [E.eXEntree]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_Y, Format(FORMAT_NB_REAL_3_DEC, [E.eYEntree]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_Z, Format(FORMAT_NB_REAL_3_DEC, [E.eZEntree]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_COLOR, ColorToGHTopoColor(E.eCouleur));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_OBS, PasStrToXMLStr(E.eObserv));
        SectionEntrances.AppendChild(SubSection);
      end;
    // section Réseaux (5 champs sur 5)
    SectionReseaux := MyDocXML.CreateElement(GTX_KEY_SECTION_RESEAUX);
      KWYXZ(GTX_KEY_SECTION_RESEAUX);
      GHTopoRoot.AppendChild(SectionReseaux);
      for i := IDX_MINI to GetNbReseaux - 1 do
      begin
        R := GetReseau(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_RESEAU);
          SubSection.SetAttribute(GTX_ATTR_RESEAU_IDX, Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_R, Format(FORMAT_NB_INTEGER, [Red(R.ColorReseau)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_G, Format(FORMAT_NB_INTEGER, [Green(R.ColorReseau)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_B, Format(FORMAT_NB_INTEGER, [Blue(R.ColorReseau)]));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_TYPE, Format(FORMAT_NB_INTEGER, [R.TypeReseau]));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_NAME,  PasStrToXMLStr(R.NomReseau));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_OBS,  PasStrToXMLStr(R.ObsReseau));
        SectionReseaux.AppendChild(SubSection);
      end;
    // section Secteurs (5 champs sur 5)
    SectionSecteurs := MyDocXML.CreateElement(GTX_KEY_SECTION_SECTEURS);
      KWYXZ(GTX_KEY_SECTION_SECTEURS);
      GHTopoRoot.AppendChild(SectionSecteurs);
      for i := IDX_MINI to GetNbSecteurs - 1 do
      begin
        SC := GetSecteur(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_SECTEUR);
          SubSection.SetAttribute(GTX_ATTR_SECTEUR_IDX    , Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_R, Format(FORMAT_NB_INTEGER, [Red(SC.CouleurSecteur)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_G, Format(FORMAT_NB_INTEGER, [Green(SC.CouleurSecteur)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_B, Format(FORMAT_NB_INTEGER, [Blue(SC.CouleurSecteur)]));
          SubSection.SetAttribute(GTX_ATTR_SECTEUR_NAME   , PasStrToXMLStr(SC.NomSecteur));
        SectionSecteurs.AppendChild(SubSection);
      end;

    // section Codes -- Champs OK
    SectionCodes := MyDocXML.CreateElement(GTX_KEY_SECTION_CODES);
      KWYXZ(GTX_KEY_SECTION_CODES);
      GHTopoRoot.AppendChild(SectionCodes);
      for i := IDX_MINI to GetNbCodes - 1 do
      begin
        C := GetCode(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_CODE);
          SubSection.SetAttribute(GTX_ATTR_CODE_NUMERO                     , Format(FORMAT_NB_INTEGER, [C.IDCode]));                   //  IDCode
          SubSection.SetAttribute(GTX_ATTR_CODE_TYPE                       , Format(FORMAT_NB_INTEGER, [0])); //C.ReservedInt]));              //  ReservedInt
          // TODO: Support de FactLong a valider
          SubSection.SetAttribute(GTX_ATTR_CODE_FACT_LONG                  , Format(FORMAT_NB_REAL_3_DEC, [1.00])); ///// SubSection.SetAttribute(GTX_ATTR_CODE_FACT_LONG     , Format(FORMAT_NB_REAL_3_DEC, [C.FactLong]));               //  FactLong    : double; // pour compatibilité ascendante
          SubSection.SetAttribute(GTX_ATTR_CODE_UCOMPASS                   , Format(FORMAT_NB_REAL_2_DEC, [C.GradAz]));                 //  GradAz
          SubSection.SetAttribute(GTX_ATTR_CODE_UCLINO                     , Format(FORMAT_NB_REAL_2_DEC, [C.GradInc]));                //  GradInc     : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_ANGLIMITE                  , Format(FORMAT_NB_REAL_2_DEC, [C.AngLimite]));              //  AngLimite   : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_L                      , Format(FORMAT_NB_REAL_3_DEC, [C.PsiL]));                   //  PsiL        : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_A                      , Format(FORMAT_NB_REAL_3_DEC, [C.PsiAz]));                  //  PsiAz       : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_P                      , Format(FORMAT_NB_REAL_3_DEC, [C.PsiP]));                   //  PsiP        : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_ERROR_TOURILLON            , Format(FORMAT_NB_REAL_3_DEC, [C.ErreurTourillon]));
          SubSection.SetAttribute(GTX_ATTR_CODE_DIAM_BOULE1                , Format(FORMAT_NB_REAL_3_DEC, [C.DiametreBoule1]));
          SubSection.SetAttribute(GTX_ATTR_CODE_DIAM_BOULE2                , Format(FORMAT_NB_REAL_3_DEC, [C.DiametreBoule1]));


          SubSection.SetAttribute(GTX_ATTR_CODE_OBS                        , PasStrToXMLStr(C.Commentaire));                              //  Commentaire : string;
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_AZ_CO            , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrAz.Co]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_AZ_ERR_MAX       , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrAz.ErreurMax]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_AZ_POS_ERR_MAX   , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrAz.PosErrMax]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_INC_CO           , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrInc.Co]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_INC_ERR_MAX      , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrInc.ErreurMax]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_INC_POS_ERR_MAX  , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrInc.PosErrMax]));

        SectionCodes.AppendChild(SubSection);
      end;
    // section Expés - Champs OK
    SectionSeances := MyDocXML.CreateElement(GTX_KEY_SECTION_EXPES);
      KWYXZ(GTX_KEY_SECTION_EXPES);
      GHTopoRoot.AppendChild(SectionSeances);
      for i := IDX_MINI to GetNbExpes - 1 do
      begin
        EX := GetExpe(i);
        {$WARNING: TEXpe.DateExpe à implementer}
        SubSection := MyDocXML.CreateElement(GTX_KEY_EXPE);
          SubSection.SetAttribute(GTX_ATTR_EXPE_NUMERO   , Format(FORMAT_NB_INTEGER   , [EX.IDExpe]));                                             // IDExpe      : integer;
          SubSection.SetAttribute(GTX_ATTR_EXPE_DATE     , DateYYYYMMDDToDateSQL(EX.AnneeExpe, EX.MoisExpe, EX.JourExpe));    // JourExpe    : integer;MoisExpe    : integer;AnneeExpe   : integer;
          SubSection.SetAttribute(GTX_ATTR_EXPE_IDXCOLOR , Format(FORMAT_NB_INTEGER   , [EX.IdxCouleur]));                                            // //Couleur     : Integer;
          SubSection.SetAttribute(GTX_ATTR_EXPE_DECLINAT , Format(FORMAT_NB_REAL_6_DEC, [EX.DeclinaisonInDegrees]));                                      // Declinaison : double;
          SubSection.SetAttribute(GTX_ATTR_EXPE_MODEDECL , Format(FORMAT_NB_INTEGER   , [Ord(EX.ModeDecl)]));                                           // ModeDecl    : integer; // deprecated;
          SubSection.SetAttribute(GTX_ATTR_EXPE_INCLINAT , Format(FORMAT_NB_REAL_6_DEC, [0.00])); //EX.Inclinaison]));                                      //  Inclinaison : double;
          SubSection.SetAttribute(GTX_ATTR_EXPE_SURVEY1  , PasStrToXMLStr(EX.Operateur));                                                          // Speleometre : String;
          SubSection.SetAttribute(GTX_ATTR_EXPE_SURVEY2  , PasStrToXMLStr(EX.ClubSpeleo));                                                         //  Speleographe: string;
          SubSection.SetAttribute(GTX_ATTR_EXPE_OBS      , PasStrToXMLStr(EX.Commentaire));                                                        //  Commentaire : string;
        SectionSeances.AppendChild(SubSection);
      end;
    // section Séries
    SectionSeries := MyDocXML.CreateElement(GTX_KEY_SECTION_SERIES);
      GHTopoRoot.AppendChild(SectionSeries);
      KWYXZ(GTX_KEY_SECTION_SERIES);
      for i := IDX_MINI to GetNbSeries - 1 do
      begin
        SER := GetSerie(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_SERIE);
          SubSection.SetAttribute(GTX_ATTR_SERIE_Numero  , Format(FORMAT_NB_INTEGER, [SER.GetNumeroDeSerie()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_NAME    , SER.GetNomSerie);
          SubSection.SetAttribute(GTX_ATTR_SERIE_ENTRANCE, Format(FORMAT_NB_INTEGER, [SER.GetNumeroEntrance()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_RESEAU  , Format(FORMAT_NB_INTEGER, [SER.GetNumeroReseau]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_SERDEP  , Format(FORMAT_NB_INTEGER, [SER.GetNoSerieDep()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_PTDEP   , Format(FORMAT_NB_INTEGER, [SER.GetNoPointDep()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_SERARR  , Format(FORMAT_NB_INTEGER, [SER.GetNoSerieArr()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_PTARR   , Format(FORMAT_NB_INTEGER, [SER.GetNoPointArr()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_COLOR   , Format(FORMAT_STRING, [ColorToHTMLColor(SER.GetCouleur())]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_CHANCE  , Format(FORMAT_NB_INTEGER, [SER.GetChance]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_OBSTACLE, Format(FORMAT_NB_INTEGER, [SER.GetObstacle]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_OBS     , PasStrToXMLStr(SER.GetObsSerie));
          SubSection.SetAttribute(GTX_ATTR_SERIE_RAIDEUR , Format(FORMAT_NB_REAL_6_DEC, [SER.GetRaideur]));

        SectionSeries.AppendChild(SubSection);
        SubSubSection := MyDocXML.CreateElement(GTX_KEY_STATIONS);
        SubSection.AppendChild(SubSubSection);
        for S := 0 to SER.GetNbVisees - 1 do
        begin
          ST := SER.GetVisee(S);
          SubSubSubSection := MyDocXML.CreateElement(GTX_KEY_VISEE);
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_ID      , Format(FMTSERST, [SER.GetNumeroDeSerie(), S]));     //
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LBL     , PasStrToXMLStr(ST.IDTerrainStation));         // IDTerrainStation: string;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_SECTEUR , Format(FORMAT_NB_INTEGER, [ST.IDSecteur]));             // IDSecteur : integer;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_TYPE    , Format(FORMAT_NB_INTEGER, [Ord(ST.TypeVisee)]));           // TypeVisee : TTypeDeVisee;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_CODE    , Format(FORMAT_NB_INTEGER, [ST.Code]));                     // Code      : integer;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_EXPE    , Format(FORMAT_NB_INTEGER, [ST.Expe]));                     // Expe      : integer;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LONG    , Format(FORMAT_NB_REAL_3_DEC, [ST.Longueur]));               // Longueur  : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_AZ      , Format(FORMAT_NB_REAL_3_DEC, [ST.Azimut]));                 // Azimut    : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_P       , Format(FORMAT_NB_REAL_3_DEC, [ST.Pente]));                  // Pente     : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LG      , Format(FORMAT_NB_REAL_3_DEC, [ST.LG]));                     // LG        : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LD      , Format(FORMAT_NB_REAL_3_DEC, [ST.LD]));                     // LD        : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_HZ      , Format(FORMAT_NB_REAL_3_DEC, [ST.HZ]));                     // HZ        : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_HN      , Format(FORMAT_NB_REAL_3_DEC, [ST.HN]));                     // HN        : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_OBS     , PasStrToXMLStr(ST.Commentaires));                             // Commentaires    : string;
          SubSubSection.AppendChild(SubSubSubSection);
        end;
      end;
    // section Antennes (à la fin du document XML)
    QNbAntennes := GetNbAntennes();
    if (QNbAntennes > 0) then
    begin
      SectionAntennes := MyDocXML.CreateElement(GTX_KEY_SECTION_ANTENNAS);
      KWYXZ(GTX_KEY_SECTION_ANTENNAS);
      GHTopoRoot.AppendChild(SectionAntennes);
      for i := IDX_MINI to QNbAntennes - 1 do
      begin
        ANT := GetViseeAntenne(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_ANTENNA_SHOT);
          SubSection.SetAttribute(GTX_KEY_ANTENNA_NETWORK , Format(FORMAT_NB_INTEGER, [ANT.Reseau]));                      // Reseau              : integer;
          SubSection.SetAttribute(GTX_KEY_ANTENNA_SECTEUR , Format(FORMAT_NB_INTEGER, [ANT.Secteur]));                     // Secteur             : integer;
          SubSection.SetAttribute(GTX_KEY_ANTENNA_SERIE   , Format(FORMAT_NB_INTEGER, [ANT.SerieDepart]));                 // SerieDepart         : integer;
          SubSection.SetAttribute(GTX_KEY_ANTENNA_POINT   , Format(FORMAT_NB_INTEGER, [ANT.PtDepart]));                    // PtDepart            : integer;
          SubSection.SetAttribute(GTX_KEY_ANTENNA_LONG    , Format(FORMAT_NB_REAL_3_DEC, [ANT.Longueur]));                  // Longueur            : double;      //         'Longueur
          SubSection.SetAttribute(GTX_KEY_ANTENNA_AZIMUT  , Format(FORMAT_NB_REAL_3_DEC, [ANT.Azimut]));                    // Azimut              : double;      //         'Azimut
          SubSection.SetAttribute(GTX_KEY_ANTENNA_PENTE   , Format(FORMAT_NB_REAL_3_DEC, [ANT.Pente]));                     // Pente               : double;      //         'Pente
        SectionAntennes.AppendChild(SubSection);
      end;
    end;
    KWYXZ('--> Serializing XML file');
    // sérialisation du fichier
    WriteXMLFile(MyDocXML, FichierXML);
    KWYXZ('--> Done');
  finally
    FreeAndNil(MyDocXML);//MyDocXML.Free;
  end;
end;
//*****************************************************************************
// Simplifier les visées en antenne d'une station
procedure TToporobotStructure2012.ResetMarkersToDelete(); inline;
begin
  FTableViseesAntenne.ResetMarkersToDelete();
end;

function TToporobotStructure2012.PurgerTableAntennes(): integer; inline;
begin
  Result := FTableViseesAntenne.Purger();
end;

procedure TToporobotStructure2012.SimplifyAntennesOfStation(const QSerie: TNumeroSerie; const QPoint: integer;
                                                            const Tolerance: double);
var
  MySerie: TObjSerie;
  QIdx: integer;
  MyVisee: TUneVisee;
  MyCode: TCode;
  MyExpe: TExpe;
begin
  if (GetSerieByNumeroSerie(QSerie, MySerie, QIdx)) then
  begin
    MyVisee := MySerie.GetVisee(QPoint);
    MyCode  := GetCodeByNumero(MyVisee.Code);
    MyExpe  := GetExpeByNumero(MyVisee.Expe);
    FTableViseesAntenne.SimplifyAntennesOfStation(QSerie, QPoint, MyCode, MyExpe, Tolerance);
  end;
end;
// Simplifier les visées d'une série
procedure TToporobotStructure2012.SimplifyAntennesOfSerie(const QSerie: TObjSerie; const Tolerance: double);
var
  NbStations, St: integer;
  QNumSerie: TNumeroSerie;
begin
  QNumSerie := QSerie.GetNumeroDeSerie();
  AfficherMessage(Format('SimplifyAntennesOfSerie %d - Tolerance = %.2f m', [QNumSerie, Tolerance]));
  NbStations := QSerie.GetNbVisees();
  if (0 = NbStations) then Exit;
  for St := 1 to NbStations - 1 do SimplifyAntennesOfStation(QNumSerie, St, Tolerance);
  PurgerTableAntennes();
end;
procedure TToporobotStructure2012.SimplifyAntennesOfAllReseau(const Tolerance: double);
var
  NbSeries, i: Integer;
  MySerie: TObjSerie;
  EWE: String;
begin
  NbSeries := GetNbSeries();
  AfficherMessage(Format('SimplifyAntennesOfAllReseau: %d series - Tolerance = %.2f m', [NbSeries, Tolerance]));
  ResetMarkersToDelete();
  for i := 1 to NbSeries - 1 do
  begin
    MySerie := GetSerie(i);
    EWE := Format('Série %d (%d stations)', [MySerie.GetNumeroDeSerie(), MySerie.GetNbVisees()]);
    if (Assigned(FProcDispProgression)) then FProcDispProgression(EWE, i, 0, NbSeries, 100);
    SimplifyAntennesOfSerie(MySerie, Tolerance);
  end;
end;

//==============================================================================
// Calcul des LRUD d'une station, depuis une série
function TToporobotStructure2012.CalcLRUD_AStationOfTheSerie(const MySerie: TObjSerie; const QNumeroStation: integer): boolean;
const
  DEMI_ECART_MAXIMAL_AZIMUT_DIRECTEUR = 10.00;
var
  MyCurrentVisee   : TUneVisee;
  MyNearVisee      : TUneVisee;
  MyCode           : TCode;
  MyAntenne        : TViseeAntenneFound;
  QArrAntennesFound: TArrayOfTViseeAntenne;

  i, n             : Integer;
  Quadrant         : double;
  AzimutMoyen, AzimutDirecteurGauche, AzimutDirecteurDroit: Double;
  LGMoyen, LDMoyen, HZMoyen, HNMoyen: double;
  TwoPIsurGradInclin, QInclin: double;
  dp, dz: ValReal;
  b1, b2, b3, b4: Boolean;

begin
  Result := false;
  AfficherMessage(Format('%s.CalcLRUD_OfStationOfAnSerie: Serie %d', [ClassName, MySerie.GetNumeroDeSerie()]));
  MyCurrentVisee     := MySerie.GetVisee(QNumeroStation);
  MyCode             := GetCodeByNumero(MyCurrentVisee.Code);
  // extraction des visées radiantes
  n := ExtractViseesAntennesOfStation(MySerie.GetNumeroDeSerie(), QNumeroStation, QArrAntennesFound);
  if (n = 0) then Exit;
  Quadrant  := 90.00;
  // calcul du quadrant
  case round(MyCode.GradAz) of
    359, 360: Quadrant :=   90.00;  // visées directes en degrés
    399, 400: Quadrant :=  100.00;  // visées directes en grades
    349, 350: Quadrant :=  -90.00;  // visées inverses en degrés
    389, 390: Quadrant := -100.00;  // visées inverses en grades
  end;
  // calcul de l'azimut directeur: Visées d'extrémité = perpendiculaires à l'azimut
  if ((QNumeroStation = 1) OR (QNumeroStation = (MySerie.GetNbVisees() - 1))) then
  begin
    AzimutMoyen := MyCurrentVisee.Azimut;
    AfficherMessage(Format('Station d''extrémité - Az0 = %.3f', [MyCurrentVisee.Azimut]));
  end
  else
  begin
    MyNearVisee  := MySerie.GetVisee(QNumeroStation + 1);
    AfficherMessage(Format('Station Intermédiaire - Az0 = %.3f, Az1 = %.3f', [MyCurrentVisee.Azimut, MyNearVisee.Azimut]));
    AzimutMoyen    := CalcAzimutMoyen(MyCode.GradAz, [MyCurrentVisee.Azimut, MyNearVisee.Azimut]);
  end;
  AzimutDirecteurGauche := AzimutMoyen - Quadrant;
  AzimutDirecteurDroit  := AzimutMoyen + Quadrant;
  // Moyenne des visées proches de l'azimut directeur à gauche
  // En toute rigueur, il faudrait calculer la bounding-box de la section projetée sur le plan d'azimut MyAzimutDirecteurDroit
  // mais comme il s'agit ici de sections transversales (de toutes manières purement indicatives),
  // on va se contenter de faire un max des projections Hz et Vx
  LGMoyen := -1E8;
  LDMoyen := -1E8;
  HZMoyen := -1E8;
  HNMoyen :=  1E8;
  case round(MyCode.GradInc) of
    359, 360, 361: TwoPIsurGradInclin := 2 * PI / 360.00;
    399, 400, 401: TwoPIsurGradInclin := 2 * PI / 400.00;
  else
    TwoPIsurGradInclin := 2 * PI / 360.00;
  end;
  for i := 0 to n - 1 do
  begin
    MyAntenne := QArrAntennesFound[i];
    if (IsNearToAzimut(MyCode.GradAz, MyAntenne.VA.Azimut, AzimutDirecteurGauche, DEMI_ECART_MAXIMAL_AZIMUT_DIRECTEUR)) then
    begin
      QInclin := MyAntenne.VA.Pente  * TwoPIsurGradInclin;
      dp := MyAntenne.VA.Longueur * cos(QInclin);
      dz := MyAntenne.VA.Longueur * sin(QInclin);
      LGMoyen := Max(LGMoyen, dp);
      HZMoyen := Max(HZMoyen, dz);
      HNMoyen := Min(HNMoyen, dz);
    end;
    if (IsNearToAzimut(MyCode.GradAz, MyAntenne.VA.Azimut, AzimutDirecteurDroit, DEMI_ECART_MAXIMAL_AZIMUT_DIRECTEUR)) then
    begin
      QInclin := MyAntenne.VA.Pente  * TwoPIsurGradInclin;
      dp := MyAntenne.VA.Longueur * cos(QInclin);
      dz := MyAntenne.VA.Longueur * sin(QInclin);
      LDMoyen := Max(LGMoyen, dp);
      HZMoyen := Max(HZMoyen, dz);
      HNMoyen := Min(HNMoyen, dz);
    end;
  end;
  // on ne met à jour que si les calculs sont OK
  b1 := InRange(LGMoyen, 0, SEUIL_LONGUEUR_MAXI_TOPOROBOT);
  b2 := InRange(LDMoyen, 0, SEUIL_LONGUEUR_MAXI_TOPOROBOT);
  b3 := InRange(HZMoyen, 0, SEUIL_LONGUEUR_MAXI_TOPOROBOT);
  b4 := InRange(HNMoyen, 0, SEUIL_LONGUEUR_MAXI_TOPOROBOT);
  if (b1) then MyCurrentVisee.LG := LGMoyen;
  if (b2) then MyCurrentVisee.LD := LDMoyen;
  if (b3) then MyCurrentVisee.HZ := HZMoyen;
  if (b4) then MyCurrentVisee.HN := HNMoyen;
  if (b1 and b2 and b3 and b4) then MyCurrentVisee.Commentaires += ' * LRUD recalculées';
  MySerie.PutVisee(QNumeroStation, MyCurrentVisee);
end;

// Calcul des LRUD de toutes les stations d'une série
function TToporobotStructure2012.CalcLRUD_AllStationsOfTheSerie(const MySerie: TObjSerie): boolean;
var
  n, i: Integer;
begin
  Result := false;
  AfficherMessage(Format('%s.CalcLRUD_AllStationsOfTheSerie: Serie %d', [ClassName, MySerie.GetNumeroDeSerie()]));
  n := MySerie.GetNbVisees();
  if (n > 1) then
  begin
    for i := 1 to n - 1 do CalcLRUD_AStationOfTheSerie(MySerie, i);
  end;
end;

// Calcul ponctuel des LRUD d'une station
// (ne pas utiliser pour calculer toutes les stations d'une série; utiliser CalcLRUD_AllStationsOfTheSerie() pour celà)
function TToporobotStructure2012.CalcLRUD_AStationOfSerieByNumSerSt(const QNumeroSerie: TNumeroSerie; const QNumeroStation: integer): boolean;
var
  MySerie    : TObjSerie;
  QIdxSerie: integer;
begin
  Result := false;
  AfficherMessage(Format('%s.CalcLRUDOfStationFromRadiantShots: Serie %d, Station: %d', [ClassName, QNumeroSerie, QNumeroStation]));
  if (not GetSerieByNumeroSerie(QNumeroSerie, MySerie, QIdxSerie)) then Exit(false);
  Result := CalcLRUD_AStationOfTheSerie(MySerie, QNumeroStation);
end;

//******************************************************************************
// détection de doublons dans les numéros de série, expés et codes
// retourne l'index interne de l'objet, ou -1 si aucune erreur
function TToporobotStructure2012.HasDoublonsInNumsSeries(const NS: TNumeroSerie; out QInternalIdx: integer): boolean;
var
  MySerie: TObjSerie;
  Nb, i, NbOccur: Integer;
begin
  QInternalIdx := 0;
  result := false;
  NbOccur := 0;
  Nb := GetNbSeries();
  for i := 0 to Nb - 1 do
  begin
    MySerie := GetSerie(i);
    if (NS = MySerie.GetNumeroDeSerie()) then
    begin
      NbOccur += 1;
      QInternalIdx := i;
    end;
  end;
  Result := (NbOccur > 1);
end;


(*
procedure TToporobotStructure2012.SaveToJSON(const Filename: TStringDirectoryFilename);
var
  MySerie: TObjSerie;
  JSMyserie: TJSONObject;
  JSLesVisees: TJSONArray;
  NbVisees, v: Integer;
  MyVisee: TUneVisee;
begin
  AfficherMessage(Format('%s.SaveToJSON: %s', [ClassName, Filename]));

  // les séries
  MySerie := getSerie(1);
    JSMyserie := TJSonObject.Create;
    JSMyserie.Add(GTX_ATTR_SERIE_Numero     , MySerie.GetNumeroDeSerie());
    JSMyserie.Add(GTX_ATTR_SERIE_NAME       , MySerie.GetNomSerie());
    JSMyserie.Add(GTX_ATTR_SERIE_SERDEP     , MySerie.GetNoSerieDep());
    JSMyserie.Add(GTX_ATTR_SERIE_PTDEP      , MySerie.GetNoPointDep());
    JSMyserie.Add(GTX_ATTR_SERIE_SERARR     , MySerie.GetNoSerieArr());
    JSMyserie.Add(GTX_ATTR_SERIE_PTARR      , MySerie.GetNoPointArr());
    // les visées
    JSLesVisees := TJSONArray.Create();
      NbVisees := MySerie.GetNbVisees();
      for v := 0 to NbVisees - 1 do
      begin
        MyVisee := MySerie.GetVisee(v);
      end;








  JA:=
JA.Add(’street’,’5 Main Street’);
JA.Add(’City’,’San Diego, CA’);
JA.Add(’Zip’,91912);
JT:=TJSonArray.Create;
JT.Add(’619 332-3452’);
JT.Add(’664 223-4667’);
J:=TJSONObject.Create;
J.Add(’name’,’Mary Lebow’);
J.Add(’address’,JA);
J.Add(’phonenumbers’,JT);
JAB:=TJSONObject.Create;
JAB.Add(’addressbook’,J);
Writeln(JAB.AsJSON);
JAB.Free;
    //*)


end.


