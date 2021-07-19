unit UnitEntitesExtended;

// Table des entités étendue
// 06/11/2013: Fonctions de tri fixées
// 27/11/2013: Recensement des dates (nb de dates inférieur ou égal au nb de séances)
// 13/04/2013: Export SVG
// 14/04/2014: Fixation de bugs
// 04/08/2014: Export KML et OSM: Correction des pbs liés aux entrées
// 20/12/2014: Export des antennes au format GPX
// 06/02/2015: Le mode ftFLAGS du MétaFiltre est désactivé (fonctionne mais jamais utilisé en pratique)
// 06/02/2015: Remaniment des fonctions de recherche
// 12/02/2015: Ajout d'un tri ascendant sur le membre TagDouble des entités
// 19/06/2015: Export OSM: Entrées OK
// 24/06/2015: Export GCD: Les ID antennes contiennent le point de rattachement
// 12/10/2015: Export PostScript
// 22/12/2015: Génération de cartes dynamiques Leaflet
// 29/02/2015: Les fonctions de recherche balayent la table des entrées puis celle des entités
// 20/03/2016: Les listes passent aux génériques
// 27/04/2016: Ajout du filtre ENTREE_RATTACHEMENT au MétaFiltre
// 20/05/2016: La liste des entités passe aux génériques
// 03/08/2017: Modification de GetEntiteFromSerSt'): Très forte augmentation de la vitesse de recherche
// 02/01/2018: Les fonctions de dessin des diagrammes vont dans TCdrRoseDiagramExt et TCdrDepthDiagramExt
// 13/04/2018: Unification de TPostScriptCanvas et TSVGCanvas
// 29/11/2018: Les visées et antennes vont dans deux tables différentes - Unification de noms pour les objets TEntrance
// 06/03/2020: Point de contrôle temporel (contrôle de version)
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  //GHCD_Types,
  StructuresDonnees,
  UnitStructuresMetaFiltre,
  Common,
  UnitListesSimplesWithGeneriques,
  UnitClassPalette,
  Classes, SysUtils, Graphics, math,
  Clipbrd, LCLType,
  ConvertisseurJPC,
  //UnitDXFDrawing,
  SVGCanvasUnit,
  PostScriptCanvasUnit;
type

{ TBDDEntites }

 TBDDEntites = class
  private
    // code EPSG
    FCodeEPSG       : integer;
    // tables des entités
    FTableDesVisees  : TTableEntitesVisees; // visées
    FTableDesAntennes: TTableEntitesAntennes; // antennes
    // tables
    FTableEntrances : TTableEntrances;
    FTableSecteurs  : TTableSecteurs;
    FTableReseaux   : TTableReseaux;
    FTableExpes     : TTableExpes;
    FTableCodes     : TTableCodes;
    FTableJonctions : TTableJonctionsXYZ;
    // palette Toporobot
    FPalette256   : TPalette256;
    // barre de progression
    FProcDispProgression: TProcDisplayProgression;
    // altitude du point zéro
    FPositionDuPointZero: TPoint3Df;
    // encombrement du reseau
    FCoinBasGauche : TPoint3Df;
    FCoinHautDroit : TPoint3Df;
    // encombrement de la zone métafiltrée
    FMetafilteredCoinBasGauche : TPoint3Df;
    FMetafilteredCoinHautDroit : TPoint3Df;
    // ID des stations extremes
    FIDStationXMini: string;
    FIDStationYMini: string;
    FIDStationZMini: string;
    FIDStationXMaxi: string;
    FIDStationYMaxi: string;
    FIDStationZMaxi: string;
    // histogramme des profondeurs et des directions
    FClassRepartDepthDiagram: TVecteurDouble;
    FClassRepartRoseDiagram : TVecteurDouble;
    // tableaux pour statistiques
    FSpeleometrieParReseaux,
    FSpeleometrieParSecteurs,
    FSpeleometrieParExpes,
    FSpeleometrieParCodes,
    FSpeleometrieParDates: TTableauVentilationSpeleometrie;
    FRecapitulation      : TVentilationSpeleometrie;
    // Longueur du réseau
    FLongueurDuReseau: double;
    // couleurs mini et maxi
    FCouleurZMin    : TColor;
    FCouleurZMax    : TColor;
    // tableau des dates
    FListeDesDates : array of TDateTime;
    FNbDates       : integer;
    // nombre de visées retenues par le MétaFiltre
    FNbreViseesRetenues: integer;
    // Derniers résultats de GetEntiteFromSerSt
    FLastEntiteFound : TBaseStation;
    //FLastSerFound    : TNumeroSerie;
    //FLastPtFound     : TNumeroStation;
    // canevas SVG
    FSVGCanvas: TSVGCanvas;

    procedure AfficherErreurSVG();



    function GetEntiteViseeFromCle(const DoMatchExact: boolean; const Cle: string; out EE: TBaseStation): boolean;

    procedure ViderTablesSimples();
    function CalcSpeleometrieReseauOuSecteur(const QMode: integer; const IdxReseauOuSecteur: integer; out TR: TSpeleometrieReseauOuSecteur): boolean;

  public
    constructor Create;
    function  Initialiser(const PositionPointZero: TPoint3Df; const ColorZMini, ColorZMaxi: TColor): boolean;
    procedure CalcLengthReseau();

    function  GetPalette256(): TPalette256;
    function GetLongueurDuReseau(): double;
    procedure SetProcDisplayProgression(const ProcProgression: TProcDisplayProgression);
    procedure ResetTables();
    procedure ViderLesViseesEtAntennes();
    // highlighting de visées
    procedure DisHighLightAll();
    procedure HighlightSeries(const QArrSeriesHighlighted: array of TNumeroSerie);
    procedure HighlightExpes(const QArrExpesHighlighted: array of TNumeroExpe);
    procedure HighlightCodes(const QArrCodesHighlighted: array of TNumeroCode);
    procedure HighlightEntrances(const QArrEntrancesHighlighted: array of TNumeroEntrance);
    procedure HighlightReseaux(const QArrReseauxHighlighted: array of TNumeroReseau);
    procedure HighlightSecteurs(const QArrSecteursHighlighted: array of TNumeroSecteur);



    procedure Finaliser();
    // Code EPSG
    function  GetCodeEPSG(): integer;
    procedure SetCodeEPSG(const C: integer);

    // Accès aux visées
    procedure AddEntiteVisee(const E: TBaseStation);
    function  GetEntiteVisee(const Idx: integer): TBaseStation;
    procedure PutEntiteVisee(const Idx: integer; const E: TBaseStation);
    procedure SetEntiteViseeVisible(const Idx: integer; const QVisible: boolean);

    function GetNbEntitesVisees(): integer;
    // dégradés de couleurs
    procedure SetColorZMiniZMaxi(const ColorZMini, ColorZMaxi: TColor);
    function  GetColorZMaxi(): TColor;
    function  GetColorZMini(): TColor;

    // Accès aux antennes
    procedure AddEntiteAntenne(const E: TBaseStation);
    function  GetEntiteAntenne(const Idx: integer): TBaseStation;
    function  GetNbEntitesAntennes(): integer;
    procedure PutEntiteAntenne(const Idx: integer; const E: TBaseStation);

    // couleurs en fonction des réseaux
    function  GetCouleurEntiteByEntrance(const E: TBaseStation): TGHTopoColor;
    function  GetCouleurEntiteByIdxEntrance(const Idx: integer): TGHTopoColor;
    function  GetCouleurEntiteByReseau(const E: TBaseStation): TGHTopoColor;
    function  GetCouleurEntiteByIdxReseau(const Idx: integer): TGHTopoColor;
    // couleurs en fonction des secteurs
    function  GetCouleurEntiteBySecteur(const E: TBaseStation): TGHTopoColor;
    function  GetCouleurEntiteByIdxSecteur(const Idx: integer): TGHTopoColor;
    // couleurs en fonction des expés
    function  GetCouleurEntiteByExpe(const E: TBaseStation): TGHTopoColor;
    function  GetCouleurEntiteByIdxExpe(const IDX: TNumeroExpe): TGHTopoColor;
    // gestion des entrées
    procedure ClearTableEntrances();
    procedure AddAEntrance(const AEntree: TEntrance);
    function  GetEntrance(const NumEntree: integer): TEntrance;
    function  GetNbEntrances(): integer;
    // gestion des réseaux
    procedure ClearTableReseaux();
    procedure AddReseau(const AReseau: TReseau);
    function  GetReseau(const NumReseau: integer): TReseau;
    function  GetNbReseaux(): integer;
    // gestion des secteurs
    procedure ClearTableSecteurs();
    procedure AddSecteur(const ASecteur: TSecteur);
    function  GetSecteur(const NumSecteur: integer): TSecteur;
    function  GetNbSecteurs(): integer;
    // gestion des expés
    procedure ClearTableExpes();
    procedure AddExpe(const LaExpe: TExpe);
    function  GetExpe(const NumExpe: integer): TExpe;
    function  GetExpeByIndex(const Idx: TNumeroExpe): TExpe;
    function  GetNbExpes(): integer;
    // gestion des codes
    procedure ClearTableCodes();
    procedure AddCode(const LeCode: TCode);
    function  GetCode(const NoCode: integer): TCode;
    function  GetCodeByIndex(const Idx: TNumeroCode): TCode;
    function  GetNbCodes(): integer;
    // jonctions
    procedure AddJonction(const J: TJonctionXYZ);
    function  GetJonction(const N: integer): TJonctionXYZ;
    function  GetNbJonctions(): integer;
    function  FindJonctionBySerieSt(const Ser: TNumeroSerie; const St: TNumeroStation; out QJonction: TJonctionXYZ): boolean;
    procedure SortJonctions();
    procedure ExporterListeJonctions(const ToClipboard: boolean; const QFilename: TStringDirectoryFilename);


    // utilitaires de tri
    procedure SortByDepth(); inline;
    procedure SortBySerSts(); inline;
    procedure SortByTagDouble(); inline;
    // métafiltre
    function MetaFiltre(const Filtre: string; out QDevelVisees: double): integer;
    // encombrement du reseau
    procedure SetMinMax(const DoMetafilteredOnly: boolean);
    // encombrement de la cavité
    function GetCoinBasGauche(): TPoint3Df; inline;
    function GetCoinHautDroit(): TPoint3Df; inline;
    // encombrement de la zone métafiltrée
    function GetMetafilteredCoinBasGauche(): TPoint3Df; inline;
    function GetMetafilteredCoinHautDroit(): TPoint3Df; inline;
    function GetIDStationXMaxi(): string; inline;
    function GetIDStationXMini(): string; inline;
    function GetIDStationYMaxi(): string; inline;
    function GetIDStationYMini(): string; inline;
    function GetIDStationZMaxi(): string; inline;
    function GetIDStationZMini(): string; inline;
    // tableau récapitulatif
    function  GetRecapitulation(): TVentilationSpeleometrie;

    // fonctions de recherche
    function GetStationOrEntranceFromXYZ(const QX, QY, QZ: double;
                                         const RayonMaxiCapture: double;
                                         const TablesAParcourir: TBDDEntitesFindTablesAParcourir;
                                         const IgnoreZ: boolean;
                                         out   InternalIdx: integer;
                                         out   BP: TBaseStation;
                                         out   EN: TEntrance;
                                         out   Distance: double;
                                         out   ER: TBDDEntitesFindViseeOrEntrance) : boolean;

    function  GetEntiteViseeFromSerSt(const Ser: TNumeroSerie; const St: TNumeroStation; out EE: TBaseStation): boolean;
    function  FindStationByCle(const DoMatchExact: boolean; const Cle: string; out E: TBaseStation): Boolean;
    function  FindEntranceByCle(const DoMatchExact: boolean; const Cle: string; out E: TEntrance): boolean;


    // statistiques
    procedure CalculSpeleometrie();
    // définir l'altitude du point zéro de la cavité
    procedure SetPositionPointZero(const QPositionPointZero: TPoint3Df);
    function  GetPositionPointZero(): TPoint3Df;
    function  GetDeltaXYZFromPositionSt0(const ST: TBaseStation): TPoint3Df;
    function  GetSpeleometrieParReseauxByIdx(const Idx: integer): TVentilationSpeleometrie;
    function  GetSpeleometrieParSecteurByIdx(const Idx: integer): TVentilationSpeleometrie;
    function  GetSpeleometrieParExpeByIdx(const Idx: integer): TVentilationSpeleometrie;
    function  GetSpeleometrieParCodeByIdx(const Idx: integer): TVentilationSpeleometrie;
    function  GetSpeleometrieParDateByIdx(const Idx: integer): TVentilationSpeleometrie;
    // recensement des dates
    function  RecenserDates(): integer;
    function  GetUneDate(const Idx: integer): TDateTime;
    function  GetNbDates(): integer;
    // utilitaire d'export vers logiciels de cartographie
    // export GHCavedraw
    procedure ExportForGHCaveDraw(const MyFile: TStringDirectoryFilename; const QMyFiltre: string);
    // couleur en fonction de l'altitude
    procedure CalcCouleursByDepth(const QColorZMini, QColorZMaxi: TColor);
    // couleur en fonction du mode de travail
    function  GetColorViseeFromModeRepresentation(const M: TModeRepresentationGaleries; const E: TBaseStation): TGHTopoColor;
    // diagrammes des directions et des altitudes
    function  ParseRoseDiagram(const NbPetales: integer; const ViseeMini: double): boolean;
    function  ParseDepthHistogramme(const NbPetales: integer; const ViseeMini: double; const Normalize: boolean): Boolean;
    // export OpenOffice;org
    procedure ExportVue2DSVG(const Filename: TStringDirectoryFilename;
                             const DoXHTML: boolean;
                             const Param2D: TVue2DParams;
                             const QProcProgress: TProcDisplayProgression);
    procedure ExportVue2DPostScript(const PSFileName: TStringDirectoryFilename; const FM: TModeRepresentationGaleries; const FE: TSetElementsDrawn);
    // export des antennes comme nuage de points
    procedure ExporterAntennesNuagePoints(const FichierPLY: TStringDirectoryFilename);
    // spéléométrie simplifiée pour réseaux ou secteurs
    // Mode: 1 = réseau; 2 = secteurs
    function  CalcSpeleometrieParReseauxOuSecteurs(const QMode: integer; out TR: TArraySpeleometrieReseauOuSecteur): boolean;
    function  GetNbEntitesMetafiltrees(): integer;

    // calculer une visée puis l'ajouter
    // ceci afin de pouvoir visualiser immédiatement une visée sans avoir à recalculer le réseau
    function  GenererEntiteDepuisUneVisee(const QStation: TToporobotIDStation;
                                         const QV: TUneVisee; out EE: TBaseStation): boolean;
    // extraction des data précalculées pour les diagrammes
    function  GetClassesRepartDepthDiagram(): TVecteurDouble; inline;
    function  GetClassesRepartRoseDiagram(): TVecteurDouble; inline;
    // export vers Therion DXVI
    procedure ExportVersTherionXVI(const FichierXVI: TStringDirectoryFilename);
    // Croquis de terrain: Calcul des coordonnées absolues depuis
    // un point référencé par un TIDBasePoint et un Offset
    function  CalcCoordinatesFromBasePtAndOffset(const BP: TIDBaseStation; const Offset: TPoint3Df; out CA: TPoint3Df): boolean;
    // surlignage
    procedure HighLightVisees(const S: TModeSelectionListe; const N: integer);
    // dégradé
    function  CalcColorDegradeByAltitude(const Z: double): TColor;
end;


implementation
uses
  DGCDummyUnit,
  Dialogs;

const
  LOW_INDEX = 0;
  INVDEGREES = 180.0 / PI;


// Trier les visées par ZOrder
function SortEntiteZOrder(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TBaseStation;
  bidon1: double;
  bidon2: double;
begin
  E1:=Item1;
  E2:=Item2;
  bidon1 := 0.50 * (E1.PosExtr0.Z + E1.PosStation.Z);
  bidon2 := 0.50 * (E2.PosExtr0.Z + E2.PosStation.Z);
  if      (bidon1 < bidon2) then Result := -1
  else if (bidon1 = bidon2) then Result :=  0
  else                           Result :=  1;
end;
// Trier les visées par TagDouble (e.g. distance)
function SortEntiteByTagDouble(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TBaseStation;
  bidon1: double;
  bidon2: double;
begin
  E1 := Item1;
  E2 := Item2;
  bidon1 := E1.TagDouble;
  bidon2 := E2.TagDouble;
  if      (bidon1 < bidon2) then Result := -1
  else if (bidon1 = bidon2) then Result :=  0
  else                           Result :=  1;
end;
function SortEntiteSerSt(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TBaseStation;
  P1, P2: int64;
begin
  E1:=Item1;
  E2:=Item2;
  P1 := NB_MAXI_SERIES_PAR_CAVITE * E1.Entite_Serie + E1.Entite_Station;
  P2 := NB_MAXI_SERIES_PAR_CAVITE * E2.Entite_Serie + E2.Entite_Station;
  if      (P1 < P2) then Result := -1
  else if (P1 = P2) then Result :=  0
  else                   Result :=  1;
end;

function QMakeTIDStation(const QS, QT:integer): TIDBaseStation; inline;
begin
  result := NB_MAXI_SERIES_PAR_CAVITE * QS + MULTIPLICATEUR_STATION * QT;
end;

function SortJonctionBySerSt(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TJonctionXYZ;
  P1, P2: TIDBaseStation;
begin
  E1:=Item1;
  E2:=Item2;
  P1 := QMakeTIDStation( E1.NoSer, E1.NoSt); //NB_MAXI_SERIES_PAR_CAVITE * E1.NoSer + E1.NoSt;
  P2 := QMakeTIDStation( E2.NoSer, E2.NoSt); //NB_MAXI_SERIES_PAR_CAVITE * E1.NoSer + E1.NoSt;
  if      (P1 < P2) then Result := -1
  else if (P1 = P2) then Result :=  0
  else                   Result :=  1;
end;

//******************************************************************************
{ TBDDEntites }

constructor TBDDEntites.Create;
begin
  inherited Create;
  // créer la palette
  FPalette256 := TPalette256.Create;
  FPalette256.GenerateTOPOROBOTPalette();
  FNbreViseesRetenues := 0;
  // créer les tables
  FTableDesVisees      := TTableEntitesVisees.Create; // les visées
  FTableDesAntennes    := TTableEntitesAntennes.Create; // les antennes

  FTableEntrances      := TTableEntrances.Create;
  FTableReseaux        := TTableReseaux.Create;
  FTableSecteurs       := TTableSecteurs.Create;
  FTableCodes          := TTableCodes.Create;
  FTableExpes          := TTableExpes.Create;
  FTableJonctions      := TTableJonctionsXYZ.Create;
end;


function TBDDEntites.Initialiser(const PositionPointZero: TPoint3Df; const ColorZMini, ColorZMaxi: TColor): boolean;
var
  E: TBaseStation;
begin
  AfficherMessage(Format('%s.Initialise()', [ClassName]));
  SetColorZMiniZMaxi(ColorZMini, ColorZMaxi);
  FLastEntiteFound.Entite_Serie   := 0;
  FLastEntiteFound.Entite_Station := 0;
  SetPositionPointZero(PositionPointZero);  // position du point zéro
  Result := false;
  try
    ResetTables();
    E.Entite_Serie    := 1;
    E.Entite_Station  := 0;
    E.eEntrance       := 0;
    E.eReseau         := 0;
    E.eSecteur        := 0;
    E.Type_Entite     := tgDEFAULT;
    E.CouleurStd      := clBlue;
    E.CouleurDegrade  := clwhite;
    E.IsPOI           := false;
    E.IDTerrain       := '';
    E.Enabled         := true;
    E.eCode           := 1;
    E.eExpe           := 1;
    E.DateLeve        := Now;
    E.oLongueur       := 0.0001;
    E.oAzimut         := 0.0001;
    E.oPente          := 0.0001;
    E.oLG               := 0.00;
    E.oLD               := 0.00;
    E.oHZ               := 0.00;
    E.oHN               := 0.00;

    E.PosExtr0           := PositionPointZero;    // valeurs calculées: centerline
    E.PosStation         := PositionPointZero;
    E.PosOPG             := PositionPointZero;   // valeurs calculées: silhouette
    E.PosOPD             := PositionPointZero;
    E.PosPG              := PositionPointZero;
    E.PosPD              := PositionPointZero;
    // champs pour le même usage que l'attribut Tag des TForm.
    E.TagInteger         := 0;
    E.TagDouble          := 0.00;
    // Mise en évidence de certains éléments (hors MétaFiltre)
    E.Highlighted        := false;
    // champs texte => en fin de ligne
    E.oCommentaires      := '';

    self.AddEntiteVisee(E);
    Result := True;
  finally
  end;
end;

procedure TBDDEntites.SetColorZMiniZMaxi(const ColorZMini, ColorZMaxi: TColor);
begin
  FCouleurZMin := ColorZMini;
  FCouleurZMax := ColorZMaxi;
end;
function TBDDEntites.GetColorZMini(): TColor;
begin
  Result := FCouleurZMin;
end;
function TBDDEntites.GetColorZMaxi(): TColor;
begin
  Result := FCouleurZMax;
end;

function TBDDEntites.GetPalette256(): TPalette256;
begin
  Result := FPalette256;
end;

function TBDDEntites.GetLongueurDuReseau(): double;
begin
  result := FLongueurDuReseau;
end;



procedure TBDDEntites.SetProcDisplayProgression(const ProcProgression: TProcDisplayProgression);
begin
  FProcDispProgression := ProcProgression;
end;

procedure TBDDEntites.ResetTables();
begin
  ViderLesViseesEtAntennes();
  ViderTablesSimples();
end;

procedure TBDDEntites.AfficherErreurSVG();
var
  SvgError: String;
begin
  SvgError := FSVGCanvas.GetLastError();
  AfficherMessageErreur('Erreur SVG: ' + SvgError);
end;

procedure TBDDEntites.ViderTablesSimples();
begin
  try
    SetLength(FListeDesDates, 0);
    FNbDates := 0;
    ViderLesViseesEtAntennes();
    FTableEntrances.ClearListe();
    FTableReseaux.ClearListe();
    FTableSecteurs.ClearListe();
    FTableCodes.ClearListe();
    FTableExpes.ClearListe();
    FTableJonctions.ClearListe();

    AfficherMessage(Format('%s.Vidage des tables simples OK', [Classname]));
  except
    AfficherMessage('Echec purge des tables simples');
  end;
end;

procedure TBDDEntites.ViderLesViseesEtAntennes();
begin
  FLongueurDuReseau := 0.00;
  FTableDesVisees.ClearListe();
  FTableDesAntennes.ClearListe();
end;

procedure TBDDEntites.Finaliser();
begin
  AfficherMessage(Format('%s.Finaliser', [ClassName]));
  try

    ResetTables();
    FPalette256.Finaliser();
    FListeDesDates := nil;
  finally
    FreeAndNil(FPalette256);
    FreeAndNil(FTableDesVisees);
    FreeAndNil(FTableDesAntennes);
    FreeAndNil(FTableEntrances);
    FreeAndNil(FTableReseaux);
    FreeAndNil(FTableSecteurs);
    FreeAndNil(FTableExpes);
    FreeAndNil(FTableCodes);
    FreeAndNil(FTableJonctions);
  end;
end;

function TBDDEntites.GetCodeEPSG(): integer;
begin
  Result := FCodeEPSG;
end;

procedure TBDDEntites.SetCodeEPSG(const C: integer);
begin
  FCodeEPSG := C;
end;

// ajout, modif, suppression, lecture d'éléments
// les visées
procedure TBDDEntites.AddEntiteVisee(const E: TBaseStation);
begin
  FTableDesVisees.AddElement(E);
end;

function TBDDEntites.GetEntiteVisee(const Idx: integer): TBaseStation;
begin
  Result := FTableDesVisees.GetElement(Idx);
end;

procedure TBDDEntites.PutEntiteVisee(const Idx: integer; const E: TBaseStation);
begin
  FTableDesVisees.PutElement(Idx, E);
end;

procedure TBDDEntites.SetEntiteViseeVisible(const Idx: integer; const QVisible: boolean);
var
  WU: TBaseStation;
begin
  WU := GetEntiteVisee(Idx);
  WU.Enabled := QVisible;
  PutEntiteVisee(Idx, WU);
end;

function TBDDEntites.GetNbEntitesVisees(): integer;
begin
  Result := FTableDesVisees.GetNbElements();
end;

// les antennes
// ajout, modif, suppression, lecture d'éléments
procedure TBDDEntites.AddEntiteAntenne(const E: TBaseStation);
begin
  FTableDesAntennes.AddElement(E);
end;

function TBDDEntites.GetEntiteAntenne(const Idx: integer): TBaseStation;
begin
  try
    Result := FTableDesAntennes.GetElement(Idx);
  except
    Result := FTableDesAntennes.GetElement(0);
  end;
end;

procedure TBDDEntites.PutEntiteAntenne(const Idx: integer; const E: TBaseStation);
begin
  // fonctionnement erratique (ne fonctionne pas en multithread)
  FTableDesAntennes.PutElement(Idx, E);
end;



function TBDDEntites.GetNbEntitesAntennes(): integer;
begin
  Result := FTabledesAntennes.GetNbElements();
end;
//*************************************
// les entrées
procedure TBDDEntites.ClearTableEntrances();
begin
  FTableEntrances.ClearListe();
end;

procedure TBDDEntites.AddAEntrance(const AEntree: TEntrance);
begin
  FTableEntrances.AddElement(AEntree);
end;

function TBDDEntites.GetEntrance(const NumEntree: integer): TEntrance;
begin
  Result := FTableEntrances.GetElement(NumEntree);
end;

function TBDDEntites.GetNbEntrances(): integer;
begin
  Result := FTableEntrances.GetNbElements;
end;
// réseaux
procedure TBDDEntites.ClearTableReseaux();
begin
  FTableReseaux.ClearListe();
end;

procedure TBDDEntites.AddReseau(const AReseau: TReseau);
begin
  FTableReseaux.AddElement(AReseau);
end;

function TBDDEntites.GetReseau(const NumReseau: integer): TReseau;
begin
  try
    Result := FTableReseaux.GetElement(NumReseau);
  except
    Result := FTableReseaux.GetElement(0);
  end;
end;


function TBDDEntites.GetNbReseaux(): integer;
begin
  Result := FTableReseaux.GetNbElements;
end;
// secteurs
procedure TBDDEntites.ClearTableSecteurs();
begin
  FTableSecteurs.ClearListe();
end;

procedure TBDDEntites.AddSecteur(const ASecteur: TSecteur);
begin
  FTableSecteurs.AddElement(ASecteur);
end;

function TBDDEntites.GetSecteur(const NumSecteur: integer): TSecteur;
begin
  try
    Result := FTableSecteurs.GetElement(NumSecteur);
  except
    Result := FTableSecteurs.GetElement(0);
  end;
end;


function TBDDEntites.GetNbSecteurs(): integer;
begin
  Result := FTableSecteurs.GetNbElements;
end;
// expés
procedure TBDDEntites.ClearTableExpes();
begin
  FTableExpes.ClearListe();
end;

procedure TBDDEntites.AddExpe(const LaExpe: TExpe);
begin
  FTableExpes.AddElement(LaExpe);
end;

function TBDDEntites.GetExpe(const NumExpe: integer): TExpe;
begin
  Result := FTableExpes.GetElement(NumExpe);
end;

function TBDDEntites.GetExpeByIndex(const Idx: TNumeroExpe): TExpe;
begin
  Result := FTableExpes.GetElementByIndex(Idx);
end;

function TBDDEntites.GetNbExpes(): integer;
begin
  Result := FTableExpes.GetNbElements;
end;
//codes
procedure TBDDEntites.ClearTableCodes();
begin
  FTableCodes.ClearListe();
end;


procedure TBDDEntites.AddCode(const LeCode: TCode);
begin
  FTableCodes.AddElement(LeCode);
end;

function TBDDEntites.GetCode(const NoCode: integer): TCode;
begin
  Result := FTableCodes.GetElement(NoCode);
end;

function TBDDEntites.GetCodeByIndex(const Idx: TNumeroCode): TCode;
begin
  Result := FTableCodes.GetElementByIndex(Idx);
end;

function TBDDEntites.GetNbCodes(): integer;
begin
  Result := FTableCodes.GetNbElements;
end;
// liste des jonctions
procedure TBDDEntites.AddJonction(const J: TJonctionXYZ);
begin
  FTableJonctions.AddElement(J);

end;

function TBDDEntites.GetJonction(const N: integer): TJonctionXYZ;
begin
  try
    Result := FTableJonctions.GetElement(N);
  except
    Result := FTableJonctions.GetElement(0);
  end;
end;

function TBDDEntites.GetNbJonctions(): integer;
begin
  Result := FTableJonctions.GetNbElements;
end;

function TBDDEntites.FindJonctionBySerieSt(const Ser: TNumeroSerie; const St: TNumeroStation; out QJonction: TJonctionXYZ): boolean;
var
  i, Nb: Integer;
  function FindDepth(const I1, I2: integer; const QIDX: TIDBaseStation):  integer;
  var
    PVT: integer;
    C1: TJonctionXYZ;
    EWE: TIDBaseStation;
  begin
    PVT := (I2+I1) div 2;                                           // coupure en deux => calcul index médian
    if (I1 > I2) then exit(-1);                                     // début > fin >> sortie directe avec erreur
    C1 := GetJonction(PVT);
    EWE :=  QMakeTIDStation(C1.NoSer , C1.NoSt);
    if (QIDX = EWE) then exit(PVT);                         // comparaison. Si vrai >> sortie avec numéro d'index
    if (QIDX < EWE) then exit(FindDepth(I1, PVT-1, QIDX));  // sinon, recherche en profondeur avec un niveau supplémentaire
    Result := FindDepth(PVT+1, I2, QIDX);
  end;
begin
  Result := false;
  i := FindDepth(0, GetNbJonctions() - 1, QMakeTIDStation(Ser, St));
  if (i >= 0) then
  begin
    QJonction     := GetJonction(i);
    exit(True);
  end;
  //*)

  (* version naïve par scan
  result := false;
  Nb := GetNbJonctions();
  for i := 0 to Nb -1 do
  begin
    QJonction := GetJonction(i);
    if ((Ser = QJonction.NoSer) and (St = QJonction.NoSt)) then exit(True);
  end;
  //*)

end;

procedure TBDDEntites.SortJonctions();
begin
  FTableJonctions.Sort(SortJonctionBySerSt);
end;
//*)
procedure TBDDEntites.ExporterListeJonctions(const ToClipboard: boolean; const QFilename: TStringDirectoryFilename);
var
  LS: TStringList;
  CT: TClipboard;
  Nb, i: Integer;
  EWE: String;
  MyJonction: TJonctionXYZ;

begin
  Nb := GetNbJonctions();
  AfficherMessage(format('%s.ExporterListeJonctions(%s): %d nodes', [ClassName, QFilename, Nb]));
  if (Nb <= 1) then Exit;
  LS := TStringList.Create;
  try
    LS.Clear;
    EWE := 'ID' + #9 + 'Station' + #9 + 'X' + #9 + 'Y' + #9 + 'Z' + #9 + '' + #9 + 'Lat' + #9 + 'Lon';
    LS.Add(EWE);
    for i := 1 to Nb - 1 do
    begin
      MyJonction := GetJonction(i);
      EWE := Format(FMTSERST + #9 + FORMAT_STRING + #9 + FORMAT_STRING + #9 + FORMAT_STRING,
                    [MyJonction.NoSer, MyJonction.NoSt,
                     FormatterNombreOOo(MyJonction.Position.X, 3, true),
                     FormatterNombreOOo(MyJonction.Position.Y, 3, true),
                     FormatterNombreOOo(MyJonction.Position.Z, 3, true)
                    ]);
      LS.Add(EWE);
    end;
    if (ToClipBoard) then
    begin
      CT := TClipboard.Create(ctClipBoard);
      try
        CT.AsText := LS.Text;
      finally
        FreeandNil(CT);
      end;
    end
    else
    begin
      LS.SaveToFile(QFileName);
    end;
    LS.Clear;
  finally
    FreeandNil(LS);
  end;
end;


//******************************************************************************
function TBDDEntites.GetCouleurEntiteByReseau(const E: TBaseStation): TGHTopoColor;
var
  EWE: TReseau;
begin
  EWE  := self.GetReseau(E.eReseau);
  Result := EWE.ColorReseau;
end;

function TBDDEntites.GetCouleurEntiteByIdxReseau(const Idx: integer): TGHTopoColor;
var
  EWE: TReseau;
begin
  EWE := self.GetReseau(Idx);
  Result := EWE.ColorReseau;
end;

function TBDDEntites.GetCouleurEntiteByEntrance(const E: TBaseStation): TGHTopoColor;
var
  EWE: TEntrance;
begin
  EWE    := self.GetEntrance(E.eEntrance);
  result := EWE.eCouleur;
end;

function TBDDEntites.GetCouleurEntiteByIdxEntrance(const Idx: integer): TGHTopoColor;
var
  EWE: TEntrance;
begin
  EWE    := self.GetEntrance(Idx);
  Result := EWE.eCouleur;
end;


function TBDDEntites.GetCouleurEntiteBySecteur(const E: TBaseStation): TGHTopoColor;
var
  EWE: TSecteur;
begin
  EWE := self.GetSecteur(E.eSecteur);
  Result := EWE.CouleurSecteur;
end;


function TBDDEntites.GetCouleurEntiteByIdxSecteur(const Idx: integer): TGHTopoColor;
var
  EWE: TSecteur;
begin
  EWE     := self.GetSecteur(Idx);
  Result  := EWE.CouleurSecteur;
end;

function TBDDEntites.GetCouleurEntiteByExpe(const E: TBaseStation): TGHTopoColor;
var
  EWE: TExpe;
begin
  EWE := self.GetExpeByIndex(E.eExpe);
  Result.setFrom(FPalette256.GetColorByIndex(EWE.IdxCouleur), 255);
end;
function TBDDEntites.GetCouleurEntiteByIdxExpe(const IDX: TNumeroExpe): TGHTopoColor;
var
  EWE: TExpe;
  WU : TColor;
begin
  EWE := self.GetExpeByIndex(IDX);
  Result.setFrom(FPalette256.GetColorByIndex(EWE.IdxCouleur), 255);
end;

//******************************************************************************
// Encombrement du réseau
// définir mini et maxi
// font double emploi avec les membres privés FCoinBasGauche et FCoinHautDroit
procedure TBDDEntites.SetMinMax(const DoMetafilteredOnly: boolean);
const
  GM = 1e20;
var
  i, NbEntrances: integer;
  PM3: TPoint3Df;
  XMini, XMaxi, YMini, YMaxi, ZMini, ZMaxi: double;
  EWE: TBaseStation;
  MyEntrance: TEntrance;
  function QIsValidCoords(const QP: TPoint3Df): boolean;
  const
    ECRETAGE = 1E7;
  begin
    Result := InRange(QP.X, -ECRETAGE, ECRETAGE) and
              InRange(QP.Y, -ECRETAGE, ECRETAGE) and
              InRange(QP.Z, -ECRETAGE, ECRETAGE);
  end;
  procedure MiouMiou(const Miou: TBaseStation);
  begin
    if (Not (QIsValidCoords(Miou.PosExtr0) AND
             QIsValidCoords(Miou.PosStation))) then Exit;
    if (Miou.PosExtr0.X > XMaxi) then
    begin
      XMaxi := Miou.PosExtr0.X;
      if (Miou.Enabled) then FMetafilteredCoinHautDroit.X := XMaxi;
      FIDStationXMaxi := Miou.toStringWithIDTerrain();
    end;
    if (Miou.PosExtr0.X < XMini) then
    begin
      XMini := Miou.PosExtr0.X;
      if (Miou.Enabled) then FMetafilteredCoinBasGauche.X := XMini;
      FIDStationXMini := Miou.toStringWithIDTerrain();
    end;

    if (Miou.PosExtr0.Y > YMaxi) then
    begin
      YMaxi := Miou.PosExtr0.Y;
      if (Miou.Enabled) then FMetafilteredCoinHautDroit.Y := YMaxi;
      FIDStationYMaxi := Miou.toStringWithIDTerrain();
    end;
    if (Miou.PosExtr0.Y < YMini) then
    begin
      YMini := Miou.PosExtr0.Y;
      if (Miou.Enabled) then FMetafilteredCoinBasGauche.Y := YMini;
      FIDStationYMini := Miou.toStringWithIDTerrain();
    end;
    if (Miou.PosExtr0.Z > ZMaxi) then
    begin
      ZMaxi := Miou.PosExtr0.Z;
      if (Miou.Enabled) then FMetafilteredCoinHautDroit.Z := ZMaxi;
      FIDStationZMaxi := Miou.toStringWithIDTerrain();
    end;
    if (Miou.PosExtr0.Z < ZMini) then
    begin
      ZMini := Miou.PosExtr0.Z;
      if (Miou.Enabled) then FMetafilteredCoinBasGauche.Z := ZMini;
      FIDStationZMini := Miou.toStringWithIDTerrain();
    end;
    //*)
    // Station
    if (Miou.PosStation.X > XMaxi) then
    begin
      XMaxi := Miou.PosStation.X;
      if (Miou.Enabled) then FMetafilteredCoinHautDroit.X := XMaxi;
      FIDStationXMaxi := Miou.toStringWithIDTerrain();
    end;
    if (Miou.PosStation.X < XMini) then
    begin
      XMini := Miou.PosStation.X;
      if (Miou.Enabled) then FMetafilteredCoinBasGauche.X := XMini;
      FIDStationXMini := Miou.toStringWithIDTerrain();
    end;

    if (Miou.PosStation.Y > YMaxi) then
    begin
      YMaxi := Miou.PosStation.Y;
      if (Miou.Enabled) then FMetafilteredCoinHautDroit.Y := YMaxi;
      FIDStationYMaxi := Miou.toStringWithIDTerrain();
    end;
    if (Miou.PosStation.Y < YMini) then
    begin
      YMini := Miou.PosStation.Y;
      if (Miou.Enabled) then FMetafilteredCoinBasGauche.Y := YMini;
      FIDStationYMini := Miou.toStringWithIDTerrain();
    end;

    if (Miou.PosStation.Z > ZMaxi) then
    begin
      ZMaxi := Miou.PosStation.Z;
      if (Miou.Enabled) then FMetafilteredCoinHautDroit.Z := ZMaxi;
      FIDStationZMaxi := Miou.toStringWithIDTerrain();
    end;
    if (Miou.PosStation.Z < ZMini) then
    begin
      ZMini := Miou.PosStation.Z;
      if (Miou.Enabled) then FMetafilteredCoinBasGauche.Z := ZMini;
      FIDStationZMini := Miou.toStringWithIDTerrain();
    end;
  end;
begin
  AfficherMessage(ClassName + '.SetMinMax()');
  with self do
  begin
    XMini :=  GM;
    YMini :=  GM;
    ZMini :=  GM;
    XMaxi := -GM;
    YMaxi := -GM;
    ZMaxi := -GM;
    if (DoMetafilteredOnly) then
    begin
      for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
      begin
        EWE := GetEntiteVisee(i);
        if (EWE.Enabled) then MiouMiou(EWE);
      end;
    end
    else
    begin
      for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
      begin
        EWE := GetEntiteVisee(i);
        MiouMiou(EWE);
      end;
      // TODO: Les entrées
      NbEntrances := self.GetNbEntrances();

      if (NbEntrances > 0) then
      begin
        for i := 1 to NbEntrances - 1 do
        begin
          MyEntrance := self.GetEntrance(i);
          if (not QIsValidCoords(MyEntrance.ePosition)) then continue;
          XMini := Min(XMini, MyEntrance.ePosition.X);
          YMini := Min(YMini, MyEntrance.ePosition.Y);
          ZMini := Min(ZMini, MyEntrance.ePosition.Z);
          XMaxi := Max(XMaxi, MyEntrance.ePosition.X);
          YMaxi := Max(YMaxi, MyEntrance.ePosition.Y);
          ZMaxi := Max(ZMaxi, MyEntrance.ePosition.Z);

        end;
        FMetafilteredCoinBasGauche.setFrom(XMini, YMini, ZMini);
        FMetafilteredCoinHautDroit.setFrom(XMaxi, YMaxi, ZMaxi);
      end;
    end;

    // affectation des membres privés suivants
    FCoinBasGauche.setFrom(XMini, YMini, ZMini);
    FCoinHautDroit.setFrom(XMaxi, YMaxi, ZMaxi);
    AfficherMessage(Format('-- Reseau complet : Mini: %.0f, %.0f, %.0f', [FCoinBasGauche.X, FCoinBasGauche.Y, FCoinBasGauche.Z]));
    AfficherMessage(Format('-- Reseau complet : Maxi: %.0f, %.0f, %.0f', [FCoinHautDroit.X, FCoinHautDroit.Y, FCoinHautDroit.Z]));
    AfficherMessage(Format('-- Zone filtree   : Mini: %.0f, %.0f, %.0f', [FMetafilteredCoinBasGauche.X, FMetafilteredCoinBasGauche.Y, FMetafilteredCoinBasGauche.Z]));
    AfficherMessage(Format('-- Zone filtree   : Maxi: %.0f, %.0f, %.0f', [FMetafilteredCoinHautDroit.X, FMetafilteredCoinHautDroit.Y, FMetafilteredCoinHautDroit.Z]));
  end;
end;

// Coordonnees extremes
function TBDDEntites.GetCoinBasGauche(): TPoint3Df;
begin
  Result := FCoinBasGauche;
end;

function TBDDEntites.GetCoinHautDroit(): TPoint3Df;
begin
  Result := FCoinHautDroit;
end;

function TBDDEntites.GetMetafilteredCoinBasGauche(): TPoint3Df;
begin
  Result := FMetafilteredCoinBasGauche;
end;

function TBDDEntites.GetMetafilteredCoinHautDroit(): TPoint3Df;
begin
  Result := FMetafilteredCoinHautDroit;
end;

// ID des stations extremes
function TBDDEntites.GetIDStationXMini(): string;
begin
  Result := FIDStationXMini;
end;

function TBDDEntites.GetIDStationYMini(): string;
begin
  Result := FIDStationYMini;
end;
function TBDDEntites.GetIDStationZMini(): string;
begin
  Result := FIDStationZMini;
end;

function TBDDEntites.GetRecapitulation(): TVentilationSpeleometrie;
begin
  Result := FRecapitulation;
end;

function TBDDEntites.GetIDStationXMaxi(): string;
begin
  Result := FIDStationXMaxi;
end;
function TBDDEntites.GetIDStationYMaxi(): string;
begin
  Result := FIDStationYMaxi;
end;
function TBDDEntites.GetIDStationZMaxi(): string;
begin
  Result := FIDStationZMaxi;
end;

//******************************************************************************

procedure TBDDEntites.SortByDepth();
begin
  FTableDesVisees.Sort(SortEntiteZOrder);
end;
procedure TBDDEntites.SortBySerSts();
begin
  FTableDesVisees.Sort(SortEntiteSerSt);
end;
procedure TBDDEntites.SortByTagDouble();
begin
  FTableDesVisees.Sort(SortEntiteByTagDouble);
end;

/// fonctions de recherche
// recherche d'une entité près d'un point (x, y)
// Retourne: VRAI si entité trouvée, FAUX sinon
// Transmet: Entité trouvée
//           Index interne de l'entité
function TBDDEntites.GetStationOrEntranceFromXYZ(const QX, QY, QZ: double;
                                                 const RayonMaxiCapture: double;
                                                 const TablesAParcourir: TBDDEntitesFindTablesAParcourir;
                                                 const IgnoreZ: boolean;
                                                 out   InternalIdx: integer;
                                                 out   BP: TBaseStation;
                                                 out   EN: TEntrance;
                                                 out   Distance: double;
                                                 out   ER: TBDDEntitesFindViseeOrEntrance) : boolean;
var
  dMax: double;
  i, Nb: Integer;
  QBP: TBaseStation;
  d: Double;
  QEN: TEntrance;
begin
  //{$ERROR: Vérifier ceci: ne semble pas retourner correctement}
  Result := false;
  if (tpENTRANCES in TablesAParcourir) then
  begin
    InternalIdx := -1;
    dMax := INFINI;
    ER := tpENTRANCES;
    Nb := GetNbEntrances();
    for i := 0 to Nb - 1  do
    begin
      QEN := GetEntrance(i);
      if (IgnoreZ) then d := Hypot2D(QEN.ePosition.X - QX, QEN.ePosition.Y - QY)
                   else d := Hypot3D(QEN.ePosition.X - QX, QEN.ePosition.Y - QY, QEN.ePosition.Z - QZ);
      if (d <= RayonMaxiCapture) then
      begin
        if (d < dMax) then
        begin
          dMax := d;
          EN := QEN;
          InternalIdx   := i;
          Distance      := dMax;
          Result        := True;
        end;
      end;
    end;
    //AfficherMessageErreur(Format('GetStationOrEntranceFromXYZ: %d', [InternalIdx]));
    if (Result) then exit;
  end;
  if (tpVISEES in TablesAParcourir) then
  begin
    InternalIdx := -1;
    dMax := INFINI;
    ER := tpVISEES;
    Nb := self.GetNbEntitesVisees();
    for i := LOW_INDEX to Nb - 1 do
    begin
      QBP := self.GetEntiteVisee(i);
      // on zappe les antennes
      if (QBP.Type_Entite = tgVISEE_RADIANTE) then Continue;
      // on zappe les entitées métafiltrées
      if (not QBP.Enabled) then Continue;
       d := Hypot2D(QBP.PosStation.X - QX, QBP.PosStation.Y - QY);
      //if (IgnoreZ) then d := Hypot2D(QBP.PosStation.X - X, QBP.PosStation.Y - Y)
      //             else d := Hypot3D(QBP.PosStation.X - X, QBP.PosStation.Y - Y, QBP.PosStation.Z - Z);
      //if (d <= RayonMaxiCapture) then
      begin
        if (d < dMax) then
        begin
          dMax := d;
          BP := QBP;
          InternalIdx   := i;
          Distance      := dMax;
          Result        := True;
        end;
      end;
      //AfficherMessageErreur(Format('GetStationOrEntranceFromXYZ: %d', [InternalIdx]));
    end;
    if (Result) then exit;
  end;

end;


function TBDDEntites.GetEntiteViseeFromCle(const DoMatchExact: boolean; const Cle: string; out EE: TBaseStation): boolean;
var
  WU: String;
  i, Nb: Integer;
  S: String;
  QCond: Boolean;
  Entr: TEntrance;
begin
  AfficherMessageErreur('GetEntiteViseeFromCle: ' + Cle);
  Result := false;
  WU := UpperCase(Trim(Cle));
  // d'abord les entrées
  Nb := GetNbEntrances();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      Entr := GetEntrance(i);
      if (DoMatchExact) then
      begin
        S := UpperCase(Trim(Entr.eIDTerrain));
        QCond := (S = WU);
      end
      else
      begin
        S  := UpperCase(Trim(Entr.eIDTerrain) + '|' + Entr.eNomEntree + '|' + Trim(Entr.eObserv));
        QCond := (Pos(WU, S) > 0);
      end;

      if (QCond) then
      begin
        EE.Type_Entite     := tgENTRANCE;
        EE.Entite_Serie    := Entr.eRefSer;
        EE.Entite_Station  := Entr.eRefSt;
        EE.IDTerrain       := Format('%s - %d.%d - Entree: %s', [Entr.eIDTerrain, Entr.eRefSer, Entr.eRefSt, Entr.eNomEntree]);
        EE.oCommentaires   := Entr.eObserv;
        EE.PosStation      := Entr.ePosition;
        Result := True;
        Exit;
      end;
    end;
  end;

  // enuite, la table des entités
  for i := LOW_INDEX to self.GetNbEntitesVisees() - 1 do
  begin
    EE := GetEntiteVisee(i);
    if (EE.Type_Entite = tgVISEE_RADIANTE) then Continue;
    if (DoMatchExact) then
    begin
      S := UpperCase(Trim(EE.IDTerrain));
      QCond := (S = WU);
    end
    else
    begin
      S  := UpperCase(Trim(EE.IDTerrain) + '|' + Trim(EE.oCommentaires));
      QCond := (Pos(WU, S) > 0);
    end;
    if (QCond) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;
// Cette fonction est coûteuse: on vérifie si le précédent appel de GetEntiteFromSerSt()
// a été effectué avec le même couple (Ser, St)
// Si oui, on retourne le résultat de cette recherche, préalablement sauvegardé
// Si non, on effectue la recherche et on sauvegarde son résultat dans FLastEntiteFound
// Comme les entités ne sont pas forcément triées selon leur ID,
// seul un balayage est possible
// Sans optimisation  : Temps de calcul des 82918 antennes: 00:00:24.273 (Langoiran_20191120.xtb)
// Avec optimisation  : Temps de calcul des 82918 antennes: 00:00:01.856
function TBDDEntites.GetEntiteViseeFromSerSt(const Ser: TNumeroSerie; const St: TNumeroStation; out EE: TBaseStation): boolean;
var
  i, Nb: Integer;
  Entr: TEntrance;
  // recherche par méthode récursive dichotomique
  function FindDepth(const I1, I2: integer; const QIDX: TIDBaseStation): integer;
  var
    PVT: Int64;
    C1: TBaseStation;
    QIDStation: TIDBaseStation;
  begin
    // coupure en deux => calcul index médian
    PVT := (I2+I1) div 2;
    // début > fin >> sortie directe avec erreur
    if (I1 > I2) then Exit(-1);
    C1 := GetEntiteVisee(PVT); //GetBasePoint(PVT);
    // comparaison. Si vrai >> sortie avec numéro d'index
    QIDStation := QMakeTIDStation(C1.Entite_Serie, C1.Entite_Station);
    if (QIDStation = QIdx) then Exit(PVT);
    // sinon, recherche en profondeur avec un niveau supplémentaire
    if (QIDX < QIDStation) then
    begin
      Result := FindDepth(I1, PVT-1, QIDX);
      Exit;
    end;
    Result := FindDepth(PVT+1, I2, QIDX);
  end;
begin
  Result := false;
  if ((Ser = FLastEntiteFound.Entite_Serie) and (St = FLastEntiteFound.Entite_Station)) then
  begin
    EE := FLastEntiteFound;
    exit(true);
  end;
  // Recherche dichotomique
  Nb := self.GetNbEntitesVisees();
  i := FindDepth(LOW_INDEX, Nb - 1, QMakeTIDStation(Ser, St));
  if (i >= 0) then
  begin
    EE     := GetEntiteVisee(i);
    Exit(True);
  end;
  // si échec, on balaye la table
  for i := LOW_INDEX to Nb - 1 do
  begin
    EE := self.GetEntiteVisee(i);
    if (EE.Type_Entite  = tgENTRANCE)       then Continue;
    if (EE.Type_Entite  = tgFIXPOINT)       then Continue;
    if ((EE.Entite_Serie = Ser) and (EE.Entite_Station = St)) then
    begin
      FLastEntiteFound := EE;
      Exit(True);
    end;
  end;

  // Les entrées en 2e intention (sinon: bug graphique)
  (*
  Nb := self.GetNbEntrances();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      Entr := GetEntrance(i);
      if ((Entr.eRefSer = Ser) and (Entr.eRefSt = St)) then
      begin
        EE.Type_Entite     := tgENTRANCE;
        EE.Entite_Serie    := Ser;
        EE.Entite_Station  := St;
        EE.IDTerrain       := Entr.eIDTerrain + ' (Entree)';
        EE.oCommentaires   := Entr.eObserv;
        EE.PosStation.X    := Entr.eXEntree;
        EE.PosStation.Y    := Entr.eYEntree;
        EE.PosStation.Z    := Entr.eZEntree;
        FLastEntiteFound := EE;
        Exit(True);
      end;
    end;
  end;
  //*)
end;
// recherche d'une entité à partir d'une clé
// Si la clé est de la forme 101.33, c'est à priori un couple série-station; on utilise GetEntiteFromSerSt()
// Si échec de cette première recherche, on utilise GetEntiteFromCle()
// Retourne: VRAI si entité trouvée, FAUX sinon
// Transmet: Entité trouvée
function TBDDEntites.FindStationByCle(const DoMatchExact: boolean; const Cle: string; out E: TBaseStation): Boolean;
var
  S: String;
  WU: Integer;
  S1, S2: String;
  N: Integer;
  qSer, qSt: Integer;
begin
  Result := False;
  S := UpperCase(Trim(Cle));
  AfficherMessage(Format('%s.FindStationByCle(%s)', [self.ClassName, Cle]));
  // analyse de la clé
  // la clé contient un point --> recherche par série/station
  WU := Pos('.', S);
  if (WU > 0) then
  begin
    AfficherMessage('--> Recherche directe station.');
    N := Length(S);
    S1 := Trim(Copy(S, 0, WU - 1));
    S2 := Trim(Copy(S, WU + 1, N));
    qSer := StrToIntDef(S1, 0);
    qSt  := StrToIntDef(S2, 0);
    if (GetEntiteViseeFromSerSt(qSer, qSt, E)) then
    begin
      Result := True;
      Exit;
    end;
  end;
  // station non trouvee ? Tentative de Recherche par clé
  Result := GetEntiteViseeFromCle(DoMatchExact, S, E);
end;
// recherche d'une entrée par clé ou couple série/pt.
// Si DoMatchExact est armé, la recherche ne se fait que sur eIDTerrain
function TBDDEntites.FindEntranceByCle(const DoMatchExact: boolean; const Cle: string; out E: TEntrance): boolean;
var
  i, Nb: Integer;
  QKey, QAT: String;
  QCond: Boolean;
begin
  Result := false;
  QKey := LowerCase(trim(Cle));
  Nb := self.GetNbEntrances();
  if (Nb = 0) then exit;
  for i := 0 to Nb - 1 do
  begin
    E := GetEntrance(i);
    if (DoMatchExact) then
    begin
      QAT := LowerCase(Trim(E.eIDTerrain)); // seul le champ eIDTerrain est évalué
      QCond := (QKey = QAT);
    end
    else
    begin
      QAT := Lowercase(Trim(E.eIDTerrain + '|' + trim(E.eNomEntree) + '|' + trim(E.eObserv)));
      QCond := (Pos(QKey, QAT) > 0);
    end;
    if (QCond) then Exit(True);
  end;
end;


//******************************************************************************
procedure TBDDEntites.CalcLengthReseau();
var
  i: Integer;
  EWE: TBaseStation;
  dy, dx, dz: Double;
begin
  FLongueurDuReseau := 0.00;
  for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
  begin
    EWE := GetEntiteVisee(i);
    if (EWE.Type_Entite in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON, tgSURFACE, tgTUNNEL, tgMINE]) then
    begin
      FLongueurDuReseau += Hypot3D(dx, dy, dz);
      dx := EWE.PosStation.X - EWE.PosExtr0.X;
      dy := EWE.PosStation.Y - EWE.PosExtr0.Y;
      dz := EWE.PosStation.Z - EWE.PosExtr0.Z;
    end;
  end;
end;

// Statistiques
// calculs statistiques
// semble OK
procedure TBDDEntites.CalculSpeleometrie();
var
  i,j : integer;
  EWE: TBaseStation;
  WU: TExpe;
  X69: TDateTime;
  QAT: TCode;
  function  GetEmptyVentilation(): TVentilationSpeleometrie;
  begin
    with Result do
    begin
      Fossiles   := 0.00;
      Vadoses    := 0.00;
      Ennoyables := 0.00;
      Siphons    := 0.00;
      Tunnels    := 0.00;
      Filons     := 0.00;
      Speciaux   := 0.00;
    end;
  end;
  function GetLigneRecapitulative(const QV: TVentilationSpeleometrie; const EE: TBaseStation): TVentilationSpeleometrie;
  var
    dv: Double;
  begin
    Result := QV;
    dv := Hypot3D(EE.PosStation.X - EE.PosExtr0.X,
                  EE.PosStation.Y - EE.PosExtr0.Y,
                  EE.PosStation.Z - EE.PosExtr0.Z);
    case EE.Type_Entite of
      tgENTRANCE      : pass; // entrées
      tgVISEE_RADIANTE: pass; // visées en antenne
      tgDEFAULT,
      tgFOSSILE    : Result.Fossiles    := Result.Fossiles   + dv;
      tgVADOSE     : Result.Vadoses     := Result.Vadoses    + dv;
      tgENNOYABLE  : Result.Ennoyables  := Result.Ennoyables + dv;
      tgSIPHON     : Result.Siphons     := Result.Siphons    + dv;
      tgFIXPOINT   : pass; // points fixes
      tgSURFACE    : Result.Speciaux    := Result.Speciaux   + dv;
      tgTUNNEL     : Result.Filons      := Result.Filons     + dv;
      tgMINE       : Result.Tunnels     := Result.Tunnels    + dv;
    else
                     Result.Fossiles    := Result.Fossiles   + dv;
    end;
  end;
  procedure  AjouterLongueurVisee(var QTableauRecap: TTableauVentilationSpeleometrie; const QE: TBaseStation; const QIdx: integer);
  begin
    QTableauRecap[QIdx] := GetLigneRecapitulative(QTableauRecap[QIdx], QE);
  end;
  procedure PurgerTableau(var QTableauRecap: TTableauVentilationSpeleometrie; const QNb: integer);
  var
    n: Integer;
  begin
    SetLength(QTableauRecap, QNb);
    for n := 0 to QNb - 1 do QTableauRecap[n] := GetEmptyVentilation();
  end;
begin
  AfficherMessage('TdlgSpeleometrie.CalculSpeleometrie()');
  AfficherMessageErreur('TdlgSpeleometrie.CalculSpeleometrie()');
  AfficherMessage('');
  // init tableaux
  PurgerTableau(FSpeleometrieParReseaux   , GetNbReseaux());
  PurgerTableau(FSpeleometrieParSecteurs  , GetNbSecteurs());
  PurgerTableau(FSpeleometrieParExpes     , GetNbExpes());
  PurgerTableau(FSpeleometrieParCodes     , GetNbCodes());
  PurgerTableau(FSpeleometrieParDates     , GetNbDates());
  AfficherMessageErreur(format('  Speleometrie par expes (%d expes)',[GetNbExpes()]));
  //-----------------------
  try
    for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      EWE := GetEntiteVisee(i);
      for j := 0 to GetNbExpes() - 1 do
      begin
        WU := GetExpe(j);
        if (EWE.eExpe = WU.IDExpe) then AjouterLongueurVisee(FSpeleometrieParExpes, EWE, j);
      end;
    end;
    AfficherMessageErreur(' ---> Expes OK');
  except
    AfficherMessageErreur(' ---> Expes KO');
  end;
  AfficherMessageErreur(format('  Speleometrie par dates (%d dates)',[GetNbDates]));
  //-----------------------
  try
    for i:= LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      EWE := GetEntiteVisee(i);
      for j:=0 to GetNbDates() - 1 do
      begin
        // troncature des dates et comparaison
        if (Trunc(EWE.DateLeve) = Trunc(FListeDesDates[j])) then AjouterLongueurVisee(FSpeleometrieParDates, EWE, j);
      end;
    end;
    AfficherMessageErreur(' ---> Dates OK');
  except
    AfficherMessageErreur(' ---> Dates KO');
  end;


  AfficherMessageErreur(format('  Speleometrie par codes (%d codes)',[GetNbCodes]));
  //-----------------------
  try
    for i:= LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      EWE := GetEntiteVisee(i);
      for j:=0 to GetNbCodes() - 1 do
      begin
        QAT := GetCode(j);
        if (EWE.eCode = QAT.IDCode) then
          AjouterLongueurVisee(FSpeleometrieParCodes, EWE, j);
      end;
    end;
    AfficherMessageErreur(' ---> Codes OK');
  except
    AfficherMessageErreur(' ---> Codes KO');
  end;
  // calcul de la spéléométrie par réseaux
  AfficherMessageErreur(format('  Speleometrie par reseaux (%d reseaux)',[GetNbReseaux]));
  try
    for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      EWE := GetEntiteVisee(i);
      for j := 0 to GetNbReseaux() -1 do
      begin
        if (EWE.eReseau = j) then AjouterLongueurVisee(FSpeleometrieParReseaux, EWE, j);
      end;
    end;
    AfficherMessageErreur(' ---> Reseaux OK');
  except
    AfficherMessageErreur(' ---> Reseaux KO');
  end;
  // calcul de la spéléométrie par secteurs
  AfficherMessageErreur(format('  Speleometrie par secteurs (%d secteurs)',[GetNbSecteurs]));
  //SetLength(FSpeleometrieParDates,FNbSeances);
  try
    for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      EWE := GetEntiteVisee(i);
      for j := 0 to GetNbSecteurs() - 1 do
      begin
        if (EWE.eSecteur = j) then AjouterLongueurVisee(FSpeleometrieParSecteurs, EWE, j);
      end;
    end;
    AfficherMessageErreur(' ---> Secteurs OK');
  except
    AfficherMessageErreur(' ---> Secteurs OK');
  end;
  // calcul de la spéléométrie totale
  AfficherMessageErreur('Speleometrie totale');
  //SetLength(FSpeleometrieParDates,FNbSeances);
  try
    FRecapitulation := GetEmptyVentilation();
    for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      EWE := GetEntiteVisee(i);
      FRecapitulation := GetLigneRecapitulative(FRecapitulation, EWE);
    end;
    AfficherMessageErreur(' ---> Total OK');
  except
  end;
  //*)
end;
// attraper les spéléométries par index
function TBDDEntites.GetSpeleometrieParExpeByIdx(const Idx: integer): TVentilationSpeleometrie;
begin
  Result := FSpeleometrieParExpes[Idx];
end;

function TBDDEntites.GetSpeleometrieParCodeByIdx(const Idx: integer): TVentilationSpeleometrie;
begin
  Result := FSpeleometrieParCodes[Idx];
end;

function TBDDEntites.GetSpeleometrieParDateByIdx(const Idx: integer): TVentilationSpeleometrie;
begin
  Result := FSpeleometrieParDates[Idx];
end;

function TBDDEntites.GetSpeleometrieParReseauxByIdx(const Idx: integer): TVentilationSpeleometrie;
begin
  Result := FSpeleometrieParReseaux[Idx];
end;
function TBDDEntites.GetSpeleometrieParSecteurByIdx(const Idx: integer): TVentilationSpeleometrie;
begin
  Result := FSpeleometrieParSecteurs[Idx];
end;
// semble OK. A valider
// SetMinMax inutile
procedure TBDDEntites.ExportForGHCaveDraw(const MyFile: TStringDirectoryFilename; const QMyFiltre: string);
var
  F: TextFile;
  i: integer;
  QID: Int64;
  OldFilter: string;
  E: TBaseStation;
  NbVisees   : Integer;
  NbAntennes: Integer;
  QDevelViseesVisibles: double;
  procedure WrtLn(const S: string);
  begin
    WriteLn(F, S);
  end;
begin
  NbVisees        := self.GetNbEntitesVisees();
  NbAntennes      := self.GetNbEntitesAntennes();
  AfficherMessage(Format('%s.ExportForGHCaveDraw(%s):%d shots, %d radiants: Filtres: %s',[ClassName, MyFile, NbVisees, NbAntennes, QMyFiltre]));
  AssignFile(F, MyFile);
  try
    // sauvegarder le filtre
    OldFilter := QMyFiltre;
    // export vers GHCaveDraw = on envoie tout
    MetaFiltre('', QDevelViseesVisibles);
    // **********************************
    ReWrite(F);
    // en-tête
    // points de base des éléments du dessin
    WrtLn('# Base points');
    WrtLn('');
    WrtLn('basepoints');
    WrtLn(Format('# %d centerline shots', [NbVisees]));
    // les visées de cheminement
    for i := LOW_INDEX to NbVisees - 1 do
    begin
      E  := GetEntiteVisee(i);
      QID := E.getGHCaveDrawIDPtCenterline();
      WrtLn(Format(GCD_FMT_BASEPOINTS,
                  [QID  ,
                   E.IDTerrain,
                   E.Type_Entite,
                   GetCouleurEntiteByExpe(E).toTColor(), //E.ColorEntite,
                   E.PosExtr0.X, E.PosExtr0.Y, E.PosExtr0.Z,
                   E.PosStation.X, E.PosStation.Y, E.PosStation.Z,
                   E.PosPG.X, E.PosPG.Y,  E.PosOPG.Z, // Z1PB,
                   E.PosPD.X, E.PosPD.Y,  E.PosOPD.Z//E.X2PD, E.Y2PD,  E.Z1PH
                   ]));
    end;
    // les antennes
    WrtLn(Format('# %d radiant shots', [NbAntennes]));
    if (NbAntennes > 0) then
    begin
      for i := 0 to NbAntennes - 1 do
      begin
        E  := GetEntiteAntenne(i);
        // NOTA: Les visées en antennes ne sont pas prises en compte par
        //       les fonctions de recherche et d'indexation de GHCaveDraw
        QID := E.getGHCaveDrawIDPtAntenne(i);
        WrtLn(Format(GCD_FMT_BASEPOINTS,
                    [QID  ,
                     E.IDTerrain,
                     E.Type_Entite,
                     GetCouleurEntiteByExpe(E).toTColor(),
                     E.PosExtr0.X, E.PosExtr0.Y, E.PosExtr0.Z,
                     E.PosStation.X, E.PosStation.Y, E.PosStation.Z,
                     E.PosPG.X, E.PosPG.Y,  E.PosOPG.Z, // Z1PB,
                     E.PosPD.X, E.PosPD.Y,  E.PosOPD.Z//E.X2PD, E.Y2PD,  E.Z1PH
                     ]));
      end;
    end;
    WrtLn('endbasepoints');
  finally
    MetaFiltre(OldFilter, QDevelViseesVisibles);   // restauration du filtre précédent
    CloseFile(F);
  end;
end;



// Validé OK
procedure TBDDEntites.CalcCouleursByDepth(const QColorZMini, QColorZMaxi: TColor);
var
  i, Nb: Integer;
  EWE: TBaseStation;
begin
  Nb := self.GetNbEntitesVisees();
  SetColorZMiniZMaxi(QColorZMini, QColorZMaxi);
  for i:= LOW_INDEX to Nb - 1 do
  begin
    EWE := GetEntiteVisee(i);
    EWE.CouleurDegrade := CalcColorDegradeByAltitude( 0.50 * (EWE.PosExtr0.Z + EWE.PosStation.Z));
    PutEntiteVisee(i, EWE);
  end;
end;



function TBDDEntites.GetColorViseeFromModeRepresentation(const M: TModeRepresentationGaleries; const E: TBaseStation): TGHTopoColor;
begin
  case M of
    rgENTRANCES: Result := GetCouleurEntiteByEntrance(E);
    rgRESEAUX  : Result := GetCouleurEntiteByReseau(E);
    rgSECTEURS : Result := GetCouleurEntiteBySecteur(E);
    rgSEANCES  : Result := GetCouleurEntiteByExpe(E);
    rgGRAY     : Result.setFrom(clSilver);
    rgDEPTH    : Result.setFrom(E.CouleurDegrade);
  else
    Result.setFrom(clSilver);
  end;
end;


//******************************************************************************
// MétaFiltre
//******************************************************************************
//Signature: function TBDDEntites.MetaFiltre(const Filtre: string): integer;
{$INCLUDE MetaFiltre.inc} // Ne pas modifier ce fichier
//******************************************************************************
function TBDDEntites.RecenserDates(): integer;
var
  i: integer;
  Year, Month, Day: word;
  EWE: TBaseStation;
  LS: TStringList;
begin
  FNbDates := 0;
  AfficherMessage(Format('%s.RecenserDates ()', [ClassName]));
  Result := 0;
  LS := TStringList.Create;
  try
    LS.Sorted := True;
    LS.Duplicates := dupIgnore;
    LS.Clear;
    for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      EWE := self.GetEntiteVisee(i);
      DecodeDate(EWE.DateLeve, Year, Month, Day);
      LS.Add(DateYYYYMMDDToDateSQL(Year, Month, Day));
    end;
    // remplir le tableau des dates
    SetLength(FListeDesDates, 0);
    SetLength(FListeDesDates, LS.Count);
    for i := 0 to LS.Count - 1 do
    begin
      Year  := StrToIntDef(Copy(LS.Strings[i], 1, 4), 1900);
      Month := StrToIntDef(Copy(LS.Strings[i], 6, 2), 1);
      Day   := StrToIntDef(Copy(LS.Strings[i], 9, 2), 1);
      FListeDesDates[i] := GetSecuredDate(Year, Month, Day);
    end;
    Result   := LS.Count;
    FNbDates := LS.Count;

    LS.Clear;  // destruction de la liste
  finally
    FreeAndNil(LS);//LS.Free;
  end;
  AfficherMessage(Format('%s.RecenserDates () OK', [ClassName]));
end;

function TBDDEntites.GetUneDate(const Idx: integer): TDateTime;
begin
  try
    Result := FListeDesDates[Idx];
  except
    Result := TDateTime(0.00);
  end;
end;

function TBDDEntites.GetNbDates(): integer;
begin
  Result := FNbDates;
end;

//******************************************************************************
// stats et diagrammes
// histogramme des profondeurs : calcul
function TBDDEntites.ParseDepthHistogramme(const NbPetales: integer; const ViseeMini: double; const Normalize: boolean): Boolean;
var
  i, j: integer;
  Interval: double;
  dx, dy, dz: double;
  LPT, LP: double;
  Z1, Z2, ZMoy: double;
  ClasseMax: double;
  SinP: double; // sinus de la pente
  MyEntite: TBaseStation;
begin
  Result := false;
  AfficherMessage(Format('%s.ParseDepthHistogramme: Nb = %d; Mini=%.2f', [ClassName, NbPetales, ViseeMini]));
  try
    // pente maxi
    SinP := 0.50; // angle de 30°
    // intervalle
    Interval := (FCoinHautDroit.Z - FCoinBasGauche.Z) / NbPetales;
    // table des classes
    SetLength(FClassRepartDepthDiagram, 0);
    SetLength(FClassRepartDepthDiagram, NbPetales);
    // initialiser cette table
    for i := 0 to NbPetales - 1 do FClassRepartDepthDiagram[i] := 0.00;
    // boucle de calcul
    LPT := 0.00;
    for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      // calcul longueur projetee
      MyEntite := GetEntiteVisee(i);

      if (not MyEntite.Enabled) then Continue; // visées non dessinées
      // /!\ Utiliser IsInNaturalCave et non IsInCaveOrTunnel
      if (not MyEntite.IsInNaturalCave()) then Continue; // galeries non naturelles
      dx := MyEntite.PosStation.X - MyEntite.PosExtr0.X;
      dy := MyEntite.PosStation.Y - MyEntite.PosExtr0.Y;
      dz := MyEntite.PosStation.Z - MyEntite.PosExtr0.Z;
      ZMoy := (MyEntite.PosStation.Z + MyEntite.PosExtr0.Z) / 2.0;
      LP := Hypot3D(dx, dy, dz);

      // test: passer à entitée suivante si LP<ViseeMini
      if (LP < ViseeMini) then CONTINUE;
      if (not IsInRange(dz / LP, -SinP, SinP)) then Continue;
      // cumul des longueurs projetées
      LPT := LPT + LP;
      // répartition
      for j := 0 to NbPetales - 1 do
      begin
        Z1 := FCoinBasGauche.Z + Interval * j;
        Z2 := FCoinBasGauche.Z + Interval * (j + 1);
        if (IsInRange(ZMoy, Z1, Z2)) then FClassRepartDepthDiagram[j] := FClassRepartDepthDiagram[j] + LP;
      end;
    end; // for i ..
    // Longueur totale nulle -> sortie de la fonction
    if (LPT = 0) then Exit;
    if (Normalize) then
    begin
      // mise en pourcentage
      ClasseMax := 0.0;
      for j := 0 to NbPetales - 1 do
        if (FClassRepartDepthDiagram[j] > ClasseMax) then
          ClasseMax := FClassRepartDepthDiagram[j];
      for j := 0 to NbPetales - 1 do FClassRepartDepthDiagram[j] := FClassRepartDepthDiagram[j] / ClasseMax;
    end;
    Result := True;
  except
  end;
end;


// histogramme des directions : calcul
function TBDDEntites.ParseRoseDiagram(const NbPetales: integer; const ViseeMini: double): boolean;
var
  i, j: integer;
  LP: double;
  dx: double;
  dy: double;
  Ang: double;
  LPT: double;
  ClasseMax: double;
  Interval: double;
  MyEntite: TBaseStation;
begin
  //AfficherMessage(Format('%s.ParseDiagram: Nb = %d; Mini=%.2f', [ClassName, NbPetales, ViseeMini]));
  Result := false;
  try
    Interval := PI / NbPetales;
    // dimensionner table des classes
    SetLength(FClassRepartRoseDiagram, 0);
    SetLength(FClassRepartRoseDiagram, NbPetales);
    // initialiser cette table
    for i := 0 to NbPetales - 1 do FClassRepartRoseDiagram[i] := 0.00;
    // boucle de calcul
    LPT := 0.00;
    for i := LOW_INDEX to GetNbEntitesVisees() - 1 do
    begin
      MyEntite := GetEntiteVisee(i);
      // calcul longueur projetee
      if (not MyEntite.Enabled) then Continue;
      if (MyEntite.Type_Entite = tgENTRANCE) then Continue;
      //if (Type_Entite = tgTYPE_ENTITES_VISEES_ANTENNE) then Continue;
      // /!\ Utiliser IsViseetInNaturalCave et non IsViseetInCaveOrTunnel
      if (not MyEntite.IsInNaturalCave()) then Continue;
      dx := MyEntite.PosStation.X - MyEntite.PosExtr0.X;
      dy := MyEntite.PosStation.Y - MyEntite.PosExtr0.Y;
      //dz := MyEntite.PosStation.Z - MyEntite.PosExtr0.Z;
      LP := Sqrt(Sqr(dx) + Sqr(dy));
      // test: passer à entitée suivante si LP<ViseeMini
      if (LP < ViseeMini) then Continue;
      // cumul des longueurs projetées
      LPT := LPT + LP;
      // calcul de l'angle
      // pour le calcul d'une rosette symétrique
      // l'angle retourné est dans la plage [0; PI]
      //Ang:=(0.50*PI)-ArcSin(dy/LP);
      Ang := ArcTan2(dx, dy + 1e-05);
      if (Ang < 0) then Ang := Ang + 2 * PI;
      // répartition
      for j := 0 to NbPetales - 1 do
      begin
        if ((Ang >= j * Interval) and (Ang < (j + 1) * Interval)) then
        begin
          FClassRepartRoseDiagram[j] := FClassRepartRoseDiagram[j] + LP;
        end;
        if ((Ang >= (PI + j * Interval)) and (Ang < (PI + (j + 1) * Interval))) then
        begin
          FClassRepartRoseDiagram[j] := FClassRepartRoseDiagram[j] + LP;
        end;
      end;
    end; // for i ..
    // Longueur totale nulle -> sortie de la fonction
    if (LPT = 0) then Exit;
     // mise en pourcentage
    ClasseMax := 0;
    for j := 0 to NbPetales - 1 do
      if (FClassRepartRoseDiagram[j] > ClasseMax) then ClasseMax := FClassRepartRoseDiagram[j];
    for j := 0 to NbPetales - 1 do FClassRepartRoseDiagram[j] := FClassRepartRoseDiagram[j] / ClasseMax;
    Result := True;
  except
  end;// with self ...
end;





procedure TBDDEntites.ExportVue2DSVG(const Filename: TStringDirectoryFilename;
                                     const DoXHTML: boolean;
                                     const Param2D: TVue2DParams;
                                     const QProcProgress: TProcDisplayProgression);
const
  NOM_STYLE_ENTRANCE            = 'entrance';
  NOM_STYLE_CENTERLINE          = 'centerline';
  NOM_STYLE_VISEE_RAYONNANTE    = 'antenna';
  NOM_STYLE_CROSS_SECTION       = 'cross-section';
  NOM_STYLE_PAROIS_DEFAUT       = 'walls-default';
  NOM_STYLE_SILHOUETTE_DEFAUT   = 'silhouette-default';
  NOM_STYLE_STATIONS_DEFAULT    = 'stations-default';
  NOM_STYLE_NODES_DEFAULT       = 'nodes-default';
  NOM_STYLE_QUADRILLAGE         = 'quadrillage';
  NOM_STYLE_REGLE_NORD_ODD      = 'regle-nord-odd';
  NOM_STYLE_REGLE_NORD_EVEN     = 'regle-nord-even';
  NOM_STYLE_CADRE_PERIMETRIQUE  = 'cadre-perimetrique';



  NOM_STYLE_CADRE               = 'CadreDessin';
  FMT_STYLE_ENTRANCE = 'Entrance%d';
  FMT_STYLE_RESEAU   = 'Reseau%d';
  FMT_STYLE_SECTEUR  = 'Secteur%d';
  FMT_STYLE_SEANCE   = 'Seance%d';
var
  miou: String;
  chat: String;
  QIdx, QNb: integer;
  LocalProcProgression : TProcDisplayProgression;


  MyReseau: TReseau;
  QQQQ: String;
  MySecteur: TSecteur;
  MyExpe: TExpe;
  MyEntrance: TEntrance;
  MargePerimetrique: float;

  procedure ListerStylesEntrances();
  var
    i, Nb: Integer;
    EWE  : TEntrance;
    NS   : String;
    WU: TGHTopoColor;

  begin
    Nb := GetNbEntrances();
    for i := 0 to Nb - 1 do
    begin
      EWE := GetEntrance(i);
      NS  := Format(FMT_STYLE_ENTRANCE, [i]);
      WU  := GetCouleurEntiteByIdxEntrance(i);
      FSVGCanvas.WriteStyleLinePolygoneTexte(NS,
                                             WU.toTColor(), 255, 0.01, psSolid,
                                             WU.toTColor(), Param2D.ongFillOpacite, bsSolid,
                                             DEFAULT_FONT_NAME, 0.3, clBlack, 255, [fsBold], EWE.eNomEntree);
    end;
  end;
  procedure ListerStylesReseaux();
  var
    i, Nb: Integer;
    EWE  : TReseau;
    NS   : String;
    WU: TGHTopoColor;
  begin
    Nb := GetNbReseaux;
    for i := 0 to Nb - 1 do
    begin
      EWE := GetReseau(i);
      NS  := Format(FMT_STYLE_RESEAU, [i]);
      WU  := GetCouleurEntiteByIdxReseau(i);
      FSVGCanvas.WriteStyleLinePolygoneTexte(NS,
                                             WU.toTColor(), 255, 0.01, psSolid,
                                             WU.toTColor(), Param2D.ongFillOpacite, bsSolid,
                                             DEFAULT_FONT_NAME, 0.3, clBlack, 255, [], EWE.NomReseau);
    end;
  end;
  procedure ListerStylesSecteurs();
  var
    i, Nb: Integer;
    EWE  : TSecteur;
    NS   : String;
    WU: TGHTopoColor;
  begin
    Nb := GetNbSecteurs;
    for i := 0 to Nb - 1 do
    begin
      EWE := GetSecteur(i);
      NS  := Format(FMT_STYLE_SECTEUR, [i]);
      WU  := GetCouleurEntiteByIdxSecteur(i);
      FSVGCanvas.WriteStyleLinePolygoneTexte(NS,
                                             WU.toTColor(), 255, 0.01, psSolid,
                                             WU.toTColor(), Param2D.ongFillOpacite, bsSolid,
                                             DEFAULT_FONT_NAME, 0.3, clBlack, 255, [fsBold], EWE.NomSecteur);

    end;
  end;
  procedure ListerStylesSeances();
  var
    i, Nb: Integer;
    EWE  : TExpe;
    NS   : String;
    WU: TGHTopoColor;
  begin
    Nb := GetNbExpes;
    for i := 0 to Nb - 1 do
    begin
      EWE := GetExpe(i);
      NS  := Format(FMT_STYLE_SEANCE, [EWE.IDExpe]);
      WU :=  GetCouleurEntiteByIdxExpe(EWE.IdxCouleur);
      FSVGCanvas.WriteStyleLinePolygoneTexte(NS,
                                             WU.toTColor(), 255, 0.01, psSolid,
                                             WU.toTColor(), Param2D.ongFillOpacite, bsSolid,
                                             DEFAULT_FONT_NAME, 0.3, clBlack, 255, [fsBold], EWE.Commentaire);
    end;
  end;
  procedure DessinerCenterlines(const QMode: TModeTOptionsCenterLine);
  var
    i, Nb: Integer;
    V: TBaseStation;
    EWE: String;
  begin
    EWE := '';
    if (mclCENTERLINE     in QMode) then EWE += 'Polygonale, ';
    if (mclCROSS_SECTIONS in QMode) then EWE += 'Sections, ';
    AfficherMessage('--> Dessin centerlines');
    Nb := GetNbEntitesVisees();
    for i := LOW_INDEX to Nb - 1 do
    begin
      V := GetEntiteVisee(i);
      if (Assigned(LocalProcProgression)) then LocalProcProgression(format('Centerline (%s) shot %d', [EWE, i]), i, 0, Nb, 100);
      //if (V.Type_Entite = tgENTRANCE) then Continue;
      //if (V.Type_Entite = tgVISEE_RADIANTE) then Continue;
      if (mclCENTERLINE     in QMode) then FSVGCanvas.DrawLine(NOM_STYLE_CENTERLINE   , V.PosExtr0.X, V.PosExtr0.Y, V.PosStation.X, V.PosStation.Y);
      if (mclCROSS_SECTIONS in QMode) then FSVGCanvas.DrawLine(NOM_STYLE_CROSS_SECTION, V.PosPG.X, V.PosPG.Y, V.PosPD.X, V.PosPD.Y); //V.X2PG, V.Y2PG, V.X2PD, V.Y2PD);
    end;
  end;
  procedure DessinerAntennes();
  var
    i, Nb: Integer;
    V: TBaseStation;
  begin
    AfficherMessage('--> Dessin antennes');
    Nb := GetNbEntitesAntennes();
    for i := LOW_INDEX to Nb - 1 do
    begin
      V := GetEntiteAntenne(i);
      if (Assigned(LocalProcProgression)) then LocalProcProgression(format('Radiant shot %d', [i]), i, 0, Nb, 200);
      if (V.Type_Entite = tgVISEE_RADIANTE) then
      begin
        FSVGCanvas.DrawLine(NOM_STYLE_VISEE_RAYONNANTE   , V.PosExtr0.X, V.PosExtr0.Y, V.PosStation.X, V.PosStation.Y);
      end;
    end;
  end;

  procedure DessinerSilhouette(const AvecParois: boolean; const QIdxEntrance, QIdxReseau, QIdxsecteur, QIdxExpe: integer);
  var
    Quad: TArrayPoints2Df;
    i, Nb: Integer;
    V: TBaseStation;
    QQC: TColor;
    QQR: TReseau;
    QQS: TSecteur;
    QQE: TExpe;
    QStyle: String;
    QQEntrance: TEntrance;
  begin
    AfficherMessage(Format('--> Dessin Silhouette: %d - %d - %d - %d' , [QIdxEntrance, QIdxReseau, QIdxsecteur, QIdxExpe]));
    Nb := GetNbEntitesVisees();
    //for i := 2 to Nb - 1 do   // TODO: A vérifier
    for i := 0 to Nb - 1 do
    begin
      V := GetEntiteVisee(i);
      if (V.Type_Entite =tgENTRANCE) then Continue;
      case Param2D.ongModeRepresentation of
        rgENTRANCES:
          begin
            QQEntrance := GetEntrance(V.eEntrance);
            if (V.eEntrance <> QIdxEntrance) then Continue;
            QStyle := Format(FMT_STYLE_ENTRANCE, [V.eEntrance]);
          end;
        rgRESEAUX:
          begin
            QQR := GetReseau(V.eReseau);
            if (V.eReseau <> QIdxReseau) then Continue;
            QStyle := Format(FMT_STYLE_RESEAU, [V.eReseau]);
          end;
        rgSECTEURS:
          begin
            QQS := GetSecteur(V.eSecteur);
            if (V.eSecteur <> QIdxsecteur) then Continue;
            QStyle := Format(FMT_STYLE_SECTEUR, [V.eSecteur]);
          end;
        rgSEANCES:
          begin
            QQE := GetExpeByIndex(V.eExpe);
            if (QQE.IDExpe <> QIdxExpe) then Continue;
            QStyle := Format(FMT_STYLE_SEANCE, [QQE.IDExpe]);
          end;
        rgGRAY:
          begin
            QStyle := NOM_STYLE_SILHOUETTE_DEFAUT;
          end;
        rgDEPTH:
          begin
            QQC := CalcColorDegradeByAltitude(V.PosStation.Z);
          end;
      end;
      SetLength(Quad, 4);

      FSVGCanvas.BeginListeVertex();
        FSVGCanvas.AddVertex(V.PosOPD.X, V.PosOPD.Y);
        FSVGCanvas.AddVertex(V.PosPD.X, V.PosPD.Y);
        FSVGCanvas.AddVertex(V.PosPG.X, V.PosPG.Y);
        FSVGCanvas.AddVertex(V.PosOPG.X, V.PosOPG.Y);
      FSVGCanvas.EndListeVertex();
      if (Param2D.ongModeRepresentation = rgDEPTH) then FSVGCanvas.DrawPolygonWithoutStyle(QQC, QQC, 0.00, True, false)
                                                   else FSVGCanvas.DrawPolygon(QStyle, false);
      if (AvecParois) then
      begin
        //SVGCanvas.DrawPolygonSansStyle(NOM_STYLE_CENTERLINE   , V.PosExtr0.Z, V.Une_Station_1_Y, V..PosStation.X, V.PosStation.Y);
      end;
    end;
  end;
  procedure DessinerEntrances(const B: boolean);
  var
    NbE, i: Integer;
    MyEntrance: TEntrance;
  begin
    NbE := GetNbEntrances();
    if (NbE = 0) then Exit;
    for i := 0 to NbE - 1 do
    begin
      MyEntrance := GetEntrance(i);
      if (Assigned(LocalProcProgression)) then LocalProcProgression(format('Marker entrance %d: %s', [i, MyEntrance.eNomEntree]), i, 0, NbE, 1);
      FSVGCanvas.DrawCircle(NOM_STYLE_ENTRANCE, MyEntrance.ePosition.X, MyEntrance.ePosition.Y, 5.00, 5.00);
      FSVGCanvas.DrawTexte(NOM_STYLE_ENTRANCE, 1, MyEntrance.ePosition.X, MyEntrance.ePosition.Y, 0.00, MyEntrance.eNomEntree);
    end;
  end;

  procedure DessinerNoeuds(const B: boolean);
  var
    NbE, i: Integer;
    MyJonction: TJonctionXYZ;
  begin
    NbE := GetNbJonctions();
    if (NbE = 0) then Exit;
    for i := 0 to NbE - 1 do
    begin
      MyJonction := GetJonction(i);
      if (Assigned(LocalProcProgression)) then LocalProcProgression(format('Marker junction %d: %d%d', [i, MyJonction.NoSer, MyJonction.NoSt]), i, 0, NbE, 100);
      FSVGCanvas.DrawCircle(NOM_STYLE_NODES_DEFAULT, MyJonction.Position.X, MyJonction.Position.Y, 2.00, 2.00);
      FSVGCanvas.DrawTexte(NOM_STYLE_NODES_DEFAULT, 1, MyJonction.Position.X, MyJonction.Position.Y, 0.00, format(FMTSERST, [MyJonction.NoSer, MyJonction.NoSt]));
    end;
  end;

  procedure DessinerQuadrillage();
  var
    EWE         , S: string;
    QdrSpacing  , A: Double;
    tt          : Int64;
    BB: TSVGBounds;
  begin
    BB := FSVGCanvas.GetDrawingBounds();
    EWE := 'Quadrillage';
    FSVGCanvas.BeginGroupe(EWE, EWE, 0.00, 0.00);
      QdrSpacing := Param2D.ongQdrSpc;
      tt := round(BB.X1 / QdrSpacing);
      A  := QdrSpacing * tt;
      while (A < BB.X2) do
      begin
        FSVGCanvas.DrawLine(NOM_STYLE_QUADRILLAGE, A, BB.Y1, A, BB.Y2);
        S := Format(FORMAT_NB_REAL_0_DEC,[A]);
        FSVGCanvas.DrawTexte(NOM_STYLE_QUADRILLAGE, 8, A, BB.Y1 + 2.00, 0.00, S);
        A := A + QdrSpacing;
      end;
      tt := round(BB.Y1 / QdrSpacing);
      A  := QdrSpacing * tt;
      while (A < BB.Y2) do
      begin
        FSVGCanvas.DrawLine(NOM_STYLE_QUADRILLAGE, BB.X1, A, BB.X2, A);
        S := Format(FORMAT_NB_REAL_0_DEC,[A]);
        FSVGCanvas.DrawTexte(NOM_STYLE_QUADRILLAGE, 4, BB.X1 + 2.00, A, 0.00, S);
        A := A + QdrSpacing;
      end;
    FSVGCanvas.EndGroupe(EWE);
  end;
  procedure DessinerEchelle(const LongueurEchelle: double);
  const
    POS_ECHELLE = 10.00;
    RATIO_ECHELLE_WITH_HEIGHT = 16;
    NB_CASES_ECHELLE          = 8;

  var
    EWE: String;
    HauteurEchelle, LargeurCase, HauteurCase, HauteurNord: Double;
    i: Integer;
    BB: TSVGBounds;
    C1: TPoint3Df;
  begin
    EWE := 'Regle';
    HauteurEchelle := LongueurEchelle / RATIO_ECHELLE_WITH_HEIGHT;
    LargeurCase    := LongueurEchelle / NB_CASES_ECHELLE;
    HauteurCase    := HauteurEchelle  / 2;
    HauteurNord    := LongueurEchelle / 2;

    BB := FSVGCanvas.GetDrawingBounds();
    C1.setFrom(BB.X1 + POS_ECHELLE, BB.Y1 + POS_ECHELLE, 0.00);
    FSVGCanvas.BeginGroupe(EWE, EWE, 0.00, 0.00);
      FSVGCanvas.DrawRectangle(NOM_STYLE_REGLE_NORD_EVEN, C1.X, C1.Y, C1.X + LongueurEchelle, C1.Y + HauteurEchelle);
      for i := 0 to NB_CASES_ECHELLE - 1 do
      begin
        FSVGCanvas.DrawRectangle(NOM_STYLE_REGLE_NORD_ODD,
                                 C1.X + LargeurCase * i,            C1.Y + IIF(Odd(i), HauteurCase, 0.00),
                                 C1.X + LargeurCase * (i + 1),      C1.Y + IIF(Odd(i), HauteurCase * 2, HauteurCase));

      end;
      // le nord
       FSVGCanvas.DrawLine(NOM_STYLE_REGLE_NORD_EVEN, C1.X, C1.Y + HauteurEchelle, C1.X, C1.Y + HauteurEchelle + HauteurNord);
       FSVGCanvas.BeginListeVertex();
         FSVGCanvas.AddVertex(C1.X, C1.Y + HauteurEchelle + HauteurNord);
         FSVGCanvas.AddVertex(C1.X + LargeurCase, C1.Y + HauteurEchelle + 0.15 * HauteurNord);
         FSVGCanvas.AddVertex(C1.X, C1.Y + HauteurEchelle + 0.15 * HauteurNord);
       FSVGCanvas.DrawPolygon(NOM_STYLE_REGLE_NORD_ODD, false);
       FSVGCanvas.DrawTexte(NOM_STYLE_REGLE_NORD_EVEN, 1, C1.X + 1.01 * LongueurEchelle, C1.Y, 0.00, Format('%.0f m', [LongueurEchelle]));
    FSVGCanvas.EndGroupe(EWE);
  end;
  procedure DessinerCadrePerimetrique();
  var
    EWE: String;
    BB: TSVGBounds;
  begin
    BB := FSVGCanvas.GetDrawingBounds();
    EWE := 'CadrePerimetrique';
    FSVGCanvas.BeginGroupe(EWE, EWE, 0.00, 0.00);
      FSVGCanvas.DrawRectangle(NOM_STYLE_CADRE_PERIMETRIQUE, BB.X1, BB.Y1, BB.X2, BB.Y2);
    FSVGCanvas.EndGroupe(EWE);
  end;

begin
  LocalProcProgression := QProcProgress;
  miou := ChooseString(Ord(Param2D.ongModeRepresentation), ['rgENTRANCES', 'rgRESEAUX', 'rgSECTEURS', 'rgSEANCES', 'rgGRAY', 'rgDEPTH']);
  chat := '';
  if (edPolygonals     in Param2D.ongElementsDrawn) then chat += 'edPolygonals, ';
  if (edStations       in Param2D.ongElementsDrawn) then chat += 'edStations, ';
  if (edIDStations     in Param2D.ongElementsDrawn) then chat += 'edIDStations, ';
  if (edAltitudes      in Param2D.ongElementsDrawn) then chat += 'edAltitudes, ';
  if (edCotes          in Param2D.ongElementsDrawn) then chat += 'edCotation, ';
  if (edWalls          in Param2D.ongElementsDrawn) then chat += 'edWalls, ';
  if (edCrossSections  in Param2D.ongElementsDrawn) then chat += 'edCrossSections, ';
  if (edFillGalerie    in Param2D.ongElementsDrawn) then chat += 'edFillGalerie, ';
  if (edQuadrilles     in Param2D.ongElementsDrawn) then chat += 'edQuadrilles, ';
  if (edENTRANCE_MKS   in Param2D.ongElementsDrawn) then chat += 'edENTRANCE_MKS, ';
  if (edCROQUIS        in Param2D.ongElementsDrawn) then chat += 'edCROQUIS, ';
  if (edANTENNES       in Param2D.ongElementsDrawn) then chat += 'edANTENNES, ';
  if (edJONCTIONS      in Param2D.ongElementsDrawn) then chat += 'edJONCTIONS, ';



  AfficherMessage(Format('%s.ExportVue2DSVG(%s): %s', [ClassName, Filename, BoolToStr(DoXHTML, 'XHTML', 'SVG')]));
  AfficherMessage(Format('-- Mode représentation: %s', [miou]));
  AfficherMessage(Format('-- Elements dessinés  : %s', [chat]));
  FSVGCanvas := TSVGCanvas.Create;
  try
    // marge périmétrique
    MargePerimetrique := 0.02 * Hypot(FCoinHautDroit.X - FCoinBasGauche.X,
                                      FCoinHautDroit.Y - FCoinBasGauche.Y);
    // en-tetes

    FSVGCanvas.Commentaire := '';
    FSVGCanvas.Scale       := 1.00;
    // maxi et mini
    //FSVGCanvas.SetDrawingBounds(FCoinBasGauche.X, FCoinBasGauche.Y, FCoinHautDroit.X, FCoinHautDroit.Y);
    if (Not FSVGCanvas.InitializeDocument(Filename,
                                         DoXHTML,
                                         'Vue 3D by ' + ApplicationName,
                                         Format('%s: Vue en plan', [ExtractFileName(FileName)]),
                                         FCoinBasGauche.X - MargePerimetrique, FCoinBasGauche.Y - MargePerimetrique,
                                         FCoinHautDroit.X + MargePerimetrique, FCoinHautDroit.Y + MargePerimetrique,
                                         AfficherErreurSVG)) then Exit;
    // patterns
    FSVGCanvas.BeginPatternsSection;
      // les patterns ici
    FSVGCanvas.EndPatternsSection;
    // section de styles
    FSVGCanvas.BeginStylesSection;
      // styles de cube englobant
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_CADRE,
                                             clRed, 255, 0.1, psSolid,
                                             clWhite, 255, bsClear,
                                             DEFAULT_FONT_NAME, 0.3, clBlack, 255, [], 'bounding_box_cube');




      // entrées
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_ENTRANCE,
                                             clBlue, 255, DEFAULT_PEN_WIDTH_IN_MM, psSolid,
                                             clBlue, 128, bsSolid,
                                             DEFAULT_FONT_NAME,
                                             Param2D.ongTailleTexteEntrances, Param2D.ongCouleurEntrances,
                                             255, [fsBold], 'Entrances_labels');


      // centerlines, station et sections
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_CENTERLINE,
                                             clBlack, 255,  Param2D.ongViseesLargeurInMM, psSolid,
                                             clBlack, 128, bsClear,
                                             DEFAULT_FONT_NAME,
                                             Param2D.ongTailleTexteCotation, Param2D.ongCouleurCotation,
                                             255, [], 'Centerlines');


      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_CROSS_SECTION,
                                             clGray, 255, 0.015, psSolid,
                                             clGray, 128, bsClear,
                                             DEFAULT_FONT_NAME, 4.0, clBlack, 255, [], 'cross_sections');

      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_PAROIS_DEFAUT,
                                             clBlack, 255, 0.01, psSolid,
                                             clGray, 128, bsSolid,
                                             DEFAULT_FONT_NAME, 4.0, clBlack, 255, [], 'default walls');


      // visées en antenne
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_VISEE_RAYONNANTE,
                                             clGray, 255, 0.01, psSolid,
                                             clGray, 128, bsSolid,
                                             DEFAULT_FONT_NAME, 4.0, clBlack, 255, [], 'antenna_shots');

            // style par défaut: gris
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_SILHOUETTE_DEFAUT,
                                             clGray, 255, 0.00, psSolid,
                                             clGray, 128, bsSolid,
                                             DEFAULT_FONT_NAME, 2.0, clBlack, 255, [], 'Default Silhouettes');
      // noeuds
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_NODES_DEFAULT,
                                             clBlue, 255, 0.01, psSolid,
                                             clAqua, 128, bsSolid,
                                             DEFAULT_FONT_NAME,
                                             Param2D.ongTailleTexteNoeuds, Param2D.ongCouleurNoeuds,
                                             255, [], 'Default Nodes');
      // quadrillage
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_QUADRILLAGE,
                                             Param2D.ongQdrColor, 255, 0.1, psSolid,
                                             clBlack, 192, bsSolid,
                                             DEFAULT_FONT_NAME,
                                             2.0, clBlue, 255, [], 'Quadrillage');
      // règle
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_REGLE_NORD_ODD,
                                             clBlack, 255, 0.02, psSolid,
                                             clGray, 128, bsSolid,
                                             DEFAULT_FONT_NAME, 1.5, clBlack, 255, [], 'Regle-odd');
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_REGLE_NORD_EVEN,
                                             clBlack, 255, 0.1, psSolid,
                                             clWhite, 128, bsClear,
                                             DEFAULT_FONT_NAME, 4.00, clBlack, 255, [], 'Regle-even');
      // cadre périmétrique
      FSVGCanvas.WriteStyleLinePolygoneTexte(NOM_STYLE_CADRE_PERIMETRIQUE,
                                             clBlue, 255, 0.25, psSolid,
                                             clWhite, 128, bsClear,
                                             DEFAULT_FONT_NAME, 1.5, clBlack, 255, [], 'cadre-perimetrique');
      // les styles de galeries ici
      case Param2D.ongModeRepresentation of
        rgENTRANCES  : ListerStylesEntrances();
        rgRESEAUX    : ListerStylesReseaux();
        rgSECTEURS   : ListerStylesSecteurs();
        rgSEANCES    : ListerStylesSeances();
      end;
    FSVGCanvas.EndStylesSection;
    // dessin
    FSVGCanvas.BeginDrawingSection();
      // cadre périmétrique
      DessinerCadrePerimetrique();

      if (edPolygonals     in Param2D.ongElementsDrawn) then DessinerCenterLines([mclCENTERLINE]);
      if (edCrossSections  in Param2D.ongElementsDrawn) then DessinerCenterlines([mclCROSS_SECTIONS]);
      if (edANTENNES       in Param2D.ongElementsDrawn) then DessinerAntennes();
      // quadrillage
      DessinerQuadrillage();
      // échelle
      DessinerEchelle(Param2D.ongQdrSpc);
      case Param2D.ongModeRepresentation of
        rgENTRANCES:
          begin
            QNb := GetNbEntrances();
            for QIdx := 0 to QNb - 1 do
            begin
              MyEntrance := GetEntrance(QIdx);
              if (Assigned(LocalProcProgression)) then LocalProcProgression(format('Entrance %d: %s', [QIdx, MyEntrance.eNomEntree]), QIdx, 0, QNb, 1);
              QQQQ       := Format('Entrance%d', [QIdx]);
              FSVGCanvas.BeginGroupe(QQQQ, MyEntrance.eNomEntree, 0.0, 0.0);
                if (edFillGalerie    in Param2D.ongElementsDrawn) then DessinerSilhouette(False, QIdx, -1, -1, -1);
              FSVGCanvas.EndGroupe(QQQQ);
            end;
          end;

        rgRESEAUX:
          begin
            QNb := GetNbReseaux();
            for QIdx := 0 to GetNbReseaux - 1 do
            begin
              MyReseau := GetReseau(QIdx);
              if (Assigned(LocalProcProgression)) then LocalProcProgression(format('Reseau %d: %s', [QIdx, MyReseau.NomReseau]), QIdx, 0, QNb, 10);
              QQQQ     := Format('Reseau%d', [QIdx]);
              FSVGCanvas.BeginGroupe(QQQQ, MyReseau.NomReseau, 0.0, 0.0);
                if (edFillGalerie    in Param2D.ongElementsDrawn) then DessinerSilhouette(False, -1, QIdx, -1, -1);
              FSVGCanvas.EndGroupe(QQQQ);
            end;
          end;
        rgSECTEURS:
          begin
            QNb := GetNbSecteurs();
            for QIdx := 0 to QNb - 1 do
            begin
              MySecteur := GetSecteur(QIdx);
              if (Assigned(LocalProcProgression)) then LocalProcProgression(format('Secteur %d: %s', [QIdx, MySecteur.NomSecteur]), QIdx, 0, QNb, 10);
              QQQQ     := Format('Secteur%d', [QIdx]);
              FSVGCanvas.BeginGroupe(QQQQ, MySecteur.NomSecteur, 0.0, 0.0);
                if (edFillGalerie    in Param2D.ongElementsDrawn) then DessinerSilhouette(False, -1, -1, QIdx, -1);
              FSVGCanvas.EndGroupe(QQQQ);
            end;
          end;
        rgSEANCES:
          begin
            QNb := GetNbExpes();
            for QIdx := 0 to QNb - 1 do
            begin
              MyExpe   := GetExpe(QIdx);
              {$WARNING: TEXpe.DateExpe à implementer}
              if (Assigned(LocalProcProgression)) then LocalProcProgression(format('Séance %d: %d du %s', [QIdx, MyExpe.IDExpe, DateYYYYMMDDToDateSQL(MyExpe.AnneeExpe, MyExpe.MoisExpe, MyExpe.JourExpe)]), QIdx, 0, QNb, 10);
              QQQQ     := Format('Expe%d', [MyExpe.IDExpe]);
              FSVGCanvas.BeginGroupe(QQQQ, MyExpe.Commentaire, 0.0, 0.0);
                if (edFillGalerie    in Param2D.ongElementsDrawn) then DessinerSilhouette(False, -1, -1, -1, MyExpe.IDExpe);
              FSVGCanvas.EndGroupe(QQQQ);
            end;
          end;
        rgDEPTH:
          begin
            DessinerSilhouette(False, -1, -1, -1, -1);
          end;
      end;
      if (edENTRANCE_MKS in Param2D.ongElementsDrawn) then DessinerEntrances(True);        // entrées
      if (edJONCTIONS    in Param2D.ongElementsDrawn) then DessinerNoeuds(True);
    FSVGCanvas.EndDrawingSection();
    // cloture
    FSVGCanvas.FinalizeDocument;
  finally
    LocalProcProgression := nil;
    FreeAndNil(FSVGCanvas);//SVGCanvas.Free;
  end;
end;

procedure TBDDEntites.SetPositionPointZero(const QPositionPointZero: TPoint3Df);

begin
  FPositionDuPointZero := QPositionPointZero;

  AfficherMessage(Format('%s.SetAltitudePointZero(%.2f, %.2f, %.2f)', [ClassName, FPositionDuPointZero.X, FPositionDuPointZero.Y, FPositionDuPointZero.Z]));
end;

function TBDDEntites.GetPositionPointZero(): TPoint3Df;
begin
  Result := FPositionDuPointZero;
end;

function TBDDEntites.GetDeltaXYZFromPositionSt0(const ST: TBaseStation): TPoint3Df;
begin
  Result.X := ST.PosStation.X - FPositionDuPointZero.X;
  Result.Y := ST.PosStation.Y - FPositionDuPointZero.Y;
  Result.Z := ST.PosStation.Z - FPositionDuPointZero.Z;
end;

//==============================================================================

//******************************************************************************
//******************************************************************************
function TBDDEntites.CalcSpeleometrieParReseauxOuSecteurs(const QMode: integer; out TR: TArraySpeleometrieReseauOuSecteur): boolean;
var
  i, Nb: Integer;
  EWE: TSpeleometrieReseauOuSecteur;
begin
  Result := false;
  case QMode of
    0: Nb := self.GetNbReseaux();
    1: Nb := self.GetNbSecteurs();
  end;
  AfficherMessage(Format('%s.CalcSpeleometriePourReseauOuSecteur: (%s) Nb = %d', [ClassName, IIF((QMode = 1), 'RESEAUX', 'SECTEURS'), Nb]));

  SetLength(TR, 0);
  SetLength(TR, Nb);
  for i := 0 to Nb - 1 do
  begin
    CalcSpeleometrieReseauOuSecteur(QMode, i, EWE);
    TR[i] := EWE;
  end;
end;

function TBDDEntites.GetNbEntitesMetafiltrees(): integer;
begin
  Result := FNbreViseesRetenues;
end;

function TBDDEntites.GenererEntiteDepuisUneVisee(const QStation: TToporobotIDStation;
                                                 const QV: TUneVisee; out EE: TBaseStation): boolean;
var
  CC: TCode;
  EX: TExpe;
  EWE: TBaseStation;
  VQ: TUneVisee;
  dC: TPoint3Df;
  dP: double;
begin
  Result := false;
  try
    VQ := QV;
    // rechercher la station d'accrochage
    AfficherMessageErreur(Format('GenererEntiteDepuisUneVisee(): Depart: %d.%d - C=%d, E=%d - L=%.3f, A=%.3f, P=%.3f',
                                 [QStation.aSerie, QStation.aStation,
                                  QV.Code, QV.Expe,
                                  QV.Longueur, QV.Azimut, QV.Pente]));
    // extraction du point de départ (coordonnées)
    GetEntiteViseeFromSerSt(QStation.aSerie, QStation.aStation, EWE); // then Exit;

    AfficherMessageErreur(Format('%d.%d - Type: %d - %.3f, %.3f, %.3f', [EWE.Entite_Serie, EWE.Entite_Station, QV.TypeVisee, EWE.PosStation.X, EWE.PosStation.Y, EWE.PosStation.Z]));
    CC := GetCodeByIndex(QV.Code);
    EX := GetExpeByIndex(QV.Expe);
    // préparation du résultat
    EE.Entite_Serie    := QStation.aSerie;
    EE.Entite_Station  := QStation.aStation;
    EE.DateLeve        := Now();
    EE.Enabled         := true;
    EE.eCode           := QV.Code;
    EE.eExpe           := QV.Expe;
    EE.IDTerrain       := QV.IDTerrainStation;
    EE.eSecteur        := QV.IDSecteur;

    // tout ceci sera recalculé à la prochaine compilation
    EE.eReseau         := 0;
    EE.eEntrance       := 0;
    EE.setLongAzInc(QV.Longueur, QV.Azimut, QV.Pente);
    EE.setLRUD(QV.LD, QV.LG, QV.HZ, QV.HN);
    // calcul coordonnées
    dC.Empty(); // TODO: Revoir ici
    CalculerVisee(VQ, CC, EX, dC, dP);
    EE.PosExtr0        := EWE.PosStation;
    EE.PosStation.setFrom(EE.PosExtr0.X + dC.X, EE.PosExtr0.Y + dC.Y, EE.PosExtr0.Z + dC.Z);
    EE.Type_Entite     := QV.TypeVisee;
    Result := True;
  except

  end;
end;

// retourne le tableau de stats (dev, encombrement) pour un réseau ou un secteur donné
function TBDDEntites.CalcSpeleometrieReseauOuSecteur(const QMode: integer; const IdxReseauOuSecteur: integer; out TR: TSpeleometrieReseauOuSecteur): boolean;
var
  MyReseau: TReseau;
  MySecteur: TSecteur;
  WU: String;
  i, Nb: Integer;
  EE: TBaseStation;
  IsNatural: Boolean;
  EWE: Boolean;
  procedure CheckC1(const QE: TBaseStation);
  begin
    if (QE.PosExtr0.X < TR.CoordMini.X)   then TR.CoordMini.X := QE.PosExtr0.X;
    if (QE.PosExtr0.Y < TR.CoordMini.Y)   then TR.CoordMini.Y := QE.PosExtr0.Y;
    if (QE.PosExtr0.Z < TR.CoordMini.Z)   then TR.CoordMini.Z := QE.PosExtr0.Z;

    if (QE.PosStation.X < TR.CoordMini.X) then TR.CoordMini.X := QE.PosStation.X;
    if (QE.PosStation.Y < TR.CoordMini.Y) then TR.CoordMini.Y := QE.PosStation.Y;
    if (QE.PosStation.Z < TR.CoordMini.Z) then TR.CoordMini.Z := QE.PosStation.Z;

  end;
  procedure CheckC2(const QE: TBaseStation);
  begin
    if (QE.PosExtr0.X > TR.CoordMaxi.X)   then TR.CoordMaxi.X := QE.PosExtr0.X;
    if (QE.PosExtr0.Y > TR.CoordMaxi.Y)   then TR.CoordMaxi.Y := QE.PosExtr0.Y;
    if (QE.PosExtr0.Z > TR.CoordMaxi.Z)   then TR.CoordMaxi.Z := QE.PosExtr0.Z;

    if (QE.PosStation.X > TR.CoordMaxi.X) then TR.CoordMaxi.X := QE.PosStation.X;
    if (QE.PosStation.Y > TR.CoordMaxi.Y) then TR.CoordMaxi.Y := QE.PosStation.Y;
    if (QE.PosStation.Z > TR.CoordMaxi.Z) then TR.CoordMaxi.Z := QE.PosStation.Z;
  end;
begin
  Result := false;
  try
    case QMode of
      0: MyReseau  := self.GetReseau(IdxReseauOuSecteur);
      1: MySecteur := self.GetSecteur(IdxReseauOuSecteur);
    end;
    case QMode of
      0: WU := MyReseau.NomReseau;
      1: WU := MySecteur.NomSecteur;
    end;
    Nb := self.GetNbEntitesVisees();
    TR.IDReseauSecteur  := IdxReseauOuSecteur;
    TR.Developpement    := 0.00;
    TR.NomReseauSecteur := WU;
    TR.NbVisees         := 0;
    TR.CoordMini.setFrom( INFINI,  INFINI,  INFINI);
    TR.CoordMaxi.setFrom(-INFINI, -INFINI, -INFINI);
    TR.Etendue.Empty();
    for i := 0 to Nb - 1 do
    begin
      EE := self.GetEntiteVisee(i);
      if (EE.Type_Entite = tgSURFACE)   then continue;
      if (EE.Type_Entite = tgFIXPOINT)  then continue;
      if (EE.Type_Entite = tgENTRANCE)  then continue;
      if (EE.Type_Entite = tgVISEE_RADIANTE) then continue;

      IsNatural := (EE.Type_Entite = tgDEFAULT) OR
                   (EE.Type_Entite = tgFOSSILE) OR
                   (EE.Type_Entite = tgVADOSE) OR
                   (EE.Type_Entite = tgENNOYABLE) OR
                   (EE.Type_Entite = tgSIPHON);
      case QMode of
        0: EWE := IsNatural and (EE.eReseau   = IdxReseauOuSecteur);
        1: EWE := IsNatural and (EE.eSecteur  = IdxReseauOuSecteur);
        2: EWE := IsNatural and (EE.eEntrance = IdxReseauOuSecteur);
      end;
      if (EWE) then
      begin
        TR.Developpement := TR.Developpement + Hypot3D(EE.PosStation.X - EE.PosExtr0.X,
                                                       EE.PosStation.Y - EE.PosExtr0.Y,
                                                       EE.PosStation.Z - EE.PosExtr0.Z);
        CheckC1(EE);
        CheckC2(EE);
        TR.NbVisees  += 1;
        TR.Etendue.setFrom(TR.CoordMaxi.X - TR.CoordMini.X,
                           TR.CoordMaxi.Y - TR.CoordMini.Y,
                           TR.CoordMaxi.Z - TR.CoordMini.Z);
      end
    end;
    TR.DonneesValides := (TR.Developpement > 0.0001);
    Result := True;
  except

  end;
end;

// Export en PostScript
procedure TBDDEntites.ExportVue2DPostScript(const PSFileName: TStringDirectoryFilename; const FM: TModeRepresentationGaleries; const FE: TSetElementsDrawn);
const
  LAYER_CENTERLINES = 'centerlines';
  LAYER_SECTIONS    = 'sections';
  LAYER_WALLS       = 'walls';
var
  MyPSCanvas: TPostScriptCanvas;
  MyEntite: TBaseStation;
  i, Nb: Integer;
  EWE: TColor;
  PP: array[0..3] of TPoint2Df;
  MyPen: TPenPSProperties;
begin
  AfficherMessage(Format('%s.ExportVue2DPostScript: %s', [ClassName, PSFileName]));
  MyPSCanvas := TPostScriptCanvas.Create;
  try
    MyPSCanvas.InitializeDocument(PSFileName, false,
                                  'Vue en plan', 'Vue en plan en PostScript',
                                  self.FCoinBasGauche.X, self.FCoinBasGauche.Y,
                                  self.FCoinHautDroit.X, self.FCoinHautDroit.Y);
      // centerlines
      MyPSCanvas.BeginLayer(LAYER_CENTERLINES);
      if (edPolygonals in FE) then
      begin
        Nb := self.GetNbEntitesVisees();
        for i := 0 to Nb - 1 do
        begin
          MyEntite := GetEntiteVisee(i);
          MyPen.Color := GetColorViseeFromModeRepresentation(FM, MyEntite).toTColor();
          MyPen.fWidth:= 0.50;
          MyPSCanvas.SetPen(MyPen);
          MyPSCanvas.DrawLine('',
                              MyEntite.PosExtr0.X, MyEntite.PosExtr0.Y,
                              MyEntite.PosStation.X, MyEntite.PosStation.Y);
        end;
      end;
      // antennes
      if (edANTENNES in FE) then
      begin
        MyPen.Color := clGray;
        MyPen.fWidth:= 0.00;
        Nb := self.GetNbEntitesAntennes();
        for i := 0 to Nb - 1 do
        begin
          MyEntite := GetEntiteAntenne(i);
          MyPSCanvas.SetPen(MyPen);
          MyPSCanvas.DrawLine('',
                              MyEntite.PosExtr0.X, MyEntite.PosExtr0.Y,
                              MyEntite.PosStation.X, MyEntite.PosStation.Y);
        end;
      end;
      MyPSCanvas.EndLayer(LAYER_CENTERLINES);
      // sections
      MyPSCanvas.BeginLayer(LAYER_SECTIONS);
      if (edCrossSections in FE) then
      begin
        MyPen.Color := clGray;
        MyPen.fWidth:= 0.00;
        Nb := self.GetNbEntitesVisees();
        for i := 0 to Nb - 1 do
        begin
          MyEntite := GetEntiteVisee(i);
          if (MyEntite.Type_Entite in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON]) then
          begin
            MyPSCanvas.SetPen(MyPen);
            MyPSCanvas.DrawLine('',
                                MyEntite.PosOPG.X, MyEntite.PosOPG.Y,
                                MyEntite.PosOPD.X, MyEntite.PosOPD.Y);
          end;
        end;
      end;
      MyPSCanvas.EndLayer(LAYER_SECTIONS);
      // contours
      MyPSCanvas.BeginLayer(LAYER_WALLS);
      if (edWalls in FE) then
      begin
        Nb := self.GetNbEntitesVisees();
        for i := 0 to Nb - 1 do
        begin
          MyEntite := GetEntiteVisee(i);
          if (MyEntite.Type_Entite in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON]) then
          begin
            MyPen.Color := GetColorViseeFromModeRepresentation(FM, MyEntite).toTColor();
            MyPen.fWidth:= 0.00;
            PP[0].setFrom(MyEntite.PosOPG.X, MyEntite.PosOPG.Y);
            PP[1].setFrom(MyEntite.PosOPD.X, MyEntite.PosOPD.Y);
            PP[2].setFrom(MyEntite.PosPD.X, MyEntite.PosPD.Y);
            PP[3].setFrom(MyEntite.PosPG.X, MyEntite.PosPG.Y);
            MyPSCanvas.DrawLine('', PP[1].X, PP[1].Y, PP[2].X, PP[2].Y);
            MyPSCanvas.DrawLine('', PP[0].X, PP[0].Y, PP[3].X, PP[3].Y);
          end;
        end;
      end;
      MyPSCanvas.EndLayer(LAYER_WALLS);
    MyPSCanvas.FinalizeDocument();
  finally
    FreeAndNil(MyPSCanvas);//MyPSCanvas.Free;
  end;
end;

function TBDDEntites.GetClassesRepartDepthDiagram(): TVecteurDouble;
begin
  Result := FClassRepartDepthDiagram;
end;
function TBDDEntites.GetClassesRepartRoseDiagram(): TVecteurDouble;
begin
  Result := FClassRepartRoseDiagram;
end;

procedure TBDDEntites.ExportVersTherionXVI(const FichierXVI: TStringDirectoryFilename);
begin
  AfficherMessage(Format('%s.ExportVersTherionXVI(%s)', [self.ClassName, FichierXVI]));
end;

function TBDDEntites.CalcCoordinatesFromBasePtAndOffset(const BP: TIDBaseStation; const Offset: TPoint3Df; out CA: TPoint3Df): boolean;
var
  QSr: TNumeroSerie;
  QSt: integer;
  EWE: TBaseStation;
begin
  Result := false;
  if (ExtractSerStFromTIDStation(BP, QSr, QSt) AND
      GetEntiteViseeFromSerSt(QSr, QSt, EWE)) then
  begin
    CA.X := EWE.PosStation.X + Offset.X;
    CA.Y := EWE.PosStation.Y + Offset.Y;
    CA.Z := EWE.PosStation.Z;
    Result := True;
  end;
end;

procedure TBDDEntites.HighLightVisees(const S: TModeSelectionListe; const N: integer);
var
  BS: TBaseStation;
  i, Nb: Integer;
begin
  Nb := GetNbEntitesVisees();
  AfficherMessage(Format('%s.HighLightSerie(): %d', [Classname, Nb]));
  for i := 0 to Nb - 1 do
  begin
    BS := GetEntiteVisee(i);
    case S of
      mslENTRANCES    : pass;
      mslRESEAUX      : pass;
      mslSECTEURS     : pass;
      mslCODE         : BS.Highlighted := (TNumeroCode(N)  = BS.eCode);
      mslEXPE         : BS.Highlighted := (TNumeroExpe(N)  = BS.eExpe);
      mslSERIE        :
        begin
          if (BS.Highlighted) then BS.Highlighted := false
                              else BS.Highlighted := (TNumeroSerie(N) = BS.Entite_Serie);

        end;
      mslTYPE_GALERIE : pass;
      mslDATE         : pass;
      mslNAMESPACES   : pass;
    end;
    PutEntiteVisee(i, BS);
  end;
end;

function TBDDEntites.CalcColorDegradeByAltitude(const Z: double): TColor;
begin
  Result := GetColorDegrade(Z, FCoinBasGauche.Z, FCoinHautDroit.Z, FCouleurZMin, FCouleurZMax);
end;



procedure TBDDEntites.DisHighLightAll();
var
  Nb, i: Integer;
  BS: TBaseStation;
begin
  Nb := GetNbEntitesVisees();
  for i := 0 to Nb - 1 do
  begin
    BS := GetEntiteVisee(i);
    BS.Highlighted := false;
    PutEntiteVisee(i, BS);
  end;
end;

procedure TBDDEntites.HighlightSeries(const QArrSeriesHighlighted: array of TNumeroSerie);
var
  i, j, Nb, NbVisees: Integer;
  BS: TBaseStation;
begin
  Nb := length(QArrSeriesHighlighted);
  if (0 = Nb) then exit;
  NbVisees :=  GetNbEntitesVisees();
  for i := 0 to NbVisees - 1 do
  begin
    BS := GetEntiteVisee(i);
    for j := 0 to Nb -1 do
    begin
      if (QArrSeriesHighlighted[j] = BS.Entite_Serie) then
      begin
        BS.Highlighted := True;
        Break;
      end;
    end;
    PutEntiteVisee(i, BS);
  end;
end;

procedure TBDDEntites.HighlightExpes(const QArrExpesHighlighted: array of TNumeroExpe);
var
  Nb, i, NbVisees, j: Integer;
  BS: TBaseStation;
begin
  Nb := length(QArrExpesHighlighted);
  if (0 = Nb) then exit;
  NbVisees :=  GetNbEntitesVisees();
  for i := 0 to NbVisees - 1 do
  begin
    BS := GetEntiteVisee(i);
    for j := 0 to Nb -1 do
    begin
      if (QArrExpesHighlighted[j] = BS.eExpe) then
      begin
        BS.Highlighted := True;
        Break;
      end;
    end;
    PutEntiteVisee(i, BS);
  end;
end;

procedure TBDDEntites.HighlightCodes(const QArrCodesHighlighted: array of TNumeroCode);
var
  Nb, i, NbVisees, j: Integer;
  BS: TBaseStation;
begin
  Nb := length(QArrCodesHighlighted);
  if (0 = Nb) then exit;
  NbVisees :=  GetNbEntitesVisees();
  for i := 0 to NbVisees - 1 do
  begin
    BS := GetEntiteVisee(i);
    for j := 0 to Nb -1 do
    begin
      if (QArrCodesHighlighted[j] = BS.eCode) then
      begin
        BS.Highlighted := True;
        Break;
      end;
    end;
    PutEntiteVisee(i, BS);
  end;
end;

procedure TBDDEntites.HighlightEntrances(const QArrEntrancesHighlighted: array of TNumeroEntrance);
var
  i, j, Nb, NbVisees: Integer;
  BS: TBaseStation;
begin
  Nb := length(QArrEntrancesHighlighted);
  if (0 = Nb) then exit;
  NbVisees :=  GetNbEntitesVisees();
  for i := 0 to NbVisees - 1 do
  begin
    BS := GetEntiteVisee(i);
    for j := 0 to Nb -1 do
    begin
      if (QArrEntrancesHighlighted[j] = BS.eEntrance) then
      begin
        BS.Highlighted := True;
        Break;
      end;
    end;
    PutEntiteVisee(i, BS);
  end;

end;

procedure TBDDEntites.HighlightReseaux(const QArrReseauxHighlighted: array of TNumeroReseau);
var
  i, j, Nb, NbVisees: Integer;
  BS: TBaseStation;
begin
  Nb := length(QArrReseauxHighlighted);
  if (0 = Nb) then exit;
  NbVisees :=  GetNbEntitesVisees();
  for i := 0 to NbVisees - 1 do
  begin
    BS := GetEntiteVisee(i);
    for j := 0 to Nb -1 do
    begin
      if (QArrReseauxHighlighted[j] = BS.eReseau) then
      begin
        BS.Highlighted := True;
        Break;
      end;
    end;
    PutEntiteVisee(i, BS);
  end;
end;


procedure TBDDEntites.HighlightSecteurs(const QArrSecteursHighlighted: array of TNumeroSecteur);
var
  i, j, Nb, NbVisees: Integer;
  BS: TBaseStation;
begin
  Nb := length(QArrSecteursHighlighted);
  if (0 = Nb) then exit;
  NbVisees :=  GetNbEntitesVisees();
  for i := 0 to NbVisees - 1 do
  begin
    BS := GetEntiteVisee(i);
    for j := 0 to Nb -1 do
    begin
      if (QArrSecteursHighlighted[j] = BS.eSecteur) then
      begin
        BS.Highlighted := True;
        Break;
      end;
    end;
    PutEntiteVisee(i, BS);
  end;
end;




// exporter les antennes comme nuages de points
procedure TBDDEntites.ExporterAntennesNuagePoints(const FichierPLY: TStringDirectoryFilename);
const PROPERTY_FLOAT = 'property float %s';
var
  fp: TextFile;
  i, Nb: Integer;
  VA: TBaseStation;
begin
  AfficherMessage(Format('%s.ExportToPLY: %d vertex to %s', [classname, Nb, FichierPLY]));
  Nb := GetNbEntitesAntennes();
  if (0 = Nb) then Exit;
  AssignFile(fp, FichierPLY);
  try
    ReWrite(fp);
    writeln(fp, 'ply');
    writeln(fp, Format('format %s', ['ascii 1.0']));
    writeln(fp, Format('comment %s - %s', [rsGHTOPOEXENAME, DateTimeToStr(Now())]));          // comment created by safir
    writeln(fp, Format('element %s %d', ['vertex', Nb]));                                     // element vertex 3337
    writeln(fp, Format(PROPERTY_FLOAT, ['x']));                                               // property float x
    writeln(fp, Format(PROPERTY_FLOAT, ['y']));                                               // property float y
    writeln(fp, Format(PROPERTY_FLOAT, ['z']));                                               // property float z
    writeln(fp, Format('element %s %d', ['face', 0]));                                        // element face 0
    writeln(fp, Format('property %s %s %s %s', ['list', 'uchar', 'int', 'vertex_indices']));  //property list uchar int vertex_indices
    writeln(fp, 'end_header');
    for i := 0 to Nb - 1 do
    begin
      VA := GetEntiteAntenne(i);
      WriteLn(fp, Format('%s %s %s', [FormatterNombreWithDotDecimal(VA.PosStation.X), FormatterNombreWithDotDecimal(VA.PosStation.Y), FormatterNombreWithDotDecimal(VA.PosStation.Z)]));
    end;
  finally
    Closefile(fp);
  end;
end;



end.

