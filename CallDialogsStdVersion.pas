unit CallDialogsStdVersion;
// CallDialogs pour la version std de GHTopo

// Date: 19/04/2012
// Statut: Fonctionnel
// Ajouts en cours au fil du portage
// 17/01/2014: Nouveau dialogue Distance
// 26/04/2014: Centralisation du TSaveDialog
// 22/05/2015: Ajout d'un dialogue de recherche de IDTerrain
// 13/10/2015: Fixation du bug des chemins d'accès Unicode (caractères accentués)
// 13/06/2019: Point de contrôle temporel (contrôle de version)
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  StructuresDonnees,
  Common, Classes, SysUtils, FileUtil,
  ToporobotClasses2012,
  unitCroquisTerrain,
  ConvertisseurJPC,
  UnitObjetSerie,
  UnitEntitesExtended,
  UnitClassPalette,
  UnitClasseMaillage,
  UnitListesSimplesWithGeneriques,
  unitProfilTopo
  , Forms, Dialogs, Controls, Graphics, StdCtrls
  , Grids
  , Printers      //, OSPrinters, // OSPrinters est INDISPENSABLE ici !!
  , PrintersDlgs
  , Printer4Lazarus
  , FastGEO
  , UnitGraphes1
  ;
// mini convertisseur
function DisplayMiniConvertisseur(const EPSGIn, EPSGOut: integer; var QX, QY: double): boolean;
// appel de l'outil Calculette
function DisplayCalculette(const FD: TToporobotStructure2012;
                           const FBE: TBDDEntites;
                           const MM : TMaillage;
                           const FL: TGeoPolygon2D;
                           const Expression: string;
                           const QTabIndex: integer;
                           var Coords: TPoint2Df;
                           out ResultatCalcul: double;
                           out Declimag: double): boolean;

// éditeur de texte interne
procedure DisplayTextEditor(const FileTexte: TStringDirectoryFilename; const EditMode: boolean);  overload;
procedure DisplayTextEditor(const QTexte: string); overload;
// sélection expé, code, ... dans liste
function  SelectionDansListe(const DT: TToporobotStructure2012; const Mode: TModeSelectionListe; const UseInternalIndex: boolean; out SelectedIndex: integer): boolean;


// sélection de couleur Toporobot
function SelectionCouleurToporobot(const OldIdx: integer): Integer;
// choix d'une couleur
function ChooseColor(const OldColor: TColor): TColor;
// choix d'une couleur, avec un retour sous forme de texte
function SelectColor(): string;

// choix d'un type de galerie
function ChooseTypeVisee(const T: integer): integer;

// paramétrage d'un onglet de vue 2D
function ParametrerOngletVue2D(var O: TVue2DParams): boolean;
// paramétrage d'un onglet de vue 3D
function ParametrerOngletVue3D(var O: TVue3DParams): boolean;
// paramétrage du visualisateur coupe développée
function ParametrerOngletCoupeDeveloppee(var O: TCoupeDeveloppeeParams): boolean;

// définition d'attributs de texte pour imprimante
function SelectAttributsTexte(const AT: TTexteAttributs): TTexteAttributs;

// système d'aide
procedure DisplayHelpSystem(const Topics: string; const DoDisplaySandbox: boolean = false);
// fenêtres avec passage d'un TBDDEntites
// -- centre d'impression
function DisplayCentreImpressionExt(const QBDDTopo     : TToporobotStructure2012;
                                    const QBDDEntites  : TBDDEntites;
                                    const QCroquis     : TCroquisTerrain;
                                    const QMaillageMNT : TMaillage;
                                    const QVue2DParams : TVue2DParams;
                                    const QFiltres     : string = ''): boolean;
// -- export SIG
function DisplayExportSIG(const QDocTopo: TToporobotStructure2012; const QBDDEntites: TBDDEntites): boolean;
// -- statistiques
function DisplayStatistiquesExt(const QDocTopo: TToporobotStructure2012; const QBDDEntites: TBDDEntites): boolean;
procedure DisplayVue3DGDIExt(const QBDDEntites: TBDDEntites;
                             const DT: TToporobotStructure2012;
                             const QMaillageMNT : TMaillage;
                             const QVue2DParams : TVue2DParams);
// vue 3D OpenGL
{$IFDEF USE_VIEWER_OPENGL}
procedure DisplayVue3DOpenGLExt(const QBDDEntites: TBDDEntites;
                                const QMaillage: TMaillage;
                                const QFiltres: string);
{$ENDIF}

// affichage d'une station (détails + QR)
procedure AfficherInfosQRCodeStation(const FD: TBDDEntites;
                                     const ST: TBaseStation);
// Capture d'écran
procedure CapturerVueVersPNG(const QHandle: THandle; const QFileName: string);

// Enregistrement de fichiers
function DoDialogSaveFile(const QFilter: string; const QDefaultExt: string; var QFileName: TStringDirectoryFilename; out idxFilter: integer): boolean;
function DoDialogOpenFile(const QFilter: string; const QDefaultExt: string; var QFileName: TStringDirectoryFilename): boolean; overload;
function DoDialogOpenFile(const QInitialdir:TStringDirectoryFilename;
                          const QFilter: string;
                          const QDefaultExt:
                          string; var QFileName: TStringDirectoryFilename): boolean; overload;
function DoDialogOpenFileGHTopo(const AllowQuitGHTopo: boolean;
                                const QListeRecents: TStringList;
                                var   QFileName: TStringDirectoryFilename): boolean; overload;
// dialogue simplifié d'ouverture de fichiers GHTopo
function DoDialogOpenFileGHTopoSimplifie(out QFileName: TStringDirectoryFilename): boolean;
{$IFDEF USE_COUPES_DEVELOPPEES}
// Editeur de coupe développée
procedure AfficherEditeurCoupeDeveloppee(const FT: TToporobotStructure2012);
{$ENDIF USE_COUPES_DEVELOPPEES}
// Question Oui-Non
function GHTopoQuestionOuiNon(const Msg: string): boolean;
function GHTopoQuestionOuiNonWithOptionalAction(const Title, MsgQuestion, MsgOptionally: string; var DoAction: boolean): boolean;

// liste les stations les plus proches d'une station et retourne le numéro série/station de la station choisie
function ChooseNearStationAtPoint(const TD: TToporobotStructure2012;
                                  const BTC: TBDDEntites;
                                  const QNumeroSerie  : TNumeroSerie;
                                  const QNumeroStation: integer;
                                  const RayonDeCapture: double;
                                  out EntiteSelected: TBaseStation): boolean;
function DisplayNearStationsAtPoint(const TD: TToporobotStructure2012;
                                    const BTC: TBDDEntites;
                                    const QNumeroSerie: TNumeroSerie;
                                    const QNumeroStation: integer;
                                    const RayonDeCapture: double): boolean;

// choisir un ID Terrain
function SelectIDTerrain(const DC: TToporobotStructure2012; var QIDxTerrain: integer): boolean;
function DispDlgFindIDTerrain(const QDC: TToporobotStructure2012;
                              out   QDoMatchExact: boolean;
                              out   QFindWhat: string): boolean;

//******************************************************************************
// FONCTIONS SPECIFIQUES A LA VERSION TABLETTE
//******************************************************************************


// édition de codes, expés, secteurs, réseaux
// Ces fonctions modifient un record et retournent une autorisation (O/N). Point
// Le paramètre FD ne sert que pour les opérations de contrôle.

function EditerEntrance(const FD: TToporobotStructure2012;  const QIdx: integer; const B: TBDDEntites; const M: TMaillage; var EE: TEntrance): boolean;
function EditerReseau(const FD: TToporobotStructure2012; const QIdx: integer; var RS: TReseau): boolean;
function EditerSecteur(const FD: TToporobotStructure2012; const QIdx: integer; var SS: TSecteur): boolean;
function EditerCode(const FD: TToporobotStructure2012; const QIdx: integer; var CC: TCode): boolean;
function EditerExpe(const FD: TToporobotStructure2012; const QIdx: integer; var EX: TExpe): boolean;
function DisplayUneSerie(const FD: TToporobotStructure2012; const PD: TBDDEntites; const QIdx: integer; var SR: TObjSerie): boolean;
function EditerAnnotation(const FC: TCroquisTerrain; const QIdx: integer; var AN: TKrobardAnnotation): boolean;

// recopie vers le bas dans une grille
procedure GridUndocopy(const MyGrid: TStringGrid; const AutoIncremented: boolean);
// suppression de lignes
procedure GridEraseLines(const MyGrid: TStringGrid);
// insertion de lignes
procedure GridInsertLignes(const MyGrid: TStringGrid);
// se positionner sur une cellule de la grilles
procedure GridSetCurrentRow(const MyGrid: TStringGrid; const R, C: integer);

// sélection d'un topographe
function SelectTopographes(out Topographes: string): boolean;

// fusionner des topographies
procedure DispFusionTopographies(const FD: TToporobotStructure2012;
                                 const CV: TConversionSysteme);
// imprimante
function SelectionnerImprimante(var MyPrinter: TPrinter): boolean;
// choix de code, d'expé, secteur
function SelectionnerSecteurCodeExpe(const FD: TToporobotStructure2012;
                                     const MyRow: integer;
                                     const DoUseLastItems: boolean;
                                     var MyIdxSecteur: integer;
                                     var MyIdxCode: TNumeroCode;
                                     var MyIdxExpe: TNumeroExpe): boolean;

// gestion des visées rayonnantes
function DispGestionDesViseesRayonnantes(const FD: TToporobotStructure2012;
                                     const ED: TBDDEntites): boolean;
// lister les dépendances d'une série
function ListerDependancesDeUneSerie(const FD: TToporobotStructure2012;
                                     const IdxSerie: integer): boolean;
// maintenance du document
procedure DispMaintenanceDocument(const FD: TToporobotStructure2012;
                                  const FE: TBDDEntites;
                                  const FC: TConversionSysteme;
                                  const PostAction: TProcOfObjectWithOneBoolParameter);
// affichage de la distance entre deux stations
function DispDistanceEntreStations(const FD: TToporobotStructure2012;
                                   const FE: TBDDEntites;
                                   var   S1, S2: TBaseStation;
                                   const DescriptionAction: string;
                                   const CaptionAdditionalAction: string): integer;

// affichage d'un profil topo
function  DisplayProfilTopo(const DT: TToporobotStructure2012; const QMaillage: TMaillage; var ProfilTopo: TProfilTopo): TModalResult;
// gestion des filtres
function DisplayAndEditFiltres(const FD: TToporobotStructure2012; var F: string): boolean;


// sélection d'un système de coordonnées
function SelectCoordinatesSystem(const FC: TConversionSysteme; var SC: TLabelSystemesCoordsEPSG): boolean;

// editer une couche GIS
function EditerGisLayer(var L: TGISLayer): boolean;

// InputQuery amélioré
// En mode Raspberry, affiche un clavier virtuel sinon appelle un InputQuery
function GHTopoInputQuery(const ACaption, APrompt: string; var QValeur: string): boolean; inline;
function GHTopoInputPasswordQuery(const ACaption, APrompt: string; var QPWD: string): boolean;





// afficher un clavier (pour Raspberry et GHTopo simplifié)
{$IFDEF GHTOPO_SIMPLIFIE}
  //{$IFDEF RASPBERRY_PI}
  function DisplayClavierVirtuel(const ACaption, APrompt: string; var AValeur: string): boolean;
  //{$ENDIF RASPBERRY_PI}
{$ELSE}
// import depuis un autre document (uniquement en mode GHTopoFX)
function DisplayImportFromOtherDocumentGHTopo(const FD: TToporobotStructure2012; const FC: TConversionSysteme): boolean;
{$ENDIF GHTOPO_SIMPLIFIE}

// afficher les profils topo
function DisplayListeProfils(const DT: TToporobotStructure2012; const MyMaillage: TMaillage): boolean;
function DisplayEditerPOI(const DT: TToporobotStructure2012; var MyPOI: TPointOfInterest): boolean;
// afficher un graphe de réseau
function DisplayGrapheOfReseau(const B: TBDDEntites; const G: TPathFindingGraphe): boolean;
// Spécifique Chine: formulaire de dénonciation de chrétiens au PCC
procedure RegisterChineseUser();
// Saisie de ID stations
function SaisirIDStation(const Msg: string; var S: string): boolean;
// clavier numérique
function DisplayClavierNumerique(const Title: string; const ModeSaisie: TPaveNumModeSaisie; var Value: string): boolean;
// Recherche dans la base
function SearchInGHTopoDatabase(const FD: TToporobotStructure2012; const FE: TBDDEntites; const QFindWhat: string; const QPerformAction: TProcedureOfObject): boolean;

implementation
uses
  DGCDummyUnit
  // Les virgules de séparation d'unités sont placées devant elles
  // ce qui facilite la compilation conditionnelle
  , dlgToporobotColorNew   // sélecteur de couleur
  , dlgSelectDansListes // sélection dans une liste
  , dlgSelectionTypeVisee // type visée
  // CentreImpression,      // outil d'impression (obsolète)
  , dlgHelpSystem         // système d'aide
  , dlgCalculette         // calculette
  , dlgParametrerOngletVue2D // dialogue de configuration d'une vue 2D
  , MiniTextEditor        // éditeur de texte incorporé
  , dlgSetTexteAttributs  // selecteur de fontes imprimante
  //dlgSibertConvertisseur, // pilote pour le convertisseur Sibert
  // fenêtres utilisant un TBDDEntites
  , CentreImpressionExt   // centre d'impression
  , VueEn3DExt            // visualisateur 3D GDI
  , frmEditSerie          // éditeur de série isolée

  {$IFDEF USE_VIEWER_OPENGL}
  , VueEn3DOpenGLExt         // visualisateur 3D OpenGL
  {$ENDIF}
  , dlgExportVersSIGExt   // export vers SIG
  , dlgStatistiquesExt    // stats (nouvelle version)
  //dlgStatistiquesExt2,    // stats (nouvelle version)

  , frmInfosDetailleesStation // Infos station + QRCode
  {$IFDEF USE_COUPES_DEVELOPPEES}
  , dlgCoupeDeveloppee      // éditeur de coupe développée
  {$ELSE}
  {$ENDIF}


  , dlgListeNearViseesTo    // sélecteur de stations voisines
  , frmSelectIDTerrain      // sélection d'IDTerrain
  , frmFindAStation

  // dialogues d'édition réseau, secteur, expés, codes
  , frmEditEntrance
  , frmEditReseau
  , frmEditSecteur
  , frmEditCode
  , frmEditExpe
  , frmEditAnnotation
  , frmSelectTopographes // topographes
  , frmFusionnerTopos
  , frmDependancesDeUneSerie
  , frmMaintenanceDocument
  , frmSelectGHTopoFiles
  , frmSelectGHTopoFilesSimplifie
  , frmDistanceEntreDeuxPoints
  , frmEditFiltres
  , frmSelectSecteurCodeExpe
  , frmLesViseesRayonnantes
  , frmProfilTopo
  , frmListeProfilsTopo
  , frmSelectCoordsSystem
  , frmMiniConvertisseur
  , frmClavierNumerique
  , frmQuestionOuiNonWithAction
  , dlgEditerPOI
  , frmDisplayGraphe
  {$IFDEF GHTOPO_SIMPLIFIE}
    //{$IFDEF RASPBERRY_PI}
    , frmClavierVirtuel
    //{$ENDIF RASPBERRY_PI}
  {$ELSE}
  , frmAjouterSeriesDepuisAutreDoc

  {$ENDIF GHTOPO_SIMPLIFIE}
  , frmEditGISLayer
  , frmRegisterChineseUser
  , frmSearchInDatabase
  ; // et ce point-virgule est indispensable
//******************************************************************************
// mini convertisseur
function DisplayMiniConvertisseur(const EPSGIn, EPSGOut: integer; var QX, QY: double): boolean;
var
  TD: TdlgMiniConvertisseur;
begin
  TD := TdlgMiniConvertisseur.Create(application);
  try
    if (TD.Initialiser( EPSGIn, EPSGOut, QX, QY)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        QX := TD.editX_Cible1JPC.Value;
        QY := TD.editY_Cible1JPC.Value;
        Result := True;
      end;

    end;
  finally
    FreeAndNil(TD);
  end;
end;

// calculette et convertisseur de coordonnées
function DisplayCalculette(const FD : TToporobotStructure2012;
                           const FBE: TBDDEntites;
                           const MM : TMaillage;
                           const FL : TGeoPolygon2D;
                           const Expression: string;
                           const QTabIndex: integer;
                           var Coords: TPoint2Df;
                           out ResultatCalcul: double;
                           out Declimag: double): boolean;
var
  TD: TfrmCalculette;
begin
  Result := False;
  ResultatCalcul := 0.00;

  TD := TfrmCalculette.Create(Application);

  try
    TD.SetDefaultExpression(Expression);
    TD.SetCoordonnees(Coords);
    TD.SetTabIndex(QTabIndex);
    {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
    TD.SetDocuTopo(FD, FBE, MM, FL);
    {$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}
    TD.ShowModal;

    if (TD.ModalResult = mrOK) then
    begin
      ResultatCalcul:= TD.GetResultatCalcul;
      Declimag      := TD.GetDeclimag;
      Coords        := TD.GetCoordonnees;
      Result := True;
    end;
  finally
    TD.Release;
  end;
end;

// Editeur de texte interne
// TODO: L'implémenter (fonction vide)
//EditMode=0 : Consultation; 1 = Edition;
procedure DisplayTextEditor(const FileTexte: TStringDirectoryFilename; const EditMode: boolean); overload;
var
  TD: TdlgEditeur;
begin
  TD := TdlgEditeur.Create(Application);
  try
    if (TD.InitialiseEditor(FileTexte, EditMode)) then TD.ShowModal;
  finally
    TD.Free;
  end;
end;
procedure DisplayTextEditor(const QTexte: string); overload;
var
  TD: TdlgEditeur;
begin
  TD := TdlgEditeur.Create(Application);
  try
    if (TD.InitialiseEditor(QTexte)) then TD.ShowModal;
  finally
    TD.Free;
  end;
end;

// Question Oui-Non
function GHTopoQuestionOuiNon(const Msg: string): boolean;
begin
  Result := (MessageDlg(GetResourceString(Msg), mtConfirmation,[mbYES, mbNO], 0) = mrYES);
end;
// Question Oui Non avec une action optionnelle
function GHTopoQuestionOuiNonWithOptionalAction(const Title, MsgQuestion, MsgOptionally: string; var DoAction: boolean): boolean;
var
  TD: TdlgQuestionOuiNonWithAction;
begin
  Result   := false;
  TD := TdlgQuestionOuiNonWithAction.Create(Application);
  try
    if (TD.Initialiser(Title, MsgQuestion, MsgOptionally, DoAction)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrYes) then
      begin
        DoAction := TD.GetChecked();
        Result   := True;
      end;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

// sélection expé, code, ... dans liste
function SelectionDansListe(const DT: TToporobotStructure2012; const Mode: TModeSelectionListe; const UseInternalIndex: boolean; out SelectedIndex: integer): boolean;
var
  TD: TdlgSelectElement;
begin
  // ancien index = en cas d'abandon, reprend cet index
  Result := false;
  TD := TdlgSelectElement.Create(Application);
  SelectedIndex := 0;
  try
    if (TD.InitialiseListeSelection(DT, Mode, SelectedIndex, UseInternalIndex)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        SelectedIndex := TD.GetIndexElement(UseInternalIndex); //  Result := GetNumeroElement;      ;
        Result := True;
      end;
    end
    else
    begin
      ShowMessage(rsMATCHNOTFOUND);
    end;
  finally
    TD.Release;
  end
end;

// sélection de couleurs issues de palettes
// Version simplifiée: les autres palettes ne sont pas utilisées bien
// qu'opérationnelles. La palette AutoCAD est utilisée dans un autre module
function SelectionCouleurToporobot(const OldIdx: integer): integer;
var
  TD: TDialogSelTopoColor;
begin
  Result := OldIdx;
  TD := TDialogSelTopoColor.Create(Application);
  try
      TD.Initialiser(OldIdx);
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then Result  := TD.GetIdxColor;
  finally
    TD.Release;
  end;
end;


// choix d'une couleur RGB
function ChooseColor(const OldColor: TColor): TColor;
var
  TC: TColorDialog;
begin
  Result := OldColor;
  TC := TColorDialog.Create(Application);
  TC.Color:= OldColor;
  if (TC.Execute) then Result := TC.Color;
  FreeAndNil(TC);
end;

function SelectColor(): string;
var
  EWE: TColor;
begin
  EWE := ChooseColor(clWhite);
  Result := Format('$%.2X%.2X%.2X', [Red(EWE), Green(EWE), Blue(EWE)]);
end;



//******************************************************************************
// Fenêtres utilisant un TBDDEntites
function DisplayCentreImpressionExt(const QBDDTopo     : TToporobotStructure2012;
                                    const QBDDEntites  : TBDDEntites;
                                    const QCroquis     : TCroquisTerrain;
                                    const QMaillageMNT : TMaillage;
                                    const QVue2DParams : TVue2DParams;
                                    const QFiltres     : string = ''): boolean;

var
  PT: TfrmPrintingCenterExt;
begin
  Result := False;
  PT := TfrmPrintingCenterExt.Create(Application);
  try
    if (PT.InitialiseByPointer(QBDDTopo,
                               QBDDEntites,
                               QCroquis,
                               QMaillageMNT,
                               QVue2DParams,
                               QFiltres)) then
    begin
      PT.Caption := Format(GetResourceString(rsPRN_TITLE), [QBDDTopo.GetNomEtude()]);
      PT.ShowModal;
    end;
    Result := True;
  finally
    PT.Release;
  end;
end;
// export SIG
function DisplayExportSIG(const QDocTopo   : TToporobotStructure2012;
                          const QBDDEntites: TBDDEntites): boolean;
var
  TD: TfrmExportVersSIGExt;
begin
  Result := False;
  TD := TfrmExportVersSIGExt.Create(Application);
  if (TD.InitialiseByPointer(QDocTopo, QBDDEntites)) then TD.ShowModal;
  Result := True;
  TD.Release;
 end;

// statistiques
function DisplayStatistiquesExt(const QDocTopo   : TToporobotStructure2012;
                                const QBDDEntites: TBDDEntites): boolean;
var
  FS : TfrmStatistiquesExt;
  QDevelViseesVisibles: double;
begin
  AfficherMessage(Format('DisplayStatistiquesExt: %d - %s entites', [QBDDEntites.GetNbEntitesVisees(), Application.ExeName]));
  Result := False;
  FS := TfrmStatistiquesExt.Create(Application);
  try
    if (FS.InitialiseByPointer(QDocTopo, QBDDEntites, '')) then
    begin
      FS.Caption := EnlevePerluete(GetResourceString(rsSTATISTIQUES));
      FS.ShowModal;
      QBDDEntites.MetaFiltre('', QDevelViseesVisibles);
    end;
    Result := True;
  finally
    FS.Release;
  end;
end;
// vue 3D GDI
procedure DisplayVue3DGDIExt(const QBDDEntites: TBDDEntites;
                             const DT: TToporobotStructure2012;
                             const QMaillageMNT : TMaillage;
                             const QVue2DParams : TVue2DParams);

var
  TD: TfrmVue3DExt;
begin
  AfficherMemoryUsage();
  TD := TfrmVue3DExt.Create(Application);
  try
    if (TD.InitialiseByPointer(QBDDEntites, DT, QMaillageMNT, QVue2DParams)) then
    begin
      TD.Caption := EnlevePerluete(GetResourceString(rsVUE3D));
      TD.ShowModal;
    end;
  finally
    TD.Release;
  end;
  AfficherMemoryUsage();
end;
// vue 3D OpenGL
{$IFDEF USE_VIEWER_OPENGL}
procedure DisplayVue3DOpenGLExt(const QBDDEntites: TBDDEntites;
                                const QMaillage: TMaillage;
                                const QFiltres: string);
var
  TD: TfrmVueEn3DOpenGLExt;
begin
  AfficherMemoryUsage();
  TD := TfrmVueEn3DOpenGLExt.Create(Application);
  try
    if (TD.InitialiseByPointers(QBDDEntites, QMaillage, QFiltres)) then
    begin
      TD.Caption := GetResourceString(rsRENDU3D);
      TD.ShowModal;
    end;
  finally
    TD.Release;
  end;
  AfficherMemoryUsage();
end;
{$ENDIF}

//******************************************************************************
// choix d'un type de galerie
function ChooseTypeVisee(const T: integer): integer;
var
  TD: TdlgSelectTypeVisee;
begin
  Result := T;
  TD := TdlgSelectTypeVisee.Create(Application);
  try
    TD.SetTypeVisee(T);
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then Result := TD.GetTypeVisee();
  finally
    TD.Release;
  end;
end;

function ParametrerOngletVue2D(var O: TVue2DParams): boolean;
var
  TD: TfrmParametrerOngletVue2D;
begin
  Result := false;
  TD := TfrmParametrerOngletVue2D.Create(Application);
  try
    TD.SetModeParametrageDialog(mpdVUE2D);
    TD.SetValuesOnglet2D(O);
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then
    begin
      O := TD.GetValuesOnglet2D();
      result := True;
    end;
  finally
    TD.Release;
  end;
end;

function ParametrerOngletVue3D(var O: TVue3DParams): boolean;
var
  TD: TfrmParametrerOngletVue2D;
begin
  Result := false;
  TD := TfrmParametrerOngletVue2D.Create(Application);
  try
    TD.SetModeParametrageDialog(mpdVUE3D);
    TD.SetValuesOnglet3D(O);
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then
    begin
      O := TD.GetValuesOnglet3D();
      Result := True;
    end;
  finally
    TD.Release;
  end;
end;
function ParametrerOngletCoupeDeveloppee(var O: TCoupeDeveloppeeParams): boolean;
var
  TD: TfrmParametrerOngletVue2D;
begin
  Result := false;
  TD := TfrmParametrerOngletVue2D.Create(Application);
  try
    TD.SetModeParametrageDialog(mpdCOUPE_DEVELOPPEE);
    TD.SetValuesCoupeDeveloppee(O);
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then
    begin
      O := TD.GetValuesCoupeDeveloppee();
      Result := True;
    end;
  finally
    TD.Release;
  end;
end;

function SelectAttributsTexte(const AT: TTexteAttributs): TTexteAttributs;
var
  TD: TfrmSelectTexteAttributs;
begin
  Result := AT;
  TD := TfrmSelectTexteAttributs.Create(Application);
  try
    TD.SetTexteAttributs(AT);
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then Result := TD.GetTexteAttributs;
  finally
    TD.Release;
  end;
end;


procedure DisplayHelpSystem(const Topics: string; const DoDisplaySandbox: boolean = false);
var
  s: string;
  TD: TfrmHelpSystem;
begin
  {$IFDEF FRENCH_MESSAGES}   s:= GetGHTopoDirectory() + 'HelpFile_fr.xml'; {$ENDIF}
  {$IFDEF ENGLISH_MESSAGES}  s:= GetGHTopoDirectory() + 'HelpFile_en.xml'; {$ENDIF}
  {$IFDEF SPANISH_MESSAGES}  s:= GetGHTopoDirectory() + 'HelpFile_de.xml'; {$ENDIF}
  {$IFDEF ITALIAN_MESSAGES}  s:= GetGHTopoDirectory() + 'HelpFile_it.xml'; {$ENDIF}
  {$IFDEF DEUTSCH_MESSAGES}  s:= GetGHTopoDirectory() + 'HelpFile_de.xml'; {$ENDIF}
  TD := TfrmHelpSystem.Create(Application);
  try
    if (not TD.Initialiser(s, Topics, DoDisplaySandbox)) then ShowMessage(Format(GetResourceString(rsMSG_FILENOTFOUND),[s]));
    TD.ShowModal;
  finally
    TD.Release;
  end;
end;

procedure AfficherInfosQRCodeStation(const FD: TBDDEntites; const ST: TBaseStation);
var
  TD: TdlgInfosQRStation;
begin
  TD := TdlgInfosQRStation.Create(Application);
  try
    TD.Initialise(FD, ST);
    TD.ShowModal;
  finally
    TD.Release;
  end;
end;

//******************************************************************************
// Capture d'écran
{$IFDEF MSWINDOWS}

procedure CapturerVueVersPNG(const QHandle: THandle; const QFileName: string);
var
  Bitmap: TBitmap; //TFPImageBitmap;
  DC    : HDC;
begin
  AfficherMessage('CapturerVueVersPNG');
  try
    Bitmap := TBitmap.Create;
    DC     := GetDC(QHandle); // Unité windows indispensable
    Bitmap.LoadFromDevice(DC);
    ReleaseDC(QHandle, DC);
    Bitmap.SaveToFile(QFileName);
  finally
    FreeAndNil(Bitmap);//Bitmap.Free;
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
procedure CapturerVueVersPNG(const QHandle: THandle; const QFileName: string);
begin
  ;
end;
{$ENDIF}
// Enregistrement de fichiers
function DoDialogSaveFile(const QFilter: string;
                          const QDefaultExt: string;
                          var QFileName: TStringDirectoryFilename;
                          out idxFilter: integer): boolean;
var
  TD: TSaveDialog;
begin
  Result := False;
  TD := TSaveDialog.Create(nil);
  try
    TD.Options      := TD.Options + [ofOverWritePrompt];
    TD.InitialDir   := ExtractFilePath(QFileName);
    TD.FileName     := ExtractFileName(QFileName);
    TD.Filter       := QFilter;
    TD.DefaultExt   := QDefaultExt;
    if (TD.Execute) then
    begin
      QFileName := TD.FileName;
      idxFilter := TD.FilterIndex;
      Result := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;
// Ouverture de fichiers
function DoDialogOpenFile(const QFilter: string; const QDefaultExt: string; var QFileName: TStringDirectoryFilename): boolean;
var
  TD: TOpenDialog;
begin
  Result := False;
  TD := TOpenDialog.Create(Application);
  try
    TD.FileName   := QFileName;
    TD.InitialDir := GetGHTopoDirectory;
    TD.Filter     := QFilter;
    TD.DefaultExt := QDefaultExt;
    if (TD.Execute) then
    begin
      QFileName := TD.FileName;
      Result    := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;
function DoDialogOpenFile(const QInitialdir:TStringDirectoryFilename;
                          const QFilter: string;
                          const QDefaultExt:
                          string; var QFileName: TStringDirectoryFilename): boolean; overload;
var
  TD: TOpenDialog;
begin
  Result := False;
  TD := TOpenDialog.Create(Application);
  try
    TD.FileName   := QFileName;
    TD.InitialDir := QInitialdir;
    TD.Filter     := QFilter;
    TD.DefaultExt := QDefaultExt;
    if (TD.Execute) then
    begin
      QFileName := TD.FileName;
      Result    := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;
// nouveau dialogue d'ouverture de fichiers GHTopo
function DoDialogOpenFileGHTopo(const AllowQuitGHTopo: boolean;
                                const QListeRecents: TStringList;
                                var QFileName: TStringDirectoryFilename): boolean; overload;
var
  TD: TdlgSelectGHTopoFiles;
begin
  Result := False;
  TD := TdlgSelectGHTopoFiles.Create(Application);
  try
    if (TD.Initialiser(AllowQuitGHTopo, QListeRecents)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        QFileName := TD.GetFileName();
        exit(True);
      end;
    end;

  finally
    TD.Finaliser();
    FreeAndNil(TD);
  end;
end;

// dialogue simplifié d'ouverture de fichiers GHTopo
function DoDialogOpenFileGHTopoSimplifie(out QFileName: TStringDirectoryFilename): boolean;
var
  TD: TdlgSelectGHTopoFilesSimplifie;
begin
  Result := False;
  TD := TdlgSelectGHTopoFilesSimplifie.Create(nil);
  try
    if (TD.Initialiser()) then
    begin
      AfficherMessage('*** 006');
      TD.ShowModal;
      AfficherMessage('*** 007');
      if (TD.ModalResult = mrOK) then
      begin
        QFileName := TD.GetFileName();
        exit(True);
      end;
    end;

  finally
    TD.Finaliser();
    FreeAndNil(TD); // TD.Release;
  end;
end;



// Editeur de coupe développée
{$IFDEF USE_COUPES_DEVELOPPEES}
procedure AfficherEditeurCoupeDeveloppee(const FT: TToporobotStructure2012);
var

  TD: TfrmCoupeDeveloppee;
begin
  {$IFDEF USE_COUPES_DEVELOPPEES}
  TD := TfrmCoupeDeveloppee.Create(Application);
  try
    TD.Initialiser(FT);
    TD.ShowModal;
  finally
    TD.Release;
  end;
  //*)
  {$ELSE}
  ShowMessage('Fonctionnalité non disponible dans cette version' + #13#10 +
              '(adaptations en cours)');
  {$ENDIF}
end;
{$ENDIF USE_COUPES_DEVELOPPEES}




function GHTopoInputQuery(const ACaption, APrompt: string; var QValeur: string): boolean;
begin
  {$IFDEF GHTOPO_SIMPLIFIE}
   //{$IFDEF RASPBERRY_PI}
     Result := DisplayClavierVirtuel(ACaption, APrompt, QValeur);
   // {$ELSE}
   //   Result := InputQuery(ACaption, APrompt, QValeur);
   //{$ENDIF RASPBERRY_PI}
 {$ELSE}
    Result := InputQuery(ACaption, APrompt, QValeur);
 {$ENDIF GHTOPO_SIMPLIFIE}

end;

function GHTopoInputPasswordQuery(const ACaption, APrompt: string; var QPWD: string): boolean;
begin
  Result := InputQuery(ACaption, APrompt, True, QPWD);
end;

// choisir une station plus proche d'une station donnée
function ChooseNearStationAtPoint(const TD: TToporobotStructure2012;
                                  const BTC: TBDDEntites;
                                  const QNumeroSerie  : TNumeroSerie;
                                  const QNumeroStation: integer;
                                  const RayonDeCapture: double;
                                  out   EntiteSelected: TBaseStation): boolean;
var
  EWE: TBaseStation;
  BB : TfrmListeNearViseesTo;
  QIDTerrainStation: String;
  QMaSerie: TNumeroSerie;
  QMaStation: integer;
begin
  Result     := false;
  QMaSerie   := QNumeroSerie;
  QMaStation := QNumeroStation;
  (*
  ShowMessage('Passe dans ChooseNearStationAtPoint');
  // d'abord, on recherche si la station (S, P) existe
  if (not BTC.GetEntiteViseeFromSerSt(QNumeroSerie, QNumeroStation, EWE)) then
  begin
    QIDTerrainStation := ''; // On demande un IDterrain ou un couple
    if (InputQuery('Rechercher une station', 'ID station', QIDTerrainStation)) then
    begin
      if (Not TD.FindPtTopoByCle(false, QIDTerrainStation, QMaSerie, QMaStation)) then
      begin
        ShowMessageFmt('Station "%s" introuvable', [QIDTerrainStation]);
        Exit;
      end;
      ShowMessage('On saute à ligne 920');
    end
    else   //  sinon, on sort
    begin
      Exit;
    end;
  end;
  // ensuite on crée le dialogue
  //*)
  BB := TfrmListeNearViseesTo.Create(Application);
  try
    if (BB.Initialiser(TD, BTC, QMaSerie, QMaStation, RayonDeCapture, True)) then
    begin
      BB.ShowModal;
      if (BB.ModalResult = mrOK) then
      begin
        EntiteSelected := BB.GetResultEntite;
        if ((EntiteSelected.Entite_Serie = -1) and (EntiteSelected.Entite_Station = -1)) then Exit;
        Result := True;
      end;
      BB.Finaliser();
    end;
  finally
    BB.Release;
  end;
end;
// liste les stations les plus proches d'une station donnée
function DisplayNearStationsAtPoint(const TD: TToporobotStructure2012;
                                    const BTC: TBDDEntites;
                                    const QNumeroSerie: TNumeroSerie;
                                    const QNumeroStation: integer;
                                    const RayonDeCapture: double): boolean;
var
  EWE: TBaseStation;
  BB : TfrmListeNearViseesTo;
begin
  Result := false;
  if (not BTC.GetEntiteViseeFromSerSt(QNumeroSerie, QNumeroStation, EWE)) then
  begin
    ShowMessageFmt('Station %d.%d introuvable', [QNumeroSerie, QNumeroStation]);
    Exit;
  end;
  // ensuite on crée le dialogue
  BB := TfrmListeNearViseesTo.Create(Application);
  try
    if (BB.Initialiser(TD, BTC, EWE.Entite_Serie, EWE.Entite_Station, RayonDeCapture, false)) then
    begin
      BB.ShowModal;
      BB.Finaliser();
    end;
  finally
    BB.Release;
  end;
end;

function SelectIDTerrain(const DC: TToporobotStructure2012; var QIDxTerrain: integer): boolean;
var
  BB: TdlgSelectIDTerrain;
begin
  Result := False;
  BB := TdlgSelectIDTerrain.Create(Application);
  try
    if (BB.Initialiser(DC)) then
    begin
      BB.ShowModal;
      if (BB.ModalResult = mrOK) then
      begin
        QIDxTerrain := BB.GetIDXTerrainFound;
        Result := True;
      end;
    end;
  finally
    BB.Release;
  end;
end;


function DispDlgFindIDTerrain(const QDC: TToporobotStructure2012;
                              out   QDoMatchExact: boolean;
                              out   QFindWhat: string): boolean;
var
  BB: TdlgFindAStation;
begin
  Result := False;
  BB := TdlgFindAStation.Create(Application);
  try
    if (BB.Initialiser(QDC)) then
    begin
      BB.ShowModal;
      if (BB.ModalResult = mrOK) then
      begin
        QFindWhat := BB.GetFindWhat();
        QDoMatchExact := BB.GetDoMatchExact();
        //ShowMessage(QFindWhat + ' - ' + BoolToStr(QDoMatchExact, 'Recherche exacte', 'Rechercher une partie'));
        Result := True;
      end;
    end;
  finally
    BB.Release;
  end;
end;


//------------------------------------------------------------------------------
function EditerEntrance(const FD: TToporobotStructure2012;
                        const QIdx: integer;
                        const B: TBDDEntites;
                        const M: TMaillage;
                        var   EE: TEntrance): boolean;
var
  QDlg: TdlgEditEntrance;
begin
  Result := False;
  try
    QDlg := TdlgEditEntrance.Create(Application);
    QDlg.PrepareDialog(FD, QIdx, EE, 'Entrée', B, M);
    QDlg.ShowModal;
    if (QDlg.ModalResult = mrOK) then
    begin
      EE := QDlg.GetAEntrance();
      Result := True;
    end;
  finally
    QDlg.Release;
  end;
end;
function EditerReseau(const FD: TToporobotStructure2012; const QIdx: integer; var RS: TReseau): boolean;
var
  QDlg: TdlgEditReseau;
begin
  Result := False;
  try
    QDlg := TdlgEditReseau.Create(Application);
    QDlg.PrepareDialog(FD, QIdx, RS, 'Réseau');
    QDlg.ShowModal;
    if (QDlg.ModalResult = mrOK) then
    begin
      RS := QDlg.GetAReseau();
      Result := True;
    end;
  finally
    QDlg.Release;
  end;
end;

function EditerSecteur(const FD: TToporobotStructure2012; const QIdx: integer; var SS: TSecteur): boolean;
var
  QDlg: TdlgEditSecteur;
begin
  Result := False;
  try
    QDlg := TdlgEditSecteur.Create(Application);
    QDlg.PrepareDialog(FD, QIdx, SS, 'Secteur');
    QDlg.ShowModal;
    if (QDlg.ModalResult = mrOK) then
    begin
      SS := QDlg.GetASecteur();
      Result := True;
    end;
  finally
    QDlg.Release;
  end;
end;

function EditerCode(const FD: TToporobotStructure2012; const QIdx: integer; var CC: TCode): boolean;
var
  QDlg: TdlgEditCode;
begin
  Result := False;
  if (QIdx < 0) then Exit;
  try
    QDlg := TdlgEditCode.Create(Application);
    QDlg.PrepareDialog(FD, QIdx, CC, 'Instruments topo');
    QDlg.ShowModal;
    if (QDlg.ModalResult = mrOK) then
    begin
      CC := QDlg.GetACode();
      Result := True;
    end;
  finally
    QDlg.Release;
  end;
end;

function EditerExpe(const FD: TToporobotStructure2012; const QIdx: integer; var EX: TExpe): boolean;
var
  QDlg: TdlgEditExpe;
begin
  Result := False;
  if (QIdx < 0) then Exit;
  try
    QDlg := TdlgEditExpe.Create(Application);

    QDlg.PrepareDialog(FD, QIdx, EX, 'Séance topo');
    QDlg.ShowModal;
    if (QDlg.ModalResult = mrOK) then
    begin
      EX := QDlg.GetAExpe();
      Result := True;
    end;
  finally
    QDlg.Release;
  end;
end;
// édition d'une série isolée
function DisplayUneSerie(const FD: TToporobotStructure2012;
                         const PD: TBDDEntites;
                         const QIdx: integer;
                         var   SR: TObjSerie): boolean;
var
  QDlg: TdlgEditSerie;
begin
  Result := False;
  QDlg := TdlgEditSerie.Create(Application);
  try
    QDlg.PrepareDialog(FD, PD, SR, QIdx);
    QDlg.ShowModal;
    Result := QDlg.DoPerform();
  finally
    FreeAndNil(QDlg);
  end;
end;

function EditerAnnotation(const FC: TCroquisTerrain; const QIdx: integer; var AN: TKrobardAnnotation): boolean;
var
  QDlg: TdlgEditAnnotation;
begin
  Result := False;
  try
    QDlg := TdlgEditAnnotation.Create(Application);
    QDlg.PrepareDialog(FC, AN);
    QDlg.ShowModal;
    if (QDlg.ModalResult = mrOK) then
    begin
      AN := QDlg.GetAnnotation();
      Result := (Trim(AN.Texte) <> ''); // avant de valider, on vérifie si le texte n'est pas vide
    end;
  finally
    QDlg.Release;
  end;

end;

//------------------------------------------------------------------------------
// recopie vers le bas
procedure GridUndocopy(const MyGrid: TStringGrid; const AutoIncremented: boolean);
var
  i, j, Nb: integer;
  WU: integer;
  EWE: string;
begin
  Nb := MyGrid.Selection.Bottom - MyGrid.Selection.Top;
  if (Nb = 0) then
  begin
    EWE  := Format(FORMAT_NB_INTEGER, [MyGrid.RowCount - 1]);
    if (GHTopoInputQuery('Recopier vers le bas', 'Nombre de lignes', EWE)) then
      Nb := StrToIntDef(EWE, MyGrid.RowCount - 1)
    else
      Exit;
  end;
  WU := MyGrid.Selection.Top + Nb;
  if (WU > (MyGrid.RowCount -1)) then WU := MyGrid.RowCount - 1;
  for i := MyGrid.Selection.Top + 1 to WU do
  begin
    for j := MyGrid.Selection.Left to MyGrid.Selection.Right do
      MyGrid.Cells[j, i] := MyGrid.Cells[j, MyGrid.Selection.Top];
  end;
end;
// effacement de lignes
procedure GridEraseLines(const MyGrid: TStringGrid);
var
  Nb, WU, i, L: LongInt;
begin
  L := MyGrid.Selection.Top;
  Nb := MyGrid.Selection.Bottom - L;
  // Demande de confirmation
  if (not GHTopoQuestionOuiNon(Format(GetResourceString(rsCDR_SERIES_FLAT_MSG_REMOVE_LINE), [Nb + 1, L]))) then Exit;
  WU := L + Nb;
  if (WU > (MyGrid.RowCount -1)) then WU := MyGrid.RowCount - 1;
  for i := WU downto L do  MyGrid.DeleteRow(i);
end;

procedure GridInsertLignes(const MyGrid: TStringGrid);
begin
  MyGrid.InsertColRow(false, MyGrid.Row);
end;
procedure GridSetCurrentRow(const MyGrid: TStringGrid; const R, C: integer);
begin
  MyGrid.Row    := R;
  MyGrid.TopRow := R;
  if (c > 0) then MyGrid.Col := c;
  MyGrid.SetFocus;
end;

function SelectTopographes(out Topographes: string): boolean;
var
  TD: TdlgSelectTopographes;
begin
  Topographes := '';
  Result := false;
  TD := TdlgSelectTopographes.Create(Application);
  try
    TD.Initialise();
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then
    begin
      Topographes := TD.GetTopographe();
      Result := True;
    end;
  finally
    TD.Release;
  end;
end;



// fusionner des topographies
procedure DispFusionTopographies(const FD: TToporobotStructure2012;
                                 const CV: TConversionSysteme);
var
  TD: TdlgFusionTopos;
begin
  TD := TdlgFusionTopos.Create(Application);
  try
    if (TD.Initialiser(FD, CV)) then
    begin
      TD.ShowModal;
    end;
  finally
    TD.Release;
  end;
end;


function SelectionnerImprimante(var MyPrinter: TPrinter): boolean;
var
  PC: TPrinterSetupDialog;
begin
  result := false;
  PC := TPrinterSetupDialog.Create(Application);
  try
    result := PC.Execute;
  finally
    FreeAndNil(PC);//PC.Free;
  end;
end;

// choix de code, d'expé, secteur
function SelectionnerSecteurCodeExpe(const FD: TToporobotStructure2012;
                                     const MyRow: integer;
                                     const DoUseLastItems: boolean;
                                     var MyIdxSecteur: integer;
                                     var MyIdxCode: TNumeroCode;
                                     var MyIdxExpe: TNumeroExpe): boolean;
var
  TD: TdlgSelectSecteurCodeExpe;
begin
  result := false;
  TD := TdlgSelectSecteurCodeExpe.Create(Application);
  try
    if (TD.Initialiser(FD, MyRow, MyIdxSecteur, MyIdxCode, MyIdxExpe, DoUseLastItems)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        MyIdxSecteur := TD.GetIdxSecteur();
        MyIdxCode    := TD.GetIdxCode();
        MyIdxExpe    := TD.GetIdxExpe();
        Result := True;
      end;
    end;
  finally
    TD.Release;
  end;
end;

// gestion des visées rayonnantes
function DispGestionDesViseesRayonnantes(const FD: TToporobotStructure2012;
                                         const ED: TBDDEntites): boolean;
var
  TD: TfrmGestionViseesRayonnantes;
begin
  Result := false;
  TD := TfrmGestionViseesRayonnantes.Create(Application);
  try
    if (TD.Initialiser(FD, ED)) then TD.ShowModal;
  finally
    TD.Release;
  end;
end;

// lister les dépendances d'une série
function ListerDependancesDeUneSerie(const FD: TToporobotStructure2012;
                                     const IdxSerie: integer): boolean;
var
  TD: TdlgDependancesSerie;
begin
  result := false;
  TD := TdlgDependancesSerie.Create(application);
  try
    if (TD.Initialiser(FD, IdxSerie)) then
    begin
      TD.ShowModal;
      Result := (TD.ModalResult = mrYes);
    end;
  finally
    TD.Release;
  end;
end;

// maintenance du document
procedure DispMaintenanceDocument(const FD: TToporobotStructure2012;
                                  const FE: TBDDEntites;
                                  const FC: TConversionSysteme;
                                  const PostAction: TProcOfObjectWithOneBoolParameter);
var
  TD: TdlgMaintenanceDocTopo;
begin
  TD := TdlgMaintenanceDocTopo.Create(Application);
  try
    if (TD.Initialiser(FD, FE, FC)) then
    begin
      TD.ShowModal;
      if (Assigned(PostAction) and TD.DoPostAction(false)) then PostAction(false);
    end;
  finally
    TD.Release;
  end;
end;
// affichage de la distance entre deux stations
function DispDistanceEntreStations(const FD: TToporobotStructure2012;
                                   const FE: TBDDEntites;
                                   var   S1, S2: TBaseStation;
                                   const DescriptionAction: string;
                                   const CaptionAdditionalAction: string): integer;
var
  TD: TdlgDistanceBetweenTwoStations;
begin
  result := 0;
  TD := TdlgDistanceBetweenTwoStations.Create(Application);
  try
    if (TD.Initialiser(FD, FE, S1, S2, DescriptionAction, CaptionAdditionalAction)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        Result += 1;
        S1 := TD.GetStation1();
        S2 := TD.GetStation2();
        if (TD.DoAllowAdditionalAction()) then Result += 1;
      end;
    end;
  finally
    TD.Release;
  end;
end;

// affichage d'un profil topo
function DisplayProfilTopo(const DT: TToporobotStructure2012; const QMaillage: TMaillage; var ProfilTopo: TProfilTopo): TModalResult;
var
  TD: TdlgProfilTopo;
begin
  TD := TdlgProfilTopo.Create(Application);
  try
    if (TD.Initialiser(DT, QMaillage, ProfilTopo)) then
    begin
      TD.ShowModal;
      Result := TD.ModalResult;
      if (Result = mrOK) then
      begin
        ProfilTopo := TD.GetProfilTopo(); // récupère le profil topo pour actualisation du plan
      end;
      TD.Finaliser();
    end;
  finally
    TD.Release;
  end;
end;

// gestion des filtres
function DisplayAndEditFiltres(const FD: TToporobotStructure2012; var F: string): boolean;
var
  TD: TdlgEditFiltres;
  WU: Boolean;
begin
  result := false;
  {$IFDEF RASPBERRY_PI}
  Result := (GHTopoInputQuery('Filtres', 'Filtre', F));
  {$ELSE}
  TD := TdlgEditFiltres.Create(Application);
  try
    WU := ('' <> Trim(F));
    if (TD.Initialiser(FD, F, WU)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        F := TD.GetFiltre();
        Result := True;
      end;
    end;
  finally
    TD.Release;
  end;
  {$ENDIF RASPBERRY_PI}
end;



// afficher les profils topo
function DisplayListeProfils(const DT: TToporobotStructure2012; const MyMaillage: TMaillage): boolean;
var
  TD: TdlgLesProfilsTopo;
begin
  result := false;
  //if (MyMaillage.GetNbProfilsTopo() = 0) then
  //begin
  //  ShowMessage('Aucun profil topographique');
  //  exit;
  //end;
  TD := TdlgLesProfilsTopo.Create(Application);
  try
    if (TD.Initialiser(DT, MyMaillage)) then
    begin
      TD.ShowModal;
      TD.Finaliser();
      result := true;
    end;
  finally
    TD.Release;
  end;
end;

function DisplayEditerPOI(const DT: TToporobotStructure2012; var MyPOI: TPointOfInterest): boolean;
var
  TD: TfrmEditerPOI;
begin
  result := false;
  TD := TfrmEditerPOI.Create(Application);
  try
    if (TD.Initialiser(DT, MyPOI)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        MyPOI:= TD.getPOI();
        result := true;
      end;
    end;
  finally
    TD.Release;
  end;
end;

// sélection d'un système de coordonnées
function SelectCoordinatesSystem(const FC: TConversionSysteme; var SC: TLabelSystemesCoordsEPSG): boolean;
var
  TD: TdlgSelectionCoordsSystem;
begin
  result := false;
  TD :=  TdlgSelectionCoordsSystem.Create(Application);
  try
    if (TD.Initialiser(FC, SC)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        SC := TD.GetSelectedEPSGSysCoords();
        result := true;
      end;
    end;
  finally
    TD.Release;
  end;
end;





// editer une couche GIS
function EditerGisLayer(var L: TGISLayer): boolean;
var
  QDlg: TdlgEditGisLayer;
begin
  Result := False;
  try
    QDlg := TdlgEditGisLayer.Create(Application);
    if (Qdlg.Initialiser(L)) then
    begin
      QDlg.ShowModal;
      if (QDlg.ModalResult = mrOK) then
      begin
        L := QDlg.GetGisLayer();
        result := True;
      end;
    end;
  finally
    FreeAndNil(QDlg);
  end;
end;

// afficher un graphe de réseau
function DisplayGrapheOfReseau(const B: TBDDEntites; const G: TPathFindingGraphe): boolean;
var
  TD: TdlgDisplayGraphe;
begin
  TD := TdlgDisplayGraphe.Create(Application);
  begin
    try
      if (TD.Initialiser(B, G)) then TD.ShowModal;
      TD.Finaliser();
      result := True;
    finally
      FreeAndNil(TD);
    end;
  end;
end;
//******************************************************************************
// Spécifique Raspberry
//******************************************************************************


{$IFDEF GHTOPO_SIMPLIFIE}

function DisplayClavierVirtuel(const ACaption, APrompt: string; var AValeur: string): boolean;
var
  CV: TdlgClavierVirtuel;
begin
  result := false;
  CV := TdlgClavierVirtuel.Create(Application);
  try
    if (CV.Initialiser(ACaption, APrompt, AValeur)) then
    begin
      CV.ShowModal;
      if (CV.ModalResult = mrOK) then
      begin
        AValeur := CV.GetValue();
        result  := True;
      end;
    end;
  finally
    FreeAndNil(CV);
  end;
end;
// import depuis un autre document
{$ELSE}
function DisplayImportFromOtherDocumentGHTopo(const FD: TToporobotStructure2012; const FC: TConversionSysteme): boolean;
var
  TD: TdlgAddSeriesFromOtherDoc;
begin
  TD := TdlgAddSeriesFromOtherDoc.Create(Application);
  try
    if (TD.Initialiser(FD, FC)) then
    begin
      TD.ShowModal;
      TD.Finaliser();
    end;
  finally
    FreeAndNil(TD);
  end;
end;
{$ENDIF GHTOPO_SIMPLIFIE}



// Spécifique Chine: formulaire d'enregistrement
procedure RegisterChineseUser();
var
  TD: TdlgRegisterChineseUser;
begin
  TD := TdlgRegisterChineseUser.Create(Application);
  try
    if (TD.Initialiser()) then TD.ShowModal;
  finally
    FreeAndNil(TD);
  end;
end;
function DisplayClavierNumerique(const Title: string; const ModeSaisie: TPaveNumModeSaisie; var Value: string): boolean;
var
  DG: TdlgClavierNumerique;
begin
  result := false;
  DG := TdlgClavierNumerique.create(Application);
  try
    DG.Initialiser(Title, ModeSaisie, Value);
    DG.ShowModal;
    if (DG.ModalResult = mrOK) then
    begin
      Value  := DG.AsString();
      result := true;
    end;
  finally
    DG.Release;
  end;
end;

// Saisie de ID stations
function SaisirIDStation(const Msg: string; var S: string): boolean;
begin
  {$IFDEF RASPBERRY_PI}
  Result := InputQuery(Msg, 'ID:', S);
  {$ELSE}
  Result := DisplayClavierNumerique(Msg, pnmTOPOROBOT_STATION, S);//InputQuery(Msg, 'ID:', S);
  {$ENDIF RASPBERRY_PI}
end;

// Recherche dans la base
function SearchInGHTopoDatabase(const FD: TToporobotStructure2012; const FE: TBDDEntites; const QFindWhat: string; const QPerformAction: TProcedureOfObject): boolean;
var
  TD: TdlgSearchInDatabase;
begin
  result := false;
  TD := TdlgSearchInDatabase.Create(Application);
  try
    if (TD.Initialiser(FD, FE, QFindWhat)) then
    begin
      TD.ShowModal;
    end;
  finally
    FreeAndNil(TD);
  end;

end;

end.

