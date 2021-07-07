unit CadreSerieIndependant;
{$INCLUDE CompilationParameters.inc}
// Date: 24/08/2012
// Version n'utilisant pas TToporobotstructure sauf pour les contrôles
// Statut: En cours de portage
// 18/06/2013: Gestion des déplacements dans la grille OK
// 19/06/2013: Contrôles de validité
// 22/10/2013: Bug sur Champ 'Observations' fixé
// 06/02/2014: Remplacement de l'évaluateur d'expressions
// 29/10/2014: Mise à 3 décimales des valeurs L, Az, P (DistoX)
// 14/11/2014: Support du DistoX (mode attente) --> Supprimé au 28/12/2016
// 02/06/2015: Nouvelle interface de check des données
// 04/06/2015: Un changement de numéro de série met à jour les noeuds d'accrochage des séries qui lui sont rattachées.
// 22/02/2015: Adaptation de l'interface aux grands numéros de série - Centralisation du paramétrage des largeurs de colonnes
// 23/07/2016: Revue du code - Recopie vers le bas 'Excel like'
// 28/12/2016: Refonte complète de la liste d'actions, des constantes, des boutons et des menus.
//             Refonte du copier/coller
// DONE      : Lorsqu'on quitte la série, proposer un avertissement 'data unsaved' si données modifiées
//    Propale: On calcule un condensat SHA1 des données au chargement du cadre et on le sauve dans FCondensat
//             La procédure de changement de série appelle la fonction IsDataModified() qui calcule
//             un nouveau condensat et le compare avec celui contenu dans FCondensat
// 13/06/2019: Point de contrôle temporel (contrôle de version)
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  ToporobotClasses2012,   // pour contrôle de validité des codes et expés
  UnitEntitesExtended,    // pour recherche de stations voisines
  unitobjetserie,
  {$IFDEF MSWINDOWS}
  //unitDistoXWindows,
  {$ENDIF}
  {$IFDEF LINUX}
  //unitDistoXLinux,
  {$ENDIF}
  evaluateur_expressions,
  //unitUtilsComposants,
  Common,
  CallDialogsStdVersion,
  Classes, SysUtils, FileUtil, Forms, Dialogs, Controls, ExtCtrls, Grids, Graphics,
  StdCtrls, ActnList, Menus, curredit, LCLType,
  Clipbrd, Buttons,
  types;
type

  { TCdrSerieIndependant }

  TCdrSerieIndependant = class(TFrame)
    acAddLine: TAction;
    acEraseLine: TAction;
    acReplierTableur: TAction;
    acPasteFromClipboard: TAction;
    acCopierSelection: TAction;
    acChooseReseau: TAction;
    acChooseCode: TAction;
    acChooseSecteur: TAction;
    acChooseExpe: TAction;
    acChooseTypeVisee: TAction;
    acOuvrirDistoX: TAction;
    acExtractIDTerrain: TAction;
    acCopierLeTableau: TAction;
    acUndoCopyCurrentCodeExpe: TAction;
    acUndoCopy: TAction;
    btnSelectReseau: TButton;
    btnAttribuerExtremiteLibre: TButton;
    Button2: TButton;
    btnCloseErrWnd: TButton;
    CdrSerieActionList: TActionList;
    btnGetSerStDepartByID: TButton;
    btnGetSerStArriveeByID: TButton;
    btnShowHideHeader: TButton;
    btnselectEntrance: TButton;
    cmbChance: TComboBox;
    cmbObstacle: TComboBox;
		cmbPropalesRaideur: TComboBox;
    editLG0: TCurrencyEdit;
    editLD0: TCurrencyEdit;
    editHZ0: TCurrencyEdit;
    editHN0: TCurrencyEdit;
    editNumEntreeRattachement: TCurrencyEdit;
    editNumeroReseau: TCurrencyEdit;
    editRaideur: TCurrencyEdit;
    editCommentaire: TEdit;
    editPointArrivee: TCurrencyEdit;
    editSerieDepart: TCurrencyEdit;
    editNomSerie: TEdit;
    editSerieArrivee: TCurrencyEdit;
    editPointDepart: TCurrencyEdit;
    editNumeroSerie: TCurrencyEdit;
    CdrSerieImgList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    lbIdxEntree: TLabel;
    lbInternalNumSerie: TStaticText;
    lblReseau: TLabel;
    lbNomEntree: TStaticText;
    lbLongueurCumulee: TStaticText;
    lbNbStations: TLabel;
    lblChances: TLabel;
    lblObstacles: TLabel;
    lblSerie: TLabel;
    lblSerieName: TLabel;
    lblDepart: TLabel;
    lblArrivee: TLabel;
    lbNomReseau: TStaticText;
    lsbErreurs: TListBox;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    mnuChooseReseauSecteurCodeExpe: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pnlReseaux: TPanel;
    pnlEnteteSerie: TPanel;
    pnlErreursACorriger: TPanel;
    pnlTableur: TPanel;
    grdStations: TStringGrid;
    CdrSeriePopUp: TPopupMenu;
    lbMessages: TStaticText;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    lbDataAreModified: TStaticText;
    procedure acAddLineExecute(Sender: TObject);
    procedure acChooseCodeExecute(Sender: TObject);
    procedure acChooseExpeExecute(Sender: TObject);
    procedure acChooseSecteurExecute(Sender: TObject);
    procedure acChooseTypeViseeExecute(Sender: TObject);
    procedure acCopierLeTableauExecute(Sender: TObject);
    procedure acCopierSelectionExecute(Sender: TObject);
    procedure acCreerViseeDepuisDistoXExecute(Sender: TObject);
    procedure acEraseLineExecute(Sender: TObject);
    procedure acExtractIDTerrainExecute(Sender: TObject);
    procedure acPasteFromClipboardExecute(Sender: TObject);
    procedure acReplierTableurExecute(Sender: TObject);
    procedure acChooseReseauExecute(Sender: TObject);
    procedure acUndoCopyCurrentCodeExpeExecute(Sender: TObject);
    procedure acUndoCopyExecute(Sender: TObject);
    procedure btnAttribuerExtremiteLibreClick(Sender: TObject);
    procedure btnCloseErrWndClick(Sender: TObject);

    procedure btnCopyGridClick(Sender: TObject);
    procedure btnGetSerStDepartByIDClick(Sender: TObject);
    procedure btnGetSerStArriveeByIDClick(Sender: TObject);
    procedure btnselectEntranceClick(Sender: TObject);
    procedure btnSelectReseauClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure chkLockedChange(Sender: TObject);
    procedure cmbPropalesRaideurChange(Sender: TObject);

    procedure editNumEntreeRattachementKeyPress(Sender: TObject; var Key: char);
    procedure editPointDepartExit(Sender: TObject);
    procedure ConfigurerGrdPopUp(const NumColonne: integer);
    procedure grdStationsClick(Sender: TObject);

    procedure grdStationsDblClick(Sender: TObject);
    procedure grdStationsGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure grdStationsHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure grdStationsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure grdStationsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);

    procedure lsbErreursClick(Sender: TObject);
  private
    { private declarations }
    FDocuToporobot    : TToporobotStructure2012;
    FCurrentSerie     : TObjSerie;   // /!\ FCurrentSerie est un pointeur.
    FBDDEntites       : TBDDEntites;
    FCondensat        : string;   // condensat des données
    FRowCourante      : integer;
    FColCourante      : integer;
    FCurrentIdxSerie  : integer;

    FEnteteSerieDeplie: boolean;
    FLastItemFound    : integer;
    FLastTextToFind   : string;
    // ancienne valeur du numéro de série
    FOldNumeroserie: integer;
    function  CalcCondensat(): string;
    procedure AttribuerExtremiteLibre();
    procedure DisplayHint(const MyCtrl: TControl; const Msg: string);
    function  GridCheckErrorInGrdStations(const CurrColonne, CurrLigne: integer; const QValeur: double): integer;
    procedure InsertGrdLines(const N: integer);
    procedure RechercherStation(const QEditSer, QEditPt: TCurrencyEdit);
    procedure AfficherEnteteComplet(const P: boolean);
    procedure InitCadre();
    procedure InitCaptions();
    function  IsEmptyRow(const IdxRow: integer): boolean;

  public
    { public declarations }
    procedure Initialise(const DT: TToporobotStructure2012;
                         const PD: TBDDEntites;
                         const QS: TObjSerie;
                         const QIdx: integer);
                         //const DX: TPilotageDistoX);
    function  GetCurrentSerie(): TObjSerie;
    procedure RefreshTableaux();
    function  ImplementerModifs(out QNbErreurs: integer): boolean;
    function  IsDataModified(): boolean;


  end;

implementation
uses
  DGCDummyUnit;
const
  GRD_STATIONS_NB_COLONNES   = 17;
  WDTH_COL_0         = 64;
  WDTH_COL_LRUD = 50;
  NUM_COL_IDTERRAIN   = 1;      WDTH_COL_IDTERRAIN   = 100;
  NUM_COL_SECTEUR     = 2;      WDTH_COL_SECTEUR     = 50;
  NUM_COL_TYPEVISEE   = 3;      WDTH_COL_TYPEVISEE   = 20;
  NUM_COL_CODE        = 4;      WDTH_COL_CODE        = 36;
  NUM_COL_EXPE        = 5;      WDTH_COL_EXPE        = WDTH_COL_CODE;
  NUM_COL_L           = 6;      WDTH_COL_L           = 68;
  NUM_COL_A           = 7;      WDTH_COL_AZ          = 60;
  NUM_COL_P           = 8;      WDTH_COL_P           = 60;
  NUM_COL_LG          = 9;      WDTH_COL_LG          = WDTH_COL_LRUD;
  NUM_COL_LD          = 10;     WDTH_COL_LD          = WDTH_COL_LRUD;
  NUM_COL_HZ          = 11;     WDTH_COL_HZ          = WDTH_COL_LRUD;
  NUM_COL_HN          = 12;     WDTH_COL_HN          = WDTH_COL_LRUD;
  NUM_COL_OBS         = 13;     WDTH_COL_OBS         = 300;
  NUM_COL_HORODATE    = 14;     WDTH_COL_HORODATE    = 200;
  NUM_COL_TEMPERATURE = 15;     WDTH_COL_TEMPERATURE = 50;
  NUM_COL_HUMIDITY    = 16;     WDTH_COL_HUMIDITY    = 50;

{$R *.lfm}
// Un TToporobotStructure2012 est indispensable pour l'appel
// des sélecteurs de Code et de Expe
procedure TCdrSerieIndependant.Initialise(const DT: TToporobotStructure2012;
                                          const PD: TBDDEntites;
                                          const QS: TObjSerie;
                                          const QIdx: integer);
var
  SR: TObjSerie;
  EE: TEntrance;
  N: TNumeroEntrance;
begin
  self.ParentFont    := True;
  pnlErreursACorriger.Visible := false;
  FDocuToporobot     := DT;
  FBDDEntites        := PD;
  FCurrentSerie      := QS;
  FCurrentIdxSerie   := QIdx;
  FOldNumeroserie    := QS.GetNumeroDeSerie();
  InitCaptions();
  InitCadre();
  RefreshTableaux();
  FCondensat := CalcCondensat();  // calcul du condensat des données initiales
  lbInternalNumSerie.Caption := format('%d / %d', [FCurrentIdxSerie, FDocuToporobot.GetNbSeries() - 1]);

end;

function TCdrSerieIndependant.GetCurrentSerie(): TObjSerie;
begin
  Result := FCurrentSerie;
end;


procedure TCdrSerieIndependant.grdStationsClick(Sender: TObject);
var
  L, EWE: double;
  i: Integer;
begin
  // un click dans la grille, même sans aucune modif de données,
  // présume une modif effectuée
  if (grdStations.Row > (grdStations.RowCount - 1)) then Exit;
  if (grdStations.Col > (grdStations.ColCount - 1)) then Exit;
  try
    FRowCourante := grdStations.Row;
    FColCourante := grdStations.Col;
    L := 0.00;
    // Configuration du pop-up
    ConfigurerGrdPopUp(grdStations.Col);
    // calcul de la longueur cumulée
    if (grdStations.Row > 0) then
    begin
      for i := 1 to FRowCourante do
      begin
        EWE := ConvertirEnNombreReel(grdStations.Cells[NUM_COL_L, i], 0.00);
        L := L + EWE;
      end;
    end;
    lbLongueurCumulee.Caption := Format('L%dC%d: %.2f m', [FRowCourante, FColCourante, L]);

  except
  end;
end;

procedure TCdrSerieIndependant.InitCaptions();
  procedure SetAcHint(const ACDC: TAction; const QCaption: string);
  begin
    ACDC.Caption := GetResourceString(QCaption);
    ACDC.Hint    := GetResourceString(QCaption);
  end;
begin
  FEnteteSerieDeplie := True;
  FLastItemFound     := 0;
  FLastTextToFind    := '';
  // caption des boutons
  SetAcHint(acAddLine           , rsCDR_SERIE_AC_ADD_LINE);
  SetAcHint(acEraseLine         , rsCDR_SERIE_AC_DEL_LINE);
  SetAcHint(acExtractIDTerrain  , rsCDR_SERIE_AC_EXTRACT_LABELS);

  SetAcHint(acCopierSelection   , rsCDR_SERIE_AC_GRD_SEL_COPY);
  SetAcHint(acCopierLeTableau   , rsBTN_COPIER_TABLEAU);
  SetAcHint(acPasteFromClipboard, rsCDR_SERIE_AC_GRD_PASTE);
  SetAcHint(acUndoCopy          , rsCDR_SERIE_AC_UNDOCOPY);

  SetAcHint(acChooseReseau      , rsCDR_SERIE_AC_SELECT_RESEAU);
  SetAcHint(acChooseSecteur     , rsCDR_SERIE_AC_SELECT_SECTEUR);
  SetAcHint(acChooseTypeVisee   , rsCDR_SERIE_AC_SELECT_TYPE_VISEE);
  SetAcHint(acChooseCode        , rsCDR_SERIE_AC_SELECT_CODE);
  SetAcHint(acChooseExpe        , rsCDR_SERIE_AC_SELECT_EXPE);
  SetAcHint(acUndoCopyCurrentCodeExpe, rsCDR_SERIE_UNDOCOPY_CODE_EXPE);

  lblSerie.Caption          := GetResourceString(rsCDR_SERIE_NUMERO);
  lblSerieName.Caption      := GetResourceString(rsCDR_SERIE_NAME);
  lblDepart.Caption         := GetResourceString(rsCDR_SERIE_DEPART);
  lblArrivee.Caption        := GetResourceString(rsCDR_SERIE_ARRIVEE);
  lblChances.Caption        := GetResourceString(rsCDR_SERIE_CHANCE);
  lblObstacles.Caption      := GetResourceString(rsCDR_SERIE_OBSTACLE);
  lblReseau.Caption         := GetResourceString(rsCDR_SERIE_LB_RESEAU);

  btnShowHideHeader.Hint    := GetResourceString(rsCDR_SERIE_SHOW_HIDE_HEADER);


  lbIdxEntree.Caption       := GetResourceString(rsCDR_SERIE_ENTREE_RATT);

  //pnlTypesVisees.Visible := false;
  with cmbChance do
  begin
    Clear;
    Items.Add(GetResourceString(rsCDR_SERIE_CHANCE0));
    Items.Add(GetResourceString(rsCDR_SERIE_CHANCE1));
    Items.Add(GetResourceString(rsCDR_SERIE_CHANCE2));
    Items.Add(GetResourceString(rsCDR_SERIE_CHANCE3));
    DropDownCount := cmbChance.Items.Count;
  end;
  with cmbObstacle do
  begin
    Clear;
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE0));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE1));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE2));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE3));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE4));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE5));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE6));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE7));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE8));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE9));
    // spécifique GHTopo ( équivalent à AUTRE pour Toporobot)
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE10));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE11));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE12));
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE13));
    // Sylvestre Clément a la phobie des grenouilles
    Items.Add(GetResourceString(rsCDR_SERIE_OBSTACLE14));

    DropDownCount := cmbObstacle.Items.Count;
  end;
end;

procedure TCdrSerieIndependant.InitCadre();
const
  LARGEUR_COLONNES_SECTEUR   = 30;
  NB_LINES = 500;
var
  i: integer;
  EWE, QCurrPosImg: integer;
  QNumeroDeSerie: TNumeroSerie;
  procedure MiouMiou(const Miou: boolean);
  begin
    pnlReseaux.Visible        := Miou;
    acChooseReseau.Enabled    := Miou;
    lblChances.Visible        := Miou;
    lblObstacles.Visible      := Miou;
    cmbChance.Visible         := Miou;
    cmbObstacle.Visible       := Miou;
  end;
  // Todo: Vue en plan: Lorsqu'on affiche les ID station, proposer le format suivant:
  //       <Etiquette station> [Couple serie_station]
  //     eg: A64 A65 [123.45]
  // TODO: Lorsqu'on choisit une entrée, proposer d'en extraire le couple
  // série.station si et seulement si:
  // if (MyEntree.eSerieEntree == MySerie.NumeroSerie)
  // begin
  //   extraire couple série.station et préremplir les cases correspondantes
  // end;
begin
  AfficherEnteteComplet(True);
  pnlErreursACorriger.Top := 0;
  grdStations.ColCount := GRD_STATIONS_NB_COLONNES;
  MiouMiou(true);
  acChooseSecteur.Enabled   := True;
  pnlErreursACorriger.top := 0;
  grdStations.FixedRows := 1;
  grdStations.Cells[0                   , 0] := GetResourceString(rsCDR_SERIE_COL_POINT);
  grdStations.Cells[NUM_COL_IDTERRAIN   , 0] := GetResourceString(rsCDR_SERIE_COL_ID_TERRAIN);
  grdStations.Cells[NUM_COL_SECTEUR     , 0] := GetResourceString(rsCDR_SERIE_COL_SECTEUR);
  grdStations.Cells[NUM_COL_TYPEVISEE   , 0] := GetResourceString(rsCDR_SERIE_COL_TYPE);
  grdStations.Cells[NUM_COL_CODE        , 0] := GetResourceString(rsCDR_SERIE_COL_CODE);
  grdStations.Cells[NUM_COL_EXPE        , 0] := GetResourceString(rsCDR_SERIE_COL_EXPE);

  grdStations.Cells[NUM_COL_L           , 0] := GetResourceString(rsCDR_SERIE_COL_LEGNTH);
  grdStations.Cells[NUM_COL_A           , 0] := GetResourceString(rsCDR_SERIE_COL_AZIMUTH);
  grdStations.Cells[NUM_COL_P           , 0] := GetResourceString(rsCDR_SERIE_COL_INCLIN);

  grdStations.Cells[NUM_COL_LG          , 0] := GetResourceString(rsCDR_SERIE_COL_LG);
  grdStations.Cells[NUM_COL_LD          , 0] := GetResourceString(rsCDR_SERIE_COL_LD);
  grdStations.Cells[NUM_COL_HZ          , 0] := GetResourceString(rsCDR_SERIE_COL_HZ);
  grdStations.Cells[NUM_COL_HN          , 0] := GetResourceString(rsCDR_SERIE_COL_HN);

  grdStations.Cells[NUM_COL_OBS         , 0] := GetResourceString(rsCDR_SERIE_COL_COMMENTAIRE);

  grdStations.Cells[NUM_COL_HORODATE    , 0] := GetResourceString(rsCDR_SERIE_COL_HORODATE);
  grdStations.Cells[NUM_COL_TEMPERATURE , 0] := GetResourceString(rsCDR_SERIE_COL_TEMPERATURE);
  grdStations.Cells[NUM_COL_HUMIDITY    , 0] := GetResourceString(rsCDR_SERIE_COL_HUMIDITY);

  with grdStations do
  begin
    cmbChance.ItemIndex := 0;
    cmbObstacle.ItemIndex := 0;
    RowCount := 1 + NB_LINES;
    EWE := FCurrentSerie.GetNbVisees;
    ColWidths[0] := WDTH_COL_0;
    ColWidths[NUM_COL_IDTERRAIN]   := WDTH_COL_IDTERRAIN;
    ColWidths[NUM_COL_SECTEUR]     := WDTH_COL_SECTEUR;
    ColWidths[NUM_COL_TYPEVISEE]   := WDTH_COL_TYPEVISEE;

    ColWidths[NUM_COL_CODE]        := WDTH_COL_CODE;
    ColWidths[NUM_COL_EXPE]        := WDTH_COL_EXPE;

    ColWidths[NUM_COL_L]           := WDTH_COL_L;
    ColWidths[NUM_COL_A]           := WDTH_COL_AZ;
    ColWidths[NUM_COL_P]           := WDTH_COL_P;

    for i := NUM_COL_LG to NUM_COL_HN     do ColWidths[i] := WDTH_COL_LRUD;

    ColWidths[NUM_COL_OBS]         := WDTH_COL_OBS;
    ColWidths[NUM_COL_HORODATE]    := WDTH_COL_HORODATE;
    ColWidths[NUM_COL_TEMPERATURE] := WDTH_COL_TEMPERATURE;
    ColWidths[NUM_COL_HUMIDITY]    := WDTH_COL_HUMIDITY;



    // [MODIF_ENTETE_TABLEUR]
    QNumeroDeSerie := FCurrentSerie.GetNumeroDeSerie();
    for i := 1 to RowCount - 1 do Cells[0, i] := Format(FMTSERST, [QNumeroDeSerie, i]);

    // Sous Delphi, utiliser l'option goAlwaysShowEditor
    Options := [goEditing,
                //goAlwaysShowEditor,
                goRangeSelect,
                goAutoAddRows,
                goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
                goRowHighlight,
                goCellHints,
                goColSizing,
                goThumbTracking,
                goTabs]; // ,goAlwaysShowEditor


    // vider les cases ID Terrain
    editPointDepart.Hint  := '';
    editPointArrivee.Hint := '';
    // se mettre en tête de tableau
    Row := 1;
    Col := 1;
    TopRow := Row;
  end;
  grdStations.ShowHint := True;
end;

procedure TCdrSerieIndependant.RefreshTableaux();
var
  i, nr, QNumReseau, QNbStations: integer;
  V: TUneVisee;
  WU: TNumeroEntrance;
  EWE: TEntrance;
  RR: TReseau;
begin
  // en-tête
  editNumeroSerie.AsInteger  := FCurrentSerie.GetNumeroDeSerie();
  editNomSerie.Text          := _AnsiToLCLStr(FCurrentSerie.GetNomSerie);
  editSerieDepart.AsInteger  := FCurrentSerie.GetNoSerieDep;
  editPointDepart.AsInteger  := FCurrentSerie.GetNoPointDep;
  editSerieArrivee.AsInteger := FCurrentSerie.GetNoSerieArr;
  editPointArrivee.AsInteger := FCurrentSerie.GetNoPointArr;
  editRaideur.Value          := FCurrentSerie.GetRaideur;
  cmbChance.ItemIndex        := FCurrentSerie.GetChance;
  cmbObstacle.ItemIndex      := FCurrentSerie.GetObstacle;
  editCommentaire.Text       := _AnsiToLCLStr(FCurrentSerie.GetObsSerie);
  // on attrappe la visée 0 pour les LRUD
  V := FCurrentSerie.GetVisee(0);
  editLG0.Value := V.LG;
  editLD0.Value := V.LD;
  editHZ0.Value := V.HZ;
  editHN0.Value := V.HN;

  QNumReseau := FCurrentSerie.GetNumeroReseau();
  if (FDocuToporobot.HasNumeroReseau(QNumReseau)) then
  begin
    nr := QNumReseau;
  end
  else
  begin
    ShowMessage(Format('Reseau inexistant: %d', [QNumReseau]));
    nr := 0;
  end;
  editNumeroReseau.AsInteger := nr;
  RR := FDocuToporobot.GetReseau(nr);
  lbNomReseau.Caption        := _AnsiToLCLStr(RR.NomReseau);
  // entrées
  WU := FCurrentSerie.GetNumeroEntrance();
  editNumEntreeRattachement.AsInteger := WU;
  try
    EWE := FDocuToporobot.GetEntrance(WU);
    lbNomEntree.Caption := EWE.eNomEntree;
  except
    lbNomEntree.Caption        := '----';
  end;

  // tableau des points
  lbNbStations.Caption := Format('%d stations', [FCurrentSerie.GetNbVisees]);
  for i := 1 to grdStations.RowCount - 1 do grdStations.Rows[i].Clear;
  // [MODIF_ENTETE_TABLEUR]: i := 1 (précédemment: i := 0)
  for i := 1 to grdStations.RowCount - 1 do grdStations.Cells[0, i] := Format(FMTSERST, [FCurrentSerie.GetNumeroDeSerie(), i]);
  //V := FCurrentSerie.getVisee(0);
  //grdStations.cells[NUM_COL_L, 0] := format(FORMAT_NB_REAL_3_DEC, [V.Longueur]);
  // [MODIF_ENTETE_TABLEUR]: i := 1 (précédemment: i := 0)
  QNbStations := FCurrentSerie.GetNbVisees();
  //grdStations.RowCount := 1 + QNbStations;

  for i := 1 to QNbStations - 1 do
  begin
    V := FCurrentSerie.GetVisee(i);
    try
      grdStations.Cells[NUM_COL_IDTERRAIN   , i] := V.IDTerrainStation;
      grdStations.Cells[NUM_COL_SECTEUR     , i] := Format(FORMAT_NB_INTEGER, [V.IDSecteur]);
      grdStations.Cells[NUM_COL_TYPEVISEE   , i] := format(FORMAT_NB_INTEGER, [Ord(V.TypeVisee)]);

      grdStations.Cells[NUM_COL_CODE        , i] := format(FORMAT_NB_INTEGER, [V.Code]);
      grdStations.Cells[NUM_COL_EXPE        , i] := format(FORMAT_NB_INTEGER, [V.Expe]);

      grdStations.Cells[NUM_COL_L           , i] := format(FORMAT_NB_REAL_3_DEC, [V.Longueur]);
      grdStations.Cells[NUM_COL_A           , i] := format(FORMAT_NB_REAL_3_DEC, [V.Azimut]);
      grdStations.Cells[NUM_COL_P           , i] := format(FORMAT_NB_REAL_3_DEC, [V.Pente]);

      grdStations.Cells[NUM_COL_LG          , i] := format(FORMAT_NB_REAL_3_DEC, [V.LG]);
      grdStations.Cells[NUM_COL_LD          , i] := format(FORMAT_NB_REAL_3_DEC, [V.LD]);
      grdStations.Cells[NUM_COL_HZ          , i] := format(FORMAT_NB_REAL_3_DEC, [V.HZ]);
      grdStations.Cells[NUM_COL_HN          , i] := format(FORMAT_NB_REAL_3_DEC, [V.HN]);

      grdStations.Cells[NUM_COL_OBS         , i] := _AnsiToLCLStr(V.Commentaires);
      grdStations.Cells[NUM_COL_HORODATE    , i] := DateTimePascalToDateTimeSQL(V.Horodatage);

      grdStations.Cells[NUM_COL_TEMPERATURE , i] := format(FORMAT_NB_REAL_3_DEC, [V.Temperature]);
      grdStations.Cells[NUM_COL_HUMIDITY    , i] := format(FORMAT_NB_REAL_3_DEC, [V.Humidity]);

    except
    end;
  end;
  grdStations.Col := 1;
  grdStations.Row := 1;
  grdStations.LeftCol := 0;
end;

//******************************************************************************
procedure TCdrSerieIndependant.grdStationsDblClick(Sender: TObject);
var
  P: TPoint;
  QCol, QRow: integer;
  Q: integer;
  S: string;
  UnCode: TCode;
  UneExpe: TExpe;
begin

  (*
  //******************************************************
  // DONE: Ce code fonctionne mais l'événement OnDblClick est intercepté
  //       par l'éditeur de texte incorporé
  // Source: http://www.developpez.net/forums/d299139/ \n
  //         environnements-developpement/delphi/selection-cellule-tstringgrid-double-click/

  P := Mouse.CursorPos;
  //GetCursorPos(P) ;
  P := grdStations.ScreenToClient(P);
  grdStations.MouseToCell(P.X, P.Y, Qcol, QRow);
  if grdStations.Row = 0 then Exit;
  case grdStations.Col of
    NUM_COL_TYPEGAL: // sélection d'un type de galerie
    begin
      q := StrToIntDef(grdStations.Cells[NUM_COL_TYPEGAL, grdStations.Row], 0);
      q := ChooseTypeVisee(q);
      grdStations.Cells[NUM_COL_TYPEGAL, grdStations.Row] := Format(FORMAT_NB_INTEGER, [q]);
    end;
    NUM_COL_CODE:
    begin // sélectionner un code
      q := StrToIntDef(grdStations.Cells[NUM_COL_CODE, grdStations.Row], 1);
      q := SelectionDansListe(FDocuToporobot, mslCODE, q, False);
      if (q > 0) then UnCode := FDocuToporobot.GetCodeByIndex(q)
                 else UnCode := FDocuToporobot.GetCodeByIndex(1);
      grdStations.Cells[NUM_COL_CODE, grdStations.Row] := Format(FORMAT_NB_INTEGER, [UnCode.IDCode]);
    end;
    NUM_COL_EXPE:
    begin // sélectionner une expé
      q := StrToIntDef(grdStations.Cells[NUM_COL_EXPE, grdStations.Row], 1);
      q := SelectionDansListe(FDocuToporobot, mslEXPE, q, False);
      if (q > 0) then UneExpe := FDocuToporobot.GetExpeByIndex(Q)
                 else UneExpe := FDocuToporobot.GetExpeByIndex(1);
      grdStations.Cells[NUM_COL_EXPE, grdStations.Row] := Format(FORMAT_NB_INTEGER, [UneExpe.IDExpe]);
    end;
    NUM_COL_OBS:
    begin // commentaire station
      s := grdStations.Cells[NUM_COL_OBS, grdStations.Row];
      if (GHTopoInputQuery(GetResourceString(rsINPUT_COMMENTAIRE_TITRE),
                           GetResourceString(rsINPUT_COMMENTAIRE_MSG),
                           s))
      then grdStations.Cells[NUM_COL_OBS, grdStations.Row] := s;
    end;
    else;
  end;

  //*)
end;
// /!\ OnGetCellHint() nécessite d'armer le flag Options.goCellHint et de mettre ShowHint à True
procedure TCdrSerieIndependant.grdStationsGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
var
  EWE: LongInt;
  SR: TSecteur;
  EX: TExpe;
  CD: TCode;
  WU: String;
begin
  WU := trim(grdStations.Cells[ACol, ARow]);
  //if (WU = '') then Exit;
  EWE := StrToIntDef(WU, 0);
  case ACol of
    NUM_COL_SECTEUR:
      begin
        SR  := FDocuToporobot.GetSecteur(EWE);
        HintText := Format('Secteur %d: %s', [EWE, SR.NomSecteur]);
      end;
    NUM_COL_TYPEVISEE:
      begin
        HintText := GetDescTypeVisee(TTypeDeVisee(EWE));
      end;
    NUM_COL_EXPE:
      begin
        EX := FDocuToporobot.GetExpeByNumero(EWE);
        {$WARNING: TEXpe.DateExpe à implementer}
        HintText := Format('Expe: %d - %.2d/%.2d/%.4d - %s', [EX.IDExpe, EX.JourExpe, EX.MoisExpe, EX.AnneeExpe, EX.Commentaire]);
      end;
    NUM_COL_CODE:
      begin
        CD := FDocuToporobot.GetCodeByNumero(EWE);
        HintText := Format('Code: %d - %0.f, %.0f - %s', [CD.IDCode, CD.GradAz, CD.GradInc, CD.Commentaire]);
      end
  else;
  end;
end;

procedure TCdrSerieIndependant.grdStationsHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
var
  WU: Integer;
  QQ: Integer;
  k: Integer;
  function MiouMiou(const Miou: integer): integer;
  var
    NTM: TModeSelectionListe;
    QSelectedIndex: integer;
  begin
    Result := Miou;
    case Index of
       NUM_COL_SECTEUR : NTM := mslSECTEURS;
       NUM_COL_CODE    : NTM := mslCODE;
       NUM_COL_EXPE    : NTM := mslEXPE;
    end;
    if (SelectionDansListe(FDocuToporobot, NTM, False, QSelectedIndex)) then Result := QSelectedIndex;
  end;
begin
  if (not IsColumn) then Exit;
  case Index of
    NUM_COL_SECTEUR:
      begin
        WU := StrToIntDef(grdStations.Cells[Index, grdStations.Row], 0);
        WU := MiouMiou(WU);
        grdStations.Cells[NUM_COL_SECTEUR, grdStations.Row] := Format(FORMAT_NB_INTEGER, [WU]);
      end;
    NUM_COL_TYPEVISEE:
      begin
        (*
        QQ := grdStations.Left;
        for k := 0 to NUM_COL_TYPEGAL - 1 do QQ := QQ + grdStations.ColWidths[k];
        pnlTypesVisees.Top  := 60;
        pnlTypesVisees.Left := QQ;
        WU := StrToIntDef(grdStations.Cells[Index, grdStations.Row], 0);
        pnlTypesVisees.Visible     := true;
        lsbTypesGaleries.ItemIndex := WU;
        //*)
      end;
    NUM_COL_CODE:
      begin
        WU := StrToIntDef(grdStations.Cells[Index, grdStations.Row], 0);
        WU := MiouMiou(WU);
        grdStations.Cells[NUM_COL_CODE, grdStations.Row] := Format(FORMAT_NB_INTEGER, [WU]);
      end;
    NUM_COL_EXPE:
      begin
        WU := StrToIntDef(grdStations.Cells[Index, grdStations.Row], 0);
        WU := MiouMiou(WU) ;
        grdStations.Cells[NUM_COL_EXPE, grdStations.Row] := Format(FORMAT_NB_INTEGER, [WU]);
      end;
  else
    pass;
  end;
end;


procedure TCdrSerieIndependant.grdStationsKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  S: string;
  Err: boolean;
  Q: integer;
  EWE: double;
begin
  // /!\: Propriété AutoAdvance doit être désactivée
  //------------------------
  with grdStations do
  begin
    //EditorMode := True;
    if (Col = 1) then  // Colonne 1: Tous caractères interdits sauf ceux spécifiés
    begin
      (*
      if (EditorMode)then
      begin // EditorMode = activé lorsqu'on saisit une valeur
        //              désactivé lors de la validation ou le déplacement
        if not (Key in [13, 8, Ord('A') .. Ord('Z'),
          Ord('a') .. Ord('z'), Ord('0') .. Ord('9'),
          Ord('.'), Ord('-'), Ord('_'),
          Ord('+')]) then
          Key := 0;
      end;
      //*)
    end;
    // Valeurs numériques:
    // Adaptation aux claviers sans pavé numérique (portables)
    // la lettre M est remplacée par un signe moins
    if (Col in [NUM_COL_L, NUM_COL_A, NUM_COL_P, NUM_COL_LG, NUM_COL_LD, NUM_COL_HZ, NUM_COL_HN]) then
    begin
      if ((Key = Ord('M')) or (Key = Ord('m'))) then
        Key := Ord('-');
    end;
    // appui sur les touches
    case key of
      VK_RETURN:
      begin
        AfficherMessage(Format('<RETURN> pressé (Colonne: %d)', [Col]));
        // si on se trouve en colonne Long..Pente .. LRUD,
        // on évalue l'expression si la cellule contient un signe égal en tête
        if (Col in [NUM_COL_EXPE, NUM_COL_CODE,
                    NUM_COL_L,
                    NUM_COL_A, NUM_COL_P, NUM_COL_LG .. NUM_COL_HN]) then
        begin
          S := Trim(Cells[Col, Row]);
          if (S = '') then S := '0.000'; // cellule vide => valeur 0.00
          if (S[1] = '=') then  System.Delete(S, 1, 1); // pour ancienne méthode et habitudes de tableur
          EWE := EvaluationRapideExpression(S);
          if (Col in [NUM_COL_EXPE, NUM_COL_CODE]) then
            Cells[Col, Row] := Format(FORMAT_NB_INTEGER, [Trunc(EWE)])
          else
            Cells[Col, Row] := Format(FORMAT_NB_REAL_3_DEC, [EWE]);
          // erreur = se replacer sur la case concernée
          Q := GridCheckErrorInGrdStations(Col, Row, EWE);
          if (Q > 0) then Col := Q - 1; // recule d'une colonne pour contrebalancer l'incrément de colonne
        end;
        // si on arrive en fin de ligne,
        // on passe à la suivante
        // en recopiant le type de visée, l'Expé et le Code
        // et on se positionne sur la colonne longueur
        if ((Col + 1) > NUM_COL_OBS) then
        begin
          Row := Row + 1;
          Cells[NUM_COL_IDTERRAIN , Row] := IncrementString(Cells[NUM_COL_IDTERRAIN, Row - 1]);
          Cells[NUM_COL_SECTEUR   , Row] := Cells[NUM_COL_SECTEUR, Row-1];
          Cells[NUM_COL_TYPEVISEE , Row] := Cells[NUM_COL_TYPEVISEE, Row - 1]; // type visée
          Cells[NUM_COL_CODE      , Row] := Cells[NUM_COL_CODE, Row - 1];      // code
          Cells[NUM_COL_EXPE      , Row] := Cells[NUM_COL_EXPE, Row - 1];      // expé
          Col := NUM_COL_EXPE;
          LeftCol := 1;
        end;
        Col := Col + 1;
      end;
    end;
  end;
end;

procedure TCdrSerieIndependant.grdStationsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  R: TRect;
  SC: TGridRect;
  NbCellsSelected: LongInt;
begin
  SC := grdStations.Selection;
  NbCellsSelected := (1 + SC.Right - SC.Left) * (1 + SC.Bottom - SC.Top); // ne pas afficher si sélection de plusieurs cellules
  if ((aRow > 0) and (aCol = NUM_COL_TYPEVISEE) and (1 = NbCellsSelected)) then
  begin
    R := grdStations.CellRect(aCol, aRow);
    R.Left   := R.Left + grdStations.Left;
    R.Right  := R.Right + grdStations.Left;
    R.Top    := R.Top + grdStations.Top;
    R.Bottom := R.Bottom + grdStations.Top;
   end;
   CanSelect := True;
end;



procedure TCdrSerieIndependant.lsbErreursClick(Sender: TObject);
var
  EWE, WU: TGHStringArray;
  i, L, C: integer;
begin
  if (lsbErreurs.ItemIndex < 0) then Exit;
  try
    EWE := Split(lsbErreurs.Items[lsbErreurs.ItemIndex], ':');
    WU  := Split(EWE[0], ',');
    L := StrToIntDef(Trim(WU[0]), 1);
    C := StrToIntDef(Trim(WU[1]), 1);
    if (L <= 0) then
    begin
      for i := 0 to ComponentCount - 1 do
      begin
        if (Components[i].Tag = L) and (Components[i] is TCurrencyEdit) then TCurrencyEdit(Components[i]).SetFocus;
      end;
    end
    else
    begin
      grdStations.Row := L;
      grdStations.Col := C;
      grdStations.SetFocus;
    end;
  except
    ; // showmessage('toto');
  end;
end;
// doit être appelé juste après mise en place du formulaire et du tableau
function TCdrSerieIndependant.CalcCondensat(): string;
var
  j, i: Integer;
begin
  Result := '';
  // condensat des valeurs numériques
  Result += Format('%d:%d%d:%d.%d:%d.%d:',
                  [editNumeroSerie.AsInteger,
                   editNumEntreeRattachement.AsInteger, editNumeroReseau.AsInteger,
                   editSerieDepart.AsInteger, editPointDepart.AsInteger,
                   editSerieArrivee.AsInteger, editPointArrivee.AsInteger
                  ]);
  // on condense  les chances et obstacles
  Result += Format('%d:%d', [cmbChance.ItemIndex, cmbObstacle.ItemIndex]);
  // données texte
  Result += Trim(editNomSerie.Text) +
            Trim(editCommentaire.Text);
  // condensat du tableau
  // On est abruti, con, bête et méchant: on concatène le contenu (trimmé) des cellules)
  for i := 0 to grdStations.RowCount - 1 do
    for j := 0 to grdStations.ColCount - 1 do
      Result += Trim(grdStations.Cells[j, i]);
  Result := CalcSHA1(Result);
  AfficherMessageErreur('Condensat initial: ' + Result);
end;


procedure TCdrSerieIndependant.RechercherStation(const QEditSer, QEditPt: TCurrencyEdit);
var
  S, P: integer;
  QFindWhat, Q: string;
  QDoMatchExact: boolean;
begin
  QDoMatchExact := false;
  if (DispDlgFindIDTerrain(FDocuToporobot, QDoMatchExact, QFindWhat)) then
  begin
    if (FDocuToporobot.FindPtTopoByCle(QDoMatchExact, QFindWhat, S, P)) then
    begin
      QEditSer.AsInteger := S;
      QEditPt.AsInteger  := P;
      QFindWhat := '   ' + QFindWhat;
      S := Pos('|', QFindWhat);
      Q := Trim(Copy(QFindWhat, 1, S - 1));
      QEditSer.Hint := Q;
      QEditPt.Hint  := Q;

    end
    else
      ShowMessage(GetResourceString(rsMATCHNOTFOUND));
  end;
end;

procedure TCdrSerieIndependant.btnGetSerStDepartByIDClick(Sender: TObject);
begin
  RechercherStation(editSerieDepart, editPointDepart);
end;

procedure TCdrSerieIndependant.acAddLineExecute(Sender: TObject);
begin
  GridInsertLignes(grdStations);
end;

procedure TCdrSerieIndependant.acChooseCodeExecute(Sender: TObject);
var
  WU: Integer;
begin
  WU := StrToIntDef(grdStations.Cells[NUM_COL_CODE, FRowCourante], 0);
  if (SelectionDansListe(FDocuToporobot, mslCODE, False, WU)) then grdStations.Cells[NUM_COL_CODE, FRowCourante] := Format(FORMAT_NB_INTEGER, [WU]);
end;

procedure TCdrSerieIndependant.acChooseExpeExecute(Sender: TObject);
var
  WU: Integer;
begin
  WU := StrToIntDef(grdStations.Cells[NUM_COL_EXPE, FRowCourante], 0);
  if (SelectionDansListe(FDocuToporobot, mslEXPE, False, WU)) then grdStations.Cells[NUM_COL_EXPE, FRowCourante] := Format(FORMAT_NB_INTEGER, [WU]);
end;

procedure TCdrSerieIndependant.acChooseSecteurExecute(Sender: TObject);
var
  WU: Integer;
begin
   WU := StrToIntDef(grdStations.Cells[NUM_COL_SECTEUR, FRowCourante], 0);
   if (SelectionDansListe(FDocuToporobot, mslSECTEURS, False, WU)) then grdStations.Cells[NUM_COL_SECTEUR, FRowCourante] := Format(FORMAT_NB_INTEGER, [WU]);
end;

procedure TCdrSerieIndependant.acChooseTypeViseeExecute(Sender: TObject);
var
  QQ: Integer;
  k: Integer;
  WU: Integer;
begin
  (*
  QQ := grdStations.Left;
  for k := 0 to NUM_COL_TYPEGAL - 1 do QQ := QQ + grdStations.ColWidths[k];
  pnlTypesVisees.Top  := 60;
  pnlTypesVisees.Left := QQ;
  WU := StrToIntDef(grdStations.Cells[NUM_COL_TYPEGAL, grdStations.Row], 0);
  pnlTypesVisees.Visible     := true;
  lsbTypesGaleries.ItemIndex := WU;
  //*)
end;

procedure TCdrSerieIndependant.acCopierLeTableauExecute(Sender: TObject);
begin
  grdStations.CopyToClipboard();
end;

procedure TCdrSerieIndependant.acCopierSelectionExecute(Sender: TObject);
begin
  grdStations.CopyToClipboard(True);
end;

procedure TCdrSerieIndependant.acCreerViseeDepuisDistoXExecute(Sender: TObject);
begin
  ;
end;

procedure TCdrSerieIndependant.acEraseLineExecute(Sender: TObject);
begin
  GridEraseLines(grdStations);
end;

procedure TCdrSerieIndependant.acExtractIDTerrainExecute(Sender: TObject);
var
  EWE: String;
  i: Integer;
begin
  for i := 0 to grdStations.RowCount - 1 do
  begin
    if (IsEmptyRow(i)) then Break;
    EWE := ExtractIDTerrainFromComment(grdStations.Cells[NUM_COL_OBS, i]);
    grdStations.Cells[NUM_COL_IDTERRAIN, i] := EWE;
  end;
end;

procedure TCdrSerieIndependant.acPasteFromClipboardExecute(Sender: TObject);
var
  i, j, QR, QC: integer;
  ClipBoard: TClipboard;
  ValuesArray: TGHStringArray2D;
  EWE, WU: string;
begin
  if (not GHTopoQuestionOuiNon(rsMSG_HAS_ALREADY_DATA)) then Exit;
  ClipBoard := TClipboard.Create(ctClipboard);
  try
   if (ClipBoard.HasFormat(CF_TEXT)) then
   begin
     AfficherMessage('>> Le presse-papiers contient du texte');
     EWE := ClipBoard.AsText;
     // remplacer les virgules par le séparateur décimal
     EWE := StringReplace(EWE, ',', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
     ValuesArray := MakeTStringArray2DFromStr(EWE, TAB);
     // affichage dans le tableau
     for i := 0 to MAX_NB_LIGNES_TSTRINGARRAY2D do
     begin
       QR := i + FRowCourante;
       if (QR < grdStations.RowCount) then
       begin
         for j := 0 to MAX_SIZE_PARAM_ARRAY do
         begin
           QC := j + FColCourante;
           if (QC < grdStations.ColCount) then grdStations.Cells[QC, QR] := ValuesArray[i,j];
         end;
       end;
      end;
    end else
    begin
      ShowMessage('Presse-papiers vide ou invalide');
    end;
  finally
    FreeAndNil(ClipBoard);
  end;
end;



procedure TCdrSerieIndependant.acReplierTableurExecute(Sender: TObject);
begin
  AfficherEnteteComplet(not FEnteteSerieDeplie);
end;

procedure TCdrSerieIndependant.acUndoCopyCurrentCodeExpeExecute(Sender: TObject);
var
  MyRow, MyIdxSecteur, i: Integer;
  MyIdxCode: TNumeroCode;
  MyIdxExpe: TNumeroExpe;

begin
  {$IFDEF GROS_MINET}
     ShowMessage('Not implemented');
  {$ELSE}
  if (Not GHTopoQuestionOuiNon('Vous pouvez aussi utiliser l''outil de maintenance du document ' + #13#10 +
                               'pour réattribuer les index - Continuer ?')) then Exit;
  MyRow := grdStations.Row;
  MyIdxSecteur := StrToIntDef(grdStations.Cells[NUM_COL_SECTEUR, MyRow], 0);
  MyIdxCode    := StrToIntDef(grdStations.Cells[NUM_COL_CODE   , MyRow], 0);
  MyIdxExpe    := StrToIntDef(grdStations.Cells[NUM_COL_EXPE   , MyRow], 0);
  if (SelectionnerSecteurCodeExpe(FDocuToporobot, MyRow, True, MyIdxSecteur, MyIdxCode, MyIdxExpe)) then
  begin
    for i := MyRow to grdStations.RowCount - 1 do
    begin
      grdStations.Cells[NUM_COL_SECTEUR, i] := Format(FORMAT_NB_INTEGER, [MyIdxSecteur]);
      grdStations.Cells[NUM_COL_CODE   , i] := Format(FORMAT_NB_INTEGER, [MyIdxCode]);
      grdStations.Cells[NUM_COL_EXPE   , i] := Format(FORMAT_NB_INTEGER, [MyIdxExpe]);
    end;
  end;
  {$ENDIF}
end;

procedure TCdrSerieIndependant.AfficherEnteteComplet(const P: boolean);
var
  H1, H2 : integer;
begin
  H1 := pnlEnteteSerie.Top + editNumEntreeRattachement.Top - 1;
  H2 := pnlEnteteSerie.Top + pnlEnteteSerie.Height;
  AfficherMessage(Booltostr(FEnteteSerieDeplie, 'Entete déplié', 'Entete replié'));
  if (P = FEnteteSerieDeplie) then Exit;
  FEnteteSerieDeplie := P;
  if (FEnteteSerieDeplie) then
  begin
    pnlTableur.Top := H2;
    pnlTableur.Height := self.Height - H2;
  end
  else
  begin
    pnlTableur.Top := H1;
    pnlTableur.Height := self.Height - H1;
  end;
end;


procedure TCdrSerieIndependant.acChooseReseauExecute(Sender: TObject);
begin
  ;
end;

procedure TCdrSerieIndependant.InsertGrdLines(const N: integer);
var
  i, j: integer;
begin
  AfficherMessage(Format('%s.InsertGrdLine', [ClassName]));
  for i := grdStations.RowCount - 1 downto grdStations.Row + N do
  begin
    for j := 1 to grdStations.ColCount - 1 do
      grdStations.Cells[j, i] := grdStations.Cells[j, i - N];
  end;
  for i := grdStations.Row to grdStations.Row + N - 1 do
  begin
    for j := NUM_COL_L to NUM_COL_HN do grdStations.Cells[j, i] := '';
    grdStations.Cells[NUM_COL_OBS, i] := Format('** Inseree (%d) **', [1 + i - grdStations.Row]);
  end;
end;

procedure TCdrSerieIndependant.acUndoCopyExecute(Sender: TObject);
begin
  GridUndocopy(grdStations, true);
end;

procedure TCdrSerieIndependant.btnAttribuerExtremiteLibreClick(Sender: TObject);
begin
  AttribuerExtremiteLibre();
end;

procedure TCdrSerieIndependant.btnCloseErrWndClick(Sender: TObject);
begin
  pnlErreursACorriger.Visible := false;
end;




procedure TCdrSerieIndependant.btnCopyGridClick(Sender: TObject);
begin
  grdStations.CopyToClipboard(true);
end;
function TCdrSerieIndependant.IsEmptyRow(const IdxRow: integer): boolean;
begin
  Result := (IdxRow > 0) and
            (Trim(grdStations.Cells[NUM_COL_L, IdxRow]) = '') and
            (Trim(grdStations.Cells[NUM_COL_A, IdxRow]) = '') and
            (Trim(grdStations.Cells[NUM_COL_P, IdxRow]) = '');
end;

procedure TCdrSerieIndependant.btnGetSerStArriveeByIDClick(Sender: TObject);
var
  EE: TBaseStation;
  QIDTerrainStation: String;
  QMaSerie: TNumeroSerie;
  QMaStation: integer;
begin
  QIDTerrainStation := Format(FMTSERST, [editSerieArrivee.AsInteger, editPointArrivee.AsInteger]); // On demande un IDterrain ou un couple
  if (InputQuery('Rechercher une station', 'ID station', QIDTerrainStation)) then
  begin
    if (Not FDocuToporobot.FindPtTopoByCle(false, QIDTerrainStation, QMaSerie, QMaStation)) then
    begin
      ShowMessageFmt('Station "%s" introuvable', [QIDTerrainStation]);
      Exit;
    end;
    editSerieArrivee.AsInteger := QMaSerie;
    editPointArrivee.AsInteger := QMaStation;
  end
end;

procedure TCdrSerieIndependant.btnselectEntranceClick(Sender: TObject);
var
  WU: TEntrance;
  n: LongInt;
begin
  n := editNumEntreeRattachement.AsInteger;
  if (SelectionDansListe(FDocuToporobot, mslENTRANCES, False, n)) then
  begin
    editNumEntreeRattachement.AsInteger  := n;
    WU := FDocuToporobot.GetEntrance(editNumEntreeRattachement.AsInteger);
    lbNomEntree.Caption := _AnsiToLCLStr(WU.eNomEntree);
  end;
end;

procedure TCdrSerieIndependant.btnSelectReseauClick(Sender: TObject);
var
  WU: TReseau;
  n: LongInt;
begin
  n := editNumeroReseau.AsInteger;
  if (SelectionDansListe(FDocuToporobot, mslRESEAUX, false, n)) then
  begin
    editNumeroReseau.AsInteger := n;
    WU := FDocuToporobot.GetReseau(editNumeroReseau.AsInteger);
    lbNomReseau.Caption := _AnsiToLCLStr(WU.NomReseau);
  end;
end;


procedure TCdrSerieIndependant.Button2Click(Sender: TObject);
begin
  ShowMessage('Ce coefficient caractérise la déformabilité de la série' + #13#10 +
              'Mettre une valeur élevée (e.g: 5 à 10): la série est peu déformable' + #13#10 +
              'Mettre une valeur faible (eg: 0.5): la série est très déformable');
end;

procedure TCdrSerieIndependant.chkLockedChange(Sender: TObject);
begin

end;


procedure TCdrSerieIndependant.cmbPropalesRaideurChange(Sender: TObject);
begin
  case cmbPropalesRaideur.ItemIndex of
    0: pass;                          //  Valeur fournie
    1: editRaideur.Value  :=    1.0;  // par défaut;
    2: editRaideur.Value  := 1000.0;  //  Enclume;
    3: editRaideur.Value  :=   10.0;  // Bite de cheval
    4: editRaideur.Value  :=    5.0;  // Bite de nègre
    5: editRaideur.Value  :=    1.0;  // Bite normale
    6: editRaideur.Value  :=    0.5;  // Flaccide
    7: editRaideur.Value  :=    0.3;  //  Bite de Buldo
    8: editRaideur.Value  :=    0.1;  // Impuissance quasi totale    //*)
  end;
end;



procedure TCdrSerieIndependant.editNumEntreeRattachementKeyPress(Sender: TObject; var Key: char);
var
  MyEntrance: TEntrance;
  n: LongInt;
begin
  if (Key = #13) then
  begin
    n := editNumEntreeRattachement.AsInteger;
    if (IsInRange(n, 0, FDocuToporobot.GetNbEntrances())) then
    begin
      MyEntrance := FDocuToporobot.GetEntrance(editNumEntreeRattachement.AsInteger);
      lbNomEntree.Caption := _AnsiToLCLStr(MyEntrance.eNomEntree);
    end;
  end;
end;

procedure TCdrSerieIndependant.editPointDepartExit(Sender: TObject);
begin

end;



procedure TCdrSerieIndependant.ConfigurerGrdPopUp(const NumColonne: integer);
begin
  mnuChooseReseauSecteurCodeExpe.Visible := True;
  case grdStations.Col of
    NUM_COL_SECTEUR    : mnuChooseReseauSecteurCodeExpe.Action := acChooseSecteur;
    NUM_COL_TYPEVISEE  : mnuChooseReseauSecteurCodeExpe.Action := acChooseTypeVisee;
    NUM_COL_CODE       : mnuChooseReseauSecteurCodeExpe.Action := acChooseCode;
    NUM_COL_EXPE       : mnuChooseReseauSecteurCodeExpe.Action := acChooseExpe;
  else
    begin
      mnuChooseReseauSecteurCodeExpe.Action := nil;
      mnuChooseReseauSecteurCodeExpe.Visible:= false;
    end;
  end;
end;

// calculer et attribuer extrémité libre
// DONE: Ajout de la variable wu pour éviter réutilisation de var de boucle en dehors de celle-ci
procedure TCdrSerieIndependant.AttribuerExtremiteLibre();
var
  b: boolean;
  i: integer;
  wu: integer;
begin
  wu := 0;
  for i := 0 to grdStations.RowCount - 1 do
  begin
    wu := i;
    b := (i > 0) and (grdStations.Cells[NUM_COL_L, i] = '') and
         (grdStations.Cells[NUM_COL_A, i] = '') and (grdStations.Cells[NUM_COL_P, i] = '');
    if (b) then Break;
  end; // for
  editPointArrivee.AsInteger := wu - 1;
  editSerieArrivee.AsInteger := editNumeroSerie.AsInteger;
end;

procedure TCdrSerieIndependant.DisplayHint(const MyCtrl: TControl; const Msg: string);
begin

end;

// Sauvegarde modifs formulaire et tableau
function TCdrSerieIndependant.ImplementerModifs(out QNbErreurs: integer): boolean;
var
  i: integer;
  V: TUneVisee;
  NbAffectedViseesAntennes: Integer;
  SD, EWE: Integer;
  qdx, qdy, qdz: double;
  QSerieDoublon: TObjSerie;
  WU: boolean;
  fatxe: String;
  // ajout du message d'erreur
  procedure AddMsgErreur(const NoLigne, NoColonne: integer; const Msg: string);
  begin
    lsbErreurs.Items.Add(Format('%d, %d: %s', [NoLigne, NoColonne, GetResourceString(Msg)]));
  end;
  // vérification des stations
  function CheckAStation(const AStation: TUneVisee): integer;
  const
    LRUD_TOO_BIG = 15.00;
  var
    AMin, AMax: double;
    CC: TCode;
    EE: TExpe;
    n: Integer;
    procedure SetAMinAMax(const A1, A2: double); inline;
    begin
      AMin := A1;
      AMax := A2;
    end;
  begin
    Result := 0;
    //AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));

    // TODO: entrées
    // TODO: réseaux
    n := FDocuToporobot.GetNbSecteurs();
    if (not IsInRange(Ord(AStation.IDSecteur), 0, n - 1)) then AddMsgErreur(i, NUM_COL_SECTEUR, Format(rsCDR_SERIES_MSG_ERROR_NONEXISTENT_SECTEUR, [AStation.IDSecteur, 0, n - 1]));

    //if (not FDocuToporobot.ExistsIdxSecteur(AStation.IDSecteur)) then AddMsgErreur(i, NUM_COL_SECTEUR, Format(rsCDR_SERIES_MSG_ERROR_NONEXISTENT_SECTEUR, [AStation.IDSecteur]));
    // type de visée
    if (not IsInRange(Ord(AStation.TypeVisee), 0, 10)) then AddMsgErreur(i, NUM_COL_TYPEVISEE, Format(rsCDR_SERIES_MSG_ERROR_INVALID_TYPE_VISEE, [AStation.TypeVisee, 0, 10]));
    // longueur, ne doit pas dépasser 160 m
    AMin := 0.00; AMax := SEUIL_LONGUEUR_MAXI_TOPOROBOT;
    if (not IsInRange(AStation.Longueur, AMin, AMax)) then AddMsgErreur(i, NUM_COL_L, Format(rsCDR_SERIES_MSG_ERROR_INVALID_LONGUEUR, [AStation.Longueur, AMin, AMax]));
    // largeurs et hauteurs
    AMin := 0.00; AMax := SEUIL_LONGUEUR_MAXI_TOPOROBOT;
    if (not IsInRange(AStation.LG, AMin, AMax)) then AddMsgErreur(i, NUM_COL_LG, Format(rsCDR_SERIES_MSG_ERROR_INVALID_LG, [AStation.LG, AMin, AMax]));
    if (not IsInRange(AStation.LD, AMin, AMax)) then AddMsgErreur(i, NUM_COL_LD, Format(rsCDR_SERIES_MSG_ERROR_INVALID_LD, [AStation.LD, AMin, AMax]));
    if (not IsInRange(AStation.HZ, AMin, AMax)) then AddMsgErreur(i, NUM_COL_HZ, Format(rsCDR_SERIES_MSG_ERROR_INVALID_HZ, [AStation.HZ, AMin, AMax]));
    if (not IsInRange(AStation.HN, AMin, AMax)) then AddMsgErreur(i, NUM_COL_HN, Format(rsCDR_SERIES_MSG_ERROR_INVALID_HN, [AStation.HN, AMin, AMax]));

    if (AStation.LG > LRUD_TOO_BIG) then  AddMsgErreur(i, NUM_COL_LG, rsCDR_SERIE_MSG_WARN_LRUD);
    if (AStation.LD > LRUD_TOO_BIG) then  AddMsgErreur(i, NUM_COL_LD, rsCDR_SERIE_MSG_WARN_LRUD);
    if (AStation.HZ > LRUD_TOO_BIG) then  AddMsgErreur(i, NUM_COL_HZ, rsCDR_SERIE_MSG_WARN_LRUD);
    if (AStation.HN > LRUD_TOO_BIG) then  AddMsgErreur(i, NUM_COL_HN, rsCDR_SERIE_MSG_WARN_LRUD);

    if (FDocuToporobot.ExistsIdxCode(AStation.Code)) then  // Contrôles sur codes
    begin
      CC := FDocuToporobot.GetCodeByNumero(AStation.Code); // Fixé
      // azimuts
      AMin := 0.00;
      case Trunc(CC.GradAz) of
        359, 360: AMax := DEGRES_PAR_TOUR; // directes degrés
        399, 400: AMax := GRADES_PAR_TOUR; // directes grades
        349, 350: AMax := DEGRES_PAR_TOUR; // inverses degrés
        389, 390: AMax := GRADES_PAR_TOUR; // inverses grades
      end;
      if (not IsInRange(AStation.Azimut, AMin, AMax)) then AddMsgErreur(i, NUM_COL_A, Format(rsCDR_SERIES_MSG_ERROR_AZIMUT_OUT_OF_RANGE, [AStation.Azimut, AMin, AMax]));
      // pentes
      case Trunc(CC.GradInc) of
        360: SetAMinAMax(-QUADRANT_DEGRES, QUADRANT_DEGRES);     // zéro horizontal degrés
        400: SetAMinAMax(-QUADRANT_GRADES, QUADRANT_GRADES);   // zéro horizontal grades
        361: SetAMinAMax(0.00, ANGLE_JANE_BRIKIN_DEGRES);      // zéro zénithal degrés
        401: SetAMinAMax(0.00, ANGLE_JANE_BRIKIN_GRADES);      // zéro zénithal grades
        359: SetAMinAMax(0.00, ANGLE_JANE_BRIKIN_DEGRES);      // zéro nadiral degrés
        399: SetAMinAMax(0.00, ANGLE_JANE_BRIKIN_GRADES);      // zéro nadiral grades
        370: SetAMinAMax(-580.00, 580.00);   // pourcentage; +/- 80° max
        380: SetAMinAMax(-SEUIL_LONGUEUR_MAXI_TOPOROBOT, SEUIL_LONGUEUR_MAXI_TOPOROBOT);   // dénivelés; maxi=long max visée
        350: SetAMinAMax(-QUADRANT_DEGRES, QUADRANT_DEGRES);     // zéro horizontal degré, inverse
        390: SetAMinAMax(-QUADRANT_GRADES, QUADRANT_GRADES);   // zéro horizontal grade, inverse
      else
        SetAMinAMax(-100.00, 100.00);        // zéro horizontal grades par défaut
      end;
      if (not IsInRange(AStation.Pente, AMin, AMax)) then AddMsgErreur(i, NUM_COL_P,  Format(rsCDR_SERIES_MSG_ERROR_PENTE_OUT_OF_RANGE, [AStation.Pente, AMin, AMax]));
    end else // Des codes inexistants vident de sens tout contrôle de validité
    begin
      AddMsgErreur(i, NUM_COL_CODE,  Format(rsCDR_SERIES_MSG_ERROR_NONEXISTENT_CODE, [AStation.Code]));
    end;
    if (FDocuToporobot.ExistsIdxExpe(AStation.Expe)) then   // Contrôles sur expés
    begin
      pass;  // TODO: en attente des contrôles sur expés
    end else
    begin // Des expés inexistantes vident de sens tout contrôle de validité
      AddMsgErreur(i, NUM_COL_EXPE, Format(rsCDR_SERIES_MSG_ERROR_NONEXISTENT_EXPE, [AStation.Expe]));
    end;
  end;
begin
  QNbErreurs := 0;
  {$DEFINE ACTIVETRY}
  {$UNDEF ACTIVETRY}

  AfficherMessage(Format('%s.ImplementerModifs', [ClassName]));
  Result := False;
  lsbErreurs.Clear;

  // calcul éventuel estrémités de série
  if (editPointArrivee.AsInteger = -1) then AttribuerExtremiteLibre();
  //AttribuerExtremiteLibre(editSerieArrivee.AsInteger, editPointArrivee.AsInteger);
  {$IFDEF ACTIVETRY}
  try
  {$ENDIF}
    FCurrentSerie.SetNumeroSerie(TNumeroSerie(editNumeroSerie.AsInteger));
    FCurrentSerie.SetNomSerie(Trim(_LCLStrToAnsi(editNomSerie.Text)));
    SD := editNumEntreeRattachement.AsInteger;     // entrées
    FCurrentSerie.SetNumeroEntrance(SD);
    SD := editNumeroReseau.AsInteger;              // réseaux
    FCurrentSerie.SetNumeroReseau(SD);
    WU := FDocuToporobot.HasPtTopoBySerSt(editSerieDepart.AsInteger, editPointDepart.AsInteger) OR
          FDocuToporobot.HasPtTopoBySerSt(editSerieArrivee.AsInteger, editPointArrivee.AsInteger);
    if (not WU) then AddMsgErreur(-3, -4, rsCDR_SERIES_MSG_ERROR_ORPHAN_SERIE);

    FCurrentSerie.SetSeriePtExtremites(editSerieDepart.AsInteger, editPointDepart.AsInteger, editSerieArrivee.AsInteger, editPointArrivee.AsInteger);
    FCurrentSerie.SetChanceObstacle(cmbChance.ItemIndex, cmbObstacle.ItemIndex);
    FCurrentSerie.SetRaideur(editRaideur.Value);
    FCurrentSerie.SetObsSerie(Trim(_LCLStrToAnsi(editCommentaire.Text)));
    // [MODIF_ENTETE_TABLEUR] Attrapper la station 0 de la série
    V := FCurrentSerie.GetVisee(0);
    V.setLRUD(editLG0.Value, editLD0.Value, editHZ0.Value, editHN0.Value);
    FCurrentSerie.ClearStations();      // Purger la liste des points topos
    FCurrentSerie.AddVisee(V);
    // critère de sortie: Colonne 1 à 5 vides ou nulles
    AfficherMessage(GetResourcestring(rsCDR_SERIES_MSG_ERROR_CHECKING_SERIE));
    // si les colonnes Long, Az et P sont vides,
    // la fin de la table des stations est supposée atteinte
    //[MODIF_ENTETE_TABLEUR]
    for i := 1 to grdStations.RowCount - 1 do
    begin
      if (IsEmptyRow(i)) then Break;
      V.IDTerrainStation := FormatterIDTerrainStation(grdStations.Cells[NUM_COL_IDTERRAIN, i]);
      V.IDSecteur        := StrToIntDef(grdStations.Cells[NUM_COL_SECTEUR, i], 0);
      V.TypeVisee        := GetTypeDeVisee(StrToIntDef(grdStations.Cells[NUM_COL_TYPEVISEE, i], 0));
      V.Code             := StrToIntDef(grdStations.Cells[NUM_COL_CODE, i], 1);
      V.Expe             := StrToIntDef(grdStations.Cells[NUM_COL_EXPE, i], 1);
      V.setLongAzInc(grdStations.Cells[NUM_COL_L, i],
                     grdStations.Cells[NUM_COL_A, i],
                     grdStations.Cells[NUM_COL_P, i]);
      V.setLRUD(grdStations.Cells[NUM_COL_LG, i],
                grdStations.Cells[NUM_COL_LD, i],
                grdStations.Cells[NUM_COL_HZ, i],
                grdStations.Cells[NUM_COL_HN, i]);
      V.Commentaires     := _LCLStrToAnsi(Trim(grdStations.Cells[NUM_COL_OBS, i]));
      V.Horodatage       := DateTimeSQLToDateTimePascal(grdStations.Cells[NUM_COL_HORODATE, i]);
      V.Temperature      := ConvertirEnNombreReel(grdStations.Cells[NUM_COL_TEMPERATURE, i], 0.00);
      V.Humidity         := ConvertirEnNombreReel(grdStations.Cells[NUM_COL_HUMIDITY   , i], 0.00);
      CheckAStation(V);
      FCurrentSerie.AddVisee(V);
    end;
    FCurrentSerie.CalculIncertitude(-1, qdx, qdy, qdz);            // calcul d'incertitude


    // rechercher si un doublon existe
    EWE := 0;
    if (FDocuToporobot.HasDoublonsInNumsSeries(FCurrentSerie.GetNumeroDeSerie(), EWE)) then
    begin
      QSerieDoublon := FDocuToporobot.GetSerie(EWE);
      fatxe := format('Le numéro de série %d est utilisé par la série %d: %s [%d]', [FCurrentSerie.GetNumeroDeSerie(), QSerieDoublon.GetNumeroDeSerie(), QSerieDoublon.GetNomSerie(), EWE]);

      AddMsgErreur(-3, -4, fatxe);
      FCurrentSerie.SetNumeroSerie(FOldNumeroserie);
      FOldNumeroserie := FCurrentSerie.GetNumeroDeSerie();
      editNumeroSerie.AsInteger := FOldNumeroserie;
      ShowMessage(fatxe);
      Exit;
    end
    else
    begin
      // on renomme les visées en antenne et on réattribue les ID des noeuds des séries dépendantes
      if (FCurrentSerie.GetNumeroDeSerie() <> FOldNumeroserie) then
      begin
        NbAffectedViseesAntennes := FDocuToporobot.UpdateSerPtExtremsSeriesRattachees(FOldNumeroserie, FCurrentSerie.GetNumeroDeSerie());
        NbAffectedViseesAntennes := FDocuToporobot.ReAttribuerViseesRayonnantesDeUneSerie(FCurrentSerie, FOldNumeroserie);
        ShowMessage(Format(GetResourceString(rsCDR_SERIE_NB_ANTENNES_MODIFIEES), [NbAffectedViseesAntennes]));
      end;
    end;
    FOldNumeroserie := FCurrentSerie.GetNumeroDeSerie();
    AfficherMessage(Format('%s.ImplementerModifs OK', [ClassName]));
    QNbErreurs := lsbErreurs.Count;
    pnlErreursACorriger.Visible := (QNbErreurs <> 0);        // liste des erreurs affichée s'il y en a


    FCondensat := CalcCondensat();                                 // et recalcul du condensat
    Result := True;                                                // Tout est OK: On met Result à TRUE;

  {$IFDEF ACTIVETRY}
  except
    AfficherMessage(Format('%s.ImplementerModifs KO', [ClassName]));
  end;
  {$ENDIF}
end;

function TCdrSerieIndependant.IsDataModified(): boolean;
begin
  Result := (FCondensat <> CalcCondensat());
end;

// En fonction des paramètres de la ligne, vérifie les données
function TCdrSerieIndependant.GridCheckErrorInGrdStations(const CurrColonne, CurrLigne: integer;
                                                          const QValeur: double): integer;
var
  MsgErr: string;
  procedure MiouMiou(const Cond: boolean; const QC: integer; const Msg: string);
  begin
    if (Cond) then
    begin
      Result := QC;
      MsgErr := GetResourceString(Msg);
    end;
  end;
begin
  Result := 0;
  MsgErr := '';
  MiouMiou(True, -1, 'OK');
  case CurrColonne of
    NUM_COL_CODE            : MiouMiou(not FDocuToporobot.ExistsIdxCode(Trunc(QValeur)), CurrColonne, rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND);
    NUM_COL_EXPE            : MiouMiou(not FDocuToporobot.ExistsIdxExpe(Trunc(QValeur)), CurrColonne, rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND);
    NUM_COL_L               : MiouMiou(not IsInRange(QValeur, 0.001, SEUIL_LONGUEUR_MAXI_TOPOROBOT), CurrColonne, Format(rsCDR_SERIE_MSG_ERR_LONG, [SEUIL_LONGUEUR_MAXI_TOPOROBOT]));
    NUM_COL_A               : pass;
    NUM_COL_P               : pass;
    NUM_COL_LG .. NUM_COL_HN: MiouMiou(QValeur < 0.00, CurrColonne, rsCDR_SERIE_MSG_ERR_LRUD);
  else
    pass;
  end;
  lbMessages.Caption := GetResourceString(MsgErr);
  AfficherMessageErreur(Format('%s.ErrorInStation: L%dC%d: %.2f = %d - %s', [ClassName, CurrColonne, CurrLigne, QValeur, Result, MsgErr]));
end;


end.
