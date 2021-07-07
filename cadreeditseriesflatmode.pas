unit CadreEditSeriesFlatMode;
// Editeur de données en mode 'flat'
// à la manière de Visual Topo
// 27/07/2016: Affichage OK, tri OK
// 28/07/2016: Gestion du buffer,
//             Recherche d'erreurs (résultats dans le panneau de messages)
//             Enregistrement - TODO: Suppression des séries; sécuriser la fonctionnalité
//             Navigateur latéral
//             Panneau de messages cliquable (like fenêtre Messages de Lazarus)
//             Recherche plein texte (résultats dans le panneau de messages)
{$ERROR Actuellement inutilisé}
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  ToporobotClasses2012, UnitEntitesExtended,
  Common, UnitObjetSerie,
  evaluateur_expressions,
  CallDialogsStdVersion, CadreListeSeries,
  //CadreListeSeries,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Grids, Graphics, LCLType,
  Dialogs, StdCtrls, ExtCtrls, ActnList, Menus, PairSplitter, Buttons;

type
  TStatutRow = (tsrROW_EMPTY, tsrROW_SERIE_0, tsrROW_HEADER_SERIE, tsrROW_STATION, tsrROW_INVALID);
type
  // On définit un type de fonction de comparaison qui sera utilisée
  // par la fonction de tri
  TFuncCompare = function (const Index1, Index2: Integer): Integer of object;

type

  { TCdrEditSeriesModeFlat }

  TCdrEditSeriesModeFlat = class(TFrame)
    acInsertLine: TAction;
    acRemoveLine: TAction;
    acAddLine: TAction;
    acContinueHere: TAction;
    acStartNewSerieHere: TAction;
    acGotoLine: TAction;
    acFindText: TAction;
    acSaveBuffer: TAction;
    acLoadBuffer: TAction;
    acCopierTableau: TAction;
    acParseTableau: TAction;
    acSortTableau: TAction;
    acRefresh: TAction;
    acEditSerieByDlg: TAction;
    acUndocopy: TAction;
    ActionList1: TActionList;
    //CdrListeSeries1:TCdrListeSeries;
    grdFlatSeries: TStringGrid;
    ImageList1: TImageList;
    lbNbMessages: TLabel;
    lbProcessings: TStaticText;
    lbInfos: TStaticText;
    lsbErreurs: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    pnlGrille: TPanel;
    pnlErreursACorriger: TPanel;
    popupMnuLigneStation: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
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
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    procedure acAddLineExecute(Sender: TObject);
    procedure acContinueHereExecute(Sender: TObject);
    procedure acCopierTableauExecute(Sender: TObject);

    procedure acFindTextExecute(Sender: TObject);
    procedure acGotoLineExecute(Sender: TObject);
    procedure acInsertLineExecute(Sender: TObject);
    procedure acLoadBufferExecute(Sender: TObject);
    procedure acParseTableauExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acRemoveLineExecute(Sender: TObject);
    procedure acSaveBufferExecute(Sender: TObject);
    procedure acSortTableauExecute(Sender: TObject);
    procedure acStartNewSerieHereExecute(Sender: TObject);
    procedure acUndocopyExecute(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure grdFlatSeriesClick(Sender: TObject);
    procedure grdFlatSeriesDblClick(Sender: TObject);
    procedure grdFlatSeriesDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure grdFlatSeriesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lsbErreursClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
  private
    { private declarations }
    FDocTopo: TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FCurrentStatutRow: TStatutRow;
    procedure AddMsgErreur(const NoLigne, NoColonne: integer; const Msg: string);
    procedure ChoisirSerie(const Idx: integer);
    function FindNoRowOfHeaderSerie(const SR: TObjSerie; out NumRow: integer): boolean;
    function GetStatutRow(const QRow: integer; out Msg: string): TStatutRow;
    function GridCheckErrorInGrdStations(const CurrColonne, CurrLigne: integer; const QValeur: double): boolean;
    procedure InitCaptions();
    function PreScanTableau(): boolean;
    procedure QSaveBuffer();

    function CheckAStation(const IdxGrdLigne: integer; const AStation: TUneVisee): boolean;
    function ComparerLignes(const Index1, Index2: Integer): Integer;
    function CreateNewSerie(const QNumSerie: integer; const StationArriveeConnue: boolean; const EWE: TStrings; out SR: TObjSerie): boolean;
    procedure DispEnTeteSerie(const SR: TObjSerie; const InternalIdx: integer);
    procedure DispLbInfos(const Msg: string);
    procedure DispVisee(const SR: TObjSerie; const VS: TUneVisee; const NoPt, InternalIdx: integer);
    function  getIdxLastNotFreeRow(): integer;
    procedure ImplementerModifsHeaderSerie(var SR: TObjSerie; const EWE: TStrings; const CurrentRow: integer);
    function  MakeTViseeFromGrd(const EWE: TStrings; const QNoVisee: integer; out V: TUneVisee): boolean;
    function  ParserTableau(): Integer;

    procedure SetHeaderSerie();
    procedure SetHeaderVisee();
    procedure TrierLaTable();
    procedure SortTable(L, R: Integer; const SCompare: TFuncCompare);
    procedure Undocopy(const MySelection: TGridRect; const AutoIncremented: boolean);

    procedure ReListerLeTableauDesSeries(const QIdx: integer);
  public
    { public declarations }
    function  Initialiser(const FD: TToporobotStructure2012): boolean;
    procedure Finaliser();
  end;

implementation

{$R *.lfm}
const
  DOSSIER_BUFFERS = 'Buffers_Series_Editor';
const
  COL_COMMENTAIRES               = 14;
const
  COL_SERIE_INDEX_SERIE          = 0;
  COL_SERIE_MARKER_HEADER_SERIE  = 1;
  COL_SERIE_STATION_DEPART       = 2;
  COL_SERIE_STATION_ARRIVEE      = 3;
  COL_SERIE_NUMERO_RESEAU        = 4;
  COL_SERIE_ENTREE_RATTACHEMENT  = 5;
  COL_SERIE_CHANCE               = 6;
  COL_SERIE_OBSTACLE             = 7;
  COL_SERIE_NOM_SERIE            = COL_COMMENTAIRES;
  COL_SERIE_COMMENTAIRES_SERIE   = COL_COMMENTAIRES + 1;
const
  COL_VISEE_SERIE         = 0; // Série
  COL_VISEE_POINT         = 1; // Point
  COL_VISEE_ID_TERRAIN    = 2; // ID terrain
  COL_VISEE_SECTEUR       = 3; // Secteur
  COL_VISEE_TYPE          = 4; // Type
  COL_VISEE_CODE          = 5; // Code
  COL_VISEE_EXPE          = 6; // Expé
  COL_VISEE_LONGUEUR      = 7; // Longueur
  COL_VISEE_AZIMUT        = 8; // Azimut
  COL_VISEE_PENTE         = 9; // Pente
  COL_VISEE_LG            = 10; // LG
  COL_VISEE_LD            = 11; // LD
  COL_VISEE_HZ            = 12; // HZ
  COL_VISEE_HN            = 13; // HN
  COL_VISEE_OBS           = COL_COMMENTAIRES; // Commentaires

function TCdrEditSeriesModeFlat.GetStatutRow(const QRow: integer; out Msg: string): TStatutRow;
var
  j: Integer;
  WU: String;
  p, q: LongInt;
begin
  result := tsrROW_INVALID;
  Msg := 'Ligne invalide';
  // on recherche si la ligne est vide
  WU := '';
  for j := 0 to grdFlatSeries.ColCount - 1 do WU += Trim(grdFlatSeries.Cells[j, QRow]);
  if (Trim(WU) = '') then
  begin
    Result := tsrROW_EMPTY;
    Msg := 'Ligne vide';
    Exit;
  end;
  // extraction série / point
  p := StrToIntDef(grdFlatSeries.Cells[COL_VISEE_SERIE, QRow], -1);
  q := StrToIntDef(grdFlatSeries.Cells[COL_VISEE_POINT, QRow],  0);
  // On vérifie si la ligne conserne la série 0 qui est non modifiable
  if (p = 0) then
  begin
    Result := tsrROW_SERIE_0;
    Msg := 'La ligne contient la série 0 non modifiable';
    Exit;
  end;
  // on vérifie si la ligne contient un header de série
  if ((p > 0) and (q = -1)) then
  begin
    Result := tsrROW_HEADER_SERIE;
    Msg := 'La ligne contient un header de série';
    Exit;
  end;
  // on vérifie si la ligne contient une station
  if ((p > 0) and (q >= 0)) then
  begin
    Result := tsrROW_STATION;
    Msg := 'La ligne contient une station';
    Exit;
  end;

end;
// préscan de la grille: recherches d'erreurs grossières dans le tableau-buffer
(*
 A ce stade, on n'implémente rien
 Le tableau est reconditionné
 Points contrôlés
 - Correspondance des types de données dans les cellules
   Si un nombre est attendu, la cellule doit contenir un nombre
 - Toutes les valeurs qui ne nécessitent pas l'accès à l'arbre des séries
   Séries:
   * Réseaux
   * Entrées
   * Chances, Obstacles
   * Coefficients de calcul
 - Stations:
   * Secteurs
   * Types de galeries
   * Codes
   * Expés
   * Longueurs, azimuts, angles en fonction des codes et expés DE LA GRILLE
 Retourne Vrai si les séries et stations sont bien formées
//*)
function TCdrEditSeriesModeFlat.PreScanTableau(): boolean;
var
  i: Integer;
begin
  Result := false;
  // le préscan peut effacer ou ajouter des lignes
  i := 0;
  while (i < grdFlatSeries.RowCount) do
  begin
    grdFlatSeries.Row := i;

    Inc(i);
  end;

end;
// recherche d'une ligne contenant un entete de série
function TCdrEditSeriesModeFlat.FindNoRowOfHeaderSerie(const SR: TObjSerie; out NumRow: integer): boolean;
var
  i   : Integer;
  EWE : TStrings;
  n   , p: LongInt;
begin
  result := False;
  for i := 1 to grdFlatSeries.RowCount - 1 do
  begin
    EWE := grdFlatSeries.Rows[i];
    n := StrToIntDef(EWE[0], -1);
    p := StrToIntDef(EWE[1], 0);
    if ((n > SR.GetNumeroDeSerie()) and (p = -1)) then
    begin
      NumRow := i;
      Result := true;
      Exit;
    end;
  end;
end;



// vérification des stations
procedure TCdrEditSeriesModeFlat.AddMsgErreur(const NoLigne, NoColonne: integer; const Msg: string);
begin
  lsbErreurs.Items.Add(Format('[%d, %d] %s', [NoLigne, NoColonne, GetResourceString(Msg)]));
  lbNbMessages.Caption := Format('%d occurrences', [lsbErreurs.Count]);
end;



function TCdrEditSeriesModeFlat.CheckAStation(const IdxGrdLigne: integer; const AStation: TUneVisee): boolean;
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
  Result := false;
  // AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));

  // TODO: entrées
  // TODO: réseaux
  // DONE: secteurs
  n := FDocTopo.GetNbSecteurs();
  if (not IsInRange(Ord(AStation.IDSecteur), 0, n - 1)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_SECTEUR, Format(rsCDR_SERIES_MSG_ERROR_NONEXISTENT_SECTEUR, [AStation.IDSecteur, 0, n - 1]));
  //if (not FDocuToporobot.ExistsIdxSecteur(AStation.IDSecteur)) then AddMsgErreur(i, NUM_COL_SECTEUR, Format(rsCDR_SERIES_MSG_ERROR_NONEXISTENT_SECTEUR, [AStation.IDSecteur]));
  // type de visée
  if (not IsInRange(Ord(AStation.TypeVisee), 0, 10)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_TYPE, Format(rsCDR_SERIES_MSG_ERROR_INVALID_TYPE_VISEE, [AStation.TypeVisee, 0, 10]));
  // longueur, ne doit pas dépasser 160 m
  AMin := 0.001; AMax := SEUIL_LONGUEUR_MAXI;
  if (not IsInRange(AStation.Longueur, AMin, AMax)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_LONGUEUR, Format(rsCDR_SERIES_MSG_ERROR_INVALID_LONGUEUR, [AStation.Longueur, AMin, AMax]));
  // largeurs et hauteurs
  AMin := 0.00; AMax := SEUIL_LONGUEUR_MAXI;
  if (not IsInRange(AStation.LG, AMin, AMax)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_LG, Format(rsCDR_SERIES_MSG_ERROR_INVALID_LG, [AStation.LG, AMin, AMax]));
  if (not IsInRange(AStation.LD, AMin, AMax)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_LD, Format(rsCDR_SERIES_MSG_ERROR_INVALID_LD, [AStation.LD, AMin, AMax]));
  if (not IsInRange(AStation.HZ, AMin, AMax)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_HZ, Format(rsCDR_SERIES_MSG_ERROR_INVALID_HZ, [AStation.HZ, AMin, AMax]));
  if (not IsInRange(AStation.HN, AMin, AMax)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_HN, Format(rsCDR_SERIES_MSG_ERROR_INVALID_HN, [AStation.HN, AMin, AMax]));

  // expés
  if (not FDocTopo.ExistsIdxExpe(AStation.Expe)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_EXPE, Format(rsCDR_SERIES_MSG_ERROR_NONEXISTENT_EXPE, [AStation.Expe]));
  // Des codes inexistants vides de sens tout contrôle de validité
  if (not FDocTopo.ExistsIdxCode(AStation.Code)) then
  begin
    AddMsgErreur(IdxGrdLigne, COL_VISEE_CODE,  Format(rsCDR_SERIES_MSG_ERROR_NONEXISTENT_CODE, [AStation.Code]));
  end
  else
  begin
    CC := FDocTopo.GetCodeByNumero(AStation.Code); // Fixé
    // azimuts
    AMin := 0.00;            DEGRES_PAR_TOUR;
    case Trunc(CC.GradAz) of
      359, 360: AMax := DEGRES_PAR_TOUR; // directes degrés
      399, 400: AMax := GRADES_PAR_TOUR; // directes grades
      349, 350: AMax := DEGRES_PAR_TOUR; // inverses degrés
      389, 390: AMax := GRADES_PAR_TOUR; // inverses grades
    end;
    if (not IsInRange(AStation.Azimut, AMin, AMax)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_AZIMUT, Format(rsCDR_SERIES_MSG_ERROR_AZIMUT_OUT_OF_RANGE, [AStation.Azimut, AMin, AMax]));
    // pentes

    case Trunc(CC.GradInc) of
      360: SetAMinAMax(-QUADRANT_DEGRES, QUADRANT_DEGRES);
      400: SetAMinAMax(-QUADRANT_GRADES, QUADRANT_GRADES);
      361: SetAMinAMax(0.00, ANGLE_JANE_BRIKIN_DEGRES); // zéro zénithal degrés
      401: SetAMinAMax(0.00, 200.00); // zéro zénithal grades
      359: SetAMinAMax(0.00, ANGLE_JANE_BRIKIN_DEGRES); // zéro nadiral degrés
      399: SetAMinAMax(0.00, ANGLE_JANE_BRIKIN_GRADES); // zéro nadiral grades
      370: SetAMinAMax(-580.00, 580.00); // pourcentage; +/- 80° max
      380: SetAMinAMax(-SEUIL_LONGUEUR_MAXI, SEUIL_LONGUEUR_MAXI); // dénivelés; maxi=long max visée
      350: SetAMinAMax(-QUADRANT_DEGRES, QUADRANT_DEGRES);   // zéro horizontal degré, inverse
      390: SetAMinAMax(-QUADRANT_GRADES, QUADRANT_GRADES); // zéro horizontal grade, inverse
    else
      SetAMinAMax(-100.00, 100.00);
    end;
    if (not IsInRange(AStation.Pente, AMin, AMax)) then AddMsgErreur(IdxGrdLigne, COL_VISEE_PENTE,  Format(rsCDR_SERIES_MSG_ERROR_PENTE_OUT_OF_RANGE, [AStation.Pente, AMin, AMax]));
  end;
end;

// retourne la dernière ligne non vide
function TCdrEditSeriesModeFlat.getIdxLastNotFreeRow(): integer;
var
  n, i: Integer;
  Msg: String;
begin
  n := grdFlatSeries.RowCount - 1;
  Result := n - 1;
  for i := n-1 downto 1 do
  begin
    Result := i;
    if (GetStatutRow(i, Msg) <> tsrROW_EMPTY) then Exit;
  end;
end;

procedure TCdrEditSeriesModeFlat.acAddLineExecute(Sender: TObject);
begin
  grdFlatSeries.InsertColRow(false, grdFlatSeries.RowCount - 1);
  grdFlatSeries.Row := grdFlatSeries.RowCount - 1;
end;

procedure TCdrEditSeriesModeFlat.acContinueHereExecute(Sender: TObject);
var
  n, p, q: Integer;
  WU: TStatutRow;
  Msg: string;
begin
  if (not GHTopoQuestionOuiNon(rsCDR_SERIES_FLAT_MSG_CONTINUE_THIS_ACTION)) then exit;
  Msg := '';
  n := getIdxLastNotFreeRow() + 1;
  p := grdFlatSeries.Row;

  ShowMessageFmt('n=%d', [n]);
  WU  := GetStatutRow(grdFlatSeries.Row, Msg);
  // seules les lignes contenant une station sont utilisables
  if (WU <> tsrROW_STATION) then
  begin
    ShowMessage(Msg);
    Exit;
  end;
  q   := 1 + StrToIntDef(Trim(grdFlatSeries.Cells[1, grdFlatSeries.Row])   , 0);
  grdFlatSeries.Cells[COL_VISEE_SERIE   , n] := grdFlatSeries.Cells[COL_VISEE_SERIE    , p]; // rappel du numéro de série
  grdFlatSeries.Cells[COL_VISEE_POINT   , n] := Format(FORMAT_NB_INTEGER, [q]);
  grdFlatSeries.Cells[COL_VISEE_SECTEUR , n] := grdFlatSeries.Cells[COL_VISEE_SECTEUR  , p];  // rappel du secteur
  grdFlatSeries.Cells[COL_VISEE_TYPE    , n] := grdFlatSeries.Cells[COL_VISEE_TYPE     , p];  // rappel du type de visee

  grdFlatSeries.Cells[COL_VISEE_CODE    , n] := grdFlatSeries.Cells[COL_VISEE_CODE     , p];  // rappel du code
  grdFlatSeries.Cells[COL_VISEE_EXPE    , n] := grdFlatSeries.Cells[COL_VISEE_EXPE     , p];  // rappel du expe

end;

procedure TCdrEditSeriesModeFlat.acCopierTableauExecute(Sender: TObject);
begin
  grdFlatSeries.CopyToClipboard(false);
end;



procedure TCdrEditSeriesModeFlat.acFindTextExecute(Sender: TObject);
var
  QTextToFind: string;
  EWE: TStrings;
  i, j: Integer;
begin
  // On recherche ici toutes les occurences du texte
  // et on les envoie sur la console interne (celle située sous la grille)
  // même fonctionnement que la fonction 'Chercher dans les fichiers' de Lazarus
  if (GHTopoInputQuery('Recherche', 'Texte à rechercher', QTextToFind)) then
  begin
    QTextToFind := LowerCase(Trim(QTextToFind));
    lsbErreurs.Clear;
    for i := 0 to grdFlatSeries.RowCount - 1 do
    begin
      EWE := grdFlatSeries.Rows[i];
      // recherche bête et méchante ...
      for j := 0 to EWE.Count - 1 do
      begin
        if (Pos(QTextToFind, LowerCase(EWE[j])) > 0) then
        begin
          AddMsgErreur(i, j, EWE[j]);
          lbNbMessages.Caption := Format('%d occurrences', [lsbErreurs.Count]);
        end;
      end;
    end;
    if (lsbErreurs.Count = 0) then ShowMessage(GetResourceString(rsMATCHNOTFOUND));
  end;
end;

procedure TCdrEditSeriesModeFlat.acGotoLineExecute(Sender: TObject);
var
  QValeur: string;
  n: LongInt;
begin
  if (GHTopoInputQuery('Aller à la ligne', 'Numéro de ligne', QValeur)) then
  begin
    n := StrToIntDef(QValeur, grdFlatSeries.Row); // si échec, on reste sur place
    if (n < grdFlatSeries.RowCount) then GridSetCurrentRow(grdFlatSeries, n, 0);
  end;
end;

procedure TCdrEditSeriesModeFlat.acInsertLineExecute(Sender: TObject);
begin
  // pas besoin de confirmation (opération réversible)
  GridInsertLignes(grdFlatSeries);
end;

procedure TCdrEditSeriesModeFlat.acLoadBufferExecute(Sender: TObject);
var
  OD: TOpenDialog;
begin
  // pas d'utilisation de DoDialogOpenFile(): des paramètres supplémentaires sont utilisés
  if (not GHTopoQuestionOuiNon(rsCDR_SERIES_FLAT_MSG_REPLACE_BUFFER)) then Exit;
  OD := TOpenDialog.Create(Application);
  try
    OD.InitialDir := GetGHTopoDirectory + DOSSIER_BUFFERS;
    OD.Filter     := 'Fichiers CSV|*.csv|Tous|*.*';
    OD.DefaultExt := '.csv';

    //if (OD.Execute) then grdFlatSeries.LoadFromCSVFile(OD.FileName, #9, True, 0, false);
    if (OD.Execute) then grdFlatSeries.LoadFromCSVFile(OD.FileName, #9, false);
  finally
    FreeAndNil(OD);// OD.Free;
  end;
end;

procedure TCdrEditSeriesModeFlat.acParseTableauExecute(Sender: TObject);
begin
  QSaveBuffer();
  ParserTableau();
  ReListerLeTableauDesSeries(1);
  lbProcessings.Caption := '';
  pnlErreursACorriger.Color := IIF(lsbErreurs.Count = 0, clSilver, clYellow);
  lbProcessings.Caption := '';
end;

procedure TCdrEditSeriesModeFlat.acRefreshExecute(Sender: TObject);
begin
  ReListerLeTableauDesSeries(0);
  ReListerLeTableauDesSeries(0);
end;

procedure TCdrEditSeriesModeFlat.acRemoveLineExecute(Sender: TObject);
var
  EWE: TStrings;
  SD, SP: LongInt;
begin
  // on détecte si la ligne contient un entete de série
  EWE := grdFlatSeries.Rows[grdFlatSeries.Row];
  SD := StrToIntDef(EWE[0], 0);
  SP := StrToIntDef(EWE[1], 0);
  // des précautions sont à prendre pour les lignes contenant l'entête d'une série
  if ((SD > 0) and (SP = -1)) then
  begin
    ShowMessageFmt(GetResourceString(rsCDR_SERIES_FLAT_MSG_ROW_UNDELETABLE), [grdFlatSeries.Row]);
    Exit;
  end;

  // La confirmation de la suppression se fait dans la fonction suivante:
  GridEraseLines(grdFlatSeries);
end;
// sauvegarde horodatée du buffer en CSV brut
procedure TCdrEditSeriesModeFlat.QSaveBuffer();
var
  WU, EWE, QFileName: String;
begin
  // sauvegarde le buffer tableau brut en tant que CSV
  WU := GetGHTopoDirectory + DOSSIER_BUFFERS;
  EWE := Format('Buffer_%s.csv', [DatePascalToDateHeureCondensee(Now)]);
  ForceDirectories(WU);
  QFileName := WU + PathDelim + EWE;
  grdFlatSeries.SaveToCSVFile(QFileName, #9);
  AfficherMessage(Format(GetResourceString(rsCDR_SERIES_FLAT_MSG_BUFFER_SAVED), [EWE, DateTimeToStr(Now())]));
end;

procedure TCdrEditSeriesModeFlat.acSaveBufferExecute(Sender: TObject);
begin
  QSaveBuffer();
end;

procedure TCdrEditSeriesModeFlat.acSortTableauExecute(Sender: TObject);
begin
  TrierLaTable();
end;

procedure TCdrEditSeriesModeFlat.acStartNewSerieHereExecute(Sender: TObject);
var
  R, QNoSer: Integer;
  EWE: TStrings;
  SR: TObjSerie;
begin
  if (not GHTopoQuestionOuiNon(rsCDR_SERIES_FLAT_MSG_CONTINUE_THIS_ACTION)) then exit;
  R := grdFlatSeries.Row;
  EWE := grdFlatSeries.Rows[R];
  QNoSer := FDocTopo.getMaxIdxSerie() + 1;
  if (CreateNewSerie(QNoSer, False, EWE, SR)) then
  begin
    FDocTopo.AddSerie(SR);
    ReListerLeTableauDesSeries(FDocTopo.GetNbSeries() - 1);
    if (FindNoRowOfHeaderSerie(SR, R)) then GridSetCurrentRow(grdFlatSeries, R, 0);
  end;
end;

procedure TCdrEditSeriesModeFlat.acUndocopyExecute(Sender: TObject);
begin
  if (not GHTopoQuestionOuiNon(rsCDR_SERIES_FLAT_MSG_CONTINUE_THIS_ACTION)) then exit;
  GridUndocopy(grdFlatSeries, True);
end;


function TCdrEditSeriesModeFlat.MakeTViseeFromGrd(const EWE: TStrings; const QNoVisee: integer; out V: TUneVisee): boolean;
begin
  Result := False;
  try
    V := EmptyVisee('');
    V.NoVisee           := QNoVisee;
    V.IDTerrainStation  := Trim(EWE[COL_VISEE_ID_TERRAIN  ]); // 'ID terrain';
    V.IDSecteur         := StrToIntDef(EWE[COL_VISEE_SECTEUR     ], 0); // 'Secteur';
    V.TypeVisee         := TTypeDeVisee(StrToIntDef(EWE[COL_VISEE_TYPE        ], 0)); // 'Type';
    V.Code              := StrToIntDef(EWE[COL_VISEE_CODE        ], 0); // 'Code';
    V.Expe              := StrToIntDef(EWE[COL_VISEE_EXPE        ], 0); // 'Expé';
    V.setLongAzInc(EWE[COL_VISEE_LONGUEUR],
                   EWE[COL_VISEE_AZIMUT  ],
                   EWE[COL_VISEE_PENTE   ]);
    V.setLRUD(ConvertirEnNombreReel(EWE[COL_VISEE_LG          ], 0.00); // 'LG';
              ConvertirEnNombreReel(EWE[COL_VISEE_LD          ], 0.00); // 'LD';
    V.HZ                := ConvertirEnNombreReel(EWE[COL_VISEE_HZ          ], 0.00); // 'HZ';
    V.HN                := ConvertirEnNombreReel(EWE[COL_VISEE_HN          ], 0.00); // 'HN';
    V.Commentaires      := Trim(EWE[COL_VISEE_OBS]);
    Result := True;
  except
    ;
  end;
end;


function TCdrEditSeriesModeFlat.CreateNewSerie(const QNumSerie: integer; const StationArriveeConnue: boolean; const EWE: TStrings; out SR: TObjSerie): boolean;
var
  SD, PD, CD, EX: LongInt;
  SA, PA: Integer;
  WU: String;
begin
  Result := false;
  SR := TObjSerie.Create;
  try
    SR.ClearStations();                                      // Opération 1: Vider la liste des visées
    SR.SetChanceObstacle(0, 0);                              // Opération 2: Chances et obstacles
    SD := StrToIntDef(Trim(EWE[0]), 0);
    PD := StrToIntDef(Trim(EWE[1]), 0);
    SR.SetSeriePtDep(SD, PD);                                // Opération 3a: Départ de la série
    SR.SetNumeroSerie(QNumSerie);                            // Opération 4: Numéro de série
    WU := GetResourceString(rsCDR_SERIES_FLAT_NEW_SERIE);
    SR.SetNomObsSerie(WU, WU);                               // Opération 5: Nom et observations sur la série
    SR.SetRaideur(1.00);                                     // Opération 6: Raideur
    SR.SetCouleur(clBlack);                                  // Opération 7: Couleur
    SR.SetNumeroEntrance(0);                                 // Opération 8: Entrée de rattachement
    SR.SetNumeroReseau(0);                                   // Opération 9: Réseau de rattachement

    // Opération 10: Traitement des visées
    CD := StrToIntDef(Trim(EWE[5]), 0);
    EX := StrToIntDef(Trim(EWE[6]), 0);

    SR.AddVisee(0, 0,
                CD, EX, tgDEFAULT,
                0.01, 0.01, -0.01,
                0.00, 0.00, 0.00, 0.00,
                EWE[2],
                '');
    // affectation du point d'arrivée
    if (StationArriveeConnue) then
    begin
      // TODO
      SA := 1;
      PA := 0;
    end
    else
    begin
      SA := QNumSerie;
      PA := SR.GetNbVisees();
    end;
    SR.SetSeriePtArr(SA, PA);                                // Opération 3b: Fin de la série
    Result := True;
  except
  end;
end;



function TCdrEditSeriesModeFlat.ParserTableau(): Integer;
var
  i, Nb, QCurrentNumeroSerie, n, QInternalIdxSerie: Integer;
  EWE: TStrings;
  QNoSer: LongInt;
  QIsLineHeaderSerie: Boolean;
  SR: TObjSerie;
  MyVisee: TUneVisee;
begin
  SR := nil;
  pnlErreursACorriger.Color := clYellow;
  lsbErreurs.Clear;
  lbProcessings.Caption := GetResourceString(rsCDR_SERIES_FLAT_MSG_PARSING);
  AfficherMessage(Format('%s.ParserTableau()', [ClassName]));
  Result := 0;
  // d'abord, on trie
  TrierLaTable();
  Nb := grdFlatSeries.RowCount;
  QCurrentNumeroSerie := -1;
  try
    for i := 1 to Nb - 1 do
    begin
      if (i mod 200 = 0) then lbProcessings.Caption := Format(GetResourceString(rsCDR_SERIES_FLAT_MSG_PARSING_LINES), [i]);
      EWE := grdFlatSeries.Rows[i];
      QNoSer := StrToIntDef(EWE[0], -1);
      QIsLineHeaderSerie := (StrToIntDef(EWE[1], 0) = -1);
      if (QIsLineHeaderSerie and (QNoSer > 0)) then
      begin
        if (FDocTopo.GetSerieByNumeroSerie(QNoSer, SR, QInternalIdxSerie)) then
        begin
          AfficherMessage(Format('-- [%d] %d - %d.%d to %d.%d - %s', [QInternalIdxSerie, SR.GetNumeroDeSerie(), SR.GetNoSerieDep(), SR.GetNoPointDep(), SR.GetNoSerieArr(), SR.GetNoPointArr(), SR.GetNomSerie()]));
          QCurrentNumeroSerie := QNoSer;
          // modifier l'en-tête en extrayant les valeurs des colonnes
          ImplementerModifsHeaderSerie(SR, EWE, i);
          SR.ClearStations();
        end
        else
        begin
          if (GHTopoQuestionOuiNon(Format(rsCDR_SERIES_FLAT_MSG_DO_CREATE_SERIE_NTH, [QNoSer]))) then
          begin
            pass;
          end;
        end;
      end
      else
      begin
        QNoSer := StrToIntDef(EWE[0], -2);
        if (QCurrentNumeroSerie = QNoSer) then
        begin
          n := SR.GetNbVisees() - 1;
          if (MakeTViseeFromGrd(EWE, n, MyVisee)) then
          begin
            CheckAStation(i, MyVisee);
            SR.AddVisee(MyVisee);
          end;
        end;
      end;
    end;

  except
    ; //ShowMessageFmt('Ligne %d', [i]);
  end;
  // traiter les extrémités libres
  n := FDocTopo.GetNbSeries();
  for i := 0 to n - 1 do
  begin
    SR := FDocTopo.GetSerie(i);
    if (SR.GetNoPointArr() = -1) then SR.SetNoPointArr(SR.GetNbVisees() - 1);
  end;
end;

{ TCdrEditSeriesModeFlat }


function TCdrEditSeriesModeFlat.ComparerLignes(const Index1, Index2: Integer): Integer;
const QMaxInt = -9999999;
var
  WU1, WU2: Int64;
begin
  WU1 := StrToIntDef(grdFlatSeries.Cells[0, Index1], QMaxInt) shl 16 +
         StrToIntDef(grdFlatSeries.Cells[1, Index1], 0);
  WU2 := StrToIntDef(grdFlatSeries.Cells[0, Index2], QMaxInt) shl 16 +
         StrToIntDef(grdFlatSeries.Cells[1, Index2], 0);
  if      (WU1 < WU2) then Result := -1
  else if (WU1 = WU2) then Result :=  0
  else                     Result :=  1;
end;

procedure TCdrEditSeriesModeFlat.ChoisirSerie(const Idx: integer);
var
  EWE: TObjSerie;
  QIdx: integer;
begin
  if (Idx < 1) then Exit;
  EWE := FDocTopo.GetSerie(Idx - 1);
  if (FindNoRowOfHeaderSerie(EWE, QIdx)) then GridSetCurrentRow(grdFlatSeries, QIdx, 0);
end;

function TCdrEditSeriesModeFlat.Initialiser(const FD: TToporobotStructure2012): boolean;
begin
  Result := false;
  FDocTopo := FD;
  FCurrentStatutRow := tsrROW_SERIE_0;
  InitCaptions();

  grdFlatSeries.Options := [goEditing,
                            //goAlwaysShowEditor,
                            goRangeSelect,
                            goAutoAddRows,
                            goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
                            goRowHighlight,
                            goCellHints,
                            //goHeaderHotTracking, goHeaderPushedLook,
                            goTabs]; // ,goAlwaysShowEditor
  grdFlatSeries.FixedCols := 0;
  grdFlatSeries.FixedRows := 1;
  grdFlatSeries.ColCount  := 21;
  grdFlatSeries.RowCount  := 50;
  ReListerLeTableauDesSeries(0);
  result := True;

end;

procedure TCdrEditSeriesModeFlat.Finaliser();
begin

end;

procedure TCdrEditSeriesModeFlat.InitCaptions();
  procedure S777(const AC: TAction; const QCaption: string);
  begin
    AC.Caption := GetResourceString(QCaption);
    AC.Hint    := AC.Caption;
  end;
begin
  S777(acLoadBuffer               , rsCDR_SERIES_FLAT_TAB_LOAD_BUFFER);
  S777(acSaveBuffer               , rsCDR_SERIES_FLAT_TAB_SAVE_BUFFER);
  S777(acGotoLine                 , rsCDR_SERIES_FLAT_TAB_GOTO_LINE);
  S777(acFindText                 , rsCDR_SERIES_FLAT_TAB_FIND);

  S777(acInsertLine               , rsCDR_SERIES_FLAT_TAB_INSERT_LINE);
  S777(acRemoveLine               , rsCDR_SERIES_FLAT_TAB_REMOVE_LINE);
  S777(acUndocopy                 , rsCDR_SERIES_FLAT_TAB_UNDOCOPY);

  S777(acStartNewSerieHere        , rsCDR_SERIES_FLAT_TAB_INSERT_SERIE_HERE);
  S777(acContinueHere             , rsCDR_SERIES_FLAT_TAB_CONTINUE_HERE);

  S777(acParseTableau             , rsCDR_SERIES_FLAT_TAB_PARSE_TABLEAU);
  S777(acCopierTableau            , rsCDR_SERIES_FLAT_TAB_COPY_TABLEAU);
  S777(acSortTableau              , rsCDR_SERIES_FLAT_TAB_SORT_TABLEAU);

  S777(acEditSerieByDlg           , rsCDR_SERIES_FLAT_EDIT_SERIE_BY_DIALOG);
end;

// recopie vers le bas
procedure TCdrEditSeriesModeFlat.Undocopy(const MySelection: TGridRect; const AutoIncremented: boolean);
var
  i, j, Nb: integer;
  WU: integer;
begin
  Nb := MySelection.Bottom - MySelection.Top;
  if (Nb = 0) then Nb := StrToIntDef(InputBox(acUndoCopy.Caption, GetResourceString(rsCDR_SERIES_FLAT_MSG_NB_LINES), IntToStr(Nb)), 1);
  WU := MySelection.Top + Nb;
  if (WU > (grdFlatSeries.RowCount -1)) then WU := grdFlatSeries.RowCount - 1;
  for i := MySelection.Top + 1 to WU do
    for j := MySelection.Left to MySelection.Right do
      grdFlatSeries.Cells[j, i] := grdFlatSeries.Cells[j, MySelection.Top];
  // auto incrémentation
  (*
  if (AutoIncremented and (MySelection.Left = NUM_COL_IDTERRAIN)) then
  begin
    for i := MySelection.Top + 1 to WU do
    begin
      grdFlatSeries.Cells[NUM_COL_IDTERRAIN, i] := IncrementString(grdFlatSeries.Cells[NUM_COL_IDTERRAIN, i - 1]);
    end;
  end;
  //*)
end;


procedure TCdrEditSeriesModeFlat.lsbErreursClick(Sender: TObject);
var
  EWE: String;
  p: SizeInt;
  WU: TGHStringArray;
  r, c: LongInt;
begin
  if (lsbErreurs.Items.Count = 0) then Exit;
  try
    EWE := lsbErreurs.Items[lsbErreurs.ItemIndex];
    p := Pos(']', EWE);
    EWE := Copy(EWE, 1, p-1);
    System.Delete(EWE, 1, 1);
    WU  := split(EWE, ',');
    r := StrToIntDef(WU[0], 0);
    c := StrToIntDef(WU[1], 0);
    GridSetCurrentRow(grdFlatSeries, r, c);

  except
  end;
end;



// tri du tableau

procedure TCdrEditSeriesModeFlat.TrierLaTable();
begin
  lbProcessings.Caption := 'Tri en cours ...';
  grdFlatSeries.Enabled := false;
  self.SortTable(1, grdFlatSeries.RowCount - 1, self.ComparerLignes);
  grdFlatSeries.Enabled := True;
  lbProcessings.Caption := '';
end;
{ La fonction de tri proprement dite : un tri rapide (Quick Sort) }
procedure TCdrEditSeriesModeFlat.SortTable(L, R: Integer; const SCompare: TFuncCompare);

  { Il nous faut une fonction pour échanger les lignes }
  procedure ExchangeItems(Index1, Index2: Integer); inline;
  begin
    grdFlatSeries.ExchangeColRow(false, Index1, Index2);
  end;
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(I, P) < 0 do Inc(I);
      while SCompare(J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then SortTable(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TCdrEditSeriesModeFlat.SpeedButton4Click(Sender: TObject);
begin

end;

//******************************************************************************


procedure TCdrEditSeriesModeFlat.DispEnTeteSerie(const SR: TObjSerie; const InternalIdx: integer);
begin
  grdFlatSeries.Cells[COL_SERIE_INDEX_SERIE            , InternalIdx] := Format(FORMAT_NB_INTEGER, [SR.GetNumeroDeSerie()]);
  grdFlatSeries.Cells[COL_SERIE_MARKER_HEADER_SERIE    , InternalIdx] := Format(FORMAT_NB_INTEGER, [-1]);
  grdFlatSeries.Cells[COL_SERIE_STATION_DEPART         , InternalIdx] := Format('%d.%d', [SR.GetNoSerieDep(), SR.GetNoPointDep()]);
  grdFlatSeries.Cells[COL_SERIE_STATION_ARRIVEE        , InternalIdx] := Format('%d.%d', [SR.GetNoSerieArr(), SR.GetNoPointArr()]);
  grdFlatSeries.Cells[COL_SERIE_NUMERO_RESEAU          , InternalIdx] := Format(FORMAT_NB_INTEGER, [SR.GetNumeroReseau()]);
  grdFlatSeries.Cells[COL_SERIE_ENTREE_RATTACHEMENT    , InternalIdx] := Format(FORMAT_NB_INTEGER, [SR.GetNumeroEntrance()]);
  grdFlatSeries.Cells[COL_SERIE_CHANCE                 , InternalIdx] := Format(FORMAT_NB_INTEGER, [SR.GetChance()]);
  grdFlatSeries.Cells[COL_SERIE_OBSTACLE               , InternalIdx] := Format(FORMAT_NB_INTEGER, [SR.GetObstacle()]);
  grdFlatSeries.Cells[COL_SERIE_NOM_SERIE              , InternalIdx] := Format(FORMAT_STRING, [SR.GetNomSerie()]);
  grdFlatSeries.Cells[COL_SERIE_COMMENTAIRES_SERIE     , InternalIdx] := Format('%s', [SR.GetObsSerie()]);
end;

procedure TCdrEditSeriesModeFlat.SetHeaderSerie();
var
  i: Integer;
begin
  for i := 0 to grdFlatSeries.ColCount - 1 do grdFlatSeries.Cells[i, 0] := '';
  grdFlatSeries.Cells[COL_SERIE_INDEX_SERIE          , 0] := GetResourceString(rsCDR_SERIE_NUMERO); //'Série';
  grdFlatSeries.Cells[COL_SERIE_MARKER_HEADER_SERIE  , 0] := GetResourceString('Flag');
  grdFlatSeries.Cells[COL_SERIE_STATION_DEPART       , 0] := GetResourceString(rsCDR_SERIE_DEPART);
  grdFlatSeries.Cells[COL_SERIE_STATION_ARRIVEE      , 0] := GetResourceString(rsCDR_SERIE_ARRIVEE);
  grdFlatSeries.Cells[COL_SERIE_NUMERO_RESEAU        , 0] := GetResourceString(rsCDR_SERIE_LB_RESEAU);
  grdFlatSeries.Cells[COL_SERIE_ENTREE_RATTACHEMENT  , 0] := GetResourceString(rsCDR_ENTR_NOENTRANCE);
  grdFlatSeries.Cells[COL_SERIE_CHANCE               , 0] := GetResourceString(rsCDR_SERIE_CHANCE);
  grdFlatSeries.Cells[COL_SERIE_OBSTACLE             , 0] := GetResourceString(rsCDR_SERIE_OBSTACLE);
  grdFlatSeries.Cells[COL_SERIE_NOM_SERIE            , 0] := GetResourceString(rsCDR_SERIE_NAME);
  grdFlatSeries.Cells[COL_SERIE_COMMENTAIRES_SERIE   , 0] := GetResourceString(rsOBS);
end;
procedure TCdrEditSeriesModeFlat.SetHeaderVisee();
var
  i: Integer;
begin
  for i := 0 to grdFlatSeries.ColCount - 1 do grdFlatSeries.Cells[i, 0] := '';
  grdFlatSeries.Cells[COL_VISEE_SERIE       , 0] := GetResourceString(rsCDR_SERIE_NUMERO);
  grdFlatSeries.Cells[COL_VISEE_POINT       , 0] := GetResourceString(rsCDR_SERIE_COL_POINT);
  grdFlatSeries.Cells[COL_VISEE_ID_TERRAIN  , 0] := GetResourceString(rsCDR_SERIE_COL_ID_TERRAIN);
  grdFlatSeries.Cells[COL_VISEE_SECTEUR     , 0] := GetResourceString(rsCDR_SERIE_COL_SECTEUR);
  grdFlatSeries.Cells[COL_VISEE_TYPE        , 0] := GetResourceString(rsCDR_SERIE_COL_TYPE);
  grdFlatSeries.Cells[COL_VISEE_CODE        , 0] := GetResourceString(rsCDR_SERIE_COL_CODE);
  grdFlatSeries.Cells[COL_VISEE_EXPE        , 0] := GetResourceString(rsCDR_SERIE_COL_EXPE);
  grdFlatSeries.Cells[COL_VISEE_LONGUEUR    , 0] := GetResourceString(rsCDR_SERIE_COL_LEGNTH);
  grdFlatSeries.Cells[COL_VISEE_AZIMUT      , 0] := GetResourceString(rsCDR_SERIE_COL_AZIMUTH);
  grdFlatSeries.Cells[COL_VISEE_PENTE       , 0] := GetResourceString(rsCDR_SERIE_COL_INCLIN);
  grdFlatSeries.Cells[COL_VISEE_LG          , 0] := GetResourceString(rsCDR_SERIE_COL_LG);
  grdFlatSeries.Cells[COL_VISEE_LD          , 0] := GetResourceString(rsCDR_SERIE_COL_LD);
  grdFlatSeries.Cells[COL_VISEE_HZ          , 0] := GetResourceString(rsCDR_SERIE_COL_HZ);
  grdFlatSeries.Cells[COL_VISEE_HN          , 0] := GetResourceString(rsCDR_SERIE_COL_HN);
  grdFlatSeries.Cells[COL_VISEE_OBS         , 0] := GetResourceString(rsOBS);
end;
// /!\ : SR est un POINTEUR. Toutes les modifs sur cette variable sont implémentées dans la base
procedure TCdrEditSeriesModeFlat.ImplementerModifsHeaderSerie(var SR: TObjSerie; const EWE: TStrings; const CurrentRow: integer);
var
  WU: String;
  S, P: integer;
  n: LongInt;
begin
  //DecomposeStationToporobot(const S: string): TToporobotStation;
  SR.SetNomObsSerie(Trim(EWE[COL_SERIE_NOM_SERIE]), Trim(EWE[COL_SERIE_COMMENTAIRES_SERIE]));
  // extraction station début
  WU := Trim(EWE[COL_SERIE_STATION_DEPART]);
  if (FDocTopo.FindPtTopoByCle(false, WU, S, P)) then
    SR.SetSeriePtDep(S, P)
  else if (SR.GetNoPointDep() = 0) then
    AddMsgErreur(CurrentRow, COL_SERIE_STATION_DEPART, Format(GetResourceString(rsWARN_SERIE_REVERSE),
                                                              [SR.GetNumeroDeSerie(),
                                                               SR.GetNoSerieArr(), SR.GetNoPointArr()]))
  else
    AddMsgErreur(CurrentRow, COL_SERIE_STATION_DEPART, GetResourceString(rsERR_SERIE_START_STATION_NOT_FOUND));

  // extraction station fin
  WU := Trim(EWE[COL_SERIE_STATION_ARRIVEE]);
  if (FDocTopo.FindPtTopoByCle(false, WU, S, P)) then
  begin
    SR.SetSeriePtArr(S, P);
  end
  else if ((SR.GetNoSerieArr() <> SR.GetNumeroDeSerie()) and (SR.GetNoPointArr() = 0)) then
  begin
   AddMsgErreur(CurrentRow, COL_SERIE_STATION_ARRIVEE, Format(GetResourceString(rsWARN_SERIE_CONNECTED_FIXPT),
                                                              [SR.GetNumeroDeSerie(),
                                                               SR.GetNoSerieArr(), SR.GetNoPointArr()]));
  end
  else
  begin
    SR.SetSeriePtArr(SR.GetNumeroDeSerie(), -1);
    AddMsgErreur(CurrentRow, COL_SERIE_STATION_ARRIVEE, GetResourceString(rsERR_SERIE_END_STATION_NOT_FOUND));
  end;
  // reseaux
  n := StrToIntDef(EWE[COL_SERIE_NUMERO_RESEAU], 0);
  SR.SetNumeroReseau(n);
  if (not IsInRange(n, 0, FDocTopo.GetNbReseaux() - 1)) then
    AddMsgErreur(CurrentRow, COL_SERIE_NUMERO_RESEAU, GetResourceString(rsERR_RESEAU_NOT_FOUND));
  // entrée de rattachement
  n := StrToIntDef(EWE[COL_SERIE_ENTREE_RATTACHEMENT], 0);
  SR.SetNumeroEntrance(n);
  if (not IsInRange(n, 0, FDocTopo.GetNbEntrees() - 1)) then
    AddMsgErreur(CurrentRow, COL_SERIE_ENTREE_RATTACHEMENT, GetResourceString(rsERR_ENTRANCE_NOT_FOUND));
  // chance
  n := StrToIntDef(EWE[COL_SERIE_CHANCE], 0);
  SR.SetChance(n);
  // obstacle
  n := StrToIntDef(EWE[COL_SERIE_OBSTACLE], 0);
  SR.SetObstacle(n);
end;

procedure TCdrEditSeriesModeFlat.DispVisee(const SR: TObjSerie; const VS: TUneVisee; const NoPt, InternalIdx: integer);
begin
  grdFlatSeries.Cells[COL_VISEE_SERIE      , InternalIdx] := Format(FORMAT_NB_INTEGER, [SR.GetNumeroDeSerie()]);
  grdFlatSeries.Cells[COL_VISEE_POINT      , InternalIdx] := Format(FORMAT_NB_INTEGER, [NoPt]);
  grdFlatSeries.Cells[COL_VISEE_ID_TERRAIN , InternalIdx] := Format('%s', [VS.IDTerrainStation]);
  grdFlatSeries.Cells[COL_VISEE_SECTEUR    , InternalIdx] := Format(FORMAT_NB_INTEGER, [VS.IDSecteur]);
  grdFlatSeries.Cells[COL_VISEE_TYPE       , InternalIdx] := Format(FORMAT_NB_INTEGER, [VS.TypeVisee]);
  grdFlatSeries.Cells[COL_VISEE_CODE       , InternalIdx] := Format(FORMAT_NB_INTEGER, [VS.Code]);
  grdFlatSeries.Cells[COL_VISEE_EXPE       , InternalIdx] := Format(FORMAT_NB_INTEGER, [VS.Expe]);
  grdFlatSeries.Cells[COL_VISEE_LONGUEUR   , InternalIdx] := FormatterNombreOOo(VS.Longueur, 3, false);
  grdFlatSeries.Cells[COL_VISEE_AZIMUT     , InternalIdx] := FormatterNombreOOo(VS.Azimut, 3, False);
  grdFlatSeries.Cells[COL_VISEE_PENTE      , InternalIdx] := FormatterNombreOOo(VS.Pente, 3, False);
  grdFlatSeries.Cells[COL_VISEE_LG         , InternalIdx] := FormatterNombreOOo(VS.LG, 3, False);
  grdFlatSeries.Cells[COL_VISEE_LD         , InternalIdx] := FormatterNombreOOo(VS.LD, 3, False);
  grdFlatSeries.Cells[COL_VISEE_HZ         , InternalIdx] := FormatterNombreOOo(VS.HZ, 3, False);
  grdFlatSeries.Cells[COL_VISEE_HN         , InternalIdx] := FormatterNombreOOo(VS.HN, 3, False);
  grdFlatSeries.Cells[COL_VISEE_OBS        , InternalIdx] := Format('%s', [VS.Commentaires]);
end;


procedure TCdrEditSeriesModeFlat.FrameResize(Sender: TObject);
begin
  PairSplitterSide1.Height := Trunc(0.8 * pnlGrille.Height);
end;


procedure TCdrEditSeriesModeFlat.DispLbInfos(const Msg: string);
begin
  lbInfos.Caption := Format('L%dC%d - %s', [grdFlatSeries.Row, grdFlatSeries.Col, Msg]);
end;

procedure TCdrEditSeriesModeFlat.grdFlatSeriesClick(Sender: TObject);
var
  WU: LongInt;
  Msg: string;
begin
  FCurrentStatutRow := GetStatutRow(grdFlatSeries.Row, Msg);
  DispLbInfos(Msg);
  WU := StrToIntDef(grdFlatSeries.Cells[1, grdFlatSeries.Row], -1);
  if (WU = -1) then
  begin
    SetHeaderSerie();
  end else begin
    SetHeaderVisee();
  end;
end;

procedure TCdrEditSeriesModeFlat.grdFlatSeriesDblClick(Sender: TObject);
var
  q: LongInt;
  UnCode: TCode;
  UneExpe: TExpe;
  UnSecteur: TSecteur;
begin
  case FCurrentStatutRow of
    tsrROW_HEADER_SERIE:
      begin
        case grdFlatSeries.Col of
          COL_SERIE_NUMERO_RESEAU: ;
          COL_SERIE_ENTREE_RATTACHEMENT: ;
        end;
      end;
    tsrROW_STATION:
      begin
        case grdFlatSeries.Col of
          COL_VISEE_CODE:
          begin
            q := StrToIntDef(grdFlatSeries.Cells[COL_VISEE_CODE, grdFlatSeries.Row], 1);
            if (SelectionDansListe(FDocTopo, mslCODE, false, q)) then
            begin
              if (q > 0) then UnCode := FDocTopo.GetCodeByNumero(q)
                         else UnCode := FDocTopo.GetCodeByNumero(1);
              grdFlatSeries.Cells[COL_VISEE_CODE, grdFlatSeries.Row] := Format(FORMAT_NB_INTEGER, [UnCode.IDCode]);
            end;
          end;
          COL_VISEE_EXPE:
          begin
            q := StrToIntDef(grdFlatSeries.Cells[COL_VISEE_EXPE, grdFlatSeries.Row], 1);
            if (SelectionDansListe(FDocTopo, mslEXPE, False, q)) then
            begin
              if (q > 0) then UneExpe := FDocTopo.GetExpeByNumero(q)
                         else UneExpe := FDocTopo.GetExpeByNumero(1);
              grdFlatSeries.Cells[COL_VISEE_EXPE, grdFlatSeries.Row] := Format(FORMAT_NB_INTEGER, [UneExpe.IDExpe]);
            end;
          end;
          COL_VISEE_SECTEUR:
          begin
            q := StrToIntDef(grdFlatSeries.Cells[COL_VISEE_SECTEUR, grdFlatSeries.Row], 0);
            if (SelectionDansListe(FDocTopo, mslSECTEURS, False, q)) then
            begin
              if (q > 0) then UnSecteur := FDocTopo.GetSecteur(q)
                         else UnSecteur := FDocTopo.GetSecteur(0);
              grdFlatSeries.Cells[COL_VISEE_SECTEUR, grdFlatSeries.Row] := Format(FORMAT_NB_INTEGER, [q]);
            end;
          end;
          COL_VISEE_TYPE: // sélection d'un type de galerie
          begin
            q := StrToIntDef(grdFlatSeries.Cells[COL_VISEE_TYPE, grdFlatSeries.Row], 0);
            q := ChooseTypeVisee(q);
            grdFlatSeries.Cells[COL_VISEE_TYPE, grdFlatSeries.Row] := Format(FORMAT_NB_INTEGER, [q]);
          end;
        end;
      end
  else
    ;
  end;
end;

procedure TCdrEditSeriesModeFlat.grdFlatSeriesDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  n: LongInt;
  EWE: TColor;
  Msg: string;
  QAT: TStatutRow;
  l: double;
  procedure DessinerCell(const BG: TColor; const S: string);
  begin
    grdFlatSeries.Canvas.Brush.Color := BG;
    grdFlatSeries.Canvas.FillRect(aRect);
    grdFlatSeries.Canvas.TextOut(aRect.Left + 1, aRect.Top + 1, S);
  end;

begin
  grdFlatSeries.Canvas.Brush.Color := clCream;
  if (aRow = 0) then
  begin
    DessinerCell(clSilver, grdFlatSeries.Cells[aCol, aRow]);
    exit;
  end;

  QAT := GetStatutRow(aRow, Msg);
  case QAT of
    tsrROW_EMPTY        : EWE := clCream;
    tsrROW_SERIE_0      : EWE := clYellow;
    tsrROW_HEADER_SERIE : EWE := clAqua;
    tsrROW_STATION      : EWE := clWhite;
    tsrROW_INVALID      : EWE := clRed;
  else
    EWE := clCream;
  end;
  // contrôle des réseaux et séries
  n := StrToIntDef(grdFlatSeries.Cells[aCol, aRow], 0);
  case QAT of
    tsrROW_EMPTY: ;
    tsrROW_SERIE_0: ;
    tsrROW_HEADER_SERIE:
      begin
        case (aCol) of
          COL_SERIE_NUMERO_RESEAU       : EWE := IIF(IsInRange(n, 0, FDocTopo.GetNbReseaux() - 1), clAqua, clFuchsia);
          COL_SERIE_ENTREE_RATTACHEMENT : EWE := IIF(IsInRange(n, 0, FDocTopo.GetNbEntrees() - 1), clAqua, clFuchsia);
        end; // case (aCol) of
      end;
    tsrROW_STATION:
      begin
        case (aCol) of
          COL_VISEE_SECTEUR      : EWE := IIF(IsInRange(n, 0, FDocTopo.GetNbSecteurs() - 1), clWhite, clFuchsia);
          COL_VISEE_CODE         : EWE := IIF(FDocTopo.ExistsIdxCode(n), clWhite, clFuchsia);
          COL_VISEE_EXPE         : EWE := IIF(FDocTopo.ExistsIdxExpe(n), clWhite, clFuchsia);
          COL_VISEE_LONGUEUR     : // longueur must be strictly positive
            begin
              l := ConvertirEnNombreReel(grdFlatSeries.Cells[aCol, aRow], -1.00);
              EWE := IIF(l > 0.0005, clWhite, clFuchsia);
            end;
          COL_VISEE_AZIMUT,                   // azimuth must be positive or null
          COL_VISEE_LG .. COL_VISEE_HN:       // LRUD must be positive or null
            begin
              l := ConvertirEnNombreReel(grdFlatSeries.Cells[aCol, aRow], -1.00);
              EWE := IIF(l >= 0.00, clWhite, clFuchsia);
            end;
        end; // case (aCol) of
      end;
    tsrROW_INVALID: ;

  end; //  case QAT of
  if (gdSelected in aState) then EWE := clBlue;
  DessinerCell(EWE, grdFlatSeries.Cells[aCol, aRow]);
end;

procedure TCdrEditSeriesModeFlat.grdFlatSeriesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  S: String;
  EWE: Double;
  Q: LongInt;
begin
  //Exit;
  // /!\: Propriété AutoAdvance doit être désactivée
  //------------------------
  //EditorMode := True;
  // appui sur les touches
  case key of
    VK_RETURN:
    begin
      AfficherMessage(Format('<RETURN> pressé (Colonne: %d)', [grdFlatSeries.Col]));
      // si on se trouve en colonne Long..Pente .. LRUD,
      // on évalue l'expression si la cellule contient un signe égal en tête
      if (grdFlatSeries.Col in [COL_VISEE_CODE, COL_VISEE_EXPE,
                                COL_VISEE_LONGUEUR, COL_VISEE_AZIMUT, COL_VISEE_PENTE,
                                COL_VISEE_LG .. COL_VISEE_HN  ]) then
      begin
        S := Trim(grdFlatSeries.Cells[grdFlatSeries.Col, grdFlatSeries.Row]);
        if (S = '') then S := '0.000'; // cellule vide => valeur 0.00
        if (S[1] = '=') then  System.Delete(S, 1, 1); // pour ancienne méthode et habitudes de tableur

        EWE := EvaluationRapideExpression(S);

        if (grdFlatSeries.Col in [COL_VISEE_EXPE, COL_VISEE_CODE]) then
          grdFlatSeries.Cells[grdFlatSeries.Col, grdFlatSeries.Row] := Format(FORMAT_NB_INTEGER, [Trunc(EWE)])
        else
          grdFlatSeries.Cells[grdFlatSeries.Col, grdFlatSeries.Row] := Format(FORMAT_NB_REAL_3_DEC, [EWE]);
        // erreur = se replacer sur la case concernée
        //Q := GridCheckErrorInGrdStations(grdFlatSeries.Col, Row, EWE);
        //if (Q > 0) then
        //grdFlatSeries.Col := Q - 1; // recule d'une colonne pour contrebalancer l'incrément de colonne
      end;
      // si on arrive en fin de ligne,
      // on passe à la suivante
      // en recopiant le type de visée, l'Expé et le Code
      // et on se positionne sur la colonne longueur
      if ((grdFlatSeries.Col + 1) > COL_VISEE_OBS) then
      begin
        grdFlatSeries.LeftCol := 0;
        grdFlatSeries.Row := grdFlatSeries.Row + 1;
        grdFlatSeries.Cells[COL_VISEE_SERIE      , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_SERIE, grdFlatSeries.Row - 1];
        Q := StrToIntDef(grdFlatSeries.Cells[COL_VISEE_POINT  , grdFlatSeries.Row - 1], 0);
        grdFlatSeries.Cells[COL_VISEE_POINT      , grdFlatSeries.Row] := format(FORMAT_NB_INTEGER, [Q + 1]);
        grdFlatSeries.Cells[COL_VISEE_ID_TERRAIN , grdFlatSeries.Row] := ''; //IncrementString(grdFlatSeries.Cells[COL_VISEE_ID_TERRAIN, grdFlatSeries.Row - 1]);
        grdFlatSeries.Cells[COL_VISEE_SECTEUR    , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_SECTEUR  , grdFlatSeries.Row - 1];
        grdFlatSeries.Cells[COL_VISEE_TYPE       , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_TYPE     , grdFlatSeries.Row - 1]; // type visée
        grdFlatSeries.Cells[COL_VISEE_CODE       , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_CODE     , grdFlatSeries.Row - 1];      // code
        grdFlatSeries.Cells[COL_VISEE_EXPE       , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_EXPE     , grdFlatSeries.Row - 1];      // expé
        grdFlatSeries.Cells[COL_VISEE_ID_TERRAIN , grdFlatSeries.Row] := IncrementString(grdFlatSeries.Cells[COL_VISEE_ID_TERRAIN, grdFlatSeries.Row - 1]);
        grdFlatSeries.Cells[COL_VISEE_SECTEUR    , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_SECTEUR, grdFlatSeries.Row-1];
        grdFlatSeries.Cells[COL_VISEE_TYPE       , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_TYPE    , grdFlatSeries.Row - 1]; // type visée
        grdFlatSeries.Cells[COL_VISEE_CODE      , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_CODE, grdFlatSeries.Row - 1];      // code
        grdFlatSeries.Cells[COL_VISEE_EXPE      , grdFlatSeries.Row] := grdFlatSeries.Cells[COL_VISEE_EXPE, grdFlatSeries.Row - 1];      // expé
        grdFlatSeries.Col := COL_VISEE_EXPE;
        grdFlatSeries.LeftCol := 0;
      end;
      grdFlatSeries.Col :=  grdFlatSeries.Col + 1;
    end;
  end;
end;

procedure TCdrEditSeriesModeFlat.ReListerLeTableauDesSeries(const QIdx: integer);
var
  NbSer, CurrLineIdx, i, NbV, j: Integer;
  MySerie: TObjSerie;
  VS: TUneVisee;
begin
  AfficherMessageErreur(Format('%s.ReListerLeTableauDesSeries()', [ClassName]));
  try
    //try
      grdFlatSeries.Enabled := false;
      grdFlatSeries.Visible := false;

      lbProcessings.Caption := 'Chargement ...';
      grdFlatSeries.Clear;
      // on met un nombre réduit de lignes: la fonction StrToIntDef() est assez lente si son paramètre n'est pas un nombre
      grdFlatSeries.RowCount := 50;
      grdFlatSeries.ColCount := 25;
      grdFlatSeries.ColWidths[COL_COMMENTAIRES] := 400;
      NbSer := FDocTopo.GetNbSeries();
      AfficherMessageErreur(Format('ReListerLeTableauDesSeries(%d)', [NbSer]));
      CurrLineIdx := 1;
      for i := 0 to NbSer - 1 do
      begin
        MySerie := FDocTopo.GetSerie(i);
        AfficherMessage(Format('--> %d: %s', [i, MySerie.GetNomSerie()]));
        DispEnTeteSerie(MySerie, CurrLineIdx);
        CurrLineIdx += 1;
        if (grdFlatSeries.RowCount - CurrLineIdx < 10) then grdFlatSeries.RowCount := grdFlatSeries.RowCount + 50;
        NbV := MySerie.GetNbVisees();
        for j := 0 to NbV - 1 do
        begin
          VS := MySerie.GetVisee(j);
          DispVisee(MySerie, VS, j, CurrLineIdx);
          CurrLineIdx += 1;
          // si il reste moins de 10 lignes, on en rajoute 50 d'un coup
          if (grdFlatSeries.RowCount - CurrLineIdx < 10) then grdFlatSeries.RowCount := grdFlatSeries.RowCount + 50;
        end;
      end;
      // On rajoute une ligne de garde en fin de tableau
      grdFlatSeries.RowCount := grdFlatSeries.RowCount + 1;
      AfficherMessage(Format('ReListerLeTableauDesSeries(%d) OK', [NbSer]));
      lbProcessings.Caption := '';

    //Except
    //  on E: Exception do AfficherMessageErreur(E.Message);
    //end;
  finally
    grdFlatSeries.Enabled := true;
    grdFlatSeries.Visible := true;
  end;
end;

// En fonction des paramètres de la ligne, vérifie les données
function TCdrEditSeriesModeFlat.GridCheckErrorInGrdStations(const CurrColonne, CurrLigne: integer;
                                                            const QValeur: double): boolean;
var
  E: TExpe;
  C: TCode;
  L: double;
  MsgErr: string;
begin
  Result := false;
  MsgErr := 'OK';
  with grdFlatSeries do
  begin
    case CurrColonne of
      COL_VISEE_CODE:
      begin
        if (not FDocTopo.ExistsIdxCode(Trunc(QValeur))) then
        begin
          MsgErr := rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND;
          Result := false;
        end;
        Result := True;
      end;
      COL_VISEE_EXPE:
      begin
        if (not FDocTopo.ExistsIdxExpe(Trunc(QValeur))) then
        begin
          MsgErr := rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND;
          Result := false;
        end;
        Result := True;
      end;
      COL_VISEE_LONGUEUR:
      begin
        if (not IsInRange(QValeur, 0.001, MAX_LENGTH_VISEE_4_TOPOROBOT)) then
        begin
          MsgErr := Format(rsCDR_SERIE_MSG_ERR_LONG, [MAX_LENGTH_VISEE_4_TOPOROBOT]);
          Result := false;
        end;
        Result := True;
      end;
      COL_VISEE_AZIMUT: Result := True;
      COL_VISEE_PENTE : Result := True;
      COL_VISEE_LG .. COL_VISEE_HN:
      begin
        if (QValeur < 0.00) then
        begin
          MsgErr := rsCDR_SERIE_MSG_ERR_LRUD;     Result := false;
        end;
        Result := True;
      end
      else
        ;
    end;
    if (not Result) then
    begin
      lbInfos.Caption := GetResourceString(MsgErr);
      AfficherMessageErreur(Format('%s.ErrorInStation: L%dC%d: %.2f - %s', [ClassName, CurrColonne, CurrLigne, QValeur,  MsgErr]));
    end;
  end;
end;


end.

