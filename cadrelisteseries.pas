// liste des séries admettant les filtres

unit CadreListeSeries;
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes
  , SysUtils
  , FileUtil, Forms, Controls, StdCtrls, ComCtrls, Dialogs, Graphics, LCLType, ActnList, Buttons, CheckLst
  , StructuresDonnees
  , ToporobotClasses2012
  , UnitObjetSerie
  , unitUtilsComposants
  , UnitListesSimplesWithGeneriques
  , Types
  ;
type TModeFiltrage = (mfNONE, mfENTRANCES, mfRESEAUX);

type TButtonsShown = set of (tbsAPPLY, tbsADD, tbsREMOVE, tbsSORT, tbsCSV, tbsHELP);

type

  { TCdrListeSeries }

  TCdrListeSeries = class(TFrame)
    acCdrSerAddItem: TAction;
    acCdrSerApplyModifs: TAction;
    acCdrSerDeleteItem: TAction;
    acCdrSerExportListesCSV: TAction;
    acCdrSerFind: TAction;
    acnlstCadreSerie: TActionList;
    acCdrSerSort: TAction;
    acCdrSerHelp: TAction;
    acCocherSeriesFromText: TAction;
    acChkLstSelectAll: TAction;
    acChkLstDeSelectAll: TAction;
    acChkLstReverseSelection: TAction;
    lsbListe: TCheckListBox;
    cmbEntreesOuReseaux: TComboBox;
    cmbTypeFiltrage: TComboBox;
    editSelectionMultiple: TEdit;
    editRechercheRapide: TEdit;
    hcColsTitres: THeaderControl;
    imglstCadreSerie: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lbNbreElements: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    procedure acCdrSerAddItemExecute(Sender: TObject);
    procedure acCdrSerApplyModifsExecute(Sender: TObject);
    procedure acCdrSerExportListesCSVExecute(Sender: TObject);
    procedure acCdrSerFindExecute(Sender: TObject);
    procedure acCdrSerSortExecute(Sender: TObject);
    procedure acCdrSerHelpExecute(Sender: TObject);
    procedure acChkLstDeSelectAllExecute(Sender: TObject);
    procedure acChkLstReverseSelectionExecute(Sender: TObject);
    procedure acChkLstSelectAllExecute(Sender: TObject);
    procedure acCocherSeriesFromTextExecute(Sender: TObject);
    procedure cmbEntreesOuReseauxChange(Sender: TObject);
    procedure cmbTypeFiltrageChange(Sender: TObject);
    procedure editRechercheRapideKeyPress(Sender: TObject; var Key: char);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbListeClick(Sender: TObject);
    procedure lsbListeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    { private declarations }
    FBusy: boolean;
    FZoneDeSelection: TRect;
    FCurrentItemAtPos: integer;
    FModeFiltrage: TModeFiltrage;
    FSelectedIndex: integer;
    FAllowMultiSelect: boolean;
    FListeSelectedSeries: TListeSelectedSeries;
    FDocuTopo           : TToporobotStructure2012;
    FProcGetNumeroSerie : TProcOfObjectWithTNumeroSerie;
    FProcApplyModifs    : TProcedureOfObject;
    procedure CocherSeriesDepuisLigneSaisie(const S: string);
    procedure InitCmbFiltre(const F: TModeFiltrage);
    procedure InitHeaderListe();
    procedure ListerSeries(const FNumeroEntreeOuReseau: Integer; const QIdx: integer);
    procedure ListerEntrees();
    procedure ListerReseaux();
  public
    { public declarations }
    function Initialiser(const FD: TToporobotStructure2012;
                        const QProcGetSerie: TProcOfObjectWithTNumeroSerie;
                        const QProcApplyModifs: TProcedureOfObject;
                        const QButtons: TButtonsShown;
                        const AllowMultiSelectSeries: boolean): boolean;
    procedure Relister(const QIdx: integer);
    function  GetSelectedIndex(): integer;
    function  GetSelectedNumSerie(): TNumeroSerie;
    function  IsItemListeSelected(const Idx: integer): boolean;
    procedure Finaliser();
  end;

implementation
uses
  Common,
  CallDialogsStdVersion;


const
  HEADER_LST_NUM_COL_COCHE          =  0;
  HEADER_LST_NUM_COL_NUM_SERIE      =  1;
  HEADER_LST_NUM_COL_ST_DEPART      =  2;
  HEADER_LST_NUM_COL_ST_ARRIVEE     =  3;
  HEADER_LST_NUM_COL_NOM_SERIE      =  4;
  HEADER_LST_NUM_COL_ENTREE         =  5;


  {$IFDEF GROS_MINET}
  HEADER_LST_NUM_COL_RESEAU         = -1;
  HEADER_LST_NUM_COL_NB_VISEES      =  6;
  HEADER_LST_NUM_COL_SERIES_LIBRES  =  7;  jkhsq A vérifier
  {$ELSE}
  HEADER_LST_NUM_COL_RESEAU         =  6;
  HEADER_LST_NUM_COL_NB_VISEES      =  7;
  HEADER_LST_NUM_COL_SERIES_LIBRES  =  8;
  {$ENDIF}

{$R *.lfm}

{ TCdrListeSeries }


// NOTA: TCdrListeSeries possède un pointeur sur l'objet TToporobotStructure2012
// Par conséquent, on peut faire toutes les opérations sur les séries

procedure TCdrListeSeries.CocherSeriesDepuisLigneSaisie(const S: string);
var
  LSR   : TStringList;
  i , k: Integer;
  EWE, S1, S2, ser: TNumeroSerie;
  MySTR: String;
  WU: TGHStringArray;
  procedure CocherSerie(const Idx: TNumeroSerie);
  var
    QQ: Integer;
  begin
    QQ := FDocuTopo.GetIdxSerieByNumero(Idx);
    if (QQ > 0) then lsbListe.Checked[QQ] := true;
  end;
begin
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
            //AfficherMessageErreur(Format('Intervalle trouvé: %d - %d', [S1, S2]));
            if (S1 > S2) then Swap(S1, S2); // inversion éventuelle des bornes
            if (Abs(S2 - S1) > 0) then
              for ser := S1 to S2 do CocherSerie(ser)
            else // Bornes identiques => on coche l'élément S1
              CocherSerie(S1);
          end;
        end
        else
        begin
          AfficherMessageErreur(Format('-- Recherche sur Valeur: %d', [EWE]));
          if (EWE > 0) then CocherSerie(EWE);
        end;
      end;
    end;
  finally
    LSR.Clear;
    FreeAndNil(LSR);//LSR.Free;
  end;
end;

procedure TCdrListeSeries.acCdrSerApplyModifsExecute(Sender: TObject);
begin
  if (lsbListe.ItemIndex < 0) then Exit;
  if (assigned(FProcApplyModifs)) then FProcApplyModifs;
end;

procedure TCdrListeSeries.acCdrSerExportListesCSVExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QIdxFilter: integer;
begin
  if (DoDialogSaveFile('Fichier CSV|*.csv', '.csv', QFileName, QIdxFilter)) then
  begin
    FDocuTopo.ExportListeEntetesSeries(QFileName);
  end;
end;

procedure TCdrListeSeries.acCdrSerFindExecute(Sender: TObject);
var
  QIdx: integer;
begin
  if (FindIdxOccurrenceInCheckListBox(lsbListe, trim(editRechercheRapide.Text), QIdx)) then
    lsbListe.ItemIndex := QIdx
  else
    ShowMessage(GetResourceString(rsMATCHNOTFOUND));
end;

procedure TCdrListeSeries.acCdrSerSortExecute(Sender: TObject);
begin
  FDocuTopo.SortSeries();
  Relister(0);
end;

procedure TCdrListeSeries.acCdrSerHelpExecute(Sender: TObject);
begin
  DisplayHelpSystem('LISTE_SERIES');
end;

procedure TCdrListeSeries.acChkLstDeSelectAllExecute(Sender: TObject);
var
  Nb, i: Integer;
begin
  Nb := lsbListe.Count;
  if (nb = 0) then exit;
  for i := 0 to Nb -1 do lsbListe.Checked[i] := false;
end;

procedure TCdrListeSeries.acChkLstReverseSelectionExecute(Sender: TObject);
var
  Nb, i: Integer;
begin
  Nb := lsbListe.Count;
  if (nb = 0) then exit;
  for i := 0 to Nb -1 do lsbListe.Checked[i] := not lsbListe.Checked[i];
end;

procedure TCdrListeSeries.acChkLstSelectAllExecute(Sender: TObject);
var
  Nb, i: Integer;
begin
  Nb := lsbListe.Count;
  if (nb = 0) then exit;
  for i := 0 to Nb -1 do lsbListe.Checked[i] := True;
end;

procedure TCdrListeSeries.acCocherSeriesFromTextExecute(Sender: TObject);
begin
  CocherSeriesDepuisLigneSaisie(editSelectionMultiple.Text);
end;

procedure TCdrListeSeries.acCdrSerAddItemExecute(Sender: TObject);
begin
  if (not GHTopoQuestionOuiNon(rsDLG_BDD_ADD_SERIE)) then exit;
  if (FDocuTopo.CreateNewSerie(1,0, -1, rs2NDMEMBER)) then
  begin
    Relister(FDocuTopo.GetNbSeries() - 1);
  end
  else
  begin
    ShowMessage('Echec creation série');
  end;
end;

procedure TCdrListeSeries.cmbEntreesOuReseauxChange(Sender: TObject);
begin
  FSelectedIndex := 0;
  ListerSeries(cmbEntreesOuReseaux.ItemIndex, 0);
end;

procedure TCdrListeSeries.cmbTypeFiltrageChange(Sender: TObject);
begin
  FModeFiltrage := TModeFiltrage(cmbTypeFiltrage.ItemIndex);
  case FModeFiltrage of
    mfNONE      : ;
    mfENTRANCES : ListerEntrees();
    mfRESEAUX   : ListerReseaux();
  end;
  ListerSeries(0, 0);
  cmbEntreesOuReseaux.Enabled := cmbTypeFiltrage.ItemIndex <> ord(mfNONE);
end;


procedure TCdrListeSeries.editRechercheRapideKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) then acCdrSerFindExecute(self);
end;

procedure TCdrListeSeries.Finaliser();
begin
  try
    FListeSelectedSeries.ClearListe();
  finally
    FreeAndNil(FListeSelectedSeries);//FListeSelectedSeries.Free;
  end;
end;

function TCdrListeSeries.GetSelectedIndex(): integer;
var
  n: Integer;
begin
  n := lsbListe.Count;
  Result := IIF((n > 0), FSelectedIndex, -1);
  AfficherMessage(Format('%s.GetSelectedIndex: %d / %d', [ClassName, Result, n]));
end;

function TCdrListeSeries.GetSelectedNumSerie(): TNumeroSerie;
begin
  Result := FListeSelectedSeries.GetElement(lsbListe.ItemIndex);
end;

function TCdrListeSeries.IsItemListeSelected(const Idx: integer): boolean;
begin
  Result := lsbListe.Checked[Idx];
end;



procedure TCdrListeSeries.hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbListe.Invalidate;
end;

procedure TCdrListeSeries.lsbListeClick(Sender: TObject);
var
  EWE: TNumeroSerie;
begin
  if (assigned(FProcGetNumeroSerie)) then
  begin
    EWE := FListeSelectedSeries.GetElement(lsbListe.ItemIndex);
    FProcGetNumeroSerie(EWE);
  end;
end;


procedure TCdrListeSeries.InitCmbFiltre(const F: TModeFiltrage);
begin
  cmbEntreesOuReseaux.Enabled := false;
  FModeFiltrage := F;
  cmbTypeFiltrage.Clear;
  cmbTypeFiltrage.Items.Add(GetResourceString(rsTBS_TOUT));
  cmbTypeFiltrage.Items.Add(GetResourceString(rsTBS_ENTRANCE));
  {$IFNDEF GROS_MINET}  // Sylvestre CLEMENT n'utilise pas les réseaux
    cmbTypeFiltrage.Items.Add(GetResourceString(rsTBS_RESEAUX));
  {$ENDIF}
  cmbTypeFiltrage.ItemIndex := Ord(FModeFiltrage);
  cmbEntreesOuReseaux.Clear;
end;

procedure TCdrListeSeries.InitHeaderListe();
const
  LC_COCHES           = 30;
  LC_ID_NUMERIQUES    = 60;
  LC_ID_SERIE_STATION = 70;
  LC_NOM_SERIE        = 300;
  LC_NOM_ENTREE       = 250;
  LC_NOM_RESEAU       = 250;
  LC_NB_POINTS        = 80;
  LC_SERIES_LIBRES    = 100;
var
 ht: THeaderSection;
 FNbItems: Integer;
 miou: String;
 procedure AjouterTitreColonne(const Titre: string; const LGMin, LG: integer);
 begin
   ht := hcColsTitres.Sections.Add;
   ht.Text := Titre;
   ht.MinWidth := LGMin;
   ht.Width    := LG;
 end;
 procedure AjouterTitreColonneRes(const RS: string; const LGMin, LG: integer);
 begin
   AjouterTitreColonne(GetResourceString(RS), LGMin, LG);
 end;
begin
  // purge des titres des headers
  hcColsTitres.Sections.Clear;
  miou := GetResourceString(rsTBS_SERIES);
  FNbItems := FDocuTopo.GetNbSeries;
  //pnlListes.Caption := Format('%s (%d items)', [miou, FNbItems]);
  with hcColsTitres do
  begin
    AjouterTitreColonneRes('X', LC_COCHES, LC_COCHES);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_ID, LC_ID_NUMERIQUES, LC_ID_NUMERIQUES);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_DEPART, LC_ID_SERIE_STATION, LC_ID_SERIE_STATION);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_ARRIVEE, LC_ID_SERIE_STATION, LC_ID_SERIE_STATION);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_NOM, 100, LC_NOM_SERIE);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_ENTREE, 100, LC_NOM_ENTREE);
    {$IFDEF GROS_MINET}
    pass;
    {$ELSE}
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_RESEAU, 100, LC_NOM_RESEAU);
    {$ENDIF}
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_NBPOINTS, LC_NB_POINTS, LC_NB_POINTS);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_INTERVALLE, LC_SERIES_LIBRES, LC_SERIES_LIBRES);
  end;
end;


function TCdrListeSeries.Initialiser(const FD: TToporobotStructure2012;
                                    const QProcGetSerie: TProcOfObjectWithTNumeroSerie;
                                    const QProcApplyModifs: TProcedureOfObject;
                                    const QButtons: TButtonsShown;
                                    const AllowMultiSelectSeries: boolean): boolean;
  procedure S777(const ACDC: Taction; const S: string; const QVisible: boolean);
  begin
    ACDC.Caption := GetResourceString(S);
    ACDC.Hint    := GetResourceString(S);
    ACDC.Visible := QVisible;
  end;

begin
  result := false;
  FBusy := false;
  FSelectedIndex := 0;
  FZoneDeSelection := Rect(0,0,0,0);
  FCurrentItemAtPos:= -1;
  try
    //FNumeroEntree := 0;
    FDocuTopo := FD;
    FProcGetNumeroSerie  := QProcGetSerie;
    FProcApplyModifs     := QProcApplyModifs;
    FListeSelectedSeries := TListeSelectedSeries.Create;
    InitCmbFiltre(mfNONE);
    InitHeaderListe();
    lsbListe.ItemHeight  := 20;
    lsbListe.MultiSelect := false; //
    FAllowMultiSelect := AllowMultiSelectSeries;
    ListerSeries(-1, 0);
    S777(acCdrSerAddItem        , rsCDR_SERIES_ADD_SERIE   , tbsADD    in QButtons);
    S777(acCdrSerDeleteItem     , rsCDR_SERIES_DELETE_SERIE, tbsREMOVE in QButtons);
    S777(acCdrSerApplyModifs    , rsCDR_SERIES_VALID_SERIE , tbsAPPLY  in QButtons);
    S777(acCdrSerSort           , rsCDR_SERIES_SORT_SERIES , tbsSORT   in QButtons);
    S777(acCdrSerExportListesCSV, rsCDR_SERIES_EXPORT_CSV  , tbsCSV    in QButtons);
    S777(acCdrSerHelp           , rsCDR_SERIES_HELP        , tbsHELP   in QButtons);
    Result := True;
  except
    pass;
  end;
end;


procedure TCdrListeSeries.ListerEntrees();
var
  i, Nb: Integer;
  EWE: TEntrance;
  WU: String;
begin
  if (FBusy) then exit;
  FBusy := True;
  try
    cmbEntreesOuReseaux.Clear;
    Nb := FDocuTopo.GetNbEntrances();
    for i := 0 to Nb - 1 do
    begin
      EWE := FDocuTopo.GetEntrance(i);
      WU  := Format('%d: %s (%d.%d [%s])', [i, EWE.eNomEntree, EWE.eRefSer, EWE.eRefSt, EWE.eIDTerrain]);
      cmbEntreesOuReseaux.Items.Add(WU);
    end;
    cmbEntreesOuReseaux.DropDownCount := IIF(Nb > 30, 30, Nb);
    cmbEntreesOuReseaux.ItemIndex := 0;
    cmbEntreesOuReseaux.Enabled   := True;

  finally
    FBusy := false;
  end;

end;

procedure TCdrListeSeries.ListerReseaux();
var
  i, Nb: Integer;
  EWE  : TReseau;
  WU   : String;
begin
  if (FBusy) then exit;
  FBusy := True;
  try
    cmbEntreesOuReseaux.Clear;
    Nb := FDocuTopo.GetNbReseaux();
    for i := 0 to Nb - 1 do
    begin
      EWE := FDocuTopo.GetReseau(i);
      WU  := Format('%d: %s', [i, EWE.NomReseau]);
      cmbEntreesOuReseaux.Items.Add(WU);
    end;
    cmbEntreesOuReseaux.DropDownCount := IIF(Nb > 30, 30, Nb);
    cmbEntreesOuReseaux.ItemIndex := 0;
    cmbEntreesOuReseaux.Enabled := True;
  finally
    FBusy := false;
  end;
end;

procedure TCdrListeSeries.ListerSeries(const FNumeroEntreeOuReseau: Integer; const QIdx: integer);
var
  i, Nb: Integer;
  SR: TObjSerie;
  procedure AddSerieAtListe(const S: TObjSerie; const QIdx: integer);
  var
    WU: String;
  begin
    FListeSelectedSeries.AddElement(QIdx);
    WU := format('%d: (%d) - %s', [S.GetNumeroDeSerie(), S.GetNumeroEntrance(), S.GetNomSerie()]);
    lsbListe.Items.add(WU);
  end;
  procedure LOL(const B: boolean);
  begin
    acCdrSerApplyModifs.Enabled := B;
    acCdrSerDeleteItem.Enabled  := B;
    acCdrSerFind.Enabled        := B;
    acCdrSerSort.Enabled        := B;
  end;
begin
  FSelectedIndex     := 0;
  if (FBusy) then exit;
  FBusy := True;
  try
    Nb := FDocuTopo.GetNbSeries();
    FListeSelectedSeries.ClearListe();
    lsbListe.Enabled := false;
    lsbListe.Clear;
    // -1 -> on liste tout
    if (FModeFiltrage = mfNONE) then
    begin
      for i := 0 to Nb - 1 do AddSerieAtListe(FDocuTopo.GetSerie(i), i);
    end
    else
    begin
      for i := 0 to Nb - 1 do
      begin
        SR := FDocuTopo.GetSerie(i);
        case FModeFiltrage of
          mfNONE      : AddSerieAtListe(SR, i);
          mfENTRANCES : if (FNumeroEntreeOuReseau = SR.GetNumeroEntrance()) then AddSerieAtListe(SR, i);
          mfRESEAUX   : if (FNumeroEntreeOuReseau = SR.GetNumeroReseau())   then AddSerieAtListe(SR, i);
        end;
      end;
    end;
    if (lsbListe.Items.Count = 0) then
    begin
      LOL(false);
      Exit;
    end;
    LOL(True);

    lsbListe.ItemIndex := 0;
    // et on se positionne sur l'item demandé
    if (IsInRange(QIdx, 0, lsbListe.Count - 1)) then
    begin
      FSelectedIndex     := QIdx;
      lsbListe.ItemIndex := QIdx;
    end;
    lbNbreElements.Caption := Format(rsCDR_SERIE_LB_NB_SERIES, [lsbListe.Count]);
  finally
    FBusy := false;
    lsbListe.Enabled := true;
  end;
end;

procedure TCdrListeSeries.Relister(const QIdx: integer);
begin
  FBusy := false;
  ListerSeries(0, QIdx);
end;

procedure TCdrListeSeries.lsbListeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  sr: TObjSerie;
  // bg = couleur de fond; tc= couleur du texte
  procedure DessineItem(const bg,tc: TColor);
  VAR
    rs: TReseau;
    en: TEntrance;
    ColorItemSelected: TColor;
    sr0, sr1: TObjSerie;
    NbIndexSeriesLibres: TNumeroSerie;
    WU: String;
    // /!\ Ne pas utiliser les fonctions homonymes dédiées aux ListBox
    procedure QDessineFiletColonne(const TB: integer); inline;
    begin
      lsbListe.Canvas.Line(TB, ARect.Top, TB, ARect.Bottom);
    end;
    procedure QDessineRectangleColore(const QBG, PC, BC: TColor; const L, R, T, B: integer);
    begin
      lsbListe.canvas.Pen.Color   := PC;
      lsbListe.canvas.Brush.Color := BC;
      lsbListe.canvas.Rectangle(Rect(L, T, R, B));
      // et on restaure l'ancienne couleur
      lsbListe.Canvas.Brush.Color := QBG;
      lsbListe.canvas.Pen.Color   := clSilver;
    end;
    procedure QDrawColTexte(const QHS: THeaderSection; const QText: string);
    var
      QR: TRect;
    begin
      QR := Rect(QHS.Left, ARect.Top + mg, QHS.Right - 8, ARect.Bottom - mg);
      QDessineFiletColonne(QHS.Left - Q4);
      lsbListe.canvas.TextRect(QR, QHS.Left + 4, ARect.Top+1, _AnsiToLCLStr(QText));
    end;
    procedure QDrawColRectColoreWithTexte(const QHS: THeaderSection; const QDoDrawFilet: boolean; const QRectColor: TColor; const QText: string);
    var
     QOlgBG: TColor;
    begin
     QOlgBG := lsbListe.Canvas.Brush.Color;
     if (QDoDrawFilet) then QDessineFiletColonne(QHS.Left - Q4);
     if (Trim(QText) <> '') then
     begin
       QDessineRectangleColore(bg, clBlack, QRectColor, QHS.Left, QHS.Left + 32, ARect.Top + mg, ARect.Bottom - mg);
       lsbListe.Canvas.TextOut(QHS.Left + 32 + 4, ARect.Top+1, _AnsiToLCLStr(QText));
     end else
     qDessineRectangleColore(bg, clBlack, QRectColor, QHS.Left, QHS.Right - 4, ARect.Top + mg, ARect.Bottom - mg);
     lsbListe.Canvas.Brush.Color := QOlgBG;
    end;
  begin
    lsbListe.canvas.Pen.Color   := clSilver;
    lsbListe.Canvas.Brush.Color := bg;
    lsbListe.Canvas.Font.Color  := tc;
    lsbListe.Canvas.FillRect(ARect);

    ColorItemSelected := IIF(lsbListe.Checked[Index], clRed, clWhite);
    QDrawColRectColoreWithTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_COCHE], false, ColorItemSelected, '');
    QDrawColTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_NUM_SERIE]       , Format(FORMAT_NB_INTEGER,[sr.GetNumeroDeSerie()]));
    QDrawColTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_ST_DEPART]       , Format(FMTSERST,[sr.GetNoSerieDep, sr.GetNoPointDep]));
    QDrawColTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_ST_ARRIVEE]      , Format(FMTSERST,[sr.GetNoSerieArr, sr.GetNoPointArr]));
    QDrawColTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_NOM_SERIE]       , _AnsiToLCLStr(sr.GetNomSerie));
    en := FDocuTopo.GetEntrance(sr.GetNumeroEntrance());
    QDrawColTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_ENTREE]          , _AnsiToLCLStr(en.eNomEntree));
    // réseau
    rs := FDocuTopo.GetReseau(sr.GetNumeroReseau());
    QDrawColRectColoreWithTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_RESEAU], True, rs.ColorReseau, _AnsiToLCLStr(rs.NomReseau));
    qDrawColTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_NB_VISEES]       , Format(FORMAT_NB_INTEGER,[sr.GetNbVisees]));
    // nombre d'éléments libres entre deux numéros de série
    WU := '';
    if (Index > 0) then
    begin
      sr0 := FDocuTopo.GetSerie(FListeSelectedSeries.GetElement(Index - 1));
      sr1 := FDocuTopo.GetSerie(FListeSelectedSeries.GetElement(Index));
      NbIndexSeriesLibres := (sr1.GetNumeroDeSerie() - sr0.GetNumeroDeSerie()) - 1;
      WU := IIF (0 = NbIndexSeriesLibres, '', Format(FORMAT_NB_INTEGER,[NbIndexSeriesLibres]));
    end;
    qDrawColTexte(hcColsTitres.Sections.Items[HEADER_LST_NUM_COL_SERIES_LIBRES], WU);
  end;
begin
  lsbListe.Canvas.Pen.Mode := pmCopy;
  //try
    sr := FDocuTopo.GetSerie(FListeSelectedSeries.GetElement(Index));   //FDocumentToporobot.GetSerie(Index);
    if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clWhite, clBlack);
  //except
  //end;
end;



end.

