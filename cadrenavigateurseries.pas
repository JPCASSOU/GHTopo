unit cadreNavigateurSeries;
// 23/11/2018: Barre de recherche
// 13/06/2019: Point de contrôle temporel (contrôle de version)

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils,
  StructuresDonnees, Common,
  ToporobotClasses2012,
  UnitObjetSerie,
  CallDialogsStdVersion,
  FileUtil,  Forms, Controls, ActnList, Buttons, StdCtrls, Dialogs;

type

  { TCdrNavigateurSeries }

  TCdrNavigateurSeries = class(TFrame)
    acGotoFirst: TAction;
    acGotoPrev: TAction;
    acGoto10Prev: TAction;
    acGoto10Next: TAction;
    acGotoNext: TAction;
    acGotoLast: TAction;
    acFindByText: TAction;
    acNewSerie: TAction;
    acSortSeries: TAction;
    acSelectSerieInListe: TAction;
    acRemoveSerie: TAction;
    acCheckerLesSeries: TAction;
    acValidateModifs: TAction;
    ActionList1: TActionList;
    editNomSerie: TEdit;
    ImageList1: TImageList;
    lbSearch: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    procedure acCheckerLesSeriesExecute(Sender: TObject);
    procedure acFindByTextExecute(Sender: TObject);
    procedure acGoto10NextExecute(Sender: TObject);
    procedure acGoto10PrevExecute(Sender: TObject);
    procedure acGotoFirstExecute(Sender: TObject);
    procedure acGotoLastExecute(Sender: TObject);
    procedure acGotoNextExecute(Sender: TObject);
    procedure acGotoPrevExecute(Sender: TObject);
    procedure acNewSerieExecute(Sender: TObject);
    procedure acRemoveSerieExecute(Sender: TObject);
    procedure acSelectSerieInListeExecute(Sender: TObject);
    procedure acSortSeriesExecute(Sender: TObject);
    procedure acValidateModifsExecute(Sender: TObject);
    procedure editNomSerieKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure editNomSerieKeyPress(Sender: TObject; var Key: char);
    procedure FrameClick(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
  private
    { private declarations }
    FDocTopo: TToporobotStructure2012;
    FDejaChangeDeSerie: boolean;
    FProcDisAllowedChangeSerie: TProcOfObjectReturnsAsBoolean;
    FProcGotoSerie  : TProcedureOfObject;
    FProcNewSerie   : TProcedureOfObject;
    FProcApplyModifs: TProcedureOfObject;
    FIdxSerie       : integer;
    procedure LoadSerie(const QIdx: integer);
    procedure Skip(const Offset: integer);
    procedure SetSecureIdxSerie(const QIdx: integer);
    procedure InitCaptions();
  public
    { public declarations }
    function Initialiser(const B: TToporobotStructure2012;
                         const QProcChangeSerieAllowed: TProcOfObjectReturnsAsBoolean;
                         const QProcGotoSerie: TProcedureOfObject;
                         const QProcNewSerie : TProcedureOfObject;
                         const QProcApply    : TProcedureOfObject): boolean;
    function GetIdxSerie(): integer;
  end;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TCdrNavigateurSeries }
const
  MIN_SERIE_IDX = 1;
  TO_REFRESH = -666;
  TO_FIRST   = 0;
  TO_LAST    = 666666;

procedure TCdrNavigateurSeries.acCheckerLesSeriesExecute(Sender: TObject);
begin
  FDocTopo.CheckerLesDonneesTopo();
end;

procedure TCdrNavigateurSeries.acFindByTextExecute(Sender: TObject);
var
  n: Integer;
  SearchResults: TArrayOfTToporobotIDStation;
begin
  // recherche plein texte dans tous les cas
  FDocTopo.SearchTextInTableSeriesStations(editNomSerie.Text, SearchResults);
  n := FDocTopo.FindIdxSerieByText(editNomSerie.Text);
  if (n > 0) then
  begin
    SetSecureIdxSerie(n);
    if (Assigned(FProcGotoSerie))then FProcGotoSerie();
  end
  else
  begin
    ShowMessage(GetResourceString(rsMATCHNOTFOUND));
  end;
end;


procedure TCdrNavigateurSeries.acGoto10NextExecute(Sender: TObject);
begin
  Skip(10);
end;

procedure TCdrNavigateurSeries.acGoto10PrevExecute(Sender: TObject);
begin
  Skip(-10);
end;

procedure TCdrNavigateurSeries.acGotoFirstExecute(Sender: TObject);
begin
  Skip(TO_FIRST);
end;

procedure TCdrNavigateurSeries.acGotoLastExecute(Sender: TObject);
begin
  Skip(TO_LAST);
end;

procedure TCdrNavigateurSeries.acGotoNextExecute(Sender: TObject);
begin
  Skip(1);
end;

procedure TCdrNavigateurSeries.acGotoPrevExecute(Sender: TObject);
begin
  Skip(-1);
end;

procedure TCdrNavigateurSeries.acNewSerieExecute(Sender: TObject);
begin
  if (not GHTopoQuestionOuiNon(rsCOUPE_DEVEL_AC_ADD_SERIE)) then Exit;
  if (assigned(FProcNewSerie)) then
  begin
    FProcNewSerie();
    Skip(TO_LAST);
  end;
end;

procedure TCdrNavigateurSeries.acRemoveSerieExecute(Sender: TObject);
begin
  if (ListerDependancesDeUneSerie(FDocTopo, FIdxSerie)) then
  begin
    //exit ; // A supprimer après mise au point
    FDocTopo.RemoveSerie(FIdxSerie);
    Skip(TO_LAST);
  end;
end;

procedure TCdrNavigateurSeries.acSelectSerieInListeExecute(Sender: TObject);
var
  n: Integer;
begin
  n := FIdxSerie;
  if (SelectionDansListe(FDocTopo, mslSERIE, True, n)) then
  begin
    LoadSerie(n);
  end;
end;

procedure TCdrNavigateurSeries.acSortSeriesExecute(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon(rsCDR_NAVIG_SERIES_TRIER)) then
  begin
    FDocTopo.SortSeries();
    LoadSerie(1);
  end;
end;

procedure TCdrNavigateurSeries.acValidateModifsExecute(Sender: TObject);
begin
  if (Assigned(FProcApplyModifs)) then  FProcApplyModifs();
end;

procedure TCdrNavigateurSeries.editNomSerieKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //if (Key = vkRETURN) then acFindByTextExecute(self);
  //AfficherMessage(format('Key: %d', [key]));
end;

procedure TCdrNavigateurSeries.editNomSerieKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then acFindByTextExecute(self);
end;

procedure TCdrNavigateurSeries.FrameClick(Sender: TObject);
begin

end;

function TCdrNavigateurSeries.GetIdxSerie: integer;
begin
  Result := FIdxSerie;
end;

procedure TCdrNavigateurSeries.InitCaptions;
  procedure SetAcHint(const ACDC: TAction; const QCaption: string);
  begin
    ACDC.Caption := GetResourceString(QCaption);
    ACDC.Hint    := GetResourceString(QCaption);
  end;
begin
  SetAcHint(acNewSerie      , rsNOUVELLE_SERIE);
  SetAcHint(acGotoFirst     , rsCDR_NAVIG_SERIES_TO_FIRST);
  SetAcHint(acGotoPrev      , rsCDR_NAVIG_SERIES_TO_PREV);
  SetAcHint(acGoto10Prev    , '');
  SetAcHint(acGoto10Next    , '');
  SetAcHint(acGotoNext      , rsCDR_NAVIG_SERIES_TO_NEXT);
  SetAcHint(acGotoLast      , rsCDR_NAVIG_SERIES_TO_LAST);
  SetAcHint(acValidateModifs, rsCDR_NAVIG_SERIES_IMPLEMENT_MODIFS);
  SetAcHint(acSortSeries    , rsCDR_NAVIG_SERIES_TRIER);
  SetAcHint(acFindByText    , rsCDR_NAVIG_SERIES_FIND_SERIE_BY_TEXT);
  SetAcHint(acCheckerLesSeries, rsCHECKBASE);
  lbSearch.Caption := GetResourceString(rsCDR_NAVIG_SERIES_FIND_WHAT);
end;

function TCdrNavigateurSeries.Initialiser(const B: TToporobotStructure2012;
                                          const QProcChangeSerieAllowed: TProcOfObjectReturnsAsBoolean;
                                          const QProcGotoSerie: TProcedureOfObject;
                                          const QProcNewSerie : TProcedureOfObject;
                                          const QProcApply    : TProcedureOfObject): boolean;
begin
  Result         := False;
  FDocTopo       := B;
  FDejaChangeDeSerie := false;    //
  FProcDisAllowedChangeSerie := QProcChangeSerieAllowed;
  FProcGotoSerie   := QProcGotoSerie;
  FProcApplyModifs := QProcApply;
  FProcNewSerie    := QProcNewSerie;
  InitCaptions();
  Skip(0);
  result   := True;
  // bouton de suppression
  acRemoveSerie.Visible := true;
  editNomSerie.Text := '';
end;


procedure TCdrNavigateurSeries.SetSecureIdxSerie(const QIdx: integer);
var
  n: Integer;
begin
  FIdxSerie := MIN_SERIE_IDX;
  n :=  FDocTopo.GetNbSeries();
  if (n = 0) then exit;
  FIdxSerie := QIdx;
  if (QIdx < MIN_SERIE_IDX) then FIdxSerie := MIN_SERIE_IDX;
  if (QIdx > n - 1) then FIdxSerie := n - 1;
end;


procedure TCdrNavigateurSeries.Skip(const Offset: integer);
var
  MySerie: TObjSerie;
begin
  // Valeurs de l'offset
  //  0 = Première série
  // 1001 = Dernière série
  // Autre valeur: Se déplacer de Offset

  if (FDejaChangeDeSerie and Assigned(FProcDisAllowedChangeSerie)) then
  begin
    if (FProcDisAllowedChangeSerie()) then
    begin
      case MessageDlg('Changement de série demandé', 'Sauvegarder les modifications', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes   : FProcApplyModifs;
        mrNo    : pass; // ne rien faire et passer au label Suite:
        mrCancel: Exit;
      end;
    end;
  end;
  // suite:
  if      (Offset = TO_FIRST)   then SetSecureIdxSerie(0)
  else if (Offset = TO_REFRESH) then SetSecureIdxSerie(FIdxSerie)
  else if (Offset = TO_LAST)    then SetSecureIdxSerie(FDocTopo.GetNbSeries() - 1)
  else                               SetSecureIdxSerie(FIdxSerie + Offset);
  MySerie := FDocTopo.GetSerie(FIdxSerie);
  if (Assigned(FProcGotoSerie)) then FProcGotoSerie();
  FDejaChangeDeSerie := True;
end;
procedure TCdrNavigateurSeries.LoadSerie(const QIdx: integer);
var
  MySerie: TObjSerie;
begin
  SetSecureIdxSerie(QIdx);
  MySerie := FDocTopo.GetSerie(FIdxSerie);
  //editNomSerie.Text         := MySerie.GetNomSerie();
  if (Assigned(FProcGotoSerie)) then FProcGotoSerie();
end;

procedure TCdrNavigateurSeries.SpeedButton10Click(Sender: TObject);
begin

end;

end.

