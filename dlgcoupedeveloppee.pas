unit dlgCoupeDeveloppee;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  StructuresDonnees,
  ToporobotClasses2012,
  unitCoupeDeveloppee,
  UnitObjetSerie,
  CadreCoupeDeveloppee,
  CallDialogsStdVersion,
  UnitListesSimplesWithGeneriques,
  Classes, SysUtils, FileUtil, curredit, SynEdit,
  unitUtilsComposants,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, PairSplitter, StdCtrls,
  ComCtrls, Buttons, types, LCLType, ActnList;

type

  { TfrmCoupeDeveloppee }

  TfrmCoupeDeveloppee = class(TForm)
    acReverseSensBrancheFromMap: TAction;
    acOpenCoupe: TAction;
    acSaveCoupe: TAction;
    acRegen: TAction;
    acRemoveLastSerie: TAction;
    acExportCoupeDevSVG: TAction;
    acMakeCoupe: TAction;
    acExportGCP: TAction;
    acClearCoupe: TAction;
    acReinterpreterScript: TAction;
    Action2: TAction;
    acAddSerie: TAction;
    acParametreVue: TAction;
    Action3: TAction;
    acZoomPlus: TAction;
    acZoomMoins: TAction;
    acZoomFenetre: TAction;
    acPanVue: TAction;
    acZoomtout: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    btnRegenCoupe: TButton;
    Button1: TButton;
    Button2: TButton;
    CdrCoupeDeveloppee1: TCdrCoupeDeveloppee;
    chkRecalculerAChaqueAjout: TCheckBox;
    editScriptCoupeDev: TSynEdit;
    editSerBranche: TCurrencyEdit;
    editStBranche: TCurrencyEdit;
    hcColsTitres: THeaderControl;
    ImageList1: TImageList;
    imgLstTreeView: TImageList;
    lsbListeDesSeries: TListBox;
    lsbSeriesCoupe: TListBox;
    lsbSeriesLieesAtCurrent: TListBox;
    PageControl1: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlSeriesLiees: TPanel;
    pnlListeSeries: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    lbSerieParente: TStaticText;
    TabSheet1: TTabSheet;
    btnVisiPnlSeriesLiees: TToggleBox;
    TabSheet2: TTabSheet;
    procedure acAddSerieExecute(Sender: TObject);
    procedure acClearCoupeExecute(Sender: TObject);
    procedure acExportCoupeDevSVGExecute(Sender: TObject);
    procedure acExportGCPExecute(Sender: TObject);
    procedure acMakeCoupeExecute(Sender: TObject);
    procedure acOpenCoupeExecute(Sender: TObject);
    procedure acPanVueExecute(Sender: TObject);
    procedure acParametreVueExecute(Sender: TObject);
    procedure acRegenExecute(Sender: TObject);
    procedure acReinterpreterScriptExecute(Sender: TObject);
    procedure acRemoveLastSerieExecute(Sender: TObject);
    procedure acReverseSensBrancheFromMapExecute(Sender: TObject);
    procedure acSaveCoupeExecute(Sender: TObject);
    procedure acZoomFenetreExecute(Sender: TObject);
    procedure acZoomMoinsExecute(Sender: TObject);
    procedure acZoomPlusExecute(Sender: TObject);
    procedure acZoomtoutExecute(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure btnRegenCoupeClick(Sender: TObject);
    procedure btnRecenserBranchesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl;      Section: THeaderSection);
    procedure lsbBrancheClick(Sender: TObject);
    procedure lsbListeDesSeriesClick(Sender: TObject);
    procedure lsbListeDesSeriesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbListeDesSeriesSelectionChange(Sender: TObject; User: boolean);
    procedure lsbSeriesCoupeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbSeriesLieesAtCurrentDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure btnVisiPnlSeriesLieesChange(Sender: TObject);
  private
    { private declarations }
    FDocToporobot: TToporobotStructure2012;
    FCoupeDeveloppee: TCoupeDeveloppee;
    FNumeroSerie : integer;
    procedure BasculerBrancheByIdx(const Idx: integer; const NumSerie, NumStation: integer);
    procedure DessinerItemDeListe(const LSB: TListBox; const DoUseInternalIdxSerie: boolean; Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure InitCaptions();
    procedure InitHeaderListe();
    procedure ListerLesSeries(const QIdx: integer);
    procedure ListerLesSeriesRetenues(const Idx: integer);
    procedure RecalculerGrapheEtRegenererVue(const DoRecenserBranches, DoZoom: boolean);
    procedure ZoomerPM(const CoefZoom: double; const Grooth: boolean);
  public
    { public declarations }
    function Initialiser(const T: TToporobotStructure2012): boolean;
    procedure Finaliser();
  end;

var
  frmCoupeDeveloppee: TfrmCoupeDeveloppee;

implementation

{$R *.lfm}

{ TfrmCoupeDeveloppee }
// lister les séries
procedure TfrmCoupeDeveloppee.ListerLesSeries(const QIdx: integer);
var
  i: Integer;
  S: TObjSerie;
  EWE: Integer;
begin
  EWE := QIdx;
  lsbListeDesSeries.Enabled:=False;
  lsbListeDesSeries.Clear;
  for i:= 0 to FDocToporobot.GetNbSeries - 1 do
  begin
    S := FDocToporobot.GetSerie(i);
    lsbListeDesSeries.Items.Add(Format('%d: %s', [S.GetNumeroDeSerie(), S.GetNomSerie()]));
  end;
  if (QIdx = -1) then
  begin
    EWE := FDocToporobot.GetNbSeries() - 1;
    FNumeroSerie := EWE;
  end;
  lsbListeDesSeries.ItemIndex := EWE;
  lsbListeDesSeries.Enabled:=True;
end;

procedure TfrmCoupeDeveloppee.lsbBrancheClick(Sender: TObject);
begin

end;

procedure TfrmCoupeDeveloppee.lsbListeDesSeriesClick(Sender: TObject);
begin

end;

// factorisation de code pour les listes
procedure TfrmCoupeDeveloppee.DessinerItemDeListe(const LSB: TListBox;
                                                  const DoUseInternalIdxSerie: boolean;
                                                  Control: TWinControl;
                                                  Index: Integer;
                                                  ARect: TRect;
                                                  State: TOwnerDrawState);
var
  sr: TObjSerie;
  rs: TReseau;
  n: LongInt;
  QIdx: integer;
  procedure DessineItem(const bg,tc: TColor);
  begin
    ResetColorRow(LSB, ARect, bg, tc);
    DrawColTexte(LSB, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[sr.GetNumeroDeSerie()]));
    DrawColTexte(LSB, ARect, hcColsTitres.Sections.Items[1], True , Format(FMTSERST,[sr.GetNoSerieDep, sr.GetNoPointDep]));
    DrawColTexte(LSB, ARect, hcColsTitres.Sections.Items[2], True , Format(FMTSERST,[sr.GetNoSerieArr, sr.GetNoPointArr]));
    DrawColTexte(LSB, ARect, hcColsTitres.Sections.Items[3], True , _AnsiToLCLStr(sr.GetNomSerie));
    rs := FDocToporobot.GetReseau(sr.GetNumeroReseau());
    DrawColRectColoreWithTexte(LSB, ARect, hcColsTitres.Sections.Items[4], True , bg, rs.ColorReseau, _AnsiToLCLStr(rs.NomReseau));
    DrawColTexte(LSB, ARect, hcColsTitres.Sections.Items[5], True , Format(FORMAT_NB_INTEGER,[sr.GetNbVisees()]));
  end;
begin
  try
    if (DoUseInternalIdxSerie) then
    begin
      sr := FDocToporobot.GetSerie(Index);
    end
    else
    begin
      n  := StrToIntDef(LSB.Items[Index], 0);
      if (not FDocToporobot.GetSerieByNumeroSerie(n, SR, QIdx)) then Exit;
    end;
    if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clwhite, clBlack);
  except
  end;
end;

procedure TfrmCoupeDeveloppee.lsbListeDesSeriesDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  DessinerItemDeListe(lsbListeDesSeries, True, Control, Index, ARect, State);
end;

procedure TfrmCoupeDeveloppee.lsbListeDesSeriesSelectionChange(Sender: TObject; User: boolean);
var
  LS: TListOfIntegers;
  i: Integer;
  EWE: TObjSerie;
begin
  if (lsbListeDesSeries.Count = 0) then Exit;
  LS := TListOfIntegers.Create;
  try
    lsbSeriesLieesAtCurrent.Clear;
    if (FDocToporobot.FindIndexesSeriesRattachedAtSerie(lsbListeDesSeries.ItemIndex, LS)) then
    begin
      EWE := FDocToporobot.GetSerie(lsbListeDesSeries.ItemIndex);
      lbSerieParente.caption := Format('%d - %s', [EWE.GetNumeroDeSerie(), EWE.GetNomSerie()]);
      for i := 0 to LS.Count - 1 do lsbSeriesLieesAtCurrent.Items.Add(Format(FORMAT_NB_INTEGER, [LS.GetElement(i)]));
      lsbSeriesLieesAtCurrent.ItemIndex := 0;
    end;
  finally
    FreeAndNil(LS);//LS.Free;
  end;
end;

procedure TfrmCoupeDeveloppee.lsbSeriesCoupeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  DessinerItemDeListe(lsbSeriesCoupe, false, Control, Index, ARect, State);
end;

procedure TfrmCoupeDeveloppee.lsbSeriesLieesAtCurrentDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  DessinerItemDeListe(lsbSeriesLieesAtCurrent, false, Control, Index, ARect, State);
end;

procedure TfrmCoupeDeveloppee.btnVisiPnlSeriesLieesChange(Sender: TObject);
begin
  pnlSeriesLiees.Visible := btnVisiPnlSeriesLiees.Checked;
end;





// initialisation header
procedure TfrmCoupeDeveloppee.InitHeaderListe();
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
  FNbItems := FDocToporobot.GetNbSeries;
  //pnlListes.Caption := Format('%s (%d items)', [miou, FNbItems]);
  with hcColsTitres do
  begin
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_ID, 40, 40);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_DEPART, 70, 70);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_ARRIVEE, 70, 70);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_NOM, 100, 300);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_RESEAU, 100, 300);
    AjouterTitreColonneRes(rsSELECT_LISTE_SERIES_NBPOINTS, 60, 60);
  end;
end;



procedure TfrmCoupeDeveloppee.acZoomtoutExecute(Sender: TObject);
begin
  CdrCoupeDeveloppee1.ResetVue;
end;

procedure TfrmCoupeDeveloppee.acZoomFenetreExecute(Sender: TObject);
begin
  CdrCoupeDeveloppee1.SetModeTravail(mtZOOM_PREMIER_COIN);
end;

procedure TfrmCoupeDeveloppee.acZoomMoinsExecute(Sender: TObject);
begin
  ZoomerPM(1.10, false);
end;

procedure TfrmCoupeDeveloppee.acZoomPlusExecute(Sender: TObject);
begin
  ZoomerPM(1.10, true);
end;

procedure TfrmCoupeDeveloppee.acPanVueExecute(Sender: TObject);
begin
  CdrCoupeDeveloppee1.SetModeTravail(mtPAN_PREMIER_POINT);
end;

procedure TfrmCoupeDeveloppee.acParametreVueExecute(Sender: TObject);
var
  WU: TCoupeDeveloppeeParams;
begin
  WU := CdrCoupeDeveloppee1.GetParamsVueCoupe();
  if (ParametrerOngletCoupeDeveloppee(WU)) then CdrCoupeDeveloppee1.SetParamsVueCoupe(WU);
end;


procedure TfrmCoupeDeveloppee.acExportCoupeDevSVGExecute(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
  if (DoDialogSaveFile('Scalable Vector Graphics (*.svg)|*.svg|', '.svg', QFilename, QFilterIndex)) then
    FCoupeDeveloppee.ExportSVG(QFilename);
end;

procedure TfrmCoupeDeveloppee.acExportGCPExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QIdxFilter: integer;
begin
  if (DoDialogSaveFile('Polygonale GHCaveDraw (*.gcp)|*.gcp', '.gcp', QFileName, QIdxFilter)) then
  begin
    FCoupeDeveloppee.ExporterCoupeGCP(QFileName);
  end;
end;

procedure TfrmCoupeDeveloppee.acAddSerieExecute(Sender: TObject);
var
  n: LongInt;
  QIdx: integer;
  MySerie: TObjSerie;
begin
  if (lsbListeDesSeries.Count = 0) then exit;
  n := lsbListeDesSeries.ItemIndex;
  MySerie := FDocToporobot.GetSerie(n);
  if (FCoupeDeveloppee.AddSerie(MySerie, True)) then
  begin
    editScriptCoupeDev.Lines.Add(Format('AddSerie %d // %s', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]));
    if (chkRecalculerAChaqueAjout.Checked) then RecalculerGrapheEtRegenererVue(True, false);
    ListerLesSeriesRetenues(-1);
  end
  else
  begin
    ShowMessage('Série non raccordée au réseau');
  end;
end;

procedure TfrmCoupeDeveloppee.acClearCoupeExecute(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Supprimer tout')) then
  begin
    FCoupeDeveloppee.ResetAll();
    ListerLesSeriesRetenues(-1);

  end;
end;

procedure TfrmCoupeDeveloppee.acMakeCoupeExecute(Sender: TObject);
begin
  if (lsbListeDesSeries.ItemIndex > 0) then
  begin
    RecalculerGrapheEtRegenererVue(True, false);
    ListerLesSeriesRetenues(-1);
  end;
end;

procedure TfrmCoupeDeveloppee.ListerLesSeriesRetenues(const Idx: integer);
var
  Nb, i: Integer;
  EWE: TObjSerie;
begin
  lsbSeriesCoupe.Clear;
  Nb := FCoupeDeveloppee.GetNbSeries();
  AfficherMessage(Format('ListerSeriesRetenues: %d series', [Nb]));
  if (Nb = 0) then Exit;
  for i := 0 to Nb -1 do
  begin
    EWE := FCoupeDeveloppee.GetSerie(i);
    lsbSeriesCoupe.Items.add(Format(FORMAT_NB_INTEGER, [EWE.GetNumeroDeSerie()]));
    lsbSeriesCoupe.ItemIndex := IIF(Idx = -1, lsbSeriesCoupe.Count-1, Idx);
  end;
end;


procedure TfrmCoupeDeveloppee.acOpenCoupeExecute(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
  QMyScript: TStringList;
  Nb, i: Integer;
begin
  if (DoDialogOpenFile('Coupes développées|coupe_dev*.txt|Tous|*.*', '.txt', QFilename)) then
  begin
     if (FCoupeDeveloppee.LoadScriptCoupe(QFilename)) then
     begin
       QMyScript := FCoupeDeveloppee.Script;
       Nb := QMyScript.Count;
       editScriptCoupeDev.Lines.Clear;
       for i := 0 to Nb-1 do editScriptCoupeDev.Lines.add(QMyScript.Strings[i]);
       if (FCoupeDeveloppee.InterpreterScript()) then
       begin
         FCoupeDeveloppee.SetMiniMaxi();
         CdrCoupeDeveloppee1.ResetVue();
         ListerLesSeriesRetenues(-1);
       end
     end;
  end;
end;

procedure TfrmCoupeDeveloppee.acRegenExecute(Sender: TObject);
begin
  //FCoupeDeveloppee.RecalculerLaCoupe;
  ListerLesSeriesRetenues(-1);
  CdrCoupeDeveloppee1.ResetVue();
end;

procedure TfrmCoupeDeveloppee.acReinterpreterScriptExecute(Sender: TObject);
var
  i, Nb: Integer;
begin
  Nb := editScriptCoupeDev.Lines.Count;
  if (Nb = 0) then exit;
  if (not GHTopoQuestionOuiNon(rsCOUPE_DEVEL_WARN_COUPE_WILL_RESET)) then exit;
  FCoupeDeveloppee.Script.Clear;
  for i := 0 to Nb -1 do FCoupeDeveloppee.Script.Add(trim(editScriptCoupeDev.Lines[i]));
  if (FCoupeDeveloppee.InterpreterScript()) then
  begin
    FCoupeDeveloppee.SetMiniMaxi();
    CdrCoupeDeveloppee1.ResetVue();
    ListerLesSeriesRetenues(-1);
  end;
end;

procedure TfrmCoupeDeveloppee.acRemoveLastSerieExecute(Sender: TObject);
var
  Nb: Integer;
begin
  if (not GHTopoQuestionOuiNon(rsCOUPE_DEVEL_AC_REMOVE_SERIE)) then Exit;
  try
    Nb := FCoupeDeveloppee.GetNbSeries();
    if (Nb = 0) then exit;
    FCoupeDeveloppee.RemoveSerie(Nb - 1);
    ListerLesSeries(0);
    RecalculerGrapheEtRegenererVue(True, false);
    ListerLesSeriesRetenues(-1);
    CdrCoupeDeveloppee1.ResetVue();
  except
  end;
end;

procedure TfrmCoupeDeveloppee.acReverseSensBrancheFromMapExecute(Sender: TObject);
var
  Idx, QNumSerie, QNumStation: Integer;
begin
  Idx := CdrCoupeDeveloppee1.GetCurrentIdxBranche();
  QNumSerie   := CdrCoupeDeveloppee1.GetCurrentNumSerie();
  QNumStation := CdrCoupeDeveloppee1.GetCurrentNumStation();

  BasculerBrancheByIdx(Idx, QNumSerie, QNumStation);
end;

procedure TfrmCoupeDeveloppee.acSaveCoupeExecute(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
  QFilterIndex: integer;
begin
   if (DoDialogSaveFile('Coupe developpee (*.txt)|*.txt|', '.txt', QFilename, QFilterIndex)) then
   begin
     FCoupeDeveloppee.SaveScriptCoupe(QFilename);
   end;
end;

procedure TfrmCoupeDeveloppee.BasculerBrancheByIdx(const Idx: integer; const NumSerie, NumStation: integer);
begin
  if (Idx >= 0) then
  begin
    FCoupeDeveloppee.BasculerSensDessinBrancheByIdx(Idx);
    // et ajouter au script
    editScriptCoupeDev.Lines.Add(Format('TurnBranchOfSerSt %d.%d', [NumSerie, NumStation]));
    RecalculerGrapheEtRegenererVue(False, True);
  end;
end;

procedure TfrmCoupeDeveloppee.BitBtn4Click(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmCoupeDeveloppee.InitCaptions();
  procedure SetAcHint(const AC: TAction; const QCaption: string);
  var
    EWE: TStringArray;
  begin
    EWE := QCaption.Split(['|']);
    AC.Caption := GetResourceString(Trim(EWE[0]));
    AC.Hint    := GetResourceString(Trim(EWE[1]));
  end;
begin
  SetAcHint(acOpenCoupe          , rsCOUPE_DEVEL_AC_OPEN_COUPE);
  SetAcHint(acSaveCoupe          , rsCOUPE_DEVEL_AC_SAVE_COUPE);
  SetAcHint(acExportCoupeDevSVG  , rsCOUPE_DEVEL_AC_EXPORT_SVG);
  SetAcHint(acMakeCoupe          , rsCOUPE_DEVEL_AC_MAKE_COUPE);
  SetAcHint(acAddSerie           , rsCOUPE_DEVEL_AC_ADD_SERIE);
  SetAcHint(acRemoveLastSerie    , rsCOUPE_DEVEL_AC_REMOVE_SERIE);
  SetAcHint(acClearCoupe         , rsCOUPE_DEVEL_AC_DO_CLEAR_ALL);
  SetAcHint(acReinterpreterScript, rsCOUPE_DEVEL_AC_REINTERPRETER_SCRIPT);
end;





procedure TfrmCoupeDeveloppee.btnRegenCoupeClick(Sender: TObject);
begin
  //FCoupeDeveloppee.RecalculerLaCoupe;
  ListerLesSeriesRetenues(-1);
  CdrCoupeDeveloppee1.ResetVue;
end;





procedure TfrmCoupeDeveloppee.btnRecenserBranchesClick(Sender: TObject);
begin
  FCoupeDeveloppee.RecenserLesBranches;
end;

procedure TfrmCoupeDeveloppee.Button1Click(Sender: TObject);
var
  IdxBranche: Integer;
  MyBranche: TBrancheCoupeDeveloppeeAsRecord;
begin

  IdxBranche := FCoupeDeveloppee.FindIdxBrancheContainingSerieStation(editSerBranche.AsInteger, editStBranche.AsInteger);
  if (IdxBranche > 0) then
  begin
    MyBranche := FCoupeDeveloppee.GetBranche(IdxBranche);
    ShowMessageFmt('%d: %s', [MyBranche.NumeroSerie, MyBranche.NomBranche]);
  end
  else
    ShowMessage('Branche non trouvée');
end;


procedure TfrmCoupeDeveloppee.RecalculerGrapheEtRegenererVue(const DoRecenserBranches, DoZoom: boolean);
var
  C1, C2: TPointCoupeDeveloppee;
begin
  C1 := CdrCoupeDeveloppee1.GetCoinBasGauche();
  C2 := CdrCoupeDeveloppee1.GetCoinHautDroit();

  if (FCoupeDeveloppee.ConstruireLaCoupeDeveloppee(DoRecenserBranches)) then
  begin
    AfficherMessage('-- Graphe aux sommets OK');
    FCoupeDeveloppee.SetMiniMaxi;
    CdrCoupeDeveloppee1.ResetVue;
    if (DoZoom) then CdrCoupeDeveloppee1.SetViewLimits(C1.P, C1.Z, C2.P, C2.Z);
  end
  else
  begin
    ShowMessage('Le réseau n''est pas entièrement connecté');
  end;
end;

procedure TfrmCoupeDeveloppee.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  self.Finaliser();
end;

procedure TfrmCoupeDeveloppee.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  if (GHTopoQuestionOuiNon('Quitter')) then
  begin
    CanClose := True;
  end;
end;



procedure TfrmCoupeDeveloppee.hcColsTitresSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbListeDesSeries.Invalidate;
end;


function TfrmCoupeDeveloppee.Initialiser(const T: TToporobotStructure2012): boolean;
begin
  Result := false;
  InitCaptions();
  FDocToporobot := T;
  FCoupeDeveloppee := TCoupeDeveloppee.Create;
  try
    FCoupeDeveloppee.Initialiser(T);
    InitHeaderListe;
    ListerLesSeries(0);
    CdrCoupeDeveloppee1.Initialise(FCoupeDeveloppee);
    Result := True;
  except
  end;
end;

procedure TfrmCoupeDeveloppee.Finaliser();
begin
  try
    FCoupeDeveloppee.Finaliser();
  finally
    FreeAndNil(FCoupeDeveloppee);//FCoupeDeveloppee.Free;
  end;
end;

procedure TfrmCoupeDeveloppee.ZoomerPM(const CoefZoom : double; const Grooth : boolean);
var
  Q1, Q2   : TPointCoupeDeveloppee;
  QC1, QC2 : TPointCoupeDeveloppee;
  Diag     : Double;
  Sens     : double;
begin
  if (Not CdrCoupeDeveloppee1.getDoDrawCoupe) then Exit;
  try
    Q1     := CdrCoupeDeveloppee1.GetCoinBasGauche;
    Q2     := CdrCoupeDeveloppee1.GetCoinHautDroit;
    Sens   := IIF(Grooth, 1, -1);
    Diag   := Sens * CoefZoom * 0.01 * Hypot2D(Q2.P - Q1.P, Q2.Z - Q1.Z);
    QC1    := MakeTPointCoupeDeveloppee(Q1.P + Diag, Q1.Z + Diag);
    QC2    := MakeTPointCoupeDeveloppee(Q2.P - Diag, Q2.Z - Diag);
    CdrCoupeDeveloppee1.SetViewLimits(QC1.P, QC1.Z, QC2.P, QC2.Z);
    CdrCoupeDeveloppee1.RedessinEcran;
  except
  end;
end;
end.

