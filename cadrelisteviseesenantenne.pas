unit CadreListeViseesEnAntenne;

//{$ERROR Fichier inutilisé}
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  CallDialogsStdVersion, Dialogs,
  Graphics, LCLType,
  ToporobotClasses2012, UnitObjetSerie,
  UnitListesSimplesWithGeneriques,
  unitUtilsComposants,
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, ActnList, Buttons,  curredit, Clipbrd, Types;

type

{ TCdrListeViseesEnAntenne }

 TCdrListeViseesEnAntenne = class(TFrame)
    acActionsListeAntennes: TActionList;
    acNettoyerAntennes: TAction;
    acRelister: TAction;
    acSupprimerAntennesSelectionnees: TAction;
    acAddAntenneManually: TAction;
    acPreviousSerie: TAction;
    acNextSerie: TAction;
    acGotoNthStation: TAction;
    acSaveToCSV: TAction;
    acNex10tSerie: TAction;
    acPrevious10Serie: TAction;
    acFirstSerie: TAction;
    acLastSerie: TAction;
    acCopierListe: TAction;
    hcColsTitres: THeaderControl;
    lbSerie: TLabel;
    lbPoint: TLabel;
    editNumeroSerie: TCurrencyEdit;
    imgLstListeAntennes: TImageList;
    lbNbViseesAntennes: TLabel;
    lsbViseesAntennes: TListBox;
    pnlListeAntennes: TPanel;
    Panel2: TPanel;
    sclStationsOfSerie: TScrollBar;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton9: TSpeedButton;
    lbNbStationsOfSerie: TStaticText;
    procedure acAddAntenneManuallyExecute(Sender: TObject);
    procedure acCopierListeExecute(Sender: TObject);
    procedure acFirstSerieExecute(Sender: TObject);
    procedure acLastSerieExecute(Sender: TObject);
    procedure acNettoyerAntennesExecute(Sender: TObject);
    procedure acNex10tSerieExecute(Sender: TObject);
    procedure acNextSerieExecute(Sender: TObject);
    procedure acPrevious10SerieExecute(Sender: TObject);
    procedure acPreviousSerieExecute(Sender: TObject);
    procedure acRelisterExecute(Sender: TObject);
    procedure acSaveToCSVExecute(Sender: TObject);
    procedure acSupprimerAntennesSelectionneesExecute(Sender: TObject);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbViseesAntennesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure sclStationsOfSerieChange(Sender: TObject);
  private
    FIsReady    : boolean;
    FMyDocTopo  : TToporobotStructure2012;
    FInternalIdxSerie    : integer;
    //FCurrentNumeroStation: integer;
    FListeIdxAntennesRetenues: TListOfIntegers;
    procedure ExporterAntennesSelectedCSV(const Filename: TStringDirectoryFilename);
    procedure GotoSerie(const Idx: integer);
    procedure ListerLesAntennesOfSerie(const QSerie: TObjSerie; const QStation: integer = 0);
  public
    function  Initialiser(const DocuTopo: TToporobotStructure2012; const QCurrentNumeroSerie: TNumeroSerie = 0): boolean;
    procedure Finaliser();
    procedure SetCurrentInternalIdxSerie(const NS : TNumeroSerie);
  end;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}



{ TCdrListeViseesEnAntenne }
function TCdrListeViseesEnAntenne.Initialiser(const DocuTopo: TToporobotStructure2012; const QCurrentNumeroSerie: TNumeroSerie = 0): boolean;
  procedure SetAcHint(const A: TAction; const H: string);
  var
    QS: String;
  begin
    QS := GetResourceString(H);
    A.Caption := QS;
    A.Hint    := EnlevePerluete(QS);
  end;
begin
  result := false;
  // captions des actions
  SetAcHint(acRelister            , rsCDR_LISTE_ANTENNES_RELISTER);
  SetAcHint(acNextSerie           , rsCDR_LISTE_ANTENNES_NEXT_STATION);
  SetAcHint(acPreviousSerie       , rsCDR_LISTE_ANTENNES_PREV_STATION);

  SetAcHint(acGotoNthStation      , rsCDR_LISTE_ANTENNES_GOTO_NTH_STATION);
  SetAcHint(acSaveToCSV           , rsCDR_LISTE_ANTENNES_SAVE_CSV);
  SetAcHint(acAddAntenneManually  , rsCDR_LISTE_ANTENNES_ADD_ANTENNE);
  SetAcHint(acNettoyerAntennes    , rsCDR_LISTE_ANTENNES_NETTOYER);
  SetAcHint(acSupprimerAntennesSelectionnees, rsCDR_LISTE_ANTENNES_DELETE_SELECTED);
  SetAcHint(acNettoyerAntennes    , 'Nettoyer les visées');
  SetAcHint(acRelister            , 'Rafraichir');
  SetAcHint(acCopierListe         , rsCDR_SERIES_FLAT_TAB_COPY_TABLEAU);

  lsbViseesAntennes.MultiSelect := True;

  FInternalIdxSerie     := 0;
  //FCurrentNumeroStation := 0;
  FMyDocTopo := DocuTopo;
  {$IFDEF GHTOPO_SIMPLIFIE}
  editNumeroSerie.Enabled  := False;
  acGotoNthStation.Visible := True;
  {$ELSE}
  editNumeroSerie.Enabled  := true;
  editCurrStation.Enabled  := true;
  acGotoNthStation.Visible := false;
  {$ENDIF GHTOPO_SIMPLIFIE}
  FListeIdxAntennesRetenues := TListOfIntegers.Create;
  try
    FListeIdxAntennesRetenues.ClearListe();
    SetCurrentInternalIdxSerie(QCurrentNumeroSerie);
    ListerLesAntennesOfSerie(FMyDocTopo.GetSerie(FInternalIdxSerie));
    result := True;
  except
  end;
  FIsReady := Result;
end;

procedure TCdrListeViseesEnAntenne.Finaliser();
begin
  FIsReady := false;
  try
    FListeIdxAntennesRetenues.ClearListe();
  finally
    FreeAndNil(FListeIdxAntennesRetenues);
  end;
end;

procedure TCdrListeViseesEnAntenne.SetCurrentInternalIdxSerie(const NS: TNumeroSerie);
var
  MySerie: TObjSerie;
begin
  if (not FMyDocTopo.GetSerieByNumeroSerie(NS, MySerie, FInternalIdxSerie)) then exit;
  GotoSerie(FInternalIdxSerie);
end;


procedure TCdrListeViseesEnAntenne.ListerLesAntennesOfSerie(const QSerie: TObjSerie; const QStation: integer = 0);
const
  FMT_LN = '%6d | %11s | %8.3f | %8.3f | %8.3f | %s';
var
  NbAntennes, i: Integer;
  EWE: TViseeAntenne;
  WU, Nb: Integer;
  QAT: String;
  QNumeroDeSerie: TNumeroSerie;
begin
  AfficherMessage('ListerLesAntennesOfSerie() ' + Inttostr(QSerie.GetNumeroDeSerie()));
  NbAntennes := FMyDocTopo.GetNbAntennes();
  lbNbViseesAntennes.Caption := format('%d', [NbAntennes]);
  if (0 = NbAntennes) then Exit;
  QNumeroDeSerie := QSerie.GetNumeroDeSerie();
  lsbViseesAntennes.Visible := false;
  FListeIdxAntennesRetenues.ClearListe();
  lsbViseesAntennes.Clear;

  if (0 = QNumeroDeSerie) then    // lister toutes les antennes
  begin
    AfficherMessage(format('--> Toutes les %d antennes', [NbAntennes]));
    for i := 0 to NbAntennes - 1 do FListeIdxAntennesRetenues.AddElement(i);
  end
  else
  begin
    for i := 0 to NbAntennes - 1 do
    begin
      EWE := FMyDocTopo.GetViseeAntenne(i);
      if (EWE.SerieDepart = QNumeroDeSerie) then
      begin
        if (0 = QStation) then    // toutes les antennes de la série
          FListeIdxAntennesRetenues.AddElement(i)
        else                      // seulement les antennes de la station
          if (QStation = EWE.PtDepart) then FListeIdxAntennesRetenues.AddElement(i);
      end;
    end;
  end;
  // ajouter les antennes retenues à la listbox
  Nb := FListeIdxAntennesRetenues.GetNbElements();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do lsbViseesAntennes.Items.add(inttostr(i));
    lsbViseesAntennes.ItemIndex := 0;
  end
  else
  begin
    pnlListeAntennes.Caption := 'Aucune visée radiante';
  end;
  AfficherMessage(Format('%d antennes retenues', [FListeIdxAntennesRetenues.GetNbElements()]));
  lsbViseesAntennes.Visible  := True;
  lbNbViseesAntennes.Caption := format('%d antennes affichées sur %d', [FListeIdxAntennesRetenues.GetNbElements(), FMyDocTopo.GetNbAntennes()]);
end;

procedure TCdrListeViseesEnAntenne.ExporterAntennesSelectedCSV(const Filename: TStringDirectoryFilename);
var
  fp            : TextFile;
  QNbAntennes, i: Integer;
  VA            : TViseeAntenne;
  EWE           : String;
begin
  AfficherMessage(Format('%s.ExporterAntennesSelectedCSV: %s', [ClassName, FileName]));
  QNbAntennes := FListeIdxAntennesRetenues.GetNbElements();
  if (0 = QNbAntennes) then exit;
  AssignFile(fp, FileName);
  try
    Rewrite(fp);
    WriteLn(fp, 'No' + #9 + 'Serie' + #9 + 'Point' + #9 + 'Longueur' + #9 + 'Azimut' + #9 + 'Pente');
    for i := 1 to QNbAntennes - 1 do
    begin
      VA := FMyDocTopo.GetViseeAntenne(FListeIdxAntennesRetenues.GetElement(i));
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






procedure TCdrListeViseesEnAntenne.acNettoyerAntennesExecute(Sender: TObject);
var
  BF: TProcDisplayProgression;
  WU: Integer;
  QSerie: TObjSerie;
begin
  try
    if (GHTopoQuestionOuiNon(GetResourceString(rsCDR_ANTENNES_CONFIRM_NETTOYAGE))) then
    begin
      acNettoyerAntennes.Enabled := false;
      BF := FMyDocTopo.GetProcDisplayProgression();
      FMyDocTopo.SetProcDisplayProgression(nil);
      WU  := FMyDocTopo.NettoyerAntennes(40.00, -1, -1);
      QSerie := FMyDocTopo.GetSerie(FInternalIdxSerie);
      ListerLesAntennesOfSerie(QSerie);
    end;
  finally
    acNettoyerAntennes.Enabled := True;
    FMyDocTopo.SetProcDisplayProgression(BF);
  end;
end;

procedure TCdrListeViseesEnAntenne.GotoSerie(const Idx: integer);
var
  MySerie: TObjSerie;
  n, IdxMaxSerie: Integer;
begin
  FInternalIdxSerie := Idx;
  IdxMaxSerie := FMyDocTopo.GetNbSeries() - 1;
  if (FInternalIdxSerie < 1) then FInternalIdxSerie := 1;
  if (FInternalIdxSerie > IdxMaxSerie) then FInternalIdxSerie := IdxMaxSerie;
  MySerie := FMyDocTopo.GetSerie(FInternalIdxSerie);
  n := MySerie.GetNbVisees() - 1;
  editNumeroSerie.AsInteger   := MySerie.GetNumeroDeSerie();
  sclStationsOfSerie.Min      := 0;
  sclStationsOfSerie.Max      := MySerie.GetNbVisees() - 1;
  sclStationsOfSerie.Position := 0;
  sclStationsOfSerie.Visible  := (MySerie.GetNbVisees() > 0);
  lbNbStationsOfSerie.caption := Format('%d / %d', [sclStationsOfSerie.Position, sclStationsOfSerie.Max]);
  ListerLesAntennesOfSerie(MySerie, 0);
end;


procedure TCdrListeViseesEnAntenne.acFirstSerieExecute(Sender: TObject);
begin
  GotoSerie(1);
end;
procedure TCdrListeViseesEnAntenne.acPrevious10SerieExecute(Sender: TObject);
begin
  GotoSerie(FInternalIdxSerie - 10);
end;

procedure TCdrListeViseesEnAntenne.acPreviousSerieExecute(Sender: TObject);
begin
  GotoSerie(FInternalIdxSerie - 1);
end;

procedure TCdrListeViseesEnAntenne.acNex10tSerieExecute(Sender: TObject);
begin
  GotoSerie(FInternalIdxSerie + 10);
end;

procedure TCdrListeViseesEnAntenne.acNextSerieExecute(Sender: TObject);
begin
  GotoSerie(FInternalIdxSerie + 1);
end;

procedure TCdrListeViseesEnAntenne.acLastSerieExecute(Sender: TObject);
begin
  GotoSerie(FMyDocTopo.GetNbSeries() - 1);
end;

procedure TCdrListeViseesEnAntenne.acAddAntenneManuallyExecute(Sender: TObject);
var
  MySerie          : TObjSerie;
  MyNumeroStation  : TNumeroStation;
  MyBaseStation    : TUneVisee;
  MyAntenne        : TViseeAntenne;
  EWE              : string;
  WU               : TGHStringArray;
  QLong, QPente, QAzimut: double;
begin
  EWE := '';
  MySerie := FMyDocTopo.GetSerie(FInternalIdxSerie);
  MyNumeroStation := MySerie.GetNbVisees() - 1;
  MyBaseStation   := MySerie.GetVisee(MyNumeroStation);
  // pour éviter la création d'une boîte de dialogue dédiée:
  // saisie à l'ancienne
  if (InputQuery('Ajout manuel d''une antenne', 'Long; Azimut; Pente', EWE)) then
  begin
    WU := Split(EWE, ';');
    QLong   := ConvertirEnNombreReel(Trim(WU[0]), -1.00);
    QAzimut := ConvertirEnNombreReel(Trim(WU[1]), -1.00);
    QPente  := ConvertirEnNombreReel(Trim(WU[2]), -101.00);
    if (QLong   <    0.00) then exit;
    if (QAzimut <    0.00) then exit;
    if (QPente  < -100.00) then exit;
    MyAntenne.EntranceRatt := MySerie.GetNumeroEntrance();
    MyAntenne.Reseau       := MySerie.GetNumeroReseau();
    MyAntenne.Secteur      := MyBaseStation.IDSecteur;
    MyAntenne.SerieDepart  := MySerie.GetNumeroDeSerie();
    MyAntenne.PtDepart     := MyNumeroStation;

    MyAntenne.setLongAzInc(QLong, QAzimut, QPente);
    FMyDocTopo.AddViseeAntenne(MyAntenne);
    ListerLesAntennesOfSerie(MySerie, 0);
  end;
end;

procedure TCdrListeViseesEnAntenne.acCopierListeExecute(Sender: TObject);
var
  CT: TClipboard;
  n: Integer;
  EWE, i: integer;
  WU: String;
  VA: TViseeAntenne;
begin
  n := FListeIdxAntennesRetenues.GetNbElements();
  if (n = 0) then Exit;
  CT := TClipboard.Create(ctClipboard);
  try
    CT.Clear;
    WU := 'Reseau' + #9 + 'Secteur' + #9 + 'Station' + #9 + 'Long.' + #9 + 'Az.' + #9 + 'Inc.' + #13#10;
    for i := 0 to n - 1 do
    begin
      EWE := FListeIdxAntennesRetenues.GetElement(i);
      VA  := FMyDocTopo.GetViseeAntenne(EWE);
      WU  += Format('%d', [VA.Reseau]) + #9 +
             Format('%d', [VA.Secteur]) + #9 +
             va.toString() + #9 +
             FormatterNombreOOo(VA.Longueur, 3) + #9 +
             FormatterNombreOOo(VA.Azimut  , 3) + #9 +
             FormatterNombreOOo(VA.Pente   , 3) + #13#10;
      CT.AsText := WU;
    end;
  finally
    FreeAndNil(CT);
  end;
end;

procedure TCdrListeViseesEnAntenne.acRelisterExecute(Sender: TObject);
begin
  pass;
end;

procedure TCdrListeViseesEnAntenne.acSaveToCSVExecute(Sender: TObject);
var
  QIdx: integer;
  QFilename: TStringDirectoryFilename;
begin
  if (0 = FListeIdxAntennesRetenues.GetNbElements()) then
  begin
    ShowMessage(GetResourceString(rsCDR_LISTE_ANTENNES_NONE));
    Exit;
  end;
  QFilename := GetGHTopoDirectory() + 'RadiantShots_' + DatePascalToDateHeureCondensee(Now());
  if (DoDialogSaveFile('Fichiers CSV|*.csv', '.csv', QFilename, QIdx)) then
  begin
    self.ExporterAntennesSelectedCSV(QFilename);
  end;
end;

procedure TCdrListeViseesEnAntenne.acSupprimerAntennesSelectionneesExecute(Sender: TObject);
var
  i: Integer;
  EWE: Integer;
  MySerie: TObjSerie;
begin
  if (0 = FListeIdxAntennesRetenues.GetNbElements()) then Exit;
  if (not GHTopoQuestionOuiNon('Supprimer les visées sélectionnées')) then Exit;
  MySerie := FMyDocTopo.GetSerie(FInternalIdxSerie);
  for i := lsbViseesAntennes.Count - 1 downto 0 do
  begin
    if (lsbViseesAntennes.Selected[i]) then
    begin
      EWE := FListeIdxAntennesRetenues.GetElement(i);
      FMyDocTopo.RemoveViseeAntenne(EWE);
    end;
  end;
  ListerLesAntennesOfSerie(MySerie, sclStationsOfSerie.Position);
end;

procedure TCdrListeViseesEnAntenne.hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbViseesAntennes.Invalidate;
end;

procedure TCdrListeViseesEnAntenne.lsbViseesAntennesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  WU : integer;
  V: TViseeAntenne;
  BGC: TColor;
  procedure DessineItem(const bg,tc: TColor);
  begin
    ResetColorRow(lsbViseesAntennes, ARect, bg, tc);
    DrawColTexte(lsbViseesAntennes, ARect, hcColsTitres.Sections.Items[0], false, Format('%d', [V.Reseau]));
    DrawColTexte(lsbViseesAntennes, ARect, hcColsTitres.Sections.Items[1], true , Format('%d', [V.Secteur]));
    DrawColTexte(lsbViseesAntennes, ARect, hcColsTitres.Sections.Items[2], true , v.toString());
    DrawColTexte(lsbViseesAntennes, ARect, hcColsTitres.Sections.Items[3], true , FormatterNombreOOo(V.Longueur, 3));
    DrawColTexte(lsbViseesAntennes, ARect, hcColsTitres.Sections.Items[4], true , FormatterNombreOOo(V.Azimut, 3));
    DrawColTexte(lsbViseesAntennes, ARect, hcColsTitres.Sections.Items[5], true , FormatterNombreOOo(V.Pente, 3));
  end;
begin
  WU  := FListeIdxAntennesRetenues.GetElement(Index);
  V   := FMyDocTopo.GetViseeAntenne(WU);
  BGC := IIF(Odd(V.PtDepart), clWhite, clYellow);
  if (odSelected in state) then DessineItem(clBlue, BGC) else DessineItem(BGC, clBlack);
end;

procedure TCdrListeViseesEnAntenne.sclStationsOfSerieChange(Sender: TObject);
var
  MySerie: TObjSerie;
begin
  MySerie := FMyDocTopo.GetSerie(FInternalIdxSerie);
  ListerLesAntennesOfSerie(MySerie, sclStationsOfSerie.Position);
  lbNbStationsOfSerie.caption := format('%d/%d', [sclStationsOfSerie.Position, MySerie.GetNbVisees() - 1]);
end;
end.

