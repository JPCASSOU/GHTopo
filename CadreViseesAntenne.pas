unit CadreViseesAntenne;
// Visées en antenne
// Date: 19/08/2012
// Statut: Opérationnel
// 04/10/2013: Améliorations dans la grille
// 06/02/2014: Remplacement de l'évaluateur d'expressions
// 14/02/2014: Implémentation du champ secteur
// 29/10/2014: Mise à 3 décimales des valeurs L, Az, P (DistoX)
// 26/09/2017: Suppression de la grille et de sa gestion: elle était
//             inutilisée, mobilisait beaucoup de mémoire
//             et s'est avérée la cause de plantages
// 27/09/2017: Incorporé dans un dialogue modal (n'est plus une fenêtre permanente)

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  evaluateur_expressions,
  StructuresDonnees,
  ToporobotClasses2012, UnitObjetSerie, Dialogs,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, StdCtrls, Graphics,
  ActnList, Menus, Buttons, LCLType, ExtCtrls, ComCtrls;

type

  { TCdrAntennes }

  TCdrAntennes = class(TFrame)
    acSauvegrd: TAction;
    acSelectCode: TAction;
    acSelectExpe: TAction;
    acSelectReseau: TAction;
    acSelectSecteur: TAction;
    acArmerDistoX: TAction;
    acRefreshListeAntennes: TAction;
    acNettoyerAntennes: TAction;
    acOuvrirFichierTexte: TAction;
    acChangerPointAccrochageAntennes: TAction;
    acSortBySerieSt: TAction;
    acAttribCodesExpes: TAction;
    acSimplifyAntennesOfAll: TAction;
    acSimplifyAntennesOfSerie: TAction;
    acSimplifyAntennesOfStation: TAction;
    acViderListeAntennes: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    Button1: TButton;
    CdrAntennesActionList: TActionList;
    CdrAntennesImagesList: TImageList;
    editLongueurMaxAntenne: TCurrencyEdit;
    editSimplyfySerieNoSerie: TCurrencyEdit;
    editSimplificationTolerance: TCurrencyEdit;
    editSimplyfySerieNoStation: TCurrencyEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbLengthMaxAntenne: TLabel;
    lbSimplifyTolerance: TLabel;
    lbEtapeTraitement: TLabel;
    lbStatutReattribAntennes: TStaticText;
    lbStatutNettoyageAntennes: TStaticText;
    lbStatutDestructionAntennes: TStaticText;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    lbStatutTriAntennes: TStaticText;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    procedure acAttribCodesExpesExecute(Sender: TObject);
    procedure acChangerPointAccrochageAntennesExecute(Sender: TObject);
    procedure acFindStationDepartExecute(Sender: TObject);
    procedure acNettoyerAntennesExecute(Sender: TObject);
    procedure acSimplifyAntennesOfSerieExecute(Sender: TObject);
    procedure acSimplifyAntennesOfStationExecute(Sender: TObject);
    procedure acSimplifyAntennesOfAllExecute(Sender: TObject);
    procedure acSortBySerieStExecute(Sender: TObject);
    procedure acViderListeAntennesExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    FMyDocToporobot: TToporobotStructure2012;
    procedure InitCaptions();
    procedure SetVoyantStatut(const LB: TStaticText; const DoneOK: boolean; const MsgOK: string; const MsgKO: string = 'Echec');
    procedure DisplayProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
  public
    { public declarations }
    function Initialise(const DT: TToporobotStructure2012) : boolean;
  end;

implementation
uses
  CallDialogsStdVersion;


{$R *.lfm}

function TCdrAntennes.Initialise(const DT: TToporobotStructure2012) : boolean;
begin
  Result := False;
  try
    FMyDocToporobot := DT;
    InitCaptions();
    GroupBox1.Caption := format(GetResourceString(rsCDR_ANTENNES_LB_NB_ANTENNES), [FMyDocToporobot.GetNbAntennes()]);
    Result := True;
  except

  end;
end;



procedure TCdrAntennes.SetVoyantStatut(const LB: TStaticText;
                                       const DoneOK: boolean;
                                       const MsgOK: string;
                                       const MsgKO: string = 'Echec');
begin
  LB.Color := IIF(DoneOK, clGreen, clRed);
  LB.Caption := IIF(DoneOK, MsgOK, MsgKO);
end;










procedure TCdrAntennes.acNettoyerAntennesExecute(Sender: TObject);
var
  WU: Integer;
  BF: TProcDisplayProgression;
begin
  acNettoyerAntennes.Enabled := false;
  try
    if (GHTopoQuestionOuiNon(GetResourceString(rsCDR_ANTENNES_CONFIRM_NETTOYAGE))) then
    begin
      BF := FMyDocToporobot.GetProcDisplayProgression();
      FMyDocToporobot.SetProcDisplayProgression(DisplayProgression);
      WU  := FMyDocToporobot.NettoyerAntennes(editLongueurMaxAntenne.Value, -1, -1);
      SetVoyantStatut(lbStatutNettoyageAntennes, WU >= 0, Format('%d antennes supprimées', [WU]));
    end;
  finally
    acNettoyerAntennes.Enabled := True;
    FMyDocToporobot.SetProcDisplayProgression(BF);
  end;
end;

procedure TCdrAntennes.acSimplifyAntennesOfSerieExecute(Sender: TObject);
var
  BF: TProcDisplayProgression;
  MySerie: TObjSerie;
  QIdx, EWE: integer;
begin
  try
    acSimplifyAntennesOfSerie.Enabled := false;
    BF := FMyDocToporobot.GetProcDisplayProgression();
    FMyDocToporobot.SetProcDisplayProgression(nil);
    if (FMyDocToporobot.GetSerieByNumeroSerie(editSimplyfySerieNoSerie.AsInteger, MySerie, QIdx)) then
    begin
      if (GHTopoQuestionOuiNon(GetResourceString(rsCDR_ANTENNES_CONFIRM_SIMPLIFICATION))) then
      begin
        FMyDocToporobot.ResetMarkersToDelete();
        FMyDocToporobot.SimplifyAntennesOfSerie(MySerie, editSimplificationTolerance.Value);
        EWE := FMyDocToporobot.PurgerTableAntennes();
        ShowMessage(Format('%d antennes removed', [EWE]));
      end;
    end
    else
      ShowMessage('Série introuvable');
  finally
    acSimplifyAntennesOfSerie.Enabled := true;
    FMyDocToporobot.SetProcDisplayProgression(BF);
  end;
end;

procedure TCdrAntennes.acSimplifyAntennesOfStationExecute(Sender: TObject);
var
  BF: TProcDisplayProgression;
  MySerie: TObjSerie;
  QIdx, EWE: integer;
begin
  try
    acSimplifyAntennesOfStation.Enabled := false;
    BF := FMyDocToporobot.GetProcDisplayProgression();
    FMyDocToporobot.SetProcDisplayProgression(nil);

    if (FMyDocToporobot.GetSerieByNumeroSerie(editSimplyfySerieNoSerie.AsInteger, MySerie, QIdx)) then
    begin
      FMyDocToporobot.ResetMarkersToDelete();
      FMyDocToporobot.SimplifyAntennesOfStation(MySerie.GetNumeroDeSerie(), editSimplyfySerieNoStation.AsInteger, editSimplificationTolerance.Value);
      EWE := FMyDocToporobot.PurgerTableAntennes();
      ShowMessage(Format('%d antennes removed', [EWE]));
    end
    else
      ShowMessage('Série introuvable');

  finally
    acSimplifyAntennesOfStation.Enabled := true;
    FMyDocToporobot.SetProcDisplayProgression(BF);
  end;
end;

procedure TCdrAntennes.acSimplifyAntennesOfAllExecute(Sender: TObject);
var
  BF: TProcDisplayProgression;
begin
  try
    acSimplifyAntennesOfAll.Enabled := false;
    if (GHTopoQuestionOuiNon(GetResourceString(rsCDR_ANTENNES_CONFIRM_SIMPLIFICATION))) then
    begin
      BF := FMyDocToporobot.GetProcDisplayProgression();
      FMyDocToporobot.SetProcDisplayProgression(DisplayProgression);
      FMyDocToporobot.SimplifyAntennesOfAllReseau(editSimplificationTolerance.Value);
    end;
  finally
    acSimplifyAntennesOfAll.Enabled := true;
    FMyDocToporobot.SetProcDisplayProgression(BF);
  end;
end;





procedure TCdrAntennes.acSortBySerieStExecute(Sender: TObject);
var
  WU: integer;
begin
  WU := FMyDocToporobot.SortAntennesBySerieStation();
  SetVoyantStatut(lbStatutTriAntennes, WU >= 0, Format('%d antennes triées', [WU]));
end;

procedure TCdrAntennes.acViderListeAntennesExecute(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Supprimer TOUTES les visées en antenne')) then
  begin
    FMyDocToporobot.ViderListeViseesAntennes;
  end;
end;

procedure TCdrAntennes.Button1Click(Sender: TObject);
var
  MySerie: TObjSerie;
  n: Integer;
begin
  n := 1;
  if (SelectionDansListe(FMyDocToporobot, mslSERIE, True, n)) then
  begin
    MySerie := FMyDocToporobot.GetSerie(n);
    editSimplyfySerieNoSerie.AsInteger := MySerie.GetNumeroDeSerie();
  end;
end;

procedure TCdrAntennes.DisplayProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
begin
  lbEtapeTraitement.Caption := Format('%s: %d of %d', [Etape, Done, Ending]);
  ProgressBar1.Min      := Starting;
  ProgressBar1.Max      := Ending;
  ProgressBar1.Position := Done;
  GroupBox1.Caption  := Format(GetResourceString(rsCDR_ANTENNES_LB_NB_ANTENNES), [FMyDocToporobot.GetNbAntennes()]);
  Application.ProcessMessages;
end;


procedure TCdrAntennes.acAttribCodesExpesExecute(Sender: TObject);
var
  WU: Boolean;
begin
  WU := FMyDocToporobot.ReattribuerCodesExpesViseesAntennes();
  SetVoyantStatut(lbStatutReattribAntennes, WU, 'Réattribution effectuée');
end;

procedure TCdrAntennes.acChangerPointAccrochageAntennesExecute(Sender: TObject);
var
  MyAntenne: TViseeAntenne;
  EWE: String;
  WU: String;
  QSer: integer;
  QPt: integer;
  Nb: Integer;
  QDoMatchExact: boolean;
begin
  (*
  QDoMatchExact := false;
  if (grdViseesEnAntenne.Row < 1) then Exit;
  MyAntenne := FMyDocToporobot.GetViseeAntenne(grdViseesEnAntenne.Row);
  EWE := Format('Deplacer Antenne: %d.%d - %.2f, %.2f, %.2f', [MyAntenne.SerieDepart, MyAntenne.PtDepart, MyAntenne.Longueur, MyAntenne.Azimut,  MyAntenne.Pente]);
  WU  := MyAntenne.toString();
  if (InputQuery('Changement de point de base', 'Nouveau point de base (couple serie et station ss.pp)', WU)) then
  begin
    if (FMyDocToporobot.FindPtTopoByCle(QDoMatchExact, WU, QSer, QPt)) then
    begin
      Nb := FMyDocToporobot.DeplacerOrigineViseesRayonnantes(MyAntenne.SerieDepart, MyAntenne.PtDepart, QSer, QPt);
      ShowMessageFmt('%d antennes deplacees', [Nb]);
      RemplirTableau;
    end
    else
      ShowMessageFmt('Point de base %s introuvable', [WU]);
  end;
  //*)
end;

procedure TCdrAntennes.acFindStationDepartExecute(Sender: TObject);
begin

end;

procedure TCdrAntennes.InitCaptions;
  procedure EWE(const WU: TAction; const S: string);
  begin
    WU.Caption  := GetResourceString(S);
    WU.Hint     := WU.Caption;
  end;
begin

  EWE(acOuvrirFichierTexte        , rsCDR_ANTENNES_AC_IMPORT_CSV);
  EWE(acSauvegrd                  , rsCDR_ANTENNES_AC_SAVEGRD);
  EWE(acAttribCodesExpes          , rsCDR_ANTENNES_ATTRIB_CODES_EXPES);
  EWE(acNettoyerAntennes          , rsCDR_ANTENNES_NETTOYER_ANTENNES);
  EWE(acRefreshListeAntennes      , rsCDR_ANTENNES_REFRESH_LISTE);
  EWE(acSelectReseau              , rsCDR_ANTENNES_SELECT_RESEAU);
  EWE(acSelectCode                , rsCDR_ANTENNES_SELECT_CODE);
  EWE(acSelectExpe                , rsCDR_ANTENNES_SELECT_EXPE);
  EWE(acSelectSecteur             , rsCDR_ANTENNES_SELECT_SECTEUR);
  EWE(acViderListeAntennes        , rsCDR_ANTENNES_VIDER_LISTE_ANTENNES);
  EWE(acSimplifyAntennesOfAll     , rsCDR_ANTENNES_SIMPLIFY_FOR_ALL_RESEAU);
  EWE(acSimplifyAntennesOfSerie   , rsCDR_ANTENNES_SIMPLIFY_FOR_SERIE);
  EWE(acSimplifyAntennesOfStation , rsCDR_ANTENNES_SIMPLIFY_FOR_STATION);
  lbLengthMaxAntenne.Caption := GetResourceString(rsCDR_ANTENNES_LB_MAX_LENGTH_ANTENNE);
end;


end.

