unit CadreListesSimples;
// TODO: Remplacer cmbListeSelectionnee par un ensemble de boutons
// [Entrees][Codes][Séances]

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  math,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitListesSimplesWithGeneriques,
  UnitClasseMaillage,
  Graphics,
  CallDialogsStdVersion,
  unitUtilsComposants,
  UnitObjetSerie,
  CadreRadioButtons,
  UnitClassPalette,
  Classes, SysUtils, FileUtil, curredit, Forms, Dialogs, Controls, ExtCtrls, ComCtrls, StdCtrls, Buttons, ActnList, Types, LCLType;

type

{ TCdrListesSimples }

 TCdrListesSimples = class(TFrame)
    acAddItem: TAction;
    acDeleteItem: TAction;
    acEditItem: TAction;
    acExportListesCSV: TAction;
    acFind: TAction;
    acGotoByNumero: TAction;
    acHelpListesSimples: TAction;
    acnlstProjectManager: TActionList;
    acSort: TAction;
    CdrRadioButtons1: TCdrRadioButtons;
    editRayonCaptureProxi: TCurrencyEdit;
    editSearch: TEdit;
    hcColsTitres: THeaderControl;
    imglstProjectManager: TImageList;
    lbRayonCaptureProxi: TLabel;
    lsbListe: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    SpeedButton1: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    lbNbreElements: TStaticText;
    procedure acAddItemExecute(Sender: TObject);
    procedure acDeleteItemExecute(Sender: TObject);
    procedure acEditItemExecute(Sender: TObject);
    procedure acExportListesCSVExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acGotoByNumeroExecute(Sender: TObject);
    procedure acHelpListesSimplesExecute(Sender: TObject);
    procedure acSortExecute(Sender: TObject);
    procedure editRayonCaptureProxiKeyPress(Sender: TObject; var Key: char);
    procedure editSearchKeyPress(Sender: TObject; var Key: char);

    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbListeDblClick(Sender: TObject);
    procedure lsbListeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbListeSelectionChange(Sender: TObject; User: boolean);
  strict private
    procedure ListerLesElements(const QModeBDD: TModeBDD; const QIdx: integer);
    function QGetCorrectIndex(const QIdx, NbItems: integer): integer;
  private
    { private declarations }
    FModeBDD              : TModeBDD;
    FDocumentToporobot    : TToporobotStructure2012;
    FBDDEntites           : TBDDEntites;
    FMaillage             : TMaillage;
    FListeDesProximites   : TListeStationProcheEndSerie;
    FListeSeriesEncadrees : TListeSeriesEncadrees;
    FModeTriProximites    : byte;

    // Callbacks pour la liste des proximités
    FProcOfObjectWithTBaseStation: TProcOfObjectWithBaseStation;
    FProcOfObjectWithXY          : TProcOfObjectWithXY;
    procedure InitCaptions();
    procedure QProcAfterSelectRBTN(const P: integer);
    procedure InitHeaderListesSimples();
    // liste des proximités (stations proches de l'extrémité d'une série)
    procedure AddProximite(const P: TStationProcheOfEndSerie);
    function  GetNbProximites(): integer;
    function  GetProximite(const Idx: integer): TStationProcheOfEndSerie;
    procedure PutProximite(const Idx: integer; const P: TStationProcheOfEndSerie);

    procedure RechercherProximites(const QRayonCapture: double);

    // liste des séries encadrées (dont le numéro de série de la station d'arrivée est différent de celui de la série)
    procedure AddSerieEncadree(const P: TSerieEncadree);
    function  GetNbSeriesEncadrees(): integer;
    function  GetSerieEncadree(const Idx: integer): TSerieEncadree;
    procedure RechercherSeriesEncadrees();
    procedure GotoError(const Err: TMessaqeErreurGHTopoCompiler);
  public
    { public declarations }
    function  InitialiserListeSimple(const FD: TToporobotStructure2012;
                                    const FE: TBDDEntites;
                                    const FM: TMaillage;
                                    const M: TModeBDD): boolean;
    procedure Finaliser();
    procedure SetPtrBDDEntites(const B : TBDDEntites);
    procedure SetProcOfObjectWithBaseStation(const P: TProcOfObjectWithBaseStation);
    procedure SetProcOfObjectWithXY(const P: TProcOfObjectWithXY);
    procedure ListerListesSimples(const QModeBDD: TModeBDD = mbddCHECK_ERRORS);
  end;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TCdrListesSimples }
function TCdrListesSimples.InitialiserListeSimple(const FD: TToporobotStructure2012;
                                                  const FE: TBDDEntites;
                                                  const FM: TMaillage;
                                                  const M : TModeBDD): boolean;
begin
  Result := false;
  self.ParentFont                  := True;
  self.CdrRadioButtons1.ParentFont := self.ParentFont;
  self.hcColsTitres.ParentFont     := self.ParentFont;
  self.lsbListe.ParentFont         := self.ParentFont;

  FModeTriProximites    := 0;
  FProcOfObjectWithTBaseStation := nil;
  FProcOfObjectWithXY           := nil;
  FListeDesProximites   := TListeStationProcheEndSerie.Create;
  FListeSeriesEncadrees := TListeSeriesEncadrees.Create;
  lsbListe.ItemHeight   := 22;
  editRayonCaptureProxi.Value := 2.01;
  ProgressBar1.Visible  := false;
  try
    FListeDesProximites.ClearListe();
    FListeSeriesEncadrees.ClearListe();
    FModeBDD           := M;
    FDocumentToporobot := FD;
    FBDDEntites        := FE;
    FMaillage          := FM;
    ListerListesSimples(FModeBDD);
    InitCaptions();
  except
  end;
end;


procedure TCdrListesSimples.Finaliser();
begin
  try
    FListeDesProximites.ClearListe();
    FListeSeriesEncadrees.ClearListe();
  finally
    FreeAndNil(FListeDesProximites);
    FreeAndNil(FListeSeriesEncadrees);
  end;
end;

procedure TCdrListesSimples.SetPtrBDDEntites(const B: TBDDEntites);
begin
  FBDDEntites := B;
  ListerLesElements(mbddNOEUDS, 0);
end;

procedure TCdrListesSimples.SetProcOfObjectWithBaseStation(const P: TProcOfObjectWithBaseStation);
begin
  FProcOfObjectWithTBaseStation := P;

end;

procedure TCdrListesSimples.SetProcOfObjectWithXY(const P: TProcOfObjectWithXY);
begin
  FProcOfObjectWithXY := P;
end;

procedure TCdrListesSimples.acAddItemExecute(Sender: TObject);
var
  n : Integer;
  EE: TEntrance;
  RS: TReseau;
  SS: TSecteur;
  CC: TCode;
  EX: TExpe;
  EWE: String;
begin
  case FModeBDD of
    mbddENTRANCES     : EWE := GetResourceString(rsDLG_BDD_ADD_ENTREE);
    mbddRESEAUX       : EWE := GetResourceString(rsDLG_BDD_ADD_RESEAU);
    mbddSECTEURS      : EWE := GetResourceString(rsDLG_BDD_ADD_SECTEUR);
    mbddCODES         : EWE := GetResourceString(rsDLG_BDD_ADD_CODE);
    mbddEXPES         : EWE := GetResourceString(rsDLG_BDD_ADD_EXPE);
    mbddPOI           : exit; // les POI sont reconstruits depuis les commentaires des stations
    mbddCHECK_ERRORS  : exit; // cette liste est reconstruite lors du check de la base
    mbddPROXIMITES    : EWE := GetResourceString(rsDLG_BDD_ADD_BOUCLAGE);
    mbddSERIES_ENCADREES: exit;
  else
    pass;
  end;

  n := lsbListe.Count - 1;
  case FModeBDD of
    mbddENTRANCES: // entrées
      begin
        if (not GHTopoQuestionOuiNon(EWE)) then Exit;
        EE := MakeTEntrance('Nouvelle entrée', '', 0.00, 0.00, 0.00, 0, 0, '');
        //ShowMessage(BoolToStr(assigned(FBDDEntites), 'FBDDEntites transmis', 'FBDDEntites KO'));
        if (EditerEntrance(FDocumentToporobot, n, FBDDEntites, FMaillage, EE)) then
        begin
          FDocumentToporobot.AddEntrance(EE);
          n := FDocumentToporobot.GetNbEntrances();
          ListerLesElements(FModeBDD, n - 1);
        end;
      end;
    mbddRESEAUX  : // réseaux
      begin
        if (not GHTopoQuestionOuiNon(EWE)) then Exit;
        RS := MakeTReseau(clBlue, 0, 'Nouveau réseau', '');
        if (EditerReseau(FDocumentToporobot, n, RS)) then
        begin
          FDocumentToporobot.AddReseau(RS);
          n := FDocumentToporobot.GetNbReseaux();
          ListerLesElements(FModeBDD, n - 1);
        end;
      end;
    mbddSECTEURS : // secteurs;
      begin
        if (not GHTopoQuestionOuiNon(EWE)) then Exit;
        SS := MakeTSecteur(clRed, 'Nouveau secteur');
        if (EditerSecteur(FDocumentToporobot, n, SS)) then
        begin
          FDocumentToporobot.AddSecteur(SS);
          n := FDocumentToporobot.GetNbSecteurs();
          ListerLesElements(FModeBDD, n - 1);
        end;
      end;
    mbddCODES: // codes
      begin
        if (not GHTopoQuestionOuiNon(EWE)) then Exit;
        // attraper le dernier code
        n := FDocumentToporobot.GetNbCodes();
        CC := FDocumentToporobot.GetCode(n - 1);
        CC.IDCode := 1 + FDocumentToporobot.getMaxIdxCode();
        if (EditerCode(FDocumentToporobot, n, CC)) then
        begin
          FDocumentToporobot.AddCode(CC);
          n := FDocumentToporobot.GetNbCodes();
          ListerLesElements(FModeBDD, n - 1);
        end;
      end;
    mbddEXPES: // expés
      begin
        if (not GHTopoQuestionOuiNon(EWE)) then Exit;
        n := FDocumentToporobot.GetNbExpes();
        EX := FDocumentToporobot.GetExpe(n - 1);
        EX.IDExpe := 1 + FDocumentToporobot.getMaxIdxExpe();
        if (EditerExpe(FDocumentToporobot, n, EX)) then
        begin
          FDocumentToporobot.AddExpe(EX);
          n := FDocumentToporobot.GetNbExpes();
          ListerLesElements(FModeBDD, n - 1);
        end;
      end;
    mbddCHECK_ERRORS: // messages d'erreur
      begin
        pass;
      end;
    mbddPROXIMITES:
      begin
        pass;
      end;
    mbddSERIES_ENCADREES: // séries encadrées: on ne fait rien
      begin
        pass;
      end;
  else
    pass;
  end;
end;

procedure TCdrListesSimples.acDeleteItemExecute(Sender: TObject);
var
  QIdx: integer;
begin
  QIdx := lsbListe.ItemIndex;
  if (QIdx < 1) then Exit;
  if (GHTopoQuestionOuiNon(Format(GetResourceString(rsMSG_WARN_DELETE_ITEM), [QIdx]))) then
  begin
    case FModeBDD of
      mbddENTRANCES    : if (FDocumentToporobot.RemoveEntrance(QIdx))  then ListerLesElements(FModeBDD, 0);
      mbddRESEAUX      : if (FDocumentToporobot.RemoveReseau(QIdx))    then ListerLesElements(FModeBDD, 0);
      mbddSECTEURS     : if (FDocumentToporobot.RemoveSecteur(QIdx))   then ListerLesElements(FModeBDD, 0);
      mbddCODES        : if (FDocumentToporobot.RemoveCode(QIdx))      then ListerLesElements(FModeBDD, 0);
      mbddEXPES        : if (FDocumentToporobot.RemoveExpe(QIdx))      then ListerLesElements(FModeBDD, 0);
      mbddPROXIMITES   : // supprimer un bouclage
        begin
          pass;
        end;
      mbddSERIES_ENCADREES: // libérer un bouclage (séries encadrées)
        begin
          pass;
        end;
      mbddPOI:
        begin
          if (FDocumentToporobot.RemovePointOfInterest(QIdx)) then ListerLesElements(FModeBDD, 0);
        end;
    else
      pass;
    end;
  end;
end;

procedure TCdrListesSimples.acEditItemExecute(Sender: TObject);
var
  n, QIdx: Integer;
  RS: TReseau;
  EE: TEntrance;
  SS: TSecteur;
  CC: TCode;
  EX: TExpe;
  QDoAction: Boolean;
  MyProximite: TStationProcheOfEndSerie;
  MySerie: TObjSerie;
  QAT: String;
  MyVisee: TUneVisee;
  POI: TPointOfInterest;
begin
  n := lsbListe.ItemIndex;
  if (n < 0) then Exit;
  case FModeBDD of
    mbddENTRANCES: // entrées
      begin
        EE := FDocumentToporobot.GetEntrance(n);
        if (EditerEntrance(FDocumentToporobot, n, FBDDEntites, FMaillage, EE)) then
        begin
          FDocumentToporobot.PutEntrance(n, EE);
          ListerLesElements(FModeBDD, n);
        end;
      end;
    mbddRESEAUX  : // réseaux
      begin
        RS := FDocumentToporobot.GetReseau(n);
        if (EditerReseau(FDocumentToporobot, n, RS)) then
        begin
          FDocumentToporobot.PutReseau(n, RS);
          ListerLesElements(FModeBDD, n);
        end;
      end;
    mbddSECTEURS : // secteurs;
      begin
        SS := FDocumentToporobot.GetSecteur(n);
        if (EditerSecteur(FDocumentToporobot, n, SS)) then
        begin
          FDocumentToporobot.PutSecteur(n, SS);
          ListerLesElements(FModeBDD, n);
        end;
      end;
    mbddCODES: // codes
      begin
        if (n < 1) then Exit; // expé et code 0 ne sont pas modifiables
        CC := FDocumentToporobot.GetCode(n);
        if (EditerCode(FDocumentToporobot, n, CC)) then
        begin
          FDocumentToporobot.PutCode(n, CC);
          ListerLesElements(FModeBDD, n);
        end;
      end;
    mbddEXPES: // expés
      begin
        if (n < 1) then Exit; // expé et code 0 ne sont pas modifiables
        EX := FDocumentToporobot.GetExpe(n);
        if (EditerExpe(FDocumentToporobot, n, EX)) then
        begin
          FDocumentToporobot.PutExpe(n, EX);
          ListerLesElements(FModeBDD, n);
        end;
      end;
    mbddPOI:
      begin
        if (n < 1) then exit;
        POI := FDocumentToporobot.GetPointOfInterest(n);
        if (DisplayEditerPOI(FDocumentToporobot, POI)) then
        begin
          FDocumentToporobot.PutPointOfInterest(n, POI);
          ListerLesElements(FModeBDD, n);
        end;
      end;
    mbddCHECK_ERRORS:
     begin
       if (n >= 0) then GotoError(FDocumentToporobot.GetMessageErreur(n));
     end;
    mbddPROXIMITES: // création d'un bouclage
      begin
        QDoAction := True;
        n := lsbListe.ItemIndex;
        if (n >= 0) then
        begin
          MyProximite := GetProximite(n);
          if (FDocumentToporobot.GetSerieByNumeroSerie(MyProximite.BaseStation.aSerie, MySerie, QIdx)) then
          begin
            QAT := Format('Accrocher l''extrémité de la série %d - "%s"' + #13#10 + 'à la station %d.%d',
                          [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie(),
                           MyProximite.NearestStation.aSerie, MyProximite.NearestStation.aStation]);
            if (GHTopoQuestionOuiNonWithOptionalAction('Mise en place d''un bouclage', QAT, 'Créer une visée de raccordement', QDoAction)) then
            begin
              if (QDoAction) then  // d'abord, on ajoute une visée si demandé (l'ajout est conseillé pour éviter de désorganiser le dessin)
              begin
                MyVisee              := MySerie.GetLastVisee(); // on récupère la dernière visée
                MyVisee.Longueur     := 0.01;                   // dont on réutilise les Az, P, LRUD
                MyVisee.Commentaires := Format('Linked to %d.%d - ', [MyProximite.NearestStation.aSerie, MyProximite.NearestStation.aStation]);
                MySerie.AddVisee(MyVisee);  // MySerie est un pointeur ... donc SetSeriePtArr() modifie la bdd mémoire.
              end;
              MySerie.SetSeriePtArr(MyProximite.NearestStation.aSerie, MyProximite.NearestStation.aStation);
              MyProximite.Statut := Format('Linked to %d.%d', [MyProximite.NearestStation.aSerie, MyProximite.NearestStation.aStation]);
              PutProximite(n, MyProximite);
              lsbListe.Invalidate;
            end;
          end;
        end;
      end;
  else
    pass;
  end;
end;

procedure TCdrListesSimples.acExportListesCSVExecute(Sender: TObject);
var
  WU: TStringDirectoryFilename;
  EWE: String;
  QFilterIndex: integer;
begin
  case FModeBDD of
    mbddENTRANCES    : EWE := 'Entrees';
    mbddRESEAUX      : EWE := 'Reseaux';
    mbddSECTEURS     : EWE := 'Secteurs';
    mbddCODES        : EWE := 'Codes';
    mbddEXPES        : EWE := 'Expes';
    mbddPOI          : EWE := 'Points_Of_Interest';
    mbddCHECK_ERRORS : EWE := 'Database_Check_Error_Messages';
  end;
  WU := Format('Liste001_%s.csv', [EWE]);
  if (DoDialogSaveFile('CSV file (*.csv)|*.csv|', '.csv', WU, QFilterIndex)) then
  begin
    case FModeBDD of
      mbddENTRANCES     : FDocumentToporobot.ExportListeEntreesCSV(WU);
      mbddRESEAUX       : FDocumentToporobot.ExportListeReseauxCSV(WU);
      mbddSECTEURS      : FDocumentToporobot.ExportListeSecteursCSV(WU);
      mbddCODES         : FDocumentToporobot.ExportListeCodesCSV(WU);
      mbddEXPES         : FDocumentToporobot.ExportListeExpesCSV(WU);
      mbddPOI           : FDocumentToporobot.ExportListePOIToCSV(WU);
      mbddCHECK_ERRORS  : FDocumentToporobot.ExportListeErreursToCSV(WU);
      mbddNOEUDS        : FBDDEntites.ExporterListeJonctions(WU);
    end;
  end;
end;

procedure TCdrListesSimples.acFindExecute(Sender: TObject);
var
  QIdx: integer;
  EWE: string;

begin
  EWE := trim(editSearch.Text);
  case FModeBDD of
    mbddDISABLED         : exit;
    mbddENTRANCES        : QIdx := FDocumentToporobot.FindIdxEntranceByText(EWE);
    mbddRESEAUX          : QIdx := FDocumentToporobot.FindIdxReseauByText(EWE);
    mbddSECTEURS         : QIdx := FDocumentToporobot.FindIdxSecteurByText(EWE);
    mbddCODES            : QIdx := FDocumentToporobot.FindIdxCodeByText(EWE);
    mbddEXPES            : QIdx := FDocumentToporobot.FindIdxExpeByText(EWE);
    mbddPOI              : exit;
    mbddCHECK_ERRORS     : exit;
    mbddNOEUDS           : exit;
    mbddNAMESPACES       : exit;
    mbddPROXIMITES       : exit;
    mbddSERIES_ENCADREES : exit;
  end;
  if (QIdx >= 0) then lsbListe.ItemIndex := QIdx
                 else ShowMessage(GetResourceString(rsMATCHNOTFOUND));
end;

procedure TCdrListesSimples.acGotoByNumeroExecute(Sender: TObject);
var
  zdar: String;
  miou: String;
  EWE, WU: Integer;
begin
  miou := '';
  case FModeBDD of
    mbddENTRANCES    : zdar := rsDLG_FIND_ENTRANCE_BY_TEXT;
    mbddRESEAUX      : zdar := rsDLG_FIND_RESEAU_BY_TEXT;
    mbddSECTEURS     : zdar := rsDLG_FIND_SECTEUR_BY_TEXT;
    mbddCODES        : zdar := rsDLG_FIND_CODE_BY_TEXT;
    mbddEXPES        : zdar := rsDLG_FIND_EXPE_BY_TEXT;
    mbddPOI          : Exit;
    mbddCHECK_ERRORS : Exit;
  end;
  if (not GHTopoInputQuery(GetResourceString(zdar),
                           GetResourceString(rsDLG_FIND_PROMPT),
                           miou)) then Exit;
  EWE := StrToIntDef(miou, 0);
  case FModeBDD of
    mbddENTRANCES: WU := FDocumentToporobot.GetIdxEntreeByNumero(EWE);
    mbddRESEAUX  : WU := FDocumentToporobot.GetIdxReseauByNumero(EWE);
    mbddSECTEURS : WU := FDocumentToporobot.GetIdxSecteurByNumero(EWE);
    mbddCODES    : WU := FDocumentToporobot.GetIdxCodeByNumero(EWE);
    mbddEXPES    : WU := FDocumentToporobot.GetIdxExpeByNumero(EWE);
  else
    pass;
  end;
  if (WU = -1) then
  begin
    ShowMessage(GetResourceString(rsMATCHNOTFOUND));
    Exit;
  end;
end;

procedure TCdrListesSimples.acHelpListesSimplesExecute(Sender: TObject);
begin
  DisplayHelpSystem('LISTES_SIMPLES');
end;

procedure TCdrListesSimples.acSortExecute(Sender: TObject);
begin
  case FModeBDD of
    mbddENTRANCES: pass;
    mbddRESEAUX  : pass;
    mbddSECTEURS : pass;
    mbddCODES:
      begin
        FDocumentToporobot.SortCodes;
        ListerLesElements(FModeBDD, 0);
      end;
    mbddEXPES:
      begin
        FDocumentToporobot.SortExpes;
        ListerLesElements(FModeBDD, 0);
      end;
    mbddPOI            : pass;
    mbddCHECK_ERRORS   :
     begin
       if (GHTopoQuestionOuiNon('Vérifier à nouveau les données')) then
       begin
         FDocumentToporobot.CheckerLesDonneesTopo();
         ListerLesElements(FModeBDD, 0);
       end;
     end;
    mbddPROXIMITES     :
     begin
       FListeDesProximites.Trier(FModeTriProximites);
       ListerLesElements(FModeBDD, 0);
       FModeTriProximites  := (FModeTriProximites + 1) MOD 2;
     end;
    mbddSERIES_ENCADREES:
     begin
       pass;
     end;
  else
    pass;
  end;
end;

procedure TCdrListesSimples.editRayonCaptureProxiKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) then
  begin
    if (GHTopoQuestionOuiNon('Rechercher à nouveau les proximités')) then self.RechercherProximites(editRayonCaptureProxi.Value);
  end;
end;

procedure TCdrListesSimples.editSearchKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) then acFindExecute(self);
end;

procedure TCdrListesSimples.hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbListe.Invalidate;
end;

procedure TCdrListesSimples.QProcAfterSelectRBTN(const P: integer);
begin
  {$IFDEF GROS_MINET}
  case P of
    0: ListerListesSimples(mbddENTRANCES);
    1: ListerListesSimples(mbddCODES);
    2: ListerListesSimples(mbddEXPES);
    3: ListerListesSimples(mbddCHECK_ERRORS);
    4: ListerListesSimples(mbddNOEUDS);
    5: ListerListesSimples(mbddPROXIMITES);
    6: ListerListesSimples(mbddSERIES_ENCADREES);
  end;
  {$ELSE}
  case P of
    0: ListerListesSimples(mbddENTRANCES);
    1: ListerListesSimples(mbddRESEAUX);
    2: ListerListesSimples(mbddSECTEURS);
    3: ListerListesSimples(mbddCODES);
    4: ListerListesSimples(mbddEXPES);
    5: ListerListesSimples(mbddPOI);
    6: ListerListesSimples(mbddCHECK_ERRORS);
    7: ListerListesSimples(mbddNOEUDS);
    8: ListerListesSimples(mbddPROXIMITES);
    9: ListerListesSimples(mbddSERIES_ENCADREES);
  else
    pass;
  end;
  {$ENDIF}

end;


procedure TCdrListesSimples.InitCaptions();
  procedure SetAcHint(const A: TAction; const H: string);
  begin
    A.Hint    := GetResourceString(H);
    A.Caption := A.Hint;
  end;
begin
    CdrRadioButtons1.Initialiser([GetResourceString(rsTBS_ENTRANCE),
                                 {$IFNDEF GROS_MINET}
                                  GetResourceString(rsTBS_RESEAUX),
                                  GetResourceString(rsTBS_SECTEURS),
                                 {$ENDIF}
                                  GetResourceString(rsTBS_CODES),
                                  GetResourceString(rsTBS_TRIPS),
                                 {$IFNDEF GROS_MINET}
                                 GetResourceString(rsTBS_POI_ABREGE),
                                 {$ENDIF}
                                  GetResourceString(rsTBS_CHECKUP_MESSAGES),
                                  GetResourceString(rsTBS_NOEUDS),
                                  GetResourceString(rsTBS_SERIES_ENCADREES)
                                 ], 0, QProcAfterSelectRBTN);
    SetAcHint(acExportListesCSV  , rsDLG_BDD_EXPORT_CSV);
    SetAcHint(acHelpListesSimples, rsDLG_BDD_HELP_LISTES);
end;
//***************** Gestion de la liste ****************************************
procedure TCdrListesSimples.ListerListesSimples(const QModeBDD: TModeBDD = mbddCHECK_ERRORS);
  procedure MiouMiou(const Miou: boolean; const A: TAction; const H: string);
  begin
    A.Hint    := GetResourceString(H);
    A.Caption := A.Hint;
    A.Enabled := Miou;
  end;
begin
  FModeBDD := QModeBDD;
  InitHeaderListesSimples();
  ListerLesElements(QModeBDD, 0);
  acAddItem.Enabled       := false;
  acDeleteItem.Enabled    := false;
  acEditItem.Enabled      := false;
  acFind.Enabled          := false;
  acSort.Enabled          := false;
  acGotoByNumero.Enabled  := false;
  case QModeBDD of
    mbddENTRANCES:
      begin
        MiouMiou(True, acAddItem          , rsDLG_BDD_ADD_ENTREE);
        MiouMiou(True, acDeleteItem       , rsDLG_BDD_REMOVE_ENTREE);
        MiouMiou(True, acEditItem         , rsDLG_BDD_APPLY_MODIFS);
        MiouMiou(True, acFind             , rsDLG_BDD_FIND);
        MiouMiou(True, acSort             , rsDLG_BDD_SORT);
        MiouMiou(True, acGotoByNumero     , rsDLG_BDD_GOTO_BY_NUMERO);
      end;
    mbddRESEAUX:
      begin
        MiouMiou(True, acAddItem          , rsDLG_BDD_ADD_RESEAU);
        MiouMiou(True, acDeleteItem       , rsDLG_BDD_REMOVE_RESEAU);
        MiouMiou(True, acEditItem         , rsDLG_BDD_APPLY_MODIFS);
        MiouMiou(True, acFind             , rsDLG_BDD_FIND);
        MiouMiou(True, acSort             , rsDLG_BDD_SORT);
        MiouMiou(True, acGotoByNumero     , rsDLG_BDD_GOTO_BY_NUMERO);
      end;
    mbddSECTEURS:
      begin
        MiouMiou(True, acAddItem          , rsDLG_BDD_ADD_SECTEUR);
        MiouMiou(True, acDeleteItem       , rsDLG_BDD_REMOVE_SECTEUR);
        MiouMiou(True, acEditItem         , rsDLG_BDD_APPLY_MODIFS);
        MiouMiou(True, acFind             , rsDLG_BDD_FIND);
        MiouMiou(True, acSort             , rsDLG_BDD_SORT);
        MiouMiou(True, acGotoByNumero     , rsDLG_BDD_GOTO_BY_NUMERO);
      end;
    mbddEXPES:
      begin
        MiouMiou(True, acAddItem          , rsDLG_BDD_ADD_EXPE);
        MiouMiou(True, acDeleteItem       , rsDLG_BDD_REMOVE_EXPE);
        MiouMiou(True, acEditItem         , rsDLG_BDD_APPLY_MODIFS);
        MiouMiou(True, acFind             , rsDLG_BDD_FIND);
        MiouMiou(True, acSort             , rsDLG_BDD_SORT);
        MiouMiou(True, acGotoByNumero     , rsDLG_BDD_GOTO_BY_NUMERO);
      end;
    mbddCODES:
      begin
        MiouMiou(True, acAddItem          , rsDLG_BDD_ADD_CODE);
        MiouMiou(True, acDeleteItem       , rsDLG_BDD_REMOVE_CODE);
        MiouMiou(True, acEditItem         , rsDLG_BDD_APPLY_MODIFS);
        MiouMiou(True, acFind             , rsDLG_BDD_FIND);
        MiouMiou(True, acSort             , rsDLG_BDD_SORT);
        MiouMiou(True, acGotoByNumero     , rsDLG_BDD_GOTO_BY_NUMERO);
      end;
    mbddPOI:
       begin
         MiouMiou(True, acEditItem        , rsDLG_BDD_POI_EDIT);
         MiouMiou(True, acDeleteItem      , rsDLG_BDD_POI_REMOVE);

       end;
    mbddCHECK_ERRORS: // On utilise le bouton de tri pour le recheck de la base
     begin
       MiouMiou(True , acEditItem         , rsDLG_BDD_GOTO_ERROR);
       MiouMiou(True , acSort             , rsDLG_BDD_RECHECK);
     end;
    mbddNOEUDS:
     begin
       MiouMiou(False, acAddItem, '');
     end;
    mbddPROXIMITES:
      begin
        MiouMiou(True, acEditItem, rsDLG_BDD_ADD_BOUCLAGE);
        MiouMiou(True, acSort   , rsDLG_BDD_SORT_BOUCLAGE);
      end;
    mbddSERIES_ENCADREES:
      begin
        MiouMiou(True, acDeleteItem, rsDLG_BDD_LIBERE_SERIE);
      end;
  else
    begin
      MiouMiou(False, acAddItem          , '');
      MiouMiou(False, acDeleteItem       , '');
      MiouMiou(false, acEditItem         , '');
      MiouMiou(false, acFind             , '');
      MiouMiou(false, acSort             , '');
      MiouMiou(false, acGotoByNumero     , '');
    end;
  end;
  // DONE: bouton de tri actif uniquement pour codes, expés et bouclages
  acSort.Enabled := (QModeBDD in [mbddCODES, mbddEXPES, mbddCHECK_ERRORS, mbddPROXIMITES]);
end;

procedure TCdrListesSimples.lsbListeDblClick(Sender: TObject);
begin
  acEditItem.Execute;
end;

function TCdrListesSimples.QGetCorrectIndex(const QIdx, NbItems: integer): integer;
var
  n: Integer;
begin
  result := 0;
  if (NbItems = 0) then exit(-1);
  n := NbItems - 1;
  if (QIdx < 0) then exit(0);
  if (QIdx > n) then exit(n);
  Result := QIdx;
end;

procedure TCdrListesSimples.ListerLesElements(const QModeBDD: TModeBDD; const QIdx: integer);

var
  i, Nb, MyIdx: Integer;
  EWE: String;
begin
  case QModeBDD of
    mbddDISABLED         : Nb := 0;
    mbddENTRANCES        : Nb := FDocumentToporobot.GetNbEntrances();
    mbddRESEAUX          : Nb := FDocumentToporobot.GetNbReseaux();
    mbddSECTEURS         : Nb := FDocumentToporobot.GetNbSecteurs();
    mbddCODES            : Nb := FDocumentToporobot.GetNbCodes();
    mbddEXPES            : Nb := FDocumentToporobot.GetNbExpes();
    mbddPOI              : Nb := FDocumentToporobot.GetNbPointsOfInterests();
    mbddCHECK_ERRORS     : Nb := FDocumentToporobot.GetNbMessagesErreur();
    mbddNOEUDS           : Nb := FBDDEntites.GetNbJonctions();
    mbddNAMESPACES       : Nb := FDocumentToporobot.GetNbNameSpaces();
    mbddPROXIMITES       : Nb := self.GetNbProximites();
    mbddSERIES_ENCADREES : Nb := self.GetNbSeriesEncadrees();
  end;
  case QModeBDD of
    mbddDISABLED         : EWE := GetResourceString('');
    mbddENTRANCES        : EWE := GetResourceString(rsTBS_ENTRANCE);
    mbddRESEAUX          : EWE := GetResourceString(rsTBS_RESEAUX);
    mbddSECTEURS         : EWE := GetResourceString(rsTBS_SECTEURS);
    mbddCODES            : EWE := GetResourceString(rsTBS_CODES);
    mbddEXPES            : EWE := GetResourceString(rsTBS_TRIPS);
    mbddPOI              : EWE := GetResourceString(rsTBS_POI);
    mbddCHECK_ERRORS     : EWE := GetResourceString(rsTBS_CHECKUP_MESSAGES);
    mbddNOEUDS           : EWE := GetResourceString(rsTBS_NOEUDS);
    mbddNAMESPACES       : EWE := GetResourceString(rsTBS_NAMESPACES);
    mbddPROXIMITES       :
      begin
        EWE := GetResourceString(rsTBS_PROXIMITES);
        if (GHTopoQuestionOuiNon('Rechercher à nouveau les proximités')) then self.RechercherProximites(editRayonCaptureProxi.Value);
        Nb := self.GetNbProximites();
      end;
    mbddSERIES_ENCADREES :
      begin
        EWE := GetResourceString(rsTBS_SERIES_ENCADREES);
        if (GHTopoQuestionOuiNon('Rechercher à nouveau les séries encadrées')) then self.RechercherSeriesEncadrees();
        Nb := self.GetNbSeriesEncadrees();
      end;
  end;
  lbNbreElements.Caption := Format('%s (%d items)', [EWE, Nb]);
  lsbListe.Enabled := False;
  lsbListe.Clear;
  if (0 = Nb) then exit;
  MyIdx := QGetCorrectIndex(QIdx, Nb);
  for i := 0 to Nb - 1 do lsbListe.Items.Add('');
  if (lsbListe.count > 0) then lsbListe.ItemIndex := MyIdx;
  lsbListe.Enabled       := True;
end;
//******************************************************************************
procedure TCdrListesSimples.InitHeaderListesSimples();
const
  LC_NAMESPACES       = 200;
  LC_ID_NUMERIQUES    = 60;
  LC_ID_SERIE_STATION = 110;
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
  editRayonCaptureProxi.Visible := false;
  hcColsTitres.Sections.Clear;
  case FModeBDD of
    mbddNAMESPACES:
     begin
       miou := GetResourceString(rsTBS_NAMESPACES);
     end;
    mbddENTRANCES:
      begin
        // Pas de namespace ici
        AjouterTitreColonneRes(rsSELECT_LISTE_ENTREES_ID, LC_ID_NUMERIQUES, LC_ID_NUMERIQUES);
        AjouterTitreColonneRes('', 30, 30);
        AjouterTitreColonneRes(rsSELECT_LISTE_ENTREES_NOM, 150, 300);
        AjouterTitreColonneRes(rsSELECT_LISTE_ENTREES_REF, 80, 80);
        AjouterTitreColonneRes('IDTerrain', 80, 110);
        AjouterTitreColonneRes(rsSELECT_LISTE_ENTREES_X, 80, 90);
        AjouterTitreColonneRes(rsSELECT_LISTE_ENTREES_Y, 80, 90);
        AjouterTitreColonneRes(rsSELECT_LISTE_ENTREES_Z, 80, 90);
        AjouterTitreColonneRes(rsSELECT_LISTE_OBSERV, 400, 400);
      end;
    mbddRESEAUX:
      begin
        // Pas de namespace ici
        //AjouterTitreColonneRes(rsSELECT_LISTE_NAMESPACE, LC_NAMESPACES, LC_NAMESPACES);
        AjouterTitreColonneRes(rsSELECT_LISTE_RESEAUX_ID, LC_ID_NUMERIQUES, LC_ID_NUMERIQUES);
        AjouterTitreColonneRes(rsSELECT_LISTE_RESEAUX_COLOR, 60, 60);
        AjouterTitreColonneRes(rsSELECT_LISTE_RESEAUX_NOM, 150, 250);
        AjouterTitreColonneRes(rsSELECT_LISTE_OBSERV, 400, 400);
      end;
    mbddSECTEURS:
      begin
        // Pas de namespace ici
        AjouterTitreColonneRes(rsSELECT_LISTE_SECTEURS_ID, LC_ID_NUMERIQUES, LC_ID_NUMERIQUES);
        AjouterTitreColonneRes(rsSELECT_LISTE_SECTEURS_COLOR, 60, 60);
        AjouterTitreColonneRes(rsSELECT_LISTE_SECTEURS_NOM, 400, 400);
      end;
    mbddCODES:
      begin
        //AjouterTitreColonneRes(rsSELECT_LISTE_NAMESPACE, LC_NAMESPACES, LC_NAMESPACES);
        AjouterTitreColonneRes(rsSELECT_LISTE_CODES_ID, LC_ID_NUMERIQUES, LC_ID_NUMERIQUES);
        AjouterTitreColonneRes(rsSELECT_LISTE_CODES_AZIMUTS, 100, 150);
        AjouterTitreColonneRes(rsSELECT_LISTE_CODES_CORR_AZIMUTS, 80, 90);
        AjouterTitreColonneRes(rsSELECT_LISTE_CODES_PENTES, 100, 260);
        AjouterTitreColonneRes(rsSELECT_LISTE_CODES_CORR_PENTES, 80, 90);
        AjouterTitreColonneRes(rsSELECT_LISTE_CODES_OBS, 150, 300);
      end;
    mbddEXPES:
      begin
        //AjouterTitreColonneRes(rsSELECT_LISTE_NAMESPACE, LC_NAMESPACES, LC_NAMESPACES);
        AjouterTitreColonneRes(rsSELECT_LISTE_EXPES_ID, LC_ID_NUMERIQUES, LC_ID_NUMERIQUES);
        AjouterTitreColonneRes(rsSELECT_LISTE_EXPES_COULEUR, 80, 80);
        AjouterTitreColonneRes(rsSELECT_LISTE_EXPES_DATE, 90, 90);
        AjouterTitreColonneRes(rsSELECT_LISTE_EXPES_OPERATEUR_TOPO, 80, 170);
        AjouterTitreColonneRes(rsSELECT_LISTE_EXPES_CLUB_SPELEO, 80, 170);
        AjouterTitreColonneRes(rsSELECT_LISTE_EXPES_MODE_DECLINAISON, 100, 100);
        AjouterTitreColonneRes(rsSELECT_LISTE_EXPES_DECLINAISON, 80, 100);
        AjouterTitreColonneRes(rsSELECT_LISTE_EXPES_OBS, 150, 300);
      end;
    mbddPOI:
      begin
        // Pas de namespaces ici
        AjouterTitreColonneRes(rsSELECT_LISTE_POI_IDX                  , 40, 60);
        AjouterTitreColonneRes(rsSELECT_LISTE_POI_SERIE_POINT          , 80, 100);
        AjouterTitreColonneRes(rsSELECT_LISTE_POI_LBLTERRAIN           , 80, 100);
        AjouterTitreColonneRes(rsSELECT_LISTE_POI_COULEUR              , 40, 60);
        AjouterTitreColonneRes(rsSELECT_LISTE_POI_DESCRIPTION          , 666, 100);
      end;
    mbddCHECK_ERRORS:
      begin
        // Pas de namespaces ici
        AjouterTitreColonneRes(rsSELECT_LISTE_CHECK_MSG_TABLE          , 120, 120);
        AjouterTitreColonneRes(rsSELECT_LISTE_CHECK_MSG_IDX            , 60, 60);
        AjouterTitreColonneRes(rsSELECT_LISTE_CHECK_MSG_COULEUR        , 30, 30);
        AjouterTitreColonneRes(rsSELECT_LISTE_CHECK_MSG_CRITICITE      , 140, 140);
        AjouterTitreColonneRes(rsSELECT_LISTE_CHECK_MSG_MESSAGE        , 1200, 1200);
      end;
    mbddNOEUDS:
      begin
        // Pas de namespaces ici
        AjouterTitreColonneRes(rsSELECT_LISTE_NOEUD_ID                 , 80 , 80);
        AjouterTitreColonneRes(rsSELECT_LISTE_NOEUD_STATION            , 140, 120);
        AjouterTitreColonneRes(rsSELECT_LISTE_NOEUD_COORDS_X           , 120, 120);
        AjouterTitreColonneRes(rsSELECT_LISTE_NOEUD_COORDS_Y           , 120, 120);
        AjouterTitreColonneRes(rsSELECT_LISTE_NOEUD_COORDS_Z           , 120, 120);
        AjouterTitreColonneRes(rsSELECT_LISTE_NOEUD_LABEL              , 200, 200);
      end;
    mbddPROXIMITES:
      begin
        // Pas de namespaces ici
        AjouterTitreColonneRes(rsSELECT_LISTE_PROXIMITE_SERIE                     , 100 , 100);
        AjouterTitreColonneRes(rsSELECT_LISTE_PROXIMITE_LONGUEUR_SERIE            , 120 , 120);
        AjouterTitreColonneRes(rsSELECT_LISTE_PROXIMITE_BASEPOINT                 , 140 , 140);
        AjouterTitreColonneRes(rsSELECT_LISTE_PROXIMITE_BASEPOINT_OBSERV          , 220 , 220);
        AjouterTitreColonneRes(rsSELECT_LISTE_PROXIMITE_NEAREST                   , 140 , 140);
        AjouterTitreColonneRes(rsSELECT_LISTE_PROXIMITE_DISTANCE                  , 120 , 120);
        AjouterTitreColonneRes(rsSELECT_LISTE_PROXIMITE_COMMENTAIRES              , 300 , 666);
        editRayonCaptureProxi.Visible := True;
      end;
    mbddSERIES_ENCADREES:
      begin
        // Pas de namespaces ici
        AjouterTitreColonneRes(rsSELECT_LISTE_SER_ENCADREES_NO_SERIE              , 100 , 100);
        AjouterTitreColonneRes(rsSELECT_LISTE_SER_ENCADREES_DEBUT                 , 200 , 200);
        AjouterTitreColonneRes(rsSELECT_LISTE_SER_ENCADREES_FIN                   , 200 , 200);
        AjouterTitreColonneRes(rsSELECT_LISTE_SER_ENCADREES_NOM_SERIE             , 400 , 666);
      end;
  else
    pass;
  end;
  lbRayonCaptureProxi.Visible   := editRayonCaptureProxi.Visible;
end;

procedure TCdrListesSimples.lsbListeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
    DIST_OUT_OF_ORDER = 8.00;
    Q4  = 4; // compensation du décalage entre le header et la liste
    mg  = 1;
var
  C, WU: TColor;
  m: integer;
  rs: TReseau;
  es: TEntrance;
  cs: TCode;
  ss: TExpe;
  miou: string;
  sc: TSecteur;
  pp: TPointOfInterest;
  cm: TMessaqeErreurGHTopoCompiler;
  nd: TJonctionXYZ;
  sp: TStationProcheOfEndSerie;
  se: TSerieEncadree;
  // bg = couleur de fond; tc= couleur du texte
  procedure DessineItem(const bg,tc: TColor);
  VAR
    QIdxNamespace, QNoCode, QNoExpe, QNoSerie: integer;
    MyNamespace: TNameSpace;
    DistColor: TColor;
    QPP: TPalette256;
    QAT: String;

  begin
    case FModeBDD of
      mbddENTRANCES:
        begin
          ResetColorRow(lsbListe, ARect, bg, tc);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[Index]));
          DrawColRectColoreWithTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], True, bg, es.eCouleur, '');

          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true, es.eNomEntree);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], true, Format(FMTSERST,[es.eRefSer, es.eRefSt]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[4], true, es.eIDTerrain);
          DrawColCoordsXYZ(lsbListe, ARect, hcColsTitres.Sections.Items[5], bg, clRed, es.eXEntree);
          DrawColCoordsXYZ(lsbListe, ARect, hcColsTitres.Sections.Items[6], bg, clRed, es.eYEntree);
          DrawColCoordsXYZ(lsbListe, ARect, hcColsTitres.Sections.Items[7], bg, clRed, es.eZEntree);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[8], true, es.eObserv);
        end;
      mbddRESEAUX:
        begin
          ResetColorRow(lsbListe, ARect, bg, tc);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[Index]));
          DrawColRectColoreWithTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], True, bg, rs.ColorReseau, '');
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true, rs.NomReseau);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], true, rs.ObsReseau);
        end;
      mbddSECTEURS:
        begin
          ResetColorRow(lsbListe, ARect, bg, tc);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[Index]));
          DrawColRectColoreWithTexte(lsbListe, ARect,hcColsTitres.Sections.Items[1], True, bg, sc.CouleurSecteur, '');
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true, sc.NomSecteur);
        end;
      mbddCODES:
        begin
          DecomposeNumeroCode(cs.IDCode, QIdxNamespace, QNoCode);
          MyNamespace := FDocumentToporobot.GetNameSpace(QIdxNamespace);
          ResetColorRow(lsbListe, ARect, bg, tc);
          //DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, MyNamespace.Nom);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], true , Format(FORMAT_NB_INTEGER, [QNoCode]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], true , ExplainCodeAzimut(cs));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true , Format(FORMAT_NB_REAL_3_DEC, [cs.ParamsFuncCorrAz.Co]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], true , ExplainCodePente(cs)); //Format(FORMAT_NB_REAL_0_DEC, [cs.Gradinc]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[4], true , Format(FORMAT_NB_REAL_3_DEC, [cs.ParamsFuncCorrInc.Co]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[5], true , cs.Commentaire);
        end;
      mbddEXPES:
        begin
          DecomposeNumeroExpe(ss.IDExpe, QIdxNamespace, QNoExpe);
          MyNamespace := FDocumentToporobot.GetNameSpace(QIdxNamespace);
          ResetColorRow(lsbListe, ARect, bg, tc);
          try
            {$WARNING: TEXpe.DateExpe à implementer}
            miou := DateToStr(GetSecuredDate(ss.AnneeExpe,ss.MoisExpe,ss.JourExpe));
          except
            miou := '01/01/2000';
          end;
          //DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, MyNamespace.Nom);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], true , Format(FORMAT_NB_INTEGER,[QNoExpe]));
          QPP := FBDDEntites.GetPalette256();
          DrawColRectColoreWithTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], True, bg, QPP.GetColorByIndex(ss.IdxCouleur), Format(FORMAT_NB_INTEGER,[ss.IdxCouleur]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true , miou);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], true , ss.Operateur);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[4], true , ss.ClubSpeleo);
          case ss.ModeDecl of
            cdmMANUEL     : QAT := 'Manuelle';
            cdmAUTOMATIQUE: QAT := 'Automatique';
          end;
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[5], true , QAT);

          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[6], true , Format(FORMAT_NB_REAL_3_DEC,[ss.DeclinaisonInDegrees]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[7], true , ss.Commentaire);
        end;
      mbddPOI:
        begin
          ResetColorRow(lsbListe, ARect, bg, tc);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER, [Index]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], true , Format(FMTSERST, [PP.Serie, PP.Station]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true , PP.LabelTerrain);
          DrawColRectColoreWithTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], True, bg, PP.Couleur, '');
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[4], true , PP.Description);
        end;
      mbddCHECK_ERRORS:
        begin
          ResetColorRow(lsbListe, ARect, bg, tc);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, DescribeTableExamineeByCode(cm.TableExaminee));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], true , Format(FORMAT_NB_INTEGER, [cm.Index]));
          DrawColRectColoreWithTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], True, bg, cm.Couleur, '');
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], true , DescribeCriticiteErreur(cm.Criticite));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[4], true , cm.Message);
        end;
       mbddNOEUDS:
        begin
          DecomposeNumeroSerie(nd.NoSer, QIdxNamespace, QNoSerie);
          ResetColorRow(lsbListe, ARect, bg, tc);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER, [nd.NoNoeud]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], true , Format(FMTSERST, [QNoSerie, nd.NoSt]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true , Format(FORMAT_NB_REAL_3_DEC, [nd.X]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], true , Format(FORMAT_NB_REAL_3_DEC, [nd.Y]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[4], true , Format(FORMAT_NB_REAL_3_DEC, [nd.Z]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[5], true , Format(FORMAT_STRING, [nd.IDJonction]));
        end;
       mbddPROXIMITES:
        begin
          ResetColorRow(lsbListe, ARect, bg, tc);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER, [sp.BaseStation.aSerie]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], true , Format(FORMAT_NB_REAL_3_DEC, [sp.LongueurSerie]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true , Format(FMTSERST, [sp.BaseStation.aSerie, sp.BaseStation.aStation]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], true , sp.ObservLastStation);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[4], true , Format(FMTSERST, [sp.NearestStation.aSerie, sp.NearestStation.aStation]));
          DistColor   := GetColorDegrade(sp.Distance, 0.001, Min(editRayonCaptureProxi.Value, DIST_OUT_OF_ORDER), clLime, clRed);
          DrawColRectColoreWithTexte(lsbListe, ARect, hcColsTitres.Sections.Items[5], True, bg, DistColor, Format(FORMAT_NB_REAL_3_DEC, [sp.Distance]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[6], true , sp.Statut);
        end;
       mbddSERIES_ENCADREES:
        begin
          ResetColorRow(lsbListe, ARect, bg, tc);
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[0], false, Format('%d', [se.NumeroSerie]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[1], true , Format(FMTSERST, [se.StationDebut.aSerie, se.StationDebut.aStation]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[2], true , Format(FMTSERST, [se.StationFin.aSerie, se.StationFin.aStation]));
          DrawColTexte(lsbListe, ARect, hcColsTitres.Sections.Items[3], true , se.NomSerie);
        end;
      else
        pass;
      end;
  end;
begin
  try
    case FModeBDD of
      mbddENTRANCES       : es := FDocumentToporobot.GetEntrance(Index);
      mbddSECTEURS        : sc := FDocumentToporobot.GetSecteur(Index);
      mbddRESEAUX         : rs := FDocumentToporobot.GetReseau(Index);
      mbddCODES           : cs := FDocumentToporobot.GetCode(Index);
      mbddEXPES           : ss := FDocumentToporobot.getExpe(Index);
      mbddPOI             : if (Index > (FDocumentToporobot.GetNbPointsOfInterests() - 1)) then Exit else pp := FDocumentToporobot.GetPointOfInterest(Index);
      mbddCHECK_ERRORS    : if (Index > (FDocumentToporobot.GetNbMessagesErreur()    - 1)) then Exit else cm := FDocumentToporobot.GetMessageErreur(Index);
      mbddNOEUDS          : nd := FBDDEntites.GetJonction(Index);
      mbddPROXIMITES      : if (Index > (GetNbProximites() - 1)) then Exit else sp := self.GetProximite(Index);
      mbddSERIES_ENCADREES: se := self.GetSerieEncadree(Index);
    end;
    WU := IIF((FModeBDD in [mbddCODES, mbddEXPES]) and (Index = 0), clGray, clWhite);
    if (odSelected in state) then DessineItem(clBlue, WU) else DessineItem(WU, clBlack);
  except
    pass;
  end;
end;

procedure TCdrListesSimples.lsbListeSelectionChange(Sender: TObject; User: boolean);

var
  MyExtrSerie: TStationProcheOfEndSerie;
  MyNode     : TJonctionXYZ;
  MyEntrance : TEntrance;
  MyPOI      : TPointOfInterest;
  EWE        : TToporobotIDStation;
begin
  case FModeBDD of
    mbddENTRANCES:
       begin
         if (assigned(FProcOfObjectWithXY)) then
         begin
           MyEntrance := FDocumentToporobot.GetEntrance(lsbListe.ItemIndex);
           FProcOfObjectWithXY(MyEntrance.eXEntree, MyEntrance.eYEntree, MyEntrance.eNomEntree);
         end;
       end;
    mbddNOEUDS:
       begin
         if (assigned(FProcOfObjectWithXY)) then
         begin
           MyNode := FBDDEntites.GetJonction(lsbListe.ItemIndex);
           FProcOfObjectWithXY(MyNode.X, MyNode.Y, Format(FMTSERST, [MyNode.NoSer, MyNode.NoSt]));
         end;
       end;
    mbddPOI:
       begin
         if (assigned(FProcOfObjectWithXY)) then
         begin
           MyPOI := FDocumentToporobot.GetPointOfInterest(lsbListe.ItemIndex);
           EWE.aSerie       := MyPOI.Serie;
           EWE.aStation     := MyPOI.Station;
           EWE.aIDTerrain   := MyPOI.Description;
           EWE.eIdxNameSpace:= 0;
           FProcOfObjectWithTBaseStation(EWE);
         end;
       end;
    mbddPROXIMITES:
      begin
        if (assigned (FProcOfObjectWithTBaseStation)) then
        begin
          MyExtrSerie := FListeDesProximites.GetElement(lsbListe.ItemIndex);
          FProcOfObjectWithTBaseStation(MyExtrSerie.BaseStation);
        end;
      end;
  else
    pass;
  end;
end;
// liste des proximités
procedure TCdrListesSimples.AddProximite(const P: TStationProcheOfEndSerie);
begin
  FListeDesProximites.AddElement(P);
end;
function TCdrListesSimples.GetProximite(const Idx: integer): TStationProcheOfEndSerie;
begin
  result := FListeDesProximites.GetElement(Idx);
end;

procedure TCdrListesSimples.PutProximite(const Idx: integer; const P: TStationProcheOfEndSerie);
begin
  FListeDesProximites.PutElement(Idx, P);
end;

function TCdrListesSimples.GetNbProximites(): integer;
begin
  result := FListeDesProximites.GetNbElements();
end;
// recherche des proximités
procedure TCdrListesSimples.RechercherProximites(const QRayonCapture: double);
var
  NbSeries, NbEntitesVisees, s, v, QIdx: Integer;
  MySerie: TObjSerie;
  BP, NP, SP: TBaseStation;
  QDistance, QDist: double;
  EWE: TStationProcheOfEndSerie;
  MyLastStationOfSerie: TUneVisee;
begin
  AfficherMessage(Format('%s.RechercherProximites(%.2f)', [Classname, QRayonCapture]));
  NbSeries := FDocumentToporobot.GetNbSeries();
  NbEntitesVisees  := FBDDEntites.GetNbEntitesVisees();
  self.FListeDesProximites.ClearListe();
  ProgressBar1.Visible := True;
  ProgressBar1.Min     := 0;
  ProgressBar1.Max     := NbSeries - 1;
  for s := 1 to NbSeries - 1 do
  begin
    MySerie := FDocumentToporobot.GetSerie(s);
    // on ne traite que les extrémités d'une série non raccordées à une autre série
    // Faille prévisible: les autoloops
    if (MySerie.GetNumeroDeSerie() = MySerie.GetNoSerieArr()) then
    begin
      if (FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNoSerieArr(), MySerie.GetNoPointArr(), BP)) then
      begin
        SP := BP;
        QDistance:= INFINI;
        for v := 0 to NbEntitesVisees - 1 do
        begin
          NP := FBDDEntites.GetEntiteVisee(v);
          QDist := Hypot3D(NP.PosStation.X - BP.PosStation.X, NP.PosStation.Y - BP.PosStation.Y, NP.PosStation.Z - BP.PosStation.Z);
          if (IsZero(QDist)) then Continue;
          if (QDist < QDistance) then
          begin
            if (BP.Entite_Serie <> NP.Entite_Serie) then
            begin
              QDistance := QDist;
              SP := NP;
            end;
          end;
        end;
        if (QDistance > 0.005) then
        begin
          EWE.LongueurSerie    := MySerie.GetLongueurOfSerie();
          MyLastStationOfSerie := MySerie.GetLastVisee();   // attrapper les commentaires de la dernière station
          EWE.BaseStation.aSerie      := BP.Entite_Serie;
          EWE.BaseStation.aStation    := BP.Entite_Station;
          EWE.NearestStation.aSerie   := SP.Entite_Serie;
          EWE.NearestStation.aStation := SP.Entite_Station;
          EWE.Distance                := QDistance;
          EWE.Statut                  := '';
          EWE.ObservLastStation       := MyLastStationOfSerie.Commentaires;
          if (QDistance < QRayonCapture) then self.AddProximite(EWE);
        end;
      end;
    end;
    ProgressBar1.Position := s;
    Application.ProcessMessages;
  end;
  ProgressBar1.Visible := false;
  // et on trie
  FListeDesProximites.Trier(0);
end;
// Les séries encadrées
procedure TCdrListesSimples.AddSerieEncadree(const P: TSerieEncadree);
begin
  FListeSeriesEncadrees.AddElement(P);
end;

function TCdrListesSimples.GetNbSeriesEncadrees(): integer;
begin
  Result := FListeSeriesEncadrees.GetNbElements();
end;

function TCdrListesSimples.GetSerieEncadree(const Idx: integer): TSerieEncadree;
begin
  Result := FListeSeriesEncadrees.GetElement(Idx);
end;
procedure TCdrListesSimples.RechercherSeriesEncadrees();
var
  i, Nb  : Integer;
  MySerie: TObjSerie;
  SR     : TSerieEncadree;
begin
  AfficherMessage(Format('%s.RechercherSeriesEncadrees()', [Classname]));
  lsbListe.Enabled := false;
  Nb := FDocumentToporobot.GetNbSeries();
  FListeSeriesEncadrees.ClearListe();
  for i := 1 to Nb - 1 do
  begin
    MySerie := FDocumentToporobot.GetSerie(i);
    if (MySerie.GetNumeroDeSerie() <> MySerie.GetNoSerieArr()) then
    begin
      SR.NomSerie                 := MySerie.GetNomSerie();
      SR.NumeroSerie              := MySerie.GetNumeroDeSerie();
      SR.StationDebut.aSerie      := MySerie.GetNoSerieDep();
      SR.StationDebut.aStation    := MySerie.GetNoPointDep();
      SR.StationFin.aSerie        := MySerie.GetNoSerieArr();
      SR.StationFin.aStation      := MySerie.GetNoPointArr();
      AddSerieEncadree(SR);
    end;
  end;
  lsbListe.Enabled := True;
end;

procedure TCdrListesSimples.GotoError(const Err: TMessaqeErreurGHTopoCompiler);
var
  EE: TEntrance;
  RR: TReseau;
  SC: TSecteur;
  SR: TObjSerie;
  QIdx: integer;
  CC: TCode;
  EX: TExpe;
begin
  case Err.TableExaminee of
    tmeENTRANCES:
      begin
        EE := FDocumentToporobot.GetEntrance(ERR.Index);
        if (EditerEntrance(FDocumentToporobot, Err.Index, FBDDEntites , FMaillage, EE)) then
        begin
          FDocumentToporobot.PutEntrance(ERR.Index, EE);
          FDocumentToporobot.CheckerLesDonneesTopo();
          ListerLesElements(mbddCHECK_ERRORS, 0);
        end;
      end;
    tmeRESEAUX:
      begin
        RR :=FDocumentToporobot.GetReseau(ERR.Index);
        if (EditerReseau(FDocumentToporobot, Err.Index, RR)) then
        begin
          FDocumentToporobot.PutReseau(ERR.Index, RR);
          FDocumentToporobot.CheckerLesDonneesTopo();
          ListerLesElements(mbddCHECK_ERRORS, 0);
        end;
      end;
    tmeSECTEURS:
       begin
         SC :=FDocumentToporobot.GetSecteur(ERR.Index);
         if (EditerSecteur(FDocumentToporobot, Err.Index, SC)) then
         begin
          FDocumentToporobot.PutSecteur(ERR.Index, SC);
          FDocumentToporobot.CheckerLesDonneesTopo();
          ListerLesElements(mbddCHECK_ERRORS, 0);
         end;
       end;
    tmeCODES:
       begin
         CC :=FDocumentToporobot.GetCodeByNumero(ERR.Index);
         QIdx := FDocumentToporobot.GetIdxCodeByNumero(CC.IDCode);
         if (EditerCode(FDocumentToporobot, QIdx, CC)) then
         begin
          FDocumentToporobot.PutCode(QIdx, CC);
          FDocumentToporobot.CheckerLesDonneesTopo();
          ListerLesElements(mbddCHECK_ERRORS, 0);
         end;
       end;
    tmeEXPES:
       begin
         EX :=FDocumentToporobot.GetExpeByNumero(ERR.Index);
         QIdx := FDocumentToporobot.GetIdxExpeByNumero(EX.IDExpe);
         if (EditerExpe(FDocumentToporobot, QIdx, EX)) then
         begin
          FDocumentToporobot.PutExpe(QIdx, EX);
          FDocumentToporobot.CheckerLesDonneesTopo();
          ListerLesElements(mbddCHECK_ERRORS, 0);
         end;
       end;
    tmeSERIES:
       begin
         if (FDocumentToporobot.GetSerieByNumeroSerie(ERR.Index, SR, QIdx)) then
         begin
           if (DisplayUneSerie(FDocumentToporobot, FBDDEntites, QIdx, SR)) then
           begin
             FDocumentToporobot.CheckerLesDonneesTopo();
             ListerLesElements(mbddCHECK_ERRORS, 0);
           end;
         end;
       end;
  end;
end;
end.

