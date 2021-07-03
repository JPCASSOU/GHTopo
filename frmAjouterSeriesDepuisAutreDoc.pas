unit frmAjouterSeriesDepuisAutreDoc;


{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  UnitObjetSerie,
  ConvertisseurJPC,
  CadreListeSeries,

  Classes, SysUtils,
  LazFileUtils,
  curredit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, DateTimePicker;

type TTodoActionWithExistentSerie = (aesAPPEND_VISEES, aesREPLACE_VISEES, aesIGNORE_SERIE);
type

  { TdlgAddSeriesFromOtherDoc }

  TdlgAddSeriesFromOtherDoc = class(TForm)
    btnClose: TBitBtn;
    btnSelectFilename: TButton;
    btnImport: TButton;
    btnCharger: TButton;
    btnReprojeter: TButton;
    CdrListeSeries1: TCdrListeSeries;
    chkAddEntrances: TCheckBox;
    chkCheckStationRattachement: TButton;
    chkCreateNewCode: TCheckBox;
    chkCreateNewExpe: TCheckBox;
    chkCreerNouveauReseau: TCheckBox;
    cmbExtrAccrochage: TComboBox;
    btnColorReseau: TColorButton;
    editDateExpe: TDateTimePicker;
    editPtZeroReseauAddedSerie: TCurrencyEdit;
    editPtZeroReseauAddedPoint: TCurrencyEdit;
    editImportFilename: TEdit;
    editFirstNumCodeForSynthese: TCurrencyEdit;
    editFirstNumExpeForSynthese: TCurrencyEdit;
    editFirstNumSerieForSynthese: TCurrencyEdit;
    editNouveauReseau: TEdit;
    editPointRaccordement: TCurrencyEdit;
    editSerieAAccrocher: TCurrencyEdit;
    editSerieRaccordement: TCurrencyEdit;
    editX1: TCurrencyEdit;
    editX2: TCurrencyEdit;
    editY1: TCurrencyEdit;
    editY2: TCurrencyEdit;
    grbxCreateNewCodeExpe: TGroupBox;
    grbxRaccorderA: TGroupBox;
    grbxReprojectionCoordonnees: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbLastAttributedCode: TStaticText;
    lbLastAttributedExpe: TStaticText;
    lbNomSerieSource: TStaticText;
    lbSystemeCoordsImported: TStaticText;
    PageControl1: TPageControl;
    pnlListeSeries: TPanel;
    pnlReprojection: TPanel;
    pnlDoActions: TPanel;
    pnlSelectFilename: TPanel;
    rgbxTodoWithSerie: TRadioGroup;
    lbSystemeCoordsCourant: TStaticText;
    lbLastAttributedSerie: TStaticText;
    tabShtAddCavite: TTabSheet;
    tabshtTopoDuJour: TTabSheet;
    procedure btnChargerClick(Sender: TObject);
    procedure btnReprojeterClick(Sender: TObject);
    procedure btnSelectFilenameClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure chkCheckStationRattachementClick(Sender: TObject);
    procedure editFirstNumSerieForSyntheseChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    FCoordConvertisseur: TConversionSysteme;
    FDocTopoCourant    : TToporobotStructure2012;
    FDocTopoAAjouter   : TToporobotStructure2012;
    procedure AjouterLaTopoImportee();
    function AjouterLesSeriesTopoDuJour(): boolean;
    function ChargerLaTopoExt(const FC: TStringDirectoryFileName): boolean;
    procedure GetSerieAccrochage(const N: TNumeroSerie);
  public
    function  Initialiser(const FD: TToporobotStructure2012; const FC: TConversionSysteme): boolean;
    procedure Finaliser();

  end;

var
  dlgAddSeriesFromOtherDoc: TdlgAddSeriesFromOtherDoc;

implementation

{$R *.lfm}

{ TdlgAddSeriesFromOtherDoc }
uses
  DGCDummyUnit,
  CallDialogsStdVersion;


function TdlgAddSeriesFromOtherDoc.Initialiser(const FD: TToporobotStructure2012; const FC: TConversionSysteme): boolean;
const
  FMT_LAST_ITEM_OF_CURRENT_DOC = 'Dernier %s du document courant: %d';
var
  QButtons: TButtonsShown;
  EWE: TLabelSystemesCoordsEPSG;
  MyEntrance: TEntrance;
  procedure MiouMiou(const Miou: TCurrencyEdit; const V: integer);
  begin
    Miou.AsInteger := V + 1;
    Miou.MinValue  := V + 1;
  end;
begin
  result := false;
  pnlDoActions.Enabled := false;


  chkCreateNewCode.Checked := True;
  chkCreateNewExpe.Checked := True;
  editDateExpe.Date := Now() - 1.00; // les reports se faisant généralement le lendemain, ceci rétablit la date de séance
  rgbxTodoWithSerie.ItemIndex := Ord(aesAPPEND_VISEES);
  FDocTopoCourant     := FD;
  FCoordConvertisseur := FC;
  FDocTopoAAjouter    := TToporobotStructure2012.Create;
  try
    lbLastAttributedCode.caption   := format(FMT_LAST_ITEM_OF_CURRENT_DOC, ['code'   , FDocTopoCourant.getMaxIdxCode()]);
    lbLastAttributedExpe.caption   := format(FMT_LAST_ITEM_OF_CURRENT_DOC, ['séance' , FDocTopoCourant.getMaxIdxExpe()]);
    lbLastAttributedSerie.caption  := format(FMT_LAST_ITEM_OF_CURRENT_DOC, ['série'  , FDocTopoCourant.getMaxIdxSerie()]);
    EWE := FDocTopoCourant.GetCodeEPSGSystemeCoordonnees();
    lbSystemeCoordsCourant.Caption := Format('EPSG:%d - %s', [EWE.CodeEPSG, EWE.NomEPSG]);
    MyEntrance := FDocTopoCourant.GetEntrance(0);
    editX1.Value := MyEntrance.ePosition.X;
    editY1.Value := MyEntrance.ePosition.Y;

    // récupération des Idx
    MiouMiou(editFirstNumCodeForSynthese   , FDocTopoCourant.getMaxIdxCode());
    MiouMiou(editFirstNumExpeForSynthese   , FDocTopoCourant.getMaxIdxExpe());
    MiouMiou(editFirstNumSerieForSynthese  , FDocTopoCourant.getMaxIdxSerie());

    lbLastAttributedCode.caption   := format('%d', [FDocTopoCourant.getMaxIdxCode()]);
    lbLastAttributedExpe.caption   := format('%d', [FDocTopoCourant.getMaxIdxExpe()]);
    lbLastAttributedSerie.caption  := format('%d', [FDocTopoCourant.getMaxIdxSerie()]);

    PageControl1.ActivePageIndex := 1;
    Result := True;
    pnlListeSeries.Enabled := (PageControl1.ActivePageIndex = 0);
  except
    pass;
  end;
end;
procedure TdlgAddSeriesFromOtherDoc.Finaliser();
begin
  try
    FDocTopoAAjouter.Finaliser();
  finally
    FreeAndNil(FDocTopoAAjouter);//FDocTopoSource.Free;
  end;
end;
function TdlgAddSeriesFromOtherDoc.AjouterLesSeriesTopoDuJour(): boolean;
var
  Nb, i, NbSts, St, QStartIdxVisee, QInternalIdx, QNbSts: Integer;
  MySerieSource, MySerieCible, SerieExistante: TObjSerie;
  QTransfertSerieOK: Boolean;
  MyCode: TCode;
  MyExpe: TExpe;
  YYYY, MM, DD: word;
  QTodoWithSeriesExistantes: TTodoActionWithExistentSerie;
  MyReseau: TReseau;
  MySecteur: TSecteur;
  procedure MiouMiou(const QSerieDest: TObjSerie; const QV : TUneVisee; const QSt: integer);
  var
    QVisee: TUneVisee;
    QArrayAntennes: TArrayOfTViseeAntenne;
    a: Integer;
  begin
    QVisee := QV;
    QVisee.Code      := MyCode.IDCode;
    QVisee.Expe      := MyExpe.IDExpe;
    QVisee.IDSecteur := 0;
    QSerieDest.AddVisee(QVisee);
    if (FDocTopoAAjouter.ExtractViseesAntennesOfStation(QSerieDest.GetNumeroDeSerie(), QSt, QArrayAntennes) > 0) then
    begin
      for a := 0 to High(QArrayAntennes) do FDocTopoCourant.AddViseeAntenne(QArrayAntennes[a].VA);
    end;
  end;
begin
  result := false;
  Nb := FDocTopoAAjouter.GetNbSeries();
  ShowMessage('AjouterLesSeries()');
  AfficherMessageErreur(Format('%s.AjouterLesSeries(): %d series', [ClassName, Nb]));
  AfficherMessageErreur('En provenance du document: ' + FDocTopoAAjouter.GetDatabaseName);
  if (Nb <= 1) then Exit;
  QTodoWithSeriesExistantes := TTodoActionWithExistentSerie(rgbxTodoWithSerie.ItemIndex);
  // préparer les codes et expés
  if (chkCreateNewCode.Checked) then
  begin
    MyCode := FDocTopoCourant.GetLastCode();
    MyCode.Commentaire := 'Topo du ' + DateTimeToStr(editDateExpe.Date);
    MyCode.IDCode += 1;
    FDocTopoCourant.AddCode(MyCode);
  end;
  if (chkCreateNewCode.Checked) then
  begin
    MyExpe := FDocTopoCourant.GetLastExpe();
    DecodeDate(editDateExpe.Date, YYYY, MM, DD);
    {$WARNING: TEXpe.DateExpe à implementer}
    MyExpe.AnneeExpe := YYYY; MyExpe.MoisExpe := MM; MyExpe.JourExpe := DD;
    MyExpe.IDExpe  += 1;
    MyExpe.IdxCouleur := (MyExpe.IdxCouleur + 1) MOD 256; // propale: ajouter 1 à l'index couleur de la dernière expé
    FDocTopoCourant.AddExpe(MyExpe);
  end;
  // attraper les derniers codes, reseau, secteur, expé
  MyReseau  := FDocTopoCourant.GetLastReseau();
  MySecteur := FDocTopoCourant.GetLastSecteur();
  MyCode    := FDocTopoCourant.GetLastCode();
  MyExpe    := FDocTopoCourant.GetLastExpe();

  for i := 1 to Nb - 1 do
  begin
    if (CdrListeSeries1.IsItemListeSelected(i)) then
    begin
      MySerieSource := FDocTopoAAjouter.GetSerie(i);
      AfficherMessageErreur(Format('%d: %d - %s', [i, MySerieSource.GetNumeroDeSerie(), MySerieSource.GetNomSerie()]));
      if (FDocTopoCourant.GetSerieByNumeroSerie(MySerieSource.GetNumeroDeSerie(), SerieExistante, QInternalIdx)) then
      begin
        AfficherMessageErreur(Format('-- Serie %d existante - Actions: %s', [SerieExistante.GetNumeroDeSerie(), ChooseString(Ord(QTodoWithSeriesExistantes), ['Ajouter visées en queue', 'Remplacer visées', 'Ne rien faire'])]));
        NbSts := MySerieSource.GetNbVisees();
        case QTodoWithSeriesExistantes of
          aesAPPEND_VISEES:
          begin
            QStartIdxVisee := SerieExistante.GetNbVisees();
            if (QStartIdxVisee < (NbSts - 1)) then
            begin
              for St := QStartIdxVisee to NbSts - 1 do MiouMiou(SerieExistante, MySerieSource.GetVisee(St), St);
              SerieExistante.SetNoPointArr(SerieExistante.GetNbVisees() - 1);
            end
            else AfficherMessageErreur('-- Aucune visée à ajouter');
          end;
          aesREPLACE_VISEES:
          begin
            SerieExistante.ClearStations();
            QStartIdxVisee := 0;
            for St := QStartIdxVisee to NbSts - 1 do MiouMiou(SerieExistante, MySerieSource.GetVisee(St), St);
            SerieExistante.SetNoPointArr(SerieExistante.GetNbVisees() - 1);
          end;
          aesIGNORE_SERIE: pass; // ne rien faire ici
        end;
      end
      else
      begin // série non trouvée = on en crée une
        QTransfertSerieOK := false;
        // /!\ Ne pas faire un simple FDocTopoCible.AddSerie(MySerieSource):
        //     MySerieSource est un pointeur d'objet appartenant à FDocTopoSource,
        //     lequel sera détruit à la fermeture du dialogue et MySerieSource avec ... ;-)
        // Ne pas ici utiliser la fonction  CreateAndCopySerie();
        MySerieCible := TObjSerie.Create;
        try
          MySerieCible.ClearStations();                                                                                 // Opération 1: Vider la liste des visées
          MySerieCible.SetChanceObstacle(MySerieSource.GetChance(), MySerieSource.GetObstacle());                       // Opération 2: Chances et obstacles
          MySerieCible.SetSeriePtExtremites(MySerieSource.GetNoSerieDep(), MySerieSource.GetNoPointDep(),               // Opération 3: Extrémités de la série
                                            MySerieSource.GetNoSerieArr(), MySerieSource.GetNoPointArr());
          MySerieCible.SetNumeroSerie(MySerieSource.GetNumeroDeSerie());                                                // Opération 4: Numéro de série
          MySerieCible.SetNomObsSerie(MySerieSource.GetNomSerie(), '*** Imported *** ' + MySerieSource.GetObsSerie());  // Opération 5: Nom et observations sur la série
          MySerieCible.SetRaideur(MySerieSource.GetRaideur());                                                          // Opération 6: Raideur
          MySerieCible.SetCouleur(MySerieSource.GetCouleur());                                                          // Opération 7: Couleur
          MySerieCible.SetNumeroEntrance(0);                                                                            // Opération 8: Entrée de rattachement
          MySerieCible.SetNumeroReseau(0);                                                                              // Opération 9: Réseau de rattachement
          // Opération 10: Traitement des visées
          // les stations -- TODO: A valider
          QStartIdxVisee := 0;
          NbSts := MySerieSource.GetNbVisees();
          for St := QStartIdxVisee to NbSts - 1 do MiouMiou(MySerieCible, MySerieSource.GetVisee(St), St);
          QTransfertSerieOK := True; // création OK ? On autorise l'ajout
        except
          FreeAndNil(MySerieCible);//MySerieCible.Free;  // en cas d'échec, on libère immédiatement
        end;
        if (QTransfertSerieOK) then
        begin
          FDocTopoCourant.AddSerie(MySerieCible);
          MySerieCible := FDocTopoCourant.GetLastSerie();
        end; // if (QTransfertSerieOK) then
      end;   // if (FDocTopoCible.GetSerieByNumeroSerie(MySerieSource.GetNumeroDeSerie(), SerieExistante, QSInternalIdx)) then
    end;     // if (CdrListeSeries1.IsItemListeSelected(i)) then
  end;       // for i := 1 to Nb - 1 do
end;



procedure TdlgAddSeriesFromOtherDoc.btnSelectFilenameClick(Sender: TObject);
var
  QDefaultExt    : String;
  QFilename      : TStringDirectoryFilename;
begin
  QFilename := Trim(editImportFilename.Text);
  QDefaultExt := '.xtb';
  if (DoDialogOpenFile(GetGHTopoDirectory(), GetResourceString(rsGHTOPO_FILE_FILTER_W_TEXT), QDefaultExt, QFileName)) then editImportFilename.Text := QFilename;
end;

procedure TdlgAddSeriesFromOtherDoc.btnChargerClick(Sender: TObject);
var
  PO, PC : TProjUV;
  EWE1, EWE2: TLabelSystemesCoordsEPSG;
  MyEntrance: TEntrance;
  MyReseau: TReseau;
begin
  if (ChargerLaTopoExt(Trim(editImportFilename.Text))) then
  begin
    pnlDoActions.Enabled := CdrListeSeries1.Initialiser(FDocTopoAAjouter, GetSerieAccrochage, nil, [tbsHELP], True);
    // réseau
    MyReseau := FDocTopoAAjouter.GetLastReseau();
    editNouveauReseau.Text     := MyReseau.NomReseau;
    btnColorReseau.ButtonColor := MyReseau.ColorReseau.toTColor();

    // systèmes de coordonnées
    EWE1 := FDocTopoCourant.GetCodeEPSGSystemeCoordonnees();
    lbSystemeCoordsCourant.Caption := Format('EPSG:%d - %s', [EWE1.CodeEPSG, EWE1.NomEPSG]);
    // systèmes de coordonnées
    EWE2 := FDocTopoAAjouter.GetCodeEPSGSystemeCoordonnees();
    lbSystemeCoordsImported.Caption := Format('EPSG:%d - %s', [EWE2.CodeEPSG, EWE2.NomEPSG]);
    pnlReprojection.Enabled := (EWE1.CodeEPSG <> EWE2.CodeEPSG);

    MyEntrance := FDocTopoAAjouter.GetEntrance(0);
    editX1.Value := MyEntrance.ePosition.X;
    editY1.Value := MyEntrance.ePosition.Y;
    // reprojection d'office
    if (EWE1.CodeEPSG = EWE2.CodeEPSG) then
    begin
      PO.U := MyEntrance.ePosition.X;  PO.V := MyEntrance.ePosition.Y;
      PC := FCoordConvertisseur.ConversionSyst1ToSyst2EPSG(EWE1.CodeEPSG, EWE2.CodeEPSG, PO);
      editX2.Value := PC.U;
      editY2.Value := PC.V;
    end
    else
    begin
      editX2.Value := MyEntrance.ePosition.X;
      editY2.Value := MyEntrance.ePosition.Y;
    end;
    // Point zéro du réseau ajouté
    editPtZeroReseauAddedSerie.AsInteger := 100000 + FDocTopoCourant.GetNbEntrances() + 1;
    editPtZeroReseauAddedPoint.AsInteger := 0;

  end;
end;

procedure TdlgAddSeriesFromOtherDoc.btnReprojeterClick(Sender: TObject);
var
  EPSGSrc, EPSGDest: TLabelSystemesCoordsEPSG;
  PDest, PScr: TProjUV;
begin
  EPSGSrc  := FDocTopoAAjouter.GetCodeEPSGSystemeCoordonnees();
  EPSGDest := FDocTopoCourant.GetCodeEPSGSystemeCoordonnees();
  if (EPSGSrc.CodeEPSG <> EPSGDest.CodeEPSG) then
  begin
    PScr.U := editX1.Value;
    PScr.V := editY1.Value;

    PDest := FCoordConvertisseur.ConversionSyst1ToSyst2EPSG(EPSGSrc.CodeEPSG, EPSGDest.CodeEPSG, PScr);
    editX2.Value := PDest.U;
    editY2.Value := PDest.V;
  end;
end;

procedure TdlgAddSeriesFromOtherDoc.btnImportClick(Sender: TObject);
begin
  if (not GHTopoQuestionOuiNon('Prêt à ajouter séries - Continuer')) then Exit;
  case PageControl1.ActivePageIndex of
    0: AjouterLesSeriesTopoDuJour();
    1: AjouterLaTopoImportee();
  end;
  ShowMessage(GetResourceString(rsDONE_ANY_PROCESS));
end;



procedure TdlgAddSeriesFromOtherDoc.chkCheckStationRattachementClick(Sender: TObject);
var
  MySerie: TObjSerie;
  QIdx: integer;
begin
  if (not FDocTopoCourant.HasPtTopoBySerSt(editSerieRaccordement.AsInteger, editPointRaccordement.AsInteger)) then
  begin
    ShowMessage(GetResourceString(rsMATCHNOTFOUND));
    exit;
  end;
  if (FDocTopoAAjouter.GetSerieByNumeroSerie(editSerieAAccrocher.AsInteger, MySerie, QIdx)) then
  begin
    case cmbExtrAccrochage.ItemIndex of
      0: MySerie.SetSeriePtDep(editSerieRaccordement.AsInteger, editPointRaccordement.AsInteger);
      1: MySerie.SetSeriePtArr(editSerieRaccordement.AsInteger, editPointRaccordement.AsInteger);
    end;
    CdrListeSeries1.Relister(0);
  end;
end;

procedure TdlgAddSeriesFromOtherDoc.editFirstNumSerieForSyntheseChange(Sender: TObject);
var
  EWE: LongInt;
begin
  EWE := editFirstNumSerieForSynthese.AsInteger;
  editFirstNumExpeForSynthese.AsInteger := EWE;
  editFirstNumCodeForSynthese.AsInteger := EWE;
end;

procedure TdlgAddSeriesFromOtherDoc.PageControl1Change(Sender: TObject);
begin
  pnlListeSeries.Enabled := (0 = PageControl1.ActivePageIndex);
end;





//******************************************************************************
// charger la topo
// Chargement de la topo
function TdlgAddSeriesFromOtherDoc.ChargerLaTopoExt(const FC: TStringDirectoryFileName): boolean;
begin
  Result := false;
  AfficherMessage(Format('%s.ChargerLaTopoExt: %s', [ClassName, FC]));
  if (not FileExistsUTF8(FC)) then Exit(false);
  FDocTopoAAjouter.ReInitialiser(True);

  FDocTopoAAjouter.SetDatabaseName('Doc_to_append');
  // vérifier si c'est un fichier Text
  if (Pos('.text', LowerCase(FC))>0) then
  begin
    if (FDocTopoAAjouter.LoadFichierText(FC) < 0) then
    begin
      //showmessage('gmj');
      AfficherMessageErreur('Le fichier comporte des erreurs - Voir les consoles');
      FDocTopoAAjouter.Finaliser;   // Echec = on détruit l'objet
      Exit;
    end;
  end
  else if (Pos('.gtx', FC) > 0) then // fichier GHTopo XML ?
  begin
    if (FDocTopoAAjouter.LoadFromXML(FC) < 0) then
    begin
      AfficherMessageErreur('Le fichier comporte des erreurs. Il doit être audité dans un éditeur XML');
      FDocTopoAAjouter.Finaliser();
      Exit;
    end;
  end
  else if (Pos('.txt', FC) > 0) then // fichier GHTopo XML ?
  begin
    if (FDocTopoAAjouter.LoadFromPocketTopoTXT(FC) < 0) then
    begin
      AfficherMessageErreur('Le fichier comporte des erreurs. Il doit être audité dans un éditeur XML');
      FDocTopoAAjouter.Finaliser();
      Exit;
    end;
  end
  else
  begin // sinon, c'est un fichier supposé Tab ou XTB
    if (FDocTopoAAjouter.LoadFromXTB(FC)<0) then
    begin
      AfficherMessageErreur('Le fichier comporte des erreurs - Voir le rapport');
      // Echec = on détruit l'objet
      FDocTopoAAjouter.Finaliser();
      Exit;
    end;
  end;
  // tout est OK ?

  self.Caption:= MakeTitleMainWindowGHTopo(ExtractFileName(FC));
  Application.ProcessMessages;
  Result := True;
end;

procedure TdlgAddSeriesFromOtherDoc.GetSerieAccrochage(const N: TNumeroSerie);
var
  SR: TObjSerie;
  QIdx: integer;

begin
  if (FDocTopoAAjouter.GetSerieByNumeroSerie(N, SR, QIdx)) then
  begin
    editSerieAAccrocher.AsInteger := SR.GetNumeroDeSerie();
    lbNomSerieSource.Caption      := SR.GetNomSerie();
  end;
end;

procedure TdlgAddSeriesFromOtherDoc.AjouterLaTopoImportee();
var
  i, Nb, v: Integer;
  MyEntrance: TEntrance;
  DoAddEntrances, DoCreateNewReseau, DoCreateNewSecteur: Boolean;
  StartIdxSerie   : TNumeroSerie;
  StartIdxCode    : TNumeroCode;
  StartIdxExpe    : TNumeroExpe;
  StartIdxReseau  : TNumeroReseau;
  StartIdxSecteur : TNumeroSecteur;
  StartIdxEntrance: TNumeroEntrance;
  MyReseau        : TReseau;
  MySecteur       : TSecteur;
  MyAntenne       : TViseeAntenne;
  MySerie, MySerieToAdd: TObjSerie;
  MyVisee: TUneVisee;
  MyCode: TCode;
  MyExpe: TExpe;
  EPSGSrc, EPSGDest: TLabelSystemesCoordsEPSG;
  PScr, PDest: TProjUV;

begin
  AfficherMessageErreur('AjouterLaTopoImportee');
  DoAddEntrances     := True; //chkAddEntrances.Checked;
  DoCreateNewReseau  := chkCreerNouveauReseau.Checked;
  DoCreateNewSecteur := True;

  StartIdxEntrance   := FDocTopoCourant.GetNbEntrances();
  StartIdxReseau     := FDocTopoCourant.GetNbReseaux();
  StartIdxSecteur    := FDocTopoCourant.GetNbSecteurs();

  // la question demandée pour ces trois valeurs est 'Index de départ du premier élément'
  // donc on retire 1 afin de retomber sur nos pattes
  // (la réponse usuelle à ce genre de question est un entier se terminant par 0)
  StartIdxSerie      := editFirstNumSerieForSynthese.AsInteger - 1;
  StartIdxCode       := editFirstNumCodeForSynthese.AsInteger  - 1;
  StartIdxExpe       := editFirstNumExpeForSynthese.AsInteger  - 1;
  if (DoAddEntrances) then
  begin
    Nb := FDocTopoAAjouter.GetNbEntrances();
    AfficherMessage(Format(' --> Adding %d entrances', [Nb]));
    for i := 0 to Nb - 1 do
    begin
      try
        MyEntrance := FDocTopoAAjouter.getEntrance(i);
        MyEntrance.eCouleur.setFrom(btnColorReseau.ButtonColor);
        MyEntrance.eRefSer := editPtZeroReseauAddedSerie.AsInteger;//100000 + FDocTopoCourant.GetNbEntrances() + i + 1;
        MyEntrance.eRefSt  := 0;
        // reprojection des coordonnées
        EPSGSrc  := FDocTopoAAjouter.GetCodeEPSGSystemeCoordonnees();
        EPSGDest := FDocTopoCourant.GetCodeEPSGSystemeCoordonnees();
        if (EPSGSrc.CodeEPSG <> EPSGDest.CodeEPSG) then
        begin
          PScr.U := MyEntrance.ePosition.X;
          PScr.V := MyEntrance.ePosition.Y;

          PDest := FCoordConvertisseur.ConversionSyst1ToSyst2EPSG(EPSGSrc.CodeEPSG, EPSGDest.CodeEPSG, PScr);
          MyEntrance.ePosition.X := PDest.U;
          MyEntrance.ePosition.Y := PDest.V;
        end;
        //*)
        FDocTopoCourant.AddEntrance(MyEntrance);

      except
        ShowMessage(Format('%s.Erreur dans AjouterLaTopoImportee()', [ClassName]));
      end;
    end;
  end;

  if (DoCreateNewReseau) then
  begin

    // on ajoute les réseaux
    Nb := FDocTopoAAjouter.GetNbReseaux();
    AfficherMessage(Format(' --> Adding %d reseaux', [Nb]));
    for i := 0 to Nb - 1  do
    begin
      MyReseau := FDocTopoAAjouter.GetReseau(i);
      if (0 = i) then
      begin
        MyReseau.NomReseau    := Trim(editNouveauReseau.Text);
        MyReseau.ColorReseau.setFrom(btnColorReseau.ButtonColor);
      end;
      FDocTopoCourant.AddReseau(MyReseau);
    end;
  end;
  if (DoCreateNewSecteur) then
  begin
    Nb := FDocTopoAAjouter.GetNbSecteurs();
    AfficherMessage(Format(' --> Adding %d secteurs', [Nb]));
    for i := 0 to Nb - 1  do
    begin
      MySecteur := FDocTopoAAjouter.GetSecteur(i);
      FDocTopoCourant.AddSecteur(MySecteur);
    end;
  end;
  // Ajout des antennes
  Nb := FDocTopoAAjouter.GetNbAntennes();
  AfficherMessage(Format(' --> Adding %d antennes', [Nb]));
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1  do
    begin
      MyAntenne := FDocTopoAAjouter.GetViseeAntenne(i);
      MyAntenne.EntranceRatt   += StartIdxEntrance;
      MyAntenne.SerieDepart    += StartIdxSerie;
      MyAntenne.Reseau         += StartIdxReseau;
      MyAntenne.Secteur        += StartIdxSecteur;
      FDocTopoCourant.AddViseeAntenne(MyAntenne);
    end;
  end;
  // Ajout des codes
  Nb := FDocTopoAAjouter.GetNbCodes();
  AfficherMessage(Format(' --> Adding %d codes', [Nb]));
  //StartIdxCode += 1;
  for i := 1 to Nb - 1  do
  begin
    MyCode := FDocTopoAAjouter.GetCode(i);
    MyCode.IDCode += StartIdxCode;
    FDocTopoCourant.AddCode(MyCode);
  end;
  //StartIdxExpe += 1;
  // Ajout des expés
  Nb := FDocTopoAAjouter.GetNbExpes();
  AfficherMessage(Format(' --> Adding %d seances', [Nb]));
  for i := 1 to Nb - 1  do
  begin
    MyExpe := FDocTopoAAjouter.GetExpe(i);
    MyExpe.IDExpe += StartIdxExpe;
    FDocTopoCourant.AddExpe(MyExpe);
  end;


  Nb := FDocTopoAAjouter.GetNbSeries();
  AfficherMessage(Format(' --> Adding %d series', [Nb]));
  if (Nb > 0) then
  begin
    MyReseau   := FDocTopoCourant.GetLastReseau();
    MyEntrance := FDocTopoCourant.GetLastEntrance();
    for i := 1 to Nb - 1  do
    begin
      // Ne pas ici utiliser la fonction  CreateAndCopySerie() en raison des traitements additionnels indiqués par '+++'
      MySerie  := FDocTopoAAjouter.GetSerie(i);
      MySerieToAdd := TObjSerie.Create;
      try
        MySerieToAdd.ClearStations();                                                                        // Opération 1: Vider la liste des visées
        MySerieToAdd.SetChanceObstacle(MySerie.GetChance(), MySerie.GetObstacle());                          // Opération 2: Chances et obstacles
        MySerieToAdd.SetSeriePtExtremites(MySerie.GetNoSerieDep() + StartIdxSerie, MySerie.GetNoPointDep(),  // Opération 3: Extrémités de la série
                                          MySerie.GetNoSerieArr() + StartIdxSerie, MySerie.GetNoPointArr());
        MySerieToAdd.SetNumeroSerie(MySerie.GetNumeroDeSerie()      + StartIdxSerie);           // +++       // Opération 4: Numéro de série
        MySerieToAdd.SetNomObsSerie(MySerie.GetNomSerie(), MySerie.GetObsSerie());                           // Opération 5: Nom et observations sur la série
        MySerieToAdd.SetCouleur(MySerie.GetCouleur());                                                       // Opération 6: Raideur
        MySerieToAdd.SetRaideur(MySerie.GetRaideur());                                                       // Opération 7: Couleur

        MySerieToAdd.SetNumeroEntrance(MySerie.GetNumeroEntrance()  + StartIdxEntrance);        // +++       // Opération 8: Entrée de rattachement
        MySerieToAdd.SetNumeroReseau(MySerie.GetNumeroReseau()      + StartIdxReseau);          // +++       // Opération 9: Réseau de rattachement

        // Opération 10: Traitement des visées
        if (1 = MySerie.GetNumeroDeSerie()) then // +++
        begin
          MySerieToAdd.SetSeriePtDep(editPtZeroReseauAddedSerie.AsInteger, editPtZeroReseauAddedPoint.AsInteger);
        end;
        for v := 0 to MySerie.GetNbVisees() - 1 do // +++
        begin
          MyVisee := MySerie.GetVisee(v);
          MyVisee.IDSecteur += StartIdxSecteur;
          MyVisee.Code      += StartIdxCode;
          MyVisee.Expe      += StartIdxExpe;
          MySerieToAdd.AddVisee(MyVisee);
        end;
        FDocTopoCourant.AddSerie(MySerieToAdd);
      except
        FreeAndNil(MySerieToAdd);
      end;
    end;
  end;
end;

end.

