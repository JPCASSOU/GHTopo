unit frmMaintenanceDocument;
// 13/06/2019: Point de contrôle temporel (contrôle de version)

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils,
  StructuresDonnees,
  Common,
  CallDialogsStdVersion,
  ToporobotClasses2012,
  UnitEntitesExtended,
  CadreViseesAntenne,
  CadreListeSeries,
  CadreListesSimples,
  //UnitClassPalette,
  UnitObjetSerie,
  ConvertisseurJPC,
  ExportationSQL,
  FileUtil, curredit, Forms, Controls, Graphics, Dialogs, Buttons, ComCtrls, ExtCtrls, StdCtrls, EditBtn, Types;

type TAttributsAChanger = set of (aacENTRANCES, aacRESEAUX, aacCODES, aacEXPES, aacSECTEURS);
type TdlgMaintenanceDocTopo = class(TForm)
    BitBtn1: TBitBtn;
    btnReattribuerAuxSeriesSelectionnees: TButton;
    btnReattribuerCode: TButton;
    btnReattribuerIDExpe: TButton;
    btnReattribuerIDSerie: TButton;
    btnModifier: TButton;
    btnSelectCode: TButton;
    btnSelectExpe: TButton;
    btnSelectReseau: TButton;
    btnSelectEntrance: TButton;
    btnSupprimerSeriesSelectionnees: TButton;
    Button1: TButton;
    Button2: TButton;
    btnSystemeCorrdonnees: TButton;
    Button3: TButton;
    CdrAntennes1: TCdrAntennes;
    CdrListeSeries1: TCdrListeSeries;
    CdrListesSimples1: TCdrListesSimples;
    chkStructureOnly: TCheckBox;
    chkDoEntrances: TCheckBox;
    chkDoCodes: TCheckBox;
    chkDoSeances: TCheckBox;
    chkDoReseaux: TCheckBox;
    editNameSpace: TEdit;
    editCurrNumeroCode: TCurrencyEdit;
    editCurrNumeroExpe: TCurrencyEdit;
    editCurrNumeroReseau: TCurrencyEdit;
    editCurrNumeroEntree: TCurrencyEdit;
    editNomEtude: TEdit;
    editAncienIDCode: TCurrencyEdit;
    editAncienIDExpe: TCurrencyEdit;
    editAncienIDSerie: TCurrencyEdit;
    editNouvelIDCode: TCurrencyEdit;
    editNouvelIDExpe: TCurrencyEdit;
    editNouvelIDSerie: TCurrencyEdit;
    editSQLFilename: TFileNameEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlExportSQL: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    lbMessages: TStaticText;
    tabShtListesSimples: TTabSheet;
    tabshtSeries: TTabSheet;
    tabShtAntennes: TTabSheet;
    tabshtGeneral: TTabSheet;
    procedure btnModifierClick(Sender: TObject);
    procedure btnReattribuerAuxSeriesSelectionneesClick(Sender: TObject);
    procedure btnReattribuerCodeClick(Sender: TObject);
    procedure btnReattribuerIDExpeClick(Sender: TObject);
    procedure btnReattribuerIDSerieClick(Sender: TObject);
    procedure btnSelectCodeClick(Sender: TObject);
    procedure btnSelectEntranceClick(Sender: TObject);
    procedure btnSelectExpeClick(Sender: TObject);
    procedure btnSelectReseauClick(Sender: TObject);
    procedure btnSupprimerSeriesSelectionneesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnSystemeCorrdonneesClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FDocTopo: TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FConvertisseurCoords: TConversionSysteme;
    FDoPostAction: boolean;
    procedure GenererSubsetXTB(const QFilename: TStringDirectoryFilename);
    function GetAttributsAChanger(): TAttributsAChanger;
    procedure ReattribuerEntreeReseauCodeExpe(const AttributsAChanger: TAttributsAChanger;
                                              const QIdxEntrance: TNumeroEntrance;
                                              const QIdxReseau  : TNumeroReseau;
                                              const QIdxCode    : TNumeroCode;
                                              const QIdxExpe    : TNumeroExpe);
    // reprojection des entrées
    procedure ReprojeterLesEntrees(const OldEPSG, NewEPSG: TLabelSystemesCoordsEPSG);
  public
    function  Initialiser(const FD: TToporobotStructure2012;
                          const FE: TBDDEntites;
                          const FC: TConversionSysteme): boolean;
    procedure Finaliser();
    function  DoPostAction(const BoolDummy: boolean): boolean;


  end;

var
  dlgMaintenanceDocTopo: TdlgMaintenanceDocTopo;

implementation

{$R *.lfm}
uses
  DGCDummyUnit;

{ TdlgMaintenanceDocTopo }



procedure TdlgMaintenanceDocTopo.ReattribuerEntreeReseauCodeExpe(const AttributsAChanger: TAttributsAChanger;
                                                                 const QIdxEntrance: TNumeroEntrance;
                                                                 const QIdxReseau  : TNumeroReseau;
                                                                 const QIdxCode    : TNumeroCode;
                                                                 const QIdxExpe    : TNumeroExpe);
var
  i, NbSeries, NbVisees, j: Integer;
  MySerie    : TObjSerie;
  MyVisee: TUneVisee;
  EWE: String;
begin
  if (not GHTopoQuestionOuiNon('Cette fonction peut désorganiser la base - Continuer ?')) then Exit;
  EWE := format('%s.ReattribuerEntreeReseauCodeExpe(%d, %d, %d, %d', [ClassName, QIdxEntrance, QIdxReseau, QIdxCode, QIdxExpe]);

  // on checke les numéros de expé, code, réseau
  if (not FDocTopo.ExistsIdxEntrance(QIdxEntrance)) then Exit;
  if (not FDocTopo.ExistsIdxReseau(QIdxReseau)) then Exit;
  if (not FDocTopo.ExistsIdxCode(QIdxCode)) then Exit;
  if (not FDocTopo.ExistsIdxExpe(QIdxExpe)) then Exit;
  NbSeries := FDocTopo.GetNbSeries();
  for i := 0 to NbSeries - 1 do
  begin
    if (CdrListeSeries1.IsItemListeSelected(i)) then
    begin
      MySerie := FDocTopo.GetSerie(i);
      NbVisees := MySerie.GetNbVisees();
      lbMessages.Caption := Format('--> %d: %d (%d stations): %s', [i, MySerie.GetNumeroDeSerie(), NbVisees, MySerie.GetNomSerie()]);

      if (aacENTRANCES   in AttributsAChanger) then MySerie.SetNumeroEntrance(QIdxEntrance);
      if (aacRESEAUX     in AttributsAChanger) then MySerie.SetNumeroReseau(QIdxReseau);
      if (aacCODES       in AttributsAChanger) then
      begin
        for j := 0 to NbVisees - 1 do
        begin
          MyVisee        := MySerie.GetVisee(j);
          MyVisee.Code   := QIdxCode;
          MySerie.PutVisee(j, MyVisee);
        end;
      end;
      if (aacEXPES       in AttributsAChanger) then
      begin
        for j := 0 to NbVisees - 1 do
        begin
          MyVisee        := MySerie.GetVisee(j);
          MyVisee.Expe   := QIdxExpe;
          MySerie.PutVisee(j, MyVisee);
        end;
      end;
    end;
  end;
end;



procedure TdlgMaintenanceDocTopo.GenererSubsetXTB(const QFilename: TStringDirectoryFilename);
var
  MySubsetXTB: TToporobotStructure2012;
  P1: TPoint3Df;
  EEE: TEntrance;
  WU: String;
  Nb, i, NbSeriesSelectionnees: Integer;
  MySerieCible , MySerieSource: TObjSerie;
  CDS: TGHTopoColor;
begin
  AfficherMessage(format('%s.GenererSubsetXTB(%s)', [ClassName, QFilename]));
  MySubsetXTB := TToporobotStructure2012.Create;
  try
    {$WARNING Ne pas utiliser TToporobotStructure2012.ReInitialiser() ici}
    // MySubsetXTB.ReInitialiser(True);
    MySubsetXTB.SetDatabaseName(QFilename);
    MySubsetXTB.SetCodeEPSGSystemeCoordonnees(FDocTopo.GetCodeEPSGSystemeCoordonnees());
    MySubsetXTB.SetNomEtude('Sous-ensemble de ' + FDocTopo.GetNomEtude());
    P1.Empty();
    MySubsetXTB.SetDefaultCoords(P1);
    WU := ''; // mettre ici les numéros de séries extraites;
    MySubsetXTB.SetCommentairesEtude(WU);
    MySubsetXTB.ClearListeSeries();
    MySubsetXTB.ViderListeViseesAntennes();
    MySubsetXTB.ViderTablesSimples();


    // création de l'entrée 1.0
    CDS.setFrom(clRed);
    EEE.setFrom('Point_0', '1.0', P1, 1, 0, CDS,'Point de rattachement');
    MySubsetXTB.AddEntrance(EEE);
    Nb := FDocTopo.GetNbReseaux();  // les réseaux
    for i := 0 to Nb - 1 do MySubsetXTB.AddReseau(FDocTopo.GetReseau(i));
    Nb := FDocTopo.GetNbSecteurs();// les secteurs
    for i := 0 to Nb - 1 do MySubsetXTB.AddSecteur(FDocTopo.GetSecteur(i));
    Nb := FDocTopo.GetNbCodes();  // les codes
    for i := 0 to Nb - 1 do MySubsetXTB.AddCode(FDocTopo.GetCode(i));
    Nb := FDocTopo.GetNbExpes();   // lex expés
    for i := 0 to Nb - 1 do MySubsetXTB.AddExpe(FDocTopo.GetExpe(i));

    // les séries (uniquement les séries sélectionnées)
    Nb := CdrListeSeries1.lsbListe.Count;
    {$NOTE: La ligne suivante est inutile: GHTopo reconstruira une série 0 au chargement}
    //FDocTopo.CreateNewSerie(1,0, -1, 'serie 0'); //TODO: Semble inutile A voir

    // première série
    if (CreateAndCopySerie(FDocTopo.GetSerie(1), MySerieCible)) then MySubsetXTB.AddSerie(MySerieCible);
    NbSeriesSelectionnees := 0;
    for i := 0 to Nb - 1 do
    begin
      if (CdrListeSeries1.IsItemListeSelected(i)) then
      begin
        Inc(NbSeriesSelectionnees);
        if (CreateAndCopySerie(FDocTopo.GetSerie(i), MySerieCible)) then MySubsetXTB.AddSerie(MySerieCible);
      end;
    end;
    MySubsetXTB.SaveToXTB(QFilename, mtabEXTENDEDTAB, tfWINDOWS);
    // finalisation
    MySubsetXTB.Finaliser();
    // La série 1 du document original est détruite . Why ??? Réponse: Toujours ce pb de pointeur avec les TObjSerie. Résolu par CreateAndCopySerie();
    //ShowMessageFmt('Après  MySubsetXTB.Finaliser(): Série %d problématique: %s - %d', [MySerieSource.GetNumeroDeSerie(), MySerieSource.GetNomSerie(), MySerieSource.GetNbVisees()]);
  finally
    FreeAndNil(MySubsetXTB);
  end;
end;

procedure TdlgMaintenanceDocTopo.btnReattribuerIDExpeClick(Sender: TObject);
var
  E1, E2: TNumeroExpe;
begin
  E1 := editAncienIDExpe.AsInteger;
  E2 := editNouvelIDExpe.AsInteger;
  FDocTopo.ReplaceIdxExpes(E1, E2);
  FDocTopo.ReplaceIndexInStations(E1, E2, mrsEXPES);
  FDocTopo.ReattribuerCodesExpesViseesAntennes();
end;

procedure TdlgMaintenanceDocTopo.btnReattribuerIDSerieClick(Sender: TObject);
var
  E2, E1: TNumeroSerie;
  MySerie: TObjSerie;
  QIdx: integer;
begin
  E1 := editAncienIDSerie.AsInteger;
  E2 := editNouvelIDSerie.AsInteger;
  if (FDocTopo.GetSerieByNumeroSerie(E2, MySerie, QIdx)) then
  begin
    //FDocTopo.GetSerieByNumeroSerie(E2, MySerie, QIdx);
    ShowMessageFmt('Série %d déjà attribuée: "%s"', [MySerie.GetNumeroDeSerie(), MySerie.GetNomSerie()]);
    exit;
  end;
  FDocTopo.ReplaceIndexSeriesInEntrances(E1, E2);
  FDocTopo.ReplaceIdxSeries(E1, E2);
  //FDocTopo.ReplaceIndexInAntennes(E1, E2, [mraEXPES]);
  FDocTopo.ReattribuerCodesExpesViseesAntennes();
  CdrListeSeries1.Relister(0);
end;

procedure TdlgMaintenanceDocTopo.btnSelectCodeClick(Sender: TObject);
var
  n: LongInt;
begin
  n := editCurrNumeroCode.AsInteger;
  if (SelectionDansListe(FDocTopo, mslCODE, false, n)) then  editCurrNumeroCode.AsInteger := n;
end;

procedure TdlgMaintenanceDocTopo.btnSelectEntranceClick(Sender: TObject);
var
  n: LongInt;
begin
  n := editCurrNumeroReseau.AsInteger;
  if (SelectionDansListe(FDocTopo, mslENTRANCES, false, n)) then  editCurrNumeroEntree.AsInteger := n;
end;

procedure TdlgMaintenanceDocTopo.btnSelectExpeClick(Sender: TObject);
var
  n: LongInt;
begin
  n := editCurrNumeroExpe.AsInteger;
  if (SelectionDansListe(FDocTopo, mslEXPE, false, n)) then  editCurrNumeroExpe.AsInteger := n;
end;

procedure TdlgMaintenanceDocTopo.btnSelectReseauClick(Sender: TObject);
var
  n: LongInt;
begin
  n := editCurrNumeroReseau.AsInteger;
  if (SelectionDansListe(FDocTopo, mslRESEAUX, false, n)) then  editCurrNumeroReseau.AsInteger := n;
end;

procedure TdlgMaintenanceDocTopo.btnSupprimerSeriesSelectionneesClick(Sender: TObject);
var
  n, i: Integer;
begin
  n := FDocTopo.GetNbSeries();
  if (not GHTopoQuestionOuiNon('Ceci supprimera DEFINITIVEMENT les séries sélectionnées - Continuer ?')) then exit;
  for i := n - 1 downto 1 do
  begin
    if (CdrListeSeries1.IsItemListeSelected(i)) then FDocTopo.RemoveSerie(i);
  end;
  CdrListeSeries1.Relister(0);
end;

procedure TdlgMaintenanceDocTopo.Button1Click(Sender: TObject);
begin

end;

procedure TdlgMaintenanceDocTopo.Button2Click(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QIdxFilter: integer;
  MyTextEditor: String;
begin
  if (not GHTopoQuestionOuiNon('Fonctionnalité expérimentale' + #13#10 +
                               'Le fichier XTB généré peut devoir être modifié' + #13#10 +
                               'Ceci ouvrira le fichier résultant dans un éditeur de texte '  + #13#10 +
                               'Continuer')) then Exit;
  QFileName := GetGHTopoDirectory() + 'Subset_001.xtb';
  if (DoDialogSaveFile('Fichier XTB', '.xtb', QFileName, QIdxFilter)) then
  begin
    GenererSubsetXTB(QFileName);
    MyTextEditor := 'C:\Program Files\Notepad++\notepad++.exe';
    RunExternalProgram(MyTextEditor, '', QFilename);
  end;

end;

procedure TdlgMaintenanceDocTopo.btnSystemeCorrdonneesClick(Sender: TObject);
var
  OldEPSG, NewEPSG: TLabelSystemesCoordsEPSG;
begin
  OldEPSG := FDocTopo.GetCodeEPSGSystemeCoordonnees();
  if (SelectCoordinatesSystem(FConvertisseurCoords, NewEPSG)) then
  begin
    if (NewEPSG.CodeEPSG <> OldEPSG.CodeEPSG) then
    begin
      FDocTopo.SetCodeEPSGSystemeCoordonnees(NewEPSG);
      // acquittement
      NewEPSG := FDocTopo.GetCodeEPSGSystemeCoordonnees();
      btnSystemeCorrdonnees.Caption := Format('EPSG:%d - %s',[NewEPSG.CodeEPSG, NewEPSG.NomEPSG]);
      if (GHTopoQuestionOuiNon('Reprojeter les coordonnées des entrées')) then
      begin
        ReprojeterLesEntrees(OldEPSG, NewEPSG);
        FDoPostAction := true;
      end ;
    end;
  end;

end;

procedure TdlgMaintenanceDocTopo.Button3Click(Sender: TObject);
var
  MyExportSQL: TExportationSQL;
begin
  MyExportSQL := TExportationSQL.Create;
  try
    if (MyExportSQL.Initialiser(FDocTopo, chkStructureOnly.Checked, editSQLFilename.FileName)) then
    begin
      if (MyExportSQL.CreateDatabase('MyDatabase001')) then
      begin
        MyExportSQL.GenererTableEntrances('TableEntrances');
        MyExportSQL.GenererTableCodes('TableCodes');
        MyExportSQL.GenererTableExpes('TableExpes');
        MyExportSQL.GenererTableSeries('TableSeries');

        MyExportSQL.GenererTableVisees('TableVisees');
        MyExportSQL.GenererTableAntennes('TableAntennes');
        MyExportSQL.CloseDatabase();
      end;

      MyExportSQL.Finaliser;
    end;
    showmessage('Terminé');
  finally
    FreeAndNil(MyExportSQL);
  end;
end;

procedure TdlgMaintenanceDocTopo.btnReattribuerCodeClick(Sender: TObject);
var
  E1, E2: TNumeroCode;
begin
  E1 := editAncienIDCode.AsInteger;
  E2 := editNouvelIDCode.AsInteger;
  FDocTopo.ReplaceIdxCodes(E1, E2);
  FDocTopo.ReplaceIndexInStations(E1, E2, mrsCODES);
  FDocTopo.ReattribuerCodesExpesViseesAntennes();
end;

procedure TdlgMaintenanceDocTopo.btnModifierClick(Sender: TObject);
var
  MyNamespace: TNameSpace;
begin
  MyNamespace := FDocTopo.GetNameSpace(0);
  MyNamespace.Nom := trim(editNameSpace.Text);
  FDocTopo.PutNameSpace(0, MyNamespace);
  FDocTopo.SetNomEtude(trim(editNomEtude.Text));
  self.Caption := 'Maintenance du document: ' + FDocTopo.GetNomEtude();
  (*
  OldEPSG := FDocTopo.GetCodeEPSGSystemeCoordonnees();
  NewEPSG := FConvertisseurCoords.GetCodeEPSGNomSysteme(cmbCodesEPSG.ItemIndex);
  if (NewEPSG.CodeEPSG <> OldEPSG.CodeEPSG) then
  begin
    if (GHTopoQuestionOuiNon('Ceci reprojettera les coordonnées des entrées - Continuer')) then
    begin
      FDocTopo.SetCodeEPSGSystemeCoordonnees(NewEPSG);
      ReprojeterLesEntrees(OldEPSG, NewEPSG);
      FDoPostAction := true;
    end
    else
    begin
      cmbCodesEPSG.ItemIndex := FConvertisseurCoords.GetIndexSystemeByCodeEPSG(OldEPSG.CodeEPSG);
    end;
  end;
  //*)
end;

function TdlgMaintenanceDocTopo.GetAttributsAChanger(): TAttributsAChanger;
begin
  Result := [];
  if (chkDoEntrances.Checked) then Result := Result + [aacENTRANCES] else Result := Result - [aacENTRANCES];
  if (chkDoReseaux.Checked)   then Result := Result + [aacRESEAUX]   else Result := Result - [aacRESEAUX];
  if (chkDoCodes.Checked)     then Result := Result + [aacCODES]     else Result := Result - [aacCODES];
  if (chkDoSeances.Checked)   then Result := Result + [aacEXPES]     else Result := Result - [aacEXPES];
end;

procedure TdlgMaintenanceDocTopo.btnReattribuerAuxSeriesSelectionneesClick(Sender: TObject);
var
  EWE: TAttributsAChanger;
begin
  EWE :=  GetAttributsAChanger();
  ReattribuerEntreeReseauCodeExpe(EWE,
                                  editCurrNumeroEntree.AsInteger, editCurrNumeroReseau.AsInteger,
                                  editCurrNumeroCode.AsInteger, editCurrNumeroExpe.AsInteger);
  CdrListeSeries1.Relister(0);
end;


function TdlgMaintenanceDocTopo.Initialiser(const FD: TToporobotStructure2012;
                                            const FE: TBDDEntites;
                                            const FC: TConversionSysteme): boolean;
var
  n, i: Integer;
  MyCode: TCode;
  MyExpe: TExpe;
  MyEPSG: TLabelSystemesCoordsEPSG;
  MyNameSpace: TNameSpace;
begin
  result := false;
  try
    FDoPostAction := false;
    FDocTopo := FD;
    FBDDEntites := FE;
    FConvertisseurCoords := FC;
    self.Caption := 'Maintenance du document: ' + FDocTopo.GetNomEtude();
    editNomEtude.Text   := FDocTopo.GetNomEtude();
    // espace de nom principal
    MyNameSpace := FDocTopo.GetNameSpace(0);
    editNameSpace.Text := MyNameSpace.Nom;
    CdrListesSimples1.InitialiserListeSimple(FDocTopo, FBDDEntites, nil, mbddCODES);
    CdrAntennes1.Initialise(FD);
    CdrListeSeries1.Initialiser(FD, nil, nil, [tbsSORT, tbsCSV, tbsHELP], True);
    // systèmes de coordonnées
    MyEPSG := FDocTopo.GetCodeEPSGSystemeCoordonnees();
    btnSystemeCorrdonnees.Caption := Format('EPSG:%d - %s',[MyEPSG.CodeEPSG, MyEPSG.NomEPSG]);
    // derniers items: code, expé, réseau
    MyCode   := FDocTopo.GetLastCode();
    MyExpe   := FDocTopo.GetLastExpe();
    editCurrNumeroExpe.AsInteger := MyExpe.IDExpe;
    editCurrNumeroCode.AsInteger := MyCode.IDCode;
    editCurrNumeroReseau.AsInteger := FDocTopo.GetNbReseaux() - 1;
    editSQLFilename.InitialDir := GetGHTopoDirectory();
    editSQLFilename.FileName   := 'SQLExport001.sql';

    result := True;
  finally
  end;
end;

procedure TdlgMaintenanceDocTopo.ReprojeterLesEntrees(const OldEPSG, NewEPSG: TLabelSystemesCoordsEPSG);
var
  EWE: String;
  Nb, i: Integer;
  MyEntrance: TEntrance;
  SrcPoint, DestPoint: TProjUV;
begin
  EWE := Format('%s.ReprojeterLesEntrees: EPSG:%d -> EPSG:%d', [ClassName, OldEPSG.CodeEPSG, NewEPSG.CodeEPSG]);
  AfficherMessage(EWE);
  Nb := FDocTopo.GetNbEntrances();
  for i := 0 to Nb - 1 do
  begin
    MyEntrance := FDocTopo.GetEntrance(i);
    SrcPoint.U := MyEntrance.ePosition.X;
    SrcPoint.V := MyEntrance.ePosition.Y;

    DestPoint := FConvertisseurCoords.ConversionSyst1ToSyst2EPSG(OldEPSG.CodeEPSG, NewEPSG.CodeEPSG, SrcPoint);
    MyEntrance.ePosition.X := DestPoint.U;
    MyEntrance.ePosition.Y := DestPoint.V;
    FDocTopo.PutEntrance(i, MyEntrance);
  end;
  // et on redéfinit le point zéro
  MyEntrance := FDocTopo.GetEntrance(0);
  FDocTopo.SetRefSeriePoint(MyEntrance.eRefSer, MyEntrance.eRefSt);
  FDocTopo.SetDefaultCoords(MyEntrance.ePosition.X, MyEntrance.ePosition.Y, MyEntrance.ePosition.Z);

end;

procedure TdlgMaintenanceDocTopo.Finaliser();
begin
  pass;
end;

function TdlgMaintenanceDocTopo.DoPostAction(const BoolDummy: boolean): boolean;
begin
  result := FDoPostAction;
end;

end.

