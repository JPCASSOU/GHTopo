unit dlgStatistiquesExt;
// 09/06/2015: i18n + réactivation diagramme en barres
// 29/02/2016: Formattage OOo, inactivation des valeurs aberrantes, i18n, corrections
// 02/01/2017: Refonte du look, adaptation au 800x480
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  ToporobotClasses2012,
  UnitEntitesExtended,
  unitUtilsComposants,
  CallDialogsStdVersion,
  Common,
  CadreRoseDiagramme,
  CadreDepthDiagramme,
  Classes, SysUtils, FileUtil, curredit, Clipbrd,
  SynEdit,
  SynPluginSyncroEdit, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Grids, ExtCtrls, Buttons, Types, LCLType;

type

{ TfrmStatistiquesExt }

 TfrmStatistiquesExt = class(TForm)
    BitBtn1: TBitBtn;
    btnColorBarresFill: TColorButton;
    btnColorBarresLine: TColorButton;
    btnCopyTableauRecap: TButton;
    btnCopyTableauCoords: TButton;
    btnCopyTableauEntrees: TButton;
    btnCopyTableauStatsReseaux: TButton;
    btnRedrawDiagrams: TButton;
    btnExportRoseDiagramSVG: TButton;
    btnExportDepthDiagramSVG: TButton;
    btnCopier: TButton;
    CdrDepthDiagramme1: TCdrDepthDiagramme;
    CdrRoseDiagramme1: TCdrRoseDiagramme;
    cmbAffichageTableaux: TComboBox;
    cmbReseauOuSecteur: TComboBox;
    editFiltres: TEdit;
    editLongViseeMini: TCurrencyEdit;
    editNbPetales: TCurrencyEdit;
    editNbBarresDepth: TCurrencyEdit;
    grdSyntheseSpeleometrique: TStringGrid;
    grbxEtendueReseau: TGroupBox;
    grbxDeveloppements: TGroupBox;
    HeaderControl1: THeaderControl;
    HeaderControl2: THeaderControl;
    Label7: TLabel;
    Label8: TLabel;
    lbDevelTopal: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbDenivTopal: TLabel;
    lbDevTotal: TLabel;
    lbDevNaturels: TLabel;
    lbDevFossiles: TLabel;
    lbDevArtificiels: TLabel;
    lbDevSpecials: TLabel;
    lbDevTunnels: TLabel;
    lbDevMines: TLabel;
    lbMetrageTotal: TStaticText;
    lbMetrageGeneral: TStaticText;
    lbMetrageFossile: TStaticText;
    lbDeniveleTotal: TStaticText;
    lbMetrageVadoses: TStaticText;
    lbMetrageInondable: TStaticText;
    lbMetrageSiphon: TStaticText;
    lbMetrageSpeciaux: TStaticText;
    lbMetrageTunnels: TStaticText;
    lbMetrageMines: TStaticText;
    lbDevSpeciaux: TLabel;
    lbDevVadoses: TLabel;
    lbDevInondables: TLabel;
    lbDevSiphons: TLabel;
    lbStationYMini: TStaticText;
    lbStationZMini: TStaticText;
    lbStationXMaxi: TStaticText;
    lbStationYMaxi: TStaticText;
    lbStationZMaxi: TStaticText;
    lbXMini: TStaticText;
    lbStationXMini: TStaticText;
    lbYMini: TStaticText;
    lbZMini: TStaticText;
    lbXMaxi: TStaticText;
    lbYMaxi: TStaticText;
    lbZMaxi: TStaticText;
    lbEtendueX: TStaticText;
    lbEtendueY: TStaticText;
    lbEtendueZ: TStaticText;
    lbNbEntrances: TLabel;
    lbNbStations: TLabel;
    Label2: TLabel;
    lbLongMiniVisee1: TLabel;
    lbNbBarres: TLabel;
    lbNbPetales: TLabel;
    lbNbReseauxSecteurs: TLabel;
    lbNbItems: TLabel;
    lsbVisees: TListBox;
    lsbEntrances: TListBox;
    pagectlHistogrammes: TPageControl;
    pnlRoseDiagram: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    grdStatsReseauxSecteurs: TStringGrid;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    TabCtrlStatistiques: TPageControl;
    tabshtRoseDiagram: TTabSheet;
    tabShtDepthHisto: TTabSheet;
    tabShtReseauSecteur: TTabSheet;
    tabShtEntrances: TTabSheet;
    tabShtSynthese: TTabSheet;
    tabShtCoordonnees: TTabSheet;
    tabShtDiagrams: TTabSheet;
    tabShtTableaux: TTabSheet;
    procedure btnColorBarresFillColorChanged(Sender: TObject);
    procedure btnColorBarresLineColorChanged(Sender: TObject);
    procedure btnCopierClick(Sender: TObject);
    procedure btnCopyTableauEntreesClick(Sender: TObject);
    procedure btnCopyTableauRecapClick(Sender: TObject);
    procedure btnCopyTableauCoordsClick(Sender: TObject);
    procedure btnCopyTableauStatsReseauxClick(Sender: TObject);
    procedure btnExportDepthDiagramSVGClick(Sender: TObject);
    procedure btnRedrawDiagramsClick(Sender: TObject);
    procedure btnExportRoseDiagramSVGClick(Sender: TObject);
    procedure cmbAffichageTableauxChange(Sender: TObject);
    procedure cmbReseauOuSecteurChange(Sender: TObject);
    procedure editFiltresChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grdSyntheseSpeleometriqueDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure HeaderControl2SectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbEntrancesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbViseesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure sclPourcentageSeuilHightLightChange(Sender: TObject);
  private
    FTabSpeleoParReseaux : TArraySpeleometrieReseauOuSecteur;
    FTabSpeleoParSecteurs: TArraySpeleometrieReseauOuSecteur;
    FDocTopo   : TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FOldFiltre : string;
    FModeAffichageTableau: TModeSelectionListe;
    procedure ListerCoordonneesStations();
    procedure ListerCoordonneesEntrances();
    procedure InitialiseStats();
    procedure AfficherInfosBDD();
    procedure RedessinerLesDiagrammes();
    procedure RefreshTableau(const QMode: TModeSelectionListe);
    procedure RefresTableauReseauxSecteurs(const QMode: integer);
  public
    function InitialiseByPointer(const QDTT: TToporobotStructure2012;
                                 const QBDD: TBDDEntites;
                                 const QFiltres: string): boolean;
    procedure Finaliser();


end;
var
  frmStatistiquesExt: TfrmStatistiquesExt;

implementation
uses
  DGCDummyUnit;

const
  COLOR_ALTERNATE_ROW: TColor = $00FEFEE4;
  LSB_ITEM_HEIGHT = 20;
{$R *.lfm}

{ TfrmStatistiquesExt }
procedure TfrmStatistiquesExt.InitialiseStats();
begin
  try
    AfficherMessage('Etablissement des statistiques');
    FBDDEntites.SetMinMax(False);
    FBDDEntites.RecenserDates;
    cmbReseauOuSecteur.ItemIndex := 0;
    FBDDEntites.CalculSpeleometrie;
    RefreshTableau(mslRESEAUX);
    FBDDEntites.CalcSpeleometrieParReseauxOuSecteurs(0, FTabSpeleoParReseaux);
    FBDDEntites.CalcSpeleometrieParReseauxOuSecteurs(1, FTabSpeleoParSecteurs);
    RefresTableauReseauxSecteurs(0);
    AfficherInfosBDD();
    TabCtrlStatistiques.ActivePageIndex := 0;
    cmbAffichageTableaux.ItemIndex := 0;
  finally
  end;
end;

procedure TfrmStatistiquesExt.btnCopierClick(Sender: TObject);
var
  MyClipboard: TClipboard;
  FR: TVentilationSpeleometrie;
  WU: String;
  C1, C2: TPoint3Df;
begin
  FR := FBDDEntites.GetRecapitulation();
  C1 := FBDDEntites.GetCoinBasGauche();
  C2 := FBDDEntites.GetCoinHautDroit();
  MyClipboard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    WU := lbDevelTopal.Caption       + TAB + FormatterNombreOOo(FR.Fossiles + FR.Ennoyables + FR.Siphons + FR.Vadoses) + #13#10;
    WU += lbDenivTopal.Caption       + TAB + FormatterNombreOOo(C2.Z - C1.Z) + #13#10;
    WU += #13#10;
    WU += lbMetrageFossile.Caption   + TAB + FormatterNombreOOo(FR.Fossiles)   + #13#10;
    WU += lbMetrageVadoses.Caption   + TAB + FormatterNombreOOo(FR.Vadoses)    + #13#10;
    WU += lbMetrageInondable.Caption + TAB + FormatterNombreOOo(FR.Ennoyables) + #13#10;
    WU += lbMetrageSiphon.Caption    + TAB + FormatterNombreOOo(FR.Siphons)    + #13#10;
    WU += #13#10;
    WU += lbMetrageSpeciaux.Caption  + TAB + FormatterNombreAvecSepMilliers(FR.Speciaux) + #13#10;
    WU += lbMetrageTunnels.Caption   + TAB + FormatterNombreAvecSepMilliers(FR.Tunnels)  + #13#10;
    WU += lbMetrageMines.Caption     + TAB + FormatterNombreAvecSepMilliers(FR.Filons)   + #13#10;
    WU += #13#10;
    WU += 'X ' + TAB + FormatterNombreOOo(C1.X) + TAB + FBDDEntites.GetIDStationXMini() + TAB +
                       FormatterNombreOOo(C2.X - C1.X) + TAB +
                       FormatterNombreOOo(C2.X) + TAB + FBDDEntites.GetIDStationXMaxi() +  #13#10;
    WU += 'Y ' + TAB + FormatterNombreOOo(C1.Y) + TAB + FBDDEntites.GetIDStationYMini() + TAB +
                       FormatterNombreOOo(C2.Y - C1.Y) + TAB +
                       FormatterNombreOOo(C2.Y) + TAB + FBDDEntites.GetIDStationYMaxi() +  #13#10;
    WU += 'Z ' + TAB + FormatterNombreOOo(C1.Z) + TAB + FBDDEntites.GetIDStationZMini() + TAB +
                       FormatterNombreOOo(C2.Z - C1.Z) + TAB +
                       FormatterNombreOOo(C2.Z) + TAB + FBDDEntites.GetIDStationZMaxi() +  #13#10;
    MyClipboard.AsText := WU;
  finally
    FreeAndNil(MyClipboard);
  end;
end;

procedure TfrmStatistiquesExt.AfficherInfosBDD();
var
  FR: TVentilationSpeleometrie;
  C1, C2: TPoint3Df;
  QDevNaturel: Double;
begin
  FR := FBDDEntites.GetRecapitulation();
  // étendue du réseau
  C1 := FBDDEntites.GetCoinBasGauche();
  C2 := FBDDEntites.GetCoinHautDroit();

  lbDeniveleTotal.Caption       := FormatterNombreAvecSepMilliers(C2.Z - C1.Z);

  lbMetrageGeneral.Caption        := FormatterNombreAvecSepMilliers(FR.Fossiles + FR.Ennoyables + FR.Siphons + FR.Vadoses);

  lbMetrageFossile.Caption      := FormatterNombreAvecSepMilliers(FR.Fossiles);
  lbMetrageVadoses.Caption      := FormatterNombreAvecSepMilliers(FR.Vadoses);
  lbMetrageInondable.Caption    := FormatterNombreAvecSepMilliers(FR.Ennoyables);
  lbMetrageSiphon.Caption       := FormatterNombreAvecSepMilliers(FR.Siphons);
  QDevNaturel := FR.Fossiles + FR.Vadoses + FR.Ennoyables + FR.Siphons;
  lbMetrageSpeciaux.Caption     := FormatterNombreAvecSepMilliers(FR.Speciaux);

  lbMetrageTunnels.Caption      := FormatterNombreAvecSepMilliers(FR.Tunnels);
  lbMetrageMines.Caption        := FormatterNombreAvecSepMilliers(FR.Filons);

  lbMetrageTotal.Caption        := FormatterNombreAvecSepMilliers(QDevNaturel + FR.Speciaux + FR.Tunnels + FR.Filons);

  lbXMini.Caption := FormatterNombreAvecSepMilliers(C1.X);
  lbYMini.Caption := FormatterNombreAvecSepMilliers(C1.Y);
  lbZMini.Caption := FormatterNombreAvecSepMilliers(C1.Z);

  lbXMaxi.Caption := FormatterNombreAvecSepMilliers(C2.X);
  lbYMaxi.Caption := FormatterNombreAvecSepMilliers(C2.Y);
  lbZMaxi.Caption := FormatterNombreAvecSepMilliers(C2.Z);

  lbEtendueX.Caption := FormatterNombreAvecSepMilliers(C2.X - C1.X);
  lbEtendueY.Caption := FormatterNombreAvecSepMilliers(C2.Y - C1.Y);
  lbEtendueZ.Caption := FormatterNombreAvecSepMilliers(C2.Z - C1.Z);

  lbStationXMini.caption := FBDDEntites.GetIDStationXMini();
  lbStationYMini.caption := FBDDEntites.GetIDStationYMini();
  lbStationZMini.caption := FBDDEntites.GetIDStationZMini();

  
  lbStationXMaxi.caption := FBDDEntites.GetIDStationXMaxi();
  lbStationYMaxi.caption := FBDDEntites.GetIDStationYMaxi();
  lbStationZMaxi.caption := FBDDEntites.GetIDStationZMaxi();

end;

procedure TfrmStatistiquesExt.cmbAffichageTableauxChange(Sender: TObject);
begin
  case cmbAffichageTableaux.ItemIndex of
    0: RefreshTableau(mslRESEAUX);
    1: RefreshTableau(mslSECTEURS);
    2: RefreshTableau(mslCODE);
    3: RefreshTableau(mslEXPE);
    4: RefreshTableau(mslDATE)
  else
    RefreshTableau(mslRESEAUX);
  end;
end;

procedure TfrmStatistiquesExt.cmbReseauOuSecteurChange(Sender: TObject);
begin
  RefresTableauReseauxSecteurs(cmbReseauOuSecteur.ItemIndex);
end;

procedure TfrmStatistiquesExt.editFiltresChange(Sender: TObject);
begin

end;

procedure TfrmStatistiquesExt.FormCreate(Sender: TObject);
begin
  pass;
end;

procedure TfrmStatistiquesExt.FormResize(Sender: TObject);
begin
  pnlRoseDiagram.Width := pnlRoseDiagram.Height;
end;

procedure TfrmStatistiquesExt.btnCopyTableauRecapClick(Sender: TObject);
begin
  grdSyntheseSpeleometrique.CopyToClipboard(false);

end;

procedure TfrmStatistiquesExt.btnCopyTableauStatsReseauxClick(Sender: TObject);
begin
  grdStatsReseauxSecteurs.CopyToClipboard();
end;

procedure TfrmStatistiquesExt.btnExportDepthDiagramSVGClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QIdxFilter: integer;
begin
  QFileName := GetGHTopoDirectory() + 'RoseDiagram1.svg';
  if (DoDialogSaveFile('Fichiers SVG (*.svg)|.svg', '.svg', QFileName, QIdxFilter)) then
    CdrDepthDiagramme1.ExporterSVG(QFileName);
end;

procedure TfrmStatistiquesExt.btnCopyTableauCoordsClick(Sender: TObject);
var
  MyClipBoard: TClipboard;
  Nb, i: Integer;
  EWE: TBaseStation;
  WU: String;
begin
  MyClipBoard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    Nb := FBDDEntites.GetNbEntitesVisees();
    WU := '';
    for i := 0 to HeaderControl1.Sections.Count - 1 do WU += HeaderControl1.Sections.Items[i].Text + TAB;
    WU += #13#10;
    for i := 0 to Nb -1 do
    begin
      EWE := FBDDEntites.GetEntiteVisee(i);
      WU  += Format(FMTSERST, [EWE.Entite_Serie, EWE.Entite_Station]) + TAB +
             EWE.IDTerrain + TAB +
             GetDescTypeVisee(EWE.Type_Entite) + TAB +
             FormatterNombreOOo(EWE.oLongueur, 3, False) + TAB +
             FormatterNombreOOo(EWE.oAzimut, 3, False) + TAB +
             FormatterNombreOOo(EWE.oPente, 3, False) + TAB +
             FormatterNombreOOo(EWE.oLG, 3, False) + TAB +
             FormatterNombreOOo(EWE.oLD, 3, False) + TAB +
             FormatterNombreOOo(EWE.oHZ, 3, False) + TAB +
             FormatterNombreOOo(EWE.oHN, 3, False) + TAB +
             FormatterNombreOOo(EWE.PosStation.X, 3, False) + TAB +
             FormatterNombreOOo(EWE.PosStation.Y, 3, False) + TAB +
             FormatterNombreOOo(EWE.PosStation.Z, 3, False) + #13#10;
    end;
    MyClipBoard.AsText := WU;
  finally
    FreeAndNil(MyClipBoard);
  end;
end;


procedure TfrmStatistiquesExt.btnCopyTableauEntreesClick(Sender: TObject);
var
  MyClipBoard: TClipboard;
  i, Nb: Integer;
  EWE: TEntrance;
  WU: String;
begin
  MyClipBoard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    Nb := FBDDEntites.GetNbEntrances();
    WU := '';
    for i := 0 to HeaderControl1.Sections.Count - 1 do WU += HeaderControl1.Sections.Items[i].Text + TAB;
    WU += #13#10;
    for i := 0 to Nb -1 do
    begin
      EWE := FBDDEntites.GetEntrance(i);
      WU  += format('%d', [i]) + TAB +
             format('#%.2X%.2X%.2X', [Red(EWE.eCouleur), Green(EWE.eCouleur), Blue(EWE.eCouleur)]) + TAB +
             Format(FMTSERST, [EWE.eRefSer, EWE.eRefSt]) + TAB +
             EWE.eNomEntree + TAB +
             FormatterNombreOOo(EWE.eXEntree, 3, False) + TAB +
             FormatterNombreOOo(EWE.eYEntree, 3, False) + TAB +
             FormatterNombreOOo(EWE.eZEntree, 3, False) + TAB +
             EWE.eObserv + #13#10;
    end;
    MyClipBoard.AsText := WU;
  finally
    FreeAndNil(MyClipBoard);
  end;
end;


procedure TfrmStatistiquesExt.btnColorBarresFillColorChanged(Sender: TObject);
begin
  self.btnRedrawDiagramsClick(self);
end;

procedure TfrmStatistiquesExt.btnColorBarresLineColorChanged(Sender: TObject);
begin
  self.btnRedrawDiagramsClick(self);
end;




procedure TfrmStatistiquesExt.btnRedrawDiagramsClick(Sender: TObject);
begin
  CdrRoseDiagramme1.SetNombreColorPetales(editNbPetales.AsInteger    , btnColorBarresLine.ButtonColor, btnColorBarresFill.ButtonColor);
  CdrDepthDiagramme1.SetNombreColorBarres(editNbBarresDepth.AsInteger, btnColorBarresLine.ButtonColor, btnColorBarresFill.ButtonColor);
  CdrRoseDiagramme1.RedessinerDiagramme();
  CdrDepthDiagramme1.RedessinerDiagramme();
  pagectlHistogrammes.Invalidate;
end;





procedure TfrmStatistiquesExt.btnExportRoseDiagramSVGClick(Sender: TObject);
var
  QFileName: TStringDirectoryFilename;
  QIdxFilter: integer;
begin
  QFileName := GetGHTopoDirectory() + 'RoseDiagram1.svg';
  if (DoDialogSaveFile('Fichiers SVG (*.svg)|.svg', '.svg', QFileName, QIdxFilter)) then
    CdrRoseDiagramme1.ExporterSVG(QFileName);
end;

function TfrmStatistiquesExt.InitialiseByPointer(const QDTT: TToporobotStructure2012;
                                                 const QBDD: TBDDEntites;
                                                 const QFiltres: string): boolean;
var
  QDevelViseesVisibles: double;
begin
  AfficherMessage('-- InitialiseByPointer');
  Result := False;
  lsbEntrances.ItemHeight := LSB_ITEM_HEIGHT;
  lsbVisees.ItemHeight    := LSB_ITEM_HEIGHT;
  FOldFiltre := QFiltres;
  FDocTopo    := QDTT;
  FBDDEntites := QBDD;
  try
    FBDDEntites.MetaFiltre(QFiltres, QDevelViseesVisibles);
    InitialiseStats();
    Result := True;
  finally
  end;
end;



procedure TfrmStatistiquesExt.Finaliser();
var
  QDevelViseesVisibles: double;
begin
  FBDDEntites.MetaFiltre(FOldFiltre, QDevelViseesVisibles);   // restauration du filtre
end;
procedure TfrmStatistiquesExt.RefreshTableau(const QMode: TModeSelectionListe);
const
  NB_COLONNES = 13;
var
  i, Nb: integer;
  Totaux: TVentilationSpeleometrie;
  procedure AddValeursSpeleometrie(const QQ: TVentilationSpeleometrie);
  begin
    Totaux.Ennoyables += QQ.Ennoyables;
    Totaux.Filons     += QQ.Filons;
    Totaux.Fossiles   += QQ.Fossiles;
    Totaux.Siphons    += QQ.Siphons;
    Totaux.Speciaux   += QQ.Speciaux;
    Totaux.Tunnels    += QQ.Tunnels;
    Totaux.Vadoses    += QQ.Vadoses;
  end;

  procedure WriteHeader();
  const
    WU = 110;
  begin
    with grdSyntheseSpeleometrique do
    begin
      ColWidths[0] := 300;
      case QMode of
        mslRESEAUX   : Cells[0, 0] := GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE0);
        mslSECTEURS  : Cells[0, 0] := GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE1);
        mslCODE      : Cells[0, 0] := GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE2);
        mslEXPE      : Cells[0, 0] := GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE3);
        mslDATE      : Cells[0, 0] := GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE4);
      end;
      Cells[1, 0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_FOSSILES)    ; ColWidths[1]  := WU;
      Cells[2, 0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_VADOSES)     ; ColWidths[2]  := WU;
      Cells[3, 0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_ENNOYABLES)  ; ColWidths[3]  := WU;
      Cells[4, 0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_SIPHONS)     ; ColWidths[4]  := WU;
      Cells[5, 0] := ''                                                      ; ColWidths[5]  := 10;
      Cells[6, 0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_NATURELS)    ; ColWidths[6]  := WU + 10;
      Cells[7, 0] := ''                                                      ; ColWidths[7]  := 10;
      Cells[8, 0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_SPECIAUX)    ; ColWidths[8]  := WU;
      Cells[9, 0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_TUNNELS)     ; ColWidths[9]  := WU;
      Cells[10,0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_MINES)       ; ColWidths[10] := WU;
      Cells[11,0] := ''                                                      ; ColWidths[11] := 10;

      Cells[12,0] := GetResourceString(rsSTATISTIQUES_TYPE_SHOT_TOTAL)       ; ColWidths[12] := WU;

    end;
  end;
  procedure WriteLigne(const Idx: integer);
  var
    Q: integer;
    SR: TVentilationSpeleometrie;
    qR: TReseau;
    qS: TSecteur;
    qC: TCode;
    qE: TExpe;
    qD: TDateTime;
    QDevNaturel: Double;
  begin
    Q := 1 + Idx;
    case FModeAffichageTableau of
      mslRESEAUX:
        begin
           qR := FBDDEntites.GetReseau(Idx);
           grdSyntheseSpeleometrique.Cells[0, Q] := GetResourceString(qR.NomReseau);
           SR := FBDDEntites.GetSpeleometrieParReseauxByIdx(Idx);
         end;
      mslSECTEURS:
        begin
          qS := FBDDEntites.GetSecteur(Idx);
          grdSyntheseSpeleometrique.Cells[0, Q] := GetResourceString(qS.NomSecteur);
          SR := FBDDEntites.GetSpeleometrieParSecteurByIdx(Idx);
        end;
      mslCODE:
        begin
          qC := FBDDEntites.GetCode(Idx);
          grdSyntheseSpeleometrique.Cells[0, Q] := GetResourceString(qC.Commentaire);
          SR := FBDDEntites.GetSpeleometrieParCodeByIdx(Idx);
        end;
      mslEXPE:
        begin
          qE := FBDDEntites.GetExpe(Idx);
          grdSyntheseSpeleometrique.Cells[0, Q] := Format('%d - %s', [qE.IDExpe, GetResourceString(qE.Commentaire)]);
          SR := FBDDEntites.GetSpeleometrieParExpeByIdx(Idx);
        end;
      mslDATE:
        begin
          qD := FBDDEntites.GetUneDate(Idx);
          grdSyntheseSpeleometrique.Cells[0, Q] := Format(FORMAT_STRING, [DateToStr(qD)]);
          SR := FBDDEntites.GetSpeleometrieParDateByIdx(Idx);
        end;
    end;
    AddValeursSpeleometrie(SR);     // calcul du total
    grdSyntheseSpeleometrique.Cells[1, Q] := FormatterNombreOOo(SR.Fossiles, 3, false);
    grdSyntheseSpeleometrique.Cells[2, Q] := FormatterNombreOOo(SR.Vadoses, 3, false);
    grdSyntheseSpeleometrique.Cells[3, Q] := FormatterNombreOOo(SR.Ennoyables, 3, false);
    grdSyntheseSpeleometrique.Cells[4, Q] := FormatterNombreOOo(SR.Siphons, 3, false);
    grdSyntheseSpeleometrique.Cells[5, Q] := '';
    QDevNaturel := SR.Fossiles + SR.Vadoses + SR.Ennoyables + SR.Siphons;
    grdSyntheseSpeleometrique.Cells[6, Q] := FormatterNombreOOo(QDevNaturel, 3, false);
    grdSyntheseSpeleometrique.Cells[7, Q] := '';
    grdSyntheseSpeleometrique.Cells[8, Q] := FormatterNombreOOo(SR.Speciaux, 3, false);
    grdSyntheseSpeleometrique.Cells[9, Q] := FormatterNombreOOo(SR.Tunnels, 3, false);
    grdSyntheseSpeleometrique.Cells[10,Q] := FormatterNombreOOo(SR.Filons, 3, false);
    grdSyntheseSpeleometrique.Cells[11, Q] := '';
    grdSyntheseSpeleometrique.Cells[12,Q]  := FormatterNombreOOo(QDevNaturel + SR.Speciaux + SR.Tunnels + SR.Filons, 3, false);

  end;
  procedure WriteFooter();
  var
    Q: Integer;
    QDevNaturel: Double;
  begin
    Q := grdSyntheseSpeleometrique.RowCount - 2;
    grdSyntheseSpeleometrique.Cells[0, Q] := '';
    grdSyntheseSpeleometrique.Cells[1, Q] := FormatterNombreOOo(Totaux.Fossiles, 3, false);
    grdSyntheseSpeleometrique.Cells[2, Q] := FormatterNombreOOo(Totaux.Vadoses, 3, false);
    grdSyntheseSpeleometrique.Cells[3, Q] := FormatterNombreOOo(Totaux.Ennoyables, 3, false);
    grdSyntheseSpeleometrique.Cells[4, Q] := FormatterNombreOOo(Totaux.Siphons, 3, false);
    grdSyntheseSpeleometrique.Cells[5, Q] := '';
    QDevNaturel := Totaux.Fossiles + Totaux.Vadoses + Totaux.Ennoyables + Totaux.Siphons;
    grdSyntheseSpeleometrique.Cells[6, Q] := FormatterNombreOOo(QDevNaturel, 3, false);
    grdSyntheseSpeleometrique.Cells[7, Q] := '';
    grdSyntheseSpeleometrique.Cells[8, Q] := FormatterNombreOOo(Totaux.Speciaux, 3, false);
    grdSyntheseSpeleometrique.Cells[9, Q] := FormatterNombreOOo(Totaux.Tunnels, 3, false);
    grdSyntheseSpeleometrique.Cells[10,Q] := FormatterNombreOOo(Totaux.Filons, 3, false);
    grdSyntheseSpeleometrique.Cells[11, Q] := '';
    grdSyntheseSpeleometrique.Cells[12,Q]  := FormatterNombreOOo(QDevNaturel + Totaux.Speciaux + Totaux.Tunnels + Totaux.Filons, 3, false);
    Q := grdSyntheseSpeleometrique.RowCount - 1;
    grdSyntheseSpeleometrique.Cells[0, Q] := 'Total general';
    grdSyntheseSpeleometrique.Cells[1, Q] := 'Naturels';
    grdSyntheseSpeleometrique.Cells[2, Q] := FormatterNombreOOo(Totaux.Fossiles + Totaux.Vadoses + Totaux.Ennoyables + Totaux.Siphons, 3, false);
    grdSyntheseSpeleometrique.Cells[3, Q] := 'Total';
    grdSyntheseSpeleometrique.Cells[4, Q] := FormatterNombreOOo(Totaux.Fossiles + Totaux.Vadoses + Totaux.Ennoyables + Totaux.Siphons + Totaux.Speciaux, 3, false);
  end;
begin
  FModeAffichageTableau := QMode;
  Totaux.Ennoyables := 0.00;
  Totaux.Filons     := 0.00;
  Totaux.Fossiles   := 0.00;
  Totaux.Siphons    := 0.00;
  Totaux.Speciaux   := 0.00;
  Totaux.Tunnels    := 0.00;
  Totaux.Vadoses    := 0.00;
  case FModeAffichageTableau of
    mslRESEAUX  : Nb := FBDDEntites.GetNbReseaux();
    mslSECTEURS : Nb := FBDDEntites.GetNbSecteurs();
    mslCODE     : Nb := FBDDEntites.GetNbCodes();
    mslEXPE     : Nb := FBDDEntites.GetNbExpes();
    mslDATE     : Nb := FBDDEntites.GetNbDates();
  end;
  case FModeAffichageTableau of
    mslRESEAUX  : lbNbItems.Caption := Format(GetResourceString(rsSTATISTIQUES_NB_RESEAUX)  ,[Nb]);
    mslSECTEURS : lbNbItems.Caption := Format(GetResourceString(rsSTATISTIQUES_NB_SECTEURS) ,[Nb]);
    mslCODE     : lbNbItems.Caption := Format(GetResourceString(rsSTATISTIQUES_NB_CODES)    ,[Nb]);
    mslEXPE     : lbNbItems.Caption := Format(GetResourceString(rsSTATISTIQUES_NB_SEANCES)  ,[Nb]);
    mslDATE     : lbNbItems.Caption := Format(GetResourceString(rsSTATISTIQUES_NB_DATES)    ,[Nb]);
  end;
  grdSyntheseSpeleometrique.ColCount := NB_COLONNES;
  grdSyntheseSpeleometrique.RowCount := 1 + Nb + 1 + 1 + 1;
  grdSyntheseSpeleometrique.Clean;
  WriteHeader();
  for i := 0 to Nb -1 do
  begin
    WriteLigne(i);
  end;
  WriteFooter();
end;



//******************************************************************************
procedure TfrmStatistiquesExt.FormShow(Sender: TObject);
begin

  self.Caption              := GetResourceString(rsSTATISTIQUES_TITRE);
  tabShtSynthese.Caption    := GetResourceString(rsSTATISTIQUES_TAB_SYNTHESE);
  tabShtTableaux.Caption    := GetResourceString(rsSTATISTIQUES_TAB_TABLEAUX);
  tabShtDiagrams.Caption    := GetResourceString(rsSTATISTIQUES_TAB_DIAGRAMMES);
  tabShtReseauSecteur.Caption := GetResourceString(rsSTATISTIQUES_TAB_RESEAU_SECTEURS);
  tabShtCoordonnees.Caption := GetResourceString(rsSTATISTIQUES_TAB_COORDONNEES);
  tabShtEntrances.caption   := GetResourceString(rsSTATISTIQUES_TAB_ENTRANCES);
  // caption de la synthèse
  lbDevelTopal.caption         := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_DEVEL_GALERIE);
  lbDenivTopal.caption         := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_DENIV_TOTAL);

  grbxDeveloppements.Caption   := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_DEVELOPPEMENTS);
  grbxEtendueReseau.Caption    := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_ETENDUE_RESEAU);

  lbDevNaturels.Caption        := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_NATURELS);
    lbDevFossiles.Caption        := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_Fossiles);
    lbDevVadoses.Caption         := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_Vadoses);
    lbDevInondables.Caption      := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_Inondables);
    lbDevSiphons.Caption         := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_Siphons);

  lbDevSpecials.Caption        := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_SPECIAUX);
    lbDevSpeciaux.Caption        := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_TopoSurface);
  lbDevArtificiels.Caption     := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_ARTIFICIELS);
    lbDevTunnels.Caption         := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_Tunnels);
    lbDevMines.Caption           := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_Mines);
  lbDevTotal.Caption           := GetResourceString(rsSTATISTIQUES_SYNTHESE_LB_DEVEL_GENERAL);;


  editLongViseeMini.Value     := 0.40;
  editNbPetales.AsInteger     := 40;
  editNbBarresDepth.AsInteger := 80;

  cmbAffichageTableaux.Clear;
  cmbAffichageTableaux.Style := csDropDownList;
  cmbAffichageTableaux.Items.Add(GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE0));
  cmbAffichageTableaux.Items.Add(GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE1));
  cmbAffichageTableaux.Items.Add(GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE2));
  cmbAffichageTableaux.Items.Add(GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE3));
  cmbAffichageTableaux.Items.Add(GetResourceString(rsSTATISTIQUES_CMB_MODE_TABLE4));
  cmbAffichageTableaux.ItemIndex := 0;
  tabshtRoseDiagram.Caption     := GetResourceString(rsSTATISTIQUES_LBL_TITRE_ROSE_DIAGRAM);
  tabShtDepthHisto.Caption      := GetResourceString(rsSTATISTIQUES_LBL_TITRE_DEPTH_DIAGRAM);
  lbLongMiniVisee1.Caption      := GetResourceString(rsSTATISTIQUES_LBL_LONG_MINI_VISEE);
  lbNbPetales.Caption           := GetResourceString(rsSTATISTIQUES_LBL_NB_PETALES);
  lbNbBarres.Caption            := GetResourceString(rsSTATISTIQUES_LBL_NB_BARRES);
  btnRedrawDiagrams.Caption  := GetResourceString(rsSTATISTIQUES_BTN_REDESSINER);
  btnCopyTableauRecap.Caption        := GetResourceString(rsBTN_COPIER_TABLEAU);
  btnCopyTableauCoords.Caption       := GetResourceString(rsBTN_COPIER_TABLEAU);
  btnCopyTableauStatsReseaux.Caption := GetResourceString(rsBTN_COPIER_TABLEAU);
  ListerCoordonneesStations();
  tabShtCoordonnees.Visible := True;
  ListerCoordonneesEntrances();
  tabShtEntrances.Visible   := True;
  {$ifdef GROS_MINET}
  tabShtReseauSecteur.Visible := false;
  {$else}
  tabShtReseauSecteur.Visible := true;
  {$endif}
  RedessinerLesDiagrammes;
end;

procedure TfrmStatistiquesExt.grdSyntheseSpeleometriqueDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  n, HauteurLigne: LongInt;
  EWE: TColor;
  Nb: Integer;
  // Alignment: idem clavier numérique
  procedure DessinerCell(const BG: TColor; const S: string; const Alignment: byte);
  var
    RT: TSize;
    QX, QY: LongInt;
  begin
    grdSyntheseSpeleometrique.Canvas.Brush.Color := BG;
    grdSyntheseSpeleometrique.Canvas.FillRect(aRect);
    RT := grdSyntheseSpeleometrique.Canvas.TextExtent(S);

    {$IFDEF RASPBERRY_PI}
    QX := aRect.Left + 6;
    QY := aRect.Top + 2;
    {$ELSE}
    case Alignment of
      1, 4, 7: QX := aRect.Left + 6;
      2, 5, 8: QX := (aRect.Left + aRect.Right) div 2 - RT.Width div 2;
      3, 6, 9: QX := aRect.Right - RT.Width - 6;
    end;
    case Alignment of
      1, 2, 3: QY := (aRect.Bottom - RT.Height) - 2;
      4, 5, 6: QY := (aRect.Top + aRect.Bottom) div 2 - RT.Height div 2;
      7, 8, 9: QY :=  aRect.Top + 2;
    end;
    {$ENDIF RASPBERRY_PI}
    grdSyntheseSpeleometrique.Canvas.TextOut(QX, QY, S);
  end;

begin
  HauteurLigne := grdSyntheseSpeleometrique.RowHeights[0];
  grdSyntheseSpeleometrique.Canvas.Brush.Color := clCream;
  if ((aRow = 0) or (aCol = 0)) then
  begin
    DessinerCell(clSilver, grdSyntheseSpeleometrique.Cells[aCol, aRow], 4);
    exit;
  end;
  if (aCol in [7, 11]) then
  begin
    DessinerCell(clAqua, grdSyntheseSpeleometrique.Cells[aCol, aRow], 5);
    exit;
  end;
  // séparateur
  Nb := 0;
  case FModeAffichageTableau of
    mslENTRANCES   : Nb := FBDDEntites.GetNbEntrances();
    mslRESEAUX     : Nb := FBDDEntites.GetNbReseaux();
    mslSECTEURS    : Nb := FBDDEntites.GetNbSecteurs();
    mslCODE        : Nb := FBDDEntites.GetNbCodes();
    mslEXPE        : Nb := FBDDEntites.GetNbExpes();
    mslDATE        : Nb := FBDDEntites.GetNbDates();
  end;
  if (aRow = Nb + 1) then
  begin
    grdSyntheseSpeleometrique.RowHeights[aRow] := 10;
    DessinerCell(clRed, grdSyntheseSpeleometrique.Cells[aCol, aRow], 5);
    exit;
  end;
  EWE := IIF(Odd(aRow), clWhite, COLOR_ALTERNATE_ROW);
  grdSyntheseSpeleometrique.RowHeights[aRow] := HauteurLigne;
  if (gdSelected in aState) then EWE := clBlue;
  DessinerCell(EWE, grdSyntheseSpeleometrique.Cells[aCol, aRow], 6);
end;

procedure TfrmStatistiquesExt.HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbVisees.Invalidate;
end;

procedure TfrmStatistiquesExt.HeaderControl2SectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbEntrances.Invalidate;
end;

procedure TfrmStatistiquesExt.lsbEntrancesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  EWE: TEntrance;
  QBG: TColor;
  procedure DessineItem(const bg,tc: TColor);
  begin
    ResetColorRow(lsbEntrances, ARect, bg, tc);
    DrawColTexte(lsbEntrances, ARect, HeaderControl2.Sections.Items[0], false, Format('%d', [Index]));
    DrawColRectColoreWithTexte(lsbEntrances, ARect, HeaderControl2.Sections.Items[1], true, bg, EWE.eCouleur, '');
    DrawColTexte(lsbEntrances, ARect, HeaderControl2.Sections.Items[2], True, Format(FMTSERST, [EWE.eRefSer, EWE.eRefSt]));
    DrawColTexte(lsbEntrances, ARect, HeaderControl2.Sections.Items[3], True, EWE.eNomEntree);
    DrawColTexte(lsbEntrances, ARect, HeaderControl2.Sections.Items[4], True, FormatterNombreOOo(EWE.eXEntree, 3, false));
    DrawColTexte(lsbEntrances, ARect, HeaderControl2.Sections.Items[5], True, FormatterNombreOOo(EWE.eYEntree, 3, false));
    DrawColTexte(lsbEntrances, ARect, HeaderControl2.Sections.Items[6], True, FormatterNombreOOo(EWE.eZEntree, 3, false));
    DrawColTexte(lsbEntrances, ARect, HeaderControl2.Sections.Items[7], True, EWE.eObserv);
  end;
begin
  EWE := FBDDEntites.GetEntrance(Index);
  QBG := IIF(Odd(Index), clWhite, COLOR_ALTERNATE_ROW);
  if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(QBG, clBlack);
end;

procedure TfrmStatistiquesExt.lsbViseesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  EWE: TBaseStation;
  QBG: TColor;
  procedure DessineItem(const bg,tc: TColor);
  var
    c: Integer;
  begin
    ResetColorRow(lsbVisees, ARect, bg, tc);
    DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[0], false, Format(FMTSERST, [EWE.Entite_Serie, EWE.Entite_Station]));
    DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[1], True, EWE.IDTerrain);
    DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[2], True, GetDescTypeVisee(EWE.Type_Entite));
    if (IsViseeInSerie(EWE)) then
    begin
      DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[3], True, FormatterNombreOOo(EWE.oLongueur, 3, False));
      DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[4], True, FormatterNombreOOo(EWE.oAzimut, 3, False));
      DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[5], True, FormatterNombreOOo(EWE.oPente, 3, False));
      DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[6], True, FormatterNombreOOo(EWE.oLG, 3, False));
      DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[7], True, FormatterNombreOOo(EWE.oLD, 3, False));
      DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[8], True, FormatterNombreOOo(EWE.oHZ, 3, False));
      DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[9], True, FormatterNombreOOo(EWE.oHN, 3, False));
    end
    else
    begin
      for c := 3 to 9 do DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[c], True, '');
    end;
    DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[10], True, FormatterNombreOOo(EWE.PosStation.X, 3, False));
    DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[11], True, FormatterNombreOOo(EWE.PosStation.Y, 3, False));
    DrawColTexte(lsbVisees, ARect, HeaderControl1.Sections.Items[12], True, FormatterNombreOOo(EWE.PosStation.Z, 3, False));
  end;
begin
  EWE := FBDDEntites.GetEntiteVisee(Index);
  QBG := IIF(Odd(Index), clWhite, COLOR_ALTERNATE_ROW);
  if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(QBG, clBlack);
end;

procedure TfrmStatistiquesExt.sclPourcentageSeuilHightLightChange(Sender: TObject);
begin

end;


procedure TfrmStatistiquesExt.RedessinerLesDiagrammes();
begin
  CdrRoseDiagramme1.Initialiser(FBDDEntites,
                                Trim(editFiltres.Text),
                                editNbPetales.AsInteger,
                                editLongViseeMini.Value,
                                btnColorBarresLine.ButtonColor,
                                btnColorBarresFill.ButtonColor);
  CdrRoseDiagramme1.RedessinerDiagramme();

  // depth diagram
  CdrDepthDiagramme1.Initialiser(FBDDEntites,
                                 Trim(editFiltres.Text),
                                 editNbBarresDepth.AsInteger,
                                 editLongViseeMini.Value,
                                 btnColorBarresLine.ButtonColor,
                                 btnColorBarresFill.ButtonColor,
                                 0.6);
  CdrDepthDiagramme1.RedessinerDiagramme();
end;
// lister coordonnées stations
procedure TfrmStatistiquesExt.ListerCoordonneesStations();
var
  i, Nb: Integer;
begin
  Nb := FBDDEntites.GetNbEntitesVisees();
  FBDDEntites.SortBySerSts;
  AfficherMessage(Format('%s.ListeCoordonneesStations(): %d entites', [ClassName, Nb]));
  lsbVisees.Clear;
  for i := 0 to Nb - 1 do lsbVisees.Items.add('');
  lsbVisees.ItemIndex := 0;
  lbNbStations.Caption := format(GetResourceString(rsNB_ELEMENTS_STATIONS), [Nb]);
end;

procedure TfrmStatistiquesExt.ListerCoordonneesEntrances();
var
  i, Nb: Integer;
begin
  Nb := FBDDEntites.GetNbEntrances();
  // NOTA: Les entrées ont déjà été recensées lors du calcul
  lsbEntrances.Clear;
  for i := 0 to Nb - 1 do lsbEntrances.items.add('');
  lbNbEntrances.Caption := Format(GetResourceString(rsNB_ELEMENTS_ENTRANCES), [Nb]);
end;

procedure TfrmStatistiquesExt.RefresTableauReseauxSecteurs(const QMode: integer);
var
  i, Nb: Integer;
  EWE: TSpeleometrieReseauOuSecteur;
  WU: Boolean;
  procedure S666Str(const QCol, QRow: integer; const QValid: boolean; const QValeur: string);
  begin
    grdStatsReseauxSecteurs.Cells[QCol, QRow] := IIF(QValid, QValeur, '');
  end;
  procedure S666Int(const QCol, QRow: integer; const QValid: boolean; const QValeur: integer);
  begin
    grdStatsReseauxSecteurs.Cells[QCol, QRow] := IIF(QValid, Format('%d', [QValeur]), '');
  end;
  procedure S666Float(const QCol, QRow: integer; const QValid: boolean; const QValeur: double);
  begin
    grdStatsReseauxSecteurs.Cells[QCol, QRow] := IIF(QValid, FormatterNombreOOo(QValeur, 3, false), '');
  end;
begin
  case QMode of
    0: begin
         Nb := 1 + High(FTabSpeleoParReseaux);
         lbNbReseauxSecteurs.caption := Format('%d réseaux', [Nb]);
       end;
    1: begin
         Nb := 1 + High(FTabSpeleoParSecteurs);
         lbNbReseauxSecteurs.caption := Format('%d secteurs', [Nb]);
       end;
  end;
  grdStatsReseauxSecteurs.RowCount := Nb + 1;
  grdStatsReseauxSecteurs.ColCount := 13;
  grdStatsReseauxSecteurs.ColWidths[0] := 40;
  for i := 1 to grdStatsReseauxSecteurs.ColCount - 1 do grdStatsReseauxSecteurs.ColWidths[i] := 110;

  S666Str(0, 0, True, 'ID' );
  S666Str(1, 0, True, 'Nom');
  grdStatsReseauxSecteurs.ColWidths[1] := 320;
  S666Str(2, 0, True, 'Dev');
  S666Str(3, 0, True, 'X Min');
  S666Str(4, 0, True, 'X Max');
  S666Str(5, 0, True, 'Y Min');
  S666Str(6, 0, True, 'Y Max');
  S666Str(7, 0, True, 'Z Min');
  S666Str(8, 0, True, 'Z Max');
  S666Str(9, 0, True, '|<- X ->|');
  S666Str(10, 0, True, '|<- Y ->|');
  S666Str(11, 0, True, '|<- Z ->|');
  S666Str(12, 0, True, 'Nb Sts');
  for i := 0 to Nb - 1 do
  begin
    case QMode of
      0: EWE := FTabSpeleoParReseaux[i];
      1: EWE := FTabSpeleoParSecteurs[i];
    end;
    WU := EWE.DonneesValides;
    S666Int(0,  i+1, True, EWE.IDReseauSecteur);                 // toujours affiché
    S666Str(1,  i+1, True, _AnsiToLCLStr(EWE.NomReseauSecteur)); // toujours affiché
    S666Float(2,  i+1, WU, EWE.Developpement);
    S666Float(3,  i+1, WU, EWE.CoordMini.X);
    S666Float(4,  i+1, WU, EWE.CoordMaxi.X);
    S666Float(5,  i+1, WU, EWE.CoordMini.Y);
    S666Float(6,  i+1, WU, EWE.CoordMaxi.Y);
    S666Float(7,  i+1, WU, EWE.CoordMini.Z);
    S666Float(8,  i+1, WU, EWE.CoordMaxi.Z);
    S666Float(9,  i+1, WU, EWE.Etendue.X);
    S666Float(10, i+1, WU, EWE.Etendue.Y);
    S666Float(11, i+1, WU, EWE.Etendue.Z);
    S666Int(12, i+1, WU, EWE.NbVisees);
  end;
end;
end.

