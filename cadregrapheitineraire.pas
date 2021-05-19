unit CadreGrapheItineraire;
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils,
  StructuresDonnees,
  Common,
  Graphics,
  UnitEntitesExtended,
  UnitGraphes1,
  UnitFichesTopo,
  BZGraphesTypes,
  BZGraphesClasses,
  CallDialogsStdVersion,
  DGCTypes, CadreDGCDrawingContext,
  Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Buttons, Grids, Dialogs, EditBtn, curredit;
type TDisplayDevice = record
  DisplayName: string;
  Width      : integer;
  Height     : integer;
end;
type

{ TCdrGrapheItineraire }

 TCdrGrapheItineraire = class(TFrame)
    btnCalculerItineraire: TButton;
    btnCopierTableau: TButton;
    btnExportHTML: TButton;
    btnCouleur: TColorButton;
    btnGenererFichesPDF: TButton;
    btnPickStartFromCarte: TButton;
    btnPickEndFromCarte: TButton;
    CdrDGCDrawingContext1: TCdrDGCDrawingContext;
    cmbUniteBoussole: TComboBox;
    cmbDeviceCible: TComboBox;
    btnBackgroundCarte: TColorButton;
    btnColorCenterline: TColorButton;
    btnColorStations: TColorButton;
    editHTMLHeight: TCurrencyEdit;
    editHTMLHeight1: TCurrencyEdit;
    editHTMLWidth: TCurrencyEdit;
    editNomItineraire: TEdit;
    editPDFOutputFiches: TFileNameEdit;
    editStationArrivee: TEdit;
    editStationDepart: TEdit;
    editHTMLOutput: TFileNameEdit;
    grdRoadmap: TStringGrid;
    Label1: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    lbEtape: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbStDepart: TLabel;
    lbStArrivee: TLabel;
    Label3: TLabel;
    lbLongueurParcours: TStaticText;
    lbDistanceEuclidienne: TStaticText;
    lbMouseCoords: TStaticText;
    lbNbPointsPassage: TStaticText;
    PageControl1: TPageControl;
    pnlProgression: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    tabExportHTML: TTabSheet;
    tabShtPlan: TTabSheet;
    tabShtRoadMap: TTabSheet;
    procedure btnBackgroundCarteColorChanged(Sender: TObject);
    procedure btnCalculerItineraireClick(Sender: TObject);
    procedure btnColorCenterlineColorChanged(Sender: TObject);
    procedure btnColorStationsColorChanged(Sender: TObject);
    procedure btnCopierTableauClick(Sender: TObject);
    procedure btnCouleurClick(Sender: TObject);
    procedure btnCreerFicheStationClick(Sender: TObject);
    procedure btnExportHTMLClick(Sender: TObject);
    procedure btnGenererFichesPDFClick(Sender: TObject);
    procedure btnPickEndFromCarteClick(Sender: TObject);
    procedure btnPickStartFromCarteClick(Sender: TObject);
    procedure cmbDeviceCibleChange(Sender: TObject);
    procedure cmbUniteBoussoleChange(Sender: TObject);
  strict private
    FMapBackgroundColor: TColor;
    FMapCenterlineColor: TColor;
    FMapStationsColor  : TColor;

  private
    FCurrentIdxItineraire: integer;
    FCurrentStation: TBZClassNode;
    FBDDEntites: TBDDEntites;
    FGraphe: TPathFindingGraphe;
    FDisplayDevices: array of TDisplayDevice;

    procedure DisplayProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
    procedure GetCoordsPointClicked();
    procedure RemplirComboBoxDevices();
  public
    function  Initialiser(const B: TBDDEntites; const G: TPathFindingGraphe): boolean;
    function  GetGraphe(): TPathFindingGraphe;
    procedure CalculerItineraireByIndex(const Idx: integer);
    procedure SetCurrentIdxItineraire(const Idx: integer);
    function  GetCurrentIdxItineraire(): integer;
    procedure RefreshGraphe(const MyPath: TPathBetweenNodes);
    procedure DessinerGraphe(const MyPath: TPathBetweenNodes);
    function  GetCurrentStation(): TBZClassNode;
    procedure SetBackgroundColor(const C: TColor);
    procedure SetCenterlineColor(const C: TColor);
end;


implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TCdrGrapheItineraire }
function TCdrGrapheItineraire.Initialiser(const B: TBDDEntites; const G: TPathFindingGraphe): boolean;
begin
  Result := false;
  FMapBackgroundColor := clCream;
  FMapCenterlineColor := clBlue;
  FMapStationsColor   := clMaroon;

  btnBackgroundCarte.ButtonColor := FMapBackgroundColor;
  btnColorCenterline.ButtonColor := FMapCenterlineColor;
  btnColorStations.ButtonColor   := FMapStationsColor;

  FCurrentIdxItineraire := 0;

  PageControl1.ActivePageIndex := 0;
  pnlProgression.Visible  := false;
  btnCalculerItineraire.Caption := 'Appliquer';
  cmbUniteBoussole.Clear;
  cmbUniteBoussole.Items.Add(GetResourceString(rsDESC_UNITE_ANGULAIRE_DEGRES));
  cmbUniteBoussole.Items.Add(GetResourceString(rsDESC_UNITE_ANGULAIRE_GRADES));
  cmbUniteBoussole.ItemIndex := 0;
  RemplirComboBoxDevices();
  editHTMLOutput.FileName := GetGHTopoDirectory() + '00000_GrapheJS_001.htm';
  editHTMLOutput.Filter   := 'Fichiers HTML|*.htm';

  editPDFOutputFiches.FileName := GetGHTopoDirectory() + '00000_FichesStations_001.pdf';
  editPDFOutputFiches.Filter   := 'Fichiers PDF|*.pdf';

  //editHTMLWidth.AsInteger  := Screen.Width  - 20;
  //editHTMLHeight.AsInteger := Screen.Height - 140;
  try
    FBDDEntites := B;
    FGraphe := G;
    FCurrentStation := FGraphe.GetStation(0);
    result  := True;
  except
  end;
end;

function TCdrGrapheItineraire.GetGraphe(): TPathFindingGraphe;
begin
  result := FGraphe;
end;

procedure TCdrGrapheItineraire.SetCurrentIdxItineraire(const Idx: integer);
begin
  FCurrentIdxItineraire := Idx;
end;

function TCdrGrapheItineraire.GetCurrentIdxItineraire(): integer;
begin
  result := FCurrentIdxItineraire;
end;

procedure TCdrGrapheItineraire.btnCalculerItineraireClick(Sender: TObject);
var
  EE1, EE2: TBaseStation;
  Q1, Q2, EWE: Boolean;
  MyPath: TPathBetweenNodes;
  QIdx: integer;
begin
  Q1 := FGraphe.BDDEntites.FindStationByCle(false, Trim(editStationDepart.Text) , EE1);
  Q2 := FGraphe.BDDEntites.FindStationByCle(false, Trim(editStationArrivee.Text), EE2);
  if (not Q1) then
  begin
    ShowMessageFmt('Point de départ %s introuvable', [editStationDepart.Text]);
    Exit;
  end;
  if (not Q2) then
  begin
    ShowMessageFmt('Point d''arrivée %s introuvable', [editStationArrivee.Text]);
    Exit;
  end;
  FGraphe.GetItineraire(FCurrentIdxItineraire, MyPath);
  MyPath.Initialiser(EE1.Entite_Serie, EE1.Entite_Station,
                     EE2.Entite_Serie, EE2.Entite_Station,
                     Trim(editNomItineraire.Text),
                     btnCouleur.ButtonColor);

  EWE := FGraphe.RechercherPlusCourtChemin(MyPath);
  if (EWE) then FGraphe.PutItineraire(FCurrentIdxItineraire, MyPath);
  RefreshGraphe(MyPath);
  DessinerGraphe(MyPath);

end;



procedure TCdrGrapheItineraire.btnColorCenterlineColorChanged(Sender: TObject);
begin
  FMapCenterlineColor := btnColorCenterline.ButtonColor;
end;



procedure TCdrGrapheItineraire.btnColorStationsColorChanged(Sender: TObject);
begin
  FMapStationsColor := btnColorStations.ButtonColor;
end;



procedure TCdrGrapheItineraire.btnBackgroundCarteColorChanged(Sender: TObject);
begin
   FMapBackgroundColor := btnBackgroundCarte.ButtonColor;
end;

procedure TCdrGrapheItineraire.CalculerItineraireByIndex(const Idx: integer);
var
  MyPath: TPathBetweenNodes;
  EWE: Boolean;
begin
  FCurrentIdxItineraire := Idx;
  FGraphe.GetItineraire(FCurrentIdxItineraire, MyPath);
  EWE := FGraphe.RechercherPlusCourtChemin(MyPath);
  if (EWE) then FGraphe.PutItineraire(FCurrentIdxItineraire, MyPath);
  RefreshGraphe(MyPath);
  DessinerGraphe(MyPath);
end;

procedure TCdrGrapheItineraire.btnCopierTableauClick(Sender: TObject);
begin
  grdRoadmap.CopyToClipboard();
end;

procedure TCdrGrapheItineraire.btnCouleurClick(Sender: TObject);
begin
  ;;
end;

procedure TCdrGrapheItineraire.btnCreerFicheStationClick(Sender: TObject);
begin
  // pour ne pas se farcir la procédure chiante de sortie de GHTopo
  if (GHTopoQuestionOuiNon('Quitter IMMEDIATEMENT GHTopo ?')) then Application.Terminate;
end;

procedure TCdrGrapheItineraire.btnExportHTMLClick(Sender: TObject);
var
  QMenuWidth: Integer;
begin
  QMenuWidth := 200;
  FGraphe.ExporterGrapheEnJavascript('Graphe du réseau', editHTMLOutput.FileName,
                                      Trim(editStationDepart.Text), Trim(editStationArrivee.Text),
                                      FMapBackgroundColor, //btnBackgroundCarte.ButtonColor,
                                      FMapCenterlineColor, //btnColorCenterline.ButtonColor,
                                      editHTMLWidth.AsInteger,
                                      editHTMLHeight.AsInteger,
                                      QMenuWidth);
  ShowMessage(GetResourceString(rsDONE_ANY_PROCESS));


  // pour ne pas se farcir la procédure chiante de sortie de GHTopo
  //if (GHTopoQuestionOuiNon('Quitter IMMEDIATEMENT GHTopo ?')) then Application.Terminate;
end;

procedure TCdrGrapheItineraire.DisplayProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
begin
  lbEtape.Caption := Format('%s: %d of %d', [Etape, Done, Ending]);
  ProgressBar1.Min      := Starting;
  ProgressBar1.Max      := Ending;
  ProgressBar1.Position := Done;
  Application.ProcessMessages;
end;

procedure TCdrGrapheItineraire.btnGenererFichesPDFClick(Sender: TObject);
var
  FT: TFichesTopo;
  i, Nb: Integer;
  St0: TBaseStation;
  MyPath: TPathBetweenNodes;
  MyNode: TNumeroNoeud;
  MySt: TBZClassNode;
  QSr: TNumeroSerie;
  QSt: TNumeroStation;
begin
  if (not FGraphe.GetItineraire(FCurrentIdxItineraire, MyPath)) then
  begin
    ShowMessage(GetResourceString(rsDLG_GRAPHE_ITINERAIRE_INDEFINI));
    Exit;
  end;
  if (not GHTopoQuestionOuiNon(rsMSG_WARN_LONG_PROCESS)) then Exit;


  Nb := MyPath.GetNbNoeuds();
  if (0 = Nb) then exit;
  FT := TFichesTopo.Create;
  try
    pnlProgression.Visible := True;
    if (FT.Initialiser(FBDDEntites, DisplayProgression)) then
    begin
      for i := 0 to Nb - 1 do
      begin
        MyNode := MyPath.GetNoeud(i);
        MySt := FGraphe.GetStation(MyNode);
        ExtractSerStFromTIDStation(MySt.IDStation, QSr, QSt);
        if (FBDDEntites.GetEntiteViseeFromSerSt(QSr, QSt, ST0)) then FT.AddStation(St0);
      end;
      FT.ImprimerLesFiches(editPDFOutputFiches.FileName, True);
      FT.Finaliser();
      ShowMessage(GetResourceString(rsDONE_ANY_PROCESS));
    end;
  finally
    pnlProgression.Visible := false;
    FreeAndNil(FT);
  end;
  //if (GHTopoQuestionOuiNon('Quitter IMMEDIATEMENT GHTopo')) then Application.Terminate;
end;


procedure TCdrGrapheItineraire.btnPickEndFromCarteClick(Sender: TObject);
begin
  editStationArrivee.Text := FCurrentStation.ToString();
end;

procedure TCdrGrapheItineraire.btnPickStartFromCarteClick(Sender: TObject);
begin
  editStationDepart.Text := FCurrentStation.ToString();
end;

procedure TCdrGrapheItineraire.cmbDeviceCibleChange(Sender: TObject);
var
  EWE: TDisplayDevice;
begin
  EWE := FDisplayDevices[cmbDeviceCible.ItemIndex];
  editHTMLWidth.AsInteger  := EWE.Width   - 20;
  editHTMLHeight.AsInteger := EWE.Height  - 170;
end;

procedure TCdrGrapheItineraire.cmbUniteBoussoleChange(Sender: TObject);
var
  MyPath: TPathBetweenNodes;
begin
  FGraphe.GetItineraire(FCurrentIdxItineraire, MyPath);
  RefreshGraphe(MyPath);
end;



procedure TCdrGrapheItineraire.GetCoordsPointClicked();
var
  EWE : TDGCPoint2D;
  QSer: TNumeroSerie;
  QPt : TNumeroStation;
begin
  EWE := CdrDGCDrawingContext1.GetCurrentPosition();
  // attrapper l'ID du noeud le plus proche
  if (FGraphe.FindStationByXY(EWE.X, EWE.Y, FCurrentStation)) then
  begin
    //ExtractSerStFromTIDStation(QST.IDStation, QSer, QPt);
    //lbMouseCoords.Caption := Format('%d.%d: %f, %.2f', [QSer, QPt, QST.X, QST.Y]);
    lbMouseCoords.Caption := Format('%s: %f, %.2f', [FCurrentStation.ToString(), FCurrentStation.X, FCurrentStation.Y]);
  end;
end;

procedure TCdrGrapheItineraire.RemplirComboBoxDevices();
var
  M: TMonitor;
  NbMonitors, i: Integer;
  procedure AddDisplayDevice(const Miou: string; const sw, sh: integer);
  var
    n: Integer;
  begin
    n := Length(FDisplayDevices);
    SetLength(FDisplayDevices, n+1);
    FDisplayDevices[n].DisplayName := Miou;
    FDisplayDevices[n].Width       := sw;
    FDisplayDevices[n].Height      := sh;
    cmbDeviceCible.Items.Add(Format('%s = %dx%d', [FDisplayDevices[n].DisplayName, FDisplayDevices[n].Width, FDisplayDevices[n].Height]));
  end;
begin
  NbMonitors := Screen.MonitorCount;
  SetLength(FDisplayDevices, 0);
  cmbDeviceCible.Clear;
  cmbDeviceCible.Style := csDropDownList;
  for i:= 0 to NbMonitors - 1 do
  begin
    M := Screen.Monitors[i];
    AddDisplayDevice(BoolToStr(M.Primary, 'Primary', 'Auxiliary'), M.Width, M.Height);
  end;
  AddDisplayDevice('Raspberry Pi TouchScreen 7"', 1024, 600);
  cmbDeviceCible.ItemIndex := 0;
  cmbDeviceCibleChange(self);
end;

procedure TCdrGrapheItineraire.RefreshGraphe(const MyPath: TPathBetweenNodes);
var
  i, NbPointsPassage: Integer;
  StDepart, StArrivee: TBZClassNode;
  DistanceEuclidienne, QLong, QAz, QPente, QLongueurParcours: double;
  QSr: TNumeroSerie;
  QSt: TNumeroStation;

  procedure MiouMiou(const QIdxCol: integer; const QColWidth: integer; const QColTitre: string);
  begin
     grdRoadmap.ColWidths[QIdxCol] := QColWidth;
     grdRoadmap.Cells[QIdxCol, 0]  := GetResourceString(QColTitre);
  end;
begin

  lbNbPointsPassage.Caption     := '';
  lbDistanceEuclidienne.Caption := '';
  lbLongueurParcours.Caption := '';

  editNomItineraire.Text   := MyPath.NomItineraire;
  editStationDepart.Text   := Format(FMTSERST, [MyPath.SerieDepart  , MyPath.StationDepart]);
  editStationArrivee.Text  := Format(FMTSERST, [MyPath.SerieArrivee , MyPath.StationArrivee]);
  btnCouleur.ButtonColor   := MyPath.Color;


  NbPointsPassage := MyPath.GetNbNoeuds();
  AfficherMessageErreur(inttostr(NbPointsPassage));
  DistanceEuclidienne := 0.00;
  grdRoadmap.RowCount := 1;
  grdRoadmap.ColCount := 10;
  MiouMiou(0,  50, 'Point');
  MiouMiou(1,  80, 'ID');

  MiouMiou(2,  90, 'Cap suivant');
  MiouMiou(3,  90, 'Dist. suivant');
  MiouMiou(4,  90, 'Pente suivant');


  MiouMiou(5, 110, 'X');
  MiouMiou(6, 110, 'Y');
  MiouMiou(7, 110, 'Z');

  MiouMiou(8, 100, 'Chaînage');
  MiouMiou(9, 666, 'Observ');

  if (NbPointsPassage > 2) then
  begin
    StDepart  := FGraphe.GetStation(MyPath.GetNoeud(0));
    StArrivee := FGraphe.GetStation(MyPath.GetNoeud(NbPointsPassage-1));
    DistanceEuclidienne += Hypot3D(StArrivee.X - StDepart.X,
                                   StArrivee.Y - StDepart.Y,
                                   StArrivee.Z - StDepart.Z);


    lbNbPointsPassage.Caption     := Format('%d points', [NbPointsPassage]);
    lbDistanceEuclidienne.Caption := Format('Distance euclidienne: %f m', [DistanceEuclidienne]);
    lbLongueurParcours.Caption := Format('Longueur du parcours: %f m', [MyPath.LongueurParcours]);
    // feuille de route
    grdRoadmap.RowCount := NbPointsPassage + 1;
    QLongueurParcours := 0.00;
    QLong             := 0.00;
    QAz               := 0.00;
    QPente            := 0.00;
    for i := 0 to NbPointsPassage - 2 do
    begin
      StDepart   := FGraphe.GetStation(MyPath.GetNoeud(i));
      StArrivee  := FGraphe.GetStation(MyPath.GetNoeud(i+1));
      QLongueurParcours += Hypot3D(StArrivee.X - StDepart.X,
                                     StArrivee.Y - StDepart.Y,
                                     StArrivee.Z - StDepart.Z);
      ExtractSerStFromTIDStation(StArrivee.IDStation, QSr, QSt);
      GetBearingInc(StArrivee.X - StDepart.X,
                    StArrivee.Y - StDepart.Y,
                    StArrivee.Z - StDepart.Z,
                    QLong, QAz, QPente,
                    IIF(cmbUniteBoussole.ItemIndex = 0, 360.00, 400.00), 360.00);
      grdRoadmap.Cells[0, i+1] := Format('%d', [i+1]);
      //grdRoadmap.Cells[1, i+1] := Format('%d.%d', [QSr, QSt]);
      grdRoadmap.Cells[1, i+1] := StArrivee.ToString();

      grdRoadmap.Cells[2, i+1] := FormatterNombreOOo(QAz);
      grdRoadmap.Cells[3, i+1] := FormatterNombreOOo(QLong);
      grdRoadmap.Cells[4, i+1] := FormatterNombreOOo(QPente);


      grdRoadmap.Cells[5, i+1] := FormatterNombreOOo(StArrivee.X, 3);
      grdRoadmap.Cells[6, i+1] := FormatterNombreOOo(StArrivee.Y, 3);
      grdRoadmap.Cells[7, i+1] := FormatterNombreOOo(StArrivee.Z, 3);

      grdRoadmap.Cells[8, i+1] := FormatterNombreOOo(QLongueurParcours);
      grdRoadmap.Cells[9, i+1] := StDepart.MetaData;
    end;
  end;
end;

procedure TCdrGrapheItineraire.DessinerGraphe(const MyPath: TPathBetweenNodes);
var
  i, Nb, j: Integer;
  EWE0, EWE1: TNumeroNoeud;
  FFF: UnicodeString;
  MySt, MySt0, StVoisine: TBZClassNode;
  QNbA: Int64;
  QSr: TNumeroSerie;
  QSt: TNumeroStation;
begin
  AfficherMessageErreur('DessinerGraphe()');
  CdrDGCDrawingContext1.Initialiser(FGraphe.XMini - 10.0, FGraphe.YMini - 10.0, FGraphe.XMaxi + 10.0, FGraphe.YMaxi + 10.0, True, FMapBackgroundColor);
  CdrDGCDrawingContext1.SetProcOnClick(GetCoordsPointClicked);
  CdrDGCDrawingContext1.BeginDrawing();
    CdrDGCDrawingContext1.AddStyleSheet('Nodes'  , clRed         , 255, psSolid, 1, 0.00, clCream, 128, bsSolid, 'Arial', clMaroon, 255, 15, 2.00, [fsBold], '');
    CdrDGCDrawingContext1.AddStyleSheet('Arcs'   , FMapCenterlineColor, 255, psSolid, 1, 0.00, clAqua , 128, bsSolid, 'Arial', clGreen, 255, 14, 2.00, [], '');
    CdrDGCDrawingContext1.AddStyleSheet('Chemin' , MyPath.Color  , 255, psSolid, 3, 0.00, clAqua , 128, bsSolid, 'Arial', clMaroon, 255, 10, 3.00, [fsBold], '');
    CdrDGCDrawingContext1.BeginGroupe('Noeuds');
      Nb := FGraphe.GetNbStations();
      for i := 0 to Nb -1 do
      begin
        MySt := FGraphe.GetStation(i);
        CdrDGCDrawingContext1.AddEllipse(1, MySt.X, MySt.Y, 0.2, 0.2);
      end;
    CdrDGCDrawingContext1.EndGroupe('Noeuds');
    AfficherMessageErreur('DessinerGraphe(): 002');
    CdrDGCDrawingContext1.BeginGroupe('Arcs');
      Nb := FGraphe.GetNbStations();
      for i := 0 to Nb -1 do
      begin
        MySt := FGraphe.GetStation(i);
        QNbA := MySt.NodeLinkList.Count;
        if (QNba > 0) then
        begin
          for j := 0 to QNBa - 1 do
          begin
            StVoisine := FGraphe.GetStation(MySt.LinkNode[j].TargetNodeIndex);
            CdrDGCDrawingContext1.AddLine(2, MySt.X, MySt.Y, StVoisine.X, StVoisine.Y);
          end;
        end;
      end;
    CdrDGCDrawingContext1.EndGroupe('Arcs');
    AfficherMessageErreur('DessinerGraphe(): 003');
    CdrDGCDrawingContext1.BeginGroupe('Chemins');
    Nb := MyPath.GetNbNoeuds();
    AfficherMessage(Format('Dessin du parcours: %d', [Nb]));
    if (Nb > 1) then
    begin
      for i := 1 to Nb - 1 do
      begin
        MySt0 := FGraphe.GetStation(MyPath.GetNoeud(i-1));
        MySt  := FGraphe.GetStation(MyPath.GetNoeud(i));

        CdrDGCDrawingContext1.AddLine(3, MySt0.X, MySt0.Y, MySt.X, MySt.Y);
        CdrDGCDrawingContext1.AddEllipse(3, MySt.X, MySt.Y, 0.3, 0.3);
        //ExtractSerStFromTIDStation(MyST.IDStation, QSr, QSt);
        //CdrDGCDrawingContext1.AddTexte(3 ,  MySt.X + 0.35,  MySt.Y + 0.35, 1, 0, Format(FMTSERST, [QSr, QSt]));
        CdrDGCDrawingContext1.AddTexte(3 ,  MySt.X + 0.35,  MySt.Y + 0.35, 1, 0, MySt.ToString());
      end;
    end;
    CdrDGCDrawingContext1.EndGroupe('Chemins');
   CdrDGCDrawingContext1.Flush();
  CdrDGCDrawingContext1.EndDrawing();
  FFF := CdrDGCDrawingContext1.GetLastError();
  if (FFF <> '') then ShowMessage(FFF);
end;

function TCdrGrapheItineraire.GetCurrentStation(): TBZClassNode;
begin
  result := FCurrentStation;
end;

procedure TCdrGrapheItineraire.SetBackgroundColor(const C: TColor);
begin

end;

procedure TCdrGrapheItineraire.SetCenterlineColor(const C: TColor);
begin

end;



end.

