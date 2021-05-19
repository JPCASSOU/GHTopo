unit frmDisplayGraphe;
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, math,
  StructuresDonnees,
  Common,
  UnitGraphes1,
  UnitEntitesExtended,
  unitUtilsComposants,
  CallDialogsStdVersion,
  CadreGrapheItineraire,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, ComCtrls, Grids, PairSplitter, curredit, PReport, LCLType, Types;
type TLigneDistancier = array of TPathBetweenNodes;

type

{ TdlgDisplayGraphe }

 TdlgDisplayGraphe = class(TForm)
    BitBtn1: TBitBtn;
    btnAddItineraire: TButton;
    btnOuvrirItineraires: TButton;
    btnSaveItineraires: TButton;
    Button2: TButton;
    btnCopierDistancier: TButton;
    Button3: TButton;
    CdrGrapheItineraire1: TCdrGrapheItineraire;
    HeaderControl1: THeaderControl;
    lbNbItineraires: TLabel;
    lsbItineraires: TListBox;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    grdDistancier: TStringGrid;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    PRPage1: TPRPage;
    procedure btnCopierDistancierClick(Sender: TObject);
    procedure btnAddItineraireClick(Sender: TObject);
    procedure btnExportHTMLClick(Sender: TObject);
    procedure btnOuvrirItinerairesClick(Sender: TObject);
    procedure btnSaveItinerairesClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure grdDistancierClick(Sender: TObject);
    procedure grdDistancierDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure grdDistancierSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure grdDistancierSelection(Sender: TObject; aCol, aRow: Integer);
    procedure lsbItinerairesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbItinerairesSelectionChange(Sender: TObject; User: boolean);
  private
    FDistancier: array of TLigneDistancier;
    procedure ListerLesItineraires(const Idx: integer);
  public
    function Initialiser(const B: TBDDEntites; const G: TPathFindingGraphe): boolean;
    procedure Finaliser();
  end;

var
  dlgDisplayGraphe: TdlgDisplayGraphe;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TdlgDisplayGraphe }

function TdlgDisplayGraphe.Initialiser(const B: TBDDEntites; const G: TPathFindingGraphe): boolean;
begin

  SetLength(FDistancier, 1);
  SetLength(FDistancier[0], 1);
  grdDistancier.ColCount := 1;
  grdDistancier.RowCount := 1;
  result := CdrGrapheItineraire1.Initialiser(B, G);
end;

procedure TdlgDisplayGraphe.Finaliser();
begin
  SetLength(FDistancier, 1);
  SetLength(FDistancier[0], 1);
end;

procedure TdlgDisplayGraphe.FormShow(Sender: TObject);
var
  G: TPathFindingGraphe;
  MyPath: TPathBetweenNodes;

begin
  G := CdrGrapheItineraire1.GetGraphe();
  self.Caption := GetResourceString(rsDLG_GRAPHE_TITLE);
  CdrGrapheItineraire1.SetCurrentIdxItineraire(0);
  G.GetItineraire(CdrGrapheItineraire1.GetCurrentIdxItineraire(), MyPath);

  CdrGrapheItineraire1.RefreshGraphe(MyPath);
  CdrGrapheItineraire1.DessinerGraphe(MyPath);
  ListerLesItineraires(-1);
end;

procedure TdlgDisplayGraphe.grdDistancierClick(Sender: TObject);
begin
  ///*)
end;



procedure TdlgDisplayGraphe.grdDistancierSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin

end;

procedure TdlgDisplayGraphe.grdDistancierSelection(Sender: TObject; aCol, aRow: Integer);
var
  ITI: TPathBetweenNodes;
  Nb: Integer;
begin
  Nb := length(FDistancier);
  try
    if ((aCol > 0) AND (aRow > 0)) then
    begin
      if ((aRow > Nb) OR (aCol > Nb)) then exit;
      ITI := FDistancier[aRow - 1, grdDistancier.ColCount - aCol - 1];
      if (IsZero(ITI.LongueurParcours)) then exit;
      CdrGrapheItineraire1.RefreshGraphe(ITI);
      CdrGrapheItineraire1.DessinerGraphe(ITI);
    end;
  except
  end;
end;


procedure TdlgDisplayGraphe.lsbItinerairesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  MyPath: TPathBetweenNodes;
  procedure DessineItem(const bg,tc: TColor);
  begin
    ResetColorRow(lsbItineraires, ARect, bg, tc);
    DrawColRectColoreWithTexte(lsbItineraires, ARect, HeaderControl1.Sections.Items[0], True, bg, MyPath.Color,  Format(FORMAT_NB_INTEGER,[Index]));
    DrawColTexte(lsbItineraires, ARect, HeaderControl1.Sections.Items[1], True , Format(FMTSERST,[MyPath.SerieDepart , MyPath.StationDepart]));
    DrawColTexte(lsbItineraires, ARect, HeaderControl1.Sections.Items[2], True , Format(FMTSERST,[MyPath.SerieArrivee, MyPath.StationArrivee]));
    DrawColTexte(lsbItineraires, ARect, HeaderControl1.Sections.Items[3], True , Format('%.0f',[MyPath.LongueurParcours]));
    DrawColTexte(lsbItineraires, ARect, HeaderControl1.Sections.Items[4], True , MyPath.NomItineraire);
  end;
begin
  if (0 = lsbItineraires.Count) then exit;
  CdrGrapheItineraire1.GetGraphe().GetItineraire(Index, MyPath);
  if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clWhite, clBlack);
end;

procedure TdlgDisplayGraphe.lsbItinerairesSelectionChange(Sender: TObject; User: boolean);
begin
   CdrGrapheItineraire1.CalculerItineraireByIndex(lsbItineraires.ItemIndex);
end;

procedure TdlgDisplayGraphe.ListerLesItineraires(const Idx: integer);
var
  MyGraphe: TPathFindingGraphe;
  i, Nb: Integer;
begin
  MyGraphe := CdrGrapheItineraire1.GetGraphe();
  Nb := MyGraphe.GetNbItineraires();
  lbNbItineraires.Caption := format('%d itinéraires', [Nb]);
  lsbItineraires.Clear;
  lsbItineraires.ItemHeight := 20;
  if (Nb = 0) then exit;
  for i := 0 to Nb-1 do lsbItineraires.Items.Add('');
  lsbItineraires.ItemIndex := IIF(Idx = -1, lsbItineraires.Count - 1, 0);
end;

procedure TdlgDisplayGraphe.btnAddItineraireClick(Sender: TObject);
var
  MyGraphe: TPathFindingGraphe;
  MyPath: TPathBetweenNodes;
begin
  MyGraphe := CdrGrapheItineraire1.GetGraphe();
  MyPath.Initialiser(1, 1, 1, 1, Format('Itineraire%d', [MyGraphe.GetNbItineraires() + 1]), clBlue);
  MyGraphe.AddItineraire(MyPath);
  ListerLesItineraires(-1);
end;

procedure TdlgDisplayGraphe.btnExportHTMLClick(Sender: TObject);
begin

end;



procedure TdlgDisplayGraphe.btnOuvrirItinerairesClick(Sender: TObject);
var
  TD: TOpenDialog;
  G: TPathFindingGraphe;
begin
  TD := TOpenDialog.Create(self);
  try
    TD.InitialDir := GetGHTopoDirectory();
    TD.Filter := 'Fichiers texte (*.txt)|*.txt';
    TD.FileName := MakeFilenameFromDate('Itineraires_', Now(), 'txt');
    if (TD.Execute) then
    begin
      G := CdrGrapheItineraire1.GetGraphe();
      G.LoadItinerairesFromFile(TD.FileName);
      ListerLesItineraires(0);
    end;
  finally
    FreeAndNil(TD);
  end;
end;

procedure TdlgDisplayGraphe.btnSaveItinerairesClick(Sender: TObject);
var
  TD: TSaveDialog;
  G: TPathFindingGraphe;
begin
  TD := TSaveDialog.Create(self);
  try
    TD.InitialDir := GetGHTopoDirectory();
    TD.Filter := 'Fichiers texte (*.txt)|*.txt';
    TD.FileName := MakeFilenameFromDate('000_Itineraires_', Now(), 'txt');
    if (TD.Execute) then
    begin
      G := CdrGrapheItineraire1.GetGraphe();
      G.SaveItinerairesToFile(TD.FileName);
    end;
  finally
    FreeAndNil(TD);
  end;
end;



procedure TdlgDisplayGraphe.btnCopierDistancierClick(Sender: TObject);
begin
  grdDistancier.CopyToClipboard();
end;



// calcul d'un distancier avec les itinéraires
procedure TdlgDisplayGraphe.Button2Click(Sender: TObject);
var
  MyGraphe: TPathFindingGraphe;
  NbItineraires, i, Nb, j: Integer;
  ITI: TPathBetweenNodes;
  LesExtremites: TStringList;
  QSr0, QSr1: TNumeroSerie;
  QSt0, QSt1: TNumeroStation;
begin
  MyGraphe := CdrGrapheItineraire1.GetGraphe();
  NbItineraires := MyGraphe.GetNbItineraires();
  AfficherMessageErreur(Format('%s.CalculerDistancier(%d)', [ClassName, NbItineraires]));
  if (NbItineraires = 0) then exit;
  LesExtremites := TStringList.Create;
  try
    grdDistancier.Enabled := false;
    LesExtremites.Clear;
    LesExtremites.Sorted     := True;
    LesExtremites.Duplicates := dupIgnore;
    // recenser les extrémités
    for i := 0 to NbItineraires - 1 do
    begin
      MyGraphe.GetItineraire(i, ITI);
      LesExtremites.Add(Format('%d', [MakeTIDBaseStation(ITI.SerieDepart, ITI.StationDepart, false)]));
      LesExtremites.Add(Format('%d', [MakeTIDBaseStation(ITI.SerieArrivee, ITI.StationArrivee, false)]));
    end;
    // remplir la grille
    Nb := LesExtremites.Count;
    grdDistancier.RowCount := 1 + Nb;
    grdDistancier.ColCount := 1 + Nb;
    if (0 = Nb) then exit;
    // calcul du distancier
    SetLength(FDistancier, Nb);
    for i := 0 to Nb-1 do SetLength(FDistancier[i], Nb);
    ProgressBar1.Min := 0;
    ProgressBar1.Max := Nb;
    for i := 0 to Nb - 1 do
    begin
      ExtractSerStFromTIDStation(StrToInt64Def(LesExtremites[i], 0), QSr0, QSt0);
      ProgressBar1.Position := i;
      for j := 0 to Nb - 1 do
      begin
        ExtractSerStFromTIDStation(StrToInt64Def(LesExtremites[j], 0), QSr1, QSt1);
        FDistancier[i][j].Initialiser(QSr0, QSt0, QSr1, QSt1, Format('From %d.%d to %d.%d', [QSr0, QSt0, QSr1, QSt1]), clRed);
        MyGraphe.RechercherPlusCourtChemin(FDistancier[i][j]);
      end;
    end;
    ProgressBar1.Position := 0;
    // et on remplit le tableau
    for i := 0 to Nb - 1 do
    begin
      ExtractSerStFromTIDStation(StrToInt64Def(LesExtremites[i], 0), QSr0, QSt0);
      //if (i = j) then continue;
      grdDistancier.Cells[0, i+1] := Format(FMTSERST, [QSr0, QSt0]);
      for j := 0 to Nb - 1 do
      begin
        ExtractSerStFromTIDStation(StrToInt64Def(LesExtremites[j], 0), QSr1, QSt1);
        grdDistancier.Cells[Nb - j, 0] := Format(FMTSERST, [QSr1, QSt1]);
      end;
      for j := 0 to Nb - 1 do
      begin
        ExtractSerStFromTIDStation(StrToInt64Def(LesExtremites[j], 0), QSr1, QSt1);
        grdDistancier.Cells[Nb - j, i + 1] := FormatterNombreOOo(FDistancier[i][j].LongueurParcours, 0);
      end;
    end;
    grdDistancier.Enabled := True;
  finally
    FreeAndNil(LesExtremites);
  end;
end;

procedure TdlgDisplayGraphe.Button3Click(Sender: TObject);
var
  G: TPathFindingGraphe;
begin
  if (lsbItineraires.ItemIndex < 1) then exit;
  if (GHTopoQuestionOuiNon('Supprimer itinéraire')) then
  begin
    G := CdrGrapheItineraire1.GetGraphe();
    G.RemoveItineraire(lsbItineraires.ItemIndex);
    ListerLesItineraires(0);
  end;
end;

procedure TdlgDisplayGraphe.grdDistancierDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
const WU : TColor = $00CDF9C6;
var
  EWE: TColor;
  procedure DessinerCell(const BG: TColor; const S: string);
  begin
    grdDistancier.Canvas.Brush.Color := BG;
    grdDistancier.Canvas.FillRect(aRect);
    grdDistancier.Canvas.TextOut(aRect.Left + 1, aRect.Top + 1, S);
  end;
begin
  if ((0 = aCol) or (0 = aRow)) then exit;
  EWE := clWhite;
  if (aCol = grdDistancier.Col) then EWE := WU;
  if (aRow = grdDistancier.Row) then EWE := WU;
  if (gdSelected in aState)     then EWE := clBlue;
  DessinerCell(EWE, grdDistancier.Cells[aCol, aRow]);
end;
end.

