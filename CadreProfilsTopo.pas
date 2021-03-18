unit CadreProfilsTopo;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  UnitClasseMaillage,
  ToporobotClasses2012,
  unitProfilTopo,
  Classes, SysUtils, FileUtil,
  unitUtilsComposants,
  FastGEO,
  UnitObjetSerie,
  TAGraph,
  TATypes,
  TASeries,
  TAChartUtils,
  Clipbrd,
  Graphics,
  curredit, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Grids, Dialogs, Types, LCLType;

type TCdrProfilTopo = class(TFrame)
    btnCopierTableau: TButton;
    btnProfilColor: TColorButton;
    btnCopierListeConduitsRecoupes: TButton;
    Chart1: TChart;
    editNomProfil: TEdit;
    editX1: TCurrencyEdit;
    editX2: TCurrencyEdit;
    editY1: TCurrencyEdit;
    editY2: TCurrencyEdit;
    grdValeursProfil: TStringGrid;
    HeaderControl1: THeaderControl;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbCoordsProfil: TStaticText;
    lbNbConduitsRecoupes: TLabel;
    lbNbPointsProfil: TLabel;
    lsbGaleriesRecoupees: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnlProfil: TPanel;
    pnlProfil1: TPanel;
    lbOrientationProfil: TStaticText;
    tabShtConduits: TTabSheet;
    tabShtProfil: TTabSheet;
    tabShtValeurs: TTabSheet;
    procedure btnCopierListeConduitsRecoupesClick(Sender: TObject);
    procedure btnCopierTableauClick(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HeaderControl1SectionClick(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbGaleriesRecoupeesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    FMaillage    : TMaillage;
    FDocTopo     : TToporobotStructure2012; // pour descro des conduits traversés
    FProfilTopo  : TProfilTopo;
    FProfilReady : boolean;
    FCourbeProfil: TLineSeries;
    FPtsConduits : TLineSeries;
    procedure DispProfil();
    procedure InitCaptions();
    procedure ListerConduitsRecoupes();
    procedure ListerValeursProfilTN();
  public
    function  Initialiser(const DT: TToporobotStructure2012; const QMaillage: TMaillage; const ProfilTopo: TProfilTopo): boolean;
    procedure Finaliser();
    function  GetProfilTopo(): TProfilTopo;
  end;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TCdrProfilTopo }

function TCdrProfilTopo.Initialiser(const DT: TToporobotStructure2012; const QMaillage: TMaillage; const ProfilTopo: TProfilTopo): boolean;
begin
  AfficherMessage(Format('%s.Initialiser: %s', [ClassName, ProfilTopo.ProfilName]));
  result := false;
  FDocTopo   := DT;
  FMaillage  := QMaillage;
  FProfilReady := false;


  FProfilTopo := ProfilTopo;
  if (FMaillage.GetNbProfilsTopo() = 0) then exit(false);
  try
    InitCaptions();
    Chart1.ClearSeries;
    DispProfil();
    ListerValeursProfilTN();
    ListerConduitsRecoupes();
    PageControl1.ActivePageIndex := 0;
    result    := true;
    AfficherMessage('Profil OK');
  except
    pass;
  end;
end;

procedure TCdrProfilTopo.Finaliser();
begin
  try
    FCourbeProfil.Clear;
  finally
    FreeAndNil(FCourbeProfil);//FCourbeProfil.Free;
  end;
end;

procedure TCdrProfilTopo.InitCaptions();
var
  c: Integer;
begin
  AfficherMessage(Format('%s.InitCaptions()',[ClassName]));
  btnCopierTableau.Caption               := GetResourceString(rsBTN_COPIER_TABLEAU);
  btnCopierListeConduitsRecoupes.Caption := GetResourceString(rsBTN_COPY_LIST_GAL_RECOUPEES);
  grdValeursProfil.RowCount := 1; // Ne pas mettre à zéro (= plantage)
  grdValeursProfil.ColCount := 5;
  for c := 1 to 4 do grdValeursProfil.ColWidths[c] := 100;
  grdValeursProfil.Cells[0, 0] := GetResourceString(rsCDR_PROFIL_COL_NO_PT);
  grdValeursProfil.Cells[1, 0] := GetResourceString(rsCDR_PROFIL_COL_ABSCISSE);
  grdValeursProfil.Cells[2, 0] := GetResourceString(rsCDR_PROFIL_COL_COTE);
  grdValeursProfil.Cells[3, 0] := GetResourceString(rsCDR_PROFIL_COL_X);
  grdValeursProfil.Cells[4, 0] := GetResourceString(rsCDR_PROFIL_COL_Y);

end;

procedure TCdrProfilTopo.Chart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PM: TDoublePoint;
  PZ: Double;
begin
  if (FProfilReady) then
  begin
    PM := Chart1.ImageToGraph(MakeTPoint(X, Y));
    PZ := FProfilTopo.GetAltitudeProfil(PM.X);
    lbCoordsProfil.Caption := format('Abscisse: %.2f, Alt: %.2f, Alt surface: %.2f - Recouvrement: %.2f', [PM.X, PM.Y, PZ, PZ - PM.Y]);
  end;
end;

procedure TCdrProfilTopo.HeaderControl1SectionClick(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  case Section.Index of
    2: FProfilTopo.SortConduitsRecoupesBySerieStation();
    4: FProfilTopo.SortConduitsRecoupesByAbscissesProfil();
    5: FProfilTopo.SortConduitsRecoupesByAltitudes();
  else
    pass;
  end;
  if (Section.Index in [2, 4, 5]) then ListerConduitsRecoupes();
end;

procedure TCdrProfilTopo.HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbGaleriesRecoupees.Invalidate;
end;

procedure TCdrProfilTopo.lsbGaleriesRecoupeesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  MyConduit: TBaseStation;
  PP: TPoint3DfOrderedByP;
  DeltaZ, QAltTN, XX: Double;
  procedure DessineItem(const bg,tc: TColor);
  var
    QIdxNamespace, QNoSerie: integer;
    MyNamespace: TNameSpace;
    MyReseau: TReseau;
  begin
    DecomposeNumeroSerie(MyConduit.Entite_Serie, QIdxNamespace, QNoSerie);
    MyNamespace := FDocTopo.GetNameSpace(QIdxNamespace);
    ResetColorRow(lsbGaleriesRecoupees, ARect, bg, tc);
    DrawColTexte(lsbGaleriesRecoupees, ARect, HeaderControl1.Sections.Items[0], False, Format(FORMAT_NB_INTEGER,[Index]));
    DrawColTexte(lsbGaleriesRecoupees, ARect, HeaderControl1.Sections.Items[1], True, Format(FMTSERST, [QNoSerie, MyConduit.Entite_Station]));
    MyReseau := FDocTopo.GetReseau(MyConduit.eReseau);
    DrawColRectColoreWithTexte(lsbGaleriesRecoupees, ARect, HeaderControl1.Sections.Items[2], True, bg, MyReseau.ColorReseau, '');
    DrawColTexte(lsbGaleriesRecoupees, ARect, HeaderControl1.Sections.Items[3], True, Format(FORMAT_NB_REAL_3_DEC, [MyConduit.TagDouble]));
    DrawColTexte(lsbGaleriesRecoupees, ARect, HeaderControl1.Sections.Items[4], True, Format(FORMAT_NB_REAL_3_DEC, [MyConduit.PosStation.Z]));
    DrawColTexte(lsbGaleriesRecoupees, ARect, HeaderControl1.Sections.Items[5], True, Format(FORMAT_NB_REAL_3_DEC, [QAltTN]));
    DrawColTexte(lsbGaleriesRecoupees, ARect, HeaderControl1.Sections.Items[6], True, Format(FORMAT_NB_REAL_3_DEC, [DeltaZ]));
  end;
begin
   try
     if (0 = FProfilTopo.GetNbConduitsRecoupes()) then Exit;
     MyConduit := FProfilTopo.GetConduitRecoupe(Index);
     QAltTN := FProfilTopo.GetAltitudeProfil(MyConduit.TagDouble);
     DeltaZ := QAltTN - MyConduit.PosStation.Z;
     lsbGaleriesRecoupees.canvas.brush.color := clWhite;
     if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clWhite, clBlack);
   except
     pass;
   end;
end;

procedure TCdrProfilTopo.btnCopierTableauClick(Sender: TObject);
begin
  grdValeursProfil.CopyToClipboard();
end;

procedure TCdrProfilTopo.btnCopierListeConduitsRecoupesClick(Sender: TObject);
var
  EWE: String;
  MyClipBoard: TClipboard;
  Nb, i, QIdxNamespace, QNoSerie: Integer;
  MyConduit: TBaseStation;
  QAltTN, DeltaZ: Double;
  MyNamespace: TNameSpace;
begin
  Nb := FProfilTopo.GetNbConduitsRecoupes();
  if (Nb = 0) then Exit;
  MyClipBoard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    // contenu
    EWE := '';
    for i := 0 to HeaderControl1.Sections.Count - 1 do EWE += HeaderControl1.Sections.Items[i].Text + #9;
    EWE += #13#10;
    for i := 0 to Nb - 1 do
    begin
      MyConduit := FProfilTopo.GetConduitRecoupe(i);
      QAltTN    := FProfilTopo.GetAltitudeProfil(MyConduit.TagDouble);
      DeltaZ    := QAltTN - MyConduit.PosStation.Z;
      DecomposeNumeroSerie(MyConduit.Entite_Serie, QIdxNamespace, QNoSerie);
      MyNamespace := FDocTopo.GetNameSpace(QIdxNamespace);
      EWE += Format(FORMAT_NB_INTEGER + #9 +
                    FORMAT_STRING + #9 +
                    FMTSERST +#9 +
                    FORMAT_STRING + #9 +
                    FORMAT_STRING + #9 +
                    FORMAT_STRING + #13#10,
                      [i, MyNamespace.Nom,
                      QNoSerie, MyConduit.Entite_Station,
                      FormatterNombreOOo(MyConduit.TagDouble, 3, false),
                      FormatterNombreOOo(QAltTN, 3, false),
                      FormatterNombreOOo(DeltaZ, 3, false)]);
    end;
    MyClipBoard.AsText := EWE;
  finally
    FreeAndNil(MyClipBoard);//MyClipBoard.Free;
  end;
end;

procedure TCdrProfilTopo.DispProfil();
var
  Nb, i, QIdxNamespace, QNoSerie: Integer;
  Pt: TPoint3DfOrderedByP;
  PT0, PT1: TPoint2Df;
  QIX, QIY: double;
  MyVisee: TBaseStation;
  MyNamespace: TNameSpace;
begin
  AfficherMessageErreur('DispProfil(): 001');
  Chart1.ClearSeries;
  FProfilReady := false;
  self.Caption := FProfilTopo.ProfilName;
  btnProfilColor.ButtonColor := FProfilTopo.ProfilColor;
  editNomProfil.Text         := FProfilTopo.ProfilName;
  PT0 := FProfilTopo.GetExtremite1();
  PT1 := FProfilTopo.GetExtremite2();

  editX1.Value  := PT0.X;
  editY1.Value  := PT0.Y;
  editX2.Value  := PT1.X;
  editY2.Value  := PT1.Y;
  lbOrientationProfil.Caption := Format('%.2f deg', [getAzimut(PT1.X - PT0.X, PT1.Y - PT0.Y, 360.00)]);
  // Le tracé du profil
  // on supprime la série précédente s'il y en a
  try
    Nb := FProfilTopo.GetNbPointsProfilTN();
    if (0 = Nb) then exit;
    FCourbeProfil := TLineSeries.Create(Chart1);
    FCourbeProfil.Clear;
    FCourbeProfil.ShowPoints     := false;
    FCourbeProfil.LinePen.Color  := FProfilTopo.ProfilColor;
    FCourbeProfil.LinePen.Width  := 3;

    for i := 0 to Nb - 1 do
    begin
      Pt := FProfilTopo.GetPointProfilTN(i);
      FCourbeProfil.AddXY(Pt.P, Pt.Z, '', FProfilTopo.ProfilColor);
    end;
    Chart1.AddSeries(FCourbeProfil);
    FProfilReady  := True;
  except
    pass;
  end;
  // Les conduits
  Nb := FProfilTopo.GetNbConduitsRecoupes();
  if (Nb > 0) then
  begin
    try
      FPtsConduits := TLineSeries.Create(Chart1);
      FPtsConduits.Clear;
      FPtsConduits.ShowPoints     := True;
      FPtsConduits.LineType       := ltNone;
      FPtsConduits.Pointer.Brush.Color := clAqua;
      FPtsConduits.Pointer.Pen.Color   := clBlue;
      FPtsConduits.Pointer.Pen.Width   := 0;
      FPtsConduits.Pointer.Style       := psCircle;
      FPtsConduits.Marks.Style         := smsLabel;

      for i := 0 to Nb - 1 do
      begin
        MyVisee := FProfilTopo.GetConduitRecoupe(i);
        // calcul de l'intersection
        Intersect(PT0.X, PT0.Y, PT1.X, PT1.Y, MyVisee.PosExtr0.X, MyVisee.PosExtr0.Y, MyVisee.PosStation.X, MyVisee.PosStation.Y, QIX, QIY);
        Pt.P := Hypot2D(QIX - PT0.X, QIY - PT0.Y);
        Pt.Z := 0.50 * (MyVisee.PosStation.Z + MyVisee.PosExtr0.Z);
        DecomposeNumeroSerie(MyVisee.Entite_Serie, QIdxNamespace, QNoSerie);
        MyNamespace := FDocTopo.GetNameSpace(QIdxNamespace);
        FPtsConduits.AddXY(Pt.P, Pt.Z,
                           Format('%d.%d%s', [QNoSerie, MyVisee.Entite_Station, IIF(0 = QIdxNamespace, '', '@' + MyNamespace.Nom)]),
                           clBlue);
      end;
      Chart1.AddSeries(FPtsConduits);
    except
      pass;
    end; // if (Nb > 0) then
  end;
end;

function TCdrProfilTopo.GetProfilTopo(): TProfilTopo;
begin
  FProfilTopo.ProfilColor      := btnProfilColor.ButtonColor;
  FProfilTopo.ProfilName       := Trim(editNomProfil.Text);
  Result                       := FProfilTopo;
end;

procedure TCdrProfilTopo.ListerValeursProfilTN();
var
  i, Nb, q: Integer;
  PP: TPoint3DfOrderedByP;
begin
  Nb := FProfilTopo.GetNbPointsProfilTN();
  lbNbPointsProfil.Caption := format(GetResourceString(rsCDR_PROFIL_NB_POINTS), [Nb]);
  if (Nb = 0) then Exit;
  grdValeursProfil.RowCount := 1 + Nb;
  for i := 0 to Nb - 1 do
  begin
    PP := FProfilTopo.GetPointProfilTN(i);
    q := i+1;
    grdValeursProfil.Cells[0, q] := Format(FORMAT_NB_INTEGER, [i]);
    grdValeursProfil.Cells[1, q] := FormatterNombreOOo(PP.P, 3, false);
    grdValeursProfil.Cells[2, q] := FormatterNombreOOo(PP.Z, 3, false);
    grdValeursProfil.Cells[3, q] := FormatterNombreOOo(PP.X, 3, false);
    grdValeursProfil.Cells[4, q] := FormatterNombreOOo(PP.Y, 3, false);
  end;

end;
procedure TCdrProfilTopo.ListerConduitsRecoupes();
var
  Nb, i, QInternalIdx: Integer;
  PT: TBaseStation;
  OMG: TObjSerie;
begin
  Nb := FProfilTopo.GetNbConduitsRecoupes();
  lsbGaleriesRecoupees.Clear;
  lsbGaleriesRecoupees.Sorted := false;
  lbNbConduitsRecoupes.Caption := format(GetResourceString(rsCDR_PROFIL_NB_CONDUITS), [Nb]);
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    PT := FProfilTopo.GetConduitRecoupe(i);
    FDocTopo.GetSerieByNumeroSerie(PT.Entite_Serie, OMG, QInternalIdx);
    lsbGaleriesRecoupees.Items.Add(format('%d: %d.%d (%s)', [i, Pt.Entite_Serie, Pt.Entite_Station, OMG.GetNomSerie()]));
  end;
end;

end.

