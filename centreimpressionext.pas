unit CentreImpressionExt;
// Centre d'impression nouvelle version
// Date: 08/11/2013
// Statut: Opérationnel. Des détails à compléter et à revoir
// Interfaçage par pointeur sur TBDDEntites: OK
// Prévisualisation: OK
// Impression: OK - des détails à revoir.
// 04/08/2014: Nouveau dessin des entrées
// 09/08/2017: Réactivation du module à la demande de S. Clément
// 09/08/2017: Suppression du type TPageProperties
//             et du membre FPrinter: Printer est déjà une variable déclarée dans Printers.pas
// 2020-01-29: Contrôle temporel
// TODO: Réutiliser le centre d'impression de GHCaveDraw si on a le temps

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  CallDialogsStdVersion,
  unitUtilsComposants,
  Common,
  math,
  types,
  BGRABitmap,
  BGRABitmapTypes,
  BGRACanvas,
  BGRAPhongTypes,
  BGRAGradients,
  UnitTGHTopoDrawDrawingContext,
  ToporobotClasses2012,
  UnitEntitesExtended,
  unitCroquisTerrain,
  UnitClasseMaillage,
  Printers, OSPrinters, // OSPrinters est INDISPENSABLE ici !!

  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons;
// types de données locaux
type

  { TfrmPrintingCenterExt }

  TfrmPrintingCenterExt = class(TForm)
    BitBtn1: TBitBtn;
    btnApply: TButton;
    btnHelpFiltres: TButton;
    btnPreview: TButton;
    btnSelectPrinter: TButton;
    btnStartImpression: TButton;
    btnApplyParams2D: TButton;
    btnParametresVue: TButton;
    chkPrintPagesVides: TCheckBox;
    chkRegle: TCheckBox;
    cmbEchelles: TComboBox;
    editFiltres: TEdit;
    editTailleRegle: TCurrencyEdit;
    grbPrinterNames: TGroupBox;
    grbxEchelles: TGroupBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    lbEchelle: TLabel;
    lbMouseCoordinates: TStaticText;
    lbNbPages: TLabel;
    lbOrientation: TLabel;
    lbPageFormat: TLabel;
    lbPrintCurrent: TLabel;
    lbPrinterName: TLabel;
    PaintBoxVue: TPaintBox;
    Panel1: TPanel;
    pnlPrintSettings: TPanel;
    pnlCadre: TPanel;
    pnlProgressPrinting: TPanel;
    progbarPrinting: TProgressBar;
    procedure btnPreviewClick(Sender: TObject);
    procedure btnStartImpressionClick(Sender: TObject);
    procedure btnSelectPrinterClick(Sender: TObject);
    procedure btnApplyParams2DClick(Sender: TObject);
    procedure btnParametresVueClick(Sender: TObject);
    procedure cmbEchellesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxVueDblClick(Sender: TObject);
    procedure PaintBoxVueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxVuePaint(Sender: TObject);
  strict private
    FRedessinInProcess: boolean;
  private
    { private declarations }
    FDocuTopo      : TToporobotStructure2012;
    FBDDEntites    : TBDDEntites;                 // BDD entités
    FCroquisTerrain: TCroquisTerrain;         // crobard
    FMaillage    : TMaillage;
    FVue2DParams: TVue2DParams;
    // nombre de pages sur X et Y
    // tableau des pages dessinées
    FNbPagesX         : integer;
    FNbPagesY         : integer;
    FTableauPagesDrawn: array of array of boolean;
    FDoDrawPages      : boolean;
    // modes de représentation
    //FModeRepresentationGaleries: TModeRepresentationGaleries;
    // variables internes
    FRappScrReal : double;
    FInvRappScrReal: double;
    // coordonnées internes de la souris dans la vue
    FPP: TPoint;
    // limites du dessin
    FRXMini      : double;
    FRXMaxi      : double;
    FRYMini      : double;
    FRYMaxi      : double;
    // variables diverses

    // échelle du dessin
    FEchelle: double;
    // quadrillage
    FQdrCrossSize  : double;
    // dessiner les annotations si elles sont chargées
    // paramétrage imprimante
    procedure CalcIndexPageIncludingPt(const Pt: TPoint2Df; var IdxX, IdxY: integer);
    procedure ImprimerLaTopo();
    procedure PrintAPage(const Echelle: double; const L, C: integer);
    procedure SetMyCurrentPrinter();
    // conversion de coordonnées
    function GetCoordsMonde(const PP: TPoint): TPoint2Df;
    function GetCoordsPlan(const QX, QY: double): TPoint; overload;
    function GetCoordsPlan(const PM: TPoint2Df): TPoint; overload;

    // recadrage du dessin
    procedure Recadrer();
    procedure CalcNbPagesAndEmpty();
    procedure DrawApercu();
  public
    { public declarations }
    function InitialiseByPointer(const QDocTopo  : TToporobotStructure2012;
                                 const QBDD      : TBDDEntites;
                                 const QCroquis  : TCroquisTerrain;
                                 const QMaillage : TMaillage;
                                 const QVue2DParams: TVue2DParams;
                                 const QFiltres: string): boolean;
    // libération du contexte
    procedure Finaliser();
  end;

var
  frmPrintingCenterExt: TfrmPrintingCenterExt;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}
const ONE_INCH_IN_MM: double = 25.40;
const QECHELLE_PAR_DEFAULT   = 1000;
// fonctions locales hors objet
function Millimetres2PixelsXf(const Millims: double): integer;
begin
  Result:=trunc(100 * Printer.XDPI * Millims / ONE_INCH_IN_MM) div 100;
end;
function Millimetres2PixelsYf(const Millims: double): integer;
begin
  Result:=trunc(100 * Printer.YDPI * Millims / ONE_INCH_IN_MM) div 100;
end;
function Pixels2MillimetresX(const Pixls: integer): Double;
begin
  Result := ONE_INCH_IN_MM * Pixls / Printer.XDPI;
end;
function Pixels2MillimetresY(const Pixls: integer): Double;
begin
  Result := ONE_INCH_IN_MM * Pixls / Printer.YDPI;
end;


{ TfrmPrintingCenterExt }
// définir imprimante courante: paramètres, etc ...
procedure TfrmPrintingCenterExt.SetMyCurrentPrinter();
begin
  lbPrinterName.Caption := Printer.PrinterName;
  lbOrientation.Caption := ChooseString(Ord(Printer.Orientation),
                                          ['PORTRAIT', 'LANDSCAPE', 'REVERSE LANDSCAPE', 'REVERSEPORTRAIT']);
  lbPageFormat.Caption  := Format('%f mm x %f mm', [Pixels2MillimetresX(Printer.PageWidth),
                                                    Pixels2MillimetresY(Printer.PageHeight)]);
  Recadrer();                  // reconstruire le plan
  CalcNbPagesAndEmpty();       // calculer le nombre de pages
  DrawApercu();                // redessiner
end;




// conversion de coordonnées
function TfrmPrintingCenterExt.GetCoordsPlan (const PM: TPoint2Df): TPoint;
begin
  Result := GetCoordsPlan(PM.X, PM.Y);
end;

function TfrmPrintingCenterExt.GetCoordsPlan(const QX, QY: double): TPoint;
begin
  // rotation et mise à l'échelle
  (* Formule: (F = FRappScrReal)
  | Xp |   | +F    0    -F * FRXMini |   | PM.X |
  |    |   |                         |   |      |
  | Yp | = |  0   -F    +F * FRYMaxi | * | PM.Y |
  |    |   |                         |   |      |
  |  1 |   |  0    0          1      |   |   1  |
  //*)
  Result.X := Round((QX - FRXMini) * FRappScrReal);
  Result.Y := Round((FRYMaxi - QY) * FRappScrReal);

end;




function TfrmPrintingCenterExt.GetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  (* Formule: (F = FRappScrReal)
  | Xm |   | +1/F    0     FRXMini   |   | PP.X |
  |    |   |                         |   |      |
  | Ym | = |  0   -1/F     FRYMaxi   | * | PP.Y |
  |    |   |                         |   |      |
  |  1 |   |  0    0          1      |   |   1  |
  //*)
  Result.X :=  FInvRappScrReal * PP.X + FRXMini;
  Result.Y := -FInvRappScrReal * PP.Y + FRYMaxi;
end;



// cadrage du dessin
// pas besoin de fonction de zoom
// (sinon, on aurait utilisé un TCdrVisualisateur2D)
// La zone de prévisu doit être carrée
procedure TfrmPrintingCenterExt.Recadrer();
var
  Marge: double;
  qdx, qdy, QLMax: Double;
  C1, C2: TPoint3Df;
  WU: Boolean;
begin
  C1 := FBDDEntites.GetCoinBasGauche;
  C2 := FBDDEntites.GetCoinHautDroit;
  Marge := 0.02 * Hypot2D(C2.X - C1.X, C2.Y - C1.Y);
  FRXMini := C1.X - Marge;
  FRXMaxi := C2.X + Marge;
  FRYMini := C1.Y - Marge;
  FRYMaxi := C2.Y + Marge;
  qdx   := FRXMaxi - FRXMini;
  qdy   := FRYMaxi - FRYMini;
  QLMax := Max(Qdx, qdy);
  WU := (qdy > qdx);

  FRXMaxi := FRXMini + QLMax;
  FRYMaxi := FRYMini + QLMax;
  FRappScrReal     :=  PaintBoxVue.Width / QLMax;
  FInvRappScrReal  := 1 / FRappScrReal;
end;

function TfrmPrintingCenterExt.InitialiseByPointer(const QDocTopo  : TToporobotStructure2012;
                                                   const QBDD      : TBDDEntites;
                                                   const QCroquis  : TCroquisTerrain;
                                                   const QMaillage : TMaillage;
                                                   const QVue2DParams: TVue2DParams;
                                                   const QFiltres: string): boolean;
var
  i  : Integer;
  WU : TSetElementsDrawn;
  QDevelViseesVisibles: double;
  procedure GarnirLsbEchelles(const TT: array of integer; const EchelleParDefaut: integer);
  var
    t: Integer;
  begin
    cmbEchelles.OnChange := nil;
    cmbEchelles.Clear;
    for t := 0 to High(TT) do cmbEchelles.items.add(Format('%d', [TT[t]]));
    cmbEchelles.ItemIndex := cmbEchelles.Items.IndexOf(Format('%d', [EchelleParDefaut]));
    cmbEchelles.OnChange := self.cmbEchellesChange;
  end;
begin
  Result := False;
  FRedessinInProcess := false;
  AfficherMessage(Format('%s.InitialiseByPointer', [ClassName]));
  // affecter pointeur sur la BDD
  FDocuTopo       := QDocTopo;
  FBDDEntites     := QBDD;
  FCroquisTerrain := QCroquis;
  FMaillage       := QMaillage;
  FVue2DParams    := QVue2DParams;
  try
    FBDDEntites.MetaFiltre(QFiltres, QDevelViseesVisibles);
    // Initialisation de l'imprimante: Nécessite le paquet Printers4Lazarus,
    // qui est à ajouter à la liste des paquets requis par GHTopo
    // Ouvrir l'inspecteur de projet
    // Cliquer sur [ + ] en tete de la fenetre
    // Une fenetre s'ouvre
    // Onglet __/ Nouvelle Condition \__
    // Dérouler la première combobox
    // Sélectionner 'Printer4Lazarus'
    // Les unités Printers et OSPrinters sont INDISPENSABLES !!
    // Printer est une variable globale initialisée par le paquet.
    // Ne pas utiliser  TPrinter.Create;
    AfficherMessage('Fixation variables locales');
    FQdrCrossSize  := FVue2DParams.ongQdrSpc / 5;
    editTailleRegle.Value   := 2 * FVue2DParams.ongQdrSpc;
    GarnirLsbEchelles([100, 200, 400, 500, 800, 1000, 1250, 1500, 2000, 2500, 4000, 5000, 8000, 10000, 12500, 15000, 20000, 25000], QECHELLE_PAR_DEFAULT);
    FEchelle       := 1 / QECHELLE_PAR_DEFAULT;
    AfficherMessage('Setting printer');
    // définition de l'imprimante
    SetMyCurrentPrinter();
    Recadrer();
    DrawApercu();
    //------------------------------
    AfficherMessage('init OK');
    Result := True;
  except
  end;
end;

procedure TfrmPrintingCenterExt.Finaliser();
begin
  pass;
end;
// calcul des pages vides
// calculer le nombre de pages et celles qui sont vides
procedure TfrmPrintingCenterExt.CalcNbPagesAndEmpty();
var
  i,j, v: integer;
  E1    : TBaseStation;
  L1, H1: double;
  QPageWidthInMM, QPageHeightInMM, QInvEchelle: Double;

  R666  : TRect2Df;
  RVS   : TRect2Df;
  Grayed: boolean;
begin
  QPageWidthInMM  := Pixels2MillimetresX(Printer.PageWidth);
  QPageHeightInMM := Pixels2MillimetresY(Printer.PageHeight);

  AfficherMessage('Calcul du nombre de pages');
  // calculer le nombre de pages
  FNbPagesX := 1 + Trunc(1000 * (FRXMaxi - FRXMini) * FEchelle / QPageWidthInMM);
  FNbPagesY := 1 + Trunc(1000 * (FRYMaxi - FRYMini) * FEchelle / QPageHeightInMM);
  lbNbPages.Caption :=Format('%d x %d pages',[FNbPagesX, FNbPagesY]);
  SetLength(FTableauPagesDrawn, 0, 0);
  SetLength(FTableauPagesDrawn, FNbPagesX, FNbPagesY);
  QInvEchelle := 1 / (1000 * FEchelle);
  L1 := QPageWidthInMM  * QInvEchelle ;
  H1 := QPageHeightInMM * QInvEchelle;
  for i:=0 to FNbPagesX-1 do
    for j:=0 to FNbPagesY-1 do
    begin
      R666.X1 := FRXMini + i * L1;
      R666.Y1 := FRYMini + j * H1;

      R666.X2 := FRXMini + (1+i) * L1;
      R666.Y2 := FRYMini + (1+j) * H1;
      // supprimer les pages vides
      Grayed:=false;
      for v:=1 to FBDDEntites.GetNbEntitesVisees() - 1 do
      begin
        E1 := FBDDEntites.GetEntiteVisee(v);
        if (E1.Type_Entite = tgEntrance) then Continue;
        RVS.X1 := Min(E1.PosExtr0.X, E1.PosStation.X);
        RVS.Y1 := Min(E1.PosExtr0.Y, E1.PosStation.Y);

        RVS.X2 := Max(E1.PosExtr0.X, E1.PosStation.X);
        RVS.Y2 := Max(E1.PosExtr0.Y, E1.PosStation.Y);
        Grayed := Grayed or IntersectRectangles(R666, RVS);
      end;
      FTableauPagesDrawn[i,j]:=Grayed;
      AfficherMessage(Format('--> Page %d.%d: %s',[i, j, IIF((FTableauPagesDrawn[i,j]), 'Affichée', 'Masquée')]));
    end;
    FDoDrawPages:=True;
end;


//******************************************************************************
// dessin de l'aperçu
procedure TfrmPrintingCenterExt.DrawApercu();
var
  TmpBuffer      : TBitmap;
  R              : TRect;
  DC: TGHTopoDrawingContext;
  BGC: TBGRAPixel;
  // affichage des pages (avec mention de pages vides)
  procedure DrawPages();
  var
    i,j   : integer;
    P1, P2: TPoint;
    L1, H1, QInvEchelle, QPageWidthInMM, QPageHeightInMM: Double;
    QC1, QC2: TPoint2Df;
  begin
    if (not FDoDrawPages) then Exit;
    DC.DefineBrosseEtCrayon(bsSolid, clWhite, 255, psSolid, 0, clBlue, 255);
    DC.DefineFonte(DEFAULT_FONT_NAME, clBlue, [], 8);
    QInvEchelle := 1 / (1000*FEchelle);
    QPageWidthInMM  := Pixels2MillimetresX(Printer.PageWidth);
    QPageHeightInMM := Pixels2MillimetresY(Printer.PageHeight);
    L1 := QPageWidthInMM   * QInvEchelle;
    H1 := QPageHeightInMM  * QInvEchelle;
     for i := 0 to FNbPagesX-1 do
        for j:=0 to FNbPagesY-1 do
        begin
          //ShowMessageFmt('Page L%dC%d %s', [j, i, IIF(FTableauPagesDrawn[i,j], 'printed', 'ignored')]);
          QC1 := MakeTPoint2Df(FRXMini + i * L1, FRYMini + j * H1);
          QC2 := MakeTPoint2Df(FRXMini + (1+i) * L1, FRYMini + (1+j) * H1);
          DC.CanvasBGRA.Brush.Color := IIF(FTableauPagesDrawn[i,j], clWhite, clGray);
          DC.DrawRectangle(QC1, QC2, True);
        end;
     DC.RestoreBrosseEtCrayon();
     DC.RestoreFonte();
  end;
  procedure DrawBoundingBox();
  var
    QC1, QC2: TPoint3Df;
  begin
    DC.DefineBrosseEtCrayon(bsClear, clBlack, 255, psSolid, 0, clSilver, 192);
    QC1 := FBDDEntites.GetCoinBasGauche();
    QC2 := FBDDEntites.GetCoinHautDroit();
    DC.DrawRectangle(MakeTPoint2Df(QC1.X, QC1.Y), MakeTPoint2Df(QC2.X, QC2.Y), false);
    DC.RestoreBrosseEtCrayon();
  end;
  procedure DrawRegle(const TailleRegle: double);
  const
    NB_CARREAUX_X = 8;
  var
    i : Integer;
    Mg: double;
    QXo, QYo, QY1: double;
    LargeurCarreau, HauteurCarreau: double;
    QQR1, QQR2: TPoint2Df;
  begin
    Mg := (FRXMaxi - FRXMini) / 50; // marge
    QXo := FRXMini + Mg;
    QYo := FRYMini + Mg;
    LargeurCarreau := TailleRegle / NB_CARREAUX_X;
    HauteurCarreau := TailleRegle / 25;
    AfficherMessage(Format(' --> DrawRegle: L = %.0f m at (%.0f, %.0f)', [TailleRegle, FRXMini + Mg, FRYMini + Mg]));
    // cadre périmétrique
    DC.DefineBrosseEtCrayon(bsSolid, clWhite, 128, pssolid, 0, clBlack, 255);
    QQR1 := MakeTPoint2Df(QXo, QYo);
    QQR2 := MakeTPoint2Df(QQR1.X + TailleRegle, QQR1.Y + 2 * HauteurCarreau);
    DC.DrawRectangle(QQR1, QQR2, True);
    // carreaux alternés
    for i := 0 to NB_CARREAUX_X - 1 do
    begin
      if (Odd(i)) then QY1 := QYo + HauteurCarreau else QY1 := QYo;
      DC.DefineBrosse(bsSolid, clGray, 192);
      QQR1 := MakeTPoint2Df(QXo + (i * LargeurCarreau), QY1);
      QQR2 := MakeTPoint2Df(QQR1.X + LargeurCarreau, QQR1.Y + HauteurCarreau);
      DC.DrawRectangle(QQR1, QQR2, True);
    end;
    // légende
    QQR1 := MakeTPoint2Df(QXo, QYo);
    QQR2 := MakeTPoint2Df(QQR1.X + TailleRegle, QQR1.Y + 2 * HauteurCarreau);
    DC.DefineFonte(DEFAULT_FONT_NAME, clBlue, [fsBold], 14);
    DC.DrawTexte(QQR1.X, QQR2.Y + 1.0, 2, '0');
    DC.DrawTexte(QQR2.X, QQR2.Y + 1.0, 2, Format('%.0f m',[TailleRegle]));
    //QDrawTextLegende(QXo, QYo + 2 * HauteurCarreau + Mg/4, TailleRegle, HauteurCarreau);
  end;
begin
  AfficherMessage(Format('%s.DrawApercu',[ClassName]));
  if (FRedessinInProcess) then exit; // sémaphore 'FRedessinInProcess' est armé ? = On quitte (le dessin est en cours)
  FRedessinInProcess := True;        // Armement du sémaphore
  (*
  BGC := BGRA(Red(FVue2DParams.ongBackGround),
              Green(FVue2DParams.ongBackGround),
              Blue(FVue2DParams.ongBackGround),
              255);
  //*)
  BGC := BGRA(Red(clSilver), Green(clSilver), Blue(clSilver), 255);

  DC := TGHTopoDrawingContext.Create(PaintBoxVue.Width, PaintBoxVue.Height, BGC);
  try
    if (DC.Initialiser(FDocuTopo,
                       FBDDEntites,
                       FCroquisTerrain,
                       FMaillage,
                       FVue2DParams,
                       mslSERIE,
                       false)) then
    begin
      DC.SetBounds(FRXMini, FRYMini, FRXMaxi, FRYMaxi);
      DC.BeginDrawing();
        // les cadres de pages
        DrawBoundingBox();         // dessin des limites
        DrawPages();               // dessin des pages

        if (edQuadrilles     in FVue2DParams.ongElementsDrawn) then DC.DrawQuadrillage(FVue2DParams.ongQdrType, FVue2DParams.ongQdrColor, FVue2DParams.ongQdrSpc);
        //if (edANTENNES       in FVue2DParams.ongElementsDrawn) then DC.DrawAntennes();
        //if (edCrossSections  in FVue2DParams.ongElementsDrawn) then DC.DrawSections();
        if (edPolygonals     in FVue2DParams.ongElementsDrawn) then DC.DrawPolygonals(FVue2DParams.ongDrawFastCenterline);
        if (edFillGalerie    in FVue2DParams.ongElementsDrawn) then DC.DrawGaleries(FVue2DParams.ongDrawFastCenterline);
        if (edWalls          in FVue2DParams.ongElementsDrawn) then DC.DrawGaleries(FVue2DParams.ongDrawFastCenterline);
        if (edStations       in FVue2DParams.ongElementsDrawn) then DC.DrawStations();
        //if (edIDStations     in FVue2DParams.ongElementsDrawn) then DC.DrawIDStations();
        //if (edJONCTIONS      in FVue2DParams.ongElementsDrawn) then DC.DrawJonctions();
        //if (edAltitudes      in FVue2DParams.ongElementsDrawn) then DC.DrawCotation(false);
        //if (edCotes          in FVue2DParams.ongElementsDrawn) then DC.DrawCotation(true);
        //if (edPOI            in FVue2DParams.ongElementsDrawn) then DC.DrawPOIs();
        if (edENTRANCE_MKS   in FVue2DParams.ongElementsDrawn) then DC.DrawEntrancesOrPointEntities(edENTRANCE_NAMES   in FVue2DParams.ongElementsDrawn);
        if (chkRegle.Checked) then DrawRegle(editTailleRegle.Value);    // dessin de la règle
      DC.EndDrawing();
      DC.Draw(PaintBoxVue.Canvas, 0, 0, True);
    end;
  finally
    FRedessinInProcess := False; // libération
    FreeAndNil(DC);
  end;
end;
// retourne les index de pages en fonction des coordonnées réelles
procedure TfrmPrintingCenterExt.CalcIndexPageIncludingPt(const Pt: TPoint2Df; var IdxX, IdxY: integer);
var
  i,j   : integer;
  L1, H1, QInvEchelle, QPageWidthInMM, QPageHeightInMM: Double;
  R666  : TRect2Df;
begin
  IdxX:=-1;
  IdxY:=-1;
  AfficherMessage(Format('%s.CalcIndexPageIncludingPt(%.2f, %.2f)',
                         [ClassName, Pt.X, Pt.Y]));
  QInvEchelle := 1 / (1000*FEchelle);
  QPageWidthInMM  := Pixels2MillimetresX(Printer.PageWidth);
  QPageHeightInMM := Pixels2MillimetresY(Printer.PageHeight);

  L1 := QPageWidthInMM  * QInvEchelle;
  H1 := QPageHeightInMM * QInvEchelle;
  for i:=0 to FNbPagesX-1 do
  begin
    for j:=0 to FNbPagesY-1 do
    begin
      R666.X1 := FRXMini + i * L1;
      R666.Y1 := FRYMini + j * H1;
      R666.X2 := FRXMini + (1+i) * L1;
      R666.Y2 := FRYMini + (1+j) * H1;
      if IsInRange(Pt.X, R666.X1, R666.X2) and
         IsInRange(Pt.Y, R666.Y1, R666.Y2) then
      begin
        IdxX:=i; IdxY:=j;
        AfficherMessage(Format('Page %d, %d sélectionnée',[i,j]));
        Exit;
      end;
    end;  // for i,j
  end;
end;

//******************************************************************************
// imprimer la topo
// Statut: OK
procedure TfrmPrintingCenterExt.ImprimerLaTopo();
var
  L,C,N,Q: integer;
begin
  pnlPrintSettings.Visible     := False;
  pnlProgressPrinting.Visible  := True;
  lbPrintCurrent.Caption := GetResourceString(rsPRN_START_PRINTING);
  Printer.BeginDoc;
    AfficherMessage('>> 002');
    N:=0;
    progbarPrinting.Min := 0;
    Q := FNbPagesX * FNbPagesY;
    progbarPrinting.Max := Q;
    //Printer.NewPage;
    AfficherMessage('');
    AfficherMessage('');
    try
      for C:=0 to FNbPagesX-1 do
        for L:=0 to FNbPagesY-1 do
        begin
          Inc(N);
          progbarPrinting.Position := N;
          if (Not FTableauPagesDrawn[C,L]) then Continue;
          try

            AfficherMessage('=======================================================');
            AfficherMessage(Format('>> Impression de la page %d-%d - 003',[c,l]));
            //Printer.BeginPage;
              PrintAPage(FEchelle, L, C);
            //Printer.EndPage;
            AfficherMessage('done');
            Printer.NewPage;
            Application.ProcessMessages;
            AfficherMessage('');
            ///lbPrintCurrent.Caption:= Format('Printing %d/%d', [N,Q]);
          except
            on E: Exception do AfficherMessage(E.Message);
            //AfficherMessage(Format('>> Printing error in page %d %d',[C,L]));
          end;
        end;
     Printer.EndDoc;
     AfficherMessage(GetResourceString(rsDONE_PRINTING));
    except
      on E: Exception do AfficherMessage(E.Message);
    end;
  pnlProgressPrinting.Visible  := False;
  pnlPrintSettings.Visible     := True;
end;
//******************************************************************************
// imprimer une page
// Statut: Opérationnel mais qques détails à revoir
// TODO: Pb de lignes superflues dans les entrées
// TODO: Optimiser ce code
procedure TfrmPrintingCenterExt.PrintAPage(const Echelle: double; const L,C: integer);
const
  RD = 8;
var
  WU: String;
  DC: TGHTopoDrawingContext;
  QLargeurZone, QHauteurZone, QPZX1, QPZY1, QPZX2, QPZY2: Double;
  PRNRect: TRect;
  function Coord2Draft(const Xf, Yf: double): TPoint;
  var
    QXf, QYf: Double;
  begin
    // transformation en millimètres et mise à l'échelle
    QXf:=((-FRXMini+Xf) * 1000.00) * Echelle;
    QYf:=((-FRYMini+Yf) * 1000.00) * Echelle;

    Result.x := -Printer.PageWidth  * C + Millimetres2PixelsXf(QXf);
    Result.y :=  Printer.PageHeight * L +
                 Printer.PageHeight - Millimetres2PixelsYf(QYf);
  end;
  {$IFDEF MSWINDOWS}
    {$DEFINE PRINTER_BGRA}
    {$UNDEF PRINTER_BGRA}
  {$ENDIF}
  {$IFDEF LINUX}
    {$UNDEF PRINTER_BGRA}
  {$ENDIF}
  {$IFNDEF PRINTER_BGRA}
    {$INCLUDE SubsOfPRNPrintAPage.inc}
  {$ENDIF}
begin
  {$IFDEF PRINTER_BGRA}
  PRNRect := Rect(0, 0, Printer.PageWidth, Printer.PageHeight);

  QLargeurZone := (FRXMaxi - FRXMini) / FNbPagesX;
  QHauteurZone := (FRYMaxi - FRYMini) / FNbPagesY;
  QPZX1 := FRXMini + C * QLargeurZone;
  QPZY1 := FRYMini + L * QHauteurZone;
  QPZX2 := QPZX1 + QLargeurZone;
  QPZY2 := QPZY1 + QHauteurZone;
  AfficherMessage(Format('L%dC%d: %dx%d, (%f, %f) -> (%f, %f)',
                        [L, C, Printer.PageWidth, Printer.PageHeight,
                         QPZX1, QPZY1, QPZX2, QPZY2]));
  AfficherMessage('Creation page');
  DC := TGHTopoDrawingContext.Create(Printer.PageWidth, Printer.PageHeight, clWhite);
  if (DC.Initialiser(FDocuTopo,
                     FBDDEntites,
                     FCroquisTerrain,
                     FMaillage,
                     FVue2DParams,
                     mslSERIE, false)) then
  begin
    try
      DC.SetBounds(QPZX1, QPZY1, QPZX2, QPZY2);
      DC.BeginDrawing();
        if (edQuadrilles     in FVue2DParams.ongElementsDrawn) then DC.DrawQuadrillage(FVue2DParams.ongQdrType, FVue2DParams.ongQdrColor, FVue2DParams.ongQdrSpc);
        if (edANTENNES       in FVue2DParams.ongElementsDrawn) then DC.DrawAntennes();
        if (edCrossSections  in FVue2DParams.ongElementsDrawn) then DC.DrawSections();
        if (edPolygonals     in FVue2DParams.ongElementsDrawn) then DC.DrawPolygonals();
        if (edFillGalerie    in FVue2DParams.ongElementsDrawn) then DC.DrawGaleries();
        if (edWalls          in FVue2DParams.ongElementsDrawn) then DC.DrawGaleries();
        if (edStations       in FVue2DParams.ongElementsDrawn) then DC.DrawStations();
        if (edIDStations     in FVue2DParams.ongElementsDrawn) then DC.DrawIDStations();
        if (edJONCTIONS      in FVue2DParams.ongElementsDrawn) then DC.DrawJonctions();
        if (edAltitudes      in FVue2DParams.ongElementsDrawn) then DC.DrawCotation(false);
        if (edCotes          in FVue2DParams.ongElementsDrawn) then DC.DrawCotation(true);
        if (edPOI            in FVue2DParams.ongElementsDrawn) then DC.DrawPOIs();
        if (edENTRANCES      in FVue2DParams.ongElementsDrawn) then DC.DrawEntrancesOrPointEntities();
        AfficherMessage('1000');

      DC.EndDrawing();
        AfficherMessage('1010');
        DC.Draw(Printer.Canvas, 0, 0, True); // OK avec Windows, KO avec Linux GDK
        AfficherMessage('1011');
      AfficherMessage('1012');
    finally
      FreeAndNil(DC);
    end;
  end;
  {$ELSE}
  try
    AfficherMessage(Format('Impression carré %d.%d (page %d)',[L,C, Printer.PageNumber]));
    WU := ChooseString(Ord(Printer.Orientation), ['PORTRAIT', 'LANDSCAPE', 'REVERSE LANDSCAPE', 'REVERSEPORTRAIT']);

    AfficherMessage('-- Orientation: ' + WU);
    DrawQuadrilles();
    DrawGaleries();
    if (edANTENNES      in FVue2DParams.ongElementsDrawn) then DrawViseesRadiantes();
    if (edPolygonals    in FVue2DParams.ongElementsDrawn) then DrawPolygonals(edFillGalerie in FVue2DParams.ongElementsDrawn);
    if (edENTRANCE_MKS  in FVue2DParams.ongElementsDrawn) then DrawEntrances(edENTRANCE_NAMES in FVue2DParams.ongElementsDrawn);
    if (edCROQUIS       in FVue2DParams.ongElementsDrawn) then DrawCroquis(); // DrawAnnotations;
    if (edcrossSections in FVue2DParams.ongElementsDrawn) then DrawSections();
    if (edStations      in FVue2DParams.ongElementsDrawn) then DrawStations();
    if (edIDStations    in FVue2DParams.ongElementsDrawn) then DrawIDStations();
    if (edAltitudes     in FVue2DParams.ongElementsDrawn) then DrawCotationStations(false);
    if (edCotes         in FVue2DParams.ongElementsDrawn) then DrawCotationStations(True);
    if (edJONCTIONS     in FVue2DParams.ongElementsDrawn) then DrawJonctions();
    if (edPOI           in FVue2DParams.ongElementsDrawn) then DrawPOIs();
    DrawCorners();         // coins de découpage
    DrawCartouche();       // Cartouche
    if (chkRegle.Checked) then DrawRegle(editTailleRegle.Value); // Règle
  except
  end;
  {$ENDIF PRINTER_BGRA}
end;
//******************************************************************************
procedure TfrmPrintingCenterExt.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Finaliser();
end;

procedure TfrmPrintingCenterExt.btnPreviewClick(Sender: TObject);
begin
  Recadrer();
  DrawApercu();
end;

procedure TfrmPrintingCenterExt.btnStartImpressionClick(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Lancer l''impression ?')) then ImprimerLaTopo();
end;

procedure TfrmPrintingCenterExt.btnApplyParams2DClick(Sender: TObject);
begin
  if (FVue2DParams.ongQdrSpc < 5.00) then FVue2DParams.ongQdrSpc := 5.00;
  //CalcNbPagesAndEmpty;   // inutile ici
  Recadrer();
  DrawApercu();
end;

procedure TfrmPrintingCenterExt.btnParametresVueClick(Sender: TObject);
begin
  if (ParametrerOngletVue2D(FVue2DParams)) then DrawApercu();
end;



procedure TfrmPrintingCenterExt.cmbEchellesChange(Sender: TObject);
var
  WU: LongInt;
begin
  WU := StrToIntDef(cmbEchelles.Items[cmbEchelles.ItemIndex], 1000);
  FEchelle := 1 / WU;
  CalcNbPagesAndEmpty();
  Recadrer();
  DrawApercu();
end;

procedure TfrmPrintingCenterExt.btnSelectPrinterClick(Sender: TObject);
begin
  SelectionnerImprimante(Printer);
  SetMyCurrentPrinter();
end;


procedure TfrmPrintingCenterExt.FormCreate(Sender: TObject);
begin
  chkRegle.Caption             := GetResourceString(rsREGLE);
  grbxEchelles.Caption         := GetResourceString(rsECHELLE);
  grbPrinterNames.Caption      := GetResourceString(rsPRN_TBPRINTER);
  lbEchelle.Caption            := GetResourceString(rsECHELLE);
  btnStartImpression.Caption   := GetResourceString(rsSTARTPRINTING);
  btnPreview.Caption           := GetResourceString(rsPREVIEW);
  // règle
  chkRegle.Checked             := true;
  editTailleRegle.Value        := 100.00;
end;

procedure TfrmPrintingCenterExt.PaintBoxVueDblClick(Sender: TObject);
var
  PM: TPoint2Df;
  IdxX, IdxY: integer;
begin
  PM    := GetCoordsMonde(FPP);
  IdxX  := 0; IdxY := 0;
  CalcIndexPageIncludingPt(PM, IdxX, IdxY);
  if (IdxX > -1) and (IdxY>-1) then begin
    FTableauPagesDrawn[IdxX, IdxY] := Not(FTableauPagesDrawn[IdxX, IdxY]);
    DrawApercu();
  end;
end;

procedure TfrmPrintingCenterExt.PaintBoxVueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PM: TPoint2Df;
begin
  try
    FPP := MakeTPoint(X, Y);
    PM := GetCoordsMonde(FPP);
    lbMouseCoordinates.Caption := Format('%.2f, %.2f',[PM.X, PM.Y]);
  except
  end;
end;

procedure TfrmPrintingCenterExt.PaintBoxVuePaint(Sender: TObject);
begin
  DrawApercu();
end;

end.

