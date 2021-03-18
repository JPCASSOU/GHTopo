// Afficheur de listes incorporées ds les viewers
// (réseaux, secteurs, dates, profondeurs)
// Date: 03/05/2013
// 29/05/2015: Interaction entre ce cadre et les autres modules
// 27/07/2017: Possibilité de surligner par réseaux, secteurs, codes, expés
//             en plus du surlignage par séries
{$INCLUDE CompilationParameters.inc}
unit CadreListesPourVueGraphique;
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, FileUtil, Graphics,
  Forms, Controls, CheckLst, ComCtrls, StdCtrls, Grids, LCLType,
  Dialogs,
  StructuresDonnees,
  Common,
  CallDialogsStdVersion,
  //UnitClassPalette,
  UnitEntitesExtended,
  CadreGHTopoContext2D, // pour la localisation
  ToporobotClasses2012,
  unitobjetserie, UnitClassPalette, types;

type TIndexItem = record
  aIdx     : integer;
  aChecked : boolean;
end;

type

{ TCdrListesPourVisualisateurs }

 TCdrListesPourVisualisateurs = class(TFrame)
    btnLocaliser: TButton;
    btnHighlightSelected: TButton;
    btnCocher: TButton;
    btnHelp: TButton;
    btnDeselectAll: TButton;
    btnConvertirEnFiltre: TButton;
    btnExtraireLesIndex: TButton;
    chkDoHighLightCurrentItem: TCheckBox;
    editSeriesARetenir: TEdit;
    lsbListeElements: TCheckListBox;
    cmbModeListe: TComboBox;
    hcColsTitres: THeaderControl;
    lbTitreListe: TLabel;
    lbRegenerationListe: TStaticText;
    procedure btnCocherClick(Sender: TObject);
    procedure btnConvertirEnFiltreClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnExtraireLesIndexClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnLocaliserClick(Sender: TObject);
    procedure btnHighlightSelectedClick(Sender: TObject);
    procedure chkDoHighLightCurrentItemChange(Sender: TObject);
    procedure cmbModeListeChange(Sender: TObject);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbListeElementsClick(Sender: TObject);
    procedure lsbListeElementsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbListeElementsSelectionChange(Sender: TObject; User: boolean);
  private
    FDocToporobot    : TToporobotStructure2012;
    FTableEntites    : TBDDEntites; // pour le mode 'Stations'
    FCadreVue2D      : TGHTopoContext2DA;
    FUseInternalIndex: boolean;
    FModeSelection   : TModeSelectionListe;
    FNbItems         : LongInt;
    FTableauIndex    : array of TIndexItem;       // tableau d'index

    function  SetTIndexItem(const QIdx: integer; const QChecked: boolean): TIndexItem;
    function  FindInternalIdx(const Idx: integer): integer;
    procedure RefreshListe();
    procedure SurlignerItem();
    procedure DoHighLightSelectedItems();
  public
    function InitialiseListeSelection(const DT: TToporobotStructure2012;
                                      const DE: TBDDEntites;
                                      const GHVue2D: TGHTopoContext2DA;
                                      const MS: TModeSelectionListe;
                                      const QIndex: integer;
                                      const UseInternalIndex: boolean;
                                      const DoReinitFull: boolean): boolean;
    function GetModeSelection(): TModeSelectionListe;
  end;

implementation

{$R *.lfm}

{ TCdrListesPourVisualisateurs }
function TCdrListesPourVisualisateurs.InitialiseListeSelection(const DT: TToporobotStructure2012;
                                                               const DE: TBDDEntites;
                                                               const GHVue2D: TGHTopoContext2DA;
                                                               const MS: TModeSelectionListe;
                                                               const QIndex: integer;
                                                               const UseInternalIndex: boolean;
                                                               const DoReinitFull: boolean): boolean;
var
  i      : integer;
  miou   : string;
  ht     : THeaderSection;
  CC     : TCode;
  EE     : TExpe;
  SSR    : TObjSerie;
  RR     : TReseau;
  SS     : TSecteur;
  procedure AjouterTitreColonne(const Titre: string; const LG: integer);
  begin
    ht          := hcColsTitres.Sections.Add;
    ht.Text     := GetResourceString(Titre);
    ht.MinWidth := LG;
  end;
begin
  Result := False;
  FUseInternalIndex := UseInternalIndex;
  AfficherMessage(Format('%s.InitialiseListeSelection()', [classname]));
  if (DoReinitFull) then
  begin
    FDocToporobot  := DT;
    FTableEntites  := DE;
    FModeSelection := MS;
    FCadreVue2D    := GHVue2D;
    cmbModeListe.Clear;
    cmbModeListe.Items.Add(GetResourceString(rsITEM_ENTRANCES));         // mslENTRANCES
    cmbModeListe.Items.Add(GetResourceString(rsITEM_RESEAUX));           // mslRESEAUX
    cmbModeListe.Items.Add(GetResourceString(rsITEM_SECTEURS));          // mslSECTEURS
    cmbModeListe.Items.Add(GetResourceString(rsITEM_CODES));             // mslCODE
    cmbModeListe.Items.Add(GetResourceString(rsITEM_EXPES));             // mslEXPE
    cmbModeListe.Items.Add(GetResourceString(rsITEM_SERIES));            // mslSERIE
    cmbModeListe.Items.Add(GetResourceString(rsITEM_TYPE_VISEE));        // mslTYPE_GALERIE
    cmbModeListe.Items.Add(GetResourceString(rsITEM_DATES));             // mslDATE
    cmbModeListe.Items.Add(GetResourceString(rsITEM_NAMESPACES));        // mslDATE
    cmbModeListe.ItemIndex := Ord(FModeSelection);
  end;

  btnHighlightSelected.Caption := GetResourceString(rsBTN_HIGHLIGHT_SELECTED);

  // purge des titres des headers
  hcColsTitres.Sections.Clear;
  AjouterTitreColonne('V' , 24);
  // purge des listes
  lsbListeElements.Clear;
  lsbListeElements.ItemHeight := 22;
  // titres
  case FModeSelection of
    mslENTRANCES: begin
                  miou := GetResourceString(rsSELECT_LISTE_ENTREES);
                  FNbItems          := FDocToporobot.GetNbEntrances();
                  self.Caption      := GetResourceString(rsSELECT_LISTE_ENTRANCE);
                  AjouterTitreColonne(rsSELECT_LISTE_ENTREES_ID     , 40);
                  AjouterTitreColonne(''                            , 26);
                  AjouterTitreColonne(rsSELECT_LISTE_ENTREES_NOM    , 300);
                  AjouterTitreColonne(rsSELECT_LISTE_ENTREES_REF    , 80);
                  AjouterTitreColonne(rsSELECT_LISTE_ENTREES_X      , 100);
                  AjouterTitreColonne(rsSELECT_LISTE_ENTREES_Y      , 100);
                  AjouterTitreColonne(rsSELECT_LISTE_ENTREES_Z      , 100);
                end;
    mslRESEAUX: begin
                  miou              := GetResourceString(rsSELECT_LISTE_RESEAUX);
                  FNbItems          := FDocToporobot.GetNbReseaux();
                  self.Caption      := GetResourceString(rsSELECT_LISTE_RESEAU);
                  AjouterTitreColonne(rsSELECT_LISTE_RESEAUX_ID     , 40);
                  AjouterTitreColonne(''                            , 26);
                  AjouterTitreColonne(rsSELECT_LISTE_RESEAUX_NOM    , 300);
                end;
    mslSECTEURS:begin
                  miou              := GetResourceString(rsSELECT_LISTE_SECTEURS);
                  FNbItems          := FDocToporobot.GetNbSecteurs;
                  self.Caption      := GetResourceString(rsSELECT_LISTE_SECTEUR);
                  AjouterTitreColonne(rsSELECT_LISTE_SECTEURS_ID    , 40);
                  AjouterTitreColonne(''                            , 26);
                  AjouterTitreColonne(rsSELECT_LISTE_SECTEURS_NOM   , 300);
                end;
    mslCODE:    begin
                  miou              := GetResourceString(rsSELECT_LISTE_CODES);
                  FNbItems          := FDocToporobot.GetNbCodes;
                  self.Caption      := GetResourceString(rsSELECT_LISTE_CODE);
                  AjouterTitreColonne(rsSELECT_LISTE_CODES_ID       , 40);
                  AjouterTitreColonne(rsSELECT_LISTE_CODES_AZIMUTS  , 80);
                  AjouterTitreColonne(rsSELECT_LISTE_CODES_PENTES   , 80);
                  AjouterTitreColonne(rsSELECT_LISTE_CODES_OBS      , 500);
                end;
    mslEXPE:   begin
                  miou               := GetResourceString(rsSELECT_LISTE_EXPES);
                  FNbItems           := FDocToporobot.GetNbExpes;
                  self.Caption       := GetResourceString(rsSELECT_LISTE_EXPE);
                  AjouterTitreColonne(rsSELECT_LISTE_EXPES_ID             , 40);
                  AjouterTitreColonne(rsSELECT_LISTE_EXPES_COULEUR        , 80);
                  AjouterTitreColonne(rsSELECT_LISTE_EXPES_DATE           , 90);
                  AjouterTitreColonne(rsSELECT_LISTE_EXPES_OPERATEUR_TOPO , 140);
                  AjouterTitreColonne(rsSELECT_LISTE_EXPES_CLUB_SPELEO    , 140);
                  AjouterTitreColonne(rsSELECT_LISTE_EXPES_DECLINAISON    , 70);
                  AjouterTitreColonne(rsSELECT_LISTE_EXPES_OBS            , 500);
                end;
    mslSERIE:  begin
                  miou := GetResourceString(rsSELECT_LISTE_SERIES);
                  FNbItems := FDocToporobot.GetNbSeries;
                  self.Caption      := GetResourceString(rsSELECT_LISTE_SERIE);
                  AjouterTitreColonne(rsSELECT_LISTE_SERIES_ID            , 60);
                  AjouterTitreColonne(rsSELECT_LISTE_SERIES_DEPART        , 90);
                  AjouterTitreColonne(rsSELECT_LISTE_SERIES_ARRIVEE       , 90);
                  AjouterTitreColonne(rsSELECT_LISTE_SERIES_NOM           , 400);
                  AjouterTitreColonne(rsSELECT_LISTE_SERIES_RESEAU        , 300);
                  AjouterTitreColonne(rsSELECT_LISTE_SERIES_NBPOINTS      , 80);
                end;
    mslDATE:
               begin
                  miou := GetResourceString(rsSELECT_LISTE_DATES);
                  FNbItems      := FTableEntites.GetNbDates;
                  self.Caption  := GetResourceString(rsSELECT_LISTE_DATES);
                  AjouterTitreColonne(rsSELECT_LISTE_UNE_DATE, 120);
               end;
    mslNAMESPACES:
                begin
                  miou := GetResourceString(rsSELECT_LISTE_NAMESPACE);
                  FNbItems      := FDocToporobot.GetNbNameSpaces();
                  self.Caption  := GetResourceString(rsSELECT_LISTE_NAMESPACE);
                  AjouterTitreColonne(rsSELECT_LISTE_NAMESPACE_COULEUR     , 80);
                  AjouterTitreColonne(rsSELECT_LISTE_NAMESPACE_NOM         , 200);
                  AjouterTitreColonne(rsSELECT_LISTE_NAMESPACE_DESCRIPTION , 90);
                end;
  end;
  lbTitreListe.Caption := Format(GetResourceString(rsSELECT_LISTE_NB_ELEMENTS), [FNbItems, miou]);
  lbRegenerationListe.Caption := GetResourceString('Regen liste');
  Result:=False;
  // liste
  try
    lsbListeElements.Visible     := False;
    lbRegenerationListe.Visible  := True;

    SetLength(FTableauIndex, 0);
    SetLength(FTableauIndex, FNbItems);
    for i := 0 to High(FTableauIndex) do
    begin
      FTableauIndex[i].aChecked  := false;
      FTableauIndex[i].aIdx      := 0;
    end;
    //ShowMessageFmt('FNbItems = %d', [FNbItems]);
    for i := 0 to FNbItems - 1 do
    begin
      try
        case FModeSelection of
        mslRESEAUX:
          begin
            RR := FDocToporobot.GetReseau(i);
            FTableauIndex[i] := SetTIndexItem(i, True);
          end;
        mslSECTEURS:
          begin
            SS := FDocToporobot.GetSecteur(i);
            FTableauIndex[i] := SetTIndexItem(i, True);
          end;
        mslCODE :
          begin
            CC := FDocToporobot.GetCode(i);
            FTableauIndex[i] := SetTIndexItem(CC.IDCode, True);
          end;
        mslEXPE :
          begin
            EE := FDocToporobot.GetExpe(i);
            FTableauIndex[i] := SetTIndexItem(EE.IDExpe, True);
          end;
        mslSERIE :
          begin
            SSR := FDocToporobot.GetSerie(i);
            FTableauIndex[i] := SetTIndexItem(SSR.GetNumeroDeSerie(), True);
          end;
        else
          FTableauIndex[i] := SetTIndexItem(i, True);
        end;

      except
        AfficherMessageErreur(Format('-- Erreur in %d', [i]));

      end;
      // Pour un TListBox avec éléments dessinés par l'utilisateur
      // TOUJOURS ajouter un item non vide sous peine de SIGSEGV
      // (typiquement si la liste est triée et refuse les doublons)
      lsbListeElements.Items.Add(Format('Item%d', [i]));  // Nécessaire et suffisant
      //lsbListeElements.Items.Add('');                   // /!\ DANGER: Sigsegv
    end;
    //FNumeroElement := IIF(FUseInternalIndex, QIndex, FindInternalIdx(QIndex));

    lsbListeElements.ItemIndex := 0; // FNumeroElement;
    chkDoHighLightCurrentItem.Caption := 'Surligner';
    Result := True;
    lsbListeElements.Visible := True;
   lbRegenerationListe.Visible  := False;
 except
   pass;
 end;
end;

procedure TCdrListesPourVisualisateurs.DoHighLightSelectedItems();
var
  EWE: String;
  i, QIdx, NbS: Integer;
  MySerie: TObjSerie;
  QArrEntrancesHighlighted: array of TNumeroEntrance;
  QArrSeriesHighlighted   : array of TNumeroSerie;
  QArrCodesHighlighted    : array of TNumeroCode;
  QArrExpesHighlighted    : array of TNumeroExpe;
  QArrReseauxHighlighted  : array of TNumeroReseau;
  QArrSecteursHighlighted : array of TNumeroSecteur;


  MyExpe: TExpe;
  myReseau: TReseau;
begin
  if (Length(FTableauIndex) = 0) then Exit;
  NbS := 0;


  FTableEntites.DisHighLightAll();
  case FModeSelection of
    mslENTRANCES:
      begin
        SetLength(QArrEntrancesHighlighted , NbS);
        EWE := '';
        for i := 0 to Length(FTableauIndex) - 1 do
        begin
          if (lsbListeElements.Checked[i]) then
          begin
            SetLength(QArrEntrancesHighlighted, NbS + 1);
            QArrEntrancesHighlighted[NbS] := FTableauIndex[i].aIdx;
            EWE += Format('%d;', [QArrEntrancesHighlighted[NbS]]);
            NbS += 1;
          end;
        end;
        FTableEntites.HighlightEntrances(QArrEntrancesHighlighted);
        SetLength(QArrEntrancesHighlighted, 0);
        FCadreVue2D.RefreshDessin();
      end;
    mslRESEAUX:
      begin
        SetLength(QArrReseauxHighlighted , NbS);
        EWE := '';
        for i := 0 to Length(FTableauIndex) - 1 do
        begin
          if (lsbListeElements.Checked[i]) then
          begin
            SetLength(QArrReseauxHighlighted, NbS + 1);
            QArrReseauxHighlighted[NbS] := FTableauIndex[i].aIdx;
            EWE += Format('%d;', [QArrReseauxHighlighted[NbS]]);
            NbS += 1;
          end;
        end;
        FTableEntites.HighlightReseaux(QArrReseauxHighlighted);
        SetLength(QArrReseauxHighlighted, 0);
        FCadreVue2D.RefreshDessin();
      end;
    mslSECTEURS :
      begin
        SetLength(QArrSecteursHighlighted , NbS);
        EWE := '';
        for i := 0 to Length(FTableauIndex) - 1 do
        begin
          if (lsbListeElements.Checked[i]) then
          begin
            SetLength(QArrSecteursHighlighted, NbS + 1);
            QArrSecteursHighlighted[NbS] := FTableauIndex[i].aIdx;
            EWE += Format('%d;', [QArrSecteursHighlighted[NbS]]);
            NbS += 1;
          end;
        end;
        FTableEntites.HighlightSecteurs(QArrSecteursHighlighted);
        SetLength(QArrSecteursHighlighted, 0);
        FCadreVue2D.RefreshDessin();
      end;
    mslCODE:
      begin
        SetLength(QArrCodesHighlighted , NbS);
        EWE := '';
        for i := 0 to Length(FTableauIndex) - 1 do
        begin
          if (lsbListeElements.Checked[i]) then
          begin
            MyExpe := FDocToporobot.GetExpeByNumero(FTableauIndex[i].aIdx);
            SetLength(QArrCodesHighlighted, NbS + 1);
            QArrCodesHighlighted[NbS] := MyExpe.IDExpe;
            EWE += Format('%d;', [QArrCodesHighlighted[NbS]]);
            NbS += 1;
          end;
        end;
        FTableEntites.HighlightCodes(QArrCodesHighlighted);
        SetLength(QArrCodesHighlighted, 0);
        FCadreVue2D.RefreshDessin();
      end;
    mslEXPE:
      begin
        SetLength(QArrExpesHighlighted , NbS);
        EWE := '';
        for i := 0 to Length(FTableauIndex) - 1 do
        begin
          if (lsbListeElements.Checked[i]) then
          begin
            MyExpe := FDocToporobot.GetExpeByNumero(FTableauIndex[i].aIdx);
            SetLength(QArrExpesHighlighted, NbS + 1);
            QArrExpesHighlighted[NbS] := MyExpe.IDExpe;
            EWE += Format('%d;', [QArrExpesHighlighted[NbS]]);
            NbS += 1;
          end;
        end;
        FTableEntites.HighlightExpes(QArrExpesHighlighted);
        SetLength(QArrExpesHighlighted, 0);
        FCadreVue2D.RefreshDessin();
      end;
    mslSERIE:
      begin
        SetLength(QArrSeriesHighlighted, NbS);
        EWE := '';
        for i := 0 to Length(FTableauIndex) - 1 do
        begin
          if (lsbListeElements.Checked[i]) then
          begin
            FDocToporobot.GetSerieByNumeroSerie(FTableauIndex[i].aIdx, MySerie, QIdx);
            SetLength(QArrSeriesHighlighted, NbS + 1);
            QArrSeriesHighlighted[NbS] := MySerie.GetNumeroDeSerie();
            EWE += Format('%d;', [QArrSeriesHighlighted[NbS]]);
            NbS += 1;
          end;
        end;
        FTableEntites.HighlightSeries(QArrSeriesHighlighted);
        SetLength(QArrSeriesHighlighted, 0);
        FCadreVue2D.RefreshDessin();
      end;
    mslTYPE_GALERIE: pass;
    mslDATE        : pass;
    mslNAMESPACES  : pass;
  end;
end;

function TCdrListesPourVisualisateurs.GetModeSelection(): TModeSelectionListe;
begin
  Result := FModeSelection;
end;

procedure TCdrListesPourVisualisateurs.lsbListeElementsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
  // lgr = 25;
var
  rs: TReseau;
  es: TEntrance;
  cs: TCode;
  ss: TExpe;
  sr: TObjSerie;
  sc: TSecteur;
  dt: TDateTime;
  // /!\ Ne pas utiliser les fonctions homonymes dédiées aux TListBox
  procedure QDessineCoche(const IsChecked: boolean);
  var
    QR: TRect;
    H: Integer;
  begin
    {$IFDEF MSWINDOWS}
    with lsbListeElements.Canvas do
    begin
      Pen.Style    := psSolid;
      Brush.Style  := bsSolid;
      Pen.Color    := clBlack;
      QR.Left      := ARect.Left   + 1;
      QR.Top       := ARect.Top    + 1;
      QR.Bottom    := ARect.Bottom - 1;
      H            := QR.Bottom - QR.Top;
      QR.Right     := QR.Left + H;
      Brush.Color  := IIF(IsChecked, clBlue, clWhite);
      Rectangle(QR);
    end;
    {$ELSE}
    pass; // Sous Linux, les cases sont dessinées différemment
    {$ENDIF}
  end;
  // bg = couleur de fond; tc= couleur du texte
  procedure DessineItem(const bg,tc: TColor; const QChecked: boolean);
  VAR
    QIdxNameSpace, QNoSerie: integer;
    EWE: String;
    MyNamespace: TNameSpace;
    PP: TPalette256;
    procedure QDessineFiletColonne(const TB: integer); inline;
    begin
      lsbListeElements.Canvas.Line(TB, ARect.Top, TB, ARect.Bottom);
    end;
    procedure QDessineRectangleColore(const QBG, PC, BC: TColor; const L, R, T, B: integer);
    begin
      lsbListeElements.canvas.Pen.Color   := PC;
      lsbListeElements.canvas.Brush.Color := BC;
      lsbListeElements.canvas.Rectangle(Rect(L, T, R, B));
      // et on restaure l'ancienne couleur
      lsbListeElements.Canvas.Brush.Color := QBG;
      lsbListeElements.canvas.Pen.Color   := clSilver;
    end;
    procedure QResetColorRow(const qbg, qtc: TColor);
    begin
      lsbListeElements.Canvas.Brush.Color:= qbg;
      lsbListeElements.Canvas.Font.Color := qtc;
      lsbListeElements.Canvas.Pen.Color  := clSilver; // pour les filets
      lsbListeElements.Canvas.FillRect(ARect);
    end;
    procedure QDrawColTexte(const QHS: THeaderSection; const DoDrawFilet: boolean; const QText: string);
    var
      QR: TRect;
    begin
      QR := Rect(QHS.Left, ARect.Top + mg, QHS.Right - 8, ARect.Bottom - mg);
      if (DoDrawFilet) then QDessineFiletColonne(QHS.Left - Q4);
      lsbListeElements.canvas.TextRect(QR, QHS.Left + 4, ARect.Top+1, _AnsiToLCLStr(QText));
    end;
    procedure QDrawColRectColoreWithTexte(const QHS: THeaderSection; const QDoDrawFilet: boolean; const QRectColor: TColor; const QText: string);
    var
      QOlgBG: TColor;
    begin
      QOlgBG := lsbListeElements.Canvas.Brush.Color;
      if (QDoDrawFilet) then QDessineFiletColonne(QHS.Left - Q4);
      if (Trim(QText) <> '') then
      begin
        QDessineRectangleColore(bg, clBlack, QRectColor, QHS.Left, QHS.Left + 32, ARect.Top + mg, ARect.Bottom - mg);
        lsbListeElements.Canvas.TextOut(QHS.Left + 32 + 4, ARect.Top+1, _AnsiToLCLStr(QText));
      end else
        QDessineRectangleColore(bg, clBlack, QRectColor, QHS.Left, QHS.Right - 4, ARect.Top + mg, ARect.Bottom - mg);
      lsbListeElements.Canvas.Brush.Color := QOlgBG;
    end;
  begin
    case FModeSelection of
      mslENTRANCES:
        begin
          QResetColorRow(bg, tc);
          QDrawColTexte(hcColsTitres.Sections.Items[1], false, Format(FORMAT_NB_INTEGER,[Index]));
          QDrawColTexte(hcColsTitres.Sections.Items[3], True , es.eNomEntree);
          QDrawColTexte(hcColsTitres.Sections.Items[4], True , Format(FMTSERST,[es.eRefSer, es.eRefSt]));
          QDrawColTexte(hcColsTitres.Sections.Items[5], True , FormatterNombreAvecSepMilliers(es.eXEntree));
          QDrawColTexte(hcColsTitres.Sections.Items[6], True , FormatterNombreAvecSepMilliers(es.eYEntree));
          QDrawColTexte(hcColsTitres.Sections.Items[7], True , FormatterNombreAvecSepMilliers(es.eZEntree));
        end;
      mslRESEAUX:
        begin
          QResetColorRow(bg, tc);
          QDrawColTexte(hcColsTitres.Sections.Items[1], False, Format(FORMAT_NB_INTEGER,[Index]));
          QDrawColRectColoreWithTexte(hcColsTitres.Sections.Items[2], True, rs.ColorReseau, '');
          QDrawColTexte(hcColsTitres.Sections.Items[3], True, rs.NomReseau);
        end;
      mslSECTEURS:
        begin
          QResetColorRow(bg, tc);
          QDrawColTexte(hcColsTitres.Sections.Items[1], False, Format(FORMAT_NB_INTEGER,[Index]));
          QDrawColRectColoreWithTexte(hcColsTitres.Sections.Items[2], True, sc.CouleurSecteur, '');
          QDrawColTexte(hcColsTitres.Sections.Items[3], True , sc.NomSecteur);
        end;
      mslCODE:
        begin
          QResetColorRow(bg, tc);
          QDrawColTexte(hcColsTitres.Sections.Items[1], False, Format(FORMAT_NB_INTEGER, [cs.IDCode]));
          QDrawColTexte(hcColsTitres.Sections.Items[2], True , Format(FORMAT_NB_REAL_0_DEC, [cs.GradAz]) );
          QDrawColTexte(hcColsTitres.Sections.Items[3], True , Format(FORMAT_NB_REAL_0_DEC, [cs.Gradinc]));
          QDrawColTexte(hcColsTitres.Sections.Items[4], True , cs.Commentaire);
        end;
      mslEXPE:
        begin
          QResetColorRow(bg, tc);
          QDrawColTexte(hcColsTitres.Sections.Items[1], False, Format(FORMAT_NB_INTEGER,[ss.IDExpe]));
          PP := FTableEntites.GetPalette256();
          QDrawColRectColoreWithTexte(hcColsTitres.Sections.Items[2], True, PP.GetColorByIndex(ss.IdxCouleur), Format(FORMAT_NB_INTEGER,[ss.IdxCouleur]));
          {$WARNING: TEXpe.DateExpe à implementer}
          QDrawColTexte(hcColsTitres.Sections.Items[3], True , DateToStr(GetSecuredDate(ss.AnneeExpe,ss.MoisExpe,ss.JourExpe)));
          QDrawColTexte(hcColsTitres.Sections.Items[4], True , ss.Operateur);
          QDrawColTexte(hcColsTitres.Sections.Items[5], True , ss.ClubSpeleo);
          QDrawColTexte(hcColsTitres.Sections.Items[6], True , Format(FORMAT_NB_REAL_3_DEC,[ss.DeclinaisonInDegrees]));
          QDrawColTexte(hcColsTitres.Sections.Items[7], True , ss.Commentaire);
        end;
      mslSERIE:
        begin
          QResetColorRow(bg, tc);
          DecomposeNumeroSerie(sr.GetNumeroDeSerie(), QIdxNameSpace, QNoSerie);
          EWE := IIF(QIdxNameSpace = 0, '', Format('@%d', [QIdxNameSpace]));
          QDrawColTexte(hcColsTitres.Sections.Items[1], False, Format('%d%s', [QNoSerie, EWE]));
          DecomposeNumeroSerie(sr.GetNoSerieDep(), QIdxNameSpace, QNoSerie);
          EWE := IIF(QIdxNameSpace = 0, '', Format('@%d', [QIdxNameSpace]));
          QDrawColTexte(hcColsTitres.Sections.Items[2], True , Format('%d.%d%s', [QNoSerie, sr.GetNoPointDep, EWE]));
          DecomposeNumeroSerie(sr.GetNoSerieArr(), QIdxNameSpace, QNoSerie);
          EWE := IIF(QIdxNameSpace = 0, '', Format('@%d', [QIdxNameSpace]));
          QDrawColTexte(hcColsTitres.Sections.Items[3], True , Format('%d.%d%s', [QNoSerie, sr.GetNoPointArr(), EWE]));
          QDrawColTexte(hcColsTitres.Sections.Items[4], True , _AnsiToLCLStr(sr.GetNomSerie));

          // réseau
          rs := FDocToporobot.GetReseau(sr.GetNumeroReseau());
          QDrawColRectColoreWithTexte(hcColsTitres.Sections.Items[5], True, rs.ColorReseau, rs.NomReseau);
          QDrawColTexte(hcColsTitres.Sections.Items[6], True , Format(FORMAT_NB_INTEGER,[sr.GetNbVisees()]));
        end;
      mslDATE:
        begin
          QResetColorRow(bg, tc);
          QDrawColTexte(hcColsTitres.Sections.Items[1], True , Format(FORMAT_STRING,[DateToStr(dt)]));
        end;
      mslNAMESPACES:
        begin
          QResetColorRow(bg, tc);
          DecomposeNumeroSerie(sr.GetNumeroDeSerie(), QIdxNameSpace, QNoSerie);
          QDrawColRectColoreWithTexte(hcColsTitres.Sections.Items[1], True, MyNamespace.Couleur, Format(FORMAT_NB_INTEGER, [Index]));
          QDrawColTexte(hcColsTitres.Sections.Items[2], True , MyNamespace.Nom);
          QDrawColTexte(hcColsTitres.Sections.Items[3], True , MyNamespace.Description);
        end;
    end; //case FModeSelection of
    QDessineCoche(QChecked);
  end;
begin
  try
    case FModeSelection of
      mslENTRANCES : es := FDocToporobot.GetEntrance(Index);
      mslRESEAUX   : rs := FDocToporobot.GetReseau(Index);
      mslSECTEURS  : sc := FDocToporobot.GetSecteur(Index);
      mslCODE      : cs := FDocToporobot.GetCode(Index);
      mslEXPE      : ss := FDocToporobot.getExpe(Index);
      mslSERIE     : sr := FDocToporobot.GetSerie(Index);
      mslDATE      : dt := FTableEntites.GetUneDate(Index);
      //mslSTATIONS_CALCULEES : et := FTableEntites.GetEntiteVisee(Index);
    end; //case FModeSelection of
    //ShowMessageFmt(FORMAT_NB_INTEGER,[Index]);
    if (odSelected in State) then DessineItem(clBlue , clWhite, lsbListeElements.Checked[Index])
                             else DessineItem(clwhite, clBlack, lsbListeElements.Checked[Index]);
  except
    pass;
  end;
end;

procedure TCdrListesPourVisualisateurs.lsbListeElementsSelectionChange(Sender: TObject; User: boolean);
begin
  SurlignerItem();
end;

procedure TCdrListesPourVisualisateurs.cmbModeListeChange(Sender: TObject);
begin
  FModeSelection := TModeSelectionListe(cmbModeListe.ItemIndex);
  FCadreVue2D.SetModeSelectionListe(FModeSelection);
  RefreshListe();
end;


procedure TCdrListesPourVisualisateurs.hcColsTitresSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbListeElements.Invalidate;
end;

procedure TCdrListesPourVisualisateurs.lsbListeElementsClick(Sender: TObject);
begin
  //if (assigned(FProcChangeListItem)) then FProcChangeListItem;
  //SurlignerItem();
end;

procedure TCdrListesPourVisualisateurs.btnLocaliserClick(Sender: TObject);
const
  MARGE = 100;
  QFMT_FLT = '%s=%d;';
var
  MF: String;
  RS: TReseau;
  SC: TSecteur;
  EX: TExpe;
  i , n: Integer;
  C1: TPoint3Df;
  C2: TPoint3Df;
  SR: TObjSerie;
  ES: TEntrance;
  procedure zdar();
  var
    DbgTag: String;
    QDevelViseesVisibles: double;
  begin
    FTableEntites.Metafiltre(MF, QDevelViseesVisibles);
    FTableEntites.SetMinMax(True);
    C1 := FTableEntites.GetMetafilteredCoinBasGauche;
    C2 := FTableEntites.GetMetafilteredCoinHautDroit;
    DbgTag := Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
    FCadreVue2D.SetViewLimits(C1.X - MARGE, C1.Y - MARGE,
                              C2.X + MARGE, C2.Y + MARGE,
                              DbgTag);
    FCadreVue2D.ApplyMetaFiltre(true, MF);
  end;
begin
  MF := '';
  // codé à l'arrache
  case FModeSelection  of
    mslENTRANCES:
      begin
        n := FDocToporobot.GetNbEntrances();
        if (0 = n) then exit;
        for i := 0 to n - 1 do
        begin
          ES := FDocToporobot.GetEntrance(i);
          if (lsbListeElements.Checked[i]) then MF += Format(QFMT_FLT, [rsMETAFILTRE_ENTREE_RATT, i]);
        end;
        zdar();
      end;
    mslRESEAUX:
      begin
        n := FDocToporobot.GetNbReseaux();
        if (0 = n) then exit;
        for i := 0 to n - 1 do
        begin
          RS := FDocToporobot.GetReseau(i);
          if (lsbListeElements.Checked[i]) then MF += Format(QFMT_FLT, [rsMETAFILTRE_RESEAU, i]);                    //Format('RESEAU=%d;', [i]);
        end;
        zdar();
      end;
    mslSECTEURS:
      begin
        n := FDocToporobot.GetNbSecteurs();
        if (0 = n) then exit;
        for i := 0 to n - 1 do
        begin
          SC := FDocToporobot.GetSecteur(i);
          if (lsbListeElements.Checked[i]) then MF += Format(QFMT_FLT, [rsMETAFILTRE_SECTEUR, i]);
        end;
        zdar();
      end;
    mslEXPE:
      begin
        MF := '';
        for i := 0 to FDocToporobot.GetNbExpes() - 1 do
        begin
          EX := FDocToporobot.GetExpe(i);
          if (lsbListeElements.Checked[i]) then MF += Format(QFMT_FLT, [rsMETAFILTRE_EXPE, EX.IDExpe]);
        end;
        zdar();
      end;
    mslSERIE:
      begin
        MF := '';
        for i := 0 to FDocToporobot.GetNbSeries() - 1 do
        begin
          SR := FDocToporobot.GetSerie(i);
          if (lsbListeElements.Checked[i]) then MF += Format(QFMT_FLT, [rsMETAFILTRE_SERIE, SR.GetNumeroDeSerie()]);
        end;
        zdar();
      end;
    otherwise
      pass;
  end;
end;



procedure TCdrListesPourVisualisateurs.btnHelpClick(Sender: TObject);
begin
  pass;
end;

procedure TCdrListesPourVisualisateurs.btnCocherClick(Sender: TObject);
var
  LSR   : TStringList;
  i , k: Integer;
  EWE, S1, S2, ser: TNumeroSerie;
  MySTR: String;
  WU: TGHStringArray;
  procedure CocherSerie(const Idx: TNumeroSerie);
  var
    QQ: Integer;
  begin
    QQ := FDocToporobot.GetIdxSerieByNumero(Idx);
    if (QQ > 0) then lsbListeElements.Checked[QQ] := true;
  end;
begin
  LSR := TStringList.Create;
  try
    if (SplitToTStringList(Trim(editSeriesARetenir.Text), ';', LSR, True, dupIgnore)) then
    begin
      for i := 0 to LSR.Count - 1 do
      begin
        MySTR := Trim(LSR.Strings[i]);
        EWE := StrToIntDef(MySTR, -1);
        if (EWE = -1) then
        begin
          // on recherche s'il y a une expression de la forme 666-999
          WU := Split(MySTR, '-');
          //for k := 0 to high(WU) do AfficherMessageErreur(Format('WU[%d] = "%s"', [k, WU[k]]));
          S1 := StrToIntDef(WU[0], 0);
          S2 := StrToIntDef(WU[1], 0);
          if ((S1 * S2) > 0) then  // S1 .ET. S2 non nuls ->  Intervalle trouvé
          begin
            //AfficherMessageErreur(Format('Intervalle trouvé: %d - %d', [S1, S2]));
            if (S1 > S2) then Swap(S1, S2); // inversion éventuelle des bornes
            if (Abs(S2 - S1) > 0) then
              for ser := S1 to S2 do CocherSerie(ser)
            else // Bornes identiques => on coche l'élément S1
              CocherSerie(S1);
          end;
        end
        else
        begin
          AfficherMessageErreur(Format('-- Recherche sur Valeur: %d', [EWE]));
          if (EWE > 0) then CocherSerie(EWE);
        end;
      end;
    end;
  finally
    LSR.Clear;
    FreeAndNil(LSR);//LSR.Free;
  end;

end;

procedure TCdrListesPourVisualisateurs.btnConvertirEnFiltreClick(Sender: TObject);
var
  EWE: TCaption;
begin
  EWE := ConvertirEnFiltreSeries(editSeriesARetenir.Text);
  InputBox('Conversion en filtre', 'Filtre à copier', EWE);
end;

procedure TCdrListesPourVisualisateurs.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FTableauIndex) - 1 do
  begin
    FTableauIndex[i].aChecked   := false;
    lsbListeElements.Checked[i] := FTableauIndex[i].aChecked;
  end;
end;

procedure TCdrListesPourVisualisateurs.btnExtraireLesIndexClick(Sender: TObject);
var
  EWE: String;
  i, QIdx: Integer;
  MySerie: TObjSerie;
begin
  EWE := '';
  for i := 0 to Length(FTableauIndex) - 1 do
  begin
    if (lsbListeElements.Checked[i]) then
    begin
      if (FDocToporobot.GetSerieByNumeroSerie(FTableauIndex[i].aIdx, MySerie, QIdx)) then
      begin
        EWE += Format('%d;', [MySerie.GetNumeroDeSerie()]);
      end;
    end;
  end;
  editSeriesARetenir.Text := EWE;
end;



procedure TCdrListesPourVisualisateurs.btnHighlightSelectedClick(Sender: TObject);
begin
  DoHighLightSelectedItems();
end;


procedure TCdrListesPourVisualisateurs.chkDoHighLightCurrentItemChange(Sender: TObject);
begin
  FCadreVue2D.SetHighLightCurrentItem(chkDoHighLightCurrentItem.Checked);
end;

function TCdrListesPourVisualisateurs.SetTIndexItem(const QIdx: integer; const QChecked: boolean): TIndexItem;
begin
  Result.aIdx     := QIdx;
  Result.aChecked := QChecked;
end;

function TCdrListesPourVisualisateurs.FindInternalIdx(const Idx: integer): integer;
var
  i: Integer;
begin
  Result := -1;
  if (Length(FTableauIndex) = 0) then Exit(-1);
  try
    for i:= 0 to High(FTableauIndex) do
    begin
      if (FTableauIndex[i].aIdx = Idx) then Exit(i);
    end;
  except
  end;
end;





procedure TCdrListesPourVisualisateurs.RefreshListe;
begin
  AfficherMessage(Format('%s.RefreshListe()', [classname]));
  InitialiseListeSelection(FDocToporobot,
                           FTableEntites,
                           FCadreVue2D,
                           FModeSelection,
                           0,
                           FUseInternalIndex,
                           False);
end;
procedure TCdrListesPourVisualisateurs.SurlignerItem;
var
  WU: Integer;
  SR: TObjSerie;
  CD: TCode;
  CE: TExpe;
  RS: TReseau;
  SC: TSecteur;
  ES: TEntrance;
  NS: TNameSpace;
begin
  lsbListeElements.Hint := '';
  WU := self.lsbListeElements.ItemIndex;
  //ShowMessage(IntToStr(WU));
  case FModeSelection of
    mslENTRANCES:
      begin
        if (chkDoHighLightCurrentItem.Checked) then
        begin
          ES := FDocToporobot.GetEntrance(WU);
          FCadreVue2D.SetModeSelectionListe(FModeSelection);
          FCadreVue2D.SetCurrentNumeroEntrance(WU);
        end;
      end;
    mslRESEAUX:
      begin
        if (chkDoHighLightCurrentItem.Checked) then
        begin
          RS := FDocToporobot.GetReseau(WU);
          FCadreVue2D.SetModeSelectionListe(FModeSelection);
          FCadreVue2D.SetCurrentNumeroReseau(WU);
        end;
      end;
    mslSECTEURS:
      begin
        if (chkDoHighLightCurrentItem.Checked) then
        begin
          SC := FDocToporobot.GetSecteur(WU);
          FCadreVue2D.SetModeSelectionListe(FModeSelection);
          FCadreVue2D.SetCurrentNumeroSecteur(WU);
        end;
      end;
    mslSERIE:
      begin
        if (chkDoHighLightCurrentItem.Checked) then
        begin
          SR := FDocToporobot.GetSerie(WU);
          FCadreVue2D.SetModeSelectionListe(FModeSelection);
          FCadreVue2D.SetCurrentStationBySerSt(SR.GetNumeroDeSerie(), -1);
        end;
      end;
    mslCODE:
      begin
        if (chkDoHighLightCurrentItem.Checked) then
        begin
          CD := FDocToporobot.GetCode(WU);
          FCadreVue2D.SetModeSelectionListe(FModeSelection);
          FCadreVue2D.SetCurrentNumeroCode(CD.IDCode);
        end;
      end;
    mslEXPE:
      begin
        if (chkDoHighLightCurrentItem.Checked) then
        begin
          CE := FDocToporobot.GetExpe(WU);
          FCadreVue2D.SetModeSelectionListe(FModeSelection);
          FCadreVue2D.SetCurrentNumeroExpe(CE.IDExpe);
        end;
      end;
    mslNAMESPACES:
      begin
        if (chkDoHighLightCurrentItem.Checked) then
        begin
          NS := FDocToporobot.GetNameSpace(WU);
          lsbListeElements.Hint := NS.Nom + #13#10 + NS.Description;
          FCadreVue2D.SetModeSelectionListe(FModeSelection);
          FCadreVue2D.SetCurrentNumeroNamespace(WU);
        end;
      end;
  otherwise
    pass;
  end;

end;
end.


//******************************************************************************


