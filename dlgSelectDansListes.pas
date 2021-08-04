unit dlgSelectDansListes;
// différent de la version Delphi. Retourne un nombre
// Date: 19/04/2012
// Statut: Fonctionnel
// 20/04/2012: DONE: Redessin de la liste après modification de largeur de colonne
// 04/05/2012: DONE: Retourne les index TOPOROBOT et non les index internes
// 01/12/2013: DONE: GetResourceString() systématiquement utilisé pour les captions
// 16/04/2014: Suppression de la fonction vide GetIndexByInternalIndex
// 13/10/2016: Ajout d'une box de recherche rapide

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  UnitClassPalette,
  Common,
  StructuresDonnees,
  ToporobotClasses2012,
  unitobjetserie,
  unitUtilsComposants,
  //CadreListesSimples,
  Classes,
  SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  types, LCLType, Buttons;

type

  { TdlgSelectElement }

  TdlgSelectElement = class(TForm)
    btnOK: TBitBtn;
    BitBtn2: TBitBtn;
    btnFind: TButton;
    editRechercheRapide: TEdit;
    hcColsTitres: THeaderControl;
    lbNbElements: TLabel;
    lsbListeElements: TListBox;
    procedure BitBtn2Click(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure editRechercheRapideKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbListeElementsClick(Sender: TObject);
    procedure lsbListeElementsDrawItem(Control: TWinControl; Index: Integer;  ARect: TRect; State: TOwnerDrawState);
  private
    { private declarations }
    FReady: boolean;
    FDocToporobot: TToporobotStructure2012;
    FNumeroElement: integer;
    FModeSelection: TModeSelectionListe;
    FNbItems: LongInt;
    FMyPalette: TPalette256;
    // tableau d'index
    FTableauIndex: array of Integer;

    function FindInternalIdx(const Idx: integer): integer;
  public
    { public declarations }
    function GetNumeroElement: integer;
    function InitialiseListeSelection(const DT: TToporobotStructure2012;
                                      const MS: TModeSelectionListe;
                                      const QIndex: integer;
                                      const UseInternalIndex: boolean): boolean;
    function GetIndexElement(const UseInternalIndex: boolean): integer;
  end; 

var
  dlgSelectElement: TdlgSelectElement;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}
function TdlgSelectElement.FindInternalIdx(const Idx: integer): integer;
var
 i: integer;
begin
  Result := -1;
  if (length(FTableauIndex) = 0) then Exit;
  for i:= 0 to High(FTableauIndex) do
  begin
    if (FTableauIndex[i] = Idx) then exit(i);
  end;
end;

function TdlgSelectElement.InitialiseListeSelection(const DT: TToporobotStructure2012;
                                                    const MS: TModeSelectionListe;
                                                    const QIndex: integer;
                                                    const UseInternalIndex: boolean): boolean;
var
  i: integer;
  EWE: string;
  ht:  THeaderSection;
  //MyIndex: LongInt;
  CC: TCode;
  EE: TExpe;
  SSR: TObjSerie;
  procedure AjouterTitreColonne(const Titre: string; const LG: integer);
  begin
    ht := hcColsTitres.Sections.Add;
    ht.Text := GetResourceString(Titre);
    ht.MinWidth := LG;
  end;
  procedure AfficherNbElements();
    procedure MiouMiou(const Miou: string); inline;
    begin
      lbNbElements.Caption := format(GetResourceString(Miou), [lsbListeElements.Count]);
    end;
  begin
    case FModeSelection of
      mslENTRANCES   : MiouMiou(rsNB_ELEMENTS_ENTRANCES);
      mslRESEAUX     : MiouMiou(rsNB_ELEMENTS_RESEAUX);
      mslSECTEURS    : MiouMiou(rsNB_ELEMENTS_SECTEURS);
      mslCODE        : MiouMiou(rsNB_ELEMENTS_CODES);
      mslEXPE        : MiouMiou(rsNB_ELEMENTS_EXPES);
    else
      pass;
    end;
  end;
begin
  FReady := False;
  Result := False;
  // préparation de la palette
  FMyPalette := TPalette256.Create;
  FMyPalette.GenerateTOPOROBOTPalette;
  FDocToporobot   := DT;
  FModeSelection  := MS;
  hcColsTitres.Sections.Clear;
  case FModeSelection of
    mslENTRANCES:
      begin
        EWE := GetResourceString(rsSELECT_LISTE_ENTREES);
        FNbItems          := FDocToporobot.GetNbEntrances();
        self.Caption      := GetResourceString(rsSELECT_LISTE_ENTRANCE);
        AjouterTitreColonne(rsSELECT_LISTE_ENTREES_ID, 70);
        AjouterTitreColonne('', 30);
        AjouterTitreColonne(rsSELECT_LISTE_ENTREES_NOM, 340);
        AjouterTitreColonne(rsSELECT_LISTE_ENTREES_REF, 100);
        AjouterTitreColonne(rsSELECT_LISTE_ENTREES_X, 140);
        AjouterTitreColonne(rsSELECT_LISTE_ENTREES_Y, 140);
        AjouterTitreColonne(rsSELECT_LISTE_ENTREES_Z, 140);
      end;
    mslRESEAUX:
      begin
        EWE := GetResourceString(rsSELECT_LISTE_RESEAUX);
        FNbItems := FDocToporobot.GetNbReseaux;
        self.Caption      := GetResourceString(rsSELECT_LISTE_RESEAU);
        AjouterTitreColonne(rsSELECT_LISTE_RESEAUX_ID , 60);
        AjouterTitreColonne(rsSELECT_LISTE_RESEAUX_COLOR , 100);
        AjouterTitreColonne(rsSELECT_LISTE_RESEAUX_NOM , 340);
      end;
    mslSECTEURS:
      begin
        EWE := GetResourceString(rsSELECT_LISTE_SECTEURS);
        FNbItems := FDocToporobot.GetNbSecteurs;
        self.Caption      := GetResourceString(rsSELECT_LISTE_SECTEUR);
        AjouterTitreColonne(rsSELECT_LISTE_SECTEURS_ID    , 60);
        AjouterTitreColonne(rsSELECT_LISTE_SECTEURS_COLOR , 100);
        AjouterTitreColonne(rsSELECT_LISTE_SECTEURS_NOM   , 340);
      end;
    mslCODE:
      begin
        EWE := GetResourceString(rsSELECT_LISTE_CODES);
        FNbItems := FDocToporobot.GetNbCodes;
        self.Caption      := GetResourceString(rsSELECT_LISTE_CODE);
        AjouterTitreColonne(rsSELECT_LISTE_CODES_ID , 60);
        AjouterTitreColonne(rsSELECT_LISTE_CODES_AZIMUTS , 100);
        AjouterTitreColonne(rsSELECT_LISTE_CODES_PENTES , 100);
        AjouterTitreColonne(rsSELECT_LISTE_CODES_OBS , 500);
      end;
    mslEXPE:
      begin
        EWE := GetResourceString(rsSELECT_LISTE_EXPES);
        FNbItems := FDocToporobot.GetNbExpes;
        self.Caption      := GetResourceString(rsSELECT_LISTE_EXPE);
        AjouterTitreColonne(rsSELECT_LISTE_EXPES_ID,  60);
        AjouterTitreColonne(rsSELECT_LISTE_EXPES_COULEUR , 100);
        AjouterTitreColonne(rsSELECT_LISTE_EXPES_DATE , 120);
        AjouterTitreColonne(rsSELECT_LISTE_EXPES_OPERATEUR_TOPO , 200);
        AjouterTitreColonne(rsSELECT_LISTE_EXPES_CLUB_SPELEO , 200);
        AjouterTitreColonne(rsSELECT_LISTE_EXPES_DECLINAISON , 100);
        AjouterTitreColonne(rsSELECT_LISTE_EXPES_OBS , 500);
      end;
    mslSERIE:
      begin
        EWE := GetResourceString(rsSELECT_LISTE_SERIES);
        FNbItems := FDocToporobot.GetNbSeries;
        self.Caption      := GetResourceString(rsSELECT_LISTE_SERIE);
        AjouterTitreColonne(rsSELECT_LISTE_SERIES_ID, 60);
        AjouterTitreColonne(rsSELECT_LISTE_SERIES_DEPART, 100);
        AjouterTitreColonne(rsSELECT_LISTE_SERIES_ARRIVEE, 100);
        AjouterTitreColonne(rsSELECT_LISTE_SERIES_NOM, 400);
        AjouterTitreColonne(rsSELECT_LISTE_SERIES_RESEAU, 300);
        AjouterTitreColonne(rsSELECT_LISTE_SERIES_NBPOINTS, 80);
        AjouterTitreColonne(rsSELECT_LISTE_SERIES_INTERVALLE, 160);
      end;
  end;
  Self.Caption := Format(rsSELECT_LISTE_NB_ELEMENTS, [FNbItems, EWE]);
  Result:=False;
  // liste
  lsbListeElements.ItemHeight := 22;
  try
    SetLength(FTableauIndex, 0);
    if FNbItems = 0 then Exit;
    lsbListeElements.Clear;
    SetLength(FTableauIndex, FNbItems);
    for i:=0 to FNbItems - 1 do
    begin
      case FModeSelection of
        mslENTRANCES  : FTableauIndex[i] := i;
        mslRESEAUX    : FTableauIndex[i] := i;
        mslSECTEURS   : FTableauIndex[i] := i;
        mslCODE :
          begin
            CC := FDocToporobot.GetCode(i);
            FTableauIndex[i] := CC.IDCode;
          end;
        mslEXPE :
          begin
            EE := FDocToporobot.GetExpe(i);
            FTableauIndex[i] := EE.IDExpe;
          end;
        mslSERIE :
          begin
            SSR := FDocToporobot.GetSerie(i);
            FTableauIndex[i] := SSR.GetNumeroDeSerie();
          end;
        else
          FTableauIndex[i] := i;
      end;
      lsbListeElements.Items.Add('');
    end;
    if (UseInternalIndex) then FNumeroElement := QIndex
                          else FNumeroElement := FindInternalIdx(QIndex);
    lsbListeElements.ItemIndex := FNumeroElement;
    AfficherNbElements();
    Result := True;
    FReady := True;
  except
  end;
end;

function  TdlgSelectElement.GetNumeroElement: integer;
begin
  Result := FNumeroElement;
end;
function TdlgSelectElement.GetIndexElement(const UseInternalIndex: boolean): integer;
begin
  if (not FReady) then Exit;
  try
    Result := IIF(UseInternalIndex, FNumeroElement, FTableauIndex[FNumeroElement]);
  except
    Result := -1;
  end;
end;



procedure TdlgSelectElement.lsbListeElementsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  rs: TReseau;
  es: TEntrance;
  cs: TCode;
  ss: TExpe;
  sr: TObjSerie;
  EWE: string;
  sc: TSecteur;
  // bg = couleur de fond; tc= couleur du texte
  procedure DessineItem(const bg,tc: TColor);
  var
    WU: String;
    sr1, sr0: TObjSerie;
    NbIndexSeriesLibres: TNumeroSerie;
  begin
    case FModeSelection of
      mslENTRANCES:
        begin
          ResetColorRow(lsbListeElements, ARect, bg, tc);
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[0], false , Format(FORMAT_NB_INTEGER,[Index]));
          DrawColRectColoreWithTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[1], true, bg, es.eCouleur.toTColor(), '');
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[2], true , _AnsiToLCLStr(es.eNomEntree));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[3], true , Format(FMTSERST,[es.eRefSer, es.eRefSt]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[4], true , FormatterNombreAvecSepMilliers(es.ePosition.X));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[5], true , FormatterNombreAvecSepMilliers(es.ePosition.Y));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[6], true , FormatterNombreAvecSepMilliers(es.ePosition.Z));
        end;
      mslRESEAUX:
        begin
          ResetColorRow(lsbListeElements, ARect, bg, tc);
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[Index]));
          DrawColRectColoreWithTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[1], true, bg, rs.ColorReseau.toTColor(), '');
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[2], true , _AnsiToLCLStr(rs.NomReseau));
        end;
      mslSECTEURS:
        begin
          ResetColorRow(lsbListeElements, ARect, bg, tc);
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[Index]));
          DrawColRectColoreWithTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[1], True, bg, sc.CouleurSecteur.toTColor(), '');
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[2], true , _AnsiToLCLStr(sc.NomSecteur));
        end;
      mslCODE:
        begin
          ResetColorRow(lsbListeElements, ARect, bg, tc);
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[cs.IDCode]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[1], true , Format(FORMAT_NB_REAL_0_DEC, [cs.GradAz]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[2], true , Format(FORMAT_NB_REAL_0_DEC, [cs.Gradinc]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[3], true , _AnsiToLCLStr(cs.Commentaire));
        end;
      mslEXPE:
        begin
          ResetColorRow(lsbListeElements, ARect, bg, tc);
          try
            EWE := DateToStr(ss.DateExpe); // DateToStr est localisé selon la date
          except
            EWE := '01/01/2000';
          end;
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[ss.IDExpe]));
          DrawColRectColoreWithTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[1], True, bg, FMyPalette.GetColorByIndex(ss.IdxCouleur), Format(FORMAT_NB_INTEGER,[ss.IdxCouleur]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[2], true , EWE);
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[3], true , _AnsiToLCLStr(ss.Operateur));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[4], true , _AnsiToLCLStr(ss.ClubSpeleo));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[5], true , Format(FORMAT_NB_REAL_3_DEC, [ss.DeclinaisonInDegrees]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[6], true , _AnsiToLCLStr(ss.Commentaire));
        end;
      mslSERIE:
        begin
          ResetColorRow(lsbListeElements, ARect, bg, tc);
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[sr.GetNumeroDeSerie()]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[1], true , Format(FMTSERST,[sr.GetNoSerieDep, sr.GetNoPointDep]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[2], true , Format(FMTSERST,[sr.GetNoSerieArr, sr.GetNoPointArr]));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[3], true , _AnsiToLCLStr(sr.GetNomSerie));
          rs := FDocToporobot.GetReseau(sr.GetNumeroReseau);
          DrawColRectColoreWithTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[4], True, bg,
                                     rs.ColorReseau.toTColor(), _AnsiToLCLStr(rs.NomReseau));
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[5], true , Format(FORMAT_NB_INTEGER,[sr.GetNbVisees]));
          // nombre d'éléments libres entre deux numéros de série
          WU := '';
          if (Index > 0) then
          begin
            sr0 := FDocToporobot.GetSerie(Index - 1);
            sr1 := FDocToporobot.GetSerie(Index);
            NbIndexSeriesLibres := (sr1.GetNumeroDeSerie() - sr0.GetNumeroDeSerie()) - 1;
            WU := IIF (0 = NbIndexSeriesLibres, '', Format(FORMAT_NB_INTEGER,[NbIndexSeriesLibres]));
          end;
          DrawColTexte(lsbListeElements, ARect, hcColsTitres.Sections.Items[6], true, WU);

        end;
    end; //case FModeSelection of
  end;
begin
  if (Not FReady) then Exit;
  case FModeSelection of
    mslENTRANCES : es := FDocToporobot.GetEntrance(Index);
    mslRESEAUX   : rs := FDocToporobot.GetReseau(Index);
    mslSECTEURS  : sc := FDocToporobot.GetSecteur(Index);
    mslCODE      : cs := FDocToporobot.GetCode(Index);
    mslEXPE      : ss := FDocToporobot.getExpe(Index);
    mslSERIE     : sr := FDocToporobot.GetSerie(Index);
  else
    exit;
  end; //case FModeSelection of
  if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clwhite, clBlack);
end;

procedure TdlgSelectElement.FormClose(Sender: TObject;  var CloseAction: TCloseAction);
begin
  try
    lsbListeElements.Clear;
    FMyPalette.Finaliser();
  finally
    FreeAndNil(FMyPalette);
  end;
end;

procedure TdlgSelectElement.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

procedure TdlgSelectElement.hcColsTitresSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbListeElements.Invalidate;
end;

procedure TdlgSelectElement.lsbListeElementsClick(Sender: TObject);
begin
  FNumeroElement := lsbListeElements.ItemIndex;
end;


procedure TdlgSelectElement.BitBtn2Click(Sender: TObject);
begin
  FNumeroElement := -1;
end;

procedure TdlgSelectElement.btnFindClick(Sender: TObject);
var
  QIdx: integer;
  EWE: TCaption;
begin
  EWE := trim(editRechercheRapide.Text);
  case FModeSelection of
    mslENTRANCES    : QIdx := FDocToporobot.FindIdxEntranceByText(EWE);
    mslRESEAUX      : QIdx := FDocToporobot.FindIdxReseauByText(EWE);
    mslSECTEURS     : QIdx := FDocToporobot.FindIdxSecteurByText(EWE);
    mslCODE         : QIdx := FDocToporobot.FindIdxCodeByText(EWE);
    mslEXPE         : QIdx := FDocToporobot.FindIdxExpeByText(EWE);
    mslSERIE        : QIdx := FDocToporobot.FindIdxSerieByText(EWE);
  else
    exit;
  end;
  if (QIdx >= 0) then lsbListeElements.ItemIndex := QIdx
                 else ShowMessage(GetResourceString(rsMATCHNOTFOUND));
end;

procedure TdlgSelectElement.editRechercheRapideKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) then btnFindClick(self);
end;
end.

