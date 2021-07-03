unit frmHierarchieObjets;

{$mode delphi}
{$ERROR N'a rien à faire ici}

interface

uses
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin, CdrHierarchieObjets,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type

  { TdlgHierarchieObjets }

  TdlgHierarchieObjets = class(TForm)
    Button1: TButton;
    CadreHierarchieObjets1: TCadreHierarchieObjets;
    editNumSerie: TCurrencyEdit;
    editNomTexte: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbCoordsBaseStation: TLabel;
    lbBoundingBoxX2: TStaticText;
    lbBoundingBoxY2: TStaticText;
    lbBoundingBoxY1: TStaticText;
    lbGroupe: TStaticText;
    lbDescObject: TStaticText;
    lbIDBaseStation: TStaticText;
    lbNbSommetsObject: TLabel;
    lsbObjectsFound: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnlScrap: TPanel;
    lbIdxObject: TStaticText;
    lbCouleurScrap: TStaticText;
    lbBoundingBoxX1: TStaticText;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure acDeleteobjectExecute(Sender: TObject);
    procedure acEditObjectExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlScrapClick(Sender: TObject);
  private
    procedure DispBoundingBox( const BB: TBoundingBox) ;
    procedure DispInfosCourbe(const QIdx: integer; const MyCourbe: TCourbe);
    procedure DispInfosGroupe(const myGroupe: TGroupeEntites);
    procedure DispInfosObjet() ;
    procedure DispInfosPolygone(const QIdx: integer; const MyPolygone: TPolygone
      );
    procedure DispInfosPolyligne(const QIdx: integer;
      const MyPolyligne: TPolyLigne);
    procedure DispInfosScrap( const QIdx: integer; const MyScrap: TScrap) ;
    procedure DispInfosSimpleLigne(const QIdx: integer;
      const MyLigne: TSimpleLigne);
    procedure DispInfosSymbole(const QIdx: integer; const MySymbole: TSymbole);
    procedure DispInfosTexte(const QIdx: integer; const MyTexte: TTextObject);
    { private declarations }

  public
    { public declarations }
    function  Initialise(const FD: TDocumentDessin): boolean;
    procedure Finalise();
  end;

var
  dlgHierarchieObjets: TdlgHierarchieObjets;

implementation

{$R *.lfm}

procedure TdlgHierarchieObjets.pnlScrapClick(Sender: TObject);
begin

end;

procedure TdlgHierarchieObjets.FormCreate(Sender: TObject);
begin

end;

procedure TdlgHierarchieObjets.acDeleteobjectExecute(Sender: TObject);
begin

end;

procedure TdlgHierarchieObjets.acEditObjectExecute(Sender: TObject);
begin

end;
// codé à l'arrache. Réagencer
procedure TdlgHierarchieObjets.Button1Click(Sender: TObject);
var
  FD: TDocumentDessin;
  Nb, i: Integer;
  NumeroDeSerie: LongInt;
  MyScrap: TScrap;
  MyCourbe: TCourbe;
  MyPolygone: TPolygone;
  MyPolyline: TPolyLigne;
  MySimpleLigne: TSimpleLigne;
  MySymbole: TSymbole;
  MyTexte: TTextObject;
  function ScrapContainsSerie(const QScrap: TScrap; const QSer: integer): boolean;
  var
    NbV, v, QNumeroSerie: Integer;
    BP: TBaseStation;
  begin
    Result := false;
    NbV := High(QScrap.Sommets);
    for v := 0 to NbV do
    begin
      if (FD.GetBasePointByIndex(QScrap.Sommets[v].IDStation, BP)) then
      begin
        QNumeroSerie := ExtractNumSerieFromTIDBaseStation(BP.IDStation);
        if (QSer = QNumeroSerie) then begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  function PolyligneContainsSerie(const QPolyline: TPolyLigne; const QSer: integer): boolean;
  var
    NbV, v, QNumeroSerie: Integer;
    BP: TBaseStation;
  begin
    Result := false;
    NbV := High(QPolyline.Sommets);
    for v := 0 to NbV do
    begin
      if (FD.GetBasePointByIndex(QPolyline.Sommets[v].IDStation, BP)) then
      begin
        QNumeroSerie := ExtractNumSerieFromTIDBaseStation(BP.IDStation);
        if (QSer = QNumeroSerie) then begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  function PolygoneContainsSerie(const QPolygone: TPolygone; const QSer: integer): boolean;
  var
    NbV, v, QNumeroSerie: Integer;
    BP: TBaseStation;
  begin
    Result := false;
    NbV := High(QPolygone.Sommets);
    for v := 0 to NbV do
    begin
      if (FD.GetBasePointByIndex(QPolygone.Sommets[v].IDStation, BP)) then
      begin
        QNumeroSerie := ExtractNumSerieFromTIDBaseStation(BP.IDStation);
        if (QSer = QNumeroSerie) then begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  function CourbeContainsSerie(const QCourbe: TCourbe; const QSer: integer): boolean;
  var
    NbV, v, QNumeroSerie: Integer;
    BP1, BP2: TBaseStation;
    QMyArc: TArcCourbe;
  begin
    Result := false;
    NbV := High(QCourbe.Arcs);
    for v := 0 to NbV do
    begin
      QMyArc := QCourbe.Arcs[v];
      if (FD.GetBasePointByIndex(QMyArc.IDStationP1, BP1) and
          FD.GetBasePointByIndex(QMyArc.IDStationP2, BP2)) then
      begin
        QNumeroSerie := ExtractNumSerieFromTIDBaseStation(BP1.IDStation);
        if (QSer = QNumeroSerie) then
        begin
          Result := True;
          Exit;
        end;
        QNumeroSerie := ExtractNumSerieFromTIDBaseStation(BP2.IDStation);
        if (QSer = QNumeroSerie) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  function SimpleLigneContainsSerie(const QSimpleLigne: TSimpleLigne; const QSer: integer): boolean;
  var
    QNumeroSerie1, QNumeroSerie2: Integer;
    BP1, BP2: TBaseStation;
  begin
    Result := false;
    try
      FD.GetBasePointByIndex(QSimpleLigne.IDBaseStExt1, BP1);
      FD.GetBasePointByIndex(QSimpleLigne.IDBaseStExt1, BP2);
      QNumeroSerie1 := ExtractNumSerieFromTIDBaseStation(BP1.IDStation);
      QNumeroSerie2 := ExtractNumSerieFromTIDBaseStation(BP2.IDStation);
      if ((QSer = QNumeroSerie1) or (QSer = QNumeroSerie2)) then
      begin
        Result := True;
        Exit;
      end;
    except
    end;
  end;
  function SymboleContainsSerie(const QSymbole: TSymbole; const QSer: integer): boolean;
  var
    BP1: TBaseStation;
    QNumeroSerie1: Integer;
  begin
    Result := false;
    try
      FD.GetBasePointByIndex(QSymbole.IDBaseStation, BP1);
      QNumeroSerie1 := ExtractNumSerieFromTIDBaseStation(BP1.IDStation);
      if (QSer = QNumeroSerie1) then
      begin
        Result := True;
        Exit;
      end;
    except
    end;
  end;
  function TexteContainsSerie(const QTexte: TTextObject; const QSer: integer): boolean;
  var
    BP1: TBaseStation;
    QNumeroSerie1: Integer;
  begin
    Result := false;
    try
      FD.GetBasePointByIndex(QTexte.IDBaseSt, BP1);
      QNumeroSerie1 := ExtractNumSerieFromTIDBaseStation(BP1.IDStation);
      if (QSer = QNumeroSerie1) then
      begin
        Result := True;
        Exit;
      end;
    except
    end;
  end;

begin
  NumeroDeSerie := editNumSerie.AsInteger;
  lsbObjectsFound.Clear;
  FD := CadreHierarchieObjets1.GetDocDessin();
  // scraps
  Nb := FD.GetNbScraps();
  if (Nb > 0) then
  begin
    lsbObjectsFound.Items.Add('=== SCRAPS ===');
    for i := 0 to Nb - 1 do
    begin
      MyScrap := FD.GetScrap(i);
      if (ScrapContainsSerie(MyScrap, NumeroDeSerie)) then lsbObjectsFound.Items.Add(Format('%d: %s', [i, MyScrap.Nom]));
    end;
  end;
  // courbes
  Nb := FD.GetNbCourbes();
  if (Nb > 0) then
  begin
    lsbObjectsFound.Items.Add('=== COURBES ===');
    for i := 0 to Nb - 1 do
    begin
      MyCourbe := FD.GetCourbe(i);
      if (CourbeContainsSerie(MyCourbe, NumeroDeSerie)) then lsbObjectsFound.Items.Add(Format('%d: Courbe from groupe %d', [i, MyCourbe.IDGroupe]));
    end;
  end;
  // polylignes
  Nb := FD.GetNbPolylignes();
  if (Nb > 0) then
  begin
    lsbObjectsFound.Items.Add('=== POLYLIGNES ===');
    for i := 0 to Nb - 1 do
    begin
      MyPolyline := FD.GetPolyligne(i);
      if (PolyligneContainsSerie(MyPolyline, NumeroDeSerie)) then lsbObjectsFound.Items.Add(Format('%d: Polyline from groupe %d', [i, MyPolyline.IDGroupe]));
    end;
  end;

  // polygones
  Nb := FD.GetNbPolygones();
  if (Nb > 0) then
  begin
    lsbObjectsFound.Items.Add('=== POLYGONES ===');
    for i := 0 to Nb - 1 do
    begin
      MyPolygone := FD.GetPolygone(i);
      if (PolygoneContainsSerie(MyPolygone, NumeroDeSerie)) then lsbObjectsFound.Items.Add(Format('%d: Polygone from groupe %d', [i, MyPolygone.IDGroupe]));
    end;
  end;
  // lignes
  Nb := FD.GetNbSimpleLignes();
  if (Nb > 0) then
  begin
    lsbObjectsFound.Items.Add('=== LIGNES ===');
    for i := 0 to Nb - 1 do
    begin
      MySimpleLigne := FD.GetSimpleLigne(i);
      if (SimpleLigneContainsSerie(MySimpleLigne, NumeroDeSerie)) then lsbObjectsFound.Items.Add(Format('%d: Ligne from groupe %d', [i, MySimpleLigne.IDGroupe]));
    end;
  end;
  // symboles
  Nb := FD.GetNbSymboles();
  if (Nb > 0) then
  begin
    lsbObjectsFound.Items.Add('=== SYMBOLES ===');
    for i := 0 to Nb - 1 do
    begin
      MySymbole := FD.GetSymbole(i);
      if (SymboleContainsSerie(MySymbole, NumeroDeSerie)) then lsbObjectsFound.Items.Add(Format('%d: Symbole from groupe %d', [i, MySymbole.IDGroupe]));
    end;
  end;

  // textes
  Nb := FD.GetNbTextes();
  if (Nb > 0) then
  begin
    lsbObjectsFound.Items.Add('=== TEXTES ===');
    for i := 0 to Nb - 1 do
    begin
      MyTexte := FD.GetTexte(i);
      if (TexteContainsSerie(MyTexte, NumeroDeSerie)) then lsbObjectsFound.Items.Add(Format('%d: Texte from groupe %d: %s', [i, MyTexte.IDGroupe, MyTexte.Text]));
    end;
  end;




end;

{ TdlgHierarchieObjets }
procedure TdlgHierarchieObjets.DispBoundingBox(const BB: TBoundingBox);
begin
  lbBoundingBoxX1.Caption    := Format('X1 = %.2f', [BB.C1.X]);
  lbBoundingBoxY1.Caption    := Format('Y1 = %.2f', [BB.C1.Y]);
  lbBoundingBoxX2.Caption    := Format('X2 = %.2f', [BB.C2.X]);
  lbBoundingBoxY2.Caption    := Format('Y2 = %.2f', [BB.C2.Y]);
end ;

procedure TdlgHierarchieObjets.DispInfosScrap(const QIdx: integer; const MyScrap: TScrap);
var
  FD: TDocumentDessin;
  Grp: TGroupeEntites;
begin
  FD := CadreHierarchieObjets1.GetDocDessin();
  lbIdxObject.Caption        := format(FORMAT_NB_INTEGER, [QIdx]);
  lbDescObject.Caption       := 'scrap';
  lbCouleurScrap.Color       := MyScrap.Couleur;
  lbNbSommetsObject.Caption  := format(FORMAT_NB_INTEGER, [1 + High(MyScrap.Sommets)]);
  Grp := FD.GetGroupeByIDGroupe(MyScrap.IDGroupe);
  lbGroupe.Caption := Format('%d - %s', [Grp.IDGroupeEntites, Grp.NomGroupe]);
  editNomTexte.Text          := MyScrap.Nom;
  DispBoundingBox(MyScrap.BoundingBox);
  lbCoordsBaseStation.Caption := ''; lbIDBaseStation.Caption := '';

end ;

procedure TdlgHierarchieObjets.DispInfosPolygone(const QIdx: integer; const MyPolygone: TPolygone);
var
  FD: TDocumentDessin;
  Grp: TGroupeEntites;
begin
  FD := CadreHierarchieObjets1.GetDocDessin();
  lbIdxObject.Caption        := format(FORMAT_NB_INTEGER, [QIdx]);
  Grp := FD.GetGroupeByIDGroupe(MyPolygone.IDGroupe);
  lbGroupe.Caption := Format('%d - %s', [Grp.IDGroupeEntites, Grp.NomGroupe]);
  lbDescObject.Caption       := GetDescStylePolygone(MyPolygone.IDStylePolygone);
  lbNbSommetsObject.Caption  := format(FORMAT_NB_INTEGER, [1 + High(MyPolygone.Sommets)]);
  lbCouleurScrap.Color       := clWhite;
  editNomTexte.Text          := '';
  DispBoundingBox(MyPolygone.BoundingBox);
  lbCoordsBaseStation.Caption := ''; lbIDBaseStation.Caption := '';
end ;
procedure TdlgHierarchieObjets.DispInfosPolyligne(const QIdx: integer; const MyPolyligne: TPolyLigne);
var
  FD: TDocumentDessin;
  Grp: TGroupeEntites;
begin
  FD := CadreHierarchieObjets1.GetDocDessin();
  lbIdxObject.Caption        := format(FORMAT_NB_INTEGER, [QIdx]);
  Grp := FD.GetGroupeByIDGroupe(MyPolyligne.IDGroupe);
  lbGroupe.Caption := Format('%d - %s', [Grp.IDGroupeEntites, Grp.NomGroupe]);
  lbDescObject.Caption       := GetDescStylePolyligne(MyPolyligne.IDStylePolyLine);
  lbNbSommetsObject.Caption  := format(FORMAT_NB_INTEGER, [1 + High(MyPolyligne.Sommets)]);
  lbCouleurScrap.Color       := clWhite;
  editNomTexte.Text          := '';
  DispBoundingBox(MyPolyligne.BoundingBox);
  lbCoordsBaseStation.Caption := ''; lbIDBaseStation.Caption := '';

end ;
procedure TdlgHierarchieObjets.DispInfosCourbe(const QIdx: integer; const MyCourbe: TCourbe);
var
  FD: TDocumentDessin;
  Grp: TGroupeEntites;
begin
  FD := CadreHierarchieObjets1.GetDocDessin();
  lbIdxObject.Caption        := format(FORMAT_NB_INTEGER, [QIdx]);
  Grp := FD.GetGroupeByIDGroupe(MyCourbe.IDGroupe);
  lbGroupe.Caption := Format('%d - %s', [Grp.IDGroupeEntites, Grp.NomGroupe]);
  lbDescObject.Caption       := GetDescStyleCourbe(MyCourbe.IDStyleCourbe);
  lbNbSommetsObject.Caption  := format(FORMAT_NB_INTEGER, [1 + High(MyCourbe.Arcs)]);
  lbCouleurScrap.Color       := clWhite;
  editNomTexte.Text          := '';
  DispBoundingBox(MyCourbe.BoundingBox);
  lbCoordsBaseStation.Caption := ''; lbIDBaseStation.Caption := '';
end ;
procedure TdlgHierarchieObjets.DispInfosSymbole(const QIdx: integer; const MySymbole: TSymbole);
var
  FD: TDocumentDessin;
  Grp: TGroupeEntites;
  BP: TBaseStation;
begin
  FD := CadreHierarchieObjets1.GetDocDessin();
  FD.GetBasePointByIndex(MySymbole.IDBaseStation, BP);
  lbIdxObject.Caption        := format(FORMAT_NB_INTEGER, [QIdx]);
  Grp := FD.GetGroupeByIDGroupe(MySymbole.IDGroupe);
  lbGroupe.Caption := Format('%d - %s', [Grp.IDGroupeEntites, Grp.NomGroupe]);
  lbDescObject.Caption       := GetDescNatureSymbole(MySymbole.TypeObject);
  lbNbSommetsObject.Caption  := format(FORMAT_NB_INTEGER, [0]); //[1 + High(MyPolyligne.Sommets)]);
  lbCouleurScrap.Color       := clWhite;
  editNomTexte.Text          := MySymbole.TagTexte;
  lbIDBaseStation.Caption     := Format('%d - %s', [BP.IDStation, BP.IDTerrain]);
  lbCoordsBaseStation.Caption := Format('X = %.2f, Y = %.2f, Z = %.2f', [
                                        BP.PosStation.X + MySymbole.Offset.X,
                                        BP.PosStation.Y + MySymbole.Offset.Y,
                                        BP.PosStation.Z
                                       ]);
end ;

procedure TdlgHierarchieObjets.DispInfosTexte(const QIdx: integer; const MyTexte: TTextObject);
var
  FD: TDocumentDessin;
  Grp: TGroupeEntites;
  BP: TBaseStation;
begin
  FD := CadreHierarchieObjets1.GetDocDessin();
  lbIdxObject.Caption         := format(FORMAT_NB_INTEGER, [QIdx]);
  Grp := FD.GetGroupeByIDGroupe(MyTexte.IDGroupe);
  lbGroupe.Caption := Format('%d - %s', [Grp.IDGroupeEntites, Grp.NomGroupe]);
  lbNbSommetsObject.Caption   := format(FORMAT_NB_INTEGER, [0]); //[1 + High(MyPolyligne.Sommets)]);
  FD.GetBasePointByIndex(MyTexte.IDBaseSt, BP);

  lbDescObject.Caption        := GetDescStyleTexte(MyTexte.IDStyleTexte);
  editNomTexte.Text           := InterpreterTexteAnnotation(MyTexte.Text, MyTexte.MaxLength, BP);
  lbCouleurScrap.Color        := clWhite;
  lbIDBaseStation.Caption     := Format('%d - %s', [BP.IDStation, BP.IDTerrain]);
  lbCoordsBaseStation.Caption := Format('X = %.2f, Y = %.2f, Z = %.2f', [
                                        BP.PosStation.X + MyTexte.Offset.X,
                                        BP.PosStation.Y + MyTexte.Offset.Y,
                                        BP.PosStation.Z
                                       ]);
end ;
procedure TdlgHierarchieObjets.DispInfosSimpleLigne(const QIdx: integer; const MyLigne: TSimpleLigne);
var
  FD: TDocumentDessin;
  Grp: TGroupeEntites;
begin
  FD := CadreHierarchieObjets1.GetDocDessin();
  lbIdxObject.Caption        := format(FORMAT_NB_INTEGER, [QIdx]);
  Grp := FD.GetGroupeByIDGroupe(MyLigne.IDGroupe);
  lbGroupe.Caption := Format('%d - %s', [Grp.IDGroupeEntites, Grp.NomGroupe]);
  lbNbSommetsObject.Caption  := format(FORMAT_NB_INTEGER, [0]); //[1 + High(MyPolyligne.Sommets)]);
  editNomTexte.Text          := '';
  lbCouleurScrap.Color       := clWhite;
end ;
procedure TdlgHierarchieObjets.DispInfosGroupe(const myGroupe: TGroupeEntites);
begin

end;

procedure TdlgHierarchieObjets.DispInfosObjet();
var
  WU        : String;
  QTypeObjet: TTypeObjet;
  IdxObj    : Integer;
  FD: TDocumentDessin;
begin
  FD := CadreHierarchieObjets1.GetDocDessin();
  QTypeObjet := CadreHierarchieObjets1.GetCurrentTypeObject;
  IdxObj     := CadreHierarchieObjets1.GetCurrentIdxObject;

  //WU := ChooseString(Ord(QTypeObjet), ['Scrap', 'Courbes', 'Polylignes', 'Polygones', 'Lignes', 'Symboles', 'Textes', 'Aucun', 'Inconnu']);
  //ShowMessageFmt('Type objet: %s - Idx: %d', [WU, IdxObj]);
  case QTypeObjet of
    tobjNONE, tobjUNKNOWN: ;
    tobjSCRAP       : DispInfosScrap(IdxObj, FD.GetScrap(IdxObj));
    tobjCOURBE      : DispInfosCourbe(IdxObj, FD.GetCourbe(IdxObj));
    tobjPOLYLIGNE   : DispInfosPolyligne(IdxObj, FD.GetPolyligne(IdxObj));
    tobjPOLYGONE    : DispInfosPolygone(IdxObj, FD.GetPolygone(IdxObj));
    tobjLIGNE       : DispInfosSimpleLigne(IdxObj, FD.GetSimpleLigne(IdxObj));
    tobjSYMBOLE     : DispInfosSymbole(IdxObj, FD.GetSymbole(IdxObj));
    tobjTEXTE       : DispInfosTexte(IdxObj, FD.GetTexte(IdxObj));
  else
    ;
  end ;
end;

function TdlgHierarchieObjets.Initialise(const FD: TDocumentDessin): boolean;
begin
  result := false;
  try
    CadreHierarchieObjets1.SetDocDessin(FD);
    CadreHierarchieObjets1.ListerLesObjets();
    CadreHierarchieObjets1.SetProcHandleItemSelected(DispInfosObjet());
    result := True;
  except
  end;
end;
procedure TdlgHierarchieObjets.Finalise();
begin
  CadreHierarchieObjets1.ViderListe;
end;

end.

