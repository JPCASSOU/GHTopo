// Génération de quatre fiches topo format A6 sur une feuille A4
unit UnitFichesTopo;

{$mode delphi}

interface

uses
  StructuresDonnees,
  UnitListesSimplesWithGeneriques,
  Common,
  UnitEntitesExtended,
  ubarcodes,
  //Printers, OSPrinters, // OSPrinters est INDISPENSABLE ici !!
  PdfDoc,
  PdfFonts,
  PdfTypes,
  Classes, SysUtils, Graphics;

type { TFichesTopo }

  TFichesTopo = class
  private
    FListeStations: TTableEntitesVisees;
    FBDDEntites: TBDDEntites;
    FPDFDoc    : TPdfDoc;
    FProcProgression: TProcDisplayProgression;
    procedure ImprimerUneFiche(const IdxCurr: integer);
  public
    function  Initialiser(const pBDD: TBDDEntites; const QProcProgression: TProcDisplayProgression): boolean;
    procedure Clear();
    procedure Finaliser();
    procedure AddStation(const BS: TBaseStation);
    procedure AddStationBySerieStation(const Sr: TNumeroSerie; const St: TNumeroStation);
    function  GetStation(const Idx: integer): TBaseStation;
    function  GetNbStations(): integer;
    procedure RemoveStation(const Idx: integer);
    procedure ImprimerLesFiches(const QFilenamePDF: TStringDirectoryFilename; const QCompressed: boolean);
end;

implementation
uses
  DGCDummyUnit;

const ONE_INCH_IN_MM: double = 25.40;
const QECHELLE_PAR_DEFAULT   = 1000;
// fonctions locales hors objet
  (*
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
//*)
{ TFichesTopo }

function TFichesTopo.Initialiser(const pBDD: TBDDEntites; const QProcProgression: TProcDisplayProgression): boolean;
var
  i: Integer;
  BS: TBaseStation;
begin
  result := false;
  AfficherMessage(Format('%s.Initialiser()', [ClassName]));
  FProcProgression := QProcProgression;
  FBDDEntites  := pBDD;
  FListeStations := TTableEntitesVisees.Create;
  FPDFDoc := TPdfDoc.Create;
  try
    FListeStations.ClearListe();
    for i := 0 to GetNbStations() - 1 do
    begin
      BS := GetStation(i);
      AfficherMessageErreur(Format(' --> %d: %d.%d', [i, BS.Entite_Serie, BS.Entite_Station]));
    end;
    result := True;
  finally
  end;
end;

procedure TFichesTopo.Clear();
begin
  FListeStations.ClearListe();
end;

procedure TFichesTopo.ImprimerUneFiche(const IdxCurr: integer);
const
  K = 4;
  GUTTER = 5;
  MG = 40;
  HAUTEUR_TEXTE_ID_STATION  = 24;
  HAUTEUR_TEXTE_ID_NEXT_STATION  = 8;
  HAUTEUR_TEXTE_COORDONNEES = 12;
var
  QMargeX, QMargeY: Integer;
  QLargeurFiche, QHauteurFiche: integer;
  QX0, QY0, LargQRCode, Nb, IdxNext, IdxPrev: integer;
  MyStation, MyStationNear: TBaseStation;
  QBarCode: TBarcodeQR;
  EWE: String;
  Fnt: TPdfFont;
  QLong, QAzimut, QPente: double;
  procedure QDrawRectangle(const QX, QY, QL, QH: integer; const DoFill: boolean = true);
  begin
    FPDFDoc.Canvas.NewPath;
    FPDFDoc.Canvas.MoveTo(QX      , QY);
    FPDFDoc.Canvas.LineTo(QX + QL , QY);
    FPDFDoc.Canvas.LineTo(QX + QL , QY + QH);
    FPDFDoc.Canvas.LineTo(QX      , QY + QH);
    FPDFDoc.Canvas.LineTo(QX      , QY);
    if (DoFill) then FPDFDoc.Canvas.ClosepathFillStroke
                else FPDFDoc.Canvas.Closepath;
  end;

  procedure QDrawQRCode(const QX, QY, QCoteX, QCoteY: integer);
  var
    PasX, PasY: integer;
    i, j, QTaille: Integer;
    MTX: TMatriceResultat;
    procedure DessineCarre(const i, j, V: integer);
    var
      WU: TColor;
    begin
      WU := IIF((V <> 0), clBlack, clWhite);
      case V of
        0, 16, 32                 : WU := clWhite;     // données: Blanc
        1, 17, 20, 21, 33, 65     : WU := clBlack;     // données: Noir
      else
        WU := clGray;
      end;
      FPDFDoc.Canvas.SetRGBFillColor(WU);
      FPDFDoc.Canvas.SetRGBStrokeColor(WU);
      QDrawRectangle(QX + PasX * i, QY + QCoteY - PasY * j, PasX, PasY);
    end;
  begin
      // extraction de la matrice
      QTaille := QBarCode.GetNbQRTaille;
      MTX     := QBarCode.GetMatrixQRResultat;
      PasX    := round(QCoteX / QTaille);
      PasY    := round(QCoteY / QTaille);
      for i := 0 to QTaille - 1 do
        for j := 0 to QTaille - 1 do
          DessineCarre(i, j, MTX[i, j]);
  end;
  procedure QDrawTexte(const QX, QY: integer; const QTexte: string; const QTextColor: TColor = clBlack);
  begin
    FPDFDoc.Canvas.SetRGBStrokeColor(TPdfColor(QTextColor));
    FPDFDoc.Canvas.SetRGBFillColor(TPdfColor(QTextColor));
    FPDFDoc.Canvas.TextOut(QX, QY, QTexte);
  end;
  procedure QDrawNearStation(const QST: TBaseStation; const AsNext: boolean; const QHT: integer);
  const
    FMT_NEAR_ST = '%s: %d.%d - Dist: %.2f m, Cap: %.2f°, Incl: %.2f°';
  var
    SS: String;
    QV: TPoint3Df;
  begin
    QV.setFrom(QST.PosStation.X - MyStation.PosStation.X,
               QST.PosStation.Y - MyStation.PosStation.Y,
               QST.PosStation.Z - MyStation.PosStation.Z);
    GetBearingInc(360.00, 360.00, QV, QLong, QAzimut, QPente);
    FPDFDoc.Canvas.SetFont('Arial', HAUTEUR_TEXTE_ID_NEXT_STATION);
    SS := Format(FMT_NEAR_ST, [IIF(AsNext, 'Next', 'Prev'), QST.Entite_Serie, QST.Entite_Station, QLong, QAzimut, QPente]);
    QDrawTexte(QX0 + QMargeX, QHT, SS);
    EWE += SS + #13#10;
  end;

begin
  Nb := GetNbStations() - 1;
  if (IdxCurr > Nb) then Exit;
  MyStation := GetStation(IdxCurr);
  EWE := MyStation.toTextForQRCode();

  QMargeX       := FPDFDoc.Canvas.PageWidth  div MG;
  QMargeY       := FPDFDoc.Canvas.PageHeight div MG;


  QLargeurFiche := (FPDFDoc.Canvas.PageWidth  - round(K * QMargeX)) div 2;
  QHauteurFiche := (FPDFDoc.Canvas.PageHeight - round(K * QMargeY)) div 2;
  LargQRCode    := QLargeurFiche - 2 * QMargeX;


  // origine
  case (IdxCurr MOD 4)of
    0: begin
         QX0 := GUTTER;
         QY0 := GUTTER + QHauteurFiche + GUTTER;
       end;
    1: begin
         QX0 := GUTTER + QLargeurFiche + GUTTER;
         QY0 := GUTTER + QHauteurFiche + GUTTER;
       end;
    2: begin
         QX0 := GUTTER;
         QY0 := GUTTER;
       end;
    3: begin
         QX0 := GUTTER + QLargeurFiche + GUTTER;
         QY0 := GUTTER;
       end;
  end;
  // cadre enveloppe
  FPDFDoc.Canvas.SetLineWidth(0.01);
  FPDFDoc.Canvas.SetRGBStrokeColor(TPdfColor(clBlack));
  FPDFDoc.Canvas.SetRGBFillColor(TPdfColor(clWhite));
  QDrawRectangle(QX0, QY0, QLargeurFiche, QHauteurFiche);
  QBarCode := TBarcodeQR.Create(nil);
  try
    try
      QBarCode.ECCLevel     := eBarcodeQR_ECCLevel_H;  // ne pas oublier: Haute classe de qualité
      QBarCode.Text         := EWE;
      QBarCode.Generate;
      QDrawQRCode(QX0 + QMargeX, QY0 + QMargeX, LargQRCode, LargQRCode);
    except
      AfficherMessageErreur('**** ');
    end;
  finally
    FreeAndNil(QBarCode);
  end;

  FPDFDoc.Canvas.SetFont('Arial', HAUTEUR_TEXTE_ID_STATION);
  QDrawTexte(QX0 + QMargeX, QY0 + QHauteurFiche - HAUTEUR_TEXTE_ID_STATION - 10, Format('Station: %d.%d', [MyStation.Entite_Serie, MyStation.Entite_Station]));
  FPDFDoc.Canvas.SetFont('Arial', HAUTEUR_TEXTE_COORDONNEES);
  QDrawTexte(QX0 + QMargeX, QY0 + QHauteurFiche - HAUTEUR_TEXTE_ID_STATION - 10 - HAUTEUR_TEXTE_COORDONNEES - 10,
             Format('X = %s, Y = %s, Z = %s', [
                    FormatterNombreAvecSepMilliers(MyStation.PosStation.X, 0),
                    FormatterNombreAvecSepMilliers(MyStation.PosStation.Y, 0),
                    FormatterNombreAvecSepMilliers(MyStation.PosStation.Z, 0)
                    ]));

  // imprimer la station suivante et son cap
  EWE := EWE + #13#10 +
             '================' +
             #13#10;
  IdxNext := IdxCurr + 1;
  if (IdxNext < Nb) then
  begin
    MyStationNear := GetStation(IdxNext);
    QDrawNearStation(MyStationNear, True, QY0 + QHauteurFiche - HAUTEUR_TEXTE_ID_STATION - 10 - HAUTEUR_TEXTE_COORDONNEES - 10 - HAUTEUR_TEXTE_ID_NEXT_STATION - 12);
  end;
  IdxPrev := IdxCurr - 1;
  if (IdxPrev >= 0) then
  begin
    MyStationNear := GetStation(IdxPrev);
    QDrawNearStation(MyStationNear, False, QY0 + QHauteurFiche - HAUTEUR_TEXTE_ID_STATION - 10 - HAUTEUR_TEXTE_COORDONNEES - 10 - HAUTEUR_TEXTE_ID_NEXT_STATION - 8 - HAUTEUR_TEXTE_ID_NEXT_STATION - 8);
  end
end;

procedure TFichesTopo.ImprimerLesFiches(const QFilenamePDF: TStringDirectoryFilename; const QCompressed: boolean);
var
  i: integer;
  FP: TFileStream;
  PW, PH, Nb: Integer;
begin
  AfficherMessage(Format('-- %s.ImprimerLesFiches()', [classname]));
  AfficherMessageErreur('000');

  FPDFDoc.NewDoc;     // Nouveau document
  if (QCompressed) then  FPDFDoc.CompressionMethod := cmFlateDecode
                   else  FPDFDoc.CompressionMethod := cmNone;
    FPDFDoc.AddPage;  // Nouvelle page
    FPDFDoc.Canvas.PageWidth  := FPDFDoc.DefaultPageWidth;
    FPDFDoc.Canvas.PageHeight := FPDFDoc.DefaultPageHeight;
    FPDFDoc.Canvas.SetFont('Arial', 10);
    Nb := GetNbStations();
    for i := 0 to Nb - 1 do
    begin
      ImprimerUneFiche(i);
      if ((i+1) MOD 4 = 0) then
      begin
        FPDFDoc.AddPage;
        AfficherMessageErreur(Format('Generating %d of %d ', [i+1, Nb]));
        if (Assigned(FProcProgression)) then FProcProgression('Impression fiches', i, 0, Nb-1, 0);
      end;
    end;
    // sauvegarde PDF
    AfficherMessageErreur(Format('Save to file with%s compression: %s ', [IIF(QCompressed, '', 'out'), QFilenamePDF]));
    FP := TFileStream.Create(trim(QFilenamePDF), fmCreate);
    try
      FPDFDoc.SaveToStream(FP);
    finally
      FreeAndNil(FP);
    end;
    AfficherMessageErreur('Done');
    AfficherMessageErreur('');
  FPDFDoc.FreeDoc;
end;

procedure TFichesTopo.Finaliser();
begin
  try
    AfficherMessage('--> Finalise PDF');
    FListeStations.ClearListe();
  finally
    FreeAndNil(FListeStations);
    FreeAndNil(FPDFDoc);
  end;
end;

procedure TFichesTopo.AddStation(const BS: TBaseStation);
begin
  FListeStations.AddElement(BS);
end;

procedure TFichesTopo.AddStationBySerieStation(const Sr: TNumeroSerie; const St: TNumeroStation);
var
  BS: TBaseStation;
begin
  if (FBDDEntites.GetEntiteViseeFromSerSt(SR, ST, BS)) then self.AddStation(BS);
end;

function TFichesTopo.GetStation(const Idx: integer): TBaseStation;
begin
  Result := FListeStations.GetElement(Idx);
end;

function TFichesTopo.GetNbStations(): integer;
begin
  result := FListeStations.GetNbElements();
end;

procedure TFichesTopo.RemoveStation(const Idx: integer);
begin
  FListeStations.RemoveElement(Idx);
end;
end.
