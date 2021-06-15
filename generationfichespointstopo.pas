// Génération de fiches topo en PDF avec QRCode
unit GenerationFichesPointsTopo;

{$INCLUDE CompilationParameters.inc}

interface

uses
  Classes, SysUtils, Graphics,
  StructuresDonnees,
  Common,
  FPimage,
  FPReadJPEG,
  FPWriteJPEG,
  PdfDoc,
  PdfFonts,
  PdfTypes,
  ubarcodes;
type
{ TEnsembleFichesPointsTopo }

 TEnsembleFichesPointsTopo = class
  private
    FBarCode: TBarcodeQR;
  public
    function  Initialise(): Boolean;
    //function  PutStation // inutile
    procedure GenererUneFicheTerrain(const QPDF: TPdfDoc; const S: string);
    // nouvelles fiches
    procedure CreerNouvellesFiches(const Nb: integer; const QFileName: string);
end;
procedure DrawQRCodeToPDF(const MyPDF: TPdfDoc; const Mtx: TMatriceResultat; const Taille: integer; const X, Y, Cote: Double);


implementation
function GenererCodeTerrain(): string;
const
 NN = 6;
var
 c: Integer;
 Q: Char;
begin
 Result := '';
 for c := 0 to NN - 1 do
 begin
   Q := chr(65 + Trunc(26 * Random));
   Result += Q;
 end;
end;

{ TEnsembleFichesPointsTopo }
procedure DrawQRCodeToPDF(const MyPDF: TPdfDoc; const Mtx: TMatriceResultat; const Taille: integer; const X, Y, Cote: Double);
var
  Pas: double;
  i: Integer;
  j: Integer;
  procedure DessineCarre(const i, j, V: integer);
  var
    WU: TColor;
  begin
    WU := IIF((V <> 0), clBlack, clWhite);

    case V of
       0: WU := clWhite;     // données: Blanc
       1: WU := clBlack;     // données: Noir
      16: WU := clWhite; //clLime;      // carrés de contrôle (zones blanches)
      17: WU := clBlack; //clBlue;      // carrés de contrôle (zones noires)
      20: WU := clBlack; //clFuchsia;       // carré en M[8,8]: noir
      21: WU := clBlack; //clMaroon;    // carré en M[8,8]: noir
      32: WU := clWhite; //clAqua;    // zones de détrompage: blanc
      33: WU := clBlack; //clGreen;    // zones de détrompage: noir
      65: WU := clBlack; //clRed;     // carrés en M[6, 8] et [8, 6]: noir
    else
      WU := clGray;
    end;
    //*)
    with MyPDF.Canvas do
    begin
      Rectangle(X + Pas * i,
                Y + Cote - Pas * j,
                Pas, Pas); //
      SetRGBFillColor(WU);
      SetRGBStrokeColor(WU);
      Fill;          // ordre de remplir
      FillStroke;
    end;
  end;
begin
  // un rectangle englobant
  with MyPDF.Canvas do
  begin
    Rectangle(X, Y, Cote, Cote);
    SetRGBFillColor($F0F0F0);
    SetRGBStrokeColor($F0F0F0);
    Fill;          // ordre de remplissage
    FillStroke;    // couleur de chemin
    // pas
    Pas := Cote / Taille;
    for i := 0 to Taille - 1 do
      for j := 0 to Taille - 1 do
        DessineCarre(i, j, Mtx[i, j]);
   end;
end;

function TEnsembleFichesPointsTopo.Initialise(): Boolean;
begin
  Result := False;
  try
    AfficherMessage(Format('%s.Initialise', [ClassName]));
    // démarrage du TBarcodeQR
    FBarCode := TBarcodeQR.Create(Nil);
    Result := True;
  except
  end;
end;




procedure TEnsembleFichesPointsTopo.GenererUneFicheTerrain(const QPDF: TPdfDoc; const S: string);
const
  PAGE_HEIGHT       = 750;
  PAGE_WIDTH        = PAGE_HEIGHT / sqrt(2);
  COTE_CARRE_QRCODE = PAGE_WIDTH - 50;
  //POS_Y_RESEAU      = PAGE_HEIGHT - 20;
  POS_Y_LBL_STATION = PAGE_HEIGHT - 25;
  POS_Y_QRCODE      = PAGE_HEIGHT - 85 - COTE_CARRE_QRCODE;
  POS_FER_GAUCHE    = 25;

  POS_Y_COORDONNEES = POS_Y_QRCODE + COTE_CARRE_QRCODE + 30;
  POS_Y_DATE        = POS_Y_COORDONNEES + 40;

var
  WU: String;
  EWE: TMatriceResultat;
  Nb: Integer;
begin
  //!\ Dans l'unité ubarcodes.pas, ligne 451:
  // vérifier que cette ligne vaut: if (FDoMonochrome) then

  AfficherMessage(Format('--> GenererUneFicheStation: %s', [S]));
  try
    QPDF.AddPage;  // nouvelle page
    // notre fameux QRCode ici
    FBarCode.ECCLevel     := eBarcodeQR_ECCLevel_H;  // ne pas oublier: Haute classe de qualité
    FBarCode.Text := Trim(S);
    FBarCode.Generate;
    EWE := FBarCode.GetMatrixQRResultat;
    Nb  := FBarCode.GetNbQRTaille;
    DrawQRCodeToPDF(QPDF, EWE, Nb, POS_FER_GAUCHE, POS_Y_QRCODE, COTE_CARRE_QRCODE);
    // les autres infos
    QPDF.Canvas.SetRGBStrokeColor(clBlack);
    QPDF.Canvas.SetRGBFillColor(clBlack);
    // ID station
    QPDF.Canvas.SetFont(DEFAULT_FONT_NAME, 80);
    QPDF.Canvas.TextOut(POS_FER_GAUCHE, POS_Y_LBL_STATION, S);
  except

  end;
end;

procedure TEnsembleFichesPointsTopo.CreerNouvellesFiches(const Nb: integer; const QFileName: string);
var
  ListeCodesTerrain: TStringList;
  FPDFDoc          : TPdfDoc;
  VFile            : TFileStream;
  NbFichesCreees   : integer;
  EWE : string;
  i: Integer;
begin
  AfficherMessage(Format('%s.CreerNouvellesFiches: %d', [ClassName, Nb]));
  // création des codes terrain
  ListeCodesTerrain := TStringList.Create;
  FPDFDoc := TPdfDoc.Create;
  VFile   := TFileStream.Create(QFileName, fmCreate);
  try
    ListeCodesTerrain.Sorted     := True;
    ListeCodesTerrain.Duplicates := dupIgnore;
    ListeCodesTerrain.Clear;
    // démarrage des générateurs
    Randomize;
    NbFichesCreees := 0;
    // ici, on n'utilise pas de boucle for pour éviter des collisions de nom
    while (NbFichesCreees < Nb) do
    begin
      EWE := GenererCodeTerrain;
      ListeCodesTerrain.Add(EWE);
      Inc(NbFichesCreees);
    end;
    // fiche de test
    EWE := 'LLANFAIRPWLLGWYNGYLLGOGERYCHWYRNDROBWLLLLANTISILIOGOGOGOCH';
    ListeCodesTerrain.Add(EWE);
    Inc(NbFichesCreees);
    // génération des PDF
    FPDFDoc.NewDoc;
    FPDFDoc.AddPage;

    // envoi des fiches
    for i := 0 to ListeCodesTerrain.Count - 1 do
    begin
      EWE := ListeCodesTerrain.Strings[i];
      GenererUneFicheTerrain(FPDFDoc, EWE);
    end;
    // sauvegarde finale
    FPDFDoc.SaveToStream(VFile);
    FPDFDoc.FreeDoc;
    // vidages
    ListeCodesTerrain.Clear;
  finally
    FreeAndNil(VFile);//VFile.Free;
    FreeAndNil(FPDFDoc);//FPDFDoc.Free;
    FreeAndNil(ListeCodesTerrain);//ListeCodesTerrain.Free;
  end;
end;


end.


