unit unitCroquisTerrain;
// Gestion du croquis de terrain simplifié
// Support: Textes et polylignes (remplies ou non)
// 13/08/2020: Point de contrôle temporel (contrôle de version)

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, math,
  StructuresDonnees,
  Common,
  UnitListesSimplesWithGeneriques,
  UnitEntitesExtended,
  DOM, XMLWrite, XMLRead;

type  TCroquisTerrain = class
  private
    FFilename                 : TStringDirectoryFilename;
    FBDDEntites               : TBDDEntites;
    FBufferOfVertex           : TCroquisBufferVertexes;
    FListeStylesPolylignes    : TCroquisListeStylesPolylignes;
    FListeStylesAnnotations   : TCroquisListeStylesAnnotations;
    FListeAnnotations         : TCroquisListeAnnotations;
    FListePolylignes          : TCroquisListePolylignes;

    FCurrentIdxStyleAnnotation: integer;
    FCurrentIdxStylePolyligne : TKrobardIDStylePolyligne;

    FLastAnnotationDeleted    : TKrobardAnnotation;
    FLastPolyLineDeleted      : TKrobardPolyligne;
    FCroquisTerrainIsReady    : boolean;
    // Génère un fichier include pour GHCaveDraw
    function  FindNearestIdxPolyligneToXY(const P: TPoint2Df): integer;
    function  PolySimplifyInt2D(const Tol: double; out QSimple: TArrayPoints3Df): integer;
    procedure SimplifyInt2D(var Tol2: double; var Marker: array of boolean; const j, k: integer);

    procedure InitialiserStylesAnnotations();
    procedure InitialiserStylesPolylignes();
    procedure ViderListesStyles();

  public
    procedure ViderLesListesObjets();
    property  IsReady    : boolean read FCroquisTerrainIsReady;
    property  CurrentIdxStyleAnnotation: integer read FCurrentIdxStyleAnnotation write FCurrentIdxStyleAnnotation;
    property  CurrentIdxStylePolyligne: TKrobardIDStylePolyligne read FCurrentIdxStylePolyligne write FCurrentIdxStylePolyligne;
    function  Initialiser(const QBDDEntites: TBDDEntites; const QFileName: string): boolean;
    procedure Finaliser();
    function  LoadFromXML(const QFilename: TStringDirectoryFilename): boolean;
    function  SaveToXML(const QFilename: TStringDirectoryFilename): boolean;
    procedure BeginPolyline(const P: TPoint3Df; const IdxPolyAProlonger: integer);
    procedure AddVertexAtBuffer(const P: TPoint3Df); overload;
    procedure AddVertexAtBuffer(const QX, QY: double); overload;

    procedure EndPolyline();
    function  MakePolyline(out MyPoly: TKrobardPolyligne): boolean;
    function  GetNbStylesAnnotations(): integer; inline;
    function  GetStyleAnnotation(const Idx: integer): TTexteAttributs;

    function  GetNbStylesPolyLines(): integer; inline;
    function  GetStylePolyligne(const Idx: integer): TKrobardStylePolyligne;

    procedure AddAnnotation(const Annotation: TKrobardAnnotation); inline;
    function  GetAnnotation(const Idx: integer): TKrobardAnnotation; inline;
    procedure PutAnnotation(const Idx: integer; const AAnnotation: TKrobardAnnotation); inline;
    function  RemoveAnnotation(const Idx: integer): boolean; inline;
    function  GetNbAnnotations(): integer; inline;

    procedure AddPolyline(const Polyline: TKrobardPolyligne); inline;
    function  GetPolyline(const Idx: integer): TKrobardPolyligne; inline;
    procedure PutPolyline(const Idx: integer; const APolyline: TKrobardPolyligne); inline;
    function  RemovePolyline(const Idx: integer): boolean; inline;
    function  GetNbPolylines(): integer; inline;
    // rechercher la polyligne dont la fin est proche de (x, y)
    function  FindIdxPolylineWithExtr2NearToXY(const QX, QY: double; const RC: double): integer;

    function  GetNbBufferVertexes(): integer;
    function  GetBufferVertex(const Idx: integer): TPoint3Df;

    procedure SetCurrentIdxStylePolyligne(const S: TKrobardIDStylePolyligne);
    procedure SetCurrentIdxStyleAnnotation(const S: integer);

    function  TryDeleteAnnnotationNearToXY(const MyPos: TPoint2Df): boolean;
    function  TryDeletePolylineNearToXY(const MyPos: TPoint2Df): boolean;
end;

implementation
const
  TOLERANCE_CORDE = 0.05;
const
   GTX_KEY_CROQUIS_SECTION_STYLES_ANNOTATIONS = 'StylesAnnotations';
    GTX_KEY_STYLE_ANNOTATION                   = 'AnnotationStyle';
      GTX_ATTR_STYLE_ANNOTATION_STYLE_NAME      = 'ANStyleName';
      GTX_ATTR_STYLE_ANNOTATION_FONT_NAME       = 'ANFontName';
      GTX_ATTR_STYLE_ANNOTATION_FontColor       = 'ANFontColor';
      GTX_ATTR_STYLE_ANNOTATION_BackColor       = 'ANBackColor';
      GTX_ATTR_STYLE_ANNOTATION_FontStyle       = 'ANFontStyle';
      GTX_ATTR_STYLE_ANNOTATION_HauteurTexte    = 'ANHauteurTexte';
      GTX_ATTR_STYLE_ANNOTATION_Position        = 'ANPosition';
      GTX_ATTR_STYLE_ANNOTATION_AngleRot        = 'ANAngleRot';
   GTX_KEY_CROQUIS_SECTION_STYLES_POLY        = 'StylesPolylines';
    GTX_KEY_STYLE_POLY                         = 'PolyStyle';
   GTX_KEY_CROQUIS_SECTION_ANNOTATIONS        = 'Annotations';
     GTX_KEY_ANNOTATION                        = 'UneAnnotation';
       GTX_ATTR_ANNOTATION_IDSTYLE               = 'IDStyle';
       GTX_ATTR_ANNOTATION_ALIGNMENT             = 'Alignement';
       GTX_ATTR_ANNOTATION_BASEPOINT             = 'Basepoint';
       GTX_ATTR_ANNOTATION_OFFSET_X              = 'OffsetX';
       GTX_ATTR_ANNOTATION_OFFSET_Y              = 'OffsetY';
       GTX_ATTR_ANNOTATION_OFFSET_Z              = 'OffsetZ';
       GTX_ATTR_ANNOTATION_TEXTE                 = 'Texte';
   GTX_KEY_CROQUIS_SECTION_POLYLINES          = 'Polylines';
     GTX_KEY_POLYLINE                           = 'UnePolyline';
       GTX_ATTR_POLYLINE_IDSTYLE                 = 'IDStyle';
       GTX_ATTR_POLYLINE_VERTEXLIST              = 'Vertexes';
        GTX_KEY_POLYLINE_VERTEX                      = 'Vertex';
          GTX_ATTR_POLYLINE_VERTEX_IDBasePoint   = 'IDBaseStation';
          GTX_ATTR_POLYLINE_VERTEX_OffsetX   = 'OffsetX';
          GTX_ATTR_POLYLINE_VERTEX_OffsetY   = 'OffsetY';
          GTX_ATTR_POLYLINE_VERTEX_OffsetZ   = 'OffsetZ';
// Simplification d'un tracé
 function VecMinInt2D(const A, B: TPoint3Df): TPoint3Df;
 begin
   Result.X := A.X - B.X;
   Result.Y := A.Y - B.Y;
   Result.Z := A.Z - B.Z;
 end;

 function DotProdInt2D(const A, B: TPoint3Df): Double;
 begin
   Result := A.X * B.X + A.Y * B.Y;
 end;

 function NormSquaredInt2D(const A: TPoint3Df): Double;
 begin
   Result := A.X * A.X + A.Y * A.Y;
 end;

 function DistSquaredInt2D(const A, B: TPoint3Df): Double;
 begin
   Result := NormSquaredInt2D(VecMinInt2D(A, B));
 end;
 function SafeDivide(const a, b: double; const Default: double): double;
 begin
   try
     Result := a / b;
   except
     Result := Default;
   end;
 end;

procedure TCroquisTerrain.SimplifyInt2D(var Tol2: double;
                                        var Marker: array of boolean; const j, k: integer);
 // Simplify polyline in OrigList between j and k. Marker[] will be set to True
 // for each point that must be included
var
   i, MaxI: integer; // Index at maximum value
   MaxD2: double;    // Maximum value squared
   CU, CW, B: double;
   DV2: double;
   P0, P1, PB, U, W, Oi: TPoint3Df;
begin
   // Is there anything to simplify?
   if k <= j + 1 then exit;

   P0 := GetBufferVertex(j);
   P1 := GetBufferVertex(k);
   U  := VecMinInt2D(P1, P0); // Segment vector
   CU := DotProdInt2D(U, U); // Segment length squared
   MaxD2 := 0;
   MaxI  := 0;
   // Loop through points and detect the one furthest away
   for i := j + 1 to k - 1 do begin
     Oi := GetBufferVertex(i);
     W  := VecMinInt2D(Oi, P0);
     CW := DotProdInt2D(W, U);

     // Distance of point Orig[i] from segment
     if (CW <= 0) then
     begin
       // Before segment
       DV2 := DistSquaredInt2D(Oi, P0)
     end
     else
     begin
       if (CW > CU) then
       begin
         // Past segment
         DV2 := DistSquaredInt2D(Oi, P1);
       end else
       begin
         // Fraction of the segment
         B := SafeDivide(CW, CU, 0.00);
         PB.X := P0.X + B * U.X;
         PB.Y := P0.Y + B * U.Y;
         DV2 := DistSquaredInt2D(Oi, PB);
       end;
     end;

     // test with current max distance squared
     if (DV2 > MaxD2) then
     begin
       // Orig[i] is a new max vertex
       MaxI  := i;
       MaxD2 := DV2;
     end;
   end;

   // If the furthest point is outside tolerance we must split
   if (MaxD2 > Tol2) then
   begin // error is worse than the tolerance
     // split the polyline at the farthest vertex from S
     Marker[MaxI] := True;  // mark Orig[maxi] for the simplified polyline
     // recursively simplify the two subpolylines at Orig[maxi]
     SimplifyInt2D(Tol2, Marker, j, MaxI); // polyline Orig[j] to Orig[maxi]
     SimplifyInt2D(Tol2, Marker, MaxI, k); // polyline Orig[maxi] to Orig[k]
   end;
end;
//*)


//*******************************************************************************
{ TCroquisTerrain }
function TCroquisTerrain.PolySimplifyInt2D(const Tol: double; out   QSimple: TArrayPoints3Df): integer;
var
  i, NbBuffVertex: integer;
  Marker: array of boolean;
  Tol2: double;
  FTmpLst: TListeSimple<TPoint3Df>;
begin
  NbBuffVertex := GetNbBufferVertexes();
  AfficherMessage(Format('PolySimplifyInt2D: Tol: %.2f, n=%d', [Tol, NbBuffVertex]));
  Result := -1;
  if (NbBuffVertex < 2) then exit;
  Tol2 := Tol * Tol;
  try
    // Create a marker array
    SetLength(Marker, NbBuffVertex);
    // Include first and last point
    Marker[0]     := True;
    Marker[NbBuffVertex - 1] := True;
    // Exclude intermediate for now
    for i := 1 to NbBuffVertex - 2 do Marker[i] := False;
     // Simplify
     SimplifyInt2D(Tol2, Marker, 0, NbBuffVertex - 1);
     AfficherMessage(Format('Marqueurs -- NbBuffVertex=%d', [NbBuffVertex]));
     // Copy to resulting list
     FTmpLst:= TListeSimple<TPoint3Df>.Create;
     FTmpLst.ClearListe();
     for i := 0 to NbBuffVertex - 1 do
     begin
       AfficherMessage(Format('%d: %s', [i, BoolToStr(Marker[i], 'Vrai', 'Faux')]));
       if (Marker[i]) then FTmpLst.AddElement(GetBufferVertex(i));
     end;
     NbBuffVertex := FTmpLst.GetNbElements();
     SetLength(QSimple, NbBuffVertex);
     for i := 0 to NbBuffVertex - 1 do
     begin
       QSimple[i] := FTmpLst.GetElement(i);
       AfficherMessage(Format('%d: %.2f, %.2f', [i, QSimple[i].X, QSimple[i].Y]));
     end;
     FTmpLst.ClearListe();
     Result := NbBuffVertex;
   finally
     FreeAndNil(FTmpLst);//FTmpLst.Free;
   end;
end;

procedure TCroquisTerrain.ViderLesListesObjets();
begin
  FListeAnnotations.ClearListe();
  FListePolylignes.ClearListe();
end;



procedure TCroquisTerrain.InitialiserStylesPolylignes();
  function MiouMiou(const Miou        : string;
                    const QLineColor  : TColor;
                    const QLineOpacity: byte;
                    const QLineStyle  : TPenStyle;
                    const QLineWidth  : integer;
                    const QFillColor  : TColor;
                    const QFillOpacity: byte;
                    const QClosed     : boolean;
                    const QFilled     : boolean): TKrobardStylePolyligne;
  begin
    Result.Name       := Miou;
    Result.LineColor  := QLineColor;
    Result.LineOpacity:= QLineOpacity;
    Result.LineWidth  := QLineWidth;
    Result.LineStyle  := QLineStyle;
    Result.FillColor  := QFillColor;
    Result.FillOpacity:= QFillOpacity;
    Result.Closed     := QClosed;
    Result.Filled     := QFilled;
  end;
begin
  FListeStylesPolylignes.ClearListe();
  // lignes
  FListeStylesPolylignes.AddElement(MiouMiou('Paroi'             , clMaroon    , 255, psSolid, 2, clWhite    , 128, false, false));
  FListeStylesPolylignes.AddElement(MiouMiou('Paroi_fracassee'   , clRed       , 255, psSolid, 2, clWhite    , 128, false, false));
  FListeStylesPolylignes.AddElement(MiouMiou('Surplomb'          , clGreen     , 255, psSolid, 2, clWhite    , 128, false, false));
  FListeStylesPolylignes.AddElement(MiouMiou('Ressaut'           , clPurple    , 255, psSolid, 0, clWhite    , 128, false, false));
  FListeStylesPolylignes.AddElement(MiouMiou('Ligne_pente'       , clGray      , 255, psSolid, 0, clWhite    , 128, false, false));
  // polygones
  FListeStylesPolylignes.AddElement(MiouMiou('Eboulis'           , clGray      , 255, psSolid, 0, clGray     , 128, true , true));
  FListeStylesPolylignes.AddElement(MiouMiou('Gros_bloc'         , clBlack     , 255, psSolid, 0, clGray     , 128, true , true));
  FListeStylesPolylignes.AddElement(MiouMiou('Lac'               , clBlue      , 255, psSolid, 0, clAqua     , 128, true , true));
  FListeStylesPolylignes.AddElement(MiouMiou('Siphon'            , clNavy      , 255, psSolid, 0, clBlue     , 192, true , true));
  FListeStylesPolylignes.AddElement(MiouMiou('Argile'            , $00157B95   , 255, psSolid, 0, $00157B95  , 192, true , true));
  FListeStylesPolylignes.AddElement(MiouMiou('Sable '            , clYellow    , 255, psSolid, 0, clYellow   , 192, true , true));
end;

procedure TCroquisTerrain.ViderListesStyles();
begin
  FListeStylesPolylignes.ClearListe();
  FListeStylesAnnotations.ClearListe();
end;

procedure TCroquisTerrain.InitialiserStylesAnnotations();
  function CuiCui(const QStyleName: string;
                  const QFontName: string;
                  const QFontColor: TColor;
                  const QFontSize : double;             // en mètres
                  const QFontStyle: TFontStyles;
                  const QTextAlignment: byte): TTexteAttributs;
  begin
    Result.StyleName    := QStyleName;
    Result.BackColor    := clWhite;
    Result.FontName     := QFontName;
    Result.FontColor    := QFontColor;
    Result.Alignement   := QTextAlignment;
    Result.HauteurTexte := QFontSize;
    Result.FontStyle    := QFontStyle;
    Result.AngleRot     := 0;
  end;
begin
  FListeStylesAnnotations.ClearListe();
  FListeStylesAnnotations.AddElement(Cuicui('default'    , DEFAULT_FONT_NAME, clBlack, 1.0, [], 1));
  FListeStylesAnnotations.AddElement(Cuicui('Cotation'   , DEFAULT_FONT_NAME, clBlue , 1.0, [fsBold], 1));
end;



function TCroquisTerrain.Initialiser(const QBDDEntites: TBDDEntites; const QFileName: string): boolean;
begin
  AfficherMessage(Format('%s.Initialiser: %s', [self.ClassName, QFileName]));
  FFilename := QFileName;
  result := false;
  FBDDEntites := QBDDEntites;
  AfficherMessageErreur('FBDDEntites ' + IIF(Assigned(FBDDEntites), 'OK', 'KO'));
  FCurrentIdxStyleAnnotation:= 0;
  FCurrentIdxStylePolyligne := 0;
  try
    FBufferOfVertex           := TCroquisBufferVertexes.Create;
    FListeStylesPolylignes    := TCroquisListeStylesPolylignes.Create;
    FListeStylesAnnotations   := TCroquisListeStylesAnnotations.Create;
    FListeAnnotations         := TCroquisListeAnnotations.Create;
    FListePolylignes          := TCroquisListePolylignes.Create;

    ViderLesListesObjets();
    InitialiserStylesAnnotations();
    InitialiserStylesPolylignes();
    FCroquisTerrainIsReady    := True;
    result := true;
  except
  end;
end;

procedure TCroquisTerrain.Finaliser();
begin
  try
    ViderListesStyles();
    ViderLesListesObjets();
    FCroquisTerrainIsReady    := false;
  finally
    FreeAndNil(FBufferOfVertex);//FBufferOfVertex.Free;
    FreeAndNil(FListeStylesPolylignes);//FListeStylesPolylignes.Free;
    FreeAndNil(FListeStylesAnnotations);//FListeStylesAnnotations.Free;
    FreeAndNil(FListePolylignes);//FListePolylignes.Free;
    FreeAndNil(FListeAnnotations);//FListeAnnotations.Free;
  end;
end;


function TCroquisTerrain.LoadFromXML(const QFilename: TStringDirectoryFilename): boolean;
var
  MyDocXML: TXMLDocument;
  GHTopoCroquisRoot         : TDOMElement;
  SectionStylesAnnotation   : TDOMNodeList;
  SectionStylesPolylines    : TDOMNodeList;
  SectionAnnotations        : TDOMNodeList;
  SectionPolylignes         : TDOMNodeList;
  ListeVertex               : TDOMNodeList;
  EWE, WU, QIN, QAT, WOK    : TDOMNode;
  i, Nb, St, QST            : Integer;
  MyAnnotation              : TKrobardAnnotation;
  MyPolyline                : TKrobardPolyligne;
  QNbAnn, QNbSts            : LongWord;
  MyVertex                  : TKrobardPolyVertex;
  QSR                       : TNumeroSerie;
  MyEntite                  : TBaseStation;
  QPoint                    : TPoint3Df;
  function QExtractTextContent(const QQQ: TDOMNode): string; inline;
  begin
    Result := Trim(PasStrToXMLStr(QQQ.TextContent));
  end;
begin
  result := false;
  AfficherMessage(Format('%s.LoadFromXML(%s)', [ClassName, QFilename]));
  FListeAnnotations.ClearListe();
  FListePolylignes.ClearListe();
  // réinit du document
  MyDocXML := TXMLDocument.Create;
  try
    ReadXMLFile(MyDocXML, QFilename);
    GHTopoCroquisRoot := MyDocXML.DocumentElement;
    // annotations
    AfficherMessage('======================');
    AfficherMessage('-> Reading section: ' + GTX_KEY_CROQUIS_SECTION_ANNOTATIONS);
    EWE := GHTopoCroquisRoot.FindNode(GTX_KEY_CROQUIS_SECTION_ANNOTATIONS);
    if (EWE <> Nil) then
    begin
      SectionAnnotations := EWE.ChildNodes;
      QNbAnn := SectionAnnotations.Count;
      for i := 0 to QNbAnn - 1 do
      begin
        try
          WU := SectionAnnotations.Item[i];
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ANNOTATION_ALIGNMENT);
          MyAnnotation.Alignment := StrToIntDef(QIN.TextContent, 0);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ANNOTATION_IDSTYLE);
          MyAnnotation.IDStyle   := StrToIntDef(QIN.TextContent, 0);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ANNOTATION_BASEPOINT);
          MyAnnotation.Position.IDBaseStation :=  StrToInt64Def(QIN.TextContent, -1);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ANNOTATION_OFFSET_X);
          MyAnnotation.Position.Offset.X :=  ConvertirEnNombreReel(QIN.TextContent, 0.00);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ANNOTATION_OFFSET_Y);
          MyAnnotation.Position.Offset.Y :=  ConvertirEnNombreReel(QIN.TextContent, 0.00);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ANNOTATION_OFFSET_Z);
          MyAnnotation.Position.Offset.Z :=  ConvertirEnNombreReel(QIN.TextContent, 0.00);
          QIN := WU.Attributes.GetNamedItem(GTX_ATTR_ANNOTATION_TEXTE);
          MyAnnotation.Texte :=  QExtractTextContent(QIN);
          self.AddAnnotation(MyAnnotation);
        except
        end;
      end;
    end;
    // polylignes
    AfficherMessageErreur('======================');
    AfficherMessageErreur('-> Read section: ' + GTX_KEY_CROQUIS_SECTION_POLYLINES);
    EWE := GHTopoCroquisRoot.FindNode(GTX_KEY_CROQUIS_SECTION_POLYLINES);
    if (EWE <> Nil) then
    begin
      SectionPolylignes := EWE.ChildNodes;
      AfficherMessageErreur(Format('%d polylignes', [SectionPolylignes.Count]));
      for i := 0 to SectionPolylignes.Count - 1 do
      begin
        WU  := SectionPolylignes.Item[i];
        QIN := WU.Attributes.GetNamedItem(GTX_ATTR_POLYLINE_IDSTYLE);
        MyPolyline.IDStyle := StrToIntDef(QIN.TextContent, 0);
        QAT := WU.FindNode(GTX_ATTR_POLYLINE_VERTEXLIST);
        if (QAT <> NIL) then
        begin
          ListeVertex := QAT.ChildNodes;
          QNbSts := ListeVertex.Count;
          AfficherMessageErreur(Format('** Polyligne %d - %d vertex', [i, QNbSts]));
          if (QNbSts = 0) then Continue;
          SetLength(MyPolyline.Sommets, QNbSts);
          for St := 0 to QNbSts -1 do
          begin
            WOK := ListeVertex.Item[St];
            QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_POLYLINE_VERTEX_IDBasePoint);
            MyVertex.IDBaseStation := StrToInt64Def(QIN.TextContent, -1);
            QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_POLYLINE_VERTEX_OffsetX);
            MyVertex.Offset.X := ConvertirEnNombreReel(QIN.TextContent, 0.00);
            QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_POLYLINE_VERTEX_OffsetY);
            MyVertex.Offset.Y := ConvertirEnNombreReel(QIN.TextContent, 0.00);
            QIN := WOK.Attributes.GetNamedItem(GTX_ATTR_POLYLINE_VERTEX_OffsetZ);
            MyVertex.Offset.Z := ConvertirEnNombreReel(QIN.TextContent, 0.00);
            MyPolyline.Sommets[St] := MyVertex;
          end; // St := 0 to QNbSts -1 do;
          // calcul de la bounding box
          MyPolyline.BoundingBox := MakeTRect2Df(INFINI, INFINI, -INFINI, -INFINI);
          for St := 0 to QNbSts -1 do
          begin
            MyVertex := MyPolyline.Sommets[St];
            if (FBDDEntites.CalcCoordinatesFromBasePtAndOffset(MyVertex.IDBaseStation, MyVertex.Offset, QPoint)) then
            begin
              MyPolyline.BoundingBox.X1 := Min(MyPolyline.BoundingBox.X1, QPoint.X);
              MyPolyline.BoundingBox.Y1 := Min(MyPolyline.BoundingBox.Y1, QPoint.Y);
              MyPolyline.BoundingBox.X2 := Max(MyPolyline.BoundingBox.X2, QPoint.X);
              MyPolyline.BoundingBox.Y2 := Max(MyPolyline.BoundingBox.Y2, QPoint.Y);
            end;
          end; // St :
          if (Length(MyPolyline.Sommets) > 0) then self.AddPolyline(MyPolyline);
        end;
      end;
    end;
    // contrôle
    AfficherMessageErreur(#13+#10+'Contenu du croquis');
    Nb := GetNbAnnotations();
    AfficherMessageErreur(Format('%d annotations', [Nb]));
    if (Nb = 0) then exit;
    for i := 0 to Nb - 1 do
    begin
      MyAnnotation := GetAnnotation(i);
      AfficherMessageErreur(Format('-- %d : %d - %s', [i, MyAnnotation.Position.IDBaseStation, MyAnnotation.Texte]));
    end;
    Nb := GetNbPolylines();
    AfficherMessageErreur(Format('%d polylignes', [Nb]));
    if (Nb = 0) then exit;
    for i := 0 to Nb - 1 do
    begin
      MyPolyline := GetPolyline(i);
      QNbSts     :=  length(MyPolyline.Sommets);
      AfficherMessageErreur(Format('-- %d %d : %d sommets', [i, MyPolyline.IDStyle, QNbSts]));
      AfficherMessageErreur(Format('BBX: %f %f -> %f %f', [ MyPolyline.BoundingBox.X1, MyPolyline.BoundingBox.Y1, MyPolyline.BoundingBox.X2, MyPolyline.BoundingBox.Y2]));
    end;
    FCroquisTerrainIsReady    := True;
    result                    := True; // même vide, un croquis est valable
  finally
    FreeAndNil(MyDocXML);//MyDocXML.Free;
  end;
end;

function TCroquisTerrain.SaveToXML(const QFilename: TStringDirectoryFilename): boolean;
var
  MyDocXML: TXMLDocument;
  GHTopoCroquisRoot         : TDOMElement;
  SectionStylesAnnotation   : TDOMElement;
  SectionStylesPolylines    : TDOMElement;
  SectionAnnotations        : TDOMElement;
  SectionPolylignes         : TDOMElement;
  SubSection, SubSubSection , SubSubSubSection: TDOMElement;
  Nb, i, NbSommets, s: Integer;
  MyStyleAnnotation: TTexteAttributs;
  MyAnnotation: TKrobardAnnotation;
  MyPolyline: TKrobardPolyligne;
  MyVertex: TKrobardPolyVertex;
begin
  result := false;
  AfficherMessage(format('%s.SaveToXML(%s)', [ClassName, QFilename]));
  MyDocXML := TXMLDocument.Create;
    GHTopoCroquisRoot := MyDocXML.CreateElement('GHTopoCroquis');
    MyDocXML.AppendChild(GHTopoCroquisRoot);
    // styles annotations
    SectionStylesAnnotation := MyDocXML.CreateElement(GTX_KEY_CROQUIS_SECTION_STYLES_ANNOTATIONS);
    GHTopoCroquisRoot.AppendChild(SectionStylesAnnotation);
    Nb := GetNbStylesAnnotations();
    AfficherMessage(inttostr(nb));
    for i := 0 to Nb - 1 do
    begin
      MyStyleAnnotation := GetStyleAnnotation(i);
      SubSection := MyDocXML.CreateElement(GTX_KEY_STYLE_ANNOTATION);
        SubSection.SetAttribute(GTX_ATTR_STYLE_ANNOTATION_STYLE_NAME, PasStrToXMLStr(MyStyleAnnotation.StyleName));
        SubSection.SetAttribute(GTX_ATTR_STYLE_ANNOTATION_FONT_NAME, PasStrToXMLStr(MyStyleAnnotation.FontName));
        SubSection.SetAttribute(GTX_ATTR_STYLE_ANNOTATION_FontColor, ColorToHTMLColor(MyStyleAnnotation.FontColor));
        SubSection.SetAttribute(GTX_ATTR_STYLE_ANNOTATION_BackColor, ColorToHTMLColor(MyStyleAnnotation.BackColor));
        SubSection.SetAttribute(GTX_ATTR_STYLE_ANNOTATION_HauteurTexte, Format(FORMAT_NB_REAL_2_DEC, [MyStyleAnnotation.HauteurTexte]));
        SubSection.SetAttribute(GTX_ATTR_STYLE_ANNOTATION_Position, Format(FORMAT_NB_INTEGER, [MyStyleAnnotation.Alignement]));
        SubSection.SetAttribute(GTX_ATTR_STYLE_ANNOTATION_AngleRot, Format(FORMAT_NB_INTEGER, [MyStyleAnnotation.AngleRot]));
      SectionStylesAnnotation.AppendChild(SubSection);
    end;
    Nb := GetNbAnnotations();
    if (Nb > 0) then
    begin
      SectionAnnotations := MyDocXML.CreateElement(GTX_KEY_CROQUIS_SECTION_ANNOTATIONS);
      GHTopoCroquisRoot.AppendChild(SectionAnnotations);
      for i := 0 to Nb - 1 do
      begin
        MyAnnotation := GetAnnotation(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_ANNOTATION);
          SubSection.SetAttribute(GTX_ATTR_ANNOTATION_IDSTYLE    , Format(FORMAT_NB_INTEGER, [MyAnnotation.IDStyle]));
          SubSection.SetAttribute(GTX_ATTR_ANNOTATION_ALIGNMENT  , Format(FORMAT_NB_INTEGER, [MyAnnotation.Alignment]));
          SubSection.SetAttribute(GTX_ATTR_ANNOTATION_BASEPOINT  , Format(FORMAT_NB_INTEGER, [MyAnnotation.Position.IDBaseStation]));
          SubSection.SetAttribute(GTX_ATTR_ANNOTATION_OFFSET_X   , Format(FORMAT_NB_REAL_2_DEC, [MyAnnotation.Position.Offset.X]));
          SubSection.SetAttribute(GTX_ATTR_ANNOTATION_OFFSET_Y   , Format(FORMAT_NB_REAL_2_DEC, [MyAnnotation.Position.Offset.Y]));
          SubSection.SetAttribute(GTX_ATTR_ANNOTATION_OFFSET_Z   , Format(FORMAT_NB_REAL_2_DEC, [MyAnnotation.Position.Offset.Z]));
          SubSection.SetAttribute(GTX_ATTR_ANNOTATION_TEXTE      , PasStrToXMLStr(MyAnnotation.Texte));
        SectionAnnotations.AppendChild(SubSection);
      end;
    end;
    // les polylignes
    Nb := GetNbPolylines();
    if (Nb > 0) then
    begin
      SectionPolylignes := MyDocXML.CreateElement(GTX_KEY_CROQUIS_SECTION_POLYLINES);
      GHTopoCroquisRoot.AppendChild(SectionPolylignes);
      for i := 0 to Nb - 1 do
      begin
        MyPolyline := GetPolyline(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_POLYLINE);
          SubSection.SetAttribute(GTX_ATTR_POLYLINE_IDSTYLE, Format(FORMAT_NB_INTEGER, [MyPolyline.IDStyle]));
        SectionPolylignes.AppendChild(SubSection);
        SubSubSection := MyDocXML.CreateElement(GTX_ATTR_POLYLINE_VERTEXLIST);
        SubSection.AppendChild(SubSubSection);
        NbSommets := Length(MyPolyline.Sommets);
        for s := 0 to NbSommets - 1 do
        begin
          MyVertex := MyPolyline.Sommets[s];
          SubSubSubSection := MyDocXML.CreateElement(GTX_KEY_POLYLINE_VERTEX);
            SubSubSubSection.SetAttribute(GTX_ATTR_POLYLINE_VERTEX_IDBasePoint   , Format(FORMAT_NB_INTEGER, [MyVertex.IDBaseStation]));
            SubSubSubSection.SetAttribute(GTX_ATTR_POLYLINE_VERTEX_OffsetX       , Format(FORMAT_NB_REAL_3_DEC, [MyVertex.Offset.X]));
            SubSubSubSection.SetAttribute(GTX_ATTR_POLYLINE_VERTEX_OffsetY       , Format(FORMAT_NB_REAL_3_DEC, [MyVertex.Offset.Y]));
            SubSubSubSection.SetAttribute(GTX_ATTR_POLYLINE_VERTEX_OffsetZ       , Format(FORMAT_NB_REAL_3_DEC, [MyVertex.Offset.Z]));
          SubSubSection.AppendChild(SubSubSubSection);
        end;
      end;
    end;
    // sérialisation du fichier
    WriteXMLFile(MyDocXML, QFilename);
  //finally
    FreeAndNil(MyDocXML);//MyDocXML.Free;
  //end;
end;

procedure TCroquisTerrain.BeginPolyline(const P: TPoint3Df; const IdxPolyAProlonger: integer);
var
  QPoly: TKrobardPolyligne;
  i, QNamespace, QSt: Integer;
  MySommet: TKrobardPolyVertex;
  WU, QIsAntenne: Boolean;
  QSer: TNumeroSerie;
  QP: TPoint3Df;
  EWE: TBaseStation;
begin
  AfficherMessage(format('-- %s.BeginPolyline(%.2f, %.2f)', [self.ClassName, P.X, P.Y]));
  FBufferOfVertex.ClearListe();
  // Prolonger une polyligne existante ?
  if (IdxPolyAProlonger >= 0) then
  begin
    AfficherMessageErreur(Format('--> La nouvelle polyligne s''accrochera à la polyligne %d', [IdxPolyAProlonger]));
    QPoly := self.GetPolyline(IdxPolyAProlonger);   // On attrappe la polyligne à prolonger
    // on ajoute ses sommets au buffer
    for i := 0 to High(QPoly.Sommets) do
    begin
      MySommet := QPoly.Sommets[i];
      // calcul de la position absolue du sommet
      if (FBDDEntites.CalcCoordinatesFromBasePtAndOffset(MySommet.IDBaseStation, MySommet.Offset, QP)) then
         FBufferOfVertex.AddElement(QP);
    end;
    self.RemovePolyline(IdxPolyAProlonger);      // et on jette la polyligne à prolonger
  end;
  FBufferOfVertex.AddElement(P);
end;
procedure TCroquisTerrain.AddVertexAtBuffer(const P: TPoint3Df);
begin
  FBufferOfVertex.AddElement(P);
end;

procedure TCroquisTerrain.AddVertexAtBuffer(const QX, QY: double);
begin
  FBufferOfVertex.AddElement(MakeTPoint3Df(QX, QY, 0.00));
end;

procedure TCroquisTerrain.EndPolyline();
var
  MyPoly: TKrobardPolyligne;
begin
  AfficherMessageErreur(Format('%s.EndPolyline()', [self.ClassName]));
  if (MakePolyline(MyPoly)) then AddPolyline(MyPoly);
  AfficherMessageErreur(Format('%d polylignes', [GetNbPolylines()]));
end;



function TCroquisTerrain.MakePolyline(out MyPoly: TKrobardPolyligne): boolean;
var
  i, Nb, QIdx, QNbV, QL: Integer;
  BuffVertex   : TPoint3Df;
  QBaseStation : TBaseStation;
  QSimplifie   : TArrayPoints3Df;
  QDistance    : double;
  QEntrance: TEntrance;
  QIsStation: boolean;
  QE: TBDDEntitesFindViseeOrEntrance;
begin
  result := false;
  Nb := FBufferOfVertex.GetNbElements();
  AfficherMessageErreur(Format('%s.MakePolyline(): %d sommets', [ClassName, Nb]));
  if (Nb < 2) then Exit; // 1 seul point ? --> [ ]. Pas de dégénérés ici ! lol
  // simplification du tracé
  PolySimplifyInt2D(TOLERANCE_CORDE, QSimplifie);
  Nb := length(QSimplifie);
  if (Nb < 2) then Exit; // nouveau test pour éviter de sortir un dégénéré
  // bounding box
  MyPoly.BoundingBox := MakeTRect2Df(INFINI, INFINI, -INFINI, -INFINI);
  try  // et on construit l'objet de sortie
    MyPoly.IDStyle := FCurrentIdxStylePolyligne;
    SetLength(MyPoly.Sommets, Nb);
    for i := 0 to Nb - 1 do
    begin
      BuffVertex := QSimplifie[i];
      // bounding box
      MyPoly.BoundingBox.X1 := Min(Mypoly.BoundingBox.X1, BuffVertex.X);
      MyPoly.BoundingBox.Y1 := Min(Mypoly.BoundingBox.Y1, BuffVertex.Y);
      MyPoly.BoundingBox.X2 := Max(Mypoly.BoundingBox.X2, BuffVertex.X);
      MyPoly.BoundingBox.Y2 := Max(Mypoly.BoundingBox.Y2, BuffVertex.Y);
      if (FBDDEntites.GetStationOrEntranceFromXYZ(BuffVertex.X, BuffVertex.Y, 0.00, MAX_DISTANCE_CAPTURE, [tpVISEES], False, QIdx, QBaseStation, QEntrance, QDistance, QE)) then
      begin
        MyPoly.Sommets[i].IDBaseStation := MakeTIDBaseStation(QBaseStation.Entite_Serie, QBaseStation.Entite_Station, false);
        MyPoly.Sommets[i].Offset        := MakeTPoint3Df(BuffVertex.X - QBaseStation.PosStation.X,
                                                         BuffVertex.Y - QBaseStation.PosStation.Y,
                                                         QBaseStation.PosStation.Z);
      end;
    end;
    FBufferOfVertex.ClearListe(); // et on vide le buffer pour une autre utilisation
    result := True;
  except
  end;
end;

function TCroquisTerrain.GetNbStylesAnnotations(): integer;
begin
  result := FListeStylesAnnotations.GetNbElements();
end;

function TCroquisTerrain.GetStyleAnnotation(const Idx: integer): TTexteAttributs;
begin
  result := FListeStylesAnnotations.GetElement(Idx);
end;

function TCroquisTerrain.GetNbStylesPolyLines(): integer;
begin
  Result := FListeStylesPolylignes.GetNbElements();
end;

function TCroquisTerrain.GetStylePolyligne(const Idx: integer): TKrobardStylePolyligne;
begin
  result := FListeStylesPolylignes.GetElement(Idx);
end;

procedure TCroquisTerrain.AddAnnotation(const Annotation: TKrobardAnnotation);
var
  EWE: TKrobardAnnotation;
begin
  if (Length(Trim(Annotation.Texte)) = 0) then Exit; // texte vide -> []
  EWE := Annotation;
  FListeAnnotations.AddElement(Annotation);
end;

function TCroquisTerrain.GetAnnotation(const Idx: integer): TKrobardAnnotation;
begin
  Result := FListeAnnotations.GetElement(Idx);
end;

procedure TCroquisTerrain.PutAnnotation(const Idx: integer; const AAnnotation: TKrobardAnnotation);
begin
  FListeAnnotations.PutElement(Idx, AAnnotation);
end;

function TCroquisTerrain.RemoveAnnotation(const Idx: integer): boolean;
begin
  Result := FListeAnnotations.RemoveElement(Idx);
end;

function TCroquisTerrain.GetNbAnnotations(): integer;
begin
  Result := FListeAnnotations.GetNbElements();
end;

procedure TCroquisTerrain.AddPolyline(const Polyline: TKrobardPolyligne);
begin
  FListePolylignes.AddElement(Polyline);
end;

function TCroquisTerrain.GetPolyline(const Idx: integer): TKrobardPolyligne;
begin
  Result := FListePolylignes.GetElement(Idx);
end;

procedure TCroquisTerrain.PutPolyline(const Idx: integer; const APolyline: TKrobardPolyligne);
begin
  FListePolylignes.PutElement(Idx, APolyline);
end;

function TCroquisTerrain.RemovePolyline(const Idx: integer): boolean;
begin
  Result := FListePolylignes.RemoveElement(Idx);
end;

function TCroquisTerrain.GetNbPolylines(): integer;
begin
  Result := FListePolylignes.GetNbElements();
end;

function TCroquisTerrain.FindIdxPolylineWithExtr2NearToXY(const QX, QY: double; const RC: double): integer;
var
  i, s, Nb, QNamespace, QSt: Integer;
  MyPolyline     : TKrobardPolyligne;
  MySommet       : TKrobardPolyVertex;
  QSer           : TNumeroSerie;
  QIsAntenne, WU : boolean;
  EWE            : TBaseStation;
  QPX, QPY       : Double;
  SQRC, R2       : Double;
  QP             : TPoint3Df;
begin
  result := -1;
  Nb := self.GetNbPolylines();
  AfficherMessageErreur(Format('FindIdxPolylineWithExtr2NearToXY: %.2f, %.2f, %2f (%d polylines)', [QX, QY, RC, Nb]));
  if (Nb = 0) then exit;
  SQRC := RC ** 2; // pour éviter le calcul d'une racine carrée
  for i := Nb - 1 downto 0  do
  begin
    MyPolyline := self.GetPolyline(i);
    // attrapper le dernier sommet de la polyligne
    s := High(MyPolyline.Sommets);
    MySommet := MyPolyline.Sommets[s];
    if (FBDDEntites.CalcCoordinatesFromBasePtAndOffset(MySommet.IDBaseStation, MySommet.Offset, QP)) then
    begin
      R2 := sqr(QX - QP.X) + sqr(QY - QP.Y);
      if (R2 < SQRC) then
      begin
        AfficherMessageErreur(Format('%d: Xs: %.2f, Ys: %.2f, Px: %.2f, Py: %.2f - dx: %.2f, dy: %.2f', [i, QX, QY, QP.X, QP.Y, QX - QP.X, QY - QP.Y]));
        Exit(i);
      end;
    end;
  end;
end;

function TCroquisTerrain.GetNbBufferVertexes(): integer;
begin
  result := FBufferOfVertex.GetNbElements();
end;

function TCroquisTerrain.GetBufferVertex(const Idx: integer): TPoint3Df;
begin
  Result := FBufferOfVertex.GetElement(Idx);
end;

procedure TCroquisTerrain.SetCurrentIdxStylePolyligne(const S: TKrobardIDStylePolyligne);
begin
  FCurrentIdxStylePolyligne := S; // MOD (FListeStylesPolylignes.GetNbElements());
end;

procedure TCroquisTerrain.SetCurrentIdxStyleAnnotation(const S: integer);
begin
  FCurrentIdxStyleAnnotation := S; // MOD (FListeStylesAnnotations.GetNbElements());
end;

function TCroquisTerrain.FindNearestIdxPolyligneToXY(const P: TPoint2Df): integer;
var
  i, Nb, QIDNearest: Integer;
  DistCourante : double;
  NI: Int64;
  PC: TKrobardPolyligne;
  QPolylinesFound: TListOfIntegers;
  Idx1: integer;
  procedure ScanVertexArray(const QIdx: integer);
  var
    V  : TKrobardPolyVertex;
    a  : Integer;
    QSt: integer;
    q  : double;
    QSr: TNumeroSerie;
    EWE: TBaseStation;
    P0: TPoint3Df;
  begin
    for a := 0 to High(PC.Sommets) do
    begin
      V := PC.Sommets[a];
      FBDDEntites.CalcCoordinatesFromBasePtAndOffset(V.IDBaseStation, V.Offset, P0);
      q := Sqr(P0.X - P.X) + sqr(P0.Y - P.Y );
      if (q < DistCourante) then
      begin
        DistCourante := q;
        QIDNearest := QIdx;
      end;
    end;
  end;
begin
  Result       := -1;
  QIDNearest   := -1;
  Nb := GetNbPolylines();
  if (Nb = 0) then Exit;
  QPolylinesFound := TListOfIntegers.Create;
  try
    QPolylinesFound.ClearListe();   // liste contenant tous les objets dont la BBx contient P
    for i := 0 to Nb - 1 do
    begin
      PC := GetPolyline(i);
      if (PointInRectangle(P, PC.BoundingBox)) then QPolylinesFound.AddElement(i);
    end;
    Nb := QPolylinesFound.GetNbElements();
    if (Nb > 0) then
    begin
      DistCourante := INFINI;
      for i := 0 to Nb - 1 do
      begin
        Idx1 := QPolylinesFound.GetElement(i);
        AfficherMessageErreur(Format('-- %d:  Scan polyligne %d',[i, Idx1]));
        PC := GetPolyline(Idx1);
        ScanVertexArray(Idx1);
      end;
      Result := QIDNearest;
      QPolylinesFound.ClearListe();
    end;
  finally
    FreeAndNil(QPolylinesFound);//QPolylinesFound.Free;
  end;
end;

function TCroquisTerrain.TryDeleteAnnnotationNearToXY(const MyPos: TPoint2Df): boolean;
var
  Nb, QIdxNearest, i: Integer;
  QDist, RMin: Double;
  MyAnn: TKrobardAnnotation;
  PM: TPoint3Df;
begin
  result := false;
  QIdxNearest := -1;
  Nb := GetNbAnnotations();
  if (Nb = 0) then exit;
  RMin := INFINI;
  for i := 0 to Nb - 1 do
  begin
    MyAnn := GetAnnotation(i);
    if (FBDDEntites.CalcCoordinatesFromBasePtAndOffset(MyAnn.Position.IDBaseStation, MyAnn.Position.Offset, PM)) then
    begin
      QDist := (PM.X - MyPos.X) ** 2 + (PM.Y - MyPos.Y) ** 2;
      if (QDist < RMin) then
      begin
        RMin := QDist;
        QIdxNearest := i;
      end;
    end;
  end;
  if (QIdxNearest = -1) then Exit;
  if (Nb > 0) then FLastAnnotationDeleted := GetAnnotation(Nb - 1);
  RemoveAnnotation(QIdxNearest);
  result := true;
end;

function TCroquisTerrain.TryDeletePolylineNearToXY(const MyPos: TPoint2Df): boolean;
var
  QIdxNearest, Nb: Integer;
begin
  result := false;
  //AfficherMessage(Format('%s.TryDeletePolylineNearToXY(%.2f, %.2f)', [ClassName, MyPos.X, MyPos.Y]));
  QIdxNearest := FindNearestIdxPolyligneToXY(MyPos);
  //AfficherMessageErreur(Format('%s.TryDeletePolylineNearToXY(%.2f, %.2f) retourne %d', [ClassName, MyPos.X, MyPos.Y, QIdxNearest]));
  if (QIdxNearest >=0) then
  begin
    Nb := GetNbPolylines();
    if (Nb > 0) then FLastPolyLineDeleted := GetPolyline(Nb - 1);
    RemovePolyline(QIdxNearest);
    Result := True;
  end;
end;

end.
