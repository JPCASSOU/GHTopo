// Subroutines of CentreImpressionExt.PrintAPage
// Indispensable pour la version Linux
//==============================================
  //procedure DrawGaleries(const Filled: boolean);
  procedure DrawGaleries();
  var
    i: integer;
    E: TBaseStation;
    P: array[0..3] of TPoint;
    q1, q2: boolean;
  begin
    AfficherMessage('--> DrawGaleries()');
    Printer.Canvas.Pen.Width  := 0;
    q1 := (edFillGalerie in FVue2DParams.ongElementsDrawn);
    q2 := (edWalls       in FVue2DParams.ongElementsDrawn);
    for i := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do
    begin
      E := FBDDEntites.GetEntiteVisee(i);
      if (E.Type_Entite = tgENTRANCE)          then Continue;
      if (E.Type_Entite = tgVISEE_RADIANTE)    then Continue;

      if (Not E.Enabled) then Continue;    // prise en compte du MétaFiltre
      P[0] := Coord2Draft(E.PosOPG.X, E.PosOPG.Y);
      P[1] := Coord2Draft(E.PosOPD.X, E.PosOPD.Y);
      P[2] := Coord2Draft(E.PosPD.X, E.PosPD.Y);
      P[3] := Coord2Draft(E.PosPG.X, E.PosPG.Y);
      if (q1) then
      begin
        Printer.Canvas.Brush.Color := FBDDEntites.GetColorViseeFromModeRepresentation(FVue2DParams.ongModeRepresentation, E);
        Printer.Canvas.Pen.Color   := Printer.Canvas.Brush.Color;
        Printer.Canvas.Polygon(P);
      end;
      if (q2) then
      begin // parois
        Printer.Canvas.Pen.Color:=clBlack;
        Printer.Canvas.Line(P[0], P[3]);
        Printer.Canvas.Line(P[1], P[2]);
      end;
    end;
  end;
  procedure DrawPolygonals(const BlackLines: boolean);
  var
    i: integer;
    E: TBaseStation;
    P1, P2: TPoint;
  begin
    AfficherMessage('--> DrawPolygonals()');
    for I := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do
    begin
      E := FBDDEntites.GetEntiteVisee(I);
      if (E.Type_Entite = tgENTRANCE)       then Continue;
      if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;
      Printer.Canvas.Pen.Width := Millimetres2PixelsXf(0.35);//FVue2DParams.ongViseesLargeur;
      Printer.Canvas.Pen.Color := FBDDEntites.GetColorViseeFromModeRepresentation(FVue2DParams.ongModeRepresentation, E);
      if (BlackLines) then Printer.Canvas.Pen.Color := clBlack;
      //Pen.Color := E.ColorEntite;
      P1 := Coord2Draft(E.PosExtr0.X, E.PosExtr0.Y);
      P2 := Coord2Draft(E.PosStation.X, E.PosStation.Y);
      Printer.Canvas.Line(P1, P2);
    end;
  end;
  procedure DrawSections();
  var
    i: integer;
    E: TBaseStation;
    P1, P2: TPoint;
  begin
    AfficherMessage('--> DrawSections()');
    Printer.Canvas.Pen.Width := Millimetres2PixelsXf(0.10);
    for I:=0 to FBDDEntites.GetNbEntitesVisees() - 1 do
    begin
      E := FBDDEntites.GetEntiteVisee(I);
      if (E.Type_Entite = tgENTRANCE)       then Continue;
      if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;
      Printer.Canvas.Pen.Color := clGray;
      P1 := Coord2Draft(E.PosPG.X, E.PosPG.Y);
      P2 := Coord2Draft(E.PosPD.X, E.PosPD.Y);
      Printer.Canvas.Line(P1, P2);
    end;
  end;
  procedure DrawQuadrilles();
  var
    P1, P2: TPoint;
    C1, C2: TPoint2Df;
    Coin1, Coin2: TPoint3Df;
    t: integer;
    A,B, B0, qQuadrillageSpc: double;
    S: string;
  begin
    AfficherMessage('--> DrawQuadrilles()');
    Coin1 := FBDDEntites.GetCoinBasGauche;
    Coin2 := FBDDEntites.GetCoinHautDroit;
    qQuadrillageSpc := EnsureSetQuadrillageSpacing(FVue2DParams.ongQdrSpc);
    Printer.Canvas.Pen.Color := FVue2DParams.ongQdrColor;
    Printer.Canvas.Pen.Width := Millimetres2PixelsXf(0.10);
    case FVue2DParams.ongQdrType of
      qtNONE: ;
      qtGRID: begin
        t := trunc(Coin1.X / qQuadrillageSpc);
        A := FVue2DParams.ongQdrSpc * t;
        Printer.Canvas.Brush.Color:= clWhite;
        Printer.Canvas.Font.Color := clBlack;
        Printer.Canvas.Font.Name  := DEFAULT_FONT_NAME;
        Printer.Canvas.Font.Style := [fsItalic];
        Printer.Canvas.Font.Height := Millimetres2PixelsYf(3.00);
        while (A < Coin2.X) do
        begin
          C1 := MakeTPoint2Df(A, Coin1.Y);
          C2 := MakeTPoint2Df(A, Coin2.Y);
          P1 := Coord2Draft(C1.X, C1.Y);
          P2 := Coord2Draft(C2.X, C2.Y);
          Printer.Canvas.MoveTo(P1); Printer.Canvas.LineTo(P2);
          // cotation
          S:=Format(FORMAT_NB_REAL_0_DEC,[A]);
          Printer.Canvas.TextOut(P1.X - Printer.Canvas.TextWidth(S) div 2, P1.Y - 2, S);
          A := A + qQuadrillageSpc;
         end;
         t := trunc(Coin1.Y / qQuadrillageSpc);
         A := FVue2DParams.ongQdrSpc * t;
         while (A<Coin2.Y) do
         begin
           C1 := MakeTPoint2Df(Coin1.X, A);
           C2 := MakeTPoint2Df(Coin2.X, A);
           P1:=Coord2Draft(C1.X, C1.Y);
           P2:=Coord2Draft(C2.X, C2.Y);
           Printer.Canvas.MoveTo(P1); Printer.Canvas.LineTo(P2);
           S:=Format('%.0f ',[A]);
           Printer.Canvas.TextOut(P1.X + 2, P1.Y - Printer.Canvas.TextHeight(S) div 2, S);
           A:= A + qQuadrillageSpc;
         end;
        end;
      qtCROSS:
      begin
        t := trunc(Coin1.X / qQuadrillageSpc);
        A := qQuadrillageSpc * t;
        t := trunc(Coin1.Y / qQuadrillageSpc);
        B := qQuadrillageSpc * t;
        B0:= B;
        while (A < Coin2.X) do
        begin
         B:=B0;
         while (B < Coin2.Y) do
         begin
           C1 := MakeTPoint2Df(A - FQdrCrossSize, B);
           C2 := MakeTPoint2Df(A + FQdrCrossSize, B);
           P1 := Coord2Draft(C1.X, C1.Y);
           P2 := Coord2Draft(C2.X, C2.Y);
           Printer.Canvas.MoveTo(P1); Printer.Canvas.LineTo(P2);
           C1 := MakeTPoint2Df(A, B - FQdrCrossSize);
           C2 := MakeTPoint2Df(A, B + FQdrCrossSize);
           P1 := Coord2Draft(C1.X, C1.Y);
           P2 := Coord2Draft(C2.X, C2.Y);
           Printer.Canvas.MoveTo(P1); Printer.Canvas.LineTo(P2);
           B := B + qQuadrillageSpc;
         end;
         A := A + qQuadrillageSpc;
        end;
      end;
      qtPOINTS: ;
    end; // case QdrType
  end;
  // dessin du cadre de découpage
  procedure DrawCorners();
  var
    CornerSize: integer;
  begin
    AfficherMessage('--> DrawCorners()');

    CornerSize:= Printer.PageWidth div 20;
    Printer.Canvas.Pen.Color:=clBlue;
    Printer.Canvas.Pen.Width:=0;

    Printer.Canvas.MoveTo(0,CornerSize);
    Printer.Canvas.LineTo(0,0);
    Printer.Canvas.LineTo(CornerSize,0);

    Printer.Canvas.MoveTo(0, Printer.PageHeight-1-CornerSize);
    Printer.Canvas.LineTo(0, Printer.PageHeight-1);
    Printer.Canvas.Lineto(CornerSize,Printer.PageHeight-1);

    Printer.Canvas.MoveTo(Printer.PageWidth-1-CornerSize,0);
    Printer.Canvas.LineTo(Printer.PageWidth-1, 0);
    Printer.Canvas.LineTo(Printer.PageWidth-1,CornerSize);

    Printer.Canvas.MoveTo(Printer.PageWidth-1-CornerSize, Printer.PageHeight-1);
    Printer.Canvas.LineTo(Printer.PageWidth-1, Printer.PageHeight-1);
    Printer.Canvas.LineTo(Printer.PageWidth-1, Printer.PageHeight-1-CornerSize);

    // numéros de page
    Printer.Canvas.Font.Color  := clBlack;
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Style  := [];
    Printer.Canvas.Font.Size   := 8;
    Printer.Canvas.TextOut(10,10, Format('Row %d, Column %d - by GHTopo',[L+1, C+1]));

    Printer.Canvas.Font.Color  := clBlack;
    Printer.Canvas.Brush.Color := clWhite;
  end;


  procedure DrawEntrances(const DoDrawNames: boolean);
  var
    R666, R777 : integer;
    i, Nb: integer;
    E: TBaseStation;
    P2: TPoint;
    PM    : TPoint2Df;
    MyEntrance: TEntrance;
  begin
    AfficherMessage('--> DrawEntrances()');
    R666 := Millimetres2PixelsXf(0.80);
    R777 := Trunc(1.2 * R666);
    Nb := FBDDEntites.GetNbEntrances();
    if (Nb = 0) then exit;
    Printer.Canvas.Font.Height   := Millimetres2PixelsXf(FVue2DParams.ongTailleTexteEntrances);
    Printer.Canvas.Font.Style    := [fsBold];
    Printer.Canvas.Font.Color    := FVue2DParams.ongCouleurEntrances;
    for i := 0 to Nb - 1 do
    begin
      MyEntrance := FBDDEntites.GetEntrance(i);
      PM := MakeTPoint2Df(MyEntrance.eXEntree, MyEntrance.eYEntree);
      P2 := Coord2Draft(PM.X, PM.Y);
      Printer.Canvas.Pen.Width   := 0;
      Printer.Canvas.Brush.Color := clFuchsia;
      Printer.Canvas.EllipseC(P2.X, P2.Y, R666, R666);
      Printer.Canvas.Brush.Color := clWhite;
      if (DoDrawNames) then Printer.Canvas.TextOut(P2.X + R777, P2.Y + R777, MyEntrance.eNomEntree);
    end; // for
    Printer.Canvas.Font.Style  := [];
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlack;
  end;

  procedure DrawIDStations();
  var
    i: integer;
    E: TBaseStation;
    P1: TPoint;
  begin
    AfficherMessage('--> DrawIDStations()');
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := FVue2DParams.ongCouleurIDStation;
    Printer.Canvas.Font.Name   := DEFAULT_FONT_NAME;
    Printer.Canvas.Font.Height := Millimetres2PixelsYf(FVue2DParams.ongTailleTexteIDStation);
    Printer.Canvas.Font.Style  := [];
    for I:=0 to FBDDEntites.GetNbEntitesVisees() - 1 do
    begin
      E := FBDDEntites.GetEntiteVisee(I);
      if (E.Type_Entite = tgENTRANCE)       then Continue;
      if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;
      Printer.Canvas.Pen.Color := clGray;
      P1 := Coord2Draft(E.PosStation.X, E.PosStation.Y);
      Printer.Canvas.TextOut(P1.X+2+RD, P1.Y+2+RD, E.toString());
    end;
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlack;
  end;
  procedure DrawStations();
  var
    i: integer;
    E: TBaseStation;
    P1: TPoint;
  begin
    AfficherMessage('--> DrawStations()');
    Printer.Canvas.Pen.Color   := clRed;
    Printer.Canvas.Brush.Color := Printer.Canvas.Pen.Color;
    for i := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do begin
      E := FBDDEntites.GetEntiteVisee(i);
      // TODO: Revoir cette zone
      if (E.Type_Entite = tgENTRANCE)       then Continue;
      if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;
      P1 := Coord2Draft(E.PosStation.X, E.PosStation.Y);
      Printer.Canvas.EllipseC(P1.X, P1.Y, RD, RD);
    end;
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlack;
  end;

  // dessin du cartouche et du cadre périmétrique
  procedure DrawCartouche();
  const
    MG = 2.0; // marge 20 m
  var
    P1, P2: TPoint; //, P3, P4: TPoint;
  begin
    AfficherMessage('--> DrawCartouche()');
    P1 := Coord2Draft(FRXMini + MG, FRYMini + MG);
    P2 := Coord2Draft(FRXMaxi - MG, FRYMaxi - MG);
    //P3 := Coord2Draft(FTableEntites.cnMaxi.X, FTableEntites.cnMaxi.Y);
    //P4 := Coord2Draft(FTableEntites.cnMini.X, FTableEntites.cnMaxi.Y);
    Printer.Canvas.Pen.Color  := clBlack;
    Printer.Canvas.Pen.Width  := Millimetres2PixelsXf(0.45);
      //Brush.Color:=Pen.Color;
    Printer.Canvas.MoveTo(P1.X, P1.Y); // ne pas utiliser MakeTPoint
    Printer.Canvas.LineTo(P2.X, P1.Y);
    Printer.Canvas.LineTo(P2.X, P2.Y);
    Printer.Canvas.LineTo(P1.X, P2.Y);
    Printer.Canvas.LineTo(P1.X, P1.Y);
    Printer.Canvas.Pen.Width  := 0;

    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlack;
  end;
  // cotation
  procedure DrawCotationStations(const DoDeltaCoords: boolean);
  const
    PROPORTIONAL_FONT_HEIGHT = 0.005;
  var
    i: integer;
    E: TBaseStation;
    P1: TPoint;
    QQ: TPoint3Df;
    WU: Double;
  begin
    AfficherMessage('--> DrawCotationStations()');
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Name   := DEFAULT_FONT_NAME;
    Printer.Canvas.Font.Color  := FVue2DParams.ongCouleurCotation;
    Printer.Canvas.Font.Height := Millimetres2PixelsYf(FVue2DParams.ongTailleTexteCotation);
    // hauteur de texte proportionnelle à la taille de la page
    // Printer.Canvas.Font.Height := round(min(Printer.PageHeight, Printer.PageWidth) * PROPORTIONAL_FONT_HEIGHT);
    Printer.Canvas.Font.Style  := [];
    for i := 0 to FBDDEntites.GetNbEntitesVisees() - 1 do
    begin
      E := FBDDEntites.GetEntiteVisee(I);
      QQ := FBDDEntites.GetDeltaXYZFromPositionSt0(E);
      if (E.Type_Entite = tgENTRANCE)       then Continue;
      if (E.Type_Entite = tgVISEE_RADIANTE) then Continue;

      Printer.Canvas.Pen.Color := clGray;
      P1 := Coord2Draft(E.PosStation.X, E.PosStation.Y);
      WU := IIF(DoDeltaCoords, QQ.Z, E.PosStation.Z);

      Printer.Canvas.TextOut(P1.X+2+RD, P1.Y+2-RD, Format('%f',[WU]));
    end;
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlack;
  end;
   // tracé des jonctions
  procedure DrawJonctions();
  var
    QNbJonctions, i: Integer;
    MyJonction: TJonctionXYZ;
    P1: TPoint;
  begin
    AfficherMessage('--> DrawJonctions()');
    QNbJonctions := FBDDEntites.GetNbJonctions();
    if (0 = QNbJonctions) then Exit;
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlue;
    Printer.Canvas.Font.Name   := DEFAULT_FONT_NAME;
    Printer.Canvas.Font.Color  := FVue2DParams.ongCouleurNoeuds;
    Printer.Canvas.Font.Height := Millimetres2PixelsYf(FVue2DParams.ongTailleTexteNoeuds);
    Printer.Canvas.Font.Style  := [fsBold];

    for i := 0 to QNbJonctions do
    begin
      MyJonction := FBDDEntites.GetJonction(i);
      P1 := Coord2Draft(MyJonction.X, MyJonction.Y);
      Printer.Canvas.TextOut(P1.x + 2, P1.Y + 2, MyJonction.ToString());
    end;
  end;
  // visées radiantes
  procedure DrawViseesRadiantes();
  var
    i, n: integer;
    E: TBaseStation;
    P1, P2: TPoint;
  begin
    n := FBDDEntites.GetNbEntitesAntennes();
    AfficherMessage(Format('--> DrawViseesRadiantes(): %d', [n]));
    if (n = 0) then exit;
    Printer.Canvas.Pen.Color := clGray;
    Printer.Canvas.pen.Width := Millimetres2PixelsXf(0.010);
    for i := 0 to n - 1 do
    begin
      E := FBDDEntites.GetEntiteAntenne(i);
      P1 := Coord2Draft(E.PosExtr0.X, E.PosExtr0.Y);
      P2 := Coord2Draft(E.PosStation.X, E.PosStation.Y);
      Printer.Canvas.Line(P1, P2);
    end;
  end;
  // dessin de la règle
  procedure DrawRegle(const TailleRegle: double);
  const
    NB_CARREAUX_X = 8;
  var
    i : Integer;
    Mg: double;
    QXo, QYo, QY1: double;
    LargeurCarreau, HauteurCarreau: double;
    procedure QDrawRect(const X, Y, L, H: double; const LC, FC: TColor);
    var
      QP1, QP2: TPoint;
    begin
      QP1 := Coord2Draft(X, Y);
      QP2 := Coord2Draft(X+L, Y+H);
      Printer.Canvas.Pen.Width    := 0;
      Printer.Canvas.Pen.Color    := LC;
      Printer.Canvas.Brush.Color  := FC;
      Printer.Canvas.Rectangle(QP1.x, QP1.y, QP2.x, QP2.y);
    end;
    procedure QDrawTextLegende(const X, Y, L, H: double);
    var
      HauteurTexte: integer;
      QP1, QP2: TPoint;
      WU : string;
      EWE: TSize;
    begin
      Printer.Canvas.Brush.Color:= clWhite;
      Printer.Canvas.Font.Style := [fsBold];
      Printer.Canvas.Font.Color := clBlue;
      WU := Format('%.0f m',[L]);
      QP1   := Coord2Draft(X, Y);
      QP2   := Coord2Draft(X, Y + H);
      HauteurTexte := QP2.Y - QP1.Y;
      Printer.Canvas.Font.Height  := HauteurTexte;
      EWE := Printer.Canvas.TextExtent('0');
      Printer.Canvas.TextOut(QP1.X, QP1.Y - EWE.cy, '0');
      QP1   := Coord2Draft(X + L, Y);
      EWE   := Printer.Canvas.TextExtent(WU);
      QP1.X := QP1.X - EWE.cx;
      Printer.Canvas.TextOut(QP1.X, QP1.Y - EWE.Cy, WU);
    end;
  begin
    Mg := (FRXMaxi - FRXMini) / 50; // marge
    QXo := FRXMini + Mg;
    QYo := FRYMini + Mg;
    LargeurCarreau := TailleRegle / NB_CARREAUX_X;
    HauteurCarreau := TailleRegle / 25;
    AfficherMessage(Format(' --> DrawRegle: L = %.0f m at (%.0f, %.0f)', [TailleRegle, FRXMini + Mg, FRYMini + Mg]));
    // cadre périmétrique
    QDrawRect(QXo, QYo, TailleRegle, 2 * HauteurCarreau, clBlack, clWhite);
    // carreaux alternés
    for i := 0 to NB_CARREAUX_X - 1 do
    begin
      if (Odd(i)) then QY1 := QYo + HauteurCarreau else QY1 := QYo;
      QDrawRect(QXo + (i * LargeurCarreau), QY1, LargeurCarreau, HauteurCarreau, clBlack, clGray);
    end;
    // légende
    QDrawTextLegende(QXo, QYo + 2 * HauteurCarreau + Mg/4, TailleRegle, HauteurCarreau);
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlack;
  end;


  // tracé du croquis
  procedure DrawCroquis();
  var
    i, NbPolys, NbAnnotations: Integer;
    procedure QTracerPolyline(const P: TKrobardPolyligne);
    var
      MyStyle: TKrobardStylePolyligne;
      s, NbV: Integer;
      PV: TKrobardPolyVertex;
      PM: TPoint3Df;
      QPolygon: array of TPoint;
    begin
      NbV := Length(P.Sommets);
      if (NbV < 2) then Exit;
      //if (not IntersectRectangles(MyPoly.BoundingBox, MyRectVue)) then Exit;
      SetLength(QPolygon, NbV);

      MyStyle := FCroquisTerrain.GetStylePolyligne(P.IDStyle);
      Printer.Canvas.Brush.Color := MyStyle.FillColor;
      if (Mystyle.Filled) then Printer.Canvas.Brush.Style := bsSolid
                          else Printer.Canvas.Brush.Style := bsClear;
      Printer.Canvas.Pen.Color  := MyStyle.LineColor;
      Printer.Canvas.Pen.Style  := psSolid;//MyStyle.LineStyle;
      //Printer.Canvas.Pen.Style  := MyStyle.LineStyle;
      Printer.Canvas.Pen.Width  := Millimetres2PixelsXf(MyStyle.LineWidth / 3.0);
      for s := 0 to NbV - 1 do
      begin
        PV := P.Sommets[s];
        FBDDEntites.CalcCoordinatesFromBasePtAndOffset(PV.IDBaseStation, PV.Offset, PM);
        QPolygon[s] := Coord2Draft(PM.X, PM.Y);
        //AfficherMessageErreur(Format('%d: %f %f -> %d %d', [s, PM.X, PM.Y, QPolygon[s].x, QPolygon[s].y]));
      end;
      if (MyStyle.Closed) then Printer.Canvas.Polygon(QPolygon)
                          else Printer.Canvas.Polyline(QPolygon);

    end;
    procedure QTracerAnnotation(const A: TKrobardAnnotation);
    var
      MyStyle: TTexteAttributs;
      QP: TPoint;
      EWE: TPoint3Df;
      // TODO: A factoriser
      function InterpreterAnnotation(): string;
      var
        pz: SizeInt;

        aSerie, aStation: TIDBaseStation;
      begin
        Result   := A.Texte;
        aSerie   := A.Position.IDBaseStation div NB_MAXI_SERIES_PAR_CAVITE;
        aStation := (A.Position.IDBaseStation mod NB_MAXI_SERIES_PAR_CAVITE) div 10;

        FBDDEntites.CalcCoordinatesFromBasePtAndOffset(A.Position.IDBaseStation, MakeTPoint3Df(0.00, 0.00, 0.00), EWE);
        Result := StringReplace(Result, '%z', Format('Alt %.2f', [EWE.Z]), [rfIgnoreCase, rfReplaceAll]);
        Result := StringReplace(Result, FORMAT_STRING, Format(FMTSERST, [aSerie, aStation]), [rfIgnoreCase, rfReplaceAll]);
      end;
    begin
      MyStyle := FCroquisTerrain.GetStyleAnnotation(A.IDStyle);
      Printer.Canvas.Brush.Color:= MyStyle.BackColor;
      Printer.Canvas.Font.Style := MyStyle.FontStyle;
      Printer.Canvas.Font.Color := MyStyle.FontColor;
      Printer.Canvas.Font.Size  := Millimetres2PixelsXf(3.50);
      if (FBDDEntites.CalcCoordinatesFromBasePtAndOffset(A.Position.IDBaseStation, A.Position.Offset, EWE)) then
      begin
        QP   := Coord2Draft(EWE.X, EWE.Y);
        Printer.Canvas.TextOut(QP.X + 4, QP.Y - 4, InterpreterAnnotation());
      end;
    end;
  begin
    Printer.Canvas.Brush.Color := clWhite;
    NbPolys   := FCroquisTerrain.GetNbPolylines();
    NbAnnotations  := FCroquisTerrain.GetNbAnnotations();
    AfficherMessage(Format(' --> DrawCroquis: %d polys, %d textes', [NbPolys, NbAnnotations]));
    if (NbPolys > 0) then
    begin
      for i := 0 to NbPolys - 1 do QTracerPolyline(FCroquisTerrain.GetPolyline(i));
    end;
    if (NbAnnotations > 0) then
    begin
      for i := 0 to NbAnnotations - 1 do QTracerAnnotation(FCroquisTerrain.GetAnnotation(i));
    end;
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlack;
  end;
  procedure DrawPOIs();
  const QRayonInMM = 1.5;
  var
    i, NbStations, QRayonX, QRayonY: Integer;
    MyStation: TBaseStation;
    PP: TPoint;
    EWE: String;
  begin
    QRayonX := Millimetres2PixelsXf(QRayonInMM);
    QRayonY := Millimetres2PixelsYf(QRayonInMM);
    NbStations := FBDDEntites.GetNbEntitesVisees();
    Printer.Canvas.Pen.Color   := clRed;
    Printer.Canvas.Brush.Color := clAqua;
    Printer.Canvas.pen.Width   := Millimetres2PixelsXf(0.015);
    Printer.Canvas.Font.Height := Millimetres2PixelsYf(FVue2DParams.ongTailleTextePOIs);
    Printer.Canvas.Font.Color  := FVue2DParams.ongCouleurPOIs;

    for i := 0 to NbStations - 1 do
    begin
      MyStation := FBDDEntites.GetEntiteVisee(i);
      EWE := StringReplace(MyStation.oCommentaires, '$POI:', '', [rfIgnoreCase, rfReplaceAll]);
      if (MyStation.IsPOI) then
      begin
        PP := Coord2Draft(MyStation.PosStation.X, MyStation.PosStation.Y);
        Printer.Canvas.Brush.Color := clAqua;
        Printer.Canvas.EllipseC(PP.X, PP.Y, QRayonX, QRayonY);
        Printer.Canvas.Brush.Color := clWhite;
        Printer.Canvas.TextOut(PP.X + 2, PP.Y + 2, Format('%d.%d: %s', [MyStation.Entite_Serie, MyStation.Entite_Station, EWE]));

      end;
    end;
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.Font.Color  := clBlack;
  end;
