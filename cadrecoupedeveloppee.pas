// Editeur de coupes développées
// 05/05/2016: Réacculturation - Les listes simples passent aux génériques
unit CadreCoupeDeveloppee;

{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  unitobjetserie,
  Common,
  Math, types,
  Graphics,
  BGRACanvas, BGRABitmap,
  unitUtilsComposants,
  unitCoupeDeveloppee,
  //UnitBranchesCoupeDeveloppee,
  Dialogs,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TCdrCoupeDeveloppee }

  TCdrCoupeDeveloppee = class(TFrame)
    Button1: TButton;
    lbGCSMouse: TStaticText;
    lbInfos: TStaticText;
    lbModeTravail: TStaticText;
    Panel1: TPanel;
    Vue: TPaintBox;
    procedure VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VuePaint(Sender: TObject);
  private
    { private declarations }
    FDoDrawCoupe: boolean;
    FCoupeDeveloppeeParams: TCoupeDeveloppeeParams;
    FCoupeDeveloppee : TCoupeDeveloppee;
    FModesTravail    : TModesTravail;
    FMyPos           : TPointCoupeDeveloppee;      // coordonnées souris
    // gestion des zooms
    FZC1, FZC2       :   TPointCoupeDeveloppee;
    FZP1, FZP2       :   TPoint;
    FZoomC1OK        : Boolean;
    FRegionPMini     :double;
    FRegionPMaxi     :double;
    FRegionZMini     :double;
    FRegionZMaxi     :double;
    // paramètres de vues
    FRappHLVue       : double;
    FRappScrReal     : double;
    FInvRappScrReal  : double;
    // le numéro de branche courante
    FCurrentNoBranche   : integer;
    FCurrentNoSerie     : integer;
    FCurrentNoStation   : integer;
    function GetCoordsMonde(const PP: TPoint): TPointCoupeDeveloppee;
    function GetCoordsPlan(const PM: TPointCoupeDeveloppee): TPoint;
    function GetRZMaxi(): double;
    procedure DeplacerVueInt(const DepX, DepY: integer);
    procedure DeplacerVue(const DepX, DepY: double);
  public
    { public declarations }
    function Initialise(const T: TCoupeDeveloppee): boolean;
    procedure Finalise();
    procedure ResetVue();
    procedure setDoDrawCoupe(const B: boolean);
    function  getDoDrawCoupe(): Boolean;
    procedure SetModeTravail(const MT: TModesTravail);
    function  GetCurrentIdxBranche(): integer;
    function  GetCurrentNumSerie(): integer;
    function  GetCurrentNumStation(): integer;
    function  GetCoinBasGauche(): TPointCoupeDeveloppee;
    function  GetCoinHautDroit(): TPointCoupeDeveloppee;
    procedure SetViewLimits(const P1, Z1, P2, Z2: double);
    procedure RedessinEcran();
    procedure CentrerSurPZ(const QP, QZ: double);
    function  GetParamsVueCoupe(): TCoupeDeveloppeeParams;
    procedure SetParamsVueCoupe(const E: TCoupeDeveloppeeParams);
  end;

implementation

{$R *.lfm}

{ TCdrCoupeDeveloppee }

procedure TCdrCoupeDeveloppee.ResetVue();
var
  cnMini, cnMaxi: TPointCoupeDeveloppee;
  M, dp, dz          : double;
  EtendueP           : double;
  ViewCoinBasGauche  : TPointCoupeDeveloppee;
  ViewCoinHautDroit  : TPointCoupeDeveloppee;
  AspectRatio        : double;
  ratioP             : double;
begin
  try
    AfficherMessage(Format('%s.ResetVue(%d series)', [ClassName, FCoupeDeveloppee.GetNbSeries]));
    //FCoupeDeveloppee.r;
    cnMini := FCoupeDeveloppee.GetCoinBasGauche;
    cnMaxi := FCoupeDeveloppee.GetCoinHautDroit;
    SetViewLimits(cnMini.P, cnMini.Z, cnMaxi.P, cnMaxi.Z);
    FModesTravail    := mtREADY;
    FDoDrawCoupe     := true;
    M := 5.00; //0.01 * Hypot2D(DX, DY);
    // marge périmétrique
    cnMini.P -= M;
    cnMini.Z -= M;
    cnMaxi.P += M;
    cnMaxi.Z += M;
    EtendueP := cnMaxi.P - cnMini.P;
    //EtendueZ := cnMaxi.Z - cnMini.Z;
    AfficherMessage(Format('-- De (%.2f, %.2f) a (%.2f, %.2f)', [cnMini.P, cnMini.Z, cnMaxi.P, cnMaxi.Z]));

    // attraper les paramètres de la vue
    AfficherMessage(Format('w=%d, h=%d', [Panel1.Width, Panel1.Height]));
    ViewCoinBasGauche := GetCoordsMonde(MakeTPoint(0, 0));
    ViewCoinHautDroit := GetCoordsMonde(MakeTPoint(Panel1.Width, Panel1.Height));

    AfficherMessage(Format('-- Vue courante: %.2f, %.2f -> %.2f, %.2f',
                           [ViewCoinBasGauche.P, ViewCoinBasGauche.Z,
                            ViewCoinHautDroit.P, ViewCoinHautDroit.Z]));

    // première passe: ajustement à la largeur
    AspectRatio := Panel1.Height / Panel1.Width;
    dp := ViewCoinHautDroit.P - ViewCoinBasGauche.P;
    dz := ViewCoinHautDroit.Z - ViewCoinBasGauche.Z;

    ratioP   := EtendueP / dP;

    cnMaxi.P := cnMini.P + ratioP * dp;
    cnMaxi.Z := cnMini.Z + EtendueP * AspectRatio;

    SetViewLimits(cnMini.P, cnMini.Z, cnMaxi.P, cnMaxi.Z);
    AfficherMessage(Format('-- Limites de vue: %.2f, %.2f -> %.2f, %.2f',
                           [FRegionPMini, FRegionZMini,
                            FRegionPMaxi, FRegionZMaxi]));
    Vue.Invalidate;
  except
  end;
end;

procedure TCdrCoupeDeveloppee.setDoDrawCoupe(const B: boolean);
begin
  FDoDrawCoupe:=B;
end;
function TCdrCoupeDeveloppee.getDoDrawCoupe(): Boolean;
begin
  Result := FDoDrawCoupe;
end;

procedure TCdrCoupeDeveloppee.VuePaint(Sender: TObject);
begin
  try
    RedessinEcran;

  except
  end;
end;

procedure TCdrCoupeDeveloppee.VueMouseMove(Sender: TObject; Shift: TShiftState;  X, Y: Integer);
  procedure DrawLigne(const P1, P2: TPoint); inline;
  begin
    Vue.Canvas.Line(P1.X,P1.Y, P2.X, P2.Y);
  end;
  procedure DrawRectangle(const P1, P2: TPoint);
  begin
    Vue.Canvas.Brush.Style := bsClear;
    Vue.Canvas.Rectangle(P1.X, P1.Y, P2.X, P2.Y);
  end;
  procedure DrawShapeElastique(const QX, QY: integer; const SType: byte);
  begin
    case SType of
      1: DrawLigne(FZP1, FZP2);
      2: DrawRectangle(FZP1, FZP2);
    end;
    FZP2 := MakeTPoint(QX, QY);
    case SType of
      1: DrawLigne(FZP1, FZP2);
      2: DrawRectangle(FZP1, FZP2);
    end;
  end;
begin
  if (not FDoDrawCoupe) then Exit;
  FMyPos   := GetCoordsMonde(MakeTPoint(X, Y));
  lbGCSMouse.Caption:=Format('%.2f, %.2f',[FMyPos.P, FMyPos.Z]);
   case FModesTravail of
    mtREADY:
      begin
        FZP1 := MakeTPoint(X, Y);
        FZP2 := MakeTPoint(X, Y);
      end;
    mtPAN_SECOND_POINT:
      begin
        //if (Not FAttenduSecondPoint) then Exit;
        DrawShapeElastique(X, Y, 1);
      end;
    mtDISTANCE_SECOND_POINT:
      begin
        //if (Not FAttenduSecondPoint) then Exit;
        DrawShapeElastique(X, Y, 1);
      end;
    mtZOOM_SECOND_COIN:
      begin
        //if (Not FAttenduSecondPoint) then Exit;
        DrawShapeElastique(X, Y, 2);
      end;
  else
    ;
  end;
end;



procedure TCdrCoupeDeveloppee.VueMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  QAT: Integer;
  QSerie: TObjSerie;
  QOffset: TPointCoupeDeveloppee;
  procedure ReinitFZC(const DoFC1, DoFC2: boolean);
  begin
    if (DoFC1) then
    begin
      FZC1 := FMyPos;
      FZP1 := MakeTPoint(X, Y);
    end;
    if (DoFC2) then
    begin
      FZC2 := FMyPos;
      FZP2 := MakeTPoint(X, Y);
    end;
  end;
begin
  if (not FDoDrawCoupe) then Exit;
  case FModesTravail of
    mtREADY:
      begin
        QAT := FCoupeDeveloppee.FindIndexBrancheNearToXY(FMyPos.P, FMyPos.Z, FCurrentNoSerie, FCurrentNoStation);
        if (QAT >= 0) then
        begin
          FCurrentNoBranche := QAT;
          if (FCoupeDeveloppee.GetSerieByNumeroSerie(FCurrentNoSerie, QSerie)) then
          begin
            lbInfos.Caption := Format('Branche: %d - Station topo: %d.%d -  %s', [
                                      FCurrentNoBranche,
                                      FCurrentNoSerie, FCurrentNoStation,
                                      QSerie.GetNomSerie]);
          end;
        end;
      end;
    mtPAN_PREMIER_POINT:
      begin
        ReinitFZC(True, True);
        FZC1 := FMyPos;
        FZP1 := MakeTPoint(X, Y);
        SetModeTravail(mtPAN_SECOND_POINT);
      end;
    mtPAN_SECOND_POINT:
      begin
        ReinitFZC(False, True);
        QOffset.P := FZC2.P - FZC1.P;
        QOffset.Z := FZC2.Z - FZC1.Z;
        DeplacerVue(QOffset.P, QOffset.Z);
        Vue.Invalidate;
        SetModeTravail(mtREADY);
      end;
    mtZOOM_PREMIER_COIN:
      begin
        ReinitFZC(True, True);
        SetModeTravail(mtZOOM_SECOND_COIN);
      end;

    mtZOOM_SECOND_COIN:
      begin
        ReinitFZC(False, True);
        SetViewLimits(FZC1.P, FZC1.Z, FZC2.P, FZC2.Z);
        Vue.Invalidate;
        SetModeTravail(mtREADY);
      end;
  end;
end;


procedure TCdrCoupeDeveloppee.SetViewLimits(const P1, Z1, P2, Z2: double);
const
  Epsilon = 1e-2;
var
  qP1, qP2, qZ1, qZ2: double;
  d1, d2: double;
begin
  AfficherMessage(Format('%s.SetViewLimits(%.2f, %.2f, %.2f, %.2f', [ClassName, P1, Z1, P2, Z2]));
  qP1 := P1;
  qZ1 := Z1;
  qP2 := P2;
  qZ2 := Z2;

  d1 := qP2 - qP1;
  d2 := qZ2 - qZ1;
  // si zone trop étroite, abandonner
  if (Abs(d1) < Epsilon) or (Abs(d2) < Epsilon) then  Exit;
  // échanger de manière à rendre indifférent le sens du rectangle de sélection
  if (qP2 < qP1) then Swap(qP1, qP2);
  if (qZ2 < qZ1) then Swap(qZ1, qZ2);

  FRegionPMini := qP1;
  FRegionPMaxi := qP2;
  FRegionZMini := qZ1;
  FRegionZMaxi := qZ2;
  //*)
  // Redéfinition de la hauteur maxi
  FRegionZMaxi := GetRZMaxi;
  FRegionZMaxi := qZ2;
end;

function TCdrCoupeDeveloppee.GetCoordsMonde(const PP: TPoint): TPointCoupeDeveloppee;
begin
  Result.P :=  FInvRappScrReal * PP.X + FRegionPMini;
  Result.Z := -FInvRappScrReal * PP.Y + FRegionZMaxi;
end;
function TCdrCoupeDeveloppee.GetCoordsPlan(const PM: TPointCoupeDeveloppee): TPoint;
begin
  Result.X := Round((PM.P - FRegionPMini) * FRappScrReal);
  Result.Y := Round((FRegionZMaxi - PM.Z) * FRappScrReal);
end;
// cette fonction retourne d'autres paramètres
function TCdrCoupeDeveloppee.GetRZMaxi(): double;
begin
  // calcul du rapport Hauteur/largeur de vue
  FRappHLVue := Vue.Height / Vue.Width;
  // calcul du rapport Ecran/Réel
  FRappScrReal := Vue.Width / (FRegionPMaxi - FRegionPMini);
  FInvRappScrReal := 1 / FRappScrReal;
  // calcul de la hauteur de visualisation
  Result := FRegionZMini + (FRegionPMaxi - FRegionPMini) * FRappHLVue;
end;
// REDESSIN DE L'ECRAN
procedure TCdrCoupeDeveloppee.RedessinEcran();
var
  R: TRect;
  TmpBuffer: TBGRABitmap;
  //procedure Drawli
  //----------------------------------------------------------------------------
  procedure DrawNodes;
  var
    i, Nb: Integer;
    JC: TJonctionCoupeDeveloppee;
    PM: TPointCoupeDeveloppee;
    P1: TPoint;
  begin
    Nb := FCoupeDeveloppee.GetNbJonctions;
    AfficherMessage(Format('---> Dessin des %d noeuds', [Nb]));
    if (Nb > 0) then
    begin
      with TmpBuffer.CanvasBGRA do
      begin
        Pen.Color   := clRed;
        Brush.Color := clYellow;
        for i := 0 to Nb - 1 do
        begin
          JC := FCoupeDeveloppee.GetJonction(i);
          PM.setFrom(JC.Abscisse, JC.Cote);
          P1 := GetCoordsPlan(PM);
          EllipseC(P1.X, P1.Y, 4, 4);
        end;
      end;
    end;
  end;
  //----------------------------------------------------------------------------
  procedure DrawBranches;
  var
    i, Nb     : Integer;
    myBranche : TBrancheCoupeDeveloppeeAsRecord;
    //  centerlines
    procedure DrawCenterLines(const Brch: TBrancheCoupeDeveloppeeAsRecord);
    var
      JC1       : TJonctionCoupeDeveloppee;
      v, NbV    : integer;
      myVisee   : TUneVisee;
      uP, uZ    : double;
      PP        : TPoint;
      PM        : TPointCoupeDeveloppee;
    begin
      JC1 := FCoupeDeveloppee.GetJonction(Brch.NumeroNoeudDepart);
      //JC2 := FCoupeDeveloppee.GetJonction(Brch.NumeroNoeudArrivee);
      with TmpBuffer.CanvasBGRA do
      begin
        Pen.Color   := clBlue;
        Pen.Width   := FCoupeDeveloppeeParams.ViseesLargeur;
        PM.setFrom(JC1.Abscisse, JC1.Cote);
        PP := GetCoordsPlan(PM);
        MoveTo(PP);
        //NbV := myBranche.GetNbVisees;
        NbV := 1 + High(myBranche.ArrVisees);
        if (NbV > 0) then
        begin
          uP := 0.00;
          uZ := 0.00;
          for v := 0 to NbV - 1 do
          begin
            myVisee := myBranche.ArrVisees[v]; //myVisee := myBranche.GetVisee(v);
            //uP += myVisee.DeltaP * IIF((myBranche.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
            //uZ += myVisee.DeltaZ;
            uP += myVisee.AccroissP * IIF((myBranche.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
            uZ += myVisee.AccroissXYZ.Z;

            PM.setFrom(JC1.Abscisse + uP, JC1.Cote + uZ);
            PP := GetCoordsPlan(PM);
            LineTo(PP);
          end;
        end;
      end;
    end;
    // parois
    procedure DrawParois(const Brch: TBrancheCoupeDeveloppeeAsRecord; const IsFloor: boolean);
    var
      JC1         : TJonctionCoupeDeveloppee;
      v, NbV      : integer;
      myVisee     : TUneVisee;
      uP, uZ, uH  : double;
      PP          : TPoint;
      PM          : TPointCoupeDeveloppee;
    begin
      JC1 := FCoupeDeveloppee.GetJonction(Brch.NumeroNoeudDepart);
      //JC2 := FCoupeDeveloppee.GetJonction(Brch.NumeroNoeudArrivee);
      with TmpBuffer.CanvasBGRA do
      begin
        Pen.Color   := clRed;
        pen.Width   := 2;
        PM.setFrom(JC1.Abscisse, JC1.Cote);
        PP := GetCoordsPlan(PM);
        MoveTo(PP);
        NbV := 1 + High(myBranche.ArrVisees); // NbV := myBranche.GetNbVisees;
        if (NbV > 0) then
        begin
          uP := 0.00;
          uZ := 0.00;
          for v := 0 to NbV - 1 do
          begin
            myVisee := myBranche.ArrVisees[v]; //myVisee := myBranche.GetVisee(v);
            //uP += myVisee.DeltaP * IIF((myBranche.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
            //uZ += myVisee.DeltaZ;

            uP += myVisee.AccroissP * IIF((myBranche.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
            uZ += myVisee.AccroissXYZ.Z;

            uH := IIF(IsFloor, uZ - myVisee.HN, uZ + myVisee.HZ);

            PM.setFrom(JC1.Abscisse + uP, JC1.Cote + uH);
            PP := GetCoordsPlan(PM);
            LineTo(PP);
          end;
        end;
      end;
    end;
    // sections
    procedure DrawSections(const Brch: TBrancheCoupeDeveloppeeAsRecord);
    var
      JC1           : TJonctionCoupeDeveloppee;
      v, NbV        : integer;
      myVisee       : TUneVisee;
      uP, uZ, uH, uN: double;
      PP            : TPoint;
      PM            : TPointCoupeDeveloppee;
    begin
      with TmpBuffer.CanvasBGRA do
      begin
        JC1 := FCoupeDeveloppee.GetJonction(Brch.NumeroNoeudDepart);
        //JC2 := FCoupeDeveloppee.GetJonction(Brch.NumeroNoeudArrivee);
        Pen.Color   := clSilver;
        Pen.Width   := 0;
        NbV := 1 + High(myBranche.ArrVisees); // NbV := myBranche.GetNbVisees;
        if (NbV > 0) then
        begin
          uP := 0.00;
          uZ := 0.00;
          for v := 0 to NbV - 1 do
          begin
            myVisee := myBranche.ArrVisees[v]; //myVisee := myBranche.GetVisee(v);              myVisee := myBranche.GetVisee(v);
            //uP += myVisee.DeltaP * IIF((myBranche.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
            //uZ += myVisee.DeltaZ;
            uP += myVisee.AccroissP * IIF((myBranche.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
            uZ += myVisee.AccroissXYZ.Z;

            uH := uZ + myVisee.HZ;
            uN := uZ - myVisee.HN;
            PM.setFrom(JC1.Abscisse + uP, JC1.Cote + uH);
            PP := GetCoordsPlan(PM);
            MoveTo(PP);
            PM.setFrom(JC1.Abscisse + uP, JC1.Cote + uN);
            PP := GetCoordsPlan(PM);
            LineTo(PP);
          end;
        end;
      end;
    end;
    // ID stations, cotations, etc ...
    procedure DrawTextesStations(const Brch: TBrancheCoupeDeveloppeeAsRecord;
                                 const DoAllStations: boolean;
                                 const M: integer);
    var
      JC1, JC2  : TJonctionCoupeDeveloppee;
      v, NbV    : integer;
      myVisee   : TUneVisee;
      uP, uZ    : double;
      WU: String;
      procedure drwTexte(const QAbsc, QCote: double; const QDir: boolean; const Txt: string);
      var
        QPM: TPointCoupeDeveloppee;
        QPP: TPoint;
        EWE: TSize;
      begin
        QPM.setFrom(QAbsc, QCote);
        QPP := GetCoordsPlan(QPM);
        QPP.X := QPP.X + 2;
        QPP.Y := QPP.Y - 2;
        if (Not QDir) then
        begin
          EWE := TmpBuffer.Canvas.TextExtent(Txt);
          QPP.X := QPP.X - EWE.cx - 4;
        end;
        TmpBuffer.Canvas.TextOut(QPP.X, QPP.Y, Txt);
      end;
      // dessin  des jonctions (noeuds)
      procedure DRJ(const QJ: TJonctionCoupeDeveloppee; const QDir: Boolean);
      var
        miou: String;
      begin
        TmpBuffer.CanvasBGRA.Brush.Color := clWhite;
        TmpBuffer.CanvasBGRA.Font.Color  := clBlue;
        TmpBuffer.CanvasBGRA.Font.Style  := [];
        TmpBuffer.CanvasBGRA.Font.Height := 8;
        case M of
          0: miou := Format(FORMAT_STRING       , [QJ.IDJonction]);
          1: miou := Format(FORMAT_NB_REAL_0_DEC, [QJ.Cote]);
          2: miou := Format(FORMAT_NB_REAL_0_DEC, [QJ.Cote]);
        else
          miou := '';
        end;
        drwTexte(QJ.Abscisse,QJ.Cote, QDir, miou);
      end;
    begin
      with TmpBuffer.CanvasBGRA do
      begin
        JC1 := FCoupeDeveloppee.GetJonction(Brch.NumeroNoeudDepart);
        JC2 := FCoupeDeveloppee.GetJonction(Brch.NumeroNoeudArrivee);
        Pen.Color   := clSilver;
        if (DoAllStations) then
        begin
          NbV := 1 + High(myBranche.ArrVisees); // NbV := myBranche.GetNbVisees;       NbV := myBranche.GetNbVisees;
          if (NbV > 0) then
          begin
            TmpBuffer.CanvasBGRA.Font.Color := clBlue;
            TmpBuffer.CanvasBGRA.Font.Style := [];
            TmpBuffer.CanvasBGRA.Font.Height  := 6;
            uP := 0.00;
            uZ := 0.00;
            for v := 0 to NbV - 1 do
            begin
              myVisee := myBranche.ArrVisees[v]; //myVisee := myBranche.GetVisee(v);              myVisee := myBranche.GetVisee(v);
              //uP += myVisee.DeltaP * IIF((Brch.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
              //uZ += myVisee.DeltaZ;
              uP += myVisee.AccroissP * IIF((Brch.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
              uZ += myVisee.AccroissXYZ.Z;

              case M of
                0: WU := Format(FMTSERST, [Brch.NumeroSerie, v]);
                1: WU := Format(FORMAT_NB_REAL_0_DEC, [JC1.Cote]);
                2: WU := Format(FORMAT_NB_REAL_0_DEC, [JC1.Cote]);
              else
                ;
              end;
              drwTexte(JC1.Abscisse + uP,
                       JC1.Cote + uZ,
                       Brch.SensTraceCoupe = stcdVERS_DROITE,
                       WU);
            end;
          end;
        end;
        // ID des jonctions en gras
        DRJ(JC1, Brch.SensTraceCoupe = stcdVERS_DROITE);
        DRJ(JC2, Brch.SensTraceCoupe = stcdVERS_DROITE);
      end;
    end;
  begin
    Nb := FCoupeDeveloppee.GetNbBranches;
    AfficherMessage(Format('---> Dessin des %d branches', [Nb]));
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do
      begin
        myBranche := FCoupeDeveloppee.GetBranche(i);
        if (edPolygonals    in FCoupeDeveloppeeParams.ElementsDrawn) then DrawCenterLines(myBranche);
        if (edWalls         in FCoupeDeveloppeeParams.ElementsDrawn) then
        begin
          DrawParois(myBranche, True);
          DrawParois(myBranche, False);
        end;
        if (edCrossSections in FCoupeDeveloppeeParams.ElementsDrawn) then DrawSections(myBranche);
        //if (edIDStations    in FCoupeDeveloppeeParams.ElementsDrawn) then DrawTextesStations(myBranche, false, 0);
        //if (edCotes         in FCoupeDeveloppeeParams.ElementsDrawn) then DrawTextesStations(myBranche, false, 1);
        if (edAltitudes     in FCoupeDeveloppeeParams.ElementsDrawn) then DrawTextesStations(myBranche, false, 2);
      end;
    end;
  end;
  procedure DrawQuadrillesCoupe(const AmorcesOnly: boolean);
  var
    q, t: Int64;
    A     : double;
    C1, C2: TPointCoupeDeveloppee;
    QPP1, QPP2: TPoint;
    S: String;
    QEquidistance: Double;
  begin

    QEquidistance := EnsureSetQuadrillageSpacing(FCoupeDeveloppeeParams.QdrSpc);
    t := trunc(FRegionZMini / QEquidistance);
    A := QEquidistance * t;
    TmpBuffer.CanvasBGRA.Pen.Width := 0;
    TmpBuffer.CanvasBGRA.Pen.Color := FCoupeDeveloppeeParams.QdrColor;
    TmpBuffer.CanvasBGRA.Font.Style:= [];
    TmpBuffer.CanvasBGRA.Font.Color:= clBlack;
    TmpBuffer.CanvasBGRA.Font.Height := 8;
    while (A < FRegionZMaxi) do
    begin
      C1.setFrom(FRegionPMini, A);
      C2.setFrom(FRegionPMaxi, A);
      QPP1 := GetCoordsPlan(C1);
      QPP2 := GetCoordsPlan(C2);
      if (AmorcesOnly) then QPP2.X := QPP1.X + 40;
      TmpBuffer.Canvas.MoveTo(QPP1.X, QPP1.Y);
      TmpBuffer.Canvas.LineTo(QPP2.X, QPP2.Y);
      //-------- Coordonnées en rive
      S := Format(FORMAT_NB_REAL_0_DEC,[A]);
      q := QPP1.Y - TmpBuffer.Canvas.TextHeight(S) div 2;
      TmpBuffer.Canvas.TextOut(2, q, S);
      //----------------------------
      A := A + QEquidistance;
    end;
  end;
begin
  if (not FDoDrawCoupe) then Exit;
  AfficherMessage('Redessin');
  TmpBuffer := TBGRABitmap.Create(vue.Width, vue.Height);
  try
    with TmpBuffer do
    begin
      R.Left   := Vue.Left;
      R.Top    := Vue.Top;
      R.Bottom := Vue.Top  + Vue.Height;
      R.Right  := Vue.Left + Vue.Width;
      CanvasBGRA.Brush.Color := FCoupeDeveloppeeParams.BackGround; // clwhite;
      CanvasBGRA.FillRect(R);
      DrawPipistrelle(TmpBuffer);   // pipistrelle
      DrawNodes;         // les noeuds
      DrawBranches;      // les branches
      // quadrillage
      if (edQuadrilles in FCoupeDeveloppeeParams.ElementsDrawn) then DrawQuadrillesCoupe(true);
    end;
    TmpBuffer.Draw(vue.Canvas, 0, 0, True);
  finally
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
  end;
end;

procedure TCdrCoupeDeveloppee.CentrerSurPZ(const QP, QZ: double);
var
  C     : TPointCoupeDeveloppee;
  dp, dz: double;
begin
  // obtenir le centre actuel
  C.setFrom(0.50 * (FRegionPMaxi - FRegionPMini),
            0.50 * (FRegionZMaxi - FRegionZMini));
  dp := QP - C.P;
  dz := QZ - C.Z;
  SetViewLimits(FRegionPMini + dp, FRegionZMini + dz, FRegionPMaxi + dp, FRegionZMaxi + dz);
  Vue.Invalidate;
end;

function TCdrCoupeDeveloppee.GetParamsVueCoupe(): TCoupeDeveloppeeParams;
begin
  Result := FCoupeDeveloppeeParams;
end;

procedure TCdrCoupeDeveloppee.SetParamsVueCoupe(const E: TCoupeDeveloppeeParams);
begin
  FCoupeDeveloppeeParams := E;
  Vue.Invalidate;
end;

procedure TCdrCoupeDeveloppee.DeplacerVueInt(const DepX, DepY: integer);
var
  P : TPoint;
  dq, dx, dy: Double;
begin
  P := MakeTPoint(0, 0);
  FZC1:= GetCoordsMonde(P);
  P := MakeTPoint(Vue.Width, Vue.Height);
  FZC2:= GetCoordsMonde(P);
  dq  := 0.02 * (FZC2.P - FZC1.P);
  dx  :=  dq * DepX;
  dy  := -dq * DepY;
  SetViewLimits(FRegionPMini + dx, FRegionZMini + dy, FRegionPMaxi + dx, FRegionZMaxi + dy);
  Vue.Invalidate; //RedessinEcran;
end;

procedure TCdrCoupeDeveloppee.DeplacerVue(const DepX, DepY: double);
begin
  SetViewLimits(FRegionPMini - DepX, FRegionZMini - DepY, FRegionPMaxi - DepX, FRegionZMaxi - DepY);
  Vue.Invalidate;
end;



procedure TCdrCoupeDeveloppee.SetModeTravail(const MT: TModesTravail);
  procedure QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
  begin
    Vue.Canvas.Pen.Mode := QPM;
    Vue.Canvas.Pen.Style:= QPS;
    Vue.Canvas.Pen.Color:= QPC;
    Vue.Canvas.Pen.Width:= QPW;
  end;
  procedure Rearmer();
  begin
    QSetPen(pmCopy, psSolid, 0, clGray);
  end;
begin
  FModesTravail := MT;
  FZoomC1OK     := False;
  FModesTravail := MT;
  case FModesTravail of
    mtREADY:
      begin
       Rearmer();
      end;
     mtPAN_PREMIER_POINT:
      begin
        //FAttenduSecondPoint := false;
        QSetPen(pmCopy, psSolid, 0, clGray);
      end;
    mtPAN_SECOND_POINT:
      begin
        //FAttenduSecondPoint := true;
        QSetPen(pmNotXor, psSolid, 0, clGray);
      end;
    mtZOOM_PREMIER_COIN:
      begin
        //FAttenduSecondPoint := false;
        QSetPen(pmCopy, psSolid, 0, clGray);
      end;
    mtZOOM_SECOND_COIN:
      begin
        //FAttenduSecondPoint := true;
        QSetPen(pmNotXor, psSolid, 0, clGray);
      end;
  end;
  lbModeTravail.Caption := ChooseString(Ord(MT),
                                       ['READY',
                                        'PAN', 'PAN',
                                        'ZOOM', 'ZOOM',
                                        'MF_ZONE', 'MF_ZONE',
                                        'DISTANCE', 'DISTANCE'
                                       ]);
  lbInfos.Caption := ChooseString(Ord(MT),
                                       ['--',
                                        'Premier point', 'Second point',
                                        'Premier coin', 'Second coin',
                                        'Premier coin', 'Second coin',
                                        'Premier point', 'Second point'
                                       ]);


end;

function TCdrCoupeDeveloppee.GetCurrentIdxBranche(): integer;
begin
  Result := FCurrentNoBranche;
end;

function TCdrCoupeDeveloppee.GetCurrentNumSerie(): integer;
begin
  result := FCurrentNoSerie;
end;

function TCdrCoupeDeveloppee.GetCurrentNumStation(): integer;
begin
  result := FCurrentNoStation;

end;



function TCdrCoupeDeveloppee.GetCoinBasGauche(): TPointCoupeDeveloppee;
begin
  Result.P := FRegionPMini;
  Result.Z := FRegionZMini;
end;

function TCdrCoupeDeveloppee.GetCoinHautDroit(): TPointCoupeDeveloppee;
begin
  Result.P := FRegionPMaxi;
  Result.Z := FRegionZMaxi;
end;

//******************************************************************************

function TCdrCoupeDeveloppee.Initialise(const T: TCoupeDeveloppee): boolean;
begin
  result := false;
  try
    FDoDrawCoupe := False;
    AfficherMessage(Format('%s.Initialise()', [ClassName]));
    FCoupeDeveloppee := T;
    FCoupeDeveloppeeParams.BackGround    := clWhite;
    FCoupeDeveloppeeParams.QdrColor      := clGray;
    FCoupeDeveloppeeParams.QdrSpc        := 20.00;
    FCoupeDeveloppeeParams.ElementsDrawn := [edPolygonals,
                                             edStations, edIDStations,
                                             edAltitudes, edCotes,
                                             edWalls, edCrossSections, edFillGalerie,
                                             edQuadrilles,
                                             edENTRANCE_MKS,
                                             edCROQUIS];
    //ResetVue;
    FCurrentNoBranche  := -1;
    FCurrentNoSerie    := 0;
    FCurrentNoStation  := 0;
    result := true;
  except
  end;
end;
procedure TCdrCoupeDeveloppee.Finalise();
begin
  ;
end;

end.

