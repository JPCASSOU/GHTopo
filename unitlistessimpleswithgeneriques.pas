unit UnitListesSimplesWithGeneriques;
// 25/10/2013: Découplage des listes simples de l'unité ToporobotClasses
// 14/04/2014: Sécurisation des accès aux réseaux, secteurs, entrées et antennes.
//             NE PAS SECURISER L'ACCES AUX EXPES ET CODES (traitement spécifique)
// 11/08/2016: Refactoring +
//             wrappers destinés à éviter les écritures de la forme
//               FTableEntrees  : TListeSimple<TEntrance>;
//             dans le code appelant. On utilise désormais
//               FTableEntrees  : TTableEntrees;
// 13/06/2019: Point de contrôle temporel (contrôle de version)
//
// Quelques rappels concernant les TList:
// - Ce sont des listes chaînées
// - Elles comportent un tableau Items[] de pointeurs permettant un temps constant aux éléments
// - La taille maxi est de (2**31) / 16 = 134 217 728 éléments (134 millions)
{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Graphics, math,
  Classes, SysUtils
  ;
// liste générique (fonctionne très bien)
type

{ TListeSimple }
 //TThreadList

 TListeSimple<T> = class(TFPList)
  private
  public
    procedure ClearListe();
    function  GetNbElements(): integer; inline;
    procedure InsertElement(const Idx: integer; const E: T);
    procedure AddElement(const E: T);  // et Rros Minet
    function  GetElement(const Idx: integer): T; inline;
    function  GetLastElement(): T; inline;
    procedure PutElement(const Idx: integer; const E: T); inline;
    function  RemoveElement(const Idx: integer): boolean;
    function  RemoveLastElement(): boolean;
    procedure EnQueue(const E: T); inline; // file: Ajoute un entrant -- Synonyme de procedure AddElement();
    function  DeQueue(): T;                // file: extrait et supprime le premier entré (l'élément 0 est devant le guichet)
    procedure Push(const E: T); inline;    // Pile: Ajoute un élément -- Synonyme de AddElement()
    function  Pop(): T;                    // Pile: Extrait le dernier élément entré et le supprime de la liste

end;

// LISTES SIMPLES
// /!\ Laisser comme tel en raison des fonctions additionnelles
//     GetElementByIndex() et ExistsElement() qui s'appellent de la même façon

// wrappers destinés à éviter les écritures de la forme
//    FTableEntrees  : TListeSimple<TEntrance>;
// dans le code appelant. On utilise désormais
//    FTableEntrees  : TTableEntrees;

type TTableNamespaces = class(TListeSimple<TNameSpace>)    // espaces de nom
   private
   public
end;

type

{ TListeOfGISLayers }

 TListeOfGISLayers = class(TListeSimple<TGISLayer>)    // espaces de nom
   private
     function  CanAddLayer(const L: TGISLayer): boolean;
   public
     procedure AddGISLayer(const QSymboleStyle: integer;
                            const QSymbolSize: double;
                            const QSymbolColor: TColor;
                            const QSymbolOpacity: byte;
                            const QLayerVarName    : string;
                            const QLayerTitle      : string;
                            const QLayerDescription: string = '';
                            const QLayerAttribution: string = '');
end;



type TListeMessagesErreur = class(TListeSimple<TMessaqeErreurGHTopoCompiler>)
  private
  public
end;

type TListOfIntegers = class(TListeSimple<integer>)
  private
  public
end;

type TTableEntitesVisees = class(TListeSimple<TBaseStation>)
   private
   public
end;
type TTableEntitesAntennes = class(TListeSimple<TBaseStation>)
   private
   public
end;
type  TTableEntrances = class(TListeSimple<TEntrance>)
   private
   public
end;

type TTableReseaux = class(TListeSimple<TReseau>)
   private
   public
end;
type TTableSecteurs = class(TListeSimple<TSecteur>)
   private
   public
end;

type TTableFiltresPersonnalises = class(TListeSimple<TFiltrePersonnalise>)
   private
   public
end;

type TTableIDTerrain = class(TListeSimple<TToporobotIDStation>)
   private
   public
end;



type TListeSelectedSeries = class(TListeSimple<TNumeroSerie>)
  private
  public
end;

type TListePointsTopo = class(TListeSimple<TUneVisee>)
  private
  public
end;

type  TMNTTableVertex    = class(TListeSimple<TMNTVertex>)
  private
  public
end;

type  TMNTTableTriangles = class(TListeSimple<TMNTTriangleABC>)
  private
  public
end;


type TListeAdjacenceSommet = class(TListeSimple<integer>)
  private
  public
end;


// Classe table Branches
// structure Branches (ancienne méthode, suffisante ici)
type TTableBranchesXYZ = class(TListeSimple<TBrancheXYZ>)
  private
  public
end;


//
// Classe pour la table des jonctions
type TTableJonctionsXYZ = class(TListeSimple<TJonctionXYZ>)
  private
  public
end;

type TListeJonctionsCoupeDeveloppee = class(TListeSimple<TJonctionCoupeDeveloppee>)
  private
  public
end;

type
{ TListeOfTGHStringArray }

 TListeOfTGHStringArray = class(TListeSimple<TGHStringArray>)
  private
  public
    procedure SortByFirstColumn();
end;

{ TTableViseesAntenne }

 type TTableViseesAntenne = class(TListeSimple<TViseeAntenne>)
   private
   public
     function  Purger(): integer;
     procedure ResetMarkersToDelete(); //
     function  ExtractAntennesFromBasePoint(const Ser: TNumeroSerie; const St: integer; const DoDeleteAntennesFound: boolean; out ArrAntennesFound: TArrayOfTViseeAntenne): integer;
     procedure SimplifyAntennesOfStation(const QSerie: TNumeroSerie; const QPoint: integer;
                                         const QCode : TCode; const QExpe : TExpe;
                                         const ToleranceSimplification: double);
     //procedure ExportToCloudCompare(const FichierCSV: TStringDirectoryFilename);
end;


type THelpListeDesSections = class(TListeSimple<THelpFileSection>)
  private
  public
end;





type TListeVectorsDataDistoX = class(TListeSimple<TVectorDataDistoX>)
  private
  public
end;
(*
type TListeTPoint2DfTagged = class(TListeSimple<TPoint2DfTagged>)
  private
  public
end;
//*)

type TListePoints2Df = class(TListeSimple<TPoint2Df>)
  private
  public
end;
type TListePoints3Df = class(TListeSimple<TPoint3Df>)
  private
  public
end;
//******************************************************************************
// classes enrichies
type TTableCodes = class(TListeSimple<TCode>)
   private
   public
     function  GetElementByIndex(const Idx: TNumeroCode): TCode;
     function  ExistsElement(const Idx: TNumeroCode): boolean;
end;
type TTableExpes = class(TListeSimple<TExpe>)
   private
   public
     function  GetElementByIndex(const Idx: TNumeroExpe): TExpe;
     function  ExistsElement(const Idx: TNumeroExpe): boolean;
end;
type TListePortionsTube = class(TListeSimple<TPortionTubeVisee>)
  private
  public
    procedure SortByDepthField();
end;
type TListePointsOfInterest = class(TListeSimple<TPointOfInterest>)
  private
  public
end;


type TCroquisListeStylesPolylignes = class(TListeSimple<TKrobardStylePolyligne>)
  private
  public
end;
type TCroquisListeStylesAnnotations = class(TListeSimple<TTexteAttributs>)
  private
  public
end;
type TCroquisListePolylignes = class(TListeSimple<TKrobardPolyligne>)
  private
  public
end;
type TCroquisListeAnnotations = class(TListeSimple<TKrobardAnnotation>)
  private
  public
end;
type TCroquisBufferVertexes = class(TListeSimple<TPoint3Df>)
  private
  public
end;

// profils de terrain (transect)
type TListePointsProfil = class(TListeSimple<TPoint3DfOrderedByP>)
  private
  public
end;
type TListeConduitsRecoupes = class(TListeSimple<TBaseStation>)
  private
  public
end;
 // proximités = stations proches d'une fin de série
type  TListeStationProcheEndSerie = class(TListeSimple<TStationProcheOfEndSerie>)
  private
  public
    procedure Trier(const Mode: byte);
end;
type  TListeSeriesEncadrees = class(TListeSimple<TSerieEncadree>)
  private
  public
end;

type  TTamponMesuresViseesDistoX = class(TListeSimple<TMesureBruteDistoX>)
  private
  public
end;

type  TListeMesuresViseesDistoX = class(TListeSimple<TMesureViseeDistoX>)
  private
  public
end;
//******************************************************************************
implementation
uses
  DGCDummyUnit; // Anti-erreur 'Fin du conde source non trouvée'

{ TListeSimple<T> }
procedure TListeSimple<T>.ClearListe();
var
  i, n: Integer;
begin
  n := self.Count;
  if (n > 0) then
  for i:=Count-1 downto 0 Do
  begin
    if (self.Items[i] <> Nil) then Dispose(self.Items[i]); // Libération
    self.Delete(i);                                        // Suppression de l'élément
  end;
end;


function TListeSimple<T>.GetNbElements: integer;
begin
  Result := self.Count;
end;

procedure  TListeSimple<T>.InsertElement(const Idx: integer; const E: T);
var pE: ^T;
begin
  New(pE);
  pE^ := E;
  self.Insert(Idx, pE);
end;
procedure TListeSimple<T>.AddElement(const E: T);
var pE: ^T;
begin
  New(pE);
  pE^ := E;
  self.Add(pE);
end;

function TListeSimple<T>.GetElement(const Idx: integer): T;
begin
  Result := T(Items[Idx]^);
end;




procedure TListeSimple<T>.PutElement(const Idx: integer; const E: T);
begin
  try
    if (Idx < 0) then exit;
    T(Items[Idx]^) := E;
  except
  end;
end;

function TListeSimple<T>.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;

function TListeSimple<T>.RemoveLastElement(): boolean;
var
  Nb: Integer;

begin
  Nb := self.Count;
  if (0 = Nb) then Exit(false);
  result := self.RemoveElement(Nb - 1);
end;

procedure TListeSimple<T>.EnQueue(const E: T);
begin
  AddElement(E);
end;

function TListeSimple<T>.DeQueue(): T;
begin
  Result := GetElement(0);
  RemoveElement(0);
end;

procedure TListeSimple<T>.Push(const E: T);
begin
  AddElement(E);
end;

function TListeSimple<T>.Pop(): T;
var
  n: Integer;
begin
  n := GetNbElements();
  if (n > 0) then
  begin
    Result := GetElement(n-1);
    RemoveElement(n-1);
  end;
end;
function TListeSimple<T>.GetLastElement(): T;
var n: integer;
begin
  n := self.getNbElements();
  result := self.getElement(n-1);
end;

{ TListeOfGISLayers }

function TListeOfGISLayers.CanAddLayer(const L: TGISLayer): boolean;
var
  i, Nb: Integer;
  QLayer: TGISLayer;
begin
  result := false;
  Nb := self.Count;
  if (Nb = 0) then Exit(True);
  for i := 0 to Nb - 1 do
  begin
    QLayer := GetElement(i);
    if (QLayer.LayerVarName = L.LayerVarName) then exit(false);
  end;
  result := true;
end;


procedure TListeOfGISLayers.AddGISLayer(const QSymboleStyle: integer;
                                        const QSymbolSize: double;
                                        const QSymbolColor: TColor;
                                        const QSymbolOpacity: byte;
                                        const QLayerVarName    : string;
                                        const QLayerTitle      : string;
                                        const QLayerDescription: string = '';
                                        const QLayerAttribution: string = '');
var
  MyLayer: TGISLayer;
  EWE: string;
begin
  if (not CheckOSMLayerVarName(QLayerVarName, EWE)) then Exit;
  MyLayer.SymboleStyle        := QSymboleStyle;
  MyLayer.SymbolSize          := QSymbolSize;
  MyLayer.SymbolColor.setFrom(QSymbolColor, QSymbolOpacity);
  MyLayer.LayerVarName        := EWE;
  MyLayer.LayerTitle          := QLayerTitle;
  MyLayer.LayerDescription    := QLayerDescription;
  MyLayer.LayerAttribution    := QLayerAttribution;
  if (CanAddLayer(MyLayer)) then AddElement(MyLayer);
end;

{ TTableViseesAntenne }

function TTableViseesAntenne.Purger(): integer;
var
  Nb, i: Integer;
  MyAntenne: TViseeAntenne;
begin
  Nb     := GetNbElements();
  Result := Nb;
  if (Nb = 0) then Exit(0);
  for i := Nb - 1 downto 0 do
  begin
    MyAntenne := GetElement(i);
    if (MyAntenne.MarkedForDelete) then RemoveElement(i);
  end;
  Self.Pack;
  Result := Nb - GetNbElements();
end;

procedure TTableViseesAntenne.ResetMarkersToDelete();
var
  i, Nb: Integer;
  MyAntenne: TViseeAntenne;
begin
  Nb     := GetNbElements();
  if (Nb = 0) then Exit();
  for i := 0 to Nb - 1 do
  begin
    MyAntenne := GetElement(i);
    MyAntenne.MarkedForDelete := false;
    PutElement(i, MyAntenne);
  end;
end;


function TTableViseesAntenne.ExtractAntennesFromBasePoint(const Ser: TNumeroSerie; const St: integer; const DoDeleteAntennesFound: boolean; out ArrAntennesFound: TArrayOfTViseeAntenne): integer;
var
  Nb, i: integer;
  MyAntenne: TViseeAntenne;
begin
  Result := 0;
  SetLength(ArrAntennesFound, 0);
  Nb := GetNbElements();
  if (Nb = 0) then Exit(0);
  for i := Nb - 1 downto 1 do
  begin
    MyAntenne := GetElement(i);
    if ((Ser = MyAntenne.SerieDepart) and (St = MyAntenne.PtDepart)) then
    begin
      Result += 1;
      SetLength(ArrAntennesFound, Result);
      ArrAntennesFound[Result - 1].Idx := i;
      ArrAntennesFound[Result - 1].VA  := MyAntenne;
      if (DoDeleteAntennesFound) then
      begin
        MyAntenne.MarkedForDelete := True;
        PutElement(i, MyAntenne);
      end;
    end;
  end;
  //AfficherMessageErreur(Format('%s.ExtractAntennesFromBasePoint(%d.%d - %d found of %d)', [ClassName, Ser, St, Result, Nb]));
end;

procedure TTableViseesAntenne.SimplifyAntennesOfStation(const QSerie: TNumeroSerie;
                                                        const QPoint: integer;
                                                        const QCode : TCode;
                                                        const QExpe : TExpe;
                                                        const ToleranceSimplification: double);
const AZ0 = 2 * PI;
var
  ArrAntennesFound, QArrSimplifie: TArrayOfTViseeAntenne;
  QMarkers: array of boolean;
  NbAntennesFound, i, QNb: Integer;
  EWE: TViseeAntenneFound;
  MyVisee: TUneVisee;
  procedure QDispMsgErr(const Msg: string); inline;
  begin
    //AfficherMessageErreur(Msg);
  end;
  function VecMinFloat3D(const A, B: TViseeAntenneFound): TViseeAntenneFound;
  begin
    Result.Position.setFrom(A.Position.X - B.Position.X,
                            A.Position.Y - B.Position.Y,
                            A.Position.Z - B.Position.Z);
  end;
  function DotProdFloat3D(const A, B: TViseeAntenneFound): double; inline;  // Dotproduct = A * B
  begin
    Result := A.Position.x * B.Position.x + A.Position.y * B.Position.y + A.Position.z * B.Position.z;

  end;
  function NormSquaredFloat3D(const A: TViseeAntenneFound): double; inline;  // Square of the norm |A|
  begin
    Result := sqr(A.Position.x) + sqr(A.Position.y) + sqr(A.Position.z);

  end;
  function DistSquaredFloat3D(const A, B: TViseeAntenneFound): double;  inline;  // Square of the distance from A to B
  begin
    Result := NormSquaredFloat3D(VecMinFloat3D(A, B));
  end;
  function SafeDivide(const a, b: double; const Default: double): double;
  begin
    try
      Result := a / b;
    except
      Result := Default;
    end;
  end;
  //----------------------------------------
  procedure QSortByAzimutAbsolu(lidx, ridx: integer);
  var
    k, e, mid: integer;
    Buffer: TViseeAntenneFound;
  begin
    with self do
    begin
      if (lidx >= ridx) then Exit;
      mid := (lidx + ridx) div 2;
      Buffer := ArrAntennesFound[lidx];
      ArrAntennesFound[lidx]:=ArrAntennesFound[mid];
      ArrAntennesFound[mid]:=Buffer;
      e:=lidx;
      for k:=lidx+1 to ridx do
      begin
        if (ArrAntennesFound[k].AzimutAbsolu < ArrAntennesFound[lidx].AzimutAbsolu)  then
        begin
          Inc(e);
          Buffer := ArrAntennesFound[e];
          ArrAntennesFound[e]:=ArrAntennesFound[k];
          ArrAntennesFound[k]:=Buffer;
        end;
      end;
      Buffer := ArrAntennesFound[lidx];
      ArrAntennesFound[lidx]:=ArrAntennesFound[e];
      ArrAntennesFound[e]:=Buffer;
      QSortByAzimutAbsolu(lidx, e-1);
      QSortByAzimutAbsolu(e+1, ridx);
    end;
  end;
  //-----------------------------------------------------------
  procedure SimplifyInt2D(var Tol2: double; const j, k: integer);
  // Simplify polyline in OrigList between j and k. Marker[] will be set to True
  // for each point that must be included
  var
    ii, MaxI: integer; // Index at maximum value
    MaxD2: double;    // Maximum value squared
    CU, CW, B: double;
    DV2: double;
    P0, P1, U, W, PB: TViseeAntenneFound;
  begin
    if k <= j + 1 then exit;  // Is there anything to simplify?
    P0 := ArrAntennesFound[j];
    P1 := ArrAntennesFound[k];
    U  := VecMinFloat3D(P1, P0); // Segment vector
    CU := DotProdFloat3d(U, U); // Segment length squared
    MaxD2 := 0;
    MaxI  := 0;
    for ii := j + 1 to k - 1 do // Loop through points and detect the one furthest away
    begin
      W  := VecMinFloat3D(ArrAntennesFound[ii], P0);
      CW := DotProdFloat3D(W, U);

      // Distance of point ArrAntennesFound[ii] from segment
      if CW <= 0 then
      begin
        DV2 := DistSquaredFloat3D(ArrAntennesFound[ii], P0)   // Before segment
      end else
      begin
        if (CW > CU) then
        begin
          DV2 := DistSquaredFloat3D(ArrAntennesFound[ii], P1); // Past segment
        end else begin
          // Fraction of the segment
          try
            B := CW / CU;
          except
            B := 0; // in case CU = 0
          end;
          PB.Position.setFrom(P0.Position.x + B * U.Position.x,
                              P0.Position.y + B * U.Position.y,
                              P0.Position.z + B * U.Position.z);
          DV2 := DistSquaredFloat3D(ArrAntennesFound[ii], PB);
        end;
      end;
      if (DV2 > MaxD2) then // test with current max distance squared
      begin
        MaxI  := ii;      // Orig[ii] is a new max vertex
        MaxD2 := DV2;
      end;
    end;
    if (MaxD2 > Tol2) then   // If the furthest point is outside tolerance we must split
    begin // error is worse than the tolerance // split the polyline at the farthest vertex from S
      QMarkers[MaxI] := True;  // mark Orig[maxi] for the simplified polyline
      // recursively simplify the two subpolylines at Orig[maxi]
      SimplifyInt2D(Tol2, j, MaxI); // polyline Orig[j] to Orig[maxi]
      SimplifyInt2D(Tol2, MaxI, k); // polyline Orig[maxi] to Orig[k]
    end;
  end;
  //-----------------------------------------------------------
  function PolySimplifyInt2D(const Tol: double;
                             out   QSimple: TArrayOfTViseeAntenne): integer;
  var
    ii     : integer;
    Tol2   : double;
    FTmpLst: TListeSimple<TViseeAntenneFound>;
    NbAntennesRetenues: LongInt;
  begin
    QDispMsgErr(Format('PolySimplifyInt2D: Tol: %.2f, n=%d', [Tol, NbAntennesFound]));
    Result := 0;
    if (NbAntennesFound < 3) then exit;
    Tol2 := Tol * Tol;
    try
      FTmpLst:= TListeSimple<TViseeAntenneFound>.Create;
      try
        FTmpLst.ClearListe();
        SetLength(QMarkers, NbAntennesFound);  // Create a marker array
        QMarkers[0]     := True;               // Include first and last point
        QMarkers[NbAntennesFound - 1] := True;
        // Exclude intermediate for now
        for ii := 1 to NbAntennesFound - 2 do QMarkers[ii] := False;
        SimplifyInt2D(Tol2, 0, NbAntennesFound - 1);  // Simplify
        QDispMsgErr(Format('Marqueurs -- N=%d', [NbAntennesFound]));
        for ii := 0 to NbAntennesFound - 1 do  // Copy to resulting list
        begin
          if (QMarkers[ii]) then FTmpLst.AddElement(ArrAntennesFound[ii]);
        end;
        NbAntennesRetenues := FTmpLst.GetNbElements();
        SetLength(QSimple, NbAntennesRetenues);
        for ii := 0 to NbAntennesRetenues - 1 do QSimple[ii] := FTmpLst.GetElement(ii);
        FTmpLst.ClearListe();
        Result := NbAntennesRetenues;
      except
        Result := -1;
      end;
    finally
      FreeAndNil(FTmpLst);
    end;
  end;
begin
  QDispMsgErr(Format('%s.SimplifyAntennesOfStation(%d.%d)', [ClassName, QSerie, QPoint]));
  QDispMsgErr('------------------------------');
  SetLength(QMarkers, 0);
  QDispMsgErr(Format('Etape %d: %s %d.%d', [1,  'Extraction des visées radiantes de la station',  QSerie, QPoint]));

  NbAntennesFound := ExtractAntennesFromBasePoint(QSerie, QPoint, True, ArrAntennesFound);
  // nombre d'antennes insuffisant ou nul: arrêt du traitement
  if (NbAntennesFound < 3) then
  begin
    //if (NbAntennesFound > 0) then ResetMarkersToDelete(); // si NbAntennesFound compris entre 0 et 2, on désactive les marqueurs de suppression;
    //if (NbAntennesFound = 0) then AfficherMessageErreur('*** Aucune antenne pour cette station - Fin de traitement')
    //                         else AfficherMessageErreur('*** Moins de 3 antennes, simplification inutile');
    exit;
  end;
  try
    QDispMsgErr(Format('Extraction réussie: Station %d.%d - %d antennes extraites - Code: %d (%s)', [QSerie, QPoint, NbAntennesFound, QCode.IDCode, QCode.Commentaire]));

    QDispMsgErr(Format('Etape %d: %s', [2,  'Calcul des coordonnées locales']));

    // calcul des coordonnées locales
    for i := 0 to NbAntennesFound - 1 do
    begin
      EWE := ArrAntennesFound[i];
      EWE.Position.Empty();
      EWE.DP := 0.00;

      MyVisee.IDSecteur       := 0; // VA.Secteur; // TODO: Secteur à implémenter
      MyVisee.Code            := QCode.IDCode;
      MyVisee.Expe            := QExpe.IDExpe;
      MyVisee.Longueur        := EWE.VA.Longueur;
      MyVisee.Azimut          := EWE.VA.Azimut;
      MyVisee.Pente           := EWE.VA.Pente;
      MyVisee.setLRUD(0.00, 0.00, 0.00, 0.00);
      MyVisee.Commentaires     := '';
      MyVisee.IDTerrainStation := '';
      MyVisee.TypeVisee        := tgVISEE_RADIANTE;

      CalculerVisee(MyVisee, QCode, QExpe, EWE.Position, EWE.DP);
      EWE.AzimutAbsolu := AZ0 + ArcTan2(EWE.Position.y, EWE.Position.x);
      ArrAntennesFound[i] := EWE; // et on met à jour
    end;
    // tri des shots par azimut
    QDispMsgErr(Format('Etape %d: %s', [3,  'Tri par azimuts croissants']));
    QSortByAzimutAbsolu(0, NbAntennesFound - 1);
    (*
    for i := 0 to NbAntennesFound - 1 do
    begin
      EWE := ArrAntennesFound[i];
      QDispMsgErr(Format('-- %d:%d: %d.%d - %.3f, %.3f, %.3f; %.2f, %.2f, %.2f, - AA: %.5f rad = %2f deg - %s',
                            [i, EWE.Idx,
                             EWE.VA.SerieDepart, EWE.VA.PtDepart,
                             EWE.VA.Longueur, EWE.VA.Azimut, EWE.VA.Pente,
                             EWE.DX, EWE.DY, EWE.DZ,
                             EWE.AzimutAbsolu, (Trunc(100 * (360 + radtodeg(EWE.AzimutAbsolu))) MOD 36000) / 100.00 ,
                             IIF(EWE.VA.MarkedForDelete, 'To remove', '--')]));
    end;
    QDispMsgErr('-----');
    //*)
    (*
    for i := 0 to NbAntennesFound - 1 do
    begin
      EWE := ArrAntennesFound[i];

      QDispMsgErr(Format('%d; %d.%d; %s; %s; %s; %s; %s; %s',
                            [i, EWE.VA.SerieDepart, EWE.VA.PtDepart,
                             FormatterNombreOOo(EWE.VA.Longueur, 3),
                             FormatterNombreOOo(EWE.VA.Azimut, 3),
                             FormatterNombreOOo(EWE.VA.Pente, 3),
                             FormatterNombreOOo(EWE.DX, 3),
                             FormatterNombreOOo(EWE.dy, 3),
                             FormatterNombreOOo(EWE.dz, 3)]));
    //**********************************************************
    end;
    //*)
    // simplification
    QDispMsgErr(Format('Etape %d: %s (tolerance: %.2f m)', [4,  'Simplification', ToleranceSimplification]));

    QNb := PolySimplifyInt2D(0.50, QArrSimplifie);
    QDispMsgErr(Format('%d Antennes retenues.', [QNb]));
    QNb := length(QArrSimplifie);
    QDispMsgErr(Format('Etape %d: %d %s ', [4, QNb, 'antennes a ajouter a la table']));
    for i := 0 to QNb - 1 do
    begin
      EWE := QArrSimplifie[i];
      EWE.VA.MarkedForDelete := false;
      self.AddElement(EWE.VA);

      QDispMsgErr(Format('%d; %d.%d; %s; %s; %s; %s; %s; %s',
                            [i, EWE.VA.SerieDepart, EWE.VA.PtDepart,
                             FormatterNombreOOo(EWE.VA.Longueur, 3),
                             FormatterNombreOOo(EWE.VA.Azimut, 3),
                             FormatterNombreOOo(EWE.VA.Pente, 3),
                             FormatterNombreOOo(EWE.Position.X, 3),
                             FormatterNombreOOo(EWE.Position.y, 3),
                             FormatterNombreOOo(EWE.Position.z, 3)]));  //*)
    //**********************************************************
    end;
    QDispMsgErr(Format('Etape %d: %s', [6,  'Purge']));
    // purge
    (*
    Qnb := GetNbElements();
    for i := 0 to QNb - 1 do
    begin
      EWE.VA := GetElement(i);
      if (EWE.VA.MarkedForDelete) then
          QDispMsgErr(Format('To delete: %d; %d.%d; %s; %s; %s;',
                            [i, EWE.VA.SerieDepart, EWE.VA.PtDepart,
                             FormatterNombreOOo(EWE.VA.Longueur, 3),
                             FormatterNombreOOo(EWE.VA.Azimut, 3),
                             FormatterNombreOOo(EWE.VA.Pente, 3)]));
    //**********************************************************
    end;
    //*)

    //if (DoPurgerImmediatement) then AfficherMessageErreur(Format('%d antennes removed', [self.Purger()]));
    // ajout
    SetLength(QMarkers, 0);
  except
    QDispMsgErr('*** Erreur lors de la simplification - Restauration ***');


  end;
end;

(*
procedure TTableViseesAntenne.ExportToCloudCompare(const FichierCSV: TStringDirectoryFilename);
var
  i, Nb: Integer;
begin
  Nb := GetNbElements();
  AfficherMessage(format('%s.ExportToCloudCompare: %d data in "%s"', [Nb, FichierCSV]));

end;
//*)

//******************************************************************************
function TTableCodes.GetElementByIndex(const Idx: TNumeroCode): TCode;
var
  ii: integer;
  E : TCode;
begin
  for ii:= 0 to self.Count - 1 do
  begin
    E := self.GetElement(ii);
    if (E.IDCode = Idx) then exit(E);
  end;
  // si item introuvable, on renvoie l'élément 0
  Result := self.GetElement(0);
end;
function TTableCodes.ExistsElement(const Idx: TNumeroCode): boolean;
var
  i: integer;
  EWE: TCode;
begin
  Result := False;
  for i:= 0 to self.Count - 1 do
  begin
    EWE := self.GetElement(i);
    if (EWE.IDCode = Idx) then Exit(True);
  end;
end;
//******************************************************************************
{ TTableExpes }

function TTableExpes.GetElementByIndex(const Idx: TNumeroExpe): TExpe;
var
  ii: integer;
  E : TExpe;
begin
  for ii:=0 to self.Count-1 do
  begin
    E := self.GetElement(ii);
    if (E.IDExpe = Idx) then exit(E);
  end;
  Result := self.getElement(0);
end;

function TTableExpes.ExistsElement(const Idx: TNumeroExpe): boolean;
var
  i: integer;
  EWE: TExpe;
begin
  Result := False;
  for i:= 0 to self.GetNbElements - 1 do
  begin
    EWE := self.GetElement(i);
    if (EWE.IDExpe = Idx) then Exit(True);
  end;
end;
//******************************************************************************
{ TTableViseesAntenne }
(*
function TTableViseesAntenne.GetElementByIndex(const Idx: integer): TViseeAntenne;
var
  ii: integer;
  E : TViseeAntenne;
begin
  Result := self.GetElement(0); // sécurisation
  if (not IsInRange(Idx, 0, self.Count - 1)) then Exit;
  for ii:=0 to self.Count-1 do
  begin
    E := self.GetElement(ii);
    if (E.IDViseeAntenne = Idx) then Break;
  end;
  Result := E;
end;
//*)
//******************************************************************************

// tri des facettes
procedure TListePortionsTube.SortByDepthField();
  procedure QSortByDepth(lidx, ridx: integer);
  var
    k, e, mid: integer;
    Buffer: TPortionTubeVisee;
  begin
    with self do
    begin
      if lidx >= ridx then Exit;
      mid := (lidx + ridx) div 2;

      Buffer := self.GetElement(lidx);
      self.PutElement(lidx, self.getElement(mid));
      self.PutElement(mid, Buffer);
      e := lidx;
      for k:=lidx+1 to ridx do
      begin
        if (GetElement(k).DepthField > GetElement(lidx).DepthField) then
        begin
          Inc(e);
          Buffer := GetElement(e);
          PutElement(e, GetElement(k));
          PutElement(k, Buffer);
        end;
      end;
      Buffer := GetElement(lidx);
      PutElement(lidx, GetElement(e));
      Putelement(e, Buffer);
      QSortByDepth(lidx, e-1);
      QSortByDepth(e+1, ridx);
    end;
  end;
begin
  QSortByDepth(1, self.getNbElements() - 1);
end;

{ TListeStationProcheEndSerie }
// Trier les visées par TagDouble (e.g. distance)
function SortProximitesByDistance(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TStationProcheOfEndSerie;
  bidon1: double;
  bidon2: double;
begin
  E1 := Item1;
  E2 := Item2;
  bidon1 := E1.Distance;
  bidon2 := E2.Distance;
  if      (bidon1 < bidon2) then Result := -1
  else if (bidon1 = bidon2) then Result :=  0
  else                           Result :=  1;
end;
function SortProximitesByBaseStation(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TStationProcheOfEndSerie;
  bidon1: int64;
  bidon2: int64;
begin
  E1 := Item1;
  E2 := Item2;
  bidon1 := E1.BaseStation.aSerie * 100000 + E1.BaseStation.aStation;
  bidon2 := E2.BaseStation.aSerie * 100000 + E1.BaseStation.aStation;
  if      (bidon1 < bidon2) then Result := -1
  else if (bidon1 = bidon2) then Result :=  0
  else                           Result :=  1;
end;
procedure TListeStationProcheEndSerie.Trier(const Mode: byte);
begin
  case Mode of
    0: self.Sort(SortProximitesByDistance);
    1: self.Sort(SortProximitesByBaseStation);
  else
    pass;
  end;
end;

{ TListeOfTGHStringArray }
function SortByFirstColumnOfTGHStringArray(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TGHStringArray;
  bidon1, bidon2: string;
begin
  E1 := Item1;
  E2 := Item2;
  bidon1 := E1[0];
  bidon2 := E2[0];
  if      (bidon1 < bidon2) then Result := -1
  else if (bidon1 = bidon2) then Result :=  0
  else                           Result :=  1;
end;
function SortBySerStDepart(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TGHStringArray;
  bidon1, bidon2: TIDBaseStation;
  function Q666(const S: string): TIDBaseStation;
  var
    EWE: TToporobotIDStation;
  begin
    EWE.setFrom(S);
    Result := MakeTIDBaseStation(EWE.aSerie, EWE.aStation, false);
  end;
begin
  E1 := Item1;
  E2 := Item2;
  bidon1 := Q666(E1[0]);
  bidon2 := Q666(E2[0]);
  if      (bidon1 < bidon2) then Result := -1
  else if (bidon1 = bidon2) then Result :=  0
  else                           Result :=  1;
end;


procedure TListeOfTGHStringArray.SortByFirstColumn();
begin
  self.Sort(SortBySerStDepart);
end;





end.
