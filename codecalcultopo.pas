unit CodeCalculTopo;
{$INCLUDE CompilationParameters.inc}
//****************************************************
// Projet     : GHTopo
// Modules    : Code de calcul topo
// Licence    : General Public License - WITHOUT WARRANTY
// Auteur     : JP CASSOU
// OS         : Windows 9x - Linux
// Langage    : Free-Pascal 2.6.x sous Lazarus
//-----------------------------------------------------
// Module     : ToporobotClasses:
//            : Structure des données d'un dossier Toporobot
// Compatibilité intégrale avec le standard Toporobot 1994.
// 22/08/2012 : Refonte du noyau de GHTopo (ToporobotClasses)
//              en séparant le SGBD et le code de calcul
// 24/08/2012 : Code de calcul validé.
// 14/02/2013 : Réacculturation au code de GHTopo. Petites corrections
// 03/10/2013 : Support du lasermètre Stanley TLM330
// 04/10/2013 : Conversions UTF8 <> ANSI fixed
// 05/11/2013 : Support de la notion de secteurs
// 10/11/2013 : Support de la nouvelle structure de données compilées
// 09/04/2014 : Support des pourcentages (mode 370) corrigé
// 05/05/2014 : Le calcul des antennes ne nécessite plus la création de noeuds
// 23/05/2014 : Optimisation dans le calcul des contours (4x moins de calcul trigo)
// 26/05/2014 : Activation/désactivation du mode débogage des matrices
// 28/05/2014 : Quelques fix, dont un calcul oublié.
// 04/08/2014 : Les entrées sont exclues de la liste des entités,
//              ce qui fixe par la même occasion de nombreux bugs.
// 08/08/2014 : Optimisation dans GetNoeud() augmentant considérablement la vitesse
//              de la phase de découpage en branches.
// 06/09/2014 : Déplacement de la fonction CalculerVisee dans l'unité Common
// 08/10/2014 : Liste des écarts
// 18/12/2014 : CalculerAccroissements() : Nettoyage des branches à accroissement nul
// 06/02/2015 : Les visées en antenne reprennent le couple série/point d'accrochage
//              sous la forme E.Entite_serie = -NumSerie; E.Entite_St = NumPoint
// 07/06/2016 : Les listes de jonctions et de branches passent aux génériques
// 28/08/2016 : Nettoyage de code dans la gestion des branches
// 26/09/2016 : MAJ dans les fonctions de correction d'erreurs systématiques instrumentales
// 07/10/2016 : Mise en place d'un callback pour affichage de la progression (refonte de l'interface)
// 29/03/2017 : TCodeCalculTopo hérite maintenant de TConteneurCodeCalcul situé dans unitConteneurCodeCalcul.pas
// 13/09/2017 : Décomposition de la phase de résolution du système matriciel:
//              La factorisation de B fournit une matrice L via la fonction MakeLMatrix()
//              Cette matrice L est ensuite utilisée par la fonction RemonterSysteme() qui retourne les coordonnées des noeuds
//              Avantage: La méthode des pondérations forfaitaires implique une matrice de compensation B identique
//              pour les trois axes; elle ne doit être construite qu'une seule fois
//              Sa factorisation L est également identique pour les trois axes, ce qui permet de la sortir de la boucle (X, Y, Z)
//              La factorisation de B est l'étape la plus longue du calcul (trois niveaux de boucles)
// 14/09/2017 : La recherche de l'étendue des termes non nuls des matrices B et L (aka R) accélère très fortement
//              la vitesse de calcul qui devient presque instantanée même avec de très gros réseaux
//              (testé avec la fusion de Citon I et II, 1940 noeuds: 4 secondes pour le calcul matriciel, contre 19 sans recherche d'étendue)
// 18/09/2017 : Mémorisation LOL des matrices: Fonctionnel et testé avec succès mais plus lent que la méthode tableaux rectangulaires 2D
// 06/08/2018 : Mémorisation LOT des matrices: idem LOL, un peu plus rapide
// 08/08/2018 : Unification des objets matrices: les TMatrix deviennent des objets TMatriceCreuses
//              -> la directive USE_MATRICES_CREUSES est inutile
// 09/08/2018 : Suppression de certains vecteurs inutiles
//              La fonction MakeVectorSecondMembre() construit les trois seconds membres d'un coup
//              La matrice R est détruite par MakeVectorSecondMembre()
// 02/09/2018 : Support de l'erreur de tourillon (composante verticale de l'erreur de centrage)
// 29/11/2018 : Réusinage du code, adaptation aux tables distinctes centerlines / radiantes
//-----------------------------------------------------
// 04/12/2019 : Point de contrôle temporel (contrôle de version)
// 17/05/2021 : Adaptation aux nouveaux types de données XYZ

(*
            /´¯/)          (\¯`\
           /  //            \\  \
          /  //              \\  \
   _ /´¯/   /´¯\           /¯`\   \¯`\__
 /  /  /   /   /O_      __O\   \   \  \  \
(  (  (   (   / )  )   (  ( \   )   )  )  )
\             \/  /     \  \/            /
 \               /       \              /
  \             (         )            /
   \             \       /            /

JE SUIS CHRISTIANOPHOBE
//*)



interface

uses
  {$IFDEF LINUX}
  cthreads,
  {$endif}
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  unitConteneurCodeCalcul,
  {$IFDEF MULTI_THREADING}
     {$ERROR: Multithreading inadapté}
  {$ENDIF MULTI_THREADING}
  ToporobotClasses2012,
  unitobjetserie,
  UnitEntitesExtended,
  Common,
  Classes, SysUtils, Graphics, math,
  // choix du modèle de stockage des matrices
  {$IFDEF USE_MATRIX_DATASTRUCTURE_LIST_OF_LISTS}
  unitMatricesCreuses_LOT,                // OK, un peu plus rapide que LOL,
  {$ELSE}
  unitMatricesCreusesArray2D,
  {$ENDIF}
  //unitMatricesCreuses_LOL,              // OK
  Forms; // pour Application
// Noeuds
type TNoeud = record
  IDNoeud : integer;
  Position: TPoint3Df;
end;

// LISTE DES BRANCHES
//Table des Noeuds
type TTableNoeuds = array of TNoeud;
// Code de calcul
type

{ TCodeDeCalcul }

 TCodeDeCalcul = class(TConteneurCodeCalcul)
  strict private
    R_Matrix: TMatriceCreuse;
    B_Matrix: TMatriceCreuse;
    L_Matrix: TMatriceCreuse;
    // index extrêmes de la matrice B
    FBMatrix_LowIndex  : TArrayOfIntegers;
    FBMatrix_HighIndex : TArrayOfIntegers;
    // matrice de pondération diagonale
    W_Mtx    : TVecteurDouble;
    VecteurSecondMembre: TArrayPoints3Df;
    // matrices temporaires
    S_Vector : TArrayPoints3Df; // vecteur local complémentaire
    XX       : TArrayPoints3Df;
    procedure InitialiseMatrices(const m, n: integer);
    procedure FinalizeMatrices();
    //procedure MakeAVector();
    procedure MakeBMatrix();
    procedure MakeVectorSecondMembre();
    function  MakeFactorisedLMatrix(out QL: TMatriceCreuse): boolean;
    procedure MakeWMatrix();
    procedure MakeRMatrix();




    procedure SolveAxis(const Axe: TAxeCoordonnees);
    //procedure SolveAxisX();
    //procedure SolveAxisY();
    //procedure SolveAxisZ();
    function  ExtractValueFromTPoint3Df(const Axe: TAxeCoordonnees; const P: TPoint3Df): double; inline;

    function  TraiterViseesEnAntenne(): boolean;
    // procédures de traitement unitaire pour les travaux multithreadables
    procedure ProcessARowRMatrix(const NoThread, Idx: integer);
    procedure ProcessARowBMatrix(const NoThread, Idx: integer);
    procedure ProcessAHighIndexRow(const NoThread, Idx: integer);
    procedure ProcessALowIndexRow(const NoThread, Idx: integer);
    procedure ProcessAViseeEnAntenne(const NoThread, Idx: integer);
    procedure ProcessABrancheToBDDEntites(const NoThread, Idx: integer);
    procedure ProcessRepartitionEcartsOfBranche(const NoThread, Idx: integer);

  private
    FTableNoeuds : TTableNoeuds;
    function  GetNoeud(const Serie, Station: integer): integer;
    function  GetMaxNode(): integer;
    function  CalculerCoordNoeuds(): boolean;
    procedure CalculContoursGaleriesExtended(const FichierTOP: string; const DoGenerateFile: boolean);
    function  EnvoyerJonctionsVersBDDEntites(): boolean;
    procedure recopierTablesEntreesReseauxSecteursCodesExpes();
    procedure RemonterSysteme(const QL: TMatriceCreuse; const Axe: TAxeCoordonnees);
    procedure RepartirEcarts();
  public
    // lancer le calcul
    function  CalculComplet(const UseDebug: boolean): boolean;
    // générer un fichier de synthèse (i.e. un TAB simplifié avec les coordonnées)
    function  GenererFichierSyntheseAvecCoordonnees(const FileName: string): boolean;
end;



implementation
uses
  DGCDummyUnit,
  Dialogs;
const
  PALIERS_INDICATION: integer = 100;
//******************************************************************************
(* TCodeDeCalcul *)
procedure TCodeDeCalcul.InitialiseMatrices(const m, n: integer);
begin
  AfficherMemoryUsage();
  // matrices temporaires
  SetLength(XX, 0);
  SetLength(XX, n + 1);
  SetLength(S_Vector, 0);
  SetLength(S_Vector, m + 1);
  SetLength(VecteurSecondMembre, 0);
  SetLength(VecteurSecondMembre, n + 1);
  R_Matrix := TMatriceCreuse.Create;
  R_Matrix.Initialiser(m    , n    , 'R');
  B_Matrix := TMatriceCreuse.Create;
  B_Matrix.Initialiser(n + 1, n + 1, 'B');
  L_Matrix := TMatriceCreuse.Create;
  L_Matrix.Initialiser(n + 1, n + 1, 'L');
  AfficherMemoryUsage();
end;

procedure TCodeDeCalcul.FinalizeMatrices();
begin
  try
    B_Matrix.Finaliser();
    L_Matrix.Finaliser();
    AfficherMemoryUsage();
  finally
    FreeAndNil(B_Matrix);//B_Matrix.Free;
    FreeAndNil(L_Matrix);//L_Matrix.Free;
  end;
  SetLength(VecteurSecondMembre, 0);
  SetLength(W_mtx   , 0);
  SetLength(XX      , 0);
  SetLength(S_Vector, 0);
end;
function TCodeDeCalcul.GetMaxNode(): integer;
var
  Branche: TBrancheXYZ;
  i      : integer;
  M      , QNbBranches: integer;
begin
  M := Low(Integer);
  QNbBranches := GetNbBranches();
  for i := 1 to QNbBranches - 1 do
  begin
    Branche:= GetBranche(i);
    if (Branche.NoeudDepart  > M) then M := Branche.NoeudDepart;
    if (Branche.NoeudArrivee > M) then M := Branche.NoeudArrivee;
  end;
  Result := M;
  AfficherMemoryUsage();
end;
// matrice de connexion (ici, matrice d'incidence)
procedure TCodeDeCalcul.ProcessARowRMatrix(const NoThread, Idx: integer);
var
  Branche: TBrancheXYZ;
  j: Integer;
begin
  Branche := GetBranche(Idx);
  for j := 1 to GetMaxNode() do
  begin
    // /!\ La matrice d'incidence des graphes orientés ne supporte pas les boucles, par construction
    // cf https://fr.wikipedia.org/wiki/Matrice_d%27incidence
    if      (Branche.NoeudDepart  = j) then R_Matrix.SetValeur(Idx, j, -1)
    else if (Branche.NoeudArrivee = j) then R_Matrix.SetValeur(Idx, j,  1)
                                       else R_Matrix.SetValeur(Idx, j,  0);
  end;

end;

procedure TCodeDeCalcul.MakeRMatrix();
var
  QNbBranches, QNbNodes, i: integer;
  EWE: String;
  t: TDateTime;

  //YaAutoloop: boolean;
begin
  QNbBranches := GetNbBranches();
  QNbNodes    := GetMaxNode();
  t := Now();
  for i := 1 to QNbBranches - 1 do
  begin
    if (i MOD 100 = 0) then
    begin
      EWE := Format('Matrice d''incidence: ' + rsLINEOFNB, [i, QNbBranches]);
      if (Assigned(self.FProcDispProgressionMonoThread)) then self.FProcDispProgressionMonoThread(EWE, i, 0, QNbBranches, 200);
    end;
    ProcessARowRMatrix(-1, i);
  end;
  t := Now() - t;
  AfficherMessageErreur(Format('Temps de construction de la matrice d''incidence R: %.8f', [t * 86400]));
  R_Matrix.RemoveAllZeroes();
  AfficherMemoryUsage();
  //R_Matrix.Lister();
  //R_Matrix.CreerRepresentationMatriceEnImage(GetGHTopoDirectory()+ 'R_Matrix.bmp');
end;
// matrice de compensation = Rt.W.R
// A paralléliser en priorité
procedure TCodeDeCalcul.ProcessALowIndexRow(const NoThread, Idx: integer);
var
  j: Integer;
begin
  for j := 1 to R_Matrix.MaxRow do
  begin
    if (Abs(R_Matrix.GetValeur(j, Idx)) > 0.00) then
    begin
      FBMatrix_LowIndex[Idx] := j;
      Break;
    end;
  end;
end;
procedure TCodeDeCalcul.ProcessAHighIndexRow(const NoThread, Idx: integer);
var
  j: Integer;
begin
  for j := R_Matrix.MaxRow downto 1 do     //for j := QNbBranches - 1 downto 1 do
  begin
    // Ne pas utiliser la fonction IsZero (lenteur)
    if (Abs(R_Matrix.GetValeur(j, Idx)) > 0.00) then
    begin
      FBMatrix_HighIndex[Idx] := j;
      Break;
    end;
  end;

end;

procedure TCodeDeCalcul.ProcessARowBMatrix(const NoThread, Idx: integer);
var
  ww: double;
  j, k: Integer;
begin
  for j := 1 to Idx do
  begin
    ww := 0.00;
    for k := FBMatrix_LowIndex[Idx] to FBMatrix_HighIndex[Idx] do ww += R_Matrix.GetValeur(k,Idx) * W_mtx[k] * R_Matrix.GetValeur(k,j);
    B_Matrix.SetValeur(Idx, j, WW);
  end;
end;


procedure TCodeDeCalcul.MakeBMatrix();
var
  t           : TDateTime;
  QNbNodes    : integer;
  i, j        : integer;
begin

  AfficherMessage(GetResourceString(rsBUILDMATRICE) + ' ' + MULTI_OR_MONO_THREADED);
  QNbNodes     := GetMaxNode();
  // rechercher les index mini et maxi des valeurs non nulles de RMatri
  SetLength(FBMatrix_LowIndex  , 0);
  SetLength(FBMatrix_HighIndex , 0);
  SetLength(FBMatrix_LowIndex  , QNbNodes + 1);
  SetLength(FBMatrix_HighIndex , QNbNodes + 1);


  AfficherMemoryUsage();
  AfficherMessage('---> '+ GetResourceString(rsFIND_SUM_LIMITS));
  Application.ProcessMessages;
  t := Now();
  for i := 1 to QNbNodes do ProcessALowIndexRow(-1, i);
  for i := 1 to QNbNodes do ProcessAHighIndexRow(-1, i);
  AfficherMessage('---> '+ GetResourceString(rsPRODUIT_MAT_Rt_W_R));
  Application.ProcessMessages;
  for i := 1 to QNbNodes do
  begin
    if (i MOD 100 = 0) then
    begin
      if (Assigned(self.FProcDispProgressionMonoThread)) then self.FProcDispProgressionMonoThread(Format('Matrice de compensation: ' + rsLINEOFNB, [i, QNbNodes]), i, 0, QNbNodes, 100);
      AfficherMessage(Format(rsLINEOFNB, [i, QNbNodes]), False);
    end;
    ProcessARowBMatrix(-1, i);
  end;
  Application.ProcessMessages;
  // remplissage de la symétrie
  for i:=1 to QNbNodes - 1 do
    for j:=1+i to QNbNodes do
      B_Matrix.SetValeur(i,j, B_Matrix.GetValeur(j,i));

  t := Now() - t;
  AfficherMessageErreur(Format('Temps de construction de la matrice B: %.8f', [t * 86400]));
  B_Matrix.RemoveAllZeroes();
  //B_Matrix.CreerRepresentationMatriceEnImage(GetGHTopoDirectory()+ 'B_Matrix.bmp');
end;
function TCodeDeCalcul.MakeFactorisedLMatrix(out QL: TMatriceCreuse): boolean;
var
   t: TDateTime;
   i, j, k   , QNbNodes: integer;
   LowIndex  : array of Integer;
   vv        : double;
   ww, Q2: double;
begin
   Result := false;
   t := Now();
   QNbNodes := GetMaxNode();
   AfficherMessage(Format('%s.MakeFactorisedMatrix() [%s]', [ClassName, MULTI_OR_MONO_THREADED]));
   AfficherMessage('---> '+ GetResourceString(rsFIND_SUM_LIMITS));
   SetLength(LowIndex, 0);
   SetLength(LowIndex, QNbNodes + 1);
   AfficherMemoryUsage();
   try
     // rechercher les index mini et maxi des valeurs non nulles de BMatrix
     // (énorme accélération de la vitesse de calcul)
     // /!\ Ne pas utiliser QL.RecenserLowIndexes(): la matrice QL est modifiée en permanence
     //     et utilise les LowIndex de la matrice B
     for i := 1 to QNbNodes do
     begin
       if (i MOD 200 = 0) then
       begin
         AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, QNbNodes]), False);
         if (Assigned(self.FProcDispProgressionMonoThread)) then self.FProcDispProgressionMonoThread(GetResourceString(rsFACTORISEMATRICE), i, 0, QNbNodes, 100);
       end;
       for j:=1 to QNbNodes do
       begin
         if (abs(B_Matrix.GetValeur(j, i)) > 0) then
         begin
           LowIndex[i] := j;
           Break;
         end;
       end;
     end;
     //*)

     AfficherMessage(GetResourceString(rsDESCENTE));
     // A paralléliser

     for i := 1 to QNbNodes do
     begin
       if (Assigned(self.FProcDispProgressionMonoThread)) then
          self.FProcDispProgressionMonoThread(Format(GetResourceString(rsDESCENTE + ': ' + rsLINEOFNB),[i, QNbNodes]), i, 0, QNbNodes, 100);
       if (i mod PALIERS_INDICATION = 0) then AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, QNbNodes]), False);
       vv := 0;
       for j := 1 to i - 1 do
       begin
         Q2 := QL.GetValeur(i,j);
         vv += Q2 * Q2;
       end;
       QL.SetValeur(i,i, Sqrt(abs(B_Matrix.GetValeur(i,i)) - vv));
       for j := 1 + i to QNbNodes do
       begin
         ww := 0;
         for k := LowIndex[i] to i-1 do
           ww += QL.GetValeur(i,k) * QL.GetValeur(j,k);  // très forte accélération de la vitesse de calcul
         QL.SetValeur(j,i, (B_Matrix.GetValeur(i,j) - ww) / (QL.GetValeur(i,i) + 1e-24));
       end;
     end;
     // Fin de la zone de parallélisation
     //QL.CreerRepresentationMatriceEnImage(GetGHTopoDirectory() + 'L_Matrix.bmp');
     QL.RemoveAllZeroes();
     //QL.Lister();
     t := Now() - t;
     AfficherMessageErreur(Format('Temps de factorisation de la matrice B: %.8f', [t * 86400]));
     Result := True;
   except
   end;
 end;
 procedure TCodeDeCalcul.RemonterSysteme(const QL: TMatriceCreuse; const Axe: TAxeCoordonnees);
 var
   i, j, k   : integer;
   QNbNodes, QNbBranches: integer;
   vv        : double;
   ww        , QQ666, tmp: double;
   QPosPtZero: TPoint3Df;
 begin
   AfficherMessageErreur('RemonterSysteme: ');
   QNbNodes    := GetMaxNode();
   QNbBranches := GetNbBranches();
   QPosPtZero.Empty();
   try
     for i :=  1 to QNbNodes  do
     begin
       case Axe of
         axisX: XX[i].X := 0.00;
         axisY: XX[i].Y := 0.00;
         axisZ: XX[i].Z := 0.00;
       end;
     end;
     // second membre; triangularisation
     AfficherMessage(GetResourceString(rsTRIANGULARISATION));
     for i :=  1 to QNbNodes  do
     begin
       //if (i mod PALIERS_INDICATION = 0) then  AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, NbNodes ]));
       //if (Assigned(self.ProcDispProgression)) then self.ProcDispProgression(Format(GetResourceString(rsTRIANGULARISATION + ': ' + rsLINEOFNB),[i, NbNodes ]), i, 0, NbNodes );
       ww := 0;
       for k:= 1 to i - 1 do
         ww := ww + L_Matrix.GetValeur(i,k) * ExtractValueFromTPoint3Df(Axe, S_Vector[k]);
       QQ666 := L_Matrix.GetValeur(i,i) + 1e-24;
       case Axe of
         axisX: S_Vector[i].X  := (VecteurSecondMembre[i].X - ww) / QQ666;
         axisY: S_Vector[i].Y  := (VecteurSecondMembre[i].Y - ww) / QQ666;
         axisZ: S_Vector[i].Z  := (VecteurSecondMembre[i].Z - ww) / QQ666;
       end;
     end;
     // remontée du système; inconnues recherchées
     AfficherMessage(GetResourceString(rsREMONTEE));
     AfficherMessage('--> Remontée');
     for i := QNbNodes downto 1 do
     begin
       if (i mod PALIERS_INDICATION = 0) then AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, QNbNodes]), False);
       //if (Assigned(self.FProcDispProgressionMonoThread)) then self.FProcDispProgressionMonoThread(Format(GetResourceString(rsREMONTEE + ': ' + rsLINEOFNB),[i, QNbNodes]), i, 0, QNbNodes, 200);
       ww := 0;
       for k := 1 + i to QNbNodes do
         ww := ww + L_Matrix.GetValeur(k,i) * ExtractValueFromTPoint3Df(Axe, XX[k]);
       tmp := (ExtractValueFromTPoint3Df(Axe, S_Vector[i]) - ww) / (L_Matrix.GetValeur(i,i) + 1e-24);
       case Axe of
         axisX: XX[i].X := tmp;
         axisY: XX[i].Y := tmp;
         axisZ: XX[i].Z := tmp;
       end;
     end;
     XX[0] := XX[1];
     // Table des Noeuds
     QPosPtZero := FDocTopo.GetPositionDuPointZero();
     for i := 1 to QNbNodes do
     begin
       FTableNoeuds[i].IDNoeud := i;
       if (i mod 200 = 0) then AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, QNbNodes]), False);
       case Axe of
         axisX: FTableNoeuds[i].Position.X := (XX[i].X - XX[0].X) + QPosPtZero.X;
         axisY: FTableNoeuds[i].Position.Y := (XX[i].Y - XX[0].Y) + QPosPtZero.Y;
         axisZ: FTableNoeuds[i].Position.Z := (XX[i].Z - XX[0].Z) + QPosPtZero.Z;
       end;
     end;
     // Passage par cette ligne = c'est OK


     AfficherMessage(Format(GetResourceString(rsCOORDONNEES_OK),[QNbNodes]));
   finally
     AfficherMessage(GetResourceString(rsDEL_TEMP_MATRIX));
   end;
 end;

// Lancer le calcul (nouvelle version)
function  TCodeDeCalcul.CalculComplet(const UseDebug: boolean): boolean;
var
  UneVisee: TUneVisee;
  Branche: TBrancheXYZ;
  EWE: String;
begin
  Result := false;
  AfficherMessage(GetResourceString(rsPURGE_TABLE_BRCH));
  if (Assigned(self.FProcDispProgressionMonoThread)) then self.FProcDispProgressionMonoThread(GetResourceString(rsPURGE_TABLE_BRCH), -1, -1, -1, 100);
  ViderTableJonctionsBranches();
  AfficherMemoryUsage();
  // Purger la table des branches
  try
    // Branche0
    UneVisee.Empty('');
    SetLength(Branche.PointsTopo, 0);              // N
    addViseeAtBranche(Branche, UneVisee);          // N
    Branche.NoBranche    := 0;
    Branche.NomBranche   := 'Point 0';
    Branche.NoeudDepart  := 0;
    Branche.NoeudArrivee := 0;
    AddBranche(Branche);

    AfficherMessage(Format('%s.CalculComplet',[self.ClassName]));
    Result := False;
    AfficherMessage('--> Jonctions');
    RecenserJonctions();
    AfficherMessage('--> Branches');

    RecenserBranches();
    AfficherMessage('--> Accroissements');
    CalculerAccroissements();  // calcul des accroissements des branches

    AfficherMessage('--> Noeuds');
    CalculerCoordNoeuds();     // calcul matriciel des coordonnées des noeuds

    AfficherMessage('--> Repartition');
    RepartirEcarts();          // répartition parallèle des écarts

    AfficherMessage('--> Contours');
    CalculContoursGaleriesExtended(EWE, false);  // calcul des contours des galeries

    AfficherMessage('--> Antennes');
    TraiterViseesEnAntenne();
    AfficherMessage('--> Vidage');
    ViderTableJonctionsBranches();
    AfficherMessage('Terminé');

    SetLength(FBMatrix_LowIndex ,0);
    SetLength(FBMatrix_HighIndex,0);

    AfficherMemoryUsage();
    Result := True;
  except
    //on E: Exception do
    pass;
  end;
end;


function TCodeDeCalcul.GetNoeud(const Serie, Station: integer): integer;
var
  i   : integer;
  Jonc: TJonctionXYZ;
begin
  Result := -1;
  for i := 0 to GetNbJonctions() - 1 do
  begin
    Jonc := GetJonction(i);
    if ((Serie = Jonc.NoSer) and (Station = Jonc.NoSt)) then Exit(i);   // nouvelle méthode
  end;
end;



function TCodeDeCalcul.CalculerCoordNoeuds(): boolean;
var
  T0, T1: TDateTime;
  tm: TDateTime;
  t: integer;
  Branche : TBrancheXYZ;
  QNbBranches, QNbNodes: integer;
  WU: String;
begin
  Result := false;
  T0 := Now();
  tm := Now();
  QNbBranches := GetNbBranches();
  QNbNodes    := GetMaxNode();
  InitialiseMatrices(QNbBranches, QNbNodes);
  SetLength(FTableNoeuds, 0);
  SetLength(FTableNoeuds, QNbNodes+1);
  AfficherMessage('');
  AfficherMessage(GetResourceString(rsBIND_MATRICIAL_PROBLEM));
  AfficherMessage('=================================');
  AfficherMessage(GetResourceString(rsBIND_INCIDENCE_MATRIX));
  MakeRMatrix(); // matrice d'incidence (assemblage)
  MakeWMatrix(); // matrice de pondération (diagonale)
  MakeBMatrix(); // matrice de compensation
  MakeVectorSecondMembre(); // second membre

  AfficherMessage('');
  AfficherMessage(GetResourceString(rsFACTORISEMATRICE));
  // factorisation de la matrice de compensation
  MakeFactorisedLMatrix(L_Matrix);
  AfficherMessage('--> MakeFactorisedLMatrix');
  tm := Now();
  AfficherMessage(GetResourceString(rsCALC_COORDS_NOEUDS));

  // version monocoeur
  SolveAxis(axisX);
  SolveAxis(axisY);
  SolveAxis(axisZ);
  // contrôle coordonnées noeuds
  (*
  Nb := High(FTableNoeuds);
  AfficherMessageErreur(Format('Coordonnées des %d noeuds', [Nb]));
  for i := 1 to Nb do
  begin
    AfficherMessageErreur(Format('%d: %.3f, %.3f, %.3f', [i, FTableNoeuds[i].Position.X, FTableNoeuds[i].Position.Y, FTableNoeuds[i].Position.Z]));
  end;
  //*)
  tm := Now() - tm;
  // assignation des écarts
  for t:=1 to QNbBranches - 1 do //GetNbBranches() - 1 do
  begin
    Branche := GetBranche(t);
    Branche.CoordsDepart  := FTableNoeuds[Branche.NoeudDepart].Position;
    Branche.CoordsArrivee := FTableNoeuds[Branche.NoeudArrivee].Position;
    PutBranche(t,Branche);
  end;
  AfficherMessage('');
  // libération mémoire
  //-- finalisation
  AfficherMessage(GetResourceString(rsFREE_TEMP_VARS));
  FinalizeMatrices();
  T1 := Now();
  AfficherMessageErreur(DatePascalToDateHeureCondensee(T1-T0));
  result := true;
end;
// répartition des écarts de fermeture entre les noeuds
procedure TCodeDeCalcul.ProcessRepartitionEcartsOfBranche(const NoThread, Idx: integer);
var
  Branche : TBrancheXYZ;
  EcartXYZ: TPoint3Df;
  EpsXYZ  : TPoint3Df;
  QCoord0 : TPoint3Df;

  //EcartX, EcartY, EcartZ,
  R, DevelBranche: Double;
  //EpsX, EpsY, EpsZ: double;

  //XXo, YYo, ZZo: double;
  s: Integer;
  V1, V2, V: TUneVisee;
begin
  Branche := GetBranche(Idx);
  EcartXYZ.setFrom((Branche.CoordsArrivee.X - Branche.CoordsDepart.X) - Branche.Accroissement.X,
                   (Branche.CoordsArrivee.Y - Branche.CoordsDepart.Y) - Branche.Accroissement.Y,
                   (Branche.CoordsArrivee.Z - Branche.CoordsDepart.Z) - Branche.Accroissement.Z);

  //EcartX := (Branche.XArrivee - Branche.XDepart) - Branche.DeltaX;
  //EcartY := (Branche.YArrivee - Branche.YDepart) - Branche.DeltaY;
  //EcartZ := (Branche.ZArrivee - Branche.ZDepart) - Branche.DeltaZ;
  // calcul de R
  R := 1e-10;
  for s := 0 to High(Branche.PointsTopo) do  //Branche.PointsTopo.GetNbElements() - 1do
  begin
    V := Branche.PointsTopo[s];
    //R += Hypot3D(V.DeltaX, V.DeltaY, V.DeltaZ);
    R += V.AccroissXYZ.getNorme();
  end;
  DevelBranche := R;
  // mettre l'azimut de V[1] dans V[0]
  if (High(Branche.PointsTopo) > 0) then //if (Branche.PointsTopo.Count > 1) then
  begin
    V1 := Branche.PointsTopo[0];   //V1 := GetBrStation(i,0);
    V2 := Branche.PointsTopo[1];   //V2 := GetBrStation(i,1);
    V1.Azimut  :=  V2.Azimut;
    Branche.PointsTopo[0] := V1;   // PutBrStation(i, 0, V1);
  end;
  // calcul de répartition
  EpsXYZ.setFrom(EcartXYZ.X / DevelBranche, EcartXYZ.Y / DevelBranche, EcartXYZ.Z / DevelBranche);
  //EpsX := EcartX / DevelBranche;
  //EpsY := EcartY / DevelBranche;
  //EpsZ := EcartZ / DevelBranche;

  R    := 1e-10;
  QCoord0 := Branche.CoordsDepart;
  //XXo  := Branche.XDepart;
  //YYo  := Branche.YDepart;
  //ZZo  := Branche.ZDepart;
  for s := 0 to High(Branche.PointsTopo) do //for s := 0 to Branche.PointsTopo.GetNbElements() - 1 do
  begin
    V := Branche.PointsTopo[s]; //V :=GetBrStation(i, s);
    //R += Hypot3D(V.DeltaX, V.DeltaY, V.DeltaZ);
    R += V.AccroissXYZ.getNorme();
    QCoord0.X += V.AccroissXYZ.X;//V.DeltaX;
    QCoord0.Y += V.AccroissXYZ.Y;//V.DeltaY;
    QCoord0.Z += V.AccroissXYZ.Z;//V.DeltaZ;


    //XXo += V.DeltaX;       //   XXo := XXo + V.DeltaX;
    //YYo += V.DeltaY;       //   YYo := YYo + V.DeltaY;
    //ZZo += V.DeltaZ;       //   ZZo := ZZo + V.DeltaZ;

    V.AccroissXYZ.X := QCoord0.X + R * EpsXYZ.X;  // V.DeltaX := QCoord0.X + R * EpsXYZ.X;
    V.AccroissXYZ.Y := QCoord0.Y + R * EpsXYZ.Y;  // V.DeltaY := QCoord0.Y + R * EpsXYZ.Y;
    V.AccroissXYZ.Z := QCoord0.Z + R * EpsXYZ.Z;  // V.DeltaZ := QCoord0.Z + R * EpsXYZ.Z;





    //V.DeltaX :=  XXo + R * EpsX;
    //V.DeltaY := YYo + R * EpsY;
    //V.DeltaZ := ZZo + R * EpsZ;

    Branche.PointsTopo[s] := V; //PutBrStation(i, s, V);
  end;
  PutBranche(Idx, Branche);
end;

procedure TCodeDeCalcul.RepartirEcarts();
var
  i, QNbBranches: integer;
  t: TDateTime;
begin;
  AfficherMessage('');
  AfficherMessage(GetResourceString(rsREPARTIR_ECARTS));
  QNbBranches := GetNbBranches();
  AfficherMessageErreur(Format('Deformation du reseau: %d branches', [QNbBranches]));
  AfficherMessageErreur('=========================================');
  //AfficherMessageErreur('Branche; X1; Y1; Z1; X2; Y2; Z2; Devel; EcartX; EcartY; EcartZ; EcartTotal; EpsX; EpsY; EpsZ; EpsTotal');
  t := Now();
  AfficherMessageErreur(format('%s.RepartirEcarts(%d) [Monothreaded]', [classname, QNbBranches]));
  for i:=1 to QNbBranches - 1 do ProcessRepartitionEcartsOfBranche(-1, i);
  t := now() - t;
  AfficherMessageErreur(Format('Temps de calcul des accroissements des %d branches: %.8f sec', [QNbBranches, t * 86400]));
  AfficherMessageErreur('=========================================');
end;


// matrice de pondération
procedure TCodeDeCalcul.MakeWMatrix();
var
  i, QNbBranches: integer;
begin
  QNbBranches := GetNbBranches();
  SetLength(W_mtx, 0);
  SetLength(W_mtx, QNbBranches + 1);
  for i:=1 to QNbBranches - 1 do W_mtx[i] := Getbranche(i).Rigidite;  // paramètre de raideur de la branche
end;
// second membre
procedure TCodeDeCalcul.MakeVectorSecondMembre();
var
  i,k    , QNbBranches, QNbNodes: integer;
  wwx, wwy, wwz, EWE: double;
  MyBranche: TBrancheXYZ;
begin;
  AfficherMessageErreur('--> MakeVectorSecondMembre()');
  QNbBranches := GetNbBranches();
  QNbNodes    := GetMaxNode();
  for i := 1 to QNbNodes do
  begin
    wwx := 0.00;
    wwy := 0.00;
    wwz := 0.00;
    for k := 1 to QNbBranches - 1 do
    begin
      EWE := R_Matrix.GetValeur(k,i) *  W_mtx[k];
      if (k = 1) then  // sécurisation branche 1
      begin
        MyBranche.Accroissement.setFrom(0.01, 0.01, 0.01);
      end;
      MyBranche := GetBranche(k);
      wwx += EWE * MyBranche.Accroissement.X; //A[k].X;
      wwy += EWE * MyBranche.Accroissement.Y; //A[k].Y;
      wwz += EWE * MyBranche.Accroissement.Z; //A[k].Z;
    end;

    VecteurSecondMembre[i].setFrom(wwx, wwy, wwz);
    //VecteurSecondMembre[i].Z := wwz;
  end;
  // on peut jeter la matrice R
  try
    R_Matrix.Finaliser();
  finally
    FreeAndNil(R_Matrix);
  end;
end;

procedure TCodeDeCalcul.SolveAxis(const Axe: TAxeCoordonnees);
var
  i, Nb: Integer;
  QFileName: TStringDirectoryFilename;
begin
  AfficherMessage(GetResourceString(rsAXIS) + Chr(88 + Ord(Axe)));
  QFileName := GetGHTopoDirectory() + 'Matrice_B.mtx';
  AfficherMessageErreur(GetResourceString(rs2NDMEMBER));
  AfficherMessageErreur(GetResourceString(rsNODECOORDINATES));
  RemonterSysteme(L_Matrix, Axe);


end;


(*

procedure TCodeDeCalcul.SolveAxisX();
begin
  AfficherMessageErreur('SolveAxis X');
  SolveAxis(axisX);
end;

procedure TCodeDeCalcul.SolveAxisY();
begin
  AfficherMessageErreur('SolveAxis Y');
  SolveAxis(axisY);
end;

procedure TCodeDeCalcul.SolveAxisZ();
begin
  AfficherMessageErreur('SolveAxis Z');
  SolveAxis(axisZ);
end;
//*)
function TCodeDeCalcul.ExtractValueFromTPoint3Df(const Axe: TAxeCoordonnees; const P: TPoint3Df): double;
begin
  case Axe of
    axisX: Result := P.X;
    axisY: Result := P.Y;
    axisZ: Result := P.Z;
  end;
end;

// calcul des contours de galeries (nouvelle version)
procedure TCodeDeCalcul.ProcessABrancheToBDDEntites(const NoThread, Idx: integer);
var
  MyBranche: TBrancheXYZ;
  function TraiterBranche(const QBranche: TBrancheXYZ; const Br: integer): boolean;
  var
    TabVisee    : array of TUneVisee; //TStation;
    St          : Integer;
    AlphaD      : Double;
    qCosAlphaD  : double;
    qSinAlphaD  : double;
    NbViseesInBrche: Integer;
    QEntite     : TBaseStation;
    Cnt         , QNbExpes: integer;
    QEntrance   : TEntrance;
    AReseau     : TReseau;
    MyExpe      : TExpe;
    EWE1, EWE2: Boolean;
  begin
    Result := False;
    try
      NbViseesInBrche := 1 + High(QBranche.PointsTopo); // QBranche.PointsTopo.GetNbElements();
      Cnt := 0;
      AReseau := FDocTopo.GetReseau(QBranche.NoReseau);
      SetLength(TabVisee, 0);
      SetLength(TabVisee, 2 + NbViseesInBrche);
      for St:= 0 to NbViseesInBrche - 1 do TabVisee[St] := QBranche.PointsTopo[st];
      //AfficherMessage(Format('----> Points OK - Idx max TabVisee: %d .. %d; NbViseesInBrche = %d', [Low(TabVisee), high(TabVisee), NbViseesInBrche]));
      TabVisee[0].LD := TabVisee[1].LD;
      TabVisee[0].LG := TabVisee[1].LG;
      TabVisee[0].HZ := TabVisee[1].HZ;
      TabVisee[0].HN := TabVisee[1].HN;
      TabVisee[0].TypeVisee := TabVisee[1].TypeVisee;
      if (NbViseesInBrche = 1) then
      begin
        TabVisee[1]   :=  TabVisee[0];
      end
      else
      begin
        TabVisee[NbViseesInBrche]        := TabVisee[NbViseesInBrche - 1];
        //TabVisee[NbViseesInBrche].DeltaX := TabVisee[NbViseesInBrche - 1].DeltaX + (TabVisee[NbViseesInBrche - 1].DeltaX - TabVisee[NbViseesInBrche - 2].DeltaX);
        //TabVisee[NbViseesInBrche].DeltaY := TabVisee[NbViseesInBrche - 1].DeltaY + (TabVisee[NbViseesInBrche - 1].DeltaY - TabVisee[NbViseesInBrche - 2].DeltaY);
        TabVisee[NbViseesInBrche].AccroissXYZ.X := TabVisee[NbViseesInBrche - 1].AccroissXYZ.X + (TabVisee[NbViseesInBrche - 1].AccroissXYZ.X - TabVisee[NbViseesInBrche - 2].AccroissXYZ.X);
        TabVisee[NbViseesInBrche].AccroissXYZ.Y := TabVisee[NbViseesInBrche - 1].AccroissXYZ.Y + (TabVisee[NbViseesInBrche - 1].AccroissXYZ.Y - TabVisee[NbViseesInBrche - 2].AccroissXYZ.Y);

      end;
      // calcul contours
      QNbExpes := FDocTopo.GetNbExpes();
      for St := 1 to NbViseesInBrche - 1 do
      begin
        try
          if (St = 1) then
          begin
            AlphaD := CalculerAngleBissecteur(TabVisee[St].AccroissXYZ.X - TabVisee[St-1].AccroissXYZ.X,
                                              TabVisee[St].AccroissXYZ.Y - TabVisee[St-1].AccroissXYZ.Y,
                                              TabVisee[St].AccroissXYZ.X - TabVisee[St-1].AccroissXYZ.X,
                                              TabVisee[St].AccroissXYZ.Y - TabVisee[St-1].AccroissXYZ.Y);
          end;
            sincos(AlphaD, qSinAlphaD, qCosAlphaD);
            if (FDocTopo.GetIdxCodeByNumero(TabVisee[St].Code) = -1) then
            begin
              AfficherMessage(Format('*** [WARNING] Branche: %d - St: %d - Code %d incorrect mis à 1', [Br, St, TabVisee[St].Code]));
              TabVisee[St].Code := 1;
              QEntite.DateLeve  := Now();
            end;
            QEntite.eCode       := TabVisee[St].Code;
            if (FDocTopo.GetIdxExpeByNumero(TabVisee[St].Expe) = -1) then
            begin
              AfficherMessage(Format('*** [WARNING] Branche: %d - St: %d - Expe %d incorrecte mise à 1', [Br, St, TabVisee[St].Expe]));
              TabVisee[St].Expe := 1;
              QEntite.DateLeve       := Now();
            end;
            QEntite.eExpe       := TabVisee[St].Expe;
            QEntite.eSecteur    := TabVisee[St].IDSecteur;
            QEntite.eEntrance   := QBranche.NoEntranceRatt;
            QEntite.eReseau     := QBranche.NoReseau;
            QEntite.Type_Entite := TabVisee[St].TypeVisee;

            MyExpe                  := FDocTopo.GetExpeByNumero(TabVisee[St].Expe);
            {$WARNING: TEXpe.DateExpe à implementer}
            QEntite.DateLeve        := GetSecuredDate(MyExpe.AnneeExpe, MyExpe.MoisExpe, MyExpe.JourExpe);
            QEntite.eEntrance       := QBranche.NoEntranceRatt;
            QEntite.Entite_Serie    := QBranche.NoSerie;
            QEntite.Entite_Station  := TabVisee[St].NoVisee;
            // données originales
            QEntite.oLongueur       := TabVisee[St].Longueur;
            QEntite.oAzimut         := TabVisee[St].Azimut;
            QEntite.oPente          := TabVisee[St].Pente;
            QEntite.oLG             := TabVisee[St].LG;
            QEntite.oLD             := TabVisee[St].LD;
            QEntite.oHZ             := TabVisee[St].HZ;
            QEntite.oHN             := TabVisee[St].HN;
            // centerline
            QEntite.PosExtr0.setFrom(TabVisee[St-1].AccroissXYZ.X, TabVisee[St-1].AccroissXYZ.Y, TabVisee[St-1].AccroissXYZ.Z);
            QEntite.PosStation.setFrom(TabVisee[St].AccroissXYZ.X, TabVisee[St].AccroissXYZ.Y, TabVisee[St].AccroissXYZ.Z);
            // habillage
            QEntite.PosOPG.X := QEntite.PosExtr0.X - TabVisee[St-1].LG * qCosAlphaD;
            QEntite.PosOPG.Y := QEntite.PosExtr0.Y - TabVisee[St-1].LG * qSinAlphaD;
            QEntite.PosOPD.X := QEntite.PosExtr0.X + TabVisee[St-1].LD * qCosAlphaD;
            QEntite.PosOPD.Y := QEntite.PosExtr0.Y + TabVisee[St-1].LD * qSinAlphaD;

            QEntite.PosOPD.Z := QEntite.PosExtr0.Z + TabVisee[St-1].HZ;
            QEntite.PosOPG.Z := QEntite.PosExtr0.Z - TabVisee[St-1].HN;
            AlphaD := CalculerAngleBissecteur(TabVisee[St].AccroissXYZ.X - TabVisee[St-1].AccroissXYZ.X,
                                              TabVisee[St].AccroissXYZ.Y - TabVisee[St-1].AccroissXYZ.Y,
                                              TabVisee[St+1].AccroissXYZ.X - TabVisee[St].AccroissXYZ.X,
                                              TabVisee[St+1].AccroissXYZ.Y - TabVisee[St].AccroissXYZ.Y);
            SinCos(AlphaD, qSinAlphaD, qCosAlphaD);
            QEntite.PosPG.X := QEntite.PosStation.X - TabVisee[St].LG * qCosAlphaD;
            QEntite.PosPG.Y := QEntite.PosStation.Y - TabVisee[St].LG * qSinAlphaD;
            QEntite.PosPD.X := QEntite.PosStation.X + TabVisee[St].LD * qCosAlphaD;
            QEntite.PosPD.Y := QEntite.PosStation.Y + TabVisee[St].LD * qSinAlphaD;
            QEntite.PosPD.Z := QEntite.PosStation.Z + TabVisee[St].HZ;
            QEntite.PosPG.Z := QEntite.PosStation.Z - TabVisee[St].HN;
            QEntite.CouleurDegrade := clBlue;                                                        // couleur pour dégradé
            EWE1 := Pos(LowerCase(KEYWORD_POI_TODO), LowerCase(TabVisee[St].Commentaires)) > 0;
            EWE2 := Pos(LowerCase(KEYWORD_POI_DONE), LowerCase(TabVisee[St].Commentaires)) > 0;

            QEntite.IsPOI := (EWE1 OR EWE2);    // Points d'intérêt
            QEntite.Highlighted   := false;
            QEntite.IDTerrain     := TabVisee[St].IDTerrainStation;
            QEntite.oCommentaires := TabVisee[St].Commentaires;


          case QEntite.Type_Entite of
            tgENTRANCE:  // les entrées ne sont pas des entités comme les autres et font l'objet d'une liste séparée.
              begin
                pass;
                Inc(Cnt);
              end;
            tgDEFAULT,
            tgFOSSILE,
            tgVADOSE,
            tgENNOYABLE,
            tgSIPHON,
            tgSURFACE,
            tgTUNNEL,
            tgMINE:
              begin
                FBDDEntites.AddEntiteVisee(QEntite);
                Inc(Cnt);
              end;
          else
            AfficherMessage('Type de entité inconnu');
          end;
        except
        end;
      end;   // for Br
      Result := (NbViseesInBrche = (Cnt + 1)); // Valeur de retour: OK si égalité entre le nombre de visées initial et le nombre de visées effectivement ajouté.
    except
    end;
  end;

begin
  MyBranche := GetBranche(Idx);
  if (Not TraiterBranche(MyBranche, Idx)) then AfficherMessageErreur(Format('*** Erreur pour la branche %d - %s', [Idx, MyBranche.NomBranche]));
end;

procedure TCodeDeCalcul.CalculContoursGaleriesExtended(const FichierTOP: string; const DoGenerateFile: boolean);
var
  VA          : TViseeAntenne;
  Br          , NbreBranches: Integer;
  EPSG        : TLabelSystemesCoordsEPSG;
  MyBranche   : TBrancheXYZ;
  t: TDateTime;
begin;
  AfficherMessage('');
  AfficherMessage('CalculContoursGaleriesExtended: ' + FichierTOP);
  AfficherMessage('--------------------------------');
  NbreBranches := GetNbBranches();
  t := Now();
  // démarrage de l'objet TBDDEntites
  try
    if (Not FBDDEntites.Initialiser(FDocTopo.GetPositionDuPointZero(), FCouleurZMini, FCouleurZMaxi)) then Exit;
    EPSG := FDocTopo.GetCodeEPSGSystemeCoordonnees;
    FBDDEntites.SetCodeEPSG(EPSG.CodeEPSG);
    recopierTablesEntreesReseauxSecteursCodesExpes();
    // construction de la liste des visées
    AfficherMessage(GetResourceString(rsSCAN_BRCHS));
    for Br:= 1 to NbreBranches - 1 do ProcessABrancheToBDDEntites(-1, Br);
    AfficherMessageErreur(rsSCAN_BRCHS + ' OK');
    t := now() - t;
    AfficherMessageErreur('RecenserDates()');
    FBDDEntites.RecenserDates();   // recenser les dates
    AfficherMessageErreur(Format('Temps de calcul des %d contours des branches jj: %.8f sec', [NbreBranches, t * 86400]));
    AfficherMessage('Calcul contours galeries OK');
    if (DoGenerateFile) then pass;
  finally
  end;
  // provisoire: jonctions
  EnvoyerJonctionsVersBDDEntites();
  AfficherMessage('--------------------------------');
end;

procedure TCodeDeCalcul.ProcessAViseeEnAntenne(const NoThread, Idx: integer);
var
  QVA: TViseeAntenne;
  QEntite: TBaseStation;
  function CalcViseeAntenne(const VA: TViseeAntenne; out   OutEE: TBaseStation): boolean;
  var
    VS      : TUneVisee;
    DC      : TPoint3Df;
    DP      : double;
    EE      : TBaseStation;
    MyCode  : TCode;
    MyExpe  : TExpe;
  begin
    result := false;
    DC.Empty();
    DP := 0.00;
    if (FBDDEntites.GetEntiteViseeFromSerSt(VA.SerieDepart, VA.PtDepart, EE)) then
    begin
      DC := EE.PosStation; //
      //DX := EE.PosStation.X;
      //DY := EE.PosStation.Y;
      //DZ := EE.PosStation.Z;
      // les codes et expés de l'antenne héritent de ceux de la station d'accrochage
      MyCode := FDocTopo.GetCodeByNumero(EE.eCode);
      MyExpe := FDocTopo.GetExpeByNumero(EE.eExpe);
    end
    else
    begin
      exit(false);
    end;
    VS.IDSecteur        := 0; // VA.Secteur; // TODO: Secteur à implémenter
    VS.Code             := EE.eCode;
    VS.Expe             := EE.eExpe;
    VS.Longueur         := VA.Longueur;
    VS.Azimut           := VA.Azimut;
    VS.Pente            := VA.Pente;
    VS.LG               := 0.00;
    VS.LG               := 0.00;
    VS.HZ               := 0.00;
    VS.HN               := 0.00;
    VS.Commentaires     := ''; //VA.Commentaires;
    VS.IDTerrainStation := ''; //VA.IDTerrainStation;
    VS.TypeVisee        := tgVISEE_RADIANTE;
    CalculerVisee(VS, MyCode, MyExpe, DC, DP);
    DC.setFrom(EE.PosStation.X + VS.AccroissXYZ.X,
               EE.PosStation.Y + VS.AccroissXYZ.Y,
               EE.PosStation.Z + VS.AccroissXYZ.Z);

    //DX := EE.PosStation.X + VS.AccroissXYZ.X; // DX := EE.PosStation.X + VS.DeltaX;
    //DY := EE.PosStation.Y + VS.AccroissXYZ.Y; // DY := EE.PosStation.Y + VS.DeltaY;
    //DZ := EE.PosStation.Z + VS.AccroissXYZ.Z; // DZ := EE.PosStation.Z + VS.DeltaZ;
    //******************************
    // TODO: Dans GHCaveDraw, les antennes ne doivent servir que de guides et
    //       n'ont pas à être capturées
    OutEE.Entite_Serie    := -VA.SerieDepart; // -QNo;
    OutEE.Entite_Station  :=  VA.PtDepart;//0;
    // code et expé (hérités de ceux de la station d'accrochage)
    OutEE.eCode       := EE.eCode;
    OutEE.eExpe       := EE.eExpe;
    OutEE.eSecteur    := VA.Secteur;
    OutEE.eEntrance   := VA.EntranceRatt;
    OutEE.eReseau     := VA.Reseau;
    OutEE.Type_Entite := tgVISEE_RADIANTE;
    OutEE.DateLeve    := Now();
    OutEE.Enabled         := True;   // drapeau
    // données originales
    OutEE.oLongueur       := VA.Longueur;
    OutEE.oAzimut         := VA.Azimut;
    OutEE.oPente          := VA.Pente;
    OutEE.oLG             := 0.00;
    OutEE.oLD             := 0.00;
    OutEE.oHZ             := 0.00;
    OutEE.oHN             := 0.00;
    // centerline
    OutEE.PosExtr0   := EE.PosStation;
    OutEE.PosStation := DC; //.setFrom(DX, DY, DZ);
    // habillage
    OutEE.PosOPG := EE.PosStation;
    OutEE.PosOPD := EE.PosStation;
    OutEE.PosPG := DC; //.setFrom(DX, DY, DZ);
    OutEE.PosPD := DC; //.setFrom(DX, DY, DZ);
    // couleur par défaut
    OutEE.CouleurDegrade:= FPalette256.GetColorByIndex(MyExpe.IdxCouleur); //clGray ;
    // commentaires
    OutEE.IDTerrain     := ''; //VA.IDTerrainStation;
    OutEE.oCommentaires := ''; //VA.Commentaires;
    OutEE.IsPOI         := false;
    // passage ici = OK
    result := true;
  end;
begin
  QVA    := FDocTopo.GetViseeAntenne(Idx);
  if (CalcViseeAntenne(QVA, QEntite)) then FBDDEntites.AddEntiteAntenne(QEntite);

end;

function TCodeDeCalcul.TraiterViseesEnAntenne(): boolean;
var
  t              : TDateTime;
  n, NbAntennes : integer;
begin
  //AfficherMessageErreur(format('%s.TraiterViseesEnAntenne()', [classname]));
  result := false;
  NbAntennes := FDocTopo.GetNbAntennes();
  t := now();
  //NbAntennes := FDocTopo.GetNbAntennes();
  AfficherMessage(Format(GetResourceString(rsCALCUL_ANTENNES), [NbAntennes]));
  if (NbAntennes > 0) then
  begin
    for n := 1 to NbAntennes - 1 do
    begin
      ProcessAViseeEnAntenne(-1, n);
      if ((Assigned(self.FProcDispProgressionMonoThread)) and ((n MOD 250) = 0)) then self.FProcDispProgressionMonoThread(Format(GetResourceString(rsCALCUL_ANTENNES), [n]), n, 0, NbAntennes - 1, 1000);
    end
  end else
  begin
    AfficherMessage(' --> No antenna shots');
  end;
  t := Now() - t;
  AfficherMessageErreur(Format('Temps de calcul des %d antennes: %.8f sec', [NbAntennes, t * 86400]));
  result := True;
end;

function TCodeDeCalcul.EnvoyerJonctionsVersBDDEntites(): boolean;
var
  n, i, ND: Integer;
  J       : TJonctionXYZ;
begin
  result := false;
  n := GetNbJonctions();
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    J := GetJonction(i);
    ND := GetNoeud(J.NoSer, J.NoSt);
    if (ND >= 0) then
    begin
      J.Position := FTableNoeuds[i].Position;
      //J. X := FTableNoeuds[i].XN;
      //J.Y := FTableNoeuds[i].YN;
      //J.Z := FTableNoeuds[i].ZN;
      FBDDEntites.AddJonction(J);
    end;
  end;
  result := true;
end;

// générer un fichier de synthèse (TAB simplifié avec coordonnées)
function TCodeDeCalcul.GenererFichierSyntheseAvecCoordonnees(const FileName: string): boolean;
var
  WU    : String;
  fp    : TextFile;
  i, j  : Integer;

  MyEntrance: TEntrance;
  MySerie   : TObjSerie;
  MyEntite: TBaseStation;
  SD, PD: Integer;
  IxdSerie: Integer;
  procedure WrtEnTeteSection(const S: string);
  begin
    WriteLn(fp, '');
    WriteLn(fp, Format('[%s]', [S]));
    WriteLn(fp, '');
  end;
  procedure WrtLn(const S: string); inline;
  begin
    WriteLn(fp, S);
  end;
  procedure WrtEntite(const S: TBaseStation);
  begin
    if ((S.Entite_Serie < 0) or (S.Entite_Station < 0)) then Exit;
    WrtLn(Format(FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
                 FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
                 FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 +
                 FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 +
                 FORMAT_STRING + #9 +
                 FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 + FORMAT_NB_REAL_3_DEC + #9 +
                 FORMAT_STRING,
                [S.Entite_Serie, S.Entite_Station,
                 S.eCode, S.eExpe,
                 S.oLongueur, S.oAzimut, S.oPente,
                 S.oLD, S.oLG, S.oHZ, S.oHN,
                 S.IDTerrain,
                 S.PosStation.X, S.PosStation.Y, S.PosStation.Z,
                 S.oCommentaires
                ]));
  end;
begin
  Result := False;
  WU := Format('%s.GenererFichierSyntheseAvecCoordonnees: %s', [ClassName, FileName]);
  AfficherMessage(WU);
  AssignFile(fp, FileName);
  try
    ReWrite(fp);
    // les entrées
    WrtEnTeteSection('Entrances');
    for i := 0 to FDocTopo.GetNbEntrances() - 1 do
    begin
      MyEntrance := FDocTopo.GetEntrance(i);
    end;
    // les séries
    WrtEnTeteSection('Series');
    for i := 1 to FDocTopo.GetNbSeries - 1 do // série 0 ignorée
    begin
      MySerie  := FDocTopo.GetSerie(i);
      SD       := MySerie.GetNoSerieDep();
      PD       := MySerie.GetNoPointDep();
      IxdSerie := MySerie.GetNumeroDeSerie();
      WrtLn(Format(FORMAT_NB_INTEGER + #9 +
                   FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
                   FORMAT_NB_INTEGER + #9 + FORMAT_NB_INTEGER + #9 +
                   FORMAT_STRING,
                   [MySerie.GetNumeroDeSerie() * (-1),
                    SD, PD,
                    MySerie.GetNoSerieArr, MySerie.GetNoPointArr,
                    MySerie.GetNomSerie
                   ]));
      // première visée
      FBDDEntites.GetEntiteViseeFromSerSt(MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), MyEntite);
      WrtEntite(MyEntite);
      for j := 1 to MySerie.GetNbVisees() - 1 do
      begin
        FBDDEntites.GetEntiteViseeFromSerSt(IxdSerie, j, MyEntite);
        WrtEntite(MyEntite);
      end;
      WrtLn('');
    end;
    AfficherMessage('--- Generation OK');
    Result := True;
  finally
    CloseFile(fp);
  end;
end;


procedure TCodeDeCalcul.recopierTablesEntreesReseauxSecteursCodesExpes();
var
  i           : Integer;
begin
  AfficherMessage('recopierTablesEntreesReseauxSecteursCodesExpes');
  FBDDEntites.ClearTableEntrances();
  FBDDEntites.ClearTableReseaux();
  FBDDEntites.ClearTableSecteurs();
  FBDDEntites.ClearTableCodes();
  FBDDEntites.ClearTableExpes();
  for i := 0 to FDocTopo.GetNbEntrances() - 1   do FBDDEntites.AddAEntrance(FDocTopo.GetEntrance(i));
  for i := 0 to FDocTopo.GetNbReseaux() - 1     do FBDDEntites.AddReseau(FDocTopo.GetReseau(i));
  for i := 0 to FDocTopo.GetNbSecteurs() - 1    do FBDDEntites.AddSecteur(FDocTopo.GetSecteur(i));
  for i := 0 to FDocTopo.GetNbCodes() - 1       do FBDDEntites.AddCode(FDocTopo.GetCode(i));
  for i := 0 to FDocTopo.GetNbExpes() - 1       do FBDDEntites.AddExpe(FDocTopo.GetExpe(i));
end;

end.
