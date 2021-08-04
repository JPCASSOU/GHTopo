// 05/05/2016: Réacculturation - Les listes simples passent aux génériques
// 09/05/2016: Il est possible de démarrer une coupe développée depuis n'importe quelle série
// 10/05/2016: Construction des coupes développées par calcul matriciel
// 13/05/2016: Moteur de calcul opérationnel après une mise au point très laborieuse
// 12/06/2016: Cette unité est neutralisée si l'utilisation des génériques est désactivée

unit unitCoupeDeveloppee;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  Classes,
  SysUtils,
  StructuresDonnees,
  ToporobotClasses2012,
  UnitListesSimplesWithGeneriques,
  unitobjetserie,
  UnitClassPalette
  //, UnitBranchesCoupeDeveloppee
  , StdCtrls      // pour ListerSeriesRetenues
  , SVGCanvasUnit // pour export SVG
  , Graphics
  ;
const
  MAX_NB_PRED_SUCC = 50;
  OMEGA            = -1;
type TTypeJonction = (tjENTRANCE, tjCARREFOUR, tjEXTREMITE_CONDUIT);
type TJonctionCoupeDeveloppee = record
  NumeroJonction : Int64;
  TypeJonction   : TTypeJonction;
  IDJonction     : string;
  NoSerie        : integer;
  NoPoint        : integer;
  Abscisse       : double;
  Cote           : double;
end;



type TSommetVisite = record
  IndexSommet: Int64;
  EstVisite  : boolean;
end;
type TListeBranchesEntreDeuxSommets = array of integer;
//******************************************************************************
// Pour éviter des bogues, utilisation de branches sous forme de records
type TBrancheCoupeDeveloppeeAsRecord = record
  NumeroSerie       : integer;
  NumeroReseau      : integer;
  NumeroBranche     : integer;
  NomBranche        : string;
  NumeroNoeudDepart : int64;
  NumeroNoeudArrivee: int64;


  DeltaP            : double;
  DeltaZ            : double;
    // sens de dessin
  SensTraceCoupe    : TSensTraceCoupeDev;
  // visées
  ArrVisees         : array of TUneVisee;
end;

{ TCoupeDeveloppee }
type TCoupeDeveloppee = class
  private
    FDocuToporobot: TToporobotStructure2012;
    // liste des séries
    FListeDesSeries: TListeSimple<TObjSerie>;
    // liste des noeuds
    FListeDesJonctions: TListeSimple<TJonctionCoupeDeveloppee>;
    // liste des branches
    //FListeDesBranches: TListeSimple<TBrancheCoupeDeveloppee>; //TObjectList;
    FListeDesBranches: TListeSimple<TBrancheCoupeDeveloppeeAsRecord>; //TObjectList;

    FPMini : double;
    FPMaxi : double;
    FZMini : double;
    FZMaxi : double;
    //--------------------------------------------------------------------------
    procedure AfficherMatrice(const Matrix: TMatrix; const m, n: integer);
    procedure CalculerCoordNoeuds();
    function  GetIdxJonctionBySerSt(const s, p: integer): integer;

    //--------------------------------------------------------------------------
    procedure AfficherListeJonctions();

    // vérifie si la série a un point d'accrochage connu de la coupe
    function  VerifierSiSerieAUnAntecedentConnu(const S: TObjSerie): integer;
    // vérifie si la série est unique (anti-doublon)
    function  VerifierSiSerieEstUnique(const S: TObjSerie): boolean;

    function RecenserJonctionsEtBranches(): boolean;
    procedure ViderListeBranches();
  public
    function  Initialise(const DT: TToporobotStructure2012): boolean;
    procedure ListerSeriesRetenues(const LSB: TListBox; const Idx: integer);
    procedure ListerBranchesDUneSerie(const LSB: TListBox; const Idx: integer);
    procedure SetMiniMaxi();
    // Les séries
    procedure ViderListeSeries();
    function  GetNbSeries(): integer;
    procedure AddSerie(const S: TObjSerie; const DoOuvrirSerie: boolean);
    function  GetSerie(const Idx: integer): TObjSerie;
    function  GetSerieByIdx(const Idx: integer): TObjSerie;
    procedure PutSerie(const Idx: integer; const S: TObjSerie);
    procedure RemoveSerie(const Idx: integer);
    // Les noeuds (= jonctions)
    function  GetNbJonctions(): integer;
    procedure AddJonction(const S: TJonctionCoupeDeveloppee;
                          const DoPurgeListes: boolean);
    function  GetJonction(const Idx: integer): TJonctionCoupeDeveloppee;
    function  GetJonctionBySerSt(const s,p: integer): TJonctionCoupeDeveloppee;
    procedure PutJonction(const Idx: integer; const S: TJonctionCoupeDeveloppee);

    procedure RemoveJonction(const Idx: integer);
    function  RecenserLesJonctions(): integer;

    // Les branches
    function  RecenserLesBranches(): boolean;
    function  initBranche(const TD: TToporobotStructure2012;
                          const QNumeroBranche: integer;
                          const QNoSerie: integer;
                          const QNumeroReseau: integer;
                          const QNoeudDepart, QNoeudArrivee: integer;
                          const QNomBranche: string): TBrancheCoupeDeveloppee;
    function  GetNbBranches(): integer;
    procedure AddBranche(const S: TBrancheCoupeDeveloppee; const DoCalcPZ: boolean);
    function  GetBranche(const Idx: integer): TBrancheCoupeDeveloppee;
    procedure PutBranche(const Idx: integer; const BRCH: TBrancheCoupeDeveloppee);
    function  FindIndexBrancheNearToXY(const X, Y: double): integer;
    procedure BasculerSensDessinBrancheByIdx(const Idx: integer);

    procedure Finalise;
    //procedure RecalculerLaCoupe;
    function GetCoinBasGauche: TPointCoupeDeveloppee;
    function GetCoinHautDroit: TPointCoupeDeveloppee;
    function  FindAccrochageCoordsPZSerie(const S: TObjSerie): TPointCoupeDeveloppee;
    function  FindIdxSerieNearPoint(const P, Z: double): integer;

    procedure exportVersGHCD(const filename: string);

    function ConstruireLaCoupeDeveloppee(const DoRegenGraphe: boolean): boolean;
    function  GetMaxIdxNode: integer;

    // lister les noeuds et arcs
    procedure ListerLesJonctions();
    // export ODG
    procedure ExportSVG(const FichierSVG: string);
    // sauvegarde de la coupe
    procedure SaveCoupe(const QFilename: string);
    function  LoadCoupe(const QFilename: string): boolean;


end;

implementation
//uses Forms, Dialogs, frmJournal;
{ TCoupeDeveloppee }

const IDX_MIN_JONCTION = 0;
(*
procedure DlgAlert(const S: string);
begin
  ShowMessage(S);
  AfficherMessageErreur(S);
  dlgProcessing.CopierJournalErreurs();
end;
//*)

function CalcDeltaPZ(const Br: TBrancheCoupeDeveloppeeAsRecord): TBrancheCoupeDeveloppeeAsRecord;
begin
  Result.DeltaP  := 0.001;
  Result.FDeltaZ := 0.001;
  Nb := GetNbVisees;
  if (Nb > 0) then
  begin
    for i := 0 to nb - 1 do
    begin
       V := GetVisee(i);
       C := FDocumentToporobot.GetCodeByIndex(V.Code);
       E := FDocumentToporobot.GetExpeByIndex(V.Expe);
       // factorisation du code
       CalculerVisee(V, C, E,
                     AX, AY, 1.00,
                     AZ, AP);
       self.FDeltaP := self.FDeltaP + IIF((FSensTraceCoupe = stcdVERS_DROITE), V.DeltaP, -V.DeltaP);
       self.FDeltaZ := self.FDeltaZ + V.DeltaZ;
       PutVisee(i, V);
    end;
  end;

end;

// Les séries
function TCoupeDeveloppee.GetNbSeries: integer;
begin
  Result := FListeDesSeries.GetNbElements();
end;

procedure TCoupeDeveloppee.AddSerie(const S: TObjSerie; const DoOuvrirSerie: boolean);
var
  qSerie: TObjSerie;
  i, Nb: Integer;
  EWE: TUneVisee;
  Q1, Q2, Q3: Boolean;
begin
  // Source de bugs corrigée
  // COPIER la série et non faire une simple affectation !!!!
  // Création de l'objet série
  qSerie := TObjSerie.Create;
  qSerie.ClearStations;
  // Copie de la série
  qSerie.SetNomObsSerie(S.GetNomSerie,
                        S.GetObsSerie);
  qSerie.SetChanceObstacle(S.GetChance,
                           S.GetObstacle);

  qSerie.SetSeriePtExtremites(S.GetNoSerieDep, S.GetNoPointDep,
                              S.GetNoSerieArr, S.GetNoPointArr);
  // séries raccordées au réseau via la seconde extrémité
  // Si (IdxSerie == NoSerArr) ET (NoPtDep != 0), on ouvre la série ("anti-spirale")
  // Si (IdxSerie != NoSerArr) ET (NoPtDep != 0), on ouvre la série
  // Si (IdxSerie != NoSerArr) ET (NoPtDep == 0), on ne touche à rien

  // Série 12.6 (69 points) se raccorde à la série 115.6 -> (Numéro de série <> Série arrivée); (PtDep <> 0) -> elle doit être ouverte = Arrivée en 12.68
  // Série 63.0 (30 points) se raccorde à la série 62.23 -> (Numéro de série <> Série arrivée); (PtDep = 0)  -> on ne touche à rien
  Q1 := (S.GetNoSerieArr() <> S.GetIndexSerie());      // = Serie arrivée <> numéro de série
  Q2 := (S.GetNoPointDep() <> 0);
  Q3 := (S.GetNoSerieArr() = S.GetIndexSerie());
  if (Q3 and Q2) then qSerie.SetNoPointArr(S.GetNbVisees() - 1);
  if (Q1 and Q2) then
  begin
    qSerie.SetNoSerieArr(S.GetIndexSerie());
    qSerie.SetNoPointArr(S.GetNbVisees() - 1);
  end;


  qSerie.SetIndexSerie(S.GetIndexSerie);
  qSerie.SetNoReseau(S.GetNoReseau);
  qSerie.SetCouleur(S.GetCouleur);
  qSerie.SetAccroissements(S.GetAccroissementX,
                           S.GetAccroissementY,
                           S.GetAccroissementZ,
                           S.GetAccroissementP);
  qSerie.SetRaideur(1.00); // n'intervient pas dans une coupe développée
  // Copie des stations
  Nb := S.GetNbVisees;
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      EWE := S.GetVisee(i);
      qSerie.AddVisee(EWE);
    end;
  end;
  // interdire les séries non raccordées
  Q1 := True; //VerifierSiSerieAUnAntecedentConnu(qSerie) <> 0;
  //AfficherMessage('--- Ancrage ' + BoolToStr(Q1, 'connu', 'inconnu'));
  Q2 := VerifierSiSerieEstUnique(qSerie);
  //AfficherMessage('--- Unique ? ' + BoolToStr(Q2, 'Oui', 'Non'));
  if (Q1 and Q2) then
  begin
    FListeDesSeries.AddElement(qSerie);
  end;
end;


function  TCoupeDeveloppee.GetSerie(const Idx: integer): TObjSerie;
begin
  result := FListeDesSeries.GetElement(Idx);
end;

function TCoupeDeveloppee.GetSerieByIdx(const Idx: integer): TObjSerie;
var
  i, Nb: Integer;
  EWE  : TObjSerie;
begin
  Result := nil;
  Nb := self.GetNbSeries;
  for i := 0 to Nb - 1 do
  begin
    EWE := GetSerie(i);
    if (Idx = EWE.GetIndexSerie) then
    begin
      Result := EWE;
      Exit;
    end;
  end;
end;

procedure TCoupeDeveloppee.PutSerie(const Idx: integer; const S: TObjSerie);
begin
  FListeDesSeries.PutElement(Idx, S);
end;
procedure TCoupeDeveloppee.RemoveSerie(const Idx: integer);
var
  OS: TObjSerie;
begin
  if (FListeDesSeries.Count = 0) then exit;
  try
    try
      OS := self.GetSerie(Idx);
      OS.ClearStations;
      OS.Free;
    except
    end;
  finally
    FListeDesSeries.RemoveElement(Idx);
  end;
end;
// Les noeuds
function TCoupeDeveloppee.GetNbJonctions: integer;
begin
  Result := FListeDesJonctions.Count;
end;

procedure TCoupeDeveloppee.AddJonction(const S: TJonctionCoupeDeveloppee;
                                       const DoPurgeListes: boolean);
var
  WU: TJonctionCoupeDeveloppee;
  i: Integer;
begin
  WU := S;
  WU.Abscisse           := 0.00;
  WU.Cote               := 0.00;
  FListeDesJonctions.AddElement(WU);
end;

function TCoupeDeveloppee.GetJonction(const Idx: integer): TJonctionCoupeDeveloppee;
begin
  Result := FListeDesJonctions.GetElement(Idx);
end;



function TCoupeDeveloppee.GetJonctionBySerSt(const s, p: integer): TJonctionCoupeDeveloppee;
var
  i, Nb  : Integer;
  Jonc: TJonctionCoupeDeveloppee;
  STS: String;
begin
  Result.NumeroJonction := -1;
  Nb := GetNbJonctions;
  for i := 1 to Nb - 1 do
  begin
    Jonc := GetJonction(i);
    if ((s = Jonc.NoSerie) and (p = Jonc.NoPoint)) then        // nouvelle méthode
    begin
      Result := Jonc;
      Exit;
    end;
  end;
end;
function TCoupeDeveloppee.GetIdxJonctionBySerSt(const s, p: integer): integer;
var
  i, Nb  : Integer;
  Jonc: TJonctionCoupeDeveloppee;
  STS: String;
begin
  Result := -1;
  Nb := GetNbJonctions;
  for i := IDX_MIN_JONCTION to Nb - 1 do
  begin
    Jonc := GetJonction(i);
    if ((s = Jonc.NoSerie) and (p = Jonc.NoPoint)) then        // nouvelle méthode
    begin
      Result := i;
      Exit;
    end;
  end;
end;



function TCoupeDeveloppee.GetNbBranches: integer;
begin
  Result := FListeDesBranches.GetNbElements();
end;

procedure TCoupeDeveloppee.AddBranche(const S: TBrancheCoupeDeveloppee; const DoCalcPZ: boolean);
begin
  if (DoCalcPZ) then S.CalcDeltaPZ();
  FListeDesBranches.AddElement(S);
end;

function TCoupeDeveloppee.GetBranche(const Idx: integer): TBrancheCoupeDeveloppee;
begin
  Result := FListeDesBranches.GetElement(Idx);
end;

procedure TCoupeDeveloppee.PutBranche(const Idx: integer; const BRCH: TBrancheCoupeDeveloppee);
begin
  FListeDesBranches.PutElement(Idx, BRCH);
end;

function TCoupeDeveloppee.FindIndexBrancheNearToXY(const X, Y: double): integer;
var
  i, Nb: Integer;
  R, Rmax: double;
  Br: TBrancheCoupeDeveloppee;
  JC1, JC2: TJonctionCoupeDeveloppee;
  v, NbS: Integer;
  qP, qZ: Extended;
  myVisee: TUneVisee;
begin
  AfficherMessage(Format('FindIndexBrancheNearToXY: %.2f, %.2f', [X, Y]));
  Result := -1;
  Rmax   := 1E24;
  Nb := self.GetNbBranches;
  if (Nb = 0) then Exit;
  for i := 0 to Nb-1 do
  begin
    Br  := GetBranche(i);
    JC1 := GetJonction(Br.NumeroNoeudDepart);
    JC2 := GetJonction(Br.NumeroNoeudArrivee);
    NbS := Br.GetNbVisees;
    if (NbS = 0) then
    begin
      qP := 0.50 * (JC1.Abscisse + JC2.Abscisse);
      qZ := 0.50 * (JC1.Cote     + JC2.Cote);
      R  := Hypot2D(X - qP, Y - qZ);
      if (R < RMax) then
      begin
        RMax   := R;
        Result := i;
      end;
    end
    else
    begin
      qP := JC1.Abscisse;
      qZ := JC1.Cote;
      for v := 0 to NbS - 1 do
      begin
        myVisee := Br.GetVisee(v);
        qP += myVisee.DeltaP * iif((Br.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
        qZ += myVisee.DeltaZ;
        R  := Hypot2D(X - qP, Y - qZ);
        if (R < RMax) then
        begin
          RMax := R;
          Result := i;
        end;
      end;
    end;
  end;
  AfficherMessage(Format('FindIndexBrancheNearToXY: %.2f, %.2f = %d', [X, Y, Result]));
end;

procedure TCoupeDeveloppee.PutJonction(const Idx: integer; const S: TJonctionCoupeDeveloppee);
begin
  FListeDesJonctions.PutElement(Idx, S);
end;




procedure TCoupeDeveloppee.RemoveJonction(const Idx: integer);
begin
  FListeDesJonctions.RemoveElement(Idx);
end;


function TCoupeDeveloppee.initBranche(const TD: TToporobotStructure2012;
                                      const QNumeroBranche: integer;
                                      const QNoSerie: integer;
                                      const QNumeroReseau: integer;
                                      const QNoeudDepart, QNoeudArrivee: integer;
                                      const QNomBranche: string): TBrancheCoupeDeveloppee;
begin
  Result := TBrancheCoupeDeveloppee.Create;
  Result.SetDocToporobotPointer(TD);
  Result.ViderListeVisees;
  result.NumeroBranche      := QNumeroBranche;
  Result.NumeroReseau       := QNumeroReseau;

  Result.NomBranche         := QNomBranche;
  Result.NumeroSerie        := QNoSerie;
  Result.NumeroNoeudDepart  := QNoeudDepart;
  Result.NumeroNoeudArrivee := QNoeudArrivee;
  Result.DeltaP             := 0.01;
  Result.DeltaZ             := 0.01;
  Result.SensTraceCoupe     := stcdVERS_DROITE;
  Result.addViseeByValeurs(0, '', //NOI18N
                           0,
                           tgDEFAULT,
                           0, 0,
                           0.01, 0.00, -45.00,
                           0.00, 0.00, 0.00, 0.00,
                           ''); //NOI18N
end;

//-----------------------------------------

(*
procedure TCoupeDeveloppee.RecalculerLaCoupe;
begin
  //------------------------------------
  // nouvelle méthode
  ViderMatriceIncidence; // vide la matrice d'incidence
  ViderListeJonctions;
  ViderListeBranches;
end;
//*)
procedure TCoupeDeveloppee.SetMiniMaxi;
var
  Nb: Integer;
  i: Integer;
  JC: TJonctionCoupeDeveloppee;
begin
  FPMini :=  1E18;
  FPMaxi := -1E18;
  FZMini :=  1E18;
  FZMaxi := -1E18;
  Nb := self.GetNbJonctions;
  if (Nb = 0) then
  begin
    FPMini := -100;
    FPMaxi :=  100;
    FZMini := -100;
    FZMaxi :=  100;
    Exit;
  end;
  for i := IDX_MIN_JONCTION to Nb-1 do
  begin
    JC := self.GetJonction(i);
    if (JC.Abscisse < FPMini) then FPMini := JC.Abscisse;
    if (JC.Abscisse > FPMaxi) then FPMaxi := JC.Abscisse;
    if (JC.Cote < FZMini) then FZMini := JC.Cote;
    if (JC.Cote > FZMaxi) then FZMaxi := JC.Cote;
  end;
end;

function TCoupeDeveloppee.GetCoinBasGauche: TPointCoupeDeveloppee;
begin
  Result.P := FPMini;
  Result.Z := FZMini;
end;

function TCoupeDeveloppee.GetCoinHautDroit: TPointCoupeDeveloppee;
begin
  Result.P := FPMaxi;
  Result.Z := FZMaxi;
end;

function TCoupeDeveloppee.VerifierSiSerieAUnAntecedentConnu(const S: TObjSerie): integer;
type TTypeExtr = (Q_DEPART, Q_ARRIVEE);
var
  Nb: Integer;
  function AUneExtremiteConnue(const EE: TTypeExtr): boolean;
  var
    EWE: TObjSerie;
    i, p: Integer;
    nbS: Integer;
    QS0, QS, QP: Integer;
    V: TUneVisee;
    IDX: Integer;
  begin
    Result := false;
    QS := IIF((EE = Q_DEPART) , S.GetNoSerieDep, S.GetNoSerieArr);
    QP := IIF((EE = Q_ARRIVEE), S.GetNoPointDep, S.GetNoPointArr);
    for i := 0 to Nb - 1 do
    begin
      EWE := GetSerie(i);
      nbS := EWE.GetNbVisees;
      IDX := EWE.GetIndexSerie;
      for p := 0 to nbS - 1 do
      begin
        V := EWE.GetVisee(p);
        if ((QS = IDX) and (QP = p)) then
        begin
          Result := True;
        end;
      end;
    end;
  end;
begin
  Result := 0;
  Nb := self.GetNbSeries;
  // si la coupe n'a pas de série, poser VRAI
  if (Nb = 0) then
  begin
    Result := 1;
    Exit;
  end;
  if (AUneExtremiteConnue(Q_DEPART))  then begin Result :=  1; exit; end;
  if (AUneExtremiteConnue(Q_ARRIVEE)) then begin Result := -1; exit; end;
end;

function TCoupeDeveloppee.VerifierSiSerieEstUnique(const S: TObjSerie): boolean;
var
  i, Nb: Integer;
  EWE: TObjSerie;
  NbSeriesDoublon: Integer;
begin
  Result := false;
  Nb := self.GetNbSeries;
  //AfficherMessage(inttostr(Nb));
  // si la coupe n'a pas de série, poser VRAI
  if (Nb <= 0) then
  begin
    Result := True;
    Exit;
  end;
  NbSeriesDoublon := 0;
  for i := 0 to Nb - 1 do
  begin
    EWE := GetSerie(i);
    if (S.GetIndexSerie = EWE.GetIndexSerie) then NbSeriesDoublon += 1;
  end;
  //AfficherMessage(inttostr(NbSeriesDoublon));
  Result := (NbSeriesDoublon < 1);
end;



function TCoupeDeveloppee.FindAccrochageCoordsPZSerie(const S: TObjSerie): TPointCoupeDeveloppee;
var
  QS: Integer;
  QP: Integer;
  EWE: TObjSerie;
  VS: TUneVisee;
  i, Nb: Integer;
begin
  Result.P := 0.00;
  Result.Z := 0.00;
  try
    QS := S.GetNoSerieDep;
    QP := S.GetNoPointDep;
    EWE := GetSerieByIdx(QS);
    if (EWE = nil) then Exit;
    //AfficherMessage(Format('Serie accrochage: %d - %s', [EWE.GetIndexSerie, EWE.GetNomSerie]));
    Nb := EWE.GetNbVisees;
    //if (nb = 0) then Exit;
    //for i := 0 to nb - 1 do
    //begin
      VS := EWE.GetVisee(QP);

      Result.P := VS.AccroissP;
      Result.Z := VS.AccroissZ;
    //end;
  except
    ;
    //AfficherMessageErreur('00');
  end;
  (*
  AfficherMessage(Format('%d - %s est accrochée au point %d.%d (%.2f, %.2f)',
                        [S.GetIndexSerie,
                         S.GetNomSerie,
                         QS, QP,
                         Result.P,
                         result.Z
                        ]));
  //*)
end;

function TCoupeDeveloppee.Initialise(const DT: TToporobotStructure2012): boolean;
begin
  Result := False;
  try
    FListeDesSeries   := TListeSimple<TObjSerie>.Create;
    FListeDesJonctions:= TListeSimple<TJonctionCoupeDeveloppee>.Create;
    FListeDesBranches := TListeSimple<TBrancheCoupeDeveloppee>.Create;
    FDocuToporobot := DT;

    AfficherMessage(Format('%s.Initialise avec DocTopo (%d series)', [ClassName, FDocuToporobot.GetNbSeries]));

    ViderListeSeries();
    FListeDesJonctions.ClearListe();
    FListeDesBranches.ClearListe();
  except
  end;
end;

procedure TCoupeDeveloppee.Finalise;
begin
  try
    ViderListeSeries();
    FListeDesJonctions.ClearListe();
    FListeDesBranches.ClearListe();
  finally
    FListeDesSeries.Free;
    FListeDesJonctions.Free;
    FListeDesBranches.Free;
  end;
end;

procedure TCoupeDeveloppee.ViderListeSeries;
var
  ii, Nb: Integer;
  UneSerie, OS: TObjSerie;
begin
  AfficherMessage(Format('-- %s.ViderListeSeries()',[self.ClassName]));
  Nb := GetNbSeries();
  if (Nb > 0) then
  begin
    for ii := 0 to Nb - 1 do
    begin
      try
        OS := self.GetSerie(ii);
        OS.ClearStations;
        OS.Free;
      except
      end;
    end;
  end;


  FListeDesSeries.ClearListe();
  // on ajoute une série nulle
  //Serie 0
  UneSerie := TObjSerie.Create;
  with UneSerie do
  begin
    ClearStations;
    SetNoReseau(0);
    SetNomObsSerie(rsSERIE_INITIALISATION, '');
    SetIndexSerie(0);
    SetSeriePtExtremites(0,0,1,0);
    SetChanceObstacle(0,0);
    SetRaideur(1.00);
    AddVisee(EmptyVisee('Point 1/0'));
  end;
end;

//------------------------------------------------------------------------------
procedure TCoupeDeveloppee.ListerLesJonctions();
var
  J: TJonctionCoupeDeveloppee;
  i, Nb: Integer;
begin
  Nb := GetNbJonctions;
  AfficherMessageErreur(StringOfChar('-', 80));
  AfficherMessageErreur(Format('-- Liste des %d jonctions', [Nb]));

  if (Nb > 0) then
  begin
    for i := IDX_MIN_JONCTION to Nb - 1 do
    begin
      J := GetJonction(i);
      AfficherMessageErreur(Format('   i = %d, No Jonction = %d, ID = %s - %d. %d, P = %.2f; Z = %.2f',
                            [i,
                             J.NumeroJonction, J.IDJonction,
                             J.NoSerie, J.NoPoint,
                             J.Abscisse, J.Cote]));
    end;
  end;
end;


procedure TCoupeDeveloppee.AfficherMatrice(const Matrix: TMatrix; const m, n: integer);
var
  mm, i, nn, j: Integer;
  EWE: String;
begin
  mm := High(Matrix);

  AfficherMessageErreur(Format('=== Matrice d''incidence %d x %d', [m, n]));

  for i := 0 to mm do
  begin
    EWE := '';
    nn := High(Matrix[i]);
    for j := 0 to nn do
      EWE += Format('%03.0f;  ', [Matrix[i][j]]);
    AfficherMessageErreur(EWE);
  end;
  AfficherMessageErreur('====================');


end;


//*)


function TCoupeDeveloppee.GetMaxIdxNode: integer;
var
  Branche: TBrancheCoupeDeveloppee;
  i      : integer;
  M      : integer;
begin
  M := -1;
  for i := 1 to GetNbBranches-1 do
  begin
    Branche:= GetBranche(i);
    if (Branche.NumeroNoeudDepart  > M) then M := Branche.NumeroNoeudDepart;
    if (Branche.NumeroNoeudArrivee > M) then M := Branche.NumeroNoeudArrivee;
  end;
  Result := M;
end;

function TCoupeDeveloppee.RecenserLesJonctions: integer;
var
  i  : Integer;
  EWE: TObjSerie;
  NbS: Integer;
  WU: TStringArray;
  ListeJnct: TStringList;
  Ser: Integer;
  Serie: TObjSerie;
  St: Integer;
  J : TJonctionCoupeDeveloppee;
  Entree: TEntrance;
begin
  FListeDesJonctions.ClearListe();
  AfficherMessageErreur('********************************************************************');
  AfficherMessageErreur(Format('%s.RecenserLesJonctions',[self.ClassName]));
  result := -1;
  AfficherMessage(GetResourceString(rsRECENSEM_JONC));
  ListeJnct:=TStringList.Create;

  try
    ListeJnct.Clear;
    ListeJnct.Sorted :=True;
    ListeJnct.Duplicates :=dupIgnore;
    //ListeJnct.Add(Format(FMT_NDSER_PT,[0, 0]));
    //ListeJnct.Add(Format(FMT_NDSER_PT,[1, 0]));
    AfficherMessageErreur(Format('-- %d series', [self.GetNbSeries]));
    // Les série/point de départ/arrivée sont forcément des jonctions (noeuds)
    for Ser := 0 to self.GetNbSeries - 1 do
    begin
      Serie:=self.GetSerie(Ser);
      ListeJnct.Add(Format(FMT_NDSER_PT,[Serie.GetNoSerieDep,  Serie.GetNoPointDep]));
      ListeJnct.Add(Format(FMT_NDSER_PT,[Serie.GetNoSerieArr,  Serie.GetNoPointArr]));
    end;
    //---------------------------------------------------------------------------
    // Jonction0
    J.IDJonction       := '0.0';
    J.NumeroJonction   := 0;
    J.NoSerie   := 0;
    J.NoPoint   := 0;
    J.Abscisse  := 0.00;
    J.Cote      := 0.00;
    AddJonction(J, True);

    // en interne, l'ID de noeud est un littéral
    for St:=0 to ListeJnct.Count - 1 do
    begin
      J.IDJonction       := ListeJnct.Strings[St];
      J.NumeroJonction   := St + 1;
      WU := Split(J.IDJonction, '-');
      J.NoSerie   := StrToIntDef(Trim(WU[0]), -1);
      J.NoPoint   := StrToIntDef(Trim(WU[1]), -1);
      J.Abscisse  := 0.00;
      J.Cote      := 0.00;
      AddJonction(J, True);
    end;
    // contrôles
    ListerLesJonctions();
    // On passe ici ? OK, c'est bon
    Result := GetNbJonctions;
    AfficherMessageErreur(format('-- > %d junctions', [Result]));
    AfficherMessageErreur('********************************************************************');
  finally
    ListeJnct.Free;   // on libère la table provisoire
  end;
end;

procedure TCoupeDeveloppee.ViderListeBranches();
var
  Nb: LongInt;
  OB: TBrancheCoupeDeveloppee;
  i: Integer;
begin

  try
    Nb := FListeDesBranches.GetNbElements();
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do     // libérer les branches
      begin
        try
          OB := GetBranche(i);
          OB.ClearListe();
          OB.Free;
        except
        end;
      end;
    end;
  finally
    FListeDesBranches.ClearListe();
  end;
end;

function TCoupeDeveloppee.RecenserLesBranches: boolean;
var
  NbrDeSeries: Integer;
  NbrDeJonctions: Integer;
  Branche0: TBrancheCoupeDeveloppee;

  mySerie: TObjSerie;
  Br: Integer;
  i: Integer;
  NbreStations: Integer;
  v: Integer;
  myVisee: TUneVisee;
  NbreDeBranches: Integer;
  E1: TJonctionCoupeDeveloppee;
  E2, Nd1, Nd2: TJonctionCoupeDeveloppee;
  QQQ: String;
  //NoeudDeDepartToutesEntrees: TJonctionCoupeDeveloppee;
  myEntree: TEntrance;
  QQX: Double;
  QQY: Double;
  QQZ: Double;
  qL: double;
  qAz: double;
  qP: double;
  WU: TPoint3Df;
  function qsetNomBranche(const zz: integer): string;
  begin
    Result := Format('Branche%d', [zz]);
  end;
begin
  Result := false;
  ViderListeBranches();
  NbrDeSeries    := self.getNbSeries();
  NbrDeJonctions := self.getNbJonctions();
  AfficherMessageErreur(Format('Recensement branches: %d series et %d jonctions', [NbrDeSeries, NbrDeJonctions]));

  AfficherMessage('--> Pret a scanner');
  // Branche fictive indispensable pour ajuster les index de branches pour les
  // équations matricielles dont les matrices vont de 1 à n
  // Cette branche n'est jamais utilisée après le calcul des coordonnées des noeuds
  Branche0 := self.initBranche(FDocuToporobot, 0, 0, 0, 0, 0, qsetNomBranche(0));
  AddBranche(Branche0, True);
  // Branche initiale: La branche 1 gènère la première ligne de la matrice R tel que R[1,1] = 1 et R[1,j] = 0
  mySerie  := self.getSerie(0);
  Nd2 := GetJonctionBySerSt(mySerie.GetNoSerieDep(), mySerie.GetNoPointDep());
  Branche0 := self.initBranche(FDocuToporobot, 0, 0, 0, 0, Nd2.NumeroJonction, qsetNomBranche(Br));
  AddBranche(Branche0, True);
  Br := GetNbBranches();
  for i := 0 to NbrDeSeries - 1 do
  begin
    mySerie  := self.getSerie(i);
    Nd1       := self.GetJonctionBySerSt(mySerie.GetNoSerieDep(), mySerie.GetNoPointDep());
    Nd2       := self.GetJonctionBySerSt(mySerie.GetNoSerieArr(), mySerie.GetNoPointArr());

    Branche0 := self.initBranche(FDocuToporobot, Br, mySerie.GetIndexSerie, mySerie.GetNoReseau, Nd1.NumeroJonction, Nd2.NumeroJonction, qsetNomBranche(Br));
    NbreStations := mySerie.GetNbVisees();
    for v := 1 to NbreStations - 1 do
    begin
      myVisee := mySerie.GetVisee(v);
      //myVisee.NoVisee := v;
      Nd1 := self.GetJonctionBySerSt(mySerie.GetIndexSerie, v);
      Nd2 := self.GetJonctionBySerSt(mySerie.GetIndexSerie, v);

      Branche0.addVisee(myVisee);
      if ((Nd1.NumeroJonction > -1) and (v < (NbreStations - 1))) then
      begin
        // clôturer la branche courante
        Branche0.NumeroNoeudArrivee := Nd1.NumeroJonction;
        self.addBranche(Branche0, True);
        // et creer la suivante
        Br += 1;
        Branche0 := initBranche(FDocuToporobot, Br, mySerie.GetIndexSerie, mySerie.getnoReseau, Nd1.NumeroJonction, Nd2.NumeroJonction, qsetNomBranche(Br));
      end;
      // fin de serie = fin de branche: cloturer la branche
      if (v = NbreStations - 1) then
      begin
        Nd2 := self.GetJonctionBySerSt(mySerie.GetNoSerieArr, mySerie.GetNoPointArr);
        Branche0.NumeroNoeudArrivee := Nd2.NumeroJonction;
        self.addBranche(Branche0, True);
        Br += 1;
      end;
    end;
  end;
  AfficherMessage(GetResourceString(rsFINDING_BRANCHES) + ' OK');
end;

procedure TCoupeDeveloppee.exportVersGHCD(const filename: string);
begin
  AfficherMessage(Format('%s.exportVersGHCD(%s) ', [ClassName, Filename]));
end;

procedure TCoupeDeveloppee.ListerSeriesRetenues(const LSB: TListBox; const Idx: integer);
var
  i, Nb: Integer;
  EWE: TObjSerie;
  WU : string;
begin
  LSB.Clear;
  Nb := self.GetNbSeries;
  AfficherMessage(Format('ListerSeriesRetenues: %d series', [Nb]));
  if (Nb = 0) then Exit;
  for i := 0 to Nb -1 do
  begin
    EWE := self.GetSerie(i);
    WU  := Format('%d - %s',
           [EWE.GetIndexSerie(),
            EWE.GetNomSerie()
           ]);
    AfficherMessage(WU);
    LSB.Items.add(WU);
    LSB.ItemIndex := IIF(Idx = -1, LSB.Count-1, Idx);
  end;
end;
procedure TCoupeDeveloppee.ListerBranchesDUneSerie(const LSB: TListBox; const Idx: integer);
var
  Nb, i: Integer;
  BR: TBrancheCoupeDeveloppee;
  WU1, WU2: TJonctionCoupeDeveloppee;
  SR: TObjSerie;
begin
  LSB.Clear;
  Nb := GetNbBranches();
  AfficherMessage(Format('ListerBranchesDUneSerie: Nb: %d - %d', [Nb, Idx]));
  if (Nb = 0) then Exit;
  SR := GetSerie(Idx);
  try
    for i := 0 to Nb - 1 do
    begin
      BR := GetBranche(i);
      AfficherMessage(Format('%d: %d - Serie: %d', [i, Br.NumeroBranche, BR.NumeroSerie]));
      if (BR.NumeroSerie = SR.GetIndexSerie()) then
      begin
        WU1 := GetJonction(BR.NumeroNoeudDepart);
        WU2 := GetJonction(BR.NumeroNoeudArrivee);
        LSB.Items.Add(Format('%d: %d - From: %d.%d To %d.%d - Serie: %d - Sens: %s', [
                             i, Br.NumeroBranche,
                             WU1.NoSerie, WU1.NoPoint,
                             WU2.NoSerie, WU2.NoPoint,
                             BR.NumeroSerie,
                             BoolToStr(BR.SensTraceCoupe = stcdVERS_DROITE, '-->', '<--')
                             ]));
      end;
    end;
  except
  end;
end;

function TCoupeDeveloppee.FindIdxSerieNearPoint(const P, Z: double): integer;
var
  EWE: TObjSerie;
  Nearest: Extended;
  nb, ns: Integer;
  i, s: Integer;
  WU: TUneVisee;
  r: Extended;
begin
  result := -1;
  Nearest := 6.66E18;
  nb := self.GetNbSeries;
  if (nb = 0) then Exit;
  for i := 0 to nb -1 do
  begin
    EWE := GetSerie(i);
    ns := EWE.GetNbVisees;
    if (ns = 0) then exit;
    for s := 0 to ns -1 do
    begin
      WU := EWE.GetVisee(s);
      r  := sqr(P - WU.AccroissP) + sqr(Z - WU.AccroissZ);
      if (r < Nearest) then
      begin
        Result := i;
        Nearest:= r;
      end;
    end;
  end;
end;
// Recenser les jonctions et les branches
function TCoupeDeveloppee.RecenserJonctionsEtBranches(): boolean;
begin
  Result := false;
  try
    FListeDesJonctions.ClearListe();
    FListeDesBranches.ClearListe();
    self.RecenserLesJonctions();
    self.RecenserLesBranches();
    AfficherMessage(Format('-- %s.RecenserJonctionsEtBranches(): %d Jonctions et %d branches', [self.ClassName, GetNbJonctions, GetNbBranches]));
    Result := (self.GetNbBranches() > 0);
  except
  end;
end;

// construire la liste du graphe
function TCoupeDeveloppee.ConstruireLaCoupeDeveloppee(const DoRegenGraphe: boolean): boolean;
var
  EWE: String;
begin
  EWE := Format('%s.ConstruireGrapheEnListe: %d series', [ClassName, GetNbSeries]);
  AfficherMessage(EWE);
  AfficherMessageErreur(EWE);
  result := false;
  // On ne régénère pas le graphe de la coupe lorsque on ne fait que pivoter une branche
  if (DoRegenGraphe) then
  begin
    if (not RecenserJonctionsEtBranches()) then Exit;
  end;
  try
    CalculerCoordNoeuds();
    Result := True;
  except
  end;
  /////////////////////////////////////
  {
  Nb := self.GetNbBranches;
  if (Nb = 0) then exit;

  AfficherMessage('-------------------------------------------------------');
  AfficherMessageErreur(Format('--- Liste des %d branches', [Nb]));

  for i := 0 to Nb -1 do
  begin
    MyBranche := GetBranche(i);
    E1 := GetJonction(MyBranche.NumeroNoeudDepart);
    E2 := GetJonction(MyBranche.NumeroNoeudArrivee);

    AfficherMessageErreur(Format('Branche %d; %d; Du noeud; %d ;%d.%d; au noeud; %d [%d.%d];%s; %d; shots ; dp =; %.2f; dz =; %.2f;',
                           [i,
                            MyBranche.NumeroBranche,
                            MyBranche.NumeroNoeudDepart , E1.NoSerie, E1.NoPoint,
                            MyBranche.NumeroNoeudArrivee, E2.NoSerie, E2.NoPoint,
                            MyBranche.NomBranche,
                            MyBranche.GetNbVisees,
                            MyBranche.DeltaP, MyBranche.DeltaZ
                           ]));
  end;
  // stats du graphe
  NbArcs       := GetNbBranches;
  NbSommets    := GetNbJonctions;
  AfficherMessage(Format('Le graphe comporte %d sommets et %d arcs', [NbSommets, NbArcs]));
  // passage ici == OK
  Result := True;
  //*}
end;



procedure TCoupeDeveloppee.SaveCoupe(const QFilename: string);
var
  LS: TStringList;
  n, i: Integer;
  SR: TObjSerie;
  EWE: String;
begin
  LS := TStringList.Create;
  try
    LS.Clear;
    n := GetNbSeries();
    if (n = 0) then Exit;
    for i := 0 to n-1 do
    begin
      SR := GetSerie(i);
      EWE := Format('%d; %d; %d.%d; %d.%d; %s', [
                    i, SR.GetIndexSerie(),
                    SR.GetNoSerieDep(), SR.GetNoPointDep(),
                    SR.GetNoSerieArr(), SR.GetNoPointArr(),
                    SR.GetNomSerie()
                   ]);

      LS.Add(EWE);
    end;
    LS.SaveToFile(QFilename);
  finally
    LS.Free;
  end;
end;
function TCoupeDeveloppee.LoadCoupe(const QFilename: string): boolean;
var
  LS: TStringList;
  EWE: TStringArray;
  NS: LongInt;
  n, i, QInternalIdxSerie: Integer;
  SR: TObjSerie;
begin
  AfficherMessage(Format('%s.LoadCoupe: %s', [ClassName, QFilename]));
  Result := false;
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.LoadFromFile(QFilename);
    n := LS.Count;
    if (n = 0) then Exit;
    // nettoyer les lignes vides
    for i := LS.Count - 1 downto 0 do
    begin
      if (trim(LS.Strings[i]) = '') then LS.Delete(i);
    end;
    // recenser
    for i := 0 to n-1 do
    begin
      EWE := Split(LS.Strings[i], ';');
      NS  := StrToIntDef(EWE[1], 0);
      FDocuToporobot.GetSerieByIdxSerie(NS, SR, QInternalIdxSerie);
      self.AddSerie(SR, True);
    end;
    LS.Clear;
    // controle
    n := GetNbSeries();
    for i := 0 to n - 1 do
    begin
      SR := GetSerie(i);
      AfficherMessage(Format('-- %d: %d - %s', [i, SR.GetIndexSerie(), SR.GetNomSerie()]));
    end;
    // passage ici -> OK
    result := true;
  finally
    LS.Free;
  end;

end;


procedure TCoupeDeveloppee.AfficherListeJonctions;
var
  i, j, Nb: Integer;
  myJonction: TJonctionCoupeDeveloppee;
  NbPreds, NbSuccs: Integer;
begin
  Nb := GetNbJonctions;
  AfficherMessage(Format('%s.AfficherListeJonctions: %d jonctions', [ClassName,Nb]));
  for i := IDX_MIN_JONCTION to Nb - 1 do
  begin
    myJonction := GetJonction(i);
    AfficherMessage(Format('-- Node: %d: %d (%d.%d)',
                          [i,
                           myJonction.NumeroJonction,
                           myJonction.NoSerie, myJonction.NoPoint
                          ]));
  end;
end;

procedure TCoupeDeveloppee.BasculerSensDessinBrancheByIdx(const Idx: integer);
var
  Nb: Integer;
  EWE: TBrancheCoupeDeveloppee;
  i: Integer;
begin
  Nb := self.GetNbBranches;
  if (IsInRange(Idx, 0, Nb-1)) then
  begin
    EWE := self.GetBranche(Idx);
    if (EWE.SensTraceCoupe = stcdVERS_DROITE) then EWE.SensTraceCoupe := stcdVERS_GAUCHE
                                              else EWE.SensTraceCoupe := stcdVERS_DROITE;

    EWE.DeltaP := -EWE.DeltaP;
    self.PutBranche(Idx, EWE);
  end;
end;
//------------------------------------------------------------------------------
procedure TCoupeDeveloppee.ExportSVG(const FichierSVG: string);
const
  NOM_STYLE_CROSS_SECTION      = 'CrossSection';
  NOM_STYLE_STATION_MARKER     = 'StationMarker';
  NOM_STYLE_STATION_MARKER_TXT = 'StationMarkerText';



  FMT_ID_GROUPE_BRANCHE        = 'Branche%d';
  FMT_STYLE_CENTERLINE_SECTEUR = 'Centerlines_secteur_%d';
  FMT_STYLE_CENTERLINE_RESEAU  = 'Centerlines_reseau_%d';

var
  MyContexteSVG: TSVGCanvas;
  i, Nb: Integer;
  BR: TBrancheCoupeDeveloppee;
  EWE: TReseau;
  function DrawCenterLineBranche(const B: TBrancheCoupeDeveloppee): boolean;
  const
    MARKER_RADIUS = 0.500;
  var
    s, nbs: Integer;
    myVisee: TUneVisee;
    JC1: TJonctionCoupeDeveloppee;
    JC2: TJonctionCoupeDeveloppee;
    uP: Extended;
    uZ: Extended;
    WU: String;
    LS: TArrayPoints2Df;
    DeltaP: Extended;
    DeltaZ: Extended;
  begin
    Result := false;

    JC1 := GetJonction(B.NumeroNoeudDepart);
    JC2 := GetJonction(B.NumeroNoeudArrivee);
    SetLength(LS, 0);
    nbs := B.GetNbVisees;
    if (nbs > 0) then
    begin
      SetLength(LS, nbs);
      LS[0].X := 0.00; //JC1.Abscisse;
      LS[0].Y := 0.00; //JC1.Cote;
      WU := Format(FMT_ID_GROUPE_BRANCHE, [B.NumeroBranche]);
      MyContexteSVG.BeginGroupe(WU, '', JC1.Abscisse, JC1.Cote);
      if (nbs > 1) then
      begin
        uP := 0.00;
        uZ := 0.00;
        for s := 1 to nbs - 1 do
        begin
          myVisee := B.GetVisee(s);
          uP += myVisee.DeltaP * IIF((B.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
          uZ += myVisee.DeltaZ;
          LS[s].X := uP;
          LS[s].Y := uZ;
          // on trace ici les cross sections
          MyContexteSVG.DrawLine(NOM_STYLE_CROSS_SECTION, uP, uZ - myVisee.HN, uP, uZ + myVisee.HZ);
        end;
      end;
      nbs := High(LS);
      DeltaP := JC2.Abscisse - JC1.Abscisse;
      DeltaZ := JC2.Cote     - JC1.Cote;
      LS[nbs].X := DeltaP; LS[nbs].Y := DeltaZ;
      for s := 0 to High(LS) do AfficherMessage(Format('--> %d/%d: %f, %f', [s+1, nbs, LS[s].X, LS[s].Y]));
      WU := Format(FMT_ID_GROUPE_BRANCHE, [B.NumeroBranche]);
      // tracé proprement dit
      MyContexteSVG.DrawPolygonOrPolyline(Format(FMT_STYLE_CENTERLINE_RESEAU, [B.NumeroReseau]),
                                                 LS,
                                                 false);
      // tracé des marqueurs de jonctions
      //MyContexteSVG.DrawCircle(NOM_STYLE_STATION_MARKER, 0.00, 0.00, MARKER_RADIUS);
      //MyContexteSVG.DrawCircle(NOM_STYLE_STATION_MARKER, DeltaP, DeltaZ, MARKER_RADIUS);
      MyContexteSVG.DrawPolygoneInscrit(NOM_STYLE_STATION_MARKER, 6, 0.00, 0.00, 5*MARKER_RADIUS);
      MyContexteSVG.DrawPolygoneInscrit(NOM_STYLE_STATION_MARKER, 6, DeltaP, DeltaZ, 5*MARKER_RADIUS);

      MyContexteSVG.DrawText(NOM_STYLE_STATION_MARKER_TXT, 1,
                             DeltaP + 2*MARKER_RADIUS, DeltaZ + 2*MARKER_RADIUS,
                             0,
                             Format('%.2f', [JC2.Cote])
                             );
      //MyContexteSVG.D;
      MyContexteSVG.EndGroupe(WU);
    end;

  end;
begin
  AfficherMessage(Format('%s.ExportSVG(%s)', [ClassName, FichierSVG]));
  MyContexteSVG := TSVGCanvas.Create;
  try
    if (MyContexteSVG.InitializeDocument(FichierSVG,
                                         True,
                                         'Coupe developpee',
                                         'Coupe developpee partielle',
                                         FPMini, FZMini, FPMaxi, FZMaxi)) then
    begin
      self.SetMiniMaxi;
      MyContexteSVG.BeginStylesSection;     // styles des objets
        // marqueurs de stations
        MyContexteSVG.WriteStyleSolidPolygone(NOM_STYLE_STATION_MARKER, clRed, clYellow, 0.1, '');
        MyContexteSVG.WriteStyleText(NOM_STYLE_STATION_MARKER_TXT,
                                     DEFAULT_FONT_NAME,
                                     [fsBold],
                                     clBlue,
                                     1.25, 'Style text station marker');
        // cross_sections
        MyContexteSVG.WriteStyleSolidLine(NOM_STYLE_CROSS_SECTION, clGray, 0.15, '');
        // styles des centerlines par réseaux -- TODO: A faire par séances, etc ...
        Nb := FDocuToporobot.GetNbReseaux;
        for i := 0 to Nb -1 do
        begin
          EWE := FDocuToporobot.GetReseau(i);
          MyContexteSVG.WriteStyleSolidLine(Format(FMT_STYLE_CENTERLINE_RESEAU, [i]),
                                            EWE.ColorReseau,
                                            0.25,
                                            '');
        end;

      MyContexteSVG.EndStylesSection;
      //MyContexteSVG.BeginGroupe('Coupe', '', 0.00, 0.00);

      // groupes; 1 groupe = 1 branche
      Nb := GetNbBranches;
      for i := 0 to Nb-1 do
      begin
        BR := GetBranche(i);
        DrawCenterLineBranche(BR);
      end;
      //MyContexteSVG.EndGroupe('Coupe');
      MyContexteSVG.FinalizeDocument();
    end;
  finally
    MyContexteSVG.Free;
  end;
end;
// Calcul des coordonnées des noeuds
procedure TCoupeDeveloppee.CalculerCoordNoeuds;
const
  NBSTEPS = 10;
var
  R, B: TMatrix;
  A, Delta: TVector;
  n, QAxe, njc: integer;
  MyJonction: TJonctionCoupeDeveloppee;
  // matrice de connexion (ici, matrice d'incidence)
  function GetMaxNode(): integer;
  var
    Branche: TBrancheCoupeDeveloppee;
    i      : integer;
    M      : integer;
  begin
    M := Low(Integer);
    for i:=1 to GetNbBranches-1 do
    begin
      Branche:= GetBranche(i);
      if (Branche.NumeroNoeudDepart > M)  then M := Branche.NumeroNoeudDepart;
      if (Branche.NumeroNoeudArrivee > M) then M := Branche.NumeroNoeudArrivee;
    end;
    Result:=M;
  end;
  procedure MakeRMatrix();
  var
    i,j: integer;
    Branche: TBrancheCoupeDeveloppee;
    EWE: String;
    JC1, JC2: TJonctionCoupeDeveloppee;
    function beuh(const QB: TBrancheCoupeDeveloppee; const QJ: integer): double;
    begin
       if      (QB.NumeroNoeudDepart  = j) then Result := -1.0
       else if (QB.NumeroNoeudArrivee = j) then Result := +1.0
                                           else Result :=  0.0;
    end;
  begin
    SetLength(R, 0,0);
    SetLength(R, GetNbBranches() + 1, n + 1);
    AfficherMessageErreur(Format('%s.MakeRMatrix', [ClassName]));
    for i:= 0 to GetNbBranches - 1 do
    begin
      Branche := GetBranche(i);
      for j := 0 to n do R[i,j] := beuh(Branche, j);
    end;
    EWE := 'Branche; Dep->Arr; dP; dZ;';
    //AfficherMessageErreur(EWE);
    for i := 0 to GetNbBranches - 1 do
    begin
      Branche := GetBranche(i);
      JC1 := GetJonction(Branche.NumeroNoeudDepart);
      JC2 := GetJonction(Branche.NumeroNoeudArrivee);
      EWE := Format('Branche: %.4d; %d -> %d ; %d.%d -> %d.%d; %s; %s;', [i,
                        Branche.NumeroNoeudDepart, Branche.NumeroNoeudArrivee,
                        JC1.NoSerie, JC1.NoPoint,
                        JC2.NoSerie, JC2.NoPoint,
                        FormatterNombreOOo(Branche.DeltaP, 3, false),
                        FormatterNombreOOo(Branche.DeltaZ, 3, false)]);
      for j := 0 to n do
        EWE += Format('%03.0f;  ', [R[i][j]]);
      //AfficherMessageErreur(EWE);
    end;
  end;
  // matrice de compensation = matrice de Laplace = Rt.W.R
  procedure MakeBMatrix;
  var
    i,j,k: integer;
    ww: double;
    //f: TextFile;
    q, qNbBr: integer;
    LowIndex  : array of Integer;
    HighIndex : array of Integer;
    EWE: String;

  begin

    AfficherMessage(GetResourceString(rsBUILDMATRICE_LAPLACIENNE));
    qNbBr := self.GetNbBranches();
    AfficherMessageErreur(Format('MakeBMatrix: d branches, %d noeuds', [qNbBr, n]));
    SetLength(B, 0,0);
    SetLength(B, n+1, n+1);
    for i := 0 to High(B) do
      for j := 0 to High(B[i]) do
        B[i][j] := 0.00;
    // rechercher les index mini et maxi des valeurs non nulles de RMatrix

    SetLength(LowIndex,0);
    SetLength(HighIndex,0);

    SetLength(LowIndex,n+1);
    SetLength(HighIndex,n+1);
    AfficherMessage('---> '+ GetResourceString(rsFIND_SUM_LIMITS));

    for i:=1 to n do
    begin
      for j:=1 to qNbBr - 1 do
      begin
        if (Abs(R[j, i]) > 0) then
        begin
          LowIndex[i]:=j;
          Break;
        end;
      end;
    end;
    for i:=1 to n do
    begin
      for j:= qNbBr - 1 downto 1 do
      begin
        if (Abs(R[j,i]) > 0) then
        begin
          HighIndex[i]:=j;
          Break;
        end;
      end;
    end;
    q:=0;
    for i:=1 to n do
    begin
      q:= q + 1+(HighIndex[i] - LowIndex[i]);
    end;
    //*)
    for i:=1 to n do
    begin
      for j:=1 to i do
      begin
        ww:=0;
        //for k:=LowIndex[i] to HighIndex[i] do ww := ww + R[k,i] * R[k,j];
        for k:=1 to qNbBr - 1 do ww := ww + R[k,i] * R[k,j];
        B[i,j]:=ww;
      end;
    end;
    // remplissage de la symétrie
    for i:=1 to N-1 do
      for j:=1+i to N do
        B[i,j]:=B[j,i];
    SetLength(LowIndex,0);
    SetLength(HighIndex,0);
    // affichage de la matrice
    EWE := 'Matrice de compensation';
    AfficherMessageErreur(EWE);
    for i := 0 to GetNbBranches-1 do
    begin
      EWE := '';
      for j := 0 to n do
        EWE += Format('%03.0f;  ', [B[i][j]]);
      AfficherMessageErreur(EWE);
    end;
  end;

  // accroissements des branches
  procedure MakeAVector(const Axe: Byte);
  var
    k, qNbBrch: integer;
    Branche: TBrancheCoupeDeveloppee;
  begin
    qNbBrch := GetNbBranches();
    AfficherMessageErreur(Format('-- MakeAVector: %d', [qNbBrch]));
    AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    SetLength(A, 0);
    SetLength(A, qNbBrch);
    for k := 0 to High(A) do A[k] := 0.00;
    for k:= 0 to qNbBrch - 1 do
    begin
      AfficherMessageErreur(Format(' --- 004.%d', [k]));
      Branche:=GetBranche(k);
      case Axe of
        1: A[k]:=Branche.DeltaP;
        2: A[k]:=Branche.DeltaZ;
      end;
    end;
    //A[1]:=0.01;
    for k := 0 to high(A) do AfficherMessageErreur(Format(' --- A[%d] = %.2f', [k, A[k]]));
  end;
  // second membre
  procedure MakeDeltaVector;
  var
    i,k    : integer;
    ww     : double;
  begin;
    SetLength(Delta, 0);
    SetLength(Delta, n+1);
    AfficherMessageErreur(Format('-- MakeDeltaVector: %d', [n+1]));
    for i:=1 to n do
    begin
      ww := 0;
      for k:=1 to GetNbBranches do
      begin
        ww := ww + R[k,i] * A[k];
      end;
      Delta[i]:=ww;
    end;
    for k := 0 to high(A) do AfficherMessageErreur(Format(' --- Delta[%d] = %.2f', [k, Delta[k]]));
  end;
  // résolution
  procedure SolveMatrix(const Axe: byte);
  var
    i, j, k   : integer;
    vv        : double;
    ww        : double;
    V_Matrix  : TMatrix; // matrice locale de factorisation
    S_Vector  : TVector; // vecteur local complémentaire
    XX        : TVector;
    QPosPtZero: TPoint3Df;
    JC: TJonctionCoupeDeveloppee;
  begin
    //try
      SetLength(XX, 0);
      SetLength(XX, n+1);
      SetLength(V_Matrix, 0,0);
      SetLength(V_Matrix, n+1, n+1);
      SetLength(S_Vector, 0);
      SetLength(S_Vector, GetNbBranches+1);
      for i:=1 to n do XX[i]:=0.0;
      // descente: V.V* = A
      AfficherMessageErreur(GetResourceString(rsDESCENTE));
      for i:=1 to n do
      begin
        if (i mod 100 = 0) then AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, n]));
        vv := 0;
        for k := 1 to i-1 do
          vv := vv + sqr(V_Matrix[i,k]);
        V_Matrix[i,i]:=Sqrt(abs(B[i,i]-vv));
        for j:=1+i to n do
        begin
          ww := 0;
          for k := 1 to i-1 do
            ww := ww + V_Matrix[i,k] * V_Matrix[j,k];
          V_Matrix[j,i] := (B[i,j] - ww) / (V_Matrix[i,i] + 1e-24);
        end;
      end;
      // second membre; triangularisation
      AfficherMessageErreur(GetResourceString(rsTRIANGULARISATION));
      for i:=1 to n do
      begin
        if (i mod 200 = 0) then AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, n]));
        ww := 0;
        for k:=1 to i-1 do ww := ww + V_Matrix[i,k] * S_Vector[k];
        S_Vector[i]  := (Delta[i] - ww) / (V_Matrix[i,i] + 1e-24);
      end;
      // remontée du système; inconnues recherchées
      AfficherMessage(GetResourceString(rsREMONTEE));
      for i:=n downto 1 do
      begin
        if (i mod 200 = 0) then AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, n]));
        ww := 0;
        for k:=1+i to n do ww := ww+ V_Matrix[k,i] * XX[k];
        XX[i]:=(S_Vector[i] - ww) / (V_Matrix[i,i] + 1e-24);
      end;
      XX[0] := XX[1];

      // Table des Noeuds
      for i:=1 to n do
      begin
        XX[i]:=XX[i]-XX[0];
        JC := GetJonction(i-1);
        case Axe of
          1: JC.Abscisse := XX[i-1];  //
          2: JC.Cote     := XX[i-1];  //
        end;
        PutJonction(i-1, JC);
      end;
      // jonction 0
      JC := GetJonction(0);
      JC.Abscisse := 0.01;
      JC.Cote     := 0.01;
      PutJonction(0, JC);

      // ++++++++++++++++
      // Passage par cette ligne = c'est OK
      AfficherMessageErreur(Format(GetResourceString(rsCOORDONNEES_OK),[n]));
      Exit;
    //finally
      AfficherMessage(GetResourceString(rsDEL_TEMP_MATRIX));
      SetLength(V_Matrix,0,0);
      SetLength(S_Vector, 0);
      SetLength(XX,0);
    //end;
  end;
  procedure FinalizeMatrices;
  begin
    //SetLength(R,0,0); ne pas détruire la matrice d'incidence
    SetLength(Delta,0);
    SetLength(A,0);
    SetLength(B,0,0);
  end;

begin
  AfficherMessage(GetResourceString(rsBINDMATRIX));
  n := GetMaxNode() + 1;
  AfficherMessageErreur(Format(GetResourceString(rsSTEP_CALC_01),[1,NBSTEPS]));
  MakeRMatrix; // matrice d'incidence (assemblage)
  AfficherMessageErreur(Format(GetResourceString(rsSTEP_CALC_03),[3,NBSTEPS]));

  MakeBMatrix; // matrice de compensation
  AfficherMessageErreur(Format(GetResourceString(rsSTEP_CALC_04),[4,NBSTEPS]));
  for QAxe := 1 to 2 do
  begin
    AfficherMessage('');
    AfficherMessage(GetResourceString(rsFACTORISEMATRICE_LAPLACIENNE));
    AfficherMessage(GetResourceString(rsAXIS) + ' ' + inttostr(QAxe));
    MakeAVector(QAxe);
    AfficherMessage(GetResourceString(rs2NDMEMBER));
    MakeDeltaVector;
    AfficherMessage(GetResourceString(rsCOMPESMATRIX));
    SolveMatrix(QAxe);
  end;

  // affichage coordonnées des jonctions
  n := GetNbJonctions();
  AfficherMessageErreur('Coordonnees jonctions');
  for njc :=IDX_MIN_JONCTION to n - 1 do
  begin
    MyJonction := GetJonction(njc);
    AfficherMessageErreur(Format('-- %d; %s; %d.%d;  %.2f; %.2f;', [njc, MyJonction.IDJonction,
                                                                    MyJonction.NoSerie, MyJonction.NoPoint,
                                                                    MyJonction.Abscisse, MyJonction.Cote]));

  end;

  //-- finalisation
  AfficherMessage(GetResourceString(rsFREE_TEMP_VARS));
  FinalizeMatrices;
  //*)

end;
//******************************************************************************
// Fonctions de graphes obsolètes
//******************************************************************************
// exploration du graphe:
// Modes: mpgEN_LARGEUR    = parcours en largeur,    utilise une file
// Modes: mpgEN_PROFONDEUR = parcours en profondeur, utilise une pile


end.

