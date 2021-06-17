// 05/05/2016: Réacculturation - Les listes simples passent aux génériques
// 09/05/2016: Il est possible de démarrer une coupe développée depuis n'importe quelle série
// 10/05/2016: Construction des coupes développées par calcul matriciel
// 13/05/2016: Moteur de calcul opérationnel après une mise au point très laborieuse
// 12/06/2016: Cette unité est neutralisée si l'utilisation des génériques est désactivée

// 30/08/2016: Les branches des coupes développées deviennent de simples record
//             (plus simple et moins de bugs: un SetLength() préserve le tableau (aka REDIM PRESERVE du Basic) et
//             est suffisant pour cette application (pas de suppression de visées)
// L'ancienne version OK est dans unitCoupeDeveloppee_old

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
  , ComCtrls      // pour TTreeView
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
  NbVisees          : integer;
end;

{ TCoupeDeveloppee }
type TCoupeDeveloppee = class
  private
    FDocuToporobot: TToporobotStructure2012;
    // contenu du script
    FScript: TStringList;
    // liste des séries
    FListeDesSeries    : TListeSimple<TObjSerie>;
    // liste des noeuds
    FListeDesJonctions : TListeSimple<TJonctionCoupeDeveloppee>;
    // liste des branches
    FListeDesBranches  : TListeSimple<TBrancheCoupeDeveloppeeAsRecord>; //TObjectList;

    FPMini : double;
    FPMaxi : double;
    FZMini : double;
    FZMaxi : double;
    //--------------------------------------------------------------------------
    procedure addViseeAtBranche(var BR: TBrancheCoupeDeveloppeeAsRecord; const V: TUneVisee);
    function  CalcDeltaPZ(const Br: TBrancheCoupeDeveloppeeAsRecord): TBrancheCoupeDeveloppeeAsRecord;
    procedure CalculerCoordNoeuds();
    function  GetIdxJonctionBySerSt(const s, p: integer): integer;

    //--------------------------------------------------------------------------
    procedure AfficherListeJonctions();

    // vérifie si la série a un point d'accrochage connu de la coupe
    function  VerifierSiSerieAUnAntecedentConnu(const S: TObjSerie): integer;
    // vérifie si la série est unique (anti-doublon)
    function  VerifierSiSerieEstUnique(const S: TObjSerie): boolean;

    function  RecenserJonctionsEtBranches(): boolean;
    procedure ViderListeBranches();

  public
    property  Script: TStringList read FScript write FScript;
    function  Initialiser(const DT: TToporobotStructure2012): boolean;
    procedure Finaliser();
    procedure ResetAll();
    procedure ListerBranchesDUneSerie(const LSB: TListBox; const Idx: integer);
    procedure SetMiniMaxi();
    // Les séries
    procedure ViderListeSeries();
    function  GetNbSeries(): integer;
    function  AddSerie(const S: TObjSerie; const DoOuvrirSerie: boolean): boolean;
    function  GetSerie(const Idx: integer): TObjSerie;
    function  GetSerieByNumeroSerie(const Idx: TNumeroSerie; out SR: TObjSerie): boolean;
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
                          const QNomBranche: string): TBrancheCoupeDeveloppeeAsRecord;
    function  GetNbBranches(): integer;
    procedure AddBranche(const S: TBrancheCoupeDeveloppeeAsRecord; const DoCalcPZ: boolean);
    function  GetBranche(const Idx: integer): TBrancheCoupeDeveloppeeAsRecord;
    procedure PutBranche(const Idx: integer; const BRCH: TBrancheCoupeDeveloppeeAsRecord);
    function  FindIndexBrancheNearToXY(const X, Y: double; out QNumSerie, QNumStation: integer): integer;
    procedure BasculerSensDessinBrancheByIdx(const Idx: integer);


    //procedure RecalculerLaCoupe;
    function  GetCoinBasGauche: TPointCoupeDeveloppee;
    function  GetCoinHautDroit: TPointCoupeDeveloppee;
    function  FindAccrochageCoordsPZSerie(const S: TObjSerie): TPointCoupeDeveloppee;
    function  FindIdxSerieNearPoint(const P, Z: double): integer;

    function  ConstruireLaCoupeDeveloppee(const DoRegenGraphe: boolean): boolean;
    function  GetMaxIdxNode: integer;

    // lister les noeuds et arcs
    procedure ListerLesJonctions();
    // export ODG
    procedure ExportSVG(const FichierSVG: string);
    // sauvegarde de la coupe
    procedure SaveScriptCoupe(const QFilename: string);
    function  LoadScriptCoupe(const QFilename: string): boolean;
    function  InterpreterScript(): boolean;


    procedure ExporterCoupeGCP(const FichierGCP: TStringDirectoryFilename);

    function  FindIdxBrancheContainingSerieStation(const QSer, QSt: integer): integer;


end;

implementation
uses
  DGCDummyUnit;
//uses Forms, Dialogs, frmJournal;
{ TCoupeDeveloppee }
const IDX_MIN_JONCTION = 0;


function TCoupeDeveloppee.CalcDeltaPZ(const Br: TBrancheCoupeDeveloppeeAsRecord): TBrancheCoupeDeveloppeeAsRecord;
var
  Nb, i: Integer;
  V: TUneVisee;
  C: TCode;
  E: TExpe;
  AX, AY, AP, AZ: double;
begin
  Result := Br;
  Result.DeltaP  := 0.0001;
  Result.DeltaZ  := 0.0001;
  Nb := 1 + High(Result.ArrVisees);
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
       V := Result.ArrVisees[i];
       C := FDocuToporobot.GetCodeByNumero(V.Code);
       E := FDocuToporobot.GetExpeByNumero(V.Expe);
       // factorisation du code
       CalculerVisee(V, C, E, AX, AY, AZ, AP);
       //Result.DeltaP := Result.DeltaP + IIF((Result.SensTraceCoupe = stcdVERS_DROITE), V.DeltaP, -V.DeltaP);
       //Result.DeltaZ := Result.DeltaZ + V.DeltaZ;
       Result.DeltaP += IIF((Result.SensTraceCoupe = stcdVERS_DROITE), V.AccroissP, -V.AccroissP);
       Result.DeltaZ += V.AccroissXYZ.Z;

       Result.ArrVisees[i] := V; //PutVisee (i, V);
    end;
  end;

end;

// Les séries
function TCoupeDeveloppee.GetNbSeries(): integer;
begin
  Result := FListeDesSeries.GetNbElements();
end;

function TCoupeDeveloppee.AddSerie(const S: TObjSerie; const DoOuvrirSerie: boolean): boolean;
var
  qSerie: TObjSerie;
  i, Nb: Integer;
  EWE: TUneVisee;
  Q1, Q2, Q3: Boolean;
begin
  Result := false;
  // Source de bugs corrigée
  // COPIER la série et non faire une simple affectation !!!!
  // Création de l'objet série
  qSerie := TObjSerie.Create;
  try
    qSerie.ClearStations();
    // Copie de la série
    qSerie.SetNomObsSerie(S.GetNomSerie, S.GetObsSerie);
    qSerie.SetChanceObstacle(S.GetChance, S.GetObstacle);

    qSerie.SetSeriePtExtremites(S.GetNoSerieDep, S.GetNoPointDep,
                                S.GetNoSerieArr, S.GetNoPointArr);
    // séries raccordées au réseau via la seconde extrémité
    // Si (IdxSerie == NoSerArr) ET (NoPtDep != 0), on ouvre la série ("anti-spirale")
    // Si (IdxSerie != NoSerArr) ET (NoPtDep != 0), on ouvre la série
    // Si (IdxSerie != NoSerArr) ET (NoPtDep == 0), on ne touche à rien

    // Série 12.6 (69 points) se raccorde à la série 115.6 -> (Numéro de série <> Série arrivée); (PtDep <> 0) -> elle doit être ouverte = Arrivée en 12.68
    // Série 63.0 (30 points) se raccorde à la série 62.23 -> (Numéro de série <> Série arrivée); (PtDep = 0)  -> on ne touche à rien
    Q1 := (S.GetNoSerieArr() <> S.GetNumeroDeSerie());      // = Serie arrivée <> numéro de série
    Q2 := (S.GetNoPointDep() <> 0);
    Q3 := (S.GetNoSerieArr() = S.GetNumeroDeSerie());
    if (Q3 and Q2) then qSerie.SetNoPointArr(S.GetNbVisees() - 1);
    if (Q1 and Q2) then
    begin
      qSerie.SetNoSerieArr(S.GetNumeroDeSerie());
      qSerie.SetNoPointArr(S.GetNbVisees() - 1);
    end;
    qSerie.SetNumeroSerie(S.GetNumeroDeSerie());
    qSerie.SetNumeroReseau(S.GetNumeroReseau());
    qSerie.SetRaideur(1.00); // n'intervient pas dans une coupe développée
    qSerie.SetCouleur(S.GetCouleur);
    qSerie.SetAccroissements(S.GetAccroissementX(), S.GetAccroissementY(), S.GetAccroissementZ(), S.GetAccroissementP());

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
    Q1 := True;
    //Q1 := IIF(self.GetNbSeries() = 0, True, VerifierSiSerieAUnAntecedentConnu(qSerie) <> 0);
    //AfficherMessage('--- Ancrage ' + BoolToStr(Q1, 'connu', 'inconnu'));
    Q2 := VerifierSiSerieEstUnique(qSerie);
    //AfficherMessage('--- Unique ? ' + BoolToStr(Q2, 'Oui', 'Non'));
    if (Q1 and Q2) then
    begin
      FListeDesSeries.AddElement(qSerie);
      Result := true;
    end
    else
    begin
      result := false;
      // on détruit l'objet (fuites de mémoire sinon)
      qSerie.ClearStations();
      FreeAndNil(qSerie);//qSerie.Free;
      AfficherMessage('*** Série non raccordée au réseau');
    end;

  except
    FreeAndNil(qSerie);
  end;
end;


function  TCoupeDeveloppee.GetSerie(const Idx: integer): TObjSerie;
begin
  result := FListeDesSeries.GetElement(Idx);
end;

function TCoupeDeveloppee.GetSerieByNumeroSerie(const Idx: TNumeroSerie; out SR: TObjSerie): boolean;
var
  i, Nb: Integer;
begin
  Result := false;
  Nb := self.GetNbSeries();
  if (Nb = 0) then exit;
  for i := 0 to Nb - 1 do
  begin
    SR := GetSerie(i);
    if (Idx = SR.GetNumeroDeSerie()) then Exit(True);
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

    except
    end;
    FreeAndNil(OS);//OS.Free;
  finally
    FListeDesSeries.RemoveElement(Idx);
  end;
end;
// Les noeuds
function TCoupeDeveloppee.GetNbJonctions(): integer;
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



function TCoupeDeveloppee.GetNbBranches(): integer;
begin
  Result := FListeDesBranches.GetNbElements();
end;

procedure TCoupeDeveloppee.AddBranche(const S: TBrancheCoupeDeveloppeeAsRecord; const DoCalcPZ: boolean);
var
  EWE: TBrancheCoupeDeveloppeeAsRecord;
begin
  EWE := S;
  if (DoCalcPZ) then EWE := CalcDeltaPZ(EWE);
  FListeDesBranches.AddElement(EWE);
end;

function TCoupeDeveloppee.GetBranche(const Idx: integer): TBrancheCoupeDeveloppeeAsRecord;
begin
  Result := FListeDesBranches.GetElement(Idx);
end;

procedure TCoupeDeveloppee.PutBranche(const Idx: integer; const BRCH: TBrancheCoupeDeveloppeeAsRecord);
begin
  FListeDesBranches.PutElement(Idx, BRCH);
end;

function TCoupeDeveloppee.FindIndexBrancheNearToXY(const X, Y: double; out QNumSerie, QNumStation: integer): integer;
var
  i, Nb: Integer;
  R, Rmax: double;
  Br: TBrancheCoupeDeveloppeeAsRecord;
  JC1, JC2: TJonctionCoupeDeveloppee;
  v, NbS: Integer;
  qP, qZ: double;
  myVisee: TUneVisee;
begin
  Result := -1;
  Rmax   := 1E24;
  Nb := self.GetNbBranches;
  if (Nb = 0) then Exit;
  for i := 0 to Nb-1 do
  begin
    Br  := GetBranche(i);
    JC1 := GetJonction(Br.NumeroNoeudDepart);
    JC2 := GetJonction(Br.NumeroNoeudArrivee);
    NbS := 1 + High(Br.ArrVisees);
    if (NbS = 0) then
    begin
      qP := 0.50 * (JC1.Abscisse + JC2.Abscisse);
      qZ := 0.50 * (JC1.Cote     + JC2.Cote);
      R  := Hypot2D(X - qP, Y - qZ);
      if (R < RMax) then
      begin
        RMax   := R;
        QNumSerie   := Br.NumeroSerie;
        QNumStation := 0;
        Result := i;

      end;
    end
    else
    begin
      qP := JC1.Abscisse;
      qZ := JC1.Cote;
      for v := 0 to NbS - 1 do
      begin
        myVisee := Br.ArrVisees[v];
        qP += myVisee.AccroissP * iif((Br.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
        qZ += myVisee.AccroissXYZ.Z;
        R  := Hypot2D(X - qP, Y - qZ);
        if (R < RMax) then
        begin
          RMax := R;
          QNumSerie   := Br.NumeroSerie;
          QNumStation := myVisee.NoVisee;
          Result := i;

        end;
      end;
    end;
  end;
  //AfficherMessage(Format('FindIndexBrancheNearToXY: %.2f, %.2f = %d', [X, Y, Result]));
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
                                      const QNomBranche: string): TBrancheCoupeDeveloppeeAsRecord;
var
  V: TUneVisee;
begin
  result.NumeroBranche      := QNumeroBranche;
  Result.NumeroReseau       := QNumeroReseau;

  Result.NomBranche         := QNomBranche;
  Result.NumeroSerie        := QNoSerie;
  Result.NumeroNoeudDepart  := QNoeudDepart;
  Result.NumeroNoeudArrivee := QNoeudArrivee;
  Result.DeltaP             := 0.01;
  Result.DeltaZ             := 0.01;
  Result.SensTraceCoupe     := stcdVERS_DROITE;
  SetLength(Result.ArrVisees, 0);
  V.Empty();
  V.setFrom(0, tgDEFAULT, 0, 0, 0, 0.01, 0.00, -45.00, 0.0, 0.0, 0.0, 0.0, '', '');
  addViseeAtBranche(Result, V);
end;
procedure TCoupeDeveloppee.addViseeAtBranche(var BR: TBrancheCoupeDeveloppeeAsRecord; const V: TUneVisee);
var
  n: Integer;
begin
  n := Length(BR.ArrVisees);
  SetLength(BR.ArrVisees, n + 1);
  n := High(BR.ArrVisees);
  BR.ArrVisees[n] := V;
  BR.NbVisees := 1 + n;
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
procedure TCoupeDeveloppee.SetMiniMaxi();
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
    NbS, j: Integer;
    QSD, QPD, QSA, QPA: Integer;
    MySerie: TObjSerie;
    EWE: TUneVisee;

  begin
    Result := false;
    QSD := S.GetNoSerieDep();
    QPD := S.GetNoSerieDep();
    QSA := S.GetNoSerieArr();
    QPA := S.GetNoSerieArr();
    if (not GetSerieByNumeroSerie(S.GetNoSerieDep(), MySerie)) then exit;
    NbS := MySerie.GetNbVisees();
    if (NbS = 0) then Exit;
    for j := 0 to NbS - 1 do
    begin
      EWE := MySerie.GetVisee(j);
      //AfficherMessageErreur(Format('(%d.%d) - %d.%d', [QSD, QPD, MySerie.GetNumeroDeSerie(), j]));
      case EE of
        Q_DEPART :
        begin
          if (QPD = j) then
          begin
            result := true;
            Break;
          end;
        end;
        Q_ARRIVEE:
        begin
          pass;
        end;
      end;
    end;
  end;
begin
  Result := 0;

  Nb := self.GetNbSeries();
  (*
  AfficherMessageErreur(format('%s.VerifierSiSerieAUnAntecedentConnu(%d series): Serie: %d (%d.%d) > (%d.%d) - %s',
                              [ClassName, Nb,
                               S.GetNumeroDeSerie(),
                               S.GetNoSerieDep(), S.GetNoPointDep(),
                               S.GetNoSerieArr(), S.GetNoPointArr(),
                               S.GetNomSerie()]));
  //*)

  // si la coupe n'a pas de série, poser VRAI
  if (Nb = 0) then
  begin
    Result := 1;
    AfficherMessageErreur('*** La coupe est vide ***');
    Exit;
  end;
  //AfficherMessageErreur(Format('-- AUneExtremiteConnue(%d)', [Nb]));
  if (AUneExtremiteConnue(Q_DEPART))  then begin Result :=  1; exit; end;
end;

function TCoupeDeveloppee.VerifierSiSerieEstUnique(const S: TObjSerie): boolean;
var
  i, Nb: Integer;
  EWE: TObjSerie;
  NbSeriesDoublon: Integer;
begin
  Result := false;
  Nb := self.GetNbSeries;
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
    if (S.GetNumeroDeSerie() = EWE.GetNumeroDeSerie()) then NbSeriesDoublon += 1;
  end;
  Result := (NbSeriesDoublon < 1);
end;



function TCoupeDeveloppee.FindAccrochageCoordsPZSerie(const S: TObjSerie): TPointCoupeDeveloppee;
var
  QS: Integer;
  QP: Integer;
  EWE: TObjSerie;
  VS: TUneVisee;
begin
  Result.P := 0.00;
  Result.Z := 0.00;
  try
    QS := S.GetNoSerieDep();
    QP := S.GetNoPointDep();
    if (not GetSerieByNumeroSerie(QS, EWE)) then exit;
    //AfficherMessage(Format('Serie accrochage: %d - %s', [EWE.GetIndexSerie, EWE.GetNomSerie]));
    //Nb := EWE.GetNbVisees;
    //if (nb = 0) then Exit;
    //for i := 0 to nb - 1 do
    //begin
      VS := EWE.GetVisee(QP);
      Result.P := VS.AccroissP;
      Result.Z := VS.AccroissXYZ.Z; //VS.AccroissZ;
    //end;
  except
    pass;
  end;
end;

function TCoupeDeveloppee.Initialiser(const DT: TToporobotStructure2012): boolean;
begin
  Result := False;
  try
    FScript           := TStringList.Create;
    //FScript.Sorted    := True;
    //FScript.Duplicates:= dupIgnore;

    FListeDesSeries   := TListeSimple<TObjSerie>.Create;
    FListeDesJonctions:= TListeSimple<TJonctionCoupeDeveloppee>.Create;
    FListeDesBranches := TListeSimple<TBrancheCoupeDeveloppeeAsRecord>.Create;
    FDocuToporobot    := DT;

    AfficherMessage(Format('%s.Initialise avec DocTopo (%d series)', [ClassName, FDocuToporobot.GetNbSeries]));
    ResetAll();

  except
  end;
end;


procedure TCoupeDeveloppee.ExporterCoupeGCP(const FichierGCP: TStringDirectoryFilename);
const
  ABSCISSE_ORIGINE =  10000;
var
  fp: TextFile;
  NbBr, i, NbV, v: Integer;
  E: TBaseStation;
  JC1, JC2: TJonctionCoupeDeveloppee;
  Brch: TBrancheCoupeDeveloppeeAsRecord;
  PM: TPointCoupeDeveloppee;
  myVisee: TUneVisee;
  uP, uZ: double;
  QID: TIDBaseStation;
  procedure WrtLn(const S: string); inline;
  begin
    writeln(fp, S);
  end;
begin
  AfficherMessage(format('%s.ExporterCoupeGCP: %s', [classname, FichierGCP]));
  AssignFile(fp, FichierGCP);
  try
    ReWrite(fp);
    WrtLn(Format('# Coupe developpee: %s - %s', [FichierGCP, DateTimeToStr(Now())]));
    WrtLn('# Base points');
    WrtLn('');
    WrtLn('basepoints');
    NbBr := self.GetNbBranches();
    for i := 0 to NbBr - 1 do
    begin
      Brch := GetBranche(i);
      JC1 := self.GetJonction(Brch.NumeroNoeudDepart);
      JC2 := self.GetJonction(Brch.NumeroNoeudArrivee);

      PM := MakeTPointCoupeDeveloppee(JC1.Abscisse, JC1.Cote);
      NbV := 1 + High(Brch.ArrVisees);
      E.PosExtr0.X := JC1.Abscisse + ABSCISSE_ORIGINE;
      E.PosExtr0.Y := JC1.Cote;
      E.PosExtr0.Z := JC1.Cote;

      if (NbV > 0) then
      begin
        uP := 0.00;
        uZ := 0.00;
        for v := 1 to NbV - 1 do
        begin
          myVisee := Brch.ArrVisees[v]; //myVisee := myBranche.GetVisee(v);
          //uP += myVisee.DeltaP * IIF((Brch.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
          //uZ += myVisee.DeltaZ;
          uP += myVisee.AccroissP * IIF((Brch.SensTraceCoupe = stcdVERS_DROITE), 1.00, -1.00);
          uZ += myVisee.AccroissXYZ.Z;

          PM := MakeTPointCoupeDeveloppee(JC1.Abscisse + uP, JC1.Cote + uZ);

          E.Entite_Serie := Brch.NumeroSerie;
          E.PosStation.X := PM.P + ABSCISSE_ORIGINE;
          E.PosStation.Y := PM.Z;
          E.PosStation.Z := PM.Z;
          E.Entite_Station  := myVisee.NoVisee;
          E.IDTerrain       := myVisee.IDTerrainStation;
          E.Type_Entite     := myVisee.TypeVisee;
          E.PosPD.X         := E.PosStation.X;
          E.PosPD.Y         := E.PosStation.Y;
          E.PosPD.X         := E.PosStation.X;
          E.PosPD.Y         := E.PosStation.Y;
          E.PosPG.Z         := E.PosStation.Z - myVisee.HN;
          E.PosPD.Z         := E.PosStation.Z + myVisee.HZ;
          QID := MakeTIDBaseStation(E.Entite_Serie, E.Entite_Station, false);


          // ajouter l'entité
          WrtLn(Format(FMT_BASEPOINTS,
               [QID  ,
                E.IDTerrain,
                E.Type_Entite,
                clBlue, //GetCouleurEntiteRGBByExpe(E), //E.ColorEntite,
                E.PosExtr0.X, E.PosExtr0.Y, E.PosExtr0.Z,
                E.PosStation.X, E.PosStation.Y, E.PosStation.Z,
                E.PosPD.X, E.PosPG.Z,  E.PosPG.Z,
                E.PosPD.X, E.PosPD.Z,  E.PosPD.Z
               ]));
          // et préparer la suivante
          E.PosExtr0 := E.PosStation
        end;
      end;
    end;

    WrtLn('endbasepoints');
  finally
    CloseFile(fp);
  end;

end;
// rechercher la branche qui contient un  couple série/station
function TCoupeDeveloppee.FindIdxBrancheContainingSerieStation(const QSer, QSt: integer): integer;
var
  i, NbBranches: Integer;
  MyBranche: TBrancheCoupeDeveloppeeAsRecord;
  JD, JA: TJonctionCoupeDeveloppee;
begin
  Result := -1;
  NbBranches := GetNbBranches();
  if (0 = NbBranches) then exit;
  for i := 0 to NbBranches - 1 do
  begin
    MyBranche := GetBranche(i);
    //AfficherMessageErreur(Format('Branche: %d - %d = %d ?', [i, MyBranche.NumeroSerie, QSer]));
    if (MyBranche.NumeroSerie = QSer) then
    begin
      JD := GetJonction(MyBranche.NumeroNoeudDepart);
      JA := GetJonction(MyBranche.NumeroNoeudArrivee);
      //AfficherMessageErreur(Format('--> La branche %d est dans la série %d - Elle part de %d.%d et arrive sur  %d.%d', [i, QSer, JD.NoSerie, JD.NoPoint, JA.NoSerie, JA.NoPoint]));
      if (JD.NoSerie = QSer) then
      begin
        if (IsInRange(QSt, JD.NoPoint, JA.NoPoint)) then Exit(i);
      end
      else
      begin
        if (QSt <= JA.NoPoint) then exit(i);
      end;

    end;
  end;
end;



procedure TCoupeDeveloppee.Finaliser();
begin
  try
    ResetAll();
  finally
    FreeAndNil(FListeDesSeries);//FListeDesSeries.Free;
    FreeAndNil(FListeDesJonctions);//FListeDesJonctions.Free;
    FreeAndNil(FListeDesBranches);//FListeDesBranches.Free;
    FreeAndNil(FScript);
  end;
end;

procedure TCoupeDeveloppee.ResetAll();
begin
  FScript.Clear;
  ViderListeSeries();
  FListeDesJonctions.ClearListe();
  FListeDesBranches.ClearListe();
end;

procedure TCoupeDeveloppee.ViderListeSeries();
var
  ii, Nb: Integer;
  UneSerie, OS: TObjSerie;
  MyVisee: TUneVisee;
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
      except
      end;
      FreeAndNil(OS);
    end;
  end;
  FListeDesSeries.ClearListe();
  // on ajoute une série nulle
  UneSerie := TObjSerie.Create;
  try
    UneSerie.ClearStations();
    UneSerie.SetChanceObstacle(0,0);
    UneSerie.SetSeriePtExtremites(0,0,1,0);
    UneSerie.SetNumeroSerie(0);
    UneSerie.SetNomObsSerie(rsMSG_SERIE_INITIALISATION, '');
    UneSerie.SetRaideur(1.00);
    UneSerie.SetCouleur(clBlack);
    UneSerie.SetNumeroReseau(0);
    UneSerie.SetNumeroEntrance(0);
    MyVisee.Empty('Point 1.0');
    UneSerie.AddVisee(MyVisee);
  except
    FreeAndNil(UneSerie);
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


function TCoupeDeveloppee.GetMaxIdxNode: integer;
var
  Branche: TBrancheCoupeDeveloppeeAsRecord;
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

function TCoupeDeveloppee.RecenserLesJonctions(): integer;
var
  i  : Integer;
  EWE: TObjSerie;
  NbS: Integer;
  WU: TGHStringArray;
  ListeJnct: TStringList;
  Ser: Integer;
  Serie: TObjSerie;
  St: Integer;
  J : TJonctionCoupeDeveloppee;
  Entree: TEntrance;
begin
  FListeDesJonctions.ClearListe();
  AfficherMessageErreur(Format('%s.RecenserLesJonctions',[self.ClassName]));
  result := -1;
  AfficherMessage(GetResourceString(rsRECENSEM_JONC));
  ListeJnct := TStringList.Create;
  try
    ListeJnct.Clear;
    ListeJnct.Sorted :=True;
    ListeJnct.Duplicates :=dupIgnore;
    //ListeJnct.Add(Format(FMT_NDSER_PT,[0, 0]));
    //ListeJnct.Add(Format(FMT_NDSER_PT,[1, 0]));
    //AfficherMessageErreur(Format('-- %d series', [self.GetNbSeries]));
    // Les série/point de départ/arrivée sont forcément des jonctions (noeuds)
    for Ser := 0 to self.GetNbSeries - 1 do
    begin
      Serie := self.GetSerie(Ser);
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
    //ListerLesJonctions();
    // On passe ici ? OK, c'est bon
    Result := GetNbJonctions();
  finally
    FreeAndNil(ListeJnct);
  end;
end;

procedure TCoupeDeveloppee.ViderListeBranches();
begin
  FListeDesBranches.ClearListe();
end;

function TCoupeDeveloppee.RecenserLesBranches(): boolean;
var
  NbrDeSeries: Integer;
  NbrDeJonctions: Integer;
  Branche0: TBrancheCoupeDeveloppeeAsRecord;

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
  AfficherMessage(Format('%s: %d series et %d jonctions', [GetResourceString(rsFINDING_BRANCHES), NbrDeSeries, NbrDeJonctions]));

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

    Branche0 := self.initBranche(FDocuToporobot, Br, mySerie.GetNumeroDeSerie(), mySerie.GetNumeroReseau(), Nd1.NumeroJonction, Nd2.NumeroJonction, qsetNomBranche(Br));
    NbreStations := mySerie.GetNbVisees();
    for v := 1 to NbreStations - 1 do
    begin
      myVisee := mySerie.GetVisee(v);
      Nd1 := self.GetJonctionBySerSt(mySerie.GetNumeroDeSerie(), v);
      Nd2 := self.GetJonctionBySerSt(mySerie.GetNumeroDeSerie(), v);
      // mettre à jour le numéro de visée si ce n'est pas déjà fait
      myVisee.NoVisee := v;
      addViseeAtBranche(Branche0, myVisee); //      Branche0.addVisee(myVisee);
      if ((Nd1.NumeroJonction > -1) and (v < (NbreStations - 1))) then
      begin
        // clôturer la branche courante
        Branche0.NumeroNoeudArrivee := Nd1.NumeroJonction;
        self.addBranche(Branche0, True);
        // et creer la suivante
        Br += 1;
        Branche0 := initBranche(FDocuToporobot, Br, mySerie.GetNumeroDeSerie(), mySerie.GetNumeroReseau, Nd1.NumeroJonction, Nd2.NumeroJonction, qsetNomBranche(Br));
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

end;

procedure TCoupeDeveloppee.ListerBranchesDUneSerie(const LSB: TListBox; const Idx: integer);
var
  Nb, i: Integer;
  BR: TBrancheCoupeDeveloppeeAsRecord;
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
      if (BR.NumeroSerie = SR.GetNumeroDeSerie()) then
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
  EWE       : TObjSerie;
  WU        : TUneVisee;
  Nearest, r: double;
  nb, ns    : Integer;
  i, s      : Integer;
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
      r  := sqr(P - WU.AccroissP) + sqr(Z - WU.AccroissXYZ.Z); //AccroissZ);
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
    //AfficherMessage(Format('-- %s.RecenserJonctionsEtBranches(): %d Jonctions et %d branches', [self.ClassName, GetNbJonctions, GetNbBranches]));
    Result := (self.GetNbBranches() > 0);
  except
  end;
end;

// construire la liste du graphe
function TCoupeDeveloppee.ConstruireLaCoupeDeveloppee(const DoRegenGraphe: boolean): boolean;
begin
  AfficherMessage(Format('%s.ConstruireGrapheEnListe: %d series', [ClassName, GetNbSeries]));
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
end;

function TCoupeDeveloppee.LoadScriptCoupe(const QFilename: string): boolean;
var
  NS: LongInt;
  n, i, QInternalIdxSerie: Integer;
  SR: TObjSerie;
begin
  AfficherMessage(Format('%s.LoadCoupe: %s', [ClassName, QFilename]));
  Result := false;
  try
    FScript.Clear;
    FScript.LoadFromFile(QFilename);
    n := FScript.Count;
    if (n = 0) then Exit;
    for i := FScript.Count - 1 downto 0 do      // nettoyer les lignes vides
    begin
      if (trim(FScript.Strings[i]) = '') then FScript.Delete(i);
    end;
    result := (FScript.Count > 0);
  finally
    pass;
  end;
end;
// Aucun traitement à faire: le script est maintenu en permanence
procedure TCoupeDeveloppee.SaveScriptCoupe(const QFilename: string);
begin
  FScript.SaveToFile(QFilename);
end;
function TCoupeDeveloppee.InterpreterScript(): boolean;
var
  Nb, i, QIdx: Integer;
  EWE: TGHStringArray;
  NS, QNumSerie, QNumStation: LongInt;
  MyLigne: String;
  QSR: TObjSerie;
begin
  Result := false;
  ViderListeSeries();
  Nb := FScript.Count;
  if (Nb = 0) then exit;
  for i := 0 to Nb - 1 do
  begin
    MyLigne := LowerCase(Trim(FScript.Strings[i]));
    //remplacer tous les séparateurs
    MyLigne := StringReplace(MyLigne, ' ' , #9, [rfReplaceAll]);
    MyLigne := StringReplace(MyLigne, '//', #9, [rfReplaceAll]);
    MyLigne := StringReplace(MyLigne, '.', #9, [rfReplaceAll]);
    EWE := Split(MyLigne, #9);
    if (Trim(EWE[0]) = 'addserie') then
    begin
      NS := StrToIntDef(EWE[1], -1);
      if (NS > 0) then
      begin
        if (FDocuToporobot.GetSerieByNumeroSerie(NS, QSR, QIdx)) then self.AddSerie(QSR, True);
      end;
    end;
  end;
  // Premier calcul
  ConstruireLaCoupeDeveloppee(True);
  for i := 0 to Nb - 1 do
  begin
    MyLigne := LowerCase(Trim(FScript.Strings[i]));
    //remplacer tous les séparateurs
    MyLigne := StringReplace(MyLigne, ' ' , #9, [rfReplaceAll]);
    MyLigne := StringReplace(MyLigne, '//', #9, [rfReplaceAll]);
    MyLigne := StringReplace(MyLigne, '.', #9, [rfReplaceAll]);
    EWE := Split(MyLigne, #9);
    // basculer les branches
    if (Trim(EWE[0]) = 'turnbranchofserst') then
    begin
      QNumSerie     := StrToIntDef(EWE[1], -1);
      QNumStation   := StrToIntDef(EWE[2], 0);
      if (QNumSerie > 0) then
      begin
        QIdx := FindIdxBrancheContainingSerieStation(QNumSerie, QNumStation);
        if (QIdx > 0) then
        begin
          self.BasculerSensDessinBrancheByIdx(QIdx);
        end;
      end;
    end;

  end;
  // recalculer SANS reconstruire le graphe du réseau
  result := ConstruireLaCoupeDeveloppee(false);



end;




procedure TCoupeDeveloppee.AfficherListeJonctions();
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
  EWE: TBrancheCoupeDeveloppeeAsRecord;
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
begin
end;
(*
const
  NOM_STYLE_ECHELLE            = 'StyleEchelle';
  NOM_STYLE_CROSS_SECTION      = 'StyleCrossSection';
  NOM_STYLE_STATION_MARKER     = 'StyleStationMarker';
  NOM_STYLE_STATION_MARKER_TXT = 'StyleStationMarkerText';
  NOM_STYLE_ECHELLE_TXT        = 'StyleEchelleText';



  FMT_ID_GROUPE_BRANCHE        = 'Branche%d';
  FMT_STYLE_CENTERLINE_SECTEUR = 'StyleCenterlinesSecteur_%d';
  FMT_STYLE_CENTERLINE_RESEAU  = 'StyleCenterlinesReseau_%d';

var
  MyContexteSVG: TSVGCanvas;
  i, Nb: Integer;
  BR: TBrancheCoupeDeveloppeeAsRecord;
  EWE: TReseau;
  function DrawCenterLineBranche(const B: TBrancheCoupeDeveloppeeAsRecord): boolean;
  const
    MARKER_RADIUS = 0.500;
  var
    s, nbs: Integer;
    myVisee: TUneVisee;
    JC1: TJonctionCoupeDeveloppee;
    JC2: TJonctionCoupeDeveloppee;
    uP: double;
    uZ: double;
    WU: String;
    LS: TArrayPoints2Df;
    DeltaP: double;
    DeltaZ: double;
  begin
    Result := false;

    JC1 := GetJonction(B.NumeroNoeudDepart);
    JC2 := GetJonction(B.NumeroNoeudArrivee);
    SetLength(LS, 0);
    //nbs := B.GetNbVisees;
    nbs := 1 + High(B.ArrVisees);
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
          myVisee := B.ArrVisees[s];   //myVisee := B.GetVisee(s);
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
                             Format(FORMAT_NB_REAL_2_DEC, [JC2.Cote])
                             );
      //MyContexteSVG.D;
      MyContexteSVG.EndGroupe(WU);
    end;

  end;
  procedure DrawEchelle(const Taille: double);
  const
    NOM_GROUPE_ECHELLE = 'Echelle';
    QWIDTH_ECHELLE     = 2.00;
  var
    BB: TBrancheCoupeDeveloppeeAsRecord;
    JC1: TJonctionCoupeDeveloppee;
  begin
    AfficherMessage('-- DrawEchelle');
    // attrapper les coordonnées du point de départ de la première station
    BB := FListeDesBranches.GetElement(0);
    JC1 := GetJonction(BB.NumeroNoeudDepart);


    MyContexteSVG.BeginGroupe(NOM_GROUPE_ECHELLE, 'Echelle', JC1.Abscisse - 5.00, JC1.Cote);
      MyContexteSVG.DrawRectangle(NOM_STYLE_ECHELLE, 0.0, 0.0, QWIDTH_ECHELLE, -Taille);
      MyContexteSVG.DrawText(NOM_STYLE_ECHELLE_TXT, 4, QWIDTH_ECHELLE + 0.1, 0.0, 0.0, Format('%.0f', [0.00]));
      MyContexteSVG.DrawText(NOM_STYLE_ECHELLE_TXT, 4, QWIDTH_ECHELLE + 0.1, -Taille, 0.0, Format('%.0f m', [Taille]));


    MyContexteSVG.EndGroupe(NOM_GROUPE_ECHELLE);
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
      MyContexteSVG.BeginStylesSection();     // styles des objets
        // styles d'échelle
        MyContexteSVG.WriteStyleSolidPolygone(NOM_STYLE_ECHELLE, clBlack, clSilver, 255, 0.1, '');
        MyContexteSVG.WriteStyleText(NOM_STYLE_ECHELLE_TXT,
                                     DEFAULT_FONT_NAME,
                                     [],
                                     clBlack,
                                     0.3, '');
        // marqueurs de stations
        MyContexteSVG.WriteStyleSolidPolygone(NOM_STYLE_STATION_MARKER, clRed, clYellow, 255, 0.1, '');
        MyContexteSVG.WriteStyleText(NOM_STYLE_STATION_MARKER_TXT,
                                     DEFAULT_FONT_NAME,
                                     [fsBold],
                                     clBlue,
                                     0.35, 'Style text station marker');
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
      MyContexteSVG.EndStylesSection();
      //MyContexteSVG.BeginGroupe('Coupe', '', 0.00, 0.00);
      MyContexteSVG.BeginDrawingSection();
        // groupes; 1 groupe = 1 branche
        Nb := GetNbBranches;
        for i := 0 to Nb-1 do
        begin
          BR := GetBranche(i);
          DrawCenterLineBranche(BR);
        end;
        //MyContexteSVG.EndGroupe('Coupe');
        // échelle
        DrawEchelle(50.00);
      MyContexteSVG.EndDrawingSection();
      MyContexteSVG.FinalizeDocument();
    end;
  finally
    FreeAndNil(MyContexteSVG);//MyContexteSVG.Free;
  end;
end;
//*)
// Calcul des coordonnées des noeuds
procedure TCoupeDeveloppee.CalculerCoordNoeuds();
const
  NBSTEPS = 10;
var
  R, B: TMatrix;
  A, Delta: TVecteurDouble;
  n, QAxe, njc: integer;
  MyJonction: TJonctionCoupeDeveloppee;
  // matrice de connexion (ici, matrice d'incidence)
  function GetMaxNode(): integer;
  var
    Branche: TBrancheCoupeDeveloppeeAsRecord;
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
    Branche: TBrancheCoupeDeveloppeeAsRecord;
    EWE: String;
    JC1, JC2: TJonctionCoupeDeveloppee;
    function beuh(const QB: TBrancheCoupeDeveloppeeAsRecord; const QJ: integer): double;
    begin
       if      (QB.NumeroNoeudDepart  = j) then Result := -1.0
       else if (QB.NumeroNoeudArrivee = j) then Result := +1.0
                                           else Result :=  0.0;
    end;
  begin
    SetLength(R, 0,0);
    SetLength(R, GetNbBranches() + 1, n + 1);
    AfficherMessage(Format('%s.MakeRMatrix', [ClassName]));
    for i:= 0 to GetNbBranches - 1 do
    begin
      Branche := GetBranche(i);
      for j := 0 to n do R[i,j] := beuh(Branche, j);
    end;
    (*
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

    end;
    //*)
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
    AfficherMessage(GetResourceString(rsBUILDMATRICE));
    qNbBr := self.GetNbBranches();
    AfficherMessage(Format('MakeBMatrix: %d branches, %d noeuds', [qNbBr, n]));
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
        if (Abs(R[j, i]) > 0.00) then
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
        if (Abs(R[j,i]) > 0.00) then
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
        for k:=LowIndex[i] to HighIndex[i] do ww := ww + R[k,i] * R[k,j];
        //for k:=1 to qNbBr - 1 do ww := ww + R[k,i] * R[k,j];
        B[i,j]:=ww;
      end;
    end;
    // remplissage de la symétrie
    for i:=1 to N-1 do
      for j:=1+i to N do
        B[i,j]:=B[j,i];
    SetLength(LowIndex,0);
    SetLength(HighIndex,0);
    // affichage de la matrice (contrôle)
    (*
    EWE := 'Matrice de compensation';
    AfficherMessageErreur(EWE);
    for i := 0 to GetNbBranches-1 do
    begin
      EWE := '';
      for j := 0 to n do
        EWE += Format('%03.0f;  ', [B[i][j]]);
      AfficherMessageErreur(EWE);
    end;
    //*)
  end;

  // accroissements des branches
  procedure MakeAVector(const Axe: Byte);
  var
    k, qNbBrch: integer;
    Branche: TBrancheCoupeDeveloppeeAsRecord;
  begin
    qNbBrch := GetNbBranches();
    AfficherMessage(Format('-- MakeAVector: %d', [qNbBrch]));
    //AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    SetLength(A, 0);
    SetLength(A, qNbBrch);
    for k := 0 to High(A) do A[k] := 0.00;
    for k:= 0 to qNbBrch - 1 do
    begin
      Branche:=GetBranche(k);
      case Axe of
        1: A[k] := Branche.DeltaP;
        2: A[k] := Branche.DeltaZ;
      end;
    end;
    //for k := 0 to high(A) do AfficherMessageErreur(Format(' --- A[%d] = %.2f', [k, A[k]]));

  end;
  // second membre
  procedure MakeDeltaVector();
  var
    i,k    : integer;
    ww     : double;
  begin;
    SetLength(Delta, 0);
    SetLength(Delta, n+1);
    AfficherMessage(Format('-- MakeDeltaVector: %d', [n+1]));
    for i:=1 to n do
    begin
      ww := 0;
      for k:=1 to GetNbBranches() - 1 do
      begin
        ww := ww + R[k,i] * A[k];
      end;
      Delta[i]:=ww;
    end;
    //for k := 0 to high(A) do AfficherMessageErreur(Format(' --- Delta[%d] = %.2f', [k, Delta[k]]));
    //AfficherMessage(Format('-- MakeDeltaVector: %d OK', [n+1]));
  end;
  // résolution
  procedure SolveMatrix(const Axe: byte);
  var
    i, j, k   : integer;
    vv        : double;
    ww        : double;
    V_Matrix  : TMatrix; // matrice locale de factorisation
    S_Vector  : TVecteurDouble; // vecteur local complémentaire
    XX        : TVecteurDouble;
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
      AfficherMessage(GetResourceString(rsDESCENTE));
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
      AfficherMessage(GetResourceString(rsTRIANGULARISATION));
      for i:=1 to n do
      begin
        if (i mod 100 = 0) then AfficherMessage(Format(GetResourceString(rsLINEOFNB),[i, n]));
        ww := 0;
        for k:=1 to i-1 do ww := ww + V_Matrix[i,k] * S_Vector[k];
        S_Vector[i]  := (Delta[i] - ww) / (V_Matrix[i,i] + 1e-24);
      end;
      // remontée du système; inconnues recherchées
      AfficherMessage(GetResourceString(rsREMONTEE));
      for i := n downto 1 do
      begin
        ww := 0;
        for k:=1+i to n do ww := ww+ V_Matrix[k,i] * XX[k];
        XX[i] := (S_Vector[i] - ww) / (V_Matrix[i,i] + 1e-24);
      end;
      XX[0] := XX[1];
      //AfficherMessage(Format('Axe: %s - Table des noeuds', [chr(87+Axe)]));
      AfficherMessage(format('Table des noeuds (%d jonctions)', [GetNbJonctions()]));

      // Table des Noeuds
      for i:=1 to n do
      begin
        XX[i]:=XX[i]-XX[0];
        JC := GetJonction(i-1);
        case Axe of
          1: JC.Abscisse := XX[i-1];  //
          2: JC.Cote     := XX[i-1];  //
        end;
        //AfficherMessage(Format('-- %d : %.3f, %.3f', [i-1, JC.Abscisse, JC.Cote]));
        PutJonction(i-1, JC);
      end;
      AfficherMessage('Table des noeuds OK');
      // jonction 0
      JC := GetJonction(0);
      JC.Abscisse := 0.01;
      JC.Cote     := 0.01;

      PutJonction(0, JC);
      AfficherMessage('Jonctions OK');
      // ++++++++++++++++
      // Passage par cette ligne = c'est OK
      AfficherMessage(Format(GetResourceString(rsCOORDONNEES_OK),[n]));
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
  AfficherMessage(GetResourceString(rsBIND_INCIDENCE_MATRIX));
  n := GetMaxNode() + 1;
  MakeRMatrix; // matrice d'incidence (assemblage)
  MakeBMatrix; // matrice de compensation
  AfficherMessage('*** Calculs matriciels ***');
  for QAxe := 1 to 2 do
  begin
    AfficherMessage(Format('Axe %s', [chr(87+QAxe)]));
    AfficherMessage(GetResourceString(rsFACTORISEMATRICE));
    AfficherMessage(GetResourceString(rsAXIS) + ' ' + inttostr(QAxe));
    MakeAVector(QAxe);
    AfficherMessage(GetResourceString(rs2NDMEMBER));
    MakeDeltaVector;
    AfficherMessage(GetResourceString(rsNODECOORDINATES));
    SolveMatrix(QAxe);
  end;
  AfficherMessage('*** Calculs matriciels OK ***');
  // affichage coordonnées des jonctions
  (*
  n := GetNbJonctions();
  AfficherMessageErreur('Coordonnees jonctions');
  for njc :=IDX_MIN_JONCTION to n - 1 do
  begin
    MyJonction := GetJonction(njc);
    AfficherMessageErreur(Format('-- %d; %s; %d.%d;  %.2f; %.2f;', [njc, MyJonction.IDJonction,
                                                                    MyJonction.NoSerie, MyJonction.NoPoint,
                                                                    MyJonction.Abscisse, MyJonction.Cote]));

  end;
  //*)
  //-- finalisation
  AfficherMessage(GetResourceString(rsFREE_TEMP_VARS));
  FinalizeMatrices;
end;
//******************************************************************************
// Fonctions de graphes obsolètes
//******************************************************************************
// exploration du graphe:
// Modes: mpgEN_LARGEUR    = parcours en largeur,    utilise une file
// Modes: mpgEN_PROFONDEUR = parcours en profondeur, utilise une pile


end.

