unit GHTopoMultiThreading;
{$INCLUDE CompilationParameters.inc}
{$ASSERTIONS ON}
interface
uses
  StructuresDonnees,
  Common,
  {$IFDEF USE_MATRIX_DATASTRUCTURE_LIST_OF_LISTS}
  unitMatricesCreuses_LOT,                // OK, un peu plus rapide que LOL,
  {$ELSE}
  unitMatricesCreusesArray2D,
  {$ENDIF}
  UnitListesSimplesWithGeneriques,
  ToporobotClasses2012,
  UnitEntitesExtended,
  Classes, SysUtils, Graphics;

type

{ TThreadMakeCompensationMatrix }

 TThreadMakeCompensationMatrix = class(TThread)
private
  FDocTopo        : TToporobotStructure2012;
  FProcProgression: TProcOfObjectUsesInteger;
  FNbItems        : integer;
  FIdxStart       : integer;
  FIdxEnd         : integer;
  FNbThreads      : integer;
  FNoThread       : integer;
  // matrices
  FIncidenceMatrix    : TMatriceCreuse;
  FVecteurPonderation : TVecteurDouble;
  FCompensationMatrix : TMatriceCreuse;

  FMatrix_LowIndex    : TArrayOfIntegers;
  FMatrix_HighIndex   : TArrayOfIntegers;

protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants

  procedure Execute; override;
  procedure CalcCompensationMatrix();
public
  property Terminated; // cette propriété est 'protected' dans TThread
  Constructor Create(const DocTopo: TToporobotStructure2012;
                     const IncidenceMatrix: TMatriceCreuse;
                     const VecteurPonderation: TArrayOfFloats;
                     const Matrix_LowIndex  : TArrayOfIntegers;
                     const Matrix_HighIndex : TArrayOfIntegers;
                     const CompensationMatrix: TMatriceCreuse;
                     const NbThreads, NoThread: integer;
                     const NbItems, IdxStart, IdxEnd: integer;
                     const P: TProcOfObjectUsesInteger);

                     //const lPtr: PTMatrix);
  function AttendPour(): integer;
end;


type

{ TThreadFactoriseCompensationMatrix }

 TThreadFactoriseCompensationMatrix = class(TThread)
  private
    FDocTopo        : TToporobotStructure2012;
    FProcProgression: TProcOfObjectUsesInteger;
    FNbItems        : integer;
    FIdxStart       : integer;
    FIdxEnd         : integer;
    FNbThreads      : integer;
    FNoThread       : integer;
    // matrices
    FCompensationMatrix : TMatriceCreuse;
    FLMatrix            : TMatriceCreuse;
    FMatrix_LowIndex    : TArrayOfIntegers;
    FMatrix_HighIndex   : TArrayOfIntegers;

  protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants

    procedure Execute; override;
    procedure FactoriseCompensationMatrix();
  public
    property Terminated; // cette propriété est 'protected' dans TThread
    Constructor Create(const DocTopo: TToporobotStructure2012;

                       const CompensationMatrix: TMatriceCreuse;
                       const LMatrix: TMatriceCreuse;
                       const Matrix_LowIndex  : TArrayOfIntegers;
                       const Matrix_HighIndex : TArrayOfIntegers;

                       const NbThreads, NoThread: integer;
                       const NbItems, IdxStart, IdxEnd: integer;
                       const P: TProcOfObjectUsesInteger);

                       //const lPtr: PTMatrix);
    function AttendPour(): integer;
  end;
//******************************************************************************


Type

{ TThreadBranchesProcessing }

 TThreadBranchesProcessing = class(TThread)
private
  FDocTopo        : TToporobotStructure2012;
  FProcProgression: TProcOfObjectUsesInteger;
  FNbItems        : integer;
  FIdxStart       : integer;
  FIdxEnd         : integer;
  FNbThreads      : integer;
  FNoThread       : integer;
  // liste des branches
  FTableBranches  : TTableBranchesXYZ;

protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants

  procedure Execute; override;
  procedure CalcAccroissementBranches();
public
  property Terminated; // cette propriété est 'protected' dans TThread
  Constructor Create(const DocTopo: TToporobotStructure2012;
                     const TableBranches: TTableBranchesXYZ;
                     const NbThreads, NoThread: integer;
                     const NbItems, IdxStart, IdxEnd: integer;
                     const P: TProcOfObjectUsesInteger);
  function AttendPour(): integer;

                     //const lPtr: PTMatrix);
end;


Type

{ TThreadViseesEnAntennesProcessing }

  TThreadViseesEnAntennesProcessing = class(TThread)
private
  FDocTopo        : TToporobotStructure2012;
  FBDDEntites     : TBDDEntites;
  FProcProgression: TProcOfObjectUsesInteger;
  FNbItems        : integer;
  FIdxStart       : integer;
  FIdxEnd         : integer;
  FNbThreads      : integer;
  FNoThread       : integer;
  // liste des branches
  FTableBranches  : TTableBranchesXYZ;

protected   // = visible dans une instance d'une classe mais pas dans celle de ses descendants

  procedure Execute; override;
public
  property Terminated; // cette propriété est 'protected' dans TThread
  Constructor Create(const DocTopo: TToporobotStructure2012;
                     const BDDEntites: TBDDEntites;
                     const NbThreads, NoThread: integer;
                     const NbItems, IdxStart, IdxEnd: integer;
                     const P: TProcOfObjectUsesInteger);
  function AttendPour(): integer;

end;

implementation
uses
  DGCDummyUnit;

{ TThreadFactoriseCompensationMatrix }

procedure TThreadFactoriseCompensationMatrix.Execute;
begin
  Synchronize(nil);
  while (not Terminated) do
  begin
    FactoriseCompensationMatrix();
  end;

end;

procedure TThreadFactoriseCompensationMatrix.FactoriseCompensationMatrix();
var
  i, j, k, n: Integer;
  Q2, vv, ww: Double;
begin
  n := FNbItems;
  for i := FIdxStart to FIdxEnd do
  begin
    vv := 0;
    for j := 1 to i - 1 do
    begin
     Q2 := FLMatrix.GetValeur(i,j);
     vv += Q2 * Q2;
    end;
    FLMatrix.SetValeur(i,i, Sqrt(abs(FCompensationMatrix.GetValeur(i,i)) - vv));
    for j := 1 + i to n do
    begin
     ww := 0;
     for k := FMatrix_LowIndex[i] to i-1 do
     begin
       ww += FLMatrix.GetValeur(i,k) * FLMatrix.GetValeur(j,k);  // très forte accélération de la vitesse de calcul
     end;
     FLMatrix.SetValeur(j,i, (FCompensationMatrix.GetValeur(i,j) - ww) / (FLMatrix.GetValeur(i,i) + 1e-24));
    end;
  end;

end;

constructor TThreadFactoriseCompensationMatrix.Create(const DocTopo: TToporobotStructure2012;
                                                       const CompensationMatrix: TMatriceCreuse;
                                                       const LMatrix: TMatriceCreuse;
                                                       const Matrix_LowIndex  : TArrayOfIntegers;
                                                       const Matrix_HighIndex : TArrayOfIntegers;
                                                       const NbThreads, NoThread: integer;
                                                       const NbItems, IdxStart, IdxEnd: integer;
                                                       const P: TProcOfObjectUsesInteger);

var
  QIdxMax: Integer;
begin
  FDocTopo            := DocTopo;
  FCompensationMatrix := CompensationMatrix;
  FLMatrix            := LMatrix;
  FMatrix_LowIndex    := Matrix_LowIndex;
  FMatrix_HighIndex   := Matrix_HighIndex;
  FNbThreads          := NbThreads;
  FNoThread           := NoThread;
  FNbItems            := NbItems;
  FIdxStart           := IdxStart;
  FIdxEnd             := IdxEnd;
  QIdxMax             := FNbItems; // - 1;

  if (FIdxEnd > QIdxMax) then FIdxEnd := QIdxMax;
  FProcProgression := P;
  inherited Create(false); // false -> exécution immédiate

end;

function TThreadFactoriseCompensationMatrix.AttendPour(): integer;
begin
  //Result := WaitFor;
  Result := 0;
  self.Terminate;
end;


//******************************************************************************
// Matrice de compensation
{ TThreadMakeCompensationMatrix }

procedure TThreadMakeCompensationMatrix.Execute;
begin
  Synchronize(nil);
  while (not Terminated) do
  begin
    CalcCompensationMatrix();
  end;

end;

procedure TThreadMakeCompensationMatrix.CalcCompensationMatrix();
var
  i, j, k: Integer;
  ww  : Double;
begin
  for i := FIdxStart to FIdxEnd do
  begin
    if (assigned(FProcProgression)) then FProcProgression(FNoThread, FIdxStart, FIdxEnd, i);
    for j := 1 to i do
    begin
      ww := 0;
      for k := FMatrix_LowIndex[i] to FMatrix_HighIndex[i] do ww += FIncidenceMatrix.GetValeur(k,i) * FVecteurPonderation[k] * FIncidenceMatrix.GetValeur(k,j);
      FCompensationMatrix.SetValeur(i, j, WW);
    end;
  end;
end;

constructor TThreadMakeCompensationMatrix.Create(const DocTopo: TToporobotStructure2012;
                                                 const IncidenceMatrix: TMatriceCreuse;
                                                 const VecteurPonderation: TArrayOfFloats;
                                                 const Matrix_LowIndex: TArrayOfIntegers;
                                                 const Matrix_HighIndex: TArrayOfIntegers;
                                                 const CompensationMatrix: TMatriceCreuse;
                                                 const NbThreads, NoThread: integer;
                                                 const NbItems, IdxStart, IdxEnd: integer;
                                                 const P: TProcOfObjectUsesInteger);
var
  QIdxMax: Integer;
begin
  FDocTopo            := DocTopo;
  FIncidenceMatrix    := IncidenceMatrix;
  FVecteurPonderation := VecteurPonderation;
  FCompensationMatrix := CompensationMatrix;

  FMatrix_LowIndex    := Matrix_LowIndex;
  FMatrix_HighIndex   := Matrix_HighIndex;
  FNbThreads          := NbThreads;
  FNoThread           := NoThread;
  FNbItems            := NbItems;
  FIdxStart           := IdxStart;
  FIdxEnd             := IdxEnd;
  QIdxMax             := FNbItems; // - 1;

  if (FIdxEnd > QIdxMax) then FIdxEnd := QIdxMax;
  FProcProgression := P;
  inherited Create(false); // false -> exécution immédiate
end;

function TThreadMakeCompensationMatrix.AttendPour(): integer;
begin
  //result := WaitFor;
  Result := 0;
  self.Terminate;
end;

//******************************************************************************
constructor TThreadBranchesProcessing.Create(
                     const DocTopo: TToporobotStructure2012;
                     const TableBranches: TTableBranchesXYZ;
                     const NbThreads, NoThread: integer;
                     const NbItems, IdxStart, IdxEnd: integer;
                     const P: TProcOfObjectUsesInteger);
var
  QIdxMax: Integer;
begin
  FDocTopo        := DocTopo;
  FTableBranches  := TableBranches;
  FreeOnTerminate := False;
  FNbThreads      := NbThreads;
  FNoThread       := NoThread;
  FNbItems        := NbItems;
  FIdxStart       := IdxStart;
  FIdxEnd         := IdxEnd;
  QIdxMax         := FNbItems; // -1;

  if (FIdxEnd > QIdxMax) then FIdxEnd := QIdxMax;
  FProcProgression := P;
  inherited Create(false); // false -> exécution immédiate
end;

function TThreadBranchesProcessing.AttendPour(): integer;
begin
  //Result := WaitFor;
  Result := 0;
  self.Terminate;
end;

procedure TThreadBranchesProcessing.Execute;
begin
  Synchronize(nil);
  while (not Terminated) do
  begin
    CalcAccroissementBranches();
  end;
end;

procedure TThreadBranchesProcessing.CalcAccroissementBranches();
var
  Br, Vs: integer;
  DX, DY, DZ, DP: double;
  Branche: TBrancheXYZ;
  Visee: TUneVisee;
begin
  for Br := FIdxStart to FIdxEnd do
  begin
    if (assigned(FProcProgression)) then FProcProgression(FNoThread, FIdxStart, FIdxEnd, Br);

    Branche:= FTableBranches.GetElement(Br);
      DX := 0.0; DY := 0.0; DZ := 0.0; DP := 0.0;
      for Vs := 0 to High(Branche.PointsTopo) do //Branche.PointsTopo.GetNbElements() - 1 do
      begin
        Visee := Branche.PointsTopo[Vs];
        CalculerVisee(Visee,
                      FDocTopo.GetCodeByNumero(Visee.Code),
                      FDocTopo.GetExpeByNumero(Visee.Expe),
                      DX, DY,
                      1.00,
                      DZ, DP);
        Branche.PointsTopo[Vs] := Visee; ////PutBrStation(Br, Vs, Visee);
      end; //for Vs:=0 to Branche.PointsTopo.Count-1 do begin
      Branche.DeltaX := DX;
      Branche.DeltaY := DY;
      Branche.DeltaZ := DZ;
    FTableBranches.PutElement(Br, Branche);
    //*)
  end;
end;
//******************************************************************************

{ TThreadViseesEnAntennesProcessing }

constructor TThreadViseesEnAntennesProcessing.Create(const DocTopo: TToporobotStructure2012;
                                                     const BDDEntites: TBDDEntites;
                                                     const NbThreads, NoThread: integer;
                                                     const NbItems, IdxStart, IdxEnd: integer;
                                                     const P: TProcOfObjectUsesInteger);
var
  QIdxMax: Integer;
begin
  FDocTopo        := DocTopo;
  FBDDEntites     := BDDEntites;
  FreeOnTerminate := False;
  FNbThreads      := NbThreads;
  FNoThread       := NoThread;
  FNbItems        := NbItems;
  FIdxStart       := IdxStart;
  FIdxEnd         := IdxEnd;
  QIdxMax         := FNbItems; // - 1;

  if (FIdxEnd > QIdxMax) then FIdxEnd := QIdxMax;
  FProcProgression := P;
  inherited Create(false); // false -> exécution immédiate

end;

function TThreadViseesEnAntennesProcessing.AttendPour(): integer;
begin
  //Result := WaitFor;
  Result := 0;
  self.Terminate;
end;

procedure TThreadViseesEnAntennesProcessing.Execute;
  function CalcViseeAntenne(const VA: TViseeAntenne; out   OutEE: TBaseStation): boolean;
  var
    VS      : TUneVisee;
    DX, DY, DZ, DP: double;
    EE: TBaseStation;
    MyCode: TCode;
    MyExpe: TExpe;
  begin
    result := false;
    DX := 0.00; DY := 0.00; DZ := 0.00;
    if (FBDDEntites.GetEntiteViseeFromSerSt(VA.SerieDepart, VA.PtDepart, EE)) then
    begin
      DX := EE.PosStation.X;
      DY := EE.PosStation.Y;
      DZ := EE.PosStation.Z;
      // les codes et expés de l'antenne héritent de ceux de la station d'accrochage
      MyCode := FDocTopo.GetCodeByNumero(EE.eCode);
      MyExpe := FDocTopo.GetExpeByNumero(EE.eExpe);
    end
    else
    begin
      exit(false);
    end;
    //*)
    VS.IDSecteur       := 0; // VA.Secteur; // TODO: Secteur à implémenter
    VS.Code            := EE.eCode;
    VS.Expe            := EE.eExpe;
    VS.Longueur        := VA.Longueur;
    VS.Azimut          := VA.Azimut;
    VS.Pente           := VA.Pente;
    VS.LG := 0.00;
    VS.LG := 0.00;
    VS.HZ := 0.00;
    VS.HN := 0.00;
    VS.Commentaires     := VA.Commentaires;
    VS.IDTerrainStation := VA.IDTerrainStation;
    VS.TypeVisee        := tgVISEE_RADIANTE;
    CalculerVisee(VS,
                  MyCode, MyExpe,
                  DX, DY,
                  1.00,
                  DZ, DP);

    DX := EE.PosStation.X + VS.DeltaX;
    DY := EE.PosStation.Y + VS.DeltaY;
    DZ := EE.PosStation.Z + VS.DeltaZ;
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
    OutEE.Enabled           := True;   // drapeau
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
    OutEE.PosStation := MakeTPoint3Df(DX, DY, DZ);
    // habillage
    OutEE.PosOPG := EE.PosStation;
    OutEE.PosOPD := EE.PosStation;
    OutEE.PosPG  := MakeTPoint3Df(DX, DY, DZ);
    OutEE.PosPD  := MakeTPoint3Df(DX, DY, DZ);
    // couleur par défaut
    //OutEE.CouleurDegrade:= FPalette256.GetColorByIndex(MyExpe.IdxCouleur); //clGray ;
    OutEE.CouleurDegrade  := clGray ;

    // commentaires
    OutEE.IDTerrain     := VA.IDTerrainStation;
    OutEE.oCommentaires := VA.Commentaires;
    OutEE.IsPOI         := false;
    // passage ici = OK
    result := true;
  end;
var
  i: Integer;
  QVA: TViseeAntenne;
  QEntite: TBaseStation;
  WU: Boolean;
begin
  Synchronize(nil);
  while (not Terminated) do
  begin
    for i := FIdxStart to FIdxEnd do
    begin
      if (assigned(FProcProgression)) then FProcProgression(FNoThread, FIdxStart, FIdxEnd, i);
      QVA    := FDocTopo.GetViseeAntenne(i);
      WU := CalcViseeAntenne(QVA, QEntite);
      if (WU) then
      begin
        FBDDEntites.AddEntiteAntenne(QEntite);
      end;
    end;
  end;

end;


end.
