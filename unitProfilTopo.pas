unit unitProfilTopo;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  UnitListesSimplesWithGeneriques,
  Classes, SysUtils, Graphics;
// profil de terrain
type

{ TProfilTopo }

 TProfilTopo = class
  private
    FProfilName   : string;
    FLineAttributes: TLineAttributes;
    FExtremite1   : TPoint2Df;
    FExtremite2   : TPoint2Df;
    FListePointsProfil: TListePointsProfil;

    FListeConduitsRecoupes: TListeConduitsRecoupes; // pour création de transects
  public
    function  Initialiser(const P1, P2: TPoint2Df): boolean;
    procedure Finaliser();
    function  GetNbPointsProfilTN(): integer;

    function  GetPointProfilTN(const Idx: integer): TPoint3DfOrderedByP;
    procedure AddPointProfilTN(const P: TPoint3DfOrderedByP);

    property  ProfilName  : string read FProfilName  write FProfilName;
    property  LineAttributes : TLineAttributes read FLineAttributes write FLineAttributes;
    function  GetExtremite1(): TPoint2Df; inline;
    function  GetExtremite2(): TPoint2Df; inline;
    procedure SetExtremite1(const P: TPoint2Df); inline;
    procedure SetExtremite2(const P: TPoint2Df); inline;

    procedure SortProfilTerrainByAbscisses();
    procedure SortConduitsRecoupesBySerieStation();
    procedure SortConduitsRecoupesByAltitudes();
    procedure SortConduitsRecoupesByAbscissesProfil();


    function  GetAltitudeProfil(const P: double): double;
    procedure EpurageProfilTN();

    function  GetNbConduitsRecoupes(): integer;
    procedure AddConduitRecoupe(const P: TBaseStation);
    function  GetConduitRecoupe(const Idx: integer): TBaseStation;


end;

// profil de terrain
type

{ TListeProfilsTopo }

 TListeProfilsTopo = class(TListeSimple<TProfilTopo>)
  private
  public
    procedure RemoveProfil(const Idx: integer);
    procedure ClearListeProfils();
end;

implementation

{ TProfilTopo }
function SortPointsTNByAbscisses(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TPoint3DfOrderedByP;
begin
  E1:=Item1;
  E2:=Item2;
  if (E1.P < E2.P) then
    Result := -1
  else if (E1.P = E2.P) then
    Result :=  0
  else
    Result :=  1;
end;
function SortConduitsRecoupesBySrSt(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TBaseStation;
  SP1, SP2: Int64;
  function MiouMiou(const Miou: TBaseStation): Int64; inline;
  begin
    Result := (Miou.Entite_Station + 10000 * Miou.Entite_Serie);
  end;
begin
  E1:=Item1;
  E2:=Item2;
  SP1 := MiouMiou(E1^);
  SP2 := MiouMiou(E2^);
  if (SP1 < SP2) then
    Result := -1
  else if (SP1 = SP2) then
    Result :=  0
  else
    Result :=  1;
end;
function SortConduitsRecoupesByCoteZ(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TBaseStation;
  SP1, SP2: double;
  function MiouMiou(const Miou: TBaseStation): double; inline;
  begin
    Result := Miou.PosStation.Z;
  end;
begin
  E1 := Item1;
  E2 := Item2;
  SP1 := MiouMiou(E1^);
  SP2 := MiouMiou(E2^);
  if (SP1 < SP2) then
    Result := -1
  else if (SP1 = SP2) then
    Result :=  0
  else
    Result :=  1;
end;
function SortConduitsRecoupesByAbscisses(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TBaseStation;
  SP1, SP2: double;
  function MiouMiou(const Miou: TBaseStation): double; inline;
  begin
    Result := Miou.TagDouble;
  end;
begin
  E1 := Item1;
  E2 := Item2;
  SP1 := MiouMiou(E1^);
  SP2 := MiouMiou(E2^);
  if (SP1 < SP2) then
    Result := -1
  else if (SP1 = SP2) then
    Result :=  0
  else
    Result :=  1;
end;

procedure TListeProfilsTopo.RemoveProfil(const Idx: integer);
var
  P: TProfilTopo;
begin
  P := self.GetElement(Idx);
  try
    P.Finaliser();
  finally
    self.RemoveElement(Idx);
  end;
end;

// liste des profils topo
procedure TListeProfilsTopo.ClearListeProfils();
var
  i, Nb: Integer;
  QProfilTopo: TProfilTopo;
begin
  Nb := self.GetNbElements();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      QProfilTopo := self.GetElement(i);
      try      // nettoyage des enfants de QProfilTopo;
        QProfilTopo.Finaliser();
      finally  // et libération
        FreeAndNil(QProfilTopo);//QProfilTopo.Free;
      end;
    end;
  end;
  self.ClearListe();
end;

(* TProfilTopo *)
function TProfilTopo.Initialiser(const P1, P2: TPoint2Df): boolean;
begin
  result := false;
  FListePointsProfil      := TListePointsProfil.Create;
  FListeConduitsRecoupes  := TListeConduitsRecoupes.Create;
  try
    FListePointsProfil.ClearListe();
    FListeConduitsRecoupes.ClearListe();
    SetExtremite1(P1);
    SetExtremite2(P2);
    result := True;
  except
  end;
end;

procedure TProfilTopo.Finaliser();
begin
  try
    FListePointsProfil.ClearListe();
    FListeConduitsRecoupes.ClearListe();
  finally
    FreeAndNil(FListePointsProfil);
    FreeAndNil(FListeConduitsRecoupes);
  end;
end;

function TProfilTopo.GetNbPointsProfilTN(): integer;
begin
  Result := FListePointsProfil.GetNbElements();
end;

function TProfilTopo.GetNbConduitsRecoupes(): integer;
begin
  Result := FListeConduitsRecoupes.GetNbElements();
end;

function TProfilTopo.GetPointProfilTN(const Idx: integer): TPoint3DfOrderedByP;
begin
  Result := FListePointsProfil.GetElement(Idx);
end;
procedure TProfilTopo.AddPointProfilTN(const P: TPoint3DfOrderedByP);
begin
  FListePointsProfil.AddElement(P);
end;

procedure TProfilTopo.SortProfilTerrainByAbscisses();
begin
  FListePointsProfil.Sort(SortPointsTNByAbscisses);
end;
procedure TProfilTopo.SortConduitsRecoupesBySerieStation();
begin
  FListeConduitsRecoupes.Sort(SortConduitsRecoupesBySrSt);
end;
procedure TProfilTopo.SortConduitsRecoupesByAltitudes();
begin
  FListeConduitsRecoupes.Sort(SortConduitsRecoupesByCoteZ);
end;

procedure TProfilTopo.SortConduitsRecoupesByAbscissesProfil();
begin
  FListeConduitsRecoupes.Sort(SortConduitsRecoupesByAbscisses);
end;

procedure TProfilTopo.EpurageProfilTN();
var
  P0, P1: TPoint3DfOrderedByP;
  Nb, i: Integer;
  d: Double;
begin
  Nb := FListePointsProfil.GetNbElements();
  if (Nb = 0) then Exit;
  for i := FListePointsProfil.GetNbElements() - 1 downto 1 do
  begin
    P1 := FListePointsProfil.GetElement(i);
    P0 := FListePointsProfil.GetElement(i-1);
    d := P1.P - P0.P;
    if (d < 0.01) then FListePointsProfil.RemoveElement(i);
  end;
end;





procedure TProfilTopo.AddConduitRecoupe(const P: TBaseStation);
var
  n: Integer;
begin
  FListeConduitsRecoupes.AddElement(P);
end;
function TProfilTopo.GetConduitRecoupe(const Idx: integer): TBaseStation;
begin
  Result := FListeConduitsRecoupes.GetElement(Idx);
end;



function  TProfilTopo.GetExtremite1(): TPoint2Df; inline;
begin
  Result := FExtremite1;
end;

function  TProfilTopo.GetExtremite2(): TPoint2Df; inline;
begin
  Result := FExtremite2;
end;

procedure TProfilTopo.SetExtremite1(const P: TPoint2Df); inline;
begin
  FExtremite1 := P;
end;

procedure TProfilTopo.SetExtremite2(const P: TPoint2Df); inline;
begin
  FExtremite2 := P;
end;


function  TProfilTopo.GetAltitudeProfil(const P: double): double;
var
  Nb, i: Integer;
  P0, P1: TPoint3DfOrderedByP;
  dP, dZ: Double;
begin
  Result := 0.00;
  Nb := FListePointsProfil.GetNbElements();
  if (Nb < 2) then exit(0.00);
  for i := 0 to Nb-2 do
  begin
    P0 := FListePointsProfil.GetElement(i);
    P1 := FListePointsProfil.GetElement(i+1);
    if (IsInRange(P, P0.P, P1.P)) then
    begin
      dP := P1.P - P0.P;
      dZ := P1.Z - P0.Z;
      Result := P0.Z + (P - P0.P) * dZ/dP;
      Exit;
    end;
  end;
end;





end.

