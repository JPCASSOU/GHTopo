unit unitListeMesuresTopoDistoX;
{$mode delphiunicode}

interface

uses
  StructuresDonnees,
  UnitListesSimplesWithGeneriques,
  Common,
  Classes, SysUtils;

const NB_MESURES_ACQUISES_POUR_UNE_VISEE = 3;

// pour le DistoX
type

{ TListeMesuresTopoDistoX }

 TListeMesuresTopoDistoX = class
  private
    FListeTampon : TTamponMesuresViseesDistoX;
    FListeVisees : TListeMesuresViseesDistoX;

    FNbMesuresPourUneVisee: integer;
    FDistoXSerialNumber   : TDistoXSerialNumber;
    FUniteAzimuts: double;
    FUnitePentes : double;
    FToleranceL  : double;
    FToleranceAz : double;
    FToleranceP  : double;

    //FListe       : TListeMesuresTopoDistoX;


  public
    function  Initialiser(const D: TDistoXSerialNumber; const GradAz, GradInc: double; const QTolLongueurs, QTolAzimuts, QTolPentes: double): boolean;
    procedure ClearAllListes();
    procedure ClearTampon();
    procedure ClearVisees();
    procedure Finaliser();
    procedure SetDistoXSerialNumber(const D: TDistoXSerialNumber);
    procedure SetUnitesAngulaires(const GradAz, GradInc: double);
    procedure SetTolerances(const QTolLongueurs, QTolAzimuts, QTolPentes: double);
    function CalcAzimutMoyen(out ViseeMoyenne: TMesureViseeDistoX): boolean;
    function GetNbMesuresPourUneVisee(): integer;
    // le tampon de mesures
    procedure AddMesureToBuffer(const M: TMesureBruteDistoX);
    function  GetMesureFromBuffer(const Idx: integer): TMesureBruteDistoX;
    procedure PutMesureToBuffer(const Idx: integer; const M: TMesureBruteDistoX);
    function  getNbMesuresOfBuffer(): integer;

    // les visées
    procedure AddVisee(const V: TMesureViseeDistoX);
    function  GetVisee(const Idx: integer): TMesureViseeDistoX;
    procedure RemoveVisee(const Idx: integer);
    function  GetNbVisees(): integer;
    function  PopVisee(): TMesureViseeDistoX;




end;


implementation
uses
  DGCDummyUnit;

{ TListeMesuresTopoDistoX }

function TListeMesuresTopoDistoX.Initialiser(const D: TDistoXSerialNumber; const GradAz, GradInc: double; const QTolLongueurs, QTolAzimuts, QTolPentes: double): boolean;
begin
  result := false;
  FNbMesuresPourUneVisee := NB_MESURES_ACQUISES_POUR_UNE_VISEE;
  SetDistoXSerialNumber(D);
  SetUnitesAngulaires(GradAz, GradInc);
  SetTolerances(QTolLongueurs, QTolAzimuts, QTolPentes);
  FListeTampon := TTamponMesuresViseesDistoX.Create;
  FListeVisees := TListeMesuresViseesDistoX.Create;

  try
    self.ClearAllListes();
    result := True;
  except

  end;
end;

procedure TListeMesuresTopoDistoX.ClearAllListes();
begin
  ClearTampon();
  ClearVisees();
end;
// Le tampon
procedure TListeMesuresTopoDistoX.ClearTampon();
begin
  FListeTampon.ClearListe();
end;

procedure TListeMesuresTopoDistoX.ClearVisees();
begin
  FListeVisees.ClearListe();
end;


procedure TListeMesuresTopoDistoX.AddMesureToBuffer(const M: TMesureBruteDistoX);
begin
  FListeTampon.AddElement(M);
  if (getNbMesuresOfBuffer() > FNbMesuresPourUneVisee) then FListeTampon.RemoveElement(0);
end;

function TListeMesuresTopoDistoX.GetMesureFromBuffer(const Idx: integer): TMesureBruteDistoX;
begin
  Result := FListeTampon.GetElement(Idx);
end;

procedure TListeMesuresTopoDistoX.PutMesureToBuffer(const Idx: integer; const M: TMesureBruteDistoX);
begin
  FListeTampon.PutElement(Idx, M);
end;
function TListeMesuresTopoDistoX.getNbMesuresOfBuffer(): integer;
begin
  result := FListeTampon.GetNbElements();
end;

procedure TListeMesuresTopoDistoX.AddVisee(const V: TMesureViseeDistoX);
begin
  FListeVisees.AddElement(V);
end;

function TListeMesuresTopoDistoX.GetVisee(const Idx: integer): TMesureViseeDistoX;
begin
  result := FListeVisees.GetElement(Idx);
end;

procedure TListeMesuresTopoDistoX.RemoveVisee(const Idx: integer);
begin
  FListeVisees.RemoveElement(Idx);
end;


// Les visées
function TListeMesuresTopoDistoX.GetNbVisees(): integer;
begin
  Result := FListeVisees.GetNbElements();
end;

function TListeMesuresTopoDistoX.PopVisee(): TMesureViseeDistoX;
begin
  Result.setFrom(Now(), FDistoXSerialNumber, -1.00, 0.0, 0.0, tvdRADIANTE);
end;


//*****************************************

procedure TListeMesuresTopoDistoX.Finaliser();
begin
  try
    ClearAllListes();
  finally
    FreeAndNil(FListeTampon);
    FreeAndNil(FListeVisees);
  end;
end;

procedure TListeMesuresTopoDistoX.SetDistoXSerialNumber(const D: TDistoXSerialNumber);
begin
  FDistoXSerialNumber := D;
end;

procedure TListeMesuresTopoDistoX.SetUnitesAngulaires(const GradAz, GradInc: double);
begin
  FUniteAzimuts := GradAz;
  FUnitePentes  := GradInc;
end;

procedure TListeMesuresTopoDistoX.SetTolerances(const QTolLongueurs, QTolAzimuts, QTolPentes: double);
begin
  FToleranceL  := QTolLongueurs;
  FToleranceAz := QTolAzimuts;
  FToleranceP  := QTolPentes;
end;

function TListeMesuresTopoDistoX.CalcAzimutMoyen(out ViseeMoyenne: TMesureViseeDistoX): boolean;
var
  i, n, IdxFrom, IdxTo: Integer;
  TwoPi_Sur_GradAz, TwoPi_Sur_GradInc : Double;
  Grad_Az_Sur_4   , Grad_Inc_Sur_4    : double;
  QR: double;
  QVecteur, QProjViseeMoyenne: TPoint3Df;
  MyMesureDistoX: TMesureViseeDistoX;
  ToleranceAngulaireMiseEnDistance, QDL: ValReal;
  QConditionLongueursOK, QConditionAngulaireOK: Boolean;
  WU1, WU2: String;
  QViseeBrute: TMesureBruteDistoX;
  function CalcDXDYDZ(const QL, QAz, QInc: double): TPoint3Df;
  var
    QLP: double;
  begin
    QLP      := QL  * cos(QInc);
    Result.X := QLP * Sin(QAz);
    Result.Y := QLP * Cos(QAz);
    Result.Z := QL  * sin(QInc);
  end;

begin
  Result := false;
  ViseeMoyenne.setFrom(Now(), FDistoXSerialNumber, -1.00, 0.00, 0.00, tvdRADIANTE);
  TwoPi_Sur_GradAz  := TWO_PI / FUniteAzimuts;
  TwoPi_Sur_GradInc := TWO_PI / FUnitePentes;
  Grad_Az_Sur_4     := 0.25 * FUniteAzimuts;
  Grad_Inc_Sur_4    := 0.25 * FUnitePentes;
  n := getNbMesuresOfBuffer();
  AfficherMessageErreur(Format('%s.CalcAzimutMoyenOfTMesuresViseeDistoX(%.2f, %.2f, %.2f, %.5f, %.5f - n = %d)',
                              [classname, FUniteAzimuts, FUnitePentes, FToleranceL, FToleranceAz, FToleranceP, n]));
  if (n = 0) then exit(false); // Tableau de mesures vide -> on sort
  if (n < FNbMesuresPourUneVisee) then // Moins de 3 mesures = Ce sont des visées radiantes
  begin
    AfficherMessageErreur('Fuck the Christ');
    QViseeBrute := FListeTampon.GetLastElement();
    ViseeMoyenne.setFrom(Now, FDistoXSerialNumber, QViseeBrute.Longueur, QViseeBrute.Azimut, QViseeBrute.Pente);

    AfficherMessageErreur('n = (1..2) -> Visées radiante: ' + ViseeMoyenne.DebugString());
    Exit(false);
  end;
  // Calcul de la visée moyenne
  QVecteur.Empty();
  for i := 0 to n - 1  do
  begin
    QViseeBrute := FListeTampon.GetElement(i);
    QViseeBrute.DeltaXYZ := CalcDXDYDZ(QViseeBrute.Longueur, QViseeBrute.Azimut * TwoPi_Sur_GradAz, QViseeBrute.Pente * TwoPi_Sur_GradInc);
    PutMesureToBuffer(i, QViseeBrute);
    QViseeBrute := GetMesureFromBuffer(i);
    QVecteur.X += QViseeBrute.DeltaXYZ.X;
    QVecteur.Y += QViseeBrute.DeltaXYZ.Y;
    QVecteur.Z += QViseeBrute.DeltaXYZ.Z;
  end;
  // Longueur, azimut et pentes moyens
  QVecteur.X := QVecteur.X / n;
  QVecteur.Y := QVecteur.Y / n;
  QVecteur.Z := QVecteur.Z / n;
  GetBearingInc(FUniteAzimuts, FUnitePentes, QVecteur, ViseeMoyenne.Longueur, ViseeMoyenne.Azimut, ViseeMoyenne.Pente);
  // On projette la visée moyenne dans un repère local
  QProjViseeMoyenne := CalcDXDYDZ(ViseeMoyenne.Longueur, ViseeMoyenne.Azimut * TwoPi_Sur_GradAz, ViseeMoyenne.Pente * TwoPi_Sur_GradInc);
  AfficherMessageErreur(Format('Visée moyenne: %.2f; %.2f; %.2f - %.3f; %.3f; %.3f',
                             [ViseeMoyenne.Longueur, ViseeMoyenne.Azimut, ViseeMoyenne.Pente,
                              QProjViseeMoyenne.X,  QProjViseeMoyenne.Y,  QProjViseeMoyenne.Z]));
  // Tolérance sur longueurs
  for i := 0 to n-1 do
  begin
    QViseeBrute := FListeTampon.GetElement(i);
    QDL := abs(QViseeBrute.Longueur - ViseeMoyenne.Longueur);
    QConditionLongueursOK := (QDL < FToleranceL);
  end;
  // Tolérance sur angles
  // On transforme la tolérance angulaire en rayon de capture
  ToleranceAngulaireMiseEnDistance := ViseeMoyenne.Longueur * sin(FToleranceAz * DEG_TO_RAD);
  // Parcours de la liste des projetées
  for i := 0 to n-1 do
  begin
    QViseeBrute := FListeTampon.GetElement(i);
    // La nouvelle origine est l'extrémité de la visée moyenne
    QVecteur.setFrom(QViseeBrute.DeltaXYZ.X - QProjViseeMoyenne.X,
                     QViseeBrute.DeltaXYZ.Y - QProjViseeMoyenne.Y,
                     QViseeBrute.DeltaXYZ.Z - QProjViseeMoyenne.Z);
    QR  := QVecteur.getNorme();

    // Test de proximité des extrémités des visées avec celle de la visée moyenne
    QConditionAngulaireOK := (QR < ToleranceAngulaireMiseEnDistance);

    WU1 := IIF(QConditionLongueursOK, 'OK', 'NotOK');
    WU2 := IIF(QConditionAngulaireOK, 'OK', 'NotOK');
    AfficherMessageErreur(Format('%d: %.2f; %.2f; %.2f - dl = %.2f, r = %.3f, r0 = %.3f - Longueurs: %s, angles: %s', [i, QVecteur.X, QVecteur.Y, QVecteur.Z, QDL ,QR, ToleranceAngulaireMiseEnDistance, WU1, WU2]));
    Result := (QConditionLongueursOK and QConditionAngulaireOK);
    if (Result) then ViseeMoyenne.setTypeMesure(tvdCHEMINEMENT);
  end;
end;

function TListeMesuresTopoDistoX.GetNbMesuresPourUneVisee(): integer;
begin
  result := FNbMesuresPourUneVisee;
end;


end.
