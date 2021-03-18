unit CadreListeNearStations;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  ToporobotClasses2012,
  StructuresDonnees,
  UnitEntitesExtended,
  Common,
  unitUtilsComposants,
  math,
  Classes, Graphics, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, Types, LCLType;

type

  { TCdrListeNearestStations }

  TCdrListeNearestStations = class(TFrame)
    hcColsTitres: THeaderControl;
    lbNbStationsFound: TLabel;
    lsbStations: TListBox;
    lbStationDeBase: TStaticText;
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbStationsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    FDocTopo          : TToporobotStructure2012;
    FBDDEntites       : TBDDEntites;
    FTempBDDEntites   : TBDDEntites;
    FCurrBaseStation  : TBaseStation;
  public
    function Initialiser(const TD: TToporobotStructure2012;
                         const BDD: TBDDEntites;
                         const QE: TBaseStation;
                         const QDoMatchExact: boolean;
                         const QRayonCapture: double): boolean;
    procedure SetCurrBaseStation(const BS: TBaseStation);

    procedure Relister(const QDoMatchExact: boolean;
                       const QRayonCapture: double);

    function  GetNbStationsFound(): integer;
    function  GetNearEntiteSelected(): TBaseStation;
    procedure Finaliser();

  end;

implementation

{$R *.lfm}

{ TCdrListeNearestStations }
function TCdrListeNearestStations.Initialiser(const TD: TToporobotStructure2012;
                                              const BDD: TBDDEntites;
                                              const QE: TBaseStation;
                                              const QDoMatchExact: boolean;
                                              const QRayonCapture: double): boolean;
begin
  AfficherMessage(Format('%s.Initialiser', [ClassName]));
  Result := False;
  FDocTopo          := TD;
  FBDDEntites       := BDD;
  SetCurrBaseStation(QE);
  FTempBDDEntites := TBDDEntites.Create;
  try
    FTempBDDEntites.Initialiser(MakeTPoint3Df(0.0, 0.0, 0.0), clAqua, clMaroon);
    FTempBDDEntites.ResetTables();
    Relister(QDoMatchExact, QRayonCapture);
    result := True;
  except
  end;

end;

procedure TCdrListeNearestStations.SetCurrBaseStation(const BS: TBaseStation);
begin
  FCurrBaseStation := BS;
  lbStationDeBase.Caption := Format('Voisinage de la station %d.%d (%s)', [FCurrBaseStation.Entite_Serie, FCurrBaseStation.Entite_Station, FCurrBaseStation.IDTerrain]);
end;

procedure TCdrListeNearestStations.Finaliser();
begin
  try
    FTempBDDEntites.Finaliser();
  finally
    FreeAndNil(FTempBDDEntites);//FTempBDDEntites.Free;
  end;
end;

procedure TCdrListeNearestStations.Relister(const QDoMatchExact: boolean;
                                            const QRayonCapture: double);
var
  i, Nb: Integer;
  dist, dx, dy, dz: double;
  EWE: TBaseStation;
begin
  lsbStations.Clear;
  FTempBDDEntites.ViderLesViseesEtAntennes();
  Nb := FBDDEntites.GetNbEntitesVisees();
  for i := 0 to Nb - 1 do
  begin
    EWE := FBDDEntites.GetEntiteVisee(i);
    if (EWE.Type_Entite = tgVISEE_RADIANTE) then Continue;  // les visées en antennes ne sont pas prises en compte
    // la station de base elle-même est bien sûr zappée
    if ((EWE.Entite_Serie   = FCurrBaseStation.Entite_Serie) and
        (EWE.Entite_Station = FCurrBaseStation.Entite_Station)) then Continue;
    dx := EWE.PosStation.X - FCurrBaseStation.PosStation.X;
    dy := EWE.PosStation.Y - FCurrBaseStation.PosStation.Y;
    dz := EWE.PosStation.Z - FCurrBaseStation.PosStation.Z;
    dist := Hypot3D(dx, dy, dz);
    if (IsZero(dist, 0.001)) then Continue; // Distance nulle = c'est une série qui boucle en ce point
    // on taggue l'entité avec la distance
    EWE.TagDouble := dist;
    if (EWE.TagDouble < QRayonCapture) then FTempBDDEntites.AddEntiteVisee(EWE);
  end;
  FTempBDDEntites.SortByTagDouble();    // trier du plus proche au plus éloigner
  // et on liste
  lsbStations.Clear;
  Nb := FTempBDDEntites.GetNbEntitesVisees();
  if (Nb > 0) then
  begin
    for i := 0 to Nb -1 do lsbStations.Items.Add('');
    lsbStations.ItemIndex:=0;
  end;
  lbNbStationsFound.Caption := format('%d stations', [lsbStations.Count]);
end;

function TCdrListeNearestStations.GetNbStationsFound(): integer;
begin
  Result := FTempBDDEntites.GetNbEntitesVisees();
end;

function TCdrListeNearestStations.GetNearEntiteSelected(): TBaseStation;
var
  n: Integer;
begin
  Result := FBDDEntites.GetEntiteVisee(0);
  Result.Entite_Serie   := -1;
  Result.Entite_Station := -1;
  n := FTempBDDEntites.GetNbEntitesVisees();
  if (n = 0) then Exit;
  Result := FTempBDDEntites.GetEntiteVisee(lsbStations.ItemIndex);
end;

procedure TCdrListeNearestStations.hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbStations.Invalidate;
end;

procedure TCdrListeNearestStations.lsbStationsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  EWE: TBaseStation;
  C: TColor;
  m: integer;
  dp, dx, dy, dz: double;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    QIdxNameSpace, QNoSerie: integer;
    MyNamespace: TNameSpace;
  begin
    DecomposeNumeroSerie(EWE.Entite_Serie, QIdxNameSpace, QNoSerie);
    MyNamespace := FDocTopo.GetNameSpace(QIdxNameSpace);
    ResetColorRow(lsbStations, ARect, bg, tc);
    //DrawColTexte(lsbStations, ARect, hcColsTitres.Sections.Items[0], false, MyNamespace.Nom);
    DrawColTexte(lsbStations, ARect, hcColsTitres.Sections.Items[0], true, Format(FMTSERST,[QNoSerie, EWE.Entite_Station]));
    DrawColTexte(lsbStations, ARect, hcColsTitres.Sections.Items[1], true, EWE.IDTerrain);
    DrawColTexte(lsbStations, ARect, hcColsTitres.Sections.Items[2], true, Format(FORMAT_NB_REAL_3_DEC,[EWE.TagDouble]));
    DrawColTexte(lsbStations, ARect, hcColsTitres.Sections.Items[3], true, Format(FORMAT_NB_REAL_3_DEC,[dp]));
    DrawColTexte(lsbStations, ARect, hcColsTitres.Sections.Items[4], true, Format(FORMAT_NB_REAL_3_DEC,[dx]));
    DrawColTexte(lsbStations, ARect, hcColsTitres.Sections.Items[5], true, Format(FORMAT_NB_REAL_3_DEC,[dy]));
    DrawColTexte(lsbStations, ARect, hcColsTitres.Sections.Items[6], true, Format(FORMAT_NB_REAL_3_DEC,[dz]));
  end;
begin
  EWE := FTempBDDEntites.GetEntiteVisee(Index);
  dx  := FCurrBaseStation.PosStation.X - EWE.PosStation.X;
  dy  := FCurrBaseStation.PosStation.Y - EWE.PosStation.Y;
  dz  := FCurrBaseStation.PosStation.Z - EWE.PosStation.Z;
  dp  := Hypot2D(dx, dy);
  if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clwhite, clBlack);
end;
end.

