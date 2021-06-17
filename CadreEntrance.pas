unit CadreEntrance;
// Date: 16/05/2013
// Statut: OK
// 04/10/2013: Conversions UTF8 <> ANSI fixed
// 19/04/2018: Simplification et corrections
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  StructuresDonnees,
  CallDialogsStdVersion,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitClasseMaillage,
  Classes, SysUtils, FileUtil, Forms, Dialogs, Controls, StdCtrls, curredit;

type TCdrEntreeCavites = class(TFrame)
    btnCallCalculette: TButton;
    btnGetCoordsFromIDTerrain: TButton;
    btnGetCoordsFromSerSt: TButton;
    btnCouleurEntrance: TColorButton;
    btnExtractFromMNT: TButton;
    editIDTerrain: TEdit;
    editCommentaires: TEdit;
    editNomEntree: TEdit;
    editPoint: TCurrencyEdit;
    editSerie: TCurrencyEdit;
    editXEntree: TCurrencyEdit;
    editYEntree: TCurrencyEdit;
    editZEntree: TCurrencyEdit;
    Label1: TLabel;
    lbIDTerrainEntree: TLabel;
    lbStationOfEntrance: TLabel;
    lbEntranceCoordinates: TLabel;
    lbEntranceName: TLabel;
    lbComments: TLabel;
    lbNumEntrance: TStaticText;
	lbCurrentEPSG: TStaticText;
    procedure btnExtractFromMNTClick(Sender: TObject);
    procedure btnGetCoordsFromIDTerrainClick(Sender: TObject);
    procedure btnCallCalculetteClick(Sender: TObject);
    procedure btnGetCoordsFromSerStClick(Sender: TObject);
  private
    FIdx     : integer;
    FDocTopo :  TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FMaillage : TMaillage;
    procedure InitCaptions();
    procedure PutEntranceInForm(const E: TEntrance; const DoInitCaptions: boolean);
    { private declarations }
  public
    { public declarations }
    function Initialiser(const FD: TToporobotStructure2012; const FB: TBDDEntites; const FM: TMaillage; const QEntrance: TEntrance; const QIdxEntrance: integer): boolean;
    function GetEntrance(): TEntrance;
  end;

implementation
uses DGCDummyUnit;

{$R *.lfm}


procedure TCdrEntreeCavites.InitCaptions();
begin
  lbEntranceName.Caption               := GetResourceString(rsCDR_ENTR_ENTRNAME);
  lbIDTerrainEntree.Caption            := GetResourceString(rsCDR_ENTR_ENTR_ID);
  lbEntranceCoordinates.Caption        := GetResourceString(rsCDR_ENTR_COORDINATES);
  lbStationOfEntrance.Caption          := GetResourceString(rsCDR_ENTR_STATOFENTR);
  btnCallCalculette.Caption            := GetResourceString(rsCDR_ENTR_CALLCALCULETTE);
  lbComments.Caption                   := GetResourceString(rsSELECT_LISTE_OBSERV);
  btnGetCoordsFromIDTerrain.Caption    := GetResourceString(rsCDR_ENTR_BTN_GET_COORD_FROM_IDTERRAIN);
  btnGetCoordsFromSerSt.Caption        := GetResourceString(rsCDR_ENTR_BTN_GET_COORD_FROM_SER_ST);
  btnExtractFromMNT.caption            := GetResourceString(rsCDR_ENTR_ALTITUDE_FROM_MNT);
end;


procedure TCdrEntreeCavites.btnCallCalculetteClick(Sender: TObject);
var
  QCoords: TPoint2Df;
  QResultatCalcul, QDeclimag: double;
  EWE: TLabelSystemesCoordsEPSG;
begin
  QResultatCalcul := 0.00;
  QDeclimag       := 0.00;
  QCoords.Empty();

  (*
  if (CallCalculette(FDocTopo, FBDDEntites, nil,'', 1, QCoords, QResultatCalcul, QDeclimag)) then
  begin
    editXEntree.Value := QCoords.X;
    editYEntree.Value := QCoords.Y;
  end;
  //*)
  QCoords.X := editXEntree.Value;
  QCoords.Y := editYEntree.Value;
  EWE := FDocTopo.GetCodeEPSGSystemeCoordonnees();
  if (DisplayMiniConvertisseur(27573, EWE.CodeEPSG, QCoords.X, QCoords.Y)) then
  begin
    editXEntree.Value := QCoords.X;
    editYEntree.Value := QCoords.Y;
  end;
end;

procedure TCdrEntreeCavites.btnGetCoordsFromSerStClick(Sender: TObject);
var
  EWE: TBaseStation;
begin
  if (FBDDEntites.GetEntiteViseeFromSerSt(editSerie.AsInteger, editPoint.AsInteger, EWE)) then
  begin
    editXEntree.Value  := EWE.PosStation.X;
    editYEntree.Value  := EWE.PosStation.Y;
    editZEntree.Value  := EWE.PosStation.Z;
    editIDTerrain.Text := EWE.IDTerrain;
    editSerie.AsInteger:= EWE.Entite_Serie;
    editPoint.AsInteger:= EWE.Entite_Station;
  end
  else ShowMessage(GetResourceString(rsMATCHNOTFOUND));
end;



procedure TCdrEntreeCavites.btnGetCoordsFromIDTerrainClick(Sender: TObject);
var
  EWE: TBaseStation;
begin
  if (FBDDEntites.FindStationByCle(true, trim(editIDTerrain.Text), EWE)) then
  begin
    editXEntree.Value  := EWE.PosStation.X;
    editYEntree.Value  := EWE.PosStation.Y;
    editZEntree.Value  := EWE.PosStation.Z;
    editIDTerrain.Text := EWE.IDTerrain;
    editSerie.AsInteger:= EWE.Entite_Serie;
    editPoint.AsInteger:= EWE.Entite_Station;
  end
  else
  begin
    ShowMessage(GetResourceString(rsMATCHNOTFOUND));
  end;
end;

procedure TCdrEntreeCavites.btnExtractFromMNTClick(Sender: TObject);
var
  EWE: double;
begin
  if (FMaillage.CalcAltitudeMaillageAtPoint(editXEntree.Value, editYEntree.value, EWE)) then editZEntree.Value := EWE;
end;





procedure TCdrEntreeCavites.PutEntranceInForm(const E: TEntrance; const DoInitCaptions: boolean);
var
  EWE: TLabelSystemesCoordsEPSG;
begin;
  if (DoInitCaptions) then InitCaptions;

  lbNumEntrance.Caption := Format(FORMAT_NB_INTEGER, [FIdx]);
  editNomEntree.Text := _AnsiToLCLStr(E.eNomEntree);
  editIDTerrain.Text := _AnsiToLCLStr(E.eIDTerrain);
  btnCouleurEntrance.ButtonColor := E.eCouleur;
  editXEntree.Value  := E.ePosition.X;
  editYEntree.Value  := E.ePosition.Y;
  editZEntree.Value  := E.ePosition.Z;
  editSerie.AsInteger:= E.eRefSer;
  editPoint.AsInteger:= E.eRefSt;
  editCommentaires.Text:=_AnsiToLCLStr(E.eObserv);
  EWE := FDocTopo.GetCodeEPSGSystemeCoordonnees();
  lbCurrentEPSG.Caption := Format('EPSG:%d (%s)', [EWE.CodeEPSG, EWE.NomEPSG]);
end;

function TCdrEntreeCavites.Initialiser(const FD: TToporobotStructure2012; const FB: TBDDEntites; const FM: TMaillage; const QEntrance: TEntrance; const QIdxEntrance: integer): boolean;

begin
  btnGetCoordsFromIDTerrain.Visible := true;
  btnGetCoordsFromSerSt.Visible     := true;
  result := False;
  FDocTopo    := FD;
  FBDDEntites := FB;
  FMaillage   := FM;
  PutEntranceInForm(QEntrance, True);
  btnGetCoordsFromIDTerrain.Enabled := Assigned(FBDDEntites);
  btnGetCoordsFromSerSt.Enabled     := Assigned(FBDDEntites);
  btnExtractFromMNT.Enabled         := false;
  if (Assigned(FMaillage)) then
  begin
    if (FMaillage.IsValidMaillage()) then btnExtractFromMNT.Enabled := True;
  end;
  result := True;
end;

function TCdrEntreeCavites.GetEntrance(): TEntrance;
begin
  Result.eNomEntree   := _LCLStrToAnsi(Trim(editNomEntree.Text));
  Result.eIDTerrain   := _LCLStrToAnsi(Trim(editIDTerrain.Text));
  Result.eCouleur     := btnCouleurEntrance.ButtonColor;
  Result.eRefSer      := editSerie.AsInteger;
  Result.eRefSt       := editPoint.AsInteger;
  Result.ePosition.setFrom(editXEntree.Value, editYEntree.Value, editZEntree.Value);
  Result.eObserv      := _LCLStrToAnsi(trim(editCommentaires.Text));
end;

end.

