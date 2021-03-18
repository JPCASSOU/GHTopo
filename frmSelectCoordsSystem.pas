unit frmSelectCoordsSystem;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  unitUtilsComposants,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Buttons,
  ConvertisseurJPC, StructuresDonnees, Types, LCLType;

type

  { TdlgSelectionCoordsSystem }

  TdlgSelectionCoordsSystem = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    hcColsTitres: THeaderControl;
    lsbCoordsSystems: TListBox;
    lbCurrentGCS: TStaticText;
    procedure lsbCoordsSystemsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    FConversionSysteme: TConversionSysteme;
  public
    function Initialiser(const FC: TConversionSysteme; const SC: TLabelSystemesCoordsEPSG): boolean;
    function GetSelectedEPSGSysCoords(): TLabelSystemesCoordsEPSG;
  end;

var
  dlgSelectionCoordsSystem: TdlgSelectionCoordsSystem;

implementation
uses
   DGCDummyUnit;

{$R *.lfm}

{ TdlgSelectionCoordsSystem }

procedure TdlgSelectionCoordsSystem.lsbCoordsSystemsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  CC: TLabelSystemesCoordsEPSG;
  procedure DessineItem(const bg,tc: TColor);
  begin
    ResetColorRow(lsbCoordsSystems, ARect, bg, tc);
    DrawColTexte(lsbCoordsSystems, ARect, hcColsTitres.Sections.Items[0], false, Format(FORMAT_NB_INTEGER, [CC.CodeEPSG]));
    DrawColTexte(lsbCoordsSystems, ARect, hcColsTitres.Sections.Items[1], True , CC.NomEPSG);
  end;
begin
  CC := FConversionSysteme.GetCodeEPSGNomSysteme(Index);
  if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clwhite, clBlack);
end;

function TdlgSelectionCoordsSystem.Initialiser(const FC: TConversionSysteme; const SC: TLabelSystemesCoordsEPSG): boolean;
var
  Nb, i: Integer;
  EWE: TLabelSystemesCoordsEPSG;
begin
  result := false;
  try
    self.Caption := GetResourceString(rsREDEF_COORD_SYSTEME);

    lbCurrentGCS.Caption := Format('EPSG:%d - %s', [SC.CodeEPSG, SC.NomEPSG]);
    FConversionSysteme := FC;
    Nb := FConversionSysteme.GetNbSystemes();
    lsbCoordsSystems.Clear;
    for i := 0 to Nb - 1 do
    begin
      EWE := FConversionSysteme.GetCodeEPSGNomSysteme(i);
      lsbCoordsSystems.Items.Add(format('%d - %s', [EWE.CodeEPSG, EWE.NomEPSG]));
    end;
    lsbCoordsSystems.ItemIndex := FConversionSysteme.GetIndexSystemeByCodeEPSG(SC.CodeEPSG);
    result := true;
  except
    pass;
  end;
end;

function TdlgSelectionCoordsSystem.GetSelectedEPSGSysCoords(): TLabelSystemesCoordsEPSG;
begin
  Result := FConversionSysteme.GetCodeEPSGNomSysteme(lsbCoordsSystems.ItemIndex);
end;


end.

