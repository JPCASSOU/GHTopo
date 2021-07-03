unit DGCUnitListeStylesObjets;
// Gestion de la liste des feuilles de styles


{$mode delphiunicode}

interface

uses
  DGCTypes,
  DGCUtilityFunctions,
  DGCListesSimples,
  Classes, SysUtils, Graphics;

type TDGCListeStylesSheets = class(TDGCListeSimple<TDGCStyleSheet>)
 private

 public
   procedure ClearListe(const DoRegenDefaultStyle: boolean); overload;
   function  FindIdxByName(const N: string): integer;
end;

implementation
uses
  DGCDummyUnit; // pour éviter le bug de 'Fin de code source non trouvée

//******************************************************************************
procedure TDGCListeStylesSheets.ClearListe(const DoRegenDefaultStyle: boolean);
var
  Nb, i: Integer;
  MyDefaultStyle: TDGCStyleSheet;
begin
  Nb := self.Count;
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      try
        Dispose(self.Items[i]);
      except
      end;
    end;
  end;
  self.Clear();
  if (DoRegenDefaultStyle) then
  begin
    with MyDefaultStyle do
    begin
      Stylename      := 'default';
      Description    := 'Default Stylesheet';

      PenColor.setFrom(clBlack, 255);
      PenStyle       := psSolid;
      PenWidthInPX   := 0;
      PenWidthInMM   := DGC_DEFAULT_PEN_WIDTH_IN_MM;


      BrushColor.setFrom(clWhite, 255);
      BrushStyle     := bsClear;

      FontName       := DGC_DEFAULT_FONT_NAME;
      FontColor.setFrom(clBlack, 255);
      FontSizeInPts  := 12;
      FontSizeInMM   := 8;
      FontStyle      := [];
    end;
    self.AddElement(MyDefaultStyle);

  end;
end;







function TDGCListeStylesSheets.FindIdxByName(const N: string): integer;
var
  i, Nb: Integer;
  EWE: TDGCStyleSheet;
begin
  Result := DIEU_AU_CARRE;
  Nb := self.GetNbElements();
  if (Nb = 0) then exit;
  for i := 0 to Nb - 1 do
  begin
    EWE := self.GetElement(i);
    if (N = EWE.Stylename) then Exit(i);
  end;
end;

end.

