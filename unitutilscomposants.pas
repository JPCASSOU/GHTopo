unit unitUtilsComposants;
// Opérations sur des contrôles (listbox, etc...)
// 06/02/2014: Remplacement de l'évaluateur d'expressions
// 25/09/2018: DrawPipistrelle est factorisé
// 11/10/2018: Pas mal de factorisations et corrections
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees
  , Common
  , ConvertisseurJPC
  , BGRABitmap
  , BGRACanvas
  , math
  , StdCtrls, Grids, CheckLst
  , Classes, SysUtils, Graphics
  , ComCtrls
  , Forms
  , Dialogs
  ;


  procedure RemplirCombosSystemesCoordonnees(const FC: TConversionSysteme; const Cmb: TComboBox; const DoUseEPSG: boolean; const IdxOrEPSG: integer);
  procedure SetVoyantMode(const V: TStaticText; const b: boolean); inline;

  procedure RemplirComboBoxCOMPorts(const cmbPorts: TComboBox; const qIdx: integer);
  function  getLastNumLigneDonnees(const Grd: TStringGrid): integer;
  // recherche dans des TStrings
  function FindIdxOccurrenceInTStrings(const QList: TStrings; const FindWhat: string): integer;
  function  FindIdxOccurrenceInCheckListBox(const QListBox: TCheckListBox; const FindWhat: string; out QIdx: integer): boolean;


  procedure InitialiserComboBoxQuadrillage(const QCmbx: TComboBox; const QIdx: TQdrType);
  procedure InitialiserComboBoxRepresentationVisees(const QCmbx: TComboBox; const QIdx: TModeRepresentationGaleries);

  procedure PositionnerFenetre(const Wnd: TForm; const X, Y, L, H: integer);
  //procedure DrawBandeauDeuil(const Cnv: TBGRACanvas);
  procedure DrawPipistrelle(const TmpBuffer: TBGRABitmap);
  // pour le dessin des items de listes
  procedure DessineFiletColonne(const LSB: TListBox; const ARect: TRect; const TB: integer); inline;
  procedure DessineRectangleColore(const LSB: TListBox; const ARect: TRect; const QBG, PC, BC: TColor; const L, R, T, B: integer);
  procedure ResetColorRow(const LSB: TListBox; const ARect: TRect; const qbg, qtc: TColor);
  procedure DrawColTexte(const LSB: TListBox; const ARect: TRect; const QHS: THeaderSection; const DoDrawFilet: boolean; const QText: string);
  procedure DrawColCoordsXYZ(const LSB: TListBox; const ARect: TRect; const QHS: THeaderSection; const QbgColor, QErrColor: TColor; const QValue: double);
  procedure DrawColRectColoreWithTexte(const LSB: TListBox; const ARect: TRect; const QHS: THeaderSection; const QDoDrawFilet: boolean; const QBG, QRectColor: TColor; const QText: string);
  // barres de prograssion
  procedure ParametrerProgressbar(const PB: TProgressBar; const QMin, QMax, QPosition: integer);


implementation
procedure RemplirCombosSystemesCoordonnees(const FC: TConversionSysteme; const Cmb: TComboBox; const DoUseEPSG: boolean; const IdxOrEPSG: integer);
var
  i: integer;
  EWE: TLabelSystemesCoordsEPSG;
  WU: Integer;
begin
  Cmb.Clear;
  Cmb.DropDownCount := Min(20, FC.GetNbSystemes);
  for i := 0 to FC.GetNbSystemes - 1 do
  begin
    EWE := FC.GetCodeEPSGNomSysteme(i);
    Cmb.Items.Add(Format('EPSG:%d - %s' ,[EWE.CodeEPSG, EWE.NomEPSG]));
  end;
  if (DoUseEPSG) then
  begin
    WU := FC.GetIndexSystemeByCodeEPSG(IdxOrEPSG);
    if (WU > 0) then Cmb.ItemIndex := WU else Cmb.ItemIndex := IdxOrEPSG;
  end
  else
  begin
    Cmb.ItemIndex := IdxOrEPSG;
  end;
end;

procedure SetVoyantMode(const V: TStaticText; const b: boolean); inline;
begin
  V.Color := IIF(b, clLime, clRed);
end;

{$IFDEF MSWINDOWS}
procedure RemplirComboBoxCOMPorts(const cmbPorts: TComboBox; const qIdx: integer);
const
  NbPorts = 64;
var
  i: Integer;
begin
  cmbPorts.Clear;
  cmbPorts.DropDownCount := Min(NbPorts, 8);
  for i := 0 to NbPorts - 1 do cmbPorts.Items.Add(Format('COM%d', [i]));
  cmbPorts.ItemIndex := qIdx;   // pour DistoX
end;
{$ENDIF}
{$IFDEF LINUX}
procedure RemplirComboBoxCOMPorts(const cmbPorts: TComboBox; const qIdx: integer);
var
  i: Integer;
begin
  cmbPorts.Clear;
  cmbPorts.DropDownCount := 8;
  for i := 0 to 15 do cmbPorts.Items.Add(Format('/dev/ttyS%d', [i]));     // ports série
  for i := 0 to 15 do cmbPorts.Items.Add(Format('/dev/rfcomm%d', [i]));   // ports RFCOMM
  cmbPorts.ItemIndex := qIdx;   // pour DistoX
end;
{$ENDIF LINUX}

function getLastNumLigneDonnees(const Grd: TStringGrid): integer;
var
  i, NbL: Integer;
  WU: String;
  function ConcatAllCols(const r: integer): string;
  var
    j, NbC: Integer;
  begin
    Result := '';
    NbC := Grd.ColCount;
    for j := 1 to 3 do Result += Trim(Grd.Cells[j, r]);
  end;
begin
  Result := 0;
  NbL := Grd.RowCount;
  for i := 1 to NbL - 1 do
  begin
    WU  := ConcatAllCols(i);
    if (length(WU) < 3) then Exit(i);
  end;
end;
// recherche dans des TStrings
function FindIdxOccurrenceInTStrings(const QList: TStrings; const FindWhat: string): integer;
var
  S, EWE: String;
  WU, i: LongInt;
  qn: Integer;
  function QFindIndex(const QIdx: integer): integer;
  var
    j: Integer;
    QAT: TGHStringArray;
    n: integer;
  begin
    Result := -1;
    try
      for j := 0 to QList.Count - 1 do
      begin
        QAT := Split(Trim(QList[j]), ':');
        n := StrToIntDef(QAT[0], -1);
        if (n = QIdx) then Exit(j);
      end;
    except
    end;
  end;
begin
  Result := -1;
  if (QList.Count = 0) then Exit;
  S := lowercase(RemoveDiacritiques(FindWhat));
  // recherche d'un signe égal en tête => extraction de l'index
  if (S[1] = '=') then
  begin
    System.Delete(S, 1, 1);
    WU := StrToIntDef(S, 0);
    qn := QFindIndex(WU);
    if (IsInRange(qn, 0, QList.Count - 1)) then Exit(qn);
  end
  else  // recherche sur les items
  begin
    for i := 0 to QList.Count - 1 do
    begin
      EWE := lowercase(RemoveDiacritiques(Trim(QList[i])));
      if (Pos(S, EWE) > 0) then Exit(i);
    end;
  end;
end;


// recherche dans une checklistbox
function FindIdxOccurrenceInCheckListBox(const QListBox: TCheckListBox; const FindWhat: string; out QIdx: integer): boolean;
begin
  QIdx := FindIdxOccurrenceInTStrings(QListBox.Items, FindWhat);
  Result := (QIdx >= 0);
end;

procedure InitialiserComboBoxRepresentationVisees(const QCmbx: TComboBox; const QIdx: TModeRepresentationGaleries);
begin
  QCmbx.Style := csDropDownList;
  QCmbx.Clear;
    QCmbx.Items.add(GetResourceString(rsVUE2D_REPRESENTATION_ENTRANCES));
    QCmbx.Items.Add(GetResourceString(rsVUE2D_REPRESENTATION_NETWORKS));
    QCmbx.Items.Add(GetResourceString(rsVUE2D_REPRESENTATION_SECTEURS));
    QCmbx.Items.Add(GetResourceString(rsVUE2D_REPRESENTATION_EXPES));
    QCmbx.Items.Add(GetResourceString(rsVUE2D_REPRESENTATION_GRAY));
    QCmbx.Items.Add(GetResourceString(rsVUE2D_REPRESENTATION_DEPTH));
    QCmbx.Items.Add(GetResourceString(rsVUE2D_REPRESENTATION_TYPES));
  QCmbx.ItemIndex := ord(QIdx);
end;

procedure PositionnerFenetre(const Wnd: TForm; const X, Y, L, H: integer);
begin
  Wnd.Left := X;
  Wnd.Top  := Y;
  Wnd.Width := L;
  Wnd.Height := H;
  Wnd.WindowState := wsNormal;
end;

procedure InitialiserComboBoxQuadrillage(const QCmbx: TComboBox; const QIdx: TQdrType);
begin
  QCmbx.Style := csDropDownList;
  QCmbx.Clear;
  QCmbx.Items.add(GetResourceString(rsQDNONE));
  QCmbx.Items.add(GetResourceString(rsQDGRID));
  QCmbx.Items.add(GetResourceString(rsQDCROSS));
  QCmbx.Items.add(GetResourceString(rsQDPOINTS));
  QCmbx.ItemIndex := Ord(QIdx);
end;
(*
procedure DrawBandeauDeuil(const Cnv: TBGRACanvas);
const
  MODULE = 20;
  K1     = 2.50;
  K2     = 4.00;
var
  PP: ARRAY[0..3] of TPoint;
  QQ1, QQ2: Int64;
begin
  QQ1 := round (K1 * MODULE);
  QQ2 := round (K2 * MODULE);
  PP[0] := MakeTPoint(QQ1  , 0);
  PP[1] := MakeTPoint(0    , QQ1);
  PP[2] := MakeTPoint(0    , QQ2);
  PP[3] := MakeTPoint(QQ2  , 0);
  Cnv.Brush.Color := clBlack;
  Cnv.Brush.Style := bsSolid;
  Cnv.Pen.Color   := clBlack;
  Cnv.Polygon(PP);
end;
//*)

procedure DrawPipistrelle(const TmpBuffer: TBGRABitmap);
const
  UnitLongueur: integer = 2;
var
  i, j: integer;
  TraX, TraY: integer;
  Pipistrelle: array[0..1, 0..13] of TPoint;
begin
  //AfficherMessage(' --> DrawPipistrelle');

  TraX := TmpBuffer.Width - UnitLongueur * 16;
  TraY := TmpBuffer.Height - UnitLongueur * 16;
  for i := 0 to 13 do
  begin
    Pipistrelle[0, i] := MakeTPoint(0, 0);
    Pipistrelle[1, i] := MakeTPoint(0, 0);
  end;
  Pipistrelle[0, 0]  := MakeTPoint(0, 6 * UnitLongueur);
  Pipistrelle[0, 1]  := MakeTPoint(0, -9 * UnitLongueur);
  Pipistrelle[0, 2]  := MakeTPoint(16 * UnitLongueur, -9 * UnitLongueur);
  Pipistrelle[0, 3]  := MakeTPoint(0, -5 * UnitLongueur);
  Pipistrelle[0, 4]  := MakeTPoint(3 * UnitLongueur, -6 * UnitLongueur);
  Pipistrelle[0, 5]  := MakeTPoint(8 * UnitLongueur, -1 * UnitLongueur);
  Pipistrelle[0, 6]  := MakeTPoint(13 * UnitLongueur, -1 * UnitLongueur);
  Pipistrelle[0, 7]  := MakeTPoint(8 * UnitLongueur, 4 * UnitLongueur);
  Pipistrelle[0, 9]  := MakeTPoint(2 * UnitLongueur, 2 * UnitLongueur);
  Pipistrelle[0, 8]  := MakeTPoint(0, 1 * UnitLongueur);
  Pipistrelle[0, 10] := MakeTPoint(4 * UnitLongueur, 8 * UnitLongueur);
  Pipistrelle[0, 11] := MakeTPoint(0, 6 * UnitLongueur);
  Pipistrelle[0, 12] := MakeTPoint(4 * UnitLongueur, -8 * UnitLongueur);
  Pipistrelle[0, 13] := MakeTPoint(5 * UnitLongueur, -8 * UnitLongueur);
  for i := 0 to 13 do
  begin
    Pipistrelle[1, i] := MakeTPoint(-Pipistrelle[0, i].X, Pipistrelle[0, i].Y);
    Pipistrelle[1, i] := MakeTPoint(Pipistrelle[1, i].X + TraX, TmpBuffer.Height - (Pipistrelle[1, i].Y + TraY));
    Pipistrelle[0, i] := MakeTPoint(Pipistrelle[0, i].X + TraX, TmpBuffer.Height - (Pipistrelle[0, i].Y + TraY));
  end;
  with TmpBuffer.CanvasBGRA do
  begin
    Pen.Color := clBlue;
    Pen.Width := 0;
    for j := 0 to 1 do
    begin
      MoveTo(Pipistrelle[j, 0]);
        LineTo(Pipistrelle[j, 1]);
        LineTo(Pipistrelle[j, 2]);
      MoveTo(Pipistrelle[j, 3]);
      for i :=  4 to  8 do LineTo(Pipistrelle[j, i]);
      MoveTo(Pipistrelle[j, 9]);
      for i := 10 to 11 do LineTo(Pipistrelle[j, i]);
      MoveTo(Pipistrelle[j, 4]);
      for i := 12 to 13 do LineTo(Pipistrelle[j, i]);
    end;
  end;
end;
//------------------------------------------------------------------------------
// dessin des éléments d'un Listbox
procedure ResetColorRow(const LSB: TListBox; const ARect: TRect; const qbg, qtc: TColor);
begin
  LSB.Canvas.Brush.Color:= qbg;
  LSB.Canvas.Font.Color := qtc;
  LSB.Canvas.Pen.Color  := clSilver; // pour les filets
  LSB.Canvas.FillRect(ARect);
end;
procedure DessineFiletColonne(const LSB: TListBox; const ARect: TRect; const TB: integer); inline;
begin
  LSB.Canvas.Line(TB, ARect.Top, TB, ARect.Bottom);
end;
procedure DessineRectangleColore(const LSB: TListBox; const ARect: TRect; const QBG, PC, BC: TColor; const L, R, T, B: integer);
begin
  LSB.canvas.Pen.Color   := PC;
  LSB.canvas.Brush.Color := BC;
  LSB.canvas.Rectangle(Rect(L, T, R, B));
  // et on restaure l'ancienne couleur
  LSB.canvas.Pen.Color   := clSilver;
  LSB.Canvas.Brush.Color := QBG;
end;

procedure DrawColTexte(const LSB: TListBox; const ARect: TRect; const QHS: THeaderSection; const DoDrawFilet: boolean; const QText: string);
const
  Q4 = 4;
  mg = 1;
var
  QR: TRect;
begin
  QR := Rect(QHS.Left, ARect.Top + mg, QHS.Right - 8, ARect.Bottom - mg);
  if (DoDrawFilet) then DessineFiletColonne(LSB, ARect, QHS.Left - Q4);
  LSB.canvas.TextRect(QR, QHS.Left + 4, ARect.Top+1, _AnsiToLCLStr(QText));
end;
procedure DrawColCoordsXYZ(const LSB: TListBox; const ARect: TRect; const QHS: THeaderSection; const QbgColor, QErrColor: TColor; const QValue: double);
const
  Q4 = 4;
  mg = 1;
begin
  DessineFiletColonne(LSB, ARect, QHS.Left - Q4);
  if (QValue < 0.01) then
  begin
    LSB.Canvas.Brush.Color := QErrColor;
    LSB.Canvas.Rectangle(QHS.Left, ARect.Top, QHS.Left + QHS.Width - 4, ARect.Bottom);
  end;
  LSB.canvas.TextOut(QHS.Left + 4, ARect.Top+1, FormatterNombreAvecSepMilliers(QValue));
  LSB.Canvas.Brush.Color := QbgColor;
end;
procedure DrawColRectColoreWithTexte(const LSB: TListBox; const ARect: TRect; const QHS: THeaderSection; const QDoDrawFilet: boolean; const QBG, QRectColor: TColor; const QText: string);
const
  Q4 = 4;
  mg = 1;
var
 QR: TRect;
 QOlgBG: TColor;
begin
 QOlgBG := LSB.Canvas.Brush.Color;
 if (QDoDrawFilet) then DessineFiletColonne(LSB, ARect, QHS.Left - Q4);
 if (Trim(QText) <> '') then
 begin
   DessineRectangleColore(LSB, ARect, QBG, clBlack, QRectColor, QHS.Left, QHS.Left + 32, ARect.Top + mg, ARect.Bottom - mg);
   LSB.Canvas.TextOut(QHS.Left + 32 + 4, ARect.Top+1, _AnsiToLCLStr(QText));
 end else
   DessineRectangleColore(LSB, ARect, QBG, clBlack, QRectColor, QHS.Left, QHS.Right - 6, ARect.Top + mg, ARect.Bottom - mg);
 LSB.Canvas.Brush.Color := QOlgBG;
end;

procedure ParametrerProgressbar(const PB: TProgressBar; const QMin, QMax, QPosition: integer);
begin
  PB.Min := QMin;
  PB.Max := QMax;
  PB.Position := QPosition;
end;



end.

