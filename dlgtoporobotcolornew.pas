unit dlgToporobotColorNew;

{$mode delphi}{$H+}

interface

uses
  Common,
  math,
  UnitClassPalette,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, StdCtrls;

type TDialogSelTopoColor = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pbxPalette: TPaintBox;
    pnlGP: TPanel;
    lblIndex: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure pbxPaletteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbxPalettePaint(Sender: TObject);
  private
    { private declarations }
    FPalette:   TPalette256;
    FCanDrawPalette: boolean;
    FCurrIndex: integer;
    procedure DrawPalette();
    procedure UpdateColor(const XX, YY: integer);
    procedure UpdateColorPrm(const Idx: byte);
    function  GetIndex(const XX, YY: integer): byte;
    function  MakeLC(out QL, QT: integer):boolean;
  public
    { public declarations }
    function  Initialiser(const Idx: integer): boolean;
    procedure Finaliser();
    function  GetIdxColor(): integer;
    function  GetColor(): TColor;
  end;

var
  DialogSelTopoColor: TDialogSelTopoColor;

implementation

{$R *.lfm}

{ TDialogSelTopoColor }



procedure TDialogSelTopoColor.DrawPalette();
var
  CCX, CCY: integer;
  i, h, l, t: integer;
  R, RC: TRect;
  TmpBuffer: TBitMap;
begin
  if (not FCanDrawPalette) then Exit;
  CCX      := pbxPalette.Width div 16;
  CCY      := pbxPalette.Height div 16;
  TmpBuffer := TBitmap.Create;
  try
    TmpBuffer.Height   := pbxPalette.Height;
    TmpBuffer.Width    := pbxPalette.Width;
    R.Left   := pbxPalette.Left;
    R.Top    := pbxPalette.Top;
    R.Bottom := pbxPalette.Left + pbxPalette.Height;
    R.Right  := pbxPalette.Top + pbxPalette.Width;
    TmpBuffer.Canvas.Pen.Width := 0;
    TmpBuffer.Canvas.Pen.Color := clBlack;
    TmpBuffer.Canvas.Brush.Color := clWhite;
    TmpBuffer.Canvas.FillRect(R);
    TmpBuffer.Canvas.Brush.Color := clWhite;
    TmpBuffer.Canvas.FillRect(R);
    // dessiner les carrés
    h := 0;
    l := 0;
    for i := 0 to 255 do
    begin
      t := h * CCY;
      if h > 15 then
      begin
        l := l + CCX;
        t := 0;
        h := 0;
      end;
      RC.Left   := l;
      RC.Top    := t;
      RC.Right  := l + CCX;
      RC.Bottom := t + CCY;
      TmpBuffer.Canvas.Brush.Color := FPalette.GetColorByIndex(i);
      TmpBuffer.Canvas.FillRect(RC);
      Inc(h);
    end;
    // dessin du rectangle couleur sélectionné
    if (MakeLC(l, t)) then
    begin
      TmpBuffer.Canvas.Pen.Width := 3;
      TmpBuffer.Canvas.Brush.Color := FPalette.GetColorByIndex(FCurrIndex);
      RC.Left   := l * CCX;
      RC.Top    := t * CCY;
      RC.Right  := (1 + l) * CCX;
      RC.Bottom := (1 + t) * CCY;
      TmpBuffer.Canvas.Rectangle(RC);
      //TmpBuffer.Canvas.FillRect(RC);
    end;
    pbxPalette.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
  end;

end;
function TDialogSelTopoColor.Initialiser(const Idx: integer): boolean;
begin
  FCanDrawPalette := False;
  FCurrIndex := Idx;
  FPalette := TPalette256.Create;
  FPalette.GenerateTOPOROBOTPalette;
  FCanDrawPalette := True;

  pbxPalette.Invalidate;
  lblIndex.Caption := IntToStr(FCurrIndex);

end;

procedure TDialogSelTopoColor.pbxPaletteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpDateColor(X, Y);
end;

procedure TDialogSelTopoColor.pbxPalettePaint(Sender: TObject);
begin
  DrawPalette();
end;

procedure TDialogSelTopoColor.UpdateColor(const XX, YY: integer);
begin
  FCurrIndex := GetIndex(XX, YY);
  UpdateColorPrm(FCurrIndex);
end;

procedure TDialogSelTopoColor.UpdateColorPrm(const Idx: byte);
begin
  FCurrIndex     := Idx;
  lblIndex.Color := FPalette.GetColorByIndex(Idx);
  lblIndex.Caption := Format('#%d', [FCurrIndex]);
  pbxPalette.Invalidate;
end;

procedure TDialogSelTopoColor.Finaliser();
begin
  try
    ;
  finally
    FreeAndNil(FPalette);//FPalette.Free;
  end;
end;



procedure TDialogSelTopoColor.FormShow(Sender: TObject);
begin
  self.Position := poScreenCenter;
  //self.Width  := Min(Screen.Width  - 80, 400);
  //self.Height := Min(Screen.Height - 200, 400);
  //self.Left   := (Screen.Width - self.Width) div 2;
  //self.Top    := (Screen.Height - self.Height) div 2;

  DimensionnerEtCentrerFenetre(self);
end;

function TDialogSelTopoColor.GetColor(): TColor;
begin
  Result := FPalette.GetColorByIndex(FCurrIndex);
end;

function TDialogSelTopoColor.GetIdxColor(): integer;
begin
  Result := FCurrIndex;
end;

function TDialogSelTopoColor.GetIndex(const XX, YY: integer): byte;
var
  CCX, CCY, L, C: Integer;
begin
  Result := 0;
  CCX    := pbxPalette.Width div 16;
  CCY    := pbxPalette.Height div 16;
  L      := YY div CCY;
  C      := XX div CCX;
  //Label4.Caption:=Format('L%d C%d',[L,C]);
  Result := C * 16 + L;
end;

function TDialogSelTopoColor.MakeLC(out QL, QT: integer): boolean;
begin
  result := false;
  if (FCurrIndex < 0) then Exit;
  QT := FCurrIndex mod 16;
  QL := FCurrIndex div 16;
  AfficherMessage(format('I=%d, L=%d, T=%d', [FCurrIndex, QL, QT]));
  result := true;
end;



end.

