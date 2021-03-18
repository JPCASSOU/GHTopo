unit CadreRadioButtons;
// Remplace le TTabControl, dont la propriété Style ne fonctionne pas

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Classes, SysUtils, Graphics, Forms, Controls, ComCtrls, StdCtrls, Dialogs, ExtCtrls;

type

  { TCdrRadioButtons }

  TCdrRadioButtons = class(TFrame)
    procedure FrameResize(Sender: TObject);
    procedure MyButtonClick(Sender: TObject);
  strict private
    FCurrentIndex : integer;
    FListeButtons : array of TStaticText;
    FProcAfterSelectRBTN: TProcOfObjectWithOneIntParameter;
    procedure RedrawButtons();
    procedure SelectButton(const QIdx: integer; const DoPerform: boolean);
  private
    procedure CreateButton(const QIndex: integer; const QCaption: TCaption; const QButtonDown: boolean);
  public
    function Initialiser(const Items: array of TCaption;
                         const ItemIndex: integer;
                         const ProcAfterSelectRBTN: TProcOfObjectWithOneIntParameter): boolean;
  end;

implementation
uses
  DGCDummyUnit;
{$R *.lfm}

{ TCdrRadioButtons }

procedure TCdrRadioButtons.FrameResize(Sender: TObject);
begin
  RedrawButtons();
end;

procedure TCdrRadioButtons.MyButtonClick(Sender: TObject);
begin
  SelectButton(TColorButton(Sender).Tag, True);
end;

procedure TCdrRadioButtons.SelectButton(const QIdx: integer; const DoPerform: boolean);
var
  Nb, i: Integer;
begin
  Nb := Length(FListeButtons);
  FCurrentIndex := QIdx;

  for i := 0 to Nb - 1 do
  begin
    try
      if (i = FCurrentIndex) then
      begin
        FListeButtons[i].Color := clAqua;
        FListeButtons[i].Font.Style := FListeButtons[i].Font.Style + [fsBold];
        if (DoPerform and Assigned(FProcAfterSelectRBTN)) then FProcAfterSelectRBTN(FCurrentIndex);
      end
      else
      begin
        FListeButtons[i].Color := clForm;
        FListeButtons[i].Font.Style := FListeButtons[i].Font.Style - [fsBold];
      end;
    except

    end;
  end;
end;
procedure TCdrRadioButtons.RedrawButtons();
var
  Nb, i: Integer;
  BtnHeight, BtnWidth: Int64;
  MyButton: TStaticText;
begin
  Nb := Length(FListeButtons);
  if (Nb = 0) then Exit;
  BtnWidth  := trunc(self.ClientWidth div Nb);
  BtnHeight := self.ClientHeight;

  for i := 0 to Nb - 1 do
  begin
    MyButton := FListeButtons[i];
    MyButton.Top           := 0;
    MyButton.Left          := i * BtnWidth;
    MyButton.Height        := BtnHeight;
    MyButton.Width         := BtnWidth;
    MyButton.BorderWidth   := 0;
    MyButton.Color         := clForm;
  end;
  SelectButton(FCurrentIndex, False);
end;

procedure TCdrRadioButtons.CreateButton(const QIndex: integer; const QCaption: TCaption; const QButtonDown: boolean);
var
  MyButton: TStaticText;
begin
  MyButton := TStaticText.Create(self);
  MyButton.Parent      := self;
  MyButton.ParentColor := false;
  MyButton.Transparent := false;
  MyButton.Name    := Format('Button%.2d', [QIndex]);
  MyButton.Tag     := QIndex;
  MyButton.Caption := QCaption;
  MyButton.OnClick := MyButtonClick;
  MyButton.Enabled := True;
  MyButton.Visible := True;
  MyButton.Color   := clForm;
  MyButton.BorderStyle := sbsSingle;
  MyButton.Alignment   := taCenter;

  FListeButtons[QIndex] := MyButton;
end;


function TCdrRadioButtons.Initialiser(const Items: array of TCaption;
                                      const ItemIndex: integer;
                                      const ProcAfterSelectRBTN: TProcOfObjectWithOneIntParameter): boolean;
var
  i, Nb: Integer;
  procedure PurgerLstButtons();
  var
    QNb, Qi: Integer;
  begin
    QNb := Length(FListeButtons);
    if (0 = QNb) then Exit;
    for Qi := 0 to QNb-1 do
    begin
      try
        FreeAndNil(FListeButtons[Qi]);
      finally
      end;
    end;
    SetLength(FListeButtons, 0);
  end;
begin
  result := false;
  FCurrentIndex := 0;
  FProcAfterSelectRBTN := ProcAfterSelectRBTN;
  Nb := length(Items);
  if (0 = Nb) then Exit;
  PurgerLstButtons();
  SetLength(FListeButtons, Nb);
  for i := 0 to Nb - 1 do CreateButton(i, Items[i], (ItemIndex = i));
  RedrawButtons();
end;

end.

