unit frmEditAnnotation;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  unitCroquisTerrain,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls;

type

  { TdlgEditAnnotation }

  TdlgEditAnnotation = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnFindStation: TButton;
    btnInsertAltitude: TButton;
    btnInsertIDStation: TButton;
    cmbAnnotationStyle: TComboBox;
    editOffsetX: TCurrencyEdit;
    editOffsetY: TCurrencyEdit;
    editOffsetZ: TCurrencyEdit;
    editPt: TCurrencyEdit;
    editSer: TCurrencyEdit;
    editTexte: TEdit;
    grbxAlignement: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    rbxAlign1: TRadioButton;
    rbxAlign2: TRadioButton;
    rbxAlign3: TRadioButton;
    rbxAlign4: TRadioButton;
    rbxAlign5: TRadioButton;
    rbxAlign6: TRadioButton;
    rbxAlign7: TRadioButton;
    rbxAlign8: TRadioButton;
    rbxAlign9: TRadioButton;
    procedure btnInsertAltitudeClick(Sender: TObject);
    procedure btnInsertIDStationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetAnnAlignmentFromForm(): byte;
    procedure setAnnAlignmentInForm(const A: byte);
    { private declarations }
  public
    { public declarations }
    function PrepareDialog(const FC: TCroquisTerrain; const QAnnotation: TKrobardAnnotation): boolean;
    function GetAnnotation(): TKrobardAnnotation;
  end;

var
  dlgEditAnnotation: TdlgEditAnnotation;

implementation

{$R *.lfm}

{ TdlgEditAnnotation }
procedure TdlgEditAnnotation.setAnnAlignmentInForm(const A: byte);
begin
  case A of
    0: rbxAlign1.Checked := True;
    1: rbxAlign1.Checked := True;
    2: rbxAlign2.Checked := True;
    3: rbxAlign3.Checked := True;
    4: rbxAlign4.Checked := True;
    5: rbxAlign5.Checked := True;
    6: rbxAlign6.Checked := True;
    7: rbxAlign7.Checked := True;
    8: rbxAlign8.Checked := True;
    9: rbxAlign9.Checked := True;
  end;
end;

procedure TdlgEditAnnotation.btnInsertAltitudeClick(Sender: TObject);
begin
  editTexte.Text := editTexte.Text + ' %z';
end;

procedure TdlgEditAnnotation.btnInsertIDStationClick(Sender: TObject);
begin
  editTexte.Text := editTexte.Text + ' %s';
end;

procedure TdlgEditAnnotation.FormShow(Sender: TObject);
begin
  DimensionnerEtCentrerFenetre(self);
end;

function TdlgEditAnnotation.GetAnnAlignmentFromForm(): byte;
begin
  Result := 0;
  if (rbxAlign1.Checked) then Exit(1);
  if (rbxAlign2.Checked) then Exit(2);
  if (rbxAlign3.Checked) then Exit(3);
  if (rbxAlign4.Checked) then Exit(4);
  if (rbxAlign5.Checked) then Exit(5);
  if (rbxAlign6.Checked) then Exit(6);
  if (rbxAlign7.Checked) then Exit(7);
  if (rbxAlign8.Checked) then Exit(8);
  if (rbxAlign9.Checked) then Exit(9);
end;

function TdlgEditAnnotation.GetAnnotation: TKrobardAnnotation;
begin
  Result.Position.IDBaseStation := MakeTIDBaseStation(editSer.AsInteger, editPt.AsInteger, false);
  Result.Position.Offset.X    := editOffsetX.Value;
  Result.Position.Offset.Y    := editOffsetY.Value;
  Result.Position.Offset.Z    := editOffsetZ.Value;
  Result.Alignment   := GetAnnAlignmentFromForm();
  Result.IDStyle     := cmbAnnotationStyle.ItemIndex;
  Result.Texte       := Trim(editTexte.Text);
end;

function TdlgEditAnnotation.PrepareDialog(const FC: TCroquisTerrain; const QAnnotation: TKrobardAnnotation): boolean;
var
  i, n, ST: Integer;
  WU: TTexteAttributs;
  SR: TNumeroSerie;
begin
  Result := false;
  try
    cmbAnnotationStyle.Clear;
    n := FC.GetNbStylesAnnotations();
    for i := 0 to n - 1 do
    begin
      WU := FC.GetStyleAnnotation(i);
      cmbAnnotationStyle.Items.Add(WU.StyleName);
    end;
    cmbAnnotationStyle.ItemIndex := QAnnotation.IDStyle;
    ExtractSerStFromTIDStation(QAnnotation.Position.IDBaseStation, SR, ST);
    editSer.AsInteger    := SR;
    editPt.AsInteger     := ST;
    cmbAnnotationStyle.ItemIndex := QAnnotation.IDStyle;
    setAnnAlignmentInForm(0);
    editOffsetX.Value    := QAnnotation.Position.Offset.X;
    editOffsetY.Value    := QAnnotation.Position.Offset.Y;
    editOffsetZ.Value    := QAnnotation.Position.Offset.Z;
    editTexte.Text       := QAnnotation.Texte;
    result := True;
  except
  end;
end;


end.

