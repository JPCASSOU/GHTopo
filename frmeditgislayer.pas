unit frmEditGISLayer;

{$mode delphi}


interface

uses
  StructuresDonnees, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, curredit;

type

  { TdlgEditGisLayer }

  TdlgEditGisLayer = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnMarkerColor: TColorButton;
    cmbMarkerStyle: TComboBox;
    editMarkerOpacity: TCurrencyEdit;
    editLayerAttribution: TEdit;
    editLayerName: TEdit;
    editLayerVarName: TEdit;
    editLayerDescription: TEdit;
    editMarkerSize: TCurrencyEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
  private

  public
    function Initialiser(const QLayer: TGISLayer): boolean;
    function GetGisLayer(): TGISLayer;
  end;

var
  dlgEditGisLayer: TdlgEditGisLayer;

implementation

{$R *.lfm}

{ TdlgEditGisLayer }

function TdlgEditGisLayer.Initialiser(const QLayer: TGISLayer): boolean;
begin
  result := false;
  try
    editLayerVarName.Text      := QLayer.LayerVarName;
    editLayerName.Text         := qlayer.LayerTitle;
    editLayerDescription.Text  := QLayer.LayerDescription;
    editLayerAttribution.Text  := QLayer.LayerAttribution;

    btnMarkerColor.ButtonColor := QLayer.SymbolColor.toTColor();
    editMarkerSize.Value       := QLayer.SymbolSize;
    editMarkerOpacity.AsInteger:= QLayer.SymbolColor.Alpha;
    cmbMarkerStyle.Clear;
    cmbMarkerStyle.Items.Add('Marqueur OSM');
    cmbMarkerStyle.Items.Add('Cercle');
    cmbMarkerStyle.Items.Add('Carré');
    cmbMarkerStyle.Items.Add('Triangle pointe en haut');
    cmbMarkerStyle.Items.Add('Triangle pointe en bas');
    cmbMarkerStyle.Items.Add('Losange');
    cmbMarkerStyle.ItemIndex   := QLayer.SymboleStyle;
    result := True;
  except

  end;

end;

function TdlgEditGisLayer.GetGisLayer(): TGISLayer;
begin
  Result.LayerVarName          := Trim(editLayerVarName.Text);
  Result.LayerTitle            := Trim(editLayerName.Text);
  Result.LayerDescription      := Trim(editLayerDescription.Text);
  Result.LayerAttribution      := Trim(editLayerAttribution.Text);

  Result.SymboleStyle          := cmbMarkerStyle.ItemIndex;
  Result.SymbolSize            := editMarkerSize.Value;
  Result.SymbolColor.setFrom(btnMarkerColor.ButtonColor, editMarkerOpacity.AsInteger);
end;

end.

