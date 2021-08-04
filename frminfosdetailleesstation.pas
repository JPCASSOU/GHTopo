// Infos détaillées d'une station + QRCode
unit frmInfosDetailleesStation;
{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  UnitEntitesExtended,
  Common,
  Classes, SysUtils, FileUtil, ubarcodes, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, StdCtrls, Grids, Spin;

type

  { TdlgInfosQRStation }

  TdlgInfosQRStation = class(TForm)
    btnCopierTableau: TButton;
    lbCouleurSecteur: TStaticText;
    lbNomReseau: TLabel;
    lbCoordX: TStaticText;
    lbCoordY: TStaticText;
    lbCoordZ: TStaticText;
    lbNomSecteur: TLabel;
    QRCode: TBarcodeQR;
    lbSerieSt: TStaticText;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    lbIDLitteral: TStaticText;
    GrdQRCode: TStringGrid;
    editCaseUnitaire: TSpinEdit;
    lbCouleurReseau: TStaticText;
    procedure btnCopierTableauClick(Sender: TObject);
    procedure chkDoMonochromeChange(Sender: TObject);
    procedure editCaseUnitaireChange(Sender: TObject);
    procedure GrdQRCodeDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  private
    { private declarations }
    FMyTableEntites: TBDDEntites;
    FMyStation: TBaseStation;
    FTexteAEncoder: String;
    procedure GenererGrilleQRCode(const Txt: string; const DimCase: integer);
  public
    { public declarations }
    procedure Initialise(const FD: TBDDEntites;
                         const S: TBaseStation);

  end;

var
  dlgInfosQRStation: TdlgInfosQRStation;

implementation

{$R *.lfm}

{ TdlgInfosQRStation }

procedure TdlgInfosQRStation.GrdQRCodeDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  EWE: Integer;
  WU: TColor;
  function GetColorCell(const C: integer): TColor;
  begin
    case C of
     -1: Result := $EEEEEE;
      0: result := clWhite;     // données: Blanc
      1: Result := clBlack;     // données: Noir
     16: Result := clAqua;      // carrés de contrôle (zones blanches)
     17: Result := clBlue;      // carrés de contrôle (zones noires)
     20: Result := clRed;       // carré en M[8,8]: noir
     21: Result := clMaroon;    // carré en M[8,8]: noir
     32: Result := clYellow;    // zones de détrompage: blanc
     33: Result := clMaroon;    // zones de détrompage: noir
     65: Result := clGreen;     // carrés en M[6, 8] et [8, 6]: noir
    otherwise
      result := $EEEEEE;
    end;
    //*)
  end;

begin
  if ((aCol > 0) and (aRow > 0)) then
  begin
    EWE := StrToIntDef(GrdQRCode.Cells[aCol, aRow], -1);
    WU  := GetColorCell(EWE);
    GrdQRCode.Canvas.Pen.Style   := psSolid;
    GrdQRCode.Canvas.Pen.Color   := WU;
    GrdQRCode.Canvas.Brush.Color := WU;
    GrdQRCode.Canvas.Brush.Style := bsSolid;
    GrdQRCode.Canvas.Rectangle(aRect);
    //GrdQRCode.Canvas.TextOut(aRect.Left, aRect.Top, GrdQRCode.Cells[acol, arow]);
  end;
end;

procedure TdlgInfosQRStation.chkDoMonochromeChange(Sender: TObject);
begin
  GenererGrilleQRCode(FTexteAEncoder, editCaseUnitaire.Value);
end;

procedure TdlgInfosQRStation.btnCopierTableauClick(Sender: TObject);
begin
  GrdQRCode.CopyToClipboard();
end;

procedure TdlgInfosQRStation.editCaseUnitaireChange(Sender: TObject);
begin
  GenererGrilleQRCode(FTexteAEncoder, editCaseUnitaire.Value);
end;

procedure TdlgInfosQRStation.GenererGrilleQRCode(const Txt: string; const DimCase: integer);
var
  EWE: TMatriceResultat;
  i, j, L, C: Integer;
begin
  QRCode.Text := Txt;
  QRCode.Generate;
  EWE := QRCode.GetMatrixQRResultat;
  L := QRCode.GetNbQRTaille;
  C := L; //QRBarcode.GetNbQRMtxNbColonnes;

  with GrdQRCode do
  begin
    RowCount := L+3;
    ColCount := L+3;
    for i := 0 to RowCount - 1 do RowHeights[i] := DimCase;
    for j := 0 to ColCount - 1 do ColWidths[j] := DimCase;
    for i := 0 to L-1 do
      for j := 0 to C-1 do
        Cells[i+1, j+1] := IntToStr(EWE[i, j]);
  end;
end;

procedure TdlgInfosQRStation.Initialise(const FD: TBDDEntites; const S: TBaseStation);
var
  EWE: String;
  WU: String;
  RS: TReseau;
  SC: TSecteur;
begin
  FMyTableEntites := FD;
  FMyStation := S;
  EWE := FMyStation.toString();
  WU  := IIF((Trim(FMyStation.IDTerrain) = ''), EWE, FMyStation.IDTerrain);
  self.Caption      := Format('Station: %d.%d: [%s]', [FMyStation.Entite_Serie, FMyStation.Entite_Station, FMyStation.IDTerrain]);

  lbSerieSt.Caption     := EWE;
  lbIDLitteral.Caption  := WU;

  lbCoordX.Caption      := FormatterNombreAvecSepMilliers(FMyStation.PosStation.X);
  lbCoordY.Caption      := FormatterNombreAvecSepMilliers(FMyStation.PosStation.Y);
  lbCoordZ.Caption      := FormatterNombreAvecSepMilliers(FMyStation.PosStation.Z);
  // réseau et secteur
  RS := FMyTableEntites.GetReseau(FMyStation.eReseau);
  SC := FMyTableEntites.GetSecteur(FMyStation.eSecteur);
  lbCouleurReseau.Color    := RS.ColorReseau.toTColor();
  lbCouleurReseau.Caption  := Format(FORMAT_NB_INTEGER, [FMyStation.eReseau]);
  lbNomReseau.Caption      := _AnsiToLCLStr(RS.NomReseau);
  lbCouleurSecteur.Color   := SC.CouleurSecteur.toTColor();
  lbCouleurSecteur.Caption := Format(FORMAT_NB_INTEGER, [FMyStation.eSecteur]);
  lbNomSecteur.Caption     := _AnsiToLCLStr(SC.NomSecteur);

  // init du QR
  FTexteAEncoder := EWE + CR_LF + WU  + CR_LF;
  editCaseUnitaire.Value  := 20;
  GenererGrilleQRCode(FTexteAEncoder, editCaseUnitaire.Value);
end;

end.

