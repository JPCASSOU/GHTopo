unit dlgExportVersSIGExt;
// Export vers SIG et le logiciel GHCaveDraw
// 22/10/2013: Une polygonale GHCaveDraw est un fichier *.gcp au lieu de *.gcd
// 14/11/2013: Opérationnel
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees, ToporobotClasses2012, UnitEntitesExtended,
  CallDialogsStdVersion, ConvertisseurJPC,
  unitExportCenterlinesSilhouettesToGIS, Common, Classes, SysUtils,
  LazFileUtils,

  curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons,
  EditBtn, ComCtrls;

type

{ TfrmExportVersSIGExt }

 TfrmExportVersSIGExt = class(TForm)
    BitBtn1: TBitBtn;
    btnCenterlineColor: TColorButton;
    btnSilhouetteColor: TColorButton;
    btnProcess: TButton;
    btnSelectFilename: TButton;
    chkGrpOutputFormats: TCheckGroup;
    chkgrpCenterlinesSilhouettes: TCheckGroup;
    chkUseLeafletLocalLibrary: TCheckBox;
    editTitreDocument: TEdit;
    editSilhouetteOpacity: TCurrencyEdit;
    editLeafletCtxtWidthinPercent: TCurrencyEdit;
    editLeafletCtxtHeight: TCurrencyEdit;
    editExportFilename: TEdit;
    editCenterlineLinewidth: TCurrencyEdit;

    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbEPSGCavite: TStaticText;
    lbLineWidthPolygo: TLabel;
    lbCouleurSilhouette: TLabel;
    lbCouleurPolygo: TLabel;
    lbFichier: TLabel;
    LbStepProcess: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlDefaultColors: TPanel;
    pnlProgression: TPanel;
    ProgressBar1: TProgressBar;
    rgbxModeExport: TRadioGroup;
    procedure btnProcessClick(Sender: TObject);
    procedure btnSelectFilenameClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDocTopo          : TToporobotStructure2012;
    FBDDEntites       : TBDDEntites;
    FConvertisseurEPSG: TConversionSysteme;  // convertisseur EPSG
    procedure AfficherProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
    procedure ExporterGIS(const QFilenameGIS: TStringDirectoryFilename;
                          const DocumentTitle: string;
                          const OutputFormat: TFormatExportGIS;
                          const UseLocalJSLibraries: boolean);
    { private declarations }
  public
    { public declarations }
    function InitialiseByPointer(const QDocTopo: TToporobotStructure2012;
                                 const QBDD    : TBDDEntites): boolean;
  end;

var
  frmExportVersSIGExt: TfrmExportVersSIGExt;

implementation

{$R *.lfm}
function TfrmExportVersSIGExt.InitialiseByPointer(const QDocTopo: TToporobotStructure2012; const QBDD: TBDDEntites): boolean;
var
  i     : Integer;
  MyEPSG: TLabelSystemesCoordsEPSG;
  WU: TStringDirectoryFilename;
begin
  Result := False;
  editLeafletCtxtWidthinPercent.AsInteger := 85; //85%
  editLeafletCtxtHeight.AsInteger         := Screen.Height - 200;

  try
    FDocTopo      := QDocTopo;
    FBDDEntites   := QBDD;
    FBDDEntites.SetProcDisplayProgression(self.AfficherProgression);
    //-----------------------------
    AfficherMessage('Démarrage de l''exportateur GIS');
    // système de coordonnées
    FConvertisseurEPSG := TConversionSysteme.Create;
    if (not FConvertisseurEPSG.Initialiser) then
    begin
      ShowMessage(GetResourceString(rsMSG_PROJ4S_FAIL));
      Exit(false);
    end;
    MyEPSG := FConvertisseurEPSG.GetEPSGSystemeFromCodeEPSG(FDocTopo.GetCodeEPSGSystemeCoordonnees().CodeEPSG);
    lbEPSGCavite.Caption := format('EPSG:%d - %s', [MyEPSG.CodeEPSG, MyEPSG.NomEPSG]);
    editExportFilename.Text := GetGHTopoDirectory() + 'Export001.htm';

    chkGrpOutputFormats.Caption  := GetResourceString(rsEXPORT_SIG_OUTPUT_FORMATS);
    chkGrpOutputFormats.Items.Clear;

    chkGrpOutputFormats.Items.Add(GetResourceString(rsEXPORT_SIG_LEAFLET));
    chkGrpOutputFormats.Items.Add(GetResourceString(rsEXPORT_SIG_GOOGLE_KML));
    chkGrpOutputFormats.Items.Add(GetResourceString(rsEXPORT_SIG_GEO_JSON));
    //chkGrpOutputFormats.Items.Add(GetResourceString(rsEXPORT_SIG_AUTOCAD_DXF));

    chkGrpOutputFormats.Checked[0] := true;
    chkGrpOutputFormats.Checked[1] := true;
    chkGrpOutputFormats.Checked[2] := true;



    LbStepProcess.Caption := '';
    chkUseLeafletLocalLibrary.Caption := GetResourceString(rsEXPORT_SIG_USE_LOCAL_LEAFLET);
    chkUseLeafletLocalLibrary.Checked := false;
    chkUseLeafletLocalLibrary.enabled := DirectoryExists(GetGHTopoDirectory() + 'Leaflet');




    chkgrpCenterlinesSilhouettes.Caption := GetResourceString(rsEXPORT_SIG_CENTERLINE_SILHOUETTES);
    chkgrpCenterlinesSilhouettes.Items.Clear;
    chkgrpCenterlinesSilhouettes.Items.Add(GetResourceString(rsEXPORT_SIG_ENTRANCES));
    chkgrpCenterlinesSilhouettes.Items.Add(GetResourceString(rsEXPORT_SIG_CENTERLINE));
    chkgrpCenterlinesSilhouettes.Items.Add(GetResourceString(rsEXPORT_SIG_SILHOUETTE));
    chkgrpCenterlinesSilhouettes.Items.Add(GetResourceString(rsEXPORT_SIG_POI));
    chkgrpCenterlinesSilhouettes.Items.Add(GetResourceString(rsEXPORT_SIG_NODES));


    chkgrpCenterlinesSilhouettes.Checked[0] := True;
    chkgrpCenterlinesSilhouettes.Checked[1] := true;
    chkgrpCenterlinesSilhouettes.Checked[2] := True;
    chkgrpCenterlinesSilhouettes.Checked[3] := True;
    chkgrpCenterlinesSilhouettes.Checked[4] := True;


    rgbxModeExport.Items.Clear;
    rgbxModeExport.Items.Add(GetResourceString(rsEXPORT_SIG_USE_COLOR_UNIQUE));
    rgbxModeExport.Items.Add(GetResourceString(rsEXPORT_SIG_USE_COLOR_ENTRANCES));
    rgbxModeExport.Items.Add(GetResourceString(rsEXPORT_SIG_USE_COLOR_RESEAUX));

    rgbxModeExport.ItemIndex := 0;
    btnProcess.Caption       := GetResourceString(rsEXPORT_SIG_BTN_GO);
    Result := True;
  except
  end;
end;


procedure TfrmExportVersSIGExt.FormShow(Sender: TObject);
begin
  self.Caption                   := GetResourceString(rsEXPORT_SIG_TITRE);
  lbFichier.Caption              := GetResourceString(rsEXPORT_SIG_LB_FICHIER);
end;


procedure TfrmExportVersSIGExt.btnProcessClick(Sender: TObject);
var
  QFilename, QFileNameWithExt: String;
  QDirectory: TStringDirectoryFilename;
  QTitreDoc: TCaption;
begin
  Panel1.Enabled := False;
  try
    QFilename  := ExtractFileNameWithoutExt(trim(editExportFilename.Text));
    QDirectory := ExtractFilePath(trim(editExportFilename.Text));
    if (not DirectoryExists(QDirectory)) then ForceDirectories(QDirectory);
    QTitreDoc := Trim(editTitreDocument.Text);
    // export selon cases à cocher
    if (chkGrpOutputFormats.Checked[0]) then
    begin
      QFileNameWithExt := QFilename + '.htm';
      ExporterGIS(QFileNameWithExt, QTitreDoc, gisOSM    , chkUseLeafletLocalLibrary.Checked);
    end;
    if (chkGrpOutputFormats.Checked[1]) then
    begin
      QFileNameWithExt := QFilename + '.kml';
      ExporterGIS(QFileNameWithExt, QTitreDoc, gisKML    , chkUseLeafletLocalLibrary.Checked);
    end;
    if (chkGrpOutputFormats.Checked[2]) then
    begin
      QFileNameWithExt := QFilename + '.json';
      ExporterGIS(QFileNameWithExt, QTitreDoc, gisGeoJSON, chkUseLeafletLocalLibrary.Checked);
    end;
  finally
    Panel1.Enabled := true;
  end;
end;


procedure TfrmExportVersSIGExt.btnSelectFilenameClick(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
  QIdxFilter: integer;
  QDefaultExt, QFileFilter: String;
begin
  QFilename   := Trim(editExportFilename.Text);
  QDefaultExt := '.htm'; //ChooseString(cmbFormatExport.ItemIndex, ['.htm', '.kml']);
  QFileFilter := 'GIS files|*.htm; *.kml; *.json'; //ChooseString(cmbFormatExport.ItemIndex, [rsLEAFLET_FILE_FILTER, rsKML_FILE_FILTER]);

  if (DoDialogSaveFile(QFileFilter, QDefaultExt, QFilename, QIdxFilter)) then editExportFilename.Text := QFilename;
end;



procedure TfrmExportVersSIGExt.AfficherProgression(const Etape: string; const Done, Starting, Ending, Step: integer);
begin
  if (Done < 0) then Exit;
  try
    if ((Done mod 20 = 0)) then
    begin
      if (Starting <> ProgressBar1.Min) then ProgressBar1.Min := Starting;
      if (Ending   <> ProgressBar1.Max) then ProgressBar1.Max := Ending;

      ProgressBar1.Position := Done;
      LbStepProcess.Caption := Etape;
      Application.ProcessMessages;
    end;
  except
;
  end;
end;

procedure TfrmExportVersSIGExt.ExporterGIS(const QFilenameGIS: TStringDirectoryFilename;
                                           const DocumentTitle: string;
                                           const OutputFormat: TFormatExportGIS;
                                           const UseLocalJSLibraries: boolean);
var
  QExportGIS: TExportGIS;
  QExportCenterLinesSilhouettes: TExportCenterLinesSilhouettes;
  QColoredBy: TColoredBy;
begin
  FBDDEntites.SortBySerSts();
  LbStepProcess.Caption := '';
  ProgressBar1.Position := 0;
  QExportGIS := TExportGIS.Create;
  try

    QExportCenterLinesSilhouettes := [];
    if (chkgrpCenterlinesSilhouettes.Checked[0]) then QExportCenterLinesSilhouettes := QExportCenterLinesSilhouettes + [expgisENTRANCES];
    if (chkgrpCenterlinesSilhouettes.Checked[1]) then QExportCenterLinesSilhouettes := QExportCenterLinesSilhouettes + [expgisCENTERLINES];
    if (chkgrpCenterlinesSilhouettes.Checked[2]) then QExportCenterLinesSilhouettes := QExportCenterLinesSilhouettes + [expgisSILHOUETTES];
    if (chkgrpCenterlinesSilhouettes.Checked[3]) then QExportCenterLinesSilhouettes := QExportCenterLinesSilhouettes + [expgisPOIs];
    if (chkgrpCenterlinesSilhouettes.Checked[4]) then QExportCenterLinesSilhouettes := QExportCenterLinesSilhouettes + [expgisNODES];
    if (QExportCenterLinesSilhouettes = []) then Exit;
    QColoredBy := TColoredBy(rgbxModeExport.ItemIndex);
    if (QExportGIS.Initialiser(FConvertisseurEPSG,
                               FDocTopo,
                               DocumentTitle,
                               FBDDEntites,
                               AfficherProgression,
                               QExportCenterLinesSilhouettes,
                               QColoredBy,
                               btnCenterlineColor.ButtonColor, 255,
                               editCenterlineLinewidth.Value,
                               btnSilhouetteColor.ButtonColor,
                               editSilhouetteOpacity.AsInteger)) then
    begin
      case OutputFormat of
        gisOSM     : QExportGIS.ExporterToOSM(QFilenameGIS,
                                              editLeafletCtxtWidthinPercent.AsInteger,
                                              editLeafletCtxtHeight.AsInteger,
                                              UseLocalJSLibraries);
        gisKML     : QExportGIS.ExporterToKML(QFilenameGIS);
        gisGeoJSON : QExportGIS.ExporterToGeoJSON(QFilenameGIS);
        gisDXF     : QExportGIS.ExporterToDXF(QFilenameGIS);
      else
        pass;
      end;
    end;
  finally
    FreeAndNil(QExportGIS);
  end;
  LbStepProcess.Caption := GetResourceString(rsDONE_ANY_PROCESS);
  FBDDEntites.SortByDepth();
end;

end.

