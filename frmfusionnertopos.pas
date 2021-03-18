// 2018-04-20: Le support de la fusion des topos ajoute une décimale de PI au numéro de version
unit frmFusionnerTopos;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, Common,
  StructuresDonnees,
  ToporobotClasses2012,
  UnitFusionTopos,
  ConvertisseurJPC, FileUtil, SynEdit, ListViewFilterEdit, TreeFilterEdit,
  Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn, FileCtrl, ExtCtrls,
  Buttons, ShellCtrls, PairSplitter, ComCtrls;

type

  { TdlgFusionTopos }

  TdlgFusionTopos = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    btnCleanupListeFichiers: TButton;
    btnUp: TButton;
    btnDown: TButton;
    Button3: TButton;
    btnRemoveItem: TButton;
    CheckBox1: TCheckBox;
    chkDoConvertFromGCSCavite: TCheckBox;
    cmbSystemesCoordonnees: TComboBox;
    editNameOutputFile: TFileNameEdit;
    Label2: TLabel;
    lbConvertisseurReady: TStaticText;
    lsbFichiersAFusionner: TListBox;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    lsbFichiers: TShellListView;
    ShellTreeView1: TShellTreeView;
    memoOutput: TSynEdit;
    procedure btnDownClick(Sender: TObject);
    procedure btnRemoveItemClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnCleanupListeFichiersClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lsbFichiersAFusionnerClick(Sender: TObject);
    procedure ShellTreeView1Change(Sender: TObject; Node: TTreeNode);

  private
    { private declarations }
    FTargetDocuTopo: TToporobotStructure2012;
    FPtrConvertisseurCoordonnees: TConversionSysteme;
    //function ChargerLaTopo(const QDocTopoSource: TToporobotStructure2012; const QFilename: TStringDirectoryFilename): boolean;
    procedure ListerSystemesCoordsInCmbx();
    procedure VirerDoublonsLsbFichiers();
  public
    { public declarations }
    function Initialiser(const FD: TToporobotStructure2012;
                         const CV: TConversionSysteme): boolean;

    procedure Finaliser();
  end;

var
  dlgFusionTopos: TdlgFusionTopos;

implementation

{$R *.lfm}

{ TdlgFusionTopos }
procedure TdlgFusionTopos.Button1Click(Sender: TObject);
var
  n, i: Integer;
  EWE: String;
  function ExisteDeja(const WU: string): boolean;
  var
    NbF, q: Integer;
  begin
    Result := false;
    NbF := lsbFichiersAFusionner.Count;
    if (NbF = 0) then Exit;
    for q := 0 to NbF - 1 do
    begin
      if (lsbFichiersAFusionner.Items[q] = WU) then Exit(true);
    end;
  end;
begin
  n := lsbFichiers.Items.Count;
  if (n = 0) then exit;
  for i := 0 to n - 1 do
  begin
    if (lsbFichiers.Items.Item[i].Selected) then
    begin
      EWE := lsbFichiers.Root + lsbFichiers.Items.Item[i].Caption;
      if (not ExisteDeja(EWE)) then lsbFichiersAFusionner.Items.Add(EWE);
    end;
  end;
  VirerDoublonsLsbFichiers();
end;

procedure TdlgFusionTopos.btnRemoveItemClick(Sender: TObject);
var
  n: Integer;
begin
  n := lsbFichiersAFusionner.ItemIndex;
  if (n >= 0) then lsbFichiersAFusionner.Items.Delete(n);
end;

procedure TdlgFusionTopos.btnDownClick(Sender: TObject);
var
  n0, n1: Integer;
begin
  if (lsbFichiersAFusionner.Count < 2) then exit;
  if (lsbFichiersAFusionner.ItemIndex = lsbFichiersAFusionner.Count - 1) then exit;
  n0 := lsbFichiersAFusionner.ItemIndex;
  n1 := lsbFichiersAFusionner.ItemIndex + 1;
  lsbFichiersAFusionner.Items.Exchange(n0, n1);
  lsbFichiersAFusionner.ItemIndex := n1;
end;

procedure TdlgFusionTopos.btnUpClick(Sender: TObject);
var
  n0, n1: Integer;
begin
  if (lsbFichiersAFusionner.Count < 2) then exit;
  if (lsbFichiersAFusionner.ItemIndex = 0) then exit;
  n0 := lsbFichiersAFusionner.ItemIndex - 1;
  n1 := lsbFichiersAFusionner.ItemIndex;
  lsbFichiersAFusionner.Items.Exchange(n0, n1);
  lsbFichiersAFusionner.ItemIndex := n0;

end;

procedure TdlgFusionTopos.btnCleanupListeFichiersClick(Sender: TObject);
begin
  lsbFichiersAFusionner.Clear;
end;

procedure TdlgFusionTopos.Button3Click(Sender: TObject);
var
  TD: TContainerFusionTopo;
  n, i: Integer;
  MyEPSG: TLabelSystemesCoordsEPSG;
begin
  AfficherMessage('--> Starting fusion container');
  n := lsbFichiersAFusionner.Count;
  if (n = 0) then
  begin
    ShowMessage('Aucun fichier sélectionné');
    Exit;
  end;
  TD := TContainerFusionTopo.Create;
  try
    if (TD.Initialiser(FTargetDocuTopo,
                       FPtrConvertisseurCoordonnees,
                       FTargetDocuTopo.GetCodeEPSGSystemeCoordonnees(),
                       chkDoConvertFromGCSCavite.Checked)) then
    begin
      // ajouter les fichiers à la liste

      for i := 0 to n - 1 do TD.AddFichierAFusionner(lsbFichiersAFusionner.Items[i]);
      // avant de lancer la fusion, vérifier s'il y a des fichiers
      if (0 = TD.GetNbFichiersAFusionner()) then
      begin
        ShowMessage('Aucun fichier valide dans la liste de fusion');
        exit;
      end;
      // attraper le premier fichier
      for i := 0 to n - 1 do
      begin
        memoOutput.Lines.add(format('Fusion de #%d', [i]));
        TD.FusionnerNthTopo(i);
      end;
      TD.Finaliser();
    end;
  finally
    FreeAndNil(TD);//TD.Free;
  end;
end;

procedure TdlgFusionTopos.FormResize(Sender: TObject);
begin
  PairSplitterSide3.Width := self.ClientWidth div 3;
end;

procedure TdlgFusionTopos.VirerDoublonsLsbFichiers();
var
  n, i: Integer;
begin
  n := lsbFichiersAFusionner.Count;
  if (n < 1) then Exit;
  for i := n - 1 downto 1 do
    if (lsbFichiersAFusionner.Items[i] = lsbFichiersAFusionner.Items[i-1]) then lsbFichiersAFusionner.Items.Delete(i);
end;

function TdlgFusionTopos.Initialiser(const FD: TToporobotStructure2012;
                                     const CV: TConversionSysteme): boolean;
begin
  FTargetDocuTopo := FD;
  AfficherMessage(Format('%s.Initialiser()', [ClassName]));
  result := False;
  PairSplitterSide3.Width := Trunc(self.ClientWidth * 0.40);
  FPtrConvertisseurCoordonnees := CV;
  AfficherMessage('--> Checking coordinates converter');
  //if (not FPtrConvertisseurCoordonnees.IsReady()) then Exit;
  ListerSystemesCoordsInCmbx();
  lbConvertisseurReady.Color := IIF(FPtrConvertisseurCoordonnees.IsReady(), clGreen, clRed);
  lsbFichiersAFusionner.Sorted := false;
  lsbFichiersAFusionner.Clear;
  // les fichiers du dossier de travail
  ShellTreeView1.Root := GetGHTopoDirectory();
  ShellTreeView1.Path := GetGHTopoDirectory();
  lsbFichiers.Mask    := '*.xtb';
  result := True;
end;

procedure TdlgFusionTopos.Finaliser();
begin
  try
    pass;
  finally

  end;
end;

procedure TdlgFusionTopos.ListerSystemesCoordsInCmbx();
var
  n, i: Integer;
  EWE: TLabelSystemesCoordsEPSG;
begin
  n := FPtrConvertisseurCoordonnees.GetNbSystemes();
  cmbSystemesCoordonnees.DropDownCount := 20;
  cmbSystemesCoordonnees.Clear;
  for i := 0 to n-1 do
  begin
    EWE := FPtrConvertisseurCoordonnees.GetCodeEPSGNomSysteme(i);
    cmbSystemesCoordonnees.Items.Add(Format('EPSG:%d - %s', [EWE.CodeEPSG, EWE.NomEPSG]));
  end;
  cmbSystemesCoordonnees.ItemIndex := 0;
end;

procedure TdlgFusionTopos.lsbFichiersAFusionnerClick(Sender: TObject);
begin

end;

procedure TdlgFusionTopos.ShellTreeView1Change(Sender: TObject; Node: TTreeNode);
begin

end;

end.

