unit frmListeProfilsTopo;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  //UnitListesSimplesWithGeneriques,
  ToporobotClasses2012,
  unitProfilTopo,
  UnitClasseMaillage,
  unitUtilsComposants,
  CadreProfilsTopo,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, PairSplitter, StdCtrls, ComCtrls, Types, LCLType;

type

  { TdlgLesProfilsTopo }

  TdlgLesProfilsTopo = class(TForm)
    BitBtn1: TBitBtn;
    btnModifierProfil: TButton;
    btnSupprimerProfil: TButton;
    btnSaveProfils: TButton;
    btnLoadProfils: TButton;
    btnRemoveAllProfils: TButton;
    CdrProfilTopo1: TCdrProfilTopo;
    HeaderControl1: THeaderControl;
    lbNbProfilsTopo: TLabel;
    lsbProfilsTopoMaillage: TListBox;
    mnlMaillages: TPanel;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    procedure btnLoadProfilsClick(Sender: TObject);
    procedure btnRemoveAllProfilsClick(Sender: TObject);
    procedure btnSaveProfilsClick(Sender: TObject);
    procedure btnSupprimerProfilClick(Sender: TObject);
    procedure btnModifierProfilClick(Sender: TObject);
    procedure lsbProfilsTopoMaillageDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbProfilsTopoMaillageSelectionChange(Sender: TObject; User: boolean);
  private
    FMyMaillage: TMaillage;
    FDocTopo   : TToporobotStructure2012;
    procedure DisplayProfil(const P: TProfilTopo);
    procedure ListerProfilsMaillages();
  public
    function  Initialiser(const DT: TToporobotStructure2012; const M: TMaillage): boolean;
    procedure Finaliser();

  end;

var
  dlgLesProfilsTopo: TdlgLesProfilsTopo;

implementation
uses
  DGCDummyUnit,
  CallDialogsStdVersion
  ;

{$R *.lfm}


{ TdlgLesProfilsTopo }
function TdlgLesProfilsTopo.Initialiser(const DT: TToporobotStructure2012; const M: TMaillage): boolean;
begin
  AfficherMessage(Format('%s.Initialiser', [ClassName]));
  result := false;
  lsbProfilsTopoMaillage.ItemIndex := -1;
  self.Caption               := GetResourceString(rsTITRE_PROFILS);
  btnLoadProfils.Caption     := GetResourceString(rsBTN_LOAD_PROFILS);
  btnSaveProfils.Caption     := GetResourceString(rsBTN_SAVE_PROFILS);

  btnModifierProfil.Caption  := GetResourceString(rsBTN_APPLY_MODIFS_PROFIL);
  btnSupprimerProfil.Caption := GetResourceString(rsBTN_REMOVE_PROFIL);
  try
    FMyMaillage := M;
    FDocTopo    := DT;
    ListerProfilsMaillages();
    result := True;
  except
  end;
end;

procedure TdlgLesProfilsTopo.DisplayProfil(const P: TProfilTopo);
begin
  CdrProfilTopo1.Initialiser(FDocTopo, FMyMaillage, P);
end;

procedure TdlgLesProfilsTopo.Finaliser();
begin
  pass;
end;

procedure TdlgLesProfilsTopo.ListerProfilsMaillages();
var
  i, Nb: Integer;
  P: TProfilTopo;
begin
  Nb := FMyMaillage.GetNbProfilsTopo();
  lbNbProfilsTopo.Caption := Format('%d profils', [Nb]);
  lsbProfilsTopoMaillage.Clear;
  lsbProfilsTopoMaillage.ItemIndex := -1;
  if (Nb = 0) then Exit;

  for i := 0 to Nb - 1 do
  begin
    P := FMyMaillage.GetProfilTopo(i);
    lsbProfilsTopoMaillage.Items.Add(P.ProfilName);
  end;
  lsbProfilsTopoMaillage.ItemIndex := 0;
end;

procedure TdlgLesProfilsTopo.lsbProfilsTopoMaillageDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  PF: TProfilTopo;
  procedure DessineItem(const bg,tc: TColor);
  begin
    ResetColorRow(lsbProfilsTopoMaillage, ARect, bg, tc);
    DrawColTexte(lsbProfilsTopoMaillage, ARect, HeaderControl1.Sections.Items[0], false, Format(FORMAT_NB_INTEGER,[Index]));
    DrawColRectColoreWithTexte(lsbProfilsTopoMaillage, ARect, HeaderControl1.Sections.Items[1], True, bg, PF.ProfilColor, '');
    DrawColTexte(lsbProfilsTopoMaillage, ARect, HeaderControl1.Sections.Items[2], True, PF.ProfilName);
  end;
begin
  if (0 = FMyMaillage.GetNbProfilsTopo()) then exit;
  try
    PF := FMyMaillage.GetProfilTopo(Index);
    if (odSelected in state) then DessineItem(clBlue, clWhite) else DessineItem(clWhite, clBlack);
  except
  end;
end;

procedure TdlgLesProfilsTopo.btnSupprimerProfilClick(Sender: TObject);
var
  n: Integer;
begin
  n := lsbProfilsTopoMaillage.ItemIndex;
  if (n < 0) then Exit;
  // Plantage après suppression d'un élément, débogage chiant +++++
  // Parade trouvée: dès que le dernier profil restant est supprimé, on déquille le présent dialoque !!
  if (GHTopoQuestionOuiNon('Supprimer le profil')) then
  begin
    FMyMaillage.RemoveProfilTopo(n);
    if (FMyMaillage.GetNbProfilsTopo() > 0) then ListerProfilsMaillages()
                                            else self.Close;               // on ferme purement et simplement ce dialogue devenu vide
  end;
end;

procedure TdlgLesProfilsTopo.btnSaveProfilsClick(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
  QIdxFilter: integer;
begin
  QFilename := 'Profils_001.csv';
  if (DoDialogSaveFile(rsCSV_FILE_FILTER, '.csv', QFilename, QIdxFilter)) then
  begin
    FMyMaillage.SaveProfilsToFile(QFilename);
  end;
end;

procedure TdlgLesProfilsTopo.btnLoadProfilsClick(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
begin
  QFilename := 'Profils_001.csv';
  if (DoDialogOpenFile(rsCSV_FILE_FILTER, '.csv', QFilename)) then
  begin
    FMyMaillage.LoadProfilsFromFile(QFilename);
    ListerProfilsMaillages();
  end;
end;

procedure TdlgLesProfilsTopo.btnRemoveAllProfilsClick(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Supprimer tous les profils')) then
  begin
    FMyMaillage.ClearProfilsTopo();
    ListerProfilsMaillages();
  end;
end;

procedure TdlgLesProfilsTopo.btnModifierProfilClick(Sender: TObject);
var
  n: Integer;
  PP: TProfilTopo;
begin
  n := lsbProfilsTopoMaillage.ItemIndex;
  if (n < 0) then Exit;
  PP := CdrProfilTopo1.GetProfilTopo();
  FMyMaillage.PutProfilTopo(n, PP);
  ListerProfilsMaillages();
end;
procedure TdlgLesProfilsTopo.lsbProfilsTopoMaillageSelectionChange(Sender: TObject; User: boolean);
var
  PP: TProfilTopo;
  nb: Integer;
begin
  nb := FMyMaillage.GetNbProfilsTopo();
  if (nb > 0) then
  begin
    PP := FMyMaillage.GetProfilTopo(lsbProfilsTopoMaillage.ItemIndex);
    DisplayProfil(PP);
  end;
end;


end.

