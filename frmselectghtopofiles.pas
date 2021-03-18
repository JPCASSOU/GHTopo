unit frmSelectGHTopoFiles;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  {$IFDEF MSWINDOWS}
  windows, shlobj,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, FileCtrl, ComCtrls, ExtCtrls, StdCtrls, Buttons, EditBtn, PairSplitter,
  IniFiles;

type

  { TdlgSelectGHTopoFiles }

  TdlgSelectGHTopoFiles = class(TForm)
    btnAddUserFolder: TButton;
    btnDropUserFolder: TButton;
    btnOK: TBitBtn;
    btnOK1: TBitBtn;
    btnRemoveDocument: TButton;
    btnQuitterGHTopo: TButton;
    chkAddFolderOfhisAtUserList: TCheckBox;
    editFileName: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    lsbLastDocuments: TListBox;
    lsbMesDocuments: TFileListBox;
    lsbMyDataFolder: TListBox;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    lbCurrentFolder: TStaticText;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    rbxGHTopoGTX: TCheckBox;
    rbxGHTopoXTB: TCheckBox;
    rbxToporobotTAB: TCheckBox;
    rbxToporobotText: TCheckBox;
    rbxPocketTopoTXT: TCheckBox;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure btnAddUserFolderClick(Sender: TObject);
    procedure btnDropUserFolderClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnQuitterGHTopoClick(Sender: TObject);

    procedure btnRemoveDocumentClick(Sender: TObject);
    procedure cmbMyDataFolderChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lsbMyDataFolderClick(Sender: TObject);
    procedure lsbMyDataFolderSelectionChange(Sender: TObject; User: boolean);
    procedure rbxGHTopoGTXChange(Sender: TObject);
    procedure rbxGHTopoXTBChange(Sender: TObject);
    procedure rbxPocketTopoTXTChange(Sender: TObject);
    procedure rbxToporobotTABChange(Sender: TObject);
    procedure rbxToporobotTextChange(Sender: TObject);
  private
    FFileName  : TStringDirectoryFilename;
    FListeUserAdditionalFolders: TStringList;
    FListeRecentFiles: TStringList;
    function GetDirectoryOfUserDesktop(): TStringDirectoryFilename;
    function GetDirectoryOfUserMyDocuments(): TStringDirectoryFilename;
    procedure ListerFichiers(const Idx: integer);
    procedure ListerLastDocuments();
    function MakeFileFilters(): string;
    {$IFDEF MSWINDOWS}function SpecialFolder(Folder: Integer): String;{$ENDIF}
    function GetFolderOfLastDocument(): TStringDirectoryFilename;
    procedure SaveUserFoldersAtINI();

    function  RetrieveUserFoldersFromIni(): boolean;
    function  FindDossiersTopoInGHTopoDir(): boolean;
  public
    function  Initialiser(const AllowQuitGHTopo: boolean; const QListeRecents: TStringList): boolean;
    procedure Finaliser();
    function GetFileName(): TStringDirectoryFilename;
  end;

var
  dlgSelectGHTopoFiles: TdlgSelectGHTopoFiles;

implementation
uses
  CallDialogsStdVersion;

{$R *.lfm}

{$IFDEF MSWINDOWS}
CONST NB_MAX_FOLDERS_RESERVED = 5;
// Obtenir les dossiers spécials de Windows
(*
Selon le répertoire que vous souhaitez obtenir, vous devez utiliser l'une des variables suivantes :

    CSIDL_APPDATA : Répertoire contenant les données des applications ;
    CSIDL_COMMON_FAVORITES : Répertoire contenant les Favoris commun à tous les utilisateurs ;
    CSIDL_COMMON_STARTMENU : Répertoire du menu Démarrer commun à tous les utilisateurs ;
    CSIDL_COMMON_PROGRAMS : Répertoire Programmes du menu Démarrer commun à tous les utilisateurs ;
    CSIDL_COMMON_STARTUP : Répertoire du groupe Démarrage du menu Démarrer commun à tous les utilisateurs ;
    CSIDL_COMMON_DESKTOPDIRECTORY : Répertoire correspondant au bureau commun à tous les utilisateurs ;
    CSIDL_COOKIES : Répertoire ou sont stockés les cookies d'Internet Explorer ;
    CSIDL_DESKTOP : Répertoire correspondant à votre Bureau ;
    CSIDL_DESKTOPDIRECTORY : Répertoire correspondant à votre Bureau ;
    CSIDL_FAVORITES : Répertoire Favoris ;
    CSIDL_FONTS : Répertoire dans lequel sont stockées toutes les polices de caractères ;
    CSIDL_HISTORY : Répertoire contenant l'historique d'Internet Explorer ;
    CSIDL_INTERNET_CACHE : Répertoire ou sont stockés les fichiers temporaires d'Internet Explorer ;
    CSIDL_NETHOOD : Répertoire Voisinage Réseau ;
    CSIDL_PERSONAL : Répertoire Mes Documents ;
    CSIDL_PRINTHOOD : Répertoire de voisinage d'impression ;
    CSIDL_PROGRAMS : Répertoire Programmes du menu Démarrer ;
    CSIDL_RECENT : Répertoire dans lequel se trouvent les raccourcis vers les Fichiers récemment ouverts ;
    CSIDL_SENDTO : Répertoire dans lequel se trouvent les raccourcis Envoyer vers ;
    CSIDL_STARTMENU : Répertoire Menu Démarrer ;
    CSIDL_STARTUP : Répertoire du groupe Démarrage du Menu Démarrer ;
    CSIDL_TEMPLATES : Répertoire contenant les modèles de documents de Windows.
//* ********************************************************************** *)
function TdlgSelectGHTopoFiles.SpecialFolder(Folder: Integer): String;
var
  SFolder : pItemIDList;
  SpecialPath : Array[0..MAX_PATH] Of Char;
begin
  SHGetSpecialFolderLocation(self.Handle, Folder, SFolder);
  SHGetPathFromIDList(SFolder, SpecialPath);
  Result := StrPas(SpecialPath);
end;


// répertoire du burerau utilisateur
function TdlgSelectGHTopoFiles.GetDirectoryOfUserDesktop(): TStringDirectoryFilename;
begin
  Result := SpecialFolder(CSIDL_DESKTOP);
end;
// répertoire de 'Mes Documents'
function TdlgSelectGHTopoFiles.GetDirectoryOfUserMyDocuments(): TStringDirectoryFilename;
begin
  Result := SpecialFolder(CSIDL_PERSONAL);
end;

{$ELSE}
CONST NB_MAX_FOLDERS_RESERVED = 3;
function TdlgSelectGHTopoFiles.GetDirectoryOfUserDesktop(): TStringDirectoryFilename;
begin

end;
function TdlgSelectGHTopoFiles.GetDirectoryOfUserMyDocuments(): TStringDirectoryFilename;
begin

end;
{$ENDIF}

function TdlgSelectGHTopoFiles.GetFolderOfLastDocument(): TStringDirectoryFilename;
var
n: Integer;
begin
// on attrappe tout simplement le dernier document ouvert
// si aucun fichier ouvert, on prend le dossier de GHtopo et on sort
n := FListeRecentFiles.Count;
if (n = 0) then
begin
 Result := GetGHTopoDirectory();
end
else
begin
 Result := ExtractFilePath(FListeRecentFiles[0]);
end;
end;

procedure TdlgSelectGHTopoFiles.SaveUserFoldersAtINI();
var
  INI: TIniFile;
  n, i: Integer;
begin
  n := FListeUserAdditionalFolders.Count;
  if (n = 0) then Exit;
  INI := TINIFile.Create(GetGHTopoDirectory() + GHTOPO_STD_INI_FILENAME);
  try
   for i := 0 to n - 1 do
     INI.WriteString(INI_SECTION_USER_FOLDERS, Format('UserAdditionalFolder%d', [i]), FListeUserAdditionalFolders.Strings[i]);
  finally
    FreeAndNil(INI);//INI.Free;
  end;
end;

function TdlgSelectGHTopoFiles.RetrieveUserFoldersFromIni(): boolean;
var
  INI: TIniFile;
  i, n: Integer;
  EWE: String;
  QList1: TStringList;
WU: TGHStringArray;
begin
  result := false;
  INI := TINIFile.Create(GetGHTopoDirectory() + GHTOPO_STD_INI_FILENAME);
  QList1 := TStringList.Create;
  try
    QList1.Clear;
    FListeUserAdditionalFolders.Clear;
     INI.ReadSectionValues(INI_SECTION_USER_FOLDERS, QList1);
     n := QList1.Count;
     if (n > 0) then
     begin
       for i := 0 to n - 1 do
       begin
         EWE := QList1.Strings[i];
         WU := Split(EWE, '=');
         EWE := Trim(WU[1]);
         if (DirectoryExists(EWE)) then FListeUserAdditionalFolders.Add(EWE);
       end;
     end;
     QList1.Clear;
     Result := True;
  finally
    FreeAndNil(QList1);//QList1.Free ;
    FreeAndNil(INI);//INI.Free;
  end;
end;

function TdlgSelectGHTopoFiles.FindDossiersTopoInGHTopoDir(): boolean;
const
  PREFIX_DOSSIERS_TOPO = '0000_Topo';
var
  QInfo: TRawByteSearchRec;
  Count: Integer;
  EWE: TStringDirectoryFilename;
begin
  Count := 0;
  result := false;

  EWE := GetGHTopoDirectory() + PREFIX_DOSSIERS_TOPO + '*.*';
  AfficherMessage(EWE);
  if (FindFirst (EWE, faDirectory, QInfo) = 0) then
  begin
    Repeat
      Inc(Count);
      //if (QInfo.Attr = faDirectory) then
      begin
        AfficherMessage(QInfo.Name);
        FListeUserAdditionalFolders.Add(QInfo.Name);//lsbMyDataFolder.Items.Add(QInfo.Name);
      end;
    Until (FindNext(QInfo) <> 0);
    FindClose(QInfo);
  end;
  result := (Count > 0);
end;

//******************************************************************************
{ TdlgSelectGHTopoFiles }

procedure TdlgSelectGHTopoFiles.cmbMyDataFolderChange(Sender: TObject);
begin
  ListerFichiers(lsbMyDataFolder.ItemIndex);
end;

procedure TdlgSelectGHTopoFiles.FormShow(Sender: TObject);
begin
  lsbMesDocuments.SetFocus;
end;

procedure TdlgSelectGHTopoFiles.btnOKClick(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: begin  // dossiers utilisateur
         FFileName := lsbMesDocuments.Directory + PathDelim +
                      lsbMesDocuments.Items[lsbMesDocuments.ItemIndex];
       end;
    1: begin
         FFileName := lsbLastDocuments.Items[lsbLastDocuments.ItemIndex];
       end;
    2: begin
         FFileName := editFileName.FileName;
         if (chkAddFolderOfhisAtUserList.Checked) then
         begin
           FListeUserAdditionalFolders.Add(ExtractFileDir(FFileName));
         end;
       end;
  end;
  // sauvegarde des dossiers utilisateurs
  SaveUserFoldersAtINI();
end;

procedure TdlgSelectGHTopoFiles.btnQuitterGHTopoClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TdlgSelectGHTopoFiles.btnAddUserFolderClick(Sender: TObject);
var
  TD: TSelectDirectoryDialog;
begin
  TD := TSelectDirectoryDialog.Create(Application);
  try
    TD.InitialDir := GetGHTopoDirectory();
    //TD.Options :=;
    if (TD.Execute) then FListeUserAdditionalFolders.Add(TD.FileName);
    ListerFichiers(lsbMyDataFolder.Count - 1);

  finally
    FreeAndNil(TD);//TD.Free;
  end;
end;

procedure TdlgSelectGHTopoFiles.btnDropUserFolderClick(Sender: TObject);
begin
  if (FListeUserAdditionalFolders.Count = 0) then exit;
  if (lsbMyDataFolder.ItemIndex < NB_MAX_FOLDERS_RESERVED) then Exit;
  if (GHTopoQuestionOuiNon('Retirer ce dossier')) then
  begin
    FListeUserAdditionalFolders.Delete(FListeUserAdditionalFolders.Count - 1);
    ListerFichiers(0);
  end;
end;



procedure TdlgSelectGHTopoFiles.btnRemoveDocumentClick(Sender: TObject);
var
  i, n: Integer;
begin
  n := FListeRecentFiles.Count;
  if (n = 0) then exit;
  i := lsbLastDocuments.ItemIndex;
  if (i >= 0) then FListeRecentFiles.Delete(i);
  ListerLastDocuments();
end;

procedure TdlgSelectGHTopoFiles.lsbMyDataFolderClick(Sender: TObject);
begin
  ListerFichiers(lsbMyDataFolder.ItemIndex);
end;

procedure TdlgSelectGHTopoFiles.lsbMyDataFolderSelectionChange(Sender: TObject; User: boolean);
begin
  btnDropUserFolder.Enabled := (lsbMyDataFolder.ItemIndex > NB_MAX_FOLDERS_RESERVED);
end;

procedure TdlgSelectGHTopoFiles.rbxGHTopoGTXChange(Sender: TObject);
begin
  ListerFichiers(lsbMyDataFolder.ItemIndex);
end;

procedure TdlgSelectGHTopoFiles.rbxGHTopoXTBChange(Sender: TObject);
begin
  ListerFichiers(lsbMyDataFolder.ItemIndex);
end;

procedure TdlgSelectGHTopoFiles.rbxPocketTopoTXTChange(Sender: TObject);
begin
  ListerFichiers(lsbMyDataFolder.ItemIndex);
end;

procedure TdlgSelectGHTopoFiles.rbxToporobotTABChange(Sender: TObject);
begin
  ListerFichiers(lsbMyDataFolder.ItemIndex);
end;

procedure TdlgSelectGHTopoFiles.rbxToporobotTextChange(Sender: TObject);
begin
  ListerFichiers(lsbMyDataFolder.ItemIndex);
end;

procedure TdlgSelectGHTopoFiles.ListerFichiers(const Idx: integer);
var
  MyFolder: TStringDirectoryFilename;
  EWE_1, EWE_2: String;
  n, i: Integer;
begin
  btnOK.Enabled := true;
  // créer le dossier 'Mes documents topo' si inexistant
  EWE_1 := GetGHTopoDirectory() + MON_DOSSIER_CHIFFRES_TOPO;
  if (not DirectoryExists(EWE_1)) then ForceDirectories(EWE_1);
  EWE_2 := GetGHTopoDirectory() + MON_DOSSIER_QSAVES;
  if (not DirectoryExists(EWE_2)) then ForceDirectories(EWE_2);
  lsbMyDataFolder.Clear;
  lsbMyDataFolder.Items.Add('Dossier du dernier document');
  lsbMyDataFolder.Items.Add('Mes données topo');
  lsbMyDataFolder.Items.Add('Dossier de GHTopo');
  lsbMyDataFolder.Items.Add('Sauvegardes horodatées');
  {$IFDEF MSWINDOWS}
  lsbMyDataFolder.Items.Add('Bureau');
  lsbMyDataFolder.Items.Add('Mes documents');
  {$ENDIF}

  // dossiers utilisateur additionnele
  n := FListeUserAdditionalFolders.Count;
  if (n > 0) then
  begin
    for i := 0 to n-1 do
    begin
      MyFolder := FListeUserAdditionalFolders.Strings[i];
      lsbMyDataFolder.Items.Add(MyFolder);
    end;
  end;
  // les dossiers topo préfixés par 0000_Topo


  lsbMyDataFolder.ItemIndex := Idx;
  case lsbMyDataFolder.ItemIndex of
    0: MyFolder := GetFolderOfLastDocument();
    1: myfolder := EWE_1;
    2: MyFolder := GetGHTopoDirectory();
    3: MyFolder := EWE_2;
    {$IFDEF MSWINDOWS}
    4: MyFolder := GetDirectoryOfUserDesktop();
    5: MyFolder := GetDirectoryOfUserMyDocuments();
    {$ENDIF MSWINDOWS}
  else
    MyFolder := lsbMyDataFolder.Items[lsbMyDataFolder.ItemIndex];
  end;
  lsbMesDocuments.Directory := MyFolder;
  lsbMesDocuments.Mask      := MakeFileFilters();
  lbCurrentFolder.Caption   := MyFolder;

  // nettoyage: suppression des doublons et fichiers inexistants


  if (lsbMesDocuments.Count > 0) then
  begin
    lsbMesDocuments.ItemIndex := 0;

  end;


end;

procedure TdlgSelectGHTopoFiles.ListerLastDocuments();
var
  i, n: Integer;
  S, EWE: String;
begin
  //  nettoyage
  n := FListeRecentFiles.Count;
  if (n = 0) then exit;
  // suppression des fichiers inexistants
  for i := n - 1 downto 0 do
  begin
    EWE := FListeRecentFiles.Strings[i];
    if (not FileExists(EWE)) then FListeRecentFiles.Delete(i);
  end;
  n := FListeRecentFiles.Count;
  if (n = 0) then exit;
  for i := n - 1 downto 0 do
  begin
    if ((i > 1) and (FListeRecentFiles.Strings[i] = FListeRecentFiles.Strings[i-1])) then FListeRecentFiles.Delete(i);
  end;
  lsbLastDocuments.Enabled := false;
  lsbLastDocuments.Clear;
  n := FListeRecentFiles.Count;
  if (n = 0) then Exit;
  for i := 0 to n - 1 do lsbLastDocuments.items.Add(FListeRecentFiles.Strings[i]);
  lsbLastDocuments.ItemIndex := 0;
  lsbLastDocuments.Enabled := (n > 0);
end;

function TdlgSelectGHTopoFiles.MakeFileFilters(): string;
begin
  Result := '';
  if (rbxGHTopoXTB.Checked)     then Result += '*.xtb;';
  if (rbxGHTopoGTX.Checked)     then Result += '*.gtx;';
  if (rbxToporobotTAB.Checked)  then Result += '*.tab;';
  if (rbxToporobotText.Checked) then Result += '*.text;';
  if (rbxPocketTopoTXT.Checked) then Result += '*.txt;';
end;



function TdlgSelectGHTopoFiles.Initialiser(const AllowQuitGHTopo: boolean; const QListeRecents: TStringList): boolean;
begin
  result := false;
  self.Caption                   := GetResourceString(rsDLG_OPEN_DOCUMENT + ' ' + rsGHTOPOEXENAME);
  btnQuitterGHTopo.Caption       := GetResourceString(rsGHTOPO_QUIT);
  btnQuitterGHTopo.Visible       := AllowQuitGHTopo;
  chkAddFolderOfhisAtUserList.Checked := True;
  //lblObligationApostasie.Caption := GetResourceString(rsOBLIGATION_APOSTASIE + rsGHTOPOEXENAME);
  FListeUserAdditionalFolders := TStringList.Create;
  try
    FListeUserAdditionalFolders.Clear;
    FListeUserAdditionalFolders.Sorted     := True;
    FListeUserAdditionalFolders.Duplicates := dupIgnore;
    RetrieveUserFoldersFromIni();
    FindDossiersTopoInGHTopoDir();
  except;
  end;
  FListeRecentFiles := QListeRecents;
  editFileName.Filter := GetResourceString(rsGHTOPO_FILE_FILTER_W_TEXT);
  ListerFichiers(1);
  ListerLastDocuments();
  PageControl1.ActivePageIndex := 0;
  result := true;
end;

procedure TdlgSelectGHTopoFiles.Finaliser();
begin
  try
    FListeUserAdditionalFolders.Clear;
  finally
    FreeAndNil(FListeUserAdditionalFolders);
  end;
end;

function TdlgSelectGHTopoFiles.GetFileName(): TStringDirectoryFilename;
begin
  result := FFileName;
end;

end.

