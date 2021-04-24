unit dlgHelpSystem;
// Système d'aide. Fonctionnalités de modification non implémentées (peu utilisées).
// 12/12/2013: Regroupement des fenêtres "A propos" et "Système d'aide"
// 14/04/2014: Ajout du QRCode
// 25/04/2016: Corrections diverses
// 29/12/2016: Simplification et réduction de la taille fenêtre
// 03/09/2020: Pointage temporel
{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  dateutils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, ExtCtrls,  UnitHelpSystemXML,
  ubarcodes;
const
  URL_GETTING_GHTOPO = 'http://ghtopo.blog4ever.com';

type

  { TfrmHelpSystem }

  TfrmHelpSystem = class(TForm)
    btnGoURLGHTopo: TButton;
    btnQRCode: TButton;
    cmbTitles: TComboBox;
    editToEncodeQRCode: TEdit;
    lbUtilisateurFinal: TLabel;
    memoAdditionnalInfos: TMemo;
    QRCode: TBarcodeQR;
    BitBtn1: TBitBtn;
    lbGHTopoName: TLabel;
    lsbResolutionEcrans: TListBox;
    memoText: TMemo;
    PageControl1: TPageControl;
    tabShtQRCode: TTabSheet;
    tabshtHelpSystem: TTabSheet;
    tabShtAbout: TTabSheet;
    procedure btnGoURLGHTopoClick(Sender: TObject);
    procedure btnQRCodeClick(Sender: TObject);
    procedure cmbTitlesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);


  private
    { private declarations }
    FHelpFile : THelpFileStructure;
    procedure PutSectionInForm(const S: THelpFileSection);
    procedure PutSectionInFormByIdx(const Idx: integer);
    procedure ShowAPropos();
  public
    { public declarations }
    function Initialiser(const HelpFile: string; const QTopics: string; const DoDisplaySandbox: boolean): boolean;
  end;

var
  frmHelpSystem: TfrmHelpSystem;

implementation
type TArrayPlagesValeurs = array of double;

{$R *.lfm}

function GenererTexteAleatoire(): string;
const
  ASC_SPC   = ord(' ');
  ASC_TILDA = ord('~');
  ASC_PRINTABLE_INTERVAL = ASC_TILDA - ASC_SPC;
var
 c: Integer;
 Q: Char;
 NN: Int64;
begin

 NN := 1 + Trunc(64 * Random());
 Result := '';
 for c := 0 to NN - 1 do
 begin
   // caractères imprimables: De l'espace ($32) au tilde ($
   Q := chr(ASC_SPC + Trunc(ASC_PRINTABLE_INTERVAL * Random()));
   Result += Q;
 end;
end;
{ TfrmHelpSystem }

procedure TfrmHelpSystem.FormCreate(Sender: TObject);
begin
  FHelpFile := THelpFileStructure.Create;
end;


procedure TfrmHelpSystem.btnGoURLGHTopoClick(Sender: TObject);
var
  QUrl: String;
begin
  QUrl := URL_GETTING_GHTOPO;
  {$IFDEF MSWINDOWS}
    ShellExecute(0,'open',PChar(QUrl),'','',SW_SHOW);
  {$ELSE}
    pass;
  {$ENDIF}
end;

procedure TfrmHelpSystem.btnQRCodeClick(Sender: TObject);
begin
  QRCode.Text := Trim(editToEncodeQRCode.Text);
end;



procedure TfrmHelpSystem.cmbTitlesChange(Sender: TObject);
begin
  PutSectionInFormByIdx(cmbTitles.ItemIndex);
end;



procedure TfrmHelpSystem.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHelpFile);//FHelpFile.Free;
end;

procedure TfrmHelpSystem.FormResize(Sender: TObject);
begin
  QRCode.Width := QRCode.Height;
end;

procedure TfrmHelpSystem.FormShow(Sender: TObject);
begin
  self.Caption := GetResourceString(rsHLP_TITRE);
  tabshtHelpSystem.Caption       := GetResourceString(rsHLP_TAB_RUBRIQUES);
  tabShtAbout.Caption            := GetResourceString(rsHLP_TAB_ABOUT);
  tabShtQRCode.Caption           := 'QRCode';
  ShowAPropos();
  DimensionnerEtCentrerFenetre(self);
end;




procedure TfrmHelpSystem.ShowAPropos;
const
  FMT_ECRANS_RESOL = 'Monitor %d: %dx%d - %s';
var
  DateCompilation: TDateTime;
  i, W, H: Integer;
  M: TMonitor;
  {$IFDEF MSWINDOWS}
  OSVersion: TOSVersionInfo;
  {$ENDIF}
  YYYY, MM, DD, HH, MN, SS, MS: word;
  WU, EWE: String;
begin
  btnGoURLGHTopo.Caption := URL_GETTING_GHTOPO;
  DateCompilation      := FileDateToDateTime(FileAge(ParamStr(0)));
  DecodeDateTime(DateCompilation, YYYY, MM, DD, HH, MN, SS, MS);
  lbGHTopoName.Caption := GetResourceString(rsGHTOPOEXENAME);
  lsbResolutionEcrans.Clear;
  for i:= 0 to Screen.MonitorCount - 1 do
  begin
    M := Screen.Monitors[i];
    W := M.Width;
    H := M.Height;
    lsbResolutionEcrans.Items.Add(_AnsiToLCLStr(Format(FMT_ECRANS_RESOL, [M.MonitorNum,W, H, BoolToStr(M.Primary, 'Primary', 'Auxiliary')])));
  end;
  lsbResolutionEcrans.ItemIndex := 0;
  //editToEncodeQRCode.Text := Trim(GetGHTopoDirectory);
  // code QR
  WU := URL_GETTING_GHTOPO;
  QRCode.Text := WU;
  // infos additionnelles
  memoAdditionnalInfos.Lines.Clear;
  memoAdditionnalInfos.Lines.Add(Format(GetResourceString(rsGHTOPOVERSION), [DD, MM, YYYY, HH, MN, SS]));
  {$IFDEF MSWINDOWS}
    WU := Format('OS: %s', ['Microsoft Windows']);
  {$ENDIF}
  {$IFDEF LINUX}
    WU := Format('OS: %s', ['Linux']);      //TODO: Obtenir la version de Linux
  {$ENDIF}
  memoAdditionnalInfos.Lines.Add(WU);
  memoAdditionnalInfos.Lines.Add(Format(GetResourceString(rsGHTOPOAUTHOR), [YYYY]));
  memoAdditionnalInfos.Lines.Add(GetResourceString(rsCOORDS_CONVERTER_AUTHOR));
  memoAdditionnalInfos.Lines.Add(GetResourceString(rsTYPE_INTERFACE_DESKTOP));

  memoAdditionnalInfos.Lines.Add('--');
  memoAdditionnalInfos.Lines.Add('Target CPU: ' + {$I %FPCTARGETCPU%});
  memoAdditionnalInfos.Lines.Add(Format('Nb cores  : %d', [GetNbCoresProcessor()]));
  memoAdditionnalInfos.Lines.Add('Target OS: ' + {$I %FPCTARGETOS%});
  memoAdditionnalInfos.Lines.Add('FPC Compiler: ' + {$I %FPCVERSION%});
  EWE := 'Compilation: ' + {$I %DATE%} + ' ' + {$I %TIME%};
  memoAdditionnalInfos.Lines.Add(EWE);
  // modèle de stockage des matrices
  memoAdditionnalInfos.Lines.Add('');
  memoAdditionnalInfos.Lines.Add('Structures de données mémoire pour le stockage des matrices:');
  {$IFDEF USE_MATRIX_DATASTRUCTURE_LIST_OF_LISTS}
  memoAdditionnalInfos.Lines.Add('Tableaux de listes (faible empreinte mémoire, mais temps de calcul élevés)');
  {$ELSE}
  memoAdditionnalInfos.Lines.Add('Tableaux de tableaux (Calcul rapide mais forte empreinte mémoire)');
  {$ENDIF}
  memoAdditionnalInfos.Lines.Add('');
  // Visualisateur OpenGL
  {$IFDEF USE_VIEWER_OPENGL}
  EWE := 'Avec visualisateur OpenGL';
  {$ELSE}
  EWE := 'Sans visualisateur OpenGL';
  {$ENDIF}
  memoAdditionnalInfos.Lines.Add(EWE);
  // Spécifique Gros Minet
  memoAdditionnalInfos.Lines.Add('');
  {$IFDEF GROS_MINET}
    memoAdditionnalInfos.Lines.Add('J''ai cru voir un Gros-Minet. Mai oui, l''utilisateur est un gRRRRos Minet !');
  {$ELSE}
    pass;
  {$ENDIF}

end;

function TfrmHelpSystem.Initialiser(const HelpFile: string; const QTopics: string; const DoDisplaySandbox: boolean): boolean;
var
  Sect: THelpFileSection;
  i : Integer;
  Idx: Integer;
begin
  // çà a le mérite d'être clair
  // Degré de christianophobie: 85
  //  70: Discrimination d'Etat ( = interdictions professionnelles, impôts confessionnels)
  //  75: Christianisme illégal et délictualisé ( = amendes et peines de prison)
  //  80: Christianisme illégal et criminalisé (= peines de prison obligatoires)

  //  95: Apostasie obligatoire sous peine de mort
  //  98: Assassinats extrajudiciaires systématiques (pendaison, égorgement, ...)
  // 100: Programme officiel d'extermination systématique

  Result := FHelpFile.LoadFromFile(HelpFile);
  PageControl1.ActivePageIndex := IIF (Trim(QTopics) = '' , 1, 0);
  if (Not Result) then Exit;
  if (FHelpFile.GetNbreSections > 0) then
  begin
    try
      cmbTitles.Clear;
      for i:=0 to FHelpFile.GetNbreSections - 1 do
      begin
        Sect := FHelpFile.GetSection(i);
        cmbTitles.Items.Add(GetResourceString(Sect.Title));
      end;
      Idx := FHelpFile.SearchIndexSection(QTopics);
      if (Idx < 0) then Idx := 0;
      cmbTitles.ItemIndex := Idx;
      PutSectionInFormByIdx(cmbTitles.ItemIndex);
      lbUtilisateurFinal.Caption := 'Utilisateur: ' + GetResourceString(DFT_OPERATEUR);
      result := True;
    except
    end;
  end;
end;


procedure TfrmHelpSystem.PutSectionInForm(const S: THelpFileSection);
var
  PS: integer;
  S1, S2: string;
begin
  self.Caption  := Format('Sect. %d: <%s>: %s', [S.Index, _AnsiToLCLStr(S.Topics), _AnsiToLCLStr(S.Title)]);
  s1:=S.Texte;
  memoText.Lines.Clear;
  PS:= Pos(#10, s1);
  memoText.Visible:=False;
  while (PS > 0) do
  begin
    s2:=Trim(Copy(s1, 1, PS-1));
    memoText.Lines.Add(s2);
    s1:=Copy(s1, PS+1, Length(s1)-PS+1);
    PS:=Pos(#10, s1);
  end;
  memoText.Lines.Add(Trim(s1));
  memoText.Visible:=True;
end;
procedure TfrmHelpSystem.PutSectionInFormByIdx(const Idx: integer);
var
  S: THelpFileSection;
begin
  S:=FHelpFile.GetSection(Idx);
  PutSectionInForm(S);
end;



end.
