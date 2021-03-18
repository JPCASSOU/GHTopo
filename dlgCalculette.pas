// 25/11/2013: Déplacement du code de la calculette dans un cadre CadreCalculette.pas
unit dlgCalculette;
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  StructuresDonnees,
  CadreCalculette,
  {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
  UnitClasseMaillage,
  ToporobotClasses2012,
  UnitEntitesExtended,
  FastGEO,
  {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, SynHighlighterPas, Types;
type  { TfrmCalculette }
  TfrmCalculette = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CdrCalculette1: TCdrCalculette;
    Panel1: TPanel;
    lbDocTopoName: TStaticText;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    FMyTabIndex: integer;
    {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    FDocuTopo: TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FMaillage: TMaillage;
    FLassoDeSelection: TGeoPolygon2D;
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}

  public
    { public declarations }
    procedure SetDefaultExpression(const S: string);
    procedure SetCoordonnees(const P: TPoint2Df);
    function GetCoordonnees(): TPoint2Df;
    function GetDeclimag(): double;
    function GetResultatCalcul(): double;
    function GetCodeEPSG(): TLabelSystemesCoordsEPSG;
    procedure SetTabIndex(const Idx: integer);
    {$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
    procedure SetDocuTopo(const FD: TToporobotStructure2012; const BDE: TBDDEntites; const MM: TMaillage; const L: TGeoPolygon2D);
    {$endif CALCULETTE_EMBEDDED_IN_GHTOPO}

  end;

var
  frmCalculette: TfrmCalculette;

implementation
uses
  CallDialogsStdVersion;

{$R *.lfm}

{ TfrmCalculette }
procedure TfrmCalculette.SetDefaultExpression(const S: string);
begin
  CdrCalculette1.SetDefaultExpressionA(S);
end;

procedure TfrmCalculette.SetTabIndex(const Idx: integer);
begin
  FMyTabIndex := Idx;
end;
{$ifdef CALCULETTE_EMBEDDED_IN_GHTOPO}
procedure TfrmCalculette.SetDocuTopo(const FD: TToporobotStructure2012; const BDE: TBDDEntites; const MM: TMaillage; const L: TGeoPolygon2D);
begin
  FDocuTopo   := FD;
  FBDDEntites := BDE;
  FMaillage   := MM;
  FLassoDeSelection := L;
end;
{$endif CALCULETTE_EMBEDDED_IN_GHTOPO}
procedure TfrmCalculette.SetCoordonnees(const P: TPoint2Df);
begin
  CdrCalculette1.SetCoordonneesA(P);
end;
function TfrmCalculette.GetResultatCalcul(): double;
begin
  Result := CdrCalculette1.GetResultatCalculA();
end;
function TfrmCalculette.GetDeclimag(): double;
begin
  Result := CdrCalculette1.GetDeclimagA();
end;

function TfrmCalculette.GetCoordonnees(): TPoint2Df;
begin
  Result := CdrCalculette1.GetCoordonneesA();
end;

procedure TfrmCalculette.FormShow(Sender: TObject);
var
  MH, MV: Integer;
begin
  MH := 40;
  MV := 40;
  self.Top := MV;
  self.Left:= MH;
  self.Width    := Screen.Width  - 2 * MH;
  self.Height   := Screen.Height - 5 * MV;
  self.Position := poScreenCenter;
  self.Caption := GetResourceString(rsDLG_CALC_TITLE);
  {$IFDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
  if (Not CdrCalculette1.Initialiser(FMyTabIndex, FDocuTopo, FBDDEntites, FMaillage)) then
  begin
    ShowMessage('Calculette inoperante');
    self.Close;
  end;
  lbDocTopoName.Caption := IIF(Assigned(FDocuTopo), 'Classe du document: ' + FDocuTopo.ClassName, '---');
  {$ELSE CALCULETTE_EMBEDDED_IN_GHTOPO}
  if (Not CdrCalculette1.Initialiser(0)) then
  begin
    ShowMessage('Calculette inoperante');
    self.Close;
  end;
  lbDocTopoName.Caption := '---';
  {$ENDIF CALCULETTE_EMBEDDED_IN_GHTOPO}
end;

function TfrmCalculette.GetCodeEPSG(): TLabelSystemesCoordsEPSG;
begin
  Result := CdrCalculette1.GetCurrentCodeEPSGNomSysteme();
end;



procedure TfrmCalculette.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    CdrCalculette1.Finaliser;
  finally
  end;
end;



procedure TfrmCalculette.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  if (GHTopoQuestionOuiNon('Fermer')) then CanClose := True;
end;





end.

