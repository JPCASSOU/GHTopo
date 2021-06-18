unit BacASable;
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees, ToporobotClasses2012,  Common,
  CallDialogsStdVersion,  Classes, SysUtils, FileUtil,
  curredit, Forms, Controls, Graphics, Dialogs, StdCtrls,
  PairSplitter, ComCtrls, ExtCtrls
  // pour la déclinaison magnétique
  {$IFDEF MSWINDOWS}
    , UnitWrapperDeclimag
  {$ENDIF}

  {$IFDEF LINUX}
    , UnitWrapperDeclimagLinux
  {$ENDIF}
  {$IFDEF MACOSX}
    , UnitWrapperDeclimagMacOSX
  {$ENDIF}
  ;
type

  { TdlgBacASable }

  TdlgBacASable = class(TForm)
    btnSelectAttrTexte: TButton;
    btnDeclimag: TButton;
    btnCoordonneesIsolees: TButton;
    btnConvertTableauCoords: TButton;
    btnCalculDeclimags: TButton;
    Button15: TButton;
    Button16: TButton;
    editTheta: TCurrencyEdit;
    editPhi: TCurrencyEdit;
    editCoordX: TCurrencyEdit;
    editCoordY: TCurrencyEdit;
    editEPSG: TCurrencyEdit;
    PageControl1: TPageControl;
    TabSheet3: TTabSheet;
    procedure btnSelectAttrTexteClick(Sender: TObject);
    procedure Initialiser(const FD: TToporobotStructure2012);
  private
    { private declarations }
    FDocTopo: TToporobotStructure2012;

  public
  end;

var
  dlgBacASable: TdlgBacASable;

implementation

{$R *.lfm}

procedure TdlgBacASable.Initialiser(const FD: TToporobotStructure2012);
begin
  FDocTopo := FD;
end;

procedure TdlgBacASable.btnSelectAttrTexteClick(Sender: TObject);
var
  EWE: TTexteAttributs;
begin
  EWE.setFrom('Style01', DEFAULT_FONT_NAME, clRed, clYellow, [fsBold, fsItalic], 3.5, 74, 62);
  EWE := SelectAttributsTexte(EWE);
end;
end.
