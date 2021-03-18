unit frmDistoXUtils;
{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  //windows,
  StructuresDonnees,
  Common,
  CallDialogsStdVersion,
  //ToporobotClasses2012,
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls;

type

  { TdlgDistoXUtils }

  TdlgDistoXUtils = class(TForm)
    btnClose: TBitBtn;
    Panel1: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);

  private
    { private declarations }
  public
    { public declarations }
    function Initialiser(): boolean;

  end;

var
  dlgDistoXUtils: TdlgDistoXUtils;

implementation

{$R *.lfm}

{ TdlgDistoXUtils }




procedure TdlgDistoXUtils.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CdrDistoX1.Finaliser();
end;

procedure TdlgDistoXUtils.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  if (GHTopoQuestionOuiNon('Le DistoX est en service - Quitter')) then CanClose := True;

end;

procedure TdlgDistoXUtils.btnCloseClick(Sender: TObject);
begin

end;



function TdlgDistoXUtils.Initialiser(): boolean;
begin
  result := CdrDistoX1.InitialiserCadreDistoX();
end;


end.

