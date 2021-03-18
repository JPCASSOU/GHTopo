unit frmProfilTopo;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  UnitClasseMaillage,
  unitProfilTopo,
  CadreProfilsTopo,
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls;

type

  { TdlgProfilTopo }

  TdlgProfilTopo = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Button1: TButton;
    CdrProfilTopo1: TCdrProfilTopo;
    Panel1: TPanel;

  private
    FMaillage  : TMaillage;
    FProfilTopo: TProfilTopo;

  public
    function Initialiser(const DT: TToporobotStructure2012; const QMaillage: TMaillage; var ProfilTopo: TProfilTopo): boolean;
    function  GetProfilTopo(): TProfilTopo;
    procedure Finaliser();
  end;

var
  dlgProfilTopo: TdlgProfilTopo;

implementation

{$R *.lfm}

{ TdlgProfilTopo }



function TdlgProfilTopo.Initialiser(const DT: TToporobotStructure2012; const QMaillage: TMaillage; var ProfilTopo: TProfilTopo): boolean;
begin
  Result := CdrProfilTopo1.Initialiser(DT, QMaillage, ProfilTopo);
end;

function TdlgProfilTopo.GetProfilTopo(): TProfilTopo;
begin
  Result := CdrProfilTopo1.GetProfilTopo();
end;

procedure TdlgProfilTopo.Finaliser();
begin
  CdrProfilTopo1.Finaliser();
end;
end.

