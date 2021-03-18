unit PiloteDistoX2MainFrm;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  CadreDixtoXNew,
  Classes, SysUtils, Forms, Controls,  Dialogs, StdCtrls, ExtCtrls;

type

  { TMainFormPilote }

  TMainFormPilote = class(TForm)
    CdrDistoX2New1: TCdrDistoX2New;
    Panel1: TPanel;
    lbViseeTransmise: TStaticText;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure TraiterViseeCheminement(const V: TUneVisee);
    procedure TraiterViseeAntenne(const V: TViseeAntenne);

  public
    function Initialiser(): boolean;
    procedure Finaliser();
  end;

var
  MainFormPilote: TMainFormPilote;

implementation

{$R *.lfm}

{ TMainFormPilote }




procedure TMainFormPilote.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  self.Finaliser();
end;

procedure TMainFormPilote.FormShow(Sender: TObject);
begin
  self.Initialiser();
  CdrDistoX2New1.SetProcTransmitViseeCheminement(TraiterViseeCheminement());
  CdrDistoX2New1.SetProcTransmitViseeAntenne(TraiterViseeAntenne());

end;

procedure TMainFormPilote.TraiterViseeCheminement(const V: TUneVisee);
begin
  lbViseeTransmise.Caption := format('%s: %d.%d;  C: %d;E: %d; S: %d: %.2f, %.2f, %.2f', [
                    'Cheminement', CdrDistoX2New1.GetCurrentNumeroSerie(), CdrDistoX2New1.GetCurrentNumeroStation(),
                    V.Expe, V.Code, V.IDSecteur,
                    V.Longueur, V.Azimut, V.Pente]);
end;

procedure TMainFormPilote.TraiterViseeAntenne(const V: TViseeAntenne);
begin
  lbViseeTransmise.Caption := format('%s: %.2f, %.2f, %.2f', ['Antenne', V.Longueur, V.Azimut, V.Pente]);
end;

function TMainFormPilote.Initialiser(): boolean;
begin
  Result := CdrDistoX2New1.Initialiser();
end;

procedure TMainFormPilote.Finaliser();
begin
  CdrDistoX2New1.Finaliser();
end;
end.

