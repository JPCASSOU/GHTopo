// sélection simultanée de codes, expés et secteurs, très utile en topo de carrière
unit frmSelectSecteurCodeExpe;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  StructuresDonnees, Common, ToporobotClasses2012,
  FileUtil, curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

  { TdlgSelectSecteurCodeExpe }

  TdlgSelectSecteurCodeExpe = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    editIdxSecteur: TCurrencyEdit;
    editIdxCode: TCurrencyEdit;
    editIdxExpe: TCurrencyEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbDescCode: TStaticText;
    lbDescExpe: TStaticText;
    Panel1: TPanel;
    lbDescSecteur: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { private declarations }
    FDoUseLastItems: boolean;
    FMyDocTopo   : TToporobotStructure2012;
    procedure SetInfosSecteur(const Idx: integer);
    procedure SetInfosCode(const Idx: TNumeroCode);
    procedure SetInfosExpe(const Idx: TNumeroExpe);
  public
    { public declarations }
    function Initialiser(const FD: TToporobotStructure2012;
                         const MyRow: integer;
                         const IdxSecteur: integer;
                         const IdxCode: TNumeroCode;
                         const IdxExpe: TNumeroExpe;
                         const DoUseLastItems: boolean): boolean;
    function GetIdxSecteur(): integer;
    function GetIdxCode(): TNumeroCode;
    function GetIdxExpe(): TNumeroExpe;
  end;

var
  dlgSelectSecteurCodeExpe: TdlgSelectSecteurCodeExpe;

implementation
uses
  CallDialogsStdVersion;


{$R *.lfm}

{ TdlgSelectSecteurCodeExpe }

procedure TdlgSelectSecteurCodeExpe.Button1Click(Sender: TObject);
var
  QSelectedIndex: integer;
begin
  QSelectedIndex := editIdxSecteur.AsInteger;
  if (SelectionDansListe(FMyDocTopo, mslSECTEURS, True, QSelectedIndex)) then SetInfosSecteur(QSelectedIndex);
end;

procedure TdlgSelectSecteurCodeExpe.Button2Click(Sender: TObject);
var
  QSelectedIndex: LongInt;
begin
  QSelectedIndex := editIdxCode.AsInteger;
  if (SelectionDansListe(FMyDocTopo, mslCODE, false, QSelectedIndex)) then SetInfosCode(QSelectedIndex);
end;

procedure TdlgSelectSecteurCodeExpe.Button3Click(Sender: TObject);
var
  QSelectedIndex: LongInt;
begin
  QSelectedIndex := editIdxExpe.AsInteger;
  if (SelectionDansListe(FMyDocTopo, mslEXPE, false, QSelectedIndex)) then SetInfosExpe(QSelectedIndex);
end;

function TdlgSelectSecteurCodeExpe.GetIdxCode(): TNumeroCode;
begin
  Result := editIdxCode.AsInteger;
end;

function TdlgSelectSecteurCodeExpe.GetIdxExpe(): TNumeroExpe;
begin
  Result := editIdxExpe.AsInteger;
end;

function TdlgSelectSecteurCodeExpe.GetIdxSecteur: integer;
begin
  Result := editIdxSecteur.AsInteger;
end;

function TdlgSelectSecteurCodeExpe.Initialiser(const FD: TToporobotStructure2012;
                                               const MyRow: integer;
                                               const IdxSecteur: integer;
                                               const IdxCode: TNumeroCode;
                                               const IdxExpe: TNumeroExpe;
                                               const DoUseLastItems: boolean): boolean;
begin
  result     := false;
  self.Caption := Format('Recopier secteur, code et seance depuis station %d', [MyRow]);
  FDoUseLastItems := DoUseLastItems;
  FMyDocTopo := FD;
  SetInfosSecteur(IdxSecteur);
  SetInfosCode(IdxCode);
  SetInfosExpe(IdxExpe);
  result     := True;
end;


procedure TdlgSelectSecteurCodeExpe.SetInfosCode(const Idx: TNumeroCode);
var
  EWE: TCode;
  QIdx: TNumeroCode;
  n: Integer;
begin
  if (FDoUseLastItems) then
  begin
    n := FMyDocTopo.GetNbCodes() - 1;
    EWE := FMyDocTopo.GetCode(n);
    QIdx := EWE.IDCode;
  end
  else
    QIdx := Idx;
  editIdxCode.AsInteger := QIdx;
  EWE := FMyDocTopo.GetCodeByNumero(QIdx);
  lbDescCode.Caption := Format('%.0f, %.0f - %s', [EWE.GradAz, EWE.GradInc, EWE.Commentaire]);
end;
procedure TdlgSelectSecteurCodeExpe.SetInfosExpe(const Idx: TNumeroExpe);
var
  EWE: TExpe;
  n: Integer;
  QIdx: TNumeroExpe;
begin
  if (FDoUseLastItems) then
  begin
    n := FMyDocTopo.GetNbExpes() - 1;
    EWE := FMyDocTopo.GetExpe(n);
    QIdx := EWE.IDExpe;
  end
  else
    QIdx := Idx;
  editIdxExpe.AsInteger := QIdx;
  EWE := FMyDocTopo.GetExpeByNumero(QIdx);
  {$WARNING: TEXpe.DateExpe à implementer}
  lbDescExpe.Caption := Format('%.2d/%.2d/%.4d - %s', [EWE.JourExpe, EWE.MoisExpe, EWE.AnneeExpe, EWE.Commentaire]);
end;

procedure TdlgSelectSecteurCodeExpe.SetInfosSecteur(const Idx: integer);
var
  EWE: TSecteur;
  QIdx: Integer;
begin
  if (FDoUseLastItems) then QIdx := FMyDocTopo.GetNbSecteurs() - 1
                       else QIdx := Idx;
  editIdxSecteur.AsInteger := QIdx;
  EWE := FMyDocTopo.GetSecteur(QIdx);
  lbDescSecteur.Caption := EWE.NomSecteur;
end;

end.

