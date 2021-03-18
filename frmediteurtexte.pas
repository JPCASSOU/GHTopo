unit frmEditeurTexte;

{$mode delphi}

interface

uses
  GHCD_Types,
  Classes, SysUtils, FileUtil, SynHighlighterAny, SynEdit, SynMemo, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TdlgEditeurTexte }

  TdlgEditeurTexte = class(TForm)
    BitBtn1: TBitBtn;
    memoTextes: TMemo;
  private
    { private declarations }
    FModeSelectionEntites: TModeSelectionEntites;
  public
    { public declarations }
    procedure LoadFile(const QFileName: string);
    procedure SetText(const LS: TStringList);
    procedure SetModeSelectionEntites(const QM: TModeSelectionEntites);
  end;

var
  dlgEditeurTexte: TdlgEditeurTexte;

implementation

{$R *.lfm}

{ TdlgEditeurTexte }

procedure TdlgEditeurTexte.LoadFile(const QFileName: string);
begin
  memoTextes.Lines.LoadFromFile(QFileName);
end;

procedure TdlgEditeurTexte.SetModeSelectionEntites(const QM: TModeSelectionEntites);
begin
  FModeSelectionEntites := QM;
end;

procedure TdlgEditeurTexte.SetText(const LS: TStringList);
begin
  memoTextes.Lines := LS;
end;

end.
