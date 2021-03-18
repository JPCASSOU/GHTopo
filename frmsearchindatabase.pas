unit frmSearchInDatabase;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  UnitEntitesExtended,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons;

type

  { TdlgSearchInDatabase }

  TdlgSearchInDatabase = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnFind: TButton;
    editFindWhat: TEdit;
    lbFindWhat: TLabel;
    lbResultatsRecherche: TLabel;
    lsbResults: TListBox;
    Panel1: TPanel;
    pnlFindWhat: TPanel;
    procedure btnFindClick(Sender: TObject);
    procedure editFindWhatChange(Sender: TObject);
  private
    FDocumentToporobot        : TToporobotStructure2012;
    FBDDEntites               : TBDDEntites;
    procedure Rechercher();
  public
    function Initialiser(const FD: TToporobotStructure2012; const FBDD: TBDDEntites; const QFindWhat: string): boolean;
  end;

var
  dlgSearchInDatabase: TdlgSearchInDatabase;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

{ TdlgSearchInDatabase }

procedure TdlgSearchInDatabase.editFindWhatChange(Sender: TObject);
begin

end;

procedure TdlgSearchInDatabase.Rechercher();
var
  QResults: TArrayOfTToporobotIDStation;
  i, Nb: Integer;
  EWE: TToporobotIDStation;
begin
  SetLength(QResults, 0);
  lsbResults.Clear;
  FDocumentToporobot.SearchTextInTableSeriesStations(editFindWhat.Text, QResults);
  Nb := length(QResults);
  lbResultatsRecherche.Caption := format(GetResourceString(rsDLG_SEARCH_IN_DATABASES_LB_RESULTS), [Nb]);
  if (Nb = 0) then exit;
  for i := 0 to Nb-1 do
  begin
    EWE := QResults[i];
    lsbResults.Items.add(format('%d.%d:  %s', [EWE.aSerie, EWE.aStation, EWE.aIDTerrain]));
  end;
  lsbResults.ItemIndex := 0;

end;

procedure TdlgSearchInDatabase.btnFindClick(Sender: TObject);
begin
  Rechercher();
end;

function TdlgSearchInDatabase.Initialiser(const FD: TToporobotStructure2012; const FBDD: TBDDEntites; const QFindWhat: string): boolean;
begin
  result := false;
  FDocumentToporobot   := FD;
  FBDDEntites          := FBDD;
  self.Caption                 := GetResourceString(rsDLG_SEARCH_IN_DATABASES_TITLE);
  lbResultatsRecherche.Caption := Format(GetResourceString(rsDLG_SEARCH_IN_DATABASES_LB_RESULTS), [0]);
  lbFindWhat.Caption           := GetResourceString(rsDLG_SEARCH_IN_DATABASES_EDIT_FINDWHAT);
  btnFind.Caption              := GetResourceString(rsDLG_SEARCH_IN_DATABASES_BTN_FINDWHAT);
  editFindWhat.Text            := QFindWhat;
  result := true;
end;

end.

