unit frmLesListesSimples;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Classes, SysUtils,
  Common,
  ToporobotClasses2012,
  UnitEntitesExtended,
  UnitClasseMaillage,
  CadreListesSimples,
  FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

{ TfrmListesSimples }

 TfrmListesSimples = class(TForm)
    CdrListesSimples1: TCdrListesSimples;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
    FDocuTopo  : TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FMaillage  : TMaillage;
  public
    { public declarations }
    function  Initialiser(const B: TToporobotStructure2012;
                          const E: TBDDEntites;
                          const M: TMaillage;
                          const CallbackGotoErrorSerie      : TCallbackGotoErrorSerie;
                          const CallbackGotoProximiteSurPlan: TProcOfObjectWithBaseStation;
                          const CallbackGotoXYSurPlan       : TProcOfObjectWithXY): boolean;
    procedure Finaliser();
    procedure SetModeBDD(const M: TModeBDD);
  end;

var
  frmListesSimples: TfrmListesSimples;

implementation

{$R *.lfm}

{ TfrmListesSimples }
procedure TfrmListesSimples.Finaliser();
begin
  CdrListesSimples1.Finaliser();
end;

procedure TfrmListesSimples.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
end;



function TfrmListesSimples.Initialiser(const B: TToporobotStructure2012;
                                       const E: TBDDEntites;
                                       const M: TMaillage;
                                       const CallbackGotoErrorSerie      : TCallbackGotoErrorSerie;
                                       const CallbackGotoProximiteSurPlan: TProcOfObjectWithBaseStation;
                                       const CallbackGotoXYSurPlan       : TProcOfObjectWithXY): boolean;
begin
  result := False;
  self.Caption := GetResourceString(rsWND_LISTES_SIMPLES);
  FDocuTopo   := B;
  FBDDEntites := E;
  FMaillage   := M;
  CdrListesSimples1.InitialiserListeSimple(FDocuTopo, FBDDEntites, FMaillage, mbddENTRANCES);
  CdrListesSimples1.SetProcOfObjectWithBaseStation(CallbackGotoProximiteSurPlan);
  CdrListesSimples1.SetProcOfObjectWithXY(CallbackGotoXYSurPlan);
  result      := True;

end;

procedure TfrmListesSimples.SetModeBDD(const M: TModeBDD);
begin
  CdrListesSimples1.ListerListesSimples(M);
end;
(*
procedure TfrmListesSimples.ActualiserBDDEntites(const E: TBDDEntites);
begin
  CdrListesSimples1.SetPtrBDDEntites(E);

end;
//*)

(* Echelles de christianophobie

 70: Pratique d'un culte fortement déconseillée, brimades incessantes, internements abusifs, interdictions
     (Chine)

*76: Christianophobie d'Etat. Impôt confessionnel dont seuls les athées sont exonérés.
     Brimades, discriminations sociales (fonction publique réservée aux athées),
     internements abusifs de prêtres
 80: Interdiction des religions, internement des membres du clergé, discriminations sociales
     (Enver HOXDA)
 90: Extermination du clergé, apostasie forcée sous peine de mort, interdiction totale,
     destruction de lieux de cultes
     (modèle de christianophobie SS prôné par Martin BORMANN, programme démarré en 1941
     avec l'assassinat de 2579 prêtres polonais, dont 276 par égorgement ou pendaison)
100: Extermination systématique des chrétiens: hommes, femmes et enfants
     (Néron, Peuls)

//*)






end.

