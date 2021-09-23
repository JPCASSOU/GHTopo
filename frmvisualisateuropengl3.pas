unit frmVisualisateurOpenGL3;
// Nouveau viewer OpenGL
{$INCLUDE CompilationParameters.inc}

interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  UnitEntitesExtended, UnitClasseMaillage, CadreDGCDrawingContext, CadreOpenGL3,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;


type

  { TdlgVisualisateurOpenGL3 }

  TdlgVisualisateurOpenGL3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    btnBackColor: TColorButton;
    CdrOpenGL3_1: TCdrOpenGL3;
    cmbTypePerspective: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    sclZoom: TScrollBar;
    sclTheta: TScrollBar;
    sclPhi: TScrollBar;
    sclFectMagnificationZ: TScrollBar;
    procedure btnBackColorColorChanged(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cmbTypePerspectiveChange(Sender: TObject);
    procedure sclFectMagnificationZChange(Sender: TObject);
    procedure sclPhiChange(Sender: TObject);
    procedure sclThetaChange(Sender: TObject);
    procedure sclZoomChange(Sender: TObject);
  private
    FBDDEntites: TBDDEntites;
    FMaillage  : TMaillage;
    FFiltres   : string;
    procedure DispDebugMsg(const Msg: string);
  public
    function Initialiser(const QBDDEntites: TBDDEntites;
                         const QMaillage: TMaillage;
                         const QFiltres: string): boolean;
  end;

var
  dlgVisualisateurOpenGL3: TdlgVisualisateurOpenGL3;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TdlgVisualisateurOpenGL3 }


procedure TdlgVisualisateurOpenGL3.btnBackColorColorChanged(Sender: TObject);
begin
  CdrOpenGL3_1.Background.SetRGBA(btnBackColor.ButtonColor);
  //CdrOpenGL3_1.ConstruireScene();
end;



procedure TdlgVisualisateurOpenGL3.Button1Click(Sender: TObject);
var
  n: Integer;
begin
  CdrOpenGL3_1.SetSceneBounds(-5.0, -5.0, -5.0, 5.0, 5.0, 3.0);
  CdrOpenGL3_1.ClearAllListesAffichage();
  CdrOpenGL3_1.CreateANewListeAffichage('ORIGINE'  , 'Marqueur oriogine des coordonnées');
  CdrOpenGL3_1.CreateANewListeAffichage('CUBE'     , 'Cube enveloppe du réseau');
  CdrOpenGL3_1.CreateANewListeAffichage('TRIANGLES', 'Quelques triangles');


  CdrOpenGL3_1.BeginScene();
    // le cube
    CdrOpenGL3_1.BeginListeAffichage('ORIGINE');
      CdrOpenGL3_1.MakeRepereOrigine(0.50);
    CdrOpenGL3_1.EndListeAffichage();
    CdrOpenGL3_1.BeginListeAffichage('CUBE');
      CdrOpenGL3_1.MakeBoundingBoxScene();
    CdrOpenGL3_1.EndListeAffichage();
    // Les objets
    CdrOpenGL3_1.BeginListeAffichage('TRIANGLES');
      CdrOpenGL3_1.BeginSetOfTriangles();
        CdrOpenGL3_1.AddTriangle(-0.5, -0.5, 0.0,
                                  0.6, -0.4, 0.0,
                                  0.1, 0.7, 0.0);
        CdrOpenGL3_1.AddTriangle(0.7, 0.7, 0.0,
                                  -0.3, -0.4, 0.0,
                                  -0.2, -0.7, 0.0);


      CdrOpenGL3_1.EndSetOfTriangles();
    CdrOpenGL3_1.EndListeAffichage();


  CdrOpenGL3_1.EndScene();

end;

procedure TdlgVisualisateurOpenGL3.Button2Click(Sender: TObject);
begin
  CdrOpenGL3_1.MiniAnimation();
end;

procedure TdlgVisualisateurOpenGL3.cmbTypePerspectiveChange(Sender: TObject);
begin
  CdrOpenGL3_1.SetTypePerspective(TTypePerspective(cmbTypePerspective.ItemIndex));
end;

procedure TdlgVisualisateurOpenGL3.sclFectMagnificationZChange(Sender: TObject);
begin
  CdrOpenGL3_1.SetMagnificationZ(sclFectMagnificationZ.Position / 100.0);
end;

procedure TdlgVisualisateurOpenGL3.sclPhiChange(Sender: TObject);
begin
  CdrOpenGL3_1.SetPhi(sclPhi.Position);
end;

procedure TdlgVisualisateurOpenGL3.sclThetaChange(Sender: TObject);
begin
  CdrOpenGL3_1.SetTheta(sclTheta.Position);
end;

procedure TdlgVisualisateurOpenGL3.sclZoomChange(Sender: TObject);
begin
  CdrOpenGL3_1.SetZoom(sclZoom.Position / 100.0);

end;

procedure TdlgVisualisateurOpenGL3.DispDebugMsg(const Msg: string);
begin
  AfficherMessageErreur(Msg);
end;

function TdlgVisualisateurOpenGL3.Initialiser(const QBDDEntites: TBDDEntites; const QMaillage: TMaillage; const QFiltres: string): boolean;
begin
  result := false;
  FBDDEntites := QBDDEntites;
  FMaillage   := QMaillage;
  FFiltres    := QFiltres;

  result := CdrOpenGL3_1.Initialiser(DispDebugMsg);
  CdrOpenGL3_1.Background.SetRGBA(btnBackColor.ButtonColor);
end;

end.

