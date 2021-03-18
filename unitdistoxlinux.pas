unit unitDistoXLinux;

{$mode delphi}
// 11/04/2015: Adaptation pour Linux : OK
// TODO: Implémentation des fonctions à faire

interface

uses
  Classes, SysUtils, Math,
  StructuresDonnees,
  Common,
  //ComPort,
  StdCtrls,
  ExtCtrls, // pour Timer
  CalculMatriciel3x3,
  Grids,
  Forms;
const
  // axes
  mrX_AXIS = 1;
  mrY_AXIS = 2;
  mrZ_AXIS = 3;
  // mappage de la mémoire du DistoX
  ADDR_START_SECTION_CONFIGURATION   = $B000; // calibration coefficients, Ox00B000 -> Ox00B3FF
  ADDR_START_SECTION_DATA_STORE      = $B400; // data segment            , Ox00B400 -> Ox00FFFF


type

{ TPilotageDistoX }
 // /!\ CECI EST UN SQUELETTE

 TPilotageDistoX = class
  private
    // composant LazSerial
    FCommPortOpened     : Boolean;
    FCalibrationModeON  : boolean;
    FLaserON            : boolean;
    FIsReadingTopoData  : boolean;
    FNbMesuresVisees      : integer;
    FNbMesuresCalibration : integer;
    // controles d'affichages
    FLabelActionsProcessing: TStaticText;
    FGridDataViseesDistoX : TStringGrid;
    FGridDataGMDistoX     : TStringGrid;
    FTimer                : TTimer;
    oldtype, oldX, oldY, oldZ: Integer;
    // listes des mesures
    FListeMesuresTopo     : TList;
    // outils de calibration (fusion de TPilotageDistoX)
    FListeMesuresDeG: TList;
    FListeMesuresDeM: TList;
    fMatrix_aG  : TMatrix3x3;
    fMatrix_aM  : TMatrix3x3;
    fVecteur_bG : TPoint3Df;
    fVecteur_bM : TPoint3Df;
    fDelta      : double;
    fNbIters    : integer;
    // callback de transmission de visée
    fProcTransmitMesureVisee: TProcTransmitMesureVisee; //procedure (var V: TMesureViseeDistoX) of object;
    FProcAfficherProgression: TProcDisplayProgression;
    // calibration

    function Acknowledge(const B: byte): boolean;
    procedure CheckOverflow(var M: TMatrix3x3; var V: TPoint3Df);
    procedure OptVectors(const gr, mr: TPoint3Df; const alpha: double; out gx, mx: TPoint3Df);
    function  LireEtDepilerTrameDe8OctetsData: boolean;
    procedure setJeuDeDonneesExemple;
    procedure TurnVectors(const gxp, mxp, gr, mr: TPoint3Df; out gx, mx: TPoint3Df);
    procedure ViderListesMesuresGM;

    //-------------------------------------
    procedure ProcTimer(Sender: TObject);
    procedure SetHeaderGrdGM;
    function  getEmptyMesure(const QTypeMesure: TTypeMesureDistoX): TMesureViseeDistoX;
  public
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLazSerialDevice(const D: string);
    function  OpenLazSerial: boolean;
    procedure CloseLazSerial;
    procedure ExtinctDevice;
    procedure TrigMesure;
    // callback de transmission de valeur de mesure de visées
    procedure SetProcTransmitMesureVisee(const P: TProcTransmitMesureVisee);
    procedure SetProcProgression(const P: TProcDisplayProgression);
    // widgets utilisés par TPilotageDistoX
    procedure SetOutputDataViseesGrid(const G: TStringGrid);
    procedure SetOutputDataGMGrid(const G: TStringGrid);
    procedure SetLabelActionsProcessing(const L: TStaticText);
    function  IsReady: boolean;
    procedure SetCalibrationOn(const o: boolean);
    procedure SetLaserOn(const o: boolean);
    // lecture données topo
    procedure SetReadingTopoData(const B: boolean);
    // statuts
    function  GetStatutLaser      : boolean;
    function  GetStatutCalibration: boolean;
    function  GetStatutReadingData: boolean;
    // table de mesures de visée
    procedure AddMesureVisee(const V : TMesureViseeDistoX);
    procedure RemoveMesureVisee(const Idx: integer);
    procedure RemoveLastMesureVisee;
    function  GetMesureVisee(const Idx: integer): TMesureViseeDistoX;
    function  GetLastMesureVisee: TMesureViseeDistoX;
    procedure PutMesureVisee(const Idx: integer; const V: TMesureViseeDistoX);
    function  GetNbViseesAcquises: integer;
    // calibration
    function  Check: boolean;
    function GetDataGMFromGrid(const grd: TStringGrid): boolean;
    function SetDataGMInGrid(const grd: TStringGrid): boolean;

    procedure AddMesureG(const MG: TPoint3Df);
    procedure AddMesureM(const MG: TPoint3Df);
    procedure AddMesuresGMByValues(const gx, gy, gz, mx, my, mz: double);
    function  GetMesureG(const Idx: integer): TPoint3Df;
    function  GetMesureM(const Idx: integer): TPoint3Df;

    function  GetNbMesuresG: integer;
    function  GetNbMesuresM: integer;

    function  IsReadyForCalibrate: boolean;

    procedure Optimise;

    function GetMatrix_aG: TMatrix3x3;
    function GetMatrix_aM  : TMatrix3x3;
    function GetVecteur_bG : TPoint3Df;
    function GetVecteur_bM : TPoint3Df;
    function GetDelta      : double;
    function GetNbIters    : integer;
    // lire paramètres de calibration depuis le DistoX
    procedure LireLesParametresDeCalibDepuisDistoX(var BytesRead: TMemoryMapDistoX);
    // envoi des paramètres au DistoX
    function SendCalibrationParametersToDistoX: integer;
    // lire un paquet de 4 octets dans la mémoire du DistoX
    function ReadPacketFromAddress(const Address: word): TPacketOfBytes;
    // dump de la zone de données
    function DumpDataSection(const AddressStart: word; const NbMots: integer): integer;
    // vider liste des mesures topo
    procedure ViderListeMesuresTopo;
    // charger données brutes depuis un fichier
    procedure ChargerDonneesBrutes(const Filename: string);
    procedure SauverDonneesBrutes(const Filename: string);

    // afficher les données dans une liste
    procedure afficherViseesDansGrille(const GRD: TStringGrid);

end;

implementation

{ TPilotageDistoX }

function TPilotageDistoX.Acknowledge(const B: byte): boolean;
begin

end;

procedure TPilotageDistoX.CheckOverflow(var M: TMatrix3x3; var V: TPoint3Df);
begin

end;

procedure TPilotageDistoX.OptVectors(const gr, mr: TPoint3Df;
  const alpha: double; out gx, mx: TPoint3Df);
begin

end;

function TPilotageDistoX.LireEtDepilerTrameDe8OctetsData: boolean;
begin

end;

procedure TPilotageDistoX.setJeuDeDonneesExemple;
begin

end;

procedure TPilotageDistoX.TurnVectors(const gxp, mxp, gr, mr: TPoint3Df; out gx, mx: TPoint3Df);
begin

end;

procedure TPilotageDistoX.ViderListesMesuresGM;
begin

end;

procedure TPilotageDistoX.ProcTimer(Sender: TObject);
begin

end;

procedure TPilotageDistoX.SetHeaderGrdGM;
begin

end;

function TPilotageDistoX.getEmptyMesure(const QTypeMesure: TTypeMesureDistoX
  ): TMesureViseeDistoX;
begin

end;

//constructor TPilotageDistoX.Create(AOwner: TComponent);
//begin
//  inherited Create(AOwner);
//end;

destructor TPilotageDistoX.Destroy;
begin
  inherited Destroy;
end;

procedure TPilotageDistoX.SetLazSerialDevice(const D: string);
begin

end;

function TPilotageDistoX.OpenLazSerial: boolean;
begin

end;

procedure TPilotageDistoX.CloseLazSerial;
begin

end;

procedure TPilotageDistoX.ExtinctDevice;
begin

end;

procedure TPilotageDistoX.TrigMesure;
begin

end;

procedure TPilotageDistoX.SetProcTransmitMesureVisee(const P: TProcTransmitMesureVisee);
begin
  fProcTransmitMesureVisee := P;
end;

procedure TPilotageDistoX.SetProcProgression(const P: TProcDisplayProgression);
begin
  FProcAfficherProgression := P;
end;

procedure TPilotageDistoX.SetOutputDataViseesGrid(const G: TStringGrid);
begin
  FGridDataGMDistoX := G;
end;

procedure TPilotageDistoX.SetOutputDataGMGrid(const G: TStringGrid);
begin

end;

procedure TPilotageDistoX.SetLabelActionsProcessing(const L: TStaticText);
begin
  FLabelActionsProcessing := L;
end;

function TPilotageDistoX.IsReady: boolean;
begin

end;

procedure TPilotageDistoX.SetCalibrationOn(const o: boolean);
begin

end;

procedure TPilotageDistoX.SetLaserOn(const o: boolean);
begin

end;

procedure TPilotageDistoX.SetReadingTopoData(const B: boolean);
begin

end;

function TPilotageDistoX.GetStatutLaser: boolean;
begin

end;

function TPilotageDistoX.GetStatutCalibration: boolean;
begin

end;

function TPilotageDistoX.GetStatutReadingData: boolean;
begin

end;

procedure TPilotageDistoX.AddMesureVisee(const V: TMesureViseeDistoX);
begin

end;

procedure TPilotageDistoX.RemoveMesureVisee(const Idx: integer);
begin

end;

procedure TPilotageDistoX.RemoveLastMesureVisee;
begin

end;

function TPilotageDistoX.GetMesureVisee(const Idx: integer): TMesureViseeDistoX;
begin

end;

function TPilotageDistoX.GetLastMesureVisee: TMesureViseeDistoX;
begin

end;

procedure TPilotageDistoX.PutMesureVisee(const Idx: integer;
  const V: TMesureViseeDistoX);
begin

end;

function TPilotageDistoX.GetNbViseesAcquises: integer;
begin

end;

function TPilotageDistoX.Check: boolean;
begin

end;

function TPilotageDistoX.GetDataGMFromGrid(const grd: TStringGrid): boolean;
begin

end;

function TPilotageDistoX.SetDataGMInGrid(const grd: TStringGrid): boolean;
begin

end;

procedure TPilotageDistoX.AddMesureG(const MG: TPoint3Df);
begin

end;

procedure TPilotageDistoX.AddMesureM(const MG: TPoint3Df);
begin

end;

procedure TPilotageDistoX.AddMesuresGMByValues(const gx, gy, gz, mx, my,
  mz: double);
begin

end;

function TPilotageDistoX.GetMesureG(const Idx: integer): TPoint3Df;
begin

end;

function TPilotageDistoX.GetMesureM(const Idx: integer): TPoint3Df;
begin

end;

function TPilotageDistoX.GetNbMesuresG: integer;
begin

end;

function TPilotageDistoX.GetNbMesuresM: integer;
begin

end;

function TPilotageDistoX.IsReadyForCalibrate: boolean;
begin

end;

procedure TPilotageDistoX.Optimise;
begin

end;

function TPilotageDistoX.GetMatrix_aG: TMatrix3x3;
begin

end;

function TPilotageDistoX.GetMatrix_aM: TMatrix3x3;
begin

end;

function TPilotageDistoX.GetVecteur_bG: TPoint3Df;
begin

end;

function TPilotageDistoX.GetVecteur_bM: TPoint3Df;
begin

end;

function TPilotageDistoX.GetDelta: double;
begin

end;

function TPilotageDistoX.GetNbIters: integer;
begin

end;

procedure TPilotageDistoX.LireLesParametresDeCalibDepuisDistoX(
  var BytesRead: TMemoryMapDistoX);
begin

end;

function TPilotageDistoX.SendCalibrationParametersToDistoX: integer;
begin

end;

function TPilotageDistoX.ReadPacketFromAddress(const Address: word
  ): TPacketOfBytes;
begin

end;

function TPilotageDistoX.DumpDataSection(const AddressStart: word; const NbMots: integer): integer;
begin

end;

procedure TPilotageDistoX.ViderListeMesuresTopo;
begin

end;

procedure TPilotageDistoX.ChargerDonneesBrutes(const Filename: string);
begin

end;

procedure TPilotageDistoX.SauverDonneesBrutes(const Filename: string);
begin

end;

procedure TPilotageDistoX.afficherViseesDansGrille(const GRD: TStringGrid);
begin

end;

end.
