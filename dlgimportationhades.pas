unit dlgImportationHADES;
// Pour Récupération de vieux dossiers HADES Classic uniquement.

{$mode delphi}
{$PACKRECORDS 1}
interface

uses
  Common,
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls, ShellCtrls, ComCtrls, FileCtrl;

// Anciennes structures de données HADES
// structure des cheminements
type TParamCheminements = record
   CodeChem             : array [0..4] of char;   // Cheminement de branchement
   Sub_System           : Smallint;      // Sous-système: cavité, grand réseau, etc ...
   NomChem              : array[0..79] of char;   // Nom du cheminement
   DateCreation         : array[0..7]  of char;   // Date du relevé
   Operateurs           : array[0..79] of char;   // Opérateurs
   //Commentaire          : array[0..254] of char;  // Commentaire
   Commentaire          : array[0..222] of char;  // Commentaire
   IDLittNdDep          : array[0..15] of char;
   IDLittNdArr          : array[0..15] of char;

   GroupeColor_R        : byte;      // Mois de dernier calcul du dossier
   GroupeColor_V        : byte;
   NoeudDepart          : Smallint;      // Table d'assemblage: Noeud de départ
   GroupeColor_B        : byte;
   GroupeColor_A        : byte;          // Groupe de couleur
   NoeudArrivee         : Smallint;      // Table d'assemblage: Noeud d// arrivée
   XDep                 : single;        // X départ
   YDep                 : single;        // Y départ
   ZDep                 : single;        // Z Départ
   UniteBoussole        : char;          // Unité boussole: D, G, M
   UniteClino           : char;          // Unité clino: D,G,M
   ZeroAzimutal         : char;          // Zéro azimutal: O/N
   CorrectionBoussole   : single;        // Correction sur boussole
   Declinaison          : single;        // Déclinaison magnétique
   CorrectionClino      : single;        // Correction sur clino
   TDecametre           : char;          // Mesure des longueurs: T/D
   Topofil              : single;        // Longueur de départ
   FactCorrect          : single;        // Facteur de correction longueurs
   Developpement        : single;        // Développement total
   X_Noeud_Depart       : single;        //
   Y_Noeud_Depart       : single;        // Coordonnées des noeuds
   Z_Noeud_Depart       : single;        //
   X_Noeud_Arrivee      : single;        // puis
   Y_Noeud_Arrivee      : single;        //
   Z_Noeud_Arrivee      : single;        // Encombrement du cheminement
   DeltaXTrigo          : single;        // Accroissement trigonométrique sur X
   DeltaYTrigo          : single;        // Accroissement trigonométrique sur Y
   DeltaZTrigo          : single;        // Accroissement trigonométrique sur Z
   Wx                   : single;        // Poids global sur X
   Wy                   : single;        // Poids global sur Y
   Wz                   : single;        // Poids global sur Z
   PrecisionLongueur    : single;
   PrecisionBoussole    : single;
   PrecisionClino       : single;
   NbPoints             : Smallint;
end;




// structure des tables des stations
Type THadesVisees = record
   NumPoint            : smallint;    //'Numéro du point
   PtDepart            : smallint;    //        'Départ visée
   PtArrivee           : smallint;    //        'Arrivée visée
   Longueur            : single;      //         'Longueur
   Azimut              : single;      //         'Azimut
   Pente               : single;      //         'Pente
   LD                  : single;      //         'Distance à droite
   LG                  : single;      //         'Distance à gauche
   HZ                  : single;      //         'Hauteur au-dessus
   HN                  : single;      //         'Hauteur du point
   //Commentaire         : array[0..24] of char; // Commentaire
   Commentaire         : array[0..79] of char; // Commentaire
   X                   : single;      //         'X
   Y                   : single;      //         'Y cheminement
   Z                   : single;      //         'Z
   Wx                  : single;      //
   Wy                  : single;      //
   Wz                  : single;      //
   LPC                 : single;      //         'Longueur projetée cumulée
   XPD                 : single;      //         'X point droit contour
   YPD                 : single;      //         'Y point gauche contour
   XPG                 : single;      //         'X point droit contour
   YPG                 : single;      //         'Y point gauche contour
End;
Type THadesViseesSimplifie = record
   NumPoint            : Integer;    //'Numéro du point
   PtDepart            : Integer;    //        'Départ visée
   PtArrivee           : Integer;    //        'Arrivée visée
   Longueur            : Double;      //         'Longueur
   Azimut              : Double;      //         'Azimut
   Pente               : Double;      //         'Pente
   LD                  : Double;      //         'Distance à droite
   LG                  : Double;      //         'Distance à gauche
   HZ                  : Double;      //         'Hauteur au-dessus
   HN                  : Double;      //         'Hauteur du point
   Commentaire         : string; // Commentaire
End;

type TTableauViseesHades = array of THadesViseesSimplifie;
type TParamCheminementsSimplifie = record
   CodeChem             : string;   // Cheminement de branchement
   Sub_System           : integer;      // Sous-système: cavité, grand réseau, etc ...
   NomChem              : string;   // Nom du cheminement
   DateCreation         : TDateTime;   // Date du relevé
   Operateurs           : string;   // Opérateurs
   //Commentaire          : array[0..254] of char;  // Commentaire
   Commentaire          : string;
   IDLittNdDep          : string;
   IDLittNdArr          : string;
   NoeudDepart          : integer;      // Table d'assemblage: Noeud de départ
   NoeudArrivee         : integer;      // Table d'assemblage: Noeud d// arrivée
   UniteBoussole        : char;          // Unité boussole: D, G, M
   UniteClino           : char;          // Unité clino: D,G,M
   ZeroAzimutal         : char;          // Zéro azimutal: O/N
   CorrectionBoussole   : Double;        // Correction sur boussole
   Declinaison          : Double;        // Déclinaison magnétique
   CorrectionClino      : Double;        // Correction sur clino
   TDecametre           : char;          // Mesure des longueurs: T/D
   IsTopofil            : boolean;
   Topofil              : Double;        // Longueur de départ
   FactCorrect          : Double;        // Facteur de correction longueurs
   ListeVisees          : TTableauViseesHades;
end;

//------------------------------------------------------------------------------


//------------------------------------------------------------------------------

type

  { TfrmImportHADES }

  TfrmImportHADES = class(TForm)
    btnExtractionDonnees: TButton;
    lsbContenuXYZ: TListBox;
    lsbListerXYZ: TButton;
    lbDossierHades: TLabel;
    lsbContenuPRM: TListBox;
    lsbContenuDossier: TFileListBox;
    treeListeDossiers: TShellTreeView;
    procedure btnExtractionDonneesClick(Sender: TObject);
    procedure lsbContenuPRMClick(Sender: TObject);
    procedure treeListeDossiersChange(Sender: TObject; Node: TTreeNode);
  private
    FCurrentDossierHADES: string;

    FTableauDonneesPRM: array of TParamCheminementsSimplifie;
    { private declarations }
    function IsDossierHADES(const D: string): boolean;
    procedure ListerPRM(const FichierPRM: string);
    procedure ListerXYZ(const FichierXYZ: string; const Params: TParamCheminementsSimplifie);

  public
    { public declarations }
    procedure Initialiser(const D: string);
  end;

var
  frmImportHADES: TfrmImportHADES;

implementation

{$R *.lfm}

{ TfrmImportHADES }


procedure TfrmImportHADES.treeListeDossiersChange(Sender: TObject; Node: TTreeNode);
var
  EWE: String;
begin
  //EWE := treeListeDossiers.Root + Node.GetTextPath; // avec Lazarus 1.1.x
  EWE := Node.GetTextPath;                            // avec Lazarus 1.2.x
  lbDossierHades.Caption  := EWE;
  if (IsDossierHADES(EWE)) then
  begin
    lsbContenuDossier.Directory  := EWE;
    FCurrentDossierHADES         := EWE;
    btnExtractionDonnees.Enabled := True;
  end
  else
  begin
    lsbContenuDossier.Clear;
    btnExtractionDonnees.Enabled := false;
  end;
end;

procedure TfrmImportHADES.btnExtractionDonneesClick(Sender: TObject);
begin
  ListerPRM(lsbContenuDossier.Directory + PathDelim + 'RE01.PRM');
end;

procedure TfrmImportHADES.lsbContenuPRMClick(Sender: TObject);
var
  WU: TParamCheminementsSimplifie;
begin
  WU := FTableauDonneesPRM[lsbContenuPRM.ItemIndex];
  ListerXYZ(FCurrentDossierHADES + PathDelim + WU.CodeChem + '.XYZ', WU);
end;


function TfrmImportHADES.IsDossierHADES(const D: string): boolean;
var
  EWE: String;
begin
  EWE := StringReplace(D + PathDelim + 'RE01.PRM', '/', '\', [rfReplaceAll, rfIgnoreCase]);
  EWE := StringReplace(EWE, '\\', '\', [rfReplaceAll, rfIgnoreCase]);
  Result := FileExistsUTF8(EWE);
end;

procedure TfrmImportHADES.ListerPRM(const FichierPRM: string);
var
  EWE: TParamCheminements;
  WU : TParamCheminementsSimplifie;
  fp: file of TParamCheminements;
  Nb: Int64;
  i: Integer;
begin
  AfficherMessage(Format('%s.ListerPRM: %s', [ClassName, FichierPRM]));
  AssignFile(fp, FichierPRM);

  try
    ReSet(fp);
    Nb := FileSize(fp);
    AfficherMessage(Format('--> %d elements', [Nb]));
    SetLength(FTableauDonneesPRM, Nb);

    lsbContenuPRM.Clear;
    for i := 0 to Nb-1 do
    begin
      Seek(fp, i);
      Read(fp, EWE);
      WU.CodeChem        := Trim(String(EWE.CodeChem));
      WU.NomChem         := Trim(String(EWE.NomChem));
      WU.DateCreation    := StrToIntDef(Trim(String(EWE.DateCreation)), -1);
      WU.Operateurs      := Trim(String(EWE.Operateurs));
      WU.Topofil         := EWE.Topofil;
      WU.IsTopofil       := Trim(String(EWE.TDecametre)) = 'T';
      FTableauDonneesPRM[i] := WU;

      lsbContenuPRM.Items.Add(Format('%d: "%s" - "%s" - "%s" - NdDep: %d [%s] - NdArr: %d [%s]', [i,
                                                  WU.CodeChem,
                                                  WU.NomChem,
                                                  WU.Operateurs,
                                                  WU.NoeudDepart , WU.IDLittNdDep,
                                                  WU.NoeudArrivee, WU.IDLittNdArr]));
      //*)
    end;
    lsbContenuPRM.ItemIndex := 0;
  finally
    CloseFile(fp);
  end;
end;

procedure TfrmImportHADES.ListerXYZ(const FichierXYZ: string; const Params: TParamCheminementsSimplifie);
var
  fp  : file of THadesVisees;
  EWE : THadesVisees;
  i   : integer;
  Nb  : Int64;
begin
  AfficherMessage(Format('%s.ListerXYZ: %s', [ClassName, FichierXYZ]));
  AssignFile(fp, FichierXYZ);
  lsbContenuXYZ.Clear;
  try
    ReSet(fp);
    Nb := FileSize(fp);
    AfficherMessage(Format('--> %d elements', [Nb]));
    EWE.Longueur := Params.Topofil;
    EWE.Azimut   := 0.00;
    EWE.Pente    := 0.00;
    EWE.LG   := 0.00;
    EWE.LD   := 0.00;
    EWE.HZ   := 0.00;
    EWE.HN   := 0.00;
    EWE.Commentaire:= IIF(Params.IsTopofil, 'Mode topofil', 'Mode deca');
    lsbContenuXYZ.Items.Add(Format('%d ; %.2f, %.2f, %.2f; %.2f, %.2f, %.2f, %.2f; "%s"', [
                                    0,
                                    EWE.Longueur, EWE.Azimut, EWE.Pente,
                                    EWE.LG, EWE.LD, EWE.HZ, EWE.HN,
                                    String(EWE.Commentaire),
                                    EWE.X, EWE.Y, EWE.Z, EWE.LPC
                                    ]));
    for i := 0 to Nb -1 do
    begin
      Seek(fp, i);
      Read(fp, EWE);
      lsbContenuXYZ.Items.Add(Format('%d ; %.2f, %.2f, %.2f; %.2f, %.2f, %.2f, %.2f; "%s"', [
                                    i+1,
                                    EWE.Longueur, EWE.Azimut, EWE.Pente,
                                    EWE.LG, EWE.LD, EWE.HZ, EWE.HN,
                                    String(EWE.Commentaire),
                                    EWE.X, EWE.Y, EWE.Z, EWE.LPC
                                    ]));
    end;
  finally
    CloseFile(fp);
  end;
end;

procedure TfrmImportHADES.Initialiser(const D: string);
begin
  FCurrentDossierHADES := '';
  SetLength(FTableauDonneesPRM, 0);
  treeListeDossiers.Root := D;
end;



end.

