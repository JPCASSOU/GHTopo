unit UnitFusionTopos;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, LazFileUtils, Graphics,
  StructuresDonnees,
  Common,
  ConvertisseurJPC,
  ToporobotClasses2012, UnitObjetSerie;

type

{ TContainerFusionTopo }

 TContainerFusionTopo = class
  private
    // pointeur vers le document destination
    // qui est généralement le document courant de GHTopo
    FPtrDocTopoDest   : TToporobotStructure2012;
    // pointeur sur le convertisseur de coordonnées
    FPtrConvertisseur : TConversionSysteme;
    // système de coordonnées général
    FSystemEPSG       : TLabelSystemesCoordsEPSG;
    // Convertir les coordonnées de l'entrée depuis son repère
    FDoReprojectEntrancesCoordinates: boolean;
    // Liste des fichiers à fusionner
    FListeFichiersAFusionner: TStringList;

    (*
    FLastIdxSerieUsed    : TNumeroSerie;
    FLastIdxReseauUsed   : TNumeroReseau;
    FLastIdxSecteurUsed  : TNumeroSecteur;
    FLastIdxEntranceUsed : TNumeroEntrance;
    FLastIdxCodeUsed     : TNumeroCode;
    FLastIdxExpeUsed     : TNumeroExpe;
    //*)


    function ChargerLaTopo(const FD: TToporobotStructure2012; const QFileName: TStringDirectoryFilename): boolean;
    function FusionnerTopo(const nth: integer; const QFilename: TStringDirectoryFilename): boolean;
  public
    function  Initialiser(const FD: TToporobotStructure2012;
                          const CV: TConversionSysteme;
                          const SystemeEPSG: TLabelSystemesCoordsEPSG;
                          const DoReprojectEntrancesCoordinates: boolean): boolean;
    procedure Finaliser();
    procedure AddFichierAFusionner(const FN: TStringDirectoryFilename);

    function GetNbFichiersAFusionner(): integer;
    function FusionnerNthTopo(const Idx: integer): integer;
end;

implementation

{ TContainerFusionTopo }

function TContainerFusionTopo.Initialiser(const FD: TToporobotStructure2012;
                                          const CV: TConversionSysteme;
                                          const SystemeEPSG: TLabelSystemesCoordsEPSG;
                                          const DoReprojectEntrancesCoordinates: boolean): boolean;
begin
  Result := false;
  AfficherMessage(Format('%s.Initialiser()', [ClassName]));
  FPtrDocTopoDest   := FD;
  FPtrConvertisseur := CV;
  FSystemEPSG := SystemeEPSG;

  AfficherMessage(Format('--> Système de coordonnées: EPSG:%d - %s', [FSystemEPSG.CodeEPSG, FSystemEPSG.NomEPSG]));
  AfficherMessage('--> Document destination: ' + FPtrDocTopoDest.GetNomEtude());
  FPtrConvertisseur := CV;
  FDoReprojectEntrancesCoordinates := DoReprojectEntrancesCoordinates;
  FListeFichiersAFusionner := TStringList.Create;
  try
    FListeFichiersAFusionner.Clear;
    result := True;
  except
    result := false;
  end;
end;

procedure TContainerFusionTopo.Finaliser();
begin
  AfficherMessage(Format('%s.Finaliser()', [ClassName]));
  try
    FListeFichiersAFusionner.Clear;
  finally
    FreeAndNil(FListeFichiersAFusionner);//FListeFichiersAFusionner.Free;
  end;
end;

procedure TContainerFusionTopo.AddFichierAFusionner(const FN: TStringDirectoryFilename);
begin
  if (FileExistsUTF8(FN)) then FListeFichiersAFusionner.Add(FN);
end;

function TContainerFusionTopo.GetNbFichiersAFusionner(): integer;
begin
  result := FListeFichiersAFusionner.Count;
end;

function TContainerFusionTopo.FusionnerNthTopo(const Idx: integer): integer;
var
  EWE: String;
begin
  result := 0;
  AfficherMessage(Format('---->%s.FusionnerNthTopo: %d', [ClassName, Idx]));

  EWE := FListeFichiersAFusionner.Strings[Idx];
  FusionnerTopo(Idx, EWE);

end;

// Fusion d'une topographie
function TContainerFusionTopo.FusionnerTopo(const nth: integer; const QFilename: TStringDirectoryFilename): boolean;
var
  FDocTopoAFusionner: TToporobotStructure2012;
  QStartIdxExpe     : TNumeroExpe;
  QStartIdxCode     : TNumeroCode;
  QStartIdxSerie    : TNumeroSerie;
  MyExpe: TExpe;
  Nb, i, NbExpes, NbCodes, NSD, NSA, QNbVisees, j, FNthCavite: Integer;
  MyCode: TCode;
  MyEntrance: TEntrance;
  MySerie: TObjSerie;
  NSS: TNumeroSerie;
  MyVisee: TUneVisee;
  MyReseau: TReseau;
  MySecteur: TSecteur;
  SystProjOfDocTopoAFusionner: TLabelSystemesCoordsEPSG;
  GPTOut, GPTIn: TProjUV;
  MyNamespace: TNameSpace;

begin
  result := false;
  AfficherMessage(Format('         %s.FusionnerTopo: %d: %s', [ClassName, nth, QFilename]));
  FDocTopoAFusionner := TToporobotStructure2012.Create;
  try
    if (ChargerLaTopo(FDocTopoAFusionner, QFileName)) then
    begin
      AfficherMessageErreur(Format('Fusion de topo: Chargement de %s OK', [QFilename]));
      AfficherMessageErreur(Format('Topographie: %s', [FDocTopoAFusionner.GetDatabaseName]));
      AfficherMessageErreur('--------------');
      AfficherMessageErreur(Format('  %d entrees ', [FDocTopoAFusionner.GetNbEntrances()]));
      AfficherMessageErreur(Format('  %d reseaux ', [FDocTopoAFusionner.GetNbReseaux()]));
      AfficherMessageErreur(Format('  %d secteurs ', [FDocTopoAFusionner.GetNbSecteurs()]));
      AfficherMessageErreur(Format('  %d codes ', [FDocTopoAFusionner.GetNbCodes()]));
      AfficherMessageErreur(Format('  %d seances ', [FDocTopoAFusionner.GetNbExpes()]));
      AfficherMessageErreur(Format('  %d tirs radiants ', [FDocTopoAFusionner.GetNbAntennes()]));
      AfficherMessageErreur(Format('  %d series; %d', [FDocTopoAFusionner.GetNbSeries(), FDocTopoAFusionner.getMaxIdxSerie()]));
      AfficherMessageErreur('***');



      FNthCavite := (1 + nth) * NB_MAXI_SERIES_PAR_CAVITE;
      // Conversion des entrées


      // Fusion des secteurs
      // Fusion de la liste des expés
      Nb := FDocTopoAFusionner.GetNbExpes();
      for i := 1 to Nb - 1 do
      begin
        MyExpe := FDocTopoAFusionner.GetExpe(i);
        MyExpe.IDExpe      := FNthCavite + MyExpe.IDExpe;
        MyExpe.Commentaire := Format('[CaviteFusionnee_%d - %s', [nth, MyExpe.Commentaire]);
        FPtrDocTopoDest.AddExpe(MyExpe);
      end;
      // Fusion de la liste des expés
      Nb := FDocTopoAFusionner.GetNbCodes();
      for i := 1 to Nb - 1 do
      begin
        MyCode := FDocTopoAFusionner.GetCode(i);
        MyCode.IDCode      := FNthCavite + Mycode.IDCode;
        MyCode.Commentaire := Format('[CaviteFusionnee_%d - %s', [nth, MyCode.Commentaire]);
        FPtrDocTopoDest.AddCode(MyCode);
      end;
      // et on met à jour l'index anti-chevauchement des codes
      //*********
      // Les entrées
      // pour l'instant, on n'utilise que la première entrée
      Nb := FDocTopoAFusionner.GetNbEntrances();
      SystProjOfDocTopoAFusionner := FDocTopoAFusionner.GetCodeEPSGSystemeCoordonnees();

      //for i := 0 to Nb - 1 do
      begin
        MyEntrance := FDocTopoAFusionner.GetEntrance(0);

        MyEntrance.eRefSer := FNthCavite;
        MyEntrance.eObserv := Format('[CaviteFusionnee_%d - %s', [nth, MyEntrance.eObserv]);
        if (FDoReprojectEntrancesCoordinates) then
        begin

          GPTIn.U := MyEntrance.eXEntree;
          GPTIn.V := MyEntrance.eYEntree;

          GPTOut  := FPtrConvertisseur.ConversionSyst1ToSyst2EPSG(SystProjOfDocTopoAFusionner.CodeEPSG, FSystemEPSG.CodeEPSG, GPTIn);
          MyEntrance.eXEntree  := GPTOut.U;
          MyEntrance.eYEntree  := GPTOut.V;

          AfficherMessageErreur(Format('Entrance %d: %s - EPSG:%d = %.2f, %.2f, %.2f -> EPSG:%d = %.2f, %.2f',
                                       [nth, MyEntrance.eNomEntree,
                                        SystProjOfDocTopoAFusionner.CodeEPSG, GPTIn.U, GPTIn.V,  MyEntrance.eZEntree,
                                        FSystemEPSG.CodeEPSG,  MyEntrance.eXEntree, MyEntrance.eYEntree
                                       ]));

        end;
        FPtrDocTopoDest.AddEntrance(MyEntrance);
      end;
      // Fusion des réseaux
      Nb := FDocTopoAFusionner.GetNbReseaux();
      for i := 0 to Nb - 1 do
      begin
        MyReseau := FDocTopoAFusionner.GetReseau(i);
        MyReseau.NomReseau := Format('%s: %s', [MyEntrance.eNomEntree, MyReseau.NomReseau]);
        MyReseau.ObsReseau := Format('[Reseau issu de la fusion #%d] %s', [nth, MyReseau.ObsReseau]);
        FPtrDocTopoDest.AddReseau(MyReseau);
      end;
      // Fusion des secteurs
      Nb := FDocTopoAFusionner.GetNbSecteurs();
      for i := 0 to Nb - 1 do
      begin
        MySecteur := FDocTopoAFusionner.GetSecteur(i);
        MySecteur.NomSecteur := Format('%s: %s', [MyEntrance.eNomEntree, MySecteur.NomSecteur]);
        FPtrDocTopoDest.AddSecteur(MySecteur);
      end;
      // Fusion des séries
      Nb := FDocTopoAFusionner.GetNbSeries();
      for i := 1 to Nb - 1 do
      begin
        MySerie := FDocTopoAFusionner.GetSerie(i);
        NSS := FNthCavite + MySerie.GetNumeroDeSerie();
        NSD := FNthCavite + MySerie.GetNoSerieDep();
        NSA := FNthCavite + MySerie.GetNoSerieArr();
        MySerie.SetNumeroSerie(NSS);
        if (i = 1) then
        begin
          MySerie.SetNoSerieDep(FNthCavite);
          MySerie.SetNoPointDep(0);
        end else begin
          MySerie.SetNoSerieDep(NSD);
        end;
        MySerie.SetNoSerieArr(NSA);
        MySerie.SetNumeroEntrance(FPtrDocTopoDest.GetNbEntrances() - 1);
        MySerie.SetNumeroReseau(FPtrDocTopoDest.GetNbReseaux() - 1 + MySerie.GetNumeroReseau());
        // mise à jour des expés, codes, ... dans les visées
        QNbVisees := MySerie.GetNbVisees();
        for j := 0 to QNbVisees - 1 do
        begin
          MyVisee := MySerie.GetVisee(j);
          MyVisee.Commentaires += Format(':: C=%d - E=%d', [MyVisee.Code, MyVisee.Expe]);
          MyVisee.Code       := FNthCavite + MyVisee.Code;
          MyVisee.Expe       := FNthCavite + MyVisee.Expe;
          MyVisee.IDSecteur  := FPtrDocTopoDest.GetNbSecteurs() - 1 + MyVisee.IDSecteur;
          MySerie.PutVisee(j, MyVisee);
        end;
        FPtrDocTopoDest.AddSerie(MySerie);
      end;
    end;
    // et on ajoute l'espace de noms associé
    MyNamespace.Nom      := ExtractFileNameOnly(QFilename);
    MyNamespace.Couleur  := RGBToColor(Random(255), Random(255), Random(255));
    MyNamespace.Description := 'Fusionned from ' + QFilename;
    FPtrDocTopoDest.AddNameSpace(MyNamespace);

    FDocTopoAFusionner.Finaliser();
  finally
    FDocTopoAFusionner.Free;
  end;
end;
// chargement sécurisé d'une topo
function TContainerFusionTopo.ChargerLaTopo(const FD: TToporobotStructure2012; const QFileName: TStringDirectoryFilename): boolean;
begin
  AfficherMessage(Format('                 %s.ChargerLaTopo: %s', [ClassName, QFilename]));
  result := false;
  if (not FileExistsUTF8(QFilename)) then
  begin
    AfficherMessageErreur(Format(GetResourceString(rsMSG_FILENOTFOUND), [QFilename]));
    Exit(false);
  end;

  try
    FD.ReInitialiser(True);
    FD.SetDatabaseName(QFilename);
    if (Pos('.text', LowerCase(QFilename))>0) then
    begin
      if (FD.LoadFichierText(QFilename) < 0) then
      begin
        AfficherMessageErreur('Le fichier comporte des erreurs - Voir les consoles');
        Exit(false);
      end;
    end
    else if (Pos('.gtx', QFilename) > 0) then // fichier GHTopo XML ?
    begin
      if (FD.LoadFromXML(QFilename) < 0) then
      begin
        AfficherMessageErreur('Le fichier comporte des erreurs. Il doit être audité dans un éditeur XML');
        Exit(False);
      end;
    end
    else
    begin // sinon, c'est un fichier supposé Tab ou XTB
      if (FD.LoadFromXTB(QFilename)<0) then
      begin
        AfficherMessageErreur('Le fichier comporte des erreurs - Voir le rapport');
        Exit(false);
      end;
    end;
    Result := true;
  except

  end;
end;
end.

