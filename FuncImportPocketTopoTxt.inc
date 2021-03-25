//FuncImportPocketTopoTxt.inc
// charger un fichier PocketTopo TXT
//function TToporobotStructure2012.LoadFromPocketTopoTXT(const FichierTXT: TStringDirectoryFilename): integer;  deprecated;



type TLineError = (errLnNONE,
                   errLnUNIMPLEMENTED,
                   errLnEMPTY_LINE,
                   errLnCOMMENT_LINE,
                   errLnIS_STATION_1_0,
                   errLnCHANGE_CURRENT_STATION,
                   errLnINVALID_ID_STATION_DEP,
                   errLnINVALID_ST_ARR,
                   errLnSTATION_DETECTED,
                   errLnREPRISE_SERIE,
                   errLnIGNORED,
                   errLnOTHER,
                   errLnSTOP_SIGNAL);
var
  EPSGDefault       : TLabelSystemesCoordsEPSG;
  EWE               : TGHStringArray;
  pTXT              : TextFile;
  LigneTXT          : String;
  WU                : String;
  ErrMsg , QNomEtude: string;
  QCurrentIdDepart, QCurrentIdArrivee: String;
  p: integer;
  i, Nb: integer;
  NbLignesLues, nbSeries, QNbViseesAMoyenner, qna, QVisee, QInternalIdx, VV: Integer;
  QDateSeanceTopo: TDateTime;
  MyExpe: TExpe;
  MyCode: TCode;
  ListeOfTGHStringArray: TListeOfTGHStringArray;
  QStationDepart0, QStationArrivee0: string;
  QStationDepart1, QStationArrivee1: string;

  MyAntenne: TViseeAntenne;
  MySerie: TObjSerie;
  MyVisee: TUneVisee;

  QLongueur, QAzimut, QPente: Extended;
  QArrViseesAMoyenner: array of TMesureViseeDistoX;
  QStationDepart, QStationArrivee, QLaStationCourante: TToporobotIDStation;
  EWE0, EWE1: TGHStringArray;
  QNumeroDeSerieCourant : TNumeroSerie;

  procedure ReadAndDispLigneLue(out MyLine: string);
  begin
    ReadLn(pTXT, MyLine);
    MyLine += ' '; // espace de sécurité en queue
    Inc(NbLignesLues);
    //AfficherMessageErreur(Format('%d: %s', [NbLignesLues, MyLine]));
  end;

begin
  Result := -1;
  QDateSeanceTopo := Now();
  ReInitialiser(True);
  //self.ClearListeSeries();
  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;

  WU := Format('%s.LoadFromPocketTopoTXT: %s', [ClassName, FichierTXT]);
  AfficherMessageErreur(WU + ' - Fonctionnalite en développement');
  // Fichier introuvable ? OK, On sort
  if (not FileExists(FichierTXT)) then  Exit(-32);
  // nom étude par défaut
  SetNomEtude(ExtractFileNameWithoutExt(FichierTXT));
  SetCodeEPSGSystemeCoordonnees(EPSGDefault);






  CreateNewEntrance();
  CreateNouveauReseau();
  CreateNouveauSecteur();
  MyCode := MakeUsualTCode(1, 360.00, 360.00, 1.00, 0.01, 1.00, 1.00, 0.00, 0.00, 0.00, 'Topo du ' + DatePascalToDateSQL(QDateSeanceTopo));
  self.AddCode(MyCode);
  MyExpe := MakeTExpe(1, YearOf(QDateSeanceTopo), MonthOf(QDateSeanceTopo), DayOf(QDateSeanceTopo), 2, cdmAUTOMATIQUE, 0.00, 'Speleometre', 'Speleographe', 'Expe01');
  self.AddExpe(MyExpe);

  // et on les extrait
  MyCode := GetLastCode();
  MyExpe := GetLastExpe();
  AfficherMessageErreur(Format('-- Code %d/%d: %s', [MyCode.IDCode, GetNbCodes(), MyCode.Commentaire]));
  AfficherMessageErreur(Format('-- Expé %d/%d: %s', [MyExpe.IDExpe, GetNbExpes(), DatePascalToDateSQL(EncodeDate(MyExpe.AnneeExpe, MyExpe.MoisExpe, MyExpe.JourExpe))]));
  QCurrentIdDepart  := '1.0';
  QCurrentIdArrivee := '1.0';

  try
    ListeOfTGHStringArray := TListeOfTGHStringArray.Create;
    ListeOfTGHStringArray.ClearListe();



    AssignFile(pTXT, FichierTXT);
    NbLignesLues := 0;
    try
      ReSet(pTXT);
      // La première ligne contient le nom de l'étude
      //Tabanac_20210321   (m, 360)
      ReadAndDispLigneLue(LigneTXT);
      p := Pos(' ', LigneTXT);
      QNomEtude := Copy(LigneTXT, 1, p);
      SetNomEtude(QNomEtude);
      AfficherMessageErreur(GetNomEtude());
      //
      // on lit les autres lignes
      while(not eof(pTXT)) do
      begin
        ReadAndDispLigneLue(LigneTXT);
        if (''  = Trim(LigneTXT)) then Continue; // ligne vide = on continue
        if ('#' = LigneTXT[1])   then Continue; // dièse en tête           = commentaire
        if ('[' = LigneTXT[1])   then Continue; // crochet ouvrant en tête = ligne ignorée

        if (1 = pos('stop', LowerCase(LigneTXT))) then Break;  // signal d'arrêt du traitement à inclure dans le fichier
        //0        1         2         3         4         5         6         7         8         9         0
        //1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
        //     1.0              0,00          0,00      0,00


        EWE := SplitFixedColLine(LigneTXT, [1, 11, 23, 33, 41, 50, 57, 100]);
        ListeOfTGHStringArray.AddElement(EWE);
        //QCurrentIdArrivee:= IIF(EWE[1];
        //1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
        //     1.0         1.1       0.050   81.53  -11.88  [1]
      end;

    finally
      CloseFile(pTXT);
    end;
    // contrôle et traitements
    Nb := ListeOfTGHStringArray.GetNbElements();
    AfficherMessageErreur(Format('==== %d', [Nb]));
    ListeOfTGHStringArray.SortByFirstColumn();
    // on extrait les antennes
    for i := Nb -1 downto 0 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      if ('' = Trim(EWE[1])) then ListeOfTGHStringArray.RemoveElement(i);
      QStationDepart := DecomposeStationToporobot(Trim(EWE[0]));
      MyAntenne.EntranceRatt:= 0;
      MyAntenne.Reseau      := 0;
      MyAntenne.Secteur     := 0;
      MyAntenne.MarkedForDelete := false;

      MyAntenne.SerieDepart := QStationDepart.aSerie;
      MyAntenne.PtDepart    := QStationDepart.aStation;
      MyAntenne.Longueur    := ConvertirEnNombreReel(Trim(EWE[2]), -1.00);
      MyAntenne.Azimut      := ConvertirEnNombreReel(Trim(EWE[3]), 0.00);
      MyAntenne.Pente       := ConvertirEnNombreReel(Trim(EWE[4]), 0.00);
      MyAntenne.Commentaires:= '';
      MyAntenne.IDTerrainStation := '';

      if (MyAntenne.Longueur > 0.00) then AddViseeAntenne(MyAntenne);
    end;
    AfficherMessageErreur(Format('==== %d antennes ajoutées', [GetNbAntennes()]));
    Nb := ListeOfTGHStringArray.GetNbElements();
    AfficherMessageErreur(Format('==== %d', [Nb]));
    for i := 0 to Nb -1 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      AfficherMessageErreur(EWE[0] + '; ' + EWE[1] + '; ' + EWE[2] + '; ' + EWE[3] + '; ' + EWE[4] + '; ' +  EWE[5] + '; ');
    end;
    // nombre de séries:
    CreateNewSerie(1, 0, 1, 'Galerie principale');
    nbSeries := GetNbSeries();
    AfficherMessageErreur(Format('%d séries', [nbSeries]));

    // les nouvelles séries = lignes de la forme
    //   363.33      386.0       0.000    0.00    0.00         "retrancher 26 deg aux azimuts"
    Nb := ListeOfTGHStringArray.GetNbElements();
    for i := Nb -1 downto 0 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      QStationDepart  := DecomposeStationToporobot(Trim(EWE[0]));
      QStationArrivee := DecomposeStationToporobot(Trim(EWE[1]));
      if ((QStationArrivee.aSerie > 0) and (0 = QStationArrivee.aStation)) then
      begin
        CreateNewSerie(QStationDepart.aSerie, QStationDepart.aStation, QStationArrivee.aSerie, 'Depart en ' + Trim(EWE[0]));

        ListeOfTGHStringArray.RemoveElement(i);
      end;
    end;
    (*
     NSR.ClearStations();          // Opération 1: Vider la liste des visées
    NSR.SetChanceObstacle(0, 0);  // Opération 2: Chances et obstacles
      MySerieCible.SetChanceObstacle(MySerieSource.GetChance(), MySerieSource.GetObstacle());                       // Opération 2: Chances et obstacles
          MySerieCible.SetSeriePtExtremites(MySerieSource.GetNoSerieDep(), MySerieSource.GetNoPointDep(),               // Opération 3: Extrémités de la série
                                            MySerieSource.GetNoSerieArr(), MySerieSource.GetNoPointArr());
          MySerieCible.SetNumeroSerie(MySerieSource.GetNumeroDeSerie());                                                // Opération 4: Numéro de série
          MySerieCible.SetNomObsSerie(MySerieSource.GetNomSerie(), '*** Imported *** ' + MySerieSource.GetObsSerie());  // Opération 5: Nom et observations sur la série
          MySerieCible.SetRaideur(MySerieSource.GetRaideur());                                                          // Opération 6: Raideur
          MySerieCible.SetCouleur(MySerieSource.GetCouleur());                                                          // Opération 7: Couleur
          MySerieCible.SetNumeroEntrance(0);                                                                            // Opération 8: Entrée de rattachement
          MySerieCible.SetNumeroReseau(0);                                                                              // Opération 9: Réseau de rattachement


    //*)



    nbSeries := GetNbSeries();
    AfficherMessageErreur(Format('%d séries', [nbSeries]));
    SortSeries();
    for i := 0 to nbSeries -1 do
    begin
      MySerie := GetSerie(i);
      AfficherMessageErreur(Format('%d: %d: %d.%d -> %d.%d: %s', [i, MySerie.GetNumeroDeSerie(), MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), MySerie.GetNoSerieArr(), MySerie.GetNoPointArr(), MySerie.GetNomSerie()]));
    end;

    // moyenner les visées
    Nb := ListeOfTGHStringArray.GetNbElements();
    QLongueur := 0.00;
    QAzimut   := 0.00;
    QPente    := 0.00;
    QNbViseesAMoyenner := 0;
    SetLength(QArrViseesAMoyenner, 0);
    for i := Nb -1 downto 1 do
    begin


      EWE0 := ListeOfTGHStringArray.GetElement(i-1);
      EWE1 := ListeOfTGHStringArray.GetElement(i);

      (*
      QUneVisee.Device   := 0;
      QUneVisee.Longueur := ConvertirEnNombreReel(EWE1[2], -1.00);
      QUneVisee.Azimut   := ConvertirEnNombreReel(EWE1[3],  0.00);
      QUneVisee.Pente    := ConvertirEnNombreReel(EWE1[4],  0.00);
      //*)

      QStationDepart0  := Trim(EWE0[0]); QStationArrivee0  := Trim(EWE0[1]);
      QStationDepart1  := Trim(EWE1[0]); QStationArrivee1  := Trim(EWE1[1]);
      //QArrViseesAMoyenner;
      if ((QStationDepart1 = QStationDepart0) and (QStationArrivee1 = QStationArrivee0)) then
      begin
        //qna := length(QArrViseesAMoyenner);
        //SetLength(QArrViseesAMoyenner, 1 + qna);
        //QArrViseesAMoyenner[qna] := QUneVisee;

        ListeOfTGHStringArray.RemoveElement(i);
      end;
    end;
    Nb := ListeOfTGHStringArray.GetNbElements();
    AfficherMessageErreur(Format('= Doublons supprimés === %d', [Nb]));
    for i := 0 to Nb -1 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      AfficherMessageErreur(EWE[0] + '; ' + EWE[1] + '; ' + EWE[2] + '; ' + EWE[3] + '; ' + EWE[4] + '; ' +  EWE[5] + '; ');
    end;
    // remplissage des séries
    Nb := ListeOfTGHStringArray.GetNbElements();
    AfficherMessageErreur(Format('= Séries === %d', [Nb]));
    QNumeroDeSerieCourant := 0;

    for i := 0 to Nb -1 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      QLaStationCourante := DecomposeStationToporobot(Trim(EWE[0]));
      if (QLaStationCourante.aSerie <> QNumeroDeSerieCourant) then
      begin
        QNumeroDeSerieCourant := QLaStationCourante.aSerie;
        GetSerieByNumeroSerie(QNumeroDeSerieCourant, MySerie, QInternalIdx);
      end;
      MyVisee := EmptyVisee('');
      MyVisee.Code      := MyCode.IDCode;
      MyVisee.Expe      := MyExpe.IDExpe;
      MyVisee.IDSecteur := 0;
      MyVisee.TypeVisee := tgDEFAULT;


      MyVisee.Longueur := ConvertirEnNombreReel(EWE[2], -1.00);
      MyVisee.Azimut   := ConvertirEnNombreReel(EWE[3],  0.00);
      MyVisee.Pente    := ConvertirEnNombreReel(EWE[4],  0.00);
      MySerie.AddVisee(MyVisee);
    end;
    // et affichage de contrôle
    nbSeries := GetNbSeries();
    AfficherMessageErreur(Format('%d séries', [nbSeries]));
    SortSeries();
    for i := 0 to nbSeries -1 do
    begin
      MySerie := GetSerie(i);
      MySerie.SetNoSerieArr(MySerie.GetNumeroDeSerie());
      MySerie.SetNoPointArr(MySerie.GetNbVisees() - 1);
      AfficherMessageErreur(Format('%d: %d (%d pts): %d.%d -> %d.%d: %s', [i, MySerie.GetNumeroDeSerie(), MySerie.GetNbVisees(),MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), MySerie.GetNoSerieArr(), MySerie.GetNoPointArr(), MySerie.GetNomSerie()]));
    end;




    ListeOfTGHStringArray.ClearListe();
    SetCurrentNumeroCode(1);
    SetCurrentNumeroExpe(1);
    SetCurrentNumeroEntrance(0);
    SetCurrentNumeroReseau(0);
    SetCurrentNumeroSecteur(0);
    SetCurrentNumeroSerie(1);

    Result := GetNbSeries();
  finally
    FreeAndNil(ListeOfTGHStringArray);
  end;
end;

{





  NbLignesLues      : integer;
  NbErrorsInLoading : integer;
  IdxCourant        : Integer;

  UneVisee          : TMesureViseeDistoX;
  QArrViseesAMoyenner: array of TMesureViseeDistoX;

  QPreviousStationDepart  : TToporobotIDStation;
  QPreviousStationArrivee : TToporobotIDStation;

  QCurrentStationDepart   : TToporobotIDStation;
  QCurrentStationArrivee  : TToporobotIDStation;

  MyExpe: TExpe;
  MyCode: TCode;
  QDateSeanceTopo: TDateTime;
  QAT: TLineError;

  procedure WriteWarning(const wm: string);   // afficher avertissement
  begin
    AfficherMessage(wm);
  end;
  procedure WriteDebugMsg(const QNoLine: integer; const MSG: string);
  begin
    AfficherMessageErreur(Format('[%d]: %s', [QNoLine, MSG]));
  end;

  function QCompareTwoStations(const S1, S2: TToporobotIDStation): boolean;
  begin
    Result := (S1.aSerie   = S2.aSerie) and
              (S1.aStation = S2.aStation);

  end;

  function QTraiterLigneDeDonnees(const MyLine: string): TLineError;
  var
    EWE: TGHStringArray;
    SameBasePt: Boolean;
    QLongueur, QAzimut, QPente: Double;
    n, QInternalIdx: Integer;
    MySerie: TObjSerie;
  begin
    Result   := errLnUNIMPLEMENTED;
    if ('' = Trim(MyLine)) then Exit(errLnEMPTY_LINE); // ligne vide
    // signal d'arrêt du traitement à inclure dans le fichier
    if (1 = pos('stop', LowerCase(MyLine))) then exit(errLnSTOP_SIGNAL);

    // lignes contenant les expés: Ne pas les utiliser
    // [1]: 2016/12/12     0.00  "JP CASSOU\rGRAS-GESA"
    // [2]: 2016/12/16     0.00  "JP CASSOU\rLALANNE"
    if ('[' = MyLine[1])   then Exit(errLnIGNORED);
     (*
.        1         2         3         4         5         6        7         8         9        10        11
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
     1.0         1.1       0.020  139.00  -10.00  [16]    "entree citon 1"
  2335.13     2221.0       0.000    0.00    0.00
  2221.0      2221.1       4.480  100.10   -6.30  [67]
  2221.1                   5.440    8.12    0.00  [67]
  2221.1                   1.290  188.12    0.00  [67]
  2221.3      2221.4       6.890  100.82   -1.83  [67]

//*)



    QCurrentStationDepart  := DecomposeStationToporobot(EWE[0]);
    QCurrentStationArrivee := DecomposeStationToporobot(EWE[1]);


    // station 1.0 trouvée -> on sort
    if ((1 = QCurrentStationDepart.aSerie) and (0 = QCurrentStationDepart.aStation) and (QCurrentStationArrivee.aSerie = -1)) then
    begin
      MySerie := TObjSerie.Create;
      MySerie.ClearStations();
      MySerie.SetNomObsSerie('Serie 1', '');
      MySerie.SetNumeroSerie(1);
      MySerie.SetSeriePtDep(1, 0);
      MySerie.AddVisee(0, 1, 1, tgDEFAULT, 0.01, 135, -45, 1,1,1,1, '', '');
      MySerie.SetSeriePtArr(MySerie.GetNumeroDeSerie(), MySerie.GetNbVisees() - 1);
      AddSerie(MySerie);


      QPreviousStationDepart  := QCurrentStationDepart;
      QPreviousStationArrivee := QCurrentStationArrivee;

      Exit(errLnIS_STATION_1_0);
    end;



    // lignes de la forme: 4105.25     4131.0       0.000    0.00    0.00  [68]
    // --> On crée une nouvelle série
    if (QCurrentStationDepart.aSerie > 0) then
    begin
      if (QCurrentStationArrivee.aStation = 0) then
      begin
        WriteDebugMsg(NbLignesLues,  Format('Une série %d va être créée depuis %d.%d', [QCurrentStationArrivee.aSerie, QCurrentStationDepart.aSerie, QCurrentStationDepart.aStation]));
        SetLength(QArrViseesAMoyenner, 0);

        // créer ici la nouvelle série
        MySerie := TObjSerie.Create;
        MySerie.ClearStations();
        MySerie.SetNumeroSerie(QCurrentStationArrivee.aSerie);
        MySerie.SetSeriePtExtremites(QCurrentStationDepart.aSerie, QCurrentStationDepart.aStation, QCurrentStationArrivee.aSerie, 0);
        self.AddSerie(MySerie);
        QPreviousStationDepart  := QCurrentStationDepart;
        QPreviousStationArrivee := QCurrentStationArrivee;
        exit(errLnNONE);
      end;
      QLongueur := ConvertirEnNombreReel(EWE[2], -1.00);
      QAzimut   := ConvertirEnNombreReel(EWE[3], -1.00);
      QPente    := ConvertirEnNombreReel(EWE[4], -666.00);
      // lignes à champ 'Vers' (2e colonne) vides
      // lignes de la forme:   2227.4                   XXXX  XXXX   XXXX  [67]
      if (QCurrentStationArrivee.aSerie = -1) and (QCurrentStationArrivee.aStation = -1) then
      begin
        // lignes de la forme:   2227.4                   5.010  238.27    0.00  [67] --> Ce sont des antennes
        if (QLongueur > 0.00) then
        begin
          //WriteDebugMsg(NbLignesLues,  Format('Une antenne partant de %d.%d va être ajoutée: %.2f, %.2f, %.2f', [MyStationTopoDepart.aSerie, MyStationTopoDepart.aStation, QLongueur, QAzimut, QPente]))
        end
        else  // lignes de la forme: 2227.1                   0.00     0.00    0.00  [67]--> Ce sont des reprises d'une série (fonction 'Continuer ici' de PocketTopo)
        begin
          Result := errLnREPRISE_SERIE;
        end;
      end
      else // lignes de la forme 2221.2      2221.3       6.070   94.05   -0.79  [67] --> Ce sont des visées de cheminement à moyenner
      begin
        if ((QCurrentStationDepart.aSerie   = QPreviousStationArrivee.aSerie) AND
            (QCurrentStationDepart.aStation = QPreviousStationArrivee.aStation)) then
        begin
          (*
          2221.65     2221.66     16.320  110.00    3.60  [67]
          2221.12     2225.0       0.000    0.00    0.00
          2225.0      2225.1      13.200  281.11    3.59  [67]
          2225.1      2242.1       0.000    0.00    0.00
          2221.13     2227.0       0.000    0.00    0.00
          //*)
          if (QCurrentStationArrivee.aStation <> QPreviousStationArrivee.aStation) then
          begin
            if (GetSerieByNumeroSerie(QCurrentStationArrivee.aSerie, MySerie, QInternalIdx)) then
            begin
              WriteDebugMsg(NbLignesLues, Format('Visée à ajouter: %d.%d - %.2f %.2f %.2f', [MySerie.GetNumeroDeSerie(), QCurrentStationArrivee.aStation, QLongueur, QAzimut, QPente]));
              MySerie.AddVisee(0, 1, 1, tgDEFAULT, QLongueur, QAzimut, QPente, 1, 1, 1, 1, '', '');
              MySerie.SetSeriePtArr(QCurrentStationArrivee.aSerie, MySerie.GetNbVisees() - 1);//QCurrentStationArrivee.aStation);

            end;
          end;
        end;
      end;
      QPreviousStationDepart  := QCurrentStationDepart;
      QPreviousStationArrivee := QCurrentStationArrivee;
    end;
  end;
begin


  // On crée un nouveau code, et une nouvelle expé (ils seront les seuls)
  QDateSeanceTopo := Now();

  // Station courante: 1.0
  QCurrentStationDepart.eIdxNameSpace := 0;
  QCurrentStationDepart.aSerie     := 1;
  QCurrentStationDepart.aStation   := 0;
  QCurrentStationDepart.aIDTerrain := '';

  QCurrentStationArrivee.eIdxNameSpace := 0;
  QCurrentStationArrivee.aSerie     := 1;
  QCurrentStationArrivee.aStation   := 0;
  QCurrentStationArrivee.aIDTerrain := '';

  QPreviousStationDepart  := QCurrentStationDepart;
  QPreviousStationArrivee := QCurrentStationArrivee;

  // initialisation de tableau de visées à moyenner
  UneVisee.TimeStamp := Now();
  UneVisee.Device    := 0;
  //UneVisee.TypeViseeDistoX := tvdUNDEFINED;
  UneVisee.Longueur  := 0.00;
  UneVisee.Azimut    := 0.00;
  UneVisee.Pente     := 0.00;
  SetLength(QArrViseesAMoyenner, 0);
  try // try..finally niveau 0
     //*************************************************************************
     AfficherMessage(GetResourceString(rsSEPARATOR_LINE));
     // nom de la base
     SetDatabaseName(ExtractFileNameWithoutExt(FichierTXT)); // Laz 1.8: ExtractFileNameWithoutExt() remplace ExtractFileNameOnly()
     // Initialisation des listes
     ReInitialiser(True);
     // code et expé par défault
     AfficherMessageErreur(Format('%s: %s', [_accolade_ouvrante_ $I %FILE% _accolade_fermante_, _accolade_ouvrante_$I %LINE% _accolade_fermante_]));

     MyExpe := MakeTExpe(1, YearOf(QDateSeanceTopo), MonthOf(QDateSeanceTopo), DayOf(QDateSeanceTopo), 2, cdmAUTOMATIQUE, 0.00, 'Speleometre', 'Speleographe', 'Expe01');
     MyCode := MakeUsualTCode(1, 360.00, 360.00, 1.00, 0.01, 1.00, 1.00, 0.00, 0.00, 0.00, 'Topo du ' + DatePascalToDateSQL(QDateSeanceTopo));
     self.AddCode(MyCode);
     self.AddExpe(MyExpe);
     // et on les extrait
     MyCode := GetLastCode();
     MyExpe := GetLastExpe();
     WriteDebugMsg(-2559, Format('Code %d: %s', [MyCode.IDCode, MyCode.Commentaire]));
     WriteDebugMsg(-2560, Format('Expé %d: %s', [MyExpe.IDExpe, DatePascalToDateSQL(EncodeDate(MyExpe.AnneeExpe, MyExpe.MoisExpe, MyExpe.JourExpe))]));

     // code EPSG par défaut
     SetCodeEPSGSystemeCoordonnees(EPSGDefault);
     //*************************************************************************
     // début du véritable traitement
     // ouverture fichier Text
     WriteDebugMsg(-2518, 'Lecture de ' + FichierTXT);

     AfficherMessageErreur(Format('--> %d expés, %d codes, %d series, %d antennes', [GetNbExpes(), GetNbCodes(), GetNbSeries(), GetNbAntennes()]));
     NbLignesLues      := 0;
     NbErrorsInLoading := 0;
     IdxCourant := -1;
     AssignFile(pTXT, FichierTXT);
     try // niveau 1: pour le fichier Text
       Reset(pTXT);
       while (Not Eof(pTXT)) do   // lire tout le fichier
       begin
         try // traitement local d'exceptions dans la lecture des lignes
           ReadLn(pTXT, LigneTXT);  // /:\ Ne pas modifier la ligne avec la fonction Trim() - Faire les tests sur une copie
           Inc(NbLignesLues);
           // Traitement de la ligne 1 : doit être ignorée. TODO: extraire l'unité des instruments
           // Citon_IV_20200627   (m, 360)
           if (NbLignesLues = 1)    then Continue;
           QAT := QTraiterLigneDeDonnees(LigneTXT);
           case QAT of
             errLnNONE:
               pass;
             errLnEMPTY_LINE:
               WriteDebugMsg(NbLignesLues, 'Ligne vide, ignorée');
             errLnCOMMENT_LINE:
               WriteDebugMsg(NbLignesLues, 'Ligne de commentaires, ignorée');
             errLnIS_STATION_1_0:
               WriteDebugMsg(NbLignesLues, LigneTXT);
             errLnCHANGE_CURRENT_STATION:
               WriteDebugMsg(NbLignesLues, Format('Changement de station: --> %d.%d',[QCurrentStationDepart.aSerie, QCurrentStationDepart.aStation]));
             errLnINVALID_ID_STATION_DEP:
               pass;
             errLnINVALID_ST_ARR:
               pass;
             errLnSTATION_DETECTED:
               pass;
             errLnIGNORED:
               pass;
             errLnOTHER:
               pass;
             errLnREPRISE_SERIE:
               WriteDebugMsg(NbLignesLues, Format('Reprise de la série: %d.%d',[QCurrentStationDepart.aSerie, QCurrentStationDepart.aStation]));
             errLnSTOP_SIGNAL:
               begin
                 WriteDebugMsg(NbLignesLues, 'Signal d''arrêt rencontré');
                 Break;
               end;
           end;
           if (QAT in [errLnOTHER, errLnIGNORED, errLnEMPTY_LINE, errLnIS_STATION_1_0]) then Continue;
         except
           AfficherMessageErreur(Format('[%d] %s', [NbLignesLues, LigneTXT]));
         end;
       end;
       WriteDebugMsg(-2543, 'Lecture terminée');
       Result := 0;
     finally
       CloseFile(pTXT);
     end;

  finally
    pass;
  end;

end;
}
