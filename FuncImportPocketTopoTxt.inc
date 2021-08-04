//FuncImportPocketTopoTxt.inc
// charger un fichier PocketTopo TXT
function TToporobotStructure2012.LoadFromPocketTopoTXT(const FichierTXT: TStringDirectoryFilename): integer;  deprecated;
var
  EPSGDefault       : TLabelSystemesCoordsEPSG;
  EWE               : TGHStringArray;
  pTXT              : TextFile;
  LigneTXT          : String;
  //WU                : String;
  QNomEtude: string;
  p, i, j, Nb: integer;
  NbLignesLues, nbSeries, QInternalIdx: Integer;
  QDateSeanceTopo: TDateTime;
  MyExpe: TExpe;
  MyCode: TCode;
  ListeOfTGHStringArray: TListeOfTGHStringArray;
  QStationDepart0, QStationArrivee0: string;
  QStationDepart1, QStationArrivee1: string;

  MyAntenne: TViseeAntenne;
  MySerie: TObjSerie;
  MyVisee: TUneVisee;


  QStationDepart, QStationArrivee, QLaStationCourante: TToporobotIDStation;
  EWE0, EWE1: TGHStringArray;
  QNumeroDeSerieCourant : TNumeroSerie;
  MyEntrance: TEntrance;

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
  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;

  AfficherMessageErreur(Format('%s.LoadFromPocketTopoTXT: %s', [ClassName, FichierTXT]));
  // Fichier introuvable ? OK, On sort
  if (not FileExistsUTF8(FichierTXT)) then  Exit(-32);
  // nom étude par défaut = nom du fichier
  SetNomEtude(ExtractFileNameWithoutExt(FichierTXT));
  SetCodeEPSGSystemeCoordonnees(EPSGDefault);
  MyEntrance.eRefSer    := 1;
  MyEntrance.eRefSt     := 0;
  MyEntrance.eCouleur.setFrom(clBlue, 255);
  MyEntrance.eNomEntree := 'Main Entrance';
  MyEntrance.eIDTerrain := '1.0';
  MyEntrance.ePosition.Empty();
  MyEntrance.eObserv    := '';
  AddEntrance(MyEntrance);

  MyCode.setUsualParameters(1, 360.00, 360.00, 1.00, 0.01, 1.00, 1.00, 0.00, 0.00, 0.00, 'Topo du ' + DatePascalToDateSQL(QDateSeanceTopo));
  self.AddCode(MyCode);
  MyExpe.setFrom(1, YearOf(QDateSeanceTopo), MonthOf(QDateSeanceTopo), DayOf(QDateSeanceTopo), 6, cdmAUTOMATIQUE, 0.00, 'Speleometre', 'Speleographe', 'Expe01');
  self.AddExpe(MyExpe);
  AfficherMessageErreur(Format('*** %d entrances, %d reseaux, %d secteurs, %d codes, %d seances', [GetNbEntrances(), GetNbReseaux(), GetNbSecteurs(), GetNbCodes(), GetNbExpes()]));
  for i := 0 to GetNbCodes() - 1 do
  begin
    MyCode := GetCode(i);
    AfficherMessageErreur(Format('-- %d: Code: %d, UB: %.2f, UC: %.2f', [i, MyCode.IDCode, MyCode.GradAz, MyCode.GradInc]));
  end;
  for i := 0 to GetNbExpes() - 1 do
  begin
    MyExpe := GetExpe(i);
    AfficherMessageErreur(Format('-- %d: Expe: %d, %s, %s', [i, MyExpe.IDExpe, DatePascalToDateSQL(MyExpe.DateExpe), MyExpe.Commentaire]));
  end;
  MyCode := GetLastCode();
  MyExpe := GetLastExpe();
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
        if ('#' = LigneTXT[1])    then Continue; // dièse en tête           = commentaire
        if ('[' = LigneTXT[1])    then Continue; // crochet ouvrant en tête = ligne ignorée
        if (1 = pos('stop', LowerCase(LigneTXT))) then Break;  // signal d'arrêt du traitement à inclure dans le fichier
        //0        1         2         3         4         5         6         7         8         9         0
        //1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
        //     1.0              0,00          0,00      0,00
        EWE := SplitFixedColLine(LigneTXT, [1, 11, 23, 33, 41, 50, 58, 100]);
        ListeOfTGHStringArray.AddElement(EWE);
      end;
    finally
      CloseFile(pTXT);
    end;
    // contrôle et traitements
    Nb := ListeOfTGHStringArray.GetNbElements();
    AfficherMessageErreur(Format('==== %d', [Nb]));
    // On trie la liste en fonction de la première colonne (regroupement des séries)
    ListeOfTGHStringArray.SortByFirstColumn();
    // on extrait les antennes (lignes dont le champ 1 est vide)
    // et on vire les lignes concernées
    for i := Nb -1 downto 0 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      if ('' = Trim(EWE[1])) then ListeOfTGHStringArray.RemoveElement(i);
      QStationDepart.setFrom(EWE[0]);
      MyAntenne.EntranceRatt:= 0;
      MyAntenne.Reseau      := 0;
      MyAntenne.Secteur     := 0;
      MyAntenne.MarkedForDelete := false;

      MyAntenne.SerieDepart := QStationDepart.aSerie;
      MyAntenne.PtDepart    := QStationDepart.aStation;
      MyAntenne.setLongAzInc(EWE[2], EWE[3], EWE[4]);
      if (MyAntenne.Longueur > 0.00) then AddViseeAntenne(MyAntenne);
    end;
    AfficherMessageErreur(Format('==== %d antennes ajoutées', [GetNbAntennes()]));
    (*
    Nb := ListeOfTGHStringArray.GetNbElements();
    for i := 0 to Nb -1 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      AfficherMessageErreur(EWE[0] + '; ' + EWE[1] + '; ' + EWE[2] + '; ' + EWE[3] + '; ' + EWE[4] + '; ' +  EWE[5] + '; ');
    end;
    //*)
    CreateNewSerie(1, 0, 1, 'Galerie principale');
    nbSeries := GetNbSeries();
    AfficherMessageErreur(Format('%d séries', [nbSeries]));

    // les nouvelles séries = lignes de la forme
    //   363.33      386.0       0.000    0.00    0.00         "retrancher 26 deg aux azimuts"
    // On crée les séries et on jette les lignes concernées
    Nb := ListeOfTGHStringArray.GetNbElements();
    for i := Nb -1 downto 0 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      QStationDepart.setFrom(EWE[0]);
      QStationArrivee.setFrom(EWE[1]);
      if ((QStationArrivee.aSerie > 0) and (0 = QStationArrivee.aStation)) then
      begin
        CreateNewSerie(QStationDepart.aSerie, QStationDepart.aStation, QStationArrivee.aSerie, 'Depart en ' + Trim(EWE[0]));
        ListeOfTGHStringArray.RemoveElement(i);
      end;
    end;
    nbSeries := GetNbSeries();
    AfficherMessageErreur(Format('%d séries', [nbSeries]));
    SortSeries();
    (*
    for i := 0 to nbSeries -1 do
    begin
      MySerie := GetSerie(i);
      AfficherMessageErreur(Format('%d: %d: %d.%d -> %d.%d: %s', [i, MySerie.GetNumeroDeSerie(), MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), MySerie.GetNoSerieArr(), MySerie.GetNoPointArr(), MySerie.GetNomSerie()]));
    end;
    //*)
    // moyenner les visées
    Nb := ListeOfTGHStringArray.GetNbElements();
    // Ici, on supprime les lignes 'doublons' (ie: Colonnes 0 et 1 identiques)
    // sans moyenner les valeurs .
    // TODO: Moyenner les valeurs de ces lignes
    for i := Nb -1 downto 1 do
    begin
      EWE0 := ListeOfTGHStringArray.GetElement(i-1);
      EWE1 := ListeOfTGHStringArray.GetElement(i);

      QStationDepart0  := Trim(EWE0[0]); QStationArrivee0  := Trim(EWE0[1]);
      QStationDepart1  := Trim(EWE1[0]); QStationArrivee1  := Trim(EWE1[1]);
      if ((QStationDepart1 = QStationDepart0) and (QStationArrivee1 = QStationArrivee0)) then
      begin
        ListeOfTGHStringArray.RemoveElement(i);
      end;
    end;
    (*
    Nb := ListeOfTGHStringArray.GetNbElements();
    AfficherMessageErreur(Format('= Doublons supprimés === %d', [Nb]));
    for i := 0 to Nb -1 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      AfficherMessageErreur(EWE[0] + '; ' + EWE[1] + '; ' + EWE[2] + '; ' + EWE[3] + '; ' + EWE[4] + '; ' +  EWE[5] + '; ');
    end;
    //*)
    // remplissage des séries
    Nb := ListeOfTGHStringArray.GetNbElements();
    AfficherMessageErreur(Format('= Séries === %d', [Nb]));
    QNumeroDeSerieCourant := 0;

    for i := 0 to Nb - 1 do
    begin
      EWE := ListeOfTGHStringArray.GetElement(i);
      QLaStationCourante.setFrom(EWE[0]);
      if (QLaStationCourante.aSerie <> QNumeroDeSerieCourant) then
      begin
        QNumeroDeSerieCourant := QLaStationCourante.aSerie;
        GetSerieByNumeroSerie(QNumeroDeSerieCourant, MySerie, QInternalIdx);
      end;
      MyVisee.Empty('');
      MyVisee.setFrom(0,
                      tgDEFAULT, 0,
                      MyCode.IDCode, MyExpe.IDExpe,
                      ConvertirEnNombreReel(EWE[2], -1.00),
                      ConvertirEnNombreReel(EWE[3],  0.00),
                      ConvertirEnNombreReel(EWE[4],  0.00),
                      0.0, 0.0, 0.0, 0.0,
                      Now(),
                      '', StringReplace(Trim(EWE[6]), '"', '', [rfReplaceAll]));
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
      MySerie.SetChanceObstacle(0, 0);
      MySerie.SetNumeroEntrance(0);
      MySerie.SetNumeroReseau(0);
      MySerie.SetNomObsSerie('', '');
      AfficherMessageErreur(Format('%d: %d (%d pts): %d.%d -> %d.%d: %s', [i, MySerie.GetNumeroDeSerie(), MySerie.GetNbVisees(),MySerie.GetNoSerieDep(), MySerie.GetNoPointDep(), MySerie.GetNoSerieArr(), MySerie.GetNoPointArr(), MySerie.GetNomSerie()]));
      for j := 0 to MySerie.GetNbVisees() - 1 do
      begin
        MyVisee := MySerie.GetVisee(j);
        AfficherMessageErreur(Format('  -> %d: T=%d, S=%d, C=%d, E=%d - %.2f, %.2f, %.2f', [
                                     j, Ord(MyVisee.TypeVisee), MyVisee.IDSecteur, MyVisee.Code, MyVisee.Expe,
                                     MyVisee.Longueur, MyVisee.Azimut, MyVisee.Pente
                                     ]));
      end;
    end;
    ListeOfTGHStringArray.ClearListe();
    for i := 0 to GetNbCodes() - 1 do
    begin
      MyCode := GetCode(i);
      AfficherMessageErreur(Format('-- %d: Code: %d, UB: %.2f, UC: %.2f', [i, MyCode.IDCode, MyCode.GradAz, MyCode.GradInc]));
    end;
    for i := 0 to GetNbExpes() - 1 do
    begin
      MyExpe := GetExpe(i);
      AfficherMessageErreur(Format('-- %d: Expe: %d, %s, %s', [i, MyExpe.IDExpe, DatePascalToDateSQL(MyExpe.DateExpe), MyExpe.Commentaire]));
    end;
    //**************************
    Preconditionner(FichierTXT);
    //**************************
    Result := GetNbSeries();
  finally
    FreeAndNil(ListeOfTGHStringArray);
  end;
end;


