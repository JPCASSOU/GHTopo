// Fichier de déclaration des clés pour le format GHTopo XML GTX
const
  GTX_ATTR_COLOR_R     = 'ColorR';
  GTX_ATTR_COLOR_G     = 'ColorG';
  GTX_ATTR_COLOR_B     = 'ColorB';

  GTX_KEY_SECTION_GENERAL      = 'General';
   GTX_KEY_CAVITE               = 'Cavite';
    GTX_ATTR_NAMESPACE           = 'Namespace';
    GTX_ATTR_NOM_ETUDE           = 'FolderName';
    GTX_ATTR_COORDS_SYSTEM_NOM   = 'CoordsSystem';
    GTX_ATTR_COORDS_SYSTEM_EPSG  = 'CoordsSystemEPSG';
    GTX_ATTR_COMMENTAIRES_ETUDE  = 'FolderObservations';
   // Espaces de noms
  GTX_KEY_SECTION_NAMESPACES    = 'Namespaces';
    GTX_KEY_NAMESPACE            = 'Namespace';
     GTX_ATTR_NAMESPACE_NAME        = 'Name';
     GTX_ATTR_NAMESPACE_DESCRIPTION = 'Description';

  GTX_KEY_SECTION_FILTERS      = 'Filters';
    GTX_KEY_FILTER                = 'Filtre';
    GTX_ATTR_FILTER_IDX           = 'Numero';
    GTX_ATTR_FILTER_NAME          = 'Name';
    GTX_ATTR_FILTER_EXPRESSION    = 'Expression';
    GTX_ATTR_FILTER_DESCRIPTION   = 'Description';

  GTX_KEY_SECTION_ENTRANCES    = 'Entrances';
    GTX_KEY_ENTRANCE             = 'Entrance';
      GTX_ATTR_ENTRANCE_IDX      = 'Numero';
      GTX_ATTR_ENTRANCE_IDTERRAIN= 'IdTerrain';
      GTX_ATTR_ENTRANCE_NAME     = 'Name';
      GTX_ATTR_ENTRANCE_REFSERIE = 'RefSerie';
      GTX_ATTR_ENTRANCE_REFPT    = 'RefPoint';
      GTX_ATTR_ENTRANCE_X        = 'X';
      GTX_ATTR_ENTRANCE_Y        = 'Y';
      GTX_ATTR_ENTRANCE_Z        = 'Z';
      GTX_ATTR_ENTRANCE_COLOR    = 'Colour';
      GTX_ATTR_ENTRANCE_OBS      = 'Comments';

  GTX_KEY_SECTION_RESEAUX     = 'Networks';
    GTX_KEY_RESEAU            = 'Network';
      GTX_ATTR_RESEAU_IDX     = 'Numero';
      GTX_ATTR_RESEAU_TYPE    = 'Type';
      GTX_ATTR_RESEAU_NAME    = 'Name';
      GTX_ATTR_RESEAU_OBS     = 'Comments';

  GTX_KEY_SECTION_SECTEURS    = 'Secteurs';
    GTX_KEY_SECTEUR            = 'Secteur';
      GTX_ATTR_SECTEUR_IDX     = 'Numero';
      GTX_ATTR_SECTEUR_NAME    = 'Name';

  GTX_KEY_SECTION_CODES       = 'Codes';
    GTX_KEY_CODE              = 'Code';
      GTX_ATTR_CODE_NUMERO    = 'Numero';
      GTX_ATTR_CODE_UCLINO    = 'ClinoUnit';
      GTX_ATTR_CODE_UCOMPASS  = 'CompassUnit';
      GTX_ATTR_CODE_FACT_LONG = 'FactLong';
      GTX_ATTR_CODE_TYPE      = 'Type';
      GTX_ATTR_CODE_ANGLIMITE = 'AngleLimite';
      GTX_ATTR_CODE_PSI_L     = 'PsiL';
      GTX_ATTR_CODE_PSI_A     = 'PsiAz';
      GTX_ATTR_CODE_PSI_P     = 'PsiP';
      GTX_ATTR_CODE_OBS       = 'Comments';
      GTX_ATTR_CODE_ERROR_TOURILLON = 'ErrorTourillon';
      GTX_ATTR_CODE_DIAM_BOULE1     = 'DiamBoule1';
      GTX_ATTR_CODE_DIAM_BOULE2     = 'DiamBoule2';
      GTX_ATTR_CODE_FUNC_CORR_AZ_CO           = 'FuncCorrAzCo';
      GTX_ATTR_CODE_FUNC_CORR_AZ_ERR_MAX      = 'FuncCorrAzErrMax';
      GTX_ATTR_CODE_FUNC_CORR_AZ_POS_ERR_MAX  = 'FuncCorrAzPosErrMax';
      GTX_ATTR_CODE_FUNC_CORR_INC_CO          = 'FuncCorrIncCo';
      GTX_ATTR_CODE_FUNC_CORR_INC_ERR_MAX     = 'FuncCorrIncErrMax';
      GTX_ATTR_CODE_FUNC_CORR_INC_POS_ERR_MAX = 'FuncCorrIncPosErrMax';

  GTX_KEY_SECTION_EXPES       = 'Seances';
    GTX_KEY_EXPE              = 'Trip';
      GTX_ATTR_EXPE_NUMERO    = 'Numero';
      GTX_ATTR_EXPE_DATE      = 'Date';
      GTX_ATTR_EXPE_IDXCOLOR  = 'Color';
      GTX_ATTR_EXPE_SURVEY1   = 'Surveyor1';
      GTX_ATTR_EXPE_SURVEY2   = 'Surveyor2';
      GTX_ATTR_EXPE_DECLINAT  = 'Declination';
      GTX_ATTR_EXPE_INCLINAT  = 'Inclination';
      GTX_ATTR_EXPE_MODEDECL  = 'ModeDeclination';
      GTX_ATTR_EXPE_OBS       = 'Comments';

  GTX_KEY_SECTION_SERIES      = 'Series';
    GTX_KEY_SERIE             = 'Serie';
      GTX_ATTR_SERIE_Numero   = 'Numero';
      GTX_ATTR_SERIE_NAME     = 'Name';
      GTX_ATTR_SERIE_SERDEP   = 'SerDep';
      GTX_ATTR_SERIE_PTDEP    = 'PtDep';
      GTX_ATTR_SERIE_SERARR   = 'SerArr';
      GTX_ATTR_SERIE_PTARR    = 'PtArr';
      GTX_ATTR_SERIE_ENTRANCE = 'Entrance';
      GTX_ATTR_SERIE_RESEAU   = 'Network';
      GTX_ATTR_SERIE_OBSTACLE = 'Obstacle';
      GTX_ATTR_SERIE_CHANCE   = 'Chance';
      GTX_ATTR_SERIE_RAIDEUR  = 'Raideur';
      GTX_ATTR_SERIE_OBS      = 'Comments';
      GTX_ATTR_SERIE_COLOR    = 'Color';
      GTX_KEY_STATIONS        = 'Stations';
        GTX_KEY_VISEE         = 'Shot';
          GTX_ATTR_VISEE_ID   = 'ID';
          GTX_ATTR_VISEE_LBL  = 'Label';
          GTX_ATTR_VISEE_SECTEUR = GTX_KEY_SECTEUR;
          GTX_ATTR_VISEE_TYPE = 'TypeShot';
          GTX_ATTR_VISEE_CODE = GTX_KEY_CODE;
          GTX_ATTR_VISEE_EXPE = GTX_KEY_EXPE;
          GTX_ATTR_VISEE_LONG = 'Length';
          GTX_ATTR_VISEE_AZ   = 'Az';
          GTX_ATTR_VISEE_P    = 'Incl';
          GTX_ATTR_VISEE_LG   = 'Left';
          GTX_ATTR_VISEE_LD   = 'Right';
          GTX_ATTR_VISEE_HZ   = 'Up';
          GTX_ATTR_VISEE_HN   = 'Down';
          GTX_ATTR_VISEE_OBS  = 'Comments';

  GTX_KEY_SECTION_ANTENNAS    = 'AntennaShots';
    GTX_KEY_ANTENNA_SHOT      = 'AntennaShot';
    GTX_KEY_ANTENNA_NETWORK   = 'Network';
    GTX_KEY_ANTENNA_SECTEUR   = GTX_KEY_SECTEUR;
    GTX_KEY_ANTENNA_SERIE     = 'SerDep';
    GTX_KEY_ANTENNA_POINT     = 'PtDep';
    GTX_KEY_ANTENNA_LONG      = 'Length';
    GTX_KEY_ANTENNA_AZIMUT    = 'Az';
    GTX_KEY_ANTENNA_PENTE     = 'Incl';
//******************************************************************************
// FuncLoadSaveFichierXML.inc
// Chargement et sauvegarde des fichiers XML
function  TToporobotStructure2012.LoadFromXML(const FichierXML: TStringDirectoryFilename): integer;
var
  EPSGDefault: TLabelSystemesCoordsEPSG;
  MyDocXML   : TXMLDocument;
  GHTopoRoot : TDOMElement;
  // items de tables simples
  UnNameSpace           : TNameSpace;
  UnFiltrePerso         : TFiltrePersonnalise;
  UneEntree             : TEntrance;
  UnReseau              : TReseau;
  UnSecteur             : TSecteur;
  UneExpe               : TExpe;
  UnCode                : TCode;
  UneViseeAntenne       : TViseeAntenne;
  UneSerie              : TObjSerie;
  UneVisee              : TUneVisee;
  // noeuds
  EWE, WU, WOK, QAT     : TDOMNode;
  // listes DOM
  ListeRubriquesGeneral : TDOMNodeList;
  ListeDesFiltres       : TDOMNodeList;
  ListeDesEntrees       : TDOMNodeList;
  ListeDesReseaux       : TDOMNodeList;
  ListeDesSecteurs      : TDOMNodeList;
  ListeDesExpes         : TDOMNodeList;
  ListeDesCodes         : TDOMNodeList;
  ListeDesSeries        : TDOMNodeList;
  ListeDesAntennes      : TDOMNodeList;
  ListeStations         : TDOMNodeList;
  ListeNamespaces       : TDOMNodeList;
  QMyDate: TDateTime;
  St, i: Integer;
  qSerDepart, qPtDepart, qSerArrivee, qPtArrivee: Integer;
  qIdxSerie: TNumeroSerie;
  procedure KWYXZ(const S666: string); inline;
  begin
    AfficherMessage(Format('-- Found "%s" section', [S666]));
  end;
  function QExtractTextContent(var QQQ: TDOMNode): string; inline;
  begin
    Result := Trim(PasStrToXMLStr(QQQ.TextContent));
  end;

  function ExtractStringOfNode(var QN: TDOMNode; const XMLKey: string): string;
  var
    QIN1: TDOMNode;
  begin
    QIN1 := QN.Attributes.GetNamedItem(XMLKey);
    Result := QExtractTextContent(QIN1);
  end;
  function ExtractIntOfNode(var QN: TDOMNode; const XMLKey: string; const QDefaultValue: integer): integer;
  var
    QIN1: TDOMNode;
  begin
    QIN1 := QN.Attributes.GetNamedItem(XMLKey);
    Result := StrToIntDef(QIN1.TextContent, QDefaultValue);
  end;
  function ExtractByteOfNode(var QN: TDOMNode; const XMLKey: string; const QDefaultValue: byte): byte;
  var
    QIN1: TDOMNode;
  begin
    QIN1 := QN.Attributes.GetNamedItem(XMLKey);
    Result := StrToIntDef(QIN1.TextContent, QDefaultValue);
  end;
  function ExtractRealOfNode(var QN: TDOMNode; const XMLKey: string; const QDefaultValue: double): double;
  var
    QIN1: TDOMNode;
  begin
    QIN1 := QN.Attributes.GetNamedItem(XMLKey);
    Result := ConvertirEnNombreReel(QIN1.TextContent, QDefaultValue);
  end;
  function ExtractColorOfNode(var QN: TDOMNode): TColor;
  var
    qR, qG, qB: Byte;
  begin
    qR := ExtractByteOfNode(QN, GTX_ATTR_COLOR_R, 255);
    qG := ExtractByteOfNode(QN, GTX_ATTR_COLOR_G, 255);
    qB := ExtractByteOfNode(QN, GTX_ATTR_COLOR_B, 255);
    Result := RGBToColor(qR, qG, qB);
  end;
begin
  Result := -1;
  AfficherMemoryUsage();
  AfficherMessage(Format('%s.LoadFromXML(%s)', [ClassName, FichierXML]));
   // nom étude par défaut
  SetNomEtude('Etude001');
  SetCommentairesEtude('Issued from XML format');
  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;

  SetCodeEPSGSystemeCoordonnees(EPSGDefault);
  // réinit du document
  self.ReInitialiser(True);
  MyDocXML := TXMLDocument.Create;
  try
    ReadXMLFile(MyDocXML, FichierXML);
    GHTopoRoot := MyDocXML.DocumentElement;
    // section General
    AfficherMessage('-> Reading section: ' + GTX_KEY_SECTION_GENERAL);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_GENERAL);
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    //ShowMessageFmt('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
    if (EWE <> Nil) then
    begin
      ListeRubriquesGeneral := EWE.ChildNodes;
      WU := ListeRubriquesGeneral.Item[0];
      SetNomEtude(ExtractStringOfNode(WU, GTX_ATTR_NOM_ETUDE));
      EPSGDefault.CodeEPSG := ExtractIntOfNode(WU, GTX_ATTR_COORDS_SYSTEM_EPSG, DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);
      EPSGDefault.NomEPSG  := ExtractStringOfNode(WU, GTX_ATTR_COORDS_SYSTEM_NOM);
      SetCodeEPSGSystemeCoordonnees(EPSGDefault);
      SetCommentairesEtude(ExtractStringOfNode(WU, GTX_ATTR_COMMENTAIRES_ETUDE));
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // espaces de noms
    AfficherMessage('-> Reading section: ' + GTX_KEY_SECTION_NAMESPACES);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_NAMESPACES);
    if (EWE <> Nil) then
    begin
      ListeNamespaces := EWE.ChildNodes;
      for i := 0 to ListeNamespaces.Count - 1 do
      begin
        try
          WU := ListeNamespaces.Item[i];
          UnNameSpace.Couleur      := ExtractColorOfNode(WU);
          UnNameSpace.Nom          := ExtractStringOfNode(WU, GTX_ATTR_NAMESPACE_NAME);
          UnNameSpace.Description  := ExtractStringOfNode(WU, GTX_ATTR_NAMESPACE_DESCRIPTION);
          self.AddNameSpace(UnNameSpace);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // filtres personnalisés
    AfficherMessage('-> Reading section: ' + GTX_KEY_SECTION_FILTERS);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_FILTERS);
    if (EWE <> Nil) then
    begin
      ListeDesFiltres := EWE.ChildNodes;
      for i := 0 to ListeDesFiltres.Count - 1 do
      begin
        try
          WU := ListeDesFiltres.Item[i];
          UnFiltrePerso.NomFiltre      := ExtractStringOfNode(WU, GTX_ATTR_FILTER_NAME);
          UnFiltrePerso.CouleurFiltre  := ExtractColorOfNode(WU);
          UnFiltrePerso.Expression     := ExtractStringOfNode(WU, GTX_ATTR_FILTER_EXPRESSION);
          UnFiltrePerso.Description    := ExtractStringOfNode(WU, GTX_ATTR_FILTER_DESCRIPTION);
          self.AddFiltrePerso(UnFiltrePerso);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les entrées
    AfficherMessage('-> Reading section: ' + GTX_KEY_SECTION_ENTRANCES);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_ENTRANCES);
    if (EWE <> Nil) then
    begin
      ListeDesEntrees := EWE.ChildNodes;
      for i := 0 to ListeDesEntrees.Count - 1 do
      begin
        try
          WU := ListeDesEntrees.Item[i];
          UneEntree.eIDTerrain  := ExtractStringOfNode(WU, GTX_ATTR_ENTRANCE_IDTERRAIN);
          UneEntree.eNomEntree  := ExtractStringOfNode(WU, GTX_ATTR_ENTRANCE_NAME);
          UneEntree.eRefSer     := ExtractIntOfNode(WU, GTX_ATTR_ENTRANCE_REFSERIE, 1);
          UneEntree.eRefSt      := ExtractIntOfNode(WU, GTX_ATTR_ENTRANCE_REFPT, 0);
          UneEntree.ePosition.setFrom(ExtractRealOfNode(WU, GTX_ATTR_ENTRANCE_X, -1.00),
                                      ExtractRealOfNode(WU, GTX_ATTR_ENTRANCE_Y, -1.00),
                                      ExtractRealOfNode(WU, GTX_ATTR_ENTRANCE_Z, -1.00));
          UneEntree.eCouleur    := GHTopoColorToColorDef(ExtractStringOfNode(WU, GTX_ATTR_ENTRANCE_COLOR), clBlue);
          UneEntree.eObserv     := ExtractStringOfNode(WU, GTX_ATTR_ENTRANCE_OBS);
          self.AddEntrance(UneEntree);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les réseaux
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_RESEAUX);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_RESEAUX);
    if (EWE <> Nil) then
    begin
      ListeDesReseaux := EWE.ChildNodes;
      for i := 0 to ListeDesReseaux.Count - 1 do
      begin
        try
          WU := ListeDesReseaux.Item[i];
          UnReseau.ColorReseau := ExtractColorOfNode(WU);
          UnReseau.TypeReseau  := ExtractIntOfNode(WU, GTX_ATTR_RESEAU_TYPE, 0);
          UnReseau.NomReseau   := ExtractStringOfNode(WU, GTX_ATTR_RESEAU_NAME);
          UnReseau.ObsReseau   := ExtractStringOfNode(WU, GTX_ATTR_RESEAU_OBS);
          self.AddReseau(UnReseau);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    //ShowMessageFmt('%s: %s', [{$I %FILE%}, {$I %LINE%}]);
    // lister les secteurs
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_SECTEURS);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_SECTEURS);
    if (EWE <> Nil) then
    begin
      ListeDesSecteurs := EWE.ChildNodes;
      for i := 0 to ListeDesSecteurs.Count - 1 do
      begin
        try
          WU := ListeDesSecteurs.Item[i];
          UnSecteur.CouleurSecteur := ExtractColorOfNode(WU);
          UnSecteur.NomSecteur     := ExtractStringOfNode(WU, GTX_ATTR_SECTEUR_NAME);
          self.AddSecteur(UnSecteur);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les codes
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_CODES);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_CODES);
    if (EWE <> Nil) then
    begin
      ListeDesCodes := EWE.ChildNodes;
      for i := 0 to ListeDesCodes.Count - 1 do
      begin
        try
          WU := ListeDesCodes.Item[i];
          UnCode.IDCode                      := ExtractIntOfNode(WU, GTX_ATTR_CODE_NUMERO, 0);
          UnCode.FactLong                    := ExtractRealOfNode(WU, GTX_ATTR_CODE_FACT_LONG, 1.00);
          UnCode.GradAz                      := ExtractRealOfNode(WU, GTX_ATTR_CODE_UCOMPASS, 360.00);
          UnCode.GradInc                     := ExtractRealOfNode(WU, GTX_ATTR_CODE_UCLINO, 360.00);
          UnCode.AngLimite                   := ExtractRealOfNode(WU, GTX_ATTR_CODE_ANGLIMITE, 0.00);
          UnCode.PsiL                        := ExtractRealOfNode(WU, GTX_ATTR_CODE_PSI_L, 0.01);
          UnCode.PsiAz                       := ExtractRealOfNode(WU, GTX_ATTR_CODE_PSI_A, 0.01);
          UnCode.PsiP                        := ExtractRealOfNode(WU, GTX_ATTR_CODE_PSI_P, 0.01);
          UnCode.ErreurTourillon             := ExtractRealOfNode(WU, GTX_ATTR_CODE_ERROR_TOURILLON, 0.00);
          UnCode.DiametreBoule1              := ExtractRealOfNode(WU, GTX_ATTR_CODE_DIAM_BOULE1, 0.00);
          UnCode.DiametreBoule2              := ExtractRealOfNode(WU, GTX_ATTR_CODE_DIAM_BOULE2, 0.00);
          UnCode.ParamsFuncCorrAz.Co         := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_AZ_CO          , 0.00);
          UnCode.ParamsFuncCorrAz.ErreurMax  := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_AZ_ERR_MAX     , 0.00);
          UnCode.ParamsFuncCorrAz.PosErrMax  := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_AZ_POS_ERR_MAX , 0.00);
          UnCode.ParamsFuncCorrInc.Co        := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_INC_CO         , 0.00);
          UnCode.ParamsFuncCorrInc.ErreurMax := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_INC_ERR_MAX    , 0.00);
          UnCode.ParamsFuncCorrInc.PosErrMax := ExtractRealOfNode(WU, GTX_ATTR_CODE_FUNC_CORR_INC_POS_ERR_MAX, 0.00);
          UnCode.Commentaire   := ExtractStringOfNode(WU, GTX_ATTR_CODE_OBS);
          self.AddCode(UnCode);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    //ShowMessageFmt('%s: %s', [{$I %FILE%}, {$I %LINE%}]);

    // lister les expés
    AfficherMessage('======================');
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_EXPES);
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_EXPES);
    if (EWE <> Nil) then
    begin
      ListeDesExpes := EWE.ChildNodes;
      for i := 0 to ListeDesExpes.Count - 1 do
      begin
        try
          WU := ListeDesExpes.Item[i];
          UneExpe.IDExpe        := ExtractIntOfNode(WU, GTX_ATTR_EXPE_NUMERO, 0);
          {$WARNING: TEXpe.DateExpe à implementer}
          //UneExpe.DateExpe := ;
          QMyDate               := DateSQLToDatePascal(ExtractStringOfNode(WU, GTX_ATTR_EXPE_DATE));

          UneExpe.AnneeExpe     := YearOf(QMyDate);
          UneExpe.MoisExpe      := MonthOf(QMyDate);
          UneExpe.JourExpe      := DayOf(QMyDate);
          UneExpe.IdxCouleur    := ExtractIntOfNode(WU, GTX_ATTR_EXPE_IDXCOLOR, 0);
          UneExpe.Operateur     := ExtractStringOfNode(WU, GTX_ATTR_EXPE_SURVEY1);
          UneExpe.ClubSpeleo    := ExtractStringOfNode(WU, GTX_ATTR_EXPE_SURVEY2);
          UneExpe.ModeDecl      := TModeCalculDeclimag(ExtractIntOfNode(WU, GTX_ATTR_EXPE_MODEDECL, 0));
          UneExpe.DeclinaisonInDegrees := ExtractRealOfNode(WU, GTX_ATTR_EXPE_DECLINAT, 0.00);
          UneExpe.Commentaire   := ExtractStringOfNode(WU, GTX_ATTR_EXPE_OBS);
          self.AddExpe(UneExpe);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les séries
    AfficherMessage('-> Read section: ' + GTX_KEY_SECTION_SERIES);
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_SERIES);
    if (EWE <> Nil) then
    begin
      ListeDesSeries := EWE.ChildNodes;
      AfficherMessage(Format('%d séries', [ListeDesSeries.Count]));
      for i := 0 to ListeDesSeries.Count - 1 do
      begin
        UneSerie := TObjSerie.Create;
        try
          UneSerie.ClearStations;
          WU  := ListeDesSeries.Item[i];
          qIdxSerie    := TNumeroSerie(ExtractIntOfNode(WU, GTX_ATTR_SERIE_Numero, 0));
          UneSerie.SetNumeroSerie(qIdxSerie);
          UneSerie.SetNomSerie(ExtractStringOfNode(WU, GTX_ATTR_SERIE_NAME));
          UneSerie.SetObsSerie(ExtractStringOfNode(WU, GTX_ATTR_SERIE_OBS));
          //AfficherMessageErreur(Format('%s: %s - %s', [{$I %FILE%}, {$I %LINE%}, UneSerie.GetNomSerie()]));
          qSerDepart  := ExtractIntOfNode(WU, GTX_ATTR_SERIE_SERDEP, 0);

          qPtDepart   := ExtractIntOfNode(WU, GTX_ATTR_SERIE_PTDEP , 0);
          qSerArrivee := ExtractIntOfNode(WU, GTX_ATTR_SERIE_SERARR, 0);
          qPtArrivee  := ExtractIntOfNode(WU, GTX_ATTR_SERIE_PTARR , 0);
          UneSerie.SetSeriePtExtremites(qSerDepart, qPtDepart, qSerArrivee, qPtArrivee);
          UneSerie.SetChance(ExtractIntOfNode(WU, GTX_ATTR_SERIE_CHANCE, 0));
          UneSerie.SetObstacle(ExtractIntOfNode(WU, GTX_ATTR_SERIE_OBSTACLE, 0));
          UneSerie.SetNumeroEntrance(TNumeroEntrance(ExtractIntOfNode(WU, GTX_ATTR_SERIE_ENTRANCE, 0)));
          UneSerie.SetNumeroReseau(TNumeroReseau(ExtractIntOfNode(WU, GTX_ATTR_SERIE_RESEAU, 0)));
          UneSerie.SetRaideur(ExtractRealOfNode(WU, GTX_ATTR_SERIE_RAIDEUR, 1.00));
          UneSerie.SetCouleur(clBlue);

          // les stations
          QAT := WU.FindNode(GTX_KEY_STATIONS);
          if (QAT <> NIL) then
          begin
            ListeStations := QAT.ChildNodes;
            //AfficherMessage(Format('Série %d %s - %d stations', [UneSerie.GetIndexSerie, UneSerie.GetNomSerie, ListeStations.Count]));
            for St := 0 to ListeStations.Count - 1 do
            begin
              try
                WOK := ListeStations.Item[St];
                UneVisee.NoVisee := St;
                UneVisee.IDTerrainStation := ExtractStringOfNode(WOK, GTX_ATTR_VISEE_LBL);
                UneVisee.IDSecteur        := ExtractIntOfNode(WOK, GTX_ATTR_VISEE_SECTEUR, 0);
                UneVisee.TypeVisee        := TTypeDeVisee(ExtractIntOfNode(WOK, GTX_ATTR_VISEE_TYPE, 0));

                UneVisee.Code             := TNumeroCode(ExtractIntOfNode(WOK, GTX_ATTR_VISEE_CODE, 0));
                UneVisee.Expe             := TNumeroExpe(ExtractIntOfNode(WOK, GTX_ATTR_VISEE_EXPE, 0));


                UneVisee.Longueur         := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_LONG, 0.00);
                UneVisee.Azimut           := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_AZ  , 0.00);
                UneVisee.Pente            := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_P   , 0.00);

                UneVisee.LG               := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_LG  , 0.00);
                UneVisee.LD               := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_LD  , 0.00);
                UneVisee.HZ               := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_HZ  , 0.00);
                UneVisee.HN               := ExtractRealOfNode(WOK, GTX_ATTR_VISEE_HN  , 0.00);

                UneVisee.Commentaires     := ExtractStringOfNode(WOK, GTX_ATTR_VISEE_OBS);

                UneSerie.AddVisee(UneVisee);
              except
              end;
            end; // for stations
            self.AddSerie(UneSerie);
          end;
        except // try séries
        end;
      end;  // for séries
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    // lister les antennes
    EWE := GHTopoRoot.FindNode(GTX_KEY_SECTION_ANTENNAS);
    if (EWE <> Nil) then
    begin
      ListeDesAntennes := EWE.ChildNodes;
      for i := 0 to ListeDesAntennes.Count - 1 do
      begin
        try
          if (i mod 2000 = 0) then AfficherMessage(Format('Antenne %d of %d', [i, ListeDesAntennes.Count]), false);
          WU := ListeDesAntennes.Item[i];
          //UneViseeAntenne.IDTerrainStation   := ExtractStringOfNode(WU, GTX_KEY_ANTENNA_LABEL);
          UneViseeAntenne.Reseau             := ExtractIntOfNode(WU, GTX_KEY_ANTENNA_NETWORK  , 0);
          UneViseeAntenne.Secteur            := ExtractIntOfNode(WU, GTX_KEY_ANTENNA_SECTEUR  , 0);
          UneViseeAntenne.SerieDepart        := ExtractIntOfNode(WU, GTX_KEY_ANTENNA_SERIE    , 0);
          UneViseeAntenne.PtDepart           := ExtractIntOfNode(WU, GTX_KEY_ANTENNA_POINT    , 0);
          UneViseeAntenne.Longueur           := ExtractRealOfNode(WU, GTX_KEY_ANTENNA_LONG, 0.00);
          UneViseeAntenne.Azimut             := ExtractRealOfNode(WU, GTX_KEY_ANTENNA_AZIMUT, 0.00);
          UneViseeAntenne.Pente              := ExtractRealOfNode(WU, GTX_KEY_ANTENNA_PENTE, 0.00);
          //UneViseeAntenne.Commentaires       := ExtractStringOfNode(WU, GTX_KEY_ANTENNA_OBS);
          UneViseeAntenne.MarkedForDelete    := false;
          self.AddViseeAntenne(UneViseeAntenne);
        except
        end;
      end;
    end;
    AfficherMessageErreur(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
    //**************************
    Preconditionner(FichierXML);
    //****************************
    Result := self.GetNbSeries();
  finally
    FreeAndNil(MyDocXML);//MyDocXML.Free;
  end;
end;
//******************************************************************************
procedure TToporobotStructure2012.SaveToXML(const FichierXML: TStringDirectoryFilename);
const
  IDX_MINI = 1;
var
  i: Integer;
  MyDocXML          : TXMLDocument;
  GHTopoRoot        : TDOMElement;
  SectionGeneral    : TDOMElement;
  SectionNamespaces : TDOMElement;
  SectionFiltres    : TDOMElement;
  SectionEntrances, SectionReseaux, SectionSecteurs   : TDOMElement;
  SectionCodes, SectionSeances                        : TDOMElement;
  SubSection, SubSubSection, SubSubSubSection         : TDOMElement;
  SectionSeries         : TDOMElement;
  SectionAntennes       : TDOMElement;
  E  : TEntrance;
  R  : TReseau;
  SC : TSecteur;
  C  : TCode;
  EX : TExpe;
  SER: TObjSerie;
  ST : TUneVisee;
  ANT: TViseeAntenne;
  EPSG: TLabelSystemesCoordsEPSG;
  //NV: TReleaseNotes;
  FF: TFiltrePersonnalise;
  S, QNbAntennes, QNbFiltres, QNbNameSpaces: Integer;
  SS: TNameSpace;
  procedure KWYXZ(const S666: string);
  begin
    AfficherMessage(Format('-- Writing "%s" section', [S666]));
  end;
begin
  AfficherMessage(Format('%s.SaveToXML(%s)', [ClassName, FichierXML]));
  MyDocXML := TXMLDocument.Create;
  try
    // racine du document = nom du logiciel
    GHTopoRoot := MyDocXML.CreateElement('GHTopo');
    MyDocXML.AppendChild(GHTopoRoot);
    // section Général
    SectionGeneral := MyDocXML.CreateElement(GTX_KEY_SECTION_GENERAL);
      GHTopoRoot.AppendChild(SectionGeneral);
      SubSection := MyDocXML.CreateElement(GTX_KEY_CAVITE);
      //SubSection.SetAttribute(GTX_ATTR_NAMESPACE, PasStrToXMLStr(GetNameSpace()));
      SubSection.SetAttribute(GTX_ATTR_NOM_ETUDE, PasStrToXMLStr(GetNomEtude()));
      EPSG := GetCodeEPSGSystemeCoordonnees;
      SubSection.SetAttribute(GTX_ATTR_COORDS_SYSTEM_EPSG, Format(FORMAT_NB_INTEGER, [EPSG.CodeEPSG]));
      SubSection.SetAttribute(GTX_ATTR_COORDS_SYSTEM_NOM, PasStrToXMLStr(EPSG.NomEPSG));
      SubSection.SetAttribute(GTX_ATTR_COMMENTAIRES_ETUDE, PasStrToXMLStr(GetCommentairesEtude));
      SectionGeneral.AppendChild(SubSection);
    // section Espaces de noms
    QNbNameSpaces := self.GetNbNameSpaces();
    if (QNbNameSpaces > 0) then
    begin
      SectionNamespaces := MyDocXML.CreateElement(GTX_KEY_SECTION_NAMESPACES);
      GHTopoRoot.AppendChild(SectionNamespaces);
      KWYXZ(GTX_KEY_SECTION_NAMESPACES);
      for i := 0 to QNbNameSpaces - 1 do
      begin
        SS := GetNameSpace(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_NAMESPACE);
          SubSection.SetAttribute(GTX_ATTR_NAMESPACE             , Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_NAMESPACE_NAME        , PasStrToXMLStr(SS.Nom));
          SubSection.SetAttribute(GTX_ATTR_COLOR_R     , Format(FORMAT_NB_INTEGER, [Red(SS.Couleur)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_G     , Format(FORMAT_NB_INTEGER, [Green(SS.Couleur)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_B     , Format(FORMAT_NB_INTEGER, [Blue(SS.Couleur)]));
          SubSection.SetAttribute(GTX_ATTR_NAMESPACE_DESCRIPTION , PasStrToXMLStr(SS.Description));
        SectionNamespaces.AppendChild(SubSection);
      end;
    end;

    // section Filtres perso
    QNbFiltres := self.GetNbFiltresPersos();
    if (QNbFiltres > 0) then
    begin
      SectionFiltres := MyDocXML.CreateElement(GTX_KEY_SECTION_FILTERS);
      GHTopoRoot.AppendChild(SectionFiltres);
      KWYXZ(GTX_KEY_SECTION_FILTERS);
      for i := 0 to QNbFiltres - 1 do
      begin
        FF := GetFiltrePerso(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_FILTER);
          SubSection.SetAttribute(GTX_ATTR_FILTER_IDX        , Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_FILTER_NAME       , PasStrToXMLStr(FF.NomFiltre));
          SubSection.SetAttribute(GTX_ATTR_COLOR_R           , Format(FORMAT_NB_INTEGER, [Red(FF.CouleurFiltre)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_G           , Format(FORMAT_NB_INTEGER, [Green(FF.CouleurFiltre)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_B           , Format(FORMAT_NB_INTEGER, [Blue(FF.CouleurFiltre)]));
          SubSection.SetAttribute(GTX_ATTR_FILTER_EXPRESSION , PasStrToXMLStr(FF.Expression));
          SubSection.SetAttribute(GTX_ATTR_FILTER_DESCRIPTION, PasStrToXMLStr(FF.Description));
        SectionFiltres.AppendChild(SubSection);
      end;
    end;
    // section Entrées
    SectionEntrances := MyDocXML.CreateElement(GTX_KEY_SECTION_ENTRANCES);
      GHTopoRoot.AppendChild(SectionEntrances);
      KWYXZ(GTX_KEY_SECTION_ENTRANCES);
      for i:= 0 to GetNbEntrances() - 1 do
      begin
        E := GetEntrance(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_ENTRANCE);
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_IDX      , Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_IDTERRAIN, Format(FORMAT_STRING, [PasStrToXMLStr(E.eIDTerrain)]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_NAME     , Format(FORMAT_STRING, [PasStrToXMLStr(E.eNomEntree)]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_REFSERIE , Format(FORMAT_NB_INTEGER, [E.eRefSer]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_REFPT    , Format(FORMAT_NB_INTEGER, [E.eRefSt]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_X, Format(FORMAT_NB_REAL_3_DEC, [E.ePosition.X]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_Y, Format(FORMAT_NB_REAL_3_DEC, [E.ePosition.Y]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_Z, Format(FORMAT_NB_REAL_3_DEC, [E.ePosition.Z]));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_COLOR, ColorToGHTopoColor(E.eCouleur));
          SubSection.SetAttribute(GTX_ATTR_ENTRANCE_OBS, PasStrToXMLStr(E.eObserv));
        SectionEntrances.AppendChild(SubSection);
      end;
    // section Réseaux (5 champs sur 5)
    SectionReseaux := MyDocXML.CreateElement(GTX_KEY_SECTION_RESEAUX);
      KWYXZ(GTX_KEY_SECTION_RESEAUX);
      GHTopoRoot.AppendChild(SectionReseaux);
      for i := IDX_MINI to GetNbReseaux - 1 do
      begin
        R := GetReseau(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_RESEAU);
          SubSection.SetAttribute(GTX_ATTR_RESEAU_IDX, Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_R, Format(FORMAT_NB_INTEGER, [Red(R.ColorReseau)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_G, Format(FORMAT_NB_INTEGER, [Green(R.ColorReseau)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_B, Format(FORMAT_NB_INTEGER, [Blue(R.ColorReseau)]));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_TYPE, Format(FORMAT_NB_INTEGER, [R.TypeReseau]));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_NAME,  PasStrToXMLStr(R.NomReseau));
          SubSection.SetAttribute(GTX_ATTR_RESEAU_OBS,  PasStrToXMLStr(R.ObsReseau));
        SectionReseaux.AppendChild(SubSection);
      end;
    // section Secteurs (5 champs sur 5)
    SectionSecteurs := MyDocXML.CreateElement(GTX_KEY_SECTION_SECTEURS);
      KWYXZ(GTX_KEY_SECTION_SECTEURS);
      GHTopoRoot.AppendChild(SectionSecteurs);
      for i := IDX_MINI to GetNbSecteurs - 1 do
      begin
        SC := GetSecteur(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_SECTEUR);
          SubSection.SetAttribute(GTX_ATTR_SECTEUR_IDX    , Format(FORMAT_NB_INTEGER, [i]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_R, Format(FORMAT_NB_INTEGER, [Red(SC.CouleurSecteur)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_G, Format(FORMAT_NB_INTEGER, [Green(SC.CouleurSecteur)]));
          SubSection.SetAttribute(GTX_ATTR_COLOR_B, Format(FORMAT_NB_INTEGER, [Blue(SC.CouleurSecteur)]));
          SubSection.SetAttribute(GTX_ATTR_SECTEUR_NAME   , PasStrToXMLStr(SC.NomSecteur));
        SectionSecteurs.AppendChild(SubSection);
      end;

    // section Codes -- Champs OK
    SectionCodes := MyDocXML.CreateElement(GTX_KEY_SECTION_CODES);
      KWYXZ(GTX_KEY_SECTION_CODES);
      GHTopoRoot.AppendChild(SectionCodes);
      for i := IDX_MINI to GetNbCodes - 1 do
      begin
        C := GetCode(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_CODE);
          SubSection.SetAttribute(GTX_ATTR_CODE_NUMERO                     , Format(FORMAT_NB_INTEGER, [C.IDCode]));                   //  IDCode
          SubSection.SetAttribute(GTX_ATTR_CODE_TYPE                       , Format(FORMAT_NB_INTEGER, [0])); //C.ReservedInt]));              //  ReservedInt
          // TODO: Support de FactLong a valider
          SubSection.SetAttribute(GTX_ATTR_CODE_FACT_LONG                  , Format(FORMAT_NB_REAL_3_DEC, [1.00])); ///// SubSection.SetAttribute(GTX_ATTR_CODE_FACT_LONG     , Format(FORMAT_NB_REAL_3_DEC, [C.FactLong]));               //  FactLong    : double; // pour compatibilité ascendante
          SubSection.SetAttribute(GTX_ATTR_CODE_UCOMPASS                   , Format(FORMAT_NB_REAL_2_DEC, [C.GradAz]));                 //  GradAz
          SubSection.SetAttribute(GTX_ATTR_CODE_UCLINO                     , Format(FORMAT_NB_REAL_2_DEC, [C.GradInc]));                //  GradInc     : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_ANGLIMITE                  , Format(FORMAT_NB_REAL_2_DEC, [C.AngLimite]));              //  AngLimite   : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_L                      , Format(FORMAT_NB_REAL_3_DEC, [C.PsiL]));                   //  PsiL        : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_A                      , Format(FORMAT_NB_REAL_3_DEC, [C.PsiAz]));                  //  PsiAz       : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_PSI_P                      , Format(FORMAT_NB_REAL_3_DEC, [C.PsiP]));                   //  PsiP        : double;
          SubSection.SetAttribute(GTX_ATTR_CODE_ERROR_TOURILLON            , Format(FORMAT_NB_REAL_3_DEC, [C.ErreurTourillon]));
          SubSection.SetAttribute(GTX_ATTR_CODE_DIAM_BOULE1                , Format(FORMAT_NB_REAL_3_DEC, [C.DiametreBoule1]));
          SubSection.SetAttribute(GTX_ATTR_CODE_DIAM_BOULE2                , Format(FORMAT_NB_REAL_3_DEC, [C.DiametreBoule1]));


          SubSection.SetAttribute(GTX_ATTR_CODE_OBS                        , PasStrToXMLStr(C.Commentaire));                              //  Commentaire : string;
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_AZ_CO            , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrAz.Co]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_AZ_ERR_MAX       , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrAz.ErreurMax]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_AZ_POS_ERR_MAX   , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrAz.PosErrMax]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_INC_CO           , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrInc.Co]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_INC_ERR_MAX      , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrInc.ErreurMax]));
          SubSection.SetAttribute(GTX_ATTR_CODE_FUNC_CORR_INC_POS_ERR_MAX  , Format(FORMAT_NB_REAL_6_DEC, [C.ParamsFuncCorrInc.PosErrMax]));

        SectionCodes.AppendChild(SubSection);
      end;
    // section Expés - Champs OK
    SectionSeances := MyDocXML.CreateElement(GTX_KEY_SECTION_EXPES);
      KWYXZ(GTX_KEY_SECTION_EXPES);
      GHTopoRoot.AppendChild(SectionSeances);
      for i := IDX_MINI to GetNbExpes - 1 do
      begin
        EX := GetExpe(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_EXPE);
          SubSection.SetAttribute(GTX_ATTR_EXPE_NUMERO   , Format(FORMAT_NB_INTEGER   , [EX.IDExpe]));                                             // IDExpe      : integer;
          SubSection.SetAttribute(GTX_ATTR_EXPE_DATE     , DateYYYYMMDDToDateSQL(EX.AnneeExpe, EX.MoisExpe, EX.JourExpe));    // JourExpe    : integer;MoisExpe    : integer;AnneeExpe   : integer;
          SubSection.SetAttribute(GTX_ATTR_EXPE_IDXCOLOR , Format(FORMAT_NB_INTEGER   , [EX.IdxCouleur]));                                            // //Couleur     : Integer;
          SubSection.SetAttribute(GTX_ATTR_EXPE_DECLINAT , Format(FORMAT_NB_REAL_6_DEC, [EX.DeclinaisonInDegrees]));                                      // Declinaison : double;
          SubSection.SetAttribute(GTX_ATTR_EXPE_MODEDECL , Format(FORMAT_NB_INTEGER   , [Ord(EX.ModeDecl)]));                                           // ModeDecl    : integer; // deprecated;
          SubSection.SetAttribute(GTX_ATTR_EXPE_INCLINAT , Format(FORMAT_NB_REAL_6_DEC, [0.00])); //EX.Inclinaison]));                                      //  Inclinaison : double;
          SubSection.SetAttribute(GTX_ATTR_EXPE_SURVEY1  , PasStrToXMLStr(EX.Operateur));                                                          // Speleometre : String;
          SubSection.SetAttribute(GTX_ATTR_EXPE_SURVEY2  , PasStrToXMLStr(EX.ClubSpeleo));                                                         //  Speleographe: string;
          SubSection.SetAttribute(GTX_ATTR_EXPE_OBS      , PasStrToXMLStr(EX.Commentaire));                                                        //  Commentaire : string;
        SectionSeances.AppendChild(SubSection);
      end;
    // section Séries
    SectionSeries := MyDocXML.CreateElement(GTX_KEY_SECTION_SERIES);
      GHTopoRoot.AppendChild(SectionSeries);
      KWYXZ(GTX_KEY_SECTION_SERIES);
      for i := IDX_MINI to GetNbSeries - 1 do
      begin
        SER := GetSerie(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_SERIE);
          SubSection.SetAttribute(GTX_ATTR_SERIE_Numero  , Format(FORMAT_NB_INTEGER, [SER.GetNumeroDeSerie()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_NAME    , SER.GetNomSerie);
          SubSection.SetAttribute(GTX_ATTR_SERIE_ENTRANCE, Format(FORMAT_NB_INTEGER, [SER.GetNumeroEntrance()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_RESEAU  , Format(FORMAT_NB_INTEGER, [SER.GetNumeroReseau]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_SERDEP  , Format(FORMAT_NB_INTEGER, [SER.GetNoSerieDep()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_PTDEP   , Format(FORMAT_NB_INTEGER, [SER.GetNoPointDep()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_SERARR  , Format(FORMAT_NB_INTEGER, [SER.GetNoSerieArr()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_PTARR   , Format(FORMAT_NB_INTEGER, [SER.GetNoPointArr()]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_COLOR   , Format(FORMAT_STRING, [ColorToHTMLColor(SER.GetCouleur())]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_CHANCE  , Format(FORMAT_NB_INTEGER, [SER.GetChance]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_OBSTACLE, Format(FORMAT_NB_INTEGER, [SER.GetObstacle]));
          SubSection.SetAttribute(GTX_ATTR_SERIE_OBS     , PasStrToXMLStr(SER.GetObsSerie));
          SubSection.SetAttribute(GTX_ATTR_SERIE_RAIDEUR , Format(FORMAT_NB_REAL_6_DEC, [SER.GetRaideur]));

        SectionSeries.AppendChild(SubSection);
        SubSubSection := MyDocXML.CreateElement(GTX_KEY_STATIONS);
        SubSection.AppendChild(SubSubSection);
        for S := 0 to SER.GetNbVisees - 1 do
        begin
          ST := SER.GetVisee(S);
          SubSubSubSection := MyDocXML.CreateElement(GTX_KEY_VISEE);
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_ID      , Format(FMTSERST, [SER.GetNumeroDeSerie(), S]));     //
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LBL     , PasStrToXMLStr(ST.IDTerrainStation));         // IDTerrainStation: string;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_SECTEUR , Format(FORMAT_NB_INTEGER, [ST.IDSecteur]));             // IDSecteur : integer;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_TYPE    , Format(FORMAT_NB_INTEGER, [Ord(ST.TypeVisee)]));           // TypeVisee : TTypeDeVisee;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_CODE    , Format(FORMAT_NB_INTEGER, [ST.Code]));                     // Code      : integer;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_EXPE    , Format(FORMAT_NB_INTEGER, [ST.Expe]));                     // Expe      : integer;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LONG    , Format(FORMAT_NB_REAL_3_DEC, [ST.Longueur]));               // Longueur  : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_AZ      , Format(FORMAT_NB_REAL_3_DEC, [ST.Azimut]));                 // Azimut    : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_P       , Format(FORMAT_NB_REAL_3_DEC, [ST.Pente]));                  // Pente     : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LG      , Format(FORMAT_NB_REAL_3_DEC, [ST.LG]));                     // LG        : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_LD      , Format(FORMAT_NB_REAL_3_DEC, [ST.LD]));                     // LD        : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_HZ      , Format(FORMAT_NB_REAL_3_DEC, [ST.HZ]));                     // HZ        : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_HN      , Format(FORMAT_NB_REAL_3_DEC, [ST.HN]));                     // HN        : double;
            SubSubSubSection.SetAttribute(GTX_ATTR_VISEE_OBS     , PasStrToXMLStr(ST.Commentaires));                             // Commentaires    : string;
          SubSubSection.AppendChild(SubSubSubSection);
        end;
      end;
    // section Antennes (à la fin du document XML)
    QNbAntennes := GetNbAntennes();
    if (QNbAntennes > 0) then
    begin
      SectionAntennes := MyDocXML.CreateElement(GTX_KEY_SECTION_ANTENNAS);
      KWYXZ(GTX_KEY_SECTION_ANTENNAS);
      GHTopoRoot.AppendChild(SectionAntennes);
      for i := IDX_MINI to QNbAntennes - 1 do
      begin
        ANT := GetViseeAntenne(i);
        SubSection := MyDocXML.CreateElement(GTX_KEY_ANTENNA_SHOT);
          SubSection.SetAttribute(GTX_KEY_ANTENNA_NETWORK , Format(FORMAT_NB_INTEGER, [ANT.Reseau]));                      // Reseau              : integer;
          SubSection.SetAttribute(GTX_KEY_ANTENNA_SECTEUR , Format(FORMAT_NB_INTEGER, [ANT.Secteur]));                     // Secteur             : integer;
          SubSection.SetAttribute(GTX_KEY_ANTENNA_SERIE   , Format(FORMAT_NB_INTEGER, [ANT.SerieDepart]));                 // SerieDepart         : integer;
          SubSection.SetAttribute(GTX_KEY_ANTENNA_POINT   , Format(FORMAT_NB_INTEGER, [ANT.PtDepart]));                    // PtDepart            : integer;
          SubSection.SetAttribute(GTX_KEY_ANTENNA_LONG    , Format(FORMAT_NB_REAL_3_DEC, [ANT.Longueur]));                  // Longueur            : double;      //         'Longueur
          SubSection.SetAttribute(GTX_KEY_ANTENNA_AZIMUT  , Format(FORMAT_NB_REAL_3_DEC, [ANT.Azimut]));                    // Azimut              : double;      //         'Azimut
          SubSection.SetAttribute(GTX_KEY_ANTENNA_PENTE   , Format(FORMAT_NB_REAL_3_DEC, [ANT.Pente]));                     // Pente               : double;      //         'Pente
        SectionAntennes.AppendChild(SubSection);
      end;
    end;
    KWYXZ('--> Serializing XML file');
    // sérialisation du fichier
    WriteXMLFile(MyDocXML, FichierXML);
    KWYXZ('--> Done');
  finally
    FreeAndNil(MyDocXML);//MyDocXML.Free;
  end;
end;


