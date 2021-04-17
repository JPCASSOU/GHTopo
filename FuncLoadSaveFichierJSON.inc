// FuncLoadSaveFichierJSON.inc
// Fichier de déclaration des clés pour le format JSON
{$ERROR En attente}
const
  JSON_ATTR_COLOR_R     = 'ColorR';
  JSON_ATTR_COLOR_G     = 'ColorG';
  JSON_ATTR_COLOR_B     = 'ColorB';

  JSON_KEY_SECTION_GENERAL      = 'General';
   JSON_KEY_CAVITE               = 'Cavite';
    JSON_ATTR_NAMESPACE           = 'Namespace';
    JSON_ATTR_NOM_ETUDE           = 'FolderName';
    JSON_ATTR_COORDS_SYSTEM_NOM   = 'CoordsSystem';
    JSON_ATTR_COORDS_SYSTEM_EPSG  = 'CoordsSystemEPSG';
    JSON_ATTR_COMMENTAIRES_ETUDE  = 'FolderObservations';
   // Espaces de noms
  JSON_KEY_SECTION_NAMESPACES    = 'Namespaces';
    JSON_KEY_NAMESPACE            = 'Namespace';
     JSON_ATTR_NAMESPACE_NAME        = 'Name';
     JSON_ATTR_NAMESPACE_DESCRIPTION = 'Description';

  JSON_KEY_SECTION_FILTERS      = 'Filters';
    JSON_KEY_FILTER                = 'Filtre';
    JSON_ATTR_FILTER_IDX           = 'Numero';
    JSON_ATTR_FILTER_NAME          = 'Name';
    JSON_ATTR_FILTER_EXPRESSION    = 'Expression';
    JSON_ATTR_FILTER_DESCRIPTION   = 'Description';

  JSON_KEY_SECTION_ENTRANCES    = 'Entrances';
    JSON_KEY_ENTRANCE             = 'Entrance';
      JSON_ATTR_ENTRANCE_IDX      = 'Numero';
      JSON_ATTR_ENTRANCE_IDTERRAIN= 'IdTerrain';
      JSON_ATTR_ENTRANCE_NAME     = 'Name';
      JSON_ATTR_ENTRANCE_REFSERIE = 'RefSerie';
      JSON_ATTR_ENTRANCE_REFPT    = 'RefPoint';
      JSON_ATTR_ENTRANCE_X        = 'X';
      JSON_ATTR_ENTRANCE_Y        = 'Y';
      JSON_ATTR_ENTRANCE_Z        = 'Z';
      JSON_ATTR_ENTRANCE_COLOR    = 'Colour';
      JSON_ATTR_ENTRANCE_OBS      = 'Comments';

  JSON_KEY_SECTION_RESEAUX     = 'Networks';
    JSON_KEY_RESEAU            = 'Network';
      JSON_ATTR_RESEAU_IDX     = 'Numero';
      JSON_ATTR_RESEAU_TYPE    = 'Type';
      JSON_ATTR_RESEAU_NAME    = 'Name';
      JSON_ATTR_RESEAU_OBS     = 'Comments';

  JSON_KEY_SECTION_SECTEURS    = 'Secteurs';
    JSON_KEY_SECTEUR            = 'Secteur';
      JSON_ATTR_SECTEUR_IDX     = 'Numero';
      JSON_ATTR_SECTEUR_NAME    = 'Name';

  JSON_KEY_SECTION_CODES       = 'Codes';
    JSON_KEY_CODE              = 'Code';
      JSON_ATTR_CODE_NUMERO    = 'Numero';
      JSON_ATTR_CODE_UCLINO    = 'ClinoUnit';
      JSON_ATTR_CODE_UCOMPASS  = 'CompassUnit';
      JSON_ATTR_CODE_FACT_LONG = 'FactLong';
      JSON_ATTR_CODE_TYPE      = 'Type';
      JSON_ATTR_CODE_ANGLIMITE = 'AngleLimite';
      JSON_ATTR_CODE_PSI_L     = 'PsiL';
      JSON_ATTR_CODE_PSI_A     = 'PsiAz';
      JSON_ATTR_CODE_PSI_P     = 'PsiP';
      JSON_ATTR_CODE_OBS       = 'Comments';
      JSON_ATTR_CODE_ERROR_TOURILLON = 'ErrorTourillon';
      JSON_ATTR_CODE_DIAM_BOULE1     = 'DiamBoule1';
      JSON_ATTR_CODE_DIAM_BOULE2     = 'DiamBoule2';
      JSON_ATTR_CODE_FUNC_CORR_AZ_CO           = 'FuncCorrAzCo';
      JSON_ATTR_CODE_FUNC_CORR_AZ_ERR_MAX      = 'FuncCorrAzErrMax';
      JSON_ATTR_CODE_FUNC_CORR_AZ_POS_ERR_MAX  = 'FuncCorrAzPosErrMax';
      JSON_ATTR_CODE_FUNC_CORR_INC_CO          = 'FuncCorrIncCo';
      JSON_ATTR_CODE_FUNC_CORR_INC_ERR_MAX     = 'FuncCorrIncErrMax';
      JSON_ATTR_CODE_FUNC_CORR_INC_POS_ERR_MAX = 'FuncCorrIncPosErrMax';

  JSON_KEY_SECTION_EXPES       = 'Seances';
    JSON_KEY_EXPE              = 'Trip';
      JSON_ATTR_EXPE_NUMERO    = 'Numero';
      JSON_ATTR_EXPE_DATE      = 'Date';
      JSON_ATTR_EXPE_IDXCOLOR  = 'Color';
      JSON_ATTR_EXPE_SURVEY1   = 'Surveyor1';
      JSON_ATTR_EXPE_SURVEY2   = 'Surveyor2';
      JSON_ATTR_EXPE_DECLINAT  = 'Declination';
      JSON_ATTR_EXPE_INCLINAT  = 'Inclination';
      JSON_ATTR_EXPE_MODEDECL  = 'ModeDeclination';
      JSON_ATTR_EXPE_OBS       = 'Comments';

  JSON_KEY_SECTION_SERIES      = 'Series';
    JSON_KEY_SERIE             = 'Serie';
      JSON_ATTR_SERIE_Numero   = 'Numero';
      JSON_ATTR_SERIE_NAME     = 'Name';
      JSON_ATTR_SERIE_SERDEP   = 'SerDep';
      JSON_ATTR_SERIE_PTDEP    = 'PtDep';
      JSON_ATTR_SERIE_SERARR   = 'SerArr';
      JSON_ATTR_SERIE_PTARR    = 'PtArr';
      JSON_ATTR_SERIE_ENTRANCE = 'Entrance';
      JSON_ATTR_SERIE_RESEAU   = 'Network';
      JSON_ATTR_SERIE_OBSTACLE = 'Obstacle';
      JSON_ATTR_SERIE_CHANCE   = 'Chance';
      JSON_ATTR_SERIE_RAIDEUR  = 'Raideur';
      JSON_ATTR_SERIE_OBS      = 'Comments';
      JSON_ATTR_SERIE_COLOR    = 'Color';
      JSON_KEY_STATIONS        = 'Stations';
        JSON_KEY_VISEE         = 'Shot';
          JSON_ATTR_VISEE_ID   = 'ID';
          JSON_ATTR_VISEE_LBL  = 'Label';
          JSON_ATTR_VISEE_SECTEUR = JSON_KEY_SECTEUR;
          JSON_ATTR_VISEE_TYPE = 'TypeShot';
          JSON_ATTR_VISEE_CODE = JSON_KEY_CODE;
          JSON_ATTR_VISEE_EXPE = JSON_KEY_EXPE;
          JSON_ATTR_VISEE_LONG = 'Length';
          JSON_ATTR_VISEE_AZ   = 'Az';
          JSON_ATTR_VISEE_P    = 'Incl';
          JSON_ATTR_VISEE_LG   = 'Left';
          JSON_ATTR_VISEE_LD   = 'Right';
          JSON_ATTR_VISEE_HZ   = 'Up';
          JSON_ATTR_VISEE_HN   = 'Down';
          JSON_ATTR_VISEE_OBS  = 'Comments';

  JSON_KEY_SECTION_ANTENNAS    = 'AntennaShots';
    JSON_KEY_ANTENNA_SHOT      = 'AntennaShot';
    JSON_KEY_ANTENNA_NETWORK   = 'Network';
    JSON_KEY_ANTENNA_SECTEUR   = JSON_KEY_SECTEUR;
    JSON_KEY_ANTENNA_SERIE     = 'SerDep';
    JSON_KEY_ANTENNA_POINT     = 'PtDep';
    JSON_KEY_ANTENNA_LONG      = 'Length';
    JSON_KEY_ANTENNA_AZIMUT    = 'Az';
    JSON_KEY_ANTENNA_PENTE     = 'Incl';
//******************************************************************************
function TToporobotStructure2012.LoadFromJSON(const FichierJSON: TStringDirectoryFilename): integer;
var
  EPSGDefault: TLabelSystemesCoordsEPSG;
begin
  Result := -1;
  AfficherMemoryUsage();
  AfficherMessage(Format('%s.LoadFromJSON(%s)', [ClassName, FichierJSON]));

  SetNomEtude('Etude001');     // nom étude par défaut
  SetCommentairesEtude('Issued from XML format');
  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;
  SetCodeEPSGSystemeCoordonnees(EPSGDefault);
  // réinit du document
  self.ReInitialiser(True);

  EXIT;  // A virer dès mise au point
  //**************************
  Preconditionner(FichierJSON);
  //****************************
  Result := self.GetNbSeries();
end;

procedure TToporobotStructure2012.SaveToJSON(const FichierJSON: TStringDirectoryFilename);
var
  jArray: TJSONArray;
begin
  AfficherMessage(Format('%s.SaveToJSON(%s)', [ClassName, FichierJSON]));

  // add a new sub-array
  jArray := TJSONArray.Create;
  jArray.Add('North');
  jArray.Add('South');
  jArray.Add('East');
  jArray.Add('West');
  //jObject.Add('Directions', jArray);


end;
