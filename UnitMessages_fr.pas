unit UnitMessages_fr;
  // Fichier UnitMessages_fr.pas cree le 09/05/2014 a 19:28:30
  // Genere par les macros du classeur: file:///C:/0_Logiciels_JPC/0_GHTopoFPC_20140503/Outils_Calc/GestionResourceStrings.ods
  //********************************************************************************
  // /!\ DO NOT MODIFY - Generated automatically
  // Please use spreadsheet utility for modification of strings
  // Language: FR - Français
  // ================================================================================
  // NOUVEAUTE 2016: Ce fichier doit être encodé en UTF8 sans BOM
  //
interface
uses
  StructuresDonnees; // pour le code instrument lasermetres

// CONST section

const
  NATIONAL_GEODESIC_SYSTEM_IDX = 1;
// RESOURCESTRING section
// noms d'items pour factorisation
  rsITEM_ENTRANCES                  = 'Entrées';
  rsITEM_RESEAUX                    = 'Réseaux';
  rsITEM_SECTEURS                   = 'Secteurs';
  rsITEM_CODES                      = 'Codes';
  rsITEM_EXPES                      = 'Séances';
  rsITEM_SERIES                     = 'Séries';
  rsITEM_NAMESPACES                 = 'Espaces de nom';
  rsITEM_TYPE_VISEE                 = 'Type de visées';
  rsITEM_DATES                      = 'Dates';
  rsITEM_PROXIMITES                 = 'Proximités';
  rsITEM_SERIES_ENCADREES           = 'Terminus lié';

resourcestring
  // nom du logiciel
  rsGHTOPOEXENAME                            = 'GHTopo';
  rsABOUT_AUTHOR_0                           = 'L''auteur de %s est %s';
  rsABOUT_AUTHOR_ATHEISTE                    = 'athéiste militant';
  rsABOUT_AUTHOR_CHRISTIANOPHOBE             = 'christianophobe';
  rsDLG_OPEN_DOCUMENT                        = 'Ouverture d''un document';

  // Notification obligation d'apostasie
  rsOBLIGATION_APOSTASIE                     = 'Vous devez faire acte d''apostasie pour utiliser ';

  rsGHTOPOVERSION                            = 'Version 3.1415926532 du %.2d/%.2d/%.4d %.2d:%.2d:%.2d';
  rsGHTOPOLICENSE                            = 'Logiciel sous licence GPL';
  rsGHTOPOAUTHOR                             = '(c) 1989 ..%d Jean Pierre CASSOU';
  rsGHTOPO_MEMORY_USAGE                      = 'Mémoire réservée par GHTopo: %.0n, Utilisée: %.0n, Libre: %.0n';
  rsCOORDS_CONVERTER_AUTHOR                  = 'Convertisseur de coordonnées adapté du code source (GPL 2005) de celui de Eric SIBERT http://eric.sibert.fr/';
  // infos sur la session
  rsGHTOPORESOL                              = 'Résolution %d x %d';
  rsGHTOPOPlatFormLinux                      = 'Plateforme: Linux (Français)';
  rsGHTOPOPlatFormWin32                      = 'Plateforme: Microsoft Windows (Français)';
  // Langue choisie
  rsCHOOSELANGUAGE                           = 'Lancement en langue francaise';
  // fin GHTopo
  rsEND_OF_GHTOPO                            = 'Fin GHTopo';
  // type d'interface                        =
  rsTYPE_INTERFACE_TABLETTE                  = 'Version TabletPC';
  rsTYPE_INTERFACE_DESKTOP                   = 'Version PC de bureau';
  // unités
  rsDESC_DEVICE_TLM330                       = 'TLM 330';
  rsDESC_UNITE_ANGULAIRE_DEGRES              = 'Degrés';
  rsDESC_UNITE_ANGULAIRE_GRADES              = 'Grades';
  rsDESC_UNITE_PENTES_PERCENT                = 'Poucentage';
  rsDESC_UNITE_PENTE_DENIVELE                = 'Dénivelé';
  rsDESC_VISEE_DIRECTE                       = 'Directe';
  rsDESC_VISEE_INVERSE                       = 'Inverse';
  rsDESC_POS_ZERO_HZ                         = 'Horizontal';
  rsDESC_POS_ZERO_ZENITHAL                   = 'Zénithal';
  rsDESC_POS_ZERO_NADIRAL                    = 'Nadiral';

  // nombre d'éléments dans les listes
  rsNB_ELEMENTS_ENTRANCES                    = '%d entrées';
  rsNB_ELEMENTS_RESEAUX                      = '%d réseaux';
  rsNB_ELEMENTS_SECTEURS                     = '%d secteurs';
  rsNB_ELEMENTS_CODES                        = '%d codes';
  rsNB_ELEMENTS_EXPES                        = '%d séances';
  rsNB_ELEMENTS_ANNOTATIONS                  = '%d annotations';
  rsNB_ELEMENTS_POI                          = '%d points d''intérêt';
  rsNB_ELEMENTS_CHECK_MSG                    = '%d messages';
  rsNB_ELEMENTS_NODES                        = '%d noeuds';
  rsNB_ELEMENTS_NAMESPACES                   = '%d namespaces';
  rsNB_ELEMENTS_STATIONS                     = '%d stations';

  // Messages liés à la progression d'un process:
  //--------------------------------------------
  rsDONE_PRINTING                            = 'Impression terminée';
  rsDONE_CALCUL                              = 'Calcul terminé';
  rsDONE_ANY_PROCESS                         = 'Opération terminée';

  // *********************************************
  // resourcestring des libellés communs     =
  rsLBL_COMMENTS                             = 'Commentaires';
  rsSERIE_INITIALISATION                     = 'Serie 0 non modifiable';
  rsMSG_SERIE_INITIALISATION                 = 'La série 0 n''est pas modifiable';

  // Filtres de fichiers
  //rsFILEFILTER_ALL = 'Tous (*.*)|*.*';     =
  rsGHTOPO_FILE_FILTER_WO_TEXT               = 'Fichiers GHTopo (*.xtb)|*.xtb|' +
                                               'Fichiers GHTopo XML (*.gtx)|*.gtx|' +
                                               'Fichiers Toporobot Tab [deprecated] (*.Tab)|*.Tab|' +
                         //'Fichiers JSON (*.json)|*.json|' +
                                               'Tous (*.*)|*.*';
  rsGHTOPO_FILE_FILTER_W_TEXT                = 'Fichiers GHTopo (*.xtb)|*.xtb|' +
                                               'Fichiers GHTopo XML (*.gtx)|*.gtx|' +
                                               'Fichiers Toporobot Tab [deprecated] (*.Tab)|*.Tab|' +
                                               'Fichiers Toporobot Text [deprecated] (*.Text)|*.Text|' +
                                               'Fichiers PocketTopo TXT|*.txt' +
                                               'Fichiers JSON (*.json)|*.json|' +
                                               'Tous (*.*)|*.*';
  rsKML_FILE_FILTER                          = 'Fichier Google Earth (*.kml)|*.kml|Tous (*.*)|*.*';
  rsGCD_FILE_FILTER                          = 'Polygonale GHCaveDraw (*.gcd)|*.gcd|Tous (*.*)|*.*';
  rsGPX_FILE_FILTER                          = 'Fichier GPS (*.gpx)|*.gpx|Tous (*.*)|*.*';
  rsOSM_FILE_FILTER                          = 'Fichier travail OpenStreetMap (*.osm)|*.osm|Tous (*.*)|*.*';
  rsCSV_FILE_FILTER                          = 'Texte tabulé (*.csv)|*.csv|Tous (*.*)|*.*';
  rsTXT_FILE_FILTER                          = 'Fichiers texte |*.txt|Tous (*.*)|*.*';
  rsLEAFLET_FILE_FILTER                      = 'HTML Leaflet |*.htm|Tous (*.*)|*.*';

  rsVTOPO_FILE_FILTER                        = 'Document Visual Topo (*.tro)|*.tro|Tous (*.*)|*.*';
  rsTHERION_FILE_FILTER                      = 'Centerlines Therion (*.th)|*.th|Tous (*.*)|*.*';
  rsBTN_COPIER_TABLEAU                       = 'Copier le tableau';
  // *********************************************
  // resourcestring du menu principal        =
  rsMSGASSIGNCAPTIONS                        = 'Mise en place de l''interface';
  rsMSG_REGEN_ADDITIONAL_LAYER               = 'Vous devez éventuellement régénérer la liste des couches'+ #13#10 +
                                               'Reconstruire cette liste';
  rsCALCURAPIDE                              = 'Calculette:';
  rsMNU_FILE                                 = '&Fichier';
    rsNEW                                    = 'Nouveau';
    rsOPEN                                   = 'Ouvrir';
    rsSAVE                                   = 'Sauvegarder';
    rsSAVEAS                                 = '&Enregistrer sous';
    rsQUICK_SAVE                             = 'Sauvegarde horodatée';
    rsMNU_SAVELOG                            = 'Enregistrer historique';
    rsCLOSE                                  = 'Fermer le document';
    rsRECENT_DOCS                            = 'Derniers documents ouverts';
    rsRELOAD                                 = 'Recharger le document';
    rsEDITTAB                                = 'Editer fichier &Tab';
    rsPRINT                                  = 'Im&primer';
    rsVTOPO                                  = 'Export Visual Topo';
    rsAJOUT_SERIES_DEPUIS_AUTRE_DOC          = 'Ajouter des séries depuis un autre document';
    rsEXPORT_THERION                         = 'Export vers Therion (expérimental)';
    rsEXPORT_TOPOROBOT                       = 'Export vers Toporobot / PocketTopo Text';
    rsEXPORT_TOPODROID                       = 'Export vers Topodroid';
    rsEXPORT_COMPASS_PLT                     = 'Export vers Compass PLT';
    rsEXPORT_SVG                             = 'Export vers SVG';
    rsGHCAVEDRAW                             = 'Polygonale pour GHCaveDraw';
    rsEXPORT_SERIES_TAB_CROISE               = 'Export des séries pour tableaux croisés';
    rsEXPORT_GIS                             = 'Export vers SIG';
    rsEXPORTGRAPHIQUE                        = 'Export graphique (PS, DXF et SVG)';
    rsERRINFOXTB                             = 'Rapport d''erreur de lecture';
    rsGHTOPO_QUIT                            = 'Quitter GHTopo';
    rsQUIT_EDITOR                            = 'Quitter l''éditeur de texte';
    rsGENERER_DOSSIER_THERION                = 'Générer un dossier complet Therion (expérimental)';
    rsFUSIONNER_TOPOS                        = 'Fusionner des documents GHTopo (expérimental)';

  rsMNU_EDITION                              = '&Edition';
    rsCOPY_TO_CLIPBOARD                      = 'Copier';
    rsCOPY_ALL_TEXT_TO_CLIPBOARD             = 'Copier le texte';

  rsMNU_TOPOGRAPHIE                          = '&Topographie';
    rsCHECKBASE                              = 'Vérifier les données';
    rsCOMPILE                                = '&Calculer le réseau';
    rsVUEPLAN                                = 'Vue en &Plan';
    rsVUE3D                                  = '&Vue 3D';
    rsRENDU3D                                = 'Rendu &3D';
    rsSTATISTIQUES                           = '&Statistiques';
    rsINFOCAVITE                             = '&Info cavité';
    rsMAILLAGES_UTILS                        = 'Maillages de surface';
    rsNODESCOORDINATES                       = 'Coordonnées des &Noeuds';
    rsREDEF_COORD_SYSTEME                    = 'Changer de système de coordonnées';
    rsRECALCUL_DECLIMAG                      = 'Recalculer les déclinaisons magnétiques des séances';
    rsVIDER_CROQUIS                          = 'Vider le croquis';
  rsMNU_WINDOW                               = '&Fenêtre';
  rsMNU_TOOLS                                = '&Outils';
  rsMNU_HELP                                 = '&Aide';
    rsHLPINDEX                               = '&Index';
    rsHLPNEWS                                = 'Nouveautés';
    rsABOUT                                  = 'A propos de GHTopo';
  rsMNU_STAY_ON_TOP                          = 'Mettre devant';

  rsGESTION_VISEES_RADIANTES                 = 'Gestion des visées rayonnantes';
  // titres des fenêtres
  rsWND_DATABASE                             = rsITEM_SERIES;
  rsWND_LISTES_SIMPLES                       = 'Listes (codes, séances, ...)';
  rsWND_CONSOLE                              = '&Console';
  rsWND_DISTO_X                              = 'Visées du DistoX';
  rsWND_PLAN                                 = '&Vue en plan';
  rsCOPY_CONSOLE_ERREUR                      = 'Copier le contenu de la console d''erreurs';
  rsCLEAR_CONSOLE_ERREUR                     = 'Vider la console d''erreurs';

  // ********************************
  // resourcestring du visualisateur en plan =
  rsVUE2D_TITLE                              = 'Vue en plan';
  rsVUE2D_DISPLAY_MASK                       = 'Afficher / Masquer ';
  rsVUE2D_DISPLAY_PARAMETRES_VUE             = 'Paramètres d''affichage de la vue';
  rsVUE2D_DISPLAY_LISTE_SERIES               = 'Liste des séries';
  rsVUE2D_DISPLAY_DIAGRAMMES                 = 'Diagrammes';
  rsVUE2D_REPRESENTATION_MODE                = 'Représentation';

  rsVUE2D_REPRESENTATION_ENTRANCES           = rsITEM_ENTRANCES;
  rsVUE2D_REPRESENTATION_NETWORKS            = rsITEM_RESEAUX;
  rsVUE2D_REPRESENTATION_SECTEURS            = rsITEM_SECTEURS;
  rsVUE2D_REPRESENTATION_EXPES               = rsITEM_EXPES;
  rsVUE2D_REPRESENTATION_GRAY                = 'Gris';
  rsVUE2D_REPRESENTATION_DEPTH               = 'Profondeurs';
  rsVUE2D_REPRESENTATION_TYPES               = 'Nature visée';

  rsVUE2D_REPRESENTATION_STATIONS            = 'Points topo';
  rsVUE2D_LOAD_MAILLAGE                      = 'Charger un modèle numérique de terrain';
  rsVUE2D_DISTANCE                           = 'Distance entre deux stations';
  rsVUE2D_FINDSTATION                        = 'Rechercher une station';
  //
  //rsVUE2D_DEPTH_DIAGRAM = 'Histogramme altimétrique';

  rsVUE2D_METAFILTRE                         = 'Filtres';
  rsVUE2D_PANVUE                             = 'Deplacer vue';

  rsVUE2D_PRINTING                           = 'Imprimer le plan';
  rsVUE2D_REFRESH                            = 'Redessiner la vue';
  rsVUE2D_EXPORT_DXF                         = 'Export au format DXF';
  rsVUE2D_STATISTIQUES                       = 'Statistiques';
  rsVUE2D_VUE3D_GDI                          = 'Vue 3D (sans OpenGL)';
  rsVUE2D_VUE3D_OPENGL                       = 'Rendu 3D OpenGL';
  rsVUE2D_ZOOMALL                            = 'Zoom sur tout le réseau';
  rsVUE2D_ZOOMFENETRE                        = 'Zoom sur fenêtre';

  rsVUE2D_FMT_INFOS_STATION_RAPIDE           = 'Station: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
  rsVUE2D_FMT_INFOS_ID_STATION               = '%d.%d [%s]';
  rsVUE2D_FMT_INFOS_MESURES                  = 'L = %.2f, Az = %.2f, P =%.2f, G = %.2f, D = %.2f, H = %.2f, B = %.2f';
  rsVUE2D_FMT_INFOS_COORDONNEES              = 'X=%s, Y=%s, Z=%s; x=%s, y=%s, z=%s';
  rsVUE2D_EXPORT_XHTML                       = 'Exporter au format XHTML';
  rsVUE2D_EXPORT_SVG                         = 'Exporter au format SVG';
  // *******************************************************
  // resourcestring des onglets du Gestionnaire de Cavité
  rsTBS_GENERAL                              = 'Général';
  rsTBS_LISTES_SIMPLES                       = 'Listes';
  rsTBS_DISTO_X                              = 'DistoX';
  rsTABLE                                    = 'Liste:';
  rsBTN_FIND                                 = 'Rechercher';
  rsTBS_GENERAL_NOM                          = 'Nom du réseau';
  rsTBS_GENERAL_OBS                          = 'Commentaires';
  rsTBS_GENERAL_SYST                         = 'Système de coordonnées';
  rsTBS_TOUT                                 = 'Tout';
  rsTBS_NAMESPACES                           = rsITEM_NAMESPACES;
  rsTBS_ENTRANCE                             = rsITEM_ENTRANCES;
  rsTBS_SECTEURS                             = rsITEM_SECTEURS;
  rsTBS_CODES                                = rsITEM_CODES;
  rsTBS_TRIPS                                = rsITEM_EXPES;
  rsTBS_SERIES                               = rsITEM_SERIES;
  rsTBS_RESEAUX                              = rsITEM_RESEAUX;
  rsTBS_PROXIMITES                           = rsITEM_PROXIMITES;
  rsTBS_SERIES_ENCADREES                     = rsITEM_SERIES_ENCADREES;
  rsTBS_ANTENNES                             = 'Visées en antenne';
  rsTBS_ANNOTATIONS                          = 'Annotations';
  rsTBS_POI                                  = 'Points d''intérêt';
  rsTBS_POI_ABREGE                           = 'POIs';

  rsTBS_CHECKUP_MESSAGES                     = 'Messages';
  rsTBS_NOEUDS                               = 'Noeuds';
  rsTBS_RELEASE_NOTES                        = 'Notes de version';
  rsTBS_SERIES_FLAT_TABLE                    = 'Séries sous forme de tableau';
  rsTBS_SERIES_VUE_EN_PLAN                   = 'Vue en plan';
  rsTBS_MAINTENANCE                          = 'Maintenance';
  // *******************************************************
  // resourcestring de la section General    =
  rsLB_NOM_ETUDE                             = 'Nom de l''étude';
  rsLB_COMMENTAIRE_ETUDE                     = 'Commentaires';
  rsLB_CODE_EPSG                             = 'Code EPSG du système de coordonnées';
  rsBTN_SELECT_EPSG                          = 'Choisir ...';
  rsBTN_CALC_DECLIMAGS                       = 'Calculer les déclinaisons magnétiques';
  // *******************************************************
  // resourcestring du cadre Entrées
  rsCDR_ENTR_NOENTRANCE                      = 'Entrée';
  rsCDR_ENTR_ENTRNAME                        = 'Nom de l''entrée';
  rsCDR_ENTR_ENTR_ID                         = 'ID de l''entrée';
  rsCDR_ENTR_COORDINATES                     = 'Coordonnées';
  rsCDR_ENTR_STATOFENTR                      = 'Station de référence';
  rsCDR_ENTR_CALLCALCULETTE                  = 'Convertisseur';
  rsCDR_ENTR_BTN_GET_COORD_FROM_IDTERRAIN    = '-> ID terrain';
  rsCDR_ENTR_BTN_GET_COORD_FROM_SER_ST       = '-> série/station';
  rsCDR_ENTR_ALTITUDE_FROM_MNT               = 'Depuis MNT';
  // *******************************************************
  // resourcestrings du cadre Réseaux        =
  rsCDR_RESEAU_LBIDX                         = 'Numero';
  rsCDR_RESEAU_NAME                          = 'Nom';
  rsCDR_RESEAU_TYPE                          = 'Type de réseau';
  rsCDR_RESEAU_CB0                           = 'Cavité naturelle';
  rsCDR_RESEAU_CB1                           = 'Cavité artificielle';
  rsCDR_RESEAU_CB2                           = 'Topo de surface';
  rsCDR_RESEAU_CB3                           = 'Thalweg';
  rsCDR_RESEAU_CB4                           = 'Route ou piste';
  rsCDR_RESEAU_CB5                           = 'Sentier';
  rsCDR_RESEAU_CB6                           = 'Autre';
  // *******************************************************
  // resourcestrings du cadre Secteurs
  rsCDR_SECTEUR_LBIDX                        = 'Numero';
  rsCDR_SECTEUR_NAME                         = 'Nom';
  rsCDR_SECTEUR_COLOUR                       = 'Couleur';
  // *******************************************************
  // resourcestrings du cadre Expés
  rsCDR_EXPE_SEANCE                          = 'Séance topo';
  rsCDR_EXPE_DATE                            = 'Date';
  rsCDR_EXPE_DECLIMAG                        = 'Déclinaison';
  rsCDR_EXPE_INCLIN                          = 'Inclinaison';
  rsCOLOR                                    = 'Couleur';
  rsCDR_EXPE_SPELEOMETRE                     = 'Opérateurs';
  rsCDR_EXPE_SPELEOGRAPHE                    = 'Club';
  // *******************************************************
  // resourcestring du cadre Codes           =
  rsCDR_CODES_NUMERO                         = 'Code';
  rsCDR_CODES_TYPEGALERIE                    = 'Type de visée';
  // types de galeries
  rsTYPE_VISEE_DEFAULT                       = 'Défaut';
  rsTYPE_VISEE_ENTRANCE                      = 'Entrée';
  rsTYPE_VISEE_FOSSILE                       = 'Fossile';
  rsTYPE_VISEE_VADOSE                        = 'Ecoulement libre';
  rsTYPE_VISEE_ENNOYABLE                     = 'Ennoyable';
  rsTYPE_VISEE_SIPHON                        = 'Siphon';
  rsTYPE_VISEE_FIXPOINT                      = 'Point fixe';
  rsTYPE_VISEE_SURFACE                       = 'Visée de liaison (raccord)';
  rsTYPE_VISEE_TUNNEL                        = 'Tunnel artificiel';
  rsTYPE_VISEE_MINE                          = 'Filon minier';
  rsTYPE_VISEE_ANTENNE                       = 'Visée rayonnante';
  rsTITRE_SELECTEUR_VISEE                    = 'Choix du type de visée';
    rsCMBTYPE_D                              = '0 - Défaut (conduit fossile)';
    rsCMBTYPE_E                              = '1 - Entrée';
    rsCMBTYPE_B                              = '2 - Galerie fossile';
    rsCMBTYPE_V                              = '3 - Ecoulement libre';
    rsCMBTYPE_W                              = '4 - Siphon';
    rsCMBTYPE_C                              = '5 - Passage ennoyable';
    rsCMBTYPE_F                              = '6 - Point fixe';
    rsCMBTYPE_S                              = '7 - Topo de surface';
    rsCMBTYPE_A                              = '8 - Tunnel artificiel';
    rsCMBTYPE_M                              = '9 - Filon minier';
  rsCDR_CODES_VISEE_HZ                       = 'Mesure Horizontale';
  rsCDR_CODES_VISEE_VT                       = 'Verticale';
  rsCDR_CODES_VDIRECT                        = 'Directe';
  rsCDR_CODES_VINVERSE                       = 'Inverse';
  rsCDR_CODES_GRADCOMPAS                     = 'Graduation du compas';
  rsCDR_CODES_GRADCLINO                      = 'Graduation du clinomètre';
  rsCDR_CODES_CMBUNIT_0                      = '400 - Grades';
  rsCDR_CODES_CMBUNIT_1                      = '360 - Degrés';
  rsCDR_CODES_CMBUNIT_2                      = '370 - Pourcentages';
  rsCDR_CODES_CMBUNIT_3                      = '380 - Dénivellations';
  rsCDR_CODES_CMBUNIT_4                      = '800 - Lasermètre/clinomètre de bâtiment';
  rsCDR_CODES_FACT                           = 'Longueurs x';
  rsCDR_CODES_POSZERO                        = 'Position du zéro:';
  rsCDR_CODES_CMBZERO_0                      = 'Nadiral';
  rsCDR_CODES_CMBZERO_1                      = 'Horizontal';
  rsCDR_CODES_CMBZERO_2                      = 'Zénithal';
  rsCDR_CODES_ANGLIMIT                       = 'Angle limite:';
  rsCDR_CODES_PRECISION                      = 'Précision instruments';
  rsCDR_CODES_ERREUR_TOURILLON               = 'Ecart de tourillon';
  rsCDR_CODES_DIAM_BOULES_CIBLES             = 'Diamètre boules-cibles';

  // *********************************************
  // resourcestring du cadre Séries          =
  rsDLG_PJMNGR_MOVETOSERIE                   = 'Changer de série ?';
  rsDLG_PJMNGR_ADDSERIE                      = 'Ajouter Série';
  rsCDR_SERIE_CHOOSE_SECTEUR                 = 'Choisir un secteur';
  rsCDR_SERIE_SHOW_HIDE_HEADER               = 'Affiche / masque l''entête';
  rsCDR_SERIE_CHOOSE_TYPE_VISEE              = 'Choisir un type de visée';
  rsCDR_SERIE_CHOOSE_CODE                    = 'Choisir un code instruments';
  rsCDR_SERIE_CHOOSE_EXPE                    = 'Choisir une séance topo';
  rsCDR_SERIE_LB_RESEAU                      = 'Réseau';
  rsCDR_SERIE_ADDPHOTO                       = 'Ajouter photo';
  rsCDR_SERIE_NUMERO                         = 'Série';
  rsCDR_SERIE_NAME                           = 'Nom';
  rsCDR_SERIE_DEPART                         = 'Départ';
  rsCDR_SERIE_ARRIVEE                        = 'Arrivée';
  rsCDR_SERIE_CHANCE                         = 'Chance';
    rsCDR_SERIE_CHANCE0                      = 'Aucune';
    rsCDR_SERIE_CHANCE1                      = 'Faible';
    rsCDR_SERIE_CHANCE2                      = 'Bonne';
    rsCDR_SERIE_CHANCE3                      = 'Excellente';
    rsCDR_SERIE_OBSTACLE                     = 'Obstacle';
    rsCDR_SERIE_OBSTACLE0                    = 'Aucun';
    rsCDR_SERIE_OBSTACLE1                    = 'Puits';
    rsCDR_SERIE_OBSTACLE2                    = 'Cheminée';
    rsCDR_SERIE_OBSTACLE3                    = 'Etroiture';
    rsCDR_SERIE_OBSTACLE4                    = 'Lac';
    rsCDR_SERIE_OBSTACLE5                    = 'Siphon';
    rsCDR_SERIE_OBSTACLE6                    = 'Effondrement';
    rsCDR_SERIE_OBSTACLE7                    = 'Concrétionnement';
    rsCDR_SERIE_OBSTACLE8                    = 'Sédiments';
    rsCDR_SERIE_OBSTACLE9                    = 'Autre';

    rsCDR_SERIE_OBSTACLE10                   = 'Gaz toxiques';
    rsCDR_SERIE_OBSTACLE11                   = 'Oies agressives';
    rsCDR_SERIE_OBSTACLE12                   = 'Animaux dangereux';
    rsCDR_SERIE_OBSTACLE13                   = 'Baisodrome';
    rsCDR_SERIE_OBSTACLE14                   = 'Grenouille';

    rsCDR_SERIE_COL_POINT                    = 'Station';
    rsCDR_SERIE_COL_TYPE                     = 'Type';
    rsCDR_SERIE_COL_ID_TERRAIN               = 'Etiquette';
    rsCDR_SERIE_COL_SECTEUR                  = 'Secteur';
    rsCDR_SERIE_COL_CODE                     = 'Code';
    rsCDR_SERIE_COL_EXPE                     = 'Séance';
    rsCDR_SERIE_COL_LEGNTH                   = 'Longueur';
    rsCDR_SERIE_COL_AZIMUTH                  = 'Azimut';
    rsCDR_SERIE_COL_INCLIN                   = 'Pente';
    rsCDR_SERIE_COL_LG                       = 'Gauche';
    rsCDR_SERIE_COL_LD                       = 'Droite';
    rsCDR_SERIE_COL_HZ                       = 'Haut';
    rsCDR_SERIE_COL_HN                       = 'Bas';
    rsCDR_SERIE_COL_COMMENTAIRE              = 'Commentaire';
  rsCDR_SERIE_UNDOCOPY_CODE_EXPE             = 'Affecter code et séance à partir de cette ligne';
  rsCDR_SERIE_LOCKED                         = 'Verrouillé';
  rsCDR_SERIE_NBLINES                        = 'Nombre de lignes';

  rsCDR_SERIE_IMPLEMENT                      = 'Valider';
  rsCDR_SERIE_ENTREE_RATT                    = 'Entrée';
  rsINPUT_COMMENTAIRE_TITRE                  = 'Commentaires station';
  rsINPUT_COMMENTAIRE_MSG                    = 'Entrez un texte';

  rsCDR_SERIE_AC_GRD_SEL_COPY                = 'Copier la sélection';
  rsCDR_SERIE_AC_GRD_PASTE                   = 'Coller';

  rsCDR_SERIE_AC_ADD_LINE                    = 'Insérer des lignes';
  rsCDR_SERIE_AC_DEL_LINE                    = 'Supprimer des lignes';
  rsCDR_SERIE_AC_UNDOCOPY                    = 'Recopier vers le bas';
  rsCDR_SERIE_AC_INC_UNDOCOPY                = 'Recopie vers le bas incrémentale';


  rsCDR_SERIE_AC_SELECT_RESEAU               = 'Sélectionner un réseau';
  rsCDR_SERIE_AC_SELECT_SECTEUR              = 'Sélectionner un secteur';
  rsCDR_SERIE_AC_SELECT_TYPE_VISEE           = 'Sélectionner un type de visée';
  rsCDR_SERIE_AC_SELECT_CODE                 = 'Sélectionner un code';
  rsCDR_SERIE_AC_SELECT_EXPE                 = 'Sélectionner une séance';
  rsCDR_SERIE_AC_EXTRACT_LABELS              = 'Extraire les ID terrain depuis le commentaire de la visée' + #10 +
                                               'et les copier dans la colonne ID Terrain';
  rsCDR_SERIE_AC_LAST_ENTRANCE               = 'Associer à l''entrée %d - %s';
  rsCDR_SERIE_LB_NB_SERIES                   = '%d séries';
  // messages d'erreur
  rsCDR_SERIE_MSG_ERR_ENTREE_NOT_FOUND       = 'Entrée introuvable';
  rsCDR_SERIE_MSG_ERR_RESEAU_NOT_FOUND       = 'Réseau introuvable';
  rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND         = 'Code instruments introuvable';
  rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND         = 'Session topo introuvable';
  rsCDR_SERIE_MSG_ERR_SERIE_NOT_FOUND        = 'Série introuvable';
  rsCDR_SERIE_MSG_ERR_LONG                   = 'La longueur doit être positive et inférieure à %.0f m';
  rsCDR_SERIE_MSG_ERR_LRUD                   = 'La distance ne doit pas être négative';
  rsCDR_SERIE_MSG_WARN_LRUD                  = '[Note]: LRUD disproportionnée - Vérifier ces valeurs';

  rsCDR_SERIE_NB_ANTENNES_MODIFIEES          = '%d visées rayonnantes modifiées';
  rsCDR_SERIES_MSG_ERROR_LONGUEURS_ADMISES   = ' %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_ATTRIBUTED_NO_SERIE = 'Numéro de série déjà attribué';
  rsCDR_SERIES_MSG_ERROR_ORPHAN_SERIE        = 'Série non raccordée au réseau';
  rsCDR_SERIES_MSG_ERROR_CHECKING_SERIE      = 'Vérification des valeurs';
  rsCDR_SERIES_MSG_ERROR_AZIMUT_OUT_OF_RANGE = 'Azimut %.3f invalide; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_PENTE_OUT_OF_RANGE  = 'Pente %.3f invalide; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_NONEXISTENT_SECTEUR = 'Secteur %d inexistant';
  rsCDR_SERIES_MSG_ERROR_NONEXISTENT_RESEAU  = 'Réseau %d inexistant';
  rsCDR_SERIES_MSG_ERROR_NONEXISTENT_CODE    = 'Code %d inexistant - Contrôles angulaires inopérants';
  rsCDR_SERIES_MSG_ERROR_NONEXISTENT_EXPE    = 'Séance %d inexistant';
  rsCDR_SERIES_MSG_ERROR_INVALID_TYPE_VISEE  = 'Type de visee %d incorrect; valeurs admises: %d à %d';
  rsCDR_SERIES_MSG_ERROR_INVALID_LONGUEUR    = 'Longueur %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_INVALID_LG          = 'LG %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_INVALID_LD          = 'LD %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_INVALID_HZ          = 'HZ %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_INVALID_HN          = 'HN %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  // *******************************************************
  // resoursestring du cadre Antennes (Maintenance)
  rsCDR_ANTENNES_AC_IMPORT_CSV               = 'Ouvrir un fichier texte CSV';
  rsCDR_ANTENNES_AC_ADDLINE                  = 'Ajouter une ligne';
  rsCDR_ANTENNES_AC_DELLINE                  = 'Supprimer une ligne';
  rsCDR_ANTENNES_AC_SAVEGRD                  = 'Valider les modifications';
  rsCDR_ANTENNES_DEL_LINE                    = 'Supprimer la ligne %d';
  rsCDR_ANTENNES_FIND_RESEAU                 = 'Réseau pour la ligne %d';
  rsCDR_ANTENNES_FIND_CODE                   = 'Code pour la ligne %d';
  rsCDR_ANTENNES_FIND_EXPE                   = 'Expé pour la ligne %d';
  rsCDR_ANTENNES_ATTRIB_CODES_EXPES          = 'Réattribuer les codes et séances';
  rsCDR_ANTENNES_NETTOYER_ANTENNES           = 'Supprimer les visées invalides';
  rsCDR_ANTENNES_VIDER_LISTE_ANTENNES        = 'Vider toute la liste';
  rsCDR_ANTENNES_REFRESH_LISTE               = 'Rafraîchir la liste';
  rsCDR_ANTENNES_SELECT_CODE                 = 'Code instruments';
  rsCDR_ANTENNES_SELECT_EXPE                 = 'Séance';
  rsCDR_ANTENNES_SELECT_RESEAU               = 'Réseau';
  rsCDR_ANTENNES_SELECT_SECTEUR              = 'Secteur';
  rsCDR_ANTENNES_MSG_DATA_WILL_BE_ERASED     = 'Certaines visées seront remplacées - Continuer';
  rsCDR_ANTENNES_LB_MAX_LENGTH_ANTENNE       = 'Longueur maxi';
  rsCDR_ANTENNES_CONFIRM_NETTOYAGE           = 'Confirmer le nettoyage des antennes';
  rsCDR_ANTENNES_CONFIRM_SIMPLIFICATION      = 'Confirmer la simplification des antennes';
  rsCDR_ANTENNES_SIMPLIFY_FOR_ALL_RESEAU     = 'Pour tout le réseau';
  rsCDR_ANTENNES_SIMPLIFY_FOR_SERIE          = 'Pour la série';
  rsCDR_ANTENNES_SIMPLIFY_FOR_STATION        = 'Pour la station';





  rsCDR_ANTENNES_LB_NB_ANTENNES              = 'Traitement des %d visées rayonnantes';
  // *******************************************************
  // resoursestring du cadre Liste des Antennes
  rsCDR_LISTE_ANTENNES_DELETE_SELECTED       = 'Supprimer les visées sélectionnées';
  rsCDR_LISTE_ANTENNES_RELISTER              = 'Relister';
  rsCDR_LISTE_ANTENNES_NETTOYER              = 'Supprimer les visées invalides';
  rsCDR_LISTE_ANTENNES_NEXT_STATION          = 'Station suivante';
  rsCDR_LISTE_ANTENNES_PREV_STATION          = 'Station précédente';
  rsCDR_LISTE_ANTENNES_SAVE_CSV              = 'Exporter en CSV';
  rsCDR_LISTE_ANTENNES_GOTO_NTH_STATION      = 'Aller à la station';
  rsCDR_LISTE_ANTENNES_ADD_ANTENNE           = 'Ajouter une visée radiante';
  rsCDR_LISTE_ANTENNES_NONE                  = 'Aucune visée radiante';



  // *******************************************************
  // resourcestring du cadre CdrNavigateurDB
  rsCDR_NAVIG_DB_DO_SORT                     = 'Trier';
  rsCDR_NAVIG_DB_DO_ADD                      = 'Ajouter élément';
  rsCDR_NAVIG_DB_DO_DELETE                   = 'Supprimer élément';
  // *********************************
  // resourcestring du dialogue Vtopo
  rsVTOPO_EDPREFIX                           = 'Préfixe des stations';
  rsVTOPO_LBIDPTDEP                          = 'No point de départ';
  rsVTOPO_LBREPORTER                         = 'Opérateur du report';
  rsLBFICHIER                                = 'Fichier';
  rsLBMAINENTRANCE                           = 'Entrée principale';
  rsLBIDMAINST                               = 'Station de départ';
  rsLBSTPREFIX                               = 'Préfixe des stations';
  rsLBREPORTER                               = 'Report';
  rsLBMAINENTCOORD                           = 'Coordonnées de l''entrée principale';
  rsLBCOLORDEFAULT                           = 'Couleur par défaut';
  // *******************************************************
  // resourcestring du visualisateur OpenGL
  rsOGLVIEWERTITLE                           = 'Visualisateur OpenGL [%s]';
  rsLBLBACKCOLOR                             = 'Arrière-plan';
  rsLBLCUBECOLOR                             = 'Cube';
  rsLBLREFCOLOR                              = 'Référentiel';
  rsLBLREFSIZE                               = 'Taille';
  rsOPENGLERROR                              = 'Erreur OpenGL';
  rsOGLVQUIT                                 = 'Quitter le visualisateur OpenGL';
  // *******************************************************
  // resourcestrings de l''outil d''exportation graphique

  rsDODISPDESELECTEDPARTS                    = 'Afficher les parties refusées par le MétaFiltre - Couleur';
  rsTAB_LAYERS                               = 'Couches';
  rsTAB_QUADR                                = 'Quadrillage';
  rsTAB_DESSIN                               = 'Dessin';
  rsTAB_TEXTES                               = 'Textes';
  rsTYPEQDR                                  = 'Type de quadrillage';
    rsQDNONE                                 = 'Aucun';
    rsQDGRID                                 = 'Grille';
    rsQDCROSS                                = 'Croix';
    rsQDPOINTS                               = 'Points';
  rsMODE_DESSIN_MAILLAGE                     = 'Dessin du maillage';
    rsMNT_DM_NONE                            = 'Aucun';
    rsMNT_DM_WIREFRAME                       = 'Fil de fer';
    rsMNT_DM_TRANSPARENCE                    = 'Transparence';


  rsPSDXF_TITLE                              = 'Export graphique: [%s]';
    rsGRAPHICS_PS                            = 'PostScript PS';
    rsGRAPHICS_DXF                           = 'AutoCAD DXF';
    rsGRAPHICS_SVG                           = 'Scalable Vector Graphics SVG';
    rsGRAPHICS_WMF                           = 'Windows MetaFile WMF';
  rsDLGDXF_TITLE                             = 'Export DXF: %s';
  // *******************************************************
  // mots clefs du MétaFiltre
  rsHLPMETAFILTRE                            = 'METAFILTRE';
  rsMETAFILTRE_APPLY                         = 'Appliquer';
    rsMETAFILTRE_NIL                         = 'RIEN';
    rsMETAFILTRE_ALL                         = 'TOUT';
    rsMETAFILTRE_ID                          = 'No';
    rsMETAFILTRE_LENGTH                      = 'LONGUEUR';
    rsMETAFILTRE_AZIMUTH                     = 'GISEMENT';
    rsMETAFILTRE_PENTE                       = 'PENTE';
    rsMETAFILTRE_DATE                        = 'DATE';
    rsMETAFILTRE_COLOR                       = 'COULEUR';
    rsMETAFILTRE_X                           = 'COORD_X';
    rsMETAFILTRE_Y                           = 'COORD_Y';
    rsMETAFILTRE_Z                           = 'COORD_Z';
    rsMETAFILTRE_LARGEUR                     = 'LARGEUR';
    rsMETAFILTRE_HAUTEUR                     = 'HAUTEUR';
    rsMETAFILTRE_DATES                       = 'DATES';
    rsMETAFILTRE_COLORS                      = 'COULEURS';
    rsMETAFILTRE_SERIE                       = 'SERIE';
    rsMETAFILTRE_RESEAU                      = 'RESEAU';
    rsMETAFILTRE_CODE                        = 'CODE';
    rsMETAFILTRE_EXPE                        = 'SEANCE';
    rsMETAFILTRE_TYPEVISEE                   = 'TYPE_VISEE';
    rsMETAFILTRE_SECTEUR                     = 'SECTEUR';
    rsMETAFILTRE_ENTREE_RATT                 = 'ENTREE_RATTACHEMENT';
    rsMETAFILTRE_NAMESPACE                   = 'NAMESPACE';

  rsMSG_METAFILTRE_ALL_REJECTED              = 'Le MétaFiltre a rejeté toutes les visées';
  // *******************************************************
  // recherche de stations
  rsMSG_FIND_STATION_TITRE                   = 'Recherche de station';
  rsMSG_FIND_STATION_PROMPT                  = 'Entrer un repère terrain ou un couple série et station (séparateur: point décimal)';
  // *******************************************************
  // resourcestring du dialogue du MétaFiltre
  rsDLGMETAFILTRE_TBS1                       = 'Dates';
  rsDLGMETAFILTRE_TBS2                       = 'Couleurs';
  rsDLGMETAFILTRE_PERSO                      = 'Personnalisés';
  // ******************************************************
  // resourcestring du sélecteur de couleurs
  rsSELECTCOLORTITLE                         = 'Selection d''une couleur';
  rsLBLUSEDCOLORS                            = 'Dernières couleurs:';
  rsDLGCOULSAVEPAL                           = 'Enregistrer palette';
  rsDLGCOULRESTPAL                           = 'Restaurer palette';
  rsDLGCOULFILTERPAL                         = 'Fichiers de palette (*.pal)|*.pal|Tous (*.*)|*.*';
  rsDLGCOUDELPAL                             = 'Ecraser le fichier existant';
  rsPALETTENOTFOUNT                          = 'Palette introuvable';
  // ******************************************************
  // resourcestring de l''utilitaire d''export graphique
  rsDLGGRAPHIC_OUTPUTFMT                     = 'Format de sortie:';
  rsDLGGRAPHIC_LBFILENAME                    = 'Nom de fichier:';
  rsDLGGRAPHIC_LBOBS                         = 'Commentaires:';
  rsDLG_GRAPHIC_TABTITLE                     = 'Export graphique';
  rsDLGGRAPHIC_GBCROIX                       = 'Quadrillage';
  rsDLGGRAPHIC_GBCHEM                        = 'Cheminements et sections';
  rsDLGGRAPHIC_GBWALLS                       = 'Parois et couleurs';
  rsDLGGRAPHIC_SPACING                       = 'Espacement';
  rsDLGGRAPHIC_TYPEGRID                      = 'Type:';
  rsDLGGRAPHIC_CMBGRD2                       = 'Croix';
  rdDLGGRAPHIC_LAYER                         = 'Couche:';
  rdDLGGRAPHIC_WALLFILL                      = 'Remplissage';
  rsDLGGRAPHIC_WFILL1                        = 'Plein (1 couleur)';
  rsDLGGRAPHIC_WFILL2                        = 'Types de galeries';
  rsDLGGRAPHIC_WFILL3                        = 'Couleurs des visées';
  rsDLGGRAPHIC_WFILL4                        = 'Par réseaux';
  rsDLGGRAPHIC_WFILL5                        = 'Par dates';
  rsDLGGRAPHIC_CHKCHEM                       = 'Exporter cheminements';
  rsDLGGRAPHIC_CHKSECT                       = 'Exporter sections';
  // ******************************************************
  // resourcestring du sélecteur de listes
  rsSELECT_LISTE_ENTRANCE                    = 'Sélection d''une entrée';
  rsSELECT_LISTE_RESEAU                      = 'Sélection d''un réseau';
  rsSELECT_LISTE_SECTEUR                     = 'Sélection d''un secteur';
  rsSELECT_LISTE_CODE                        = 'Sélection d''un code instruments';
  rsSELECT_LISTE_EXPE                        = 'Sélection d''une séance';
  rsSELECT_LISTE_SERIE                       = 'Sélection d''une série';
  // ******************************************************
  // resourcestring du centre d''impression
  rsPRN_NOPRINTER                            = 'Pas d''imprimante installée';
  rsPRN_TBPRINTER                            = 'Imprimante';
  rsPRN_TBDRAW                               = 'Dessin';
  rsPRN_TITLE                                = 'Centre dimpression [%s]';
  rsPRN_CHKPOLY                              = 'Polygonales';
  rsPRN_CHKFILL                              = 'Remplissage';
  rsPRN_CHKWALLS                             = 'Parois';
  rsPRN_CHKSECTS                             = 'Sections';
  rsPRN_CHKSTATIONS                          = 'Stations';
  rsPRN_CHKSTATIONS_LBL                      = 'No stations';
  rsPRN_CHKALTITUDE                          = 'Altitude';
  rsPRN_CHKCOTE                              = 'Cotes';
  rsPRN_CHKQUADRILLAGE                       = 'Quadrillage';
  rsPRN_CHKENTREES                           = 'Entrees';
  rsPRN_CHKANNOTATIONS                       = 'Annotations';
  rsPRM_LBANGLEROT                           = 'Angle de rotation';
  rsPRN_TYPEQDR                              = 'Type de quadrillage';
  rsPRN_QDNONE                               = 'Aucun';
  rsPRN_QDCROSS                              = 'Croix';
  rsPRN_QDQUADRILLES                         = 'Grille';
  rsPRN_QDPOINTS                             = 'Points';
  rsPRN_SCALING                              = 'Echelle';
  rsPRN_LBSPACING                            = 'Espacement';
  rsLANDSCAPE                                = 'Paysage';
  rsPORTRAIT                                 = 'Portrait';
  rsPRN_START_PRINTING                       = 'Démarrage de l''impression';

  rsDLGIMP_TAB1                              = 'Aperçu';
  rsDLGIMP_TAB2                              = 'Imprimante';
  rsDLGIMP_TAB3                              = 'Options de dessin';
  rsDLGIMP_TAB4                              = rsITEM_RESEAUX;
  rsQDRSPACING                               = 'Espacement';
  rsECHELLE                                  = 'Echelle: 1 /';
  rsLAYERS                                   = 'Couches de dessin';
  rsPREVIEW                                  = 'Previsualisation';
  rsSTARTPRINTING                            = 'Lancer impression';
  rsREGLE                                    = 'Règle';
  // -------------------------------------------------
  // resourcestring du système d'aide
  rsHLP_TITRE                                = 'Système d''aide de GHTopo';
  rsHLP_BNEDIT                               = 'Editer le fichier d''aide';
  rsHLP_DOCONTINUE                           = 'Souhaitez-vous continuer ?';
  rsHLP_TAB_RUBRIQUES                        = 'Rubriques d''aide';
  rsHLP_TAB_ABOUT                            = 'A propos de GHTopo';
  // -------------------------------------------------
  // resourcestring du dialogue Serie/station
  rsDLG_SERST_TITLE                          = 'Recherche de station';
  rsDLG_SERST_SERIE                          = 'Série';
  rsDLG_SERST_STATION                        = 'Station';
  rsDLG_SERST_CLE                            = 'Code terrain';
  rsDLG_SERST_LBSEARCH                       = 'Recherche';
  rsDLG_SERST_BYSERST                        = 'Par couple série / station';
  rsDLG_SERST_BYREFTER                       = 'Par référence de terrain';
  // -------------------------------------------------
  // resourcestring du dialogue Editeur d''annotations
  rsDLG_ANN_TITLE                            = 'Editeur d''annotations';
  rsDLG_ANN_LBTEXTE                          = 'Annotation';
  rsDLG_ANN_LBMAX                            = 'Longueur max';
  rsDLG_ANN_CHKTXTDISP                       = 'Dessiner cette annotation';
  rsDLG_ANN_GRBPOSTEXT                       = 'Positionnement du texte';
  rsDLG_ANN_GRBMETH0                         = 'Coordonnées absolues';
  rsDLG_ANN_GRBMETH1                         = 'Accroché à la station';
  rsDLG_ANN_LBPOSTEXTE                       = 'Point de base';
  rsDLG_ANN_LBSTATION                        = 'Station topo';
  rsDLG_ANN_LBOFFSET                         = 'Décalage (m) : X = %.2f m ; Y = %.2f m';
  rsDLG_ANN_GRBATTRTXT                       = 'Attributs de caractères';
  rsDLG_ANN_GRBBASEPT                        = 'Point de base du texte';
  // -------------------------------------------------
  // recherche de station par ID littéral
  rsDLG_FIND_PT_BY_ID_TITLE                  = 'Recherche par ID littéral';
  rsDLG_FIND_PT_BY_ID_PROMPT                 = 'Code terrain';
  rsDLG_FIND_PROMPT                          = 'Texte recherché';
  rsDLG_FIND_ENTRANCE_BY_TEXT                = 'Recherche d''une entrée';
  rsDLG_FIND_RESEAU_BY_TEXT                  = 'Recherche de réseau';
  rsDLG_FIND_CODE_BY_TEXT                    = 'Recherche de code instruments';
  rsDLG_FIND_EXPE_BY_TEXT                    = 'Recherche de séance topo';
  rsDLG_FIND_SERIE_BY_TEXT                   = 'Recherche de série';
  rsDLG_FIND_SECTEUR_BY_TEXT                 = 'Recherche de secteur';
  // accès par numéro
  rsDLG_GOTO_PROMPT                          = 'Numéro';
  rsDLG_GOTO_ENTRANCE_BY_NUMERO              = 'Aller à une entrée';
  rsDLG_GOTO_RESEAU_BY_NUMERO                = 'Aller à un réseau';
  rsDLG_GOTO_SECTEUR_BY_NUMERO               = 'Aller à un secteur';
  rsDLG_GOTO_CODE_BY_NUMERO                  = 'Aller à un code instruments';
  rsDLG_GOTO_EXPE_BY_NUMERO                  = 'Aller à une séance topo';
  rsDLG_GOTO_SERIE_BY_NUMERO                 = 'Aller à une série';

  // resourcestring de l'outil calculette/convertisseurs
  rsDLG_CALC_TITLE                           = 'Calculateurs et outils complémentaires';
  rsDLG_CALC_TAB_CAL                         = 'Calculatrice';
  rsDLG_CALC_EXPR                            = 'Entrer une expression';
  rsDLG_CALC_DOCALC                          = 'Calculer';
  rsDLG_CALC_CDR_CONVERT                     = 'Convertisseur de coordonnées';
  rsDLG_CALC_CDR_DECLIMAG                    = 'Déclinaison magnétique';
  rsDLG_CALC_BTN_CONVERT                     = 'Convertir';
  rsDLG_CALC_BTN_CONVERT_TABLEAU             = 'Convertir les points';
  rsDLG_CALC_BTN_ECHANGE                     = 'Echanger';
  rsDLG_CALC_BTN_OPEN_CSV                    = 'Charger fichier CSV';
  rsDLG_CALC_BTN_PASTE                       = 'Coller vers le tableau';
  rsDLG_CALC_BTN_EXPORT_CSV                  = 'Exporter le tableau en CSV';
  rsDLG_CALC_BTN_EXPORT_GIS                  = 'Exporter les points en KML, GPX ou OSM';
  rsDLG_CALC_BTN_QRCODE_PT                   = 'Générer le QRCode de ce point';
  rsDLG_CALC_BTN_QRCODE_SEL                  = 'Générer le QRCode de la sélection';
  rsDLG_CALC_BTN_COPY_COORDS_ISOLEES         = 'Copier ces coordonnées';
  rsDLG_CALC_BTN_QRCODE_FROM_TEXT            = 'Générer un QRCode depuis le texte';
  rsDLG_CALC_BTN_QRCODE_TO_SVG               = 'Créer un SVG depuis le QRCode';
  rsDLG_CALC_LB_SYST_SOURCE                  = 'Du système';
  rsDLG_CALC_LB_SYST_CIBLE                   = 'Vers le système';

  rsDLG_CALC_LB_COORDS_EN_ENTREE             = 'Valeurs initiales';
  rsDLG_CALC_LB_COORDS_EN_SORTIE             = 'Valeurs calculées';

  rsDLG_CALC_HINT_GRD_CONVERSIONS            = 'Saisir des valeurs dans la grille ou coller depuis le presse-papiers';
  rsDLG_CALC_EXPRESSION                      = 'Expression';
  rsDLG_CALC_SYSCOORDS                       = 'Systèmes de coordonnées';
  rsDLG_CALC_LSB_FUNCTIONS                   = 'Liste des fonctions';
  rsDLG_CALC_LSB_CALCULS                     = 'Liste des calculs';
  rsDLG_CALC_LSB_VARIABLES                   = 'Liste des variables';
  rsDLG_CALC_COORDS_UNITAIRE                 = 'Coordonnées isolées';
  rsDLG_CALC_COORDS_TABLEAU                  = 'Conversion en rafale';
  rsDLG_CALC_DECL_UNITAIRE                   = 'Déclinaison magnétique';
  rsDLG_CALC_DECL_TABLEAU                    = 'Tableau de déclinaisons';
  rsDLG_CALC_TAB_QRCODE                      = 'Génération de QRCodes';
  rsDLG_CALC_TAB_PASCAL_SCRIPT               = 'Interpréteur PascalScript';
  rsDLG_CALC_TAB_TEXTFILES                   = 'Import de fichiers textes';
  rsDLG_CALC_TAB_MAILLAGES_TIN               = 'Utilitaires de maillages TIN';


  rsDLG_CALC_TAB_COORDS_QRCODE_LISTE         = 'Générer un QRCode de ces coordonnées';
  rsDLG_CALC_BTN_PASCAL_SCRIPT_DO_RUN        = 'Exécuter';

  rsDLG_CALC_ACN_OSM_REMOVE_ALL_LAYERS       = 'Supprimer toutes les couches additionnelles';
  rsDLG_CALC_ACN_OSM_ADD_LAYER               = 'Ajouter une couche';
  rsDLG_CALC_ACN_OSM_EDIT_LAYER              = 'Editer une couche';
  rsDLG_CALC_ACN_OSM_COPY_LAYERS             = 'Exporter les couches vers le presse papiers';
  rsDLG_CALC_ACN_OSM_PASTE_LAYERS            = 'Importer les couches depuis le presse papiers (s''ajoutent aux existantes)';


  rsDLG_CALC_ACN_OSM_REMOVE_LAYER            = 'Supprimer une couche';
  rsDLG_CALC_ACN_OSM_EXTRACT_LAYERS          = 'Ajouter des couches depuis le tableau';
  rsDLG_CALC_ACN_OSM_CONFIRM_CLEAR_LAYERS    = 'La liste des couches n''est pas vide - Continuer';
  rsDLG_CALC_ACN_OSM_CONFIRM_DELETE_LAYER    = 'Supprimer la couche %d: %s';

  rsDLG_CALC_TAB_COLOR_UTILS                 = 'Couleurs';
  rsDLG_CALC_TAB_RESOL_TRIANGLES             = 'Résolution de triangles';
  rsDLG_CALC_TAB_CARACTERISTIQUES_SECTIONS   = 'Aires et inerties';

  // onglet PascalScript
  rsTAB_PS_AC_RUN                            = 'Exécuter le script';
  rsTAB_PS_AC_STOP                           = 'Arrêter le script';
  rsTAB_PS_AC_NEW                            = 'Nouveau script';
  rsTAB_PS_AC_OPEN                           = 'Ouvrir un script';
  rsTAB_PS_AC_SAVE                           = 'Sauvegarder le script';
  rsTAB_PS_AC_COPY_OUTPUT                    = 'Copier la console de sortie';
  rsTAB_PS_TabShtConsoleOutput_CAPTION       = 'Console de sortie du script';
  rsTAB_PS_TabShtMessagesCompilo_CAPTION     = 'Messages de l''interpréteur';
  // onglet Métadonnées d'une image
  rsDLG_CALC_TAB_IMAGE_METADATA              = 'Métadonnées d''une image';
  //****************************************************************************

  rsLB_DOCUMENT_TITLE                        = 'Titre du document';




  // gestion du presse papiers pour l''import de données dans la grille Stations
  rsDLG_CLIPBRD_PTS_TITLE                    = 'Importation de points topo';

  // resourcestring du cadre Série
  rsCDR_SERIES_ADD_SERIE                     = 'Ajouter une série';
  rsCDR_SERIES_DELETE_SERIE                  = 'Supprimer cette série';
  rsCDR_SERIES_EDIT_SERIE                    = 'Edition de la série %d - %s [%d]';

  rsCDR_SERIES_VALID_SERIE                   = 'Enregistrer les modifications de la série courante';
  rsCDR_SERIES_SORT_SERIES                   = 'Trier les séries dans l''ordre croissant';
  rsCDR_SERIES_EXPORT_CSV                    = 'Exporter les en-têtes dans un fichier CSV';
  rsCDR_SERIES_HELP                          = 'Aide';

  rsDLG_BDD_APPLY_MODIFS                     = 'Modifier la ligne courante';
  rsDLG_BDD_FIND                             = 'Rechercher';
  rsDLG_BDD_GOTO_BY_NUMERO                   = 'Aller à l''item numéro n';
  rsDLG_BDD_SORT                             = 'Trier';
  rsDLG_BDD_RECHECK                          = 'Vérifier la base à nouveau';
  rsDLG_BDD_GOTO_ERROR                       = 'Aller à l''erreur';
  rsDLG_BDD_EXPORT_CSV                       = 'Exporter la liste en CSV';
  rsDLG_BDD_HELP_LISTES                      = 'Aide';

  // resourcestring du cadre Listes simples

  rsDLG_BDD_REMOVE_ENTREE                    = 'Supprimer une entrée';
  rsDLG_BDD_REMOVE_RESEAU                    = 'Supprimer un réseau';
  rsDLG_BDD_REMOVE_SECTEUR                   = 'Supprimer un secteur';
  rsDLG_BDD_REMOVE_CODE                      = 'Supprimer un code instruments';
  rsDLG_BDD_REMOVE_EXPE                      = 'Supprimer une séance';
  rsDLG_BDD_REMOVE_SERIE                     = 'Supprimer une série';
  rsDLG_BDD_REMOVE_BOUCLAGE                  = 'Supprimer le bouclage';
  rsDLG_BDD_LIBERE_SERIE                     = 'Libérer l''extrémité de la série';

  rsDLG_BDD_SORT_BOUCLAGE                    = 'Trier le bouclage';

  rsDLG_BDD_ADD_ENTREE                       = 'Ajouter une entrée';
  rsDLG_BDD_ADD_RESEAU                       = 'Ajouter un réseau';
  rsDLG_BDD_ADD_SECTEUR                      = 'Ajouter un secteur';
  rsDLG_BDD_ADD_CODE                         = 'Ajouter un code instruments';
  rsDLG_BDD_ADD_EXPE                         = 'Ajouter une séance';
  rsDLG_BDD_ADD_SERIE                        = 'Ajouter une série';
  rsDLG_BDD_ADD_BOUCLAGE                     = 'Créer un bouclage';

  rsDLG_BDD_POI_EDIT                         = 'Editer le point d''intérêt';
  rsDLG_BDD_POI_REMOVE                       = 'Supprimer le point d''intérêt';


  (*
  rsDLG_BDD_INSERT_ENTREE                    = 'Insérer une entrée ?';
  rsDLG_BDD_INSERT_RESEAU                    = 'Insérer un réseau ?';
  rsDLG_BDD_INSERT_SECTEUR                   = 'Insérer un secteur ?';
  rsDLG_BDD_INSERT_CODE                      = 'Insérer un code instruments ?';
  rsDLG_BDD_INSERT_EXPE                      = 'Insérer une séance ?';
  rsDLG_BDD_INSERT_SERIE                     = 'Insérer une série ?';
  rsDLG_BDD_INSERT_NoteVersion               = 'Insérer une note de version ?';
  //*)
  // resourcestring du dialogue Graphe du réseau
  rsDLG_GRAPHE_TITLE                         = 'Graphe du réseau';
  rsDLG_GRAPHE_ITINERAIRE_INDEFINI           = 'Itinéraire non défini';

  // messages divers
  rsMSG_HAS_ALREADY_DATA                     = 'Le tableau contient déjà des données. Continuer ?';
  rsDELETEITEM                               = 'Détruire élément ?';
  rsONCLOSEPRJMNGR                           = 'Ceci fermera le document courant - Sauvegarder les modifications ?';
  rsMSG_SEEALSO                              = 'Voir aussi ...';
  rsMSG_NDSNEEDED                            = 'Fichier noeuds inexistant - Recalculer le réseau';
  rsMSG_SAVECHANGES                          = 'Enregistrer les modifications';
  rsMSG_ERASESTATION                         = 'Ecraser la station %d ?';
  rsMSG_FILENOTFOUND                         = 'Fichier %s non trouvé';
  rsMSG_READY                                = 'PRET';
  rsMSG_NOFILEOPENED                         = 'Pas de fichier ouvert';
  rsDISPLAY_HELP_SYSTEM                      = 'Démarrage du système d''aide';
  rsHLPCENTER_TITLE                          = 'Système d''aide de GHTopo';
  rsMATCHNOTFOUND                            = 'Occurrence non trouvée';
  rsNOCANCLOSE                               = 'Quitter en sauvegardant';
  rsWARN_FILE_ALREADY_OPEN                   = 'Un document est déjà ouvert - Poursuivre ?';
  rsWARN_FILE_ALREADY_EXISTS                 = 'Ecraser le fichier %s existant ?';

  rsSAVECHANGES                              = 'Enregistrer les modifications';
  rsDO_RELOAD_DOCUMENT                       = 'Recharger le document';
  rsNOCANCLOSEWND                            = 'Fenêtre permanente de GHTopo, ne peut être fermée';
  rsERASEEXISTNAMEDFILE                      = 'Ecraser le fichier %s ?';
  rsSAVESLOST                                = 'Les modifications seront perdues';
  rsCFGMTFLTR_UNABLE                         = 'Erreur dans le fichier de filtres personnalisés';
  rsERRORSTARTPRNCTR                         = 'Erreur de démarrage du Centre d''Impression';
  rsNOPRINTERINSTALLED                       = 'Aucune imprimante installée';
  rsERRORLOADINGTOP                          = 'Erreur en chargement du fichier TOP';
  rsNUM_SERIE_ALREADY_ATTRIBUTED             = 'Numéro de série %d déjà attribué';
  rsALLOW_ATTRIBUTE_NUM_SERIE                = 'Laisser GHTopo attribuer un numéro de série';
  rsFAILED_TO_ADD_SERIE                      = 'Echec en création de la série';
  // Messages d''erreur
  rsMSG_VUE3D_FAIL                           = 'Echec au démarrage de la vue 3D';
  rsMSG_VUE2D_FAIL                           = 'Echec au démarrage de la vue en plan';
  rsMSG_STATS_DLG_FAIL                       = 'Echec au démarrage du dialogue de statistiques';
  rsMSG_PRINT_CENTER_FAIL                    = 'Echec au démarrage du centre dimpression';
  rsMSG_PROJ_MANAGER_FAIL                    = 'Echec au démarrage du frontal de gestion de la base';
  rsMSG_PROJ4S_FAIL                          = 'Echec au démarrage du convertisseur de coordonnées Proj4s';
  rsMSG_DECLIMAG_FAIL                        = 'Echec au démarrage du calculateur de déclinaison magnétique';
  rsMSG_TABLE_ENTITY_FAIL                    = 'Echec au chargement de la table des entités';
  rsMSG_CODE_NOT_FOUND                       = 'Code introuvable';
  rsMSG_EXPE_NOT_FOUND                       = 'Expé introuvable';
  rsMSG_RESEAU_NOT_FOUND                     = 'Réseau introuvable';
  rsMSG_SECTEUR_NOT_FOUND                    = 'Secteur introuvable';
  rsMSG_ANGLE_OUT_OF_RANGE                   = '%s doit être compris entre %.2f et %.2f';
  rsMSG_WARN_DELETE_ITEM                     = 'La suppression de l''objet %d peut désorganiser la base. Continuer ?';
  // Messages dans TToporobotStructure.LoadFichierTab()
  rsRD_TAB_MSG_ERR                           = 'Erreur lors du traitement de la ligne';
  rsRD_TAB_D_MULTI_OBS                       = 'Début de commentaires multilignes en ligne %d';
  rsRD_TAB_F_MULTI_OBS                       = 'Fin de commentaires multilignes en ligne %d';
  rsRD_TAB_STOP_9999                         = '*** Line %d: Lecture arrêtée par le code -9999 ***';
  rsRD_TAB_LN_OBS                            = '-- La ligne %d est un commentaire';
  rsRD_TAB_LASTSAVES                         = 'Dernière sauvegarde: %s %s';
  rsRD_TAB_CLASSEURS                         = '-7 [Classeurs] Cette fonctionnalité est ignorée';
  rsRD_TAB_ENTRANCE                          = 'Entrée créée #%d';
  rsRD_TAB_ENTR_NOGEOREF                     = '[Avertissement] (Ligne %d): Entrée %d (%s) non géoréférencée';
  rsRD_TAB_ENTR_BADLINK                      = '[Avertissement] (%d): Entrée %d (%s): Raccordement incorrect [Série: %d - Station: %d]';
  rsRD_TAB_IGNORED_SEC                       = '[Information] Section ignorée';
  rsRD_TAB_BAD_TRIP                          = 'Séance incorrecte';
  rsRD_TAB_BAD_CODE                          = 'Code incorrect';
  rsRD_TAB_BAD_DATE                          = '[Avertissement] (%d) - Date incorrecte mise à la date actuelle';
  rsRD_TAB_BAD_IDXRES                        = '[Avertissement] (%d) - Index de réseau incorrect (%d) pour la série %d - Mis à 0';
  rsRD_TAB_IDEXTR_MM                         = '[Avertissement] (%d) - Les ID dextrémité de la série %d sont identiques';
  rsRD_TAB_SELF_CLOSURE                      = '[Avertissement] (%d) - Le terminus de la série %d se branche sur elle-même.';
  rsRD_TAB_NEG_LONG                          = '[Avertissement] (%d) - Longueur négative (%.2f m), changée de signe';
  rsRD_ERROR_LN                              = '*** Erreur en ligne #%d: %s';
  rsRD_CONTNT_LN                             = 'Contenu de la ligne: %s';
  rsRD_TAB_FIELD_VALUES                      = 'Valeurs des champs:';
  rsRD_TAB_NOFATAL_ERR                       = 'Le fichier ne comporte pas d''erreurs fatales';
  rsRD_TAB_WTFATAL_ERR                       = 'Le fichier comporte %d erreurs';
  // *************************************************************
  // resourcestring du code de calcul
  rsDEFAULT                                  = 'Défaut';
  rsFORFIXEDPOINTS                           = 'Item pour entrees et points fixes';
  rsWARNINGENTRYADDED                        = 'Le nombre d''entrées dans -6 est différent de celui de -5 Corrigé.';
  rsBIND_MATRICIAL_PROBLEM                   = 'MISE EN PLACE DU PROBLEME MATRICIEL';
  rsLINEOFNB                                 = '---> Ligne %d / %d';
  rsBIND_INCIDENCE_MATRIX                    = 'MATRICE D''INCIDENCE: R';
  rsNB_NON_NUL_TERMES                        = '%d termes non nuls dans la matrice [%s]';
  rsPR100_NON_NULS                           = 'soit %.3f%% de (%d x %d) = %d termes';
  rsFINDING_BRANCHES                         = 'Recensement des branches';
  rsADDENTRANCES                             = 'Ajout des entrees';
  rsELAGAGE_BRANCHES                         = 'Elagage des branches en cul de sac';
  rsCALCUL_BRANCHES_ELAGUEES                 = 'Calcul branches en cul de sac';
  rsBUILDMATRICE                             = 'CONSTRUCTION DE LA MATRICE DE COMPENSATION: B = Rt.Wi.R:';
  //rsBUILDMATRICE_LAPLACIENNE                 = 'CONSTRUCTION DE LA MATRICE LAPLACIENNE: B = Rt.R';

  rsFIND_SUM_LIMITS                          = '-- Recherche des limites utiles de sommation';
  rsPRODUIT_MAT_Rt_W_R                       = '-- Produit matriciel B = Rt.W.R';

  rsFACTORISEMATRICE                         = 'FACTORISATION DE LA MATRICE DE COMPENSATION';
  rsCALC_COORDS_NOEUDS                       = 'CALCUL DES COORDONNEES DES NOEUDS';
  rsAXIS                                     = 'Axe:';
  rsDESCENTE                                 = '--> Descente: L.L* = B';
  rsREMONTEE                                 = '--> Remontee du systeme';
  rsTRIANGULARISATION                        = '--> Triangularisation';
  rs2NDMEMBER                                = '- Second membre';
  rsNODECOORDINATES                          = '- Phase de remontée: Coordonnées des noeuds';
  rsCOORDONNEES_OK                           = '--> Coordonnées des %d noeuds OK';
  rsDEL_TEMP_MATRIX                          = '--> Destruction des matrices temporaires';
  rsWRITE_NODES_COORDS                       = 'Ecriture des coordonnées des noeuds dans:';
  rsNODES_COORDS_FOR_DB                      = 'Coordonnées des noeuds pour:';
  rsCALCULCONTOURS                           = 'CALCUL CONTOURS GALERIES';
  rsCALCUL_ANTENNES                          = 'Calcul des visées en antenne (%d)';

  rsRECENSEM_JONC                            = '-- Recensement des jonctions';
  rsPURGE_TABLE_JONC                         = '-- Purge table des jonctions';
  rsPURGE_TABLE_BRCH                         = '-- Purge table des branches';
  rsNB_BRCHS                                 = '%d branches';

  rsFREE_TEMP_VARS                           = 'LIBERATION DES VARIABLES TEMPORAIRES';
  rsREPARTIR_ECARTS                          = 'REPARTITION DES ECARTS';
  rsSCAN_BRCHS                               = '-- Balayage des branches';
  rsCHECK_VISEE_VALID_INTERVAL               = '%s doit être compris entre %.2f %s et %.2f %s';
  rsCHECK_VISEE_VALID_TYPE_VISEE             = 'Le type de visée doit être compris entre %d et %d';

  // lecture des fichiers
  rsSEPARATOR_LINE                           = '-------------------------------';
  rsWARNINGS_READFILE                        = 'Avertissements de lecture de:';
  rsCONV_TAB_MAC_UNIX                        = '-> Conversion des fichiers TAB pouvant venir du Mac ou d''Unix';
  // -------------------------------------------------

  // -------------------------------------------------
  // Misc
  rsSELECTALL                                = 'Tout sélectionner';
  rsDESELECTALL                              = 'Tout désélectionner';
  rsOBS                                      = 'Commentaires';
  rsFILTERS                                  = 'Filtres';
  rsCHKSELECTALL                             = 'Sél. tout';
  rsCHKDESELECTALL                           = 'Désél. tout';
  rsCHKREVERSE                               = 'Inv. sél.';
  rsMAIN_NETWORK                             = 'Réseau principal';
  // -------------------------------------------------
  // resourcestrings de dlgSelectDansListes
  rsSELECT_LISTE_NAMESPACE                   = rsITEM_NAMESPACES;
  rsSELECT_LISTE_NAMESPACE_COULEUR           = 'Couleur';
  rsSELECT_LISTE_NAMESPACE_NOM               = 'Nom';
  rsSELECT_LISTE_NAMESPACE_DESCRIPTION       = 'Description';



  rsSELECT_LISTE_GENERAL_ID                  = 'Numéro';
  rsSELECT_LISTE_NB_ELEMENTS                 = 'Liste des %d %s';
  rsSELECT_LISTE_OBSERV                      = 'Observations';
  rsSELECT_LISTE_ENTREES                     = rsITEM_ENTRANCES;
  rsSELECT_LISTE_ENTREES_ID                  = 'Numéro';

  rsSELECT_LISTE_ENTREES_NOM                 = 'Entrée';
  rsSELECT_LISTE_ENTREES_REF                 = 'Réf.';
  rsSELECT_LISTE_ENTREES_X                   = 'X';
  rsSELECT_LISTE_ENTREES_Y                   = 'Y';
  rsSELECT_LISTE_ENTREES_Z                   = 'Z';
  rsSELECT_LISTE_RESEAUX                     = rsITEM_RESEAUX;
  rsSELECT_LISTE_RESEAUX_ID                  = 'Numéro';
  rsSELECT_LISTE_RESEAUX_COLOR               = 'Couleur';
  rsSELECT_LISTE_RESEAUX_NOM                 = 'Réseau';
  rsSELECT_LISTE_SECTEURS                    = rsITEM_SECTEURS;
  rsSELECT_LISTE_SECTEURS_ID                 = 'Numéro';
  rsSELECT_LISTE_SECTEURS_COLOR              = 'Couleur';
  rsSELECT_LISTE_SECTEURS_NOM                = 'Secteur';
  rsSELECT_LISTE_CODES                       = rsITEM_CODES;
  rsSELECT_LISTE_CODES_ID                    = 'Numéro';
  rsSELECT_LISTE_CODES_AZIMUTS               = 'Azimuts';
  rsSELECT_LISTE_CODES_CORR_AZIMUTS          = 'Corr. Azimuts';
  rsSELECT_LISTE_CODES_PENTES                = 'Pentes';
  rsSELECT_LISTE_CODES_CORR_PENTES           = 'Corr. Pentes';
  rsSELECT_LISTE_CODES_OBS                   = 'Commentaires';
  rsSELECT_LISTE_EXPES                       = rsITEM_EXPES;
  rsSELECT_LISTE_EXPES_ID                    = 'Numéro';
  rsSELECT_LISTE_EXPES_COULEUR               = 'Couleur';
  rsSELECT_LISTE_EXPES_DATE                  = 'Date';
  rsSELECT_LISTE_EXPES_OPERATEUR_TOPO        = 'Opérateur 1';
  rsSELECT_LISTE_EXPES_CLUB_SPELEO           = 'Equipe ou club';
  rsSELECT_LISTE_EXPES_MODE_DECLINAISON      = 'Déclinaison';
  rsSELECT_LISTE_EXPES_DECLINAISON           = 'Valeur';
  rsSELECT_LISTE_EXPES_OBS                   = 'Commentaires';
  rsSELECT_LISTE_SERIES                      = rsITEM_SERIES;

  rsSELECT_LISTE_SERIES_ID                   = 'Numéro';
  rsSELECT_LISTE_SERIES_DEPART               = 'Départ';
  rsSELECT_LISTE_SERIES_ARRIVEE              = 'Arrivée';
  rsSELECT_LISTE_SERIES_NOM                  = 'Nom';
  rsSELECT_LISTE_SERIES_ENTREE               = 'Entrée';
  rsSELECT_LISTE_SERIES_RESEAU               = 'Réseau';
  rsSELECT_LISTE_SERIES_NBPOINTS             = 'Nb Points';
  rsSELECT_LISTE_SERIES_INTERVALLE           = 'Nb de séries libres';

  rsSELECT_LISTE_DATES                       = 'Dates';
  rsSELECT_LISTE_UNE_DATE                    = 'Date';
  rsSELECT_LISTE_STATIONS                    = 'Points topo';

  rsSELECT_LISTE_STATIONS_ID                 = 'Numéro';
  rsSELECT_LISTE_STATIONS_NB                 = 'Nb stations';
  rsSELECT_LISTE_STATIONS_LABEL              = 'ID littéral';
  rsSELECT_LISTE_STATIONS_X                  = 'X';
  rsSELECT_LISTE_STATIONS_Y                  = 'Y';
  rsSELECT_LISTE_STATIONS_Z                  = 'Z';

  rsSELECT_LISTE_ANNOTATIONS_IDX             = 'No';
  rsSELECT_LISTE_ANNOTATIONS_SERIE_PT        = 'Station';
  rsSELECT_LISTE_ANNOTATIONS_OFFSET_X        = 'dx';
  rsSELECT_LISTE_ANNOTATIONS_OFFSET_Y        = 'dy';
  rsSELECT_LISTE_ANNOTATIONS_OFFSET_Z        = 'dz';
  rsSELECT_LISTE_ANNOTATIONS_IDX_STYLE       = 'Style';
  rsSELECT_LISTE_ANNOTATIONS_ALIGNMENT       = 'Align.';
  rsSELECT_LISTE_ANNOTATIONS_TEXT            = 'Texte';

  rsSELECT_LISTE_POI_IDX                     = 'No';
  rsSELECT_LISTE_POI_SERIE_POINT             = 'Station';
  rsSELECT_LISTE_POI_LBLTERRAIN              = 'ID terrain';
  rsSELECT_LISTE_POI_COULEUR                 = 'Couleur';
  rsSELECT_LISTE_POI_DESCRIPTION             = 'Description';

  rsSELECT_LISTE_CHECK_MSG_TABLE             = 'Table';
  rsSELECT_LISTE_CHECK_MSG_IDX               = 'ID';
  rsSELECT_LISTE_CHECK_MSG_COULEUR           = ' -- ';
  rsSELECT_LISTE_CHECK_MSG_CRITICITE         = 'Criticité';
  rsSELECT_LISTE_CHECK_MSG_MESSAGE           = 'Message';

  rsSELECT_LISTE_MSG_REGENERATING_LISTE      = 'Régénération de la liste';

  rsSELECT_LISTE_RELEASE_NOTE_IDX            = 'No';
  rsSELECT_LISTE_RELEASE_NOTE_DATE           = 'Date';
  rsSELECT_LISTE_RELEASE_NOTE_AUTHOR         = 'Auteur';
  rsSELECT_LISTE_RELEASE_NOTE_DESCRIPTION    = 'Description';

  rsSELECT_LISTE_NOEUD_ID                    = 'ID';
  rsSELECT_LISTE_NOEUD_STATION               = 'Station';
  rsSELECT_LISTE_NOEUD_COORDS_X              = 'X';
  rsSELECT_LISTE_NOEUD_COORDS_Y              = 'Y';
  rsSELECT_LISTE_NOEUD_COORDS_Z              = 'Z';
  rsSELECT_LISTE_NOEUD_LABEL                 = 'Etiquette';

  rsSELECT_LISTE_PROXIMITE_SERIE             = 'Série';
  rsSELECT_LISTE_PROXIMITE_LONGUEUR_SERIE    = 'Longueur';
  rsSELECT_LISTE_PROXIMITE_BASEPOINT         = 'Dernière station';
  rsSELECT_LISTE_PROXIMITE_BASEPOINT_OBSERV  = 'Observations';
  rsSELECT_LISTE_PROXIMITE_NEAREST           = 'La plus proche';
  rsSELECT_LISTE_PROXIMITE_DISTANCE          = 'Distance';
  rsSELECT_LISTE_PROXIMITE_COMMENTAIRES      = 'Commentaires';

  rsSELECT_LISTE_SER_ENCADREES_NO_SERIE      = 'Numéro';
  rsSELECT_LISTE_SER_ENCADREES_NOM_SERIE     = 'Nom de la série';
  rsSELECT_LISTE_SER_ENCADREES_DEBUT         = 'Début';
  rsSELECT_LISTE_SER_ENCADREES_FIN           = 'Fin';

  // -------------------------------------------------
  // resourcestrings de dlgExportVersSIGExt
  rsEXPORT_SIG_TITRE                         = 'Export vers SIG';
  rsEXPORT_SIG_LEAFLET                       = 'Leaflet OpenStreetMap (.htm)';
  rsEXPORT_SIG_GOOGLE_KML                    = 'Google KML (.kml)';
  rsEXPORT_SIG_GEO_JSON                      = 'GéoJSON (.json)';
  rsEXPORT_SIG_AUTOCAD_DXF                   = 'AutoCAD DXF (.dxf)';

  rsEXPORT_SIG_SYST_COORDONNEES              = 'Système de coordonnées';
  rsEXPORT_SIG_SIG_CIBLE                     = 'Format de sortie';
  rsEXPORT_SIG_LB_FICHIER                    = 'Nom du fichier';
  rsEXPORT_SIG_USE_COLOR_UNIQUE              = 'Utiliser couleurs unique';
  rsEXPORT_SIG_USE_COLOR_ENTRANCES           = 'Par entrées';
  rsEXPORT_SIG_USE_COLOR_RESEAUX             = 'Par réseaux';
  rsEXPORT_SIG_USE_COLOR_SECTEURS            = 'Par secteurs';
  //rsEXPORT_SIG_USE_COLOR_CODES               = 'Par entrées';
  rsEXPORT_SIG_USE_COLOR_EXPES               = 'Par séances';
  rsEXPORT_SIG_CENTERLINE_SILHOUETTES        = 'Eléments exportés';
  rsEXPORT_SIG_CENTERLINE                    = 'Polygonales';
  rsEXPORT_SIG_SILHOUETTE                    = 'Silhouettes';
  rsEXPORT_SIG_ENTRANCES                     = 'Entrances';
  rsEXPORT_SIG_POI                           = 'Points d''intérêt';
  rsEXPORT_SIG_NODES                         = 'Noeuds';
  rsEXPORT_SIG_USE_LOCAL_LEAFLET             = 'Utiliser Leaflet.js en local';

  rsEXPORT_SIG_BTN_GO                        = 'Générer';
  rsEXPORT_SIG_BTN_PREFIX                    = 'Préfixe';
  // -------------------------------------------------
  // resourcestrings de dlgStatistiquesExt
  rsSTATISTIQUES_TITRE                       = 'Statistiques';
  rsSTATISTIQUES_TAB_SYNTHESE                = 'Synthèse';
   rsSTATISTIQUES_SYNTHESE_LB_DEVEL_GALERIE  = 'Développement total des réseaux';
   rsSTATISTIQUES_SYNTHESE_LB_DENIV_TOTAL    = 'Dénivelé total';
   rsSTATISTIQUES_SYNTHESE_LB_DEVELOPPEMENTS = 'Développements';
   rsSTATISTIQUES_SYNTHESE_LB_ETENDUE_RESEAU = 'Etendue du réseau';
   rsSTATISTIQUES_SYNTHESE_LB_NATURELS       = 'Naturels';
   rsSTATISTIQUES_SYNTHESE_LB_SPECIAUX       = 'Spéciaux';
   rsSTATISTIQUES_SYNTHESE_LB_ARTIFICIELS    = 'Artificiels';
   rsSTATISTIQUES_SYNTHESE_LB_Fossiles       = 'Fossiles';
   rsSTATISTIQUES_SYNTHESE_LB_Vadoses        = 'Vadoses';
   rsSTATISTIQUES_SYNTHESE_LB_Inondables     = 'Ennoyables';
   rsSTATISTIQUES_SYNTHESE_LB_Siphons        = 'Siphons';

   rsSTATISTIQUES_SYNTHESE_LB_TopoSurface    = 'Spéciaux';

   rsSTATISTIQUES_SYNTHESE_LB_Tunnels        = 'Tunnels';
   rsSTATISTIQUES_SYNTHESE_LB_Mines          = 'Miniers';

   rsSTATISTIQUES_SYNTHESE_LB_DEVEL_GENERAL  = 'Total général facturable';







  rsSTATISTIQUES_TAB_TABLEAUX                = 'Tableaux';

  rsSTATISTIQUES_TAB_RESEAU_SECTEURS         = 'Réseaux et secteurs';
  rsSTATISTIQUES_TAB_DIAGRAMMES              = 'Diagrammes';
  rsSTATISTIQUES_TAB_COORDONNEES             = 'Coordonnées des stations';
  rsSTATISTIQUES_TAB_ENTRANCES               = 'Coordonnées des entrées';

  rsSTATISTIQUES_LBL_TITRE_ROSE_DIAGRAM      = 'Directions';
  rsSTATISTIQUES_LBL_TITRE_DEPTH_DIAGRAM     = 'Altitudes';
  rsSTATISTIQUES_LBL_LONG_MINI_VISEE         = 'Longueur mini visée';
  rsSTATISTIQUES_LBL_NB_PETALES              = 'Nb de pétales';
  rsSTATISTIQUES_LBL_NB_BARRES               = 'Nb de barres';
  rsSTATISTIQUES_BTN_REDESSINER              = 'Redessiner';
  rsSTATISTIQUES_CMB_MODE_TABLE0             = rsITEM_RESEAUX;
  rsSTATISTIQUES_CMB_MODE_TABLE1             = rsITEM_SECTEURS;
  rsSTATISTIQUES_CMB_MODE_TABLE2             = rsITEM_CODES;
  rsSTATISTIQUES_CMB_MODE_TABLE3             = rsITEM_EXPES;
  rsSTATISTIQUES_CMB_MODE_TABLE4             = 'Dates';
  rsSTATISTIQUES_NB_RESEAUX                  = '%d réseaux';
  rsSTATISTIQUES_NB_SECTEURS                 = '%d secteurs';
  rsSTATISTIQUES_NB_CODES                    = '%d codes';
  rsSTATISTIQUES_NB_SEANCES                  = '%d séances';
  rsSTATISTIQUES_NB_DATES                    = '%d dates';
  rsSTATISTIQUES_TYPE_SHOT_FOSSILES          = 'Fossiles';
  rsSTATISTIQUES_TYPE_SHOT_VADOSES           = 'Vadoses';
  rsSTATISTIQUES_TYPE_SHOT_ENNOYABLES        = 'Inondables';
  rsSTATISTIQUES_TYPE_SHOT_SIPHONS           = 'Siphons';
  rsSTATISTIQUES_TYPE_SHOT_NATURELS          = 'Naturels';
  rsSTATISTIQUES_TYPE_SHOT_SPECIAUX          = 'Spéciaux';
  rsSTATISTIQUES_TYPE_SHOT_TUNNELS           = 'Tunnels';
  rsSTATISTIQUES_TYPE_SHOT_MINES             = 'Filons';
  rsSTATISTIQUES_TYPE_SHOT_TOTAL             = 'Total';
  // resourcestrings relatids au DistoX
  rsDISTOX_DO_RECONNECT                      = 'Reconnecter le DistoX';
  rsDISTOX_UTILS                             = 'Utilitaires DistoX [%s]';
  // resourcestrings de dlgCoupeDeveloppee
  rsCOUPE_DEVEL_AC_OPEN_COUPE                = 'Ouvrir|Ouvrir une coupe développée';
  rsCOUPE_DEVEL_AC_SAVE_COUPE                = 'Enregistrer|Enregistrer la coupe développée';
  rsCOUPE_DEVEL_AC_EXPORT_SVG                = 'SVG|Exporter en SVG';
  rsCOUPE_DEVEL_AC_REVERSE_BRANCHE           = 'Pivoter|Pivoter branche';
  rsCOUPE_DEVEL_AC_ADD_SERIE                 = 'Ajouter|Ajouter une série';
  rsCOUPE_DEVEL_AC_REMOVE_SERIE              = 'Retirer|Retirer la série';
  rsCOUPE_DEVEL_AC_MAKE_COUPE                = 'Calculer|Construire la coupe développée';
  rsCOUPE_DEVEL_AC_DO_CLEAR_ALL              = 'Vider|Supprimer toutes les séries';
  rsCOUPE_DEVEL_AC_REINTERPRETER_SCRIPT      = 'Interpréter|Interpréter le script';
  rsCOUPE_DEVEL_WARN_COUPE_WILL_RESET        = 'Ceci effacera la coupe en cours. Continuer';



  // resourcestrings de CadreListeSeriesFlatMode
  rsCDR_SERIES_FLAT_TAB_GOTO_LINE            = 'Aller sur la ligne';
  rsCDR_SERIES_FLAT_TAB_FIND                 = 'Rechercher';
  rsCDR_SERIES_FLAT_TAB_LOAD_BUFFER          = 'Charger le buffer';
  rsCDR_SERIES_FLAT_TAB_SAVE_BUFFER          = 'Sauver le buffer';
  rsCDR_SERIES_FLAT_TAB_INSERT_LINE          = 'Insérer une ligne';
  rsCDR_SERIES_FLAT_TAB_REMOVE_LINE          = 'Supprimer la ligne';
  rsCDR_SERIES_FLAT_TAB_INSERT_SERIE_HERE    = 'Nouvelle série depuis ce point';
  rsCDR_SERIES_FLAT_TAB_CONTINUE_HERE        = 'Continuer cette série';
  rsCDR_SERIES_FLAT_TAB_PARSE_TABLEAU        = 'Analyser le tableau';
  rsCDR_SERIES_FLAT_TAB_COPY_TABLEAU         = 'Copier le tableau';
  rsCDR_SERIES_FLAT_TAB_SORT_TABLEAU         = 'Trier le tableau dans l''ordre des séries';
  rsCDR_SERIES_FLAT_EDIT_SERIE_BY_DIALOG     = 'Editer série';
  rsCDR_SERIES_FLAT_TAB_UNDOCOPY             = 'Recopier vers le bas';
  rsCDR_SERIES_FLAT_MSG_ROW_UNDELETABLE      = 'La ligne %d contient un en-tête de série et ne peut être supprimée';
  rsCDR_SERIES_FLAT_MSG_BUFFER_SAVED         = 'Buffer %s saved %s';
  rsCDR_SERIES_FLAT_MSG_REMOVE_LINE          = 'Supprimer %d lignes à partir de la ligne %d';
  rsCDR_SERIES_FLAT_MSG_REPLACE_BUFFER       = 'Remplacer les données de la grille';
  rsCDR_SERIES_FLAT_MSG_CONTINUE_THIS_ACTION = 'Effectuer cette action';
  rsCDR_SERIES_FLAT_NEW_SERIE                = 'Nouvelle série';
  rsCDR_SERIES_FLAT_MSG_PARSING              = 'Analyse en cours ...';
  rsCDR_SERIES_FLAT_MSG_PARSING_LINES        = 'Processing %d lines';
  rsCDR_SERIES_FLAT_MSG_DO_CREATE_SERIE_NTH  = 'Créer la série %d';
  rsCDR_SERIES_FLAT_MSG_NB_LINES             = 'Nombre de lignes';
  rsWARN_SERIE_REVERSE                       = 'La série %d arrive sur le réseau par la station %d.%d';
  rsERR_SERIE_START_STATION_NOT_FOUND        = 'Station de départ introuvable';
  rsERR_SERIE_END_STATION_NOT_FOUND          = 'Station d''arrivée introuvable';
  rsWARN_SERIE_CONNECTED_FIXPT               = 'La série %d arrive sur un point fixe ou sur le départ d''une série: %d.%d';
  rsERR_RESEAU_NOT_FOUND                     = 'Réseau introuvable';
  rsERR_ENTRANCE_NOT_FOUND                   = 'Entrée introuvable';
  // resourcestrings de GHTopoRPIMainWnd
  rsGHTOPO_RPI_MAIN_WND_TAB_SERIES           = 'Séries';
  rsGHTOPO_RPI_MAIN_WND_TAB_VUE2D            = 'Plan';
  rsGHTOPO_RPI_MAIN_WND_TAB_LISTES           = 'Listes';
  rsGHTOPO_RPI_MAIN_WND_TAB_DISTOX           = 'DistoX2';
  rsGHTOPO_RPI_MAIN_WND_TAB_ANTENNES         = 'Visées radiantes';




  // dlialogue Recherche de station
  rsDLG_FIND_STATION_TITLE                   = 'Recherche de station topo';
  rsDLG_FIND_STATION_PROMPT                  = 'Libellé du point topo';
  rsDLG_FIND_STATION_DO_MATCH                = 'Correspondance exacte';

  // resourcestring du navigateur séries
  rsCDR_NAVIG_SERIES_TO_FIRST                = 'Première série';
  rsCDR_NAVIG_SERIES_TO_PREV                 = 'Série précédente';
  rsCDR_NAVIG_SERIES_TO_NEXT                 = 'Série suivante';
  rsCDR_NAVIG_SERIES_TO_LAST                 = 'Dernière série';
  rsCDR_NAVIG_SERIES_GOTO_SERIE_NB           = 'Vers la série %d';
  rsCDR_NAVIG_SERIES_IMPLEMENT_MODIFS        = 'Valider les modifications';
  rsCDR_NAVIG_SERIES_GOTO_SERIE_BY_NUMERO    = 'Aller à la série spécifiée par son numéro';
  rsCDR_NAVIG_SERIES_FIND_SERIE_BY_TEXT      = 'Rechercher une série contenant le texte';
  rsCDR_NAVIG_SERIES_TRIER                   = 'Trier les séries en ordre croissant';
  rsCDR_NAVIG_SERIES_FIND_WHAT               = 'Rechercher: ';

  // pour nouveaux éléments
  rsNOUVELLE_ENTREE                          = 'Nouvelle entrée';
  rsNOUVEAU_RESEAU                           = 'Nouveau réseau';
  rsNOUVEAU_SECTEUR                          = 'Nouveau secteur';
  rsNOUVEAU_CODE                             = 'Nouveau code';
  rsNOUVELLE_EXPE                            = 'Nouvelle séance';
  rsNOUVELLE_SERIE                           = 'Nouvelle série';

  // resourcestring du cadre Visualisateur 2D
  rsVUE2D_WINDOW_TITLE                       = 'Vue en plan';
  rsCDR_METAFILTRE_PROMPT                    = 'Filtres';
  rsCDR_METAFILTRE_CHK_ACTIF                 = 'Actif';
  rsCDR_VUE2D_TAB_VUE_INITIALE               = 'Vue initiale';
  rsCDR_VUE2D_TAB_VUE                        = 'Vue %0.2d';
  rsCDR_VUE2D_AC_ZOOM_ALL                    = 'Zoom tout';
  rsCDR_VUE2D_AC_ZOOM_WINDOW                 = 'Zoom fenêtre';
  rsCDR_VUE2D_AC_PAN_VUE                     = 'Pan vue';
  rsCDR_VUE2D_AC_ZOOM_PLUS                   = 'Zoom +';
  rsCDR_VUE2D_AC_ZOOM_MOINS                  = 'Zoom -';
  rsCDR_VUE2D_AC_LOCALISER_STATION           = 'Localiser une station';
  rsCDR_VUE2D_AC_REQUEST_MESURE_DISTOX       = 'Demander une mesure au DistoX';

    // croquis
  rsCDR_VUE2D_AC_SET_MODE_TRAVAIL_NONE       = 'Mode READY';
  rsCDR_VUE2D_AC_NEW_ANNOTATION              = 'Nouvelle annotation';
  rsCDR_VUE2D_AC_NEW_POLYLINE                = 'Nouvelle polyligne';
  rsCDR_VUE2D_AC_DELETE_ANNOTATION           = 'Effacer annotation';
  rsCDR_VUE2D_AC_DELETE_POLYLINE             = 'Effacer polyligne';


  rsCDR_VUE2D_AC_PARAM_ONGLET                = 'Préférences de la vue';
  rsCDR_VUE2D_AC_DISTANCE_BT_STATIONS        = 'Distance entre deux stations';
  rsCDR_VUE2D_MNU_FILTRES_STATION            = 'Filtres selon la station: %d.%d [%s]';
  rsCDR_VUE2D_MNU_HIGHLIGHT_BY_STATION       = 'Surligner selon la station: %d.%d [%s]';

  rsCDR_VUE2D_ADD_SERIE_HERE                 = 'Démarrer ici'; // 'Insérer une nouvelle série ici';
  rsCDR_VUE2D_CONTINUE_SERIE_HERE            = 'Continuer ici';
  rsCDR_VUE2D_DISP_STATIONS_VOISINES         = 'Stations autour de ce point';
  rsCDR_VUE2D_ADDSERIE_BETWEEN_STATION       = 'Insérer une série entre deux stations';
  rsCDR_VUE2D_ISOVALEUR_FROM_Z               = 'Courbe de niveau pour l''altitude indiquée';
  rsCDR_VUE2D_ADD_VISEE_RADIANTE_HERE        = 'Insérer une visée radiante ici';
  rsCDR_VUE2D_LOCALISER_STATION              = 'Localiser une station';
  rsCDR_VUE2D_ADD_ANNOTATION_HERE            = 'Insérer une annotation ici';
  rsCDR_VUE2D_EDIT_THIS_ANNOTATION           = 'Editer cette annotation';
  rsCDR_VUE2D_DELETE_THIS_ANNOTATION         = 'Supprimer cette annotation';
  rsCDR_VUE2D_CREATE_TIMELAPSE               = 'Créer un timelapse';
  rsCDR_VUE2D_CLOSE_BOUCLE                   = 'Boucler sur un autre cheminement';
  rsCDR_VUE2D_EXTRACT_PROFIL_FROM_MNT        = 'Extraire un profil topographique de surface';
  rsCDR_VUE2D_AC_LOAD_MNT                    = 'Charger un MNT (Modèle Numérique de Terrain)';

  rsCDR_VUE2D_AC_DISP_MINIFORM_VISEE         = 'Afficher/Masquer saisie rapide de visées';
  rsCDR_VUE2D_SET_ACTIVE_STATION             = 'Définir comme station courante';
  rsCDR_VUE2D_DISP_POPUP                     = 'Afficher le menu de la vue en plan';
  rsCDR_VUE2D_AC_CROQUIS_OPEN                = 'Ouvrir un croquis';
  rsCDR_VUE2D_AC_CROQUIS_SAVE                = 'Enregistrer le croquis';
  rsCDR_VUE2D_AC_DISP_LAST_CLICKED           = 'Afficher les derniers points cliqués sur le plan';

  rsCDR_VUE2D_AC_CROQUIS_NEW_ANNOTATION      = 'Nouvelle annotation';
  rsCDR_VUE2D_AC_CROQUIS_NEW_POLYLIGNE       = 'Nouvelle polyligne';
  rsCDR_VUE2D_AC_CROQUIS_DELETE_ANNOTATION   = 'Effacer annotation';
  rsCDR_VUE2D_AC_CROQUIS_DELETE_POLYLIGNE    = 'Effacer  polyligne';

  // titres du sous-menu
  rsCDR_VUE2D_AC_METAFILTRE_NAMESPACE        = 'Namespace %d: %s';
  rsCDR_VUE2D_AC_METAFILTRE_SERIE            = 'Série %d';
  rsCDR_VUE2D_AC_METAFILTRE_TOPO_DU_JOUR     = 'Topo du %s';
  rsCDR_VUE2D_AC_METAFILTRE_ENTRANCE         = 'Entrée %d';
  rsCDR_VUE2D_AC_METAFILTRE_RESEAU           = 'Réseau %d';
  rsCDR_VUE2D_AC_METAFILTRE_SECTEUR          = 'Secteur %d';
  rsCDR_VUE2D_AC_METAFILTRE_CODE             = 'Code %d';
  rsCDR_VUE2D_AC_METAFILTRE_EXPE             = 'Séance %d';
  rsCDR_VUE2D_AC_METAFILTRE_YEAR             = 'Année %d';
  rsCDR_VUE2D_AC_METAFILTRE_CURRENT_VIEW     = 'Filtrer sur la vue actuelle';
  rsCDR_VUE2D_AC_ISOVALEUR_Z                 = 'Courbe de niveau pour l''altitude %.0f m';
  rsCDR_VUE2D_AC_EDIT_THIS_SERIE             = 'Editer la série %d';
  rsCDR_VUE2D_AC_DELETE_THIS_POI             = 'Supprimer le point d''intérêt';
  rsCDR_VUE2D_AC_ADD_POI_HERE                = 'Ajouter un POI ici';


  // message Recalculer le réseau
  rsMSG_QUESTION_RECALCULER                  = 'Recalculer le réseau';

  // fenêtre Visualisateur 2D
  rsVUE2D_GRBX_ELEMENTS_DRAWN                = 'Eléments dessinés';
    rsVUE2D_CHK_DRW_STATIONS                 = 'Stations';
    rsVUE2D_CHK_DRW_ENTRANCES                = rsITEM_ENTRANCES;
    rsVUE2D_CHK_DRW_WALLS                    = 'Parois';
    rsVUE2D_CHK_DRW_CENTERLINES              = 'Polygonales';
    rsVUE2D_CHK_DRW_ALTITUDES                = 'Altitudes';
    rsVUE2D_CHK_DRW_COTES                    = 'Cotes';
    rsVUE2D_CHK_DRW_ANTENNES                 = 'Visées radiantes';
    rsVUE2D_CHK_DRW_IDStations               = 'Numéro';
    rsVUE2D_CHK_DRW_SECTIONS                 = 'Sections';
    rsVUE2D_CHK_DRW_FILL                     = 'Remplissage';
    rsVUE2D_CHK_DRW_QUADRILLAGE              = 'Quadrillage';
  rsVUE2D_GRBX_DEGRADE_ALTITUDES             = 'Dégradé d''altitudes';
  rsVUE2D_PRMS_DRW_BTN_APPLY                 = 'Appliquer';
  // cadre / fiche Profils
  rsTITRE_PROFILS                            = 'Profils verticaux (transects)';
    rsCDR_PROFIL_NB_POINTS                   = '%d points';
    rsCDR_PROFIL_NB_CONDUITS                 = '%d conduits recoupés';
    rsCDR_PROFIL_COL_NO_PT                   = 'No Point';
    rsCDR_PROFIL_COL_ABSCISSE                = 'Abscisse';
    rsCDR_PROFIL_COL_COTE                    = 'Altitude';
    rsCDR_PROFIL_COL_X                       = 'X';
    rsCDR_PROFIL_COL_Y                       = 'Y';
    rsBTN_LOAD_PROFILS                       = 'Ouvrir des profils';
    rsBTN_SAVE_PROFILS                       = 'Sauvegarder les profils';
    rsBTN_REMOVE_PROFIL                      = 'Supprimer le profil courant';
    rsBTN_APPLY_MODIFS_PROFIL                = 'Appliquer au profil courant';
    rsBTN_COPY_LIST_GAL_RECOUPEES            = 'Copier la liste';

  // pour la fonction DescribeTableExamineeByCode()
  rsCRITICITE_ERROR_INFORMATION              = 'Information';
  rsCRITICITE_ERROR_WARNING                  = 'Avertissement';
  rsCRITICITE_ERROR_ERROR                    = 'Erreur potentielle';
  rsCRITICITE_ERROR_CRITICAL                 = 'Erreur fatale';
  rsCRITICITE_ERROR_FIXED                    = 'Erreur fixée';
  // pour les fonctions de check
  rsCHECKED_TABLE_ENTRANCES                  = rsITEM_ENTRANCES;
  rsCHECKED_TABLE_RESEAUX                    = rsITEM_RESEAUX;
  rsCHECKED_TABLE_SECTEURS                   = rsITEM_SECTEURS;
  rsCHECKED_TABLE_CODES                      = rsITEM_CODES;
  rsCHECKED_TABLE_EXPES                      = rsITEM_EXPES;
  rsCHECKED_TABLE_SERIES                     = rsITEM_SERIES;


  rsCHK_MSG_DOUBLONS_SERIES                  = 'Doublons dans les séries: ([%d] %d - %s) et ([%d] %d - %s)';
  rsCHK_MSG_AUTOLOOP                         = 'Auto-loop : station %d.%d linked to %d.%d';
  rsCHK_MSG_SERIE_CNX_ENTREE                 = 'Série %d raccordée à l''entrée: %d.%d (%s)';
  rsCHK_MSG_SERIE_ORPHELINE                  = 'Série %d orpheline ou connectée par son terminus: %d.%d -> %d.%d';
  rsCHK_MSG_SERIE_WITHOUT_SHOTS              = 'Série %d sans visées';
  rsCHK_MSG_VISEE_TRES_COURTE                = 'Serie %d: (%d stations): Developpement faible %.2f < %.2f m';
  rsCHK_MSG_SERIE_DEV_NUL                    = 'Serie %d: (%d stations): Developpement nul';
  rsCHK_MSG_SERIE_NB_STATIONS_WARNING        = 'Nombre de stations insuffisant: %d';
  rsCHK_MSG_SERIE_PB_STATION_ARRIVEE         = 'Serie %d: Station %d.%d devrait être %d.%d';

  rsCHK_MSG_SERIE_NB_TOTAL_STATIONS          = '%d stations au total';

  // resourcestrings du visualisateur 3D
  rsVUE3D_BTN_EXPORT_GCP                     = 'Export GHCaveDraw';
  rsMSG_EXPORT_GCP_PHI_MUST_BE_ZERO          = 'L''angle d''élévation Phi doit être nul pour utiliser cette fonction';
  //
  rsBTN_HIGHLIGHT_SELECTED                   = 'Surligner sélectionnés';
  // resourcestrings de la boite de recherche dans la base
  rsDLG_SEARCH_IN_DATABASES_TITLE            = 'Recherche dans les bases de données';
  rsDLG_SEARCH_IN_DATABASES_LB_RESULTS       = 'Résultats de la recherche: %d éléments trouvés';
  rsDLG_SEARCH_IN_DATABASES_EDIT_FINDWHAT    = 'Recherche';
  rsDLG_SEARCH_IN_DATABASES_BTN_FINDWHAT     = 'Rechercher';

  rsMSG_GRAPHE_POINT_NOT_FOUND               = 'Le point de %s %s est introuvable';
  rsMSG_WARN_LONG_PROCESS                    = 'Cette opération est assez longue - Continuer';
//================================================================================
implementation

end.
