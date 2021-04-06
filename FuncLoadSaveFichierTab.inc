//FuncLoadFichierTab.inc
// Chargement et sauvegarde des fichiers TAB
function TToporobotStructure2012.LoadFromXTB(const FichierTAB: TStringDirectoryFilename): integer;
var
  EPSGDefault : TLabelSystemesCoordsEPSG;
  MyFiltre    : TFiltrePersonnalise;
  NoLigneTAB, i: integer;
  yyy, mmm, ddd: Word;
  // variables de lignes
  LigneTab: String;
  PrmsLn, QArrStrg: TGHStringArray;
  ErrMsg, QStr: String;
  // liste provisoire pour lecture de la section -6
  ProvListeEntrees: TStringList;
  // série
  UneSerie: TObjSerie;
  NbErrorsInLoading: Integer;
  blaireau, QN, QFichierCorrect: Integer;
  MyNameSpace: TNameSpace;

  procedure WriteWarning(const Msg: string);
  begin
    AfficherMessageErreur(Msg);
  end;
  // Vérifie s'il y a une instruction INCLUDE
  // (qui doit être impérativement en début de ligne)
  // et retourne Vrai si OK en passant le nom de fichier
  // Le fichier à inclure doit être dans le même dossier que le fichier principal
  function HasInstructionInclude(const QMyLigne: string; out QFichierInclude: TStringDirectoryFilename): boolean;
  const
    DIRECTIVE_INCLUDE = '$INCLUDE:';
  var
    P, Q: SizeInt;
    EWE: String;
    QMyDir: TStringDirectoryFilename;
  begin
    Result := False;
    if (1 = Pos(DIRECTIVE_INCLUDE, QMyLigne)) then
    begin
      AfficherMessageErreur('HasInstructionInclude: ' + QMyLigne);
      // extraction du chemin
      QMyDir := ExtractFilePath(FichierTAB);
      // extraction du nom du fichier
      P := Pos(':', QMyLigne);
      Q := Length(QMyLigne) - P + 1;
      EWE := Trim(Copy(QMyLigne, P+1, Q));
      // assemblage du chemin et du nom de fichier spécifié
      QFichierInclude := QMyDir + EWE;
      AfficherMessageErreur(QFichierInclude);
      // et on vérifie si le fichier existe
      Result := FileExistsUTF8(QFichierInclude);
    end;
  end;

  // Procédure récursive de lecture
  // Retourne 0 si aucune erreur
  //         -1 si échec de lecture du fichier ppal
  //         -2 si fichier INCLUDE introuvable
  //         -3 si échec de lecture du fichier INCLUDE
  function LireUnFichier(const QFilename: string): integer;
  var
    pTAB                : TextFile;
    QFileIsToporobotTabStrict: boolean;
    Ex: Integer;
    Prefix1, Prefix2    : Integer;
    QMonFichierAInclure: TStringDirectoryFilename;
    qVisee : TUneVisee;
    // items de tables simples
    UneEntree: TEntrance;
    UnReseau : TReseau;
    UnSecteur: TSecteur;
    UneExpe  : TExpe;
    UnCode   : TCode;
    UneViseeAntenne: TViseeAntenne;
    MyNameSpace: TNameSpace;
    // il y a des séries ?
    //HasSeries: boolean;
    function LireLigne: string;
    var
      Lign: string;
    begin
      ReadLn(pTab, Lign);
      Inc(NoLigneTAB);
      Result:= Lign;// PurgerAccents(Lign);
    end;
  begin
    Result := -1;
    QFileIsToporobotTabStrict := Pos('tab', LowerCase(ExtractFileExt(FichierTAB))) > 0;
    AssignFile(pTAB, QFilename);
    try
      ReSet(pTAB);
      while Not Eof(pTAB) do
      begin
        try // traitement local d'exceptions dans la lecture des lignes
          // réinitialisation du message d'erreur par défaut
          ErrMsg  := rsRD_TAB_MSG_ERR;
          Prefix1 := 0;
          LigneTab := LireLigne;
          if (HasInstructionInclude(LigneTab, QMonFichierAInclure)) then
          begin
            Result := LireUnFichier(QMonFichierAInclure);
          end;
          if (Trim(LigneTab)='')    then Prefix1 := -100  // lignes vides
          else if (LigneTab[1]='#') then Prefix1 := -1000 // commentaires sur une ligne
          else if LigneTab[1]='{' then    // commentaires sur plusieurs lignes
          begin
            AfficherMessage(Format(rsRD_TAB_D_MULTI_OBS,[NoLigneTAB]));
            while Not (LigneTab[1]='}') do LigneTab := LireLigne; /// PurgerAccents(LireLigne);
            AfficherMessage(Format(rsRD_TAB_F_MULTI_OBS,[NoLigneTAB]));
          end
          else begin
            PrmsLn   := split(LigneTab, SEPARATOR_TAB);
            Prefix1  := StrToIntDef(PrmsLn[0], -110);
          end;
          // routage selon le préfixe
          case Prefix1 of
            -9999: Break; // Arrêt forcé du traitement (utiliser avec précaution)
            -1000: AfficherMessage(Format(rsRD_TAB_LN_OBS,[NoLigneTab]));   // commentaire
             -900: ; // balise de pause
             -110: ; // Ligne invalide; ignorée
             -100: ; // ignorer les lignes vides
              -20: AfficherMessage(Format(rsRD_TAB_LASTSAVES,[PrmsLn[1], PrmsLn[2]])); // horodatage
              -19: begin
                     self.SetNomEtude(Trim(PrmsLn[1])); // Nouvelle section: Nom de l'étude
                     self.SetCommentairesEtude(Trim(PrmsLn[2]));
                   end;

              -18: begin // Nouvelle section: Espace de noms (pour les synthèses)
                     //00  01 02       03       04     05 06
                     //-18	0	255 	0 	0 	Pont_de_Gerbaut	Gouffre du Pont de Gerbaut
                     MyNameSpace.Couleur := RGBToColor(StrToIntDef(Trim(PrmsLn[2]), 255),
                                                       StrToIntDef(Trim(PrmsLn[3]), 0),
                                                       StrToIntDef(Trim(PrmsLn[4]), 0));
                     MyNameSpace.Nom          := Trim(PrmsLn[5]);
                     MyNameSpace.Description  := Trim(PrmsLn[6]);
                     AfficherMessageErreur('Namespace trouvé:' + MyNameSpace.Nom);
                     self.AddNameSpace(MyNameSpace);
                   end;
              -16: begin // Nouvelle section: Filtres personnalisés

                //#### #-16	IDFiltre	NomFiltre	CouleurFiltre_R	CouleurFiltre_G	CouleurFiltre_B	FiltreExpr	Description
                     //-16	0	Aquatopo	0	128	0	SERIE=227
                     MyFiltre.CouleurFiltre :=  RGBToColor(StrToIntDef(PrmsLn[2], 0),
                                                           StrToIntDef(PrmsLn[3], 0),
                                                           StrToIntDef(PrmsLn[4], 0));
                     MyFiltre.NomFiltre     := Trim(PrmsLn[5]);
                     MyFiltre.Expression    := Trim(PrmsLn[6]);
                     MyFiltre.Description   := Trim(PrmsLn[7]);
                     AddFiltrePerso(MyFiltre);  //*)

                   end;
              -15: begin // Nouvelle section: Système de coordonnées :
                     EPSGDefault.CodeEPSG := StrToIntDef(Trim(PrmsLn[1]), DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);
                     EPSGDefault.NomEPSG  := '';
                     SetCodeEPSGSystemeCoordonnees(EPSGDefault);
                   end;
              -10: begin // Nouvelle section: secteurs
                     UnSecteur.CouleurSecteur := RGBToColor(StrToIntDef(PrmsLn[2], 0),
                                                 StrToIntDef(PrmsLn[3], 0),
                                                 StrToIntDef(PrmsLn[4], 0));
                     UnSecteur.NomSecteur     := Trim(PrmsLn[5]);
                     AddSecteur(UnSecteur);
                   end;
               -9: begin  // visées en antenne
                     UneViseeAntenne.Reseau           := StrToIntDef(PrmsLn[2], 0);
                     UneViseeAntenne.Secteur          := StrToIntDef(PrmsLn[3], 0);
                     UneViseeAntenne.SerieDepart      := StrToIntDef(PrmsLn[4], 0);
                     UneViseeAntenne.PtDepart         := StrToIntDef(PrmsLn[5], 0);
                     UneViseeAntenne.Longueur         := ConvertirEnNombreReel(PrmsLn[9], 0.00);
                     UneViseeAntenne.Azimut           := ConvertirEnNombreReel(PrmsLn[10], 0.00);
                     UneViseeAntenne.Pente            := ConvertirEnNombreReel(PrmsLn[11], 0.00);
                     UneViseeAntenne.MarkedForDelete  := False;
                     AddViseeAntenne(UneViseeAntenne);
                   end;
               -8: begin  // réseaux
                     UnReseau.ColorReseau := RGBToColor(StrToIntDef(PrmsLn[2], 0),
                                                        StrToIntDef(PrmsLn[3], 0),
                                                        StrToIntDef(PrmsLn[4], 0));
                     UnReseau.TypeReseau  := StrToIntDef(PrmsLn[5], 0);
                     UnReseau.NomReseau   := Trim(PrmsLn[6]);
                     UnReseau.ObsReseau   := Trim(PrmsLn[7]);
                     AddReseau(UnReseau);
                   end;
               -7: ; // Classeurs (désactivé)
               -6: begin  // Section -6: Entrée
                     ProvListeEntrees.Add(PrmsLn[2] + '|' + PrmsLn[3] + '|' + PrmsLn[10] );
                   end;
               -5: begin  // Entrées
                     //nombre d'entrées nul = on définit l'entrée par défaut
                     if (0 = GetNbEntrances()) then
                     begin
                       SetDefaultCoords(ConvertirEnNombreReel(PrmsLn[2], 0.00),
                                        ConvertirEnNombreReel(PrmsLn[3], 0.00),
                                        ConvertirEnNombreReel(PrmsLn[4], 0.00));
                       SetRefSeriePoint(StrToIntDef(PrmsLn[5],1), StrtoIntDef(PrmsLn[6], 0));
                     end;
                     //Commentaires:=PrmsLn[7];
                     // Ajouter les entrées
                     // le nombre d'entrées retenu est celui décompté dans
                     // la section -5
                     //eNumEntree:= GetNbEntrees + 1;
                     // On ajoute l'entrée récupérée en -6
                     try
                       QN := GetNbEntrances();
                       QStr := ProvListeEntrees.Strings[QN];
                       QArrStrg   := Split(QStr, '|');
                       UneEntree.eNomEntree := Trim(QArrStrg[0]);
                       UneEntree.eIDTerrain := Trim(QArrStrg[1]);
                       UneEntree.eCouleur   := GHTopoColorToColorDef(Trim(QArrStrg[2]), clGray);
                       ///eCouleur   := Col
                     except
                       AfficherMessage(Format(rsWARNINGENTRYADDED,[QN]));
                       UneEntree.eNomEntree := Format(rsRD_TAB_ENTRANCE,[QN]);
                       UneEntree.eCouleur   := clRed;
                     end;
                     UneEntree.eXEntree  := ConvertirEnNombreReel(PrmsLn[2], 0.00);
                     UneEntree.eYEntree  := ConvertirEnNombreReel(PrmsLn[3], 0.00);
                     UneEntree.eZEntree  := ConvertirEnNombreReel(PrmsLn[4], 0.00);


                     //------------
                     // entrées non géoréférencées ?
                     if (UneEntree.eXEntree < 100.00) or (UneEntree.eYEntree < 100.00) or (UneEntree.eZEntree < 500.00)
                     then WriteWarning(Format(GetResourceString(rsRD_TAB_ENTR_NOGEOREF), [NoLigneTAB, QN, UneEntree.eNomEntree]));
                     UneEntree.eRefSer   := StrToIntDef(PrmsLn[5],1);
                     UneEntree.eRefSt    := StrtoInt(PrmsLn[6]);
                     if ((UneEntree.eRefSer < 1) or (UneEntree.eRefSt < 0)) then
                        WriteWarning(Format(rsRD_TAB_ENTR_BADLINK, [NoLigneTAB, QN, UneEntree.eNomEntree, UneEntree.eRefSer, UneEntree.eRefSt]));
                     UneEntree.eObserv   := PrmsLn[7];
                     AddEntrance(UneEntree);
                   end;
               -4: pass;
               -3: pass;
               -2: begin // Expés
                     UneExpe.IDExpe := StrToInt(PrmsLn[1]);
                     if (UneExpe.IDExpe <= 0) then
                     begin
                        ErrMsg:=rsRD_TAB_BAD_TRIP;
                        raise Exception.Create(ErrMsg);
                     end;
                     {$WARNING: TEXpe.DateExpe à implementer - }
                     UneExpe.JourExpe      := StrToIntDef(PrmsLn[2],1);
                     UneExpe.MoisExpe      := StrToIntDef(PrmsLn[3],1);
                     UneExpe.AnneeExpe     := StrToIntDef(PrmsLn[4],2000);
                     //UneExpe.DateExpe      := GetSecuredDate(StrToIntDef(PrmsLn[4],2000),
                     //                                        StrToIntDef(PrmsLn[3],1),
                     //                                        StrToIntDef(PrmsLn[2],1));

                     if (UneExpe.JourExpe=0) or (UneExpe.MoisExpe=0) or (UneExpe.AnneeExpe=0) then
                     begin
                       WriteWarning(Format(rsRD_TAB_BAD_DATE,
                                          [NoLigneTAB]));
                       DecodeDate(Now(), yyy, mmm, ddd);
                       UneExpe.JourExpe := ddd;
                       UneExpe.MoisExpe := mmm;
                       UneExpe.AnneeExpe:= yyy;
                     end;
                     {$WARNING: End TEXpe.DateExpe à implementer}
                     UneExpe.Operateur     := PrmsLn[5];             // spéléomètre
                     UneExpe.ClubSpeleo    := PrmsLn[6];             // spéléographe
                     {$WARNING A revoir et vérifier. Si c'est un TAB pur, calcul de la déclimag en automatique}
                     if (QFileIsToporobotTabStrict) then
                     begin
                       UneExpe.ModeDecl              :=  cdmAUTOMATIQUE;   // déclinaison auto ?
                       UneExpe.DeclinaisonInDegrees  := -ConvertirEnNombreReel(PrmsLn[8], 0.00); // déclinaison Toporobot = -déclinaison VisualTopo/GHTopo
                     end else
                     begin
                       UneExpe.ModeDecl      := TModeCalculDeclimag(StrToIntDef(PrmsLn[7], 1));   // déclinaison auto ?
                       UneExpe.DeclinaisonInDegrees  := ConvertirEnNombreReel(PrmsLn[8], 0.00); // déclinaison
                     end;
                     //Inclinaison   := ConvertirEnNombreReel(PrmsLn[9], 0.00); // correction clino x10
                     UneExpe.IdxCouleur    := StrToInt(PrmsLn[10]);  // couleur
                     UneExpe.Commentaire   := PrmsLn[11];            // commentaire
                     AddExpe(UneExpe);
                   end;
               -1: begin // Codes
                     UnCode.IDCode     := StrToInt(PrmsLn[1]);    // ID Code
                     if (UnCode.IDCode <= 0) then
                     begin
                        //WriteWarning(Format('WARNING ! (%d) - Numéro de Code incorrect (Valeur: %d) - Mis à %d',[NoLigneTAB, IDCode, FNbCodes]));
                        ErrMsg:=rsRD_TAB_BAD_CODE;
                        raise Exception.Create(ErrMsg);
                     end;
                     UnCode.GradAz     := ConvertirEnNombreReel(PrmsLn[2], UNITE_ANGULAIRE_DU_CODE_ZERO);  // unité boussole
                     UnCode.GradInc    := ConvertirEnNombreReel(PrmsLn[3], UNITE_ANGULAIRE_DU_CODE_ZERO);  // unite  CLINO
                     UnCode.PsiL       := ConvertirEnNombreReel(PrmsLn[4], 0.01);  // precision longueur
                     UnCode.PsiAz      := ConvertirEnNombreReel(PrmsLn[5], 0.1);  // precision azimut
                     UnCode.PsiP       := ConvertirEnNombreReel(PrmsLn[6], 0.1);  // precision pente
                     {$WARNING: Support de FactLong a valider}
                     UnCode.FactLong   := 1.00;
                     //UnCode.FactLong   := ConvertirEnNombreReel(PrmsLn[7], 1.00)/100;  // Facteur des longueurs
                     UnCode.AngLimite  := ConvertirEnNombreReel(PrmsLn[8], 0.00);  // angle limite
                     UnCode.Commentaire := PrmsLn[9];              // commentaire
                     //ReservedInt := StrToIntDef(PrmsLn[10],0);
                     UnCode.ParamsFuncCorrAz  := MakeTParamFoncCorrectionAngulaire(ConvertirEnNombreReel(PrmsLn[11], 0.00),
                                                                                   ConvertirEnNombreReel(PrmsLn[12], 0.00),
                                                                                   ConvertirEnNombreReel(PrmsLn[13], 0.00));
                     UnCode.ParamsFuncCorrInc := MakeTParamFoncCorrectionAngulaire(ConvertirEnNombreReel(PrmsLn[14], 0.00),
                                                                                   ConvertirEnNombreReel(PrmsLn[15], 0.00),
                                                                                   ConvertirEnNombreReel(PrmsLn[16], 0.00));
                     UnCode.ErreurTourillon   := ConvertirEnNombreReel(PrmsLn[17], 0.00);
                     // provisoire: Diamètre des boules-cibles
                     UnCode.DiametreBoule1 := 0.00;
                     UnCode.DiametreBoule2 := 0.00;
                     AddCode(UnCode);
                   end;
               0: ; // pas de section 0
             // end
           otherwise  // on est dans les séries !
             // Si le préfixe 2 (2e colonne) =-1 =>nouvelle série
             Prefix1 := StrToInt(PrmsLn[0]);
             if (Prefix1 > 0) then
             begin
               Prefix2 := StrToInt(PrmsLn[1]);
               if (Prefix2 = -1) then
               begin
                 if (Prefix1 = 1) then    // si c'est la première série, on crée
                 begin
                   UneSerie := TObjSerie.Create;
                   UneSerie.ClearStations();
                 end
                 else  // sinon on ferme la série courante et on crée la suivante
                 begin
                   self.AddSerie(UneSerie);
                   UneSerie := TObjSerie.Create;
                 end;
                 UneSerie.SetNumeroSerie(TNumeroSerie(Prefix1));
                 UneSerie.SetSeriePtExtremites(StrToInt(PrmsLn[2]),
                                               StrToInt(PrmsLn[3]),
                                               StrToInt(PrmsLn[4]),
                                               StrToInt(PrmsLn[5]));
                 // NbPoints: Non utilisé
                 UneSerie.SetChanceObstacle(StrToInt(PrmsLn[7]), StrToInt(PrmsLn[8]));
                 UneSerie.SetNomSerie(PrmsLn[9]);
                 UneSerie.SetObsSerie(PrmsLn[10]);
                 UneSerie.SetNumeroReseau(StrToIntDef(PrmsLn[11],0));
                 UneSerie.SetRaideur(ConvertirEnNombreReel(Prmsln[12], 1.02));        // raideur de la série
                 UneSerie.SetNumeroEntrance(StrToIntDef(Prmsln[13], 0));               // entrée de rattachement
               end
               else
               begin
                 qVisee.TypeVisee   := tgDEFAULT;
                 qVisee.Code        := StrToInt(PrmsLn[2]);
                 qVisee.Expe        := StrToInt(PrmsLn[3]);
                 qVisee.Longueur    := ConvertirEnNombreReel(PrmsLn[4], 0.00);
                 if (qVisee.Longueur < 0.00) then
                 begin
                   WriteWarning(Format(rsRD_TAB_NEG_LONG, [NoLigneTAB, qVisee.Longueur]));
                   qVisee.Longueur := Abs(qVisee.Longueur);
                 end;
                 qVisee.Azimut      := ConvertirEnNombreReel(PrmsLn[5], 0.00);
                 qVisee.Pente       := ConvertirEnNombreReel(PrmsLn[6], 0.00);
                 qVisee.LG          := ConvertirEnNombreReel(PrmsLn[7], 0.00);  //LG
                 qVisee.LD          := ConvertirEnNombreReel(PrmsLn[8], 0.00);  //LD
                 qVisee.HZ          := ConvertirEnNombreReel(PrmsLn[9], 0.00);
                 qVisee.HN          := ConvertirEnNombreReel(PrmsLn[10], 0.00);
                 qVisee.Commentaires:= PrmsLn[11];
                 qVisee.IDTerrainStation := FormatterIDTerrainStation(PrmsLn[12]);
                 blaireau    := StrToIntDef(PrmsLn[13], 0);
                 qVisee.TypeVisee   := GetTypeDeVisee(blaireau); // galerie fossile supposée
                 qVisee.IDSecteur   := StrToIntDef(PrmsLn[14], 0);
                 UneSerie.AddVisee(qVisee);
               end;
             end   // if Prefix2
           end; // case Prefix1

           Result := 0;
        except
          Inc(NbErrorsInLoading);
          WriteWarning('');
          WriteWarning(Format(rsRD_ERROR_LN,[NoLigneTab, ErrMsg]));
          WriteWarning(Format(rsRD_CONTNT_LN,[LigneTab]));
          WriteWarning(rsRD_TAB_FIELD_VALUES);
          for Ex := 0 to High(PrmsLn) do
          begin
            WriteWarning(Format(' PrmsLn[%.2d] = "%s"',[Ex, PrmsLn[Ex]]));
            WriteWarning('');
          end;
        end;
      end; // while Not Eof(pTAB) do begin
    finally
      CloseFile(pTAB);
    end;
  end;
begin
  Result := -1;
  AfficherMemoryUsage();
  AfficherMessage(Format('%s.LoadFichierTAB(%s)',[ClassName, FichierTAB]));

  // exemple d'affichage à l'exécution du numero de la ligne (utile pour déboguer)
  //AfficherMessage(  {$INCLUDE %FILE%} );
  //AfficherMessage(  {$INCLUDE %LINE%} );
  // nom étude par défaut
  SetNomEtude('Etude001');
  SetCommentairesEtude('');

  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;
  SetCodeEPSGSystemeCoordonnees(EPSGDefault);
  ReInitialiser(True);
  NoLigneTAB := 0;
  NbErrorsInLoading := 0;

  ProvListeEntrees := TStringList.Create;
  ProvListeEntrees.Clear;
  // Lecture du fichier
  QFichierCorrect := LireUnFichier(FichierTAB);
  // On ferme la dernière série
  self.AddSerie(UneSerie);
  // Espaces de noms: s'il y en a aucun, on en crée un par défaut
  if (0 = GetNbNameSpaces()) then
  begin
    AfficherMessage('Aucun espace de noms trouvé - Création d''un namespace par défaut');
    AddNameSpace(NAMESPACE_NAME_BY_DEFAULT, NAMESPACE_COLOR_BY_DEFAULT, NAMESPACE_DESC_BY_DEFAULT);
  end;
  AfficherMessageErreur(Format('%d namespaces', [GetNbNameSpaces()]));
  for i := 0 to GetNbNameSpaces() - 1 do
  begin
    MyNameSpace := GetNameSpace(i);
    AfficherMessageErreur(Format('%d: %s', [i, MyNameSpace.Nom]));
  end;
  //**************************
  Preconditionner(FichierTAB);
  //****************************
  Result := self.GetNbSeries();

  // suppression d la liste provisoire des entrées
  try
    ProvListeEntrees.Clear;
  finally
    FreeAndNil(ProvListeEntrees);
  end;
  AfficherMemoryUsage();
end;
//******************************************************************************
procedure TToporobotStructure2012.SaveToXTB(const FichierTAB: TStringDirectoryFilename; const ModeSaveTAB: TModeSaveTAB; const TextFileFormat: TTextFileFormat);
const

  LINE_STATION = FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                 FORMAT_STRING;

  FMT_LINE_NAMESPACE   = FORMAT_NB_INTEGER  + TAB + FORMAT_NB_INTEGER + TAB +
                         '%d ' + TAB + '%d ' + TAB + '%d ' + TAB + // couleur
                         FORMAT_STRING  + TAB + FORMAT_STRING;     // noms
var
  ENDL     : string;
  Sr, St, i: integer;
  QNbFiltresPerso   : integer;
  QNbNamespaces, QNbEntrees, QNbReseaux, QNbSecteurs  : integer;
  QNbCodes, QNbExpes: integer;
  QNbAntennes       : integer;

  pTAB: TextFile;
  Entrance: TEntrance;
  Serie: TObjSerie;
  Station: TUneVisee;
  s : string;
  R : TReseau;
  SC: TSecteur;
  VA: TViseeAntenne;
  EPSG: TLabelSystemesCoordsEPSG;
  MyFiltrePerso: TFiltrePersonnalise;
  MyNamespace: TNameSpace;
  procedure WrtLn(const Str: string); inline;
  begin
    Write(pTAB, Str+ENDL);
  end;
  procedure WrtCommentaire(const Str: string); inline;
  begin
    if (ModeSaveTAB = mtabEXTENDEDTAB) then Write(pTAB, '#### '+Str+ENDL);
  end;
begin
  case TextFileFormat of  // mise en place des fins de lignes
    tfWINDOWS: begin s:='Windows';   ENDL := #13+#10; end;
    tfMAC    : begin s:='Macintosh'; ENDL := #13    ; end;
    tfUNIX   : begin s:='Unix';      ENDL := #10    ; end;
  end;
  AfficherMessage(Format('TToporobotStructure.SaveFile(%s) as %s format',[FichierTAB, s]));
  try
    // Sauvegarder ancien fichier

    AssignFile(pTAB, FichierTAB);
    ReWrite(pTAB);
    AfficherMessage('   Saving header');
    // Sections ajoutées au format TOPOROBOT
    // Fichiers XTB uniquement
    if (ModeSaveTAB = mtabEXTENDEDTAB) then
    begin
      // Section -20: Date de dernières modifications, version du fichier et du logiciel
      AfficherMessage('--> Horodating saves at #20 section');

      WrtLn(Format(FORMAT_NB_INTEGER + TAB +
                   FORMAT_STRING + TAB +
                   FORMAT_STRING + TAB +
                   FORMAT_STRING,
                   [-20,
                    DateToStr(Now), TimeToStr(Now),
                    self.FDataBaseName
                   ]));
      {$IFDEF GROS_MINET}
      WrtLn('# Sylvestre CLEMENT');
      {$ELSE}
      WrtLn('# (fr) L''auteur de GHTopo est christianophobe et hait le Christ');
      WrtLn('# (en) The author of GHTopo hates Christ');
      WrtLn('# (de) Der Autor von GHTopo hasst Christus');
      WrtLn('# (nl) De Autor van GHTopo haat Christus');
      WrtLn('# (es) El autor de GHTopo odia a Cristo');
      WrtLn('# (it) L''autore di GHTopo odia Cristo');
      WrtLn('# (pt) O autor de GHTopo odeia Cristo');
      WrtLn('# (va) Movit omnes auctor GHTopo odit Christum');
      {$ENDIF}
      //WrtLn('# ');

      // Sections -19 : Libellé et commentaires de l'étude (XTB uniquement)
      AfficherMessage('--> Writing general infos at #-19 to #-18 sections');
      WrtLn(Format(FORMAT_NB_INTEGER + TAB +
                    FORMAT_STRING + TAB + FORMAT_STRING,
                   [-19, self.FNomEtude, self.FCommentaireEtude]));
      // Section -18: Espaces de noms (XTB uniquement);
      AfficherMessage('--> Saving namespace at #-18 section');
      WrtLn('');
      QNbNamespaces := self.GetNbNameSpaces();
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -18, ['IDNamespace', 'CouleurNamespace_R', 'CouleurNamespace_G', 'CouleurNamespace_B', 'Namespace', 'Description']));

      WrtCommentaire(Format('%d namespaces', [QNbNamespaces]));
      WrtLn('');
      if (QNbNamespaces > 0) then
      begin
        for i := 0 to QNbNamespaces - 1 do
        begin
          MyNamespace := self.GetNameSpace(i);
          WrtLn(Format(FMT_LINE_NAMESPACE, [-18, i,
                                            Red(MyNamespace.Couleur), Green(MyNamespace.Couleur), Blue(MyNamespace.Couleur),
                                            MyNamespace.Nom, MyNamespace.Description]));
        end;
      end
      else WrtLn(Format(FMT_LINE_NAMESPACE, [-18, 0,
                                             Red(NAMESPACE_COLOR_BY_DEFAULT), Green(NAMESPACE_COLOR_BY_DEFAULT), Blue(NAMESPACE_COLOR_BY_DEFAULT),
                                             NAMESPACE_NAME_BY_DEFAULT, NAMESPACE_DESC_BY_DEFAULT]));
      WrtLn('');
      //************************************************************************
      // Section -16: Filtres personnalisés (XTB uniquement);
      AfficherMessage('--> Saving filters at #-16 section');
      WrtLn('');
      QNbFiltresPerso := GetNbFiltresPersos();
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -16, ['IDFiltre', 'NomFiltre', 'CouleurFiltre_R', 'CouleurFiltre_G', 'CouleurFiltre_B', 'FiltreExpr', 'Description']));
      WrtCommentaire(Format('%d filtres perso', [QNbFiltresPerso]));

      if (QNbFiltresPerso > 0) then
      begin
        for i := 0 to QNbFiltresPerso - 1 do
        begin
          MyFiltrePerso := self.GetFiltrePerso(i);
          WrtLn(Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                       FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                       FORMAT_STRING + TAB +
                       FORMAT_STRING + TAB + FORMAT_STRING,
                       [-16, i,
                        Red(MyFiltrePerso.CouleurFiltre), Green(MyFiltrePerso.CouleurFiltre), Blue(MyFiltrePerso.CouleurFiltre),
                        MyFiltrePerso.NomFiltre,
                        MyFiltrePerso.Expression, MyFiltrePerso.Description
                        ]));
        end;
      end;
      // Section -15: Système de coordonnées géographiques (XTB uniquement)
      // TODO: Désactivé pour l'instant - A réimplémenter
      AfficherMessage('--> Writing coordinates system at #-15 section');
      WrtLn('');
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -15, ['Code EPSG', 'Coordinates system description']));
      //WrtCommentaire('#-15'+ TAB + 'Code EPSG' + TAB + 'Coordinates system description');
      // -15	31563	LT3	Lambert 3 France
      WrtLn('');
      EPSG := GetCodeEPSGSystemeCoordonnees;
      WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER + TAB + FORMAT_STRING,  [-15, EPSG.CodeEPSG, EPSG.NomEPSG]));
      //*************************************
      // Section -10: Secteurs (Format XTB uniquement)
      AfficherMessage('--> Saving #-10 Sectors section');
      WrtLn('');
      WrtCommentaire('Sectors list');
      WrtLn('');
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -10, ['IdxSecteur', 'Color R', 'Color G', 'Color B', 'NomSecteur']));
      WrtLn('');
      QNbSecteurs := GetNbSecteurs();
      if (QNbSecteurs > 1) then
      begin
        for i:=1 to QNbSecteurs - 1 do
        begin
          SC := GetSecteur(i);
          WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_STRING,
                       [-10,
                        i,
                        Red(SC.CouleurSecteur),
                        Green(SC.CouleurSecteur),
                        Blue(SC.CouleurSecteur),
                        SC.NomSecteur
                        ]));
        end;
      end;
      // Section -9: Visées en antennes (Format XTB uniquement)
      QNbAntennes := self.GetNbAntennes();
      if (QNbAntennes > 0) then
      begin
        AfficherMessage('--> Saving #-9 Antenna shots section');
        WrtLn('');
        WrtCommentaire('Antenna-shots list');
        WrtLn('');
        WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -9, ['ID', 'ID Reseau', 'ID Secteur', 'Serie', 'Point', 'Code (unused)', 'Expe (unused)', 'IDTerrain', 'Longueur', 'Azimut', 'Pente', 'Observ.']));
        WrtLn('');
        for i := 1 to QNbAntennes - 1 do
        begin
          VA := GetViseeAntenne(i);
          if (VA.Longueur > 0.001) then
          begin
            //-9	1	2	1102	10	101	101	GHLMF	12.56	289.00	-14.00	le chat dans la souricière
            WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+
                              TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + // série, point
                              TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + // code, expé
                              TAB + FORMAT_STRING +          // ID terrain
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_NB_REAL_2_DEC+
                              TAB + FORMAT_STRING ,
                     [-9, i, VA.Reseau, VA.Secteur,
                             VA.SerieDepart, VA.PtDepart,
                             0, 0, //VA.Code, VA.Expe,           // Visées en antenne: Code et expés hérité de leur station d'accrochage
                             '', //VA.IDTerrainStation,
                             VA.Longueur, VA.Azimut, VA.Pente,
                             ''  //VA.Commentaires
                     ]));
          end;
        end;
      end;
      // Section -8: Réseaux (Format XTB uniquement)
      AfficherMessage('--> Saving #-8 Networks section');
      WrtLn('');
      WrtCommentaire('Networks list');
      WrtLn('');
      WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -9, ['IdxReseau', 'Color R', 'Color G', 'Color B', 'TypeReseau', 'NomReseau', 'ObsReseau']));
      WrtLn('');
      QNbReseaux := GetNbReseaux();
      if (QNbReseaux > 1) then
      begin
        for i:=1 to QNbReseaux - 1 do
        begin
          R:=GetReseau(i);
          WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                       FORMAT_NB_INTEGER +TAB+
                       FORMAT_STRING + TAB +
                       FORMAT_STRING,
                       [-8,
                        i,
                        Red(R.ColorReseau),
                        Green(R.ColorReseau),
                        Blue(R.ColorReseau),
                        R.TypeReseau,
                        R.NomReseau,
                        R.ObsReseau
                        ]));
        end;
      end;
    end; //  if (ModeSaveTAB = mtabEXTENDEDTAB) then begin
    // Sauvegarde des entrées
    AfficherMessage('--> Saving #-6 Entrances section');
    WrtLn('');
    WrtCommentaire('Entrances list');
    WrtLn('');
    WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -6, ['ID Entrance', 'NomEntrance', 'Serie', 'Station', 'X Entrance', 'Y Entrance', 'Z Entrance', 'Observ']));
    WrtLn('');
    QNbEntrees := GetNbEntrances();
    for i := 0 to QNbEntrees - 1 do
    begin
      Entrance := GetEntrance(i);
      AfficherMessage(Format('%d/%d: %d.%d:  %s', [i, QNbEntrees, Entrance.eRefSer, Entrance.eRefSt, Entrance.eNomEntree]));
      WrtLn(Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_STRING + TAB + FORMAT_STRING + TAB +
                   FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB +
                   FORMAT_STRING + TAB + FORMAT_STRING,
                  [-6, i+1,
                   Entrance.eNomEntree,
                   Entrance.eIDTerrain,
                   Entrance.eRefSer,  Entrance.eRefSt,
                   Entrance.eXEntree, Entrance.eYEntree, Entrance.eZEntree,
                   Entrance.eObserv,
                   ColorToGHTopoColor(Entrance.eCouleur)
                   ]));
    end;

    AfficherMessage('--> Saving #-5 Deprecated Entrances section');
    WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -5, ['X Entrance', 'Y Entrance', 'Z Entrance', 'Serie', 'Station', 'Observ', '*** Deprecated section. Data moved to #6 section ***']));
    WrtLn('');
    for i:=0 to QNbEntrees - 1 do
    begin
      Entrance := GetEntrance(i);
      WrtLn(Format(FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB +
                   FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB + FORMAT_NB_REAL_3_DEC + TAB +
                   FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_STRING,
                          [-5, i+1,
                          Entrance.eXEntree,
                          Entrance.eYEntree,
                          Entrance.eZEntree,
                          Entrance.eRefSer,
                          Entrance.eRefSt,
                          Entrance.eObserv
                          ]));
    end;
    WrtLn('');
    WrtCommentaire('Trips list');
    WrtLn('');
    AfficherMessage('--> Saving Expes');
    WrtCommentaire(MakeHeaderRubriquesOfAnRow(TAB, -2, ['IDExpe', 'Day', 'Month', 'Year', 'Team', 'Surveyor', 'Modedecl', 'Declination', 'dummy', 'IdxColor', 'Observ.']));
    WrtLn('');
    QNbExpes := GetNbExpes();
    if (QNbExpes > 0) then
    begin
      for  i:= 1 to QNbExpes - 1 do
      begin
        WrtLn(MakeToporobotTabLineOfExpe(-2, GetExpe(i)));
      end;
    end;
    // Codes
    //-1	999	400.00	400.00	0.00	0.00	0.00	100.00	-100.00	Fixpunkt

    AfficherMessage('--> Saving Codes');
    WrtLn('');
    WrtCommentaire('Instruments codes list');
    WrtLn('');
    QNbCodes := GetNbCodes();
    if (QNbCodes > 1) then
    begin
      for i:=1 to QNbCodes - 1 do
      begin
        WrtLn(MakeToporobotTabLineOfCode(-1, ModeSaveTAB, GetCode(i)));
      end;
      WrtLn('');
    end;
    // Séries
    case ModeSaveTAB of
      mtabEXTENDEDTAB: AfficherMessage('--> Saving Series at mtabEXTENDEDTAB mode');
      mtabTOPOROBOT  : AfficherMessage('--> Saving Series at mtabTOPOROBOT mode');
    end;
    WrtLn('');
    WrtCommentaire('Series and stations');
    WrtLn('');
    for Sr:=1 to GetNbSeries() - 1 do
    begin
      WrtLn('');
      Serie := GetSerie(Sr);
      //AfficherMessage(Format('---> %d : %s (%d pts)',[Serie.GetNumeroDeSerie(), Serie.GetNomSerie, Serie.GetNbVisees]));
      with Serie do
      begin
        case ModeSaveTAB of
          mtabEXTENDEDTAB:
            WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_STRING+TAB+FORMAT_STRING+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_REAL_3_DEC+TAB+
                                 FORMAT_NB_INTEGER,
                                 [GetNumeroDeSerie(), -1,
                                  GetNoSerieDep(), GetNoPointDep(),
                                  GetNoSerieArr(), GetNoPointArr(),
                                  GetNbVisees() - 1,
                                  GetChance(), GetObstacle(),
                                  GetNomSerie(),
                                  GetObsSerie(),
                                  GetNumeroReseau(),
                                  GetRaideur(),
                                  GetNumeroEntrance()
                                 ]));
          mtabTOPOROBOT:
            WrtLn(Format(FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+
                                 FORMAT_NB_INTEGER+TAB+FORMAT_NB_INTEGER+TAB+
                                 FORMAT_STRING,
                                 [GetNumeroDeSerie(), -1,
                                  GetNoSerieDep, GetNoPointDep,
                                  GetNoSerieArr, GetNoPointArr,
                                  GetNbVisees - 1,
                                  GetChance, GetObstacle,
                                  SafeTruncateString(GetNomSerie, 20)
                                 ]));
        end;
        for St := 0 to Serie.GetNbVisees - 1 do
        begin
          Station:=Serie.GetVisee(St);
          with Station do begin
            case ModeSaveTAB of
              mtabEXTENDEDTAB:
              begin
                WrtLn(Format(LINE_STATION + TAB + FORMAT_STRING + TAB + FORMAT_NB_INTEGER + TAB + FORMAT_NB_INTEGER,
                                [Serie.GetNumeroDeSerie(), St,
                                 Code, Expe,
                                 Longueur, Azimut, Pente,
                                 LG, LD, HZ, HN,
                                 Commentaires,
                                 IDTerrainStation,
                                 TypeVisee,
                                 IDSecteur
                                ]));
              end;
              mtabTOPOROBOT:
              begin
                WrtLn(Format(LINE_STATION,
                                [Serie.GetNumeroDeSerie(), St,
                                 Code, Expe,
                                 Longueur, Azimut, Pente,
                                 LG, LD, HZ, HN,
                                 Commentaires
                                ]));
              end;
            end;
          end;
        end;
      end;
    end;
    AfficherMessage('TToporobotStructure.SaveToFile OK');
  finally
    CloseFile(pTAB);
  end;
end;

