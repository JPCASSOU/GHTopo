// FuncImportFichierText.inc
function TToporobotStructure2012.LoadFichierText(const FichierText: TStringDirectoryFilename): integer; deprecated;
const
  // colonnes des champs
  //COL_COMMENT     = 00; // parenthèse de commentaire éventuel
  COL_NUM_SECTION = 01; // numéro de section ou numéro de série
  COL_NUM_ITEM    = 02; // numéro d'item (expé, code, point topo)

  //COL_SESSION     = 03; // numéro de session (inutilisé)
  COL_CODE        = 04; // code attaché à la station
  COL_EXPE        = 05; // numéro de séance

  COL_VAR_DATA    = 06; // colonne suivant les colonnes de préfixes et index
  COL_NOMENTREE   = COL_VAR_DATA; // nom de l'entrée
  COL_XENTREE     = COL_VAR_DATA;
  COL_YENTREE     = COL_XENTREE + 1;
  COL_ZENTREE     = COL_YENTREE + 1;
  COL_ENT_SER     = COL_ZENTREE + 1;
  COL_ENT_ST      = COL_ENT_SER + 1;

  COL_DATE_EXPE   = COL_VAR_DATA;
  COL_SPELEOMETRE = COL_DATE_EXPE + 1;
  COL_SPELEOGRAPH = COL_SPELEOMETRE + 1;
  COL_MODEDECL    = COL_SPELEOGRAPH + 1;
  COL_DECL_VALUE  = COL_MODEDECL + 1;
  COL_INCL_VALUE  = COL_DECL_VALUE + 1;
  COL_COULEUR     = COL_INCL_VALUE + 1;

  COL_U_AZIMUT    = COL_VAR_DATA;
  COL_U_PENTE     = COL_U_AZIMUT + 1;
  COL_PSI_L       = COL_U_PENTE + 1;
  COL_PSI_AZ      = COL_PSI_L +1;
  COL_PSI_P       = COL_PSI_AZ + 1;
  //COL_OLD_DECL    = COL_PSI_P  + 1;
  COL_FACT_LONG   = COL_PSI_P + 1;
  COL_ANG_LIMITE  = COL_FACT_LONG + 1;

  COL_SER_DEP     = COL_VAR_DATA;
  COL_PT_DEP      = COL_SER_DEP + 1;
  COL_SER_ARR     = COL_PT_DEP + 1;
  COL_PT_ARR      = COL_SER_ARR + 1;
  COL_NB_PTS      = COL_PT_ARR + 1;
  COL_CHANCE      = COL_NB_PTS + 1;
  COL_OBSTACLE    = COL_CHANCE + 1;

  COL_LONG        = COL_VAR_DATA;
  COL_AZ          = COL_LONG + 1;
  COL_P           = COL_AZ + 1;
  COL_LG          = COL_P + 1;
  COL_LD          = COL_LG + 1;
  COL_HZ          = COL_LD + 1;
  COL_HN          = COL_HZ + 1;
  TEMPORARYTABFILE = '~GhtopoTemp.txt';
var
  EPSGDefault : TLabelSystemesCoordsEPSG;
  // fichier d'erreur
  pTEXT        : TextFile;
  // liste provisoire pour entrées
  ProvListeEntrees: TStringList;
  // séries, réseaux, etc
  UneEntree : TEntrance;
  UneExpe : TExpe;
  UnCode  : TCode;
  UneSerie: TObjSerie;
  qVisee  : TUneVisee;
  // ligne de données
  NoLigneTEXT,
  NbErrorsInLoading: integer;
  LigneTEXT: string;
  PrmsLn   : TGHStringArray;
  // la ligne contient une parenthèse de tête ?
  HasParenthesis: boolean;
  // messages d'erreur
  ErrMsg  : string;
  // Préfixes de section
  Prefix1,
  Prefix2 : integer;

  // divers
  str         : string;
  llanfair_pg : string;
  // dates
  yyy, mmm, ddd: word;
  // Index courant
  //IdxCourant: integer;
  IdxCourantStation: integer;
  NomDeLaSerie: String;
  WU: String;
  mioumiou: Boolean;
  IdxCourant, QN: Integer;

  myViseeAntenne: TViseeAntenne;
  // Conversion de lignes Mac > PC
  procedure ConvertTextMacToPC(var Lin: string);
  begin
    // convertir accentués
    Lin := StringReplace(Lin, #136, 'a', [rfReplaceAll]);
    Lin := StringReplace(Lin, #142, 'e', [rfReplaceAll]);
    Lin := StringReplace(Lin, #143, 'e', [rfReplaceAll]);
    Lin := StringReplace(Lin, #148, 'i', [rfReplaceAll]);
    Lin := StringReplace(Lin, #137, 'a', [rfReplaceAll]);
    Lin := StringReplace(Lin, #144, 'e', [rfReplaceAll]);
    Lin := StringReplace(Lin, #144, 'e', [rfReplaceAll]);
    Lin := StringReplace(Lin, #141, 'c', [rfReplaceAll]);
  end;
  // extraire une visée en antenne
  function ExtraireViseeEnAntenne(const qStr: string;
                                  const qIdxReseau: integer;
                                  const qIdxSerie: integer;
                                  const qIdxPoint: integer;
                                  const qMyVisee: TUneVisee;
                                  var qVA : TViseeAntenne): boolean;
  var
    myStr: String;
    CrO, CrF: integer;
    EWE: TGHStringArray;
  begin
    Result := False;
    myStr := Trim(qStr);
    try
      // Visées en antenne sont de la forme [%.2f %.2f %.2f]
      // crochets indispensables
      // recherche de crochets et suppression
      CrO := Pos('[', myStr);
      if (CrO = 0) then Exit;
      if (CrO > 0) then System.Delete(myStr, CrO, 1);
      CrF := Pos(']', myStr);
      if (CrF > 0) then System.Delete(myStr, CrF, 1);
      EWE := Split(myStr, ' ');
      qVA.SerieDepart      := qIdxSerie;
      qVA.PtDepart         := qIdxPoint;
      //qVA.IDTerrainStation := qMyVisee.IDTerrainStation;
      //qVA.Code             := qMyVisee.Code;
      //qVA.Expe             := qMyVisee.Expe;
      qVA.Reseau           := qIdxReseau;
      qVA.Secteur          := qMyVisee.IDSecteur;
      //qVA.Commentaires     := '';
      qVA.Longueur         := ConvertirEnNombreReel(Trim(EWE[0]), -1.00);
      qVA.Azimut           := ConvertirEnNombreReel(Trim(EWE[1]), 0.00);
      qVA.Pente            := ConvertirEnNombreReel(Trim(EWE[2]), 0.00);
      qVA.MarkedForDelete  := false;
      //AfficherMessage(Format('%f, %f, %f',[qVA.Longueur, qVA.Azimut, qVA.Pente]));
      Result := (qVA.Longueur > 0.00);
    except
    end;
  end;
  // afficher avertissement
  procedure WriteWarning(const wm: string);
  begin
    AfficherMessage(wm);
  end;
begin
  //self.ClearListeSeries();
  EPSGDefault.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
  EPSGDefault.NomEPSG  := DEFAULT_SYSTEME_COORDONNEES_NOM;

  WU := Format('%s.LoadFichierText: %s', [ClassName, FichierText]);
  AfficherMessage(WU);
  AfficherMessageErreur(WU);
  Result := -1;
  // Fichier introuvable ? OK, On sort
  if not FileExistsUTF8(FichierText) then  begin Result:=-32; Exit; end;
  // nom étude par défaut
  SetNomEtude('Etude001');
  SetCommentairesEtude('Issued from Toporobot Text format (fixed columns)');
  SetCodeEPSGSystemeCoordonnees(EPSGDefault);


  try // try..finally niveau 0
     //*************************************************************************
     // Pour pallier à tout problème de formats de fichiers textes,
     // et notamments ceux liés aux fins de ligne,
     // on fait une conversion.
     // Pour économiser une variable, on utilise une variable utilisée + tard
     LigneTEXT := AnsiToUtf8(ExtractFilePath(FichierText)+ TEMPORARYTABFILE);
     ConvertTextFile(FichierText, LigneTEXT, tfMAC, tfWINDOWS);
     AfficherMessage(GetResourceString(rsSEPARATOR_LINE));
     // nom de la base
     SetDatabaseName(ExtractFileNameWithoutExt(FichierText)); // Laz 1.8: ExtractFileNameWithoutExt() remplace ExtractFileNameOnly()
     // Initialisation des listes
     ReInitialiser(True);
     AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
     // code EPSG par défaut
     SetCodeEPSGSystemeCoordonnees(EPSGDefault);

     // début du véritable traitement
     // liste provisoire pour lecture des sections -6
     ProvListeEntrees:=TStringList.Create;
     // ouverture fichier Text
     AssignFile(pTEXT, LigneTEXT);
     AfficherMessage('--> Lecture du fichier');
     AfficherMessage(Format('--> %d expés, %d codes', [GetNbExpes(), GetNbCodes()]));

     try // niveau 1: pour le fichier Text
       Reset(pTEXT);
       // lire tout le fichier
       NoLigneTEXT:=0;
       NbErrorsInLoading:=0;
       ErrMsg:= rsRD_TAB_MSG_ERR;
       IdxCourant := -1;
       while Not Eof(pTEXT) do
       begin
         try // traitement local d'exceptions dans la lecture des lignes
           Prefix1:=0;
           Inc(NoLigneTEXT);
           ReadLn(pTEXT, LigneTEXT);
           //AfficherMessage('--> ' + LigneTEXT);
           //AfficherMessage(Format('Ligne %d - %s', [NoLigneTEXT, LigneTEXT]));
           //AfficherMessage(LigneTEXT);
           //ConvertTextMacToPC(LigneTEXT);
           if Trim(LigneTEXT)=''    then Prefix1:=-100       // lignes vides
           else if LigneTEXT[1]='#' then Prefix1:=-1000     // commentaires sur une ligne
           else
           begin
             // Commentaire = parenthèse de tête présente
             HasParenthesis := (LigneTEXT[1] = '(');
             //if (HasParenthesis) then Continue;
             // attraper le préfixe de section (en évitant une décomposition)
             str := Trim(Copy(LigneTEXT, 2, 5));
             Prefix1   := StrToIntDef(str, -110);

             if (HasParenthesis) then  // On zappe les commentaires (difficultés d'implémentation)
             begin
               //Continue;
               // classique décomposition ...
               PrmsLn      := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80,100]);
               llanfair_pg := Trim(PrmsLn[COL_VAR_DATA]);
                case Prefix1 of
                 -5: begin // entrées

                       UneEntree := GetEntrance(IdxCourant);
                       // on concatène les lignes
                       UneEntree.eObserv := UneEntree.eObserv + llanfair_pg;

                       PutEntrance(IdxCourant, UneEntree);
                     end;
                 -4: ;     // sessions (obsolète)
                 -3: ;     // section non affectée
                 -2: begin // expés
                       IdxCourant := GetNbExpes() - 1;
                       UneExpe    := GetExpe(IdxCourant);
                       // on concatène les lignes

                       UneExpe.Commentaire := UneExpe.Commentaire + llanfair_pg;
                       PutExpe(IdxCourant, UneExpe);
                     end;
                 -1: begin // code
                       IdxCourant := GetNbCodes() - 1;
                       UnCode     := GetCode(IdxCourant);
                       // on concatène les lignes
                       UnCode.Commentaire := UnCode.Commentaire + llanfair_pg;
                       PutCode(IdxCourant, UnCode);
                     end;
               else  // séries
                 // Zapper cette section
                 begin
                   //Continue;
                   if (Prefix1 > 0) then
                   begin
                     Prefix2:=StrToInt(PrmsLn[COL_NUM_ITEM]);
                     if (Prefix2 = -1) then
                     begin // tête de série
                       IdxCourant := GetNbSeries - 1;
                       UneSerie := GetSerie(IdxCourant);
                       UneSerie.SetObsSerie(UneSerie.GetObsSerie + llanfair_pg);
                     end else
                     begin // sinon, station
                       //Continue;
                       IdxCourant := GetNbSeries - 1;
                       IdxCourantStation := UneSerie.GetNbVisees - 1;
                       qVisee := UneSerie.GetVisee(IdxCourantStation);
                       if (ExtraireViseeEnAntenne(llanfair_pg,
                                                  UneSerie.GetNumeroReseau(),
                                                  UneSerie.GetNumeroDeSerie(),
                                                  IdxCourantStation,
                                                  qVisee,
                                                  myViseeAntenne)) then
                       begin
                         AddViseeAntenne(myViseeAntenne);
                         llanfair_pg := '';
                       end;
                       qVisee.Commentaires := qVisee.Commentaires + llanfair_pg;
                       UneSerie.PutVisee(IdxCourantStation, qVisee);
                     end;
                   end;
                 end;
               end;
               // on zappe tout le reste ...
               Continue;
             end;  // if HasParenthesis
             case Prefix1 of

                -9999: begin // Arrêt forcé du traitement
                             // utiliser cette balise avec de grandes précautions
                             // et de préférence à la fin d'une série
                         WriteWarning('');
                         WriteWarning(Format(rsRD_TAB_STOP_9999, [NoLigneTEXT]));
                         WriteWarning('');
                         Break;
                       end;
                -1000: AfficherMessage(Format(GetResourceString(rsRD_TAB_LN_OBS),[NoLigneTEXT]));  // commentaire
                 -900: Continue; // balise de pause
                 -110: Continue; // Ligne invalide; ignorée
                 -100: Continue; // ignorer les lignes vides
                   -7: Continue; //AfficherMessage(GetResourceString(rsRD_TAB_CLASSEURS)); // Section -7 : Classeurs -- Ignoré par GHTopo
                   -6: begin  // Section -6: Entrée
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80, 100]);
                         ProvListeEntrees.Add(PrmsLn[COL_NOMENTREE]);
                         Continue;
                       end;
                   -5: begin  // Entrées
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 37, 49, 61, 67, 73, 80]);
                         //nombre d'entrées nul = on définit l'entrée par défaut
                         // TODO: Revoir cette zone
                         if (0 = self.GetNbEntrances()) then
                         begin
                           SetDefaultCoords(ConvertirEnNombreReel(PrmsLn[COL_XENTREE], 0.00),
                                            ConvertirEnNombreReel(PrmsLn[COL_YENTREE], 0.00),
                                            ConvertirEnNombreReel(PrmsLn[COL_ZENTREE], 0.00)
                                           );

                           SetRefSeriePoint(StrToIntDef(PrmsLn[COL_ENT_SER],1),
                                            StrtoIntDef(PrmsLn[COL_ENT_ST], 0)
                                           );

                         end;
                         // Ajouter les entrées
                         // le nombre d'entrées retenu est celui décompté dans
                         // la section -5
                         with UneEntree do
                         begin
                           QN := GetNbEntrances();
                           eIDTerrain := '';  // non supporté par TOPOROBOT
                           // On ajoute l'entrée récupérée en -6
                           try
                             eNomEntree:=ProvListeEntrees.Strings[QN];
                           except
                             AfficherMessage(Format(rsWARNINGENTRYADDED,[QN]));
                             eNomEntree:= Format(rsRD_TAB_ENTRANCE,[QN]);
                           end;
                           ePosition.setFrom(ConvertirEnNombreReel(PrmsLn[COL_XENTREE], 0.00),
                                             ConvertirEnNombreReel(PrmsLn[COL_YENTREE], 0.00),
                                             ConvertirEnNombreReel(PrmsLn[COL_ZENTREE], 0.00));
                           // couleurs non supportées = couleur par défault
                           eCouleur  := clGray;
                           //------------
                           // entrées non géoréférencées ?
                           if (ePosition.X < 100.00) or
                              (ePosition.Y < 100.00) or
                              (ePosition.Z < 100.00)
                           then WriteWarning(Format(GetResourceString(rsRD_TAB_ENTR_NOGEOREF),
                                                   [NoLigneTEXT,QN, eNomEntree]));
                           eRefSer   := StrToIntDef(PrmsLn[COL_ENT_SER],1);
                           eRefSt    := StrtoInt(PrmsLn[COL_ENT_ST]);
                           if ((eRefSer < 1) or (eRefSt < 0)) then
                                  WriteWarning(Format(rsRD_TAB_ENTR_BADLINK,
                                       [NoLigneTEXT, QN, eNomEntree, eRefSer, eRefSt]));
                           eObserv   := '';
                         end; // with UneEntree
                         // n'ajouter l'entrée que si elle comporte une RefSerie non nulle
                         mioumiou := (UneEntree.eRefSer > 0);
                         if (mioumiou) then self.AddEntrance(UneEntree);
                         // index courant
                         IdxCourant := GetNbEntrances() - 1;
                         Continue;
                       end;
                   -4: Continue;  // sessions; ignoré
                   -3: Continue;  // réservé; ignoré
                   -2: begin // Expés
                         //Ajoute l'expé dans la liste
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 36, 50, 64, 65, 73, 77, 81,100]);
                         with UneExpe do
                         begin
                           IDExpe        := StrToInt(PrmsLn[COL_NUM_ITEM]);
                           if (IDExpe<=0) then begin
                              ErrMsg:=rsRD_TAB_BAD_TRIP;
                              raise Exception.Create(ErrMsg);
                           end;
                           // décomposition de la date
                           {$WARNING: TEXpe.DateExpe à implementer}
                           str := Trim(PrmsLn[COL_DATE_EXPE]);
                           JourExpe      := StrToIntDef(Copy(STR, 0, 2), 0);
                           MoisExpe      := StrToIntDef(Copy(STR, 4, 2), 1);
                           AnneeExpe     := StrToIntDef(Copy(STR, 7, 2), 1);
                           if (JourExpe = 0) or (MoisExpe = 0) or (AnneeExpe = 0) then
                           begin
                             WriteWarning(Format(rsRD_TAB_BAD_DATE,
                                                [NoLigneTEXT]));
                             DecodeDate(Now, yyy, mmm, ddd);
                             JourExpe := ddd;
                             MoisExpe := mmm;
                             AnneeExpe:= yyy;
                           end;
                           {$WARNING: End TEXpe.DateExpe à implementer}
                           Operateur     := PrmsLn[COL_SPELEOMETRE];             // spéléomètre
                           ClubSpeleo    := PrmsLn[COL_SPELEOGRAPH];             // spéléographe
                           ModeDecl      := TModeCalculDeclimag(StrToIntDef(PrmsLn[COL_MODEDECL], 1));   // déclinaison auto ?
                           DeclinaisonInDegrees  := ConvertirEnNombreReel(PrmsLn[COL_DECL_VALUE], 0.00); // déclinaison
                           //Inclinaison   := ConvertirEnNombreReel(PrmsLn[COL_INCL_VALUE], 0.00); // correction clino x10
                           IdxCouleur    := StrToIntDef(PrmsLn[COL_COULEUR], 0);  // couleur
                           Commentaire   := ''; //PrmsLn[11];            // commentaire

                         end;  // with UneExpe
                         AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
                         AddExpe(UneExpe);
                         Continue;
                       end;
                   -1: begin // Codes
                         //                                                           U_AZ
                         //                                                            |   U_P
                         //                                                            |   |   PSI_L
                         //                                                            |   |   |   PSI_A
                         //                                                            |   |   |   |   PSI_P
                         //                                                            |   |   |   |   |   FACT
                         //                                                            |   |   |   |   |   |   ANG_LIM
                         //                                                            |   |   |   |   |   |   |
                         PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);
                         with UnCode do
                         begin
                           IDCode     := StrToInt(PrmsLn[COL_NUM_ITEM]);    // ID Code
                           if (IDCode<=0) then
                           begin
                             ErrMsg:=rsRD_TAB_BAD_CODE;
                             raise Exception.Create(ErrMsg);
                           end;
                           GradAz     := ConvertirEnNombreReel(PrmsLn[COL_U_AZIMUT], DEGRES_PAR_TOUR);  // unité boussole
                           GradInc    := ConvertirEnNombreReel(PrmsLn[COL_U_PENTE] , DEGRES_PAR_TOUR);  // unite  CLINO
                           PsiL       := ConvertirEnNombreReel(PrmsLn[COL_PSI_L], 0.01);  // precision longueur
                           PsiAz      := ConvertirEnNombreReel(PrmsLn[COL_PSI_AZ], 0.5);  // precision azimut
                           PsiP       := ConvertirEnNombreReel(PrmsLn[COL_PSI_P], 0.5);  // precision pente
                           // TODO: Support de FactLong a valider
                           FactLong   := 1.00; // ConvertirEnNombreReel(PrmsLn[COL_FACT_LONG], 1.00)/100.00;  // Facteur des longueurs
                           AngLimite  := ConvertirEnNombreReel(PrmsLn[COL_ANG_LIMITE], 0.00);  // angle limite
                           ErreurTourillon  := 0.00;
                           DiametreBoule1   := 0.00;
                           DiametreBoule2   := 0.00;
                           ParamsFuncCorrAz.Empty();// non supporté par LimeLight
                           ParamsFuncCorrInc.Empty();
                           Commentaire:= ''; //PrmsLn[9];              // commentaire
                         end; // with UnCode
                         AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
                         AddCode(UnCode);
                         Continue;
                       end;
                    0: begin
                         Continue;  // pas de section 0
                       end
             else // on est dans les séries
               begin
                 PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 80, 100]);
                 // Si le préfixe 2 (2e colonne) =-1 =>nouvelle série
                 Prefix1 := StrToInt(PrmsLn[COL_NUM_SECTION]);
                 if (Prefix1 > 0) then
                 begin
                   Prefix2 := StrToInt(PrmsLn[COL_NUM_ITEM]);
                   // C'est le header de série ?
                   // les headers de série comportent deux lignes !
                   if (Prefix2 = -2) then
                   begin
                     // première ligne
                     NomDeLaSerie := Trim(PrmsLn[COL_VAR_DATA]);
                     //AfficherMessage(Format('Une serie a ajouter: Prefix1: %d - Prefix2 = %d - %s',[Prefix1, Prefix2, NomDeLaSerie]));
                     // Lecture de la 2e ligne
                     Inc(NoLigneTEXT);
                     ReadLn(pTEXT, LigneTEXT);

                     // décomposition
                     //*******************************************************************************
                     //                                                           SD
                     //                                                            |   PD
                     //                                                            |   |   SA
                     //                                                            |   |   |   PA
                     //                                                            |   |   |   |   NB
                     //                                      Serie                 |   |   |   |   |   CH
                     //                                                            |   |   |   |   |   |   OBS
                     //                                                            |   |   |   |   |   |   |
                     PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);
                     //AfficherMessage(Format('%d %d - %s', [Prefix1, Prefix2,  UneSerie.GetNomSerie]));
                     // si c'est la première série, on crée
                     //AfficherMessage(LigneTEXT);
                     //AfficherMessage(PrmsLn[1]);
                     Prefix1 := StrToInt(PrmsLn[1]);
                     if (Prefix1 = 1) then
                     begin
                       UneSerie := TObjSerie.Create;
                       UneSerie.SetNumeroSerie(1);
                       //AfficherMessage(Format('Prefix = %d - Serie 1 creee',[Prefix1]));
                     end
                     else  // sinon on ferme la série courante et on crée la suivante
                     begin
                       self.AddSerie(UneSerie);
                       UneSerie := TObjSerie.Create;
                     end;
                     with UneSerie do
                     begin
                       SetNumeroSerie(TNumeroSerie(Prefix1));
                       SetNomSerie(NomDeLaSerie);
                       SetSeriePtExtremites(StrToInt(PrmsLn[COL_SER_DEP]),
                                            StrToInt(PrmsLn[COL_PT_DEP]),
                                            StrToInt(PrmsLn[COL_SER_ARR]),
                                            StrToInt(PrmsLn[COL_PT_ARR])
                                           );
                       SetChanceObstacle(StrToInt(PrmsLn[COL_CHANCE]),
                                         StrToInt(PrmsLn[COL_OBSTACLE]));
                       SetNumeroReseau(0); // non supporté par TOPOROBOT
                       SetObsSerie('');
                       SetRaideur(1.00); // Raideur: Non supporté par TOPOROBOT
                     end; //with UneSerie do
                   end else
                   begin // on lit les visées
                     //                 ('-->'+LigneTab);
                     //*******************************************************************************
                     //                                                           Long
                     //                                                            |   Azimut
                     //                                                            |   |   Pente
                     //                                      Parenthèse            |   |   |   LG
                     //                                      | Serie               |   |   |   |   LD
                     //                                      | |   Station         |   |   |   |   |   HZ
                     //                                      | |   |  Code         |   |   |   |   |   |   HN
                     //                                      | |   |  |    |Session|   |   |   |   |   |   |
                     //                                      | |   |  |    | Expé  |   |   |   |   |   |   |
                     PrmsLn := SplitFixedColLine(LigneTEXT, [1, 2, 7, 13, 17, 21, 25, 33, 41, 49, 57, 65, 73, 81, 100]);

                     with qVisee do
                     begin
                       TypeVisee   := tgDEFAULT;
                       Code        := StrToInt(PrmsLn[COL_CODE]);
                       Expe        := StrToInt(PrmsLn[COL_EXPE]);
                       Longueur    := ConvertirEnNombreReel(PrmsLn[COL_LONG], 0.00);
                       if (Longueur < 0.00) then
                       begin
                         WriteWarning(Format(rsRD_TAB_NEG_LONG, [NoLigneTEXT, Longueur]));
                         Longueur:=Abs(Longueur);
                       end;
                       Azimut      := ConvertirEnNombreReel(PrmsLn[COL_AZ], 0.00);
                       Pente       := ConvertirEnNombreReel(PrmsLn[COL_P], 0.00);
                       LG          := ConvertirEnNombreReel(PrmsLn[COL_LG], 0.00);  //LG
                       LD          := ConvertirEnNombreReel(PrmsLn[COL_LD], 0.00);  //LD
                       HZ          := ConvertirEnNombreReel(PrmsLn[COL_HZ], 0.00);
                       HN          := ConvertirEnNombreReel(PrmsLn[COL_HN], 0.00);
                       Commentaires:= '';
                       IDSecteur   := 0;
                       IDTerrainStation := '';   // Non supporté par TOPOROBOT
                       TypeVisee        := tgDEFAULT; // Non supporté par TOPOROBOT
                     end; // with qVisee do
                     UneSerie.AddVisee(qVisee);
                   end;   // if Prefix2
                 end; // case
               end;
             end; //  case Prefix1 of
           end;
           IdxCourantStation := UneSerie.GetNbVisees - 1;
           // imprimer la ligne traitée
           //AfficherMessage('Ligne traitée');
         except
           AfficherMessageErreur(Format('[%d] %s', [NoLigneTEXT, LigneTEXT]));
         end;
       end;  //while Not Eof(pTEXT) do
       // On ferme la dernière série
       AddSerie(UneSerie);
       //********************************
       Preconditionner(FichierText);
       //********************************
       Result := GetNbSeries();
       AfficherMessage('Fichier TEXT OK');
     finally
       CloseFile(pTEXT);
     end; // niveau 1: pour le fichier Text
  finally
    FreeAndNil(ProvListeEntrees);//ProvListeEntrees.Free;
  end; //try..finally niveau 0
end;
//******************************************************************************
// pour le moment, on exporte au format Text de Toporobot
// Statut: OK, mais des détails à revoir.
// Entrées multiples OK
// Nota: Support du TLM330 non implémenté volontairement
//*)
procedure TToporobotStructure2012.ExporterVersToporobotTEXT(const QFileName: TStringDirectoryFilename;
                                                            const LongueurMaxAntenne: double;
                                                            const DoExportAntennaShots: boolean);
                                                            deprecated 'Le format TOPOROBOT Text est peu pratique';
var
  fp: TextFile;
  EWE: String;
  myEntree: TEntrance;
  i, Nb, j, NbV: Integer;
  myExpe: TExpe;
  myCode: TCode;
  mySerie: TObjSerie;
  myVisee: TUneVisee;
  NbA: Integer;
  k: Integer;
  myAntenne: TViseeAntenne;
  q: Integer;
  zdar: String;
  function makeColonnesEnTete(const IsCommentaire: boolean;
                              const Section, ID1, ID2, ID3, ID4: integer): string;
  begin
    Result := IIF(IsCommentaire, '(', ' ') +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [Section]), 5, taRightJustify) +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [ID1])    , 6, taRightJustify) +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [ID2])    , 4, taRightJustify) +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [ID3])    , 4, taRightJustify) +
              FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [ID4])    , 4, taRightJustify);
  end;
begin
  AfficherMessage(Format('%s.ExporterVersToporobotTEXT: %s', [ClassName, QFileName]));
  AssignFile(fp, QFileName);
  try
    ReWrite(fp);
    // section -6 = entrée
    Nb := GetNbEntrances();
    for i := 0 to Nb - 1 do
    begin
      myEntree := self.GetEntrance(i);
      EWE := makeColonnesEnTete(false, -6, i + 1, 1, 1, 1) + ' ' + myEntree.eNomEntree;
      WriteLn(fp, EWE);
    end;
    for i := 0 to Nb - 1 do
    begin
      myEntree := self.GetEntrance(i);
      // section -5 = Coordonnées
      //-5     1   1   1   1   629000.72   178000.68     1777.71     1     0
      EWE := makeColonnesEnTete(false, -5, i+1, 1, 1, 1) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myEntree.ePosition.X]), 12, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myEntree.ePosition.Y]), 12, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myEntree.ePosition.Z]), 12, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER,    [myEntree.eRefSer]), 6, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER,    [myEntree.eRefSt]), 6, taRightJustify);
      WriteLn(fp, EWE);
    end;

    // section -4: Horodatage
    //-4     1   1   1   1 14/12/08 11:47:22  Guest
    for i := 1 to 2 do
    begin
      EWE := makeColonnesEnTete(false, -4, i, 1, 1, 1) +
             FormatterFixedColumn(FormatterDateHeureToporobot(Now), 18, taRightJustify) +
             '  ' + 'Guest';
      WriteLn(fp, EWE);
    end;
    // Section -3: Inutilisée
    EWE := makeColonnesEnTete(false, -3, 1, 1, 1, 1);
    WriteLn(fp, EWE);
    // Section -2: Expés
    // -2     1   1   1   1 25/08/07  MCH           AH            1    0.00   0   1
    Nb := self.GetNbExpes;
    for i := 1 to Nb - 1 do
    begin
      myExpe := GetExpe(i);
      {$WARNING: TEXpe.DateExpe à implementer}
      EWE := makeColonnesEnTete(false, -2, myExpe.IDExpe, 1, 1, 1) +
             FormatterFixedColumn(Format('%.2d/%.2d/%.2d', [myExpe.JourExpe, myExpe.MoisExpe, myExpe.AnneeExpe mod 100]), 9, taRightJustify) +
             '  ' +
             FormatterFixedColumn(myExpe.Operateur, 13, taLeftJustify) + ' ' +
             FormatterFixedColumn(myExpe.ClubSpeleo, 13, taLeftJustify) + ' ' +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [0]), 2, taLeftJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 7, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [0]), 4, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [1]), 4, taRightJustify);
      WriteLn(fp, EWE);
    end;
    // Section -1: Codes
    //-1     1   1   1   1  360.00  360.00    0.05    1.00    1.00  100.00  100.00
    Nb := self.GetNbCodes;
    for i := 1 to Nb - 1 do
    begin
      myCode := GetCode(i);
      EWE := makeColonnesEnTete(false, -1, myCode.IDCode, 1, 1, 1) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myCode.GradAz]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myCode.GradInc]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.05]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [1.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [1.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [100.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [100.00]), 8, taRightJustify);
      WriteLn(fp, EWE);
    end;
    // Sections > 0 = séries
    Nb := self.GetNbSeries;
    for i := 1 to Nb - 1 do
    begin
      mySerie := GetSerie(i);
      //1    -2   1   1   1 GALERIE D'ENTREE D10H10
      EWE := makeColonnesEnTete(false, mySerie.GetNumeroDeSerie(), -2, 1, 1, 1) +
             ' ' + mySerie.GetNomSerie;
      WriteLn(fp, EWE);
      //1    -1   1   1   1       1       0       1       6       6       3       0
      EWE := makeColonnesEnTete(false, mySerie.GetNumeroDeSerie(), -1, 1, 1, 1) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNoSerieDep]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNoPointDep]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNoSerieArr]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNoPointArr]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetNbVisees - 1]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetChance]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_INTEGER, [mySerie.GetObstacle]), 8, taRightJustify);
      WriteLn(fp, EWE);
      nbV := mySerie.GetNbVisees;
      EWE := makeColonnesEnTete(false, mySerie.GetNumeroDeSerie(), 0, 1, 1, 1) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify) +
             FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [0.00]), 8, taRightJustify);
      WriteLn(fp, EWE);
      for j := 1 to nbV -1 do   // et non 'for j = 0 to nbV - 1'
      begin
        myVisee := mySerie.GetVisee(j);
        EWE := makeColonnesEnTete(false, mySerie.GetNumeroDeSerie(), j, 1, myVisee.Code, myVisee.Expe) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.Longueur]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.Azimut]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.Pente]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.LG]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.LD]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.HZ]), 8, taRightJustify) +
               FormatterFixedColumn(Format(FORMAT_NB_REAL_2_DEC, [myVisee.HN]), 8, taRightJustify);
        WriteLn(fp, EWE);

        // export des commentaires
        q := 0;
        zdar := Trim(myVisee.Commentaires);
        if (zdar <> '') then
        begin
          EWE := makeColonnesEnTete(true, mySerie.GetNumeroDeSerie(), j, 0, myVisee.Code, myVisee.Expe) + ' ' + zdar;
          WriteLn(fp, EWE);
          q+=1;
        end;
        // exporter ici les visées en antenne
        if (DoExportAntennaShots) then
        begin
          NbA := GetNbAntennes;
          if (NbA > 0) then
          begin
            for k := 0 to NbA - 1 do
            begin
              myAntenne := GetViseeAntenne(k);
              if ((myAntenne.SerieDepart = mySerie.GetNumeroDeSerie()) and
                  (myAntenne.PtDepart    = j) and
                  (myAntenne.Longueur < LongueurMaxAntenne)) then
              begin
                // PocketTopo stocke les antennes dans les commentaires du point d'accrochage
                EWE := makeColonnesEnTete(true, myAntenne.SerieDepart, myAntenne.PtDepart, q, 0, 0) + //myAntenne.Code, myAntenne.Expe) +
                       Format(' [%.2f %.1f %.1f]', [myAntenne.Longueur, myAntenne.Azimut, myAntenne.Pente]);
                WriteLn(fp, EWE);
                q += 1;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    CloseFile(fp);
  end;
end;
