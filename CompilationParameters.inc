// Paramètres de compilations pour GHTopo
// 13/06/2019: Point de contrôle temporel (contrôle de version)
// ---------------------------------------
//
//******************************************************************************
//*** SECTION A NE PAS MODIFIER                                              ***
// Mode DELPHI
{$mode delphi} {$H+}
// alignement sur 32 bits
{$PACKRECORDS 4}
// Interfaces utilisateur
//------------------------------------------------------------------------------
//******************************************************************************
//*** FIN DE LA SECTION A NE PAS MODIFIER                                    ***
//******************************************************************************
//******************************************************************************
//******************************************************************************

{$DEFINE MULTI_THREADING}
{$UNDEF MULTI_THREADING}     // Aucun gain en multithread !  Difficulté +++

{$IFDEF MULTI_THREADING}
  {$ERROR Ne pas utiliser le multithreading (inadapté avec les TList}
{$ENDIF}

{$DEFINE CALCULETTE_EMBEDDED_IN_GHTOPO}
{.$UNDEF CALCULETTE_EMBEDDED_IN_GHTOPO}
// Langues
{$DEFINE FRENCH_MESSAGES}
{.$UNDEF FRENCH_MESSAGES}

{$DEFINE ENGLISH_MESSAGES}
{$UNDEF  ENGLISH_MESSAGES}

{$DEFINE CHINESE_MESSAGES}
{$UNDEF  CHINESE_MESSAGES}

{$DEFINE SPANISH_MESSAGES}
{$UNDEF  SPANISH_MESSAGES}

{$DEFINE ITALIAN_MESSAGES}
{$UNDEF  ITALIAN_MESSAGES}

{$DEFINE DEUTSCH_MESSAGES}
{$UNDEF  DEUTSCH_MESSAGES}

//******************************************************************************
// Spécifique Sylvestre CLEMENT
{$DEFINE GROS_MINET}
{$UNDEF GROS_MINET}

//******************************************************************************
//******************************************************************************
// Affichage des fenêtres
// Modèle de stockage des matrices
// Si ce paramètre est défini, ce sera des tableaux de listes
// sinon, c'est des TArray2Df
{$DEFINE USE_MATRIX_DATASTRUCTURE_LIST_OF_LISTS}
{$UNDEF  USE_MATRIX_DATASTRUCTURE_LIST_OF_LISTS}

//******************************************************************************
// système d'exploitation
{$NOTE: Les variables MSWINDOWS et LINUX sont prédéfinies}
{$IFDEF LINUX}
  {$WARNING: Si erreur du type 'Unit system used by fcl', vérififier ds 'Options du projet'): TargetOS=Linux}
{$ENDIF}
//******************************************************************************
// Compiler pour Raspberry Pi
// /!\ Dans les options de compilation, désactiver toutes les optimisations
{$DEFINE RASPBERRY_PI}
{$UNDEF RASPBERRY_PI}

// Mode pour la version de GHTopo sur RPI ou tablette
// Ne peut pas partir dans le DPR du projet
{$DEFINE GHTOPO_SIMPLIFIE}         // Par défaut: our version Raspberry, valable aussi sur PC
{$UNDEF GHTOPO_SIMPLIFIE}          // Si désactivé, passe en GHTopo standard multi-fenêtres (Delphi-like)
// utiliser les coupes développées
{$DEFINE USE_COUPES_DEVELOPPEES}
{.$UNDEF USE_COUPES_DEVELOPPEES}
// utiliser le viewer OpenGL ?
{$DEFINE USE_VIEWER_OPENGL}
{.$UNDEF USE_VIEWER_OPENGL} // TOpenGL inutilisable sous Linux

// Spécial Raspberry PI
{$IFDEF RASPBERRY_PI}
  {$DEFINE GHTOPO_SIMPLIFIE}       // Armement automatique le mode GHTopo simplifié
{$EndIf RASPBERRY_PI}
{$IFDEF GHTOPO_SIMPLIFIE}
  {$UNDEF USE_VIEWER_OPENGL}       // Le mode GHTopo simplifié désarme le viewer OpenGL
  {$UNDEF USE_COUPES_DEVELOPPEES}  // Désarme le mode Coupes développées
  {$UNDEF GROS_MINET}              // Désarme le mode Gros Minet
{$ENDIF}
// MacOS NON SUPPORTE

// Spécial Windows
{$IFDEF MSWINDOWS}
   {$DEFINE DISP_FRM_JOURNAL}    // journal ; sinon, le terminal fait l'affaire
{$ENDIF}

// Spécial Linux
{$IFDEF LINUX}
  {$WARNING: Retirer éventuellement la dépendance OpenGL dans l'inspecteur de projet}
  {.$UNDEF USE_COUPES_DEVELOPPEES}
  {$UNDEF USE_VIEWER_OPENGL}   // on désactive le visu OpenGL
{$ENDIF}


// Autres paramètres Spécial Raspberry PI
{$IFDEF RASPBERRY_PI}
  //{$UNDEF MSWINDOWS}                 // Un RPI est sous Linux
  //{$DEFINE LINUX}
  {$UNDEF DISP_FRM_JOURNAL}            // Journal désactivé
  {$DEFINE GHTOPO_SIMPLIFIE}           // Mode GHTopo simplifié mono fenêtre
  ///!\ Retirer éventuellement la dépendance OpenGL dans l'inspecteur de projet
  ///!\ Retirer éventuellement la dépendance LazSynaser dans l'inspecteur de projet
{$ENDIF}
// débogage: assertions
{$ASSERTIONS ON}
{.$ASSERTIONS OFF}


