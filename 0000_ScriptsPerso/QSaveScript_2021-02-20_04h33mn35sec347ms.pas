// exemple de script utilisant PascalScript
program Toto;
var
  MaillageStatus : integer;
  NbSeries: integer;
  S            : string;
  i, j, Nb, EWE  : integer;
  X0, Y0       : double;
  X1, Y1       : double;
  Pas          : double;

  PtInX , PtInY : double;
  PtOutX, PtOutY: double;
  QV: extended;
  Declimag: double;

  // séries
  QNumSerie: integer;
  QNomSerie: string;
  QNbVisees: integer;

  QX, QY, QZ, QZSurface, QRecouvrement: double;
begin
  print('Cavité: ' + GetNomEtude());
  print('====================');
  printf('%d espaces de noms', [GetNbNamespaces()]);
  printf('%d entrées', [GetNbEntrees()]);
  printf('%d séries', [GetNbSeries()]);
  printf('%d réseaux', [GetNbReseaux()]);
  printf('%d secteurs', [GetNbSecteurs()]);
  printf('%d expés', [GetNbExpes()]);
  printf('%d codes', [GetNbCodes()]);
  printf('%d antennes', [GetNbAntennes()]);
  print('******************************************');
  EWE := 42;
  print(GetGHTopoDirectory());
  // Maillages
  cls;
  print('Chargement d''un maillage');
  MaillageStatus := MNT_LoadMaillage(GetGHTopoDirectory() + '00_Maillage_Villars_001.mai');
  case MaillageStatus of
    -64: print('*** Maillage non assigné');
     -1: print('*** Erreur en chargement du maillage');
      0: printf('Maillage chargé: %d noeuds et %d triangles', [MNT_GetNbVertex(), MNT_GetNbTriangles()]);
      1: printf('Maillage valide déjà en mémoire: %d noeuds et %d triangles', [MNT_GetNbVertex(), MNT_GetNbTriangles()]);
  else
   pass;
  end;
  if (MaillageStatus >= 0) then
  begin
    MNT_ClearProfils();
    MNT_GenerateSetOfProfils(526732, 6484828 ,
                             526920, 6484793,
                             526840, 6484955,
                             20, 45, 3);

  end;
end.
