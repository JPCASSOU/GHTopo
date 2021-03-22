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
  MaillageStatus := MNT_LoadMaillage(GetGHTopoDirectory() + '000000_MNTCoume1.mai');
  case MaillageStatus of
    -64: print('*** Maillage non assigné');
     -1: print('*** Erreur en chargement du maillage');
      0: printf('Maillage chargé: %d noeuds et %d triangles', [MNT_GetNbVertex(), MNT_GetNbTriangles()]);
      1: printf('Maillage valide déjà en mémoire: %d noeuds et %d triangles', [MNT_GetNbVertex(), MNT_GetNbTriangles()]);
  else
   pass;
  end;
  Exit;
  if (MaillageStatus >= 0) then
  begin
    NbSeries := GetNbSeries();
    printf('Distances de recouvrement pour les stations topo (%d séries)',[NbSeries]);
    for i := 1 to NbSeries - 1 do
    begin
      if (DT_GetNumeroEtNomDeSerieByIdx(i, QNumSerie, QNomSerie, QNbVisees)) then
      begin
        printf('#Série %d: %d (%d visées): %s', [i, QNumSerie, QNbVisees, QNomSerie]);
        print('#==========================');
        print('#Station; X; Y; Z; Z surface; Recouvrement');
        for j := 1 to QNbVisees - 1 do
        begin
          if (BDE_ExtractCoordsFromSerSt(QNumSerie, j, QX, QY, QZ)) then
          begin
            MNT_ExtractAltitudeFromXY(QX, QY, QZSurface);
            QRecouvrement := QZSurface - QZ;
            printf('   %d.%d; %.2f; %.2f; %.2f; %.2f; %.2f', [QNumSerie, j, QX, QY, QZ, QZSurface, QRecouvrement]);
          end;
        end;
        print('');
      end;
    end;
  end;
end.
