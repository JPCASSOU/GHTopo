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
  if (MaillageStatus >= 0) then
  begin
    MNT_ClearProfils();
    MNT_GenerateSetOfProfils(425906, 6415309 ,
                             427287, 6415673,
                             426235, 6416860,
                             100, 100, 1);

  end;
end.
