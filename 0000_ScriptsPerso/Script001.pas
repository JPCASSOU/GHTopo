program NewScript01;
var
  i, Nb: integer;
begin
  cls;
  // Votre code ici
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
end.
