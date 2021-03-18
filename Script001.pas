// Script PascalScript de rédaction des requêtes 6 Minutes
// à exécuter dans l'interpréteur Pascal de GHTopo
program Toto;
const TAB = '\t';
const CRLF = '\n';
const AggegateFunc       = 'AVG'; // AVG: moyenne, SUM: somme, COUNT: Nb, etc ...

const TABLE_CANALS       = 'libscanal';
const TABLE_6_MIN        = 'donnees6minutes_2019_new_format';
const Field_LibCanal     = 'LibCanal';
const Field_Station      = 'Station';
const Field_IDLibCanal   = 'IDLibCanal';
const Field_HeureNum     = 'HeureNum';
const Field_Mesure       = 'Mesure';
const OUTPUT_DIR         = 'D:/0_6Min_Fauque/';
const NATURE_MESURE_DEBITS    = 1; // 1 = débits; 2 = vitesses
const NATURE_MESURE_VITESSES  = 2; // 1 = débits; 2 = vitesses

const Titre_Boucle       = 'Boucle' + TAB + Field_Station;
function FormatterDateSQL(const j, m, a: word): string;
begin
  Result := format('''%.4d-%.2d-%.2d''', [a, m, j]);
end;

function FormatterLibCanal(const QLibCanal: string): string;
var i: integer;
begin
  Result := QLibCanal;
  for i := 1 to Length(Result) do
  begin
    if (Result[i] = '.') then Result[i] := '-';
    if (Result[i] = ' ') then Result[i] := '_';
  end;
end;

function ExtractData6MinFromIdxCanal(const QIdxLibCanal: integer; const NatureMesureNum: byte; const QJour, QMois, QAnnee: integer): boolean;
var
  QOutputFile: string;
  TypeData   : string;
begin
  case NatureMesureNum of
    0: TypeData := 'Unknown';
    1: TypeData := 'Debits';
    2: TypeData := 'Vitesses';
    3: TypeData := 'Taux';
  end;
  printf('-- Extraction des %s depuis  %s - %s = %d', [TypeData, TABLE_6_MIN, Field_IDLibCanal, QIdxLibCanal]);
  QOutputFile := sprintf('%s%s_Canal_%d_%.4d-%.2d-%.2d.csv', [OUTPUT_DIR, TypeData, QIdxLibCanal, QAnnee, QMois, QJour]);
  printf('-- Les résultats iront dans %s', [QOutputFile]);
  printf('SELECT %s,', [Field_HeureNum]);
  printf('  (SELECT CONCAT(%s, ''%s'', %s) FROM %s WHERE (%s = %d)) AS "%s", -- %s',
         [Field_LibCanal, TAB, Field_Station,
          TABLE_CANALS, Field_IDLibCanal, QIdxLibCanal, Titre_Boucle, 'pour acquittement de LibCanal'
         ]);
  printf('          REPLACE(Format(%s(%s), 2), ".", ",") AS "%s" -- %s',
         [AggegateFunc, Field_Mesure, TypeData, 'format numérique des valeurs compatible LOO'
         ]);
  printf('FROM %s', [TABLE_6_MIN]);
  print('WHERE');
  printf('  (%s = %d) AND  -- %s', [Field_IDLibCanal, QIdxLibCanal, 'numéro de canal - Ne pas utiliser getIdxLibCanalFromStation() ici: très lent']);
  printf('  (NatureMesureNum = %d) AND -- %s', [NatureMesureNum, '1 = Débits, 2 = Vitesses, 3 = Taux']);
  printf('  (Mesure >= %d) and -- %s', [0, 'on vire les visées aberrantes']);
  printf('  (DateNum BETWEEN %s AND %s) -- %s', [FormatterDateSQL(QJour, QMois, QAnnee), FormatterDateSQL(QJour, QMois, QAnnee), 'Ces deux dates incluses']);
  printf('GROUP BY %s ASC', [Field_HeureNum]);
  printf('INTO OUTFILE "%s" FIELDS TERMINATED BY ''%s'' LINES TERMINATED BY ''%s''', [QOutputFile, TAB, CRLF]);
  printf('; -- %s', ['Fin de la requête']);
end;
var
  i, jour, Mois, Annee: integer;
begin
  cls;
  Mois  := 9;
  Annee := 2019;
  for Jour := 23 to 30 do
  begin
    print('');
    printf('-- Pour le jour: %.2d-%.2d-%.4d', [Jour, Mois, Annee]);
    print('-- ------------------------------------');
    for i := 63 to 63 do  //410
    begin
      ExtractData6MinFromIdxCanal(i, NATURE_MESURE_DEBITS   , Jour, Mois, Annee);
      ExtractData6MinFromIdxCanal(i, NATURE_MESURE_VITESSES , Jour, Mois, Annee);
    end;
  end;
  Mois  := 10;
  Annee := 2019;
  for Jour := 1 to 20 do
  begin
    print('');
    printf('-- Pour le jour: %.2d-%.2d-%.4d', [Jour, Mois, Annee]);
    print('-- ------------------------------------');
    for i := 63 to 410 do  //410
    begin
      ExtractData6MinFromIdxCanal(i, NATURE_MESURE_DEBITS   , Jour, Mois, Annee);
      ExtractData6MinFromIdxCanal(i, NATURE_MESURE_VITESSES , Jour, Mois, Annee);
    end;
  end;
  (*
  for i := 63 to 410 do
  begin
    ExtractData6MinFromIdxCanal(i, NATURE_MESURE_DEBITS);
    ExtractData6MinFromIdxCanal(i, NATURE_MESURE_VITESSES);
  end;
  // *)
end.
(*

select HeureNum,
       -- On utilise ici une astuce qui va créer deux colonnes distinctes pour la boucle et la station lors d'un export vers tableur:
         -- un caractère Tabulation '\t' est inséré
         --                       | Diviseur de colonne ici                                         | Parfaitement valide !
       (select CONCAT(LibCanal, '\t', Station) from libscanal where (IDLibCanal = 231)) as "Boucle\tStation", -- pour acquittement de LibCanal
         Replace(Format(avg(Mesure), 2), ".", ",")                                     as "Vitesse" -- format numérique des valeurs compatible LOO
from  donnees6minutes_2019_new_format
   where
   (IDLibCanal = 231) and                -- numéro de canal - Ne pas utiliser getIdxLibCanalFromStation() ici: très lent;
   (NatureMesureNum = 2) and             -- 1 = Débits, 2 = Vitesses, 3 = Taux
   (Mesure >= 0) and                     -- on vire les visées aberrantes
   (DateNum BETWEEN '2019-09-01' and '2019-10-31') and
   -- (Month(DateNum) between 9 and 10) and -- septembre et octobre
   (DAYOFWEEK(DateNum) BETWEEN 2 and 6)  -- En jours ouvrés: 1=Dimanche, 2=Lundi, ..., 7=Samedi
   group by HeureNum asc;
   -- into outfile "D:/0_6Min_Fauque/82_vitesses.csv"
   -- FIELDS TERMINATED BY '\t' LINES TERMINATED BY '\n';

//*)
