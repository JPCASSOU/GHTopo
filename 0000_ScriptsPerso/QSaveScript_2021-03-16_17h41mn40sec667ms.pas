program NewScript01;
type TVectorResult = array[0..31] of string;
var
  VR   : TVectorResult;
  i, Nb: integer;
begin
  // Votre code ici
  if (DT_BeginNewSerie(505,  0, 0, 503, 2, -1, -1, 'Serie ajoutée depuis script')) then
  begin
    DT_AddNewVisee(1, 1, 0, 0, 12.66, 135, -45, 1.0, 1.0, 1.5, 1.0, 'F666', 'Visee1');
    DT_AddNewVisee(1, 1, 0, 0, 6.25, 145, -33, 1.0, 1.0, 1.5, 1.0, 'F668', 'Visee2');
    DT_AddNewVisee(1, 1, 0, 0, 11.25, 225, -8, 1.0, 1.0, 1.5, 1.0, 'F669', 'Visee3');
    DT_AddNewVisee(1, 1, 0, 0, 12.25, 235, -44, 1.0, 1.0, 1.5, 1.0, 'F670', 'Visee4');
    DT_EndNewSerie();
  end;
end.
(*

DT_AddNewVisee(const QExpe, QCode: integer;
                                         const QTypeVisee, QSecteur: integer;
                                         const QLong, QAz, QPente, QLG, QLD, QHZ, QHN: double;
                                         const QIDTerrain, QObserv: string): boolean;
                                         *)
