unit unitAutoCompletionSpeleo;
// Aide à la saisie et complètement de commentaires topo spéléo

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Classes, SysUtils;

type

{ TAutoCompletionSpeleo }

  TAutoCompletionSpeleo = class(TStringList)
  private


  public
    function  Initialiser(): boolean;
    procedure Finaliser();
    function  getElement(const Idx: integer): string;
    function  getNbElements(): integer;
    function  FindIdxItemContainingText(const SubStr: string): integer;

end;


implementation

{ TAutoCompletionSpeleo }

function TAutoCompletionSpeleo.Initialiser(): boolean;
begin
  result := false;
  self.Clear;
  // d'abord les phrases types
  self.Add('<rien>');
  self.Add(GetResourceString('Carrefour'));
  self.Add(GetResourceString('Départ à droite'));
  self.Add(GetResourceString('Départ à gauche'));
  self.Add(GetResourceString('Départs'));
  self.Add(GetResourceString('Cloche karstique'));
  self.Add(GetResourceString('Colmatage argileux'));
  self.Add(GetResourceString('Effondrement de plafond'));
  self.Add(GetResourceString('Salle ébouleuse'));
  self.Add(GetResourceString('Mur à descendre'));
  self.Add(GetResourceString('A poursuivre'));
  self.Add(GetResourceString('A topographier'));
  self.Add(GetResourceString('Arrêt topo'));
  self.Add(GetResourceString('Base de cheminée'));
  self.Add(GetResourceString('Sommet de puits'));
  self.Add(GetResourceString('Zone inondée'));
  self.Add(GetResourceString('Chatière'));
  self.Add(GetResourceString('Ressaut'));
  self.Add(GetResourceString('Bassin'));
  self.Add(GetResourceString('Concrétions'));
  self.Add(GetResourceString('Puits noyé'));
  self.Add(GetResourceString('Perte'));
  self.Add(GetResourceString('Arrivée d''eau'));
end;

procedure TAutoCompletionSpeleo.Finaliser();
begin
  self.Clear;
end;

function TAutoCompletionSpeleo.getNbElements(): integer;
begin
  result := self.Count;
end;

function TAutoCompletionSpeleo.FindIdxItemContainingText(const SubStr: string): integer;
var
  Nb: Integer;
begin
  result := -1;
  // balayage, suffisant ici.
  Nb := self.Count;
  //self.IndexOf();
end;

function TAutoCompletionSpeleo.getElement(const Idx: integer): string;
begin
  if (0 = Idx) then exit('');
  result := self.Strings[Idx];
end;


end.

