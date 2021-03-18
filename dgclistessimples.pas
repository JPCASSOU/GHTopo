unit DGCListesSimples;
// Listes simples

{$mode delphiunicode}

interface

uses
  Classes, SysUtils; //, AvgLvlTree;

type TDGCListeSimple<T> = class(TFPList)
  private
  public
    procedure ClearListe();
    function  GetNbElements(): integer; inline;
    procedure InsertElement(const Idx: integer; const E: T);
    procedure AddElement(const E: T);  // et Rros Minet
    function  GetElement(const Idx: integer): T; inline;
    procedure PutElement(const Idx: integer; const E: T); inline;
    function  RemoveElement(const Idx: integer): boolean;
    procedure EnQueue(const E: T); inline; // file: Ajoute un entrant
    function  DeQueue(): T;                // file: extrait et supprime le premier entré (l'élément 0 est devant le guichet)
    procedure Push(const E: T); inline;    // Pile: Ajoute un élément
    function  Pop(): T;                    // Pile: Extrait le dernier élément entré et le supprime de la liste
end;


implementation

procedure TDGCListeSimple<T>.ClearListe();
var
  i, n: Integer;
begin
  //AfficherMessage(Format('%s.ClearListe()', [classname]));
  n := self.Count;
  if (n > 0) then
  for i:=Count-1 downto 0 Do
  begin
    if (self.Items[i] <> Nil) then Dispose(self.Items[i]); // Libération
    self.Delete(i);                                        // Suppression de l'élément
  end;
end;





function TDGCListeSimple<T>.GetNbElements: integer;
begin
  Result := self.Count;
end;

procedure  TDGCListeSimple<T>.InsertElement(const Idx: integer; const E: T);
var pE: ^T;
begin
  New(pE);
  pE^ := E;
  self.Insert(Idx, pE);
end;
procedure TDGCListeSimple<T>.AddElement(const E: T);
var pE: ^T;
begin
  New(pE);
  pE^ := E;
  self.Add(pE);
end;

function TDGCListeSimple<T>.GetElement(const Idx: integer): T;
begin
  Result := T(Items[Idx]^);
end;



procedure TDGCListeSimple<T>.PutElement(const Idx: integer; const E: T);
begin
  T(Items[Idx]^) := E;
end;

function TDGCListeSimple<T>.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;

procedure TDGCListeSimple<T>.EnQueue(const E: T);
begin
  AddElement(E);
end;

function TDGCListeSimple<T>.DeQueue(): T;
begin
  Result := GetElement(0);
  RemoveElement(0);
end;

procedure TDGCListeSimple<T>.Push(const E: T);
begin
  AddElement(E);
end;

function TDGCListeSimple<T>.Pop(): T;
var
  n: Integer;
begin
  n := GetNbElements();
  if (n > 0) then
  begin
    Result := GetElement(n-1);
    RemoveElement(n-1);
  end;
end;





end.

