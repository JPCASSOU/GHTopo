unit DGCBinaryTree;

{$mode delphiunicode}
interface
uses
  DGCListesSimples,
  Classes, SysUtils;

//AvgLvlTree
// TODO: Utiliser https://wiki.lazarus.freepascal.org/AVL_Tree

// Tiré de https://rmdiscala.developpez.com/cours/LesChapitres.html/Cours4/Chap4.8.htm

// tas = arbre binaire parfait (complet ou non)
// https://fr.wikipedia.org/wiki/Tas_binaire

type TNodeIndex = type Int64;
type

{ TDGCBinaryTree }

 TDGCBinaryTree = class
  private
    FNodeIndex: TNodeIndex;
    filsG , filsD : TDGCBinaryTree;
    procedure Add(var SubTree: TDGCBinaryTree; const N: TNodeIndex);

    function ChercherArbre(SubTree: TDGCBinaryTree; const N: TNodeIndex): TDGCBinaryTree;

    procedure FreeRecur(SubTree: TDGCBinaryTree);
    procedure ParcoursInfixeOrdre(SubTree: TDGCBinaryTree);
    procedure ParcoursInfixe();

    procedure ParcoursPostordre(SubTree: TDGCBinaryTree);
    procedure ParcoursPreordre(SubTree: TDGCBinaryTree);
    function PlusGrand(SubTree: TDGCBinaryTree): TDGCBinaryTree;
    function PlusPetit(SubTree: TDGCBinaryTree): TDGCBinaryTree;
    procedure Remove(var SubTree: TDGCBinaryTree; const N: TNodeIndex);

  public
    procedure AddNode(const N: TNodeIndex);
    procedure Chercher(const N: TNodeIndex);
    procedure ParcoursPrefixe();
    procedure ParcoursPostfixe();
    constructor CreerTreeBin(const N: TNodeIndex);overload;
    constructor CreerTreeBin(const N: TNodeIndex; fg , fd : TDGCBinaryTree);overload;
    destructor  Finaliser();
end;

type TListeNoeudsParcourus = class(TDGCListeSimple<TDGCBinaryTree>)
  private
  public
end;

type

{ TDGCHeap }

 TDGCHeap = class(TListeNoeudsParcourus)
  private
    FRoot: TDGCBinaryTree;
  public
    function Initialiser(): boolean;
    procedure Finaliser();
end;


implementation



{ TDGCBinaryTree }

procedure TDGCBinaryTree.FreeRecur(SubTree: TDGCBinaryTree);
begin
  FreeRecur(SubTree.filsG);
  FreeRecur(SubTree.filsD);
  FreeAndNil(SubTree);
end;

constructor TDGCBinaryTree.CreerTreeBin(const N: TNodeIndex);
begin
  self.FNodeIndex := N;
  self.filsG := nil;
  self.filsD := nil;
end;

constructor TDGCBinaryTree.CreerTreeBin(const N: TNodeIndex; fg, fd: TDGCBinaryTree);
begin
  self.FNodeIndex := N;
  self.filsG := fg;
  self.filsD := fd;
end;

procedure TDGCBinaryTree.Add(var SubTree: TDGCBinaryTree; const N: TNodeIndex);
{remplissage récursif de l'arbre binaire de recherche
par adjonctions  successives aux feuilles de l'arbre }
begin
 if (not Assigned(SubTree)) then
   SubTree := TDGCBinaryTree.CreerTreeBin(N)
 else
   if (N <= SubTree.FNodeIndex) then Add(SubTree.filsG, N)
                                else Add(SubTree.filsD, N);
end;

procedure TDGCBinaryTree.AddNode(const N: TNodeIndex);
begin
  self.Add(self, N);
end;




destructor TDGCBinaryTree.Finaliser();
begin
  FreeRecur(self);
  self := nil;
end;

procedure TDGCBinaryTree.ParcoursPreordre(SubTree : TDGCBinaryTree);
begin
  if (SubTree <> nil) then
  begin
    // le traitement ici
    //...
    ParcoursPreordre(SubTree.filsG);
    ParcoursPreordre(SubTree.filsD);
  end;
end;
procedure TDGCBinaryTree.ParcoursPostordre(SubTree : TDGCBinaryTree);
begin
  if (SubTree <> nil) then
  begin
    ParcoursPostordre(SubTree.filsG);
    ParcoursPostordre(SubTree.filsD);
    // le traitement ici
    //...
  end;
end;
procedure TDGCBinaryTree.ParcoursInfixeOrdre(SubTree : TDGCBinaryTree);
begin
  if (SubTree <> nil) then
  begin
    ParcoursInfixeOrdre(SubTree.filsG);
    // Le traitement ici
    //....
    ParcoursInfixeOrdre(SubTree.filsD);
  end
end;

procedure TDGCBinaryTree.ParcoursPrefixe();
begin
  ParcoursPreordre(self);
end;
procedure TDGCBinaryTree.ParcoursPostfixe();
begin
  ParcoursPostordre(self);
end;
procedure TDGCBinaryTree.ParcoursInfixe();
begin
  ParcoursInfixeOrdre(self);
end;

function TDGCBinaryTree.ChercherArbre(SubTree: TDGCBinaryTree; const N: TNodeIndex) : TDGCBinaryTree;
begin
  if (not Assigned(SubTree)) then exit(nil);
  if (N = SubTree.FNodeIndex) then
    result := SubTree //l'élément est dans ce noeud
  else
    if (N < SubTree.FNodeIndex) then
      result := ChercherArbre(SubTree.filsG , N) //on cherche à gauche
    else
      result := ChercherArbre(SubTree.filsD , N) //on cherche à droite
end;
procedure TDGCBinaryTree.Chercher (const N: TNodeIndex);
begin
  ChercherArbre( self , N)
end;
function TDGCBinaryTree.PlusGrand(SubTree: TDGCBinaryTree): TDGCBinaryTree;
begin
 if (SubTree.filsG = nil) then
   result := SubTree
 else
   result := PlusGrand(SubTree.filsD); //on descend à droite
end;
function TDGCBinaryTree.PlusPetit(SubTree: TDGCBinaryTree): TDGCBinaryTree;
begin
  if (SubTree.filsD = nil) then
    result := SubTree
  else
    result := PlusPetit( SubTree.filsG ) //on descend à gauche
end;
procedure TDGCBinaryTree.Remove(var SubTree: TDGCBinaryTree; const N: TNodeIndex);
var
  Loc, Node: TDGCBinaryTree;
begin
  if (SubTree <> nil) then
  begin
    if N < SubTree.FNodeIndex  then
      Remove (SubTree.filsG, N)
    else
    begin
      if (N > SubTree.FNodeIndex)  then
        Remove (SubTree.filsD, N)
      else // N = SubTree^.info
      begin
        if (SubTree.filsG = nil)  then
        begin
          Loc := SubTree;
          SubTree := SubTree.filsD;
          FreeAndNil(Loc);   // dispose(Loc)
        end
        else
        begin
          if SubTree.filsG = nil then
          begin
            Loc := SubTree;
            SubTree := SubTree.filsG;
            FreeAndNil(Loc);
          end
          else
          begin
            Node := PlusGrand ( SubTree.filsG );
            Loc := Node;
            SubTree.FNodeIndex  := Node.FNodeIndex;
            SubTree.filsG       := Node.filsG;
            FreeAndNil(Loc);
          end
        end;
      end;
    end;
  end;
end;


//******************************************************************************
{ TDGCHeap }

function TDGCHeap.Initialiser(): boolean;
begin
  FRoot := TDGCBinaryTree.Create;
  self.ClearListe();
end;

procedure TDGCHeap.Finaliser();
begin
  FreeAndNil(FRoot);
end;

end.

