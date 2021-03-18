// ------------------------------------
// Version 1.0 : Algorithmes de graphes
// ------------------------------------

// Copyright (C) 2003 (Lacomme, Prins, Sevaux)

// Ce programme (ainsi que la bibliothèque objets livrée avec le livre)
// est libre, vous pouvez le redistribuer et/ou
// le modifier selon les termes de la
// Licence Publique Générale GNU publiée par la
// Free Software Foundation (version 2 ou bien toute autre version
// ultérieure choisie par vous).
// Ce programme est distribué car potentiellement utile,
// mais SANS AUCUNE GARANTIE, ni explicite ni implicite,
// y compris les garanties de commercialisation ou
// d'adaptation dans un but spécifique. Reportez-vous à la
// Licence Publique Générale GNU pour plus de détails.
// Vous devez avoir reçu une copie de la Licence
// Publique Générale GNU en même temps que
// ce programme ; si ce n'est pas le cas, écrivez à la
// Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
// 02111-1307, États-Unis.

// Différents sites donnent des copies officielles ou non de cette licence :
// http://www.linux-france.org/article/these/gpl.html
// http://www.gnu.org/copyleft/gpl.html


unit U_BASE_GRAPHES;

interface

 uses U_TYPE_GRAPHES, Sysutils;

  type

  E_EXCEPTION_PILE_FILE = class (exception)
  end;
  
  E_EXCEPTION_HEAP = class (exception)
  end;

  E_EXCEPTION_HASHING = class (exception)
  end;

    // la structure de type PILE_FILE
  T_PILE_FILE = class (TObject)
  private
    Last: Node;
    Next: TNodeInfo;
    Num: Node;
  public
    constructor CREATE;
    destructor DESTROY; override;
    function CardOfSet: Node;
    procedure CLEAR;
    procedure DeQueue(var x:Node);
    procedure EnQueue(x:Node);
    function Front: Node;
    function InSet(x:Node): Boolean;
    procedure Pop(var x:Node);
    procedure Push(x:Node);
    function Rear: Node;
    function SetIsEmpty: Boolean;
  end;
  
    // la structure de type HEAP
  T_Heap = class (TObject)
  private
    Body: TNodeInfo;
    Num: Node;
    Where: TNodeInfo;

    Function LIRE_Body (i:Node):  Node;
    procedure ECRIRE_Body (i:Node; x :Node);
    Function LIRE_Where(i:Node):  Node;
    procedure ECRIRE_Where (i:Node; x :Node);

  public
    constructor CREATE;
    destructor DESTROY; override;
    procedure ClearHeap;
    procedure HeapInsert(var W:TNodeCost; x:Node);
    function HeapIsEmpty: Boolean;
    procedure HeapMin(var W:TNodeCost; var x:Node);
    function InHeap(x: Node): Boolean;
    procedure MoveDown(var W:TNodeCost; x:Node);
    procedure MoveUp(var W:TNodeCost; x:Node);

    property p_num : Node read Num write Num;
    property p_Body[i:Node]:Node read LIRE_Body write ECRIRE_Body;
    property p_Where[i:Node]:Node read LIRE_Where write ECRIRE_where;

    property Pt_body :TNodeInfo read Body write Body;
    property Pt_where :TNodeInfo read where write where;

  end;

    // table de hashing
  T_HashTable = class (TObject)
  private
    XNode : PTSucc;          {Origine de l'arc}
    YNode : PTSucc;          {Extremite de l'arc}
    ArcPos: PTInverse;       {Position arc dans le graphe}
    Next  : PTInverse;       {Chainage collisions}
    Lowest: ArcNum;          {Indice 1ere cellule utilisee}
    WWork : PTArcCost;       {Tableau de couts}

    Function Hash (x,y:Node): ArcNum;

    // methodes associees aux properties
    Function LIRE_XNode (i:ArcNum):  Node;
    procedure ECRIRE_XNode (i:ArcNum; x :node);
    Function LIRE_YNode (i:ArcNum):  Node;
    procedure ECRIRE_YNode (i:ArcNum; x :node);
    Function LIRE_ArcPos (i:ArcNum):  ArcNum;
    procedure ECRIRE_ArcPos (i:ArcNum; x :ArcNum);
    Function LIRE_WWork (i:ArcNum):  Cost;
    procedure ECRIRE_WWork (i:ArcNum; x :Cost);

  public
    constructor CREATE;
    destructor DESTROY; override;

    Procedure ClearHashTable;
    Procedure HashCreate (p:ArcNum; x,y:Node; k:ArcNum);
    Function HashFound (x,y:Node; p:ArcNum): Boolean;
    Procedure HashSearch (x,y:Node; var p:ArcNum);
    function ACCEDER_WWork:TArcCost;

    property p_XNode[i : ArcNum]  : Node read LIRE_XNode write ECRIRE_XNode;
    property p_YNode[i : ArcNum]  : Node read LIRE_YNode write ECRIRE_YNode;
    property p_ArcPos[i : ArcNum] : ArcNum read LIRE_ArcPos write ECRIRE_ArcPos;
    property p_WWork[i : ArcNum]  : Cost read LIRE_WWork write ECRIRE_WWork;

  end;

  // gestion des buckets
  T_BuckSpace = class (TObject)
    private
      First: Array[BuckNo] of Node;
      Next : TNodeInfo;
      Prev : TNodeInfo;

      function LIRE_First(i:BuckNo):Node;
      procedure ECRIRE_First (i:BuckNo; x:node);




    public
      constructor CREATE;
      destructor DESTROY; override;

      Procedure PopFrom (b:BuckNo; var x:Node);
      Procedure PushInto (b:BuckNo; x:Node);
      Procedure RemoveFrom (b:BuckNo; x:Node);

      property p_First[i : BuckNo]: Node read LIRE_First write ECRIRE_First;

    End;
  PBuckSpace = ^T_BuckSpace;




implementation



//************************************ T_Heap **********************************

constructor T_Heap.CREATE;
begin
  inherited CREATE;
  Num := 0;
  FillChar (Where,SizeOf(Where),Chr(0));
end;

destructor T_Heap.DESTROY;
begin
 inherited destroy;
end;




procedure T_Heap.ClearHeap;
begin
  Num := 0;
  FillChar (Where,SizeOf(Where),Chr(0));
end;

procedure T_Heap.HeapInsert(var W:TNodeCost; x:Node);
begin
  If Where[x] = 0 then
  begin
     Inc (Num);
     Body[Num] := x;
     Where[x]  := Num;
     MoveUp (W,x)
  end;
end;

function T_Heap.HeapIsEmpty: Boolean;
begin
  HeapIsEmpty := (Num = 0);
end;

procedure T_Heap.HeapMin(var W:TNodeCost; var x:Node);
begin
  If Num = 0 Then
    raise E_EXCEPTION_HEAP.Create('Unite: U_BASE_GRAPHES, '
                                  + 'Methode : HeapMin, Message : tas vide')
  Else
    begin
     x        := Body[1];
     Where[x] := 0;
     Dec (Num);
     If Num > 0 then
     begin
        Body[1]        := Body[Num+1];
        Where[Body[1]] := 1;
        MoveDown (W,Body[1])
     End
    End;
end;

function T_Heap.InHeap(x: Node): Boolean;
begin
  InHeap := (Where[x] > 0);
end;

procedure T_Heap.MoveDown(var W:TNodeCost; x:Node);
var
  i,j: Integer;
begin
  i := Where[X];
  j := i shl 1;
  If (j < Num) and (W[Body[j+1]] < W[Body[j]]) then
      Inc(j);
  While (j <= Num) and (W[Body[j]] < W[X]) do
    begin
     Body[i]        := Body[j];
     Where[Body[j]] := i;
     i              := j;
     j              := i shl 1;
     If (j < Num) and (W[Body[j+1]] < W[Body[j]]) then
      Inc(j)
    End;
  Body[i]  := X;
  Where[X] := i;
end;

procedure T_Heap.MoveUp(var W:TNodeCost; x:Node);
var
  i,j: Integer;
begin
  i := Where[X];
  j := i shr 1;
  While (i > 1) and (W[Body[j]] > W[X]) do
  begin
   Body[i]        := Body[j];
   Where[Body[j]] := i;
   i              := j;
   j              := i shr 1;
  End;
  Body[i]  := X;
  Where[X] := i;
end;


//********************************* T_PILE_FILE ********************************

     constructor T_PILE_FILE.CREATE;
     begin
       inherited create;
       Num := 0;
       FillChar (Next,SizeOf(Next),#0);
     end;

     destructor T_PILE_FILE.destroy;
     begin
       inherited Destroy;
      end;

     procedure T_PILE_FILE.CLEAR;
     begin
       Num := 0;
       FillChar (Next,SizeOf(Next),#0);
     end;
  
     function T_PILE_FILE.SetIsEmpty;
     begin
       SetIsEmpty := (Num = 0);
     End;
  
  
     Function T_PILE_FILE.InSet (x:Node): Boolean;
     Begin
       InSet := (Next[x] > 0);
     End;

  
     function T_PILE_FILE.CardOfSet: Node;
     Begin
       CardOfSet := Num;
     End;
  
  
     function T_PILE_FILE.Front: Node;
     Begin
        If Num = 0 then
           raise E_EXCEPTION_PILE_FILE.Create('Unite: U_BASE_GRAPHES,'
                                  +' Methode : Front, Message : ensemble vide');
        Front := Next[Last];
     End;


     function T_PILE_FILE.Rear : Node;
     Begin
      If Num = 0 then
          raise E_EXCEPTION_PILE_FILE.Create('Unite: U_BASE_GRAPHES,'
                                  +'  Methode : Rear, Message : ensemble vide');
       Rear := Last;
     End;
  
  
     Procedure T_PILE_FILE.EnQueue (x:Node);
     Begin
      If Next[x] = 0 then
       begin
       If Num = 0 Then
          Next[x] := x
       Else
         begin
          Next[x]    := Next[Last];
          Next[Last] := x
         end;
       Last := x;
       Inc (Num);
       end;
     end;
  
     Procedure T_PILE_FILE.Push (x:Node);
     Begin
       If Next[x] = 0 then
       begin
        If Num = 0 then
        begin
           Next[x] := x;
           Last    := x;
        End
        Else
        begin
           Next[x]    := Next[Last];
           Next[Last] := x;
        End;
        Inc (Num);
       End;
     end;
  
     procedure T_PILE_FILE.DeQueue (var x:Node);
     Begin
      If Num = 0 then
         raise E_EXCEPTION_PILE_FILE.Create('Unite: U_BASE_GRAPHES,'
                                 +'Methode : DeQueue, Message : ensemble vide');
      x          := Next[Last];
      Next[Last] := Next[x];
      Next[x]    := 0;
      Dec (Num);
     End;
  
    Procedure T_PILE_FILE.Pop (var x:Node);
    Begin
     DeQueue (x);
    End;
  

//********************************* T_HashTable ********************************


    constructor T_HashTable.CREATE;
    begin
      inherited create;
      New (XNode);
      New (YNode);
      New (ArcPos);
      New (Next);
      New (WWork);
      Lowest := MaxArcNum+1;
      FillChar (XNode^,SizeOf(TSucc),#0);
      FillChar (Next^,SizeOf(TInverse),#0);
    end;

    destructor T_HashTable.DESTROY;
    begin
      inherited Destroy;
      dispose (XNode);
      dispose (YNode);
      dispose (ArcPos);
      dispose (Next);
      dispose (WWork);
    end;

    Function T_HashTable.LIRE_XNode (i:ArcNum):  Node;
    begin
     LIRE_XNode := XNode^[i];
    end;

    procedure T_HashTable.ECRIRE_XNode (i:ArcNum; x :node);
    begin
      XNode^[i]:=x;
    end;

    Function T_HashTable.LIRE_YNode (i:ArcNum):  Node;
    begin
     LIRE_YNode := YNode^[i];
    end;

    procedure T_HashTable.ECRIRE_YNode (i:ArcNum; x :node);
    begin
      YNode^[i]:=x;
    end;

    Function T_HashTable.LIRE_ArcPos (i:ArcNum):  ArcNum;
    begin
     LIRE_ArcPos := ArcPos^[i];
    end;

    procedure T_HashTable.ECRIRE_ArcPos (i:ArcNum; x :ArcNum);
    begin
      ArcPos^[i]:=x;
    end;

    Function T_HashTable.LIRE_WWork (i:ArcNum):  Cost;
    begin
      LIRE_WWork := WWork^[i];
    end;

    procedure T_HashTable.ECRIRE_WWork (i:ArcNum; x :Cost);
    begin
      WWork^[i]:=x;
    end;

    function T_HashTable.ACCEDER_WWork:TArcCost;
    begin
      ACCEDER_WWork := WWork^;
    end;


    Function T_HashTable.Hash (x,y:Node): ArcNum;
    begin
     RandSeed := x;
     Hash := LongInt(Random(MaxArcNum))*y mod MaxArcNum;
    end;

    Procedure T_HashTable.ClearHashTable;
    begin
      Lowest := MaxArcNum+1;
      FillChar (XNode^,SizeOf(TSucc),#0);
      FillChar (Next^,SizeOf(TInverse),#0);
    end;

    Procedure T_HashTable.HashCreate (p:ArcNum; x,y:Node; k:ArcNum);
    begin
      If XNode^[p] > 0 then
      begin {Collision: on cherche un trou}
         Repeat
            Dec (Lowest)
         Until (Lowest = 0) or (XNode^[Lowest] = 0);
         If Lowest=0 then
           raise E_EXCEPTION_HASHING.Create
               ('Unite : U_BASE_GRAPHES, Methode : HashCreate,'
                                  +'Message : debordement de table de hashing');
         Next^[p] := Lowest;
         p := Lowest
      End;
      {Charge la cellule avec l'arc (x,y)}
      XNode^[p]  := x;
      YNode^[p]  := y;
      ArcPos^[p] := k
    end;

    Function T_HashTable.HashFound (x,y:Node; p:ArcNum): Boolean;
    begin
      HashFound := (XNode^[p] = x) and (YNode^[p] = y);
    end;

    Procedure T_HashTable.HashSearch (x,y:Node; var p:ArcNum);
    begin
      p := Hash (x,y);
      If XNode^[p] > 0 then
      begin
        While ((x <> XNode^[p]) or (y <> YNode^[p])) and (Next^[p] > 0) Do
           p := Next^[p]
      End
    end;


//************************************ T_BuckSpace *****************************



      constructor T_BuckSpace.CREATE;
      begin
       inherited CREATE;
       FillChar (First,SizeOf(First),Chr(0));
      end;

      destructor T_BuckSpace.DESTROY;
      begin
        inherited destroy;
      end;

      function T_BuckSpace.LIRE_First(i:BuckNo):Node;
      begin
        LIRE_First := First[i];
      end;

      procedure T_BuckSpace.ECRIRE_First (i:BuckNo; x:node);
      begin
       First [i] := x;
      end;

      Procedure T_BuckSpace.PopFrom (b:BuckNo; var x:Node);
      begin
       x := First[b];
       If Next[x] = x Then
          First[b] := 0
       Else
         begin
         Next[Prev[x]] := Next[x];
         Prev[Next[x]] := Prev[x];
         First[b]      := Next[x];
         End
      end;



      Procedure T_BuckSpace.PushInto (b:BuckNo; x:Node);
      begin
        If First[b] = 0 then
          begin
          Next[x] := x;
          Prev[x] := x;
          End
        Else
          begin
          Next[x] := First[b];
          Prev[x] := Prev[First[b]];
          Next[Prev[x]] := x;
          Prev[Next[x]] := x;
          End;
        First[b] := x;
      end;

      Procedure T_BuckSpace.RemoveFrom (b:BuckNo; x:Node);
      begin
       If Next[x] = x Then
         First[b] := 0
       Else
         begin
         Next[Prev[x]] := Next[x];
         Prev[Next[x]] := Prev[x];
         If x = First[b] then
            First[b] := Next[x]
         End
      end;


    Function T_HEAP.LIRE_Body (i:Node):  Node;
    begin
      LIRE_Body := Body[i];
    end;

    procedure T_HEAP.ECRIRE_Body (i:Node; x :Node);
    begin
      Body[i]:=x;
    end;


    Function T_HEAP.LIRE_Where (i:Node):  Node;
    begin
      LIRE_Where := Where[i];
    end;

    procedure T_HEAP.ECRIRE_Where (i:Node; x :Node);
    begin
      Where[i]:=x;
    end;

  end.


