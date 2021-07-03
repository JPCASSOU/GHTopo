{$ERROR Inutilisé dans GHTopo}
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



//----------------//
//--- U_GRAPHES --//
//----------------//


unit U_GRAPHES;

interface

uses U_TYPE_GRAPHES, U_BASE_GRAPHES, SysUtils, StdCtrls, DateUtils;


type

  E_EXCEPTION_U_GRAPHES = class (exception)
  end;

  PTR_T_GRAPHE=^T_GRAPHE;
  PTR_T_GRAPHE_LISTE=^T_GRAPHE_LISTE;
  PTR_T_GRAPHE_MATRICIEL=^T_GRAPHE_MATRICIEL;

  T_GRAPHE=class;
  T_GRAPHE_LISTE=class;
  T_GRAPHE_MATRICIEL=class;

  T_GRAPHE = class (TObject)
  private
    NX: NODE;
    NY: NODE;
    Simple: Boolean;

    //-- utilise pour les E/S de graphes dans des fichiers texte
    WFile      : Text;        {Fichier-texte pour lecture de graphes}
    WLine      : String;      {Ligne actuelle de ce fichier}
    WLineNb    : Integer;     {Numero relatif de cette ligne}
    LNE        : String[11];  {Ce numéro, edite sous forme de chaine}

    Function EdInt (i:LongInt):String;
    Function MaxCostLen (W:PTArcCost; M:ArcNum): Byte;
    Procedure CountLine (var Line:Integer; Break:Integer);
    Procedure Normalize (var S:String);
    Function StringOf (k:Byte; Cha:Char): String;
    Function Max (i,j:LongInt): LongInt;
    Function Min (i,j:LongInt): LongInt;
    Procedure LoadW (var W:TPTArcCost; W1,W2,W3:PTArcCost; var Last:CostNb);


    // PrepareFile: prepare le fichier pour la lecture
    Procedure PrepareFile (FileName,DefaultExtension:String);
    // SeekDataLine: lit prochaine ligne significative d'un fichier-graphe
    Procedure SeekDataLine;
    // ReadInt: extrait un entier de S, position k. Met a jour k.
    Function ReadInt (var k:Byte; Min,Max:LongInt): LongInt;
    // ReadHeader: lit la 1ere ligne significative d'un fichier-graphe
    Procedure ReadHeader (var NX,NY:Node; var Simple:Boolean;
                          var NCosts:CostNb; var NoArc:Cost);

    Function GraphLoops : Node; virtual; abstract;

    Function LIRE_NX :  Node;
    procedure ECRIRE_NX (x : Node);
    Function LIRE_NY :  Node;
    procedure ECRIRE_NY (x : Node);
    Function LIRE_Simple : Boolean;
    procedure ECRIRE_Simple (x :Boolean);


  public
    constructor CREATE;
    destructor Destroy;  override;

    Procedure Pause (Msg:String);

    property p_NX:NODE  read LIRE_NX write ECRIRE_NX;
    property p_NY:NODE  read LIRE_NY write ECRIRE_NY;
    property p_Simple : boolean read LIRE_Simple write ECRIRE_Simple;

    Function GraphOrder : Node;
    Procedure Error (Msg:String);

    function Chrono:TDateTime;

    Procedure GetPath (s,t:Node; var Father,Path:TNodeInfo; var Last:Node);
    Procedure Compare (A,B:TNodeCost; N:Node);

  end;

  T_GRAPHE_MATRICIEL = class (T_GRAPHE)
  private
    A: CostMatrix;
    NoArc: Cost;

    // LastCol: calcule derniere colonne d'un graphe matriciel
    Function LastCol : Node;
    // MatrixArcs: calcule le nombre d'arcs d'un graphe matriciel
    Function MatrixArcs : ArcNum;
    // GraphLoops : compte le nombre de boucles d'un graphe
    Function GraphLoops : Node; override;
    // MatrixIsSimple: teste si un graphe matriciel C est simple, en O(N2)
    Function MatrixIsSimple : Boolean;
    // MakeMatrixSimple: rend simple un graphe matriciel en O(N2)
    Procedure MakeMatrixSimple;

    Function LIRE_A (i:Node; j:Node):  Cost;
    procedure ECRIRE_A (i:Node; j:Node; x :Cost);
    Function LIRE_NoArc :  Cost;
    procedure ECRIRE_NoArc (x :Cost);


  public
    constructor CREATE;
    destructor Destroy;  override;

    Procedure RandMatrix (NX,NY:Node; Simple:Boolean;
                          LoopLess,Layered:Boolean; CMin,CMax,NoArc:Cost;
                          GenProb:Real; RSeed:LongInt; Reset:Boolean);

    // ReadMatrix: lit un graphe matriciel sur un fichier-texte de nom FileName
    Procedure ReadMatrix (FileName:String);
    // WriteMatrix: ecrit un graphe matriciel sur un fichier-texte deja ouvert
    Procedure AFFMatrix (MON_MEMO : TMemo; Msg:String;
                           Width:Byte; Break:Integer);
    Procedure WriteMatrix (var F:Text; Msg:String;
                           Width:Byte; Break:Integer);


    // Pack: convertit un graphe matriciel en graphe-liste, en O(N2)
    function Pack (W:PTArcCost): PTR_T_GRAPHE_LISTE;


    Procedure Assignment (var Card:Node; var K:Cost;
                          var Mate:TNodeInfo);
    Procedure NearestNeib (var Cycle:TNodeInfo; var C:Cost);
    Procedure NearestIns  (var Cycle:TNodeInfo; var C:Cost);
    Procedure FarthestIns (var Cycle:TNodeInfo; var C:Cost);
    Procedure BestIns     (var Cycle:TNodeInfo; var C:Cost);
    Procedure TwoOpt      (var Cycle:TNodeInfo; var C:Cost);
    Procedure SimAn       (T0,Coef,Eps:Real; NPass:Integer;
                       RSeed:LongInt;  Reset:Boolean; var Cycle:TNodeInfo;
                       var C:Cost;     var NIter:LongInt);
    Procedure Tabu        (NT,NItMax:Integer;
                       NItWI:Integer;  var Cycle:TNodeInfo;
                       var C:Cost;     var NIter:LongInt);
    property p_A[i : Node; j:Node]  : Cost read LIRE_A write ECRIRE_A;
    property p_NoArc  : Cost read LIRE_NoArc write ECRIRE_NoArc;

  end;

  T_GRAPHE_LISTE = class (T_GRAPHE)
  private
    HEAD: THEAD;
    SUCC: TSUCC;
//    W: PTArcCost;        suppression PL le 25/09/2003
    M: ARCNUM;

    // GetInDegrees : rend un tableau des demi-degres interieur
    Procedure GetInDegrees (var InDeg:TNodeInfo);

    // GraphLoops : compte le nombre de boucles d'un graphe
    Function GraphLoops : Node; override;
    // MaxOutDeg: calcule le plus grand demi-degre exterieur d'un graphe-liste
    Function MaxOutDeg : Node;
    // OutDeg: renvoie le demi-degre exterieur d'un sommet de graphe-liste
    Function OutDeg (x:Node): Node;
    // MakeGraphSimple: rend simple un graphe, en O(MaxArcNum) en moyenne
    Procedure  MakeGraphSimple (W1,W2,W3:PTArcCost);
    // ReadAdjList: lit un noeud et sa liste de successeurs
    Procedure ReadAdjList (W:TPTArcCost; NCosts,NKept:CostNb;
                           var LastHead:Node);
    // GraphIsSimple: teste si un graphe-liste G est simple, en O(MaxArcNum)
    Function GraphIsSimple (W1,W2,W3:PTArcCost): Boolean;

//    Procedure AddSuc (y:Node;
//                      Capa,Price:Cost;
 //                     var C : TArcCost);

    Procedure MakeNetwork
                      (var Mat:T_GRAPHE_MATRICIEL;
                       var C,W:TArcCost; var s,t:Node);

    Function LIRE_Head (i:Node):  ArcNum;
    procedure ECRIRE_Head (i:Node; x :ArcNum);
    Function LIRE_Succ (i:ArcNum):  Node;
    procedure ECRIRE_Succ (i:ArcNum; x :Node);
    Function LIRE_M : ArcNum;
    procedure ECRIRE_M (x :ArcNum);

    // Augment: augmente le flot sur une chaine ameliorante

    Procedure Augment (s,t:Node; var Phi:TArcCost;
                   var AugVal:TNodeCost;  var ArcTo: THead;
                   var Father:TNodeInfo);

    Procedure LFOrder (var V:TNodeInfo);
    Procedure SLOrder (var V,Di:TNodeInfo);


  public
    constructor CREATE;
    destructor DESTROY; override;

    property p_M : ArcNum read LIRE_M write ECRIRE_M;
    property p_HEAD[i : Node]  : ArcNum read LIRE_Head write ECRIRE_Head;
    property p_SUCC[i : ArcNum] : Node read LIRE_Succ write ECRIRE_Succ;


    // RandGraph: genere un graphe-liste aleatoire
    Procedure RandGraph (NX,NY:Node;
                         Simple:Boolean;
                         LoopLess,Layered:Boolean;
                         W:PTArcCost;
                         CMin,CMax:Cost;
                         GenProb:Real;
                         RSeed:LongInt;
                         Reset:Boolean);


    // ReadGraph: lit un graphe-liste G sur un fichier-texte de nom FileName
    Procedure ReadGraph (FileName:String; W1,W2,W3:PTArcCost);

    // WriteGraph: edit a forward-star graph (file already open)
    Procedure AFFGraph (MON_MEMO : Tmemo; W1,W2,W3:PTArcCost;
                          Inv:PTInverse; Msg:String; Width:Byte; Break:Integer);

    Procedure  WriteGraph  (var F:Text; W1,W2,W3:PTArcCost; Inv:PTInverse;
                            Msg:String; Width:Byte; Break:Integer);


    // UnPack: convertit un graphe-liste en graphe matriciel, en O(N2)
    function UnPack (NoArc:Cost;
                     W:PTArcCost): PTR_T_GRAPHE_MATRICIEL;

    // function LIRE_COUTS:PTArcCost;
       // modification  de PL le 25/09/2003


    // AllCC: calcul des composantes connexes de G
    Procedure AllCC (var NCC:Node; var CC:TNodeInfo);
    // AllSCC: calcul des composantes fortement connexes (TARJAN)
    Procedure AllSCC (var NSCC:Node; var SCC:TNodeInfo);
    // BFS: exploration en profondeur a partir d'un sommet s
    Procedure BFS (s:Node; var BFN,Father:TNodeInfo);
    // Bipartite: teste si un graphe (connexe ou pas) est biparti
    Procedure Bipartite (var Bip:Boolean; var Color:TNodeInfo);
    //BuildPreds: calcul du graphe inverse H d'un graphe G
    Procedure BuildPreds (var H:T_GRAPHE_LISTE; Inv:PTInverse);
    // DFS: exploration en profondeur a partir d'un sommet s
    Procedure DFS (s:Node; var DFN,Father:TNodeInfo);
    // GetCircuit d‚compose un graphe (connexe ou non) en niveaux
    Procedure GetCircuit (var Circuit:TNodeInfo; var Size:Node);
    // GetLayers: d‚compose un graphe (connexe ou non) en niveaux
    Procedure GetLayers (var NLayer:node;
                     var Layer, Sorted:TNodeInfo);

    Procedure Bellman  (var W:TArcCost;  s:Node;
                        var V:TNodeCost; var P:TNodeInfo; var NegCirc:Boolean);

    Procedure Dijkstra (var W:TArcCost; s,t:Node;
                        var V:TNodeCost; var P:TNodeInfo);

    Procedure Floyd (var W:TArcCost;
                     var V:CostMatrix;
                     var P:NodeMatrix;
                     var NegCirc:Boolean);

    Procedure Schedule (var W:TArcCost;   Alpha,Omega:Node;
                        var V:TNodeCost;  var P:TNodeInfo);

    Procedure DijHeap (var W:TArcCost; s,t:Node;
                       var V:TNodeCost; var P:TNodeInfo);

    Procedure Bucket   (var W:TArcCost; s,t:Node;
                        var V:TNodeCost; var P:TNodeInfo);

    Procedure ESOPO (var W:TArcCost; s:Node;
                     var V:TNodeCost; var P:TNodeInfo);

    Procedure FIFO (var W:TArcCost; s:Node;
                    var V:TNodeCost; var P:TNodeInfo);


   // Busacker: algorithme de flot de cout minimal
   Procedure Busacker  (var C,W:TArcCost; s,t: node; ReqF:Cost;
                        var F,K:Cost; var PHI:TArcCost);

   // Ahuja: algorithme des distances estimees au puits
   Procedure Ahuja (var C:TArcCost;    s,t: node;
                    var F:Cost;  var PHI:TArcCost);

   // CheckFlow: verifie un flot donne
   Procedure CheckFlow (var C:TArcCost;    s,t:node;
                        F:Cost;  var PHI:TArcCost);

   // Fulkerson: algorithme de flot maximal
   Procedure Fulkerson (var C:TArcCost;    s,t: node;
                     var F:Cost;  var PHI:TArcCost);

   //  algo de flot maximal avec scaling des capacites
   Procedure Scaling (var C:TArcCost;    s,t: node;
                   var F:Cost;  var PHI:TArcCost);

   // Couplage maximal dans un graphe biparti
   Procedure BipMatch   (var Card:Node; var Mate:TNodeInfo);

   Procedure KRUSKAL (var W:TArcCost; var Weight:Cost;
                      var NEdge:Node;  var Node1,Node2:TNodeInfo);

   Procedure EDMONDS (var W:TArcCost; s:Node;
                      var Weight:Cost; var Pred:TNodeInfo);

   Procedure PRIM (var W:TArcCost; var Weight:Cost;
                   var NEdge:Node; var Node1,Node2:TNodeInfo);

   Procedure EulerChain  (var Found:Boolean;
                          var Walk:TSucc; var Last:ArcNum);

   Procedure EulerPath   (var Found:Boolean;
                          var Walk:TSucc; var Last:ArcNum);

   Procedure Postman     (var W:TArcCost;
                          var Walk:TSucc; var Last:ArcNum; var K:Cost);

   Procedure Backtrack (MaxDown:LongInt;
                     var Color:TNodeInfo; var NC:Node; var NDown:LongInt);

   Function  BS1: Node;
   Function  BS2: Node;
   Function  BS3: Node;

   Procedure Check (Color:TNodeInfo; NC:Node);
   Procedure DSatur (var Color:TNodeInfo; var NC:Node);
   Procedure FFS ( var Color:TNodeInfo; var NC:Node);
   Procedure LFS (var Color:TNodeInfo; var NC:Node);
   Procedure SeqColor (var V,Color:TNodeInfo; var NC:Node);
   Procedure Local (var Color:TNodeInfo; var NC:Node);
   Procedure SimAn (memo1 : Tmemo;T0,Coef,Eps:Real; NPass:Integer;
                     RSeed:LongInt; Reset:Boolean;
                     var Color:TNodeInfo; var NC:Node; var NIter:LongInt);
   Procedure SLS (var Color:TNodeInfo; var NC:Node);
   Procedure TabuCol (NT,NItMax:Integer;
                   var Color:TNodeInfo; var NC:Node; var NIter:LongInt);
  end;

implementation

 uses Dialogs, StrUtils;


//******************************** T_GRAPHE_LISTE ******************************

constructor T_GRAPHE_LISTE.CREATE;
begin
  inherited CREATE;
//  new (Self.W);  suppression PL le 25/09/2003
end;

destructor T_GRAPHE_LISTE.DESTROY;
begin
  inherited DESTROY;
//  dispose(Self.w);   suppression PL le 25/09/2003
end;


//****************************** T_GRAPHE_MATRICIEL ****************************

constructor T_GRAPHE_MATRICIEL.CREATE;
begin
  inherited CREATE ;
end;

destructor T_GRAPHE_MATRICIEL.Destroy;
begin
  inherited DESTROY;
end;


//*********************************** T_GRAPHE ***********************************

constructor T_GRAPHE.CREATE;
begin
  inherited CREATE;
end;

destructor T_GRAPHE.Destroy;
begin
  inherited DESTROY;
end;

// ******************** Méthodes de base d'E/S *********************

Procedure T_GRAPHE.Error (Msg:String);
Begin
   raise E_EXCEPTION_U_GRAPHES.Create
      ('Unite: U_GRAPHES, Message : '+Msg)
End;

Procedure T_GRAPHE.pause (Msg:String);
Begin
   showmessage(msg);
End;

Function T_GRAPHE.EdInt (i:LongInt):String;
Begin
   EdInt:=IntToStr (i);
End;

Function T_GRAPHE.MaxCostLen (W:PTArcCost; M:ArcNum): Byte;
Var k        : ArcNum;
    MaxDigits: Byte;
    S        : String[11];
Begin
   MaxDigits := 0;
   For k := 1 to M do begin
      Str (W^[k],S);
      MaxDigits := Max (Length(S),MaxDigits)
   End;
   MaxCostLen := MaxDigits
End;

Procedure T_GRAPHE.CountLine (var Line:Integer; Break:Integer);
Begin
   If (Line >= Break) and (Break > 0) then begin
      ShowMessage('Cliquer pour continuer');
      Line := 1
   End
   Else Inc(Line)
End;

Procedure T_GRAPHE.Normalize (var S:String);
Var i: Byte;
Begin
 i := 1;
 While (i <= Length(S)) do
   If S[i] = ' ' Then
    Delete (S,i,1)
   Else
    begin
      S[i] := Upcase(S[i]);
      Inc (i);
    End
End;

Function T_GRAPHE.StringOf (k:Byte; Cha:Char): String;
Var S: String[255];
Begin
   SetLength(S,k);
   FillChar (S[1],k,Cha);
   StringOf := S;
End;

Function T_GRAPHE.Max (i,j:LongInt): LongInt;
Begin
 If i > j then
   Max := i
 else
   Max := j;
End;

Function T_GRAPHE.Min (i,j:LongInt): LongInt;
Begin
 If i < j then
   Min := i
 else
   Min := j;
End;

Procedure T_GRAPHE.LoadW (var W:TPTArcCost; W1,W2,W3:PTArcCost;
                          var Last:CostNb);
Var c: CostNb;
Begin
   W[1]  := W1; W[2] := W2; W[3] := W3;
   Last  := 0;
   For c := 1 to MaxNbCosts do If W[c] <> Nil then Last := c
End;

function T_GRAPHE.Chrono:TDateTime;
begin
  Chrono:=Now;
end;


// ******************** Les properties *********************
Function T_GRAPHE.LIRE_NX :  Node;
begin
  LIRE_NX := NX;
end;

procedure T_GRAPHE.ECRIRE_NX (x : Node);
begin
  NX := X;
end;

Function T_GRAPHE.LIRE_NY :  Node;
begin
  LIRE_NY := NY;
end;

procedure T_GRAPHE.ECRIRE_NY (x : Node);
begin
  NY := x;
end;

Function T_GRAPHE.GraphOrder : Node;
Begin
  GraphOrder := NX+NY;
End;


Procedure T_GRAPHE.PrepareFile (FileName,DefaultExtension:String);
begin
   Normalize (FileName);
   If Pos ('.',FileName) = 0 then FileName := FileName + DefaultExtension;
   Assign (WFile,FileName);
   {$I-} Reset (WFile); {$I+}
   If (IOResult <> 0) or EOF(WFile)Then
      Error ('Lecture de graphe: fichier non trouve ou vide');
   WLine   := Chr(0);
   WLineNb := 0;
   LNE     := EdInt(WLineNb)
end;


Procedure T_GRAPHE.SeekDataLine;
Begin
   Repeat
      Inc (WLineNb);
      LNE := EdInt (WLineNb);
      ReadLn (WFile,WLine); {Lit une ligne du fichier}
      Normalize (WLine)     {Elimine les blancs,convertit en majuscules}
   Until EOF(WFile) or ((WLine > '') and (WLine[1] <> '*'));
   If Length(WLine)=255 then Error ('Lecture de graphe:ligne trop longue');
   WLine := WLine + Chr(0);
End;

Function T_GRAPHE.ReadInt (var k:Byte; Min,Max:LongInt): LongInt;
Var ErrCode: Integer;
    Number : LongInt;
    j      : Byte;
Begin
   If WLine[k] = Chr(0)
   Then Error ('Lecture de graphe: entier attendu ligne '+LNE);
   If not (WLine[k] in ['+','-','0'..'9'])
   Then Error ('Lecture de graphe: caractŠre ill‚gal ligne '+LNE);
   j := k;
   While WLine[k] in ['+','-','0'..'9'] do Inc (k);
   Val (Copy(WLine,j,k-j),Number,ErrCode);
   If (ErrCode <> 0) or (Number < Min) or (Number > Max)
   Then Error ('Lecture de graphe: nombre invalide ligne '+EdInt(WLineNb));
   If WLine[k] = ',' then Inc(k);
   ReadInt := Number;
End;



Procedure T_GRAPHE.ReadHeader (var NX,NY:Node; var Simple:Boolean;
                               var NCosts:CostNb; var NoArc:Cost);
Var k: Byte;
Begin
   NY     := 0;
   NCosts := 0;
   NoArc  := 0;
   k      := Pos ('NX=',WLine)+3;
   If k = 3 then Error ('Lecture de graphe: NX manquant');
   NX     := ReadInt (k,0,MaxNode);
   k      := Pos ('NY=',WLine)+3;
   If k > 3 then begin {NY donne: graphe biparti}
      NY := ReadInt (k,0,MaxNode);
      If (NX = 0) and (NY > 0) then Error ('Lecture de graphe: mauvais NY');
      If NX+NY > MaxNode then Error ('Lecture de graphe: NX+NY trop grand')
   End;
   Simple := (NY > 0) or (Pos('SIMPLE',WLine) > 0);
   k := Pos ('COSTS=',WLine)+6;
   If k > 6 then NCosts := ReadInt (k,0,MaxNbCosts);
   k := Pos ('NOARC=',WLine)+6;
   If k > 6 then NoArc := ReadInt (k,-MaxCost,+MaxCost)
End;

Procedure T_GRAPHE.GetPath (s,t:Node; var Father,Path:TNodeInfo; var Last:Node);
Var P: T_PILE_FILE;
Begin
   P:=T_PILE_FILE.CReate;

   If Father[s] <> s
   Then Error ('GetPath: la derniere BFS/DFS n''est pas partie de s');
   Last := 0;
   If Father[t] = 0 then Exit;
   P.Clear;
   While t <> s do begin
      P.Push (t);
      t := Father[t]
   End;
   P.Push (s);
   Repeat
      Inc (Last);
      P.Pop (Path[Last])
   Until P.SetIsEmpty;

   P.Destroy;
End;

Procedure T_GRAPHE.Compare (A,B:TNodeCost; N:Node);
Var x: Node;
Begin
   For x := 1 to N do begin
      If A[x] <> B[x] then begin
         Error ('Compare: A et B different a l''indice '+IntToStr(x)+Chr(7));
         Pause   ('Abandon. Click pour continuer...');
      End
   End
End;

Procedure T_GRAPHE_LISTE.GetInDegrees (var InDeg:TNodeInfo);
  Var x: Node;
      k: ArcNum;
Begin
 For x := 1 to NX+NY do
   InDeg[x] := 0;
 For k := 1 to M do
   Inc (Indeg[Succ[k]])
end;


Function T_GRAPHE_LISTE.GraphLoops : Node;
Var i,Loops: Node;
    k      : ArcNum;
Begin
  Loops := 0;
  For i := 1 to NX+NY do
  begin
   k := Head[i];
   While (k < Head[i+1]) and (i <> Succ[k]) do
      Inc (k);
   If k < Head[i+1] then
      Inc (Loops)
  End;
  GraphLoops := Loops
End;

Function T_GRAPHE_LISTE.MaxOutDeg : Node;
 Var i,MaxDeg:Node;
begin
 MaxDeg := 0;
 For i := 1 to NX+NY do
   MaxDeg := Max (MaxDeg,Head[i+1]-Head[i]);
 MaxOutDeg := MaxDeg;
end;

Function T_GRAPHE_LISTE.OutDeg (x:Node): Node;
begin
 OutDeg := Head[x+1]-Head[x];
end;


Procedure T_GRAPHE_LISTE.MakeGraphSimple (W1,W2,W3:PTArcCost);
Var x,y     : Node;
    k,p,q   : ArcNum;
    W       : TPTArcCost;
    LastW,c : CostNb;
    Deg     : TNodeInfo;
    SaveSeed: LongInt;
//    HT      : T_HashTable;
    PHT      : ^T_HashTable;
Begin
    new (PHT);
    PHT^:=T_HashTable.Create;

    {1. Hashe les arcs, teste les arcs multiples}
    LoadW (W,W1,W2,W3,LastW);               {Table de pointeurs sur couts}

    PHT^.ClearHashTable;                    {Initialise table de hash}
    SaveSeed := RandSeed;                   {Sauve germe du generateur}
    For x := 1 to NX+NY do                  {Hache les arcs de G dans HT}
      For k := Head[x] to Head[x+1]-1 do If x <> Succ[k] then begin
         y := Succ[k];                        {NB: on ignore les boucles}
         PHT^.HashSearch (x,y,p);               {Cherche (x,y)}
         If PHT^.HashFound (x,y,p)              {Si trouv‚: p-graphe!}
         Then Error ('MakeGraphSimple: arcs multiples detectes');
         PHT^.HashCreate (p,x,y,k)              {Stocke (x,y)}
      End;
      {2. Sym‚trise les arcs et teste la symetrie des cots}
      For p := 1 to MaxArcNum do If PHT^.p_XNode[p] > 0 then begin
          x := PHT^.p_XNode[p];                     {Recupere arc cellule p}
          y := PHT^.p_YNode[p];
          k := PHT^.p_ArcPos[p];
          PHT^.HashSearch (y,x,q);              {Cherche arc (y,x)}
          If PHT^.HashFound (y,x,q) then begin  {Existe, verifie symetrie couts}
             For c := 1 to LastW do
             If (W[c] <> Nil) and (W[c]^[k] <> W[c]^[PHT^.p_ArcPos[q]])
             Then Error ('MakeGraphSimple: asymetrie sur ('
                         +EdInt(x)+','+EdInt(y)+')')
          End
          Else PHT^.HashCreate (q,y,x,k)        {N'existe pas, on le cree}
      End;
      {3. Calcule degres et M. Apres 2, car des arcs ont ete crees}
      FillChar (Deg,SizeOf(Deg),#0);
      M := 0;
      For p := 1 to MaxArcNum do If PHT^.p_XNode[p] > 0 then begin
         Inc (Deg[PHT^.p_XNode[p]]);
         Inc (M)
      End;
      {4. RŠgle Head apres le dernier predecesseur, et ferme les listes}
      Head[0] := 1;
      For x := 1 to NX+NY do Head[x] := Head[x-1] + Deg[x];
      Head[NX+NY+1] := M+1;
      {5. Recharge G … partir de la table de hash}
      For p := 1 to MaxArcNum do If PHT^.p_XNode[p] > 0 then begin
         x := PHT^.p_XNode[p];
         y := PHT^.p_YNode[p];
         Dec (Head[x]);
         Succ [Head[x]] := y
      End;
      Simple := True;
      {6. Reconstruit les tableaux de couts}
      For c := 1 to LastW do If W[c] <> Nil then begin
         For x := 1 to NX+NY do For k := Head[x] to Head[x+1]-1 do begin
            PHT^.HashSearch (x,Succ[k],p);
            PHT^.p_WWork[k] := W[c]^[PHT^.p_ArcPos[p]]
         End;
         W[c]^ := PHT^.ACCEDER_WWork;
      End;
   RandSeed := SaveSeed;
   PHT^.Destroy;
   dispose(PHT);
End;



Procedure T_GRAPHE_LISTE.RandGraph (NX,NY:Node;
                                    Simple:Boolean;
                                    LoopLess,Layered:Boolean;
                                    W:PTArcCost;
                                    CMin,CMax:Cost;
                                    GenProb:Real;
                                    RSeed:LongInt;
                                    Reset:Boolean);
Var DeltaC    : Cost;
    i,j,p,MNS : Node;
    NSucGen,q : Node;
    k,NGen    : ArcNum;
    z         : Real;
    ANS       : Array[Node] of Real;
    Deg,SucGen: TNodeInfo;
Begin
   If (NY > 0) and not Simple
       Then Error ('RandGraph: graphe biparti mais oriente');
   If Layered and Simple
      Then Error ('RandGraph: graphe en couches mais non orient‚');
   If Layered and (not LoopLess)
      Then Error ('RandGraph: graphe en couches avec boucles permises');
   If Simple and (not LoopLess)
      Then Error ('RandGraph: graphe simple avec boucles permises');
   If (GenProb <= 0.0) or (GenProb > 1.0)
      Then Error ('RandGraph: probabilit‚ invalide');
   If CMin > CMax
      then Error ('RandGraph: intervalle [CMin,CMax] invalide');

   If Reset then Randomize else RandSeed := RSeed;
   Self.NX     := NX;
   Self.NY     := NY;
   Self.M      := 0;
   Self.Simple := Simple;
   DeltaC   := CMax-CMin+1;

   If NY = 0 then MNS := NX else MNS := NY; {Nb max de successeurs}

   If (NX+NY <= 50) or (GenProb * MNS > Sqrt(MNS)) then begin
         {Generation aleatoire haute densite sommet par sommet i}
         For i := 1 to NX do begin
            Head[i] := M+1;
            {Si G biparti, on cree avec une proba GenProb des arcs (i,j) }
            {de X dans Y (les noeuds de Y ont les nø NX+1 a NX+NY). Ceux }
            {symetriques Y vers X sont crees en fin avec MakeGraphSimple.}
            If NY > 0 then begin
               For j := NX+1 to NX+NY do If Random < GenProb then begin
                  Inc (M);
                  Succ[M] := j
               End
            End
            {Sinon,on cree avec une proba GenProb (i,j) avec i,j dans X. Si }
            {G sans boucle,on ne cree pas (i,i).Si G sans circuit, on cree  }
            {des (i,j) avec i < j. Si G simple, on cree d'abord les arcs    }
            {(i,j) avec i < j, les autres a la fin par MakeGraphSimple.     }
            Else For j := 1 to NX do
                 If ((i=j) and LoopLess) or ((i>=j) and (Simple or Layered))
                 Then {Ne g‚nŠre pas (i,j)!}
                 Else If Random < GenProb then begin
                    Inc (M);
                    Succ[M] := j
                 End
         End;
         For i := NX+1 to NX+NY+1 do Head[i] := M+1
      End
      Else begin {Generation aleatoire basse densite}
         {1. Construit table ANS des nombres moyens de succs, en cumul}
         ANS[0] := 0.0;
         For i := 1 to NX do begin
            If NY > 0
            Then ANS[i] := GenProb*NY
            Else If Layered or Simple then ANS[i] := GenProb*(NX-i)
            Else If LoopLess then ANS[i] := GenProb*(NX-1)
            Else ANS[i] := GenProb*NX;
            ANS[i] := ANS[i] + ANS[i-1];
         End;
         {2. Calcul nombre total d'arcs a generer, M}
         If NY > 0
         Then M := Round (GenProb*NX*NY)
         Else If Layered or Simple then M := Round (GenProb*NX*(NX-1)/2.0)
         Else If LoopLess then M := Round (GenProb*NX*(NX-1))
         Else M := Round (GenProb*NX*NX);
         {3. Calcul du degre de chaque sommet}
         FillChar (Deg,SizeOf(Deg),#0);
         For NGen := 1 to M do begin
            z := Random*ANS[NX];
            {Cherche noeud p tel que ANS[i-1] <= z < ANS[i]}
            i := 1;
            j := NX;
            Repeat
               p := (i+j) div 2;
               If z < ANS[p-1]
               Then j := p - 1
               Else If z >= ANS[p] then i := p+1
            Until (i > j) or ((ANS[p-1] <= z) and (z < ANS[p]));
            If i > j then Error ('RandGraph: recherche dichotomique');
            Inc (Deg[p])
         End;
         {4. G‚n‚ration}
         {Regle Head apres le dernier predecesseur, et ferme les listes}
         Head[0] := 1;
         For i := 1 to NX+NY do Head[i] := Head[i-1] + Deg[i];
         Head[NX+NY+1] := M+1;
         {Charge Succ}
         For i := 1 to NX do begin
            NSucGen := 0;
            For p := 1 to Deg[i] do begin
               {Tire successeur j parmi valeurs autorisees}
               Repeat
                  If NY > 0 then j := NX+1+Random(NY)
                  Else If Layered or Simple then j := 1+i+Random(NX-i)
                  Else If not LoopLess then j := 1+Random(NX)
                  Else begin
                     j := 1+Random(NX-1);
                     If j >= i then Inc(j)
                  End;
                  {Verifie que j n'est pas deja genere}
                  q := 1;
                  While (q <= NSucGen) and (SucGen[q] <> j) do Inc (q)
               Until q > NSucGen;
               Inc (NSucGen);
               SucGen[NSucGen] := j;
               Dec (Head[i]);
               Succ [Head[i]] := j
            End
         End;
      End;
      {Generation des couts dans [CMin,CMax] si un graphe value est demande}
      If W <> Nil then For k := 1 to M do W^[k] := Random(DeltaC)+CMin;
      {"Symetrisation" de G si un graphe simple est demande}
      If Simple then MakeGraphSimple (W,Nil,Nil)
End;

Procedure T_GRAPHE_LISTE.ReadAdjList (W:TPTArcCost; NCosts,NKept:CostNb;
                       var LastHead:Node);
Var x,y    : Node;
    TheCost: Cost;
    k,L    : Byte;
    c      : CostNb;
Begin
      L := Pos (':',WLine);
      If (L <= 1) and (LastHead = 0)
      Then Error ('ReadGraph: tete de liste manquante ligne '+LNE);
      {Pas une ligne-suite: recupere la tete de liste x}
      If L > 1 then begin
         k := 1;
         x := ReadInt (k,1,NX+NY);
         If x <= LastHead
         Then Error('ReadGraph: tete de liste non triee ligne '+LNE);
         {Met a vide les listes des tetes de liste manquantes}
         Repeat
            Inc (LastHead);
            Head[LastHead] := M+1
         Until LastHead = x
      End;
      {Recupere les successeurs de la ligne}
      k := L+1;
      While WLine[k] > #0 do begin
         {Recupere le successeur actuel y et le range dans G}
         y := ReadInt (k,1,NX+NY);
         If NY > 0 then
         If ((LastHead<=NX) and (y<=NX)) or ((LastHead>NX) and (y>NX))
         Then Error ('ReadGraph: graphe non biparti ligne '+LNE);
         Inc (M);
         Succ[M] := y;
         If (NCosts > 0) and (WLine[k] <> '(')
         Then Error ('ReadGraph: couts annonces non trouves ligne '+LNE);
         If (NCosts = 0) and (WLine[k] =  '(')
         Then Error ('ReadGraph: couts non annonces ligne '+LNE);
         {Recupere les couts associes a l'arc (x,y)}
         If NCosts > 0 then begin
            Inc (k);
            For c := 1 to NCosts do begin
               TheCost := ReadInt (k,-MaxCost,+MaxCost);
               If W[c] <> Nil then W[c]^[M] := TheCost
            End;
            If WLine[k]<>')' then Error ('ReadGraph: ")" attendu ligne '+LNE);
            Inc (k)
         End;
         If WLine[k] = ',' then Inc (k)
      End
End;


Function T_GRAPHE_LISTE.GraphIsSimple (W1,W2,W3:PTArcCost): Boolean;
Var x,y     : Node;
    k,p,NSym: ArcNum;
    W       : TPTArcCost;
    LastW,c : CostNb;
    SaveSeed: LongInt;
    HT : T_HashTable;
Begin
      HT:=T_HashTable.CREATE;

      GraphIsSimple := False;
      If Odd(M) then Exit;                   {M impair, donc G non simple!}
      LoadW (W,W1,W2,W3,LastW);              {Table de pointeurs sur couts}
      HT.ClearHashTable;                     {Initialise table de hash}
      SaveSeed := RandSeed;                  {Sauve germe du generateur}
      NSym := 0;
      {Hache les arcs de G en testant boucles et symetries,y compris couts}
      For x := 1 to NX+NY do For k := Head[x] to Head[x+1]-1 do begin
         y := Succ[k];
         If x = y then Exit;                 {Boucle: graphe non simple}
         HT.HashSearch (y,x,p);              {Cherche arc symetrique}
         If HT.HashFound (y,x,p) then begin  {Trouve, verifie symetrie couts}
            For c := 1 to LastW do
            If (W[c] <> Nil) and (W[c]^[k] <> W[c]^[HT.p_ArcPos[p]]) then Exit;
            Inc (Nsym)                       {Compte une symetrie}
         End;
         HT.HashSearch (x,y,p);              {Cherche (x,y)}
         If HT.HashFound (x,y,p)             {Si trouve: p-graphe!}
         Then Error ('GraphIsSimple: arcs multiples detectes');
         HT.HashCreate (p,x,y,k)             {Stocke (x,y)}
      End;
      RandSeed      := SaveSeed;             {Retablit le germe}
      GraphIsSimple := NSym = (M div 2);     {Simple si M/2 symetries}

      HT.Destroy;
End;





Procedure T_GRAPHE_LISTE.ReadGraph (FileName:String;W1,W2,W3:PTArcCost);
Var LastHead,i: Node;
    Dummy     : Cost;
    NCosts    : CostNb;
    W         : TPTArcCost;
    NKept     : CostNb;
Begin
      PrepareFile (FileName,'.GRA');          {Cherche et ouvre le fichier}
      SeekDataLine;                           {Cherche 1ere ligne donnees}
      ReadHeader (NX,NY,Simple,NCosts,Dummy); {Lit NX, NY, etc...}
      LoadW (W,W1,W2,W3,NKept);
      If NKept > NCosts
      Then Error ('ReadGraph: trop de couts demandes par rapport a NCOSTS');
      M        := 0;
      LastHead := 0;
      While not EOF(WFile) do begin
         SeekDataLine;
         If (WLine > '') and (WLine[1] <> '*')
         Then ReadAdjList (W,NCosts,NKept,LastHead)
      End;
      {Ferme les listes des sommets finaux sans successeurs}
      For i := LastHead+1 to NX+NY+1 do Head[i] := M+1;
      If (NY > 0) and (LastHead <= NX)
      Then MakeGraphSimple (W1,W2,W3)
      Else If Simple and not GraphIsSimple (W1,W2,W3)
           Then Error ('ReadGraph: graphe non simple');
      Close (WFile)
End;


Procedure T_GRAPHE_LISTE.AFFGraph (MON_MEMO : Tmemo; W1,W2,W3:PTArcCost;
                      Inv:PTInverse; Msg:String; Width:Byte; Break:Integer);
Var i       : Node;
    k       : ArcNum;
    W       : TPTArcCost;
    L       : Array[CostNb] of Byte;
    c       : CostNb;
    LS,LT   : Node;
    LD,SucNo: Node;
    SucPLine: Node;
    Line    : Integer;
    NCosts  : CostNb;
    ll       : string;
Begin
      mon_memo.clear;
      LoadW (W,W1,W2,W3,NCosts);
      mon_memo.Lines.Add ('');
      If Msg <> '' then
      If Msg[1] <> '*' then
           mon_memo.Lines.Add ('* '+Msg)
      else  mon_memo.Lines.Add(Msg);
      ll:= ('NX='+IntToStr(NX)+', NY='+IntToStr(NY)+', COSTS='+IntToStr(NCosts)
            +', M='+IntToStr(M));
      If Simple
      Then If NY > 0
           Then mon_memo.Lines.Add (ll+', BIPARTI')
           Else mon_memo.Lines.Add (ll+', SIMPLE')
      Else mon_memo.Lines.Add ('');
      If NX = 0
      Then mon_memo.Lines.Add ('* Graphe vide!')
      Else If M=0 then
              mon_memo.Lines.Add ('* Graphe n''ayant que des sommets isoles!')
      Else begin
         LD := MaxOutDeg;        {Nb max de successeurs}
         LS := Length(EdInt(NX+NY)); {Nb max de chiffres pour un noeud}
         LT := LS+2;
         If NCosts > 0 then begin    {S'il y a des couts a imprimer}
            Inc (LT);                {Compte une "("}
            For c := 1 to NCosts do If W[c] <> Nil then begin
               {Ajoute le nb max de chiffres pour la c-iŠme valuation}
               L[c] := MaxCostLen (W[c],M);
               LT := LT+L[c]+1
            End
         End;
         SucPLine := Min(LD,(Width-8) div LT); {Nb max de sucs par ligne}
         If NCosts = 0 then begin
            mon_memo.Lines.Add ('* Noeud: Successeurs');
            mon_memo.Lines.Add ('*------:'+StringOf(Max(12,LT*SucPLine),'-'))
         End
         Else begin
            mon_memo.Lines.Add ('* Noeud: Successeurs (Couts)');
            mon_memo.Lines.Add ('*------:'+StringOf(Max(20,LT*SucPLine),'-'));
         End;
         Line := 3+Ord(Msg<>'');
         ll:='';
         {Ecrit chaque liste de successeurs non vide}
         For i:=1 to NX+NY do If Head[i+1]-Head[i] > 0 then begin
            SucNo := 0;
            CountLine (Line,Break);
            ll:=inttostr(i)+' :';
            For k := Head[i] to Head[i+1]-1 do begin
               If SucNo = SucPLine then begin
                  CountLine (Line,Break);
                  SucNo := 0
               End;
               ll:=ll+ MidStr(inttostr(Succ[k]),1,LS+1);
               If NCosts > 0 then begin
                  ll:= ll+'(';
                  For c := 1 to NCosts do If W[c] <> Nil then begin
                     If Inv = Nil then
                        ll:=ll+MidStr(InttoStr(W[c]^[k]),1,L[c])
                     else
                        ll:=ll+MidStr(inttostr(W[c]^[Inv^[k]]),1,L[c]);
                     If c < NCosts then ll:=ll+',' else ll:=ll+')'
                  End
               End;
               If k < Head[i+1]-1 then ll:=ll+ ',';
               Inc (SucNo)
            End;
            mon_memo.Lines.Add (ll)
         End
      End;
     // If Break > 0 then Pause ('Fin de listing. Une touche...')
End;


Procedure  T_GRAPHE_LISTE.WriteGraph (var F:Text; W1,W2,W3:PTArcCost;
                                      Inv:PTInverse; Msg:String;
                                      Width:Byte; Break:Integer);
Var i       : Node;
    k       : ArcNum;
    W       : TPTArcCost;
    L       : Array[CostNb] of Byte;
    c       : CostNb;
    LS,LT,NL: Node;
    LD,SucNo: Node;
    SucPLine: Node;
    S       : String[20];
    Line    : Integer;
    NCosts  : CostNb;
Begin
      LoadW (W,W1,W2,W3,NCosts);
      WriteLn (F);
      If Msg <> '' then
      If Msg[1] <> '*' then WriteLn (F,'* ',Msg) else  WriteLn (F,Msg);
      Write (F,'NX=',NX,', NY=',NY,', COSTS=',NCosts,', M=',M);
      If Simple
      Then If NY > 0
           Then WriteLn (F,', BIPARTI')
           Else WriteLn (F,', SIMPLE')
      Else WriteLn (F);
      If NX = 0
      Then WriteLn (F,'* Graphe vide!')
      Else If M=0 then WriteLn (F,'* Graphe n''ayant que des sommets isoles!')
      Else begin
         LD := MaxOutDeg;        {Nb max de successeurs}
         LS := Length(EdInt(NX+NY)); {Nb max de chiffres pour un noeud}
         LT := LS+2;
         If NCosts > 0 then begin    {S'il y a des co–ts … imprimer}
            Inc (LT);                {Compte une "("}
            For c := 1 to NCosts do If W[c] <> Nil then begin
               {Ajoute le nb max de chiffres pour la c-ieme valuation}
               L[c] := MaxCostLen (W[c],M);
               LT := LT+L[c]+1
            End
         End;
         SucPLine := Min(LD,(Width-8) div LT); {Nb max de sucs par ligne}
         If NCosts = 0 then begin
            WriteLn (F,'* Noeud: Successeurs');
            WriteLn (F,'*------:',StringOf(Max(12,LT*SucPLine),'-'))
         End
         Else begin
            WriteLn (F,'* Noeud: Successeurs (Co–ts)');
            WriteLn (F,'*------:',StringOf(Max(20,LT*SucPLine),'-'))
         End;
         Line := 3+Ord(Msg<>'');
         {Ecrit chaque liste de successeurs non vide}
         For i:=1 to NX+NY do If Head[i+1]-Head[i] > 0 then begin
            SucNo := 0;
            CountLine (Line,Break);
            Write (F,i:6,' :');
            For k := Head[i] to Head[i+1]-1 do begin
               If SucNo = SucPLine then begin
                  WriteLn (F);
                  CountLine (Line,Break);
                  Write   (F,':':8);
                  SucNo := 0
               End;
               Write (F,Succ[k]:LS+1);
               If NCosts > 0 then begin
                  Write (F,'(');
                  For c := 1 to NCosts do If W[c] <> Nil then begin
                     If Inv = Nil then Write (F,W[c]^[k]:L[c])
                                  else Write (F,W[c]^[Inv^[k]]:L[c]);
                     If c < NCosts then Write (F,',') else Write (F,')')
                  End
               End;
               If k < Head[i+1]-1 then Write (F,',');
               Inc (SucNo)
            End;
            WriteLn (F)
         End
      End;
      If Break > 0 then Pause ('Fin de listing. Une touche...')

End;








Function T_GRAPHE_LISTE.LIRE_Head (i:Node):  ArcNum;
begin
  LIRE_Head:=Head[i];
end;

procedure T_GRAPHE_LISTE.ECRIRE_Head (i:Node; x :ArcNum);
begin
  Head[i]:=x;
end;

Function T_GRAPHE_LISTE.LIRE_Succ (i:ArcNum) :  Node;
begin
  LIRE_Succ:=Succ[i];
end;

procedure T_GRAPHE_LISTE.ECRIRE_Succ (i:ArcNum; x : Node);
begin
 Succ[i]:=x;
end;

Function T_GRAPHE_LISTE.LIRE_M : ArcNum;
begin
 LIRE_M := M;
end;

procedure T_GRAPHE_LISTE.ECRIRE_M (x :ArcNum);
begin
  M := x;
end;

Function T_GRAPHE.LIRE_Simple : Boolean;
begin
 LIRE_Simple:=Simple;
end;

procedure T_GRAPHE.ECRIRE_Simple (x :Boolean);
begin
 Simple := x;
end;



function T_GRAPHE_LISTE.UnPack (NoArc:Cost;
                                W:PTArcCost): PTR_T_GRAPHE_MATRICIEL;
Var i,j : Node;
    k   : ArcNum;
    C   : PTR_T_GRAPHE_MATRICIEL;
Begin

   // creation de la zone memoire
   New(C);
   // initialisation de l'isntance
   C^ := T_GRAPHE_MATRICIEL.CREATE;

   C^.NX     := NX;
   C^.NY     := NY;
   C^.Simple := Simple;
   C^.NoArc  := NoArc;

   {Initialise la matrice a vide}
   For i := 1 to NX do For j := 1 to C.LastCol do C.p_A[i,j] := NoArc;
   {Construit une ligne de matrice par liste de successeurs}
    For i := 1 to NX do
      For k := Head[i] to Head[i+1]-1 do begin
         j := Succ[k];
         If NY > 0  then j := j - NX;
         {Si G a des couts, on les copie dans la matrice}
         If W = Nil then C.p_A[i,j] := 1 else C.p_A[i,j] := W^[k];
         If C.p_A[i,j] = NoArc then Error ('UnPack: un cout est egal a Empty')
      End;

  UnPack := C;

End;


//function T_GRAPHE_LISTE.LIRE_COUTS:PTArcCost;
//begin
// LIRE_COUTS := W;
//end;


Procedure T_GRAPHE_LISTE.AllCC (var NCC:Node; var CC:TNodeInfo);
Var s,x: Node;
    k    : ArcNum;
    Q    : T_PILE_FILE;
Begin
      Q:=T_PILE_FILE.CREATE;

      If not Simple then Error ('AllCC: graphe non simple');
      Q.Clear;
      NCC := 0;
      For s := 1 to NX+NY do CC[s] := 0;
      For s := 1 to NX+NY do If CC[s] = 0 then begin
         Inc (NCC);
         CC[s] := NCC;
         Q.EnQueue (s);
         Repeat
            Q.DeQueue (x);
            For k := Head[x] to Head[x+1]-1 do If CC[Succ[k]] = 0 then begin
               CC[Succ[k]] := NCC;
               Q.EnQueue (Succ[k])
            End
         Until Q.SetIsEmpty;
      End;
    Q.DESTROY;
End;


Procedure T_GRAPHE_LISTE.AllSCC (var NSCC:Node; var SCC:TNodeInfo);
Var s,x,y  : Node;
    Count  : Node;
    P,Q    : T_PILE_FILE;
    NEXT   : THead;
    DFN,LOW: TNodeInfo;
Begin
    P := T_PILE_FILE.Create;
    Q := T_PILE_FILE.CREATE;

    P.Clear;
    Q.Clear;
    NSCC  := 0;
    Count := 0;
    For s := 1 to GraphOrder do DFN[s] := 0;
      For s := 1 to GraphOrder do If DFN[s] = 0 then begin
         Inc (Count);
         DFN[s] := Count;
         LOW[s] := Count;
         NEXT   := Head;
         P.Push (s);
         Q.Push (s);
         REPEAT
            x := Q.Front;
            If NEXT[x] = HEAD[x+1] then begin
               If LOW[x] = DFN[x] then begin
                  Inc (NSCC);
                  Repeat
                     P.Pop (y);
                     SCC[y] := NSCC
                  Until y = x;
               End;
               Q.Pop (x);
               If not Q.SetIsEmpty
               Then LOW[Q.Front] := Min(LOW[Q.Front],LOW[x]);
            End
            Else begin
               y := Succ[NEXT[x]];
               Inc (NEXT[x]);
               If DFN[y] = 0 then begin
                  Inc (Count);
                  DFN[y] := Count;
                  LOW[y] := Count;
                  P.Push (y);
                  Q.Push (y)
               End
               Else If (DFN[y] < DFN[x]) and P.InSet (y)
                    Then LOW[x] := Min(LOW[x],DFN[y])
            End
         Until Q.SetIsEmpty;
      End;

    P.Destroy;
    Q.Destroy;

End;


Procedure T_GRAPHE_LISTE.BFS (s:Node; var BFN,Father:TNodeInfo);
Var x,y,C: Node;
    k    : ArcNum;
    Z    : T_PILE_FILE;
Begin
   Z:=T_PILE_FILE.CREATE;

   Z.Clear;
   For x := 1 to NX+NY do begin
    BFN[x]    := 0;
    Father[x] := 0
   End;
   Father[s] := s;
   BFN[s]    := 1;
   C         := 1;
   Z.EnQueue (s);
   Repeat
     Z.DeQueue (x);
     For k := Head[x] to Head[x+1]-1 do
     begin
       y := Succ[k];
       If BFN[y] = 0 then
         begin
         Inc (C);
         BFN[y]    := C;
         Father[y] := x;
         Z.EnQueue (y)
         End
     End
   Until Z.SetIsEmpty;
   Z.DESTROY;
End;


Procedure T_GRAPHE_LISTE.Bipartite (var Bip:Boolean; var Color:TNodeInfo);
Var s,x,y: Node;
    k    : ArcNum;
    Q    : T_PILE_FILE;
Begin
      Q:=T_PILE_FILE.Create;

      If not Simple then Error ('Bipartite: graphe non simple');
      Bip := True;
      For x := 1 to NX+NY do Color[x] := 0;
      For s := 1 to NX+NY do
      If (Color[s] = 0) and Bip then begin
         Q.Clear;
         Q.EnQueue (s);
         Color[s] := 1;
         Repeat
            Q.DeQueue (x);
            For k := Head[x] to Head[x+1]-1 do begin
               y := Succ[k];
               if Color[y] = Color[x]
               Then Bip := False
               Else If Color[y] = 0 then begin
                  Q.EnQueue (y);
                  Color[y] := 3 - Color[x]
               End
            End
         Until Q.SetIsEmpty or not Bip;
      End;
   Q.DESTROY;
End;

Procedure T_GRAPHE_LISTE.BuildPreds (var H:T_GRAPHE_LISTE; Inv:PTInverse);
Var InDeg  : TNodeInfo;
    x,y    : Node;
    k      : ArcNum;
Begin

      If Simple then Error ('BuildPreds: graphe simple');
      H.NX     := NX;
      H.NY     := NY;
      H.Simple := Simple;
      H.M      := M;
      {Calcul des 1/2-degres interieurs pour reserver de la place dans H.Succ}
      GetInDegrees (InDeg);
      {Regle Head juste apres le dernier predecesseur, et ferme les listes}
      With H do begin
         Head[0] := 1;
         For x := 1 to NX+NY do Head[x] := Head[x-1] + InDeg[x];
         Head[NX+NY+1] := M+1
      End;
      {Charge H.Succ}
      For x := 1 to NX+NY do For k := Head[x] to Head[x+1]-1 do begin
         y := Succ[k];   {x predecesseur de y}
         With H do begin
            Dec  (Head[y]);
            Succ [Head[y]] := x;
            If Inv <> Nil then Inv^[Head[y]] := k
         End
      End;
End;

Procedure T_GRAPHE_LISTE.DFS (s:Node; var DFN,Father:TNodeInfo);
Var x,y,C: Node;
    Z    : T_PILE_FILE;
    Next : THead;
Begin
      Z:=T_PILE_FILE.CREATE;

      Z.Clear;
      For x := 1 to NX+NY do begin
         DFN[x]    := 0;
         Father[x] := 0
      End;
      Father[s] := s;
      DFN[s]    := 1;
      C         := 1;
      Next      := Head;
      Z.Push (s);
      Repeat
         x := Z.Front;
         If Next[x] >= Head[x+1]
         Then Z.Pop (x)
         Else begin
            y := Succ[Next[x]];
            Inc (Next[x]);
            If DFN[y] = 0 then begin
               Inc (C);
               DFN[y]    := C;
               Father[y] := x;
               Z.Push (y)
            End
         End
      Until Z.SetIsEmpty;
   Z.DESTROY;
End;

Procedure T_GRAPHE_LISTE.GetCircuit (var Circuit:TNodeInfo; var Size:Node);
Var x,y,s: Node;
    Z    : T_PILE_FILE;
    Next : THead;
    Mark : TNodeBool;
    Found: Boolean;
Begin
      Z:=T_PILE_FILE.CREATE;

      If Simple then Error ('GetCircuit: graphe non oriente');
      Found := False;
      For x := 1 to NX do Mark[x] := False;
      s := 0;
      While (not Found) and (s < NX) do begin
         Inc (s);
         If not Mark[s] then begin
            Z.Clear;
            Z.Push (s);
            Mark[s] := True;
            Next    := Head;
            Repeat
               x := Z.Front;
               If Next[x] >= Head[x+1]
               Then Z.Pop (x)
               Else begin
                  y := Succ[Next[x]];
                  Inc (Next[x]);
                  If not Mark[y] then begin
                     Mark[y] := True;
                     Z.Push (y)
                  End
                  Else If Z.InSet (y) then Found := True
               End
            Until Found or Z.SetIsEmpty;
         End
      End;
      If Found then begin
         {Recupere le circuit dans l'ordre, en fin de tableau}
         x := NX;
         While Z.Front <> y do begin
            Z.Pop (Circuit[x]);
            Dec (x)
         End;
         Z.Pop (Circuit[x]);
         Size := NX - x + 1;
         {Decale les sommets du circuit en debut de tableau}
         For y := x to NX do Circuit[y-x+1] := Circuit[y]
      End
      Else Size := 0;

      Z.DESTROY;

End;

Procedure T_GRAPHE_LISTE.GetLayers (var NLayer:node;
                     var Layer, Sorted:TNodeInfo);
Var InDeg   : TNodeInfo;
    Q       : T_PILE_FILE;
    i,j,Iter: Node;
    Assigned: Node;
    k       : ArcNum;
Begin
      Q:=T_PILE_FILE.Create;

      If Simple then Error ('GetLayers: graphe simple');
      GetInDegrees (InDeg);
      NLayer   := 0;
      Assigned := 0;
      Q.Clear;
      For i := 1 to NX do begin
         Layer[i] := 0;
         If InDeg[i] = 0 then Q.EnQueue (i)
      End;
      While not Q.SetIsEmpty do begin
         Inc (NLayer);
         For Iter := 1 to Q.CardOfSet do begin
             Q.DeQueue (i);
             Layer[i] := NLayer;
             Inc (Assigned);
             Sorted[Assigned] := i;
             For k := Head[i] to Head[i+1]-1 do begin
                 j := Succ[k];
                 Dec (InDeg[j]);
                 If InDeg[j] = 0 then Q.EnQueue (j)
             End
         End
      End;
      If Assigned < NX then NLayer := 0

End;



Procedure T_GRAPHE_LISTE.Bellman  (var W:TArcCost;
                                   s:Node;
                                   var V:TNodeCost;
                                   var P:TNodeInfo;
                                   var NegCirc:Boolean);
Var Step,x,y: Node;
    k       : ArcNum;
    H       : PTR_T_GRAPHE_LISTE;
    Inv     : PTInverse;
    Stable  : Boolean;
Begin
   // definition du graphe auxiliaire
   // init pointeur
   New (H);
   // creation du graphe
   H^ := T_GRAPHE_LISTE.CREATE;


   New (Inv);
   BuildPreds (H^,Inv);
   For x := 1 to GraphOrder do begin
      P[x] := 0;
      V[x] := MaxCost
   End;
   V[s] := 0;
   P[s] := s;
   Step := 0;
   Repeat
      Inc (Step);
      Stable := True;
      For y := 1 to NX+NY do begin
         For k := Head[y] to Head[y+1]-1 do begin
            x := Succ[k];
            If (V[x] < MaxCost) and (V[x] + W[Inv^[k]] < V[y]) then begin
               V[y]   := V[x] + W[Inv^[k]];
               P[y]   := x;
               Stable := False
            End
         End
      End;
   Until Stable or (Step = GraphOrder);
   NegCirc := (not Stable) and (Step = GraphOrder);

   H^.DESTROY;
   Dispose(H);
   Dispose (Inv);
End;

Procedure T_GRAPHE_LISTE.Bucket   (var W:TArcCost; s,t:Node;
                    var V:TNodeCost; var P:TNodeInfo);
Var Buckets : PBuckSpace;
    x,y     : Node;
    LB,CB{,MB}: BuckNo;
    k       : ArcNum;
    Done    : Boolean;
Begin

    New (Buckets);
    Buckets^:=T_BuckSpace.CREATE;

     For x := 1 to p_NX+p_NY do begin
        V[x] := MaxCost;
        P[x] := 0
     End;
     V[s] := 0;
     P[s] := s;
     Buckets.PushInto (0,s);
     LB   := 0;
     CB   := 0;
     Done := False;
     Repeat
        If Buckets.p_First[CB] = 0 then begin
           Repeat
              CB := (CB + 1) and UMax
           Until (Buckets.p_First[CB] > 0) or (CB = LB);
           If CB = LB then Done := True
        End;
        If Buckets.p_First[CB] > 0 then begin
           LB := CB;
           Buckets.PopFrom (CB,x);
           For k := Head[x] to Head[x+1]-1 do begin
              y  := Succ[k];
              If V[x] + W[k] < V[y] then begin
                 If P[y] > 0
                 Then Buckets.RemoveFrom ((CB+V[y]-V[x]) and UMax,y);
                 Buckets.PushInto ((CB + W[k]) and UMax,y);
                 V[y] := V[x] + W[k];
                 P[y] := x;
              End
           End
        End
     Until Done or (x = t);
  Buckets^.DESTROY;
End;


Procedure T_GRAPHE_LISTE.Dijkstra (var W:TArcCost; s,t:Node;
                    var V:TNodeCost; var P:TNodeInfo);
Var x,y : Node;
    k   : ArcNum;
    Free: TNodeBool;
    VMin: Cost;
Begin
      For x := 1 to NX+NY do begin
         P[x]    := 0;
         V[x]    := MaxCost;
         Free[x] := True
      End;
      V[s]    := 0;
      P[s]    := s;
      Repeat
         VMin := MaxCost;
         For y := 1 to NX+NY do If Free[y] and (V[y] < VMin) then begin
            x    := y;
            VMin := V[y]
         End;
         If VMin < MaxCost then begin
            Free[x] := False;
            For k := Head[x] to Head[x+1]-1 do begin
               y  := Succ[k];
               If VMin+W[k] < V[y] then begin
                  V[y] := VMin + W[k];
                  P[y] := x
               End
            End
         End
      Until (VMin = MaxCost) or (x = t);
End;


Procedure T_GRAPHE_LISTE.Floyd (var W:TArcCost;
                                var V:CostMatrix;
                                var P:NodeMatrix;
                                var NegCirc:Boolean);
Var i,j,k: Node;
    a    : ArcNum;
Begin
      NegCirc := True;
      {Initialise la matrice V initiale}
      For i := 1 to NX + NY do For j := 1 to NX + NY do begin
         If i <> j then begin
            V[i,j] := MaxCost;
            P[i,j] := 0
         End
         Else begin
            V[i,i] := 0;
            P[i,i] := i
         End
      End;
      For i := 1 to NX + NY do For a := Head[i] to Head[i+1]-1 do begin
         j := Succ[a];
         If i <> j then begin
            V[i,j] := W[a];
            P[i,j] := i
         End
         Else If W[a] < 0 then Exit
      End;
      {Calcule des matrices V(k) successives}
      For k := 1 to NX + NY do begin
         For i := 1 to NX + NY do If V[i,k] < MaxCost then begin
            If (V[k,i] < MaxCost) and (V[i,k] + V[k,i] < 0) then Exit;
            For j := 1 to NX + NY do
            If (V[k,j] < MaxCost) and (V[i,j] > V[i,k] + V[k,j]) then begin
               V[i,j] := V[i,k] + V[k,j];
               P[i,j] := P[k,j]
            End
         End
      End;
      NegCirc := False;
End;


Procedure T_GRAPHE_LISTE.Schedule (var W:TArcCost;
                                   Alpha,Omega:Node;
                                   var V:TNodeCost;
                                   var P:TNodeInfo);
Var i,x,y,NLayer: Node;
    k           : ArcNum;
    Layer,Sorted: TNodeInfo;
Begin
      If Simple then Error ('Schedule: graphe simple interdit');
      {Decompose G en niveaux}
      GetLayers (NLayer,Layer,Sorted);
      {Verifie Alpha}
      If (Sorted[1] <> Alpha) or (Layer[Sorted[2]] = 1)
      Then Error ('Schedule: Alpha doit etre seul dans le niveau 1');
      {Verifie Omega}
      If (Sorted[NX] <> Omega) or (Layer[Sorted[NX-1]] = NLayer)
      Then Error ('Schedule: Omega doit etre seul dans le dernier niveau');
      For x := 1 to NX do V[x] := -MaxCost;
      P[Alpha] := Alpha;
      V[Alpha] := 0;
      For i := 1 to NX do begin
         x := Sorted[i];
         For k := Head[x] to Head[x+1]-1 do begin
            y := Succ[k];
            If V[x] + W[k] > V[y] then begin
               V[y] := V[x] + W[k];
               P[y] := x
            End
         End
      End
End;

Procedure T_GRAPHE_LISTE.DijHeap
                     (var W:TArcCost; s,t:Node;
                      var V:TNodeCost; var P:TNodeInfo);
Var x,y: Node;
    H  : T_Heap;
    k  : ArcNum;
Begin
      H:=T_Heap.CREATE;


      For x := 1 to NX+NY do begin
         P[x] := 0;
         V[x] := MaxCost
      End;
      H.ClearHeap;
      V[s] := 0;
      P[s] := s;
      H.HeapInsert (V,s);
      Repeat
         H.HeapMin (V,x);
         For k := Head[x] to Head[x+1]-1 do begin
            y := Succ[k];
            If V[x] + W[k] < V[y] then begin
               V[y] := V[x] + W[k];
               P[y] := x;
               If H.InHeap (y) then H.MoveUp (V,y) else H.HeapInsert (V,y)
            End
         End
      Until H.HeapIsEmpty;

     H.DESTROY;
End;


Procedure T_GRAPHE_LISTE.ESOPO (var W:TArcCost; s:Node;
                                var V:TNodeCost; var P:TNodeInfo);
Var x,y : Node;
    k   : ArcNum;
    Q   : T_PILE_FILE;
Begin
   Q:=T_PILE_FILE.CREATE;

      For x := 1 to NX+NY do begin
         P[x] := 0;
         V[x] := MaxCost
      End;
      V[s] := 0;
      P[s] := s;
      Q.Clear;
      Q.EnQueue (s);
      Repeat
         Q.DeQueue (x);
         For k := Head[x] to Head[x+1]-1 do begin
            y := Succ[k];
            If V[x] + W[k] < V[y] then begin
               V[y] := V[x] + W[k];
               If P[y] = 0
               Then Q.EnQueue (y)
               Else If not Q.InSet(y) then Q.Push (y);
               P[y] := x
            End
         End
      Until Q.SetIsEmpty;

   Q.DESTROY;
End;


Procedure T_GRAPHE_LISTE.FIFO (var W:TArcCost; s:Node;
                               var V:TNodeCost; var P:TNodeInfo);
Var x,y: Node;
    k  : ArcNum;
    Q  : T_PILE_FILE;
Begin
   Q:=T_PILE_FILE.CREATE;
      For x := 1 to NX+NY do begin
         P[x] := 0;
         V[x] := MaxCost
      End;
      V[s] := 0;
      P[s] := s;
      Q.Clear;
      Q.EnQueue (s);
      Repeat
         Q.DeQueue (x);
         For k := Head[x] to Head[x+1]-1 do begin
            y := Succ[k];
            If V[x] + W[k] < V[y] then begin
               V[y] := V[x] + W[k];
               P[y] := x;
               If not Q.InSet (y) then Q.EnQueue (y)
            End
         End
      Until Q.SetIsEmpty;
   Q.DESTROY;
End;

// Augment: augmente le flot sur une chaine ameliorante

Procedure T_GRAPHE_LISTE.Augment (s,t:Node; var Phi:TArcCost;
                   var AugVal:TNodeCost;  var ArcTo: THead;
                   var Father:TNodeInfo);
Var k    : ArcNum;
    x    : Node;
    Delta: Cost;
Begin
   Delta := AugVal[t];
   x     := t;
   Repeat
      k := ArcTo[x];
      If Succ[k] = x
      Then Inc (PHI[k],Delta)
      Else Dec (PHI[k],Delta);
      x := Father[x]
   Until x = s;
End;




// Busacker: algorithme de flot de cout minimal
Procedure T_GRAPHE_LISTE.Busacker  (var C,W:TArcCost; s,t: node; ReqF:Cost;
                     var F,K:Cost; var PHI:TArcCost);
Var H     : T_GRAPHE_LISTE;
    Inv   : PTInverse;
    V     : TNodeCost;
    P     : TNodeInfo;
    ArcTo : THead;
    AugVal: TNodeCost;
    NAug  : Integer;

Procedure ESOPO (var G,H:T_GRAPHE_LISTE;   var Inv:TInverse; var W:TArcCost; s:Node;
                 var V:TNodeCost; var P:TNodeInfo; var ArcTo:THead;
                 var AugVal:TNodeCost);
Var x,y : Node;
    k   : ArcNum;
    Q   : T_PILE_FILE;
Begin
   raise E_EXCEPTION_U_GRAPHES.Create
      ('Unite: U_GRAPHES, Méthode : Busacker avec ESOPO not implemented')
End;

Procedure FIFO (var G, H:T_GRAPHE_LISTE;   var Inv:TInverse; var W:TArcCost; s:Node;
                var V:TNodeCost; var P:TNodeInfo;  var ArcTo:THead;
                var AugVal:TNodeCost);
Var x,y: Node;
    k  : ArcNum;
    Q  : T_PILE_FILE;
Begin
      Q:=T_PILE_FILE.CREATE;
      For x := 1 to NX+NY do begin
         P[x] := 0;
         V[x] := MaxCost
      End;
      V[s] := 0;
      P[s] := s;
      AugVal[s] := MaxCost;
      Q.Clear;
      Q.EnQueue (s);
      Repeat
         Q.DeQueue (x);
         {Balaie les arcs avant d'origine x}
         For k := Head[x] to Head[x+1]-1 do
           If Phi[k] < C[k] then begin
            y := Succ[k];
            If V[x] + W[k] < V[y] then begin
               V[y]      := V[x] + W[k];
               P[y]      := x;
               ArcTo[y]  := k;
               AugVal[y] := Min (AugVal[x], C[k]-PHI[k]);
               Q.EnQueue(Y);
            End
         End;
         {Puis balaie les arcs arriere d'extremite x}
         For k := H.Head[x] to H.Head[x+1]-1 do
         If Phi[Inv[k]] > 0 then begin
            y := H.Succ[k];
            If V[x] - W[Inv[k]] < V[y] then begin
               //If P[y] = 0 then Q.EnQueue (y) else Q.Push(y);
               V[y]      := V[x] - W[Inv[k]];
               P[y]      := x;
               ArcTo[y]  := Inv[k];
               AugVal[y] := Min (AugVal[x], PHI[ArcTo[y]]);
               Q.EnQueue(y);
            End;
         End
    Until Q.SetIsEmpty;
   Q.Destroy;
End;

Begin
      If (s < 1) or (s > NX+NY) then Error ('Busacker: source inconnue');
      If (t < 1) or (t > NX+NY) then Error ('Busacker: puits inconnu');
      NAug := 0;
      H:=T_GRAPHE_LISTE.CREATE;
      New (Inv);                          {Alloue table de corresp. des arcs}
      BuildPreds (H,Inv);                 {Construit graphe inverse en O(M)}
      F := 0;                             {Initialise valeur totale du flot}
      K := 0;                             {Initialise cout total du flot}
      FillChar (PHI[1],M*SizeOf(Cost),#0);{Initialise flots sur les arcs}
      Repeat
         FIFO (Self, H,Inv^, W,s,V,P,ArcTo,AugVal);
        // ESOPO n'est pas implementé pour Busacker
        // ESOPO (Self, H,Inv^, W,s,V,P,ArcTo,AugVal);
         If P[t] > 0 then begin
            AugVal[t] := Min (AugVal[t],ReqF-F);
            Augment (s,t,Phi,AugVal,ArcTo,P);
            Inc (F,AugVal[t]);
            Inc (K,AugVal[t]*V[t]);
            Inc (NAug)
         End
      Until (P[t] = 0) or (F = ReqF);
      H.DESTROY;
      Dispose (Inv)
End;


// Ahuja: algorithme des distances estimees au puits

Procedure T_GRAPHE_LISTE.Ahuja (var C:TArcCost;    s,t: node;
                 var F:Cost;  var PHI:TArcCost);
Var H      : PTR_T_GRAPHE_LISTE;
    Inv    : PTInverse;
    x,y    : Node;
    k      : ArcNum;
    Father : TNodeInfo;
    Dist   : TNodeInfo;
    Number : TNodeInfo;
    ArcTo  : THead;
    ArcFrom: THead;
    Delta  : Cost;
    NAug   : Integer;
    Found  : Boolean;
    Optimum: Boolean;

Procedure ExactDistances (var H:T_GRAPHE_LISTE; t:Node; var Dist:TNodeInfo);
Var x,y: Node;
    k  : ArcNum;
    Q  : T_PILE_FILE;
Begin
   Q:=T_PILE_FILE.Create;

   With H do begin
      Q.Clear;
      FillChar (Number,SizeOf(Number),#0);
      For x := 1 to NX+NY do Dist[x] := MaxNode;
      Dist[t] := 0;
      Inc (Number[0]);
      Optimum := False;
      Q.EnQueue (t);
      Repeat
         Q.DeQueue (x);
         For k := Head[x] to Head[x+1]-1 do begin
            y := Succ[k];
            If Dist[y] = MaxNode then begin
               Dist[y] := Dist[x] + 1;
               Inc (Number[Dist[y]]);
               Q.EnQueue (y)
            End
         End
      Until Q.SetIsEmpty;
   End;

   Q.DESTROY;
End;

Procedure GetForwardArc (x:Node; var Found:Boolean);
Begin
      k          := ArcFrom[x];
      While (k < Head[x+1]) and
            ((Phi[k] = C[k]) or (Dist[x] <> Dist[Succ[k]] + 1))
      Do Inc (k);
      ArcFrom[x] := k;
      Found      := k < Head[x+1]
End;

Procedure GetBackwardArc (x:Node; var Found:Boolean);
Begin
   With H^ do begin
      k        := ArcTo[x];
      While (k < Head[x+1]) and
            ((Phi[Inv^[k]] = 0) or (Dist[x] <> Dist[Succ[k]] + 1))
      Do Inc (k);
      ArcTo[x] := k;
      Found    := k < Head[x+1]
   End
End;

Procedure AddForwardArc (var x:Node);
Begin
   y         := Succ[ArcFrom[x]];
   Father[y] := x;
   x         := y
End;

Procedure AddBackwardArc (var x:Node);
Begin
   y         := H^.Succ[ArcTo[x]];
   Father[y] := x;
   x         := y
End;

Procedure Augment;
Begin
   Delta := MaxCost;
   y     := t;
   Repeat
      x := Father[y];
      If ArcFrom[x] < Head[x+1]
      Then Delta := Min (Delta, C[ArcFrom[x]]-Phi[ArcFrom[x]])
      Else Delta := Max (Delta, Phi[Inv^[ArcTo[x]]]);
      y := Father[y]
   Until y = s;
   y     := t;
   Repeat
      x := Father[y];
      If ArcFrom[x] < Head[x+1]
      Then begin
         Inc (Phi[ArcFrom[x]],Delta)
End
      Else Begin
      Dec (Phi[Inv^[ArcTo[x]]]);
End;
      y := Father[y]
   Until y = s;
   Inc (F,Delta);
   Inc (NAug)
End;

Procedure ShowDist;
Var x:Node;
Begin
   Write ('DIST:');
   For x := 1 to NX+NY do Write (x,':',Dist[x],',');
   WriteLn;
   Pause ('Any key')
End;

Procedure Retreat (var x:Node; var Optimum:Boolean);
Var k   : ArcNum;
    DMin: Node;
Begin
   {Met Dist[x] a Min(Dist[y]+1;y succ de x dans le graphe d'ecart}
   DMin := MaxNode;
      For k := Head[x] to Head[x+1]-1 do begin
         y := Succ[k];
         If Phi[k] < C[k] then DMin := Min (DMin,Dist[y]);
      End;
      ArcFrom[x] := Head[x];
   With H^ do begin
      For k := Head[x] to Head[x+1]-1 do begin
         y := Succ[k];
         If Phi[Inv^[k]] > 0 then DMin := Min (DMin,Dist[y]);
      End;
      ArcTo[x] := Head[x]
   End;
   Dec (Number[Dist[x]]);
   Optimum := Number[Dist[x]] = 0;
   Dist[x] := DMin+1;
   Inc (Number[Dist[x]]);
   x := Father[x];
End;


Begin
      If (s < 1) or (s > NX+NY) then Error ('Ahuja: source inconnue');
      If (t < 1) or (t > NX+NY) then Error ('Ahuja: puits inconnu');
      New (H);                            {Alloue graphe inverse}
      H^:=T_GRAPHE_LISTE.Create;


      New (Inv);                          {Alloue table de corresp. des arcs}
      BuildPreds (H^,Inv);                {Construit graphe inverse en O(M)}
      F    := 0;                          {Initialise valeur totale du flot}
      NAug := 0;                          {Nombre d'augmentations de flot}
      FillChar (PHI[1],M*SizeOf(Cost),#0);{Initialise flots sur les arcs}
      ArcFrom := Head;                    {Arc avant actuel}
      ArcTo   := H^.Head;                 {Arc arriere actuel}
      ExactDistances (H^,t,Dist);         {Calcul des distances au puits}
      Father[s] := s;
      x := s;
      Repeat
         GetForwardArc (x,Found);
         If Found then begin
            AddForwardArc (x);
            If x = t then begin
               Augment;
               x := s
            End
         End
         Else begin
            GetBackwardArc (x,Found);
            If Found then begin
               AddBackwardArc (x);
               If x = t then begin
                  Augment;
                  x := s
               End
            End
            Else Retreat (x,Optimum)
         End
      Until Optimum or (Dist[s] >= NX+NY);

      H.Destroy;
      Dispose (H);
      Dispose (Inv)


End;

// CheckFlow: verifie un flot donn‚
Procedure T_GRAPHE_LISTE.CheckFlow (var C:TArcCost;    s,t:node;
                     F:Cost;  var PHI:TArcCost);
Var x,y    : Node;
    k      : ArcNum;
    InFlow : TNodeCost;
    OutFlow: TNodeCost;
Begin
      {1. Verifie que le flot est compatible avec les capacites}
      For k := 1 to M do begin
         If PHI[k] <    0 then Error ('CheckFlow: flot negatif sur un arc');
         If PHI[k] > C[k] then Error ('CheckFlow: flot excedant capacite en '
                                      +IntToStr(k))
      End;
      {2. Verifie conservation du flot en chaque noeud}
      FillChar (InFlow, SizeOf(InFlow), #0);
      FillChar (OutFlow,SizeOf(OutFlow),#0);
      For x := 1 to NX+NY do begin
         For k := Head[x] to Head[x+1]-1 do begin
            y := Succ[k];
            Inc (OutFlow[x],PHI[k]);
            Inc (InFlow [y],PHI[k])
         End
      End;
      For x := 1 to NX+NY do
      If (x <> s) and (x <> t) and (InFlow[x] <> OutFlow[x]) then begin
      Error ('CheckFlow : Node '+IntToStr(x)+'. In='+FloatToStr(InFlow[x])
             +'. Out='+FloatToStr(OutFlow[x]));
      Error ('CheckFlow: flot non conserve en '+IntToStr(x));
      End;
      If (x <> s) and (x <> t) and (InFlow[x] <> OutFlow[x])
      Then Error ('CheckFlow: flot non conserve en un noeud');
      If OutFlow[s] <> F then Error ('CheckFlow: flot de s <> F');
      If InFlow [t] <> F then Error ('CheckFlow: flot vers t <> F')
End;

   // Fulkerson: algorithme de flot maximal
Procedure T_GRAPHE_LISTE.Fulkerson
                    (var C:TArcCost;    s,t: node;
                     var F:Cost;  var PHI:TArcCost);
Var H     : PTR_T_GRAPHE_LISTE;
    Inv   : PTInverse;
    x,y   : Node;
    Father: TNodeInfo;
    ArcTo : THead;
  {  Delta : Cost; }
    AugVal: TNodeCost;
    Q     : T_PILE_FILE;
    NAug  : Integer;

Procedure ScanSuccs;
Var k: ArcNum;
Begin
   For k := Head[x] to Head[x+1]-1 do
   begin
      y := Succ[k];
      If (Father[y] = 0) and (PHI[k] < C[k]) then
      begin
         Father[y] := x;
         ArcTo [y] := k;
         AugVal[y] := Min (AugVal[x], C[k]-PHI[k]);
         Q.EnQueue (y)
      End;
   end;
End;

Procedure ScanPreds;
Var k: ArcNum;
Begin
   With H^ do For k := Head[x] to Head[x+1]-1 do begin
      y := Succ[k];
      If (Father[y] = 0) and (PHI[Inv^[k]] > 0) then begin
         Father[y] := x;
         ArcTo [y] := Inv^[k];
         AugVal[y] := Min (AugVal[x], PHI[ArcTo[y]]);
         Q.EnQueue (y)
      End
   End
End;

Begin
      Q:=T_PILE_FILE.CREATE;

      new(H);
      H^:=T_GRAPHE_LISTE.Create;

      If (s < 1) or (s > NX+NY) then Error ('Fulkerson: source inconnue');
      If (t < 1) or (t > NX+NY) then Error ('Fulkerson: puits inconnu');
      NAug := 0;
      New (H);                            {Alloue graphe inverse}
      H^:=T_GRAPHE_LISTE.Create;
      New (Inv);                          {Alloue table de corresp. des arcs}
      BuildPreds (H^,Inv);                {Construit graphe inverse en O(M)}
      F := 0;                             {Initialise valeur totale du flot}
      FillChar (PHI[1],M*SizeOf(Cost),#0);{Initialise flots sur les arcs}

      Repeat

         {1.  Cherche en largeur une chaine augmentante de s a t}
         {1.1 Initialisation explo en largeur, Father sert aussi de marques}
         Q.Clear;
         Q.EnQueue  (s);
         FillChar (Father,SizeOf(Father),#0);
         Father[s] := s;
         AugVal[s] := MaxCost;

         {1.2 Exploration dans le graphe d'ecart,non genere explicitement}
         Repeat
            Q.DeQueue (x);
            ScanSuccs;  {Propage le marquage par les (x,y) non satures}
            ScanPreds;  {Propage le marquage par les (y,x) de flux non nul}
         Until Q.SetIsEmpty or (Father[t] <> 0);

         {2.  Augmentation du flot si une chaine ameliorante est trouvee}
         If Father[t] <> 0 then begin
            Augment (s,t,Phi,AugVal,ArcTo,Father);
            Inc (F,AugVal[t]);
            Inc (NAug)
         End;

      Until Father[t] = 0;
      Dispose (Inv);

   Q.DESTROY;
   H^.DESTROY;
   Dispose (H);

End;



Procedure T_GRAPHE_LISTE.Scaling (var C:TArcCost;    s,t: node;
                   var F:Cost;  var PHI:TArcCost);
Var H      : PTR_T_GRAPHE_LISTE;
    Inv    : PTInverse;
    x,y    : Node;
    k      : ArcNum;
    Father : TNodeInfo;
    ArcTo  : THead;
    Delta,U: Cost;
    Thresh : Cost;
    AugVal : TNodeCost;
    Q      : T_PILE_FILE;
    NAug   : Integer;

Procedure ScanSuccs;
Var k: ArcNum;
Begin
   For k := Head[x] to Head[x+1]-1 do begin
      y := Succ[k];
      If (Father[y] = 0) and (C[k] - Phi[k] >= Thresh) then begin
         Father[y] := x;
         ArcTo [y] := k;
         AugVal[y] := Min (AugVal[x], C[k]-PHI[k]);
         Q.EnQueue (y)
      End
   End
End;

Procedure ScanPreds;
Var k: ArcNum;
Begin
   With H^ do For k := Head[x] to Head[x+1]-1 do begin
      y := Succ[k];
      If (Father[y] = 0) and (PHI[Inv^[k]] >= Thresh) then begin
         Father[y] := x;
         ArcTo [y] := Inv^[k];
         AugVal[y] := Min (AugVal[x], PHI[ArcTo[y]]);
         Q.EnQueue (y)
      End
   End
End;

Begin


      If (s < 1) or (s > NX+NY) then Error ('Scaling: source inconnue');
      If (t < 1) or (t > NX+NY) then Error ('Scaling: puits inconnu');
      NAug := 0;
      New (H);                              {Alloue graphe inverse}

      H^:=T_GRAPHE_LISTE.Create;
      Q:=T_PILE_FILE.Create;

      New (Inv);                            {Alloue table de corresp. arcs}
      BuildPreds (H^,Inv);                  {Construit graphe inverse en O(M)}
      F := 0;                               {Initialise valeur totale du flot}
      FillChar (PHI[1],M*SizeOf(Cost),#0);  {Initialise flots sur les arcs}
      U := -MaxCost;                        {Calcule capa max U}
      For k := 1 to M do U := Max (C[k],U);
      Thresh := 1;                          {Thresh = 2**Int(Log2(U))}
      While Thresh <= U do Thresh:=2*Thresh;
      Thresh := Thresh div 2;

      Repeat

         {Flot max dans graphe des arcs de capa r‚s. >= Thresh}

         Repeat

            {1.  Cherche en largeur une chaine augmentante de s a t}
            {1.1 Initialise explo en largeur, Father sert aussi de marques}
            Q.Clear;
            Q.EnQueue  (s);
            FillChar (Father,SizeOf(Father),#0);
            Father[s] := s;
            AugVal[s] := MaxCost;

            {1.2 Exploration dans le graphe d'ecart,non genere explicitement}
            Repeat
               Q.DeQueue (x);
               ScanSuccs;  {Propage le marquage par les (x,y) non satur‚s}
               ScanPreds   {Propage le marquage par les (y,x) de flux non nul}
            Until Q.SetIsEmpty or (Father[t] <> 0);

            {2.  Augmentation du flot si une chaine ameliorante est trouvee}
            If Father[t] <> 0 then begin
               Augment (s,t,Phi,AugVal,ArcTo,Father);
               Inc (F,AugVal[t]);
               Inc (NAug)
            End;

         Until Father[t] = 0;
         Thresh := Thresh div 2

      Until Thresh = 0;
      Q.Destroy;
      H^.Destroy;
      Dispose (H);
      Dispose (Inv)
End;



Procedure T_GRAPHE_LISTE.BipMatch   (var Card:Node; var Mate:TNodeInfo);
Var x,y   : Node;
    k     : ArcNum;
    Q     : T_PILE_FILE;
    Father: TNodeInfo;
    Found : Boolean;

Procedure InitialMatching;
Var x: Node;
Begin
      For x := 1 to NX do begin
         k := Head[x];
         While (Mate[x] = 0) and (k < Head[x+1]) do
         If Mate[Succ[k]] = 0 then begin
            Mate[x]       := Succ[k];
            Mate[Succ[k]] := x;
            Inc (Card)
         End
         Else Inc (k)
      End
End;

Procedure Augment (y:Node);
Var i:Node;
Begin
   Found     := True;
   Inc (Card);
   Father[y] := x;
   Repeat
      Mate[y] := Father[y];
      x := Mate[Father[y]];
      Mate[Father[y]] := y;
      y := x
   Until y = 0;
End;

Begin
      Q:=T_PILE_FILE.CREATE;

      If NY = 0 then Error ('BipMatch: graphe non biparti');
      {Couplage heuristique initial en O(M)}
      Card := 0;
      For x := 1 to NX+NY do Mate[x] := 0;

      InitialMatching;
      If Card = Min(NX,NY) then Exit;
      {Boucle generale: recherche de CAA en O(M) + transfert en O(N)}
      Repeat
         {1. Recherche de CAA par exploration en largeur}
         Found := False;
         Q.Clear;
         For x := 1 to NX do If Mate[x] = 0 then Q.EnQueue (x);
         If Q.CardOfSet > 0 then begin
            For y := NX+1 to NX+NY do Father[y] := 0;
            Repeat
               Q.DeQueue (x);
               k := Head[x];
               While (not Found) and (k < Head[x+1]) do begin
                  y := Succ[k];
                  If Mate[y] = 0
                  Then Augment (y)
                  Else If Father[y] = 0 then begin
                     Father[y] := x;
                     Q.EnQueue (Mate[y])
                  End;
                  Inc (k)
               End
            Until Q.SetIsEmpty or Found
         End
      Until not Found;

      Q.DESTROY;
End;

Procedure T_GRAPHE_LISTE.KRUSKAL (var W:TArcCost; var Weight:Cost;
                   var NEdge:Node;  var Node1,Node2:TNodeInfo);
Var Num     : ArcNum;                 {Nombre d'aretes du tas}
    TOrig   : TSucc;                  {Tas: sommet-origine de chaque arete}
    TEdge   : PTInverse;              {Tas: rang dans G.Succ de chaque arete}
    Next    : Array[Node] of Integer; {Tableau des classes d'equivalence}
    x,xClass: Node;
    y,yClass: Node;
    Iter,k  : ArcNum;

//----------------------------------------------------------------------------//
// Kruskal/MoveDown: descend l'arete stockee a la position k du tas   O(logM) //
//----------------------------------------------------------------------------//

Procedure MoveDown (k:ArcNum);
Var i,j : Word;
    kOri: Node;
    kArc: ArcNum;
Begin
      kOri := TOrig [k];
      kArc := TEdge^[k];
      i    := k;
      j    := 2 * i;
      If (j < Num) and (W[TEdge^[j+1]] < W[TEdge^[j]]) then Inc(j);
      While (j <= Num) and (W[TEdge^[j]] < W[kArc]) do begin
         TOrig [i] := TOrig[j];
         TEdge^[i] := TEdge^[j];
         i         := j;
         j         := 2 * i;
         If (j < Num) and (W[TEdge^[j+1]] < W[TEdge^[j]]) then Inc(j)
      End;
      TOrig [i]  := kOri;
      TEdge^[i]  := kArc
End;

//----------------------------------------------------------------------------//
// Kruskal/HeapMin: enleve arete de poids min du tas, reforme le tas. O(logM) //
//----------------------------------------------------------------------------//

Procedure HeapMin (var x:Node; var k:ArcNum);
Begin
   x := TOrig [1];
   k := TEdge^[1];
   Dec (Num);
   If Num > 0 then begin
      TOrig [1] := TOrig [Num+1];
      TEdge^[1] := TEdge^[Num+1];
      MoveDown (1)
   End
End;

//----------------------------------------------------------------------------//
// Kruskal/MakeHeap: construit un tas d'aretes                           O(M) //
//----------------------------------------------------------------------------//

Procedure MakeHeap;
Var x: Node;
    k: ArcNum;
Begin
      {1. Charge les ardtes en vrac, en evitant les doubles}
      Num := 0;
      For x := 1 to GraphOrder do begin
         For k := Head[x] to Head[x+1]-1 do If x < Succ[k] then begin
            Inc   (Num);
            TOrig [Num] := x;
            TEdge^[Num] := k
         End
      End;
      {2. Fait descendre tout element de H avec fils a sa position definitive}
      For k := Num div 2 downto 1 do MoveDown (k)
End;

//----------------------------------------------------------------------------//
// Kruskal/InitClasses: initialise les classes d'equivalence             O(N) //
//----------------------------------------------------------------------------//

Procedure InitClasses;
Var x: Node;
Begin
   For x := 1 to GraphOrder do Next[x] := -1
End;

//----------------------------------------------------------------------------//
// Kruskal/Find: trouve l'element canonique de la classe de x         O(logN) //
//----------------------------------------------------------------------------//

Function Find (x:Node): Node;
Begin
   While Next[x] > 0 do x := Next[x];
   Find := x
End;

//----------------------------------------------------------------------------//
// Kruskal/Union: fusionne les classes d'equivalence de x et y           O(1) //
//----------------------------------------------------------------------------//

Procedure Union (x,y:Node);
Var NNodes: Integer;
Begin
   NNodes := Next[x] + Next[y];
   If Next[x] > Next[y] then begin
      Next[x] := y;
      Next[y] := NNodes
   End
   Else begin
      Next[y] := x;
      Next[x] := NNodes
   End
End;

Begin
      If not Simple then Error ('Kruskal: graphe non simple');
      New (TEdge);
      Weight := 0;
      NEdge  := 0;
      Iter   := 0;
      InitClasses;
      MakeHeap;
      Repeat
         Inc (Iter);
         HeapMin (x,k);
         y := Succ[k];
         xClass := Find(x);
         yClass := Find(y);
         If xClass <> yClass then begin
            Union (xClass,yClass);
            Inc (Weight,W[k]);
            Inc (NEdge);
            Node1[NEdge] := x;
            Node2[NEdge] := y
         End
      Until (Iter = M div 2) or (NEdge = GraphOrder-1);
      Dispose (TEdge)
End;

Procedure T_GRAPHE_LISTE.EDMONDS (var W:TArcCost; s:Node;
                   var Weight:Cost; var Pred:TNodeInfo);
Var Pack : TNodeInfo; {Pack[x] dernier pseudo-noeud contenant x}
    Next : TNodeInfo; {+ vieux pseudo-noeud ayant recu x}
    WNow : PTArcCost; {Poids modifies}
    Orig : TNodeInfo; {Orig[x] origine dans G de (Pred[x],x)}
    Extr : TNodeInfo; {Extr[x] extremite dans G de (Pred[x],x)}
    WMin : TNodeCost; {Poids de l'arc correspondant}
    Num  : TNodeInfo; {Numero de visite dans la detection de circuit}
    Last : Node;      {Dernier pseudo-noeud}
    First: Node;      {1er pseudo-noeud de la derniere fournee}
    Limit: Node;      {Plus grand pseudo-noeud permis}
    Found: Boolean;


//----------------------------------------------------------------------------//
// Edmonds/Initialize: initialisations diverses                          O(M) //
//----------------------------------------------------------------------------//
// Definit tous les noeuds comme ‚tant contractes sur eux-memes, initialise   //
// les distances aux predecesseurs (WMin) et les couts reduits (WNow^).       //
//----------------------------------------------------------------------------//

Procedure Initialize;
Var x: Node;
Begin
   Limit := Min (2*GraphOrder-2,MaxNode);
   For x := 1 to MaxNode do begin
      Next[x] := x;
      Pack[x] := x;
      WMin[x] := MaxCost
   End;
   First  := 1;
   Last   := GraphOrder;
   WNow^  := W
End;


Procedure ComputeH;
Var x,xNow : Node;
    y,yNow : Node;
    k      : ArcNum;
Begin
      For x := 1 to GraphOrder do
      For k := Head[x] to Head[x+1]-1 do If Succ[k] <> s then begin
         y    := Succ[k];
         xNow := Pack[x];
         yNow := Pack[y];
         If (xNow <> yNow) and (yNow >= First) then begin
            If WNow^[k] < WMin[yNow] then begin
             WMin[yNow] := WNow^[k];
               Pred[yNow] := xNow;
               Orig[yNow] := x;
               Extr[yNow] := y
            End
         End
      End;
End;

Procedure PackCircuits;
Var x,y,z,Search: Node;
    xNow,yNow   : Node;
    NewNode     : Node;
    k           : ArcNum;
Begin
      Search  := 0;
      NewNode := Last;
      For x := 1 to Last do Num[x] := 0;
      For x := 1 to Last do
      If (x <> s) and (Pack[x] = x) and (Num[x] = 0) then begin
         {x noeud non marque du graphe courant, different de la racine}
         {On remonte ses preds en les numerotant Search}
         Inc (Search);
         y := x;
         Repeat
            Num[y] := Search;
            y      := Pred[y];
            y      := Pack[y]; {Necess: y peut etre dans un pseudo-noeud}
         Until (y = s) or (Num[y] > 0);
         If Num[y] = Search then begin
            {Circuit passant par y: creation pseudo-noeud et contraction}
            If NewNode = Limit
            Then Error ('Edmonds: place insuffisante pour pseudo-noeuds');
            Inc (NewNode); {Pack,Next: initialises au debut}
            z := y;
            Repeat
               Next[z] := NewNode;
               z := Pred[z];
               z := Pack[z];
            Until z = y;
         End;
      End;
      Found := NewNode > Last;
      If Found then begin {Des pseudo-noeuds ont ete crees}
         {Met a jour Pack pour les anciens noeuds}
         {Ajuste en O(M) les poids des arcs entrant dans un circuit}
         For x := 1 to GraphOrder do
         For k := Head[x] to Head[x+1]-1 do begin
            y    := Succ[k];
            xNow := Next[Pack[x]];
            yNow := Next[Pack[y]];
            If (xNow <> yNow) and (yNow > Last) then begin
               Dec (WNow^[k],WMin[Pack[y]])
            End
         End;
         For x := 1 to Last do Pack[x] := Next[Pack[x]];
         First := Last+1;
         Last  := NewNode;
      End;
End;

//----------------------------------------------------------------------------//
// Edmonds/Recover: recupere l'arborescence dans Pred                    O(M) //
//----------------------------------------------------------------------------//

Procedure Recover;
Var x,y: Node;
Begin
   For x := Last downto GraphOrder+1 do begin
      y  := Extr[x];
      Repeat
         Pred[y] := Orig[x];
         Extr[y] := Extr[x];
         Orig[y] := Orig[x];
         y       := Next[y];
      Until y = Next[y]
   End
End;

Procedure GetWeight;
Var x,y: Node;
    k  : ArcNum;
Begin
      Weight := 0;
      For x := 1 to GraphOrder do
      For k := Head[x] to Head[x+1]-1 do
      If Pred[Succ[k]] = x then Inc (Weight,W[k])
End;

Begin
      New (WNow);
      Initialize;
      Repeat
         ComputeH;
         PackCircuits
      Until not Found;
      Recover;
      GetWeight;
      Dispose (WNow)
End;


Procedure T_GRAPHE_LISTE.PRIM (var W:TArcCost; var Weight:Cost;
                               var NEdge:Node; var Node1,Node2:TNodeInfo);
Var x,y     : Node;
    k       : ArcNum;
    WMin    : Cost;
    Nearest : TNodeInfo;
    LinkCost: TNodeCost;
Begin
      If not Simple then Error ('Prim: graphe non simple');
      {Arbre initial vide}
      Weight     := 0;
      NEdge      := 0;
      {Aucun sommet n'a de voisin dans l'arbre}
      For y := 1 to GraphOrder do begin
         Nearest [y] := 0;
         LinkCost[y] := MaxCost
      End;
      {Part du sommet 1 pour amorcer l'arbre}
      x := 1;
      Repeat
         {Annexe x dans l'arbre}
         Nearest[x] := x;
         {Met … jour plus proches voisins dans l'arbre}
         For k := Head[x] to Head[x+1]-1 do begin
            y := Succ[k];
            If (Nearest[y] <> y) and (W[k] < LinkCost[y]) then begin
               LinkCost[y] := W[k];
               Nearest [y] := x
            End
         End;
         {Cherche prochain sommet x a annexer}
         WMin := MaxCost;
       For y := 2 to GraphOrder do
         If (Nearest[y] <> y) and (LinkCost[y] < WMin) then begin
            WMin := LinkCost[y];
            x    := y;
         End;
         {Stocke l'arete trouvee et compte son poids}
         If WMin < MaxCost then begin
            Inc (Weight,WMin);
            Inc (NEdge);
            Node1[NEdge] := x;
            Node2[NEdge] := Nearest[x];
         End
      Until (WMin = MaxCost) or (NEdge = GraphOrder-1);
End;

Procedure T_GRAPHE_LISTE.EulerChain  (var Found:Boolean;
                          var Walk:TSucc; var Last:ArcNum);
Type PCell  = ^Cell;
     Cell   = Record
                 Vertex: Node;
                 Link  : PCell
              End;

Var  x,y,s  : Node;
     NOdd   : Node;
     k      : ArcNum;
     Next   : THead;
     Adj    : PTSucc;
     Inv    : PTInverse;
     R,F,P,Q: PCell;
     Prev   : PCell;

Function NextArc (x:Node): ArcNum;
Begin
   While (Next[x] < Head[x+1]) and (Adj^[Next[x]] = 0) do Inc (Next[x]);
   NextArc := Next[x]
End;

Begin
      If not Simple then Error ('EulerChain: graphe orient‚');
      {1. Calcul du 1er sommet impair s (s=1 si aucun sommet impair)}
      Found := False;
      NOdd  := 0;
      s     := 1;
      For x := 1 to NX+NY do If Odd(OutDeg(x)) then begin
         Inc (NOdd);
         If NOdd = 1 then s := x else If NOdd > 3 then Exit
      End;
      {2. Reconstruction de G.Succ dans Adj^, avec Inv^}
      New (Adj);
      New (Inv);
      Next := Head;
      For x := 1 to NX+NY do
      For k := Head[x] to Head[x+1]-1 do If x < Succ[k] then begin
         y := Succ[k];
         Adj^[Next[x]] := y;       Adj^[Next[y]] := x;
         Inv^[Next[x]] := Next[y]; Inv^[Next[y]] := Next[x];
         Inc (Next[x]);            Inc (Next[y])
      End;
      {3. Initialise la recherche}
      New (R);
      R^.Vertex := s;
      R^.Link   := Nil;
      Last      := 1;
      P         := R;
      Next      := Head;
      {4. Boucle principale}
      Repeat
         x := P^.Vertex;
         k := NextArc(x);
         If k < Head[x+1] then begin
            F := Nil;
            Repeat
               y := Adj^[k];
               New (Q);
               Q^.Vertex := y;
               If F = Nil then F := Q else Prev^.Link := Q;
               Prev := Q;
               Inc (Last);
               Adj^[k] := 0;
               Adj^[Inv^[k]] := 0;
               k := NextArc(y)
            Until k = Head[y+1];
            Q^.Link := P^.Link;
            P^.Link := F
         End
         Else P := P^.Link
     Until P = Nil;
     Found := Last = (M div 2 + 1);
     {5. Renvoie le parcours dans Walk, detruit les variables dynamiques}
     P := R;
     For k := 1 to Last do begin
        Walk[k] := P^.Vertex;
        Q := P^.Link;
        Dispose (P);
        P := Q
     End;
     Dispose (Adj);
     Dispose (Inv);
End;

Procedure T_GRAPHE_LISTE.EulerPath
                         (var Found:Boolean;
                          var Walk:TSucc; var Last:ArcNum);
Type PCell  = ^Cell;
     Cell   = Record
                 Vertex: Node;
                 Link  : PCell
              End;

Var  x,y,s,t: Node;
     Balance: Integer;
     k      : ArcNum;
     InDeg  : TNodeInfo;
     Next   : THead;
     R,F,P,Q: PCell;
     Prev   : PCell;

Begin
      If Simple then Error ('EulerPath: graphe simple');
      {1. Calcul du sommet s de balance +1 (s=1 si aucun sommet de ce type)}
      Found := False;
      GetIndegrees (InDeg);
      s := 0;
      t := 0;
      For x := 1 to NX+NY do begin
         Balance := OutDeg(x) - InDeg[x];
         Case Balance of
            +1: If s = 0 then s := x else Exit;
            -1: If t = 0 then t := x else Exit;
             0: ;
            else Exit
         End
      End;
      If s = 0 then s := 1;
      {2. Initialise la recherche}
      New (R);
      R^.Vertex := s;
      R^.Link   := Nil;
      Last      := 1;
      P         := R;
      Next      := Head;
      {3. Boucle principale}
      Repeat
         x := P^.Vertex;
         If Next[x] < Head[x+1] then begin
            k := Next[x];
            y := x;
            F := Nil;
            Repeat
               Inc (Next[y]);
               y := Succ[k];
               New (Q);
               Q^.Vertex := y;
               If F = Nil then F := Q else Prev^.Link := Q;
               Prev := Q;
               Inc (Last);
               k := Next[y]
            Until k = Head[y+1];
            Q^.Link := P^.Link;
            P^.Link := F
         End
         Else P := P^.Link
     Until P = Nil;
     Found := Last = M+1;
     {4. Renvoie le parcours dans Walk, detruit les variables dynamiques}
     P := R;
     For k := 1 to Last do begin
        Walk[k] := P^.Vertex;
        Q := P^.Link;
        Dispose (P);
        P := Q
     End;
End;


Procedure T_GRAPHE_LISTE.Postman     (var W:TArcCost;
                       var Walk:TSucc; var Last:ArcNum;     var K:Cost);
Var s,t,x,y    : Node;      {Source & puits pour Busacker + vars de travail}
    SEPos,p    : ArcNum;    {Somme des exces > 0 + var. de travail}
    NC         : Node;      {Nb de composantes f-connexes}
    TC         : TNodeInfo; {Tab. n° de composantes f-connexes (dummy)}
    InDeg      : TNodeInfo; {Tableau des 1/2 degr‚s int‚rieurs}
    E          : Array[Node] of Integer; {Tableau des excŠs}
    NEPos,NENeg: Node;      {Nb d'exces > 0 et < 0}
    C,Z,Phi    : PTArcCost; {Capas, couts et flux pour le reseau}
    F          : Cost;      {Debit du flot (dummy)}
    FlowCost   : Cost;      {Cout total du flot}
    Found      : Boolean;   {Flag parcours eulerien trouve (dummy)}
    R,H        : PTR_T_GRAPHE_LISTE;    {Reseau auxiliaire et graphe eulerien}

//----------------------------------------------------------------------------//
// Postman/GetExcess: calcule les exces et le cout total des arcs.       O(M) //
//----------------------------------------------------------------------------//

Procedure GetExcess;
Var x: Node;
    p: ArcNum;
Begin
   GetInDegrees (InDeg);
   NEPos := 0; NENeg := 0; SEPos := 0; K := 0;
   For x := 1 to NX do begin
      E[x] := InDeg[x] - OutDeg(x);
      If E[x] > 0 then begin
         Inc (NEPos);
         Inc (SEPos,E[x])
      End
      Else If E[x] < 0 then Inc (NENeg);
      For p := Head[x] to Head[x+1]-1 do Inc (K,W[p])
   End;
{
WriteLn ('NEPos: ',NEPos,', NENeg: ',NENeg,', K: ',K)
}
End;

//----------------------------------------------------------------------------//
// Postman/MakeNetwork: construit le reseau auxiliaire R^.               O(M) //
//----------------------------------------------------------------------------//

Procedure MakeNetwork;

Var x: Node;
    p: ArcNum;

Procedure AddArcToR (y:Node; a,b:Cost);
Begin
   With R^ do begin
      If M = MaxArcNum then Error ('DirectedCPP: trop d''arcs');
      Inc (M);
      Succ[M] := y;
      C^[M]   := a;
      Z^[M]   := b
   End
End;

Begin
      R^.NX     := NX+2;
      R^.NY     := 0;
      R^.Simple := False;
      R^.M      := 0;
      s         := NX+1;
      t         := NX+2;
      {Charge R^ en ajoutant un arc (x,t) pour tout x d'exces < 0}
      For x := 1 to NX do begin
         R^.Head[x] := R^.M+1;
         For p := Head[x] to Head[x+1]-1 do AddArcToR (Succ[p],MaxCost,W[p]);
         If E[x] < 0 then AddArcToR (t,-E[x],0);
      End;
      {Relie s aux sommets x d'exces > 0, puis ferme les listes}
      R^.Head[s]   := R^.M+1;
      For x := 1 to NX do If E[x] > 0 then AddArcToR (x,E[x],0);
      R^.Head[t]   := R^.M+1;
      R^.Head[t+1] := R^.M+1;
End;

//----------------------------------------------------------------------------//
// Postman/MakeItEulerian: construit multigraphe eulerien H.             O(M) //
//----------------------------------------------------------------------------//

Procedure MakeItEulerian;

Var x: Node;
    p,q: ArcNum;

Procedure AddArcToH (y:Node);
Begin
   With H^ do begin
      If M = MaxArcNum then Error ('Postman: trop d''arcs');
      Inc (M);
      Succ[M] := y
   End
End;

Begin
   With R^ do begin
      H^.NX     := NX-2;
      H^.NY     := 0;
      H^.Simple := False;
      H^.M      := 0;
      {Charge H^ en ajoutant Phi(x,y) copies de l'arc (x,y)}
      For x := 1 to NX-2 do begin
         H^.Head[x] := H^.M+1;
         For p := Head[x] to Head[x+1]-1 do If Succ[p] <> t then begin
            AddArcToH (Succ[p]); {Garde l'arc d'origine}
            For q := 1 to Phi^[p] do AddArcToH (Succ[p])
         End
      End;
      With H^ do Head[NX+1] := M+1;
   End
End;

Begin

   K:=0;

   If Simple then Error ('Postman: graphe simple');
   {Verifie que G est fortement connexe avec l'algorithme de Tarjan}
   AllSCC (NC,TC);
   If NC > 1 then Error ('Postman: graphe non fortement connexe');
   {Calcul des exces et du cout total K des arcs de G}
   GetExcess;
   {Si G est eulerien, on extrait le parcours, sinon on construit R et H}
   If NEPos = 0
   Then EulerPath (Found,Walk,Last)
   Else begin
      {Alloue variables temporaires}
      New (R); New(C); New(Z); New(Phi); New (H);
      R^:=T_GRAPHE_LISTE.CREATE;
      H^:=T_GRAPHE_LISTE.CREATE;
      {Construit reseau de transport R^}
      MakeNetwork;
      {Cherche un flot de debit somme des exces > 0 et de cout min}
      R^.Busacker (C^,Z^,s,t,SEPos,F,FlowCost,Phi^);
     {Ajoute cout du flot au cout total des arcs de G}
      K := K + FlowCost;
      {Fait un multigraphe eulerien H^ en ajoutant des copies d'arcs}
      MakeItEulerian;
      {Et extrait le parcours eulerien de H^}
      H^.EulerPath (Found,Walk,Last);
      {Detruit les variables dynamiques}
      R^.DESTROY;
      H^.DESTROY;
      Dispose (R); Dispose (C); Dispose (Z); Dispose (Phi); Dispose (H);
   End;
End;

// pour les graphes matriciel

Function T_GRAPHE_MATRICIEL.LastCol : Node;
Begin
   If NY = 0 then
     LastCol := NX
   else
     LastCol := NY;
End;

Function T_GRAPHE_MATRICIEL.MatrixArcs : ArcNum;
Var i,j: Node;
    M  : ArcNum;
Begin
  M := 0;
  For i := 1 to NX do
    For j := 1 to LastCol do
      If A[i,j] <> NoArc then
        Inc(M);
  MatrixArcs := M;
end;

Function T_GRAPHE_MATRICIEL.GraphLoops : Node;
 Var i,Loops: Node;
Begin
 Loops := 0;
 If NY = 0 then
   For i := 1 to NX do
     If A[i,i] <> NoArc then
       Inc(Loops);
 GraphLoops := Loops;
End;

Function T_GRAPHE_MATRICIEL.MatrixIsSimple : Boolean;
Var i,j: Node;
Begin
   If NY > 0
   Then MatrixIsSimple := True
   Else begin
      MatrixIsSimple := False;
      For i := 2 to NX do
        For j := 1 to i-1 do
          If A[i,j] <> A[j,i] then
             Exit;
      For i := 1 to NX do
          If A[i,j] <> NoArc then
            Exit;
      MatrixIsSimple := True
   end;
End;


Procedure T_GRAPHE_MATRICIEL.MakeMatrixSimple ;
Var i,j: Node;
Begin
 If NY = 0 then begin
      For i := 2 to NX do For j := 1 to i-1 do
      If A[i,j] = NoArc
      Then A[i,j] := A[j,i]
      Else begin
         If (A[j,i] <> NoArc) and (A[j,i] <> A[i,j])
         Then Error ('MakeMatrixSimple: conflit de cout ligne'+
                     Edint(i)+', colonne '+EdInt(j));
         A[j,i] := A[i,j]
      End;
      For i := 1 to NX do A[i,i] := NoArc;
      Simple := True
   End
End;

Procedure T_GRAPHE_MATRICIEL.RandMatrix
                 (NX,NY:Node; Simple:Boolean;
                  LoopLess,Layered:Boolean; CMin,CMax,NoArc:Cost;
                  GenProb:Real; RSeed:LongInt; Reset:Boolean);
Var   i,j   : Node;
      DeltaC: Cost;
Begin
   If (NY > 0) and not Simple
   Then Error ('RandMatrix: graphe biparti mais oriente');
   If Layered and Simple
   Then Error ('RandMatrix: graphe en couches mais non oriente');
   If Layered and (not LoopLess)
   Then Error ('RandMatrix: graphe en couches avec boucles permises');
   If Simple and (not LoopLess)
   Then Error ('RandMatrix: graphe simple avec boucles permises');
   If (NoArc >= CMin) and (NoArc <= CMax)
   Then Error ('RandMatrix: RNoArc dans [CMin,CMax]');
   If (GenProb < 0.0) or (GenProb > 1.0)
   Then Error ('RandMatrix: probabilite invalide');
   If CMin > CMax then Error ('RandMatrix: intervalle [CMin,CMax] invalide');
   If Reset then Randomize else RandSeed := RSeed;
   {Initialise C … un graphe sans arcs}
   Self.NX     := NX;
   Self.NY     := NY;
   Self.NoArc  := NoArc;
   Self.Simple := Simple;

   DeltaC := CMax-CMin+1;
   For i := 1 to NX do For j := 1 to LastCol do A[i,j] := NoArc;
   {Si C simple ou en couches, non biparti: on cree la moitie sup de A}
   If (NY = 0) and (Simple or Layered) then begin
     For i := 1 to NX do For j := i+1 to NX do
       If Random < GenProb then A[i,j] := Random(DeltaC)+CMin;
       {Si C est simple, on sym‚trise la matrice A}
       If Simple then For i:=2 to NX do For j:=1 to i-1 do A[i,j]:= A[j,i]
    End
    {Si C biparti, ou non simple, non en couches: on genere A completement}
    Else For i := 1 to NX do
         For j := 1 to LastCol do
         If Random < GenProb Then A[i,j] := Random(DeltaC)+CMin;
    {Vide la diagonale si graphe sans boucle demande}
    If LoopLess and (NY = 0) then For i := 1 to NX do A[i,i] := NoArc
End;


Procedure T_GRAPHE_MATRICIEL.ReadMatrix (FileName:String);
Var i,j  : Node;
    k,l  : Byte;
    Dummy: CostNb;
Begin
      PrepareFile  (FileName,'.MAT');          {Cherche et ouvre le fichier}
      SeekDataLine;                            {Cherche 1ere ligne de donnees}
      ReadHeader   (NX,NY,Simple,Dummy,NoArc); {Lit NX, NY, etc...}
      For i := 1 to NX do begin                {Pour chaque sommet i de C}
         SeekDataLine;                         {Cherche ligne i de C}
         If EOF(WFile) and (i < NX)
         Then Error ('ReadMatrix: matrice incomplete');
         {Si un n° de ligne est donne, verifie qu'il est egal a i}
         k := 1;
         l := Pos (':',WLine);
         If (l > 0) and (ReadInt(k,0,MaxNode) <> i)
         Then Error ('ReadMatrix: indice ligne invalide,ligne fichier:'+LNE);
         k := l+1;
         {Recupere les valeurs de la ligne}
         For j := 1 to LastCol do begin
            A[i,j] := ReadInt(k,-MaxCost,MaxCost);
            If (i > j) and (NY = 0) and Simple and (A[i,j] <> A[j,i])
            Then Error('ReadMatrix: asymetrie en i='+Edint(i)+',j='+EdInt(j))
         End;
         If (NY = 0) and Simple and (A[i,i] <> NoArc)
         Then Error('ReadMatrix: boucle dans graphe simple, i='+EdInt(i))
      End;
      Close (WFile)   {Ferme fichier. Residu ignore}
End;



Procedure T_GRAPHE_MATRICIEL.WriteMatrix
                          (var F:Text; Msg:String;
                           Width:Byte; Break:Integer);
Var i,j,Lastj   : Node;
    RowNumLength: Byte;
    Line        : Integer;
    CostLength  : Byte;
    M           : ArcNum;
Begin
      WriteLn (F);
      {Ecrit la ligne de commentaire si elle est donnee}
      If Msg <> '' then
      If Msg[1] <> '*' then WriteLn (F,'* ',Msg) else WriteLn (F,Msg);
      M := MatrixArcs;
      Write (F,'NX=',NX,', NY=',NY,', NOARC=',NoArc,', M=',M);
      If Simple
      Then If NY > 0
           Then WriteLn (F,', BIPARTI')
           Else WriteLn (F,', SIMPLE')
      Else WriteLn (F);
      If NX = 0
      Then WriteLn (F,'* Graphe vide!')
      Else If M=0 then WriteLn (F,'* Graphe n''ayant que des sommets isoles!')
      Else begin
         {Calcule largeur des champs pour bien aligner les colonnes}
         RowNumLength := Length(EdInt(NX));
         If NY = 0
         Then CostLength := RowNumLength
         Else CostLength := Length(EdInt(NX+NY));
         For i:=1 to NX do For j:=1 to LastCol do
         CostLength := Max (CostLength,Length(EdInt(A[i,j])));
         If (CostLength+1)*LastCol > 252-RowNumLength
         Then Error ('WriteMatrix: lignes de matrice de + de 254 caracteres');
         Lastj := Min (LastCol,(Width-2-RowNumLength) div (CostLength+1));
         If Lastj < LastCol
         Then WriteLn ('* WIDTH trop petit: lignes coupees');
         Line := 3 + Ord(Msg<>'') + Ord(Lastj < LastCol);
         Write (F,'*',':':RowNumLength+1);
         If NY = 0 then begin
            For j:=1 to Lastj-1 do Write (F,j:CostLength,',');
            WriteLn (F,Lastj:CostLength)
         End
         Else begin
            For j:=1 to Lastj-1 do Write (F,j+NX:CostLength,',');
            WriteLn (F,Lastj+NX:CostLength)
         End;
         Write   (F,'*',StringOf(RowNumLength,'-'),':');
         WriteLn (F,StringOf((CostLength+1)*Lastj-1,'-'));
         For i := 1 to NX do begin
            CountLine (Line,Break);
            Write (F,i:RowNumLength,' :');
            For j := 1 to Lastj-1 do Write (F,A[i,j]:CostLength,',');
            WriteLn (F,A[i,Lastj]:CostLength)
         End
      End;
      If Break > 0 then Pause ('Fin de listing. Click pour continuer...')
End;



Procedure T_GRAPHE_MATRICIEL.AFFMatrix
                          (MON_MEMO:TMEMO; Msg:String;
                           Width:Byte; Break:Integer);
Var i,j,Lastj   : Node;
    RowNumLength: Byte;
    Line        : Integer;
    CostLength  : Byte;
    M           : ArcNum;
    ll : string;
    ss : string;
Begin
      MON_MEMO.Lines.Add ('');
      {Ecrit la ligne de commentaire si elle est donnee}
      If Msg <> '' then
      If Msg[1] <> '*' then
         MON_MEMO.Lines.Add ('* '+Msg)
      else
         MON_MEMO.Lines.Add (Msg);
      M := MatrixArcs;
      ll:='NX='
                           + IntTOStr(NX)
                           + ', NY='
                           + IntTostr(NY)
                           + ', NOARC='
                           + IntToStr(NoArc)
                           + ', M='
                           +IntToStr(M);
      If Simple
      Then If NY > 0
           Then ll:=ll+', BIPARTI'
           Else ll:=ll+ (', SIMPLE')
      Else ll:=ll+'';
      MON_MEMO.Lines.Add(ll);
      If NX = 0
      Then MON_MEMO.Lines.Add ('* Graphe vide!')
      Else If M=0 then
               MON_MEMO.Lines.Add ('* Graphe n''ayant que des sommets isoles!')
      Else begin
         {Calcule largeur des champs pour bien aligner les colonnes}
         RowNumLength := Length(EdInt(NX));
         If NY = 0
         Then CostLength := RowNumLength
         Else CostLength := Length(EdInt(NX+NY));
         For i:=1 to NX do For j:=1 to LastCol do
         CostLength := Max (CostLength,Length(EdInt(A[i,j])));
         If (CostLength+1)*LastCol > 252-RowNumLength
         Then Error ('WriteMatrix: lignes de matrice de + de 254 caractdres');
         Lastj := Min (LastCol,(Width-2-RowNumLength) div (CostLength+1));
         If Lastj < LastCol
         Then WriteLn ('* WIDTH trop petit: lignes coupees');
         Line := 3 + Ord(Msg<>'') + Ord(Lastj < LastCol);
         MON_MEMO.Lines.Add ('* :'+IntToStr(RowNumLength+1));
         If NY = 0 then begin
            ll := '';
            For j:=1 to Lastj-1 do
               begin
               ss:= IntToStr(j);
               MidStr(ss,1,CostLength);
               ll:=ll+ss+', ';
               end;
            ss:= IntToStr(Lastj);
            MidStr(ss,1,CostLength);
            ll:=ll+ss;
            MON_MEMO.Lines.Add (ll);
            ll:='';
         End
         Else begin
            ll:='';
            For j:=1 to Lastj-1 do
               begin
               ss:= IntToStr(j+NX);
               MidStr(ss,1,CostLength);
               ll:=ll+ss+', ';
               end;
            ss:= IntToStr(Lastj+NX);
            MidStr(ss,1,CostLength);
            ll:=ll+ss;
            MON_MEMO.Lines.Add (ll)
         End;
         ll:=   '*'+StringOf(RowNumLength,'-')+':';
         ll:=ll+StringOf((CostLength+1)*Lastj-1,'-');
         MON_MEMO.Lines.Add (ll);
         ll:='';
         For i := 1 to NX do begin
            CountLine (Line,Break);
            ss:= IntToStr(i); //+' ';
            MidStr(ss,1,RowNumLength);
            ll:=ll+ss+' : ';
            For j := 1 to Lastj-1 do
               begin
                   ss:= IntToStr(A[i,j]);
                   MidStr(ss,1,CostLength);
                   ll:=ll+ss+' ';
               end;
            ss:= IntToStr(A[i,Lastj]);
            MidStr(ss,1,CostLength);
            ll:=ll+ss;
            MON_MEMO.Lines.Add (ll);
            ll:='';
         End
      End;
      If Break > 0 then Pause ('Fin de listing. Click pour continuer...')
End;


function T_GRAPHE_MATRICIEL.Pack (W:PTArcCost): PTR_T_GRAPHE_LISTE;
Var i,j : Node;
   G    : PTR_T_GRAPHE_LISTE;

Begin

    // creation de la zone memoire
    New(G);
    // initialisation de l'instance
    G^ := T_GRAPHE_LISTE.CREATE;

    G^.NX     := NX;
    G^.NY     := NY;
    G^.Simple := Simple;
    G^.M      := 0;
      {Compresse une liste de successeurs par ligne de matrice}
    For i := 1 to NX do
      begin
       G^.p_Head[i] := G.M+1;
         For j := 1 to LastCol do if A[i,j] <> NoArc then begin
            Inc (G.M);
            If NY = 0 then G.p_Succ[G.M] := j else G.p_Succ[G.M] := j+NX;
            {NB: on ne garde les couts que si W <> Nil est fourni}
            If W <> Nil then W^[G.M] := A[i,j]
         End
      End;
      {Construit les arcs symetriques de Y dans X si G est biparti}
      For j := 1 to NY do begin
         G.p_Head[j+NX] := G.M+1;
         For i := 1 to NX do if A[i,j] <> NoArc then begin
            Inc (G.M);
            G.p_Succ[G.M] := i;
            If W <> Nil then W^[G.M] := A[i,j]
         End
      End;
      G.p_Head[NX+NY+1] := G.M+1;

   Pack := G;
End;


Function T_GRAPHE_MATRICIEL.LIRE_A (i:Node; j:Node):  Cost;
begin
 LIRE_A:=A[i,j];
end;

Procedure T_GRAPHE_MATRICIEL.ECRIRE_A (i:Node; j:Node; x :Cost);
begin
 A[i,j]:=x;
end;

Function T_GRAPHE_MATRICIEL.LIRE_NoArc :  Cost;
begin
  LIRE_NoArc:=NoArc;
end;

procedure T_GRAPHE_MATRICIEL.ECRIRE_NoArc (x :Cost);
begin
  NoArc:=x;
end;

//Procedure T_GRAPHE_LISTE.AddSuc
//               (y:Node;
//                Capa,Price:Cost;
//                var C : TArcCost);
//Begin
//      Succ[M] :=     y;
//      C[M]    := Capa ;
//      W[M]    := Price;
//      Inc(M);
//End;

Procedure T_GRAPHE_LISTE.MakeNetwork
                      (var Mat:T_GRAPHE_MATRICIEL;
                       var C,W:TArcCost; var s,t:Node);
Var x,y: Node;

Procedure AddSuc
               (y:Node;
                Capa,Price:Cost;
                var C : TArcCost);
Begin
      Succ[M] :=     y;
      C[M]    := Capa ;
      W[M]    := Price;
      Inc(M);
End;


Begin
       M      := 1;
       s      := Mat.NX + Mat.NY + 1;
       t      := Mat.NX + Mat.NY + 2;
       NX     := t;
       NY     := 0;
       Simple := False;
       For x := 1 to Mat.NX do begin
          Head[x] := M;
          With Mat do
          For y := 1 to NY do
          If Mat.A[x,y] <> NoArc then
              AddSuc (NX+y,MaxCost,
                      MAT.A[x,y],
                      C)
       End;
       For x := Mat.NX+1 to t-2 do begin
          Head[x] := M;
          AddSuc (t,1,0,C)
       End;
       Head[s] := M;
       For y := 1 to Mat.NX do AddSuc (y,1,0,C);
       Head[t] := M;
       Head[t+1] := M;
       Dec (M);
End;


Procedure T_GRAPHE_MATRICIEL.Assignment
                     (var Card:Node; var K:Cost;
                      var Mate:TNodeInfo);

Var R    : PTR_T_GRAPHE_LISTE;    {Reseau de transport pour Busacker}
    C    : PTArcCost; {Capacites sur les arcs}
    W    : PTArcCost; {Couts sur les arcs}
    F    : Cost;      {Debit du flot}
    PHI  : PTArcCost; {Flots sur les arcs}
    s,t,x: Node;      {Source et puits}
    p    : ArcNum;



Begin
      If NY = 0 then Error ('Assignment: graphe non biparti');
      New (R);  R^:=T_GRAPHE_LISTE.CREATE;
      New (C);
      New (W);
      New (Phi);
      R.MakeNetwork (Self,C^,W^,s,t);
      R.Busacker (C^,W^,s,t,Min(NX,NY),F,K,Phi^);
      Card := F;
      For x := 1 to NX+NY do Mate[x] := 0;
      For x := 1 to NX do With R^ do begin
         p := Head[x];
         While (p < Head[x+1]) and (Phi^[p] = 0) do Inc (p);
         If p < Head[x+1] then begin
            Mate[x]       := Succ[p];
            Mate[Succ[p]] := x
         End
      End;
      Dispose (R); Dispose (C); Dispose (W); Dispose (Phi);
End;



Procedure T_GRAPHE_MATRICIEL.NearestNeib (var Cycle:TNodeInfo; var C:Cost);
Var Free  : TNodeBool;
    Last  : Node;
    i,imin: Node;
    DMin  : Cost;
Begin
      C        := 0;
      Cycle[1] := 1;
      For i    := 2 to NX do Free[i] := True;
      For Last := 1 to NX-1 do begin
         DMin  := MaxCost;
         For i := 2 to NX do
         If Free[i] and (A[Cycle[Last],i] < DMin) then begin
            DMin := A[Cycle[Last],i];
            imin := i
         End;
         Free[imin]    := False;
         Cycle[Last+1] := imin;
         C             := C + DMin
      End;
      C := C + A[imin,1];
      Cycle[NX+1] := 1;
End;

Procedure T_GRAPHE_MATRICIEL.NearestIns  (var Cycle:TNodeInfo; var C:Cost);
Var Next  : TNodeInfo;
    DMin,D: Cost;
    i     : Node;
    j,jmin: Node;
    k,Iter: Node;
    Dist  : TNodeCost;
Begin
      C        := 0;
      Next[1] :=  1;
      For i := 2 to NX do begin
         Next[i] := 0;
         Dist[i] := A[i,1]
      End;
      For Iter := 1 to NX-1 do begin
         DMin := MaxCost;
         For j := 2 to NX do If (Next[j] = 0) and (Dist[j] < DMin) then begin
            DMin := Dist[j];
            i    := j
         End;
         DMin := MaxCost;
         j := 1;
         Repeat
            k := Next[j];
            D := A[j,i]+A[i,k]-A[j,k];
            If D < DMin then begin
               DMin := D;
               jmin := j
            End;
            j := k
         Until j = 1;
         C          := C + DMin;
         Next[i]    := Next[jmin];
         Next[jmin] := i;
         For j := 2 to NX do
         If Next[j] = 0 then Dist[j] := Min (Dist[j],A[j,i])
      End;
      j := 1;
      For Iter := 1 to NX+1 do begin
         Cycle[Iter] := j;
         j := Next[j]
      End
End;

Procedure T_GRAPHE_MATRICIEL.FarthestIns (var Cycle:TNodeInfo; var C:Cost);
Var Next  : TNodeInfo;
    DMin,D: Cost;
    DMax  : Cost;
    i     : Node;
    j,jmin: Node;
    k,Iter: Node;
    Dist  : TNodeCost;
Begin
      C        := 0;
      Next[1] :=  1;
      For i := 2 to NX do begin
         Next[i] := 0;
         Dist[i] := A[i,1]
      End;
      For Iter := 1 to NX-1 do begin
         DMax := 0;
         For j := 2 to NX do If (Next[j] = 0) and (Dist[j] > DMax) then begin
            DMax := Dist[j];
            i    := j
         End;
         DMin := MaxCost;
         j := 1;
         Repeat
            k := Next[j];
            D := A[j,i] + A[i,k] - A[j,k];
            If D < DMin then begin
               DMin := D;
               jmin := j
            End;
            j := k
         Until j = 1;
         C          := C + DMin;
         Next[i]    := Next[jmin];
         Next[jmin] := i;
         For j := 2 to NX do
         If Next[j] = 0 then Dist[j] := Max (Dist[j],A[j,i])
      End;
      j := 1;
      For Iter := 1 to NX+1 do begin
         Cycle[Iter] := j;
         j           := Next[j]
      End;
End;

Procedure T_GRAPHE_MATRICIEL.BestIns (var Cycle:TNodeInfo; var C:Cost);
Var Next  : TNodeInfo;
    DMin,D: Cost;
    i,imin: Node;
    j,jmin: Node;
    k,Iter: Node;
Begin
      C        := 0;
      Next[1] :=  1;
      For i := 2 to NX do Next[i] := 0;
      For Iter := 1 to NX-1 do begin
         DMin := MaxCost;
         For i := 1 to NX do If Next[i] = 0 then begin
            j := 1;
            Repeat
               k := Next[j];
               D := A[j,i] + A[i,k] - A[j,k];
               If D < DMin then begin
                  DMin := D;
                  imin := i;
                  jmin := j
               End;
               j := k
            Until j = 1;
         End;
         C          := C + DMin;
         Next[imin] := Next[jmin];
         Next[jmin] := imin
      End;
      j := 1;
      For Iter := 1 to NX+1 do begin
         Cycle[Iter] := j;
         j := Next[j]
      end;
End;

Procedure T_GRAPHE_MATRICIEL.TwoOpt      (var Cycle:TNodeInfo; var C:Cost);
Var i,j,k,l,i1,i2,i1min,i2min,i2max: Node;
    D,DMin: Cost;
Begin
      If not Simple then Error ('TwoOpt: graphe orient‚');
      NearestNeib (Cycle,C);
      Repeat
         DMin := 0;
         For i1 := 1 to NX-2 do begin
            If i1 = 1 then i2max := NX-1 else i2max := NX;
            For i2 := i1+2 to i2max do begin
               i := Cycle[i1]; j := Cycle[i1+1];
               k := Cycle[i2]; l := Cycle[i2+1];
               D := A[i,k]+A[j,l]-A[i,j]-A[k,l];
               If D < DMin then begin
                  DMin  := D;
                  i1min := i1;
                  i2min := i2
               End
            End
         End;
         If DMin < 0 then begin
            C  := C+DMin;
            i1 := i1min+1;
            i2 := i2min;
            Repeat
               i := Cycle[i1]; Cycle[i1] := Cycle[i2]; Cycle[i2] := i;
               Inc (i1); Dec (i2)
            Until i1 >= i2
         End
      Until DMin = 0;
End;


Procedure T_GRAPHE_MATRICIEL.SimAn       (T0,Coef,Eps:Real; NPass:Integer;
                       RSeed:LongInt;  Reset:Boolean; var Cycle:TNodeInfo;
                       var C:Cost;     var NIter:LongInt);
Var i,j,k,l,i1,i2: Node;
    D            : Cost;
    BestCycle    : TNodeInfo;
    BestCost     : Cost;
    T            : Real;
    Pass         : Byte;
START:Real;
Begin
      If not Simple then Error ('SimAn: graphe oriente');
      If Reset then Randomize else RandSeed := RSeed;
      {Part du cycle trivial (1,2,...,N,1)}
      C        := 0;
      Cycle[1] := 1;
      For i := 2 to NX do begin
         Cycle[i] := i;
         C        := C+A[i-1,i]
      End;
      Cycle[NX+1] := 1;
      C           := C+A[NX,1];
      BestCycle   := Cycle;
      BestCost    := C;
      NIter       := 0;
      Start       := T0;
      For Pass := 0 to NPass-1 do begin {Passes de refroidissement}
         T     := Start; {Temperature initiale de la passe}
         Start := Start / 2.0;
         Repeat
            Inc (NIter);
            i1 := 1+Random(NX-2);       {Calcul d'une transfo 2-OPT}
            If i1 = 1
            Then i2 := i1+2+Random(NX-i1-2)
            Else i2 := i1+2+Random(NX-i1-1);
            i := Cycle[i1]; j := Cycle[i1+1];
            k := Cycle[i2]; l := Cycle[i2+1];
            D := A[i,k]+A[j,l]-A[i,j]-A[k,l];
            If (D <= 0) or ((D/T <= 32.0) and (Random < Exp(-D/T)))
            Then begin
               C  := C + D;
               i1 := i1 + 1;
               Repeat
                  i:=Cycle[i1]; Cycle[i1]:=Cycle[i2]; Cycle[i2]:=i;
                  Inc (i1); Dec (i2)
               Until i1 >= i2;
               If C < BestCost then begin
                  BestCost  := C;
                  BestCycle := Cycle
               End
            End;
            T := Coef * T
         Until T < Eps
      End;
      Cycle := BestCycle;
      C     := BestCost;
End;

Procedure T_GRAPHE_MATRICIEL.Tabu        (NT,NItMax:Integer;
                       NItWI:Integer;  var Cycle:TNodeInfo;
                       var C:Cost;     var NIter:LongInt);
Const NTMax    = 51;
Type  TabuSub  = 0..NTMax;
      TabuMove = Array[1..4] of Node;
      TabuList = Array[TabuSub] of TabuMove;

Var   i,j,k,l,i1,i2,i1min,i2min,i2max: Node;
      D,DMin   : Cost;
      BestCycle: TNodeInfo;
      BestC    : Cost;
      Last     : TabuSub;
      T        : TabuList;
      Z,ZMin   : TabuMove;
      NotBetter: LongInt;

Procedure ResetTabuList;
Var k: TabuSub;
    p: Byte;
Begin
   For k := 0 to NT do For p := 1 to 4 do T[k,p] := 0;
   Last  := 0;
End;

Procedure CodeMove (a,b,c,d:Node; var Z:TabuMove);
Var x:Node;
Begin
   If a < b then begin Z[1] := a; Z[2] := b End
            else begin Z[1] := b; Z[2] := a End;
   If c < d then begin Z[3] := c; Z[4] := d End
            else begin Z[3] := d; Z[4] := c End;
   If Z[1] > Z[3] then begin
      x := Z[1]; Z[1] := Z[3]; Z[3] := x;
      x := Z[2]; Z[2] := Z[4]; Z[4] := x
   End
End;

Function MoveIsTabu (Z:TabuMove): Boolean;
Var k: TabuSub;
Begin
   k := 1;
   While (k <= NT) and
   ((Z[1]<>T[k,1]) or (Z[2]<>T[k,2]) or (Z[3]<>T[k,3]) or (Z[4]<>T[k,4]))
   Do Inc (k);
   MoveIsTabu := k <= NT
End;

Procedure StoreMove (Z:TabuMove);
Begin
   If Last = NT then Last := 1 else Inc(Last);
   T[Last] := Z
End;

Begin
      If not Simple then Error ('Tabu: graphe oriente');
      If NT > NTMax then Error ('Tabu: liste taboue trop longue');
      NearestNeib (Cycle,C);
      BestCycle := Cycle;
      BestC     := C;
      NIter     := 0;
      NotBetter := 0;
      ResetTabuList;
      Repeat
         DMin   := MaxCost;
         For i1 := 1 to NX-2 do begin
            If i1 = 1 then i2max := NX-1 else i2max := NX;
            For i2 := i1+2 to i2max do begin
               i := Cycle[i1]; j := Cycle[i1+1];
               k := Cycle[i2]; l := Cycle[i2+1];
               D := A[i,k]+A[j,l]-A[i,j]-A[k,l];
               CodeMove (i,k,j,l,Z);
               If (D < DMin) and (not MoveIsTabu(Z)) then begin
                  DMin  := D;
                  i1min := i1;
                  i2min := i2;
                  ZMin  := Z
               End
            End
         End;
         If DMin < MaxCost then begin
            Inc (NIter);
            StoreMove (ZMin);
            C  := C+DMin;
            i1 := i1min+1;
            i2 := i2min;
            Repeat
               i := Cycle[i1]; Cycle[i1] := Cycle[i2]; Cycle[i2] := i;
               Inc (i1); Dec (i2)
            Until i1 >= i2;
            If C < BestC then begin
               BestC     := C;
               BestCycle := Cycle;
               NotBetter := 0
            End
            Else Inc (NotBetter)
         End
      Until (NIter = NITMax) or (NotBetter = NItWI) or (DMin = MaxCost);
      Cycle := BestCycle;
      C     := BestC;
End;

Procedure T_GRAPHE_LISTE.Backtrack
                    (MaxDown:LongInt;
                     var Color:TNodeInfo;
                     var NC:Node; var NDown:LongInt);
Type  StackNode = Record
                     p: Node;
                     U: Set of NodeEns
                  End;
Var   S         : Array[Node] of StackNode;
      Used,Top  : Node;
      i,j,jmax,z: Node;
      q         : ArcNum;
      Down      : Boolean;
      BestColor : TNodeInfo;
      V         : TNodeInfo;
      Success   : Boolean;
      TDown     : LongInt;

      F: Text;
Begin
   //With G do begin

If Trace then begin
Assign (F,'CON');
Rewrite (F);
End;
      LFOrder (V);
      Used := 1;
      Top  := 1;
      For i := 1 to NX+NY do If i = V[1] then Color[i] := 1 else Color[i] := 0;
      NC := NX+NY+1;
      Down := True;
      NDown := 1;
      If Trace then WriteLn (F);
      Repeat
         If Down then begin
            Inc (NDown);
            S[Top].p := Used;
            Inc (Top);
            If Trace
            Then Write (F,'Tree node #',NDown,', V',Top,' = ',V[Top],', U:');
            {Compute S[Top].U,possible colors for V[Top]}
            jmax := 1;
            For j := 2 to Top-1 do jmax := Max(jmax,Color[V[j]]);
            Inc (jmax);
            jmax := Min (jmax,Min(Top,1+OutDeg(V[Top])));
            jmax := Min (jmax,NC-1);
            S[Top].U := [1..jmax];
            For q := Head[V[Top]] to Head[V[Top]+1]-1
            Do S[Top].U := S[Top].U - [Color[Succ[q]]];
            If Trace then If S[Top].U = [] then Write (F,'empty. ')
                                           else Write (F,'. ')
         End;
         If S[Top].U = [] then begin {Backtrack}
            Color[V[Top]] := 0;
            Dec (Top);
            Used := S[Top].p;
            Down := False;
            If Trace then begin
               WriteLn (F,'BACKTRACK to V',Top);
               Pause ('')
            End
         End
         Else With S[Top] do begin
            j := 1;
            While (j <= NX+NY) and not (j in U) do Inc (j);
            If j > NX+NY then Error ('BACKTRACK: cannot find j!');
            U := U - [j];
            Color[V[Top]] := j;
            If Trace then Write (F,'Give color ',j,'.');
            Used := Max(j,Used);
            If Top < NX+NY
            Then Begin
               Down := True;
               If Trace then begin WriteLn (F); Pause ('') End
            End
            Else begin {New,better coloring}
               If Trace then begin
                  WriteLn (Used,' couleurs trouvees au noeud #',NDown);
                  WriteLn (F,' ',Used,'-coloring found!');
                  Pause ('')
               End;
               BestColor := Color;
               i := 1;
               While (i <= NX+NY) and (Color[V[i]] <> Used) do Inc (i);
               If i > NX+NY then Error ('BACKTRACK: cannot find i!');
               {Remove colors Used to k-1 from S1..Si-1}
               For j := 2 to i-1 do begin
                  If Trace and (S[j].U*[Used..NC-1]<>[]) then begin
                     Write (F,'Colors removed in S[',j,'].U:');
                     For z := 1 to NX+NY do If z in (S[j].U*[Used..NC-1])
                     then Write (F,z);WriteLn (F);
                  End;
                  S[j].U := S[j].U - [Used..NC-1];
               End;
               {Uncolor V[i] to V[Top]}
               For j := i+1 to Top do Color[V[j]] := 0;
               NC   := Used;
               Used := NC-1;
               Top  := i-1;
               Down := False
            End
         End
      Until (Top = 1) or (NDown >= MaxDown);
      Color   := BestColor;
      Success := (Top = 1);
      Inc (TDown,NDown);
 //  End;
{Close (F)}
End;

Function  T_GRAPHE_LISTE.BS1: Node;
begin
   BS1 := 1 + MaxOutDeg;
end;

Function  T_GRAPHE_LISTE.BS2: Node;
Var T,x: Node;
    V  : TNodeInfo;
begin
   LFOrder (V);
   T := 0;
   For x := 1 to GraphOrder do
       T := Max(T,Min(x,OutDeg(V[x])+1));
   BS2 := T
end;


Function  T_GRAPHE_LISTE.BS3: Node;
Var T,i : Node;
    Di,V: TNodeInfo;
Begin
   SLOrder (V,Di);
   T := 0;
   For i := 1 to GraphOrder do
      T := Max (T,Di[V[i]]);
   BS3 := 1 + T
End;

Procedure T_GRAPHE_LISTE.Check (Color:TNodeInfo; NC:Node);
Var Used: TNodeBool;
    x   : Node;
    k   : ArcNum;
Begin
      For x := 1 to NX+NY do Used[x] := False;
      For x := 1 to NX+NY do begin
         If (Color[x] < 1) or (Color[x] > NC)
         Then Error ('CheckColors: n° couleur invalide');
         Used[Color[x]] := True;
         For k := Head[x] to Head[x+1]-1 do
         If Color[x] = Color[Succ[k]]
         Then Error ('CheckColors: conflit de couleurs');
      End;
      For x := 1 to NC do If not Used[x]
      Then Error ('CheckColors: NC invalide')
End;

Procedure T_GRAPHE_LISTE.DSatur (var Color:TNodeInfo; var NC:Node);
Var i,j,Iter: Node;
    BestNode: Node;
    p       : ArcNum;
    DS,DSM  : Node;     {Degre de saturation: nb de couleurs parmi les sucs}
    BestCol : Node;
    Used    : TNodeBool;
Begin
      NC := 1;
      For i := 1 to GraphOrder do Color[i] := 0;
      j     := 1;
      For i := 2 to GraphOrder do
      If OutDeg(i) > OutDeg(j) then j := i;
      Color[j] := 1;
      For Iter := 1 to GraphOrder - 1 do begin
         DSM := 0; {Calc. BestNode de DS max (et max deg si ex-aequo)}
         For i := 1 to GraphOrder do If Color[i] = 0 then begin
            For j := 1 to NC+1 do Used[j] := False; {NC coul. max utilisee}
            For p := Head[i] to Head[i+1]-1 do Used[Color[Succ[p]]] := True;
            DS := 0;
            For j := 1 to NC do If Used[j] then Inc (DS); {0 <= DS <= NC}
            If (DSM = 0) or (DS > DSM) or
               ((DS = DSM) and (OutDeg(i) > OutDeg(BestNode))) then begin
               DSM      := DS;
               BestNode := i;
               BestCol  := 0;
               Repeat Inc(BestCol) Until not Used[BestCol]
            End
         End;
         Color[BestNode] := BestCol;
         NC              := Max(NC,BestCol)
      End
End;

Procedure T_GRAPHE_LISTE.FFS ( var Color:TNodeInfo; var NC:Node);
Var x: Node;
    V: TNodeInfo;
Begin
   For x := 1 to GraphOrder do V[x] := x;
   SeqColor (V,Color,NC)
End;

Procedure T_GRAPHE_LISTE.LFS (var Color:TNodeInfo; var NC:Node);
Var V: TNodeInfo;
Begin
   LFOrder (V);
   SeqColor (V,Color,NC)
End;


Procedure T_GRAPHE_LISTE.SeqColor (var V,Color:TNodeInfo; var NC:Node);
Var Used   : TNodeBool; {Used[i] vrai ssi couleur i utilis‚e par un voisin}
   { Num,}i,j: Node;
    p      : ArcNum;
Begin
      NC := 0;
      For j := 1 to NX+NY do Color[j] := 0;
      For i := 1 to NX+NY do begin
         {Construit tableau Used}
         For j := 1 to NC+1 do Used[j] := False;
         For p := Head[V[i]] to Head[V[i]+1]-1 do begin
            j := Succ[p];
            If Color[j] > 0 then Used[Color[j]] := True
         End;
         {Cherche plus petite couleur libre j}
         j := 0;
         Repeat Inc(j) Until not Used[j];
         Color[V[i]] := j;
         NC := Max(NC,j)
      End
End;

Procedure T_GRAPHE_LISTE.Local (var Color:TNodeInfo; var NC:Node);
Var i,j     : Node;
    V,NewV  : TNodeInfo;
    NewColor: TNodeInfo;
    NewNC   : Node;
   { Di      : TNodeInfo; }
Begin
      {Ordre initial et coloration associee}
      {Ordre FF}
{     For i := 1 to NX+NY do V[i] := i;}
      {Ordre LF (meilleur)}
      LFOrder (V);
      {Ordre SL (le meilleur)}
{     SLOrder (G,V,Di);}
      SeqColor (V,Color,NC);
      If Trace then begin WriteLn; WriteLn ('NC initial : ',NC) end;
      {Boucle d'amdliorations successives}
      Repeat
         {On stoppe a la 1ere amelioration trouvee}
         i     := 0;
         Repeat
            Inc (i);
            j := i;
            Repeat
               Inc (j);
               {Permute Vi et Vj}
               NewV     := V;
               NewV[i]  := V[j];
               NewV[j]  := V[i];
               SeqColor (NewV,NewColor,NewNC);
            Until (j >= NX+NY) or (NewNC < NC)
         Until (i >= NX+NY) or (NewNC < NC);
         {Change de liste si meilleure liste trouvee}
         If NewNC < NC then begin

            V     := NewV;
            Color := NewColor;
            NC    := NewNC;
            If Trace then WriteLn ('Meilleur NC: ',NC);
         End
      Until NewNC >= NC
End;

Procedure T_GRAPHE_LISTE.SimAn (memo1 : Tmemo; T0,Coef,Eps:Real; NPass:Integer;
                     RSeed:LongInt; Reset:Boolean;
                     var Color:TNodeInfo; var NC:Node; var NIter:LongInt);
Var i,j {,x}    : Node;
    V,NewV   : TNodeInfo;
    NewColor : TNodeInfo;
    BestColor: TNodeInfo;
    NewNC    : Node;
    BestNC   : Node;
    D        : Integer;
    Start,T  : Real;
    Pass     : Integer;
Begin
      If Reset then Randomize else RandSeed := RSeed;
      {Ordre initial FF et coloration associee}
      For i := 1 to NX+NY do V[i] := i;
      SeqColor (V,Color,NC);
      If Trace then Write ('NC initial: ',NC);
      BestColor := Color;
      BestNC    := NC;
      NIter     := 0;
      Start     := T0;
      For Pass := 0 to NPass-1 do begin {Passes de refroidissement}
         T := Start;
         Start := Start / 2.0;
         If Trace then begin WriteLn; WriteLn ('Passe:',Pass,',T:',T:8:2); End;
         Repeat
            Inc (NIter);
            {Effectue une permutation aleatoire dans la liste}
            i := 1+Random(NX+NY);
            Repeat j := 1+Random(NX+NY) Until i <> j;
            NewV    := V;
            NewV[i] := V[j];
            NewV[j] := V[i];
            SeqColor (NewV,NewColor,NewNC);
            D := NewNC-NC;
            If (D <= 0) or ((D/T <= 32.0) and (Random < Exp(-D/T)))
            Then begin
               V     := NewV;
               NC    := NewNC;
               Color := NewColor;
               If NC < BestNC then begin
                  If Trace then begin
                     //GotoXY (1,WhereY);
                     memo1.lines.add  ('Meilleur NC:');
                     memo1.Lines.add (inttostr(NC));
                  End;
                  BestNC    := NC;
                  BestColor := Color
               End
            End;
            T := Coef * T
         Until T < Eps;
      End;
      NC    := BestNC;
      Color := BestColor
End;

Procedure T_GRAPHE_LISTE.SLS (var Color:TNodeInfo; var NC:Node);
Var //i   : Node;
    V,Di: TNodeInfo;
Begin
   SLOrder (V,Di);
   SeqColor (V,Color,NC)
End;


Procedure T_GRAPHE_LISTE.TabuCol (NT,NItMax:Integer;
                   var Color:TNodeInfo; var NC:Node; var NIter:LongInt);

CONST NTMax    = 101;
TYPE  TabClash = Array[Node] of TNodeInfo;
      TabuSub  = 0..NTMax;
VAR   BestColor: TNodeInfo;        {Meilleure coloration trouvee}
      K        : Node;             {Nombre de couleurs a tenter}
      NClash   : ArcNum;           {Nb de conflits dans "coloration" actuelle}
      TClash   : TabClash;         {Table de conflits}
      TabuList : Record            {Liste taboue}
                    Last: TabuSub;
                    OldNode,OldColor: Array[TabuSub] of Node
                 End;

//----------------------------------------------------------------------------//
// TabuCol/PutKColors: place arbitrairement k couleurs sur les sommets        //
//----------------------------------------------------------------------------//

Procedure PutKColors;
Var x: Node;
Begin
   For x := 1 to GraphOrder do Color[x] := 1 + x mod K
End;

//----------------------------------------------------------------------------//
// TabuCol/BuildTClash: construit table des conflits TClash et leur nb NClash //
//----------------------------------------------------------------------------//

Procedure BuildTClash (var TClash:TabClash; var NClash:ArcNum);
Var i,j: Node;
    k  : ArcNum;
Begin
      NClash := 0;
      For i := 1 to NX+NY do For j := 1 to NX+NY do TClash[i,j] := 0;
      {TClash[i,j] = nb de voisins de i avec couleur j}
      For i := 1 to NX+NY do
      For k := Head[i] to Head[i+1]-1 do begin
         j  := Succ[k];
         Inc (TClash[i,Color[j]]);
         {j > i pour ne pas compter deux fois les conflits}
         If (j > i) and (Color[i] = Color[j]) then Inc (NClash)
      End
End;

//----------------------------------------------------------------------------//
// TabuCol/ClearTabuList: met a vide la liste taboue TabuList                 //
//----------------------------------------------------------------------------//

Procedure ClearTabuList;
Var x: Node;
Begin
   With TabuList do begin
      For x := 0 to NT do OldNode[x] := 0;
      Last  := 0
   End
End;

//----------------------------------------------------------------------------//
// TabuCol/NotTabu: fonction testant si noeud i/couleur j n'est pas tabou     //
//----------------------------------------------------------------------------//

Function NotTabu (i,j:Node): Boolean;
Var p: TabuSub;
Begin
   With TabuList do begin
      p := 1;
      While (p<=NT) and ((OldNode[p]<>i) or (OldColor[p]<>j)) do Inc (p);
      NotTabu := p > NT
   End
End;

//----------------------------------------------------------------------------//
// TabuCol/UpdateTabuList: met noeud i/couleur j en liste taboue              //
//----------------------------------------------------------------------------//

Procedure UpdateTabuList (i,j:Node);
Begin
   With TabuList do begin
      If Last = NT then Last := 1 else Inc (Last);
      OldNode [Last] := i;
      OldColor[Last] := j
   End
End;

//----------------------------------------------------------------------------//
// TabuCol/Explore: explore le voisinage pour economiser des conflits         //
//----------------------------------------------------------------------------//

Procedure Explore (var MinClash:ArcNum; var BestNode,NewColor:Node);
Var i,j     : Node;
    NewClash: ArcNum;
Begin
   MinClash := MaxArcNum;
   {Pour chaque noeud i avec au moins un conflit}
   For i := 1 to NX+NY do If TClash[i,Color[i]] > 0 then begin
      {Essaie pour i une autre couleur, si transfo non taboue}
      For j := 1 to K do If j <> Color[i] then begin
         NewClash := NClash - TClash[i,Color[i]] + TClash[i,j];
         If (NewClash < MinClash) and NotTabu(i,j) then begin
            MinClash := NewClash;
            BestNode := i;
            NewColor := j
         End
      End
   End
End;

//----------------------------------------------------------------------------//
// TabuCol/TabuSearch: recherche taboue pour minimiser le nb de conflits      //
//----------------------------------------------------------------------------//

Procedure TabuSearch;
Var MinClash : ArcNum;
    BestNode : Node;
    NewColor : Node;
Begin
   ClearTabuList;                                {Vide la liste taboue}
   NIter     := 0;
   Repeat
      Inc (NIter);                               {Compte 1  exploration}
      Explore (MinClash,BestNode,NewColor);      {Calc. meilleure transfo}
      UpdateTabuList (BestNode,Color[BestNode]); {Empeche retour en arriere}
      If MinClash < MaxArcNum then begin         {Si transfo trouvee   }
         Color[BestNode] := NewColor;            {On l'effectue        }
         BuildTClash (TClash,NClash);            {MAJ table conflits   }
If trace then If NClash <> MinClash then Error ('Clash crash');
      End
   Until (NIter=NItMax) or (NClash=0) or (MinClash=MaxArcNum);
   {NB: pas de sauvegarde de la meilleure solution a chaque amelioration}
   {car le but est d'atteindre NClash=0, pas de le minimiser}
If trace then If NClash=0 then WriteLn ('Reduced to 0 in ',NIter,' iterations');
End;

Begin
   If NT > NTMax then Error ('TabuCol: liste taboue trop longue');
   {Bonne coloration initiale avec LFS}
   Dsatur (BestColor,K);
If Trace then WriteLn ('Nombre initial de couleurs: ',K);
   K     := K-1;
   {Boucle principale:chaque iteration cherche une k-coloration de G}
   Repeat
If trace then Write ('Essaie K=',K);
      PutKColors;                    {Place arbitrairement K couleurs}
      BuildTClash (TClash,NClash);   {Construit table des conflits}
If trace then WriteLn (', NClash: ',NClash);
      If NClash > 0 then TabuSearch; {Tabou minimisant les conflits}
      If NClash = 0 then begin       {0 conflit: on a une k-coloration}
         BestColor := Color;         {Meilleure coloration trouvee}
         Dec (K)                     {Essaie avec une couleur en moins}
      End
   Until NClash > 0;
   Color := BestColor;
   NC    := K+1
End;




Procedure T_GRAPHE_LISTE.SLOrder (var V,Di:TNodeInfo);
Var i,j,Deg,MinDeg: Node;
    p             : ArcNum;
    Free          : TNodeBool;
Begin
      For i := 1 to NX+NY do Free[i] := True;
      For i := NX+NY downto 1 do begin
         {Calcule Vi=j,de deg min dans sous-graphe des sommets libres}
         MinDeg := MaxNode;
         For j := 1 to NX+NY do If Free[j] then begin
            Deg   := 0;
            For p := Head[j] to Head[j+1]-1 do
            If Free[Succ[p]] then Inc (Deg);
            If Deg < MinDeg then begin
               MinDeg := Deg;
               V[i]   := j
            End;
         End;
         Free[V[i]] := False;
         Di[V[i]]   := MinDeg
      End
End;



Procedure T_GRAPHE_LISTE.LFOrder (var V:TNodeInfo);
Var x,y: Node;
    Deg: TNodeCost;
    H  : T_Heap;
Begin
      H:=T_HEAP.Create;

      {1. Construit un tableau de degres pour le tri}
      For x := 1 to NX+NY do Deg[x] := OutDeg(x);
      {2) Construit un tas en O(N), avec la méthode de Aho}
      {a) Charge les sommets en vrac dans le tas}
      H.p_Num   := NX+NY;
      For x := 1 to NX+NY do H.p_Body[x] := x;
      H.pt_Where := H.pt_Body;
      {b) Descend chaque noeud en partant de la fin du tas}
      For x := NX+NY div 2 downto 1 do H.MoveDown (Deg,x);
      {3. Tri}
      For x := NX+NY downto 2 do begin
         y        := H.p_Body[x];
         H.p_Body[x]  := H.p_Body[1];
         H.p_Body[1]  := y;
         H.p_Where[y] := 1;
         H.p_Num:=H.p_Num-1;
         H.MoveDown (Deg,y)
      End;
      V := H.pt_Body;
   H.DESTROY;
End;


end.

