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


unit U_TYPE_GRAPHES;
{$MODE DELPHI}{$H+}

interface

CONST

MaxNode      =   5000;                    {Nb max de noeuds possibles}

MaxNodeEns   =   250;  // utilise dans le type StackNode dans
                       // la méthode BackTrack

MaxArcNum    =   MaxNode*MaxNode+MaxNode;{Nb max d'arcs utilisables}
LoopLess     =  True;                    {Constante "sans boucles"}
Simple       =  True;                    {Constante "simple"}
Layered      =  True;                    {Constante "en couches"=sans circuit}
MaxCost      =  MaxLongInt;              {Valeur max permise pour les co–ts}

MaxNbCosts = 3;     {Nb max de couts par graphe geres automatiquement}
// utilisé dans les buckets
UMax      = 1023;
Trace: Boolean=false;
    {Trace pour Local,SimAn,Tabu,Backtrack}
    {a n'utiliser que si le code est recompile en mode console}
    { car le mode trace utilise les anciennes instructions writeln}

TYPE

// type permettant d'eviter la liaison avec les types dependants du compilateur

Cost         = LongInt;                  {Type cout}

Node         = 0..MaxNode+1;             {Type noeud (sommet)}

NodeEns      = 0..MaxNodeEns+1;
                        // utilise dans le type StackNode dans
                       // la méthode BackTrack

ArcNum       = 0..MaxArcNum+1;          {Type numero d'arc (listes d'adjacence)}


// type generaux de manipulation

TNodeCost    = Array[Node] of Cost;      {Type tableau de co–ts sur noeuds}
TNodeInfo    = Array[Node] of Node;      {Type pour colorations de noeuds,etc}
TNodeBool    = Array[Node] of Boolean;   {Type tableau de bool‚ens sur noeuds}

TArcCost     = Array[ArcNum] of Cost;    {Type tableau de valuations d'arcs}
PTArcCost    = ^TArcCost;                {Type pointeur sur TArcCost}
TInverse     = Array[ArcNum] of ArcNum;  {Type tableau arc --> arc inverse}
PTInverse    = ^TInverse;                {Type pointeur sur TInverse}

// types utilises pour coder le graphe par liste de successeurs

THead        = Array[Node] of ArcNum;    {Type tableau de tetes de listes}
TSucc        = Array[ArcNum] of Node;    {Type tableau de listes d'adjacence}
PTSucc       = ^TSucc;                   {Type pointeur sur TSucc}

// types utilises pour coder le graphe sous forme de matrice

CostMatrix   = Array[Node,Node] of Cost; {Type matrice de couts}
NodeMatrix   = Array[Node] of TNodeInfo; {Type matrice de noeuds (pour Floyd)}

// utilises dans l'unite graphes
CostNb     = 0..MaxNbCosts;              {Nø de valuation (de tableau cout)}
TPTArcCost = Array[CostNb] of PTArcCost; {Tableau de pointeurs sur valuations}

// utilises dans l'unité U_BASE_GRAPHE pour la classe Buckets
BuckNo    = 0..UMax;


implementation

end.
