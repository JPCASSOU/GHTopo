(*
  @abstract(Contient des classes génériques pour manipuler des
  tableaux "Array" a travers un pointeur.)

  Contient des classes pour manipuler des tableaux 1D de type Byte, Integer, Single et Double. @br
  Contient des classes pour manipuler des tableaux 2D de type Byte, Integer, Single et Double.

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  @bold(Historique) : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZSceneStrConsts, BZLogger si DEBUGLOG est activé @br

  -------------------------------------------------------------------------------------------------------------

   @bold(Credits :)@br
     @unorderedList(
       @item(J.Delauney (BeanzMaster))
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / LGPL @br

  ------------------------------------------------------------------------------------------------------------- *)
unit BZArrayClasses;

//==============================================================================
{$mode delphi}{$H+}

// Options de débogage pour que les logs soient fonctionnels, nous devons définir
// les options de compilations dans les unités des paquets. Car les options relatif à
// un projet ne sont pas propagé dans les unités d'un paquet.
{.$DEFINE DEBUG}
{.$DEFINE DEBUGLOG}

//==============================================================================

interface

uses
  Classes, SysUtils;

//==============================================================================

const
  { Granularité de croissance minimum d'un tableau }
  cDefaultListGrowthDelta = 16;

  { Lorsque la liste est plus petite que cInsertionSort_CutOff, nous utilisons InsertionSort au lieu d'appeler
   tout autre algorithme de tri, de manière récursive.
   8 et 64 semblent être les limites inférieures et supérieures où les performances se dégradent, donc
   quelque chose entre 16 et 48 ​​donne probablement les meilleures performances
   D'après mes tests 42 est une bonne valeur médiane. }
  cInsertionSort_CutOff   = 42;

  { Lorsque la liste est plus petite que cQuickSort_CutOff, nous utilisons l'algorithme QuickSort au lieu de DualQuickSort }
  cQuickSort_CutOff       = 500;

type
  { Déclaration pour définir un tableau dynamique de single }
  TBZDynSingleArray = Array of Single;
  { Déclaration pour définir un tableau dynamique de single }
  TBZDynIntegerArray = Array of Integer;
  { Déclaration pour définir un tableau dynamique de single }
  TBZDynByteArray = Array of Byte;

  { Ordre de trie Ascendant ou descendant }
  TBZSortOrder = (soAscending, soDescending);

  { Classe de base générique pour gérer un tableau de n'importe quel type, tableau statique et pointeur.
    Fournis quelques fonctions utiles et communes  ajouter, supprimer, accès aux données.
    Prend également en charge l'accès en mode LIFO (Stack)  }
type TBZBaseList<T> = Class
  private
    FTagString : string;
    type
      PT = ^T;
      TArr = array of T;
    procedure SetCount(AValue : Int64);

  protected
    {$CODEALIGN RECORDMIN=16}
    FData: TArr;  //Le pointeur de liste de base (non typé)
    {$CODEALIGN RECORDMIN=4}

    FCapacity:Int64;
    FDataSize:Int64;
    FItemSize:Int64; // Doit être défini dans les sous-classes
    FGrowthDelta: Integer;
    FParentData: Pointer;
    FHandle: Int64;

    FRevision: LongWord;
    FCount: Int64;

    FPosition: Int64;
    FFirstDone: Boolean;

    Function GetData: Pointer; inline;
    Function GetDataArray: TArr; inline;

    function GetValue(const Position: Int64): T;
    procedure SetValue(const Position : Int64; const AValue : T);

    function GetMutable(const Position: Int64): PT;
    procedure IncreaseCapacity();

    procedure SetCapacity(const NewCapacity: Int64); virtual;

    function CompareValue(Const elem1, elem2: T) : Integer; virtual;

    procedure AnyQuickSort( idxL, idxH : Integer; Dir : TBZSortOrder); //CompareValue: TBZArraySortCompareValue); //var SwapBuf : pByte);
    procedure AnyInsertionSort(idxL, idxH : Integer; Dir : TBZSortOrder); //CompareValue: TBZArraySortCompareValue);
    procedure AnyDualPivotQuickSort(idxL, idxH : Integer; Dir : TBZSortOrder); // CompareValue : TBZArraySortCompareValue);

  public
    { Creation d'un nouveau tableau vide}
    constructor Create; overload;//override;
    { Creation d'un nouveau tableau de "Reserved" éléments }
    constructor Create(Reserved: Int64); overload;
    { Créer un nouveau tableau avec un nombre d'éléments Réservés et un propriétaire }
    constructor CreateParented(AParentData: Pointer; Reserved: Int64); overload;
    { Libère et détruit le tableau }
    destructor Destroy; override;

    { Renvoie la taille totale du tableau }
    function DataSize(): Int64; // size of the list
    { Renvoie la taille d'un élément }
    function ItemSize(): Longint;

    { Ajoute un nouvel élément }
    function Add(const Value: T):Int64;
    { Ajoute un nouvel élément si celui-ci n'es pas déja présent dans le tableau. Retourne -1 si l'élément n'est pas ajouté }
    function AddNoDup(const Value: T):Int64;
    { Insert un nouvel élément à la position "Position" }
    procedure Insert(Position: Int64; const Value: T);
    { Efface un  élément à la position "Position" }
    procedure Delete(Position : Int64);
    { Echange la posiotion des éléments "Index1" et "Index2" }
    procedure Exchange(index1, index2: Int64);
    { Inverse le tableau }
    procedure Reverse(); inline;

    //procedure AddNulls(nbVals: Cardinal); inline;
    //procedure InsertNulls(Position : Int64; nbVals: Cardinal); inline;

    { Vide la liste sans altérer la capacité. }
    procedure Flush();
    { Vide la liste et libère les éléments }
    procedure ClearListe();
    { Ajoute un nouvel élément en haut tableau }
    procedure Push(const Value: T);
    { Récupère l'élément en haut du tableau }
    function Pop: T;
    { Retourne le premier élément du tableau }
    function First: T;
    { Retourne le dernier élément du tableau }
    function Last(): T;
    { Renvoie l'élément suivant du tableau, relatif à la position actuelle }
    function Next(): T;
    { Renvoie l'élément précédent du tableau, relatif à la position actuelle }
    function Prev(): T;
    { Renvoie l'élément actuel du tableau, relatif à la position actuelle }
    function Current : T;
    { Déplacer la position actuelle dans le tableau à la prochaine }
    function MoveNext():Boolean;
    { Déplacer la position actuelle dans le tableau à la précédente }
    function MovePrev():Boolean;
    { Déplacer la position actuelle dans le tableau vers le premier élément }
    function MoveFirst():Boolean;
    { Déplacer la position actuelle dans le tableau vers le dernier élément }
    function MoveLast():Boolean;
    { Renvoie la position actuelle dans le tableau }
    function GetPosition() : Int64;
    { Se placer à la postion "Pos" en fonction de StartAt@br
      @param(Valeurs possible pour StartAt  :@br
      @unorderedlist(
        @item( 0 = Depuis le début (defaut) )
        @item( 1 = Depuis la position actuelle)
        @item( 2 = En partant de la fin)
      )}
    function Seek(const pos : Int64; const StartAt : Byte = 0) : boolean;
    { Se placer à n'importe quelle position dans le tableau (equivalent a seek(position, 0)}
    function MoveTo(const Position:Int64) : Boolean;
    { Vérifie si la fin du tableau est atteinte}
    function IsEndOfArray() : Boolean;
    { Retourne l'index de l'élément "SearchItem". Dans le cas ou celui n'existe pas retourne -1 }
    function IndexOf(const SearchItem : T): Integer;
    { Renvoie l'élélement suivant de l'élément "anItem", Si l'élément n'existe pas le premier élément du tableau est retourné }
    function GetNextFrom(const anItem : T) : T;

    // Array Rasterizer
    // function Scan(CallBack):Boolean;
    // function ScanNext(CallBack):Boolean;
    // function ScanPrev(CallBack):Boolean;

    // function ScanMany(nbItem,CallBack):Boolean;
    // function ScanTo(Position,CallBack):Boolean;

    // function ScanAll(CallBack):Boolean;
    // function ScanRange(From, To, CallBack):Boolean;

    // Array Utils

   // function CompareItems(Index1, index2, CompareValue): Integer; virtual;

    { Trie le tableaux par ordre croissant ou décroissant.
      L'algorithme de tri utilisé dépend du nombre d'éléments en fonction des constantes  cInsertion_CutOff et cQuickSort_CutOff. @br
      Si la liste à un nombre d'éléments inférieur à 42 le tri par insertion est utilisé. @br
      Si la liste à un nombre d'éléments inférieur ou égale à 500 le tri rapide "QuickSort" est utilisé. @br
      Si non, c'est le tri rapide à double pivot "DualPivotQuickSort" qui est utilisé. }
    procedure Sort(Const Direction : TBZSortOrder = soAscending);  virtual;
    { Trie le tableau avec l'algorithme de trie rapide "QuickSort" version "Stable" }
    procedure QuickSort(Const Direction : TBZSortOrder = soAscending); virtual;
    { Trie le tableau avec l'algorithme de trie rapide à double pivot version "Stable", c'est l'algorithme utilisé par défaut par la méthode Sort }
    procedure DualQuickSort(Const Direction : TBZSortOrder = soAscending); virtual;
    { Trie le tableau avec l'algorithme d'insertion }
    procedure InsertionSort(Const Direction : TBZSortOrder = soAscending); virtual;

    { Mélange les éléments du tableau aléatoirement. Il vous faudra appeler la méthode Randomize en premier }
    procedure Shuffle();

    // procedure Merge(AnotherArray: TBZBaseList<T>);
    // function Clone : TBZBaseList<T>;
    // function Extract(From, Nb : Int64): TBZBaseList<T>;

    // Extra funcs for management
    // function InsertItemsAt(Pos:Int64; AnArray : TBZBaseList<T>):Boolean;
    // function InsertItemsAtEnd
    // function InsertItemsAtFirst
    // procedure DeleteItems(Index: Int64r; nbVals: Cardinal); inline;

    { Nombre d'éléments dans la liste. Lors de l'attribution d'un nombre, les éléments ajoutés sont réinitialisés à zéro. }
    property Count: Int64 read FCount write SetCount;
    { Capacité du tableau actuel. Non persistant. }
    property Capacity: Int64 read FCapacity write SetCapacity;
    { Granularité de croissance. Pas persistant. }
    property GrowthDelta: Integer read FGrowthDelta write FGrowthDelta;
    { Augmenter de un après chaque changement de contenu. }
    property Revision: LongWord read FRevision write FRevision;
    { Renvoie le propriétaire s'il existe }
    property ParentData : Pointer read FParentData;
    { Renvoie le tableau sous forme de pointeur }
    property Data : Pointer read GetData;
    property DataArray : TArr read GetDataArray;
    { Renvoie le handle du tableau }
    property Handle : Int64 read FHandle;

    { Accès aux éléments du tableau }
    property Items[i : Int64]: T read getValue write SetValue;// default;
    { Accès à l'élément dans le tableau en tant que pointeur générique }
    property Mutable[i : Int64]: PT read getMutable;
    { Tag utilisateur }
    property TagString: string read FTagString write FTagString;
  end;

  {
  //generic TBZBaseListMap3D<T> = class(specialize TBZBaseList<T>)
  //private
  //  function GetValue3D(x, y, z : Int64): T;
  //  procedure SetValue3D(x, y, z : Int64; AValue: T);
  //published
  //public
  //  constructor Create(Rows, Cols, DCols : Int64); overload;
  //  constructor CreateParented(AParentData: Pointer; Rows, Cols, DCols: Int64); overload;
  //  property Items[x,y,z : Int64]: T read GetValue3D write SetValue3D;
  //end;
  //
  //generic TBZBaseListMap4D<T> = class(specialize TBZBaseList<T>)
  //private
  //  function GetValue4D(x, y, z, w : Int64): T;
  //  procedure SetValue4D(x, y, z, w : Int64; AValue: T);
  //published
  //public
  //  constructor Create(Rows, Cols, DCols, TCols: Int64); overload;
  //  constructor CreateParented(AParentData: Pointer; Rows, Cols, DCols, TCols: Int64); overload;
  //  property Items[x,y,z,w : Int64]: T read GetValue4D write SetValue4D;
  //end;

Type
  { Tableau générique de type Byte à une Dimension }
type TBZIntegerList = class(TBZBaseList<Integer>)
  private
  protected
    function CompareValue(Const elem1, elem2: integer) : Integer;  override;
  public
end;
type TBZDoubleList = class(TBZBaseList<double>)
  private
  protected
    function CompareValue(Const elem1, elem2: double) : Integer;  override;
  public
end;
implementation

//==============================================================================

{%region%=====[ TBZBaseList ]=================================================}

constructor TBZBaseList<T>.Create;
begin
  inherited Create;
  FCapacity:=0;
 // FItemSize:=Sizeof(T); // Must be defined in subclasses  ????
  FGrowthDelta:= cDefaultListGrowthDelta;
  FParentData:=nil;
  FHandle:=0;
  //FIsDirty:=false;
  FRevision:=0;
  FCount:=0;
  FPosition:=0;
  FFirstDone:=false;
end;

constructor TBZBaseList<T>.Create(Reserved : Int64);
begin
  Create;
  FDataSize:=Reserved;//*ItemSize;
  SetCapacity(Reserved);
  //SetCount(Reserved);
end;

constructor TBZBaseList<T>.CreateParented(AParentData : Pointer; Reserved : Int64);
begin
  Create(Reserved);
  FParentData := AParentData;
end;

destructor TBZBaseList<T>.Destroy;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseList.Destroy');
    {$ENDIF}
  {$ENDIF}
  ClearListe;
  //SetLength(FData, 0);
  FData := nil;
  inherited Destroy;
end;

procedure TBZBaseList<T>.SetCount(AValue : Int64);
begin
  {$ifdef DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseList.SetCount');
      GlobalLogger.LogStatus('>>> AValue = ' + AValue.ToString);
    {$ENDIF}
    Assert(AValue >= 0);
  {$endif}
  if FCount = AValue then Exit;
  if AValue> FCapacity then SetCapacity(AValue);
  FCount := AValue;
  Inc(FRevision);
end;

Function TBZBaseList<T>.GetData : Pointer;
begin
  Result := @FData;
end;

Function TBZBaseList<T>.GetDataArray : TArr;
begin
  Result := FData;
end;

function TBZBaseList<T>.GetValue(const Position : Int64) : T;
begin
  Result := FData[Position];
end;

procedure TBZBaseList<T>.SetValue(const Position : Int64; const AValue : T);
begin
  if (Position >= FCapacity) then IncreaseCapacity;
  FData[Position] := AValue;
end;

function TBZBaseList<T>.GetMutable(const Position : Int64) : PT;
begin
  Result := @FData[Position];
end;

procedure TBZBaseList<T>.IncreaseCapacity();
begin
  if (FCapacity = 0) then SetCapacity(1)
  else
  begin
    if (FCount >= FCapacity) then SetCapacity(FCapacity + FGrowthDelta);
  end;
end;

procedure TBZBaseList<T>.SetCapacity(const NewCapacity : Int64);
begin
  if (FCapacity = newCapacity) then exit;
  FCapacity := newCapacity;
  SetLength(FData, FCapacity);
  Inc(FRevision);
end;

function TBZBaseList<T>.CompareValue(Const elem1, elem2: T) : Integer;
begin
  result := 0;
end;

function TBZBaseList<T>.DataSize() : Int64;
begin
  Result := FCount * ItemSize; //FDataSize;
end;

function TBZBaseList<T>.ItemSize() : Longint;
begin
  Result := Sizeof(T); //FItemSize;
end;

function TBZBaseList<T>.Add(const Value : T) : Int64;
begin
  Result := FCount;
  if (Result >= FCapacity) then self.IncreaseCapacity();
  FData[Result] := Value;
  Inc(FCount);
end;

function TBZBaseList<T>.AddNoDup(const Value : T) : Int64;
Var
  pos : Integer;
  isNew : Boolean;
begin
  Result := -1;
  pos := 0;
  isNew := True;
  while ((pos < FCount) and (isNew = true)) do
  begin
    if FData[pos] = Value then isNew := False;
    inc(pos);
  end;
  if IsNew then
  begin
    {$IFDEF DEBUG}
      {$IFDEF DEBUGLOG}
        GlobalLogger.LogHint('>>>> New value');
      {$ENDIF}
    {$ENDIF}
    Result := FCount;
    if (Result >= FCapacity) then IncreaseCapacity;
    FData[Result] := Value;
    Inc(FCount);
  end;
end;

procedure TBZBaseList<T>.Insert(Position : Int64; const Value : T);
begin
  if FCount = FCapacity then IncreaseCapacity;
  if Position < FCount then
    System.Move(FData[Position], FData[Position + 1], (FCount - Position) * ItemSize);
  FData[Position] := Value;
  Inc(FCount);
end;

procedure TBZBaseList<T>.Delete(Position : Int64);
begin
  if (Position < (FCount - 1)) then
  begin
    Dec(FCount);
    System.Move(FData[(Position + 1)], FData[Position], (FCount - Position) * ItemSize);
  end
  else Dec(FCount);
  Inc(FRevision);
end;

procedure TBZBaseList<T>.Exchange(index1, index2 : Int64);
var
  temp : T;
begin
  if Index1 = Index2 then Exit;
  temp := FData[index1];
  FData[index1] := FData[index2];
  FData[index2] := temp;
  Inc(FRevision);
end;

procedure TBZBaseList<T>.Reverse;
var
  s, e: Integer;
begin
  s := 0;
  e := FCount - 1;
  while s < e do
  begin
    Exchange(s, e);
    Inc(s);
    Dec(e);
  end;
  Inc(FRevision);
end;

//procedure TBZBaseList.AddNulls(nbVals : Cardinal);
//begin
//
//end;
//
//procedure TBZBaseList.InsertNulls(Position : Int64; nbVals : Cardinal);
//begin
//
//end;

procedure TBZBaseList<T>.Flush;
begin
  SetCount(0);
end;

procedure TBZBaseList<T>.ClearListe();
begin
  SetCount(0);
  SetCapacity(0);
end;

//procedure TBZBaseList.AdjustCapacityToAtLeast(const size: Integer);
//begin
//  if FCapacity < Size then SetCapacity(size);
//end;

procedure TBZBaseList<T>.Push(const Value : T);
begin
  self.Add(Value);
end;

function TBZBaseList<T>.Pop() : T;
begin
  Result := FData[FCount-1];
 Delete(FCount-1);
end;

function TBZBaseList<T>.First() : T;
begin
  Result := FData[0];
end;

function TBZBaseList<T>.Last() : T;
begin
  Result := Pop();
end;

function TBZBaseList<T>.Next() : T;
begin
  if (FPosition < FCount) then //Inc(FPosition);
    Result := FData[FPosition + 1]
  else Result := FData[FPosition];
end;

function TBZBaseList<T>.Prev() : T;
begin
  if (FPosition > 0) then  //Dec(FPosition);
   Result := FData[FPosition - 1]
  else Result := FData[FPosition];
end;

function TBZBaseList<T>.Current() : T;
begin
  Result := FData[FPosition];
end;

function TBZBaseList<T>.MoveNext() : Boolean;
begin
  Result := false;
  if (FPosition >= FCount-1) then exit;
  Result := True;
  Inc(FPosition);
end;

function TBZBaseList<T>.MovePrev() : Boolean;
begin
  Result := false;
  if (FPosition <= 0 ) then exit;
  Result := True;
  Dec(FPosition);
end;

function TBZBaseList<T>.MoveFirst() : Boolean;
begin
  result := true;
  FPosition := 0;
end;

function TBZBaseList<T>.MoveLast() : Boolean;
begin
  result := true;
  FPosition := FCount-1;
end;

function TBZBaseList<T>.GetPosition() : Int64;
begin
  Result := FPosition;
end;

function TBZBaseList<T>.Seek(const pos : Int64; const StartAt : Byte=0) : boolean;
var
  newpos : Int64;
begin
  result := true;
  Case StartAt of
    0: newpos := Pos; // From Beginning
    1:
    begin
      newpos := (FPosition-1) + Pos; // From Current positon
      if newpos >= FCount then
      begin
        //newpos := FCount-1;
        result := false;
      end;
    end;
    2:
    begin
      newpos := (FCount-1) - Pos; // From End;
      if newpos=0 then
      begin
        //newpos := 0;
        result := false;
      end;
    end;
    else newpos := pos;
  end;
  if result then FPosition := newpos;
end;

function TBZBaseList<T>.MoveTo(const Position:Int64) : Boolean;
begin

  result:= Self.Seek(Position, 0);
end;

function TBZBaseList<T>.IsEndOfArray() : Boolean;
begin
  result := (FPosition >= FCount);
end;

function TBZBaseList<T>.IndexOf(const SearchItem : T) : Integer;
var
  i: Integer;
begin
  if FCount <= 0 then Result := -1
  else
  begin
    Result := -1;
    for i  := 0 to FCount - 1 do
      if FData[i] = SearchItem then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function TBZBaseList<T>.GetNextFrom(const anItem : T) : T;
begin
  if FCount <= 0 then exit;
  Result := FData[((Self.indexOf(anItem) + 1) mod Self.Count)];
end;

{ Le "Dual Pivot QuickSort" est plus légèrement plus rapide que le QuickSort classique (dans la majorité des cas).
Sur la papier, le DualPivotQuickSort à 1.9nlnn+O(n) comparaisons, ce qui est 5% de moins que les comparaisons 2nlnn+O(n)  de l'algorithme Quicksort classique à pivot unique.
Cependant, il a besoin de 0.6nlnn+O(n) échanges de valeurs alors qu'avec le Quicksort classique il en faut 1/3nlnn+O(n).
Il existe une version en Java optimisée de cet algorithmequi donne de meilleurs résultats, mais celle-ci est complexe à mettre en place
cf : http://hg.openjdk.java.net/jdk8/jdk8/jdk/file/tip/src/share/classes/java/util/DualPivotQuicksort.java }
procedure TBZBaseList<T>.AnyDualPivotQuickSort(idxL, idxH : Integer; Dir : TBZSortOrder);
var
  idx, lpi, rpi : Integer;
  nb, li, hi  : Integer;
  SwapTemp, p, q, ak : T;

begin

  if (idxH <= idxL) then Exit;
  nb := idxH - idxL;

  if nb < cInsertionSort_CutOff then
  begin
    AnyInsertionSort(idxL, idxH, Dir);
    Exit;
  end;
  //else if nb <= cQuickSort_CutOff then
  //begin
  //  AnyQuickSort(idxL, idxH, Dir);
  //  Exit;
  //end;

  li := idxL;
  hi := idxH;

  // Debut de la partition
  if Dir = soAscending then
  begin
    if (CompareValue(FData[hi], FData[li]) < 0) then
    begin
      SwapTemp := FData[li];
      FData[li] := FData[hi];
      FData[hi] := SwapTemp;
    end;
  end
  else
  begin
    if (CompareValue(FData[hi], FData[li]) > 0) then
    begin
      SwapTemp := FData[li];
      FData[li] := FData[hi];
      FData[hi] := SwapTemp;
    end;
  end;

  p := FData[li];
  q := FData[hi];

  lpi := li + 1;
  rpi := hi - 1;
  idx := lpi;

  if Dir = soAscending then
  begin
    While (idx <= rpi) do
    begin
      ak := FData[idx];
      if (CompareValue(ak, p) < 0) then
      begin
        FData[idx] := FData[lpi];
        FData[lpi] := ak;
        inc(lpi);
      end
      else if (CompareValue(ak, q) >= 0) then
      begin
        While ((CompareValue(FData[rpi], q) > 0) and (idx < rpi)) do Dec(rpi);
        FData[idx] := FData[rpi];
        FData[rpi] := ak;
        dec(rpi);
        if (CompareValue(ak, p) < 0) then
        begin
          FData[idx] := FData[lpi];
          FData[lpi] := ak;
          inc(lpi);
        end;
      end;
      inc(idx);
    end;
  end
  else
  begin
    While (idx <= rpi) do
    begin
      ak := FData[idx];
      if (CompareValue(ak, p) > 0) then
      begin
        FData[idx] := FData[lpi];
        FData[lpi] := ak;
        inc(lpi);
      end
      else if (CompareValue(ak, q) <= 0) then
      begin
        While ((CompareValue(FData[rpi], q) < 0) and (idx < rpi)) do Dec(rpi);
        FData[idx] := FData[rpi];
        FData[rpi] := ak;
        dec(rpi);
        if (CompareValue(ak, p) > 0) then
        begin
          FData[idx] := FData[lpi];
          FData[lpi] := ak;
          inc(lpi);
        end;
      end;
      inc(idx);
    end;
  end;

  Dec(lpi);
  Inc(rpi);

  SwapTemp := FData[lpi];
  FData[lpi] := FData[li];
  FData[li] := SwapTemp;

  SwapTemp := FData[rpi];
  FData[rpi] := FData[hi];
  FData[hi] := SwapTemp;
  // Fin de la partition

  AnyDualPivotQuickSort(li,lpi - 1, Dir);
  if DIr = soAscending then
  begin
    if (CompareValue(FData[lpi], FData[rpi]) < 0) then AnyDualPivotQuickSort(lpi + 1, rpi - 1, Dir);
  end
  else
  begin
    if (CompareValue(FData[lpi], FData[rpi]) > 0) then AnyDualPivotQuickSort(lpi + 1, rpi - 1, Dir);
  end;
  AnyDualPivotQuickSort(rpi + 1, hi, Dir);
end;

procedure TBZBaseList<T>.AnyQuickSort(idxL, idxH : Integer; Dir : TBZSortOrder);
var
  li, hi : Integer;
  TP : Integer;
  mi    : Integer;
  SwapBuf : T;
begin
  if (idxL >= idxH) then exit;
  if (idxH - idxL) < cInsertionSort_CutOff then
  begin
    AnyInsertionSort(IdxL, idxH, Dir);//, CompareValue);
    Exit;
  end;

  if idxL<>0 then mi := (idxL + idxH) div 2
  else mi := (((idxL+1) + idxH) div 2) - 1;
  li := idxL;
  hi := idxH;

  SwapBuf := FData[li];
  FData[li] := FData[mi];
  FData[mi] := SwapBuf;

  TP := idxL;
  inc(li);


  if dir = soAscending then
  begin
    repeat
      if (CompareValue( FData[li], FData[idxL] ) < 0) then
      begin
        inc(TP);
        SwapBuf := FData[TP];
        FData[TP] := FData[li];
        FData[li] := SwapBuf;
      end;
      inc(li);
    until li>hi;
  end
  else
  begin
    repeat
      if (CompareValue( FData[li], FData[idxL] ) > 0) then
      begin
        inc(TP);
        SwapBuf := FData[TP];
        FData[TP] := FData[li];
        FData[li] := SwapBuf;
      end;
      inc(li);
    until li>hi;
  end;

  SwapBuf := FData[idxL];
  FData[idxL] := FData[TP];
  FData[TP] := SwapBuf;

  AnyQuickSort(idxL, TP-1, Dir);//, CompareValue);
  AnyQuickSort(TP+1, idxH, Dir);//, CompareValue);
end;

procedure TBZBaseList<T>.AnyInsertionSort(idxL, idxH : Integer; Dir : TBZSortOrder);
var
  ps, cs : Integer;
  li,hi : Integer;
  SwapBuf : T;
begin

  if FCount<2 then exit;

  if FCount = 2 then
  begin
    if (CompareValue(FData[1], FData[0]) < 0) then
    begin
      If Dir = soAscending then
      begin
        SwapBuf := FData[0];
        FData[0] := FData[1];
        FData[1] := SwapBuf;
        Exit;
      end
      else
      begin
        Exit;
      end;
    end;
  end;

  li :=idxL + 1;
  hi :=idxH;

  Repeat
   SwapBuf := FData[li]; //Move(pb[ls], SwapBuf^, Stride);
   ps := li;
   cs := ps - 1;
   If Dir = soAscending then
   begin
     While (ps >= 1) and  (CompareValue(SwapBuf, FData[cs]) < 0) do
     begin
       FData[ps] := FData[cs];
       dec(ps);
       dec(cs);
     end;
   end
   else
   begin
     While (ps >= 1) and  (CompareValue(SwapBuf, FData[cs]) > 0) do
     begin
       FData[ps] := FData[cs];
       dec(ps);
       dec(cs);
     end;
   end;
   FData[ps] := SwapBuf;
   inc(li);
  until li > hi;

end;



procedure TBZBaseList<T>.Sort(Const Direction : TBZSortOrder);
begin
  if FCount<2 then exit;
  if FCount < cInsertionSort_CutOff then AnyInsertionSort(0, FCount-1, Direction)
  else if FCount <= cQuickSort_CutOff then AnyQuickSort(0, FCount-1, Direction)
  else AnyDualPivotQuickSort(0, FCount-1, Direction);
  AnyQuickSort(0, FCount-1, Direction);
end;

procedure TBZBaseList<T>.QuickSort(Const Direction : TBZSortOrder);
begin
  if FCount<2 then exit;
  AnyQuickSort(0, FCount-1, Direction);
end;

procedure TBZBaseList<T>.DualQuickSort(Const Direction : TBZSortOrder);
begin
  if FCount<2 then exit;
  AnyDualPivotQuickSort(0, FCount-1, Direction);
end;

procedure TBZBaseList<T>.InsertionSort(Const Direction : TBZSortOrder);
begin
  AnyInsertionSort(0, FCount-1, Direction);
end;

procedure TBZBaseList<T>.Shuffle();
Var
 SwapBuffer : T;
 I, K, RandIdx : Integer;
begin
  K := FCount - 1;
  for i := 1 to  K do
  begin
    RandIdx := random(K - I + 1) + I; // Distribution uniforme
    SwapBuffer := FData[RandIdx];
    FData[RandIdx] := FData[I];
    FData[I] := SwapBuffer;
  end;
end;
{%region%=====[ TBZDoubleList ]================================================}

function TBZIntegerList.CompareValue(Const elem1, elem2: integer) : Integer;
begin
  if      elem1 = elem2 then Result :=  0
  else if elem1 < elem2 then Result := -1
  else Result := 1;
end;

{%endregion%}

{%region%=====[ TBZSingleList ]================================================}

function TBZDoubleList.CompareValue(Const elem1, elem2: double) : Integer;
begin
  if      elem1 = elem2 then Result :=  0
  else if elem1 < elem2 then Result := -1
  else Result := 1;
end;

//==============================================================================
end.

