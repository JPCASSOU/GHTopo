unit BZGraphesClasses;

{$mode delphi}{$H+}


 { Algo Dijkstra : https://ksvi.mff.cuni.cz/~dingle/2017/lecture_10.html }

interface

uses
  Classes, SysUtils, math,
  Common,
  UnitListesSimplesWithGeneriques,
  BZGraphesTypes,
  BZArrayClasses, StructuresDonnees;


type
  TBZGraphNode    = class;
  TBZNodeLink     = class;
  TBZNodeLinkList = class;
  //noeud

  { TBZClassNode }

  TBZClassNode = Class
  private
    FOwner        : TBZGraphNode;

    FNodeLinkList : TBZNodeLinkList;
    FNodeIndex    : Integer;
    // data
    FIDStation    : TIDStation;
    FPosition     : TPoint3Df;
    // metadata
    FMetaData     : string;

    procedure SetOwner(const AValue: TBZGraphNode);
    function  GetNodeLink(const Index : Integer): TBZNodeLink;

    procedure SetNodeLink(Index : Integer; const AValue: TBZNodeLink);
  public
    constructor Create(AOwner : TBZGraphNode; const QIDStation: TIDStation; const QPosition: TPoint3Df; const QMetaData: string);
    destructor  Destroy; override;

    function AddNodeLink(anIndex : Integer; const Weight: double) : TBZNodeLink; overload;

    property Owner : TBZGraphNode read FOwner write SetOwner;

    property IDStation     : TIDStation read FIDStation write FIDStation;
    property Position: TPoint3Df read FPosition write FPosition;
    property NodeIndex : Integer read FNodeIndex write FNodeIndex;
    property LinkNode[Index : Integer] : TBZNodeLink read GetNodeLink write SetNodeLink;
    property NodeLinkList : TBZNodeLinkList read FNodeLinkList;
    property MetaData: string read FMetaData write FMetaData;
    function ToString(): string;
  end;
// arc
  TBZNodeLink = class
  private
    FOwner : TBZNodeLinkList;
    FTargetNodeIndex : Integer;

    FDistance : double;
    FAzimut   : double;
    FPente    : double;

    procedure SetTargetNodeIndex(const AValue: Integer);
    procedure SetOwner(const AValue: TBZNodeLinkList);
    function  getDistance(): double;
    function  getNode(): TBZClassNode;
    function  getParentNode(): TBZClassNode;

  public
    constructor Create(AOwner : TBZNodeLinkList; ATargetNodeIndex : Integer);
    destructor Destroy; override;

    property TargetNodeIndex : Integer read FTargetNodeIndex write SetTargetNodeIndex;
    property Owner           : TBZNodeLinkList  read FOwner write setOwner;
    property Distance        : double       read getDistance;   //FDistance;
    property Node            : TBZClassNode read getNode;
    property ParentNode      : TBZClassNode read getParentNode;
  end;



  { TGraphNode }
  TOnGraphNodeTravelNode = procedure (Sender : TObject; Node : TBZClassNode; ADistance : double) of object;
  TOnGraphNodeVerboseCall = procedure (Sender : TObject; msg : String) of object;
  //TBZGraphNode = class(specialize TBZBaseList<TClass>)
  TBZGraphNode = class(TBZBaseList<TBZClassNode>)

  private

    FOnGraphNodeTravelNode   : TOnGraphNodeTravelNode;
    FOnGraphNodeVerboseCall  : TOnGraphNodeVerboseCall;

    function  getNode(Index : Integer): TBZClassNode;
    procedure setNode(Index : Integer; const AValue: TBZClassNode);

  protected
    function CompareValue(const elem1, elem2: TBZClassNode) : Integer;  override;

    procedure DoOnTravelNode(Sender : TObject; Anode : TBZClassNode; ADistance : double);
    procedure DoVerboseCall(const s :String);
  public
    constructor Create;
    destructor Destroy; override;
    function AddNode(ANode : TBZClassNode) : Integer; overload;
    function AddNode(const QIDStation: TIDStation; const QPosition: TPoint3Df; const QMetaData: string) : TBZClassNode; overload;

    function Dijkstra(FromIndex, ToIndex : Integer; out ShortestPath : TListOfIntegers) : double;

    property Node[Index : Integer] : TBZClassNode read getNode write setNode;

    property OnTravelNode   : TOnGraphNodeTravelNode read FOnGraphNodeTravelNode write FOnGraphNodeTravelNode;
    property OnVerboseCall  : TOnGraphNodeVerboseCall read FOnGraphNodeVerboseCall write FOnGraphNodeVerboseCall;
  end;
   // liste des arcs partant ou arrivant au noeud
  TBZNodeLinkList = class(TBZBaseList<TBZNodeLink>)
  private
    FOwner : TBZClassNode;
    procedure SetOwner(const AValue: TBZClassNode);
  protected
    function CompareValue(Const elem1, elem2: TBZNodeLink) : Integer;  override;
  public
    constructor Create(AOwner : TBZClassNode); overload;
    destructor Destroy; override;

    function NewNodeLink(ToIndex : Integer) : TBZNodeLink;
    function AddNodeLink(aNodeLink : TBZNodeLink) : Integer;

    property Owner : TBZClassNode read FOwner write SetOwner;
  end;


implementation
uses
  DGCDummyUnit;
{%region=====[ TPriorityQueueHeap ]=============================================}

Const
  cNone = -MaxInt;

type
  TDynIntegerArray = Array of Integer;
  TDynDoubleArray = Array of double;

  { TPriorityQueueHeap }

  TPriorityQueueHeap = Class
  public

    Count: integer;
    Heap: TDynIntegerArray;      // binary heap holding values minimized by priority
    Priority: TDynDoubleArray;  // each value's priority, or None if absent
    Position: TDynIntegerArray;  // each value's position in the heap array, or None if absent

    Constructor Create(n : Integer);
    Destructor Destroy; override;

    procedure Swap(var i, j: integer);
    function IsEmpty : Boolean;
    procedure MoveDown(Index: integer);
    procedure MoveUp(Index: integer);
    function InsertPriority(Index : Integer; APriorityValue : double) : Integer;
    function RemoveSmallest(out APriorityValue : double) : Integer;
    function IsExist(Index : Integer) : Boolean;
    function getPriority(Index : Integer) : double;
    function DecreasePriority(Index : Integer; APriorityValue : double) : Integer;
    function getParentIndex(Index : Integer) : Integer;
    function getLeftChildIndex(Index : Integer) : Integer;
    function getRightChildIndex(Index : Integer) : Integer;

  end;


Constructor TPriorityQueueHeap.Create(n: Integer);
var
  i: integer;
begin
  Self.Count := 0;
  setLength(Self.Heap, 1);
  setLength(Self.Priority, n);
  setLength(Self.Position, n);
  for i := 0 to n - 1 do
    begin
      Self.priority[i] := cNone;
      Self.position[i] := cNone;
    end;
end;

Destructor TPriorityQueueHeap.Destroy;
begin
  setLength(Self.Heap, 0);
  setLength(Self.Priority, 0);
  setLength(Self.Position, 0);
  Finalize(Self.Priority);
  Finalize(Self.Position);
  Finalize(Self.Heap);
  inherited Destroy;
end;

procedure TPriorityQueueHeap.Swap(var i, j: integer);
var
  k: integer;
begin
  k := i;
  i := j;
  j := k;
end;

function TPriorityQueueHeap.IsEmpty : Boolean;
begin
  Result := (Self.Count = 0);
end;

// Move the value at index i downward until its children are larger than it
procedure TPriorityQueueHeap.MoveDown(Index: integer);
var
  l, r, smallest: integer;
begin
  l := Self.getLeftChildIndex(Index);
  r := Self.getRightChildIndex(Index);
  smallest := Index;

  if (l < Self.Count) and (Self.Priority[Self.Heap[l]] < Self.Priority[Self.Heap[Index]]) then smallest := l;
  if (r < Self.Count)  and (Self.Priority[Self.Heap[r]] < Self.Priority[Self.Heap[smallest]]) then smallest := r;

  if smallest <> Index then  // some child is smaller
    begin
      Self.swap(Self.Heap[Index], Self.Heap[smallest]);
      Self.swap(Self.Position[Self.Heap[Index]], Self.Position[Self.Heap[smallest]]);
      Movedown(smallest);
    end;
end;

// Move the value at index i upward until its parent is smaller than it
procedure TPriorityQueueHeap.MoveUp(Index : integer);
var
  p: integer;
begin
  if Index = 0 then exit;
  p := Self.getParentIndex(Index);
  if (Self.Priority[Self.Heap[Index]] < Self.Priority[Self.Heap[p]]) then // smaller than parent
    begin
      Self.Swap(Self.Heap[Index], Self.Heap[p]);
      Self.Swap(Self.Position[Self.Heap[index]], Self.Position[Self.Heap[p]]);
      Self.MoveUp(p);
    end;
end;

function TPriorityQueueHeap.InsertPriority(Index: Integer; APriorityValue: double) : Integer;
begin
  Result := 0;
  if (Self.Priority[Index] <> cNone) then
  begin
    Result := -1;
    Exit; // value is already in queue;
  end;

  if (Self.Count >= length(Self.Heap)) then  // array is full
  begin
    setLength(Self.Heap, length(Self.Heap) * 2);
  end;
  Self.Count := Self.Count + 1;
  Self.Heap[Self.Count - 1] := Index;
  Self.Position[Index] := Self.Count - 1;
  Self.Priority[Index] := APriorityValue;
  Self.MoveUp(Self.Count - 1);
end;

function TPriorityQueueHeap.RemoveSmallest(out APriorityValue : double) : Integer;
var
  smallest: integer;
begin
  smallest := Self.Heap[0];
  Result := smallest;
  APriorityValue := Self.Priority[smallest];
  Self.Priority[smallest] := cNone;
  Self.Position[smallest] := cNone;

  Self.Heap[0] := Self.Heap[Self.Count - 1];
  Self.Position[Self.Heap[0]] := 0;
  Self.Count := Self.Count - 1;
  Self.MoveDown(0);
end;

function TPriorityQueueHeap.IsExist(Index : Integer) : Boolean;
begin
  Result := (Self.Priority[Index] <> cNone);
end;

function TPriorityQueueHeap.getPriority(Index : Integer) : double;
begin
  Result := Self.Priority[Index];
end;

function TPriorityQueueHeap.DecreasePriority(Index : Integer; APriorityValue : double) : Integer;
begin
  Result := 0;
  if Self.Priority[Index] = cNone then Result := - 1; // value is not in queue;
  if Self.Priority[Index] < APriorityValue then Result := -2; // existing priority is lower

  if (Result < 0) then Exit;

  Self.Priority[Index] := APriorityValue;
  MoveUp(Self.Position[Index]);
end;

function TPriorityQueueHeap.getParentIndex(Index : Integer) : Integer;
begin
  Result := (Index - 1) div 2;
end;

function TPriorityQueueHeap.getLeftChildIndex(Index : Integer) : Integer;
begin
  Result := 2 * Index + 1;
end;

function TPriorityQueueHeap.getRightChildIndex(Index : Integer) : Integer;
begin
  Result := 2 * Index + 2;
end;

{%endregion%}

{%region=====[ TNode ]==========================================================}

constructor TBZClassNode.Create(AOwner: TBZGraphNode; const QIDStation: TIDStation; const QPosition: TPoint3Df; const QMetaData: string);
begin
  FOwner := AOwner;
  FIDStation   := QIDStation;
  FPosition    := Position;
  FNodeLinkList := TBZNodeLinkList.Create(Self);
  FNodeIndex := -1;
  FMetaData  := QMetaData;
  //if Assigned(FOwner) or (FOwner <> nil) then
  //begin
  //  Idx := FOwner.AddNode(Self);
  //  FNodeIndex := Idx;
  //end;
end;

destructor TBZClassNode.Destroy;
begin
  FreeAndNil(FNodeLinkList);
  inherited Destroy;
end;

function TBZClassNode.GetNodeLink(const Index : Integer): TBZNodeLink;
begin
  Result := FNodeLinkList.Items[Index];
end;

procedure TBZClassNode.SetNodeLink(Index : Integer; const AValue: TBZNodeLink);
begin
  Assert(((Index >= FNodeLinkList.Count)  or (Index < 0)), 'EdgeLIst : Index hors limite');
  AValue.Owner := FNodeLinkList;
  FNodeLinkList.Items[Index] := AValue;
end;

procedure TBZClassNode.SetOwner(const AValue: TBZGraphNode);
begin
  if FOwner = AValue then Exit;
  FOwner := AValue;
end;

function TBZClassNode.AddNodeLink(anIndex: Integer; const Weight: double): TBZNodeLink;
Var
 aNodeLink : TBZNodeLink;
begin
  Assert(((anIndex >= FOwner.Count) or (anIndex < 0)), 'Node :  Edge Node Index hors limite');
  aNodeLink := TBZNodeLink.Create(FNodeLinkList, anIndex);
  FNodeLinkList.AddNodeLink(aNodeLink);
  Result := aNodeLink;
end;

function TBZClassNode.ToString(): string;
var
  QSR: TNumeroSerie;
  QSt: TNumeroStation;
begin
  ExtractSerStFromTIDStation(self.IDStation, QSR, QSt);
  Result := Format(FMTSERST, [QSR, QST]);
end;


{%endregion%}

{%region=====[ TNodeLink ]======================================================}

constructor TBZNodeLink.Create(AOwner: TBZNodeLinkList; ATargetNodeIndex: Integer);
begin
  FOwner := AOwner;
  FTargetNodeIndex := ATargetNodeIndex;
end;

destructor TBZNodeLink.Destroy;
begin
  inherited Destroy;
end;

procedure TBZNodeLink.SetOwner(const AValue: TBZNodeLinkList);
begin
  if FOwner = AValue then Exit;
  FOwner := AValue
end;

function TBZNodeLink.getDistance(): double;
var
  QParentNode, QCurrNode: TBZClassNode;
begin
  QParentNode := getParentNode();
  QCurrNode   := getNode();
  Result := sqrt((QCurrNode.Position.X - QParentNode.Position.X) ** 2 +
                 (QCurrNode.Position.Y - QParentNode.Position.Y) ** 2 +
                 (QCurrNode.Position.Z - QParentNode.Position.Z) ** 2);
  //GetBearingInc();

end;

function TBZNodeLink.getNode: TBZClassNode;
begin
  result :=FOwner.Owner.Owner.Node[FTargetNodeIndex];
end;

function TBZNodeLink.getParentNode: TBZClassNode;
begin
  Result := FOwner.Owner;
end;

procedure TBZNodeLink.SetTargetNodeIndex(const AValue: Integer);
begin
  if FTargetNodeIndex = AValue then Exit;
  FTargetNodeIndex := AValue;

end;

{%endregion%}

{%region=====[ TNodeLinkList ]==================================================}

constructor TBZNodeLinkList.Create(AOwner: TBZClassNode);
begin
  inherited Create(16);
  FOwner := AOwner;
end;

destructor TBZNodeLinkList.Destroy;
begin
  inherited Destroy;
end;

function TBZNodeLinkList.NewNodeLink(ToIndex: Integer): TBZNodeLink;
Var
  aNodeLink : TBZNodeLink;
begin
  aNodeLink := TBZNodeLink.Create(Self, ToIndex);
  Self.AddNodeLink(aNodeLink);
  Result := aNodeLink;
end;

function TBZNodeLinkList.AddNodeLink(aNodeLink: TBZNodeLink): Integer;
begin
  aNodeLink.Owner := Self;
  Result := Self.Add(aNodeLink);
end;

procedure TBZNodeLinkList.SetOwner(const AValue: TBZClassNode);
begin
  if FOwner = AValue then Exit;
  FOwner := AValue;
end;

function TBZNodeLinkList.CompareValue(Const elem1, elem2: TBZNodeLink): Integer;
begin
  if      elem1.TargetNodeIndex = elem2.TargetNodeIndex then Result :=  0
  else if elem1.TargetNodeIndex < elem2.TargetNodeIndex then Result := -1
  else                                                       Result :=  1;
end;

{%endregion%}

{%region=====[ TGraphNode ]=====================================================}

constructor TBZGraphNode.Create;
begin
  inherited Create(16);
end;

destructor TBZGraphNode.Destroy;
begin
  inherited Destroy;
end;

function TBZGraphNode.getNode(Index : Integer): TBZClassNode;
begin
  Result := Self.Items[Index];
end;

procedure TBZGraphNode.setNode(Index : Integer; const AValue: TBZClassNode);
begin
  Assert(((Index >= Self.Count) or (Index < 0)), 'GraphNode : Index hors limite');
  Self.Items[Index] := AValue;
end;

function TBZGraphNode.CompareValue(Const elem1, elem2: TBZClassNode): Integer;
begin
  if      elem1.FIDStation = elem2.FIDStation then Result :=  0
  else if elem1.FIDStation < elem2.FIDStation then Result := -1
  else                                             Result :=  1;
end;

procedure TBZGraphNode.DoOnTravelNode(Sender : TObject; Anode: TBZClassNode; ADistance: double);
begin
  if Assigned(FOnGraphNodeTravelNode) then FOnGraphNodeTravelNode(Self, ANode, ADistance);
end;

procedure TBZGraphNode.DoVerboseCall(const s: String);
begin
  if Assigned(FOnGraphNodeVerboseCall) then FOnGraphNodeVerboseCall(Self, s);
end;

function TBZGraphNode.AddNode(const QIDStation: TIDStation; const QPosition: TPoint3Df; const QMetaData: string): TBZClassNode;
Var
  aNode : TBZClassNode;
  Idx : Integer;
begin
  aNode := TBZClassNode.Create(Self, QIDStation, QPosition, QMetaData);
  aNode.IDStation := QIDStation;
  aNode.Position := QPosition;
  Idx := Self.AddNode(aNode);
  aNode.NodeIndex := Idx;
  Result := aNode;
end;

function TBZGraphNode.AddNode(ANode: TBZClassNode): Integer;
begin
  Result := Self.Add(aNode);
end;

function TBZGraphNode.Dijkstra(FromIndex, ToIndex: Integer; out ShortestPath: TListOfIntegers): double;
var
  queue : TPriorityQueueHeap;
  pred : array of integer;
  i, j, cnt : integer;
  edge : TBZNodeLink;
  Dist, DMin : Double;
  DistTmp : Double;
begin
  ShortestPath := TListOfIntegers.Create;  // {$ERROR: Remplacer le TBZIntegerList par un simple TList}
  ShortestPath.ClearListe();

  cnt := Self.Count;
  AfficherMessageErreur(Format('Dijkstra: %d', [cnt]));
  Queue := TPriorityQueueHeap.Create(cnt);
  Dist := 0.00;
  for i := 0 to (cnt - 1) do
  begin
    Queue.InsertPriority(i, MaxInt);
  end;
  setLength(pred{%H-}, cnt);
  Queue.DecreasePriority(FromIndex, 0);
  i := FromIndex;
  // DoOnTravelNode est une fonction de documentation, sans utilité pratique
  //DoOnTravelNode(Self, Self.Items[i], Dist);

  // Première partie : recherche des chemins les plus court pour chaque noeud
  while not(Queue.isEmpty) do
  begin
    i := Queue.RemoveSmallest({ out } Dist);
    //DoOnTravelNode(Self, Self.Items[i], Dist);
    if (i = ToIndex) then break;
    if (Self.Items[i].NodeLinkList.Count > 0) then
    begin
      for j := 0 to (Self.Items[i].NodeLinkList.Count - 1) do
      begin
        Edge := Self.Items[i].LinkNode[j];
        DistTmp := Dist + Edge.Distance;
        if (Queue.IsExist(Edge.TargetNodeIndex) and (DistTmp < Queue.getPriority(Edge.TargetNodeIndex))) then
        begin
          Queue.DecreasePriority(Edge.TargetNodeIndex, DistTmp);
          pred[Edge.TargetNodeIndex] := i;
          DMin := DistTmp;
        end;
      end;
    end;
  end;

  // Deuxieme partie : Extraction du chemin le plus court trouvé
  AfficherMessageErreur(Format('Dijkstra: 2e partie: %d', [ToIndex, DistTmp]));
  if (i = ToIndex) and (DistTmp < (1.00 * MaxInt)) then
  begin
    Result := DistTmp;
    i := ToIndex;
    While (pred[i] <> i) and (i <> FromIndex) do
    begin
      ShortestPath.AddElement(i);
      i := pred[i];
    end;
    ShortestPath.AddElement(FromIndex);
  end
   else result := -1.01;
  cnt := ShortestPath.GetNbElements();
  AfficherMessageErreur(Format('Dijkstra: %d chemins', [cnt]));

  FreeAndNil(Queue);
end;
// Spécifique GHTopo




end.

{%endregion%}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FGraphNode := TGraphNode.Create;
  FGraphNode.OnTravelNode := @DoOnTavelNode;
  FGraphNode.OnVerboseCall := @DoOnVerboseCall;
  FDisplayBuffer := TBZBitmap.Create(PnlView.Width, pnlView.height);
  FSelectedNodeIndex := -1;
  FSelectedNodeLinkIndex := -1;
  FRenderShortestPath := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FShortestPath) then  FreeAndNil(FShortestPath);
  FreeAndNil(FDisplayBuffer);
  FreeAndNil(FGraphNode);
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  RenderGraph;
  FDisplayBuffer.DrawToCanvas(pnlView.Canvas, pnlView.ClientRect);
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMousePos.Create(x,y);

  if (ssLeft in Shift) then
  begin
    FMouseDown := True;
    if (ssShift in Shift) then
    begin
      if  (FSelectedNodeIndex <> -1) then FNodeAction := naAddLink
      else FNodeAction := naNone;
    end
    else
    begin
      if (FSelectedNodeIndex = -1) then
      begin
        FNodeAction := naAdd;
      end
      else FNodeAction := naNone;
    end;
  end
  else FNodeAction := naNone;
end;
end.
//******************************************************************************

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
 NodePos : TBZVector;
 Node : TNode;
 NodeToLink : TNode;
begin
  if FMouseDown then
  begin
    if (FNodeAction = naAdd) then
    begin
      FMousePos.Create(x,y);
      NodePos.CreatePoint(x, y, 0);
      AddNode(NodePos);
    end
    else if (FNodeAction = naAddLink) then
    begin
      if (FSelectedNodeLinkIndex <> -1) then
      begin
        Node := FGraphNode.Node[FSelectedNodeIndex];
        NodeToLink := FGraphNode.Node[FSelectedNodeLinkIndex];
        LinkNode(Node, NodeToLink, False);
      end;
    end;
  end;
  FSelectedNodeIndex := -1;
  FSelectedNodeLinkIndex := -1;
  FNodeAction := naNone;
  FMouseDown := False;
  pnlView.Repaint;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  idx : Integer;
  Node : TNode;
  np : TBZPoint;
begin
  FMousePos.Create(x,y);
  if FMouseDown then
  begin
    if ((Shift = [ssLeft, ssShift]) and (FNodeAction =  naAddLink)) then
    begin
      FSelectedNodeLinkIndex := -1;
      for idx := 0 to FGraphNode.Count-1 do
      begin
        Node := FGraphNode.Node[Idx];
        np := Node.Position.XY.Round;
        if PointInCircle(FMousePos.X, FMousePos.Y, np.X, np.Y, 8) then //(FMousePos.AsVector2f.InCircle(Node.Position.XY, 8)) then
        begin
          FSelectedNodeLinkIndex := Idx;
          Break;
        end;
      end;
    end;
  end
  else
  begin
    FSelectedNodeIndex := -1;
    for idx := 0 to FGraphNode.Count-1 do
    begin
      Node := FGraphNode.Node[Idx];
      np := Node.Position.XY.Round;
      if PointInCircle(FMousePos.X, FMousePos.Y, np.X, np.Y, 8) then //(FMousePos.AsVector2f.InCircle(Node.Position.XY, 8)) then
      begin
        FSelectedNodeIndex := Idx;
        Break;
      end;
    end;
  end;
  pnlView.Repaint;
end;

procedure TMainForm.btnFindShortestPathClick(Sender: TObject);
Var
  i, j : Integer;
  Node : TNode;
  NodeLink : TNodeLink;
  s : String;
  Dist : Single;
begin
  FRenderShortestPath := False;
  mmoDebug.clear;
  mmoDebug.Lines.Add('==========================================================');
  mmoDebug.Lines.Add('Graph construit avec ' + (FGraphNode.count.ToString) + ' noeuds');
  mmoDebug.Lines.Add('==========================================================');
  mmoDebug.Lines.Add('');
  for i := 0 to (FGraphNode.count - 1) do
  begin
    Node := FGraphNode.Items[i];
    mmoDebug.Lines.Add('Noeud ' + i.ToString);
    if (Node.NodeLinkList.Count > 0) then
    begin;
      for j := 0 to (Node.NodeLinkList.Count - 1) do
      begin
        mmoDebug.Lines.Add('----------------------------------------------------------');
        NodeLink := Node.LinkNode[j];
        mmoDebug.Lines.Add('   Lien ' + j.ToString);
        mmoDebug.Lines.Add('     -> vers le noeud : '+ NodeLink.TargetNodeIndex.ToString);
        mmoDebug.Lines.Add('             Distance : ' + NodeLink.Distance.ToString);
      end;
    end;
    mmoDebug.Lines.Add('==========================================================');
  end;
  mmoDebug.Lines.Add('');
  mmoDebug.Lines.Add('Début la recherche du chemin le plus court');
  mmoDebug.Lines.Add('==========================================================');
  Dist := FGraphNode.Dijkstra(speFromNodeIndex.Value, speToNodeIndex.Value, FShortestPath);
  mmoDebug.Lines.Add('Fin la recherche du chemin le plus court');
  mmoDebug.Lines.Add('==========================================================');
  if (Dist > 0) then
  begin
    mmoDebug.Lines.Add('Le plus court chemin du noeud ' + speFromNodeIndex.Value.ToString + ' vers le noeud ' + speToNodeIndex.Value.ToString);
    s := '';
    for i :=(FShortestPath.Count - 1) downto 0  do
    begin
      s := s + FShortestPath.Items[i].ToString;
      if (i > 0) then s := s + ', ';
    end;
    mmoDebug.Lines.Add('Passe par les noeuds : ' + s);
    mmoDebug.Lines.Add('La distance total est de : ' + Dist.ToString);
    FRenderShortestPath := True;
    pnlView.Repaint;
  end
  else
  begin
    mmoDebug.Lines.Add('Aucun chemin trouvé');
  end;
end;

procedure TMainForm.DoOnTavelNode(Sender: TObject; Node: TNode; ADistance: Single);
var
  s : String;
begin
  s:= '[INFO] Traitement du noeuds ' + Node.NodeIndex.ToString + ' - Distance par rapport au point de départ : ' + ADistance.ToString;
  mmoDebug.Append(s);
end;

procedure TMainForm.DoOnVerboseCall(Sender: TObject; msg: String);
begin
  mmoDebug.Append(msg);
end;

procedure TMainForm.BZThreadTimer1Timer(Sender: TObject);
begin
  //RenderGraph;
  pnlView.Repaint;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //BZThreadTimer1.Enabled := True;
end;

procedure TMainForm.AddNode(aPosition: TBZVector);
Var
 aNode : TNode;
begin
  aNode := FGraphNode.AddNode(aPosition);
  FSelectedNodeIndex := aNode.NodeIndex;
  Caption := 'Ajout d''un noeud à l''index : ' + FSelectedNodeIndex.ToString + '/' + FGraphNode.Count.ToString;
  speToNodeIndex.Value := FSelectedNodeIndex;
end;

procedure TMainForm.RemoveNode(Index: Integer);
begin
  FGraphNode.Delete(Index);
end;

procedure TMainForm.LinkNode(FromNode, ToNode: TNode; Const Bidi: Boolean);
begin
  FromNode.AddNodeLink(ToNode.NodeIndex);
  if Bidi then ToNode.AddNodeLink(FromNode.NodeIndex);
end;

procedure TMainForm.MoveNode(aNode: TNode; NewPosition: TBZVector);
begin
  FGraphNode.Node[aNode.NodeIndex].Position := NewPosition;
end;

function TMainForm.FindNodeFromPosition(aPosition: TBZVector): TNode;
Var
 aNode : TNode;
begin
  Result := Nil;
  FGraphNode.MoveFirst;
  aNode := FGraphNode.Current;
  While Not(FGraphNode.IsEndOfArray) do
  begin
    if (aNode.Position = aPosition) then
    begin
      Result := aNode;
      Break;
    end;
    FGraphNode.MoveNext;
    aNode := FGraphNode.Current;
  end;
end;

procedure TMainForm.RenderGraph;
Var
 idx : Integer;
 Node  : TNode;
 NodeLink : TNodeLink;

 pA, pB : TBZFloatPoint;

  procedure RenderNode(aNode : TNode; IsSelected : Boolean);
  begin
    with FDisplayBuffer.Canvas do
    begin
      Antialias := True;
      Pen.Style := ssSolid;
      if IsSelected then Pen.Color := clrAqua else Pen.Color := clrWhite;
      Brush.Style := bsSolid;
      if IsSelected then Brush.Color := clrYellow else Brush.Color := clrSilver;
      Circle(aNode.Position.XY, 8);
      Font.Color := clrBlack;
      Font.Size := 8;
      TextOut(Round(aNode.Position.XY.X - ((4 * aNode.NodeIndex.ToString.Length) - 2)), Round(aNode.Position.XY.Y + 4), aNode.NodeIndex.ToString);
    end;
  end;

  procedure RenderRubberLineLink;
  begin
    if (FNodeAction = naAddLink) then
    begin
      //(FSelectedNodeIndex <> -1) then
      begin
        Node := FGraphNode.Node[FSelectedNodeIndex];
        with FDisplayBuffer.Canvas do
        begin
          Pen.Style := ssSolid;
          Pen.Width := 3;
          Pen.Color := clrSkyBlue;
          pA := Node.Position.XY;
          pA.x := pA.x - 1;
          MoveTo(pA);
          pB := FMousePos.AsVector2f;
          LineTo(pB);
          //if (FSelectedNodeLinkIndex <> -1) then LineTo(NodeLink.Position.XY) else
        end;
      end;
    end;
  end;

  procedure RenderNodeLinks;
  Var
   i, j, k  : Integer;
   isBidi : Boolean;
  begin
    for i := 0 to (FGraphNode.Count - 1) do
    begin
      Node := FGraphNode.Node[i];
      if (Node.NodeLinkList.Count > 0) then
      begin

        for j := 0 to (Node.NodeLinkList.Count - 1) do
        begin
          NodeLink := Node.LinkNode[J];
          isBidi := false;
          if (NodeLink.Node.NodeLinkList.Count > 0) then
          begin
            for k := 0 to (NodeLink.Node.NodeLinkList.Count - 1) do
            begin
              if (NodeLink.Node.LinkNode[k].TargetNodeIndex = Node.NodeIndex) then
              begin
                isBidi := True;
                Break;
              end;
            end;
          end;

          with FDisplayBuffer.Canvas do
          begin
            Pen.Style := ssSolid;
            Pen.Width := 3;
            pA := Node.Position.XY;
            pB := NodeLink.Node.Position.XY;
            if not(IsBidi) then
            begin
              Pen.Color := clrOrange;
              MoveTo(pA);
              LineTo(pB);
            end
            else
            begin
              Pen.Color := clrBlue;
              MoveTo(pA);
              LineTo(pB);
            end;
          end;
        end;
      end;
    end;
  end;

  procedure RenderShortestPath;
  var
    i : Integer;
  begin

    for i := 1 to (FShortestPath.Count - 1) do
    begin
      with FDisplayBuffer.Canvas do
      begin
         Pen.Style := ssSolid;
         Pen.Width := 3;
         pA := FGraphNode.Items[FShortestPath.Items[i - 1]].Position.XY;
         pB := FGraphNode.Items[FShortestPath.Items[i]].Position.XY;
         Pen.Color := clrLightMagenta;
         MoveTo(pA);
         LineTo(pB);
      end;
    end;
  end;

begin
  FDisplayBuffer.RenderFilter.DrawGrid(clrGray15, clrGray35, clrGray25, clrRed, clrGreen, 10);

  if FGraphNode.Count > 0 then
  begin
    RenderNodeLinks;
    RenderRubberLineLink;
    if FRenderShortestPath then RenderShortestPath;
    for idx := 0 to FGraphNode.Count-1 do
    begin
      Node := FGraphNode.Node[Idx];
      if (Idx = FSelectedNodeIndex) or (Idx = FSelectedNodeLinkIndex) then RenderNode(Node, True)
      else RenderNode(Node, False);
    end;
  end;
end;


{%endregion%}

end.


