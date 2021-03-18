//
//  Taken from Nicklaus Wirth :
//    Algorithmen und Datenstrukturen ( in Pascal )
//    Balanced Binary Trees p 250 ++
//
//
// Fixed By Giacomo Policicchio
// pgiacomo@tiscalinet.it
// 19/05/2000
//
{$objmode delphi}

unit UnitBinaryTree;
// Fonctionnement OK mais très lent

interface


uses
  sysutils, classes, StdCtrls;
const ROW_MULT = 100000;
type TData = record
  Key: Int64;
  Value: double;
end;

type

{ TBinTreeItem }

 TBinTreeItem = class
  left,right:TBinTreeItem;
  bal:-1..1;
  private
   count:integer;

  public
   Data: TData;
   constructor create(const D: TData);
   function compare(a:TBinTreeItem):Shortint;
   // a < self :-1  a=self :0  a > self :+1
   procedure copy(ToA:TBinTreeItem);
   procedure list(const M: TMemo);
 end;

 type

 { TBinTree }

 TBinTree = class(TPersistent)
  private
   ItemCount:integer;
   root:TBinTreeItem;
   FDataFound: TBinTreeItem;
   procedure Delete(item:TBinTreeItem;var p:TBinTreeItem;var h:boolean;var ok:boolean);
   procedure SearchAndInsert(item:TBinTreeItem;Var p:TBinTreeItem;var h:boolean;Var Found:boolean);
   function SearchItem(item:TBinTreeItem;Var p:TBinTreeItem):boolean;
   procedure balanceLeft(var p:TBinTreeItem;var h:boolean;dl:boolean);
   procedure balanceRight(var p:TBinTreeItem;var h:boolean;dl:boolean);
   procedure listitems(const M: TMemo; var p:TBinTreeItem);
  public
   constructor create;
   destructor destroy;
   Function add(item:TBinTreeItem):boolean;
   Function remove(item:TBinTreeItem):boolean;
   function search(item:TBinTreeItem):boolean;
   procedure list(const M: TMemo);      // uses item.list through listitems recursively
   function GetLastElementFound(): TBinTreeItem;
   procedure SetValue(const m, n: integer; const V: double);
   function GetValue(const m, n: integer): Double;

  end;


implementation

//=================================================================
constructor TBinTreeItem.create(const D: TData);
begin
 Data := D;
 count := 0;
end;

function TBinTreeItem.compare(a: TBinTreeItem): Shortint;
begin
 if a.data.Key < self.data.Key then result:=-1
 else
  if a.data.Key = self.data.Key then result:=0
  else
   if a.data.Key > self.data.Key then result:=1;
end;

procedure TBinTreeItem.copy(ToA: TBinTreeItem);
begin
   ToA.data:= self.Data;
end;

procedure TBinTreeItem.list(const M: TMemo);
begin
  M.lines.add(Format('%d - %.3f', [data.Key, data.Value]));
end;

//=================================================================

constructor TBinTree.create;
begin
 inherited create;
 root:=nil;
 FDataFound := nil;
 ItemCount:=0;
end;

destructor TBinTree.destroy;
begin
 while root <> nil do remove(root);
 inherited destroy;
end;

procedure TBinTree.SearchAndInsert(item: TBinTreeItem; var p: TBinTreeItem;
  var h: boolean; var Found: boolean);
begin
 found:=false;
 if p=nil then begin        // word not in tree, insert it
   p:=item;
   h:=true;
   with p do
    begin
     if root=nil then root:=p;
     count:=1;
     left:=nil; right:=nil; bal:=0;
    end;
  end
 else
 if (item.compare(p) > 0) then      // new < current
  begin
   searchAndInsert(item,p.left,h,found);
   if h and not found then BalanceLeft(p,h,false);
  end
 else
  if (item.compare(p) < 0) then     // new > current
   begin
    searchAndInsert(item,p.right,h,found);
    if h and not found then balanceRight(p,h,false);
   end
  else
   begin
    p.count:=p.count+1;
    h:=false;
    found:=true;
   end;
end;      //searchAndInsert

// returns true and a pointer to the equal item if found, false otherwise
function TBinTree.SearchItem(item: TBinTreeItem; var p: TBinTreeItem): boolean;
begin
  result:=false;
  FDataFound := nil;
  if (p=nil) then result:=false // empty
  else begin
    if (item.compare(p) =0) then
    begin
      FDataFound := p;
      result := true;
    end
    else
    begin
      if (item.compare(p) >0) then result:=searchitem(item,p.left)
      else begin
        if (item.compare(p) <0) then result:=searchitem(item,p.right)
      end;
    end;
  end;
end;


procedure TBinTree.balanceRight(var p: TBinTreeItem; var h: boolean; dl: boolean
  );
var p1,p2:TBinTreeItem;
Begin
  case p.bal of
      -1:begin
          p.bal:=0;
          if not dl then h:=false;
         end;
      0: begin
          p.bal:=+1;
          if dl then h:=false;
         end;
      +1:begin    // new balancing
            p1:=p.right;
            if (p1.bal=+1) or ((p1.bal=0) and dl) then begin  // single rr rotation
              p.right:=p1.left; p1.left:=p;
              if not dl then p.bal:=0
                        else begin
                              if p1.bal=0 then
                               begin
                                p.bal := +1; p1.bal := -1; h := false;
                               end
                              else begin
                                p.bal :=  0; p1.bal := 0;
                                (* h:=false; *)
                               end;
                             end;
              p := p1;
           end
          else begin  // double rl rotation
            p2:=p1.left;
            p1.left:=p2.right;
            p2.right:=p1;
            p.right:=p2.left;
            p2.left:=p;
            if p2.bal=+1 then p.bal:=-1 else p.bal:=0;
            if p2.bal=-1 then p1.bal:=+1 else p1.bal:=0;
            p:=p2;
            if dl then p2.bal:=0;
           end;
          if not dl then begin
            p.bal:=0;
            h:=false;
           end;
         end;
     end; // case
End;

procedure TBinTree.balanceLeft(var p:TBinTreeItem;var h:boolean;dl:boolean);
var p1,p2:TBinTreeItem;
Begin
    case p.bal of
     1:begin
        p.bal:=0;
        if not dl then h:=false;
       end;
     0:begin
        p.bal:=-1;
        if dl then  h:=false;
       end;
     -1:(* if (p.Left<>nil) or not dl then *)
        begin   // new balancing
         p1:=p.left;
         if (p1.bal=-1) or ((p1.bal=0) and dl) then begin   // single ll rotation
           p.left:=p1.right;p1.right:=p;
           if not dl then p.bal:=0
                     else begin
                           if p1.bal=0 then begin
                             p.bal:=-1;
                             p1.bal:=+1;
                             h:=false;
                            end
                           else begin
                             p.bal:=0;
                             p1.bal:=0;
                             (* h:=false; *)
                            end;
                          end;
           p:=p1;
          end
         else
          begin //double lr rotation
            p2:=p1.right;
            P1.Right:=p2.left;
            p2.left:=p1;
            p.left:=p2.right;
            p2.right:=p;
           if p2.bal=-1 then  p.bal:=+1 else p.bal:=0;
           if p2.bal=+1 then  p1.bal:=-1 else p1.bal:=0;
           p:=p2;if dl then p2.bal:=0;
          end;
           if not dl then begin
             p.bal:=0;
             h:=false;
            end;
        end; { -1 }
    end; { case }
End;


procedure TBinTree.Delete(item:TBinTreeItem;var p:TBinTreeItem;var h:boolean;var ok:boolean);
var q:TBinTreeItem; //h=false;

 procedure del(var r:TBinTreeItem;var h:boolean);
 begin //h=false
  if r.right <> nil then
   begin
    del(r.right,h);
    if h then balanceLeft(r,h,True);
   end
  else
   begin
    r.copy(q);  { q.key:=r.key; }
    q.count:=r.count;
    q:=r;
    r:=r.left;h:=true;
   end;
 end;


begin { main of delete }
 ok:=true;
 if (p=nil) then
  begin
   Ok:=false;h:=false;
  end
 else
  if (item.compare(p) > 0){(x < p^.key)} then
   begin
    delete(item,p.left,h,ok);
    if h then balanceRight(p,h,True);
   end
  else
   if (item.compare(p) < 0){(x > p^.key)}then
    begin
     delete(item,p.right,h,ok);
     if h then balanceLeft(p,h,True);
    end
   else
    begin // remove q
     q:=p;
     if q.right=nil then
      begin
       p:=q.left;h:=true;
      end
     else
      if (q.left=nil) then
       begin
        p:=q.right;h:=true;
       end
      else
       begin
        del(q.left,h);
        if h then balanceRight(p,h,True);
       end;
      q.free; {dispose(q)};
    end;
end; { delete }

function TBinTree.add(item: TBinTreeItem): boolean;
var h,found:boolean;
begin
 SearchAndInsert(item,root,h,found);
 add:=found;
end;

function TBinTree.remove(item: TBinTreeItem): boolean;
var h,ok:boolean;
begin
 Delete(item,root,h,ok);
 remove:=ok;
end;

function TBinTree.search(item: TBinTreeItem): boolean;
var h,ok:boolean;
begin
 result := SearchItem(item, root);
end;

procedure TBinTree.listitems(const M: TMemo; var p: TBinTreeItem);
begin
 if p<>nil then begin
  if (p.left <> nil) then listitems(M, p.left);
  p.list(M);
  if (p.right <> nil) then listitems(M, p.right);
 end;
end;

procedure TBinTree.list(const M: TMemo);      // uses item.list recursively
begin
 listitems(M, root);
end;

function TBinTree.GetLastElementFound: TBinTreeItem;
begin
  Result := FDataFound;
end;
function TBinTree.GetValue(const m, n: integer): Double;
var
  EWE: TData;
  bti: TBinTreeItem;
begin
  Result := 0.00;
  EWE.Key   := m * ROW_MULT + n;
  EWE.Value := 0;
  bti := TBinTreeItem.create(EWE);
  try
    if (search(bti)) then Result := FDataFound.Data.Value;
  finally
    bti.Free;
  end;
end;

procedure TBinTree.SetValue(const m, n: integer; const V: double);
var
  EWE: TData;
  bti: TBinTreeItem;
begin
  EWE.Key   := m * ROW_MULT + n;
  EWE.Value := V;
  bti := TBinTreeItem.create(EWE);
  try
    if (not search(bti)) then
    begin
      self.add(bti)
    end
    else
    begin
      self.remove(bti);
      self.add(bti);
    end;

  finally
    //bti.Free;
  end;
end;

end.
