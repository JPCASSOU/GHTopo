unit DGCUnitListeAffichage;
// Gestion de la liste d'affichage et de commandes

{$mode delphiunicode}

interface

uses
  DGCTypes,
  DGCUtilityFunctions,
  Classes, SysUtils;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+ liste d'affichage et de commandes                                          +
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
type  TDGCListeAffichage= class(TList)
  private

  public
    function GetNbElements(): integer;
    procedure ClearListe();
    function AddElement(const QType: TDGCTypeDessinObjet; const QName: string; const Obj: TObject): boolean; overload;
    function AddElement(const Obj: TDGCDessinObjet): boolean; overload;
    function GetElement(const Idx: integer): TDGCDessinObjet;
    function RemoveElement(const Idx: integer): boolean;
end;

implementation
uses
  DGCDummyUnit; // pour éviter le bug de 'Fin de code source non trouvée

{ TDGCListeDessinObjets }
function TDGCListeAffichage.AddElement(const QType: TDGCTypeDessinObjet;
                                       const QName: string;
                                       const Obj: TObject): boolean;
var
  MyObj: TDGCDessinObjet;
begin
  result := false;
  MyObj.TypeDessinObjet := QType;
  MyObj.Name            := QName;
  MyObj.DessinObjet     := Obj;
  result := self.AddElement(MyObj);
end;


function TDGCListeAffichage.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TDGCListeAffichage.ClearListe();
var
  i, Nb: Integer;
  MyObject: TDGCDessinObjet;
begin
  Nb := self.Count;
  if (0 = Nb) then Exit;
  for i := 0 to Nb - 1 do
  begin
    try
      MyObject := GetElement(i);
      FreeAndNil(MyObject.DessinObjet);
      Dispose(self.Items[i]);
    except
    end;
  end;
  self.Clear();
end;



function TDGCListeAffichage.AddElement(const Obj: TDGCDessinObjet): boolean;
var
  pS : ^TDGCDessinObjet;
begin
  result := false;
  try
    New(pS);
    pS^ := Obj;
    self.Add(pS);
    Result := True;
  except
  end;
end;

function TDGCListeAffichage.GetElement(const Idx: integer): TDGCDessinObjet;
begin
  Result := TDGCDessinObjet(self.Items[Idx]^);
end;


function TDGCListeAffichage.RemoveElement(const Idx: integer): boolean;
var
  MyObject: TDGCDessinObjet;
begin
  Result := False;
  try
    MyObject := self.GetElement(Idx);
    FreeAndNil(MyObject.DessinObjet);
    Dispose(self.Items[Idx]);
  finally
    self.Delete(Idx);
    Result := True;
  end;
end;

end.

