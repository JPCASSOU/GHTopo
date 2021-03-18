unit UnitHelpSystem;
{$INCLUDE CompilationParameters.inc}
// 24/12/2013: Version sans génération de fichier temporaire
interface

uses
  Common,
  SysUtils, Classes, FileUtil;

// constantes pour la lecture du fichier
const
  BEGINSECTION = '<Section>';
  BEGININDEX   = '<Index>';
  BEGINTOPICS  = '<Topics>';
  BEGINTITLE   = '<Title>';
  BEGINTEXTE   = '<Texte>';

// objet Section
type TSection = record
  Index   : Integer;
  Topics  : string;
  Title   : string;
  Texte   : string;
end;
// liste des sections
type

{ TListeSections }

 TListeSections = class(TList)
  private

  public
    constructor Create;
    destructor Destroy; override;
    procedure  ClearListe;

end;

// objet HelpFile
type

{ THelpFileStructure }

 THelpFileStructure = class

  private
    FListeSections: TListeSections;
    FCurrentIndex: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSection(const S: TSection);
    function  GetSection(const I: integer):TSection;
    function SearchIndexSection(const Topix: string): integer;
    function LoadFromFile(const FichierAide: string): Boolean;

    function GetNbreSections: integer;
end;


implementation

//-----------------------
// Classe TListeSections
constructor TListeSections.Create;
begin
  inherited Create;
  AfficherMessage(Format('--> %s.Create',[ClassName]));
end;
destructor TListeSections.Destroy;
begin
  AfficherMessage(Format('--> %s.Destroy',[ClassName]));
  inherited Destroy;
end;

procedure TListeSections.ClearListe;
var
  i: Integer;
begin
  try
    for i := 0 to self.Count - 1 do Dispose(Items[i]);
  finally
    self.Clear;
  end;
end;

//------------------------------------
// Classe THelpFileStructure
procedure THelpFileStructure.AddSection(const S: TSection);
var
  pS: ^TSection;
begin
  with FListeSections do
  begin
    New(pS);
    pS^:=S;
    Add(pS);
  end;
end;
function THelpFileStructure.GetSection(const I: integer):TSection;
var
  q: integer;
  pS:^TSection;
  WU: Integer;
begin
  WU := GetNbreSections;
  with FListeSections do
  begin
    if (WU = 0) then Exit;
    q := i;
    if (q < 0) then q :=0;
    if (q > (WU - 1)) then q := WU - 1;
    pS := Items[q];
    Result := pS^;
  end;
end;


//------------------------------------
// Classe THelpFileStructure
constructor THelpFileStructure.Create;
begin
  inherited Create;
  AfficherMessage(Format('%s.Create',[ClassName]));
  FListeSections:=TListeSections.Create;
  FListeSections.Clear;

end;
destructor THelpFileStructure.Destroy;
begin
  with FListeSections do
  begin
    ClearListe;
    Free;
  end;
  AfficherMessage(Format('%s.Destroy',[ClassName]));
  inherited Destroy;
end;
//-----------------------------
// DONE: Implémenter nouvelle version sans génération de tempfile.
function THelpFileStructure.LoadFromFile(const FichierAide: string): boolean;
var
  LS666: TStringList;
  Sect: TSection;
  i1, NbLines: integer;
  Line: string;
  BalEnd2: string;
  EWE: String;
  BalEnd1: String;
  function SetEnBalise(const Balise: string): string;
  var S1: string;
  begin
    S1:=Balise;
    Insert('/', S1, 2);
    Result:=S1;
  end;
  function HasBalise(const S, Balise: string): boolean;
  begin
    Result:=(Pos(Balise, S)>0);
  end;
  function ExtractText(const S, Balise: string): string;
  var
    P,Q: integer;
  begin
    P:=Pos(Balise, S) + Length(Balise);
    Q:=Pos(SetEnBalise(Balise), S);
    Result:=Trim(Copy(S,P, Q-P));
  end;
begin
  Result:=False;
  //{*
  AfficherMessage(Format('--> LoadFromFile: %s',[FichierAide]));
  if Not(FileExistsUTF8(FichierAide)) then Exit;
  //AssignFile(pHelpFile, FFichierHLP);
  BalEnd1 := SetEnBalise(BEGINSECTION);
  BalEnd2 := SetEnBalise(BEGINTEXTE);
  i1 := 0;
  LS666   := TStringList.Create;
  try
    LS666.Clear;
    try
      // chargement du fichier
      LS666.LoadFromFile(FichierAide);
      // interprétation
      while i1 < (LS666.Count - 1) do
      begin
        Line := LS666.Strings[i1];  i1 += 1;
        if (HasBalise(Line, BEGINSECTION)) then
        begin
          // on récupère l'index
          EWE := LS666.Strings[i1]; i1 += 1;
          Sect.Index  :=StrToInt(ExtractText(EWE, BEGININDEX));
          // on récupère le topic
          EWE := LS666.Strings[i1]; i1 += 1;
          Sect.Topics :=ExtractText(EWE, BEGINTOPICS);
          // on récupère le titre
          EWE := LS666.Strings[i1]; i1 += 1;
          Sect.Title  :=ExtractText(EWE, BEGINTITLE);    ;
          Sect.Texte:='';
          while (Pos(BalEnd1, Line)=0) do
          begin
            Line := LS666.Strings[i1]; i1 += 1;
            if (Line = '') then Sect.Texte:=Sect.Texte + #10
                           else Sect.Texte:=Sect.Texte + Line;
          end;
          Sect.Texte:=ExtractText(Sect.Texte, BEGINTEXTE);
          // on ajoute la section à la liste
          AddSection(Sect);
        end;
      end; // while i1 < (LS666.Count - 1) do
      Result := True;
    except
    end;
    LS666.Clear;
  finally
    FreeAndNil(LS666);//LS666.Free;
  end;
end;

function THelpFileStructure.GetNbreSections: integer;
begin
  Result := FListeSections.Count;
end;

function THelpFileStructure.SearchIndexSection(const Topix: string): integer;
var
  i: integer;
  s: TSection;
begin
  AfficherMessage(Format('%s.SearchIndexSection(%s)',[ClassName, Topix]));
  Result := -1;
  try
    // si le fichier est invalide, sortie avec échec
    if (GetNbreSections = 0) then Exit;
    // si le topic est vide, alors afficher l'aide par défaut
    if Trim(Topix)='' then
    begin
      FCurrentIndex:=0;
      Result:=0;
      Exit;
    end;
    for i:=0 to GetNbreSections - 1 do
    begin
      S:=GetSection(i);
      // insensibilité à la casse
      if (UpperCase(Trim(Topix)) = UpperCase(Trim(S.Topics))) then
      begin
        FCurrentIndex := i;
        Result := i;
        Exit;
      end;
    end;
  except
  end;
end;

end.
 
