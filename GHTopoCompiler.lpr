program GHTopoCompiler;
{.$APPTYPE CONSOLE}

{$INCLUDE CompilationParameters.inc}
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
  {$ENDIF}
  Interfaces,    // /!\ Indispensable !!! Ajouter la condition LCLBase
  StructuresDonnees,
  Classes, SysUtils, CustApp
  { you can add units after this }
  , Common
  , ToporobotClasses2012
  , CodeCalculTopo
  , UnitEntitesExtended
  , ConvertisseurJPC
  ;

type
  { TGHTopoCompiler }
  TGHTopoCompiler = class(TCustomApplication)
  private
    FHasAction      : boolean;
    FDocTopo        : TToporobotStructure2012;
    FCodeCalculTopo : TCodeDeCalcul;
    FBDDEntites     : TBDDEntites;
    FConvertisseurCoords: TConversionSysteme;
    function CompilerUneTopo(const FC: string): integer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    function  InitialiserConteneurs(): boolean;
    procedure FinaliserConteneurs();

  end;

{ TGHTopoCompiler }

procedure TGHTopoCompiler.DoRun;
var
  ErrorMsg, FC: String;
begin
  FHasAction := false;
  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    FHasAction := True;
    Terminate;
  end;
  if HasOption('v', 'version') then
  begin
    AfficherMessage('GHTopo version ' + Format(rsGHTOPOVERSION, [30, 08, 2016]));
    FHasAction := True;
    Terminate;
  end;
  if HasOption('c', 'compile') then
  begin
    FC := self.GetOptionValue('c');
    if (not InitialiserConteneurs()) then self.Terminate;
      self.CompilerUneTopo(FC);
    FinaliserConteneurs();
    FHasAction := True;
    Terminate;
  end;
  if HasOption('d', 'ghcavedraw') then
  begin
    FC := self.GetOptionValue('d');
    if (not InitialiserConteneurs()) then self.Terminate;
      self.CompilerUneTopo(FC);
      FBDDEntites.ExportForGHCaveDraw(ChangeFileExt(FC, '.gcp'), '');
    FinaliserConteneurs();
    FHasAction := True;
    Terminate;
  end;
  if HasOption('k', 'kml') then
  begin
    if (not InitialiserConteneurs()) then self.Terminate;
      FC := self.GetOptionValue('k');
      self.CompilerUneTopo(FC);
      FConvertisseurCoords.Initialiser();
      // export simultané vers KML et OSM tant que le doc et le convertisseur sont actifs
      FBDDEntites.ExportForCarto(ChangeFileExt(FC, '.htm'),
                                 FConvertisseurCoords,
                                 FDocTopo.GetCodeEPSGSystemeCoordonnees().CodeEPSG,
                                 gisLEAFLET,
                                 True,
                                 '', '', 255, True
                                 );
      FBDDEntites.ExportForCarto(ChangeFileExt(FC, '.osm'),
                                 FConvertisseurCoords,
                                 FDocTopo.GetCodeEPSGSystemeCoordonnees().CodeEPSG,
                                 gisOSM,
                                 True,
                                 '', '', 255, True
                                 );
    FinaliserConteneurs();
    FHasAction := True;
    Terminate;
  end;
  if (not FHasAction) then
  begin
    WriteHelp;
    Terminate;
  end;
end;

function TGHTopoCompiler.InitialiserConteneurs(): boolean;
begin
  result := false;
  FDocTopo             := TToporobotStructure2012.Create;
  FCodeCalculTopo      := TCodeDeCalcul.Create;
  FBDDEntites          := TBDDEntites.Create;
  FConvertisseurCoords := TConversionSysteme.Create;
  try
    result := true;

  except
  end;
end;

procedure TGHTopoCompiler.FinaliserConteneurs();
begin
  try
    FDocTopo.Finaliser();
    FCodeCalculTopo.Finaliser();
    FBDDEntites.Finaliser();
    FConvertisseurCoords.Finaliser();
  finally
    FDocTopo.Free;
    FCodeCalculTopo.Free;
    FBDDEntites.Free;
  end;
end;



constructor TGHTopoCompiler.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

 //FCodeCalculTopo.Initialiser(FDocTopo, '');

end;

destructor TGHTopoCompiler.Destroy;
begin

  inherited Destroy;
end;

procedure TGHTopoCompiler.WriteHelp;
begin
  { add your help code here }
  AfficherMessage('Usage: ' +  ExeName + ' -h');
  AfficherMessage('Commands:');
  AfficherMessage('-c, -compile <file>    : Compiles the file specified');
  AfficherMessage('-d, -ghcavedraw <file> : Compiles the file specified and export GCP centerlines');
  AfficherMessage('-k, -kml <file> : Compiles the file specified and export KML, OSM');

end;
function TGHTopoCompiler.CompilerUneTopo(const FC: string): integer;
var
  CodeCalculTopo: TCodeDeCalcul;
begin
  Result := 1;
  AfficherMessage(Format('Compiling %s', [FC]));
  if (not FileExists(FC)) then
  begin
    AfficherMessage('*** File not found ***');
    Exit;
  end;
  if (FDocTopo.LoadFichierTab(FC) > 0) then
  begin
    try
      FCodeCalculTopo.Initialiser(FDocTopo, 'toto.top');
      Result := IIF(FCodeCalculTopo.CalculComplet(FBDDEntites, false), 0, 1);
    except
    end;
  end;
end;

var
  Application: TGHTopoCompiler;
begin
  Application := TGHTopoCompiler.Create(nil);
  Application.Run;
  Application.Free;
end.

