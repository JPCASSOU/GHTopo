unit ExportationSQL;

{$mode delphi}

interface

uses
  StructuresDonnees,
  ToporobotClasses2012,
  Common, UnitObjetSerie,
  Classes, SysUtils;

type

{ TExportationSQL }

 TExportationSQL = class
  private
    FDocTopo : TToporobotStructure2012;
    FWithData: boolean;
    fp       : TextFile;


    procedure WriteLine(const L: string);
    procedure BeginTable(const TableName: string);
    procedure AddField(const Fieldname, FieldType, FieldComplements: string); overload;
    procedure AddField(const Fieldname, FieldType: string; const FieldLength: integer = -1; const HasMore: boolean = true); overload;
    procedure BeginInsertData(const TableName: string; const TableFields: array of string);
    procedure EndInsertData(const TableName: string);
    procedure EndTable(const TableName: string);

  public
    function Initialiser(const FD: TToporobotStructure2012; const SQLSchemaOnly: boolean; const QFilename: TStringDirectoryFilename): boolean;

    procedure Finaliser();
    function  CreateDatabase(const MyDataBase: string): boolean;
    procedure CloseDatabase();
    function  GenererTableEntrances(const TN: string): boolean;
    function  GenererTableCodes(const TN: string): boolean;
    function  GenererTableExpes(const TN: string): boolean;
    function  GenererTableSeries(const TN: string): boolean;
    function  GenererTableVisees(const TN: string): boolean;
    function  GenererTableAntennes(const TN: string): boolean;


end;

implementation
const
  SQL_TYPE_INTEGER     = 'INTEGER';
  SQL_TYPE_BIGINT      = 'INTEGER';
  SQL_TYPE_DOUBLE      = 'REAL';
  SQL_TYPE_DATE        = 'DATE';
  SQL_TYPE_STRING      = 'VARCHAR(%d)';
  SQL_ATTR_PRIMARY_KEY = 'PRIMARY KEY';
  SQL_ATTR_UNIQUE      = 'UNIQUE';
  SQL_ATTR_NOT_NULL    = 'NOT NULL';
  SQL_ATTR_CLE_PRIMAIRE = SQL_ATTR_PRIMARY_KEY + ' ' + SQL_ATTR_UNIQUE + ' ' + SQL_ATTR_NOT_NULL;

  // nom des champs de la table Entrances
  SQL_ENTRANCE_eRefSer       = 'eRefSer';
  SQL_ENTRANCE_eRefSt        = 'eRefSt';
  SQL_ENTRANCE_eCouleur      = 'eCouleur';
  SQL_ENTRANCE_eXEntree      = 'eXEntree';
  SQL_ENTRANCE_eYEntree      = 'eYEntree';
  SQL_ENTRANCE_eZEntree      = 'eZEntree';
  SQL_ENTRANCE_eIDTerrain    = 'eIDTerrain';
  SQL_ENTRANCE_eNomEntree    = 'eNomEntree';
  SQL_ENTRANCE_eObserv       = 'eObserv';
  // Table codes
  SQL_CODE_IDCode            = 'IDCode';
  SQL_CODE_GradAz            = 'GradAz';
  SQL_CODE_GradInc           = 'GradInc';
  SQL_CODE_PsiL              = 'PsiL';
  SQL_CODE_PsiAz             = 'PsiAz';
  SQL_CODE_PsiP              = 'PsiP';
  SQL_CODE_FactLong          = 'FactLong';
  SQL_CODE_AngLimite         = 'AngLimite';
  SQL_CODE_ErreurTourillon   = 'ErreurTourillon';

  SQL_CODE_FUNC_CORR_AZ_Co            = 'FuncCorrAzCo';
  SQL_CODE_FUNC_CORR_AZ_ErreurMax     = 'FuncCorrAz_ErreurMax';
  SQL_CODE_FUNC_CORR_AZ_PosErreurMax  = 'FuncCorrAzPosErrMax';

  SQL_CODE_FUNC_CORR_P_Co             = 'FuncCorrIncCo';
  SQL_CODE_FUNC_CORR_P_ErreurMax      = 'FuncCorrIncErreurMax';
  SQL_CODE_FUNC_CORR_P_PosErreurMax   = 'FuncCorrIncPosErrMax';
  SQL_CODE_Commentaire       = 'Commentaire';
  // Table expés
  SQL_EXPE_IDExpe            = 'IDExpe';
  SQL_EXPE_DATE              = 'Date';
  SQL_EXPE_ModeDecl          = 'ModeDecl';
  SQL_EXPE_IdxCouleur        = 'IdxCouleur';
  SQL_EXPE_Declinaison       = 'Declinaison';
  SQL_EXPE_Operateur         = 'Operateur';
  SQL_EXPE_ClubSpeleo        = 'ClubSpeleo';
  SQL_EXPE_Commentaire       = 'Commentaire';
  // Table Séries
  SQL_SERIE_NumeroSerie      = 'NumeroSerie';
  SQL_SERIE_NumeroEntrance   = 'Entrance';
  SQL_SERIE_NumeroReseau     = 'Reseau';
  SQL_SERIE_SerieDepart      = 'SerieDepart';
  SQL_SERIE_PtDepart         = 'PtDepart';
  SQL_SERIE_SerieArrivee     = 'SerieArrivee';
  SQL_SERIE_PtArrivee        = 'PtArrivee';
  SQL_SERIE_NbPoints         = 'NbPoints';
  SQL_SERIE_Chance           = 'Chance';
  SQL_SERIE_Obstacle         = 'Obstacle';
  SQL_SERIE_Couleur          = 'Couleur';
  SQL_SERIE_Raideur          = 'Raideur';
  SQL_SERIE_NomSerie         = 'NomSerie';
  SQL_SERIE_Commentaires     = 'Commentaires';


  // Table Visées
  SQL_VISEES_IDVisee       = 'IDVisee';
  SQL_VISEES_Serie         = 'Serie';
  SQL_VISEES_Point         = 'Point';
  SQL_VISEES_TypeVisee     = 'TypeVisee';
  SQL_VISEES_IDSecteur     = 'IDSecteur';
  SQL_VISEES_Code          = 'Code';
  SQL_VISEES_Expe          = 'Expe';
  SQL_VISEES_Longueur      = 'Longueur';
  SQL_VISEES_Azimut        = 'Azimut';
  SQL_VISEES_Pente         = 'Pente';
  SQL_VISEES_LG            = 'LG';
  SQL_VISEES_LD            = 'LD';
  SQL_VISEES_HZ            = 'HZ';
  SQL_VISEES_HN            = 'HN';
  SQL_VISEES_IDTerrain     = 'IDTerrain';
  SQL_VISEES_Commentaires  = 'Commentaires';



  // Tables Antennes
  SQL_ANTENNES_IDAntenne     = 'IDAntenne';
  SQL_ANTENNES_EntranceRatt  = 'EntranceRatt';
  SQL_ANTENNES_IDReseau      = 'IDReseau';
  SQL_ANTENNES_IDSecteur     = 'IDSecteur';
  SQL_ANTENNES_Code          = 'Code';
  SQL_ANTENNES_Expe          = 'Expe';
  SQL_ANTENNES_SerDep        = 'SerieDepart';
  SQL_ANTENNES_PtDep         = 'PointDepart';
  SQL_ANTENNES_Longueur      = 'Longueur';
  SQL_ANTENNES_Azimut        = 'Azimut';
  SQL_ANTENNES_Pente         = 'Pente';
  SQL_ANTENNES_Commentaires  = 'Commentaires';



{ TExportationSQL }

procedure TExportationSQL.WriteLine(const L: string);
begin
  WriteLn(fp, L);
end;

procedure TExportationSQL.BeginTable(const TableName: string);
begin
  WriteLine(Format('  DROP TABLE IF EXISTS `%s`;', [TableName]));
  WriteLine(Format('  CREATE TABLE `%s` (', [TableName]));
end;

procedure TExportationSQL.AddField(const Fieldname, FieldType, FieldComplements: string);
begin
  WriteLine(Format('    `%s` %s %s,', [Fieldname, FieldType, FieldComplements]));
end;

procedure TExportationSQL.AddField(const Fieldname, FieldType: string; const FieldLength: integer = -1; const HasMore: boolean = true);
var
  WU: String;
begin
  WU := IIF(FieldLength = -1, FieldType, Format(FieldType, [FieldLength]));
  WriteLine(Format('    `%s` %s%s', [Fieldname, WU, IIF(HasMore, ',', '')]));
end;

procedure TExportationSQL.BeginInsertData(const TableName: string; const TableFields: array of string);
var
  EWE: String;
  i, Nb: Integer;
begin
  EWE := Format('  INSERT INTO `%s` (', [TableName]);
  Nb := length(TableFields);
  for i := 0 to Nb - 1 do
  begin
    EWE += Format('`%s`%s', [TableFields[i], IIF(i = (Nb - 1), ')', ', ')]);
  end;
  //... noms des champs)
  EWE += ' VALUES ';
  WriteLine(EWE);
end;

procedure TExportationSQL.EndInsertData(const TableName: string);
var
  EWE: String;
begin
  pass;
  //EWE := Format('  ); -- INSERT INTO %s', [TableName]);
  //WriteLine(EWE);
end;

procedure TExportationSQL.EndTable(const TableName: string);
begin
  WriteLine(Format('  ); -- %s', [TableName]));
end;

function TExportationSQL.CreateDatabase(const MyDataBase: string): boolean;
begin
  Result := false;
  AfficherMessage(Format('%s.CreateDatabase() %s', [ClassName, MyDataBase]));
  Result := True;
end;

procedure TExportationSQL.CloseDatabase();
begin
  AfficherMessage(Format('%s.CloseDatabase()', [ClassName]));
end;

function TExportationSQL.GenererTableEntrances(const TN: string): boolean;
var
  i, Nb: Integer;
  EWE: TEntrance;
  WU: String;
begin
  Result := false;
  AfficherMessage(Format('%s.GenererTableEntrances(): %s', [ClassName, TN]));
  BeginTable(TN);
    AddField(SQL_ENTRANCE_eRefSer     , SQL_TYPE_INTEGER);
    AddField(SQL_ENTRANCE_eRefSt      , SQL_TYPE_INTEGER);
    AddField(SQL_ENTRANCE_eCouleur    , SQL_TYPE_INTEGER);
    AddField(SQL_ENTRANCE_eXEntree    , SQL_TYPE_DOUBLE);
    AddField(SQL_ENTRANCE_eYEntree    , SQL_TYPE_DOUBLE);
    AddField(SQL_ENTRANCE_eZEntree    , SQL_TYPE_DOUBLE);
    AddField(SQL_ENTRANCE_eIDTerrain  , SQL_TYPE_STRING, 16);
    AddField(SQL_ENTRANCE_eNomEntree  , SQL_TYPE_STRING, 80);
    AddField(SQL_ENTRANCE_eObserv     , SQL_TYPE_STRING, 160, false);
  EndTable(TN);
  if (FWithData) then
  begin
    BeginInsertData(TN, [
      SQL_ENTRANCE_eRefSer,
      SQL_ENTRANCE_eRefSt,
      SQL_ENTRANCE_eCouleur,
      SQL_ENTRANCE_eXEntree,
      SQL_ENTRANCE_eYEntree,
      SQL_ENTRANCE_eZEntree,
      SQL_ENTRANCE_eIDTerrain,
      SQL_ENTRANCE_eNomEntree,
      SQL_ENTRANCE_eObserv]);
      Nb := FDocTopo.GetNbEntrances();
      for i := 0 to Nb - 1 do
      begin
        EWE := FDocTopo.GetEntrance(i);
        WU := Format('   (%d, %d, %d, %.3f, %.3f, %.3f, "%s", "%s", "%s")', [
                           EWE.eRefSer, EWE.eRefSt, EWE.eCouleur.toTColor(),
                           EWE.ePosition.X, EWE.ePosition.Y, EWE.ePosition.Z,
                           mysqli_real_escape_string(EWE.eIDTerrain),
                           mysqli_real_escape_string(EWE.eNomEntree),
                           mysqli_real_escape_string(EWE.eObserv)]);

        WU += IIF(i = (Nb - 1), ';', ', ');
        WriteLine(WU);
      end;
    EndInsertData(TN);
  end;
  Result := True;
end;

function TExportationSQL.GenererTableCodes(const TN: string): boolean;
var
  EWE: TCode;
  Nb, i: Integer;
  WU: String;
begin
  Result := false;
  AfficherMessage(Format('%s.GenererTableCodes(): %s', [ClassName, TN]));
  BeginTable(TN);
    AddField(SQL_CODE_IDCode                      , SQL_TYPE_INTEGER, SQL_ATTR_CLE_PRIMAIRE);
    AddField(SQL_CODE_GradAz                      , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_GradInc                     , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_PsiL                        , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_PsiAz                       , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_PsiP                        , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_FactLong                    , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_AngLimite                   , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_ErreurTourillon             , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_FUNC_CORR_AZ_Co             , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_FUNC_CORR_AZ_ErreurMax      , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_FUNC_CORR_AZ_PosErreurMax   , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_FUNC_CORR_P_Co              , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_FUNC_CORR_P_ErreurMax       , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_FUNC_CORR_P_PosErreurMax    , SQL_TYPE_DOUBLE);
    AddField(SQL_CODE_Commentaire                 , SQL_TYPE_STRING, 160, false);
  EndTable(TN);
  if (FWithData) then
  begin
    BeginInsertData(TN, [
      SQL_CODE_IDCode,
      SQL_CODE_GradAz,
      SQL_CODE_GradInc,
      SQL_CODE_PsiL,
      SQL_CODE_PsiAz,
      SQL_CODE_PsiP,
      SQL_CODE_FactLong,
      SQL_CODE_AngLimite,
      SQL_CODE_ErreurTourillon,
      SQL_CODE_FUNC_CORR_AZ_Co,
      SQL_CODE_FUNC_CORR_AZ_ErreurMax,
      SQL_CODE_FUNC_CORR_AZ_PosErreurMax,
      SQL_CODE_FUNC_CORR_P_Co,
      SQL_CODE_FUNC_CORR_P_ErreurMax,
      SQL_CODE_FUNC_CORR_P_PosErreurMax,
      SQL_CODE_Commentaire
     ]);
      Nb := FDocTopo.GetNbCodes();
      for i := 0 to Nb - 1 do
      begin
        EWE := FDocTopo.GetCode(i);
        WU := Format('   (%d, %.2f, %.2f,'+
                          '%.4f, %.4f, %.4f,'+
                          '%.4f, %.3f, %.3f,'+
                          '%.3f, %.3f, %.3f,'+
                          '%.3f, %.3f, %.3f,'+
                          '"%s")', [
                          EWE.IDCode,
                          EWE.GradAz, EWE.GradInc, EWE.PsiL, EWE.PsiAz, EWE.PsiP,
                          EWE.FactLong, EWE.AngLimite, EWE.ErreurTourillon,
                          EWE.ParamsFuncCorrAz.Co, EWE.ParamsFuncCorrAz.ErreurMax, EWE.ParamsFuncCorrAz.PosErrMax,
                          EWE.ParamsFuncCorrInc.Co, EWE.ParamsFuncCorrInc.ErreurMax, EWE.ParamsFuncCorrInc.PosErrMax,
                          mysqli_real_escape_string(EWE.Commentaire)]);
        WU += IIF(i = (Nb - 1), ';', ', ');
        WriteLine(WU);
      end;
    EndInsertData(TN);
  end;
end;

function TExportationSQL.GenererTableExpes(const TN: string): boolean;
var
  Nb, i: Integer;
  EWE: TExpe;
  WU: String;
begin
  Result := false;
  AfficherMessage(Format('%s.GenererTableExpes(): %s', [ClassName, TN]));
  BeginTable(TN);
    AddField(SQL_EXPE_IDExpe         , SQL_TYPE_INTEGER, SQL_ATTR_CLE_PRIMAIRE);
    AddField(SQL_EXPE_DATE           , SQL_TYPE_DATE);
    AddField(SQL_EXPE_IdxCouleur     , SQL_TYPE_INTEGER);
    AddField(SQL_EXPE_ModeDecl       , SQL_TYPE_INTEGER);
    AddField(SQL_EXPE_Declinaison    , SQL_TYPE_DOUBLE);
    AddField(SQL_EXPE_Operateur      , SQL_TYPE_STRING, 40);
    AddField(SQL_EXPE_ClubSpeleo     , SQL_TYPE_STRING, 40);
    AddField(SQL_EXPE_Commentaire    , SQL_TYPE_STRING, 160, false);

  EndTable(TN);
  if (FWithData) then
  begin
    BeginInsertData(TN, [
      SQL_EXPE_IDExpe,
      SQL_EXPE_DATE,
      SQL_EXPE_IdxCouleur,
      SQL_EXPE_ModeDecl,
      SQL_EXPE_Declinaison,
      SQL_EXPE_Operateur,
      SQL_EXPE_ClubSpeleo,
      SQL_EXPE_Commentaire
      ]);
      Nb := FDocTopo.GetNbExpes();
      for i := 0 to Nb - 1 do
      begin
        EWE := FDocTopo.GetExpe(i);
        WU := Format('   (%d, ''%s'', '+
                          '%d, %d, %.4f,'+
                          '"%s", "%s", "%s")', [
                          EWE.IDExpe,
                          DateYYYYMMDDToDateSQL(EWE.AnneeExpe, EWE.MoisExpe, EWE.JourExpe),   {$WARNING: TEXpe.DateExpe à implementer}
                          EWE.IdxCouleur, EWE.ModeDecl, EWE.DeclinaisonInDegrees,
                          mysqli_real_escape_string(EWE.Operateur),
                          mysqli_real_escape_string(EWE.ClubSpeleo),
                          mysqli_real_escape_string(EWE.Commentaire)]);
        WU += IIF(i = (Nb - 1), ';', ', ');
        WriteLine(WU);
      end;
    EndInsertData(TN);
  end;
end;



function TExportationSQL.GenererTableAntennes(const TN: string): boolean;
var
  Nb, i: Integer;
  EWE: TViseeAntenne;
  WU: String;
begin
  Result := false;
  AfficherMessage(Format('%s.GenererTableAntennes(): %s', [ClassName, TN]));
  BeginTable(TN);
    AddField(SQL_ANTENNES_IDAntenne       , SQL_TYPE_BIGINT);
    AddField(SQL_ANTENNES_EntranceRatt    , SQL_TYPE_INTEGER);
    AddField(SQL_ANTENNES_IDReseau        , SQL_TYPE_INTEGER);
    AddField(SQL_ANTENNES_IDSecteur       , SQL_TYPE_INTEGER);
    AddField(SQL_ANTENNES_Code            , SQL_TYPE_INTEGER);
    AddField(SQL_ANTENNES_Expe            , SQL_TYPE_INTEGER);
    AddField(SQL_ANTENNES_SerDep          , SQL_TYPE_INTEGER);
    AddField(SQL_ANTENNES_PtDep           , SQL_TYPE_INTEGER);
    AddField(SQL_ANTENNES_Longueur        , SQL_TYPE_DOUBLE);
    AddField(SQL_ANTENNES_Azimut          , SQL_TYPE_DOUBLE);
    AddField(SQL_ANTENNES_Pente           , SQL_TYPE_DOUBLE);
    AddField(SQL_ANTENNES_Commentaires    , SQL_TYPE_STRING, 160, false);

  EndTable(TN);
  Nb := FDocTopo.GetNbAntennes();
  if ((Nb > 0) and FWithData) then
  begin
    BeginInsertData(TN, [
      SQL_ANTENNES_IDAntenne,
      SQL_ANTENNES_EntranceRatt,
      SQL_ANTENNES_IDReseau,
      SQL_ANTENNES_IDSecteur,
      SQL_ANTENNES_Code,
      SQL_ANTENNES_Expe,
      SQL_ANTENNES_SerDep,
      SQL_ANTENNES_PtDep,
      SQL_ANTENNES_Longueur,
      SQL_ANTENNES_Azimut,
      SQL_ANTENNES_Pente,
      SQL_ANTENNES_Commentaires
      ]);
      for i := 0 to Nb - 1 do
      begin
        EWE := FDocTopo.GetViseeAntenne(i);
        WU := Format('   (%d, ' +
                          '%d, %d, %d, ' +
                          '%d, %d, '+
                          '%d, %d, '+
                          '%.3f, %.3f, %.3f)', [
                          -(NB_MAXI_SERIES_PAR_CAVITE * EWE.SerieDepart + MULTIPLICATEUR_STATION * EWE.PtDepart),
                          EWE.EntranceRatt, EWE.Reseau, EWE.Secteur,
                          0, 0, //, E.Code, EWE.Expe,           // Visées en antenne: Code et expés hérité de leur station d'accrochage
                          EWE.SerieDepart, EWE.PtDepart,
                          EWE.Longueur, EWE.Azimut, EWE.Pente]);
        WU += IIF(i = (Nb - 1), ';', ', ');
        WriteLine(WU);
      end;
    EndInsertData(TN);
  end;
end;

function TExportationSQL.GenererTableSeries(const TN: string): boolean;
var
  NbSeries, s: Integer;
  MySerie: TObjSerie;
  WU: String;
begin
  Result := false;
  AfficherMessage(Format('%s.GenererTableSeries(): %s', [ClassName, TN]));
  BeginTable(TN);
    AddField(SQL_SERIE_NumeroSerie           , SQL_TYPE_INTEGER, SQL_ATTR_CLE_PRIMAIRE);
    AddField(SQL_SERIE_NumeroEntrance        , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_NumeroReseau          , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_SerieDepart           , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_PtDepart              , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_SerieArrivee          , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_PtArrivee             , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_NbPoints              , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_Chance                , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_Obstacle              , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_Couleur               , SQL_TYPE_INTEGER);
    AddField(SQL_SERIE_Raideur               , SQL_TYPE_DOUBLE);
    AddField(SQL_SERIE_NomSerie              , SQL_TYPE_STRING, 80);
    AddField(SQL_SERIE_Commentaires          , SQL_TYPE_STRING, 160, false);

  EndTable(TN);
  NbSeries := FDocTopo.GetNbSeries();
  if (FWithData) then
  begin
    BeginInsertData(TN, [
      SQL_SERIE_NumeroSerie,
      SQL_SERIE_NumeroEntrance,
      SQL_SERIE_NumeroReseau,
      SQL_SERIE_SerieDepart,
      SQL_SERIE_PtDepart,
      SQL_SERIE_SerieArrivee,
      SQL_SERIE_PtArrivee,
      SQL_SERIE_NbPoints,
      SQL_SERIE_Chance,
      SQL_SERIE_Obstacle,
      SQL_SERIE_Couleur,
      SQL_SERIE_Raideur,
      SQL_SERIE_NomSerie,
      SQL_SERIE_Commentaires
      ]);
      for s := 1 to NbSeries - 1 do
      begin
        MySerie    := FDocTopo.GetSerie(s);
        WU := Format('   (%d, ' +
                         '%d, %d, ' +
                         '%d, %d, %d, %d, ' +
                         '%d, ' +
                         '%d, %d, ' +
                         '%d, ' +
                         '%.4f, ' +
                         '"%s", "%s")', [
                         MySerie.GetNumeroDeSerie(),
                         MySerie.GetNumeroEntrance(), MySerie.GetNumeroReseau(),
                         MySerie.GetNoSerieDep(),  MySerie.GetNoPointDep(),
                         MySerie.GetNoSerieArr(),  MySerie.GetNoPointArr(),
                         MySerie.GetNbVisees(),

                         MySerie.GetChance(), MySerie.GetObstacle(),
                         MySerie.GetCouleur(),
                         MySerie.GetRaideur(),
                         mysqli_real_escape_string(MySerie.GetNomSerie()),
                         mysqli_real_escape_string(MySerie.GetObsSerie())
                         ]);
        WU += IIF(s = (NbSeries - 1), ';', ', ');
        WriteLine(WU);
      end;
    EndInsertData(TN);

  end;
end;

function TExportationSQL.GenererTableVisees(const TN: string): boolean;
var
  Nb, i, NbSeries, s: Integer;
  WU: String;
  MySerie: TObjSerie;
  EWE: TUneVisee;
  MyNumSerie: TNumeroSerie;
begin
  Result := false;
  AfficherMessage(Format('%s.GenererTableVisees(): %s', [ClassName, TN]));
  BeginTable(TN);
    AddField(SQL_VISEES_IDVisee         , SQL_TYPE_BIGINT);
    AddField(SQL_VISEES_Serie           , SQL_TYPE_INTEGER);
    AddField(SQL_VISEES_Point           , SQL_TYPE_INTEGER);
    AddField(SQL_VISEES_TypeVisee       , SQL_TYPE_INTEGER);
    AddField(SQL_VISEES_IDSecteur       , SQL_TYPE_INTEGER);
    AddField(SQL_VISEES_Code            , SQL_TYPE_INTEGER);
    AddField(SQL_VISEES_Expe            , SQL_TYPE_INTEGER);

    AddField(SQL_VISEES_Longueur        , SQL_TYPE_DOUBLE);
    AddField(SQL_VISEES_Azimut          , SQL_TYPE_DOUBLE);
    AddField(SQL_VISEES_Pente           , SQL_TYPE_DOUBLE);
    AddField(SQL_VISEES_LG              , SQL_TYPE_DOUBLE);
    AddField(SQL_VISEES_LD              , SQL_TYPE_DOUBLE);
    AddField(SQL_VISEES_HZ              , SQL_TYPE_DOUBLE);
    AddField(SQL_VISEES_HN              , SQL_TYPE_DOUBLE);
    AddField(SQL_VISEES_IDTerrain       , SQL_TYPE_STRING, 20);
    AddField(SQL_VISEES_Commentaires    , SQL_TYPE_STRING, 160, false);
  EndTable(TN);

  NbSeries := FDocTopo.GetNbSeries();
  if (FWithData) then
  begin
    for s := 1 to NbSeries - 1 do
    begin
      MySerie    := FDocTopo.GetSerie(s);
      MyNumSerie := MySerie.GetNumeroDeSerie();
      Nb := MySerie.GetNbVisees();
      BeginInsertData(TN, [
        SQL_VISEES_IDVisee,
        SQL_VISEES_Serie,
        SQL_VISEES_Point,
        SQL_VISEES_TypeVisee,
        SQL_VISEES_IDSecteur,
        SQL_VISEES_Code,
        SQL_VISEES_Expe,

        SQL_VISEES_Longueur,
        SQL_VISEES_Azimut,
        SQL_VISEES_Pente,
        SQL_VISEES_LG ,
        SQL_VISEES_LD,
        SQL_VISEES_HZ,
        SQL_VISEES_HN,
        SQL_VISEES_IDTerrain,
        SQL_VISEES_Commentaires
        ]);
        for i := 0 to Nb - 1 do
        begin
          EWE := MySerie.GetVisee(i);
          WU := Format('   (%d, ' +
                            '%d, %d, ' +
                            '%d, %d, '+
                            '%d, %d, '+
                            '%.3f, %.3f, %.3f,'+
                            '%.3f, %.3f, %.3f, %.3f, '+
                            '"%s", "%s")', [
                            NB_MAXI_SERIES_PAR_CAVITE * MyNumSerie + MULTIPLICATEUR_STATION * i,
                            MyNumSerie, i,
                            EWE.TypeVisee,  EWE.IDSecteur,
                            EWE.Code, EWE.Expe,
                            EWE.Longueur, EWE.Azimut, EWE.Pente,
                            EWE.LG, EWE.LD, EWE.HZ, EWE.HN,
                            mysqli_real_escape_string(EWE.IDTerrainStation),
                            mysqli_real_escape_string(EWE.Commentaires)]);
          WU += IIF(i = (Nb - 1), ';', ', ');
          WriteLine(WU);
        end;
      EndInsertData(TN);
    end;
  end;
  //*)
end;

function TExportationSQL.Initialiser(const FD: TToporobotStructure2012; const SQLSchemaOnly: boolean; const QFilename: TStringDirectoryFilename): boolean;
begin
  Result := false;
  FDocTopo := FD;
  FWithData := NOT SQLSchemaOnly;
  AfficherMessage(Format('%s.Initialiser(): %s - %s données', [ClassName, QFilename, IIF(FWithData, 'Avec', 'Sans')]));
  AssignFile(fp, QFileName);
  try
    ReWrite(fp);
    Result := True;
  except
    Result := false;
  end;
end;

procedure TExportationSQL.Finaliser();
begin
  AfficherMessage(Format('%s.Finaliser()', [ClassName]));
  try

  finally
    CloseFile(fp);
  end;
end;

end.

