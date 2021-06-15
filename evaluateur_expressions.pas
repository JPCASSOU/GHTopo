
{$DEFINE EVAL_EXPR_YOUSSEF}
{.$UNDEF  EVAL_EXPR_YOUSSEF}

unit evaluateur_expressions;
// 2017-07-27: Support simplifié des variables
interface
{$mode delphi}{$H+}

uses
  Math
  , SysUtils
  , Classes
  , IniFiles
  , Common
{$IFNDEF EVAL_EXPR_YOUSSEF}
  , fpexprpars
{$ENDIF}
;

type TVariable =  record
  Nom: string;
  Valeur: double;
end;
type

{ TEvaluateurExpressions }
 TEvaluateurExpressions = class


  private
    {$IFNDEF EVAL_EXPR_YOUSSEF}
       FPExpressionParser: TFPExpressionParser;
    {$ENDIF}
    FListeDeVariables : TStringList;
    function  RemplacerVariablesParLeursValeurs(var MyExpression: string; out PosErr: integer): boolean;
  public
    //FFichierIni: string;
    function  Initialiser(): boolean;
    procedure Finaliser();
    function  GetNbreDeFonctions(): integer;
    function  GetNomFonction(const Idx: integer): string;
    function  Evaluate(const MyExpression: string; var Error: Integer; var Description: string; Identifier: string = ''; Value: Double = 0): Double;
    function  Eval(Expression: string; Identifier: string = ''; Value: Double = 0): Double;
    procedure AddVariable(const V: TVariable); overload;
    procedure AddVariable(const K: string; const V: double); overload;
    function  GetNbVariables(): integer;
    function  GetVariable(const Idx: integer): TVariable;
    //function  LoadFromIni(): boolean;
    //procedure SaveToIni();
end;


function EvaluationRapideExpression(const Expr: string): double;
function FloatToFrac(X: Double; Format: string = '%d/%d'; Default: string = '%g'): string;

implementation
uses
  dialogs;
const
  MAX_FACT = 170;
  MAX_SQR = 1e154;
  MAX_CARDINAL = 4294967295;
  MAX_FRAC = 1000000;


{$IFDEF EVAL_EXPR_YOUSSEF}

{
         Mathematic expressions evaluator.
         Copyright © T.Youssef 05/02/2003

         Last revision: 21/02/2003

         Expression = The string expression to evaluate
         Error = The offset where parsing has stopped
         Description = Description of the Error if ever there is
}
// 21/02/2014: Petite correction: la saisie de '.63' donne '0.63'

const NB_OF_FUNCTIONS = 42;

const
  Identifiers: array [0..NB_OF_FUNCTIONS] of string =
     (
      'e',           //0
      'pi',          //1
      'sin',         //2
      'cos',         //3
      'tan',         //4
      'arcsin',      //5
      'arccos',      //6
      'arctan',      //7
      'exp',         //8
      'ln',          //9
      'log',         //10
      'sqrt',        //11
      'int',         //12
      'sinh',        //13
      'cosh',        //14
      'tanh',        //15
      'arcsinh',     //16
      'arccosh',     //17
      'arctanh',     //18
      'frac',        //19
      'abs',         //20
      'floor',       //21
      'ceil',        //22
      'degtorad',    //23    // conversion d'angles
      'radtodeg',    //24
      'gradtorad',   //25
      'radtograd',   //26
      'degtograd',   //27
      'gradtodeg',   //28
      'sind',        //29    // trigo en degrés
      'cosd',        //30
      'tand',        //31
      'asind',       //32
      'acosd',       //33
      'atand',       //34
      'sing',        //35    // trigo en grade
      'cosg',        //36
      'tang',        //37
      'asing',       //38
      'acosg',       //39
      'atang',       //40
      'trunc',       //41
      'round'        //42
     );


const
  INI_SECTION_HISTORIQUE_CALCULS = 'Historique';
  INI_SECTION_VARIABLES          = 'Variables';





resourcestring
  Err_Par = 'Parenthèses incorrectes';
  Err_Modulo = 'L''argument de ''%s'' ne doit pas être un modulo de Pi/2 ou de -Pi/2.';
  Err_Interv = 'L''argument de ''%s'' doit être compris entre %s et %s.';
  Err_Log = 'L''argument d''une logarithme doit être strictement superieur à 0.';
  Err_Sqrt = 'L''argument de ''sqrt'' doit être superieur ou égal à 0.';
  Err_Fact = 'L''argument de ''!'' doit être une valeur entière positive.';
  Err_Power = 'Le premier argument de la puissance réelle ''^'' doit être strictement superieur à 0.';
  Err_Int = 'Les arguments de ''%s'' doivent être des valeures entières.'; 
  Err_ZeroDiv = 'Division par zéro.';
  Err_UnknownId = 'Identificateur non reconnu ''%s''.';
  Err_UnknownSym = 'Opérateur ou expression inattendu ''%s''.';
  Err_Expr = 'Identificateur ou expression attendus, mais fin de chaine trouvée.';
  Err_OverFlow = 'Débordement de pile.';



{$WARNINGS OFF}
function IsAlphaNumeric(const S: string): boolean;
var
  n, i: Integer;
  Q: Boolean;
begin
  //ShowMessage('IsAlphaNumeric(): ' + S);
  result := True;
  n := length(S);
  if (n = 0) then Exit(false);
  //ShowMessage(S);
  // Premier caractère doit être une lettre
  if (not (upCase(S[1]) in ['A' ..'Z'])) then exit(false);
  for i := 1 to n do
  begin
    Q := upCase(S[i]) in ['A' .. 'Z', '0' .. '9', '_'];
    Result := Result and Q;
    //ShowMessageFmt('%d - %s - %s %s', [i, upCase(S[i]), BoolToStr(Q, 'Vrai', 'Faux'), BoolToStr(Result, 'Vrai', 'Faux')]);
  end;
end;


// évaluation rapide d'expressions isolées:
// crée l'évaluateur, fait le calcul puis détruit l'évaluateur

function EvaluationRapideExpression(const Expr: string): double;
var
  EWE: String;
  TD: TEvaluateurExpressions;
begin
  Result := 0;

  // remplacer les virgules par le point décimal
  EWE := StringReplace(Expr, '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
  EWE := StringReplace(EWE , ',', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
  // Si le premier caractère de l'expression est un séparateur décimal,
  // on ajoute un zéro en tête
  // Raison: Sur tableur, il est possible de saisir une expression de la forme "0.123" en ".123"
  if (EWE[1] = DefaultFormatSettings.DecimalSeparator) then EWE := '0' + EWE;
  TD := TEvaluateurExpressions.Create;
  try
    TD.Initialiser;
    result := TD.Eval(EWE);
    TD.Finaliser();
  finally
    TD.Free;
  end;
end;
{$WARNINGS ON}

function LowCase(const C: Char): Char;
begin
  Result := C;
  if C in ['A'..'Z'] then
    Inc(Result, 32);
end;

function Fact(N: Double): Double;
begin
  if N <= 0 then
    Result := 1
  else
    Result := N * Fact(N - 1);
end;

function ModuloPi2(R: Double): Boolean;
var
  i: Integer;
const
  Delta = 1e-10;
begin
  i := 0;
  if R >= 0 then
    while (R >= Pi * (i + 1/2)) do
      Inc(i)
  else
    while (R <= Pi * (i + 1/2)) do
      Dec(i);
  Result := Abs(R - Pi * (i + 1/2)) <= Delta;
end;


function TEvaluateurExpressions.RemplacerVariablesParLeursValeurs(var MyExpression: string; out PosErr: integer): boolean;
var
  n, i: Integer;
  V: TVariable;
  P: SizeInt;
  QLeft, QRight: String;
begin
  Result := false;
  // quelques protections
  MyExpression := ' ' + MyExpression + ' ';
  // on vérifie d'abord si l'expression est une affectation
  P := pos('=', MyExpression);
  if (P > 0) then
  begin
    // on supprime le sigill éventuel
    MyExpression := StringReplace(MyExpression, '$', '', [rfReplaceAll, rfIgnoreCase]);
    QLeft  := Trim(Copy(MyExpression, 1, P-1));
    QRight := Trim(Copy(MyExpression, P+1, Length(MyExpression)));
    V.Nom    := Trim(QLeft);
    if (IsAlphaNumeric(V.Nom)) then
    begin
      V.Valeur := StrToFloatDef(Trim(QRight), 0.00);
      AddVariable(V);    // et on ajoute la variable à la liste
    end;
    exit(false);   // avant de quitter avec Faux pour empêcher l'évaluation de l'expression
  end;
  // on recense les variables
  n := FListeDeVariables.Count;
  if (n = 0) then Exit(True); // pas de variables = on quitte direct
  for i := 0 to n - 1 do
  begin
    // ici, StringReplace() fait tout le boulot à notre place
    // on préfixe les noms de variables par le sigill $, comme en PHP
    // et on remplace toutes les occurrences de $var par leur valeur
    V := GetVariable(i);
    MyExpression := StringReplace(MyExpression, '$'+ Trim(V.Nom),  FloatToStr(V.Valeur), [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := True;
end;


function TEvaluateurExpressions.Eval(Expression: string; Identifier: string; Value: Double): Double;
var
  E: Integer;
  D: string;
begin
  Result := Evaluate(Expression, E, D, Identifier, Value);
  if E <> 0 then
    raise Exception.CreateFmt('Erreur à la position %d : %s', [E, D]);
end;

function TEvaluateurExpressions.Evaluate(const MyExpression: string; var Error: Integer; var Description: string; Identifier: string; Value: Double): Double;
var
  Idx, L, StackLevel: Integer;
  Sign: Integer;
  Expression: string;

  function Current(): Char;
  begin
    if (Idx <= L) then Result := Expression[Idx]
                  else Result := #0;
  end;

  procedure Next();
  begin
    inc(Idx);
  end;

  procedure Skip();
  begin
    while ((Idx <= L) and (Current in [#32, #10, #13])) do Next();
  end;

  procedure SkipReverse();
  begin
    while ((Idx > 0) and (Current in [#32, #10, #13])) do Dec(Idx);
  end;

  function ReadIdentifier(): string;
  begin
    Result := '';
    while (Idx <= L) and (LowCase(Current) in ['a'..'z', '0'..'9', '_']) do
    begin
      Result := Result + LowCase(Current);
      Next();
    end;
  end;

  function ReadFloat(): Double;
  var
    S: string;
    procedure S666();
    begin
      S := S + Current();
      Next();
    end;
    procedure S777(); inline;
    begin
      while (Idx <= L) and (Current in ['0'..'9']) do S666();
    end;

  begin
    S := '';
    S777();
    if (Current() = DefaultFormatSettings.DecimalSeparator) then
    begin
      S666();
      S777();
    end;
    if (LowCase(Current()) = 'e') then
    begin
      S666();
      if (Current() in ['+', '-']) then S666();
      S777();
    end;
    Result := StrToFloatDef(S, 0.00);
  end;

  function Factor(): Double;
  var
    S: string;
    i: integer;
    R: Double;
    Sign: Integer;
  begin
    Skip();
    Result := 0.00;
    Sign := 1;
    while (Idx <= L) and (Current() in ['+', '-']) do
    begin
      if Current = '-' then
      Sign := -1 * Sign;
      Next();
      Skip();
    end;
    if (Idx > L) then
    begin
      SkipReverse();
      Error := Idx;      
      Description := Err_Expr;
      Exit;
    end;
    case LowCase(Current()) of
      '0'..'9' : begin
                   Result := Sign * ReadFloat();
                 end;
      ')'      : begin
                   Error := Idx;
                   Description := Err_Par;
                 end;
      '('      : begin
                   Next;
                   i := 1;
                   S := '';
                   while (Idx <= l) and (i > 0) do
                   begin
                     case Current() of
                       ')' : Dec(i);
                       '(' : Inc(i);
                     end;
                     if (i > 0) then S := S + Current();
                     Next();
                   end;  
                   if i <> 0 then
                   begin
                     Dec(Idx);
                     SkipReverse();
                     Error := Idx + 1;
                     Description := Err_Par;
                     Exit;
                   end;
                   Result := Sign * Evaluate(S, Error, Description, Identifier, Value);
                   if (Error <> 0) then Error := Idx + Error - Length(S) - 2;
                 end;
      'a'..'z','_'
               : begin
                   S := ReadIdentifier();
                   if (SameText(Identifier, S)) then
                   begin
                     Result := Sign * Value;
                     Exit;
                   end;
                   for i := 0 to High(Identifiers) do
                     if (Identifiers[i] = S) then
                     begin
                       case i of
                         0 : Result := Sign * Exp(1);
                         1 : Result := Sign * Pi;
                         2 : Result := Sin(Factor());
                         3 : Result := Cos(Factor());
                         4 : begin
                               R := Factor();
                               if ModuloPi2(R) then
                               begin
                                 Error := Idx - 2;
                                 SkipReverse;
                                 Description := Format(Err_Modulo, [S]);
                                 Exit;
                               end;
                               Result := Tan(R);
                             end;
                         5 : begin
                               R := Factor();
                               if (R > 1) or (R < -1) then
                               begin
                                 Error := Idx - 1;
                                 Description := Format(Err_Interv, [S, '-1', '1']);
                                 Exit;
                               end;
                               Result := ArcSin(R);
                             end;
                         6 : begin
                               R := Factor();
                               if (R > 1) or (R < -1) then
                               begin
                                 Error := Idx - 1;
                                 Description := Format(Err_Interv, [S, '-1', '1']);
                                 Exit;
                               end;
                               Result := ArcCos(R);
                             end;
                         7 : Result := ArcTan(Factor());
                         8 : Result := Exp(Factor());
                         9 : begin
                               R := Factor;
                               if R <= 0 then
                               begin
                                 Error := Idx - 1;
                                 Description := Err_Log;
                                 Exit;
                               end;
                               Result := Ln(R);
                             end;
                         10: begin
                               R := Factor();
                               if R <= 0 then
                               begin
                                 Error := Idx - 1;
                                 Description := Err_Log;
                                 Exit;
                               end;
                               Result := Log10(R);
                             end;
                         11: begin
                               R := Factor();
                               if R < 0 then
                               begin
                                 Error := Idx - 1;
                                 Description := Err_Sqrt;
                                 Exit;
                               end;
                               Result := Sqrt(R);
                             end;
                         12: Result := Int(Factor);
                         13: Result := Sinh(Factor);
                         14: Result := Cosh(Factor);
                         15: begin
                               R := Factor();
                               if ModuloPi2(R) then
                               begin
                                 Error := Idx - 2;
                                 Description := Format(Err_Modulo, [S]);
                                 Exit;
                               end;
                               Result := Tanh(R);
                             end;
                         16: begin
                               R := Factor();
                               Result := ArcSinh(R);
                             end;
                         17: begin
                               R := Factor();
                               Result := ArcCosh(R);
                             end;
                         18: begin
                               R := Factor();
                               Result := ArcTanh(R);
                             end;
                         19: Result := Frac(Factor());
                         20: Result := Abs(Factor());
                         21: Result := floor(Factor());
                         22: Result := ceil(Factor());
                         // conversions d'angles
                         23: Result := degtorad(Factor());
                         24: Result := radtodeg(Factor());
                         25: Result := gradtorad(Factor());
                         26: Result := radtograd(Factor());
                         27: Result := degtograd(Factor());
                         28: Result := gradtodeg(Factor());
                         // trigo en degrés
                         29: Result := Sin(degtorad(Factor()));
                         30: Result := cos(degtorad(Factor()));
                         31: Result := tan(degtorad(Factor()));

                         32: Result := radtodeg(arcsin(Factor()));
                         33: Result := radtodeg(arccos(Factor()));
                         34: Result := radtodeg(arctan(Factor()));
                         // trigo en grade
                         35: Result := Sin(gradtorad(Factor()));
                         36: Result := cos(gradtorad(Factor()));
                         37: Result := tan(gradtorad(Factor()));

                         38: Result := radtograd(arcsin(Factor()));
                         39: Result := radtograd(arccos(Factor()));
                         40: Result := radtograd(arctan(Factor()));
                         // arrondi
                         41: Result := trunc(Factor());
                         42: Result := round(Factor());
                       end;
                       Exit;
                     end;
                   Description := Format(Err_UnknownId, [S]);
                   Error := Idx - 1;
                 end;
      else
        begin
          Error := Idx;
          Description := Format(Err_UnknownSym, [Current()]);
        end;
    end;
  end;

  function Expr1st(): Double;
  begin
    Skip();
    Result := Factor();
    Skip();
    if (Error <> 0) then Exit;
    case Current of
      '!' :
        begin
          if (not ((Int(Result) = Result) and (Result >= 0))) then
          begin
            Error := Idx - 1;
            Description := Err_Fact;
            Exit;
          end;
          if Result > MAX_FACT then
          begin
            Error := Idx - 1;
            Description := Err_OverFlow;
            Exit;
          end;
          Result := Fact(Result);
          Next;
        end;
      '#' : // symbole provisoire pour le carré
        begin
          if (Result > MAX_SQR) then
          begin
            Error := Idx;
            Description := Err_OverFlow;
            Exit;
          end;
          Result := Result * Result;
          Next;
        end;
   else
     ;;
   end;  // case Current of
  end;

  function Expr2nd(): Double;
  var
    R: Double;
  begin
    Skip;
    Result := Expr1st();
    if Error <> 0 then Exit;
    Skip;
    case Current() of
        '^' : begin
                Next;
                R := Expr2nd;
                if (Error <> 0) then Exit;
                if (Result <= 0) then
                begin
                  if (not ((R = 0) xor (Result = 0)) and (R = Int(R))) then
                  begin
                    Result := IntPower(Result, Trunc(R));
                    Exit;
                  end;
                  while Current <> '^' do Dec(Idx);
                  Dec(Idx);
                  SkipReverse();
                  Error := Idx;
                  Description := Err_Power;
                  Exit;
                end;
                if (Result <> 0) then Result := Exp(Ln(Result) * R);
              end;
    end;
  end;

  function Expr3rd(): Double;
  var
    R: Double;
    Id: string;
  begin
    Skip();
    Result := Expr2nd();
    if Error <> 0 then Exit;
    Skip();
    case LowCase(Current()) of
        '%', 'm':
             begin
                if Current <> '%' then
                  begin
                    Id := ReadIdentifier();
                    if Id <> 'mod' then
                    begin
                      Dec(Idx, Length(Id));
                      Exit;
                    end;
                  end
                else
                  Id := '%';
                Next();
                R := Expr3rd();
                if (Error <> 0) then Exit;
                if (R = 0) then
                begin
                  Error := Idx;
                  Description := Err_ZeroDiv;
                  Exit;
                end;
                if (Trunc(Result) <> Result) or (Trunc(R) <> R) then
                begin
                  Error := Idx;
                  Description := Format(Err_Int, [Id]);
                  Exit;
                end;
                Result := Trunc(Result) mod Trunc(R);
              end;
    end;
  end;

  function Expr3rdBis(): Double;
  var
    R: Double;
    Id: string;
  begin
    Skip();
    Result := Expr3rd();
    if (Error <> 0) then Exit;
    Skip();
    case LowCase(Current()) of
        'd':
             begin
                Id := ReadIdentifier();
                if (Id <> 'div') then
                begin
                  Dec(Idx, Length(Id));
                  Exit;
                end;
                Next;
                R := Expr3rdBis();
                if (Error <> 0) then Exit;
                if (R = 0) then
                begin
                  Error := Idx;
                  Description := Err_ZeroDiv;
                  Exit;
                end;
                if (Trunc(Result) <> Result) or (Trunc(R) <> R) then
                begin
                  Error := Idx;
                  Description := Format(Err_Int, [Id]);
                  Exit;
                end;
                Result := Trunc(Result) div Trunc(R);
              end;
    end;
  end;

  function Expr4th(): Double;
  var
    R: Double;
  begin
    Skip();
    Result := Expr3rdBis();
    if (Error <> 0) then Exit;
    Skip();
    case Current() of
        '/' : begin
                Next();
                R := Expr4th();
                if (Error <> 0) then Exit;
                if (R = 0) then
                begin
                  Error := Idx;
                  Description := Err_ZeroDiv;
                  Exit;
                end;
                Result := Result / R;
              end;
    end;
  end;

  function Expr(): Double;
  begin
    StackLevel += 1;
    Skip();
    Result := Expr4th();
    if (Error <> 0) then Exit;
    Skip();
    case LowCase(Current()) of
        '*' : begin
                Next();
                Result := Result * Expr();
              end;
        'a'..'z', '0'..'9', '_', '(' :
              begin
                Result := Result * Expr();
              end;
    end;
    //AfficherMessageErreur(Format('Stack level %d: %s', [StackLevel, Expression]));
    StackLevel -= 1;
  end;
begin
  Idx := 1;
  StackLevel := 0;
  Expression := Trim(Lowercase(MyExpression));
  // Habitudes du tableur: si on trouve un égal en tête, on le vire
  if (Expression[1] = '=') then System.Delete(Expression, 1,1);
  // Affectation d'une variable du type a=(1+1)
  //if ((Expression[1] in ['a' .. 'z']) AND (Expression[2] = '=')) then
  //begin

  //end;
  // remplacement des variables par leurs valeurs
  if (not RemplacerVariablesParLeursValeurs(Expression, Error)) then exit(0);

  // le premier caractère est un '+' = on le supprime
  if (Expression[1] = '+') then System.Delete(Expression, 1,1);
  // l'expression démarre par une virgule = on ajoute '0' en tête
  if (Expression[1] = DefaultFormatSettings.DecimalSeparator) then Expression := '0' + Expression;
  // la suite ...
  L := Length(Expression);
  Sign := 1;
  Result := 0;
  Error := 0;
  Skip();
  if (Idx > L) then
  begin
    Error := 1;
    Description := Err_Expr;
    Exit;
  end;
  while ((Error = 0) and (Idx <= L)) do
  begin
    Skip();
    while ((Idx <= L) and (Current() in ['+', '-'])) do
    begin
      if (Current = '-') then Sign := -Sign;
      Next();
      Skip();
    end;
    Result := Result + Sign * Expr();
    Sign := 1;
  end;
end;

{ TEvaluateurExpressions }

function TEvaluateurExpressions.Initialiser: boolean;
var
  i: Integer;
begin
  Result := False;
  //FFichierIni := ExtractFilePath(ParamStr(0)) + 'Calculator.ini';
  FListeDeVariables := TStringList.Create;
  try
    FListeDeVariables.NameValueSeparator := '=';
    FListeDeVariables.Clear;
    for i := 0 to 25 do AddVariable(chr(65+i), 0.00);
    //LoadFromIni();
    Result := True;
  except

  end;
end;
(*
function TEvaluateurExpressions.LoadFromIni(): boolean;
var
  MyIniFile: TIniFile;
  QLstVars: TStringList; // Rappel: Un TStringList contient également un dictionnaire
                         // dont les clés sont dans .Names[] et les valeurs dans .ValueFromIndex[]
  i, n: Integer;
begin
  result := True;
  MyIniFile := TIniFile.Create(FFichierIni);
  try
    QLstVars := TStringList.Create;
    try
      //QLstVars.clear; // à activer ASAP
      QLstVars.NameValueSeparator := '='; // facultatif: défini à '=' par défaut
      MyIniFile.ReadSectionValues(INI_SECTION_VARIABLES, QLstVars);
      n := QLstVars.Count;
      //ShowMessage(inttostr(n));
      if (n >= 0) then
      begin
        for i := 0 to n - 1 do
        begin
          //ShowMessage(QLstVars.Strings[i] + 'K=' + QLstVars.Names[i] + ' - V = ' + QLstVars.ValueFromIndex[i]);
          AddVariable(QLstVars.Names[i], ConvertirEnNombreReel(QLstVars.ValueFromIndex[i], 0.00));

        end;
      end;
    finally
      QLstVars.Free;
    end;
  finally
    // After the ini file was used it must be freed to prevent memory leaks.
    MyIniFile.Free;
  end;

end;
//*)
(*
procedure TEvaluateurExpressions.SaveToIni();
var
  MyIniFile: TIniFile;
  n, i: Integer;
  VV: TVariable;
begin
  MyIniFile := TIniFile.Create(FFichierIni);
  try
    n := GetNbVariables();
    for i := 0 to n - 1 do
    begin
      VV := GetVariable(i);
      MyIniFile.WriteFloat(INI_SECTION_VARIABLES,  VV.Nom, VV.Valeur);
    end;
  finally
    // After the ini file was used it must be freed to prevent memory leaks.
    MyIniFile.Free;
  end;
end;
//*)

procedure TEvaluateurExpressions.Finaliser;
begin
  try
    //SaveToIni();
    FListeDeVariables.Clear;
  finally
    FListeDeVariables.Free;
  end;
end;

function TEvaluateurExpressions.GetNbreDeFonctions: integer;
begin
  Result := High(Identifiers);
end;

function TEvaluateurExpressions.GetNbVariables: integer;
begin
  Result := FListeDeVariables.Count;
end;

function TEvaluateurExpressions.GetNomFonction(const Idx: integer): string;
begin
  Result := Identifiers[Idx];
end;

function TEvaluateurExpressions.GetVariable(const Idx: integer): TVariable;
begin
  Result.Nom    := Trim(FListeDeVariables.Names[Idx]);
  Result.Valeur := StrToFloatDef(FListeDeVariables.ValueFromIndex[Idx], 0.00);
end;

procedure TEvaluateurExpressions.AddVariable(const V: TVariable); overload;
begin
  self.AddVariable(V.Nom, V.Valeur);
end;

procedure TEvaluateurExpressions.AddVariable(const K: string; const V: double); overload;
var
  n: Integer;
  vv: String;
begin
  n  := FListeDeVariables.IndexOfName(UpperCase(K));
  vv := FloatToStr(V);
  if (n >= 0) then // la variable existe
  begin
    FListeDeVariables.ValueFromIndex[n] := vv;
  end else
  begin
    FListeDeVariables.Add(Format('%s%s%s', [K, FListeDeVariables.NameValueSeparator, VV]));
  end;
end;

{$ELSE}
function IsAlphaNumeric(const S: string): boolean;
var
  n, i: Integer;
  Q: Boolean;
begin
  //ShowMessage('IsAlphaNumeric(): ' + S);
  result := True;
  n := length(S);
  if (n = 0) then Exit(false);
  //ShowMessage(S);
  // Premier caractère doit être une lettre
  if (not (upCase(S[1]) in ['A' ..'Z'])) then exit(false);
  for i := 1 to n do
  begin
    Q := upCase(S[i]) in ['A' .. 'Z', '0' .. '9', '_'];
    Result := Result and Q;
    ShowMessageFmt('%d - %s - %s %s', [i, upCase(S[i]), BoolToStr(Q, 'Vrai', 'Faux'), BoolToStr(Result, 'Vrai', 'Faux')]);
  end;
end;


function EvaluationRapideExpression(const Expr: string): double;
var
  EWE, QErrDesc: String;
  TD: TEvaluateurExpressions;
  QError: Integer;
begin
  Result := 0;
  // remplacer les virgules par le point décimal
  EWE := StringReplace(Expr, '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
  EWE := StringReplace(EWE , ',', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
  TD := TEvaluateurExpressions.Create;
  try
    QError := 0;
    QErrDesc := '';
    TD.Initialiser();
    result := TD.Evaluate(EWE, QError, QErrDesc);
    TD.Finaliser();
  finally
    TD.Free;
  end;
end;





{ TEvaluateurExpressions }
function TEvaluateurExpressions.Initialiser(): boolean;
begin
  ShowMessage('Initialiser');
  result := false;
  FPExpressionParser := TFPExpressionParser.Create(nil);
  ShowMessage('001');
  FPExpressionParser.BuiltIns := [bcMath, bcUser];
  //FPExpressionParser.Identifiers.;
  // quelques variables
  AddVariable('Diable', 666);
  AddVariable('Dieu', 404);

  result := true;
  ShowMessage('002');
end;

function TEvaluateurExpressions.LoadFromIni: boolean;
begin
  result := false;
end;

function TEvaluateurExpressions.RemplacerVariablesParLeursValeurs(var MyExpression: string; out PosErr: integer): boolean;
var
  P: SizeInt;
  V: TVariable;
  QLeft, QRight: String;
  n: Integer;
begin
  Result := false;
  // quelques protections
  MyExpression := ' ' + MyExpression + ' ';
  // on vérifie d'abord si l'expression est une affectation
  P := pos('=', MyExpression);
  if (P > 0) then
  begin
    // on supprime le sigill éventuel
    MyExpression := StringReplace(MyExpression, '$', '', [rfReplaceAll, rfIgnoreCase]);

    QLeft  := Trim(Copy(MyExpression, 1, P-1));
    QRight := Trim(Copy(MyExpression, P+1, Length(MyExpression)));
    ShowMessage(QLeft + ' <- ' + QRight);
    V.Nom    := Trim(QLeft);
    ShowMessage(V.Nom);
    if (IsAlphaNumeric(V.Nom)) then
    begin
      ShowMessage(QRight);
      V.Valeur := ConvertirEnNombreReel(Trim(QRight), 0.00);
      // et on ajoute la variable à la liste
      AddVariable(V);
    end;
    // avant de quitter avec Faux pour empêcher l'évaluation de l'expression
    exit(false);
  end;


  // on recense les variables
  //ShowMessage(MyExpression);
  (*
  n := FListeDeVariables.Count;
  if (n = 0) then Exit(True); // pas de variables = on quitte direct
  for i := 0 to n - 1 do
  begin
    // ici, StringReplace() fait tout le boulot à notre place
    // on préfixe les noms de variables par le sigill $, comme en PHP
    // et on remplace toutes les occurrences de $var par leur valeur
    V := GetVariable(i);
    MyExpression := StringReplace(MyExpression,
                                  '$'+ Trim(V.Nom),
                                  FloatToStr(V.Valeur),
                                  [rfReplaceAll, rfIgnoreCase]);
  end;
  //ShowMessage(MyExpression);
  //*)
  Result := True;
end;

procedure TEvaluateurExpressions.SaveToIni;
begin

end;

procedure TEvaluateurExpressions.Finaliser;
begin
  try
    //FPExpressionParser.Clear;
  finally
    FPExpressionParser.Free;
  end;
end;


procedure TEvaluateurExpressions.AddVariable(const K: string; const V: double);
begin
  self.FPExpressionParser.Identifiers.AddFloatVariable(K, V);
end;

procedure TEvaluateurExpressions.AddVariable(const V: TVariable);
begin
  self.AddVariable(V.Nom, V.Valeur);
end;

function TEvaluateurExpressions.Eval(Expression: string; Identifier: string; Value: Double): Double;
begin
  ;
end;

function TEvaluateurExpressions.Evaluate(const MyExpression: string; var Error: Integer; var Description: string; Identifier: string; Value: Double): Double;
var
  WU: TFPExpressionResult;
  EWE: Double;
  QExpression: string;
begin
  // remplacement des variables par leurs valeurs
  QExpression := MyExpression;
  if (not RemplacerVariablesParLeursValeurs(QExpression, Error)) then exit(0);
  FPExpressionParser.Expression := QExpression;
  FPExpressionParser.EvaluateExpression(WU);

  case WU.ResultType of
    rtBoolean  : EWE := ifthen(FPExpressionParser.AsBoolean, 1.0, 0.0);
    rtInteger  : EWE := FPExpressionParser.AsInteger * 1.00;
    rtFloat    : EWE := FPExpressionParser.AsFloat;
    rtDateTime : EWE := FPExpressionParser.AsFloat;
    rtString   : EWE := 0.00;
  end;

  Result := EWE;

end;


function TEvaluateurExpressions.GetNbreDeFonctions: integer;
begin
  result := 0; //FPExpressionParser.Identifiers.Count;
end;

function TEvaluateurExpressions.GetNbVariables: integer;
begin
  result := self.FPExpressionParser.Identifiers.Count;
end;

function TEvaluateurExpressions.GetNomFonction(const Idx: integer): string;
var
  WU: TFPExprIdentifierDefs;
  EWE: TFPExprIdentifierDef;
begin
  result := '---------'; Exit;
  WU := FPExpressionParser.Identifiers;
  EWE := WU.Identifiers[Idx];
  Result := EWE.Name;
end;

function TEvaluateurExpressions.GetVariable(const Idx: integer): TVariable;
var
  WU: TFPExprIdentifierDef;
begin
  WU := self.FPExpressionParser.Identifiers[Idx];
  Result.Nom := WU.Name;
  Result.Valeur := WU.AsFloat;
end;

{$ENDIF}


function FloatToFrac(X: Double; Format: string; Default: string): string;
const
  Delta = 1e-13;
var
  A, B: Cardinal;
  Sign: integer;
  X2: Double;
begin
  if Frac(X) = 0 then
  begin
    Result := SysUtils.Format('%d', [Trunc(X)]);
    Exit;
  end;
  A := 0;
  B := 1;
  Sign := Trunc(X / Abs(X));
  X2 := Frac(Abs(X));
  while (A < MAX_CARDINAL) and (B < MAX_FRAC) and (Abs(X2 - A / B) > Delta) do
  begin
    if X2 > A / B then Inc(A);
    if X2 < A / B then Inc(B);
  end;
  if Abs(X2 - A / B) > Delta then
    Result := SysUtils.Format(Default, [X])
  else
    Result := SysUtils.Format(Format, [Trunc(Sign * (A + Int(Abs(X)) * B)) , B]);
end;

end.
