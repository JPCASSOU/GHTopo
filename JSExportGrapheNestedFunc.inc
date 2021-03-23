// Fichier JSExportGrapheNestedFunc.inc
// appelé par UnitGraphes1.pas
// contenant les sous-fonctions de la méthode suivante
(* Exporter le graphe en Javascript
procedure TPathFindingGraphe.ExporterGrapheEnJavascript(const DocTitle: string;
                                                        const FileNameJS: RawByteString;
                                                        const QIDStationDepart, QIDStationArrivee: string;
                                                        const BackgroundColorCarte, CouleurCenterlines: TColor;
                                                        const CanvasWidthInPixels, CanvasHeightInPixels, WidthMenuLateralInPixels: integer);
//*)
//******************************************************************************
// primitives de base
procedure WriteLine(const S: string); inline;
begin
  WriteLn(fp, S);
end;
procedure WrtLinFmt(Const Fmt : String; const Args : Array of const);
begin
  WriteLn(fp, Format(Fmt, Args));
end;
procedure WriteDebugLine(const S: string); inline;
begin
  WriteLn(fp, Format('console.log(%s);', [S]));
end;
procedure WrtDebugFmtLin(const fmt: string; const Args : Array of const); inline;
begin
  WriteDebugLine(Format(Fmt, Args));
end;
procedure JSSeparateur(const QMotif: Char = '='; const nb: integer = 80);
begin
  WriteLine('//' + StringOfChar(QMotif, nb));
end;
// utilisé notamment pour tester les nouvelles fonctions
// sans avoir à les sauvegarder ailleurs
procedure BeginJSVerbatim(const Desc: string);
begin
  WriteLine(StringOfChar('/', 132));
  WriteLine('// VERBATIM: ' + Desc);
  WriteLine(StringOfChar('/', 132));
end;

procedure JSVerbatim(const L: string);
begin
  WriteLine(L);
end;

procedure EndJSVerbatim();
begin
  WriteLine(StringOfChar('/', 132));
  WriteLine('// END VERBATIM:');
  WriteLine(StringOfChar('/', 132));
end;

// fonctions utilitaires
function QColorToHTMLColor(const Couleur: TColor): string;
begin
  Result := Format('#%.2X%.2X%.2X', [Red(Couleur), Green(Couleur), Blue(Couleur)]);
end;
function ToJSCoord(const X: double): integer;
begin
  result := trunc(TO_DECIMETERS * X);
end;
// wrappers des structures de contrôle de flux (IF, WHILE, SWITCH, ...)
procedure jsFOR(const QVarCounter, QVarNb: string; const QStart: integer = 0; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%sfor (var %s = %d; %s < %s; %s++) // -- FOR %s', [WU, QVarCounter, QStart, QVarCounter, QVarNb, QVarCounter, QTag]);
  WrtLinFmt('%s{', [WU]);
end;
procedure jsNEXT(const QVarCounter: string; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} // -- NEXT %s ... %s', [WU, QVarCounter, QTag]);
end;
procedure jsWHILE(const QCondition: string; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%swhile (%s) // -- WHILE %s', [WU, QCondition, QTag]);
  WrtLinFmt('%s{', [WU]);
end;
procedure jsWEND(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} // -- WEND %s', [WU, QTag]);
end;
procedure jsIF(const QCondition: string; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%sif (%s) // %s', [WU, QCondition, QTag]);
  WrtLinFmt('%s{', [WU]);
end;
procedure jsELSE(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} else { // %s', [WU, QTag]);
end;
procedure jsENDIF(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} // %s', [WU, QTag]);
end;

procedure jsSELECT_CASE(const QSelecteur: string; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%sswitch(%s) { // %s', [WU, QSelecteur, QTag]);
end;
procedure jsCASE(const QItem: integer; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s  case %d: // %s', [WU, QItem, QTag]);
end;
procedure jsBREAK(const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s    break;', [WU]);
end;
procedure jsCASE_ELSE(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s  default: // %s', [WU, QTag]);
end;
procedure jsEND_SELECT(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} // switch %s', [WU, QTag]);
end;

// les classes CSS
procedure BeginCSSClass(const QCSSClass: string);
begin
  WrtLinFmt('       #%s{', [QCSSClass]);
end;
procedure EndCSSClass();
begin
  WriteLine('       }');
end;
// les différentes sections du fichier HTML
procedure BeginHTML();
begin
  WriteLine('<!DOCTYPE html>');
  WrtLinFmt('<!-- Generated %s by %s version %s -->', [DateTimePascalToDateTimeSQL(Now), ApplicationName, GetGHTopoVersion()]);
  WriteLine('<HTML>');
end;
procedure EndHTML();
begin
  WriteLine('</HTML>');
end;
procedure BeginCSS_Styles();
begin
   WriteLine('<STYLE>');
end;
procedure EndCSS_Styles();
begin
  WriteLine('</STYLE>');
end;
procedure BeginHEAD();
begin
  WriteLine('<HEAD>');
  WrtLinFmt('  <TITLE>%s</TITLE>', [DocTitle]);
  WrtLinFmt('  <META charset="%s" />', ['utf-8']);
end;
procedure EndHEAD();
begin
  WriteLine('</HEAD>');
end;

procedure BeginScriptSection();
begin
  WrtLinFmt('  <script type="%s">', [MIME_TEXT_JAVASCRIPT]);
  WrtLinFmt('// %s generated by GHTopo at %s', [FileNameJS, DatePascalToDateSQL(Now())]);
end;
procedure EndScriptSection();
begin
  WriteLine('  </SCRIPT>');
end;
procedure BeginBODY(const MyJS_FUNCTION_INITIALISER, MyIDStationDepart, MyIDStationArrivee: string);
begin
  WrtLinFmt('<BODY onLoad="%s(''%s'', ''%s'')">', [MyJS_FUNCTION_INITIALISER, MyIDStationDepart, MyIDStationArrivee]);
end;

procedure EndBODY();
begin
  WriteLine('  <noscript>' + 'JavaScript is required' + '</noscript>' );
  WriteLine('</BODY>');
end;
// composants d'un doc html: divs, forms, tableaux
procedure BeginDiv(const QID: string; const QIndent: integer = 2);
begin
  WrtLinFmt('%s<div id="%s">', [StringOfChar(' ', QIndent), QID]);
end;
procedure EndDiv(const QIndent: integer = 2);
begin
  WrtLinFmt('%s</div>', [StringOfChar(' ', QIndent)]);
end;
procedure BeginForm(const QName: string; const QIndent: integer = 4);
begin
  WrtLinFmt('%s<form name="%s">', [StringOfChar(' ', QIndent), QName]);
end;
procedure EndForm(const QIndent: integer = 4);
begin
  WrtLinFmt('%s</form>', [StringOfChar(' ', QIndent)]);
end;
procedure BeginTable(const WidthInPercent, HeightInPercent: integer; const QIndent: integer = 4); overload;
begin
  WrtLinFmt('    %s<table style="width:%d%%;height:%d%%;">', [StringOfChar(' ', QIndent), WidthInPercent, HeightInPercent]);
end;
procedure BeginRow(const QIndent: integer = 6); overload;
begin
  WrtLinFmt('    %s<tr>', [StringOfChar(' ', QIndent)]);
end;
procedure EndRow(const QIndent: integer = 6); overload;
begin
  WrtLinFmt('    %s</tr>', [StringOfChar(' ', QIndent)]);
end;
procedure EndTable(const QIndent: integer = 4);
begin
  WrtLinFmt('    %s</table>', [StringOfChar(' ', QIndent)]);
end;
// positionnement d'objets HTML (DIV, tableaux)
procedure PositionnerPanneau(const QNamePanneau: string;
                               const QLeft, QTop, QWidth, QHeight: integer;
                               const QBorderStyle: string;
                               const QBorderColor, QBackGroundColor: TColor;
                               const QFontName: string   = 'Arial';
                               const QFontSize: integer  = 15;
                               const QFontAttr: string   = 'bold');
begin
  BeginCSSClass(QNamePanneau);         //   WrtLinFmt('       #%s{', []);
    WrtLinFmt('         %s:%s;'         , ['position', 'absolute']);
    WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS, ['left'  , QLeft]);
    WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS, ['top'   , QTop]);
    WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS, ['width' , QWidth]);
    WrtLinFmt(FMT_POSITION_DIV_IN_PIXELS, ['height', QHeight]);
    WrtLinFmt('         %s:%s;'         , ['border-style', QBorderStyle]);
    WrtLinFmt('         %s:%s;'         , ['color'     , QColorToHTMLColor(QBorderColor)]);
    WrtLinFmt('         %s:%s;'         , ['background', QColorToHTMLColor(QBackGroundColor)]);
    WrtLinFmt('         %s:%dpx %s;'    , ['font', QFontSize, QFontName]);
    WrtLinFmt('         %s:%s;'         , ['font-weight', QFontAttr]);
  EndCSSClass();
end;

procedure JSMakeButtonInTable(const btnName, btnCaption, btnOnClick: string);
var
  MyStyleButton: String;
begin
  MyStyleButton := Format('width:%d%%;height:%d%%;', [100, 100]) +
                   format('font-family:%s,%s;'     , ['arial', 'sans-serif']) +
                   Format('font-size:%dpx;'        , [24]) +
                   Format('font-weight:%s;'        , ['bold']);
  WrtLinFmt('    <TD width="%d%%" height="%d%%"><INPUT type="button" name="%s" value="%s" style="%s" onclick="%s()"> </TD>',
             [33, 33, btnName, btnCaption, MyStyleButton, btnOnClick]);
end;
procedure JSMakeLabelledEdit(const LbName, LbCaption: string; const EditName, EditText: string);
begin
  WrtLinFmt('    <LABEL for="%s">%s</LABEL><BR>'    , [LbName, LbCaption]);
  WrtLinFmt('    <INPUT type="text" id="%s" style="width:%d%%" name="%s" value="%s"><BR>', [EditName, 95, EditName, EditText]);
end;
// définitions de variables globales
procedure QRedigeDeclGlobalVars();
const
  VAR_FMT_NUM   = 'var %s = %d;';
  VAR_FMT_UNDEF = 'var %s;';
begin
  WriteLine('');
  WriteLine('// Variables globales');
  WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FXMini, 0]);
  WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FXMaxi, 0]);
  WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FYMini, 0]);
  WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FYMaxi, 0]);
  WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FXCentreMap, 0]);
  WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FYCentreMap, 0]);
  WrtLinFmt(VAR_FMT_NUM, [JS_GLOBAL_VAR_FLastIdxFound, -1]);
  WrtLinFmt('var %s = new T%s;', [JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME]);
  WriteLine('// Variables des contrôles');
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyContainer]);
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyCanvas]);
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MylbMouseCoords]);
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MylbRoadmapNbPoints]);
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MylsbRoadMap]);
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyeditFindWhat]);
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyeditStationDepart]);
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_MyeditStationArrivee]);
  WrtLinFmt(VAR_FMT_UNDEF, [JS_GLOBAL_VAR_FGlobalNoeudCourant]);
  WriteLine('// Variables globales pour mettre en cache l''état de l''événement');
  WrtLinFmt('var %s = new Array();', [JS_GLOBAL_VAR_EvtCache]);
  WrtLinFmt('var %s = %d;', [JS_GLOBAL_VAR_PrevDiff, -1]);
  JSSeparateur('=', 120);
end;
// définition de variables JS
procedure jsSetLocalVar(const QVarName: string; const QExpr: string; const QComment: string = ''); overload;
begin
  WrtLinFmt('  var %s = %s; // %s', [QVarName, QExpr, QComment]);
end;
procedure jsSetLocalVar(const QVarName: string; const QValue: integer; const QComment: string = ''); overload;
begin
  WrtLinFmt('  var %s = %d; // %s', [QVarName, QValue, QComment]);
end;
procedure jsSetLocalVar(const QVarName: string; const QValue: double; const QComment: string = ''); overload;
begin
  WrtLinFmt('  var %s = %.10f; // %s', [QVarName, QValue, QComment]);
end;
// définitions de fonctions JS
procedure BeginJSFunction(const QProcName: string; const QParams: string = '');
begin
  FCurrentProcName := QProcName;
  WrtLinFmt('function %s(%s)', [FCurrentProcName, QParams]);
  WriteLine('{');
end;
procedure EndJSFunction();
begin
  WrtLinFmt('} // EndJSFunction %s ', [FCurrentProcName]);
  JSSeparateur('-', 60);
end;
procedure BeginJSNestedFunction(const QProcName: string; const QParams: string = '');
begin
  WrtLinFmt('  function %s(%s) // %s', [QProcName, QParams, 'Nested']);
  WriteLine('  {');
end;
procedure EndJSNestedFunction();
begin
  WriteLine('  } // EndJSNestedFunction');
end;
procedure BeginJSEventListener(const EventName: string; const Desc: string = '');
begin
  WrtLinFmt('  // Listener: %s - %s', [EventName, Desc]);
  WrtLinFmt('  %s.addEventListener("%s", function(e) {', ['window', EventName]);
end;
procedure EndJSEventListener();
begin
  WriteLine('  });');
  JSSeparateur('-', 60);
end;

//******************************************************************************
procedure QBeginRedigerFonctionsUtilitaires();
 begin
   JSSeparateur();
   WrtLinFmt('// Les %s', [JS_SECTION_FUNCTIONS_UTILITAIRES]);
 end;
 procedure QEndRedigerFonctionsUtilitaires();
 begin
   JSSeparateur();
   WrtLinFmt('// Fin section %s', [JS_SECTION_FUNCTIONS_UTILITAIRES]);
   WriteLine('');
 end;

 procedure QRedigeFunctionGetAzimut();
 const
   VARNAME_TWO_PI = 'TWO_PI';
   VARNAME_a      = 'a';
 begin
   BeginJSFunction(JS_UTILITY_FUNC_GetAzimut, 'dx, dy, Unite');
     WrtLinFmt('  const %s = 2 * %s.PI;', [VARNAME_TWO_PI, JS_MATH_LIBRARY]);
     WrtLinFmt('  var %s = %s.atan2(dy, dx + 1e-12);', [VARNAME_a, JS_MATH_LIBRARY]);
     WrtLinFmt('  if (%s < 0) %s = %s + %s;', [VARNAME_a, VARNAME_a, VARNAME_a, VARNAME_TWO_PI]);
     WrtLinFmt('  %s = 0.50 * %s.PI - %s;', [VARNAME_a, JS_MATH_LIBRARY, VARNAME_a]);      // a := 0.50 * PI - a;
     WrtLinFmt('  if (%s < 0) %s = %s + %s;', [VARNAME_a, VARNAME_a, VARNAME_a, VARNAME_TWO_PI]);
     WrtLinFmt('  return %s * 0.50 * Unite / %s.PI;', [VARNAME_a, JS_MATH_LIBRARY]);
   EndJSFunction();
 end;
procedure QRedigeFunctionGetBearingInc();
const
   VARNAME_dx   = 'dx';
   VARNAME_dy   = 'dy';
   VARNAME_dz   = 'dz';
   VARNAME_fUB  = 'fUB';
   VARNAME_fUC  = 'fUC';
   VARNAME_qdist = 'qdist';
   VARNAME_qdp   = 'qdp';
   VARNAME_qAz   = 'qaz';
   VARNAME_qInc  = 'qincl';
   QFMT_HYPOT = '%s.hypot(%s, %s)';
var
  WU: String;
begin
  WU := format('%s, %s, %s, %s, %s', [VARNAME_dx  , VARNAME_dy, VARNAME_dz, VARNAME_fUB , VARNAME_fUC]);
  BeginJSFunction(JS_UTILITY_FUNC_GetBearingInc, WU);
    jsSetLocalVar(VARNAME_qdp    , format(QFMT_HYPOT, [JS_MATH_LIBRARY, VARNAME_dx , VARNAME_dy]), 'Dist HZ');
    jsSetLocalVar(VARNAME_qdist  , format(QFMT_HYPOT, [JS_MATH_LIBRARY, VARNAME_qdp, VARNAME_dz]), 'Distance');
    jsSetLocalVar(VARNAME_qInc   , format('%s.atan2(%s, %s) * 0.5 * %s / %s.PI', [JS_MATH_LIBRARY, VARNAME_dz, VARNAME_qdp, VARNAME_fUC, JS_MATH_LIBRARY]), '');
    jsSetLocalVar(VARNAME_qAz    , format('%s(%s, %s, %s)', [JS_UTILITY_FUNC_GetAzimut, VARNAME_dx, VARNAME_dy, VARNAME_fUB]), '');
    WriteLine('  return {');
    WrtLinFmt('    %s: %s / 10.0,', [JS_VARNAME_OUT_Dist, VARNAME_qdist]);
    WrtLinFmt('    %s: %s,', [JS_VARNAME_OUT_Az  , VARNAME_qAz]);
    WrtLinFmt('    %s: %s', [JS_VARNAME_OUT_Inc , VARNAME_qInc]);
    WriteLine('  }; // return tuple');
  EndJSFunction();
end;


procedure JSTraceLigne(const SX1, SY1, SX2, SY2: string);
begin
  WrtLinFmt('  %s(%s, %s);', [JS_FUNC_CoordsToPlan, SX1, SY1]);
  WriteLine('  PS1X = PX;');
  WriteLine('  PS1Y = PY;');
  WrtLinFmt('  %s(%s, %s);', [JS_FUNC_CoordsToPlan, SX2, SY2]);
  WriteLine('  PS2X = PX;');
  WriteLine('  PS2Y = PY;');
  WrtLinFmt('  %s.beginPath();'         , [NAMEREF_CANVAS_DC]);
  WrtLinFmt('  %s.moveTo(PS1X, PS1Y);'  , [NAMEREF_CANVAS_DC]);
  WrtLinFmt('  %s.lineTo(PS2X, PS2Y);'  , [NAMEREF_CANVAS_DC]);
  WrtLinFmt('  %s.stroke();'            , [NAMEREF_CANVAS_DC]);
end;

procedure JSToto(const BeginOrEndPath, JS_FRM_EditStation, JS_Varname_Station, JS_Varname_IdxStation: string);
begin
  WrtLinFmt('  var %s   = %s.value;', [JS_Varname_Station, JS_FRM_EditStation]);
  WrtLinFmt('  var %s = %s.FindIdxNoeudByCle(%s);', [JS_Varname_IdxStation, JS_GRAPHE_CLASS_VAR_NAME, JS_Varname_Station]);
  jsIF(Format('%s < 0', [JS_Varname_IdxStation]), '');
    WrtLinFmt('     window.alert("Station %s ''" + %s + "'' introuvable");', [BeginOrEndPath, JS_Varname_Station]);
    WriteLine('     return;');
  jsENDIF();
end;
// définitions de méthodes de classe
procedure BeginClassJSFunction(const QClassName, QMethod: string; const QParams: string = '');
begin
  FCurrentClassName:= QClassName;
  FCurrentProcName := QMethod;
  WrtLinFmt('// BeginClassJSFunction: *** ----- %s -- %s', [FCurrentClassName, FCurrentProcName]);
  WrtLinFmt('  %s.%s = function(%s)', [FCurrentClassName, FCurrentProcName, QParams]);
  WriteLine('  {');
end;
procedure EndClassJSFunction();
begin
  WrtLinFmt('  } // EndClassJSFunction // %s.%s', [FCurrentClassName, FCurrentProcName]);
  JSSeparateur('-', 60);
end;

procedure JSSetMemberAtThis(const Varname: string);
begin
  WrtLinFmt('  %s.%s = Q%s;', [JS_THIS, Varname, Varname]);
end;

// Classe TJSGraphe
procedure QBeginClasseTJSGraphe();
begin
  WrtLinFmt('// La classe T%s', [JS_GRAPHE_CLASS_VAR_NAME]);
  WrtLinFmt('function T%s() {', [JS_GRAPHE_CLASS_VAR_NAME]);
  WrtLinFmt('  %s.%s  = new Array();', [JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
  WrtLinFmt('  %s.%s    = new Array();', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);

  WrtLinFmt('  %s.FXMini       = %d;', [JS_THIS, ToJSCoord(self.FXMini)]);
  WrtLinFmt('  %s.FYMini       = %d;', [JS_THIS, ToJSCoord(self.FYMini)]);

  WrtLinFmt('  %s.FXMaxi       = %d;', [JS_THIS, ToJSCoord(self.FXMaxi)]);
  WrtLinFmt('  %s.FYMaxi       = %d;', [JS_THIS, ToJSCoord(self.FYMaxi)]);
  BeginClassJSFunction(JS_THIS, 'Init');
    pass;
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, 'AddNoeud', JS_ARGS_ADD_NOEUD);
    WrtLinFmt('    %s.%s.push(new TJSGrapheNode(%s));', [JS_THIS, JS_GRAPH_LISTE_NOEUDS, JS_ARGS_ADD_NOEUD]);
    WrtLinFmt('    %s.FNbNoeuds = %s.%s.length;', [JS_THIS, JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, 'GetNoeud', 'Index');
    WrtLinFmt('    return %s.%s[Index];', [JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, 'GetNbNoeuds');
    WrtLinFmt('    return %s.%s.length;', [JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, 'FindIdxNoeudByCle', 'Cle');
    jsSetLocalVar('Nb', Format('%s.GetNbNoeuds()', [JS_THIS]), 'Nombre de noeuds (test)');
    WriteLine('    if (0 == Nb) return -1;');
    jsFOR('i', 'Nb', 0, '', 4);
      WrtLinFmt('      if (%s.%s[i].IDNoeud === Cle) return i;', [JS_THIS, JS_GRAPH_LISTE_NOEUDS]);
    jsNEXT('i');
    WriteLine('    return -1;');
  EndClassJSFunction();
end;
procedure QEndClasseTJSGraphe();
begin
  WriteLine('} // End class' + JS_GRAPHE_CLASS_VAR_NAME);
  JSSeparateur('+', 132);
end;

procedure QRedigeClasseTJSGrapheNode();
const
  CLASSNAME_TJSGrapheNode = 'TJSGrapheNode';
begin
  BeginJSFunction(CLASSNAME_TJSGrapheNode, JS_ARGS_ADD_NOEUD);
    JSSetMemberAtThis('IDNoeud');
    JSSetMemberAtThis('X');
    JSSetMemberAtThis('Y');
    JSSetMemberAtThis('Z');
    JSSetMemberAtThis('ListeVoisins');
  EndJSFunction();
end;
procedure QRedigeClassJSGrapheFunctionFindIdxNoeudNearestToXY();
const FMT_QDXY = '(%s - %s.%s[i].%s)';
begin
  BeginClassJSFunction(JS_THIS, 'FindIdxNoeudNearestToXY', 'QX, QY');
    jsSetLocalVar('Result', -1);           //      WrtLinFmt('    var %s = %d;', ['Result', -1]);
    jsSetLocalVar('Nb', format('%s.GetNbNoeuds()', [JS_THIS]), 'cuicui');  ; //   WrtLinFmt('    var Nb = %s.GetNbNoeuds();', [JS_THIS]);
    WriteLine('    if (0 == Nb) return -1;');
    jsSetLocalVar('QDistance', JS_CONST_INFINITY, 'Infinité');//      WrtLinFmt('    var %s = %s;', ['QDistance', JS_CONST_INFINITY]);
    jsFOR('i', 'Nb', 0, '', 4);
      jsSetLocalVar('qdx', Format(FMT_QDXY, ['QX', JS_THIS, JS_GRAPH_LISTE_NOEUDS, 'X']), 'Qdx'); // WrtLinFmt('       var %s = (QX - %s.%s[i].%s);', ['qdx', JS_THIS, JS_GRAPH_LISTE_NOEUDS, 'X']);
      jsSetLocalVar('qdy', Format(FMT_QDXY, ['QY', JS_THIS, JS_GRAPH_LISTE_NOEUDS, 'Y']), 'Qdy'); // WrtLinFmt('       var %s = (QY - %s.%s[i].%s);', ['qdy', JS_THIS, JS_GRAPH_LISTE_NOEUDS, 'Y']);
      jsSetLocalVar('qdist', format('%s * %s  + %s * %s', ['qdx', 'qdx', 'qdy', 'qdy']), 'distance en plan');  //        WrtLinFmt('       var %s = %s * %s  + %s * %s;', ['qdist', 'qdx', 'qdx', 'qdy', 'qdy']);
      jsIF(format('%s < %s', ['qdist', 'QDistance']), '', 8);
        WrtLinFmt('          %s = %s;', ['QDistance', 'qdist']);
        WrtLinFmt('          %s = %s;', ['Result', 'i']);
      jsENDIF('', 8);
    jsNEXT('i');
    WriteLine('    return Result;');
  EndClassJSFunction();
end;
procedure QRedigeClassJSGrapheFunctionDijkstra();
const FMT_FIND_IDX_NOEUD = '%s.FindIdxNoeudByCle(%s)';
begin
  WriteLine('  // Algo de Dijkstra');
  BeginClassJSFunction(JS_THIS, 'Dijkstra', 'StationDepart, StationArrivee');
    jsSetLocalVar(JS_VAR_IDX_ND_DEPART , Format(FMT_FIND_IDX_NOEUD, [JS_THIS, 'StationDepart']) , 'Recherche départ');
    jsSetLocalVar(JS_VAR_IDX_ND_ARRIVEE, Format(FMT_FIND_IDX_NOEUD, [JS_THIS, 'StationArrivee']), 'Recherche arrivée');
    WrtLinFmt('    if (%s == -1) return -1;', [JS_VAR_IDX_ND_DEPART]);
    WrtLinFmt('    if (%s == -1) return -2;', [JS_VAR_IDX_ND_ARRIVEE]);
    WrtLinFmt('    if (%s == %s) return -3;', [JS_VAR_IDX_ND_DEPART, JS_VAR_IDX_ND_ARRIVEE]);
    WrtLinFmt('    %s.%s  = []; // vider le chemin le plus court', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
    WriteLine('    // Variables locales');
    jsSetLocalVar(JS_VARNAME_CURRENT_NODE, 'null');
    jsSetLocalVar('MyNodeVoisin', 'null');
    jsSetLocalVar('QIdxVoisin', 0);
    jsSetLocalVar('NbVoisins' , 0);
    jsSetLocalVar('QDistance' , 0.00);
    jsSetLocalVar(JS_VARNAME_NbNoeuds, format('%s.GetNbNoeuds()', [JS_THIS]), 'miaou');
    jsSetLocalVar(JS_LOCAL_ListePreds,  'new Array()', 'Liste des prédécesseurs');
    jsFOR('i', JS_VARNAME_NbNoeuds, 0, '', 4);
      WrtLinFmt('      %s[i] = %d;', [JS_LOCAL_ListePreds, 0]);
    jsNEXT('i');

    jsSetLocalVar(JS_LOCAL_ListeDistances,  'new Array()', 'Liste des distances');
    jsFOR('i', JS_VARNAME_NbNoeuds, 0, '', 4);
      WrtLinFmt('   %s[i] = %s; ', [JS_LOCAL_ListeDistances, JS_CONST_INFINITY]);
    jsNEXT('i', '', 4);
    WrtLinFmt('    %s[%s] = 0;', [JS_LOCAL_ListeDistances, JS_VAR_IDX_ND_DEPART]);
    jsSetLocalVar(JS_LOCAL_ListeNdsVisites,  'new Array()', 'Liste des déjà vus');
    jsFOR('i', JS_VARNAME_NbNoeuds, 0, '', 4);
      WrtLinFmt('     %s[i] = false;', [JS_LOCAL_ListeNdsVisites]);
    jsNEXT('i');
    WriteLine('    // Retourne le nombre de noeuds visités');
    BeginJSNestedFunction(JS_FUNCTION_NbNoeudsVisites);
      WriteLine('      Result = 0;');
      jsFOR('i', JS_VARNAME_NbNoeuds, 0, '', 4);
         WrtLinFmt('     if (%s[i]) Result++;', [JS_LOCAL_ListeNdsVisites]);
      jsNEXT('i');
      WriteLine('      return Result;');
    EndJSNestedFunction();
    WriteLine('    // Retourne le noeud le plus proche dans les noeuds non visités');
    BeginJSNestedFunction(JS_FUNCTION_TrouveMin);
      jsSetLocalVar('Result', -1);
      jsSetLocalVar('DistMini', JS_CONST_INFINITY);
      jsFOR('i', JS_VARNAME_NbNoeuds, 0, '', 6);
        jsIF(Format('!%s[i]', [JS_LOCAL_ListeNdsVisites]), '', 8);
          jsIF(Format('%s[i] < DistMini', [JS_LOCAL_ListeDistances]), '', 10);
            WrtLinFmt('            DistMini = %s[i];', [JS_LOCAL_ListeDistances]);
            WriteLine('            Result = i;');
          jsENDIF('', 10);
        jsENDIF('', 8);
      jsNEXT('i', '', 6);
      WriteLine('      return Result;');
    EndJSNestedFunction();
    jsWHILE(Format('%s() < %s', [JS_FUNCTION_NbNoeudsVisites, JS_VARNAME_NbNoeuds]), 'Boucle while principale', 4);
      jsSetLocalVar('N1', format('%s()', [JS_FUNCTION_TrouveMin]), '');
      WrtLinFmt('       if (N1 == %s) break;', [JS_VAR_IDX_ND_ARRIVEE]);
      WrtLinFmt('       %s[N1] = true;', [JS_LOCAL_ListeNdsVisites]);
      WrtLinFmt('       %s = %s.GetNoeud(N1);', [JS_VARNAME_CURRENT_NODE, JS_THIS]);
      WrtLinFmt('       NbVoisins = %s.ListeVoisins.length;', [JS_VARNAME_CURRENT_NODE]);
      jsIF('NbVoisins > 0', '', 8);
        jsFOR('k', 'NbVoisins', 0, '', 10);
          WrtLinFmt('           QIdxVoisin = %s.ListeVoisins[k];', [JS_VARNAME_CURRENT_NODE]);
          WriteLine('           // fonction MajDistances() déroulée ');
          WrtLinFmt('           var Poids = %s.%s(N1, QIdxVoisin);', [JS_THIS, JS_FUNCTION_DistanceBetweenNodes]);
          jsIF(Format('%s[QIdxVoisin] > (%s[N1] + Poids)', [JS_LOCAL_ListeDistances, JS_LOCAL_ListeDistances]), '', 12);
            WrtLinFmt('             %s[QIdxVoisin] = %s[N1] + Poids;', [JS_LOCAL_ListeDistances, JS_LOCAL_ListeDistances]);
            WrtLinFmt('             %s[QIdxVoisin] = N1;', [JS_LOCAL_ListePreds]);
          jsENDIF('', 12);
        jsNEXT('k', '', 10);
      jsENDIF('', 8);
    jsWEND('', 4);
    WriteLine('    // Et maintenant, on remonte');
    WrtLinFmt('    %s.%s = [];', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
    jsSetLocalVar('N2', JS_VAR_IDX_ND_ARRIVEE, '');
    jsWHILE(format('N2 != %s', [JS_VAR_IDX_ND_DEPART]), 'While de remontée', 4);
      WrtLinFmt('      %s.%s(N2);', [JS_THIS, JS_FUNCTION_AddIdxNoeudAtPathFound]);
      WrtLinFmt('      N2 = %s[N2];', [JS_LOCAL_ListePreds]);
    jsWEND('', 4);
  EndClassJSFunction();
end;
procedure QRedigeClassJSGrapheFunctionsPathFound();
begin
  BeginClassJSFunction(JS_THIS, JS_FUNCTION_AddIdxNoeudAtPathFound, 'IdxNoeud');
    WriteLine('    // L''algo Dijkstra construit la liste des parcourus "à l''envers"');
    WriteLine('    // aussi, l''ajout se fait en début de tableau"');
    WrtLinFmt('    %s.%s.splice(%d, %d, IdxNoeud);', [JS_THIS, JS_GRAPH_SHORTEST_PATH, 0, 0]);
    WrtLinFmt('    // %s.%s.push(IdxNoeud);', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, JS_FUNCTION_GetIdxNoeudOfPathFound, 'IdxNoeud');
    WrtLinFmt('    return %s.%s[IdxNoeud];', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, JS_FUNCTION_GetNbStationsOfPathFound, '');
    WrtLinFmt('    return %s.%s.length;', [JS_THIS, JS_GRAPH_SHORTEST_PATH]);
  EndClassJSFunction();
end;
procedure QRedigeClassJSGrapheFunctionsGetBounds();
begin
  WriteLine('  // GetBounds');
  BeginClassJSFunction(JS_THIS, 'GetXMini', '');
    WrtLinFmt('    return %s.FXMini;', [JS_THIS]);
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, 'GetYMini', '');
    WrtLinFmt('    return %s.FYMini;', [JS_THIS]);
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, 'GetXMaxi', '');
    WrtLinFmt('    return %s.FXMaxi;', [JS_THIS]);
  EndClassJSFunction();
  BeginClassJSFunction(JS_THIS, 'GetYMaxi', '');
    WrtLinFmt('    return %s.FYMaxi;', [JS_THIS]);
  EndClassJSFunction();
end;
procedure QRedigeClassJSGrapheFunctionDistanceBetweenNodes();
const FMT_GET_NOEUD = '%s.GetNoeud(%s)';
begin
  BeginClassJSFunction(JS_THIS, JS_FUNCTION_DistanceBetweenNodes, Format('%s, %s', [JS_VAR_IDX_ND_DEPART, JS_VAR_IDX_ND_ARRIVEE]));
    jsSetLocalVar('ST1', format(FMT_GET_NOEUD, [JS_THIS, JS_VAR_IDX_ND_DEPART]), '');   //   WrtLinFmt('    var ST1 = %s.GetNoeud(%s);', []);
    jsSetLocalVar('ST2', format(FMT_GET_NOEUD, [JS_THIS, JS_VAR_IDX_ND_ARRIVEE]), '');  //   WrtLinFmt('    var ST2 = %s.GetNoeud(%s);', []);
    jsSetLocalVar(JS_VARNAME_dx, 'ST2.X - ST1.X', '');
    jsSetLocalVar(JS_VARNAME_dy, 'ST2.Y - ST1.Y', '');
    jsSetLocalVar('QDist', format('%s.sqrt(%s * %s + %s * %s)', [JS_MATH_LIBRARY, JS_VARNAME_dx, JS_VARNAME_dx, JS_VARNAME_dy, JS_VARNAME_dy]), 'distance');

    WriteLine('    if (QDist < 0.01) return(0.01)');
    WriteLine('                 else return QDist; // sécurisation: aucun poids nul autorisé dans Dijkstra');
  EndClassJSFunction();
end;
procedure QRedigeFunctionListerRoadMap();
const
  QVAR_LongParcours = 'LongParcours';
  FMT_METHOD_GetNoeud = '%s.GetNoeud(%s.%s(%s))';
begin
  BeginJSFunction(JS_FUNCTION_ListerRoadMap);
    jsSetLocalVar('Nb', Format('%s.%s.length', [JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPH_SHORTEST_PATH]));
    WriteLine('   if (Nb == 0) return;');
    jsSetLocalVar(QVAR_LongParcours, 0.00);
    WrtLinFmt('   %s.options.length = Nb;', [JS_DOCUMENT_FORM_lsbRoadMap]);
    jsFOR('i', 'Nb', 0, '', 4);
      jsSetLocalVar(JS_VARNAME_CURRENT_NODE, Format(FMT_METHOD_GetNoeud,  [JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound, 'i']));
      jsIF('i >= 1', '', 6);
        jsSetLocalVar(JS_VARNAME_PREVIOUS_NODE, Format(FMT_METHOD_GetNoeud, [JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound, 'i-1']));
        jsSetLocalVar(JS_VARNAME_dx, format('%s.X - %s.X', [JS_VARNAME_CURRENT_NODE, JS_VARNAME_PREVIOUS_NODE]));
        jsSetLocalVar(JS_VARNAME_dy, format('%s.Y - %s.Y', [JS_VARNAME_CURRENT_NODE, JS_VARNAME_PREVIOUS_NODE]));
        WrtLinFmt('       %s += %s.hypot(dx, dy);', [QVAR_LongParcours, JS_MATH_LIBRARY, JS_VARNAME_dx, JS_VARNAME_dy]);
      jsENDIF('', 6);
      WrtLinFmt('     %s.options[i].value = %s.IDNoeud;', [JS_DOCUMENT_FORM_lsbRoadMap, JS_VARNAME_CURRENT_NODE]);
      WrtLinFmt('     %s.options[i].text = %s.IDNoeud;', [JS_DOCUMENT_FORM_lsbRoadMap, JS_VARNAME_CURRENT_NODE]);
    jsNEXT('i', '', 4);
    WrtLinFmt('     %s = %s.ceil(%s / 10);', [QVAR_LongParcours, JS_MATH_LIBRARY, QVAR_LongParcours]);
    WrtLinFmt('   My%s.innerHTML    = Nb.toString() + " stations (" + %s.toFixed(2) +" m)";', [JS_DOCUMENT_FORM_lbRoadmapNbPoints, QVAR_LongParcours]);
  EndJSFunction();
end;
procedure QRedigeFunctionCalcCheminMinimalBetweenTwoNodes();
begin
  BeginJSFunction(JS_FUNCTION_CalcCheminMinimalBetweenTwoNodes, 'St1, St2');
    WrtLinFmt('  %s.Dijkstra(St1, St2);  ', [JS_GRAPHE_CLASS_VAR_NAME]);
    WrtLinFmt('  %s(%s);', [JS_FUNCTION_DRAW_GRAPHE, JS_VAR_FLAG_DO_DRAW_GRAPHE]);
    WrtLinFmt('  %s();', [JS_FUNCTION_ListerRoadMap]);
  EndJSFunction();
end;
procedure QRedigeCallbackOnResizeCanvas();
const
  FMT_FLOOR = '  %s = %s.floor(0.50 * (%s + %s));';
begin
  BeginJSFunction(JS_CALLBACK_CANVAS_OnResizeCanvas);
    jsSetLocalVar('r', Format('%s.clientHeight / %s.clientWidth', [JS_GLOBAL_VAR_MyContainer, JS_GLOBAL_VAR_MyContainer]));
    WrtLinFmt('  %s = %s.floor(%s + r * (%s - %s));', [JS_GLOBAL_VAR_FYMaxi, JS_MATH_LIBRARY, JS_GLOBAL_VAR_FYMini, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]);
    WrtLinFmt(FMT_FLOOR, [JS_GLOBAL_VAR_FXCentreMap, JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]);
    WrtLinFmt(FMT_FLOOR, [JS_GLOBAL_VAR_FYCentreMap, JS_MATH_LIBRARY, JS_GLOBAL_VAR_FYMaxi, JS_GLOBAL_VAR_FYMini]);
    WrtLinFmt('  %s(%s);', [JS_FUNCTION_DRAW_GRAPHE, JS_VAR_FLAG_DO_DRAW_GRAPHE]);
  EndJSFunction();
end;
procedure QRedigeCallbackOnClickCanvas();
begin
  BeginJSFunction(JS_CALLBACK_CANVAS_OnClickCanvas);
    (*
     WrtLinFmt('  var %s = %s.GetNoeud(%d);', [JS_VARNAME_CURRENT_NODE , JS_GRAPHE_CLASS_VAR_NAME, 1]);
     WrtLinFmt('  var Nb = %s.FindIdxNoeudNearestToXY(%s.X, %s.Y);', [JS_GRAPHE_CLASS_VAR_NAME, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
     Wrtln(      ('  if (Nb >= 0)'));
     Wrtln(      ('  {'));
     WrtLinFmt('    var %s = %s.GetNoeud(%s);', ['QSt' , JS_GRAPHE_CLASS_VAR_NAME,'Nb']);
     WrtLinFmt('    window.alert("Station: " + %s.IDNoeud);', ['QSt']);
     Wrtln(      ('  }'));
     Wrtln(      ('  else'));
     Wrtln(      ('  {'));
     Wrtln(      ('    window.alert("Station introuvable");'));

     Wrtln(      ('  }'));
     WrtLinFmt('  var EWE = (Nb >= 0) ? "ID = " + %s.IDNoeud : "%s";', [JS_VARNAME_CURRENT_NODE, '']);
     //*)
  EndJSFunction();
end;
procedure QRedigeCallbackOnMouseDownCanvas();
const
  FMT_DELTA = '%s - %s';
begin
  BeginJSFunction(JS_CALLBACK_CANVAS_OnMouseDownCanvas, 'e');
    jsSetLocalVar('QX', 'e.x');
    jsSetLocalVar('QY', 'e.y');
    jsSetLocalVar('qdx', Format(FMT_DELTA, [JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]));
    jsSetLocalVar('qdy', Format(FMT_DELTA, [JS_GLOBAL_VAR_FYMaxi, JS_GLOBAL_VAR_FYMini]));
    jsSetLocalVar('rx', Format('qdx / %s.width' , [JS_GLOBAL_VAR_MyCanvas]));
    jsSetLocalVar('ry', Format('qdy / %s.height', [JS_GLOBAL_VAR_MyCanvas]));
    jsSetLocalVar('XM', Format('rx * QX + %s', [JS_GLOBAL_VAR_FXMini]));
    jsSetLocalVar('YM', Format('-ry * QY + %s', [JS_GLOBAL_VAR_FYMaxi]));
    jsSetLocalVar('Nb', Format('%s.FindIdxNoeudNearestToXY(XM, YM)', [JS_GRAPHE_CLASS_VAR_NAME]));
    jsIF('Nb >= 0', '', 4);
      WrtLinFmt('    %s = %s.GetNoeud(%s);', [JS_GLOBAL_VAR_FGlobalNoeudCourant , JS_GRAPHE_CLASS_VAR_NAME,'Nb']);
      WrtLinFmt('    %s.innerHTML = "ID = " + %s.IDNoeud;', [JS_DOCUMENT_FORM_lbCanvasMousePos, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
    jsELSE('', 4);
      WrtLinFmt('    %s.innerHTML = "%s";', [JS_DOCUMENT_FORM_lbCanvasMousePos, 'Not found']);
    jsENDIF('', 4);
  EndJSFunction();
end;
procedure QRedigeFunctionsPanZoomPlan();
const
  QFMT_FUNC_ZOOM  = '  ZoomVue(%d);';
  QFMT_IDND       = '    %s.value = %s.IDNoeud;';
  QVAR_EWE        = 'EWE';
  FMT_FACTOR_EWE  = '  %s %s= Factor * EWE;';
  FMT_DELTAXY     = '  %s += Delta%s;';
  FMT_FLOOR_DELTA = '%s.floor(0.05 * (%s - %s))';
begin
  JSSeparateur('*', 100);
  WriteLine('// Zoom et pan vue');
  BeginJSFunction('PanVue', 'DeltaX, DeltaY');
    WrtLinFmt(FMT_DELTAXY, [JS_GLOBAL_VAR_FXMini, 'X']);
    WrtLinFmt(FMT_DELTAXY, [JS_GLOBAL_VAR_FXMaxi, 'X']);
    WrtLinFmt(FMT_DELTAXY, [JS_GLOBAL_VAR_FYMini, 'Y']);
    WrtLinFmt(FMT_DELTAXY, [JS_GLOBAL_VAR_FYMaxi, 'Y']);
    WrtLinFmt('  %s();', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
  EndJSFunction();
  BeginJSFunction('ZoomVue', 'Factor');
    WrtLinFmt('  var EWE = %s.floor(0.05 * (%s - %s));', [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]);
    WrtLinFmt(FMT_FACTOR_EWE, [JS_GLOBAL_VAR_FXMini, '+']);
    WrtLinFmt(FMT_FACTOR_EWE, [JS_GLOBAL_VAR_FXMaxi, '-']);
    WrtLinFmt(FMT_FACTOR_EWE, [JS_GLOBAL_VAR_FYMini, '+']);
    WrtLinFmt(FMT_FACTOR_EWE, [JS_GLOBAL_VAR_FYMaxi, '-']);
    WrtLinFmt('  %s();', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_ResetVue, '');
    WrtLinFmt('  %s = %s.GetXMini();', [JS_GLOBAL_VAR_FXMini, JS_GRAPHE_CLASS_VAR_NAME]);
    WrtLinFmt('  %s = %s.GetXMaxi();', [JS_GLOBAL_VAR_FXMaxi, JS_GRAPHE_CLASS_VAR_NAME]);
    WrtLinFmt('  %s = %s.GetYMini();', [JS_GLOBAL_VAR_FYMini, JS_GRAPHE_CLASS_VAR_NAME]);
    WrtLinFmt('  %s = %s.GetYMaxi();', [JS_GLOBAL_VAR_FYMaxi, JS_GRAPHE_CLASS_VAR_NAME]);
    WrtLinFmt('  %s();', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_PanVueU, '');
    jsSetLocalVar(QVAR_EWE, Format(FMT_FLOOR_DELTA, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]));
    WrtLinFmt('  PanVue(0, -%s);', [QVAR_EWE]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_PanVueD, '');
    jsSetLocalVar(QVAR_EWE, Format(FMT_FLOOR_DELTA, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]));
    WrtLinFmt('  PanVue(0,  %s);', [QVAR_EWE]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_PanVueL, '');
    jsSetLocalVar(QVAR_EWE, Format(FMT_FLOOR_DELTA, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]));
    WrtLinFmt('  PanVue(%s, 0);', [QVAR_EWE]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_PanVueR, '');
    jsSetLocalVar(QVAR_EWE, Format(FMT_FLOOR_DELTA, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]));
    WrtLinFmt('  PanVue(-%s, 0);', [QVAR_EWE]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_ZoomPlus, '');
    WrtLinFmt(QFMT_FUNC_ZOOM, [1]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_ZoomMoins, '');
    WrtLinFmt(QFMT_FUNC_ZOOM, [-1]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_CentrerSurCourant, '');
    WrtLinFmt('  %s(%s.X, %s.Y);', [JS_FUNCTION_CentrerSurXY, JS_GLOBAL_VAR_FGlobalNoeudCourant, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_PickStationDep);
    WrtLinFmt(QFMT_IDND, [JS_DOCUMENT_FORM_editStationDepart, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
  EndJSFunction();
  BeginJSFunction(JS_VUE_PickStationArr);
    WrtLinFmt(QFMT_IDND, [JS_DOCUMENT_FORM_editStationArrivee, JS_GLOBAL_VAR_FGlobalNoeudCourant]);
  EndJSFunction();
end;
procedure QRedigeFunctionCentrerSurXY();
const
  FMT_FLOOR_DELTA = '%s.floor(0.5 * (%s - %s))';
begin
  BeginJSFunction(JS_FUNCTION_CentrerSurXY, 'QX, QY');
    jsSetLocalVar('QL', format(FMT_FLOOR_DELTA, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]));
    jsSetLocalVar('QH', format(FMT_FLOOR_DELTA, [JS_MATH_LIBRARY, JS_GLOBAL_VAR_FYMaxi, JS_GLOBAL_VAR_FYMini]));
    WrtLinFmt('  %s = QX - QL;', [JS_GLOBAL_VAR_FXMini]);
    WrtLinFmt('  %s = QX + QL;', [JS_GLOBAL_VAR_FXMaxi]);
    WrtLinFmt('  %s = QY - QH;', [JS_GLOBAL_VAR_FYMini]);
    WrtLinFmt('  %s = QY + QH;', [JS_GLOBAL_VAR_FYMaxi]);
    WrtLinFmt('  %s();', [JS_CALLBACK_CANVAS_OnResizeCanvas]);
  EndJSFunction();
end;
procedure QDrawShortestPath(const DoDrawEtiquettes: boolean; const LineWidth: integer; const LC: TColor; const LAlpha: byte);
begin
  WrtLinFmt('    %s.strokeStyle = "rgba(%d, %d, %d, %.0f)";',[NAMEREF_CANVAS_DC, Red(LC), Green(LC), Blue(LC), LAlpha/256.0]);
  WrtLinFmt('    %s.lineWidth   = %d;',[NAMEREF_CANVAS_DC, LineWidth]);
  WrtLinFmt('    var %s = %s.GetNoeud(%s.%s(0));',[JS_VARNAME_CURRENT_NODE, JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound]);
  WrtLinFmt('    %s.beginPath();',[NAMEREF_CANVAS_DC]);
  WrtLinFmt('    %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
  WriteLine('    PS1X = PX;');
  WriteLine('    PS1Y = PY;');
  WrtLinFmt('    %s.moveTo(PS1X, PS1Y);', [NAMEREF_CANVAS_DC]);
  jsFOR('i', 'Nb', 0, '', 4);
    WrtLinFmt('      var %s = %s.GetNoeud(%s.%s(i));', [JS_VARNAME_CURRENT_NODE, JS_GRAPHE_CLASS_VAR_NAME, JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound]);
    WrtLinFmt('      %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
    WriteLine('      PS1X = PX;');
    WriteLine('      PS1Y = PY;');
    WrtLinFmt('      %s.lineTo(PS1X, PS1Y);', [NAMEREF_CANVAS_DC]);
    if (DoDrawEtiquettes) then WrtLinFmt('      %s.fillText(%s.IDNoeud, PX + %d, PY + %d);', [NAMEREF_CANVAS_DC, JS_VARNAME_CURRENT_NODE, SIZE_STATION_MARKER + 1, SIZE_STATION_MARKER + 1]);
  jsNEXT('i');
  WrtLinFmt('    %s.stroke();', [NAMEREF_CANVAS_DC]);
end;
procedure QRedigeFunctionDrawGraphe();
const
  QFMT_DELTA = '%s - %s';
  QVAR_ETENDUE_X    = 'QEtendueX';
  QVAR_QIdxVoisin   = 'QIdxVoisin';
  QVAR_NbVoisins    = 'NbVoisins';
  QVAR_MyNodeVoisin = 'MyNodeVoisin';
  procedure QRedigeNestConvFunc();
  begin
    WriteLine('  // Fonction nested de conversion');
    BeginJSFunction(JS_FUNC_CoordsToPlan, 'QX, QY');
      jsSetLocalVar('qdx', Format(QFMT_DELTA, [JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]));
      jsSetLocalVar('qdy', Format(QFMT_DELTA, [JS_GLOBAL_VAR_FYMaxi, JS_GLOBAL_VAR_FYMini]));
      jsSetLocalVar('XM', format('QX - %s', [JS_GLOBAL_VAR_FXMini]));
      jsSetLocalVar('YM', format('QY - %s', [JS_GLOBAL_VAR_FYMini]));
      jsSetLocalVar('r' , format('%s.width / qdx', [JS_GLOBAL_VAR_MyCanvas]));
      WrtLinFmt('    PX = %s.floor(r * XM);', [JS_MATH_LIBRARY]);
      WrtLinFmt('    PY = %s.height - %s.floor(r * YM);', [JS_GLOBAL_VAR_MyCanvas, JS_MATH_LIBRARY]);
    EndJSFunction();
    WriteLine('  // Fin fonctions nested')
  end;
begin
  BeginJSFunction(JS_FUNCTION_DRAW_GRAPHE, 'DoDrawLabelStations');
    WrtLinFmt('  var Nb = %s.GetNbNoeuds();', [JS_GRAPHE_CLASS_VAR_NAME]);
    WriteLine('  if (Nb === 0) return;');
    WrtLinFmt('  %s.width = %s.clientWidth;', [JS_GLOBAL_VAR_MyCanvas, JS_GLOBAL_VAR_MyContainer]);
    WrtLinFmt('  %s.height = %s.clientHeight;', [JS_GLOBAL_VAR_MyCanvas, JS_GLOBAL_VAR_MyContainer]);
    WrtLinFmt('  var %s = %s.getContext("%s");', [NAMEREF_CANVAS_DC, JS_GLOBAL_VAR_MyCanvas, '2d']);
    WrtLinFmt('  %s.strokeStyle = "rgba(%d, %d, %d, %.0f)";', [NAMEREF_CANVAS_DC, Red(CouleurCenterlines), Green(CouleurCenterlines), Blue(CouleurCenterlines), 1.0]);
    WrtLinFmt('  %s.lineWidth   = "%dpx";', [NAMEREF_CANVAS_DC, QLineWidth]);
    WrtLinFmt('  %s.fillStyle   = "rgba(200, 0, 0, 1)";', [NAMEREF_CANVAS_DC]);
    WrtLinFmt('  %s.font        = "%dpx %s";', [NAMEREF_CANVAS_DC, 12, 'sans-serif']);
    WriteLine('  // variables semi-globales');
    jsSetLocalVar('PX', 0);
    jsSetLocalVar('PY', 0);
    jsSetLocalVar('PS1X', 0);
    jsSetLocalVar('PS1Y', 0);
    jsSetLocalVar('PS2X', 0);
    jsSetLocalVar('PS2Y', 0);
    QRedigeNestConvFunc();
    WriteLine('  // Traçage du graphe');
    jsSetLocalVar(QVAR_ETENDUE_X, Format(QFMT_DELTA,[JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FXMini]), 'Etendue du réseau en X');
    jsFOR('i', 'Nb', 0, '', 4);
      WrtLinFmt('    %s = %s.GetNoeud(i);', [JS_VARNAME_CURRENT_NODE, JS_GRAPHE_CLASS_VAR_NAME]);
      WrtLinFmt('    %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
      WriteLine('    PS1X = PX;');
      WriteLine('    PS1Y = PY;');
      WrtLinFmt('    %s.fillRect(PX - %d, PY - %d, %d, %d);', [NAMEREF_CANVAS_DC, SIZE_STATION_MARKER, SIZE_STATION_MARKER, 2 * SIZE_STATION_MARKER, 2 * SIZE_STATION_MARKER]);
      WrtLinFmt('    if (%s < %.0f) %s.fillText(%s.IDNoeud, PX + %d, PY + %d);', [QVAR_ETENDUE_X, 10 * ETENDUE_X_MAX_FOR_DISP_LBL_STATIONS, NAMEREF_CANVAS_DC, JS_VARNAME_CURRENT_NODE, SIZE_STATION_MARKER + 1, SIZE_STATION_MARKER + 1]);
      jsSetLocalVar(QVAR_NbVoisins, Format('%s.ListeVoisins.length', [JS_VARNAME_CURRENT_NODE]));
      jsIF(Format('%s > 0', [QVAR_NbVoisins]), '', 4);
        jsFOR('k', QVAR_NbVoisins, 0, '', 6);                 //WriteLine('      for (var k = 0; k < NbVoisins; k++)');
          jsSetLocalVar(QVAR_QIdxVoisin, Format('%s.ListeVoisins[%s]', [JS_VARNAME_CURRENT_NODE, 'k']));
          jsSetLocalVar(QVAR_MyNodeVoisin, Format('%s.GetNoeud(%s)', [JS_GRAPHE_CLASS_VAR_NAME, QVAR_QIdxVoisin]));
          WrtLinFmt('        %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, QVAR_MyNodeVoisin, QVAR_MyNodeVoisin]);
          WrtLinFmt('        %s.beginPath();'         , [NAMEREF_CANVAS_DC]);
          WrtLinFmt('        %s.moveTo(PS1X, PS1Y);'  , [NAMEREF_CANVAS_DC]);
          WrtLinFmt('        %s.lineTo(PX , PY);'     , [NAMEREF_CANVAS_DC]);
          WrtLinFmt('        %s.stroke();'            , [NAMEREF_CANVAS_DC]);
        jsNEXT('k', '', 6);
      jsENDIF('', 4);
    jsNEXT('i');
    WriteLine('  // On trace le réticule'); //
    WrtLinFmt('  %s.strokeStyle = "rgba(%d, %d, %d, %.0f)";', [NAMEREF_CANVAS_DC, Red(clGray), Green(clGray), Blue(clGray), 192/256.0]);
    WrtLinFmt('  %s.lineWidth   = "%dpx";', [NAMEREF_CANVAS_DC, QLineWidth]);
       JSTraceLigne(JS_GLOBAL_VAR_FXMini, JS_GLOBAL_VAR_FYCentreMap, JS_GLOBAL_VAR_FXMaxi, JS_GLOBAL_VAR_FYCentreMap);
    JSTraceLigne(JS_GLOBAL_VAR_FXCentreMap, JS_GLOBAL_VAR_FYMini, JS_GLOBAL_VAR_FXCentreMap, JS_GLOBAL_VAR_FYMaxi);
    WriteLine('  // Tracé du plus court chemin: ');
    WrtLinFmt('  Nb = %s.%s();', [JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetNbStationsOfPathFound]);
    jsIF('Nb > 2', '', 4);
      QDrawShortestPath(false, 7, clRed      , 128);
      QDrawShortestPath(false, 4, clYellow   , 192);
      QDrawShortestPath(true , 1, clMaroon   , 255);
    jsENDIF('', 4);
    WriteLine('  // et on met en évidence le dernier point trouvé');
    jsIF(Format('%s >= 0', [JS_GLOBAL_VAR_FLastIdxFound]), '', 4);
      WrtLinFmt('    %s.font        = "%dpx %s";', [NAMEREF_CANVAS_DC, 16, 'sans-serif']);
      WrtLinFmt('    %s = %s.GetNoeud(%s);', [JS_VARNAME_CURRENT_NODE, JS_GRAPHE_CLASS_VAR_NAME, JS_GLOBAL_VAR_FLastIdxFound]);
      WrtLinFmt('    %s(%s.X, %s.Y);', [JS_FUNC_CoordsToPlan, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
      WrtLinFmt('    %s.fillText(%s.IDNoeud, PX + %d, PY + %d);', [NAMEREF_CANVAS_DC, JS_VARNAME_CURRENT_NODE, SIZE_STATION_MARKER + 1, SIZE_STATION_MARKER + 1]);
   jsENDIF('', 4);
  EndJSFunction();
end;
// interaction avec la page
procedure QRedigeFunctionProcOnSubmit();  // OK
begin
  BeginJSFunction(JS_DOCUMENT_FORM_btnSearch_ProcOnSubmit);
    jsSetLocalVar('EWE', format('%s.value', [JS_DOCUMENT_FORM_editFindWhat]));
    jsSetLocalVar('QIdx',format('%s.FindIdxNoeudByCle(EWE)', [JS_GRAPHE_CLASS_VAR_NAME]));
    jsIF('QIdx >= 0', '', 4);
      WrtLinFmt('    %s = QIdx;', [JS_GLOBAL_VAR_FLastIdxFound]);
      jsSetLocalVar(JS_VARNAME_CURRENT_NODE, format('%s.GetNoeud(%s)', [JS_GRAPHE_CLASS_VAR_NAME, JS_GLOBAL_VAR_FLastIdxFound]));
      WrtLinFmt('    %s(%s.X, %s.Y);', [JS_FUNCTION_CentrerSurXY, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
    jsELSE('', 4);
      WriteLine('    window.alert("Station " + EWE + " introuvable");');
    jsENDIF('', 4);
  EndJSFunction();
end;
procedure QRedigeFunctionBtnCalcShortestPathClick();
begin
  BeginJSFunction(JS_DOCUMENT_FORM_btnCalcShortestPath_ProcOnSubmit);
    JSToto('départ'  , JS_DOCUMENT_FORM_editStationDepart , 'ST1', 'IdxSt1');
    JSToto('arrivée' , JS_DOCUMENT_FORM_editStationArrivee, 'ST2', 'IdxSt2');
    WrtLinFmt('  %s(%s, %s);', [JS_FUNCTION_CalcCheminMinimalBetweenTwoNodes, 'ST1', 'ST2']);
  EndJSFunction();
end;
procedure QRedigeFunctionLsbRoadmapOnSelect();
const
  QVAR_QIdxPathPtCurr = 'QIdxPathPtCurr';
  QVAR_QIdxPathPtNext = 'QIdxPathPtNext';
  QFMT_COORDS_INNER_HTML = '   %s.innerHTML    = "%s: " + %s.%s.toFixed(2);';
  procedure MiouMiouIF(const QVar: string);
  begin
    WriteLine(Format('   if (%s === %d) return;', [QVar, -1]));
  end;
begin
  WriteLine('// Interaction avec la feuille de route');
  BeginJSFunction(JS_DOCUMENT_FORM_lsbRoadmap_ProcOnSelect, '');
    jsSetLocalVar('QIdx', Format('%s.selectedIndex', [JS_DOCUMENT_FORM_lsbRoadMap]), 'Index courant');
    jsSetLocalVar(QVAR_QIdxPathPtCurr, Format('%s.%s(%s)', [JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound, 'QIdx']), 'Recherche du point courant');
    MiouMiouIF(QVAR_QIdxPathPtCurr);
    jsSetLocalVar(JS_VARNAME_CURRENT_NODE, Format('%s.GetNoeud(%s)', [JS_GRAPHE_CLASS_VAR_NAME, QVAR_QIdxPathPtCurr]), '');
    WriteLine('   // et centrage sur le point courant');
    WrtLinFmt('   %s(%s.X, %s.Y);', [JS_FUNCTION_CentrerSurXY, JS_VARNAME_CURRENT_NODE, JS_VARNAME_CURRENT_NODE]);
    jsSetLocalVar(QVAR_QIdxPathPtNext, Format('%s.%s(%s)', [JS_GRAPHE_CLASS_VAR_NAME, JS_FUNCTION_GetIdxNoeudOfPathFound, 'QIdx + 1']), 'Recherche du point suivant');
    MiouMiouIF(QVAR_QIdxPathPtNext);
    jsSetLocalVar(JS_VARNAME_NEXT_NODE, Format('%s.GetNoeud(%s)', [JS_GRAPHE_CLASS_VAR_NAME, QVAR_QIdxPathPtNext]), '');
    jsSetLocalVar(JS_VARNAME_CapToNext, Format('%s(%s.X - %s.X, %s.Y - %s.Y, %s.Z - %s.Z, %.2f, %.2f)',
                  [JS_UTILITY_FUNC_GetBearingInc,
                   JS_VARNAME_NEXT_NODE, JS_VARNAME_CURRENT_NODE,
                   JS_VARNAME_NEXT_NODE, JS_VARNAME_CURRENT_NODE,
                   JS_VARNAME_NEXT_NODE, JS_VARNAME_CURRENT_NODE,
                   360.00, 360.00]),
                   'Cap et distance');
    WrtLinFmt('   %s.innerHTML    = "Next: " + %s.IDNoeud;'      , [JS_DOCUMENT_FORM_lbCanvasMousePos, JS_VARNAME_NEXT_NODE]);
    WrtLinFmt(QFMT_COORDS_INNER_HTML, [JS_DOCUMENT_FORM_lbDistNextPoint  , 'Dist', JS_VARNAME_CapToNext, JS_VARNAME_OUT_Dist]);
    WrtLinFmt(QFMT_COORDS_INNER_HTML, [JS_DOCUMENT_FORM_lbAzimutNextPoint, 'Cap' , JS_VARNAME_CapToNext, JS_VARNAME_OUT_Az]);
    WrtLinFmt(QFMT_COORDS_INNER_HTML, [JS_DOCUMENT_FORM_lbPenteNextPoint , 'Incl', JS_VARNAME_CapToNext, JS_VARNAME_OUT_Inc]);
  EndJSFunction();
end;
// listeners et autres
procedure QInitialiserEventsOfTactilePointer();
begin
  WriteLine('  // Gestion des mouvements de doigts sur la tablette (gestures)');
  WriteLine('  // A implanter dans Initialiser() après assignation des contrôles via GetElementByID()');
  WriteLine('  // https://developer.mozilla.org/fr/docs/Web/API/Pointer_events/gestes_pincer_zoom');

  WrtLinFmt('  %s.onpointerdown   = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerdown_handler]);
  WrtLinFmt('  %s.onpointerup     = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerup_handler]);
  WrtLinFmt('  %s.onpointercancel = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerup_handler]);
  WrtLinFmt('  %s.onpointermove   = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointermove_handler]);
  WrtLinFmt('  %s.onpointerout    = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerup_handler]);
  WrtLinFmt('  %s.onpointerleave  = %s;', [JS_GLOBAL_VAR_MyCanvas, JS_TACTILE_EVENT_pointerup_handler]);
  WriteLine('  // Fin Gestures');
end;
procedure QRedigerTactileHandlers();
const
  ARG_EVENT   = 'event';
  COUNTER_VAR = 'i';
  VAR_CURRDIFF = 'currDiff';
var
  WU: String;
begin
  WriteLine('// Callbacks des handlers de tactile');
  BeginJSFunction(JS_TACTILE_PROC_remove_event, ARG_EVENT);
    // Supprime l'événement du cache
    WU := Format('%s.length', [ARG_EVENT]);
    jsFOR(COUNTER_VAR, WU, 0, '', 4);
      jsIF(Format('%s[%s].pointerId === %s.pointerId', [JS_GLOBAL_VAR_EvtCache, COUNTER_VAR, ARG_EVENT]), '', 6);
        WrtLinFmt('       %s.splice(%s, %d);', [JS_GLOBAL_VAR_EvtCache, COUNTER_VAR, 1]);
        jsBREAK(8);
      jsENDIF('', 6);
    jsNEXT(COUNTER_VAR, '', 4);
  EndJSFunction();
  // L'événement pointerdown signale le début d'une interraction de toucher.
  // OnMouseDown
  BeginJSFunction(JS_TACTILE_EVENT_pointerdown_handler, ARG_EVENT);
    // L'événement est mis en cache pour prendre en charge les gestes à 2 doigts
    WrtLinFmt('  %s.push(%s);', [JS_GLOBAL_VAR_EvtCache, ARG_EVENT]);
    //WrtLinFmt('  console.log("%s", %s);', [JS_TACTILE_EVENT_pointerdown_handler, ARG_EVENT]);
  EndJSFunction();
  // OnMouseMove
  BeginJSFunction(JS_TACTILE_EVENT_pointermove_handler, ARG_EVENT);
    //WrtLinFmt('  console.log("%s", %s);', [JS_TACTILE_EVENT_pointermove_handler, ARG_EVENT]);
    // Trouve le pointeur en cours dans le cache et le met à jour avec cet événement
     WU := Format('%s.length', [ARG_EVENT]);
     jsFOR(COUNTER_VAR, WU, 0, '', 4);
       jsIF(Format('%s.pointerId === %s[%s].pointerId', [ARG_EVENT,  JS_GLOBAL_VAR_EvtCache, COUNTER_VAR]), '', 6);       // if (ev.pointerId == evCache[i].pointerId) {
         WrtLinFmt('     %s[%s] = %s;', [JS_GLOBAL_VAR_EvtCache, COUNTER_VAR, ARG_EVENT]);                                  //    evCache[i] = ev;
         jsBREAK(8);
       jsENDIF('', 4);
     jsNEXT(COUNTER_VAR, '', 4);
     // Si deux pointeurs sont utilisés, vérifie le geste de pincement
     jsIF(Format('%s.length === %d', [ARG_EVENT, 2]), '', 4); //  if (evCache.length == 2) {
       WrtLinFmt('      var %s = %s.abs(%s[%d].clientX - %s[%d].clientX);', [VAR_CURRDIFF, JS_MATH_LIBRARY, JS_GLOBAL_VAR_EvtCache, 0, JS_GLOBAL_VAR_EvtCache, 1]);
       jsIF(Format('%s > %d', [JS_GLOBAL_VAR_PrevDiff, 0]), '', 6);
         jsIF(Format('%s > %s', [VAR_CURRDIFF, JS_GLOBAL_VAR_PrevDiff, 0]), 'La distance entre les deux pointeurs a augmenté', 8);
           //WrtLinFmt('  console.log("%s");', ['Zoom Plus']);
         jsENDIF('', 8);
         jsIF(Format('%s < %s', [VAR_CURRDIFF, JS_GLOBAL_VAR_PrevDiff, 0]), 'La distance entre les deux pointeurs a diminué', 8);
           //WrtLinFmt('  console.log("%s");', ['Zoom Moins']);
         jsENDIF('', 8);
       jsENDIF('', 6);
     jsENDIF('', 4);
     WrtLinFmt('%s = %s; // %s', [JS_GLOBAL_VAR_PrevDiff, VAR_CURRDIFF, 'Met en cache la distance pour les événements suivants']);
  EndJSFunction();
  // OnMouseUp
  BeginJSFunction(JS_TACTILE_EVENT_pointerup_handler, ARG_EVENT);
    WrtLinFmt('  console.log("%s", %s);', [JS_TACTILE_EVENT_pointerup_handler, ARG_EVENT]);
    // Retire ce pointeur du cache et rétablit l'arrière-plan et
    WrtLinFmt('  %s(%s);', [JS_TACTILE_PROC_remove_event, ARG_EVENT]);
    // traitements
    //ev.target.style.border = "1px solid black";

    // Si le nombre de pointeurs restant est inférieur à deux, remet à zéro la différence
    WrtLinFmt('  if (%s.length < %d) %s = -1;', [JS_GLOBAL_VAR_EvtCache, 2, JS_GLOBAL_VAR_PrevDiff]);
  EndJSFunction();
end;

