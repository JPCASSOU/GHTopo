unit DGCStylesPenBrushFont;
// Définition des classes pour les crayons, brosses et fontes

{$mode delphiunicode}

interface

uses
  DGCTypes,
  DGCUtilityFunctions,
  Classes, SysUtils, Graphics;
type TDGCPen = class
  public
    Couleur   : TDGCColor;
    WidthInPX : integer;
    WidthInMM : double;
    Style     : TPenStyle;
end;
type TDGCBrush = class
  public
    Couleur : TDGCColor;
    Style   : TBrushStyle;
end;
type TDGCFont = class
  public
    FontName: string;
    Couleur: TDGCColor;
    Height : Integer;
    Style  : TFontStyles;
end;

implementation
uses
  DGCDummyUnit; // pour éviter le bug de 'Fin de code source non trouvée

end.

