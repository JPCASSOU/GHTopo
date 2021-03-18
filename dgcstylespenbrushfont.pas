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
    Couleur   : TColor;
    Opacity   : byte;
    WidthInPX : integer;
    WidthInMM : double;
    Style     : TPenStyle;
end;
type TDGCBrush = class
  public
    Couleur : TColor;
    Opacity : byte;
    Style   : TBrushStyle;
end;
type TDGCFont = class
  public
    FontName: string;
    Couleur: TColor;
    Opacity: byte;
    Height : Integer;
    Style  : TFontStyles;
end;

implementation
uses
  DGCDummyUnit; // pour éviter le bug de 'Fin de code source non trouvée

end.

