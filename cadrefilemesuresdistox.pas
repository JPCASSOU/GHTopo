unit CadreFileMesuresDistoX;

{$mode objfpc}{$H+}

interface

uses
  ToporobotClasses2012,
  Classes, SysUtils, FileUtil, Forms, Controls;

type

  { TCdrFileMesuresDistoX }

  TCdrFileMesuresDistoX = class(TFrame)
  private
    { private declarations }
  public
    { public declarations }
    function Initialiser(const FD: TToporobotStructure2012): boolean;
  end;

implementation

{$R *.lfm}

{ TCdrFileMesuresDistoX }

function TCdrFileMesuresDistoX.Initialiser(const FD: TToporobotStructure2012): boolean;
begin

end;

end.

