unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiPanel,
  MultiEventLine, MultiRadioGroup;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiEventLine1: TMultiEventLine;
    MultiPanel1: TMultiPanel;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

