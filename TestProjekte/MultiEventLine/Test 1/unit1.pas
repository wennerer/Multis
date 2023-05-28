unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiPanel,
  MultiEventLine, MultiRadioGroup, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton1: TMultiButton;
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

