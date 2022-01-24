unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, MultiPanel,
  MultiButton, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton2: TMultiButton;
    MultiPanel1: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    ScrollBox1: TScrollBox;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

