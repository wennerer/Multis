unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ColorBox, StdCtrls,
  MultiPanel, TextBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorBox4: TColorBox;
    MultiPanel1: TMultiPanel;
    TextBox1: TTextBox;
    TextBox2: TTextBox;
    TextBox3: TTextBox;
    TextBox4: TTextBox;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

