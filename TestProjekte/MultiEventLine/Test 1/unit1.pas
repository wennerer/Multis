unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  MultiPanel, MultiEventLine, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
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

