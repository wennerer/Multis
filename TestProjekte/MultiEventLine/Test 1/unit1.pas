unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Spin,
  StdCtrls, MultiPanel, MultiEventLine;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    MultiEventLine1: TMultiEventLine;
    MultiPanel1: TMultiPanel;
    SpinEdit1: TSpinEdit;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

