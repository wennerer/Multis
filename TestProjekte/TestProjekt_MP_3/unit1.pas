unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MultiPanel,
  MultiButton, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton1: TMultiButton;
    MultiPanel1: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    procedure MultiButton1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
  MultiPanel1.Appear:=true;
end;

end.

