unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiPanel,
  CustomShape, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiPanel1: TMultiPanel;
    procedure MultiPanel1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiPanel1Click(Sender: TObject);
begin
  showmessage('Klick');
end;

end.

