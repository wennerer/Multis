unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiPanel,
  CustomShape, MultiplexSlider, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton1: TMultiButton;
    MultiPanel1: TMultiPanel;
    procedure MultiButton1Click(Sender: TObject);
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

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
  showmessage('?');
end;

end.

