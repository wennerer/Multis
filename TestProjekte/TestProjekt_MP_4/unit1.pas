unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  MultiPanel, MultiButtonStyleManager, MultiplexSlider, MultiButton,
  CustomShape;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton1: TMultiButton;
    MultiPanel1: TMultiPanel;
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiPanel1Click(Sender: TObject);
    procedure MultiPanel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MultiPanel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MultiPanel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiPanel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TForm1.MultiPanel1Click(Sender: TObject);
begin
  showmessage('');
end;

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
 if MultiPanel1.Visible then MultiPanel1.Disappear:=true else MultiPanel1.Appear:=true;
end;

procedure TForm1.MultiPanel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TForm1.MultiPanel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

end.

