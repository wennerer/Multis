unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MultiSeperator, MultiButton, MultiButtonStyleManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiSeperator1: TMultiSeperator;
    procedure Button1ControlBorderSpacingChange(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiButton2Click(Sender: TObject);
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
 //MultiSeperator1.BorderSpacing.InnerBorder:=5;
 (*if MultiSeperator2.Orientation = mspVertical then MultiSeperator2.Orientation := mspHorizontal
 else MultiSeperator2.Orientation := mspVertical; *)
end;

procedure TForm1.Button1ControlBorderSpacingChange(Sender: TObject);
begin

end;

procedure TForm1.MultiButton2Click(Sender: TObject);
begin
 //MultiSeperator1.BorderSpacing.Around:=10;
end;

end.

