unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiSwitch,
  MultiButton, FlexiSwitch;


type

  { TForm1 }

  TForm1 = class(TForm)
    FlexiSwitch1: TFlexiSwitch;
    MultiButton1: TMultiButton;
    MultiSwitch1: TMultiSwitch;
    procedure FormCreate(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
  private
    aStep : integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
 inc(aStep);
 if aStep > 20 then aStep := 0;
 MultiSwitch1.FocusFrameWidth:=aStep;
 caption := inttostr(aStep);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 aStep := 5;
 caption := inttostr(aStep);
end;

end.

