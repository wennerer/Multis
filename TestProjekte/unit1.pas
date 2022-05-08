unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLProc,
  MultiPanel, MultiplexSlider, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    MultiPanel1: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
begin
 canvas.Pen.Color:=cllime;
 canvas.Pen.Width:=5;
 canvas.Line(0,0,width,height);
 //DebugLn('FormPaint');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  MultiPanel1.Appear:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //MultiPanel1.Visible:=false;
  MultiPanel1.Disappear:=true;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if MultiPanel1.Visible then MultiPanel1.Visible:=false else MultiPanel1.Visible:=true;
end;

procedure TForm1.FormShow(Sender: TObject);
begin

end;

end.

