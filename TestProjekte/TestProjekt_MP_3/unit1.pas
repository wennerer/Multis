unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MultiPanel, MultiButton, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiButton2Click(Sender: TObject);
    procedure MultiButton3Click(Sender: TObject);
    procedure MultiButton4Click(Sender: TObject);
    procedure MultiplexSlider1Change(const aValue: integer);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  //MultiPanel1.ParentAsBkgrd:=false;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 MultiPanel2.Appear:=true;
end;

procedure TForm1.MultiButton2Click(Sender: TObject);
begin
  MultiPanel1.Disappear:=true;
end;

procedure TForm1.MultiButton3Click(Sender: TObject);
begin
  MultiPanel2.Disappear:=true;
end;

procedure TForm1.MultiButton4Click(Sender: TObject);
begin
 if MultiPanel1.Visible then MultiPanel1.Visible:=false else MultiPanel1.Visible:=true;
end;

procedure TForm1.MultiplexSlider1Change(const aValue: integer);
begin
  Panel1.Caption:=floattostr(aValue / 1000);
  MultiPanel1.AnimationSpeed:=aValue / 1000;
  MultiPanel2.AnimationSpeed:=aValue / 1000;
end;

end.

