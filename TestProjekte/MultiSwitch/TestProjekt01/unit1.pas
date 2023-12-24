unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  MultiSwitch, MultiPanel, MultiplexSlider, MultiSeperator, MultiButton;


type

  { TForm1 }

  TForm1 = class(TForm)
    FloatSpinEdit1: TFloatSpinEdit;
    MultiButton1: TMultiButton;
    MultiPanel1: TMultiPanel;
    aMultiSwitch: TMultiSwitch;
    MultiplexSlider1: TMultiplexSlider;
    MultiplexSlider2: TMultiplexSlider;
    MultiplexSlider3: TMultiplexSlider;
    MultiSeperator1: TMultiSeperator;
    MultiSwitch1: TMultiSwitch;
    MultiSwitch2: TMultiSwitch;
    MultiSwitch3: TMultiSwitch;
    OpenDialog1: TOpenDialog;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    procedure FloatSpinEdit1Change(Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiplexSlider1Change(const aValue: integer);
    procedure MultiplexSlider2Change(const aValue: integer);
    procedure MultiplexSlider3Change(const aValue: integer);
    procedure MultiSwitch1Change({%H-}Sender: TObject);
    procedure MultiSwitch2Direction({%H-}Sender: TObject; aDirection: TSwDirection);
    procedure MultiSwitch3Left({%H-}Sender: TObject);
    procedure MultiSwitch3Right({%H-}Sender: TObject);


  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.MultiplexSlider1Change(const aValue: integer);
begin
 aMultiSwitch.Width:=aValue;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FloatSpinEdit1Change(Sender: TObject);
begin
 aMultiSwitch.ImgSizeFactor:=FloatSpinEdit1.Value;
end;

procedure TForm1.MultiButton1Click(Sender: TObject);
var img1,img2 : string;
begin
 OpenDialog1 := TOpenDialog.Create(self);
 try
  OpenDialog1.Title  := 'Load left image';
  OpenDialog1.Filter := 'Image|*.png|';
  if OpenDialog1.Execute then
   img1 := OpenDialog1.FileName;

  OpenDialog1.Title  := 'Load right image';
  if OpenDialog1.Execute then
   img2 := OpenDialog1.FileName;

  aMultiSwitch.LoadImagesfromFile(img1,img2);
 finally
  OpenDialog1.Free;
 end;
end;

procedure TForm1.MultiplexSlider2Change(const aValue: integer);
begin
 aMultiSwitch.FocusFrameWidth:=aValue;
end;

procedure TForm1.MultiplexSlider3Change(const aValue: integer);
begin
 aMultiSwitch.Speed:=aValue;
end;

procedure TForm1.MultiSwitch1Change(Sender: TObject);
begin
 if aMultiSwitch.Roll then aMultiSwitch.Roll := false
 else aMultiSwitch.Roll := true;
end;

procedure TForm1.MultiSwitch2Direction(Sender: TObject; aDirection: TSwDirection);
begin
 aMultiSwitch.Direction:= aDirection;
end;

procedure TForm1.MultiSwitch3Left(Sender: TObject);
begin
 aMultiSwitch.SwitchMode:= msClick;
end;

procedure TForm1.MultiSwitch3Right(Sender: TObject);
begin
 aMultiSwitch.SwitchMode:= msSlide;
end;






end.

