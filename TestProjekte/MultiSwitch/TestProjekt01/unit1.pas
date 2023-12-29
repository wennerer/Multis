unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ColorBox, LCLIntf, MultiSwitch, MultiPanel, MultiplexSlider, MultiSeperator,
  MultiButton, FlexiSwitch;


type

  { TForm1 }

  TForm1 = class(TForm)
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    Edit1: TEdit;
    Edit2: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    FloatSpinEdit4: TFloatSpinEdit;
    Memo1: TMemo;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiPanel1: TMultiPanel;
    aMultiSwitch: TMultiSwitch;
    MultiplexSlider1: TMultiplexSlider;
    MultiplexSlider2: TMultiplexSlider;
    MultiplexSlider3: TMultiplexSlider;
    MultiSeperator1: TMultiSeperator;
    MultiSwitch1: TMultiSwitch;
    MultiSwitch10: TMultiSwitch;
    MultiSwitch11: TMultiSwitch;
    MultiSwitch12: TMultiSwitch;
    MultiSwitch13: TMultiSwitch;
    MultiSwitch2: TMultiSwitch;
    MultiSwitch3: TMultiSwitch;
    MultiSwitch4: TMultiSwitch;
    MultiSwitch5: TMultiSwitch;
    MultiSwitch6: TMultiSwitch;
    MultiSwitch7: TMultiSwitch;
    MultiSwitch8: TMultiSwitch;
    MultiSwitch9: TMultiSwitch;
    OpenDialog1: TOpenDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    StaticText1: TStaticText;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText12: TStaticText;
    StaticText13: TStaticText;
    StaticText14: TStaticText;
    StaticText15: TStaticText;
    StaticText16: TStaticText;
    StaticText17: TStaticText;
    StaticText18: TStaticText;
    StaticText19: TStaticText;
    StaticText2: TStaticText;
    StaticText20: TStaticText;
    StaticText21: TStaticText;
    StaticText22: TStaticText;
    StaticText23: TStaticText;
    StaticText24: TStaticText;
    StaticText25: TStaticText;
    StaticText26: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    procedure ColorBox1Change(Sender: TObject);
    procedure ColorBox2Change(Sender: TObject);
    procedure ColorBox3Change(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure ColorButton2ColorChanged(Sender: TObject);
    procedure ColorButton3ColorChanged(Sender: TObject);
    procedure ColorButton4ColorChanged(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FloatSpinEdit1Change(Sender: TObject);
    procedure FloatSpinEdit2Change(Sender: TObject);
    procedure FloatSpinEdit3Change(Sender: TObject);
    procedure FloatSpinEdit4Change(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiButton2Click(Sender: TObject);
    procedure MultiplexSlider1Change(const aValue: integer);
    procedure MultiplexSlider2Change(const aValue: integer);
    procedure MultiplexSlider3Change(const aValue: integer);
    procedure MultiSwitch1Change({%H-}Sender: TObject);
    procedure MultiSwitch2Direction({%H-}Sender: TObject; aDirection: TSwDirection;aLeft,aRight:boolean);
    procedure MultiSwitch3Left({%H-}Sender: TObject);
    procedure MultiSwitch3Right({%H-}Sender: TObject);
    procedure MultiSwitch4Direction(Sender: TObject; aDirection: TSwDirection;aLeft,aRight:boolean);
    procedure MultiSwitch5Direction(Sender: TObject; aDirection: TSwDirection;aLeft,aRight:boolean);
    procedure MultiSwitch6Left(Sender: TObject; aSide: boolean);
    procedure MultiSwitch6Right(Sender: TObject; aSide: boolean);
    procedure MultiSwitch7Direction(Sender: TObject; aDirection: TSwDirection;
      aLeft, aRight: boolean);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);


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

procedure TForm1.FloatSpinEdit1Change(Sender: TObject);
begin
 aMultiSwitch.ImgSizeFactor:=FloatSpinEdit1.Value;
end;

procedure TForm1.FloatSpinEdit2Change(Sender: TObject);
begin
 aMultiSwitch.EnabledBlendFaktor:= FloatSpinEdit2.Value;
end;

procedure TForm1.FloatSpinEdit3Change(Sender: TObject);
begin
 aMultiSwitch.HoverBlendFaktor:=FloatSpinEdit3.Value;
end;

procedure TForm1.FloatSpinEdit4Change(Sender: TObject);
begin
 aMultiSwitch.FocusedBlendFaktor:=FloatSpinEdit4.Value;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
 aMultiSwitch.LeftCaption:=Edit1.Text;
end;

procedure TForm1.ColorButton1ColorChanged(Sender: TObject);
begin
 aMultiSwitch.LeftBgrdColor:= ColorButton1.ButtonColor;
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
 aMultiSwitch.BorderColor:= ColorBox1.Selected;
end;

procedure TForm1.ColorBox2Change(Sender: TObject);
begin
 aMultiSwitch.ButtonColor:= ColorBox2.Selected;
end;

procedure TForm1.ColorBox3Change(Sender: TObject);
begin
 aMultiSwitch.HoverColor:= ColorBox3.Selected;
end;

procedure TForm1.ColorButton2ColorChanged(Sender: TObject);
begin
 aMultiSwitch.RightBgrdColor:= ColorButton2.ButtonColor;
end;

procedure TForm1.ColorButton3ColorChanged(Sender: TObject);
begin
 aMultiSwitch.Font.Color:= ColorButton3.ButtonColor;
end;

procedure TForm1.ColorButton4ColorChanged(Sender: TObject);
begin
 aMultiSwitch.FocusColor:=ColorButton4.ButtonColor;
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
 aMultiSwitch.RightCaption:=Edit2.Text;
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

procedure TForm1.MultiButton2Click(Sender: TObject);
begin
 OpenURL('https://www.lazarusforum.de/viewtopic.php?p=128092#p128092');
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

procedure TForm1.MultiSwitch2Direction(Sender: TObject; aDirection: TSwDirection;aLeft,aRight:boolean);
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

procedure TForm1.MultiSwitch4Direction(Sender: TObject; aDirection: TSwDirection;aLeft,aRight:boolean);
begin
 aMultiSwitch.BestTextHeight:=aRight;
end;

procedure TForm1.MultiSwitch5Direction(Sender: TObject; aDirection: TSwDirection;aLeft,aRight:boolean);
begin
 aMultiSwitch.Enabled:=aRight;
end;

procedure TForm1.MultiSwitch6Left(Sender: TObject; aSide: boolean);
begin
 aMultiSwitch.FocusFrameOn:= not aSide;
end;

procedure TForm1.MultiSwitch6Right(Sender: TObject; aSide: boolean);
begin
 aMultiSwitch.FocusFrameOn:=aSide;
end;

procedure TForm1.MultiSwitch7Direction(Sender: TObject;
  aDirection: TSwDirection; aLeft, aRight: boolean);
begin
 aMultiSwitch.ForegroundFocusOn:= not aLeft;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
 aMultiSwitch.LeftImageIndex:=SpinEdit1.Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
 aMultiSwitch.RightImageIndex:=SpinEdit2.Value;
end;

procedure TForm1.SpinEdit3Change(Sender: TObject);
begin
 aMultiSwitch.Font.Height:= SpinEdit3.Value;
end;

procedure TForm1.SpinEdit4Change(Sender: TObject);
begin
 aMultiSwitch.FocusAlphaBValue:=SpinEdit4.Value;
end;






end.

