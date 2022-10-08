unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiplexSlider,
  MultiButton, MultiButtonStyleManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1    : TImageList;
    MultiButton1  : TMultiButton;
    MultiButton2  : TMultiButton;
    MultiButton3  : TMultiButton;
    MultiButton4  : TMultiButton;
    MultiButton5  : TMultiButton;
    MultiButton6  : TMultiButton;
    MultiButton7  : TMultiButton;
    MultiButton8  : TMultiButton;
    MultiButton9  : TMultiButton;
    MultiButton10 : TMultiButton;
    MultiButton11 : TMultiButton;
    MultiButton12 : TMultiButton;
    MultiButton13 : TMultiButton;
    MultiButton14 : TMultiButton;
    MultiButton15 : TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiButtonStyleManager2: TMultiButtonStyleManager;
    MultiplexSlider1: TMultiplexSlider;
    procedure FormCreate(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiButton2Click(Sender: TObject);
    procedure MultiButton3Click(Sender: TObject);
    procedure MultiButton4Click(Sender: TObject);
    procedure MultiButton5Click(Sender: TObject);
    procedure MultiButton6Click(Sender: TObject);
    procedure MultiButton7Click(Sender: TObject);
    procedure MultiButton8Click(Sender: TObject);
    procedure MultiButton9Click(Sender: TObject);
    procedure MultiButton10Click(Sender: TObject);
    procedure MultiButton11Click(Sender: TObject);
    procedure MultiButton12Click(Sender: TObject);
    procedure MultiButton13Click(Sender: TObject);
    procedure MultiButton14Click(Sender: TObject);
    procedure MultiButton15Click(Sender: TObject);
    procedure CheckTheState;
  private
    z : array [0..5] of integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
 z[5]:=1;
 CheckTheState;
end;

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
 MultiplexSlider1.TextSettings.Position:= poNone;
 CheckTheState;
end;

procedure TForm1.MultiButton2Click(Sender: TObject);
begin
 MultiplexSlider1.TextSettings.Position:= poBottom;
 CheckTheState;
end;

procedure TForm1.MultiButton3Click(Sender: TObject);
begin
 MultiplexSlider1.TextSettings.Position:= poTop;
 CheckTheState;
end;

procedure TForm1.MultiButton4Click(Sender: TObject);
begin
 MultiplexSlider1.TextSettings.Position:= poLeft;
 CheckTheState;
end;

procedure TForm1.MultiButton5Click(Sender: TObject);
begin
 MultiplexSlider1.TextSettings.Position:= poRight;
 CheckTheState;
end;

procedure TForm1.MultiButton6Click(Sender: TObject);
begin
 if MultiplexSlider1.Orientation = msoHorizontal then
  MultiplexSlider1.Orientation := msoVertical
 else
  MultiplexSlider1.Orientation := msoHorizontal;
 CheckTheState;
end;

procedure TForm1.MultiButton7Click(Sender: TObject);
begin
 if MultiplexSlider1.AutoSize then
  MultiplexSlider1.AutoSize := false
 else
  MultiplexSlider1.AutoSize := true;
 CheckTheState;
end;

procedure TForm1.MultiButton8Click(Sender: TObject);
begin
 Case z[2] of
  0: MultiplexSlider1.FocusFrameWidth :=  0;
  1: MultiplexSlider1.FocusFrameWidth :=  5;
  2: MultiplexSlider1.FocusFrameWidth := 10;
  3: MultiplexSlider1.FocusFrameWidth := 15;
 end;
 inc(z[2]); if z[2] > 3 then z[2]:=0;
 CheckTheState;
end;

procedure TForm1.MultiButton9Click(Sender: TObject);
begin
 if z[3] <> 0 then MultiplexSlider1.BorderColor:= clPurple else MultiplexSlider1.BorderColor := clNone;
 Case z[3] of
  0: MultiplexSlider1.BorderWidth :=  0;
  1: MultiplexSlider1.BorderWidth :=  1;
  2: MultiplexSlider1.BorderWidth :=  2;
  3: MultiplexSlider1.BorderWidth :=  3;
  4: MultiplexSlider1.BorderWidth :=  4;
  5: MultiplexSlider1.BorderWidth :=  5;
 end;
 inc(z[3]); if z[3] > 5 then z[3]:=0;
 CheckTheState;
end;

procedure TForm1.MultiButton10Click(Sender: TObject);
begin
 if z[0] <> 0 then MultiplexSlider1.TextSettings.BorderColor:= clLime
  else MultiplexSlider1.TextSettings.BorderColor := clNone;
 Case z[0] of
  0: MultiplexSlider1.TextSettings.BorderWidth :=  0;
  1: MultiplexSlider1.TextSettings.BorderWidth :=  1;
  2: MultiplexSlider1.TextSettings.BorderWidth :=  2;
  3: MultiplexSlider1.TextSettings.BorderWidth :=  3;
  4: MultiplexSlider1.TextSettings.BorderWidth :=  4;
  5: MultiplexSlider1.TextSettings.BorderWidth :=  5;
 end;
 inc(z[0]); if z[0] > 5 then z[0]:=0;
 CheckTheState;
end;

procedure TForm1.MultiButton11Click(Sender: TObject);
var i : integer;
begin
 if (MultiplexSlider1.Orientation = msoHorizontal) or
    (ord(MultiplexSlider1.TextSettings.Position) < 3)  then
  begin
   showmessage('AutoSize only active with msoVertical and poLeft or poRight');
   exit;
  end;
 Case z[1] of
  0: i :=   900;
  1: i :=  2700;
  2: i :=     0;
 end;
 MultiplexSlider1.TextSettings.Font.Orientation := i;
 inc(z[1]); if z[1] > 2 then z[1]:=0;
 CheckTheState;
end;

procedure TForm1.MultiButton12Click(Sender: TObject);
begin
 if (MultiplexSlider1.Orientation = msoHorizontal) or
    (ord(MultiplexSlider1.TextSettings.Position) < 3)  then
  begin
   showmessage('AutoSize only active with msoVertical and poLeft or poRight');
   exit;
  end;
 if MultiplexSlider1.TextSettings.CaptionBelow then MultiplexSlider1.TextSettings.CaptionBelow := false
 else MultiplexSlider1.TextSettings.CaptionBelow := true;
 if MultiplexSlider1.TextSettings.CaptionBelow then
   MultiplexSlider1.TextSettings.Font.Orientation :=0;
 z[1]:=0;
 CheckTheState;
end;

procedure TForm1.MultiButton13Click(Sender: TObject);
begin
 Case z[4] of
  0: MultiplexSlider1.TextSettings.PostCaption := 'and this is a very long text!!!';
  1: MultiplexSlider1.TextSettings.PostCaption := '';
 end;
 inc(z[4]);if z[4] > 1 then z[4]:=0;
 CheckTheState;
end;

procedure TForm1.MultiButton14Click(Sender: TObject);
begin
 Case z[5] of
  0: MultiplexSlider1.TextSettings.Font.Height:=  0;
  1: MultiplexSlider1.TextSettings.Font.Height:= 10;
  2: MultiplexSlider1.TextSettings.Font.Height:= 20;
  3: MultiplexSlider1.TextSettings.Font.Height:= 30;
 end;
 inc(z[5]);if z[5] > 3 then z[5]:=0;
 CheckTheState;
end;

procedure TForm1.MultiButton15Click(Sender: TObject);
var lv : integer;
begin
 MultiplexSlider1.AutoSize:=false;
 MultiplexSlider1.Orientation := msoHorizontal;
 MultiplexSlider1.TextSettings.Position:= poNone;
 MultiplexSlider1.FocusFrameWidth:=5;
 MultiplexSlider1.BorderWidth:=1;
 MultiplexSlider1.TextSettings.BorderWidth:=1;
 MultiplexSlider1.TextSettings.Font.Height:=0;
 MultiplexSlider1.TextSettings.Font.Orientation:=0;
 MultiplexSlider1.TextSettings.PostCaption:='';
 MultiplexSlider1.TextSettings.CaptionBelow:= false;
 MultiplexSlider1.SetBounds(50,25,250,50);
 MultiplexSlider1.AutoSize:= true;

 for lv:= 0 to 5 do z[lv]:=0;
 z[5]:=1;
 CheckTheState;
end;

procedure TForm1.CheckTheState;
var i:integer;
begin

 for i:=0 to pred(ComponentCount) do
  begin
   if (Components[i] is TMultiButton) then
    begin
     if (Components[i] as TMultiButton).Tag = 6 then
      begin
       if MultiplexSlider1.Orientation = msoHorizontal then
        (Components[i] as TMultiButton).MessageButton.Caption:= 'H'
       else (Components[i] as TMultiButton).MessageButton.Caption:='V';
      end;
     if (Components[i] as TMultiButton).Tag = 7 then
      begin
       if MultiplexSlider1.AutoSize then
        (Components[i] as TMultiButton).MessageButton.ImageIndex := 0
       else (Components[i] as TMultiButton).MessageButton.ImageIndex := 1;
      end;
     if (Components[i] as TMultiButton).Tag = 8 then
      (Components[i] as TMultiButton).MessageButton.Caption:= inttostr(MultiplexSlider1.FocusFrameWidth);
     if (Components[i] as TMultiButton).Tag = 9 then
      (Components[i] as TMultiButton).MessageButton.Caption:= inttostr(MultiplexSlider1.BorderWidth);
     if (Components[i] as TMultiButton).Tag = 10 then
      (Components[i] as TMultiButton).MessageButton.Caption:= inttostr(MultiplexSlider1.TextSettings.BorderWidth);
     if (Components[i] as TMultiButton).Tag = 11 then
      (Components[i] as TMultiButton).MessageButton.Caption:= inttostr(MultiplexSlider1.TextSettings.Font.Orientation);
     if (Components[i] as TMultiButton).Tag = 12 then
      begin
       if MultiplexSlider1.TextSettings.CaptionBelow then
        (Components[i] as TMultiButton).MessageButton.ImageIndex := 0
       else (Components[i] as TMultiButton).MessageButton.ImageIndex := 1;
      end;
     if (Components[i] as TMultiButton).Tag = 13 then
      begin
       if MultiplexSlider1.TextSettings.PostCaption ='' then
        (Components[i] as TMultiButton).MessageButton.ImageIndex := 1
       else (Components[i] as TMultiButton).MessageButton.ImageIndex := 0;
      end;
     if (Components[i] as TMultiButton).Tag = 14 then
      (Components[i] as TMultiButton).MessageButton.Caption:= inttostr(MultiplexSlider1.TextSettings.Font.Height);
    end;
  end;
end;

end.

