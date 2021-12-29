unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MultiButton1: TMultiButton;
    MultiButton10: TMultiButton;
    MultiButton11: TMultiButton;
    MultiButton12: TMultiButton;
    MultiButton13: TMultiButton;
    MultiButton14: TMultiButton;
    MultiButton15: TMultiButton;
    MultiButton16: TMultiButton;
    MultiButton17: TMultiButton;
    MultiButton18: TMultiButton;
    MultiButton19: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton20: TMultiButton;
    MultiButton21: TMultiButton;
    MultiButton22: TMultiButton;
    MultiButton23: TMultiButton;
    MultiButton24: TMultiButton;
    MultiButton25: TMultiButton;
    MultiButton26: TMultiButton;
    MultiButton27: TMultiButton;
    MultiButton28: TMultiButton;
    MultiButton29: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton30: TMultiButton;
    MultiButton31: TMultiButton;
    MultiButton32: TMultiButton;
    MultiButton33: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButton7: TMultiButton;
    MultiButton8: TMultiButton;
    MultiButton9: TMultiButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    RadioGroup1: TRadioGroup;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    procedure MenuItem2Click(Sender: TObject);
    procedure MultiButton15Click(Sender: TObject);
    procedure MultiButton16Click(Sender: TObject);
    procedure MultiButton17Click(Sender: TObject);
    procedure MessageButtonClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
 if Sender = MenuItem2 then
  begin
   Panel3.Visible:= false;
   Panel4.Visible:= true;
  end;
 if Sender = MenuItem1 then
  begin
   Panel4.Visible:= false;
   Panel3.Visible:= true;
  end;
end;

procedure TForm1.MultiButton15Click(Sender: TObject);
begin
 Panel5.Visible:= false;
end;

procedure TForm1.MultiButton16Click(Sender: TObject);
begin
 Panel6.Visible:= false;
end;

procedure TForm1.MultiButton17Click(Sender: TObject);
begin
 Panel6.Visible:= false;
 Panel5.Visible:= true;
 MultiButton15.Caption:= (Sender as TMultiButton).Caption;
end;

procedure TForm1.MessageButtonClick(Sender: TObject);
begin
 Panel5.Visible:= false;
 Panel6.Visible:= true;
 RadioGroup1.Caption:= (Sender as TMultiButton).Caption;
 Panel6.Left:= (Sender as TMultiButton).Left;
 if (Sender as TMultiButton).Name = 'MultiButton25' then
  Panel6.Left:= width - Panel6.Width;
end;


end.

