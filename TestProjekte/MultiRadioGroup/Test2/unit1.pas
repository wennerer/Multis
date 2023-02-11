unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  ExtCtrls, MultiPanel, MultiRadioGroup, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiRadioGroup1: TMultiRadioGroup;
    MultiRadioGroup2: TMultiRadioGroup;
    MultiRadioGroup3: TMultiRadioGroup;
    MultiRadioGroup4: TMultiRadioGroup;
    MultiRadioGroup5: TMultiRadioGroup;
    MultiRadioGroup6: TMultiRadioGroup;
    MultiRadioGroup7: TMultiRadioGroup;
    MultiRadioGroup8: TMultiRadioGroup;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure MultiRadioGroup1Change(Sender: TObject; const aIndex: integer);
    procedure MultiRadioGroup1Click(Sender: TObject; const aIndex: integer);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiRadioGroup1Click(Sender: TObject; const aIndex: integer);
begin
 Label2.Font.Color:= RGB(random(255),random(255),random(255));
 Label2.Caption:= inttostr((Sender as TMultiRadioGroup).RadioButtons[aIndex].Tag);
 Label5.Caption:= (Sender as TMultiRadioGroup).Name;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 MultiRadioGroup1.SetFocus;
 Timer1.Enabled:=false;
end;

procedure TForm1.MultiRadioGroup1Change(Sender: TObject; const aIndex: integer);
begin
 Label4.Font.Color:= RGB(random(255),random(255),random(255));
 Label4.Caption:= inttostr(aIndex);
 Label5.Caption:= (Sender as TMultiRadioGroup).Name;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
 Timer1.Enabled:=true;
end;



end.

