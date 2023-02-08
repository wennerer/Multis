unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  MultiPanel, MultiRadioGroup;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MultiPanel1: TMultiPanel;
    MultiRadioGroup1: TMultiRadioGroup;
    MultiRadioGroup2: TMultiRadioGroup;
    MultiRadioGroup3: TMultiRadioGroup;
    procedure MultiRadioGroup1Change(Sender: TObject; const aIndex: integer);
    procedure MultiRadioGroup1Click(Sender: TObject; const aIndex: integer);
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
end;

procedure TForm1.MultiRadioGroup1Change(Sender: TObject; const aIndex: integer);
begin
 Label4.Font.Color:= RGB(random(255),random(255),random(255));
 Label4.Caption:= inttostr(aIndex);
end;


end.

