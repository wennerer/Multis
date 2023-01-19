unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, MultiRadioGroup, MultiPanel, MultiButton, MultiplexSlider, TextBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ImageList1: TImageList;
    ImageList2: TImageList;
    MultiButton1: TMultiButton;
    MultiRadioGroup1: TMultiRadioGroup;
    RadioGroup1: TRadioGroup;
    TextBox1: TTextBox;
    procedure MultiRadioGroup1Change(const aIndex: integer);
    procedure MultiRadioGroup1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RadioGroup1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  showmessage('');
end;

procedure TForm1.MultiRadioGroup1Change(const aIndex: integer);
begin
 Textbox1.Caption:= inttostr(aIndex);
end;

procedure TForm1.MultiRadioGroup1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;


end.

