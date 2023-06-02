unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ColorBox, MultiPanel,
  MultiEventLine, MultiRadioGroup, MultiButton, MultiButtonStyleManager,
  MultiSeperator;

type

  { TMainForm }

  TMainForm = class(TForm)
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiEventLine1: TMultiEventLine;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    MultiSeperator1: TMultiSeperator;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
 Left := (Monitor.Width div 2) - (Width div 2);
 Top  := (Monitor.Height div 2) - (Height div 2);
end;



{$Include drawing.inc}
end.

