unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ColorBox, Spin,
  StdCtrls, MultiPanel, MultiEventLine, MultiRadioGroup, MultiButton,
  MultiButtonStyleManager, MultiSeperator, MultiplexSlider, MultiCheckGroup;

const
  Line        = 0;
  Rectangle   = 1;
  Polygone    = 2;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
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
    MultiPanel4: TMultiPanel;
    MultiRadioGroup1: TMultiRadioGroup;
    MultiSeperator1: TMultiSeperator;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure ColorBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure MultiButtonClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);

  private
    PenColor   : TColor;
    PenWidth   : integer;
    BrushColor : TColor;
    draw       : boolean;
    Choice     : integer;
    sx,sy      : integer;
    ex,ey      : integer;
    Points     : array of TPoint;
    count      : integer;
    start      : boolean;
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
 Choice     := 0; draw := false;
 PenColor   := clBlue;
 PenWidth   := 1;
 BrushColor := clLime;
 ColorBox1.Selected := clBlue;
 ColorBox2.Selected := clLime;
 count := 1;start := false;

end;







{$Include drawing.inc}
end.

