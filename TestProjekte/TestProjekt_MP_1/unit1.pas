unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, MultiPanel,
  MultiButton, MultiButtonStyleManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    MultiPanel4: TMultiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //self.DisableAlign;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  caption:= inttostr(MultiPanel1.Height);
end;

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
  //showmessage('');
  invalidate;
end;

end.

