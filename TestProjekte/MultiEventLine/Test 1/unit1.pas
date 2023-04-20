unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MultiEventLine, MultiRadioGroup, MultiPanel, MultiCheckGroup, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ImageList1: TImageList;
    MultiEventLine1: TMultiEventLine;
    MultiPanel1: TMultiPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MultiRadioGroup1Change(Sender: TObject; const aIndex: integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiRadioGroup1Change(Sender: TObject; const aIndex: integer);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 MultiEventLine1.Events.Items[0].Style:= mesRect;
 MultiEventLine1.Invalidate;
end;

end.

