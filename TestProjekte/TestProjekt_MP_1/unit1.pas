unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiPanel,
  MultiButton, TypInfo, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    MultiPanel4: TMultiPanel;
    procedure FormCreate(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiPanelCompressed(Sender: TObject);
    procedure MultiPanelStreched(Sender: TObject);

  private
    i : integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiButton1Click(Sender: TObject);
var lv : integer;
begin
 inc(i);
 if i > 2 then i:=0;
 MultiButton1.Caption:= GetEnumName(TypeInfo(TTrigger), Ord(i));

 for lv:=0 to pred(ComponentCount) do
  begin
   if Components[lv] is TMultiPanel then TMultiPanel(Components[lv]).DropDownMenu.Trigger:= TTrigger(integer(i));
   if Components[lv] is TMultiPanel then TMultiPanel(Components[lv]).ImageIndex:= -1;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var lv : integer;
begin
 for lv:=0 to pred(ComponentCount) do if Components[lv] is TMultiPanel then
  TMultiPanel(Components[lv]).DropDownMenu.Hotspot := rect(15,15,30,30);
end;

procedure TForm1.MultiPanelCompressed(Sender: TObject);
var lv : integer;
begin
 for lv:=0 to pred(ComponentCount) do if Components[lv] is TMultiPanel then TMultiPanel(Components[lv]).ImageIndex:= -1;
end;

procedure TForm1.MultiPanelStreched(Sender: TObject);
begin
 if i=2 then
  begin
   case (Sender as TMultiPanel).Tag of
    1: MultiPanel1.ImageIndex:= 0;
    2: MultiPanel2.ImageIndex:= 0;
    3: MultiPanel3.ImageIndex:= 0;
    4: MultiPanel4.ImageIndex:= 0;
   end;
  end;
end;


end.

