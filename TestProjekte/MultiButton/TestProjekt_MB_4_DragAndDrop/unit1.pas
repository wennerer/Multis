unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    ListBox1: TListBox;
    MultiButton1: TMultiButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiButton1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MultiButton1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MultiButton1MessageButtonClick(Sender: TObject);

  private
    LastSelected : string;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
var lv : integer;
begin
  for lv := 0 to 5 do ListBox1.Items.Add('Eintrag '+inttostr(lv));
end;


procedure TForm1.MultiButton1Click(Sender: TObject);
var lv : integer;
begin
 if Listbox1.Count = 0 then
  begin
   for lv := 0 to 5 do ListBox1.Items.Add('Eintrag '+inttostr(lv));
   MultiButton1.MessageButton.Visible:=false;
   MultiButton1.ImageIndex:=0;
  end;
end;

procedure TForm1.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if  Button  =  mbLeft  then ListBox1.BeginDrag(true);
end;


procedure TForm1.MultiButton1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept  :=  true ;
end;

procedure TForm1.MultiButton1MessageButtonClick(Sender: TObject);
begin
 ListBox1.Items.Add(LastSelected);
 MultiButton1.MessageButton.Visible:=false;
 MultiButton1.ImageIndex:=0;
end;


procedure TForm1.MultiButton1DragDrop(Sender, Source: TObject; X, Y: Integer);
var lv : integer;
begin
 if Source = ListBox1 then
  begin
   for lv := pred(ListBox1.Items.Count) downto 0 do
      if ListBox1.Selected[lv] then
       begin
        LastSelected :=Listbox1.Items[ListBox1.Itemindex];
        ListBox1.Items.Delete(lv);
        MultiButton1.MessageButton.Visible:=true;
        MultiButton1.ImageIndex:=1;
      end;
  end;
 end;
end.

