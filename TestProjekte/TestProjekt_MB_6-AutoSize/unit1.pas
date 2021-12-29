unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MultiButton,typinfo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ImageList1: TImageList;
    ImageList2: TImageList;
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
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButton7: TMultiButton;
    MultiButton8: TMultiButton;
    MultiButton9: TMultiButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var comp : TComponent;
const i : integer = 0;
begin
 AutoSize:=false;
 for comp in self do
  begin
   if comp is TMultiButton then
    begin
     case i of
     0 : (Comp as TMultibutton).Caption:= (Comp as TMultibutton).Name + '_extralong' ;
     1 : (Comp as TMultibutton).Caption:= 'TMB' ;
     2 : (Comp as TMultibutton).Caption:= (Comp as TMultibutton).Name ;
     end;
    end;
  end;
 inc(i);
 if i > 2 then i:=0;
 AutoSize:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
var comp : TComponent;
const i : integer = 0;
begin
 AutoSize:=false;
 for comp in self do
  begin
   if comp is TMultiButton then
    begin
     case i of
     0 : (Comp as TMultibutton).Font.Height:= 25 ;
     1 : (Comp as TMultibutton).Font.Height:= 8 ;
     2 : (Comp as TMultibutton).Font.Height:= 0 ;
     end;
    end;
  end;
 inc(i);
 if i > 2 then i:=0;
 AutoSize:=true;
end;

procedure TForm1.Button3Click(Sender: TObject);
var comp : TComponent;
const i : integer = 0;
begin
 AutoSize:=false;
 for comp in self do
  begin
   if comp is TMultiButton then
    begin
     if ((Comp as TMultibutton).MessageButton.Caption = 'MB') or
        ((Comp as TMultibutton).MessageButton.Caption = 'MB_extralong') then
      begin
       case i of
        0 : (Comp as TMultibutton).MessageButton.Caption:= 'MB_extralong' ;
        1 : (Comp as TMultibutton).MessageButton.Caption:= 'MB' ;
       end;
      end;
     if ((Comp as TMultibutton).MessageButton.Caption = 'Message') or
        ((Comp as TMultibutton).MessageButton.Caption = 'Message_extralong') then
      begin
       case i of
        0 : (Comp as TMultibutton).MessageButton.Caption:= 'Message_extralong' ;
        1 : (Comp as TMultibutton).MessageButton.Caption:= 'Message' ;
       end;
      end;
    end;
  end;
 inc(i);
 if i > 1 then i:=0;
 AutoSize:=true;
end;

procedure TForm1.Button4Click(Sender: TObject);
var comp : TComponent;
const i : integer = 0;
begin
 AutoSize:=false;
 for comp in self do
  begin
   if comp is TMultiButton then
    begin
     case i of
     0 : (Comp as TMultibutton).MessageButton.Font.Height:= 25 ;
     1 : (Comp as TMultibutton).MessageButton.Font.Height:= 8 ;
     2 : (Comp as TMultibutton).MessageButton.Font.Height:= 0 ;
     end;
    end;
  end;
 inc(i);
 if i > 2 then i:=0;
 AutoSize:=true;
end;

procedure TForm1.Button5Click(Sender: TObject);
var comp : TComponent;
const i : integer = 0;
begin
 AutoSize:=false;
 for comp in self do
  begin
   if comp is TMultiButton then
    begin
     case i of
     0 : (Comp as TMultibutton).FocusFrameWidth :=  0;
     1 : (Comp as TMultibutton).FocusFrameWidth := 10;
     2 : (Comp as TMultibutton).FocusFrameWidth :=  5;
     end;
     Button5.Caption:='FocusFrameWidth ['+inttostr((Comp as TMultibutton).FocusFrameWidth)+']';
    end;
  end;
 inc(i);
 if i > 2 then i:=0;
 AutoSize:=true;

end;

procedure TForm1.Button6Click(Sender: TObject);
const i : integer = 4;
begin
 inc(i); if i > 15 then i:=0;
 MultiButton22.MessageButton.Alignment:= TMBAlignment(integer(i));
 MultiButton23.MessageButton.Alignment:= TMBAlignment(integer(i));
 MultiButton24.MessageButton.Alignment:= TMBAlignment(integer(i));
 MultiButton25.MessageButton.Alignment:= TMBAlignment(integer(i));
 Button6.Caption := 'MessageButton Alignment ['+GetEnumName(TypeInfo(TMBAlignment), Ord(i))+']';

end;

procedure TForm1.FormCreate(Sender: TObject);
//var comp : TComponent;
begin
 (*AutoSize:=false;
 for comp in self do
  begin
   if comp is TMultiButton then
    begin
     (Comp as TMultibutton).Caption:= 'MB' ;
     (Comp as TMultibutton).Font.Height:=6;
    end;
  end;

 AutoSize:=true;   *)
end;



end.

