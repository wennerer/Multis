unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MultiCheckGroup, MultiPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MultiCheckGroup1: TMultiCheckGroup;
    MultiCheckGroup2: TMultiCheckGroup;
    MultiCheckGroup3: TMultiCheckGroup;
    MultiCheckGroup4: TMultiCheckGroup;
    MultiCheckGroup5: TMultiCheckGroup;
    MultiCheckGroup6: TMultiCheckGroup;
    MultiCheckGroup7: TMultiCheckGroup;
    MultiCheckGroup8: TMultiCheckGroup;
    MultiCheckGroup9: TMultiCheckGroup;
    MultiPanel1: TMultiPanel;
    procedure MultiCheckGroupChange(Sender: TObject; const aIndex: integer;
      checked: boolean);
    procedure MultiCheckGroupClick(Sender: TObject; const aIndex: integer;
      checked: boolean);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiCheckGroupClick(Sender: TObject; const aIndex: integer;checked : boolean);
var s : string;
begin
 if checked then s := 'checked' else s:= 'not checked';
 Label3.Caption:= (Sender as TMultiCheckGroup).Name +' Index: '+inttostr(aIndex)+
                  ' Tag: ' +inttostr((Sender as TMultiCheckGroup).Checkboxes.Items[aIndex].Tag)+
                  ' / '+s;
 s:=' ';
end;

procedure TForm1.MultiCheckGroupChange(Sender: TObject; const aIndex: integer;checked : boolean);
var s : string;
begin
 if checked then s := 'checked' else s:= 'not checked';
 Label4.Caption:= (Sender as TMultiCheckGroup).Name +' Index: '+inttostr(aIndex)+
                  ' Tag: ' +inttostr((Sender as TMultiCheckGroup).Checkboxes.Items[aIndex].Tag)+
                  ' / '+s;
 s:=' ';
end;

end.

