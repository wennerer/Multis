unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MultiRadioGroup, MultiPanel, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList3: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiRadioGroup1: TMultiRadioGroup;
    MultiRadioGroup10: TMultiRadioGroup;
    MultiRadioGroup2: TMultiRadioGroup;
    MultiRadioGroup3: TMultiRadioGroup;
    MultiRadioGroup4: TMultiRadioGroup;
    MultiRadioGroup5: TMultiRadioGroup;
    MultiRadioGroup6: TMultiRadioGroup;
    MultiRadioGroup7: TMultiRadioGroup;
    MultiRadioGroup8: TMultiRadioGroup;
    MultiRadioGroup9: TMultiRadioGroup;
    procedure MultiRadioGroup1Click(Sender: TObject; const aIndex: integer);
    procedure MultiRadioGroupsChange(Sender: TObject; const aIndex: integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiRadioGroupsChange(Sender: TObject; const aIndex: integer);
begin
 Label2.Caption:= (Sender as TMultiRadioGroup).Name;
 Label4.Caption:= inttostr(aIndex);
 Label6.Caption:= inttostr((Sender as TMultiRadioGroup).RadioButtons[aIndex].Tag);
end;

procedure TForm1.MultiRadioGroup1Click(Sender: TObject; const aIndex: integer);
begin
 Label2.Caption:= (Sender as TMultiRadioGroup).Name;
 Label4.Caption:= inttostr(aIndex);
 Label6.Caption:= inttostr((Sender as TMultiRadioGroup).RadioButtons[aIndex].Tag);
end;

end.

