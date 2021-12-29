unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
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
    MultiButton28: TMultiButton;
    MultiButton29: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton30: TMultiButton;
    MultiButton31: TMultiButton;
    MultiButton32: TMultiButton;
    MultiButton33: TMultiButton;
    MultiButton34: TMultiButton;
    MultiButton35: TMultiButton;
    MultiButton36: TMultiButton;
    MultiButton37: TMultiButton;
    MultiButton38: TMultiButton;
    MultiButton39: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButton7: TMultiButton;
    MultiButton8: TMultiButton;
    MultiButton9: TMultiButton;
    procedure MultiButton16MessageButtonClick(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiButton38Click(Sender: TObject);
    procedure MultiButton39Click(Sender: TObject);

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.MultiButton1Click(Sender: TObject);
begin
 if caption = 'Test the MultiButton' then
  begin
   caption := (Sender as TMultiButton).Name;
   MultiButton14.Enabled:=true;
   MultiButton14.Caption:= 'MultiButton14'+#13+'Enabled';
   MultiButton36.Enabled:=true;
   MultiButton36.Caption:= 'MultiButton14'+#13+'Enabled';
  end
  else
  begin
   caption := 'Test the MultiButton';
   MultiButton14.Enabled:=false;
   MultiButton14.Caption:= 'MultiButton14'+#13+'Enabled_false';
   MultiButton36.Enabled:=false;
   MultiButton36.Caption:= 'MultiButton14'+#13+'Enabled_false';
  end;

end;

procedure TForm1.MultiButton38Click(Sender: TObject);
const c : integer=0;
begin
 inc(c);
 MultiButton38.MessageButton.Caption:=inttostr(c);
end;

procedure TForm1.MultiButton39Click(Sender: TObject);
begin
  if MultiButton39.MessageButton.Visible then MultiButton39.MessageButton.Visible:=false
  else MultiButton39.MessageButton.Visible := true;
end;


procedure TForm1.MultiButton16MessageButtonClick(Sender: TObject);
begin
 if caption = 'Test the MultiButton' then
  begin
   caption := (Sender as TMultiButton).Name+'.MessageButton';
  end
  else
  begin
   caption := 'Test the MultiButton';
  end;
end;


end.

