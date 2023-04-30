{%MultiEventLine}


{ TPropertySetAllEvents }

procedure TPropertySetAllEvents.Edit;
begin
  inherited Edit;
  CreateWindow;
end;

function TPropertySetAllEvents.GetValue: string;
begin
  //Result:=inherited GetValue;
 result := 'SetAllPropertiesForEvents';
end;

function TPropertySetAllEvents.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes;
  Result := [paMultiSelect, paDialog];
end;


procedure TPropertySetAllEvents.CreateWindow;
var SetAllForm  : TCustomForm;
    lv          : integer;

begin
 try
   aObject     := TSetAll.Create;
   aObject.FBorderColor :=    TMultiEventLine(GetComponent(0)).BorderColor;


   SetAllForm                := TForm.Create(Application);
   SetAllForm.Width          := 715;
   SetAllForm.Height         := 510;
   SetAllForm.Left           := (SetAllForm.Monitor.Width div 2) - (SetAllForm.Width div 2);
   SetAllForm.Top            := (SetAllForm.Monitor.Height div 2) -(SetAllForm.Height div 2);
   SetAllForm.BorderStyle    := bsSingle;
   SetAllForm.Caption        := 'Set All Properties For Events';

   for lv:=0 to 1 do
    begin
     FButtons[lv]             := TButton.Create(SetAllForm);
     FButtons[lv].Parent      := SetAllForm;
     FButtons[lv].Left        := 10;
     FButtons[lv].Top         := 10 + (lv*30);
     FButtons[lv].Width       := 150;
     FButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button
    FButtons[0].Caption       := 'BorderColor';
    FButtons[1].Caption       := 'BorderWidth';


   for lv:=0 to 0 do
    begin
     FColorBox[lv]             := TColorBox.Create(SetAllForm);
     FColorBox[lv].Parent      := SetAllForm;
     FColorBox[lv].Left        := 180;
     FColorBox[lv].Width       := 150;
     FColorBox[lv].OnChange    := @ColorBoxOnChange;
    end;//ColorBox
   FColorBox[0].Top         := 10;
   FColorBox[0].Selected    := aObject.FBorderColor;

   for lv:=0 to 0 do
    begin
     FSpinEdit[lv]             := TSpinEdit.Create(SetAllForm);
     FSpinEdit[lv].Parent      := SetAllForm;
     FSpinEdit[lv].Left        := 180;
     FSpinEdit[lv].Width       := 150;
     FSpinEdit[lv].MinValue    := 0;
     FSpinEdit[lv].MaxValue    := 10;
     FSpinEdit[lv].OnChange    := @SpinEditOnChange;
    end;//SpinEdit
   FSpinEdit[0].Top            := 40;
   FSpinEdit[0].Value          := aObject.FBorderWidth;

   SetAllForm.ShowModal;


 finally
   aObject.Free;
   SetAllForm.Free;
 end;
end;

procedure TPropertySetAllEvents.ButtonsOnClick(Sender: TObject);
var aColorDialog : TColorDialog;
begin
 if Sender = FButtons[0] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := aObject.FBorderColor ;
   try
    if aColorDialog.Execute then
     aObject.FBorderColor  := aColorDialog.Color;
     FColorBox[0].Selected := aObject.FBorderColor;
   finally
    aColorDialog.Free;
   end;

   SetPtrValue(aObject);
  end;
end;

procedure TPropertySetAllEvents.ColorBoxOnChange(Sender: TObject);
begin
 if Sender = FColorBox[0] then aObject.FBorderColor := FColorBox[0].Selected;


 SetPtrValue(aObject);
end;

procedure TPropertySetAllEvents.SpinEditOnChange(Sender: TObject);
begin
 if Sender = FSpinEdit[0] then aObject.FBorderWidth := FSpinEdit[0].Value;

 SetPtrValue(aObject);
end;


