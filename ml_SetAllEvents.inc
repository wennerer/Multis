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
var lv          : integer;
    aShape      : TShape;
begin
 try
   TmpSet     := TSetAll.Create(nil);
   TmpSet.Assign(TMultiEventLine(GetComponent(0)).FSetAll);
   OldSet     := TSetAll.Create(nil);
   OldSet.Assign(TMultiEventLine(GetComponent(0)).FSetAll);


   SetAllForm                := TForm.Create(Application);
   SetAllForm.Width          := 715;
   SetAllForm.Height         := 510;
   SetAllForm.Left           := (SetAllForm.Monitor.Width div 2) - (SetAllForm.Width div 2);
   SetAllForm.Top            := (SetAllForm.Monitor.Height div 2) -(SetAllForm.Height div 2);
   SetAllForm.BorderStyle    := bsSingle;
   SetAllForm.Caption        := 'Set All Properties For Events';

   for lv:=0 to 11 do
    begin
     FButtons[lv]             := TButton.Create(SetAllForm);
     FButtons[lv].Parent      := SetAllForm;
     FButtons[lv].Left        := 10;
     FButtons[lv].Top         := 10 + (lv*30);
     FButtons[lv].Width       := 150;
     FButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button
   for lv:=12 to 22 do
    begin
     FButtons[lv]             := TButton.Create(SetAllForm);
     FButtons[lv].Parent      := SetAllForm;
     FButtons[lv].Left        := 380;
     FButtons[lv].Top         := 10 + ((lv-12)*30);
     FButtons[lv].Width       := 150;
     FButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button

    FButtons[0].Caption       := 'BorderColor';
    FButtons[1].Caption       := 'BorderWidth';


   for lv:=0 to 0 do
    begin
     FColorBox[lv]             := TColorBox.Create(SetAllForm);
     FColorBox[lv].Parent      := SetAllForm;
     FColorBox[lv].Left        := 170;
     FColorBox[lv].Width       := 150;
     FColorBox[lv].OnChange    := @ColorBoxOnChange;
    end;//ColorBox
   FColorBox[0].Top         := 10;
   FColorBox[0].Selected    := TmpSet.FBorderColor;

   for lv:=0 to 0 do
    begin
     FSpinEdit[lv]             := TSpinEdit.Create(SetAllForm);
     FSpinEdit[lv].Parent      := SetAllForm;
     FSpinEdit[lv].Left        := 170;
     FSpinEdit[lv].Width       := 150;
     FSpinEdit[lv].MinValue    := 0;
     FSpinEdit[lv].MaxValue    := 10;
     FSpinEdit[lv].Alignment   := taRightJustify;
     FSpinEdit[lv].OnChange    := @SpinEditOnChange;
    end;//SpinEdit
   FSpinEdit[0].Top            := 40;
   FSpinEdit[0].Value          := TmpSet.FBorderWidth;

   aShape             := TShape.Create(SetAllForm);
   aShape.Parent      := SetAllForm;
   aShape.Left        := 5;
   aShape.Width       := 705;
   aShape.Top         := 400;
   aShape.Height      := 3;
   aShape.Brush.Color := clMaroon;

   for lv:=23 to 24 do
    begin
     FButtons[lv]             := TButton.Create(SetAllForm);
     FButtons[lv].Parent      := SetAllForm;
     FButtons[lv].Top         := 450;
     FButtons[lv].Width       := 150;
     FButtons[lv].OnClick     := @ButtonsOnClick;
    end;//Button
    FButtons[23].Left        := 100;
    FButtons[24].Left        := 465;
    FButtons[23].Caption     := 'Okay';
    FButtons[24].Caption     := 'Abbrechen';

   SetAllForm.ShowModal;


 finally
   TmpSet.Free;
   OldSet.Free;
   SetAllForm.Free;
 end;
end;

procedure TPropertySetAllEvents.ButtonsOnClick(Sender: TObject);
var aColorDialog : TColorDialog;
begin
 if Sender = FButtons[0] then
  begin
   aColorDialog       := TColorDialog.Create(nil);
   aColorDialog.Color := TmpSet.FBorderColor ;
   try
    if aColorDialog.Execute then
     TmpSet.FBorderColor  := aColorDialog.Color;
     FColorBox[0].Selected := TmpSet.FBorderColor;
   finally
    aColorDialog.Free;
   end;
  end;
  if Sender = FButtons[1] then
   begin
    TmpSet.FBorderWidth:= TmpSet.FBorderWidth+1;
    if TmpSet.FBorderWidth > 10 then TmpSet.FBorderWidth := 1;
    FSpinEdit[0].Value := TmpSet.FBorderWidth;
   end;


  if Sender = FButtons[23] then
   begin
    SetAllForm.Close;
    exit;
   end;
  if Sender = FButtons[24] then
   begin
    SetPtrValue(OldSet);
    SetAllForm.Close;
    exit;
   end;
  SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.ColorBoxOnChange(Sender: TObject);
begin
 if Sender = FColorBox[0] then TmpSet.FBorderColor := FColorBox[0].Selected;


 SetPtrValue(TmpSet);
end;

procedure TPropertySetAllEvents.SpinEditOnChange(Sender: TObject);
begin
 if Sender = FSpinEdit[0] then TmpSet.FBorderWidth := FSpinEdit[0].Value;

 SetPtrValue(TmpSet);
end;


