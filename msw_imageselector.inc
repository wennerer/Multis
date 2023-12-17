{%MultiSwitch --- this is a part of TMultiSwitch}

{ TPropertyImageSelector }

procedure TPropertyImageSelector.DoShowEditor;
var lv,i,j : integer;
    FImages             : Array[0..39] of TCustomBitmap;
begin
 PEForm                := TForm.Create(Application);
 try
  PEForm.Width          := 670;
  PEForm.Height         := 520;
  PEForm.Left           := (PEForm.Monitor.Width div 2) - (PEForm.Width div 2);
  PEForm.Top            := (PEForm.Monitor.Height div 2) -(PEForm.Height div 2);
  PEForm.BorderStyle    := bsSingle;
  PEForm.Caption        := 'Select a image for a RollButton';

  FFirst := true;

  for lv := 0 to 39 do
    FImages[lv] := TPortableNetworkGraphic.Create;


  FImages[0].LoadFromResourceName(HInstance, 'm_puk009');
  FImages[1].LoadFromResourceName(HInstance, 'm_puk008');
  FImages[2].LoadFromResourceName(HInstance, 'm_puk010');
  FImages[3].LoadFromResourceName(HInstance, 'm_puk007');
  FImages[4].LoadFromResourceName(HInstance, 'm_puk006');
  FImages[5].LoadFromResourceName(HInstance, 'm_puk005');
  FImages[6].LoadFromResourceName(HInstance, 'm_puk003');
  FImages[7].LoadFromResourceName(HInstance, 'm_puk004');
  //load more images, maximal 40   Images must have 64x64pixel!
  FImages[8].LoadFromResourceName(HInstance, 'm_puk002');
  for lv := 9 to 39 do //load the rest with dummy
    FImages[lv].Assign(FImages[8]);

  FRollImage            := TRollImage.Create;
  FRollImage.aDirection := TMultiSwitch(GetComponent(0)).FDirection;

  i := 0;j:=0;
  for lv := 0 to 39 do
   begin
    FThumbnail[lv]            := TThumbnail.Create(PEForm);
    FThumbnail[lv].Parent     := PEForm;
    FThumbnail[lv].Left       := 20+(80*i);
    FThumbnail[lv].Top        := 20+(80*j);
    FThumbnail[lv].Tag        := lv;
    FThumbnail[lv].FImage     := FImages[lv];
    FThumbNail[lv].OnClick    :=@SelectedImage;
    inc(i);
    if i > 7 then
     begin
      i := 0;
      inc(j);
     end;
   end;

  for lv := 0 to 1 do
   begin
    FRadioButton[lv]        := TRadioButton.Create(PEForm);
    FRadioButton[lv].Parent := PEForm;
    FRadioButton[lv].Left   :=  20;
    FRadioButton[lv].Top    := 440 + (lv*30);
    FRadioButton[lv].OnClick:=@RadioButtons;
   end;
  FRadioButton[0].Caption := 'LeftRollImage [Left]';
  FRadioButton[1].Caption := 'RightRollImage [Right]';
  if FRollImage.aDirection = fsLeft then FRadioButton[0].Checked := true
  else FRadioButton[1].Checked := true;


  for lv := 0 to 1 do
   begin
    FButton[lv]        := TButton.Create(PEForm);
    FButton[lv].Parent := PEForm;
    FButton[lv].Left   := 232 + (lv *220);
    FButton[lv].Width  := 200;
    FButton[lv].Top    := 460;
    FButton[lv].Height :=  25;
    FButton[lv].OnClick:=@ButtonsClick;
   end;
  FButton[0].Caption := 'Apply';
  FButton[1].Caption := 'Close';


   PEForm.ShowModal;

 Finally
   FRollImage.Free;
   for lv := 0 to 39 do FImages[lv].Free;
   for lv := 0 to 9 do FThumbnail[lv].Free;
   PEForm.Free;
 end;
end;

procedure TPropertyImageSelector.SelectedImage(Sender: TObject);
var comp        : TComponent;
begin
 for comp in PEForm do
  if comp is TThumbnail then (comp as TThumbnail).aColor:=clSilver;
 (Sender as TThumbnail).aColor:=clLime;
 FRollImage.ImageIndex :=(Sender as TThumbnail).Tag;
end;

procedure TPropertyImageSelector.ButtonsClick(Sender: TObject);
begin
 if Sender = FButton[1] then  //Close
  begin
   PEForm.Close;
   exit;
  end;
 if Sender = FButton[0] then  //Apply
  begin
   SetPtrValue(FRollImage);
  end;
end;

procedure TPropertyImageSelector.RadioButtons(Sender: TObject);
var i : integer;
begin
 if FFirst then
  begin
   FFirst := false;
   exit;
  end;
 i := FRollImage.ImageIndex;
 FRollImage.ImageIndex:= -1;
 if FRadioButton[0].Checked then FRollImage.aDirection:= fsLeft;
 if FRadioButton[1].Checked then FRollImage.aDirection:= fsRight;
 SetPtrValue(FRollImage);
 FRollImage.ImageIndex := i;
end;


procedure TPropertyImageSelector.Edit;
begin
  inherited Edit;
  DoShowEditor;
end;

function TPropertyImageSelector.GetValue: string;
begin
 //Result:=inherited GetValue;
  result := 'ImageSelector';  //Das ist der Text der im OI angezeigt wird, kann man eingeben was man will!
end;

function TPropertyImageSelector.GetAttributes: TPropertyAttributes;
begin
 Result:=inherited GetAttributes;
 Result := [paMultiSelect, paDialog];  //wird benötigt das die Schaltfläche im OI erscheint
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Thumbnail---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

{ TThumbnail }

procedure TThumbnail.SetAColor(AValue: TColor);
begin
  if FAColor=AValue then Exit;
  FAColor:=AValue;
  Invalidate;
end;

constructor TThumbnail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 74;
  Height:= 74;
  FAColor := clSilver;
end;

destructor TThumbnail.Destroy;
begin
  inherited Destroy;
end;

procedure TThumbnail.MouseEnter;
begin
  inherited MouseEnter;
  if FAColor = clLime then exit;
  FAColor := clRed;
  Invalidate;
end;

procedure TThumbnail.MouseLeave;
begin
  inherited MouseLeave;
  if FAColor = clRed then
  FAColor := clSilver;
  Invalidate;
end;

procedure TThumbnail.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(OnClick) then OnClick(self);
end;



procedure TThumbnail.Paint;
begin
  inherited Paint;
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color   := FAColor;
  Canvas.Rectangle(0,0,Width,Height);

  Canvas.Draw(5,5,FImage);
end;

