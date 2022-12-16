(*This is a part of the TMultiButtonStyleManager, last change 11.09.2022*)

{%MultiButtonStyleManager ,this is a part of MultiButtonStyleManager}



{ TMultiButtonStyleManagerComponent }

constructor TStyleManagerComponent.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  StyleComp := aComponent;
  Des := ADesigner;


end;

procedure TStyleManagerComponent.Edit;
begin
 DoShowEditor;
end;

function TStyleManagerComponent.GetVerbCount: Integer;
begin
  Result:= 1;
end;

function TStyleManagerComponent.GetVerb(Index: Integer): string;
begin
  Result := 'MultiButtonStyleManagerEditor';
end;

procedure TStyleManagerComponent.ExecuteVerb(Index: Integer);
begin
  DoShowEditor;
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


procedure TStyleManagerComponent.DoShowEditor;
begin
 try
   Editor                := TForm.Create(Application);
   Editor.Width          := 800;
   Editor.Height         := 600;
   Editor.Left           := (screen.Width div 2) - (Editor.Width div 2);
   Editor.Top            := (screen.Height div 2) -(Editor.Height div 2);
   Editor.BorderStyle    := bsSingle;
   Editor.Caption        := 'MultiButtonStyleManagerEditor';

   DesignButton             := TMultiButton.Create(Editor);
   AssignProperties(DesignButton);
   DesignButton.Parent      := Editor;
   DesignButton.Tag         := 100;
   DesignButton.Name        := 'DesignButton';
   DesignButton.Left        := 10;
   DesignButton.Top         := 10;
   DesignButton.MessageButton.Caption:= 'M';

   TmpButton             := TMultiButton.Create(Editor);
   AssignProperties(TmpButton);


   GeneralSettings;

   Settings              := TPageControl.Create(Editor);
   Settings.Parent       := Editor;
   Settings.Color        := clWindow;
   Settings.SetBounds(10,300,Editor.width-20,Editor.height-310);

   LookSettings;

   MSGLookSettings;

   ColorSettings;

   FontSettings;

   Editor.ShowModal;
 finally
   Editor.Free;
 end;
end;


procedure TStyleManagerComponent.AssignProperties(aMultiButton : TMultiButton);
begin

  OffSetWidth    := (StyleComp as TMultiButtonStyleManager).OffSetWidth;
  OffSetHeight   := (StyleComp as TMultiButtonStyleManager).OffSetHeight;

  TmpOffSetWidth    := (StyleComp as TMultiButtonStyleManager).OffSetWidth;
  TmpOffSetHeight   := (StyleComp as TMultiButtonStyleManager).OffSetHeight;

  aMultiButton.Style             := MultiButton.TMButtonStyle((StyleComp as TMultiButtonStyleManager).Style);
  aMultiButton.Width             := (StyleComp as TMultiButtonStyleManager).Width;
  aMultiButton.Height            := (StyleComp as TMultiButtonStyleManager).Height;
  aMultiButton.RndRctRadius      := (StyleComp as TMultiButtonStyleManager).RndRctRadius;
  aMultiButton.FocusFrameON    := (StyleComp as TMultiButtonStyleManager).FocusFrameON;
  aMultiButton.FocusFrameWidth   := (StyleComp as TMultiButtonStyleManager).FocusFrameWidth;
  aMultiButton.ForegroundFocusOn := (StyleComp as TMultiButtonStyleManager).ForegroundFocusOn;
  aMultiButton.FocusAlphaBValue  := (StyleComp as TMultiButtonStyleManager).FocusAlphaBValue;
  aMultiButton.ShowBorder        := (StyleComp as TMultiButtonStyleManager).ShowBorder;
  aMultiButton.HoverOn           := (StyleComp as TMultiButtonStyleManager).HoverOn;
  aMultiButton.BorderWidth       := (StyleComp as TMultiButtonStyleManager).BorderWidth;
  aMultiButton.ColorGradient     := MultiButton.TGradientCourse((StyleComp as TMultiButtonStyleManager).ColorGradient);

  aMultiButton.MessageButton.Visible  := (StyleComp as TMultiButtonStyleManager).MessageButtonVisible;
  aMultiButton.MessageButton.Style    := MultiButton.TMButtonStyle((StyleComp as TMultiButtonStyleManager).MessageButtonStyle);
  aMultiButton.MessageButton.Width    := (StyleComp as TMultiButtonStyleManager).MessageButtonWidth;
  aMultiButton.MessageButton.Height   := (StyleComp as TMultiButtonStyleManager).MessageButtonHeight;
  aMultiButton.MessageButton.Alignment:= MultiButton.TMBAlignment((StyleComp as TMultiButtonStyleManager).MessageButtonAlignment);
  aMultiButton.MessageButton.PositionFactor:= (StyleComp as TMultiButtonStyleManager).MessageButtonPositionFactor;
  aMultiButton.MessageButton.CalculateAlthoughInvisible := (StyleComp as TMultiButtonStyleManager).MessageButtonCalculateAlthoughInvisible;
  aMultiButton.MessageButton.HoverOn  := (StyleComp as TMultiButtonStyleManager).MessageButtonHoverOn;
  aMultiButton.MessageButton.ShowPressed:= (StyleComp as TMultiButtonStyleManager).MessageButtonShowPressed;
  aMultiButton.MessageButton.ShowBorder:= (StyleComp as TMultiButtonStyleManager).MessageButtonShowBorder;
  aMultiButton.MessageButton.PresdColBlendVal:= (StyleComp as TMultiButtonStyleManager).MessageButtonPresdColBlendVal;
  aMultiButton.MessageButton.BorderWidth := (StyleComp as TMultiButtonStyleManager).MessageButtonBorderWidth;
  aMultiButton.MessageButton.ColorGradient:= MultiButton.TGradientCourse((StyleComp as TMultiButtonStyleManager).MessageButtonColorGradient);

  aMultiButton.ColorStart        := (StyleComp as TMultiButtonStyleManager).ColorStart;
  aMultiButton.ColorEnd          := (StyleComp as TMultiButtonStyleManager).ColorEnd;
  aMultiButton.BorderColor       := (StyleComp as TMultiButtonStyleManager).BorderColor;
  aMultiButton.HoverStartColor   := (StyleComp as TMultiButtonStyleManager).HoverStartColor;
  aMultiButton.HoverEndColor     := (StyleComp as TMultiButtonStyleManager).HoverEndColor;
  aMultiButton.HoverFontColor    := (StyleComp as TMultiButtonStyleManager).HoverFontColor;
  aMultiButton.Font.Assign((StyleComp as TMultiButtonStyleManager).Font);
  aMultiButton.FocusColor        := (StyleComp as TMultiButtonStyleManager).FocusColor;
  aMultiButton.PressedStartColor := (StyleComp as TMultiButtonStyleManager).PressedStartColor;
  aMultiButton.PressedEndColor   := (StyleComp as TMultiButtonStyleManager).PressedEndColor;
  aMultiButton.PressedFontColor  := (StyleComp as TMultiButtonStyleManager).PressedFontColor;
  aMultiButton.MessageButton.ColorStart        := (StyleComp as TMultiButtonStyleManager).MessageButtonColorStart;
  aMultiButton.MessageButton.ColorEnd          := (StyleComp as TMultiButtonStyleManager).MessageButtonColorEnd;
  aMultiButton.MessageButton.BorderColor       := (StyleComp as TMultiButtonStyleManager).MessageButtonBorderColor;
  aMultiButton.MessageButton.Font              := (StyleComp as TMultiButtonStyleManager).MessageButtonFont;
  aMultiButton.MessageButton.HoverColor        := (StyleComp as TMultiButtonStyleManager).MessageButtonHoverColor;
  aMultiButton.MessageButton.PressedColor      := (StyleComp as TMultiButtonStyleManager).MessageButtonPressedColor;

  aMultiButton.CaptionAlignment     := (StyleComp as TMultiButtonStyleManager).CaptionAlignment;
  aMultiButton.CaptionLayout        := (StyleComp as TMultiButtonStyleManager).CaptionLayout;
  aMultiButton.CaptionHorMargin     := (StyleComp as TMultiButtonStyleManager).CaptionHorMargin;
  aMultiButton.CaptionVerMargin     := (StyleComp as TMultiButtonStyleManager).CaptionVerMargin;
  aMultiButton.CaptionWordbreak     := (StyleComp as TMultiButtonStyleManager).CaptionWordbreak;

  aMultiButton.MessageButton.CaptionAlignment     := (StyleComp as TMultiButtonStyleManager).MessageButtonCaptionAlignment;
  aMultiButton.MessageButton.CaptionLayout        := (StyleComp as TMultiButtonStyleManager).MessageButtonCaptionLayout;
  aMultiButton.MessageButton.CaptionHorMargin     := (StyleComp as TMultiButtonStyleManager).MessageButtonCaptionHorMargin;
  aMultiButton.MessageButton.CaptionVerMargin     := (StyleComp as TMultiButtonStyleManager).MessageButtonCaptionVerMargin;

  end;

procedure TStyleManagerComponent.GeneralSettings;
var Panel            : TPanel;
    OkButton         : TButton;
    AbortButton      : TButton;
    ApplyButton      : TButton;
    SaveButton       : TButton;
    LoadButton       : TButton;
    EditLeft         : TLabeledEdit;
    EditTop          : TLabeledEdit;
    CheckOffSetWidth : TCheckBox;
    CheckOffSetHeight: TCheckBox;
begin
   Panel                    := TPanel.Create(Editor);
   Panel.Parent             := Editor;
   Panel.Color              := clWindow;
   Panel.SetBounds(600,20,190,230);

   EditLeft                   := TLabeledEdit.Create(Panel);
   EditLeft.Parent            := Panel;
   EditLeft.SetBounds(10,30,70,28);
   EditLeft.EditLabel.Caption := rs_left;
   EditLeft.Caption           := inttostr(DesignButton.Left);
   EditLeft.Alignment         := taRightJustify;
   EditLeft.Tag               := 0;
   EditLeft.OnChange          := @SetLeftTop;
   EditLeft.ShowHint          := true;
   EditLeft.Hint              := rs_EditLeft;

   EditTop                   := TLabeledEdit.Create(Panel);
   EditTop.Parent            := Panel;
   EditTop.SetBounds(110,30,70,28);
   EditTop.EditLabel.Caption := rs_top;
   EditTop.Caption           := inttostr(DesignButton.Top);
   EditTop.Alignment         := taRightJustify;
   EditTop.Tag               := 1;
   EditTop.OnChange          := @SetLeftTop;
   EditTop.ShowHint          := true;
   EditTop.Hint              := rs_EditTop;

   CheckOffSetWidth          := TCheckBox.Create(Panel);
   CheckOffSetWidth.Parent   := Panel;
   CheckOffSetWidth.SetBounds(10,70,300,20);
   CheckOffSetWidth.Checked  := OffSetWidth;
   CheckOffSetWidth.Caption  := rs_takewidth;
   CheckOffSetWidth.Tag      := 0;
   CheckOffSetWidth.OnChange := @OffSet;
   CheckOffSetWidth.ShowHint := true;
   CheckOffSetWidth.Hint     := rs_CheckOffSetWidth;

   CheckOffSetHeight          := TCheckBox.Create(Panel);
   CheckOffSetHeight.Parent   := Panel;
   CheckOffSetHeight.SetBounds(10,90,300,20);
   CheckOffSetHeight.Checked  := OffSetHeight;
   CheckOffSetHeight.Caption  := rs_takeheight;
   CheckOffSetHeight.Tag      := 1;
   CheckOffSetHeight.OnChange := @OffSet;
   CheckOffSetHeight.ShowHint := true;
   CheckOffSetHeight.Hint     := rs_CheckOffSetHeight;

   SaveButton                 := TButton.Create(Editor);
   SaveButton.Parent          := Panel;
   SaveButton.Tag             := 104;
   SaveButton.Caption         := rs_save;
   SaveButton.Height          := 25;
   SaveButton.Width           :=  75;
   SaveButton.Left            := 10;
   SaveButton.Top             := 125;
   SaveButton.OnClick         := @Save_LoadButtonOnClick;
   SaveButton.ShowHint        := true;
   SaveButton.Hint            := rs_SaveButton;

   LoadButton                 := TButton.Create(Editor);
   LoadButton.Parent          := Panel;
   LoadButton.Tag             := 105;
   LoadButton.Caption         := rs_load;
   LoadButton.Height          := 25;
   LoadButton.Width           := 75;
   LoadButton.Left            := 105;
   LoadButton.Top             := 125;
   LoadButton.OnClick         := @Save_LoadButtonOnClick;
   LoadButton.ShowHint        := true;
   LoadButton.Hint            := rs_LoadButton;

   ApplyButton                 := TButton.Create(Editor);
   ApplyButton.Parent          := Panel;
   ApplyButton.Tag             := 103;
   ApplyButton.Caption         := rs_apply;
   ApplyButton.Height          := 25;
   ApplyButton.Width           := 170;
   ApplyButton.Left            := 10;
   ApplyButton.Top             := 160;
   ApplyButton.OnClick         := @OK_AbortButtonOnClick;
   ApplyButton.ShowHint        := true;
   ApplyButton.Hint            := rs_ApplyButton;

   OkButton                 := TButton.Create(Editor);
   OkButton.Parent          := Panel;
   OkButton.Tag             := 101;
   OkButton.Caption         := rs_ok;
   OkButton.Height          := 25;
   OkButton.Width           :=  75;
   OkButton.Left            := 10;
   OkButton.Top             := 195;
   OkButton.OnClick         := @OK_AbortButtonOnClick;
   OkButton.ShowHint        := true;
   OkButton.Hint            := rs_OkButton;

   AbortButton                 := TButton.Create(Editor);
   AbortButton.Parent          := Panel;
   AbortButton.Tag             := 102;
   AbortButton.Caption         := rs_cancel;
   AbortButton.Height          := 25;
   AbortButton.Width           := 75;
   AbortButton.Left            := 105;
   AbortButton.Top             := 195;
   AbortButton.OnClick         := @OK_AbortButtonOnClick;
   AbortButton.ShowHint        := true;
   AbortButton.Hint            := rs_AbortButton;

end;

procedure TStyleManagerComponent.Save_LoadButtonOnClick(Sender: TObject);
begin
 if (Sender as TButton).Tag = 104 then   //save
  SaveProperties;

 if (Sender as TButton).Tag = 105 then   //load
  LoadProperties;

end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---look---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TStyleManagerComponent.LookSettings;
var lv            : integer;
    StaticText1   : TStaticText;
    StaticText2   : TStaticText;
begin
 Look                := TTabsheet.Create(Editor);
 Look.Parent         := Settings;
 Look.Caption        := rs_look;

 StaticText1         := TStaticText.Create(Look);
 StaticText1.Parent  := look;
 StaticText1.SetBounds(10,120,150,25);
 StaticText1.Caption := 'Style:';

 StyleCombobox        := TComboBox.Create(Look);
 StyleCombobox.Parent := Look;
 StyleCombobox.SetBounds(10,140,150,28);
 StyleCombobox.Items.Add('mbsRect');
 StyleCombobox.Items.Add('mbsRoundRect');
 StyleCombobox.Items.Add('mbsCircle');
 StyleCombobox.Items.Add('mbsEllipse');
 StyleCombobox.ItemIndex:= ord(DesignButton.Style);
 StyleCombobox.OnChange   := @StyleOnChange;
 StyleCombobox.ShowHint   := true;
 StyleCombobox.Hint       := rs_style;

 StaticText2         := TStaticText.Create(Look);
 StaticText2.Parent  := look;
 StaticText2.SetBounds(10,65,150,25);
 StaticText2.Caption := 'ColorGradient:';

 ColGradCombo        := TComboBox.Create(Look);
 ColGradCombo.Parent := Look;
 ColGradCombo.SetBounds(10,85,150,28);
 ColGradCombo.Items.Add('gcHorizontal');
 ColGradCombo.Items.Add('gcVertical');
 ColGradCombo.Items.Add('gcSpread');
 ColGradCombo.Items.Add('gcRadiant');
 ColGradCombo.Items.Add('gcAlternate');
 ColGradCombo.ItemIndex := ord(DesignButton.ColorGradient);
 ColGradCombo.OnChange  := @ColGradOnChange;
 ColGradCombo.ShowHint  := true;
 ColGradCombo.Hint      := rs_ColorGradient;

 for lv:=0 to 5 do
  begin
   lEdits[lv]         := TLabeledEdit.Create(Look);
   lEdits[lv].Parent  := Look;
   lEdits[lv].Width   :=  70;
   lEdits[lv].Height  :=  28;
   lEdits[lv].Tag     := lv;
   lEdits[lv].Alignment:= taRightJustify;
   lEdits[lv].OnChange:= @lEditsOnChange;
   lEdits[lv].ShowHint:= true;
  end;
 lEdits[0].EditLabel.Caption := 'Width:';
 lEdits[0].Left    := 10;
 lEdits[0].Top     := 32;
 lEdits[0].Caption := inttostr(DesignButton.Width);
 lEdits[0].Hint    := rs_width_;
 if OffSetWidth then
  lEdits[0].Font.Color:= clDefault else lEdits[0].Font.Color:= clRed;

 lEdits[1].EditLabel.Caption := 'Height:';
 lEdits[1].Left    := 90;
 lEdits[1].Top     := 32; //85
 lEdits[1].Caption := inttostr(DesignButton.Height);
 lEdits[1].Hint    := rs_height_;
 if OffSetHeight then
  lEdits[1].Font.Color:= clDefault else lEdits[1].Font.Color:= clRed;
 if DesignButton.Style = MultiButton.TMButtonStyle(mbsCircle) then lEdits[1].Enabled:=false;

 lEdits[2].EditLabel.Caption := 'RndRctRadius:';
 lEdits[2].Left    := 10;
 lEdits[2].Top     := 200;//140;
 lEdits[2].Width   := 150;
 lEdits[2].Caption := inttostr(DesignButton.RndRctRadius);
 lEdits[2].Hint    := rs_rndrct;
 if DesignButton.Style <> MultiButton.TMButtonStyle(mbsRoundRect) then lEdits[2].Enabled:= false else lEdits[2].Enabled:= true;

 lEdits[3].EditLabel.Caption := 'Foc.FrameWidth:';
 lEdits[3].Left    := 210;
 lEdits[3].Top     := 85;
 lEdits[3].Width   := 100;
 lEdits[3].Caption := inttostr(DesignButton.FocusFrameWidth);
 lEdits[3].Hint    := rs_focframewidth;

 lEdits[4].EditLabel.Caption := 'Foc.AlphaBVal.:';
 lEdits[4].Left    := 210;
 lEdits[4].Top     := 140;
 lEdits[4].Width   := 100;
 lEdits[4].Caption := inttostr(DesignButton.FocusAlphaBValue);
 lEdits[4].Hint    := rs_focalphabval;

 lEdits[5].EditLabel.Caption := 'BorderWidth:';
 lEdits[5].Left    := 350;
 lEdits[5].Top     :=  85;
 lEdits[5].Width   := 100;
 lEdits[5].Caption := inttostr(DesignButton.BorderWidth);
 lEdits[5].Hint    := rs_borderwidth;

 for lv:=0 to 3 do
  begin
   lCheckBox[lv]         := TCheckBox.Create(Look);
   lCheckBox[lv].Parent  := Look;
   lCheckBox[lv].Width   := 100;
   lCheckBox[lv].Height  :=  28;
   lCheckBox[lv].Tag     := lv;
   lCheckBox[lv].OnClick := @LCheckBoxOnClick;
   lCheckBox[lv].ShowHint:= true;
  end;

   lCheckBox[0].Caption := 'FocusFrameOn';
   lCheckBox[0].Checked := DesignButton.FocusFrameON;
   lCheckBox[0].Left    := 210;
   lCheckBox[0].Top     := 28;
   lCheckBox[0].Hint    := rs_focusframeon;

   lCheckBox[1].Caption := 'ShowBorder';
   lCheckBox[1].Checked := DesignButton.ShowBorder;
   lCheckBox[1].Left    := 350;
   lCheckBox[1].Top     := 28;
   lCheckBox[1].Hint    := rs_showborder;

   lCheckBox[2].Caption := 'HoverOn';
   lCheckBox[2].Checked := DesignButton.HoverOn;
   lCheckBox[2].Left    := 490;
   lCheckBox[2].Top     := 28;
   lCheckBox[2].Hint    := rs_hoveron;

   lCheckBox[3].Caption := 'ForegroundFocusOn';
   lCheckBox[3].Checked := DesignButton.ForegroundFocusOn;
   lCheckBox[3].Left    := 210;
   lCheckBox[3].Top     := 200;
   lCheckBox[3].Hint    := rs_foregrdfocuson;

end;

procedure TStyleManagerComponent.LCheckBoxOnClick(Sender: TObject);
begin
  case (Sender as TCheckBox).Tag of
   0: DesignButton.FocusFrameON   := (Sender as TCheckBox).Checked;
   1: DesignButton.ShowBorder       := (Sender as TCheckBox).Checked;
   2: DesignButton.HoverOn          := (Sender as TCheckBox).Checked;
   3: DesignButton.ForegroundFocusOn:= (Sender as TCheckBox).Checked;
  end;

end;

procedure TStyleManagerComponent.StyleOnChange(Sender: TObject);
begin
 case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.Style := MultiButton.TMButtonStyle(mbsRect);
  1 : DesignButton.Style := MultiButton.TMButtonStyle(mbsRoundRect);
  2 : DesignButton.Style := MultiButton.TMButtonStyle(mbsCircle);
  3 : DesignButton.Style := MultiButton.TMButtonStyle(mbsEllipse);
 end;
 if DesignButton.Style = MultiButton.TMButtonStyle(mbsCircle) then lEdits[1].Caption := inttostr(DesignButton.Width);
 if DesignButton.Style <> MultiButton.TMButtonStyle(mbsRoundRect) then lEdits[2].Enabled:= false else lEdits[2].Enabled:= true;
 if DesignButton.Style = MultiButton.TMButtonStyle(mbsCircle) then lEdits[1].Enabled:=false else lEdits[1].Enabled:=true;
end;

procedure TStyleManagerComponent.ColGradOnChange(Sender: TObject);
begin
 case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.ColorGradient := MultiButton.TGradientCourse(gcHorizontal);
  1 : DesignButton.ColorGradient := MultiButton.TGradientCourse(gcVertical);
  2 : DesignButton.ColorGradient := MultiButton.TGradientCourse(gcSpread);
  3 : DesignButton.ColorGradient := MultiButton.TGradientCourse(gcRadiant);
  4 : DesignButton.ColorGradient := MultiButton.TGradientCourse(gcAlternate);
 end;
end;

procedure TStyleManagerComponent.lEditsOnChange(Sender: TObject);
var i,r : integer;
begin
 r := DesignButton.RndRctRadius;
 case (Sender as TLabeledEdit).Tag of
  0: begin
      DesignButton.Width           := strtointdef((Sender as TLabeledEdit).Caption,0);
      DesignButton.RndRctRadius    := r;
     end;
  1: DesignButton.Height           := strtointdef((Sender as TLabeledEdit).Caption,0);
  2: DesignButton.RndRctRadius     := strtointdef((Sender as TLabeledEdit).Caption,0);
  3: DesignButton.FocusFrameWidth  := strtointdef((Sender as TLabeledEdit).Caption,0);
  4: begin
      i:= strtointdef((Sender as TLabeledEdit).Caption,0);
      if (i<0) or (i>255) then
       begin
        showmessage('Value must between 0 and 255');
        lEdits[4].Caption := inttostr(255);
        exit;
       end;
      DesignButton.FocusAlphaBValue :=i;
     end;
  5: DesignButton.BorderWidth       := strtointdef((Sender as TLabeledEdit).Caption,0);
 end;
 if DesignButton.Style = MultiButton.TMButtonStyle(mbsCircle) then lEdits[1].Caption:=inttostr(DesignButton.Width);

end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---MessageButton look---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TStyleManagerComponent.MSGLookSettings;
var lv           : integer;
    StaticText1  : TStaticText;
    StaticText2  : TStaticText;
    StaticText3  : TStaticText;
begin
 MsgButtonLook                := TTabsheet.Create(Editor);
 MsgButtonLook.Parent         := Settings;
 MsgButtonLook.Caption        := rs_msgbuttonlook;


 for lv:=0 to 4 do
  begin
   MsgCheckBox[lv]         := TCheckBox.Create(MsgButtonLook);
   MsgCheckBox[lv].Parent  := MsgButtonLook;
   MsgCheckBox[lv].Width   := 100;
   MsgCheckBox[lv].Height  :=  28;
   MsgCheckBox[lv].Tag     := lv;
   MsgCheckBox[lv].OnClick := @MsgCheckBoxOnClick;
   MsgCheckBox[lv].ShowHint:= true;
  end;

 MsgCheckBox[0].Caption := 'Visible';
 MsgCheckBox[0].Checked := DesignButton.MessageButton.Visible;
 MsgCheckBox[0].Left    := 10;
 MsgCheckBox[0].Top     := 10;
 MsgCheckBox[0].Hint    := rs_visible;

 StaticText1         := TStaticText.Create(MsgButtonLook);
 StaticText1.Parent  := MsgButtonlook;
 StaticText1.SetBounds(10,40,150,25);
 StaticText1.Caption := 'Style:';

 MsgStyleCombobox        := TComboBox.Create(MsgButtonLook);
 MsgStyleCombobox.Parent := MsgButtonLook;
 MsgStyleCombobox.SetBounds(10,60,150,28);
 MsgStyleCombobox.Items.Add('mbsRect');
 MsgStyleCombobox.Items.Add('mbsRoundRect');
 MsgStyleCombobox.Items.Add('mbsCircle');
 MsgStyleCombobox.Items.Add('mbsEllipse');
 MsgStyleCombobox.ItemIndex:= ord(DesignButton.MessageButton.Style);
 MsgStyleCombobox.OnChange:= @MsgStyleOnChange;
 MsgStyleCombobox.ShowHint:= true;
 MsgStyleCombobox.Hint    := rs_msgbuttonstyle;

 for lv:=0 to 4 do
  begin
   MsgEdits[lv]         := TLabeledEdit.Create(MsgButtonLook);
   MsgEdits[lv].Parent  := MsgButtonLook;
   MsgEdits[lv].Width   :=  70;
   MsgEdits[lv].Height  :=  18;
   MsgEdits[lv].Tag     := lv;
   MsgEdits[lv].Alignment:= taRightJustify;
   MsgEdits[lv].OnChange:= @MsgEditsOnChange;
   MsgEdits[lv].ShowHint:= true;
   MsgEdits[lv].NumbersOnly:= true;
  end;
 MsgEdits[0].EditLabel.Caption := 'Width:';
 MsgEdits[0].Left    := 10;
 MsgEdits[0].Top     := 115;
 MsgEdits[0].Caption := inttostr(DesignButton.MessageButton.Width);
 MsgEdits[0].Hint    := rs_width;

 MsgEdits[1].EditLabel.Caption := 'Height:';
 MsgEdits[1].Left    := 90;
 MsgEdits[1].Top     := 115;
 MsgEdits[1].Caption := inttostr(DesignButton.MessageButton.Height);
 MsgEdits[1].Hint    := rs_height;
 if DesignButton.MessageButton.Style = MultiButton.TMButtonStyle(mbsCircle) then MsgEdits[1].Enabled:=false;

 StaticText2         := TStaticText.Create(MsgButtonLook);
 StaticText2.Parent  := MsgButtonlook;
 StaticText2.SetBounds(10,150,150,25);
 StaticText2.Caption := 'Alignment:';

 MsgAlignment        := TComboBox.Create(MsgButtonLook);
 MsgAlignment.Parent := MsgButtonLook;
 MsgAlignment.SetBounds(10,170,150,28);
 MsgAlignment.Items.Add('alNW');
 MsgAlignment.Items.Add('alN');
 MsgAlignment.Items.Add('alNE');
 MsgAlignment.Items.Add('alE');
 MsgAlignment.Items.Add('alSE');
 MsgAlignment.Items.Add('alS');
 MsgAlignment.Items.Add('alSW');
 MsgAlignment.Items.Add('alW');
 MsgAlignment.Items.Add('alRightIn');
 MsgAlignment.Items.Add('alLeftIn');
 MsgAlignment.Items.Add('alTopIn');
 MsgAlignment.Items.Add('alBottomIn');
 MsgAlignment.Items.Add('alRightOut');
 MsgAlignment.Items.Add('alLeftOut');
 MsgAlignment.Items.Add('alTopOut');
 MsgAlignment.Items.Add('alBottomOut');
 MsgAlignment.ItemIndex:= ord(DesignButton.MessageButton.Alignment);
 MsgAlignment.OnChange := @MsgAlignmentOnChange;
 MsgAlignment.ShowHint := true;
 MsgAlignment.Hint     := rs_msgalignment;

 MsgEdits[2].EditLabel.Caption := 'PositionFactor:';
 MsgEdits[2].Left    := 10;
 MsgEdits[2].Top     := 225;
 MsgEdits[2].Width   := 150;
 MsgEdits[2].Caption := inttostr(DesignButton.MessageButton.PositionFactor);
 MsgEdits[2].Hint    := rs_posfac;

 MsgCheckBox[1].Caption := 'CalculateAlthoughInvisible';
 MsgCheckBox[1].Checked := DesignButton.MessageButton.CalculateAlthoughInvisible;
 MsgCheckBox[1].Left    := 180;
 MsgCheckBox[1].Top     := 10;
 MsgCheckBox[1].Hint    := rs_calcinvisible;

 MsgCheckBox[2].Caption := 'HoverOn';
 MsgCheckBox[2].Checked := DesignButton.MessageButton.HoverOn;
 MsgCheckBox[2].Left    := 180;
 MsgCheckBox[2].Top     := 40;
 MsgCheckBox[2].Hint    := rs_hoveron;

 MsgCheckBox[3].Caption := 'ShowPressed';
 MsgCheckBox[3].Checked := DesignButton.MessageButton.ShowPressed;
 MsgCheckBox[3].Left    := 180;
 MsgCheckBox[3].Top     := 70;
 MsgCheckBox[3].Hint    := rs_showpressed;

 MsgEdits[3].EditLabel.Caption := 'PresdColBlendVal:';
 MsgEdits[3].Left    := 180;
 MsgEdits[3].Top     := 115;
 MsgEdits[3].Width   := 150;
 MsgEdits[3].Caption := inttostr(DesignButton.MessageButton.PresdColBlendVal);
 MsgEdits[3].Hint    := rs_prcolblendval;

 MsgCheckBox[4].Caption := 'ShowBorder';
 MsgCheckBox[4].Checked := DesignButton.MessageButton.ShowBorder;
 MsgCheckBox[4].Left    := 180;
 MsgCheckBox[4].Top     := 150;
 MsgCheckBox[4].Hint    := rs_showborder;

 MsgEdits[4].EditLabel.Caption := 'BorderWidth:';
 MsgEdits[4].Left    := 180;
 MsgEdits[4].Top     := 195;
 MsgEdits[4].Width   := 150;
 MsgEdits[4].Caption := inttostr(DesignButton.MessageButton.BorderWidth);
 MsgEdits[4].Hint    := rs_borderwidth;

 StaticText3         := TStaticText.Create(MsgButtonLook);
 StaticText3.Parent  := MsgButtonLook;
 StaticText3.SetBounds(400,10,150,25);
 StaticText3.Caption := 'ColorGradient:';

 MsgColGradCombo        := TComboBox.Create(MsgButtonLook);
 MsgColGradCombo.Parent := MsgButtonLook;
 MsgColGradCombo.SetBounds(400,30,150,28);
 MsgColGradCombo.Items.Add('gcHorizontal');
 MsgColGradCombo.Items.Add('gcVertical');
 MsgColGradCombo.Items.Add('gcSpread');
 MsgColGradCombo.Items.Add('gcRadiant');
 MsgColGradCombo.Items.Add('gcAlternate');
 MsgColGradCombo.ItemIndex:= ord(DesignButton.MessageButton.ColorGradient);
 MsgColGradCombo.OnChange:= @MsgColGradOnChange;
 MsgColGradCombo.ShowHint:= true;
 MsgColGradCombo.Hint    := rs_ColorGradient;
end;

procedure TStyleManagerComponent.MsgCheckBoxOnClick(Sender: TObject);
begin
 case (Sender as TCheckBox).Tag of
   0: DesignButton.MessageButton.Visible                    := (Sender as TCheckBox).Checked;
   1: DesignButton.MessageButton.CalculateAlthoughInvisible := (Sender as TCheckBox).Checked;
   2: DesignButton.MessageButton.HoverOn                    := (Sender as TCheckBox).Checked;
   3: DesignButton.MessageButton.ShowPressed                := (Sender as TCheckBox).Checked;
   4: DesignButton.MessageButton.ShowBorder                 := (Sender as TCheckBox).Checked;
  end;
end;

procedure TStyleManagerComponent.MsgStyleOnChange(Sender: TObject);
begin
  case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.MessageButton.Style := MultiButton.TMButtonStyle(mbsRect);
  1 : DesignButton.MessageButton.Style := MultiButton.TMButtonStyle(mbsRoundRect);
  2 : DesignButton.MessageButton.Style := MultiButton.TMButtonStyle(mbsCircle);
  3 : DesignButton.MessageButton.Style := MultiButton.TMButtonStyle(mbsEllipse);
 end;
 if DesignButton.MessageButton.Style = MultiButton.TMButtonStyle(mbsCircle) then MsgEdits[1].Caption := inttostr(DesignButton.MessageButton.Width);
 if DesignButton.MessageButton.Style = MultiButton.TMButtonStyle(mbsCircle) then MsgEdits[1].Enabled:=false else MsgEdits[1].Enabled:=true;
end;

procedure TStyleManagerComponent.MsgEditsOnChange(Sender: TObject);
var i : integer;
begin
 case (Sender as TLabeledEdit).Tag of
  0: begin
      if DesignButton.MessageButton.Style = MultiButton.TMButtonStyle(mbsCircle) then
       if strtointdef((Sender as TLabeledEdit).Caption,0) = 1 then exit;
       //1 throws an exception ??
      DesignButton.MessageButton.Width            := strtointdef((Sender as TLabeledEdit).Caption,0);
     end;
  1: DesignButton.MessageButton.Height           := strtointdef((Sender as TLabeledEdit).Caption,0);
  2: DesignButton.MessageButton.PositionFactor   := strtointdef((Sender as TLabeledEdit).Caption,0);
  3: begin
      i:= strtointdef((Sender as TLabeledEdit).Caption,0);
      if (i<0) or (i>255) then
       begin
        showmessage('Value must between 0 and 255');
        MsgEdits[3].Caption := inttostr(255);
        exit;
       end;
      DesignButton.MessageButton.PresdColBlendVal :=i;
     end;
  4: DesignButton.MessageButton.BorderWidth   := strtointdef((Sender as TLabeledEdit).Caption,1);
 end;
 if DesignButton.MessageButton.Style = MultiButton.TMButtonStyle(mbsCircle) then MsgEdits[1].Caption:=inttostr(DesignButton.MessageButton.Width);
end;

procedure TStyleManagerComponent.MsgAlignmentOnChange(Sender: TObject);
begin
 case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alNW);
  1 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alN);
  2 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alNE);
  3 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alE);
  4 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alSE);
  5 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alS);
  6 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alSW);
  7 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alW);
  8 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alRightIn);
  9 : DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alLeftIn);
  10: DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alTopIn);
  11: DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alBottomIn);
  12: DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alRightOut);
  13: DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alLeftOut);
  14: DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alTopOut);
  15: DesignButton.MessageButton.Alignment := MultiButton.TMBAlignment(alBottomOut);

 end;
end;

procedure TStyleManagerComponent.MsgColGradOnChange(Sender: TObject);
begin
  case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.MessageButton.ColorGradient := MultiButton.TGradientCourse(gcHorizontal);
  1 : DesignButton.MessageButton.ColorGradient := MultiButton.TGradientCourse(gcVertical);
  2 : DesignButton.MessageButton.ColorGradient := MultiButton.TGradientCourse(gcSpread);
  3 : DesignButton.MessageButton.ColorGradient := MultiButton.TGradientCourse(gcRadiant);
  4 : DesignButton.MessageButton.ColorGradient := MultiButton.TGradientCourse(gcAlternate);
 end;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---ColorSettings---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TStyleManagerComponent.AdjustColorBox(aColor: TColor; aIndex: integer);
var Str         : string;
    lv          : integer;
    clfl        : boolean;
begin
 str := colortostring(aColor); clfl := false;

 for lv:=0 to pred(AColorBox[aIndex].Items.Count) do
   begin
    if AColorBox[aIndex].Items.Strings[lv] = str then clfl :=true;
    if clfl then break;
   end;
 if not clfl then AColorBox[aIndex].Items.InsertObject(0,str,TObject(PtrInt(aColor)));
 AColorBox[aIndex].Selected:=aColor;
end;

procedure TStyleManagerComponent.ColorSettings;
var lv            : integer;
    StaticText1   : TStaticText;
    StaticText2   : TStaticText;

begin
 Colors                := TTabsheet.Create(Editor);
 Colors.Parent         := Settings;
 Colors.Caption        := rs_colors;

 StaticText1           := TStaticText.Create(Colors);
 StaticText1.Parent    := Colors;
 StaticText1.Caption   := rs_mbsettings;
 StaticText1.SetBounds(10,5,300,25);

 StaticText2           := TStaticText.Create(Colors);
 StaticText2.Parent    := Colors;
 StaticText2.Caption   := rs_msgbsettings;
 StaticText2.SetBounds(520,5,300,25);

 PopUp         := TPopupMenu.Create(Colors);
 PopUp.Parent  := Colors;
 Item0:=TMenuItem.Create(Colors);
 Item0.Caption := rs_popupcopy;
 Item0.OnClick := @Item0CopyColor;
 PopUp.Items.Add(Item0);
 Item1:=TMenuItem.Create(Colors);
 Item1.Caption := rs_popuppaste;
 Item1.OnClick := @Item1PasteColor;
 PopUp.Items.Add(Item1);
 Item1.Enabled := false;


 for lv:=0 to 16 do
  begin
   ColorButtons[lv]         := TMultiButton.Create(Colors);
   ColorButtons[lv].Parent  := Colors;
   ColorButtons[lv].Width   := 100;
   ColorButtons[lv].Height  :=  28;
   ColorButtons[lv].FocusFrameWidth:= 2;
   ColorButtons[lv].Style   := MultiButton.TMButtonStyle(mbsRect);
   ColorButtons[lv].Tag     := lv;
   ColorButtons[lv].OnClick := @ColorButtonsOnClick;
   ColorButtons[lv].MessageButton.Style       := MultiButton.TMButtonStyle(mbsRect);
   ColorButtons[lv].MessageButton.Alignment   := MultiButton.TMBAlignment(alRightIn);
   ColorButtons[lv].MessageButton.Width       := 20;
   ColorButtons[lv].MessageButton.Height      := 20;
   ColorButtons[lv].PopupMenu := PopUp;
   ColorButtons[lv].OnMouseEnter:= @ColorButtonsMouseEnter;
   ColorButtons[lv].ShowHint := true;

   AColorBox[lv]         := TColorBox.Create(Colors);
   AColorBox[lv].Parent  := Colors;
   AColorBox[lv].Tag     := lv;
   AColorBox[lv].Width   := 120;
   AColorBox[lv].Style   := [cbStandardColors,cbExtendedColors,cbSystemColors,cbCustomColors];
   AColorBox[lv].OnChange:= @ColorBoxOnChange;



   if lv<=5 then
    begin
     ColorButtons[lv].Left   := 10;
     AColorBox[lv].Left      := 112;
     ColorButtons[lv].Top     := 30+(lv*35);
     AColorBox[lv].Top        := 30+(lv*35);
    end;
   if (lv>5) and (lv<=10) then
    begin
     ColorButtons[lv].Left   := 250;
     AColorBox[lv].Left      := 352;
     ColorButtons[lv].Top     := 30+((lv-6)*35);
     AColorBox[lv].Top        := 30+((lv-6)*35);
    end;
   if lv>10 then
    begin
     ColorButtons[lv].Left   := 520;
     AColorBox[lv].Left      := 622;
     ColorButtons[lv].Top     := 30+((lv-11)*35);
     AColorBox[lv].Top        := 30+((lv-11)*35);
    end;
  end;

  ColorButtons[0].Caption := 'ColorStart';
  ColorButtons[0].Hint    := rs_colorstart;
  AdjustColorBox(DesignButton.ColorStart,0);
  ColorButtons[1].Caption := 'ColorEnd';
  ColorButtons[1].Hint    := rs_colorend;
  AdjustColorBox(DesignButton.ColorEnd,1);
  ColorButtons[2].Caption := 'BorderColor';
  ColorButtons[2].Hint    := rs_bordercolor;
  AdjustColorBox(DesignButton.BorderColor,2);
  ColorButtons[3].Caption := 'HoverStartCol';
  ColorButtons[3].Hint    := rs_hoverstartcol;
  AdjustColorBox(DesignButton.HoverStartColor,3);
  ColorButtons[4].Caption := 'HoverEndColor';
  ColorButtons[4].Hint    := rs_hoverendcol;
  AdjustColorBox(DesignButton.HoverEndColor,4);
  ColorButtons[5].Caption := 'HoverFontCol';
  ColorButtons[5].Hint    := rs_hoverfontcol;
  AdjustColorBox(DesignButton.HoverFontColor,5);
  ColorButtons[6].Caption := 'FontColor';
  ColorButtons[6].Hint    := rs_fontcolor;
  AdjustColorBox(DesignButton.Font.Color,6);
  ColorButtons[7].Caption := 'FocusColor';
  ColorButtons[7].Hint    := rs_focuscolor;
  AdjustColorBox(DesignButton.FocusColor,7);
  ColorButtons[8].Caption := 'PressedStCol';
  ColorButtons[8].Hint    := rs_pressedstcol;
  AdjustColorBox(DesignButton.PressedStartColor,8);
  ColorButtons[9].Caption := 'PressedEndCol';
  ColorButtons[9].Hint    := rs_pressedendcol;
  AdjustColorBox(DesignButton.PressedEndColor,9);
  ColorButtons[10].Caption := 'PrssdFontCol';
  ColorButtons[10].Hint    := rs_prssdfontcol;
  AdjustColorBox(DesignButton.PressedFontColor,10);

  ColorButtons[11].Caption := 'ColorStart';
  ColorButtons[11].Hint    := rs_msgcolorstart;
  AdjustColorBox(DesignButton.MessageButton.ColorStart,11);
  ColorButtons[12].Caption := 'ColorEnd';
  ColorButtons[12].Hint    := rs_msgcolorend;
  AdjustColorBox(DesignButton.MessageButton.ColorEnd,12);
  ColorButtons[13].Caption := 'BorderColor';
  ColorButtons[13].Hint    := rs_msgbordercolor;
  AdjustColorBox(DesignButton.MessageButton.BorderColor,13);
  ColorButtons[14].Caption := 'FontColor';
  ColorButtons[14].Hint    := rs_msgfontcolor;
  AdjustColorBox(DesignButton.MessageButton.Font.Color,14);
  ColorButtons[15].Caption := 'HoverColor';
  ColorButtons[15].Hint    := rs_msghovercolor;
  AdjustColorBox(DesignButton.MessageButton.HoverColor,15);
  ColorButtons[16].Caption := 'PressedCol';
  ColorButtons[16].Hint    := rs_pressedcol;
  AdjustColorBox(DesignButton.MessageButton.PressedColor,16);

  ColorButtons[0].MessageButton.Visible    := true;
  ColorButtons[0].MessageButton.Caption    := 'G';
  ColorButtons[0].CaptionAlignment         := taLeftJustify;
  ColorButtons[0].CaptionHorMargin         := 4;
  ColorButtons[0].MessageButton.OnClick    := @SetColorGradient;
  ColorButtons[11].MessageButton.Visible   := true;
  ColorButtons[11].MessageButton.Caption   := 'G';
  ColorButtons[11].CaptionAlignment        := taLeftJustify;
  ColorButtons[11].CaptionHorMargin        := 4;
  ColorButtons[11].MessageButton.OnClick   := @SetColorGradient;

  ColorButtons[7].MessageButton.Visible    := true;
  ColorButtons[7].MessageButton.Caption    := #$CE#$B1;  //widechar($03B1);
  ColorButtons[7].CaptionAlignment         := taLeftJustify;
  ColorButtons[7].CaptionHorMargin         := 4;
  ColorButtons[7].MessageButton.Font.Height:= 18;
  ColorButtons[7].MessageButton.OnClick    := @SetAlphaBlendValue;
  ColorButtons[16].MessageButton.Visible   := true;
  ColorButtons[16].MessageButton.Caption   := #$CE#$B1;
  ColorButtons[16].CaptionAlignment        := taLeftJustify;
  ColorButtons[16].CaptionHorMargin        := 4;
  ColorButtons[16].MessageButton.Font.Height:= 18;
  ColorButtons[16].MessageButton.OnClick   := @SetAlphaBlendValue;
end;

procedure TStyleManagerComponent.ColorButtonsOnClick(Sender: TObject);
var ColorDialog : TColorDialog;
begin
 try
   ColorDialog    := TColorDialog.Create(nil);
   ColorDialog.Color := AColorBox[AkCol].Selected;
   if ColorDialog.Execute then
   begin
    AdjustColorBox(ColorDialog.Color,(Sender as TMultiButton).Tag);
    case (Sender as TMultiButton).Tag of
     0: DesignButton.ColorStart        := ColorDialog.Color;
     1: DesignButton.ColorEnd          := ColorDialog.Color;
     2: DesignButton.BorderColor       := ColorDialog.Color;
     3: DesignButton.HoverStartColor   := ColorDialog.Color;
     4: DesignButton.HoverEndColor     := ColorDialog.Color;
     5: DesignButton.HoverFontColor    := ColorDialog.Color;
     6: DesignButton.Font.Color        := ColorDialog.Color;
     7: DesignButton.FocusColor        := ColorDialog.Color;
     8: DesignButton.PressedStartColor := ColorDialog.Color;
     9: DesignButton.PressedEndColor   := ColorDialog.Color;
    10: DesignButton.PressedFontColor  := ColorDialog.Color;
    11: DesignButton.MessageButton.ColorStart        := ColorDialog.Color;
    12: DesignButton.MessageButton.ColorEnd          := ColorDialog.Color;
    13: DesignButton.MessageButton.BorderColor       := ColorDialog.Color;
    14: DesignButton.MessageButton.Font.Color        := ColorDialog.Color;
    15: DesignButton.MessageButton.HoverColor        := ColorDialog.Color;
    16: DesignButton.MessageButton.PressedColor      := ColorDialog.Color;
    end;
   end;
 finally
   ColorDialog.Free;
 end;
end;

procedure TStyleManagerComponent.ColorBoxOnChange(Sender: TObject);
begin
 case (Sender as TColorBox).Tag of
     0: DesignButton.ColorStart        := (Sender as TColorBox).Selected;
     1: DesignButton.ColorEnd          := (Sender as TColorBox).Selected;
     2: DesignButton.BorderColor       := (Sender as TColorBox).Selected;
     3: DesignButton.HoverStartColor   := (Sender as TColorBox).Selected;
     4: DesignButton.HoverEndColor     := (Sender as TColorBox).Selected;
     5: DesignButton.HoverFontColor    := (Sender as TColorBox).Selected;
     6: DesignButton.Font.Color        := (Sender as TColorBox).Selected;
     7: DesignButton.FocusColor        := (Sender as TColorBox).Selected;
     8: DesignButton.PressedStartColor := (Sender as TColorBox).Selected;
     9: DesignButton.PressedEndColor   := (Sender as TColorBox).Selected;
    10: DesignButton.PressedFontColor  := (Sender as TColorBox).Selected;
    11: DesignButton.MessageButton.ColorStart        := (Sender as TColorBox).Selected;
    12: DesignButton.MessageButton.ColorEnd          := (Sender as TColorBox).Selected;
    13: DesignButton.MessageButton.BorderColor       := (Sender as TColorBox).Selected;
    14: DesignButton.MessageButton.Font.Color        := (Sender as TColorBox).Selected;
    15: DesignButton.MessageButton.HoverColor        := (Sender as TColorBox).Selected;
    16: DesignButton.MessageButton.PressedColor      := (Sender as TColorBox).Selected;
    end;
end;

procedure TStyleManagerComponent.ColorButtonsMouseEnter(Sender: TObject);
begin
  AkCol := (Sender as TMultiButton).Tag;
end;

procedure TStyleManagerComponent.Item0CopyColor(Sender: TObject);
begin
 copiedcolor := AColorBox[AkCol].Selected;
 Item1.Enabled:= true;
end;

procedure TStyleManagerComponent.Item1PasteColor(Sender: TObject);
begin
 AdjustColorBox(copiedcolor,AkCol);
end;

procedure TStyleManagerComponent.SetColorGradient(Sender: TObject);
begin
 if Sender = ColorButtons[0] then
  begin
   look.Show;
   ColGradCombo.SetFocus;
  end else
  begin
   MsgButtonLook.Show;
   MsgColGradCombo.SetFocus;
  end;

end;

procedure TStyleManagerComponent.SetLeftTop(Sender: TObject);
begin
 case (Sender as TLabeledEdit).Tag of
  0: DesignButton.Left            := strtointdef((Sender as TLabeledEdit).Caption,0);
  1: DesignButton.Top             := strtointdef((Sender as TLabeledEdit).Caption,0);

 end;
end;

procedure TStyleManagerComponent.OffSet(Sender: TObject);
begin
 case (Sender as TCheckBox).Tag of
  0: OffSetWidth           := (Sender as TCheckBox).Checked;
  1: OffSetHeight          := (Sender as TCheckBox).Checked;
 end;
 if OffSetWidth then
  lEdits[0].Font.Color:= clDefault else lEdits[0].Font.Color:= clRed;
 if OffSetHeight then
  lEdits[1].Font.Color:= clDefault else lEdits[1].Font.Color:= clRed;
end;

procedure TStyleManagerComponent.SetAlphaBlendValue(Sender: TObject);
begin
 if Sender = ColorButtons[7] then
  begin
   look.Show;
   lEdits[4].SetFocus
  end else
  begin
   MsgButtonLook.Show;
   MsgEdits[3].SetFocus;
  end;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX----FONT-SETTINGS---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TStyleManagerComponent.FontSettings;
var StaticText1   : TStaticText;
    StaticText2   : TStaticText;
    StaticText3   : TStaticText;
    StaticText4   : TStaticText;
    lv            : integer;
    StaticText5   : TStaticText;
    StaticText6   : TStaticText;
begin
 Fonts                := TTabsheet.Create(Editor);
 Fonts.Parent         := Settings;
 Fonts.Caption        := rs_fonts;

 StaticText1           := TStaticText.Create(Fonts);
 StaticText1.Parent    := Fonts;
 StaticText1.Caption   := rs_mbsettings;
 StaticText1.SetBounds(10,5,300,25);

 StaticText2           := TStaticText.Create(Fonts);
 StaticText2.Parent    := Fonts;
 StaticText2.Caption   := rs_msgbsettings;
 StaticText2.SetBounds(400,5,300,25);

 StaticText3         := TStaticText.Create(Fonts);
 StaticText3.Parent  := Fonts;
 StaticText3.SetBounds(10,30,150,25);
 StaticText3.Caption := 'CaptionAlignment:';

 CapAligCombo        := TComboBox.Create(Fonts);
 CapAligCombo.Parent := Fonts;
 CapAligCombo.SetBounds(10,50,150,28);
 CapAligCombo.Items.Add('taLeftJustify');
 CapAligCombo.Items.Add('taRightJustify');
 CapAligCombo.Items.Add('taCenter');
 CapAligCombo.ItemIndex:= ord(DesignButton.CaptionAlignment);
 CapAligCombo.OnChange := @CapAligOnChange;
 CapAligCombo.ShowHint := true;
 CapAligCombo.Hint     := rs_CaptionAlignment;

 StaticText4         := TStaticText.Create(Fonts);
 StaticText4.Parent  := Fonts;
 StaticText4.SetBounds(200,30,150,25);
 StaticText4.Caption := 'CaptionLayout:';

 CapLayCombo        := TComboBox.Create(Fonts);
 CapLayCombo.Parent := Fonts;
 CapLayCombo.SetBounds(200,50,150,28);
 CapLayCombo.Items.Add('tlTop');
 CapLayCombo.Items.Add('tlCenter');
 CapLayCombo.Items.Add('tlBottom');
 CapLayCombo.ItemIndex:= ord(DesignButton.CaptionLayout);
 CapLayCombo.OnChange := @CapLayOnChange;
 CapLayCombo.ShowHint := true;
 CapLayCombo.Hint     := rs_CaptionLayout;

 for lv:=0 to 3 do
  begin
   CapEdits[lv]         := TLabeledEdit.Create(Fonts);
   CapEdits[lv].Parent  := Fonts;
   CapEdits[lv].Width   :=  150;
   CapEdits[lv].Height  :=  18;
   CapEdits[lv].Tag     := lv;
   CapEdits[lv].Alignment:= taRightJustify;
   CapEdits[lv].OnChange:= @CapEditsOnChange;
   CapEdits[lv].ShowHint:= true;
  end;
 CapEdits[0].EditLabel.Caption := 'CaptionHorMargin:';
 CapEdits[0].Left    := 10;
 CapEdits[0].Top     := 100;
 CapEdits[0].Caption := inttostr(DesignButton.CaptionHorMargin);
 CapEdits[0].Hint    := rs_CaptionHorMargin;
 if DesignButton.CaptionAlignment <> taLeftJustify then CapEdits[0].Enabled:= false;

 CapEdits[1].EditLabel.Caption := 'CaptionVerMargin:';
 CapEdits[1].Left    := 200;
 CapEdits[1].Top     := 100;
 CapEdits[1].Caption := inttostr(DesignButton.CaptionVerMargin);
 CapEdits[1].Hint    := rs_CaptionVerMargin;
 if DesignButton.CaptionLayout <> tlTop then CapEdits[1].Enabled:= false;

 for lv:=0 to 1 do
  begin
   FontDialogB[lv]         := TMultiButton.Create(Fonts);
   FontDialogB[lv].Parent  := Fonts;
   FontDialogB[lv].Width   := 150;
   FontDialogB[lv].Height  :=  28;
   FontDialogB[lv].FocusFrameWidth:= 2;
   FontDialogB[lv].Style   := MultiButton.TMButtonStyle(mbsRect);
   FontDialogB[lv].Tag     := lv;
   FontDialogB[lv].OnClick := @FontDialogOnClick;
   FontDialogB[lv].ShowHint:= true;
  end;
 FontDialogB[0].Caption     :='Font';
 FontDialogB[0].Left        := 10;
 FontDialogB[0].Top         := 140;
 FontDialogB[0].Hint        := rs_fontdialog;

 WordBCheckBox         := TCheckBox.Create(Fonts);
 WordBCheckBox.Parent  := Fonts;
 WordBCheckBox.Width   := 150;
 WordBCheckBox.Height  :=  28;
 WordBCheckBox.OnClick := @WordBOnClick;
 WordBCheckBox.Caption := 'CaptionWordBreak';
 WordBCheckBox.Checked := DesignButton.CaptionWordbreak;
 WordBCheckBox.Left    := 200;
 WordBCheckBox.Top     := 140;
 WordBCheckBox.ShowHint:= true;
 WordBCheckBox.Hint    := rs_wordbreak;

 StaticText5         := TStaticText.Create(Fonts);
 StaticText5.Parent  := Fonts;
 StaticText5.SetBounds(400,30,150,25);
 StaticText5.Caption := 'CaptionAlignment:';

 MsgCapAligCombo        := TComboBox.Create(Fonts);
 MsgCapAligCombo.Parent := Fonts;
 MsgCapAligCombo.SetBounds(400,50,150,28);
 MsgCapAligCombo.Items.Add('taLeftJustify');
 MsgCapAligCombo.Items.Add('taRightJustify');
 MsgCapAligCombo.Items.Add('taCenter');
 MsgCapAligCombo.ItemIndex:= ord(DesignButton.MessageButton.CaptionAlignment);
 MsgCapAligCombo.OnChange:= @MsgCapAligOnChange;
 MsgCapAligCombo.ShowHint:= true;
 MsgCapAligCombo.Hint    := rs_CaptionAlignment;

 StaticText6         := TStaticText.Create(Fonts);
 StaticText6.Parent  := Fonts;
 StaticText6.SetBounds(590,30,150,25);
 StaticText6.Caption := 'CaptionLayout:';

 MsgCapLayCombo        := TComboBox.Create(Fonts);
 MsgCapLayCombo.Parent := Fonts;
 MsgCapLayCombo.SetBounds(590,50,150,28);
 MsgCapLayCombo.Items.Add('tlTop');
 MsgCapLayCombo.Items.Add('tlCenter');
 MsgCapLayCombo.Items.Add('tlBottom');
 MsgCapLayCombo.ItemIndex:= ord(DesignButton.MessageButton.CaptionLayout);
 MsgCapLayCombo.OnChange:= @MsgCapLayOnChange;
 MsgCapLayCombo.ShowHint:= true;
 MsgCapLayCombo.Hint    := rs_CaptionLayout;

 CapEdits[2].EditLabel.Caption := 'CaptionHorMargin:';
 CapEdits[2].Left    := 400;
 CapEdits[2].Top     := 100;
 CapEdits[2].Caption := inttostr(DesignButton.MessageButton.CaptionHorMargin);
 CapEdits[2].Hint    := rs_CaptionHorMargin;
 if DesignButton.MessageButton.CaptionAlignment <> taLeftJustify then CapEdits[2].Enabled:= false;

 CapEdits[3].EditLabel.Caption := 'CaptionVerMargin:';
 CapEdits[3].Left    := 590;
 CapEdits[3].Top     := 100;
 CapEdits[3].Caption := inttostr(DesignButton.MessageButton.CaptionVerMargin);
 CapEdits[3].Hint    := rs_CaptionVerMargin;
 if DesignButton.MessageButton.CaptionLayout <> tlTop then CapEdits[3].Enabled:= false;

 FontDialogB[1].Caption     :='Font';
 FontDialogB[1].Left        := 400;
 FontDialogB[1].Top         := 140;
 FontDialogB[1].Hint        := rs_fontdialog;
end;

procedure TStyleManagerComponent.CapAligOnChange(Sender: TObject);
begin
 case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.CaptionAlignment := taLeftJustify;
  1 : DesignButton.CaptionAlignment := taRightJustify;
  2 : DesignButton.CaptionAlignment := taCenter;
 end;
 if DesignButton.CaptionAlignment <> taLeftJustify then CapEdits[0].Enabled:= false else CapEdits[0].Enabled:=true;
end;

procedure TStyleManagerComponent.CapLayOnChange(Sender: TObject);
begin
 case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.CaptionLayout := tlTop;
  1 : DesignButton.CaptionLayout := tlCenter;
  2 : DesignButton.CaptionLayout := tlBottom;
 end;
 if DesignButton.CaptionLayout <> tlTop then CapEdits[1].Enabled:= false else CapEdits[1].Enabled := true;
end;

procedure TStyleManagerComponent.CapEditsOnChange(Sender: TObject);
begin
  case (Sender as TLabeledEdit).Tag of
  0: DesignButton.CaptionHorMargin      := strtointdef((Sender as TLabeledEdit).Caption,0);
  1: DesignButton.CaptionVerMargin      := strtointdef((Sender as TLabeledEdit).Caption,0);

 end;
end;

procedure TStyleManagerComponent.FontDialogOnClick(Sender: TObject);
var FontDialog : TFontDialog;

begin
 try
  FontDialog := TFontDialog.Create(nil);
  if (Sender as TMultiButton).Tag = 0 then
   begin
    FontDialog.Font := DesignButton.Font;
    if FontDialog.Execute then DesignButton.Font := FontDialog.Font;
   end;
  if (Sender as TMultiButton).Tag = 1 then
   begin
    FontDialog.Font := DesignButton.MessageButton.Font;
    if FontDialog.Execute then DesignButton.MessageButton.Font := FontDialog.Font;
   end;
 finally
  FontDialog.Free;
 end;
end;

procedure TStyleManagerComponent.WordBOnClick(Sender: TObject);
begin
 DesignButton.CaptionWordbreak    := (Sender as TCheckBox).Checked;
end;

procedure TStyleManagerComponent.MsgCapAligOnChange(Sender: TObject);
begin
 case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.MessageButton.CaptionAlignment := taLeftJustify;
  1 : DesignButton.MessageButton.CaptionAlignment := taRightJustify;
  2 : DesignButton.MessageButton.CaptionAlignment := taCenter;
 end;
 if DesignButton.MessageButton.CaptionAlignment <> taLeftJustify then CapEdits[2].Enabled:= false else CapEdits[2].Enabled:=true;
end;

procedure TStyleManagerComponent.MsgCapLayOnChange(Sender: TObject);
begin
 case (Sender as TComboBox).ItemIndex of
  0 : DesignButton.MessageButton.CaptionLayout := tlTop;
  1 : DesignButton.MessageButton.CaptionLayout := tlCenter;
  2 : DesignButton.MessageButton.CaptionLayout := tlBottom;
 end;
 if DesignButton.MessageButton.CaptionLayout <> tlTop then CapEdits[3].Enabled:= false else CapEdits[3].Enabled := true;
end;

procedure TStyleManagerComponent.OK_AbortButtonOnClick(Sender: TObject);
var aMultiButton : TMultiButton;
begin
 aMultiButton := DesignButton;
 (StyleComp as TMultiButtonStyleManager).OffSetWidth       := OffSetWidth;
 (StyleComp as TMultiButtonStyleManager).OffSetHeight      := OffSetHeight;

 if (Sender as TButton).Tag = 102 then
  begin
   aMultiButton := TmpButton;
   (StyleComp as TMultiButtonStyleManager).OffSetWidth       := TmpOffSetWidth;
   (StyleComp as TMultiButtonStyleManager).OffSetHeight      := TmpOffSetHeight;
  end;




 (StyleComp as TMultiButtonStyleManager).Style             := MultiButtonStyleManager.TMButtonStyle(aMultiButton.Style);

 if OffSetWidth then
 (StyleComp as TMultiButtonStyleManager).Width             := aMultiButton.Width;
 if OffSetHeight then
 (StyleComp as TMultiButtonStyleManager).Height            := aMultiButton.Height;

 (StyleComp as TMultiButtonStyleManager).RndRctRadius      := aMultiButton.RndRctRadius;
 (StyleComp as TMultiButtonStyleManager).FocusFrameON      := aMultiButton.FocusFrameON;
 (StyleComp as TMultiButtonStyleManager).FocusFrameWidth   := aMultiButton.FocusFrameWidth;
 (StyleComp as TMultiButtonStyleManager).ForegroundFocusOn := aMultiButton.ForegroundFocusOn;
 (StyleComp as TMultiButtonStyleManager).FocusAlphaBValue  := aMultiButton.FocusAlphaBValue;
 (StyleComp as TMultiButtonStyleManager).ShowBorder        := aMultiButton.ShowBorder;
 (StyleComp as TMultiButtonStyleManager).HoverOn           := aMultiButton.HoverOn;
 (StyleComp as TMultiButtonStyleManager).BorderWidth       := aMultiButton.BorderWidth;
 (StyleComp as TMultiButtonStyleManager).ColorGradient     := MultiButtonStyleManager.TGradientCourse(aMultiButton.ColorGradient);

 (StyleComp as TMultiButtonStyleManager).MessageButtonVisible  := aMultiButton.MessageButton.Visible;
 (StyleComp as TMultiButtonStyleManager).MessageButtonStyle    := MultiButtonStyleManager.TMButtonStyle(aMultiButton.MessageButton.Style);
 (StyleComp as TMultiButtonStyleManager).MessageButtonWidth    := aMultiButton.MessageButton.Width;
 (StyleComp as TMultiButtonStyleManager).MessageButtonHeight   := aMultiButton.MessageButton.Height;
 (StyleComp as TMultiButtonStyleManager).MessageButtonAlignment:= MultiButtonStyleManager.TMBAlignment(aMultiButton.MessageButton.Alignment);
 (StyleComp as TMultiButtonStyleManager).MessageButtonPositionFactor:= aMultiButton.MessageButton.PositionFactor;
 (StyleComp as TMultiButtonStyleManager).MessageButtonCalculateAlthoughInvisible:= aMultiButton.MessageButton.CalculateAlthoughInvisible;
 (StyleComp as TMultiButtonStyleManager).MessageButtonHoverOn  := aMultiButton.MessageButton.HoverOn;
 (StyleComp as TMultiButtonStyleManager).MessageButtonShowPressed:= aMultiButton.MessageButton.ShowPressed;
 (StyleComp as TMultiButtonStyleManager).MessageButtonShowBorder:= aMultiButton.MessageButton.ShowBorder;
 (StyleComp as TMultiButtonStyleManager).MessageButtonPresdColBlendVal:= aMultiButton.MessageButton.PresdColBlendVal;
 (StyleComp as TMultiButtonStyleManager).MessageButtonBorderWidth:= aMultiButton.MessageButton.BorderWidth;
 (StyleComp as TMultiButtonStyleManager).MessageButtonColorGradient:= MultiButtonStyleManager.TGradientCourse(aMultiButton.MessageButton.ColorGradient);

 (StyleComp as TMultiButtonStyleManager).ColorStart        := aMultiButton.ColorStart;
 (StyleComp as TMultiButtonStyleManager).ColorEnd          := aMultiButton.ColorEnd;
 (StyleComp as TMultiButtonStyleManager).BorderColor       := aMultiButton.BorderColor;
 (StyleComp as TMultiButtonStyleManager).HoverStartColor   := aMultiButton.HoverStartColor;
 (StyleComp as TMultiButtonStyleManager).HoverEndColor     := aMultiButton.HoverEndColor;
 (StyleComp as TMultiButtonStyleManager).HoverFontColor    := aMultiButton.HoverFontColor;
 (StyleComp as TMultiButtonStyleManager).Font.Assign(aMultiButton.Font);
 (StyleComp as TMultiButtonStyleManager).FocusColor        := aMultiButton.FocusColor;
 (StyleComp as TMultiButtonStyleManager).PressedStartColor := aMultiButton.PressedStartColor;
 (StyleComp as TMultiButtonStyleManager).PressedEndColor   := aMultiButton.PressedEndColor;
 (StyleComp as TMultiButtonStyleManager).PressedFontColor  := aMultiButton.PressedFontColor;
 (StyleComp as TMultiButtonStyleManager).MessageButtonColorStart          := aMultiButton.MessageButton.ColorStart;
 (StyleComp as TMultiButtonStyleManager).MessageButtonColorEnd            := aMultiButton.MessageButton.ColorEnd;
 (StyleComp as TMultiButtonStyleManager).MessageButtonBorderColor         := aMultiButton.MessageButton.BorderColor;
 (StyleComp as TMultiButtonStyleManager).MessageButtonFont.Assign(aMultiButton.MessageButton.Font);
 (StyleComp as TMultiButtonStyleManager).MessageButtonHoverColor          := aMultiButton.MessageButton.HoverColor;
 (StyleComp as TMultiButtonStyleManager).MessageButtonPressedColor        := aMultiButton.MessageButton.PressedColor;

 (StyleComp as TMultiButtonStyleManager).CaptionAlignment     := aMultiButton.CaptionAlignment;
 (StyleComp as TMultiButtonStyleManager).CaptionLayout        := aMultiButton.CaptionLayout;
 (StyleComp as TMultiButtonStyleManager).CaptionHorMargin     := aMultiButton.CaptionHorMargin;
 (StyleComp as TMultiButtonStyleManager).CaptionVerMargin     := aMultiButton.CaptionVerMargin;
 (StyleComp as TMultiButtonStyleManager).CaptionWordbreak     := aMultiButton.CaptionWordbreak;

 (StyleComp as TMultiButtonStyleManager).MessageButtonCaptionAlignment     := aMultiButton.MessageButton.CaptionAlignment;
 (StyleComp as TMultiButtonStyleManager).MessageButtonCaptionLayout        := aMultiButton.MessageButton.CaptionLayout;
 (StyleComp as TMultiButtonStyleManager).MessageButtonCaptionHorMargin     := aMultiButton.MessageButton.CaptionHorMargin;
 (StyleComp as TMultiButtonStyleManager).MessageButtonCaptionVerMargin     := aMultiButton.MessageButton.CaptionVerMargin;



 (StyleComp as TMultiButtonStyleManager).Modified;
 if (Sender as TButton).Tag <> 103 then Editor.Close;
end;

procedure TStyleManagerComponent.SaveProperties;
var SaveDialog : TSaveDialog;
    DatenStr   : TFileStream;
begin
  try
   SaveDialog              := TSaveDialog.Create(nil);
   //SaveDialog.Title        := rs_M_SaveAS ;
   SaveDialog.Options      := SaveDialog.Options+[ofoverwriteprompt,ofpathmustexist];
   SaveDialog.DefaultExt   := '.mbs';
   SaveDialog.Filter       := 'mbs Files (*.mbs)';
   SaveDialog.FileName     := 'NewStyle';
   //SaveDialog.InitialDir   := '/templates';

   if SaveDialog.Execute then
    begin
     try
      DatenStr := TFileStream.Create(SaveDialog.FileName,fmcreate);

      DatenStr.Write(DesignButton.Style,sizeof(DesignButton.Style));
      DatenStr.Write(DesignButton.Width,sizeof(DesignButton.Width));
      DatenStr.Write(DesignButton.Height,sizeof(DesignButton.Height));
      DatenStr.Write(DesignButton.RndRctRadius,sizeof(DesignButton.RndRctRadius));
      DatenStr.Write(DesignButton.FocusFrameON,sizeof(DesignButton.FocusFrameON));
      DatenStr.Write(DesignButton.FocusFrameWidth,sizeof(DesignButton.FocusFrameWidth));
      DatenStr.Write(DesignButton.ForegroundFocusOn,sizeof(DesignButton.ForegroundFocusOn));
      DatenStr.Write(DesignButton.FocusAlphaBValue,sizeof(DesignButton.FocusAlphaBValue));
      DatenStr.Write(DesignButton.ShowBorder,sizeof(DesignButton.ShowBorder));
      DatenStr.Write(DesignButton.HoverOn,sizeof(DesignButton.HoverOn));
      DatenStr.Write(DesignButton.BorderWidth,sizeof(DesignButton.BorderWidth));
      DatenStr.Write(DesignButton.ColorGradient,sizeof(DesignButton.ColorGradient));
      DatenStr.Write(DesignButton.ColorStart,sizeof(DesignButton.ColorStart));
      DatenStr.Write(DesignButton.ColorEnd,sizeof(DesignButton.ColorEnd));
      DatenStr.Write(DesignButton.BorderColor,sizeof(DesignButton.BorderColor));
      DatenStr.Write(DesignButton.HoverStartColor,sizeof(DesignButton.HoverStartColor));
      DatenStr.Write(DesignButton.HoverEndColor,sizeof(DesignButton.HoverEndColor));
      DatenStr.Write(DesignButton.HoverFontColor,sizeof(DesignButton.HoverFontColor));
      DatenStr.Write(DesignButton.FocusColor,sizeof(DesignButton.FocusColor));
      DatenStr.Write(DesignButton.PressedStartColor,sizeof(DesignButton.PressedStartColor));
      DatenStr.Write(DesignButton.PressedEndColor,sizeof(DesignButton.PressedEndColor));
      DatenStr.Write(DesignButton.PressedFontColor,sizeof(DesignButton.PressedFontColor));
      DatenStr.Write(DesignButton.CaptionHorMargin,sizeof(DesignButton.CaptionHorMargin));
      DatenStr.Write(DesignButton.CaptionVerMargin,sizeof(DesignButton.CaptionVerMargin));
      DatenStr.Write(DesignButton.CaptionWordbreak,sizeof(DesignButton.CaptionWordbreak));
      DatenStr.Write(DesignButton.CaptionAlignment,sizeof(DesignButton.CaptionAlignment));
      DatenStr.Write(DesignButton.CaptionLayout,sizeof(DesignButton.CaptionLayout));
      DatenStr.Write(DesignButton.Font.Color,sizeof(DesignButton.Font.Color));

      DatenStr.Write(DesignButton.MessageButton.Visible,sizeof(DesignButton.MessageButton.Visible));
      DatenStr.Write(DesignButton.MessageButton.Style,sizeof(DesignButton.MessageButton.Style));
      DatenStr.Write(DesignButton.MessageButton.Width,sizeof(DesignButton.MessageButton.Width));
      DatenStr.Write(DesignButton.MessageButton.Height,sizeof(DesignButton.MessageButton.Height));
      DatenStr.Write(DesignButton.MessageButton.Alignment,sizeof(DesignButton.MessageButton.Alignment));
      DatenStr.Write(DesignButton.MessageButton.PositionFactor,sizeof(DesignButton.MessageButton.PositionFactor));
      DatenStr.Write(DesignButton.MessageButton.CalculateAlthoughInvisible,
                     sizeof(DesignButton.MessageButton.CalculateAlthoughInvisible));
      DatenStr.Write(DesignButton.MessageButton.HoverOn,sizeof(DesignButton.MessageButton.HoverOn));
      DatenStr.Write(DesignButton.MessageButton.ShowPressed,sizeof(DesignButton.MessageButton.ShowPressed));
      DatenStr.Write(DesignButton.MessageButton.ShowBorder,sizeof(DesignButton.MessageButton.ShowBorder));
      DatenStr.Write(DesignButton.MessageButton.PresdColBlendVal,sizeof(DesignButton.MessageButton.PresdColBlendVal));
      DatenStr.Write(DesignButton.MessageButton.BorderWidth,sizeof(DesignButton.MessageButton.BorderWidth));
      DatenStr.Write(DesignButton.MessageButton.ColorGradient,sizeof(DesignButton.MessageButton.ColorGradient));
      DatenStr.Write(DesignButton.MessageButton.ColorStart,sizeof(DesignButton.MessageButton.ColorStart));
      DatenStr.Write(DesignButton.MessageButton.ColorEnd,sizeof(DesignButton.MessageButton.ColorEnd));
      DatenStr.Write(DesignButton.MessageButton.BorderColor,sizeof(DesignButton.MessageButton.BorderColor));
      DatenStr.Write(DesignButton.MessageButton.HoverColor,sizeof(DesignButton.MessageButton.HoverColor));
      DatenStr.Write(DesignButton.MessageButton.PressedColor,sizeof(DesignButton.MessageButton.PressedColor));
      DatenStr.Write(DesignButton.MessageButton.CaptionAlignment,sizeof(DesignButton.MessageButton.CaptionAlignment));
      DatenStr.Write(DesignButton.MessageButton.CaptionLayout,sizeof(DesignButton.MessageButton.CaptionLayout));
      DatenStr.Write(DesignButton.MessageButton.CaptionHorMargin,sizeof(DesignButton.MessageButton.CaptionHorMargin));
      DatenStr.Write(DesignButton.MessageButton.CaptionVerMargin,sizeof(DesignButton.MessageButton.CaptionVerMargin));
      DatenStr.Write(DesignButton.MessageButton.Font.Color,sizeof(DesignButton.MessageButton.Font.Color));
     finally
      DatenStr.Free;
     end;
    end;
  finally
   SaveDialog.Free;
  end;





 end;

procedure TStyleManagerComponent.LoadProperties;
var OpenDialog : TOpenDialog;
    DatenStr   : TFileStream;
    aStyle     : TMButtonStyle;
    aColor     : TColor;
    aInteger   : integer;
    aBoolean   : boolean;
    aByte      : byte;
    aGradient  : TGradientCourse;
    aAlignment : TAlignment;
    aTextLayout: TTextLayout;
    mbAlignment: TMBAlignment;
    aButton    : TButton;
begin
 try
  OpenDialog    := TOpenDialog.Create(nil);
  //OpenDialog.Title  := rs_OpenTitel;
  OpenDialog.Filter := 'mbs Files|*.mbs';//rs_OpenFilter;

  if OpenDialog.Execute then
  begin
   try
    DatenStr := TFileStream.Create(OpenDialog.FileName, fmOpenRead);

    DatenStr.Read(aStyle{%H-}, SizeOf(aStyle));
    DesignButton.Style:= MultiButton.TMButtonStyle(MultiButtonStyleManager.TMButtonStyle(aStyle));
    DatenStr.Read(aInteger{%H-}, SizeOf(aInteger));
    DesignButton.Width:= aInteger;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.Height:= aInteger;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.RndRctRadius:= aInteger;
    DatenStr.Read(aBoolean{%H-}, SizeOf(aBoolean));
    DesignButton.FocusFrameON:= aBoolean;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.FocusFrameWidth:= aInteger;
    DatenStr.Read(aBoolean{%H-}, SizeOf(aBoolean));
    DesignButton.ForegroundFocusOn:= aBoolean;
    DatenStr.Read(aByte{%H-}, SizeOf(aByte));
    DesignButton.FocusAlphaBValue:= aByte;
    DatenStr.Read(aBoolean{%H-}, SizeOf(aBoolean));
    DesignButton.ShowBorder:= aBoolean;
    DatenStr.Read(aBoolean{%H-}, SizeOf(aBoolean));
    DesignButton.HoverOn:= aBoolean;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.BorderWidth:= aInteger;
    DatenStr.Read(aGradient{%H-}, SizeOf(aGradient));
    DesignButton.ColorGradient:= MultiButton.TGradientCourse(MultiButtonStyleManager.TGradientCourse(aGradient));
    DatenStr.Read(aColor{%H-}, SizeOf(aColor));
    DesignButton.ColorStart:= aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.ColorEnd:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.BorderColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.HoverStartColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.HoverEndColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.HoverFontColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.FocusColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.PressedStartColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.PressedEndColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.PressedFontColor:=aColor;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.CaptionHorMargin:= aInteger;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.CaptionVerMargin:= aInteger;
    DatenStr.Read(aBoolean, SizeOf(aBoolean));
    DesignButton.CaptionWordbreak:= aBoolean;
    DatenStr.Read(aAlignment{%H-}, SizeOf(aAlignment));
    DesignButton.CaptionAlignment:= aAlignment;
    DatenStr.Read(aTextLayout{%H-}, SizeOf(aTextLayout));
    DesignButton.CaptionLayout:= aTextLayout;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.Font.Color:=aColor;

    DatenStr.Read(aBoolean, SizeOf(aBoolean));
    DesignButton.MessageButton.Visible:= aBoolean;
    DatenStr.Read(aStyle, SizeOf(aStyle));
    DesignButton.MessageButton.Style:= MultiButton.TMButtonStyle(MultiButtonStyleManager.TMButtonStyle(aStyle));
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.MessageButton.Width:= aInteger;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.MessageButton.Height:= aInteger;
    DatenStr.Read(mbAlignment{%H-}, SizeOf(mbAlignment));
    DesignButton.MessageButton.Alignment:= MultiButton.TMBAlignment(MultiButtonStyleManager.TMButtonStyle(mbAlignment));
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.MessageButton.PositionFactor:= aInteger;
    DatenStr.Read(aBoolean, SizeOf(aBoolean));
    DesignButton.MessageButton.CalculateAlthoughInvisible:= aBoolean;
    DatenStr.Read(aBoolean, SizeOf(aBoolean));
    DesignButton.MessageButton.HoverOn:= aBoolean;
    DatenStr.Read(aBoolean, SizeOf(aBoolean));
    DesignButton.MessageButton.ShowPressed:= aBoolean;
    DatenStr.Read(aBoolean, SizeOf(aBoolean));
    DesignButton.MessageButton.ShowBorder:= aBoolean;
    DatenStr.Read(aByte, SizeOf(aByte));
    DesignButton.MessageButton.PresdColBlendVal:= aByte;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.MessageButton.BorderWidth:= aInteger;
    DatenStr.Read(aGradient, SizeOf(aGradient));
    DesignButton.MessageButton.ColorGradient:= MultiButton.TGradientCourse(MultiButtonStyleManager.TGradientCourse(aGradient));
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.MessageButton.ColorStart:= aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.MessageButton.ColorEnd:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.MessageButton.BorderColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.MessageButton.HoverColor:=aColor;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.MessageButton.PressedColor:=aColor;
    DatenStr.Read(aAlignment, SizeOf(aAlignment));
    DesignButton.MessageButton.CaptionAlignment:= aAlignment;
    DatenStr.Read(aTextLayout, SizeOf(aTextLayout));
    DesignButton.MessageButton.CaptionLayout:= aTextLayout;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.MessageButton.CaptionHorMargin:= aInteger;
    DatenStr.Read(aInteger, SizeOf(aInteger));
    DesignButton.MessageButton.CaptionVerMargin:= aInteger;
    DatenStr.Read(aColor, SizeOf(aColor));
    DesignButton.MessageButton.Font.Color:=aColor;
   finally
    DatenStr.Free;
   end;
  end;
 finally
  OpenDialog.Free;
 end;
 aButton      := TButton.Create(Editor);
 aButton.Tag  := 101;
 OK_AbortButtonOnClick(aButton);
end;









