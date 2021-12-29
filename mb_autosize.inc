(*This is a part of the TMultiButton*)

{%MultiButton ,this is a part of TMultiButton last change 08.08.2021}

function TMultiButton.GetTextWidth (AText : String ; AFont : TFont ) : Integer ;
var bmp : TBitmap ;
begin
 Result := 0 ;
 bmp := TBitmap.Create ;
 try
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextWidth(AText);
 finally
  bmp.Free;
 end;
end ;

function TMultiButton.GetTextHeight (AText : String ; AFont : TFont ) : Integer ;
var bmp : TBitmap ;
begin
 Result := 0 ;
 bmp := TBitmap.Create ;
 try
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextHeight(AText);
 finally
  bmp.Free;
 end;
end ;

//Only MultiButton Without Image

procedure TMultiButton.OnlyMultiButtonWithoutImage(var PreferredWidth,
  PreferredHeight: integer; spacer: integer);
begin
 PreferredWidth:= GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth);
 if FTextStyle.Alignment = taCenter       then PreferredWidth := PreferredWidth+(2*spacer);
 if FTextStyle.Alignment = taRightJustify then PreferredWidth := PreferredWidth+(2*spacer);
 if FTextStyle.Alignment = taLeftJustify  then PreferredWidth := PreferredWidth+FCapLeft+spacer;

 PreferredHeight:= GetTextHeight(FCaption,FFont)+(2*FFocusFrameWidth);
 if FTextStyle.Layout = tlCenter       then PreferredHeight := PreferredHeight+(2*spacer);
 if FTextStyle.Layout = tlBottom       then PreferredHeight := PreferredHeight+(2*spacer);
 if FTextStyle.Layout = tlTop          then PreferredHeight := PreferredHeight+FCapTop+spacer;
end;

//Only MultiButton With Image

procedure TMultiButton.OnlyMultiButtonWithImage(var PreferredWidth,
  PreferredHeight: integer; spacer: integer);
var  ImageVerCenter : integer;
     ImageHorCenter : integer;
begin
 if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
  begin
   ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
   ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));

   if ImageHorCenter < (width div 3) then IL:=true;
   if ImageHorCenter > ((width div 3)*2) then IR:=true;
   if (ImageHorCenter >= (width div 3)) and (ImageHorCenter <= ((width div 3)*2)) then
    if ImageVerCenter >= (height div 2) then IB:=true;
   if (ImageHorCenter >= (width div 3)) and (ImageHorCenter <= ((width div 3)*2)) then
    if ImageVerCenter < (height div 2) then IT :=true;
  end;

//Image Left
 if IL=true then
  begin
   FTextStyle.Alignment  := taLeftJustify;
   if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
   FTextStyle.Layout     := tlCenter;
   ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2) - FFocusFrameWidth ;
   ImageLeft := spacer;
   CaptionHorMargin  := (2*spacer)+ FImageList.Width;
   PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width;
  end;
 //Image Right
 if IR=true then
  begin
   FTextStyle.Alignment  := taLeftJustify;
   CaptionHorMargin := spacer;
   if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
   FTextStyle.Layout     := tlCenter;
   ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2) - FFocusFrameWidth;
   ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont);
   PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width;
  end;
 //Image Bottom
 if IB=true then
   begin
    FTextStyle.Alignment  := taCenter;
    PreferredWidth := (2*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth);
    if FImagelist.Width > GetTextWidth(FCaption,FFont) then
     PreferredWidth := (2*spacer)+FImagelist.Width+(2*FFocusFrameWidth);
    ImageLeft := (PreferredWidth div 2) - (FImagelist.Width div 2)-FFocusFrameWidth;

    ImageTop  := spacer+GetTextHeight(FCaption,FFont);
    FTextStyle.Layout := tlTop;
    CaptionVerMargin := (spacer);
    PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
   end;
 //Image Top
 if IT =true then
   begin
    FTextStyle.Alignment  := taCenter;
    PreferredWidth := (2*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth);
    if FImagelist.Width > GetTextWidth(FCaption,FFont) then
     PreferredWidth := (2*spacer)+FImagelist.Width+(2*FFocusFrameWidth);
    ImageLeft := (PreferredWidth div 2) - (FImagelist.Width div 2)-FFocusFrameWidth;

    ImageTop  := spacer;
    FTextStyle.Layout := tlTop;
    CaptionVerMargin := (spacer)+FImageList.Height;
    PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
   end;
end;

procedure TMultiButton.DetermineMessageButtonSize(spacer: integer);
begin
 if FMessageButton.Caption <> '' then
  begin
   FMessageButton.Width  := (2*spacer)+GetTextWidth(FMessageButton.FCaption,FMessageButton.FFont);
   FMessageButton.Height := (2*spacer)+GetTextHeight(FMessageButton.FCaption,FMessageButton.FFont);
   FMessageButton.FTextStyle.Alignment := taCenter;
   FMessageButton.FTextStyle.Layout    := tlCenter;
  end;

 if (MessageButton.Images <> nil) and (MessageButton.FImageIndex <> -1) then
  begin
   FMessageButton.Width     := (2*spacer)+FMessageButton.FImageList.Width;
   FMessageButton.Height    := (2*spacer)+FMessageButton.FImageList.Height;
   FMessageButton.FTextStyle.Layout    := tlTop;
   FMessageButton.FTextStyle.Alignment := taLeftJustify;
   FMessageButton.FImageLeft:= spacer;
   FMessageButton.FImageTop := spacer;
  end;

 if (FMessageButton.Caption <> '') and (MessageButton.FImageIndex <> -1) and (MessageButton.Images <> nil) then
  begin
   FMessageButton.Width := (3*spacer)+GetTextWidth(FMessageButton.FCaption,FMessageButton.FFont)+FMessageButton.FImageList.Width;
   FMessageButton.FTextStyle.Layout    := tlCenter;
   FMessageButton.FTextStyle.Alignment := taLeftJustify;
   FMessageButton.FImageLeft           := spacer;
   FMessageButton.FCapLeft             := (2*spacer)+FMessageButton.FImageList.Width;
   FMessageButton.Height := (2*spacer)+GetTextHeight(FMessageButton.FCaption,FMessageButton.FFont);
   if FMessageButton.Height < ((2*spacer)+FMessageButton.FImageList.Height) then
    FMessageButton.Height    := (2*spacer)+FMessageButton.FImageList.Height;
   FMessageButton.FImageTop := (FMessageButton.Height div 2) - (FMessageButton.FImageList.Height div 2);
  end;

 if FMessageButton.FStyle = mbsCircle then FMessageButton.Height:=FMessageButton.Width;
end;


//MultiButton Without Image + MessageButton

procedure TMultiButton.MultiButtonWithoutImageWithMessageButton(
  var PreferredWidth, PreferredHeight: integer; spacer, PF: integer);
begin
 if (FMessageButton.FAlignment = alE) then
  begin
   FTextStyle.Alignment := taLeftJustify;
   FCapLeft:= spacer;
   FTextStyle.Layout    := tlCenter;
   PreferredWidth := (GetTextWidth(FCaption,FFont) + FMessageButton.FWidth+(2*FFocusFrameWidth)+(3*spacer));
   if PreferredHeight < (FMessageButton.Height+(2*FFocusFrameWidth)) then
      PreferredHeight := FMessageButton.Height+(2*FFocusFrameWidth);
  end;
 if (FMessageButton.FAlignment = alW) then
  begin
   FTextStyle.Alignment := taLeftJustify;
   FTextStyle.Layout    := tlCenter;
   FCapLeft:= (FMessageButton.FWidth-(FMessageButton.FWidth div PF));
   PreferredWidth := (GetTextWidth(FCaption,FFont) + FMessageButton.FWidth)+(2*FFocusFrameWidth)+(2*spacer);
   if PreferredHeight < (FMessageButton.Height+(2*FFocusFrameWidth)) then
      PreferredHeight := FMessageButton.Height+(2*FFocusFrameWidth);

  end;
 if (FMessageButton.FAlignment = alN) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlTop;
   FCapTop := (FMessageButton.FHeight-(FMessageButton.FHeight div PF));
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);

   if PreferredWidth < (FMessageButton.Width+(2*FFocusFrameWidth)) then
    PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
  end;
 if (FMessageButton.FAlignment = alS) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlTop;
   FCapTop := spacer;
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   if PreferredWidth < (FMessageButton.Width+(2*FFocusFrameWidth)) then
    PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
  end;
 if (FMessageButton.FAlignment = alNE) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlTop;
   FCapTop := (FMessageButton.FHeight-(FMessageButton.FHeight div PF));
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+spacer+
                     FMessageButton.FWidth-(FMessageButton.FWidth div PF);
  end;
 if (FMessageButton.FAlignment = alSE) then
  begin
   FTextStyle.Alignment := taCenter;
   PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+spacer+
                     FMessageButton.FWidth-(FMessageButton.FWidth div PF);

   FTextStyle.Layout    := tlTop;
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   FCapTop := spacer;

  end;
 if (FMessageButton.FAlignment = alNW) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlTop;
   FCapTop := (FMessageButton.FHeight-(FMessageButton.FHeight div PF));

   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+spacer+
                     FMessageButton.FWidth-(FMessageButton.FWidth div PF);
  end;
 if (FMessageButton.FAlignment = alSW) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlTop;
   FCapTop := spacer;
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+spacer+
                     FMessageButton.FWidth-(FMessageButton.FWidth div PF);
  end;
 if (FMessageButton.FAlignment = alBottomOut) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlCenter;
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   if FMessageButton.Width < PreferredWidth then FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);
   if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
   if (FMessageButton.Images <> nil) and (FMessageButton.FImageIndex <> -1) and (FMessageButton.FCaption = '') then
    FMessageButton.FImageLeft:= (FMessageButton.FWidth div 2) - (FMessageButton.FImageList.Width div 2);
  end;
 if (FMessageButton.FAlignment = alTopOut) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlCenter;
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   if FMessageButton.Width < PreferredWidth then FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);
   if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
   if (FMessageButton.Images <> nil) and (FMessageButton.FImageIndex <> -1) and (FMessageButton.FCaption = '') then
    FMessageButton.FImageLeft:= (FMessageButton.FWidth div 2) - (FMessageButton.FImageList.Width div 2);
  end;
 if (FMessageButton.FAlignment = alLeftOut) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlCenter;
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ GetTextHeight(FCaption,FFont);
   if FMessageButton.Height < PreferredHeight then FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);
   if PreferredHeight < FMessageButton.Height then PreferredHeight := FMessageButton.Height+(2*FFocusFrameWidth);
   PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth)+(2*spacer)+GetTextWidth(FCaption,FFont);
   if (FMessageButton.Images <> nil) and (FMessageButton.FImageIndex <> -1) and (FMessageButton.FCaption = '') then
    FMessageButton.FImageTop:= (FMessageButton.FHeight div 2) - (FMessageButton.FImageList.Height div 2);
  end;
 if (FMessageButton.FAlignment = alRightOut) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlCenter;

   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ GetTextHeight(FCaption,FFont);
   if FMessageButton.Height < PreferredHeight then FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);
   if PreferredHeight < FMessageButton.Height then PreferredHeight := FMessageButton.Height+(2*FFocusFrameWidth);
   PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth)+(2*spacer)+GetTextWidth(FCaption,FFont);
   if (FMessageButton.Images <> nil) and (FMessageButton.FImageIndex <> -1) and (FMessageButton.FCaption = '') then
    FMessageButton.FImageTop:= (FMessageButton.FHeight div 2) - (FMessageButton.FImageList.Height div 2);
  end;
 if (FMessageButton.FAlignment = alBottomIn) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlTop;
   FCapTop := spacer;
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
  end;
 if (FMessageButton.FAlignment = alTopIn) then
  begin
   FTextStyle.Alignment := taCenter;
   FTextStyle.Layout    := tlTop;
   FCapTop := spacer+FMessageButton.Height;
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
   if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
  end;
 if (FMessageButton.FAlignment = alLeftIn) then
  begin
   FTextStyle.Alignment := taLeftJustify;
   FTextStyle.Layout    := tlCenter;
   FCapLeft:= FMessageButton.FWidth+spacer;
   PreferredWidth := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FWidth+GetTextWidth(FCaption,FFont);
   if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+GetTextHeight(FCaption,FFont);
   if PreferredHeight < FMessageButton.Height +(2*FFocusFrameWidth) then
    PreferredHeight := FMessageButton.Height+(2*FFocusFrameWidth);
  end;
 if (FMessageButton.FAlignment = alRightIn) then
  begin
   FTextStyle.Alignment := taLeftJustify;
   FTextStyle.Layout    := tlCenter;
   FCapLeft:= spacer;
   PreferredWidth := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FWidth+GetTextWidth(FCaption,FFont);
   if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
   PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+GetTextHeight(FCaption,FFont);
   if PreferredHeight < FMessageButton.Height +(2*FFocusFrameWidth) then
    PreferredHeight := FMessageButton.Height+(2*FFocusFrameWidth);
  end;

end;



//MultiButton With Image + MessageButton

procedure TMultiButton.MultiButtonWithImageWithMessageButton(
  var PreferredWidth, PreferredHeight: integer; spacer, PF: integer);
var  ImageVerCenter : integer;
     ImageHorCenter : integer;
begin
//alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE___alE
 if (FMessageButton.FAlignment = alE) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));

     if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;

     if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;

     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter >= (height div 2) then IB:=true;

     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := spacer;
     CaptionHorMargin  := (2*spacer)+ FImageList.Width;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     CaptionHorMargin := spacer;
     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;

     FTextStyle.Alignment  := taLeftJustify;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := (((2*spacer)+GetTextWidth(FCaption,FFont)) div 2) - (FImagelist.Width div 2);
     if FImagelist.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImagelist.Width+(1*FFocusFrameWidth)+
                        FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin :=  spacer+ (FImagelist.Width div 2) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer+GetTextHeight(FCaption,FFont);
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (spacer);
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := (((2*spacer)+GetTextWidth(FCaption,FFont)) div 2) - (FImagelist.Width div 2);
     if FImagelist.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImagelist.Width+(1*FFocusFrameWidth)+
                        FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin :=  spacer+ (FImagelist.Width div 2) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer;
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FImageList.Height;
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
    end;
  end;
//alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW___alW
 if (FMessageButton.FAlignment = alW) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;
     if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
     and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
     if ImageVerCenter >= (height div 2) then IB:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
     and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
     if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     PreferredHeight:= GetTextHeight(FCaption,FFont)+(2*FFocusFrameWidth)+ (2*spacer);
     if FImageList.Height > GetTextHeight(FCaption,FFont) then PreferredHeight:=FImageList.Height+(2*FFocusFrameWidth)+ (2*spacer);
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := FMessageButton.Width - FFocusFrameWidth ;
     CaptionHorMargin  := FMessageButton.Width + (spacer)+ FImageList.Width- FFocusFrameWidth;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont)+FMessageButton.Width- (FMessageButton.FWidth div PF);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+
                        FMessageButton.Width;
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));
     if FImagelist.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImagelist.Width+(1*FFocusFrameWidth)+
                        FMessageButton.Width;
       ImageLeft := FMessageButton.Width- (FMessageButton.FWidth div PF);
       CaptionHorMargin :=  (ImageLeft+ (FImagelist.Width div 2))-(GetTextWidth(FCaption,FFont) div 2);
      end;

     FTextStyle.Layout := tlTop;
     ImageTop  := spacer+GetTextHeight(FCaption,FFont);
     CaptionVerMargin := (spacer);
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));
     if FImagelist.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImagelist.Width+(1*FFocusFrameWidth)+
                        FMessageButton.Width;
       ImageLeft := FMessageButton.Width- (FMessageButton.FWidth div PF);
       CaptionHorMargin :=  (ImageLeft+ (FImagelist.Width div 2))-(GetTextWidth(FCaption,FFont) div 2);
      end;

     FTextStyle.Layout := tlTop;
     ImageTop  := spacer;
     CaptionVerMargin := FImageList.Height;
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
    end;
  end;
//alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN___alN
 if (FMessageButton.FAlignment = alN) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width ) div 3) then IL:=true;
     if ImageHorCenter > (((Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width) div 3)*2)) then
      if ImageVerCenter >= (height div 2) then IB:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width ) div 3)*2)) then
      if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := (PreferredHeight div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment := taLeftJustify;
     ImageLeft := spacer ;
     CaptionHorMargin  := (2*spacer)+ FImageList.Width;
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := (PreferredHeight div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment := taLeftJustify;
     CaptionHorMargin  := spacer;
     ImageLeft :=  (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;

     FTextStyle.Layout := tlTop;

     CaptionVerMargin := spacer + FMessageButton.Height-FFocusFrameWidth;
     ImageTop  := FMessageButton.Height+(2*spacer)+GetTextHeight(FCaption,FFont)-FFocusFrameWidth;

     PreferredHeight := FMessageButton.Height+(4*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FMessageButton.Height+(2*spacer)+FImageList.Height -FFocusFrameWidth;
     ImageTop  := spacer +FMessageButton.Height-FFocusFrameWidth;
     PreferredHeight := FMessageButton.Height+(4*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
 end;
//alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS___alS
 if (FMessageButton.FAlignment = alS) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width ) div 3) then IL:=true;
     if ImageHorCenter > (((Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width) div 3)*2)) then
      if ImageVerCenter >= ((height - FMessageButton.Height) div 2) then IB:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width ) div 3)*2)) then
      if ImageVerCenter < ((height - FMessageButton.Height) div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((PreferredHeight-FMessageButton.Height) div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := ((PreferredHeight-FMessageButton.Height) div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment := taLeftJustify;
     ImageLeft := spacer ;
     CaptionHorMargin  := spacer+ FImageList.Width;
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((PreferredHeight-FMessageButton.Height) div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := ((PreferredHeight-FMessageButton.Height) div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment := taLeftJustify;
     CaptionHorMargin  := spacer;
     ImageLeft :=  (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := spacer;
     ImageTop  := (2*spacer)+GetTextHeight(FCaption,FFont);
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (2*spacer)+FImageList.Height;
     ImageTop  := spacer;
     PreferredHeight := FMessageButton.Height+(4*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
  end;
//alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE___alNE
 if (FMessageButton.FAlignment = alNE) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;
     if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter >= (height div 2) then IB:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := (PreferredHeight div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := spacer;
     CaptionHorMargin  := (2*spacer)+ FImageList.Width;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := (PreferredHeight div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     CaptionHorMargin := spacer;
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont)+(1*FFocusFrameWidth);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := (((2*spacer)+GetTextWidth(FCaption,FFont)) div 2) - (FImagelist.Width div 2)+FFocusFrameWidth;
     if FImagelist.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (2*spacer)+FImagelist.Width+(1*FFocusFrameWidth)+
                        FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := (FImagelist.Width div 2) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FMessageButton.Height-FFocusFrameWidth;
     ImageTop  := FMessageButton.Height+spacer+GetTextHeight(FCaption,FFont)-FFocusFrameWidth;
     PreferredHeight := FMessageButton.Height+(4*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := (((2*spacer)+GetTextWidth(FCaption,FFont)) div 2) - (FImagelist.Width div 2)+FFocusFrameWidth;
     if FImagelist.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (2*spacer)+FImagelist.Width+(1*FFocusFrameWidth)+
                        FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := (FImagelist.Width div 2) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FMessageButton.Height+spacer++FImageList.Height -FFocusFrameWidth;
     ImageTop  := FMessageButton.Height-FFocusFrameWidth;
     PreferredHeight := FMessageButton.Height+(4*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
 end;
//alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW___alNW
 if (FMessageButton.FAlignment = alNW) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;
     if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter >= (height div 2) then IB:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := (PreferredHeight div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := FMessageButton.Width -(FMessageButton.Width div pf);
     CaptionHorMargin  := FMessageButton.Width +spacer+ FImageList.Width- (FMessageButton.Width div pf);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width
                        +FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := (PreferredHeight div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont)+FMessageButton.Width- (FMessageButton.FWidth div PF);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+
                        FMessageButton.Width;
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FMessageButton.Height + spacer - (FMessageButton.Width div pf)-FFocusFrameWidth;
     ImageTop  := CaptionVerMargin+spacer+GetTextHeight(FCaption,FFont);
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));

     FTextStyle.Layout := tlTop;
     ImageTop  := FMessageButton.Height + spacer - (FMessageButton.Width div pf)-FFocusFrameWidth;
     CaptionVerMargin := ImageTop+spacer +FImageList.Height;
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
  end;
//alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW___alSW
 if (FMessageButton.FAlignment = alSW) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
      if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;
      if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;
      if (ImageHorCenter >= ((Width) div 3))
       and (ImageHorCenter <= (((Width) div 3)*2)) then
       if ImageVerCenter >= ((height - FMessageButton.Height) div 2) then IB:=true;
      if (ImageHorCenter >= ((Width) div 3))
       and (ImageHorCenter <= (((Width ) div 3)*2)) then
       if ImageVerCenter < ((height - FMessageButton.Height) div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((PreferredHeight-FMessageButton.Height) div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := ((PreferredHeight-FMessageButton.Height) div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := FMessageButton.Width -(FMessageButton.Width div pf);
     CaptionHorMargin  := FMessageButton.Width +spacer+ FImageList.Width- (FMessageButton.Width div pf);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width
                        +FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((PreferredHeight-FMessageButton.Height) div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := ((PreferredHeight-FMessageButton.Height) div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont)+FMessageButton.Width- (FMessageButton.FWidth div PF);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+
                        FMessageButton.Width;
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := spacer;
     ImageTop  := (2*spacer)+GetTextHeight(FCaption,FFont);
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := FMessageButton.Width- (FMessageButton.FWidth div PF);
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := spacer+FImageList.Height;
     ImageTop  := spacer;
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
  end;
//alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE___alSE
 if (FMessageButton.FAlignment = alSE) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2))+FFocusFrameWidth;
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter <= ((Width - FMessageButton.Width) div 3) then IL:=true;
     if ImageHorCenter >= (((Width - FMessageButton.Width) div 3)*2) then IR:=true;

     if (ImageHorCenter > ((Width- FMessageButton.Width) div 3))
      and (ImageHorCenter < (((Width- FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter > ((height - FMessageButton.Height) div 2) then IB:=true;

     if (ImageHorCenter > ((Width- FMessageButton.Width) div 3))
      and (ImageHorCenter < (((Width- FMessageButton.Width ) div 3)*2)) then
      if ImageVerCenter < ((height - FMessageButton.Height) div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := (PreferredHeight div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := spacer;
     CaptionHorMargin  := (2*spacer)+ FImageList.Width;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := (PreferredHeight div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     CaptionHorMargin := spacer;
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := (((2*spacer)+GetTextWidth(FCaption,FFont)) div 2) - (FImagelist.Width div 2);

     if FImagelist.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (2*spacer)+FImagelist.Width+(1*FFocusFrameWidth)+
                        FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := (FImagelist.Width div 2) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := spacer;
     ImageTop  := (2*spacer)+GetTextHeight(FCaption,FFont);
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
      CaptionHorMargin := spacer;
      ImageLeft := (((2*spacer)+GetTextWidth(FCaption,FFont)) div 2) - (FImagelist.Width div 2);

      if FImagelist.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (2*spacer)+FImagelist.Width+(1*FFocusFrameWidth)+
                        FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := (FImagelist.Width div 2) - (GetTextWidth(FCaption,FFont) div 2);
      end;

      FTextStyle.Layout := tlTop;
      CaptionVerMargin := (2*spacer)+FImageList.Height;
      ImageTop  := spacer;
      PreferredHeight := FMessageButton.Height+(4*spacer)
                         +GetTextHeight(FCaption,FFont)+FImageList.Height+FFocusFrameWidth;
     end;
  end;
   //end;
//alBottomIn___alBottomIn___alBottomIn___alBottomIn___alBottomIn___alBottomIn___alBottomIn___alBottomIn
 if (FMessageButton.FAlignment = alBottomIn) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width ) div 3) then IL:=true;
     if ImageHorCenter > (((Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width) div 3)*2)) then
      if ImageVerCenter >= (((height - FMessageButton.Height-(2*FocusFramewidth)) div 2)) then IB:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width ) div 3)*2)) then
      if ImageVerCenter < (((height - FMessageButton.Height-(2*FocusFramewidth)) div 2)) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(3*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(3*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((PreferredHeight-FMessageButton.Height) div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := ((PreferredHeight-FMessageButton.Height) div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment := taLeftJustify;
     ImageLeft := spacer ;
     CaptionHorMargin  := spacer+ FImageList.Width;
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(3*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(3*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((PreferredHeight-FMessageButton.Height) div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := ((PreferredHeight-FMessageButton.Height) div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment := taLeftJustify;
     CaptionHorMargin  := spacer;
     ImageLeft :=  (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := spacer;
     ImageTop  := (2*spacer)+GetTextHeight(FCaption,FFont);
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+(2*FFocusFrameWidth);
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (2*spacer)+FImageList.Height;
     ImageTop  := spacer;
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+(2*FFocusFrameWidth);
    end;
 end;
//alTopIn___alTopIn___alTopIn___alTopIn___alTopIn___alTopIn___alTopIn___alTopIn___alTopIn___alTopIn___alTopIn___
 if (FMessageButton.FAlignment = alTopIn) then
 begin
  if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
   begin
    ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
    ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
    if ImageHorCenter < ((Width ) div 3) then IL:=true;
    if ImageHorCenter > (((Width) div 3)*2) then IR:=true;
    if (ImageHorCenter >= ((Width) div 3))
     and (ImageHorCenter <= (((Width) div 3)*2)) then
     if ImageVerCenter >= (((height - FMessageButton.Height-(2*FocusFramewidth)) div 2)+
       FMessageButton.Height+FocusFramewidth) then IB:=true;
    if (ImageHorCenter >= ((Width) div 3))
     and (ImageHorCenter <= (((Width ) div 3)*2)) then
     if ImageVerCenter < (((height - FMessageButton.Height-(2*FocusFramewidth)) div 2)+
       FMessageButton.Height+FocusFramewidth) then IT :=true;
   end;
 //Image Left
  if IL then
   begin
    FTextStyle.Layout    := tlTop;
    PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
    if GetTextHeight(FCaption,FFont) < FImageList.Height then
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
    ImageTop  := (((PreferredHeight-FMessageButton.Height-FFocusFrameWidth )div 2)
                 - (FImageList.Height div 2)-FFocusFrameWidth)+FMessageButton.Height;
    FCapTop :=  (((PreferredHeight-FMessageButton.Height-FFocusFrameWidth )div 2)
                 - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth)+FMessageButton.Height;

    FTextStyle.Alignment := taLeftJustify;
    ImageLeft := spacer ;
    CaptionHorMargin  := spacer+ FImageList.Width;
    PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
    if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
   end;
 //Image Right
  if IR then
   begin
    FTextStyle.Layout    := tlTop;
    PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
    if GetTextHeight(FCaption,FFont) < FImageList.Height then
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
    ImageTop  := (((PreferredHeight-FMessageButton.Height-FFocusFrameWidth )div 2)
                 - (FImageList.Height div 2)-FFocusFrameWidth)+FMessageButton.Height;
    FCapTop :=  (((PreferredHeight-FMessageButton.Height-FFocusFrameWidth )div 2)
                 - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth)+FMessageButton.Height;

    FTextStyle.Alignment := taLeftJustify;
    CaptionHorMargin  := spacer;
    ImageLeft :=  (2+spacer)+GetTextWidth(FCaption,FFont);
    PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
    if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
   end;
 //Image Bottom
  if IB then
   begin
    FTextStyle.Alignment  := taLeftJustify;
    PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
    if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
    CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
    ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;

    FTextStyle.Layout := tlTop;
    CaptionVerMargin := FMessageButton.Height+spacer;
    ImageTop  := FMessageButton.Height+(2*spacer)+GetTextHeight(FCaption,FFont);
    PreferredHeight := FMessageButton.Height+(4*spacer)
                       +GetTextHeight(FCaption,FFont)+FImageList.Height+(2*FFocusFrameWidth);
   end;
 //Image Top
  if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FMessageButton.Height+(2+spacer)+FImageList.Height;
     ImageTop  := FMessageButton.Height+spacer;
     PreferredHeight := FMessageButton.Height+(4*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+(2*FFocusFrameWidth);
    end;
 end;
//alLeftIn___alLeftIn___alLeftIn___alLeftIn___alLeftIn___alLeftIn___alLeftIn___alLeftIn___alLeftIn___alLeftIn
 if (FMessageButton.FAlignment = alLeftIn) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;
     if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter >= (height div 2) then IB:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     PreferredHeight:= GetTextHeight(FCaption,FFont)+(2*FFocusFrameWidth)+ (2*spacer);
     if FImageList.Height > GetTextHeight(FCaption,FFont) then PreferredHeight:=FImageList.Height+(2*FFocusFrameWidth)+ (2*spacer);
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := FMessageButton.Width+spacer;
     CaptionHorMargin  := FMessageButton.Width + (2*spacer)+ FImageList.Width;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     CaptionHorMargin := FMessageButton.Width+spacer;
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont)+FMessageButton.Width;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+
                        FMessageButton.Width;

     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := FMessageButton.Width+(spacer);
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));
     if FImageList.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImageList.Width+(2*FFocusFrameWidth)+FMessageButton.Width;
       ImageLeft := FMessageButton.Width+spacer;
       CaptionHorMargin := FMessageButton.Width+((spacer)+(FImagelist.Width div 2)) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer+GetTextHeight(FCaption,FFont);
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (spacer);
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := FMessageButton.Width+spacer;
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));
     if FImageList.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImageList.Width+(2*FFocusFrameWidth)+FMessageButton.Width;
       ImageLeft := FMessageButton.Width+spacer;
       CaptionHorMargin := FMessageButton.Width+((spacer)+(FImagelist.Width div 2)) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer;
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FImageList.Height;
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
    end;
  end;
//alRightIN___alRightIN___alRightIN___alRightIN___alRightIN___alRightIN___alRightIN___alRightIN___alRightIN
 if (FMessageButton.FAlignment = alRightIn) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;
     if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter >= (height div 2) then IB:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := spacer;
     CaptionHorMargin  := (2*spacer)+ FImageList.Width;
     PreferredWidth := (4*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     CaptionHorMargin := spacer;
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth := (4*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;

     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := (((2*spacer)+GetTextWidth(FCaption,FFont)) div 2) - (FImagelist.Width div 2);
     if FImageList.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImageList.Width+(2*FFocusFrameWidth)+FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := ((spacer)+(FImagelist.Width div 2)) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer+GetTextHeight(FCaption,FFont);
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (spacer);
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := (((2*spacer)+GetTextWidth(FCaption,FFont)) div 2) - (FImagelist.Width div 2);
     if FImageList.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImageList.Width+(2*FFocusFrameWidth)+FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := ((spacer)+(FImagelist.Width div 2)) - (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer;
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FImageList.Height;
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height;
    end;
  end;
//alBottomOut___alBottomOut___alBottomOut___alBottomOut___alBottomOut___alBottomOut___alBottomOut___alBottomOut
 if (FMessageButton.FAlignment = alBottomOut) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width ) div 3) then IL:=true;
     if ImageHorCenter > (((Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width) div 3)*2)) then
      if ImageVerCenter >= (((height - FMessageButton.Height-(2*FocusFramewidth)) div 2)) then IB:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width ) div 3)*2)) then
      if ImageVerCenter < (((height - FMessageButton.Height-(2*FocusFramewidth)) div 2)) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(3*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(3*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((PreferredHeight-FMessageButton.Height) div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := ((PreferredHeight-FMessageButton.Height) div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment := taLeftJustify;
     ImageLeft := spacer ;
     CaptionHorMargin  := spacer+ FImageList.Width;
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(3*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(3*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((PreferredHeight-FMessageButton.Height) div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FCapTop := ((PreferredHeight-FMessageButton.Height) div 2) - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth;

     FTextStyle.Alignment := taLeftJustify;
     CaptionHorMargin  := spacer;
     ImageLeft :=  (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;
     FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := spacer;
     ImageTop  := (2*spacer)+GetTextHeight(FCaption,FFont);
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+(2*FFocusFrameWidth);
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;
     FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (2*spacer)+FImageList.Height;
     ImageTop  := spacer;
     PreferredHeight := FMessageButton.Height+(3*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+(2*FFocusFrameWidth);
    end;
  end;
//alTopOut___alTopOut___alTopOut___alTopOut___alTopOut___alTopOut___alTopOut___alTopOut___alTopOut___alTopOut
 if (FMessageButton.FAlignment = alTopOut) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width ) div 3) then IL:=true;
     if ImageHorCenter > (((Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width) div 3)*2)) then
      if ImageVerCenter >= (((height - FMessageButton.Height-(2*FocusFramewidth)) div 2)+
      FMessageButton.Height+FocusFramewidth)-FMessageButton.Height then IB:=true;
     if (ImageHorCenter >= ((Width) div 3))
      and (ImageHorCenter <= (((Width ) div 3)*2)) then
      if ImageVerCenter < (((height - FMessageButton.Height-(2*FocusFramewidth)) div 2)+
      FMessageButton.Height+FocusFramewidth)-FMessageButton.Height then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := ((((PreferredHeight-FMessageButton.Height)div 2)
                   - (FImageList.Height div 2)-FFocusFrameWidth));
     FCapTop :=  (((PreferredHeight-FMessageButton.Height)div 2)
                   - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth);

     FTextStyle.Alignment := taLeftJustify;
     ImageLeft := spacer ;
     CaptionHorMargin  := spacer+ FImageList.Width;
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
     FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Layout    := tlTop;
     PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+GetTextHeight(FCaption,FFont);
     if GetTextHeight(FCaption,FFont) < FImageList.Height then
      PreferredHeight := (2*FFocusFrameWidth)+(2*spacer)+ FMessageButton.FHeight+FImageList.Height;
     ImageTop  := (((PreferredHeight-FMessageButton.Height )div 2)
                  - (FImageList.Height div 2)-FFocusFrameWidth);
     FCapTop :=  (((PreferredHeight-FMessageButton.Height )div 2)
                  - (GetTextHeight(FCaption,FFont) div 2)-FFocusFrameWidth);

     FTextStyle.Alignment := taLeftJustify;
     CaptionHorMargin  := spacer;
     ImageLeft :=  (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth :=(2*FFocusFrameWidth)+(3*spacer)+GetTextWidth(FCaption,FFont)+FImageList.Width;
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*spacer);
     FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;
     FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := spacer;
     ImageTop  := (2*spacer)+GetTextHeight(FCaption,FFont);
     PreferredHeight := FMessageButton.Height+(4*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+(2*FFocusFrameWidth);
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2+spacer);
     if PreferredWidth < FMessageButton.Width then PreferredWidth := FMessageButton.Width+(2*FFocusFrameWidth);
     CaptionHorMargin := ((PreferredWidth div 2) - (GetTextWidth(FCaption,FFont) div 2))-FFocusFrameWidth;
     ImageLeft := ((PreferredWidth div 2) - (FImagelist.Width div 2))-FFocusFrameWidth;
     FMessageButton.Width := PreferredWidth - (2*FFocusFrameWidth);

     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (2+spacer)+FImageList.Height;
     ImageTop  := spacer;
     PreferredHeight := FMessageButton.Height+(4*spacer)
                        +GetTextHeight(FCaption,FFont)+FImageList.Height+(2*FFocusFrameWidth);
    end;
  end;
//alLeftOut___alLeftOut___alLeftOut___alLeftOut___alLeftOut___alLeftOut___alLeftOut___alLeftOut___alLeftOut
 if (FMessageButton.FAlignment = alLeftOut) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;
     if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter >= (height div 2) then IB:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     PreferredHeight:= GetTextHeight(FCaption,FFont)+(2*FFocusFrameWidth)+ (2*spacer);
     if FImageList.Height > GetTextHeight(FCaption,FFont) then PreferredHeight:=FImageList.Height+(2*FFocusFrameWidth)+ (2*spacer);
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := spacer;
     CaptionHorMargin  := (2*spacer)+ FImageList.Width;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     CaptionHorMargin := spacer;
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+
                        FMessageButton.Width;

     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));
     if FImageList.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImageList.Width+(2*FFocusFrameWidth)+FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := (ImageLeft+(FImagelist.Width div 2))- (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer+GetTextHeight(FCaption,FFont);
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (spacer);
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height-1;
     FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));
     if FImageList.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImageList.Width+(2*FFocusFrameWidth)+FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := (ImageLeft+(FImagelist.Width div 2))- (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer;
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FImageList.Height;
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height-1;
     FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);
    end;
  end;
//alRightOut___alRightOut___alRightOut___alRightOut___alRightOut___alRightOut___alRightOut___alRightOut___alRightOut
 if (FMessageButton.FAlignment = alRightOut) then
  begin
   if (IL=false) and (IR=False) and (IB=false) and (IT=False) then
    begin
     ImageHorCenter := ((FImageLeft) + (FImageList.Width div 2));
     ImageVerCenter := ((FImageTop) + (FImageList.Height div 2));
     if ImageHorCenter < ((Width - FMessageButton.Width) div 3) then IL:=true;
     if ImageHorCenter > (((Width - FMessageButton.Width) div 3)*2) then IR:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter >= (height div 2) then IB:=true;
     if (ImageHorCenter >= ((Width - FMessageButton.Width) div 3))
      and (ImageHorCenter <= (((Width - FMessageButton.Width) div 3)*2)) then
      if ImageVerCenter < (height div 2) then IT :=true;
    end;
 //Image Left
   if IL then
    begin
     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     PreferredHeight:= GetTextHeight(FCaption,FFont)+(2*FFocusFrameWidth)+ (2*spacer);
     if FImageList.Height > GetTextHeight(FCaption,FFont) then PreferredHeight:=FImageList.Height+(2*FFocusFrameWidth)+ (2*spacer);
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);

     FTextStyle.Alignment  := taLeftJustify;
     ImageLeft := spacer;
     CaptionHorMargin  := (2*spacer)+ FImageList.Width;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+FMessageButton.Width;
    end;
 //Image Right
   if IR then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     CaptionHorMargin := spacer;
     ImageLeft := (2+spacer)+GetTextWidth(FCaption,FFont);
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+FImageList.Width+
                        FMessageButton.Width;

     if PreferredHeight < FImageList.Height then PreferredHeight:= FImageList.Height + (2*spacer)+(2*FFocusFrameWidth);
     FTextStyle.Layout     := tlCenter;
     ImageTop  := (PreferredHeight div 2) - (FImageList.Height div 2)-FFocusFrameWidth;
     FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);
    end;
 //Image Bottom
   if IB then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));
     if FImageList.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImageList.Width+(2*FFocusFrameWidth)+FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := (ImageLeft+(FImagelist.Width div 2))- (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer+GetTextHeight(FCaption,FFont);
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := (spacer);
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height-1;
     FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);
    end;
 //Image Top
   if IT then
    begin
     FTextStyle.Alignment  := taLeftJustify;
     PreferredWidth := (3*spacer)+GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+
                        FMessageButton.Width;
     CaptionHorMargin := spacer;
     ImageLeft := CaptionHorMargin+ ((GetTextWidth(FCaption,FFont) div 2) - (FImagelist.Width div 2));
     if FImageList.Width > GetTextWidth(FCaption,FFont) then
      begin
       PreferredWidth := (3*spacer)+FImageList.Width+(2*FFocusFrameWidth)+FMessageButton.Width;
       ImageLeft := spacer;
       CaptionHorMargin := (ImageLeft+(FImagelist.Width div 2))- (GetTextWidth(FCaption,FFont) div 2);
      end;

     ImageTop  := spacer;
     FTextStyle.Layout := tlTop;
     CaptionVerMargin := FImageList.Height;
     PreferredHeight := (3*spacer)+(2*FFocusFrameWidth)+GetTextHeight(FCaption,FFont)+FImageList.Height-1;
     FMessageButton.Height := PreferredHeight - (2*FFocusFrameWidth);
    end;
  end;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Vertical Caption---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiButton.OnlyMultiButtonWithoutImage900(var PreferredWidth,
  PreferredHeight: integer; spacer: integer);
begin
  FCaptionWordbreak:=false;

  PreferredWidth:= GetTextHeight(FCaption,FFont)+(2*FFocusFrameWidth)+(2*spacer);
  FTextStyle.Alignment := taLeftJustify;
  CaptionHorMargin:= spacer;
  if FFont.Orientation = 2700 then CaptionHorMargin := GetTextHeight(FCaption,FFont)+spacer;
  if FFont.Orientation = -900 then CaptionHorMargin := GetTextHeight(FCaption,FFont)+spacer;

  PreferredHeight:= GetTextWidth(FCaption,FFont)+(2*FFocusFrameWidth)+(2*spacer);
  FTextStyle.Layout := tlTop;
  CaptionVerMargin := GetTextWidth(FCaption,FFont)+spacer;
  if FFont.Orientation = 2700 then CaptionVerMargin := spacer;
  if FFont.Orientation = -900 then CaptionVerMargin := spacer;
end;




