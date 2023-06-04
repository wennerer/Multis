{%MultiEventLine}


{ TInfoBox }

constructor TInfoBox.create(AOwner: TMultiEvent);
begin
 inherited create;
 FOwner               := aOwner;
 FInfoBoxPosition     := ibNone;
 FWidth               := 80;
 FHeight              := 20;
 FHorizCorrection     := 0;
 FVertCorrection      := 5;
 FCapLeft             := 2;
 FCapTop              := 0;
 FTextStyle.Layout    := tlCenter;
 FTextStyle.Alignment := taLeftJustify;
 FCaptionWordbreak    := false;
 FTextStyle.Clipping  := true;
 FColor               := clWhite;
 FBorderColor         := clNone;
 FBorderWidth         := 1;
 FStyle               := ibsRectangle;
 FRRRadius            := 5;
 FFont                := TFont.Create;
 FPositionCanged      := false;

end;

destructor TInfoBox.Destroy;
begin
 FFont.Free;
 inherited Destroy;
end;


//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Setter---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TInfoBox.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
 FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetCaptionWordbreak(AValue: boolean);
begin
 if FCaptionWordbreak=AValue then Exit;
  FCaptionWordbreak:=AValue;

 if not  FCaptionWordbreak then
    begin
     FTextStyle.SingleLine:= true;
     FTextStyle.Wordbreak := false;
    end else
    begin
     FTextStyle.SingleLine:= false;
     FTextStyle.Wordbreak := true;
    end;
 FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont.Assign(AValue);
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetHorizCorrection(AValue: integer);
begin
  if FHorizCorrection=AValue then Exit;
  FHorizCorrection:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetInfoBoxPosition(AValue: TInfoBoxPosition);
begin
  if FInfoBoxPosition=AValue then Exit;
  FInfoBoxPosition:=AValue;
  FPositionCanged := true;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
 FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetStyle(AValue: TInfoBoxStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetTextStyle(AValue: TTextStyle);
begin
 FTextStyle:=AValue;
 FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetVertCorrection(AValue: integer);
begin
  if FVertCorrection=AValue then Exit;
  FVertCorrection:=AValue;
  FOwner.InvalidateEventLine;
end;

procedure TInfoBox.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  FOwner.InvalidateEventLine;
end;
