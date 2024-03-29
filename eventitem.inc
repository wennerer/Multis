{%MultiEventLine}

{ TMultiEvent }

constructor TMultiEvent.Create(ACollection: TCollection);
begin
 inherited Create(ACollection);
 FEvents              := ACollection;
 FDisplayName         := 'Event';
 FEnabled             := true;
 FVisible             := true;
 FLeft                := 0;
 FTop                 := 0;
 FSize                := 15;
 FNumbers             := true;
 FBorderColor         := clBlack;
 FBorderWidth         := 1;
 FColorStart          := clWhite;
 FColorEnd            := clCream;
 FGradient            := gcSpread;
 FStyle               := mesCircle;
 FRRRadius            := 5;
 FDisabledColor       := $D2D2D2;
 FBlendValue          := 180;
 FHover               := false;
 FHoverColor          := clOlive;
 FHoverBlendValue     := 120;
 FImageIndex          := -1;
 FFont                := TFont.Create;
 //FFont.OnChange       := @CheckBoxFontChanged;
 FInfoBox := TInfoBox.create(self);
end;

destructor TMultiEvent.Destroy;
begin
  FFont.Free;
  FInfoBox.Free;
  inherited Destroy;
end;




procedure TMultiEvent.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.InvalidateEventLine;
begin
 (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetFont(AValue: TFont);
begin
 if FFont=AValue then Exit;
 FFont.Assign(aValue);
 (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetImageList(AValue: TCustomImageList);
begin
  if FImageList=AValue then Exit;
  FImageList:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

procedure TMultiEvent.SetSize(AValue: integer);
begin
  if FSize=AValue then Exit;
  FSize:=AValue;
  (FEvents as TMultiEventCollection).FDrawEvent.Invalidate;
end;

function TMultiEvent.GetDisplayName: string;
begin
  Result:=inherited GetDisplayName;
end;

procedure TMultiEvent.SetDisplayName(const Value: string);
begin
  //inherited SetDisplayName(Value);
 FDisplayName := Value;
end;

function TMultiEvent.GetOwner: TPersistent;
begin
  //Result:=inherited GetOwner;
 Result := FEvents;
end;




