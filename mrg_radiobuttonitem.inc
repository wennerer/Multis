(*This is a part of the TMultiRadioGroup*)

{%MultiRadioGroup ,this is a part of TMultiRadioGroup last change 11.01.2023}

//xxxxxxxxxxxxxxxxxxxxxxx-- This is the Collection --xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
{ TMRadioButtons }

function TMRadioButtons.GetRadioButton(Index: Integer): TMRadioButton;
begin
 result := TMRadioButton( inherited Items[Index] );
end;

function TMRadioButtons.GetEnabled: Boolean;
begin
 result := VisibleCount > 0;
end;

function TMRadioButtons.GetVisibleCount: Integer;
{$ifNdef newcols}
var
  i: Integer;
{$endif}
begin
  {$ifdef newcols}
  result := Count;
  {$else}
  result := 0;
  for i:=0 to Count-1 do
    if Items[i].Visible then
      inc(result);

  {$endif}
end;

procedure TMRadioButtons.SetRadioButton(Index: Integer; AValue: TMRadioButton);
begin
 Items[Index].Assign( aValue );
end;

function TMRadioButtons.GetOwner: TPersistent;
begin
 // Result:=inherited GetOwner;
 Result := FMultiRadioGroup;
end;

constructor TMRadioButtons.Create(aCollection: TMultiRadioGroup;
  aItemClass: TCollectionItemClass);
begin
 inherited Create( aItemClass );
 FMultiRadioGroup := aCollection;
end;









//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---ITEM--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---TMRadioButton---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
{ TMRadioButton }

constructor TMRadioButton.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCaption := 'Radiobutton';
  FColor := clNone;
  FWidth := 200;
  FHeight := 20;
  FCaptionWordbreak := false;
  FCapLeft        := 0;
  FCapTop         := 0;
  FVisible := true;
  FFont     := TFont.Create;
  FParentFont:= true;


  FTextStyle.Alignment := taLeftJustify;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= false;
  FTextStyle.Wordbreak := false;
  FTextStyle.Clipping  := true;

end;

destructor TMRadioButton.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TMRadioButton.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TMRadioButton.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
end;

procedure TMRadioButton.SetAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
end;

procedure TMRadioButton.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  FCaptionChange:=true;
end;

procedure TMRadioButton.SetCaptionWordbreak(AValue: boolean);
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
end;

procedure TMRadioButton.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
end;

procedure TMRadioButton.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

procedure TMRadioButton.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont.Assign(aValue);
end;

procedure TMRadioButton.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TMRadioButton.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
end;

procedure TMRadioButton.SetParentFont(AValue: boolean);
begin
  if FParentFont=AValue then Exit;
  FParentFont:=AValue;
end;

procedure TMRadioButton.SetTextStyle(AValue: TTextStyle);
begin
 FTextStyle:=AValue;
end;

procedure TMRadioButton.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

