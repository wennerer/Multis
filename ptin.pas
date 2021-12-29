{ <Point in ...>

  Copyright (C) <23.11.2020> <Bernd Hübner>
  for some improvements see https://www.lazarusforum.de/viewtopic.php?f=29&t=13252

  This library is free software; you can redistribute it and/or modify it under the
  terms of the GNU Library General Public License as published by the Free Software
  Foundation; either version 2 of the License, or (at your option) any later
  version with the following modification:

  As a special exception, the copyright holders of this library give you permission
  to link this library with independent modules to produce an executable,
  regardless of the license terms of these independent modules,and to copy and
  distribute the resulting executable under terms of your choice, provided that you
  also meet, for each linked independent module, the terms and conditions of the
  license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this
  exception to your version of the library, but you are not obligated to do so. If
  you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along
  with this library; if not, write to the Free Software Foundation, Inc., 51
  Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit ptin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math;

function PointInCircle(aRect:TRect;x,y:integer) : boolean;
function PointInRoundRect(aRect:TRect;x,y,rx,ry:integer):boolean;
function PointInEllipse(aRect:TRect;x,y:integer):boolean;
function PtInPoly(const poly: array of TPoint;p: TPoint): Boolean;

implementation

function PointInCircle(aRect:TRect;x,y:integer) : boolean;
var radius     : integer;
    a,b        : integer;
begin
 radius := (aRect.Right - aRect.Left) div 2;
 a := aRect.CenterPoint.X - x;
 b := aRect.CenterPoint.Y - y;
 result := (Round(HyPot(a,b)) <= radius);
end;

function PointInRoundRect(aRect:TRect;x,y,rx,ry:integer):boolean;
//works up rx<=aRect.width,ry<=aRect.height !!!
var checkRect : TRect;
    checkP    : TPoint;
begin
 result:=true;
 checkP.X:=x;checkP.Y:=y;
 checkRect := rect(aRect.Left+(rx div 2),aRect.Top,aRect.Right-(rx div 2),aRect.Bottom);
 if PtInRect(checkRect,checkP) then exit;
 checkRect := rect(aRect.Left,aRect.Top+(ry div 2),aRect.Right,aRect.Bottom-(ry div 2));
 if PtInRect(checkRect,checkP) then exit;
//check top left
 checkRect := rect(aRect.Left,aRect.Top,aRect.Left+(aRect.Width div 2),aRect.Top+(aRect.Height div 2));
 if PtInRect(checkRect,checkP) then
  begin
   checkRect := rect(aRect.Left,aRect.Top,aRect.Left+rx,aRect.Top+ry);
   if PointInEllipse(checkRect,x,y) then exit;
 end;

 //check top right
  checkRect := rect(aRect.Right-(aRect.Width div 2),aRect.Top,aRect.Right,aRect.Top+(aRect.Height div 2));
  if PtInRect(checkRect,checkP) then
   begin
    checkRect := rect(aRect.Right-rx,aRect.Top,aRect.Right,aRect.Top+ry);
    if PointInEllipse(checkRect,x,y) then exit;
  end;
 //check down left
  checkRect := rect(aRect.Left,aRect.Top+(aRect.Height div 2),aRect.Left+(aRect.Width div 2),aRect.Bottom);
  if PtInRect(checkRect,checkP) then
   begin
    checkRect := rect(aRect.Left,aRect.Bottom-ry,aRect.Left+rx,aRect.Bottom);
    if PointInEllipse(checkRect,x,y) then exit;
  end;
 //check down right
  checkRect := rect(aRect.Left+(aRect.Width div 2),aRect.Top+(aRect.Height div 2),aRect.Right,aRect.Bottom);
  if PtInRect(checkRect,checkP) then
   begin
    checkRect := rect(aRect.Right-rx,aRect.Bottom-ry,aRect.Right,aRect.Bottom);
    if PointInEllipse(checkRect,x,y) then exit;
  end;
 result:=false;
end;

function PointInEllipse(aRect:TRect;x,y:integer):boolean;
var rx,ry,res1,res2 : double;
begin
 rx:= aRect.Width / 2;
 ry:= aRect.Height / 2;
 res1:= (sqr(x-aRect.CenterPoint.X)) / (sqr(rx));
 res2:= (sqr(y-aRect.CenterPoint.Y)) / (sqr(ry));
 res1:=res1+res2;
 if res1 <= 1 then result:= true else result:=false;
end;

function PtInPoly(const poly: array of TPoint;p: TPoint): Boolean;
var i,k : integer;
Begin
 result := false;
 k := High(poly);
 For i := 0 to high(poly) do begin
  if (
     ( ((poly[i].y <= p.y) and (p.y < poly[k].y)) or ((poly[k].y <= p.y) and (p.y < poly[i].y)) ) and
        (p.x < ((poly[k].x - poly[i].x) * (p.y - poly[i].y) / (poly[k].y - poly[i].y) + poly[i].x) )
     ) then result := not result;
  k := i
 end;
end;

end.

