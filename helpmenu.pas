{ <Adds a new item "Mutis-Help" to the Help menu>
  <Version 1.0.0.1>
  Copyright (C) <23.10.2022> <Bernd Hübner>
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

unit helpmenu;

{$mode objfpc}{$H+}

interface 

uses 
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, LCLIntf,
 {LMessages,} MenuIntf, rs_mbstylemanager, PathTo;



procedure Register;



implementation

procedure StartHelp({%H-}Sender : TObject);
var PathToConfigDir : string;
    PathToMultis    : string;
begin
  //search for ConfigDirectory
  PathToConfigDir := PathToConfig;
  //showmessage(PathToConfigDir);

 //read the path to multis
  if FileExists(PathToConfigDir+'packagefiles.xml') then
   PathToMultis     := ReadPathToMultis(PathToConfigDir+'packagefiles.xml');

 if not OpenDocument(PathToMultis+'help'+PathDelim+'DescriptionMultis_'+rs_lang +'.pdf')
  then showmessage(rs_muhelperror);
end;



procedure Register;
var PathToConfigDir : string;
    PathToMultis    : string;
    HelpWanted      : boolean;
    Hlw             : string;
    IsNew           : boolean;
    Nw              : string;
begin
 {$I helpmenu.lrs}
  IsNew           := true;
  HelpWanted      := true;

  //search for ConfigDirectory
  PathToConfigDir := PathToConfig;


 //read the path to multis and the current version
  if FileExists(PathToConfigDir+'packagefiles.xml') then
   begin
    PathToMultis     := ReadPathToMultis(PathToConfigDir+'packagefiles.xml');
   end else
   begin
    IsNew           := false;
    Nw              := 'No';
    HelpWanted      := false;
    HlW             := 'No';
   end;


  if FileExists(PathToMultis+'multis.xml') then
   begin
    HLW := ReadHelp(PathToMultis+'multis.xml');
    if HlW='Yes' then HelpWanted:=true else HelpWanted:=false;
    Nw  := ReadNew(PathToMultis+'multis.xml');
    if Nw = 'YES' then IsNew := true else IsNew := false;
   end;

  if IsNew then
   if InstallDialog then HelpWanted:=true else HelpWanted:=false;
  if HelpWanted then HlW:='Yes' else HlW:='No';

  Nw := 'No';
  WriteMultisXMl(Nw,HlW,PathToMultis);

  if HelpWanted then
  RegisterIDEMenuCommand(itmHelpTools, 'MultisHelp',rs_muhelp,nil,@StartHelp,nil,'help');


end;


end.
