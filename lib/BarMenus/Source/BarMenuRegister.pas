{ Unit: BarMenuRegister
  ===========================================================================
  Created: 2001-07-02 (yyyy-mm-dd)
  ===========================================================================

  The contents of this file are subject to the Bluecave Public License V 1.1
  (the "License"). You may not copy or use this file, in either source code
  or executable form, except in compliance with the License. You may obtain
  a copy of the License at http://www.bluecave.net/licenses/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.

  Copyright (C) 2001-2002 Bluecave Software. All Rights Reserved.
  http://www.bluecave.net/

  Copyright (C) 2001-2002 Jouni Airaksinen. All Rights Reserved.
  http://Mintus.Codefield.com/ -- Mintus@Codefield.com

  ===========================================================================

  History:

    2001-07-02: Initial version.

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}
{$I BarMenus.inc}

unit BarMenuRegister;

interface

uses
  Classes;

procedure Register;

implementation

uses
{$IFDEF OLDCLASS}
  BarPopupMenuLayer,
{$ENDIF}
  BarMenus,
  BcCustomDrawModule;

procedure Register;
const
  TabPage = 'Bluecave';
begin
  RegisterComponents(TabPage,
    [{$IFDEF OLDCLASS}TBarPopupMenu, {$ENDIF}
     TBcBarPopupMenu,
     TBcBarMainMenu,
     TBcCustomDrawModule]);
end;

end.
