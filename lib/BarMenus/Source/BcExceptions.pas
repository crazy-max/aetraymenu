{ Unit: BcExceptions
  ===========================================================================

  Copyright (C) 2001-2002 Bluecave Software. All Rights Reserved.
  http://www.bluecave.net/

  Copyright (C) 2001-2002 Jouni Airaksinen. All Rights Reserved.
  http://Mintus.Codefield.com/ -- Mintus@Codefield.com

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}

unit BcExceptions;

interface

uses SysUtils, Classes;

resourcestring
  SInvalidPropertyIndexAt = 'Invalid property index %d at %s.';
  SInvalidPropertyIndex = 'Invalid property index %d.';
  SRequireOwner = 'Tried to create %s with no owner.';

type
  EInvalidPropertyIndex = class(Exception);
  ERequireOwner = class(Exception);

implementation

end.
