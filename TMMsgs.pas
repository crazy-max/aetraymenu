unit TMMsgs;
{
  Aestan Tray Menu
  Made by Onno Broekmans; visit http://www.xs4all.nl/~broekroo/aetraymenu
  for more information.

  This work is hereby released into the Public Domain. To view a copy of the
  public domain dedication, visit:
      http://creativecommons.org/licenses/publicdomain/
  or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford,
  California 94305, USA.

  This unit contains some constants for various 'compiler' messages etc.
}

interface

resourcestring
  {***** PARSING *****}
  { Parameter parsing }
  SParsingParamHasNoValue = 'Specified parameter "%s" has no value';
  SParsingParamQuoteError = 'Mismatched or misplaced quotes on parameter "%s"';
  SParsingParamMissingClosingQuote = 'Missing closing quote on parameter "%s"';
  SParsingParamDuplicated = 'Cannot have multiple "%s" parameters';
  SParsingParamEmpty2 = 'Parameter "%s" is empty';
  SParsingParamNoQuotes2 = 'Parameter "%s" cannot include quotes (")';
  SParsingParamUnknownParam = 'Unrecognized parameter name "%s"';
  SParsingParamInvalidBoolValue = 'Unrecognized boolean value "%s" (' +
    'should be 1, 0, yes, no, on, off, true or false)';
  SParsingParamInvalidIntValue = 'Integer expected on parameter "%s"';
  SParsingUnknownVarType = 'Unknown variable type "%s"';
  SParsingInvalidVarParam = 'The "%s" parameter is invalid in combination with ' +
    'this type of variable';
  SParsingInvalidRegRoot = 'Invalid registry root "%s"';
  SParsingUnknownMenuItemType = 'Unknown menu item type "%s"';
  SParsingInvalidMenuItemParam = 'The "%s" parameter is invalid in ' +
    'combination with this type of menu item';
  SParsingUnknownActionType = 'Unknown action type "%s"';
  SParsingInvalidActionParam = 'The "%s" parameter is invalid in ' +
    'combination with this type of action';
  SParsingUnknownShowCmd = 'Unknown ShowCmd type "%s"';
  SParsingParamUnknownService = 'Parameter "%s" specifies an unknown service';
  SParsingServiceParamInvalid = 'The "Service" parameter is only valid in combination ' +
    'with "ServiceSubMenu" items or "Service" actions';
  SParsingInvalidServiceAction = 'Unknown service action "%s"';
  SParsingParamExpected = 'Parameter "%s" expected';
  SParsingCannotAssignActionFlagsYet = 'Please specify action flags after ' +
    'specifying the action type';
  SParsingAssignVarTypeFirst = 'You have to specify the variable''s type first';

  { Flags }
  SParsingParamUnknownFlag2 = 'Parameter "%s" includes an unknown flag';
  SParsingInvalidActionFlag = 'The "%s" flag is invalid in combination with ' +
    'this type of action';

  { Line parsing }
  SParsingSectionTagInvalid = 'Invalid section tag';
  SParsingSectionBadEndTag = 'Not inside "%s" section, but an end tag for ' +
    'it was encountered';
  SParsingTextNotInSection = 'Text is not inside a section';

  { Section directives }
  SParsingUnknownDirective = 'Unrecognized [%s] section directive "%s"';
  SParsingEntryAlreadySpecified = '[%s] section directive "%s" already specified';
  SParsingInvalidDirectiveValue = 'Invalid value "%s" on directive "%s"';
  SParsingUnknownFontStyle = 'Unknown font style flag';
  SParsingInvalidFontSpec = 'Invalid font specification';
  SParsingInvalidColor = 'Invalid color specification on directive "%s"';
  SParsingAmbiguousTrayIcon = 'You cannot specify both the TrayIcon*Running and ' +
    'the TrayIcon directives in the [Config] section';
  SParsingAmbiguousAboutText = 'You cannot specify both the AboutTextFile ' +
    'directive and the [AboutText] section';

  { Other }
  SParsingCouldNotLoadFile = 'Could not load the file "%s" specified ' +
    'in the directive "%s"';
  SParsingCouldNotOpenRegKey = 'Could not open registry key "%s\%s"';
  SParsingCouldNotReadRegValue = 'Could not query registry value "%s\%s\%s"';
  SParsingRegValueInvalidFormat = 'Registry value "%s\%s\%s" has an invalid format: %s';
  SParsingValidatePropertiesFailed = 'Internal error: TTMConfigReader hasn''t been ' +
    'initialized properly';
  SParsingInvalidService = 'Invalid service "%s"';
  SParsingCmdLineParameterNotFound = 'Command-line parameter "%s" not found';

  {***** POST-PARSING VALIDATION *****}
  SValNoTrayIconAssigned = 'No tray icon was specified. Please assign a tray ' +
    'icon by using one of the TrayIcon* directives in the [Config] section';
  SValMustSpecifyThreeStateIcons = 'You have to specify all of the following ' +
    'directives in the [Config] section: TrayIconAllRunning, TrayIconSomeRunning ' +
    'and TrayIconNoneRunning';

  {***** OTHER *****}
  SReaderHasntBeenInitializedYet = 'TTMConfigReader hasn''t been initialized properly yet';
  SReaderSyntaxError = 'The configuration file contains a syntax error on line %d:';
  SCannotSetPromptVarValue = 'Cannot set the value of a "Prompt" variable';

  {***** DEFAULT MESSAGES *****}
  SDefAllRunningHint = 'All services running';
  SDefSomeRunningHint = '%n of %t service(s) running';
  SDefNoneRunningHint = 'None of %t service(s) running';

  {*** MAIN PROGRAM ***}
  SMainCouldNotLoadConfig = 'Could not load configuration file';
  SMainCouldNotExecuteMenuItem = 'Could not execute menu item (internal error)';
  SMainBuiltInNotImplemented = 'Sorry, somehow you''ve come across a built-in action that hasn''t been implemented yet';

  {*** HTML WINDOW ***}
  SHtmlUnknownAction = 'Unknown action "%s". You should declare actions ' +
    'using the [Config]: HtmlActions directive before using them.';

implementation

end.
