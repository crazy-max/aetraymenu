AESTAN TRAY MENU
================

Please visit http://www.onnodb.com/aetraymenu for the latest news and information regarding Aestan Tray Menu.

RELEASE NOTES
-------------
Important note: this version has been compiled with the madExcept (http://www.madshi.net) library. This implies that this file version is for non-commercial use only. A version without MadExcept has also been included, with the filename "AeTrayMenu_NoME.exe".
Also note that the version without madExcept has exactly the same funtionality, but only misses some special handling of exceptions.

*** 1.6.1
Added [StartupAction] section.

*** 1.4.3
The about box can be customized now. Use [Config]: AboutHeader, AboutVersion to customize the header labels. You can specify a section [AboutText] with the full text, or [Config]: AboutTextFile with the filename of the text file containing the about text.
These directives have not been documented yet in the help file.

*** 1.4.2
This release contains only one bugfix for the problem marked [1] in the release notes for 1.4.1 (below); it appears it was a Delphi problem.

*** 1.4.1
This is the first public release of AeTrayMenu.
Known issues:
- The helpfile is still incomplete. Although the most important parts are done, the documentation on the directives in the [Menu.*.Settings] sections hasn't been written yet. However, I hope you can find out most of the information about [Menu.*.Settings] by looking at the examples and experimenting. Also, an index has to added to the helpfile yet.
- [1] On WinXP, the traymenu sometimes seems to raise exceptions randomly in rare occasions. Those errors are not fatal, and one can continue using the program without a restart. This is a minor issue, but I will look into it as soon as possible.
- Some more examples should be included.
- Maybe some more work is needed on adding some 'finishing touches' to the whole package.