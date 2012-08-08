//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BcBarMenusB50.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\Source\BarMenus.pas");
USEUNIT("..\Source\BarPopupMenuLayer.pas");
USEUNIT("..\Source\BcExceptions.pas");
USEUNIT("..\Source\BcFontUtilities.pas");
USEUNIT("..\Source\BcLayeredWindows.pas");
USEUNIT("..\Source\BcRectUtilities.pas");
USEUNIT("..\Source\BcUtilities.pas");
USEUNIT("..\Source\BcDrawModule.pas");
USEUNIT("..\Source\BcCustomDrawModule.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
