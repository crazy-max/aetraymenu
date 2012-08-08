//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BcBarMenusBD50.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\Source\BarMenuRegister.pas");
USERES("..\Source\BarMenuRegister.dcr");
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
