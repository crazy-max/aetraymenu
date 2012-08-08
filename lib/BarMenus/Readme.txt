  ============================================================================
                              BarMenu Components
        Copyright (C) 2000-2003 Bluecave Software. All Rights Reserved.
  ============================================================================

  You may use this Package under compliance of Public License or
  Corporation License.

  Public License:
  ===============

  For license terms see License.txt. You may not use this package in anyway
  unless you agree the terms. So, before you continue, please read the
  License.

  Corporation License:
  ====================

  For license terms see CorpLicense.txt. You may not use this package in
  anyway unless you agree the terms. So, before you continue, please read the
  License.

  Corporation License License Fee is $29.90. For detailed information how to
  order read Register.txt.


  Contact information:
  ====================

  Latest version of the BarMenus is available at:
  http://www.bluecave.net/products/barmenus/BarMenus.zip

  Bluecave Software, http://www.bluecave.net/
  support@bluecave.net

  Jouni Airaksinen, http://www.bluecave.net/mintus/
  Mintus@Codefield.com

  If you have any questions or suggestions contact me. Use either my personal
  email or the Bluecave Software support email. Any emails related to this
  package are welcome. If you want to praise these components, you are
  welcome to do so. Actually, I would appreciate your comments very much.
  Without any mental support, I seen no point developing these for others
  than just me and my needs.

  
  BarMenu Components Mailing list:
  ================================

  BarMenu Components has a mailing list. The list is for questions,
  feature requests and general discussion. You will also get notifications
  through the list whenever new a version of BarMenu Components is released.
  The list is a very convient way to communicate with the author and other
  users of the BarMenu Components.

  To join the list go to, http://www.freelists.org/list/barmenus

  Mailing list provided by www.freelists.org.


  Other people involved:
  ======================

  Thanks to following people who have contributed their work to
  BarMenu Components:

    * Magnus Flysjö (magnus.flysjo@visuellkommunikation.com)
    * Yury Zaytsev, Binary Insight SDT (www.binary-insight.com)

  Thanks for Magnus Flysjö for some suggestions and improvements to the
  code.
  
  Special thanks to Stefan Agoston for testing the package compatibility
  with Delphi 6 and C++Builder 5.

  Big thanks for Yuri Zaytsev for testing and providing packages for
  C++Builder 6.


  What are BarMenu Components?
  ============================

  BarMenu Components is a package to create menus and popupmenus with a
  gradient bar - like the classic start-menu. Gradient bar is drawn dynamically
  so you get very smooth bar with customizable colors. Alternatively you can
  have bitmap as a bar or even both; bitmap *and* gradient bar. Gradient
  bar can have dynamically created vertical caption with highlight and shadow
  colors. Also you can have additional bitmap (e.g. logo) on the bar. If you
  want you can also draw the menu bar manually through events.

  BarMenu Components are compatible with Windows XP. Windows version is
  autodetected and your menus have automatically XP look under Windows XP
  and the standard look under older versions of Windows'. It is also possible
  to force the MenuStyle.

  Menu separator lines in BarMenus can be drawn in a short way - like in
  Office 2000. Also it is possible to have text in the separator line - like in
  ICQ. Separator line ends can be faded like in Windows XP and/or lines can be
  flat (single line, color can be selected).

  Making some special application and need custom menu font? These components
  have MenuFont property to change the font other than the system default just
  for the menus in your application.

  For more flexibility there is a DrawModule concept. DrawModule is separate
  module which handles the MenuItem drawing for the whole menu. You can switch
  between different DrawModules in designtime and runtime. Let the user
  choose the menu look in your application - implement with just one line of
  code!

  Components are compatible with VCL, so it is easy to convert existing
  menus. In addition BarMenus are resource friendly, no Form based solutions.
  Package contains two demo applications.

  BarMenu Components package contains following components:
    - TBcBarMainMenu
    - TBcBarPopupMenu
    - TBcCustomDrawModule


  Requirements:
  =============

  BarMenu Components can be compiled and used with (tested):
    * Delphi 5
    * Delphi 6
    * Delphi 7
    * C++Builder 5
    * C++Builder 6

  Might be coming:
    version for Kylix 1 and/or 2


  Installing:
  ===========

  After unzipping the files in the package to own directory you should have
  following directories:

  Demo1\     - a demo program demonstrating the component
  Demo2\     - a demo program demonstrating dynamic use of the component
  Packages\  - packages for different compilers (Runtime, Designtime)
                * Delphi 5 (BcBarMenus50.dpk, BcBarMenusD50.dpk)
                * Delphi 6 (BcBarMenus60.dpk, BcBarMenusD60.dpk)
                * C++Builder 5 (BcBarMenusB50.dpk, BcBarMenusBD50.dpk)
                * C++Builder 6 (BcBarMenusB60.dpk, BcBarMenusBD60.dpk)
  Source\    - source files

  1. Add the Source\ directory to your library path.
     * Tools | Enviroment Options | Library | Library path ...
     * write the path to the edit box and press Add.
  2. Open appropiate runtime package file (without the "D" letter before
     the version number) in your IDE by selecting File | Open.
  3. In the resulting package window, click the Compile button.
  4. Open appropiate designtime package file (with the "D" letter before
     the version number) in your IDE by selecting File | Open.
  5. In the resulting package window, click the Install button.  If this
     package has already been installed (i.e. you are updating existing
     files), click the Compile button instead.

  Now you should have the components under Bluecave tab in the component palette.

  If you have problems with upgrading it is advisable to uninstall the BarMenus
  packages and then close your IDE and delete all BcBarMenus*.bpl files from your
  system (usually at ($DELPHI)\Projects\Bpl\ and/or ($SYSTEM)\). After that try
  again to install with the above five step instructions.


  Uninstalling:
  =============

  If you need to uninstall the components:

  1. Open Packages window by selecting Component | Install Packages.
  2. Find the "Bluecave: BarMenu Components (Designtime)" package from the
     window and uncheck the box next to it.
  3. Click Remove and then Yes.
  4. In addition you may remove the Source\ directory from the library path.

  To be completely sure of full uninstallation you can delete the BcBarMenus*.bpl
  files from system. Note that if you have applications compiled with the runtime
  packages, and you delete the bpl files, your applications won't work anymore.


  TBcBarPopupMenu and TBcBarMainMenu:
  ===================================

  TBcBarPopupMenu works like the normal TPopupMenu but TBcBarPopupMenu
  introduces few new properties. TBcBarMainMenu is similar to normal
  TMainMenu and it introduces few new properties as well.

  Following properties are common for both components:

  + property Separators: TBcSeparators
  | |
  | +-- property SeparatorStyle: TSeparatorStyle
  | |     Controls the the separator line style. Note that with style ssCaption
  | |     if there is no caption text (the Hint property of menuitem) the line
  | |     is drawn in a standard way.
  | |
  | +-- property Alignment: TAlignment
  | |     Alignment of the separator line text.
  | |
  | +-- property GradientStyle: TGradientStyle
  | |     Style of the caption gradient with style ssCaption.
  | |
  | +-- property GradientStart: TColor
  | +-- property GradientEnd: TColor
  | |     Colors of the separator line caption with style ssCaption.
  | |
  | +-- property Fade: Boolean
  | |     If set True the separators (ssShortLine, ssNormal) are drawn
  | |     as single line with faded ends.
  | |
  | +-- property FadeWidth: Boolean
  | |     Width of the fade at the ends.
  | |
  | +-- property FadeColor: Boolean
  | |     Color of the line when Fade is True.
  | |
  | +-- property FlatLines: Boolean
  | |     Lines as single color line.
  | |
  | +-- property Font: TFont
  | |     Separator line caption text font. Has no effect if UseSystemFont is
  | |     set True.
  | |
  | +-- property UseSystemFont: Boolean
  |       If set True the separator line captions are draw using the default
  |       system menu font instead of the font in the Font property.
  |
  + property Flat: Boolean     // protected property - do not use
  |   Draw menu borders Flat instead of the standard 3D look.
  |
  + property Opacity: TOpacity // protected property - do not use
  |   Controls the opacity of the menu. Values 0..255 are accepted where 255
  |   is completely opaque and 0 is completely transparent. Works only in
  |   Windows 2000/XP. Ignored in Windows'. This property does not exist
  |   in TBcBarMainMenu.
  |
  + property MenuFont: TFont
  |   Font for the menuitems. Use this e.g. to personalize the menifont only for
  |   your application because of special font requirements.
  |
  + property UseSystemFont: Boolean
  |   If set True the menu items are draw using the default
  |   system menu font instead of the font in the MenuFont property.
  |
  + property MenuStyle: TBcMenuStyle
  |   This property allows you to select msStandard, msWindowsXP or msAuto. msAuto
  |   will select drawing style depending on the Windows version in which the
  |   application is run. msStandard is the VCL old way and msWindowsXP is
  |   Windows XP way.
  |
  + property DrawModule: TBcBarMenusDrawModule
  |   DrawModule which handles drawing of the menuitems within the
  |   menu. If this is nil then default drawing implemented in BarMenus is used.
  |   Currently default drawing passes menuitem drawing to VCL and lineitems
  |   it draws itself. See more about DrawModules at TBcBarMenusDrawModule.
  |
  + property Bar: TBcBar
    | These properties alter the look of the menu bar. In TBcBarMainMain the
    | bar is shown in the first visible top level menu.
    |
    +-- property BarPicture: TBcBarPicture
    |   | This property controls the picture on the bar:
    |   |
    |   +-- property Picture: TPicture
    |   |     Picture which is drawn on the bar. It can be Icon, Metafile, Bmp or
    |   |     other TGraphic supported format.
    |   |
    |   +-- property VertAlignment: TVertAlignment
    |   +-- property HorzAlignment: THorzAlignment
    |   |     Alignment values to position picture, fine tune position with Offsets
    |   |
    |   +-- property OffsetX: Integer
    |   +-- property OffsetY: Integer
    |   |     Fine tune the picture position
    |   |
    |   +-- property Transparent: Boolean
    |   |     If this is True, then the Picture is drawn transparently. Transparent
    |   |     color is the bottom left pixel. Note that this property has no effect
    |   |     unless Picture is TBitmap resource.
    |   |
    |   +-- property Visible: Boolean
    |         Visibility of BarBackPicture. Can be overridden with
    |         OnAdvancedBeforeDrawBar.
    |
    +-- property BarBackPicture: TBcBarBackPicture
    |   | This property controls the background picture on the bar:
    |   |
    |   +-- property Picture: TPicture
    |   |     Background Picture which is drawn on the bar.
    |   |
    |   +-- property DrawStyle: TDrawStyle
    |   |     Style how the background is drawn to the bar. Possible values:
    |   |     dsNormal, dsTile, dsStretch.
    |   |     Note that if Picture is Icon stretch does not work.
    |   |
    |   +-- property VertAlignment: TVertAlignment
    |   +-- property HorzAlignment: THorzAlignment
    |   |     Alignment values to position bitmap, fine tune position with Offsets
    |   |     If DrawStyle is dsTile then these tell from where start to tile.
    |   |     Note: No effect if DrawStyle is dsStretch.
    |   |
    |   +-- property OffsetX: Integer
    |   +-- property OffsetY: Integer
    |   |     Fine tune the bitmap position. If DrawStyle is dsTile, these will fine
    |   |     tune the tiling start position.
    |   |     Note: No effect if DrawStyle is dsStretch.
    |   |
    |   +-- property Transparent: Boolean
    |   |     If this is True, then the Bitmap is drawn transparently. Transparent
    |   |     color is the bottom left pixel. If the bitmap is transparent or doesn't
    |   |     fill the whole bar the Bitmap is drawn on top gradient bar. Note that
    |   |     this property has no effect unless Picture is TBitmap resource.
    |   |
    |   +-- property Visible: Boolean
    |         Visibility of BarBackPicture. Can be overridden with
    |         OnAdvancedBeforeDrawBar.
    |
    +-- property BarCaption: TBcBarCaption
    |   | This property controls the vertical caption on the bar:
    |   |
    |   +-- property Caption: string
    |   |     Caption which is drawn to the bar
    |   |
    |   +-- property Font: TFont
    |   |     Font which is used to draw the caption
    |   |
    |   +-- property OffsetY: Integer
    |   |     Offset from the bottom of the bar. Caption is always centered to the
    |   |     bar. If Bitmap is aligned to the bottom, then offset is from the bitmap
    |   |     top. Defaults to 6 (which is most of the time just okay).
    |   |
    |   +-- property Alignment: TVertAlignment
    |   |     To where the text is aligned in the bar. Defaults to vaBottom.
    |   |
    |   +-- property Direction: TDirection
    |   |     Direction from which the text is drawn. Defaults to dDownToUp.
    |   |
    |   +-- property Visible: Boolean
    |         Visibility of BarCaption. Can be overridden with OnAdvancedBeforeDrawBar.
    |
    +-- property GradientStyle: TGradientStyle
    |     Gradient style. Can have values gsHorizontal, gsVertical, gsDiagonalLeftRight
    |     and gsDiagonalLeftRight.
    |
    +-- property GradientStart: TColor
    +-- property GradientEnd: TColor
    |     These control the color of the gradient bar. To disable gradient bar drawing
    |     e.g. for transparent bar background bitmaps set both colors to same color
    |     (clBtnFace for system popupmenu color).
    |
    +-- property Visible: Boolean
    |     If this is False, the popupmenu looks like normal popupmenu. Note, you can
    |     still use the ShortLines and the separator line captions!
    |
    +-- property Width: Integer
    |     Width of gradient bar
    |
    +-- property Border: TColor
    |     Color of the bar border
    |
    +-- property Side: TSide
    |     Side of the bar in the menu. sLeft or sRight.
    |
    +-- Space: Integer
          Space between bar and the menuitems. Defaults to 1.


  Following events are common for both components:

  + property OnBeforeDrawBar: TOnBeforeDrawBarEvent
  |   This event is executed just before the bar needs to be refreshed.
  |   You can make any custom initialization and/or draw the whole bar yourself
  |   to the given canvas. Set DefaultDraw to False, if you don't want to
  |   the default bar drawing to take place. Note, if DefaultDraw is set False
  |   no drawing to bar is done. So it is completely up to you want the bar
  |   contains. DefaultDraw defaults to True.
  |   Warning: You should use the given canvas only locally.
  |
  + property OnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent
  |   Same as OnBeforeDrawBar but has one extra parameter DrawParts: TBarParts,
  |   which allows you to select which Parts are drawn. Good if you wan to do
  |   custom drawing but want e.g. the gradient been drawn to the background.
  |   DrawParts is preset to contain all visible parts.
  |
  + property OnAfterDrawBar: TOnAfterDrawBarEvent
  |   This event is executed after the internal bar drawing has been done. This
  |   way you can implement some custom content over the bar.
  |
  + property OnMeasureMenuItem: TOnMeasureMenuItemEvent
      This event is global for all items in the menu and is called before
      requesting DrawModule measurement. So you can override the default Item
      Width and/or Height here before those are processed further. If you change the
      DefaultMeasure var parameter to False then DrawModule measuring is omitted.
      See example in Demo application.


  Following methods exists in both components:

  + procedure FlushDoubleBuffer;
  |   Frees the doublebuffer used for the menu bar. It will be recreated and
  |   redrawn when it's needed again.
  |
  + procedure BeginUpdate;
  + procedure EndUpdate;
      These methods should be used when the menus are created dynamically
      at runtime. Create the items like you would with normal VCL menus, but
      enclose the creation with BeginUpdate and EndUpdate methods. Those calls
      make sure the items are updated correctly (especially the root level).


  TBcBarPopupMenu methods:

  + procedure Popup(P: TPoint);
  |   Same as the TPopupMenu Popup(X, Y: Integer) but takes the cordinates as
  |   a TPoint.
  |
  + procedure PopupAtCursor;
      Popups the menu at mouse cursor position.


  TBcBarMenusDrawModule:

    This is a abstract class. Do not create instances of this class.

    To implement own drawing module inherit your class from this base class
    and implement the abstract methods.

    If you implement own drawing modules send those to Bluecave Software
    (Mintus@Codefield.com) to be added as part of the official release.
    Bluecave Software reserves right to fit the code into Bluecave
    Software coding style. When sending please state if Bluecave Software
    may distribute your code under Corporation License. Your Copyrights
    will be preserved.

    The modules sent should be independed components (they do not require
    any 3rd party packages).

    How to access the menu BarMenusIntf:

      Add uses BarMenus to implementation part and use the function
      GetMenuBarMenusIntf() on DrawingMenu property of TBcBarMenusDrawModule.


  TBcCustomDrawModule:

  + property Description: string
  |   Just a custom string e.g. to provide user interface some readable name
  |   of the currently active DrawModule.
  |
  + property OnMeasureMenuItem: TOnMeasureMenuItem
  + property OnDrawMenuItem: TOnDrawMenuItem
      Provides easy way to do menuitem drawing for all the menuitems within
      the menu while preserving the barmenu. It is important to respect the ARect
      parameter. You can use TMenuItem measure/draw event at the same time.
      By using the DefaultDraw/DefaultMeasure property you can force the Default
      menuitem processing. DefaultDraw/DefaultMeasure defaults to True, so
      remember to set it False if you do custom drawing.


  Helper functions:

  There are numerious helper functions at BarMenus.pas which are not documented.
  Probably the most useful functions are:

  + function GetMenuBarMenusIntf(AMenu: TMenu): IBarMenusIntf;
  + function GetMenuItemBarMenusIntf(AMenuItem: TMenuItem): IBarMenusIntf;
      These functions return the menu or owner menu of the menuitem as a
      IBarMenusIntf interface. The interface is a common interface to the
      barmenus which is compatible with both TBcBarMainMenu and TBcBarPopupMenu
      classes. 

  Global variables:

  + Win98Plus: Boolean
  + WinXP: Boolean
  + Win2K: Boolean
    These are set when the units are initialized. Their values depend
    on which system the application is run.


  Problems with or just questions about the BarMenu Components?
  =============================================================

  Please read the FAQ.txt.


  Known problems and limitations:
  ===============================

    * TBcBarMainMenu Does not work with Borland's TMenuBar component.
    * You cannot make submenus to have menu bars.
    * If bar Side is set to sRight the submenu arrows are drawn over the
      bar. Workaround -> make the bar look ok even if the arrows are at
      "wrong" place. Use e.g. tiled, aligned and transparent single color
      background bar to "reserve" the arrow space.
    * MenuFont property does not affect the menuitem measuring sizes
      (work-around, use OnMeasureMenuItem to do it manually)


  ============================================================================
                                               Copyright (C) Bluecave Software
