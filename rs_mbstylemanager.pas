(*This is a part of the multis package, last change 11.01.2022*)

unit rs_mbstylemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
    rs_left                  = 'Left:';
    rs_top                   = 'Top:';
    rs_takewidth             = 'Take over the width';
    rs_takeheight            = 'Take over the height';
    rs_save                  = 'Save';
    rs_load                  = 'Load';
    rs_apply                 = 'Apply';
    rs_ok                    = 'OK';
    rs_cancel                = 'Cancel';

    rs_EditLeft              = 'Left position of the DesignButton in the editor';
    rs_EditTop               = 'Top position of the DesignButton in the editor';
    rs_CheckOffSetWidth      = 'Determines whether the width is adopted';
    rs_CheckOffSetHeight     = 'Determines whether the height is adopted';
    rs_SaveButton            = 'Saves the current properties in a file';
    rs_LoadButton            = 'Loads saved properties from a file and closes the editor';
    rs_ApplyButton           = 'Adopts the current properties without closing';
    rs_OkButton              = 'Accepts the current properties and closes the editor';
    rs_AbortButton           = 'Discards the current properties and closes the editor';

    rs_look                  = 'Look';
    rs_style                 = 'The geometric shape of the button';
    rs_ColorGradient         = 'The direction of the gradient';
    rs_width_                = 'The horizontal size of the control.The width of the MultiButton is minus HoverFrameWidth';
    rs_height_               = 'The vertical size of the control.The height of the MultiButton is minus HoverFrameWidth';
    rs_rndrct                = 'Corner diameter if the geometric shape is RoundRect';
    rs_focframewidth         = 'The whidth of the focus-frame ';
    rs_focalphabval          = 'How translucent the focusframe is (0=transparent, 255=opaque)';
    rs_borderwidth           = 'The whidth of the border';
    rs_focusframeon          = 'Indicates when the button has focus';
    rs_showborder            = 'Allows to show or hide a border';
    rs_hoveron               = 'Allows to show or hide a hoverevent';
    rs_foregrdfocuson        = 'Indicates when the button has focus';

    rs_msgbuttonlook         = 'MsgButtonLook';
    rs_visible               = 'Allows to show or hide the control, and all of its children';
    rs_msgbuttonstyle        = 'The geometric shape of the messagebutton';
    rs_width                 = 'The width of the messagebutton';
    rs_height                = 'The height of the messagebutton';
    rs_msgalignment          = 'The position of the messagebutton';
    rs_posfac                = 'Position factor,affects the position, only active if alSE,alSW,alNW,alNE,alW,alE,alN,alS';
    rs_calcinvisible         = 'Is required if the MessagButton is only visible at runtime';
    rs_showpressed           = 'Allows to show or hide the pressedoption ';
    rs_prcolblendval         = 'How translucent the pressedcolor is (0=transparent, 255=opaque)';

    rs_colors                = 'Colors';
    rs_mbsettings            = 'MultiButton Settings:';
    rs_msgbsettings          = 'MessageButton Settings:';
    rs_popupcopy             = 'Copy the color';
    rs_popuppaste            = 'Paste a color';
    rs_colorstart            = 'The start color of the button (for color gradient)';
    rs_colorend              = 'The end color of the button (for color gradient)';
    rs_bordercolor           = 'The color of the border';
    rs_hoverstartcol         = 'The startcolor of a hoverevent (for color gradient)';
    rs_hoverendcol           = 'The endcolor of a hoverevent (for color gradient)';
    rs_hoverfontcol          = 'The color of the Caption during one hoverevent';
    rs_fontcolor             = 'The color of the Caption';
    rs_focuscolor            = 'The color of the Fokusframe/Foregroundfocus when the control has the focus';
    rs_pressedstcol          = 'The starting color of the button when it is pressed (for color gradient)';
    rs_pressedendcol         = 'The end color of the button when it is pressed (for color gradient)';
    rs_prssdfontcol          = 'The color of the text of the caption when the button is pressed';

    rs_msgcolorstart         = 'The start color of the messagebutton ( for color gradient)';
    rs_msgcolorend           = 'The end color of the messagebutton ( for color gradient)';
    rs_msgbordercolor        = 'The color of the border';
    rs_msgfontcolor          = 'The color of the font used in messagebutton';
    rs_msghovercolor         = 'The color of a hoverevent';
    rs_pressedcol            = 'The color of the messagebutton when it is pressed';

    rs_fonts                 = 'Fonts';
    rs_CaptionAlignment      = 'Alignment of the text in the caption (left, center, right)';
    rs_CaptionLayout         = 'Alignment of the text in the caption (top, center, bottom)';
    rs_CaptionHorMargin      = 'The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)';
    rs_CaptionVerMargin      = 'The vertical distance of the text in the text rectangle (only effective with tlTop)';
    rs_fontdialog            = 'The font to be used for text display in this button, opens a dialog';
    rs_wordbreak             = 'Allows a line break in the caption';

    //for unit CustomPen
     rs_penerror            = 'Error creating the pen';
     rs_clNoneColorStart    = 'Notice! BackgrdColorStart owns the value clNone. Background is unvisibel.';
     rs_clNoneColorEnd      = 'Notice! BackgrdColorEnd owns the value clNone. Background is unvisibel.';

    //for unit mp_customPanelStyleEditor
     rs_new                 = 'New';
     rs_grid                = 'Grid';
     rs_notice              = 'The artboard is 500 x 500 pixels';

    //helpmenu
     rs_lang                = 'en';
     rs_muhelp              = 'Multis-Help';
     rs_muhelperror         = 'Multis-helpfiles not found!';
     rs_packfileerror       = 'packagefiles.xml not found!';

implementation

end.

