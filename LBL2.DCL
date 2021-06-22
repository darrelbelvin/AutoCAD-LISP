lbl : dialog {
        label = " Line/Curve Annotation ";

        :row{
                : boxed_radio_column {
                        key             = "Selection";
                        label           = "Selection Options";
                        alignment       = centered;
                        fixed_height    = true;
                        fixed_width     = true;
                        mnemonic        = "S";
                        is_tab_stop     = true;
                        :radio_button{
                                key     = "Points";
                                label   = "Pick by Points";
                        }
                        :radio_button{
                                key     = "Entities";
                                label   = "Pick by Line/Arc";
                        }
                }
                :popup_list{
                        key             = "style";
                        label           = "Text Style:";
                        fixed_width     = true;
                        alignment       = centered;
                        edit_width      = 10;
                        mnemonic        = "T";
                }

        }
        :row{spacer_1;}
        :row{
                :column{ :spacer { width=1;}}
                :column{
                        //label="Line Options";
                        :button{
                                key         = "LBOD";
                                label       = " Bearing/Distance ";
                        }
                        :button{
                                key         = "LDOB";
                                label       = " Distance/Bearing ";
                        }
                        :button{
                                key         = "LBDO";
                                label       = " Bearing-Distance/";
                        }
                        :button{
                                key         = "LBDU";
                                label       = " /Bearing-Distance";
                        }
                        :button{
                                key         = "LDO";
                                label       = "     Distance/    ";
                        }
                        :button{
                                key         = "LDU";
                                label       = "     /Distance    ";
                        }
                        :button{
                                key         = "LBO";
                                label       = "     Bearing/     ";
                        }
                        :button{
                                key         = "LBU";
                                label       = "     /Bearing     ";
                        }
                        :button{
                                key         = "LHBOD";
                                label       = "  Hang  Brg/Dist  ";
                        }
                        :button{
                                key         = "LHBD";
                                label       = "  Hang  Brg,Dist  ";
                        }
                        spacer_1;
                        :button{
                                key         = "LLST";
                                label       = "  List  Brg,Dist  ";
                        }
                }
                :column{ :spacer { width=5;}}
                :column{
                        //label="Curve Options";
                        :button{
                                key         = "CROLI";
                                label       = " Radius ( Length  ";
                        }
                        :button{
                                key         = "CRILO";
                                label       = " Radius ) Length  ";
                        }
                        :button{
                                key         = "CRLO";
                                label       = " Radius-Length (  ";
                        }
                        :button{
                                key         = "CRLI";
                                label       = " ( Radius-Length  ";
                        }
                        :button{
                                key         = "CRO";
                                label       = "     Radius (     ";
                        }
                        :button{
                                key         = "CRI";
                                label       = "     Radius )     ";
                        }
                        :button{
                                key         = "CLO";
                                label       = "     Length (     ";
                        }
                        :button{
                                key         = "CLI";
                                label       = "     Length )     ";
                        }
                        :button{
                                key         = "CHRL";
                                label       = "     Hang  R/L    ";
                        }
                       :button{
                                key         = "CHRLDT";
                                label       = "   Hang R/L/D/T   ";
                        }
                        spacer_1;
                        :button{
                                key         = "CLST";
                                label       = "    List Curve    ";
                        }

                }
                :column{ :spacer { width=1;}}
        }
        :row{spacer_1;}
        :row{
                :column{
                        :spacer{height=1;}
                }
                :button{
                        key         = "cancel";
                        label       = " Cancel ";
                        alignment   = centered;
                        fixed_width = true;
                        is_cancel   = true;
                        mnemonic    = "C";
                }
               :button{
                        key         = "help";
                        label       = "  Help  ";
                        alignment   = centered;
                        fixed_width = true;
                        mnemonic    = "H";
                }
                :button{
                        key         = "info";
                        label       = "  Info  ";
                        alignment   = centered;
                        fixed_width = true;
                        mnemonic    = "I";
                }
                :column{
                        :spacer{height=1;}
                }
        }
        :row{spacer_1;}
}
b_logo : dialog {
    label = " Program Information ";    // Overridden by caller
    : row {
        : image {
            key = "logo";
            width = 8;
            aspect_ratio = 1.0;
            fixed_height = true;
            alignment = top;
            color = dialog_background;
        }
        :spacer_1{}
        : paragraph {
            : text_part {
                key = "text1";
                width = 45;
            }
            : text_part {
                key = "text2";
                width = 45;
            }
            : text_part {
                key = "text3";
                width = 45;
            }
            : text_part {
                key = "text4";
                width = 45;
            }
            : text_part {
                key = "text5";
                width = 45;
            }
            : text_part {
                key = "text6";
                width = 45;
            }
        }
    }

    : ok_button {                     // Permit exit via either ENTER
        is_cancel = true;             //   or CTRL-C.
    }
}

