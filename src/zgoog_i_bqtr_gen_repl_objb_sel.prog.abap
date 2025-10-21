**********************************************************************
*  Copyright 2025 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-t04.
  PARAMETERS: p_psel RADIOBUTTON GROUP rb4 USER-COMMAND uc1 DEFAULT 'X',
              p_pfil RADIOBUTTON GROUP rb4.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-t05.
  PARAMETERS: p_pupl RADIOBUTTON GROUP rb5 USER-COMMAND uc1 DEFAULT 'X'  MODIF ID lp,
              p_pdwn RADIOBUTTON GROUP rb5  MODIF ID lp.
  PARAMETERS: p_upath TYPE string MODIF ID l5u LOWER CASE,
              p_dpath TYPE string MODIF ID l5d LOWER CASE.
  SELECTION-SCREEN:  PUSHBUTTON /40(30) btn_down USER-COMMAND but1 MODIF ID l5d.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b7 WITH FRAME TITLE TEXT-t07.
  PARAMETERS: p_activ AS CHECKBOX MODIF ID m07 USER-COMMAND uc2 DEFAULT 'X'.
  SELECTION-SCREEN COMMENT /01(79) TEXT-001  MODIF ID m07.
  PARAMETERS: p_durmi TYPE btcpmin DEFAULT '01' MODIF ID m07,
              p_min   RADIOBUTTON GROUP rb7 USER-COMMAND uc7 DEFAULT 'X' MODIF ID m07,
              p_hour  RADIOBUTTON GROUP rb7 MODIF ID m07.
SELECTION-SCREEN END OF BLOCK b7.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
  PARAMETERS: p_trkey TYPE /goog/trkey MODIF ID lc1 MATCHCODE OBJECT /goog/sh_bqtr_transf_key,
              p_infot TYPE rstrang_gui_sourcetlogo MODIF ID ift,
              p_infop TYPE rscompname MODIF ID ifp,
              p_inftb TYPE tabname MODIF ID itb,
              p_extmd TYPE RSBUPDMODE MODIF ID exm,
              p_chain TYPE rspc_chain MODIF ID cha,
              p_trig  TYPE rspc_variant MODIF ID tri,
              p_info  TYPE rsinfoarea MODIF ID inf MATCHCODE OBJECT rsshapdia.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  btn_down = 'Download Template'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'BUT1'.
      lcl_file_handler=>download_template( ).

  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dpath.
  lcl_file_handler=>f4_directory( CHANGING cv_file = p_dpath ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upath.
  lcl_file_handler=>f4_file( CHANGING cv_file = p_upath ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_trig.
  lcl_output_handler=>f4_rspc_trigger( CHANGING cv_trig = p_trig ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_psel =  abap_true.
      IF screen-group1 = 'L5D' OR screen-group1 = 'L5U' OR screen-group1 = 'LP'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ELSE.
      CASE screen-group1.
        WHEN 'LC1' OR 'BDS' OR 'CDS' OR 'APL' OR 'ADS' OR 'CHA' OR 'TRI' OR 'INF' OR 'IFT' OR 'IFP' OR 'ITB' OR 'EXM'.
          screen-input     = 0.
          screen-invisible = 1.
      ENDCASE.
      IF p_pupl = abap_true.
        IF screen-group1 = 'L5D'.
          screen-input     = 0.
          screen-invisible = 1.
        ENDIF.
      ELSE.
        IF screen-group1 = 'L5U'.
          screen-input     = 0.
          screen-invisible = 1.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
