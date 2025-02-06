**********************************************************************
*  Copyright 2024 Google LLC                                         *
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

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS: p_create RADIOBUTTON GROUP rb1 USER-COMMAND uc1 DEFAULT 'X',
              p_clean  RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
  PARAMETERS: p_sall  RADIOBUTTON GROUP rb2 USER-COMMAND uc2 DEFAULT 'X',
              p_sbwds RADIOBUTTON GROUP rb2,
              p_sadso RADIOBUTTON GROUP rb2,
              p_strnf RADIOBUTTON GROUP rb2,
              p_sdtp  RADIOBUTTON GROUP rb2,
              p_strig RADIOBUTTON GROUP rb2,
              p_schai RADIOBUTTON GROUP rb2.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-t06.
  PARAMETERS: p_mandt AS CHECKBOX MODIF ID m02 USER-COMMAND uc2 DEFAULT 'X',
              p_mfnam TYPE name_feld MODIF ID m01 DEFAULT 'MANDT',
              p_activ AS CHECKBOX MODIF ID m02 USER-COMMAND uc2 DEFAULT 'X'.
  SELECTION-SCREEN COMMENT /01(79) TEXT-001  MODIF ID m02.

SELECTION-SCREEN END OF BLOCK b6.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
  PARAMETERS: p_trkey TYPE /goog/trkey MODIF ID lc1 MATCHCODE OBJECT /goog/sh_bqtr_transf_key,
              p_dsnam TYPE rsoltpsourcer MODIF ID bds MATCHCODE OBJECT rsds_datasource,
              p_cds   TYPE ddlname MODIF ID cds MATCHCODE OBJECT rsds_datasource,
              p_logsy TYPE rsslogsys MODIF ID lc1,
              p_appl  TYPE rsapplnm MODIF ID apl,
              p_adso  TYPE char9 MODIF ID ads MATCHCODE OBJECT rsoadso_search,
              p_chain TYPE rspc_chain MODIF ID cha,
              p_trig  TYPE rspc_variant MODIF ID tri,
              p_info  TYPE rsinfoarea MODIF ID inf MATCHCODE OBJECT rsshapdia.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  btn_down = 'Download Template'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'UC1'.
      CLEAR: p_sbwds, p_sadso, p_strnf, p_sdtp, p_strig, p_schai.
    WHEN 'BUT1'.
      lcl_file_handler=>download_template( ).

  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dpath.
  lcl_file_handler=>f4_directory( CHANGING cv_file = p_dpath ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upath.
  lcl_file_handler=>f4_file( CHANGING cv_file = p_upath ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_logsy.
  lcl_output_handler=>f4_bw_logical_system( CHANGING cv_logsys = p_logsy ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_appl.
  lcl_output_handler=>f4_application_component( CHANGING cv_appl = p_appl ).

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
        WHEN 'LC1' OR 'CDS' OR 'APL' OR 'ADS' OR 'CHA' OR 'TRI' OR 'INF'.
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
    IF p_mandt = abap_false.
      IF screen-group1 = 'M01'.
        screen-input     = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF p_clean = abap_true.
      IF screen-name CS 'P_S'.
        IF ( screen-name CS 'P_SADSO' OR screen-name CS 'P_STRIG' OR
             screen-name CS 'P_SCHAI' OR screen-name CS 'P_SBWDS' OR
             screen-name CS 'P_SALL' ).
          screen-input     = 1.
          screen-invisible = 0.
        ELSE.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
      ELSEIF screen-group1 = 'M01' OR screen-group1 = 'M02'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ELSEIF p_create = abap_true.
      IF screen-name CS 'P_S'.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.

    IF p_sadso IS NOT INITIAL.
      IF p_clean = abap_true AND NOT ( screen-group1 = 'ADS' OR screen-group1 IS INITIAL ).
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ELSEIF p_strig IS NOT INITIAL.
      IF p_clean = abap_true AND NOT ( screen-group1 = 'TRI' OR screen-group1 IS INITIAL ).
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ELSEIF p_schai IS NOT INITIAL.
      IF p_clean = abap_true AND NOT ( screen-group1 = 'CHA' OR screen-group1 IS INITIAL ).
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ELSEIF p_sbwds IS NOT INITIAL.
      IF p_clean = abap_true AND NOT ( screen-group1 = 'CDS' OR screen-group1 IS INITIAL ).
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
