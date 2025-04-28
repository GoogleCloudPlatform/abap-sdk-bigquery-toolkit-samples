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

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_pupl RADIOBUTTON GROUP rb5 USER-COMMAND uc1 DEFAULT 'X'  MODIF ID lp,
              p_pdwn RADIOBUTTON GROUP rb5  MODIF ID lp.
  PARAMETERS: p_upath TYPE string MODIF ID l5u LOWER CASE,
              p_dpath TYPE string MODIF ID l5d LOWER CASE.
  SELECTION-SCREEN:  PUSHBUTTON /40(30) btn_down USER-COMMAND but1 MODIF ID l5d.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: "p_file TYPE localfile OBLIGATORY,
    p_pkg TYPE devclass OBLIGATORY,
    p_tr  TYPE trkorr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_old   AS CHECKBOX,
              p_mandt AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  btn_down = 'Download Template'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'BUT1'.
      lcl_file_handler=>download_template( ).
  ENDCASE.

  "validate package
  SELECT SINGLE devclass FROM tdevc INTO @DATA(lv_devc) WHERE devclass = @p_pkg.
  IF sy-subrc <> 0.
    MESSAGE 'Invalid Package' TYPE 'E'.
  ENDIF.

  "validate TR
  SELECT SINGLE trkorr FROM e070 INTO @DATA(lv_tr) WHERE trkorr = @p_tr.
  IF sy-subrc <> 0.
    MESSAGE 'Invalid TR' TYPE 'E'.
  ENDIF.

  CLEAR: lv_devc, lv_tr.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dpath.
  lcl_file_handler=>f4_directory( CHANGING cv_file = p_dpath ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upath.
  lcl_file_handler=>f4_file( CHANGING cv_file = p_upath ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
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
    MODIFY SCREEN.
  ENDLOOP.
