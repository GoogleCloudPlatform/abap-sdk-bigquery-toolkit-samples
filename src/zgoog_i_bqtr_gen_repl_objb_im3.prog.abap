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
CLASS lcl_output_handler IMPLEMENTATION.
  METHOD change_screen.
    LOOP AT SCREEN.
      IF iv_mandt = abap_false.
        IF screen-group1 = 'M01'.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = 'M01'.
          screen-input     = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD f4_transport_request.
    CALL FUNCTION 'TR_F4_REQUESTS'
      IMPORTING
        ev_selected_request = cv_trkorr                 " Selected request
      .
  ENDMETHOD.

  METHOD f4_bw_logical_system.
    DATA: lt_ret_tab   TYPE STANDARD TABLE OF ddshretval,
          lt_field_tab TYPE STANDARD TABLE OF dfies,
          ls_ret_tab   TYPE ddshretval,
          lv_value     TYPE help_info-fldvalue.

    FIELD-SYMBOLS: <ls_field>       TYPE dfies.

    SELECT * FROM rslogsysodp INTO TABLE @DATA(lt_rslogsysodp).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'LOGSYS'
        value_org       = 'S'
        value           = lv_value
      TABLES
        value_tab       = lt_rslogsysodp
        field_tab       = lt_field_tab
        return_tab      = lt_ret_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      READ TABLE lt_ret_tab INTO ls_ret_tab INDEX 1.
      IF sy-subrc = 0.
        cv_logsys = ls_ret_tab-fieldval.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD f4_application_component.
    DATA: lt_ret_tab   TYPE STANDARD TABLE OF ddshretval,
          lt_field_tab TYPE STANDARD TABLE OF dfies,
          ls_ret_tab   TYPE ddshretval,
          lv_value     TYPE help_info-fldvalue.

    FIELD-SYMBOLS: <ls_field>       TYPE dfies.

    SELECT roappl~applnm, roapplt~txtlg FROM roappl LEFT JOIN
      roapplt ON roappl~applnm = roapplt~applnm
              AND langu = @sy-langu
      INTO TABLE @DATA(lt_roappl).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'APPLNM'
        value_org       = 'S'
        value           = lv_value
      TABLES
        value_tab       = lt_roappl
        field_tab       = lt_field_tab
        return_tab      = lt_ret_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      READ TABLE lt_ret_tab INTO ls_ret_tab INDEX 1.
      IF sy-subrc = 0.
        cv_appl = ls_ret_tab-fieldval.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD f4_rspc_trigger.
    DATA: lt_ret_tab   TYPE STANDARD TABLE OF ddshretval,
          lt_field_tab TYPE STANDARD TABLE OF dfies,
          ls_ret_tab   TYPE ddshretval,
          lv_value     TYPE help_info-fldvalue.

    FIELD-SYMBOLS: <ls_field>       TYPE dfies.

    SELECT rspctriggert~variante, txtlg  FROM rspctrigger LEFT JOIN
      rspctriggert ON rspctrigger~variante = rspctriggert~variante
              AND langu = @sy-langu
      INTO TABLE @DATA(lt_trigger).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'APPLNM'
        value_org       = 'S'
        value           = lv_value
      TABLES
        value_tab       = lt_trigger
        field_tab       = lt_field_tab
        return_tab      = lt_ret_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      READ TABLE lt_ret_tab INTO ls_ret_tab INDEX 1.
      IF sy-subrc = 0.
        cv_trig = ls_ret_tab-fieldval.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
