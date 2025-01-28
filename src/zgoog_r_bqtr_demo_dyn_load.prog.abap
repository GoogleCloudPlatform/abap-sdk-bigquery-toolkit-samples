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
REPORT zgoog_r_bqtr_demo_dyn_load.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_trkey TYPE /goog/trkey MODIF ID lc1 MATCHCODE OBJECT /goog/sh_bqtr_transf_key OBLIGATORY,
              p_dsrc  TYPE string OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS: p_max  TYPE i.

SELECTION-SCREEN END OF BLOCK b2.


CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS process.

ENDCLASS.


START-OF-SELECTION.
  lcl_main=>process( ).

CLASS lcl_main IMPLEMENTATION.

  METHOD process.

    DATA: lo_ex TYPE REF TO cx_root.
    DATA: lo_bqtr TYPE REF TO /goog/cl_bqtr_data_load.

    TRY.

        CREATE OBJECT lo_bqtr
          EXPORTING
            iv_mass_tr_key = p_trkey
            iv_data_source = p_dsrc.

        DATA: lt_data_ref TYPE REF TO data.
        FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.


        CREATE DATA lt_data_ref TYPE STANDARD TABLE OF (p_dsrc).
        ASSIGN lt_data_ref->* TO <lt_data>.

        SELECT *
               FROM (p_dsrc)
               INTO TABLE <lt_data>
          UP TO p_max ROWS.

        DATA: lv_error_code TYPE sysubrc.
        DATA: lt_return TYPE bapiret2_t.

        lo_bqtr->replicate_data(
         EXPORTING
           it_content     = <lt_data>
         IMPORTING
           ev_error_code  = lv_error_code
           et_return      = lt_return ).

        IF lv_error_code = 0.
          DATA: lv_count TYPE i.
          lv_count = lines( <lt_data> ).
          WRITE: / |{ lv_count } Records sent to BigQuery|.
        ELSE.
          DATA: ls_return TYPE bapiret2.

          LOOP AT lt_return INTO ls_return.
            WRITE: / | Error sending data to BigQuery|.
            DATA: lv_message_out TYPE string.
            MESSAGE ID ls_return-id TYPE 'I' NUMBER  ls_return-number
            WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
            INTO lv_message_out.
            WRITE: / lv_message_out.
          ENDLOOP.
        ENDIF.

      CATCH cx_root INTO lo_ex.
        WRITE: / lo_ex->get_text( ).
        RETURN.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
