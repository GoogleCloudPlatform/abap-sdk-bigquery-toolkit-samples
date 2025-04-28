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

CLASS lcl_file_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS download_template.
    CLASS-METHODS f4_file CHANGING cv_file TYPE string.
    CLASS-METHODS f4_directory CHANGING cv_file TYPE string.
    CLASS-METHODS csv_to_itab IMPORTING iv_file  TYPE string
                              CHANGING  ct_table TYPE gtt_excel_data.

ENDCLASS.

CLASS lcl_cdsview_mass_cr DEFINITION.
  PUBLIC SECTION.
    METHODS: process_file_details, display_op.

    METHODS get_field_alias IMPORTING is_field        TYPE dd03l
                            RETURNING VALUE(rv_alias) TYPE string.

    METHODS prepare_data EXPORTING ev_keyfieldlist    TYPE string
                                   ev_nonkeyfieldlist TYPE string.

    METHODS create_objects EXPORTING ev_ddl_source   TYPE string
                                     ev_tabdesc      TYPE string
                                     ev_final_string TYPE string
                                     ev_sql_view     TYPE char16.

    METHODS create_cds IMPORTING iv_ddl_source   TYPE string
                                 iv_tabdesc      TYPE string
                                 iv_final_string TYPE string
                                 iv_sql_view     TYPE char16.

    METHODS : top_of_page CHANGING ir_content TYPE REF TO cl_salv_form_element.

ENDCLASS.
