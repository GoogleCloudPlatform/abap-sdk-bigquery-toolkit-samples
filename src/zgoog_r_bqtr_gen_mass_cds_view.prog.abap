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
REPORT zgoog_r_bqtr_gen_mass_cds_view.

INCLUDE zgoog_i_bqtr_gen_mass_cds_top. "data declaration.
INCLUDE zgoog_i_bqtr_gen_mass_cds_sel. "selection screen
INCLUDE zgoog_i_bqtr_gen_mass_cds_c01. "class definition and implementation
INCLUDE zgoog_i_bqtr_gen_mass_cds_c02. "class definition and implementation


START-OF-SELECTION.

  DATA: lo_cds_mass TYPE REF TO lcl_cdsview_mass_cr.
  CREATE OBJECT lo_cds_mass.

  IF lo_cds_mass IS BOUND.

    lcl_file_handler=>csv_to_itab( EXPORTING iv_file  = p_upath
                                   CHANGING  ct_table = gt_excel_data
                                           ).
    IF gt_excel_data[] IS NOT INITIAL.
      "process user input data
      CALL METHOD lo_cds_mass->process_file_details.
    ELSE.
      MESSAGE 'No tables specified in the input file / no header provided. Please check the file' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.
