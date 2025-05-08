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


CLASS lcl_cdsview_mass_cr IMPLEMENTATION.

  METHOD prepare_data.
    DATA : lv_lines TYPE i.

    CLEAR: lv_lines, ev_keyfieldlist, ev_nonkeyfieldlist.

    IF sy-subrc = 0 AND gt_dd03l_extract[] IS NOT INITIAL.
      CLEAR : ev_keyfieldlist,ev_nonkeyfieldlist, lv_lines.
      DESCRIBE TABLE gt_dd03l_extract LINES lv_lines.
      LOOP AT gt_dd03l_extract INTO gs_dd03l_extract WHERE fieldname(1) <> '.'.
        DATA(lv_alias) = get_field_alias( gs_dd03l_extract ).
        IF gs_dd03l_extract-keyflag = 'X'.

          IF gs_dd03l_extract-fieldname = 'MANDT' AND ( p_old = abap_false OR p_mandt = abap_false ).
            " Skip MANDT for root view entities as MANDT is not supported.
            " Skip MANDT if mandt is excluded.
            CONTINUE.
          ELSEIF gs_dd03l_extract-fieldname = 'MANDT' AND p_old = abap_true.
            IF sy-tabix <> lv_lines.
              CONCATENATE ev_keyfieldlist 'key' gs_dd03l_extract-fieldname lv_alias ',' cl_abap_char_utilities=>newline INTO ev_keyfieldlist SEPARATED BY space.
            ELSE.
              CONCATENATE ev_keyfieldlist 'key' gs_dd03l_extract-fieldname lv_alias ' ' cl_abap_char_utilities=>newline INTO ev_keyfieldlist SEPARATED BY space.
            ENDIF.
          ELSE.
            IF sy-tabix <> lv_lines.
              CONCATENATE ev_keyfieldlist 'key' gs_dd03l_extract-fieldname lv_alias ',' cl_abap_char_utilities=>newline INTO ev_keyfieldlist SEPARATED BY space.
            ELSE.
              CONCATENATE ev_keyfieldlist 'key' gs_dd03l_extract-fieldname lv_alias ' ' cl_abap_char_utilities=>newline INTO ev_keyfieldlist SEPARATED BY space.
            ENDIF.
          ENDIF.
        ELSE.
          IF sy-tabix <> lv_lines.
            CONCATENATE ev_nonkeyfieldlist gs_dd03l_extract-fieldname lv_alias ',' cl_abap_char_utilities=>newline INTO ev_nonkeyfieldlist SEPARATED BY space.
          ELSE .
            CONCATENATE ev_nonkeyfieldlist gs_dd03l_extract-fieldname lv_alias ' ' cl_abap_char_utilities=>newline INTO ev_nonkeyfieldlist SEPARATED BY space.
          ENDIF.
        ENDIF.
        CLEAR gs_dd03l_extract.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD process_file_details.
    DATA lo_mass_cds TYPE REF TO lcl_cdsview_mass_cr.
    CREATE OBJECT lo_mass_cds.

    IF gt_excel_data IS NOT INITIAL.

      CLEAR: gv_input_rec_cnt, gv_output_rec_cnt.
      DESCRIBE TABLE gt_excel_data LINES gv_input_rec_cnt.


      LOOP AT gt_excel_data ASSIGNING FIELD-SYMBOL(<fs_excel_data>).
        TRANSLATE <fs_excel_data>-tablename TO UPPER CASE.
        IF <fs_excel_data>-cdsviewname IS INITIAL. "generate cds view name if not provided in the input file
          CONCATENATE 'Z_' <fs_excel_data>-tablename '_CDS' INTO <fs_excel_data>-cdsviewname.
        ENDIF.
        TRANSLATE <fs_excel_data>-cdsviewname TO UPPER CASE.
      ENDLOOP.

    ENDIF.

    SORT gt_excel_data BY cdsviewname.
    DELETE ADJACENT DUPLICATES FROM gt_excel_data.
    IF gt_excel_data IS NOT INITIAL.

      "check if cds view is already created
      SELECT tadir~obj_name,tadir~devclass,e070~strkorr,ddldependency~objectname  FROM tadir
                                         INNER JOIN e071
                                         ON tadir~obj_name = e071~obj_name
                                          INNER JOIN e070
                                          ON e071~trkorr = e070~trkorr
                                          INNER JOIN ddldependency
                                          ON tadir~obj_name = ddldependency~ddlname
                                          INTO TABLE @gt_tr_cds_details
                                          FOR ALL ENTRIES IN @gt_excel_data
                                          WHERE tadir~obj_name = @gt_excel_data-cdsviewname
                                            AND tadir~delflag = @abap_false.
    ENDIF.

    CLEAR gs_output.
    IF gt_tr_cds_details IS NOT INITIAL.
      LOOP AT gt_excel_data ASSIGNING FIELD-SYMBOL(<fs_excel_data2>).
        READ TABLE gt_tr_cds_details INTO gs_tr_cds_details WITH KEY obj_name = <fs_excel_data2>-cdsviewname.
        IF sy-subrc = 0.
          gs_output-tablename = <fs_excel_data2>-tablename.
          gs_output-cdsviewname = <fs_excel_data2>-cdsviewname.
          gs_output-sqlviewname = gs_tr_cds_details-objectname.
          gs_output-package = gs_tr_cds_details-devclass.
          gs_output-transport = gs_tr_cds_details-strkorr.
          gs_output-status = 'CDS view already exists'.
          APPEND gs_output TO gt_output.
          <fs_excel_data2>-tablename = 'X'.
        ENDIF.
        CLEAR: gs_tr_cds_details, gs_output.
      ENDLOOP.
    ENDIF.

    CLEAR gt_dd03l_extract[].
    DELETE gt_excel_data WHERE tablename = 'X'.
    SORT gt_excel_data BY tablename.
    DESCRIBE TABLE gt_excel_data LINES gv_output_rec_cnt.
    IF gt_excel_data[] IS NOT INITIAL.
      LOOP AT gt_excel_data INTO gs_excel_data.
        "fetch the table metadata
*        CALL FUNCTION 'IUUC_DD03L_EXTRACT'
*          EXPORTING
*            iv_tabname   = gs_excel_data-tablename
*          TABLES
*            it_fieldlist = gt_dd03l_extract[].
        SELECT *
          FROM dd03l
          INTO TABLE gt_dd03l_extract
          WHERE tabname = gs_excel_data-tablename.

        IF sy-subrc = 0 AND gt_dd03l_extract IS NOT INITIAL.
          IF lo_mass_cds IS BOUND.
            CLEAR: gv_keyfieldlist, gv_nonkeyfieldlist.

            "prepare the cds body for key and non key fields
            CALL METHOD lo_mass_cds->prepare_data
              IMPORTING
                ev_keyfieldlist    = gv_keyfieldlist
                ev_nonkeyfieldlist = gv_nonkeyfieldlist.

            "prepare the cds view header, annotations and combine with body to get the entire cds view string
            CALL METHOD lo_mass_cds->create_objects
              IMPORTING
                ev_tabdesc      = DATA(lv_tabdesc)
                ev_ddl_source   = DATA(lv_ddl_source)
                ev_final_string = DATA(lv_final_string)
                ev_sql_view     = DATA(lv_sql_view).

            "create the cds view object
            CALL METHOD lo_mass_cds->create_cds
              EXPORTING
                iv_tabdesc      = lv_tabdesc
                iv_ddl_source   = lv_ddl_source
                iv_final_string = lv_final_string
                iv_sql_view     = lv_sql_view.

          ENDIF.
        ELSE.
          CLEAR gs_output.
          gs_output-tablename = gs_excel_data-tablename.
          gs_output-cdsviewname = 'N/A'.
          gs_output-sqlviewname = 'N/A'.
          gs_output-package = 'N/A'.
          gs_output-transport = 'N/A'.
          gs_output-status = 'Invalid Table Name'.
          APPEND gs_output TO gt_output.
          gv_output_rec_cnt = gv_output_rec_cnt - 1.
          CLEAR gs_output.
        ENDIF.
        CLEAR: gs_excel_data, gt_dd03l_extract[], gv_keyfieldlist, gv_nonkeyfieldlist.
      ENDLOOP.
    ENDIF.

    IF gt_output[] IS NOT INITIAL.
      CALL METHOD lo_mass_cds->display_op.
    ELSE.
      MESSAGE 'No data selected' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD create_objects.
    DATA: lv_sql_view(16) TYPE c,
          lv_final_string TYPE string.

    ev_ddl_source = gs_excel_data-cdsviewname.

    DATA(lv_def_view) = 'define view'.
    DATA(lv_sel_from) = 'as select from'.
    DATA : lv_lb(1)       TYPE c VALUE '{',
           lv_rb(1)       TYPE c VALUE '}',
           lv_sqlviewdesc TYPE string.

    SELECT SINGLE ddtext FROM dd02t INTO @ev_tabdesc WHERE tabname = @gs_excel_data-tablename AND  ddlanguage = 'E'.
    CONCATENATE ev_tabdesc 'CDS View' INTO ev_tabdesc SEPARATED BY space.
    CONCATENATE ev_tabdesc 'SQL View' INTO lv_sqlviewdesc SEPARATED BY space.
    CONCATENATE lv_sel_from gs_excel_data-tablename INTO DATA(lv_sel_table) SEPARATED BY space.


    "prepare the sql view name
    IF gs_excel_data-sqlviewname IS INITIAL.
      CONCATENATE 'Z_' gs_excel_data-tablename '_SQL' INTO lv_sql_view.
    ELSE.
      lv_sql_view = gs_excel_data-sqlviewname.
    ENDIF.
    TRANSLATE lv_sql_view TO UPPER CASE.
    CONCATENATE '@AbapCatalog.sqlViewName:' space '''' lv_sql_view '''' INTO DATA(lv_sqlview_ann).

    DATA(lv_string1) = lv_sqlview_ann.
    DATA : lv_excel_tabname(30) TYPE c.
    lv_excel_tabname = gs_excel_data-tablename.
    TRANSLATE lv_excel_tabname TO LOWER CASE.

    IF p_old = abap_true.

      "annotations for header , delta and extractor enablement
      CONCATENATE cl_abap_char_utilities=>newline
                  '@AbapCatalog.compiler.compareFilter: true' cl_abap_char_utilities=>newline
                  '@AbapCatalog.preserveKey: true' cl_abap_char_utilities=>newline
                  '@AccessControl.authorizationCheck: #NOT_REQUIRED' cl_abap_char_utilities=>newline
                  '@EndUserText.label:' space '''' lv_sqlviewdesc '''' cl_abap_char_utilities=>newline INTO DATA(lv_string2) .
      CONCATENATE 'define view' gs_excel_data-cdsviewname 'as select from' gs_excel_data-tablename  INTO DATA(lv_string4) SEPARATED BY space.
    ELSE.
      CLEAR: lv_string1.
      CONCATENATE '@EndUserText.label:' space '''' lv_sqlviewdesc '''' cl_abap_char_utilities=>newline INTO lv_string2 .
      CONCATENATE 'define root view entity' gs_excel_data-cdsviewname 'as select from' gs_excel_data-tablename cl_abap_char_utilities=>newline INTO lv_string4 SEPARATED BY space.
    ENDIF.
    DATA(lv_string3) = '@Analytics.dataExtraction.enabled: true' && cl_abap_char_utilities=>newline &&
                       '@Analytics.dataCategory: #FACT' && cl_abap_char_utilities=>newline &&
                       '@Analytics.dataExtraction.delta.changeDataCapture.automatic: true' && cl_abap_char_utilities=>newline.

    CONCATENATE lv_lb gv_keyfieldlist INTO DATA(lv_string5) SEPARATED BY space.
    CONCATENATE gv_nonkeyfieldlist lv_rb INTO DATA(lv_string6) SEPARATED BY space.
    TRANSLATE lv_string5 TO LOWER CASE.
    TRANSLATE lv_string6 TO LOWER CASE.

    "concatenate different sections into a single string
    CONCATENATE lv_string1 lv_string2 lv_string3 lv_string4 lv_string5 lv_string6 INTO ev_final_string SEPARATED BY space.

    ev_sql_view = lv_sql_view.
  ENDMETHOD.

  METHOD create_cds.


    DATA: lv_putstate   TYPE objstate VALUE 'N',
          lv_name       TYPE ddlname,
          lv_devclass   TYPE devclass,
          lv_ddl_source TYPE ddlname,
          ls_ddddlsrcv  TYPE ddddlsrcv.

    DATA(lref_dd_ddl_handler) = cl_dd_ddl_handler_factory=>create( ).

    CLEAR : lv_ddl_source,lv_devclass.
    DATA:lo_header  TYPE REF TO cl_salv_form_layout_grid,
         lo_h_label TYPE REF TO cl_salv_form_label.

    lv_ddl_source = gs_excel_data-cdsviewname.


    CREATE OBJECT lo_header.

    CLEAR: ls_ddddlsrcv.
    ls_ddddlsrcv-source = iv_final_string.
    ls_ddddlsrcv-ddtext = iv_tabdesc.
    ls_ddddlsrcv-ddlanguage = sy-langu.
    ls_ddddlsrcv-ddlname = iv_ddl_source.
    TRY.

        CALL METHOD lref_dd_ddl_handler->save
          EXPORTING
            name         = lv_ddl_source
            put_state    = lv_putstate
            ddddlsrcv_wa = ls_ddddlsrcv.

      CATCH cx_dd_ddl_save.
    ENDTRY.
    IF sy-subrc = 0.
      TRY.
          CALL METHOD lref_dd_ddl_handler->activate
            EXPORTING
              name = lv_ddl_source.

        CATCH cx_dd_ddl_activate.

      ENDTRY.
    ENDIF.

    DATA : lv_subrc TYPE sysubrc.
    CLEAR lv_subrc.

    TRY.

        CALL METHOD lref_dd_ddl_handler->write_tadir
          EXPORTING
            objectname = gs_excel_data-cdsviewname
            devclass   = p_pkg "'ZCDS_WRAPPER_DS'
            prid       = -1
          RECEIVING
            rc         = lv_subrc.

      CATCH cx_dd_ddl_save. " Exception While Writing TADIR Entry
    ENDTRY.


    DATA : lv_order  TYPE trkorr,
           lv_task   TYPE trkorr,
           lv_subrc2 TYPE sysubrc,
           lv_subrc3 TYPE sysubrc.

    CLEAR: lv_order, lv_task, lv_subrc2, lv_subrc3.
    IF lv_subrc = 0.
      CALL METHOD lref_dd_ddl_handler->write_trkorr
        EXPORTING
          trkorr     = p_tr
          objectname = gs_excel_data-cdsviewname
          prid       = -1
        IMPORTING
          order      = lv_order
          task       = lv_task
          rc         = lv_subrc2.
      IF lv_subrc2 = 0 AND iv_sql_view IS NOT INITIAL.

        CALL METHOD lref_dd_ddl_handler->write_trkorr
          EXPORTING
            trkorr     = p_tr
            objectname = iv_sql_view
            prid       = -1
          IMPORTING
            order      = lv_order
            task       = lv_task
            rc         = lv_subrc3.
      ENDIF.

      CLEAR: gs_output.
      IF lv_subrc2 = 0. "AND lv_subrc2 = 0.

        gs_output-tablename = gs_excel_data-tablename.
        gs_output-cdsviewname = gs_excel_data-cdsviewname.
        gs_output-sqlviewname = iv_sql_view.
        gs_output-package = p_pkg.
        gs_output-transport = p_tr.
        gs_output-status = 'CDS view Created'.
      ELSE.
        gs_output-tablename = gs_excel_data-tablename.
        gs_output-cdsviewname = gs_excel_data-cdsviewname.
        gs_output-sqlviewname = iv_sql_view.
        gs_output-package = p_pkg.
        gs_output-transport = p_tr.
        gs_output-status = 'Some error occured'.
      ENDIF.
      IF p_old = abap_false.
        CLEAR: gs_output-sqlviewname.
      ENDIF.
      APPEND gs_output TO gt_output.

    ENDIF.
    CLEAR gs_output.

  ENDMETHOD.

  METHOD display_op.
    DATA: go_alv        TYPE REF TO cl_salv_table,
          go_columns    TYPE REF TO cl_salv_columns,
          go_funcs      TYPE REF TO cl_salv_functions,
          go_ex         TYPE REF TO cx_root,
          lo_header     TYPE REF TO cl_salv_form_layout_grid,
          lo_h_label    TYPE REF TO cl_salv_form_label,
          lt_colname    TYPE salv_t_column_ref,
          column        TYPE REF TO cl_salv_column,
          lv_txt_medium TYPE scrtext_m,
          gr_content    TYPE REF TO cl_salv_form_element,
          lo_cds        TYPE REF TO lcl_cdsview_mass_cr.


    IF gt_output IS NOT INITIAL.
      TRY .
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = go_alv
            CHANGING
              t_table      = gt_output
          ).
          CREATE OBJECT lo_header.
          lo_h_label = lo_header->create_label( row = 1 column = 1 ).

          lo_h_label->set_text( 'Title for the ALV' ).

          go_alv->set_top_of_list( lo_header ).

          CREATE OBJECT lo_cds.
          IF lo_cds IS BOUND.
            CALL METHOD top_of_page
              CHANGING
                ir_content = gr_content.
          ENDIF.

          CALL METHOD go_alv->set_top_of_list( gr_content ).

          go_columns = go_alv->get_columns( ).
          lt_colname = go_columns->get( ).

          LOOP AT lt_colname INTO DATA(ls_colname).

            column = go_columns->get_column( to_upper( ls_colname-columnname ) ).

            lv_txt_medium = ls_colname-columnname.

            column->set_medium_text( lv_txt_medium ).

          ENDLOOP.

          go_funcs = go_alv->get_functions( ).
          go_funcs->set_all( ).

        CATCH cx_salv_msg INTO go_ex.
          MESSAGE go_ex TYPE 'E'.

        CATCH cx_salv_not_found INTO DATA(lo_salv).
          DATA(lv_error) = lo_salv->get_text( ).
          MESSAGE lv_error TYPE 'E'.
      ENDTRY.

    ENDIF.


    IF gt_output IS NOT INITIAL.
      go_alv->display( ).
    ENDIF.


  ENDMETHOD.

  METHOD top_of_page.
    DATA : lr_grid  TYPE REF TO cl_salv_form_layout_grid,

           lr_text  TYPE REF TO cl_salv_form_text,

           lr_label TYPE REF TO cl_salv_form_label,

           lr_head  TYPE string.


    CREATE OBJECT lr_grid.


    lr_grid->create_header_information( row     = 1
                                        column  = 1
                                        text    = lr_head
                                        tooltip = lr_head ).

** Add Row **

    lr_grid->add_row( ).

** Add Label in Grid **

    lr_label = lr_grid->create_label( row     = 2
                                      column  = 1
                                      text    = 'No of CDS views requested :  '
                                      tooltip = 'No of CDS views requested :  ' ).

** Add Text in The Grid **

    lr_text = lr_grid->create_text( row     = 2
                                    column  = 2
                                    text    = gv_input_rec_cnt
                                    tooltip = gv_input_rec_cnt ).


** Add Row **

    lr_grid->add_row( ).

** Add Label in Grid **

    lr_label = lr_grid->create_label( row     = 3
                                      column  = 1
                                      text    = 'No of CDS views created :  '
                                      tooltip = 'No of CDS views created :  ' ).

** Add Text in The Grid **

    lr_text = lr_grid->create_text( row     = 3
                                    column  = 2
                                    text    = gv_output_rec_cnt
                                    tooltip = gv_output_rec_cnt ).

** Set Label and Text Link **

    lr_label->set_label_for( lr_text ).

** Move lr_grid to lr_content **

    ir_content = lr_grid.
  ENDMETHOD.

  METHOD get_field_alias.

    CONSTANTS: lc_alias TYPE char4 VALUE ' AS '.

    DATA: lv_fieldname_orig TYPE string,
          lv_alias          TYPE string.

    lv_fieldname_orig = is_field-fieldname.
    TRANSLATE lv_fieldname_orig TO LOWER CASE.

    lv_alias = is_field-fieldname.

    /goog/cl_bqtr_utility=>prepare_text( CHANGING cv_text = lv_alias ).

    IF lv_fieldname_orig <> lv_alias.
      CONCATENATE ' AS' lv_alias INTO rv_alias SEPARATED BY space.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
