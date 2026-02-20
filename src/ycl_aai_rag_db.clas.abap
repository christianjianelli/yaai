CLASS ycl_aai_rag_db DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_rag_db.

    ALIASES create FOR yif_aai_rag_db~create.
    ALIASES read   FOR yif_aai_rag_db~read.
    ALIASES update FOR yif_aai_rag_db~update.
    ALIASES delete FOR yif_aai_rag_db~delete.
    ALIASES query  FOR yif_aai_rag_db~query.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_aai_rag_data_t TYPE STANDARD TABLE OF yaai_rag_data WITH DEFAULT KEY.

    METHODS convert_file_content
      IMPORTING
        i_id         TYPE uuid
        i_filename   TYPE string
        i_content    TYPE string
      EXPORTING
        e_t_rag_data TYPE ty_aai_rag_data_t.

ENDCLASS.



CLASS ycl_aai_rag_db IMPLEMENTATION.

  METHOD yif_aai_rag_db~create.

    DATA: ls_aai_rag TYPE yaai_rag.

    DATA: l_offset    TYPE i,
          l_line_no   TYPE i,
          l_remaining TYPE i.


    CLEAR: e_id, e_error.

    IF i_filename IS NOT INITIAL.

      SELECT id FROM yaai_rag
        WHERE filename = @i_filename
        INTO @DATA(l_id)
        UP TO 1 ROWS.
      ENDSELECT.

      IF sy-subrc = 0.
        e_error = |File { i_filename } already exists in the database|.
        RETURN.
      ENDIF.

    ENDIF.

    TRY.

        ls_aai_rag-id = cl_system_uuid=>create_uuid_x16_static( ).

      CATCH cx_uuid_error ##NO_HANDLER.
    ENDTRY.

    ls_aai_rag-filename = i_filename.
    ls_aai_rag-description = i_description.
    ls_aai_rag-keywords = i_keywords.

    me->convert_file_content(
      EXPORTING
        i_id         = ls_aai_rag-id
        i_filename   = i_filename
        i_content    = i_content
      IMPORTING
        e_t_rag_data = DATA(lt_aai_rag_data)
    ).

    IF lt_aai_rag_data[] IS NOT INITIAL.

      INSERT yaai_rag FROM @ls_aai_rag.
      INSERT yaai_rag_data FROM TABLE @lt_aai_rag_data.

      e_id = ls_aai_rag-id.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_rag_db~read.

    DATA l_bin_data TYPE xstring.

    CLEAR: e_id,
           e_filename,
           e_description,
           e_keywords,
           e_content,
           e_error.


    SELECT FROM yaai_rag FIELDS id, filename, description, keywords
      WHERE id = @i_id
         OR filename = @i_filename
      INTO @DATA(ls_rag)
      UP TO 1 ROWS.                                     "#EC CI_NOORDER
    ENDSELECT.

    IF sy-subrc <> 0.
      e_error = |File { i_filename } not found in the database|.
      RETURN.
    ENDIF.

    SELECT FROM yaai_rag_data FIELDS id, line_no, bin_data
      WHERE id = @ls_rag-id
      ORDER BY PRIMARY KEY
      INTO TABLE @DATA(lt_rag_data).

    IF sy-subrc <> 0.
      e_error = |File { i_filename } not found in the database|.
      RETURN.
    ENDIF.

    LOOP AT lt_rag_data ASSIGNING FIELD-SYMBOL(<ls_rag_data>).

      CONCATENATE l_bin_data <ls_rag_data>-bin_data INTO l_bin_data IN BYTE MODE.

    ENDLOOP.

    IF l_bin_data IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zip) = NEW cl_abap_zip( ).

    lo_zip->load(
      EXPORTING
        zip             = l_bin_data
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_zip->get(
      EXPORTING
        name                    = CONV #( ls_rag-filename )
      IMPORTING
        content                 = DATA(l_content_bin)
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3
    ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    e_id = ls_rag-id.
    e_filename = ls_rag-filename.
    e_description = ls_rag-description.
    e_keywords = ls_rag-keywords.

    e_content = cl_abap_codepage=>convert_from( l_content_bin ).

  ENDMETHOD.


  METHOD yif_aai_rag_db~update.

    e_updated = abap_false.

    CLEAR e_error.

    SELECT id, filename
      FROM yaai_rag
      WHERE id = @i_id
         OR filename = @i_filename
      INTO @DATA(ls_rag)
      UP TO 1 ROWS.                                     "#EC CI_NOORDER
    ENDSELECT.

    IF sy-subrc = 0.

      IF i_description IS SUPPLIED.

        UPDATE yaai_rag
          SET description = @i_description
          WHERE id = @ls_rag-id.

        IF sy-subrc <> 0.
          e_error = 'Error while updating the document'.
          RETURN.
        ENDIF.

      ENDIF.

      IF i_keywords IS SUPPLIED.

        UPDATE yaai_rag
          SET keywords = @i_keywords
          WHERE id = @ls_rag-id.

        IF sy-subrc <> 0.
          ROLLBACK WORK.
          e_error = 'Error while updating the document'.
          RETURN.
        ENDIF.

      ENDIF.

    ELSE.
      e_error = |File { i_filename } not found in the database|.
      RETURN.
    ENDIF.

    IF i_append = abap_true AND i_content IS SUPPLIED.

      me->read(
        EXPORTING
          i_id      = ls_rag-id
        IMPORTING
          e_content = DATA(l_content)
      ).

      l_content = l_content && cl_abap_char_utilities=>cr_lf && i_content.

    ENDIF.

    IF i_content IS SUPPLIED.

      IF l_content IS INITIAL.
        l_content = i_content.
      ENDIF.

      me->convert_file_content(
        EXPORTING
          i_id         = ls_rag-id
          i_filename   = CONV #( ls_rag-filename )
          i_content    = l_content
        IMPORTING
          e_t_rag_data = DATA(lt_aai_rag_data)
      ).

      IF lt_aai_rag_data IS NOT INITIAL.

        DELETE FROM yaai_rag_data
          WHERE id = @ls_rag-id.

        INSERT yaai_rag_data FROM TABLE @lt_aai_rag_data.

        IF sy-subrc <> 0.
          ROLLBACK WORK.
          e_updated = abap_false.
          e_error = 'Error while updating the file content'.
          RETURN.
        ENDIF.

      ENDIF.

    ENDIF.

    e_updated = abap_true.

  ENDMETHOD.


  METHOD yif_aai_rag_db~delete.

    e_deleted = abap_false.

    CLEAR e_error.

    SELECT FROM yaai_rag FIELDS id
      WHERE id = @i_id
         OR filename = @i_filename
      INTO @DATA(l_id)
      UP TO 1 ROWS.                                     "#EC CI_NOORDER
    ENDSELECT.

    IF sy-subrc = 0.

      DELETE FROM yaai_rag_data
        WHERE id = @l_id.

      DELETE FROM yaai_rag
        WHERE id = @l_id.

      IF sy-subrc = 0.
        e_deleted = abap_true.
      ELSE.

        e_error = |Document { i_id } { i_filename } not found in the database|.

        RETURN.

      ENDIF.

    ELSE.

      e_error = |Document { i_id } { i_filename } not found in the database|.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_rag_db~query.

    DATA: lt_rng_filename    TYPE RANGE OF yaai_rag-filename,
          lt_rng_description TYPE RANGE OF yaai_rag-description,
          lt_rng_keywords    TYPE RANGE OF yaai_rag-keywords.

    FREE e_t_documents.

    IF i_filename IS NOT INITIAL.
      lt_rng_filename = VALUE #( ( sign = 'I' option = 'CP' low = |*{ i_filename }*| ) ).
    ENDIF.

    IF i_description IS NOT INITIAL.
      lt_rng_description = VALUE #( ( sign = 'I' option = 'CP' low = |*{ i_description }*| ) ).
    ENDIF.

    IF i_keywords IS NOT INITIAL.
      lt_rng_keywords = VALUE #( ( sign = 'I' option = 'CP' low = |*{ i_keywords }*| ) ).
    ENDIF.

    SELECT id, filename, description, keywords
      FROM yaai_rag
      WHERE filename IN @lt_rng_filename
        AND description IN @lt_rng_description
        AND keywords IN @lt_rng_keywords
        INTO TABLE @DATA(lt_documents).

    e_t_documents = CORRESPONDING #( lt_documents ).

  ENDMETHOD.

  METHOD convert_file_content.

    DATA: l_offset    TYPE i,
          l_line_no   TYPE i,
          l_remaining TYPE i.

    FREE e_t_rag_data.

    DATA(l_content_bin) = cl_abap_codepage=>convert_to( source = i_content ).

    DATA(lo_zip) = NEW cl_abap_zip( ).

    lo_zip->add( name = i_filename
                 content = l_content_bin ).

    DATA(l_zip) = lo_zip->save( ).

    DATA(l_len) = xstrlen( l_zip ).

    DATA(l_max_len) = dbmaxlen( l_zip ).

    WHILE l_offset < l_len.

      APPEND INITIAL LINE TO e_t_rag_data ASSIGNING FIELD-SYMBOL(<ls_rag_data>).

      <ls_rag_data>-id = i_id.

      l_line_no = l_line_no + 1.

      l_remaining = l_len - l_offset.

      <ls_rag_data>-line_no = l_line_no.

      IF l_remaining > l_max_len.
        <ls_rag_data>-bin_data = l_zip+l_offset(l_max_len).
      ELSE.
        <ls_rag_data>-bin_data = l_zip+l_offset(l_remaining).
      ENDIF.

      l_offset = l_offset + l_max_len.

    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
