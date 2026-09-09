
CLASS ycl_aai_db DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_db.

    TYPES ty_file_content_bin_t TYPE STANDARD TABLE OF yde_aai_bin_data WITH EMPTY KEY.

    ALIASES create_id FOR yif_aai_db~create_id.
    ALIASES delete_chat FOR yif_aai_db~delete_chat.
    ALIASES persist_chat FOR yif_aai_db~persist_chat.
    ALIASES persist_message FOR yif_aai_db~persist_message.
    ALIASES persist_system_instructions FOR yif_aai_db~persist_system_instructions.
    ALIASES persist_tools FOR yif_aai_db~persist_tools.
    ALIASES persist_files FOR yif_aai_db~persist_files.
    ALIASES get_files FOR yif_aai_db~get_files.
    ALIASES get_chat FOR yif_aai_db~get_chat.
    ALIASES block_chat FOR yif_aai_db~block_chat.
    ALIASES release_chat FOR yif_aai_db~release_chat.
    ALIASES is_chat_blocked FOR yif_aai_db~is_chat_blocked.
    ALIASES request_approval FOR yif_aai_db~request_approval.
    ALIASES get_approval FOR yif_aai_db~get_approval.
    ALIASES update_approval FOR yif_aai_db~update_approval.

    ALIASES mt_messages FOR yif_aai_db~mt_messages.
    ALIASES mt_tools FOR yif_aai_db~mt_tools.

    ALIASES m_api FOR yif_aai_db~m_api.
    ALIASES m_id FOR yif_aai_db~m_id.
    ALIASES m_user FOR yif_aai_db~m_user.

    ALIASES mc_scope_one_time FOR yif_aai_db~mc_scope_one_time.
    ALIASES mc_scope_chat FOR yif_aai_db~mc_scope_chat.

    METHODS constructor
      IMPORTING
        i_api     TYPE csequence
        i_id      TYPE yde_aai_id OPTIONAL
        i_preload TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _convert_file_content
      IMPORTING
        i_filename      TYPE string
        i_content       TYPE string
      EXPORTING
        e_t_content_bin TYPE ty_file_content_bin_t.

ENDCLASS.



CLASS YCL_AAI_DB IMPLEMENTATION.


  METHOD constructor.

    me->m_api = i_api.

    me->m_user = cl_abap_context_info=>get_user_technical_name( ).

    IF i_id IS NOT INITIAL.

      me->m_id = i_id.

      IF i_preload = abap_true.

        me->get_chat(
          EXPORTING
            i_id         = me->m_id
          IMPORTING
            e_t_messages = me->mt_messages
            e_t_tools    = me->mt_tools
        ).

      ENDIF.

    ENDIF.

    IF me->m_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

    ENDIF.

  ENDMETHOD.


  METHOD is_chat_blocked.

    SELECT SINGLE blocked
      FROM yaai_chat
      WHERE id = @me->m_id
      INTO @r_blocked.

  ENDMETHOD.


  METHOD yif_aai_db~block_chat.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    e_blocked = abap_false.

    IF me->m_id IS INITIAL.
      RETURN.
    ENDIF.

    IF me->m_id IS NOT INITIAL.

      lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE @abap_true
        FROM yaai_chat
        WHERE id = @me->m_id
          AND username IN @lt_rng_user
          INTO @DATA(l_exist).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    UPDATE yaai_chat
      SET blocked = @abap_true
      WHERE id = @me->m_id.

    e_blocked = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD yif_aai_db~create_id.

    CLEAR r_id.

    TRY.

        r_id = cl_system_uuid=>create_uuid_c32_static( ).

      CATCH cx_uuid_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD yif_aai_db~delete_chat.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

    DELETE FROM yaai_chat
      WHERE id = @me->m_id
        AND username IN @lt_rng_user.

    e_deleted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    IF e_deleted = abap_true.

      DELETE FROM yaai_msg
        WHERE id = @me->m_id.

      DELETE FROM yaai_msg_file
        WHERE id = @me->m_id.

      DELETE FROM yaai_log
        WHERE id = @me->m_id.

      DELETE FROM yaai_async
        WHERE chat_id = @me->m_id. "#EC CI_NOFIELD

      DELETE FROM yaai_tools
        WHERE id = @me->m_id.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~get_approval.

    e_requested = abap_false.
    e_approved = abap_false.

    CLEAR e_scope.

    " One-time approval
    SELECT scope, approved
      FROM yaai_approval
     WHERE id = @me->m_id
       AND class_name = @i_class_name
       AND method_name = @i_method_name
       AND used = @abap_false
       AND scope = @mc_scope_one_time
      INTO (@e_scope, @e_approved)
      UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc = 0.
      e_requested = abap_true.
    ELSE.

      " Chat-wide approval
      SELECT scope, approved
        FROM yaai_approval
       WHERE id = @me->m_id
         AND class_name = @i_class_name
         AND method_name = @i_method_name
         AND scope = @mc_scope_chat
        INTO (@e_scope, @e_approved)
        UP TO 1 ROWS.
      ENDSELECT.

      IF sy-subrc = 0.
        e_requested = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~get_chat.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    DATA l_id TYPE yaai_chat-id.

    FREE: e_t_messages,
          e_t_msg_data,
          e_t_tools.

    IF i_id IS SUPPLIED.
      l_id = i_id.
    ENDIF.

    IF l_id IS INITIAL.
      l_id = me->m_id.
    ENDIF.

    lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

    SELECT SINGLE id
      FROM yaai_chat
      WHERE id = @l_id
        AND api = @me->m_api
        AND username IN @lt_rng_user
      INTO @l_id.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT id, seqno, msg, msg_date, msg_time
      FROM yaai_msg
      WHERE id = @l_id
      ORDER BY PRIMARY KEY
      INTO CORRESPONDING FIELDS OF TABLE @e_t_messages.

    IF i_ui = abap_false.

      SELECT id, seqno, prompt
        FROM yaai_prompt
        WHERE id = @l_id
        ORDER BY PRIMARY KEY
        INTO TABLE @DATA(lt_prompt).

      LOOP AT lt_prompt ASSIGNING FIELD-SYMBOL(<ls_prompt>).

        READ TABLE e_t_messages ASSIGNING FIELD-SYMBOL(<ls_message>)
          WITH KEY id = <ls_prompt>-id
                   seqno = <ls_prompt>-seqno
          BINARY SEARCH.

        IF sy-subrc = 0.

          <ls_message>-msg = <ls_prompt>-prompt.

        ENDIF.

      ENDLOOP.

    ENDIF.

    SELECT id, class_name, method_name, proxy_class, description
      FROM yaai_tools
      WHERE id = @i_id
      INTO CORRESPONDING FIELDS OF TABLE @e_t_tools.

    IF e_t_msg_data IS REQUESTED.

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      LOOP AT e_t_messages ASSIGNING FIELD-SYMBOL(<l_msg>).

        APPEND INITIAL LINE TO e_t_msg_data ASSIGNING FIELD-SYMBOL(<ls_msg>).

        lo_aai_util->deserialize(
          EXPORTING
            i_json = <l_msg>-msg
          IMPORTING
            e_data = <ls_msg>
        ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~get_files.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    DATA: lt_files         TYPE SORTED TABLE OF yaai_msg_file
            WITH UNIQUE KEY id seqno filename,
          lt_files_content TYPE SORTED TABLE OF yaai_msg_file
            WITH NON-UNIQUE KEY id filename seqno.

    DATA l_bin_data TYPE xstring.

    FREE e_t_files.

    IF me->m_id IS NOT INITIAL.

      lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE @abap_true
        FROM yaai_chat
        WHERE id = @me->m_id
          AND username IN @lt_rng_user
          INTO @DATA(l_exist).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    SELECT DISTINCT id, seqno, filename
      FROM yaai_msg_file
      WHERE id = @me->m_id
        AND seqno = @i_seqno
      ORDER BY id, seqno, filename
      INTO CORRESPONDING FIELDS OF TABLE @lt_files.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT id, filename, seqno, line_no, file_type, file_size, content
      FROM yaai_msg_file
      WHERE id = @me->m_id
        AND seqno = @i_seqno
      ORDER BY id, filename, seqno, line_no
      INTO CORRESPONDING FIELDS OF TABLE @lt_files_content.

    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>).

      CLEAR l_bin_data.

      LOOP AT lt_files_content ASSIGNING FIELD-SYMBOL(<ls_files_content>)
        WHERE id = <ls_file>-id
          AND seqno = <ls_file>-seqno
          AND filename = <ls_file>-filename.

        CONCATENATE l_bin_data <ls_files_content>-content INTO l_bin_data IN BYTE MODE.

      ENDLOOP.

      DATA(lo_zip) = NEW cl_abap_zip( ).

      lo_zip->load(
        EXPORTING
          zip             = l_bin_data
        EXCEPTIONS
          zip_parse_error = 1
          OTHERS          = 2
      ).

      IF sy-subrc <> 0.
        CLEAR lo_zip.
        CONTINUE.
      ENDIF.

      lo_zip->get(
        EXPORTING
          name                    = CONV #( <ls_files_content>-filename )
        IMPORTING
          content                 = DATA(l_content_bin)
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3
      ).

      IF sy-subrc <> 0.
        CLEAR lo_zip.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO e_t_files ASSIGNING FIELD-SYMBOL(<ls_e_file>).

      <ls_e_file>-filename = <ls_files_content>-filename.
      <ls_e_file>-file_type = <ls_files_content>-file_type.
      <ls_e_file>-file_size = <ls_files_content>-file_size.
      <ls_e_file>-content = cl_abap_codepage=>convert_from( l_content_bin ).

      CLEAR lo_zip.

    ENDLOOP.

  ENDMETHOD.


  METHOD yif_aai_db~persist_chat.

    DATA l_id TYPE yde_aai_id.

    CLEAR: e_id,
           e_persisted.

    IF i_id IS SUPPLIED.
      l_id = i_id.
    ENDIF.

    IF l_id IS INITIAL.
      l_id = me->create_id( ).
    ENDIF.

    DATA(ls_chat) = VALUE yaai_chat( id = l_id
                                     api = me->m_api
                                     username = me->m_user
                                     chat_date = sy-datlo
                                     chat_time = sy-timlo ).

    INSERT yaai_chat FROM @ls_chat.

    e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    e_id = l_id.

  ENDMETHOD.


  METHOD yif_aai_db~persist_files.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    IF me->m_id IS NOT INITIAL.

      lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE @abap_true
        FROM yaai_chat
        WHERE id = @me->m_id
          AND username IN @lt_rng_user
          INTO @DATA(l_exist).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    LOOP AT i_t_files ASSIGNING FIELD-SYMBOL(<ls_file>).

      DATA(ls_chat_file) = VALUE yaai_msg_file( id = me->m_id
                                                seqno = i_seqno
                                                filename = <ls_file>-filename
                                                line_no = 1
                                                file_type = <ls_file>-file_type
                                                file_size = <ls_file>-file_size ).

      me->_convert_file_content(
        EXPORTING
          i_filename  = <ls_file>-filename
          i_content = <ls_file>-content
        IMPORTING
          e_t_content_bin = DATA(lt_content_bin)
      ).

      LOOP AT lt_content_bin ASSIGNING FIELD-SYMBOL(<l_content_bin>).

        ls_chat_file-content = <l_content_bin>.

        INSERT yaai_msg_file FROM ls_chat_file.

        ls_chat_file-line_no = ls_chat_file-line_no + 1.

      ENDLOOP.

      CLEAR ls_chat_file.

    ENDLOOP.

  ENDMETHOD.


  METHOD yif_aai_db~persist_message.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    DATA: l_id     TYPE yde_aai_chat_id,
          l_seqno  TYPE yde_aai_seqno,
          l_tokens TYPE yde_aai_tokens,
          l_model  TYPE yde_aai_model.

    CLEAR: e_id,
           e_seqno,
           e_persisted.

    IF i_id IS SUPPLIED.

      l_id = i_id.

    ENDIF.

    IF l_id IS INITIAL.

      l_id = me->m_id.

    ENDIF.

    IF l_id IS NOT INITIAL.

      lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE @abap_true
        FROM yaai_chat
        WHERE id = @l_id
          AND username IN @lt_rng_user
          INTO @DATA(l_exist).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    IF l_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

      l_id = me->m_id.

      l_seqno = 1.

      e_seqno = l_seqno.

    ENDIF.

    e_id = l_id.

    IF l_seqno = 0.

      SELECT MAX( seqno )
        FROM yaai_msg
        WHERE id = @l_id
        INTO @l_seqno.

      l_seqno = l_seqno + 1.

      e_seqno = l_seqno.

    ENDIF.

    IF i_tokens IS SUPPLIED.
      l_tokens = i_tokens.
    ENDIF.

    IF i_model IS SUPPLIED.
      l_model = i_model.
    ENDIF.

    IF i_message IS SUPPLIED.

      DATA(ls_msg) = VALUE yaai_msg( id = l_id
                                     seqno = l_seqno
                                     msg = i_message
                                     tokens = l_tokens
                                     model = l_model ).

    ENDIF.

    IF i_data IS SUPPLIED.

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      ls_msg = VALUE yaai_msg( id = l_id
                               seqno = l_seqno
                               msg = lo_aai_util->serialize( i_data )
                               tokens = l_tokens
                               model = l_model ).

    ENDIF.

    IF i_prompt IS SUPPLIED AND i_prompt IS NOT INITIAL.

      DATA(ls_prompt) = VALUE yaai_prompt( id = l_id
                                           seqno = l_seqno
                                           prompt = lo_aai_util->serialize( i_prompt ) ).

    ENDIF.

    IF ls_msg IS NOT INITIAL.

      ls_msg-msg_date = sy-datlo.
      ls_msg-msg_time = sy-timlo.
      ls_msg-async_task_id = i_async_task_id.

      INSERT yaai_msg FROM @ls_msg.

      e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      IF ls_prompt IS NOT INITIAL.

        INSERT yaai_prompt FROM @ls_prompt.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~persist_system_instructions.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    DATA: l_id    TYPE yde_aai_id,
          l_seqno TYPE i.

    CLEAR: e_id,
           e_persisted.

    IF i_id IS SUPPLIED.

      l_id = i_id.

    ENDIF.

    IF l_id IS INITIAL.

      l_id = me->m_id.

    ENDIF.

    IF l_id IS NOT INITIAL.

      lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE @abap_true
        FROM yaai_chat
        WHERE id = @l_id
          AND username IN @lt_rng_user
          INTO @DATA(l_exist).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    IF l_id IS INITIAL.

      me->persist_chat(
        IMPORTING
          e_id = me->m_id
      ).

      l_id = me->m_id.

    ENDIF.

    e_id = l_id.

    l_seqno = 0.

    SELECT SINGLE @abap_true
      FROM yaai_msg
      WHERE id = @l_id
        AND seqno = @l_seqno
        INTO @DATA(l_exists).

    IF sy-subrc = 0.
      e_persisted = abap_true.
    ENDIF.

    IF i_system_instructions IS SUPPLIED.

      DATA(ls_msg) = VALUE yaai_msg( id = l_id
                                     seqno = l_seqno
                                     msg = i_system_instructions ).

    ENDIF.

    IF i_data IS SUPPLIED.

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      ls_msg = VALUE yaai_msg( id = l_id
                               seqno = l_seqno
                               msg = lo_aai_util->serialize( i_data ) ).

    ENDIF.

    IF ls_msg IS NOT INITIAL.

      ls_msg-msg_date = sy-datlo.
      ls_msg-msg_time = sy-timlo.

      INSERT yaai_msg FROM @ls_msg.

      e_persisted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~persist_tools.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    e_persisted = abap_true.

    IF i_t_tools[] IS INITIAL.
      RETURN.
    ENDIF.

    IF me->m_id IS NOT INITIAL.

      lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE @abap_true
        FROM yaai_chat
        WHERE id = @me->m_id
          AND username IN @lt_rng_user
          INTO @DATA(l_exist).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    DATA(lt_tools) = i_t_tools.

    LOOP AT lt_tools ASSIGNING FIELD-SYMBOL(<ls_tool>).

      <ls_tool>-id = me->m_id.

    ENDLOOP.

    INSERT yaai_tools FROM TABLE @lt_tools ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.


  METHOD yif_aai_db~release_chat.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    e_released = abap_false.

    IF me->m_id IS INITIAL.
      RETURN.
    ENDIF.

    IF me->m_id IS NOT INITIAL.

      lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE @abap_true
        FROM yaai_chat
        WHERE id = @me->m_id
          AND username IN @lt_rng_user
          INTO @DATA(l_exist).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    UPDATE yaai_chat
      SET blocked = @abap_false
      WHERE id = @me->m_id.

    e_released = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD yif_aai_db~request_approval.

    e_created = abap_false.

    SELECT SINGLE @abap_true
      FROM yaai_approval
     WHERE id = @me->m_id
       AND class_name = @i_class_name
       AND method_name = @i_method_name
       AND used = @abap_false
      INTO @DATA(l_exist).

    IF sy-subrc = 0.
      " There is already an approval request created for this tool
      e_created = abap_true.
      RETURN.
    ENDIF.

    DATA(ls_approval) = VALUE yaai_approval( id = me->m_id
                                             class_name = i_class_name
                                             method_name = i_method_name
                                             scope = mc_scope_one_time ).

    SELECT MAX( seqno )
      FROM yaai_approval
      WHERE id = @me->m_id
        AND class_name = @i_class_name
        AND method_name = @i_method_name
      INTO @ls_approval-seqno.

    ls_approval-seqno = ls_approval-seqno + 1.

    INSERT yaai_approval FROM ls_approval.

    IF sy-subrc = 0.
      e_created = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_db~update_approval.

    DATA lt_rng_user TYPE RANGE OF syst-uname.

    DATA l_short_time_stamp TYPE timestamp.

    e_updated = abap_false.

    IF me->m_id IS NOT INITIAL.

      lt_rng_user = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE @abap_true
        FROM yaai_chat
        WHERE id = @me->m_id
          AND username IN @lt_rng_user
          INTO @DATA(l_exist).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

    ENDIF.

    DATA(l_scope) = mc_scope_one_time.

    IF i_approved IS SUPPLIED.

      IF i_approved = abap_true.

        IF i_scope IS NOT INITIAL.
          l_scope = i_scope.
        ENDIF.

        GET TIME STAMP FIELD l_short_time_stamp.

        UPDATE yaai_approval
           SET approved = @i_approved,
               approved_at = @l_short_time_stamp,
               scope = @l_scope
         WHERE id = @me->m_id
           AND class_name = @i_class_name
           AND method_name = @i_method_name
           AND approved = @abap_false.

        IF sy-subrc = 0.
          e_updated = abap_true.
        ENDIF.

      ELSE.

        " If reversing an approval then delete all approvals for the tool
        DELETE FROM yaai_approval
          WHERE id = @me->m_id
            AND class_name = @i_class_name
            AND method_name = @i_method_name
            AND approved = @abap_true.

        IF sy-subrc = 0.
          e_updated = abap_true.
        ENDIF.

      ENDIF.

    ENDIF.

    " Used state is one way only ...
    IF i_used IS SUPPLIED AND i_used = abap_true.

      GET TIME STAMP FIELD l_short_time_stamp.

      UPDATE yaai_approval
         SET used = @i_used,
             used_at = @l_short_time_stamp
       WHERE id = @me->m_id
         AND class_name = @i_class_name
         AND method_name = @i_method_name
         AND used = @abap_false.

      IF sy-subrc = 0.
        e_updated = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD _convert_file_content.

    DATA: l_offset    TYPE i,
          l_line_no   TYPE i,
          l_remaining TYPE i.

    FREE e_t_content_bin.

    DATA(l_content_bin) = cl_abap_codepage=>convert_to( source = i_content ).

    DATA(lo_zip) = NEW cl_abap_zip( ).

    lo_zip->add( name = i_filename
                 content = l_content_bin ).

    DATA(l_zip) = lo_zip->save( ).

    DATA(l_len) = xstrlen( l_zip ).

    DATA(l_max_len) = dbmaxlen( l_zip ).

    WHILE l_offset < l_len.

      APPEND INITIAL LINE TO e_t_content_bin ASSIGNING FIELD-SYMBOL(<l_content_bin>).

      l_remaining = l_len - l_offset.

      IF l_remaining > l_max_len.
        <l_content_bin> = l_zip+l_offset(l_max_len).
      ELSE.
        <l_content_bin> = l_zip+l_offset(l_remaining).
      ENDIF.

      l_offset = l_offset + l_max_len.

    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
