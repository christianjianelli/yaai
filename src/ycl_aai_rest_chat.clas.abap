CLASS ycl_aai_rest_chat DEFINITION
  PUBLIC
  INHERITING FROM ycl_aai_rest_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_msg_s,
             seqno        TYPE yde_aai_seqno,
             msg          TYPE yde_aai_chat_msg,
             msg_date     TYPE yde_aai_msg_date,
             msg_time     TYPE yde_aai_msg_time,
             total_tokens TYPE yde_aai_tokens,
             model        TYPE yde_aai_model,
           END OF ty_msg_s,

           BEGIN OF ty_msg_file_s,
             seqno     TYPE yde_aai_seqno,
             filename  TYPE yaai_msg_file-filename,
             file_type TYPE yaai_msg_file-file_type,
             file_size TYPE yaai_msg_file-file_size,
             content   TYPE string,
           END OF ty_msg_file_s,

           BEGIN OF ty_log_s,
             id       TYPE string,
             seqno    TYPE yde_aai_seqno,
             message  TYPE yde_aai_log_message,
             username TYPE usnam,
             log_date TYPE yde_aai_chat_date,
             log_time TYPE yde_aai_chat_time,
             msgid    TYPE symsgid,
             msgno    TYPE symsgno,
             msgty    TYPE bapi_mtype,
           END OF ty_log_s,

           BEGIN OF ty_tool_s,
             class_name  TYPE string,
             method_name TYPE string,
             proxy_class TYPE string,
             description TYPE string,
           END OF ty_tool_s,

           BEGIN OF ty_approval_s,
             class_name  TYPE string,
             method_name TYPE string,
             scope       TYPE string,
             approved    TYPE abap_bool,
             approved_at TYPE string,
             used        TYPE abap_bool,
             used_at     TYPE string,
           END OF ty_approval_s,

           BEGIN OF ty_task_flow_s,
             id                 TYPE string,
             task_id            TYPE string,
             task_name          TYPE yde_aai_task_name,
             previous_task_id   TYPE string,
             previous_task_name TYPE yde_aai_task_name,
             task_status        TYPE yde_aai_task_status,
           END OF ty_task_flow_s,

           ty_task_flow_t TYPE STANDARD TABLE OF ty_task_flow_s WITH EMPTY KEY,

           ty_msg_t       TYPE STANDARD TABLE OF ty_msg_s WITH EMPTY KEY,

           ty_msg_files_t TYPE STANDARD TABLE OF ty_msg_file_s WITH EMPTY KEY,

           ty_log_t       TYPE STANDARD TABLE OF ty_log_s WITH EMPTY KEY,

           ty_tools_t     TYPE STANDARD TABLE OF ty_tool_s WITH EMPTY KEY,

           ty_approvals_t TYPE STANDARD TABLE OF ty_approval_s WITH EMPTY KEY,

           BEGIN OF ty_chat_query_s,
             id         TYPE string,
             api        TYPE yde_aai_api,
             username   TYPE usnam,
             chat_date  TYPE yde_aai_chat_date,
             chat_time  TYPE yde_aai_chat_time,
             max_seq_no TYPE i,
             tokens     TYPE i,
             blocked    TYPE abap_bool,
           END OF ty_chat_query_s,

           ty_chat_t TYPE STANDARD TABLE OF ty_chat_query_s WITH EMPTY KEY,

           BEGIN OF ty_chat_s,
             id          TYPE string,
             api         TYPE yde_aai_api,
             username    TYPE usnam,
             chat_date   TYPE yde_aai_chat_date,
             chat_time   TYPE yde_aai_chat_time,
             max_seq_no  TYPE i,
             tokens      TYPE i,
             blocked     TYPE abap_bool,
             plan_rag_id TYPE string,
             messages    TYPE ty_msg_t,
             log         TYPE ty_log_t,
             tools       TYPE ty_tools_t,
             approvals   TYPE ty_approvals_t,
             files       TYPE ty_msg_files_t,
             task_flow   TYPE ty_task_flow_t,
           END OF ty_chat_s,

           BEGIN OF ty_response_read_s,
             chat  TYPE ty_chat_s,
             chats TYPE ty_chat_t,
           END OF ty_response_read_s,

           BEGIN OF ty_response_query_s,
             chats TYPE ty_chat_t,
           END OF ty_response_query_s,

           BEGIN OF ty_chat_update_s,
             id      TYPE string,
             updated TYPE abap_bool,
             error   TYPE string,
           END OF ty_chat_update_s,

           BEGIN OF ty_chat_delete_s,
             id      TYPE string,
             deleted TYPE abap_bool,
             error   TYPE string,
           END OF ty_chat_delete_s.

    METHODS yif_aai_rest_resource~read REDEFINITION.

    METHODS yif_aai_rest_resource~update REDEFINITION.

    METHODS yif_aai_rest_resource~delete REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aai_rest_chat IMPLEMENTATION.

  METHOD yif_aai_rest_resource~read.

    DATA: lt_rng_username  TYPE RANGE OF yaai_log-username,
          lt_rng_chat_date TYPE RANGE OF yaai_log-log_date,
          lt_files         TYPE SORTED TABLE OF yaai_msg_file
            WITH UNIQUE KEY id seqno filename,
          lt_files_content TYPE SORTED TABLE OF yaai_msg_file
            WITH NON-UNIQUE KEY id filename seqno.

    DATA: ls_response_query TYPE ty_response_query_s,
          ls_response_read  TYPE ty_response_read_s.

    DATA: l_chat_date_from TYPE yaai_log-log_date,
          l_chat_date_to   TYPE yaai_log-log_date,
          l_json           TYPE string,
          l_chat_id        TYPE yaai_chat-id,
          l_bin_data       TYPE xstring,
          l_object         TYPE ust12-objct,
          l_field1         TYPE ust12-field VALUE 'USER' ##NO_TEXT,
          l_field2         TYPE ust12-field VALUE 'ACTVT' ##NO_TEXT.

    DATA(l_id) = condense( to_upper( i_o_request->get_form_field( name = 'id' ) ) ).

    IF l_id = 'UNDEFINED' ##NO_TEXT.

      "Not Found
      i_o_response->set_status(
        EXPORTING
          code = 404
          reason = 'Not Found'
      ).

      RETURN.

    ENDIF.

    l_chat_id = l_id.

    IF l_chat_id IS NOT INITIAL. " Read

      lt_rng_username = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).

      SELECT SINGLE low
        FROM tvarvc
        WHERE name = @yif_aai_const=>c_chat_auth_obj_param
          AND type = 'P'
          AND numb = '0000'
        INTO @DATA(l_authorization_object_name).

      IF l_authorization_object_name IS NOT INITIAL.

        l_object = l_authorization_object_name.

        AUTHORITY-CHECK OBJECT l_object
          ID l_field1  FIELD sy-uname
          ID l_field2  FIELD '03'.

        IF sy-subrc = 0.
          FREE lt_rng_username.
        ENDIF.

      ENDIF.

      SELECT SINGLE id, api, username, chat_date, chat_time, blocked
        FROM yaai_chat
        WHERE id = @l_chat_id
          AND username IN @lt_rng_username
        INTO @DATA(ls_chat).

      IF sy-subrc <> 0.

        "Not Found
        i_o_response->set_status(
          EXPORTING
            code = 401
            reason = 'Unauthorized'
        ).

      ENDIF.

      ls_response_read-chat = CORRESPONDING #( ls_chat ).

      SELECT id, chat_id, rag_id
        FROM yaai_agent_plan
        WHERE chat_id = @l_chat_id
        INTO @DATA(ls_agent_plan)
        UP TO 1 ROWS.                   "#EC CI_NOORDER "#EC CI_NOFIRST
      ENDSELECT.

      IF sy-subrc = 0.
        ls_response_read-chat-plan_rag_id = ls_agent_plan-rag_id.
      ENDIF.

      SELECT id, seqno, msg, msg_date, msg_time, tokens AS total_tokens, model
        FROM yaai_msg
        WHERE id = @l_chat_id
        ORDER BY id, seqno
        INTO TABLE @DATA(lt_msg).

      IF sy-subrc = 0.

        LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).

          ls_response_read-chat-tokens = ls_response_read-chat-tokens + <ls_msg>-total_tokens.

          <ls_msg>-msg = escape( val    = <ls_msg>-msg
                                 format = cl_abap_format=>e_html_text ).

        ENDLOOP.

        IF sy-subrc = 0.
          ls_response_read-chat-max_seq_no = <ls_msg>-seqno.
        ENDIF.

        ls_response_read-chat-messages = CORRESPONDING #( lt_msg ).

      ENDIF.

      SELECT id, seqno, message, username, log_date, log_time, msgid, msgno, msgty
        FROM yaai_log
       WHERE id = @l_chat_id
        INTO TABLE @DATA(lt_log).

      IF sy-subrc = 0.
        ls_response_read-chat-log = CORRESPONDING #( lt_log ).
      ENDIF.

      SELECT class_name, method_name, proxy_class, description
        FROM yaai_tools
       WHERE id = @l_chat_id
        INTO TABLE @DATA(lt_tools).

      IF sy-subrc = 0.
        ls_response_read-chat-tools = CORRESPONDING #( lt_tools ).
      ENDIF.

      SELECT class_name, method_name, scope, approved, approved_at, used, used_at
        FROM yaai_approval
       WHERE id = @l_chat_id
        INTO TABLE @DATA(lt_approvals).

      IF sy-subrc = 0.
        ls_response_read-chat-approvals = CORRESPONDING #( lt_approvals ).
      ENDIF.

      SELECT DISTINCT id, seqno, filename
        FROM yaai_msg_file
        WHERE id = @l_chat_id
        ORDER BY id, seqno, filename
        INTO CORRESPONDING FIELDS OF TABLE @lt_files.

      SELECT id, filename, seqno, line_no, file_type, file_size, content
        FROM yaai_msg_file
        WHERE id = @l_chat_id
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

        APPEND INITIAL LINE TO ls_response_read-chat-files ASSIGNING FIELD-SYMBOL(<ls_response_read_chat_file>).

        <ls_response_read_chat_file>-seqno = <ls_files_content>-seqno.
        <ls_response_read_chat_file>-filename = <ls_files_content>-filename.
        <ls_response_read_chat_file>-file_type = <ls_files_content>-file_type.
        <ls_response_read_chat_file>-file_size = <ls_files_content>-file_size.

        <ls_response_read_chat_file>-content = cl_abap_codepage=>convert_from( l_content_bin ).

        CLEAR lo_zip.

      ENDLOOP.

      SELECT a~id, a~chat_id, a~task_id, b~name AS task_name, a~previous_task_id, c~name AS previous_task_name, a~status
        FROM yaai_agent_task AS a
        INNER JOIN yaai_task AS b
        ON a~task_id = b~id
        LEFT OUTER JOIN yaai_task AS c
        ON a~previous_task_id = c~id
       WHERE chat_id = @l_chat_id
        INTO TABLE @DATA(lt_agent_tasks_db).

      IF lt_agent_tasks_db IS NOT INITIAL.

        DATA(lt_tasks_sorted) = NEW ycl_aai_agent_task_tools( )->sort( i_t_flow = CORRESPONDING #( lt_agent_tasks_db ) ).

        ls_response_read-chat-task_flow = CORRESPONDING #( lt_tasks_sorted ).

        LOOP AT ls_response_read-chat-task_flow ASSIGNING FIELD-SYMBOL(<ls_task_flow>).

          READ TABLE lt_agent_tasks_db ASSIGNING FIELD-SYMBOL(<ls_agent_tasks_db>)
            WITH KEY task_id = <ls_task_flow>-task_id
                     previous_task_id = <ls_task_flow>-previous_task_id.

          IF sy-subrc = 0.

            <ls_task_flow>-task_name = <ls_agent_tasks_db>-task_name.
            <ls_task_flow>-previous_task_name = <ls_agent_tasks_db>-previous_task_name.
            <ls_task_flow>-task_status = <ls_agent_tasks_db>-status.

          ENDIF.

        ENDLOOP.

      ENDIF.

      ls_response_read-chats = VALUE #( ( id = ls_response_read-chat-id
                                          api = ls_response_read-chat-api
                                          username = ls_response_read-chat-username
                                          chat_date = ls_response_read-chat-chat_date
                                          chat_time = ls_response_read-chat-chat_time
                                          max_seq_no = ls_response_read-chat-max_seq_no
                                          tokens = ls_response_read-chat-tokens
                                          blocked = ls_response_read-chat-blocked ) ).

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_read
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ELSE. " Query

      DATA(l_datefrom) = i_o_request->get_form_field( name = 'datefrom' ).
      DATA(l_dateto) = i_o_request->get_form_field( name = 'dateto' ).
      DATA(l_username) = i_o_request->get_form_field( name = 'username' ).

      IF l_datefrom IS NOT INITIAL AND l_dateto IS NOT INITIAL.
        lt_rng_chat_date = VALUE #( ( sign = 'I' option = 'BT' low = l_datefrom high = l_dateto ) ).
      ELSEIF l_datefrom IS NOT INITIAL AND l_dateto IS INITIAL.
        lt_rng_chat_date = VALUE #( ( sign = 'I' option = 'EQ' low = l_datefrom ) ).
      ENDIF.

      IF l_username IS NOT INITIAL.
        lt_rng_username = VALUE #( ( sign = 'I' option = 'EQ' low = l_username ) ).
      ENDIF.

      SELECT a~id, a~api, a~username, a~chat_date, a~chat_time, a~blocked, MAX( b~seqno ) AS max_seq_no, SUM( b~tokens ) AS tokens
        FROM yaai_chat AS a
        LEFT OUTER JOIN yaai_msg AS b
        ON a~id = b~id
        WHERE chat_date IN @lt_rng_chat_date
        AND username IN @lt_rng_username
        GROUP BY a~id, a~api, a~username, a~chat_date, a~chat_time, a~blocked
        INTO TABLE @DATA(lt_chat)
        UP TO 100 ROWS.

      IF sy-subrc = 0.
        ls_response_query-chats = CORRESPONDING #( lt_chat ).
      ENDIF.

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_query
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ENDIF.

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~update.

    DATA ls_response_update TYPE ty_chat_update_s.

    DATA l_json TYPE string.

    ls_response_update-id = to_upper( i_o_request->get_form_field( name = 'chat_id' ) ).

    DATA(l_action) = to_upper( i_o_request->get_form_field( name = 'action' ) ).

    IF ls_response_update-id IS INITIAL.

      "Not Found
      i_o_response->set_status(
        EXPORTING
          code = 404
          reason = 'Not Found'
      ).

      RETURN.

    ENDIF.

    CASE l_action.

      WHEN 'BLOCK'.

        NEW ycl_aai_db(
          i_api     = space
          i_id      = CONV #( ls_response_update-id )
        )->block_chat(
          IMPORTING
            e_blocked = ls_response_update-updated
        ).

        IF ls_response_update-updated = abap_false.
          ls_response_update-error = 'Error while trying to block the chat.'.
        ENDIF.

      WHEN 'RELEASE'.

        NEW ycl_aai_db(
          i_api     = space
          i_id      = CONV #( ls_response_update-id )
        )->release_chat(
          IMPORTING
            e_released = ls_response_update-updated
        ).

        IF ls_response_update-updated = abap_false.
          ls_response_update-error = 'Error while trying to release the chat.'.
        ENDIF.

    ENDCASE.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response_update
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

  METHOD yif_aai_rest_resource~delete.

    DATA ls_response_delete TYPE ty_chat_delete_s.

    DATA l_json TYPE string.

    ls_response_delete-id = to_upper( i_o_request->get_form_field( name = 'chat_id' ) ).

    IF ls_response_delete-id IS INITIAL.

      "Not Found
      i_o_response->set_status(
        EXPORTING
          code = 404
          reason = 'Not Found'
      ).

      RETURN.

    ENDIF.

    NEW ycl_aai_db(
      i_api     = space
      i_id      = CONV #( ls_response_delete-id )
    )->delete_chat(
      IMPORTING
        e_deleted = ls_response_delete-deleted
    ).

    IF ls_response_delete-deleted = abap_false.
      ls_response_delete-error = 'Error while trying to delete the chat.'.
    ENDIF.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response_delete
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    i_o_response->set_content_type( content_type = 'application/json' ).

    i_o_response->set_cdata(
      EXPORTING
        data = l_json
    ).

  ENDMETHOD.

ENDCLASS.
