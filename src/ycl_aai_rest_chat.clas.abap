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

           ty_msg_t   TYPE STANDARD TABLE OF ty_msg_s WITH EMPTY KEY,

           ty_log_t   TYPE STANDARD TABLE OF ty_log_s WITH EMPTY KEY,

           ty_tools_t TYPE STANDARD TABLE OF ty_tool_s WITH EMPTY KEY,

           BEGIN OF ty_chat_query_s,
             id         TYPE string,
             api        TYPE yde_aai_api,
             username   TYPE usnam,
             chat_date  TYPE yde_aai_chat_date,
             chat_time  TYPE yde_aai_chat_time,
             max_seq_no TYPE i,
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
             blocked     TYPE abap_bool,
             plan_rag_id TYPE string,
             messages    TYPE ty_msg_t,
             log         TYPE ty_log_t,
             tools       TYPE ty_tools_t,
           END OF ty_chat_s,

           BEGIN OF ty_response_read_s,
             chat TYPE ty_chat_s,
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
          lt_rng_chat_date TYPE RANGE OF yaai_log-log_date.

    DATA: ls_response_query TYPE ty_response_query_s,
          ls_response_read  TYPE ty_response_read_s.

    DATA: l_chat_date_from TYPE yaai_log-log_date,
          l_chat_date_to   TYPE yaai_log-log_date,
          l_json           TYPE string.

    DATA(l_id) = to_upper( i_o_request->get_form_field( name = 'id' ) ).

    IF l_id IS NOT INITIAL. " Read

      SELECT SINGLE id ,api ,username ,chat_date ,chat_time, blocked
        FROM yaai_chat
        WHERE id = @l_id
        INTO @DATA(ls_chat).

      IF sy-subrc <> 0.

        "Not Found
        i_o_response->set_status(
          EXPORTING
            code = 404
            reason = 'Not Found'
        ).

      ENDIF.

      ls_response_read-chat = CORRESPONDING #( ls_chat ).

      SELECT id, chat_id, rag_id
        FROM yaai_agent_plan
        WHERE chat_id = @l_id
        INTO @DATA(ls_agent_plan)
        UP TO 1 ROWS.
      ENDSELECT.

      IF sy-subrc = 0.
        ls_response_read-chat-plan_rag_id = ls_agent_plan-rag_id.
      ENDIF.

      SELECT id , seqno, msg, msg_date, msg_time, tokens AS total_tokens, model
        FROM yaai_msg
        WHERE id = @l_id
        ORDER BY id, seqno
        INTO TABLE @DATA(lt_msg).

      IF sy-subrc = 0.

        LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).

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
       WHERE id = @l_id
        INTO TABLE @DATA(lt_log).

      IF sy-subrc = 0.
        ls_response_read-chat-log = CORRESPONDING #( lt_log ).
      ENDIF.

      SELECT class_name, method_name, proxy_class, description
        FROM yaai_tools
          WHERE id = @l_id
            INTO TABLE @DATA(lt_tools).

      IF sy-subrc = 0.
        ls_response_read-chat-tools = CORRESPONDING #( lt_tools ).
      ENDIF.

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

      SELECT a~id, a~api, a~username, a~chat_date, a~chat_time, a~blocked, MAX( b~seqno ) AS max_seq_no
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
