CLASS ycl_aai_anthropic DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_anthropic.
    INTERFACES yif_aai_chat.

    ALIASES on_message_send FOR yif_aai_chat~on_message_send.
    ALIASES on_response_received FOR yif_aai_chat~on_response_received.
    ALIASES on_message_failed FOR yif_aai_chat~on_message_failed.
    ALIASES on_chat_is_blocked FOR yif_aai_chat~on_chat_is_blocked.

    ALIASES set_model FOR yif_aai_anthropic~set_model.
    ALIASES set_temperature FOR yif_aai_anthropic~set_temperature.
    ALIASES set_system_instructions FOR yif_aai_anthropic~set_system_instructions.
    ALIASES set_connection FOR yif_aai_anthropic~set_connection.
    ALIASES set_endpoint FOR yif_aai_anthropic~set_endpoint.
    ALIASES bind_tools FOR yif_aai_anthropic~bind_tools.
    ALIASES chat FOR yif_aai_anthropic~chat.
    ALIASES get_conversation FOR yif_aai_anthropic~get_conversation.

    ALIASES mo_function_calling FOR yif_aai_anthropic~mo_function_calling.
    ALIASES mo_agent FOR yif_aai_anthropic~mo_agent.

    ALIASES m_anthropic_version FOR yif_aai_anthropic~m_anthropic_version.
    ALIASES m_endpoint FOR yif_aai_anthropic~m_endpoint.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_anthropic READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_anthropic.

    METHODS constructor
      IMPORTING
        i_model         TYPE csequence OPTIONAL
        i_max_tokens    TYPE i OPTIONAL
        i_o_connection  TYPE REF TO yif_aai_conn OPTIONAL
        i_o_persistence TYPE REF TO yif_aai_db OPTIONAL
        i_o_agent       TYPE REF TO yif_aai_agent OPTIONAL.


  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection  TYPE REF TO yif_aai_conn,
          _o_persistence TYPE REF TO yif_aai_db,
          _o_log         TYPE REF TO ycl_aai_log.

    DATA: _t_chat_messages    TYPE yif_aai_anthropic~ty_chat_messages_t,
          _t_chat_messages_db TYPE yif_aai_db=>ty_messages_t,
          _t_message_images   TYPE yif_aai_anthropic~ty_message_images_t,
          _t_message_files    TYPE yif_aai_anthropic~ty_message_files_t.

    DATA: _model               TYPE string,
          _temperature         TYPE p LENGTH 2 DECIMALS 1,
          _max_tokens          TYPE i VALUE 2048,
          _system_instructions TYPE string,
          _max_tool_calls      TYPE i.

    METHODS _load_agent_settings.

    METHODS _get_files
      IMPORTING
        i_t_files     TYPE ytt_aai_files
      EXPORTING
        e_t_images    TYPE yif_aai_anthropic~ty_images_t
        e_t_files     TYPE yif_aai_anthropic~ty_files_t
        e_t_images_db TYPE yif_aai_anthropic~ty_message_images_t
        e_t_files_db  TYPE yif_aai_anthropic~ty_message_files_t.

    METHODS _log
      IMPORTING
        i_s_msg TYPE bapiret2.

ENDCLASS.



CLASS ycl_aai_anthropic IMPLEMENTATION.


  METHOD constructor.

    me->m_anthropic_version = '2023-06-01'.

    IF i_model IS NOT INITIAL.
      me->_model = i_model.
    ELSE.

      SELECT model FROM yaai_model
        WHERE id = @yif_aai_const=>c_anthropic
          AND default_model = @abap_true
        INTO @me->_model
        UP TO 1 ROWS.                                   "#EC CI_NOORDER
      ENDSELECT.

      IF sy-subrc <> 0.

        SELECT model FROM yaai_model
          WHERE id = @yif_aai_const=>c_anthropic
          INTO @me->_model
         UP TO 1 ROWS.                                  "#EC CI_NOORDER
        ENDSELECT.

      ENDIF.

    ENDIF.

    IF i_max_tokens IS NOT INITIAL.
      me->_max_tokens = i_max_tokens.
    ENDIF.

    me->_temperature = 1.

    me->_max_tool_calls = 10.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

    IF i_o_persistence IS SUPPLIED.

      me->_o_persistence = i_o_persistence.

      me->_o_persistence->get_chat(
        IMPORTING
          e_t_messages = me->_t_chat_messages_db
          e_t_msg_data = me->_t_chat_messages
      ).

      LOOP AT me->_t_chat_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

        DATA(l_index) = sy-tabix.

        IF <ls_message>-role <> yif_aai_openai=>mc_user.
          CONTINUE.
        ENDIF.

        READ TABLE me->_t_chat_messages_db ASSIGNING FIELD-SYMBOL(<ls_message_db>) INDEX l_index.

        IF sy-subrc = 0.

          me->_o_persistence->get_files(
            EXPORTING
              i_seqno   = <ls_message_db>-seqno
            IMPORTING
              e_t_files = DATA(lt_files_db)
          ).

          " Images and files
          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          me->_get_files(
            EXPORTING
              i_t_files  = lt_files_db
            IMPORTING
              e_t_images = DATA(lt_images)
              e_t_files  = DATA(lt_files)
          ).

          IF lt_images IS NOT INITIAL.

            DATA(ls_message_images) = VALUE yif_aai_anthropic~ty_message_images_s( seqno = <ls_message_db>-seqno
                                                                                   images = CORRESPONDING #( lt_images ) ).

            INSERT ls_message_images INTO TABLE me->_t_message_images.

          ENDIF.

          IF lt_files IS NOT INITIAL.

            DATA(ls_message_files) = VALUE yif_aai_anthropic~ty_message_files_s( seqno = <ls_message_db>-seqno
                                                                                 files = CORRESPONDING #( lt_files ) ).

            INSERT ls_message_files INTO TABLE me->_t_message_files.

          ENDIF.

          FREE lt_files.

        ENDIF.

      ENDLOOP.

    ENDIF.

    "If an Agent is passed then its settings overwrite any other previous setting
    IF i_o_agent IS BOUND.

      me->mo_agent = i_o_agent.

      me->_load_agent_settings( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF m_ref IS NOT BOUND.
      m_ref = NEW #( ).
    ENDIF.

    IF i_model IS SUPPLIED.
      m_ref->set_model( i_model ).
    ENDIF.

    r_ref = m_ref.

  ENDMETHOD.

  METHOD _load_agent_settings.

    DATA(ls_model) = me->mo_agent->get_model(
      EXPORTING
        i_api = CONV #( yif_aai_const=>c_anthropic )
    ).

    IF ls_model-model IS NOT INITIAL.
      me->_model = ls_model-model.
    ENDIF.

    IF ls_model-temperature IS NOT INITIAL.
      me->_temperature = ls_model-temperature.
    ENDIF.

    IF ls_model-max_tool_calls IS NOT INITIAL.

      me->_max_tool_calls = ls_model-max_tool_calls.

      IF me->_max_tool_calls = 0.
        " Like 'unlimited' tool calls but preventing infinite loops
        me->_max_tool_calls = 10000.
      ENDIF.

    ENDIF.

    DATA(l_system_instructions) = me->mo_agent->get_system_instructions( ).

    IF l_system_instructions IS NOT INITIAL.

      me->set_system_instructions(
        i_system_instructions = l_system_instructions
      ).

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_anthropic~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tools_calls IS SUPPLIED.
      me->_max_tool_calls = i_max_tools_calls.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_anthropic~chat.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA: lt_response_content     TYPE yif_aai_anthropic~ty_content_t,
          lt_response_content_des TYPE yif_aai_anthropic~ty_content_t,
          lt_response_content_aux TYPE yif_aai_anthropic~ty_content_t,
          lt_tool_images          TYPE yif_aai_anthropic~ty_images_t,
          lt_tool_files           TYPE yif_aai_anthropic~ty_files_t.

    DATA ls_anthropic_chat_response TYPE yif_aai_anthropic~ty_anthropic_chat_response_s.

    DATA: l_message TYPE string,
          l_prompt  TYPE string,
          l_tools   TYPE string VALUE '[]',
          l_tokens  TYPE i.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.

      me->_log( i_s_msg = VALUE #( number = '018' message_v1 = yif_aai_const=>c_anthropic ) ).

      MESSAGE ID 'YAAI' TYPE 'E' NUMBER '018' WITH yif_aai_const=>c_anthropic INTO DATA(l_error_018).

      RAISE EVENT on_message_failed
        EXPORTING
          error_text = l_error_018.

      RETURN.

    ENDIF.

    IF me->_o_persistence IS BOUND AND
       me->_o_persistence->is_chat_blocked( ).

      RAISE EVENT on_chat_is_blocked.

      RETURN.

    ENDIF.

    IF i_o_agent IS BOUND AND me->mo_agent IS NOT BOUND.

      me->mo_agent = i_o_agent.

      me->_load_agent_settings( ).

    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF i_new = abap_true.

      FREE me->_t_chat_messages.

    ENDIF.

    IF me->_t_chat_messages IS INITIAL.

      IF i_greeting IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

        <ls_msg> = VALUE #( role = 'assistant' content = lo_aai_util->serialize( i_data = i_greeting ) ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

    ENDIF.

    APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

    DATA(l_seqno) = lines( me->_t_chat_messages ).

    IF i_o_prompt IS BOUND.

      l_prompt = i_o_prompt->get_prompt( ).

      l_message = i_o_prompt->get_user_message( ).

    ELSE.

      l_message = i_message.

    ENDIF.

    <ls_msg> = VALUE #( role = 'user' content = lo_aai_util->serialize( i_data = l_message ) ).

    IF l_prompt IS NOT INITIAL.

      DATA(ls_prompt) = <ls_msg>.

      ls_prompt-content = lo_aai_util->serialize( i_data = l_prompt ).

    ENDIF.

    IF me->_o_persistence IS BOUND.

      " persist the user message and the augmented prompt
      me->_o_persistence->persist_message( i_data = <ls_msg>
                                           i_prompt = ls_prompt
                                           i_async_task_id = i_async_task_id
                                           i_model = CONV #( me->_model ) ).

      IF me->_system_instructions IS NOT INITIAL.

        DATA(ls_msg) = VALUE yif_aai_anthropic~ty_chat_message_s( role = 'system' content = lo_aai_util->serialize( i_data = me->_system_instructions ) ).

        me->_o_persistence->persist_system_instructions( i_data = ls_msg ).

      ENDIF.

    ENDIF.

    " In memory we keep the augmented prompt instead of the user message
    IF l_prompt IS NOT INITIAL.
      <ls_msg>-content = lo_aai_util->serialize( i_data = l_prompt ).
    ENDIF.

    " Images and files
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF i_t_files IS NOT INITIAL AND me->_o_persistence IS BOUND.

      me->_o_persistence->persist_files(
        EXPORTING
          i_seqno   = l_seqno
          i_t_files = i_t_files
      ).

    ENDIF.

    me->_get_files(
      EXPORTING
        i_t_files  = i_t_files
      IMPORTING
        e_t_images = DATA(lt_images)
        e_t_files  = DATA(lt_files)
    ).

    IF lt_images IS NOT INITIAL.

      DATA(ls_message_images) = VALUE yif_aai_anthropic~ty_message_images_s( seqno = l_seqno
                                                                             images = CORRESPONDING #( lt_images ) ).

      INSERT ls_message_images INTO TABLE me->_t_message_images.

    ENDIF.

    IF lt_files IS NOT INITIAL.

      DATA(ls_message_files) = VALUE yif_aai_anthropic~ty_message_files_s( seqno = l_seqno
                                                                           files = CORRESPONDING #( lt_files ) ).

      INSERT ls_message_files INTO TABLE me->_t_message_files.

    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF me->_o_connection IS NOT BOUND.

      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_anthropic ).

    ENDIF.

    me->_o_connection->add_http_header_param(
        EXPORTING
          i_name  = 'anthropic-version'
          i_value = me->m_anthropic_version
      ).

    me->_o_connection->add_http_header_param(
      EXPORTING
        i_name  = 'x-api-key'
        i_value = |{ yif_aai_const=>c_placeholder_pattern }APIKEY{ yif_aai_const=>c_placeholder_pattern }|
    ).

    IF me->m_endpoint IS INITIAL.
      me->m_endpoint = yif_aai_const=>c_anthropic_messages_endpoint.
    ENDIF.

    IF i_o_agent IS BOUND AND me->mo_function_calling IS NOT BOUND.

      me->mo_function_calling = NEW ycl_aai_func_call_anthropic( i_o_agent ).

    ENDIF.

    DO ( me->_max_tool_calls + 1 ) TIMES.

      DATA(l_tool_calls) = sy-index.

      IF me->_o_persistence IS BOUND AND
       me->_o_persistence->is_chat_blocked( ).
        RAISE EVENT on_chat_is_blocked.
        EXIT.
      ENDIF.

      IF me->_o_connection->create_connection( i_endpoint = me->m_endpoint ).

        FREE ls_anthropic_chat_response.

        IF me->mo_function_calling IS BOUND.

          me->mo_function_calling->get_tools(
            IMPORTING
              e_tools = l_tools
          ).

        ENDIF.

        "Do not send system messages to the API. They are being persisted just to be make them visible to the developer.
        "The system instructions are passed in the system field (see the serialization below).
        DELETE me->_t_chat_messages WHERE role = 'system'.

        DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_anthropic~ty_anthropic_chat_request_s( model = me->_model
                                                                                                             temperature = me->_temperature
                                                                                                             max_tokens = me->_max_tokens
                                                                                                             stream = abap_false
                                                                                                             system = me->_system_instructions
                                                                                                             messages = me->get_conversation( )
                                                                                                             tools = l_tools ) ).

        me->_o_connection->set_body( l_json ).

        RAISE EVENT on_message_send.

*       Uncomment these lines to write the JSON in a file on the server for analysis of its content and structure.
*        IF me->_o_persistence IS BOUND.
*          NEW ycl_aai_log( i_chat_id = me->_o_persistence->m_id )->write_json_on_log_file( l_json ).
*        ENDIF.

        FREE l_json.

        me->_o_connection->do_receive(
          IMPORTING
            e_response = l_json
            e_failed   = e_failed
        ).

        IF e_failed = abap_true.

          me->_o_connection->get_error_text(
            IMPORTING
              e_error_text = e_response
          ).

          IF e_t_response IS REQUESTED.
            APPEND INITIAL LINE TO e_t_response ASSIGNING FIELD-SYMBOL(<l_response>).
            <l_response> = e_response.
          ENDIF.

          APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = 'assistant' content = lo_aai_util->serialize( i_data = e_response ) ).

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_async_task_id = i_async_task_id
                                                 i_model = CONV #( me->_model ) ).
          ENDIF.

          RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

          EXIT.

        ENDIF.

        RAISE EVENT on_response_received.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = ls_anthropic_chat_response
        ).

        IF ls_anthropic_chat_response-type = 'error'.

          e_response = |{ ls_anthropic_chat_response-error-type } { ls_anthropic_chat_response-error-message }|.

          IF e_t_response IS REQUESTED.
            APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
            <l_response> = e_response.
          ENDIF.

          APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = 'assistant' content = lo_aai_util->serialize( i_data = e_response ) ).

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_async_task_id = i_async_task_id
                                                 i_model = CONV #( me->_model ) ).
          ENDIF.

          RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

          EXIT.

        ENDIF.

        l_tokens = ls_anthropic_chat_response-usage-input_tokens + ls_anthropic_chat_response-usage-output_tokens.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = ls_anthropic_chat_response-content
          IMPORTING
            e_data = lt_response_content
        ).

        FREE: lt_response_content_aux, lt_response_content_des.

        LOOP AT lt_response_content ASSIGNING FIELD-SYMBOL(<ls_content>).

          IF <ls_content>-type = 'text'.

            lo_aai_util->deserialize(
              EXPORTING
                i_json = ls_anthropic_chat_response-content
              IMPORTING
                e_data = lt_response_content_aux
            ).

            IF lt_response_content_aux IS INITIAL.

              APPEND <ls_content> TO lt_response_content_des.

              IF <ls_content>-type = 'text'.

                APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

                <ls_msg> = VALUE #( role = ls_anthropic_chat_response-role
                                    content = lo_aai_util->serialize( i_data = <ls_content>-text ) ).

                IF e_response IS INITIAL.
                  e_response = <ls_content>-text.
                ELSE.
                  e_response = |{ e_response }{ cl_abap_char_utilities=>newline }{ <ls_content>-text }|.
                ENDIF.

                IF me->_o_persistence IS BOUND.
                  me->_o_persistence->persist_message( i_data = <ls_msg>
                                                       i_tokens = l_tokens
                                                       i_async_task_id = i_async_task_id
                                                       i_model = CONV #( me->_model ) ).
                  CLEAR l_tokens.
                ENDIF.

              ENDIF.

            ELSE.

              LOOP AT lt_response_content_aux ASSIGNING FIELD-SYMBOL(<ls_content_aux>).

                APPEND <ls_content_aux> TO lt_response_content_des.

                IF <ls_content_aux>-type = 'text'.

                  APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

                  <ls_content_aux>-text = lo_aai_util->replace_unicode_escape_seq( <ls_content_aux>-text ).

                  <ls_msg> = VALUE #( role = ls_anthropic_chat_response-role
                                      content = lo_aai_util->serialize( i_data = <ls_content_aux>-text ) ).

                  IF e_response IS INITIAL.
                    e_response = <ls_content_aux>-text.
                  ELSE.
                    e_response = |{ e_response }{ cl_abap_char_utilities=>newline }{ <ls_content_aux>-text }|.
                  ENDIF.

                  IF me->_o_persistence IS BOUND.
                    me->_o_persistence->persist_message( i_data = <ls_msg>
                                                         i_tokens = l_tokens
                                                         i_async_task_id = i_async_task_id
                                                         i_model = CONV #( me->_model ) ).
                    CLEAR l_tokens.
                  ENDIF.

                ENDIF.

              ENDLOOP.

            ENDIF.

          ENDIF.

        ENDLOOP.

        IF lt_response_content_des IS NOT INITIAL.

          FREE lt_response_content.

          lt_response_content[] = lt_response_content_des[].

        ENDIF.

        DELETE lt_response_content WHERE type <> 'tool_use'.

        IF lt_response_content IS INITIAL.
          EXIT.
        ENDIF.

        IF ( to_lower( ls_anthropic_chat_response-stop_reason ) = 'tool_use' OR
             to_lower( ls_anthropic_chat_response-stop_reason ) = 'end_turn' ).

          LOOP AT lt_response_content ASSIGNING <ls_content>.

            IF <ls_content>-type <> 'tool_use'.
              CONTINUE.
            ENDIF.

            IF NOT me->mo_function_calling IS BOUND.
              CONTINUE.
            ENDIF.

            APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = ls_anthropic_chat_response-role
                                content = '[' && lo_aai_util->serialize( i_data = CORRESPONDING yif_aai_anthropic~ty_chat_message_tool_use_s( <ls_content> ) ) && ']' ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

            " Tool call approval
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            DATA(l_tool_call_approved) = abap_true.

            IF me->_o_persistence IS BOUND AND
               me->mo_function_calling IS BOUND.

              DATA(lo_fc_approvals) = NEW ycl_aai_func_call_approvals( ).

              lo_fc_approvals->check_tool_call_approval(
                EXPORTING
                  i_tool_name     = to_upper( condense( <ls_content>-name ) )
                  i_o_persistence = me->_o_persistence
                  i_t_tools       = CORRESPONDING #( me->mo_function_calling->mt_methods )
                IMPORTING
                  e_approved      = l_tool_call_approved
                  e_tool_response = DATA(l_tool_call_approval_response)
              ).

            ENDIF.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            IF l_tool_call_approved = abap_true.

              me->mo_function_calling->call_tool(
                EXPORTING
                  i_tool_name   = to_upper( <ls_content>-name )
                  i_json        = <ls_content>-input
                IMPORTING
                  e_t_files     = DATA(lt_tool_response_files)
                RECEIVING
                  r_response    = DATA(l_tool_response)
              ).

              IF lo_fc_approvals IS BOUND.

                lo_fc_approvals->set_approval_as_used(
                  EXPORTING
                    i_tool_name     = to_upper( condense( <ls_content>-name ) )
                    i_o_persistence = me->_o_persistence
                ).

              ENDIF.

            ELSE.
              l_tool_response = l_tool_call_approval_response.
            ENDIF.

            l_tool_response = lo_aai_util->serialize(
              EXPORTING
                i_data = l_tool_response
            ).

            l_tool_response = lo_aai_util->serialize(
              EXPORTING
                i_data = VALUE yif_aai_anthropic~ty_chat_message_tool_result_s( type = 'tool_result'
                                                                                tool_use_id = <ls_content>-id
                                                                                content = l_tool_response ) ).

            l_tool_response = '[' && l_tool_response && ']'.

            APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'user' content = l_tool_response ).

            IF me->_o_persistence IS BOUND.

              me->_o_persistence->persist_message(
                EXPORTING
                  i_data = <ls_msg>
                  i_async_task_id = i_async_task_id
                  i_model = CONV #( me->_model )
                IMPORTING
                  e_seqno = l_seqno ).

              " Images and files returned from tool call
              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
              IF lt_tool_response_files IS NOT INITIAL.

                me->_o_persistence->persist_files(
                  EXPORTING
                    i_seqno   = l_seqno
                    i_t_files = lt_tool_response_files
                ).

                me->_get_files(
                  EXPORTING
                    i_t_files     = lt_tool_response_files
                  IMPORTING
                    e_t_images    = lt_tool_images
                    e_t_files     = lt_tool_files
                ).

                IF lt_tool_images IS NOT INITIAL.

                  ls_message_images = VALUE yif_aai_anthropic~ty_message_images_s( seqno = l_seqno
                                                                                   images = CORRESPONDING #( lt_tool_images ) ).

                  INSERT ls_message_images INTO TABLE me->_t_message_images.

                ENDIF.

                IF lt_tool_files IS NOT INITIAL.

                  ls_message_files = VALUE yif_aai_anthropic~ty_message_files_s( seqno = l_seqno
                                                                                 files = CORRESPONDING #( lt_tool_files ) ).

                  INSERT ls_message_files INTO TABLE me->_t_message_files.

                ENDIF.

              ENDIF.
              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

              FREE: lt_tool_response_files,
                    lt_tool_images,
                    lt_tool_files.

            ENDIF.

          ENDLOOP.

          IF l_tool_calls >= me->_max_tool_calls.

            APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'user' ).

            "The maximum number of tool calls allowed has been reached.
            MESSAGE ID 'YAAI' TYPE 'S' NUMBER '017' INTO <ls_msg>-content.

            <ls_msg>-content = lo_aai_util->serialize( i_data = <ls_msg>-content ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

          ENDIF.

          CONTINUE.

        ENDIF.

        EXIT.

      ELSE.

        me->_o_connection->get_error_text(
          IMPORTING
            e_error_text = e_response
        ).

        IF e_t_response IS REQUESTED.
          APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
          <l_response> = e_response.
        ENDIF.

        EXIT.

      ENDIF.

    ENDDO.

    IF e_t_response IS REQUESTED AND ls_anthropic_chat_response-type <> 'error'.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_anthropic~get_conversation.

    TYPES: BEGIN OF ty_message_content_type_s,
             type TYPE string,
           END OF ty_message_content_type_s.

    DATA: lt_message_content_type TYPE STANDARD TABLE OF ty_message_content_type_s WITH EMPTY KEY,
          lt_tool_result          TYPE STANDARD TABLE OF yif_aai_anthropic~ty_chat_message_tool_result_s WITH EMPTY KEY.

    DATA: ls_message_content_type TYPE ty_message_content_type_s.

    DATA: l_json       TYPE string,
          l_json_image TYPE string,
          l_output     TYPE xstring.

    IF me->_t_message_images IS INITIAL AND
       me->_t_message_files IS INITIAL.

      rt_messages = me->_t_chat_messages.

      RETURN.

    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->_t_chat_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      DATA(l_index) = sy-tabix.

      IF <ls_message>-role = 'user'.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = <ls_message>-content
          IMPORTING
            e_data = lt_message_content_type
        ).

        CLEAR ls_message_content_type.

        IF lt_message_content_type IS NOT INITIAL.
          ls_message_content_type = lt_message_content_type[ 1 ].
        ENDIF.

        IF ls_message_content_type IS INITIAL.

          DATA(ls_text_block) = VALUE yif_aai_anthropic~ty_text_block_param_s( type = 'text' text = <ls_message>-content ).

          l_json = lo_aai_util->serialize( ls_text_block ).

          READ TABLE me->_t_chat_messages_db ASSIGNING FIELD-SYMBOL(<ls_message_db>) INDEX l_index.

          " If the message is already persisted use SEQNO
          IF sy-subrc = 0.
            l_index = <ls_message_db>-seqno.
          ENDIF.

          READ TABLE me->_t_message_images ASSIGNING FIELD-SYMBOL(<ls_message_images>)
            WITH KEY seqno = l_index.

          IF sy-subrc = 0.

            LOOP AT <ls_message_images>-images ASSIGNING FIELD-SYMBOL(<ls_image>).

              DATA(ls_image_block) = VALUE yif_aai_anthropic~ty_image_block_param_s(
                type = 'image'
                source = VALUE #( data = <ls_image>-image_url
                                  media_type = <ls_image>-media_type
                                  type = 'base64' )
              ).

              l_json_image = lo_aai_util->serialize( ls_image_block ).

              l_json = |{ l_json },{ l_json_image }|.

            ENDLOOP.

          ENDIF.

          READ TABLE me->_t_message_files ASSIGNING FIELD-SYMBOL(<ls_message_files>)
            WITH KEY seqno = l_index.

          IF sy-subrc = 0.

            LOOP AT <ls_message_files>-files ASSIGNING FIELD-SYMBOL(<ls_file>).

              IF <ls_file>-file_type = 'application/pdf'.

                DATA(ls_document_block) = VALUE yif_aai_anthropic~ty_document_block_s( source-data = <ls_file>-file_data
                                                                                       source-media_type = <ls_file>-file_type
                                                                                       source-type = 'base64'
                                                                                       type = 'document' ).

                DATA(l_json_document) = lo_aai_util->serialize( ls_document_block ).

                l_json = |{ l_json },{ l_json_document }|.

              ENDIF.

              IF <ls_file>-file_type CP 'text/*'.

                CLEAR ls_document_block.

                ls_document_block = VALUE yif_aai_anthropic~ty_document_block_s( source-data = <ls_file>-file_data
                                                                                 source-media_type = 'text/plain'
                                                                                 source-type = 'text'
                                                                                 type = 'document' ).

                l_json_document = lo_aai_util->serialize( ls_document_block ).

                l_json = |{ l_json },{ l_json_document }|.

              ENDIF.

            ENDLOOP.

          ENDIF.

          l_json = '[' && l_json && ']'.

          <ls_message>-content = l_json.

          CONTINUE.

        ENDIF.

        IF ls_message_content_type-type = 'tool_result'.

          READ TABLE me->_t_chat_messages_db ASSIGNING <ls_message_db> INDEX l_index.

          " If the message is already persisted use SEQNO
          IF sy-subrc = 0.
            l_index = <ls_message_db>-seqno.
          ENDIF.

          DATA(l_tool_response_has_images) = abap_false.
          DATA(l_tool_response_has_files) = abap_false.

          READ TABLE me->_t_message_images ASSIGNING <ls_message_images>
            WITH KEY seqno = l_index.

          IF sy-subrc = 0.
            l_tool_response_has_images = abap_true.
          ENDIF.

          READ TABLE me->_t_message_files ASSIGNING <ls_message_files>
            WITH KEY seqno = l_index.

          IF sy-subrc = 0.
            l_tool_response_has_files = abap_true.
          ENDIF.

          IF l_tool_response_has_images = abap_false AND l_tool_response_has_files = abap_false.
            CONTINUE.
          ENDIF.

          DATA(ls_tool_result) = VALUE yif_aai_anthropic~ty_chat_message_tool_result_s( ).

          lo_aai_util->deserialize(
            EXPORTING
              i_json = <ls_message>-content
            IMPORTING
              e_data = lt_tool_result
          ).

          IF lt_tool_result IS INITIAL.
            CONTINUE.
          ENDIF.

          ls_tool_result = lt_tool_result[ 1 ].

          IF ls_tool_result IS INITIAL.
            CONTINUE.
          ENDIF.

          CLEAR ls_text_block.

          ls_text_block = VALUE yif_aai_anthropic~ty_text_block_param_s( type = 'text' text = ls_tool_result-content ).

          l_json = lo_aai_util->serialize( ls_text_block ).

          IF l_tool_response_has_images = abap_true.

            LOOP AT <ls_message_images>-images ASSIGNING <ls_image>.

              ls_image_block = VALUE yif_aai_anthropic~ty_image_block_param_s(
                type = 'image'
                source = VALUE #( data = <ls_image>-image_url
                                  media_type = <ls_image>-media_type
                                  type = 'base64' )
              ).

              l_json_image = lo_aai_util->serialize( ls_image_block ).

              l_json = |{ l_json },{ l_json_image }|.

            ENDLOOP.

          ENDIF.

          IF l_tool_response_has_files = abap_true.

            LOOP AT <ls_message_files>-files ASSIGNING <ls_file>.

              IF <ls_file>-file_type = 'application/pdf'.

                CLEAR ls_document_block.

                ls_document_block = VALUE yif_aai_anthropic~ty_document_block_s( source-data = <ls_file>-file_data
                                                                                 source-media_type = <ls_file>-file_type
                                                                                 source-type = 'base64'
                                                                                 type = 'document' ).

                l_json_document = lo_aai_util->serialize( ls_document_block ).

                l_json = |{ l_json },{ l_json_document }|.

              ENDIF.

              IF <ls_file>-file_type CP 'text/*'.

                CLEAR ls_document_block.

                ls_document_block = VALUE yif_aai_anthropic~ty_document_block_s( source-data = <ls_file>-file_data "cl_abap_codepage=>convert_from( l_output )
                                                                                 source-media_type = 'text/plain'
                                                                                 source-type = 'text'
                                                                                 type = 'document' ).

                l_json_document = lo_aai_util->serialize( ls_document_block ).

                l_json = |{ l_json },{ l_json_document }|.

              ENDIF.

            ENDLOOP.

          ENDIF.

          l_json = '[' && l_json && ']'.

          ls_tool_result-content = l_json.

          l_json = lo_aai_util->serialize(
            EXPORTING
              i_data = ls_tool_result ).

          <ls_message>-content = '[' && l_json && ']'.

        ENDIF.

      ENDIF.

    ENDLOOP.

    rt_messages = me->_t_chat_messages.

  ENDMETHOD.


  METHOD yif_aai_anthropic~get_history.

    e_t_history = me->_t_chat_messages.

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_history.

    me->_t_chat_messages = i_t_history.

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_max_tokens.

    me->_max_tokens = i_max_tokens.

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_model.

    me->_model = i_model.

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_persistence.

    me->_o_persistence = i_o_persistence.

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_system_instructions.

    me->_system_instructions = i_system_instructions.

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_version.

    me->m_anthropic_version = i_version.

  ENDMETHOD.


  METHOD yif_aai_chat~chat.

    me->chat(
      EXPORTING
        i_message       = i_message
        i_new           = i_new
        i_greeting      = i_greeting
        i_async_task_id = i_async_task_id
        i_t_files       = i_t_files
        i_o_prompt      = i_o_prompt
        i_o_agent       = i_o_agent
      IMPORTING
        e_response      = e_response
        e_failed        = e_failed
        e_t_response    = e_t_response
    ).

  ENDMETHOD.


  METHOD yif_aai_anthropic~set_endpoint.

    m_endpoint = i_endpoint.

  ENDMETHOD.

  METHOD _get_files.

    " Supported image file types
    " PNG (.png) → image/png
    " JPEG (.jpeg and .jpg) → image/jpeg
    " WEBP (.webp) → image/webp
    " Non-animated GIF (.gif) → image/gif

    CONSTANTS: lc_png  TYPE string VALUE 'image/png'  ##NO_TEXT,
               lc_jpeg TYPE string VALUE 'image/jpeg' ##NO_TEXT,
               lc_webp TYPE string VALUE 'image/webp' ##NO_TEXT,
               lc_gif  TYPE string VALUE 'image/gif'  ##NO_TEXT.

    FREE: e_t_images, e_t_files.

    LOOP AT i_t_files ASSIGNING FIELD-SYMBOL(<ls_file>).

      CASE condense( to_lower( <ls_file>-file_type ) ).

        WHEN lc_png OR lc_jpeg OR lc_webp OR lc_gif.

          "APPEND VALUE #( image_url = |data:{ condense( to_lower( <ls_file>-file_type ) ) };base64,{ <ls_file>-content }| ) TO e_t_images.
          APPEND VALUE #( media_type = <ls_file>-file_type
                          image_url = <ls_file>-content ) TO e_t_images.

        WHEN OTHERS.

          APPEND VALUE #( filename = <ls_file>-filename
                          file_data = <ls_file>-content
                          file_type = <ls_file>-file_type ) TO e_t_files.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD _log.

    IF me->_o_log IS NOT BOUND.
      me->_o_log = NEW #( ).
    ENDIF.

    me->_o_log->add( i_s_msg = i_s_msg ).

    IF sy-msgid IS NOT INITIAL AND
       sy-msgty IS NOT INITIAL AND
       sy-msgno IS NOT INITIAL.

      me->_o_log->add( VALUE #( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
