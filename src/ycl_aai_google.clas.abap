CLASS ycl_aai_google DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_google.
    INTERFACES yif_aai_chat.

    ALIASES on_message_send FOR yif_aai_chat~on_message_send.
    ALIASES on_response_received FOR yif_aai_chat~on_response_received.
    ALIASES on_message_failed FOR yif_aai_chat~on_message_failed.
    ALIASES on_chat_is_blocked FOR yif_aai_chat~on_chat_is_blocked.

    ALIASES set_model FOR yif_aai_google~set_model.
    ALIASES set_temperature FOR yif_aai_google~set_temperature.
    ALIASES set_system_instructions FOR yif_aai_google~set_system_instructions.
    ALIASES set_connection FOR yif_aai_google~set_connection.
    ALIASES set_persistence FOR yif_aai_google~set_persistence.
    ALIASES bind_tools FOR yif_aai_google~bind_tools.
    ALIASES chat FOR yif_aai_chat~chat.
    ALIASES generate FOR yif_aai_google~generate.
    ALIASES get_conversation FOR yif_aai_google~get_conversation.
    ALIASES set_endpoint FOR yif_aai_google~set_endpoint.

    ALIASES mo_function_calling FOR yif_aai_google~mo_function_calling.
    ALIASES mo_agent FOR yif_aai_google~mo_agent.

    ALIASES m_endpoint FOR yif_aai_google~m_endpoint.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_google.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_google READ-ONLY.

    METHODS constructor
      IMPORTING
        i_model         TYPE csequence OPTIONAL
        i_t_history     TYPE yif_aai_google~ty_contents_t OPTIONAL
        i_o_connection  TYPE REF TO yif_aai_conn OPTIONAL
        i_o_persistence TYPE REF TO yif_aai_db OPTIONAL
        i_o_agent       TYPE REF TO yif_aai_agent OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection  TYPE REF TO yif_aai_conn,
          _o_persistence TYPE REF TO yif_aai_db,
          _o_log         TYPE REF TO ycl_aai_log.

    DATA: _t_chat_messages  TYPE yif_aai_google~ty_contents_t,
          _t_messages_db    TYPE yif_aai_db=>ty_messages_t,
          _t_message_images TYPE yif_aai_google~ty_message_images_t,
          _t_message_files  TYPE yif_aai_google~ty_message_files_t.

    DATA: _model               TYPE string,
          _temperature         TYPE p LENGTH 2 DECIMALS 1,
          _system_instructions TYPE string,
          _max_tool_calls      TYPE i.

    METHODS _load_agent_settings.

    METHODS _get_files
      IMPORTING
        i_t_files     TYPE ytt_aai_files
      EXPORTING
        e_t_images    TYPE yif_aai_google~ty_images_t
        e_t_files     TYPE yif_aai_google~ty_files_t
        e_t_images_db TYPE yif_aai_google~ty_message_images_t
        e_t_files_db  TYPE yif_aai_google~ty_message_files_t.

    METHODS _log
      IMPORTING
        i_s_msg TYPE bapiret2.

    METHODS _append_to_history
      IMPORTING
        i_s_response TYPE yif_aai_google~ty_contents_response_s
        i_tokens     TYPE i OPTIONAL
      EXPORTING
        e_seqno      TYPE i.

ENDCLASS.



CLASS ycl_aai_google IMPLEMENTATION.


  METHOD constructor.

    IF i_model IS NOT INITIAL.
      me->_model = i_model.
    ELSE.

      SELECT model
        FROM yaai_model
        WHERE id = @yif_aai_const=>c_google
          AND default_model = @abap_true
         INTO @me->_model
        UP TO 1 ROWS.                                   "#EC CI_NOORDER
      ENDSELECT.

      IF sy-subrc <> 0.

        SELECT model
          FROM yaai_model
          WHERE id = @yif_aai_const=>c_google
           INTO @me->_model
          UP TO 1 ROWS.                                 "#EC CI_NOORDER
        ENDSELECT.

      ENDIF.

    ENDIF.

    me->_temperature = 1.

    me->_max_tool_calls = 10.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

    IF i_t_history IS SUPPLIED.
      me->_t_chat_messages = i_t_history.
    ENDIF.

    IF i_o_persistence IS SUPPLIED.

      me->_o_persistence = i_o_persistence.

      me->_o_persistence->get_chat(
        IMPORTING
          e_t_msg_data = me->_t_chat_messages
      ).

      " Images and files
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      LOOP AT me->_t_chat_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

        DATA(l_index) = sy-tabix.

        IF <ls_message>-role <> yif_aai_openai=>mc_user.
          CONTINUE.
        ENDIF.

        READ TABLE me->_t_messages_db ASSIGNING FIELD-SYMBOL(<ls_message_db>) INDEX l_index.

        IF sy-subrc = 0.

          me->_o_persistence->get_files(
            EXPORTING
              i_seqno   = <ls_message_db>-seqno
            IMPORTING
              e_t_files = DATA(lt_files_db)
          ).

          me->_get_files(
            EXPORTING
              i_t_files  = lt_files_db
            IMPORTING
              e_t_images = DATA(lt_images)
              e_t_files  = DATA(lt_files)
          ).

          IF lt_images IS NOT INITIAL.

            DATA(ls_message_images) = VALUE yif_aai_google~ty_message_images_s( seqno = <ls_message_db>-seqno
                                                                                images = CORRESPONDING #( lt_images ) ).

            INSERT ls_message_images INTO TABLE me->_t_message_images.

          ENDIF.

          FREE lt_images.

          IF lt_files IS NOT INITIAL.

            DATA(ls_message_files) = VALUE yif_aai_google~ty_message_files_s( seqno = <ls_message_db>-seqno
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
        i_api = CONV #( yif_aai_const=>c_google )
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

  METHOD yif_aai_chat~chat.

    me->generate(
      EXPORTING
        i_message       = i_message
        i_new           = i_new
        i_greeting      = i_greeting
        i_t_files       = i_t_files
        i_async_task_id = i_async_task_id
        i_o_prompt      = i_o_prompt
        i_o_agent       = i_o_agent
      IMPORTING
        e_response   = e_response
        e_t_response = e_t_response
    ).

  ENDMETHOD.


  METHOD yif_aai_google~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tools_calls IS SUPPLIED.
      me->_max_tool_calls = i_max_tools_calls.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_google~generate.

    FIELD-SYMBOLS: <ls_generate_request> TYPE any,
                   <l_data>              TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA: lt_tool_images TYPE yif_aai_google~ty_images_t,
          lt_tool_files  TYPE yif_aai_google~ty_files_t.

    DATA: ls_response TYPE yif_aai_google~ty_google_generate_response_s.

    DATA: l_endpoint TYPE string,
          l_greeting TYPE string,
          l_message  TYPE string,
          l_prompt   TYPE string.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.

      me->_log( i_s_msg = VALUE #( number = '018' message_v1 = yif_aai_const=>c_google ) ).

      MESSAGE ID 'YAAI' TYPE 'E' NUMBER '018' WITH yif_aai_const=>c_google INTO DATA(l_error_018).

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

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_google ).
    ENDIF.

    DATA(l_apikey_url_placeholder) = |{ yif_aai_const=>c_placeholder_pattern }APIKEY{ yif_aai_const=>c_placeholder_pattern }|.

    IF me->m_endpoint IS NOT INITIAL.
      l_endpoint = me->m_endpoint.
    ELSE.
      l_endpoint = |/v1beta/models/{ me->_model }:generateContent|.
    ENDIF.

    me->_o_connection->add_http_header_param(
      EXPORTING
        i_name  = 'X-goog-api-key'
        i_value = l_apikey_url_placeholder
    ).

    IF i_new = abap_true.

      FREE me->_t_chat_messages.

    ENDIF.

    IF i_o_agent IS BOUND AND me->mo_agent IS NOT BOUND.

      me->mo_agent = i_o_agent.

      me->_load_agent_settings( ).

    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF me->_t_chat_messages IS INITIAL.

      IF i_greeting IS NOT INITIAL.

        l_greeting = '{"text": ' && lo_aai_util->serialize( i_greeting ) && '}'.

        APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

        <ls_msg> = VALUE #( role = yif_aai_google=>mc_role_model
                            parts = VALUE #( ( l_greeting ) ) ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

    ENDIF.

    IF i_o_prompt IS BOUND.

      l_prompt = i_o_prompt->get_prompt( ).

      l_prompt = '{"text": ' && lo_aai_util->serialize( l_prompt ) && '}'.

      l_message = i_o_prompt->get_user_message( ).

      l_message = '{"text": ' && lo_aai_util->serialize( l_message ) && '}'.

    ELSE.

      l_message = i_message.

      l_message = '{"text": ' && lo_aai_util->serialize( l_message ) && '}'.

    ENDIF.

    APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

    <ls_msg> = VALUE #( role = yif_aai_google=>mc_role_user
                        parts = VALUE #( ( l_message ) ) ).

    DATA(l_seqno) = lines( me->_t_chat_messages ).

    IF l_prompt IS NOT INITIAL.
      DATA(ls_prompt) = <ls_msg>.
      ls_prompt = VALUE #( role = yif_aai_google=>mc_role_user
                           parts = VALUE #( ( l_prompt ) ) ).
    ENDIF.

    IF me->_o_persistence IS BOUND.
      me->_o_persistence->persist_message( i_data = <ls_msg>
                                           i_prompt = ls_prompt
                                           i_async_task_id = i_async_task_id
                                           i_model = CONV #( me->_model ) ).
    ENDIF.

    " In memory we keep the augmented prompt instead of the user message
    IF l_prompt IS NOT INITIAL.
      <ls_msg> = VALUE #( role = yif_aai_google=>mc_role_user
                          parts = VALUE #( ( l_prompt ) ) ).
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

      DATA(ls_message_images) = VALUE yif_aai_google~ty_message_images_s( seqno = l_seqno
                                                                          images = CORRESPONDING #( lt_images ) ).

      INSERT ls_message_images INTO TABLE me->_t_message_images.

    ENDIF.

    IF lt_files IS NOT INITIAL.

      DATA(ls_message_files) = VALUE yif_aai_google~ty_message_files_s( seqno = l_seqno
                                                                        files = CORRESPONDING #( lt_files ) ).

      INSERT ls_message_files INTO TABLE me->_t_message_files.

    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF i_o_agent IS BOUND AND me->mo_function_calling IS NOT BOUND.

      me->mo_function_calling = NEW ycl_aai_func_call_google( i_o_agent ).

    ENDIF.

    DO ( me->_max_tool_calls + 1 ) TIMES.

      DATA(l_tool_calls) = sy-index.

      IF me->_o_persistence IS BOUND AND
       me->_o_persistence->is_chat_blocked( ).
        RAISE EVENT on_chat_is_blocked.
        EXIT.
      ENDIF.

      IF me->_o_connection->create_connection( i_endpoint = l_endpoint ).

        DATA(ls_generate_request) = VALUE yif_aai_google~ty_google_generate_request_s( contents = me->get_conversation( ) ).

        "Do not send system messages to the API. They are being persisted just to be make them visible to the developer.
        DELETE ls_generate_request-contents WHERE role = yif_aai_google=>mc_role_system.

        ls_generate_request-tools = '[]'.

        IF me->mo_function_calling IS BOUND.

          me->mo_function_calling->get_tools(
            IMPORTING
              e_tools = ls_generate_request-tools
          ).

        ENDIF.

        ls_generate_request-generation_config-temperature = me->_temperature.

        ASSIGN ls_generate_request TO <ls_generate_request>.

        IF me->_system_instructions IS NOT INITIAL.

          "If the System Instructions is set then we need to pass a different request
          DATA(ls_generate_request_sys) = VALUE yif_aai_google~ty_google_generate_req_sys_s(  ).

          ls_generate_request_sys-system_instruction = VALUE #( parts = VALUE #( ( text = me->_system_instructions ) ) ).

          ls_generate_request_sys-contents = ls_generate_request-contents.
          ls_generate_request_sys-generation_config = CORRESPONDING #( ls_generate_request-generation_config ).
          ls_generate_request_sys-tools = ls_generate_request-tools.

          ASSIGN ls_generate_request_sys TO <ls_generate_request>.

          DATA(l_system_instructions) = '{"text": ' && lo_aai_util->serialize( me->_system_instructions ) && '}'.

          DATA(ls_msg) = VALUE yif_aai_google~ty_contents_s( role = yif_aai_google=>mc_role_system
                                                             parts = VALUE #( ( l_system_instructions ) ) ).

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_system_instructions( i_data = ls_msg ).
          ENDIF.

        ENDIF.

        DATA(l_json) = lo_aai_util->serialize( i_data = <ls_generate_request> ).

        me->_o_connection->set_body( l_json ).

        FREE l_json.

        RAISE EVENT on_message_send.

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

          <ls_msg> = VALUE #( role = yif_aai_google=>mc_role_model
                              parts = VALUE #( ( '{"text": ' && lo_aai_util->serialize( e_response ) && '}' ) ) ).

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

        lo_aai_util->deserialize(
          EXPORTING
            i_json       = l_json
            i_camel_case = abap_true
          IMPORTING
            e_data       = ls_response
        ).

        IF ls_response IS INITIAL.

          MESSAGE e020(yaai) INTO e_response.

          APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = yif_aai_google=>mc_role_model
                              parts = VALUE #( ( '{"text": ' && lo_aai_util->serialize( e_response ) && '}' ) ) ).

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_async_task_id = i_async_task_id
                                                 i_model = CONV #( me->_model ) ).
          ENDIF.

          RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

          RETURN.

        ENDIF.

        RAISE EVENT on_response_received.

        IF ls_response-error IS NOT INITIAL.

          e_response = |Error! code: { ls_response-error-code }, message: { ls_response-error-message }, status: { ls_response-error-status }|.

          APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = yif_aai_google=>mc_role_model
                              parts = VALUE #( ( '{"text": ' && lo_aai_util->serialize( e_response ) && '}' ) ) ).

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

        LOOP AT ls_response-candidates ASSIGNING FIELD-SYMBOL(<ls_candidates>).

          "Add LLM response to the chat history
          me->_append_to_history( i_s_response = <ls_candidates>-content
                                  i_tokens = ls_response-usage_metadata-total_token_count ).

          DATA(l_function_call) = abap_false.

          LOOP AT <ls_candidates>-content-parts ASSIGNING FIELD-SYMBOL(<ls_parts>).

            IF <ls_parts>-functioncall IS INITIAL.

              <ls_parts>-text = lo_aai_util->replace_unicode_escape_seq( <ls_parts>-text ).

              e_response = <ls_parts>-text.

              CONTINUE.

            ENDIF.

            l_function_call = abap_true.

            ASSIGN <ls_parts>-functioncall-args TO <l_data>.

            " This deserialization may be necessary depending on how the arguments are received. We may need to parse an escaped string to a JSON string.
            " Example: parse this "{\"latitude\":48.8566,\"longitude\":2.3522}" to a JSON like {"latitude": 48.8566, "longitude": 2.3522}
            lo_aai_util->deserialize(
              EXPORTING
                i_json = <ls_parts>-functioncall-args
              IMPORTING
                e_data = lr_data
            ).

            IF lr_data IS NOT INITIAL.

              DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_data_ref( lr_data ).

              " Make sure the deserialized object is a JSON string before assigning it
              IF lo_typedescr->type_kind = cl_abap_typedescr=>typekind_string.

                ASSIGN lr_data->* TO <l_data>.

              ENDIF.

            ENDIF.

            " Tool call approval
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            DATA(l_tool_call_approved) = abap_true.

            IF me->_o_persistence IS BOUND AND
               me->mo_function_calling IS BOUND.

              DATA(lo_fc_approvals) = NEW ycl_aai_func_call_approvals( ).

              lo_fc_approvals->check_tool_call_approval(
                EXPORTING
                  i_tool_name     = to_upper( condense( <ls_parts>-functioncall-name ) )
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
                  i_tool_name   = to_upper( <ls_parts>-functioncall-name )
                  i_json        = <l_data>
                IMPORTING
                  e_t_files     = DATA(lt_tool_response_files)
                RECEIVING
                  r_response    = DATA(l_tool_response)
              ).

              IF lo_fc_approvals IS BOUND.

                lo_fc_approvals->set_approval_as_used(
                  EXPORTING
                    i_tool_name     = to_upper( condense( <ls_parts>-functioncall-name ) )
                    i_o_persistence = me->_o_persistence
                ).

              ENDIF.

            ELSE.
              l_tool_response = l_tool_call_approval_response.
            ENDIF.

            "The response cannot be just a text. It must be an object with any attribute(s) name(s).
            l_tool_response = '{"text":' && lo_aai_util->serialize( l_tool_response ) && '}'.

            me->_append_to_history(
              EXPORTING
                i_s_response = VALUE #( parts = VALUE #( ( function_response = VALUE #( name = <ls_parts>-functioncall-name
                                                                                        response = l_tool_response )
                ) ) role = yif_aai_google=>mc_role_user )
              IMPORTING
                e_seqno = l_seqno
            ).

            IF me->_o_persistence IS BOUND.

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

                  ls_message_images = VALUE yif_aai_google~ty_message_images_s( seqno = l_seqno
                                                                                images = CORRESPONDING #( lt_tool_images ) ).

                  INSERT ls_message_images INTO TABLE me->_t_message_images.

                ENDIF.

                IF lt_tool_files IS NOT INITIAL.

                  ls_message_files = VALUE yif_aai_google~ty_message_files_s( seqno = l_seqno
                                                                              files = CORRESPONDING #( lt_tool_files ) ).

                  INSERT ls_message_files INTO TABLE me->_t_message_files.

                ENDIF.

                FREE: lt_tool_response_files,
                      lt_tool_images,
                      lt_tool_files.

              ENDIF.
              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            ENDIF.

          ENDLOOP.

        ENDLOOP.

        IF l_function_call = abap_true.

          IF l_tool_calls >= me->_max_tool_calls.

            APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

            "The maximum number of tool calls allowed has been reached.
            MESSAGE ID 'YAAI' TYPE 'S' NUMBER '017' INTO l_message.

            l_message = '{"text": ' && lo_aai_util->serialize( l_message ) && '}'.

            <ls_msg> = VALUE #( role = yif_aai_google=>mc_role_user
                                parts = VALUE #( ( l_message ) ) ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

          ENDIF.

          CONTINUE.
        ENDIF.

        IF e_t_response IS REQUESTED.

          SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

        ENDIF.

      ELSE.

        me->_o_connection->get_error_text(
          IMPORTING
            e_error_text = e_response
        ).

        IF e_t_response IS REQUESTED.
          APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
          <l_response> = e_response.
        ENDIF.

        APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = yif_aai_google=>mc_role_model
                            parts = VALUE #( ( '{"text": ' && lo_aai_util->serialize( e_response ) && '}' ) ) ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

        RAISE EVENT on_message_failed
          EXPORTING
            error_text = e_response.

      ENDIF.

      EXIT.

    ENDDO.

  ENDMETHOD.


  METHOD yif_aai_google~get_conversation.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    FREE rt_conversation.

    rt_conversation = me->_t_chat_messages.

    LOOP AT rt_conversation ASSIGNING FIELD-SYMBOL(<ls_msg>).

      DATA(l_index) = sy-tabix.

      IF <ls_msg>-role = yif_aai_google=>mc_role_user.

        READ TABLE me->_t_messages_db ASSIGNING FIELD-SYMBOL(<ls_message_db>) INDEX l_index.

        " If the message is already persisted use SEQNO
        IF sy-subrc = 0.
          l_index = <ls_message_db>-seqno.
        ENDIF.

        READ TABLE me->_t_message_images ASSIGNING FIELD-SYMBOL(<ls_message_images>)
          WITH KEY seqno = l_index.

        IF sy-subrc = 0.

          LOOP AT <ls_message_images>-images ASSIGNING FIELD-SYMBOL(<ls_image>).

            DATA(ls_inline_data_image) = VALUE yif_aai_google=>ty_parts_inline_data_s( ).

            ls_inline_data_image-inline_data-data = <ls_image>-file_data.
            ls_inline_data_image-inline_data-mime_type = <ls_image>-mime_type.

            DATA(l_inline_data_image_json) = lo_aai_util->serialize( i_data = ls_inline_data_image ).

            APPEND l_inline_data_image_json TO <ls_msg>-parts.

          ENDLOOP.

        ENDIF.

        READ TABLE me->_t_message_files ASSIGNING FIELD-SYMBOL(<ls_message_files>)
            WITH KEY seqno = l_index.

        IF sy-subrc = 0.

          LOOP AT <ls_message_files>-files ASSIGNING FIELD-SYMBOL(<ls_file>).

            DATA(ls_inline_data_file) = VALUE yif_aai_google=>ty_parts_inline_data_s( ).

            ls_inline_data_file-inline_data-data = <ls_file>-file_data.
            ls_inline_data_file-inline_data-mime_type = <ls_file>-mime_type.

            DATA(l_inline_data_file_json) = lo_aai_util->serialize( i_data = ls_inline_data_file ).

            APPEND l_inline_data_file_json TO <ls_msg>-parts.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD yif_aai_google~get_history.

    e_t_history = me->_t_chat_messages.

  ENDMETHOD.


  METHOD yif_aai_google~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.


  METHOD yif_aai_google~set_history.

    me->_t_chat_messages = i_t_history.

  ENDMETHOD.


  METHOD yif_aai_google~set_model.

    me->_model = i_model.

  ENDMETHOD.


  METHOD yif_aai_google~set_persistence.

    me->_o_persistence = i_o_persistence.

  ENDMETHOD.


  METHOD yif_aai_google~set_system_instructions.

    me->_system_instructions = i_system_instructions.

  ENDMETHOD.


  METHOD yif_aai_google~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.


  METHOD _append_to_history.

    FIELD-SYMBOLS <ls_data> TYPE any.

    DATA: ls_parts_text        TYPE yif_aai_google~ty_parts_response_text_s,
          ls_function_call     TYPE yif_aai_google~ty_parts_request_func_call_s,
          ls_function_response TYPE yif_aai_google~ty_parts_response_func_resp_s,
          ls_contents          TYPE yif_aai_google~ty_contents_s.

    DATA: l_json_parts TYPE /ui2/cl_json=>json.

    CLEAR e_seqno.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT i_s_response-parts ASSIGNING FIELD-SYMBOL(<ls_parts>).

      ls_parts_text = CORRESPONDING #( <ls_parts> ).
      ls_function_call-function_call = <ls_parts>-functioncall.
      ls_function_call-thought_signature = <ls_parts>-thought_signature.
      ls_function_response = CORRESPONDING #( <ls_parts> ).

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN ls_function_call-function_call-args WITH space.

      DO 3 TIMES.

        CASE sy-index.

          WHEN 1.

            ls_parts_text-text = lo_aai_util->replace_unicode_escape_seq( ls_parts_text-text ).

            ASSIGN ls_parts_text TO <ls_data>.

          WHEN 2.

            " The Thought Signatures recently implemented by Google may return an
            " empty function call item in the response parts.
            " Interestingly, this item cannot be sent back to the API as part of the conversation history,
            " as it results in an INVALID_ARGUMENT error.
            " As a workaround, we ignore this empty function call and do not persist it.
            "-------------------------------------------------------
            IF ls_function_call-function_call-name IS INITIAL.
              CLEAR ls_function_call.
            ENDIF.
            "-------------------------------------------------------

            ASSIGN ls_function_call TO <ls_data>.

          WHEN 3.

            ASSIGN ls_function_response TO <ls_data>.

        ENDCASE.

        IF <ls_data> IS NOT INITIAL.

          IF l_json_parts IS INITIAL.

            l_json_parts = lo_aai_util->serialize( <ls_data> ).

          ELSE.

            l_json_parts = |{ l_json_parts }, { lo_aai_util->serialize( <ls_data> ) }|.

          ENDIF.

        ENDIF.

      ENDDO.

    ENDLOOP.

    APPEND l_json_parts TO ls_contents-parts.

    ls_contents-role = i_s_response-role.

    APPEND ls_contents TO me->_t_chat_messages.

    IF me->_o_persistence IS BOUND.

      me->_o_persistence->persist_message(
        EXPORTING
          i_data = ls_contents
          i_tokens = i_tokens
          i_model = CONV #( me->_model )
        IMPORTING
          e_seqno = e_seqno ).

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_google~set_endpoint.

    me->m_endpoint = i_endpoint.

  ENDMETHOD.

  METHOD _get_files.

    " Supported image file types
    " PNG (.png) → image/png
    " JPEG (.jpeg and .jpg) → image/jpeg
    " WEBP (.webp) → image/webp

    CONSTANTS: lc_png  TYPE string VALUE 'image/png'  ##NO_TEXT,
               lc_jpeg TYPE string VALUE 'image/jpeg' ##NO_TEXT,
               lc_webp TYPE string VALUE 'image/webp' ##NO_TEXT.

    FREE: e_t_images, e_t_files.

    LOOP AT i_t_files ASSIGNING FIELD-SYMBOL(<ls_file>).

      CASE condense( to_lower( <ls_file>-file_type ) ).

        WHEN lc_png OR lc_jpeg OR lc_webp.

          APPEND VALUE #( filename = <ls_file>-filename
                          mime_type = <ls_file>-file_type
                          file_data = <ls_file>-content ) TO e_t_images.

        WHEN OTHERS.

          APPEND VALUE #( filename = <ls_file>-filename
                          mime_type = <ls_file>-file_type
                          file_data = <ls_file>-content ) TO e_t_files.

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
