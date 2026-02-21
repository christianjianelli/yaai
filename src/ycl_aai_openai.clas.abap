CLASS ycl_aai_openai DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_openai.
    INTERFACES yif_aai_chat.

    ALIASES on_message_send FOR yif_aai_chat~on_message_send.
    ALIASES on_response_received FOR yif_aai_chat~on_response_received.
    ALIASES on_message_failed FOR yif_aai_chat~on_message_failed.
    ALIASES on_chat_is_blocked FOR yif_aai_chat~on_chat_is_blocked.

    ALIASES set_model FOR yif_aai_openai~set_model.
    ALIASES use_completions FOR yif_aai_openai~use_completions.
    ALIASES set_system_instructions FOR yif_aai_openai~set_system_instructions.
    ALIASES set_connection FOR yif_aai_openai~set_connection.
    ALIASES set_endpoint FOR yif_aai_openai~set_endpoint.
    ALIASES set_persistence FOR yif_aai_openai~set_persistence.
    ALIASES set_temperature FOR yif_aai_openai~set_temperature.
    ALIASES set_reasoning_effort FOR yif_aai_openai~set_reasoning_effort.
    ALIASES set_verbosity FOR yif_aai_openai~set_verbosity.
    ALIASES bind_tools FOR yif_aai_openai~bind_tools.
    ALIASES generate FOR yif_aai_openai~generate.
    ALIASES chat_completions FOR yif_aai_openai~chat_completions.
    ALIASES embed FOR yif_aai_openai~embed.
    ALIASES chat FOR yif_aai_chat~chat.
    ALIASES set_history FOR yif_aai_openai~set_history.
    ALIASES get_conversation FOR yif_aai_openai~get_conversation.
    ALIASES get_conversation_chat_comp FOR yif_aai_openai~get_conversation_chat_comp.

    ALIASES mo_function_calling FOR yif_aai_openai~mo_function_calling.
    ALIASES mo_agent FOR yif_aai_openai~mo_agent.
    ALIASES m_endpoint FOR yif_aai_openai~m_endpoint.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_openai READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_openai.

    METHODS constructor
      IMPORTING
        i_api                 TYPE csequence OPTIONAL
        i_model               TYPE csequence OPTIONAL
        i_use_completions     TYPE abap_bool DEFAULT abap_false
        i_parallel_tool_calls TYPE abap_bool DEFAULT abap_true
        i_safety_identifier   TYPE csequence OPTIONAL
        i_t_history           TYPE yif_aai_openai~ty_generate_messages_t OPTIONAL
        i_o_prompt            TYPE REF TO yif_aai_prompt OPTIONAL
        i_o_connection        TYPE REF TO yif_aai_conn OPTIONAL
        i_o_persistence       TYPE REF TO yif_aai_db OPTIONAL
        i_o_agent             TYPE REF TO yif_aai_agent OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection  TYPE REF TO yif_aai_conn,
          _o_persistence TYPE REF TO yif_aai_db.

    DATA: _model                     TYPE string,
          _use_completions           TYPE abap_bool VALUE abap_false,
          _temperature               TYPE p LENGTH 2 DECIMALS 1,
          _parallel_tool_calls       TYPE abap_bool VALUE abap_false,
          _safety_identifier         TYPE string,
          _verbosity                 TYPE string,
          _reasoning_effort          TYPE string,
          _system_instructions       TYPE string,
          _system_instructions_role  TYPE string,
          _openai_generate_request   TYPE yif_aai_openai~ty_openai_generate_request_s,
          _openai_generate_response  TYPE yif_aai_openai~ty_openai_generate_response_s,
          _openai_chat_comp_response TYPE yif_aai_openai~ty_openai_chat_comp_resp_s,
          _messages                  TYPE yif_aai_openai~ty_generate_messages_t,
          _max_tool_calls            TYPE i.

    METHODS _load_agent_settings.

ENDCLASS.



CLASS ycl_aai_openai IMPLEMENTATION.


  METHOD constructor.

    IF i_model IS NOT INITIAL.
      me->_model = i_model.
    ELSE.
      DATA(l_id) = yif_aai_const=>c_openai.
      IF i_api IS NOT INITIAL.
        l_id = i_api.
      ENDIF.
      SELECT model FROM yaai_model
        WHERE id = @l_id
          AND default_model = @abap_true
         INTO @me->_model
         UP TO 1 ROWS.                                  "#EC CI_NOORDER
      ENDSELECT.
      IF sy-subrc <> 0.
        me->_model = 'gpt-5-nano'. " default model
      ENDIF.
    ENDIF.

    me->_messages = i_t_history.

    me->_temperature = 1. "non gpt5 models

    me->_verbosity = yif_aai_openai~mc_verbosity_medium.

    me->_reasoning_effort = yif_aai_openai~mc_reasoning_effort_medium.

    me->_parallel_tool_calls = i_parallel_tool_calls.

    me->_safety_identifier = COND #( WHEN i_safety_identifier IS SUPPLIED THEN i_safety_identifier
                                     ELSE cl_abap_context_info=>get_user_technical_name( ) ).

    me->_max_tool_calls = 10.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

    IF i_o_persistence IS SUPPLIED.

      me->_o_persistence = i_o_persistence.

      me->_o_persistence->get_chat(
        IMPORTING
          e_t_msg_data = me->_messages
      ).

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
        i_api = CONV #( me->_o_connection->m_api )
    ).

    IF ls_model-model IS NOT INITIAL.
      me->_model = ls_model-model.
    ENDIF.

    IF ls_model-temperature IS NOT INITIAL.
      me->_temperature = ls_model-temperature.
    ENDIF.

    IF ls_model-verbosity IS NOT INITIAL.
      me->_verbosity = ls_model-verbosity.
    ENDIF.

    IF ls_model-reasoning IS NOT INITIAL.
      me->_reasoning_effort = ls_model-reasoning.
    ENDIF.

    IF ls_model-max_tool_calls IS NOT INITIAL.
      me->_max_tool_calls = ls_model-max_tool_calls.
    ENDIF.

    DATA(l_system_instructions) = me->mo_agent->get_system_instructions( ).

    IF l_system_instructions IS NOT INITIAL.

      me->set_system_instructions(
        i_system_instructions = l_system_instructions
      ).

    ENDIF.

  ENDMETHOD.

  METHOD yif_aai_chat~chat.

    IF me->_use_completions = abap_false.

      me->generate(
        EXPORTING
          i_message       = i_message
          i_new           = i_new
          i_greeting      = i_greeting
          i_async_task_id = i_async_task_id
          i_o_prompt      = i_o_prompt
          i_o_agent       = i_o_agent
        IMPORTING
          e_response      = e_response
          e_failed        = e_failed
          e_t_response    = e_t_response
      ).

    ELSE.

      me->chat_completions(
        EXPORTING
          i_message       = i_message
          i_new           = i_new
          i_greeting      = i_greeting
          i_async_task_id = i_async_task_id
          i_o_prompt      = i_o_prompt
          i_o_agent       = i_o_agent
        IMPORTING
          e_response      = e_response
          e_failed        = e_failed
          e_t_response    = e_t_response
      ).

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_openai~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tools_calls IS SUPPLIED.
      me->_max_tool_calls = i_max_tools_calls.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_openai~chat_completions.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA: l_json    TYPE string,
          l_tools   TYPE string VALUE '[]',
          l_message TYPE string,
          l_prompt  TYPE string.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    me->_system_instructions_role = 'system'.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    IF me->_o_persistence IS BOUND AND
       me->_o_persistence->is_chat_blocked( ).
      RAISE EVENT on_chat_is_blocked.
      EXIT.
    ENDIF.

    IF i_new = abap_true.
      FREE me->_messages.
    ENDIF.

    IF i_o_agent IS BOUND AND me->mo_agent IS NOT BOUND.
      me->mo_agent = i_o_agent.
      me->_load_agent_settings( ).
    ENDIF.

    IF me->_messages IS INITIAL.

      IF me->_system_instructions IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

        <ls_msg> = VALUE #( role = me->_system_instructions_role
                            content = me->_system_instructions
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_system_instructions( i_data = <ls_msg> ).
        ENDIF.

      ENDIF.

      IF i_greeting IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = 'assistant'
                            content = i_greeting
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

    ELSE.

      IF me->_system_instructions IS NOT INITIAL.

        READ TABLE me->_messages TRANSPORTING NO FIELDS
          WITH KEY role = me->_system_instructions_role.

        IF sy-subrc <> 0.

          INSERT VALUE #( role = me->_system_instructions_role
                          content = me->_system_instructions
                          type = 'message' ) INTO me->_messages INDEX 1.

          IF me->_o_persistence IS BOUND.

            READ TABLE me->_messages ASSIGNING <ls_msg> INDEX 1.

            me->_o_persistence->persist_system_instructions( i_data = <ls_msg> ).

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    IF i_o_prompt IS BOUND.

      l_prompt = i_o_prompt->get_prompt( ).

      l_message = i_o_prompt->get_user_message( ).

    ELSE.

      l_message = i_message.

    ENDIF.

    APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.

    <ls_msg> = VALUE #( role = 'user'
                        content = l_message
                        type = 'message' ).

    IF l_prompt IS NOT INITIAL.

      DATA(ls_prompt) = <ls_msg>.

      ls_prompt-content = l_prompt.

    ENDIF.

    IF me->_o_persistence IS BOUND.
      " persist the user message and the augmented prompt
      me->_o_persistence->persist_message( i_data = <ls_msg>
                                           i_prompt = ls_prompt
                                           i_async_task_id = i_async_task_id
                                           i_model = CONV #( me->_model ) ).
    ENDIF.

    " In memory we keep the augmented prompt instead of the user message
    IF l_prompt IS NOT INITIAL.
      <ls_msg>-content = l_prompt.
    ENDIF.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF me->m_endpoint IS INITIAL.
      me->m_endpoint = yif_aai_const=>c_openai_completions_endpoint.
    ENDIF.

    IF me->mo_agent IS BOUND AND me->mo_function_calling IS NOT BOUND.

      "TODO
      "me->mo_function_calling = NEW ycl_aai_func_call_openai( me->mo_agent ).

    ENDIF.

    DO me->_max_tool_calls TIMES.

      IF me->_o_persistence IS BOUND AND
         me->_o_persistence->is_chat_blocked( ).
        RAISE EVENT on_chat_is_blocked.
        EXIT.
      ENDIF.

*      IF me->_o_connection->create( i_endpoint = me->m_endpoint ).
*
*        FREE me->_openai_chat_comp_response.
*
*        IF me->mo_function_calling IS BOUND.
*
*          me->mo_function_calling->get_tools_chat_completions(
*            IMPORTING
*              e_tools = l_tools
*          ).
*
*        ENDIF.
*
*        IF l_tools = '[]'.
*
*          l_json = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_completions_req_s( model = me->_model
*                                                                                                        stream = abap_false
*                                                                                                        messages = me->get_conversation_chat_comp( ) ) ).
*
*        ELSE.
*
*          l_json = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_comp_tools_req_s( model = me->_model
*                                                                                                       stream = abap_false
*                                                                                                       messages = me->get_conversation_chat_comp( )
*                                                                                                       tools = l_tools ) ).
*
*        ENDIF.
*
*        me->_o_connection->set_body( l_json ).
*
*        FREE l_json.
*
*        RAISE EVENT on_message_send.
*
*        me->_o_connection->execute(
*          IMPORTING
*            e_response = l_json
*            e_failed   = e_failed
*        ).
*
*        IF e_failed = abap_true.
*
*          me->_o_connection->get_error_text(
*            IMPORTING
*              e_error_text = e_response
*          ).
*
*          IF e_t_response IS REQUESTED.
*            APPEND INITIAL LINE TO e_t_response ASSIGNING FIELD-SYMBOL(<l_response>).
*            <l_response> = e_response.
*          ENDIF.
*
*          RAISE EVENT on_message_failed
*            EXPORTING
*              error_text = e_response.
*
*          EXIT.
*
*        ENDIF.
*
*        lo_aai_util->deserialize(
*          EXPORTING
*            i_json = l_json
*          IMPORTING
*            e_data = me->_openai_chat_comp_response
*        ).
*
*        IF me->_openai_chat_comp_response-object = 'error'.
*
*          e_response = |{ me->_openai_chat_comp_response-code }: { me->_openai_chat_comp_response-message }|.
*
*          RAISE EVENT on_message_failed
*            EXPORTING
*              error_text = e_response.
*
*          EXIT.
*        ENDIF.
*
*        RAISE EVENT on_response_received.
*
*        DATA(l_function_call) = abap_false.
*
*        LOOP AT me->_openai_chat_comp_response-choices ASSIGNING FIELD-SYMBOL(<ls_choices>).
*
*          IF <ls_choices>-message-tool_calls IS INITIAL.
*            CONTINUE.
*          ENDIF.
*
*          l_function_call = abap_true.
*
*          LOOP AT <ls_choices>-message-tool_calls ASSIGNING FIELD-SYMBOL(<ls_tool_calls>).
*
*            APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.
*
*            <ls_msg> = VALUE #( role = <ls_choices>-message-role
*                                type = 'function_call'
*                                arguments = <ls_tool_calls>-function-arguments
*                                call_id = <ls_tool_calls>-id
*                                name = <ls_tool_calls>-function-name ).
*
*            IF me->_o_persistence IS BOUND.
*
*              me->_o_persistence->persist_message( i_data = <ls_msg>
*                                                   i_tokens = _openai_chat_comp_response-usage-total_tokens
*                                                   i_model = CONV #( me->_model ) ).
*
*              CLEAR _openai_chat_comp_response-usage-total_tokens.
*
*            ENDIF.
*
*            ASSIGN <ls_tool_calls>-function-arguments TO <l_data>.
*
*            " This deserialization may be necessary depending on how the arguments are received. We may need to parse an escaped string to a JSON string.
*            " Example: parse this "{\"latitude\":48.8566,\"longitude\":2.3522}" to a JSON like {"latitude": 48.8566, "longitude": 2.3522}
*            lo_aai_util->deserialize(
*              EXPORTING
*                i_json = <ls_tool_calls>-function-arguments
*              IMPORTING
*                e_data = lr_data
*            ).
*
*            IF lr_data IS NOT INITIAL.
*
*              DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_data_ref( lr_data ).
*
*              " Make sure the deserialized object is a JSON string before assigning it
*              IF lo_typedescr->type_kind = cl_abap_typedescr=>typekind_string.
*
*                ASSIGN lr_data->* TO <l_data>.
*
*              ENDIF.
*
*            ENDIF.
*
*            me->mo_function_calling->call_tool(
*              EXPORTING
*                i_tool_name   = to_upper( <ls_tool_calls>-function-name )
*                i_json        = <l_data>
*              RECEIVING
*                r_response    = DATA(l_tool_response)
*            ).
*
*            APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.
*
*            <ls_msg> = VALUE #( role = 'tool'
*                                type = 'function_call_output'
*                                call_id = <ls_tool_calls>-id
*                                output = l_tool_response ).
*
*            IF me->_o_persistence IS BOUND.
*              me->_o_persistence->persist_message( i_data = <ls_msg>
*                                                   i_model = CONV #( me->_model ) ).
*            ENDIF.
*
*          ENDLOOP.
*
*        ENDLOOP.
*
*        IF l_function_call = abap_true.
*          CONTINUE.
*        ENDIF.
*
*        LOOP AT me->_openai_chat_comp_response-choices ASSIGNING <ls_choices>.
*
*          IF <ls_choices>-message-role <> 'assistant'.
*            CONTINUE.
*          ENDIF.
*
*          e_response = <ls_choices>-message-content.
*
*          APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.
*
*          <ls_msg> = VALUE #( role = <ls_choices>-message-role
*                              type = 'message'
*                              content = e_response ).
*
*          IF me->_o_persistence IS BOUND.
*            me->_o_persistence->persist_message( i_data = <ls_msg>
*                                                 i_tokens = _openai_chat_comp_response-usage-total_tokens
*                                                 i_model = CONV #( me->_model ) ).
*          ENDIF.
*
*        ENDLOOP.
*
*        EXIT.
*
*      ELSE.
*
*        me->_o_connection->get_error_text(
*          IMPORTING
*            e_error_text = e_response
*        ).
*
*        IF e_t_response IS REQUESTED.
*          APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
*          <l_response> = e_response.
*        ENDIF.
*
*      ENDIF.

    ENDDO.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_openai~embed.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
    ENDIF.

    IF me->m_endpoint IS INITIAL.
      me->m_endpoint = yif_aai_const=>c_openai_embed_endpoint.
    ENDIF.

*    IF me->_o_connection->create( i_endpoint = me->m_endpoint ).
*
*      DATA(lo_aai_util) = NEW ycl_aai_util( ).
*
*      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_embed_request_s( model = me->_model
*                                                                                                        input = i_input ) ).
*
*      me->_o_connection->set_body( l_json ).
*
*      FREE l_json.
*
*      me->_o_connection->execute(
*        IMPORTING
*          e_response = l_json
*      ).
*
*      lo_aai_util->deserialize(
*        EXPORTING
*          i_json = l_json
*        IMPORTING
*          e_data = e_s_response
*      ).
*
*    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_openai~generate.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA lt_tools TYPE STANDARD TABLE OF yaai_tools WITH DEFAULT KEY.

    DATA: l_tools   TYPE string VALUE '[]',
          l_message TYPE string,
          l_prompt  TYPE string.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.
      RETURN.
    ENDIF.

    IF me->_o_persistence IS BOUND AND
       me->_o_persistence->is_chat_blocked( ).
      RAISE EVENT on_chat_is_blocked.
      EXIT.
    ENDIF.

    IF i_new = abap_true.
      FREE me->_messages.
    ENDIF.

    IF i_o_agent IS BOUND AND me->mo_agent IS NOT BOUND.
      me->mo_agent = i_o_agent.
      me->_load_agent_settings( ).
    ENDIF.

    IF me->_messages IS INITIAL.

      IF me->_system_instructions IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

        <ls_msg> = VALUE #( role = me->_system_instructions_role
                            content = me->_system_instructions
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

      IF i_greeting IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = 'assistant'
                            content = i_greeting
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

    ELSE.

      IF me->_system_instructions IS NOT INITIAL.

        READ TABLE me->_messages TRANSPORTING NO FIELDS
          WITH KEY role = 'developer'.

        IF sy-subrc <> 0.

          INSERT VALUE #( role = 'developer'
                          content = me->_system_instructions
                          type = 'message' ) INTO me->_messages INDEX 1.

          READ TABLE me->_messages ASSIGNING <ls_msg> INDEX 1.

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_system_instructions( i_data = <ls_msg> ).
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    IF i_o_prompt IS BOUND.

      l_prompt = i_o_prompt->get_prompt( ).

      l_message = i_o_prompt->get_user_message( ).

    ELSE.

      l_message = i_message.

    ENDIF.

    APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.

    <ls_msg> = VALUE #( role = 'user'
                        content = l_message
                        type = 'message' ).

    IF l_prompt IS NOT INITIAL.

      DATA(ls_prompt) = <ls_msg>.

      ls_prompt-content = l_prompt.

    ENDIF.

    IF me->_o_persistence IS BOUND.
      " persist the user message and the augmented prompt
      me->_o_persistence->persist_message( i_data = <ls_msg>
                                           i_prompt = ls_prompt
                                           i_async_task_id = i_async_task_id
                                           i_model = CONV #( me->_model ) ).
    ENDIF.

    " In memory we keep the augmented prompt instead of the user message
    IF l_prompt IS NOT INITIAL.
      <ls_msg>-content = l_prompt.
    ENDIF.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF me->m_endpoint IS INITIAL.
      me->m_endpoint = yif_aai_const=>c_openai_generate_endpoint.
    ENDIF.

    IF me->mo_agent IS BOUND AND me->mo_function_calling IS NOT BOUND.

      me->mo_function_calling = NEW ycl_aai_func_call_openai( me->mo_agent ).

    ENDIF.

    DO me->_max_tool_calls TIMES.

      IF me->_o_persistence IS BOUND AND
         me->_o_persistence->is_chat_blocked( ).
        RAISE EVENT on_chat_is_blocked.
        EXIT.
      ENDIF.

      IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_openai_generate_endpoint ).

        FREE me->_openai_generate_response.

        IF me->mo_function_calling IS BOUND.

          me->mo_function_calling->get_tools(
            IMPORTING
              e_tools = l_tools
          ).

        ENDIF.

        IF me->_model CP 'gpt-5*'.

          DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_generate_request_s( model = me->_model
                                                                                                             stream = abap_false
                                                                                                             text-verbosity = me->_verbosity
                                                                                                             reasoning-effort = me->_reasoning_effort
                                                                                                             parallel_tool_calls = me->_parallel_tool_calls
                                                                                                             safety_identifier = me->_safety_identifier
                                                                                                             input = me->get_conversation( )
                                                                                                             tools = l_tools ) ).
        ELSE.

          l_json = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_generate_req_wt_s( model = me->_model
                                                                                                      stream = abap_false
                                                                                                      temperature = me->_temperature
                                                                                                      parallel_tool_calls = me->_parallel_tool_calls
                                                                                                      safety_identifier = me->_safety_identifier
                                                                                                      input = me->get_conversation( )
                                                                                                      tools = l_tools ) ).
        ENDIF.

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

          RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

          EXIT.

        ENDIF.

        "l_json = lo_aai_util->replace_unicode_escape_seq( i_content = l_json ).

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = me->_openai_generate_response
        ).

        RAISE EVENT on_response_received.

        DATA(l_function_call) = abap_false.

        LOOP AT _openai_generate_response-output ASSIGNING FIELD-SYMBOL(<ls_output>).

          IF <ls_output>-type <> 'function_call'.
            CONTINUE.
          ENDIF.

          l_function_call = abap_true.

          APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( type = 'function_call'
                              arguments = <ls_output>-arguments
                              call_id = <ls_output>-call_id
                              name = <ls_output>-name ).

          IF me->_o_persistence IS BOUND.

            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_tokens = _openai_generate_response-usage-total_tokens
                                                 i_model = CONV #( me->_model ) ).

            CLEAR _openai_generate_response-usage-total_tokens.

          ENDIF.

          ASSIGN <ls_output>-arguments TO <l_data>.

          " This deserialization may be necessary depending on how the arguments are received. We may need to parse an escaped string to a JSON string.
          " Example: parse this "{\"latitude\":48.8566,\"longitude\":2.3522}" to a JSON like {"latitude": 48.8566, "longitude": 2.3522}
          lo_aai_util->deserialize(
            EXPORTING
              i_json = <ls_output>-arguments
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

          me->mo_function_calling->call_tool(
            EXPORTING
              i_tool_name   = to_upper( <ls_output>-name )
              i_json        = <l_data>
            RECEIVING
              r_response    = DATA(l_tool_response)
          ).

          APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( type = 'function_call_output'
                              call_id = <ls_output>-call_id
                              output = l_tool_response ).

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_model = CONV #( me->_model ) ).
          ENDIF.

        ENDLOOP.

        IF l_function_call = abap_true.
          CONTINUE.
        ENDIF.

        IF _openai_generate_response-error IS NOT INITIAL.

          e_response = |{ _openai_generate_response-error-code }: { _openai_generate_response-error-message }|.

          RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

          EXIT.

        ENDIF.

        LOOP AT _openai_generate_response-output ASSIGNING <ls_output>.

          IF <ls_output>-type <> 'message' OR <ls_output>-role <> 'assistant'.
            CONTINUE.
          ENDIF.

          LOOP AT <ls_output>-content ASSIGNING FIELD-SYMBOL(<ls_content>).

            IF <ls_content>-type <> 'output_text'.
              CONTINUE.
            ENDIF.

            APPEND INITIAL LINE TO me->_messages ASSIGNING <ls_msg>.

            <ls_content>-text = lo_aai_util->replace_unicode_escape_seq( <ls_content>-text ).

            <ls_msg> = VALUE #( role = <ls_output>-role
                                content = <ls_content>-text
                                type = <ls_output>-type ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_tokens = _openai_generate_response-usage-total_tokens
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

            e_response = e_response && <ls_content>-text.

          ENDLOOP.

        ENDLOOP.

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

      ENDIF.

    ENDDO.

    IF e_response IS INITIAL.

      e_response = 'We''re having a little trouble getting a complete answer to your question at the moment.'.

    ENDIF.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_openai~get_conversation.

    DATA l_json TYPE string.

    CLEAR r_conversation.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      CLEAR l_json.

      CASE to_lower( <ls_message>-type ).

        WHEN 'message'.

          DATA(ls_message) = CORRESPONDING yif_aai_openai~ty_type_message_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_message ).

        WHEN 'function_call'.

          DATA(ls_function_call) = CORRESPONDING yif_aai_openai~ty_function_call_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_function_call ).

        WHEN 'function_call_output'.

          DATA(ls_function_call_output) = CORRESPONDING yif_aai_openai~ty_function_call_output_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_function_call_output ).

      ENDCASE.

      IF r_conversation IS INITIAL.
        r_conversation = l_json.
      ELSE.
        r_conversation = |{ r_conversation }, { l_json }|.
      ENDIF.

    ENDLOOP.

    r_conversation = |[{ r_conversation }]|.

  ENDMETHOD.


  METHOD yif_aai_openai~get_conversation_chat_comp.

    DATA l_json TYPE string.

    CLEAR r_conversation.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      CLEAR l_json.

      CASE to_lower( <ls_message>-type ).

        WHEN 'message'.

          DATA(ls_message) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_nt_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_message ).

        WHEN 'function_call'.

          DATA(ls_function_call) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_tc_s( <ls_message> ).

          ls_function_call-tool_calls = VALUE #( ( id = <ls_message>-call_id
                                                   type = 'function'
                                                   function = VALUE #( name = <ls_message>-name
                                                                       arguments = <ls_message>-arguments ) ) ).

          l_json = lo_aai_util->serialize( ls_function_call ).

        WHEN 'function_call_output'.

          DATA(ls_function_call_output) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_tr_s( <ls_message> ).

          ls_function_call_output-content = <ls_message>-output.
          ls_function_call_output-tool_call_id = <ls_message>-call_id.

          l_json = lo_aai_util->serialize( ls_function_call_output ).

      ENDCASE.

      IF r_conversation IS INITIAL.
        r_conversation = l_json.
      ELSE.
        r_conversation = |{ r_conversation }, { l_json }|.
      ENDIF.

    ENDLOOP.

    r_conversation = |[{ r_conversation }]|.

  ENDMETHOD.


  METHOD yif_aai_openai~get_history.

    e_t_history = me->_messages.

  ENDMETHOD.


  METHOD yif_aai_openai~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.


  METHOD yif_aai_openai~set_history.

    me->_messages = i_t_history.

  ENDMETHOD.


  METHOD yif_aai_openai~set_model.

    me->_model = i_model.

  ENDMETHOD.


  METHOD yif_aai_openai~set_persistence.

    me->_o_persistence = i_o_persistence.

  ENDMETHOD.


  METHOD yif_aai_openai~set_reasoning_effort.

    me->_reasoning_effort = i_reasoning_effort.

  ENDMETHOD.


  METHOD yif_aai_openai~set_system_instructions.

    me->_system_instructions = i_system_instructions.
    me->_system_instructions_role = i_system_instructions_role.

  ENDMETHOD.


  METHOD yif_aai_openai~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.


  METHOD yif_aai_openai~set_verbosity.

    me->_verbosity = i_verbosity.

  ENDMETHOD.


  METHOD yif_aai_openai~use_completions.

    me->_use_completions = i_use_completions.

  ENDMETHOD.


  METHOD yif_aai_openai~set_endpoint.

    me->m_endpoint = i_endpoint.

  ENDMETHOD.
ENDCLASS.
