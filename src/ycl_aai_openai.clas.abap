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
    ALIASES audio_transcription FOR yif_aai_openai~audio_transcription.

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
          _o_persistence TYPE REF TO yif_aai_db,
          _o_log         TYPE REF TO ycl_aai_log.

    DATA: _model                        TYPE string,
          _max_tool_calls               TYPE i,
          _use_completions              TYPE abap_bool VALUE abap_false,
          _temperature                  TYPE p LENGTH 2 DECIMALS 1,
          _parallel_tool_calls          TYPE abap_bool VALUE abap_false,
          _safety_identifier            TYPE string,
          _verbosity                    TYPE string,
          _reasoning_effort             TYPE string,
          _system_instructions          TYPE string,
          _system_instructions_role     TYPE string,
          _s_openai_generate_response   TYPE yif_aai_openai~ty_openai_generate_response_s,
          _s_openai_chat_comp_response  TYPE yif_aai_openai~ty_openai_chat_comp_resp_s,
          _s_openai_transcript_response TYPE yif_aai_openai~ty_openai_transcription_resp_s,
          _t_messages                   TYPE yif_aai_openai~ty_generate_messages_t,
          _t_messages_db                TYPE yif_aai_db=>ty_messages_t,
          _t_message_images             TYPE yif_aai_openai~ty_message_images_t,
          _t_message_files              TYPE yif_aai_openai~ty_message_files_t.


    METHODS _load_agent_settings.

    METHODS _get_files
      IMPORTING
        i_t_files     TYPE ytt_aai_files
      EXPORTING
        e_t_images    TYPE yif_aai_openai~ty_images_t
        e_t_files     TYPE yif_aai_openai~ty_files_t
        e_t_images_db TYPE yif_aai_openai~ty_message_images_t
        e_t_files_db  TYPE yif_aai_openai~ty_message_files_t.

    METHODS _log
      IMPORTING
        i_s_msg TYPE bapiret2.

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

      SELECT model
        FROM yaai_model
        WHERE id = @l_id
          AND default_model = @abap_true
         INTO @me->_model
         UP TO 1 ROWS.                                  "#EC CI_NOORDER
      ENDSELECT.

      IF sy-subrc <> 0.

        SELECT model
          FROM yaai_model
          WHERE id = @l_id
           INTO @me->_model
           UP TO 1 ROWS.                                "#EC CI_NOORDER
        ENDSELECT.

      ENDIF.

    ENDIF.

    me->_t_messages = i_t_history.

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
          e_t_messages = me->_t_messages_db
          e_t_msg_data = me->_t_messages
      ).

      " Images and files
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      LOOP AT me->_t_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

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

            DATA(ls_message_images) = VALUE yif_aai_openai~ty_message_images_s( seqno = <ls_message_db>-seqno
                                                                                images = CORRESPONDING #( lt_images ) ).

            INSERT ls_message_images INTO TABLE me->_t_message_images.

          ENDIF.

          FREE lt_images.

          IF lt_files IS NOT INITIAL.

            DATA(ls_message_files) = VALUE yif_aai_openai~ty_message_files_s( seqno = <ls_message_db>-seqno
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


  METHOD yif_aai_chat~chat.

    IF me->_use_completions = abap_false.

      me->generate(
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

    ELSE.

      me->chat_completions(
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

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_openai~audio_transcription.

    DATA: l_json     TYPE string,
          l_boundary TYPE string,
          l_head     TYPE string,
          l_tail     TYPE string,
          l_head_x   TYPE xstring,
          l_tail_x   TYPE xstring,
          l_body     TYPE xstring.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
    ENDIF.

    IF me->m_endpoint IS INITIAL.
      me->m_endpoint = yif_aai_const=>c_openai_audio_trans_endpoint.
    ENDIF.

    IF me->_o_connection->create_connection( i_endpoint = me->m_endpoint ).

      me->_o_connection->get_http_client(
        IMPORTING
          e_http_client = DATA(lo_http_client)
      ).

      TRY.

          l_boundary = |----OpenAIFormBoundary{ cl_system_uuid=>create_uuid_c32_static( ) }|.

        CATCH cx_uuid_error ##NO_HANDLER.
          l_boundary = '----OpenAIFormBoundary7MA4YWxkTrZu0gW'.
      ENDTRY.

      " Build multipart body
      l_head =
         |--{ l_boundary }{ cl_abap_char_utilities=>cr_lf }| &&
         |Content-Disposition: form-data; name="model"{ cl_abap_char_utilities=>cr_lf }{ cl_abap_char_utilities=>cr_lf }| &&
         |gpt-4o-transcribe{ cl_abap_char_utilities=>cr_lf }| &&
         |--{ l_boundary }{ cl_abap_char_utilities=>cr_lf }| &&
         |Content-Disposition: form-data; name="response_format"{ cl_abap_char_utilities=>cr_lf }{ cl_abap_char_utilities=>cr_lf }| &&
         |json{ cl_abap_char_utilities=>cr_lf }| &&
         |--{ l_boundary }{ cl_abap_char_utilities=>cr_lf }| &&
         |Content-Disposition: form-data; name="file"; filename="{ i_filename }"{ cl_abap_char_utilities=>cr_lf }| &&
         |Content-Type: audio/mpeg{ cl_abap_char_utilities=>cr_lf }{ cl_abap_char_utilities=>cr_lf }|.

      l_tail = |{ cl_abap_char_utilities=>cr_lf }--{ l_boundary }--{ cl_abap_char_utilities=>cr_lf }|.

      l_head_x = cl_abap_codepage=>convert_to( source = l_head ).
      l_tail_x = cl_abap_codepage=>convert_to( source = l_tail ).

      CONCATENATE l_head_x i_input l_tail_x INTO l_body IN BYTE MODE.

      lo_http_client->request->set_header_field( name = 'Content-Type' value = |multipart/form-data; boundary={ l_boundary }| ).

      lo_http_client->request->set_data( l_body ).

      me->_o_connection->do_receive(
        IMPORTING
          e_response = l_json
          e_failed   = DATA(l_failed)
      ).

      NEW ycl_aai_util( )->deserialize(
        EXPORTING
          i_json = l_json
        IMPORTING
          e_data = me->_s_openai_transcript_response
      ).

      e_response = me->_s_openai_transcript_response-text.

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

    DATA: lt_tool_images TYPE yif_aai_openai~ty_images_t,
          lt_tool_files  TYPE yif_aai_openai~ty_files_t.

    DATA: l_json          TYPE string,
          l_tools         TYPE string VALUE '[]',
          l_message       TYPE string,
          l_prompt        TYPE string,
          l_tool_response TYPE string.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    me->_system_instructions_role = 'system'.

    IF me->_model IS INITIAL.

      DATA(l_api) = yif_aai_const=>c_openai.

      IF me->_o_connection IS BOUND.
        l_api = me->_o_connection->m_api.
      ENDIF.

      me->_log( i_s_msg = VALUE #( number = '018' message_v1 = l_api ) ).

      MESSAGE ID 'YAAI' TYPE 'E' NUMBER '018' WITH l_api INTO DATA(l_error_018).

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

    IF i_new = abap_true.
      FREE me->_t_messages.
    ENDIF.

    IF i_o_agent IS BOUND AND me->mo_agent IS NOT BOUND.
      me->mo_agent = i_o_agent.
      me->_load_agent_settings( ).
    ENDIF.

    IF me->_t_messages IS INITIAL.

      IF me->_system_instructions IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_t_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

        <ls_msg> = VALUE #( role = me->_system_instructions_role
                            content = me->_system_instructions
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_system_instructions( i_data = <ls_msg> ).
        ENDIF.

      ENDIF.

      IF i_greeting IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = 'assistant'
                            content = i_greeting
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

    ELSE.

      IF me->_system_instructions IS NOT INITIAL.

        READ TABLE me->_t_messages TRANSPORTING NO FIELDS
          WITH KEY role = me->_system_instructions_role.

        IF sy-subrc <> 0.

          INSERT VALUE #( role = me->_system_instructions_role
                          content = me->_system_instructions
                          type = 'message' ) INTO me->_t_messages INDEX 1.

          IF me->_o_persistence IS BOUND.

            READ TABLE me->_t_messages ASSIGNING <ls_msg> INDEX 1.

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

    APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

    <ls_msg> = VALUE #( role = 'user'
                        content = l_message
                        type = 'message' ).

    IF l_prompt IS NOT INITIAL.

      DATA(ls_prompt) = <ls_msg>.

      ls_prompt-content = l_prompt.

    ENDIF.

    DATA(l_seqno) = lines( me->_t_messages ).

    IF me->_o_persistence IS BOUND.

      " persist the user message and the augmented prompt
      me->_o_persistence->persist_message(
        EXPORTING
          i_data = <ls_msg>
          i_prompt = ls_prompt
          i_async_task_id = i_async_task_id
          i_model = CONV #( me->_model )
        IMPORTING
          e_seqno = l_seqno
      ).

    ENDIF.

    " In memory we keep the augmented prompt instead of the user message
    IF l_prompt IS NOT INITIAL.
      <ls_msg>-content = l_prompt.
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

      DATA(ls_message_images) = VALUE yif_aai_openai~ty_message_images_s( seqno = l_seqno
                                                                          images = CORRESPONDING #( lt_images ) ).

      INSERT ls_message_images INTO TABLE me->_t_message_images.

    ENDIF.

    IF lt_files IS NOT INITIAL.

      DATA(ls_message_files) = VALUE yif_aai_openai~ty_message_files_s( seqno = l_seqno
                                                                        files = CORRESPONDING #( lt_files ) ).

      INSERT ls_message_files INTO TABLE me->_t_message_files.

    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
    ENDIF.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF me->m_endpoint IS INITIAL.
      me->m_endpoint = yif_aai_const=>c_openai_completions_endpoint.
    ENDIF.

    IF me->mo_agent IS BOUND AND me->mo_function_calling IS NOT BOUND.

      me->mo_function_calling = NEW ycl_aai_func_call_openai( me->mo_agent ).

    ENDIF.

    DO ( me->_max_tool_calls + 1 ) TIMES.

      DATA(l_tool_calls) = sy-index.

      IF me->_o_persistence IS BOUND AND
         me->_o_persistence->is_chat_blocked( ).
        RAISE EVENT on_chat_is_blocked.
        EXIT.
      ENDIF.

      IF me->_o_connection->create_connection( i_endpoint = me->m_endpoint ).

        FREE me->_s_openai_chat_comp_response.

        IF me->mo_function_calling IS BOUND.

          me->mo_function_calling->get_tools_chat_completions(
            IMPORTING
              e_tools = l_tools
          ).

        ENDIF.

        IF l_tools = '[]'.

          l_json = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_completions_req_s( model = me->_model
                                                                                                      stream = abap_false
                                                                                                      temperature = me->_temperature
                                                                                                      messages = me->get_conversation_chat_comp( ) ) ).

        ELSE.

          l_json = lo_aai_util->serialize( i_data = VALUE yif_aai_openai~ty_openai_comp_tools_req_s( model = me->_model
                                                                                                     stream = abap_false
                                                                                                     temperature = me->_temperature
                                                                                                     messages = me->get_conversation_chat_comp( )
                                                                                                     tools = l_tools ) ).

        ENDIF.

        me->_o_connection->set_body( l_json ).

*       Uncomment these lines to write the JSON in a file on the server for analysis of its content and structure.
*        IF me->_o_persistence IS BOUND.
*          NEW ycl_aai_log( i_chat_id = me->_o_persistence->m_id )->write_json_on_log_file( l_json ).
*        ENDIF.

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

          IF e_response IS NOT INITIAL.

            IF e_t_response IS REQUESTED.
              APPEND INITIAL LINE TO e_t_response ASSIGNING FIELD-SYMBOL(<l_response>).
              <l_response> = e_response.
            ENDIF.

            APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'assistant'
                                content = e_response
                                type = 'message' ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

          ENDIF.

          RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

          EXIT.

        ENDIF.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
            i_camel_case = abap_true
          IMPORTING
            e_data = me->_s_openai_chat_comp_response
        ).

        IF me->_s_openai_chat_comp_response IS INITIAL OR
           condense( to_upper( me->_s_openai_chat_comp_response-detail ) ) = yif_aai_const=>c_unauthorized.

          MESSAGE e020(yaai) INTO e_response.

          IF me->_s_openai_chat_comp_response-detail IS NOT INITIAL.
            e_response = |{ e_response } Detail: { me->_s_openai_chat_comp_response-detail }|.
          ENDIF.

          APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = 'assistant'
                              content = e_response
                              type = 'message' ).

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

        IF me->_s_openai_chat_comp_response-object = 'error'.

          e_response = |{ me->_s_openai_chat_comp_response-code }: { me->_s_openai_chat_comp_response-message }|.

          IF e_response IS NOT INITIAL.

            APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'assistant'
                                content = e_response
                                type = 'message' ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

          ENDIF.

          RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

          EXIT.
        ENDIF.

        IF me->_s_openai_chat_comp_response-error-message IS NOT INITIAL.

          e_response = |{ me->_s_openai_chat_comp_response-error-code } { me->_s_openai_chat_comp_response-error-message }|.

          APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = 'assistant'
                              content = e_response
                              type = 'message' ).

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

        DATA(l_function_call) = abap_false.

        LOOP AT me->_s_openai_chat_comp_response-choices ASSIGNING FIELD-SYMBOL(<ls_choices>).

          IF <ls_choices>-message-tool_calls IS INITIAL.
            CONTINUE.
          ENDIF.

          l_function_call = abap_true.

          LOOP AT <ls_choices>-message-tool_calls ASSIGNING FIELD-SYMBOL(<ls_tool_calls>).

            APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = <ls_choices>-message-role
                                type = 'function_call'
                                arguments = <ls_tool_calls>-function-arguments
                                call_id = <ls_tool_calls>-id
                                name = <ls_tool_calls>-function-name ).

            IF me->_o_persistence IS BOUND.

              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_tokens = _s_openai_chat_comp_response-usage-total_tokens
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).

              CLEAR _s_openai_chat_comp_response-usage-total_tokens.

            ENDIF.

            ASSIGN <ls_tool_calls>-function-arguments TO <l_data>.

            " This deserialization may be necessary depending on how the arguments are received. We may need to parse an escaped string to a JSON string.
            " Example: parse this "{\"latitude\":48.8566,\"longitude\":2.3522}" to a JSON like {"latitude": 48.8566, "longitude": 2.3522}
            lo_aai_util->deserialize(
              EXPORTING
                i_json = <ls_tool_calls>-function-arguments
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
                  i_tool_name     = to_upper( condense( <ls_tool_calls>-function-name ) )
                  i_o_persistence = me->_o_persistence
                  i_t_tools       = CORRESPONDING #( me->mo_function_calling->mt_methods )
                IMPORTING
                  e_approved      = l_tool_call_approved
                  e_tool_response = DATA(l_tool_call_approval_response)
              ).

              IF l_tool_call_approved = abap_false.
                l_tool_response = l_tool_call_approval_response.
              ENDIF.

            ENDIF.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            IF l_tool_call_approved = abap_true.

              me->mo_function_calling->call_tool(
                EXPORTING
                  i_tool_name   = to_upper( <ls_tool_calls>-function-name )
                  i_json        = <l_data>
                IMPORTING
                  e_t_files     = DATA(lt_tool_response_files)
                RECEIVING
                  r_response    = l_tool_response
              ).

              IF lo_fc_approvals IS BOUND.

                lo_fc_approvals->set_approval_as_used(
                  EXPORTING
                    i_tool_name     = to_upper( condense( <ls_tool_calls>-function-name ) )
                    i_o_persistence = me->_o_persistence
                ).

              ENDIF.

            ENDIF.

            APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'tool'
                                type = 'function_call_output'
                                call_id = <ls_tool_calls>-id
                                output = l_tool_response ).

            IF me->_o_persistence IS BOUND.

              me->_o_persistence->persist_message(
                EXPORTING
                  i_data = <ls_msg>
                  i_async_task_id = i_async_task_id
                  i_model = CONV #( me->_model )
                IMPORTING
                  e_seqno = l_seqno
              ).

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

                  ls_message_images = VALUE yif_aai_openai~ty_message_images_s( seqno = l_seqno
                                                                                images = CORRESPONDING #( lt_tool_images ) ).

                  INSERT ls_message_images INTO TABLE me->_t_message_images.

                ENDIF.

                IF lt_tool_files IS NOT INITIAL.

                  ls_message_files = VALUE yif_aai_openai~ty_message_files_s( seqno = l_seqno
                                                                              files = CORRESPONDING #( lt_tool_files ) ).

                  INSERT ls_message_files INTO TABLE me->_t_message_files.

                ENDIF.

                FREE: lt_tool_response_files,
                      lt_tool_images,
                      lt_tool_files.

              ENDIF.
              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            ENDIF.

            CLEAR l_tool_response.

          ENDLOOP.

        ENDLOOP.

        IF l_function_call = abap_true.

          IF l_tool_calls >= me->_max_tool_calls.

            APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'user'
                                type = 'message' ).

            "The maximum number of tool calls allowed has been reached.
            MESSAGE ID 'YAAI' TYPE 'S' NUMBER '017' INTO <ls_msg>-content.

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

          ENDIF.

          CONTINUE.
        ENDIF.

        LOOP AT me->_s_openai_chat_comp_response-choices ASSIGNING <ls_choices>.

          IF <ls_choices>-message-role <> 'assistant'.
            CONTINUE.
          ENDIF.

          e_response = lo_aai_util->replace_unicode_escape_seq( <ls_choices>-message-content ).

          APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = <ls_choices>-message-role
                              type = 'message'
                              content = e_response ).

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_tokens = _s_openai_chat_comp_response-usage-total_tokens
                                                 i_async_task_id = i_async_task_id
                                                 i_model = CONV #( me->_model ) ).
          ENDIF.

        ENDLOOP.

        EXIT.

      ELSE.

        me->_o_connection->get_error_text(
          IMPORTING
            e_error_text = e_response
        ).

        IF e_response IS NOT INITIAL.

          APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = 'assistant'
                              content = e_response
                              type = 'message' ).

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_async_task_id = i_async_task_id
                                                 i_model = CONV #( me->_model ) ).
          ENDIF.

        ENDIF.

        RAISE EVENT on_message_failed
          EXPORTING
            error_text = e_response.

        IF e_t_response IS REQUESTED.
          APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
          <l_response> = e_response.
        ENDIF.

        EXIT.

      ENDIF.

    ENDDO.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_openai~embed.
    "TODO
  ENDMETHOD.


  METHOD yif_aai_openai~generate.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA: lt_tools       TYPE STANDARD TABLE OF yaai_tools WITH EMPTY KEY,
          lt_tool_images TYPE yif_aai_openai~ty_images_t,
          lt_tool_files  TYPE yif_aai_openai~ty_files_t.

    DATA: l_tools         TYPE string VALUE '[]',
          l_message       TYPE string,
          l_prompt        TYPE string,
          l_tool_response TYPE string.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.

      DATA(l_api) = yif_aai_const=>c_openai.

      IF me->_o_connection IS BOUND.
        l_api = me->_o_connection->m_api.
      ENDIF.

      me->_log( i_s_msg = VALUE #( number = '018' message_v1 = l_api ) ).

      MESSAGE ID 'YAAI' TYPE 'E' NUMBER '018' WITH yif_aai_const=>c_openai INTO DATA(l_error_018).

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

    IF i_new = abap_true.
      FREE me->_t_messages.
    ENDIF.

    IF i_o_agent IS BOUND AND me->mo_agent IS NOT BOUND.
      me->mo_agent = i_o_agent.
      me->_load_agent_settings( ).
    ENDIF.

    IF me->_t_messages IS INITIAL.

      IF me->_system_instructions IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_t_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

        <ls_msg> = VALUE #( role = me->_system_instructions_role
                            content = me->_system_instructions
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

      IF i_greeting IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = 'assistant'
                            content = i_greeting
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

    ELSE.

      IF me->_system_instructions IS NOT INITIAL.

        READ TABLE me->_t_messages TRANSPORTING NO FIELDS
          WITH KEY role = 'developer'.

        IF sy-subrc <> 0.

          INSERT VALUE #( role = 'developer'
                          content = me->_system_instructions
                          type = 'message' ) INTO me->_t_messages INDEX 1.

          READ TABLE me->_t_messages ASSIGNING <ls_msg> INDEX 1.

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

    APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

    <ls_msg> = VALUE #( role = 'user'
                        content = l_message
                        type = 'message' ).

    IF l_prompt IS NOT INITIAL.

      DATA(ls_prompt) = <ls_msg>.

      ls_prompt-content = l_prompt.

    ENDIF.

    DATA(l_seqno) = lines( me->_t_messages ).

    IF me->_o_persistence IS BOUND.

      " persist the user message and the augmented prompt
      me->_o_persistence->persist_message(
        EXPORTING
          i_data = <ls_msg>
          i_prompt = ls_prompt
          i_async_task_id = i_async_task_id
          i_model = CONV #( me->_model )
        IMPORTING
          e_seqno = l_seqno
      ).

    ENDIF.

    " In memory we keep the augmented prompt instead of the user message
    IF l_prompt IS NOT INITIAL.
      <ls_msg>-content = l_prompt.
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

      DATA(ls_message_images) = VALUE yif_aai_openai~ty_message_images_s( seqno = l_seqno
                                                                          images = CORRESPONDING #( lt_images ) ).

      INSERT ls_message_images INTO TABLE me->_t_message_images.

    ENDIF.

    IF lt_files IS NOT INITIAL.

      DATA(ls_message_files) = VALUE yif_aai_openai~ty_message_files_s( seqno = l_seqno
                                                                        files = CORRESPONDING #( lt_files ) ).

      INSERT ls_message_files INTO TABLE me->_t_message_files.

    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

    DO ( me->_max_tool_calls + 1 ) TIMES.

      DATA(l_tool_calls) = sy-index.

      IF me->_o_persistence IS BOUND AND
         me->_o_persistence->is_chat_blocked( ).
        RAISE EVENT on_chat_is_blocked.
        EXIT.
      ENDIF.

      IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_openai_generate_endpoint ).

        FREE me->_s_openai_generate_response.

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

*       Uncomment these lines to write the JSON in a file on the server for analysis of its content and structure.
        IF me->_o_persistence IS BOUND.
          NEW ycl_aai_log( i_chat_id = me->_o_persistence->m_id )->write_json_on_log_file( l_json ).
        ENDIF.

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

          IF e_response IS NOT INITIAL.

            IF e_t_response IS REQUESTED.
              APPEND INITIAL LINE TO e_t_response ASSIGNING FIELD-SYMBOL(<l_response>).
              <l_response> = e_response.
            ENDIF.

            APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'assistant'
                                content = e_response
                                type = 'message' ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

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
            e_data = me->_s_openai_generate_response
        ).

        IF me->_s_openai_generate_response IS INITIAL.

          MESSAGE e020(yaai) INTO e_response.

          APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = 'assistant'
                              content = e_response
                              type = 'message' ).

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

        DATA(l_function_call) = abap_false.

        LOOP AT _s_openai_generate_response-output ASSIGNING FIELD-SYMBOL(<ls_output>).

          IF <ls_output>-type <> 'function_call'.
            CONTINUE.
          ENDIF.

          l_function_call = abap_true.

          APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( type = 'function_call'
                              arguments = <ls_output>-arguments
                              call_id = <ls_output>-call_id
                              name = <ls_output>-name ).

          IF me->_o_persistence IS BOUND.

            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_tokens = _s_openai_generate_response-usage-total_tokens
                                                 i_async_task_id = i_async_task_id
                                                 i_model = CONV #( me->_model ) ).

            CLEAR _s_openai_generate_response-usage-total_tokens.

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

          " Tool call approval
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          DATA(l_tool_call_approved) = abap_true.

          IF me->_o_persistence IS BOUND AND
             me->mo_function_calling IS BOUND.

            DATA(lo_fc_approvals) = NEW ycl_aai_func_call_approvals( ).

            lo_fc_approvals->check_tool_call_approval(
              EXPORTING
                i_tool_name     = to_upper( condense( <ls_output>-name ) )
                i_o_persistence = me->_o_persistence
                i_t_tools       = CORRESPONDING #( me->mo_function_calling->mt_methods )
              IMPORTING
                e_approved      = l_tool_call_approved
                e_tool_response = DATA(l_tool_call_approval_response)
            ).

            IF l_tool_call_approved = abap_false.
              l_tool_response = l_tool_call_approval_response.
            ENDIF.

          ENDIF.
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          IF l_tool_call_approved = abap_true.

            me->mo_function_calling->call_tool(
              EXPORTING
                i_tool_name   = to_upper( <ls_output>-name )
                i_json        = <l_data>
              IMPORTING
                e_t_files     = DATA(lt_tool_response_files)
              RECEIVING
                r_response    = l_tool_response
            ).

            IF lo_fc_approvals IS BOUND.

              lo_fc_approvals->set_approval_as_used(
                EXPORTING
                  i_tool_name     = to_upper( condense( <ls_output>-name ) )
                  i_o_persistence = me->_o_persistence
              ).

            ENDIF.

          ENDIF.

          APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( type = 'function_call_output'
                              call_id = <ls_output>-call_id
                              output = l_tool_response ).

          IF me->_o_persistence IS BOUND.

            me->_o_persistence->persist_message(
              EXPORTING
                i_data = <ls_msg>
                i_async_task_id = i_async_task_id
                i_model = CONV #( me->_model )
              IMPORTING
                e_seqno = l_seqno
            ).

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

                ls_message_images = VALUE yif_aai_openai~ty_message_images_s( seqno = l_seqno
                                                                              images = CORRESPONDING #( lt_tool_images ) ).

                INSERT ls_message_images INTO TABLE me->_t_message_images.

              ENDIF.

              IF lt_tool_files IS NOT INITIAL.

                ls_message_files = VALUE yif_aai_openai~ty_message_files_s( seqno = l_seqno
                                                                            files = CORRESPONDING #( lt_tool_files ) ).

                INSERT ls_message_files INTO TABLE me->_t_message_files.

              ENDIF.

              FREE: lt_tool_response_files,
                    lt_tool_images,
                    lt_tool_files.

            ENDIF.
            """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          ENDIF.

          CLEAR l_tool_response.

          FREE: lt_tool_images,
                lt_tool_files.

        ENDLOOP.

        IF l_function_call = abap_true.

          IF l_tool_calls >= me->_max_tool_calls.

            APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = 'user'
                                type = 'message' ).

            "The maximum number of tool calls allowed has been reached.
            MESSAGE ID 'YAAI' TYPE 'S' NUMBER '017' INTO <ls_msg>-content.

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).
            ENDIF.

          ENDIF.

          CONTINUE.

        ENDIF.

        IF _s_openai_generate_response-error IS NOT INITIAL.

          e_response = |{ _s_openai_generate_response-error-code }: { _s_openai_generate_response-error-message }|.

          APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = 'assistant'
                              content = e_response
                              type = 'message' ).

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

        LOOP AT _s_openai_generate_response-output ASSIGNING <ls_output>.

          IF <ls_output>-type <> 'message' OR <ls_output>-role <> 'assistant'.
            CONTINUE.
          ENDIF.

          LOOP AT <ls_output>-content ASSIGNING FIELD-SYMBOL(<ls_content>).

            IF <ls_content>-type <> 'output_text'.
              CONTINUE.
            ENDIF.

            APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

            <ls_content>-text = lo_aai_util->replace_unicode_escape_seq( <ls_content>-text ).

            <ls_msg> = VALUE #( role = <ls_output>-role
                                content = <ls_content>-text
                                type = <ls_output>-type ).

            IF me->_o_persistence IS BOUND.
              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_tokens = _s_openai_generate_response-usage-total_tokens
                                                   i_async_task_id = i_async_task_id
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

        APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = 'assistant'
                            content = e_response
                            type = 'message' ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

        RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

        IF e_t_response IS REQUESTED.
          APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
          <l_response> = e_response.
        ENDIF.

        EXIT.

      ENDIF.

    ENDDO.

    IF e_response IS INITIAL.

      e_response = 'We''re having a little trouble getting a complete answer to your question at the moment.'.

      APPEND INITIAL LINE TO me->_t_messages ASSIGNING <ls_msg>.

      <ls_msg> = VALUE #( role = 'assistant'
                          content = e_response
                          type = 'message' ).

      IF me->_o_persistence IS BOUND.
        me->_o_persistence->persist_message( i_data = <ls_msg>
                                             i_async_task_id = i_async_task_id
                                             i_model = CONV #( me->_model ) ).
      ENDIF.

    ENDIF.

    IF e_t_response IS REQUESTED AND e_response IS NOT INITIAL.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_openai~get_conversation.

    DATA l_json TYPE string.

    CLEAR r_conversation.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->_t_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      CLEAR l_json.

      DATA(l_index) = sy-tabix.

      CASE to_lower( <ls_message>-type ).

        WHEN 'message'.

          DATA(ls_message) = CORRESPONDING yif_aai_openai~ty_message_content_json_s( <ls_message> ).

          CASE ls_message-role.

            WHEN yif_aai_openai=>mc_developer OR yif_aai_openai=>mc_user.

              IF ls_message-role = yif_aai_openai=>mc_user.

                READ TABLE me->_t_messages_db ASSIGNING FIELD-SYMBOL(<ls_message_db>) INDEX l_index.

                " If the message is already persisted use SEQNO
                IF sy-subrc = 0.
                  l_index = <ls_message_db>-seqno.
                ENDIF.

                READ TABLE me->_t_message_images ASSIGNING FIELD-SYMBOL(<ls_message_images>)
                  WITH KEY seqno = l_index.

                IF sy-subrc = 0.

                  DATA(ls_input_text) = VALUE yif_aai_openai=>ty_input_text_s( type = yif_aai_openai=>mc_input_text
                                                                               text = ls_message-content ).

                  l_json = lo_aai_util->serialize( ls_input_text ).

                  LOOP AT <ls_message_images>-images ASSIGNING FIELD-SYMBOL(<ls_image>).

                    DATA(ls_input_image) = VALUE yif_aai_openai~ty_input_image_s( image_url = <ls_image>-image_url
                                                                                  type = yif_aai_openai~mc_input_image
                                                                                  detail = 'auto' ).

                    DATA(l_json_image) = lo_aai_util->serialize( ls_input_image ).

                    l_json = |{ l_json },{ l_json_image }|.

                  ENDLOOP.

                ENDIF.

                READ TABLE me->_t_message_files ASSIGNING FIELD-SYMBOL(<ls_message_files>)
                  WITH KEY seqno = l_index.

                IF sy-subrc = 0.

                  IF ls_input_text IS INITIAL.

                    ls_input_text = VALUE yif_aai_openai=>ty_input_text_s( type = yif_aai_openai=>mc_input_text
                                                                           text = ls_message-content ).

                    l_json = lo_aai_util->serialize( ls_input_text ).

                  ENDIF.

                  LOOP AT <ls_message_files>-files ASSIGNING FIELD-SYMBOL(<ls_file>).

                    DATA(ls_input_file) = VALUE yif_aai_openai~ty_input_file_s( type = yif_aai_openai~mc_input_file
                                                                                filename = <ls_file>-filename
                                                                                file_data = <ls_file>-file_data ).

                    DATA(l_json_file) = lo_aai_util->serialize( ls_input_file ).

                    l_json = |{ l_json },{ l_json_file }|.

                  ENDLOOP.

                ENDIF.

              ENDIF.

              IF l_json IS NOT INITIAL.
                ls_message-content = |[{ l_json }]|.
              ELSE.
                ls_message-content = lo_aai_util->serialize( CONV string( ls_message-content ) ).
              ENDIF.

              l_json = lo_aai_util->serialize( ls_message ).

              CLEAR: ls_input_text,
                     l_json_file,
                     l_json_image.

            WHEN yif_aai_openai=>mc_assistant.

              DATA(ls_output_text) = VALUE yif_aai_openai=>ty_input_text_s( type = yif_aai_openai=>mc_output_text
                                                                            text = ls_message-content ).

              l_json = lo_aai_util->serialize( ls_output_text ).

              ls_message-content = |[{ l_json }]|.

              l_json = lo_aai_util->serialize( ls_message ).

            WHEN OTHERS.

              l_json = lo_aai_util->serialize( ls_message ).

          ENDCASE.

        WHEN 'function_call'.

          DATA(ls_function_call) = CORRESPONDING yif_aai_openai~ty_function_call_s( <ls_message> ).

          l_json = lo_aai_util->serialize( ls_function_call ).

        WHEN 'function_call_output'.

          DATA(ls_function_call_output) = CORRESPONDING yif_aai_openai~ty_function_call_output_s( <ls_message> ).

          READ TABLE me->_t_messages_db ASSIGNING <ls_message_db> INDEX l_index.

          " If the message is already persisted use SEQNO
          IF sy-subrc = 0.
            l_index = <ls_message_db>-seqno.
          ENDIF.

          READ TABLE me->_t_message_images ASSIGNING <ls_message_images>
            WITH KEY seqno = l_index.

          IF sy-subrc = 0.

            READ TABLE me->_t_message_images ASSIGNING <ls_message_images>
              WITH KEY seqno = l_index.

            IF sy-subrc = 0.

              ls_input_text = VALUE yif_aai_openai=>ty_input_text_s( type = yif_aai_openai=>mc_input_text
                                                                     text = ls_function_call_output-output ).

              l_json = lo_aai_util->serialize( ls_input_text ).

              LOOP AT <ls_message_images>-images ASSIGNING <ls_image>.

                ls_input_image = VALUE yif_aai_openai~ty_input_image_s( image_url = <ls_image>-image_url
                                                                        type = yif_aai_openai~mc_input_image
                                                                        detail = 'auto' ).

                l_json_image = lo_aai_util->serialize( ls_input_image ).

                l_json = |{ l_json },{ l_json_image }|.

              ENDLOOP.

            ENDIF.

            READ TABLE me->_t_message_files ASSIGNING <ls_message_files>
              WITH KEY seqno = l_index.

            IF sy-subrc = 0.

              IF ls_input_text IS INITIAL.

                ls_input_text = VALUE yif_aai_openai=>ty_input_text_s( type = yif_aai_openai=>mc_input_text
                                                                       text = ls_message-content ).

                l_json = lo_aai_util->serialize( ls_input_text ).

              ENDIF.

              LOOP AT <ls_message_files>-files ASSIGNING <ls_file>.

                ls_input_file = VALUE yif_aai_openai~ty_input_file_s( type = yif_aai_openai~mc_input_file
                                                                      filename = <ls_file>-filename
                                                                      file_data = <ls_file>-file_data ).

                l_json_file = lo_aai_util->serialize( ls_input_file ).

                l_json = |{ l_json },{ l_json_file }|.

              ENDLOOP.

            ENDIF.

          ENDIF.

          IF l_json IS NOT INITIAL.

            DATA(ls_function_call_output_f) = CORRESPONDING yif_aai_openai~ty_function_call_output_f_s( ls_function_call_output ).

            ls_function_call_output_f-output = |[{ l_json }]|.

            l_json = lo_aai_util->serialize( ls_function_call_output_f ).

          ELSE.

            l_json = lo_aai_util->serialize( ls_function_call_output ).

          ENDIF.

          CLEAR: ls_input_text,
                 l_json_file,
                 l_json_image.

      ENDCASE.

      IF r_conversation IS INITIAL.
        r_conversation = l_json.
      ELSE.
        r_conversation = |{ r_conversation },{ l_json }|.
      ENDIF.

    ENDLOOP.

    r_conversation = |[{ r_conversation }]|.

  ENDMETHOD.


  METHOD yif_aai_openai~get_conversation_chat_comp.

    DATA l_json TYPE string.

    CLEAR r_conversation.

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    LOOP AT me->_t_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

      CLEAR l_json.

      DATA(l_index) = sy-tabix.

      CASE to_lower( <ls_message>-type ).

        WHEN 'message'.

          DATA(ls_message) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_nt_s( <ls_message> ).

          IF ls_message-role = yif_aai_openai=>mc_user AND
             ( me->_t_message_images[] IS NOT INITIAL OR me->_t_message_files[] IS NOT INITIAL ).

            READ TABLE me->_t_messages_db ASSIGNING FIELD-SYMBOL(<ls_message_db>) INDEX l_index.

            " If the message is already persisted use SEQNO
            IF sy-subrc = 0.
              l_index = <ls_message_db>-seqno.
            ENDIF.

            READ TABLE me->_t_message_images ASSIGNING FIELD-SYMBOL(<ls_message_images>)
              WITH KEY seqno = l_index.

            IF sy-subrc = 0.

              DATA(ls_input_text) = VALUE yif_aai_openai~ty_input_text_s( type = 'text' text = <ls_message>-content ).

              l_json = lo_aai_util->serialize( ls_input_text ).

              LOOP AT <ls_message_images>-images ASSIGNING FIELD-SYMBOL(<ls_image>).

                DATA(ls_image_url) = VALUE yif_aai_openai~ty_image_url_chat_compl_s( url = <ls_image>-image_url
                                                                                     detail = 'auto' ).

                DATA(ls_content_image) = VALUE yif_aai_openai~ty_content_image_chat_compl_s( type = 'image_url'
                                                                                             image_url = ls_image_url ).

                DATA(l_json_content_image) = lo_aai_util->serialize( ls_content_image ).

                l_json = |{ l_json },{ l_json_content_image }|.

              ENDLOOP.

            ENDIF.

            READ TABLE me->_t_message_files ASSIGNING FIELD-SYMBOL(<ls_message_files>)
              WITH KEY seqno = l_index.

            IF sy-subrc = 0.

              IF ls_input_text IS INITIAL.

                ls_input_text = VALUE yif_aai_openai~ty_input_text_s( type = 'text' text = <ls_message>-content ).

                l_json = lo_aai_util->serialize( ls_input_text ).

              ENDIF.

              LOOP AT <ls_message_files>-files ASSIGNING FIELD-SYMBOL(<ls_file>).

                DATA(ls_content_file) = VALUE yif_aai_openai~ty_content_file_chat_compl_s(
                  type = 'file'
                  file = VALUE yif_aai_openai~ty_file_s( filename = <ls_file>-filename
                                                         file_data = <ls_file>-file_data ) ).

                DATA(l_json_content_file) = lo_aai_util->serialize( ls_content_file ).

                l_json = |{ l_json },{ l_json_content_file }|.

              ENDLOOP.

            ENDIF.

          ENDIF.

          IF l_json IS NOT INITIAL.

            DATA(ls_message_content_json) = VALUE yif_aai_openai~ty_type_message_chat_comp_cj_s( role = ls_message-role content = |[{ l_json }]| ).

            l_json = lo_aai_util->serialize( ls_message_content_json ).

          ELSE.
            l_json = lo_aai_util->serialize( ls_message ).
          ENDIF.

          CLEAR: ls_input_text,
                 ls_content_image,
                 ls_content_file.

        WHEN 'function_call'.

          DATA(ls_function_call) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_tc_s( <ls_message> ).

          ls_function_call-tool_calls = VALUE #( ( id = <ls_message>-call_id
                                                   type = 'function'
                                                   function = VALUE #( name = <ls_message>-name
                                                                       arguments = <ls_message>-arguments ) ) ).

          l_json = lo_aai_util->serialize( ls_function_call ).

        WHEN 'function_call_output'.

          DATA(ls_function_call_output) = CORRESPONDING yif_aai_openai~ty_type_message_chat_comp_tr_s( <ls_message> ).

          ls_function_call_output-role = 'tool'.
          ls_function_call_output-content = <ls_message>-output.
          ls_function_call_output-tool_call_id = <ls_message>-call_id.

          READ TABLE me->_t_messages_db ASSIGNING <ls_message_db> INDEX l_index.

          " If the message is already persisted use SEQNO
          IF sy-subrc = 0.
            l_index = <ls_message_db>-seqno.
          ENDIF.

          READ TABLE me->_t_message_images ASSIGNING <ls_message_images>
            WITH KEY seqno = l_index.

          IF sy-subrc = 0.

            ls_input_text = VALUE yif_aai_openai~ty_input_text_s( type = 'text' text = ls_function_call_output-content ).

            l_json = lo_aai_util->serialize( ls_input_text ).

            LOOP AT <ls_message_images>-images ASSIGNING <ls_image>.

              ls_image_url = VALUE yif_aai_openai~ty_image_url_chat_compl_s( url = <ls_image>-image_url
                                                                             detail = 'auto' ).

              ls_content_image = VALUE yif_aai_openai~ty_content_image_chat_compl_s( type = 'image_url'
                                                                                     image_url = ls_image_url ).

              l_json_content_image = lo_aai_util->serialize( ls_content_image ).

              l_json = |{ l_json },{ l_json_content_image }|.

            ENDLOOP.

          ENDIF.

          READ TABLE me->_t_message_files ASSIGNING <ls_message_files>
            WITH KEY seqno = l_index.

          IF sy-subrc = 0.

            IF ls_input_text IS INITIAL.

              CLEAR l_json.

              ls_input_text = VALUE yif_aai_openai~ty_input_text_s( type = 'text' text = ls_function_call_output-content ).

              l_json = lo_aai_util->serialize( ls_input_text ).

            ENDIF.

            LOOP AT <ls_message_files>-files ASSIGNING <ls_file>.

              ls_content_file = VALUE yif_aai_openai~ty_content_file_chat_compl_s(
                type = 'file'
                file = VALUE yif_aai_openai~ty_file_s( filename = <ls_file>-filename
                                                       file_data = <ls_file>-file_data ) ).

              l_json_content_file = lo_aai_util->serialize( ls_content_file ).

              l_json = |{ l_json },{ l_json_content_file }|.

            ENDLOOP.

          ENDIF.

          IF l_json IS NOT INITIAL.

            DATA(ls_function_call_output_j) = CORRESPONDING yif_aai_openai~ty_type_msg_chat_comp_trj_s( ls_function_call_output ).

            ls_function_call_output_j-content = |[{ l_json }]|.

            l_json = lo_aai_util->serialize( ls_function_call_output_j ).

          ELSE.
            l_json = lo_aai_util->serialize( ls_function_call_output ).
          ENDIF.

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

    e_t_history = me->_t_messages.

  ENDMETHOD.


  METHOD yif_aai_openai~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.


  METHOD yif_aai_openai~set_endpoint.

    me->m_endpoint = i_endpoint.

  ENDMETHOD.


  METHOD yif_aai_openai~set_history.

    me->_t_messages = i_t_history.

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

          APPEND VALUE #( image_url = |data:{ condense( to_lower( <ls_file>-file_type ) ) };base64,{ <ls_file>-content }| ) TO e_t_images.

        WHEN OTHERS.

          APPEND VALUE #( filename = <ls_file>-filename
                          file_data = |data:{ condense( to_lower( <ls_file>-file_type ) ) };base64,{ <ls_file>-content }| ) TO e_t_files.

      ENDCASE.

    ENDLOOP.

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
