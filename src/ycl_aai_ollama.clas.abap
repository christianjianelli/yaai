CLASS ycl_aai_ollama DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES yif_aai_ollama.
    INTERFACES yif_aai_chat.

    ALIASES on_message_send FOR yif_aai_chat~on_message_send.
    ALIASES on_response_received FOR yif_aai_chat~on_response_received.
    ALIASES on_message_failed FOR yif_aai_chat~on_message_failed.
    ALIASES on_chat_is_blocked FOR yif_aai_chat~on_chat_is_blocked.

    ALIASES set_model FOR yif_aai_ollama~set_model.
    ALIASES set_context_length FOR yif_aai_ollama~set_context_length.
    ALIASES set_temperature FOR yif_aai_ollama~set_temperature.
    ALIASES set_think FOR yif_aai_ollama~set_think.
    ALIASES set_keep_alive FOR yif_aai_ollama~set_keep_alive.
    ALIASES set_system_instructions FOR yif_aai_ollama~set_system_instructions.
    ALIASES set_connection FOR yif_aai_ollama~set_connection.
    ALIASES bind_tools FOR yif_aai_ollama~bind_tools.
    ALIASES chat FOR yif_aai_ollama~chat.
    ALIASES generate FOR yif_aai_ollama~generate.
    ALIASES embed FOR yif_aai_ollama~embed.
    ALIASES get_chat_messages FOR yif_aai_ollama~get_chat_messages.

    ALIASES mo_function_calling FOR yif_aai_ollama~mo_function_calling.
    ALIASES mo_agent FOR yif_aai_ollama~mo_agent.

    CLASS-DATA m_ref TYPE REF TO ycl_aai_ollama READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING
                i_model      TYPE csequence OPTIONAL
      RETURNING VALUE(r_ref) TYPE REF TO ycl_aai_ollama.

    METHODS constructor
      IMPORTING
        i_model         TYPE csequence OPTIONAL
        i_o_prompt      TYPE REF TO yif_aai_prompt OPTIONAL
        i_o_connection  TYPE REF TO yif_aai_conn OPTIONAL
        i_o_persistence TYPE REF TO yif_aai_db OPTIONAL
        i_o_agent       TYPE REF TO yif_aai_agent OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _o_connection  TYPE REF TO yif_aai_conn,
          _o_persistence TYPE REF TO yif_aai_db,
          _o_log         TYPE REF TO ycl_aai_log.

    DATA: _t_chat_messages          TYPE yif_aai_ollama~ty_chat_messages_t.

    DATA: _model                      TYPE string,
          _temperature                TYPE p LENGTH 2 DECIMALS 1,
          _num_ctx                    TYPE i,
          _max_tool_calls             TYPE i,
          _think                      TYPE abap_bool,
          _keep_alive                 TYPE string,
          _reasoning_effort           TYPE string,
          _system_instructions        TYPE string,
          _ollama_chat_response_s     TYPE yif_aai_ollama~ty_ollama_chat_response_s,
          _ollama_generate_response_s TYPE yif_aai_ollama~ty_ollama_generate_response_s,
          _t_messages_db              TYPE yif_aai_db=>ty_messages_t,
          _t_message_images           TYPE yif_aai_ollama~ty_message_images_t.

    METHODS _load_agent_settings.

    METHODS _get_files
      IMPORTING
        i_t_files     TYPE ytt_aai_files
      EXPORTING
        e_t_images    TYPE yif_aai_ollama~ty_images_t
        e_t_images_db TYPE yif_aai_ollama~ty_message_images_t.

    METHODS _log
      IMPORTING
        i_s_msg TYPE bapiret2.

ENDCLASS.



CLASS ycl_aai_ollama IMPLEMENTATION.


  METHOD constructor.

    IF i_model IS NOT INITIAL.
      me->_model = i_model.
    ELSE.

      SELECT model
        FROM yaai_model
        WHERE id = @yif_aai_const=>c_ollama
          AND default_model = @abap_true
         INTO @me->_model
         UP TO 1 ROWS.                                  "#EC CI_NOORDER
      ENDSELECT.

      IF sy-subrc <> 0.

        SELECT model
          FROM yaai_model
          WHERE id = @yif_aai_const=>c_ollama
           INTO @me->_model
           UP TO 1 ROWS.                                "#EC CI_NOORDER
        ENDSELECT.

      ENDIF.

    ENDIF.

    me->_temperature = 1.

    me->_reasoning_effort = yif_aai_ollama~mc_reasoning_effort_low.

    me->_think = abap_false.

    me->_keep_alive = '10m'.

    me->_num_ctx = 4096.

    me->_max_tool_calls = 10.

    IF i_o_connection IS SUPPLIED.
      me->_o_connection = i_o_connection.
    ENDIF.

    IF i_o_persistence IS SUPPLIED.

      me->_o_persistence = i_o_persistence.

      me->_o_persistence->get_chat(
        IMPORTING
          e_t_messages = me->_t_messages_db
          e_t_msg_data = me->_t_chat_messages
      ).

      " Images
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
          ).

          IF lt_images IS NOT INITIAL.

            DATA(ls_message_images) = VALUE yif_aai_ollama~ty_message_images_s( seqno = <ls_message_db>-seqno
                                                                                images = CORRESPONDING #( lt_images ) ).

            INSERT ls_message_images INTO TABLE me->_t_message_images.

          ENDIF.

          FREE lt_images.

        ENDIF.

      ENDLOOP.

    ENDIF.

    "If an Agent is passed then its settings overwrite any other previous setting
    IF i_o_agent IS BOUND.

      me->mo_agent = i_o_agent.

      me->_load_agent_settings( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_chat_messages.

    rt_messages = me->_t_chat_messages.

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


  METHOD yif_aai_ollama~bind_tools.

    me->mo_function_calling = i_o_function_calling.

    IF i_max_tool_calls IS SUPPLIED.
      me->_max_tool_calls = i_max_tool_calls.
    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_ollama~chat.

    FIELD-SYMBOLS <l_data> TYPE string.

    DATA lr_data TYPE REF TO data.

    DATA: l_tools   TYPE string VALUE '[]',
          l_message TYPE string,
          l_prompt  TYPE string.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.

      me->_log( i_s_msg = VALUE #( number = '018' message_v1 = yif_aai_const=>c_ollama ) ).

      MESSAGE ID 'YAAI' TYPE 'E' NUMBER '018' WITH yif_aai_const=>c_ollama INTO DATA(l_error_018).

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

      FREE me->_t_chat_messages.

    ENDIF.

    IF me->_t_chat_messages IS INITIAL.

      IF me->_system_instructions IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

        <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_system
                            content = me->_system_instructions ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_system_instructions( i_data = <ls_msg> ).
        ENDIF.

      ENDIF.

      IF i_greeting IS NOT INITIAL.

        APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_assistant content = i_greeting ).

        IF me->_o_persistence IS BOUND.
          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

      ENDIF.

    ELSE.

      IF me->_system_instructions IS NOT INITIAL.

        READ TABLE me->_t_chat_messages TRANSPORTING NO FIELDS
          WITH KEY role = yif_aai_ollama=>mc_system.

        IF sy-subrc <> 0.

          INSERT VALUE #( role = yif_aai_ollama=>mc_system
                          content = me->_system_instructions ) INTO me->_t_chat_messages INDEX 1.

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_system_instructions( i_data = me->_t_chat_messages[ 1 ] ).
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

    APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

    <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_user content = i_message ).

    IF l_prompt IS NOT INITIAL.

      DATA(ls_prompt) = <ls_msg>.

      ls_prompt-content = l_prompt.

    ENDIF.

    DATA(l_seqno) = lines( me->_t_chat_messages ).

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

    " Images
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF i_t_files IS NOT INITIAL AND me->_o_persistence IS BOUND.

      me->_o_persistence->persist_files(
        EXPORTING
          i_seqno   = l_seqno
          i_t_files = i_t_files
      ).

      me->_get_files(
        EXPORTING
          i_t_files  = i_t_files
        IMPORTING
          e_t_images = DATA(lt_images)
      ).

    ENDIF.

    IF lt_images IS NOT INITIAL.

      DATA(ls_message_images) = VALUE yif_aai_ollama~ty_message_images_s( seqno = l_seqno
                                                                          images = CORRESPONDING #( lt_images ) ).

      INSERT ls_message_images INTO TABLE me->_t_message_images.

    ENDIF.

    LOOP AT me->_t_chat_messages ASSIGNING <ls_msg>.

      DATA(l_index) = sy-tabix.

      CASE <ls_msg>-role.

        WHEN yif_aai_ollama=>mc_user OR yif_aai_ollama=>mc_tool.

          READ TABLE me->_t_messages_db ASSIGNING FIELD-SYMBOL(<ls_message_db>) INDEX l_index.

          " If the message is already persisted use SEQNO
          IF sy-subrc = 0.
            l_index = <ls_message_db>-seqno.
          ENDIF.

          READ TABLE me->_t_message_images ASSIGNING FIELD-SYMBOL(<ls_message_images>)
            WITH KEY seqno = l_index.

          IF sy-subrc = 0.

            FREE <ls_msg>-images.

            LOOP AT <ls_message_images>-images ASSIGNING FIELD-SYMBOL(<ls_image>).

              APPEND <ls_image>-image TO <ls_msg>-images.

            ENDLOOP.

          ENDIF.

      ENDCASE.

    ENDLOOP.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    DATA(lo_aai_util) = NEW ycl_aai_util( ).

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).
    ENDIF.

    IF me->mo_agent IS BOUND AND me->mo_function_calling IS NOT BOUND.
      me->mo_function_calling = NEW ycl_aai_func_call_ollama( me->mo_agent ).
    ENDIF.

    DO ( me->_max_tool_calls + 1 ) TIMES.

      DATA(l_tool_calls) = sy-index.

      IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_ollama_chat_endpoint ).

        FREE me->_ollama_chat_response_s.

        IF me->mo_function_calling IS BOUND.

          me->mo_function_calling->get_tools(
            IMPORTING
              e_tools = l_tools
          ).

        ENDIF.

        DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_ollama~ty_ollama_chat_request_s( model = me->_model
                                                                                                       options = VALUE #( temperature = me->_temperature
                                                                                                                          num_ctx = me->_num_ctx
                                                                                                                          think = me->_reasoning_effort )
                                                                                                       messages = me->_t_chat_messages
                                                                                                       think = me->_think
                                                                                                       keep_alive = me->_keep_alive
                                                                                                       tools = l_tools ) ).

        me->_o_connection->set_body( l_json ).

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

          <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_assistant
                              content = e_response ).

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
            i_json = l_json
          IMPORTING
            e_data = me->_ollama_chat_response_s
        ).

        IF me->_ollama_chat_response_s IS INITIAL.

          MESSAGE e020(yaai) INTO e_response.

          APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_assistant
                              content = e_response ).

          IF me->_o_persistence IS BOUND.
            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_async_task_id = i_async_task_id
                                                 i_model = CONV #( me->_model ) ).
          ENDIF.

          RAISE EVENT on_message_failed
            EXPORTING
              error_text = e_response.

        ENDIF.

        IF me->_ollama_chat_response_s-message-tool_calls[] IS NOT INITIAL AND me->mo_function_calling IS BOUND.

          APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

          <ls_msg> = me->_ollama_chat_response_s-message.

          IF me->_o_persistence IS BOUND.

            me->_o_persistence->persist_message( i_data = <ls_msg>
                                                 i_tokens = me->_ollama_chat_response_s-eval_count + me->_ollama_chat_response_s-prompt_eval_count
                                                 i_async_task_id = i_async_task_id
                                                 i_model = CONV #( me->_model ) ).

          ENDIF.

          LOOP AT me->_ollama_chat_response_s-message-tool_calls ASSIGNING FIELD-SYMBOL(<ls_tool>).

            " Tool call approval
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
            DATA(l_tool_call_approved) = abap_true.

            IF me->_o_persistence IS BOUND AND
               me->mo_function_calling IS BOUND.

              DATA(lo_fc_approvals) = NEW ycl_aai_func_call_approvals( ).

              lo_fc_approvals->check_tool_call_approval(
                EXPORTING
                  i_tool_name     = to_upper( condense( <ls_tool>-function-name ) )
                  i_o_persistence = me->_o_persistence
                  i_t_tools       = CORRESPONDING #( me->mo_function_calling->mt_methods )
                IMPORTING
                  e_approved      = l_tool_call_approved
                  e_tool_response = DATA(l_tool_call_approval_response)
              ).

            ENDIF.
            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            " For some reason, probably a bug in Ollama API, part of the arguments in the JSON are passed as escaped strings.
            " Because just part of the JSON has to be unescaped it is not possible to use the proper deserialize method.
            " So the workaround is to unescape the JSON using the replaces below.
            " Hopefully the Ollama API bug will be fixed and this workaround can be removed.
            REPLACE ALL OCCURRENCES OF '\"' IN <ls_tool>-function-arguments WITH '"'.
            REPLACE ALL OCCURRENCES OF '"[' IN <ls_tool>-function-arguments WITH '['.
            REPLACE ALL OCCURRENCES OF ']"' IN <ls_tool>-function-arguments WITH ']'.

            IF l_tool_call_approved = abap_true.

              me->mo_function_calling->call_tool(
                EXPORTING
                  i_tool_name   = to_upper( <ls_tool>-function-name )
                  i_json        = <ls_tool>-function-arguments
                IMPORTING
                  e_t_files     = DATA(lt_tool_response_files)
                RECEIVING
                  r_response    = DATA(l_tool_response)
              ).

              IF lo_fc_approvals IS BOUND.

                lo_fc_approvals->set_approval_as_used(
                  EXPORTING
                    i_tool_name     = to_upper( condense( <ls_tool>-function-name ) )
                    i_o_persistence = me->_o_persistence
                ).

              ENDIF.

            ELSE.
              l_tool_response = l_tool_call_approval_response.
            ENDIF.

            APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_tool
                                tool_name = <ls_tool>-function-name
                                content = l_tool_response ).

            IF me->_o_persistence IS BOUND.

              me->_o_persistence->persist_message( i_data = <ls_msg>
                                                   i_async_task_id = i_async_task_id
                                                   i_model = CONV #( me->_model ) ).

              " Images returned from tool call
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
                    e_t_images    = DATA(lt_tool_images)
                ).

                IF lt_tool_images IS NOT INITIAL.

                  ls_message_images = VALUE yif_aai_ollama~ty_message_images_s( seqno = l_seqno
                                                                                images = CORRESPONDING #( lt_tool_images ) ).

                  INSERT ls_message_images INTO TABLE me->_t_message_images.

                  LOOP AT lt_tool_images ASSIGNING <ls_image>.

                    APPEND <ls_image>-image TO <ls_msg>-images.

                  ENDLOOP.

                ENDIF.

                FREE: lt_tool_response_files,
                      lt_tool_images.

              ENDIF.
              """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

            ENDIF.

          ENDLOOP.

          IF l_tool_calls >= me->_max_tool_calls.

            APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

            <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_user ).

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

        IF me->_ollama_chat_response_s-error IS NOT INITIAL.

          e_response = me->_ollama_chat_response_s-error.

          IF e_t_response IS REQUESTED.
            APPEND INITIAL LINE TO e_t_response ASSIGNING <l_response>.
            <l_response> = e_response.
          ENDIF.

          APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

          <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_assistant
                              content = e_response ).

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

        me->_ollama_chat_response_s-message-content = lo_aai_util->replace_unicode_escape_seq( me->_ollama_chat_response_s-message-content ).

        APPEND me->_ollama_chat_response_s-message TO me->_t_chat_messages.

        APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role =  me->_ollama_chat_response_s-message-role
                            content = me->_ollama_chat_response_s-message-content ).

        IF me->_o_persistence IS BOUND.

          me->_o_persistence->persist_message( i_data = <ls_msg>
                                               i_tokens = me->_ollama_chat_response_s-eval_count + me->_ollama_chat_response_s-prompt_eval_count
                                               i_async_task_id = i_async_task_id
                                               i_model = CONV #( me->_model ) ).
        ENDIF.

        e_response = me->_ollama_chat_response_s-message-content.

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

        APPEND INITIAL LINE TO me->_t_chat_messages ASSIGNING <ls_msg>.

        <ls_msg> = VALUE #( role = yif_aai_ollama=>mc_assistant
                            content = e_response ).

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

    ENDDO.

    IF e_t_response IS REQUESTED AND me->_ollama_chat_response_s-error IS INITIAL.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_ollama~embed.

    CLEAR: e_s_response,
           e_failed.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).
    ENDIF.

    IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_ollama_embed_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_ollama~ty_ollama_embed_request_s( model = me->_model
                                                                                                      input = i_input ) ).

      me->_o_connection->set_body( l_json ).

      FREE l_json.

      me->_o_connection->do_receive(
        IMPORTING
          e_response = l_json
          e_failed   = e_failed
      ).

      IF e_failed = abap_true.

        RETURN.

      ELSE.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = e_s_response
        ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_ollama~generate.

    CLEAR: e_response,
           e_failed.

    FREE e_t_response.

    IF me->_model IS INITIAL.

      me->_log( i_s_msg = VALUE #( number = '018' message_v1 = yif_aai_const=>c_ollama ) ).

      MESSAGE ID 'YAAI' TYPE 'E' NUMBER '018' WITH yif_aai_const=>c_ollama INTO DATA(l_error_018).

      RAISE EVENT on_message_failed
        EXPORTING
          error_text = l_error_018.

      RETURN.

    ENDIF.

    IF me->_o_connection IS NOT BOUND.
      me->_o_connection = NEW ycl_aai_conn( i_api = yif_aai_const=>c_ollama ).
    ENDIF.

    IF me->_o_connection->create_connection( i_endpoint = yif_aai_const=>c_ollama_generate_endpoint ).

      DATA(lo_aai_util) = NEW ycl_aai_util( ).

      DATA(l_json) = lo_aai_util->serialize( i_data = VALUE yif_aai_ollama~ty_ollama_generate_request_s( model = me->_model
                                                                                                         prompt = i_message
                                                                                                         stream = abap_false ) ).

      me->_o_connection->set_body( l_json ).

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

        RETURN.

      ELSE.

        lo_aai_util->deserialize(
          EXPORTING
            i_json = l_json
          IMPORTING
            e_data = me->_ollama_generate_response_s
        ).

        me->_ollama_generate_response_s-response = lo_aai_util->replace_unicode_escape_seq( me->_ollama_generate_response_s-response ).

        e_response = me->_ollama_generate_response_s-response.

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

      RETURN.

    ENDIF.

    IF e_t_response IS REQUESTED.

      SPLIT e_response AT cl_abap_char_utilities=>newline INTO TABLE e_t_response.

    ENDIF.

  ENDMETHOD.


  METHOD yif_aai_ollama~get_history.

    e_t_history = me->_t_chat_messages.

  ENDMETHOD.


  METHOD yif_aai_ollama~set_connection.

    me->_o_connection = i_o_connection.

  ENDMETHOD.


  METHOD yif_aai_ollama~set_context_length.

    me->_num_ctx = i_context_length.

  ENDMETHOD.


  METHOD yif_aai_ollama~set_history.

    me->_t_chat_messages = i_t_history.

  ENDMETHOD.


  METHOD yif_aai_ollama~set_keep_alive.

    me->_keep_alive = i_keep_alive.

  ENDMETHOD.


  METHOD yif_aai_ollama~set_model.

    me->_model = i_model.

  ENDMETHOD.


  METHOD yif_aai_ollama~set_system_instructions.

    me->_system_instructions = i_system_instructions.

  ENDMETHOD.


  METHOD yif_aai_ollama~set_temperature.

    me->_temperature = i_temperature.

  ENDMETHOD.


  METHOD yif_aai_ollama~set_think.

    me->_think = i_think.

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

    FREE: e_t_images.

    LOOP AT i_t_files ASSIGNING FIELD-SYMBOL(<ls_file>).

      CASE condense( to_lower( <ls_file>-file_type ) ).

        WHEN lc_png OR lc_jpeg OR lc_webp OR lc_gif.

          APPEND VALUE #( image = <ls_file>-content ) TO e_t_images.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD _load_agent_settings.

    DATA(ls_model) = me->mo_agent->get_model(
      EXPORTING
        i_api = CONV #( yif_aai_const=>c_ollama )
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

    me->_think = ls_model-think.

    IF ls_model-reasoning IS NOT INITIAL.

      me->_reasoning_effort = ls_model-reasoning.

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
