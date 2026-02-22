FUNCTION Y_F_AAI_RUN_ASYNC_TASK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_API) TYPE  YDE_AAI_API
*"     VALUE(I_TASK_ID) TYPE  YDE_AAI_ASYNC_TASK_ID
*"     VALUE(I_CHAT_ID) TYPE  YDE_AAI_CHAT_ID
*"     VALUE(I_API_KEY) TYPE  STRING
*"     VALUE(I_MESSAGE) TYPE  STRING
*"     VALUE(I_CONTEXT) TYPE  STRING OPTIONAL
*"     VALUE(I_AGENT_ID) TYPE  YDE_AAI_AGENT_ID OPTIONAL
*"     VALUE(I_MODEL) TYPE  STRING OPTIONAL
*"     VALUE(I_LOG) TYPE  XFELD DEFAULT ABAP_TRUE
*"----------------------------------------------------------------------
  CASE i_api.

    WHEN yif_aai_const=>c_openai.

      NEW ycl_aai_async_chat_openai( )->run(
        EXPORTING
          i_task_id  = i_task_id
          i_chat_id  = i_chat_id
          i_api_key  = i_api_key
          i_message  = i_message
          i_context  = i_context
          i_agent_id = i_agent_id
          i_model    = i_model
          i_log      = abap_true
      ).

    WHEN yif_aai_const=>c_anthropic.

      NEW ycl_aai_async_chat_anthropic( )->run(
        EXPORTING
          i_task_id  = i_task_id
          i_chat_id  = i_chat_id
          i_api_key  = i_api_key
          i_message  = i_message
          i_context  = i_context
          i_agent_id = i_agent_id
          i_model    = i_model
          i_log      = abap_true
      ).

    WHEN yif_aai_const=>c_google.

      NEW ycl_aai_async_chat_google( )->run(
        EXPORTING
          i_task_id  = i_task_id
          i_chat_id  = i_chat_id
          i_api_key  = i_api_key
          i_message  = i_message
          i_context  = i_context
          i_agent_id = i_agent_id
          i_model    = i_model
          i_log      = abap_true
      ).

    WHEN yif_aai_const=>c_mistral.

      NEW ycl_aai_async_chat_mistral( )->run(
        EXPORTING
          i_task_id  = i_task_id
          i_chat_id  = i_chat_id
          i_api_key  = i_api_key
          i_message  = i_message
          i_context  = i_context
          i_agent_id = i_agent_id
          i_model    = i_model
          i_log      = abap_true
      ).

*    WHEN yif_aai_const=>c_ollama.

    WHEN OTHERS.

      DATA(lo_async) = NEW ycl_aai_async( ).

      lo_async->update_status(
        EXPORTING
          i_task_id = i_task_id
          i_status  = yif_aai_async=>mc_task_failed
      ).

      DATA(lo_log) = NEW ycl_aai_log( i_chat_id ).

      lo_log->add( VALUE #( number = '016' type = 'E' message_v1 = i_api ) ).

      RETURN.

  ENDCASE.

ENDFUNCTION.
