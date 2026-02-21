INTERFACE yif_aai_async_chat_run
  PUBLIC .

  METHODS run
    IMPORTING
      i_task_id  TYPE yde_aai_async_task_id
      i_chat_id  TYPE yde_aai_chat_id
      i_api_key  TYPE csequence
      i_message  TYPE csequence
      i_context  TYPE csequence OPTIONAL
      i_agent_id TYPE yde_aai_agent_id OPTIONAL
      i_model    TYPE csequence OPTIONAL
      i_log      TYPE abap_bool DEFAULT abap_true.

ENDINTERFACE.
