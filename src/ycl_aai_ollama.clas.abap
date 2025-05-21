CLASS ycl_aai_ollama DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF chat_message,
             role    TYPE string,
             content TYPE string,
           END OF chat_message.

    TYPES: chat_messages_t TYPE STANDARD TABLE OF chat_message WITH NON-UNIQUE key role.

    TYPES: BEGIN OF ollama_chat_request,
             model    TYPE string,
             messages TYPE chat_messages_t,
           END OF ollama_chat_request,

           BEGIN OF ollama_chat_response,
             model   TYPE string,
             message TYPE chat_message,
           END OF ollama_chat_response.

    METHODS constructor
      IMPORTING
        i_model TYPE string.

    METHODS chat
      IMPORTING
        i_message TYPE string.

    METHODS generate.

    METHODS embed.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: _model TYPE string,
          _ollama_api_request  TYPE ollama_chat_request,
          _ollama_api_response TYPE ollama_chat_response,
          _chat_messages       TYPE chat_messages_t.

ENDCLASS.



CLASS ycl_aai_ollama IMPLEMENTATION.

  METHOD constructor.

    me->_model = i_model.

  ENDMETHOD.

  METHOD chat.

  ENDMETHOD.

  METHOD embed.

  ENDMETHOD.

  METHOD generate.

  ENDMETHOD.

ENDCLASS.
