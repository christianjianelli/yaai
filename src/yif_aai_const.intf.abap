INTERFACE yif_aai_const
  PUBLIC.

  " General
  CONSTANTS: c_message_id TYPE bapiret2-id VALUE 'YAAI'.

  " Ollama
  CONSTANTS: c_ollama                   TYPE string VALUE 'OLLAMA',
             c_ollama_base_url_param    TYPE string VALUE 'YAAI_OLLAMA_BASE_URL',
             c_ollama_generate_endpoint TYPE string VALUE '/api/generate',
             c_ollama_chat_endpoint     TYPE string VALUE '/api/chat'.

  " OpenAI
  CONSTANTS: c_openai                   TYPE string VALUE 'OPENAI',
             c_openai_base_url_param    TYPE string VALUE 'YAAI_OPENAI_BASE_URL',
             c_openai_generate_endpoint TYPE string VALUE '/v1/responses'.

ENDINTERFACE.
