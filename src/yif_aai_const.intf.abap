INTERFACE yif_aai_const
  PUBLIC.

  "General
  CONSTANTS: c_message_id          TYPE bapiret2-id VALUE 'YAAI',
             c_placeholder_pattern TYPE c LENGTH 1 VALUE '%'.

  "Ollama
  CONSTANTS: c_ollama                   TYPE string VALUE 'OLLAMA',
             c_ollama_base_url_param    TYPE string VALUE 'YAAI_OLLAMA_BASE_URL',
             c_ollama_generate_endpoint TYPE string VALUE '/api/generate',
             c_ollama_chat_endpoint     TYPE string VALUE '/api/chat',
             c_ollama_embed_endpoint    TYPE string VALUE '/api/embed'.

  "OpenAI
  CONSTANTS: c_openai                      TYPE string VALUE 'OPENAI',
             c_openai_base_url_param       TYPE string VALUE 'YAAI_OPENAI_BASE_URL',
             c_openai_generate_endpoint    TYPE string VALUE '/v1/responses',
             c_openai_completions_endpoint TYPE string VALUE '/v1/chat/completions',
             c_openai_embed_endpoint       TYPE string VALUE '/v1/embeddings'.

  "Google
  CONSTANTS: c_google                      TYPE string VALUE 'GOOGLE',
             c_google_base_url_param       TYPE string VALUE 'YAAI_GOOGLE_BASE_URL'.

  "Anthropic
  CONSTANTS: c_anthropic                   TYPE string VALUE 'ANTHROPIC',
             c_anthropic_base_url_param    TYPE string VALUE 'YAAI_ANTHROPIC_BASE_URL',
             c_anthropic_messages_endpoint TYPE string VALUE '/v1/messages'.

  "Mistral
  CONSTANTS: c_mistral                   TYPE string VALUE 'mistral',
             c_mistral_base_url_param    TYPE string VALUE 'YAAI_MISTRAL_BASE_URL',
             c_mistral_messages_endpoint TYPE string VALUE '/v1/chat/completions'.

ENDINTERFACE.
