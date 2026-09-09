INTERFACE yif_aai_const
  PUBLIC.

  "ABAP AI tools version
  CONSTANTS: c_version TYPE string VALUE '1.2.3'.

  "General
  CONSTANTS: c_message_id          TYPE bapiret2-id VALUE 'YAAI' ##NO_TEXT,
             c_placeholder_pattern TYPE c LENGTH 1 VALUE '%' ##NO_TEXT,
             c_unauthorized        TYPE string VALUE 'UNAUTHORIZED' ##NO_TEXT.

  "Authorization Objects
  CONSTANTS: c_chat_auth_obj_param TYPE tvarvc-name VALUE 'YAAI_CHAT_AUTH_OBJ'.

  "Ollama
  CONSTANTS: c_ollama                   TYPE string VALUE 'OLLAMA' ##NO_TEXT,
             c_ollama_base_url_param    TYPE string VALUE 'YAAI_OLLAMA_BASE_URL' ##NO_TEXT,
             c_ollama_generate_endpoint TYPE string VALUE '/api/generate' ##NO_TEXT,
             c_ollama_chat_endpoint     TYPE string VALUE '/api/chat' ##NO_TEXT,
             c_ollama_embed_endpoint    TYPE string VALUE '/api/embed' ##NO_TEXT.

  "OpenAI
  CONSTANTS: c_openai                      TYPE string VALUE 'OPENAI' ##NO_TEXT,
             c_openai_base_url_param       TYPE string VALUE 'YAAI_OPENAI_BASE_URL' ##NO_TEXT,
             c_openai_generate_endpoint    TYPE string VALUE '/v1/responses' ##NO_TEXT,
             c_openai_completions_endpoint TYPE string VALUE '/v1/chat/completions' ##NO_TEXT,
             c_openai_embed_endpoint       TYPE string VALUE '/v1/embeddings' ##NO_TEXT,
             c_openai_audio_trans_endpoint TYPE string VALUE '/v1/audio/transcriptions' ##NO_TEXT.

  "Google
  CONSTANTS: c_google                TYPE string VALUE 'GOOGLE' ##NO_TEXT,
             c_google_base_url_param TYPE string VALUE 'YAAI_GOOGLE_BASE_URL' ##NO_TEXT.

  "Anthropic
  CONSTANTS: c_anthropic                   TYPE string VALUE 'ANTHROPIC' ##NO_TEXT,
             c_anthropic_base_url_param    TYPE string VALUE 'YAAI_ANTHROPIC_BASE_URL' ##NO_TEXT,
             c_anthropic_messages_endpoint TYPE string VALUE '/v1/messages' ##NO_TEXT.

  "Mistral
  CONSTANTS: c_mistral                   TYPE string VALUE 'MISTRAL' ##NO_TEXT,
             c_mistral_base_url_param    TYPE string VALUE 'YAAI_MISTRAL_BASE_URL' ##NO_TEXT.

  "SAP AI Core
  CONSTANTS: c_sap_ai_core                TYPE string VALUE 'SAP_AI_CORE' ##NO_TEXT,
             c_sap_ai_core_base_url_param TYPE string VALUE 'YAAI_SAP_AI_CORE_BASE_URL' ##NO_TEXT,
             c_sap_ai_core_resource_group TYPE string VALUE 'default' ##NO_TEXT,
             c_sap_ai_core_res_grp_param  TYPE string VALUE 'YAAI_SAP_AI_CORE_RESOURCE_GRP' ##NO_TEXT.

  "Deepseek
  CONSTANTS: c_deepseek                   TYPE string VALUE 'DEEPSEEK' ##NO_TEXT,
             c_deepseek_base_url_param    TYPE string VALUE 'YAAI_DEEPSEEK_BASE_URL' ##NO_TEXT.

  "Moonshot AI
  CONSTANTS: c_moonshot                   TYPE string VALUE 'MOONSHOT' ##NO_TEXT,
             c_moonshot_base_url_param    TYPE string VALUE 'YAAI_MOONSHOT_BASE_URL' ##NO_TEXT.

ENDINTERFACE.
