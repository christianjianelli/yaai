INTERFACE yif_aai_agent
  PUBLIC.

  TYPES: BEGIN OF ty_agent_tool_s,
           class_name  TYPE yaai_agent_tool-class_name,
           method_name TYPE yaai_agent_tool-method_name,
           proxy_class TYPE yaai_agent_tool-proxy_class,
           description TYPE yaai_agent_tool-description,
         END OF ty_agent_tool_s,

         BEGIN OF ty_agent_doc_s,
           rag_id      TYPE yde_aai_rag_id,
           filename    TYPE yde_aai_filename,
           description TYPE yde_aai_description,
           keywords    TYPE yde_aai_keywords,
         END OF ty_agent_doc_s,

         ty_agent_tools_t TYPE STANDARD TABLE OF ty_agent_tool_s WITH DEFAULT KEY,
         ty_agent_docs_t  TYPE STANDARD TABLE OF ty_agent_doc_s WITH DEFAULT KEY.

  DATA: m_agent_id TYPE yaai_agent-id READ-ONLY,
        m_chat_id  TYPE yaai_agent-id READ-ONLY.

  METHODS get_system_instructions
    IMPORTING
              i_agent_id                   TYPE yaai_agent-id OPTIONAL
              i_agent_name                 TYPE yaai_agent-name OPTIONAL
                PREFERRED PARAMETER i_agent_id
    RETURNING VALUE(r_system_instructions) TYPE string.

  METHODS get_tools
    IMPORTING
              i_agent_id             TYPE yaai_agent-id OPTIONAL
              i_chat_id              TYPE yaai_chat-id OPTIONAL
              i_agent_name           TYPE yaai_agent-name OPTIONAL
              i_load_on_demand_tools TYPE abap_bool DEFAULT abap_false
                PREFERRED PARAMETER i_agent_id
    RETURNING VALUE(r_t_agent_tools) TYPE ty_agent_tools_t.

  METHODS get_docs
    IMPORTING
              i_agent_id            TYPE yaai_agent-id OPTIONAL
              i_agent_name          TYPE yaai_agent-name OPTIONAL
                PREFERRED PARAMETER i_agent_id
    RETURNING VALUE(r_t_agent_docs) TYPE ty_agent_docs_t.

  METHODS get_prompt_template
    IMPORTING
              i_agent_id               TYPE yaai_agent-id OPTIONAL
              i_agent_name             TYPE yaai_agent-name OPTIONAL
                PREFERRED PARAMETER i_agent_id
    RETURNING VALUE(r_prompt_template) TYPE string.

  METHODS get_model
    IMPORTING
              i_agent_id       TYPE yaai_agent-id OPTIONAL
              i_agent_name     TYPE yaai_agent-name OPTIONAL
              i_api            TYPE yaai_agent_mdl-api
    RETURNING VALUE(r_s_model) TYPE yaai_agent_mdl.

ENDINTERFACE.
