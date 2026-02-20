INTERFACE yif_aai_agent_db
  PUBLIC.

  TYPES: ty_agent_tools_t  TYPE STANDARD TABLE OF yaai_agent_tool WITH EMPTY KEY,
         ty_agent_docs_t   TYPE STANDARD TABLE OF yaai_agent_rag WITH EMPTY KEY,
         ty_agent_models_t TYPE STANDARD TABLE OF yaai_agent_mdl WITH EMPTY KEY.

  METHODS create
    IMPORTING
      i_s_agent        TYPE yaai_agent
      i_t_agent_tools  TYPE ty_agent_tools_t  OPTIONAL
      i_t_agent_docs   TYPE ty_agent_docs_t   OPTIONAL
      i_t_agent_models TYPE ty_agent_models_t OPTIONAL
    EXPORTING
      e_id             TYPE uuid
      e_error          TYPE string.

  METHODS read
    IMPORTING
      i_agent_id       TYPE yaai_agent-id OPTIONAL
      i_agent_name     TYPE yaai_agent-name OPTIONAL
        PREFERRED PARAMETER i_agent_id
    EXPORTING
      e_s_agent        TYPE yaai_agent
      e_t_agent_tools  TYPE ty_agent_tools_t
      e_t_agent_docs   TYPE ty_agent_docs_t
      e_t_agent_models TYPE ty_agent_models_t
      e_error          TYPE string.

  METHODS update
    IMPORTING
      i_s_agent        TYPE yaai_agent
      i_t_agent_tools  TYPE ty_agent_tools_t OPTIONAL
      i_t_agent_docs   TYPE ty_agent_docs_t   OPTIONAL
      i_t_agent_models TYPE ty_agent_models_t OPTIONAL
    EXPORTING
      e_updated        TYPE abap_bool
      e_error          TYPE string.

  METHODS delete
    IMPORTING
      i_agent_id TYPE yaai_agent-id OPTIONAL
    EXPORTING
      e_deleted  TYPE abap_bool
      e_error    TYPE string.

ENDINTERFACE.
