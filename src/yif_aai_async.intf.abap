INTERFACE yif_aai_async
  PUBLIC.

  TYPES: BEGIN OF ty_task_s,
           id        TYPE yaai_async-id,
           chat_id   TYPE yaai_async-chat_id,
           name      TYPE yaai_async-name,
           status    TYPE yaai_async-status,
           username  TYPE yaai_async-username,
           startdate TYPE yaai_async-startdate,
           starttime TYPE yaai_async-starttime,
         END OF ty_task_s,

         ty_task_t TYPE STANDARD TABLE OF ty_task_s WITH DEFAULT KEY.

  CONSTANTS: mc_task_created   TYPE yaai_async-status VALUE 'Created'   ##NO_TEXT,
             mc_task_running   TYPE yaai_async-status VALUE 'Running'   ##NO_TEXT,
             mc_task_finished  TYPE yaai_async-status VALUE 'Finished'  ##NO_TEXT,
             mc_task_failed    TYPE yaai_async-status VALUE 'Failed'    ##NO_TEXT,
             mc_task_unknown   TYPE yaai_async-status VALUE 'Unknown'   ##NO_TEXT,
             mc_task_cancelled TYPE yaai_async-status VALUE 'Cancelled' ##NO_TEXT.

  METHODS create
    IMPORTING
              i_chat_id        TYPE ty_task_s-chat_id
              i_task_name      TYPE ty_task_s-name
    RETURNING VALUE(r_task_id) TYPE ty_task_s-id.

  METHODS read
    IMPORTING
      i_task_id TYPE ty_task_s-id
    EXPORTING
      e_s_task  TYPE ty_task_s.

  METHODS update
    IMPORTING
              i_s_task         TYPE ty_task_s
    RETURNING VALUE(r_updated) TYPE abap_bool.

  METHODS delete
    IMPORTING
              i_task_id        TYPE ty_task_s-id
    RETURNING VALUE(r_deleted) TYPE abap_bool.

  METHODS run
    IMPORTING
              i_task_id        TYPE ty_task_s-id
              i_o_task         TYPE REF TO object "if_bgmc_op_single_tx_uncontr
    RETURNING VALUE(r_started) TYPE abap_bool.

  METHODS get_status
    IMPORTING
              i_task_id       TYPE ty_task_s-id
    RETURNING VALUE(r_status) TYPE ty_task_s-status.

  METHODS set_monitor
    IMPORTING
              i_task_id    TYPE ty_task_s-id
              i_monitor    TYPE string
    RETURNING VALUE(r_set) TYPE abap_bool.

  METHODS get_monitor
    IMPORTING
              i_task_id        TYPE ty_task_s-id
    RETURNING VALUE(r_monitor) TYPE string.

  METHODS get_process_monitor
    IMPORTING
              i_task_id                 TYPE ty_task_s-id
    RETURNING VALUE(ro_process_monitor) TYPE REF TO object. "if_bgmc_process_monitor.

  METHODS get_process_state
    IMPORTING
              i_task_id      TYPE ty_task_s-id
    RETURNING VALUE(r_state) TYPE string. "if_bgmc_process_monitor=>ty_state.

  METHODS update_status
    IMPORTING
              i_task_id        TYPE ty_task_s-id
              i_status         TYPE ty_task_s-status OPTIONAL
              i_process_state  TYPE string "if_bgmc_process_monitor=>ty_state OPTIONAL
    RETURNING VALUE(r_updated) TYPE abap_bool.

  METHODS get_tasks_by_chat_id
    IMPORTING
              i_chat_id        TYPE ty_task_s-chat_id
    RETURNING VALUE(r_t_tasks) TYPE ty_task_t.

  METHODS get_response
    IMPORTING
              i_task_id         TYPE ty_task_s-id
    RETURNING VALUE(r_response) TYPE yde_aai_response.

  METHODS update_response
    IMPORTING
              i_task_id        TYPE ty_task_s-id
              i_response       TYPE csequence
    RETURNING VALUE(r_updated) TYPE abap_bool.

ENDINTERFACE.
