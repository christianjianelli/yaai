CLASS ycl_aai_agent_task_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    TYPES: BEGIN OF ty_task_flow_s,
             id               TYPE yde_aai_task_flow_id,
             task_id          TYPE yde_aai_task_id,
             previous_task_id TYPE yde_aai_task_id,
           END OF ty_task_flow_s,

           BEGIN OF ty_task_flow_for_chart_s,
             id                 TYPE yde_aai_task_flow_id,
             task_name          TYPE yde_aai_task_name,
             previous_task_name TYPE yde_aai_task_id,
           END OF ty_task_flow_for_chart_s,

           ty_task_flow_t            TYPE STANDARD TABLE OF ty_task_flow_s WITH KEY task_id,
           ty_task_flow_for_chart_tt TYPE STANDARD TABLE OF ty_task_flow_for_chart_s WITH KEY task_name.

    METHODS constructor
      IMPORTING
        i_o_agent TYPE REF TO yif_aai_agent OPTIONAL.

    METHODS get_tasks
      RETURNING VALUE(r_response) TYPE string.

    METHODS update_task_status
      IMPORTING
                i_task_id         TYPE string
                i_task_status     TYPE yde_aai_task_status
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA _o_agent TYPE REF TO yif_aai_agent.

    METHODS _initialize_task_flow.

    METHODS _sort_topological
      IMPORTING
        i_t_flow        TYPE ty_task_flow_t
      RETURNING
        VALUE(r_t_flow) TYPE ty_task_flow_t.

    METHODS _generate_mermaid_flowchart
      IMPORTING
        i_t_flow                   TYPE ty_task_flow_for_chart_tt
      RETURNING
        VALUE(r_mermaid_flowchart) TYPE string.

ENDCLASS.



CLASS ycl_aai_agent_task_tools IMPLEMENTATION.

  METHOD constructor.

    IF i_o_agent IS SUPPLIED.

      me->_o_agent = i_o_agent.

    ENDIF.

  ENDMETHOD.

  METHOD get_tasks.

    DATA l_task_status TYPE string.

    CLEAR r_response.

    IF me->_o_agent IS NOT BOUND.
      RETURN.
    ENDIF.

    me->_initialize_task_flow( ).

    SELECT id, chat_id, task_id, previous_task_id, status
      FROM yaai_agent_task
       WHERE id = @me->_o_agent->m_agent_id
        AND chat_id = @me->_o_agent->m_chat_id
        AND blocked = @abap_false
      INTO TABLE @DATA(lt_agent_tasks).

    DATA(lt_flow) = me->_sort_topological( i_t_flow = CORRESPONDING #( lt_agent_tasks ) ).

    r_response = |Task Id, Name, Status, Previous Task Id, Description|.

    LOOP AT lt_flow ASSIGNING FIELD-SYMBOL(<ls_task>).

      READ TABLE lt_agent_tasks ASSIGNING FIELD-SYMBOL(<ls_agent_tasks>)
        WITH KEY task_id = <ls_task>-task_id.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      SELECT ddtext
        FROM dd07t
        WHERE domname = 'YDO_AAI_TASK_STATUS'
          AND ddlanguage = @sy-langu
          AND domvalue_l = @<ls_agent_tasks>-status
          INTO @l_task_status
          UP TO 1 ROWS.
      ENDSELECT.

      SELECT SINGLE id, name, description
        FROM yaai_task
        WHERE id = @<ls_task>-task_id
        INTO @DATA(ls_task).

      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ <ls_task>-task_id },{ ls_task-name },{ l_task_status },{ <ls_task>-previous_task_id },{ ls_task-description }|.

    ENDLOOP.

  ENDMETHOD.

  METHOD update_task_status.

    CLEAR r_response.

    IF me->_o_agent IS NOT BOUND.
      RETURN.
    ENDIF.

    UPDATE yaai_agent_task
      SET status = @i_task_status
      WHERE id = @me->_o_agent->m_agent_id
        AND chat_id = @me->_o_agent->m_chat_id
        AND task_id = @i_task_id.

    IF sy-subrc = 0.
      r_response = |Task { i_task_id } status updated successfully.|.
    ELSE.
      r_response = |Task { i_task_id } status not updated.|.
    ENDIF.

  ENDMETHOD.

  METHOD _initialize_task_flow.

    SELECT @abap_true
      FROM yaai_agent_task
     WHERE id = @me->_o_agent->m_agent_id
       AND chat_id = @me->_o_agent->m_chat_id
      INTO @DATA(l_initialized).
    ENDSELECT.

    IF sy-subrc = 0.
      " Already initialized
      RETURN.
    ENDIF.

    SELECT SINGLE task_flow_id
      FROM yaai_agent
      WHERE id = @me->_o_agent->m_agent_id
      INTO @DATA(l_task_flow_id).

    IF l_task_flow_id IS INITIAL.
      " No task flow set
      RETURN.
    ENDIF.

    SELECT id, task_id, previous_task_id
      FROM yaai_task_flow
      WHERE id = @l_task_flow_id
      INTO TABLE @DATA(lt_task_flow).

    LOOP AT lt_task_flow ASSIGNING FIELD-SYMBOL(<ls_task>).

      INSERT yaai_agent_task FROM @( VALUE yaai_agent_task( id = me->_o_agent->m_agent_id
                                                            chat_id = me->_o_agent->m_chat_id
                                                            task_id = <ls_task>-task_id
                                                            previous_task_id = <ls_task>-previous_task_id ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD _sort_topological.

    "--- Kahn's Algorithm (iterative BFS topological sort) ---"
    "
    " Principle:
    "   1. Count how many incoming edges (predecessors) each task has  → in-degree map
    "   2. Seed a queue with all tasks whose in-degree = 0 (root tasks)
    "   3. Repeatedly dequeue a task, append it to result,
    "      and decrement the in-degree of its successors.
    "      When a successor reaches in-degree 0, enqueue it.
    "   4. If result length < input length → a cycle exists.

    TYPES: BEGIN OF ty_indegree,
             task_id  TYPE yde_aai_task_id,
             indegree TYPE i,
           END OF ty_indegree.

    DATA: lt_indegree TYPE HASHED TABLE OF ty_indegree WITH UNIQUE KEY task_id,
          lt_queue    TYPE STANDARD TABLE OF yde_aai_task_id WITH DEFAULT KEY.

    "--- Step 1: Initialize in-degree = 0 for every task ---"
    LOOP AT i_t_flow ASSIGNING FIELD-SYMBOL(<ls_node>).
      INSERT VALUE #( task_id = <ls_node>-task_id  indegree = 0 )
        INTO TABLE lt_indegree.
    ENDLOOP.

    "--- Step 2: Count incoming edges per task ---"
    LOOP AT i_t_flow ASSIGNING <ls_node>
      WHERE previous_task_id IS NOT INITIAL.

      ASSIGN lt_indegree[ task_id = <ls_node>-task_id ] TO FIELD-SYMBOL(<ls_deg>).

      IF sy-subrc = 0.
        <ls_deg>-indegree = <ls_deg>-indegree + 1.
      ENDIF.

    ENDLOOP.

    "--- Step 3: Seed queue with root tasks (in-degree = 0) ---"
    LOOP AT lt_indegree ASSIGNING FIELD-SYMBOL(<ls_root>)
      WHERE indegree = 0.

      APPEND <ls_root>-task_id TO lt_queue.

    ENDLOOP.

    "--- Step 4: Process queue iteratively ---"
    WHILE lt_queue IS NOT INITIAL.

      "Dequeue first element (FIFO)"
      DATA(l_current) = lt_queue[ 1 ].

      DELETE lt_queue INDEX 1.

      "Append to sorted result"
      APPEND i_t_flow[ task_id = l_current ] TO r_t_flow.

      "Find all successors of current task and decrement their in-degree"
      LOOP AT i_t_flow ASSIGNING <ls_node>
        WHERE previous_task_id = l_current.

        READ TABLE lt_indegree
          ASSIGNING <ls_deg>
          WITH KEY task_id = <ls_node>-task_id.

        IF sy-subrc = 0.

          <ls_deg>-indegree = <ls_deg>-indegree - 1.

          "If in-degree dropped to 0, all predecessors are resolved → enqueue"
          IF <ls_deg>-indegree = 0.
            APPEND <ls_node>-task_id TO lt_queue.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDWHILE.

    "--- Step 5: Cycle detection ---"
    IF lines( r_t_flow ) < lines( i_t_flow ).
      FREE r_t_flow.
      r_t_flow = i_t_flow.
    ENDIF.

  ENDMETHOD.

  METHOD _generate_mermaid_flowchart.

    "--- 1. Build Mermaid header ---"
    r_mermaid_flowchart = |flowchart TD\n|.

    IF i_t_flow IS INITIAL.
      r_mermaid_flowchart = |{ r_mermaid_flowchart }{ cl_abap_char_utilities=>newline }No tasks found for flow."]|.
      RETURN.
    ENDIF.

    "--- 2. Identify root tasks (previous_task_id is initial/blank) ---"
    LOOP AT i_t_flow ASSIGNING FIELD-SYMBOL(<ls_flow>).

      "Sanitize IDs: replace spaces/special chars for valid Mermaid node IDs"
      DATA(lv_task_safe) = CONV string( <ls_flow>-task_name ).
      DATA(lv_prev_safe) = CONV string( <ls_flow>-previous_task_name ).

      REPLACE ALL OCCURRENCES OF REGEX '[^a-zA-Z0-9_]' IN lv_task_safe WITH '_'.
      REPLACE ALL OCCURRENCES OF REGEX '[^a-zA-Z0-9_]' IN lv_prev_safe WITH '_'.

      IF <ls_flow>-previous_task_name IS INITIAL.
        "Root task: render as a rounded start node"
        r_mermaid_flowchart = r_mermaid_flowchart && |  { lv_task_safe }(["{ <ls_flow>-task_name }"]){ cl_abap_char_utilities=>newline }|.
      ELSE.
        "Connected task: render node + edge from previous"
        r_mermaid_flowchart = r_mermaid_flowchart && |  { lv_prev_safe } --> { lv_task_safe }{ cl_abap_char_utilities=>newline }|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    me->_o_agent = NEW ycl_aai_agent(
      i_agent_id = '000C2927F0011FE18DB73922A64EF791'
      i_chat_id  = '00000000000000000000000000000001'
    ).

    DATA(l_get_tasks) = abap_true.
    DATA(l_update_task_status) = abap_false.

    CASE abap_true.

      WHEN l_get_tasks.

        l_response = me->get_tasks( ).

      WHEN l_update_task_status.

        l_response = me->update_task_status(
                       i_task_id     = '000C2956EF8E1FE18FC9738D08658D36'
                       i_task_status = 'X'
                     ).

    ENDCASE.

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
