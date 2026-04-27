*&---------------------------------------------------------------------*
*& Report yr_aai_agent_run
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yr_aai_agent_run.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS p_agent TYPE c LENGTH 32 OBLIGATORY.
PARAMETERS p_api   TYPE yde_aai_api OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  ycl_aai_agent=>run_autonomously(
    EXPORTING
      i_agent_id = CONV #( p_agent )
      i_api      = p_api
  ).
