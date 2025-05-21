CLASS ycl_aai_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aai_log.
    ALIASES add FOR yif_aai_log~add_message.
    ALIASES mt_msg FOR yif_aai_log~mt_messages.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS ycl_aai_log IMPLEMENTATION.

  METHOD yif_aai_log~add_message.

    DATA(ls_msg) = i_s_msg.

    IF ls_msg-id IS INITIAL.
      ls_msg-id = yif_aai_const=>message_id.
    ENDIF.

    IF ls_msg-type IS INITIAL.
      ls_msg-type = 'E'.
    ENDIF.

    APPEND ls_msg TO me->mt_msg.

  ENDMETHOD.

  METHOD yif_aai_log~add_messages.

    LOOP AT i_t_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).

      me->add( i_s_msg = <ls_msg> ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
