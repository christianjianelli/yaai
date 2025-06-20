# ABAP AI System Instructions

## Overview

System instructions (also known as "system prompts") are special messages provided to a Large Language Model (LLM) to guide its behavior, tone, or constraints during a conversation. In ABAP AI, you can pass system instructions to the LLM using the `set_system_instructions` method of the `yif_aai_openai` interface (or its implementing class).

## Purpose

System instructions are typically used to:

- Define the assistant's persona (e.g., "You are a helpful SAP consultant.")
- Set boundaries or rules for responses
- Specify the format or style of answers
- Provide context or background information

## How to Use

1. **Create/Open the LLM Client Instance**

   Typically, you will have an instance of a class implementing `yif_aai_openai`, such as `ycl_aai_openai`.

   ```abap
   DATA(lo_llm) = NEW ycl_aai_openai( ).
   ```

2. **Set the System Instructions**

   Call the `set_system_instructions` method, passing your instructions as a string.

   ```abap
   lo_llm->set_system_instructions(
     i_system_instructions = |You are an expert SAP ABAP developer. Answer concisely and accurately.|
   ).
   ```

3. **Continue with Message Generation**

   After setting the system instructions, you can proceed to generate messages as usual. The instructions will be included in the conversation context sent to the LLM.

   ```abap
   DATA(l_response) TYPE string.
   lo_llm->generate(
     EXPORTING
       i_message  = 'How do I create a custom report in ABAP?'
     IMPORTING
       e_response = l_response
   ).
   ```

## Example

```abap
DATA(lo_llm) = NEW ycl_aai_openai( ).

lo_llm->set_system_instructions(
  i_system_instructions = |You are an expert SAP ABAP developer. Answer concisely and accurately.|
).

DATA(l_response) TYPE string.

lo_llm->generate(
  EXPORTING
    i_message  = 'How do I create a custom report in ABAP?'
  IMPORTING
    e_response = l_response
).

cl_demo_output=>display_text( l_response ).
```

## Notes

- The system instructions should be set before generating messages. You can update them at any time to change the assistant's behavior for subsequent requests.
- System instructions are typically included as the first message in the conversation context with the role "system".
- You can use prompt templates (see `prompt_templates.md`) to generate dynamic or reusable system instructions.

## Related Methods

- `set_system_instructions`: Sets the system prompt for the LLM.
- `generate`: Generates a response from the LLM, using the current system instructions and conversation history.

For more advanced prompt engineering, see [Prompt Templates](prompt_templates.md).
