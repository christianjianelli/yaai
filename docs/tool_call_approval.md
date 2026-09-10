# yaai - ABAP AI tools - Tool Call Approvals

As of version 1.2.3, the ABAP AI tools function calling feature allows you to define whether a tool requires user approval before it can be used by the LLM.

When registering a tool in the database table `yaai_tool`, you can set the `approval` flag to indicate that the tool requires user approval. Optionally, you can also customize the text returned to the LLM to indicate that approval is required.

The tool call approval feature is available in the [ABAP AI tools Cockpit](https://github.com/christianjianelli/yaai_cockpit) as of version 1.2.3.

When the `approval` flag is set to `true`, the ABAP AI tools function calling mechanism automatically creates an entry in the `yaai_approval` database table whenever the LLM calls the tool. ABAP AI tools then informs the LLM that the tool requires user approval, and the LLM asks the user for permission to execute the tool.

## Managing Tool Call Approval Requests

The following methods are available in the `ycl_aai_db` class to manage tool call approval requests:

- `get_approval` – Retrieves the current state of a tool call approval request.
- `update_approval` – Updates the state of a tool call approval request.

> **Note:** The tool call approval feature is only available when chat persistence is enabled. Approvals are managed at the chat level, and the persistence layer is required for registering the user's approval.
