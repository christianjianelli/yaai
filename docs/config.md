# yaai - ABAP AI tools Configuration

Open the class `ycl_aai_basic_setup` in Eclipse and run it (press F9). This will populate the `YAAI_API`, `YAAI_MODEL`, and `YAAI_TOOL` tables.

The `TVARVC` table (accessible via transaction STVARV) is also supported for configuring API endpoints, but using the YAAI tables is strongly recommended. These parameters define the base URLs for external AI services and are used to construct the full API endpoints for service calls.

## STVARV Parameters

| Parameter Name           | Description                                |
|--------------------------|--------------------------------------------|
| YAAI_OPENAI_BASE_URL     | Base URL for OpenAI API endpoint           |
| YAAI_ANTHROPIC_BASE_URL  | Base URL for Anthropic API endpoint        |
| YAAI_GOOGLE_BASE_URL     | Base URL for Google Gemini API endpoint    |
| YAAI_OLLAMA_BASE_URL     | Base URL for Ollama API endpoint           |
| YAAI_MISTRAL_BASE_URL    | Base URL for Mistral API endpoint          |


### Example Entries in TVARVC

| NAME                     | VALUE                                       |
|--------------------------|---------------------------------------------|
| YAAI_OPENAI_BASE_URL     | https://api.openai.com/                     |
| YAAI_OLLAMA_BASE_URL     | http://localhost:11434                      |
| YAAI_GOOGLE_BASE_URL     | https://generativelanguage.googleapis.com   |
| YAAI_ANTHROPIC_BASE_URL  | https://api.anthropic.com                   |

### Configuration Parameters for Custom APIs

If you want the ABAP AI to automatically construct the full API URL for a custom API, you need to create a parameter in TVARVC using the naming pattern `YAAI_{API_NAME}`. Replace `{API_NAME}` with your API's name (for example, `YAAI_MYAPI`). 

When creating a connection to a custom API, specify the API name as a parameter. For example:

```abap
DATA(lo_conn) = NEW ycl_aai_conn( i_api = 'MYAPI' ).
```

## SSL Setup

To enable secure HTTPS connections to external APIs, you must import the relevant SSL certificates into your SAP system. This ensures that your ABAP system can establish trusted connections with services such as OpenAI or Google.

**Steps for SSL setup:**
1. Obtain the root and intermediate certificates from the API provider (e.g., OpenAI, Google).
2. Import these certificates into the SAP system using transaction **STRUST**.
3. Assign the certificates to the appropriate SSL client PSE (e.g., `ANONYM` or `DEFAULT`).

For detailed, step-by-step instructions, refer to the abapGit documentation:
- [abapGit SSL Setup Guide](https://docs.abapgit.org/user-guide/setup/ssl-setup.html)

**Troubleshooting SSL errors:**
- If you encounter SSL handshake or certificate errors when connecting to APIs, consult SAP Note **510007** for additional guidance:
  - [SAP Note 510007 - Additional considerations about setting up SSL on Application Server ABAP](https://me.sap.com/notes/510007)

**Tip:**  
If you are using the SAP NetWeaver ABAP Developer Edition and face persistent SSL issues, consider using a reverse proxy to simplify connectivity. See [Reverse Proxy Setup](reverse_proxy.md) for more information.
