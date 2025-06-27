# yaai - ABAP AI tools Configuration Parameters

The ABAP AI tools require specific configuration parameters to be maintained in the SAP table `TVARVC` (accessible via transaction STVARV). These parameters define the base URLs for external AI services and are used to construct the full API endpoints for service calls.

## Required Parameters

| Parameter Name           | Description                                 |
|-------------------------|---------------------------------------------|
| YAAI_OPENAI_BASE_URL    | Base URL for OpenAI API endpoints           |
| YAAI_OLLAMA_BASE_URL    | Base URL for Ollama API endpoints           |

### Example Entries in TVARVC

| NAME                   | VALUE                                 |
|------------------------|---------------------------------------|
| YAAI_OPENAI_BASE_URL   | https://api.openai.com/               |
| YAAI_OLLAMA_BASE_URL   | http://localhost:11434                |

### Configuration Parameters for Custom APIs

If you want the ABAP AI to automatically construct the full API URL for a custom API, you need to create a parameter in TVARVC using the naming pattern `YAAI_{API_NAME}`. Replace `{API_NAME}` with your API's name (for example, `YAAI_MYAPI`). 

When creating a connection to a custom API, specify the API name as a parameter. For example:

```abap
DATA(lo_conn) = NEW ycl_aai_conn( i_api = 'MYAPI' ).
```

---

> **Note:** When using the SAP NetWeaver ABAP Developer Edition to experiment with the ABAP AI tools, you may encounter SSL configuration challenges when connecting to external APIs such as OpenAI. To simplify connectivity, you can use a reverse proxy. For detailed instructions, see [Reverse Proxy Setup](reverse_proxy.md).